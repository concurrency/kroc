#!/usr/bin/env python
#
#	Script to various CCSP headers using special comments in sched.c
#	Copyright (C) 2007, 2008 Carl Ritson <cgr@kent.ac.uk>
#
#	This program is free software; you can redistribute it and/or modify
#	it under the terms of the GNU General Public License as published by
#	the Free Software Foundation; either version 2 of the License, or
#	(at your option) any later version.
#
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with this program; if not, write to the Free Software
#	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#

import sys, os, re, functools

prog_name 	= "make-header.py"

class HeaderWriter:
	def __init__(self, fh):
		self.asm	= False
		self.fh		= fh
		self.macro	= False
		self.tabs	= 0
	
	def write(self, *args):
		self.fh.write(*args)

	def begin_line(self):
		for i in range(self.tabs):
			self.write("\t")
		if self.asm:
			self.write("\"")
	
	def end_line(self, end_macro=False):
		if end_macro:
			self.macro = False
		if self.asm:
			self.write("\\n\"")
		if self.macro:
			self.write(" \\")
		self.write("\n")
	
	def add(self, *args):
		self.write(*args)

	def line(self, *args):
		self.begin_line()
		self.add(*args)
		self.end_line()
	
	def close(self):
		self.fh.close()

	def indent(self):
		self.tabs += 1
	
	def outdent(self):
		self.tabs -= 1

	def begin_macro(self):
		self.macro = True

	def end_macro(self):
		self.macro = False
	
	def begin_asm(self):
		self.asm = True
	
	def end_asm(self):
		self.asm = False

def warn(*s):
	print >>sys.stderr, prog_name + ": " + "".join(map(str, s))

def die(*s):
	warn(*s)
	sys.exit(1)

def safe_sorted(lst, comp):
	try:
		return sorted(lst, key=functools.cmp_to_key(comp))
	except NameError:
		lst = list(lst)
		return sorted(lst, key=functools.cmp_to_key(comp))

def load_defines(defines, fn):
	define_re = re.compile(r'^#define\s+(\S+)')
	
	f = open(fn)

	while 1:
		l = f.readline()
		if l == "":
			break

		l = l.strip()
		m = define_re.match(l)
		if m is not None:
			define = m.group(1)
			defines[define] = True
	
	f.close()

def load_symbols(calls, symbols, fn):
	markup_re = re.compile(r'^\s*\*\s*@(\S+)\s*:\s*(.*)')
	params_re = re.compile(r'(\d+)')

	f = open(fn)

	while 1:
		l = f.readline()
		if l == "":
			break

		l = l.strip()
		m = markup_re.match(l)
		if m is not None:
			type, data = m.group(1, 2)
			if type == "SYMBOL":
				current = { "name" : data, "INPUT" : 0, "OUTPUT" : 0 }
				symbols[data] = current
			elif type == "CALL":
				calls[data] = current
				current["handles"] = [ data ]
			elif type == "HANDLE":
				handles = re.split(r",\s*", data)
				for handle in handles:
					calls[handle] = current
				current["handles"].extend(handles)
			elif type in ("INPUT", "OUTPUT"):
				current[type] = int(data)
			elif type in ("DEPEND", "INCOMPATIBLE"):
				current[type] = re.split(r',\s*', data)
			else:
				current[type] = data
	
	f.close()

def test_symbol_requirements(defines, calls, symbols):
	for name, symbol in symbols.items():
		unsupported = False

		if "DEPENDS" in symbol:
			for define in symbol["DEPEND"]:
				unsupported = unsupported or (not define in defines)
		if "INCOMPATIBLE" in symbol:
			for define in symbol["INCOMPATIBLE"]:
				unsupported = unsupported or (define in defines)
		if unsupported:
			for call in symbol["handles"]:
				calls[call] = symbols["Y_unsupported"]
			symbol["unsupported"] = True

def enumerate_symbols(symbols):
	def compare_symbols(a, b):
		prio_a = int(symbols[a].get("PRIO", 0))
		prio_b = int(symbols[b].get("PRIO", 0))
		if prio_a == prio_b:
			return (a > b) - (a < b)
		else:
			return (prio_b > prio_a) - (prio_b < prio_a)

	i = 0
	for symbol in safe_sorted(symbols.keys(), compare_symbols):
		symbols[symbol]["offset"] = i
		i = i + 1

def short_fn(fn):
	match = re.match(".*\/(.*)", fn)
	if match is None:
		return fn
	else:
		return match.group(1)

def clean_fn(fn):
	return re.sub("[^A-Za-z0-9]", "_", short_fn(fn)).upper()

def output_header(f, fn, critical):
	cfn = clean_fn (fn)
	f.write("/* %s - generated by %s; do not modify! */\n" % (short_fn(fn), prog_name))
	if critical:
		f.write("/* WARNING: if you regenerate this file, tranx86 and libraries must be rebuilt */\n")
	f.write("\n")
	f.write("#ifndef __%s\n" % cfn)
	f.write("#define __%s\n" % cfn)
	f.write("\n")

def output_footer(f, fn):
	f.write("\n")
	f.write("#endif /* !__%s */\n" % clean_fn(fn))
	f.write("\n")

def output_kitable(symbol_list, symbols, fn):
	f = open(fn, "w")
	output_header(f, fn, True)
	
	# output defines
	for name in symbol_list:
		symbol = symbols[name]
		call = symbol["handles"][0]
		f.write("#define %s %d\n" % (call, symbol["offset"]))
		for alias in symbol["handles"][1:]:
			f.write("#define %s %s\n" % (alias, call))
	
	f.write("\n")

	# output table
	f.write("#if !defined(__cplusplus)\n")
	f.write("#if defined(__GNUC__)\n")
	f.write("__attribute__ ((unused)) /* make GCC ignore when unused */\n")
	f.write("#endif\n")
	f.write("static ccsp_entrytype ccsp_entrytable[] = {\n")
	for name in symbol_list:
		symbol		= symbols[name]
		call_offset	= symbol["handles"][0]
		support		= "KCALL_SUPPORTED"

		f.write("\t{\n")
		f.write("\t .call_offset  = %s,\n" 	% call_offset)
		f.write("\t .entrypoint   = \"%s\",\n"	% name)
		f.write("\t .input        = %d,\n"	% symbol["INPUT"])
		f.write("\t .output       = %d,\n"	% symbol["OUTPUT"])
		f.write("\t .support      = %s\n"	% support)
		f.write("\t},\n")
	f.write("};\n")
	f.write("#endif /* !__cplusplus */\n")

	output_footer(f, fn)
	f.close()

def output_calltable(symbol_list, symbols, fn):
	f = open(fn, "w")
	output_header(f, fn, False)
	
	f.write ("#include <arch/sched_asm_inserts.h>\n")

	for name in symbol_list:
		inputs = int(symbols[name].get("INPUT"))
		outputs = int(symbols[name].get("OUTPUT"))
		if name.startswith("CIF_"):
			f.write("static void *kernel_%s (void);\n" % name)
		elif not symbols[name].get("unsupported"):
			f.write("K_CALL_DEFINE_%d_%d (%s);\n" % (inputs, outputs, name))
	f.write("\n")
	
	f.write("static inline void build_calltable (void **table)\n")
	f.write("{\n")
	for (i, name) in enumerate(symbol_list):
		sname = name
		if symbols[name].get("unsupported"):
			sname = "Y_unsupported"

		f.write("\t/* %s */\n" % (name))
		if name.startswith("CIF_"):
			f.write("\ttable[% 3d] = kernel_%s ();\n" % (i, sname))
		else:
			f.write("\ttable[% 3d] = K_CALL_PTR (%s);\n" % (i, sname))
	f.write("}\n\n")

	output_footer(f, fn)
	f.close()

def gen_cif_stub(f, symbol, arch_generator):
	name 		= symbol["name"]
	arguments	= []
	inputs		= ["i" + str(i) for i in range(symbol["INPUT"])]
	outputs		= ["o" + str(i) for i in range(symbol["OUTPUT"])]
	arguments	= inputs + outputs

	f.begin_macro()

	f.begin_line()
	f.add("#define ccsp_cif_%s(" % name)
	
	arguments.insert(0, "__wptr")

	if len(arguments) > 0:
		f.add(arguments.pop(0))
		for arg in arguments:
			f.add("," + arg)

	f.add(")")
	f.end_line()

	f.indent()
	f.line("do {")

	f.indent()
	f.line("ccsp_sched_t *__sched = (ccsp_sched_t *) ((__wptr)[SchedPtr]);")

	arch_generator(f, symbol, inputs, outputs)

	f.outdent()

	f.begin_line()
	f.add (" } while (0)")
	f.end_line(end_macro = True)
	f.outdent()

def gen_i386_header(f):
	# ccsp_cif_external_call
	f.begin_macro()

	f.begin_line()
	f.line("#define ccsp_cif_external_call(func, stack, result)")

	f.indent()
	f.line("do {")
	
	f.indent()
	f.line("__asm__ __volatile__ (\"\\n\"")
	f.indent()

	f.begin_asm()
	f.line("\tmovl %%esp, %%edi")
	f.line("\tmovl %2, %%esp")
	f.line("\tcall *%1")
	f.line("\tmovl %%edi, %%esp")
	f.end_asm()
	f.line(": \"=a\" (result)")
	f.line(": \"r\" (func), \"r\" (stack)")
	f.line(": \"cc\", \"memory\", \"ecx\", \"edx\", \"edi\"")

	f.outdent()
	f.line(");")
	f.outdent()
	
	f.begin_line()
	f.add ("} while (0)")
	f.end_line(end_macro = True)
	f.outdent()

	# ccsp_cif_jump
	f.begin_macro()

	f.begin_line()
	f.line("#define ccsp_cif_jump(wptr, addr)")

	f.indent()
	f.line("do {")
	
	f.indent()
	f.line("__asm__ __volatile__ (\"\\n\"")
	f.indent()

	f.begin_asm()
	f.line("\tmovl %0, %%ebp")
	f.line("\tjmp *%1")
	f.end_asm()
	f.line(": /* no outputs */")
	f.line(": \"r\" (wptr), \"q\" (addr)")
	f.line(": \"memory\"")

	f.outdent()
	f.line(");")
	f.outdent()
	
	f.begin_line()
	f.add ("} while (0)")
	f.end_line(end_macro = True)
	f.outdent()
	
	# ccsp_cif_occam_call
	f.begin_macro()

	f.begin_line()
	f.line("#define ccsp_cif_occam_call(sched, stack, ws, func, top)")

	f.indent()
	f.line("do {")
	
	f.indent()
	f.line("word d0, d1, d2;")
	f.line("__asm__ __volatile__ (\"\\n\"")
	f.indent()

	f.begin_asm()
	f.line("\tpushl %%ebp")
	f.line("\tmovl %0, %%ebp")
	f.line("\taddl %4, %%ebp")
	f.line("\tmovl %%esp, (%%ebp)")
	f.line("\tsubl %4, %%ebp")
	f.line("\tmovl %%edi, %%esp")
	f.line("\tmovl $0f, 0(%%ebp)")
	f.line("\tjmp *%%eax")
	f.line("0:")
	f.line("\taddl %5, %%ebp")
	f.line("\tmovl (%%ebp), %%esp")
	f.line("\tpopl %%ebp")
	f.end_asm()
	f.line(": \"=d\" (d0), \"=S\" (sched), \"=D\" (d1), \"=a\" (d2)")
	f.line(": \"i\" (top * sizeof(word)),")
	f.line("  \"i\" ((top - 4) * sizeof(word)),")
	f.line("  \"0\" (ws), \"1\" (sched), \"2\" (stack), \"3\" (func)")
	f.line(": \"cc\", \"memory\", \"ebx\", \"ecx\"")

	f.outdent()
	f.line(");")
	f.outdent()
	
	f.begin_line()
	f.add ("} while (0)")
	f.end_line(end_macro = True)
	f.outdent()

def gen_i386_cif_stub(f, symbol, inputs, outputs):
	resched = (symbol["name"][0] == 'Y')
	regs	= [ "a", "d", "c" ]
	cregs	= [ "eax", "edx", "ecx" ]
	in_regs = 0
	out_regs= 0
	offset  = (6 * 4) + (4 * symbol["offset"])

	in_regs = len(inputs)
	out_regs = len(outputs)

	if in_regs > 1:
		in_regs = 1
	if out_regs > 1:
		out_regs = 1
	
	dummies = []
	if not resched:
		dummies.append("sched_dummy")
		dummies.append("wptr_dummy")
	if in_regs > out_regs:
		dummies = ["dummy0"] + dummies

	if len(dummies) > 0:
		f.line("{")
		f.indent()
		for dummy in dummies:
			f.line("word %s;" % dummy)

	if len(inputs) > 1:
		for (n, i) in enumerate(inputs):
			if n >= 1:
				f.line("__sched->cparam[%d] = (word) (%s);" % ((n - 1), i))
	
	f.line("__asm__ __volatile__ (\"\\n\"")
	f.indent()

	f.begin_asm()
	if resched:
		f.line("\tpushl %%ebx")
		f.line("\tpushl %%ebp")
		f.line("\tmovl %%esp, -28(%%edi)")
		f.line("\tmovl %%esi, %%esp")
		f.line("\tmovl %%esi, %%edx")
		f.line("\tmovl %%edi, %%ecx")
		f.line("\tmovl %%edi, %%ebp")
		f.line("\tcall *%d(%%%%esi)" % offset)
		f.line("\tmovl -28(%%ebp), %%esp")
		f.line("\tmovl %%ebp, %%edi")
		f.line("\tpopl %%ebp")
		f.line("\tpopl %%ebx")
	else:
		f.line("\tmovl %%esi, %%edx")
		f.line("\txchgl %%esp, %%esi")
		f.line("\tcall *%d(%%%%edx)" % offset)
		f.line("\tmovl %%esi, %%esp")
	f.end_asm()

	f.begin_line()
	if resched:
		f.add(": \"=D\" (__wptr), \"=S\" (__sched)")
	else:
		f.add(": \"=c\" (wptr_dummy), \"=S\" (sched_dummy)")
	if (in_regs + out_regs) > 0:
		f.add(", \"=a\" (%s)" % ((outputs + dummies)[0]))
	f.end_line()

	f.begin_line()
	f.add(": \"0\" (__wptr), \"1\" (__sched)")
	for (idx, name) in enumerate(inputs):
		if idx < in_regs:
			f.add(", \"%d\" (%s)" % (idx + 2, name))
	f.end_line()

	f.begin_line()
	f.add(": \"cc\", \"memory\", \"edx\"")
	if (in_regs + out_regs) == 0:
		f.add(", \"eax\"")
	if resched:
		f.add(", \"ecx\"")
	f.end_line()

	f.outdent()
	f.line(");")

	if resched:
		f.line("(__wptr)[SchedPtr] = (word) __sched;")

	if len(outputs) > 1:
		for (n, i) in enumerate(outputs):
			if n >= 1:
				f.line("*((word *)(&(%s))) = __sched->cparam[%d];" % (i, (n - 1)))

	if len(dummies) > 0:
		f.outdent()
		f.line("}")

def gen_mips_header(f):
	f.line("/* mips unsupported */")

def gen_mips_cif_stub(f, symbol, inputs, outputs):
	f.line("/* mips unsupported */")

def gen_ppc64_header(f):
	f.line("/* ppc64 unsupported */")

def gen_ppc64_cif_stub(f, symbol, inputs, outputs):
	f.line("/* ppc64 unsupported */")

def gen_sparc_header(f):
	f.line("/* sparc unsupported */")

def gen_sparc_cif_stub(f, symbol, inputs, outputs):
	f.line("/* sparc unsupported */")

def output_cif(defines, symbol_list, symbols, fn):
	if "TARGET_CPU_386" in defines:
		arch_header = gen_i386_header
		arch_generator = gen_i386_cif_stub
	elif "TARGET_CPU_MIPS" in defines:
		warn("generating CIF stubs for unsupported architecture...")
		arch_header = gen_mips_header
		arch_generator = gen_mips_cif_stub
	elif "TARGET_CPU_PPC64" in defines:
		warn("generating CIF stubs for unsupported architecture...")
		arch_header = gen_ppc_header
		arch_generator = gen_ppc64_cif_stub
	elif "TARGET_CPU_SPARC" in defines:
		warn("generating CIF stubs for unsupported architecture...")
		arch_header = gen_sparc_header
		arch_generator = gen_sparc_cif_stub
	else:
		die("unable to generate CIF stubs for unknown architecture.")

	f = HeaderWriter(open(fn, "w"))
	output_header(f, fn, False)

	arch_header(f)

	for name in symbol_list:
		symbol = symbols[name]
		if not symbol.get("unsupported"):
			gen_cif_stub(f, symbol, arch_generator)

	output_footer(f, fn)
	f.close()

def main(args):
	if len(args) < 3:
		die("usage: <type> <output.h> <input1> [<input2> ...]")
	
	type = args[0]
	output = args[1]
	inputs = args[2:]
	
	defines = {}
	calls = {}
	symbols = {}
	for input in inputs:
		load_defines(defines, input)
		load_symbols(calls, symbols, input)
	
	test_symbol_requirements(defines, calls, symbols)
	enumerate_symbols(symbols)

	symbol_list = safe_sorted(
		symbols.keys(),
		lambda a, b: (symbols[a]["offset"] > symbols[b]["offset"]) - (symbols[a]["offset"] < symbols[b]["offset"])
	)

	if type == "--kitable":
		output_kitable(symbol_list, symbols, output)
	elif type == "--calltable":
		output_calltable(symbol_list, symbols, output)
	elif type == "--cif":
		output_cif(defines, symbol_list, symbols, output)
	else:
		die("type must be: --kitable, --calltable or --cif")

if __name__ == "__main__":
	main(sys.argv[1:])

