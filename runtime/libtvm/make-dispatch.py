#!/usr/bin/python
# tvm - make-dispatch.py: extract instruction definitions from source
# The Transterpreter - a portable virtual machine for Transputer bytecode
# Copyright (C) 2007 Adam Sampson
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

import sys, re, os, time

def die(s):
	sys.stderr.write("Fatal: %s\n" % s)
	sys.exit(1)

def find_instructions(defs, fn):
	cond_stack = []

	header = re.sub(r'\.c$', '.h', fn)

	f = open(fn)
	while 1:
		l = f.readline()
		if l == "":
			break

		l = l.strip()
		m = re.match(r'^/\* 0x([0-9A-Fa-f]+_?) - ', l)
		if m is not None:
			code = m.group(1)
			while 1:
				defl = f.readline()
				if defl == "" or defl.startswith("#"):
					die("EOF or preprocessor instruction while looking for TVM_INSTRUCTION matching '%s' in %s" % (l, fn))
				defl = defl.strip()
				m = re.match(r'^TVM_INSTRUCTION\s*\(\s*(ins_[^\s\)]+)\s*\)', defl)
				if m is not None:
					break

			if code in defs:
				die("Multiple definitions of opcode %s" % code)
			defs[code] = ([] + cond_stack, header, m.group(1))

		if l.startswith("#if"):
			cond_stack.append(l)

		if l.startswith("#else"):
			top = cond_stack[-1]
			top = re.sub(r'^#ifdef', '#ifndef', top)
			top = re.sub(r'^#if (.*)$', r'#if !(\1)', top)
			cond_stack[-1] = top

		if l.startswith("#endif"):
			cond_stack.pop()
	f.close()

def ins_key_to_int(key):
	return int(key.replace('_', ' ').strip(), 16)

def write_switch(defs, fn):
	bits = ["""/* Generated automatically by make-dispatch.py; do not modify! */

static inline int dispatch_instruction (ECTX ectx, BYTE instr)
{
	#ifdef TVM_PROFILING
	ectx->profile.pri[instr >> 4]++;
	#endif

	switch (instr >> 4) {
"""]

	last_conds = []
	def gen_instr(indent, opcode, key):
		(conds, header, func) = defs[key]

		if conds == last_conds:
			# Same conditions as last time; remove the #endifs
			for c in conds:
				bits.pop()
		else:
			for c in conds:
				bits.append(c + "\n")

		bits.append("%scase 0x%s: return %s(ectx);\n" % (indent, opcode, func))

		for c in conds:
			bits.append("#endif\n")
		last_conds[:] = conds

	keys = defs.keys()
	keys.sort(lambda a, b: cmp(ins_key_to_int(a), ins_key_to_int(b)))

	for k in keys:
		if k == "F_":
			pass
		elif k[1] == "_":
			gen_instr("\t", "0" + k[0], k)

	bits.append("""
	case 0x0F:
		{
			WORD ins = OREG;
			CLEAR(OREG);
			
			#ifdef TVM_PROFILING
			ectx->profile.sec[ins]++;
			#endif

			switch (ins) {
""")

	last_conds[:] = []
	for k in keys:
		if k[1] != "_":
			gen_instr("\t\t\t", k, k)

	bits.append("""
			}
		}
	}
	/* FIXME: This handles unimplemented and invalid instructions the same way. */
	return ECTX_INS_INVALID;
}
""")

	f = open(fn + ".new", "w")
	f.write("".join(bits))
	f.close()
	if os.access(fn, os.F_OK):
		if os.access(fn + ".old", os.F_OK):
			os.remove(fn + ".old")
		os.rename(fn, fn + ".old")	
	os.rename(fn + ".new", fn)

def write_jumptable(defs, fn, name, keys):
	bits = ["""/* Generated automatically by make-dispatch.py; do not modify! */

#include "tvm.h"
#include "instructions.h"
"""]

	headers = {}
	min = 0xffff
	max = 0
	for key in keys:
		if key in defs:
			(conds, header, func) = defs[key]
			headers[header] = True
			key_int = ins_key_to_int(key)
			if key_int < min:
				min = key_int
			if key_int > max:
				max = key_int
	header_list = headers.keys()
	header_list.sort()
	for h in header_list:
		bits.append("#include \"%s\"\n" % h)

	bits.append("""
const unsigned int %s_min = %d;
const unsigned int %s_max = %d;
""" % (name, min, name, max))

	bits.append("""
int (*const %s[%d])(ECTX) =
{
""" % (name, (max - min) + 1))

	def gen_entry(i, func):
		comma = ","
		if i == len(keys) - 1:
			comma = ""
		s = func + comma
		bits.append("\t%-25s/* %02X */\n" % (s, i))

	def gen_not_implemented(i):
		gen_entry(i, "ins_not_implemented")

	def gen_with_conds(i, conds, func):
		if conds == []:
			gen_entry(i, func)
		else:
			bits.append("%s\n" % conds[0])
			gen_with_conds(i, conds[1:], func)
			bits.append("#else\n")
			gen_not_implemented(i)
			bits.append("#endif\n")

	for i in range(len(keys)):
		key = keys[i]
		key_int = ins_key_to_int(key)
		if key in defs:
			(conds, header, func) = defs[key]
			gen_with_conds(i, conds, func)
		elif (key_int >= min) and (key_int <= max):
			gen_not_implemented(i)

	bits.append("""
};
""")

	f = open(fn + ".new", "w")
	f.write("".join(bits))
	f.close()
	if os.access(fn, os.F_OK):
		if os.access(fn + ".old", os.F_OK):
			os.remove(fn + ".old")
		os.rename(fn, fn + ".old")	
	os.rename(fn + ".new", fn)

def write_jumptables(defs, fn_pri, fn_sec, fn_ex_sec):
	keys = ["%1X_" % i for i in range(16)]
	write_jumptable(defs, fn_pri, "primaries", keys)

	keys = ["%02X" % i for i in range(256)]
	write_jumptable(defs, fn_sec, "secondaries", keys)

	keys = ["2%02X" % i for i in range(256)]
	write_jumptable(defs, fn_ex_sec, "extended_secondaries", keys)

def main():
	inputs = sys.argv[1:]
	if inputs == []:
		die("No input files specified -- use the Makefile target!")
	output_switch = "dispatch_ins.c"
	output_pri = "jumptbl_pri.c"
	output_sec = "jumptbl_sec.c"
	output_ex_sec = "jumptbl_ex_sec.c"

	defs = {}
	for fn in inputs:
		find_instructions(defs, fn)
	write_switch(defs, output_switch)
	write_jumptables(defs, output_pri, output_sec, output_ex_sec)

if __name__ == "__main__":
	main()

