#!/usr/bin/env python
# pideps: generate dependencies for occam-pi programs.
# Copyright 2004, 2005, 2006, 2007 Adam Sampson <ats@offog.org>
# Computing Laboratory, University of Kent, Canterbury, UK
#
# pideps is free software; you can redistribute and/or modify it
# under the terms of that license as published by the Free Software
# Foundation; either version 2 of the License, or (at your option)
# any later version.
#
# pideps is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with pideps; see the file COPYING. If not, write to the Free
# Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA.

import re, os, sys, getopt

def warn(*s):
	print >>sys.stderr, " ".join(map(str, s))

def die(*s):
	warn(*s)
	sys.exit(1)

def object(ofn, binaries):
	if ofn.endswith(".module"):
		ofn = ofn[:-7] + ".lib"
	elif ofn[:-4] in binaries:
		ofn = ofn[:-4]
	elif not (ofn.endswith(".tce") or ofn.endswith(".lib")):
		ofn = sourceprefix + ofn
	return ofn

def dequote(s):
	return s[1:-1]

def debracket(s):
	return s[1:-1]

def find_dep(fn, dep, incpath):
	for dir in [os.path.dirname(fn)] + incpath:
		dpath = os.path.normpath(os.path.join(dir, dep))
		if os.access(dpath, os.F_OK):
			return dpath
	return None

def generate_dep(fn, dep, incpath, binaries, deps):
	dep_fn = find_dep(fn, dep, incpath)
	if dep_fn is not None:
		target = re.sub(r'\.occ$', r'.tce', fn)
		deps[object(target, binaries) + ": " + object(dep_fn, binaries)] = True
	else:
		print >>sys.stderr, "Cannot find dependency: %s: %s" % (fn, dep)
	return dep_fn

def parse_expr(expr, defines):
	tokens = []
	i = 0
	while i < len(expr):
		c = expr[i]
		i += 1
		if c == "(":
			tokens.append("(")
		elif c == ")":
			tokens.append(")")
		elif c == " ":
			while i < len(expr) and expr[i] == " ":
				i += 1
		else:
			j = i - 1
			while j < len(expr) and expr[j] not in " ()":
				j += 1
			tokens.append(expr[i - 1:j])
			i = j

	def evaluate(tokens):
		if tokens == []:
			die("Empty (sub)expression:", expr)

		i = 0
		val = None
		while i < len(tokens):
			t = tokens[i]
			i += 1
			if t == "(":
				level = 1
				j = i
				while j < len(tokens) and level > 0:
					if tokens[j] == "(":
						level += 1
					elif tokens[j] == ")":
						level -= 1
					j += 1
				if level != 0:
					die("Unmatched brackets:", expr)
				val = evaluate(tokens[i:j - 1])
				i = j
			elif t == "TRUE":
				val = True
			elif t == "FALSE":
				val = False
			elif t == "NOT":
				val = not evaluate(tokens[i:])
				i = len(tokens)
			elif t == "AND":
				if val is None:
					die("Unary AND:", expr)
				val = val and evaluate(tokens[i:])
				i = len(tokens)
			elif t == "OR":
				if val is None:
					die("Unary OR:", expr)
				val = val or evaluate(tokens[i:])
				i = len(tokens)
			elif t == "DEFINED":
				if i + 3 > len(tokens) or tokens[i] != "(" or tokens[i + 2] != ")":
					die("Bad usage of DEFINED:", expr)
				val = tokens[i + 1] in defines
				i += 3
			else:
				die("Unrecognised token in expression:", expr, ": ", t)

		return val

	return evaluate(tokens)

def parse(fn, deps, incpath, binaries, defines = {}):
	stack = [True]
	f = open(fn)
	for line in f.readlines():
		line = line[:-1]
		i = line.find("--")
		if i != -1:
			line = line[:i]

		ls = line.strip().split(None, 1)
		if ls == []:
			continue
		elif ls[0] == "#IF":
			stack.append(parse_expr(ls[1], defines))
		elif ls[0] == "#ENDIF":
			stack.pop()
		elif ls[0] == "#ELSE":
			stack[-1] = not stack[-1]
		elif False in stack:
			pass
		elif ls[0] == "#DEFINE":
			defines[ls[1]] = True
		elif ls[0] == "#INCLUDE":
			include_fn = generate_dep(fn, dequote(ls[1]), incpath, binaries, deps)
			if include_fn is not None:
				parse(include_fn, deps, incpath, binaries, defines)
		elif ls[0] == "#USE":
			name = dequote(ls[1])
			if name.endswith(".tce"):
				name = name[:-4]
			if not name.endswith(".lib"):
				name += ".tce"
			generate_dep(fn, name, incpath, binaries, deps)
	f.close()

def usage():
	print "Usage: pideps [-I include-dir]... [-b binary]... [-f output-file] source ..."
	sys.exit(1)

if __name__ == "__main__":
	try:
		opts, args = getopt.getopt(sys.argv[1:], "I:b:f:")
	except getopt.GetoptError:
		usage()
	if args == []:
		usage()

	incpath = []
	binaries = []
	outputfn = "Makefile.am"
	sources = args
	for o, a in opts:
		if o == "-I":
			incpath.append(a)
		elif o == "-b":
			binaries.append(a)
		elif o == "-f":
			outputfn = a

	sourceprefix = ""
	if outputfn.endswith("Makefile.am"):
		sourceprefix = "$(srcdir)/"

	deps = {}
	for file in sources:
		parse(file, deps, incpath, binaries)
	deplines = deps.keys()
	deplines.sort()

	startmarker = "## begin pideps dependencies"
	endmarker = "## end pideps dependencies"

	added = False
	fi = open(outputfn, "r")
	fo = open(outputfn + ".new", "w")
	while 1:
		l = fi.readline()
		if l == "":
			warn("No '" + startmarker + "' line found; adding one")
			fo.write("\n")
			added = True
			break
		if l.strip() == startmarker:
			break
		fo.write(l)
	while not added:
		l = fi.readline()
		if l == "":
			die("No '" + endmarker + "' line found")
		if l.strip() == endmarker:
			break
	fo.write(startmarker + "\n")
	for dep in deplines:
		fo.write(dep + "\n")
	fo.write(endmarker + "\n")
	fo.write(fi.read())
	fi.close()
	fo.close()
	os.rename(outputfn + ".new", outputfn)

