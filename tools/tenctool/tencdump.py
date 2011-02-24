#!/usr/bin/env python

import sys

if not '@pkgpythondir@'.startswith('@pkgpy'): sys.path.append('@pkgpythondir@')

import tenclib

def dump(data):
    parser = tenclib.TEncParser(data)
    tenc   = parser.parse()
    return str(tenc)

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print "Usage: %s <filename or - (minus)>" % sys.argv[0]
        sys.exit(1)

    # Read
    if sys.argv[1] == '-':
        data = sys.stdin.read()
    else:
        data   = open(sys.argv[1], 'rb').read()

    # Process
    print dump(data)
