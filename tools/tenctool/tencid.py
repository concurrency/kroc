#!/usr/bin/env python

import sys

if not '@pkgpythondir@'.startswith('@pkgpy'): sys.path.append('@pkgpythondir@')

import tenclib

def identity(data):
    parser = tenclib.TEncParser(data)
    tenc   = parser.parse()
    return tenc.dump_bytes()
 
if __name__ == '__main__':
    if len(sys.argv) < 3:
        print "Usage: %s " \
            "<tenc filename or - (minus)> <output filename or - (minus)>" % sys.argv[0]
        sys.exit(1)

    # Read
    if sys.argv[1] == '-':
        data = sys.stdin.read()
    else:
        data   = open(sys.argv[1], 'rb').read()

    # Process
    data   = identity(data)

    # Write
    if sys.argv[2] == '-':
        sys.stdout.write(data)
    else:
        fp = open(sys.argv[2], 'wb')
        fp.write(data)
        fp.close()
