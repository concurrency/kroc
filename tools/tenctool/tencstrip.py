#!/usr/bin/env python

import sys

if not '@pkgpythondir@'.startswith('@pkgpy'): sys.path.append('@pkgpythondir@')

import tenclib

def usage():
    print "Usage: %s [-d] [-t] [-s] [-f] " \
            "<tenc filename> <output filename or - (minus)>" % sys.argv[0]
    print "  default stripping is -d and -s"
    print "    -d: debug info"
    print "    -t: TLP info"
    print "    -s: symbol info"
    print "    -d: ffi table"
    sys.exit(1)

def remove(tenc, tag):
    for e in tenc.elements:
        if e.tag == 'tbcL':
            for e1 in e.elements:
                if e1.tag == tag:
                    e1.elements = []

def strip(data, debug=True, symbol=True, tlp=False, ffi=False):
    parser = tenclib.TEncParser(data)
    tenc   = parser.parse()

    if debug: remove(tenc, 'dbgL')
    if tlp: remove(tenc, 'tlpL')
    if symbol: remove(tenc, 'stbL')
    if ffi: remove(tenc, 'ffiL')
    return tenc.dump_bytes()

if __name__ == '__main__':
    if len(sys.argv) < 3 or len(sys.argv) > 7: 
        usage()

    if len(sys.argv) == 3:
        debug, symbol, tlp, ffi = 2 * [True] + 2 * [False]
        args = sys.argv[1:]
    else:
        debug, symbol, tlp, ffi = 4 * [False]
        args = sys.argv[1:]
        while args[0] in ['-d', '-t', '-s', '-f']:
            if args[0]   == '-d': debug = True
            elif args[0] == '-t': tlp = True
            elif args[0] == '-s': symbol = True
            elif args[0] == '-f': ffi = True
            else: break
            args = args[1:]
    if len(args) > 2:
        usage()

    data   = open(args[0], 'rb').read()

    data = strip(data, debug, symbol, tlp, ffi)

    if sys.argv == '-':
        sys.stdout.write(data)
    else:
        fp = open(args[1], 'wb')
        fp.write(data)
        fp.close()
