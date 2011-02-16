import sys

import tenclib

def usage():
    print "Usage: %s [-d] [-t] [-s] [-f] " \
            "<tenc filename> <output filename or ->" % sys.argv[0]
    print "  default stripping is -d and -s"
    print "    -d: debug info"
    print "    -t: TLP info"
    print "    -s: symbol info"
    print "    -d: ffi table"
    sys.exit(1)

if len(sys.argv) < 3 or len(sys.argv) > 7: 
    usage()

if len(sys.argv) == 3:
    debug, tlp, symbol, ffi = False, True, False, True
    args = sys.argv[1:]
else:
    debug, tlp, symbol, ffi = 4 * [True]
    args = sys.argv[1:]
    while args[0] in ['-d', '-t', '-s', '-f']:
        if args[0]   == '-d': debug = False
        elif args[0] == '-t': tlp = False
        elif args[0] == '-s': symbol = False
        elif args[0] == '-f': ffi = False
        else: break
        args = args[1:]
if len(args) > 2:
    usage()

data   = open(args[0], 'rb').read()
parser = tenclib.TEncParser(data)
tenc   = parser.parse()

def remove(tag):
    for e in tenc.elements:
        if e.tag == 'tbcL':
            for e1 in e.elements:
                if e1.tag == tag:
                    e1.elements = []
if not debug: remove('dbgL')
if not tlp: remove('tlpL')
if not symbol: remove('stbL')
if not ffi: remove('ffiL')

if sys.argv == '-':
    sys.stdout.write(tenc.dump_bytes())
else:
    fp = open(args[1], 'wb')
    fp.write(tenc.dump_bytes())
    fp.close()
