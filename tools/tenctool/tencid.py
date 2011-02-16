import sys

import tenclib

if len(sys.argv) < 3:
    print "Usage: %s <tenc filename> <output filename or ->" % sys.argv[0]
    sys.exit(1)

data   = open(sys.argv[1], 'rb').read()
parser = tenclib.TEncParser(data)
tenc   = parser.parse()
if sys.argv == '-':
    sys.stdout.write(tenc.dump_bytes())
else:
    fp = open(sys.argv[2], 'wb')
    fp.write(tenc.dump_bytes())
    fp.close()
