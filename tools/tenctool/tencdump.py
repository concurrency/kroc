import sys

import tenclib

if len(sys.argv) < 2:
    print "Usage: %s <filename>" % sys.argv[0]
    sys.exit(1)

data   = open(sys.argv[1], 'rb').read()
parser = tenclib.TEncParser(data)
tenc   = parser.parse()
print tenc
