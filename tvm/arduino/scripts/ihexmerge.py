#!/usr/bin/python

# ihexmerge.py: Christian L. Jacobsen, 2011

# ihexmerge: merge to intel hex files, filling in the 
# gap between them with zero bytes.
# Takes two files as input, produces merged output on std out,

import sys

file1 = open(sys.argv[1], 'r').readlines()
file2 = open(sys.argv[2], 'r').readlines()

end   = int(file1[-2][3:7], 16)
start = int(file2[0][3:7], 16)

# Align the end
end = end + end % 16

for i in file1[:-1]:
    print i.strip()
for i in range(end + 16, start, 16):
    bytes = [0x10, i >> 8, i & 0xFF, 00]
    bytes += [0x00] * 16
    bytes += [(0x100 - (sum(bytes) & 0xFF)) & 0xFF]
    print ":" + "".join(["%02x" % b for b in bytes]).upper()
for i in file2:
    print i.strip()

