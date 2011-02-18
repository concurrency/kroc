#!/usr/bin/env python

import sys

import tenclib

def dump(data):
    parser = tenclib.TEncParser(data)
    tenc   = parser.parse()
    return str(tenc)

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print "Usage: %s <filename>" % sys.argv[0]
        sys.exit(1)

    data   = open(sys.argv[1], 'rb').read()
    print dump(data)
