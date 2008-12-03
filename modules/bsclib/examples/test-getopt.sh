#!/bin/sh
# Test file.get.options using the getopt program.

echo "These should all work:"
./getopt
./getopt -x
./getopt -xy
./getopt -asss
./getopt -a sss
./getopt -asss -bsss
./getopt -a sss -b sss
./getopt -a -a -b -b
./getopt -axy
./getopt -xyaxy
./getopt foo bar
./getopt --
./getopt -a foo -x
./getopt -x -- -g
./getopt -- -g
./getopt -x -ya blah baz -g
./getopt -x -ya blah -- -g

echo "These should all fail:"
./getopt -g
./getopt -g -g
./getopt -a
./getopt --unknown

