#!/bin/sh
# Test file.get.options using the getopt program.

getopt=./getoptlong
getoptlong=./getoptlong

echo "These should all work:"
$getopt
$getopt -x
$getopt -xy
$getopt -asss
$getopt -a sss
$getopt -asss -bsss
$getopt -a sss -b sss
$getopt -a -a -b -b
$getopt -axy
$getopt -xyaxy
$getopt foo bar
$getopt --
$getopt -a foo -x
$getopt -x -- -g
$getopt -- -g
$getopt -x -ya blah baz -g
$getopt -x -ya blah -- -g
$getoptlong --foo
$getoptlong --foo --bar
$getoptlong --foo -x
$getoptlong --baz arg
$getoptlong --baz arg --quux arg
$getoptlong --baz=arg --quux=arg
$getoptlong --baz=arg --quux arg
$getoptlong --baz --unknown
$getoptlong -- --unknown
$getoptlong foo --unknown
$getoptlong --foo --quux arg --bar --baz arg2
$getoptlong --quux=

echo "These should all fail:"
$getopt -g
$getopt -g -g
$getopt -a
$getopt --unknown
$getoptlong --unknown2
$getoptlong --quux
