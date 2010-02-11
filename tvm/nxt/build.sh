#!/bin/sh -e
# Compile firmware for Lego Mindstorms NXT

make -k distclean || true

unset ACLOCAL
autoreconf -vfi

./configure \
	--host=arm-elf \
	"$@"
make
