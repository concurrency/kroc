#!/bin/sh -e
# Compile the firmware for the Arduino.

make -k distclean || true

unset ACLOCAL
autoreconf -vfi

./configure \
	--host=avr \
	"$@"
make
