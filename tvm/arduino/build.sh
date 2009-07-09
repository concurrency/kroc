#!/bin/sh -e
# Compile the firmware for the Arduino.

make -k distclean || true
./configure \
	--host=avr \
	"$@"
make
