#!/bin/sh -e
# Compile libtvm for the Arduino.

TVMDIR="../../runtime/libtvm"

CFLAGS="-Os -mmcu=atmega328p"

rm -fr libtvm
mkdir -p libtvm
cd libtvm
../$TVMDIR/configure \
	--host=avr \
	--with-transputer=t2 \
	--disable-occam-pi \
	CFLAGS="$CFLAGS"
make
size libtvm.a
