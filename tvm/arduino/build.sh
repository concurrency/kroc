#!/bin/sh -e
# Compile the firmware for the Arduino.

cleanup() 
{
	make -k distclean || true
	unset ACLOCAL
	autoreconf -vfi
}

MCUS="atmega328p atmega1280"
FCPUS="16000000 8000000"

for mcu in $MCUS
do
	for fcpu in $FCPUS
	do
		cleanup
		./configure \
			--host=avr \
			--with-mcu=$mcu \
			--with-fcpu=$fcpu \
			"$@"
		make
		make firmware.hex
	done
done

