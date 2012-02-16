#!/bin/bash

SCRIPT=$0
ROOT=`dirname $SCRIPT`

CONFDIR=$ROOT/occam/conf
CONFIGURE=$ROOT/configure

targets=`ls $CONFDIR/*.conf.in`

if [ -d build ] ; then
	echo "Please remove the build directory to continue"
	exit 1
fi

for target in $targets
do
	tname=`basename ${target%.*.*}`
	tdir=build/$tname
	mkdir -p $tdir
	source $target
	cd $tdir
	../../$CONFIGURE --host=avr \
			 --with-bytecode-addr=$TVM_BYTECODE_ADDR \
			 --with-mcu=$TVM_GCC_MCU \
			 --with-fcpu=$TVM_F_CPU \
			 --with-firmware-name=$TVM_ARDUINO_FIRMWARE \
			 "$@"
	make
	make firmware.hex
	cd ../../
	mkdir -p output
	cp $tdir/$TVM_ARDUINO_FIRMWARE output/
done
