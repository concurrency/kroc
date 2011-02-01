#!/bin/bash
# Arduino package could perhaps be replaced with a avr-gcc package?
# FIXME: 0017 is out, perhaps upgrade

platform=`uname -s`
windows=`expr "$platform" : 'MINGW' | grep -v 0`

if [ $windows ] ; then
	ARDUINO=arduino-0018
	ARDUINO_ZIP=$ARDUINO.zip
else
	ARDUINO=arduino-0016
	ARDUINO_ZIP=$ARDUINO-mac.zip
fi


INSTALL=$PWD/install
BUILD=$PWD/build

if ! [ -d $BUILD/$ARDUINO ] ; then
  mkdir -p build
  cd build
  curl -O \
    http://arduino.googlecode.com/files/$ARDUINO_ZIP \
    || exit 1
  unzip $ARDUINO_ZIP
  #cd $ARDUINO
fi

cd $BUILD
mkdir -p kroc-tvm-avr
cd kroc-tvm-avr

if [ $windows ] ; then
  ../../../../configure --target=avr --with-toolchain=tvm --prefix=$INSTALL-avr
else
  ../../../../configure --target=avr --with-toolchain=tvm \
     --prefix=$INSTALL-avr \
     CFLAGS="-arch i386 -mmacosx-version-min=10.4" \
     CXXFLAGS="-arch i386 -mmacosx-version-min=10.4" \
     LDFLAGS="-arch i386 -mmacosx-version-min=10.4" \
     OBJCFLAGS="-arch i386 -mmacosx-version-min=10.4"
fi
make
make install
cd $BUILD/kroc-tvm-avr/modules/inmoslibs/libsrc
make
make install

OLD_PATH=$PATH
PATH=$PATH:$BUILD/$ARDUINO/hardware/tools/avr/bin
cd $BUILD
cd ../../../tvm/arduino
unset ACLOCAL
autoreconf -v -f -i
cd $BUILD
mkdir -p arduino_firmware
cd arduino_firmware
../../../../tvm/arduino/build_firmware.sh
cd $BUILD
cd ..
