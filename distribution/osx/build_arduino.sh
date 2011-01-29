# Arduino package could perhaps be replaced with a avr-gcc package?
# FIXME: 0017 is out, perhaps upgrade
LIBUSB=libusb-0.1.12
ARDUINO=arduino-0016
AVRDUDE=avrdude-5.10


INSTALL=$PWD/install
BUILD=$PWD/build

if ! [ -d $BUILD/$ARDUINO ] ; then
  mkdir -p build
  cd build
  curl -O \
    http://arduino.googlecode.com/files/$ARDUINO-mac.zip \
    || exit 1
  unzip $ARDUINO-mac.zip
  #cd $ARDUINO
fi

if ! [ -d $BUILD/$LIBUSB ] ; then
  mkdir -p build
  cd build
  curl -L -O \
    http://prdownloads.sourceforge.net/libusb/$LIBUSB.tar.gz \
    || exit 1
  tar -xvzf $LIBUSB.tar.gz
  cd $LIBUSB
  # FIXME: For some reason shared libs do not get passed -arch i386
  ./configure --prefix=$INSTALL --enable-shared=no\
   CFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   CXXFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   LDFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   OBJCFLAGS="-arch i386 -mmacosx-version-min=10.4"
  make
  make install
fi

# http://download.savannah.gnu.org/releases-noredirect/avrdude/$AVRDUDE.tar.gz \
if ! [ -d $BUILD/$AVRDUDE ] ; then
  mkdir -p build
  cd build
  curl -L -O \
  http://download.savannah.gnu.org/releases/avrdude/$AVRDUDE.tar.gz \
    || exit 1
  tar -xvzf $AVRDUDE.tar.gz
  cd $AVRDUDE
  ./configure --prefix=$INSTALL \
   CFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   CXXFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   LDFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   OBJCFLAGS="-arch i386 -mmacosx-version-min=10.4"
  make
  make install
fi

cd $BUILD
mkdir kroc-tvm-avr
cd kroc-tvm-avr

../../../../configure --target=avr --with-toolchain=tvm --prefix=$INSTALL-avr \
   CFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   CXXFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   LDFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   OBJCFLAGS="-arch i386 -mmacosx-version-min=10.4"
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
