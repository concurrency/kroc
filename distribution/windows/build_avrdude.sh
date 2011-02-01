#!/bin/bash

platform=`uname -s`
windows=`expr "$platform" : 'MINGW' | grep -v 0`

ROOT=$PWD
BUILD=$PWD/build
INSTALL=$PWD/install-avrdude
LIBUSB=libusb-0.1.12
VERSION=5.10

if ! [ $windows ] ; then
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
fi

if ! [ -d $BUILD/avrdude-$VERSION ] ; then
  mkdir -p $BUILD/avrdude-$VERSION
  cd build
  curl -L -O \
    http://download.savannah.gnu.org/releases-noredirect/avrdude/avrdude-$VERSION.tar.gz \
    || exit 1
  tar -xvzf avrdude-$VERSION.tar.gz
  cd avrdude-$VERSION
  if [ $windows ] ; then
    ./configure LDFLAGS="-static" --prefix=$INSTALL \
      --enable-versioned-doc=no
  else
  ./configure --prefix=$INSTALL \
     CFLAGS="-arch i386 -mmacosx-version-min=10.4" \
     CXXFLAGS="-arch i386 -mmacosx-version-min=10.4" \
     LDFLAGS="-arch i386 -mmacosx-version-min=10.4" \
     OBJCFLAGS="-arch i386 -mmacosx-version-min=10.4"
  fi
  make
  make install
else
  echo "Looks like avrdude has already been made"
fi
