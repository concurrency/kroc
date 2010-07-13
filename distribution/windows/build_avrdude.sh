#!/bin/bash
ROOT=$PWD
VERSION=5.10


if ! [ -d build/avrdude-$VERSION ] ; then
  mkdir -p build/avrdude-$VERSION
  cd build
  curl -L -O \
    http://download.savannah.gnu.org/releases-noredirect/avrdude/avrdude-$VERSION.tar.gz \
    || exit 1
  tar -xvzf avrdude-$VERSION.tar.gz
  cd avrdude-$VERSION
  ./configure LDFLAGS="-static" --prefix=$ROOT/install-avrdude --enable-versioned-doc=no
  make
  make install
else
  echo "Looks like avrdude has already been made"
fi
