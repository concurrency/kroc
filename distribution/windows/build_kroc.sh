#!/bin/bash
SDL=SDL-1.2.14
PLAYER=player-2.1.2
STAGE=stage-2.1.1
# FIXME: Arduino package could perhaps be replaced with a avr-gcc package?

#FIXME: Possibly do SDL_sound
#http://icculus.org/SDL_sound/


INSTALL=$PWD/install
BUILD=$PWD/build

PATH=$PATH:$PWD/install/bin
export PATH

sdl-config > /dev/null 2>&1
if [ "$?" == "127" ] ; then
  mkdir -p build
  cd build
  curl -O http://www.libsdl.org/release/$SDL.tar.gz || exit 1
  tar -xvzf $SDL.tar.gz
  cd $SDL
  # Compiling with newer xquartz (xquartz.macosforge.org)
  # does not work w/o this patch
  #patch -p0 < ../../patch-SDL_x11gl_c.h.diff
  ./configure --prefix=$INSTALL
  make install
  cd ../..
fi

#pkg-config playerc
#if [ "$?" == "1" ] ; then
#  mkdir -p build
#  cd build
#  curl -L -O \
#    http://prdownloads.sourceforge.net/playerstage/$PLAYER.tar.bz2 \
#    || exit 1
#  tar -xvjf $PLAYER.tar.bz2
#  cd $PLAYER
#  ./configure --prefix=$INSTALL --disable-jplayer
#  make install
#  cd ../..
#fi

#pkg-config stage
#if [ "$?" == "1" ] ; then
#  mkdir -p build
#  cd build
#  curl -L -O \
#    http://prdownloads.sourceforge.net/playerstage/$STAGE.tar.bz2 \
#    || exit 1
#  tar -xvjf $STAGE.tar.bz2
#  cd $STAGE
#  ./configure --prefix=$INSTALL
#  make install
#  cd ../..
#fi

cd $BUILD
cd ../../../

autoreconf -v -f -i

#cd $BUILD
#mkdir kroc-ccsp
#cd kroc-ccsp
#
#../../../../configure --with-toolchain=kroc --prefix=$INSTALL
#make
#make install

cd $BUILD
mkdir kroc-tvm-posix
cd kroc-tvm-posix

../../../../configure --with-toolchain=tvm --prefix=$INSTALL
make
make install

#cd $BUILD
#mkdir kroc-tvm-posix-ppc
#cd kroc-tvm-posix-ppc
#CFLAGS="-isysroot /Developer/SDKs/MacOSX10.4u.sdk \
#  -I/usr/lib/gcc/powerpc-apple-darwin9/4.2.1/include/ \
#  -mmacosx-version-min=10.4" \
#  CC=powerpc-apple-darwin9-gcc-4.2.1 \
#  ../../../../configure --host=powerpc-apple-darwin9 \
#  --build=powerpc --with-toolchain=tvm \
#  --prefix=$INSTALL-tvm-ppc
## FIXME: Both of these fail at the moment, though they get far enough to be
## useful, which incidentally is up to, but not including, the useful library.
#make
#make install

PATH=$OLD_PATH
