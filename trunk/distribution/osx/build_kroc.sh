PKG_CONFIG=pkg-config-0.23
SDL=SDL-1.2.14
PLAYER=player-2.1.2
STAGE=stage-2.1.1
#FIXME: Possibly do SDL_sound
#http://icculus.org/SDL_sound/


INSTALL=$PWD/install
BUILD=$PWD/build

#PATH=$PATH:$PWD/install/bin
OLD_PATH=$PATH
PATH=/bin:/usr/bin:/usr/X11/bin:$PWD/install/bin
#export PATH


which -s pkg-config
if [ "$?" == "1" ] ; then
  mkdir -p build
  cd build
  curl -O http://pkgconfig.freedesktop.org/releases/$PKG_CONFIG.tar.gz || exit 1
  tar -xvzf $PKG_CONFIG.tar.gz
  cd $PKG_CONFIG
  ./configure --prefix=$INSTALL
  make install
  cd ../..
fi

which -s sdl-config
if [ "$?" == "1" ] ; then
  mkdir -p build
  cd build
  curl -O http://www.libsdl.org/release/$SDL.tar.gz || exit 1
  tar -xvzf $SDL.tar.gz
  cd $SDL
  # Compiling with newer xquartz (xquartz.macosforge.org)
  # does not work w/o this patch
  #patch -p0 < ../../patch-SDL_x11gl_c.h.diff
  ./configure --prefix=$INSTALL \
   CFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   CXXFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   LDFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   OBJCFLAGS="-arch i386 -mmacosx-version-min=10.4"
  make install
  cd ../..
fi

pkg-config playerc
if [ "$?" == "1" ] ; then
  mkdir -p build
  cd build
  curl -L -O \
    http://prdownloads.sourceforge.net/playerstage/$PLAYER.tar.bz2 \
    || exit 1
  tar -xvjf $PLAYER.tar.bz2
  cd $PLAYER
  ./configure --prefix=$INSTALL --disable-jplayer \
   CFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   CXXFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   LDFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   OBJCFLAGS="-arch i386 -mmacosx-version-min=10.4"
  make install
  cd ../..
fi

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

cd $BUILD
mkdir kroc-ccsp
cd kroc-ccsp

../../../../configure --with-toolchain=kroc --prefix=$INSTALL \
   CFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   CXXFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   LDFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   OBJCFLAGS="-arch i386 -mmacosx-version-min=10.4"
make
make install

cd $BUILD
mkdir kroc-tvm-posix
cd kroc-tvm-posix

../../../../configure --with-toolchain=tvm --prefix=$INSTALL \
   CFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   CXXFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   LDFLAGS="-arch i386 -mmacosx-version-min=10.4" \
   OBJCFLAGS="-arch i386 -mmacosx-version-min=10.4"
make
make install

PATH=$OLD_PATH

darwin_major=`uname -r | awk -F. '{ print $1 }'`
cd $BUILD
mkdir kroc-tvm-posix-ppc
cd kroc-tvm-posix-ppc
CFLAGS="-isysroot /Developer/SDKs/MacOSX10.4u.sdk \
  -I/usr/lib/gcc/powerpc-apple-darwin$darwin_major/4.2.1/include/ \
  -mmacosx-version-min=10.4" \
  CC=powerpc-apple-darwin$darwin_major-gcc-4.2.1 \
  TARGET_CC=powerpc-apple-darwin$darwin_major-gcc-4.2.1 \
  ../../../../configure --host=powerpc-apple-darwin$darwin_major \
  --target=powerpc-apple-darwin$darwin_major \
  --build=powerpc --with-toolchain=tvm \
  --prefix=$INSTALL-tvm-ppc
make
make install
