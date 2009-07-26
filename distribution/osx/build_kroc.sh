PKG_CONFIG=pkg-config-0.23
SDL=SDL-1.2.13
PLAYER=player-2.1.2
STAGE=stage-2.1.1

INSTALL=$PWD/install

PATH=$PATH:$PWD/install/bin
export PATH


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
  ./configure --prefix=$INSTALL
  make install
  cd ../..
fi

pkg-config playerc
if [ "$?" == "1" ] ; then
  mkdir -p build
  cd build
  curl -O \
    http://kent.dl.sourceforge.net/sourceforge/playerstage/$PLAYER.tar.bz2 \
    || exit 1
  tar -xvjf $PLAYER.tar.bz2
  cd $PLAYER
  ./configure --prefix=$INSTALL
  make install
  cd ../..
fi

#pkg-config stage
#if [ "$?" == "1" ] ; then
#  mkdir -p build
#  cd build
#  curl -O \
#    http://kent.dl.sourceforge.net/sourceforge/playerstage/$STAGE.tar.bz2 \
#    || exit 1
#  tar -xvjf $STAGE.tar.bz2
#  cd $STAGE
#  ./configure --prefix=$INSTALL
#  make install
#  cd ../..
#fi

cd ../../

autoreconf -v -f -i

make distclean

./configure --with-toolchain=kroc --prefix=$INSTALL
make
make install

make distclean

./configure --with-toolchain=tvm --prefix=$INSTALL
make
make install

