#!/bin/bash

ROOT=$PWD
USER=clj
COMMIT=bf92c8da7c1f50b423e6
SCOMMIT=${COMMIT:0:7}

if ! [ -d build/winsparkle ] ; then
  mkdir -p build/winsparkle
  cd build
   curl --insecure -o $USER-winsparkle-$COMMIT.tar.gz -L \
     http://github.com/$USER/winsparkle/tarball/$COMMIT \
   || exit 1
  cd winsparkle
  tar -xvzf ../$USER-winsparkle-$COMMIT.tar.gz
  cd $USER-winsparkle-$SCOMMIT
  cd 3rdparty
  /c/Program\ Files/Microsoft\ Visual\ Studio\ 9.0/VC/vcpackages/vcbuild.exe \
    WinSparkleDeps.sln
  cd ..
  /c/Program\ Files/Microsoft\ Visual\ Studio\ 9.0/VC/vcpackages/vcbuild.exe \
    WinSparkle.sln
  cp Release/WinSparkle.dll ..
fi

cd $ROOT  
cd ../../tools/winupdater

ant -Djedit.install.dir=$ROOT/build/jedit/jedit-program/ \
    -Dinstall.dir=$ROOT/build/winupdater \
    -Dwinsparkle.dir=$ROOT/build/winsparkle/$USER-winsparkle-$SCOMMIT/

