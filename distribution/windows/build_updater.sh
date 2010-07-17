#!/bin/bash

ROOT=$PWD

if ! [ -d build/winsparkle ] ; then
  mkdir -p build/winsparkle
  cd build
   curl -O -L \
     http://lyderjacobsen.org/misc/vslavik-winsparkle-v0.2-9-g941987c.zip \
   || exit 1
  cd winsparkle
  unzip ../vslavik-winsparkle-v0.2-9-g941987c.zip
  cd vslavik-winsparkle-941987c
  cd 3rdparty
  /c/Program\ Files/Microsoft\ Visual\ Studio\ 9.0/VC/vcpackages/vcbuild.exe \
    WinSparkleDeps.sln
  cd ..
  /c/Program\ Files/Microsoft\ Visual\ Studio\ 9.0/VC/vcpackages/vcbuild.exe \
    WinSparkle.sln
fi

cd $ROOT  
cd ../../tools/winupdater

ant -Djedit.install.dir=$ROOT/build/jedit/jedit-program/ \
    -Dinstall.dir=$ROOT/build/winupdater \
    -Dwinsparkle.dir=$ROOT/build/winsparkle/vslavik-winsparkle-941987c/

