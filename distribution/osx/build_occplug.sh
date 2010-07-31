ROOT=$PWD

if ! [ -d build/jedit ] ; then
  mkdir -p build/jedit
  cd build
  curl -L -O \
    http://prdownloads.sourceforge.net/jedit/jedit4.3.2install.jar \
    || exit 1
  cd jedit
  jar -xvf ../jedit4.3.2install.jar
  mkdir -p jedit-program
  cd jedit-program
  tar -xvjf ../installer/jedit-program.tar.bz2
  cd $ROOT/build/
  curl -L -O \
    http://prdownloads.sourceforge.net/jedit-plugins/ErrorList-1.9-bin.zip \
    || exit 1
  mkdir -p jedit/jedit-program/jars
  cd jedit/jedit-program/jars
  unzip ../../../ErrorList-1.9-bin.zip
fi

cd $ROOT
cd ../../tools/occplug

ant -Djedit.install.dir=$ROOT/build/jedit/jedit-program/ \
    -Dinstall.dir=$ROOT/build/occPlug -Dbuild.dir=$ROOT/build/occPlug-build
