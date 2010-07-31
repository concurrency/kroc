ROOT=$PWD

cd $ROOT
cd ../../tools/macupdater

ant -Djedit.install.dir=$ROOT/build/jedit/jedit-program/ \
    -Dinstall.dir=$ROOT/build/macupdater
