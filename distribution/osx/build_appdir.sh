# This is rather crude at the moment, needs to be a better script, prefereably
# using some preexisting mechanjism for specifying how files should be copied
# around.
set -o errexit

source functions.sh
#source version.sh
# FIXME: Temporary for dev versions
version=`date -u +"%Y%m%d.%H%M"`
shortversion="Development version: $version (`svnversion -nc ../../`)"


BUILD=build
SPARKLE="Sparkle 1.5b6"
SPARKLE_URL_BIT=${SPARKLE// /%20}
MACOSX_PLUGIN=MacOSX.jar

if ! [ -d "$BUILD/$SPARKLE" ] ; then
  mkdir -p build
  cd build
  curl -o "$SPARKLE.zip" \
  "http://sparkle.andymatuschak.org/files/$SPARKLE_URL_BIT.zip" \
    || exit 1
  unzip -d "$SPARKLE" "$SPARKLE.zip"
  cd ..
fi

# FIXME: Perhaps we should include this in the repos as it is more obscure than
# the other things we depend on and therefore more likely to go away
# from: http://www.seph.dk/uncategorized/new-mac-os-x-plugin-for-jedit/
if ! [ -f $BUILD/$MACOSX_PLUGIN ] ; then
  mkdir -p build
  cd build
  curl -O http://www.seph.dk/download/$MACOSX_PLUGIN
  cd ..
fi

sparkle_feed=http://download.transterpreter.org/appcast/mac-dev.xml

outputDir="output/"
rezDir="Transterpreter.app/Contents/Resources"

sysFrameworks="/System/Library/Frameworks"
javaDir=$rezDir
binaryFiles="ilibr kmakef kroc mkoccdeps occbuild plinker.pl tce-dump.pl tranx86"
libFiles="libSDL*.dylib libplayer*.dylib liboccam_*.a tvm/liboccam_tvm_*.dylib libkrocif.a libccsp.a"

# Make the App dir
makedir "$outputDir"
# Various Transterpreter stuff (Platform dependent)
copydir "skel/Transterpreter.app" "$outputDir/Transterpreter.app"
copyfile \
  "$sysFrameworks/JavaVM.framework/Resources/MacOS/JavaApplicationStub"\
  "$outputDir/Transterpreter.app/Contents/MacOS/"
# Includes
copydir "install/include/tvm" "$outputDir/$rezDir/include/tvm"
copydir "install/include/kroc" "$outputDir/$rezDir/include/kroc"
# Precompiled libraries
copydir "install/share/tvm" "$outputDir/$rezDir/share/tvm"
copydir "install/share/kroc" "$outputDir/$rezDir/share/kroc"
copydir "install-tvm-ppc/share/tvm" "$outputDir/$rezDir/share/tvm-ppc"
# Binary bits
mkdir -p "$outputDir/$rezDir/bin/"
for f in $binaryFiles; do
  copyfile "install/bin/$f" "$outputDir/$rezDir/bin/"
done
lipo -create "install/bin/occ21" "install-tvm-ppc/bin/occ21" \
  -output "$outputDir/$rezDir/bin/occ21"
lipo -create "install/bin/tvm" "install-tvm-ppc/bin/tvm" \
  -output "$outputDir/$rezDir/bin/tvm"
# Library bits
mkdir -p "$outputDir/$rezDir/lib/"
for f in $libFiles; do
  copyfile "install/lib/$f" "$outputDir/$rezDir/lib/"
done
# Arduino bits
mkdir -p "$outputDir/$rezDir/share/tvm-arduino/firmware"
mkdir -p "$outputDir/$rezDir/share/tvm-arduino/plumbing-include"
copydir "install-avr/share/tvm" "$outputDir/$rezDir/share/tvm-arduino"
copyfile "install-avr/bin/avr-occbuild" "$outputDir/$rezDir/bin"
copyfile "build/kroc-tvm-avr-wrapper/tvm-arduino.hex" "$outputDir/$rezDir/share/tvm-arduino/firmware"
copyfile "../../tvm/arduino/binary-to-ihex" "$outputDir/$rezDir/bin/"
copyfile "../../tvm/arduino/read-arduino" "$outputDir/$rezDir/bin/"
copyfile "install/bin/avrdude" "$outputDir/$rezDir/bin/"
copyfile "install/etc/avrdude.conf" "$outputDir/$rezDir/lib/"
copyfile "../../tvm/arduino/occam/plumbing.module" "$outputDir/$rezDir/share/tvm-arduino/plumbing-include"
copyfile "../../tvm/arduino/occam/wiring.module" "$outputDir/$rezDir/share/tvm-arduino/plumbing-include"
copyfile "../../tvm/arduino/occam/avr.module" "$outputDir/$rezDir/share/tvm-arduino/plumbing-include"
copyfile "../../tvm/arduino/occam/iom328p.inc" "$outputDir/$rezDir/share/tvm-arduino/plumbing-include"
mkdir -p "$outputDir/plumbing"
copyfile "../../tvm/arduino/occam/ch*.occ" "$outputDir/plumbing"
# jEdit things
copydir_addexcludes "build/jedit/jedit-program/" "$outputDir/$javaDir/jEdit/" \
  "LatestVersion.jar"
copyfile "build/occPlug/OccPlug.jar" \
  "$outputDir/$javaDir/jEdit/jars/"
copyfile "build/macupdater/JEditSparklePlugin.jar" \
  "$outputDir/$javaDir/jEdit/jars/"
copyfile "../common/jEdit/properties.props" \
  "$outputDir/$rezDir/jEdit/properties/"
patch "$outputDir/$javaDir/jEdit/modes/catalog" < "../common/jEdit/catalog.patch" 
copyfile "../common/jEdit/occam.xml" "$outputDir/$javaDir/jEdit/modes/"
#copydir "../common/jEdit-plugin/dependencies" "$outputDir/$javaDir/jEdit/jars"
copyfile "build/$MACOSX_PLUGIN" "$outputDir/$javaDir/jEdit/jars"

# Licenses
#copydir "common-files/licenses" "$outputDir/$rezDir/licenses"

# Sparkle
copydir "build/$SPARKLE/Sparkle.framework" "$outputDir/$rezDir/../Frameworks/Sparkle.framework"

# Examples etc (ie course dir)
copydir_addexcludes "../../modules/course" "$outputDir/course" \
  "Makefile\.in;Makefile\.am;\.deps;SConscript;aclocal.m4;autom4te\.cache;config\.sub;config\.guess;configure;configure.ac;install-sh;missing"

# Non-course examples
#copydir "../..//examples" "$outputDir/course/examples"

# Readme file
copyfile "skel/README.rtf" "$outputDir/"

#Move those to somewhere sensible
#copyfile "$outputDir/$rezDir/bin/occOPENGL.inc" "$outputDir/$rezDir/lib"
#copyfile "$outputDir/$rezDir/bin/occSDL.inc" "$outputDir/$rezDir/lib"


echo "Built bundle version: $version ('$shortversion')"
