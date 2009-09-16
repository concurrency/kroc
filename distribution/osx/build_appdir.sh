# This is rather crude at the moment, needs to be a better script, prefereably
# using some preexisting mechanjism for specifying how files should be copied
# around.

source functions.sh
source version.sh

outputDir="output/"
rezDir="Transterpreter.app/Contents/Resources"
sysFrameworks="/System/Library/Frameworks"
javaDir=$rezDir
binaryFiles="ilibr kmakef kroc mkoccdeps occ21 netbard occbuild plinker.pl tce-dump.pl tranx86 trapns tvm"
libFiles="libSDL*.dylib libplayer*.dylib"

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
# Binary bits
mkdir -p "$outputDir/$rezDir/bin/"
for f in $binaryFiles; do
  copyfile "install/bin/$f" "$outputDir/$rezDir/bin/"
done
# Library bits
mkdir -p "$outputDir/$rezDir/lib/"
for f in $libFiles; do
  copyfile "install/lib/$f" "$outputDir/$rezDir/lib/"
done
# Arduino bits
copyfile "build/kroc-tvm-avr-wrapper/tvm-arduino.hex" "$outputDir/$rezDir/lib/"
copyfile "../../tvm/arduino/avrdude.conf" "$outputDir/$rezDir/lib/"
copyfile "../../tvm/arduino/binary-to_ihex" "$outputDir/$rezDir/bin/"
copyfile "../../tvm/arduino/read-arduino" "$outputDir/$rezDir/bin/"
copyfile "../../tvm/arduino/reset-arduino" "$outputDir/$rezDir/bin/"
copyfile "build/arduino-0016/hardware/tools/avr/bin/avrdude" "$outputDir/$rezDir/bin/"
# jEdit things
copydir "build/jedit/jedit-program/" "$outputDir/$javaDir/jEdit/"
copyfile "build/occPlug/OccPlug.jar" \
  "$outputDir/$javaDir/jEdit/jars/"
#copyfile "common-files/jEdit-plugin/OccPlug.props" "$outputDir/$rezDir/jEdit/properties/"
copyfile "../common/jEdit-plugin/catalog" "$outputDir/$javaDir/jEdit/modes/"
copyfile "../common/jEdit-plugin/occam.xml" "$outputDir/$javaDir/jEdit/modes/"
copydir "../common/jEdit-plugin/dependencies" "$outputDir/$javaDir/jEdit/jars"

# Licenses
#copydir "common-files/licenses" "$outputDir/$rezDir/licenses"

# Examples etc (ie course dir)
copydir_addexcludes "../../modules/course" "$outputDir/course" \
  "Makefile\.in;Makefile\.am;\.deps;SConscript"

# Non-course examples
#copydir "../..//examples" "$outputDir/course/examples"

# Readme file
copyfile "skel/README.rtf" "$outputDir/"

#Move those to somewhere sensible
#copyfile "$outputDir/$rezDir/bin/occOPENGL.inc" "$outputDir/$rezDir/lib"
#copyfile "$outputDir/$rezDir/bin/occSDL.inc" "$outputDir/$rezDir/lib"
