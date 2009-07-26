# This is rather crude at the moment, needs to be a better script, prefereably
# using some preexisting mechanjism for specifying how files should be copied
# around.

source functions.sh
source version.sh

outputDir="output/"
rezDir="Transterpreter.app/Contents/Resources"

# Make the App dir
makedir "$outputDir"
# Various Transterpreter stuff (Platform dependent)
copydir "files/Transterpreter.app" "$outputDir/Transterpreter.app"
# Precompiled libraries
copydir "common-files/lib" "$outputDir/$rezDir/lib"
# jEdit things
copydir "common-files/jEdit" "$outputDir/$rezDir/jEdit/"
copyfile "common-files/jEdit-plugin/OccPlug.jar" "$outputDir/$rezDir/jEdit/jars/"
#copyfile "common-files/jEdit-plugin/OccPlug.props" "$outputDir/$rezDir/jEdit/properties/"
copyfile "common-files/jEdit-plugin/catalog" "$outputDir/$rezDir/jEdit/modes/"
copyfile "common-files/jEdit-plugin/occam.xml" "$outputDir/$rezDir/jEdit/modes/"
copydir "common-files/jEdit-plugin/dependencies" "$outputDir/$rezDir/jEdit/jars"
# Licenses
copydir "common-files/licenses" "$outputDir/$rezDir/licenses"

# Examples etc (ie course dir)
copydir_addexcludes common-files/course "$outputDir/course" \
  "Makefile\.am;\.deps"

# Non-course examples
copydir "common-files/examples" "$outputDir/course/examples"

# Readme file
copyfile "files/README.rtf" "$outputDir/"

#Move those to somewhere sensible
copyfile "$outputDir/$rezDir/bin/occOPENGL.inc" "$outputDir/$rezDir/lib"
copyfile "$outputDir/$rezDir/bin/occSDL.inc" "$outputDir/$rezDir/lib"
