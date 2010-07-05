#!/bin/bash
set -o errexit

source ../osx/functions.sh

version=`date -u +"%Y%m%d.%H%M"`
shortversion="Development version: $version (`svnversion -nc ../../`)"

BUILD=build

output_dir="output"
app_dir="$output_dir/Transterpreter"
bin_dir="$app_dir/bin"


binary_files="ilibr.exe mkoccdeps.exe occbuild plinker.pl tce-dump.pl tvm.exe"
library_files="*.dll"

makedir "$app_dir"
makedir "$bin_dir"

# binary files (KRoC/tvm related)
for f in $binary_files; do
	copyfile "install/bin/$f" "$bin_dir"
done

# library files (KRoC/tvm related)
for f in $library_files; do
	copyfile "install/lib/tvm/$f" "$bin_dir"
done

# posix module includes
posix_inc_dir="$app_dir/posix/include"
makedir "$posix_inc_dir"
copydir "install/share/tvm/vtinclude" "$posix_inc_dir"

# posix module libs
posix_lib_dir="$app_dir/posix/lib"
makedir "$posix_lib_dir"
copydir "install/share/tvm/vtlib" "$posix_lib_dir"

# posix firmware
posix_fw_dir="$app_dir/posix/firmware"
makedir "$posix_fw_dir"
copyfile "install/share/tvm/firmware/tvm-posix.tbc" "$posix_fw_dir"

# SDL
copyfile "install/bin/SDL.dll" "$bin_dir"

# Arduino bits
arduino_fw_dir="$app_dir/arduino/firmware"
makedir "$arduino_fw_dir"
copyfile "build/kroc-tvm-avr-wrapper/tvm-arduino.hex" "$arduino_fw_dir"
copyfile "../../tvm/arduino/binary-to-ihex" "$bin_dir"
copyfile "../../tvm/arduino/read-arduino" "$bin_dir"
arduino_inc_dir="$app_dir/arduino/include"
makedir "$arduino_inc_dir"
copyfile "../../tvm/arduino/occam/plumbing.module" "$arduino_inc_dir"
copyfile "../../tvm/arduino/occam/wiring.module" "$arduino_inc_dir"
copyfile "../../tvm/arduino/occam/avr.module" "$arduino_inc_dir"
copyfile "../../tvm/arduino/occam/iom328p.inc" "$arduino_inc_dir"
# FIXME: Should this live in the root for better visibility???
arduino_plumbing_dir="$app_dir/arduino/plumbing"
makedir "$arduino_plumbing_dir"
copyfile "../../tvm/arduino/occam/ch*.occ" "$arduino_plumbing_dir"

# arduino module includes
arduino_inc_dir="$app_dir/arduino/include"
makedir "$arduino_inc_dir"
copydir "install-avr/share/tvm/avr-vtinclude" "$arduino_inc_dir"

# arduino module libs
arduino_lib_dir="$app_dir/arduino/lib"
makedir "$arduino_lib_dir"
copydir "install-avr/share/tvm/avr-vtlib" "$arduino_lib_dir"

# avrdude
copyfile "install-avrdude/bin/avrdude.exe" "$bin_dir"
# FIXME: Figure out if we are going to include the giveio stuff

# jEdit
jedit_dir="$app_dir/jEdit"
copydir_addexcludes "build/jedit/jedit-program/" "$jedit_dir" "LatestVersion.jar"
copyfile "build/occPlug/OccPlug.jar" "$jedit_dir/jars/"
copyfile "../common/jEdit/occam.xml" "$jedit_dir/modes/"
patch "$jedit_dir/modes/catalog" < "../common/jEdit/catalog.patch"

echo "Built app version: $version ('$shortversion')"
