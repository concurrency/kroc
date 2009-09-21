#!/bin/bash

#version=`grep CFBundleVersion output/Transterpreter.app/Contents/Info.plist -A1 | tail -n1 | sed "s|[[:space:]]*</*string>||g"`
version=`defaults read $PWD/output/Transterpreter.app/Contents/Info CFBundleVersion`

mkdir -p build/zip
cd build/zip
ln -s ../../output Transterpreter
zip -r ../../Transterpreter-dev-$version.zip Transterpreter
