#!/bin/bash

# Formatted strings for unique directory names
YMD=$(date +"%Y%m%d")
YMDHM=$(date +"%Y%m%d%H%M")
YMDHMS=$(date +"%Y%m%d%H%M%S")

asterisks () 
{
  echo "*********************************************"
}

header () 
{ 
  echo
  asterisks
  echo "$1"
  asterisks
  echo
}

remove()
{
  if [ -d $1 ]; then
    rm -rf $1
  fi
}

create()
{
  if [ ! -d $1 ]; then
    mkdir -p $1
  fi
}

remove_and_create()
{
 	remove $1
	create $1
}

copy()
{
	echo "COPYING [$1] TO [$2]"
	cp $1 $2
}

# Where this should happen in the filesystem
TEMP=/tmp

# Package variables
PACKAGE_NAME=concurrency-avr
PACKAGE_VERSION=1
PACKAGE_OUTPUT="$PACKAGE_NAME"_"$PACKAGE_VERSION"_"all.deb"
PACKAGE_BUILD=$TEMP/$PACKAGE_NAME

# Where we check the code out to
SVN="$TEMP/dpkg-$YMD"

# Where to do the obj-build
OBJ="$SVN/obj"

# Where things end up
FIN=usr/local/occam/arduino
# Where things think they end up
FINAL=/$FIN

DEST=$TEMP/$PACKAGE_NAME
DEST_ROOT=$DEST/$FIN
DEST_BIN=$DEST_ROOT/bin
DEST_LIB=$DEST_ROOT/lib

if [ "$1" = "checkout" ]; then

  #######
  header "Creating destination directory: $DEST"
  #######
	remove_and_create $DEST
  
  #######
  header "Removing old SVN directory if it exists"
  #######
	remove $SVN

  #######
  header "Checking out fresh SVN trunk"
  #######
  svn co \
		http://projects.cs.kent.ac.uk/projects/kroc/svn/kroc/trunk \
		$SVN
fi

if [ "$1" = "autoreconf" ]; then
  #######
  header "Doing 'autoreconf -vfi'"
  #######
  pushd $SVN
    autoreconf -vfi
  popd
fi

if [ "$1" = "configure" ]; then
  #######
  header "Doing configure"
  #######
  pushd $SVN
    mkdir -p $OBJ
    pushd $OBJ
      ../configure \
        --prefix=$FINAL \
        --with-toolchain=tvm \
        --target=avr \
        --with-wrapper=arduino
    popd
  popd
fi

if [ "$1" = "build" ]; then
  
  #######
  header "Doing make ; make install"
  #######
  pushd $OBJ
    make 
    make DESTDIR=$DEST install
  popd
  
  #######
  header "Building Arduino wrapper"
  #######
  pushd $SVN/tvm/arduino
    ./build.sh
    make tvm-arduino.hex
  popd
  
#
# END if [ "$1" = "build" ]; then
#
fi

if [ "$1" = "install" ]; then
	pushd $OBJ
		make
		make install DESTDIR=$FINAL
	popd
fi

if [ "$1" = "copy" ]; then
  #######
  header "Copying Plumbing files"
  #######
  
  pushd $SVN/tvm/arduino
    create $DEST_FIN/bin
    copy binary-to-ihex          $DEST_BIN
    copy arduinocc               $DEST_BIN
    copy io-header-to-occam      $DEST_BIN
    copy read-arduino            $DEST_BIN
		
    copy tvm-arduino.hex         $DEST_ROOT
  
    create $DEST_FIN/lib
    copy occam/avr.module        $DEST_LIB 
    copy occam/font8x8.inc       $DEST_LIB 
    copy occam/iom328p.inc       $DEST_LIB
    copy occam/plumbing.module   $DEST_LIB
    copy occam/wiring.module     $DEST_LIB 
  popd
fi

if [ "$1" = "writeconfig" ]; then

  config ()
  {
    echo $2 >> $1
  }
  
  #######
  header "Writing conf script"
  #######
  pushd $DEST_BIN
    CONFIG="arduinocc-environment.sh"
    FINALARDUINO="$FINAL"
    echo > $CONFIG
    config $CONFIG "TVM_INST_ROOT=$FINALARDUINO" 
    config $CONFIG "TVM_BYTECODE_ADDR=0x5000"
    config $CONFIG "TVM_F_CPU=16000000"
    config $CONFIG "TVM_UPLOAD_RATE=57600"
    config $CONFIG "TVM_MCU=m328p"
    config $CONFIG ""  
    config $CONFIG "# avrdude"
    config $CONFIG "TVM_AVRDUDE_FIRMWARE_FLAGS=\"-V -F -p \$TVM_MCU\""
    config $CONFIG "TVM_AVRDUDE_CODE_FLAGS=\"-V -F -p \$TVM_MCU -b \$TVM_UPLOAD_RATE -c arduino\""
    config $CONFIG ""
    config $CONFIG "TVM_ARDUINO_FIRMWARE=$FINALARDUINO/tvm-arduino.hex"
  
    chmod 755 $CONFIG
  popd
  
  #######
  header "Writing deployment version file"
  #######
  pushd $DEST_ROOT
    VER="deployment.version"
    echo "$YMDHMS" > $VER
  popd
fi

if [ "$1" = "bundle" ]; then
  #######
  header "Packaging"
  #######
	pushd $PACKAGE_BUILD
		remove_and_create DEBIAN 

		for f in `ls $SVN/distribution/deb-pkg/DEBIAN.in` ; do
			copy $SVN/distribution/deb-pkg/DEBIAN.in/$f DEBIAN
		done


		pushd DEBIAN
			sed "s/@PACKAGEVERSION@/$YMD/g" control.in > control
			
			sed -e "s#@DSTBIN@#$FINAL/bin#g" postinst.in > postinst

			rm control.in
			rm postinst.in
		
			chmod 755 preinst 
			chmod 755 postinst
			chmod 755 prerm
			chmod 755 postrm

		popd

	popd
	
	# Get into temp		
	pushd $TEMP
		dpkg --build $PACKAGE_NAME ./
	popd
		
fi


# Upload Variables
SCPHOST=unhosting.org
SCPPATH=/data/www/org/transterpreter/www/site/downloads/lin/
DOWNLOADURL="http://www.transterpreter.org/downloads/lin"

if [ "$1" = "upload" ]; then
  ####### 
  header "Uploading Debian package"
  #######
  if [ "$2" = "" ]; then
    echo
    echo "Usage:"
    echo " ./make... upload <username>"
    echo
    exit
  fi 
 
  pushd $TEMPORARY

    if [ -f $OUTPUTPACKAGENAME ]; then
      scp $OUTPUTPACKAGENAME $2@$SCPHOST:$SCPPATH

 			echo  
      echo "FILE DOWNLOAD FROM:"
      echo
      echo "$DOWNLOADURL/$OUTPUTPACKAGENAME"
      echo 
    fi
fi  

if [ "$1" = "all" ]; then
  #######
  header "Doing everything..."
  #######
	$0 checkout
	$0 autoreconf
	$0 configure
  $0 build
	$0 install
  $0 copy
  $0 writeconfig
  $0 bundle

  if [ ! "$2" = "" ]; then
    $0 upload $2
  fi 
fi  

if [ "$1" = "" ]; then
  header "Build options"
	echo " [ checkout    ]  Checkout a fresh tree."
	echo " [ autoreconf  ]  autoreconf the tree."
	echo " [ configure   ]  configure the tree."
  echo " [ build       ]  Build the tree and tvm wrapper for the Arduino."
  echo " [ install     ]  Run 'make install'."
  echo " [ copy        ]  Copy Arduino scripts into binary path."
  echo " [ writeconfig ]  Generate Arduino support config."
  echo " [ bundle      ]  Generate tarball."
  echo " [ all         ]  From source checkout to tarballing. "
  echo " [ all <uname> ]  Upload after completion."
  echo
  exit
fi

exit

