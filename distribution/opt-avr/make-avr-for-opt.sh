#!/bin/bash

# Formatted strings for unique directory names
YMD=$(date +"%Y-%m-%d")
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

# Source-tree variables
SVNCHECKOUT="$HOME/svn/avr-$YMD"
OBJAVR=$SVNCHECKOUT/obj-avr
ARDUINOWRAPPER=$SVNCHECKOUT/tvm/arduino

# Where things should temporarily end up
# (For the DESTDIR build)
FINAL=opt/occam/arduino
DSTDIR=$HOME/pkg/avr
DSTOPT=$DSTDIR/opt
DSTOCCAM=$DSTDIR/$FINAL
DSTBIN=$DSTDIR/$FINAL/bin
DSTLIB=$DSTDIR/$FINAL/lib
DSTARDUINO=$DSTDIR/$FINAL

# Where our tools will *think* they are installed
# (When moved into place)
FINALDEST=/$FINAL

if [ "$1" = "build" ]; then
  #######
  header "Creating destination directory: $DSTDIR"
  #######
  if [ -d $DSTDIR ]; then
    rm -rf $DSTDIR
  fi
  
  if [ ! -d $DSTDIR ]; then
    mkdir -p $DSTDIR
  fi
  
  #######
  header "Removing old SVN directory if it exists"
  #######
  if [ -d $SVNCHECKOUT ]; then
    rm -rf $SVNCHECKOUT
  fi

  #######
  header "Checking out fresh SVN trunk"
  #######
  svn co http://projects.cs.kent.ac.uk/projects/kroc/svn/kroc/trunk $SVNCHECKOUT
  
  #######
  header "Doing 'autoreconf -vfi'"
  #######
  pushd $SVNCHECKOUT
    autoreconf -vfi
  popd
  
  
  #######
  header "Doing configure"
  #######
  pushd $SVNCHECKOUT
    mkdir -p $OBJAVR
    pushd $OBJAVR
      ../configure \
        --prefix=$FINALDEST \
        --with-toolchain=tvm \
        --target=avr \
        --with-wrapper=arduino
    popd
  popd
  
  #######
  header "Doing make ; make install"
  #######
  pushd $OBJAVR
    make 
    make install DESTDIR=$DSTDIR
  popd
  
  #######
  header "Building Arduino wrapper"
  #######
  pushd $ARDUINOWRAPPER
    ./build.sh
    make tvm-arduino.hex
  popd
  
#
# END if [ "$1" = "build" ]; then
#
fi

if [ "$1" = "copy" ]; then
  #######
  header "Copying Plumbing files"
  #######
  
  pushd $ARDUINOWRAPPER
    cp binary-to-ihex $DSTBIN
    cp arduinocc $DSTBIN
    cp io-header-to-occam $DSTBIN
    cp read-arduino $DSTBIN
    #cp arduino-upload $DSTBIN
    #cp arduino-firmware-upload $DSTBIN
  
    mkdir -p $DSTARDUINO
    cp tvm-arduino.hex $DSTARDUINO
  
    cp occam/avr.module $DSTLIB
    cp occam/font8x8.inc $DSTLIB
    cp occam/iom328p.inc $DSTLIB
    cp occam/plumbing.module $DSTLIB
    cp occam/wiring.module $DSTLIB
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
  pushd $DSTBIN
    CONFIG="arduinocc-environment.sh"
    FINALARDUINO="$FINALDEST"
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
  pushd $DSTOCCAM
    VER="deployment.version"
    echo "$YMDHMS" > $VER
  popd
fi

TARBALL="$YMD-occam.tar"

if [ "$1" = "bundle" ]; then
  #######
  header "Tarballing & Gzipping"
  #######
  pushd $DSTOPT
    if [ -f $TARBALL ]; then
      rm $TARBALL
    fi

    tar cvf $TARBALL occam

		if [ -f $TARBALL.gz ]; then
      rm $TARBALL.gz
    fi 

    gzip $TARBALL

  popd
fi


# Upload Variables
SCPHOST=unhosting.org
SCPPATH=/data/www/org/transterpreter/www/site/downloads/lin/
DOWNLOADURL="http://www.transterpreter.org/downloads/lin"

if [ "$1" = "upload" ]; then
  ####### 
  header "Uploading compressed bundle"
  #######
  if [ "$2" = "" ]; then
    echo
    echo "Usage:"
    echo " ./make... upload <username>"
    echo
    exit
  fi

  pushd $DSTOPT

    if [ -f $TARBALL.gz ]; then
      scp $TARBALL.gz $2@$SCPHOST:$SCPPATH

 			echo  
      echo "FILE DOWNLOAD FROM:"
      echo
      echo "$DOWNLOADURL/$TARBALL.gz"
    fi
fi  

if [ "$1" = "all" ]; then
  #######
  header "Doing everything..."
  #######
  $0 build
  $0 copy
  $0 writeconfig
  $0 bundle

  if [ ! "$2" = "" ]; then
    $0 upload $2
  fi 
fi  

if [ "$1" = "" ]; then
  header "Build options"
  echo " [ build       ]  Checkout source and build."
  echo " [ copy        ]  Copy Arduino scripts into binary path."
  echo " [ writeconfig ]  Generate Arduino support config."
  echo " [ bundle      ]  Generate tarball."
  echo " [ all         ]  From source checkout to tarballing. "
  echo " [ all <uname> ]  Upload after completion."
  echo
  exit
fi

exit

