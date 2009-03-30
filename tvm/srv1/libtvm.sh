#!/bin/sh

[ -n "$CC" ] && X_CC=$CC || X_CC=bfin-elf-gcc
X_CFLAGS="-Os -g -D__ADSPBF537__ -D__ADSPLPBLACKFIN__ $CFLAGS"

CONF_OPTS="--host=bfin-elf --enable-occam-pi --enable-t9000-short-ops"
BUILD="libtvm"
EPATH="$PWD"

if [ -n "$1" ]; then
	SRC="$1"
else
	SRC="$EPATH/../../runtime/libtvm"
fi

[ ! -e "$BUILD" ] && mkdir "$BUILD"

if [ -d "$SRC" ]; then
	cd "$SRC" && echo "cd $SRC"
	[ ! -x configure ] && autoreconf -f -i
	if [ -x configure ]; then
		[ -e Makefile ] && make distclean
		cd "$EPATH" && echo "cd $EPATH"
		if [ -d "$BUILD" ]; then
			cd "$BUILD" && echo "cd $BUILD"
			CFLAGS="$X_CFLAGS" CC="$X_CC" "$SRC/configure" $CONF_OPTS
			[ -e Makefile ] && make 
			[ -e Makefile ] && make
		else
			echo "Error: build directory $BUILD doesn't exist."
		fi
	else
		echo "Error: configure script doesn't exist in source directory ($SRC)."
	fi
	cd "$EPATH" && echo "cd $EPATH"
else
	echo "Error: source directory $SRC doesn't exist."
fi

