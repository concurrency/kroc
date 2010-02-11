#!/bin/sh

if [ ! -e nxos ]; then
	git clone http://github.com/perlfu/nxos.git nxos
fi

if [ -e nxos ]; then
	pushd nxos/nxos
	scons
	popd
fi
