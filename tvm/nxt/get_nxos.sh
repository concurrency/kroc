#!/bin/bash

if [ ! -e nxos ]; then
	git clone http://github.com/perlfu/nxos.git nxos
fi

if [ -e nxos ]; then
	pushd nxos/nxos
	scons
	popd
fi

if [ -e nxos/nxos/libnxos.a ]; then
	cp nxos/nxos/libnxos.a libnxos.a
fi

