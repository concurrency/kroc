#!/bin/sh

if [ ! -e nxos ]; then
	git clone http://github.com/perlfu/nxos.git nxos
fi

if [ -e nxos ]; then
	scons -C nxos/nxos
fi

