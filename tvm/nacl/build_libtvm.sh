#!/bin/sh

export CC="$NACL_SDK_ROOT/toolchain/mac_x86/bin/nacl-gcc"
export AR="$NACL_SDK_ROOT/toolchain/mac_x86/bin/nacl-ar"
export RANLIB="$NACL_SDK_ROOT/toolchain/mac_x86/bin/nacl-ranlib"
export LIBS="-lppruntime -limc -lplatform -lpthread -lgio -lsrpc -lstdc++ -lm -u PPP_GetInterface -u PPP_ShutdownModule -u PPP_InitializeModule"

LIBTVM_SRC=../../runtime/libtvm

mkdir libtvm_x86_32
pushd libtvm_x86_32
make distclean
CFLAGS=-m32 ../$LIBTVM_SRC/configure --host=i686-unknown-linux && make
popd

mkdir libtvm_x86_64
pushd libtvm_x86_64
make distclean
CFLAGS=-m64 ../$LIBTVM_SRC/configure --host=x86_64-unknown-linux && make
popd

