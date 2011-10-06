#!/bin/sh

export CC="$NACL_SDK_ROOT/toolchain/mac_x86/bin/nacl-gcc"
export LIBS="-lppruntime -limc -lplatform -lpthread -lgio -lsrpc -lstdc++ -lm -u PPP_GetInterface -u PPP_ShutdownModule -u PPP_InitializeModule"

make distclean
./configure --host=i686-unknown-linux && make && cp libtvm.a libtvm_x86_32.a
make distclean
CFLAGS=-m64 ./configure --host=x86_64-unknown-linux && make && cp libtvm.a libtvm_x86_64.a

