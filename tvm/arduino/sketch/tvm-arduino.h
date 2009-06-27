#ifndef TVM_ARDUINO_H
#define TVM_ARDUINO_H

// Define for lots of useful printed-out stuff.
// #define DEBUG

#include <WProgram.h>

// The Wiring stuff defines BYTE as 0, but the Transterpreter headers use it as
// a type name.
#undef BYTE

extern "C" {
	#include <tvm.h>
}

//{{{  debug.cpp
extern void hexprint(int n);
//}}}
//{{{  ffi.cpp
extern "C" {
	extern SFFI_FUNCTION sffi_table[];
	extern const int sffi_table_length;
}
//}}}
//{{{  tvm.cpp
extern int main (void);
//}}}

#endif
