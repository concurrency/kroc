
#ifndef TBZUTIL_H
#define TBZUTIL_H

enum bytecode_type {
	TBC = 1,
	TBZ
};

extern unsigned char *infile_start;   /* Always points to the very start of the loaded file */

extern int bytecode_start;
extern int bytecode_length;
extern int bytecode_flags;
extern int ffi_start;
extern int ffi_length;
extern int ffi_flags;
extern int debug_start;
extern int debug_length;
extern int debug_flags;


int parse_tbz_v0(char* filename);



/* Things that are defined in stiwmain.c. We can't define these in stiw.h, as
 * its only used when the wrapper is compiled for compiling bytecode directly
 * into the wrapper. (Though I dont think we do this at all anymore? Basically
 * this wrapper needs to be scrapped and replaced with the POSIX wrapper...
 * Something for a rainy day. */
int parse_bytecode_v2(char *filename);
extern int instsize;
extern unsigned char *transputercode;
extern unsigned char *transputercode_ptr;


#endif /* TBZUTIL_H */
