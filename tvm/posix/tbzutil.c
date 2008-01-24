
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>

#include "tbzutil.h"

/* The byteswapping macro is in here */
#include "interpreter/mem.h"

/*
 * The various header offsets
 */
/* The offsets for the descriptors of the various files */
#define TBZ_V0_BYTECODE_DESCRIPTOR_OFFSET   (3 * 4)
#define TBZ_V0_FFI_DESCRIPTOR_OFFSET        (6 * 4)
#define TBZ_V0_DEBUG_DESCRIPTOR_OFFSET      (9 * 4)
/* Offsets of the components of the descriptors */
#define TBZ_V0_DESCRIPTOR_START_OFFSET      (0 * 4)
#define TBZ_V0_DESCRIPTOR_LENGTH_OFFSET     (1 * 4)
#define TBZ_V0_DESCRIPTOR_FLAGS_OFFSET      (2 * 4)
/* Where the header ends and data begins */
#define TBZ_V0_HEADER_OFFSET               (12 * 4)

unsigned char *infile_start;   /* Always points to the very start of the loaded file */
int bytecode_start;
int bytecode_length;
int bytecode_flags;
int ffi_start;
int ffi_length;
int ffi_flags;
int debug_start;
int debug_length;
int debug_flags;

int parse_tbz_v0(char* filename)
{
  /* Bytecode bits */
  bytecode_start  = *(int *)(infile_start + TBZ_V0_BYTECODE_DESCRIPTOR_OFFSET + TBZ_V0_DESCRIPTOR_START_OFFSET);
  bytecode_length = *(int *)(infile_start + TBZ_V0_BYTECODE_DESCRIPTOR_OFFSET + TBZ_V0_DESCRIPTOR_LENGTH_OFFSET);
  bytecode_flags  = *(int *)(infile_start + TBZ_V0_BYTECODE_DESCRIPTOR_OFFSET + TBZ_V0_DESCRIPTOR_FLAGS_OFFSET);
  /* FFI bits */
  ffi_start  = *(int *)(infile_start + TBZ_V0_FFI_DESCRIPTOR_OFFSET + TBZ_V0_DESCRIPTOR_START_OFFSET);
  ffi_length = *(int *)(infile_start + TBZ_V0_FFI_DESCRIPTOR_OFFSET + TBZ_V0_DESCRIPTOR_LENGTH_OFFSET);
  ffi_flags  = *(int *)(infile_start + TBZ_V0_FFI_DESCRIPTOR_OFFSET + TBZ_V0_DESCRIPTOR_FLAGS_OFFSET);
  /* FFI bits */
  debug_start  = *(int *)(infile_start + TBZ_V0_DEBUG_DESCRIPTOR_OFFSET + TBZ_V0_DESCRIPTOR_START_OFFSET);
  debug_length = *(int *)(infile_start + TBZ_V0_DEBUG_DESCRIPTOR_OFFSET + TBZ_V0_DESCRIPTOR_LENGTH_OFFSET);
  debug_flags  = *(int *)(infile_start + TBZ_V0_DEBUG_DESCRIPTOR_OFFSET + TBZ_V0_DESCRIPTOR_FLAGS_OFFSET);

#ifndef HOST_BIGENDIAN
  bytecode_start = SwapFourBytes(bytecode_start);
  bytecode_length = SwapFourBytes(bytecode_length);
  bytecode_flags = SwapFourBytes(bytecode_flags);
  ffi_start = SwapFourBytes(ffi_start);
  ffi_length = SwapFourBytes(ffi_length);
  ffi_flags = SwapFourBytes(ffi_flags);
  debug_start = SwapFourBytes(debug_start);
  debug_length = SwapFourBytes(debug_length);
  debug_flags = SwapFourBytes(debug_flags);
#endif

  transputercode = transputercode_ptr = infile_start + bytecode_start;
  /* This was set by main when reading in the file (and is the size of the
   * entire unified bytecode file, we can just reset it to the size of the
   * bytecode */
  instsize = bytecode_length;
  /* Now we call parse_bytecode_v2 to figure out whats in the bytecode,
   * parse_bytecode_v2 will make a call to load ffi stuff too... First lets
   * check we have something sensible though */
	if(strncmp("tvm", (char *)transputercode, 3) != 0)
  {
    printf("No valid bytecode found in %s\n", filename);
    return 1;
  }
 	if(transputercode[3] != 2)
  {
    printf("Invalid bytecode version found in %s\n", filename);
    return 1;
  }
 
  return parse_bytecode_v2(filename);
}
