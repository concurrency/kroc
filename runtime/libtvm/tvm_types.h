/*
tvm - tvm_types.h
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2004-2008 Christian L. Jacobsen, Matthew C. Jadud

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef TVM_TYPES_H
#define TVM_TYPES_H

#if defined(HAVE_TVM_TVM_CONFIG_H)
#include <tvm/tvm-config.h>
#elif defined(HAVE_KROC_TVM_CONFIG_H)
#include <kroc/tvm-config.h>
#else
#include <tvm-config.h>
#endif

/* Use stddef to get NULL. */
#include <stddef.h>

#ifndef HAVE_INTTYPES_H

/* No inttypes.h; assume the sizes of the standard types are correct. */
typedef unsigned char	BYTE;
typedef short		HWORD;
typedef int		WORD;
typedef unsigned int	UWORD;

#else /* HAVE_INTTYPES_H */

#include <inttypes.h>

#if TVM_WORD_LENGTH == 4
typedef uint8_t		BYTE;
typedef int16_t		HWORD;
typedef int32_t		WORD;
typedef uint32_t	UWORD;
#elif TVM_WORD_LENGTH == 2
typedef uint8_t		BYTE;
typedef int8_t		HWORD;
typedef int16_t		WORD;
typedef uint16_t	UWORD;
#elif TVM_WORD_LENGTH == 1
typedef uint8_t		BYTE;
typedef uint8_t		HWORD;
typedef int8_t		WORD;
typedef uint8_t		UWORD;
#else
#error "Word length not supported (1)"
#endif

#endif /* HAVE_INTTYPES_H */

/* Define kinds of pointers which we want to be able to use */
#if defined(WORDPTRS_FAKE)
typedef WORD 		WORDPTR;
typedef WORD 		BYTEPTR;
#else
typedef WORD * 		WORDPTR;
typedef BYTE * 		BYTEPTR;
#endif

/* This is the prototype for occam FFI calls, it is defined, by D.C.Wood as
 * void name (word w[]). */
typedef void (*FFI_FUNCTION)(WORD w[]);

typedef struct {
	FFI_FUNCTION func;
	char *name;
} FFI_TABLE_ENTRY;

#endif /* !TVM_TYPES_H */
