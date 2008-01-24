/*
tvm - types.h
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
#ifndef TYPES_H
#define TYPES_H

/* Use stddef to get NULL. */
#include <stddef.h>

#ifndef HAVE_INTTYPES_H

/* No inttypes.h; assume the sizes of the standard types are correct. */
typedef unsigned char BYTE;
typedef short HWORD;
typedef int WORD;
typedef unsigned int UWORD;

#else /* HAVE_INTTYPES_H */

#include <inttypes.h>

#if WORDLENGTH == 4
typedef uint8_t BYTE;
typedef int16_t HWORD;
typedef int32_t WORD;
typedef uint32_t UWORD;
#elif WORDLENGTH == 2
typedef uint8_t BYTE;
typedef int8_t HWORD;
typedef int16_t WORD;
typedef uint16_t UWORD;
#elif WORDLENGTH == 1
typedef uint8_t BYTE;
typedef uint8_t HWORD;
typedef int8_t WORD;
typedef uint8_t UWORD;
#else
#error "Word length not supported (1)"
#endif

#endif /* HAVE_INTTYPES_H */

/* Define a shift to convert between words and bytes. */
#if WORDLENGTH == 8
#define WSH 3
#elif WORDLENGTH == 4
#define WSH 2
#elif WORDLENGTH == 2
#define WSH 1
#elif WORDLENGTH == 1
#define WSH 0
#else
#error "Word length not supported (2)"
#endif

/* Define kinds of pointers which we want to be able to use */
#ifdef POOTERS_REAL
typedef WORD * POOTER;
typedef BYTE * BPOOTER;
#elif defined POOTERS_FAKE
typedef WORD POOTER;
typedef WORD BPOOTER;
#else
#error "Invalid pointer typedef selector used"
#endif

/* This is the prototype for occam FFI calls, it is defined, by D.C.Wood as
 * void name (word w[]). */
typedef void (*FFI_FUNCTION)(WORD w[]);

typedef struct {
	FFI_FUNCTION func;
	char *name;
} FFI_TABLE_ENTRY;

#endif /* !TYPES_H */
