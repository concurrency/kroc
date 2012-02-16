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

#include "tvm_config.h"

#ifndef HAVE_INTTYPES_H

/* No inttypes.h; assume the sizes of the standard types are correct. */
typedef unsigned char	BYTE;
typedef signed char	SBYTE;
typedef signed short	INT16;
typedef signed short	HWORD;
typedef signed int	WORD;
typedef unsigned int	UWORD;
#ifdef TVM_HAVE_TWOWORD
typedef signed long long int TWOWORD;
typedef unsigned long long int UTWOWORD;
#endif

#else /* HAVE_INTTYPES_H */

#include <inttypes.h>

typedef uint8_t		BYTE;
typedef int8_t		SBYTE;
typedef int16_t		INT16;

#if TVM_WORD_LENGTH == 4
typedef int16_t		HWORD;
typedef int32_t		WORD;
typedef uint32_t	UWORD;
#ifdef TVM_HAVE_TWOWORD
typedef int64_t		TWOWORD;
typedef uint64_t	UTWOWORD;
#endif
#elif TVM_WORD_LENGTH == 2
typedef int8_t		HWORD;
typedef int16_t		WORD;
typedef uint16_t	UWORD;
#ifdef TVM_HAVE_TWOWORD
typedef int32_t		TWOWORD;
typedef uint32_t	UTWOWORD;
#endif
#else
#error "Word length not supported (1)"
#endif

#endif /* HAVE_INTTYPES_H */

/* Define kinds of pointers which we want to be able to use */
#if defined(WORDPTRS_VIRTUAL)
typedef WORD 		BYTEPTR;
typedef WORD		INT16PTR;
typedef WORD 		WORDPTR;
#else
typedef BYTE * 		BYTEPTR;
typedef INT16 *		INT16PTR;
typedef WORD * 		WORDPTR;
#endif

#endif /* !TVM_TYPES_H */
