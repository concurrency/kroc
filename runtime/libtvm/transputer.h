/*
tvm - transputer.h
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

#ifndef TRANSPUTER_H
#define TRANSPUTER_H

#if defined(HAVE_TVM_TVM_TYPES_H)
#include <tvm/tvm_types.h>
#elif defined(HAVE_KROC_TVM_TYPES_H)
#include <kroc/tvm_types.h>
#else
#include <tvm_types.h>
#endif

#define NUM_PRI 1

#if TVM_WORD_LENGTH == 4
#	define MIN_INT		0x80000000
#	define MAX_INT		(MIN_INT - 1)
#	define LONG_HI_MASK	0xFFFFFFFF00000000
#	define LONG_LO_MASK	0x00000000FFFFFFFF
#	define WORD_BITS	32
#	define WSH		2
#elif TVM_WORD_LENGTH == 2
#	define MIN_INT		0x8000
#	define MAX_INT		(MIN_INT - 1)
#	define LONG_HI_MASK	0xFFFF0000
#	define LONG_LO_MASK	0x0000FFFF
#	define WORD_BITS	16
#	define WSH		1
#elif TVM_WORD_LENGTH == 1
#	define MIN_INT		0x80
#	define MAX_INT		(MIN_INT - 1)
#	define LONG_HI_MASK	0xFF00
#	define LONG_LO_MASK	0x00FF
#	define WORD_BITS	8
#	define WSH		0
#else
#	error Wordlength not supported
#endif
#define NOT_PROCESS_P		0
#define NULL_P			NOT_PROCESS_P
#define ENABLING_P		(MIN_INT + 1)
#define WAITING_P		(MIN_INT + 2)
#define DISABLING_P		(MIN_INT + 3)
#define READY_P			(MIN_INT + 3)
#define TIME_SET_P		(MIN_INT + 1)
#define TIME_NOT_SET_P		(MIN_INT + 2)
#define NONE_SELECTED_O		(-1)

/* Transputer registers and flags */
extern BYTEPTR iptr;		/* Instruction pointer */
extern WORDPTR wptr;		/* Workspace pointer */

extern WORD areg;		/* Evaluation stack */
extern WORD breg;		/* Evaluation stack */
extern WORD creg;		/* Evaluation stack */
extern WORD oreg;		/* Operand register */

extern WORDPTR tptr[NUM_PRI];	/* Timer queue pointers */
extern WORD tnext[NUM_PRI];	/* Timeout register */
extern WORDPTR fptr[NUM_PRI];	/* Front pointer (scheduler queue) */
extern WORDPTR bptr[NUM_PRI];	/* Back pointer (scheduler queue) */

extern WORD error_flag;
extern WORD halt_on_error_flag;

/* State not in the original Transputer */
// extern WORD pri;		/* Current priority level */
#define pri 0

#endif /* TRANSPUTER_H */
