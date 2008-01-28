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

#include "tvm_types.h"

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

typedef struct _tvm_ectx_t tvm_ectx_t;
/* Transputer registers and flags, make up execution context */
struct _tvm_ectx_t {
	BYTEPTR iptr;		/* Instruction pointer */
	WORDPTR wptr;		/* Workspace pointer */
	
	WORD areg;		/* Evaluation stack */
	WORD breg;		/* Evaluation stack */
	WORD creg;		/* Evaluation stack */
	WORD oreg;		/* Operand register */

	WORDPTR fptr;		/* Front pointer (scheduler queue) */
	WORDPTR bptr;		/* Back pointer (scheduler queue) */
	WORDPTR tptr;		/* Timer queue pointer */
	WORD tnext;		/* Timeout register */

	WORD saved_creg;	/* Special case CREG storage */
	WORD error_flag;
	WORD halt_on_error_flag;

	WORD pri;

	int (*set_error_flag)(tvm_ectx_t *ctx, WORD flag);
	void (*add_to_queue)(tvm_ectx_t *ctx, WORDPTR ws);
	void (*add_queue_to_queue)(tvm_ectx_t *ctx, WORDPTR front, WORDPTR back);
	void (*timer_queue_insert)(tvm_ectx_t *ctx, WORD current_time, WORD reschedule_time);
	int (*run_next_on_queue)(tvm_ectx_t *ctx);

	WORD (*get_time)(tvm_ectx_t *ctx);
	void (*set_alarm)(tvm_ectx_t *ctx, WORD alarm);
};

#ifdef TVM_INTERNALS

#define IPTR	(ectx->iptr)
#define WPTR	(ectx->wptr)

#define AREG	(ectx->areg)
#define BREG	(ectx->breg)
#define CREG	(ectx->creg)
#define OREG	(ectx->oreg)

#define FPTR	(ectx->fptr)
#define BPTR	(ectx->bptr)
#define TPTR	(ectx->tptr)
#define TNEXT	(ectx->tnext)

#define PPRI	(ectx->pri)

#endif /* TVM_INTERNAL_MACROS */

#endif /* TRANSPUTER_H */
