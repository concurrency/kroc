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

/*{{{  Word length related constants */
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
/*}}}*/

/*{{{  Workspace constants and manipulation */
#define WS_TEMP		0 /* Temporary slot used by some instructions */
#define WS_IPTR		1 /* Instruction pointer when descheduled */
#define WS_LINK		2 /* Next process when on a scheduling list */
#define WS_ECTX		3 /* Execution context pointer when blocked on event */
#define WS_POINTER	4 /* Data pointer when blocked on channel */
#define WS_STATE	4 /* Saved alternative state when ALTing */
#define WS_LENGTH	4 /* Length of received message for variable communication */
#define WS_TLINK	5 /* Next process when on timer queue */
#define WS_TIME		6 /* Time waited for when blocked on timer or TALTing */

#define WORKSPACE_GET(WPTR, LOC) \
	read_word(wordptr_minus((WPTR), (LOC)))
#define WORKSPACE_SET(WPTR, LOC, VAL) \
	write_word(wordptr_minus((WPTR), (LOC)), (VAL))
/*}}}*/

/*{{{  Other transputer constants */
#define NOT_PROCESS_P		0
#define NULL_P			NOT_PROCESS_P
#define ENABLING_P		(MIN_INT + 1)
#define WAITING_P		(MIN_INT + 2)
#define DISABLING_P		(MIN_INT + 3)
#define READY_P			(MIN_INT + 3)
#define TIME_SET_P		(MIN_INT + 1)
#define TIME_NOT_SET_P		(MIN_INT + 2)
#define NONE_SELECTED_O		(-1)
/*}}}*/

/*{{{  Transputer registers and flags, make up execution context */

typedef struct _tvm_ectx_t	tvm_ectx_t;
typedef tvm_ectx_t		*ECTX;
#ifndef TVM_ECTX_PRIVATE_DATA
#define TVM_ECTX_PRIVATE_DATA	WORD
typedef TVM_ECTX_PRIVATE_DATA	tvm_ectx_priv_t;
#endif /* !TVM_ECTX_PRIVATE_DATA */

struct _tvm_ectx_t {
	BYTEPTR		iptr;	/* Instruction pointer */
	
	WORD		oreg;	/* Operand register */
	WORD		areg;	/* Evaluation stack */
	WORD		breg;	/* Evaluation stack */
	WORD		creg;	/* Evaluation stack */

	WORD		pri;	/* Priority */

	WORDPTR		wptr;	/* Workspace pointer */
	WORDPTR		fptr;	/* Front pointer (scheduler queue) */
	WORDPTR		bptr;	/* Back pointer (scheduler queue) */
	WORDPTR		tptr;	/* Timer queue pointer */
	WORD		tnext;	/* Timeout register */

	WORD 		_creg;	/* Special case CREG storage */
	
	WORD 		eflags;	/* Error flags */
	WORD 		state;	/* Context state */

	volatile WORD 	sflags;	/* Synchronisation flags */

	/* Implementation functions */
	void		(*add_to_queue)
				(ECTX ectx, WORDPTR ws);
	int		(*add_to_queue_external)
				(ECTX dst_ctx, ECTX src_ctx, WORDPTR ws);
	void		(*add_queue_to_queue)
				(ECTX ectx, WORDPTR front, WORDPTR back);
	int		(*run_next_on_queue)
				(ECTX ectx);
	int		(*set_error_flag)
				(ECTX ectx, WORD flag);
	void		(*timer_queue_insert)
				(ECTX ectx, WORDPTR ws, WORD current_time, WORD reschedule_time);

	/* Wrapper defined functions */
	WORD		(*get_time)
				(ECTX ectx);
	void		(*set_alarm)
				(ECTX ectx, WORD alarm);
	int		(*run_hook)
				(ECTX ectx);

	/* Private data */
	tvm_ectx_priv_t	priv;
};

/*}}}*/

/*{{{  Internal macros */
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
/*}}}*/

#endif /* TRANSPUTER_H */
