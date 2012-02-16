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

#include "tvm_compiler.h"
#include "tvm_types.h"

/*{{{  Word length related constants */
#if TVM_WORD_LENGTH == 4
#	define MIN_INT		0x80000000
#	define MAX_INT		(MIN_INT - 1)
#	define LONG_HI_MASK	0xFFFFFFFF00000000
#	define LONG_LO_MASK	0x00000000FFFFFFFF
#	define WORD_BITS	32
#	define TWOWORD_BITS	64
#	define WSH		2
#elif TVM_WORD_LENGTH == 2
#	define MIN_INT		0x8000
#	define MAX_INT		(MIN_INT - 1)
#	define LONG_HI_MASK	0xFFFF0000
#	define LONG_LO_MASK	0x0000FFFF
#	define WORD_BITS	16
#	define TWOWORD_BITS	32
#	define WSH		1
#else
#	error Wordlength not supported
#endif
/*}}}*/

/*{{{  Workspace constants and manipulation */
#define WS_TEMP		0 /* Temporary slot used by some instructions */
#define WS_IPTR		1 /* Instruction pointer when descheduled */
#define WS_LINK		2 /* Next process when on a scheduling list */
#define WS_PENDING	2 /* Pending bytes to send on a channel (not on Transputer) */
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

/*{{{  Shadow types */
enum {
	STYPE_UNDEF	= 0,	/* Undefined/unknown type */
	STYPE_DATA	= 1,	/* Generic data */
	STYPE_WS	= 2,	/* Workspace Pointer */
	STYPE_BC	= 3,	/* Bytecode Pointer */
	STYPE_MT	= 4,	/* Mobile Type Pointer */
	STYPE_CHAN	= 5,	/* Channel */
	STYPE_MOBILE	= 6,	/* Pointer into Mobile Memory */
	STYPE_NULL	= 7,	/* NULL Pointer */
	STYPE_VS	= 8,	/* Vectorspace Pointer */
	STYPE_RET	= 9	/* Return Address (in call-stack) */
};
/*}}}*/

/*{{{  Error flags */
enum {
	EFLAG_SETERR 	= (1 << 0),	/* Error flag set by SETERR */
	EFLAG_INTERR	= (1 << 1),	/* Integer (range) error */
	EFLAG_INTOV	= (1 << 2),	/* Integer overflow */
	EFLAG_FP	= (1 << 3),	/* Floating point error */
	EFLAG_ALT	= (1 << 4),	/* ALT algorithm error */
	EFLAG_FFI	= (1 << 5),	/* FFI error (missing function) */
	EFLAG_CHAN	= (1 << 6),	/* Channel communication error */
	EFLAG_EXTCHAN	= (1 << 7), 	/* External channel error */
	EFLAG_BAR	= (1 << 8),	/* Barrier underflow */
	EFLAG_DMEM	= (1 << 9),	/* Memory allocator error */
	EFLAG_MT	= (1 << 10),	/* Mobile type error */
	EFLAG_PROC	= (1 << 11),	/* Process API error */
};
/*}}}*/

/*{{{  Synchronisation flags */
enum {
	SFLAG_TQ	= (1 << 0),	/* Timer queue ready */
	SFLAG_TQ_P	= 0,
	SFLAG_INTR	= (1 << 1),	/* Interrupt execution */
	SFLAG_INTR_P	= 1,
	SFLAG_USER_P	= 2		/* User flags from this position */
};
/*}}}*/

/*{{{  Execution context pre-definitions */
typedef struct _tvm_ectx_t	tvm_ectx_t;
typedef tvm_ectx_t		*ECTX;
#ifndef TVM_ECTX_PRIVATE_DATA
#define TVM_ECTX_PRIVATE_DATA	WORD
typedef TVM_ECTX_PRIVATE_DATA	tvm_ectx_priv_t;
#endif /* !TVM_ECTX_PRIVATE_DATA */

#ifndef TVM_H
typedef void tvm_t;
#endif
/*}}}*/

/*{{{  External channel function prototypes. */
typedef int (*EXT_CHAN_FUNCTION)(ECTX ectx, WORD count, BYTEPTR address);
typedef int (*EXT_CHAN_MT_FUNCTION)(ECTX ectx, WORDPTR address);

typedef struct {
	UWORD			typehash;
	EXT_CHAN_FUNCTION	in;
	EXT_CHAN_FUNCTION	out;
	EXT_CHAN_MT_FUNCTION	mt_in;
	EXT_CHAN_MT_FUNCTION	mt_out;
} EXT_CHAN_ENTRY;

typedef void (*EXT_CB_FREE_FUNCTION)(ECTX ectx, void *data);
typedef int (*EXT_CB_IO_FUNCTION)(ECTX ectx, void *data, WORDPTR channel, BYTEPTR address, WORD count);
typedef int (*EXT_CB_MT_FUNCTION)(ECTX ectx, void *data, WORDPTR channel, WORDPTR address);
typedef int (*EXT_CB_XABLE_FUNCTION)(ECTX ectx, void *data, WORDPTR channel);
typedef struct {
	EXT_CB_IO_FUNCTION	in;
	EXT_CB_IO_FUNCTION	out;
	EXT_CB_IO_FUNCTION	swap;
	EXT_CB_MT_FUNCTION	mt_in;
	EXT_CB_MT_FUNCTION	mt_out;
	EXT_CB_XABLE_FUNCTION	xable;
	EXT_CB_IO_FUNCTION	xin;
	EXT_CB_MT_FUNCTION	mt_xin;
	EXT_CB_FREE_FUNCTION	free;
} EXT_CB_INTERFACE;

typedef struct {
	EXT_CB_INTERFACE	*interface;
	void			*data;
} mt_cb_ext_t;
/*}}}*/

/*{{{   Foreign function interface prototypes. */
typedef void (*FFI_FUNCTION)(WORD w[]);

typedef struct {
	FFI_FUNCTION func;
	char *name;
} FFI_TABLE_ENTRY;

typedef int (*SFFI_FUNCTION)(ECTX ectx, WORD w[]);
/*}}}*/

/*{{{  Transputer registers, etc that make up the execution context */
#define TVM_ECTX_TLP_ARGS 8

struct _tvm_ectx_t {
	/* Machine state */
	BYTEPTR		iptr;	/* Instruction pointer */
	
	WORD		oreg;	/* Operand register */
	WORD		areg;	/* Evaluation stack */
	WORD		breg;	/* Evaluation stack */
	WORD		creg;	/* Evaluation stack */
	WORD 		_creg;	/* Special case CREG storage */
	#ifdef TVM_TYPE_SHADOW
	WORD		aregT;
	WORD		bregT;
	WORD		cregT;
	WORD		_cregT;
	#endif /* TVM_TYPE_SHADOW */

	WORD		pri;	/* Priority */

	WORDPTR		wptr;	/* Workspace pointer */
	WORDPTR		fptr;	/* Front pointer (scheduler queue) */
	WORDPTR		bptr;	/* Back pointer (scheduler queue) */
	WORDPTR		tptr;	/* Timer queue pointer */
	WORD		tnext;	/* Timeout register */
	
	WORD 		eflags;	/* Error flags */
	WORD 		state;	/* Context state */

	volatile WORD 	sflags;	/* Synchronisation flags */

	/* Type store */
	#ifdef TVM_TYPE_SHADOW
	unsigned int	shadow_start;
	unsigned int	shadow_end;
	BYTE		*type_store;
	#endif /* TVM_TYPE_SHADOW */

	/* Implementation functions */
	void		(*add_to_queue)
				(ECTX ectx, WORDPTR ws);
	int		(*add_to_queue_external)
				(ECTX dst_ctx, ECTX src_ctx, WORDPTR ws);
	void		(*add_queue_to_queue)
				(ECTX ectx, WORDPTR front, WORDPTR back);
	void		(*modify_sync_flags)
				(ECTX ectx, WORD set, WORD clear);
	int		(*run_next_on_queue)
				(ECTX ectx);
	void		(*set_alarm)
				(ECTX ectx);
	int		(*set_error_flag)
				(ECTX ectx, WORD flag);
	int		(*synchronise)
				(ECTX ectx);
	void		(*timer_queue_insert)
				(ECTX ectx, WORDPTR ws, WORD current_time, WORD reschedule_time);
	void		(*timer_queue_remove)
				(ECTX ectx, WORDPTR ws);
	void		(*walk_timer_queue)
				(ECTX ectx, WORD now);
	
	/* Memory allocation related */
	#ifdef TVM_USE_TLSF
	void		*mem_pool;
	#endif

	/* Wrapper defined functions */
	#ifdef TVM_CUSTOM_MEM_OPS
	BYTEPTR		*(*memcpy)
				(BYTEPTR dst, BYTEPTR src, UWORD n);
	BYTEPTR		*(*memset)
				(BYTEPTR s, WORD c, UWORD n);
	#endif
	WORD		(*get_time)
				(ECTX ectx);
	int		(*run_hook)
				(ECTX ectx);

	/* Top-level process */
	char		tlp_fmt[TVM_ECTX_TLP_ARGS];
	int		tlp_argc;
	WORD		tlp_argv[TVM_ECTX_TLP_ARGS];

	/* FFI */
	FFI_TABLE_ENTRY	*ffi_table;
	UWORD		ffi_table_length;
	SFFI_FUNCTION	*sffi_table;
	UWORD		sffi_table_length;

	/* External channels */
	#ifdef TVM_OCCAM_PI
	EXT_CHAN_ENTRY	*ext_chan_table;
	UWORD		ext_chan_table_length;
	#endif

	/* Links to other execution contexts */
	tvm_t		*tvm;
	tvm_ectx_t	*next;

	/* Profiling data */
	#ifdef TVM_PROFILING
	struct {
		UWORD	pri[16];
		UWORD	sec[608];
	} profile;
	#endif

	/* Private data */
	tvm_ectx_priv_t	priv;
}
#if defined (TVM_PACKED_ECTX)
TVM_PACK
#endif
;
/*}}}*/

/*{{{  Execution context returns */
/* Negative return values are reserved for internal use,
 * and should never be propagated outside the interpreter.
 */
enum {
	ECTX_CONTINUE		= 0,
	ECTX_INIT		= 'i',
	ECTX_INS_INVALID	= 'I',
	ECTX_INS_UNSUPPORTED	= 'U',
	ECTX_INTERRUPT		= 'x',
	ECTX_EMPTY		= 'e',
	ECTX_ERROR		= 'E',
	ECTX_PREEMPT		= 'p',
	ECTX_RUNNING		= 'r',
	ECTX_SHUTDOWN		= 'S',
	ECTX_SLEEP		= 's',
	ECTX_TIME_SLICE		= 't'
};
enum {
	_ECTX_DESCHEDULE	= -1,
	_ECTX_BYPASS		= -2
};
/*}}}*/

/*{{{  Special FFI returns */
enum {
	/* only SFFI_OK should overlap with ECTX */
	SFFI_OK			= ECTX_CONTINUE,
	SFFI_BYPASS		= 'b',
	SFFI_RESCHEDULE		= 'r'
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

#define AREGt	(ectx->aregT)
#define BREGt	(ectx->bregT)
#define CREGt	(ectx->cregT)
#ifdef TVM_TYPE_SHADOW
#define SET_AREGt(X)	do { AREGt = (X); } while (0)
#define SET_BREGt(X)	do { BREGt = (X); } while (0)
#define SET_CREGt(X)	do { CREGt = (X); } while (0)
#else /* !TVM_TYPE_SHADOW */
#define SET_AREGt(X)	do { } while (0)
#define SET_BREGt(X)	do { } while (0)
#define SET_CREGt(X)	do { } while (0)
#endif /* !TVM_TYPE_SHADOW */


#define FPTR	(ectx->fptr)
#define BPTR	(ectx->bptr)
#define TPTR	(ectx->tptr)
#define TNEXT	(ectx->tnext)

#define PPRI	(ectx->pri)

#endif /* TVM_INTERNAL_MACROS */
/*}}}*/

#endif /* TRANSPUTER_H */
