/*
tvm - tvm.h
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

#ifndef TVM_H
#define TVM_H

#include "tvm_config.h"
#include "tvm_types.h"
#include "transputer.h"
#include "tvm_mem.h"

/*{{{  The foregin function interface jump tables */
extern FFI_TABLE_ENTRY *ffi_table;
extern FFI_FUNCTION *special_ffi_table;
/*}}}*/

/*{{{  Interpreter exit codes */
enum {
	EXIT_STACK_BOTTOM	= 0,	/* For what currently amounts to a good exit */
	EXIT_DEADLOCK		= 100,	/* When the running program has deadlocked */
	EXIT_HALTED		= 200,	/* When the program halted due to an error */
	EXIT_DEBUG_TRAP		= 300,	/* When the program halted due to an J 0 */
	EXIT_ERROR		= 400,	/* When a runtime error occurs */
	EXIT_ALIGN_ERROR	= 666,	/* A memory alignment error */
	EXIT_BAD_ADDR		= 667,	/* A bad memory address */
	EXIT_INS_INVALID	= 998,	/* If we hit an invalid instruction */
	EXIT_INS_NOT_IMP	= 999,	/* If we hit an unimplemented instruction */
	EXIT_SCHEDULER_BAD_1	= 5000	/* If the scheduler has gone bad */
};
/*}}}*/

/*{{{  Stack frame return types */
enum {
	RET_ERROR		= 0,	/* Set error flag on process return */
	RET_REAL,			/* Go to address on process return */
	RET_SHUTDOWN			/* Shutdown interpreter on return */
};
/*}}}*/

/*{{{  Error values */
enum {
	EFLAG_SETERR = 1,
	EFLAG_CSUB0,
	EFLAG_LADD,
	EFLAG_LDIV,
	EFLAG_REM,
	EFLAG_DIV,
	EFLAG_LSUB,
	EFLAG_CSNGL,
	EFLAG_CCNT1,
	EFLAG_CWORD,
	EFLAG_ADC,
	EFLAG_ADD,
	EFLAG_SUB,
	EFLAG_MUL,
	EFLAG_BAR,	/* Barrier underflow */
	EFLAG_FP,	/* Floating point error */
	EFLAG_ALT,
	EFLAG_CHAN,	/* Channel communication error */
	EFLAG_DMEM,	/* Memory allocator error */
	EFLAG_MT,	/* Mobile type error */
	EFLAG_PROC,	/* Process API error */
	EFLAG_SHUTDOWN	/* Erroneous shutdown */
};
/*}}}*/

/*{{{  Workspace constants and manipulation */
#define WS_TOP       0
#define WS_IPTR      1 /* Valid when: the process has been descheduled */
#define WS_NEXT      2 /* Valid when: the process is on a scheduling list */
#define WS_CHAN      3 /* Valid when: the process is waiting for */
#define WS_ALT_STATE 3 /*             communication, or is executing an ALT */
#define WS_NEXT_T    4 /* Valid when: the process is on */
#define WS_ALT_T     4 /*             a timer list */
#define WS_TIMEOUT   5 /* Valid when: the process is on a timer list */

#define WORKSPACE_GET(WPTR, LOC) \
	read_word(wordptr_minus((WPTR), (LOC)))
#define WORKSPACE_SET(WPTR, LOC, VAL) \
	write_word(wordptr_minus((WPTR), (LOC)), (VAL))
/*}}}*/

/*{{{  Externally provided functions */
extern WORD (*tvm_get_time)(void);
extern void (*tvm_invalid)(void);
extern void (*tvm_not_implemented)(void);
extern void (*tvm_runloop_pre)(void);
extern void (*tvm_runloop_post)(void);
/*}}}*/

/*{{{  The foregin function interface jump tables */
extern FFI_TABLE_ENTRY *ffi_table;
extern FFI_FUNCTION *special_ffi_table;
/*}}}*/

/*{{{  Interpreter API */
void tvm_init_stackframe(WORDPTR *where, int argc, WORD argv[],
		WORDPTR vectorspace, WORDPTR mobilespace, WORDPTR forkbarrier,
		int ret_type, BYTEPTR ret_addr);
void tvm_initial_stackframe(WORDPTR *where, int argc, WORD argv[],
	WORDPTR vectorspace, WORDPTR mobilespace, int add_forkingbarrier);
int tvm_run(void);
/*}}}*/

#endif /* TVM_H */
