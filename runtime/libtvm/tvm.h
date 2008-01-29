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
#include "tvm_time.h"

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

/*{{{  Execution context returns */
enum {
	ECTX_CONTINUE		= 0,
	ECTX_INIT,
	ECTX_INS_INVALID	= EXIT_INS_INVALID,
	ECTX_INS_UNSUPPORTED	= EXIT_INS_NOT_IMP,
	ECTX_EMPTY		= EXIT_DEADLOCK,
	ECTX_ERROR		= EXIT_ERROR,
	ECTX_RUNNING,
	ECTX_SHUTDOWN,
	ECTX_SLEEP,
	ECTX_TIME_SLICE
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
	EFLAG_SHUTDOWN,	/* Erroneous shutdown */
	EFLAG_FFI	/* FFI error (missing function) */
};
/*}}}*/

/*{{{  Memory allocator selection */
#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_USE_MALLOC)
#define TVM_MALLOC	malloc
#define TVM_FREE	free
#else
#define TVM_MALLOC	...ERROR...
#define TVM_FREE	...ERROR...
#endif /* !(TVM_DYNAMIC_MEMORY && TVM_USE_MALLOC) */
/*}}}*/

/*{{{  The foregin function interface jump tables */
extern FFI_TABLE_ENTRY *ffi_table;
extern FFI_FUNCTION *special_ffi_table;
/*}}}*/

/*{{{  Interpreter API */
extern void tvm_init_stackframe(WORDPTR *where, int argc, WORD argv[],
		WORDPTR vectorspace, WORDPTR mobilespace, WORDPTR forkbarrier,
		int ret_type, BYTEPTR ret_addr);
extern void tvm_initial_stackframe(WORDPTR *where, int argc, WORD argv[],
	WORDPTR vectorspace, WORDPTR mobilespace, int add_forkingbarrier);
extern void tvm_init(void);
extern int tvm_dispatch(ECTX ectx);
extern int tvm_run(ECTX ectx);
extern int tvm_run_count(ECTX ectx, UWORD count);
/*}}}*/

#endif /* TVM_H */
