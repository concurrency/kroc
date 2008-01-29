/*
tvm - interpreter.c
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

/* ANNO: FIXME: We use an integer for a for a test expression, ie
 * while(variable). Splint complains about this, and this may be due to to the
 * fact that it really would perhaps be more clear to write while(variable ==
 * 1). Anyways, I am going to disable that warning for now.
 */
/*@-predboolint@*/

#include "tvm.h"
#include "instructions.h"
#include "interpreter.h"
#include "ext_chan.h"

#ifdef TVM_DISPATCH_SWITCH
#include "mem.c"
#include "dmem.c"
#include "ins_alt.c"
#include "ins_barrier.c"
#include "ins_chan.c"
#include "ins_float.c"
#include "ins_fred.c"
#include "ins_mt.c"
#include "ins_pri.c"
#include "ins_proc.c"
#include "ins_rmox.c"
#include "ins_sec.c"
#include "ins_timer.c"
#include "ins_t800.c"
#include "instructions.c"
#include "dispatch_ins.c"
#else
#include "ins_mt.h"
#include "jumptbl_pri.h"
#include "scheduler.h"
#endif

#ifdef ENABLE_RUNLOOP_DBG_HOOKS
void (*runloop_dbg_hook_pre)(void) = 0;
void (*runloop_dbg_hook_post)(void) = 0;
#endif

/*
 * Allocate a new stackframe using the workspace pointer pointed to by 'where',
 * with argc number of arguments placed in the argv array (literal values or
 * addresses) with the addresses (or null) for vectorspace and mobilespace.
 *
 * 'where' use like 
 *    int *WPTR;
 *    ...
 *    init_stackframe(&WPTR, ...)
 */
void tvm_init_stackframe(WORDPTR *where, int argc, WORD argv[],
		WORDPTR vectorspace, WORDPTR mobilespace, WORDPTR forkingbarrier,
		int ret_type, BYTEPTR ret_addr)
{
	int index = 0;
	int i, frame_size, ret_index;

	/* Calculate the framesize, if below four, allocate four */
	frame_size = 1 + argc 
		+ (vectorspace ? 1 : 0) + (mobilespace? 1 : 0) + (forkingbarrier ? 1 : 0)
		+ (ret_type != RET_REAL ? 1 : 0);
	frame_size = (frame_size < 4 ? 4 : frame_size);

	*where = wordptr_minus(*where, frame_size);

	/* Store an index to the return pointer */
	ret_index = index++;

	/* Set up arguments */
	for(i = 0; i < argc; i++)
		write_word(wordptr_plus(*where, index++), (WORD)argv[i]);

	/* Set up vectorspace pointer if neccesary */
	if(vectorspace)
	{
		write_word(wordptr_plus(*where, index++), (WORD)vectorspace);
	}

	/* Set up mobilespace pointer if neccesary */
	if(mobilespace)
	{
		write_word(wordptr_plus(*where, index++), (WORD)mobilespace);
	}

	/* Set up forking barrier pointer if neccesary */
	if(forkingbarrier)
	{
		write_word(wordptr_plus(*where, index++), (WORD)forkingbarrier);
	}

	switch(ret_type) {
		case RET_SHUTDOWN:
			/* Put a shutdown instruction in top workspace */
			write_byte(byteptr_plus((BYTEPTR)wordptr_plus(*where, index), 0), 0x2F);
			write_byte(byteptr_plus((BYTEPTR)wordptr_plus(*where, index), 1), 0xFE);
			break;
		case RET_ERROR:
			/* Put a seterr instruction in top workspace */
			write_byte(byteptr_plus((BYTEPTR)wordptr_plus(*where, index), 0), 0x21);
			write_byte(byteptr_plus((BYTEPTR)wordptr_plus(*where, index), 1), 0xF0);
			break;
		default:
			break;
	}

	if(ret_type == RET_REAL)
	{
		/* Store the return pointer, to completion byte code */
		write_word(wordptr_plus(*where, ret_index), (WORD)ret_addr);
	}
	else
	{
		/* Store the return pointer, to completion byte code */
		write_word(wordptr_plus(*where, ret_index), (WORD)wordptr_plus(*where, index));
	}
}

void tvm_initial_stackframe(WORDPTR *where, int argc, WORD argv[],
		WORDPTR vectorspace, WORDPTR mobilespace, int add_forkingbarrier)
{
	WORDPTR fb = (WORDPTR) NULL_P;

	#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)
	if(add_forkingbarrier)
	{
		/* CGR FIXME: fb = mt_alloc(MT_MAKE_BARRIER(MT_BARRIER_FORKING), 0); */
	}
	#endif

	tvm_init_stackframe(where, argc, argv, vectorspace, mobilespace, fb, RET_SHUTDOWN, 0);
}

void tvm_init(void)
{
}

void tvm_reset_ectx(ECTX ectx)
{
	ectx->state 	= ECTX_INIT;
	ectx->eflags	= 0;
	ectx->sflags	= 0;

	/* evaluation stack */
	OREG = 0;
	AREG = 0;
	BREG = 0;
	CREG = 0;

	/* setup scheduler queues */
	WPTR = (WORDPTR)NOT_PROCESS_P;
	FPTR = (WORDPTR)NOT_PROCESS_P;
	BPTR = (WORDPTR)NOT_PROCESS_P;
	TPTR = (WORDPTR)NOT_PROCESS_P;
}

void tvm_init_ectx(ECTX ectx)
{
	tvm_reset_ectx(ectx);
	ectx->pri = 0;
	_tvm_install_scheduler(ectx);
}

int tvm_dispatch(ECTX ectx)
{
	BYTE instr;
	
	/* Read the instruction */
#if (defined MEMORY_INTF_BIGENDIAN)
	instr = *IPTR; /* FIXME */
#else
	instr = read_byte(IPTR);
#endif
	
	/* Increment instruction pointer */
	IPTR = byteptr_plus(IPTR, 1);

	/* Put the least significant bits in OREG */
	OREG |= (instr & 0x0f);

#ifdef TVM_DISPATCH_SWITCH
	return dispatch_instruction(ectx, instr);
#else
	/* Use the other bits to index into the jump table */
	return primaries[instr >> 4](ectx);
#endif
}

/** 
 * Runs an execution context until it exits for some reason.
 * Returning the exit reason.
 */
int tvm_run(ECTX ectx)
{
	int ret;

	ectx->state = ECTX_RUNNING;

	for(;;)
	{
		if ((ret = tvm_dispatch(ectx)))
		{
			return (ectx->state = ret);
		}
	}
}

/** 
 * Runs an execution context until it exits for some reason,
 * or reaches the instruction count.  Returning the exit reason.
 */
int tvm_run_count(ECTX ectx, UWORD count)
{
	int ret;

	ectx->state = ECTX_RUNNING;

	while(count--)
	{
		if ((ret = tvm_dispatch(ectx)))
		{
			return (ectx->state = ret);
		}
	}

	return ECTX_TIME_SLICE;
}

