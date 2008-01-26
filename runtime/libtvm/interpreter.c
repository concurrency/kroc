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
#endif

/* Using setjmp/longjmp lets us use a thighter interpreter loop, as we do
 * not need to test if we are finished running after every instruction.
 * On the intel this saves us about 3 instructions. Probably quite significant
 * considering how much the interpreter loop is going to run.
 * FIXME: Profile this
 */
#ifdef HAVE_SETJMP_H
#	include <setjmp.h>
#endif

#ifdef ENABLE_RUNLOOP_DBG_HOOKS
void (*runloop_dbg_hook_pre)(void) = 0;
void (*runloop_dbg_hook_post)(void) = 0;
#endif

#ifdef HAVE_SETJMP_H
/* If we are using setjmp we need somewhere to store the environment in order
 * to be able to jump out of the interpreter loop 
 */
jmp_buf runloop_env; 
#else
/* If we are not using setjmp we use a variable which is set to zero when we
 * need to exit from the interpreter loop.
 */
int running;
#endif
int exit_value; /* Blame Ed for the name of this variable */

void exit_runloop(int exit_val)
{
	exit_value = exit_val;

#ifdef HAVE_SETJMP_H
	longjmp(runloop_env, 1);
#else
	running = 0;
#endif

}

/*
 * Allocate a new stackframe using the workspace pointer pointed to by 'where',
 * with argc number of arguments placed in the argv array (literal values or
 * addresses) with the addresses (or null) for vectorspace and mobilespace.
 *
 * 'where' use like 
 *    int *wptr;
 *    ...
 *    init_stackframe(&wptr, ...)
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
	#ifdef __MOBILE_PI_SUPPORT__
	if(forkingbarrier)
	{
		write_word(wordptr_plus(*where, index++), (WORD)forkingbarrier);
	}
	#endif

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

	#ifdef __MOBILE_PI_SUPPORT__
	if(add_forkingbarrier)
	{
		fb = mt_alloc(MT_MAKE_BARRIER(MT_BARRIER_FORKING), 0);
	}
	#endif

	tvm_init_stackframe(where, argc, argv, vectorspace, mobilespace, fb, RET_SHUTDOWN, 0);
}

/** 
 * Starts the main run loop of the interpreter, fetching code starting from
 * code_ptr. This function will exit when the interpreted program has finished
 * executing. Registers and memory will be left in the state of the finished
 * interpreted program.
 *
 * The interpreter must have been setup prior to calling this function.
 *
 * @return Nothing, when the interpreted program has finished.
 */
int tvm_run(void)
{
	/* FIXME: Set up a stack frame we can return from to a special location
	 * which will break us out of this loop.
	 */
#ifdef HAVE_SETJMP_H
	if(setjmp(runloop_env) == 0)
	{
		for(;;)
		{
#else
	running = 1;
	while(running)
	{
		{ /* This to get the right number of brackets */
#endif
			/* FIXME: This here is to ensure that the iptr gets
			 * updated correctly while still decoding and running
			 * the correct instruction.  Check that the soccam
			 * interpreter does this correctly, which it may or may
			 * not do... */
			BYTE instr;

#ifdef ENABLE_RUNLOOP_DBG_HOOKS
			if(tvm_runloop_pre)
			{
				tvm_runloop_pre();
			}
#endif
			/* Increment the instruction pointer */
			//printf("iptr = %08x\n", (int) iptr);
#if (defined MEMORY_INTF_BIGENDIAN)
			/* FIXME: This is bad! but gets the lego working */
			instr = *iptr;
#else
			instr = read_byte(iptr);
#endif
			//printf("  instr = %02x\n", instr);
			iptr = byteptr_plus(iptr, 1);

			/* Put the least significant bits in oreg */
			oreg |= (instr & 0x0f);

#ifndef TVM_DISPATCH_SWITCH
			/* Use the other bits to index into the jump table */
			/* ANNO: It is OK to use a BYTE to index into this array, and not an int
			 * type, so we allow chars as array indexes */
			/*@+charindex@*/
			primaries[instr >> 4]();
			/*@=charindex@*/
#else
			dispatch_instruction(instr);
#endif

#ifdef ENABLE_RUNLOOP_DBG_HOOKS
			if(tvm_runloop_post)
			{
				tvm_runloop_post();
			}
#endif
		}
	}

	return exit_value;
}

