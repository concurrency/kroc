/*
tvm - ins_barrier.c
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

#include "tvm.h"
#include "instructions.h"
#include "scheduler.h"
#include "ins_barrier.h"

#ifdef TVM_OCCAM_PI

#define BAR_ENROLLED	0
#define BAR_COUNT	1
#define BAR_FPTR 	2
#define BAR_BPTR 	3

static void tvm_bar_complete(ECTX ectx, WORDPTR bar)
{
	/* Load barrier state. */
	WORD enrolled	= read_word(wordptr_plus(bar, BAR_ENROLLED));
	WORD bar_fptr	= read_word(wordptr_plus(bar, BAR_FPTR));
	WORD bar_bptr	= read_word(wordptr_plus(bar, BAR_BPTR));

	/* Reset barrier state. */
	write_word(wordptr_plus(bar, BAR_COUNT), enrolled);
	write_word(wordptr_plus(bar, BAR_FPTR), (WORD) NOT_PROCESS_P);
	write_word(wordptr_plus(bar, BAR_BPTR), (WORD) NOT_PROCESS_P);

	if(bar_fptr != (WORD) NOT_PROCESS_P)
	{
		/* Reschedule processes queued on barrier. */
		ADD_QUEUE_TO_QUEUE((WORDPTR) bar_fptr, (WORDPTR) bar_bptr);
	}
}

TVM_HELPER void tvm_bar_init(WORDPTR bar, UWORD initial_count)
{
	write_word(wordptr_plus(bar, BAR_ENROLLED), (WORD) initial_count);
	write_word(wordptr_plus(bar, BAR_COUNT), (WORD) initial_count);
	write_word(wordptr_plus(bar, BAR_FPTR), (WORD) NOT_PROCESS_P);
	write_word(wordptr_plus(bar, BAR_BPTR), (WORD) NOT_PROCESS_P);
}

TVM_HELPER int tvm_bar_sync(ECTX ectx, WORDPTR bar)
{
	UWORD count = (UWORD) read_word(wordptr_plus(bar, BAR_COUNT));

	/* Is this the last process to synchronise? */
	if(count > 1)
	{
		/* No, other processes have yet to synchronise, so deschedule. */
		WORDPTR bar_bptr	= (WORDPTR)read_word(wordptr_plus(bar, BAR_BPTR));

		/* Drop synchronisation count. */
		write_word(wordptr_plus(bar, BAR_COUNT), count - 1);

		/* Stop process. */
		WORKSPACE_SET(WPTR, WS_NEXT, (WORD) bar_bptr);
		WORKSPACE_SET(WPTR, WS_IPTR, (WORD) IPTR);

		/* Enqueue process to barrier. */
		if(bar_bptr == (WORDPTR) NOT_PROCESS_P)
		{
			/* Only process in queue. */
			write_word(wordptr_plus(bar, BAR_FPTR), (WORD) WPTR);
		}
		else
		{
			/* Last process in queue. */
			WORKSPACE_SET(bar_bptr, WS_NEXT, (WORD) WPTR);
		}
		write_word(wordptr_plus(bar, BAR_BPTR), (WORD) WPTR);

		/* Schedule new process. */
		RUN_NEXT_ON_QUEUE_RET();
	}
	else
	{
		/* Yes, complete barrier. */
		tvm_bar_complete(ectx, bar);
		return ECTX_CONTINUE;
	}
}

TVM_HELPER int tvm_bar_enroll(ECTX ectx, WORDPTR bar, UWORD enroll_count)
{
	UWORD enrolled	= read_word(wordptr_plus(bar, BAR_ENROLLED));
	UWORD count	= read_word(wordptr_plus(bar, BAR_COUNT));

	if((enrolled + enroll_count) < enrolled)
	{
		/* Enroll count overflow, virtually impossible under sane conditions. */
		return ectx->set_error_flag(ectx, EFLAG_BAR);
	}

	write_word(wordptr_plus(bar, BAR_ENROLLED), (WORD) (enrolled + enroll_count));
	write_word(wordptr_plus(bar, BAR_COUNT), (WORD) (count + enroll_count));

	return ECTX_CONTINUE;
}

TVM_HELPER int tvm_bar_resign(ECTX ectx, WORDPTR bar, UWORD resign_count)
{
	UWORD enrolled	= (UWORD) read_word(wordptr_plus(bar, BAR_ENROLLED));
	UWORD count	= (UWORD) read_word(wordptr_plus(bar, BAR_COUNT));

	if((resign_count > enrolled) || (resign_count > count))
	{
		/* Attempt to resign more processes than enrolled,
		 * or resign processes that are synchronising.
		 */
		return ectx->set_error_flag(ectx, EFLAG_BAR);
	}

	/* Update enroll count. */
	write_word(wordptr_plus(bar, BAR_ENROLLED), (WORD) (enrolled - resign_count));
	/* Calculate new synchronisation count. */
	count -= resign_count;
	
	if(count >= 1)
	{
		/* Update synchronisation count. */
		write_word(wordptr_plus(bar, BAR_COUNT), (WORD) count);
	}
	else
	{
		/* Count would drop to 0, so complete barrier instead. */
		tvm_bar_complete(ectx, bar);
	}

	return ECTX_CONTINUE;
}

/* 0xB0 - 0x2B 0xF0 - barrier intialisation */
TVM_INSTRUCTION (ins_bar_init)
{
	tvm_bar_init((WORDPTR) AREG, 0);

	UNDEFINE_STACK_RET();
}

/* 0xB1 - 0x2B 0xF1 - barrier synchronisation */
TVM_INSTRUCTION (ins_bar_sync)
{
	return tvm_bar_sync(ectx, (WORDPTR) AREG);
}

/* 0xB2 - 0x2B 0xF2 - barrier resignation */
TVM_INSTRUCTION (ins_bar_resign)
{
	return tvm_bar_resign(ectx, (WORDPTR) BREG, (UWORD) AREG);
}

/* 0xB3 - 0x2B 0xF3 - barrier enroll */
TVM_INSTRUCTION (ins_bar_enroll)
{
	return tvm_bar_enroll(ectx, (WORDPTR) BREG, (UWORD) AREG);
}

#endif /* TVM_OCCAM_PI */
