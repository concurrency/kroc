/*
tvm - ins_fred.c
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
#include "ext_chan.h"

#include "scheduler.h"

#include "ins_sec.h"
#include "ins_fred.h"
#include "ins_pri.h"

/* The FFI tables, which we initialise to NULL to indicate that they have not
 * yet been provided. */
FFI_TABLE_ENTRY *ffi_table = NULL;
FFI_FUNCTION *special_ffi_table = NULL;

/* 0x23 - 0x22 0xF3 - boolinvert */
/* FIXME: DELETEME! This ETC special should not be turned into a special 
 * instruction, but rather the sequence of existing instructions:
 * NOT
 * LDC 1
 * AND
 * (I think that is correct)
 */
TVM_INSTRUCTION void ins_boolinvert(void)
{
	STACK((areg == 0?1:0), breg, creg);
}

/* 0x28 - 0x22 0xF8 - reschedule */
TVM_INSTRUCTION void ins_reschedule(void)
{
	tvm_add_to_queue(wptr, iptr);

	iptr = run_next_on_queue();
}

/* 0x24 - 0x22 0xF4 - widenshort */
TVM_INSTRUCTION void ins_widenshort(void)
{
	/* Kudos to Damian for suggesting casting rather than doing something stupid,
	 * like I would have done! clj3 */
	STACK(((WORD) ((HWORD)areg)), breg, creg);
}

/* 0x25 - 0x22 0xF5 - fficall */
TVM_INSTRUCTION void ins_fficall(void)
{
	FFI_FUNCTION ffi_func_addr;
	
	/* If the FFI table has not been created, then we dont want to
	 * run this instruction as we are probably going to jump into
	 * oblivion if that is the case */
	if(areg >= 0 && !ffi_table)
	{
	  /* FIXME: We need a seperate error trap for this */
		ins_not_implemented();
	}
	if(areg < 0 && !special_ffi_table)
	{
	  /* FIXME: We need a seperate error trap for this */
		ins_not_implemented();
	}

	/* Assume that if the FFI table has been set up that all is ok, and that we
	 * can start jumping from it, areg contains the index we are jumping from,
	 * each index in the table has two words, one (the first) is the one we are
	 * jumping to, the second is */
	if(areg >= 0)
	{
		ffi_func_addr = ffi_table[areg].func;
	}
	else
	{
		/* We do not multiply by 2, as there are no pointers to strings 
		 * in this table, we also need to make areg positive and zero indexed */
		ffi_func_addr = special_ffi_table[-(areg + 1)];
	}


	/* Now we got the address, jump! Hold on to your hats :p */
	/* Though we need to make sure that we pass the correct parameter, which is
	 * the wptr + 1 (+1 to avoid the iptr on the top of the stack). */
	ffi_func_addr(wordptr_real_address(wordptr_plus(wptr, 1)));
	
	/* FFI call is done, now we need to return, use ins_ret */
	ins_ret();
}

/* 0x26 - 0x22 0xF6 - lend3 - loopend3 (for step in replicators) */
TVM_INSTRUCTION void ins_lend3(void)
{
	/* Loop start offset comes in from areg */
	/* Loop control block ptr in breg */
	WORDPTR loopcount_ptr = wordptr_plus((WORDPTR) breg, 1);
	WORDPTR loopindex_ptr = (WORDPTR) breg;
	WORD loopcount = read_word(loopcount_ptr) - 1;

	//printf(">lend3\n");
	/* Decrement count */
	write_word(loopcount_ptr, loopcount);
	if(loopcount == 0)
	{
		/* Stop looping */
		//printf("  stop looping\n");
	}
	else
	{
		WORDPTR loopstep_ptr = wordptr_plus((WORDPTR) breg, 2);
		WORD loopindex = read_word(loopindex_ptr);

		//printf("  continue looping\n");
		/* Increment index, by step */
		WORD step = read_word(loopstep_ptr);
		write_word(loopindex_ptr, loopindex + step);
		/* Loop */
		iptr = byteptr_minus(iptr, areg);
	}
	//printf("<lend3\n");
}

/* 0x27 - 0x22 0xF7 - lendbw - backwards loopend */
TVM_INSTRUCTION void ins_lendbw(void)
{
	/* Loop start offset comes in from areg */
	/* Loop control block ptr in breg */
	WORDPTR loopcount_ptr = wordptr_plus((WORDPTR) breg, 1);
	WORDPTR loopindex_ptr = (WORDPTR) breg;
	WORD loopcount = read_word(loopcount_ptr) - 1;

	//printf(">lendbw\n");

	/* Decrement count */
	write_word(loopcount_ptr, loopcount);
	if(loopcount == 0)
	{
		/* Stop looping */
		//printf("  stop looping\n");
	}
	else
	{
		/* Decrement index */
		WORD loopindex = read_word(loopindex_ptr) - 1;
		//printf("  continue looping\n");
		write_word(loopindex_ptr, loopindex);
		/* Loop */
		iptr = byteptr_minus(iptr, areg);
	}
	//printf("<lendbw\n");
}

#ifdef TVM_OCCAM_PI

/* 0x14 - 0x21 0xF4 - extvrfy - external channel verify */
TVM_INSTRUCTION void ins_extvrfy(void)
{
	/* FIXME: Actually do the verify? */

	/* We should be popping two values off the stack as far as we can tell */
	STACK(creg, UNDEFINE(creg), UNDEFINE(creg));
}

/* 0x60 - 0x26 0xF0 - extin - external channel input */
TVM_INSTRUCTION void ins_extin(void)
{
	/* Due to the fact that the KRoC uses the least significant
	 * bit set to one to indicate a EXTERNAL channel, which is fine
	 * if this is an address, but for the interpreter it is NOT an
	 * address, but an INDEX, so we need to do some shifting to get
	 * around that...
	 */
	/* ANNO: Cast to UNSIGNED WORD as the behaviour of a shift on negative values
	 * is implementation defined */
	ext_chan_table[(UWORD)breg >> 1](areg, (BYTEPTR)creg);
}

/* 0x61 - 0x26 0xF1 - extout - external channel output */
TVM_INSTRUCTION void ins_extout(void)
{
	/* Due to the fact that the KRoC uses the least significant
	 * bit set to one to indicate a EXTERNAL channel, which is fine
	 * if this is an address, but for the interpreter it is NOT an
	 * address, but an INDEX, so we need to do some shifting to get
	 * around that...
	 */
	/* ANNO: Cast to UNSIGNED WORD as the behaviour of a shift on negative values
	 * is implementation defined */
	ext_chan_table[(UWORD)breg >> 1](areg, (BYTEPTR)creg);
}

#endif /* TVM_OCCAM_PI */

/* FIXME: We don't currently support priority in the scheduler, so these two
 * instructions do nothing. */

/* 0xA2 - 0x2A 0xF2 - getpri - get priority */
TVM_INSTRUCTION void ins_getpri(void)
{
	/* Always return priority 0. */
	STACK(0, areg, breg);
}

/* 0xA5 - 0x2A 0xF5 - setpri - set priority */
TVM_INSTRUCTION void ins_setpri(void)
{
	/* Ignore the new priority. */
	STACK(breg, creg, UNDEFINE(creg));
}

int saved_creg;
/* 0xAD - 0x2A 0xFD - ins_savecreg - save the creg *magic* :) */
TVM_INSTRUCTION void ins_savecreg(void)
{
	saved_creg = creg;
}

/* 0xAE - 0x2A 0xFE - ins_restorecreg - restore the creg *magic :) */
TVM_INSTRUCTION void ins_restorecreg(void)
{
	creg = saved_creg;
}

#ifdef TVM_OCCAM_PI

/****************************************************************************
 *              0x27 0xF_         0x27 0xF_         0x27 0xF_               *
 ****************************************************************************/

#define SEM_FPTR 0
#define SEM_BPTR 1

TVM_HELPER void tvm_sem_init(WORDPTR sem)
{
	write_word(sem, (WORD) (NOT_PROCESS_P | 1));
	write_word(wordptr_plus(sem, 1), (WORD) NOT_PROCESS_P);
}

TVM_HELPER void tvm_sem_claim(WORDPTR sem)
{
	WORD sem_fptr = read_word(wordptr_plus(sem, SEM_FPTR));

	/* Check to see if the semaphore is busy */
	if(sem_fptr == (WORD) (NOT_PROCESS_P | 1))
	{ 
		/* Nobody has this semaphore already, claim it as ours */
		write_word(wordptr_plus(sem, SEM_FPTR), (WORD) NOT_PROCESS_P);
	}
	else
	{
		/* It is, join the queue */
		WORKSPACE_SET(wptr, WS_NEXT, (WORD) NOT_PROCESS_P);
		/* Save our iptr */
		WORKSPACE_SET(wptr, WS_IPTR, (WORD) iptr);
		/* Check if the semaphores front pointer is null */
		if(sem_fptr == NOT_PROCESS_P)
		{
			/* Add us as the only element */
			write_word(wordptr_plus(sem, SEM_FPTR), (WORD) wptr);
			write_word(wordptr_plus(sem, SEM_BPTR), (WORD) wptr);
		}
		else
		{
			/* Add us as the last element */
			WORDPTR sem_bptr_ptr = wordptr_plus(sem, SEM_BPTR);
			WORDPTR sem_bptr = (WORDPTR) read_word(sem_bptr_ptr);
			WORKSPACE_SET(sem_bptr, WS_NEXT, (WORD) wptr);
			write_word(sem_bptr_ptr, (WORD) wptr);
		}
		iptr = run_next_on_queue();
	}
}

TVM_HELPER void tvm_sem_release(WORDPTR sem)
{
	WORDPTR sem_fptr_ptr = wordptr_plus(sem, SEM_FPTR);
	WORD sem_fptr = read_word(sem_fptr_ptr);

	/* Is anybody waiting on the semaphore? */
	if(sem_fptr == (WORD) NOT_PROCESS_P)
	{
		/* No, so we dont need to wake anybody */
		write_word(sem_fptr_ptr, (WORD) (NOT_PROCESS_P | 1));
	}
	else
	{
		/* Yes, so we need to update ptrs and schedule waiting process */
		write_word(sem_fptr_ptr, WORKSPACE_GET((WORDPTR) sem_fptr, WS_NEXT));

		/* Put the process we picked up semaphore queue onto run queue */
		tvm_just_add_to_queue((WORDPTR)sem_fptr);
	}
}

/* 0x7A - 0x27 0xFA - seminit - initialise semaphore */
TVM_INSTRUCTION void ins_seminit(void)
{

	tvm_sem_init((WORDPTR) areg);

	UNDEFINE_STACK();
}

/* 0x7B - 0x27 0xFB - semclaim - claim semaphore */
TVM_INSTRUCTION void ins_semclaim(void)
{
	tvm_sem_claim((WORDPTR) areg);

	UNDEFINE_STACK();
}

/* 0x7C - 0x27 0xFC - semrelease - release semaphore */
TVM_INSTRUCTION void ins_semrelease(void)
{
	tvm_sem_release((WORDPTR) areg);

	UNDEFINE_STACK();
}

/****************************************************************************
 *              0x2E 0xF_         0x2E 0xF_         0x2E 0xF_               *
 ****************************************************************************/


/* FIXME: All extended input instrucitons, priority is currently not handled */

/* 0xE8 - 0x2E 0xF8 - xable - Extended Input Enable */
TVM_INSTRUCTION void ins_xable(void)
{
	WORDPTR chan_addr = (WORDPTR) areg;
	WORDPTR other_wptr = (WORDPTR) read_word(chan_addr);

	/* This is like a single guard ALT */
	/* If channel is empty, then alt on it */
	if(other_wptr == NOT_PROCESS_P)
	{
		/* Save state, set ALT to waiting */
		WORKSPACE_SET(wptr, WS_ALT_STATE, WAITING_P);
		WORKSPACE_SET(wptr, WS_IPTR, (WORD) iptr);

		/* Put ourselves into the channel word */
		write_word(chan_addr, (WORD) wptr);

		/* Find something else to run */
		iptr = run_next_on_queue();
	}

	UNDEFINE_STACK();
}

/* 0xE9 - 0x2E 0xF9 - xin - Extended Input */
TVM_INSTRUCTION void ins_xin(void)
{
	BYTEPTR write_start = (BYTEPTR) creg;
	BYTEPTR read_start;
	WORDPTR chan_addr = (WORDPTR) breg;
	WORDPTR other_wptr;
	WORD num_bytes = areg;

	other_wptr = (WORDPTR) read_word(chan_addr);
	read_start = (BYTEPTR) WORKSPACE_GET(other_wptr, WS_CHAN);

	tvm_copy_data(write_start, read_start, num_bytes);

	UNDEFINE_STACK();
}

/* 0xEC - 0x2E 0xFC - xend - Extended Input End */
TVM_INSTRUCTION void ins_xend(void)
{
	WORDPTR chan_addr = (WORDPTR) areg;
	WORDPTR other_wptr = (WORDPTR) read_word(chan_addr);

	/* Set chan word to NOT_PROCESS_P */
	write_word(chan_addr, NOT_PROCESS_P);

	/* Put the outputting process on the run queue */
	tvm_just_add_to_queue(other_wptr);

	UNDEFINE_STACK();
}

#endif /* TVM_OCCAM_PI */

