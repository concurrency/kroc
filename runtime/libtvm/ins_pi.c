/*
tvm - ins_pi.c
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

#include "ins_sec.h"
#include "ins_pi.h"
#include "ins_pri.h"

/* 0x23 - 0x22 0xF3 - boolinvert */
/* FIXME: DELETEME! This ETC special should not be turned into a special 
 * instruction, but rather the sequence of existing instructions:
 * NOT
 * LDC 1
 * AND
 * (I think that is correct)
 */
TVM_INSTRUCTION (ins_boolinvert)
{
	STACK_RET((AREG == 0?1:0), BREG, CREG);
}

/* 0x28 - 0x22 0xF8 - reschedule */
TVM_INSTRUCTION (ins_reschedule)
{
	ADD_TO_QUEUE_IPTR(WPTR, IPTR);
	RUN_NEXT_ON_QUEUE_RET();
}

/* 0x24 - 0x22 0xF4 - widenshort */
TVM_INSTRUCTION (ins_widenshort)
{
	/* Kudos to Damian for suggesting casting rather than doing something stupid,
	 * like I would have done! clj3 */
	STACK_RET(((WORD) ((HWORD)AREG)), BREG, CREG);
}

/* 0x25 - 0x22 0xF5 - fficall */
TVM_INSTRUCTION (ins_fficall)
{
	FFI_FUNCTION ffi_func_addr;
	
	/* If the FFI table has not been created, then we dont want to
	 * run this instruction as we are probably going to jump into
	 * oblivion if that is the case */
	if(AREG >= 0 && (!ectx->ffi_table || AREG >= ectx->ffi_table_length))
	{
		SET_ERROR_FLAG_RET(EFLAG_FFI);
	}
	if(AREG < 0 && (!ectx->special_ffi_table || (-(AREG + 1)) >= ectx->special_ffi_table_length))
	{
		SET_ERROR_FLAG_RET(EFLAG_FFI);
	}

	/* Assume that if the FFI table has been set up that all is ok, and that we
	 * can start jumping from it, AREG contains the index we are jumping from,
	 * each index in the table has two words, one (the first) is the one we are
	 * jumping to, the second is */
	if(AREG >= 0)
	{
		ffi_func_addr = ectx->ffi_table[AREG].func;
	}
	else
	{
		/* We do not multiply by 2, as there are no pointers to strings 
		 * in this table, we also need to make AREG positive and zero indexed */
		ffi_func_addr = ectx->special_ffi_table[-(AREG + 1)];
	}

	/* Now we got the address, jump! Hold on to your hats :p */
	/* Though we need to make sure that we pass the correct parameter, which is
	 * the WPTR + 1 (+1 to avoid the IPTR on the top of the stack). */
	ffi_func_addr(wordptr_real_address(wordptr_plus(WPTR, 1)));
	
	/* FFI call is done, now we need to return, use ins_ret */
	return ins_ret(ectx);
}

/* 0x26 - 0x22 0xF6 - lend3 - loopend3 (for step in replicators) */
TVM_INSTRUCTION (ins_lend3)
{
	/* Loop start offset comes in from AREG */
	/* Loop control block ptr in BREG */
	WORDPTR loopcount_ptr = wordptr_plus((WORDPTR) BREG, 1);
	WORDPTR loopindex_ptr = (WORDPTR) BREG;
	WORD loopcount = read_word(loopcount_ptr) - 1;

	/* Decrement count */
	write_word(loopcount_ptr, loopcount);
	if(loopcount == 0)
	{
		/* Stop looping */
	}
	else
	{
		WORDPTR loopstep_ptr = wordptr_plus((WORDPTR) BREG, 2);
		WORD loopindex = read_word(loopindex_ptr);

		/* Increment index, by step */
		WORD step = read_word(loopstep_ptr);
		write_word(loopindex_ptr, loopindex + step);
		/* Loop */
		IPTR = byteptr_minus(IPTR, AREG);
	}
	
	return ECTX_CONTINUE;
}

/* 0x27 - 0x22 0xF7 - lendbw - backwards loopend */
TVM_INSTRUCTION (ins_lendbw)
{
	/* Loop start offset comes in from AREG */
	/* Loop control block ptr in BREG */
	WORDPTR loopcount_ptr = wordptr_plus((WORDPTR) BREG, 1);
	WORDPTR loopindex_ptr = (WORDPTR) BREG;
	WORD loopcount = read_word(loopcount_ptr) - 1;

	/* Decrement count */
	write_word(loopcount_ptr, loopcount);
	if(loopcount == 0)
	{
		/* Stop looping */
	}
	else
	{
		/* Decrement index */
		WORD loopindex = read_word(loopindex_ptr) - 1;
		write_word(loopindex_ptr, loopindex);
		/* Loop */
		IPTR = byteptr_minus(IPTR, AREG);
	}
	
	return ECTX_CONTINUE;
}

#ifdef TVM_OCCAM_PI

/* 0x14 - 0x21 0xF4 - extvrfy - external channel verify */
TVM_INSTRUCTION (ins_extvrfy)
{
	UWORD typehash 	= (UWORD) AREG;
	UWORD index	= ((UWORD)BREG) >> 1;

	if (!ectx->ext_chan_table || index >= ectx->ext_chan_table_length) {
		SET_ERROR_FLAG_RET(EFLAG_EXTCHAN);
	} else if (ectx->ext_chan_table[index].typehash == 0) {
		/* typehash = 0 means any type OK */
	} else if (ectx->ext_chan_table[index].typehash != typehash) {
		SET_ERROR_FLAG(EFLAG_EXTCHAN);
	}

	UNDEFINE_STACK_RET();
}

/* 0x60 - 0x26 0xF0 - extin - external channel input */
TVM_INSTRUCTION (ins_extin)
{
	/* Due to the fact that the KRoC uses the least significant
	 * bit set to one to indicate a EXTERNAL channel, which is fine
	 * if this is an address, but for the interpreter it is NOT an
	 * address, but an INDEX, so we need to do some shifting to get
	 * around that...
	 */
	UWORD index = ((UWORD)BREG) >> 1;
	if (!ectx->ext_chan_table || index >= ectx->ext_chan_table_length)
	{
		SET_ERROR_FLAG_RET(EFLAG_EXTCHAN);
	} 
	else if (!ectx->ext_chan_table[index].in)
	{
		SET_ERROR_FLAG_RET(EFLAG_EXTCHAN);
	}
	else 
	{
		return ectx->ext_chan_table[index].in(ectx, AREG, (BYTEPTR)CREG);
	}
}

/* 0x61 - 0x26 0xF1 - extout - external channel output */
TVM_INSTRUCTION (ins_extout)
{
	/* Due to the fact that the KRoC uses the least significant
	 * bit set to one to indicate a EXTERNAL channel, which is fine
	 * if this is an address, but for the interpreter it is NOT an
	 * address, but an INDEX, so we need to do some shifting to get
	 * around that...
	 */
	UWORD index = ((UWORD)BREG) >> 1;
	if (!ectx->ext_chan_table || index >= ectx->ext_chan_table_length)
	{
		SET_ERROR_FLAG_RET(EFLAG_EXTCHAN);
	} 
	else if (!ectx->ext_chan_table[index].out)
	{
		SET_ERROR_FLAG_RET(EFLAG_EXTCHAN);
	}
	else 
	{
		return ectx->ext_chan_table[index].out(ectx, AREG, (BYTEPTR)CREG);
	}
}

#endif /* TVM_OCCAM_PI */

/* FIXME: We don't currently support priority in the scheduler, so these two
 * instructions do nothing. */

/* 0xA2 - 0x2A 0xF2 - getpri - get priority */
TVM_INSTRUCTION (ins_getpri)
{
	/* Always return priority 0. */
	STACK_RET(0, AREG, BREG);
}

/* 0xA5 - 0x2A 0xF5 - setpri - set priority */
TVM_INSTRUCTION (ins_setpri)
{
	/* Ignore the new priority. */
	STACK_RET(BREG, CREG, UNDEFINE(CREG));
}

/* 0xAD - 0x2A 0xFD - ins_savecreg - save the creg */
TVM_INSTRUCTION (ins_savecreg)
{
	ectx->_creg = CREG;
	return ECTX_CONTINUE;
}

/* 0xAE - 0x2A 0xFE - ins_restorecreg - restore the creg */
TVM_INSTRUCTION (ins_restorecreg)
{
	CREG = ectx->_creg;
	return ECTX_CONTINUE;
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

TVM_HELPER int tvm_sem_claim(ECTX ectx, WORDPTR sem)
{
	WORD sem_fptr = read_word(wordptr_plus(sem, SEM_FPTR));

	/* Check to see if the semaphore is busy */
	if(sem_fptr == (WORD) (NOT_PROCESS_P | 1))
	{ 
		/* Nobody has this semaphore already, claim it as ours */
		write_word(wordptr_plus(sem, SEM_FPTR), (WORD) NOT_PROCESS_P);
		UNDEFINE_STACK_RET();
	}
	else
	{
		/* It is, join the queue */
		WORKSPACE_SET(WPTR, WS_LINK, (WORD) NOT_PROCESS_P);
		/* Save execution context */
		WORKSPACE_SET(WPTR, WS_ECTX, (WORD) ectx);
		/* Save our IPTR */
		WORKSPACE_SET(WPTR, WS_IPTR, (WORD) IPTR);
		/* Check if the semaphores front pointer is null */
		if(sem_fptr == NOT_PROCESS_P)
		{
			/* Add us as the only element */
			write_word(wordptr_plus(sem, SEM_FPTR), (WORD) WPTR);
			write_word(wordptr_plus(sem, SEM_BPTR), (WORD) WPTR);
		}
		else
		{
			/* Add us as the last element */
			WORDPTR sem_bptr_ptr = wordptr_plus(sem, SEM_BPTR);
			WORDPTR sem_bptr = (WORDPTR) read_word(sem_bptr_ptr);
			WORKSPACE_SET(sem_bptr, WS_LINK, (WORD) WPTR);
			write_word(sem_bptr_ptr, (WORD) WPTR);
		}
		RUN_NEXT_ON_QUEUE_RET();
	}
}

TVM_HELPER int tvm_sem_release(ECTX ectx, WORDPTR sem)
{
	WORDPTR sem_fptr_ptr = wordptr_plus(sem, SEM_FPTR);
	WORD sem_fptr = read_word(sem_fptr_ptr);

	/* Is anybody waiting on the semaphore? */
	if(sem_fptr == (WORD) NOT_PROCESS_P)
	{
		/* No, so we dont need to wake anybody */
		write_word(sem_fptr_ptr, (WORD) (NOT_PROCESS_P | 1));
		UNDEFINE_STACK_RET();
	}
	else
	{
		/* Yes, so we need to update ptrs and schedule waiting process */
		write_word(sem_fptr_ptr, WORKSPACE_GET((WORDPTR) sem_fptr, WS_LINK));

		/* Put the process we picked up semaphore queue onto run queue */
		ADD_TO_QUEUE_ECTX_RET((WORDPTR)sem_fptr);
	}
}

/* 0x7A - 0x27 0xFA - seminit - initialise semaphore */
TVM_INSTRUCTION (ins_sem_init)
{

	tvm_sem_init((WORDPTR) AREG);

	UNDEFINE_STACK_RET();
}

/* 0x7B - 0x27 0xFB - semclaim - claim semaphore */
TVM_INSTRUCTION (ins_sem_claim)
{
	return tvm_sem_claim(ectx, (WORDPTR) AREG);
}

/* 0x7C - 0x27 0xFC - semrelease - release semaphore */
TVM_INSTRUCTION (ins_sem_release)
{
	return tvm_sem_release(ectx, (WORDPTR) AREG);
}

/****************************************************************************
 *              0x2E 0xF_         0x2E 0xF_         0x2E 0xF_               *
 ****************************************************************************/

/* 0xE8 - 0x2E 0xF8 - xable - Extended Input Enable */
TVM_INSTRUCTION (ins_xable)
{
	WORDPTR chan_addr = (WORDPTR) AREG;
	WORDPTR other_WPTR = (WORDPTR) read_word(chan_addr);

	/* This is like a single guard ALT */
	/* If channel is empty, then alt on it */
	if(other_WPTR == NOT_PROCESS_P)
	{
		/* Save state, set ALT to waiting */
		WORKSPACE_SET(WPTR, WS_STATE, WAITING_P);
		WORKSPACE_SET(WPTR, WS_IPTR, (WORD) IPTR);

		/* Put ourselves into the channel word */
		write_word(chan_addr, (WORD) WPTR);

		/* Find something else to run */
		RUN_NEXT_ON_QUEUE_RET();
	}

	UNDEFINE_STACK_RET();
}

/* 0xE9 - 0x2E 0xF9 - xin - Extended Input */
TVM_INSTRUCTION (ins_xin)
{
	BYTEPTR write_start = (BYTEPTR) CREG;
	BYTEPTR read_start;
	WORDPTR chan_addr = (WORDPTR) BREG;
	WORDPTR other_WPTR;
	WORD num_bytes = AREG;

	other_WPTR = (WORDPTR) read_word(chan_addr);
	read_start = (BYTEPTR) WORKSPACE_GET(other_WPTR, WS_POINTER);

	tvm_copy_data(write_start, read_start, num_bytes);

	UNDEFINE_STACK_RET();
}

/* 0xEC - 0x2E 0xFC - xend - Extended Input End */
TVM_INSTRUCTION (ins_xend)
{
	WORDPTR chan_addr = (WORDPTR) AREG;
	WORDPTR other_WPTR = (WORDPTR) read_word(chan_addr);

	/* Set chan word to NOT_PROCESS_P */
	write_word(chan_addr, NOT_PROCESS_P);

	/* Put the outputting process on the run queue */
	ADD_TO_QUEUE_ECTX_RET(other_WPTR);
}

#endif /* TVM_OCCAM_PI */

/* 0xFD - 0x2F 0xFD - null - put null onto the stack */
TVM_INSTRUCTION (ins_null)
{
	STACK_RET((WORD) NULL_P, AREG, BREG);
}

