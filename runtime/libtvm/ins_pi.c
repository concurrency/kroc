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
TVM_INSTRUCTION (ins_boolinvert)
{
	STACK_RET ((AREG == 0 ? 1 : 0), BREG, CREG, STYPE_DATA, BREGt, CREGt);
}

/* 0x28 - 0x22 0xF8 - reschedule */
TVM_INSTRUCTION (ins_reschedule)
{
	DESCHEDULE_CURRENT ();
	RUN_NEXT_ON_QUEUE_RET ();
}

/* 0x24 - 0x22 0xF4 - widenshort */
TVM_INSTRUCTION (ins_widenshort)
{
	STACK_RET (((WORD) ((HWORD) AREG)), BREG, CREG, STYPE_DATA, BREGt, CREGt);
}

/* 0x25 - 0x22 0xF5 - fficall */
TVM_INSTRUCTION (ins_fficall)
{
	/* Arguments are at WPTR + 1 (+1 to avoid the IPTR on the top of the stack). */
	WORD *args = wordptr_real_address (wordptr_plus (WPTR, 1));
	
	/* Normal FFI (AREG >= 0), or Special FFI (AREG < 0) */
	if (AREG >= 0) {
		/* Valid FFI call ? */
		FFI_FUNCTION func = NULL;

		if (ectx->ffi_table && (AREG < ectx->ffi_table_length)) {
			func = ectx->ffi_table[AREG].func;
		}

		/* Function defined ? */
		if (func != NULL) {
			func (args);
		} else {	
			SET_ERROR_FLAG (EFLAG_FFI);
		} 
	} else {
		SFFI_FUNCTION func = NULL;
		unsigned int index = -(AREG + 1);
		int ret;

		/* Valid FFI call ? */
		if (ectx->sffi_table && (index < ectx->sffi_table_length)) {
			func = ectx->sffi_table[index];
		}
		/* Function defined ? */
		if (func != NULL) {
			ret = func (ectx, args);
		} else {
			SET_ERROR_FLAG (EFLAG_FFI);
			ret = ECTX_CONTINUE;
		}
		
		switch (ret) {
			case ECTX_CONTINUE:
				break;
			case SFFI_BYPASS:
				return ECTX_CONTINUE;
			case SFFI_RESCHEDULE:
				RUN_NEXT_ON_QUEUE_RET ();
			default:
				return ret;
		}
	}

	/* FFI call is done, return using ins_ret */
	return ins_ret (ectx);
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

/* 0x27 - 0x22 0xF7 - lendb - backwards loopend */
TVM_INSTRUCTION (ins_lendb)
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
	UWORD index	= ((UWORD) BREG) >> 1;

	if (!ectx->ext_chan_table || index >= ectx->ext_chan_table_length) {
		SET_ERROR_FLAG_RET (EFLAG_EXTCHAN);
	} else if (ectx->ext_chan_table[index].typehash == 0) {
		/* typehash = 0 means any type OK */
	} else if (ectx->ext_chan_table[index].typehash != typehash) {
		SET_ERROR_FLAG (EFLAG_EXTCHAN);
	}

	UNDEFINE_STACK_RET ();
}

/* 0x60 - 0x26 0xF0 - extin - external channel input */
TVM_INSTRUCTION (ins_extin)
{
	/* Convert address to table index */
	UWORD index = ((UWORD) BREG) >> 1;
	
	if (ectx->ext_chan_table && index < ectx->ext_chan_table_length) {
		EXT_CHAN_FUNCTION func = ectx->ext_chan_table[index].in;

		if (func) {
			return func (ectx, AREG, (BYTEPTR) CREG);
		}
	}
	
	SET_ERROR_FLAG_RET (EFLAG_EXTCHAN);
}

/* 0x61 - 0x26 0xF1 - extout - external channel output */
TVM_INSTRUCTION (ins_extout)
{
	/* Convert address to table index */
	UWORD index = ((UWORD) BREG) >> 1;
	
	if (ectx->ext_chan_table && index < ectx->ext_chan_table_length) {
		EXT_CHAN_FUNCTION func = ectx->ext_chan_table[index].out;

		if (func) {
			return func (ectx, AREG, (BYTEPTR) CREG);
		}
	}
	
	SET_ERROR_FLAG_RET (EFLAG_EXTCHAN);
}

#ifdef TVM_DYNAMIC_OCCAM_PI

/* 0x24B - 0x22 0x24 0xFB - ext_mt_in - external channel mobile type input */
TVM_INSTRUCTION (ins_ext_mt_in)
{
	/* Convert address to table index */
	UWORD index = ((UWORD) AREG) >> 1;
	
	if (ectx->ext_chan_table && index < ectx->ext_chan_table_length) {
		EXT_CHAN_MT_FUNCTION func = ectx->ext_chan_table[index].mt_in;

		if (func) {
			return func (ectx, (WORDPTR) BREG);
		}
	}
	
	SET_ERROR_FLAG_RET (EFLAG_EXTCHAN);
}

/* 0x24C - 0x22 0x24 0xFC - ext_mt_out - external channel mobile type output */
TVM_INSTRUCTION (ins_ext_mt_out)
{
	/* Convert address to table index */
	UWORD index = ((UWORD) AREG) >> 1;
	
	if (ectx->ext_chan_table && index < ectx->ext_chan_table_length) {
		EXT_CHAN_MT_FUNCTION func = ectx->ext_chan_table[index].mt_out;

		if (func) {
			return func (ectx, (WORDPTR) BREG);
		}
	}
	
	SET_ERROR_FLAG_RET (EFLAG_EXTCHAN);
}

#endif /* TVM_DYANMIC_OCCAM_PI */

#endif /* TVM_OCCAM_PI */

/* FIXME: We don't currently support priority in the scheduler, so these two
 * instructions do nothing. */

/* 0xA2 - 0x2A 0xF2 - getpri - get priority */
TVM_INSTRUCTION (ins_getpri)
{
	/* Always return priority 0. */
	STACK_RET (0, AREG, BREG, STYPE_DATA, AREGt, BREGt);
}

/* 0xA5 - 0x2A 0xF5 - setpri - set priority */
TVM_INSTRUCTION (ins_setpri)
{
	/* Ignore the new priority. */
	STACK2_RET (BREG, CREG, BREGt, CREGt);
}

/* 0xAD - 0x2A 0xFD - ins_savecreg - save the creg */
TVM_INSTRUCTION (ins_savecreg)
{
	ectx->_creg = CREG;
	#ifdef TVM_TYPE_SHADOW
	ectx->_cregT = CREGt;
	#endif
	return ECTX_CONTINUE;
}

/* 0xAE - 0x2A 0xFE - ins_restorecreg - restore the creg */
TVM_INSTRUCTION (ins_restorecreg)
{
	CREG = ectx->_creg;
	SET_CREGt (ectx->_cregT);
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

TVM_HELPER int tvm_sem_release (ECTX ectx, WORDPTR sem)
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

	tvm_sem_init ((WORDPTR) AREG);

	UNDEFINE_STACK_RET ();
}

/* 0x7B - 0x27 0xFB - semclaim - claim semaphore */
TVM_INSTRUCTION (ins_sem_claim)
{
	return tvm_sem_claim (ectx, (WORDPTR) AREG);
}

/* 0x7C - 0x27 0xFC - semrelease - release semaphore */
TVM_INSTRUCTION (ins_sem_release)
{
	return tvm_sem_release (ectx, (WORDPTR) AREG);
}

/****************************************************************************
 *              0x2E 0xF_         0x2E 0xF_         0x2E 0xF_               *
 ****************************************************************************/

/* 0xE0 - 0x2E 0xF0 - checknotnull - Check Pointer Not NULL */
TVM_INSTRUCTION (ins_checknotnull)
{
	if (AREG != (WORD) NULL_P) {
		/* stack untouched */
		return ECTX_CONTINUE;
	} else {
		SET_ERROR_FLAG_RET (EFLAG_SETERR);
	}
}

/* 0xE8 - 0x2E 0xF8 - xable - Extended Channel I/O Enable */
TVM_INSTRUCTION (ins_xable)
{
	WORDPTR chan_ptr	= (WORDPTR) AREG;
	WORD chan_value		= read_word (chan_ptr);

	/* This is like a single guard ALT */
	/* If channel is empty, then alt on it */

	#ifdef TVM_EXTERNAL_CHANNEL_BUNDLES
	if (chan_value & 2) {
		WORDPTR cb		= (WORDPTR) (chan_value & (~3));
		EXT_CB_INTERFACE *intf	= (EXT_CB_INTERFACE *) read_offset (cb, mt_cb_ext_t, interface);
		void *ext_data 		= (void *) read_offset (cb, mt_cb_ext_t, data);
		if (intf != NULL) {
			if (intf->xable != NULL) {
				return intf->xable (ectx, ext_data, chan_ptr);
			}
		}
		
		SET_ERROR_FLAG_RET (EFLAG_EXTCHAN);
	}
	else
	#endif /* TVM_EXTERNAL_CHANNEL_BUNDLES */
	if (chan_value == NOT_PROCESS_P) {
		/* Save state, set ALT to waiting */
		WORKSPACE_SET (WPTR, WS_STATE, WAITING_P);
		WORKSPACE_SET (WPTR, WS_ECTX, (WORD) ectx);
		WORKSPACE_SET (WPTR, WS_PENDING, (WORD) chan_ptr);
		WORKSPACE_SET (WPTR, WS_IPTR, (WORD) IPTR);

		/* Put ourselves into the channel word */
		write_word (chan_ptr, ((WORD) WPTR) | 1);

		/* Find something else to run */
		RUN_NEXT_ON_QUEUE_RET ();
	}

	UNDEFINE_STACK_RET ();
}

#ifdef TVM_EXTERNAL_CHANNEL_BUNDLES
TVM_HELPER int channel_ext_xin (ECTX ectx, EXT_CB_INTERFACE *intf, void *ext_data, WORDPTR chan_ptr, BYTEPTR data_ptr, WORD data_len)
{
	if (intf->xin != NULL)
		return intf->xin (ectx, ext_data, chan_ptr, data_ptr, data_len);
	else
		SET_ERROR_FLAG_RET (EFLAG_EXTCHAN);
}
#endif /* TVM_EXTERNAL_CHANNEL_BUNDLES */

/* 0xE9 - 0x2E 0xF9 - xin - Extended Input */
TVM_INSTRUCTION (ins_xin)
{
	BYTEPTR data_ptr = (BYTEPTR) CREG;
	WORDPTR chan_ptr = (WORDPTR) BREG;
	WORDPTR requeue;
	WORD data_len = AREG;
	int ret;
	
	UNDEFINE_STACK ();

	ret = chan_io (
		ectx,
		chan_ptr, data_ptr, data_len,
		&requeue,
		channel_input, channel_dc_input,
		IF_EXTERNAL_CHANNEL_BUNDLES (channel_ext_xin)
	);

	if (ret > 0) {
		return ret;
	}
	else if (ret != 0) {
		SET_ERROR_FLAG_RET (EFLAG_CHAN);
	}
	else if (requeue == (WORDPTR) (NOT_PROCESS_P | 1)) {
		DESCHEDULE_CURRENT ();
		RUN_NEXT_ON_QUEUE_RET ();
	}

	/* Restore output process to channel word */
	write_word (chan_ptr, (WORD) requeue);

	return ECTX_CONTINUE;
}

/* 0xEC - 0x2E 0xFC - xend - Extended Channel I/O End */
TVM_INSTRUCTION (ins_xend)
{
	WORDPTR chan_ptr = (WORDPTR) AREG;
	WORD chan_value = read_word (chan_ptr);

	UNDEFINE_STACK ();

	/* Disconnected channel ? */
	if (chan_value != (NOT_PROCESS_P | 1)) {
		/* No; reset channel */
		write_word (chan_ptr, NOT_PROCESS_P);
		/* Put the outputting process on the run queue */
		ADD_TO_QUEUE_ECTX_RET ((WORDPTR) (chan_value & (~1)));
	}

	return ECTX_CONTINUE;
}

#endif /* TVM_OCCAM_PI */

/* 0xFD - 0x2F 0xFD - null - put null onto the stack */
TVM_INSTRUCTION (ins_null)
{
	STACK_RET ((WORD) NULL_P, AREG, BREG, STYPE_NULL, AREGt, BREGt);
}

