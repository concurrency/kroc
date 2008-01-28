/*
tvm - dmem.c
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
#include "interpreter.h"
#include "scheduler.h"

#include "pool_alloc.h"

#include "ins_chan.h"
#include "ins_fred.h"
#include "ins_mt.h"
#include "dmem.h"

#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)

/****************************************************************************
 *              0x26 0xF_         0x26 0xF_         0x26 0xF_               *
 ****************************************************************************/

/* 0x11 - 0x21 0xF1 - mreleasep - dynamic process release */
TVM_INSTRUCTION (ins_mreleasep)
{
	/* Find the pointer to the allocated block, size comes in from AREG */
	BYTEPTR ptr = ((BYTEPTR)WPTR) + (AREG * TVM_WORD_LENGTH);
	//printf(">mreleasep\n");
	//printf("  ptr = 0x%08x\n", (WORD) ptr);
	mt_release_simple(ectx, (WORDPTR) ptr, MT_MAKE_TYPE(MT_DATA));

	return run_next_on_queue();
	//printf("<mreleasep\n");
}

/****************************************************************************
 *              0x26 0xF_         0x26 0xF_         0x26 0xF_               *
 ****************************************************************************/

/* 0x62 - 0x26 0xF2 - minn - multi dimension mobile array input */
TVM_INSTRUCTION (ins_minn)
{
	/* Set up the AREG to be in bytes (what in expects) rather than 
	 * words (what minn gets)
	 */
	AREG = AREG << WSH;

	return ins_in(ectx);
}

/* 0x64 - 0x26 0xF4 - moutn - multi dimension mobile array output */
TVM_INSTRUCTION (ins_moutn)
{
	/* Set up the AREG to be in bytes (what out expects) rather than 
	 * words (what moutn gets)
	 */
	AREG = AREG << WSH;

	return ins_out(ectx);
}

#if 0 /* no recent compiler ever generates this instruction */
/* 0xE0 - 0x2E 0xF0 - mnew - dynamic allocation from pool */
TVM_INSTRUCTION (ins_mnew)
{
	if(AREG == 0)
	{
		return ectx->set_error_flag(ectx, EFLAG_DMEM);
	}

	AREG = (WORD)palloc_pool(AREG);

	return ECTX_INS_OK;
}
#endif

#if 0 /* no recent compiler ever generates this instruction */
/* 0xE1 - 0x2E 0xF1 - mfree - dynamic release to pool */
TVM_INSTRUCTION (ins_mfree)
{
	pfree_pool(AREG, (void*)BREG);

	STACK_RET(CREG, UNDEFINE(BREG), UNDEFINE(CREG));
}
#endif

/* 0xE2 - 0x2E 0xF2 - malloc - dynamic memory allocation */
TVM_INSTRUCTION (ins_malloc)
{
	WORDPTR ptr = (WORDPTR)NULL_P;
	UWORD size = AREG;
	
	if(size != 0)
	{
		ptr = mt_alloc_data(ectx, MT_SIMPLE | MT_MAKE_TYPE(MT_DATA), size);
	}

	STACK_RET((WORD)ptr, UNDEFINE(BREG), UNDEFINE(CREG));
}

/* 0xE3 - 0x2E 0xF3 - mrelease - dynamic memory release */
TVM_INSTRUCTION (ins_mrelease)
{
	if(AREG == (WORD)NULL_P)
	{
		return ectx->set_error_flag(ectx, EFLAG_DMEM);
	}

	return mt_release_simple(ectx, (WORDPTR)AREG, MT_MAKE_TYPE(MT_DATA));
}

/* 0xE4 - 0x2E 0xF4 - min - mobile input */
TVM_INSTRUCTION (ins_min)
{
	WORDPTR chan_ptr	= (WORDPTR)AREG;
	WORDPTR data_ptr	= (WORDPTR)BREG;

	return chan_swap(ectx, chan_ptr, data_ptr);
}

/* 0xE5 - 0x2E 0xF5 - mout - mobile output */
TVM_INSTRUCTION (ins_mout)
{
	WORDPTR chan_ptr = (WORDPTR)AREG;
	WORDPTR data_ptr = (WORDPTR)BREG;

	return chan_swap(ectx, chan_ptr, data_ptr);
}

/* 0xE6 - 0x2E 0xF6 - min64 - dynamic mobile array input */
TVM_INSTRUCTION (ins_min64)
{
	BYTEPTR data_ptr = (BYTEPTR)BREG;
	WORDPTR chan_ptr = (WORDPTR)AREG;

	return chan_in(ectx, 8, chan_ptr, data_ptr);
}

/* 0xE7 - 0x2E 0xF7 - mout64 - dynamic mobile array output */
TVM_INSTRUCTION (ins_mout64)
{
	BYTEPTR data_ptr = (BYTEPTR)BREG;
	WORDPTR chan_ptr = (WORDPTR)AREG;

	return chan_out(ectx, 8, chan_ptr, data_ptr);
}

/* 0xEA - 0x2E 0xFA - xmin - Extended Mobile Input */
TVM_INSTRUCTION (ins_xmin)
{
	WORDPTR chan_addr = (WORDPTR) AREG;
	WORDPTR data_ptr = (WORDPTR) BREG;
	WORDPTR other_ptr;
	WORDPTR other_WPTR;

	other_WPTR = (WORDPTR) read_word(chan_addr);
	other_ptr = (WORDPTR) WORKSPACE_GET(other_WPTR, WS_CHAN);

	swap_data_word(data_ptr, other_ptr);

	UNDEFINE_STACK_RET();
}

/* 0xEB - 0x2E 0xFB - xmin64 - Extended Dynamic Mobile Array Input */
TVM_INSTRUCTION (ins_xmin64)
{
	/* Push 8 (byte count) onto stack. */
	STACK(8, AREG, BREG);
	/* Do an XIN */
	return ins_xin(ectx);
}

/* 0x65 - 0x26 0xF5 - xminn - Extended multi-dim Dynamic Mobile Array Input */
TVM_INSTRUCTION (ins_xminn)
{
	/* Convert word count to byte count */
	AREG = AREG << WSH;
	/* Do an XIN */
	return ins_xin(ectx);
}

#endif /* TVM_DYNAMIC_MEMORY && TVM_OCCAM_PI */

/****************************************************************************
 *              0x2F 0xF_         0x2F 0xF_         0x2F 0xF_               *
 ****************************************************************************/

/* 0xFD - 0x2F 0xFD - null - put null onto the stack */
TVM_INSTRUCTION (ins_null)
{
	STACK_RET((WORD) NULL_P, AREG, BREG);
}

