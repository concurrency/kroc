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

#include "transputer.h"
#include "instructions.h"
#include "interpreter.h"
#include "scheduler.h"
#include "mem.h"

#include "pool_alloc.h"

#include "ins_chan.h"
#include "ins_fred.h"
#include "ins_mt.h"
#include "dmem.h"

#ifdef __MOBILE_PI_SUPPORT__

/****************************************************************************
 *              0x26 0xF_         0x26 0xF_         0x26 0xF_               *
 ****************************************************************************/

/* 0x11 - 0x21 0xF1 - mreleasep - dynamic process release */
TVM_INSTRUCTION void ins_mreleasep(void)
{
	/* Find the pointer to the allocated block, size comes in from areg */
	BPOOTER ptr = ((BPOOTER)wptr) + (areg * TVM_WORD_LENGTH);
	//printf(">mreleasep\n");
	//printf("  ptr = 0x%08x\n", (WORD) ptr);
	mt_release_simple((POOTER) ptr, MT_MAKE_TYPE(MT_DATA));

	iptr = run_next_on_queue();
	//printf("<mreleasep\n");
}

/****************************************************************************
 *              0x26 0xF_         0x26 0xF_         0x26 0xF_               *
 ****************************************************************************/

/* 0x62 - 0x26 0xF2 - minn - multi dimension mobile array input */
TVM_INSTRUCTION void ins_minn(void)
{
	/* Set up the areg to be in bytes (what in expects) rather than 
	 * words (what minn gets)
	 */
	areg = areg << WSH;

	ins_in();
}

/* 0x64 - 0x26 0xF4 - moutn - multi dimension mobile array output */
TVM_INSTRUCTION void ins_moutn(void)
{
	/* Set up the areg to be in bytes (what out expects) rather than 
	 * words (what moutn gets)
	 */
	areg = areg << WSH;

	ins_out();
}

/* 0xE0 - 0x2E 0xF0 - mnew - dynamic allocation from pool */
TVM_INSTRUCTION void ins_mnew(void)
{
	if(areg == 0)
	{
		set_error_flag(EFLAG_DMEM);
	}

	areg = (WORD)palloc_pool(areg);
}

/* 0xE1 - 0x2E 0xF1 - mfree - dynamic release to pool */
TVM_INSTRUCTION void ins_mfree(void)
{
	pfree_pool(areg, (void*)breg);

	STACK(creg, UNDEFINE(breg), UNDEFINE(creg));
}

/* 0xE2 - 0x2E 0xF2 - malloc - dynamic memory allocation */
TVM_INSTRUCTION void ins_malloc(void)
{
	POOTER ptr = (POOTER)NULL_P;
	UWORD size = areg;
	
	if(size != 0)
	{
		ptr = mt_alloc_data(MT_SIMPLE | MT_MAKE_TYPE(MT_DATA), size);
	}

	STACK((WORD)ptr, UNDEFINE(breg), UNDEFINE(creg));
}

/* 0xE3 - 0x2E 0xF3 - mrelease - dynamic memory release */
TVM_INSTRUCTION void ins_mrelease(void)
{
	if(areg == (WORD)NULL_P)
	{
		set_error_flag(EFLAG_DMEM);
	}

	mt_release_simple((POOTER)areg, MT_MAKE_TYPE(MT_DATA));
	
	UNDEFINE_STACK();
}

/* 0xE4 - 0x2E 0xF4 - min - mobile input */
TVM_INSTRUCTION void ins_min(void)
{
	POOTER chan_ptr	= (POOTER)areg;
	POOTER data_ptr	= (POOTER)breg;

	chan_swap(chan_ptr, data_ptr);
}

/* 0xE5 - 0x2E 0xF5 - mout - mobile output */
TVM_INSTRUCTION void ins_mout(void)
{
	POOTER chan_ptr = (POOTER)areg;
	POOTER data_ptr = (POOTER)breg;

	chan_swap(chan_ptr, data_ptr);
}

/* 0xE6 - 0x2E 0xF6 - min64 - dynamic mobile array input */
TVM_INSTRUCTION void ins_min64(void)
{
	BPOOTER data_ptr = (BPOOTER)breg;
	POOTER chan_ptr = (POOTER)areg;

	chan_in(8, chan_ptr, data_ptr);
}

/* 0xE7 - 0x2E 0xF7 - mout64 - dynamic mobile array output */
TVM_INSTRUCTION void ins_mout64(void)
{
	BPOOTER data_ptr = (BPOOTER)breg;
	POOTER chan_ptr = (POOTER)areg;

	chan_out(8, chan_ptr, data_ptr);
}

/* 0xEA - 0x2E 0xFA - xmin - Extended Mobile Input */
TVM_INSTRUCTION void ins_xmin(void)
{
	POOTER chan_addr = (POOTER) areg;
	POOTER data_ptr = (POOTER) breg;
	POOTER other_ptr;
	POOTER other_wptr;

	other_wptr = (POOTER) read_mem(chan_addr);
	other_ptr = (POOTER) WORKSPACE_GET(other_wptr, WS_CHAN);

	swap_data_word(data_ptr, other_ptr);

	UNDEFINE_STACK();
}

/* 0xEB - 0x2E 0xFB - xmin64 - Extended Dynamic Mobile Array Input */
TVM_INSTRUCTION void ins_xmin64(void)
{
	/* Push 8 (byte count) onto stack. */
	STACK(8, areg, breg);
	/* Do an XIN */
	ins_xin();
}

/* 0x65 - 0x26 0xF5 - xminn - Extended multi-dim Dynamic Mobile Array Input */
TVM_INSTRUCTION void ins_xminn(void)
{
	/* Convert word count to byte count */
	areg = areg << WSH;
	/* Do an XIN */
	ins_xin();
}

#endif /*__MOBILE_PI_SUPPORT__ */

/****************************************************************************
 *              0x2F 0xF_         0x2F 0xF_         0x2F 0xF_               *
 ****************************************************************************/

/* 0xFD - 0x2F 0xFD - null - put null onto the stack I think */
TVM_INSTRUCTION void ins_null(void)
{
	/* FIXME: Is this not just LDC 0 */
	/* FIXME: And if so, should the slinker turn it into that? */
	/* FIXME: Or is there something going on here I havent spotted */
	STACK((WORD) NULL_P, areg, breg);
}


