/*
tvm - ins_proc.c
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2004-2008 Christian L. Jacobsen, Matthew C. Jadud, Carl G. Ritson

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

#include "ins_mobile.h"
#include "ins_pri.h"
#include "ins_sec.h"

#ifdef TVM_DYNAMIC_OCCAM_PI

/* 0x22F - 0x22 0x22 0xFF - proc_alloc - allocate process workspace */
TVM_INSTRUCTION (ins_proc_alloc)
{
	WORDPTR ws;
	UWORD flags = (UWORD) AREG;
	UWORD words = (UWORD) BREG;

	if (flags) {
		/* No flags presently supported */
		SET_ERROR_FLAG (EFLAG_PROC);
	}
	
	ws = mt_alloc_data (ectx, MT_SIMPLE | MT_MAKE_TYPE (MT_DATA), words << WSH);

	STACK1_RET ((WORD) ws, STYPE_MT);
}

/* 0x230 - 0x22 0x23 0xF0 - proc_param - pass parameter into workspace */
TVM_INSTRUCTION (ins_proc_param)
{
	WORDPTR ws	= (WORDPTR) BREG;
	UWORD offset	= (UWORD) AREG;
	WORD param	= CREG;

	write_word (wordptr_plus (ws, offset), param);

	UNDEFINE_STACK_RET ();
}

/* 0x231 - 0x22 0x23 0xF1 - proc_mt_copy - clone mobile type into workspace */
TVM_INSTRUCTION (ins_proc_mt_copy)
{
	WORDPTR ptr	= (WORDPTR) CREG;
	WORDPTR ws	= (WORDPTR) BREG;
	UWORD offset	= (UWORD) AREG;

	if (ptr != (WORDPTR) NULL_P) {
		WORDPTR src = ptr;
		int ret;
		if ((ret = mt_clone (ectx, src, &ptr))) {
			return ret;
		}
	}
	
	write_word (wordptr_plus (ws, offset), (WORD) ptr);

	UNDEFINE_STACK_RET ();
}

/* 0x232 - 0x22 0x23 0xF2 - proc_mt_move - move mobile type into workspace */
TVM_INSTRUCTION (ins_proc_mt_move)
{
	WORDPTR pptr	= (WORDPTR) CREG;
	WORDPTR ws	= (WORDPTR) BREG;
	UWORD offset	= (UWORD) AREG;

	if (pptr != (WORDPTR) NULL_P) {
		WORDPTR ptr = (WORDPTR) read_word (pptr);

		if (ptr != (WORDPTR) NULL_P) {
			UWORD move;
			int ret;
			if ((ret = mt_io_update (ectx, &ptr, &move))) {
				return ret;
			}
			if (move == MT_TRUE) {
				/* Data moved, delete source reference. */
				write_word (pptr, (WORD) NULL_P);
			}
		}
	
		write_word (wordptr_plus (ws, offset), (WORD) ptr);
	} else {
		/* Source reference pointer should never be null. */
		SET_ERROR_FLAG (EFLAG_PROC);
	}	

	UNDEFINE_STACK_RET ();
}

/* 0x233 - 0x22 0x23 0xF3 - proc_start - start process */
TVM_INSTRUCTION (ins_proc_start)
{
	BYTEPTR code	= (BYTEPTR) CREG;
	WORDPTR ws	= (WORDPTR) BREG;
	UWORD offset	= (UWORD) AREG;

	ws = wordptr_plus (ws, offset);

	ADD_TO_QUEUE_IPTR (ws, code);

	UNDEFINE_STACK_RET ();
}

/* 0x234 - 0x22 0x23 0xF4 - proc_end - end process and release workspace */
TVM_INSTRUCTION (ins_proc_end)
{
	WORDPTR ws = (WORDPTR) AREG;

	mt_release_simple (ectx, ws, MT_MAKE_TYPE (MT_DATA));

	RUN_NEXT_ON_QUEUE_RET();
}

#endif /* TVM_DYNAMIC__OCCAM_PI */


#ifdef TVM_OCCAM_PI

/* 0x235 - 0x22 0x23 0xF5 - getaff - get processor affinity */
TVM_INSTRUCTION (ins_getaff)
{
	/* Always return an empty (0) affinity mask. */
	STACK_RET (0, AREG, BREG, STYPE_DATA, AREGt, BREGt);
}

/* 0x236 - 0x22 0x23 0xF6 - setaff - set processor affinity */
TVM_INSTRUCTION (ins_setaff)
{
	/* Ignore the new affinity mask. */
	STACK2_RET (BREG, CREG, BREGt, CREGt);
}

#endif /* TVM_OCCAM_PI */

/* 0x237 - 0x22 0x23 0xF7 - getpas - get priority and affinity state */
TVM_INSTRUCTION (ins_getpas)
{
	/* Always return an empty (0) affinity mask, with priority 0. */
	STACK_RET (0, AREG, BREG, STYPE_DATA, AREGt, BREGt);
}

