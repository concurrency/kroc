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

#include "transputer.h"
#include "instructions.h"

#include "mem.h"
#include "scheduler.h"

#include "ins_mt.h"
#include "ins_pri.h"
#include "ins_sec.h"

#ifdef __MOBILE_PI_SUPPORT__

/* 0x22F - 0x22 0x22 0xFF - proc_alloc - allocate process workspace */
TVM_INSTRUCTION void ins_proc_alloc(void)
{
	POOTER ws = 0;
	UWORD flags = (UWORD) areg;
	UWORD words = (UWORD) breg;

	if (flags == 0) {
		ws = mt_alloc_data (MT_SIMPLE | MT_MAKE_TYPE (MT_DATA), words << WSH);
	} else {
		/* Don't support any flags at present. */
		set_error_flag (EFLAG_PROC);
	}

	STACK ((WORD) ws, UNDEFINE(breg), UNDEFINE(creg));
}

/* 0x230 - 0x22 0x23 0xF0 - proc_param - pass parameter into workspace */
TVM_INSTRUCTION void ins_proc_param(void)
{
	POOTER ws	= (POOTER) breg;
	UWORD offset	= (UWORD) areg;
	WORD param	= creg;

	write_mem (pooter_plus (ws, offset), param);

	UNDEFINE_STACK ();
}

/* 0x231 - 0x22 0x23 0xF1 - proc_mt_copy - clone mobile type into workspace */
TVM_INSTRUCTION void ins_proc_mt_copy(void)
{
	POOTER ptr	= (POOTER) creg;
	POOTER ws	= (POOTER) breg;
	UWORD offset	= (UWORD) areg;

	if (ptr != (POOTER) NULL_P) {
		ptr = mt_clone (ptr);
	}
	
	write_mem (pooter_plus (ws, offset), (WORD) ptr);

	UNDEFINE_STACK ();
}

/* 0x232 - 0x22 0x23 0xF2 - proc_mt_move - move mobile type into workspace */
TVM_INSTRUCTION void ins_proc_mt_move(void)
{
	POOTER pptr	= (POOTER) creg;
	POOTER ws	= (POOTER) breg;
	UWORD offset	= (UWORD) areg;

	if (pptr != (POOTER) NULL_P) {
		POOTER ptr = (POOTER) read_mem (pptr);

		if (ptr != (POOTER) NULL_P) {
			if (mt_io_update (&ptr)) {
				/* Data moved, delete source reference. */
				write_mem (pptr, (WORD) NULL_P);
			}
		}
	
		write_mem (pooter_plus (ws, offset), (WORD) ptr);
	} else {
		/* Source reference pointer should never be null. */
		set_error_flag (EFLAG_PROC);
	}	

	UNDEFINE_STACK();
}

/* 0x233 - 0x22 0x23 0xF3 - proc_start - start process */
TVM_INSTRUCTION void ins_proc_start(void)
{
	BPOOTER code	= (BPOOTER) creg;
	POOTER ws	= (POOTER) breg;
	UWORD offset	= (UWORD) areg;

	ws = pooter_plus (ws, offset);

	add_to_queue ((WORD) ws, (WORD) code);

	UNDEFINE_STACK ();
}

/* 0x234 - 0x22 0x23 0xF4 - proc_end - end process and release workspace */
TVM_INSTRUCTION void ins_proc_end(void)
{
	POOTER ws = (POOTER) areg;

	mt_release_simple (ws, MT_MAKE_TYPE (MT_DATA));

	iptr = run_next_on_queue ();
}

#endif /* __MOBILE_PI_SUPPORT */


#ifdef __PI_SUPPORT__

/* 0x235 - 0x22 0x23 0xF5 - getaff - get processor affinity */
TVM_INSTRUCTION void ins_getaff(void)
{
	/* Always return an empty (0) affinity mask. */
	STACK (0, areg, breg);
}

/* 0x236 - 0x22 0x23 0xF6 - setaff - set processor affinity */
TVM_INSTRUCTION void ins_setaff(void)
{
	/* Ignore the new affinity mask. */
	STACK (breg, creg, UNDEFINE(creg));
}

#endif /* __PI_SUPPORT__ */

/* 0x237 - 0x22 0x23 0xF7 - getpas - get priority and affinity state */
TVM_INSTRUCTION void ins_getpas(void)
{
	/* Always return an empty (0) affinity mask, with priority 0. */
	STACK (0, areg, breg);
}

