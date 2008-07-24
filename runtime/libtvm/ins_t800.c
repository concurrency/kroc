/*
tvm - ins_t800.c
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
#include "ins_t800.h"

/* This file contains only instuctions which are non-floating point related but
 * required on the T800 
*/

/****************************************************************************
 *              0x25 0xF_         0x25 0xF_         0x25 0xF_               *
 ****************************************************************************/
/* 0x5A - 0x25 0xFA - dup - duplicate top of stack */
TVM_INSTRUCTION (ins_dup)
{
	STACK_RET(AREG, AREG, BREG, AREGt, AREGt, BREGt);
}

/****************************************************************************
 *              0x28 0xF_         0x28 0xF_         0x28 0xF_               *
 ****************************************************************************/
/* 0x81 - 0x28 0xF1 - wsubdb - double word subscript */
TVM_INSTRUCTION (ins_wsubdb)
{
	/* T800 provides a double word subscript instruct to save on doing
	 * a shift followed by a WSUB or BSUB when accessing double word
	 * array items.
	 */
	STACK2_RET((WORD)wordptr_plus((WORDPTR)AREG, BREG * 2), CREG, AREGt, CREGt);
}

