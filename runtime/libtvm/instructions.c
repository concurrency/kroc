/*
tvm - instructions.c
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
#ifdef TVM_PROFILING
#include "ins_names.h"
#endif

/**
 * This function is placed in the instruction jump table for instructions
 * which have not been implemented, but are otherwise valid.
 */
TVM_INSTRUCTION (ins_not_implemented)
{
	return ECTX_INS_UNSUPPORTED;
}

#ifdef TVM_PROFILING
const char *tvm_instr_pri_name (WORD ins)
{
	if (ins >= 0 && ins <= (sizeof(pri_name) / sizeof(char *))) {
		return pri_name[ins];
	} else {
		return NULL;
	}
}

const char *tvm_instr_sec_name (WORD ins)
{
	if (ins >= 0 && ins <= (sizeof(sec_name) / sizeof(char *))) {
		return sec_name[ins];
	} else {
		return NULL;
	}
}
#endif

