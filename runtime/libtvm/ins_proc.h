/*
tvm - ins_proc.h
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

#ifndef INS_PROC_H
#define INS_PROC_H

#include "instructions.h"
#include "tvm_types.h"

#ifdef TVM_DYNAMIC_OCCAM_PI
/* 0x22F - 0x22 0x22 0xFF - proc_alloc - allocate process workspace */
TVM_INSTRUCTION_PROTO (ins_proc_alloc);
/* 0x230 - 0x22 0x23 0xF0 - proc_param - pass parameter into workspace */
TVM_INSTRUCTION_PROTO (ins_proc_param);
/* 0x231 - 0x22 0x23 0xF1 - proc_mt_copy - clone mobile type into workspace */
TVM_INSTRUCTION_PROTO (ins_proc_mt_copy);
/* 0x232 - 0x22 0x23 0xF2 - proc_mt_move - move mobile type into workspace */
TVM_INSTRUCTION_PROTO (ins_proc_mt_move);
/* 0x233 - 0x22 0x23 0xF3 - proc_start - start process */
TVM_INSTRUCTION_PROTO (ins_proc_start);
/* 0x234 - 0x22 0x23 0xF4 - proc_end - end process and release workspace */
TVM_INSTRUCTION_PROTO (ins_proc_end);
#endif
#ifdef TVM_OCCAM_PI
/* 0x235 - 0x22 0x23 0xF5 - getaff - get processor affinity */
TVM_INSTRUCTION_PROTO (ins_getaff);
/* 0x236 - 0x22 0x23 0xF6 - setaff - set processor affinity */
TVM_INSTRUCTION_PROTO (ins_setaff);
#endif 
/* 0x237 - 0x22 0x23 0xF7 - getpas - get priority and affinity state */
TVM_INSTRUCTION_PROTO (ins_getpas);

#endif /* INS_PROC_H */
