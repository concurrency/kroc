/*
tvm - dmem.h
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

#ifndef DMEM_H
#define DMEM_H

#include "instructions.h"

#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)

/****************************************************************************
 *              0x26 0xF_         0x26 0xF_         0x26 0xF_               *
 ****************************************************************************/

/* 0x11 - 0x21 0xF1 - mreleasep - dynamic process release */
TVM_INSTRUCTION_PROTO (ins_mreleasep);

/****************************************************************************
 *              0x26 0xF_         0x26 0xF_         0x26 0xF_               *
 ****************************************************************************/

/* 0xE0 - 0x26 0xF2 - minn - multi dimension mobile array input */
TVM_INSTRUCTION_PROTO (ins_minn);
/* 0xE0 - 0x26 0xF4 - moutn - multi dimension mobile array output */
TVM_INSTRUCTION_PROTO (ins_moutn);

/****************************************************************************
 *              0x2E 0xF_         0x2E 0xF_         0x2E 0xF_               *
 ****************************************************************************/

#if 0
/* 0xE0 - 0x2E 0xF0 - mnew - dynamic allocation from pool */
TVM_INSTRUCTION_PROTO (ins_mnew);
/* 0xE1 - 0x2E 0xF1 - mfree - dynamic release to  pool */
TVM_INSTRUCTION_PROTO (ins_mfree);
#endif
/* 0xE2 - 0x2E 0xF2 - malloc - dynamic memory allocation */
TVM_INSTRUCTION_PROTO (ins_malloc);
/* 0xE3 - 0x2E 0xF3 - mrelease - dynamic memory release */
TVM_INSTRUCTION_PROTO (ins_mrelease);
/* 0xE4 - 0x2E 0xF4 - min - mobile input */
TVM_INSTRUCTION_PROTO (ins_min);
/* 0xE5 - 0x2E 0xF5 - mout - mobile output */
TVM_INSTRUCTION_PROTO (ins_mout);
/* 0xE6 - 0x2E 0xF6 - min64 - dynamic mobile array input */
TVM_INSTRUCTION_PROTO (ins_min64);
/* 0xE7 - 0x2E 0xF7 - mout64 - dynamic mobile array output */
TVM_INSTRUCTION_PROTO (ins_mout64);
/* 0xEA - 0x2E 0xFA - xmin - Extended Mobile Input */
TVM_INSTRUCTION_PROTO (ins_xmin);
/* 0xEB - 0x2E 0xFB - xmin64 - Extended Dynamic Mobile Array Input */
TVM_INSTRUCTION_PROTO (ins_xmin64);
/* 0x65 - 0x26 0xF5 - xminn - Extended multi-dim Dynamic Mobile Array Input */
TVM_INSTRUCTION_PROTO (ins_xminn);

#endif /* TVM_DYNAMIC_MEMORY && TVM_OCCAM_PI */

/****************************************************************************
 *              0x2F 0xF_         0x2F 0xF_         0x2F 0xF_               *
 ****************************************************************************/

/* 0xFD - 0x2F 0xFD - null - put null onto the stack I think */
TVM_INSTRUCTION_PROTO (ins_null);

#endif
