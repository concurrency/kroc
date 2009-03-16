/*
tvm - ins_pi.h
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

#ifndef INS_PI_H
#define INS_PI_H

#include "instructions.h"
#include "tvm_types.h"

/* 0x23 - 0x22 0xF3 - boolinvert */
TVM_INSTRUCTION_PROTO (ins_boolinvert);
/* 0x24 - 0x22 0xF4 - widenshort */
TVM_INSTRUCTION_PROTO (ins_widenshort);
/* 0x25 - 0x22 0xF5 - fficall */
TVM_INSTRUCTION_PROTO (ins_fficall);
/* 0x26 - 0x22 0xF6 - lend3 - loopend3 (for step in replicators) */
TVM_INSTRUCTION_PROTO (ins_lend3);
/* 0x27 - 0x22 0xF7 - lendb - backwards loopend */
TVM_INSTRUCTION_PROTO (ins_lendb);
/* 0x28 - 0x22 0xF8 - reschedule */
TVM_INSTRUCTION_PROTO (ins_reschedule);

#ifdef TVM_OCCAM_PI
/* 0x14 - 0x21 0xF4 - extvrfy - external channel verify */
TVM_INSTRUCTION_PROTO (ins_extvrfy);
/* 0x60 - 0x26 0xF0 - extin - external channel input */
TVM_INSTRUCTION_PROTO (ins_extin);
/* 0x61 - 0x26 0xF1 - extout - external channel output */
TVM_INSTRUCTION_PROTO (ins_extout);
#ifdef TVM_DYNAMIC_OCCAM_PI
/* 0x24B - 0x22 0x24 0xFB - ext_mt_in - external channel mobile type input */
TVM_INSTRUCTION_PROTO (ins_ext_mt_out);
/* 0x24C - 0x22 0x24 0xFC - ext_mt_out - external channel mobile type output */
TVM_INSTRUCTION_PROTO (ins_ext_mt_out);
#endif /* TVM_DYNAMIC_OCCAM_PI */
#endif /* TVM_OCCAM_PI */

/* 0xA2 - 0x2A 0xF2 - getpri - get priority */
TVM_INSTRUCTION_PROTO (ins_getpri);
/* 0xA5 - 0x2A 0xF5 - setpri - set priority */
TVM_INSTRUCTION_PROTO (ins_setpri);
/* 0xAD - 0x2A 0xFD - ins_savecreg - save the creg *magic* :) */
TVM_INSTRUCTION_PROTO (ins_savecreg);
/* 0xAE - 0x2A 0xFE - ins_restorecreg - restore the creg *magic :) */
TVM_INSTRUCTION_PROTO (ins_restorecreg);

#ifdef TVM_OCCAM_PI

/****************************************************************************
 *              0x27 0xF_         0x27 0xF_         0x27 0xF_               *
 ****************************************************************************/

TVM_HELPER_PROTO void tvm_sem_init(WORDPTR sem);
TVM_HELPER_PROTO int tvm_sem_claim(tvm_ectx_t *ectx, WORDPTR sem);
TVM_HELPER_PROTO int tvm_sem_release(tvm_ectx_t *ectx, WORDPTR sem);

/* 0x7A - 0x27 0xFA - seminit - initialise semaphore */
TVM_INSTRUCTION_PROTO (ins_sem_init);
/* 0x7B - 0x27 0xFB - semclaim - claim semaphore */
TVM_INSTRUCTION_PROTO (ins_sem_claim);
/* 0x7C - 0x27 0xFC - semrelease - release semaphore */
TVM_INSTRUCTION_PROTO (ins_sem_release);

/****************************************************************************
 *              0x2E 0xF_         0x2E 0xF_         0x2E 0xF_               *
 ****************************************************************************/

/* 0xE0 - 0x2E 0xF0 - checknotnull - Check Pointer Not NULL */
TVM_INSTRUCTION_PROTO (ins_checknotnull);
/* 0xE8 - 0x2E 0xF8 - xable - Extended Input Enable */
TVM_INSTRUCTION_PROTO (ins_xable);
/* 0xE9 - 0x2E 0xF9 - xin - Extended Input */
TVM_INSTRUCTION_PROTO (ins_xin);
/* 0xEC - 0x2E 0xFC - xend - Extended Input End */
TVM_INSTRUCTION_PROTO (ins_xend);

#endif /* TVM_OCCAM_PI */

/****************************************************************************
 *              0x2F 0xF_         0x2F 0xF_         0x2F 0xF_               *
 ****************************************************************************/

/* 0xFD - 0x2F 0xFD - null - put null onto the stack I think */
TVM_INSTRUCTION_PROTO (ins_null);

#endif /* INS_PI_H */
