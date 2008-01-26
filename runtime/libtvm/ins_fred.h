/*
tvm - ins_fred.h
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

#ifndef INS_FRED_H
#define INS_FRED_H

#include "instructions.h"
#include "tvm_types.h"

/* 0x23 - 0x22 0xF2 - boolinvert */
TVM_INSTRUCTION_PROTO void ins_boolinvert(void);
/* 0x24 - 0x22 0xF4 - boolinvert */
TVM_INSTRUCTION_PROTO void ins_widenshort(void);
/* 0x25 - 0x22 0xF5 - fficall */
TVM_INSTRUCTION_PROTO void ins_fficall(void);
/* 0x26 - 0x22 0xF6 - lend3 - loopend3 (for step in replicators) */
TVM_INSTRUCTION_PROTO void ins_lend3(void);
/* 0x27 - 0x22 0xF7 - lendbw - backwards loopend */
TVM_INSTRUCTION_PROTO void ins_lendbw(void);
/* 0x23 - 0x22 0xF3 - reschedule */
TVM_INSTRUCTION_PROTO void ins_reschedule(void);

#ifdef TVM_OCCAM_PI
/* 0x14 - 0x21 0xF4 - extvrfy - external channel verify */
TVM_INSTRUCTION_PROTO void ins_extvrfy(void);
/* 0x60 - 0x26 0xF0 - extin - external channel input */
TVM_INSTRUCTION_PROTO void ins_extin(void);
/* 0x61 - 0x26 0xF1 - extout - external channel output */
TVM_INSTRUCTION_PROTO void ins_extout(void);
#endif

/* 0xA2 - 0x2A 0xF2 - getpri - get priority */
TVM_INSTRUCTION_PROTO void ins_getpri(void);
/* 0xA5 - 0x2A 0xF5 - setpri - set priority */
TVM_INSTRUCTION_PROTO void ins_setpri(void);
/* 0xAD - 0x2A 0xFD - ins_savecreg - save the creg *magic* :) */
TVM_INSTRUCTION_PROTO void ins_savecreg(void);
/* 0xAE - 0x2A 0xFE - ins_restorecreg - restore the creg *magic :) */
TVM_INSTRUCTION_PROTO void ins_restorecreg(void);

#ifdef TVM_OCCAM_PI

/****************************************************************************
 *              0x27 0xF_         0x27 0xF_         0x27 0xF_               *
 ****************************************************************************/

TVM_HELPER_PROTO void tvm_sem_init(WORDPTR sem);
TVM_HELPER_PROTO void tvm_sem_claim(WORDPTR sem);
TVM_HELPER_PROTO void tvm_sem_release(WORDPTR sem);

/* 0x7A - 0x27 0xFA - seminit - initialise semaphore */
TVM_INSTRUCTION_PROTO void ins_seminit(void);
/* 0x7B - 0x27 0xFB - semclaim - claim semaphore */
TVM_INSTRUCTION_PROTO void ins_semclaim(void);
/* 0x7C - 0x27 0xFC - semrelease - release semaphore */
TVM_INSTRUCTION_PROTO void ins_semrelease(void);

/****************************************************************************
 *              0x28 0xF_         0x28 0xF_         0x28 0xF_               *
 ****************************************************************************/

/* 0xE8 - 0x2E 0xF8 - xable - Extended Input Enable */
TVM_INSTRUCTION_PROTO void ins_xable(void);
/* 0xE9 - 0x2E 0xF9 - xin - Extended Input */
TVM_INSTRUCTION_PROTO void ins_xin(void);
/* 0xEC - 0x2E 0xFC - xend - Extended Input End */
TVM_INSTRUCTION_PROTO void ins_xend(void);

#endif /* TVM_OCCAM_PI */

#endif /* INS_FRED_H */
