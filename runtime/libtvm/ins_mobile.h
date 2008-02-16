/*
tvm - ins_mobile.h
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

#ifndef INS_MOBILE_H
#define INS_MOBILE_H

#include "instructions.h"
#include "tvm_types.h"

#ifdef TVM_DYNAMIC_OCCAM_PI

#define MT_DEFINES 1
#define MT_TVM 1
#define MT_FALSE 0
#define MT_TRUE 1
#include "mobile_types.h"

TVM_HELPER_PROTO WORDPTR mt_alloc_data(tvm_ectx_t *ectx, UWORD type, UWORD size);
TVM_HELPER_PROTO int mt_alloc(tvm_ectx_t *ectx, UWORD type, UWORD size, WORDPTR *ret);
TVM_HELPER_PROTO int mt_release_simple(tvm_ectx_t *ectx, WORDPTR ptr, UWORD type);
TVM_HELPER_PROTO int mt_release(tvm_ectx_t *ectx, WORDPTR ptr);
TVM_HELPER_PROTO int mt_clone(tvm_ectx_t *ectx, WORDPTR ptr, WORDPTR *ret);
TVM_HELPER_PROTO int mt_io_update(tvm_ectx_t *ectx, WORDPTR *ptr, UWORD *move);
TVM_HELPER_PROTO int mt_chan_io(tvm_ectx_t *ectx, WORDPTR dst, WORDPTR src);

/* 0x11 - 0x21 0xF1 - mreleasep - dynamic process release */
TVM_INSTRUCTION_PROTO (ins_mreleasep);

/* 0xE2 - 0x2E 0xF2 - malloc - dynamic memory allocation */
TVM_INSTRUCTION_PROTO (ins_malloc);
/* 0xE3 - 0x2E 0xF3 - mrelease - dynamic memory release */
TVM_INSTRUCTION_PROTO (ins_mrelease);

/* 0x238 - 0x22 0x23 0xF8 - mt_alloc - allocate a mobile type */
TVM_INSTRUCTION_PROTO (ins_mt_alloc);
/* 0x239 - 0x22 0x23 0xF9 - mt_release - release a mobile type */
TVM_INSTRUCTION_PROTO (ins_mt_release);
/* 0x23A - 0x22 0x23 0xFA - mt_clone - clone a mobile type */
TVM_INSTRUCTION_PROTO (ins_mt_clone);
/* 0x23B - 0x22 0x23 0xFB - mt_in - mobile type channel input */
TVM_INSTRUCTION_PROTO (ins_mt_in);
/* 0x23C - 0x22 0x23 0xFC - mt_out - mobile type channel output */
TVM_INSTRUCTION_PROTO (ins_mt_out);
/* 0x23D - 0x22 0x23 0xFD - mt_xchg - mobile type channel exchange */
TVM_INSTRUCTION_PROTO (ins_mt_xchg);
/* 0x23E - 0x22 0x23 0xFE - mt_lock - lock a mobile type */
TVM_INSTRUCTION_PROTO (ins_mt_lock);
/* 0x23F - 0x22 0x23 0xFF - mt_unlock - unlock a mobile type */
TVM_INSTRUCTION_PROTO (ins_mt_unlock);
/* 0x240 - 0x22 0x24 0xF0 - mt_enroll - enroll processes on a mobile type */
TVM_INSTRUCTION_PROTO (ins_mt_enroll);
/* 0x241 - 0x22 0x24 0xF1 - mt_resign - resign process from a mobile type */
TVM_INSTRUCTION_PROTO (ins_mt_resign);
/* 0x242 - 0x22 0x24 0xF2 - mt_sync - synchronise on a mobile type */
TVM_INSTRUCTION_PROTO (ins_mt_sync);
/* 0x243 - 0x22 0x24 0xF3 - mt_xin - mobile type channel extended input */
TVM_INSTRUCTION_PROTO (ins_mt_xin);
/* 0x244 - 0x22 0x24 0xF4 - mt_xout - mobile type channel extended output */
TVM_INSTRUCTION_PROTO (ins_mt_xout);
/* 0x245 - 0x22 0x24 0xF5 - mt_xxchg - mobile type channel extended exchange */
TVM_INSTRUCTION_PROTO (ins_mt_xxchg);
/* 0x246 - 0x22 0x24 0xF6 - mt_dclone - clone data into a mobile type */
TVM_INSTRUCTION_PROTO (ins_mt_dclone);
/* 0x247 - 0x22 0x24 0xF7 - mt_bind - bind a mobile type in some way to a bit of data */
TVM_INSTRUCTION_PROTO (ins_mt_bind);

#endif /* TVM_DYNAMIC_OCCAM_PI */

#endif /* INS_MOBILE_H */
