/*
tvm - ins_float.h
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

#ifndef INS_FLOAT_H
#define INS_FLOAT_H

#include "instructions.h"

#if defined(TVM_EMULATE_T4) || defined(TVM_EMULATE_T8)
/* T4 only floating point instructions */
/* 0x63 - 0x26 0xF3 - unpacksn - unpack single len floating point number */
TVM_INSTRUCTION_PROTO void ins_unpacksn(void);
/* 0x6C - 0x26 0xFC - postnormsn - ??? */
TVM_INSTRUCTION_PROTO void ins_postnormsn(void);
/* 0x6D - 0x26 0xFD - roundsn - ??? */
TVM_INSTRUCTION_PROTO void ins_roundsn(void);
/* 0x71 - 0x27 0xF1 - ldinf - load single length floating point infinity */
TVM_INSTRUCTION_PROTO void ins_ldinf(void);
/* 0x72 - 0x27 0xF2 - fmul - fractional multiply */
TVM_INSTRUCTION_PROTO void ins_fmul(void);
#endif /* defined(TVM_EMULATE_T4) || defined(TVM_EMULATE_T8) */

#ifdef TVM_EMULATE_T8
/* 0x73 - 0x27 0xF3 - cflerr - check single length floating point infinity or NaN 
   this isn't implemented as it doesn't seem to be used anywhere*/

/* 0x28 and up */
/* 0x82 - 0x28 0xF2 - fpldnldbi - floating load non-local indexed double */
TVM_INSTRUCTION_PROTO void ins_fpldnldbi(void);

/* 0x83 - 0x28 0xF3 - fpcheckerr - check floating error */
TVM_INSTRUCTION_PROTO void ins_fpchkerr(void);

/* 0x84 - 0x28 0xF4 - fpstnldb - floating point store non-local double */
TVM_INSTRUCTION_PROTO void ins_fpstnldb(void);

/* 0x86 - 0x28 0xF6 - fpldnlsni - floating load non local indexed single */
TVM_INSTRUCTION_PROTO void ins_fpldnlsni(void);

/* 0x87 - 0x28 0xF7 - fpadd - floating point add */
TVM_INSTRUCTION_PROTO void ins_fpadd(void);

/* 0x88 - 0x28 0xF8 - fpstnlsn - floating store non local single */
TVM_INSTRUCTION_PROTO void ins_fpstnlsn(void);

/* 0x89 - 0x28 0xF9 - fpsub - floating point subtract */
TVM_INSTRUCTION_PROTO void ins_fpsub(void);

/* 0x8A - 0x28 0xFA - fpldnldb - floating point load non-local double */
TVM_INSTRUCTION_PROTO void ins_fpldnldb(void);

/* 0x8B - 0x28 0xFB - fpmul - floating point multiply */
TVM_INSTRUCTION_PROTO void ins_fpmul(void);

/* 0x8C - 0x28 0xFC - fpdiv - floating point divide */
TVM_INSTRUCTION_PROTO void ins_fpdiv(void);

/* 0x8E - 0x28 0xFE - fpldnlsn - floating point load non local single */
TVM_INSTRUCTION_PROTO void ins_fpldnlsn(void);

/* 0x29 and up */
/* 0x91 - 0x29 0xF1 - fpnan - floating point not a number */
TVM_INSTRUCTION_PROTO void ins_fpnan (void);

/* 0x92 - 0x29 0xF2 - fpordered - floating point ordered */
TVM_INSTRUCTION_PROTO void ins_fpordered (void);

/* 0x93 - 0x29 0xF3 - fpnotfinite - floating point not finite */
TVM_INSTRUCTION_PROTO void ins_fpnotfinite (void);

/* 0x94 - 0x29 0xF4 - fpgt - floating point equals */
TVM_INSTRUCTION_PROTO void ins_fpgt (void);

/* 0x95 - 0x29 0xF5 - fpeq - floating point equals */
TVM_INSTRUCTION_PROTO void ins_fpeq (void);

/* 0x96 - 0x29 0xF6 - fpi32tor32 load int32 as real32 */
TVM_INSTRUCTION_PROTO void ins_fpi32tor32(void);

/* 0x98 - 0x29 0xF8 - fpi32tor64 load int32 as real64 */
TVM_INSTRUCTION_PROTO void ins_fpi32tor64(void);

/* 0x9A - 0x29 0xFA - fpb32tor64 - load unsigned word as real64 */
TVM_INSTRUCTION_PROTO void ins_fpb32tor64(void);

/* 0x9D - 0x29 0xFD - fprtoi32 - real to int32 */
TVM_INSTRUCTION_PROTO void ins_fprtoi32 (void);

/* 0x9E - 0x29 0xFE - fpstnli32 - store non local int32 */
TVM_INSTRUCTION_PROTO void ins_fpstnli32 (void);

/* 0x29 0xFF - fpldzerosn - floating load zero single */
TVM_INSTRUCTION_PROTO void ins_fpldzerosn(void);

/* 0x2A and up */
/* 0xA0  - 0x2A 0xF0 - fpldzerodb - floating load zero double */
TVM_INSTRUCTION_PROTO void ins_fpldzerodb(void);

/* 0xA1  - 0x2A 0xF1 - fpint - round to floating integer */
TVM_INSTRUCTION_PROTO void ins_fpint(void);

/* 0xA3 - 0x2A 0xF3 - fpdup - floating point duplicate */
TVM_INSTRUCTION_PROTO void ins_fpdup(void);

/* 0xA4 - 0x2A 0xF4 - fprev - floating reverse */
TVM_INSTRUCTION_PROTO void ins_fprev(void);

/* 0xA6  - 0x2A 0xF6 - fpldnladddb - floating load non local and add double */
TVM_INSTRUCTION_PROTO void ins_fpldnladddb(void);

/* 0xA8  - 0x2A 0xF8 - fpldnlmuldb - floating load non local and multiply double */
TVM_INSTRUCTION_PROTO void ins_fpldnlmuldb(void);

/* 0xAA  - 0x2A 0xFA - fpldnladdsn - floating load non local and add single */
TVM_INSTRUCTION_PROTO void ins_fpldnladdsn(void);

/* 0xAC - 0x2A 0xFC - fpldnlmulsn - floating load non local and multiply single */
TVM_INSTRUCTION_PROTO void ins_fpldnlmulsn(void);

/*  0x2C and up  */
/* 0xCF - 0x2C 0xFF - fprem - floating point remainder */
TVM_INSTRUCTION_PROTO void ins_fprem(void);

/*  0x2D and up  */
/* 0xD0 - 0x2D 0xF0 - i64toreal - 64bit into to real (converted from special .I64TOREAL) */
TVM_INSTRUCTION_PROTO void ins_i64toreal(void);

/* 0xD1  - 0x2D 0xF1 - fpdivby2 - floating point divide by 2 */
TVM_INSTRUCTION_PROTO void ins_fpdivby2(void);

/* 0xD2  - 0x2D 0xF2 - fpmulby2 - floating point multiply by 2 */
TVM_INSTRUCTION_PROTO void ins_fpmulby2(void);

/* 0xD3  - 0x2D 0xF3 - fpsqrt - floating point square root */
TVM_INSTRUCTION_PROTO void ins_fpsqrt(void);

/* 0xD6  - 0x2D 0xF6 - fprz - floating point rounding mode to zero */
TVM_INSTRUCTION_PROTO void ins_fprz(void);

/* 0x07 - 0x20 0xF7 - fpr32tor64 - real32 to real64 */
TVM_INSTRUCTION_PROTO void ins_fpr32to64(void);

/* 0x08 - 0x20 0xF8 - fpr64tor32 - real64 to real32 */
TVM_INSTRUCTION_PROTO void ins_fpr64to32(void);

/* 0xD9  - 0x2D 0xF9 - fpexpdec32 - floating divide by 2^32 */
TVM_INSTRUCTION_PROTO void ins_fpexpdec32(void);

/* 0xDB - 0x2D 0xFB - fpabs - floating point absolute value */
TVM_INSTRUCTION_PROTO void ins_fpabs(void);

/* 0xDF  - 0x2D 0xFF - fpchki64 -  check that value at top of FP stack (fareg) fits in an INT32/INT64 */
TVM_INSTRUCTION_PROTO void ins_fpchki64(void);

#endif /* TVM_EMULATE_T8 */

#endif /* !INS_FLOAT_H */
