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
TVM_INSTRUCTION_PROTO (ins_unpacksn);
/* 0x6C - 0x26 0xFC - postnormsn - ??? */
TVM_INSTRUCTION_PROTO (ins_postnormsn);
/* 0x6D - 0x26 0xFD - roundsn - ??? */
TVM_INSTRUCTION_PROTO (ins_roundsn);
/* 0x71 - 0x27 0xF1 - ldinf - load single length floating point infinity */
TVM_INSTRUCTION_PROTO (ins_ldinf);
/* 0x72 - 0x27 0xF2 - fmul - fractional multiply */
TVM_INSTRUCTION_PROTO (ins_fmul);
#endif /* defined(TVM_EMULATE_T4) || defined(TVM_EMULATE_T8) */

#ifdef TVM_EMULATE_T8
/* 0x73 - 0x27 0xF3 - cflerr - check single length floating point infinity or NaN 
   this isn't implemented as it doesn't seem to be used anywhere*/

/* 0x28 and up */
/* 0x82 - 0x28 0xF2 - fpldnldbi - floating load non-local indexed double */
TVM_INSTRUCTION_PROTO (ins_fpldnldbi);

/* 0x83 - 0x28 0xF3 - fpcheckerr - check floating error */
TVM_INSTRUCTION_PROTO (ins_fpchkerr);

/* 0x84 - 0x28 0xF4 - fpstnldb - floating point store non-local double */
TVM_INSTRUCTION_PROTO (ins_fpstnldb);

/* 0x86 - 0x28 0xF6 - fpldnlsni - floating load non local indexed single */
TVM_INSTRUCTION_PROTO (ins_fpldnlsni);

/* 0x87 - 0x28 0xF7 - fpadd - floating point add */
TVM_INSTRUCTION_PROTO (ins_fpadd);

/* 0x88 - 0x28 0xF8 - fpstnlsn - floating store non local single */
TVM_INSTRUCTION_PROTO (ins_fpstnlsn);

/* 0x89 - 0x28 0xF9 - fpsub - floating point subtract */
TVM_INSTRUCTION_PROTO (ins_fpsub);

/* 0x8A - 0x28 0xFA - fpldnldb - floating point load non-local double */
TVM_INSTRUCTION_PROTO (ins_fpldnldb);

/* 0x8B - 0x28 0xFB - fpmul - floating point multiply */
TVM_INSTRUCTION_PROTO (ins_fpmul);

/* 0x8C - 0x28 0xFC - fpdiv - floating point divide */
TVM_INSTRUCTION_PROTO (ins_fpdiv);

/* 0x8E - 0x28 0xFE - fpldnlsn - floating point load non local single */
TVM_INSTRUCTION_PROTO (ins_fpldnlsn);

/* 0x29 and up */
/* 0x91 - 0x29 0xF1 - fpnan - floating point not a number */
TVM_INSTRUCTION_PROTO (ins_fpnan);

/* 0x92 - 0x29 0xF2 - fpordered - floating point ordered */
TVM_INSTRUCTION_PROTO (ins_fpordered);

/* 0x93 - 0x29 0xF3 - fpnotfinite - floating point not finite */
TVM_INSTRUCTION_PROTO (ins_fpnotfinite);

/* 0x94 - 0x29 0xF4 - fpgt - floating point equals */
TVM_INSTRUCTION_PROTO (ins_fpgt);

/* 0x95 - 0x29 0xF5 - fpeq - floating point equals */
TVM_INSTRUCTION_PROTO (ins_fpeq);

/* 0x96 - 0x29 0xF6 - fpi32tor32 load int32 as real32 */
TVM_INSTRUCTION_PROTO (ins_fpi32tor32);

/* 0x98 - 0x29 0xF8 - fpi32tor64 load int32 as real64 */
TVM_INSTRUCTION_PROTO (ins_fpi32tor64);

/* 0x9A - 0x29 0xFA - fpb32tor64 - load unsigned word as real64 */
TVM_INSTRUCTION_PROTO (ins_fpb32tor64);

/* 0x9D - 0x29 0xFD - fprtoi32 - real to int32 */
TVM_INSTRUCTION_PROTO (ins_fprtoi32);

/* 0x9E - 0x29 0xFE - fpstnli32 - store non local int32 */
TVM_INSTRUCTION_PROTO (ins_fpstnli32);

/* 0x29 0xFF - fpldzerosn - floating load zero single */
TVM_INSTRUCTION_PROTO (ins_fpldzerosn);

/* 0x2A and up */
/* 0xA0  - 0x2A 0xF0 - fpldzerodb - floating load zero double */
TVM_INSTRUCTION_PROTO (ins_fpldzerodb);

/* 0xA1  - 0x2A 0xF1 - fpint - round to floating integer */
TVM_INSTRUCTION_PROTO (ins_fpint);

/* 0xA3 - 0x2A 0xF3 - fpdup - floating point duplicate */
TVM_INSTRUCTION_PROTO (ins_fpdup);

/* 0xA4 - 0x2A 0xF4 - fprev - floating reverse */
TVM_INSTRUCTION_PROTO (ins_fprev);

/* 0xA6  - 0x2A 0xF6 - fpldnladddb - floating load non local and add double */
TVM_INSTRUCTION_PROTO (ins_fpldnladddb);

/* 0xA8  - 0x2A 0xF8 - fpldnlmuldb - floating load non local and multiply double */
TVM_INSTRUCTION_PROTO (ins_fpldnlmuldb);

/* 0xAA  - 0x2A 0xFA - fpldnladdsn - floating load non local and add single */
TVM_INSTRUCTION_PROTO (ins_fpldnladdsn);

/* 0xAC - 0x2A 0xFC - fpldnlmulsn - floating load non local and multiply single */
TVM_INSTRUCTION_PROTO (ins_fpldnlmulsn);

/*  0x2C and up  */
/* 0xCF - 0x2C 0xFF - fprem - floating point remainder */
TVM_INSTRUCTION_PROTO (ins_fprem);

/*  0x2D and up  */
/* 0xD0 - 0x2D 0xF0 - i64toreal - 64bit into to real (converted from special .I64TOREAL) */
TVM_INSTRUCTION_PROTO (ins_i64toreal);

/* 0xD1  - 0x2D 0xF1 - fpdivby2 - floating point divide by 2 */
TVM_INSTRUCTION_PROTO (ins_fpdivby2);

/* 0xD2  - 0x2D 0xF2 - fpmulby2 - floating point multiply by 2 */
TVM_INSTRUCTION_PROTO (ins_fpmulby2);

/* 0xD3  - 0x2D 0xF3 - fpsqrt - floating point square root */
TVM_INSTRUCTION_PROTO (ins_fpsqrt);

/* 0xD6  - 0x2D 0xF6 - fprz - floating point rounding mode to zero */
TVM_INSTRUCTION_PROTO (ins_fprz);

/* 0x07 - 0x20 0xF7 - fpr32tor64 - real32 to real64 */
TVM_INSTRUCTION_PROTO (ins_fpr32to64);

/* 0x08 - 0x20 0xF8 - fpr64tor32 - real64 to real32 */
TVM_INSTRUCTION_PROTO (ins_fpr64to32);

/* 0xD9  - 0x2D 0xF9 - fpexpdec32 - floating divide by 2^32 */
TVM_INSTRUCTION_PROTO (ins_fpexpdec32);

/* 0xDB - 0x2D 0xFB - fpabs - floating point absolute value */
TVM_INSTRUCTION_PROTO (ins_fpabs);

/* 0xDF  - 0x2D 0xFF - fpchki64 -  check that value at top of FP stack (fareg) fits in an INT32/INT64 */
TVM_INSTRUCTION_PROTO (ins_fpchki64);

#endif /* TVM_EMULATE_T8 */

#endif /* !INS_FLOAT_H */
