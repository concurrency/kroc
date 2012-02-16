/*
tvm - ins_sec.h
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

#ifndef INS_SEC_H
#define INS_SEC_H

/* FIXME: INS_NULL, THIS DOES NOT APPEAR TO BE A REAL INSTRUCTION, BUT
 * IS PRESENT IN SOCCAM, CHECK IF THIS IS REALLY AN ETC SPECIAL WHICH 
 * OUGHT TO BE RESOLVED INTO A REAL INSTRUCTION IN THE RESOLVER/LINKER
 * void ins_null(void);
 */

/****************************************************************************
 *                   0xF_              0xF_              0xF_               *
 ****************************************************************************/

/* 0x00 - 0xF0 - rev - reverse */
TVM_INSTRUCTION_PROTO (ins_rev);
/* 0x01 - 0xF1 - lb - load byte */
TVM_INSTRUCTION_PROTO (ins_lb);
/* 0x02 - 0xF2 - bsub - byte subscript */
TVM_INSTRUCTION_PROTO (ins_bsub);
/* 0x03 - 0xF3 - endp - end process */
TVM_INSTRUCTION_PROTO (ins_endp);
/* 0x04 - 0xF4 - diff - difference */
TVM_INSTRUCTION_PROTO (ins_diff);
/* 0x05 - 0xF5 - add - addition */
TVM_INSTRUCTION_PROTO (ins_add);
/* 0x06 - 0xF6 - gcall - general call */
TVM_INSTRUCTION_PROTO (ins_gcall);
/* 0x08 - 0xF8 - prod - Unchecked Multiplication (product) */
TVM_INSTRUCTION_PROTO (ins_prod);
/* 0x09 - 0xF9 - gt - greater than */
TVM_INSTRUCTION_PROTO (ins_gt);
/* 0x0A - 0xFA - wsub - word subscript */
TVM_INSTRUCTION_PROTO (ins_wsub);
/* 0x0C - 0xFC - sub - subtract */
TVM_INSTRUCTION_PROTO (ins_sub);
/* 0x0D - 0xFD - startp - star process */
TVM_INSTRUCTION_PROTO (ins_startp);





/****************************************************************************
 *              0x21 0xF_         0x21 0xF_         0x21 0xF_               *
 ****************************************************************************/

/* 0x10 - 0x21 0xF0 - seterr - set error */
TVM_INSTRUCTION_PROTO (ins_seterr);
/* 0x13 - 0x21 0xF3 - csub0 - check subscript from 0 */
TVM_INSTRUCTION_PROTO (ins_csub0);
/* 0x15 - 0x21 0xF5 - stopp - stop process */
TVM_INSTRUCTION_PROTO (ins_stopp);
/* 0x16 - 0x21 0xF6 - ladd - long addition */
TVM_INSTRUCTION_PROTO (ins_ladd);
/* 0x19 - 0x21 0xF9 - norm - normalise */
TVM_INSTRUCTION_PROTO (ins_norm);
/* 0x1A - 0x21 0xFA - ldiv - divides a double length value in the
 *  * reg pair CB by a single length value in A */
TVM_INSTRUCTION_PROTO (ins_ldiv);
/* 0x1B - 0x21 0x1B - ldpi - load pointer to instruction */
TVM_INSTRUCTION_PROTO (ins_ldpi);
/* 0x1D - 0x21 0xFD - xdble - extend to double */
TVM_INSTRUCTION_PROTO (ins_xdble);
/* 0x1F - 0x21 0xFF - rem - (integer) remainder */
TVM_INSTRUCTION_PROTO (ins_rem);





/****************************************************************************
 *              0x22 0xF_         0x22 0xF_         0x22 0xF_               *
 ****************************************************************************/

/* 0x20 - 0x22 0xF0 - ret - return */
TVM_INSTRUCTION_PROTO (ins_ret);
/* 0x21 - 0x22 0xF1 - lend - loop end */
TVM_INSTRUCTION_PROTO (ins_lend);
/* 0x2C - 0x22 0xFC - div - divide */
TVM_INSTRUCTION_PROTO (ins_div);





/****************************************************************************
 *              0x23 0xF_         0x23 0xF_         0x23 0xF_               *
 ****************************************************************************/
/* 0x31 - 0x23 0xF1 - lmul - long multiply */
TVM_INSTRUCTION_PROTO (ins_lmul);
/* 0x32 - 0x23 0xF2 - not - bitwise complement */
TVM_INSTRUCTION_PROTO (ins_not);
/* 0x33 - 0x23 0xF3 - xor - bitwise exclusive or */
TVM_INSTRUCTION_PROTO (ins_xor);
/* 0x35 - 0x23 0xF5 - lshr - long shift right */
TVM_INSTRUCTION_PROTO (ins_lshr);
/* 0x36 - 0x23 0xF6 - lshl - long shift left */
TVM_INSTRUCTION_PROTO (ins_lshl);
/* 0x37 - 0x23 0xF7 - lsum - long sum - used in conjunction with ladd */
TVM_INSTRUCTION_PROTO (ins_lsum);
/* 0x38 - 0x23 0xF8 - lsub - long subtract */
TVM_INSTRUCTION_PROTO (ins_lsub);
/* 0x39 - 0x23 0xF9 - runp - run process */
TVM_INSTRUCTION_PROTO (ins_runp);
/* 0x3B - 0x23 0xFB - sb - store byte */
TVM_INSTRUCTION_PROTO (ins_sb);
/* 0x3C - 0x23 0xFC - gajw - general adjust workspace */
TVM_INSTRUCTION_PROTO (ins_gajw);





/****************************************************************************
 *              0x24 0xF_         0x24 0xF_         0x24 0xF_               *
 ****************************************************************************/
/* 0x40 - 0x24 0xF0 - shr - shift right (logical) */
TVM_INSTRUCTION_PROTO (ins_shr);
/* 0x41 - 0x24 0xF1 - shl - shift left (logical) */
TVM_INSTRUCTION_PROTO (ins_shl);
/* 0x42 - 0x24 0xF2 - mint - minimum integer */
TVM_INSTRUCTION_PROTO (ins_mint);
/* 0x46 - 0x24 0xF6 - and - bitwise and */
TVM_INSTRUCTION_PROTO (ins_and);
/* 0x4A - 0x24 0xFA - move - move message */
TVM_INSTRUCTION_PROTO (ins_move);
/* 0x4B - 0x24 0xFB - or - or */
TVM_INSTRUCTION_PROTO (ins_or);
/* 0x4C - 0x24 0xFC - csngl - check single */
TVM_INSTRUCTION_PROTO (ins_csngl);
/* 0x4D - 0x24 0xFD - ccnt1 - check count from 1 */
TVM_INSTRUCTION_PROTO (ins_ccnt1);
/* 0x4F - 0x24 0xFF - ldiff - Long Difference */
TVM_INSTRUCTION_PROTO (ins_ldiff);




/****************************************************************************
 *              0x25 0xF_         0x25 0xF_         0x25 0xF_               *
 ****************************************************************************/

/* 0x52 - 0x25 0xF2 - sum - sum */
TVM_INSTRUCTION_PROTO (ins_sum);
/* 0x53 - 0x25 0xF3 - mul - multiply */
TVM_INSTRUCTION_PROTO (ins_mul);
/* 0x55 - 0x25 0xF5 - stoperr - stop on error */
TVM_INSTRUCTION_PROTO (ins_stoperr);
/* 0x56 - 0x25 0xF6 - cword - check word */
TVM_INSTRUCTION_PROTO (ins_cword);




/****************************************************************************
 *              0x27 0xF_         0x27 0xF_         0x27 0xF_               *
 ****************************************************************************/

/* 0x79 - 0x27 0xF9 - pop - pop top of stack */
TVM_INSTRUCTION_PROTO (ins_pop);





/****************************************************************************
 *              0x2F 0xF_         0x2F 0xF_         0x2F 0xF_               *
 ****************************************************************************/

/* 0xFE - 0x2F 0xFE - ins_shutdown - application shutdown */
TVM_INSTRUCTION_PROTO (ins_shutdown);

#endif /* INS_SEC_H */

