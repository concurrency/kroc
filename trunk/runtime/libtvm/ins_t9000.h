/*
tvm - ins_t9000.h
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2008 Carl G. Ritson <cgr@kent.ac.uk>

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

#ifndef INS_T9000_H
#define INS_T9000_H

#ifdef TVM_SHORT_OPS

/****************************************************************************
 *              0x2B 0xF_         0x2B 0xF_         0x2B 0xF_               *
 ****************************************************************************/

/* 0xB8 - 0x2B 0xF8 - xbword - sign extend byte to word */
TVM_INSTRUCTION_PROTO (ins_xbword);
/* 0xB9 - 0x2B 0xF9 - lbx - load byte and extend */
TVM_INSTRUCTION_PROTO (ins_lbx);
/* 0xBA - 0x2B 0xFA - cb - check byte */
TVM_INSTRUCTION_PROTO (ins_cb);
/* 0xBB - 0x2B 0xFB - cbu - check byte unsigned */
TVM_INSTRUCTION_PROTO (ins_cbu);

/****************************************************************************
 *              0x2C 0xF_         0x2C 0xF_         0x2C 0xF_               *
 ****************************************************************************/

/* 0xC1 - 0x2C 0xF1 - ssub - sixteen subscript */
TVM_INSTRUCTION_PROTO (ins_ssub);
/* 0xC7 - 0x2C 0xF7 - cir - check in range */
TVM_INSTRUCTION_PROTO (ins_cir);
/* 0xC8 - 0x2C 0xF8 - ss - store sixteen */
TVM_INSTRUCTION_PROTO (ins_ss);
/* 0xCA - 0x2C 0xFA - ls - load sixteen */
TVM_INSTRUCTION_PROTO (ins_ls);
/* 0xCC - 0x2C 0xFC - ciru - check in range unsigned */
TVM_INSTRUCTION_PROTO (ins_ciru);


/****************************************************************************
 *              0x2F 0xF_         0x2F 0xF_         0x2F 0xF_               *
 ****************************************************************************/

/* 0xF8 - 0x2F 0xF8 - xsword - sign extend sixteen to word */
TVM_INSTRUCTION_PROTO (ins_xsword);
/* 0xF9 - 0x2F 0xF9 - lsx - load sixteen and extend */
TVM_INSTRUCTION_PROTO (ins_lsx);
/* 0xFA - 0x2F 0xFA - cs - check sixteen */
TVM_INSTRUCTION_PROTO (ins_cs);
/* 0xFB - 0x2F 0xFB - csu - check sixteen unsigned */
TVM_INSTRUCTION_PROTO (ins_csu);

#endif /* TVM_SHORT_OPS */

#endif /* INS_T9000_H */
