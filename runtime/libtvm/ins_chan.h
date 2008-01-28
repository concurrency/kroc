/*
tvm - ins_chan.h
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

#ifndef INS_CHAN_H
#define INS_CHAN_H

#include "instructions.h"

TVM_HELPER_PROTO int chan_io_begin(tvm_ectx_t *ectx, WORD altable, WORDPTR chan_ptr, BYTEPTR data_ptr, WORDPTR *chan_val);
TVM_HELPER_PROTO void chan_io_end(tvm_ectx_t *ectx, WORDPTR chan_ptr, WORDPTR other_wptr);
TVM_HELPER_PROTO int chan_in(tvm_ectx_t *ectx, WORD num_bytes, WORDPTR chan_ptr, BYTEPTR write_start);
TVM_HELPER_PROTO int chan_out(tvm_ectx_t *ectx, WORD num_bytes, WORDPTR chan_ptr, BYTEPTR read_start);
TVM_HELPER_PROTO int chan_swap(tvm_ectx_t *ectx, WORDPTR chan_ptr, WORDPTR data_ptr);

/****************************************************************************
 *                   0xF_              0xF_              0xF_               *
 ****************************************************************************/

/* 0x07 - 0xF7 - in - input message */
TVM_INSTRUCTION_PROTO (ins_in);
/* 0x0B - 0xFB - out - output message */
TVM_INSTRUCTION_PROTO (ins_out);
/* 0x0E - 0xFE - outbyte - output byte */
TVM_INSTRUCTION_PROTO (ins_outbyte);
/* 0x0F- 0xFF - outword - output word */
TVM_INSTRUCTION_PROTO (ins_outword);

#endif /* INS_CHAN_H */
