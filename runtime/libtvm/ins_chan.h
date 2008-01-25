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

TVM_HELPER_PROTO WORDPTR chan_io_begin(WORD altable, WORDPTR chan_ptr, BYTEPTR data_ptr);
TVM_HELPER_PROTO void chan_io_end(WORDPTR chan_ptr, WORDPTR other_wptr);
TVM_HELPER_PROTO void chan_in(WORD num_bytes, WORDPTR chan_ptr, BYTEPTR write_start);
TVM_HELPER_PROTO void chan_out(WORD num_bytes, WORDPTR chan_ptr, BYTEPTR read_start);
TVM_HELPER_PROTO void chan_swap(WORDPTR chan_ptr, WORDPTR data_ptr);

/****************************************************************************
 *                   0xF_              0xF_              0xF_               *
 ****************************************************************************/

/* 0x07 - 0xF7 - in - input message */
TVM_INSTRUCTION_PROTO void ins_in(void);
/* 0x0B - 0xFB - out - output message */
TVM_INSTRUCTION_PROTO void ins_out(void);
/* 0x0E - 0xFE - outbyte - output byte */
TVM_INSTRUCTION_PROTO void ins_outbyte(void);
/* 0x0F- 0xFF - outword - output word */
TVM_INSTRUCTION_PROTO void ins_outword(void);

#endif /* INS_CHAN_H */
