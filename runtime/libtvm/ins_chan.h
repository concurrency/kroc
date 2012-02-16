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

typedef int (*CHAN_IO_OK)(ECTX, BYTEPTR, WORD, WORDPTR);
typedef int (*CHAN_IO_DC)(ECTX, BYTEPTR, WORD);
typedef int (*CHAN_IO_EXT)(ECTX, EXT_CB_INTERFACE *, void *, WORDPTR, BYTEPTR, WORD);

TVM_HELPER_PROTO int channel_input (ECTX ectx, BYTEPTR dst_ptr, WORD len, WORDPTR src_wptr);
TVM_HELPER_PROTO int channel_output (ECTX ectx, BYTEPTR src_ptr, WORD len, WORDPTR dst_wptr);
TVM_HELPER_PROTO int channel_dc_input (ECTX ectx, BYTEPTR dst_ptr, WORD len);
TVM_HELPER_PROTO int channel_dc_nop (ECTX ectx, BYTEPTR ptr, WORD len);
TVM_HELPER_PROTO int channel_swap (ECTX ectx, BYTEPTR src_ptr, WORD len, WORDPTR dst_wptr);
#ifdef TVM_EXTERNAL_CHANNEL_BUNDLES
TVM_HELPER_PROTO int channel_ext_input (ECTX ectx, EXT_CB_INTERFACE *intf, void *ext_data, WORDPTR chan_ptr, BYTEPTR data_ptr, WORD data_len);
TVM_HELPER_PROTO int channel_ext_output (ECTX ectx, EXT_CB_INTERFACE *intf, void *ext_data, WORDPTR chan_ptr, BYTEPTR data_ptr, WORD data_len);
TVM_HELPER_PROTO int channel_ext_swap (ECTX ectx, EXT_CB_INTERFACE *intf, void *ext_data, WORDPTR chan_ptr, BYTEPTR data_ptr, WORD data_len);
#endif /* TVM_EXTERNAL_CHANNEL_BUNDLES */
TVM_HELPER_PROTO int chan_io (ECTX ectx, 
			WORDPTR chan_ptr, BYTEPTR data_ptr, WORD data_len, 
			WORDPTR *requeue, CHAN_IO_OK data, CHAN_IO_DC dc,
			CHAN_IO_EXT ext);
TVM_HELPER_PROTO int chan_std_io (ECTX ectx,
			WORDPTR chan_ptr, BYTEPTR data_ptr, WORD data_len,
			CHAN_IO_OK data, CHAN_IO_DC dc,
			CHAN_IO_EXT ext);
TVM_HELPER_PROTO int chan_in (ECTX ectx, WORD num_bytes, WORDPTR chan_ptr, BYTEPTR write_start);
TVM_HELPER_PROTO int chan_out (ECTX ectx, WORD num_bytes, WORDPTR chan_ptr, BYTEPTR read_start);
TVM_HELPER_PROTO int chan_swap (ECTX ectx, WORDPTR chan_ptr, WORDPTR data_ptr);

#ifdef TVM_EXTERNAL_CHANNEL_BUNDLES
#define IF_EXTERNAL_CHANNEL_BUNDLES(X) X
#else
#define IF_EXTERNAL_CHANNEL_BUNDLES(X) NULL
#endif /* !TVM_EXTERNAL_CHANNEL_BUNDLES */

/****************************************************************************
 *                   0xF_              0xF_              0xF_               *
 ****************************************************************************/

/* 0x07 - 0xF7 - in - input message */
TVM_INSTRUCTION_PROTO (ins_in);
/* 0x0B - 0xFB - out - output message */
TVM_INSTRUCTION_PROTO (ins_out);
/* 0x0E - 0xFE - outbyte - output byte */
TVM_INSTRUCTION_PROTO (ins_outbyte);
/* 0x0F - 0xFF - outword - output word */
TVM_INSTRUCTION_PROTO (ins_outword);

#endif /* INS_CHAN_H */
