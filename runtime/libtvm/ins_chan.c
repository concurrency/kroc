/*
tvm - ins_chan.c
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

#include "tvm.h"
#include "instructions.h"
#include "scheduler.h"
#include "ins_chan.h"

TVM_HELPER int channel_input (ECTX ectx, BYTEPTR dst_ptr, UWORD len, WORDPTR src_wptr)
{
	BYTEPTR	src_ptr = (BYTEPTR) WORKSPACE_GET (src_wptr, WS_POINTER);
	ADD_TO_QUEUE (WPTR);
	tvm_copy_data (dst_ptr, src_ptr, len);
	return ECTX_CONTINUE;
}

TVM_HELPER int channel_output (ECTX ectx, BYTEPTR src_ptr, UWORD len, WORDPTR dst_wptr)
{
	BYTEPTR	dst_ptr = (BYTEPTR) WORKSPACE_GET (dst_wptr, WS_POINTER);
	ADD_TO_QUEUE (WPTR);
	tvm_copy_data(dst_ptr, src_ptr, len);
	return ECTX_CONTINUE;
}

TVM_HELPER int channel_broken_input (ECTX ectx, BYTEPTR dst_ptr, UWORD len)
{
	ADD_TO_QUEUE (WPTR);
	while(len--) {
		write_byte (dst_ptr, (BYTE) 0);
		dst_ptr = byteptr_plus (dst_ptr, 1);
	}
	return ECTX_CONTINUE;
}

TVM_HELPER int channel_broken_nop (ECTX ectx, BYTEPTR ptr, UWORD len)
{
	ADD_TO_QUEUE (WPTR);
	return ECTX_CONTINUE;
}

TVM_HELPER int channel_swap (ECTX ectx, BYTEPTR src_ptr, UWORD len, WORDPTR dst_wptr)
{
	BYTEPTR	dst_ptr = (BYTEPTR) WORKSPACE_GET (dst_wptr, WS_POINTER);
	ADD_TO_QUEUE (WPTR);
	swap_data_word ((WORDPTR) dst_ptr, (WORDPTR) src_ptr);
	return ECTX_CONTINUE;
}

TVM_HELPER int chan_io (ECTX ectx, 
			WORDPTR chan_ptr, BYTEPTR data_ptr, UWORD data_len, 
			WORDPTR *requeue, CHAN_IO_OK data, CHAN_IO_BROKEN broken)
{
	WORD	chan_value = read_word (chan_ptr);
	WORDPTR other_WPTR = (WORDPTR) (chan_value & (~1));

	*requeue = (WORDPTR) NOT_PROCESS_P;

	if (chan_value != NOT_PROCESS_P) {
		if (!(chan_value & 1)) {
			/* Normal communication */
			write_word (chan_ptr, NOT_PROCESS_P);
			*requeue = other_WPTR;
			return data (ectx, data_ptr, data_len, other_WPTR);
		} else if ((chan_value & ~1) != NOT_PROCESS_P) {
			WORD alt_state = WORKSPACE_GET(other_WPTR, WS_STATE);

			/* Store state */
			WORKSPACE_SET(WPTR, WS_POINTER, (WORD) data_ptr);
			WORKSPACE_SET(WPTR, WS_ECTX, (WORD) ectx);
			WORKSPACE_SET(WPTR, WS_PENDING, (WORD) data_len);
			WORKSPACE_SET(WPTR, WS_IPTR, (WORD) IPTR);

			/* Put this process into the channel word */
			write_word(chan_ptr, (WORD)WPTR);

			switch(alt_state) {
				case WAITING_P:
					*requeue = other_WPTR;
					/* Fall through */
				case ENABLING_P:
					WORKSPACE_SET(other_WPTR, WS_STATE, DISABLING_P);
					/* Fall through */
				case DISABLING_P:
				case EXTENDED_P:
					break;
				default:
					SET_ERROR_FLAG_RET(EFLAG_CHAN);
			}
		} else {
			/* Broken channel */
			return broken (ectx, data_ptr, data_len);
		}
	} else {
		/* Store state */
		WORKSPACE_SET(WPTR, WS_POINTER, (WORD) data_ptr);
		WORKSPACE_SET(WPTR, WS_ECTX, (WORD) ectx);
		WORKSPACE_SET(WPTR, WS_PENDING, data_len);
		WORKSPACE_SET(WPTR, WS_IPTR, (WORD) IPTR);
		
		/* Put this process into the channel word */
		write_word(chan_ptr, (WORD)WPTR);
	}

	return ECTX_CONTINUE;
}

TVM_HELPER int chan_std_io (ECTX ectx, 
		WORDPTR chan_ptr, BYTEPTR data_ptr, UWORD data_len,
		CHAN_IO_OK data, CHAN_IO_BROKEN broken)
{
	WORDPTR requeue;
	int ret;
	
	UNDEFINE_STACK ();

	ret = chan_io (
		ectx,
		chan_ptr, data_ptr, data_len,
		&requeue,
		data, broken
	);

	if (ret) {
		return ret;
	} else if (requeue != NOT_PROCESS_P) {
		LOAD_PROCESS_RET (requeue);
	} else {
		RUN_NEXT_ON_QUEUE_RET ();
	}
}		

TVM_HELPER int chan_in (ECTX ectx, UWORD num_bytes, WORDPTR chan_ptr, BYTEPTR write_start)
{
	return chan_std_io (
		ectx, chan_ptr, write_start, num_bytes, 
		channel_input, channel_broken_input
	);
}

TVM_HELPER int chan_out (ECTX ectx, UWORD num_bytes, WORDPTR chan_ptr, BYTEPTR read_start)
{
	return chan_std_io (
		ectx, chan_ptr, read_start, num_bytes, 
		channel_output, channel_broken_nop
	);
}

TVM_HELPER int chan_swap (ECTX ectx, WORDPTR chan_ptr, WORDPTR data_ptr)
{
	return chan_std_io (
		ectx, chan_ptr, (BYTEPTR) data_ptr, 0, 
		channel_swap, channel_broken_nop
	);
}

/****************************************************************************
 *                   0xF_              0xF_              0xF_               *
 ****************************************************************************/

/* 0x07 - 0xF7 - in - input message */
TVM_INSTRUCTION (ins_in)
{
	BYTEPTR data_ptr	= (BYTEPTR) CREG;
	WORDPTR chan_ptr	= (WORDPTR) BREG;
	WORD bytes		= AREG;

	return chan_in (ectx, bytes, chan_ptr, data_ptr);
}

/* 0x0B - 0xFB - out - output message */
TVM_INSTRUCTION (ins_out)
{
	BYTEPTR data_ptr	= (BYTEPTR) CREG;
	WORDPTR chan_ptr	= (WORDPTR) BREG;
	WORD bytes		= AREG;

	return chan_out (ectx, bytes, chan_ptr, data_ptr);
}

/* 0x0E - 0xFE - outbyte - output byte */
TVM_INSTRUCTION (ins_outbyte)
{
	BYTEPTR data_ptr	= (BYTEPTR) WPTR;
	WORDPTR chan_ptr	= (WORDPTR) BREG;
	BYTE data		= (BYTE) AREG;

	/* Put the byte to be transfered at the top of the workspace */
	write_byte (data_ptr, data);

	return chan_out (ectx, 1, chan_ptr, data_ptr);
}

/* 0x0F - 0xFF - outword - output word */
TVM_INSTRUCTION (ins_outword)
{
	BYTEPTR data_ptr	= (BYTEPTR) WPTR;
	WORDPTR chan_ptr	= (WORDPTR) BREG;
	WORD data		= AREG;

	/* Put the word to be transfered at the top of the workspace */
	write_word ((WORDPTR) data_ptr, data);

	return chan_out (ectx, TVM_WORD_LENGTH, chan_ptr, data_ptr);
}

