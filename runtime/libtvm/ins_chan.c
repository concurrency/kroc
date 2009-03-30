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

/*{{{  Documentation */
/*
 * The channel communication code is divided into three layers:
 *   - scheduling,
 *   - channel word manipulation,
 *   - and data movement.
 * 
 * Each layer does not interfere in the actions of the others,
 * although may control the flow of execution.
 *
 * The scheduling layer invokes the channel word manipulation layer
 * with pointers to data movement handlers.  It then acts on the
 * return values in a way appropriate for the type of communication
 * (normal or extended).
 *
 * The channel word manipulation layer reads the channel word,
 * invokes data movement handlers if appriopriate, and perhaps
 * saves this process in the channel.  Returning the information
 * required to make a scheduling decision.
 *
 * Data movement handlers simply move data (or not in the case of 
 * disconnected channel).
 *
 * Why this piece of documentation?  To try and remind people that
 * having a data movement handler which does scheduling operations
 * potentially confusing, and that by observing the defined
 * hierarchy the code should maintain a degree of clarity.
 */
/*}}}*/

/*{{{  Data movement */
TVM_HELPER int channel_input (ECTX ectx, BYTEPTR dst_ptr, WORD len, WORDPTR src_wptr)
{
	BYTEPTR	src_ptr = (BYTEPTR) WORKSPACE_GET (src_wptr, WS_POINTER);
	tvm_memcpy (dst_ptr, src_ptr, len);
	copy_type_shadow (ectx, dst_ptr, src_ptr, len);
	return ECTX_CONTINUE;
}

TVM_HELPER int channel_output (ECTX ectx, BYTEPTR src_ptr, WORD len, WORDPTR dst_wptr)
{
	BYTEPTR	dst_ptr = (BYTEPTR) WORKSPACE_GET (dst_wptr, WS_POINTER);
	tvm_memcpy (dst_ptr, src_ptr, -len);
	copy_type_shadow (ectx, dst_ptr, src_ptr, -len);
	return ECTX_CONTINUE;
}

TVM_HELPER int channel_swap (ECTX ectx, BYTEPTR src_ptr, WORD len, WORDPTR dst_wptr)
{
	BYTEPTR	dst_ptr = (BYTEPTR) WORKSPACE_GET (dst_wptr, WS_POINTER);
	swap_data_word (ectx, (WORDPTR) dst_ptr, (WORDPTR) src_ptr);
	return ECTX_CONTINUE;
}

TVM_HELPER int channel_dc_input (ECTX ectx, BYTEPTR dst_ptr, WORD len)
{
	while (len--) {
		write_byte (dst_ptr, (BYTE) 0);
		dst_ptr = byteptr_plus (dst_ptr, 1);
		write_type (ectx, dst_ptr, STYPE_DATA);
	}
	return ECTX_CONTINUE;
}

TVM_HELPER int channel_dc_nop (ECTX ectx, BYTEPTR ptr, WORD len)
{
	return ECTX_CONTINUE;
}

#ifdef TVM_EXTERNAL_CHANNEL_BUNDLES
TVM_HELPER int channel_ext_input (ECTX ectx, EXT_CB_INTERFACE *intf, void *ext_data, WORDPTR chan_ptr, BYTEPTR data_ptr, WORD data_len)
{
	if (intf->in != NULL)
		return intf->in (ectx, ext_data, chan_ptr, data_ptr, data_len);
	else
		SET_ERROR_FLAG_RET (EFLAG_EXTCHAN);
		
}

TVM_HELPER int channel_ext_output (ECTX ectx, EXT_CB_INTERFACE *intf, void *ext_data, WORDPTR chan_ptr, BYTEPTR data_ptr, WORD data_len)
{
	if (intf->out != NULL)
		return intf->out (ectx, ext_data, chan_ptr, data_ptr, -data_len);
	else
		SET_ERROR_FLAG_RET (EFLAG_EXTCHAN);
}

TVM_HELPER int channel_ext_swap (ECTX ectx, EXT_CB_INTERFACE *intf, void *ext_data, WORDPTR chan_ptr, BYTEPTR data_ptr, WORD data_len)
{
	if (intf->swap != NULL)
		return intf->swap (ectx, ext_data, chan_ptr, data_ptr, data_len);
	else
		SET_ERROR_FLAG_RET (EFLAG_EXTCHAN);
}
#endif /* TVM_EXTERNAL_CHANNEL_BUNDLES */
/*}}}*/

/*{{{  Channel word manipulation */
TVM_HELPER int chan_io (ECTX ectx, 
			WORDPTR chan_ptr, BYTEPTR data_ptr, WORD data_len, 
			WORDPTR *requeue, CHAN_IO_OK data, CHAN_IO_DC dc,
			CHAN_IO_EXT ext)
{
	WORD	chan_value = read_word (chan_ptr);
	WORDPTR other_WPTR = (WORDPTR) (chan_value & (~1));

	*requeue = (WORDPTR) chan_value;

	#ifdef TVM_TYPE_SHADOW
	if (read_type (ectx, chan_ptr) != STYPE_CHAN)
		write_type (ectx, chan_ptr, STYPE_CHAN);
	#endif

	if (chan_value != NOT_PROCESS_P) {
		#ifdef TVM_EXTERNAL_CHANNEL_BUNDLES
		if (chan_value & 2) {
			WORDPTR cb		= (WORDPTR) (chan_value & (~3));
			EXT_CB_INTERFACE *intf	= (EXT_CB_INTERFACE *) read_offset (cb, mt_cb_ext_t, interface);
			void *ext_data 		= (void *) read_offset (cb, mt_cb_ext_t, data);
			
			*requeue = (WORDPTR) NOT_PROCESS_P;

			if (intf != NULL) {
				int ret = ext (ectx, intf, ext_data, chan_ptr, data_ptr, data_len);
				if (ret == ECTX_CONTINUE) {
					return _ECTX_BYPASS; 
				} else {
					return ret;
				}
			} else {
				SET_ERROR_FLAG_RET (EFLAG_EXTCHAN);
			}
		} else
		#endif /* TVM_EXTERNAL_CHANNEL_BUNDLES */
		if (!(chan_value & 1)) {
			/* Normal communication */
			write_word (chan_ptr, NOT_PROCESS_P);
			return data (ectx, data_ptr, data_len, other_WPTR);
		} else if (other_WPTR != NOT_PROCESS_P) {
			/* Other end is ALTing */
			WORD alt_state = WORKSPACE_GET (other_WPTR, WS_STATE);
			
			if (alt_state != WAITING_P) {
				*requeue = NOT_PROCESS_P;
			}

			switch (alt_state) {
				case WAITING_P:
				case ENABLING_P:
					WORKSPACE_SET (other_WPTR, WS_STATE, DISABLING_P);
					/* Fall through */
				case DISABLING_P:
					break;
				default:
					SET_ERROR_FLAG_RET (EFLAG_CHAN);
			}

			/* Fall through */
		} else {
			/* Disconnected channel */
			return dc (ectx, data_ptr, data_len);
		}
	}
	
	/* Store state */
	WORKSPACE_SET (WPTR, WS_POINTER, (WORD) data_ptr);
	WORKSPACE_SET (WPTR, WS_ECTX, (WORD) ectx);
	WORKSPACE_SET (WPTR, WS_PENDING, data_len);
	WORKSPACE_SET (WPTR, WS_IPTR, (WORD) IPTR);
	
	/* Put this process into the channel word */
	write_word (chan_ptr, (WORD) WPTR);

	return _ECTX_DESCHEDULE;
}
/*}}}*/

/*{{{  I/O scheduling */
TVM_HELPER int chan_std_io (ECTX ectx, 
		WORDPTR chan_ptr, BYTEPTR data_ptr, WORD data_len,
		CHAN_IO_OK data, CHAN_IO_DC dc, CHAN_IO_EXT ext)
{
	WORDPTR requeue;
	int ret;
	
	UNDEFINE_STACK ();

	ret = chan_io (
		ectx,
		chan_ptr, data_ptr, data_len,
		&requeue,
		data, dc, ext
	);

	if (ret == ECTX_CONTINUE && requeue != NOT_PROCESS_P) {
		DESCHEDULE_CURRENT ();
	} else if (ret > 0) {
		return ret;
	} else if (ret == _ECTX_BYPASS) {
		return ECTX_CONTINUE;
	}

	requeue = (WORDPTR) (((WORD) requeue) & (~1));

	if (requeue != NOT_PROCESS_P) {
		LOAD_PROCESS_RET (requeue);
	} else {
		RUN_NEXT_ON_QUEUE_RET ();
	}
}		

TVM_HELPER int chan_in (ECTX ectx, WORD num_bytes, WORDPTR chan_ptr, BYTEPTR write_start)
{
	return chan_std_io (
		ectx, chan_ptr, write_start, num_bytes, 
		channel_input, channel_dc_input,
		IF_EXTERNAL_CHANNEL_BUNDLES (channel_ext_input)
	);
}

TVM_HELPER int chan_out (ECTX ectx, WORD num_bytes, WORDPTR chan_ptr, BYTEPTR read_start)
{
	return chan_std_io (
		ectx, chan_ptr, read_start, -num_bytes, 
		channel_output, channel_dc_nop,
		IF_EXTERNAL_CHANNEL_BUNDLES (channel_ext_output)
	);
}

TVM_HELPER int chan_swap (ECTX ectx, WORDPTR chan_ptr, WORDPTR data_ptr)
{
	return chan_std_io (
		ectx, chan_ptr, (BYTEPTR) data_ptr, 0, 
		channel_swap, channel_dc_nop,
		IF_EXTERNAL_CHANNEL_BUNDLES (channel_ext_swap)
	);
}
/*}}}*/

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

