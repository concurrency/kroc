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

#include "transputer.h"
#include "instructions.h"
#include "mem.h"
#include "ext_chan.h"
#include "scheduler.h"
#include "ins_chan.h"

TVM_HELPER POOTER chan_io_begin(WORD altable, POOTER chan_ptr, BPOOTER data_ptr)
{
	POOTER chan_val = (POOTER) read_mem(chan_ptr);

	if(chan_val == NOT_PROCESS_P)
	{
		/* ...Put this process('s wptr) into the channel word, */
		write_mem(chan_ptr, (WORD)wptr);
		/* store our state */
		WORKSPACE_SET(wptr, WS_CHAN, (WORD)data_ptr);
		WORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);

		iptr = run_next_on_queue();
	}
	else if(altable)
	{
		POOTER other_wptr = chan_val;
		WORD alt_state = WORKSPACE_GET(other_wptr, WS_ALT_STATE);

		/* Check if alt_state == MIN_INT, MIN_INT +1, MI_INT +2, MIN_INT +3 */
		if((alt_state & (~3)) == MIN_INT)
		{
			WORKSPACE_SET(wptr, WS_CHAN, (WORD)data_ptr);
			WORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);

			write_mem(chan_ptr, (WORD)wptr);
			
			switch(alt_state) {
				case WAITING_P:
					WORKSPACE_SET(other_wptr, WS_ALT_STATE, DISABLING_P);
					wptr = other_wptr;
					iptr = (BPOOTER)WORKSPACE_GET(wptr, WS_IPTR);
					break;
				case ENABLING_P:
					WORKSPACE_SET(other_wptr, WS_ALT_STATE, DISABLING_P);
					/* Fall through */
				case DISABLING_P:
					iptr = run_next_on_queue();
					break;
				default:
					set_error_flag(EFLAG_CHAN);
					break;
			}

			return NOT_PROCESS_P;
		}
	}

	return chan_val;
}

TVM_HELPER void chan_io_end(POOTER chan_ptr, POOTER other_wptr)
{
	/* Set the channel word to NotProcess.p */
	write_mem(chan_ptr, NOT_PROCESS_P);

	/* Add ourselves to the back of the runqueue */
	add_to_queue((WORD)wptr, (WORD)iptr);
	
	/* Reschedule the process at the other end of the channel */
	wptr = (POOTER)other_wptr;
	/* Load the newly scheduled processes instruction pointer */
	iptr = (BPOOTER)WORKSPACE_GET(wptr, WS_IPTR);
}

TVM_HELPER void chan_in(WORD num_bytes, POOTER chan_ptr, BPOOTER write_start)
{
	POOTER other_wptr = chan_io_begin(0, chan_ptr, write_start);

	if (other_wptr != NOT_PROCESS_P)
	{
		/* Where we start reading from */
		BPOOTER read_start = (BPOOTER)WORKSPACE_GET(other_wptr, WS_CHAN);
		/* Copy the data */
		copy_data(write_start, read_start, num_bytes);
		/* Complete channel operation */
		chan_io_end(chan_ptr, other_wptr);
	}

	UNDEFINE_STACK();
}

TVM_HELPER void chan_out(WORD num_bytes, POOTER chan_ptr, BPOOTER read_start)
{
	POOTER other_wptr = chan_io_begin(1, chan_ptr, read_start);

	if(other_wptr != NOT_PROCESS_P)
	{
		/* Normal communication */
		BPOOTER write_start = (BPOOTER)WORKSPACE_GET(other_wptr, WS_CHAN);
		/* Copy the data */
		copy_data(write_start, read_start, num_bytes);
		/* Complete channel operation */
		chan_io_end(chan_ptr, other_wptr);
	}

	UNDEFINE_STACK();
}

TVM_HELPER void chan_swap(POOTER chan_ptr, POOTER data_ptr)
{
	POOTER other_wptr = chan_io_begin(1, chan_ptr, (BPOOTER)data_ptr);

	if(other_wptr != NOT_PROCESS_P)
	{
		/* Normal communication */
		POOTER other_ptr = (POOTER)WORKSPACE_GET(other_wptr, WS_CHAN);
		/* Swap data */
		swap_data_word(data_ptr, other_ptr);
		/* Complete channel operation */
		chan_io_end(chan_ptr, other_wptr);
	}

	UNDEFINE_STACK();
}

/****************************************************************************
 *                   0xF_              0xF_              0xF_               *
 ****************************************************************************/

/* 0x07 - 0xF7 - in - input message */
/* FIXME: In soccam make sure that in and out are a bit more consittent  in
 * variable naming, and order of things */
TVM_INSTRUCTION void ins_in(void)
{
	BPOOTER data_ptr	= (BPOOTER)creg;
	POOTER chan_ptr		= (POOTER)breg;
	WORD bytes		= areg;

	chan_in(bytes, chan_ptr, data_ptr);
}

/* 0x0B - 0xFB - out - output message */
TVM_INSTRUCTION void ins_out(void)
{
	BPOOTER data_ptr	= (BPOOTER)creg;
	POOTER chan_ptr		= (POOTER)breg;
	WORD bytes		= areg;

	chan_out(bytes, chan_ptr, data_ptr);
}

/* 0x0E - 0xFE - outbyte - output byte */
TVM_INSTRUCTION void ins_outbyte(void)
{
	BPOOTER data_ptr	= (BPOOTER)wptr;
	POOTER chan_ptr		= (POOTER)breg;
	BYTE data		= (BYTE)areg;

	/* Put the byte to be transfered at the top of the workspace */
	write_byte(data_ptr, data);

	chan_out(1, chan_ptr, data_ptr);
}

/* 0x0F - 0xFF - outword - output word */
TVM_INSTRUCTION void ins_outword(void)
{
	BPOOTER data_ptr	= (BPOOTER)wptr;
	POOTER chan_ptr		= (POOTER)breg;
	WORD data		= areg;

	/* Put the word to be transfered at the top of the workspace */
	write_mem((POOTER)data_ptr, data);

	chan_out(WORDLENGTH, chan_ptr, data_ptr);
}

