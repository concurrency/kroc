/*
tvm - ins_alt.c
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
#include "interpreter.h"
#include "scheduler.h"
#include "ins_alt.h"

/****************************************************************************
 *              0x22 0xF_         0x22 0xF_         0x22 0xF_               *
 ****************************************************************************/

/* 0x2E - 0x22 0xFE - dist - disable timer */
TVM_INSTRUCTION (ins_dist)
{
	WORD process	= AREG;
	WORD guard	= BREG;
	WORD timeout	= CREG;
	WORD fired	= 0;

	if (guard) {
		WORD tlink = WORKSPACE_GET (WPTR, WS_TLINK);
		if (tlink == TIME_SET_P) {
			/* Time is set*/
			WORD time = WORKSPACE_GET (WPTR, WS_TIME);
			if (!TIME_AFTER (timeout, time)) {
				/* Time is after or equal to timeout */
				WORD selection = WORKSPACE_GET (WPTR, WS_TEMP);
				if (selection == NONE_SELECTED_O) {
					/* Nothing selected; go with this => fired */
					WORKSPACE_SET (WPTR, WS_TEMP, process);
					fired = 1;
				}
			}
		}
		else if (tlink != TIME_NOT_SET_P) {
			/* Still on timer queue; remove */
			TIMER_QUEUE_REMOVE (WPTR);
		}
	}
	
	STACK1_RET (fired, STYPE_DATA);
}

/* 0x2F - 0x22 0xFF - disc - disable channel */
TVM_INSTRUCTION (ins_disc)
{
	WORDPTR chan_ptr	= (WORDPTR) CREG;
	WORD guard		= BREG;
	WORD process		= AREG;
	WORD fired		= 0;
	
	if (guard) {
		WORD chan_proc = read_word (chan_ptr);
		
		if (chan_proc == (((WORD) WPTR) | 1)) {
			/* Channel word is our WPTR; not fired */
			write_word (chan_ptr, NOT_PROCESS_P);
		} 
		else if (chan_proc != NOT_PROCESS_P) {
			/* Something waiting on channel */
			WORD selection = WORKSPACE_GET (WPTR, WS_TEMP);
			if (selection == NONE_SELECTED_O) {
				/* Nothing selected; go with this => fired */
				WORKSPACE_SET (WPTR, WS_TEMP, process);
				fired = 1;
			}
		} 
	}

	STACK1_RET (fired, STYPE_DATA);
}


/****************************************************************************
 *              0x23 0xF_         0x23 0xF_         0x23 0xF_               *
 ****************************************************************************/

/* 0x30 - 0x23 0xF0 - diss - disable skip */
TVM_INSTRUCTION (ins_diss)
{
	WORD	guard	= BREG;
	WORD	process	= AREG;
	WORD	fired	= 0;

	if (guard) {
		WORD selection = WORKSPACE_GET (WPTR, WS_TEMP);
		if (selection == NONE_SELECTED_O) {
			WORKSPACE_SET (WPTR, WS_TEMP, process);
			fired = 1;
		}
	}

	STACK2_RET (fired, CREG, STYPE_DATA, CREGt);
}


/****************************************************************************
 *              0x24 0xF_         0x24 0xF_         0x24 0xF_               *
 ****************************************************************************/

/* 0x43 - 0x24 0xF3 - alt - alt start */
TVM_INSTRUCTION (ins_alt)
{
	/* Enabling */
	WORKSPACE_SET(WPTR, WS_STATE, ENABLING_P);
	
	return ECTX_CONTINUE;
}

/* 0x44 - 0x24 0xF4 - altwt - alt wait */
TVM_INSTRUCTION (ins_altwt)
{
	WORKSPACE_SET(WPTR, WS_TEMP, NONE_SELECTED_O);

	if (WORKSPACE_GET (WPTR, WS_STATE) == READY_P) {
		UNDEFINE_STACK_RET ();
	} else {
		WORKSPACE_SET (WPTR, WS_STATE, WAITING_P);
		WORKSPACE_SET (WPTR, WS_ECTX, (WORD) ectx);
		WORKSPACE_SET (WPTR, WS_IPTR, (WORD) IPTR);
		RUN_NEXT_ON_QUEUE_RET ();
	}
}

/* 0x45 - 0x24 0xF5 - altend - alt end */
TVM_INSTRUCTION (ins_altend)
{
	WORD selection = WORKSPACE_GET (WPTR, WS_TEMP);
	if (selection != NONE_SELECTED_O) {
		IPTR = byteptr_plus (IPTR, selection);
		return ECTX_CONTINUE;
	} else {
		SET_ERROR_FLAG_RET (EFLAG_ALT);
	}
}

/* 0x47 - 0x24 0xF7 - enbt - enable timer */
TVM_INSTRUCTION (ins_enbt)
{
	WORD guard	= AREG;
	WORD timeout	= BREG;
	
	if (guard) {
		if (WORKSPACE_GET (WPTR, WS_TLINK) == TIME_NOT_SET_P) {
			/* Set timeout as when none set already */
			WORKSPACE_SET (WPTR, WS_TLINK, TIME_SET_P);
			WORKSPACE_SET (WPTR, WS_TIME, timeout);
		}
		else if (TIME_AFTER (WORKSPACE_GET (WPTR, WS_TIME), timeout)) {
			/* Update the timeout if sooner than the existing one */
			WORKSPACE_SET(WPTR, WS_TIME, timeout);
		}
	}
	
	/* Remove the timeout from the stack */
	STACK2_RET (guard, CREG, STYPE_DATA, CREGt);
}

/* 0x48 - 0x24 0xF8 - enbc - enable channel */
TVM_INSTRUCTION (ins_enbc)
{
	WORDPTR chan_ptr	= (WORDPTR) BREG;
	WORD 	guard		= AREG;
	
	if (guard) {
		WORD chan_value	= read_word (chan_ptr);
		WORD this_ws	= ((WORD) WPTR) | 1;

		if (chan_value == NOT_PROCESS_P) {
			/* Empty channel; enable */
			write_word (chan_ptr, this_ws);
		} else if (chan_value != this_ws) {
			/* Something waiting on channel; become ready */
			WORKSPACE_SET (WPTR, WS_STATE, READY_P);
		}
	}

	/* Remove channel pointer from the stack */
	STACK2_RET (guard, CREG, STYPE_DATA, CREGt);
}

/* 0x49 - 0x24 0xF9 - enbs - enable skip */
TVM_INSTRUCTION (ins_enbs)
{
	WORD guard = AREG;

	if (guard) {
		/* Begin disabling */
		WORKSPACE_SET (WPTR, WS_STATE, READY_P);
	}

	/* Leave stack intacted */
	return ECTX_CONTINUE;
}

/* 0x4E - 0x24 0xFE - talt - timer alt start */
TVM_INSTRUCTION (ins_talt)
{
	/* Enabling and timer not set */
	WORKSPACE_SET (WPTR, WS_STATE, ENABLING_P);
	WORKSPACE_SET (WPTR, WS_TLINK, TIME_NOT_SET_P);
	
	return ECTX_CONTINUE;
}


/****************************************************************************
 *              0x25 0xF_         0x25 0xF_         0x25 0xF_               *
 ****************************************************************************/

/* 0x51 - 0x25 0xF1 - taltwt - timer alt wait */
TVM_INSTRUCTION (ins_taltwt)
{
	WORD now = ectx->get_time (ectx);

	WORKSPACE_SET(WPTR, WS_TEMP, NONE_SELECTED_O);

	if (WORKSPACE_GET (WPTR, WS_STATE) == READY_P) {
		WORKSPACE_SET (WPTR, WS_TIME, now);
		UNDEFINE_STACK_RET ();
	}
	else if (WORKSPACE_GET(WPTR, WS_TLINK) != TIME_NOT_SET_P)
	{
		WORD timeout = WORKSPACE_GET (WPTR, WS_TIME);

		if (TIME_AFTER (now, timeout)) {
			WORKSPACE_SET (WPTR, WS_STATE, READY_P);
			WORKSPACE_SET (WPTR, WS_TIME, now);
			UNDEFINE_STACK_RET ();
		}

		timeout += 1;
		WORKSPACE_SET (WPTR, WS_TIME, timeout);
		TIMER_QUEUE_INSERT (WPTR, now, timeout);
	}

	WORKSPACE_SET (WPTR, WS_STATE, WAITING_P);
	WORKSPACE_SET (WPTR, WS_ECTX, (WORD) ectx);
	WORKSPACE_SET (WPTR, WS_IPTR, (WORD) IPTR);
	RUN_NEXT_ON_QUEUE_RET ();
}

