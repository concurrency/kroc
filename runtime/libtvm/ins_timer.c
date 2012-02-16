/*
tvm - ins_timer.c
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
#include "ins_timer.h"
#include "interpreter.h"

/****************************************************************************
 *              0x22 0xF_         0x22 0xF_         0x22 0xF_               *
 ****************************************************************************/

/* 0x22 - 0x22 0xF2 - ldtimer - load timer */

/** DESCRIPTION
 * This instructions loads the current time into the A register. The resolution
 * of the timer is platform dependent.
 *
 * The original transputer had a high- and a low-priority clock, the
 * Transterpreter implements just one clock regardless of how many levels of
 * priority it implements
 */

/** DEVIATES */

/** STATE
 * (AREG (out (str current time)))
 * (BREG (out AREG))
 * (CREG (out BREG))
 * (IPTR (out nextinst))
 */

TVM_INSTRUCTION (ins_ldtimer)
{
	/* Load current time onto stack */
	STACK_RET(ectx->get_time(ectx), AREG, BREG, STYPE_DATA, AREGt, BREGt);
}

/* 0x2B - 0x22 0xFB - tin - timer input */

/** DESCRIPTION
 * This instruction is used to implement the `tim ? TIME_AFTER sometime' operation
 * in occam. This is essentially a timeout, where the process waits for the
 * current time to be later than `sometime'.
 *
 * If the current time is already later than `sometime' the current process
 * will not deschedule but continue execution. If this is not the case however,
 * this process will be put onto the timer queue, and a new process will be
 * scheduled in its stead.
 */

/** DESCHEDULE */

/* FIXME: Add timer + run queue info */
/** STATE
 * (where
 *  ((TIME_AFTER now AREG)
 *   (AREG (out undefined))
 *   (BREG (out undefined))
 *   (CREG (out undefined))
 *   (IPTR (out nextinst)))
 *  ((BEFORE now AREG)
 *   (AREG (out undefined))
 *   (BREG (out undefined))
 *   (CREG (out undefined))
 *   (IPTR (out (str IPTR of next scheduled process)))))
 */

TVM_INSTRUCTION (ins_tin)
{
	WORD current_time = ectx->get_time(ectx);
	WORD reschedule_time = AREG;

	if(TIME_AFTER(current_time, reschedule_time))
	{
		/* Do nothing, as we have already timed out */
		UNDEFINE_STACK_RET();
	}
	else
	{
		/* Store our reschedule time in our workspace */
		WORKSPACE_SET(WPTR, WS_TIME, reschedule_time);
		/* Store the IPTR in our workspace */
		WORKSPACE_SET(WPTR, WS_IPTR, (WORD)IPTR);
		
		/* We need to insert ourselves into the timer queue, this is an ordered
		 * queue, after that we need to reschedule another process */
		/* Put ourselves into the timer queue */
		TIMER_QUEUE_INSERT(WPTR, current_time, reschedule_time);
		
		/* FIXME: Is this the correct thing to do next? */
		/* Since we use the (ALT) STATE to check for READY_P for things on the
		 * timer queue, we should probably set the (ALT) STATE to a known value
		 * here (ie something else than READY_P
		 */
		WORKSPACE_SET(WPTR, WS_STATE, NOT_PROCESS_P);
		
		/* Run the next process */
		RUN_NEXT_ON_QUEUE_RET();
	}
}

