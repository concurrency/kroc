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

#include "transputer.h"
#include "instructions.h"
#include "scheduler.h"
#include "hook_timer.h"
#include "mem.h"
#include "timer.h"
#include "ins_timer.h"
#include "interpreter.h"

/* #ifdef ENABLE_SCHED_SYNC */
/* 060527 MCJ - What should this be, really? Putting it 
   under ENABLE_SCHED_SYNC is only good if every platform
   for which we want this to be true is added to ENABLE_SCHED_SYNC.
   That might be the preferred behavior, but I don't know that
   right now, being a bear of very little brain. I've set this
   to something that fixes my immediate problem, but still
   leaves it generally broken. 
*/
/* FIXME: This ought to just be #ifdef HAVE_SYS_TIME_H -- but the Arduino does have a timer! */
#if !((defined ARM7) || (defined MSP430) || (defined ARDUINO) || (defined WIN32) || (defined __H8300__))
#include <sys/time.h>
#endif

/* FIXME: This possibly ought to be somewhere else */
WORD (*get_time)(void);

int AFTER(WORD t1, WORD t2)
{
	/* From the T9000 Transputer Instruction Set Manual, nice and simple */\
	return ((t1 - t2) > 0);
}

#ifdef ENABLE_SCHED_SYNC
/* FIXME: This should come from the wrapper */
/* FIXME: Should we sanity check for time > 0 here? */
void set_alarm(int time)
{
	struct itimerval timeoutval;

	if(time <= 0)
	{
		/* If we got something which is less than zero then the timeout already
		 * happened.
		 * FIXME: Do something more intelligent than just setting the alarm even
		 * though we have already timed out. */
		time = 1;
	}

	timerclear(&timeoutval.it_interval);	
	timerclear(&timeoutval.it_value);	
	timeoutval.it_value.tv_sec = time / 1000000;
	timeoutval.it_value.tv_usec = time % 1000000;

	if(setitimer(ITIMER_REAL, &timeoutval, NULL) != 0)
	{
		/* If setitimer fails then we're going to deadlock, so it's
		 * better to explicitly blow up here. */
		exit_runloop(EXIT_SCHEDULER_BAD_1);
	}
}
#endif

/* FIXME: unsure if we should pass in priority, dont thinks so as priority is
 * probably just a global we can work grap from here, it would only be an issue
 * if we want to insert into a different priority queue than we are running at.
 */
/* PREREQUISITES:
 *   The workspace must be already have WS_TIMEOUT set correctly
 */
void timer_queue_insert(WORD current_time, WORD reschedule_time)
{
	/* Check if the queue is empty */
	if(tptr[pri] == (WORDPTR)NOT_PROCESS_P)
	{
		/* It was, insert ourselves as the only thing */
		tptr[pri] = wptr;
		/* Update the tnext value */
		/* tnext[pri] = WORKSPACE_GET(wptr, WS_TIMEOUT); */
		/* This should work instead of the above line */
		tnext[pri] = reschedule_time;
#ifdef ENABLE_SCHED_SYNC
		set_alarm(reschedule_time - current_time);
#endif
		WORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);
		WORKSPACE_SET(wptr, WS_NEXT_T, NOT_PROCESS_P);
	}
	/* Check if we should be at the top of the queue */
	else if(AFTER(tnext[pri], reschedule_time))
	{
		/* If the time in tnext is after our reschedule_time, then we should be at
		 * the front of the queue, so lets insert ourselves there. We know there
		 * is at least one other thing on the queue than us we need to insert
		 * ourselves before */

		/* Update our NEXT pointer to point to the previous head of the queue */
		WORKSPACE_SET(wptr, WS_NEXT_T, (WORD)tptr[pri]);
		/* Add us to the front pointer */
		tptr[pri]  = wptr;
		/* Set the new reschedule time in tnext */
		tnext[pri] = reschedule_time;
#ifdef ENABLE_SCHED_SYNC
		set_alarm(reschedule_time - current_time);
#endif
	}
	else
	{
		/* No, things get a bit more complicated :( */

		/* Signal we are not done yet :) */
		int done = 0;
		/* Get the first workspace on the timer queue */
		WORDPTR this_wptr = tptr[pri];
		/* Get the (first) next workspace pointer on the queue */
		WORDPTR next_wptr = (WORDPTR)WORKSPACE_GET(this_wptr, WS_NEXT_T);
	
		/* Now loop through the list */
		while(!done)
		{
			/* Are we at the end of the queue */
			if(next_wptr == (WORDPTR)NOT_PROCESS_P)
			{
				/* Yes, insert us at the end, no update of TNEXT */

				/* Adjust the process at the end of the queue to point to us */
				WORKSPACE_SET(this_wptr, WS_NEXT_T, (WORD)wptr);
				/* Adjust ourselves to point to the end of the queue NOT_PROCESS_P */
				WORKSPACE_SET(wptr, WS_NEXT_T, NOT_PROCESS_P);

				/* We are done */
				done = 1;
			}
			/* Do we need to insert ourselves after this process? */
			else if(AFTER(WORKSPACE_GET(next_wptr, WS_TIMEOUT), reschedule_time))
			{
				/* If the reschedule time of the process after the one we are looing at,
				 * is AFTER the reschedule time we have, then we need to insert
				 * ourselves before that process (ie after the current process) */

				/* Insert ourselves after the process we are looking at */
				/* We are going to point to the next process in the queue */
		    WORKSPACE_SET(wptr, WS_NEXT_T, (WORD)next_wptr);
				/* The current process in the queue is going to point to us */
		    WORKSPACE_SET(this_wptr, WS_NEXT_T, (WORD)wptr);

				/* We are done */
				done = 1;
			}
			/* We need to dig deeper in the list */
			else
			{
				/* FIXME: Do we really need to get all these values every time round? */

				/* Get the next workspace on the timer queue */
				this_wptr = next_wptr;
				/* Get the (next) next workspace pointer on the queue */
				next_wptr = (WORDPTR)WORKSPACE_GET(this_wptr, WS_NEXT_T);

				/* We are NOT done!!! */
			}
		}
	}
}
	

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
 * (areg (out (str current time)))
 * (breg (out areg))
 * (creg (out breg))
 * (iptr (out nextinst))
 */

TVM_INSTRUCTION void ins_ldtimer(void)
{
	if(get_time != 0)
	{
		/* Load current time onto stack */
		STACK(get_time(), areg, breg);
	}
	else
	{
		/* If the wrapper doesn't provide time services act as if not implemented */
		ins_not_implemented();
	}
}

/* 0x2B - 0x22 0xFB - tin - timer input */

/** DESCRIPTION
 * This instruction is used to implement the `tim ? AFTER sometime' operation
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
 *  ((AFTER now areg)
 *   (areg (out undefined))
 *   (breg (out undefined))
 *   (creg (out undefined))
 *   (iptr (out nextinst)))
 *  ((BEFORE now areg)
 *   (areg (out undefined))
 *   (breg (out undefined))
 *   (creg (out undefined))
 *   (iptr (out (str iptr of next scheduled process)))))
 */

TVM_INSTRUCTION void ins_tin(void)
{
	if(get_time != 0)
	{
		WORD current_time = get_time();
		WORD reschedule_time = areg;

		if(AFTER(current_time, reschedule_time))
		{
			/* Do nothing, as we have already timed out */
		}
		else
		{
			/* Store our reschedule time in our workspace */
			WORKSPACE_SET(wptr, WS_TIMEOUT, reschedule_time);
			/* Store the iptr in our workspace */
			WORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);
			
			/* We need to insert ourselves into the timer queue, this is an ordered
			 * queue, after that we need to reschedule another process */
			/* Put ourselves into the timer queue */
			timer_queue_insert(current_time, reschedule_time);
			/* FIXME: Is this the correct thing to do next? */
			/* Since we use the (ALT) STATE to check for READY_P for things on the
			 * timer queue, we should probably set the (ALT) STATE to a known value
			 * here (ie something else than READY_P
			 */
			WORKSPACE_SET(wptr, WS_ALT_STATE, NOT_PROCESS_P);
			/* Run the next process */
			iptr = run_next_on_queue();
			/* Undefine the entire stack */
			/* STACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg)); */
		}
	}
	else
	{
		ins_not_implemented();
	}
}



