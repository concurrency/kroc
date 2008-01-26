/*
tvm - scheduler.c
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

#ifdef ENABLE_SCHED_SYNC
#include <unistd.h>
#endif

WORD (*tvm_get_time)(void)	= NULL;
void (*tvm_set_alarm)(WORD)	= NULL;
void (*tvm_sleep)(void)		= NULL;

int has_shutdown = 0;

#ifdef SCHEDULER_ENABLE_BUSYWAIT_HOOK
/* FIXME: Should this be somewehere else? */
void (*scheduler_busywait_hook)(void);
#endif
static volatile int sched_sync = 0;

void tvm_sched_sync(void)
{
	sched_sync = 1;
}

void tvm_just_add_to_queue(WORDPTR ws)
{

	WORKSPACE_SET(ws, WS_NEXT, (WORD)NOT_PROCESS_P);
	
	if(fptr[pri] == (WORDPTR)NOT_PROCESS_P)
	{
		/* If there is nothing on the queue, we will make this process
		 * the only thing on the queue */
		fptr[pri] = ws;
		bptr[pri] = ws;
	}
	else
	{
		/* There are other things on the queue, we add this process to the 
		 * back pointer, and add a link to this process at into the previous
		 * processes workspace */
		WORKSPACE_SET(bptr[pri], WS_NEXT, (WORD)ws);
		bptr[pri] = ws;
	}
}

void tvm_add_to_queue(WORDPTR ws, BYTEPTR iptr_prime)
{
	tvm_just_add_to_queue(ws);
	
	WORKSPACE_SET(bptr[pri], WS_IPTR, (WORD)iptr_prime);
}

void tvm_add_queue_to_queue(WORDPTR front, WORDPTR back)
{
	WORKSPACE_SET(back, WS_NEXT, (WORD)NOT_PROCESS_P);
	
	if(fptr[pri] == (WORDPTR)NOT_PROCESS_P)
	{
		fptr[pri] = front;
		bptr[pri] = back;
	}
	else
	{
		WORKSPACE_SET(bptr[pri], WS_NEXT, (WORD)front);
		bptr[pri] = back;
	}
}

BYTEPTR run_next_on_queue(void)
{
	/*
	   ! SEQ
	   !   tim ? now
	   !   removed := FALSE
	   !   WHILE (Tptr <> NotProcess.p) AND (NOT (Tptr^[Time] TIME_AFTER now))
	   !     SEQ
	   !       ...  move first process on timer queue to the run queue
	   !       removed := TRUE
	   !   IF
	   !     (Tptr <> NotProcess.p) AND removed
	   !       ualarm (Tptr^[Time] MINUS now, 0)
	   !     TRUE
	   !       SKIP
	   */
sched_start:
	if((tvm_set_alarm && sched_sync) || ((!tvm_set_alarm) && (tptr != (WORDPTR)NOT_PROCESS_P)))
	{
		WORD removed = 0;
		WORD now;
		
		sched_sync = 0;
		now = tvm_get_time();
#if 0
		/* Sanity check */
		if(tptr == (WORDPTR)NOT_PROCESS_P)
		{
			printf("Got scheduler sync when tptr = NOT_PROCESS_P\n");
			abort();
		}
#endif

		if((tptr != (WORDPTR)NOT_PROCESS_P))
		{
			while((tptr != (WORDPTR)NOT_PROCESS_P) && (!(TIME_AFTER(WORKSPACE_GET(tptr, WS_TIMEOUT), now))))
			{
				/* Move first process from timer queue to the run queue */
				/*
				   ! -- Move first process on timer queue to the run queue
				   !
				   ! POINTER temp := Tptr:
				   ! SEQ
				   !   Tptr := Tptr^[TLink]
				   !   temp^[TLink] := TimeSet.p           -- i.e. time expired
				   !   temp^[Time] := now                  -- ??
				   !   IF
				   !     temp^[State] = Ready.p
				   !       SKIP                            -- used to trap to impossible(1)
				   !                                       -- the ALTing process has
				   !                                       -- already been put on the
				   !                                       -- run queue by something else
				   !     TRUE
				   !       SEQ
				   !         temp^[State] := Ready.p
				   !         ...  add on to run queue
				   */
				WORDPTR temp = tptr;
				tptr = (WORDPTR)WORKSPACE_GET(tptr, WS_NEXT_T);
				WORKSPACE_SET(temp, WS_NEXT_T, TIME_SET_P);
				WORKSPACE_SET(temp, WS_TIMEOUT, now);
				if(WORKSPACE_GET(temp, WS_ALT_STATE) == READY_P)
				{ 
					/* SKIP -- do nothing */
					/*printf("TIMER - SKIP\n");*/
				}
				else
				{ 
					/*printf("TIMER - ADDED TO QUEUE\n");*/
					WORKSPACE_SET(temp, WS_ALT_STATE, READY_P);
					/* Add on to run queue */
					tvm_just_add_to_queue(temp);
				}
				/*
				   print_timer_queue();
				   print_scheduling_queue();
				   */
				removed++;
			}
			/* We update tnext here (which is equivalent to calling ualarm in the occam
			 * code, they seem to use OS services rather than tnext though) */
			if((tptr != (WORDPTR)NOT_PROCESS_P) && removed)
			{ 
				tnext = WORKSPACE_GET(tptr, WS_TIMEOUT);
				if (tvm_set_alarm)
					tvm_set_alarm(tnext - now);
			}
		}
	}

	/* Any processes in the run queue? */
	if(fptr[pri] != (WORDPTR)NOT_PROCESS_P)
	{
		/* yes */
		wptr = fptr[pri];
	}
	else if(tptr != (WORDPTR)NOT_PROCESS_P)
	{
		if (tvm_sleep)
			tvm_sleep();
		goto sched_start;
	}
	else if (has_shutdown)
	{
		exit_runloop(EXIT_STACK_BOTTOM);
	}
	else
	{
		/* DEADLOCK */
		exit_runloop(EXIT_DEADLOCK);
	}

	/* Update thet run queue by taking new current process off it */
	if(fptr[pri] == bptr[pri])
	{
		fptr[pri] = (WORDPTR)NOT_PROCESS_P;
		bptr[pri] = (WORDPTR)NOT_PROCESS_P;
	}
	else
	{
		fptr[pri] = (WORDPTR)WORKSPACE_GET(fptr[pri], WS_NEXT);
	}
	
	return (BYTEPTR)WORKSPACE_GET(wptr, WS_IPTR);
}

/* PREREQUISITES:
 *   The workspace must be already have WS_TIMEOUT set correctly
 */
void timer_queue_insert(WORD current_time, WORD reschedule_time)
{
	/* Check if the queue is empty */
	if(tptr == (WORDPTR)NOT_PROCESS_P)
	{
		/* It was, insert ourselves as the only thing */
		tptr = wptr;
		/* Update the tnext value */
		/* tnext = WORKSPACE_GET(wptr, WS_TIMEOUT); */
		/* This should work instead of the above line */
		tnext = reschedule_time;

		if (tvm_set_alarm)
			tvm_set_alarm(reschedule_time - current_time);

		WORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);
		WORKSPACE_SET(wptr, WS_NEXT_T, NOT_PROCESS_P);
	}
	/* Check if we should be at the top of the queue */
	else if(TIME_AFTER(tnext, reschedule_time))
	{
		/* If the time in tnext is after our reschedule_time, then we should be at
		 * the front of the queue, so lets insert ourselves there. We know there
		 * is at least one other thing on the queue than us we need to insert
		 * ourselves before */

		/* Update our NEXT pointer to point to the previous head of the queue */
		WORKSPACE_SET(wptr, WS_NEXT_T, (WORD)tptr);
		/* Add us to the front pointer */
		tptr  = wptr;
		/* Set the new reschedule time in tnext */
		tnext = reschedule_time;
		
		if (tvm_set_alarm)
			tvm_set_alarm(reschedule_time - current_time);
	}
	else
	{
		/* No, things get a bit more complicated :( */

		/* Signal we are not done yet :) */
		int done = 0;
		/* Get the first workspace on the timer queue */
		WORDPTR this_wptr = tptr;
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
			else if(TIME_AFTER(WORKSPACE_GET(next_wptr, WS_TIMEOUT), reschedule_time))
			{
				/* If the reschedule time of the process after the one we are looing at,
				 * is TIME_AFTER the reschedule time we have, then we need to insert
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
