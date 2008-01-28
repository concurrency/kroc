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

static void add_to_queue(tvm_ectx_t *ectx, WORDPTR ws)
{

	WORKSPACE_SET(ws, WS_NEXT, (WORD)NOT_PROCESS_P);
	
	if(FPTR == (WORDPTR)NOT_PROCESS_P)
	{
		/* If there is nothing on the queue, we will make this process
		 * the only thing on the queue */
		FPTR = ws;
		BPTR = ws;
	}
	else
	{
		/* There are other things on the queue, we add this process to the 
		 * back pointer, and add a link to this process at into the previous
		 * processes workspace */
		WORKSPACE_SET(BPTR, WS_NEXT, (WORD)ws);
		BPTR = ws;
	}
}

static void add_to_queue_iptr(tvm_ectx_t *ectx, WORDPTR ws, BYTEPTR IPTR_prime)
{
	WORKSPACE_SET(BPTR, WS_IPTR, (WORD)IPTR_prime);
	
	ectx->add_to_queue(ws);
}

static void add_queue_to_queue(tvm_ectx_t *ectx, WORDPTR front, WORDPTR back)
{
	WORKSPACE_SET(back, WS_NEXT, (WORD)NOT_PROCESS_P);
	
	if(FPTR == (WORDPTR)NOT_PROCESS_P)
	{
		FPTR = front;
		BPTR = back;
	}
	else
	{
		WORKSPACE_SET(BPTR, WS_NEXT, (WORD)front);
		BPTR = back;
	}
}

static int run_next_on_queue(tvm_ectx_t *ectx)
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
	if((ectx->set_alarm && ectx->alarm) || ((!ectx->set_alarm) && (TPTR != (WORDPTR)NOT_PROCESS_P)))
	{
		WORD removed = 0;
		WORD now;
		
		ectx->alarm = 0;
		now = ectx->get_time();
#if 0
		/* Sanity check */
		if(TPTR == (WORDPTR)NOT_PROCESS_P)
		{
			printf("Got scheduler sync when TPTR = NOT_PROCESS_P\n");
			abort();
		}
#endif

		if((TPTR != (WORDPTR)NOT_PROCESS_P))
		{
			while((TPTR != (WORDPTR)NOT_PROCESS_P) && (!(TIME_AFTER(WORKSPACE_GET(TPTR, WS_TIMEOUT), now))))
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
				WORDPTR temp = TPTR;
				TPTR = (WORDPTR)WORKSPACE_GET(TPTR, WS_NEXT_T);
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
			/* We update TNEXT here (which is equivalent to calling ualarm in the occam
			 * code, they seem to use OS services rather than TNEXT though) */
			if((TPTR != (WORDPTR)NOT_PROCESS_P) && removed)
			{ 
				TNEXT = WORKSPACE_GET(TPTR, WS_TIMEOUT);
				if (tvm_set_alarm)
					tvm_set_alarm(TNEXT - now);
			}
		}
	}

	/* Any processes in the run queue? */
	if(FPTR != (WORDPTR)NOT_PROCESS_P)
	{
		/* yes */
		WPTR = FPTR;
	}
	else if(TPTR != (WORDPTR)NOT_PROCESS_P)
	{
		return ECTX_SLEEP;
	}
	else
	{
		return ECTX_EMPTY;
	}

	/* Update thet run queue by taking new current process off it */
	if(FPTR == BPTR)
	{
		FPTR = (WORDPTR)NOT_PROCESS_P;
		BPTR = (WORDPTR)NOT_PROCESS_P;
	}
	else
	{
		FPTR = (WORDPTR)WORKSPACE_GET(FPTR, WS_NEXT);
	}
	
	IPTR = (BYTEPTR)WORKSPACE_GET(WPTR, WS_IPTR);

	return ECTX_INS_OK;
}

/* PREREQUISITES:
 *   The workspace must be already have WS_TIMEOUT set correctly
 */
static void timer_queue_insert(tvm_ectx_t *ectx, WORD current_time, WORD reschedule_time)
{
	/* Check if the queue is empty */
	if(TPTR == (WORDPTR)NOT_PROCESS_P)
	{
		/* It was, insert ourselves as the only thing */
		TPTR = WPTR;
		/* Update the TNEXT value */
		/* TNEXT = WORKSPACE_GET(WPTR, WS_TIMEOUT); */
		/* This should work instead of the above line */
		TNEXT = reschedule_time;

		if (ectx->set_alarm)
			ectx->set_alarm(reschedule_time - current_time);

		WORKSPACE_SET(WPTR, WS_IPTR, (WORD)IPTR);
		WORKSPACE_SET(WPTR, WS_NEXT_T, NOT_PROCESS_P);
	}
	/* Check if we should be at the top of the queue */
	else if(TIME_AFTER(TNEXT, reschedule_time))
	{
		/* If the time in TNEXT is after our reschedule_time, then we should be at
		 * the front of the queue, so lets insert ourselves there. We know there
		 * is at least one other thing on the queue than us we need to insert
		 * ourselves before */

		/* Update our NEXT pointer to point to the previous head of the queue */
		WORKSPACE_SET(WPTR, WS_NEXT_T, (WORD)TPTR);
		/* Add us to the front pointer */
		TPTR  = WPTR;
		/* Set the new reschedule time in TNEXT */
		TNEXT = reschedule_time;
		
		if (ectx->set_alarm)
			ectx->set_alarm(reschedule_time - current_time);
	}
	else
	{
		/* No, things get a bit more complicated :( */

		/* Signal we are not done yet :) */
		int done = 0;
		/* Get the first workspace on the timer queue */
		WORDPTR this_WPTR = TPTR;
		/* Get the (first) next workspace pointer on the queue */
		WORDPTR next_WPTR = (WORDPTR)WORKSPACE_GET(this_WPTR, WS_NEXT_T);
	
		/* Now loop through the list */
		while(!done)
		{
			/* Are we at the end of the queue */
			if(next_WPTR == (WORDPTR)NOT_PROCESS_P)
			{
				/* Yes, insert us at the end, no update of TNEXT */

				/* Adjust the process at the end of the queue to point to us */
				WORKSPACE_SET(this_WPTR, WS_NEXT_T, (WORD)WPTR);
				/* Adjust ourselves to point to the end of the queue NOT_PROCESS_P */
				WORKSPACE_SET(WPTR, WS_NEXT_T, NOT_PROCESS_P);

				/* We are done */
				done = 1;
			}
			/* Do we need to insert ourselves after this process? */
			else if(TIME_AFTER(WORKSPACE_GET(next_WPTR, WS_TIMEOUT), reschedule_time))
			{
				/* If the reschedule time of the process after the one we are looing at,
				 * is TIME_AFTER the reschedule time we have, then we need to insert
				 * ourselves before that process (ie after the current process) */

				/* Insert ourselves after the process we are looking at */
				/* We are going to point to the next process in the queue */
				WORKSPACE_SET(WPTR, WS_NEXT_T, (WORD)next_WPTR);
				/* The current process in the queue is going to point to us */
				WORKSPACE_SET(this_WPTR, WS_NEXT_T, (WORD)WPTR);

				/* We are done */
				done = 1;
			}
			/* We need to dig deeper in the list */
			else
			{
				/* FIXME: Do we really need to get all these values every time round? */

				/* Get the next workspace on the timer queue */
				this_WPTR = next_WPTR;
				/* Get the (next) next workspace pointer on the queue */
				next_WPTR = (WORDPTR)WORKSPACE_GET(this_WPTR, WS_NEXT_T);
				/* We are NOT done!!! */
			}
		}
	}
}

void tvm_default_scheduler(tvm_ectx_t *ectx)
{
	ectx->add_to_queue 		= add_to_queue;
	/* ectx->add_to_queue_iptr	= add_to_queue_iptr; */
	ectx->add_queue_to_queue	= add_queue_to_queue;
	ectx->run_next_on_queue		= run_next_on_queue;
	ectx->timer_queue_insert	= timer_queue_insert;
}
