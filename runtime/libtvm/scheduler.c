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
#include "interpreter.h"
#include "scheduler.h"
#include "pthread.h"
#include "stdio.h"

#define NUMCORE 42

static pthread_t threads[NUMCORE];

int thread_count=1;

static void add_to_queue(ECTX ectx, WORDPTR ws)
{

	WORKSPACE_SET(ws, WS_LINK, (WORD)NOT_PROCESS_P);

        
	
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
		WORKSPACE_SET(BPTR, WS_LINK, (WORD)ws);
		BPTR = ws;
	}

        /* create new thread  */
        printf("Creating thread %d\n", thread_count);

        int res = pthread_create(&threads[thread_count], NULL, ectx->spawn_hook, (void *)thread_count);
          
        if (res) {
            printf("THREAD CREATE FAILED [%d]\n", res);
            
        }

        printf("Created thread %d\n", thread_count);

        thread_count++;

}

static int add_to_queue_external(ECTX ectx, ECTX src_ctx, WORDPTR ws)
{
	ectx->add_to_queue(ectx, ws);

	if(src_ctx != NULL)
	{	
		if(ectx->pri < src_ctx->pri)
		{
			return ECTX_PREEMPT;
		}
	}
	
	return ECTX_CONTINUE;
}

static void add_queue_to_queue(ECTX ectx, WORDPTR front, WORDPTR back)
{
	WORKSPACE_SET(back, WS_LINK, (WORD)NOT_PROCESS_P);
	
	if(FPTR == (WORDPTR)NOT_PROCESS_P)
	{
		FPTR = front;
		BPTR = back;
	}
	else
	{
		WORKSPACE_SET(BPTR, WS_LINK, (WORD)front);
		BPTR = back;
	}
}

static void busy_wait_set_alarm(ECTX ectx)
{
	ectx->modify_sync_flags(ectx, SFLAG_TQ, 0);
}

static int busy_wait_synchronise(ECTX ectx)
{
	if(ectx->sflags & SFLAG_INTR)
	{
		return ECTX_INTERRUPT;
	}
	else if(ectx->sflags & SFLAG_TQ)
	{
		WORD now = ectx->get_time(ectx);
		
		ectx->walk_timer_queue(ectx, now);

		if(TPTR == (WORDPTR)NOT_PROCESS_P)
		{
			ectx->modify_sync_flags(ectx, 0, SFLAG_TQ);
		}

		return ECTX_CONTINUE;
	}
	else
	{
		return ECTX_ERROR;
	}
}

static void modify_sync_flags(ECTX ectx, WORD set, WORD clear)
{
	ectx->sflags = (ectx->sflags & (~clear)) | set;
}

static int run_next_on_queue(ECTX ectx)
{
	/* Synchronise with environment, timers, etc */
	if(ectx->sflags)
	{
		int ret = ectx->synchronise(ectx);
		if(ret)
		{
			WPTR = (WORDPTR)NOT_PROCESS_P;
			return ret;
		}
	}

	/* Is there something to run? */
	if(FPTR != (WORDPTR)NOT_PROCESS_P)
	{
		/* Yes. */
		WPTR = FPTR;
	} 
	else
	{
		/* No, clear WPTR and exit tvm_run. */
		WPTR = (WORDPTR)NOT_PROCESS_P;

		if(TPTR != (WORDPTR)NOT_PROCESS_P)
		{
			/* We are just sleeping, not dead. */
			return ECTX_SLEEP;
		}
		else
		{
			/* This looks like deadlock... */
			return ECTX_EMPTY;
		}
	}

	/* Update thet run queue by taking new current process off it. */
	if(FPTR == BPTR)
	{
		FPTR = (WORDPTR)NOT_PROCESS_P;
		BPTR = (WORDPTR)NOT_PROCESS_P;
	}
	else
	{
		FPTR = (WORDPTR)WORKSPACE_GET(FPTR, WS_LINK);
	}

	/* Load instruction pointer from workspace. */
	IPTR = (BYTEPTR)WORKSPACE_GET(WPTR, WS_IPTR);

	return ECTX_CONTINUE;
}

static int set_error_flag(ECTX ectx, WORD flag)
{
	ectx->eflags |= flag;
	return ECTX_ERROR;
}

/* PREREQUISITES:
 *   The workspace must be already have WS_TIME set correctly
 */
static void timer_queue_insert(ECTX ectx, WORDPTR ws, WORD current_time, WORD reschedule_time)
{
	/* Check if the queue is empty */
	if(TPTR == (WORDPTR)NOT_PROCESS_P)
	{
		/* It was, insert ourselves as the only thing */
		TPTR = ws;
		/* Update the TNEXT value */
		/* TNEXT = WORKSPACE_GET(WPTR, WS_TIME); */
		/* This should work instead of the above line */
		TNEXT = reschedule_time;

		WORKSPACE_SET(ws, WS_IPTR, (WORD)IPTR);
		WORKSPACE_SET(ws, WS_TLINK, NOT_PROCESS_P);

		ectx->set_alarm(ectx);
	}
	/* Check if we should be at the top of the queue */
	else if(TIME_AFTER(TNEXT, reschedule_time))
	{
		/* If the time in TNEXT is after our reschedule_time, then we should be at
		 * the front of the queue, so lets insert ourselves there. We know there
		 * is at least one other thing on the queue than us we need to insert
		 * ourselves before */

		/* Update our NEXT pointer to point to the previous head of the queue */
		WORKSPACE_SET(ws, WS_TLINK, (WORD)TPTR);
		/* Add us to the front pointer */
		TPTR  = ws;
		/* Set the new reschedule time in TNEXT */
		TNEXT = reschedule_time;
		
		ectx->set_alarm(ectx);
	}
	else
	{
		/* Get the first workspace on the timer queue */
		WORDPTR this_ws = TPTR;
		/* Get the (first) next workspace pointer on the queue */
		WORDPTR next_ws = (WORDPTR)WORKSPACE_GET(this_ws, WS_TLINK);
	
		/* Now loop through the list */
		for(;;)
		{
			/* Are we at the end of the queue */
			if(next_ws == (WORDPTR)NOT_PROCESS_P)
			{
				/* Yes, insert us at the end, no update of TNEXT */

				/* Adjust the process at the end of the queue to point to us */
				WORKSPACE_SET(this_ws, WS_TLINK, (WORD)ws);
				/* Adjust ourselves to point to the end of the queue NOT_PROCESS_P */
				WORKSPACE_SET(ws, WS_TLINK, NOT_PROCESS_P);

				/* We are done */
				break;
			}
			/* Do we need to insert ourselves after this process? */
			else if(TIME_AFTER(WORKSPACE_GET(next_ws, WS_TIME), reschedule_time))
			{
				/* If the reschedule time of the process after the one we are looing at,
				 * is TIME_AFTER the reschedule time we have, then we need to insert
				 * ourselves before that process (ie after the current process) */

				/* Insert ourselves after the process we are looking at */
				/* We are going to point to the next process in the queue */
				WORKSPACE_SET(ws, WS_TLINK, (WORD)next_ws);
				/* The current process in the queue is going to point to us */
				WORKSPACE_SET(this_ws, WS_TLINK, (WORD)ws);

				/* We are done */
				break;
			}
			/* We need to dig deeper in the list */
			else
			{
				/* Get the next workspace on the timer queue */
				this_ws = next_ws;
				/* Get the (next) next workspace pointer on the queue */
				next_ws = (WORDPTR)WORKSPACE_GET(this_ws, WS_TLINK);
				/* We are NOT done!!! */
			}
		}
	}
}

static void timer_queue_remove(ECTX ectx, WORDPTR ws)
{
	/* Head of the timer queue? */
	if(TPTR == ws)
	{
		TPTR = (WORDPTR) WORKSPACE_GET((WORDPTR)TPTR, WS_TLINK);
		if(TPTR != (WORDPTR)NOT_PROCESS_P)
		{
			TNEXT = WORKSPACE_GET((WORDPTR)TPTR, WS_TIME);
			ectx->set_alarm(ectx);
		}
	}
	else if(TPTR != NOT_PROCESS_P)
	{
		WORDPTR previous = TPTR;
		WORDPTR current = (WORDPTR)WORKSPACE_GET(previous, WS_TLINK);
		while(current != (WORDPTR)NOT_PROCESS_P)
		{
			if(current == ws)
			{
				WORKSPACE_SET(previous, WS_TLINK, WORKSPACE_GET(ws, WS_TLINK));
				break;
			}
			else
			{
				previous = current;
				current = (WORDPTR)WORKSPACE_GET(current, WS_TLINK);
			}
		}
	}

	WORKSPACE_SET(ws, WS_TLINK, TIME_NOT_SET_P); /* Timeout canceled */
}

static void walk_timer_queue(ECTX ectx, WORD now)
{
	WORDPTR tptr 	= TPTR;
	WORD	tnext	= TNEXT;

	if(tptr == (WORDPTR)NOT_PROCESS_P || TIME_AFTER(tnext, now))
	{
		/* Timer queue empty or nothing expired. */
		return;
	}

	/* Since we are past tnext we can blindly remove the front
	 * of the timer queue before doing anymore testing.
	 */

	do {
		WORDPTR next = (WORDPTR)WORKSPACE_GET(tptr, WS_TLINK);
		
		WORKSPACE_SET(tptr, WS_TLINK, TIME_SET_P);
		WORKSPACE_SET(tptr, WS_TIME, now);

		if(WORKSPACE_GET(tptr, WS_STATE) != READY_P)
		{
			WORKSPACE_SET(tptr, WS_STATE, READY_P);
			ADD_TO_QUEUE(tptr);
		}

		if((tptr = next) == (WORDPTR)NOT_PROCESS_P)
		{
			/* Queue became empty, clean-up and leave. */
			TPTR = (WORDPTR)NOT_PROCESS_P;
			return;
		}

		tnext = WORKSPACE_GET(tptr, WS_TIME);

	} while(!TIME_AFTER(tnext, now));

	/* If we got here the queue is not empty.
	 * Write back the new head and timeout,
	 * maybe set a new alarm.
	 */

	TPTR	= tptr;
	TNEXT	= tnext;

	ectx->set_alarm(ectx);
}

void _tvm_install_scheduler(ECTX ectx)
{
	ectx->add_to_queue 		= add_to_queue;
	ectx->add_to_queue_external	= add_to_queue_external;
	ectx->add_queue_to_queue	= add_queue_to_queue;
	ectx->modify_sync_flags		= modify_sync_flags;
	ectx->run_next_on_queue		= run_next_on_queue;
	ectx->set_error_flag		= set_error_flag;
	ectx->synchronise		= busy_wait_synchronise;
	ectx->timer_queue_insert	= timer_queue_insert;
	ectx->timer_queue_remove	= timer_queue_remove;
	ectx->walk_timer_queue		= walk_timer_queue;
	ectx->set_alarm			= busy_wait_set_alarm;
	#ifdef TVM_CUSTOM_MEM_OPS
	ectx->memcpy			= _tvm_memcpy;
	ectx->memset			= _tvm_memset;
	#endif
}

