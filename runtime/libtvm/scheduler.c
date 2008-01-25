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

#include "transputer.h"
#include "instructions.h"
#include "interpreter.h"
#include "mem.h"
#include "timer.h"
#include "scheduler.h"

#ifdef ENABLE_SCHED_SYNC
#include <unistd.h>
#endif

int has_shutdown = 0;

#ifdef SCHEDULER_ENABLE_BUSYWAIT_HOOK
/* FIXME: Should this be somewehere else? */
void (*scheduler_busywait_hook)(void);
#endif
volatile int sched_sync;

/* FIXME: The add_to_queue macro in soccam is in the helpers.scm, though
 * it probably ought to live with the rest of the scheduler stuff in
 * scheduler.scm
 */
/* FIXME: I think this should probably take a WORDPTR as the first argument, and
 * possibly a BYTEPTR as the second */
void just_add_to_queue(WORD src_reg)
{

	/* 20070607 CGR */
	/* Carl points out that we were practicing bad hygene in our workspace when
	 * doing this operation. The TVM should always set the link pointer to null
	 * before adding to the queue. This line *probably* fixes that problem. */
	WORKSPACE_SET((WORDPTR)src_reg, WS_NEXT, (WORD)NOT_PROCESS_P);
	
	/* src_reg is effectively the new workspace pointer */

	if(fptr[pri] == (WORDPTR)NOT_PROCESS_P)
	{
		/* If there is nothing on the queue, we will make this process
		 * the only thing on the queue */
		fptr[pri] = (WORDPTR)src_reg;
		bptr[pri] = (WORDPTR)src_reg;
	}
	else
	{
		/* There are other things on the queue, we add this process to the 
		 * back pointer, and add a link to this process at into the previous
		 * processes workspace */
		/* FIXME: soccam does not use the workspace! macro here, it should! */
		WORKSPACE_SET(bptr[pri], WS_NEXT, src_reg);
		bptr[pri] = (WORDPTR)src_reg;
	}
}

void add_to_queue(WORD src_reg, WORD iptr_prime)
{
	just_add_to_queue(src_reg);
	
	WORKSPACE_SET(bptr[pri], WS_IPTR, iptr_prime);
}

void add_queue_to_queue(WORD front, WORD back)
{
	WORKSPACE_SET((WORDPTR)back, WS_NEXT, (WORD)NOT_PROCESS_P);
	
	if(fptr[pri] == (WORDPTR)NOT_PROCESS_P)
	{
		fptr[pri] = (WORDPTR)front;
		bptr[pri] = (WORDPTR)back;
	}
	else
	{
		WORKSPACE_SET(bptr[pri], WS_NEXT, front);
		bptr[pri] = (WORDPTR)back;
	}
}

/* FIXME: These should go elsewhere!!! */
#define TRUE 1
#define FALSE 0
BYTEPTR run_next_on_queue(void)
{
	//int loops = 0;
	int now;
	int removed;
	/*
	   ! SEQ
	   !   tim ? now
	   !   removed := FALSE
	   !   WHILE (Tptr <> NotProcess.p) AND (NOT (Tptr^[Time] AFTER now))
	   !     SEQ
	   !       ...  move first process on timer queue to the run queue
	   !       removed := TRUE
	   !   IF
	   !     (Tptr <> NotProcess.p) AND removed
	   !       ualarm (Tptr^[Time] MINUS now, 0)
	   !     TRUE
	   !       SKIP
	   */
	/*
	   printf("\\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/ \\/\n");
	   printf("ENTERED SCHEDULER!\n");
	   print_timer_queue();
	   print_scheduling_queue();
	   */
sched_start:
	/* This is a bit ugly at the moment... */
#ifdef ENABLE_SCHED_SYNC
	/* FIXME: This should expand to be able to deal with more than just timer
	 * syncs, which is what it does now */
	if(sched_sync == 1)
	{
		sched_sync = 0;
		now = get_time();
		removed = FALSE;
#if 0
		/* Sanity check */
		if(tptr[pri] == (WORDPTR)NOT_PROCESS_P)
		{
			printf("Got scheduler sync when tptr = NOT_PROCESS_P\n");
			abort();
		}
#endif

		{
#else
			/* FIXME: 20070607 CGR notes that this is expensive to ALWAYS read the clock.
			 * could it just go inside the IF statement? That's just a quick glance.*/
			now = get_time();
			removed = FALSE;

			if((tptr[pri] != (WORDPTR)NOT_PROCESS_P))
			{
				while((tptr[pri] != (WORDPTR)NOT_PROCESS_P) && (!(AFTER(WORKSPACE_GET(tptr[pri], WS_TIMEOUT), now))))
				{
#endif
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
					WORDPTR temp = tptr[pri];
					tptr[pri] = (WORDPTR)WORKSPACE_GET(tptr[pri], WS_NEXT_T);
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
						just_add_to_queue((WORD)temp);
					}
					/*
					   print_timer_queue();
					   print_scheduling_queue();
					   */
					removed = TRUE;
				}
				/* We update tnext here (which is equivalent to calling ualarm in the occam
				 * code, they seem to use OS services rather than tnext though) */
				if(tptr[pri] != (WORDPTR)NOT_PROCESS_P && removed)
				{ 
					tnext[pri] = WORKSPACE_GET(tptr[pri], WS_TIMEOUT);
#ifdef ENABLE_SCHED_SYNC
					set_alarm(tnext[pri] - now);
#endif
				}
			}

			/* Any processes in the run queue? */
			if(fptr[pri] != (WORDPTR)NOT_PROCESS_P)
			{
				/* yes */
				/*printf("SCHEDULED NEW PROCESS!\n");*/
				wptr = fptr[pri];
			}
			else if(tptr[pri] != (WORDPTR)NOT_PROCESS_P)
			{
#ifndef BUSY_WAIT
				/* It would be nice to get rid of the timer tests at the start of the kernel
				 * and put them here, which I think is ok... so they dont happen everytime
				 * we go into the kernel? Will have to think a bit more about this
				 * perhaps, and check its ok, but it would seem sensible???
				 */
				/* This function sleeps until a time close to when the next process is ready
				*/
#	ifdef ENABLE_SCHED_SYNC
				/* FIXME: The pause business should probably just be rolled into
				 * tvm_sleep() */
				pause();
#else
				tvm_sleep();
#	endif
#endif
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
			/*printf("LOOPED %d TIMES\n", loops);*/

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
			//iptr = (BYTEPTR)WORKSPACE_GET(wptr, WS_IPTR);
			return (BYTEPTR)WORKSPACE_GET(wptr, WS_IPTR);
			/*printf("/\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\ /\\        /\\\n");*/
}

