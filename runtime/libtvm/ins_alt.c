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

#include "transputer.h"
#include "instructions.h"
#include "interpreter.h"
#include "scheduler.h"
#include "mem.h"
#include "timer.h"
#include "ins_alt.h"

/****************************************************************************
 *              0x22 0xF_         0x22 0xF_         0x22 0xF_               *
 ****************************************************************************/

/* 0x2E - 0x22 0xFE - dist - disable timer */
TVM_INSTRUCTION void ins_dist(void)
{
	/*
	int fired = (((breg != 0) && (WORKSPACE_GET(wptr, WS_TOP) == NONE_SELECTED_O)) &&
		AFTER(current_time, creg));// && (WORKSPACE_GET(wptr, WS_NEXT_T) == TIME_SET_P));
	*/

	/*
	goto one;

one:
	*/
	if((!breg) || (WORKSPACE_GET(wptr, WS_NEXT_T) == TIME_NOT_SET_P))
	{
		STACK(0, UNDEFINE(breg), UNDEFINE(creg));
	}
	else if(WORKSPACE_GET(wptr, WS_NEXT_T) == TIME_SET_P)
	{
		if((!AFTER(WORKSPACE_GET(wptr, WS_TIMEOUT), creg)) || 
				(WORKSPACE_GET(wptr, WS_TOP) != NONE_SELECTED_O))
		{
			STACK(0, UNDEFINE(breg), UNDEFINE(creg));
		}
		else
		{
			WORKSPACE_SET(wptr, WS_TOP, areg);
			STACK(1, UNDEFINE(breg), UNDEFINE(creg));
		}
	}
	else
	{
		STACK(0, UNDEFINE(breg), UNDEFINE(creg));

		/*
		printf("PHANTOM REMOVAL:\n");
		print_registers();
		print_timer_queue();
		*/
	
		/* Are we the head of the timerqueue */
		if(tptr[pri] == wptr)
		{
			tptr[pri] = (POOTER) WORKSPACE_GET((POOTER)tptr[pri], WS_NEXT_T);
			/* If we were the last thing on the queue, this should not happen */
			if(tptr[pri] != (POOTER)NOT_PROCESS_P)
			{
				tnext[pri] = WORKSPACE_GET((POOTER)tptr[pri], WS_TIMEOUT);
#ifdef ENABLE_SCHED_SYNC
				set_alarm(tnext[pri] - get_time());
#endif
			}
		}
		/* We could check that tptr[pri] == NOT_PROCESS_P but this case SHOULD be
		 * impossible! So we dont bother */
		else if(tptr[pri] == (POOTER)NOT_PROCESS_P)
		{
		}
		else
		{
			POOTER previous = tptr[pri];
			POOTER current = (POOTER)WORKSPACE_GET((POOTER)tptr[pri], WS_NEXT_T);
			while((current != (POOTER)NOT_PROCESS_P) && (current != wptr))
			{
				previous = current;
				current =(POOTER) WORKSPACE_GET(current, WS_NEXT_T);
			}
			/* There is a sanity test we could do here to check if current is
			 * NOT_PROCESS_P, this is impossible though so we are not going to */
			WORKSPACE_SET(previous, WS_NEXT_T, WORKSPACE_GET(wptr, WS_NEXT_T));
		}

		WORKSPACE_SET(wptr, WS_NEXT_T, TIME_NOT_SET_P); /* Timeout canceled */
		/*print_timer_queue();*/
	}

	return;

#if 0


//two:

	if(fired)
	{
		/* We fired, set the top of the workspace to the address of code to run */
		WORKSPACE_SET(wptr, WS_TOP, areg);
	}
	else
	{

	/* Aditionally */
	//if((breg != 0) && (BEFORE(current_time, creg)) && (tptr[pri] != (POOTER)NOT_PROCESS_P))
	if(tptr[pri] != (POOTER)NOT_PROCESS_P)
	{
		POOTER loop_wptr = tptr[pri];
		POOTER loop_next = (POOTER)WORKSPACE_GET(tptr[pri], WS_NEXT_T);
		POOTER loop_prev = 0;
		fprintf(stdout, "Phantom removal:\n");
		
		//print_timer_queue();
ins_dist_loop:
		fprintf(stdout, "---\n");
		//fprintf(stdout, "  wptr:      0x%08x\n", wptr);
		//fprintf(stdout, "  loop_next: 0x%08x\n", loop_next);
		//fprintf(stdout, "  loop_prev: 0x%08x\n", loop_prev);
		if((loop_wptr == tptr[pri]) && (loop_wptr == wptr))
		{
			/* HEAD CHECK */
			/* We are at the head of the list */
			if(loop_next == (POOTER)NOT_PROCESS_P)
			{
				/* If there is nobody after this process, we can nuke the tptr */
				tptr[pri] = (POOTER)NOT_PROCESS_P;
			}
			else
			{
				/* If there are others after this process, we need to put them at the
				 * head of the queue, and also set the timeout to their timeout value.
				 */
				tptr[pri] = (POOTER)WORKSPACE_GET(loop_wptr, WS_NEXT_T);
				tnext[pri] = WORKSPACE_GET(tptr[pri], WS_TIMEOUT);
			}
		}
		else if(loop_wptr == wptr)
		{
			/* TERMINATION */
			/* If we find ourselves on the queue... */
			/* FIXME: check this is correct */
			/* ... take us out of the queue */
			WORKSPACE_SET(loop_prev, WS_NEXT_T, WORKSPACE_GET(loop_wptr, WS_NEXT_T));
		}
		else if(loop_next == (POOTER)NOT_PROCESS_P)
		{
			/* AT THE END OF THE LIST */
			/* Do nothing */
		}
		else
		{
			/* KEEP LOOKING */
			loop_prev = loop_wptr; /* FIXME: different order from soccam */
			loop_wptr = (POOTER)WORKSPACE_GET(loop_wptr, WS_NEXT_T);
			loop_next = (POOTER)WORKSPACE_GET(loop_wptr, WS_NEXT_T); /* FIXME: different from soccam */
			goto ins_dist_loop;
		}
		//print_timer_queue();
	}
	}

	STACK(fired, UNDEFINE(breg), UNDEFINE(creg));
#endif
}

/* 0x2F - 0x22 0xFF - disc - disable channel */
TVM_INSTRUCTION void ins_disc(void)
{
	WORD wptr_deref = WORKSPACE_GET(wptr, WS_TOP);
	WORD creg_deref = read_mem((POOTER)creg);
	/* FIXME: There dont seem that much point in doing this the same way
	 * as with soccam, ie by hacing the fired variable, as we dont seem to 
	 * gain anything, as we still need to do the test in a big ig. Ie we
	 * might therefore as well do the test in the if, and do the action
	 * depending on the result of that, rather than storing the result of
	 * the if in a variable and then doing another if to do the work */
	int fired = 0;

	/* If fired? */
	if((breg != 0) && 
			(wptr_deref == NONE_SELECTED_O) && 
			(creg_deref != NOT_PROCESS_P) && 
			((POOTER)creg_deref != wptr))
	{
		WORKSPACE_SET(wptr, WS_TOP, areg);
		fired = 1;
	}

	/* Additionally
	 * If this test is true, then it implies we are looking
	 * at ourselves. So, we should destroy evidence of 
	 * a waiting process (US!) before doing everything else.
	 */
	if((breg != 0) && ((POOTER)creg_deref == wptr))
	{
		/* FIXME: Here and in soccam would it be more clear to use 
		 * wptr rather than creg??? */
		write_mem((POOTER)creg, NOT_PROCESS_P);
	}

	STACK((WORD)fired, UNDEFINE(breg), UNDEFINE(creg));
}





/****************************************************************************
 *              0x23 0xF_         0x23 0xF_         0x23 0xF_               *
 ****************************************************************************/

/* 0x30 - 0x23 0xF0 - diss - disable skip */
TVM_INSTRUCTION void ins_diss(void)
{
	/* FIXME: ditto as for disc */
	int fired = 0;

	/* Fired? */
	if((breg != 0) && (WORKSPACE_GET(wptr, WS_TOP) == NONE_SELECTED_O))
	{
		WORKSPACE_SET(wptr, WS_TOP, areg);
		fired = 1;
	}

	STACK((WORD)fired, creg, UNDEFINE(creg));
}





/****************************************************************************
 *              0x24 0xF_         0x24 0xF_         0x24 0xF_               *
 ****************************************************************************/

/* 0x43 - 0x24 0xF3 - alt - alt start */
TVM_INSTRUCTION void ins_alt(void)
{
	/* Set the alt state as enabeling */
	WORKSPACE_SET(wptr, WS_ALT_STATE, ENABLING_P);
}

/* 0x44 - 0x24 0xF4 - altwt - alt wait */
TVM_INSTRUCTION void ins_altwt(void)
{
	/* FIXME: in both soccam and here, the set of WS_TOP happens in both branches,
	 * so it should be taken out of the if.
	 * */
	/* DISABLING_P is also (in the T9000 book) READY_P, so should we use that
	 * instead? 
	 */ 
	if(WORKSPACE_GET(wptr, WS_ALT_STATE) == DISABLING_P)
	{
		WORKSPACE_SET(wptr, WS_TOP, NONE_SELECTED_O);
	}
	else
	{
		WORKSPACE_SET(wptr, WS_ALT_STATE, WAITING_P);
		WORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);
		WORKSPACE_SET(wptr, WS_TOP, NONE_SELECTED_O);
		/* FIXME: Soccam contains a warning here questioning the need to 
		 * reschedule here, considering that things seem to work, I would have
		 * thought that the reschedule is good! */
		iptr = run_next_on_queue();
	}
	//STACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(breg));
}

/* 0x45 - 0x24 0xF5 - altend - alt end */
TVM_INSTRUCTION void ins_altend(void)
{
	/* Add the jump offset which has been stored at the top of the workspace by
	 * one of the disabeling instructions to the current iptr */
	iptr = bpooter_plus(iptr, WORKSPACE_GET(wptr, WS_TOP));
}

/* 0x47 - 0x24 0xF7 - enbt - enable timer */
TVM_INSTRUCTION void ins_enbt(void)
{
	/* FIXME: This is badly coded in soccam and here */
	/* FIXME: There is a redundant STACK macro call */
	if(areg == 0)
	{
		/* The quard is disabled, do nothing apart from changeing the stack */
		//STACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));
	}
	else
	{
		if(WORKSPACE_GET(wptr, WS_ALT_T) == TIME_NOT_SET_P)
		{
			/* If nobody else has set a timeout in this alt yet, set one */
			WORKSPACE_SET(wptr, WS_ALT_T, TIME_SET_P);
			WORKSPACE_SET(wptr, WS_TIMEOUT, breg);
		}
		else if(AFTER(WORKSPACE_GET(wptr, WS_TIMEOUT), breg))
		{
			/* Otherwise if the timeout of this enbt is earlier then the stored one */
			WORKSPACE_SET(wptr, WS_TIMEOUT, breg);
		}
		else
		{
			/* Do nothing I guess... */
		}
	}
	
	/* Remove the time in breg from the stack */
	STACK(areg, creg, UNDEFINE(creg));
}

/* 0x48 - 0x24 0xF8 - enbc - enable channel */
TVM_INSTRUCTION void ins_enbc(void)
{
	/* Is guard enabled? if not, do nothing */
	/* FIXME: This is the other way around from how the test is done in soccam */
	if(areg != 0)
	{
		if(read_mem((POOTER)breg) == NOT_PROCESS_P)
		{
			write_mem((POOTER)breg, (WORD)wptr);
		}
		else
		{
			if((POOTER)read_mem((POOTER)breg) == wptr)
			{
				/* Another guard of the current process is waiting
				 * on the channnel; do nothing.
				 */
			}
			else
			{
				/* another process is waiting on the channel, so set a 
				 * flag to show that the guard is ready
				 */
				WORKSPACE_SET(wptr, WS_ALT_STATE, DISABLING_P);
			}
		}
	}

	STACK(areg, creg, UNDEFINE(creg));
}

/* 0x49 - 0x24 0xF9 - enbs - enable skip */
TVM_INSTRUCTION void ins_enbs(void)
{
	/* The stack is unaffected by this instruction */

	/* FIXME: in soccam and here, this does the test differently
	 * than in enbc */
	if(areg == 1)
	{
		WORKSPACE_SET(wptr, WS_ALT_STATE, DISABLING_P);
	}
}

/* 0x4E - 0x24 0xFE - talt - timer alt start */
TVM_INSTRUCTION void ins_talt(void)
{
	/* Set the alt state as enabeling */
	WORKSPACE_SET(wptr, WS_ALT_STATE, ENABLING_P);
	/* Set up the timer */
	WORKSPACE_SET(wptr, WS_ALT_T, TIME_NOT_SET_P);
}





/****************************************************************************
 *              0x25 0xF_         0x25 0xF_         0x25 0xF_               *
 ****************************************************************************/

/* 0x51 - 0x25 0xF1 - taltwt - timer alt wait */
TVM_INSTRUCTION void ins_taltwt(void)
{
	int current_time = get_time();

	/* Set the top of the workspace to -1 */
	WORKSPACE_SET(wptr, WS_TOP, NONE_SELECTED_O);

	if(WORKSPACE_GET(wptr, WS_ALT_STATE) == DISABLING_P) /* READY_P */
	{
		/*fprintf(stderr, "1");*/
		WORKSPACE_SET(wptr, WS_TIMEOUT, current_time);
	}
	else if(WORKSPACE_GET(wptr, WS_ALT_T) == TIME_NOT_SET_P)
	{
		/*fprintf(stderr, "2");*/
		WORKSPACE_SET(wptr, WS_ALT_STATE, WAITING_P);

		WORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);
		iptr = run_next_on_queue();
	}
	/* Redundant if? */
	else if(AFTER(current_time, WORKSPACE_GET(wptr, WS_TIMEOUT)))
	{
		/*fprintf(stderr, "3");*/
		WORKSPACE_SET(wptr, WS_ALT_STATE, DISABLING_P); /* READY_P */
		WORKSPACE_SET(wptr, WS_TIMEOUT, current_time);
	}
	else
	{
		/*fprintf(stderr, "4");*/
		/*printf("TALT added stuff to timer queue\n");*/
		WORKSPACE_SET(wptr, WS_TIMEOUT, WORKSPACE_GET(wptr, WS_TIMEOUT) + 1);

		/*traverse_and_insert(tptr[pri], tptr[pri]);*/
		timer_queue_insert(current_time, WORKSPACE_GET(wptr, WS_TIMEOUT));
		/*print_timer_queue();*/
		/* Redundant if? */
		if(WORKSPACE_GET(wptr, WS_ALT_STATE) != DISABLING_P)
		{
			WORKSPACE_SET(wptr, WS_ALT_STATE, WAITING_P);

			WORKSPACE_SET(wptr, WS_IPTR, (WORD)iptr);
			iptr = run_next_on_queue();
		}
		else
		{
			set_error_flag(EFLAG_ALT);
		}
	}

	/*printf("STATE AFTER TALT:\n");*/
	/*print_state();*/
}


