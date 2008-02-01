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
	if((!BREG) || (WORKSPACE_GET(WPTR, WS_TLINK) == TIME_NOT_SET_P))
	{
		STACK_RET(0, UNDEFINE(BREG), UNDEFINE(CREG));
	}
	else if(WORKSPACE_GET(WPTR, WS_TLINK) == TIME_SET_P)
	{
		if((!TIME_AFTER(WORKSPACE_GET(WPTR, WS_TIME), CREG)) || 
				(WORKSPACE_GET(WPTR, WS_TEMP) != NONE_SELECTED_O))
		{
			STACK_RET(0, UNDEFINE(BREG), UNDEFINE(CREG));
		}
		else
		{
			WORKSPACE_SET(WPTR, WS_TEMP, AREG);
			STACK_RET(1, UNDEFINE(BREG), UNDEFINE(CREG));
		}
	}
	else
	{
		/* Are we the head of the timerqueue */
		if(TPTR == WPTR)
		{
			TPTR = (WORDPTR) WORKSPACE_GET((WORDPTR)TPTR, WS_TLINK);
			/* If we were the last thing on the queue, this should not happen */
			if(TPTR != (WORDPTR)NOT_PROCESS_P)
			{
				TNEXT = WORKSPACE_GET((WORDPTR)TPTR, WS_TIME);
				ectx->set_alarm(ectx);
			}
		}
		/* We could check that TPTR == NOT_PROCESS_P but this case SHOULD be
		 * impossible! So we dont bother */
		else if(TPTR == (WORDPTR)NOT_PROCESS_P)
		{
		}
		else
		{
			WORDPTR previous = TPTR;
			WORDPTR current = (WORDPTR)WORKSPACE_GET((WORDPTR)TPTR, WS_TLINK);
			while((current != (WORDPTR)NOT_PROCESS_P) && (current != WPTR))
			{
				previous = current;
				current =(WORDPTR) WORKSPACE_GET(current, WS_TLINK);
			}
			/* There is a sanity test we could do here to check if current is
			 * NOT_PROCESS_P, this is impossible though so we are not going to */
			WORKSPACE_SET(previous, WS_TLINK, WORKSPACE_GET(WPTR, WS_TLINK));
		}

		WORKSPACE_SET(WPTR, WS_TLINK, TIME_NOT_SET_P); /* Timeout canceled */

		STACK_RET(0, UNDEFINE(BREG), UNDEFINE(CREG));
	}
}

/* 0x2F - 0x22 0xFF - disc - disable channel */
TVM_INSTRUCTION (ins_disc)
{
	WORD WPTR_deref = WORKSPACE_GET(WPTR, WS_TEMP);
	WORD CREG_deref = read_word((WORDPTR)CREG);
	/* FIXME: There dont seem that much point in doing this the same way
	 * as with soccam, ie by hacing the fired variable, as we dont seem to 
	 * gain anything, as we still need to do the test in a big ig. Ie we
	 * might therefore as well do the test in the if, and do the action
	 * depending on the result of that, rather than storing the result of
	 * the if in a variable and then doing another if to do the work */
	int fired = 0;

	/* If fired? */
	if((BREG != 0) && 
			(WPTR_deref == NONE_SELECTED_O) && 
			(CREG_deref != NOT_PROCESS_P) && 
			((WORDPTR)CREG_deref != WPTR))
	{
		WORKSPACE_SET(WPTR, WS_TEMP, AREG);
		fired = 1;
	}

	/* Additionally
	 * If this test is true, then it implies we are looking
	 * at ourselves. So, we should destroy evidence of 
	 * a waiting process (US!) before doing everything else.
	 */
	if((BREG != 0) && ((WORDPTR)CREG_deref == WPTR))
	{
		/* FIXME: Here and in soccam would it be more clear to use 
		 * WPTR rather than CREG??? */
		write_word((WORDPTR)CREG, NOT_PROCESS_P);
	}

	STACK_RET((WORD)fired, UNDEFINE(BREG), UNDEFINE(CREG));
}





/****************************************************************************
 *              0x23 0xF_         0x23 0xF_         0x23 0xF_               *
 ****************************************************************************/

/* 0x30 - 0x23 0xF0 - diss - disable skip */
TVM_INSTRUCTION (ins_diss)
{
	/* FIXME: ditto as for disc */
	int fired = 0;

	/* Fired? */
	if((BREG != 0) && (WORKSPACE_GET(WPTR, WS_TEMP) == NONE_SELECTED_O))
	{
		WORKSPACE_SET(WPTR, WS_TEMP, AREG);
		fired = 1;
	}

	STACK_RET((WORD)fired, CREG, UNDEFINE(CREG));
}





/****************************************************************************
 *              0x24 0xF_         0x24 0xF_         0x24 0xF_               *
 ****************************************************************************/

/* 0x43 - 0x24 0xF3 - alt - alt start */
TVM_INSTRUCTION (ins_alt)
{
	/* Set the alt state as enabeling */
	WORKSPACE_SET(WPTR, WS_STATE, ENABLING_P);
	return ECTX_CONTINUE;
}

/* 0x44 - 0x24 0xF4 - altwt - alt wait */
TVM_INSTRUCTION (ins_altwt)
{
	/* FIXME: in both soccam and here, the set of WS_TEMP happens in both branches,
	 * so it should be taken out of the if.
	 * */
	/* DISABLING_P is also (in the T9000 book) READY_P, so should we use that
	 * instead? 
	 */ 
	if(WORKSPACE_GET(WPTR, WS_STATE) == DISABLING_P)
	{
		WORKSPACE_SET(WPTR, WS_TEMP, NONE_SELECTED_O);
		UNDEFINE_STACK_RET();
	}
	else
	{
		WORKSPACE_SET(WPTR, WS_STATE, WAITING_P);
		WORKSPACE_SET(WPTR, WS_IPTR, (WORD)IPTR);
		WORKSPACE_SET(WPTR, WS_TEMP, NONE_SELECTED_O);
		RUN_NEXT_ON_QUEUE_RET();
	}
}

/* 0x45 - 0x24 0xF5 - altend - alt end */
TVM_INSTRUCTION (ins_altend)
{
	/* Add the jump offset which has been stored at the top of the workspace by
	 * one of the disabeling instructions to the current IPTR */
	IPTR = byteptr_plus(IPTR, WORKSPACE_GET(WPTR, WS_TEMP));
	return ECTX_CONTINUE;
}

/* 0x47 - 0x24 0xF7 - enbt - enable timer */
TVM_INSTRUCTION (ins_enbt)
{
	/* FIXME: This is badly coded in soccam and here */
	/* FIXME: There is a redundant STACK macro call */
	if(AREG == 0)
	{
		/* The quard is disabled, do nothing apart from changeing the stack */
		//STACK(UNDEFINE(AREG), UNDEFINE(BREG), UNDEFINE(CREG));
	}
	else
	{
		if(WORKSPACE_GET(WPTR, WS_TLINK) == TIME_NOT_SET_P)
		{
			/* If nobody else has set a timeout in this alt yet, set one */
			WORKSPACE_SET(WPTR, WS_TLINK, TIME_SET_P);
			WORKSPACE_SET(WPTR, WS_TIME, BREG);
		}
		else if(TIME_AFTER(WORKSPACE_GET(WPTR, WS_TIME), BREG))
		{
			/* Otherwise if the timeout of this enbt is earlier then the stored one */
			WORKSPACE_SET(WPTR, WS_TIME, BREG);
		}
		else
		{
			/* Do nothing I guess... */
		}
	}
	
	/* Remove the time in BREG from the stack */
	STACK_RET(AREG, CREG, UNDEFINE(CREG));
}

/* 0x48 - 0x24 0xF8 - enbc - enable channel */
TVM_INSTRUCTION (ins_enbc)
{
	/* Is guard enabled? if not, do nothing */
	/* FIXME: This is the other way around from how the test is done in soccam */
	if(AREG != 0)
	{
		if(read_word((WORDPTR)BREG) == NOT_PROCESS_P)
		{
			write_word((WORDPTR)BREG, (WORD)WPTR);
		}
		else
		{
			if((WORDPTR)read_word((WORDPTR)BREG) == WPTR)
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
				WORKSPACE_SET(WPTR, WS_STATE, DISABLING_P);
			}
		}
	}

	STACK_RET(AREG, CREG, UNDEFINE(CREG));
}

/* 0x49 - 0x24 0xF9 - enbs - enable skip */
TVM_INSTRUCTION (ins_enbs)
{
	/* The stack is unaffected by this instruction */

	/* FIXME: in soccam and here, this does the test differently
	 * than in enbc */
	if(AREG == 1)
	{
		WORKSPACE_SET(WPTR, WS_STATE, DISABLING_P);
	}
	return ECTX_CONTINUE;
}

/* 0x4E - 0x24 0xFE - talt - timer alt start */
TVM_INSTRUCTION (ins_talt)
{
	/* Set the alt state as enabeling */
	WORKSPACE_SET(WPTR, WS_STATE, ENABLING_P);
	/* Set up the timer */
	WORKSPACE_SET(WPTR, WS_TLINK, TIME_NOT_SET_P);
	return ECTX_CONTINUE;
}





/****************************************************************************
 *              0x25 0xF_         0x25 0xF_         0x25 0xF_               *
 ****************************************************************************/

/* 0x51 - 0x25 0xF1 - taltwt - timer alt wait */
TVM_INSTRUCTION (ins_taltwt)
{
	int current_time = ectx->get_time(ectx);

	/* Set the top of the workspace to -1 */
	WORKSPACE_SET(WPTR, WS_TEMP, NONE_SELECTED_O);

	if(WORKSPACE_GET(WPTR, WS_STATE) == DISABLING_P) /* READY_P */
	{
		/*fprintf(stderr, "1");*/
		WORKSPACE_SET(WPTR, WS_TIME, current_time);
	}
	else if(WORKSPACE_GET(WPTR, WS_TLINK) == TIME_NOT_SET_P)
	{
		/*fprintf(stderr, "2");*/
		WORKSPACE_SET(WPTR, WS_STATE, WAITING_P);

		WORKSPACE_SET(WPTR, WS_IPTR, (WORD)IPTR);
		RUN_NEXT_ON_QUEUE_RET();
	}
	/* Redundant if? */
	else if(TIME_AFTER(current_time, WORKSPACE_GET(WPTR, WS_TIME)))
	{
		/*fprintf(stderr, "3");*/
		WORKSPACE_SET(WPTR, WS_STATE, DISABLING_P); /* READY_P */
		WORKSPACE_SET(WPTR, WS_TIME, current_time);
	}
	else
	{
		/*fprintf(stderr, "4");*/
		/*printf("TALT added stuff to timer queue\n");*/
		WORKSPACE_SET(WPTR, WS_TIME, WORKSPACE_GET(WPTR, WS_TIME) + 1);

		/*traverse_and_insert(TPTR, TPTR);*/
		TIMER_QUEUE_INSERT(WPTR, current_time, WORKSPACE_GET(WPTR, WS_TIME));
		/*print_timer_queue();*/
		/* Redundant if? */
		if(WORKSPACE_GET(WPTR, WS_STATE) != DISABLING_P)
		{
			WORKSPACE_SET(WPTR, WS_STATE, WAITING_P);

			WORKSPACE_SET(WPTR, WS_IPTR, (WORD)IPTR);
			RUN_NEXT_ON_QUEUE_RET();
		}
		else
		{
			SET_ERROR_FLAG_RET (EFLAG_ALT);
		}
	}

	return ECTX_CONTINUE;
}

