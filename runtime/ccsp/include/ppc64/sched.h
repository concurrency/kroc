/*
 *	Scheduler defintions (PPC kernel)
 *	Copyright (C) 2005 Fred Barnes  <frmb@kent.ac.uk>
 *	Based on i386 version, Copyright (C) 1998 Jim Moores
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef __SCHED_H
#define __SCHED_H

#include <ppc64/sched_types.h>
#include <ppc64/timer.h>

/* PUBLIC variables that are used elsewhere in the run time system. */
extern sync_type sf;

extern word* Wptr;
#define Tptr (sf.ptr)
/* extern word* Tptr; */

#if 0
/* paramter passing variables */
extern word** channel_address;
extern word return_address;
extern byte* source_address;
extern byte* destination_address;
extern word count;
extern Time wait_time;
extern int test_time;
extern word start_address;
extern word guard;
extern word process_address;
extern word code_offset;
extern word precondition;
extern word *usage;
extern Time timeout;
extern word result;
#endif

/* pseudo private variables (there names are externally visible, but they */
/* should not be used. */
extern Time now;
extern word temp1;
extern word *temp_ptr;
extern word *other_workspace;
extern word *current;
extern word *previous;
extern word fired;
extern bool isexternal;
extern word trapval_A;
extern word trapval_B;
extern word trapval_C;
extern word overflow_info2;
extern word overflow_info;
extern word filename_addr;
extern word procedure_addr;
extern word zerodiv_info2;
extern word zerodiv_info;
extern word range_info1;
extern word range_info2;
extern word seterr_info1;
extern word seterr_info2;
extern word b_param;
extern word b_func;
extern word res_ptr;

extern word mdparam1;
extern word mdparam2;
extern word mdparam3;
extern word mdparam4;
extern word mdparam5;
extern word mdparam6;

extern word *sp_aptr;
extern word *sp_bptr;
extern word *sp_cptr;

void dead(int status);
void kernel_panic();
int c_enter(void);
int not_on_any_queue (unsigned int ws_base, unsigned int ws_limit);
int remove_from_any_queue (unsigned int ws_base, unsigned int ws_limit);
void do_queue_process (word *process);
void show_last_debug_insert (void);

#define SCHED_POLICY_US 1		/* suspend other and reschedule invoking process */
#define SCHED_POLICY_OTHER 2		/* suspend invoking process and reschedule other */
#define SCHED_POLICY_NEITHER_US 3	/* suspend both, us first on the run-queue */
#define SCHED_POLICY_NEITHER_OTHER 4	/* suspend both, other first on the run-queue */

#define DEFAULT_SCHED_POLICY SCHED_POLICY_US

#if defined(OOS_BUILD)
	extern int *inttab[256];
	extern volatile int intcount[256];
	extern volatile int intlast;
	extern volatile int *iq_fptr, *iq_bptr;
	extern volatile long long ticount;
#endif

#endif	/* !__SCHED_H */

