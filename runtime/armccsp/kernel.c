/*
 *	kernel.c -- ARM/CCSP scheduler kernel
 *	Copyright (C) 2013 Fred Barnes, University of Kent <frmb@kent.ac.uk>
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
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include <armccsp.h>
#include <armccsp_types.h>


static ccsp_sched_t *ccsp_sched;


/*{{{  forward definitions*/
static void ccsp_schedule (ccsp_sched_t *sched);


/*}}}*/


/*{{{  static void ccsp_linkproc (ccsp_sched_t *sched, ccsp_pws_t *p)*/
/*
 *	enqueues a process on the specified scheduler queue (adds at the back)
 */
static void ccsp_linkproc (ccsp_sched_t *sched, ccsp_pws_t *p)
{
	p->link = NotProcess_p;			/* for sanity's sake */
	if (sched->fptr == NotProcess_p) {
		sched->fptr = p;
	} else {
		sched->bptr->link = p;
	}
	sched->bptr = p;
}
/*}}}*/


/*{{{  */
/*
 *	does channel output
 */
static void ccsp_chanout (ccsp_pws_t *p, void **chanaddr, void *dataaddr, int bytes)
{
	if (*chanaddr == NotProcess_p) {
		/* we're the first */
		p->pointer = dataaddr;
		p->link = NULL;
		*chanaddr = (void *)p;
		ccsp_schedule (p->sched);
	} else {
		ccsp_pws_t *other;
		void *ddest;

		/* we're the second, inputting process probably waiting */
		/* FIXME: alting input */
		other = (ccsp_pws_t *)*chanaddr;
		ddest = (void *)other->pointer;

		memcpy (ddest, dataaddr, bytes);
		ccsp_linkproc (other->sched, other);
	}
}
/*}}}*/


/*{{{  static void ccsp_schedule (ccsp_sched_t *sched)*/
/*
 *	schedules a new process for execution, assumes no previous context.
 */
static void ccsp_schedule (ccsp_sched_t *sched)
{
	if (sched->fptr == NotProcess_p) {
		armccsp_fatal ("deadlocked, no processes to run!");
	}

	sched->curp = sched->fptr;
	if (sched->fptr == sched->bptr) {
		/* last one */
		sched->fptr = NotProcess_p;
	}

	return;
}
/*}}}*/
/*{{{  ccsp_sched_t *ccsp_scheduler (void)*/
/*
 *	returns the handle for the scheduler
 */
ccsp_sched_t *ccsp_scheduler (void)
{
	return ccsp_sched;
}
/*}}}*/


/*{{{  int ccsp_initsched (void)*/
/*
 *	one-time initialisation for the scheduler.
 *	returns 0 on success, non-zero on failure
 */
int ccsp_initsched (void)
{
	ccsp_sched = (ccsp_sched_t *)armccsp_smalloc (sizeof (ccsp_sched_t));

	ccsp_sched->stack = NULL;
	ccsp_sched->curp = NotProcess_p;
	ccsp_sched->fptr = NotProcess_p;
	ccsp_sched->bptr = NotProcess_p;

	return 0;
}
/*}}}*/
/*{{{  */
/*
 *	entry to all kernel functions, called in context of individual process.
 */
void ccsp_entry (const int call, void **args)
{
	return;
}
/*}}}*/



