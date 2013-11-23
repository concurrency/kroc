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
#include <sys/time.h>
#include <unistd.h>

#include <armccsp.h>
#include <armccsp_types.h>
#include <armccsp_if.h>


static ccsp_sched_t *ccsp_sched;


/*{{{  forward definitions*/
static void ccsp_schedule (ccsp_sched_t *sched);
static void ccsp_chanout (ccsp_pws_t *p, void **chanaddr, void *dataaddr, int bytes);
static void ccsp_chanin (ccsp_pws_t *p, void **chanaddr, void *dataaddr, int bytes);
static void ccsp_shutdown (ccsp_pws_t *p);
static void ccsp_pause (ccsp_pws_t *p);
static void ccsp_seterr (ccsp_pws_t *p);
static void ccsp_seterrm (ccsp_pws_t *p, char *msg);
static void ccsp_malloc (ccsp_pws_t *p, int bytes, void **ptrp);
static void ccsp_mrelease (ccsp_pws_t *p, void *ptr);
static void ccsp_runp (ccsp_pws_t *p, ccsp_pws_t *other);
static void ccsp_stopp (ccsp_pws_t *p);
static void ccsp_ldtimer (ccsp_pws_t *p, int *tvar);
static void ccsp_tin (ccsp_pws_t *p, int *tvar);


/*}}}*/
/*{{{  call table*/
typedef struct TAG_ccsp_calltable {
	int nparams;
	void (*fcptr)(ccsp_pws_t *);
} ccsp_calltable_t;

static ccsp_calltable_t ccsp_calltable[] = {
	{ 3, (void (*)(ccsp_pws_t *))ccsp_chanout },			/* CALL_CHANOUT */
	{ 3, (void (*)(ccsp_pws_t *))ccsp_chanin },			/* CALL_CHANIN */
	{ 0, (void (*)(ccsp_pws_t *))ccsp_shutdown },			/* CALL_SHUTDOWN */
	{ 0, (void (*)(ccsp_pws_t *))ccsp_pause },			/* CALL_PAUSE */
	{ 0, (void (*)(ccsp_pws_t *))ccsp_seterr },			/* CALL_SETERR */
	{ 1, (void (*)(ccsp_pws_t *))ccsp_seterrm },			/* CALL_SETERRM */
	{ 2, (void (*)(ccsp_pws_t *))ccsp_malloc },			/* CALL_MALLOC */
	{ 1, (void (*)(ccsp_pws_t *))ccsp_mrelease },			/* CALL_MRELEASE */
	{ 1, (void (*)(ccsp_pws_t *))ccsp_runp },			/* CALL_RUNP */
	{ 0, (void (*)(ccsp_pws_t *))ccsp_stopp },			/* CALL_STOPP */
	{ 1, (void (*)(ccsp_pws_t *))ccsp_ldtimer },			/* CALL_LDTIMER */
	{ 1, (void (*)(ccsp_pws_t *))ccsp_tin },			/* CALL_TIN */
	{ -1, NULL }
};


/*}}}*/


/*{{{  static void ccsp_linkproc (ccsp_sched_t *sched, ccsp_pws_t *p)*/
/*
 *	enqueues a process on the specified scheduler queue (adds at the back)
 */
static void ccsp_linkproc (ccsp_sched_t *sched, ccsp_pws_t *p)
{
#ifdef CCSP_DEBUG
	fprintf (stderr, "ccsp_linkproc(): enqueue %p\n", p);
#endif
	p->link = NotProcess_p;			/* for sanity's sake */
	if (sched->fptr == NotProcess_p) {
		sched->fptr = p;
	} else {
		sched->bptr->link = p;
	}
	sched->bptr = p;
}
/*}}}*/
/*{{{  static int ccsp_readtime (void)*/
/*
 *	reads the system clock.
 */
static int ccsp_readtime (void)
{
	struct timeval tv;

	gettimeofday (&tv, NULL);
	return (int)(tv.tv_sec * 1000000) + (int)tv.tv_usec;
}
/*}}}*/
/*{{{  static void ccsp_processtimers (ccsp_sched_t *sched)*/
/*
 *	processes any pending timers, moving them from the timer-queue to the run-queue.
 */
static void ccsp_processtimers (ccsp_sched_t *sched)
{
	int now = ccsp_readtime ();

	while ((sched->tptr != NotProcess_p) && Time_AFTER (now, sched->tptr->timeout)) {
		/* this one is done, schedule */
		ccsp_linkproc (sched, sched->tptr);
		sched->tptr->timeout = now;		/* triggered *now* */
		sched->tptr = sched->tptr->tlink;
	}
}
/*}}}*/


/*{{{  static void ccsp_chanout (ccsp_pws_t *p, void **chanaddr, void *dataaddr, int bytes)*/
/*
 *	channel output
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
		*chanaddr = NotProcess_p;
	}
}
/*}}}*/
/*{{{  static void ccsp_chanin (ccsp_pws_t *p, void **chanaddr, void *dataaddr, int bytes)*/
/*
 *	channel input
 */
static void ccsp_chanin (ccsp_pws_t *p, void **chanaddr, void *dataaddr, int bytes)
{
	if (*chanaddr == NotProcess_p) {
		/* we're the first */
		p->pointer = dataaddr;
		p->link = NULL;
		*chanaddr = (void *)p;
		ccsp_schedule (p->sched);
	} else {
		ccsp_pws_t *other;
		void *dsrc;

		/* we're the second, outputting process waiting */
		other = (ccsp_pws_t *)*chanaddr;
		dsrc = (void *)other->pointer;

		memcpy (dataaddr, dsrc, bytes);
		ccsp_linkproc (other->sched, other);
		*chanaddr = NotProcess_p;
	}
}
/*}}}*/
/*{{{  static void ccsp_shutdown (ccsp_pws_t *p)*/
/*
 *	called to shut-down and exit -- done as the last thing in the initial process usually.
 */
static void ccsp_shutdown (ccsp_pws_t *p)
{
	/* FIXME: do something better! */
	armccsp_fatal ("finished :)");
}
/*}}}*/
/*{{{  static void ccsp_pause (ccsp_pws_t *p)*/
/*
 *	called to reschedule -- puts the current process on the back of the run-queue and runs next.
 */
static void ccsp_pause (ccsp_pws_t *p)
{
#ifdef CCSP_DEBUG
	fprintf (stderr, "ccsp_pause(): p=%p (stack=%p, base=%p, size=%d)\n", p, p->stack, p->stack_base, p->stack_size);
#endif
	ccsp_linkproc (p->sched, p);
	ccsp_schedule (p->sched);
}
/*}}}*/
/*{{{  static void ccsp_seterr (ccsp_pws_t *p)*/
/*
 *	hard run-time error (may be result of unhandled IF branch, empty ALT, overflow, etc.)
 */
static void ccsp_seterr (ccsp_pws_t *p)
{
	armccsp_fatal ("SetErr at %p, exiting", p->raddr);
}
/*}}}*/
/*{{{  static void ccsp_seterrm (ccsp_pws_t *p, char *msg)*/
/*
 *	hard run-time error (typically application error)
 */
static void ccsp_seterrm (ccsp_pws_t *p, char *msg)
{
	armccsp_fatal ("SetErr at %p: %s", p->raddr, msg);
}
/*}}}*/
/*{{{  static void ccsp_malloc (ccsp_pws_t *p, int bytes, void **ptrp)*/
/*
 *	dynamic memory allocation.
 */
static void ccsp_malloc (ccsp_pws_t *p, int bytes, void **ptrp)
{
	if (bytes <= 0) {
		armccsp_fatal ("ccsp_malloc(): request for %d bytes", bytes);
	}
	*ptrp = armccsp_smalloc (bytes);
}
/*}}}*/
/*{{{  static void ccsp_mrelease (ccsp_pws_t *p, void *ptr)*/
/*
 *	dynamic memory release.
 */
static void ccsp_mrelease (ccsp_pws_t *p, void *ptr)
{
	if (!ptr) {
		armccsp_fatal ("ccsp_mrelease(): attempt to free NULL pointer");
	}
	armccsp_sfree (ptr);
}
/*}}}*/
/*{{{  static void ccsp_runp (ccsp_pws_t *p, ccsp_pws_t *other)*/
/*
 *	called to schedule a process.
 */
static void ccsp_runp (ccsp_pws_t *p, ccsp_pws_t *other)
{
#ifdef CCSP_DEBUG
	fprintf (stderr, "ccsp_runp() p=%p (stack=%p), other=%p (stack=%p)\n", p, p->stack, other, other->stack);
#endif
	ccsp_linkproc (other->sched, other);
}
/*}}}*/
/*{{{  static void ccsp_stopp (ccsp_pws_t *p)*/
/*
 *	called to end a process (just reschedules something else).
 */
static void ccsp_stopp (ccsp_pws_t *p)
{
#ifdef CCSP_DEBUG
	fprintf (stderr, "ccsp_stopp() p=%p (stack=%p)\n", p, p->stack);
#endif
	ccsp_schedule (p->sched);
}
/*}}}*/
/*{{{  static void ccsp_ldtimer (ccsp_pws_t *p, int *tvar)*/
/*
 *	reads the current time (in microseconds).
 */
static void ccsp_ldtimer (ccsp_pws_t *p, int *tvar)
{
	*tvar = ccsp_readtime ();
}
/*}}}*/
/*{{{  static void ccsp_tin (ccsp_pws_t *p, int *tvar)*/
/*
 *	waits for a specific time (in microseconds).
 */
static void ccsp_tin (ccsp_pws_t *p, int *tvar)
{
	int now = ccsp_readtime ();
	ccsp_pws_t *walk;

	p->timeout = *tvar;

#if 1
fprintf (stderr, "ccsp_tin(): now=%d, timeout=%d\n", now, p->timeout);
#endif
	if (Time_AFTER (now, p->timeout)) {
		/* already gone */
		return;
	}
	/* place on timer queue */
	p->tlink = NotProcess_p;
	if (p->sched->tptr == NotProcess_p) {
		p->sched->tptr = p;
	} else {
		walk = p->sched->tptr;
		if (Time_AFTER (walk->timeout, p->timeout)) {
			/* front of timer-queue */
			p->tlink = walk;
			p->sched->tptr = p;
		} else {
			/* somewhere down the line */
			while ((walk->tlink != NotProcess_p) && Time_AFTER (walk->tlink->timeout, p->timeout)) {
				walk = walk->tlink;
			}
			/* insert after 'walk' */
			p->tlink = walk->tlink;
			walk->tlink = p;
		}
	}
	ccsp_schedule (p->sched);
}
/*}}}*/

/*{{{  static void ccsp_schedule (ccsp_sched_t *sched)*/
/*
 *	schedules a new process for execution, assumes no previous context.
 */
static void ccsp_schedule (ccsp_sched_t *sched)
{
restart_schedule:
	if (sched->tptr != NotProcess_p) {
		/* have timers waiting, so cannot be deadlocked */
		ccsp_processtimers (sched);
	}

	if (sched->fptr == NotProcess_p) {
		if (sched->tptr == NotProcess_p) {
			armccsp_fatal ("deadlocked, no processes to run!");
		} else {
			/* waiting for something to timeout */
			int now = ccsp_readtime ();
			int next = sched->tptr->timeout - now;

			if (next > 0) {
				struct timeval tv = { next / 1000000, next % 1000000 };

				select (0, NULL, NULL, NULL, &tv);		/* sleep! */
			}

			goto restart_schedule;
		}
	}

	/* got something, schedule it! */

	sched->curp = sched->fptr;
	if (sched->fptr == sched->bptr) {
		/* last one */
		sched->fptr = NotProcess_p;
	} else {
		sched->fptr = sched->fptr->link;
	}

#ifdef CCSP_DEBUG
	fprintf (stderr, "ccsp_schedule(): running process at %p\n", sched->curp);
#endif
	ProcessResume ((Workspace)sched->curp);

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
	ccsp_sched->tptr = NotProcess_p;

	return 0;
}
/*}}}*/
/*{{{  void ccsp_first (ccsp_pws_t *p)*/
/*
 *	one-time startup for the scheduler, called in main() context/stack.
 */
void ccsp_first (ccsp_pws_t *p)
{
	// p->stack = p->stack_base + (p->stack_size - 4);

	/* use our stack-pointer as the entry for other kernel things */
	RuntimeSaveStack ((Workspace)p);
	RuntimeSetEntry ((Workspace)p);
#ifdef CCSP_DEBUG
	fprintf (stderr, "ccsp_first(): p=%p, p->stack=%p (base=%p, size=%d), p->raddr=%p\n", p, p->stack, p->stack_base, p->stack_size, p->raddr);
#endif
	ccsp_linkproc (p->sched, p);

	/* the first time we enter this the stack-pointer will be different from subsequent times, but safe */
	ccsp_schedule (p->sched);
}
/*}}}*/
/*{{{  void ccsp_entry (ccsp_pws_t *wsp, const int call, void **args)*/
/*
 *	entry to all kernel functions.
 */
void ccsp_entry (ccsp_pws_t *wsp, const int call, void **args)
{
	switch (ccsp_calltable[call].nparams) {
	case 0:
		{
			void (*kfcn0)(ccsp_pws_t *) = (void (*)(ccsp_pws_t *))ccsp_calltable[call].fcptr;

			kfcn0 (wsp);
		}
		break;
	case 1:
		{
			void (*kfcn1)(ccsp_pws_t *, void *) = \
					(void (*)(ccsp_pws_t *, void *))ccsp_calltable[call].fcptr;

			kfcn1 (wsp, args[1]);
		}
		break;
	case 2:
		{
			void (*kfcn2)(ccsp_pws_t *, void *, void *) = \
					(void (*)(ccsp_pws_t *, void *, void *))ccsp_calltable[call].fcptr;

			kfcn2 (wsp, args[1], args[2]);
		}
		break;
	case 3:
		{
			void (*kfcn3)(ccsp_pws_t *, void *, void *, void *) = \
					(void (*)(ccsp_pws_t *, void *, void *, void *))ccsp_calltable[call].fcptr;

			kfcn3 (wsp, args[1], args[2], args[3]);
		}
		break;
	default:
		armccsp_fatal ("ccsp_entry(%d): unsupported args", call);
		break;
	}

	/* if we get back here, means we didn't deschedule */
	ProcessResume ((Workspace)wsp);

	return;
}
/*}}}*/



