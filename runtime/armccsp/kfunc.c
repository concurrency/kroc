/*
 *	kfunc.c -- assorted ARM/CCSP kernel functions
 *	Copyright (C) 2013-2015 Fred Barnes, University of Kent <frmb@kent.ac.uk>
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
#include <stdarg.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>

#include <armccsp.h>
#include <armccsp_types.h>
#include <armccsp_if.h>


/*{{{  static ccsp_pws_t *armccsp_alloc_process (void)*/
/*
 *	allocates a new process descriptor (blank).
 */
static ccsp_pws_t *armccsp_alloc_process (void)
{
	ccsp_pws_t *p = (ccsp_pws_t *)armccsp_smalloc (sizeof (ccsp_pws_t));
	int i;

	p->sched = ccsp_scheduler ();
	p->stack = NULL;
	p->raddr = NULL;

	p->stack_base = NULL;
	p->stack_size = 0;
	p->link = NotProcess_p;
	p->pointer = NULL;
	p->priofinity = 0;
	p->tlink = NotProcess_p;
	p->timeout = 0;

	p->pbar = NULL;
	p->iproc = NULL;
	p->nparams = 0;
	for (i=0; i<MAXPARAMS; i++) {
		p->params[i] = (uint32_t)NULL;
	}

	return p;
}
/*}}}*/


/*{{{  int ccsp_init (void)*/
/*
 *	called in main() context/stack to initialise the scheduler.
 *	returns non-zero on success.
 */
int ccsp_init (void)
{
	if (ccsp_initsched ()) {
		/* failed here :( */
		return 0;
	}
	return 1;
}
/*}}}*/


/*{{{  void ProcStartupCode (Workspace wptr)*/
/*
 *	called to run a process for the first time (startup)
 *	Note: this is called in the stack of the process itself, not the run-time.
 */
void ProcStartupCode (Workspace wptr)
{
	ccsp_pws_t *p = (ccsp_pws_t *)wptr;
	void (*kfcn)(Workspace) = (void (*)(Workspace))p->iproc;

#ifdef CCSP_DEBUG
	ExternalCallN (fprintf, 3, stderr, "ProcStartupCode(%p): pre-fcn\n", wptr);
#endif
	kfcn (wptr);
	/* Note: initial process will never get this far */
	if (p->pbar == NULL) {
		SetErrM (wptr, "top-level or parentless process terminated");
	} else {
		LightProcBarrier *bar = (LightProcBarrier *)p->pbar;

		bar->count--;
		if (!bar->count) {
			/* last one */
			RunP (wptr, bar->succ);
		}
		StopP (wptr);
	}
	return;
}
/*}}}*/

/*{{{  Workspace ProcAllocInitial (const int paramwords, const int stackwords)*/
/*
 *	allocates the initial process descriptor, called in main() context/stack.
 */
Workspace ProcAllocInitial (const int paramwords, const int stackwords)
{
	ccsp_pws_t *p = armccsp_alloc_process ();

	p->nparams = paramwords;
	p->stack_size = WORKSPACE_SIZE (paramwords, stackwords) * sizeof (uint32_t);
	p->stack_base = armccsp_smalloc (p->stack_size);
	p->stack = p->stack_base + (p->stack_size - 4);

	return (Workspace)p;
}
/*}}}*/
/*{{{  void ProcStartInitial_blind (Workspace p, void (*fcn)(Workspace))*/
/*
 *	starts the initial process, called in main() context/stack.
 */
void ProcStartInitial_blind (Workspace p, void (*fcn)(Workspace))
{
	ccsp_pws_t *pws = (ccsp_pws_t *)p;

	pws->iproc = (void *)fcn;
	ccsp_first (pws);

	return;
}
/*}}}*/


/*
 * BELOW: these run in the context of the process, not the main stack.
 */

/*{{{  Workspace LightProcInit (Workspace p, word *stack, const int nparams, const int stkwords)*/
/* @APICALLCHAIN: LightProcInit: =?, MAlloc */
/*
 *	lightweight process initialisation: assumes stack already allocated.
 */
Workspace LightProcInit (Workspace p, word *stack, const int nparams, const int stkwords)
{
	ccsp_pws_t *newp;
	ccsp_pws_t *pws = (ccsp_pws_t *)p;
	int i;

	newp = (ccsp_pws_t *)MAlloc (p, sizeof (ccsp_pws_t));

#if 0
ExternalCallN (fprintf, 7, stderr, "LightProcInit(): p=%p, newp=%p, stack=%p, nparams=%d, stkwords=%d\n", p, newp, stack, nparams, stkwords);
#endif
	newp->sched = pws->sched;
	newp->raddr = NULL;

	newp->stack_base = (void *)stack;
	newp->stack_size = WORKSPACE_SIZE (nparams, stkwords) * sizeof (int);
	newp->link = NotProcess_p;
	newp->pointer = NULL;
	newp->priofinity = 0;
	newp->tlink = NotProcess_p;
	newp->timeout = 0;

	newp->pbar = NULL;
	newp->iproc = NULL;
	newp->nparams = nparams;
	for (i=0; i<MAXPARAMS; i++) {
		newp->params[i] = (uint32_t)NULL;
	}

	newp->stack = newp->stack_base + (newp->stack_size - 4);

	return (Workspace)newp;
}
/*}}}*/
/*{{{  void LightProcFree (Workspace p, Workspace ws)*/
/* @APICALLCHAIN: LightProcFree: =?, MRelease */
/*
 *	lightweight process destruction: assumes stack released elsewhere.
 */
void LightProcFree (Workspace p, Workspace ws)
{
#if 0
ExternalCallN (fprintf, 4, stderr, "LightProcFree(): p=%p, ws=%p\n", p, ws);
#endif
	MRelease (p, (void *)ws);
}
/*}}}*/
/*{{{  void ProcParamAny (Workspace p, Workspace other, int paramno, void *arg)*/
/* @APICALLCHAIN: ProcParamAny: =? */
/*
 *	sets process parameter.
 */
void ProcParamAny (Workspace p, Workspace other, int paramno, void *arg)
{
	ccsp_pws_t *pws = (ccsp_pws_t *)other;

	pws->params[paramno] = (uint32_t)arg;
}
/*}}}*/
/*{{{  void *ProcGetParamAny (Workspace p, int paramno)*/
/* @APICALLCHAIN: ProcGetParamAny: =? */
/*
 *	gets process parameter.
 */
void *ProcGetParamAny (Workspace p, int paramno)
{
	ccsp_pws_t *pws = (ccsp_pws_t *)p;

	return (void *)pws->params[paramno];
}
/*}}}*/

/*{{{  void ProcPar (Workspace p, int nprocs, ...)*/
/* @APICALLCHAIN: ProcPar: =?, LightProcBarrierInit, LightProcStart, LightProcBarrierWait */
/*
 *	runs processes in parallel
 */
void ProcPar (Workspace p, int nprocs, ...)
{
	va_list ap;
	LightProcBarrier bar;
	int i;

	LightProcBarrierInit (p, &bar, nprocs);

	va_start (ap, nprocs);
	for (i=0; i<nprocs; i++) {
		Workspace ws = va_arg (ap, Workspace);
		void *fcn = va_arg (ap, void *);

		LightProcStart (p, &bar, ws, fcn);
	}
	va_end (ap);

	LightProcBarrierWait (p, &bar);
}
/*}}}*/
/*{{{  void LightProcStart (Workspace p, LightProcBarrier *bar, Workspace ws, void *fcn)*/
/* @APICALLCHAIN: LightProcStart: =?, RuntimeSetEntry, RunP */
/*
 *	starts a process
 */
void LightProcStart (Workspace p, LightProcBarrier *bar, Workspace ws, void *fcn)
{
	ccsp_pws_t *pws = (ccsp_pws_t *)ws;

	pws->pbar = (void *)bar;
	pws->iproc = fcn;

	RuntimeSetEntry (ws);

	RunP (p, ws);
}
/*}}}*/

/*{{{  int ProcAlt (Workspace p, ...)*/
/* @APICALLCHAIN: ProcAlt: =?, Alt, AltEnableChannel, SetErrM, AltWait, AltDisableChannel, AltEnd */
/*
 *	performs an alternative over a number of channels, list is NULL-terminated.  Returns the index of the ready-guard.
 */
int ProcAlt (Workspace p, ...)
{
	va_list ap;
	int i;
	int rdy = -1;
	int enb = 0;

	Alt (p);

	/*{{{  enable*/
	va_start (ap, p);
	for (i=0; ; i++) {
		Channel *c = va_arg (ap, Channel *);

		if (c == NULL) {
			break;
		}
		enb |= AltEnableChannel (p, 1, c);
	}
	va_end (ap);
	/*}}}*/

	if (!enb) {
		/* nothing enabled, this is an error! */
		SetErrM (p, "ProcAlt: no enabled guards!");
	} else {
		AltWait (p);
	}

	/*{{{  disabling*/
	va_start (ap, p);
	for (i=0; ; i++) {
		Channel *c = va_arg (ap, Channel *);

		if (c == NULL) {
			break;
		}
		if (AltDisableChannel (p, 1, c)) {
			if (rdy < 0) {
				/* only select first ready guard in the list! */
				rdy = i;
			}
		}
	}
	va_end (ap);
	/*}}}*/

	AltEnd (p);

	return rdy;
}
/*}}}*/


/*{{{  word ExternalCall0 (void *func)*/
/* @APICALLCHAIN: ExternalCall0: =?, ccsp_scheduler, EXTERNAL_CALL */
/*
 *	external call for argument-less function.
 */
word ExternalCall0 (void *func)
{
	ccsp_sched_t *sched = ccsp_scheduler ();
	word *stack = (word *)sched->stack;
	word result;

	stack -= 4;
	EXTERNAL_CALL (func, stack, result);

	return result;
}
/*}}}*/
/*{{{  word ExternalCall1 (void *func, word arg)*/
/* @APICALLCHAIN: ExternalCall1: =?, ccsp_scheduler, EXTERNAL_CALL */
/*
 *	external call for single-argument function.
 */
word ExternalCall1 (void *func, word arg)
{
	ccsp_sched_t *sched = ccsp_scheduler ();
	word *stack = (word *)sched->stack;
	word result;

	stack -= 4;
	*(stack--) = arg;

	EXTERNAL_CALL (func, stack, result);

	return result;
}
/*}}}*/
/*{{{  word ExternalCallN (void *func, word argc, ...)*/
/* @APICALLCHAIN: ExternalCallN: =?, ccsp_scheduler, EXTERNAL_CALL */
/*
 *	external call for variable-argument function.
 */
word ExternalCallN (void *func, word argc, ...)
{
	ccsp_sched_t *sched = ccsp_scheduler ();
	word *stack = (word *)sched->stack;
	va_list ap;
	word result;
	int i;

	va_start (ap, argc);
	stack -= (argc & ~0x03) + 4;
	stack -= 4;

	for (i=0; i<argc; i++) {
		stack[i] = va_arg (ap, word);
	}

	va_end (ap);

	EXTERNAL_CALL (func, stack, result);

	return result;
}
/*}}}*/



