/*
 *	kfunc.c -- assorted ARM/CCSP kernel functions
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
 */
void ProcStartupCode (Workspace wptr)
{
	ccsp_pws_t *p = (ccsp_pws_t *)wptr;

	switch (p->nparams) {
	case 0:
		{
			void (*kfcn0)(Workspace) = (void (*)(Workspace))p->iproc;

			kfcn0 (wptr);
		}
		break;
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
	p->stack_size = stackwords * sizeof (uint32_t);
	p->stack_base = armccsp_smalloc (p->stack_size);

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

