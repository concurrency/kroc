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
 *	allocates a new process descriptor (blank)
 */
static ccsp_pws_t *armccsp_alloc_process (void)
{
	ccsp_pws_t *p = (ccsp_pws_t *)armccsp_smalloc (sizeof (ccsp_pws_t));
	int i;

	p->sched = ccsp_scheduler ();
	p->stack = NULL;
	p->stack_base = NULL;
	p->link = NotProcess_p;
	p->pointer = NULL;
	p->priofinity = 0;
	for (i=0; i<MAXPARAMS; i++) {
		p->params[i] = (uint32_t)NULL;
	}
	p->nparams = 0;

	return p;
}
/*}}}*/


/*{{{  */
/*
 *	allocates the initial process descriptor.
 */
Workspace ProcAllocInitial (const int paramwords, const int stackwords)
{
	ccsp_pws_t *p = armccsp_alloc_process ();

	return (Workspace)p;
}
/*}}}*/

