/*
 *	pgroup.c -- process group handling
 *	Copyright (C) 2005 Fred Barnes <frmb@kent.ac.uk>
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined(PROCESS_GROUPS)

/*{{{  general includes*/

#ifdef OOS_BUILD
	#include <oos_funcs.h>
#else	/* !OOS_BUILD */
	#include <stdio.h>
	#include <string.h>
	#include <sys/types.h>
	#include <sys/fcntl.h>
	#include <unistd.h>
#endif	/* !OOS_BUILD */

#include <sched_consts.h>
#include <sched_types.h>
#include <arch/sched.h>
#include <rts.h>
#include <dmem_if.h>
#include <queue_defs.h>
/*}}}*/

/*{{{  mobile process includes/defines*/
#include <pgroup.h>
/*}}}*/

/*{{{  global vars*/
pgrpinfo_t default_pgroup = {
	nprocs: 1
};
/*}}}*/


/*{{{  pgrpinfo_t *pgrp_newgrp (void)*/
/*
 *	creates a new process group and returns it (blank)
 */
pgrpinfo_t *pgrp_newgrp (void)
{
	pgrpinfo_t *group = (pgrpinfo_t *)dmem_alloc (sizeof (pgrpinfo_t));

	group->nprocs = 0;
	return group;
}
/*}}}*/
/*{{{  void pgrp_freegrp (pgrpinfo_t *group)*/
/*
 *	destroys a process group
 */
void pgrp_freegrp (pgrpinfo_t *group)
{
	if (group->nprocs) {
		BMESSAGE ("warning: process group 0x%8.8x has %d processes, not destroying\n", (unsigned int)group, group->nprocs);
		return;
	}
	dmem_release (group);

	return;
}
/*}}}*/


/*{{{  static void pgrp_info (int *ra, int size)*/
/*
 *	gets process-group info
 */
static void pgrp_info (int *ra, int size)
{
	if (size < 2) {
		return;
	}

	ra[0] = (int)PGroupOf (PPriority);
	ra[1] = PGroupOf (PPriority)->nprocs;

	return;
}
/*}}}*/


/*{{{  occam interfaces (defined in the occam8 run-time library)*/
void _do_pgrp_newgrp (int *ws)
{
	pgrpinfo_t *group = PGroupOf (PPriority);

	group->nprocs--;
	group = pgrp_newgrp ();
	group->nprocs++;
	SetPGroup (PPriority, group);
}

void _do_pgrp_info (int *ws)
{
	pgrp_info ((int *)(ws[0]), (int)(ws[1]));
}
/*}}}*/

#else	/* !defined(PROCESS_GROUPS) */
/* need stubs for occam8 library */
void _do_pgrp_newgrp (int *ws)
{
	return;
}

void _do_pgrp_info (int *ws)
{
	return;
}

#endif	/* !defined(PROCESS_GROUPS) */


