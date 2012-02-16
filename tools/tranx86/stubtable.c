/*
 *	stubtable.c -- for handle STUBENTRY's
 *	Copyright (C) 2000-2005 Fred Barnes <frmb@kent.ac.uk>
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


/*{{{  includes*/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "main.h"
#include "support.h"
#include "structs.h"
#include "transputer.h"
#include "trancomm.h"
#include "tstack.h"
#include "postmortem.h"
#include "intel.h"
#include "tstate.h"
#include "archdef.h"
#include "rtlops.h"
#include "stubtable.h"


/*}}}*/
/*{{{  private data*/
/* list of stubs */
static stublist *s_head = NULL;
static stublist *s_tail = NULL;
static stublist *s_last = NULL;

/*}}}*/


/*{{{  void add_to_stubtable (int lab, ins_chain *c_first, ins_chain *c_last)*/
/*
 *	adds a stub to the list (code is coppied)
 */
void add_to_stubtable (int lab, ins_chain *c_first, ins_chain *c_last)
{
	stublist *tmp, *srch, *last;

	tmp = (stublist *)smalloc (sizeof (stublist));
	tmp->lab = lab;
	tmp->stubcode = rtl_copy_codeblock (c_first, c_last);
	tmp->next = NULL;
	if (!s_head) {
		s_head = s_tail = tmp;
	} else {
		for (srch=s_head, last=NULL; srch && (srch->lab < lab); last = srch, srch = srch->next);
		/* add between `last' and `srch' */
		if (!last) {
			/* at the start */
			tmp->next = s_head;
			s_head = tmp;
		} else if (!srch) {
			/* at the end */
			s_tail->next = tmp;
			s_tail = tmp;
		} else {
			/* in the middle */
			last->next = tmp;
			tmp->next = srch;
		}
	}
	return;
}
/*}}}*/
/*{{{  int label_is_stub (int lab)*/
/*
 *	returns non-zero if `lab' is in the stubtable
 */
int label_is_stub (int lab)
{
	stublist *tmp;

	for (tmp=s_head; tmp && (tmp->lab < lab); tmp=tmp->next);
	if (tmp && (tmp->lab == lab)) {
		s_last = tmp;
		return 1;
	}
	return 0;
}
/*}}}*/
/*{{{  ins_chain *get_stubcode (tstate *ts, int lab)*/
/*
 *	returns the instruction chain for the stub at `lab'
 *	(caller should copy what's returned)
 */
ins_chain *get_stubcode (tstate *ts, int lab)
{
	ins_chain *tmp, *walk;
	stublist *tsl;
	int i, j;
	int n_regs;
	static int old_reglist[128];
	static int new_reglist[128];

	if (s_last && (s_last->lab == lab)) {
		tmp = s_last->stubcode;
	} else {
		for (tsl=s_head; tsl && (tsl->lab < lab); tsl = tsl->next);
		if (tsl && (tsl->lab == lab)) {
			tmp = tsl->stubcode;
		} else {
			tmp = NULL;
		}
	}
	/* need to rename any virtual registers... */
	if (tmp) {
		n_regs = 0;
		for (walk=tmp; walk; walk=walk->next) {
			for (i=0; walk->in_args[i]; i++) {
				j = rtl_nvregs_in_arg (walk->in_args[i]);
				for (j--; j >= 0; j--) {
					old_reglist[n_regs] = rtl_nvreg_of_arg (walk->in_args[i], j);
					new_reglist[n_regs++] = tstack_newreg (ts->stack);
				}
			}
			for (i=0; walk->out_args[i]; i++) {
				j = rtl_nvregs_in_arg (walk->out_args[i]);
				for (j--; j >= 0; j--) {
					old_reglist[n_regs] = rtl_nvreg_of_arg (walk->out_args[i], j);
					new_reglist[n_regs++] = tstack_newreg (ts->stack);
				}
			}
		}
		/* ugly O(n^2) algorithm to remove duplicates, but size is small */
		for (i=0; i<n_regs; i++) {
			if (old_reglist[i] > -1) {
				for (j = i+1; j<n_regs; j++) {
					if (old_reglist[i] == old_reglist[j]) {
						old_reglist[j] = -1;
						new_reglist[j] = -1;
					}
				}
				rtl_rename_reg (tmp, old_reglist[i], new_reglist[i]);
			}
		}
	}
	return tmp;
}
/*}}}*/

