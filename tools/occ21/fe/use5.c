/*
 *	use5.c -- formal model checking
 *	Copyright (C) 2009 Fred Barnes <frmb@kent.ac.uk>
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

/*{{{  includes*/
#ifdef HAVE_CONFIG_H
	#include <config.h>
#endif
#include <stdio.h>
#include "feinc.h"
#include "useerror.h"
#include "usehdr.h"
#include "usedef.h"
#include "use1def.h"
#include "use2def.h"
#include "syndef.h"             /* syn_lexlevel */
#include "chkdef.h"             /* current_fe_handle etc */
#include "predefhd.h"           /* pragmas */
#include "lexdef.h"             /* for BOOL formal_model */

/*}}}*/

/*{{{  private types*/

typedef enum ENUM_fmtype {
	FM_INVALID,
	FM_SEQ,
	FM_PAR,
	FM_FIXPOINT,
	FM_ATOM,
	FM_INPUT,
	FM_OUTPUT,
	FM_DET,
	FM_NDET,
	FM_SKIP,
	FM_STOP,
	FM_DIV,
	FM_CHAOS,
	FM_FIELD
} fmtype_e;

typedef struct TAG_fmnode {
	fmtype_e type;
	treenode *org;		/* where it originated */
} fmnode_t;

/*}}}*/


/*{{{  PRIVATEPARAM int do_formalmodelcheck_tree (treenode *n, void *const voidptr)*/
/*
 *	does formal-model checking for PROCs and FUNCTIONs
 */
PRIVATEPARAM int do_formalmodelcheck_tree (treenode *n, void *const voidptr)
{
	const int old = switch_to_real_workspace ();

	switch (TagOf (n)) {
	case S_PROCDEF:
	case S_MPROCDECL:
		if (!separatelycompiled (DNameOf (n))) {
			char *pname = (char *)WNameOf (NNameOf (DNameOf (n)));

#if 1
fprintf (stderr, "do_formalmodecheck_tree(): PROC [%s]\n", pname);
#endif
		}
		break;
	}

	switch_to_prev_workspace (old);
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PUBLIC void formalmodelcheck (treenode *n, BOOL check_formalmodels)*/
/* 
 *	does formal-model checking on a tree, calls do_formalmodelcheck_tree for each PROC/FUNCTION
 */
PUBLIC void formalmodelcheck (treenode *n, BOOL check_formalmodels)
{
	if (check_formalmodels) {
		jmp_buf saved_env;

		memcpy ((char *)saved_env, (char *)env, sizeof (env));
		if (setjmp (env) == 0) {
			prewalkproctree (n, do_formalmodelcheck_tree, NULL);
		}

		memcpy ((char *)env, (char *)saved_env, sizeof (env));
		flocn = NOPOSN;
	}
	return;
}
/*}}}*/

