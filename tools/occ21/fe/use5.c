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
	FM_SEQ,			/* sequential processes (fmlist) */
	FM_PAR,			/* parallel processes (fmlist) */
	FM_FIXPOINT,		/* fixpoint (fmfix) */
	FM_ATOM,		/* atom, internal identifier (fmatom) */
	FM_INPUT,		/* input node (fmio) */
	FM_OUTPUT,		/* output node (fmio) */
	FM_DET,			/* deterministic processes (fmlist) */
	FM_NDET,		/* non-determinstic processes (fmlist) */
	FM_SKIP,		/* Skip */
	FM_STOP,		/* Stop */
	FM_DIV,			/* divergence */
	FM_CHAOS,		/* chaos */
	FM_FIELD,
	FM_TREEREF,		/* parse-tree link (fmtree) */
	FM_NODEREF,		/* local node reference (fmnode) */
	FM_NAMEDPROC		/* named/parameterised process (fmproc) */
} fmtype_e;

typedef struct TAG_fmnode {
	fmtype_e type;
	treenode *org;		/* where it originated */
	union {
		struct {
			struct TAG_fmnode **items;	/* items */
			int items_cur, items_max;	/* current, maximum */
		} fmlist;				/* SEQ,PAR,DET,NDET */
		struct {
			struct TAG_fmnode *id;		/* fixpoint identifier (atom) */
			struct TAG_fmnode *proc;	/* RHS process */
		} fmfix;				/* FIXPOINT */
		struct {
			char *id;			/* identifier */
		} fmatom;				/* ATOM */
		struct {
			struct TAG_fmnode *lhs;		/* channel */
			struct TAG_fmnode *rhs;		/* value/variable if relevant */
		} fmio;					/* INPUT,OUTPUT */
		struct {
			char *name;			/* name if relevant */
			treenode *node;			/* parse-tree node */
		} fmtree;				/* TREEREF */
		struct {
			struct TAG_fmnode *node;	/* fmnode_t */
		} fmnode;				/* NODEREF */
		struct {
			char *name;			/* name to use */
			struct TAG_fmnode **parms;	/* formal parameters */
			int parms_cur, parms_max;	/* current, maximum */
			struct TAG_fmnode *body;	/* body model */
		} fmproc;				/* NAMEDPROC */
	} u;
} fmnode_t;

typedef struct TAG_fmstate {
	struct TAG_fmstate *prev;			/* previous state (when stacking) */
	fmnode_t *temp;					/* where the model ends up */
	fmnode_t **fvars;				/* free variables (params) */
	int fvars_cur, fvars_max;			/* current, max */
	fmnode_t **target;				/* where the next item goes */
} fmstate_t;

typedef struct TAG_fmmset {
	fmnode_t **items;				/* items collected (NAMEDPROCs) */
	int items_cur, items_max;			/* current, maximum */
} fmmset_t;

/*}}}*/
/*{{{  private data*/

PRIVATE int atom_counter = 0;


/*}}}*/


/*{{{  PRIVATE void fmt_addtolist (fmnode_t ***iptr, int *cptr, int *mptr, fmnode_t *item)*/
/*
 *	adds an fmnode_t item to a list
 */
PRIVATE void fmt_addtolist (fmnode_t ***iptr, int *cptr, int *mptr, fmnode_t *item)
{
	int i;

	if (!*mptr || !*iptr) {
		/* allocate list */
		*mptr = 16;
		*iptr = (fmnode_t **)memalloc (*mptr * sizeof (fmnode_t *));
		for (i=0; i<*mptr; i++) {
			(*iptr)[i] = NULL;
		}
	} else if (*cptr == *mptr) {
		/* more space please */
		fmnode_t **cur = *iptr;

		*iptr = (fmnode_t **)memalloc ((*mptr + 16) * sizeof (fmnode_t *));
		for (i=0; i<*mptr; i++) {
			(*iptr)[i] = cur[i];
		}
		for (i=*mptr; i<(*mptr + 16); i++) {
			(*iptr)[i] = NULL;
		}
		*mptr = *mptr + 16;

		memfree (cur);
	}
	(*iptr)[*cptr] = item;
	*cptr = *cptr + 1;

	return;
}
/*}}}*/
/*{{{  PRIVATE void fmt_delfromlist (fmnode_t ***iptr, int *cptr, int *mptr, int idx)*/
/*
 *	removes an item from a specific position in a list
 */
PRIVATE void fmt_delfromlist (fmnode_t ***iptr, int *cptr, int *mptr, int idx)
{
	int i;

	if (idx >= *cptr) {
		msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, NOPOSN, "use5: fmt_delfromlist(): index out of range!");
		return;		/* out of range! */
	}
	*cptr = *cptr - 1;
	for (i=idx; i<*cptr; i++) {
		(*iptr)[i] = (*iptr)[i+1];
	}
}
/*}}}*/
/*{{{  PRIVATE void fmt_insertintolist (fmnode_t ***iptr, int *cptr, int *mptr, int idx, fmnode_t *item)*/
/*
 *	inserts an item into a list at a specific position
 */
PRIVATE void fmt_insertintolist (fmnode_t ***iptr, int *cptr, int *mptr, int idx, fmnode_t *item)
{
	int i;

	if (!*mptr || !*iptr) {
		/* allocate list */
		*mptr = 16;
		*iptr = (fmnode_t **)memalloc (*mptr * sizeof (fmnode_t *));
		for (i=0; i<*mptr; i++) {
			(*iptr)[i] = NULL;
		}
	} else if (*cptr == *mptr) {
		/* more space please */
		fmnode_t **cur = *iptr;

		*iptr = (fmnode_t **)memalloc ((*mptr + 16) * sizeof (fmnode_t *));
		for (i=0; i<*mptr; i++) {
			(*iptr)[i] = cur[i];
		}
		for (i=*mptr; i<(*mptr + 16); i++) {
			(*iptr)[i] = NULL;
		}
		*mptr = *mptr + 16;

		memfree (cur);
	}

	for (i=(*cptr - 1); i>=idx; i--) {
		(*iptr)[i+1] = (*iptr)[i];
	}
	*cptr = *cptr + 1;
	(*iptr)[idx] = item;
}
/*}}}*/
/*{{{  PRIVATE void fmt_error_now (int code, SOURCEPOSN locn, const char *string)*/
/*
 *	generate an error immediately
 */
PRIVATE void fmt_error_now (int code, SOURCEPOSN locn, const char *string)
{
	msg_out_s (SEV_ERR_JMP, USE, code, locn, string);
	return;
}
/*}}}*/
/*{{{  PRIVATE void fmt_error_internal (SOURCEPOSN locn, const char *string)*/
/*
 *	generate an error immediately
 */
PRIVATE void fmt_error_internal (SOURCEPOSN locn, const char *string)
{
	msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, locn, string);
}
/*}}}*/
/*{{{  PRIVATE fmnode_t *fmt_newnode (fmtype_e type, treenode *org)*/
/*
 *	create a new fmnode_t
 */
PRIVATE fmnode_t *fmt_newnode (fmtype_e type, treenode *org)
{
	fmnode_t *fmn = (fmnode_t *)memalloc (sizeof (fmnode_t));

	fmn->type = type;
	fmn->org = org;

	switch (type) {
	case FM_SEQ:
	case FM_PAR:
	case FM_DET:
	case FM_NDET:
		fmn->u.fmlist.items = NULL;
		fmn->u.fmlist.items_cur = 0;
		fmn->u.fmlist.items_max = 0;
		break;
	case FM_ATOM:
		fmn->u.fmatom.id = NULL;
		break;
	case FM_FIXPOINT:
		fmn->u.fmfix.id = NULL;
		fmn->u.fmfix.proc = NULL;
		break;
	case FM_INPUT:
	case FM_OUTPUT:
		fmn->u.fmio.lhs = NULL;
		fmn->u.fmio.rhs = NULL;
		break;
	case FM_SKIP:
	case FM_STOP:
	case FM_DIV:
	case FM_CHAOS:
		break;
	case FM_TREEREF:
		fmn->u.fmtree.name = NULL;
		fmn->u.fmtree.node = NULL;
		break;
	case FM_NODEREF:
		fmn->u.fmnode.node = NULL;
		break;
	case FM_NAMEDPROC:
		fmn->u.fmproc.name = NULL;
		fmn->u.fmproc.parms = NULL;
		fmn->u.fmproc.parms_cur = 0;
		fmn->u.fmproc.parms_max = 0;
		fmn->u.fmproc.body = NULL;
		break;
	default:
		break;
	}

	return fmn;
}
/*}}}*/
/*{{{  PRIVATE void fmt_freenode (fmnode_t *fmn, int deep)*/
/*
 *	frees a fmnode_t, if deep is 1, frees any internal structuring associated
 *	with the 'type' field, if 2, does a deep free
 */
PRIVATE void fmt_freenode (fmnode_t *fmn, int deep)
{
	if (!fmn) {
		fmt_error_internal (NOPOSN, "fmt_freenode(): NULL pointer!");
		return;
	}
	if (deep) {
		switch (fmn->type) {
		case FM_NAMEDPROC:
			if (deep == 2) {
				int i;

				for (i=0; i<fmn->u.fmproc.parms_cur; i++) {
					if (fmn->u.fmproc.parms[i]) {
						fmt_freenode (fmn->u.fmproc.parms[i], deep);
					}
				}
				if (fmn->u.fmproc.body) {
					fmt_freenode (fmn->u.fmproc.body, deep);
				}
			}
			if (fmn->u.fmproc.parms) {
				memfree (fmn->u.fmproc.parms);
				fmn->u.fmproc.parms = NULL;
				fmn->u.fmproc.parms_cur = 0;
				fmn->u.fmproc.parms_max = 0;
			}
			fmn->u.fmproc.body = NULL;
			if (fmn->u.fmproc.name) {
				memfree (fmn->u.fmproc.name);
				fmn->u.fmproc.name = NULL;
			}
			break;
		case FM_SEQ:
		case FM_PAR:
		case FM_DET:
		case FM_NDET:
			if (deep == 2) {
				int i;

				for (i=0; i<fmn->u.fmlist.items_cur; i++) {
					if (fmn->u.fmlist.items[i]) {
						fmt_freenode (fmn->u.fmlist.items[i], deep);
					}
				}
			}
			if (fmn->u.fmlist.items) {
				memfree (fmn->u.fmlist.items);
				fmn->u.fmlist.items = NULL;
				fmn->u.fmlist.items_cur = 0;
				fmn->u.fmlist.items_max = 0;
			}
			break;
		case FM_FIXPOINT:
			if (deep == 2) {
				if (fmn->u.fmfix.id) {
					fmt_freenode (fmn->u.fmfix.id, deep);
				}
				if (fmn->u.fmfix.proc) {
					fmt_freenode (fmn->u.fmfix.proc, deep);
				}
			}
			fmn->u.fmfix.id = NULL;
			fmn->u.fmfix.proc = NULL;
			break;
		case FM_INPUT:
		case FM_OUTPUT:
			if (deep == 2) {
				if (fmn->u.fmio.lhs) {
					fmt_freenode (fmn->u.fmio.lhs, deep);
				}
				if (fmn->u.fmio.rhs) {
					fmt_freenode (fmn->u.fmio.rhs, deep);
				}
			}
			fmn->u.fmio.lhs = NULL;
			fmn->u.fmio.rhs = NULL;
			break;
		case FM_ATOM:
			if (fmn->u.fmatom.id) {
				memfree (fmn->u.fmatom.id);
				fmn->u.fmatom.id = NULL;
			}
			break;
		case FM_TREEREF:
			if (fmn->u.fmtree.name) {
				memfree (fmn->u.fmtree.name);
				fmn->u.fmtree.name = NULL;
			}
			fmn->u.fmtree.node = NULL;
			break;
		case FM_NODEREF:
			fmn->u.fmnode.node = NULL;
			break;
		case FM_SKIP:
		case FM_STOP:
		case FM_DIV:
		case FM_CHAOS:
			break;
		default:
			break;
		}
	}
	memfree (fmn);
	return;
}
/*}}}*/
/*{{{  PRIVATE fmstate_t *fmt_newstate (void)*/
/*
 *	creates a new fmstate_t structure
 */
PRIVATE fmstate_t *fmt_newstate (void)
{
	fmstate_t *fms = (fmstate_t *)memalloc (sizeof (fmstate_t));

	fms->prev = NULL;
	fms->temp = NULL;
	fms->fvars = NULL;
	fms->fvars_cur = 0;
	fms->fvars_max = 0;
	fms->target = &fms->temp;

	return fms;
}
/*}}}*/
/*{{{  PRIVATE void fmt_freestate (fmstate_t *fmstate)*/
/*
 *	frees an fmstate_t structure
 */
PRIVATE void fmt_freestate (fmstate_t *fmstate)
{
	if (!fmstate) {
		fmt_error_internal (NOPOSN, "fmt_freestate(): NULL pointer");
		return;
	}

	if (fmstate->fvars) {
		int i;

		for (i=0; i<fmstate->fvars_cur; i++) {
			if (fmstate->fvars[i]) {
				fmt_freenode (fmstate->fvars[i], 2);
			}
		}
		memfree (fmstate->fvars);
		fmstate->fvars = NULL;
		fmstate->fvars_cur = 0;
		fmstate->fvars_max = 0;
	}
	if (fmstate->temp) {
		fmt_freenode (fmstate->temp, 2);
		fmstate->temp = NULL;
	}
	fmstate->target = NULL;
	memfree (fmstate);
	return;
}
/*}}}*/
/*{{{  PRIVATE fmmset_t *fmt_newmset (void)*/
/*
 *	creates a new fmmset_t structure
 */
PRIVATE fmmset_t *fmt_newmset (void)
{
	fmmset_t *fmm = (fmmset_t *)memalloc (sizeof (fmmset_t));

	fmm->items = NULL;
	fmm->items_cur = 0;
	fmm->items_max = 0;

	return fmm;
}
/*}}}*/
/*{{{  PRIVATE void fmt_freemset (fmmset_t *fmm)*/
/*
 *	frees a fmmset_t structure
 */
PRIVATE void fmt_freemset (fmmset_t *fmm)
{
	if (!fmm) {
		fmt_error_internal (NOPOSN, "fmt_freemset(): NULL pointer");
		return;
	}
	if (fmm->items) {
		int i;

		for (i=0; i<fmm->items_cur; i++) {
			if (fmm->items[i]) {
				fmt_freenode (fmm->items[i], 2);
			}
		}
		memfree (fmm->items);
		fmm->items = NULL;
		fmm->items_cur = 0;
		fmm->items_max = 0;
	}
	memfree (fmm);
	return;
}
/*}}}*/
/*{{{  PRIVATE void fmt_modprewalk (fmnode_t **nodep, int (*f1)(fmnode_t **, void *), void *voidptr)*/
/*
 *	calls f1() on each node-pointer in pre-walk order.
 *	if f1 returns CONTINUE_WALK, walks through children (if any), else
 *	returns normally.
 */
PRIVATE void fmt_modprewalk (fmnode_t **nodep, int (*f1)(fmnode_t **, void *), void *voidptr)
{
	fmnode_t *n;
	int r, i;

	if (!nodep || !*nodep) {
		return;
	}

	r = f1 (nodep, voidptr);
	n = *nodep;
	if (r == CONTINUE_WALK) {
		switch (n->type) {
		case FM_SEQ:
		case FM_PAR:
		case FM_DET:
		case FM_NDET:
			for (i=0; i<n->u.fmlist.items_cur; i++) {
				fmt_modprewalk (&n->u.fmlist.items[i], f1, voidptr);
			}
			break;
		case FM_FIXPOINT:
			fmt_modprewalk (&n->u.fmfix.id, f1, voidptr);
			fmt_modprewalk (&n->u.fmfix.proc, f1, voidptr);
			break;
		case FM_NAMEDPROC:
			for (i=0; i<n->u.fmproc.parms_cur; i++) {
				fmt_modprewalk (&n->u.fmproc.parms[i], f1, voidptr);
			}
			fmt_modprewalk (&n->u.fmproc.body, f1, voidptr);
			break;
		case FM_SKIP:
		case FM_STOP:
		case FM_DIV:
		case FM_CHAOS:
		case FM_ATOM:
		case FM_TREEREF:
		case FM_NODEREF:
			break;
		case FM_INPUT:
		case FM_OUTPUT:
			fmt_modprewalk (&n->u.fmio.lhs, f1, voidptr);
			fmt_modprewalk (&n->u.fmio.rhs, f1, voidptr);
			break;
		case FM_FIELD:
			/* FIXME: fields not handled yet */
			break;
		case FM_INVALID:
			break;
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void fmt_writeoutindent (int indent, FILE *fp)*/
/*
 *	writes out indentation (tab style)
 */
PRIVATE void fmt_writeoutindent (int indent, FILE *fp)
{
	int i;

	for (i=0; i<indent; i++) {
		fprintf (fp, "    ");
	}
}
/*}}}*/


/*{{{  PRIVATE fmnode_t *fmt_newatom (treenode *org)*/
/*
 *	creates a new fmnode_t ATOM type (unique identifier)
 */
PRIVATE fmnode_t *fmt_newatom (treenode *org)
{
	fmnode_t *fma = fmt_newnode (FM_ATOM, org);

	fma->u.fmatom.id = (char *)memalloc (16);
	sprintf (fma->u.fmatom.id, "A%d", atom_counter);
	atom_counter++;

	return fma;
}
/*}}}*/
/*{{{  PRIVATE void fmt_dumpnode (fmnode_t *fmn, int indent, FILE *stream)*/
/*
 *	prints an fmnode_t structure (debugging)
 */
PRIVATE void fmt_dumpnode (fmnode_t *fmn, int indent, FILE *stream)
{
	int i;

	for (i=0; i<indent; i++) {
		fprintf (stream, "    ");
	}

	if (!fmn) {
		fprintf (stream, "NULL\n");
	} else {
		switch (fmn->type) {
		case FM_SEQ:
			fprintf (stream, "SEQ\n");
			break;
		case FM_PAR:
			fprintf (stream, "PAR\n");
			break;
		case FM_DET:
			fprintf (stream, "DET\n");
			break;
		case FM_NDET:
			fprintf (stream, "NDET\n");
			break;
		case FM_SKIP:
			fprintf (stream, "SKIP\n");
			break;
		case FM_STOP:
			fprintf (stream, "STOP\n");
			break;
		case FM_DIV:
			fprintf (stream, "DIV\n");
			break;
		case FM_CHAOS:
			fprintf (stream, "CHAOS\n");
			break;
		case FM_FIXPOINT:
			fprintf (stream, "FIXPOINT\n");
			break;
		case FM_INPUT:
			fprintf (stream, "INPUT\n");
			break;
		case FM_OUTPUT:
			fprintf (stream, "OUTPUT\n");
			break;
		case FM_ATOM:
			fprintf (stream, "ATOM (%s)\n", fmn->u.fmatom.id);
			break;
		case FM_TREEREF:
			fprintf (stream, "TREEREF (0x%8.8x,%s,%s)\n", (unsigned int)fmn->u.fmtree.node, tagstring (TagOf (fmn->u.fmtree.node)),
					fmn->u.fmtree.name ?: "(no-name)");
			break;
		case FM_NODEREF:
			fprintf (stream, "NODEREF (0x%8.8x)\n", (unsigned int)fmn->u.fmnode.node);
			break;
		case FM_NAMEDPROC:
			fprintf (stream, "NAMEDPROC (%s) %d parms\n", fmn->u.fmproc.name ?: "(no-name)", fmn->u.fmproc.parms_cur);
			break;
		default:
			fprintf (stream, "unknown type %d!\n", (int)fmn->type);
			break;
		}
		switch (fmn->type) {
		case FM_SEQ:
		case FM_PAR:
		case FM_DET:
		case FM_NDET:
			for (i=0; i<fmn->u.fmlist.items_cur; i++) {
				fmt_dumpnode (fmn->u.fmlist.items[i], indent + 1, stream);
			}
			break;
		case FM_FIXPOINT:
			fmt_dumpnode (fmn->u.fmfix.id, indent + 1, stream);
			fmt_dumpnode (fmn->u.fmfix.proc, indent + 1, stream);
			break;
		case FM_INPUT:
		case FM_OUTPUT:
			fmt_dumpnode (fmn->u.fmio.lhs, indent + 1, stream);
			fmt_dumpnode (fmn->u.fmio.rhs, indent + 1, stream);
			break;
		case FM_NODEREF:
			fmt_dumpnode (fmn->u.fmnode.node, indent + 1, stream);
			break;
		case FM_NAMEDPROC:
			for (i=0; i<fmn->u.fmproc.parms_cur; i++) {
				fmt_dumpnode (fmn->u.fmproc.parms[i], indent + 1, stream);
			}
			fmt_dumpnode (fmn->u.fmproc.body, indent + 1, stream);
			break;
		default:
			break;
		}
	}
}
/*}}}*/
/*{{{  PRIVATE void fmt_dumpstate (fmstate_t *fms, FILE *stream)*/
/*
 *	prints an fmstate_t structure (debugging)
 */
PRIVATE void fmt_dumpstate (fmstate_t *fms, FILE *stream)
{
	int i;

	fprintf (stream, "fmstate:\n");
	fprintf (stream, "  fvars (%d):\n", fms->fvars_cur);
	for (i=0; i<fms->fvars_cur; i++) {
		fmt_dumpnode (fms->fvars[i], 1, stream);
	}
	fprintf (stream, "  temp:\n");
	fmt_dumpnode (fms->temp, 1, stream);
}
/*}}}*/
/*{{{  PRIVATE void fmt_dumpmset (fmmset_t *fmm, FILE *stream)*/
/*
 *	prints an fmmset_t structure (debugging)
 */
PRIVATE void fmt_dumpmset (fmmset_t *fmm, FILE *stream)
{
	int i;

	fprintf (stream, "fmmset:\n");
	fprintf (stream, "  items (%d):\n", fmm->items_cur);
	for (i=0; i<fmm->items_cur; i++) {
		fmt_dumpnode (fmm->items[i], 1, stream);
	}
}
/*}}}*/
/*{{{  PRIVATE fmnode_t *fmt_findfreevar (fmstate_t *state, treenode *n)*/
/*
 *	searches free-variables for a specifc tree-node
 */
PRIVATE fmnode_t *fmt_findfreevar (fmstate_t *state, treenode *n)
{
	int i;

	while (state) {
		for (i=0; i<state->fvars_cur; i++) {
			if (state->fvars[i]->type == FM_TREEREF) {
				if (state->fvars[i]->u.fmtree.node == n) {
					return state->fvars[i];
				}
			}
		}
		state = state->prev;
	}
	return NULL;
}
/*}}}*/
/*{{{  PRIVATEPARAM int fmt_simplifynode (fmnode_t **nodep, void *voidptr)*/
/*
 *	called to simplify a formal model specification, in prewalk order
 */
PRIVATEPARAM int fmt_simplifynode (fmnode_t **nodep, void *voidptr)
{
	fmnode_t *fmn = *nodep;

	switch (fmn->type) {
	case FM_SEQ:
	case FM_PAR:
	case FM_NDET:
	case FM_DET:
		/* collapse similar subnodes */
		{
			int i;

			for (i=0; i<fmn->u.fmlist.items_cur; i++) {
				fmnode_t *item = fmn->u.fmlist.items[i];

				if (item && (item->type == fmn->type)) {
					/* yes, collapse and remove node */
					int j;

					fmt_delfromlist (&fmn->u.fmlist.items, &fmn->u.fmlist.items_cur, &fmn->u.fmlist.items_max, i);
					i--;

					for (j=0; j<item->u.fmlist.items_cur; j++) {
						fmnode_t *subitem = item->u.fmlist.items[j];

						i++;
						fmt_insertintolist (&fmn->u.fmlist.items, &fmn->u.fmlist.items_cur, &fmn->u.fmlist.items_max, i, subitem);
						item->u.fmlist.items[j] = NULL;
					}

					fmt_freenode (item, 2);
				}
			}
		}
		break;
	default:
		break;
	}
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATE void fmt_writeoutnode_str (fmnode_t *node, FILE *fp)*/
/*
 *	called to write out the string associated with a single node
 */
PRIVATE void fmt_writeoutnode_str (fmnode_t *node, FILE *fp)
{
	if (!node) {
		fprintf (fp, "nullnode");
		return;
	}
	if (node->type == FM_NODEREF) {
		node = node->u.fmnode.node;
	}

	switch (node->type) {
	case FM_TREEREF:
		if (node->u.fmtree.name) {
			fprintf (fp, "%s", node->u.fmtree.name);
		} else {
			fprintf (fp, "addr0x%8.8x", (unsigned int)node->u.fmtree.node);
		}
		break;
	case FM_ATOM:
		fprintf (fp, "%s", node->u.fmatom.id);
		break;
	default:
		fprintf (fp, "unknown-type-%d", (int)node->type);
		break;
	}
}
/*}}}*/
/*{{{  PRIVATE void fmt_writeoutnode (fmnode_t *node, int indent, FILE *fp)*/
/*
 *	called to write out a single node to a file (recursively)
 */
PRIVATE void fmt_writeoutnode (fmnode_t *node, int indent, FILE *fp)
{
	int i;
	char sname[12];

	if (node && (node->type == FM_NODEREF)) {
		/* be transparent to these */
		node = node->u.fmnode.node;
	}

	fmt_writeoutindent (indent, fp);
	if (!node) {
		fprintf (fp, "<cspx:nullnode />\n");
		return;
	}
	
	switch (node->type) {
	case FM_SEQ:
	case FM_PAR:
	case FM_DET:
	case FM_NDET:
		switch (node->type) {
		case FM_SEQ:	strcpy (sname, "seq"); break;
		case FM_PAR:	strcpy (sname, "par"); break;
		case FM_DET:	strcpy (sname, "det"); break;
		case FM_NDET:	strcpy (sname, "ndet"); break;
		default:	strcpy (sname, "invalid"); break;
		}

		fprintf (fp, "<cspx:%s>\n", sname);
		for (i=0; i<node->u.fmlist.items_cur; i++) {
			fmt_writeoutnode (node->u.fmlist.items[i], indent + 1, fp);
		}
		fmt_writeoutindent (indent, fp);
		fprintf (fp, "</cspx:%s>\n", sname);
		break;
	case FM_INPUT:
	case FM_OUTPUT:
		switch (node->type) {
		case FM_INPUT:	strcpy (sname, "input"); break;
		case FM_OUTPUT:	strcpy (sname, "output"); break;
		default:	strcpy (sname, "invalid"); break;
		}

		fprintf (fp, "<cspx:%s>\n", sname);
		fmt_writeoutnode (node->u.fmio.lhs, indent + 1, fp);
		fmt_writeoutnode (node->u.fmio.rhs, indent + 1, fp);
		fmt_writeoutindent (indent, fp);
		fprintf (fp, "</cspx:%s>\n", sname);
		break;
	case FM_NAMEDPROC:
		fprintf (fp, "<cspx:proc name=\"%s\" events=\"", node->u.fmproc.name);
		for (i=0; i<node->u.fmproc.parms_cur; i++) {
			fmt_writeoutnode_str (node->u.fmproc.parms[i], fp);
			if (i < (node->u.fmproc.parms_cur - 1)) {
				fprintf (fp, ",");
			}
		}
		fprintf (fp, "\">\n");

		fmt_writeoutnode (node->u.fmproc.body, indent + 1, fp);

		fmt_writeoutindent (indent, fp);
		fprintf (fp, "</cspx:proc>\n");
		break;
	case FM_ATOM:
		fprintf (fp, "<cspx:atom id=\"%s\" />\n", node->u.fmatom.id);
		break;
	case FM_TREEREF:
		fprintf (fp, "<cspx:treeref name=\"");
		fmt_writeoutnode_str (node, fp);
		fprintf (fp, "\" />\n");
		break;
	case FM_FIXPOINT:
		fprintf (fp, "<cspx:fixpoint>\n");
		fmt_writeoutnode (node->u.fmfix.id, indent + 1, fp);
		fmt_writeoutnode (node->u.fmfix.proc, indent + 1, fp);

		fmt_writeoutindent (indent, fp);
		fprintf (fp, "</cspx:fixpoint>\n");
		break;
	case FM_SKIP:
		fprintf (fp, "<cspx:skip />\n");
		break;
	case FM_STOP:
		fprintf (fp, "<cspx:stop />\n");
		break;
	case FM_DIV:
		fprintf (fp, "<cspx:div />\n");
		break;
	case FM_CHAOS:
		fprintf (fp, "<cspx:chaos />\n");
		break;
	default:
		fprintf (fp, "<cspx:unknown type=\"%d\">\n", (int)node->type);
		break;
	}
}
/*}}}*/


/*{{{  PRIVATE void formalmodel_writeoutset (fmmset_t *fmm, FILE *fp, const char *filename)*/
/*
 *	called to write out formal model information to a file
 */
PRIVATE void formalmodel_writeoutset (fmmset_t *fmm, FILE *fp, const char *filename)
{
	int i;

	fprintf (fp, "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n");
	fprintf (fp, "<cspx:namespace xmlns:cspx=\"http://www.cs.kent.ac.uk/projects/ofa/kroc/NAMESPACES/cspx\">\n");
	fprintf (fp, "<cspx:program name=\"%s\">\n", filename);

	for (i=0; i<fmm->items_cur; i++) {
		fmt_writeoutnode (fmm->items[i], 1, fp);
	}

	fprintf (fp, "</cspx:program>\n");
	fprintf (fp, "</cspx:namespace>\n");
}
/*}}}*/
/*{{{  PRIVATEPARAM int do_formalmodelgen (treenode *n, void *const voidptr)*/
/*
 *	does formal-model generation, generates into 'fmstate'
 */
PRIVATEPARAM int do_formalmodelgen (treenode *n, void *const voidptr)
{
	const int tag = TagOf (n);
	fmstate_t *fmstate = (fmstate_t *)voidptr;
	fmnode_t *fmn;

	switch (tag) {
		/*{{{  SKIP -- return*/
	case S_SKIP:
		*(fmstate->target) = fmt_newnode (FM_SKIP, n);
		return STOP_WALK;
		/*}}}*/
		/*{{{  STOP -- return*/
	case S_STOP:
		*(fmstate->target) = fmt_newnode (FM_STOP, n);
		return STOP_WALK;
		/*}}}*/
		/*{{{  SEQ, PAR -- return*/
	case S_SEQ:
	case S_PAR:
		{
			fmtype_e ftype = ((tag == S_SEQ) ? FM_SEQ : FM_PAR);
			treenode *items;
			fmnode_t **saved_target = fmstate->target;
			
			fmn = fmt_newnode (ftype, n);
			for (items = CBodyOf (n); !EndOfList (items); items = NextItem (items)) {
				fmnode_t *tmptarget = NULL;

				fmstate->target = &tmptarget;
				prewalktree (ThisItem (items), do_formalmodelgen, (void *)fmstate);
				if (tmptarget) {
					/* store this */
					fmt_addtolist (&fmn->u.fmlist.items, &fmn->u.fmlist.items_cur, &fmn->u.fmlist.items_max, tmptarget);
				}
			}
			/* if item-less, reduce it to a SKIP */
			if (!fmn->u.fmlist.items_cur) {
				fmt_freenode (fmn, 2);
				fmn = fmt_newnode (FM_SKIP, n);
			}

			fmstate->target = saved_target;
			*(fmstate->target) = fmn;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  N_PARAM -- return*/
	case N_PARAM:
		{
			fmn = fmt_findfreevar (fmstate, n);
			if (fmn) {
				fmnode_t *tmp = fmt_newnode (FM_NODEREF, n);

				tmp->u.fmnode.node = fmn;
				*(fmstate->target) = tmp;
			}
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  INPUT, OUTPUT -- return*/
	case S_INPUT:
	case S_OUTPUT:
		{
			fmtype_e ftype = ((tag == S_INPUT) ? FM_INPUT : FM_OUTPUT);
			fmnode_t **saved_target = fmstate->target;

			fmn = fmt_newnode (ftype, n);
			fmstate->target = &fmn->u.fmio.lhs;
			prewalktree (LHSOf (n), do_formalmodelgen, (void *)fmstate);
			fmstate->target = &fmn->u.fmio.rhs;
			prewalktree (RHSOf (n), do_formalmodelgen, (void *)fmstate);
			if (!fmn->u.fmio.lhs && !fmn->u.fmio.rhs) {
				/* nothing here, assume SKIP */
				fmt_freenode (fmn, 2);
				fmn = fmt_newnode (FM_SKIP, n);
			}
			fmstate->target = saved_target;
			*(fmstate->target) = fmn;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  WHILE -- return*/
	case S_WHILE:
		{
			fmnode_t *id = fmt_newatom (n);
			fmnode_t **saved_target = fmstate->target;
			treenode *guard = CondGuardOf (n);

			fmn = fmt_newnode (FM_FIXPOINT, n);
			fmn->u.fmfix.id = id;
			fmstate->target = &fmn->u.fmfix.proc;

			prewalktree (CondBodyOf (n), do_formalmodelgen, (void *)fmstate);
			if (!fmn->u.fmfix.proc) {
				/* assume Skip for body of loop -- divergence? */
				fmn->u.fmfix.proc = fmt_newnode (FM_SKIP, CondBodyOf (n));
			}

			if (isconst (guard) && eval_const_treenode (guard)) {
				/* assuming WHILE TRUE type of loop */
				fmnode_t *tmp = fmt_newnode (FM_SEQ, n);
				fmnode_t *idref = fmt_newnode (FM_NODEREF, n);

				idref->u.fmnode.node = id;
				fmt_addtolist (&tmp->u.fmlist.items, &tmp->u.fmlist.items_cur, &tmp->u.fmlist.items_max, fmn->u.fmfix.proc);
				fmt_addtolist (&tmp->u.fmlist.items, &tmp->u.fmlist.items_cur, &tmp->u.fmlist.items_max, idref);
				fmn->u.fmfix.proc = tmp;
			} else {
				/* assuming WHILE <unknown> loop */
				fmnode_t *tmp = fmt_newnode (FM_SEQ, n);
				fmnode_t *tmpnd = fmt_newnode (FM_NDET, n);
				fmnode_t *idref = fmt_newnode (FM_NODEREF, n);

				idref->u.fmnode.node = id;
				fmt_addtolist (&tmp->u.fmlist.items, &tmp->u.fmlist.items_cur, &tmp->u.fmlist.items_max, fmn->u.fmfix.proc);
				fmt_addtolist (&tmpnd->u.fmlist.items, &tmpnd->u.fmlist.items_cur, &tmpnd->u.fmlist.items_max, idref);
				fmt_addtolist (&tmpnd->u.fmlist.items, &tmpnd->u.fmlist.items_cur, &tmpnd->u.fmlist.items_max,
						fmt_newnode (FM_SKIP, n));
				fmt_addtolist (&tmp->u.fmlist.items, &tmp->u.fmlist.items_cur, &tmp->u.fmlist.items_max, tmpnd);
				fmn->u.fmfix.proc = tmp;
			}

			fmstate->target = saved_target;
			*(fmstate->target) = fmn;
		}
		return STOP_WALK;
		/*}}}*/
	default:
		break;
	}

	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATEPARAM int do_formalmodelcheck_tree (treenode *n, void *const voidptr)*/
/*
 *	does formal-model checking for PROCs and FUNCTIONs
 */
PRIVATEPARAM int do_formalmodelcheck_tree (treenode *n, void *const voidptr)
{
	const int old = switch_to_real_workspace ();
	fmmset_t *fmm = (fmmset_t *)voidptr;

	switch (TagOf (n)) {
	case S_PROCDEF:
	case S_MPROCDECL:
		if (!separatelycompiled (DNameOf (n))) {
			char *pname = (char *)WNameOf (NNameOf (DNameOf (n)));
			fmstate_t *fmstate = fmt_newstate ();
			fmnode_t *fmn = NULL, *fmnproc = NULL;
			treenode *fparams = NParamListOf (DNameOf (n));
			treenode *walk;

#if 1
fprintf (stderr, "do_formalmodecheck_tree(): PROC [%s]\n", pname);
#endif
			for (walk = fparams; !EmptyList (walk); walk = NextItem (walk)) {
				treenode *parm = ThisItem (walk);

				switch (TagOf (parm)) {
				case N_PARAM:
					{
						treenode *type = chk_gettype (parm);
						char *parmname = (char *)WNameOf (NNameOf (parm));

						if (TagOf (type) == S_CHAN) {
							/* simple channel */
							fmnode_t *pnode = fmt_newnode (FM_TREEREF, parm);
							char *str = (char *)memalloc (strlen (parmname) + 2);

							sprintf (str, "%s", parmname);
							pnode->u.fmtree.name = str;
							pnode->u.fmtree.node = parm;

							fmt_addtolist (&fmstate->fvars, &fmstate->fvars_cur, &fmstate->fvars_max, pnode);
						}

#if 0
fprintf (stderr, "do_formalmodelcheck_tree(): param type:");
printtreenl (stderr, 1, type);
#endif
					}
					break;
				default:
					break;
				}
			}

			fmstate->target = &fmstate->temp;
			prewalkproctree (DValOf (n), do_formalmodelgen, (void *)fmstate);

#if 0
fprintf (stderr, "do_formalmodelcheck_tree(): got state:\n");
fmt_dumpstate (fmstate, stderr);
#endif
			if (!fmstate->temp) {
				fmn = fmt_newnode (FM_SKIP, n);
			} else {
				fmn = fmstate->temp;
				fmstate->temp = NULL;
			}

			/* wrap this up */
			fmnproc = fmt_newnode (FM_NAMEDPROC, n);
			fmnproc->u.fmproc.body = fmn;
			fmnproc->u.fmproc.name = (char *)memalloc (strlen (pname) + 3);
			{
				char *ch, *dh = fmnproc->u.fmproc.name;
				dh += sprintf (dh, "P");
				for (ch = pname; *ch != '\0'; ch++) {
					if ((*ch >= 'a') && (*ch <= 'z')) {
						*(dh++) = (*ch - 'a') + 'A';
					} else {
						*(dh++) = *ch;
					}
				}
				*dh = '\0';
			}
			fmnproc->u.fmproc.parms = fmstate->fvars;
			fmnproc->u.fmproc.parms_cur = fmstate->fvars_cur;
			fmnproc->u.fmproc.parms_max = fmstate->fvars_max;
			fmstate->fvars = NULL;
			fmstate->fvars_cur = 0;
			fmstate->fvars_max = 0;

			fmt_freestate (fmstate);

			fmt_modprewalk (&fmnproc, fmt_simplifynode, NULL);

			fmt_addtolist (&fmm->items, &fmm->items_cur, &fmm->items_max, fmnproc);
#if 0
fprintf (stderr, "do_formalmodelcheck_tree(): got formal model:\n");
fmt_dumpnode (fmnproc, 1, stderr);
#endif
		}
		break;
	}

	switch_to_prev_workspace (old);
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PUBLIC void formalmodelcheck (treenode *n, BOOL check_formalmodels, const char *filename)*/
/* 
 *	does formal-model checking on a tree, calls do_formalmodelcheck_tree for each PROC/FUNCTION
 */
PUBLIC void formalmodelcheck (treenode *n, BOOL check_formalmodels, const char *filename)
{
	if (check_formalmodels) {
		FILE *fp;
		jmp_buf saved_env;

		memcpy ((char *)saved_env, (char *)env, sizeof (env));
		fp = fopen (filename, "w");
		if (!fp) {
			msg_out_ss (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, NOPOSN, "failed to open output file %s", filename);
			return;
		}
		if (setjmp (env) == 0) {
			fmmset_t *fmm = fmt_newmset ();

			prewalkproctree (n, do_formalmodelcheck_tree, (void *)fmm);

#if 1
fprintf (stderr, "formalmodelcheck(): got models:\n");
fmt_dumpmset (fmm, stderr);
#endif
			/* write out! */
			formalmodel_writeoutset (fmm, fp, filename);

			fmt_freemset (fmm);
		}

		memcpy ((char *)env, (char *)saved_env, sizeof (env));
		flocn = NOPOSN;
		fclose (fp);
	}
	return;
}
/*}}}*/

