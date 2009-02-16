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

typedef enum ENUM_fmtype { /*{{{*/
	FM_INVALID,
	FM_SEQ,			/* sequential processes (fmlist) */
	FM_PAR,			/* parallel processes (fmlist) */
	FM_FIXPOINT,		/* fixpoint (fmfix) */
	FM_ATOM,		/* atom, internal identifier (fmatom) */
	FM_INPUT,		/* input node (fmio) */
	FM_OUTPUT,		/* output node (fmio) */
	FM_SYNC,		/* synchronisation node (fmio) */
	FM_DET,			/* deterministic processes (fmlist) */
	FM_NDET,		/* non-determinstic processes (fmlist) */
	FM_SKIP,		/* Skip */
	FM_STOP,		/* Stop */
	FM_DIV,			/* divergence */
	FM_CHAOS,		/* chaos */
	FM_FIELD,
	FM_TREEREF,		/* parse-tree link (fmtree) */
	FM_NODEREF,		/* local node reference (fmnode) */
	FM_NAMEDPROC,		/* named/parameterised process (fmproc) */
	FM_TAGSET,		/* tagged name set, from PROTOCOLs (fmtset) */
	FM_THEN,		/* list of events followed by a process (fmlist) */
	FM_EVENT,		/* single event tree reference (fmevent) */
	FM_EVENTSET,		/* set of events (with tree refererence at top-level for CHAN TYPE records) (fmevset) */
	FM_INSTANCE		/* instance of something (fminst) */
} fmtype_e;

/*}}}*/
typedef struct TAG_fmnode { /*{{{*/
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
			char *typename;			/* type name if relevant */
			treenode *nodetype;		/* type of parse-tree node if relevant */
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
		struct {
			char *name;			/* name of the type */
			struct TAG_fmnode **tags;	/* array of tags (fmtree's) */
			int tags_cur, tags_max;		/* current, maximum */
		} fmtset;				/* TAGSET */
		struct {
			char *name;			/* name of the event */
			treenode *node;			/* reference to the event (N_PARAM,N_FIELD,etc.) */
			char *typename;			/* name of the type if relevant */
			treenode *nodetype;		/* type of parse-tree node if relevant */
			int isclaim;			/* true if this is a synthetic CLAIM event */
			int isrelease;			/* true if this is a synthetic RELEASE event */
		} fmevent;				/* EVENT */
		struct {
			char *name;			/* name of the event */
			treenode *node;			/* reference to the originating treenode (N_PARAM,etc.) */
			char *typename;			/* name of the type if relevant */
			treenode *nodetype;		/* type of parse-tree node if relevant */
			struct TAG_fmnode **events;	/* array of events (fmevent's) */
			int events_cur, events_max;	/* current, maximum */
		} fmevset;				/* EVENTSET */
		struct {
			struct TAG_fmnode *pref;	/* procedure reference, either NAMEDPROC or ATOM */
			struct TAG_fmnode **args;	/* actual parameters */
			int args_cur, args_max;		/* current, maximum */
		} fminst;				/* INSTANCE */
	} u;
} fmnode_t;

/*}}}*/
typedef struct TAG_fmmset { /*{{{*/
	fmnode_t **items;				/* items collected (NAMEDPROCs) */
	int items_cur, items_max;			/* current, maximum */
} fmmset_t;

/*}}}*/
typedef struct TAG_fmstate { /*{{{*/
	struct TAG_fmstate *prev;			/* previous state (when stacking) */
	fmnode_t *temp;					/* where the model ends up */
	fmnode_t **fvars;				/* free variables (params) */
	int fvars_cur, fvars_max;			/* current, max */
	fmnode_t **target;				/* where the next item goes */
	fmmset_t *setref;				/* set reference (for finding assorted things) */
	fmnode_t *ndlist;				/* list of non-determinstic processes collected during ALT traversals */
} fmstate_t;

/*}}}*/
typedef struct TAG_fmhfp { /*{{{*/
	char *procname;
	fmnode_t **fpts;				/* fixpoints */
	int fpts_cur, fpts_max;				/* current, maximum */
} fmhfp_t;

/*}}}*/
typedef struct TAG_fmnodeitem { /*{{{*/
	fmnode_t *ptr;					/* pointer to the node in question */
	fmnode_t ***refs;				/* array of pointers to all node references */
	int refs_cur, refs_max;				/* current, max */
} fmnodeitem_t;
/*}}}*/
typedef struct TAG_fmnodelist { /*{{{*/
	fmnodeitem_t **items;				/* array of node items and references */
	int items_cur, items_max;			/* current, max */

	/* used intermittently */
	fmtype_e findtype;
} fmnodelist_t;
/*}}}*/
/*}}}*/
/*{{{  private data*/

PRIVATE int atom_counter = 0;
PRIVATE int event_counter = 0;


/*}}}*/
/*{{{  forward decls*/

PRIVATE void fmt_addtonodelist (fmnode_t *listnode, fmnode_t *item);

/*}}}*/


/*{{{  PRIVATE void fmt_addtolist (void ***iptr, int *cptr, int *mptr, void *item)*/
/*
 *	adds an item to a list (generic)
 */
PRIVATE void fmt_addtolist (void ***iptr, int *cptr, int *mptr, void *item)
{
	int i;

	if (!*mptr || !*iptr) {
		/* allocate list */
		*mptr = 16;
		*iptr = (void **)memalloc (*mptr * sizeof (void *));
		for (i=0; i<*mptr; i++) {
			(*iptr)[i] = NULL;
		}
	} else if (*cptr == *mptr) {
		/* more space please */
		void **cur = *iptr;

		*iptr = (void **)memalloc ((*mptr + 16) * sizeof (void *));
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
/*{{{  PRIVATE void fmt_delfromlist (void ***iptr, int *cptr, int *mptr, int idx)*/
/*
 *	removes an item from a specific position in a list (generic)
 */
PRIVATE void fmt_delfromlist (void ***iptr, int *cptr, int *mptr, int idx)
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
	(*iptr)[i] = NULL;
}
/*}}}*/
/*{{{  PRIVATE void fmt_insertintolist (void ***iptr, int *cptr, int *mptr, int idx, void *item)*/
/*
 *	inserts an item into a list at a specific position (generic)
 */
PRIVATE void fmt_insertintolist (void ***iptr, int *cptr, int *mptr, int idx, void *item)
{
	int i;

	if (!*mptr || !*iptr) {
		/* allocate list */
		*mptr = 16;
		*iptr = (void **)memalloc (*mptr * sizeof (void *));
		for (i=0; i<*mptr; i++) {
			(*iptr)[i] = NULL;
		}
	} else if (*cptr == *mptr) {
		/* more space please */
		void **cur = *iptr;

		*iptr = (void **)memalloc ((*mptr + 16) * sizeof (void *));
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
	case FM_THEN:
		fmn->u.fmlist.items = NULL;
		fmn->u.fmlist.items_cur = 0;
		fmn->u.fmlist.items_max = 0;
		break;
	case FM_INSTANCE:
		fmn->u.fminst.pref = NULL;
		fmn->u.fminst.args = NULL;
		fmn->u.fminst.args_cur = 0;
		fmn->u.fminst.args_max = 0;
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
	case FM_SYNC:
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
		fmn->u.fmtree.typename = NULL;
		fmn->u.fmtree.nodetype = NULL;
		break;
	case FM_NODEREF:
		fmn->u.fmnode.node = NULL;
		break;
	case FM_EVENT:
		fmn->u.fmevent.name = NULL;
		fmn->u.fmevent.node = NULL;
		fmn->u.fmevent.typename = NULL;
		fmn->u.fmevent.nodetype = NULL;
		fmn->u.fmevent.isclaim = 0;
		fmn->u.fmevent.isrelease = 0;
		break;
	case FM_EVENTSET:
		fmn->u.fmevset.name = NULL;
		fmn->u.fmevset.node = NULL;
		fmn->u.fmevset.typename = NULL;
		fmn->u.fmevset.nodetype = NULL;
		fmn->u.fmevset.events = NULL;
		fmn->u.fmevset.events_cur = 0;
		fmn->u.fmevset.events_max = 0;
		break;
	case FM_NAMEDPROC:
		fmn->u.fmproc.name = NULL;
		fmn->u.fmproc.parms = NULL;
		fmn->u.fmproc.parms_cur = 0;
		fmn->u.fmproc.parms_max = 0;
		fmn->u.fmproc.body = NULL;
		break;
	case FM_TAGSET:
		fmn->u.fmtset.name = NULL;
		fmn->u.fmtset.tags = NULL;
		fmn->u.fmtset.tags_cur = 0;
		fmn->u.fmtset.tags_max = 0;
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
			/*{{{  NAMEDPROC*/
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
			/*}}}*/
			/*{{{  TAGSET*/
		case FM_TAGSET:
			if (deep == 2) {
				int i;

				for (i=0; i<fmn->u.fmtset.tags_cur; i++) {
					if (fmn->u.fmtset.tags[i]) {
						fmt_freenode (fmn->u.fmtset.tags[i], deep);
					}
				}
			}
			if (fmn->u.fmtset.tags) {
				memfree (fmn->u.fmtset.tags);
				fmn->u.fmtset.tags = NULL;
				fmn->u.fmtset.tags_cur = 0;
				fmn->u.fmtset.tags_max = 0;
			}
			if (fmn->u.fmtset.name) {
				memfree (fmn->u.fmtset.name);
				fmn->u.fmtset.name = NULL;
			}
			break;
			/*}}}*/
			/*{{{  SEQ,PAR,DET,NDET,THEN*/
		case FM_SEQ:
		case FM_PAR:
		case FM_DET:
		case FM_NDET:
		case FM_THEN:
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
			/*}}}*/
			/*{{{  INSTANCE*/
		case FM_INSTANCE:
			if (deep == 2) {
				int i;

				for (i=0; i<fmn->u.fminst.args_cur; i++) {
					if (fmn->u.fminst.args[i]) {
						fmt_freenode (fmn->u.fminst.args[i], deep);
					}
				}
			}
			if (fmn->u.fminst.args) {
				memfree (fmn->u.fminst.args);
				fmn->u.fminst.args = NULL;
				fmn->u.fminst.args_cur = 0;
				fmn->u.fminst.args_max = 0;
			}
			fmn->u.fminst.pref = NULL;
			break;
			/*}}}*/
			/*{{{  FIXPOINT*/
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
			/*}}}*/
			/*{{{  INPUT,OUTPUT,SYNC*/
		case FM_INPUT:
		case FM_OUTPUT:
		case FM_SYNC:
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
			/*}}}*/
			/*{{{  ATOM*/
		case FM_ATOM:
			if (fmn->u.fmatom.id) {
				memfree (fmn->u.fmatom.id);
				fmn->u.fmatom.id = NULL;
			}
			break;
			/*}}}*/
			/*{{{  TREEREF*/
		case FM_TREEREF:
			if (fmn->u.fmtree.name) {
				memfree (fmn->u.fmtree.name);
				fmn->u.fmtree.name = NULL;
			}
			fmn->u.fmtree.node = NULL;
			if (fmn->u.fmtree.typename) {
				memfree (fmn->u.fmtree.typename);
				fmn->u.fmtree.typename = NULL;
			}
			fmn->u.fmtree.nodetype = NULL;
			break;
			/*}}}*/
			/*{{{  NODEREF*/
		case FM_NODEREF:
			fmn->u.fmnode.node = NULL;
			break;
			/*}}}*/
			/*{{{  EVENT*/
		case FM_EVENT:
			if (fmn->u.fmevent.name) {
				memfree (fmn->u.fmevent.name);
				fmn->u.fmevent.name = NULL;
			}
			fmn->u.fmevent.node = NULL;
			if (fmn->u.fmevent.typename) {
				memfree (fmn->u.fmevent.typename);
				fmn->u.fmevent.typename = NULL;
			}
			fmn->u.fmevent.nodetype = NULL;
			break;
			/*}}}*/
			/*{{{  EVENTSET*/
		case FM_EVENTSET:
			if (fmn->u.fmevent.name) {
				memfree (fmn->u.fmevent.name);
				fmn->u.fmevent.name = NULL;
			}
			fmn->u.fmevent.node = NULL;
			if (fmn->u.fmevent.typename) {
				memfree (fmn->u.fmevent.typename);
				fmn->u.fmevent.typename = NULL;
			}
			fmn->u.fmevent.nodetype = NULL;
			if (deep == 2) {
				int i;

				for (i=0; i<fmn->u.fmevset.events_cur; i++) {
					if (fmn->u.fmevset.events[i]) {
						fmt_freenode (fmn->u.fmevset.events[i], deep);
					}
				}
			}
			if (fmn->u.fmevset.events) {
				memfree (fmn->u.fmevset.events);
				fmn->u.fmevset.events = NULL;
				fmn->u.fmevset.events_cur = 0;
				fmn->u.fmevset.events_max = 0;
			}
			break;
			/*}}}*/
			/*{{{  SKIP,STOP,DIV,CHAOS*/
		case FM_SKIP:
		case FM_STOP:
		case FM_DIV:
		case FM_CHAOS:
			break;
			/*}}}*/
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
	fms->setref = NULL;
	fms->ndlist = NULL;

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
	if (fmstate->ndlist) {
		fmt_freenode (fmstate->ndlist, 2);
		fmstate->ndlist = NULL;
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
/*{{{  PRIVATE fmhfp_t *fmt_newhfp (void)*/
/*
 *	creates a new fmhfp_t structure
 */
PRIVATE fmhfp_t *fmt_newhfp (void)
{
	fmhfp_t *hfp = (fmhfp_t *)memalloc (sizeof (fmhfp_t));

	hfp->procname = NULL;
	hfp->fpts = NULL;
	hfp->fpts_cur = 0;
	hfp->fpts_max = 0;

	return hfp;
}
/*}}}*/
/*{{{  PRIVATE void fmt_freehfp (fmhfp_t *hfp)*/
/*
 *	frees a fmhfp_t structure
 */
PRIVATE void fmt_freehfp (fmhfp_t *hfp)
{
	if (!hfp) {
		fmt_error_internal (NOPOSN, "fmt_freehfp(): NULL pointer!");
		return;
	}
	if (hfp->fpts) {
		memfree (hfp->fpts);
		hfp->fpts = NULL;
		hfp->fpts_cur = 0;
		hfp->fpts_max = 0;
	}
	memfree (hfp);
	return;
}	
/*}}}*/
/*{{{  PRIVATE fmnodeitem_t *fmt_newnodeitem (void)*/
/*
 *	creates a new fmnodeitem_t structure
 */
PRIVATE fmnodeitem_t *fmt_newnodeitem (void)
{
	fmnodeitem_t *itm = (fmnodeitem_t *)memalloc (sizeof (fmnodeitem_t));

	itm->ptr = NULL;
	itm->refs = NULL;
	itm->refs_cur = 0;
	itm->refs_max = 0;

	return itm;
}
/*}}}*/
/*{{{  PRIVATE void fmt_freenodeitem (fmnodeitem_t *itm)*/
/*
 *	frees a fmnodeitem_t structure
 */
PRIVATE void fmt_freenodeitem (fmnodeitem_t *itm)
{
	if (!itm) {
		fmt_error_internal (NOPOSN, "fmt_freenodeitem(): NULL pointer!");
		return;
	}
	if (itm->refs) {
		memfree (itm->refs);
		itm->refs = NULL;
		itm->refs_cur = 0;
		itm->refs_max = 0;
	}
	memfree (itm);
	return;
}
/*}}}*/
/*{{{  PRIVATE fmnodelist_t *fmt_newnodelist (void)*/
/*
 *	creates a new fmnodelist_t structure
 */
PRIVATE fmnodelist_t *fmt_newnodelist (void)
{
	fmnodelist_t *lst = (fmnodelist_t *)memalloc (sizeof (fmnodelist_t));

	lst->items = NULL;
	lst->items_cur = 0;
	lst->items_max = 0;

	lst->findtype = FM_INVALID;

	return lst;
}
/*}}}*/
/*{{{  PRIVATE void fmt_freenodelist (fmnodelist_t *lst)*/
/*
 *	frees a fmnodelist_t structure (recursive)
 */
PRIVATE void fmt_freenodelist (fmnodelist_t *lst)
{
	if (!lst) {
		fmt_error_internal (NOPOSN, "fmt_freenodelist(): NULL pointer!");
		return;
	}
	if (lst->items) {
		memfree (lst->items);
		lst->items = NULL;
		lst->items_cur = 0;
		lst->items_max = 0;
	}
	memfree (lst);
	return;
}
/*}}}*/
/*{{{  PRIVATE fmnode_t *fmt_copynode (fmnode_t *n)*/
/*
 *	duplicates a formal model node/tree
 */
PRIVATE fmnode_t *fmt_copynode (fmnode_t *n)
{
	fmnode_t *c = NULL;

	if (!n) {
		return NULL;
	}
	switch (n->type) {
		/*{{{  EVENT*/
	case FM_EVENT:
		c = fmt_newnode (FM_EVENT, n->org);
		if (n->u.fmevent.name) {
			c->u.fmevent.name = (char *)memalloc (strlen (n->u.fmevent.name) + 2);
			strcpy (c->u.fmevent.name, n->u.fmevent.name);
		}
		c->u.fmevent.node = n->u.fmevent.node;
		if (n->u.fmevent.typename) {
			c->u.fmevent.typename = (char *)memalloc (strlen (n->u.fmevent.typename) + 2);
			strcpy (c->u.fmevent.typename, n->u.fmevent.typename);
		}
		c->u.fmevent.nodetype = n->u.fmevent.nodetype;
		break;
		/*}}}*/
		/*{{{  NODEREF*/
	case FM_NODEREF:
		c = fmt_newnode (FM_NODEREF, n->org);
		c->u.fmnode.node = n->u.fmnode.node;
		break;
		/*}}}*/
		/*{{{  INSTANCE*/
	case FM_INSTANCE:
		{
			int i;

			c = fmt_newnode (FM_INSTANCE, n->org);
			c->u.fminst.pref = n->u.fminst.pref;

			for (i=0; i<n->u.fminst.args_cur; i++) {
				fmnode_t *argcopy = fmt_copynode (n->u.fminst.args[i]);

				fmt_addtonodelist (c, argcopy);
			}
		}
		break;
		/*}}}*/
	default:
		fmt_error_internal (NOPOSN, "fmt_copynode(): unhandled type in node copy!");
		break;
	}
	return c;
}
/*}}}*/


/*{{{  PRIVATE void fmt_addtonodelist (fmnode_t *listnode, fmnode_t *item)*/
/*
 *	adds an item to a node list
 */
PRIVATE void fmt_addtonodelist (fmnode_t *listnode, fmnode_t *item)
{
	if (!listnode || !item) {
		fmt_error_internal (NOPOSN, "fmt_addtonodelist(): NULL pointer!");
		return;
	}
	switch (listnode->type) {
	case FM_SEQ:
	case FM_PAR:
	case FM_DET:
	case FM_NDET:
	case FM_THEN:
		fmt_addtolist ((void ***)&listnode->u.fmlist.items, &listnode->u.fmlist.items_cur, &listnode->u.fmlist.items_max, (void *)item);
		break;
	case FM_EVENTSET:
		fmt_addtolist ((void ***)&listnode->u.fmevset.events, &listnode->u.fmevset.events_cur, &listnode->u.fmevset.events_max, (void *)item);
		break;
	case FM_TAGSET:
		fmt_addtolist ((void ***)&listnode->u.fmtset.tags, &listnode->u.fmtset.tags_cur, &listnode->u.fmtset.tags_max, (void *)item);
		break;
	case FM_INSTANCE:
		fmt_addtolist ((void ***)&listnode->u.fminst.args, &listnode->u.fminst.args_cur, &listnode->u.fminst.args_max, (void *)item);
		break;
	case FM_NAMEDPROC:
		fmt_addtolist ((void ***)&listnode->u.fmproc.parms, &listnode->u.fmproc.parms_cur, &listnode->u.fmproc.parms_max, (void *)item);
		break;
	default:
		fmt_error_internal (NOPOSN, "fmt_addtonodelist(): not list capable node!");
		break;
	}
}
/*}}}*/
/*{{{  PRIVATE void fmt_delfromnodelist (fmnode_t *listnode, int idx)*/
/*
 *	deletes an item from a node list
 */
PRIVATE void fmt_delfromnodelist (fmnode_t *listnode, int idx)
{
	if (!listnode) {
		fmt_error_internal (NOPOSN, "fmt_delfromnodelist(): NULL pointer!");
		return;
	}
	switch (listnode->type) {
	case FM_SEQ:
	case FM_PAR:
	case FM_DET:
	case FM_NDET:
	case FM_THEN:
		fmt_delfromlist ((void ***)&listnode->u.fmlist.items, &listnode->u.fmlist.items_cur, &listnode->u.fmlist.items_max, idx);
		break;
	case FM_EVENTSET:
		fmt_delfromlist ((void ***)&listnode->u.fmevset.events, &listnode->u.fmevset.events_cur, &listnode->u.fmevset.events_max, idx);
		break;
	case FM_TAGSET:
		fmt_delfromlist ((void ***)&listnode->u.fmtset.tags, &listnode->u.fmtset.tags_cur, &listnode->u.fmtset.tags_max, idx);
		break;
	case FM_INSTANCE:
		fmt_delfromlist ((void ***)&listnode->u.fminst.args, &listnode->u.fminst.args_cur, &listnode->u.fminst.args_max, idx);
		break;
	case FM_NAMEDPROC:
		fmt_delfromlist ((void ***)&listnode->u.fmproc.parms, &listnode->u.fmproc.parms_cur, &listnode->u.fmproc.parms_max, idx);
		break;
	default:
		fmt_error_internal (NOPOSN, "fmt_delfromnodelist(): not list capable node!");
		break;
	}
}
/*}}}*/
/*{{{  PRIVATE void fmt_insertintonodelist (fmnode_t *listnode, int idx, fmnode_t *item)*/
/*
 *	inserts an item into a list of I/O processes
 */
PRIVATE void fmt_insertintonodelist (fmnode_t *listnode, int idx, fmnode_t *item)
{
	if (!listnode) {
		fmt_error_internal (NOPOSN, "fmt_insertintonodelist(): NULL pointer!");
		return;
	}
	switch (listnode->type) {
	case FM_SEQ:
	case FM_PAR:
	case FM_DET:
	case FM_NDET:
	case FM_THEN:
		fmt_insertintolist ((void ***)&listnode->u.fmlist.items, &listnode->u.fmlist.items_cur, &listnode->u.fmlist.items_max, idx, (void *)item);
		break;
	case FM_EVENTSET:
		fmt_insertintolist ((void ***)&listnode->u.fmevset.events, &listnode->u.fmevset.events_cur, &listnode->u.fmevset.events_max, idx, (void *)item);
		break;
	case FM_TAGSET:
		fmt_insertintolist ((void ***)&listnode->u.fmtset.tags, &listnode->u.fmtset.tags_cur, &listnode->u.fmtset.tags_max, idx, (void *)item);
		break;
	case FM_INSTANCE:
		fmt_insertintolist ((void ***)&listnode->u.fminst.args, &listnode->u.fminst.args_cur, &listnode->u.fminst.args_max, idx, (void *)item);
		break;
	case FM_NAMEDPROC:
		fmt_insertintolist ((void ***)&listnode->u.fmproc.parms, &listnode->u.fmproc.parms_cur, &listnode->u.fmproc.parms_max, idx, (void *)item);
		break;
	default:
		fmt_error_internal (NOPOSN, "fmt_insertintonodelist(): not list capable node!");
		break;
	}
}
/*}}}*/
/*{{{  PRIVATE void fmt_addtovarslist (fmstate_t *fmstate, fmnode_t *item)*/
/*
 *	adds something to the free-variables list in the given state node
 */
PRIVATE void fmt_addtovarslist (fmstate_t *fmstate, fmnode_t *item)
{
	fmt_addtolist ((void ***)&fmstate->fvars, &fmstate->fvars_cur, &fmstate->fvars_max, (void *)item);
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
		case FM_THEN:
			for (i=0; i<n->u.fmlist.items_cur; i++) {
				fmt_modprewalk (&n->u.fmlist.items[i], f1, voidptr);
			}
			break;
		case FM_INSTANCE:
			for (i=0; i<n->u.fminst.args_cur; i++) {
				fmt_modprewalk (&n->u.fminst.args[i], f1, voidptr);
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
		case FM_TAGSET:
			for (i=0; i<n->u.fmtset.tags_cur; i++) {
				fmt_modprewalk (&n->u.fmtset.tags[i], f1, voidptr);
			}
			break;
		case FM_SKIP:
		case FM_STOP:
		case FM_DIV:
		case FM_CHAOS:
		case FM_ATOM:
		case FM_TREEREF:
		case FM_NODEREF:
		case FM_EVENT:
		case FM_EVENTSET:
			break;
		case FM_INPUT:
		case FM_OUTPUT:
		case FM_SYNC:
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
/*{{{  PRIVATE void fmt_copyprotname (char *dest, const char *src)*/
/*
 *	copies a protocol name from occam-pi style name to FDR style name (for a datatype name)
 */
PRIVATE void fmt_copyprotname (char *dest, const char *src)
{
	const char *ch = src;
	char *dh;

	for (dh = dest; *ch != '\0'; ch++, dh++) {
		if (*ch == '.') {
			*dh = '_';
		} else if (islower (*ch)) {
			*dh = toupper (*ch);
		} else {
			*dh = *ch;
		}
	}
	*dh = '\0';
}
/*}}}*/
/*{{{  PRIVATE void fmt_copytagname (char *dest, const char *src)*/
/*
 *	copies a tagname from occam-pi style name to FDR style tagname (for a datatype)
 */
PRIVATE void fmt_copytagname (char *dest, const char *src)
{
	const char *ch = src;
	char *dh;

	for (dh = dest; *ch != '\0'; ch++, dh++) {
		if (*ch == '.') {
			ch++;
			if (islower (*ch)) {
				*dh = toupper (*ch);
			} else {
				*dh = *ch;
			}
		} else {
			if (ch == src) {
				/* first character special */
				if (islower (*ch)) {
					*dh = toupper (*ch);
				} else {
					*dh = *ch;
				}
			} else if (isupper (*ch)) {
				*dh = tolower (*ch);
			} else {
				*dh = *ch;
			}
		}
	}
	*dh = '\0';
}
/*}}}*/
/*{{{  PRIVATE void fmt_copyvarname (char *dest, const char *src)*/
/*
 *	copies a variable name from occam-pi style to FDR style name
 */
PRIVATE void fmt_copyvarname (char *dest, const char *src)
{
	const char *ch = src;
	char *dh;

	for (dh = dest; *ch != '\0'; ch++, dh++) {
		if (*ch == '.') {
			*dh = '_';
		} else if (isupper (*ch)) {
			*dh = tolower (*ch);
		} else {
			*dh = *ch;
		}
	}
	*dh = '\0';
}
/*}}}*/
/*{{{  PRIVATE BOOL fmt_isevent (fmnode_t *item)*/
/*
 *	returns TRUE if the specified node is an event (INPUT/OUTPUT)
 */
PRIVATE BOOL fmt_isevent (fmnode_t *item)
{
	switch (item->type) {
	case FM_INPUT:
	case FM_OUTPUT:
	case FM_SYNC:
		return TRUE;
	default:
		return FALSE;
	}
}
/*}}}*/
/*{{{  PRIVATE fmnode_t *fmt_createeventfromvar (treenode *var, BOOL donumber)*/
/*
 *	creates an FM_EVENT/FM_EVENTSET based on the given 'var' namenode
 */
PRIVATE fmnode_t *fmt_createeventfromvar (treenode *var, BOOL donumber)
{
	treenode *type = chk_gettype (var);
	char *varname = (char *)WNameOf (NNameOf (var));

	if (TagOf (type) == S_CHAN) {
		/*{{{  simple channel*/
		treenode *proto = ProtocolOf (type);
		fmnode_t *pnode = fmt_newnode (FM_EVENT, var);
		char *str = (char *)memalloc (strlen (varname) + 16);
		int slen = 0;

#if 0
fprintf (stderr, "do_formalmodelcheck_tree(): CHAN parameter, protocol is:");
printtreenl (stderr, 1, proto);
#endif
		fmt_copyvarname (str, varname);
		slen = strlen (str);
		if (donumber) {
			/* number this event globally */
			sprintf (str + slen, "%d", event_counter++);
		}
		pnode->u.fmevent.name = str;
		pnode->u.fmevent.node = var;
		if (proto) {
			switch (TagOf (proto)) {
			case N_TPROTDEF:
				{
					char *tpname = (char *)WNameOf (NNameOf (proto));
					char *tpstr = (char *)memalloc (strlen (tpname) + 8);
					
					sprintf (tpstr, "PROT_");
					fmt_copyprotname (tpstr + 5, tpname);
					pnode->u.fmevent.nodetype = proto;
					pnode->u.fmevent.typename = tpstr;
				}
				break;
			}
		}

		return pnode;
		/*}}}*/
	} else if (isdynamicmobilechantype (type)) {
		/*{{{  mobile channel-type*/
		fmnode_t *esnode = fmt_newnode (FM_EVENTSET, var);
		char *str = (char *)memalloc (strlen (varname) + 16);
		treenode *vtype = NTypeOf (var);
		int is_shared = 0;
		int slen = 0;

		fmt_copyvarname (str, varname);
		slen = strlen (str);
		if (donumber) {
			/* number this event globally */
			sprintf (str + slen, "%d", event_counter++);
		}
		esnode->u.fmevset.name = str;
		esnode->u.fmevset.node = var;
#if 0
fprintf (stderr, "do_formalmodelcheck_tree(): mobile channel-type vtype:");
printtreenl (stderr, 1, vtype);
#endif
		if (TagOf (vtype) == N_TYPEDECL) {
			/* should be a mobile channel-type */
			treenode *mtype = NTypeOf (vtype);
			char *typename = (char *)WNameOf (NNameOf (vtype));
			char *tstr = (char *)memalloc (strlen (typename) + 6);

			sprintf (tstr, "CT_");
			fmt_copyprotname (tstr + 3, typename);
			esnode->u.fmevset.nodetype = vtype;
			esnode->u.fmevset.typename = tstr;

			is_shared = NTypeAttrOf (vtype) & TypeAttr_shared;

#if 0
fprintf (stderr, "do_formalmodelcheck_tree(): mobile channel-type, is_shared=%d, mtype =\n", (unsigned int)NTypeAttrOf (vtype));
printtreenl (stderr, 1, mtype);
#endif
			/* actual type should be the mobile record of channels */
			if ((TagOf (mtype) == S_MOBILE) && (TagOf (ARTypeOf (type)) == S_RECORD)) {
				treenode *items = ARTypeOf (ARTypeOf (type));

				/* if mobile, expecting declaration list of items */
				while (items && (TagOf (items) == S_DECL)) {
					treenode *dname = DNameOf (items);
					treenode *dtype = NTypeOf (dname);

					if (TagOf (dtype) == S_CHAN) {
						/*{{{  channel of something*/
						treenode *proto = ProtocolOf (dtype);
						fmnode_t *pnode = fmt_newnode (FM_EVENT, dname);
						char *itemname = (char *)WNameOf (NNameOf (dname));
						char *str = (char *)memalloc (strlen (varname) + strlen (itemname) + 16);
						int slen = 0;
#if 0
fprintf (stderr, "do_formalmodelcheck_tree(): mobile channel-type item name:");
printtreenl (stderr, 1, dname);
#endif

						fmt_copyvarname (str, varname);
						slen = strlen (str);
						if (donumber) {
							/* number event globally (will be event_counter-1) */
							slen += sprintf (str + slen, "%d", event_counter-1);
						}
						str[slen] = '_';
						slen++;
						fmt_copyvarname (str + slen, itemname);
						pnode->u.fmevent.name = str;
						pnode->u.fmevent.node = dname;
						if (proto) {
							switch (TagOf (proto)) {
								/*{{{  N_TPROTDEF -- tagged protocol definition*/
							case N_TPROTDEF:
								{
									char *tpname = (char *)WNameOf (NNameOf (proto));
									char *tpstr = (char *)memalloc (strlen (tpname) + 8);
									
									sprintf (tpstr, "PROT_");
									fmt_copyprotname (tpstr + 5, tpname);
									pnode->u.fmevent.nodetype = proto;
									pnode->u.fmevent.typename = tpstr;
								}
								break;
								/*}}}*/
							}
						}

						fmt_addtonodelist (esnode, pnode);
						/*}}}*/
					}
					items = DBodyOf (items);
				}
			}

		}

		if (is_shared) {
			/* add claim and release events */
			fmnode_t *cln = fmt_newnode (FM_EVENT, var);
			fmnode_t *rln = fmt_newnode (FM_EVENT, var);
			char *str;
			int slen = 0;

			str = (char *)memalloc (strlen (varname) + 32);
			fmt_copyvarname (str, varname);
			slen = strlen (str);
			if (donumber) {
				/* number event globally (will be event_counter-1) */
				slen += sprintf (str + slen, "%d", event_counter-1);
			}
			sprintf (str + slen, "_Claim");
			cln->u.fmevent.name = str;
			cln->u.fmevent.node = var;
			cln->u.fmevent.isclaim = 1;

			str = (char *)memalloc (strlen (varname) + 32);
			fmt_copyvarname (str, varname);
			slen = strlen (str);
			if (donumber) {
				/* number event globally (will be event_counter-1) */
				slen += sprintf (str + slen, "%d", event_counter-1);
			}
			sprintf (str + slen, "_Release");
			rln->u.fmevent.name = str;
			rln->u.fmevent.node = var;
			rln->u.fmevent.isrelease = 1;

			fmt_addtonodelist (esnode, cln);
			fmt_addtonodelist (esnode, rln);
		}

		return esnode;
		/*}}}*/
	}

	return NULL;
}
/*}}}*/
/*{{{  PRIVATEPARAM int fmt_dofindrefstotype (fmnode_t **nodep, void *voidptr)*/
/*
 *	used in searching for references to a specific node type
 */
PRIVATEPARAM int fmt_dofindrefstotype (fmnode_t **nodep, void *voidptr)
{
	fmnodelist_t *nl = (fmnodelist_t *)voidptr;
	fmnode_t *n = *nodep;

	if (n->type == FM_NODEREF) {
		fmnode_t *refd = n->u.fmnode.node;		/* referenced node */

		if (refd && (refd->type == nl->findtype)) {
			fmnodeitem_t *nitm = NULL;
			int i;

			for (i=0; i<nl->items_cur; i++) {
				if (nl->items[i]->ptr == refd) {
					nitm = nl->items[i];		/* this one */
					break;
				}
			}
			if (!nitm) {
				/* no item yet, create and add */
				nitm = fmt_newnodeitem ();
				nitm->ptr = refd;

				fmt_addtolist ((void ***)&nl->items, &nl->items_cur, &nl->items_max, (void *)nitm);
			}

			/* see if we already have this reference, shouldn't! */
			for (i=0; i<nitm->refs_cur; i++) {
				if (nitm->refs[i] == &n->u.fmnode.node) {
					fmt_error_internal (NOPOSN, "fmt_dofindrefstotype(): duplicate reference in tree walk!");
					break;
				}
			}
			if (i == nitm->refs_cur) {
				/* add new reference */
				fmt_addtolist ((void ***)&nitm->refs, &nitm->refs_cur, &nitm->refs_max, (void *)&n->u.fmnode.node);
			}
		}
	}

	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATE fmnodelist_t *fmt_findrefstotype (fmnode_t *tree, fmtype_e type)*/
/*
 *	searches a given formal-model for references to the specified node type
 */
PRIVATE fmnodelist_t *fmt_findrefstotype (fmnode_t *tree, fmtype_e type)
{
	fmnodelist_t *nl = fmt_newnodelist ();

	nl->findtype = type;
	fmt_modprewalk (&tree, fmt_dofindrefstotype, (void *)nl);
	nl->findtype = FM_INVALID;

	return nl;
}
/*}}}*/
/*{{{  PRIVATE void fmt_addfindrefstotype (fmnode_t *tree, fmnodelist_t *nl, fmtype_e type)*/
/*
 *	searches a given formal-model for references to the specified node type, adds to existing list
 */
PRIVATE void fmt_addfindrefstotype (fmnode_t *tree, fmnodelist_t *nl, fmtype_e type)
{
	nl->findtype = type;
	fmt_modprewalk (&tree, fmt_dofindrefstotype, (void *)nl);
	nl->findtype = FM_INVALID;
	return;
}
/*}}}*/
/*{{{  PRIVATEPARAM int fmt_dofindrefstonode (fmnode_t **nodep, void *voidptr)*/
/*
 *	used in searching for references to a specific node
 */
PRIVATEPARAM int fmt_dofindrefstonode (fmnode_t **nodep, void *voidptr)
{
	fmnodeitem_t *itm = (fmnodeitem_t *)voidptr;
	fmnode_t *n = *nodep;

	if (n->type == FM_NODEREF) {
		fmnode_t *refd = n->u.fmnode.node;

		if (refd == itm->ptr) {
			int i;

			for (i=0; i<itm->refs_cur; i++) {
				if (itm->refs[i] == nodep) {
					fmt_error_internal (NOPOSN, "fmt_dofindrefstonode(): duplicate reference in tree walk!");
					break;
				}
			}
			if (i == itm->refs_cur) {
				/* add new reference */
				fmt_addtolist ((void ***)&itm->refs, &itm->refs_cur, &itm->refs_max, nodep);
			}
		}
	}
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATE fmnodeitem_t *fmt_findrefstonode (fmnode_t **nodep, fmnode_t *node)*/
/*
 *	searches a given formal-model for references to the specified node, returns the collection of NODEREF ptrs
 */
PRIVATE fmnodeitem_t *fmt_findrefstonode (fmnode_t **nodep, fmnode_t *node)
{
	fmnodeitem_t *itm = fmt_newnodeitem ();

	itm->ptr = node;
	fmt_modprewalk (nodep, fmt_dofindrefstonode, (void *)itm);

	return itm;
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
		case FM_THEN:
			fprintf (stream, "THEN\n");
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
		case FM_SYNC:
			fprintf (stream, "SYNC\n");
			break;
		case FM_ATOM:
			fprintf (stream, "ATOM (%s)\n", fmn->u.fmatom.id);
			break;
		case FM_TREEREF:
			fprintf (stream, "TREEREF (0x%8.8x,%s,%s,", (unsigned int)fmn->u.fmtree.node, tagstring (TagOf (fmn->u.fmtree.node)),
					fmn->u.fmtree.name ?: "(no-name)");
			fprintf (stream, "0x%8.8x,%s,%s)\n", (unsigned int)fmn->u.fmtree.nodetype, fmn->u.fmtree.nodetype ? tagstring (TagOf (fmn->u.fmtree.nodetype)) : "(no-type)",
					fmn->u.fmtree.typename ?: "(no-type-name)");
			break;
		case FM_NODEREF:
			fprintf (stream, "NODEREF (0x%8.8x)\n", (unsigned int)fmn->u.fmnode.node);
			break;
		case FM_EVENT:
			fprintf (stream, "EVENT (0x%8.8x,%s,%s,", (unsigned int)fmn->u.fmevent.node, tagstring (TagOf (fmn->u.fmevent.node)),
					fmn->u.fmevent.name ?: "(no-name)");
			fprintf (stream, "0x%8.8x,%s,%s)\n", (unsigned int)fmn->u.fmevent.nodetype, fmn->u.fmevent.nodetype ? tagstring (TagOf (fmn->u.fmevent.nodetype)) : "(no-type)",
					fmn->u.fmevent.typename ?: "(no-type-name)");
			break;
		case FM_EVENTSET:
			fprintf (stream, "EVENTSET (0x%8.8x,%s,%s,", (unsigned int)fmn->u.fmevset.node, tagstring (TagOf (fmn->u.fmevset.node)),
					fmn->u.fmevset.name ?: "(no-name)");
			fprintf (stream, "0x%8.8x,%s,%s)\n", (unsigned int)fmn->u.fmevset.nodetype, fmn->u.fmevset.nodetype ? tagstring (TagOf (fmn->u.fmevset.nodetype)) : "(no-type)",
					fmn->u.fmevset.typename ?: "(no-type-name)");
			break;
		case FM_NAMEDPROC:
			fprintf (stream, "NAMEDPROC (%s) %d parms\n", fmn->u.fmproc.name ?: "(no-name)", fmn->u.fmproc.parms_cur);
			break;
		case FM_TAGSET:
			fprintf (stream, "TAGSET (%s) %d tags\n", fmn->u.fmtset.name ?: "(no-name)", fmn->u.fmtset.tags_cur);
			break;
		case FM_INSTANCE:
			fprintf (stream, "INSTANCE (");
			if (!fmn->u.fminst.pref) {
				fprintf (stream, "null");
			} else if (fmn->u.fminst.pref->type == FM_NAMEDPROC) {
				fprintf (stream, "OF %s", fmn->u.fminst.pref->u.fmproc.name ?: "(no-name)");
			} else if (fmn->u.fminst.pref->type == FM_ATOM) {
				fprintf (stream, "OF ATOM (%s)", fmn->u.fminst.pref->u.fmatom.id);
			} else {
				fprintf (stream, "OF unknown-%d", (int)fmn->u.fminst.pref->type);
			}
			fprintf (stream, ") %d args\n", fmn->u.fminst.args_cur);
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
		case FM_THEN:
			for (i=0; i<fmn->u.fmlist.items_cur; i++) {
				fmt_dumpnode (fmn->u.fmlist.items[i], indent + 1, stream);
			}
			break;
		case FM_INSTANCE:
			for (i=0; i<fmn->u.fminst.args_cur; i++) {
				fmt_dumpnode (fmn->u.fminst.args[i], indent + 1, stream);
			}
			break;
		case FM_FIXPOINT:
			fmt_dumpnode (fmn->u.fmfix.id, indent + 1, stream);
			fmt_dumpnode (fmn->u.fmfix.proc, indent + 1, stream);
			break;
		case FM_INPUT:
		case FM_OUTPUT:
		case FM_SYNC:
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
		case FM_TAGSET:
			for (i=0; i<fmn->u.fmtset.tags_cur; i++) {
				fmt_dumpnode (fmn->u.fmtset.tags[i], indent + 1, stream);
			}
			break;
		case FM_EVENTSET:
			for (i=0; i<fmn->u.fmevset.events_cur; i++) {
				fmt_dumpnode (fmn->u.fmevset.events[i], indent + 1, stream);
			}
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
			fmnode_t *fvar = state->fvars[i];

			switch (fvar->type) {
			case FM_EVENT:
				if (fvar->u.fmevent.node == n) {
					return fvar;
				}
				break;
			case FM_EVENTSET:
				if (TagOf (n) == S_RECORDSUB) {
					/* probably looking for one of the individual events, ASIndex will be the field */
					int j;

					for (j=0; j<fvar->u.fmevset.events_cur; j++) {
						fmnode_t *ev = fvar->u.fmevset.events[j];

						if ((ev->type == FM_EVENT) && !ev->u.fmevent.isclaim && !ev->u.fmevent.isrelease &&
								(ASIndexOf (n) == ev->u.fmevent.node) && (ASBaseOf (n) == fvar->u.fmevset.node)) {
							return ev;
						}
					}
				} else if (fvar->u.fmevset.node == n) {
					/* looking for the real thing */
					return fvar;
				}
				break;
			default:
				break;
			}
		}
		state = state->prev;
	}
	return NULL;
}
/*}}}*/
/*{{{  PRIVATE fmnode_t *fmt_findfreevar_claim (fmstate_t *state, treenode *n)*/
/*
 *	searches free-variables for a specific tree-node's CLAIM
 */
PRIVATE fmnode_t *fmt_findfreevar_claim (fmstate_t *state, treenode *n)
{
	fmnode_t *evset = fmt_findfreevar (state, n);

	if (evset && (evset->type == FM_EVENTSET)) {
		int i;

		for (i=0; i<evset->u.fmevset.events_cur; i++) {
			fmnode_t *ev = evset->u.fmevset.events[i];

			if ((ev->type == FM_EVENT) && ev->u.fmevent.isclaim) {
				return ev;
			}
		}
	}
	return NULL;
}
/*}}}*/
/*{{{  PRIVATE fmnode_t *fmt_findfreevar_release (fmstate_t *state, treenode *n)*/
/*
 *	searches free-variables for a specific tree-node's CLAIM
 */
PRIVATE fmnode_t *fmt_findfreevar_release (fmstate_t *state, treenode *n)
{
	fmnode_t *evset = fmt_findfreevar (state, n);

	if (evset && (evset->type == FM_EVENTSET)) {
		int i;

		for (i=0; i<evset->u.fmevset.events_cur; i++) {
			fmnode_t *ev = evset->u.fmevset.events[i];

			if ((ev->type == FM_EVENT) && ev->u.fmevent.isrelease) {
				return ev;
			}
		}
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
		/*{{{  SEQ,PAR,NDET,DET*/
	case FM_SEQ:
	case FM_PAR:
	case FM_NDET:
	case FM_DET:
		/* collapse similar subnodes */
		{
			int i, changed;

			do {
				changed = 0;
				for (i=0; i<fmn->u.fmlist.items_cur; i++) {
					fmnode_t *item = fmn->u.fmlist.items[i];

					if (item && (item->type == fmn->type)) {
						/* yes, collapse and remove node */
						int j;

#if 0
fprintf (stderr, "fmt_simplifynode(): same type!\n");
#endif
						fmt_delfromnodelist (fmn, i);
						i--;

						for (j=0; j<item->u.fmlist.items_cur; j++) {
							fmnode_t *subitem = item->u.fmlist.items[j];

							i++;
							fmt_insertintonodelist (fmn, i, subitem);
							item->u.fmlist.items[j] = NULL;
						}

						fmt_freenode (item, 2);
						changed = 1;
					}
				}
			} while (changed);
		}

		/* if we're left with a single item (or just had 1 to start with), remove it */
		if (fmn->u.fmlist.items_cur == 1) {
			fmnode_t *item = fmn->u.fmlist.items[0];

			fmt_delfromnodelist (fmn, 0);
			*nodep = item;
			fmt_freenode (fmn, 2);
		}
		break;
		/*}}}*/
	default:
		break;
	}
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATEPARAM int fmt_dohoistfixpoints (fmnode_t **nodep, void *voidptr)*/
/*
 *	hoists fixpoints for a tree, adds as NAMEDPROCs to list
 */
PRIVATEPARAM int fmt_dohoistfixpoints (fmnode_t **nodep, void *voidptr)
{
	fmhfp_t *hfp = (fmhfp_t *)voidptr;
	fmnode_t *n = *nodep;

	switch (n->type) {
	case FM_FIXPOINT:
		{
			fmnode_t *fmn = fmt_newnode (FM_NAMEDPROC, n->org);
			fmnode_t *atom = n->u.fmfix.id;
			fmnode_t *xinst = NULL;			/* original instance and events */
			fmnode_t *rinst = NULL;			/* new instance and copied events */
			fmnodelist_t *evlist;
			fmnodeitem_t *atlist;
			int i;

			/* hoist fixpoints inside this one first */
			fmt_modprewalk (&n->u.fmfix.proc, fmt_dohoistfixpoints, (void *)hfp);

			if (atom->type != FM_ATOM) {
				fmt_error_internal (NOPOSN, "fmt_dohoistfixpoints(): FIXPOINT id not ATOM");
				return STOP_WALK;
			}

			fmn->u.fmproc.name = (char *)memalloc (strlen (hfp->procname) + strlen (atom->u.fmatom.id) + 3);
			sprintf (fmn->u.fmproc.name, "%s_%s", hfp->procname, atom->u.fmatom.id);
			fmn->u.fmproc.body = n->u.fmfix.proc;
			fmn->u.fmproc.parms = NULL;
			fmn->u.fmproc.parms_cur = 0;
			fmn->u.fmproc.parms_max = 0;

			/* find all event references in the fixpoint process -- these will become new parameters */
			evlist = fmt_findrefstotype (n->u.fmfix.proc, FM_EVENT);
			// fmt_addfindrefstotype (n->u.fmfix.proc, evlist, FM_EVENTSET);
#if 0
fprintf (stderr, "fmt_dohoistfixpoints(): [%s], %d events found\n", hfp->procname, evlist->items_cur);
#endif

			xinst = fmt_newnode (FM_INSTANCE, n->org);
			xinst->u.fminst.pref = fmn;

			rinst = fmt_newnode (FM_INSTANCE, n->org);
			rinst->u.fminst.pref = fmn;

			for (i=0; i<evlist->items_cur; i++) {
				fmnodeitem_t *itm = evlist->items[i];
				fmnode_t *evcopy = fmt_copynode (itm->ptr);		/* make a copy of the event */
				fmnode_t *evref;
				int j;

				/* rewrite references */
				for (j=0; j<itm->refs_cur; j++) {
					fmnode_t **iptr = itm->refs[j];

					if (*iptr != itm->ptr) {
						fmt_error_internal (NOPOSN, "fmt_dohoistfixpoints(): reference changed!");
						return STOP_WALK;
					}
					*iptr = evcopy;
				}

				/* add reference to actual parameters of original fixpoint process instance */
				evref = fmt_newnode (FM_NODEREF, itm->ptr->org);
				evref->u.fmnode.node = itm->ptr;
				fmt_addtonodelist (xinst, evref);

				itm->ptr = evcopy;

				/* add new event to formal parameters of fixpoint process */
				fmt_addtonodelist (fmn, evcopy);

				/* add reference to actual paramters of duplicate process instance */
				evref = fmt_newnode (FM_NODEREF, itm->ptr->org);
				evref->u.fmnode.node = evcopy;
				fmt_addtonodelist (rinst, evref);
			}

			fmt_freenodelist (evlist);

			n->u.fmfix.id = NULL;
			n->u.fmfix.proc = NULL;
			fmt_freenode (n, 1);

			/* find references to the atom, and replace with copies of the temporary instance */
			atlist = fmt_findrefstonode (&fmn->u.fmproc.body, atom);
#if 0
fprintf (stderr, "fmt_dohoistfixpoints(): [%s], %d references to self-atom\n", hfp->procname, atlist->refs_cur);
#endif
			for (i=0; i<atlist->refs_cur; i++) {
				fmnode_t **aptr = atlist->refs[i];
				fmnode_t *nref = *aptr;

				/* put in a duplicate of the 'rinst' instance, trash original */
				*aptr = fmt_copynode (rinst);
				fmt_freenode (nref, 2);
			}
			fmt_freenodeitem (atlist);

			/* trashing atom, replace with instance of new process */
			fmt_freenode (atom, 2);

			/* trashing temporary instance node */
			fmt_freenode (rinst, 2);

			*nodep = xinst;

			fmt_addtolist ((void ***)&hfp->fpts, &hfp->fpts_cur, &hfp->fpts_max, (void *)fmn);
		}
		return STOP_WALK;
	default:
		break;
	}
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATE void fmt_hoistfixpoints (fmnode_t *node, fmmset_t *fmm)*/
/*
 *	hoists fixpoints for a NAMEDPROC, adds to list in fmmset_t
 */
PRIVATE void fmt_hoistfixpoints (fmnode_t *node, fmmset_t *fmm)
{
	fmhfp_t *hfp = fmt_newhfp ();
	int i;

	if (node->type != FM_NAMEDPROC) {
		fmt_error_internal (NOPOSN, "fmt_hoistfixpoints(): not NAMEDPROC!");
		return;
	}

	hfp->procname = node->u.fmproc.name;

	fmt_modprewalk (&node->u.fmproc.body, fmt_dohoistfixpoints, (void *)hfp);

	for (i=0; i<hfp->fpts_cur; i++) {
		fmnode_t *fp = hfp->fpts[i];

		hfp->fpts[i] = NULL;
		fmt_addtolist ((void ***)&fmm->items, &fmm->items_cur, &fmm->items_max, (void *)fp);
	}
	hfp->fpts_cur = 0;

	fmt_freehfp (hfp);
	return;
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
	case FM_EVENT:
		if (node->u.fmevent.name) {
			fprintf (fp, "%s", node->u.fmevent.name);
		} else {
			fprintf (fp, "addr0x%8.8x", (unsigned int)node->u.fmevent.node);
		}
		break;
	case FM_EVENTSET:
		if (node->u.fmevset.name) {
			fprintf (fp, "%s", node->u.fmevset.name);
		} else {
			fprintf (fp, "addr0x%8.8x", (unsigned int)node->u.fmevset.node);
		}
		break;
	case FM_TREEREF:
		if (node->u.fmtree.name) {
			fprintf (fp, "%s", node->u.fmtree.name);
		} else {
			fprintf (fp, "addr0x%8.8x", (unsigned int)node->u.fmtree.node);
		}
		break;
	case FM_NAMEDPROC:
		if (node->u.fmproc.name) {
			fprintf (fp, "%s", node->u.fmproc.name);
		} else {
			fprintf (fp, "addr0x%8.8x", (unsigned int)node->u.fmproc.name);
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
/*{{{  PRIVATE void fmt_writeoutnode_typestr (fmnode_t *node, FILE *fp)*/
/*
 *	called to write out the type string associated with a single node
 */
PRIVATE void fmt_writeoutnode_typestr (fmnode_t *node, FILE *fp)
{
	if (!node) {
		fprintf (fp, "nullnode");
		return;
	}
	if (node->type == FM_NODEREF) {
		node = node->u.fmnode.node;
	}

	switch (node->type) {
	case FM_EVENT:
		if (node->u.fmevent.typename) {
			fprintf (fp, "%s", node->u.fmevent.typename);
		} else {
			fprintf (fp, "addr0x%8.8x", (unsigned int)node->u.fmevent.nodetype);
		}
		break;
	case FM_EVENTSET:
		if (node->u.fmevset.typename) {
			fprintf (fp, "%s", node->u.fmevset.typename);
		} else {
			fprintf (fp, "addr0x%8.8x", (unsigned int)node->u.fmevset.nodetype);
		}
		break;
	case FM_TREEREF:
		if (node->u.fmtree.typename) {
			fprintf (fp, "%s", node->u.fmtree.typename);
		} else {
			fprintf (fp, "addr0x%8.8x", (unsigned int)node->u.fmtree.nodetype);
		}
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
		fprintf (fp, "<nullnode />\n");
		return;
	}
	
	switch (node->type) {
		/*{{{  SEQ,PAR,DET,NDET,THEN*/
	case FM_SEQ:
	case FM_THEN:
	case FM_PAR:
	case FM_DET:
	case FM_NDET:
		switch (node->type) {
		case FM_SEQ:	strcpy (sname, "seq"); break;
		case FM_THEN:	strcpy (sname, "then"); break;
		case FM_PAR:	strcpy (sname, "par"); break;
		case FM_DET:	strcpy (sname, "det"); break;
		case FM_NDET:	strcpy (sname, "ndet"); break;
		default:	strcpy (sname, "invalid"); break;
		}

		fprintf (fp, "<%s>\n", sname);
		for (i=0; i<node->u.fmlist.items_cur; i++) {
			fmt_writeoutnode (node->u.fmlist.items[i], indent + 1, fp);
		}
		fmt_writeoutindent (indent, fp);
		fprintf (fp, "</%s>\n", sname);
		break;
		/*}}}*/
		/*{{{  INPUT,OUTPUT,SYNC*/
	case FM_INPUT:
	case FM_OUTPUT:
	case FM_SYNC:
		switch (node->type) {
		case FM_INPUT:	strcpy (sname, "input"); break;
		case FM_OUTPUT:	strcpy (sname, "output"); break;
		case FM_SYNC:	strcpy (sname, "sync"); break;
		default:	strcpy (sname, "invalid"); break;
		}

		fprintf (fp, "<%s>\n", sname);
		fmt_writeoutnode (node->u.fmio.lhs, indent + 1, fp);
		if (node->u.fmio.rhs) {
			fmt_writeoutnode (node->u.fmio.rhs, indent + 1, fp);
		}
		fmt_writeoutindent (indent, fp);
		fprintf (fp, "</%s>\n", sname);
		break;
		/*}}}*/
		/*{{{  NAMEDPROC*/
	case FM_NAMEDPROC:
		fprintf (fp, "<proc name=\"%s\">\n", node->u.fmproc.name);
		fmt_writeoutindent (indent+1, fp);
		fprintf (fp, "<events>\n");
		for (i=0; i<node->u.fmproc.parms_cur; i++) {
			fmnode_t *p = node->u.fmproc.parms[i];

			fmt_writeoutnode (p, indent+2, fp);
		}
		fmt_writeoutindent (indent+1, fp);
		fprintf (fp, "</events>\n");

		fmt_writeoutnode (node->u.fmproc.body, indent + 1, fp);

		fmt_writeoutindent (indent, fp);
		fprintf (fp, "</proc>\n");
		break;
		/*}}}*/
		/*{{{  INSTANCE*/
	case FM_INSTANCE:
		fprintf (fp, "<instance name=\"");
		fmt_writeoutnode_str (node->u.fminst.pref, fp);
		fprintf (fp, "\">\n");
		for (i=0; i<node->u.fminst.args_cur; i++) {
			fmt_writeoutnode (node->u.fminst.args[i], indent + 1, fp);
		}
		fmt_writeoutindent (indent, fp);
		fprintf (fp, "</instance>\n");
		break;
		/*}}}*/
		/*{{{  TAGSET*/
	case FM_TAGSET:
		fprintf (fp, "<tagset name=\"%s\">\n", node->u.fmtset.name);
		for (i=0; i<node->u.fmtset.tags_cur; i++) {
			fmt_writeoutindent (indent, fp);
			fprintf (fp, "<tag name=\"");
			fmt_writeoutnode_str (node->u.fmtset.tags[i], fp);
			fprintf (fp, "\" />\n");
		}
		fmt_writeoutindent (indent, fp);
		fprintf (fp, "</tagset>\n");
		break;
		/*}}}*/
		/*{{{  ATOM*/
	case FM_ATOM:
		fprintf (fp, "<atom id=\"%s\" />\n", node->u.fmatom.id);
		break;
		/*}}}*/
		/*{{{  TREEREF*/
	case FM_TREEREF:
		fprintf (fp, "<treeref name=\"");
		fmt_writeoutnode_str (node, fp);
		fprintf (fp, "\" ");
		if (node->u.fmtree.typename) {
			fprintf (fp, "type=\"%s\" ", node->u.fmtree.typename);
		}
		fprintf (fp, "/>\n");
		break;
		/*}}}*/
		/*{{{  EVENT*/
	case FM_EVENT:
		fprintf (fp, "<event name=\"");
		fmt_writeoutnode_str (node, fp);
		fprintf (fp, "\" ");
		if (node->u.fmevent.typename) {
			fprintf (fp, "type=\"%s\" ", node->u.fmevent.typename);
		}
		fprintf (fp, "/>\n");
		break;
		/*}}}*/
		/*{{{  EVENTSET*/
	case FM_EVENTSET:
		fprintf (fp, "<eventset name=\"");
		fmt_writeoutnode_str (node, fp);
		fprintf (fp, "\"");
		if (node->u.fmevset.typename) {
			fprintf (fp, " type=\"%s\"", node->u.fmevset.typename);
		}
		fprintf (fp, ">\n");

		for (i=0; i<node->u.fmevset.events_cur; i++) {
			fmt_writeoutnode (node->u.fmevset.events[i], indent + 1, fp);
		}

		fmt_writeoutindent (indent, fp);
		fprintf (fp, "</eventset>\n");
		break;
		/*}}}*/
		/*{{{  FIXPOINT*/
	case FM_FIXPOINT:
		fprintf (fp, "<fixpoint>\n");
		fmt_writeoutnode (node->u.fmfix.id, indent + 1, fp);
		fmt_writeoutnode (node->u.fmfix.proc, indent + 1, fp);

		fmt_writeoutindent (indent, fp);
		fprintf (fp, "</fixpoint>\n");
		break;
		/*}}}*/
		/*{{{  SKIP,STOP,DIV,CHAOS*/
	case FM_SKIP:
		fprintf (fp, "<skip />\n");
		break;
	case FM_STOP:
		fprintf (fp, "<stop />\n");
		break;
	case FM_DIV:
		fprintf (fp, "<div />\n");
		break;
	case FM_CHAOS:
		fprintf (fp, "<chaos />\n");
		break;
		/*}}}*/
	default:
		fprintf (fp, "<unknown type=\"%d\" />\n", (int)node->type);
		break;
	}
}
/*}}}*/
/*{{{  PRIVATEPARAM int fmt_doaddthen (fmnode_t **nodep, void *voidptr)*/
/*
 *	turns sequential lists of events into THEN processes (generates nicer output)
 *	return STOP_WALK to stop, CONTINUE_WALK to continue
 */
PRIVATEPARAM int fmt_doaddthen (fmnode_t **nodep, void *voidptr)
{
	fmnode_t *n = *nodep;

	if (!n) {
		return STOP_WALK;
	}
	switch (n->type) {
		/*{{{  SEQ -- find sequences of events*/
	case FM_SEQ:
		{
			int i;
#if 0
fprintf (stderr, "fmt_doaddthen(): in sequence:\n");
fmt_dumpnode (n, 1, stderr);
#endif

			for (i=0; i<n->u.fmlist.items_cur; i++) {
				fmnode_t *itm = n->u.fmlist.items[i];

				if (!itm) {
					continue;
				}
				if (fmt_isevent (itm)) {
					int j;

					for (j=i+1; (j<n->u.fmlist.items_cur) && fmt_isevent (n->u.fmlist.items[j]); j++);
					/* things from 'i' to 'j-1' inclusive are events */

#if 0
fprintf (stderr, "fmt_doaddthen(): compact from %d to %d\n", i, j-1);
#endif
					if (j > i) {
						/* at least two events */
						fmnode_t *th = fmt_newnode (FM_THEN, itm->org);
						int k;

						for (k=i; k<j; k++) {
							fmnode_t *evitm = n->u.fmlist.items[k];

							fmt_addtonodelist (th, evitm);
						}
						/* remove 'i+1' to 'j-1' items from the main list */
						for (k=i+1; k<j; k++) {
							fmt_delfromnodelist (n, i+1);
						}
						/* replace 'i' with the new THEN node */
						n->u.fmlist.items[i] = th;

						/* if the 'i+1' node exists, add to end of THEN, else add SKIP */
						if ((i+1) < n->u.fmlist.items_cur) {
							fmnode_t *proc = n->u.fmlist.items[i+1];

							fmt_addtonodelist (th, proc);
							fmt_delfromnodelist (n, i+1);
						} else {
							fmt_addtonodelist (th, fmt_newnode (FM_SKIP, itm->org));
						}
					}
				}
			}
		}
		break;
		/*}}}*/
	default:
		break;
	}
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATEPARAM int fmt_domakeeventprocesses (fmnode_t **nodep, void *voidptr)*/
/*
 *	finds events outside of THEN structures and turns into processes
 *	return STOP_WALK to stop, CONTINUE_WALK to continue
 */
PRIVATEPARAM int fmt_domakeeventprocesses (fmnode_t **nodep, void *voidptr)
{
	fmnode_t *n = *nodep;

	if (!n) {
		return STOP_WALK;
	}
	switch (n->type) {
		/*{{{  THEN -- only walk last subnode*/
	case FM_THEN:
		{
			int idx = n->u.fmlist.items_cur - 1;

			if (idx >= 0) {
				fmt_modprewalk (&n->u.fmlist.items[idx], fmt_domakeeventprocesses, NULL);
			}
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  INPUT,OUTPUT,SYNC -- turn into (... -> Skip)*/
	case FM_INPUT:
	case FM_OUTPUT:
	case FM_SYNC:
		{
			fmnode_t *fmn = fmt_newnode (FM_THEN, n->org);

			fmt_addtonodelist (fmn, n);
			fmt_addtonodelist (fmn, fmt_newnode (FM_SKIP, n->org));

			*nodep = fmn;
		}
		return STOP_WALK;
		/*}}}*/
	default:
		break;
	}
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATE void fmt_postprocessmodels (fmmset_t *fmm)*/
/*
 *	does post-processing on a set of formal models.  This involves making sure that
 *	processes are valid in the way they're composed (e.g. collecting up SEQ events into THENs,
 *	making sure the last component of a THEN is a process)
 */
PRIVATE void fmt_postprocessmodels (fmmset_t *fmm)
{
	int i;

	for (i=0; i<fmm->items_cur; i++) {
		fmt_modprewalk (&fmm->items[i], fmt_doaddthen, NULL);
		fmt_modprewalk (&fmm->items[i], fmt_domakeeventprocesses, NULL);
		/* simplify the resulting nodes */
		fmt_modprewalk (&fmm->items[i], fmt_simplifynode, NULL);
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
	fprintf (fp, "<program version=\"1.0\" name=\"%s\">\n", filename);

	for (i=0; i<fmm->items_cur; i++) {
		fmt_writeoutnode (fmm->items[i], 0, fp);
	}

	fprintf (fp, "</program>\n");
}
/*}}}*/
/*{{{  PRIVATEPARAM int do_formalmodelgen_ioexpr (treenode *n, void *const voidptr)*/
/*
 *	does formal-model generation for an I/O expression, generates into 'fmstate'
 */
PRIVATEPARAM int do_formalmodelgen_ioexpr (treenode *n, void *const voidptr)
{
	int tag = TagOf (n);
	fmstate_t *fmstate = (fmstate_t *)voidptr;
	fmnode_t *fmn;

	if (n && (TagOf (n) == S_CONSTEXP)) {
		/* try the original */
		n = CExpOf (n);
		tag = TagOf (n);
	}
	if (!n) {
		return STOP_WALK;
	}
#if 0
fprintf (stderr, "do_formalmodelgen_ioexpr(): tag = %s\n", tagstring (tag));
#endif

	switch (tag) {
	case N_TAGDEF:
		{
			char *tname = (char *)WNameOf (NNameOf (n));

			fmn = fmt_newnode (FM_TREEREF, n);
			fmn->u.fmtree.name = (char *)memalloc (strlen (tname) + 2);
			fmt_copytagname (fmn->u.fmtree.name, tname);
			fmn->u.fmtree.node = n;

			*(fmstate->target) = fmn;
		}
		return STOP_WALK;
	}
	return CONTINUE_WALK;
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
		/*{{{  PROCDEF -- return*/
	case S_PROCDEF:
		if (!separatelycompiled (DNameOf (n))) {
			char *pname = (char *)WNameOf (NNameOf (DNameOf (n)));

#if 1
fprintf (stderr, "do_formalmodelgen(): PROCDEF! [%s]\n", pname);
#endif
		}
		break;
		/*}}}*/
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
		/*{{{  SEQ,PAR -- return*/
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
					fmt_addtonodelist (fmn, tmptarget);
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
		/*{{{  REPLSEQ -- return*/
	case S_REPLSEQ:
		{
			treenode *length = ReplCLengthExpOf (n);
			fmnode_t **saved_target = fmstate->target;

			if (isconst (length)) {
				int l = eval_const_treenode (length);
				int j;

				fmn = fmt_newnode (FM_SEQ, n);
				for (j=0; j<l; j++) {
					fmnode_t *tmp = NULL;

					fmstate->target = &tmp;
					prewalktree (ReplCBodyOf (n), do_formalmodelgen, (void *)fmstate);
					if (tmp) {
						fmt_addtonodelist (fmn, tmp);
					}
				}
			} else {
				/* don't know how many, do with fixpoint */
				fmnode_t *id = fmt_newatom (n);
				fmnode_t *tmpnd = fmt_newnode (FM_NDET, n);
				fmnode_t *tmp = fmt_newnode (FM_SEQ, n);
				fmnode_t *idref = fmt_newnode (FM_NODEREF, n);

				fmn = fmt_newnode (FM_FIXPOINT, n);
				fmn->u.fmfix.id = id;
				fmstate->target = &fmn->u.fmfix.proc;

				prewalktree (ReplCBodyOf (n), do_formalmodelgen, (void *)fmstate);
				if (!fmn->u.fmfix.proc) {
					/* assume Skip for body of loop */
					fmn->u.fmfix.proc = fmt_newnode (FM_SKIP, ReplCBodyOf (n));
				}

				idref->u.fmnode.node = id;
				fmt_addtonodelist (tmpnd, fmt_newnode (FM_SKIP, n));
				fmt_addtonodelist (tmpnd, tmp);
				fmt_addtonodelist (tmp, fmn->u.fmfix.proc);
				fmt_addtonodelist (tmp, idref);
				fmn->u.fmfix.proc = tmpnd;
			}

			fmstate->target = saved_target;
			*(fmstate->target) = fmn;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  N_PARAM, RECORDSUB -- return*/
	case N_PARAM:
	case S_RECORDSUB:
		fmn = fmt_findfreevar (fmstate, n);
		if (fmn) {
			fmnode_t *tmp = fmt_newnode (FM_NODEREF, n);

			tmp->u.fmnode.node = fmn;
			*(fmstate->target) = tmp;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  INPUT,OUTPUT -- return*/
	case S_INPUT:
	case S_OUTPUT:
		{
			fmtype_e ftype = ((tag == S_INPUT) ? FM_INPUT : FM_OUTPUT);
			fmnode_t **saved_target = fmstate->target;

			fmn = fmt_newnode (ftype, n);
			fmstate->target = &fmn->u.fmio.lhs;
			prewalktree (LHSOf (n), do_formalmodelgen, (void *)fmstate);
			fmstate->target = &fmn->u.fmio.rhs;
			prewalktree (RHSOf (n), do_formalmodelgen_ioexpr, (void *)fmstate);
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
		/*{{{  IF -- return*/
	case S_IF:
		{
			fmnode_t **saved_target = fmstate->target;
			treenode *items = CBodyOf (n);
			treenode *walk;
			int add_stop = 1;

			/* this becomes a non-deterministic choice on the processes */
			fmn = fmt_newnode (FM_NDET, n);
			for (walk = items; !EndOfList (walk); walk = NextItem (walk)) {
				treenode *choice = skipspecifications (ThisItem (walk));

				if (TagOf (choice) == S_CHOICE) {
					fmnode_t *body;
					treenode *guard = CondGuardOf (choice);

					if (isconst (guard) && eval_const_treenode (guard)) {
						/* this is a TRUE guard, so no opportunity for STOP */
						add_stop = 0;
					}
					fmstate->target = &body;
					prewalktree (CondBodyOf (choice), do_formalmodelgen, (void *)fmstate);
					if (!body) {
						body = fmt_newnode (FM_SKIP, choice);
					}

					fmt_addtonodelist (fmn, body);
				}
			}

			if (add_stop) {
				fmt_addtonodelist (fmn, fmt_newnode (FM_STOP, n));
			}

			fmstate->target = saved_target;
			*(fmstate->target) = fmn;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  CASE_INPUT -- return*/
	case S_CASE_INPUT:
		{
			fmnode_t **saved_target = fmstate->target;
			treenode *chan = LHSOf (n);
			treenode *varlist = RHSOf (n);
			treenode *walk;

			/* this becomes a deterministic choice on inputs */
			fmn = fmt_newnode (FM_DET, n);
			for (walk = varlist; !EndOfList (walk); walk = NextItem (walk)) {
				treenode *var = skipspecifications (ThisItem (walk));

#if 0
fprintf (stderr, "do_formalmodegen(): CASE_INPUT, variant item after specs is [%s]\n", tagstring (TagOf (var)));
#endif
				if (TagOf (var) == S_VARIANT) {
					fmnode_t *seq = fmt_newnode (FM_SEQ, var);
					fmnode_t *input = fmt_newnode (FM_INPUT, var);
					fmnode_t *body = NULL;

					fmstate->target = &input->u.fmio.lhs;
					prewalktree (chan, do_formalmodelgen, (void *)fmstate);
					fmstate->target = &input->u.fmio.rhs;
					prewalktree (CondGuardOf (var), do_formalmodelgen_ioexpr, (void *)fmstate);

					fmt_addtonodelist (seq, input);
					fmstate->target = &body;
					prewalktree (CondBodyOf (var), do_formalmodelgen, (void *)fmstate);
					if (!body) {
						body = fmt_newnode (FM_SKIP, var);
					}
					fmt_addtonodelist (seq, body);

					/* and add it to the current list */
					fmt_addtonodelist (fmn, seq);
				}
			}

			fmstate->target = saved_target;
			*(fmstate->target) = fmn;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  DECL -- return*/
	case S_DECL:
		{
			treenode *dname = DNameOf (n);

			if (TagOf (dname) == N_DECL) {
				fmnode_t *evnode = fmt_createeventfromvar (dname, TRUE);

				if (evnode) {
#if 0
fprintf (stderr, "do_formalmodelgen(): DECL/N_DECL, got model event node=\n");
fmt_dumpnode (evnode, 1, stderr);
#endif
					fmt_addtovarslist (fmstate, evnode);
					
				}
			}
			prewalktree (DBodyOf (n), do_formalmodelgen, (void *)fmstate);
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  ALTERNATIVE -- return*/
	case S_ALTERNATIVE:
		{
			fmnode_t **saved_target = fmstate->target;
			treenode *guard = AltGuardOf (n);

			/* avoid constant FALSE pre-conditions */
			if (!guard || !isconst (guard) || eval_const_treenode (guard)) {
				treenode *inode = AltInputOf (n);
				fmnode_t *input = NULL;

				fmstate->target = &input;
				prewalktree (inode, do_formalmodelgen, (void *)fmstate);
				/* should be left with either an INPUT node, or a DET/NDET node for nested/CASE things */
#if 0
fprintf (stderr, "do_formalmodelgen(): ALTERNATIVE in ALT/PRIALT: inode =");
printtreenl (stderr, 1, inode);
fprintf (stderr, "do_formalmodelgen(): model for input is:\n");
fmt_dumpnode (input, 1, stderr);
#endif
				if (!input) {
					/* assume SKIP */
					input = fmt_newnode (FM_SKIP, n);
				}
				if (input->type == FM_SKIP) {
					fmnode_t *body = NULL;

					/* these make an effectively non-deterministic choice in the ALT it comes from */
					if (!fmstate->ndlist) {
						fmstate->ndlist = fmt_newnode (FM_NDET, n);
					}
					fmstate->target = &body;
					prewalktree (AltBodyOf (n), do_formalmodelgen, (void *)fmstate);
					if (!body) {
						body = fmt_newnode (FM_SKIP, n);
					}
					fmt_addtonodelist (fmstate->ndlist, body);

					fmt_freenode (input, 2);
					input = NULL;
				} else if ((input->type == FM_DET) || (input->type == FM_NDET)) {
					/* add directly */
				} else {
					fmnode_t *seq = fmt_newnode (FM_SEQ, n);
					fmnode_t *body = NULL;
					
					fmt_addtonodelist (seq, input);
					fmstate->target = &body;
					prewalktree (AltBodyOf (n), do_formalmodelgen, (void *)fmstate);
					if (!body) {
						body = fmt_newnode (FM_SKIP, n);
					}
					fmt_addtonodelist (seq, body);

					input = seq;
				}

				fmstate->target = saved_target;
				if (input) {
					*(fmstate->target) = input;
				}
			}
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  ALT,PRIALT -- return*/
	case S_ALT:
	case S_PRIALT:
		{
			fmnode_t **saved_target = fmstate->target;
			treenode *altlist = CBodyOf (n);
			treenode *walk;
			fmnode_t *ndlist = NULL;
			fmnode_t *saved_ndlist = fmstate->ndlist;

			fmstate->ndlist = NULL;

			fmn = fmt_newnode (FM_DET, n);
			for (walk = altlist; !EndOfList (walk); walk = NextItem (walk)) {
				fmnode_t *altitem = NULL;

				fmstate->target = &altitem;
				prewalktree (ThisItem (walk), do_formalmodelgen, (void *)fmstate);

				if (altitem) {
					fmt_addtonodelist (fmn, altitem);
				}
			}

			/* if we generated any non-determinstics, pick up here */
			ndlist = fmstate->ndlist;
			
			if (ndlist) {
				/* non-deterministic processes to add */
				fmt_insertintonodelist (ndlist, 0, fmn);
				fmn = ndlist;
			}

			fmstate->target = saved_target;
			*(fmstate->target) = fmn;
			fmstate->ndlist = saved_ndlist;
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
				fmt_addtonodelist (tmp, fmn->u.fmfix.proc);
				fmt_addtonodelist (tmp, idref);
				fmn->u.fmfix.proc = tmp;
			} else {
				/* assuming WHILE <unknown> loop */
				fmnode_t *tmpnd = fmt_newnode (FM_NDET, n);
				fmnode_t *tmp = fmt_newnode (FM_SEQ, n);
				fmnode_t *idref = fmt_newnode (FM_NODEREF, n);

				idref->u.fmnode.node = id;
				fmt_addtonodelist (tmpnd, fmt_newnode (FM_SKIP, n));
				fmt_addtonodelist (tmpnd, tmp);
				fmt_addtonodelist (tmp, fmn->u.fmfix.proc);
				fmt_addtonodelist (tmp, idref);
				fmn->u.fmfix.proc = tmpnd;
			}

			fmstate->target = saved_target;
			*(fmstate->target) = fmn;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  CLAIM -- return*/
	case S_CLAIM:
		{
			treenode *claimvar = CTempOf (n);
			fmnode_t *cln, *rln;

			cln = fmt_findfreevar_claim (fmstate, claimvar);
			rln = fmt_findfreevar_release (fmstate, claimvar);
			if (cln && rln) {
				fmnode_t **saved_target = fmstate->target;
				fmnode_t *tmp, *tmp2;

				/* generate claim; body; release */
				fmn = fmt_newnode (FM_SEQ, n);

				tmp2 = fmt_newnode (FM_NODEREF, n);
				tmp2->u.fmnode.node = cln;

				tmp = fmt_newnode (FM_OUTPUT, n);
				tmp->u.fmio.lhs = tmp2;

				fmt_addtonodelist (fmn, tmp);
				tmp = NULL;
				fmstate->target = &tmp;

				prewalktree (CBodyOf (n), do_formalmodelgen, (void *)fmstate);
				if (!tmp) {
					/* assume Skip */
					tmp = fmt_newnode (FM_SKIP, CBodyOf (n));
				}
				fmt_addtonodelist (fmn, tmp);

				tmp2 = fmt_newnode (FM_NODEREF, n);
				tmp2->u.fmnode.node = rln;

				tmp = fmt_newnode (FM_OUTPUT, n);
				tmp->u.fmio.lhs = tmp2;

				fmt_addtonodelist (fmn, tmp);

				fmstate->target = saved_target;
				*(fmstate->target) = fmn;
			} else {
				/* failed to find claim/release vars for this, so ignore */
				return CONTINUE_WALK;
			}
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  FINSTANCE,PINSTANCE -- return*/
	case S_FINSTANCE:
	case S_PINSTANCE:
		if (!separatelycompiled (INameOf (n))) {
			treenode *iname = INameOf (n);

#if 1
fprintf (stderr, "do_formalmodelgen(): FINSTANCE/PINSTANCE, of local:");
printtreenl (stderr, 1, iname);
#endif
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
		/*{{{  TPROTDEF*/
	case S_TPROTDEF:
		{
			char *tpname = (char *)WNameOf (NNameOf (DNameOf (n)));
			fmnode_t *fmn;
			treenode *tags = NTypeOf (DNameOf (n));
			treenode *walk;

#if 0
fprintf (stderr, "do_formalmodelcheck_tree(): TPROTDEF [%s]\n", tpname);
#endif
			fmn = fmt_newnode (FM_TAGSET, n);
			fmn->u.fmtset.name = (char *)memalloc (strlen (tpname) + 8);
			sprintf (fmn->u.fmtset.name, "PROT_");
			fmt_copyprotname (fmn->u.fmtset.name + 5, tpname);

			for (walk=tags; !EndOfList (walk); walk = NextItem (walk)) {
				treenode *tag = ThisItem (walk);

				if (TagOf (tag) == N_TAGDEF) {
					char *tagname = (char *)WNameOf (NNameOf (tag));
					fmnode_t *fmntag = fmt_newnode (FM_TREEREF, tag);

					fmntag->u.fmtree.node = tag;
					fmntag->u.fmtree.name = (char *)memalloc (strlen (tagname) + 1);
					fmt_copytagname (fmntag->u.fmtree.name, tagname);

					fmt_addtonodelist (fmn, fmntag);
				}
#if 0
fprintf (stderr, " ... : tag =");
printtreenl (stderr, 1, tag);
#endif
			}

			fmt_addtolist ((void ***)&fmm->items, &fmm->items_cur, &fmm->items_max, (void *)fmn);
		}
		break;
		/*}}}*/
		/*{{{  PROCDEF, MPROCDECL*/
	case S_PROCDEF:
	case S_MPROCDECL:
		if (!separatelycompiled (DNameOf (n))) {
			char *pname = (char *)WNameOf (NNameOf (DNameOf (n)));
			fmstate_t *fmstate = fmt_newstate ();
			fmnode_t *fmn = NULL, *fmnproc = NULL;
			treenode *fparams = NParamListOf (DNameOf (n));
			treenode *walk;

#if 0
fprintf (stderr, "do_formalmodelcheck_tree(): PROC [%s]\n", pname);
#endif
			/*{{{  collect up relevant parameters*/
			for (walk = fparams; !EmptyList (walk); walk = NextItem (walk)) {
				treenode *parm = ThisItem (walk);

				switch (TagOf (parm)) {
					/*{{{  N_PARAM*/
				case N_PARAM:
					{
						fmnode_t *evnode = fmt_createeventfromvar (parm, FALSE);

						if (evnode) {
							fmt_addtovarslist (fmstate, evnode);
						}
#if 0
fprintf (stderr, "do_formalmodelcheck_tree(): param type:");
printtreenl (stderr, 1, type);
#endif
					}
					break;
					/*}}}*/
				default:
					break;
				}
			}
			/*}}}*/

			fmstate->target = &fmstate->temp;
			fmstate->setref = fmm;
			prewalkproctree (DValOf (n), do_formalmodelgen, (void *)fmstate);
			fmstate->setref = NULL;

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

			/*{{{  wrap up in NAMEDPROC node*/
			fmnproc = fmt_newnode (FM_NAMEDPROC, n);
			fmnproc->u.fmproc.body = fmn;
			fmnproc->u.fmproc.name = (char *)memalloc (strlen (pname) + 3);
			{
				char *ch, *dh = fmnproc->u.fmproc.name;
				dh += sprintf (dh, "P");
				for (ch = pname; *ch != '\0'; ch++, dh++) {
					if ((*ch >= 'a') && (*ch <= 'z')) {
						*dh = (*ch - 'a') + 'A';
					} else if (*ch == '.') {
						*dh = '_';
					} else {
						*dh = *ch;
					}
				}
				*dh = '\0';
			}
			fmnproc->u.fmproc.parms = fmstate->fvars;
			fmnproc->u.fmproc.parms_cur = fmstate->fvars_cur;
			fmnproc->u.fmproc.parms_max = fmstate->fvars_max;

			/*}}}*/

			fmstate->fvars = NULL;
			fmstate->fvars_cur = 0;
			fmstate->fvars_max = 0;

			fmt_freestate (fmstate);

			fmt_modprewalk (&fmnproc, fmt_simplifynode, NULL);
			fmt_hoistfixpoints (fmnproc, fmm);

			fmt_addtolist ((void ***)&fmm->items, &fmm->items_cur, &fmm->items_max, (void *)fmnproc);
#if 0
fprintf (stderr, "do_formalmodelcheck_tree(): got formal model:\n");
fmt_dumpnode (fmnproc, 1, stderr);
#endif
		}
		break;
		/*}}}*/
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

#if 0
fprintf (stderr, "formalmodelcheck(): tree before check is:\n");
printtreenl (stderr, 1, n);
#endif
			prewalkproctree (n, do_formalmodelcheck_tree, (void *)fmm);
			fmt_postprocessmodels (fmm);

#if 0
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

