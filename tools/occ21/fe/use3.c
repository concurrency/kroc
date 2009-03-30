/* $Id: use3.c,v 1.3 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	occam two usage checker
 *	Copyright (C) 1987 Inmos Limited
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

/*****************************************************************************
 *
 *  use3 - builds up lists of free variables for procs / functions
 *         alias checks if required
 *         calls usage checker in use2 if required
 *         also calls undefinedness checker in use4
 *
 *****************************************************************************/

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include "feinc.h"
#include "useerror.h"
#include "usehdr.h"
#include "usedef.h"
#include "use1def.h"
#include "use2def.h"
#include "use4def.h"
#include "syndef.h"		/* syn_lexlevel */
#include "chkdef.h"		/* current_fe_handle, and isconst */
#include "trandef.h"		/* isdynmobilechantype */
#include "lexdef.h"		/* no_undefinedness_check flag */
/*}}}*/

/*{{{  DEBUG_MSG*/
#undef DEBUG_MSG
#ifdef DEBUG
#define DEBUG_MSG(X) { if (current_fe_data->fe_debuguse) printf X; }
#else
#define DEBUG_MSG(X)
#endif
/*}}}*/

/*{{{  PRIVATE variables*/
PRIVATE abbrevlist *inabbrevs = NULL;
PRIVATE abbrevlist *outabbrevs = NULL;
PRIVATE abbrevlist *varabbrevs = NULL;
PRIVATE abbrevlist *valabbrevs = NULL;
PRIVATE int in_abbrev = FALSE;
PRIVATE SOURCEPOSN lastloc = NOPOSN;
PRIVATE treenode *constexp_one = NULL;

typedef struct overlap_s {
	treenode **o_exp;
	treenode *o_check;
	struct overlap_s *o_next;
} overlap_t;
PRIVATE overlap_t *overlapchain;
/*}}}*/

/*{{{  FORWARD*/
FORWARD PRIVATE void aliascheckexp (treenode * n, use_mode_t use_mode);
FORWARD PRIVATE void aliascheck (treenode * n);
FORWARD PRIVATE void ripple_aliased_flags (treenode * const nptr);

/*}}}*/

/*{{{  PRIVATE void insertoverlaps (void)*/
/*****************************************************************************
 *
 *  insertoverlaps inserts into the tree all the overlap checks currently
 *                 saved up.
 *
 *****************************************************************************/
PRIVATE void insertoverlaps (void)
{
	while (overlapchain != NULL) {
		treenode *checkexp = overlapchain->o_check;
		treenode **oldexp = overlapchain->o_exp;
		overlap_t *next = overlapchain->o_next;

		*oldexp = newdopnode (S_EVAL, LocnOf (checkexp), checkexp, *oldexp, S_INT);
		/*memfree(overlapchain); */
		freevec (overlapchain, sizeof (overlap_t));
		overlapchain = next;
	}
}

/*}}}*/
/*{{{  PRIVATE treenode *buildsubscripts (treenode *tptr)*/
/*****************************************************************************
 *
 *  buildsubscripts generates a subscripts tree to be inserted into an
 *                  abbrevnode, from the element tptr.
 *
 *****************************************************************************/
PRIVATE treenode *buildsubscripts (treenode * tptr)
{
	SOURCEPOSN locn = LocnOf (tptr);
	treenode *e = NULL;

	while (TRUE) {
		switch (TagOf (tptr)) {
/*{{{       S_ARRAYSUB S_RECORDSUB*/
		case S_ARRAYSUB:
#ifdef OCCAM2_5
		case S_RECORDSUB:
#endif
			e = newlistnode (S_LIST, locn, skipevals (ASIndexOf (tptr)), e);
			tptr = ASBaseOf (tptr);
			break;
/*}}}*/
			/*{{{  S_SEGMENT */
		case S_SEGMENT:
			if (e != NULL) {
				/*{{{  add into check for current subscript */
				treenode *t;
				treenode **sptr;

				if (TagOf (ThisItem (e)) == S_FOR) {
					sptr = LeftOpAddr (ThisItem (e));
				} else {
					sptr = ThisItemAddr (e);
				}

				t = newdopnode (S_ADD, locn, *sptr, skipevals (SStartExpOf (tptr)), S_INT);
#if 0
fprintf (stderr, "buildsubscripts(): built ADD node:");
printtreenl (stderr, 4, t);
#endif
				*sptr = foldexp (t);
				/*}}} */
			} else {
				treenode *estart, *elength;

				estart = skipevals (SStartExpOf (tptr));
				elength = skipevals (SLengthExpOf (tptr));
#if 0
fprintf (stderr, "buildsubscripts(): building LIST node;  elength =");
printtreenl (stderr, 4, elength);
#endif

				e = newlistnode (S_LIST, locn, newdopnode (S_FOR, locn, estart, elength, 0), NULL);
			}
			tptr = SNameOf (tptr);
			break;
			/*}}} */
		default:
			return e;
		}
	}
}

/*}}}*/

/*{{{  PRIVATE int subscripts_clash*/
/* This modified 1/11/90 for bug 1032.
   It used to complain that a[i][0] and a[j][0] clashed.
   Modified to only complain if ALL of the subscripts definitely clash.
*/
#define CLASH_NONE   0x0
#define CLASH_ALWAYS 0x1
#define CLASH_MAYBE  0x2
#define CLASH_NEVER  0x4

PRIVATE int subscripts_clash (treenode * lhs, treenode * rhs)
{
	/* This is used for overlap checking.
	   Some 'checkable' stuff gets through the rest of the aliaschecker.
	   Eg comparing a[0][i] with a[0] (or a[0][i] with a[1]).
	   This takes out all such examples.
	 */

	/* updated 9/10/90 by CON (bug 1010) to cope with:
	   INT x IS a[1] :
	   []INT y IS [a FROM 3 FOR i] :
	 */
	/* This routine can cope with accessing to  different array elements
	   in a record. */
	treenode *base1, *count1, *base2, *count2;
	BOOL test_a_always_true = FALSE;
	BOOL test_b_always_true = FALSE;

	/* Set up so that a[i] looks like [a FROM i FOR 1] */
	if (TagOf (lhs) == S_FOR) {
		base1 = LeftOpOf (lhs);
		count1 = RightOpOf (lhs);
	} else {
		base1 = lhs;
		count1 = constexp_one;
	}
	if (TagOf (rhs) == S_FOR) {
		base2 = LeftOpOf (rhs);
		count2 = RightOpOf (rhs);
	} else {
		base2 = rhs;
		count2 = constexp_one;
	}
#if 0
printf ("subscripts_clash: base1:");
printtree (stdout, 0, base1);
printf ("\nsubscripts_clash: count1:");
printtree (stdout, 0, count1);
printf ("\nsubscripts_clash: base2:");
printtree (stdout, 0, base2);
printf ("\nsubscripts_clash: count2:");
printtree (stdout, 0, count2);
printf ("\n");
#endif
#ifdef OCCAM2_5
	if (isconst (base1) && (TagOf (base1) != N_FIELD) && isconst (count1) && isconst (base2) && (TagOf (base2) != N_FIELD))
#else
	if (isconst (base1) && isconst (count1) && isconst (base2))
#endif
	{
		const INT32 b1 = LoValOf (base1), c1 = LoValOf (count1);
		const INT32 b2 = LoValOf (base2);
		if ((b1 + c1) <= b2)
			return CLASH_NEVER;
		test_a_always_true = TRUE;
	}
#ifdef OCCAM2_5
	if (isconst (base2) && (TagOf (base2) != N_FIELD) && isconst (count2) && isconst (base1) && (TagOf (base1) != N_FIELD))
#else
	if (isconst (base2) && isconst (count2) && isconst (base1))
#endif
	{
		const INT32 b2 = LoValOf (base2), c2 = LoValOf (count2);
		const INT32 b1 = LoValOf (base1);
		if ((b2 + c2) <= b1)
			return CLASH_NEVER;
		test_b_always_true = TRUE;
	}
#ifdef OCCAM2_5
/* this condition is for different array elements in a record */
	if ((TagOf (base1) == N_FIELD) && (TagOf (base2) == N_FIELD) && !issame (base1, base2))
		return CLASH_NEVER;
#endif
#if 0
	if (lhs == rhs)		/* bug TS/2069 28/01/93 */
		return CLASH_ALWAYS;
#else
	if (isconst (count1) && isconst (count2) && issame (base1, base2))
		return CLASH_ALWAYS;
#endif

	return (test_a_always_true && test_b_always_true) ? CLASH_ALWAYS : CLASH_MAYBE;
}

/*}}}*/
/*{{{  PRIVATE void overlaperror*/
PRIVATE void overlaperror (treenode *n, int checking_vals)
{
	usereport (FALSE, checking_vals ? USE_VAR_VAL_ABBREVIATED : USE_VAR_VAR_ABBREVIATED, lastloc, n);
}

/*}}}*/
/*{{{  PRIVATE treenode *exclude_fieldnd(treenode *list)*/
#ifdef OCCAM2_5
PRIVATE treenode *exclude_fieldnd (treenode * list)
{
	treenode *ls;
	while (!EndOfList (list) && TagOf (ThisItem (list)) == N_FIELD)
		list = NextItem (list);
	ls = list;
	while (!EndOfList (ls)) {
		if (!IsLastItem (ls) && TagOf (ThisItem (NextItem (ls))) == N_FIELD)
			NewNextItem (NextItem (NextItem (ls)), ls);
		else
			ls = NextItem (ls);
	}
	return list;
}
#endif
/*}}}*/
/*{{{  PRIVATE void overlapcheck (treenode *n, abbrevlist *abbrevs, INT32 start, INT32 end, treenode *element)*/
/*****************************************************************************
 *
 *  Check whether accesses to variable 'n' in the range 'start' to 'end'
 *  overlap any previous accesses in the list 'abbrevs'
 *  If they do then either generate the appropriate error message if both
 *  access ranges are known or insert code into the tree to generate
 *  a runtime check.
 *
 *****************************************************************************/
PRIVATE void overlapcheck (treenode *n, abbrevlist *abbrevs, INT32 start, INT32 end, treenode *element)
{
	const BOOL checking_vals = (abbrevs == valabbrevs);
	while (abbrevs != NULL) {
		if (ALNameOf (abbrevs) == n) {
			const INT32 alfirst = ALFirstOf (abbrevs);
			const INT32 allast = ALLastOf (abbrevs);
			const BOOL check_subs = (TagOf (element) == S_ARRAYSUB || TagOf (element) == S_SEGMENT);
			if (check_subs && ((ALFirstOf (abbrevs) < 0) || (start < 0)))
			 {	/* non constant subscripts */
				/*{{{  get into this run-time overlap check business */
				SOURCEPOSN locn = LocnOf (element);
				treenode *lhs, *rhs, *left, *right;
				int canclash = ((alfirst == 0) && (allast == MAXINDEX)) ? CLASH_ALWAYS : /* CLASH_MAYBE */ CLASH_NONE;	/* bug 1032 */
				DEBUG_MSG (("overlapcheck: at line %d against line %d, of %s\n",
					    FileLineOf (lastloc), FileLineOf (ALLocOf (abbrevs)), usetagstring (element)));
				lhs = buildsubscripts (element);
				rhs = ALSubscriptsOf (abbrevs);
				left = lhs;
				right = rhs;
				while (!EndOfList (left) && !EndOfList (right))
/* && (canclash == CLASH_MAYBE) *//* removed for bug 1032 */
				{
					canclash |= subscripts_clash (ThisItem (left), ThisItem (right));
					left = NextItem (left);
					right = NextItem (right);
				}
				DEBUG_MSG (("overlapcheck: canclash: "));
				if ((canclash & CLASH_NEVER) != 0)
					DEBUG_MSG (("CLASH_NEVER "));
				if ((canclash & CLASH_MAYBE) != 0)
					DEBUG_MSG (("CLASH_MAYBE "));
				if ((canclash & CLASH_ALWAYS) != 0)
					DEBUG_MSG (("CLASH_ALWAYS "));
				DEBUG_MSG (("\n"));
				if ((canclash & CLASH_NEVER) != 0);	/* skip */
				else if (((canclash & CLASH_ALWAYS) != 0) && ((canclash & CLASH_MAYBE) == 0))
					/* We have found, eg, a clash between an abbreviation of
					   array[0], and array[0][i], which would otherwise not be found */
				{
					overlaperror (n, checking_vals);
					return;
				}
					else if (((canclash & CLASH_MAYBE) != 0) && current_fe_data->fe_rangechecking
						 && !current_fe_data->fe_warn_on_usage_error)
				 {	/* added 13/12/90 */
					/* RANGECHECKING can either be turned off, here, or it could be moved
					   into 'buildoverlapcheck' in bind1, if we wanted to keep it in the backend */
					/* element is the one we are currently working on,
					   abbrevs is the one we are overlap checking against */
					treenode **element_base_ptr;
					treenode *ocheck;
					overlap_t *overlap = (overlap_t *) newvec (sizeof (overlap_t));
#ifdef OCCAM2_5
					ocheck = newdopnode (S_OVERLAPCHECK, locn,
							     copytree (exclude_fieldnd (lhs), syn_lexlevel),
							     copytree (exclude_fieldnd (rhs), syn_lexlevel), S_INT);
#else
					ocheck = newdopnode (S_OVERLAPCHECK, locn, copytree (lhs, syn_lexlevel), copytree (rhs, syn_lexlevel), S_INT);

#endif
					element_base_ptr = (TagOf (element) == S_ARRAYSUB) ? ASIndexAddr (element) : SStartExpAddr (element);
					overlap->o_exp = element_base_ptr;
					overlap->o_check = ocheck;
					overlap->o_next = overlapchain;
					overlapchain = overlap;
				}
			}
/*}}}*/
			else if (!check_subs || ((start <= allast) && (end >= alfirst))) {
				overlaperror (n, checking_vals);
				return;
			}
		}
		abbrevs = ALNextOf (abbrevs);
	}
}

/*}}}*/

/*{{{  PRIVATE varlist *record_parameter_info (n, freevars)*/
/******************************************************************************
 *
 *  searches 'freevars' variable list to references to parameters of
 *  PROC / FUNCTION definition pointed to by 'n'.  References are removed
 *  from the list and linked to formal parameter name node
 *
 *****************************************************************************/
PRIVATE varlist *record_parameter_info (treenode * n, varlist * freevars)
{
	treenode *params;	/* points to list of parameters */
	for (params = NParamListOf (DNameOf (n)); !EndOfList (params); params = NextItem (params)) {
		treenode *const param_name = ThisItem (params);
		varlist **prev = &freevars;
		varlist *vlist = *prev;
		while ((vlist != NULL) && (VLNameOf (vlist) != param_name)) {
			prev = &VLNextOf (vlist);
			vlist = *prev;
		}
		SetNParamUse (param_name, vlist);
#ifdef DEBUG
		if (current_fe_data->fe_debuguse) {
			const BOOL in = ParamInputOn (param_name);
			const BOOL out = ParamOutputOn (param_name);
			if (in || out)
				printf ("param %s %s input on, %s output on\n",
					WNameOf (NNameOf (param_name)), in ? "is" : "is not", out ? "is" : "is not");
		}
#endif
		if (vlist != NULL) {
			*prev = VLNextOf (vlist);
			SetVLNext (vlist, NULL);
		}
	}
	return (freevars);
}

/*}}}*/
/*{{{  PRIVATE void check_channel_direction_usage (varlist *vlist, SOURCEPOSN loc)*/
/*****************************************************************************
 *
 *  Check that channel parameters and free channels of a procedure are only
 *  used for either input or output.
 *
 *****************************************************************************/
PRIVATE void check_channel_direction_usage (varlist *vlist, SOURCEPOSN loc)
{
	for (; vlist != NULL; vlist = VLNextOf (vlist)) {
		if ((VLInputOf (vlist) != NULL) && (VLOutputOf (vlist) != NULL)) {
			treenode *nptr = VLNameOf (vlist);
			treenode *type = NTypeOf (nptr);

			while (TagOf (type) == S_ARRAY) {
				type = ARTypeOf (type);
			}
			if (TagOf (type) == S_PORT) {
				/* skip */
			} else if (NVSharedOf (nptr)) {
				/* skip */
#ifdef MOBILES
			} else if (isdynmobilechantype (nptr) || isdynmobilearray (nptr)) {
				/* skip */
#endif
			} else if (TagOf (nptr) == N_PARAM) {
#if 0
fprintf (stderr, "check_channel_direction_usage(): N_PARAM, bad, NTypeOf (nptr) =");
printtreenl (stderr, 4, NTypeOf (nptr));
#endif
				usereport (FALSE, USE_BAD_CHAN_PARAM, loc, nptr);
			} else {
				usereport (FALSE, USE_BAD_FREE_CHAN, loc, nptr);
			}
		} else if (VLInputOf (vlist) != NULL) {
			treenode *nptr = VLNameOf (vlist);
			treenode *type = NTypeOf (nptr);

			while (TagOf (type) == S_ARRAY) {
				type = ARTypeOf (type);
			}
#if 0	/* this particular checking happens elsewhere now */
			if ((TagOf (type) == S_CHAN) && (TypeAttrOf (type) & TypeAttr_marked_out)) {
#if 0
fprintf (stderr, "something used for input, marked as output. nptr = ");
printtreenl (stderr, 4, nptr);
fprintf (stderr, "*** type (TypeAttr = 0x%x) = ", TypeAttrOf (type));
printtreenl (stderr, 4, type);
#endif
				usereport (FALSE, USE_BAD_CHAN_PARAM_OUT, loc, nptr);
			}
#endif
		} else if (VLOutputOf (vlist) != NULL) {
			treenode *nptr = VLNameOf (vlist);
			treenode *type = NTypeOf (nptr);

			while (TagOf (type) == S_ARRAY) {
				type = ARTypeOf (type);
			}
#if 0	/* this particular checking happens elsewhere now */
			if ((TagOf (type) == S_CHAN) && (TypeAttrOf (type) & TypeAttr_marked_in)) {
#if 0
fprintf (stderr, "something used for output, marked as input. nptr = ");
printtreenl (stderr, 4, nptr);
fprintf (stderr, "*** type (TypeAttr = 0x%x) = ", TypeAttrOf (type));
printtreenl (stderr, 4, type);
#endif
				usereport (FALSE, USE_BAD_CHAN_PARAM_IN, loc, nptr);
			}
#endif
		}
	}
}

/*}}}*/
/*{{{  PRIVATE void aliascheckuse_single*/
PRIVATE void aliascheckuse_single (abbrevlist * const abbrevs, treenode * const n, const INT32 start, const INT32 end, treenode * const element, const int msg)
{
	if (!NVAliasedOf (n)	/* bug 452 23/9/91 */
	    &&inabbrevlist (n, abbrevs, start, end)) {
		if (in_abbrev) {	/* in a real abbreviation - maybe insert a run-time check */
			overlapcheck (n, abbrevs, start, end, element);
		} else {
			usereport (FALSE, msg, lastloc, n);
		}
	}
}

/*}}}*/
/*{{{  PRIVATE void aliascheckuse (use_mode, n, start, end, element)*/
/*****************************************************************************
 *
 *  Check that access to variable 'n' in the range 'start' to 'end' by
 *  method 'use_mode' does not break any aliasing rules.  If it does
 *  then give an error message and if it may do then insert a runtime
 *  check into the tree
 *
 *****************************************************************************/
PRIVATE void aliascheckuse (const use_mode_t use_mode, treenode * const n, const INT32 start, const INT32 end, treenode * const element)
{
	switch (use_mode) {
	case EXP_READ:
		aliascheckuse_single (varabbrevs, n, start, end, element, USE_VAR_VAR_ABBREVIATED);
		break;
	case EXP_WRITTEN:
		aliascheckuse_single (varabbrevs, n, start, end, element, USE_VAR_VAR_ABBREVIATED);
		aliascheckuse_single (valabbrevs, n, start, end, element, USE_VAR_VAL_ABBREVIATED);
		break;
	case CHAN_XINPUT:
	case CHAN_INPUT:
/*if (basetype(NTypeOf(n)) != S_TIMER) *//* INSdi01282 - treat timer same as channel */
		aliascheckuse_single (inabbrevs, n, start, end, element, USE_VAR_VAR_ABBREVIATED);
		break;
	case CHAN_OUTPUT:
		aliascheckuse_single (outabbrevs, n, start, end, element, USE_VAR_VAR_ABBREVIATED);
		break;
	}
}

/*}}}*/
/*{{{  PRIVATE void aliascheckinstance_single (subscrlist *subs, treenode *nptr, abbrevlist *abbrevs, int msg)*/
PRIVATE void aliascheckinstance_single (subscrlist *subs, treenode *nptr, abbrevlist *abbrevs, int msg)
{
	while (subs != NULL) {
		if (inabbrevlist (nptr, abbrevs, SLFirstOf (subs), SLLastOf (subs))) {
			usereport (FALSE, msg, lastloc, nptr);
			/*break */
			return;		 /* (same thing) */
		}
		subs = SLNextOf (subs);
	}
}

/*}}}*/
/*{{{  PRIVATE void aliascheckinstance (varlist *freevars, const int var_errmsg, const int val_errmsg)*/
/*****************************************************************************
 *
 * alias check the free variable usage in a PROC/FUNCTION instance
 *
 *****************************************************************************/
PRIVATE void aliascheckinstance (varlist *freevars, const int var_errmsg, const int val_errmsg)
{
	while (freevars != NULL) {
		treenode *const nptr = VLNameOf (freevars);
		const int type = basetype (NTypeOf (nptr));
		switch (type) {
		case S_CHAN:
		case S_PORT:
		case S_TIMER:	/* INSdi01282 - treat timer same as channel */
			aliascheckinstance_single (VLInputOf (freevars), nptr, inabbrevs, var_errmsg);
			aliascheckinstance_single (VLOutputOf (freevars), nptr, outabbrevs, var_errmsg);
			break;
		default:
			aliascheckinstance_single (VLReadOf (freevars), nptr, varabbrevs, var_errmsg);

			aliascheckinstance_single (VLWrittenOf (freevars), nptr, varabbrevs, var_errmsg);
			aliascheckinstance_single (VLWrittenOf (freevars), nptr, valabbrevs, val_errmsg);
			break;
		}
		freevars = VLNextOf (freevars);
	}
}

/*}}}*/
/*{{{  PRIVATE abbrevlist *varsinabbrevexp (treenode *n)   [and associated stuff]*/
PRIVATE abbrevlist *varsin_alist;
FORWARD PRIVATE abbrevlist *varsinabbrevexp (treenode *);


/*{{{  PRIVATEPARAM int do_varsinabbrevexp (treenode *n, void *const voidptr)*/
/*****************************************************************************
 *
 *  Add to variable list 'varsin_alist' any variables used in the expression 'n'
 *
 *****************************************************************************/
PRIVATEPARAM int do_varsinabbrevexp (treenode *n, void *const voidptr)
{
	DEBUG_MSG (("do_varsinabbrevexp: %s\n", usetagstring (n)));
	switch (TagOf (n)) {
		/*{{{  subscripting / slicing */
	case S_ARRAYSUB:
	case S_SEGMENT:
		{
			treenode *const init_n = n;
			BOOL all_subs_constant = TRUE;
			INT32 start, end;
			/*{{{  search subscripts */
			while ((TagOf (n) == S_ARRAYSUB) || (TagOf (n) == S_SEGMENT)) {
				if (TagOf (n) == S_ARRAYSUB)
					/*{{{  subscript */
				{
					all_subs_constant &= (TagOf (ASIndexOf (n)) == S_CONSTEXP);
					varsin_alist = varsinabbrevexp (ASIndexOf (n));
					n = ASBaseOf (n);
				}
				/*}}} */
				else
					/*{{{  segment */
				{
					if (SStartExpOf (n) != NULL) {	/* dcwbug 10.3.97 */
						all_subs_constant &= (TagOf (SStartExpOf (n)) == S_CONSTEXP);
						varsin_alist = varsinabbrevexp (SStartExpOf (n));
					}
					if (SLengthExpOf (n) != NULL) {
						all_subs_constant &= (TagOf (SLengthExpOf (n)) == S_CONSTEXP);
						varsin_alist = varsinabbrevexp (SLengthExpOf (n));
					}
					n = SNameOf (n);
				}
				/*}}} */
			}
			/*}}} */
			switch (TagOf (n)) {
			default:
				/*{{{  must be a name */
				if (all_subs_constant /* && (elementsin(NTypeOf(n)) >= 0) */ )	/* ignore elementsin bug 646 and 1380 17/10/91 */
					subscripts_accessed (init_n, &start, &end, FALSE);
				else {
					start = -1;
					end = -1;
				}

				varsin_alist = new_abbrev_node (n, varsin_alist, start, end, lastloc, buildsubscripts (init_n));
				/*}}} */
				break;
			case S_STRING:
			case S_CONSTCONSTRUCTOR:
				break;
			case S_CONSTRUCTOR:
#ifdef OCCAM2_5
			case S_FINSTANCE:
#endif
				prewalktree (n, do_varsinabbrevexp, voidptr);
				break;
			}
			return STOP_WALK;
		}
		/*}}} */
		/*{{{  name node */
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_DECL:
	case N_VALPARAM:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_REPL:
		varsin_alist = new_abbrev_node (n, varsin_alist, 0, MAXINDEX, lastloc, NULL);
		break;
		/*}}} */
		/*{{{  specification / VALOF */
	case S_VALOF:
	case S_ABBR:
	case S_RETYPE:
	case S_VALABBR:
	case S_VALRETYPE:
	case S_PROCDEF:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
	case S_DECL:
	case S_TPROTDEF:
	case S_SPROTDEF:
	case S_PRAGMA:		/* bug 829 20/9/91 */
	case S_TYPEDECL:
#if MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
	case S_MPROCDECL:
#endif
		return TRUE;
		/*}}} */
		/*{{{  SIZE */
	case S_SIZE:		/* Don't count the operand of a SIZE as being used */
#ifdef OCCAM2_5
	case S_BYTESIN:
	case S_OFFSETOF:
#endif
		return STOP_WALK;
		/*}}} */
		/*{{{  FINSTANCE */
	case S_FINSTANCE:	/* bug 1276 25/9/91 */
		/* bacause it is a function, all free variables must be reads of 'global'
		   variables, so simply add them to the list.
		   The actual parameters are found simply by continuing the tree walk.
		 */
		{
			varlist *freevars;
			for (freevars = NFreeVarsOf (INameOf (n)); freevars != NULL; freevars = VLNextOf (freevars))
				varsin_alist = new_abbrev_node (VLNameOf (freevars), varsin_alist, 0, MAXINDEX, lastloc, NULL);
		}
		break;
		/*}}} */
	}
	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PRIVATE abbrevlist *varsinabbrevexp (treenode *n)*/
/*****************************************************************************
 *
 *  Add to variable list 'alist' any variables used in the expression 'n'
 *
 *****************************************************************************/
PRIVATE abbrevlist *varsinabbrevexp (treenode *n)
{
	abbrevlist *alist;
	abbrevlist *old_alist = varsin_alist;
	prewalktree (n, do_varsinabbrevexp, NULL);
	alist = varsin_alist;
	varsin_alist = old_alist;
	return alist;
}

/*}}}*/
/*}}}*/
/*{{{  PRIVATE void val_abbreviate (treenode *n)*/
/*****************************************************************************
 *
 *  Add all variables in the expression 'n' to the val-abbreviated list
 *
 *****************************************************************************/
PRIVATE void val_abbreviate (treenode *n)
{
	abbrevlist *vars = varsinabbrevexp (n);
	if (vars != NULL) {
		abbrevlist *vals = vars;
		while (ALNextOf (vals) != NULL)
			vals = ALNextOf (vals);
		SetALNext (vals, valabbrevs);
		valabbrevs = vars;
	}
}

/*}}}*/
/*{{{  PRIVATE void var_abbreviate (treenode *n, use_mode_t use_mode)*/
/*****************************************************************************
 *
 *  Add the accessed range of the base variable in element 'n' to the
 *  var-abbreviated list and any variables in the subscripts or slice
 *  base and count expressions to the val-abbreviated list.
 *
 *****************************************************************************/
PRIVATE void var_abbreviate (treenode *n, use_mode_t use_mode)
{
	treenode *const init_n = n;
	BOOL all_subs_constant = TRUE;
	/*{{{  check subscripts */
	{
		BOOL looping = TRUE;
		while (looping)
			switch (TagOf (n)) {
				/*{{{  subscript */
			case S_ARRAYSUB:
				all_subs_constant &= (TagOf (ASIndexOf (n)) == S_CONSTEXP);
				/* val_abbreviate(ASIndexOf(n)); */
				n = ASBaseOf (n);
				break;
			case S_RECORDSUB:
				n = ASBaseOf (n);
				break;
				/*}}} */
				/*{{{  segment */
			case S_SEGMENT:
				all_subs_constant &= ((SStartExpOf (n) != NULL) &&
						      (TagOf (SStartExpOf (n)) == S_CONSTEXP) &&
						      (SLengthExpOf (n) != NULL) && (TagOf (SLengthExpOf (n)) == S_CONSTEXP));
				/* val_abbreviate(SStartExpOf(n));
				   val_abbreviate(SLengthExpOf(n)); */
				n = SNameOf (n);
				break;
				/*}}} */
			default:
				looping = FALSE;
				break;
			}
	}
	/*}}} */
	/*{{{  add to abbrev list */
	{
		INT32 start, end;
		abbrevlist **abbrevs;
		switch (use_mode) {
		case CHAN_INPUT:
		case CHAN_XINPUT:
			abbrevs = &inabbrevs;
			break;
		case CHAN_OUTPUT:
			abbrevs = &outabbrevs;
			break;
		default:
			abbrevs = &varabbrevs;
			break;
		}
		if (all_subs_constant /*&& (elementsin(NTypeOf(n)) >= 0) */ ) {	/* bug 646 and 1380  17/10/91 */
			subscripts_accessed (init_n, &start, &end, FALSE);
		} else {
			start = -1;
			end = -1;
		}
		*abbrevs = new_abbrev_node (n, *abbrevs, start, end, lastloc, buildsubscripts (init_n));
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATEPARAM void var_abbreviate_seg (treenode *n, void *voidptr)*/
/*****************************************************************************
 *
 *  If 'n' is a segment, var_abbreviate it
 *
 *  We have to flag the case if so, so that we can insert an overlap check
 *  for the rhs (eg [a FROM i FOR j] := [a FROM k FOR l])
 *
 *****************************************************************************/
PRIVATEPARAM void var_abbreviate_seg (treenode *n, void *voidptr)
{
	voidptr = voidptr;
	if (TagOf (n) == S_SEGMENT) {
		var_abbreviate (n, EXP_WRITTEN);
		in_abbrev = TRUE;	/* pretend we're inside an abbreviation for the rhs */
	}
}

/*}}}*/
/*{{{  PRIVATE void var_expression (treenode * n, int abbrev, int test1, int mode1, int test2, int mode2)*/
/*****************************************************************************
 *
 *  Add the accessed range of the base variable in each element of 'n' to the
 *  var-abbreviated list and any variables in the subscripts or slice
 *  base and count expressions to the val-abbreviated list.
 *  Call aliascheckexp before we add the elements to the list.
 *
 *****************************************************************************/
PRIVATE void var_expression (treenode * n, int abbrev, int test1, int mode1, int test2, int mode2)
{
	DEBUG_MSG (("var_expression: %s\n", usetagstring (n)));
	if (TagOf (n) == S_CONSTRUCTOR) {
		for (n = LitExpOf (n); !EndOfList (n); n = NextItem (n)) {
			var_expression (ThisItem (n), abbrev, test1, mode1, test2, mode2);
		}
	} else {
		int saved_abbrev = in_abbrev;
		in_abbrev = abbrev;
		if (test1) {
			aliascheckexp (n, mode1);
		}
		if (test2) {
			aliascheckexp (n, mode2);
		}
		in_abbrev = saved_abbrev;
		if (test1) {
			var_abbreviate (n, mode1);
		}
		if (test2) {
			var_abbreviate (n, mode2);
		}
	}
}

/*}}}*/
/*{{{  PRIVATE varlist *freevarsinsc (treenode *n, int scope, varlist *vlist)*/
/*****************************************************************************
 *
 *  freevarsinsc takes a separately compiled routine body, 'n',
 *  a scope level, 'scope', and a list of
 *  free variables, 'vlist', and adds to 'vlist' any variables used in tree
 *  'n' of scope less than 'scope', which aren't already in vlist.
 *
 *****************************************************************************/
PRIVATE varlist *freevarsinsc (treenode *n, int scope, varlist *vlist)
{
	while (n != NULL) {
		DEBUG_MSG (("freevarsinsc: %s\n", usetagstring (n)));
		switch (TagOf (n)) {
			/*{{{  SEQ PRIPAR */
		case S_SEQ:
		case S_PRIPAR:
			n = CBodyOf (n);
			break;
			/*}}} */
			/*{{{  list */
		case S_LIST:
			vlist = freevarsinsc (ThisItem (n), scope, vlist);
			n = NextItem (n);
			break;
			/*}}} */
			/*{{{  OUTPUT INPUT */
		case S_OUTPUT:
			vlist = freevarsinexp (LHSOf (n), scope, vlist, CHAN_OUTPUT, FALSE);
			return (vlist);
		case S_INPUT:
			vlist = freevarsinexp (LHSOf (n), scope, vlist, CHAN_INPUT, FALSE);
			return (vlist);
			/*}}} */
		default:
			badtag (LocnOf (n), TagOf (n), "freevarsinsc");
		}
	}
	return (vlist);
}

/*}}}*/
/*{{{  PRIVATE void aliascheckexp (treenode *n, use_mode_t use_mode)*/
/*****************************************************************************
 *
 *  Alias check the usage of any variables in expression tree 'n'
 *
 *****************************************************************************/
PRIVATE void aliascheckexp (treenode *n, use_mode_t use_mode)
{
	while (n != NULL) {
		DEBUG_MSG (("aliascheckexp: %s\n", usetagstring (n)));
		switch (TagOf (n)) {
			/*{{{  expression */
			/*{{{  monadics conversions */
		case S_NEG:
		case S_BITNOT:
		case S_NOT:
		case S_UMINUS:
		case S_EXACT:
		case S_ROUND:
		case S_TRUNC:
			n = OpOf (n);
			break;
#ifdef MOBILES
		case S_CLONE:
		case S_DEFINED:
		case S_ADDROF:
		case S_HWADDROF:
			/* things on the RHS of clone and DEFINED are always read */
			n = OpOf (n);
			use_mode = EXP_READ;
			break;
#endif
#ifdef MOBILES
		case S_NEW_ARRAY:
			{
				treenode *list = ARDimLengthOf (n);

				while (!EmptyList (list)) {
					aliascheckexp (ThisItem (list), EXP_READ);
					list = NextItem (list);
				}
				
				if (TypeAttrOf (n) & TypeAttr_aligned) {
					aliascheckexp (ARAlignmentOf (n), EXP_READ);
				}

				return;
			}
		case S_ALLOC_PROC:
		case S_NEW_BARRIER:
			return;
#endif

		case S_SIZE:
		case S_EVAL:
#ifdef OCCAM2_5
		case S_BYTESIN:
		case S_OFFSETOF:
#endif
		case S_TYPEHASHOF:
			return;
			/*}}} */
			/*{{{  dyadics */
		case S_AND:
		case S_OR:
		case S_ADD:
		case S_SUBTRACT:
		case S_MULT:
		case S_DIV:
		case S_REM:
		case S_BITAND:
		case S_BITOR:
		case S_XOR:
		case S_LSHIFT:
		case S_RSHIFT:
		case S_PLUS:
		case S_MINUS:
		case S_TIMES:
		case S_EQ:
		case S_NE:
		case S_LS:
		case S_LE:
		case S_GR:
		case S_GE:
		case S_AFTER:
		case S_COLON2:
			/*case S_INTERVAL : */
			lastloc = LocnOf (n);
			aliascheckexp (LeftOpOf (n), use_mode);
			lastloc = LocnOf (n);
			n = RightOpOf (n);
			break;
			/*}}} */
			/*{{{  constants */
		case S_CONSTEXP:
		case S_CONSTCONSTRUCTOR:
		case S_STRING:
			return;
			/*}}} */
			/*{{{  FINSTANCE */
		case S_FINSTANCE:
			{
				BOOL save_in_abbrev;
				abbrevlist *save_val;
				lastloc = LocnOf (n);
				switch (TagOf (INameOf (n))) {
				case N_SFUNCDEF:
				case N_LFUNCDEF:
				case N_INLINEFUNCDEF:
					aliascheckinstance (NFreeVarsOf (INameOf (n)), USE_VAR_FREEVAR, USE_VAL_FREEVAR);
					break;
				default:	/* no free variables in the others */
					break;
				}
				/*{{{  save info */
				save_in_abbrev = in_abbrev;
				save_val = valabbrevs;
				/*}}} */
				/*{{{  check parameters */
				/* N.B. all function parameters are VALs */
				{
					in_abbrev = TRUE;
					for (n = IParamListOf (n); !EndOfList (n); n = NextItem (n)) {
						aliascheckexp (ThisItem (n), EXP_READ);
						val_abbreviate (ThisItem (n));
					}
				}
				/*}}} */
				/*{{{  restore info */
				in_abbrev = save_in_abbrev;
				while (valabbrevs != save_val)
					valabbrevs = release_headof_abbrevlist (valabbrevs);
				/*}}} */
				insertoverlaps ();
				return;
			}
			/*}}} */
			/*{{{  CONSTRUCTOR */
		case S_CONSTRUCTOR:
			n = LitExpOf (n);
			break;
			/*}}} */
			/*}}} */
			/*{{{  element */
			/*{{{  subscripting / slicing */
		case S_ARRAYSUB:
		case S_SEGMENT:
		case S_RECORDSUB:
			{
				treenode *const init_n = n;
				BOOL all_subs_constant = TRUE;
				lastloc = LocnOf (n);
				/*{{{  check subscripts */
				{
					BOOL looping = TRUE;
					while (looping) {
						switch (TagOf (n)) {
						default:
							looping = FALSE;
							break;
						case S_RECORDSUB:	/* bug INSdi01984 8/4/93 */
							/*{{{  record */
							n = ASBaseOf (n);
							break;
							/*}}} */
						case S_ARRAYSUB:
							/*{{{  subscript */
							all_subs_constant &= (TagOf (ASIndexOf (n)) == S_CONSTEXP);
							aliascheckexp (ASIndexOf (n), EXP_READ);
							if (in_abbrev && (use_mode == EXP_WRITTEN))
								val_abbreviate (ASIndexOf (n));
							n = ASBaseOf (n);
							break;
							/*}}} */
						case S_SEGMENT:
							/*{{{  segment */
							all_subs_constant &= ((SStartExpOf (n) != NULL) &&
									      (TagOf (SStartExpOf (n)) == S_CONSTEXP) &&
									      (SLengthExpOf (n) != NULL) && (TagOf (SLengthExpOf (n)) == S_CONSTEXP));
							aliascheckexp (SStartExpOf (n), EXP_READ);
							aliascheckexp (SLengthExpOf (n), EXP_READ);
							if (in_abbrev && (use_mode == EXP_WRITTEN)) {
								val_abbreviate (SStartExpOf (n));
								val_abbreviate (SLengthExpOf (n));
							}
							n = SNameOf (n);
							break;
							/*}}} */
						}
					}
				}
				/*}}} */
				switch (TagOf (n)) {
				default:
					/*{{{  must be a name */
					{
						INT32 start, end;
						if (all_subs_constant && current_fe_data->fe_checkalias
												/* && (elementsin(NTypeOf(n)) >= 0) */ )
												/* bug 646 and 1380 17/10/91 */
							subscripts_accessed (init_n, &start, &end, FALSE);
						else {
							start = -1;
							end = -1;
						}
						aliascheckuse (use_mode, n, start, end, init_n);
					}
					/*}}} */
					break;
				case S_STRING:
				case S_CONSTCONSTRUCTOR:
					break;
				case S_CONSTRUCTOR:
#ifdef OCCAM2_5
				case S_FINSTANCE:
#endif
					aliascheckexp (n, use_mode);
					break;
				}
				return;
			}
			/*}}} */
			/*{{{  name node */
		case N_VALABBR:
		case N_ABBR:
		case N_VALRETYPE:
		case N_RETYPE:
		case N_DECL:
		case N_VALPARAM:
		case N_PARAM:
		case N_RESULTPARAM:
		case N_REPL:
			/* Whole variable is being used */
			aliascheckuse (use_mode, n, 0, MAXINDEX, n);
			return;
			/*}}} */
			/*{{{  case tag def and other bits */
		case N_TAGDEF:
			/* more added TS/1797 18/08/92 */
		case N_PROCDEF:
		case N_SCPROCDEF:
		case N_LIBPROCDEF:
		case N_LIBMPROCDECL:
		case N_STDLIBPROCDEF:
		case N_INLINEPROCDEF:
		case N_LFUNCDEF:
		case N_SFUNCDEF:
		case N_SCFUNCDEF:
		case N_LIBFUNCDEF:
		case N_STDLIBFUNCDEF:
		case N_INLINEFUNCDEF:
#ifdef MOBILES
		case N_MPROCDECL:
#endif
			/* S_NULLARRAY added 11/02/2003 (frmb) */
		case S_NULLARRAY:
			return;
			/*}}} */
			/*}}} */
			/*{{{  list */
		case S_LIST:
			aliascheckexp (ThisItem (n), use_mode);
			n = NextItem (n);
			break;
			/*}}} */
			/*{{{  else in case statement */
		case S_ELSE:
			return;
			/*}}} */
			/*{{{  specification / VALOF */
		case S_ABBR:
		case S_RETYPE:
		case S_VALABBR:
		case S_VALRETYPE:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_DECL:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_VALOF:
		case S_PRAGMA:	/* bug 829 20/9/91 */
		case S_TYPEDECL:
#if MOBILES
		case S_FORWDECL:
		case S_PROCTYPEDECL:
		case S_MPROCDECL:
#endif
			/* treat an in-line valof as a function call */
			{
				varlist *freevars;
				abbrevlist *save_valabbrevs, *save_varabbrevs;
				abbrevlist *save_inabbrevs, *save_outabbrevs;
				int save_in_abbrev;
				lastloc = LocnOf (n);
				/*{{{  save info */
				save_in_abbrev = in_abbrev;
				save_valabbrevs = valabbrevs;
				save_varabbrevs = varabbrevs;
				save_inabbrevs = inabbrevs;
				save_outabbrevs = outabbrevs;
				valabbrevs = NULL;
				varabbrevs = NULL;
				inabbrevs = NULL;
				outabbrevs = NULL;
				in_abbrev = FALSE;
				/*}}} */
				aliascheck (n);
				freevars = freevarsin (n, -1, NULL, FALSE);
				aliascheckinstance (freevars, USE_VAR_VAR_ABBREVIATED, USE_VAR_VAL_ABBREVIATED);
				release_varlist (freevars);
				/*{{{  restore info */
				while (valabbrevs != NULL)
					valabbrevs = release_headof_abbrevlist (valabbrevs);
				while (varabbrevs != NULL)
					varabbrevs = release_headof_abbrevlist (varabbrevs);
				while (inabbrevs != NULL)
					inabbrevs = release_headof_abbrevlist (inabbrevs);
				while (outabbrevs != NULL)
					outabbrevs = release_headof_abbrevlist (outabbrevs);
				valabbrevs = save_valabbrevs;
				varabbrevs = save_varabbrevs;
				inabbrevs = save_inabbrevs;
				outabbrevs = save_outabbrevs;
				in_abbrev = save_in_abbrev;
				/*}}} */
				return;
			}
			/*}}} */
#ifdef CONDEXP
			/*{{{  conditional expression */
		case S_CONDEXP:
			{
				lastloc = LocnOf (n);
				aliascheckexp (CondExpGuardOf (n), use_mode);
				lastloc = LocnOf (n);
				aliascheckexp (CondExpTrueOf (n), use_mode);
				n = CondExpFalseOf (n);
			}
			break;
			/*}}} */
#endif
#if 0
			/*{{{  structconstructor */
		case S_STRUCTCONSTRUCTOR:
			return;
			/*}}} */
#endif
		/*{{{  channel-direction specifiers (unexpected)*/
		case S_ASINPUT:
		case S_ASOUTPUT:
			usereport (FALSE, USE_UNEXPECTED_CHAN_DIR, LocnOf (n), n);
			return;
		/*}}}*/
		default:
			badtag (LocnOf (n), TagOf (n), "aliascheckexp");
		}
	}
	return;
}

/*}}}*/
/*{{{  PRIVATE void aliascheck (treenode *n)*/
/*****************************************************************************
 *
 * Perform alias check of variables and channels in routine 'n'
 *
 *****************************************************************************/
PRIVATE void aliascheck (treenode *n)
{
	while (n != NULL) {
		DEBUG_MSG (("aliascheck: %s\n", usetagstring (n)));
		switch (TagOf (n)) {
			/*{{{  process */
			/*{{{  STOP SKIP SUSPEND END ELSE GUY ASM FORKING*/
		case S_STOP:
		case S_SKIP:
		case S_SUSPEND:
		case S_END:
		case S_ELSE:
		case S_GUY:
		case S_ASM:
		case S_FORKING:
			return;
			/*}}} */
			/*{{{  CLAIM  (return) */
		case S_CLAIM:
			{
				abbrevlist *save_val;
				treenode *claimvar, *cvtype;

				lastloc = LocnOf (n);
				/* better fold any constants in here..! */
				SetCTemp (n, foldexp (CTempOf (n)));

				claimvar = CTempOf (n);
				aliascheckexp (claimvar, EXP_WRITTEN);
				cvtype = chk_gettype_main (claimvar, TRUE);

				/*{{{  save val abbreviation info */
				save_val = valabbrevs;
				/*}}}*/
#if 0
fprintf (stderr, "use3: aliascheck(): CLAIM: claimvar = ");
printtreenl (stderr, 4, claimvar);
fprintf (stderr, "use3: aliascheck(): CLAIM: cvtype = ");
printtreenl (stderr, 4, cvtype);
#endif
				val_abbreviate (claimvar);			/* restrict variable */

				/* this should be a SHARED mobile channel-type */
				if (TagOf (cvtype) == N_TYPEDECL) {
					int oldclaimed = (NTypeAttrOf (cvtype) & TypeAttr_claimed);

					SetNTypeAttr (cvtype, NTypeAttrOf (cvtype) | TypeAttr_claimed);
					aliascheck (CBodyOf (n));
					SetNTypeAttr (cvtype, ((NTypeAttrOf (cvtype) & ~TypeAttr_claimed) | oldclaimed));
				} else {
					/* this will (presumably) result in an error elsewhere */
					aliascheck (CBodyOf (n));
				}

				/*{{{  restore val abbreviation info */
				valabbrevs = save_val;
				/*}}}*/
			}
			return;
			/*}}}*/
			/*{{{  RESIGN  (return)*/
		case S_RESIGN:
			{
				treenode *bar = CTempOf (n);

				aliascheckexp (bar, EXP_WRITTEN);
				aliascheck (CBodyOf (n));
			}
			return;
			/*}}}*/
			/*{{{  SYNC  (return)*/
		case S_SYNC:
			{
				treenode *bar = LeafLinkOf (n);

				aliascheckexp (bar, EXP_WRITTEN);
			}
			return;
			/*}}}*/
			/*{{{  SEQ IF PAR ALT PRIPAR PRIALT PLACEDPAR */
		case S_SEQ:
		case S_IF:
		case S_PAR:
		case S_ALT:
		case S_PRIPAR:
		case S_PRIALT:
		case S_PLACEDPAR:
		case S_DO:
			n = CBodyOf (n);
			break;
			/*}}} */
			/*{{{  REPLSEQ REPLIF REPLPAR REPLALT PRIREPLPAR PRIREPLALT PLACEDREPLPAR */
		case S_REPLSEQ:
		case S_REPLIF:
		case S_REPLPAR:
		case S_REPLALT:
		case S_PRIREPLPAR:
		case S_PRIREPLALT:
		case S_PLACEDREPLPAR:
		case S_REPLDO:
			lastloc = LocnOf (n);
			aliascheckexp (ReplCStartExpOf (n), EXP_READ);
			aliascheckexp (ReplCLengthExpOf (n), EXP_READ);
			if (ReplCStepExpOf (n)) {
				aliascheckexp (ReplCStepExpOf (n), EXP_READ);
			}
			if (parrepl (TagOf (n))) {	/* bug 1008 9/10/90 */
				syn_lexlevel++;
				DEBUG_MSG (("aliascheck: repl: incrementing syn_lexlevel\n"));
				aliascheck (ReplCBodyOf (n));
				DEBUG_MSG (("aliascheck: repl: decrementing syn_lexlevel\n"));
				syn_lexlevel--;
				return;
			}
			n = ReplCBodyOf (n);
			break;
			/*}}} */
#if 0				/* temporarily disable - why is it different anyway ? */
			/*{{{  PLACEDREPLPAR */
		case S_PLACEDREPLPAR:
			{
				INT32 base = eval_const_treenode (ReplCStartExpOf (n));
				INT32 count = eval_const_treenode (ReplCLengthExpOf (n));
				treenode *name = (treenode *) ReplCNameOf (n);
				n = ReplCBodyOf (n);
				SetNReplKnown (name, TRUE);
				while (count > 0) {
					SetNReplValue (name, base);
					aliascheck (n);
					base++;
					count--;
				}
				SetNReplKnown (name, FALSE);
			}
			return;
			/*}}} */
#endif
			/*{{{  WHILE CHOICE SELECTION */
		case S_WHILE:
		case S_CHOICE:
		case S_SELECTION:
			lastloc = LocnOf (n);
			aliascheckexp (CondGuardOf (n), EXP_READ);
			n = CondBodyOf (n);
			break;
			/*}}} */
			/*{{{  PROCESSOR */
		case S_PROCESSOR:
			lastloc = LocnOf (n);
			aliascheckexp (ProcessorExpOf (n), EXP_READ);
			n = ProcessorBodyOf (n);
			break;
			/*}}} */
			/*{{{  PINSTANCE */
		case S_PINSTANCE:
			{
				treenode *flist;
				treenode *initn = n;
				abbrevlist *save_val, *save_var;
				abbrevlist *save_inabbrevs, *save_outabbrevs;
				lastloc = LocnOf (n);
				/*{{{  save abbrev info */
				save_val = valabbrevs;
				save_var = varabbrevs;
				save_inabbrevs = inabbrevs;
				save_outabbrevs = outabbrevs;
				/*}}} */
				/*{{{  if mobile process activation, include name*/
				if (udv_thingisdynmobileproctype (INameOf (n))) {
#if 0
fprintf (stderr, "use3: var_expression for INameOf (n), n =\n");
printtreenl (stderr, 4, n);
#endif
					var_expression (INameOf (n), TRUE, TRUE, EXP_WRITTEN, FALSE, 0);
				}
				/*}}}*/
				/*{{{  check parameters */
				flist = NParamListOf (INameOf (n));
				{
					n = IParamListOf (n);
					in_abbrev = TRUE;
					while (!EndOfList (flist)) {
						treenode *const formal = ThisItem (flist);
						if (isnamedformal (formal)) {
							treenode *const actual = ThisItem (n);
							const int type = basetype (NTypeOf (formal));
							switch (type) {
							CASE_CONFIG_TYPE	/* bug INSdi01988 08/04/93 */
								/* treat these as VAL params */
								aliascheckexp (actual, EXP_READ);
								val_abbreviate (actual);
								break;
							case S_CHAN:
							case S_PORT:
								/* these cane never be VAL params */
								var_expression (actual, in_abbrev,
										ParamInputOn (formal), CHAN_INPUT,
										ParamOutputOn (formal), CHAN_OUTPUT);
								break;
							case S_FULLBARRIER:
								aliascheckexp (actual, EXP_WRITTEN);
								break;
							default:
								if (TagOf (formal) == N_VALPARAM) {
									aliascheckexp (actual, EXP_READ);
									val_abbreviate (actual);
#ifdef MOBILES
									/* check for CLONE'd parameters */
								} else if (TagOf (actual) == S_CLONE) {
									/* although this is horrifically side-effecting, we only mark as read */
#if 0
fprintf (stderr, "aliascheck: PINSTANCE: CLONE'd actual = \n");
printtreenl (stderr, 4, actual);
#endif
									aliascheckexp (OpOf (actual), EXP_READ);
									val_abbreviate (OpOf (actual));
								} else if (TagOf (actual) != S_NULLARRAY) {
#if 0
fprintf (stderr, "use3: aliascheck(): PINSTANCE: regular looking actual = ");
printtreenl (stderr, 4, actual);
#endif
									/* if we're passing a CLAIMed mobile channel-type, it becomes a value.  check for FIXED state later in use4 */
									if (udv_thingisdynmobilechantype (actual)) {
										treenode *atype = chk_gettype_main (actual, TRUE);

#if 0
fprintf (stderr, "use3: aliascheck(): PINSTANCE: channel-type actual, type = ");
printtreenl (stderr, 4, atype);
#endif
										if ((TagOf (atype) == N_TYPEDECL) && (NTypeAttrOf (atype) & TypeAttr_claimed)) {
											aliascheckexp (actual, EXP_READ);
											val_abbreviate (actual);
										} else {
											var_expression (actual, in_abbrev, TRUE, EXP_WRITTEN, FALSE, 0);
										}
									} else {
										var_expression (actual, in_abbrev, TRUE, EXP_WRITTEN, FALSE, 0);
									}
								}
#else
								}
								if ((TagOf (actual) != S_NULLARRAY) || (TagOf (formal) != N_PARAM)) {
									var_expression (actual, in_abbrev, TRUE, EXP_WRITTEN, FALSE, 0);
								}
#endif
								break;
							}
							n = NextItem (n);
						}
						flist = NextItem (flist);
					}
					in_abbrev = FALSE;
				}
				/*}}} */
				insertoverlaps ();
				switch (TagOf (INameOf (initn))) {
				case N_PROCDEF:
				case N_INLINEPROCDEF:
					aliascheckinstance (NFreeVarsOf (INameOf (initn)), USE_VAR_FREEVAR, USE_VAL_FREEVAR);
					break;
				default:	/* Others won't have any free variables */
					break;
				}
				/*{{{  restore abbrev info */
				while (valabbrevs != save_val) {
					valabbrevs = release_headof_abbrevlist (valabbrevs);
				}
				while (varabbrevs != save_var) {
					varabbrevs = release_headof_abbrevlist (varabbrevs);
				}
				while (inabbrevs != save_inabbrevs) {
					inabbrevs = release_headof_abbrevlist (inabbrevs);
				}
				while (outabbrevs != save_outabbrevs) {
					outabbrevs = release_headof_abbrevlist (outabbrevs);
				}
				/*}}} */
				return;
			}
			/*}}} */
			/*{{{  ASS CASE OUTPUT INPUT TAGGED_INPUT DELAYED_INPUT CASE_INPUT X_INPUT */
			/*{{{  ass */
		case S_ASS:
			{
				treenode *lhs = LHSOf (n), *rhs = RHSOf (n);
				/*{{{  save abbrev info */
				abbrevlist *save_var = varabbrevs;
				abbrevlist *save_val = valabbrevs;
				/* needn't worry about channel stuff for an assignment */
				/*}}} */
				lastloc = LocnOf (n);

#if 0
printf ("use3.c: S_ASS: lhs = ");
printtree (stdout, 4, lhs);
printf ("\nrhs = ");
printtree (stdout, 4, rhs);
printf ("\n");
#endif
				aliascheckexp (lhs, EXP_WRITTEN);

				in_abbrev = FALSE;	/* Disable overlap checks instead of errors BUG 310 */
				if (TagOf (lhs) == S_LIST) {
					walklist (var_abbreviate_seg, lhs, NULL);
				} else {
					var_abbreviate_seg (lhs, NULL);	/* in_abbrev is set TRUE if a segment */
				}

				aliascheckexp (rhs, EXP_READ);
				in_abbrev = FALSE;	/* BUG 310 ACS and CO'N */
				insertoverlaps ();

				/*{{{  restore abbrev info */
				while (valabbrevs != save_val)
					valabbrevs = release_headof_abbrevlist (valabbrevs);
				while (varabbrevs != save_var)
					varabbrevs = release_headof_abbrevlist (varabbrevs);
				/*}}} */
			}
			return;
			/*}}} */
			/*{{{  case */
		case S_CASE:
			lastloc = LocnOf (n);
			aliascheckexp (LHSOf (n), EXP_READ);
			n = RHSOf (n);
			break;
			/*}}} */
			/*{{{  output */
		case S_OUTPUT:
			lastloc = LocnOf (n);
			aliascheckexp (LHSOf (n), CHAN_OUTPUT);
			aliascheckexp (RHSOf (n), EXP_READ);
			return;
			/*}}} */
			/*{{{  input, taggedinput */
		case S_INPUT:
		case S_TAGGED_INPUT:
			lastloc = LocnOf (n);
			aliascheckexp (LHSOf (n), CHAN_INPUT);
			aliascheckexp (RHSOf (n), EXP_WRITTEN);
			return;
			/*}}} */
			/*{{{  extended input */
		case S_X_INPUT:
		case S_X_TAGGED_INPUT:
			lastloc = LocnOf (n);
			aliascheckexp (LHSOf (n), CHAN_INPUT);
			aliascheckexp (RHSOf (n), EXP_WRITTEN);
			aliascheck (ActionDuringOf (n));
			n = ActionAfterOf (n);
			break;
			/*}}} */
			/*{{{  delayedinput */
		case S_DELAYED_INPUT:
			lastloc = LocnOf (n);
			aliascheckexp (LHSOf (n), CHAN_INPUT);
			aliascheckexp (RHSOf (n), EXP_READ);
			return;
			/*}}} */
			/*{{{  caseinput */
		case S_CASE_INPUT:
		case S_X_CASE_INPUT:
			lastloc = LocnOf (n);
			aliascheckexp (LHSOf (n), CHAN_INPUT);
			n = RHSOf (n);
			break;
			/*}}} */
			/*}}} */
			/*{{{  ALTERNATIVE */
		case S_ALTERNATIVE:
			lastloc = LocnOf (n);
			aliascheckexp (AltGuardOf (n), EXP_READ);
			aliascheck (AltInputOf (n));
			n = AltBodyOf (n);
			break;
			/*}}} */
			/*{{{  VARIANT X_VARIANT */
		case S_VARIANT:
			lastloc = LocnOf (n);
			aliascheckexp (VRTaggedListOf (n), EXP_READ);
			n = VRBodyOf (n);
			break;
		case S_X_VARIANT:
			lastloc = LocnOf (n);
			aliascheckexp (VRTaggedListOf (n), EXP_READ);
			aliascheck (VRDuringOf (n));
			n = VRAfterOf (n);
			break;
			/*}}} */
			/*}}} */
#ifdef MOBILES
			/*{{{  forward declaration (specification)*/
		case S_FORWDECL:
			n = DBodyOf (n);
			break;
			/*}}}*/
#endif
			/*{{{  list */
		case S_LIST:
			aliascheck (ThisItem (n));
			n = NextItem (n);
			break;
			/*}}} */
			/*{{{  specification */
		case S_ABBR:
		case S_VALABBR:
		case S_RETYPE:
		case S_VALRETYPE:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_DECL:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_PLACE:
		case S_WSPLACE:
		case S_VSPLACE:
#if 1				/*def CONFIG */
		case S_PLACEON:
			CASE_CONFIG_SPEC
#endif
		case S_PRAGMA:	/* bug 829 19/9/91 */
#ifdef OCCAM2_5
		case S_TYPEDECL:
#endif
#ifdef MOBILES
		case S_PROCTYPEDECL:
		case S_MPROCDECL:
#endif
			/*{{{  handle specification */
			{
				abbrevlist *save_var = varabbrevs;
				abbrevlist *save_val = valabbrevs;
				abbrevlist *save_inabbrevs, *save_outabbrevs;
				save_inabbrevs = inabbrevs;
				save_outabbrevs = outabbrevs;

				while (isspecification (n)) {
					lastloc = LocnOf (n);
					DEBUG_MSG (("aliascheck: (spec) %s\n", usetagstring (n)));
					switch (TagOf (n)) {
						/*{{{  abbr / retype */
					case S_ABBR:
					case S_RETYPE:
						{
							const int type = basetype (NTypeOf (DNameOf (n)));
							switch (type) {
							case S_CHAN:
							case S_PORT:
								/*{{{  chan / port */
								{
									const int use = chan_use (DBodyOf (n), DNameOf (n));
									var_expression (DValOf (n), TRUE,
											(use & CHAN_USE_INPUT) != 0, CHAN_INPUT,
											(use & CHAN_USE_OUTPUT) != 0, CHAN_OUTPUT);
									break;
								}
								/*}}} */
								CASE_CONFIG_TYPE
									/*{{{  treat as a VAL abbrev */
								{	/* bug INSdi01988 08/04/93 */
									treenode *const rhs = DValOf (n);
									in_abbrev = TRUE;
									aliascheckexp (rhs, EXP_READ);
									in_abbrev = FALSE;
									val_abbreviate (rhs);
								}
								break;
								/*}}} */
							default:
								if (udv_thingisdynmobilechantype (DValOf (n))) {
									treenode *atype = chk_gettype_main (DValOf (n), TRUE);

									if ((TagOf (atype) == N_TYPEDECL) && (NTypeAttrOf (atype) & TypeAttr_claimed)) {
										/* abbreviating a CLAIMed something */
										aliascheckexp (DValOf (n), EXP_READ);
										val_abbreviate (DValOf (n));
									} else {
										var_expression (DValOf (n), TRUE, TRUE, EXP_WRITTEN, FALSE, 0);
									}
								} else if (TagOf (DValOf (n)) != S_NULLARRAY) {
									var_expression (DValOf (n), TRUE, TRUE, EXP_WRITTEN, FALSE, 0);
								}
								break;
							}
							insertoverlaps ();
						}
						break;
						/*}}} */
						/*{{{  valabbr / valretype */
					case S_VALABBR:
					case S_VALRETYPE:
						{
							treenode *const rhs = DValOf (n);
							in_abbrev = TRUE;
							aliascheckexp (rhs, EXP_READ);
							in_abbrev = FALSE;
							val_abbreviate (rhs);
							insertoverlaps ();
						}
						break;
						/*}}} */
						/*{{{  proc / func def */
					case S_PROCDEF:
#ifdef MOBILES
					case S_MPROCDECL:
#endif
					case S_SFUNCDEF:
					case S_LFUNCDEF:
					CASE_CONFIG_SPEC
						if (!separatelycompiled (DNameOf (n))) {
							abbrevlist *const l_save_val = valabbrevs;
							abbrevlist *const l_save_var = varabbrevs;
							abbrevlist *const l_save_inabbrevs = inabbrevs;
							abbrevlist *const l_save_outabbrevs = outabbrevs;

							valabbrevs = NULL;
							varabbrevs = NULL;
							inabbrevs = NULL;
							outabbrevs = NULL;

							syn_lexlevel++;	/* bug 1008 9/10/90 */
							DEBUG_MSG (("aliascheck: PROC incrementing syn_lexlevel\n"));
							aliascheck (DValOf (n));
							syn_lexlevel--;
							DEBUG_MSG (("aliascheck: PROC decrementing syn_lexlevel\n"));

							valabbrevs = l_save_val;
							varabbrevs = l_save_var;
							inabbrevs = l_save_inabbrevs;
							outabbrevs = l_save_outabbrevs;
						}
						break;
						/*}}} */
						/*{{{  default: do nothings */
					default:
#if 0
					case S_PLACE:
					case S_WSPLACE:
					case S_VSPLACE:
#if 1				/*def CONFIG */
					case S_PLACEON:
#endif
					case S_PRAGMA:	/* bug 829 19/9/91 */
#endif
						break;
						/*}}} */
					}
					n = DBodyOf (n);
				}
				aliascheck (n);
				/*{{{  restore abbrev info */
				while (valabbrevs != save_val)
					valabbrevs = release_headof_abbrevlist (valabbrevs);
				while (varabbrevs != save_var)
					varabbrevs = release_headof_abbrevlist (varabbrevs);
				while (inabbrevs != save_inabbrevs)
					inabbrevs = release_headof_abbrevlist (inabbrevs);
				while (outabbrevs != save_outabbrevs)
					outabbrevs = release_headof_abbrevlist (outabbrevs);
				/*}}} */
			}
			/*}}} */
			return;
			/*}}} */
			/*{{{  VALOF - in function def */
		case S_VALOF:
			lastloc = LocnOf (n);
			aliascheckexp (VLResultListOf (n), EXP_READ);
			n = VLBodyOf (n);
			break;
			/*}}} */
#if 1				/*def CONFIG */
			/*{{{  confignode */
		case S_SET:
		case S_CONNECT:
		case S_MAP:
			lastloc = LocnOf (n);
			aliascheckexp (STDevOf (n), EXP_READ);
			if (TagOf (n) != S_SET)
				aliascheckexp (STAttrNameOf (n), EXP_READ);
			aliascheckexp (STAttrExpOf (n), EXP_READ);
			return;
			/*}}} */
#endif
		default:
			badtag (LocnOf (n), TagOf (n), "aliascheck");
		}
	}
	return;
}

/*}}}*/
/*{{{  PRIVATEPARAM int do_mark_as_aliased*/
PRIVATEPARAM int do_mark_as_aliased (treenode * const tptr, void *const voidptr)
{
	(void) voidptr;
	switch (TagOf (tptr)) {
	case N_DECL:
	case N_ABBR:
	case N_VALABBR:
	case N_RETYPE:
	case N_VALRETYPE:
	case N_PARAM:
	case N_VALPARAM:
	case N_RESULTPARAM:
		if (!NVAliasedOf (tptr)) {
			SetNVAliased (tptr, TRUE);
			/*printf("Setting %s as aliased\n", WNameOf(NNameOf(tptr))); */
			ripple_aliased_flags (tptr);
		}
		break;
	default:
		break;
	}
	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PRIVATE void ripple_aliased_flags*/
PRIVATE void ripple_aliased_flags (treenode * const nptr)
{
	switch (TagOf (nptr)) {
		/*{{{  VAL abbreviation or RETYPE */
	case N_VALABBR:
	case N_VALRETYPE:
		prewalktree (DValOf (NDeclOf (nptr)), do_mark_as_aliased, NULL);
		break;
		/*}}} */
		/*{{{  non-VAL abbreviation or RETYPE */
	case N_ABBR:
	case N_RETYPE:
		{
			BOOL looping = TRUE;
			treenode *rhs = DValOf (NDeclOf (nptr));
			while (looping)
				switch (TagOf (rhs)) {
				case S_ARRAYSUB:
					prewalktree (ASIndexOf (rhs), do_mark_as_aliased, NULL);
					rhs = ASBaseOf (rhs);
					break;
				case S_RECORDSUB:
					rhs = ASBaseOf (rhs);
					break;
				case S_SEGMENT:
					prewalktree (SStartExpOf (rhs), do_mark_as_aliased, NULL);
					prewalktree (SLengthExpOf (rhs), do_mark_as_aliased, NULL);
					rhs = SNameOf (rhs);
					break;
				default:
					looping = FALSE;
				}
		}
		break;
		/*}}} */
	default:
		break;
	}
}

/*}}}*/
/*{{{  PUBLIC void use_mark_free_vars*/
PUBLIC void use_mark_free_vars (treenode * tptr, const BOOL check_usage)
{
	treenode *routinename = DNameOf (tptr);
	varlist *freevars;

	syn_lexlevel++;		/* bug 1008 9/10/90 */

	if (separatelycompiled (routinename)) {
		freevars = freevarsinsc (DValOf (tptr), -1, NULL);
	} else {
		freevars = freevarsin (DValOf (tptr), -1, NULL, FALSE);
	}

	syn_lexlevel--;

	if (check_usage) {
		/* condition added for bug 842 10/1/91 */
		check_channel_direction_usage (freevars, LocnOf (tptr));
	}

	freevars = record_parameter_info (tptr, freevars);
	SetNFreeVars (routinename, freevars);

#ifdef DEBUG
	if (current_fe_data->fe_debuguse) {
		fprintf (current_fe_data->fe_outfile, "free vars of %s : ", WNameOf (NNameOf (routinename)));
		printfreelist (current_fe_data->fe_outfile, NFreeVarsOf (routinename));
	}
#endif
	return;
}

/*}}}*/
/*{{{  PUBLIC void use_walk_free_vars*/
PUBLIC void use_walk_free_vars (treenode * nptr, int (*fn) (treenode **, void *), void *const voidptr)
{
	varlist *vlist;

	for (vlist = NFreeVarsOf (nptr); vlist != NULL; vlist = VLNextOf (vlist)) {
		if (fn (&VLNameOf (vlist), voidptr) != CONTINUE_WALK) {
			return;
		}
	}
	return;
}

/*}}}*/
/*{{{  PRIVATE void genfreevars (n)*/
/*{{{  PRIVATEPARAM int do_genfreevars*/
/*****************************************************************************
 *
 * Walk over tree 'n' and for each proc/function declared generate its
 * free variable list and store it in proc's name node
 *
 *****************************************************************************/
PRIVATEPARAM int do_genfreevars (treenode * const n, void *const voidptr)
{
	DEBUG_MSG (("do_genfreevars: %s\n", usetagstring (n)));
	switch (TagOf (n)) {
		/*{{{  REPLSEQ REPLIF REPLPAR REPLALT PRIREPLPAR PRIREPLALT PLACEDREPLPAR */
	case S_REPLSEQ:
	case S_REPLIF:
	case S_REPLPAR:
	case S_REPLALT:
	case S_PRIREPLPAR:
	case S_PRIREPLALT:
	case S_PLACEDREPLPAR:
	case S_REPLDO:
		SetNReplKnown (ReplCNameOf (n), FALSE);
		if (parrepl (TagOf (n))) {	/* bug 1008 9/10/90 */
			syn_lexlevel++;
			DEBUG_MSG (("do_genfreevars: repl: incrementing syn_lexlevel\n"));
			prewalktree (ReplCBodyOf (n), do_genfreevars, voidptr);
			syn_lexlevel--;
			DEBUG_MSG (("do_genfreevars: repl: decrementing syn_lexlevel\n"));
			return STOP_WALK;
		}
		break;		/* continue onto children */
		/*}}} */

#if 0				/* Check this later, during the usage check */
		/* to fix bug 278 in buglist (ie expand replicators first) */
		/*{{{  ASS */
	case S_ASS:
		if (TagOf (LHSOf (n)) == S_LIST) {
			/* Check for overlap of LHS of multiple assignment */
			varlist *vars_used = NULL;
			varlist *uses_vars;
			SOURCEPOSN oldloc = lastloc;
			lastloc = LocnOf (n);
			n = LHSOf (n);
			while (n != NULL) {
				uses_vars = freevarsinexp (ThisItem (n), -1, NULL, EXP_WRITTEN, TRUE);
				check_no_overlaps (vars_used, uses_vars, lastloc, FALSE);
				vars_used = merge (uses_vars, -1, vars_used);
				release_varlist (uses_vars);
				n = NextItem (n);
			}
			release_varlist (vars_used);
			lastloc = oldloc;
		}
		break;

		/*}}} */
#endif /* 0 */

		/*{{{  proc / func def */
	case S_PROCDEF:
#ifdef MOBILES
	case S_MPROCDECL:
#endif
	case S_SFUNCDEF:
	case S_LFUNCDEF:
		{
			treenode *routinename = DNameOf (n);

			syn_lexlevel++;	/* bug 1008 9/10/90 */
			DEBUG_MSG (("do_genfreevars: PROC: incrementing syn_lexlevel\n"));

			if (!separatelycompiled (routinename)) {
				prewalktree (DValOf (n), do_genfreevars, voidptr);	/*generate for nested routines */
			}

			DEBUG_MSG (("do_genfreevars: PROC: decrementing syn_lexlevel\n"));
			syn_lexlevel--;

			use_mark_free_vars (n, current_fe_data->fe_checkusage);

			if ((TagOf (n) == S_MPROCDECL) && (NFreeVarsOf (routinename))) {
#if 0
fprintf (stderr, "use3: do_genfreevars: free in n = ");
printtreenl (stderr, 4, n);
#endif
				usereport (FALSE, USE_MPP_FREEVARS, LocnOf (n), n);
			}

			prewalktree (DBodyOf (n), do_genfreevars, voidptr);
			return STOP_WALK;
		}

		/*}}} */
		/*{{{  abbr / retype */
	case S_ABBR:
	case S_VALABBR:
	case S_RETYPE:
	case S_VALRETYPE:
		{
			treenode *const nptr = DNameOf (n);
			SetNParamUse (nptr, NULL);
			if (NVAliasedOf (nptr)) {
				ripple_aliased_flags (nptr);
			}
			if (TagOf (n) == S_ABBR || TagOf (n) == S_RETYPE) {
				treenode *const name = nameof (DValOf (n));

				if ((nodetypeoftag (TagOf (name)) == NAMENODE) && NVAliasedOf (name)) {
					(void) do_mark_as_aliased (nptr, NULL);
				}
			}
		}
		break;
		/*}}} */
		/*{{{  decl */
	case S_DECL:
		{
			treenode *list = DNameOf (n);

			/* usage-count of thing declared to NULL */
			if (TagOf (list) == S_LIST) {
				while (list != NULL) {
					SetNParamUse (ThisItem (list), NULL);
					list = NextItem (list);
				}
			} else {
				SetNParamUse (list, NULL);
			}
			break;
		}
		/*}}} */
	}
	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PRIVATE void genfreevars(n)*/
/*****************************************************************************
 *
 * Walk over tree 'n' and for each proc/function declared generate its
 * free variable list and store it in proc's name node.
 * Also checks free/param channel direction usage and multiple assignment
 *                                                ^^^^^^^^^^^^^^^^^^^^^^^
 *                                                Not any more (bug 278)
 *
 *****************************************************************************/
PRIVATE void genfreevars (treenode * const n)
{
	prewalktree (n, do_genfreevars, NULL);
}

/*}}}*/
/*}}}*/

/*{{{  PUBLIC BOOL local_paraminputoroutputon()*/
/*****************************************************************************
 *
 *  if inputnotoutput is TRUE,
 *     returns TRUE if a process inputs on the channel parameter 'n'.
 *  else
 *     returns TRUE if a process outputs on the channel parameter 'n'.
 *
 *****************************************************************************/
PUBLIC BOOL local_paraminputoroutputon (treenode * const n, const BOOL inputnotoutput)
{
	return (inputnotoutput ? ParamInputOn (n) : ParamOutputOn (n));
#if 0
	/* not necessary.. */
	treenode *type;
	int markedattr;

	type = NTypeOf (n);
	while (TagOf (type) == S_ARRAY) {
		type = ARTypeOf (type);
	}
	if ((TagOf (type) == S_CHAN) || (TagOf (type) == S_PORT)) {
		markedattr = (TypeAttrOf (type) & (TypeAttr_marked_in | TypeAttr_marked_out));
	} else {
		markedattr = 0;
	}
	if (inputnotoutput) {
		if (markedattr & TypeAttr_marked_in) {
			return TRUE;
		}
		return ParamInputOn (n);
	}
	if (markedattr & TypeAttr_marked_out) {
		return TRUE;
	}
	return ParamOutputOn (n);
#endif
}

/*}}}*/
/*{{{  PUBLIC BOOL local_isafreevarof(v, n)*/
/*****************************************************************************
 *
 *  isafreevarof returns TRUE if variable 'v' is a free variable of routine
 *  'n', FALSE otherwise. This routine assumes that alias checking has
 *  already taken place.
 *
 *****************************************************************************/
PUBLIC BOOL local_isafreevarof (treenode * const v, treenode * const n)
{
	/* freevars = freevarsin(n, -1, NULL); */
	varlist *const freevars = NFreeVarsOf (n);
	const BOOL result =
		invarlist (v, freevars, EXP_READ, 0, MAXINDEX) ||
		invarlist (v, freevars, EXP_WRITTEN, 0, MAXINDEX) ||
		invarlist (v, freevars, CHAN_INPUT, 0, MAXINDEX) || invarlist (v, freevars, CHAN_OUTPUT, 0, MAXINDEX);
	/* release_varlist(freevars); */
	return (result);
}

/*}}}*/

/*{{{  PUBLIC void alias_and_usage_check*/
/*****************************************************************************
 *
 *  alias_and_usage_check is an alias and usage checker
 *  which first builds up lists of free
 *  variables for routines and places them in the routines' namenodes.
 *  The routine is then alias checked and then usage checked.
 *
 *  If fptr is not NULL, then information is written to that file.
 *
 *****************************************************************************/
PUBLIC void alias_and_usage_check (treenode * const n, const BOOL checkalias, const BOOL checkusage, FILE * const fptr)
{
	const BOOL info = (fptr != NULL) && ((current_fe_data->fe_lang & FE_LANG_NDL) == 0);
	int error_count = 0;
	int warning_count = 0;
	uerrors = NULL;
	genfreevars (n);	/* also checks free/param channel direction usage
				 * and multiple assignment <== (Not any more - bug 278)
				 */
	if (checkalias) {
		/*{{{  do the alias checking, and the usage checking, possibly */
		if (constexp_one == NULL) {
			constexp_one = newconstant (1);
		}
		if (info) {
			fprintf (fptr, "Alias%s checking\n", checkusage ? " and usage" : "");
		}
		overlapchain = NULL;
		aliascheck (n);
		if (checkusage) {
			usagecheck (n);		/* check parallel usage of things */
		} else {
			if (info) {
				msg_out (SEV_INFO_NOTERR, USE, USE_NO_USAGECHECK, NOPOSN);
			}
		}
		/*}}} */
	} else {
		if (info) {
			msg_out (SEV_INFO_NOTERR, USE, USE_NO_ALIASCHECK, NOPOSN);
		}
	}
	/* report any errors */
	for (; uerrors != NULL; uerrors = ERNextOf (uerrors)) {
		const int sev = (current_fe_data->fe_warn_on_usage_error || ERWarnOf (uerrors)) ? SEV_WARN : SEV_ERR;
		treenode *errname = ERP1Of (uerrors);

#if 0
fprintf (stderr, "alias_and_usage_check (report): sev = %s, name = ", (sev == SEV_WARN) ? "warn" : "err");
printtreenl (stderr, 4, errname);
#endif
		msg_out_s (sev, USE, ERCodeOf (uerrors), ERLocnOf (uerrors), (errname != NULL) ? use_exprstring (errname) : NULL);
			   /* (ERP1Of (uerrors) != NULL) ? WNameOf (NNameOf (ERP1Of (uerrors))) : NULL); */
		if (sev == SEV_WARN) {
			warning_count++;
		} else {
			error_count++;
		}
	}
	if (checkalias && info) {
		/*{{{  give some information */
		fprintf (fptr, "Alias%s checked ", checkusage ? " and usage" : "");
		if (error_count != 0 || warning_count != 0) {
			fprintf (fptr, "- %d error%s, %d warning%s\n",
				 error_count, error_count != 1 ? "s" : "", warning_count, warning_count != 1 ? "s" : "");
		} else {
			fputs ("ok\n", fptr);
		}
		/*}}} */
	}

	/* do other checking here */
	if (!no_undefinedness_check) {
		undefinedcheck (n);	/* check sequential usage of things */
	}

	return;
}

/*}}}*/
