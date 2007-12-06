/* $Id: use2.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	Occam two usage checker
 *	Copyright (C) 1987 Inmos Limited
 *	Modifications (C) 2004 Fred Barnes  <frmb@kent.ac.uk>
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

/*{{{  include files */
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
#include "syndef.h"		/* syn_lexlevel */
#include "chkdef.h"		/* current_fe_handle etc */
#ifdef MOBILES
#include "trandef.h"		/* isdynmobilearray/isdynmobilechantypetype */
#endif	/* MOBILES */
/*}}}*/

/*{{{  static variables */
PRIVATE SOURCEPOSN lastloc = NOPOSN;
PRIVATE int forking_scope = 0;
PRIVATE int forking_lexlevel = -1;
PRIVATE treenode *forking_node = NULL;
PRIVATE int forking_count = 0;
PRIVATE treenode *curroutine = NULL;
PRIVATE int inside_claim = 0;
PRIVATE int inside_xinput = 0;
/*}}}*/
/*{{{  external references*/
extern treenode *enclosing_par;		/* in use1.c */
/*}}}*/

/*{{{  DEBUG_MSG */
#undef DEBUG_MSG
#ifdef DEBUG
#define DEBUG_MSG(X) { if (current_fe_data->fe_debuguse) printf X; }
#else
#define DEBUG_MSG(X)
#endif
/*}}}*/

/*{{{  PRIVATE int accesses_overlap (slist1, slist2) */
/*****************************************************************************
 *
 *  Test whether any of the accesses specified in the two lists of accessed
 *  subscripts 'slist1' and 'slist2' overlap.
 *
 *****************************************************************************/
PRIVATE int accesses_overlap (subscrlist * slist1, subscrlist * slist2)
{
	while (slist1 != NULL) {
		if (subscr_overlaps (slist2, SLFirstOf (slist1), SLLastOf (slist1)))
			return (TRUE);
		slist1 = SLNextOf (slist1);
	}
	return (FALSE);
}

/*}}}*/
/*{{{  PRIVATE void check_no_overlaps (vlist1, vlist2, loc) */
/*****************************************************************************
 *
 *  Check that accesses do not overlap.  If they do generate the appropriate
 *  error message.
 *  Written variables must not overlap with the same variable being read or
 *  written in the other list.
 *  Channel input usage must not overlap
 *  Channel output usage must not overlap
 *  SHC, 3-Jan-89: Timers are not checked.
 *
 *****************************************************************************/
PRIVATE void check_no_overlaps (varlist * vlist1, varlist * vlist2, SOURCEPOSN loc, const BOOL checking_counted_array, const BOOL in_pri_par)
{				/* INSdi01282 */
	for (; vlist1 != NULL; vlist1 = VLNextOf (vlist1)) {
		treenode *const n = VLNameOf (vlist1);
		const BOOL is_timer = basetype (NTypeOf (n)) == S_TIMER;
#if 0
		printf ("check_no_overlaps: n from vlist1 is %s, is_timer? %d, in_pri_par? %d, shared? %d\n",
			WNameOf (NNameOf (n)), is_timer, in_pri_par, NVSharedOf (n));
#endif
		if ((!is_timer || in_pri_par) && !NVSharedOf (n))
			/*{{{  do check */
		{
			varlist *list;
			for (list = vlist2; (list != NULL) && (VLNameOf (list) != n); list = VLNextOf (list)) {
				/*printf("check_no_overlaps: n from vlist2 is %s\n", WNameOf(NNameOf(VLNameOf(list)))); */
				/* skip */
			}
			if (list != NULL) {
				/*printf("check_no_overlaps: n from vlist2 is finally %s\n", WNameOf(NNameOf(VLNameOf(list)))); */
				/*const BOOL variableio = current_fe_data->fe_txlib->hasvariableio; */
				if (is_timer)
				 {	/* INSdi01282 and hence we must be inside a PRI PAR */
					/*{{{  check for timer use in a PRI PAR */
					/* TIMERs are not marked with direction use when separately
					   compiled, thus since they are non-VAL parameters, it
					   thinks that they are 'written'.
					   therefore we have to check for
					   'Input'/'Written' against 'Input'/'Written'.
					 */
					if (accesses_overlap (VLInputOf (vlist1), VLInputOf (list)) ||
					    accesses_overlap (VLWrittenOf (vlist1), VLWrittenOf (list)) ||
					    accesses_overlap (VLInputOf (vlist1), VLWrittenOf (list)) ||
					    accesses_overlap (VLWrittenOf (vlist1), VLInputOf (list)))
						usereport (TRUE, USE_TIMER_IN_PRI_PAR, loc, n);
				}
				/*}}} */
				else {
					/*{{{  Check for written in parallel */
					if (accesses_overlap (VLWrittenOf (vlist1), VLWrittenOf (list))) {
						if (checking_counted_array) {	/* bug 1183 19/3/91 */
							/* bug TS/1172 means that we have to fail on this */
							usereport (FALSE, USE_COLON2_INPUT, loc, n);
						} else if ((TagOf (NTypeOf (n)) == S_FULLBARRIER) || ((TagOf (NTypeOf (n)) == S_MOBILE) && (TagOf (MTypeOf (NTypeOf (n))) == S_FULLBARRIER))) {
							usereport (FALSE, USE_BARRIER_PARALLEL, loc, n);
						} else {
							usereport (FALSE, USE_WRITTEN_IN_PAR, loc, n);
						}
					}
					/*}}} */
					/*{{{  Check for read and written in parallel */
					if (accesses_overlap (VLWrittenOf (vlist1), VLReadOf (list)) ||
					    accesses_overlap (VLReadOf (vlist1), VLWrittenOf (list))) {
						if (checking_counted_array)	/* bug 1183 19/3/91 */
#if 0
							usereport (!variableio, variableio ? USE_COLON2_INPUT : USE_FUTURE_COLON2_INPUT, loc, n);
#else
							/* bug TS/1172 means that we have to fail on this */
							usereport (FALSE, USE_COLON2_INPUT, loc, n);
#endif
						else
							usereport (FALSE, USE_READ_AND_WRITTEN_IN_PAR, loc, n);
					}
					/*}}} */
					/*{{{  Check channel usages */
#ifdef MOBILES
					/* don't check dynamic mobile channel arrays, that's a run-time thing.. */
					/* don't check mobile channel types, providing they're SHARED! */
					if (!isdynmobilearray (n) && !(isdynmobilechantype (n) && (NTypeAttrOf (NTypeOf (n)) & TypeAttr_shared)))
#endif	/* MOBILES */
					{
						if (accesses_overlap (VLInputOf (vlist1), VLInputOf (list)))
							usereport (FALSE, USE_INPUT_IN_PAR, loc, n);
						if (accesses_overlap (VLOutputOf (vlist1), VLOutputOf (list)))
							usereport (FALSE, USE_OUTPUT_IN_PAR, loc, n);
					}
					/*}}} */
				}
			}
		}
		/*}}} */
	}
}

/*}}}*/

/*{{{  PRIVATE void applytocountedinputs */
PRIVATE void applytocountedinputs (treenode * tptr, void (*f) (treenode *, void *), void *const voidptr)
/*****************************************************************************
 *  applytocountedinputs applies the supplied function to any counted array
 *                       protocol inputs which are part of the supplied
 *                       input statement.
 *****************************************************************************/
{
	switch (TagOf (tptr)) {
	default:
		badtag (LocnOf (tptr), TagOf (tptr), "applytocountedinputs");
		break;
	case S_INPUT:
	case S_X_INPUT:
	case S_TAGGED_INPUT:
	case S_X_TAGGED_INPUT:
		for (tptr = RHSOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr)) {
			if (TagOf (ThisItem (tptr)) == S_COLON2) {
				f (ThisItem (tptr), voidptr);
			}
		}
		break;
	case S_CASE_INPUT:
	case S_X_CASE_INPUT:
		for (tptr = RHSOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr)) {
			treenode *variant = skipspecifications (ThisItem (tptr));

			if ((TagOf (variant) != S_VARIANT) && (TagOf (variant) != S_X_VARIANT)) {
				badtag (LocnOf (variant), TagOf (variant), "applytocountedinputs");
			}
			for (variant = VRTaggedListOf (variant); !EndOfList (variant); variant = NextItem (variant)) {
				if (TagOf (ThisItem (variant)) == S_COLON2) {
					f (ThisItem (variant), voidptr);
				}
			}
		}
		break;
	}
}

/*}}}*/
/*{{{  PRIVATE varlist *check_multipleassign */
PRIVATE varlist *check_multipleassign (treenode * tptr, varlist * vars_used, const SOURCEPOSN locn, const BOOL checking_counted_array)
{
	varlist *uses_vars = freevarsinexp (tptr, -1, NULL, EXP_WRITTEN, TRUE);
	check_no_overlaps (vars_used, uses_vars, locn, checking_counted_array, FALSE);
	vars_used = merge (uses_vars, -1, vars_used);
	release_varlist (uses_vars);
	return vars_used;
}

/*}}}*/
/*{{{  PRIVATEPARAM void checkcountedinput */
PRIVATE varlist *checkcountedinput_vlist;
PRIVATE int checkcountedinput_scope;

PRIVATEPARAM void checkcountedinput (treenode * tptr, void *voidptr)
/* Check for 'overlap' of the length and array expression */
{
	varlist *vars_used;
	SOURCEPOSN oldloc = lastloc;
	lastloc = LocnOf (tptr);
	voidptr = voidptr;

	/*fprintf(current_fe_data->fe_outfile, "checkcountedinput: Locn: %lX\n", LocnOf(tptr)); */

	if (TagOf (tptr) != S_COLON2)
		badtag (LocnOf (tptr), TagOf (tptr), "checkcountedinput");
	vars_used = check_multipleassign (LeftOpOf (tptr), NULL, lastloc, TRUE);
	vars_used = check_multipleassign (RightOpOf (tptr), vars_used, lastloc, TRUE);

	checkcountedinput_vlist = merge (vars_used, checkcountedinput_scope, checkcountedinput_vlist);
	release_varlist (vars_used);

	lastloc = oldloc;
}

/*}}}*/
/*{{{  PUBLIC varlist *use_freevarsin (n, scope, vlist) */
/*****************************************************************************
 *
 *  use_freevarsin takes a treenode, 'n', a scope level, 'scope', and a list of
 *  free variables, 'vlist', and adds to 'vlist' any variables used in tree
 *  'n' of scope less than 'scope', which aren't already in vlist.
 *  for the special cases not handled by the general freevarsin in use1.c
 *
 *****************************************************************************/

PUBLIC varlist *use_freevarsin (treenode * n, int scope, varlist * vlist)
{
	DEBUG_MSG (("use_freevarsin: %s\n", usetagstring (n)));
	switch (TagOf (n)) {
		/*{{{  ASS */
		/* Added here to fix bug 278 in buglist */
	case S_ASS:
		if (TagOf (LHSOf (n)) == S_LIST) {
			/* Check for overlap of LHS of multiple assignment */
			varlist *vars_used = NULL;
			SOURCEPOSN oldloc = lastloc;
			lastloc = LocnOf (n);
			for (n = LHSOf (n); !EndOfList (n); n = NextItem (n))
				vars_used = check_multipleassign (ThisItem (n), vars_used, lastloc, FALSE);
			vlist = merge (vars_used, scope, vlist);
			release_varlist (vars_used);
			lastloc = oldloc;
		}
		break;
		/*}}} */
		/*{{{  INPUT TAGGED_INPUT CASE_INPUT */
		/* Added for bug 1183 19/3/91 */
	case S_INPUT:
	case S_X_INPUT:
	case S_TAGGED_INPUT:
	case S_X_TAGGED_INPUT:
	case S_CASE_INPUT:
	case S_X_CASE_INPUT:
		{
			varlist *saved_vlist = checkcountedinput_vlist;
			int saved_scope = checkcountedinput_scope;
			checkcountedinput_vlist = vlist;
			checkcountedinput_scope = scope;

			applytocountedinputs (n, checkcountedinput, NULL);
			vlist = checkcountedinput_vlist;

			checkcountedinput_scope = saved_scope;
			checkcountedinput_vlist = saved_vlist;
		}
		break;
		/*}}} */
		/*{{{  PAR PRIPAR PLACEDPAR */
	case S_PAR:
	case S_PRIPAR:
	case S_PLACEDPAR:
	case S_DO:
		{
			const BOOL in_pri_par = TagOf (n) == S_PRIPAR;
			varlist *vars_used = NULL;
			varlist *uses_vars;
			SOURCEPOSN save_lastloc = lastloc;
			treenode *saved_enclosing_par = enclosing_par;

			enclosing_par = n;
#if 0
fprintf (stderr, "use_freevarsin: PAR: here.\n");
#endif

			lastloc = LocnOf (n);
			for (n = CBodyOf (n); !EndOfList (n); n = NextItem (n)) {
				uses_vars = freevarsin (ThisItem (n), -1, NULL, TRUE);
				check_no_overlaps (vars_used, uses_vars, lastloc, FALSE, in_pri_par);
				vars_used = merge (uses_vars, -1, vars_used);
				release_varlist (uses_vars);
			}
			vlist = merge (vars_used, scope, vlist);
			release_varlist (vars_used);
			lastloc = save_lastloc;

			enclosing_par = saved_enclosing_par;
		}
		break;
		/*}}} */
		/*{{{  REPLPAR PRIREPLPAR PLACEDREPLPAR */
	case S_REPLPAR:
	case S_PRIREPLPAR:
	case S_PLACEDREPLPAR:
	case S_REPLDO:
		{
			const BOOL in_pri_par = TagOf (n) == S_PRIREPLPAR;
			const SOURCEPOSN save_lastloc = lastloc;
			treenode *saved_enclosing_par = enclosing_par;

			if (scope < 0) {
				scope = NScopeOf (ReplCNameOf (n));
			}
			vlist = freevarsinexp (ReplCStartExpOf (n), scope, vlist, EXP_READ, TRUE);
			vlist = freevarsinexp (ReplCLengthExpOf (n), scope, vlist, EXP_READ, TRUE);
			if (ReplCStepExpOf (n)) {
				vlist = freevarsinexp (ReplCStepExpOf (n), scope, vlist, EXP_READ, TRUE);
			}
			lastloc = LocnOf (n);
			enclosing_par = n;

			syn_lexlevel++;	/* bug 1008 9/10/90 */
			DEBUG_MSG (("use_freevarsin: incrementing syn_lexlevel\n"));
			if (is_evaluable (ReplCStartExpOf (n)) &&
			    is_evaluable (ReplCLengthExpOf (n)) && ((ReplCStepExpOf (n) == NULL) ? TRUE : is_evaluable (ReplCStepExpOf (n)))) {

				/*{{{  base and count both known */
				varlist *vars_used = NULL;
				varlist *uses_vars;
				INT32 base = eval_const_treenode (ReplCStartExpOf (n));
				INT32 count = eval_const_treenode (ReplCLengthExpOf (n));
				INT32 step;
				treenode *name = ReplCNameOf (n);

				if (ReplCStepExpOf (n) != NULL) {
					step = eval_const_treenode (ReplCStepExpOf (n));
				} else {
					step = 1;
				}
				n = ReplCBodyOf (n);
				SetNReplKnown (name, TRUE);
				while (count > 0) {
					DEBUG_MSG (("use_freevarsin: repl %s set to %ld\n", WNameOf (NNameOf (name)), base));
					SetNReplValue (name, base);
					uses_vars = freevarsin (n, NScopeOf (name), NULL, TRUE);
					check_no_overlaps (vars_used, uses_vars, lastloc, FALSE, in_pri_par);
					vars_used = merge (uses_vars, -1, vars_used);
					release_varlist (uses_vars);
					base += step;
					count--;
				}
				vlist = merge (vars_used, scope, vlist);
				release_varlist (vars_used);
				SetNReplKnown (name, FALSE);
				/*}}} */
			} else if (((TagOf (n) == S_REPLPAR) || (TagOf (n) == S_PRIREPLPAR)) && !is_evaluable (ReplCLengthExpOf (n))) {
				/*{{{  runtime sized PAR replicator -- emit warning about not usage-checking */
				usereport (TRUE, USE_WARN_NREPLPAR_NOUSAGE, LocnOf (n), NULL);
				/*}}}  */
			} else {
				/*{{{  base not known */
				/* replicator base is unknown so no access to free vars */
				varlist *uses_vars;
				SetNReplKnown (ReplCNameOf (n), FALSE);
				n = ReplCBodyOf (n);
				uses_vars = freevarsin (n, -1, NULL, TRUE);
				check_no_overlaps (uses_vars, uses_vars, lastloc, TRUE,	/* ie check no free vars */
						   in_pri_par);
				vlist = merge (uses_vars, scope, vlist);	/* just in case there were any */
				release_varlist (uses_vars);
				/*}}} */
			}
			DEBUG_MSG (("use_freevarsin: decrementing syn_lexlevel\n"));
			syn_lexlevel--;
			lastloc = save_lastloc;
			enclosing_par = saved_enclosing_par;
		}
		break;
		/*}}} */
	}
	return vlist;
}

/*}}}*/
/*{{{  PRIVATE BOOL goes_par (n) */
/*{{{  PRIVATEPARAM void find_counted_inputs */
PRIVATEPARAM void find_counted_inputs (treenode * tptr, void *const voidptr)
{
	(void) tptr;		/* stop warning message */
	/*fprintf(current_fe_data->fe_outfile, "find_counted_inputs: Locn: %lX\n", LocnOf(tptr)); */
	*(BOOL *) voidptr = TRUE;
}

/*}}}*/
/*{{{  PRIVATEPARAM int do_goes_par (n) */
/*****************************************************************************
 *
 *  Test whether a process goes PAR at all.  PROC and FUNCTION calls
 *  are not expanded.
 *
 *****************************************************************************/
PRIVATEPARAM int do_goes_par (treenode * n, void *voidptr)
{
	if (!*(BOOL *) voidptr) {
		DEBUG_MSG (("do_goes_par: %s\n", usetagstring (n)));
		switch (TagOf (n)) {
		case S_PAR:
		case S_PRIPAR:
		case S_PLACEDPAR:
		case S_REPLPAR:
		case S_PRIREPLPAR:
		case S_PLACEDREPLPAR:
		case S_DO:
		case S_REPLDO:
			*(BOOL *) voidptr = TRUE;
			break;
		case S_ASS:
			/* Added so that replicators are checked inside multi-assign */
			/* (fixing bug 278 in buglist) */
			*(BOOL *) voidptr = (TagOf (LHSOf (n)) == S_LIST);
			break;
			/* Added for bug 1183 19/3/91: */
		case S_INPUT:
		case S_X_INPUT:
		case S_TAGGED_INPUT:
		case S_X_TAGGED_INPUT:
		case S_CASE_INPUT:
		case S_X_CASE_INPUT:
			{
				BOOL found_counted_input = FALSE;
				applytocountedinputs (n, find_counted_inputs, &found_counted_input);
				*(BOOL *) voidptr = found_counted_input;
			}
			break;
		}
	}
	return (*(BOOL *) voidptr) ? STOP_WALK : CONTINUE_WALK;
}

/*}}}*/
/*{{{  PRIVATE BOOL goes_par (n) */
/*****************************************************************************
 *
 *  Test whether a process goes PAR at all.  PROC and FUNCTION calls
 *  are not expanded.
 *
 *****************************************************************************/
PRIVATE BOOL goes_par (treenode * const n)
{
	BOOL goes_par_result = FALSE;
	prewalkproctree (n, do_goes_par, &goes_par_result);
	return goes_par_result;
}

/*}}}*/
/*}}}*/
/*{{{  PRIVATE BOOL check_sync_params (treenode *n)*/
/*
 *	checks that formal parameters to either a MOBILE PROC or
 *	PROC TYPE declaration are synchronisations only -- haven't been
 *	augmented with $MPP etc. in here
 *
 *	returns TRUE if sync. only params, FALSE if others -- doesn't report error in here
 */
PRIVATE BOOL check_sync_params (treenode *n)
{
	if (!n) {
		return TRUE;
	}
	if ((TagOf (n) == N_MPROCDECL) || (TagOf (n) == N_PROCTYPEDECL)) {
		treenode *params = NParamListOf (n);
		treenode *parms;

		for (parms = params; !EndOfList (parms); parms = NextItem (parms)) {
			treenode *parm = ThisItem (params);

			switch (TagOf (parm)) {
			case N_VALPARAM:
				return FALSE;
			case N_RESULTPARAM:
			case N_PARAM:
				{
					/* must be a channel or mobile channel-type */
					treenode *type = NTypeOf (parm);

					switch (TagOf (type)) {
					case S_CHAN:
						break;
					case N_TYPEDECL:
						/* allowed if channel-type */
						if (!isdynmobilechantypetype (type)) {
							return FALSE;
						}
						break;
					default:
						return FALSE;
					}
				}
				break;
			default:
				msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, LocnOf (n), "check_sync_params() -- not N_PARAM");
				break;
			}
		}
	}
	return TRUE;
}

/*}}}*/
/*{{{  PRIVATEPARAM int do_usagecheck_fixedvars (treenode *n, void *const voidptr)*/
/*
 *	checks for movement of mobile parameters/abbreviations.
 *	This is called from prewalktree so may try and traverse expressions
 *	(we stop it doing that in here)
 */
PRIVATEPARAM int do_usagecheck_fixedvars (treenode *n, void *const voidptr)
{
	int notrashshared = (int)voidptr;
#if 0
fprintf (stderr, "use2: do_usagecheck_fixedvars(): voidptr = 0x%8.8x, n = ", (unsigned int)voidptr);
printtreenl (stderr, 4, n);
#endif
	switch (nodetypeoftag (TagOf (n))) {
		/*{{{  NAMENODE  (stop)*/
	case NAMENODE:
		switch (TagOf (n)) {
		case N_PARAM:
		case N_RESULTPARAM:
		case N_VALPARAM:		/* should not happen */
		case N_ABBR:
		case N_DECL:
			if (ismobile (n)) {
				treenode *mtype = NTypeOf (n);

#if 0
fprintf (stderr, "use2: do_usagecheck_fixedvars(): un-fixing name n = ");
printtreenl (stderr, 4, n);
fprintf (stderr, "use2: do_usagecheck_fixedvars(): mtype = ");
printtreenl (stderr, 4, mtype);
#endif
				if (isdynmobilechantypetype (mtype) && (NTypeAttrOf (mtype) & TypeAttr_shared) && notrashshared) {
					/* don't unfix this mobile (if shared, must be a channel-type) */
				} else if (isdynmobilebarriertype (mtype) && notrashshared) {
					/* don't unfix this mobile either (always shared) */
				} else {
					SetNTypeAttr (n, NTypeAttrOf (n) & ~TypeAttr_fixed);
				}
			}
			break;
		default:
			break;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  DOPNODE, MOPNODE, SEGMENTNODE  (stop)*/
	case MOPNODE:
	case DOPNODE:
	case SEGMENTNODE:
		/* these can't possibly unfix things */
		return STOP_WALK;
		/*}}}*/
		/*{{{  DECLNODE  (stop)*/
	case DECLNODE:
		/* check body of declaration */
		switch (TagOf (n)) {
		case S_ABBR:
		case S_DECL:
			break;
		default:
			prewalktree (DBodyOf (n), do_usagecheck_fixedvars, voidptr);
			break;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  INSTANCENODE  (stop)*/
	case INSTANCENODE:
		{
			treenode *aparams, *fparams;

			/* if any imbalance, have more formals than actuals */
			aparams = IParamListOf (n);
			for (fparams = NParamListOf (INameOf (n)); !EndOfList (aparams) && !EndOfList (fparams); fparams = NextItem (fparams)) {
				treenode *aparm = ThisItem (aparams);
				treenode *fparm = ThisItem (fparams);

				switch (TagOf (fparm)) {
				case N_PARAM:
				case N_VALPARAM:
				case N_RESULTPARAM:
#if 0
fprintf (stderr, "use2: do_usagecheck_fixedvars(): INSTANCENODE, checking fparm = ");
printtreenl (stderr, 4, fparm);
fprintf (stderr, "use2: do_usagecheck_fixedvars(): INSTANCENODE, checking aparm = ");
printtreenl (stderr, 4, aparm);
#endif
					/* some pre-defined PROCs are not sensibly typed (this is ok) */
					if (ismobile (fparm) || (TagOf (INameOf (n)) == N_PREDEFPROC)) {
						/* check actual parameter */
						switch (TagOf (aparm)) {
						case N_PARAM:
						case N_VALPARAM:
						case N_RESULTPARAM:
						case N_ABBR:
						case N_DECL:
							if (ismobile (aparm)) {
								if (!IForkedOf (n)) {
									/* both mobile, unfix actual if formal unfixed */
									if (!(NTypeAttrOf (fparm) & TypeAttr_fixed)) {
										SetNTypeAttr (aparm, NTypeAttrOf (aparm) & ~TypeAttr_fixed);
									}
								} else {
									/* FORKed instance, stays fixed if SHARED */
									if (isdynmobilechantype (aparm) && (NTypeAttrOf (NTypeOf (aparm)) & TypeAttr_shared)) {
										/* stays intact */
									} else if (isdynmobilebarrier (aparm)) {
										/* stays intact too */
									} else {
										/* unfix */
										SetNTypeAttr (aparm, NTypeAttrOf (aparm) & ~TypeAttr_fixed);
									}
								}
							}
							break;
						default:
							break;
						}
					}
					/* advance actual parameter */
					aparams = NextItem (aparams);
					break;
				default:
					break;
				}
			}

		}
		return STOP_WALK;
		/*}}}*/
	default:
		break;
	}
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATEPARAM int do_usagecheck_suspend (treenode *n, void *const voidptr)*/
/*
 *	similar to do_usagecheck() below, but checks explicitly for SUSPEND
 *	(marks "curroutine" if any found)
 *
 *	frmb: also responsible for checking FORKed usage and un-fixing MOBILE
 *	parameters that get "moved"
 */
PRIVATEPARAM int do_usagecheck_suspend (treenode *n, void *const voidptr)
{
#if 0
fprintf (stderr, "use2: do_usagecheck_suspend(): tag = %s, enclosing_par = %p\n", tagstring (TagOf (n)), enclosing_par);
#endif
	switch (TagOf (n)) {
		/*{{{  PAR, PRIPAR*/
	case S_PAR:
	case S_PRIPAR:
		{
			treenode *const saved_enclosing_par = enclosing_par;
			INT32 *saved_barattrs = NULL;
			int nbarattrs = 0;
			treenode *declbars = NULL;
			treenode *extbars = NULL;
			treenode *saved_n;

			enclosing_par = n;
			/*{{{  check for barriers*/
			if (CTempOf (n)) {
				if (TagOf (CTempOf (n)) == S_EXTENDS) {
					declbars = NULL;
					extbars = OpOf (CTempOf (n));
				} else if (TagOf (CTempOf (n)) == S_BAREXTEND) {
					declbars = LeftOpOf (CTempOf (n));
					extbars = RightOpOf (CTempOf (n));
				} else {
					declbars = CTempOf (n);
					extbars = NULL;
				}
			}
			/*}}}*/
			/*{{{  if got barrier extensions, save states in "saved_barattrs" and marked fixed*/
			if (extbars) {
				if (TagOf (extbars) == S_LIST) {
					nbarattrs = listitems (extbars);
				} else {
					nbarattrs = 1;
				}
				saved_barattrs = (INT32 *)newvec (nbarattrs * sizeof (INT32));
				if (TagOf (extbars) == S_LIST) {
					treenode *walk;
					int i;

					for (walk = extbars, i = 0; !EndOfList (walk); walk = NextItem (walk), i++) {
						if (!check_isfullbarrier (ThisItem (walk))) {
							msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, LocnOf (n), "do_usagecheck_suspend() -- bad barrier");
						}
						saved_barattrs[i] = NTypeAttrOf (ThisItem (walk)) & TypeAttr_fixed;
						SetNTypeAttr (ThisItem (walk), NTypeAttrOf (ThisItem (walk)) | TypeAttr_fixed);
					}
				} else {
					if (!check_isfullbarrier (extbars)) {
						msg_out_s (SEV_INTERNAL, USE, USE_INTERNAL_ERROR, LocnOf (n), "do_usagecheck_suspend() -- bad barrier");
					}
					saved_barattrs[0] = NTypeAttrOf (extbars) & TypeAttr_fixed;
					SetNTypeAttr (extbars, NTypeAttrOf (extbars) | TypeAttr_fixed);
				}
			}
			/*}}}*/
			/*{{{  walk parallel processes*/
			saved_n = n;
			for (n = CBodyOf (n); !EndOfList (n); n = NextItem (n)) {
				prewalkproctree (ThisItem (n), do_usagecheck_suspend, voidptr);
			}
			n = saved_n;
			/*}}}*/
			/*{{{  if barrier extensions, put back states and check fixed*/
			if (extbars) {
				if (TagOf (extbars) == S_LIST) {
					treenode *walk;
					int i;

					for (walk = extbars, i = 0; !EndOfList (walk); walk = NextItem (walk), i++) {
						if (!(NTypeAttrOf (ThisItem (walk)) & TypeAttr_fixed)) {
							usereport (FALSE, USE_BARRIER_EXTENDED_UNFIXED, LocnOf (n), ThisItem (walk));
						}
						SetNTypeAttr (ThisItem (walk), (NTypeAttrOf (ThisItem (walk)) & ~TypeAttr_fixed) | saved_barattrs[i]);
					}
				} else {
					if (!(NTypeAttrOf (extbars) & TypeAttr_fixed)) {
						usereport (FALSE, USE_BARRIER_EXTENDED_UNFIXED, LocnOf (n), extbars);
					}
					SetNTypeAttr (extbars, (NTypeAttrOf (extbars) & ~TypeAttr_fixed) | saved_barattrs[0]);
				}
				freevec ((void *)saved_barattrs, nbarattrs * sizeof (INT32));
			}
			/*}}}*/
			enclosing_par = saved_enclosing_par;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  REPLPAR, PRIREPLPAR*/
	case S_REPLPAR:
	case S_PRIREPLPAR:
		break;
		/*}}}*/
		/*{{{  SUSPEND*/
	case S_SUSPEND:
#if 0
fprintf (stderr, "use2: do_usagecheck_suspend: SUSPEND, curroutine = ");
printtreenl (stderr, 4, curroutine);
#endif
		if (curroutine) {
			SetNPSuspends (curroutine, 1);
		}
		if (inside_claim) {
			usereport (FALSE, USE_SUSPEND_INSIDE_CLAIM, LocnOf (n), NULL);
		}
		if (inside_xinput) {
			usereport (FALSE, USE_SUSPEND_INSIDE_XINPUT, LocnOf (n), NULL);
		}
		break;
		/*}}}*/
		/*{{{  X_INPUT, X_TAGGED_INPUT */
	case S_X_INPUT:
	case S_X_TAGGED_INPUT:
		{
			const int saved_inside_xinput = inside_xinput;

			/* check during */
			inside_xinput = 1;
			prewalkproctree (ActionDuringOf (n), do_usagecheck_suspend, voidptr);
			/* check after */
			inside_xinput = saved_inside_xinput;
			prewalkproctree (ActionAfterOf (n), do_usagecheck_suspend, voidptr);
			/* check trashing fixed mobiles */
			prewalktree (RHSOf (n), do_usagecheck_fixedvars, (void *)0);
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  INPUT, TAGGED_INPUT*/
	case S_INPUT:
	case S_TAGGED_INPUT:
		/* check trashing fixed mobiles */
		prewalktree (RHSOf (n), do_usagecheck_fixedvars, (void *)0);
		return STOP_WALK;
		/*}}}*/
		/*{{{  X_CASE_INPUT*/
	case S_X_CASE_INPUT:
		{
			treenode *varlist;
			const int saved_inside_xinput = inside_xinput;

			for (varlist = RHSOf (n); !EndOfList (varlist); varlist = NextItem (varlist)) {
				treenode *thisvar = skipspecifications (ThisItem (varlist));

				/* check during */
				inside_xinput = 1;
				prewalkproctree (VRDuringOf (thisvar), do_usagecheck_suspend, voidptr);
				/* check after */
				inside_xinput = saved_inside_xinput;
				prewalkproctree (VRAfterOf (thisvar), do_usagecheck_suspend, voidptr);

				/* checked inputs for trashing fixed mobiles */
				prewalktree (VRTaggedListOf (thisvar), do_usagecheck_fixedvars, (void *)0);
			}
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  OUTPUT*/
	case S_OUTPUT:
		/* check trashing fixed mobiles -- but only if not shared */
		prewalktree (RHSOf (n), do_usagecheck_fixedvars, (void *)1);
		return STOP_WALK;
		/*}}}*/
		/*{{{  ASS*/
	case S_ASS:
		/* check trashing fixed mobile -- RHS only if not shared */
		prewalktree (LHSOf (n), do_usagecheck_fixedvars, (void *)0);
		prewalktree (RHSOf (n), do_usagecheck_fixedvars, (void *)1);
		return STOP_WALK;
		/*}}}*/
		/*{{{  CASE_INPUT*/
	case S_CASE_INPUT:
		{
			treenode *varlist;

			for (varlist = RHSOf (n); !EndOfList (varlist); varlist = NextItem (varlist)) {
				treenode *thisvar = skipspecifications (ThisItem (varlist));

				/* check process */
				prewalkproctree (VRBodyOf (thisvar), do_usagecheck_suspend, voidptr);

				/* check trashing fixed mobiles in variant list */
				prewalktree (VRTaggedListOf (thisvar), do_usagecheck_fixedvars, (void *)0);
			}
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  CLAIM*/
	case S_CLAIM:
		{
			const int saved_inside_claim = inside_claim;

			inside_claim = 1;
			prewalkproctree (CBodyOf (n), do_usagecheck_suspend, voidptr);

			inside_claim = saved_inside_claim;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  FORKING */
	case S_FORKING:
		{
			const int old_forking_scope = forking_scope;
			const int old_forking_lexlevel = forking_lexlevel;
			treenode *old_forking_node = forking_node;
			const int old_forking_count = forking_count;

			forking_node = n;
			forking_scope = NScopeOf (DNameOf (CBodyOf (n)));
			forking_lexlevel = NLexLevelOf (DNameOf (CBodyOf (n)));
			forking_count = 0;

			prewalkproctree (CBodyOf (n), do_usagecheck_suspend, voidptr);

			/* any FORKs inside are enclosed by this */
			#if 0
			if (curroutine) {
				SetNPForks (curroutine, 0);
			}
			#endif

			if (!forking_count) {
				usereport (TRUE, USE_EMPTY_FORKING, LocnOf (n), NULL);
			}

			forking_node = old_forking_node;
			forking_scope = old_forking_scope;
			forking_lexlevel = old_forking_lexlevel;
			forking_count = old_forking_count;
		}
		return STOP_WALK;
		/*}}}*/
		/*{{{  PINSTANCE (local, forked) */
	case S_PINSTANCE:
		if (!separatelycompiled (INameOf (n)) && IForkedOf (n)) {
			/*{{{  FORKed instance of something local*/
			/*
			 * any free variables in the FORKed process must be:
			 *   - explicitly #PRAGMA SHARED
			 *   - outside the scope of the enclosing FORKING
			 */
			treenode *pname = INameOf (n);
			varlist *freevars = NFreeVarsOf (pname);

			forking_count++;

			/* if curroutine, mark as forks */
			if (curroutine && !forking_node) {
				SetNPForks (curroutine, 1);
			}

			if ((forking_lexlevel < 0) && freevars) {
				/* if the FORKING is missing, disallow any free variables */
				usereport (FALSE, USE_FORK_FREE_VARS, LocnOf (n), VLNameOf (freevars));
			} else {
				while (freevars) {
					treenode *free_var = VLNameOf (freevars);

#if 0
fprintf (stderr, "use2: do_usagecheck_suspend(): free-var checking. free-var =");
printtreenl (stderr, 4, free_var);
fprintf (stderr, "use2: do_usagecheck_suspend(): free-var checking. IForkOf(n) =");
printtreenl (stderr, 4, IForkOf (n));
#endif
					if (IForkOf (n) && (free_var == DNameOf (CBodyOf (IForkOf (n))))) {
						/* skip -- $fork.barrier is the free var */
					} else if (!NVSharedOf (free_var)) {
						if (isdynmobilechantype (free_var)) {
							/* allow explicitly SHARED channel-ends to be free */
							if (!(NTypeAttrOf (NTypeOf (free_var)) & TypeAttr_shared)) {
								usereport (FALSE, USE_UNSHARED_FORK_FREE_MVAR, LocnOf (n), free_var);
							}
						} else {
							/* only if it's non-value.. */
							if ((TagOf (free_var) != N_VALPARAM) && (TagOf (free_var) != N_VALABBR)) {
								usereport (FALSE, USE_UNSHARED_FORK_FREE_VAR, LocnOf (n), free_var);
							}
						}
					}
					if ((NLexLevelOf (free_var) > forking_lexlevel) || (NScopeOf (free_var) > forking_scope)) {
						usereport (FALSE, USE_FORK_FREE_VAR_SCOPE, LocnOf (n), free_var);
					}
					freevars = VLNextOf (freevars);
				}
			}
#if 0
fprintf (stderr, "do_usagecheck: PINSTANCE (forked lex=%d, scope=%d).  freevars:", forking_lexlevel, forking_scope);
{ varlist *list; for (list = NFreeVarsOf (INameOf (n)); list; list = VLNextOf (list)) {
printtree (stderr, 4, VLNameOf (list));
fprintf (stderr, " lexlevel=%d, scope=%d", NLexLevelOf (VLNameOf (list)), NScopeOf (VLNameOf (list)));
} fprintf (stderr, "\n");}
#endif
			/*}}}*/
		} else if (IForkedOf (n)) {
			/*{{{  FORKing something separately compiled */
			if (curroutine && !forking_node) {
				SetNPForks (curroutine, 1);
			}
			forking_count++;
			/*}}}*/
		}
		
		/* check to see if this instance FORKs other stuff */
		if (NPForksOf (INameOf (n)) && curroutine) {
			if (!IForkOf (n) && forking_node) {
				SetIFork (n, forking_node);
				/* and up usagecount on FORKING name */
				SetNVUseCount (DNameOf (CBodyOf (forking_node)), NVUseCountOf (DNameOf (CBodyOf (forking_node))) + 1);
			} else if (!forking_node) {
				SetNPForks (curroutine, 1);
			}
			forking_count++;
		} else if (NPForksOf (INameOf (n))) {
			forking_count++;
		}

		/* check to see if it SUSPENDs too */
		if (IForkedOf (n) && NPSuspendsOf (INameOf (n))) {
			usereport (FALSE, USE_FORK_SUSPEND, LocnOf (n), NULL);
		}
		if (NPSuspendsOf (INameOf (n)) && curroutine) {
			SetNPSuspends (curroutine, 1);
		}

		/* check for unfixed parameters */
		prewalktree (n, do_usagecheck_fixedvars, (void *)0);
		break;
		/*}}}*/
		/*{{{  ABBR, DECL*/
	case S_ABBR:
	case S_DECL:
		{
			treenode *tmp = n;

#if 0
fprintf (stderr, "use2: do_usagecheck_suspend(): found declaration/abbreviation, DNameOf(tmp) = ");
printtreenl (stderr, 4, DNameOf(tmp));
#endif
			while ((TagOf (tmp) == S_ABBR) || (TagOf (tmp) == S_DECL)) {
				treenode *name = DNameOf (tmp);

				if (TagOf (name) == S_LIST) {
					for (; !EndOfList (name); name = NextItem (name)) {
						treenode *realname = ThisItem (name);

						if (ismobile (realname)) {
#if 0
fprintf (stderr, "use2: do_usagecheck_suspend(): found mobile declaration, realname = ");
printtreenl (stderr, 4, realname);
#endif
							SetNTypeAttr (realname, NTypeAttrOf (realname) | TypeAttr_fixed);
						}
					}
				} else if (ismobile (name)) {
#if 0
fprintf (stderr, "use2: do_usagecheck_suspend(): found mobile abbreviation, name = ");
printtreenl (stderr, 4, name);
#endif
					SetNTypeAttr (name, NTypeAttrOf (name) | TypeAttr_fixed);
				}
				tmp = DBodyOf (tmp);
			}

			/* walk the body (not a declaration) */
			prewalktree (tmp, do_usagecheck_suspend, voidptr);

			tmp = n;
			while ((TagOf (tmp) == S_ABBR) || (TagOf (tmp) == S_DECL)) {
				treenode *name = DNameOf (tmp);

				if (TagOf (name) == S_LIST) {
					for (; !EndOfList (name); name = NextItem (name)) {
						treenode *realname = ThisItem (name);

						if (ismobile (realname) && (NTypeAttrOf (realname) & TypeAttr_ufixed) && !(NTypeAttrOf (realname) & TypeAttr_fixed)) {
							if (TagOf (realname) == N_ABBR) {
								usereport (FALSE, USE_FIXED_ABBR_MOVED, LocnOf (tmp), realname);
							} else {
								usereport (FALSE, USE_FIXED_DECL_MOVED, LocnOf (tmp), realname);
							}
						}
					}
				} else if (ismobile (name)) {
#if 0
fprintf (stderr, "use2: do_usagecheck_suspend(): found mobile abbreviation [%s], name = ", (NTypeAttrOf (name) & TypeAttr_fixed) ? "fixed" : "not-fixed");
printtreenl (stderr, 4, name);
fprintf (stderr, "use2: do_usagecheck_suspend(): DValOf (tmp) = ");
printtreenl (stderr, 4, DValOf (tmp));
#endif
					if ((TagOf (tmp) == S_ABBR) && ismobile (DValOf (tmp)) && (nodetypeoftag (TagOf (DValOf (tmp))) == NAMENODE)) {
						/* simple RHS of abbreviation -- copy back FIXED state */
						SetNTypeAttr (DValOf (tmp), (NTypeAttrOf (DValOf (tmp)) & ~TypeAttr_fixed) | (NTypeAttrOf (name) & TypeAttr_fixed));
					}
					if ((NTypeAttrOf (name) & TypeAttr_ufixed) && !(NTypeAttrOf (name) & TypeAttr_fixed)) {
						/* user-specified fixed, but not actually fixed when checked */
						if (TagOf (name) == N_ABBR) {
							usereport (FALSE, USE_FIXED_ABBR_MOVED, LocnOf (tmp), name);
						} else {
							usereport (FALSE, USE_FIXED_DECL_MOVED, LocnOf (tmp), name);
						}

					}
				}
				tmp = DBodyOf (tmp);
			}
		}
		return STOP_WALK;
		/*}}}*/
	}
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATEPARAM int do_usagecheck (n) */
/*****************************************************************************
 *
 *  Perform usage check of variables and channels inside a routine.
 *  Expand replicators where possible and necessary.
 *
 *****************************************************************************/
PRIVATEPARAM int do_usagecheck (treenode * n, void *const voidptr)
{
	DEBUG_MSG (("do_usagecheck: %s\n", usetagstring (n)));
	switch (TagOf (n)) {
		/*{{{  PAR PRIPAR PLACEDPAR + repl versions, plus multiple assign */
	case S_REPLPAR:
	case S_PRIREPLPAR:
	case S_PLACEDREPLPAR:
	case S_PAR:
	case S_PRIPAR:
	case S_PLACEDPAR:
	case S_DO:
	case S_REPLDO:
	case S_ASS:		/* Added to allow usage check on multiple assignments (bug 278) */
		{
			varlist *const vars_used = use_freevarsin (n, -1, NULL);

			release_varlist (vars_used);
		}
		return STOP_WALK;
		/*}}} */
#ifdef MOBILES
		/*{{{  CLAIM */
	case S_CLAIM:
		{
			/* oh, nothing to do! (restriction is in use3) */
		}
		break;
		/*}}}*/
		/*{{{  SUSPEND*/
	case S_SUSPEND:
#if 0
fprintf (stderr, "use2: do_usagecheck: SUSPEND, curroutine = ");
printtreenl (stderr, 4, curroutine);
#endif
		if (curroutine) {
			SetNPSuspends (curroutine, 1);
		}
		break;
		/*}}}*/
#endif
		/*{{{  REPLSEQ REPLIF REPLALT PRIREPLALT */
	case S_REPLSEQ:
	case S_REPLIF:
	case S_REPLALT:
	case S_PRIREPLALT:
		/* N.B. no need to expand replicator if process doesn't go PAR */
		if (is_evaluable (ReplCStartExpOf (n)) && is_evaluable (ReplCLengthExpOf (n)) &&
				((ReplCStepExpOf (n) == NULL) ? TRUE : is_evaluable (ReplCStepExpOf (n))) && goes_par (ReplCBodyOf (n))) {
			/*{{{  base & count both known & body goes PAR; return TRUE */
			INT32 base = eval_const_treenode (ReplCStartExpOf (n));
			INT32 count = eval_const_treenode (ReplCLengthExpOf (n));
			INT32 step;
			treenode *name = (treenode *) ReplCNameOf (n);

			if (ReplCStepExpOf (n) != NULL) {
				step = eval_const_treenode (ReplCStepExpOf (n));
			} else {
				step = 1;
			}
			n = ReplCBodyOf (n);
			SetNReplKnown (name, TRUE);
			while (count > 0) {
				SetNReplValue (name, base);
				prewalkproctree (n, do_usagecheck, voidptr);
				base += step;
				count--;
			}
			SetNReplKnown (name, FALSE);
			return STOP_WALK;
			/*}}} */
		} else {
			/*{{{  otherwise ... */
			SetNReplKnown (ReplCNameOf (n), FALSE);
		}
		/*}}} */
		break;
		/*}}} */
		/*{{{  INPUTs */
	case S_INPUT:
	case S_X_INPUT:
	case S_TAGGED_INPUT:
	case S_X_TAGGED_INPUT:
	case S_CASE_INPUT:	/* bug 1183 19/3/91 */
	case S_X_CASE_INPUT:
		{
			varlist *const vars_used = use_freevarsin (n, -1, NULL);

			release_varlist (vars_used);
		}
		break;
		/*}}} */

	}
	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PRIVATE void fix_mobile_params (treenode *nptr, SOURCEPOSN locn)*/
/*
 *	goes through the formal-parameter list for the specified name
 *	and adds TypeAttr_fixed to all MOBILE parameters
 */
PRIVATE void fix_mobile_params (treenode *nptr, SOURCEPOSN locn)
{
	treenode *params;
	
	for (params = NParamListOf (nptr); !EndOfList (params); params = NextItem (params)) {
		treenode *param = ThisItem (params);

		switch (TagOf (param)) {
		case S_HIDDEN_PARAM:
		case S_PARAM_MSP:
		case S_PARAM_STATICLINK:
		case S_PARAM_MPP:
		case S_PARAM_VSP:
		case S_PARAM_FB:
		case S_PARAM_WS:
		case S_HIDDEN_TYPE:
			/* not relevant */
			break;
		case N_PARAM:
			if (ismobile (param)) {
#if 0
fprintf (stderr, "use2: fix_mobile_params(): found mobile parameter, param = ");
printtreenl (stderr, 4, param);
#endif
				SetNTypeAttr (param, NTypeAttrOf (param) | TypeAttr_fixed);
			}
			break;
		case N_RESULTPARAM:
		case N_VALPARAM:
			/* cannot be fixed */
			break;
		default:
			break;
		}
	}

	return;
}
/*}}}*/
/*{{{  PRIVATE void check_fixed_params (treenode *nptr, SOURCEPOSN locn)*/
/*
 *	goes through the formal-parameter list for the specified name
 *	and checks TypeAttr_fixed against TypeAttr_ufixed
 */
PRIVATE void check_fixed_params (treenode *nptr, SOURCEPOSN locn)
{
	treenode *params;

	for (params = NParamListOf (nptr); !EndOfList (params); params = NextItem (params)) {
		treenode *param = ThisItem (params);

		switch (TagOf (param)) {
		case S_HIDDEN_PARAM:
		case S_PARAM_MSP:
		case S_PARAM_STATICLINK:
		case S_PARAM_MPP:
		case S_PARAM_VSP:
		case S_PARAM_FB:
		case S_PARAM_WS:
		case S_HIDDEN_TYPE:
			/* not relevant */
			break;
		case N_PARAM:
			if (ismobile (param)) {
#if 0
fprintf (stderr, "use2: check_fixed_params(): found mobile parameter [%s], param = ", (NTypeAttrOf (param) & TypeAttr_fixed) ? "fixed" : "not-fixed");
printtreenl (stderr, 4, param);
#endif

			}
			if ((NTypeAttrOf (param) & TypeAttr_ufixed) && !(NTypeAttrOf (param) & TypeAttr_fixed)) {
				/* user-specified fixed, but not actually fixed when checked */
				usereport (FALSE, USE_FIXED_PARAM_MOVED, locn, param);
			}
			break;
		case N_RESULTPARAM:
		case N_VALPARAM:
			/* cannot be fixed */
			break;
		}
	}

	return;
}
/*}}}*/
/*{{{  PRIVATEPARAM int do_usagecheck_tree (n) */
/*****************************************************************************
 *
 * look for PROCs and FUNCTIONs.
 * call do_usagecheck on each.
 *
 *****************************************************************************/
PRIVATEPARAM int do_usagecheck_tree (treenode *n, void *const voidptr)
{
	switch (TagOf (n)) {
		/*{{{  proc defs */
	case S_PROCDEF:	/* N.B. Functions can't go PAR */
#ifdef MOBILES
	case S_MPROCDECL:
#endif
	CASE_CONFIG_SPEC
		if (!separatelycompiled (DNameOf (n))) {
			treenode *saved_curroutine = curroutine;

			syn_lexlevel++;	/* bug 1008 9/10/90 */
			DEBUG_MSG (("do_usagecheck_tree: incrementing syn_lexlevel\n"));
			prewalkproctree (DValOf (n), do_usagecheck_tree, voidptr);	/* do nested stuff */
			DEBUG_MSG (("do_usagecheck_tree: now checking %s\n", usetagstring (n)));
			curroutine = DNameOf (n);
			if ((TagOf (n) == S_MPROCDECL) && DExtraOf (n) && !check_sync_params (DExtraOf (n))) {
				usereport (FALSE, USE_MPP_NSYNC_PARAMS, LocnOf (n), NULL);
			}
			/*{{{  put FIXED attribute on all MOBILE formal params*/
			fix_mobile_params (DNameOf (n), LocnOf (n));
			/*}}}*/
			prewalkproctree (DValOf (n), do_usagecheck, voidptr);
			prewalkproctree (DValOf (n), do_usagecheck_suspend, voidptr);
			/*{{{  check any user-specified FIXED MOBILE params against compiler-checked FIXED */
			check_fixed_params (DNameOf (n), LocnOf (n));
			/*}}}*/
			DEBUG_MSG (("do_usagecheck_tree: decrementing syn_lexlevel\n"));
			syn_lexlevel--;
			curroutine = saved_curroutine;
		}
		/* if the routine SUSPENDs, mark formal params as `indirect' */
		if ((TagOf (n) == S_PROCDEF) && NParamListOf (DNameOf (n)) && NPSuspendsOf (DNameOf (n))) {
			treenode *fparams;

			for (fparams = NParamListOf (DNameOf (n)); fparams; fparams = NextItem (fparams)) {
				treenode *fparm = ThisItem (fparams);

				if (TagOf (fparm) == N_PARAM) {
					if (TagOf (NTypeOf (fparm)) == S_CHAN) {
						SetNVIndirect (fparm, TRUE);
					}
				}
				/* FIXME: other things here too probably */
			}
		}
		break;
		/*}}} */
		/*{{{  mobile proc type decl*/
	case S_PROCTYPEDECL:
		if (!check_sync_params (DNameOf (n))) {
			usereport (FALSE, USE_PROCTYPE_NSYNC_PARAMS, LocnOf (n), NULL);
		}
		break;
		/*}}}*/
		/*{{{  FORKING */
	case S_FORKING:
		{
			const int old_forking_scope = forking_scope;
			const int old_forking_lexlevel = forking_lexlevel;
			const int old_forking_count = forking_count;

			forking_scope = NScopeOf (DNameOf (CBodyOf (n)));
			forking_lexlevel = NLexLevelOf (DNameOf (CBodyOf (n)));

			prewalkproctree (CBodyOf (n), do_usagecheck_tree, voidptr);

			forking_scope = old_forking_scope;
			forking_lexlevel = old_forking_lexlevel;
			forking_count = old_forking_count;
		}
		return STOP_WALK;
		/*}}}*/
	}
	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PUBLIC void usagecheck (n) */
/*****************************************************************************
 *
 *  Walk tree looking for PROCs and FUNCTIONs,
 *  then usage check each one.
 *
 *****************************************************************************/
PUBLIC void usagecheck (treenode *n)
{
	prewalkproctree (n, do_usagecheck_tree, NULL);
}

/*}}}*/



