/* $Id: gen7.c,v 1.7 1997/12/04 14:58:50 mdp2 Exp $ */

/*
 *	code generator - boolean expression generation - single & double
 *		length calls to generate EQC,0 changed to genboolinvert
 *		wherever possible.
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


/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include "includes.h"
#include "generror.h"
#include "instruct.h"
#include "genhdr.h"
#include "gen1def.h"
#include "gen2def.h"
#include "gen4def.h"
#include "gen5def.h"
#include "gen7def.h"
#include "gen11def.h"
#include "gen12def.h"
#include "bind3def.h"
#include "code1def.h"
/*}}}*/

/*{{{  PRIVATE void trelop(op, type, left, right, sense, genbool, regs)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  trelop generates code for relational operator 'op' into Areg, using at
 *         most 'regs' registers.
 *         If 'genbool' is TRUE, a strictly Boolean result is left in Areg.
 *         If 'sense' is FALSE, the result is inverted.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void
trelop (const int op, int type, treenode * left, treenode * right, const BOOL sense, const BOOL genbool, const int regs)
{
	if (has_fpu_core && isreal (type))
		/*{{{  do inline floating comparison */
	{
		treenode *comparison;
		const int old = switch_to_temp_workspace ();
		comparison = newdopnode (op, NOPOSN, left, right, type);
		switch_to_prev_workspace (old);
		tfpexp (comparison, regs, MAXFPUREGS);
		if (!sense)
			genboolinvert ();
	}
	/*}}} */
	else if (fitsinword (type))
		switch (op) {
			/*{{{  S_EQ */
		case S_EQ:
			if (isconst (right))
				/*{{{  swap left and right */
			{
				treenode *temp = left;
				left = right;
				right = temp;
			}
			/*}}} */

			if (type == S_BOOL)
				/*{{{  BOOL */
			{
				if (isconstexpnd (left))
					/*{{{  right; (eqc 0) */
				{
					texp (right, regs);
					if (((LoValOf (left) == 0) && sense) || ((LoValOf (left) == 1) && (!sense)))
						genboolinvert ();
				}
				/*}}} */
				else
					/*{{{  left; right; xor; (eqc 0) */
				{
					tdop (S_XOR, type, left, right, regs, TRUE, FALSE);
					if (sense)
						genboolinvert ();
				}
				/*}}} */
			}
			/*}}} */
			else if (istargetintsize (type) || (isshorttype (type)))
				/*{{{  short or INT */
			{
				/*{{{  comment on the following guard */
				/* The genbool || sense || (LoValOf(left) == 0) guard is used because
				   this branch would generate
				   exp; eqc n; eqc 0
				   under the condition (!genbool && !sense && (LoValOf(left) != 0))
				   whereas
				   exp; ldc n; diff
				   is two cycles quicker.
				 */
				/*}}} */
				if (isconstexpnd (left) && !isinconstanttable (left) && (genbool || sense || LoValOf (left) == 0))
					/* constant, but not in a table */
					/*{{{  right; eqc left */
				{
					texp (right, regs);
					if (!genbool && !sense && (LoValOf (left) == 0))
						/* we don't need to generate anything */
						;
					else {
						genprimary (I_EQC, LoValOf (left));
						if (!sense)
							genboolinvert ();
					}
				}
				/*}}} */
				else
					/*{{{  left; right; diff */
				{
					/* bug 1345 - we pass in `targetintsize' rather than 'type'
					   so that it doesn't include the sign extension 24/7/91 */
					if (isshorttype (type))
						type = targetintsize;
					tdop (S_MINUS, type, left, right, regs, TRUE, TRUE);
					/* must have at least one EQC to convert expr to Boolean */
					if (!sense)
						/*{{{  we already have correct sense - do we convert to Boolean? */
					{
						if (genbool) {	/* Result must be a genuine Boolean */
							genprimary (I_EQC, 0);
							genboolinvert ();
						}
					}
					/*}}} */
					else
						genprimary (I_EQC, 0);
				}
				/*}}} */
			}
			/*}}} */
			else
				geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "trelop");
			break;
			/*}}} */
			/*{{{  S_GR */
		case S_GR:
			if (istargetintsize (type) || isshorttype (type))
				/*{{{  short or INT */
			{
				tdop (S_GR, type, left, right, regs, FALSE, TRUE);
				if (!sense)
					genboolinvert ();
			}
			/*}}} */
			else
				geninternal_is (GEN_ERROR_IN_ROUTINE, 3, "trelop");
			break;
			/*}}} */
		default:
			geninternal_is (GEN_ERROR_IN_ROUTINE, 2, "trelop");
	} else
		trellop (op, type, left, right, sense, genbool);
/*checkerror(); *//* INSdi02485 */
}

/*}}}*/
/*{{{  PRIVATE void jumpcond(tptr, truelab, falselab, sense, genbool, regs)*/
/*{{{  comment*/
/* Evaluate expression tptr.
Go to truelab/falselab
if you hit the result early, but make sure that the result
left in Areg is inverted if sense is FALSE.
If genbool is TRUE make sure any result left in Areg is a genuine Boolean
value.
If regs == MANY_REGS then the Boolean expressions may contain temporaries,
otherwise use at most 'regs' registers in evaluating the expressions.
*/

/* Modified 24/7/91 to only create the target labels if they are actually
   required.
*/

/*}}}*/
PRIVATE void
jumpcond (treenode * const tptr, int *const truelab, int *const falselab, const BOOL sense, const BOOL genbool, const int regs)
{
	switch (TagOf (tptr)) {
		/*{{{  dyadic nodes */
		/*{{{  S_AND */
	case S_AND:
		if (sense) {
			int il = NO_LABEL;
			if (*falselab == NO_LABEL)
				*falselab = newlab ();
			jumpcond (LeftOpOf (tptr), &il, falselab, TRUE, FALSE, regs);
			gencontrolsplit (*falselab);
			if (il != NO_LABEL)
				setlab (il);
			tpreexp (RightOpOf (tptr));
			jumpcond (RightOpOf (tptr), truelab, falselab, TRUE, genbool, regs);
		} else {
			int il = NO_LABEL;
			int l = newlab ();
			jumpcond (LeftOpOf (tptr), &il, &l, TRUE, FALSE, regs);
			gencontrolsplit (l);
			if (il != NO_LABEL)
				setlab (il);
			tpreexp (RightOpOf (tptr));
			jumpcond (RightOpOf (tptr), &l, &l, TRUE, FALSE, regs);
			/*{{{  T9000_gamma_badeqc0 */
			if (T9000_gamma_badeqc0 (&tx_global)) {
				gensecondary (I_NOP);
				gencomment0 ("Incase of two eqc0's ");
			}
			/*}}} */
			gencontroljoin (l);
			genboolinvert ();
		}
		break;
		/*}}} */
		/*{{{  S_OR */
	case S_OR:
		if (sense) {
			int l = newlab ();
			int il = NO_LABEL;
			jumpcond (LeftOpOf (tptr), &l, &il, FALSE, FALSE, regs);
			gencontrolsplit (l);
			if (il != NO_LABEL)
				setlab (il);
			tpreexp (RightOpOf (tptr));	/* bug TS/1522 11/12/91 */
			jumpcond (RightOpOf (tptr), &l, &l, FALSE, FALSE, regs);
			/*{{{  T9000_gamma_badeqc0 */
			if (T9000_gamma_badeqc0 (&tx_global)) {
				gensecondary (I_NOP);
				gencomment0 ("Incase of two eqc0's ");
			}
			/*}}} */
			gencontroljoin (l);
			genboolinvert ();
		} else {
			int il = NO_LABEL;
			if (*truelab == NO_LABEL)
				*truelab = newlab ();
			jumpcond (LeftOpOf (tptr), truelab, &il, FALSE, FALSE, regs);
			gencontrolsplit (*truelab);
			if (il != NO_LABEL)
				setlab (il);
			tpreexp (RightOpOf (tptr));	/* bug TS/1522 11/12/91 */
			jumpcond (RightOpOf (tptr), truelab, falselab, FALSE, genbool, regs);
		}
		break;
		/*}}} */
		/*{{{  S_EQ */
	case S_EQ:
		trelop (S_EQ, DOpTypeOf (tptr), LeftOpOf (tptr), RightOpOf (tptr), sense, genbool, regs);
		break;
		/*}}} */
		/*{{{  S_NE */
	case S_NE:
		trelop (S_EQ, DOpTypeOf (tptr), LeftOpOf (tptr), RightOpOf (tptr), !sense, genbool, regs);
		break;
		/*}}} */
		/*{{{  S_GR */
	case S_GR:
		trelop (S_GR, DOpTypeOf (tptr), LeftOpOf (tptr), RightOpOf (tptr), sense, genbool, regs);
		break;
		/*}}} */
		/*{{{  S_LS */
	case S_LS:
		trelop (S_GR, DOpTypeOf (tptr), RightOpOf (tptr), LeftOpOf (tptr), sense, genbool, regs);
		break;
		/*}}} */
		/*{{{  S_GE */
	case S_GE:
		trelop (S_GR, DOpTypeOf (tptr), RightOpOf (tptr), LeftOpOf (tptr), !sense, genbool, regs);
		break;
		/*}}} */
		/*{{{  S_LE */
	case S_LE:
		trelop (S_GR, DOpTypeOf (tptr), LeftOpOf (tptr), RightOpOf (tptr), !sense, genbool, regs);
		break;
		/*}}} */
		/*{{{  S_AFTER */
#if 0 /* No longer required, now done in trans */	/* bug TS/1568 16/01/92 */
	case S_AFTER:
		/* Convert { left AFTER right } to { (left MINUS right) > 0 } */
		SetTag (tptr, S_MINUS);
		trelop (S_GR, DOpTypeOf (tptr), tptr, newconstant (0), sense, genbool, regs);
		break;
#endif
		/*}}} */
		/*}}} */
		/*{{{  monadic nodes */
		/*{{{  S_NOT */
	case S_NOT:
		jumpcond (OpOf (tptr), falselab, truelab, !sense, genbool, regs);
		break;
		/*}}} */
		/*}}} */
		/*{{{  default - load expression */
	default:		/* load expression */
		texp (tptr, regs);
		checkerror ();
		if (!sense)
			genboolinvert ();
		break;
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void tbool(tptr, regs)*/
/*****************************************************************************
 *
 *  tbool generates Boolean expression tptr into Areg using at most 'regs'
 *        registers.
 *
 *****************************************************************************/
PUBLIC void
tbool (treenode * const tptr, const int regs)
{
	int l = NO_LABEL;	/* This will be initialised if actually used */
	jumpcond (tptr, &l, &l, TRUE, TRUE, regs);
	if (l != NO_LABEL)
		gencontroljoin (l);
}

/*}}}*/
/*{{{  PUBLIC void tguard(tptr, fallthroughsense, otherlabel)*/
/*****************************************************************************
 *
 *  tguard generates code for the conditional expression tptr such that if
 *         the expression evaluates to the same sense (TRUE/FALSE) as
 *         'fallthroughsense', control passes to the first instruction
 *         following tguard, otherwise control passes to the label
 *         'otherlabel'.
 *         This routine differs from jumpcond as the expression result is
 *         not preserved, and thus greater shortcuts can be taken.
 *
 *****************************************************************************/
PUBLIC void
tguard (treenode * const tptr, const BOOL fallthroughsense, const int otherlabel)
{
	switch (TagOf (tptr)) {
		/*{{{  cases */
		/*{{{  case S_OR */
	case S_OR:
		{
			int lab;
			if (fallthroughsense)
				lab = newlab ();
			else
				lab = otherlabel;
			tguard (LeftOpOf (tptr), FALSE, lab);
			tpreexp (RightOpOf (tptr));
			tguard (RightOpOf (tptr), fallthroughsense, otherlabel);
			if (fallthroughsense)
				setlab (lab);
		}
		break;
		/*}}} */
		/*{{{  case S_AND */
	case S_AND:
		{
			int lab;
			if (fallthroughsense)
				lab = otherlabel;
			else
				lab = newlab ();
			tguard (LeftOpOf (tptr), TRUE, lab);
			tpreexp (RightOpOf (tptr));
			tguard (RightOpOf (tptr), fallthroughsense, otherlabel);
			if (!fallthroughsense)
				setlab (lab);
		}
		break;
		/*}}} */
		/*{{{  case S_EQ */
	case S_EQ:
		trelop (S_EQ, DOpTypeOf (tptr), LeftOpOf (tptr), RightOpOf (tptr), fallthroughsense, FALSE, MANY_REGS);
		genbranch (I_CJ, otherlabel);
		break;
		/*}}} */
		/*{{{  case S_NE */
	case S_NE:
		trelop (S_EQ, DOpTypeOf (tptr), LeftOpOf (tptr), RightOpOf (tptr), !fallthroughsense, FALSE, MANY_REGS);
		genbranch (I_CJ, otherlabel);
		break;
		/*}}} */
		/*{{{  case S_GR */
	case S_GR:
		trelop (S_GR, DOpTypeOf (tptr), LeftOpOf (tptr), RightOpOf (tptr), fallthroughsense, FALSE, MANY_REGS);
		genbranch (I_CJ, otherlabel);
		break;
		/*}}} */
		/*{{{  case S_LS */
	case S_LS:
		trelop (S_GR, DOpTypeOf (tptr), RightOpOf (tptr), LeftOpOf (tptr), fallthroughsense, FALSE, MANY_REGS);
		genbranch (I_CJ, otherlabel);
		break;
		/*}}} */
		/*{{{  case S_GE */
	case S_GE:
		trelop (S_GR, DOpTypeOf (tptr), RightOpOf (tptr), LeftOpOf (tptr), !fallthroughsense, FALSE, MANY_REGS);
		genbranch (I_CJ, otherlabel);
		break;
		/*}}} */
		/*{{{  case S_LE */
	case S_LE:
		trelop (S_GR, DOpTypeOf (tptr), LeftOpOf (tptr), RightOpOf (tptr), !fallthroughsense, FALSE, MANY_REGS);
		genbranch (I_CJ, otherlabel);
		break;
		/*}}} */
		/*{{{  case S_NOT */
	case S_NOT:
		tguard (OpOf (tptr), !fallthroughsense, otherlabel);
		break;
		/*}}} */
	default:
		texp (tptr, MANY_REGS);
		checkerror ();
		/*{{{  T9000_gamma_badeqc0 */
		if (!isaddressable (tptr) && (TagOf (tptr) != S_FINSTANCE) && T9000_gamma_badeqc0 (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Incase of two eqc0's ");
		}
		/*}}} */
		if (!fallthroughsense)
			genboolinvert ();
		genbranch (I_CJ, otherlabel);
		break;
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void genboolassertion*/
PUBLIC void
genboolassertion (treenode * tptr, BOOL correctsense, treenode * const error_tptr, const int mode)
{
	while (TagOf (tptr) == S_NOT) {
		correctsense = !correctsense;
		tptr = OpOf (tptr);
	}
#if 0
/* this is the old transputer version, usually worse as octran finds CCNT1
   and CSUB0 more difficult to emulate */
	texp (tptr, MANY_REGS);

	if (error_tptr != NULL)
		new_occam_line (error_tptr, TRUE, TRUE, FALSE);
	else
		new_occam_line (tptr, TRUE, FALSE, FALSE);
	/* if ((error_tptr != NULL) && debugoutput)
	   coder_genlocate(error_tptr, TRUE); */

	/* if correctsense is TRUE,  fail if boolean value is not TRUE */
	/* if correctsense is FALSE, fail if boolean value is not FALSE */
	genprimary (I_LDC, 1);
	gensecondary (correctsense ? I_CCNT1 : I_CSUB0);
#else
	if (error_tptr != NULL) {
		new_occam_line (error_tptr, TRUE, TRUE, FALSE);
	} else {
		new_occam_line (tptr, TRUE, FALSE, FALSE);
	}
	{
		int continue_label = newlab ();

		tguard (tptr, !correctsense, continue_label);
		gensecondary (I_SETERR);
		setlab (continue_label);
	}
#endif
	checkerror_controlled (mode);
	throw_the_result_away ();
}

/*}}}*/
