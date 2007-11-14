/* $Id: gen9.c,v 1.4 1997/11/21 17:38:59 mdp2 Exp $ */

/*
 *	code generator - case generation
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

/*#define DEBUG*/

/*  adapted for revised genstartjumptable which may optimise
 */

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include "includes.h"
#include "extlib.h"		/* IMPORTED */
#include "suplib.h"		/* IMPORTED */

#include "instruct.h"
#include "genhdr.h"
#include "generror.h"
#include "bind1def.h"
#include "bind2def.h"
#include "bind3def.h"
#include "gen1def.h"
#include "gen2def.h"
#include "gen4def.h"
#include "gen7def.h"
#include "gen9def.h"
#include "gen10def.h"
#include "gen11def.h"
#include "code1def.h"
/*}}}*/

/*{{{  constant and structure definitions*/
#define DELTA          3
#define MIN_JTAB_SIZE 13
#define DENSITY        3

typedef struct {
	INT32 lowvalue, highvalue;
	treenode *selectionexp;
	treenode *selectionp;
	int label;
	BOOL jumpdest;
} caseentry;
/*}}}*/

/*{{{  PRIVATE variables*/
PRIVATE caseentry *casetable;
PRIVATE int defaultlab, defaultjumpdest, selectortype;
PRIVATE int outerjoinlab;
PRIVATE treenode *selectorexp;
/*}}}*/

/*{{{  tjumptable forward declaration*/
PRIVATE void tjumptable (int high, int low, BOOL nomorejumptables);
/*}}}*/
/*{{{  PRIVATE int casesin (tptr)*/
/*****************************************************************************
 *
 *  casesin takes a list of selections 'tptr' and returns the number of
 *          distinct selections (including an ELSE if one present).
 *
 *****************************************************************************/
PRIVATE int casesin (treenode * tptr)
{
	int n = 0;
	for (; !EndOfList (tptr); tptr = NextItem (tptr)) {
		treenode *thisguard = CondGuardOf (skipspecifications (ThisItem (tptr)));
		if (TagOf (thisguard) == S_ELSE)
			n++;
		else
			n += listitems (thisguard);
	}
	return (n);
}

/*}}}*/
/*{{{  PRIVATE void tbinchop(low, high)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tbinchop generates a binary chop selection for the case values
 *           low to high.
 *           Only works for single-length values at the moment.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void tbinchop (const int low, const int high)
{
	const int midpoint = (high + low) / 2;
	const int thislabel = newlab ();
	caseentry *thisselection = &(casetable[midpoint]);
	treenode *snode;
	/* Jump if exp <= value */
	const int old = switch_to_temp_workspace ();
	snode = newdopnode (S_GR, NOPOSN, selectorexp, thisselection->selectionexp, selectortype);
	switch_to_prev_workspace (old);
	tguard (snode, TRUE, thislabel);	/*        selectorexp          */
	/*        ldc   selection      */
	/*        gt                   */
	/*        cj    thislabel      */
	/* At this point, we know exp > value */
	tjumptable (midpoint + 1, high, TRUE);	/*        ...  higher cases    */
	setlab (thislabel);	/* thislabel:                  */
	tjumptable (low, midpoint, TRUE);	/*        ...  lower cases     */
}

/*}}}*/
/*{{{  PRIVATE void tjumptable(low, high, nomorejumptables)*/
/*****************************************************************************
 *
 *  tjumptable generates the code for selecting cases given a sorted array
 *             of case values and labels.  Our case values are
 *             case[low .. high].
 *
 *****************************************************************************/
PRIVATE void tjumptable (const int low, const int high, const BOOL nomorejumptables)
{
	const int numcases = (high - low) + 1;
	const BOOL singlelength = fitsinword (selectortype);
#ifdef DEBUG
	{
		int i;

		DEBUG_MSG (("tjumptable: low: %d, high: %d, numcases: %d, nomorejumptables: %d\n", low, high, numcases, nomorejumptables));
		for (i = low; i <= high; i++) {
			DEBUG_MSG (("tjumptable: casetable[%2d].lowvalue = %8X\n", i, casetable[i].lowvalue));
		}
	}
#endif
	if ((numcases <= DELTA) || isquadlength (selectortype)) {
		/* bug INSdi02177 */
		/*{{{  too few cases - output jumps */
		int i;
		BOOL optimisedlast = FALSE;
		for (i = low; i < low + numcases; i++) {
			/*{{{  output a jump for casetable[i] */
			caseentry *thiscase = &(casetable[i]);
			const int old = switch_to_temp_workspace ();
			treenode *snode = newdopnode (S_EQ, NOPOSN, selectorexp, thiscase->selectionexp, selectortype);
			switch_to_prev_workspace (old);

			if ((i == high) && (thiscase->lowvalue != 0)
			    && (thiscase->lowvalue != MOSTNEG_INT32)
			    && fitsinword (selectortype)
			    && (!isinconstanttable (thiscase->selectionexp))) {
				/*{{{  optimise the last case */
				tguard (snode, TRUE, defaultlab);
				genbranch (I_J, thiscase->label);
				thiscase->jumpdest = TRUE;
				optimisedlast = TRUE;
				/*}}} */
			} else {
				tguard (snode, FALSE, thiscase->label);
			}
			/*}}} */
		}
		if (!optimisedlast) {
			genbranch (I_J, defaultlab);
			defaultjumpdest = TRUE;
		}
		/*}}} */
	} else {
		/*{{{  look for biggest jump table */
		int bestlow = 0;
		int besthigh = 0;
		int bestsize = 0;
		if (!nomorejumptables && (!singlelength || (numcases >= MIN_JTAB_SIZE)))
		{
			/*{{{  look for best jump table */
			int i, j;
			const int minjumps = singlelength ? MIN_JTAB_SIZE - 1 : DELTA;

			for (i = low; i < (low + numcases - DELTA); i++) {
				/*{{{  set j to minimum value possible for better subrange */
				j = i + ((bestsize > minjumps) ? bestsize : minjumps);
				/*}}} */
				/*{{{  test all possible subranges starting at i, ending at or past j */
				{
					const int maxaccept = ((high - i) + 1) * DENSITY;
					if (singlelength) {
						/*{{{  single length */
						while (j <= high) {
							/* bug TS/1589 - 04/11/92 this is a bug in gcc 1.36;
							   changing diff to be a BIT32 seems to fix the problem;
							   and is OK as a permanent work-around.
							 */
							/* bug TS/1575 05/11/92 - cast to BIT32 for unsigned arith */
							/*const INT32 diff = casetable[j].lowvalue - casetable[i].lowvalue); */
							/*const INT32 diff = (INT32)((BIT32)casetable[j].lowvalue - (BIT32)casetable[i].lowvalue); */
							const BIT32 diff = (BIT32) casetable[j].lowvalue - (BIT32) casetable[i].lowvalue;
							/*DEBUG_MSG(("i: %2d, j: %2d, lowvalue: %8X, diff: %8X\n",
							   i, j, casetable[j].lowvalue, diff)); */
/*if ((diff < 0) || (diff > maxaccept)) *//* TS/1589 04/11/92 */
							if (diff > (BIT32) maxaccept) {
								DEBUG_MSG (("tjumptable: bombing: diff: %8X\n", diff));
								j = high;	/* no chance so exit */
							} else {
								const int size = (j - i) + 1;
								DEBUG_MSG (("tjumptable: ok: diff: %8X\n", diff));
								if (diff <= (int) (DENSITY * size)) {	/* better subrange   */
									bestlow = i;
									besthigh = j;
									bestsize = size;
								}
							}
							j = j + 1;
						}
						/*}}} */
					} else {
						/*{{{  double length */
						while (j <= high) {
							BIT32 hdiff, ldiff;
							Int64Minus (&hdiff, &ldiff,
								    casetable[j].highvalue, casetable[j].lowvalue,
								    casetable[i].highvalue, casetable[i].lowvalue);
							if ((hdiff != 0) || (ldiff > maxaccept)) {
								j = high;	/* no chance so exit */
							} else {
								const int size = (j - i) + 1;
								if (ldiff <= (int) (DENSITY * size)) {	/* better subrange   */
									bestlow = i;
									besthigh = j;
									bestsize = size;
								}
							}
							j = j + 1;
						}
						/*}}} */
					}
				}
				/*}}} */
			}
			/*}}} */
		}
		if (bestsize == 0) {
			tbinchop (low, high);
		} else {
			/*{{{  do jump table */
			int lesslab = 0, greaterlab = 0;
			caseentry *lowestcase = &(casetable[bestlow]);
			caseentry *highestcase = &(casetable[besthigh]);

			DEBUG_MSG (("generating jump table: bestsize = %d\n", bestsize));
			/*{{{  test for below boundary */
			{
				treenode *lselnexp = lowestcase->selectionexp;
				if (!is_typed_mostneg (selectortype, LoValOf (lselnexp), HiValOf (lselnexp))) {
					int l;
					const int old = switch_to_temp_workspace ();
					treenode *snode = newdopnode (S_GE, NOPOSN, selectorexp, lselnexp, selectortype);
					switch_to_prev_workspace (old);

					if (bestlow == low)
						l = defaultlab;
					else {
						lesslab = newlab ();
						l = lesslab;
					}
					tguard (snode, TRUE, l);
				}
			}
			/*}}} */
			/*{{{  test for above boundary */
			{
				treenode *hselnexp = highestcase->selectionexp;
				if (!is_typed_mostpos (selectortype, LoValOf (hselnexp), HiValOf (hselnexp))) {
					int l;
					const int old = switch_to_temp_workspace ();
					treenode *snode = newdopnode (S_LE, NOPOSN, selectorexp, hselnexp, selectortype);
					switch_to_prev_workspace (old);

					if (besthigh == high)
						l = defaultlab;
					else {
						greaterlab = newlab ();
						l = greaterlab;
					}

					tguard (snode, TRUE, l);
				}
			}
			/*}}} */
			/*{{{  output jump table */
			{
#if 0
				const int jlab = newlab ();
				const int l = newlab ();
#endif
				treenode *snode;

				if (lowestcase->lowvalue != 0) {
					/*{{{  selectorexp; ldc lowvalue; diff */
					const int old = switch_to_temp_workspace ();
					snode = newdopnode (S_MINUS, NOPOSN,	/*           selectorexp           */
							    selectorexp,	/*           ldc   selection       */
							    lowestcase->selectionexp,	/*           diff                  */
							    S_INT);
					switch_to_prev_workspace (old);
					/*}}} */
				} else {
					/*{{{  selectorexp */
					snode = selectorexp;	/*           selectorexp           */
					/*}}} */
				}
				texp (snode, MANY_REGS);
#if 0
				/* subsumed into genstartjumptable to allow optimisation */
				genloadcasescale ();	/*           ldc   casescale       */
				/*{{{  T9000_gamma_badmul */
				if (T9000_gamma_badmul (&tx_global)) {
					gensecondary (I_NOP);
					gencomment0 ("Workaround a slow multiplier");
				}
				/*}}} */
				gensecondary (I_PROD);	/*           prod                  */
				genlabeldiff (I_LDC, jlab, l);	/*           ldc   jlab - l        */
				gensecondary (I_LDPI);	/*           ldpi                  */
				setlab (l);	/* l:                              */
				gensecondary (I_BSUB);	/*           bsub                  */
				gensecondary (I_GCALL);	/*           gcall                 */
				setlab (jlab);	/* jlab:                           */
#endif
				genstartjumptable ();	/* includes jump into table */
				genjumptableentry (I_J, lowestcase->label);
				lowestcase->jumpdest = TRUE;
				/*           j     lowestcase      */
				/*{{{  other cases                                     j     othercases .. */
				{
					int i;
					for (i = bestlow + 1; i <= besthigh; i++) {
						int j;
						/* bug TS/1575 05/11/92 - cast to BIT32 for unsigned arith */
						const int blanks = (int) ((BIT32) casetable[i].lowvalue - (BIT32) casetable[i - 1].lowvalue) - 1;

						for (j = 0; j < blanks; j++) {
							genjumptableentry (I_J, defaultlab);
							defaultjumpdest = TRUE;
						}
						genjumptableentry (I_J, casetable[i].label);
						casetable[i].jumpdest = TRUE;
					}
				}
				/*}}} */
				genendjumptable ();
				/*}}} */
			}
			if (bestlow > low) {
				/*{{{  case on lower values */
				setlab (lesslab);
				tjumptable (low, bestlow - 1, FALSE);
				/*}}} */
			}
			if (besthigh < high) {
				/*{{{  case on higher values */
				setlab (greaterlab);
				tjumptable (besthigh + 1, high, FALSE);
				/*}}} */
			}
			/*}}} */
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PRIVATEPARAM int doublecomparecases (c1, c2)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  doublecomparecases takes two caseentry structure pointers, 'c1' and 'c2'
 *                     and returns
 *                       1 if c1 selection > c2 selection
 *                       0 if c1 selection = c2 selection
 *                      -1 if c1 selection < c2 selection
 *
 *****************************************************************************/
/*}}}*/
PRIVATEPARAM int doublecomparecases (const void *const c1_void, const void *const c2_void)
{
	const caseentry *const c1 = c1_void;
	const caseentry *const c2 = c2_void;
	BOOL greater;
	Int64Gt (&greater, c1->highvalue, c1->lowvalue, c2->highvalue, c2->lowvalue);
	if (greater)
		return (1);
	else {
		BOOL equal;
		Int64Eq (&equal, c1->highvalue, c1->lowvalue, c2->highvalue, c2->lowvalue);
		return equal ? 0 : (-1);
	}
}

/*}}}*/
/*{{{  PRIVATEPARAM int comparecases (c1, c2)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  comparecases takes two caseentry structure pointers, 'c1' and 'c2'
 *               and returns
 *                1 if c1 selection > c2 selection
 *                0 if c1 selection = c2 selection
 *               -1 if c1 selection < c2 selection
 *               The comparison is upon the low 32-bits only.
 *
 *****************************************************************************/
/*}}}*/
PRIVATEPARAM int comparecases (const void *const c1_void, const void *const c2_void)
{
	const caseentry *const c1 = c1_void;
	const caseentry *const c2 = c2_void;
	if ((INT32) (c1->lowvalue) > (INT32) (c2->lowvalue))
		return (1);
	else if ((INT32) (c1->lowvalue) == (INT32) (c2->lowvalue))
		return (0);
	else
		return (-1);
}

/*}}}*/
/*{{{  PRIVATE void tselection (tptr, joinlab)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tselection generates code for the selection tree 'tptr'.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void tselection (treenode * tptr, const int joinlab)
{
	treenode *specsptr = tptr;

	tptr = tspecs (tptr);
	if (TagOf (tptr) != S_SELECTION)
		badtag (genlocn, TagOf (tptr), "tselection");
	tprocess (CondBodyOf (tptr));
	tdespecs (specsptr);
	genbranch (I_J, joinlab);
}

/*}}}*/
/*{{{  PUBLIC void mapcase (tptr)*/
/*****************************************************************************
 *
 *  mapcase maps the case tree, tptr.
 *
 *****************************************************************************/
PUBLIC void mapcase (treenode * tptr)
{
	treenode *selector_exp;
	treenode *selectionlist = RHSOf (tptr);
	mapexp (LHSAddr (tptr));
	selector_exp = LHSOf (tptr);
	if (!issimplelocal (selector_exp, be_lexlevel)) {
		selector_exp = gettemp (selector_exp, NM_WORKSPACE);
		SetLHS (tptr, selector_exp);
	}
	if (nodetypeoftag (TagOf (selector_exp)) == NAMENODE)	/* bug INSdi01959 06/04/93 */
		upusecount (selector_exp, 4);
	mapconstruction (selectionlist, mapprocess);
}

/*}}}*/
/*{{{  PUBLIC void tcase (tptr)*/
/*****************************************************************************
 *
 *  tcase generates code for a case process tptr
 *
 *****************************************************************************/
PUBLIC void tcase (treenode * tptr)
{
	/*{{{  save globals */
	caseentry *const oldcasetable = casetable;
	const int olddefaultlab = defaultlab;
	const int olddefaultjumpdest = defaultjumpdest;
	const int oldselectortype = selectortype;
	treenode *const oldselectorexp = selectorexp;
	/*}}} */
	BOOL makedefault = TRUE;
	treenode *selectionlist = RHSOf (tptr);
	const int joinlab = newlab ();
	int maxcase = 0;
	treenode *defaultp = NULL;
	/*{{{  initialise globals */
	defaultlab = NO_LABEL;
	defaultjumpdest = FALSE;
	selectorexp = LHSOf (tptr);
	selectortype = ntypeof (selectorexp);
	/* make sure that we don't call memalloc with zero bytes */
	casetable = (caseentry *) memalloc (sizeof (caseentry) * casesin (selectionlist) + 1);
	/*}}} */
	/*{{{  evaluate the selector, if neccessary */
	tpreexp (selectorexp);
	simplify (P_EXP, selectorexp);
	/*}}} */
	/*{{{  build up table of selections, and generate jumps into case body */
	{
		treenode *slist;
		for (slist = selectionlist; !EndOfList (slist); slist = NextItem (slist)) {
			treenode *thisselection = skipspecifications (ThisItem (slist));
			treenode *v = CondGuardOf (thisselection);
			const int l = newlab ();
			if (TagOf (v) == S_ELSE) {
				defaultlab = l;
				defaultp = ThisItem (slist);
				makedefault = FALSE;
			} else
				/*{{{  set up labels and values */
				for (; !EndOfList (v); v = NextItem (v)) {
					treenode *thisconstant = ThisItem (v);
					caseentry *caseentryptr = &(casetable[maxcase]);
					caseentryptr->label = l;
					caseentryptr->jumpdest = FALSE;
					caseentryptr->lowvalue = LoValOf (thisconstant);
					caseentryptr->highvalue = HiValOf (thisconstant);
					caseentryptr->selectionexp = thisconstant;
					caseentryptr->selectionp = ThisItem (slist);
					maxcase++;
				}
			/*}}} */
		}
		if (makedefault)
			defaultlab = newlab ();
#if 0
		sup_qsort (casetable, maxcase, sizeof (caseentry), fitsinword (selectortype) ? comparecases : doublecomparecases);
#else
		sup_qsort (casetable, maxcase, sizeof (caseentry), (bytesinscalar (selectortype) <= 4) ? comparecases : doublecomparecases);
#endif
		tjumptable (0, maxcase - 1, FALSE);
	}
	/*}}} */
	/*{{{  generate case body */
	{
		int casesdone = 0;
		/*{{{  generate default process */
		setlab (defaultlab);
		if (makedefault) {
			if (NEED_ERRORS)	/* bug TS/2071 29/01/93 */
				tstop (tptr);
		} else {
			if (defaultjumpdest)
				genstartblock ();
			tselection (defaultp, joinlab);
		}
		/*}}} */
		while (casesdone < maxcase) {
			treenode *thisprocess;
			int i;
			BOOL jdest;
			/* Find a process to generate */
			for (i = 0; (i < maxcase) && (casetable[i].selectionp == NULL); i++)
				/* skip */ ;
			thisprocess = casetable[i].selectionp;
			/* Generate thisprocess */
			setlab (casetable[i].label);
			jdest = casetable[i].jumpdest;
			/*{{{  find and delete other entries for this process */
			{
				int j;
				for (j = i + 1; j < maxcase; j++)
					if (casetable[j].selectionp == thisprocess) {
						/* Delete the process, so we only generate it once */
						jdest = jdest | casetable[j].jumpdest;
						casetable[j].selectionp = NULL;
						casesdone++;
					}
			}
			/*}}} */
			if (jdest)
				genstartblock ();
			tselection (thisprocess, joinlab);
			casetable[i].selectionp = NULL;
			casesdone++;
		}
		setlab (joinlab);
		genstartblock ();
	}
	/*}}} */
	/*{{{  restore globals */
	memfree (casetable);
	casetable = oldcasetable;
	defaultlab = olddefaultlab;
	defaultjumpdest = olddefaultjumpdest;
	selectortype = oldselectortype;
	selectorexp = oldselectorexp;
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void caseinit()*/
PUBLIC void caseinit (void)
{
	casetable = NULL;
	defaultlab = NO_LABEL;
	defaultjumpdest = FALSE;
	outerjoinlab = NO_LABEL;
	selectortype = -1;
	selectorexp = NULL;
}

/*}}}*/
