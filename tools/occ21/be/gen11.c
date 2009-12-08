/* $Id: gen11.c,v 1.8 1997/10/02 14:14:28 mdp2 Exp $ */

/*
 *	code generator - operand loading and storing
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

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "includes.h"
#include "generror.h"
#include "instruct.h"
#include "genhdr.h"
#include "trandef.h"
#include "bind1def.h"
#include "bind2def.h"
#include "bind3def.h"
#include "gen1def.h"
#include "gen2def.h"
#include "gen4def.h"
#include "gen5def.h"
#include "gen8def.h"
#include "gen10def.h"		/* for chan-type operations I/O */
#include "gen11def.h"
#include "gen12def.h"
#include "code1def.h"
#include "genkroc.h"
#include "mobile_types.h"
/*}}}*/

/* fred debugging stuff */
#define EXAMINE_ASSIGN 0

/*{{{  constants*/
/* These are done as bit patterns so that multiple cases can be tested
   efficiently - CON 24/7/91
*/
#define MOVEDIRN_LOAD    1
#define MOVEDIRN_STORE   2
#define MOVEDIRN_LOADPTR 4
#define MOVEDIRN_LOADEXT 8	/* load and sign extend */
/*}}}*/

/*{{{  local variables*/
PRIVATE INT32 wspadjusts[MAX_LEX_LEVELS];
PRIVATE BIT32 staticlinkoffsets[MAX_LEX_LEVELS];
/*}}}*/

/*{{{  forward declarations*/
#if 0				/* no constructors are passed to the backend any more */
PRIVATE void tconstructorassign (treenode * dest, treenode * source);
#endif
#ifdef MOBILES
PRIVATE void storemobile_iorexp (treenode *const var, const BOOL useindex, const BOOL nochk);
PRIVATE void loadmobile_int (treenode *var, const BOOL nochk);
#endif
/*}}}*/

/*{{{  scope adjustment handling*/
/*{{{  PUBLIC void adjustworkspace (adjustment)*/
/*****************************************************************************
 *
 *  adjustworkspace updates the code generator's view of where the workspace
 *                  pointer is.  All workspace accesses will be 'adjustment'
 *                  slots higher than they were.
 *
 *****************************************************************************/
PUBLIC void adjustworkspace (INT32 adjustment)
{
	wspadjusts[be_lexlevel] += adjustment;
#ifdef DEBUG
	if (adjustment != 0)
		DEBUG_MSG (("adjustworkspace: adjusting be_lexlevel %d by %ld to %ld\n", be_lexlevel, adjustment, wspadjusts[be_lexlevel]));
#endif
	asmvalues[ASMNAME_WSSIZE] += adjustment;
	asmvalues[ASMNAME_STATIC] += adjustment;
	asmvalues[ASMNAME_VSPTR] += adjustment;
}

/*}}}*/
/*{{{  PUBLIC void setadjust (lexlevel, v)*/
/*****************************************************************************
 *
 *  setadjust  initialises the code generator's view of where the workspace
 *             pointer is for lexical level 'lexlevel'.
 *
 *****************************************************************************/
PUBLIC void setadjust (int lexlevel, INT32 v)
{
	DEBUG_MSG (("setadjust: setting adjust for lexlevel %d to %ld\n", lexlevel, v));
	wspadjusts[lexlevel] = v;
}

/*}}}*/
/*}}}*/
/*{{{  static link and vector space pointer routines*/
/*{{{  PUBLIC void setsloffset(lexlevel, v)*/
/*****************************************************************************
 *
 *  setsloffset sets the static link offset for 'lexlevel' to v
 *
 *****************************************************************************/
PUBLIC void setsloffset (int lexlevel, INT32 v)
{
	DEBUG_MSG (("setsloffset: setting sloffset for lexlevel %d to %ld\n", lexlevel, v));
#if 0
fprintf (stderr, "setsloffset: setting offset of lexlevel %d to %d\n", lexlevel, v);
#endif
	staticlinkoffsets[lexlevel] = v;
}

/*}}}*/
/*{{{  PRIVATE INT32 sloffsetof(level)*/
/*****************************************************************************
 *
 *  sloffsetof returns the workspace offset of the static link at lex level
 *             'level' relative to the static link pointer of lex level
 *             'level - 1' (or if level is the current lexical level, relative
 *             to the current value of wptr).
 *
 *****************************************************************************/
PRIVATE INT32 sloffsetof (int level)
{
	if (level == be_lexlevel) {
#if 0
fprintf (stderr, "sloffsetof (%d): staticlinkoffsets[level] & OFFSET_BITS = %d, wspadjusts[level] = %d\n", level, (staticlinkoffsets[level] & OFFSET_BITS), wspadjusts[level]);
#endif
		return ((staticlinkoffsets[level] & OFFSET_BITS) + wspadjusts[level]);
	} else {
		INT32 sloffset = staticlinkoffsets[level] & OFFSET_BITS;
		if (staticlinkoffsets[level + 1] & REPL_FLAG) {
			sloffset += wspadjusts[level];
		}
		return (sloffset);
	}
}

/*}}}*/
/*{{{  PUBLIC INT32 nameoffsetof(level)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  nameoffsetof returns the adjustment to the workspace offset of a
 *               variable at lexical level 'level'.  If 'level' is the current
 *               lexical level, this is relative to the current wptr,
 *               otherwise it is relative to the static link at offset
 *               'level - 1'.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC INT32 nameoffsetof (int level)
{
	if (level == be_lexlevel) {
		return (wspadjusts[level]);
	} else if (staticlinkoffsets[level + 1] & REPL_FLAG) {
		return (wspadjusts[level]);
	} else {
		return (0);
	}
}

/*}}}*/
/*{{{  PRIVATE BOOL loadlex (l)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  loadlex takes a lexical level, l, and returns FALSE if it is the current
 *          level, otherwise it loads a pointer to the workspace at level l
 *          and returns TRUE.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE BOOL loadlex (int l)
{
	/* Generate static link code to fetch workspace at level l */
	if (l != be_lexlevel) {
		int level = be_lexlevel - 1;
		INT32 sloffset = sloffsetof (be_lexlevel);

		genprimary (I_LDL, sloffset);
		gencomment0 ("PTR staticlink");
		/*{{{  load static link for level */
		for (; level > l; level--) {
			/* If it is a repl PAR static link the static link contains the
			   workspace pointer of the routine setting up the repl
			   AT THE TIME OF SETTING UP THE REPL */
			genprimary (I_LDNL, sloffsetof (level));
			gencomment1 ("PTR staticlink %d", level - l);
		}
		/*}}} */
		return (TRUE);
	} else {
		return (FALSE);
	}
}

/*}}}*/
/*{{{  PUBLIC void loadstaticlink (newlevel)*/
PUBLIC void loadstaticlink (int newlevel)
{
#if 0
fprintf (stderr, "loadstaticlink: newlevel = %d, be_lexlevel = %d\n", newlevel, be_lexlevel);
#endif
	if (newlevel == (be_lexlevel + 1)) {
		genprimary (I_LDLP, wspadjusts[be_lexlevel]);
		/* This should be the current workspace offset */
	} else {
		int nonlocal = loadlex (newlevel);

#if 0
fprintf (stderr, "loadstaticlink: sloffsetof(newlevel) = %d\n", sloffsetof (newlevel));
#endif
		genprimary (nonlocal ? I_LDNL : I_LDL, sloffsetof (newlevel));

		/* if required static link isn't normalised, normalise it */
		if (staticlinkoffsets[newlevel] & REPL_FLAG) {
			genprimary (I_LDNLP, wspadjusts[newlevel - 1]);
		}
	}
	gencomment0 ("PTR staticlink");
}

/*}}}*/
/*{{{  PUBLIC void loadreplreturnlink ()*/
/*****************************************************************************
 *
 *  loadreplreturnlink loads the static link required when ending a replicated
 *                     parallel process
 *
 *****************************************************************************/
PUBLIC void loadreplreturnlink (void)
{
	genprimary (I_LDL, sloffsetof (be_lexlevel));
	gencomment0 ("PTR repljoin");
}

/*}}}*/
/*{{{  PUBLIC void loadnewvsp(offset)*/
PUBLIC void loadnewvsp (const BIT32 offset)
{
	genprimary (I_LDL, vspoffset + wspadjusts[be_lexlevel]);
	gencomment0 ("PTR vsp");
	genprimary (I_LDNLP, offset);
}
/*}}}*/
/*{{{  PUBLIC void loadfb (void)*/
PUBLIC void loadfb (void)
{
#if 0
fprintf (stderr, "loadfb(): fbpoffset = %d, wspadjusts[be_lexlevel] = %d, be_lexlevel = %d, fbp_lexlevel = %d\n", fbpoffset, wspadjusts[be_lexlevel], be_lexlevel, fbp_lexlevel);
#endif
	if (loadlex (fbp_lexlevel)) {
		genprimary (I_LDNL, fbpoffset + nameoffsetof (fbp_lexlevel));
	} else {
		genprimary (I_LDL, fbpoffset + nameoffsetof (fbp_lexlevel));
	}
	return;
}
/*}}}*/
#ifdef MOBILES
/*{{{  PUBLIC void loadnewmsp (const BIT32 offset)*/
/*
 *	loads the current mobile-space pointer at offset `offset'
 */
PUBLIC void loadnewmsp (const BIT32 offset)
{
	genprimary (I_LDL, mspoffset + wspadjusts[be_lexlevel]);
	gencomment0 ("PTR msp");
	genprimary (I_LDNLP, offset);
}
/*}}}*/
/*{{{  PUBLIC void loadmsp (void)*/
/*
 *	loads current mobile-space pointer
 */
PUBLIC void loadmsp (void)
{
	genprimary (I_LDL, mspoffset + wspadjusts[be_lexlevel]);
	return;
}
/*}}}*/
/*{{{  PUBLIC void loadmpp (void)*/
/*
 *	loads current mobile-process pointer
 */
PUBLIC void loadmpp (void)
{
	if (loadlex (mpp_lexlevel)) {
		genprimary (I_LDNL, mppoffset + nameoffsetof (mpp_lexlevel));
	} else {
		genprimary (I_LDL, mppoffset + nameoffsetof (mpp_lexlevel));
	}
	return;
}
/*}}}*/
#endif
/*}}}*/
/*{{{  temp restoring*/
/* bug TS/1900 22/10/92 */
PRIVATE treenode *evaluated_temp_list;
/*{{{  PUBLIC void temp_clear_all_evaluated*/
PUBLIC void temp_clear_all_evaluated (void)
{
	evaluated_temp_list = NULL;
}

/*}}}*/
/*{{{  PUBLIC void temp_mark_as_evaluated*/
PUBLIC void temp_mark_as_evaluated (treenode * const nptr)
{
	SetTag (nptr, T_PREEVALTEMP);
	SetNVNextTemp (nptr, evaluated_temp_list);
	evaluated_temp_list = nptr;
}

/*}}}*/
/*{{{  PUBLIC void temp_unevaluate_all*/
PUBLIC void temp_unevaluate_all (void)
{
	treenode *tptr;
	for (tptr = evaluated_temp_list; tptr != NULL; tptr = NVNextTempOf (tptr)) {
		SetTag (tptr, T_TEMP);
	}
	evaluated_temp_list = NULL;
}

/*}}}*/

/*}}}*/
/*{{{  PUBLIC int simplify (opdmode, opd)*/
/*****************************************************************************
 *
 *  simplify evaluates an expression into a temporary, if required
 *
 *****************************************************************************/
PUBLIC int simplify (int opdmode, treenode * opd)
{
	BETRACE ("gen11: simplify (enter): nptr=%p (%s)", opd, tagstring (TagOf (opd)));
	if (preeval (opdmode, opd)) {
		const int newmode = tempmodeof (opdmode);

#if 0
fprintf (stderr, "gen11: simplify: preevaluating mode %s to ", opdmode_string (opdmode));
fprintf (stderr, "mode %s, opd =", opdmode_string (newmode));
printtreenl (stderr, 4, opd);
#endif
		/* Mark opd as initialised so that in the course of initialising it
		   we don't start to recursively initialise it. */
		/*SetTag(opd, T_PREEVALTEMP); */
		temp_mark_as_evaluated (opd);	/* bug TS/1900 22/10/92 */

		if (NModeOf (opd) == NM_POINTER) {
			/*{{{  treat temporary as an abbreviation */
			if (opdmode != P_PTR) {
				geninternal_is (GEN_BAD_OPD, opdmode, "simplify");
			}
			loadopd (opdmode, NDeclOf (opd), 0);
			storeinname (opd, 0);
			/*}}} */
		} else {
			/*{{{  treat temporary as a declaration */
			tsimpleassign (ntypeof (opd), newmode, opd, opdmode, NDeclOf (opd), MANY_REGS);
			/*}}} */
		}
		opdmode = newmode;
	}
	BETRACE ("gen11: simplify (leave)");
	return opdmode;
}

/*}}}*/
/*{{{  PUBLIC void loadconstant*/
/*****************************************************************************
 *
 *  loadconstant loads the value of the constant c onto the expression stack
 *
 *****************************************************************************/
PUBLIC void loadconstant (const INT32 c)
{
#if 0
fprintf (stderr, "gen11: loadconstant (0x%8.8x)\n", (unsigned int)c);
#endif
	genloadconstant (c);
}

/*}}}*/
/*{{{  PRIVATE void tbyteoffset*/
PRIVATE void tbyteoffset (const INT32 offset)
{
	if (offset == 0)
		/* skip */ ;
	else if ((bytesperword == 2) && ((offset & 0x1) == 0))
		genprimary (I_LDNLP, offset / 2 /* bytesperword */ );
	else if ((bytesperword == 4) && ((offset & 0x3) == 0))
		genprimary (I_LDNLP, offset / 4 /* bytesperword */ );
	else if (use_bsub_not_adc) {	/* T9000 optimisation 18/7/91 */
		genprimary (I_LDC, offset);
		gensecondary (I_BSUB);
	} else
		genprimary (I_ADC, offset);
}

/*}}}*/
/*{{{  load/store/loadptr       names*/
/*{{{  PRIVATE void movepointer (const int dirn, const int inonlocal, const INT32 word, const int type, BOOL * const need_signextend, const BOOL devaccess)*/
PRIVATE void movepointer (const int dirn, const int inonlocal, const INT32 word, const int type, BOOL * const need_signextend, const BOOL devaccess)
/*
  This assumes a pointer has already been loaded, and does a load/store off it
  It requires 'word' to be a word offset, if we're using word sized data.
*/
{
	if (dirn == MOVEDIRN_STORE)
		checkerror ();
	if (istargetbytesize (type)) {
		tbyteoffset (word);	/* In practice this is never non-zero */
		if (dirn != MOVEDIRN_LOADPTR)
		 {		/* bug 738 5/11/90 */
			/*{{{  load/store a byte */
			if (devaccess)
				/*{{{  device instructions */
			{
				/*{{{  T9000_gamma_baddevaccess */
				if (T9000_gamma_baddevaccess (&tx_global)) {
					gensecondary (I_INTDIS);
					gencomment0 ("bug fix for device access");
				}
				/*}}} */
				gensecondary ((dirn == MOVEDIRN_STORE) ? I_DEVSB : I_DEVLB);
				/*{{{  T9000_gamma_baddevaccess */
				if (T9000_gamma_baddevaccess (&tx_global)) {
					gensecondary (I_INTENB);
					gencomment0 ("bug fix for device access");
				}
				/*}}} */
			}
			/*}}} */
			else
				gensecondary ((dirn == MOVEDIRN_STORE) ? I_SB : I_LB);
		}
		/*}}} */
	} else if (use_shortintops && isshortint (type)) {	/* T9000 shorts 17/7/91 */
		tbyteoffset (word * 2);	/* In practice this is never non-zero */
		if (dirn != MOVEDIRN_LOADPTR)
			/*{{{  load/store a sixteen */
		{
			if (devaccess)
				/*{{{  device access */
			{
				/*{{{  T9000_gamma_baddevaccess */
				if (T9000_gamma_baddevaccess (&tx_global)) {
					gensecondary (I_INTDIS);
					gencomment0 ("bug fix for device access");
				}
				/*}}} */
				gensecondary ((dirn == MOVEDIRN_STORE) ? I_DEVSS : I_DEVLS);
				/*{{{  T9000_gamma_baddevaccess */
				if (T9000_gamma_baddevaccess (&tx_global)) {
					gensecondary (I_INTENB);
					gencomment0 ("bug fix for device access");
				}
				/*}}} */
				if (dirn == MOVEDIRN_LOADEXT)
					gensecondary (I_XSWORD);
			}
			/*}}} */
			else
				gensecondary ((dirn == MOVEDIRN_STORE) ? I_SS : (dirn == MOVEDIRN_LOADEXT) ? I_LSX : I_LS);
		}
		/*}}} */
		if (need_signextend != NULL)
			*need_signextend = FALSE;
	} else
		/*{{{  load/store a word */
	{
		if (devaccess) {
			if (dirn == MOVEDIRN_LOADPTR)
				genprimary (inonlocal, word);
			else {
				genprimary (I_LDNLP, word);
				/*{{{  T9000_gamma_baddevaccess */
				if (T9000_gamma_baddevaccess (&tx_global)) {
					gensecondary (I_INTDIS);
					gencomment0 ("bug fix for device access");
				}
				/*}}} */
				gensecondary ((dirn == MOVEDIRN_STORE) ? I_DEVSW : I_DEVLW);
				/*{{{  T9000_gamma_baddevaccess */
				if (T9000_gamma_baddevaccess (&tx_global)) {
					gensecondary (I_INTENB);
					gencomment0 ("bug fix for device access");
				}
				/*}}} */
			}
		} else
			genprimary (inonlocal, word);
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE void endianadjustpointer (treenode *const nptr)*/
PRIVATE void endianadjustpointer (treenode *const nptr)
{
	int adjust = 0;
	int type;
	
	switch (TagOf (nptr)) {
		case N_ABBR:
		case N_VALABBR:
		case N_RETYPE:
		case N_VALRETYPE:
			type = ntypeof (DValOf (NDeclOf (nptr)));
			break;
		default:
			type = ntypeof (nptr);
			break;
	}

	switch (type) {
		case S_BYTE:
		case S_BOOL:
		case S_INT16:
		case S_UINT16:
			if (target_bigendian) {
				adjust = bytesperword - bytesinscalar (type);
			}
			break;
	}
	
	if (adjust) {
		genprimary (I_ADC, adjust);
	}
}
/*}}}*/
/*{{{  PRIVATE void movename (treenode *const nptr, const INT32 w, const int ilocal, const int inonlocal)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  movename loads or stores a name or a pointer to a name.
 *           nptr represents the name.
 *           For a long type, w is the word which we wish to load/store.
 *           ilocal is the load/store instruction used for a local object,
 *           inonlocal is the load/store instruction used for a non-local
 *           object.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void movename (treenode *const nptr, const INT32 w, const int ilocal, const int inonlocal)
{
#if 0
fprintf (stderr, "gen11: movename() w = %d, ilocal = %d, inonlocal = %d, isplaced (nptr) = %d, nptr = ", w, ilocal, inonlocal, isplaced (nptr));
printtreenl (stderr, 4, nptr);
#endif
	if (NVUseCountOf (nptr) == 0) {
		/* This should never happen */
		geninternal_s (GEN_BAD_USECOUNT, WNameOf (NNameOf (nptr)));
	}

	if (isplaced (nptr)) {
		/*{{{  move to/from a placed variable */
		loadconstant (NVOffsetOf (nptr));
		/*genprimary(inonlocal, w); *//* removed 8/11/91 for bug TS/1467 */
		/*}}} */
	} else {
		/*{{{  move to/from a normal variable */
		const int level = NLexLevelOf (nptr);
		const BOOL nonlocal = loadlex (level);
		const INT32 wsposn = NVOffsetOf (nptr) + w + nameoffsetof (level);

		genprimary ((nonlocal ? inonlocal : ilocal), wsposn);
		/*}}} */
	}
	/*{{{  debugging */
	if (assembly_output && ((alloc_strategy & ALLOC_NOCOMMENTS) == 0)) {
		/*{{{  comment on the type MDP */
		{
			treenode *type = follow_user_type (NTypeOf (nptr));
			if (TagOf (type) == S_ARRAY)
				type = follow_user_type (ARTypeOf (type));
			if (ispointer (nptr))
				gencomment0 ("PTR");
			gencomment0 (tagstring (TagOf (type)));	/* MDP */
		}
		/*}}} */
		switch (TagOf (nptr)) {
		case T_TEMP:
		case T_PREEVALTEMP:
			gencomment1 ("$temp%d", NVVarNumOf (nptr));
			break;
#if 0				/* now S_FNFORMALRESULT is a namenode */
		case S_FNFORMALRESULT:
			gencomment0 ("formalresult");
			break;
#endif
		default:
			gencomment0 (WNameOf (NNameOf (nptr)));
			break;
		}
		if (w != 0) {
			fprintf (outfile, " + %d", (int)w);
		}
	}
	/*}}} */
}
/*}}}*/
#ifdef MOBILES
/*{{{  PRIVATE void mappointerswap (treenode **const first, treenode **const second, int regs)*/
/* evalute pointer-swap between first and second in at most "regs" regs */
PRIVATE void mappointerswap (treenode **const first, treenode **const second, int regs)
{
	int r1 = regsfor (*first);
	int r2 = regsfor (*second);

#if 0
fprintf (stderr, "mappointerswap: r1 = %d, r2 = %d\n", r1, r2);
fprintf (stderr, "*first =");
printtreenl (stderr, 4, *first);
#endif
	if ((r1 > 1) && (r2 > 1)) {
		/* reserve local 0 for temporary */
		reservelowworkspace (1);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void genpointerswap (treenode *const first, treenode *const second)*/
PRIVATE void genpointerswap (treenode *const first, treenode *const second)
{
	treenode *fst = first;
	treenode *snd = second;
	int r1, r2;

	if (TagOf (fst) == S_UNDEFINED) {
		fst = OpOf (fst);
	}
	if (TagOf (snd) == S_UNDEFINED) {
		snd = OpOf (snd);
	}
	r1 = regsfor (fst);
	r2 = regsfor (snd);

	gencomment0 ("genpointerswap");
	if (r1 == 1) {
		loadmobile_real (fst);
		loadmobile_real (snd);
		storemobile_nochk (fst);
		storemobile_nochk (snd);
	} else if (r2 == 1) {
		loadmobile_real (snd);
		loadmobile_real (fst);
		storemobile_nochk (snd);
		storemobile_nochk (fst);
	} else {
		/* map reserved local 0 for us */
		loadmobile_real (fst);
		genprimary (I_STL, 0);
		loadmobile_real (snd);
		storemobile_nochk (fst);
		genprimary (I_LDL, 0);
		storemobile_nochk (snd);
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void gencondfreedynmobile (treenode *const nptr)*/
PUBLIC void gencondfreedynmobile (treenode *const nptr)
{
	const int skiplab = newlab ();

	gencomment0 ("{{{  gencondfreedynmobile");
	/*
	 *	LDL <size-slot>
	 *	CJ :skiplab
	 *	LDL <ptr-slot>
	 *	MRELEASE
	 *	LDC 0
	 *	STL <size-slot>
	 *  :skiplab
	 */
	loadmobile_real (nptr);
	genbranch (I_CJ, skiplab);
	loadmobile_real (nptr);
	gensecondary (I_MT_RELEASE);
	genmobileundefine (nptr);
	setlab (skiplab);
	throw_the_result_away ();
	gencomment0 ("}}}");
	return;
}
/*}}}*/
/*{{{  PRIVATE void mapdynmobilearrayassign (treenode **const dest, treenode **const src, int regs)*/
PRIVATE void mapdynmobilearrayassign (treenode **const dest, treenode **const src, int regs)
{
	/* do nothing.. (yet) */
	return;
}
/*}}}*/
/*{{{  PRIVATE void gendynmobilearrayassign (treenode *const dest, treenode *const src, int regs)*/
PRIVATE void gendynmobilearrayassign (treenode *const dest, treenode *const src, int regs)
{
	const BOOL utagged = (TagOf (dest) == S_UNDEFINED);
	treenode *const rdest = (utagged ? OpOf (dest) : dest);
	#if 0
	const int dimcount = dynmobiledimensioncount (dest);
	int idim;
	#endif

	gencomment0 ("{{{  gendynmobilearrayassign");
	/* maybe free dest */
	if (utagged) {
		if (MOpTypeOf (dest)) {
			gencondfreedynmobile (rdest);
		}
	} else {
		gencondfreedynmobile (rdest);
	}
	/* simplify src and dest, if not already */
	if (TagOf (src) == T_TEMP) {
		simplify (P_PTR, src);
	}
	if (TagOf (dest) == T_TEMP) {
		simplify (P_PTR, dest);
	}
#if 0
fprintf (stderr, "gen11: gendynmobilearrayassign: src = ");
printtreenl (stderr, 4, src);
fprintf (stderr, "gen11: gendynmobilearrayassign: dest = ");
printtreenl (stderr, 4, dest);
#endif
	loadmobile_real (src);
	storemobile (rdest);
	genmobileundefine (src);
	genmobileunpack (rdest, FALSE, TRUE);
	gencomment0 ("}}}");
	return;
}
/*}}}*/
/*{{{  PRIVATE void mapdynmobilearraycreate (treenode **dest, treenode **src, int regs)*/
/*
 *	maps out creation of a dynamic MOBILE array
 */
PRIVATE void mapdynmobilearraycreate (treenode **dest, treenode **src, int regs)
{
	/* FIXME: to tidy up what's below .. */
	reservelowworkspace (1);
	return;
}
/*}}}*/
/*{{{  PUBLIC void mobilearray_base (treenode *const nptr, treenode **const basetype, int *basebytes)*/
PUBLIC void mobilearray_base (treenode *const nptr, treenode **const basetype, int *basebytes)
{
	const BOOL utagged = (TagOf (nptr) == S_UNDEFINED);
	treenode *const rptr = (utagged ? OpOf (nptr) : nptr);
	const int dimcount = dynmobiledimensioncount (nptr);
	int idim;
	
	*basetype = gettype (rptr);
	if (TagOf (*basetype) == S_MOBILE) {
		*basetype = MTypeOf (*basetype);
	}

	for (idim = 0; idim < dimcount; idim++) {
		if (TagOf (*basetype) == S_ARRAY) {
			*basetype = ARTypeOf (*basetype);
		} else {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 0, "mobilearray_base (expected S_ARRAY in type tree)");
		}
	}

	if (TagOf (*basetype) == N_TYPEDECL) {
		/* skip into type */
		*basetype = follow_user_type (*basetype);
	}

	switch (TagOf (*basetype)) {
		case S_MOBILE:
			/* some nested MOBILE structure */
			*basebytes = bytesperword;
			break;
		case S_RECORD:
			*basebytes = ARDimOf (*basetype);
			break;
		case S_ARRAY:
			/* not scalar, but maybe countable */
			*basebytes = bytesin (*basetype);
			
			if (*basebytes < 0) {
				geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "mobilearray_base (basetype has unknown size)");
			}
			break;
		default:
			*basebytes = bytesinscalar (TagOf (*basetype));

			if (*basebytes < 0) {
				geninternal_is (GEN_ERROR_IN_ROUTINE, 2, "mobilearray_base (basetype not scalar)");
			}
			break;
	}
}
/*}}}*/
/*{{{  PRIVATE void gendynmobilearraycreate (treenode *const dest, treenode *const src, int regs)*/
PRIVATE void gendynmobilearraycreate (treenode *const dest, treenode *const src, int regs)
{
	treenode *count_exp;
	const BOOL utagged = (TagOf (dest) == S_UNDEFINED);
	treenode *const rdest = (utagged ? OpOf (dest) : dest);
	const BOOL dynchan = (TagOf (ARTypeOf (src)) == S_CHAN);
	const BOOL empty = TypeAttrOf (src) & TypeAttr_empty ? TRUE : FALSE;
	const int dimcount = dynmobiledimensioncount (dest);
	int idim;
	treenode *had_temp = NULL;
	treenode *basetype = NULL;
	int basebytes = 0;

	gencomment0 ("{{{  gendynmobilearraycreate");
	/* maybe free dest */
	if (utagged) {
		if (MOpTypeOf (dest)) {
			gencondfreedynmobile (rdest);
		}
	} else {
		gencondfreedynmobile (rdest);
	}

	/* find actual base size and type */
	mobilearray_base (dest, &basetype, &basebytes);

#if 0
fprintf (stderr, "gen11: gendynmobilearraycreate: regs = %d, src = ", regs);
printtreenl (stderr, 4, src);
fprintf (stderr, "gen11: gendynmobilearraycreate: dimcount = %d, dest = ", dimcount);
printtreenl (stderr, 4, dest);
#endif
	
	if (empty) {
		genprimary (I_LDC, 0);
	} else {
		texp (ARDimLengthOf (src), regs);
		/* do not skip anymore, allocate zero-length mobiles arrays as real */
	}

	if (dynchan) {
		treenode *temp = LeftOpOf (ARDimLengthOf (src));
		int i_type = 0;

		gensecondary (I_DUP);
		storeinname (temp, 0);
		
		if (WSH > 1) {
			genshiftimmediate (I_SHR, WSH - 1);
		}

		switch (WSH) {
			case 1:
				i_type = MT_NUM_INT16;
				break;
			case 2:
				i_type = MT_NUM_INT32;
				break;
			case 3:
				i_type = MT_NUM_INT64;
				break;
		}
		
		loadconstant (MT_MAKE_ARRAY_TYPE (dimcount, MT_MAKE_NUM (i_type)));
		gensecondary (I_MT_ALLOC);
	} else {
		BOOL empty = TypeAttrOf (src) & TypeAttr_empty ? TRUE : FALSE;
		int i_type = 0, shift = 0;

		if (TagOf (basetype) == S_MOBILE) {
			if (isdynmobilearraytype (basetype)) {
				/* We only support mobile arrays of mobile arrays of
				 * integer types, so while this will create in accurate
				 * descriptors (wrong dimensions, wrong number), it
				 * should be enough for the runtime.
				 */
				i_type = MT_MAKE_ARRAY_TYPE (1, MT_SIMPLE | MT_MAKE_TYPE (MT_NUM));
			} else if (isdynmobilechantype (basetype)) {
				treenode *base = gettype_main_orig (basetype);
				int shared_in, shared_out;
				shared_in = (NTypeAttrOf (base) == (TypeAttr_marked_in | TypeAttr_shared));
				shared_out = (NTypeAttrOf (base) == (TypeAttr_marked_out | TypeAttr_shared));
				i_type = MT_SIMPLE | MT_MAKE_TYPE (MT_CB);
				if (shared_in || shared_out) {
					i_type |= MT_CB_SHARED;
				}
			} else {
				i_type = MT_SIMPLE | MT_MAKE_TYPE (MT_MT);
			}
			shift = WSH;
		} else {
			switch (TagOf (basetype)) {
				case S_BOOL:
				case S_BYTE:
					i_type 		= MT_NUM_BYTE;
					shift 		= 0;
					break;
				case S_INT16:
				case S_UINT16:
					i_type 		= MT_NUM_INT16;
					shift 		= 1;
					break;
				case S_INT32:
				case S_UINT32:
					i_type	 	= MT_NUM_INT32;
					shift 		= 2;
					break;
				case S_INT64:
				case S_UINT64:
					i_type 		= MT_NUM_INT64;
					shift 		= 3;
					break;
				case S_REAL32:
					i_type	 	= MT_NUM_REAL32;
					shift		= 2;
					break;
				case S_REAL64:
					i_type 		= MT_NUM_REAL64;
					shift		= 3;
					break;
				case S_INT:
				case S_UINT:
					switch (WSH) {
						case 0: i_type = MT_NUM_BYTE; break;
						case 1: i_type = MT_NUM_INT16; break;
						case 2: i_type = MT_NUM_INT32; break;
						case 3: i_type = MT_NUM_INT64; break;
					}
					shift = WSH;
					break;
				default:
					/* Treat everything that's left as just a big array of
					 * bytes.  All that should be left here is arrays of
					 * RECORD types, it does however mean we can't do
					 * anything clever with the endianness of data that
					 * gets handled this way.
					 */
					i_type		= MT_NUM_BYTE;
					shift		= 0;
					break;
			}
			i_type = MT_MAKE_NUM (i_type);
		}
		if (shift && !empty) {
			genshiftimmediate (I_SHR, shift);
		}
		if (TypeAttrOf (src) & (TypeAttr_aligned | TypeAttr_dma)) {
			int alignment = 0, flags = 0;

			if (TypeAttrOf (src) & TypeAttr_dma) {
				flags = MT_ARRAY_OPTS_DMA;
			}
			if (TypeAttrOf (src) & TypeAttr_aligned) {
				treenode *exp = ARAlignmentOf (src);
				if (TagOf (exp) != S_CONSTEXP) {
					generr (GEN_BAD_ALIGNMENT);
				}
				alignment = LoValOf (exp);
			}

			i_type = MT_MAKE_ARRAY_OPTS (flags, alignment, i_type);
		}

		loadconstant (MT_MAKE_ARRAY_TYPE (dimcount, i_type));
		gensecondary (I_MT_ALLOC);
	}

	/* gensecondary (I_MALLOC); */
#if 0
fprintf (stderr, "gendynmobilearraycreate: storing result in rdest =");
printtreenl (stderr, 4, rdest);
#endif
	storemobile_nochk (rdest);
	genmobileunpack (rdest, TRUE, FALSE);
	/* storeinname (rdest, 0); */
	gencomment0 ("store mobile ptr");

	/*{{{  if allocating a dynamic mobile array of channels, be sure to NotProcess them -- temp now in tree */
	if (dynchan) {
		const int loophead = newlab ();
		const int loopend = newlab ();
		treenode *temp = LeftOpOf (ARDimLengthOf (src));

		had_temp = temp;

		loadname (temp, 0);
		storemobilesize (rdest, 1);

#if 0
fprintf (stderr, "gendynmobilearraycreate: dynchan: rdest = ");
printtreenl (stderr, 4, rdest);
fprintf (stderr, "gendynmobilearraycreate: dynchan: temp = ");
printtreenl (stderr, 4, temp);
#endif
		if (dimcount != 1) {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 0, "gendynmobilearraycreate (dynchan, but dimcount != 1)");
		}
		/* layout of the resulting memory is as follows:
		 *  workspace:
		 *	rdest[1]	-- #array elements
		 *	rdest[0]	-- ptr to first element (ptr) ------+
		 *	                                                    |
		 *  dynamic mobilespace:                                    |
		 *      chan(n-1)       -- nth channel word                 |
		 *        ...                                               |
		 *      chan0           -- 1st channel word                 |
		 *      ptr(n-1)        -- pointer to nth channel word      |
		 *        ...                                               |
		 *      ptr0            -- pointer to 1st channel word <----+
		 */

		/* _NEW_ code-gen (with a temporary) "temp" */

		/*
		 * going to generate:
		 *
		 *  loophead:
		 *	LD temp								[temp']
		 *	ADC -4								[temp]
		 *	DUP								[temp,temp]
		 *	ST temp								[temp]
		 *	LD rdest[0]							[rdest[0],temp]
		 *	SUM		-- absolute address starting at (n-1)th PTR	[addr]
		 *	LD rdest[1]							[rdest[1], addr]
		 *	SUM		-- address of corresponding channel word	[caddr]
		 *	DUP								[caddr, caddr]
		 *	NULL								[0, caddr, caddr]
		 *	REV								[caddr, 0, caddr]
		 *	STNL 0		-- initialise channel word			[caddr]
		 *	DUP								[caddr, caddr]
		 *	LD rdest[1]							[rdest[1], caddr, caddr]
		 *	DIFF								[addr, caddr]
		 *	STNL 0		-- initialise channel pointer			[]
		 *	LD temp
		 *	CJ loopend
		 *	.TSDEPTH 0
		 *	J loophead
		 *  loopend:
		 *
		 */

		gencomment0 ("begin initialise dynamic channel array");
		setlab (loophead);
		loadname (temp, 0);
		genprimary (I_ADC, -4);
		gensecondary (I_DUP);
		storeinname (temp, 0);
		loadmobile (rdest);
		genprimary (I_LDNL, 0);
		gensecondary (I_SUM);
		loaddynmobilesize (rdest, 1);
		gensecondary (I_SUM);
		gensecondary (I_DUP);
		gensecondary (I_NULL);
		gensecondary (I_REV);
		genprimary (I_STNL, 0);
		gensecondary (I_DUP);
		loaddynmobilesize (rdest, 1);
		gensecondary (I_DIFF);
		genprimary (I_STNL, 0);
		loadname (temp, 0);
		genbranch (I_CJ, loopend);
		genbranch (I_J, loophead);
		setlab (loopend);
		throw_the_result_away ();
		gencomment0 ("end initialise dynamic channel array");

	}
	/*}}}*/
	/*{{{  mobile sub-initialisation ?*/
	#if 0
	{
		treenode *etype = ARTypeOf (src);

		if (TagOf (follow_user_type (etype)) == S_MOBILE) {
			BOOL is_chan_type = FALSE;
			treenode *subtype = MTypeOf (follow_user_type (etype));

			if (TagOf (subtype) == S_RECORD) {
				treenode *fields = ARTypeOf (subtype);

				if ((TagOf (fields) == S_LIST) && (TagOf (ThisItem (fields)) == S_DECL) && (TagOf (NTypeOf (DNameOf (ThisItem (fields)))) == S_CHAN)) {
					is_chan_type = TRUE;
				} else if ((TagOf (fields) == S_DECL) && (TagOf (NTypeOf (DNameOf (fields))) == S_CHAN)) {
					is_chan_type = TRUE;
				}
				if (is_chan_type) {
					/*{{{  nested channel-type initialisations */
					int loophead = newlab ();
					int loopend = newlab ();

					/* need to initialise these to NULL -- can use rdest[1] for a temporary */
					if (dimcount != 1) {
						geninternal_is (GEN_ERROR_IN_ROUTINE, 0, "gendynmobilearraycreate: (only one dimension for nested MOBILEs please :))");
					}
					/* generating:
					 *	<size expression (bytes)>
					 *	ST rdest[1]
					 * loophead:
					 *	LD rdest[1]
					 *	LDC 4
					 *	DIFF
					 *	DUP
					 *	ST rdest[1]
					 *	LD rdest[0]
					 *	SUM
					 *	NULL
					 *	REV
					 *	STNL 0
					 *	LD rdest[1]
					 *	CJ loopend
					 *	J loophead
					 * loopend:
					 */
					gencomment0 ("begin dynamic MOBILE array of MOBILE channel-types initialisation");
					texp (ARDimLengthOf (src), regs);
					storemobilesize (rdest, 1);
					setlab (loophead);
					loaddynmobilesize (rdest, 1);
					genprimary (I_LDC, 4);
					gensecondary (I_DIFF);
					gensecondary (I_DUP);
					storemobilesize (rdest, 1);
					loadmobile (rdest);
					gensecondary (I_SUM);
					gensecondary (I_NULL);
					gensecondary (I_REV);
					genprimary (I_STNL, 0);
					loaddynmobilesize (rdest, 1);
					genbranch (I_CJ, loopend);
					genbranch (I_J, loophead);
					setlab (loopend);
					throw_the_result_away ();
					gencomment0 ("end dynamic MOBILE array of MOBILE channel-types initialisation");
					/*}}}*/
				}
			} else if (isdynmobilearraytype (follow_user_type (etype))) {
				/*{{{  initialise mobile array of mobile arrays (first dimension counts to zero)*/
				treenode *sizeexp = ARDimLengthOf (src);
				int loophead = newlab ();
				int loopend = newlab ();
				treenode *elembytes = NULL;

#if 0
fprintf (stderr, "gen11: gendynmobilearraycreate: f(etype) is MOBILE array type.  ought to initialise..  sizeexp =");
printtreenl (stderr, 4, sizeexp);
#endif
				if ((TagOf (sizeexp) == S_CONSTEXP) && (TagOf (CExpOf (sizeexp)) == S_TIMES)) {
					elembytes = RightOpOf (CExpOf (sizeexp));
				} else if ((TagOf (sizeexp) == S_TIMES) && (TagOf (RightOpOf (sizeexp)) == S_CONSTEXP)) {
					elembytes = RightOpOf (sizeexp);
				}
				if (!elembytes) {
					geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "gendynmobilearraycreate -- !elembytes");
				}
#if 0
fprintf (stderr, "gen11: gendynmobilearraycreate: MOBILE array type, elembytes =");
printtreenl (stderr, 4, elembytes);
#endif
				/* just need to initialise the first dimension counts of each element */
				/* initialisation is similar to the above for arrays of channel-types */
				gencomment0 ("begin dynamic MOBILE array of MOBILE arrays initialisation");
				texp (sizeexp, MAXREGS);
				storemobilesize (rdest, 1);
				setlab (loophead);
				loaddynmobilesize (rdest, 1);
				texp (elembytes, MAXREGS - 1);
				gensecondary (I_DIFF);
				gensecondary (I_DUP);
				storemobilesize (rdest, 1);
				loadmobile (rdest);
				gensecondary (I_SUM);
				genprimary (I_LDC, 0);		/* count */
				gensecondary (I_REV);
				genprimary (I_STNL, 1);		/* first dimension count */
				loaddynmobilesize (rdest, 1);
				genbranch (I_CJ, loopend);
				genbranch (I_J, loophead);
				setlab (loopend);
				throw_the_result_away ();
				gencomment0 ("end dynamic MOBILE array of MOBILE arrays initialisation");
				/*}}}*/
			}

		}
	}
	#endif
	/*}}}*/
	gencomment0 ("post any mobile sub-initialisation");

	/* evaluate count (elements) expression, put in size slot --> this should be TIMES [left, right] */
	count_exp = ARDimLengthOf (src);

	/* NOTE:
	 *	the count_exp tree we have here should be a sort of cascaded TIMES.
	 *	The outermost TIMES should contain the dimension sizes on one side,
	 *	and the base-type size (in bytes) on the other side.
	 */
#if 0
fprintf (stderr, "gendynmobilearraycreate (dimcount = %d) count_exp (ARDimLength(src)) =", dimcount);
printtreenl (stderr, 4, count_exp);
#endif
#if 0
fprintf (stderr, "gendynmobilearraycreate: basebytes = %d, basetype = ", basebytes);
printtreenl (stderr, 4, basetype);
#endif
	/* pick first dimension */
	if (TagOf (count_exp) == S_CONSTEXP) {
		count_exp = CExpOf (count_exp);
	}
	if (TagOf (count_exp) != S_TIMES) {
		geninternal_is (GEN_ERROR_IN_ROUTINE, 5, "gendynmobilearraycreate (expected S_TIMES in size tree)");
	}

	if ((TagOf (RightOpOf (count_exp)) == S_CONSTEXP) && (TagOf (CExpOf (RightOpOf (count_exp))) == S_DUMMYEXP) &&
			(LoValOf (RightOpOf (count_exp)) == basebytes)) {
		/* RHS of the TIMES is the base-type size */
		count_exp = LeftOpOf (count_exp);
	} else {
		count_exp = RightOpOf (count_exp);
	}
#if 0
fprintf (stderr, "gendynmobilearraycreate: ready to process dimensions, count_exp =");
printtreenl (stderr, 4, count_exp);
#endif

	for (idim = 0; idim < dimcount; idim++) {
		treenode *lcount;

		if (idim == (dimcount - 1)) {
			/* last or only dimension, straight evaluation */
			lcount = count_exp;
		} else {
			/* count_exp should be some TIMES-involved node of dimension sizes */
			if (TagOf (count_exp) == S_CONSTEXP) {
				count_exp = CExpOf (count_exp);
			}
			if (TagOf (count_exp) != S_TIMES) {
				geninternal_is (GEN_ERROR_IN_ROUTINE, 6, "gendynmobilearraycreate (expected S_TIMES in size tree)");
			}
			lcount = LeftOpOf (count_exp);
			count_exp = RightOpOf (count_exp);
		}

		texp (lcount, regs);
		storemobilesize (rdest, idim + 1);
	}
#if 0
fprintf (stderr, "gendynmobilearraycreate (generate new array and assign to var): dest (utagged:%d) =", utagged ? MOpTypeOf(dest) : -1);
printtreenl (stderr, 4, dest);
fprintf (stderr, "src (presumably something which we need to evaluate) =");
printtreenl (stderr, 4, src);
#endif
	if (!empty) {
		throw_the_result_away ();
	}

	gencomment0 ("}}}");
	return;
}
/*}}}*/
/*{{{  PRIVATE void genmobilearrayunpack (treenode *const nptr, const BOOL nochk, const BOOL dimensions)*/
PRIVATE void genmobilearrayunpack (treenode *const nptr, const BOOL nochk, const BOOL dimensions)
{
	const int level = NLexLevelOf (nptr);
	const BOOL nonlocal = loadlex (level);
	const INT32 wsposn = NVOffsetOf (nptr) + nameoffsetof (level);
	const int dimcount = dynmobiledimensioncount (nptr);
	int endlab = NO_LABEL;
	int undeflab = NO_LABEL;
	int i;

	gencomment0 ("{{{  genmobilearrayunpack");

	if (!nochk) {
		endlab = newlab ();
		undeflab = newlab ();
	}

	genprimary (nonlocal ? I_LDNL : I_LDL, wsposn + 1);
	if (!nochk) {
		gensecondary (I_DUP);
		genbranch (I_CJ, undeflab);
	}
	if (dimensions) {
		gensecondary (I_DUP);
	}
	genprimary (I_LDNL, 0);
	loadlex (level);
	genprimary (nonlocal ? I_STNL : I_STL, wsposn);
	if (dimensions) {
		for (i = 1; i <= dimcount; ++i) {
			if (i < dimcount) {
				gensecondary (I_DUP);
			}
			genprimary (I_LDNL, i);
			loadlex (level);
			genprimary (nonlocal ? I_STNL : I_STL, wsposn + 1 + i);
		}
	}
	if (!nochk) {
		genbranch (I_J, endlab);
		setlab (undeflab);
		throw_the_result_away ();
		throw_the_result_away ();
		loadconstant (0);
		loadlex (level);
		genprimary (nonlocal ? I_STNL : I_STL, wsposn + 2);
		setlab (endlab);
	}

	gencomment0 ("}}}");

	return;
}
/*}}}*/
/*{{{  PUBLIC void genmobileunpack (treenode *const nptr, const BOOL nochk, const BOOL dimensions)*/
PUBLIC void genmobileunpack (treenode *const nptr, const BOOL nochk, const BOOL dimensions)
{
	switch (TagOf (nptr)) {
		case S_ARRAYITEM:
		case S_RECORDITEM:
			break;
		default:
			if (isdynmobilearray (nptr)) {
				genmobilearrayunpack (nptr, nochk, dimensions);
			}
			break;
	}
	return;
}
/*}}}*/
/*}}}*/
/*{{{  PRIVATE void mapdynmobileproccreate (treenode **dest, treenode **src, int regs)*/
/*
 *	maps out creation of a MOBILE PROC -- trivial
 */
PRIVATE void mapdynmobileproccreate (treenode **dest, treenode **src, int regs)
{
	/* FIXME: reserve local 0 for holding the MOBILE pointer -- only really needed if "dest" is complex */
	reservelowworkspace (1);
	return;
}
/*}}}*/
/*{{{  PRIVATE void mapdynmobilebarriercreate (treenode **dest, treenode **src, int regs)*/
/*
 *	maps out creation of a MOBILE BARRIER -- trivial
 */
PRIVATE void mapdynmobilebarriercreate (treenode **dest, treenode **src, int regs)
{
	/* use local 0 to hold pointer */
	/* reservelowworkspace (1); */
	return;
}
/*}}}*/
/*{{{  PRIVATE void gendynmobileproccreate (treenode *const dest, treenode *const src, int regs)*/
PRIVATE void gendynmobileproccreate (treenode *const dest, treenode *const src, int regs)
{
	const BOOL utagged = (TagOf (dest) == S_UNDEFINED);
	treenode *const rdest = (utagged ? OpOf (dest) : dest);
	INT32 p_ws, p_vs, p_ms;
	treenode *pname;
	int nparams, mppslots;

	if (TagOf (src) != S_ALLOC_PROC) {
		geninternal_is (GEN_ERROR_IN_ROUTINE, 0, "gendynmobileproccreate");
		return;
	}
	if (utagged) {
		if (MOpTypeOf (dest)) {
			gencondfreedynproctype (rdest);
		}
	} else {
		gencondfreedynproctype (rdest);
	}

	pname = ARTypeOf (src);
	nparams = NPParamsOf (pname);
 	getprocwsandvsandms (pname, &p_ws, &p_vs, &p_ms);

	if (p_ms) {
		geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "gendynmobileproccreate");
		return;
	}

#if 0
fprintf (stderr, "gen11: gendynmobileproccreate(): regs = %d, nparams = %d, ws = %d, vs = %d, ms = %d, pname = ", regs, nparams, p_ws, p_vs, p_ms);
printtreenl (stderr, 4, pname);
fprintf (stderr, "gen11:                         : NTypeOf (pname) = ");
printtreenl (stderr, 4, NTypeOf (pname));
#endif
	mppslots = MPP_SLOTS;
	if (p_vs) {
		mppslots = MPP_SLOTS_VS;
	}
	if (p_ms) {
		mppslots = MPP_SLOTS_MS;
	}

	gencomment0 ("start mobile process allocation");
	/* allocate and store basic block */
	genprimary (I_LDC, mppslots << WSH);
	gensecondary (I_MALLOC);
	storemobile (rdest);

	/* have local temporary reserved for us */
	loadmobile_nochk (rdest);
	genprimary (I_STL, 0);

	/* load pointer to name */
	if (TagOf (pname) == N_LIBMPROCDECL) {
		genloadnamedlabptr (WNameOf (NNameOf (pname)));
	} else {
		genloadlabptr (NPLabelOf (pname), NOLAB, "MP PTR");
	}
	genprimary (I_LDL, 0);
	genprimary (I_STNL, MPP_IPTR);
	gencomment0 ("MPP_IPTR");

	/* calculate workspace requirements */
	genprimary (I_LDC, (p_ws + ((nparams > MAXREGS) ? nparams : MAXREGS) + 4) << WSH);
	gensecondary (I_DUP);
	genprimary (I_LDL, 0);
	genprimary (I_STNL, MPP_WSSIZE);
	gencomment0 ("MPP_WSSIZE");
	gensecondary (I_MALLOC);
	gensecondary (I_DUP);
	genprimary (I_LDL, 0);
	genprimary (I_STNL, MPP_WSBASE);
	gencomment0 ("MPP_WSBASE");
	genprimary (I_ADC, (p_ws + 2) << WSH);		/* setup initial Wptr */
	genprimary (I_LDL, 0);
	genprimary (I_STNL, MPP_WPTR);
	gencomment0 ("MPP_WPTR");

	/* if vectorspace required, allocate that too */
	if (p_vs) {
		genprimary (I_LDC, p_vs << WSH);
		gensecondary (I_DUP);
		genprimary (I_LDL, 0);
		genprimary (I_STNL, MPP_VSSIZE);
		gencomment0 ("MPP_VSSIZE");
		gensecondary (I_MALLOC);
		genprimary (I_LDL, 0);
		genprimary (I_STNL, MPP_VSBASE);
		gencomment0 ("MPP_VSBASE");
	} else {
		gensecondary (I_NULL);
		genprimary (I_LDL, 0);
		genprimary (I_STNL, MPP_VSBASE);
		gencomment0 ("MPP_VSBASE");
	}

	/* FIXME: mobilespace */
	gensecondary (I_NULL);
	genprimary (I_LDL, 0);
	genprimary (I_STNL, MPP_MSBASE);
	gencomment0 ("MPP_MSBASE");

	#if 0
	/* initialise barrier (NotProcess, NotProcess, 1, 1) */
	gensecondary (I_NULL);
	genprimary (I_LDL, 0);
	genprimary (I_STNL, MPP_BFPTR);
	gencomment0 ("MPP_BFPTR");
	gensecondary (I_NULL);
	genprimary (I_LDL, 0);
	genprimary (I_STNL, MPP_BBPTR);
	gencomment0 ("MPP_BBPTR");
	genprimary (I_LDC, 1);
	genprimary (I_LDL, 0);
	genprimary (I_STNL, MPP_BECNT);
	gencomment0 ("MPP_BECNT");
	genprimary (I_LDC, 1);
	genprimary (I_LDL, 0);
	genprimary (I_STNL, MPP_BCNT);
	gencomment0 ("MPP_BCNT");
	#endif

	/* initialise barrier */
	genprimary (I_LDC, 0);
	genprimary (I_LDC, MT_MAKE_BARRIER (MT_BARRIER_MPROC));
	gensecondary (I_MT_ALLOC);
	genprimary (I_LDL, 0);
	genprimary (I_STNL, MPP_BARRIER);

	/* nullify workspace-map chain */
	gensecondary (I_NULL);
	genprimary (I_LDL, 0);
	genprimary (I_STNL, MPP_MAPCHAIN);
	gencomment0 ("MPP_MAPCHAIN");

	/* set typehash and code map for the process */
#if 0
fprintf (stderr, "allocating mobile process: pname = ");
printtreenl (stderr, 4, pname);
fprintf (stderr, "allocating mobile process: NTypeOf (pname) = ");
printtreenl (stderr, 4, NTypeOf (pname));
#endif

	/* pname will be an N_MPROCDECL or N_LIBMPROCDECL */
	loadconstant (typehash (NTypeOf (pname)));
	genprimary (I_LDL, 0);
	genprimary (I_STNL, MPP_TYPEHASH);
	gencomment0 ("MPP_TYPEHASH");

	genloadcodemap (pname);
	genprimary (I_LDL, 0);
	genprimary (I_STNL, MPP_CODEMAP);
	gencomment0 ("MPP_CODEMAP");
	gencomment0 ("end mobile process allocation");

	return;
}
/*}}}*/
/*{{{  PUBLIC void gendynmobilebarriercreate (treenode *dest, treenode *const source, int regs)*/
/*
 *	generates a new mobile barrier
 */
PUBLIC void gendynmobilebarriercreate (treenode *dest, treenode *const source, int regs)
{
	const BOOL utagged = (TagOf (dest) == S_UNDEFINED);
	treenode *const rdest = (utagged ? OpOf (dest) : dest);

#if 0
fprintf (stderr, "gen11: gendynmobilebarriercreate(): dest = ");
printtreenl (stderr, 4, dest);
#endif
	/* better free (and resign) if a valid barrier */
	if (utagged) {
		if (MOpTypeOf (dest)) {
			gencondfreedynbarriertype (rdest, TRUE);
		}
	} else {
		gencondfreedynbarriertype (rdest, TRUE);
	}

	loadconstant (0);
	loadconstant (MT_MAKE_BARRIER (MT_BARRIER_FULL));
	gensecondary (I_MT_ALLOC);
	storemobile_nochk (rdest);

	return;
}
/*}}}*/
/*{{{  PUBLIC void gendynmobilebarrierassign (treenode *const dest, treenode *const source, int regs)*/
/*
 *	generates code that assigns one mobile barrier to another, possibly freeing the target
 */
PUBLIC void gendynmobilebarrierassign (treenode *const dest, treenode *const source, int regs)
{
	const BOOL utagged = (TagOf (dest) == S_UNDEFINED);
	treenode *const rdest = (utagged ? OpOf (dest) : dest);
	const int skiplab = newlab ();

#if 0
fprintf (stderr, "gen11: gendynmobilebarrierassign(): regs = %d, dest = ", regs);
printtreenl (stderr, 4, dest);
#endif
	/* better free (and resign) if a valid barrier */
	if (utagged) {
		if (MOpTypeOf (dest)) {
			gencondfreedynbarriertype (rdest, TRUE);
		}
	} else {
		gencondfreedynbarriertype (rdest, TRUE);
	}

	loadmobile (source);
	gensecondary (I_DUP);
	genbranch (I_CJ, skiplab);
	
	gensecondary (I_MT_CLONE);
	storemobile_nochk (dest);

	setlab (skiplab);
	throw_the_result_away ();
	throw_the_result_away ();
	return;
}
/*}}}*/
/*{{{  PUBLIC void loadmobilepointer (treenode *const var)*/
PUBLIC void loadmobilepointer (treenode *const var)
{
#if 0
fprintf (stderr, "gen11: loadmobilepointer: var =");
printtreenl (stderr, 4, var);
#endif
	if (TagOf (var) == S_RECORDITEM) {
		/* loading pointer of nested MOBILE */
		loadmobile (ASBaseOf (var));
		genprimary (I_LDNLP, (ASOffsetOf (var) / bytesperword));
	} else if (TagOf (var) == S_ARRAYITEM) {
		/* loading pointer of nested MOBILE -- EXP is always a BYTE offset now */
		treenode *type = gettype (var);

		texp_main (ASExpOf (var), MAXREGS, FALSE);
		loadmobile (ASBaseOf (var));
		if (isdynamicmobilechantype (type)) {
			gensecondary (I_WSUB);
		} else {
			gensecondary (I_WSUB);
		}
		/* gensecondary (I_SUM); -- changed now.. */
	} else {
		const int level = NLexLevelOf (var);
		const BOOL nonlocal = loadlex (level);
		const INT32 wsposn = NVOffsetOf (var) + nameoffsetof (level);

		if (isdynmobilearray (var)) {
			/* load pointer to hidden pointer field */
			genprimary (nonlocal ? I_LDNLP : I_LDLP, wsposn + 1);
		} else {
			genprimary (nonlocal ? I_LDNLP : I_LDLP, wsposn);
		}
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void loadmobile (treenode *var)  and  PRIVATE void loadmobile_int (treenode *var, const bool nochk)*/
PRIVATE void loadmobile_int (treenode *var, const BOOL nochk)
{
#if 0
fprintf (stderr, "gen11: loadmobile: var =");
printtreenl (stderr, 4, var);
#endif
	gencomments ("{{{  loadmobile (%s)", tagstring (TagOf (var)));
	switch (TagOf (var)) {
	case T_PREEVALTEMP:
		/* pre-evaluated to temporary */
		texp_main (var, MAXREGS - 1, FALSE);
		break;
	case S_RECORDITEM:
	case S_RECORDSUB:
		/* loading a nested MOBILE */
		loadmobile (ASBaseOf (var));
		genprimary (I_LDNL, (ASOffsetOf (var) / bytesperword));
		break;
	case S_ARRAYSUB:
		{
			treenode *type = gettype (var);

#if 0
fprintf (stderr, "gen11: loadmobile: ARRAYSUB: ASIndexOf(var) = ");
printtreenl (stderr, 4, ASIndexOf (var));
fprintf (stderr, "gen11: loadmobile: ARRAYSUB: ASExpOf(var) = ");
printtreenl (stderr, 4, ASExpOf (var));
fprintf (stderr, "gen11: loadmobile: ARRAYSUB: gettype(var) = ");
printtreenl (stderr, 4, type);
#endif
			/* get here when loading a mobile channel from a dynamic array of them (RECORDITEM -> ARRAYSUB) */
			if (isdynamicmobilechantype (type)) {
				if (regsfor (ASIndexOf (var)) > 1) {
					texp_main (ASIndexOf (var), MAXREGS - 1, FALSE);
					loadmobile (ASBaseOf (var));
				} else {
					loadmobile (ASBaseOf (var));
					texp_main (ASIndexOf (var), MAXREGS - 2, FALSE);
					gensecondary (I_REV);
				}
				gensecondary (I_WSUB);
			} else {
				geninternal_is (GEN_ERROR_IN_ROUTINE, 0, "loadmobile: unexpected ARRAYSUB");
			}
			genprimary (I_LDNL, 0);		/* dereference to get real pointer */
		}
		break;
	case S_ARRAYITEM:
		{
			treenode *type = gettype (var);
#if 0
fprintf (stderr, "gen11: loadmobile: ARRAYITEM: ASIndexOf(var) = ");
printtreenl (stderr, 4, ASIndexOf (var));
fprintf (stderr, "gen11: loadmobile: ARRAYITEM: ASExpOf(var) = ");
printtreenl (stderr, 4, ASExpOf (var));
fprintf (stderr, "gen11: loadmobile: ARRAYITEM: gettype(var) = ");
printtreenl (stderr, 4, type);
#endif
			/* loading a nested MOBILE */
			if (isdynamicmobilechantype (type)) {
				/* always indexed by WSUB -- may not have a sensible expression, so use index */
				treenode *asexp = ASExpOf (var) ? ASExpOf (var) : ASIndexOf (var);

				if (!ASExpOf (var)) {
					/* this is a bad sign.. */
					genwarn (GEN_WARN_BADCODE);
				}

				if (regsfor (asexp) > 1) {
					texp_main (asexp, MAXREGS - 1, FALSE);
					loadmobile (ASBaseOf (var));
				} else {
					loadmobile (ASBaseOf (var));
					texp_main (asexp, MAXREGS - 2, FALSE);
					gensecondary (I_REV);
				}
				gensecondary (I_WSUB);
				/* gensecondary (I_SUM)	*/	/* add in the offset.. */
			} else if (isdynamicmobilearraytype (type)) {
				const BOOL bsub = TagOf (ASExpOf (var)) == S_TIMES;

				/* ASExp is the BYTE offset for these, so BSUB it .. */
				if (regsfor (ASExpOf (var)) > 1) {
					texp_main (ASExpOf (var), MAXREGS - 1, FALSE);
					loadmobile (ASBaseOf (var));
				} else {
					loadmobile (ASBaseOf (var));
					texp_main (ASExpOf (var), MAXREGS - 2, FALSE);
					gensecondary (I_REV);
				}

				gensecondary (bsub ? I_BSUB: I_WSUB);
				genprimary (I_LDNL, 0); 	/* first part of double dereference */
			} else {
				geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "loadmobile -- ARRAYITEM for unsupported MOBILE type");
			}
			genprimary (I_LDNL, 0);		/* dereference to get real pointer */
#if 0
fprintf (stderr, "gen11: loadmobile: nested array!.  ASOffsetOf(var) = %d, ASExpOf(var) =", ASOffsetOf (var));
printtreenl (stderr, 4, ASExpOf(var));
#endif
		}
		break;
	default:
		{
			const int level = NLexLevelOf (var);
			const BOOL nonlocal = loadlex (level);
			const INT32 wsposn = NVOffsetOf (var) + nameoffsetof (level);
#if 0
fprintf (stderr, "load_mobile(): generating primary\n");
#endif
			/* genprimary (nonlocal ? I_LDNL : I_LDL, wsposn); */
			genprimary (nonlocal ? I_LDNL : I_LDL, wsposn);
			if (isdynamicmobilechantype (var) && !nochk) {
				genchecknotnull ();
			}
		}
	}
	gencomment0 ("}}}");
	return;
}
PUBLIC void loadmobile_real (treenode *var)
{
#if 0
fprintf (stderr, "gen11: loadmobile: var =");
printtreenl (stderr, 4, var);
#endif
	gencomments ("{{{  loadmobile_real (%s)", tagstring (TagOf (var)));
	switch (TagOf (var)) {
	case T_PREEVALTEMP:
	case S_RECORDITEM:
	case S_RECORDSUB:
	case S_ARRAYSUB:
		loadmobile_int (var, FALSE);
		break;
	case S_ARRAYITEM:
		{
			treenode *type = gettype (var);
			/* loading a nested MOBILE */
			if (isdynamicmobilechantype (type)) {
				loadmobile_int (var, FALSE);
			} else if (isdynamicmobilearraytype (type)) {
				const BOOL bsub = TagOf (ASExpOf (var)) == S_TIMES;

				/* ASExp is the BYTE offset for these, so BSUB it .. */
				if (regsfor (ASExpOf (var)) > 1) {
					texp_main (ASExpOf (var), MAXREGS - 1, FALSE);
					loadmobile (ASBaseOf (var));
				} else {
					loadmobile (ASBaseOf (var));
					texp_main (ASExpOf (var), MAXREGS - 2, FALSE);
					gensecondary (I_REV);
				}
				
				gensecondary (bsub ? I_BSUB : I_WSUB);
				genprimary (I_LDNL, 0); /* dereference to get real pointer */
			} else {
				geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "loadmobile -- ARRAYITEM for unsupported MOBILE type");
			}
		}
		break;
	default:
		{
			const int level = NLexLevelOf (var);
			const BOOL nonlocal = loadlex (level);
			const INT32 wsposn = NVOffsetOf (var) + nameoffsetof (level);
			
			if (isdynmobilearray (var)) {
				genprimary (nonlocal ? I_LDNL : I_LDL, wsposn + 1);
			} else {
				genprimary (nonlocal ? I_LDNL : I_LDL, wsposn);
			}
		}
	}
	gencomment0 ("}}}");
	return;
}
PUBLIC void loadmobile (treenode *var)
{
	loadmobile_int (var, FALSE);
}
PUBLIC void loadmobile_nochk (treenode *var)
{
	loadmobile_int (var, TRUE);
}
/*}}}*/
/*{{{  PUBLIC void loaddynmobilesize (treenode *const nptr, const int dim)*/
PUBLIC void loaddynmobilesize (treenode *const nptr, const int dim)
{
#if 0
fprintf (stderr, "loaddynmobilesize: dim = %d, nptr = ", dim);
printtreenl (stderr, 4, nptr);
#endif
	gencommentv ("loaddynmobilesize (%d) (%s)  {{{", dim, tagstring (TagOf (nptr)));
	if (TagOf (nptr) == S_RECORDITEM) {
		/*{{{  loading from nested MOBILE*/
		loadmobile (ASBaseOf (nptr));
		genprimary (I_LDNL, (ASOffsetOf (nptr) / bytesperword) + dim);
		/*}}}*/
	} else if (TagOf (nptr) == S_RECORDSUB) {
		/*{{{  loading from nested MOBILE -- without ASOffset*/
		treenode *field = ASIndexOf (nptr);

		if (TagOf (field) != N_FIELD) {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "loaddynmobilesize -- RECORDSUB index not FIELD");
		}
		/* loading from nested MOBILE -- no ASOffset here */
		loadmobile (ASBaseOf (nptr));
		genprimary (I_LDNL, (NVOffsetOf (ASIndexOf (nptr)) / bytesperword) + dim);
		/*}}}*/
	} else if (TagOf (nptr) == S_ARRAYSUB) {
		/*{{{  unexpected arraysub*/
#if 0
		/* could be loading from a nested MOBILE array, if it's a segment thereof */
		treenode *basetype = gettype (ASBaseOf (nptr));
		treenode *type = gettype (nptr);
		int offset = 0;

		loadmobile (ASBaseOf (nptr));

		texp_main (ASIndexOf (nptr), MAXREGS - 1, FALSE);		/* given index into first array */
		if (dynmobiledimensioncount (type) > 1) {
			/* multiply up index */
			loadconstant (dynmobiledimensioncount (type));
			gensecondary (I_PROD);
		}
		genprimary (I_LDNL, dim);

#endif
#if 0
fprintf (stderr, "loaddynmobilesize: ARRAYSUB: dim = %d, gettype (nptr) = ", dim);
printtreenl (stderr, 4, type);
#endif
		geninternal_is (GEN_ERROR_IN_ROUTINE, 2, "loaddynmobilesize -- unexpected ARRAYSUB");
		/*}}}*/
	} else if (TagOf (nptr) == S_ARRAYITEM) {
		/*{{{  loading from nested MOBILE array*/
		treenode *type = gettype (nptr);
		int skiplab = newlab ();

		if (isdynamicmobilearraytype (type)) {
			const BOOL bsub = TagOf (ASExpOf (nptr)) == S_TIMES;
			
			/* ASExp is the BYTE offset for these, so BSUB it .. */
			if (regsfor (ASExpOf (nptr)) > 1) {
				texp_main (ASExpOf (nptr), MAXREGS - 1, FALSE);
				loadmobile (ASBaseOf (nptr));
			} else {
				loadmobile (ASBaseOf (nptr));
				texp_main (ASExpOf (nptr), MAXREGS - 2, FALSE);
				gensecondary (I_REV);
			}
			
			gensecondary (bsub ? I_BSUB : I_WSUB);
		} else {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 3, "loaddynmobilesize -- unexpected MOBILE type");
		}
		
		genprimary (I_LDNL, 0);
		gensecondary (I_DUP);
		genbranch (I_CJ, skiplab);

		genprimary (I_LDNL, dim);
		gensecondary (I_DUP);

		setlab (skiplab);
		gensecondary (I_POP);
		/*}}}*/
	} else if (isdynmobilearray (nptr)) {
		/*{{{  loading size of a real dynamic mobile array*/
		const int level = NLexLevelOf (nptr);
		const BOOL nonlocal = loadlex (level);
		const INT32 wsposn = NVOffsetOf (nptr) + nameoffsetof (level);

		/* genprimary (nonlocal ? I_LDNL : I_LDL, wsposn + dim); */
		genprimary (nonlocal ? I_LDNL : I_LDL, wsposn + 1 + dim);
		/*}}}*/
	} else {
		/*{{{  loading size of something else.. (some static mobiles get called for this)*/
		treenode *type = gettype (nptr);

#if 0
fprintf (stderr, "loaddynmobilesize: gettype(nptr) =");
printtreenl (stderr, 4, type);
#endif
		if ((TagOf (type) == S_MOBILE) && (TagOf (MTypeOf (type)) == S_ARRAY) && (ARDimOf (MTypeOf (type)) >= 0)) {
			loadconstant (ARDimOf (MTypeOf (type)));
		} else {
			badtag (genlocn, TagOf (type), "loaddynmobilesize");
		}
		/*}}}*/
	}
	gencomment0 ("}}}");
	return;
}
/*}}}*/
/*{{{  PUBLIC void storemobile_nochk (treenode *const var)*/
PUBLIC void storemobile_nochk (treenode *const var)
{
	/* just store the mobile (no check) */
	storemobile_iorexp (var, TRUE, TRUE);
	return;
}
/*}}}*/
/*{{{  PUBLIC void storemobile (treenode *const var)*/
PUBLIC void storemobile (treenode *const var)
{
	/* store mobile using expression (checked) */
	storemobile_iorexp (var, FALSE, FALSE);
	return;
}
/*}}}*/
/*{{{  PRIVATE void storemobile_iorexp (treenode *const var, const BOOL useindex, const BOOL nochk)*/
PRIVATE void storemobile_iorexp (treenode *const var, const BOOL useindex, const BOOL nochk)
{
#if 0
fprintf (stderr, "gen11: storemobile: var =");
printtreenl (stderr, 4, var);
#endif
	gencomment0 ("storemobile_iorexp {{{");
	if (TagOf (var) == S_RECORDITEM) {
		/* storing into nested MOBILE */
		loadmobile (ASBaseOf (var));
		genprimary (I_STNL, (ASOffsetOf (var) / bytesperword));
	} else if (TagOf (var) == S_ARRAYITEM) {
		treenode *type = gettype (var);

#if 0
fprintf (stderr, "gen11: storemobile: nested array!.  regsfor(ASExpOf (var)) = %d, regsfor (ASBaseOf (var)) = %d\n", regsfor (ASExpOf (var)), regsfor (ASBaseOf (var)));
fprintf (stderr, "gen11: storemobile: nested array: ASIndexOf (var) =");
printtreenl (stderr, 4, ASIndexOf (var));
fprintf (stderr, "gen11: storemobile: nested array: ASExpOf (var) =");
printtreenl (stderr, 4, ASExpOf (var));
#endif
		/* new code.. */
		if (isdynamicmobilechantype (type)) {
			if (useindex || !ASExpOf (var)) {
				if (regsfor (ASIndexOf (var)) > 1) {
					texp_main (ASIndexOf (var), MAXREGS - 1, FALSE);
					loadmobile (ASBaseOf (var));
				} else {
					loadmobile (ASBaseOf (var));
					texp_main (ASIndexOf (var), MAXREGS - 2, FALSE);
					gensecondary (I_REV);
				}
			} else {
				if (regsfor (ASExpOf (var)) > 1) {
					texp_main (ASExpOf (var), MAXREGS - 1, FALSE);
					loadmobile (ASBaseOf (var));
				} else {
					loadmobile (ASBaseOf (var));
					texp_main (ASExpOf (var), MAXREGS - 2, FALSE);
					gensecondary (I_REV);
				}
			}
			gensecondary (I_WSUB);		/* frmb: this should be WSUB, not BSUB.. */
		} else if (isdynamicmobilearraytype (type)) {
			const BOOL bsub = TagOf (ASExpOf (var)) == S_TIMES;
			
			if (regsfor (ASExpOf (var)) > 1) {
				texp_main (ASExpOf (var), MAXREGS - 1, FALSE);
				loadmobile (ASBaseOf (var));
			} else {
				loadmobile (ASBaseOf (var));
				texp_main (ASExpOf (var), MAXREGS - 2, FALSE);
				gensecondary (I_REV);
			}
			
			gensecondary (bsub ? I_BSUB : I_WSUB);
		} else {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "storemobile: unexpected MOBILE type");
		}

		genprimary (I_STNL, 0);		/* store */
	} else {
		const int level = NLexLevelOf (var);
		const BOOL nonlocal = loadlex (level);
		const INT32 wsposn = NVOffsetOf (var) + nameoffsetof (level);

		if (isdynmobilearray (var)) {
			/* store to hidden pointer field and unpack */
			genprimary (nonlocal ? I_STNL : I_STL, wsposn + 1);
		} else {
			genprimary (nonlocal ? I_STNL : I_STL, wsposn);
		}
	}
	gencomment0 ("}}}");
	return;
}
/*}}}*/
/*{{{  PUBLIC void storemobilesize (treenode *const var, const int dim)*/
PUBLIC void storemobilesize (treenode *const var, const int dim)
{
#if 0
fprintf (stderr, "gen11: storemobilesize: dim = %d, var =", dim);
printtreenl (stderr, 4, var);
#endif
	gencomment1 ("{{{  storemobilesize (%d)", dim);
	if (TagOf (var) == S_RECORDITEM) {
		/* storing into nested MOBILE */
		loadmobile (ASBaseOf (var));
		genprimary (I_STNL, (ASOffsetOf (var) / bytesperword) + dim);
	} else if (TagOf (var) == S_ARRAYITEM) {
		/* storing into nested array */
		treenode *type = gettype (var);

		if (isdynamicmobilearraytype (type)) {
			const BOOL bsub = TagOf (ASExpOf (var)) == S_TIMES;

			if (regsfor (ASExpOf (var)) > 1) {
				texp_main (ASExpOf (var), MAXREGS - 1, FALSE);
				loadmobile (ASBaseOf (var));
			} else {
				loadmobile (ASBaseOf (var));
				texp_main (ASExpOf (var), MAXREGS - 2, FALSE);
				gensecondary (I_REV);
			}

			gensecondary (bsub ? I_BSUB : I_WSUB);
		} else {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "storemobilesize -- unexpected MOBILE type");
		}
		
		genprimary (I_LDNL, 0);
		genprimary (I_STNL, dim);
	} else {
		const int level = NLexLevelOf (var);
		const INT32 wsposn = NVOffsetOf (var) + nameoffsetof (level);

		gensecondary (I_DUP);

		/* genprimary (nonlocal ? I_STNL : I_STL, wsposn + dim); */
		if (isdynmobilearray (var)) {
			const BOOL nonlocal = loadlex (level);

			genprimary (nonlocal ? I_LDNL : I_LDL, wsposn + 1);
			genprimary (I_STNL, dim);
			loadlex (level);
			genprimary (nonlocal ? I_STNL : I_STL, wsposn + 1 + dim);
		} else {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "storemobilesize -- unexpected MOBILE type");
		}
	}
	gencomment0 ("}}}");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genmobileundefine (treenode *const var)*/
PUBLIC void genmobileundefine (treenode *const var)
{
	gencomment0 ("genmobileundefine {{{");
	if (TagOf (var) == S_RECORDITEM) {
	} else if (TagOf (var) == S_ARRAYITEM) {
		gensecondary (I_NULL);
		storemobile (var);
	} else {
		const int level = NLexLevelOf (var);
		const BOOL nonlocal = loadlex (level);
		const INT32 wsposn = NVOffsetOf (var) + nameoffsetof (level);

		if (!isdynmobilearray (var)) {
			gensecondary (I_NULL);
			if (nonlocal) {
				gensecondary (I_REV);
				genprimary (I_STNL, wsposn);
			} else {
				genprimary (I_STL, wsposn);
			}
		} else if (nonlocal) {
			gensecondary (I_DUP);
			gensecondary (I_NULL);
			gensecondary (I_REV);
			genprimary (I_STNL, wsposn);
			gensecondary (I_DUP);
			gensecondary (I_NULL);
			gensecondary (I_REV);
			genprimary (I_STNL, wsposn + 1);
			loadconstant (0);
			gensecondary (I_REV);
			genprimary (I_STNL, wsposn + 2);
		} else {
			gensecondary (I_NULL);
			genprimary (I_STL, wsposn);
			gensecondary (I_NULL);
			genprimary (I_STL, wsposn + 1);
			loadconstant (0);
			genprimary (I_STL, wsposn + 2);
		}
	}
	gencomment0 ("}}}");
	return;
}
/*}}}*/
/*{{{  PRIVATE void gendynmobilechanclone (treenode *dest, treenode *const source, int regs)*/
/*
 *	clones a MOBILE CHAN TYPE variable
 */
PRIVATE void gendynmobilechanclone (treenode *dest, treenode *const source, int regs)
{
	/*
	treenode *type;
	int type_bytes;
	*/
	/* int refc; */
	/* int lastchan; */

	/* maybe free dest */
	if (TagOf (dest) != S_UNDEFINED) {
		gencondfreedynchantype (dest);
	} else {
		dest = OpOf (dest);
	}

	/*
	type = gettype_main_orig (dest);
	type_bytes = bytesin (type);
	*/
	/* refc = 0; */
	/* lastchan = type_bytes / bytesperword; */

#if 0
fprintf (stderr, "gen11: gendynmobilechanclone(): regs = %d, refc = %d, source = ", regs, refc);
printtreenl (stderr, 4, source);
#endif
	/* increment counter in `source' */
	loadmobile (source);
	if (TagOf (source) == S_ARRAYITEM) {
		/* FIXME: can't be sure of array sub-state yet -- need more undefinedness checking */
		genchecknotnull ();
	}
	gensecondary (I_MT_CLONE);
	storemobile_nochk (dest);
	genmobileunpack (dest, TRUE, TRUE);
	return;
}
/*}}}*/
/*{{{  PRIVATE void gendynmobilebarrierclone (treenode *dest, treenode *const source, int regs)*/
#if 0
/*
 *	clones a MOBILE barrier.          NOT USED: this is now done by gendynmobilebarrierassign()
 */
PRIVATE void gendynmobilebarrierclone (treenode *dest, treenode *const source, int regs)
{
	int refc = 4;

	if (TagOf (dest) != S_UNDEFINED) {
		gencondfreedynbarriertype (dest, TRUE);
	} else {
		dest = OpOf (dest);
	}

	loadmobile (source);
	genprimary (I_LDNL, refc);
	genprimary (I_ADC, 1);
	loadmobile (source);
	genprimary (I_STNL, refc);

	/* copy pointer to "dest" */
	loadmobile (source);
	storemobile (dest);
	return;
}
#endif
/*}}}*/
/*{{{  PRIVATE void gendynmobileclone (treenode *dest, treenode *const source, int regs)*/
PRIVATE void gendynmobileclone (treenode *dest, treenode *const source, int regs)
{
	/* treenode *utype; */
	/* int utypesize; */
	/* int dimcount, idim; */
	const int skiplab = newlab ();

#if 0
fprintf (stderr, "gendynmobileclone: regs = %d, dest (%p) = ", regs, dest);
printtreenl (stderr, 4, dest);
fprintf (stderr, "gendynmobilecline: source (%p) = ", source);
printtreenl (stderr, 4, source);
#endif
	/* maybe free dest */
	if (TagOf (dest) != S_UNDEFINED) {
		gencondfreedynmobile (dest);
	} else {
		dest = OpOf (dest);
	}

	loadmobile_real (source);
	genbranch (I_CJ, skiplab);
	loadmobile_real (source);
	gensecondary (I_MT_CLONE);
	storemobile_nochk (dest);
	genmobileunpack (dest, TRUE, TRUE);
	setlab (skiplab);
	throw_the_result_away ();

	return;
}
/*}}}*/
/*{{{  PRIVATE void gendynmobileprocclone (treenode *dest, treenode *const source, int regs)*/
/*
 *	clones a mobile process.  we let the run-time system do this, because it's messy..
 */
PRIVATE void gendynmobileprocclone (treenode *dest, treenode *const source, int regs)
{
	/* maybe free dest */
	if (TagOf (dest) != S_UNDEFINED) {
		gencondfreedynmobile (dest);
	} else {
		dest = OpOf (dest);
	}

	loadmobile (source);
	if (TagOf (source) == S_UNDEFINED) {
		/* this will cause a run-time error */
		genchecknotnull ();
	}

	genmppclone ();
	storemobile (dest);

	return;
}
/*}}}*/
/*{{{  PUBLIC void gencondfreedynchantype (treenode *nptr)*/
/*
 *	generates code that conditionally frees a mobile channel-type
 */
PUBLIC void gencondfreedynchantype (treenode *nptr)
{
	const int skiplab = newlab ();

	if (TagOf (nptr) == S_CLONE) {
		nptr = OpOf (nptr);
	}

	gencomment0 ("{{{  gencondfreedynchantype ");
	/* Design:
	 *      LDL <ptr-slot>
	 *      CJ :skiplab
	 *      LDL <ptr-slot>
	 *      MT_RELEASE
	 * :skiplab
	 */
	loadmobile_int (nptr, TRUE);
	genbranch (I_CJ, skiplab);
	loadmobile_int (nptr, TRUE);
	gensecondary (I_MT_RELEASE);
	gencleardynchantype (nptr);
	setlab (skiplab);
	gencomment0 ("}}}");
	return;
}
/*}}}*/
/*{{{  PUBLIC void mapcondfreedynchantype (treenode **nptr)*/
/*
 *	maps out a test-and-free for mobile channel-types
 */
PUBLIC void mapcondfreedynchantype (treenode **nptr)
{
	mapexp (nptr);
	if (kroc_chantype_desc && kroc_chantype_uio) {
		mapchantypeop (nptr);
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void gencondfreedynbarriertype (treenode *nptr, const BOOL resign)*/
/*
 *	tests and dynamically frees a MOBILE BARRIER type
 */
PUBLIC void gencondfreedynbarriertype (treenode *nptr, const BOOL resign)
{
	const int skiplab = newlab ();

	loadmobile_int (nptr, TRUE);
	genbranch (I_CJ, skiplab);

	loadmobile_int (nptr, TRUE);
	gensecondary (I_MT_RELEASE);
	gencleardynbarriertype (nptr);

	setlab (skiplab);
	throw_the_result_away ();
	return;
}
/*}}}*/
/*{{{  PUBLIC void gencondfreedynproctype (treenode *nptr)*/
/*
 *	generates code to test-and-release a MOBILE process
 */
PUBLIC void gencondfreedynproctype (treenode *nptr)
{
	const int skiplab = newlab ();
	const int skiplab2 = newlab ();

	gencomment0 ("{{{  gencondfreedynproctype");
	loadmobile_int (nptr, TRUE);
	genbranch (I_CJ, skiplab);

	/* free workspace-map (regardless) */
	loadmobile_int (nptr, TRUE);
	genrmwsmap ();

	/* free barrier */
	loadmobile_int (nptr, TRUE);
	genprimary (I_LDNL, MPP_BARRIER);
	gensecondary (I_MT_RELEASE);

	/* free workspace first */
	loadmobile_int (nptr, TRUE);
	genprimary (I_LDNL, MPP_WSBASE);
	gensecondary (I_MRELEASE);

	/* if vectorspace, free that */
	loadmobile_int (nptr, TRUE);
	genprimary (I_LDNL, MPP_VSBASE);
	gensecondary (I_DUP);
	genbranch (I_CJ, skiplab2);
	gensecondary (I_MRELEASE);
	setlab (skiplab2);
	throw_the_result_away ();

	/* FIXME: mobilespace */

	/* free block itself */
	loadmobile_int (nptr, TRUE);
	gensecondary (I_MRELEASE);

	gencleardynproctype (nptr);

	setlab (skiplab);
	gencomment0 ("}}}");
	return;
}
/*}}}*/
/*{{{  PUBLIC void gencleardynchantype (treenode *const nptr)*/
PUBLIC void gencleardynchantype (treenode *const nptr)
{
	genmobileundefine (nptr);
}
/*}}}*/
/*{{{  PUBLIC void gencleardynbarriertype (treenode *const nptr)*/
PUBLIC void gencleardynbarriertype (treenode *const nptr)
{
	genmobileundefine (nptr);
}
/*}}}*/
/*{{{  PUBLIC void gencleardynproctype (treenode *const nptr)*/
/*
 *	clears out the mobile word associated with a MOBILE process
 */
PUBLIC void gencleardynproctype (treenode *const nptr)
{
	genmobileundefine (nptr);
}
/*}}}*/
/*{{{  PUBLIC void gencleardynarray (treenode *const nptr, const BOOL postout)*/
PUBLIC void gencleardynarray (treenode *const nptr, const BOOL postout)
{
	genmobileundefine (nptr);
}
/*}}}*/
/*{{{  PUBLIC void storehiddentypeof (treenode *tptr, int regs)*/
/*
 *	this stores the typehash of some MOBILE.CHAN (ANYCHANTYPE) or MOBILE.PROC
 */
PUBLIC void storehiddentypeof (treenode *tptr, int regs)
{
	treenode *const type = gettype (tptr);

#if 0
fprintf (stderr, "gen11: storehiddentypeof(): regs=%d, ispointer(tptr)=%d, tptr=", regs, ispointer (tptr));
printtreenl (stderr, 4, tptr);
#endif
	if (TagOf (type) == S_ANYCHANTYPE) {
		const int level = NLexLevelOf (tptr);
		const BOOL nonlocal = loadlex (level);
		const INT32 wsposn = NVOffsetOf (tptr) + nameoffsetof (level);

		genprimary (nonlocal ? I_STNL : I_STL, wsposn + 1);
	} else if (TagOf (type) == S_ANYPROCTYPE) {
		const int level = NLexLevelOf (tptr);
		const BOOL nonlocal = loadlex (level);
		const INT32 wsposn = NVOffsetOf (tptr) + nameoffsetof (level);

		genprimary (nonlocal ? I_STNL : I_STL, wsposn + 1);
	} else {
		geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "storehiddentypeof(): not ANYCHANTYPE/ANYPROCTYPE");
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void loadhiddentypeof (treenode *tptr, int regs)*/
/*
 *	this loads the typehash of some MOBILE.CHAN (ANYCHANTYPE) or MOBILE.PROC (ANYPROCTYPE)
 */
PUBLIC void loadhiddentypeof (treenode *tptr, int regs)
{
	treenode *type;
	
	if (TagOf (tptr) == S_CLONE) {
		tptr = OpOf (tptr);
	}
	type = gettype_main_orig (tptr);

#if 0
fprintf (stderr, "gen11: loadhiddentypeof(): regs=%d, ispointer(tptr)=%d, tptr=", regs, ispointer (tptr));
printtreenl (stderr, 4, tptr);
#endif
	if (TagOf (type) == S_ANYCHANTYPE) {
		const int level = NLexLevelOf (tptr);
		const BOOL nonlocal = loadlex (level);
		const INT32 wsposn = NVOffsetOf (tptr) + nameoffsetof (level);

		genprimary (nonlocal ? I_LDNL : I_LDL, wsposn + 1);
	} else if (TagOf (type) == S_ANYPROCTYPE) {
		const int level = NLexLevelOf (tptr);
		const BOOL nonlocal = loadlex (level);
		const INT32 wsposn = NVOffsetOf (tptr) + nameoffsetof (level);

		genprimary (nonlocal ? I_LDNL : I_LDL, wsposn + 1);
	} else if (isdynmobilechantype (tptr)) {
		if (kroc_chantype_desc) {
#if 0
fprintf (stderr, "gen11: loadhiddentypeof(): loading pointer for kroc_chantype_desc: type =");
printtreenl (stderr, 4, type);
#endif
			if (TagOf (type) == N_TYPEDECL) {
#if 0
fprintf (stderr, "gen11: loadhiddentypeof(): loading pointer for kroc_chantype_desc: MTDLabOf (NTypeOf (type)) = %d, NTypeOf (type) =", MTDLabOf (NTypeOf (type)));
printtreenl (stderr, 4, NTypeOf (type));
#endif
				type = NTypeOf (type);
			}
			genloadlabptr (MTDLabOf (type), NOLAB, "MTD PTR");
		} else {
#if 0
fprintf (stderr, "gen11: loadhiddentypeof(): loading typehash: 0x%8.8x of type =", typehash (type));
printtreenl (stderr, 4, type);
#endif
			loadconstant (typehash (type));
		}
	} else if (isdynmobileproctype (tptr)) {
#if 0
fprintf (stderr, "gen11: loadhiddentypeof(): dynmobileproctype: type =");
printtreenl (stderr, 4, type);
#endif
		loadconstant (typehash (NTypeOf (MTypeOf (type))));
	} else {
#if 0
fprintf (stderr, "gen11: loadhiddentypeof(): non-mobile type:");
printtreenl (stderr, 4, type);
#endif
		loadconstant (typehash (type));
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void loadhiddentypeptr (treenode *tptr, int regs)*/
/*
 *	loads a pointer to the typehash of some MOBILE.CHAN (ANYCHANTYPE)
 */
PUBLIC void loadhiddentypeptr (treenode *tptr, int regs)
{
	treenode *type;
	
	if (TagOf (tptr) == S_UNDEFINED) {
		tptr = OpOf (tptr);
	}
	type = gettype (tptr);

#if 0
fprintf (stderr, "gen11: loadhiddentypeptr(): regs=%d, ispointer(tptr)=%d, tptr=", regs, ispointer (tptr));
printtreenl (stderr, 4, tptr);
#endif
	if (TagOf (type) == S_ANYCHANTYPE) {
		switch (TagOf (tptr)) {
		case N_DECL:
		case N_PARAM:
		case N_RESULTPARAM:
			{
				const int level = NLexLevelOf (tptr);
				const BOOL nonlocal = loadlex (level);
				const INT32 wsposn = NVOffsetOf (tptr) + nameoffsetof (level);

				genprimary (nonlocal ? I_LDNLP : I_LDLP, wsposn + 1);
			}
			break;
		default:
			geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "loadhiddentypeptr(): not implemented yet!");
			break;
		}
	} else if (TagOf (type) == S_ANYPROCTYPE) {
		switch (TagOf (tptr)) {
		case N_DECL:
		case N_PARAM:
		case N_RESULTPARAM:
			{
				const int level = NLexLevelOf (tptr);
				const BOOL nonlocal = loadlex (level);
				const INT32 wsposn = NVOffsetOf (tptr) + nameoffsetof (level);

				genprimary (nonlocal ? I_LDNLP : I_LDLP, wsposn + 1);
			}
			break;
		default:
			geninternal_is (GEN_ERROR_IN_ROUTINE, 2, "loadhiddentypeptr(): not implemented yet!");
			break;
		}
	} else {
		geninternal_is (GEN_ERROR_IN_ROUTINE, 3, "loadhiddentypeptr(): not ANYCHANTYPE/ANYPROCTYPE");
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void mapanychantypeassign (treenode **const dest, treenode **const src, int regs)*/
/*
 *	maps assignment from/to a MOBILE.CHAN
 */
PRIVATE void mapanychantypeassign (treenode **const dest, treenode **const src, int regs)
{
	int dtype = ntypeof (*dest);
	/* int stype = ntypeof (*src); */

	if (dtype == S_ANYCHANTYPE) {
		/* easy, don't need to worry about src too much */
		mapexpopd (P_EXP, (TagOf (*src) == S_UNDEFINED) ? OpAddr (*src) : src);
		mapstoreinopd (P_EXP, (TagOf (*dest) == S_UNDEFINED) ? OpAddr (*dest) : dest);
	} else {
		/* need to check that src is of the right type, STOP otherwise -- nothing extra needed..? */
		mapexpopd (P_EXP, (TagOf (*src) == S_UNDEFINED) ? OpAddr (*src) : src);
		mapstoreinopd (P_EXP, (TagOf (*dest) == S_UNDEFINED) ? OpAddr (*dest) : dest);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void genanychantypeassign (treenode *const dest, treenode *const src, int regs)*/
/*
 *	generates code to assign from/to a MOBILE.CHAN
 */
PRIVATE void genanychantypeassign (treenode *const dest, treenode *const src, int regs, treenode *action)
{
	const BOOL dutagged = (TagOf (dest) == S_UNDEFINED);
	const BOOL sutagged = (TagOf (src) == S_UNDEFINED);
	treenode *rdest = (dutagged ? OpOf (dest) : dest);
	treenode *rsrc = (sutagged ? OpOf (src) : src);
	const int dtype = ntypeof (rdest);
	const int stype = ntypeof (rsrc);

#if 0
fprintf (stderr, "gen11: genanychantypeassign(): regs = %d, dest =", regs);
printtreenl (stderr, 4, dest);
fprintf (stderr, "     : genanychantypeassign(): src =");
printtreenl (stderr, 4, src);
fprintf (stderr, "     : genanychantypeassign(): gettype_main_orig (rsrc) =");
printtreenl (stderr, 4, gettype_main_orig (rsrc));
#endif

	gencondfreedynchantype (rdest);

	if (dtype == S_ANYCHANTYPE) {
		/* easy, assignment to MOBILE.CHAN */
		if (stype == S_ANYCHANTYPE) {
			loadhiddentypeof (rsrc, regs);
		} else {
			loadconstant (typehash (gettype_main_orig (rsrc)));
		}
		storehiddentypeof (rdest, (regs == MANY_REGS) ? (MAXREGS - 1) : (regs - 1));
	} else {
		int skiplab = newlab ();

		if (action) {
			new_occam_line (action, TRUE, TRUE, FALSE);
		}

		/* need to check it's the correct type */
		if (stype != S_ANYCHANTYPE) {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "genanychantypeassign(): source not ANYCHANTYPE");
		}
		loadhiddentypeof (rsrc, regs);
		loadconstant (typehash (gettype_main_orig (rdest)));
		gensecondary (I_DIFF);
		genbranch (I_CJ, skiplab);
		gensecondary (I_SETERR);			/* generate error */
		setlab (skiplab);
	}
	/* do pointer */
	loadmobile (rsrc);
	storemobile (rdest);
	gencleardynchantype (rsrc);

	return;
}
/*}}}*/
/*{{{  PRIVATE void mapanyproctypeassign (treenode **const dest, treenode **const src, int regs)*/
/*
 *	maps assignment from/to a MOBILE.PROC
 */
PRIVATE void mapanyproctypeassign (treenode **const dest, treenode **const src, int regs)
{
	int dtype = ntypeof (*dest);
	/* int stype = ntypeof (*src); */

	if (dtype == S_ANYPROCTYPE) {
		/* easy, don't need to worry about src too much */
		mapexpopd (P_EXP, (TagOf (*src) == S_UNDEFINED) ? OpAddr (*src) : src);
		mapstoreinopd (P_EXP, (TagOf (*dest) == S_UNDEFINED) ? OpAddr (*dest) : dest);
	} else {
		/* need to check that src is of the right type, STOP otherwise -- nothing extra needed..? */
		mapexpopd (P_EXP, (TagOf (*src) == S_UNDEFINED) ? OpAddr (*src) : src);
		mapstoreinopd (P_EXP, (TagOf (*dest) == S_UNDEFINED) ? OpAddr (*dest) : dest);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void genanyproctypeassign (treenode *const dest, treenode *const src, int regs)*/
/*
 *	generates code to assign from/to a MOBILE.PROC
 */
PRIVATE void genanyproctypeassign (treenode *const dest, treenode *const src, int regs)
{
	const BOOL dutagged = (TagOf (dest) == S_UNDEFINED);
	const BOOL sutagged = (TagOf (src) == S_UNDEFINED);
	treenode *rdest = (dutagged ? OpOf (dest) : dest);
	treenode *rsrc = (sutagged ? OpOf (src) : src);
	const int dtype = ntypeof (rdest);
	const int stype = ntypeof (rsrc);

#if 0
fprintf (stderr, "gen11: genanyproctypeassign(): regs = %d, dest =", regs);
printtreenl (stderr, 4, dest);
fprintf (stderr, "     : genanyproctypeassign(): src =");
printtreenl (stderr, 4, src);
fprintf (stderr, "     : genanyproctypeassign(): gettype_main_orig (rsrc) =");
printtreenl (stderr, 4, gettype_main_orig (rsrc));
#endif

	gencondfreedynchantype (rdest);

	if (dtype == S_ANYPROCTYPE) {
		/* easy, assignment to MOBILE.PROC */
		if (stype == S_ANYPROCTYPE) {
			loadhiddentypeof (rsrc, regs);
		} else {
			loadconstant (typehash (gettype_main_orig (rsrc)));
		}
		storehiddentypeof (rdest, (regs == MANY_REGS) ? (MAXREGS - 1) : (regs - 1));
	} else {
		int skiplab = newlab ();

		/* need to check it's the correct type */
		if (stype != S_ANYPROCTYPE) {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "genanyproctypeassign(): source not ANYPROCTYPE");
		}
		loadhiddentypeof (rsrc, regs);
		loadconstant (typehash (gettype_main_orig (rdest)));
		gensecondary (I_DIFF);
		genbranch (I_CJ, skiplab);
		gensecondary (I_SETERR);			/* generate error */
		setlab (skiplab);
	}
	/* do pointer */
	loadmobile (rsrc);
	storemobile (rdest);
	gencleardynchantype (rsrc);

	return;
}
/*}}}*/
/*{{{  PUBLIC void genaddrof (source) */
PUBLIC void genaddrof (treenode *tptr)
{
	treenode *nptr = OpOf (tptr);
	BOOL ok = ismobile (nptr);
	
	ok = ok && (!isdynmobilechantype (nptr));
	ok = ok && (!isdynmobileproctype (nptr));
	ok = ok && (!isdynmobilebarrier (nptr));

	if (ok && (TagOf (tptr) == S_ADDROF) ) {
		gencomment0 ("{{{  ADDROF");
		loadmobile (nptr);
		gencomment0 ("}}}");
	} else if (ok && (TagOf (tptr) == S_HWADDROF) && isdynmobilearray (nptr)) {
		const int skiplab = newlab ();

		gencomment0 ("{{{  HWADDROF");
		loadmobile_real (nptr);
		gensecondary (I_DUP);
		genbranch (I_CJ, skiplab);
		genprimary (I_LDNL, 1 + dynmobiledimensioncount (nptr));
		setlab (skiplab);
		gencomment0 ("}}}");
	} else {
		generr (GEN_ADDROF_BAD_TYPE);
	}
}

/*}}}*/
#endif
/*{{{  PUBLIC void loadname (nptr, w)*/
PUBLIC void loadname (treenode *const nptr, const INT32 w)
{
#if 0
fprintf (stderr, "gen11: loadname: issimplechan(nptr) = %d, ispointer(nptr) = %d, w = %d, nptr = ", issimplechan(nptr), ispointer(nptr), w);
printtreenl (stderr, 4, nptr);
#endif
	if (chanaspointer && issimplechan (nptr)) {
		/* Very special case */
		movename (nptr, w, I_LDLP, I_LDNLP);
	} else {
		if (ispointer (nptr)) {
			movename (nptr, 0, I_LDL, I_LDNL);
			genprimary (I_LDNL, w);	/* should only be called on word-sized objects */
		} else {
			movename (nptr, w, I_LDL, I_LDNL);
		}
	}
}

/*}}}*/
/*{{{  PUBLIC void storeinname (nptr, w)*/
PUBLIC void storeinname (treenode *const nptr, const INT32 w)
{
	checkerror ();
#if 0
fprintf (stderr, "gen11: storeinname: w = %d, nptr = ", w);
printtreenl (stderr, 4, nptr);
#endif
	movename (nptr, w, I_STL, I_STNL);
}

/*}}}*/
/*{{{  PUBLIC void zero_local_var*/
PUBLIC void zero_local_var (treenode * const nptr)
{
	loadconstant (0);
	storeinname (nptr, 0);
	gencomment0 ("INIT");
}

/*}}}*/
/*{{{  PRIVATE int return_subscript*/
PRIVATE int return_subscript (treenode * const tptr)
{
	treenode *const base_type = basetype_tree (gettype (tptr));
	int b;

#ifdef MOBILES
	if (TagOf (base_type) == S_MOBILE) {
		b = bytesperword;
	} else
#endif
	{
#ifdef OCCAM2_5
		b = (TagOf (base_type) == S_RECORD) ? bytesperword : (int) bytesin (base_type);
#else
		b = (int) bytesin (base_type);
#endif
	}

#if 0
fprintf (stderr, "gen11: return_subscript() b=%d, base_type=", b);
printtreenl (stderr, 4, base_type);
#endif
	if (can_use_wsubdb (b, bytesperword, tptr)) {
		return I_WSUBDB;
	} else if (b >= bytesperword) {
		return I_WSUB;
	} else if (use_shortintops && b == 2) {
		return I_SSUB;	/* T9000 shorts 17/7/91 */
	} else {
		return I_BSUB;
	}
}

/*}}}*/
/*{{{  PRIVATE void add_scaled_offset*/
PRIVATE void add_scaled_offset (treenode * const tptr, const INT32 offset)
{
	const int isubscript = return_subscript (tptr);

	if (isubscript == I_BSUB) {
		tbyteoffset (offset);
	} else if (isubscript == I_SSUB) {	/* T9000 shorts 17/7/91 */
		tbyteoffset ((BIT32) offset * 2);
	} else {
		genprimary (I_LDNLP, offset);
	}
}

/*}}}*/
/*{{{  PUBLIC void loadnamepointer (treenode *nptr, const INT32 w)*/
/*
 *	loads a pointer to a name
 */
PUBLIC void loadnamepointer (treenode *nptr, const INT32 w)
{
	treenode *name = NULL;
#if 0
printf ("loadnamepointer: w = %d, nptr = ", w);
printtreenl (stdout, 4, nptr);
#endif
	BETRACE ("gen11: loadnamepointer (enter): nptr=%p, w=%d", nptr, (int)w);
	if ((TagOf (nptr) == N_DECL) && (nodetypeoftag (TagOf (NTypeOf (nptr))) == TYPENODE) && (TypeAttrOf (NTypeOf (nptr)) & TypeAttr_placed)) {
		if (NDeclOf (nptr) && (TagOf (DBodyOf (NDeclOf (nptr))) == S_PLACE)) {
			treenode *dval = DValOf (DBodyOf (NDeclOf (nptr)));

#if 0
fprintf (stderr, "placed something or other at constant address?  dval = ");
printtreenl (stderr, 4, dval);
#endif
			if (TagOf (dval) == S_CONSTEXP) {
				loadconstant (LoValOf (dval));
				BETRACE ("loadnamepointer (leave)");
				return;
			}
		}
		movename (nptr, 0, I_LDL, I_LDNL);
		genprimary (I_LDNLP, w);
		BETRACE ("loadnamepointer (leave)");
		return;
	}

	if (TagOf (nptr) == N_VALABBR || TagOf (nptr) == N_VALRETYPE) {
		treenode *cp = DValOf (NDeclOf (nptr));
		name = nptr;
		if ((TagOf (cp) == S_STRING) || (TagOf (cp) == S_CONSTCONSTRUCTOR)) {
			nptr = cp;
		}
	}

	if ((TagOf (nptr) == S_STRING) || (TagOf (nptr) == S_CONSTCONSTRUCTOR)) {
		/*{{{  load a pointer to a constant table */
		int label = NO_LABEL;
		if (WLengthOf (CTValOf (nptr)) == 0) {
			/* optimisation for null strings */
			genprimary (I_LDLP, 0);
		} else {
			const int constlab = CTLabelOf (nptr);
			label = newlab ();
			genloadlabptr (constlab, label, (name == NULL) ? NULL : WNameOf (NNameOf (name)));
			label = NO_LABEL;	/* suppress setlab below */
		}
		if (label != NO_LABEL) {
			setlab (label);
		}
		add_scaled_offset (nptr, w);	/* Added for bug 1071 13/12/90 */
		/*}}} */
	} else if (ispointer (nptr)) {
		movename (nptr, 0, I_LDL, I_LDNL);
		genprimary (I_LDNLP, w);
		/*if (w != 0) printf("loadnamepointer: pointer; genlocn: %8X, w=%ld\n", genlocn, w); */
	} else {
		movename (nptr, w, I_LDLP, I_LDNLP);
		endianadjustpointer (nptr);
	}
	BETRACE ("gen11: loadnamepointer (leave)");
}

/*}}}*/
/*{{{  load/store/loadptr       arrays*/
/*{{{  PRIVATE INT32 loadarraypointer*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  loadarraypointer loads a pointer to the array element 'tptr' into Areg,
 *                   using at most regs registers.
 *                   A (word) offset from the loaded pointer to the element
 *                   is returned.
 *
 *****************************************************************************/
/*}}}*/
/* A transformed subscript tree */
PRIVATE INT32 loadarraypointer (treenode * const tptr, const INT32 word, const int regs)
{
	const BOOL segmentitem = (TagOf (tptr) == S_SEGMENTITEM);
	const int isubscript = return_subscript (tptr);
	treenode *nptr = nameof (tptr);
	treenode *subscriptexp = segmentitem ? SSubscriptExpOf (tptr) : ASExpOf (tptr);
	INT32 offset = (segmentitem ? SOffsetOf (tptr) : ASOffsetOf (tptr)) + word;

	BETRACE ("gen11: loadarraypointer (enter): tptr=%p, word=%d, regs=%d", tptr, (int)word, regs);
#if 0
fprintf (stderr, "loadarraypointer: word = %d, regs = %d, tptr = ", word, regs);
printtreenl (stderr, 4, tptr);
fprintf (stderr, "  segmentitem = %d, isubscript = %d, nptr = ", segmentitem, isubscript);
printtreenl (stderr, 4, nptr);
fprintf (stderr, "  offset = %d, subscriptexp = ", offset);
printtreenl (stderr, 4, subscriptexp);
#endif
	if (directload (P_EXP, tptr, be_lexlevel)) {
		/*{{{  load local pointer */
#if 0
fprintf (stderr, "loadarraypointer: directload = TRUE, calling loadnamepointer (nptr, %d), nptr =", (int)offset);
printtreenl (stderr, 4, nptr);
#endif
		loadnamepointer (nptr, offset);
		offset = 0;
		/*}}} */
	} else {
		/*{{{  load a pointer */
		if (subscriptexp != NULL) {
			/*{{{  perform subscript calculation */
#if 0
fprintf (stderr, "loadarraypointer: !directload, calling loadnamepointer (nptr, %d), nptr =", (int)offset);
printtreenl (stderr, 4, nptr);
#endif
			/* if tptr is an ARRAYITEM of RECORDSUB, and field is dynamic MOBILE, dereference "offset" here */
#ifdef MOBILES
			if (TagOf (tptr) == S_ARRAYITEM) {
				treenode const *base = ASBaseOf (tptr);

				if (TagOf (ASBaseOf (tptr)) == S_RECORDSUB) {
					treenode *rfield = ASIndexOf (base);

					texp (subscriptexp, regs);
					loadnamepointer (nptr, 0);
					gensecondary (isubscript);

					if (isdynmobilearray (rfield)) {
						genprimary (I_LDNL, (offset / bytesperword));
						offset = 0;
					}
				} else if (TagOf (base) == S_ARRAYITEM) {
					texp (subscriptexp, regs);
					loadmobile (ASBaseOf (tptr));
					gensecondary (isubscript);
				} else {
					texp (subscriptexp, regs);
					loadnamepointer (nptr, 0);
					gensecondary (isubscript);
				}
			} else
#endif
			{
				texp (subscriptexp, regs);
				loadnamepointer (nptr, 0);
				gensecondary (isubscript);
			}
			/*}}} */
		} else {
			loadnamepointer (nptr, 0);
		}
#ifdef MOBILES
		{
			/* if the underlying thing is MOBILE, load a pointer to it (LDNL <offset>) */
			treenode *const type = gettype (tptr);

			if (!segmentitem && (TagOf (type) == S_MOBILE)) {
#if 0
fprintf (stderr, "loadarraypointer (load a MOBILE pointer, generating 2nd load..): type = ");
printtreenl (stderr, 4, type);
#endif
				genprimary (I_LDNL, offset);
				offset = 0;
				if (isdynmobilearray (tptr)) {
					int skiplab = newlab ();
					
					/* load real data pointer */

					gensecondary (I_DUP);
					genbranch (I_CJ, skiplab);

					genprimary (I_LDNL, 0);
					gensecondary (I_DUP);

					setlab (skiplab);
					gensecondary (I_POP);
				}
			}
		}
#endif
		/*}}} */
	}
	if (isubscript == I_BSUB) {
#if 0
fprintf (stderr, "loadarraypointer (adding in BYTE offset now = %d)\n", offset);
#endif
		/*{{{  add in the offset now */
		tbyteoffset (offset);
		offset = 0;
		/*}}} */
	}
	/* gencomment1 ("loadarraypointer() returning %d", (int)offset); */
	BETRACE ("gen11: loadarraypointer (leave): int %d", (int)offset);
	return (offset);
}

/*}}}*/
/*{{{  PRIVATE void movearrayitem*/
PRIVATE void movearrayitem (treenode * const tptr, const INT32 word, const int regs, const int dirn, const int inonlocal)
{
	treenode *nptr = nameof (tptr);
	int type = basetype (gettype (nptr));
#ifdef OCCAM2_5
	if (type == S_RECORD) {
		type = basetype (gettype (tptr));
	}
#endif

/*printf("movearrayitem: type is %s\n", itagstring(type));*/

	BETRACE("gen11: movearrayitem (enter): tptr=%p, word=%d, regs=%d, dirn=%d, inonlocal=%d\n", tptr, (int)word, regs, dirn, inonlocal);

#if 0
fprintf (stderr, "gen11: movearrayitem (word = %d, regs = %d, dirn = %s), tptr = ", (int)word, regs, (dirn == MOVEDIRN_LOADPTR) ? "LOADPTR" : ((dirn == MOVEDIRN_LOAD) ? "LOAD" : "??"));
printtreenl (stderr, 4, tptr);
#endif
	if ((dirn != MOVEDIRN_LOADPTR) && !use_shortintops &&	/* T9000 shorts 17/7/91 */
	    ((type == S_INT16) || (type == S_UINT16)) && (targetintsize != S_INT16)) {
		badtag (genlocn, type, "movearrayitem");
	} else {
		if (((dirn & (MOVEDIRN_LOADEXT | MOVEDIRN_LOAD)) != 0) && directload (P_EXP, tptr, be_lexlevel)) {
#if 0
fprintf (stderr, "gen11: movearrayitem: doing loadname (nptr, %d + %d)\n", ASOffsetOf (tptr), word);
#endif
			loadname (nptr, ASOffsetOf (tptr) + word);
		} else if ((dirn == MOVEDIRN_STORE) && directstore (P_EXP, tptr, be_lexlevel)) {
			storeinname (nptr, ASOffsetOf (tptr) + word);
		} else {
			/*{{{  ldptr item; load/store */
			INT32 offset = loadarraypointer (tptr, word, regs);

#if 0
fprintf (stderr, "gen11: movearrayitem: after loadarraypointer(), offset = %d\n", offset);
#endif
			if ((dirn == MOVEDIRN_LOADPTR) || (bytesinscalar (type) < bytesperword)) {
				add_scaled_offset (tptr, offset);
				offset = 0;
			}

			if (dirn != MOVEDIRN_LOADPTR) {
				movepointer (dirn, inonlocal, offset, type, NULL, is_devaccess (nptr));
			}
			/*}}} */
		}
	}
	BETRACE ("gen11: movearrayitem (leave)");
}

/*}}}*/
/*}}}*/
/*{{{  load/store/loadptr       elements*/
/*{{{  PRIVATE void moveelement (tptr, word, dirn, regs)*/
/*
 *	Load or store the element represented by tptr.
 *	If dirn is MOVEDIRN_LOAD load the element into Areg.
 *	If dirn is MOVEDIRN_LOADEXT load the element into Areg and sign-extend.
 *	if dirn is MOVEDIRN_STORE store Areg into the element.
 *	If dirn is MOVEDIRN_LOADPTR load a pointer to the element into Areg.
 *	If the element is long, load/store the word'th word of it.
 *
 *	modified 5/11/90 by CO'N for bug 738, to work with word != 0 when
 *	dirn == MOVEDIRN_LOADPTR. (IE until now, it has always assumed that the offset
 *	is zero when loading a pointer.
 *
 *	The idea is that it simply loads a pointer to the same data which it
 *	would have loaded if dirn was MOVEDIRN_LOAD
 */
PRIVATE void moveelement (treenode *tptr, const INT32 word, const int dirn, const int regs)
{
	int ilocal, inonlocal;
	int tag = TagOf (tptr);
	int rtag = tag;
	BOOL isshort = TagOf (tptr) != N_PROCDEF ? isshorttype (ntypeof (tptr)) : FALSE;
	BOOL shortint = isshort ? isshortint (ntypeof (tptr)) : FALSE;
	BOOL need_signextend = (dirn == MOVEDIRN_LOADEXT) && use_shortintops && shortint;	/* bug 1345 23/7/91 */

	BETRACE ("gen11: moveelement (enter): tptr=%p, word=%d, dirn=%d, regs=%d", tptr, (int)word, dirn, regs);
	/*{{{  set up ilocal and inonlocal */
	switch (dirn) {
	case MOVEDIRN_LOAD:
	case MOVEDIRN_LOADEXT:
		if (chanaspointer && issimplechan (tptr) && !(1 && (TypeAttrOf (NTypeOf (tptr)) & TypeAttr_placed))) {
			/* Very special case, unless placed channel */
			ilocal = I_LDLP;
			inonlocal = I_LDNLP;
		} else {
			ilocal = I_LDL;
			inonlocal = I_LDNL;
		}
		break;
	case MOVEDIRN_STORE:
		ilocal = I_STL;
		inonlocal = I_STNL;
		break;
	case MOVEDIRN_LOADPTR:
		ilocal = I_LDLP;
		inonlocal = I_LDNLP;
		break;
	default:
		ilocal = I_LDL;
		inonlocal = I_LDNL;	/* to shut up warning messages */
		geninternal_i (GEN_BAD_MOVEELEMENT_DIRN, dirn);
	}
	/*}}} */

#ifdef MOBILES
	if (tag == S_UNDEFINED) {
		tptr = OpOf (tptr);
		tag = TagOf (tptr);
	} else
#endif
	if (tag == S_FNACTUALRESULT) {
		tptr = HExpOf (tptr);
		tag = TagOf (tptr);
	}

	switch (tag) {
		/*{{{  cases */
		/*{{{  decl repl param var.abbrev retype preevaltemp */
	case N_DECL:
	case N_REPL:
	case N_PARAM:
	case N_VALPARAM:
	case N_RESULTPARAM:
	case N_ABBR:
	case N_RETYPE:
	case T_PREEVALTEMP:
	case T_TEMP:		/* code commoned-ed up 5/11/90 */
	case S_FNFORMALRESULT:	/* code commoned-up by CON - 15/11/93 */
		{
			const int type = ntypeof (tptr);
			/* T_TEMP code commoned-ed up 5/11/90 */
			if ((tag == T_TEMP) && ((dirn & (MOVEDIRN_LOAD | MOVEDIRN_LOADEXT)) != 0) &&
					/*!use_shortintops && *//* T9000 shorts 17/7/91 */
					isshortint (type)) {
				simplify (P_EXP, tptr);	/* Move the value to a temporary */
			}
			if (ispointer (tptr)) {
#if 0
fprintf (stderr, "gen11: moveelement (N_.. ispointer): word = %d, dirn = %d, regs = %d, ilocal = %d, inonlocal = %d, tptr = ", word, dirn, regs, ilocal, inonlocal);
printtreenl (stderr, 4, tptr);
#endif
				movename (tptr, 0, I_LDL, I_LDNL);
				movepointer (dirn, inonlocal, word, type, &need_signextend, is_devaccess (tptr));
			} else {
#if 0
fprintf (stderr, "gen11: moveelement (N_.. !ispointer): word = %d, dirn = %d, regs = %d, ilocal = %d, inonlocal = %d, tptr = ", word, dirn, regs, ilocal, inonlocal);
printtreenl (stderr, 4, tptr);
#endif
				if (dirn == MOVEDIRN_STORE) {
					checkerror ();
				}
				movename (tptr, word, ilocal, inonlocal);
				if ((dirn == MOVEDIRN_LOADPTR) && !isplaced (tptr) && (rtag != S_FNACTUALRESULT)) {
					endianadjustpointer (tptr);
				}
			}
		}
		break;
		/*}}} */
		/*{{{  N_VALABBR N_VALRETYPE */
	case N_VALABBR:
	case N_VALRETYPE:
		/* Could be a constant table. If so, we can only load a pointer */
		{
			treenode *cp = DValOf (NDeclOf (tptr));
			const int t = TagOf (cp);
			if (t == S_STRING || t == S_CONSTEXP || t == S_CONSTCONSTRUCTOR) {
				/*{{{  its a constant table, load a pointer to it */
				moveelement (cp, word, dirn, regs);
				need_signextend = FALSE;
				/*}}} */
			} else if (ispointer (tptr)) {	/* if VAL abbr, must be greater than word sized */
				movename (tptr, 0, I_LDL, I_LDNL);
/* if (dirn != MOVEDIRN_LOADPTR) *//* test removed for bug 738 5/11/90 */
				genprimary (inonlocal, word);
			} else {
				movename (tptr, word, ilocal, inonlocal);
				if (dirn == MOVEDIRN_LOADPTR) {
					endianadjustpointer (tptr);
				}
			}
			break;
		}
		/*}}} */
		/*{{{  S_ARRAYITEM S_SEGMENTITEM */
	case S_ARRAYITEM:
	case S_SEGMENTITEM:
	case S_RECORDSUB:
	case S_RECORDITEM:
		/* tnestedsegments(tptr, regs); */

		movearrayitem (tptr, word, regs, dirn, inonlocal);
		need_signextend = FALSE;

		break;
		/*}}} */
		/*{{{  S_STRING S_CONSTCONSTRUCTOR N_LABELDEF, and PROCs */
	case S_STRING:
	case S_CONSTCONSTRUCTOR:
	case N_LABELDEF:
	case N_PROCDEF:
	case N_MPROCDECL:
	case N_LIBPROCDEF:	/* bug TS/1240 14/04/92 */
	case N_LIBMPROCDECL:
	case N_LFUNCDEF:
	case N_SFUNCDEF:
	case N_LIBFUNCDEF:
		{
			const BOOL consttable = (tag == S_STRING) || (tag == S_CONSTCONSTRUCTOR);
			if (consttable && (WLengthOf (CTValOf (tptr)) == 0)) {
				/* optimisation for null strings */
				genprimary (I_LDLP, 0);
			} else {
				const int constlab = (tag == N_LABELDEF) ? (int) NVOffsetOf (tptr)
					: consttable ? CTLabelOf (tptr)
					: NPLabelOf (tptr);
				const int l = newlab ();
				genloadlabptr (constlab, l, NULL);
			}
			/* I suppose that here we should add an offset if 'word' is
			   non-zero - bug 738 5/11/90.  */
			/* type(S_STRING) and type(S_CONSTCONSTRUCTOR) etc are S_ARRAY,
			   so we always load a word offset:
			   (I would guess that this never happens, since it seems wrong
			   for S_STRING etc, but at least it is consistent with
			   what is done for names, etc. CO'N 5/11/90) */
			if ((dirn & (MOVEDIRN_LOAD | MOVEDIRN_LOADPTR | MOVEDIRN_LOADEXT)) != 0) {
				genprimary (inonlocal, word);
			} else {
				geninternal_i (GEN_BAD_CONSTANT_LOAD, dirn);
			}
		}
		break;
		/*}}} */
		/*{{{  S_FNFORMALRESULT */
#if 0
	case S_FNFORMALRESULT:
		/* A function result pointer is effectively an abbreviation to the
		   result destination */
		/*genprimary(I_LDL, HOffsetOf(tptr)); */
		movename (tptr, 0, I_LDL, I_LDNL);
		/*gencomment0("formalresult"); */
		movepointer (dirn, inonlocal, word, TagOf (HExpOf (tptr)), &need_signextend, FALSE);
		break;
#endif
		/*}}} */
		/*{{{  S_CONSTEXP */
	case S_CONSTEXP:
		if (isinconstanttable (tptr)) {
			texp_constanttable (tptr, inonlocal, (int) word);
		} else if ((dirn & (MOVEDIRN_LOAD | MOVEDIRN_LOADEXT)) != 0) {
			loadconstant (wordof (tptr, (int) word));
		} else {
			geninternal_i (GEN_BAD_MOVEELEMENT_DIRN, dirn);
		}
		need_signextend = FALSE;
		break;
		/*}}} */
		/*{{{  S_TYPEHASHOF*/
	case S_TYPEHASHOF:
		if (dirn & (MOVEDIRN_LOAD | MOVEDIRN_LOADEXT)) {
			loadconstant (typehash (OpOf (tptr)));
		} else {
			geninternal_i (GEN_BAD_MOVEELEMENT_DIRN, dirn);
		}
		need_signextend = FALSE;
		break;
		/*}}}*/
	default:
		badtag (genlocn, tag, "moveelement");
		/*}}} */
	}

	if (need_signextend) {
		gensecondary (I_XSWORD);	/* bug 1345 23/7/91 */
	}

	BETRACE ("gen11: moveelement (leave)");
	return;
}

/*}}}*/
/*{{{  PUBLIC void loadelement(tptr, word, regs)*/
PUBLIC void loadelement (treenode * const tptr, const INT32 word, const int regs, const BOOL signextend_result)
{
	BETRACE ("gen11: loadelement (enter): tptr=%p, word=%d, regs=%d, signextend_result=%d", tptr, (int)word, regs, signextend_result);
#if 0
fprintf (stderr, "gen11: loadelement: word = %d, regs = %d, signextend_result = %d, tptr =", word, regs, signextend_result);
printtreenl (stderr, 4, tptr);
#endif
#ifdef MOBILES
	/* handle loading of dynamic MOBILE CHAN TYPE vars here */
	if ((TagOf (tptr) == S_RECORDITEM) && isdynmobilechantype (ASBaseOf (tptr))) {
#if 0
fprintf (stderr, "gen11: loadelement: RECORDITEM: ASBaseOf(tptr) is a dynamic mobile channel type.  loading mobile and pointer.  ASOffsetOf (tptr) = %d\n", ASOffsetOf (tptr));
#endif
		loadmobile (ASBaseOf (tptr));			/* loads a pointer to the structure base */
		genprimary (I_LDNLP, ASOffsetOf (tptr));
	} else if ((TagOf (tptr) == S_RECORDITEM) && ismobile (ASBaseOf (tptr)) && (isdynmobilearray (ASIndexOf (tptr)) || isdynmobilechantype (ASIndexOf (tptr)))) {
#if 0
fprintf (stderr, "gen11: loadelement: RECORDITEM: ASBaseOf(tptr) is mobile, index is a dynmobilearray or dynmobilechantype.  loading mobile and non-local\n");
#endif
		/* loading of some nested MOBILE */
		loadmobile (ASBaseOf (tptr));
		genprimary (I_LDNL, (ASOffsetOf (tptr) / bytesperword) + word);
	} else if ((TagOf (tptr) == S_ARRAYITEM) && ismobile (ASBaseOf (tptr)) && (TagOf (ASBaseOf (tptr)) == S_ARRAYITEM)) {
		/* nested MOBILE handling */
		int isubscript = return_subscript (tptr);

		loadmobile (ASBaseOf (tptr));
		/* have local 0 for temporary */
		genprimary (I_STL, 0);

		/* ASExpOf (tptr) should give us an index into the mobile array */
#if 0
fprintf (stderr, "gen11: loadelement.  loadad MOBILE base, isubscript = %d, ASExpOf (tptr) = ", isubscript);
printtreenl (stderr, 4, ASExpOf (tptr));
#endif
		if (ASExpOf (tptr)) {
			texpopd (P_EXP, ASExpOf (tptr), regs);
			genprimary (I_LDL, 0);
			gensecondary (isubscript);
			/* have the address, now actually need to load the element */
			if (isubscript == I_BSUB) {
				gensecondary (I_LB);
			} else if (isubscript == I_WSUB) {
				genprimary (I_LDNL, 0);
			} else {
				geninternal_is (GEN_ERROR_IN_ROUTINE, 0, "loadelement");
			}
		}
	} else
#endif
	{
		moveelement (tptr, word, signextend_result ? MOVEDIRN_LOADEXT : MOVEDIRN_LOAD, regs);
	}
	BETRACE ("gen11: loadelement (leave)");
}

/*}}}*/
/*{{{  PUBLIC void storeinelement(tptr, word, regs)*/
PUBLIC void storeinelement (treenode *tptr, INT32 word, int regs)
{
	BETRACE ("gen11: storeinelement (enter): tptr=%p, word=%d, regs=%d", tptr, (int)word, regs);
	moveelement (tptr, word, MOVEDIRN_STORE, regs);
	BETRACE ("gen11: storeinelement (leave)");
}

/*}}}*/
/*{{{  PUBLIC void loadelementpointer (treenode *const tptr, const INT32 word, const int regs)*/
PUBLIC void loadelementpointer (treenode *const tptr, const INT32 word, const int regs)
{
	/* modified 5/11/90 to take a 'word' parameter too */
	BETRACE ("gen11: loadelementpointer (enter): tptr=%p (%s), word=%d, regs=%d", tptr, tagstring (TagOf (tptr)), (int)word, regs);
#if 0
fprintf (stderr, "gen11: loadelementpointer: word = %d, regs = %d, tptr = ", word, regs);
printtreenl (stderr, 4, tptr);
#endif
	/* loading a pointer to a NULLARRAY means load NULL */
	if (TagOf (tptr) == S_NULLARRAY) {
		gensecondary (I_NULL);
#ifdef MOBILES
	} else if ((TagOf (tptr) == S_ARRAYITEM) && ismobile (ASBaseOf (tptr)) && (TagOf (ASBaseOf (tptr)) == S_ARRAYITEM)) {
		/* the base of this array subscription is mobile, if that is a MOBILE subscription too, handle carefully */
		int isubscript = return_subscript (tptr);

		loadmobile (ASBaseOf (tptr));
		/* have local 0 for temporary */
		genprimary (I_STL, 0);

		/* ASExpOf (tptr) should give us an index into the mobile array */
#if 0
fprintf (stderr, "gen11: loadelementpointer.  loadad MOBILE base, isubscript = %d, ASExpOf (tptr) = ", isubscript);
printtreenl (stderr, 4, ASExpOf (tptr));
#endif
		if (ASExpOf (tptr)) {
			texpopd (P_EXP, ASExpOf (tptr), regs);
			genprimary (I_LDL, 0);
			gensecondary (isubscript);
		}
#endif
	} else if (TagOf (tptr) == S_PARAM_FB) {
		/* special back-end param for FORKs */
		loadfb ();
	} else if (TagOf (tptr) == S_PARAM_MPP) {
		/* special back-end param for MOBILE PROC hook */
		loadmpp ();
	} else {
		/* printtreenl (stderr, 4, tptr); */
		moveelement (tptr, word, MOVEDIRN_LOADPTR, regs);
	}
	BETRACE ("gen11: loadelementpointer (leave)");
}

/*}}}*/
/*{{{  PUBLIC INT32 loadelementpointeroffset (treenode * const tptr, const int regs)*/
PUBLIC INT32 loadelementpointeroffset (treenode * const tptr, const int regs)
{
	INT32 offset = 0;
	BETRACE ("gen11: loadelementpointeroffset (enter): tptr=%p (%s), regs=%d", tptr, tagstring (TagOf (tptr)), regs);
	if (TagOf (tptr) == S_ARRAYITEM || TagOf (tptr) == S_RECORDITEM) {
		offset = loadarraypointer (tptr, 0, regs);	/* better code */
	} else {
		moveelement (tptr, 0, MOVEDIRN_LOADPTR, regs);
	}
	BETRACE ("gen11: loadelementpointeroffset (leave): int %d", (int)offset);
	return (offset);
}

/*}}}*/
/*}}}*/
/*{{{  load/store/loadptr       opds*/
/*{{{  PUBLIC void loadopd (int opdmode, treenode * opd, INT32 word)*/
PUBLIC void loadopd (int opdmode, treenode * opd, INT32 word)
{
	/* This is normally used for double length etc, so we can ignore
	   the sign-extension parameter to loadelement by setting it TRUE
	 */
	switch (opdmode) {
	case P_EXP:
		loadelement (opd, word, MANY_REGS, TRUE);
		break;
	case P_TEMP:
		loadname (opd, word);
		break;
	case P_PTR:
		loadelementpointer (opd, word, MANY_REGS);
		break;
	case P_TEMPPTR:
		loadnamepointer (opd, word);
		break;
	}
}

/*}}}*/
/*{{{  PUBLIC void storeinopd (opdmode, opd, word, regs)*/
/*****************************************************************************
 *
 *  storeinopd pulls the top of stack and places it in (opdmode, opd)
 *
 *****************************************************************************/
PUBLIC void storeinopd (int opdmode, treenode *opd, INT32 word, int regs)
{
#if 0
printf ("gen11: storeinopd: opdmode = %s, word = %d, regs = %d, opd = ", opdmode_string (opdmode), (int)word, regs);
printtree (stdout, 4, opd);
printf ("\n");
#endif
	switch (opdmode) {
	case P_TEMPPTR:
	case P_TEMP:
	case P_EXP:
		storeinelement (opd, word, regs);
		break;
	default:
		geninternal_is (GEN_BAD_OPD, opdmode, "storeinopd");
	}
}

/*}}}*/
/*{{{  BIT32 loadopdpointeroffset (opdmode, opd)*/
PRIVATE INT32 loadopdpointeroffset (int opdmode, treenode * opd)
{
	INT32 offset = 0;
	switch (opdmode) {
	case P_TEMPPTR:
	case P_EXP:
	case P_TEMP:
		offset = loadelementpointeroffset (opd, MANY_REGS);
		break;
	default:
		geninternal_is (GEN_BAD_OPD, opdmode, "loadopdpointeroffset");
	}
	return (offset);
}

/*}}}*/
/*}}}*/
/*}}}*/

/*{{{  assigninfo_t*/
typedef struct {
	treenode *adest;
	treenode *asource;
	int atype;
	int aregs;
	int astoreregs;
	BOOL aevaluated;
} assigninfo_t;
/*}}}*/

/*{{{  PRIVATE void setregdests (nregresults, regdests, destlist, instancedfn)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  setregdests takes the left- and right- hand sides of a call to a
 *              multivalued function and builds up a table, 'regdests' of
 *              the destination elements of the register results of the
 *              function. On exit, '*nregresults' contains the number of
 *              register results returned by the function.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void setregdests (int *nregresults, treenode ** regdests[MAXREGS], treenode * destlist, treenode * instancedfn)
{
	int i;
	int regsused = 0;
	treenode *ftypelist = FnTypeListOf (NTypeOf (instancedfn));
	while (!EndOfList (ftypelist) && (regsused < MAXREGS)) {
		if (result_return_style (ThisItem (ftypelist)) == return_style_alu)
			regdests[regsused++] = ThisItemAddr (destlist);
		destlist = NextItem (destlist);
		ftypelist = NextItem (ftypelist);
	}

	/* The list appears in the wrong order, so reverse it */
	for (i = 0; i < regsused / 2; i++) {
		/* Swap elements i and j (where j is the i'th from top) */
		const int j = regsused - 1 - i;
		treenode **temp = regdests[i];
		regdests[i] = regdests[j];
		regdests[j] = temp;
	}

	*nregresults = regsused;
}

/*}}}*/
/*{{{  PRIVATE int regsforassignments (assignment, nassignments)*/
/*****************************************************************************
 *
 *  regsforassignments returns the number of registers required to perform
 *                     all unevaluated assignments in the table 'assignment'.
 *
 *****************************************************************************/
PRIVATE int regsforassignments (assigninfo_t assignment[], const int nassignments)
{
	int i;
	int maxr = -1;
	for (i = 0; i < nassignments; i++)
		if (!assignment[i].aevaluated)
			maxr = (int) max_INT32 (maxr, assignment[i].aregs);
	return (maxr);
}

/*}}}*/
/*{{{  PRIVATE int ndependson (i, assignment, dependson, nassignments)*/
/*****************************************************************************
 *
 *  ndependson returns the number of assignments which assignment i
 *             depends on.
 *
 *****************************************************************************/
PRIVATE int ndependson (int i, assigninfo_t assignment[], char dependson[], int nassignments)
{
	int d = 0;
	int j;
	for (j = 0; j < nassignments; j++)
		if (!assignment[j].aevaluated && dependson[(i * nassignments) + j])
			d = d + 1;
	return (d);
}

/*}}}*/
/*{{{  PRIVATE int ndependants (i, assignment, dependson, nassignments)*/
/*****************************************************************************
 *
 *  ndependants returns the number of assignments which depend upon
 *              assignment i.
 *
 *****************************************************************************/
PRIVATE int ndependants (int i, assigninfo_t assignment[], char dependson[], int nassignments)
{
	int dependants = 0;
	int j;
	for (j = 0; j < nassignments; j++)
		if (!assignment[j].aevaluated && dependson[(j * nassignments) + i])
			dependants++;
	return (dependants);
}

/*}}}*/
/*{{{  PRIVATE int forced_assignment (assignment, dependson, nassignments)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  forced_assignment finds the best assignment to perform from the
 *                    assignment table 'assignment'.
 *                    The best assignment is the one which is depended upon
 *                    by the most other assignments (this breaks the greatest
 *                    number of cyclic dependencies), or in the case of a
 *                    draw, the assignment which uses most registers.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE int forced_assignment (assigninfo_t assignment[], char dependson[], int nassignments)
{
	int best_assignment = -1, maxdependants = -1, maxregisters = -1, i;

	for (i = 0; i < nassignments; i++)
		if (!assignment[i].aevaluated) {
			const int d = ndependants (i, assignment, dependson, nassignments);
			if
				/*{{{  best assignment so far */
				((d > maxdependants) || ((d == maxdependants) && (assignment[i].aregs > maxregisters)))
				/*}}} */
				/*{{{  choose this assignment */
			{
				best_assignment = i;
				maxdependants = d;
				maxregisters = assignment[i].aregs;
			}
			/*}}} */
		}

	return (best_assignment);
}

/*}}}*/
/*{{{  PUBLIC void mapmoveopd (destmode, destptr, sourcemode, sourceptr)*/
/*****************************************************************************
 *
 *  mapmoveopd maps out the moving of (sourcemode, sourceptr) to
 *             (destmode, destptr)  : both operands must be addressable
 *             N.B. At the moment, this routine assumes that source must
 *             be moved to dest using a move instruction, as other cases
 *             are pulled out by mapsimpleassign.
 *
 *****************************************************************************/
PUBLIC void mapmoveopd (int destmode, treenode ** destptr, int sourcemode, treenode ** sourceptr)
{
	/*{{{  default to a move instruction (with some last minute optimisations) */
	{
		treenode *desttype = gettype_main (*destptr);	/* we now want to keep this type tree */
		INT32 b;

		/*{{{  turn expressions into pointers */
		sourcemode = ptrmodeof (sourcemode);
		destmode = ptrmodeof (destmode);
		/*}}} */

		b = bytesin (desttype);
		if (b == -1)
			b = bytesin (gettype (*sourceptr));
		if ((b == 1) || (b == bytesperword && check_aligned (*sourceptr, (int) b) && check_aligned (*destptr, (int) b)))
			mapload2regs (destmode, destptr, sourcemode, sourceptr);
		else if (b == (-1))
			/*{{{  length is an expression */
		{
			treenode *lengthexp;
			treenode **lengthexpp = &lengthexp;	/* just in case */

			/*switch_to_temp_workspace(); */
			lengthexp = scaletreeof (desttype, 1, be_lexlevel);
			/*switch_to_real_workspace(); */

			/* Now we save the length of the block move - CO'N 18/5/90 */
			if (TagOf (*destptr) == S_ARRAYITEM) {
				DEBUG_MSG (("mapmoveopd: S_ARRAYITEM\n"));
				SetASLength (*destptr, lengthexp);
				lengthexpp = ASLengthAddr (*destptr);
			} else if (TagOf (*destptr) == S_SEGMENTITEM) {
				DEBUG_MSG (("mapmoveopd: S_SEGMENTITEM\n"));
				SetSLength (*destptr, lengthexp);
				lengthexpp = SLengthAddr (*destptr);
			}
#ifdef DEBUG
			else
				DEBUG_MSG (("mapmoveopd: neither S_ARRAYITEM nor S_SEGMENTITEM\n"));
#endif

			mapload3regs (sourcemode, sourceptr, destmode, destptr, P_EXP, lengthexpp);
			/* And, as usual, throw away the load sequence */
		}
		/*}}} */
		else if (b == 0)
			return;	/* bug TS/1782 26/08/92 */
		else
			/*{{{  length is a constant */
			mapload2regs (sourcemode, sourceptr, destmode, destptr);
		/*}}} */
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void mapsimpleassign(type, destmode, dest, sourcemode, source)  ***/
/*{{{  comment*/
/*****************************************************************************
 *
 *  mapsimpleassign maps out the assignment of source to dest, where
 *                  source and dest are single items of type 'type'.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void mapsimpleassign (int type, int destmode, treenode ** dest, int sourcemode, treenode ** source)
{
#if 0
fprintf (stderr, "gen11: mapsimpleassign: type = %s, *dest = ", itagstring (type));
printtreenl (stderr, 4, *dest);
fprintf (stderr, "gen11:     \"     \"    : *source = ");
printtreenl (stderr, 4, *source);
fprintf (stderr, "gen11: ntypeof (*dest) = %s\n", itagstring (ntypeof (*dest)));
#endif
#if EXAMINE_ASSIGN
fprintf (stderr, "gen11: mapsimpleassign: type = %s\n", itagstring (type));
#endif
#ifdef MOBILES
	if (ntypeof (*dest) == S_ANYCHANTYPE) {
		/*{{{  assignment to a MOBILE.CHAN from a regular channel-type*/
		type = S_INT;
		mapanychantypeassign (dest, source, MAXREGS);
		/*}}}*/
	} else if (ntypeof (*dest) == S_ANYPROCTYPE) {
		/*{{{  assignment to a MOBILE.PROC from a regular mobile process*/
		type = S_INT;
		mapanyproctypeassign (dest, source, MAXREGS);
		/*}}}*/
	} else if (ntypeof (*dest) == S_MOBILE) {
#if EXAMINE_ASSIGN
fprintf (stderr, "mapsimpleassign: target type is MOBILE =");
printtreenl (stderr, 4, *dest);
#endif
		/* something being assigned to a MOBILE */
		if (ntypeof (*source) == S_ANYCHANTYPE) {
			/*{{{  assignment from a MOBILE.CHAN to a regular channel-type*/
			type = S_INT;
			mapanychantypeassign (dest, source, MAXREGS);
			/*}}}*/
		} else if (ntypeof (*source) == S_ANYPROCTYPE) {
			/*{{{  assignment from a MOBILE.PROC to a regular mobile process*/
			type = S_INT;
			mapanyproctypeassign (dest, source, MAXREGS);
			/*}}}*/
		} else if (ntypeof (*source) == S_MOBILE) {
			/*{{{  mobile -> mobile assignment*/
#if EXAMINE_ASSIGN
fprintf (stderr, "mapsimpleassign: source type is MOBILE =");
printtreenl (stderr, 4, *source);
#endif
			switch (TagOf (*source)) {
			case S_CLONE:
				if (isdynmobilechantype (OpOf (*source))) {
					/* map channel-type CLONE assignment -- trivial */
					type = S_INT;
				} else if (isdynmobileproctype (OpOf (*source))) {
					type = S_INT;
				} else if (isdynmobilebarrier (OpOf (*source))) {
					generr (GEN_BARRIER_CLONE);
				} else {
					/* mangle type to do a standard copy.. (maybe) */
					treenode *ctype = follow_user_type (gettype (OpOf (*source)));

					if (TagOf (ctype) == S_MOBILE) {
						type = TagOf (follow_user_type (MTypeOf (ctype)));
					} else {
						type = TagOf (ctype);
					}
					source = OpAddr (*source);
					reservelowworkspace (1);				/* local 0 for mobile-mobile clone.. */
				}
				break;
			case S_NEW_ARRAY:
				type = S_INT;
				mapdynmobilearraycreate (dest, source, MAXREGS);		/* might reserve local 0 */
				break;
			case S_ALLOC_PROC:
				type = S_INT;
				mapdynmobileproccreate (dest, source, MAXREGS);			/* might reserve local 0 */
				break;
			case S_NEW_BARRIER:
				type = S_INT;
				mapdynmobilebarriercreate (dest, source, MAXREGS);		/* might reserve local 0 */
				break;
			default:
				if (isdynmobilearray (*source)) {
					mapdynmobilearrayassign (dest, source, MAXREGS);	/* might reserve local 0 */
				} else if (isdynmobilebarrier (*source)) {
					/* fairly trivial */
					type = S_INT;
				} else {
					/* swap for CHAN TYPE vars too */
					mappointerswap (dest, source, MAXREGS);			/* might reserve local 0 */
				}
				type = S_INT;
				break;
			}
			/*}}}*/
		} else {
#if EXAMINE_ASSIGN
fprintf (stderr, "mapsimpleassign: source type is non-MOBILE =");
printtreenl (stderr, 4, *source);
#endif
			/* non-mobile -> mobile assignment */
			type = ntypeof (*source);
		}
	}
#endif /* MOBILES */


	DEBUG_MSG (("mapsimpleassign: type: %s, destmode: %s, sourcemode: %s\n",
		    itagstring (type), opdmode_string (destmode), opdmode_string (sourcemode)));
	if (has_fpu_core && isreal (type)
			&& !isaddressableopd (sourcemode, *source)	/* bug 353 11/9/90 */
			&& !isconstopd (sourcemode, *source)) {		/* bug 708 12/9/90 */
		mapfpassign (destmode, dest, sourcemode, source);
	} else if (istargetintsize (type) || istargetbytesize (type) || (use_shortintops && isshortint (type))) {
		/* T9000 shorts 17/7/91 */
		/*{{{  single-length assign */
		if (has_fpu_core && isconversion (*source) && isreal (ntypeof (OpOf (*source))) && !isshortint (type)) {
			/* otherwise breaks on i16 := INT16 ROUND r */
			/*{{{  real to int conversion as source */
			BOOL desttempused = FALSE;

			if (T9000_alpha_nofpconv (&tx_global)) {	/* Sections 4.2.7, 4.2.8, 4.2.11, 4.2.12 of SW-0341-2 */
				treenode *new = makeconversion (ntypeof (OpOf (*source)), type, OpOf (*source), TagOf (*source), LocnOf (*source));
				freenode (source);
				*source = new;
#if EXAMINE_ASSIGN
fprintf (stderr, "mapsimpleassign(): going recursive at 1\n");
#endif
				mapsimpleassign (type, destmode, dest, sourcemode, source);
				return;
			}

			if (!issimpleopd (destmode, *dest)) {	/* added for bug 739 24/9/90 */
				*dest = gettemp (*dest, NM_POINTER);
				destmode = tempmodeof (destmode);
				mapexp (NDeclAddr (*dest));
				upusecount (*dest, 1);
				desttempused = TRUE;
			}
			mapfpexp (OpAddr (*source));
			destmode = ptrmodeof (destmode);
			mapexpopd (destmode, dest);
			if (desttempused) {
				freetemp (*dest);
			}
			/*}}} */
		} else if (directstore (destmode, *dest, be_lexlevel)) {
			/*{{{  load, then store */
			mapexpopd (sourcemode, source);
			mapstoreinopd (destmode, dest);
			/*}}} */
		} else {
			/*{{{  load pointer to dest and load source, store through pointer */
			destmode = ptrmodeof (destmode);
			mapload2regs (sourcemode, source, destmode, dest);
			/*}}} */
		}
		/*}}} */
	} else if (isdoublelength (type)) {
		mapmovelopd (destmode, dest, sourcemode, source);
	} else {
		/*{{{  special cases for assign */
		switch (type) {
			/* other cases  still to come */
			/*{{{  INT64 REAL64 on 16-bit machine */
		case S_INT64:
		case S_REAL64:
			mapmoveqopd (destmode, dest, sourcemode, source);
			break;
			/*}}} */
			/*{{{  INT16/UINT16 on 32-bit machine */
		case S_INT16:
		case S_UINT16:
			if (isaddressableopd (sourcemode, *source) || isconstopd (sourcemode, *source)) {
				const BOOL needtempdest = needtemptoload (destmode, *dest);
				const BOOL notconst = !isconstopd (sourcemode, *source);
				const BOOL needtempsource = needtemptoload (sourcemode, *source);
				DEBUG_MSG (("mapsimpleassign: INT16: needtempdest=%d, notconst=%d, needtempsource=%d\n",
					    needtempdest, notconst, needtempsource));
				if (needtempdest || (notconst && needtempsource))
					/*{{{  ldptr source; ldptr dest; ldc 2; move */
				{
					DEBUG_MSG (("mapsimpleassign: INT16: first case, mapping for move\n"));
					destmode = ptrmodeof (destmode);
					sourcemode = ptrmodeof (sourcemode);
					/*mapload2regs(destmode, dest, sourcemode, source); */
					mapload2regs (sourcemode, source, destmode, dest);	/* bug 1375 22/8/91 */
				}
				/*}}} */
				else
					/*{{{  ld source; st dest */
				{
					DEBUG_MSG (("mapsimpleassign: INT16: first case, turn into INT\n"));
					mapexpopd (sourcemode, source);
					mapstoreinopd (destmode, dest);
				}
				/*}}} */
			} else {
				DEBUG_MSG (("mapsimpleassign: INT16: second case\n"));
				if (!issimpleopd (destmode, *dest) || ispointer (*dest))
					/*{{{  temp := source; dest := temp */
				{
					*source = gettemp (*source, NM_WORKSPACE);
#if EXAMINE_ASSIGN
fprintf (stderr, "mapsimpleassign(): going recursive at 2 (temp then real)\n");
#endif
					mapsimpleassign (type, P_TEMP, source, sourcemode, NDeclAddr (*source));
					sourcemode = P_TEMP;
					mapsimpleassign (type, destmode, dest, sourcemode, source);
					freetemp (*source);
				}
				/*}}} */
				else
					/*{{{  ld source; st dest */
				{
					mapexpopd (sourcemode, source);
					mapstoreinopd (destmode, dest);
				}
				/*}}} */
			}
			break;
			/*}}} */
			/*{{{  ARRAY */
		case S_ARRAY:
		case S_RECORD:
#ifdef OCCAM2_5
			if ((type == S_RECORD) && (bytesin (gettype (*dest)) <= bytesperword)) {
				/*{{{  assign directly */
#if EXAMINE_ASSIGN
fprintf (stderr, "mapsimpleassign(): going recursive at 3 (direct assignment ARRAY/RECORD)\n");
#endif
				if (TagOf (gettype (*dest)) == S_MOBILE) {
					/* do it directly, not recursively */
					mapexpopd (sourcemode, source);
					mapstoreinopd (destmode, dest);
				} else {
					mapsimpleassign (S_INT, destmode, dest, sourcemode, source);
				}
				/*}}} */
			} else
#endif
			{
				/*{{{  do a fake 'quad length' move */
				mapmoveqopd (destmode, dest, sourcemode, source);
				/*}}} */
			}
			break;
			/*}}} */
		default:
			/*geninternal_is(GEN_ERROR_IN_ROUTINE, 4, "mapsimpleassign"); */
#if 0
fprintf (stderr, "*** badness in mapsimpleassign() type is [%s].  source =", itagstring (type));
printtreenl (stderr,8,*source);
fprintf (stderr, "    *** dest =");
printtreenl (stderr,8,*dest);
#endif
			badtag (genlocn, type, "mapsimpleassign");
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PRIVATE void mapmultiassign (mdestlist, msourcelist)  ***/
/* N.B. !!! Doesn't work for reals with inline fp */
/*{{{  comment*/
/*****************************************************************************
 *
 *  mapmultiassign takes a list of destinations, 'mdestlist' and a list of
 *                 sources, 'msourcelist', reorders them and
 *                 allocates and inserts temporaries required.  Note that
 *                 'special' temporary nodes are inserted for sources which
 *                 are held in registers before storing.
 *                 This routine only works for lists of expression sources,
 *                 multiple assignments which have multi-valued functions or
 *                 valofs on the right-hand side are handled separately.
 *                 Note in all the multiple assignment code that we can have
 *                 a multiple assignment nested inside another multiple
 *                 assignment (in a valof), so all the routines have to be
 *                 reentrant.
 *
 *                 Modified to use memalloc rather than fixing MAX_ASSIGNMENTS
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void mapmultiassign (treenode * mdestlist, treenode * msourcelist)
{
	treenode *sourcelist = msourcelist, *destlist = mdestlist;
	int nassignments = listitems (destlist);
	assigninfo_t *assignment = (assigninfo_t *) memalloc ((sizeof (assigninfo_t))
							      * nassignments);
	char *dependson = (char *) memalloc ((sizeof (char)) * nassignments * nassignments);
	int *evalorder = (int *) memalloc ((sizeof (int)) * nassignments);

	int i, j, ndone = 0;
	int regtemp[MAXREGS];
	int nregtemps = 0;


	DEBUG_MSG (("mapmultiassign:\n"));
	/*{{{  initialise 'assignment' */
	for (i = 0; i < nassignments; i++) {
		assigninfo_t *ass = &(assignment[i]);
		treenode *dest = ThisItem (destlist);
		treenode *source = ThisItem (sourcelist);
		int type = ntypeof (dest);

		if (TagOf (dest) == S_UNDEFINED) {
			dest = OpOf (dest);
		}
		if (TagOf (source) == S_UNDEFINED) {
			source = OpOf (source);
		}

		ass->atype = type;
		ass->adest = dest;
		ass->asource = source;
		ass->aevaluated = FALSE;
		/*{{{  set ass->aregs and ass->astoreregs */
		if (istargetbytesize (type) || istargetintsize (type) || (use_shortintops && isshortint (type))) {	/* T9000 shorts 18/7/91 */
			const int sr = regsfor (source);
			const int dr = regsforstore (dest);
			ass->astoreregs = dr;
			ass->aregs = ((sr == dr) ? sr + 1 : (int) max_INT32 (sr, dr));
		} else {
			ass->astoreregs = MAXREGS;
			ass->aregs = MAXREGS;
		}
		DEBUG_MSG (("mapmultiassign: assignment %d, type %s, storeregs:%d, aregs:%d\n",
			    i, itagstring (ass->atype), ass->astoreregs, ass->aregs));
		/* There is a hole here : a scalar INT16 source (on T4) to scalar INT16
		   dest only takes one register */
		/*}}} */
		destlist = NextItem (destlist);
		sourcelist = NextItem (sourcelist);
	}
	/*}}} */
	/*{{{  initialise 'dependson' */
	for (i = 0; i < nassignments; i++) {
		for (j = 0; j < nassignments; j++) {
			dependson[(nassignments * i) + j] = (i == j) ? FALSE : usedin (assignment[i].adest, assignment[j].asource, be_lexlevel);
			DEBUG_MSG (("mapmultiassign: %d dependson %d ? %d\n", i, j, dependson[(nassignments * i) + j]));
		}
	}
	/*}}} */
	while (ndone < nassignments) {
		/*{{{  move the sources to destinations, temporaries, or registers */
		/*{{{  do the assignments which aren't dependant upon any others */
		BOOL done_an_assignment = TRUE;
		while (done_an_assignment) {
			done_an_assignment = FALSE;
			for (i = 0; i < nassignments; i++)
				if (!assignment[i].aevaluated && (ndependson (i, assignment, dependson, nassignments) == 0)) {
					assignment[i].aevaluated = TRUE;
					done_an_assignment = TRUE;
					evalorder[ndone++] = i;
					DEBUG_MSG (("mapmultiassign: doing asignment %d\n", i));

					mapsimpleassign (assignment[i].atype, P_EXP, &(assignment[i].adest), P_EXP, &(assignment[i].asource));
					/*{{{  resurrect newly formed temps */
					/* INSdi03198 12/11/93 */
					/* If the 'mapsimpleassign' generated a temporary, then we must
					   resurrect it so that we don't clobber it.
					   This means that it is OK for 'tmultiassign' to delay the
					   re-assignment until the end of the multiassign.
					 */
					if (TagOf (assignment[i].asource) == T_TEMP) {
						DEBUG_MSG (("mapmultiassign: resurrecting temp for assignment %d\n", i));
						resurrect_var (assignment[i].asource);
					}
					/*}}} */
				}
		}
		/*}}} */
		if (ndone < nassignments) {
			/*{{{  break a cycle of dependency */
			int freeregs = MAXREGS - nregtemps;
			int type;
			i = forced_assignment (assignment, dependson, nassignments);
			assignment[i].aevaluated = TRUE;
			evalorder[ndone++] = i;
			DEBUG_MSG (("mapmultiassign: breaking cycle with assignment %d ", i));
			type = assignment[i].atype;
			if (fitsinregister (type) && !(has_fpu_core && isreal (type)) &&	/* added for bug 1002 4/10/90 */
					(regsforassignments (assignment, nassignments) <= freeregs - 1) && (assignment[i].astoreregs <= freeregs - 1)) {

				/*{{{  we can keep the source in a register */
				DEBUG_MSG (("in register\n"));
				mapexp (&(assignment[i].asource));
				assignment[i].asource = newtempnode (T_REGTEMP, assignment[i].asource, NM_WORKSPACE, be_lexlevel);
				regtemp[nregtemps++] = i;
				/*}}} */
			} else {
				/*{{{  assign source i to a temporary */
				/* bug 1002 4/10/90 - cannot simply create scalars for all scalar
				   variables - we mustn't if it is a long, or if on FPU */
				if (fitsinregister (type) && !(has_fpu_core && isreal (type))) {
					/*{{{  create a scalar temporary and generate into that */
					DEBUG_MSG (("scalar temporary\n"));
					mapexp (&(assignment[i].asource));
					assignment[i].asource = gettemp (assignment[i].asource, NM_WORKSPACE);
					upusecount (assignment[i].asource, 1);
					/*}}} */
				} else {
					/*{{{  create a (vector/long/real) temporary and assign into that */
					DEBUG_MSG (("(vector/long/real) temporary\n"));
					/* This branch creates a temporary _before_ evaluating the rhs, which
					   is okay for vectors, but suboptimal for scalar expressions */
					assignment[i].asource = gettemp (assignment[i].asource, NM_WORKSPACE);
					/* it would seem that we could just change NM_WORKSPACE to NM_DEFAULT,
					   but if we do, the temp is never initialised to point at vectorspace,
					   and the vectorspace is never allocated */
/*assignment[i].asource = gettemp(assignment[i].asource, NM_DEFAULT); *//* bug 235 17/9/90 */
					mapsimpleassign (type, P_TEMP, &(assignment[i].asource), P_EXP, NDeclAddr (assignment[i].asource));
					/*}}} */
				}
				/*}}} */
			}
			/*}}} */
		}
		/*}}} */
	}
	/*{{{  store any values held in registers */
	for (i = nregtemps - 1; i >= 0; i--) {
		DEBUG_MSG (("mapmultiassign: saving register temp %d\n", regtemp[i]));
		mapaddr (&(assignment[regtemp[i]].adest));
	}
	/*}}} */
	/*{{{  move temporaries to destinations */
	for (i = 0; i < nassignments; i++)
		if (TagOf (assignment[i].asource) == T_TEMP)
			/*{{{  move temporary i to destination */
		{
			DEBUG_MSG (("mapmultiassign: moving temp for multiassignment %d back\n", i));
			mapsimpleassign (assignment[i].atype, P_EXP, &(assignment[i].adest), P_TEMP, &(assignment[i].asource));
			freetemp (assignment[i].asource);
		}
	/*}}} */
	/*}}} */
	/*{{{  remake the multi-assign node with the new order */
	destlist = mdestlist;
	sourcelist = msourcelist;
	for (i = 0; i < nassignments; i++) {
		assigninfo_t *ass = &(assignment[evalorder[i]]);
		NewItem (ass->adest, destlist);
		NewItem (ass->asource, sourcelist);
		destlist = NextItem (destlist);
		sourcelist = NextItem (sourcelist);
	}
	/*}}} */
	memfree (assignment);
	memfree (dependson);
	memfree (evalorder);
}

/*}}}*/
/*{{{  PUBLIC void mapassign (destptr, sourceptr)*/
/*****************************************************************************
 *
 *  mapassign maps the assignment of 'sourceptr' to 'destptr'.
 *            The source and destinations may be single items or lists.
 *
 *****************************************************************************/
PUBLIC void mapassign (treenode ** destptr, treenode ** sourceptr)
{
	treenode *dest = *destptr, *source = *sourceptr;

#if 0
printf ("gen11: mapassign: dest =");
printtree (stdout, 2, dest);
printf ("\nsource =");
printtree (stdout, 2, source);
printf ("\n");
#endif
	if (TagOf (dest) == S_LIST) {
		/*{{{  deal with multiple assignments */
		if (listitems (source) > 1) {
			mapmultiassign (dest, source);
		} else {
			/*{{{  multi-valued function or valof */
			treenode *sourceitem = ThisItem (source);
			const int tag = TagOf (sourceitem);
			if (isspecification (sourceitem) || (tag == S_VALOF)) {
				mapvalof (sourceitem, destptr);
			} else if (tag == S_FINSTANCE) {
				if ((TagOf (INameOf (sourceitem)) != N_PREDEFFUNCTION) || !mappredef (sourceitem, dest)) {
					treenode *instancedfn = INameOf (sourceitem);
					/*{{{  map multi-valued function instance */
					{
						treenode *paramlist = IParamListOf (sourceitem);
						paramlist = augmentparams (paramlist, FnParamsOf (NTypeOf (instancedfn)), dest, sourceitem);
						SetIParamList (sourceitem, paramlist);
						/* Insert temporaries in front of any results assigned through result
						   pointers which are aliased in the function */
						/*{{{  insert temporaries in front of aliased results */
						{
							treenode *p;
							for (p = paramlist; !EndOfList (p); p = NextItem (p)) {
								treenode *thisparam = ThisItem (p);
								if (TagOf (thisparam) == S_FNACTUALRESULT) {
									treenode *r = HExpOf (thisparam);
									if (temp_required_for_fn_dest (r, sourceitem)) {
										SetHExp (thisparam, gettemp (r, NM_WORKSPACE));
									}
								}
							}
						}
						/*}}} */
						mapinstance (sourceitem);
						/*{{{  store the register results */
						{
							int nregresults;
							treenode **regdests[MAXREGS];
							setregdests (&nregresults, regdests, dest, instancedfn);
							if (nregresults > 0) {
								mapstoreregs (regdests, nregresults);
							}
						}
						/*}}} */
						/*{{{  move temporary results to their real locations, free temps */
						{
							treenode *p;
							for (p = paramlist; !EndOfList (p); p = NextItem (p)) {
								treenode *const thisparam = ThisItem (p);
								if (TagOf (thisparam) == S_FNACTUALRESULT) {
									treenode *const pnode = HExpOf (thisparam);
									if (TagOf (pnode) == T_TEMP) {
										treenode *const r = NDeclOf (pnode);
										mapsimpleassign (ntypeof (r), P_EXP, NDeclAddr (pnode), P_TEMP, HExpAddr (thisparam));
										freetemp (pnode);
									}
								}
							}
						}
						/*}}} */
					}
					/*}}} */
				}
			} else {
				geninternal_is (GEN_ERROR_IN_ROUTINE, 5, "mapassign");
			}
			/*}}} */
		}
		/*}}} */
	} else {
		mapsimpleassign (ntypeof (dest), P_EXP, destptr, P_EXP, sourceptr);
	}
}

/*}}}*/

/*{{{  PUBLIC void moveopd (destmode, dest, sourcemode, source)*/
/*****************************************************************************
 *
 * moveopd copies (sourcemode, source) to (destmode, dest)
 *         source and dest are assumed to be addressable.
 *
 *****************************************************************************/
PUBLIC void moveopd (int destmode, treenode * dest, int sourcemode, treenode * source)
{
	const BOOL dest_devaccess = is_devaccess (dest);
	const BOOL source_devaccess = is_devaccess (source);
	int type;
	DEBUG_MSG (("moveopd\n"));

	if (TagOf (dest) == S_UNDEFINED) {
		dest = OpOf (dest);
	}
	type = ntypeof (dest);
#ifdef MOBILES
	if (type == S_MOBILE) {
		type = ntypeof (source);
	}
#endif
	/*{{{  see if we can pretend its an integer assign */
	if (!use_shortintops &&	/* T9000 shorts 17/7/91 */
	    isshortint (type)) {
		/* If the source isn't addressable, we must be able to store directly
		   to dest, otherwise the source would have to be preevaluated to a
		   temporary.
		   Otherwise, we need a temp to load test on source and dest
		   decides whether we can pretend it's an integer assign */
		if (!isaddressableopd (sourcemode, source) || (!needtemptoload (destmode, dest) && !needtemptoload (sourcemode, source))) {
			DEBUG_MSG (("moveopd: converted INT16 move into INT move\n"));
			type = targetintsize;
		}
	} else if (isshorttype (type) && issimpleopd (destmode, dest) && issimpleopd (sourcemode, source)) {
		DEBUG_MSG (("moveopd: converted short move into INT move\n"));
		type = targetintsize;
	}
	/*}}} */

	if (istargetintsize (type)) {
		/*{{{  optimise single length move */
		if (preeval (destmode, dest)) {
			/*{{{  ldptr dest; stl temp; source; ldl temp; stnl offset */
			const BIT32 offset = loadelementpointeroffset (NDeclOf (dest), MANY_REGS);
			storeinname (dest, 0);
			texpopd (sourcemode, source, MANY_REGS);
			loadname (dest, 0);
			movepointer (MOVEDIRN_STORE, I_STNL, offset, targetintsize, NULL, dest_devaccess);
			/*}}} */
		} else if (regsforaddropd (destmode, dest) < MAXREGS) {
			/*{{{  source; st dest */
			texpopd (sourcemode, source, MANY_REGS);
			storeinopd (destmode, dest, 0, MAXREGS - 1);
			/*}}} */
		} else {
			/* We know destmode isn't TEMP or TEMPPTR due to regsfor result */
			/*{{{  ldptr dest; source; rev; stnl offset */
			BIT32 offset = loadelementpointeroffset (dest, MAXREGS);
			texpopd (sourcemode, source, MANY_REGS);
			gensecondary (I_REV);
			movepointer (MOVEDIRN_STORE, I_STNL, offset, targetintsize, NULL, dest_devaccess);
			/*}}} */
		}
		/*}}} */
	} else if (isdoublelength (type)) {
		/*{{{  optimise double length move */
		movelopd (destmode, dest, sourcemode, source);
		/*}}} */
	} else {
		/*{{{  default to a move instruction (with some last minute optimisations) */
		treenode *desttype = gettype (dest);
		treenode *sourcetype = gettype (source);
		INT32 b;

		if (TagOf (sourcetype) == S_MOBILE && TagOf (MTypeOf (sourcetype)) == S_ARRAY) {
			treenode *basetype = MTypeOf (sourcetype);
			while (TagOf (basetype) == S_ARRAY) {
				basetype = ARTypeOf (basetype);
			}
			basetype = follow_user_type (basetype);
			if (TagOf (basetype) == S_MOBILE) {
				generr (GEN_BAD_MOBILE_SLICE);
			}
		}

		sourcemode = ptrmodeof (sourcemode);
		destmode = ptrmodeof (destmode);

		b = bytesin (desttype);
#if 0
fprintf (stderr, "moveopd: bytesin (desttype) = %d, dest =", b);
printtreenl (stderr, 4, dest);
fprintf (stderr, "moveopd: desttype =");
printtreenl (stderr, 4, desttype);
#endif
		if (b == (-1)) {
			b = bytesin (sourcetype);
		}
#if 0
fprintf (stderr, "moveopd: bytesin ([sourcetype]) = %d, source =", b);
printtreenl (stderr, 4, source);
fprintf (stderr, "moveopd: sourcetype =");
printtreenl (stderr, 4, sourcetype);
#endif

		if (!source_devaccess && !dest_devaccess && ((b == 1) || ((b == bytesperword) &&
				check_aligned (source, (int) b) && check_aligned (dest, (int) b))
				|| ((b < bytesperword) && (b == 2) && use_shortintops &&	/* T9000 shorts 17/7/91 */
				check_aligned (source, (int) b) && check_aligned (dest, (int) b)))) {
			/*{{{  can be optimised to a load and a store */

			const int size_tag = (b == 1) ? S_BYTE : (b == 2) ? S_INT16 : targetintsize;
			tload2regs (destmode, dest, sourcemode, source, FALSE, FALSE);

			/* no need to sign extend, cos we're just about to store it */
			movepointer (MOVEDIRN_LOAD, I_LDNL, 0, size_tag, NULL, source_devaccess);
			gensecondary (I_REV);
			movepointer (MOVEDIRN_STORE, I_STNL, 0, size_tag, NULL, dest_devaccess);
			/*}}} */
		} else {
			/*{{{  use a move instruction */
			if (b == (-1)) {
				/*{{{  length is an expression */
				BOOL preeval_e2, preeval_e3;
				int loadseq;
				treenode *lengthexp;
#ifdef MOBILES
				if ((TagOf (desttype) == S_MOBILE) && (TagOf (MTypeOf (desttype)) == S_ARRAY)
					&& (ARDimOf (MTypeOf (desttype)) == -1)) {

					/* dynamic mobile as target, use it's length */
					tload2regs (sourcemode, source, destmode, dest, FALSE, FALSE);
					loaddynmobilesize (dest, 1);
					lengthexp = NULL;
				} else
#endif
				if (TagOf (dest) == S_ARRAYITEM) {
					DEBUG_MSG (("moveopd: dest is an ARRAYITEM; picking up length\n"));
					lengthexp = ASLengthOf (dest);
				} else if (TagOf (dest) == S_SEGMENTITEM) {
					DEBUG_MSG (("moveopd: dest is a SEGMENTITEM; picking up length\n"));
					lengthexp = SLengthOf (dest);
				} else {
					const int old = switch_to_temp_workspace ();
					DEBUG_MSG (("moveopd: neither ARRAYITEM nor SEGMENTITEM\n"));
					lengthexp = scaletreeof (desttype, 1, be_lexlevel);
#if 0
fprintf (stderr, "moveopd: called scaleoftree for length on desttype and got:");
printtreenl (stderr, 4, lengthexp);
#endif
					switch_to_prev_workspace (old);
				}

				/* MOBILEs set lengthexp NULL if stuff loaded above */
				if (lengthexp) {
					/* We only do this call so that we have a loadseq parameter for
					   tload3regs, ideally we should make the binder save loadseq
					   in the tree. */
					loadseq = giveorder (sourcemode, source, destmode, dest, P_EXP, lengthexp, &preeval_e2, &preeval_e3);
					DEBUG_MSG (("moveopd: calling tload3regs; loadseq is %d\n", loadseq));
					tload3regs (sourcemode, source, destmode, dest, P_EXP, lengthexp, loadseq, FALSE);
				}
				/*}}} */
			} else if (b == 0) {
				return;	/* bug TS/1782 26/08/92 */
			} else {
				/*{{{  length is a constant */
				tload2regs (sourcemode, source, destmode, dest, FALSE, FALSE);
				loadconstant (b);
				/*}}} */
			}
			checkerror ();

			if (source_devaccess || dest_devaccess) {
				/*{{{  device move */
				/*{{{  T9000_gamma_baddevaccess */
				if (T9000_gamma_baddevaccess (&tx_global)) {
					/*{{{  issue warning */
					/* This warning is issued because turning off interrupts
					   during a devmove could have severe effects on
					   interrupt latency.
					   However, these devmove instructions are only generated
					   for oddities such as PORTs which are bigger than a word,
					   and will hence be very rare, so there doesn't seem
					   to be much point in worrying about it.

					   As a compromise, we just warn the user about the problem.
					   If this matters, they can either buy a chip which works,
					   (my preferred solution)
					   or code around it by changing the shape of the PORT.

					   When this bug work-around is removed, remember to remove
					   the warning message too.

					   CON - 7/3/94.
					 */
					genwarn (GEN_DEVMOVE_LATENCY_PROBLEM);
					/*}}} */
					gensecondary (I_INTDIS);
					gencomment0 ("bug fix for device access");
				}
				/*}}} */
				gensecondary (I_DEVMOVE);
				/*{{{  T9000_gamma_baddevaccess */
				if (T9000_gamma_baddevaccess (&tx_global)) {
					gensecondary (I_INTENB);
					gencomment0 ("bug fix for device access");
				}
				/*}}} */
				/*}}} */
			} else {
#if 0
gencomment0 ("I_MOVE from moveopd");
fprintf (stderr, "gen11: moveopd: generating I_MOVE\n");
#endif
				gensecondary (I_MOVE);
			}
			/*}}} */
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PRIVATE BOOL isiniospace (treenode *tptr) */
/*
 *	BOOL isiniospace (treenode *tptr)
 *	returns non-zero if `tptr' is marked as IOSPACE
 *	also returns TRUE for ports (had their NVDevice set by fixporttype())
 */
PRIVATE BOOL isiniospace (treenode *tptr)
{
#if 0
fprintf (stderr, "gen11: isiniospace: tptr =");
printtreenl (stderr, 4, tptr);
#endif
	if ((TagOf (tptr) == S_ARRAYITEM) && (nodetypeoftag (TagOf (ASBaseOf (tptr))) == NAMENODE) &&
		(nodetypeoftag (TagOf (NTypeOf (ASBaseOf (tptr)))) == TYPENODE) &&
		(TypeAttrOf (NTypeOf (ASBaseOf (tptr))) & TypeAttr_iospace)) {

		return TRUE;
	} else if ((TagOf (tptr) == S_ARRAYITEM) && (nodetypeoftag (TagOf (ASBaseOf (tptr))) == NAMENODE) &&
			NVDeviceOf (ASBaseOf (tptr))) {
		return TRUE;
	}
	return FALSE;
}
/*}}}  */
/*{{{  PUBLIC void tsimpleassign (type, destmode, dest, sourcemode, source, regs)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tsimpleassign generates code to evaluate (sourcemode, source) and store the
 *         result in (destmode, dest).
 *         'type' is the type of the destination, 'regs' is the maximum
 *         number of registers available for the assignment.
 *
 *
 *****************************************************************************/
/*}}}*/


/* The largest number of registers we may use */
PUBLIC void tsimpleassign (int type, int destmode, treenode *dest, int sourcemode, treenode *source, int regs)
{
	const BOOL dest_devaccess = is_devaccess (dest);
	int r = (regs == MANY_REGS) ? MAXREGS : regs;

#if EXAMINE_ASSIGN
fprintf (stderr, "gen11: tsimpleassign (destmode = %s, isplaced(dest) = %d, dest =", opdmode_string (destmode), (nodetypeoftag (TagOf (dest)) == NAMENODE) ? isplaced (dest) : -1);
printtree (stderr, 4, dest);
fprintf (stderr, "\n  sourcemode = %s, isplaced(source) = %d, source =", opdmode_string (sourcemode), (nodetypeoftag (TagOf (dest)) == NAMENODE) ? isplaced (source) : -1);
printtree (stderr, 4, source);
fprintf (stderr, "\n  type = %s, r = %d\n", tagstring (type), r);
#endif
#ifdef MOBILES
	if (ntypeof (dest) == S_MOBILE) {
		/* something being assigned to a MOBILE */
		if (ntypeof (source) == S_MOBILE) {
			/*{{{  mobile -> mobile assignment (return)*/
#if EXAMINE_ASSIGN
fprintf (stderr, "tsimpleassign: MOBILE := MOBILE assignment\n");
#endif
			switch (TagOf (source)) {
			case S_CLONE:
				{
					/* mangle type to do a standard copy.. */
					treenode *ctype = follow_user_type (gettype (OpOf (source)));

					source = OpOf (source);

					if (isdynmobilearray (dest)) {
						if (disable_dynmem) {
							generr (GEN_NO_DYNMEM);
						} else {
							gendynmobileclone (dest, source, regs);
						}
						return;
					} else if (isdynmobilechantype (dest)) {
						if (disable_dynmem) {
							generr (GEN_NO_DYNMEM);
						} else {
							gendynmobilechanclone (dest, source, regs);
						}
						/* done here.. */
						return;
					} else if (isdynmobileproctype (dest)) {
						if (disable_dynmem) {
							generr (GEN_NO_DYNMEM);
						} else {
							gendynmobileprocclone (dest, source, regs);
						}
						/* done here.. */
						return;
					} else {
						if (TagOf (ctype) == S_MOBILE) {
							if (!enable_mobilespace && disable_dynmem) {
								generr (GEN_NO_DYNMEM);
							}
							type = TagOf (follow_user_type (MTypeOf (ctype)));
						} else {
							type = TagOf (ctype);
						}
					}
				}
				break;
			case S_NEW_ARRAY:
				if (disable_dynmem) {
					generr (GEN_NO_DYNMEM);
				} else {
					gendynmobilearraycreate (dest, source, regs);
				}
				return;
			case S_ALLOC_PROC:
				if (disable_dynmem) {
					generr (GEN_NO_DYNMEM);
				} else {
					gendynmobileproccreate (dest, source, regs);
				}
				return;
			case S_NEW_BARRIER:
				if (disable_dynmem) {
					generr (GEN_NO_DYNMEM);
				} else {
					gendynmobilebarriercreate (dest, source, regs);
				}
				return;
			default:
				/* ldl source; ldl dest; stl source; stl dest */
				if (isdynmobilearray (source)) {
					gendynmobilearrayassign (dest, source, regs);
				} else if (isdynmobilebarrier (source)) {
					gendynmobilebarrierassign (dest, source, regs);
				} else {
					/* swap for CHAN TYPE / PROC TYPE vars too */
					genpointerswap (dest, source);
				}
				return;
			}
			/*}}}*/
		} else if (ntypeof (source) == S_ANYCHANTYPE) {
			/*{{{  assignment from MOBILE.CHAN to regular mobile-chan (return)*/
			genanychantypeassign (dest, source, regs, dest);
			return;
			/*}}}*/
		} else if (ntypeof (source) == S_ANYPROCTYPE) {
			/*{{{  assignment from MOBILE.PROC to a regular mobile process (return)*/
			genanyproctypeassign (dest, source, regs);
			return;
			/*}}}*/
		} else {
			/* non-mobile -> mobile assignment */
			type = ntypeof (source);
		}
	} else if (ntypeof (dest) == S_ANYCHANTYPE) {
		/*{{{  assignment to MOBILE.CHAN  (return)*/
		genanychantypeassign (dest, source, regs, dest);
		return;
		/*}}}*/
	} else if (ntypeof (dest) == S_ANYPROCTYPE) {
		/*{{{  assignment to MOBILE.PROC  (return)*/
		genanyproctypeassign (dest, source, regs);
		return;
		/*}}}*/
	}
#endif /* MOBILES */
	/*{{{  pick out cases which can be turned into an INT assign */
	/*if (istargetbytesize(type) && issimpleopd(destmode, dest)) */
	if ((istargetbytesize (type) || (use_shortintops && isshortint (type))) /* T9000 shorts 17/7/91 */
	    &&issimpleopd (destmode, dest)) {
		/* Byte sized types are ok, because we can load bytes directly.
		   Other short types (the only one currently is INT16 on a 32-bit
		   machine) can only be treated as an INT  assign if both source
		   and destination are simple. */
		type = targetintsize;
	} else if ((type == S_CHAN) || (type == S_PORT)) {
		type = targetintsize;
	}
	/*}}} */

#if EXAMINE_ASSIGN
fprintf (stderr, "  gen11: tsimpleassign: type = %s\n", tagstring (type));
#endif
	if (has_fpu_core && isreal (type)
	    && !isaddressableopd (sourcemode, source)	/* re-inserted 11/9/90 bug 353 */
	    &&!isconstopd (sourcemode, source))	{ /* bug 708 12/9/90 */

		tfpassign (type, destmode, dest, sourcemode, source, regs);
	} else if (istargetintsize (type)) {
		/*{{{  optimise single length assign */
#if EXAMINE_ASSIGN
fprintf (stderr, "  gen11: tsimpleassign: istargetintsize (type) == TRUE\n");
#endif
		if (has_fpu_core && isconversion (source) && isreal (ntypeof (OpOf (source)))) {
			/*{{{  special case a conversion from real on an fp processor */
			destmode = ptrmodeof (destmode);
			destmode = simplify (destmode, dest);	/* added 24/9/90 for bug 739 */
			tfpexp (OpOf (source), regs, MANY_REGS);	/*      fp.source    */
			if (TagOf (source) == S_TRUNC) {
				if (has_directfp)
					gensecondary (I_FPRZ);	/*     (fprz)       */
				else
					genfpuentry (I_FPURZ);
			}
			gensecondary (CONVERSIONCHECKING ? I_FPRTOI32	/*   fprtoi32     */
				      : I_FPINT);	/*   or fpint     */
			if (target_bigendian && (destmode == P_PTR)) {
				/* disable big-endian mode while loading pointer */
				target_bigendian = FALSE;
				loadopd (destmode, dest, 0);	/*      ldp   dest   */
				target_bigendian = TRUE;
			} else {
				loadopd (destmode, dest, 0);	/*      ldp   dest   */
			}
			checkerror ();
			gensecondary (I_FPSTNLI32);	/*      fpstnli32    */
			/*}}} */
		} else if (preeval (destmode, dest)) {
			/*{{{  ldptr dest; stl temp; source; ldl temp; stnl */
			const BIT32 offset = loadopdpointeroffset (destmode, NDeclOf (dest));
#if EXAMINE_ASSIGN
fprintf (stderr, "  gen11: tsimpleassign: preeval (destmode, dest) TRUE\n");
#endif
			storeinname (dest, 0);
			texpopd_main (sourcemode, source, MANY_REGS, FALSE);
			loadnamepointer (dest, 0);
			movepointer (MOVEDIRN_STORE, I_STNL, offset, type, NULL, dest_devaccess);
			/*}}} */
		} else if (directstore (destmode, dest, be_lexlevel)) {
			/*{{{  simplest case: generate source, store in dest */
			/* Only sign extend if we are initialising a temporary */
			const BOOL signextend_result = (TagOf (dest) == T_PREEVALTEMP);
#if EXAMINE_ASSIGN
fprintf (stderr, "  gen11: tsimpleassign: directstore (destmode, dest, be_level) TRUE\n");
#endif
			if (isiniospace (source)) {
#if EXAMINE_ASSIGN
fprintf (stderr, "  gen11: tsimpleassign: array source is in IOSPACE\n");
#endif
				gencomment0 (".MAGIC IOSPACE");
			}
			texpopd_main (sourcemode, source, regs, signextend_result);
			storeinopd (destmode, dest, 0, r - 1);
			/*}}} */
		} else if (preeval (sourcemode, source)) {
			/*{{{  source; stl temp; ldptr dest; ldl temp; rev; stnl */
			BIT32 offset;
#if EXAMINE_ASSIGN
fprintf (stderr, "  gen11: tsimpleassign: preeval (sourcemode, source) TRUE\n");
#endif
			texpopd_main (sourcemode, NDeclOf (source), MANY_REGS, FALSE);
			storeinname (source, 0);
			sourcemode = P_TEMP;
			offset = loadopdpointeroffset (destmode, dest);
			loadname (source, 0);
			gensecondary (I_REV);
			movepointer (MOVEDIRN_STORE, I_STNL, offset, type, NULL, dest_devaccess);
			/*}}} */
		} else if (regsforopd (destmode, dest) < r) {
			/*{{{  source; ldptr dest; stnl */
			BIT32 offset;
#if EXAMINE_ASSIGN
fprintf (stderr, "  gen11: tsimpleassign: regsforopd (destmode, dest) TRUE\n");
#endif
			texpopd_main (sourcemode, source, regs, FALSE);
			offset = loadelementpointeroffset (dest, r - 1);
			movepointer (MOVEDIRN_STORE, I_STNL, offset, type, NULL, dest_devaccess);
			/*}}} */
		} else {
			/*{{{  ldptr dest; source; rev; stnl */
			BIT32 offset = loadelementpointeroffset (dest, regs);
#if EXAMINE_ASSIGN
fprintf (stderr, "  gen11: tsimpleassign: ELSE!\n");
#endif
			texpopd_main (sourcemode, source, r - 1, FALSE);
			gensecondary (I_REV);
			movepointer (MOVEDIRN_STORE, I_STNL, offset, type, NULL, dest_devaccess);
			/*}}} */
		}
		/*}}} */
	} else if (isdoublelength (type)) {
		movelopd (destmode, dest, sourcemode, source);
	} else {
		/*{{{  special cases for assign */
		switch (type) {
			/*{{{  BYTE BOOL */
			/* dest MUST be via a pointer */
		case S_BYTE:
		case S_BOOL:
#if EXAMINE_ASSIGN
fprintf (stderr, "  gen11: tsimpleassign: BOOL/BYTE!\n");
#endif
			destmode = ptrmodeof (destmode);
			/* check for special IOSPACE */
			if (type == S_BYTE) {
				if (isiniospace (dest)) {
					/* the target array is IOSPACE */
#if EXAMINE_ASSIGN
fprintf (stderr, "  gen11: tsimpleassign: array target is in IOSPACE\n");
#endif
					tload2regs (sourcemode, source, destmode, dest, FALSE, FALSE);
					gencomment0 (".MAGIC IOSPACE");
				} else if (isiniospace (source)) {
					/* the source array is IOSPACE */
#if EXAMINE_ASSIGN
fprintf (stderr, "  gen11: tsimpleassign: array source is in IOSPACE\n");
#endif
					gencomment0 (".MAGIC IOSPACE");
					tload2regs (sourcemode, source, destmode, dest, FALSE, FALSE);
				} else {
					tload2regs (sourcemode, source, destmode, dest, FALSE, FALSE);
				}
			} else {
				tload2regs (sourcemode, source, destmode, dest, FALSE, FALSE);
			}
			movepointer (MOVEDIRN_STORE, I_STNL, 0, type, NULL, dest_devaccess);
			break;
			/*}}} */
			/*{{{  INT16/UINT16 on 32-bit machine */
		case S_INT16:	/* we must be on a 32-bit machine */
		case S_UINT16:
			if (use_shortintops) {	/* T9000 shorts 17/7/91 *//* equivalent code to BYTE etc */
				destmode = ptrmodeof (destmode);
				tload2regs (sourcemode, source, destmode, dest, FALSE, FALSE);
				movepointer (MOVEDIRN_STORE, I_STNL, 0, type, NULL, dest_devaccess);
			} else {
				if (preeval (sourcemode, source)) {
					if (NModeOf (source) == NM_POINTER) {
						sourcemode = ptrmodeof (sourcemode);	/* bug 1340 - 18/7/91 */
					}
					/*fprintf(outfile, "preeval stuff; genlocn is %lX, sourcemode is now %d\n", genlocn, sourcemode); */
					sourcemode = simplify (sourcemode, source);
				}
				moveopd (destmode, dest, sourcemode, source);
			}
			break;
			/*}}} */
			/*{{{  INT64 REAL64 on 16-bit machine */
		case S_INT64:
		case S_REAL64:
			moveqopd (destmode, dest, sourcemode, source);
			break;
			/*}}} */
			/*{{{  ARRAY */
		case S_ARRAY:
		case S_RECORD:
#ifdef OCCAM2_5
			if ((type == S_RECORD) && (bytesin (gettype (dest)) <= bytesperword)) {
				if (TagOf (gettype (dest)) == S_MOBILE) {
					/* literally directly */
					texpopd_main (sourcemode, source, regs, (TagOf (dest) == T_PREEVALTEMP));
					storeinopd (destmode, dest, 0, r - 1);
				} else {
					/*{{{  assign directly */
					tsimpleassign (S_INT, destmode, dest, sourcemode, source, regs);
					/*}}} */
				}
			} else
#endif
			{
				/*{{{  do a fake 'quad length' move */
				moveqopd (destmode, dest, sourcemode, source);
				/*}}} */
			}
			break;
			/*}}} */
		default:
			geninternal_is (GEN_ERROR_IN_ROUTINE, 3, "tsimpleassign");
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PRIVATE void tmultiassign (mdestlist, msourcelist)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tmultiassign takes a multiple - assignment tree, tptr, which has been
 *               reordered by the mapper, and generates code for it.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void tmultiassign (treenode * mdestlist, treenode * msourcelist)
{
	/*{{{  range check each assignment if neccessary */
	if (RANGECHECKING) {
		treenode *destlist = mdestlist, *sourcelist = msourcelist;
		while (!EndOfList (destlist)) {
			treenode *thisdest = ThisItem (destlist);
			if (ntypeof (thisdest) == S_ARRAY)
				tcheckdimensions (gettype (thisdest), gettype (ThisItem (sourcelist)));
			destlist = NextItem (destlist);
			sourcelist = NextItem (sourcelist);
		}
	}
	/*}}} */
	/*{{{  do the assignment */
	{
		treenode *destlist = mdestlist;
		treenode *sourcelist = msourcelist;
		treenode *regdest[MAXREGS];
		int nregtemps = 0, i;
		while (!EndOfList (destlist))
			/*{{{  move source to register, temporary, or directly to dest */
		{
			/*{{{  do this assignment */
			{
				treenode *dest = ThisItem (destlist);
				treenode *source = ThisItem (sourcelist);
				if (TagOf (source) == T_TEMP) {
					/*{{{  move to temporary */
					tsimpleassign (ntypeof (dest), P_TEMP, source, P_EXP, NDeclOf (source), MAXREGS - nregtemps);
					/*}}} */
				} else if (TagOf (source) == T_REGTEMP) {
					/*{{{  load to register */
					texpopd (P_EXP, NDeclOf (source), MANY_REGS);
					regdest[nregtemps++] = dest;
					/*}}} */
				} else {
					/*{{{  move directly to dest */
					tsimpleassign (ntypeof (dest), P_EXP, dest, P_EXP, source, MAXREGS - nregtemps);
					/*}}} */
				}
			}
			/*}}} */
			destlist = NextItem (destlist);
			sourcelist = NextItem (sourcelist);
		}
		/*}}} */
		/*{{{  store register temporaries in their destinations */
		for (i = nregtemps - 1; i >= 0; i--)
			storeinopd (P_EXP, regdest[i], 0, (MAXREGS - 1) - i);
		/*}}} */
		/*{{{  move temporaries to their destinations */
		destlist = mdestlist;
		sourcelist = msourcelist;
		while (!EndOfList (destlist)) {
			treenode *dest = ThisItem (destlist), *source = ThisItem (sourcelist);
			if (TagOf (source) == T_TEMP)
				tsimpleassign (ntypeof (dest), P_EXP, dest, P_TEMP, source, MANY_REGS);
			destlist = NextItem (destlist);
			sourcelist = NextItem (sourcelist);
		}
		/*}}} */
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void tassign (dest, source)*/
/*****************************************************************************
 *
 *  tassign generates code to assign source to dest.
 *          source and dest may be lists, in which case it is assumed that
 *          they have been reordered and temporaries inserted as necessary.
 *
 *****************************************************************************/
PUBLIC void tassign (treenode *dest, treenode *source)
{
#if 0
fprintf (stderr, "tassign: dest=%p, source=%p\n", dest, source);
#endif
	if (TagOf (dest) == S_LIST) {
		/*{{{  we have a multiple assignment */
		if (listitems (source) > 1) {
			tmultiassign (dest, source);
		} else {
			/*{{{  multi-valued function or valof */
			/* N.B. Here I assume that functions and valofs always have scalar results
			   - if they have vector results then hidden dimension size checking
			   code must be generated. */

			treenode *sourceitem = ThisItem (source);
			const int tag = TagOf (sourceitem);
			if (isspecification (sourceitem) || (tag == S_VALOF)) {
				tvalof (sourceitem, dest);
			} else if (tag == S_FINSTANCE) {
				if (TagOf (INameOf (sourceitem)) == N_PREDEFFUNCTION) {
					tpredef (sourceitem, dest);
				} else {
					const BOOL recursive = (nodetypeoftag (TagOf (sourceitem)) == INSTANCENODE) ? IRecursiveOf (sourceitem) : FALSE;
					const int temp = RECURSIVE_WS;

					tinstance (sourceitem);

					if (recursive) {
						genprimary (I_STL, temp);
					}

					/*{{{  store the register results */
					{
						treenode **regdests[MAXREGS];
						int nregresults;
						setregdests (&nregresults, regdests, dest, INameOf (sourceitem));
						if (nregresults > 0) {
							/* If assembly_output && 
							   ((alloc_strategy & ALLOC_NOCOMMENTS) == 0 */
							gen_func_results (nregresults);
							tstoreregs (regdests, nregresults);
						}
						if (recursive) {
							gensecondary (I_MRELEASE);
						}
					}
					/*}}} */
					/*{{{  move any results put into temporaries to their real destinations */
					{
						treenode *paramlist;
						for (paramlist = IParamListOf (sourceitem); !EndOfList (paramlist); paramlist = NextItem (paramlist)) {
							treenode *thisparam = ThisItem (paramlist);
							if (TagOf (thisparam) == S_FNACTUALRESULT) {
								treenode *p = HExpOf (thisparam);
								if (TagOf (p) == T_TEMP) {
									treenode *r = NDeclOf (p);
									tsimpleassign (ntypeof (r), P_EXP, r, P_TEMP, p, MANY_REGS);
								}
							}
						}
					}
					/*}}} */
				}
			} else {
				geninternal_is (GEN_ERROR_IN_ROUTINE, 5, "tassign");
			}
			/*}}} */
		}
		/*}}} */
	} else {
		const int type = ntypeof (dest);
#if 0
fprintf (stderr, "tassign: ntypeof (dest) = %d\n", type);
#endif

		if ((type == S_ARRAY) && RANGECHECKING) {
			tcheckdimensions (gettype (dest), gettype (source));
		}
#if 0
#ifdef MOBILES
		/* check for abbreviations of dynamic MOBILE CHANs */
		if ((TagOf (source) == S_ARRAYITEM) && isdynmobilearray (ASBaseOf (source)) &&
			(TagOf (MTypeOf (ARTypeOf (NTypeOf (ASBaseOf (source))))) == S_CHAN)) {

			tsimpleassign (type, P_EXP, dest, P_PTR, source, MANY_REGS);
		} else
#endif	/* MOBILES */
#endif
		tsimpleassign (type, P_EXP, dest, P_EXP, source, MANY_REGS);
	}
}

/*}}}*/

