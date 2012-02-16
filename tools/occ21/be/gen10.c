/* $Id: gen10.c,v 1.5 1997/06/06 14:22:51 mdp2 Exp $ */

/*
 *	code generator - channel input and output generation
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

/*#define DEBUG */
/*
 *  MDP changes:
 *               in taltcheck: report when result of ENBC, etc is thrown away
 *               in taltdisables: report that result of DISC, etc is thrown away
 *               in tlengthcheck: call throw_the_result_away
 */

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include "includes.h"
#include "instruct.h"
#include "generror.h"
#include "genhdr.h"
#include "trandef.h"
#include "chk4def.h"	/* for lookupname/findname */
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
#include "gen12def.h"
#include "code1def.h"
#include "genkroc.h"
#include "mobile_types.h"
/*}}}*/

/*{{{  private variables*/
/*#define MAX_NESTED_ALTS 10*//* There is now no hard limit */

PRIVATE treenode **replaltstack;
PRIVATE int replaltstacklen;
PRIVATE INT32 replaltvaluesbase;	/* WS offset of first saved replicator */

/* The following variables are only live while generating code for
   the enabling and disabling sequences, and since these can't be
   nested, there is no need to save the when recursing.
*/
PRIVATE int nalternatives;
PRIVATE int alternativecount;
PRIVATE BOOL inreplalt;
PRIVATE BOOL altcheckneeded;
PRIVATE BOOL altcheckdone;

PRIVATE int alt_repl_scope_level;

/* replaltvector is used when mapping enables, and when mapping bodies */
PRIVATE treenode *replaltvector;	/* Used in mapaltenable */
PRIVATE BOOL premapped_alt;	/* TRUE if mapping top level of an ALT */

PRIVATE treenode *intnodeptr = NULL;	/* used for processing counted arrays */

#define virtualinstring      "VIRTUAL.IN%"
#define virtualoutstring     "VIRTUAL.OUT%"
#define virtualoutbytestring "VIRTUAL.OUT.BYTE%"
#define virtualoutwordstring "VIRTUAL.OUT.WORD%"
#define virtualvinstring     "VIRTUAL.VIN%"	/* variableio 11/3/91 */
#define virtualvoutstring    "VIRTUAL.VOUT%"	/* variableio 11/3/91 */
/* well, i don't think we use these, but for completeness.. */
#define virtualxablestring   "VIRTUAL.XABLE%"
#define virtualxinstring     "VIRTUAL.XIN%"
#ifdef MOBILES
#define virtualminstring     "VIRTUAL.MIN%"
#define virtualmin64string   "VIRTUAL.MIN64%"
#define virtualminnstring    "VIRTUAL.MINN%"
#define virtualxminstring    "VIRTUAL.XMIN%"
#define virtualxmin64string  "VIRTUAL.XMIN64%"
#define virtualxminnstring   "VIRTUAL.XMINN%"
#define virtualmoutstring    "VIRTUAL.MOUT%"
#define virtualmout64string  "VIRTUAL.MOUT64%"
#define virtualmoutnstring   "VIRTUAL.MOUTN%"
#endif
#define virtualxendstring    "VIRTUAL.XEND%"
/*}}}*/

/*{{{  PRIVATE treenode *iocallname(ioinst)*/
PRIVATE treenode *iocallname (const int ioinst)
{
	const char *str = NULL;	/* initialised to shut up gcc's optimiser */
	switch (ioinst) {
	case I_IN:
		str = virtualinstring;
		break;
	case I_XABLE:
		str = virtualxablestring;
		break;
	case I_XIN:
		str = virtualxinstring;
		break;
	case I_XEND:
		str = virtualxendstring;
		break;
#if 0 /* def MOBILES */
	case I_MIN:
		str = virtualminstring;
		break;
	case I_MIN64:
		str = virtualmin64string;
		break;
	case I_MINN:
		str = virtualminnstring;
		break;
	case I_XMIN:
		str = virtualxminstring;
		break;
	case I_XMIN64:
		str = virtualxmin64string;
		break;
	case I_XMINN:
		str = virtualxminnstring;
		break;
	case I_MOUT:
		str = virtualmoutstring;
		break;
	case I_MOUT64:
		str = virtualmout64string;
		break;
	case I_MOUTN:
		str = virtualmoutnstring;
		break;
#endif
	case I_OUT:
		str = virtualoutstring;
		break;
	case I_OUTWORD:
		str = virtualoutwordstring;
		break;
	case I_OUTBYTE:
		str = virtualoutbytestring;
		break;
	case I_VIN:
		str = virtualvinstring;
		break;		/* variableio 11/3/91 */
	case I_VOUT:
		str = virtualvoutstring;
		break;		/* variableio 11/3/91 */
	}
	DEBUG_MSG (("In iocallname: str = %s\n", str));
	return vlibentry (processlibname (str, 1, vlibsuffix));
}

/*}}}*/
/*{{{  PRIVATE mapioop(ioinst, iobycall)*/
PRIVATE void mapioop (const int ioinst, const BOOL usecall)
{
	DEBUG_MSG (("In mapioop: ioinst = %d, usecall = %d\n", ioinst, usecall));
	if (usecall) {
		treenode *nptr = iocallname (ioinst);
		INT32 ws, vs;
		getprocwsandvs (nptr, &ws, &vs);
		datasize = max_INT32 (datasize, (ws + REG_PARAMS + INS_EXTRA));
		/* vs will always be zero */
		add_to_libentries (nptr);
	} else {
		datasize = max_INT32 (datasize, DS_IO);
		if (ioinst == I_OUTWORD || ioinst == I_OUTBYTE) {
			reservelowworkspace (1);
		}
	}
}

/*}}}*/
/*{{{  PRIVATE treenode *fixporttype(portuse)*/
/*****************************************************************************
 *
 *  fixporttype converts a port namenode (used in the tree 'portuse')
 *              from being a 'PORT OF t' to being a 't'.
 *              This unpleasant trick allows us to use the general
 *              assign code on port input and port output.
 *              The original type tree is returned so that it can be
 *              put back at the earliest possible opportunity (see
 *              routine 'restoreporttype').
 *
 *****************************************************************************/
PRIVATE treenode *fixporttype (treenode * const portuse, treenode *** const typeaddr)
{
	treenode *portname = nameof (portuse);
	treenode **porttype = NTypeAddr (portname);
	treenode *oldtype;
	treenode **oldtypeaddr;
#if 0
fprintf (stderr, "gen10: fixporttype: *porttype =");
printtreenl (stderr, 0, *porttype);
#endif
	while (TagOf (*porttype) == S_ARRAY) {
		porttype = ARTypeAddr (*porttype);
	}
	oldtype = *porttype;
	oldtypeaddr = porttype;	/* INSdi03067 */
	*porttype = ProtocolOf (*porttype);
	SetNVDevice (portname, TRUE);
	*typeaddr = oldtypeaddr;	/* INSdi03067 */
	return oldtype;
}

/*}}}*/
/*{{{  PRIVATE void restoreporttype(portuse, porttype)*/
/*****************************************************************************
 *
 *  restoreporttype sets the type field of the port namenode
 *                  (used in the tree 'portuse') to 'porttype'.
 *
 *****************************************************************************/
PRIVATE void restoreporttype (treenode * const portuse, treenode * const porttype, treenode ** const oldtypeaddr)
{
	treenode *portname = nameof (portuse);
	treenode **type = NTypeAddr (portname);

#if 0
	printf ("restoreporttype: portuse: ");
	printtree (stdout, 0, portuse);
	printf ("\n, portname: ");
	printtree (stdout, 0, portname);
	printf ("\n, porttype: ");
	printtree (stdout, 0, porttype);
	printf ("\n, oldtypeaddr: ");
	printtree (stdout, 0, *oldtypeaddr);
	printf ("\n, type: ");
	printtree (stdout, 0, *type);
	printf ("\n");
#endif

	*oldtypeaddr = porttype;	/* restore the original copy *//* INSdi03067 */

	while (TagOf (*type) == S_ARRAY)
		type = ARTypeAddr (*type);

	if (TagOf (*type) != S_PORT)
	 {			/* INSdi03067 */
		/* a temp has been inserted, which doesn't share a common type tree */
		*type = newtypenode (S_PORT, LocnOf (porttype), NULL, *type);	/* INSdi03067 */
	}
}

/*}}}*/
/*{{{  PRIVATE int numberofalternatives(tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  numberofalternatives count the number of alternatives in an ALT
 *                       construct: a replicated alternative counts as 1.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE int numberofalternatives (treenode * tptr)
{
	while (isspecification (tptr))
		tptr = DBodyOf (tptr);
	switch (TagOf (tptr)) {
	case S_ALTERNATIVE:
		return 1;
	case S_ALT:
	case S_PRIALT:
		{
			int count = 0;
			for (tptr = CBodyOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr)) {
				count += numberofalternatives (ThisItem (tptr));
			}
			return count;
		}
	case S_REPLALT:
	case S_PRIREPLALT:
		return numberofalternatives (ReplCBodyOf (tptr));
	default:
		badtag (genlocn, TagOf (tptr), "numberofalternatives");
	}
	return (0);		/* Not reached */
}

/*}}}*/
/*{{{  PRIVATE int countnestedalts(tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  countnestedalts counts the max number of nested levels in an ALT
 *
 *****************************************************************************/
/*}}}*/
#define NESTED_ALTS  0		/* count all levels of the ALT */
#define NESTED_REPLS 1		/* just count the replicated levels */
PRIVATE int countnestedalts (treenode * tptr, int flag)
{
	DEBUG_MSG (("countnestedalts: %s\n", flag == NESTED_ALTS ? "nested ALTs" : "nested repls"));
	while (isspecification (tptr))
		tptr = DBodyOf (tptr);
	switch (TagOf (tptr)) {
	case S_ALTERNATIVE:
		if (flag == NESTED_ALTS)
			return 1;
		else
			return 0;
	case S_ALT:
	case S_PRIALT:
		{
			int count = 0;
			for (tptr = CBodyOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr)) {
				count = (int) max_INT32 (count, countnestedalts (ThisItem (tptr), flag));
			}
			if (flag == NESTED_ALTS) {
				return (count + 1);
			} else {
				/* (flag == NESTED_REPLS) */
				return (count);
			}
		}
	case S_REPLALT:
	case S_PRIREPLALT:
		return (countnestedalts (ReplCBodyOf (tptr), flag) + 1);
	default:
		badtag (genlocn, TagOf (tptr), "countnestedalts");
	}
	return (0);		/* Not reached */
}

/*}}}*/
/*{{{  PRIVATE BOOL hastruealtguard(tptr)*/
/*****************************************************************************
 *
 *  hastruealtguard returns TRUE if the ALT construct tptr has at least
 *                  one TRUE Boolean guard
 *
 *****************************************************************************/
PRIVATE BOOL hastruealtguard (treenode * tptr)
{
	while (tptr != NULL) {
		tptr = skipspecifications (tptr);
		switch (TagOf (tptr)) {
			/*{{{  S_ALT S_PRIALT */
		case S_ALT:
		case S_PRIALT:
			for (tptr = CBodyOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr)) {
				if (hastruealtguard (ThisItem (tptr))) {
					return TRUE;
				}
			}
			break;
			/*}}} */
			/*{{{  S_ALTERNATIVE */
		case S_ALTERNATIVE:
			return istrueguard (AltGuardOf (tptr));
			/*}}} */
			/*{{{  S_REPLALT S_PRIREPLALT */
		case S_REPLALT:
		case S_PRIREPLALT:
			tptr = ReplCBodyOf (tptr);
			break;
			/*}}} */
		}
	}
	return FALSE;
}

/*}}}*/
/*{{{  PRIVATEPARAM int do_alt_check_enabling_functions*/
PRIVATEPARAM int do_alt_check_enabling_functions (treenode ** tptr, void *voidptr)
{
	treenode *const nptr = *tptr;
	treenode *const fptr = voidptr;	/* namenode of fn */
#if 0
	printf ("do_alt_check_enabling_functions: Looking at %s, \"%s\", scope: %d\n",
		itagstring (TagOf (nptr)), WNameOf (NNameOf (nptr)), NScopeOf (nptr));
#endif
	if ((TagOf (nptr) == N_REPL) && (NScopeOf (nptr) >= alt_repl_scope_level)) {
		msg_out_ss (SEV_FATAL, GEN, GEN_REPL_FN_IN_ALT_GUARD, genlocn, WNameOf (NNameOf (fptr)), WNameOf (NNameOf (nptr)));
	}
	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PUBLIC void alt_check_enabling_functions*/
PUBLIC void alt_check_enabling_functions (treenode * const tptr)
{
	if (alt_repl_scope_level != (-1)) {	/* inside a repl ALT */
#if 0
		printf ("Found FINSTANCE: %s, scope: %d\n", WNameOf (NNameOf (INameOf (tptr))), NScopeOf (INameOf (tptr)));
#endif
		if (NScopeOf (INameOf (tptr)) > alt_repl_scope_level) {
			walk_free_vars (INameOf (tptr), do_alt_check_enabling_functions, INameOf (tptr) /*LocnAddr(tptr) */ );
		}
	}
}

/*}}}*/
/*{{{  PRIVATE void modify_alt_body (treenode *alt)*/
/*
 *	this is used to modify the ALT body for an enhanced ALT enable -- since we
 *	may not actually deschedule during the ALT -- and we should.
 */
PRIVATE void modify_alt_body (treenode *alt)
{
	treenode *list, *resched, *tmp;
	static wordnode *reschedule = NULL;
	const int old = switch_to_real_workspace ();

	if (!reschedule) {
		reschedule = lookupword ("RESCHEDULE", 10);
	}
#if 0
fprintf (stderr, "modify_alt_body: body =");
printtreenl (stderr, 4, AltBodyOf (alt));
fprintf (stderr, "modify_alt_body: AltChanExpOf(alt) =");
printtreenl (stderr, 4, AltChanExpOf (alt));
fprintf (stderr, "modify_alt_body: AltTimeExpOf(alt) =");
printtreenl (stderr, 4, AltTimeExpOf (alt));
#endif
	resched = NULL;
	if (!AltChanExpOf (alt) || AltTimeExpOf (alt)) {
		/* skip guard or timeout guard -- generate reschedule */
		resched = newinstancenode (S_PINSTANCE, NOPOSN, lookupname (reschedule, findname (reschedule)), NULL);
	} else {
		/* input guard -- generate reschedule if only 1 input */
		if (TagOf (AltBodyOf (alt)) != S_SEQ) {
			/* um, dunno */
			switch_to_prev_workspace (old);
			return;
		}
		list = CBodyOf (AltBodyOf (alt));
		if (TagOf (list) != S_LIST) {
			/* um, dunno */
			switch_to_prev_workspace (old);
			return;
		}
		/* ThisItem(list) should be the input guard */
		switch (TagOf (ThisItem (list))) {
		case S_INPUT:
		case S_X_INPUT:
		case S_TAGGED_INPUT:
		case S_X_TAGGED_INPUT:
		case S_CASE_INPUT:
		case S_X_CASE_INPUT:
		case S_X_INPUT_OUTPUT:
			/* single input, tagged input, CASE input or extended input, insert reschedule */
			resched = newinstancenode (S_PINSTANCE, NOPOSN, lookupname (reschedule, findname (reschedule)), NULL);
			break;
		default:
			return;
		}
	}
	if (!resched) {
		switch_to_prev_workspace (old);
		return;
	}
#if 0
fprintf (stderr, "modify_alt_body: SKIP guard: resched =");
printtreenl (stderr, 4, resched);
#endif
	/* insert resched node into ALT body */
	if (TagOf (AltBodyOf (alt)) == S_SEQ) {
		list = CBodyOf (AltBodyOf (alt));
		if (TagOf (list) != S_LIST) {
			/* this should never happen */
			geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "modify_alt_body");
			switch_to_prev_workspace (old);
			return;
		}
		tmp = newlistnode (S_LIST, NOPOSN, resched, NextItem (list));
		SetRight (list, tmp);
	} else {
		tmp = newcnode (S_SEQ, NOPOSN, newlistnode (S_LIST, NOPOSN, resched, newlistnode (S_LIST, NOPOSN, AltBodyOf (alt), NULL)));
		SetAltBody (alt, tmp);
	}
	switch_to_prev_workspace (old);
#if 0
fprintf (stderr, "modify_alt_body: done, body =");
printtreenl (stderr, 4, AltBodyOf (alt));
#endif
	return;
}
/*}}}*/
/*{{{  PRIVATEPARAM void mapaltenables(tptr)*/
/*****************************************************************************
 *
 *  mapaltenables allocates workspace for enabling each guard in
 *                the ALT tree 'tptr'.
 *
 *****************************************************************************/
PRIVATEPARAM void mapaltenables (treenode *tptr)
{
	const BOOL saved_premapped_alt = premapped_alt;
	premapped_alt = FALSE;
	DEBUG_MSG (("mapaltenables: replaltstacklen is %d\n", replaltstacklen));
	switch (TagOf (tptr)) {
	default:		/* Must be a leading specification */
		/* modified so that this calls mappreprocess now - bug 776 5/10/90 */
		mapdeclandbody (tptr, mapaltenables, TRUE, TRUE);
		break;
		/*{{{  S_ALT S_PRIALT */
	case S_ALT:
	case S_PRIALT:
		SetCTemp (tptr, replaltvector);
		if (!saved_premapped_alt) {
			mappreprocess (tptr);
		}
		mapconstruction (CBodyOf (tptr), mapaltenables);
		break;
		/*}}} */
		/*{{{  S_REPLALT S_PRIREPLALT */
	case S_REPLALT:
	case S_PRIREPLALT:
		{
			const INT32 oldweight = loop_weight;
			const int oldscope = alt_repl_scope_level;
			SOURCEPOSN locn = LocnOf (tptr);

			uploop_weight ((INT32) 2);	/* Enable + disables */
			if (alt_repl_scope_level == (-1)) {
				alt_repl_scope_level = NScopeOf (ReplCNameOf (tptr));
			}
			SetReplCTemp (tptr, replaltvector);
			replaltstacklen++;
			DEBUG_MSG (("mapaltenables: found a repl, replaltstacklen is now %d\n", replaltstacklen));
			if (!saved_premapped_alt) {
				mappreprocess (tptr);
			}
			if ((reverse_alt_disable && (TagOf (tptr) == S_PRIREPLALT)) || enhanced_alt_enable) {
				treenode *revstart, *revstep;

				/* better create some temporaries and map them out -- already been type-checked for INT-ness */
#if 0
fprintf (stderr, "gen10: mapaltenables.  orig. start =");
printtreenl (stderr, 4, ReplCStartExpOf (tptr));
fprintf (stderr, "gen10: mapaltenables.  orig. step =");
if (ReplCStepExpOf (tptr)) {
printtreenl (stderr, 4, ReplCStepExpOf (tptr));
} else { fprintf (stderr, " (null)\n"); }
#endif
				revstart = newdopnode (S_ADD, locn, copytree (ReplCStartExpOf (tptr), be_lexlevel), ReplCStepExpOf (tptr) ?
						newdopnode (S_MULT, locn, newdopnode (S_SUBTRACT, locn, copytree (ReplCLengthExpOf (tptr), be_lexlevel), newconstant (1), S_INT),
							copytree (ReplCStepExpOf (tptr), be_lexlevel), S_INT) :
						newdopnode (S_SUBTRACT, locn, copytree (ReplCLengthExpOf (tptr), be_lexlevel), newconstant (1), S_INT), S_INT);
				revstep = ReplCStepExpOf (tptr) ? newmopnode (S_UMINUS, locn, copytree (ReplCStepExpOf (tptr), be_lexlevel), S_INT) : newconstant (-1);
				/*
				 *	revstart = tptr[start] + ((tptr[length] - 1) * tptr[step]);
				 *	revstep = -(tptr[step])
				 */
				revstart = foldexp (revstart);
				revstep = foldexp (revstep);
				SetReplCRAltStart (tptr, revstart);
				SetReplCRAltStep (tptr, revstep);
#if 0
fprintf (stderr, "gen10: mapaltenables.  revstart =");
printtreenl (stderr, 4, revstart);
fprintf (stderr, "gen10: mapaltenables.  revstep =");
printtreenl (stderr, 4, revstep);
#endif
			}
			maprepl (tptr, mapaltenables);
			if ((reverse_alt_disable && (TagOf (tptr) == S_PRIREPLALT)) || enhanced_alt_enable) {
				/* map out the expressions */
				mapexp (ReplCRAltStartAddr (tptr));
				mapexp (ReplCRAltStepAddr (tptr));
			}
			replaltstacklen--;
			/*addusecount(replaltvector, NVUseCountOf(ReplCNameOf(tptr))); */
			loop_weight = oldweight;
			alt_repl_scope_level = oldscope;
		}
		break;
		/*}}} */
		/*{{{  S_ALTERNATIVE */
	case S_ALTERNATIVE:
		{
			treenode **const guard = AltGuardAddr (tptr);
			/*{{{  map for enable/disable */
			if (enhanced_alt_enable) {
				/* insert a RESCHEDULE after the first communication, if:
				 *  -  it is a SKIP guard
				 *  -  it is a timeout guard
				 *  -  it is an input guard with only one input
				 */
				modify_alt_body (tptr);
			}
			mappreprocess (tptr);
			{
				/*printf("mapenables: alternative: repl scope level is %d\n", alt_repl_scope_level); */
				if (AltChanExpOf (tptr) == NULL) {
					mapbool (guard);
				} else {
					treenode **chanexp = AltChanExpAddr (tptr);	/* added 2/11/90 bug 779 */
					treenode **enabler;
					int mode = P_EXP;

					if (AltTimeExpOf (tptr) == NULL) {
						/*{{{  mask off virtual i/o bit if required */
						/*const BOOL usecall = iobycall && maybevirtualchan(*channel); */
						/*printf("mapaltenables: realign_virtual_channels? %d, virtual? %d\n",
						   realign_virtual_channels, maybevirtualchan(*chanexp)); */
						if (realign_virtual_channels && maybevirtualchan (*chanexp)) {
							if (chanaspointer) {
								/* bug INSdi02103 */
								*chanexp = newdopnode (S_BITAND, genlocn, *chanexp,
										       newconstant ((int) (-bytesperword)), S_INT);
							} else {
								/* Cannot do this, cos I'm too lazy to implement it, and we
								   think that in practice the zw (!chanaspointer) option will
								   always be used when iobycall is FALSE */
								/* This call to memo_err makes sure that we only issue the error msg
								   once per compilation unit */
								if (memo_err (GEN, GEN_ENABLE_CHAN_ALIGNMENT_PROBLEM, NOPOSN))
									msg_out (SEV_WARN, GEN, GEN_ENABLE_CHAN_ALIGNMENT_PROBLEM, LocnOf (tptr));
							}
						}
						/*}}} */
						enabler = chanexp;
						mode = chanaspointer ? P_EXP : P_PTR;
					} else {	/* timer input */
						mapexpopd (P_EXP, chanexp);	/* Bug 288 22/5/90 */
						enabler = AltTimeExpAddr (tptr);
					}

#if 0
					if (T9000_alpha_badalt (&tx_global)) {
						/*{{{  T9000_alpha_badalt */
						mapexpopd (P_EXP, guard);
						mapexpopd (mode, enabler);
						if (!issimplelocal (*enabler, be_lexlevel)) {
							/* need to re-load channel for 'cj' trick */
							*enabler = gettemp (*enabler, NM_WORKSPACE);
							upusecount (*enabler, 2);
							freetemp (*enabler);
						}
						/*}}} */
					} else
#endif
					if (!istrueguard (*guard) && cancauseerror (*enabler)) {
						mapexpopd (P_EXP, guard);
						mapexpopd (mode, enabler);
					} else {
						mapload2regs (mode, enabler, P_EXP, guard);
					}
				}
			}
			/*}}} */
			/*SetAltTemp(tptr, replaltvector); */
			addusecount (replaltvector, replaltstacklen);
		}
		break;
		/*}}} */
	}
	premapped_alt = saved_premapped_alt;
}

/*}}}*/
/*{{{  PRIVATEPARAM void mapaltbodies*/
/*****************************************************************************
 *
 *  mapaltbodies allocates workspace for the body of each guarded process in
 *                the ALT tree 'tptr'.
 *
 *****************************************************************************/
PRIVATEPARAM void mapaltbodies (treenode * tptr)
{
	treenode *const save_vsp_nptr = enclosing_vsp_nptr;
	int *const scope = save_scope ();
	tptr = resurrect_specs (tptr);
	switch (TagOf (tptr)) {
		/*{{{  S_ALT S_PRIALT */
	case S_ALT:
	case S_PRIALT:
		mapconstruction (CBodyOf (tptr), mapaltbodies);
		break;
		/*}}} */
		/*{{{  S_REPLALT S_PRIREPLALT */
	case S_REPLALT:
	case S_PRIREPLALT:
		{
			/* Don't include the enabling and disabling loops in the usage
			   calculation for the replaltvector */
			INT32 enable_count = NVUseCountOf (ReplCNameOf (tptr));
			resurrect_var (ReplCNameOf (tptr));
			mapaltbodies (ReplCBodyOf (tptr));
			addusecount (ReplCTempOf (tptr), NVUseCountOf (ReplCNameOf (tptr)) - enable_count);
		}
		break;
		/*}}} */
		/*{{{  S_ALTERNATIVE */
	case S_ALTERNATIVE:
#if 0
		/* Have to be careful, because alternatives
		   will not yet have been through mappreprocess. */
		{
			treenode *input = AltInputOf (tptr);
			/*{{{  map the deferred specification and the input */
			{
				/* mapdeclandbody will descope the specs before the input
				 * so we must resurrect them before we process the body
				 */
				/* modified so that this calls mappreprocess now - bug 776 5/10/90 */
				mapdeclandbody (input, mapinput, TRUE, TRUE);
				input = resurrect_specs (input);
			}
			/*}}} */
			/*{{{  map the body */
			{
				switch (inputtypeof (input)) {
					/*{{{  case input */
				case INP_CASE_INPUT:
					break;	/* There is no body */
					/*}}} */
					/*{{{  everything else */
				default:
					mapprocess (AltBodyOf (tptr));
					break;
					/*}}} */
				}
			}
			/*}}} */
		}
#else
		mapdeclandbody (AltBodyOf (tptr), mapprocess, TRUE, TRUE);
#endif
		break;
		/*}}} */
	}
	restore_scope (scope);
	enclosing_vsp_nptr = save_vsp_nptr;
}

/*}}}*/
/*{{{  PRIVATE void taltcheck()*/
PRIVATE void taltcheck (const BOOL inskippingalt, const BOOL known_true_guard)
{
	if (altcheckneeded) {
		/*{{{  do a check */
		if (known_true_guard || (!inreplalt && alternativecount == 1)) {
			genprimary (I_STL, 0);
		} else {
			genprimary (I_LDL, 0);
			gensecondary (I_OR);
			if (!inreplalt && !inskippingalt && alternativecount == nalternatives) {
				genprimary (I_LDC, 1);
				gensecondary (I_CCNT1);
				altcheckdone = TRUE;
			} else {
				genprimary (I_STL, 0);
			}
		}
		/*}}} */
	} else {
		throw_the_result_away ();
	}
}

/*}}}*/
/*{{{  frmb: some private state for labeling the enhanced ALT enabling sequence*/
#define MAX_ALTERNATIVES (1024)			/* a replicated ALT costs 1 */
static int eae_labels[MAX_ALTERNATIVES];
static int eae_count = 0;
static int eae_isreplalt = 0;
static treenode *eae_replptrs[MAX_ALTERNATIVES];	/* NULL means not */

static int prealt_labels[MAX_ALTERNATIVES];
static int prealt_targets[MAX_ALTERNATIVES];
static int prealt_count = 0;
static int prealt_isreplalt = 0;
static treenode *prealt_replptrs[MAX_ALTERNATIVES];	/* NULL means not */

/*}}}*/
/*{{{  PRIVATE void tprealtfixups (void)*/
/* generates code to move replicator values from the replicator workspace into the ALTer's workspace */
PRIVATE void tprealtfixups (void)
{
	int i;

	for (i=0; i<prealt_count; i++) {
		/* only inserted when there is a replicated ALT present */
		treenode *repllist = prealt_replptrs[i];
		int idx = 0;

		if (prealt_labels[i] != NO_LABEL) {
			treenode *next = NULL;

			setlab (prealt_labels[i]);
			/* select current replicator values */
#if 0
fprintf (stderr, "tprealtfixups: repllist =");
printtreenl (stderr, 4, repllist);
#endif
			for (; !EndOfList (repllist); repllist = next) {
				treenode *thisrepl = ThisItem (repllist);
				next = NextItem (repllist);

				loadname (thisrepl, REPL_BASE);
				genprimary (I_STL, replaltvaluesbase + idx);
				gencomment1 ("INT saved repl %d", idx);
				idx++;

				/* free list item */
				freenode (&repllist);
			}

			/* then branch into guarded process */
			genbranch (I_J, prealt_targets[i]);
		}
	}
	prealt_count = 0;
	return;
}
/*}}}*/
/*{{{  PRIVATEPARAM void taltpreenables (treenode *tptr, void *const voidptr, const INT32 dummy)*/
/*
 * taltpreenables tests all the guards in an ALT or PRI ALT for readyness.
 */
PRIVATEPARAM void taltpreenables (treenode *tptr, void *const voidptr, const INT32 dummy)
{
	treenode *specsptr = tptr;
	treenode *aptr = tspecs (tptr);

	switch (TagOf (aptr)) {
		/*{{{  S_ALT, S_PRIALT*/
	case S_ALT:
		/* do ALT pre-enabling in reverse (for non-loops anyway), adds a bit of variation */
		{
			const int old = switch_to_temp_workspace ();
			const int old_prealt_isreplalt = prealt_isreplalt;
			treenode *rlist = NULL;

			/* build the reversed list */
			prealt_isreplalt = 0;
			for (aptr = CBodyOf (aptr); !EndOfList (aptr); aptr = NextItem (aptr)) {
				rlist = addtofront (ThisItem (aptr), rlist);
			}
			for (; !EndOfList (rlist); rlist = NextItem (rlist)) {
				taltpreenables (ThisItem (rlist), voidptr, dummy);
			}
			prealt_isreplalt = old_prealt_isreplalt;
			switch_to_prev_workspace (old);
		}
		break;
	case S_PRIALT:
		{
			const int old_prealt_isreplalt = prealt_isreplalt;

			prealt_isreplalt = 0;
			for (aptr = CBodyOf (aptr); !EndOfList (aptr); aptr = NextItem (aptr)) {
				taltpreenables (ThisItem (aptr), voidptr, dummy);
			}
			prealt_isreplalt = old_prealt_isreplalt;
		}
		break;
		/*}}}*/
		/*{{{  S_ALTERNATIVE*/
	case S_ALTERNATIVE:
		{
			int target = AltLabelOf (aptr);
			treenode *const guard = AltGuardOf (aptr);
			int skiplab = NO_LABEL;

#if 0
fprintf (stderr, "taltpreenables: ALTERNATIVE: replaltstacklen = %d\n", replaltstacklen);
#endif
			if (replaltstacklen > 0) {
				int i;
				treenode *repllist = NULL;
				int old;

				/* will need to recover replicator values if this one triggers */
				prealt_labels[prealt_count] = newlab ();
				target = prealt_labels[prealt_count];

				if (AltLabelOf (aptr) == NO_LABEL) {
					int tlab = newlab ();

					SetAltLabel (aptr, tlab);
					prealt_targets[prealt_count] = tlab;
				} else {
					prealt_targets[prealt_count] = AltLabelOf (aptr);
				}

				/* build the replicator list here */
				old = switch_to_real_workspace ();
				for (i=replaltstacklen - 1; i>=0; i--) {
					repllist = newlistnode (S_LIST, NOPOSN, replaltstack[i], repllist);
				}
				switch_to_prev_workspace (old);

				prealt_replptrs[prealt_count] = repllist;
				prealt_count++;
			} else if (target == NO_LABEL) {
				target = newlab ();
				SetAltLabel (aptr, target);
			}

			new_occam_line (aptr, TRUE, TRUE, FALSE);
			temp_clear_all_evaluated ();

			tpreexp (guard);
			if (AltChanExpOf (aptr) == NULL) {
				/*{{{  SKIP guard*/
				if (istrueguard (guard)) {
					loadconstant (1);
				} else {
					texpopd (P_EXP, guard, MANY_REGS);
				}
				genloadlabptr (target, NOLAB, "PROCESS PTR");
				gencomment0 (".MAGIC PREENABLE");
				gensecondary (I_ENBS3);
				/*}}}*/
			} else if (AltTimeExpOf (aptr) == NULL) {
				/*{{{  channel guard*/
				treenode *inputchannel = AltChanExpOf (aptr);

				if (TagOf (inputchannel) == T_TEMP) {
					inputchannel = NDeclOf (inputchannel);
				}
				tpreexp (inputchannel);
				if (!istrueguard (guard) && cancauseerror (inputchannel)) {
					/* hand-skip guard if enabler can cause an error */
					skiplab = newlab ();
					tguard (guard, TRUE, skiplab);
					texpopd (chanaspointer ? P_EXP : P_PTR, inputchannel, MANY_REGS);
					loadconstant (1);
				} else {
					tload2regs (chanaspointer ? P_EXP : P_PTR, inputchannel, P_EXP, guard, FALSE, TRUE);
				}
				genloadlabptr (target, NOLAB, "PROCESS PTR");
				gencomment0 (".MAGIC PREENABLE");
				gensecondary (I_ENBC3);
				/*}}}*/
			} else {
				/*{{{  timeout guard*/
				treenode *time = AltTimeExpOf (aptr);

				tpreexp (AltChanExpOf (aptr));
				tpreexp (time);
				if (!istrueguard (guard) && cancauseerror (time)) {
					/* hand-skip guard if enabler can cause an error */
					skiplab = newlab ();
					tguard (guard, TRUE, skiplab);
					texpopd (P_EXP, time, MANY_REGS);
					loadconstant (1);
				} else {
					tload2regs (P_EXP, time, P_EXP, guard, FALSE, TRUE);
				}
				genloadlabptr (target, NOLAB, "PROCESS PTR");
				gencomment0 (".MAGIC PREENABLE");
				gensecondary (I_ENBT3);
				/*}}}*/
			}
			if (skiplab != NO_LABEL) {
				setlab (skiplab);
			}
			throw_the_result_away ();
		}
		break;
		/*}}}*/
		/*{{{  S_REPLALT S_PRIREPLALT */
	case S_REPLALT:
	case S_PRIREPLALT:
		{
			const BOOL savedinreplalt = inreplalt;
			const int old_prealt_isreplalt = prealt_isreplalt;

#if 0
fprintf (stderr, "taltpreenables: REPLALT/PRIREPLALT: replaltstacklen = %d.  aptr =", replaltstacklen);
printtreenl (stderr, 4, aptr);
#endif
			tpreexp (ReplCStartExpOf (aptr));
			tpreexp (ReplCLengthExpOf (aptr));
			if (ReplCStepExpOf (aptr)) {
				tpreexp (ReplCStepExpOf (aptr));
			}

			inreplalt = TRUE;
			prealt_isreplalt = 1;
			replaltstack[replaltstacklen] = ReplCNameOf (aptr);
			replaltstacklen++;
			trepl (aptr, taltpreenables, voidptr, dummy, FALSE);
			replaltstacklen--;
			prealt_isreplalt = old_prealt_isreplalt;
			inreplalt = savedinreplalt;

		}
		break;
		/*}}}*/
	}
	tdespecs (specsptr);
}
/*}}}*/
/*{{{  PRIVATEPARAM void taltenables*/
/*****************************************************************************
 *
 *  taltenables enables all the guards in the ALT or replicated ALT tree tptr,
 *              including those of any nested alternatives.
 *
 *****************************************************************************/
PRIVATEPARAM void taltenables (treenode * tptr, void *const voidptr, const INT32 dummy)
{
	treenode *specsptr = tptr;
	treenode *aptr;

	aptr = tspecs (tptr);
	DEBUG_MSG (("taltenables\n"));
	switch (TagOf (aptr)) {
		/*{{{  S_ALT S_PRIALT */
	case S_ALT:
	case S_PRIALT:
		{
			const int old_eae_isreplalt = eae_isreplalt;

			eae_isreplalt = 0;	/* not in a replicated alt anymore */
			DEBUG_MSG (("taltenables: found a nested ALT or PRIALT\n"));
			for (aptr = CBodyOf (aptr); !EndOfList (aptr); aptr = NextItem (aptr)) {
				taltenables (ThisItem (aptr), voidptr, dummy);
			}
			eae_isreplalt = old_eae_isreplalt;
		}
		break;
		/*}}} */
		/*{{{  S_ALTERNATIVE */
	case S_ALTERNATIVE:
		{
			treenode *const guard = AltGuardOf (aptr);
			BOOL inskippingalt = FALSE;
			BOOL isplaced = FALSE;

			DEBUG_MSG (("taltenables: found a branch of the ALT\n"));
			alternativecount++;

			new_occam_line (aptr, TRUE, TRUE, FALSE);
			/* if (debugoutput)
			   coder_genlocate(aptr, TRUE); */
			/* Added 27/10/92 for a comment in the zb display */

			/* bug INSdi02047 05/05/93 */
			temp_clear_all_evaluated ();	/* bug TS/1900 22/10/92 */

			tpreexp (guard);

			if (AltChanExpOf (aptr) == NULL) {
				/*{{{  skip guard */
				tbool (guard, MANY_REGS);
				if (enhanced_alt_enable && (replaltstacklen < 2)) {
					int tlab = newlab ();

					eae_labels[eae_count] = tlab;
					eae_replptrs[eae_count] = NULL;
					eae_count++;
					genloadlabptr (tlab, NOLAB, "DISABLING PTR");
					gensecondary (I_ENBS3);
				} else {
					gensecondary (I_ENBS);
				}
				taltcheck (inskippingalt, FALSE);
				/*}}} */
			} else {
				int skiplab = NO_LABEL;	/* bug TS/1827 14/08/92 */
				treenode *enabler;
				int mode = P_EXP;

				if (AltTimeExpOf (aptr) == NULL) {
					/*{{{  set up enabling channel */
					treenode *inputchannel = AltChanExpOf (aptr);
					/* Don't try and preevaluate it here */
					if (TagOf (inputchannel) == T_TEMP) {
						inputchannel = NDeclOf (inputchannel);
					}
					enabler = inputchannel;
					if ((nodetypeoftag (TagOf (gettype (enabler))) == TYPENODE) && (TypeAttrOf (gettype (enabler)) & TypeAttr_placed)) {
						isplaced = TRUE;
					}
					mode = chanaspointer ? P_EXP : P_PTR;
					/*}}} */
				} else {
					/*{{{  timer alt */
					treenode *time = /*RHSOf(input) */ AltTimeExpOf (aptr);
					tpreexp ( /*LHSOf(input) */ AltChanExpOf (aptr));	/* Bug 288 22/5/90 */

					/* Don't try and preevaluate it here */
#if 0
					if (T9000_alpha_badalt (&tx_global) && (TagOf (time) == T_TEMP)) {
						time = NDeclOf (time);
					}
#endif

					enabler = time;
					/*}}} */
				}

				tpreexp (enabler);
#if 0
				if (T9000_alpha_badalt (&tx_global)) {
					/*{{{  T9000_alpha_badalt */
					/* strictly we only need to do this when disabling,
					   but we do it here too so that the mapper doesn't
					   get confused */
					if (!istrueguard (guard)) {
						skiplab = newlab ();	/* bug TS/1827 14/08/92 */
						inskippingalt = TRUE;
						tguard (guard, TRUE, skiplab);
					}
					/* This isn't necessary cos we only evaluate this once */
					/* If we need to re-instate it, we need to remove the
					   call to NDeclOf(...) when setting up inputchannel */
					/*mode = simplify(mode, enabler); */
					texpopd (mode, enabler, MANY_REGS);
					loadconstant (1);
					/*}}} */
				} else
#endif
				if (!istrueguard (guard) && cancauseerror (enabler)) {
					skiplab = newlab ();	/* bug TS/1827 14/08/92 */
					inskippingalt = TRUE;
					tguard (guard, TRUE, skiplab);
					texpopd (mode, enabler, MANY_REGS);
					loadconstant (1);
				} else {
					tload2regs (mode, enabler, P_EXP, guard, FALSE, TRUE);
				}
#if 0
				/*{{{  T9000_gamma_carryin */
				if ((AltTimeExpOf (aptr) != NULL) && T9000_gamma_carryin (&tx_global)) {
					genprimary (I_LDC, 0);
					gensecondary (I_SUB);
					gencomment0 ("Set carry flag for ALU Adder CarryIn bug");
				}
				/*}}} */
#endif
				/*{{{  frmb: enhanced ALT enabling sequence*/
				if (enhanced_alt_enable && !isplaced && (replaltstacklen < 2)) {
					int tlab = newlab ();

					eae_labels[eae_count] = tlab;
					eae_replptrs[eae_count] = NULL;
					eae_count++;
					genloadlabptr (tlab, NOLAB, "DISABLING PTR");
					gensecondary ((AltTimeExpOf (aptr) == NULL) ? I_ENBC3 : I_ENBT3);
				} else {
					gensecondary ((AltTimeExpOf (aptr) == NULL) ? (isplaced ? I_EXTENBC : I_ENBC) : I_ENBT);
				}
				/*}}}*/
				taltcheck (inskippingalt, skiplab != NO_LABEL);	/* INSdi02264 22/06/93 */
				if (skiplab != NO_LABEL) {
					/* bug TS/1827 14/08/92 */
					setlab (skiplab);
				}
			}
			temp_unevaluate_all ();	/* bug TS/1900 22/10/92 */
		}
		break;
		/*}}} */
		/*{{{  S_REPLALT S_PRIREPLALT */
	case S_REPLALT:
	case S_PRIREPLALT:
		{
			const BOOL savedinreplalt = inreplalt;
			const int old_eae_isreplalt = eae_isreplalt;
			const int saved_eae_count = eae_count;		/* so we know how many enb3's happened */
			int i;
			const BOOL reverse_enable = (enhanced_alt_enable && (TagOf (aptr) == S_REPLALT));

			DEBUG_MSG (("taltenables: found a nested REPLALT or PRIREPLALT\n"));

			if (reverse_enable) {
				/* mapper has left a different start/step here */
				treenode *cur_altstart = ReplCStartExpOf (aptr);
				treenode *cur_altstep = ReplCStepExpOf (aptr);

				SetReplCStartExp (aptr, ReplCRAltStartOf (aptr));
				SetReplCStepExp (aptr, ReplCRAltStepOf (aptr));
				tpreexp (ReplCStartExpOf (aptr));
				tpreexp (ReplCLengthExpOf (aptr));
				if (ReplCStepExpOf (aptr)) {
					tpreexp (ReplCStepExpOf (aptr));
				}
				replaltstack[replaltstacklen] = ReplCNameOf (aptr);
				replaltstacklen++;
				inreplalt = TRUE;
				eae_isreplalt = 1;
				trepl (aptr, taltenables, voidptr, dummy, FALSE);

				SetReplCStartExp (aptr, cur_altstart);
				SetReplCStepExp (aptr, cur_altstep);
			} else {
				tpreexp (ReplCStartExpOf (aptr));
				tpreexp (ReplCLengthExpOf (aptr));
				replaltstack[replaltstacklen] = ReplCNameOf (aptr);
				replaltstacklen++;
				if (ReplCStepExpOf (aptr)) {
					tpreexp (ReplCStepExpOf (aptr));
				}
				inreplalt = TRUE;
				eae_isreplalt = 1;
				trepl (aptr, taltenables, voidptr, dummy, FALSE);
			}
			replaltstacklen--;
			eae_isreplalt = old_eae_isreplalt;
			inreplalt = savedinreplalt;
			if (eae_count != saved_eae_count) {
				/* mapped out some enb3's, better fix replicator vals */
				for (i = saved_eae_count; i < eae_count; i++) {
					eae_replptrs[i] = aptr;
				}
			}
		}
		break;
		/*}}} */
	}
	tdespecs (specsptr);
}

/*}}}*/
/*{{{  PRIVATEPARAM void taltdisables*/
/*****************************************************************************
 *
 *  taltdisables disables all the guards in the ALT or replicated ALT tree
 *               tptr, including those of any nested alternatives.
 *
 *****************************************************************************/
PRIVATEPARAM void taltdisables (treenode * tptr, void *const voidptr, const INT32 altendlab)
{
	treenode *aptr = skipspecifications (tptr);
	treenode *specsptr;

	DEBUG_MSG (("taltdisables, replaltstacklen is %d\n", replaltstacklen));
	/*{{{  translate leading specifications for the second time */
	/* These specifications can be translated in REDUCED error mode as
	   we have already translated them once, and if anything was going to
	   go wrong, it would have done so already.
	   Don't do 'tpreexp' a second time. */
#if 0				/* We've now guaranteed (in transformalt) that all leading specs are necessary */
	while (isspecification (tptr)) {
		if (usedinaltguards (DNameOf (tptr), aptr))
			tspecification (tptr);
		tptr = DBodyOf (tptr);
	}
#endif
	specsptr = tptr;
	tptr = tspecs (tptr);
	/*}}} */
	switch (TagOf (aptr)) {
		/*{{{  S_ALT S_PRIALT */
	case S_ALT:
	case S_PRIALT:
		if ((reverse_alt_disable && (TagOf (aptr) == S_PRIALT)) || enhanced_alt_enable) {
			const int old = switch_to_temp_workspace ();
			const int old_eae_isreplalt = eae_isreplalt;
			treenode *rlist = NULL;

			/* build a reversed list of the ALT contents and process it */
			for (aptr = CBodyOf (aptr); !EndOfList (aptr); aptr = NextItem (aptr)) {
				rlist = addtofront (ThisItem (aptr), rlist);
			}
#if 0
fprintf (stderr, "taltdisables() [ALT/PRIALT]  altendlab = %d, rlist =", (int)altendlab);
printtreenl (stderr, 4, rlist);
#endif
			eae_isreplalt = 0;	/* not in a replicated ALT here */
			for (; !EndOfList (rlist); rlist = NextItem (rlist)) {
				taltdisables (ThisItem (rlist), voidptr, altendlab);
			}
			eae_isreplalt = old_eae_isreplalt;
			switch_to_prev_workspace (old);
		} else {
			for (aptr = CBodyOf (aptr); !EndOfList (aptr); aptr = NextItem (aptr)) {
				taltdisables (ThisItem (aptr), voidptr, altendlab);
			}
		}
		break;
		/*}}} */
		/*{{{  S_ALTERNATIVE */
	case S_ALTERNATIVE:
		{
			treenode *const guard = AltGuardOf (aptr);
			int startlab;
			int skiplab = NO_LABEL;	/* bug TS/1827 14/08/92 */
			BOOL isplaced = FALSE;

			if (AltLabelOf (aptr) == NO_LABEL) {
				startlab = newlab ();
				SetAltLabel (aptr, startlab);
			} else {
				startlab = AltLabelOf (aptr);
			}

			new_occam_line (aptr, TRUE, TRUE, FALSE);
			/* if (debugoutput)
			   coder_genlocate(aptr, TRUE); */
			/* Added 27/10/92 for a comment in the zb display */

			if (AltChanExpOf (aptr) == NULL) {
				/*{{{  skip guard */
				if (enhanced_alt_enable && (replaltstacklen < 2)) {
					if (!eae_count) {
						geninternal_is (GEN_ERROR_IN_ROUTINE, 2, "taltdisables");
					} else {
						eae_count--;
						setlab (eae_labels[eae_count]);
					}
				}
				tbool (guard, MANY_REGS);
				genlabeldiff (I_LDC, startlab, (int) altendlab);
				if (reverse_alt_disable) {
					gensecondary (I_NDISS);
				} else {
					gensecondary (I_DISS);
				}
				/*}}} */
			} else {
				treenode *disabler;
				int mode = P_EXP;

				if (AltTimeExpOf (aptr) == NULL) {
					/*{{{  get channel address */
					/* bug 779 2/11/90 - modified to use AltChanExpOf */
					treenode *inputchannel = /*LHSOf(input) */ AltChanExpOf (aptr);
					/* Don't try and preevaluate it here */
					if (!T9000_alpha_badalt (&tx_global) && TagOf (inputchannel) == T_TEMP) {
						inputchannel = NDeclOf (inputchannel);
					}

					disabler = inputchannel;
					if ((nodetypeoftag (TagOf (gettype (disabler))) == TYPENODE) && (TypeAttrOf (gettype (disabler)) & TypeAttr_placed)) {
						isplaced = TRUE;
					}
					mode = chanaspointer ? P_EXP : P_PTR;
					/*}}} */
				} else {
					disabler = AltTimeExpOf (aptr);
				}

#if 0	/* not in KRoC! */
				if (T9000_alpha_badalt (&tx_global)) {
					/*{{{  T9000_alpha_badalt */
					if (!istrueguard (guard)) {
						skiplab = newlab ();	/* bug TS/1827 14/08/92 */
						tguard (guard, TRUE, skiplab);
					}
					mode = simplify (mode, disabler);

					if (!testflag) {
/*gensecondary(I_LDPRODID); *//* Not on alpha! */
						genprimary (I_LDC, 0x10E);
						gencomment0 ("Silicon bug fix;");
						gensecondary (I_LDCONF);
						gencomment0 ("Slooow instruction to help prefetch get ahead");
					}

					texpopd (mode, disabler, MANY_REGS);
					texpopd (mode, disabler, MANY_REGS);
					gencomment0 ("Silicon bug fix; guard := chan/time");
					/*}}} */
				} else
#endif	/* 0 */
				/*
				 *	frmb note:
				 */
				if (enhanced_alt_enable && !isplaced && (replaltstacklen < 2)) {
					if (!eae_count) {
						geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "taltdisables");
					} else {
						/*
						 * frmb note: this (labels) might have been changed by
						 * code nserted immediately after the ALT to recalculate
						 * any replicator count.
						 */
						eae_count--;
						setlab (eae_labels[eae_count]);
					}
				}
				if (!istrueguard (guard) && cancauseerror (disabler)) {
					/*{{{  guard ; cj; chan/time ; ldc 1 */
					skiplab = newlab ();	/* bug TS/1827 14/08/92 */
					tguard (guard, TRUE, skiplab);
					texpopd (mode, disabler, MANY_REGS);
					loadconstant (1);
					/*}}} */
				} else {
					tload2regs (mode, disabler, P_EXP, guard, FALSE, TRUE);
				}
#if 0
				/*{{{  T9000_gamma_carryin */
				if ((AltTimeExpOf (aptr) != NULL) && T9000_gamma_carryin (&tx_global)) {
					genprimary (I_LDC, 0);
					gensecondary (I_SUB);
					gencomment0 ("Set carry flag for ALU Adder CarryIn bug");
				}
				/*}}} */
#endif
				genlabeldiff (I_LDC, startlab, (int) altendlab);
#if 0
				/*{{{  T9000_alpha_badalt */
				if (T9000_alpha_badalt (&tx_global) && !testflag) {
					gensecondary (I_NOP);
					gencomment0 ("try to ensure the following group together");
					/*genprimary(I_LDC, 1); */
					texpopd (mode, disabler, MANY_REGS);
					gencomment0 ("Silicon bug fix - push into D reg");
					genprimary (I_CJ, 0);
					gencomment0 ("Pop it off again to sort out the ALU");
				}
				/*}}} */
#endif
				if (reverse_alt_disable) {
					gensecondary ((AltTimeExpOf (aptr) == NULL) ? (isplaced ? I_EXTNDISC : I_NDISC) : I_NDIST);
				} else {
					gensecondary ((AltTimeExpOf (aptr) == NULL) ? I_DISC : I_DIST);
				}
			}

			/*{{{  select current value of any enclosing replicators */
			{
				if (replaltstacklen > 0) {
					int i;
					if (skiplab == NO_LABEL)
						skiplab = newlab ();	/* bug TS/1827 14/08/92 */
					genbranch (I_CJ, skiplab);	/*          cj    skiplab   */
					for (i = 0; i < replaltstacklen; i++) {
						treenode *replname = replaltstack[i];
						loadname (replname, REPL_BASE);			/*          ldl   i        */
						/* storeinname(replname, REPL_SELECTED); */	/*          stl   i + 2    */
						genprimary (I_STL, replaltvaluesbase + i);	/*          stl   saved_repl */
						gencomment1 ("INT saved repl %d", i);
					}
				} else {
					throw_the_result_away ();
				}
			}
			/*}}} */
			if (skiplab != NO_LABEL) {	/* bug TS/1827 14/08/92 */
				setlab (skiplab);	/* skiplab:                */
			}
		}
		break;
		/*}}} */
		/*{{{  S_REPLALT S_PRIREPLALT */
	case S_REPLALT:
	case S_PRIREPLALT:
		replaltstack[replaltstacklen] = ReplCNameOf (aptr);
		replaltstacklen++;
		DEBUG_MSG (("taltdisables: found a repl, replaltstacklen is now %d\n", replaltstacklen));
#if 0
fprintf (stderr, "taltdisables: found a repl, replaltstacklen is now %d, aptr =", replaltstacklen);
printtreenl (stderr, 4, aptr);
#endif
		if ((reverse_alt_disable && (TagOf (aptr) == S_PRIREPLALT)) || (enhanced_alt_enable && (TagOf (aptr) == S_PRIREPLALT))) {
			/* mapper has left a different start and step for us */
			const int old_eae_isreplalt = eae_isreplalt;
			treenode *cur_altstart = ReplCStartExpOf (aptr);
			treenode *cur_altstep = ReplCStepExpOf (aptr);

			SetReplCStartExp (aptr, ReplCRAltStartOf (aptr));
			SetReplCStepExp (aptr, ReplCRAltStepOf (aptr));
#if 0
fprintf (stderr, "gen10: taltdisables: reverse_alt_disable, PRIREPLALT: adjusted start =");
printtreenl (stderr, 4, ReplCStartExpOf (aptr));
fprintf (stderr, "gen10: taltdisables: reverse_alt_disable, PRIREPLALT: adjusted step =");
printtreenl (stderr, 4, ReplCStepExpOf (aptr));
#endif
			eae_isreplalt = 1;
			trepl (aptr, taltdisables, voidptr, altendlab, FALSE);
			SetReplCStartExp (aptr, cur_altstart);
			SetReplCStepExp (aptr, cur_altstep);
			eae_isreplalt = old_eae_isreplalt;
		} else {
			trepl (aptr, taltdisables, voidptr, altendlab, FALSE);
		}
		replaltstacklen--;
		break;
		/*}}} */
	}
	tdespecs (specsptr);
}
/*}}}*/
/*{{{  PRIVATE BOOL talthaspri (treenode *aptr) */
/*****************************************************************************
 *  talthaspri is used to determine if an ALT involves a PRI ALT at any
 *             point, necessary so we can avoid NDIS[CST] -- not compatible
 *             with replicators and DIS[CST] in a PRI ALT..
 *****************************************************************************/
PRIVATE BOOL talthaspri (treenode *aptr)
{
	treenode *tptr;

	for (tptr = aptr; tptr;) {
		tptr = skipspecifications (tptr);		/* skip leading specs */

		switch (TagOf (tptr)) {
		case S_ALT:
			for (tptr = CBodyOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr)) {
				if (talthaspri (ThisItem (tptr))) {
					return TRUE;
				}
			}
			break;
		case S_REPLALT:
			tptr = ReplCBodyOf (tptr);
			break;
		case S_PRIALT:
		case S_PRIREPLALT:
			return TRUE;
		case S_ALTERNATIVE:
			tptr = NULL;	/* guard -- end of the line.. */
			break;
		default:
			tptr = NULL;
			break;
		}
	}
	return FALSE;
}
/*}}}  */
/*{{{  PRIVATE void taltbodies(tptr, joinlab)*/
/*****************************************************************************
 *
 *  taltbodies generates code for all the ALT bodies contained in the ALT or
 *             replicated ALT tree, tptr, including the bodies of nested ALTs.
 *
 *****************************************************************************/
PRIVATE void taltbodies (treenode * tptr, int joinlab, treenode ** altbodystack, int altbodystackptr)
{
	DEBUG_MSG (("taltbodies: altbodystackptr is %d\n", altbodystackptr));

	altbodystack[altbodystackptr] = tptr;
	tptr = skipspecifications (tptr);
	switch (TagOf (tptr)) {
		/*{{{  S_ALT S_PRIALT */
	case S_ALT:
	case S_PRIALT:
		for (tptr = CBodyOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr)) {
			taltbodies (ThisItem (tptr), joinlab, altbodystack, altbodystackptr + 1);
		}
		break;
		/*}}} */
		/*{{{  S_ALTERNATIVE */
	case S_ALTERNATIVE:
		{
			/*treenode *input = AltInputOf(tptr); */
			treenode *body = AltBodyOf (tptr);
			/* treenode *specsptr; */

			setlab (AltLabelOf (tptr));

			/*{{{  translate specifications leading up to this alternative */
			{
				/* These specifications can be translated in REDUCED error mode as
				   we have already translated them at least twice, and if anything
				   was going to go wrong, it would have done so already. */
				int i;
				for (i = 0; i < (altbodystackptr + 1); i++) {
					treenode *t = altbodystack[i];
					t = tspecs (t);
				}
			}
			/*}}} */
#if 0
fprintf (stderr, "taltbodies (ALTERNATIVE): altbodystackptr = %d, body =", altbodystackptr);
printtreenl (stderr, 4, body);
#endif
			tprocess (body);
			{
				int i;

				for (i=0; i< (altbodystackptr + 1); i++) {
					treenode *t = altbodystack[i];
					tdespecs (t);
				}
			}
			genjump (joinlab);
		}
		break;
		/*}}} */
		/*{{{  S_REPLALT S_PRIREPLALT */
	case S_REPLALT:
	case S_PRIREPLALT:
		/* Change the position of the replicator from replicated_i to saved i */
		{
			treenode *replname = ReplCNameOf (tptr);
			/*INT32 newoffset = NVOffsetOf(replname) + (REPL_SELECTED - REPL_BASE); */
			/*INT32 newoffset = replaltvaluesbase + replaltstacklen; */
			INT32 newoffset = replaltvaluesbase + replaltstacklen - nameoffsetof (NLexLevelOf (replname));
			DEBUG_MSG (("taltbodies: in replalt, replaltstacklen is %d\n", replaltstacklen));
			replaltstacklen++;
			SetNVOffset (replname, newoffset);
			if (!isconst (ReplCLengthExpOf (tptr)) || (LoValOf (ReplCLengthExpOf (tptr)) != 0))
				taltbodies (ReplCBodyOf (tptr), joinlab, altbodystack, altbodystackptr + 1);
			replaltstacklen--;
		}
		break;
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void tioop*/
PUBLIC void tioop (const int ioinst, const BOOL usecall)
{
	DEBUG_MSG (("In tioop: ioinst = %d, usecall = %d\n", ioinst, usecall));
	if (usecall) {
		treenode *const nptr = iocallname (ioinst);
		genbranch (I_CALL, NPLabelOf (nptr));
		if (assembly_output) {
			gencomments ("Call %s", WNameOf (NNameOf (nptr)));
		}
		coder_return_from_call (FALSE);	/* bug TS/2005 15/12/92 */
	} else {
		gensecondary (ioinst);
#if 0
		/*{{{  T9000_alpha_badrunp */
		if (T9000_alpha_badrunp (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Fix for scheduler problem");
			gensecondary (I_NOP);
			gencomment0 ("Fix for scheduler problem");
		}
		/*}}} */
#endif
	}
}

/*}}}*/


/*{{{  PUBLIC void mapchantypeop (treenode **const ctname)*/
/*
 *	this maps a user-specified CHAN TYPE operation, in addition to
 *	any standard mappings (e.g. for CLAIM)
 */
PUBLIC void mapchantypeop (treenode **const ctname)
{
	mapexpopd (P_EXP, ctname);
	mapioop (I_OUTWORD, FALSE);
	return;
}
/*}}}*/
/*{{{  PRIVATE void mapoutputitem*/
/*****************************************************************************
 *
 *  mapoutputitem allocates workspace required for outputting
 *                (outputitemmode, outputitem) on channel
 *                (channelmode, channel)
 *
 *****************************************************************************/
PRIVATE void mapoutputitem (const int channelmode, treenode ** const channel, int outputitemmode, treenode ** const outputitem,
				treenode *const prottype, treenode *const comm)
{
	const BOOL usecall = iobycall && maybevirtualchan (*channel);
	BOOL isplaced;
	DEBUG_MSG (("mapoutputitem: usecall is %d\n", usecall));
#if 0
printf ("mapoutputitem: channelmode = %d, channel = ", channelmode);
printtree (stdout, 4, *channel);
printf ("\n  outputitemmode = %d, outputitem = ", outputitemmode);
printtree (stdout, 4, *outputitem);
printf ("\n  comm = ");
printtree (stdout, 4, comm);
printf ("\n");
#endif
	switch (TagOf (*channel)) {
	case N_DECL:
	case N_ABBR:
	case N_PARAM:
		if ((nodetypeoftag (TagOf (NTypeOf (*channel))) == TYPENODE) && (TypeAttrOf (NTypeOf (*channel)) & TypeAttr_placed)) {
			isplaced = TRUE;
		} else {
			isplaced = FALSE;
		}
		break;
	default:
		isplaced = FALSE;
		break;
	}
	if (TagOf (*outputitem) == S_COLON2) {
		/*{{{  map counted output */
		treenode **countexp = LeftOpAddr (*outputitem);
		treenode **arrayexp = RightOpAddr (*outputitem);
		treenode *dimexp = dimexpof (*arrayexp, 0);
		int countmode = P_EXP;
		const int constantcount = isconst (*countexp);
		const int type = ntypeof (*countexp);
		const SOURCEPOSN locn = LocnOf (*outputitem);

		/*{{{  preevaluate count to a temporary if necessary */
		if (!constantcount) {
			if (!issimplelocal (*countexp, be_lexlevel)) {	/* || !fitsinword(type)) */
				/* bug 1132 29/1/91 */
				*countexp = gettemp (*countexp, NM_WORKSPACE);
				upusecount (*countexp, 1);
				mapsimpleassign (type, P_TEMP, countexp, P_EXP, NDeclAddr (*countexp));
				countmode = P_TEMP;
			}
			if (isshortint (type))
				upusecount (*countexp, 2);	/* sign extension *//* bug 706 13/9/90 */
		}
		/*}}} */
		/*{{{  output the count */
		if (intnodeptr == NULL) {
			const int old = switch_to_real_workspace ();
			intnodeptr = newleafnode (S_INT, NOPOSN);
			switch_to_prev_workspace (old);
		}
		mapoutputitem (channelmode, channel, countmode, countexp, intnodeptr, NULL);
		/*}}} */
		/*{{{  output the array from 0 for count */
		/* count is variable or count is constant but not zero */
		if (!constantcount || (LoValOf (*countexp) != 0) || (HiValOf (*countexp) != 0)) {
			/*{{{  output the array from 0 for count */
			treenode *checkedcount;
			int checkcount = FALSE;
			/*{{{  map the conversion (check) if a quad word */
			/* Added for bug 1130 29/1/91 */
			if (!constantcount && isquadlength (type)) {
				const int old = switch_to_temp_workspace ();
				treenode *conversion = makeconversion (type, S_INT, *countexp, S_EXACT, locn);
				switch_to_prev_workspace (old);
				mapexp (&conversion);
			}
			/*}}} */
			/*{{{  check the count if it or the dimension are not constant */
			if (RANGECHECKING && (!constantcount || !isconst (dimexp))) {
				const int old = switch_to_temp_workspace ();
				checkedcount = newdopnode (S_CCNT1, NOPOSN, *countexp, dimexp, S_INT);
				switch_to_prev_workspace (old);
				checkcount = TRUE;
			} else {
				checkedcount = *countexp;
			}
			/*}}} */
			if (constantcount) {
				if (checkcount) {
					mapexp (&checkedcount);
				}
				mapload2regs (P_PTR, arrayexp, channelmode, channel);
			} else {
				/*{{{  ldptr array; ldptr channel; ld length */
				treenode *lengthexp;
				const int old = switch_to_temp_workspace ();
				/* 10 is just a dummy value as we don't know the real size,
				   and it doesn't matter what it is anyway, just as long
				   as the lengthexp takes two registers to load */
				lengthexp = newdopnode (S_TIMES, NOPOSN, checkedcount, newconstant (10), S_INT);
				switch_to_prev_workspace (old);
				mapload3regs (P_PTR, arrayexp, channelmode, channel, P_EXP, &lengthexp);
				/*}}} */
			}
			mapioop (isplaced ? I_EXTOUT : I_OUT, usecall);
			/*}}} */
		}
		/*}}} */
		/*{{{  free the count temporary if neccessary */
		if (countmode == P_TEMP) {
			freetemp (*countexp);
		}
		/*}}} */
		/*}}} */
#ifdef MOBILES
	} else if ((ntypeof (*outputitem) == S_ANYCHANTYPE) && (TagOf (follow_user_type (prottype)) == S_ANYCHANTYPE)) {
		/*{{{  MOBILE.CHAN output*/
		mapload2regs (outputitemmode, outputitem, channelmode, channel);
		mapioop (isplaced ? I_EXTOUT : I_OUT, usecall);
		mapload2regs (outputitemmode, outputitem, channelmode, channel);
		mapioop (isplaced ? I_EXT_MT_OUT : I_MT_OUT, usecall);

		/*}}}*/
	} else if ((ntypeof (*outputitem) == S_ANYPROCTYPE) && (TagOf (follow_user_type (prottype)) == S_ANYPROCTYPE)) {
		/*{{{  MOBILE.PROC output*/
		mapload2regs (outputitemmode, outputitem, channelmode, channel);
		mapioop (isplaced ? I_EXTOUT : I_OUT, usecall);
		mapload2regs (outputitemmode, outputitem, channelmode, channel);
		mapioop (isplaced ? I_EXT_MT_OUT : I_MT_OUT, usecall);

		/*}}}*/
	} else if ((ntypeof (*outputitem) == S_MOBILE) && (TagOf (follow_user_type (prottype)) == S_MOBILE)) {
		/*{{{  map MOBILE output*/

#if 0
fprintf (stderr, "gen10: mapoutputitem: mapping MOBILE output.  NTypeAttrOf (NTypeOf (*outputitem)) = %d, NTypeAttr (prottype) = %d\n", NTypeAttrOf (NTypeOf (*outputitem)), NTypeAttrOf (prottype));
#endif
		mapload2regs (outputitemmode, outputitem, channelmode, channel);
		mapioop (isplaced ? I_EXT_MT_OUT : I_MT_OUT, usecall);
#endif
		/*}}}*/
	} else {
		const int t = ntypeof (*outputitem);
		BOOL freeoutputitem = FALSE;
		BOOL useout;
		/*{{{  This bit added by CO'N for eg. out ! x + x, REAL32 on T8 */
		if (has_fpu_core && isreal (t) && !isaddressableopd (outputitemmode, *outputitem)) {
/*printf("out ! x + x\n"); *//* INSdi02071 */
			if (isconst (*outputitem)) {
				placeintable (*outputitem);
			} else {
#if 0				/* This isn't quite right, or is it? */
				treenode *type = gettype (*outputitem);
				*outputitem = gettemp (*outputitem, NM_WORKSPACE);
				upusecount (*outputitem, 1);
				mapsimpleassign (TagOf (type), P_TEMP, outputitem, outputitemmode, NDeclAddr (*outputitem));
				outputitemmode = P_TEMP;
				freeoutputitem = TRUE;
#endif
				mapfpexp (outputitem);
				*outputitem = gettemp (*outputitem, NM_WORKSPACE);
				outputitemmode = P_TEMP;
				freeoutputitem = TRUE;
			}
		}
		/*}}} */

		/* Bug 1203 - 12/3/91 - don't use outbyte and/or outword if the
		   output item is already addressable.
		   However, if it is a scalar constant, we haven't yet decided whether to
		   put it into the constant table, so isaddressable will not yet
		   return TRUE.
		   If we would not normally use a constant table, we still
		   do an outbyte or outword.
		 */
		/* Bug TS/1504 - 07/04/92:
		   If we're optimising for space, DO use the shorter forms if possible.
		 */
		useout = ((code_style_flags & CODE_STYLE_SPACE_NOT_TIME) == 0) &&
			(isaddressableopd (outputitemmode, *outputitem) ||
			 ((TagOf (*outputitem) == S_CONSTEXP) && shouldbeinconstanttable (*outputitem)));

		if (!useout && iscomplexopd (outputitemmode, *outputitem)) {
#if 0
fprintf (stderr, "gen10: mapoutputitem: *outputitem looks complex!\n");
#endif
			useout = TRUE;
		}

		if (isplaced) {
			treenode *type = gettype (*outputitem);
			const INT32 s = bytesin (type);

			/*{{{  if outputitem is not addressable, evaluate it into a temporary */
			if (!isaddressableopd (outputitemmode, *outputitem)) {
				if (isconst (*outputitem)) {
					placeintable (*outputitem);
				} else {
					*outputitem = gettemp (*outputitem, NM_WORKSPACE);
					upusecount (*outputitem, 1);
					mapsimpleassign (TagOf (type), P_TEMP, outputitem, outputitemmode, NDeclAddr (*outputitem));
					outputitemmode = P_TEMP;
					freeoutputitem = TRUE;
				}
			}
			/*}}} */

			outputitemmode = ptrmodeof (outputitemmode);
			if (s == -1) {
				/* length is expression */
				int loadseq;
				treenode *lengthexp;
				treenode *rhs = RHSOf (comm);

				lengthexp = scaletreeof (type, 1, be_lexlevel);
				loadseq = mapload3regs (outputitemmode, outputitem, P_EXP, channel, P_EXP, &lengthexp);
				NewNextItem (addtofront (lengthexp, NULL), rhs);
			} else {
				mapload2regs (outputitemmode, outputitem, P_EXP, channel);
			}
			mapioop (I_EXTOUT, usecall);

		} else if (!useout && istargetintsize (t)) {		/* bug 1203 12/3/91 */
			/*{{{  outword */
			mapload2regs (channelmode, channel, outputitemmode, outputitem);
			mapioop (I_OUTWORD, usecall);
			/*}}} */
		} else if (!useout && istargetbytesize (t)) {	/* bug 1203 12/3/91 */
			/*{{{  outbyte */
			mapload2regs (channelmode, channel, outputitemmode, outputitem);
			mapioop (I_OUTBYTE, usecall);
			/*}}} */
		} else {
			/*{{{  out */
			treenode *type = gettype (*outputitem);
			const INT32 s = bytesin (type);

			/*{{{  if outputitem is not addressable, evaluate it into a temporary */
			if (!isaddressableopd (outputitemmode, *outputitem)) {
				if (isconst (*outputitem)) {
					placeintable (*outputitem);
				} else {
					*outputitem = gettemp (*outputitem, NM_WORKSPACE);
					upusecount (*outputitem, 1);
					mapsimpleassign (TagOf (type), P_TEMP, outputitem, outputitemmode, NDeclAddr (*outputitem));
					outputitemmode = P_TEMP;
					freeoutputitem = TRUE;
				}
			}
			/*}}} */

			outputitemmode = ptrmodeof (outputitemmode);
			if (s == (-1)) {
				/*{{{  length is an expression */
				int loadseq;
				treenode *lengthexp;

				/*int old = switch_to_temp_workspace(); */
				lengthexp = scaletreeof (type, 1, be_lexlevel);
				/*switch_to_prev_workspace(old); */

				loadseq = mapload3regs (outputitemmode, outputitem, channelmode, channel, P_EXP, &lengthexp);
				/* Ideally we should save this loadseq value on the tree somewhere
				   so that we don't have to recalculate it in the code generator */
				{
					/* bug TS/1626 24/04/92 - save the length expression */
					treenode *rhs = RHSOf (comm);
					NewNextItem (addtofront (lengthexp, NULL), rhs);
				}
				/*}}} */
			} else {
				mapload2regs (outputitemmode, outputitem, channelmode, channel);
			}
			mapioop (I_OUT, usecall);
		}
		/*}}} */
		if (freeoutputitem) {
			freetemp (*outputitem);
		}
	}
}

/*}}}*/
/*{{{  PUBLIC void mapinputitem*/
/*****************************************************************************
 *
 *  mapinputitem allocates workspace required for inputting inputitem
 *                on channel (channelmode, channel)
 *  frmb - added x_process for extended input (also added prottype for MOBILEs)
 *
 *****************************************************************************/
PUBLIC void mapinputitem (const int channelmode, treenode **const channel, const int inputitemmode,
	treenode **const inputitem, treenode *const prottype, treenode *const comm, treenode *const x_process)
{
	const BOOL usecall = iobycall && maybevirtualchan (*channel);
	const BOOL is_extended = (x_process != NULL);
	BOOL isplaced;

	DEBUG_MSG (("mapinputitem, usecall %d\n", usecall));
	switch (TagOf (*channel)) {
	case N_DECL:
	case N_ABBR:
	case N_PARAM:
		if ((nodetypeoftag (TagOf (NTypeOf (*channel))) == TYPENODE) && (TypeAttrOf (NTypeOf (*channel)) & TypeAttr_placed)) {
			isplaced = TRUE;
		} else {
			isplaced = FALSE;
		}
		break;
	default:
		isplaced = FALSE;
		break;
	}
	if (TagOf (*inputitem) == S_COLON2) {
		const SOURCEPOSN locn = LocnOf (*inputitem);
		if (has_variableio) {		/* variableio 14/3/91 */
			/* frmb note: this ain't used in KRoC, since we generate for a T8 */
			/*{{{  Use vin instruction */
			/*
			   Simplistic version to start with;
			   we do VIN with maxsize = size of the type of the rhs.
			   We then do LDCNT and store to a temporary.
			   We then divide this by the size of one element of the array, to get
			   the count. This is converted to the correct type, and assigned
			   to the count expression.

			   The important bit to note is that we MUST NOT timeslice between
			   the VIN and the LDCNT. In the 'usecall' case, the VIN actually
			   returns the count.
			   So the philosophy is to always execute the LDCNT immediately after
			   the VIN.

			   Modify the tree to look like:

			   S_COLON2
			   /     \
			   S_LIST     rhs
			   /    \
			   maxsize S_LIST   -   S_LIST   -   S_LIST   -   NULL
			   |            |            |
			   temp         expression    countexp

			   where expression is Type-convert(temp / (SIZE array[0]))

			   Technically this should be an unsigned divide, because we can
			   communicate more than half the address space, so the number of
			   bytes transferred is actually an unsigned number.
			   However, with this compiler it is impossible to declare an array
			   of such a size; hence any such large number is impossible.
			   Therefore a normal division is ok.
			   Note that the optimisation which inserts a shift will automatically
			   perform an unsigned divide.

			   Optimisation: if the count exp is simple enough, and the divide
			   is simple enough, don't generate the temporary, but simply
			   do the divide and store directly in the code generator.
			   We mark this by inserting a CONSTEXP node where the expression should
			   be, with the size of the divide to be performed,
			   and temp is left NULL.
			 */

			treenode **countexp = LeftOpAddr (*inputitem);
			treenode **arrayexp = RightOpAddr (*inputitem);
			const int counttype = ntypeof (*countexp);
			treenode *maxsize = scaletreeof (gettype (*arrayexp), 1, be_lexlevel);
			treenode *temp = NULL;
			treenode *expression = NULL;
			treenode *arraytype = ARTypeOf (gettype (*arrayexp));
			const INT32 b = bytesin (arraytype);
			const int exponent = whichpowerof2 (b);

			mapload3regs (P_PTR, arrayexp, channelmode, channel, P_EXP, &maxsize);
			mapioop (I_VIN, usecall);

			/* decide whether we can optimise the storage so that we don't need
			   to store into a temporary
			 */
			if ((b != (-1))	/* size of rhs is constant */
			    &&(fitsinword (counttype) || isdoublelength (counttype))
			    /*&& issimplelocal(*countexp)) */
			    && (regsforstore (*countexp) < MAXREGS)) {

				/*{{{  Mark the simple case */
				expression = newconstant (b);
				if (exponent <= 1)
					mapexp (&expression);
				/* and leave temp = NULL */
				mapstoreinopd (P_EXP, countexp);
				/*}}} */
			} else {
				/*{{{  Build expression to divide down the byte count */
				temp = gettypedtemp (NULL, NM_WORKSPACE, newleafnode (S_INT, locn));
				if (b == 1) {
					expression = temp;
				} else if (exponent > 0) {
					expression = newdopnode (S_RSHIFT, locn, temp, newconstant (exponent), S_INT);
				} else {	/* either b == -1, or b is not a small power of 2 */
					expression = newdopnode (S_DIV, locn, temp, scaletreeof (arraytype, 1, be_lexlevel), S_INT);
				}

				if (isquadlength (counttype)) {
					expression = makeconversion (S_INT, counttype, expression, S_EXACT, locn);
				} else if (!istargetintsize (counttype)) {
					expression = newmopnode (S_EXACT, locn, expression, counttype);
				}

				upusecount (temp, 1);
				if (TagOf (temp) != T_TEMP) {
					badtag (locn, TagOf (temp), "mapinputitem");
				}
				SetTag (temp, T_PREEVALTEMP);

				mapsimpleassign (counttype, P_EXP, countexp, P_EXP, &expression);
				freetemp (temp);
				/*}}} */
			}

			*countexp = addtofront (maxsize, addtofront (temp, addtofront (expression, addtofront (*countexp, NULL))));
			/*}}} */
		} else {
			/*{{{  do counted input */
			treenode **countexp = LeftOpAddr (*inputitem), **arrayexp = RightOpAddr (*inputitem);
			int countmode = P_EXP;
			const int counttype = ntypeof (*countexp);
			treenode *checkedcount;

			/*{{{  declare a temporary to hold count, if necessary */
			if (!issimplelocal (*countexp, be_lexlevel)) {
				if (intnodeptr == NULL) {
					const int old = switch_to_real_workspace ();
					intnodeptr = newleafnode (S_INT, NOPOSN);
					switch_to_prev_workspace (old);
				}
				/*{{{  input to a temporary; move to the destination */
				*countexp = gettemp (*countexp, NM_WORKSPACE);
				upusecount (*countexp, 1);
				countmode = P_TEMP;
				/*{{{  input to a temporary */
				mapinputitem (channelmode, channel, P_TEMPPTR, countexp, intnodeptr, NULL, NULL);
				/*}}} */
				/*{{{  move to the destination */
				mapsimpleassign (counttype, P_EXP, NDeclAddr (*countexp), P_TEMP, countexp);
				/*}}} */
				/*}}} */
			} else {
				/*{{{  input to the destination */
				countmode = P_EXP;
				mapinputitem (channelmode, channel, P_PTR, countexp, gettype (*countexp), comm, NULL);
				/*}}} */
			}
			if (isshortint (counttype)) {
				upusecount (*countexp, 2);	/* sign extension */
			}
			/*}}} */

			/*{{{  convert a long int to int if neccessary */
			/* It will be in a temp if it isn't a local simple BYTE, or short INT */
			/* The only conversions that need checking are long ints to int */
			/* No mapping required *//* WRONG!!!!!! */
			/*{{{  map the conversion (check) if a quad word */
			/* Added for bug 1130 29/1/91 */
			if (isquadlength (counttype)) {
				const int old = switch_to_temp_workspace ();
				treenode *conversion = makeconversion (counttype, S_INT, *countexp, S_EXACT, locn);
				switch_to_prev_workspace (old);
				mapexp (&conversion);
			}
			/*}}} */
			/*}}} */
			/*{{{  if count = 0 goto skiplab */
			/* No mapping required: we've already mapped the count */
			/*}}} */
			/*{{{  add rangecheck to count expression if necessary */
			if (RANGECHECKING) {
				const int old = switch_to_temp_workspace ();
				checkedcount = newdopnode (S_CCNT1, NOPOSN, *countexp, dimexpof (*arrayexp, 0), S_INT);
				switch_to_prev_workspace (old);
			} else {
				checkedcount = *countexp;
			}
			/*}}} */
			/*{{{  input array from 0 for count */
			{
				treenode *lengthexp;
				/*{{{  lengthexp = checkedcount * scale */
				{
					const int old = switch_to_temp_workspace ();
					/* 10 is just a dummy value as we don't know the real size,
					   and it doesn't matter what it is anyway, just as long
					   as the lengthexp takes two registers to load */
					lengthexp = newdopnode (S_TIMES, NOPOSN, checkedcount, newconstant (10), S_INT);
					switch_to_prev_workspace (old);
				}
				/*}}} */
				if (is_extended) {
					mapexpopd (channelmode, channel);
					mapioop (I_XABLE, usecall);
				}
				mapload3regs (P_PTR, arrayexp, isplaced ? P_EXP : channelmode, channel, P_EXP, &lengthexp);
				if (is_extended) {
					mapioop (I_XIN, usecall);
					mapprocess (x_process);
					mapexpopd (channelmode, channel);
					mapioop (I_XEND, usecall);
				} else {
					mapioop (isplaced ? I_EXTIN : I_IN, usecall);
				}
			}
			/*}}} */
			/*{{{  free the temporary holding count, if necessary */
			if (countmode == P_TEMP) {
				freetemp (*countexp);
			}
			/*}}} */

		}
		/*}}} */
#ifdef MOBILES
	} else if ((ntypeof (*inputitem) == S_ANYCHANTYPE) && (TagOf (follow_user_type (prottype)) == S_ANYCHANTYPE)) {
		/*{{{  map MOBILE.CHAN input, absolute into var*/
		const BOOL utagged = (TagOf (*inputitem) == S_UNDEFINED);

		/* first input is already regular (integer) */
		if (utagged) {
			mapload2regs (inputitemmode, OpAddr (*inputitem), channelmode, channel);
		} else {
			mapload2regs (inputitemmode, inputitem, channelmode, channel);
		}
		mapioop (isplaced ? I_EXTIN : I_IN, usecall);

		/* second input might be extended */
		if (is_extended) {
			mapexpopd (channelmode, channel);
			mapioop (I_XABLE, usecall);
		}
		
		if (utagged) {
			mapload2regs (inputitemmode, OpAddr (*inputitem), channelmode, channel);
		} else {
			mapload2regs (inputitemmode, inputitem, channelmode, channel);
		}
		
		if (isplaced) {
			if (is_extended) {
				genwarn (GEN_WARN_BADCODE);
			}
			mapioop (I_EXT_MT_IN, usecall);
		} else {
			mapioop (is_extended ? I_MT_XIN : I_MT_IN, usecall);
		}
		
		if (is_extended) {
			/* _NOT_ going to be first or second half */
			mapprocess (x_process);
			mapexpopd (channelmode, channel);
			mapioop (I_XEND, usecall);
		}
#if 0
fprintf (stderr, "mapinputitem: ANYCHANTYPE input!\n");
#endif
		/*}}}*/
	} else if ((ntypeof (*inputitem) == S_ANYPROCTYPE) && (TagOf (follow_user_type (prottype)) == S_ANYPROCTYPE)) {
		/*{{{  map MOBILE.PROC input, absolute into var*/
		const BOOL utagged = (TagOf (*inputitem) == S_UNDEFINED);

		/* first input is always regular (integer) */
		if (utagged) {
			mapload2regs (inputitemmode, OpAddr (*inputitem), channelmode, channel);
		} else {
			mapload2regs (inputitemmode, inputitem, channelmode, channel);
		}
		mapioop (isplaced ? I_EXTIN : I_IN, usecall);

		/* second input might be extended */
		if (is_extended) {
			mapexpopd (channelmode, channel);
			mapioop (I_XABLE, usecall);
		}
		
		if (utagged) {
			mapload2regs (inputitemmode, OpAddr (*inputitem), channelmode, channel);
		} else {
			mapload2regs (inputitemmode, inputitem, channelmode, channel);
		}
		
		if (isplaced) {
			if (is_extended) {
				genwarn (GEN_WARN_BADCODE);
			}
			mapioop (I_EXT_MT_IN, usecall);
		} else {
			mapioop (is_extended ? I_MT_XIN : I_MT_IN, usecall);
		}

		if (is_extended) {
			/* _NOT_ going to be first or second half */
			mapprocess (x_process);
			mapexpopd (channelmode, channel);
			mapioop (I_XEND, usecall);
		}
		/*}}}*/
	} else if ((ntypeof (*inputitem) == S_MOBILE) && (TagOf (follow_user_type (prottype)) == S_MOBILE)) {
		/*{{{  map MOBILE input*/
		const BOOL utagged = (TagOf (*inputitem) == S_UNDEFINED);

		if (is_extended) {
			mapexpopd (channelmode, channel);
			mapioop (I_XABLE, usecall);
		}

		if (utagged) {
			mapload2regs (inputitemmode, OpAddr (*inputitem), channelmode, channel);
		} else {
			mapload2regs (inputitemmode, inputitem, channelmode, channel);
		}
		
		if (isplaced) {
			if (is_extended) {
				genwarn (GEN_WARN_BADCODE);
			}
			mapioop (I_EXT_MT_IN, usecall);
		} else {
			mapioop (is_extended ? I_MT_XIN : I_MT_IN, usecall);
		}

		if (is_extended) {
			/* _NOT_ going to be first or second half */
			mapprocess (x_process);
			mapexpopd (channelmode, channel);
			mapioop (I_XEND, usecall);
		}
		/*}}}*/
#endif
	} else {
		/*{{{  input an element*/
		treenode *type = gettype (*inputitem);

		if (bytesin (type) == (-1)) {
			/*{{{  length is an expression */
			if (is_extended) {
				mapexpopd (isplaced ? P_EXP : channelmode, channel);
			} else {
				treenode *lengthexp = NULL;
				int loadseq = -1;
				treenode *rhs = RHSOf (comm);

				lengthexp = scaletreeof (type, 1, be_lexlevel);
				loadseq = mapload3regs (inputitemmode, inputitem, isplaced ? P_EXP : channelmode, channel, P_EXP, &lengthexp);
				/* Ideally we should save this loadseq value on the tree somewhere
				   so that we don't have to recalculate it in the code generator */

				/* bug TS/1626 24/04/92 - save the length expression */
				NewNextItem (addtofront (lengthexp, NULL), rhs);
			}
			/*}}} */
		} else {
			if (is_extended) {
				mapexpopd (channelmode, channel);
			} else {
				mapload2regs (inputitemmode, inputitem, isplaced ? P_EXP : channelmode, channel);
			}
		}
		if (is_extended) {
			/* do XIN, copy, process, XEND */
			mapioop (I_XABLE, usecall);
			/* shift data */
			if (bytesin (type) != (-1)) {
				mapload2regs (inputitemmode, inputitem, channelmode, channel);
			} else {
				treenode *lengthexp = NULL;
				int loadseq = -1;
				treenode *rhs = RHSOf (comm);

				lengthexp = scaletreeof (type, 1, be_lexlevel);
				loadseq = mapload3regs (inputitemmode, inputitem, channelmode, channel, P_EXP, &lengthexp);
				/* Ideally we should save this loadseq value on the tree somewhere
				   so that we don't have to recalculate it in the code generator */

				/* bug TS/1626 24/04/92 - save the length expression */
				NewNextItem (addtofront (lengthexp, NULL), rhs);
				loadseq = mapload3regs (inputitemmode, inputitem, channelmode, channel, P_EXP, &lengthexp);
			}
			mapioop (I_XIN, usecall);
			if (TagOf (x_process) == S_X_FIRST_HALF) {
				mapprocess (OpOf (x_process));
			} else {
				mapprocess (x_process);
				mapexpopd (channelmode, channel);
				mapioop (I_XEND, usecall);
			}
		} else {
			mapioop (isplaced ? I_EXTIN : I_IN, usecall);
		}
		/*}}}*/
	}
}
/*}}}*/
/*{{{  PRIVATE void mapextendedhalf (channelmode, channel, comm,  x_process)*/
/*	maps an extended communication second-half.  this is where we need to
 *	release a channel seperately from doing the XABLE (handled in mapinputitem
 */
PRIVATE void mapextendedhalf (const int channelmode, treenode **const channel, treenode *const comm, treenode *const x_process)
{
	const BOOL usecall = iobycall && maybevirtualchan (*channel);

	if (TagOf (x_process) != S_X_SECOND_HALF) {
		badtag (LocnOf (x_process), TagOf (x_process), "mapinput");
		return;
	}
#if 0
fprintf (stderr, "mapextendedhalf: x_process is");
printtreenl (stderr, 4, x_process);
#endif
	mapprocess (OpOf (x_process));
	mapexpopd (channelmode, channel);
	mapioop (I_XEND, usecall);
	return;
}
/*}}}  */
/*{{{  PUBLIC void mapoutput(tptr)*/
/*****************************************************************************
 *
 *  mapoutput allocates workspace and temporaries for an OUTPUT node
 *
 *****************************************************************************/
PUBLIC void mapoutput (treenode * tptr)
{
	treenode *channel = LHSOf (tptr);
	treenode *outputlist = RHSOf (tptr);
	const int noutputitems = listitems (outputlist);
	treenode *channeltype = gettype (channel);

	DEBUG_MSG (("mapoutput\n"));
	if (TagOf (channeltype) != S_CHAN) {
		/*{{{  handle a PORT output */
		treenode **oldtypeaddr;	/* INSdi03067 */
		treenode *const porttype = fixporttype (channel, &oldtypeaddr);
		const int type = ntypeof (channel);
		treenode **outputitem = ThisItemAddr (outputlist);

		/* If we're using device access instructions for ports, we want to make
		   sure that we don't try anything fancy; eg FPU arith.
		   or conversions from real->int which try to store directly from
		   an FPU instruction.
		   Thus we insert a temp. This may be less than optimal, but at least
		   it is correct.
		   We have to do this for 'long' arithmetic too, because any lib calls
		   will otherwise pass the actual result (the port) as a pointer,
		   and the body of the function doesn't know it is supposed to be
		   doing device access to it.
		   This is not necessary for short arith. because this is returned
		   on the integer stack.
		   Conor O'Neill: 08/07/92.
		 */

		if (tx_global.hasdevaccess && !T9000_alpha_nodevaccess (&tx_global)
		    && !isaddressable (*outputitem) && !isconst (*outputitem)
		    && (isreal (type)
			|| isdoublelength (type)
			|| isquadlength (type)
/* || (isshortint(type) && !use_shortintops) *//* not necessary */
			|| (has_fpu_core && isconversion (*outputitem) && isreal (ntypeof (OpOf (*outputitem))))
		    )
			) {
			*outputitem = gettemp (*outputitem, NM_WORKSPACE);
			upusecount (*outputitem, 1);
			mapsimpleassign (type, P_TEMP, outputitem, P_EXP, NDeclAddr (*outputitem));
			mapsimpleassign (type, P_EXP, LHSAddr (tptr), P_TEMP, outputitem);
			freetemp (*outputitem);
		} else {
			mapsimpleassign (type, P_EXP, LHSAddr (tptr), P_EXP, outputitem);
		}
		restoreporttype (LHSOf (tptr), porttype, oldtypeaddr);
		/*}}} */
	} else {
		/*{{{  handle a channel output */
		int channelmode = chanaspointer ? P_EXP : P_PTR;
		int channeltempmode = tempmodeof (channelmode);

		const BOOL counted_array = TagOf (ThisItem (outputlist)) == S_COLON2;
		BOOL create_channel_temp = !issimplelocal (channel, be_lexlevel);
		if (ActionTypeOf (tptr) == NULL) {	/* bug TS/1626 24/4/92 */
			create_channel_temp &= ((noutputitems > 1) || (counted_array && !has_variableio));
		} else {
			create_channel_temp &= counted_array && !has_variableio;
		}

		if (create_channel_temp) {
			/*{{{  preevaluate pointer to channel into temporary before doing outputs */
			DEBUG_MSG (("mapoutput: preevaluating pointer to chan\n"));
			mapexpopd (channelmode, LHSAddr (tptr));
			channel = getopdtemp (channelmode, channel);
			SetLHS (tptr, channel);
			upusecount (channel, 1);
			channelmode = tempmodeof (channelmode);
			/*}}} */
		}
		if (ActionTypeOf (tptr) != NULL) {	/* bug TS/1626 24/04/92 */
			mapoutputitem (channelmode, LHSAddr (tptr), P_EXP, ThisItemAddr (outputlist), ActionTypeOf (tptr), tptr);
		} else {
			treenode *protocol = ProtocolOf (gettype (channel));

			if (TagOf (protocol) == N_SPROTDEF) {
				protocol = NTypeOf (protocol);
			}
			while (!EndOfList (outputlist)) {
				treenode *prottype = (TagOf (protocol) == S_LIST) ? ThisItem (protocol) : protocol;

				/*{{{  map this outputitem, move to next */
				mapoutputitem (channelmode, LHSAddr (tptr), P_EXP, ThisItemAddr (outputlist), prottype, tptr);
				if (TagOf (protocol) == S_LIST) {
					protocol = NextItem (protocol);
				}
				outputlist = NextItem (outputlist);
				/*}}} */
			}
		}
		if (channelmode == channeltempmode) {
			freetemp (channel);
		}
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void mapinput(tptr)*/
/*****************************************************************************
 *
 *  mapinput allocates workspace and temporaries for an INPUT node
 *
 *****************************************************************************/
PUBLIC void mapinput (treenode *tptr)
{
#if 0
fprintf (stderr, "mapinput: tptr =");
printtreenl (stderr, 4, tptr);
#endif
	if (TagOf (tptr) != S_SKIP) {
		treenode *channel = LHSOf (tptr);
		treenode **channeladdr = LHSAddr (tptr);
		treenode *inputlist = skipspecifications (RHSOf (tptr));
		int channelmode = chanaspointer ? P_EXP : P_PTR;
		int channeltempmode = tempmodeof (channelmode);

		switch (inputtypeof (tptr)) {
			/*{{{  channel input */
		case INP_INPUT:
		case INP_X_INPUT:
			{
				const BOOL counted_array = (inputlist ? (TagOf (ThisItem (inputlist)) == S_COLON2) : FALSE);
				BOOL create_channel_temp = !issimplelocal (channel, be_lexlevel);
				const BOOL is_extended = (inputtypeof (tptr) == INP_X_INPUT);
				const BOOL is_empty = (is_extended && !inputlist);

				if (!is_empty) {
					if (ActionTypeOf (tptr) == NULL) {
						/* bug TS/1626 24/4/92 */
						create_channel_temp &= ((listitems (inputlist) > 1) || (counted_array && !has_variableio));
					} else {
						create_channel_temp &= counted_array && !has_variableio;
					}
				}

				datasize = max_INT32 (datasize, DS_IO);

				if (create_channel_temp) {
					/*{{{  preevaluate pointer to channel into temporary before doing outputs */
					mapexpopd (channelmode, channeladdr);
					channel = getopdtemp (channelmode, channel);
					SetLHS (tptr, channel);
					upusecount (channel, 1);
					channelmode = tempmodeof (channelmode);
					/*}}} */
				}
				if (!is_empty) {
					if (ActionTypeOf (tptr) != NULL) {
						/* bug TS/1626 24/04/92 */
						mapinputitem (channelmode, LHSAddr (tptr), P_PTR, ThisItemAddr (inputlist), ActionTypeOf (tptr), tptr, (is_extended ? ActionDuringOf (tptr) : NULL));
					} else {
						treenode *protocol = ProtocolOf (gettype (channel));

						if (TagOf (protocol) == N_SPROTDEF) {
							protocol = NTypeOf (protocol);
						}
						while (!EndOfList (inputlist)) {
							treenode *prottype = (TagOf (protocol) == S_LIST) ? ThisItem (protocol) : protocol;

							/*{{{  map this inputitem, move to next */
							if (!IsLastItem (inputlist)) {
								mapinputitem (channelmode, LHSAddr (tptr), P_PTR, ThisItemAddr (inputlist), prottype, tptr, NULL);
							} else {
								mapinputitem (channelmode, LHSAddr (tptr), P_PTR, ThisItemAddr (inputlist), prottype, tptr, (is_extended ? ActionDuringOf (tptr) : NULL));
							}
							if (TagOf (protocol) == S_LIST) {
								protocol = NextItem (protocol);
							}
							inputlist = NextItem (inputlist);
							/*}}} */
						}
					}
				} else {
					mapextendedhalf (channelmode, LHSAddr (tptr), tptr, ActionDuringOf (tptr));
				}
				if (is_extended) {
					mapprocess (ActionAfterOf (tptr));
				}
				if (channelmode == channeltempmode) {
					freetemp (channel);
				}
			}
			break;
			/*}}} */
			/*{{{  read timer */
		case INP_TIMER_INPUT:
			mapexp (channeladdr);	/* Just to mark the timer as used, also for subscript checks */
			mapaddr (ThisItemAddr (inputlist));
			break;
			/*}}} */
			/*{{{  delayed input */
		case INP_DELAYED_INPUT:
			datasize = max_INT32 (datasize, DS_WAIT);
			mapexp (channeladdr);
			inputlist = RHSOf (tptr);
			mapexp (&inputlist);
#if 0
			/*{{{  T9000_alpha_badrunp */
			if (T9000_alpha_badrunp (&tx_global)) {
				treenode *reserved;
				const BOOL need_temp = !issimple (inputlist);
				if (need_temp) {
					inputlist = gettemp (inputlist, NM_WORKSPACE);
					upusecount (inputlist, 1);
				}
				reserved = alloc_fixedws (0, 1);
				kill_var (reserved);
				if (need_temp) {
					freetemp (inputlist);
					SetRHS (tptr, inputlist);
				}
			}
			/*}}} */
#endif
			break;
			/*}}} */
			/*{{{  port input */
		case INP_PORT_INPUT:
			{
				treenode **oldtypeaddr;	/* INSdi03067 */
				treenode *const porttype = fixporttype (channel, &oldtypeaddr);
				mapsimpleassign (ntypeof (channel), P_EXP, ThisItemAddr (inputlist), P_EXP, channeladdr);
				restoreporttype (*channeladdr, porttype, oldtypeaddr);
			}
			break;
			/*}}} */
			/*{{{  case input */
#if 0				/* this is now transformed into something simpler */
			/*{{{  comment     What the tree looks like after mapping */
			/*
			   S_CASE_INPUT
			   /      \
			   (holds channel ptr) T_TEMP   T_TEMP (holds inputted tag)
			   \
			   S_LIST
			   /    \
			   S_VARIANT  S_LIST
			   /             .
			   S_LIST            .
			   /    \
			   (const val S_CONSTEXP    .
			   of tag)      /           .
			   N_TAGDEF

			   N.B. The temporary inserted in front of the channel is optional, and only
			   inserted if the channel is not a simple local.
			   There is ALWAYS a temporary inserted in front of the variant list, to
			   hold the tag.
			 */
			/*}}} */
		case INP_CASE_INPUT:
			datasize = max_INT32 (datasize, DS_IO);
			{
				if (!issimplelocal (channel, be_lexlevel))
					/*{{{  preevaluate pointer to channel into temporary before doing inputs */
				{
					mapexpopd (channelmode, channeladdr);
					SetLHS (tptr, getopdtemp (channelmode, channel));
					channelmode = tempmodeof (channelmode);
					channel = LHSOf (tptr);
					upusecount (channel, 1);
				}
				/*}}} */
				/*{{{  load tag into tag temporary */
				SetRHS (tptr, gettypedtemp (RHSOf (tptr), NM_WORKSPACE, newleafnode (S_BYTE, NOPOSN)));
				upusecount (RHSOf (tptr), 1);
				mapexpopd (channelmode, LHSAddr (tptr));
				mapioop (I_IN, iobycall && maybevirtualchan (channel));
				freetemp (RHSOf (tptr));
				/*}}} */
				/*{{{  map the body */
				{
					int savedglobchannelmode = globchannelmode;
					treenode **savedglobchannelptr = globchannelptr;

					/* Set up global variables for mapvariant */
					globchannelmode = channelmode;
					globchannelptr = LHSAddr (tptr);
					mapconstruction (inputlist, mapprocess);

					globchannelptr = savedglobchannelptr;
					globchannelmode = savedglobchannelmode;
				}
				/*}}} */
				if (channelmode == channeltempmode)
					freetemp (channel);
			}
			break;
#endif
			/*}}} */
			/*{{{  tagged input */
#if 0				/* this is now transformed into something simpler */
			/*{{{  comment     What the tree looks like after mapping */
			/*
			   S_TAGGED_INPUT
			   /      \
			   (holds channel ptr) T_TEMP   T_TEMP (holds inputted tag)
			   \
			   S_LIST
			   /    \
			   (const val S_CONSTEXP  other input items
			   of tag)      /           .
			   N_TAGDEF          .

			   N.B. The temporary inserted in front of the channel is optional, and only
			   inserted if the channel is not a simple local.
			   There is ALWAYS a temporary inserted in front of the input item list, to
			   hold the tag.
			 */
			/*}}} */
		case INP_TAGGED_INPUT:
			datasize = max_INT32 (datasize, DS_IO);
			{
				if (!issimplelocal (channel, be_lexlevel))
					/*{{{  preevaluate pointer to channel into temporary before doing inputs */
				{
					mapexpopd (channelmode, channeladdr);
					SetLHS (tptr, getopdtemp (channelmode, channel));
					channelmode = tempmodeof (channelmode);
					channel = LHSOf (tptr);
					upusecount (channel, 1);
				}
				/*}}} */
				/*{{{  load tag into tag temporary */
				SetRHS (tptr, gettypedtemp (RHSOf (tptr), NM_WORKSPACE, newleafnode (S_BYTE, NOPOSN)));
				mapexpopd (channelmode, LHSAddr (tptr));
				mapioop (I_IN, iobycall && maybevirtualchan (channel));
				upusecount (RHSOf (tptr), 1);
				freetemp (RHSOf (tptr));
				/*}}} */

				inputlist = NextItem (inputlist);	/* Move past the tag */
				while (!EndOfList (inputlist)) {
					/*{{{  map this inputitem, move to next */
					/* FIXME: null prottype.. */
					mapinputitem (channelmode, LHSAddr (tptr), P_PTR, ThisItemAddr (inputlist), NULL, tptr, NULL);
					inputlist = NextItem (inputlist);
					/*}}} */
				}
				if (channelmode == channeltempmode)
					freetemp (channel);
			}
			break;
#endif
			/*}}} */
		default:
			badtag (LocnOf (tptr), TagOf (tptr), "mapinput");
		}
	}
}

/*}}}*/
/*{{{  PUBLIC void set_alt_ds*/
PUBLIC void set_alt_ds (treenode * const tptr)
{
	alt_ds = timerguardinalt (tptr) ? DS_WAIT : DS_IO;
}

/*}}}*/
/*{{{  PUBLIC void mapalt(tptr)*/
PUBLIC void mapalt (treenode * tptr)
{
	/* The following mapping does the enables and disables */
	/* Reserve wsp0 from the enables and disables.
	   During enabling it may be used as a temporary to trap degenerate
	   ALTs, during disabling it is used by the disabling instructions.
	 */
	const BOOL saved_premapped_alt = premapped_alt;
	const int saved_replaltstacklen = replaltstacklen;
	treenode *const saved_replaltvector = replaltvector;
	treenode *reserved, *replvector, *type;
	const int repls = countnestedalts (tptr, NESTED_REPLS);

	type = newtypenode (S_ARRAY, LocnOf (tptr), newconstant (repls), newleafnode (S_INT, LocnOf (tptr)));
	SetARDim (type, repls);
	DEBUG_MSG (("mapalt: nested repls is %d\n", repls));

	replvector = gettypedtemp (tptr, NM_WORKSPACE, type);
	if (nodetypeoftag (TagOf (tptr)) == CNODE) {
		SetCTemp (tptr, replvector);
	} else {
		SetReplCTemp (tptr, replvector);
	}
	replaltvector = replvector;
	reserved = alloc_fixedws (0, 1);
	insidealtguard = TRUE;
	alt_repl_scope_level = -1;	/* bug TS/1893 14/01/93 */
	set_alt_ds (tptr);

	datasize = max_INT32 (datasize, alt_ds);	/* bug TS/1774 30/07/92 */

	replaltstacklen = 0;	/* used while mapping for usage count of replaltvector */
	premapped_alt = TRUE;	/* indicate that mappreprocess has already been called */
	mapaltenables (tptr);
	kill_var (reserved);
	insidealtguard = FALSE;
	/* Map ALT bodies above ALT enables */
	mapaltbodies (tptr);

	freetemp (replvector);

	replaltvector = saved_replaltvector;
	replaltstacklen = saved_replaltstacklen;
	premapped_alt = saved_premapped_alt;
}

/*}}}*/

/*{{{  PRIVATE void tlengthcheck*/
PRIVATE void tlengthcheck (treenode * const lengthexp, treenode * const prottype)
{
	if ((TagOf (prottype) == S_ANY) &&	/* bug 412 29/8/90 */
	    RANGECHECKING) {	/* bug 1139 4/2/91 */
		/* Check for zero length communications on a CHAN OF ANY */
		texp (lengthexp, MANY_REGS);
		genprimary (I_EQC, 0);
		genprimary (I_LDC, 1);
		gensecondary (I_CSUB0);
		gencomment0 ("check for zero length comm");
		throw_the_result_away ();
	}
}

/*}}}*/
/*{{{  PRIVATE void tsignextend*/
PRIVATE void tsignextend (treenode * const nptr, const BOOL already_zeroed)
{
	if (RANGECHECKING || !already_zeroed) {
		loadname (nptr, 0);
		if (use_shortintops)
			/* variableio 12/3/91 */
			/* When half word support is inserted properly, we can junk all this
			   stuff, and simply insert type conversions into the tree which
			   will then generate the most efficient code anyway.
			   Until then, this is a quick bodge which improves iserver protocol...
			   CON - 12/3/91
			 */
			gensecondary (I_XSWORD);
		else {
			if (!already_zeroed) {
				loadconstant (typemask (S_INT16));
				gensecondary (I_AND);
			}
			if (RANGECHECKING) {
				/* if not range checking, we assume that the count is never negative! */
#if 0
				if (T9000_alpha_noxsword (&tx_global)) {	/* OK if masked first */
					gensecondary (I_XSWORD);
				} else {
					genwidenshort ();
				}
#endif
				genwidenshort ();
			}
		}
		storeinname (nptr, 0);
	}
}

/*}}}*/
/*{{{  PRIVATE void toutputitem*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  toutputitem generates code to output (outputitemmode, outputitem) on
 *              channel (channelmode, channel)
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void toutputitem (const int channelmode, treenode * const channel,
	int outputitemmode, treenode * const outputitem, treenode * const prottype, treenode * const comm)
{
	const BOOL usecall = iobycall && maybevirtualchan (channel);
	const BOOL indirect = is_indirect_name (channel);
	BOOL isplaced;
	DEBUG_MSG (("toutputitem: usecall is %d\n", usecall));
#if 0
fprintf (stderr, "toutputitem: channelmode = %d, channel = ", channelmode);
printtreenl (stderr, 4, channel);
fprintf (stderr, "    outputitemmode = %d, outputitem = ", outputitemmode);
printtreenl (stderr, 4, outputitem);
fprintf (stderr, "    prottype = ");
printtreenl (stderr, 4, prottype);
fprintf (stderr, "    comm = ");
printtreenl (stderr, 4, comm);
#endif

	switch (TagOf (channel)) {
	case N_DECL:
	case N_ABBR:
	case N_PARAM:
		if ((nodetypeoftag (TagOf (NTypeOf (channel))) == TYPENODE) && (TypeAttrOf (NTypeOf (channel)) & TypeAttr_placed)) {
			isplaced = TRUE;
		} else {
			isplaced = FALSE;
		}
		break;
	default:
		isplaced = FALSE;
		break;
	}

	if (TagOf (outputitem) == S_COLON2) {
#if 0	/* no, we don't have variableio in KRoC */
		if (has_variableio)
		 {		/* variableio 11/3/91 */
			/*{{{  use vout instruction */
			treenode *countexp = LeftOpOf (outputitem);
			treenode *arrayexp = RightOpOf (outputitem);
			BOOL preeval_e2, preeval_e3;
			int loadseq;

			tpreexp (arrayexp);	/* This wasn't done earlier for S_COLON2 */

			loadseq = giveorder (P_PTR, arrayexp, channelmode, channel, P_EXP, countexp, &preeval_e2, &preeval_e3);
			tload3regs (P_PTR, arrayexp, channelmode, channel, P_EXP, countexp, loadseq, TRUE);
			checkerror ();
			tioop (I_VOUT, usecall);
		}
		/*}}} */
		else
#endif	/* 0 (variableio) */
			/*{{{  do counted output */
		{
			treenode *countexp = LeftOpOf (outputitem), *arrayexp = RightOpOf (outputitem);
			int countmode = P_EXP;
			const int constantcount = isconst (countexp);
			int counttype = ntypeof (countexp);
			treenode *origtype = NTypeOf (countexp);	/* bug 864 29/1/91 */
			int skiplab = (-1);	/* initialised to shut up gcc's optimiser */
			treenode *dimexp = dimexpof (arrayexp, 0);
			BOOL zeroed_temp = FALSE;
			treenode *leftprottype, *rightprottype;
			/*{{{  set up leftprottype and rightprottype */
			if (TagOf (prottype) == S_ANY) {
				leftprottype = prottype;
				rightprottype = prottype;
			} else {
				leftprottype = LeftOpOf (prottype);
				rightprottype = RightOpOf (prottype);
			}
			/*}}} */
			if (preeval (countmode, countexp))
				/*{{{  preevaluate count to a temporary */
			{
				if (!use_shortintops && isshortint (counttype)

				    /* bug 706 13/9/90 */
				    /* bug 1294: don't zero var if it will be optimised to a word move */
				    /* (Same tests as in moveopd) */
				    /* bug 1294 11/6/91 */
				    &&(isaddressableopd (P_EXP, NDeclOf (countexp))
				       && needtemptoload (P_EXP, NDeclOf (countexp)))) {
					zero_local_var (countexp);
					zeroed_temp = TRUE;
				}
				tsimpleassign (counttype, P_TEMP, countexp, P_EXP, NDeclOf (countexp), MANY_REGS);
				/*SetTag(countexp, T_PREEVALTEMP); */
				temp_mark_as_evaluated (countexp);	/* bug TS/1900 22/10/92 */
				countmode = P_TEMP;
			}
			/*}}} */
			/*{{{  output the count */
			toutputitem (channelmode, channel, countmode, countexp, leftprottype, NULL);
			/*}}} */
			/*{{{  skip if count is 0, check count against size of array */
			if (!constantcount) {
				/* It will be in a temp if it isn't a local simple BYTE, or short INT */
				/* The only conversions that need checking are long ints to int */

				if (isshortint (counttype))
					/* bug 706 13/9/90 */
					/* must be a simplelocal, or would have been mapped to a temporary */
					tsignextend (countexp, zeroed_temp);
				else if (!fitsinword (counttype))
					/*{{{  convert long int temporary to int temporary */
				{
					if (RANGECHECKING) {
						treenode *checktree;
						const int old = switch_to_temp_workspace ();
						if (isquadlength (counttype))	/* bug 1130 29/1/91 */
							checktree = makeconversion (counttype, S_INT, countexp, S_EXACT, LocnOf (outputitem));
						else
							checktree = newmopnode (S_EXACT, NOPOSN, countexp, S_INT);
						switch_to_prev_workspace (old);
						texp (checktree, MANY_REGS);
						checkerror ();	/* bug 1202 11/3/91 */
					}
					if (intnodeptr == NULL) {
						const int old = switch_to_real_workspace ();
						intnodeptr = newleafnode (S_INT, NOPOSN);
						switch_to_prev_workspace (old);
					}
					SetNType (countexp, intnodeptr);	/* bug 864 - see below */
				}
				/*}}} */
				texpopd (countmode, countexp, MANY_REGS);
				skiplab = newlab ();
				genbranch (I_CJ, skiplab);
			}
			/*}}} */
			/*{{{  preprocess the arrayexp */
			/* The array expression may contain segments whose base and length
			   cannot be calculated until the count has been inputted.  */
			DEBUG_MSG (("\ntoutputitem: COLON2: calling preexp on arrayexp"));
			tpreexp (arrayexp);
			/*}}} */
			/*{{{  output the array from 0 for count */
			if	/* count is variable or count is constant but not zero */
				(!constantcount || (LoValOf (countexp) != 0))
				/*{{{  output the array from 0 for count */
			{
				treenode *arraytype = ARTypeOf (gettype (arrayexp));	/* bug 1205 13/3/91 */
				const INT32 b = bytesin (arraytype);
				treenode *checkedcount;
				int checkcount = FALSE;
				/*{{{  add range checking to countexp if neccessary */
				if (RANGECHECKING && (!constantcount || !isconst (dimexp))) {
					const int old = switch_to_temp_workspace ();
					DEBUG_MSG (("\ntoutputitem: COLON2: inserting range check"));
					checkedcount = newdopnode (S_CCNT1, NOPOSN, countexp, dimexp, S_INT);
					switch_to_prev_workspace (old);
					checkcount = TRUE;
				} else {
					DEBUG_MSG (("\ntoutputitem: COLON2: no range check"));
					checkedcount = countexp;
				}
				/*}}} */
				if (constantcount) {
					/*{{{  ldptr array; ldptr channel; ldc length */
					DEBUG_MSG (("\ntoutputitem: COLON2: calling texp on checkedcount"));
					if (checkcount)
						texp (checkedcount, MANY_REGS);
					DEBUG_MSG (("\ntoutputitem: COLON2: calling tload2regs"));
					tload2regs (P_PTR, arrayexp, isplaced ? P_EXP : channelmode, channel, FALSE, TRUE);
					loadconstant (b * LoValOf (countexp));
					/*}}} */
				} else {
					/*{{{  ldptr array; ldptr channel; ld length */
					treenode *lengthexp;
					BOOL preeval_e2, preeval_e3;
					int loadseq;
					/*{{{  lengthexp = checkedcount * scale */
					if (b != 1) {
						int old = switch_to_temp_workspace ();

#if 0				/* bug 1205 13/3/91 */
						lengthexp = newdopnode (S_TIMES, NOPOSN, checkedcount, newconstant (b), S_INT);
#else
						lengthexp = newdopnode (S_TIMES, NOPOSN, checkedcount,
									scaletreeof (arraytype, 1, be_lexlevel), S_INT);
#endif
						switch_to_prev_workspace (old);
					} else {
						lengthexp = checkedcount;
					}
					/*}}} */
					DEBUG_MSG (("\ntoutputitem: COLON2: calling tload3regs"));
					loadseq = giveorder (P_PTR, arrayexp, channelmode, channel, P_EXP, lengthexp, &preeval_e2, &preeval_e3);
					tload3regs (P_PTR, arrayexp, isplaced ? P_EXP : channelmode, channel, P_EXP, lengthexp, loadseq, TRUE);
					/*}}} */
				}
				checkerror ();	/* bug 1202 11/3/91 */
				if (indirect) {
					/* load real channel address */
					genindirect (INDIR_BREG);
				}
				tioop (isplaced ? I_EXTOUT : I_OUT, usecall);
			}
			/*}}} */
			/*}}} */
			SetNType (countexp, origtype);	/* bug 864 */
			if (!constantcount) {
				setlab (skiplab);
			}
		}
		/*}}} */
#ifdef MOBILES
	} else if ((ntypeof (outputitem) == S_ANYCHANTYPE) && (TagOf (follow_user_type (prottype)) == S_ANYCHANTYPE)) {
		/*{{{  MOBILE.CHAN output*/
		loadhiddentypeptr (outputitem, MANY_REGS);
#if 0
		loadmobilepointer (outputitem);
		genprimary (I_LDNLP, 1);		/* load pointer to type-hash */
#endif
		texpopd_main (channelmode, channel, MANY_REGS, FALSE);
		genprimary (I_LDC, 4);
		tioop (isplaced ? I_EXTOUT : I_OUT, usecall);

		loadmobilepointer (outputitem);
		texpopd_main (channelmode, channel, MANY_REGS, FALSE);
		tioop (isplaced ? I_EXT_MT_OUT : I_MT_OUT, usecall);

		/* gencleardynchantype (outputitem); */
		/*}}}*/
	} else if ((ntypeof (outputitem) == S_ANYPROCTYPE) && (TagOf (follow_user_type (prottype)) == S_ANYPROCTYPE)) {
		/*{{{  MOBILE.PROC output*/
		loadhiddentypeptr (outputitem, MANY_REGS);

		texpopd_main (channelmode, channel, MANY_REGS, FALSE);
		genprimary (I_LDC, 4);
		tioop (isplaced ? I_EXTOUT : I_OUT, usecall);

		loadmobilepointer (outputitem);
		texpopd_main (channelmode, channel, MANY_REGS, FALSE);
		tioop (isplaced ? I_EXT_MT_OUT : I_MT_OUT, usecall);

		gencleardynchantype (outputitem);
		/*}}}*/
	} else if ((ntypeof (outputitem) == S_MOBILE) && (TagOf (follow_user_type (prottype)) == S_MOBILE)) {
		/* MOBILE output on MOBILE channel *//*{{{*/
#if 0
fprintf (stderr, "toutputitem: MOBILE output on MOBILE channel.  isdynmobilebarrier (outputitem) = %d\n", isdynmobilebarrier (outputitem));
#endif
		DEBUG_MSG (("\ntoutputitem: MOBILE output"));
		if (isdynmobilearray (outputitem)) {
			/*{{{  dynamic mobile array output*/
			loadmobilepointer (outputitem);
			texpopd_main (channelmode, channel, MANY_REGS, FALSE);
			checkerror ();
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (isplaced ? I_EXT_MT_OUT : I_MT_OUT, usecall);
			gencleardynarray (outputitem, TRUE);
			/*}}}*/
		} else if (isdynmobilebarrier (outputitem)) {
			/*{{{  mobile barrier channel output*/
			loadmobile (outputitem);
			genchecknotnull ();
			gensecondary (I_POP);

			loadmobilepointer (outputitem);
			texpopd_main (channelmode, channel, MANY_REGS, FALSE);
			checkerror ();
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (isplaced ? I_EXT_MT_OUT : I_MT_OUT, usecall);
			/*}}}*/
		} else if (isdynmobilechantype (outputitem) || ((TagOf (outputitem) == S_CLONE) && isdynmobilechantype (OpOf (outputitem)))) {
			/*{{{  non-cloned mobile channel output*/
			treenode *cb;
			if (TagOf (outputitem) == S_CLONE) {
				cb = OpOf (outputitem);
			} else {
				cb = outputitem;
			}
			loadmobilepointer (cb);
			texpopd_main (channelmode, channel, MANY_REGS, FALSE);
			checkerror ();
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (isplaced ? I_EXT_MT_OUT : I_MT_OUT, usecall);
			/* if appropriate instruction undefines pointer */
			/*}}}*/
		} else if (isdynmobileproctype (outputitem)) {
			/*{{{  mobile process output*/
			loadmobilepointer (outputitem);
			texpopd_main (channelmode, channel, MANY_REGS, FALSE);
			checkerror ();
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (isplaced ? I_EXT_MT_OUT : I_MT_OUT, usecall);
			/* clear output item to NULL */
			gencleardynproctype (outputitem);
			/*}}}*/
		} else {
			loadmobilepointer (outputitem);
			texpopd_main (channelmode, channel, MANY_REGS, FALSE);
			checkerror ();
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (I_MT_XCHG, usecall);
		}
		/*}}}*/
#endif
	} else {
		/* bug INSdi02071 - just do this here? */
		outputitemmode = simplify (outputitemmode, outputitem);
		{
			/* Bug 1203 - 12/3/91 - don't use outbyte and/or outword if the
			   output item is already addressable.
			   Unlike in the mapping phase, if the value is a scalar constant
			   and has been put into a constant table, then isaddressable
			   will return TRUE, and everything else falls out correctly.
			 */
			/* Bug TS/1504 - 07/04/92:
			   If we're optimising for space, DO use the shorter forms if possible.
			 */
			/*const int addressable = isaddressableopd(outputitemmode, outputitem); */
			/*const BOOL useout = isaddressableopd(outputitemmode, outputitem); */
			BOOL useout = ((code_style_flags & CODE_STYLE_SPACE_NOT_TIME) == 0) && isaddressableopd (outputitemmode, outputitem);
			const int t = ntypeof (outputitem);

			/* don't need to do complex test here: mapoutputitem will have
			 * turned it into a temporary if necessary */

			if (isplaced) {
				treenode *const type = gettype (outputitem);
				const INT32 s = bytesin (type);

				outputitemmode = simplify (outputitemmode, outputitem);
				outputitemmode = ptrmodeof (outputitemmode);
				if (s == -1) {
					/* length is an expression */
					/*{{{  check hidden dimensions */
					if ((TagOf (prottype) != S_ANY) && RANGECHECKING) {
						tcheckdimensions (type, prottype);
					}
					/*}}} */
					/*{{{  load operands for an 'out' instruction */
					{
						BOOL preeval_e2, preeval_e3;
						int loadseq;
						treenode *lengthexp;
						int lengthmode = P_EXP;
						lengthexp = ThisItem (NextItem (RHSOf (comm)));
						lengthmode = simplify (lengthmode, lengthexp);

						/* We only do this call so that we have a loadseq parameter for
						   tload3regs, ideally we should make the binder save loadseq
						   in the tree. */
						loadseq = giveorder (outputitemmode, outputitem,
								     channelmode, channel, lengthmode, lengthexp, &preeval_e2, &preeval_e3);
						tlengthcheck (lengthexp, prottype);
						tload3regs (outputitemmode, outputitem, P_EXP, channel, lengthmode, lengthexp, loadseq, TRUE);
					}
					/*}}} */
				} else {
					tload2regs (outputitemmode, outputitem, P_EXP, channel, FALSE, TRUE);
					genprimary (I_LDC, s);
				}
				checkerror ();
				if (indirect) {
					/* load real channel address */
					genindirect (INDIR_BREG);
				}
				tioop (I_EXTOUT, usecall);
			} else if (!useout && istargetintsize (t)) {	/* bug 1203 12/3/91 */
				/*{{{  ldptr channel; ld outputitem; outword */
				tload2regs (channelmode, channel, outputitemmode, outputitem, FALSE, TRUE);
				checkerror ();	/* bug 1202 11/3/91 */
				if (indirect) {
					/* load real channel address */
					genindirect (INDIR_BREG);
				}
				tioop (I_OUTWORD, usecall);
				/*}}} */
			} else if (!useout && istargetbytesize (t)) {	/* bug 1203 12/3/91 */
				/*{{{  ldptr channel; ld outputitem; outbyte */
				tload2regs (channelmode, channel, outputitemmode, outputitem, FALSE, TRUE);
				checkerror ();	/* bug 1202 11/3/91 */
				if (indirect) {
					/* load real channel address */
					genindirect (INDIR_BREG);
				}
				tioop (I_OUTBYTE, usecall);
				/*}}} */
			} else {
				/*{{{  ldptr outputitem; ldptr channel; ld length; out */
				treenode *const type = gettype (outputitem);

				const INT32 s = bytesin (type);
				/*{{{  preevaluate the outputitem if necessary */
#if 0				/* bug 743 makes this evaluate twice 21/9/90 */
				if (preeval (outputitemmode, outputitem)) {
					int tmode = tempmodeof (outputitemmode);
					tsimpleassign (TagOf (type), tmode, outputitem, outputitemmode, NDeclOf (outputitem), MANY_REGS);
					outputitemmode = tmode;
				}
#else
				outputitemmode = simplify (outputitemmode, outputitem);
#endif
				/*}}} */
				outputitemmode = ptrmodeof (outputitemmode);
				if (s == (-1)) {
					/*{{{  length is an expression */
					/*{{{  check hidden dimensions */
					if ((TagOf (prottype) != S_ANY) && RANGECHECKING) {
						tcheckdimensions (type, prottype);
					}
					/*}}} */
					/*{{{  load operands for an 'out' instruction */
					{
						BOOL preeval_e2, preeval_e3;
						int loadseq;
						treenode *lengthexp;
						int lengthmode = P_EXP;
#if 0
						/* bug TS/1626 24/04/92 */
						int old = switch_to_temp_workspace ();
						lengthexp = scaletreeof ((TagOf (prottype) != S_ANY) ? prottype : type, 1, be_lexlevel);
						switch_to_prev_workspace (old);
#else
						lengthexp = ThisItem (NextItem (RHSOf (comm)));
						lengthmode = simplify (lengthmode, lengthexp);
#endif

						/* We only do this call so that we have a loadseq parameter for
						   tload3regs, ideally we should make the binder save loadseq
						   in the tree. */
						loadseq = giveorder (outputitemmode, outputitem,
								     channelmode, channel, lengthmode, lengthexp, &preeval_e2, &preeval_e3);
						tlengthcheck (lengthexp, prottype);
						tload3regs (outputitemmode, outputitem, channelmode, channel, lengthmode, lengthexp, loadseq, TRUE);
					}
					/*}}} */
					/*}}} */
				} else {
					tload2regs (outputitemmode, outputitem, channelmode, channel, FALSE, TRUE);
					genprimary (I_LDC, s);
				}
				checkerror ();	/* bug 1202 11/3/91 */
				if (indirect) {
					/* load real channel address */
					genindirect (INDIR_BREG);
				}
				tioop (I_OUT, usecall);
				/*}}} */
			}
		}
	}
}

/*}}}*/
/*{{{  PUBLIC void tinputitem*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tinputitem generates code to input 'inputitem' on channel
 *              (channelmode, channel)
 *  frmb - added x_process which is a nested process during extended input
 *         caller decides whether we need extended rendezvous.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void tinputitem (const int channelmode, treenode *const channel,
	const int inputitemmode, treenode *const inputitem, treenode *const prottype, treenode *const comm, treenode *x_process)
{
	const BOOL usecall = iobycall && maybevirtualchan (channel);
	const BOOL is_extended = (x_process != NULL);
	const BOOL indirect = is_indirect_name (channel);
	BOOL isplaced;

	switch (TagOf (channel)) {
	case N_DECL:
	case N_ABBR:
	case N_PARAM:
		if ((nodetypeoftag (TagOf (NTypeOf (channel))) == TYPENODE) && (TypeAttrOf (NTypeOf (channel)) & TypeAttr_placed)) {
			isplaced = TRUE;
		} else {
			isplaced = FALSE;
		}
		break;
	default:
		isplaced = FALSE;
		break;
	}

	DEBUG_MSG (("tinputitem: usecall is %d, is_extended = %d\n", usecall, is_extended));
#if 0
fprintf (stderr, "tinputitem: (channelmode = %d, dynmobile = %d, usecall = %d, is_extended = %d) comm =", channelmode, isdynmobilearray (inputitem), (int)usecall, (int)is_extended);
printtreenl (stderr, 4, comm);
#endif
#if 0	/* no, we don't have variable IO (just saves some compiler back-end..) */
	if ((TagOf (inputitem) == S_COLON2) && has_variableio) {
		/* variableio 14/3/91 */
		/*{{{  Use the VIN instruction */
		treenode *arrayexp = RightOpOf (inputitem);
		treenode *lhs = LeftOpOf (inputitem);
		treenode *maxsize = ThisItem (lhs);	/* Max size of input in bytes */
		treenode *temp = ThisItem (NextItem (lhs));	/* Temp to store the byte count */
		treenode *expression = ThisItem (NextItem (NextItem (lhs)));
		treenode *count = ThisItem (NextItem (NextItem (NextItem (lhs))));
		const int counttype = ntypeof (count);
		BOOL preeval_e2, preeval_e3;
		int loadseq;

		tpreexp (arrayexp);	/* This wasn't done earlier for S_COLON2 */

		loadseq = giveorder (P_PTR, arrayexp, channelmode, channel, P_EXP, maxsize, &preeval_e2, &preeval_e3);
		tload3regs (P_PTR, arrayexp, channelmode, channel, P_EXP, maxsize, loadseq, TRUE);
		checkerror ();
		tioop (I_VIN, usecall);
		if (!usecall) {
			gensecondary (I_LDCNT);
		}
		checkerror ();
		if (temp == NULL) {	/* optimised case */
			/*{{{  Divide by the size of each array element */
			{
				const INT32 b = LoValOf (expression);
				const int exponent = whichpowerof2 (b);
				if (b == 1);	/* skip */
				else if (exponent > 0) {
					genprimary (I_LDC, exponent);
					gensecondary (I_SHR);
				} else {
					texp (expression, MAXREGS - 1);	/* can only take 1 register cos constant */
					if (t450a_workarounds)
						t450_divrem_workaround (I_DIV);
					else
						gensecondary (I_DIV);
				}
			}
			/*}}} */
			/*{{{  Type convert and store */
			if (isdoublelength (counttype)) {
				gensecondary (I_XDBLE);
				storeinopd (P_EXP, count, 0, MAXREGS - 2);
				storeinopd (P_EXP, count, 1, MAXREGS - 1);
			} else {
				ttypeconversion (S_INT, counttype);
				storeinopd (P_EXP, count, 0, MAXREGS - 1);
			}
			/*}}} */
		} else {	/* general case */

			storeinname (temp, 0);
			tsimpleassign (counttype, P_EXP, count, P_EXP, expression, MANY_REGS);
		}
		/*}}} */
	} else
#endif	/* 0 -- has_variableio */
	if (TagOf (inputitem) == S_COLON2) {
		/*{{{  do counted input */
		treenode *countexp = LeftOpOf (inputitem), *arrayexp = RightOpOf (inputitem);
		treenode *leftprottype, *rightprottype;
		treenode *origexp = countexp;	/* added for bug 864 */
		treenode *origtype = NTypeOf (countexp);	/* ditto */
		int countmode = P_EXP;
		const int counttype = ntypeof (countexp);
		const int skiplab = newlab ();

		/*{{{  set up leftprottype and rightprottype */
		if (TagOf (prottype) == S_ANY) {
			leftprottype = prottype;
			rightprottype = prottype;
		} else {
			leftprottype = LeftOpOf (prottype);
			rightprottype = RightOpOf (prottype);
		}
		/*}}} */
		/*{{{  input the count */
		if (preeval (countmode, countexp)) {
			countmode = P_TEMP;
			/* we initialise INT16 temporaries here, unlike normal, so that
			   the sign extension goes better (bug 706 11/9/90) */
			if ( /*istargetbytesize(counttype) */ isshorttype (counttype))
				zero_local_var (countexp);
			/*{{{  input to the temporary */
			tinputitem (channelmode, channel, P_TEMPPTR, countexp, leftprottype, NULL, NULL);
			/*}}} */
			if (isshortint (counttype))	/* bug 706 11/9/90 */
				tsignextend (countexp, TRUE);
			/*{{{  move count to its real destination */
			tsimpleassign (counttype, P_EXP, NDeclOf (countexp), P_TEMP, countexp, MANY_REGS);
			/*SetTag(countexp, T_PREEVALTEMP); */
			temp_mark_as_evaluated (countexp);	/* bug TS/1900 22/10/92 */

			/*}}} */
		} else {
			countmode = P_EXP;
			/*{{{  input the count */
			tinputitem (isplaced ? P_EXP : channelmode, channel, P_PTR, countexp, leftprottype, NULL, NULL);
			/*}}} */
			if (isshortint (counttype)) {
				/* bug 706 11/9/90 */
				/* must be a simplelocal, or would have been mapped to a temporary */
				tsignextend (countexp, FALSE);
			}
		}
		/*}}} */
		/*{{{  convert a long int to int if neccessary */
		/* It will be in a temp if it isn't a local simple BYTE, or short INT */
		/* The only conversions that need checking are long ints to int */
		if (!fitsinword (counttype)) {
			/*{{{  convert long int temporary to int temporary */
			if (RANGECHECKING) {
				treenode *checktree;
				int old = switch_to_temp_workspace ();
				if (isquadlength (counttype))	/* bug 1130 29/1/91 */
					checktree = makeconversion (counttype, S_INT, countexp, S_EXACT, LocnOf (inputitem));
				else
					checktree = newmopnode (S_EXACT, NOPOSN, countexp, S_INT);
				switch_to_prev_workspace (old);
				texp (checktree, MANY_REGS);
				checkerror ();	/* bug 1202 11/3/91 */
			}
			if (intnodeptr == NULL) {
				const int old = switch_to_real_workspace ();
				intnodeptr = newleafnode (S_INT, NOPOSN);
				switch_to_prev_workspace (old);
			}
			SetNType (countexp, intnodeptr);	/* bug 864 - see below */
			/*}}} */
		}
		/*}}} */
		/*{{{  if count = 0 goto skiplab */
		texpopd (countmode, countexp, MANY_REGS);
		genbranch (I_CJ, skiplab);
		/*}}} */
		/*{{{  preprocess the arrayexp */
		/* The arrray expression may contain segments whose base and length
		   cannot be calculated until the count has been inputted.  */
		tpreexp (arrayexp);
		/*}}} */
		/*{{{  add rangecheck to count expression if neccessary */
		if (RANGECHECKING) {
			const int old = switch_to_temp_workspace ();
			countexp = newdopnode (S_CCNT1, NOPOSN, countexp, dimexpof (arrayexp, 0), S_INT);
			switch_to_prev_workspace (old);
		}
		/*}}} */
		/*{{{  input array from 0 for count */
		{
			int loadseq;
			BOOL preeval_e2, preeval_e3;
			treenode *lengthexp;
			/*{{{  lengthexp = countexp * scale */
			{
				treenode *arraytype = ARTypeOf (gettype (arrayexp));
				const INT32 b = bytesin (arraytype);
				if (b != 1) {
					const int old = switch_to_temp_workspace ();
#if 0				/* bug 1205 13/3/91 */
					lengthexp = newdopnode (S_TIMES, NOPOSN, countexp, newconstant (b), S_INT);
#else
					lengthexp = newdopnode (S_TIMES, NOPOSN, countexp, scaletreeof (arraytype, 1, be_lexlevel), S_INT);
#endif
					switch_to_prev_workspace (old);
				} else
					lengthexp = countexp;
			}
			/*}}} */
			/* We only do this call so that we have a loadseq parameter for
			   tload3regs, ideally we should make the binder save loadseq
			   in the tree. */
			if (is_extended) {
				/* extended input on the actual data */
				texpopd_main (channelmode, channel, MANY_REGS, FALSE);
				if (indirect) {
					/* load real channel address */
					genindirect (INDIR_AREG);
				}
				tioop (I_XABLE, usecall);
			}
			loadseq = giveorder (P_PTR, arrayexp, channelmode, channel, P_EXP, lengthexp, &preeval_e2, &preeval_e3);
			tload3regs (P_PTR, arrayexp, channelmode, channel, P_EXP, lengthexp, loadseq, TRUE);
			checkerror ();	/* bug 1202 11/3/91 */
			if (is_extended) {
				if (indirect) {
					/* load real channel address */
					genindirect (INDIR_BREG);
				}
				tioop (I_XIN, usecall);
				/* then do process */
#if 0
fprintf (stderr, "tinputitem: generating x_process =");
printtreenl (stderr, 4, x_process);
#endif
				tprocess (x_process);
				/* release outputting process */
				texpopd_main (channelmode, channel, MANY_REGS, FALSE);
				if (indirect) {
					/* load real channel address */
					genindirect (INDIR_AREG);
				}
				tioop (I_XEND, usecall);
			} else {
				if (indirect) {
					/* load real channel address */
					genindirect (INDIR_BREG);
				}
				tioop (isplaced ? I_EXTIN : I_IN, usecall);
			}
		}
		/*}}} */
		/*{{{  clear up the mess */
		/* bug 864 - if countexp was a long, we temporarily modified it
		   to pretend that it was an INT. This reclaims the type tree. 28/1/91.
		 */
		SetNType (origexp, origtype);
		/*}}} */
		setlab (skiplab);
		/*}}} */
#ifdef MOBILES
	} else if ((ntypeof (inputitem) == S_ANYCHANTYPE) && (TagOf (follow_user_type (prottype)) == S_ANYCHANTYPE)) {
		/*{{{  MOBILE.CHAN input, into two words of workspace*/
		const BOOL utagged = (TagOf (inputitem) == S_UNDEFINED);

		/* maybe free the target */
		if (utagged) {
			if (MOpTypeOf (inputitem)) {
				gencondfreedynchantype (OpOf (inputitem));
			}
		} else {
			gencondfreedynchantype (inputitem);
		}

#if 0
		/* first communication is the hash-code */
		texpopd_main (inputitemmode, utagged ? OpOf (inputitem) : inputitem, MANY_REGS, FALSE);
		/* offset by 4 */
		genprimary (I_LDNLP, 1);
#endif
		loadhiddentypeptr (inputitem, MANY_REGS);
		texpopd_main (channelmode, channel, MAXREGS - 1, FALSE);
		genprimary (I_LDC, 4);

		if (indirect) {
			/* load real channel address */
			genindirect (INDIR_BREG);
		}
		tioop (isplaced ? I_EXTIN : I_IN, usecall);

		/* second is the actual mobile channel */
		if (!is_extended) {
			loadmobilepointer (utagged ? OpOf (inputitem) : inputitem);
			texpopd_main (channelmode, channel, MAXREGS - 1, FALSE);

			/* tload2regs (inputitemmode, utagged ? OpOf (inputitem) : inputitem, channelmode, channel, FALSE, FALSE); */
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (isplaced ? I_EXT_MT_IN : I_MT_IN, usecall);
		}
		if (is_extended) {
			texpopd_main (channelmode, channel, MANY_REGS, FALSE);
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (I_XABLE, usecall);

			loadmobilepointer (utagged ? OpOf (inputitem) : inputitem);
			texpopd_main (channelmode, channel, MAXREGS - 1, FALSE);
			/* tload2regs (inputitemmode, utagged ? OpOf (inputitem) : inputitem, channelmode, channel, FALSE, FALSE); */
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (I_MT_XIN, usecall);

			tprocess (x_process);		/* generate "during" process */
			/* release outputting process */
			texpopd_main (channelmode, channel, MANY_REGS, FALSE);
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (I_XEND, usecall);
		}

		/*}}}*/
	} else if ((ntypeof (inputitem) == S_ANYPROCTYPE) && (TagOf (follow_user_type (prottype)) == S_ANYPROCTYPE)) {
		/*{{{  MOBILE.PROC input, into two words of workspace*/
		const BOOL utagged = (TagOf (inputitem) == S_UNDEFINED);

		/* maybe free the target */
		if (utagged) {
			if (MOpTypeOf (inputitem)) {
				gencondfreedynproctype (OpOf (inputitem));
			}
		} else {
			gencondfreedynproctype (inputitem);
		}

#if 0
		/* first communication is the hash-code */
		texpopd_main (inputitemmode, utagged ? OpOf (inputitem) : inputitem, MANY_REGS, FALSE);
		/* offset by 4 */
		genprimary (I_LDNLP, 1);
#endif
		loadhiddentypeptr (inputitem, MANY_REGS);
		texpopd_main (channelmode, channel, MAXREGS - 1, FALSE);
		genprimary (I_LDC, 4);

		if (indirect) {
			/* load real channel address */
			genindirect (INDIR_BREG);
		}
		tioop (isplaced ? I_EXTIN : I_IN, usecall);

		/* second is the actual mobile channel */
		if (!is_extended) {
			loadmobilepointer (utagged ? OpOf (inputitem) : inputitem);
			texpopd_main (channelmode, channel, MAXREGS - 1, FALSE);

			/* tload2regs (inputitemmode, utagged ? OpOf (inputitem) : inputitem, channelmode, channel, FALSE, FALSE); */
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (isplaced ? I_EXT_MT_IN : I_MT_IN, usecall);
		}
		if (is_extended) {
			texpopd_main (channelmode, channel, MANY_REGS, FALSE);
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (I_XABLE, usecall);

			loadmobilepointer (utagged ? OpOf (inputitem) : inputitem);
			texpopd_main (channelmode, channel, MAXREGS - 1, FALSE);
			/* tload2regs (inputitemmode, utagged ? OpOf (inputitem) : inputitem, channelmode, channel, FALSE, FALSE); */
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (I_MT_XIN, usecall);

			tprocess (x_process);		/* generate "during" process */
			/* release outputting process */
			texpopd_main (channelmode, channel, MANY_REGS, FALSE);
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (I_XEND, usecall);
		}

		/*}}}*/
	} else if ((ntypeof (inputitem) == S_MOBILE) && (TagOf (follow_user_type (prottype)) == S_MOBILE)) {
		/*{{{  mobile input*/
		const BOOL isdynmobile = isdynmobilearray (inputitem);
		const BOOL isdynchantype = isdynmobilechantype (inputitem);
		const BOOL isdynproctype = isdynmobileproctype (inputitem);
		const BOOL isdynbarriertype = isdynmobilebarrier (inputitem);
		const BOOL utagged = (TagOf (inputitem) == S_UNDEFINED);

#if 0
fprintf (stderr, "gen10: tinputitem: MOBILE input!\n");
#endif
		/* MOBILE input */
		if (isdynmobile) {
			/* maybe free contents */
			if (utagged) {
				/* MOpType is set in the undefined checker, only undefined if 0 */
				if (MOpTypeOf (inputitem)) {
					gencondfreedynmobile (OpOf (inputitem));
				}
			} else {
				gencondfreedynmobile (inputitem);
			}
		} else if (isdynchantype) {
			/* maybe free contents */
			if (utagged) {
				if (MOpTypeOf (inputitem)) {
					gencondfreedynchantype (OpOf (inputitem));
				}
			} else {
				gencondfreedynchantype (inputitem);
			}
		} else if (isdynbarriertype) {
			/* maybe free contents */
			if (utagged) {
				if (MOpTypeOf (inputitem)) {
					gencondfreedynbarriertype (OpOf (inputitem), TRUE);
				}
			} else {
				gencondfreedynbarriertype (inputitem, TRUE);
			}
		} else if (isdynproctype) {
			/* maybe free contents */
			if (utagged) {
				if (MOpTypeOf (inputitem)) {
					gencondfreedynproctype (OpOf (inputitem));
				}
			} else {
				gencondfreedynproctype (inputitem);
			}
		}

		if (is_extended) {
			texpopd_main (channelmode, channel, MANY_REGS, FALSE);
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (I_XABLE, usecall);
		}

		if (utagged) {
			loadmobilepointer (OpOf (inputitem));
		} else {
			loadmobilepointer (inputitem);
		}
		texpopd_main (channelmode, channel, MANY_REGS, FALSE);
		checkerror ();
		if (isdynmobilearray (inputitem)) {
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}

			if (isplaced) {
				if (is_extended) {
					genwarn (GEN_WARN_BADCODE);
				}
				tioop (I_EXT_MT_IN, usecall);
			} else {
				tioop (is_extended ? I_MT_XIN : I_MT_IN, usecall);
			}
			
			if (utagged) {
				genmobileunpack (OpOf (inputitem), FALSE, TRUE);
			} else {
				genmobileunpack (inputitem, FALSE, TRUE);
			}

		} else if (isdynbarriertype || isdynchantype || isdynproctype) {
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			
			if (isplaced) {
				if (is_extended) {
					genwarn (GEN_WARN_BADCODE);
				}
				tioop (I_EXT_MT_IN, usecall);
			} else {
				tioop (is_extended ? I_MT_XIN : I_MT_IN, usecall);
			}
		} else {
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			if (isplaced) {
				genwarn (GEN_WARN_BADCODE);
			}
			tioop (is_extended ? I_MT_XXCHG : I_MT_XCHG, usecall);
		}
		if (is_extended) {
			tprocess (x_process);		/* generate "during" process */
			/* release outputting process */
			texpopd_main (channelmode, channel, MANY_REGS, FALSE);
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_AREG);
			}
			tioop (I_XEND, usecall);
		}
		/*}}}*/
#endif
	} else {
		/*{{{  input an element */
		treenode *type = gettype (inputitem);
		const INT32 s = bytesin (type);

#if 0
fprintf (stderr, "gen10: tinputitem: (element): channelmode = %d, channel = ", channelmode);
printtreenl (stderr, 4, channel);
#endif
		if (s == (-1)) {
			/*{{{  length is an expression */
			/*{{{  check hidden dimensions */
			if ((TagOf (prottype) != S_ANY) && RANGECHECKING) {
				tcheckdimensions (type, prottype);
			}
			/*}}} */
			/*{{{  load operands for an 'in' instruction */
			if (is_extended) {
				/* going to do XABLE/../XEND */
			} else {
				BOOL preeval_e2, preeval_e3;
				int loadseq;
				treenode *lengthexp;
				int lengthmode = P_EXP;

				lengthexp = ThisItem (NextItem (RHSOf (comm)));
				lengthmode = simplify (lengthmode, lengthexp);

				/* We only do this call so that we have a loadseq parameter for
				   tload3regs, ideally we should make the binder save loadseq
				   in the tree. */
				loadseq = giveorder (inputitemmode, inputitem, channelmode, channel, lengthmode, lengthexp, &preeval_e2, &preeval_e3);
				tlengthcheck (lengthexp, prottype);
				tload3regs (inputitemmode, inputitem, channelmode, channel, lengthmode, lengthexp, loadseq, TRUE);
			}
			/*}}} */
			/*}}} */
		} else {
			if (is_extended) {
				/* going to do XABLE/../XEND */
			} else {
				tload2regs (inputitemmode, inputitem, channelmode, channel, FALSE, TRUE);
				genprimary (I_LDC, s);
			}
		}
		if (!is_extended) {
			checkerror ();	/* bug 1202 11/3/91 */
		}
		if (is_extended) {
			const BOOL skip_xable = (ActionFlagsOf (comm) & ActionFlag_skip_xable);

			/* if inside ALT, skip XABLE */
			if (!skip_xable) {
				texpopd_main (channelmode, channel, MANY_REGS, FALSE);
				checkerror ();	/* bug 1202 11/3/91 */
				/* have channel word on stack, do XABLE */
				if (indirect) {
					/* load real channel address */
					genindirect (INDIR_AREG);
				}
				tioop (I_XABLE, usecall);
			} else {
				gencomment0 ("skipping XABLE");
			}
			if (s != (-1)) {
				tload2regs (inputitemmode, inputitem, channelmode, channel, FALSE, TRUE);
				if (skip_xable) {
					checkerror ();  /* bug 1202 */
				}
				genprimary (I_LDC, s);
			} else {
				BOOL preeval_e2, preeval_e3;
				int loadseq;
				treenode *lengthexp;
				int lengthmode = P_EXP;

				lengthexp = ThisItem (NextItem (RHSOf (comm)));
				lengthmode = simplify (lengthmode, lengthexp);

				/* We only do this call so that we have a loadseq parameter for
				   tload3regs, ideally we should make the binder save loadseq
				   in the tree. */
				loadseq = giveorder (inputitemmode, inputitem, channelmode, channel, lengthmode, lengthexp, &preeval_e2, &preeval_e3);
				tlengthcheck (lengthexp, prottype);
				tload3regs (inputitemmode, inputitem, channelmode, channel, lengthmode, lengthexp, loadseq, TRUE);
				if (skip_xable) {
					checkerror ();  /* bug 1202 */
				}
			}
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_BREG);
			}
			tioop (I_XIN, usecall);
#if 0
fprintf (stderr, "tinputitem: generating x_process =");
printtreenl (stderr, 4, x_process);
#endif
			if (TagOf (x_process) == S_X_FIRST_HALF) {
				/* do during process (probably SKIP), but don't release outputter */
				tprocess (OpOf (x_process));		/* generate "during" process */
			} else {
				tprocess (x_process);		/* generate "during" process */
				/* release outputting process */
				texpopd_main (channelmode, channel, MANY_REGS, FALSE);
				if (indirect) {
					/* load real channel address */
					genindirect (INDIR_AREG);
				}
				tioop (I_XEND, usecall);
			}
		} else {
			if (indirect) {
				/* load real channel address */
				genindirect (INDIR_BREG);
			}
			tioop (isplaced ? I_EXTIN : I_IN, usecall);
		}
		/*{{{  if the input is the tag for a MOBILE.CHAN in a CASE input, better de-reference it*/
		if (kroc_chantype_desc && (ActionFlagsOf (comm) & ActionFlag_anychantypehash)) {
#if 0
fprintf (stderr, "tinputitem: inputting CASE tag for a MOBILE.CHAN.  inputitemmode = %d, inputitem = ", inputitemmode);
printtreenl (stderr, 4, inputitem);
#endif
			/* always a temporary */
			gencomment0 ("dereference for channel-type hash from type description");
			// genprimary (I_LDNL, 2);
			#if 0
			loadname (inputitem, 0);
			gensecondary (I_DUP);
			loadconstant (1 << WSH);
			gensecondary (I_DIFF);
			genprimary (I_LDNL, 0);
			genshiftimmediate (I_SHR, MT_TYPE_SHIFT + MT_FLAGS_SHIFT + 3);
			gensecondary (I_WSUB);
			genprimary (I_LDNL, 1);
			storeinname (inputitem, 0);
			#endif
		}
		/*}}}*/
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void toutput(tptr)*/
/*****************************************************************************
 *
 *  toutput generates code for an OUTPUT node
 *
 *****************************************************************************/
PUBLIC void toutput (treenode * tptr)
{
	treenode *channel = LHSOf (tptr);
	treenode *outputlist = RHSOf (tptr);
	treenode *channeltype = gettype (channel);
	DEBUG_MSG (("toutput\n"));
	if (TagOf (channeltype) != S_CHAN) {
		/*{{{  handle PORT output */
		treenode **oldtypeaddr;	/* INSdi03067 */
		treenode *porttype;
		tpreexp (channel);
		tpreexp (ThisItem (outputlist));
		porttype = fixporttype (channel, &oldtypeaddr);
		tsimpleassign (ntypeof (channel), P_EXP, channel, P_EXP, ThisItem (outputlist), MANY_REGS);
		restoreporttype (channel, porttype, oldtypeaddr);
		/*}}} */
	} else if (ActionTypeOf (tptr) != NULL) {
		/* bug TS/1626 23/04/92 */
		/*{{{  all the work was done in trans */
		int channelmode = chanaspointer ? P_EXP : P_PTR;
#if 0
fprintf (stderr, "toutput (chanaspointer = %d, channelmode = %d): generating code for:", (int)chanaspointer, channelmode);
printtreenl (stderr, 4, tptr);
fprintf (stderr, "toutput: channel type is:");
printtreenl (stderr, 4, channeltype);
#endif
		tpreexp (channel);
		channelmode = simplify (channelmode, channel);
#if 0
fprintf (stderr, "toutput: channelmode = %d after simplify ()\n", channelmode);
#endif
		tpreexp (ThisItem (outputlist));
		toutputitem (channelmode, channel, P_EXP, ThisItem (outputlist), ActionTypeOf (tptr), tptr);
		/*}}} */
	} else {
		/*{{{  handle CHAN output */
		int channelmode = chanaspointer ? P_EXP : P_PTR;
		treenode *protocol = ProtocolOf (channeltype);
		BOOL outputtag = FALSE;	/* True when we are generating code to output a tag */

		/*{{{  find the protocol type */
		if (TagOf (protocol) == N_SPROTDEF) {
			protocol = NTypeOf (protocol);
		} else if (TagOf (protocol) == N_TPROTDEF) {
			/* Pick out the type tree of the tag node */
			protocol = NTypeOf (CExpOf (ThisItem (outputlist)));
			outputtag = TRUE;
		}
		/*}}} */

		tpreexp (channel);
		channelmode = simplify (channelmode, channel);
		while (!EndOfList (outputlist)) {
			/*{{{  output item of list, move to next item */
			treenode *prottype = ((protocol != NULL) && (TagOf (protocol) == S_LIST))
				? ThisItem (protocol) : protocol;
			tpreexp (ThisItem (outputlist));
			toutputitem (channelmode, channel, P_EXP, ThisItem (outputlist), prottype, tptr);
			if ((protocol != NULL) && (TagOf (protocol) == S_LIST) && !outputtag)
				protocol = NextItem (protocol);
			outputtag = FALSE;
			outputlist = NextItem (outputlist);
			/*}}} */
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void tinput(tptr)*/
/*****************************************************************************
 *
 *  tinput generates code for an INPUT node
 *
 *****************************************************************************/
PUBLIC void tinput (treenode * tptr)
{
	treenode *channel = LHSOf (tptr), *inputlist = RHSOf (tptr);
	const BOOL is_extended = ((TagOf (tptr) == S_X_INPUT) || (TagOf (tptr) == S_X_CASE_INPUT)
				 || (TagOf (tptr) == S_X_TAGGED_INPUT));

	DEBUG_MSG (("tinput\n"));
#if 0
fprintf (stderr, "tinput: tptr =");
printtreenl (stderr, 4, tptr);
#endif
	BETRACE ("gen10: tinput (enter): tptr=%p", tptr);
	switch (inputtypeof (tptr)) {
		/*{{{  channel input */
	case INP_X_INPUT:
		if (!inputlist) {
			/* _must_ be S_X_SECOND_HALF (for empty tags) */
			int channelmode = chanaspointer ? P_EXP : P_PTR;
			BOOL usecall;
			treenode *d_proc = ActionDuringOf (tptr);

			tpreexp (channel);
			channelmode = simplify (channelmode, channel);
			if (TagOf (d_proc) != S_X_SECOND_HALF) {
				badtag (LocnOf (d_proc), TagOf (d_proc), "tinput");
			}
			d_proc = OpOf (d_proc);
			tprocess (d_proc);
			texpopd_main (channelmode, channel, MANY_REGS, FALSE);
			usecall = iobycall && maybevirtualchan (channel);
			tioop (I_XEND, usecall);
			tprocess (ActionAfterOf (tptr));
			BETRACE ("gen10: tinput (leave)");
			return;
		}
		/* fall through */
	case INP_INPUT:
		if (ActionTypeOf (tptr) != NULL) {	/* bug TS/1626 23/04/92 */
			int channelmode = chanaspointer ? P_EXP : P_PTR;
			/* const BOOL indirect = is_indirect_name (channel); */
			tpreexp (channel);

			channelmode = simplify (channelmode, channel);
			tpreexp (ThisItem (inputlist));
			tinputitem (channelmode, channel, P_PTR, ThisItem (inputlist), ActionTypeOf (tptr), tptr,
				is_extended ? (ActionDuringOf (tptr)) : NULL);
		} else {
			int channelmode = chanaspointer ? P_EXP : P_PTR;
			treenode *protocol = ProtocolOf (gettype (channel));
			if (TagOf (protocol) == N_SPROTDEF)
				protocol = NTypeOf (protocol);
			tpreexp (channel);
			channelmode = simplify (channelmode, channel);
			while (!EndOfList (inputlist)) {
				/*{{{  input item of list, move to next item */
				treenode *prottype = (TagOf (protocol) == S_LIST) ? ThisItem (protocol) : protocol;

				tpreexp (ThisItem (inputlist));
				tinputitem (channelmode, channel, P_PTR, ThisItem (inputlist), prottype, tptr,
					(is_extended && IsLastItem(inputlist)) ? ActionDuringOf (tptr) : NULL);
				if (TagOf (protocol) == S_LIST) {
					protocol = NextItem (protocol);
				}
				inputlist = NextItem (inputlist);
				/*}}} */
			}
		}
		if (is_extended) {
			tprocess (ActionAfterOf (tptr));
		}
		break;
		/*}}} */
		/*{{{  timer input */
	case INP_TIMER_INPUT:
		{
			treenode *dest = ThisItem (inputlist);
			tpreexp (dest);
			tpreexp (channel);	/* Bug 288 22/5/90 */
			/*texp(channel, MANY_REGS); *//* Bug 288 22/5/90 */
			if (regsfor (dest) < MAXREGS) {
				/*{{{  ldtimer; st dest */
				gensecondary (I_LDTIMER);
				storeinelement (dest, 0, MAXREGS - 1);
				/*}}} */
			} else {
				/*{{{  make pointer to dest, generate source, store through pointer */
				BIT32 offset = loadelementpointeroffset (dest, MANY_REGS);
				gensecondary (I_LDTIMER);
				gensecondary (I_REV);
				genprimary (I_STNL, offset);
				/*}}} */
			}
		}
		break;
		/*}}} */
		/*{{{  delayed input */
	case INP_DELAYED_INPUT:
		tpreexp (inputlist);
		tpreexp (channel);	/* Bug 288 22/5/90 */
#if 0
		/*texp(channel, MANY_REGS); *//* Bug 288 22/5/90 */
		if (T9000_alpha_badrunp (&tx_global)) {
			/*{{{  T9000_alpha_badrunp */
			int mode = P_EXP;
			mode = simplify (mode, inputlist);
			gensecondary (I_TALT);
			gencomment0 ("Can't use tin because of Scheduler bug");
			texp (inputlist, MANY_REGS);
			genprimary (I_LDC, 1);
			/*{{{  T9000_gamma_carryin */
			if (T9000_gamma_carryin (&tx_global)) {
				genprimary (I_LDC, 0);
				gensecondary (I_SUB);
				gencomment0 ("Set carry flag for ALU Adder CarryIn bug");
			}
			/*}}} */
			gensecondary (I_ENBT);
			/*{{{  T9000_gamma_carryin */
			if (T9000_gamma_carryin (&tx_global)) {
				genprimary (I_LDC, 0);
				gensecondary (I_SUB);
				gencomment0 ("Set carry flag for ALU Adder CarryIn bug");
			}
			/*}}} */
			gensecondary (I_TALTWT);
			/*}}} */
		} else
#endif	/* 0 - T9000 stuff */
		{
			texp (inputlist, MANY_REGS);
			gensecondary (I_TIN);
		}
		break;
		/*}}} */
		/*{{{  port input */
	case INP_PORT_INPUT:
		tpreexp (channel);
		{
			treenode **oldtypeaddr;	/* INSdi03067 */
			treenode *porttype = fixporttype (channel, &oldtypeaddr);
			tpreexp (ThisItem (inputlist));
			tsimpleassign (ntypeof (channel), P_EXP, ThisItem (inputlist), P_EXP, channel, MANY_REGS);
			restoreporttype (channel, porttype, oldtypeaddr);
		}
		break;
		/*}}} */
	default:
		badtag (LocnOf (tptr), TagOf (tptr), "tinput");
	}
	BETRACE ("gen10: tinput (leave)");
}

/*}}}*/
/*{{{  PUBLIC void talt(tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  talt generates code for a complete ALT construct
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void talt (treenode * tptr)
{
	const int joinlab = newlab ();
	const int altendlab = newlab ();
	const int timedalt = timerguardinalt (tptr);
	const INT32 savedreplaltvaluesbase = replaltvaluesbase;
	const int savedreplaltstacklen = replaltstacklen;
	treenode **const savedreplaltstack = replaltstack;
	treenode *const savedrepls = (nodetypeoftag (TagOf (tptr)) == CNODE) ? CTempOf (tptr) : ReplCTempOf (tptr);
	/* This will be the number of nested levels of replicators: */
	const int repllevels = (int) allocsize (savedrepls);	/* bug 1254 8/5/91 */

	DEBUG_MSG (("talt: mallocing size %d replaltstack\n", repllevels));
	/* add 1 word to malloc-ed size to prevent problems with zero byte malloc */
	replaltstack = (treenode **) memalloc (sizeof (treenode *) * (repllevels + 1));
	replaltstacklen = 0;
	/*replaltvaluesbase = NVOffsetOf(savedrepls); */
	replaltvaluesbase = NVOffsetOf (savedrepls) + nameoffsetof (NLexLevelOf (savedrepls));

	nalternatives = numberofalternatives (tptr);
	if (nalternatives != 0) {
		/*{{{  generate the alternatives */
		/* the following are only used while enabling, so can't be nested */
		alternativecount = 0;
		inreplalt = FALSE;
		altcheckneeded = (NEED_ERRORS && ((errormode & ERRORMODE_STOP) == 0)	/* INSdi02265 */
				  &&!hastruealtguard (tptr));
		altcheckdone = FALSE;

		if (alt_preenable && enhanced_alt_enable) {
			if (timedalt) {
				/* load timer and store in Time field (-5 normally, -6 with priority) */
				gensecondary (I_LDTIMER);
				genprimary (I_STL, W_TIME_SLOT);
			}
			gencomment0 ("BEGIN PRE-ENABLING");
			/* generate pre-enablings for enhanced ALT pre-enabling */
			taltpreenables (tptr, NULL, 0);
			gencomment0 ("END PRE-ENABLING");
		}

		gensecondary (timedalt ? I_TALT : I_ALT);
		insidealtguard = TRUE;
		alt_ds = timedalt ? DS_WAIT : DS_IO;
		/*{{{  check if we need to initialise degenerate alt trap */
		if (altcheckneeded) {
			BOOL inittemp = TRUE;
			treenode *alt = tptr;
			while (alt != NULL && TagOf (alt) != S_ALTERNATIVE) {
				/*{{{  walk down to the first alternative */
				alt = skipspecifications (alt);
				switch (TagOf (alt)) {
				case S_ALT:
				case S_PRIALT:
					alt = CBodyOf (alt);
					break;
				case S_LIST:
					alt = ThisItem (alt);
					break;
				case S_REPLALT:
				case S_PRIREPLALT:
					alt = NULL;
					break;
				case S_ALTERNATIVE:
					break;
				}
				/*}}} */
			}

			if (alt != NULL && TagOf (alt) == S_ALTERNATIVE) {
				/*{{{  if (don't need to init. temp) inittemp = FALSE */
				treenode *const guard = AltGuardOf (alt);
				if (AltChanExpOf (alt) == NULL) {
					inittemp = FALSE;
				} else if (AltTimeExpOf (alt) == NULL) {
					if (istrueguard (guard) || !cancauseerror (AltChanExpOf (alt))) {
						inittemp = FALSE;
					}
				} else {
					if (istrueguard (guard) || !cancauseerror (AltTimeExpOf (alt))) {
						inittemp = FALSE;
					}
				}
				/*}}} */
			}

			if (inittemp) {
				/*genprimary(I_LDC, 1); */
				genprimary (I_LDC, 0);	/* INSdi02266 22/06/93 */
				genprimary (I_STL, 0);
				gencomment0 ("INT");
			}
		}
		/*}}} */

		taltenables (tptr, NULL, 0);
		/*{{{  check for degenerate ALT (if not done already) */
		if (altcheckneeded && !altcheckdone) {
			genprimary (I_LDL, 0);
			genprimary (I_LDC, 1);
			gensecondary (I_CCNT1);
		}
		/*}}} */
		if (alternativecount == 0) {
			/* all replicators must have had zero counts */
			/*{{{  stop */
			/* removed source_output 14/3/91 */
			/*if (source_output)
			   so_stop(); */
			tstop (tptr);
			/*}}} */
		} else {
#if 0
			/*{{{  T9000_gamma_carryin */
			if (timedalt && T9000_gamma_carryin (&tx_global)) {
				genprimary (I_LDC, 0);
				gensecondary (I_SUB);
				gencomment0 ("Set carry flag for ALU Adder CarryIn bug");
			}
			/*}}} */
#endif
			gensecondary (timedalt ? I_TALTWT : I_ALTWT);
		}
		disable_csub0 = TRUE;
		/*{{{  if we need any code to fixup enb3 instructions, do it here*/
		if (enhanced_alt_enable) {
			int i, tcount;

			for (i=0, tcount=0; i<eae_count; i++) {
				if (eae_replptrs[i]) {
					tcount++;
				}
			}
			if (tcount) {
				int ls_lab = newlab ();

				genbranch (I_J, ls_lab);	/* skip over the code we're about to generate */
				tprealtfixups ();		/* fixup code for pre-enabling sequence */

				for (i=0; i<eae_count; i++) {
					if (eae_replptrs[i]) {
						treenode *repl = eae_replptrs[i];
						treenode *replname = ReplCNameOf (repl);
						const int level = NLexLevelOf (replname);
						const INT32 wsposn = NVOffsetOf (replname) + nameoffsetof (level);
						treenode *step = ReplCStepExpOf (repl);

						setlab (eae_labels[i]);
						eae_labels[i] = newlab ();	/* make a new label for the disabler */
						texp (ReplCLengthExpOf (repl), MANY_REGS);
						genprimary (I_LDL, wsposn + REPL_COUNT);		/* load current count value */
						gensecondary (I_DIFF);
						genprimary (I_LDC, 1);
						gensecondary (I_SUM);
						genprimary (I_STL, wsposn + REPL_COUNT);		/* store modified count value */
						/* maybe need to invert STEP, if using loopend3 */
						if (step) {
							int do_invert = 0;

							if (isconstexpnd (step) && (LoValOf (step) != 1) && (LoValOf (step) != -1)) {
								do_invert = 1;
							} else if (!isconstexpnd (step)) {
								do_invert = 1;
							}

							if (do_invert) {
								genprimary (I_LDL, wsposn + REPL_STEP);
								gensecondary (I_NOT);
								genprimary (I_ADC, 1);
								genprimary (I_STL, wsposn + REPL_STEP);
							}
						}
						genbranch (I_J, eae_labels[i]);	/* branch to disabler */
					}
				}
				setlab (ls_lab);
			} else {
				int ls_lab = newlab ();

				genbranch (I_J, ls_lab);
				tprealtfixups ();
				setlab (ls_lab);
			}
		}
		/*}}}*/
		/* already walked the alternative tree once at this point */
		taltdisables (tptr, NULL, altendlab);
		disable_csub0 = FALSE;
		insidealtguard = FALSE;
		/* if (alternativecount != 0)
		   gensecondary(I_ALTEND);
		   setlab(altendlab);
		 */
		genaltend (alternativecount, altendlab);
		/*{{{  generate all the bodies */
		{
			treenode **altbodystack;
			int nestedalts = countnestedalts (tptr, NESTED_ALTS);
			DEBUG_MSG (("talt: mallocing size %d altbodystack\n", nestedalts));
			altbodystack = (treenode **) memalloc (sizeof (treenode *) * nestedalts);
			taltbodies (tptr, joinlab, altbodystack, 0);	/* Generate all the bodies */
			DEBUG_MSG (("talt: freeing size %d altbodystack\n", nestedalts));
			memfree (altbodystack);
		}
		/*}}} */
		setlab (joinlab);
		genstartblock ();
		/*}}} */
	} else {
		tstop (tptr);
	}
	DEBUG_MSG (("talt: freeing size %d replaltstack\n", repllevels));
	memfree (replaltstack);

	replaltvaluesbase = savedreplaltvaluesbase;
	replaltstacklen = savedreplaltstacklen;
	replaltstack = savedreplaltstack;
}

/*}}}*/

#if defined(PD_DECODE_CHANNEL) && defined(PD_DECODE_CHANNEL3) && defined(PD_ENCODE_CHANNEL)
/*{{{  PUBLIC void mapxinputoutput (treenode *tptr)*/
/*
 *	maps out extended input/output
 *	used for ENCODE.CHANNEL and DECODE.CHANNEL
 */
PUBLIC void mapxinputoutput (treenode *tptr)
{
	int channelmode = chanaspointer ? P_EXP : P_PTR;
	treenode **lchannel = LHSAddr (tptr);
	const BOOL lusecall = iobycall && maybevirtualchan (*lchannel);
	const BOOL is_encode = (ActionFlagsOf (tptr) & ActionFlag_encode);
	const BOOL is_count = (ActionFlagsOf (tptr) & ActionFlag_count);
	const BOOL is_case = (ActionFlagsOf (tptr) & ActionFlag_case);
	const BOOL is_mobile = (ActionFlagsOf (tptr) & ActionFlag_mobile);
	const BOOL is_mobproc = (ActionFlagsOf (tptr) & ActionFlag_mobproc);
	const BOOL is_dynmob = (ActionFlagsOf (tptr) & ActionFlag_dynmob);
	const BOOL is_ed3seq = (ActionFlagsOf (tptr) & ActionFlag_ed3seq);

#if 0
fprintf (stderr, "gen10: mapxinputoutput: is_encode=%d, is_count=%d, is_case=%d, is_mobile=%d, is_dynmob=%d, is_ed3seq=%d, tptr=",
		is_encode, is_count, is_case, is_mobile, is_dynmob, is_ed3seq);
printtreenl (stderr, 4, tptr);
#endif
	if (!RHSOf (tptr)) {
		/* terminating bit */
		mapexpopd (channelmode, lchannel);
		mapioop (I_XEND, lusecall);
	} else {
		/* RHS is a list of 3 things -- the output channel, then some temporaries, depending on the operation.. */
		treenode **rchannel = ThisItemAddr (RHSOf (tptr));
		const BOOL rusecall = iobycall && maybevirtualchan (*rchannel);
		int loadseq = -1;

		if (is_encode) {
			/*{{{  map out ENCODE.CHANNEL operation */
			/* order is (extended) communicate on lchannel, into ed.addr/ed.count, communicate on rchannel, release lchannel */
			/* FIXME: only INT; INT supported at the moment */
			/* treenode **cnum = ThisItemAddr (NextItem (RHSOf (tptr))); */
			treenode **ed_addr = ThisItemAddr (NextItem (NextItem (RHSOf (tptr))));
			treenode **ed_count = ThisItemAddr (NextItem (NextItem (NextItem (RHSOf (tptr)))));
			treenode **ed_ccnt = ThisItemAddr (NextItem (NextItem (NextItem (NextItem (RHSOf (tptr))))));

			/* map two INT inputs -- first not extended */
			loadseq = mapload2regs (P_PTR, ed_addr, channelmode, lchannel);
			mapioop (I_IN, lusecall);
			if (is_ed3seq) {
				loadseq = mapload2regs (P_PTR, ed_count, channelmode, lchannel);
				mapioop (I_IN, lusecall);
				mapexpopd (channelmode, lchannel);
				mapioop (I_XABLE, lusecall);
				loadseq = mapload2regs (P_PTR, ed_ccnt, channelmode, lchannel);
				mapioop (I_XIN, lusecall);
			} else {
				mapexpopd (channelmode, lchannel);
				mapioop (I_XABLE, lusecall);
				loadseq = mapload2regs (P_PTR, ed_count, channelmode, lchannel);
				mapioop (I_XIN, lusecall);
			}

			/* then dependant stuff */
			if (!is_count && is_case) {
				/*{{{  this is the tag value in a CASE protocol */
				treenode **tval = ThisItemAddr (NextItem (NextItem (NextItem (NextItem (NextItem (RHSOf (tptr)))))));

				mapexpopd (P_EXP, ed_addr);
				mapstoreinopd (P_EXP, tval);

				/* then output it (tag$) -- trivial */
				loadseq = mapload2regs (channelmode, rchannel, P_EXP, ed_addr);
				mapioop (I_OUTBYTE, rusecall);
				/*}}}*/
			} else if (!is_count && is_mobile) {
				/*{{{   static MOBILE encode*/
				treenode **mtemp = ThisItemAddr (NextItem (NextItem (NextItem (NextItem (NextItem (RHSOf (tptr)))))));

				loadseq = mapload3regs (P_EXP, ed_addr, P_PTR, mtemp, P_EXP, ed_count);

				/* map copy and mobile output */
				loadseq = mapload2regs (P_PTR, ed_addr, P_PTR, mtemp);
				loadseq = mapload2regs (P_PTR, mtemp, channelmode, rchannel);
				mapioop (I_MT_OUT, rusecall);
				genwarn (GEN_WARN_BADCODE);
				/*}}}*/
			} else if (!is_count && is_dynmob) {
				/*{{{  dynamic MOBILE encode*/
				treenode **mtemp = ThisItemAddr (NextItem (NextItem (NextItem (NextItem (NextItem (RHSOf (tptr)))))));

				/* map dynamic mobile output */
				loadseq = mapload2regs (P_PTR, mtemp, channelmode, rchannel);
				mapioop (I_MT_OUT, rusecall);
				genwarn (GEN_WARN_BADCODE);
				/*}}}*/
			} else if (!is_count) {
				/*{{{  map output (should save loadseq somewhere really..) */
				loadseq = mapload3regs (P_EXP, ed_addr, channelmode, rchannel, P_EXP, ed_count);
				mapioop (I_OUT, rusecall);
				mapexpopd (P_EXP, ed_addr);
				mapexpopd (P_EXP, ActionTypeAddr (tptr));
				/*}}}*/
			}

			/* release lchannel */
			mapexpopd (channelmode, lchannel);
			mapioop (I_XEND, lusecall);
			/*}}}*/
		} else {
			/*{{{  map out DECODE.CHANNEL operation*/
			/* order is wait for lchannel, store in ed.addr/ed.count, communicate on rchannel, release lchannel */
			mapexpopd (channelmode, lchannel);
			mapioop (I_XABLE, lusecall);

			if (is_count) {
				/*{{{  counted array input -- count part list is [out,ed_addr,ed_count,cval] */
				/* treenode **cnum = ThisItemAddr (NextItem (RHSOf (tptr))); */
				treenode **cval = ThisItemAddr (NextItem (NextItem (RHSOf (tptr))));

				/* decode needs to do XIN into cval */
				loadseq = mapload2regs (P_PTR, cval, channelmode, lchannel);
				mapioop (I_XIN, lusecall);

				/* then communicates on the right channel -- size is constant */
				loadseq = mapload2regs (channelmode, rchannel, channelmode, lchannel);
				mapioop (I_OUTWORD, rusecall);
				mapexpopd (channelmode, rchannel);
				mapioop (I_OUTWORD, rusecall);
				if (is_ed3seq) {
					mapexpopd (channelmode, rchannel);
					mapioop (I_OUTWORD, rusecall);
				}
				/*}}}*/
			} else if (is_case) {
				/*{{{  this is the tag value in a CASE protocol */
				treenode **cnum = ThisItemAddr (NextItem (RHSOf (tptr)));
				treenode **tval = ThisItemAddr (NextItem (NextItem (RHSOf (tptr))));

				loadseq = mapload2regs (P_PTR, tval, channelmode, lchannel);
				mapioop (I_XIN, lusecall);

				/* map comms on right channel -- size is always known */
				loadseq = mapload2regs (channelmode, rchannel, channelmode, lchannel /* this gets fiddled */);
				mapioop (I_OUTWORD, rusecall);
				mapexpopd (channelmode, rchannel);	/* value to output (size) is constant 1 here */
				mapioop (I_OUTWORD, rusecall);
				if (is_ed3seq) {
					loadseq = mapload2regs (channelmode, rchannel, P_EXP, cnum);
					mapioop (I_OUTWORD, rusecall);
				}
				/*}}}*/
			} else {
				/*{{{  anything else (including MOBILEs) */
				/* treenode **cnum = ThisItemAddr (NextItem (RHSOf (tptr))); */
				treenode **ed_addr = ThisItemAddr (NextItem (NextItem (RHSOf (tptr))));
				treenode **ed_count = ThisItemAddr (NextItem (NextItem (NextItem (RHSOf (tptr)))));
				/* treenode **ed_ccnt = ThisItemAddr (NextItem (NextItem (NextItem (NextItem (RHSOf (tptr)))))); */

				/* map loads and stores in temporaries */
				mapexpopd (P_EXP, ed_addr);
				mapstoreinopd (P_EXP, ed_addr);
#if 0
fprintf (stderr, "mapxinputoutput: *ed_addr = ");
printtreenl (stderr, 4, *ed_addr);
fprintf (stderr, "mapxinputoutput: *ed_count = ");
printtreenl (stderr, 4, *ed_count);
fprintf (stderr, "mapxinputoutput: is_dynmob = %d, is_mobproc = %d, is_mobile = %d, ActionTypeOf(tptr) = ", is_dynmob, is_mobproc, is_mobile);
printtreenl (stderr, 4, ActionTypeOf (tptr));
#endif

				if (!is_mobproc) {
					if (is_dynmob) {
						/* must only map one half of this! -- base size (RHS) */
						mapexpopd (P_EXP, RightOpAddr (ActionTypeOf (tptr)));
					} else {
						mapexpopd (P_EXP, ActionTypeAddr (tptr));
					}
				} else {
					/* map a store in ed_count */
					mapstoreinopd (P_EXP, ed_count);
					/* this needs to load 3 things for the conversion */
					loadseq = mapload3regs (channelmode, lchannel, P_PTR, ed_addr, P_PTR, ed_count);
				}
				if (!is_mobile) {
					mapstoreinopd (P_EXP, ed_count);
				}
				
				/* map comms on right channel -- size is always known */
				loadseq = mapload2regs (channelmode, rchannel, P_EXP, ed_addr);
				mapioop (I_OUTWORD, rusecall);
				loadseq = mapload2regs (channelmode, rchannel, P_EXP, ed_count);
				mapioop (I_OUTWORD, rusecall);
				if (is_ed3seq) {
					mapexpopd (channelmode, rchannel);
					mapioop (I_OUTWORD, rusecall);
				}
				/*}}}*/
			}

			/* map release left channel */
			mapexpopd (channelmode, lchannel);
			mapioop (I_XEND, lusecall);
			/*}}}*/
		}
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void txinputoutput (treenode *tptr)*/
/*
 *	generates code for extended input/output
 *	used for ENCODE.CHANNEL and DECODE.CHANNEL
 */
PUBLIC void txinputoutput (treenode *tptr)
{
	int channelmode = chanaspointer ? P_EXP : P_PTR;
	treenode *channel = LHSOf (tptr);
	const BOOL usecall = iobycall && maybevirtualchan (channel);
	const BOOL is_encode = (ActionFlagsOf (tptr) & ActionFlag_encode);
	const BOOL is_count = (ActionFlagsOf (tptr) & ActionFlag_count);
	const BOOL is_case = (ActionFlagsOf (tptr) & ActionFlag_case);
	const BOOL is_ed3seq = (ActionFlagsOf (tptr) & ActionFlag_ed3seq);

	if (!RHSOf (tptr)) {
		texpopd_main (channelmode, channel, MANY_REGS, FALSE);
		tioop (I_XEND, usecall);
	} else {
		treenode *outchan = ThisItem (RHSOf (tptr));
		treenode *oseq = ThisItem (NextItem (RHSOf (tptr)));
		const BOOL outusecall = iobycall && maybevirtualchan (outchan);
		int loadseq = -1;
		BOOL preeval_e2, preeval_e3;
		const BOOL is_mobile = (ActionFlagsOf (tptr) & ActionFlag_mobile);
		const BOOL is_dynmob = (ActionFlagsOf (tptr) & ActionFlag_dynmob);
		const BOOL is_mobproc = (ActionFlagsOf (tptr) & ActionFlag_mobproc);
		const BOOL precount = (ActionFlagsOf (tptr) & ActionFlag_precount);

		if (is_encode) {
			/*{{{  generate code for ENCODE.CHANNEL operation*/
			/* normal first input for address -- FIXME: only INT; INT supported at the moment */
			/* treenode *cnum = ThisItem (NextItem (RHSOf (tptr))); */
			treenode *ed_addr = ThisItem (NextItem (NextItem (RHSOf (tptr))));
			treenode *ed_count = ThisItem (NextItem (NextItem (NextItem (RHSOf (tptr)))));
			treenode *ed_ccnt = ThisItem (NextItem (NextItem (NextItem (NextItem (RHSOf (tptr))))));

			if (!is_count) {
				/*{{{  input address then extended input count, into ed_addr and ed_count (and ed_ccnt if needed) */
				tload2regs (P_PTR, ed_addr, channelmode, channel, FALSE, FALSE);
				loadconstant (bytesperword);
				tioop (I_IN, usecall);
				/* FIXME: handle is_ed3seq */
				if (is_ed3seq) {
					tload2regs (P_PTR, ed_count, channelmode, channel, FALSE, FALSE);
					loadconstant (bytesperword);
					tioop (I_IN, usecall);
					/* wait for outputting process (ed_ccnt) */
					texpopd_main (channelmode, channel, MANY_REGS, FALSE);
					tioop (I_XABLE, usecall);
					tload2regs (P_PTR, ed_ccnt, channelmode, channel, FALSE, FALSE);
					loadconstant (bytesperword);
					tioop (I_XIN, usecall);
				} else {
					/* wait for outputting process (ed_count) */
					texpopd_main (channelmode, channel, MANY_REGS, FALSE);
					tioop (I_XABLE, usecall);
					tload2regs (P_PTR, ed_count, channelmode, channel, FALSE, FALSE);
					loadconstant (bytesperword);
					tioop (I_XIN, usecall);
				}
				/*}}}*/
			}

			if (!is_count && is_case) {
				/*{{{  CASE for a variant protocol input*/
				treenode *tvar = ThisItem (NextItem (NextItem (NextItem (NextItem (NextItem (RHSOf (tptr)))))));

				/* now dereference ed_addr and get the tag value */
				texpopd_main (P_EXP, ed_addr, MANY_REGS, FALSE);
				gensecondary (I_LB);
				texpopd_main (P_PTR, tvar, MAXREGS - 1, FALSE);
				gensecondary (I_SB);

				/* output it */
				tload2regs (channelmode, outchan, P_EXP, tvar, FALSE, FALSE);
				tioop (I_OUTBYTE, outusecall);

				/* we're guarenteed that real_addr points at a dynamic mobile array -- free it. */
				texpopd_main (P_EXP, ed_addr, MANY_REGS, FALSE);
				gensecondary (I_MRELEASE);
				/*}}}*/
#ifdef MOBILES
			} else if (!is_count && is_mobile) {
				/*{{{  handle static mobile encode (copy into mtemp)*/
				treenode *mtemp = ThisItem (NextItem (NextItem (NextItem (NextItem (NextItem (RHSOf (tptr)))))));

				/* copy data into mtemp if present and do real mobile output */
#if 0
fprintf (stderr, "gen10: coding for ENCODE.CHANNEL.  (static) mtemp = ");
printtreenl (stderr, 4, mtemp);
#endif
				/* assign to temporary MOBILE */
				texpopd_main (P_EXP, ed_addr, MANY_REGS, FALSE);
				loadmobile (mtemp);
				texpopd_main (P_EXP, ed_count, 1, FALSE);
				gensecondary (I_MOVE);

				/* output that temporary */
				loadmobilepointer (mtemp);
				texpopd_main (channelmode, outchan, 2, FALSE);
				checkerror ();
				tioop (I_MT_OUT, usecall);
				genwarn (GEN_WARN_BADCODE);

				/* we're guarenteed that ed_addr points at a dynamic mobile array -- free it. */
				texpopd_main (P_EXP, ed_addr, MANY_REGS, FALSE);
				gensecondary (I_MRELEASE);
				/*}}}*/
			} else if (!is_count && is_dynmob) {
				/*{{{  handle dynamic mobile encode*/
				treenode *dimcount = LeftOpOf (ActionTypeOf (tptr));
				treenode *basesize = RightOpOf (ActionTypeOf (tptr));
				treenode *mtemp = ThisItem (NextItem (NextItem (NextItem (NextItem (NextItem (RHSOf (tptr)))))));
				int dimconst = LoValOf (dimcount);
				int i;

#if 0
fprintf (stderr, "gen10: coding for ENCODE.CHANNEL.  (dynamic) mtemp = ");
printtreenl (stderr, 4, mtemp);
#endif
				if (dimconst > 1) {
					/* resume outputting process (it's gonna send something straight away anyhow */
					texpopd_main (channelmode, channel, MANY_REGS, FALSE);
					tioop (I_XEND, usecall);

					/* populate magic dimensions of mtemp */
					for (i=0; i<dimconst; i++) {
						loadname (ed_addr, 0);		/* address where dimensions, in order, live */
						genprimary (I_LDNL, i);
						storemobilesize (mtemp, i+1);	/* first dimension for these is 1 */
					}

					/* oki, discard this chunk of memory */
					texpopd_main (P_EXP, ed_addr, MANY_REGS, FALSE);
					gensecondary (I_MRELEASE);

					/* read next address -- the real data */
					tload2regs (P_PTR, ed_addr, channelmode, channel, FALSE, FALSE);
					loadconstant (bytesperword);
					tioop (I_IN, usecall);

					/* then extended input count -- irrelevant to us, but heyho.. */
					texpopd_main (channelmode, channel, MANY_REGS, FALSE);
					tioop (I_XABLE, usecall);

					/* extended input count -- FIXME: only INT; INT supported at the moment */
					tload2regs (P_PTR, ed_count, channelmode, channel, FALSE, FALSE);
					loadconstant (bytesperword);
					tioop (I_XIN, usecall);

					/* ignore count, just stick the address in mtemp's pointer field (dimension 0.. :|) */
					loadname (ed_addr, 0);
					storemobilesize (mtemp, 0);
				} else {
					/* single dimension.  ed_count / basetypesize will give the 1st dimension size */
					loadname (ed_addr, 0);
					storemobilesize (mtemp, 0);

					/* do count division and put in size field */
					loadname (ed_count, 0);
					texpopd_main (P_EXP, basesize, MANY_REGS, FALSE);
					gensecondary (I_DIV);
					storemobilesize (mtemp, 1);
				}

				/* then dynamic mobile output */
				loadmobilepointer (mtemp);
				texpopd_main (channelmode, outchan, 2, FALSE);
				checkerror ();
				tioop (I_MT_OUT, usecall);
				/*}}}*/
#endif
			} else if (!is_count) {
				/*{{{  regular encode*/
				/* right: if (precount), didn't deal with the count part already, so do it here */
				if (precount) {
					treenode *cvar = LHSOf (ActionTypeOf (tptr));		/* always a TIMES, with $cvar on LHS */
					treenode *atype = RHSOf (ActionTypeOf (tptr));		/* and base-type CONSTEXP on the RHS */
					int bytes = -1;

					if (TagOf (atype) == S_CONSTEXP) {
						bytes = LoValOf (atype);
					} else {
						geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "txinputoutput");
					}

					/* load BYTE count of data.  bytes is the base-type size */
					texpopd_main (P_EXP, ed_count, MANY_REGS, FALSE);
					if (bytes == 1) {
						/* do nothing! */
					} else if (bytes == 4) {
						genshiftimmediate (I_SHR, 2);
					} else {
						loadconstant (bytes);
						gensecondary (I_DIV);
					}
					/* store in cvar */
					texpopd_main (P_PTR, cvar, MAXREGS - 1, FALSE);
					genprimary (I_STNL, 0);


#if 0
fprintf (stderr, "txinputoutput: precount.  ActionTypeOf (tptr) =");
printtreenl (stderr, 4, ActionTypeOf (tptr));
fprintf (stderr, "txinputoutput: precount.  cvar =");
printtreenl (stderr, 4, cvar);
#endif
					/* and output it */
					switch (bytesin (NTypeOf (cvar))) {
					case 1:
						tload2regs (channelmode, outchan, P_EXP, cvar, FALSE, FALSE);
						tioop (I_OUTBYTE, outusecall);
						break;
					case 4:
						tload2regs (channelmode, outchan, P_EXP, cvar, FALSE, FALSE);
						tioop (I_OUTWORD, outusecall);
						break;
					default:
						geninternal_is (GEN_ERROR_IN_ROUTINE, 2, "txinputoutput");
						break;
					}

				}
				/* do the "real" output */
				loadseq = giveorder (P_EXP, ed_addr, channelmode, outchan, P_EXP, ed_count, &preeval_e2, &preeval_e3);
				tload3regs (P_EXP, ed_addr, channelmode, outchan, P_EXP, ed_count, loadseq, TRUE);
				tioop (I_OUT, outusecall);

				/* we're guarenteed that ed_addr points at a dynamic mobile array -- free it. */
				texpopd_main (P_EXP, ed_addr, MANY_REGS, FALSE);
				gensecondary (I_MRELEASE);
				/*}}}*/
			}

			if (!is_count) {
				/* resume outputting process (last thing done was xin) */
				texpopd_main (channelmode, channel, MANY_REGS, FALSE);
				tioop (I_XEND, usecall);
			}
			/*}}}*/
		} else {
			/*{{{  generate code for DECODE.CHANNEL operation*/
			/*{{{  wait for outputting process*/
			if (!is_count && !(ActionFlagsOf (tptr) & ActionFlag_skip_xable)) {
				texpopd_main (channelmode, channel, MANY_REGS, FALSE);
				tioop (I_XABLE, usecall);
			}
			/*}}}*/

			/*{{{  store any temporaries if needed (for counted/tagged protocols)*/
			if (is_count) {
				/*{{{  decoding count for counted-array protocol (modified July 2003) */
				treenode *cval = ThisItem (NextItem (NextItem (RHSOf (tptr))));

				tload2regs (P_PTR, cval, channelmode, channel, FALSE, FALSE);
				texpopd_main (P_EXP, ActionTypeOf (tptr), 1, FALSE);			/* constant */
				tioop (I_IN, usecall);							/* always something next.. */
				/*}}}*/
			} else if (is_case) {
				/*{{{  decoding tag input for CASE protocol */
				treenode *tval = ThisItem (NextItem (NextItem (RHSOf (tptr))));

				tload2regs (P_PTR, tval, channelmode, channel, FALSE, FALSE);
				loadconstant (1);
				tioop (I_XIN, usecall);
				/*}}}*/
			}
			/*}}}*/

			if (!is_count && !is_mobproc) {
				/*{{{  evaluate both channels, leaving the pointer passed in communication in Areg*/
				tload2regs (channelmode, outchan, channelmode, channel, FALSE, FALSE);
				/* top-of-stack is input channel address */
				genprimary (I_LDNL, 0);		/* dereference to get channel value */
#ifdef PROCESS_PRIORITY
				genprimary (I_LDNL, -4);	/* load pointer */
#else
				genprimary (I_LDNL, -3);	/* load pointer */
#endif
				/*}}}*/
			}

#ifdef MOBILES
			if (!is_count && is_mobproc) {
				/*{{{  mobile process -- stack is empty*/
				treenode *ed_addr = ThisItem (NextItem (NextItem (RHSOf (tptr))));
				treenode *ed_count = ThisItem (NextItem (NextItem (NextItem (RHSOf (tptr)))));

#if 0
fprintf (stderr, "txinputoutput (DECODE) mobile process: ed_addr = ");
printtreenl (stderr, 4, ed_addr);
fprintf (stderr, "txinputoutput (DECODE) mobile process: ed_count = ");
printtreenl (stderr, 4, ed_count);
#endif

				loadseq = giveorder (channelmode, channel, P_PTR, ed_addr, P_PTR, ed_count, &preeval_e2, &preeval_e3);
				tload3regs (channelmode, channel, P_PTR, ed_addr, P_PTR, ed_count, loadseq, TRUE);

				/* serialise mobile process */
				genmppserialise ();

				/* communicate out */
				tload2regs (channelmode, outchan, P_EXP, ed_addr, FALSE, FALSE);
				tioop (I_OUTWORD, outusecall);
				tload2regs (channelmode, outchan, P_EXP, ed_count, FALSE, FALSE);
				tioop (I_OUTWORD, outusecall);
				if (is_ed3seq) {
					tload2regs (channelmode, outchan, P_EXP, oseq, FALSE, FALSE);
					tioop (I_OUTWORD, outusecall);
				}
				/*}}}*/
			} else if (!is_count && is_mobile) {
				/*{{{  if there's something (static) MOBILEy going on, de-reference again to get real pointer */
				treenode *ed_addr = ThisItem (NextItem (NextItem (RHSOf (tptr))));

				genprimary (I_LDNL, 0);
				storeinname (ed_addr, 0);	/* store address */

				/* now communicate on the output channel -- FIXME: only INT; INT supported at the moment */
				tload2regs (channelmode, outchan, P_EXP, ed_addr, FALSE, FALSE);
				tioop (I_OUTWORD, outusecall);
				tload2regs (channelmode, outchan, P_EXP, ActionTypeOf (tptr), FALSE, FALSE);
				tioop (I_OUTWORD, outusecall);
				if (is_ed3seq) {
					tload2regs (channelmode, outchan, P_EXP, oseq, FALSE, FALSE);
					tioop (I_OUTWORD, outusecall);
				}
				/*}}}*/
			} else if (!is_count && is_dynmob) {
				/*{{{  if there's something (dynamic) MOBILEy going on, communicate dimensions first, then data */
				/* Areg has a workspace address where the pointer followed by n dimensions reside. */
				/* The action-type is TIMES with dimension-count on the left and base-type size on the right (tran1) */
				/* NOTE: if 1 dimension, no seperate sizes are communicated */

				treenode *ed_addr = ThisItem (NextItem (NextItem (RHSOf (tptr))));
				treenode *ed_count = ThisItem (NextItem (NextItem (NextItem (RHSOf (tptr)))));
				/* treenode *ed_ccnt = ThisItem (NextItem (NextItem (NextItem (NextItem (RHSOf (tptr)))))); */
				treenode *dimcount = LeftOpOf (ActionTypeOf (tptr));
				treenode *basesize = RightOpOf (ActionTypeOf (tptr));
				int dimconst = LoValOf (dimcount);
				int i;

				genprimary (I_LDNLP, 1);	/* address of first dimension */
				storeinname (ed_addr, 0);	/* store address */

				genprimary (I_LDC, dimconst * bytesperword);
				/* texpopd_main (P_EXP, dimcount, MANY_REGS, FALSE); */
				/* genshiftimmediate (I_SHL, WSH); */		/* one slot per dimension */
				storeinname (ed_count, 0);			/* store count */

				if (dimconst > 1) {
					/* now communicate on the output channel -- FIXME: only INT; INT supported at the moment */
					tload2regs (channelmode, outchan, P_EXP, ed_addr, FALSE, FALSE);
					tioop (I_OUTWORD, outusecall);
					tload2regs (channelmode, outchan, P_EXP, ed_count, FALSE, FALSE);
					tioop (I_OUTWORD, outusecall);
					if (is_ed3seq) {
#if 0
fprintf (stderr, "gen10: txinputoutput: oseq = ");
printtreenl (stderr, 4, oseq);
#endif
						tload2regs (channelmode, outchan, P_EXP, oseq, FALSE, FALSE);
						tioop (I_OUTWORD, outusecall);
						/* decrement oseq */
						if (TagOf (oseq) != S_CONSTEXP) {
							geninternal_is (GEN_ERROR_IN_ROUTINE, 3, "txinputoutput");
						} else {
							SetLoVal (oseq, LoValOf(oseq) - 1);
						}
					}
				}

				/* now for the data itself -- size is computed by multiplying the various dimensions together with the base-type size */
				texpopd_main (P_EXP, basesize, MANY_REGS, FALSE);
				for (i=0; i<dimconst; i++) {
					loadname (ed_addr, 0);	/* load address of first dimension */
					genprimary (I_LDNL, i);
					gensecondary (I_PROD);	/* multiply */
				}
				storeinname (ed_count, 0);			/* store count */
				loadname (ed_addr, 0);		/* load address of first dimension */
				genprimary (I_LDNL, -1);	/* load pointer */
				storeinname (ed_addr, 0);	/* store actual pointer to data */

				/* now communicate on the output channel -- FIXME: only INT; INT supported at the moment */
				tload2regs (channelmode, outchan, P_EXP, ed_addr, FALSE, FALSE);
				tioop (I_OUTWORD, outusecall);
				tload2regs (channelmode, outchan, P_EXP, ed_count, FALSE, FALSE);
				tioop (I_OUTWORD, outusecall);
				if (is_ed3seq) {
					tload2regs (channelmode, outchan, P_EXP, oseq, FALSE, FALSE);
					tioop (I_OUTWORD, outusecall);
				}
				/*}}}*/
			} else
#endif
			if (!is_count) {
				/*{{{  regular decode*/
				tioop (I_OUTWORD, outusecall);						/* output address */

				tload2regs (channelmode, outchan, P_EXP, ActionTypeOf (tptr), FALSE, FALSE);
				tioop (I_OUTWORD, outusecall);
				if (is_ed3seq) {
					tload2regs (channelmode, outchan, P_EXP, oseq, FALSE, FALSE);
					tioop (I_OUTWORD, outusecall);
				}
				/*}}}*/
			}

			if (!is_count) {
				/*{{{  resume outputting process */
				texpopd_main (channelmode, channel, MANY_REGS, FALSE);
				tioop (I_XEND, usecall);
				/*}}}*/
			}
			/*}}}*/
		}
	}
	return;
}
/*}}}*/
#endif /* PD_DECODE_CHANNEL && PD_DECODE_CHANNEL3 && PD_ENCODE_CHANNEL */


