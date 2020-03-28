/* $Id: gen8.c,v 1.9 1997/12/12 16:51:26 mdp2 Exp $ */

/*
 *	code generator - procedure, function and valof instance generation
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

/*
 *  MDP changes:
 *               change call of gencomment1 (.. %s ..) to gencomments
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
#include "predefhd.h"
#include "trandef.h"
#include "bind1def.h"
#include "bind2def.h"
#include "bind3def.h"
#include "gen1def.h"
#include "gen2def.h"
#include "gen4def.h"
#include "gen7def.h"
#include "gen8def.h"
#include "gen10def.h"
#include "gen11def.h"
#include "gen12def.h"
#include "code1def.h"
#include "profile.h"
#include "genkroc.h"
#include "mobile_types.h"
/*}}}*/

/*{{{  constants and definitions*/
typedef struct {
	int pmaxparamslotused;
	int pnestedfunctions;
	int pevaluated;
	int pmode;
#ifdef MOBILES
	int mobile_outslot;		/* slot where we left the mobile (accounting for CALL mechanism,
					 * which I agree is dangerous), or -4 if not-applicable
					 * (Wptr[-4] is the return-address, so not a param.) */
	int mobile_remember;		/* used to help correctly restore sizes on dynamic MOBILES (number of dimensions) */
	BOOL need_deref;		/* true if the actual parameter needs to be de-referenced before calling */
#endif
	treenode **pparamexp;
	treenode *pformaltype;
	BOOL pvalparam;			/* whether this is a VAL parameter or not */
	BOOL pclone;			/* whether this is a CLONE parameter */
	int pslot;			/* param slot */
	BOOL forkbarrier;
} paraminfo_t;

#define MAX_PREDEFPARAMS 20
#define MAX_PREDEFDESTS  3
/*}}}*/

/*{{{  private variables*/
PRIVATE treenode **valofdestlist;
/*}}}*/

/*{{{  PRIVATE treenode *hiddenparamnodeof(aparam, dimension)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  hiddenparamnodeof takes an actual parameter expression and a dimension
 *                    number and returns a pointer to a bit of tree
 *                    representing that dimension of the actual parameter.
 *                    This will normally be a pointer to the array node
 *                    in the type field of the name of the parameter.
 *                    However, the dimension may be a segment length expression
 *                    in which case we return a pointer to that expression.
 *                    This isn't really satisfactory as the expression is
 *                    liable to get transformed later on!
 *                    At the moment, though, if a segment length is to be
 *                    transformed, ie. a temp node inserted to preevaluate it
 *                    into, this will be done by mappreprocess, which will
 *                    already have run.
 *
 *                    This routine also works when augmenting formal  parameter
 *                    lists: it will return a pointer to the array node in the
 *                    type field of the formal parameter name node.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE treenode *hiddenparamnodeof (treenode *const aparam, const int dimension, const int arg_no)
{
	wordnode *const name = lookupword ("$hidden", 7);
	treenode *const param = TagOf (aparam) == S_CLONE ? OpOf (aparam) : aparam;
	treenode *node, *typetree;
	
	if (ismobile (param)) {
		/* type tree for mobiles maybe stale, workaround by not using it */
		typetree = newdopnode (S_NTH_DIMENSION, NOPOSN, param, newconstant (dimension + 1), S_INT);
	} else {
		typetree = copytree (dimexpof (param, dimension), be_lexlevel);
	}
	DEBUG_MSG (("hiddenparamnodeof: dimension is %d\n", dimension));

#if 0
fprintf (stderr, "hiddenparamnodeof: dimension = %d, arg_no = %d, param = ", dimension, arg_no);
printtreenl (stderr, 4, param);
fprintf (stderr, "  hiddenparamnodeof: typetree = ");
printtreenl (stderr, 4, typetree);
#endif

	node = newnamenode (S_HIDDEN_PARAM, NOPOSN, name, NULL, typetree, be_lexlevel, 0, NM_DEFAULT);
	SetHDimension (node, dimension);
	SetHArgNo (node, arg_no);
#if 0
fprintf (stderr, "  hiddenparamnodeof: returning node = ");
printtreenl (stderr, 4, node);
#endif
	return node;
}

/*}}}*/
/*{{{  PRIVATE int npparamsof (treenode *const n)*/
/*
 *	returns the number of "real" parameters used by something
 */
PRIVATE int npparamsof (treenode *const n)
{
	if ((nodetypeoftag (TagOf (n)) == NAMENODE) && (NTypeOf (n)) && (TagOf (NTypeOf (n)) == S_MOBILE) && (TagOf (MTypeOf (NTypeOf (n))) == N_PROCTYPEDECL)) {
		return NPParamsOf (MTypeOf (NTypeOf (n)));
	}
	return NPParamsOf (n);
}
/*}}}*/
/*{{{  PRIVATE buildchecktree(treenode *exp, treenode *range, treenode *extent)*/
/*{{{  comment*/
/*****************************************************************************
 *
 * buildchecktree builds, constant folds, and returns the tree
 *
 *  OLD:                 csub0              Failed to detect negative extent,
 *                     /       \            and disallowed (exp + extent) = 0
 *                   add       range
 *                /      \
 *            csub0       subtract
 *           /    \       /      \
 *         exp   range   extent   1
 *
 *  NEW:                  csub0
 *                       /       \
 *                   add           plus
 *                /      \         |    \
 *            csub0       csub0   range  1
 *           /    \       /     \
 *         exp   range   extent  mint
 *
 *  IE: 0 <= exp < range AND extent >= 0 AND 0 <= (exp + extent) <= range
 *
 *****************************************************************************/
/*}}}*/
PRIVATE treenode *buildchecktree (treenode * exp, treenode * range, treenode * extent, const BOOL check_extent)
{
	SOURCEPOSN locn = LocnOf (exp);
	treenode *checkexp, *size;

	checkexp = newdopnode (S_CSUB0, locn, exp, range, S_INT);
	if (check_extent) {	/* bug TS/2003 20/01/93 */
		treenode *const mint = newconstant ((targetintsize == S_INT16) ? MOSTNEG_INT16 : MOSTNEG_INT32);
		extent = newdopnode (S_CSUB0, locn, extent, mint, S_INT);
	}
	checkexp = newdopnode (S_ADD, locn, checkexp, extent, S_INT);
	size = newdopnode (S_PLUS, locn, range, newconstant (1), S_INT);
	checkexp = newdopnode (S_CSUB0, locn, checkexp, size, S_INT);
	checkexp = foldexp (checkexp);
	return isconst (checkexp) ? NULL : checkexp;
}

/*}}}*/
/*{{{  PRIVATE int mapevalexp(int expmode, treenode **exp)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  mapevalexp takes the expression (expmode, exp) and if it requires
 *             more than one register to generate, maps the evaluation
 *             of it into a temporary, updating exp. returns new expmode.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE int mapevalexp (int expmode, treenode ** exp)
{
	if (regsforopd (expmode, *exp) > 1) {
		mapexpopd (expmode, exp);
		*exp = gettemp (*exp, NM_WORKSPACE);
		upusecount (*exp, 1);
		return tempmodeof (expmode);
	}
	return expmode;
}

/*}}}*/
/*{{{  PRIVATE int findinstances (tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  findinstances searches through an expression tree, tptr, looking for
 *                nested functions.  If no nested functions are found, the
 *                value returned is -1, otherwise the value returned is
 *                the number of parameters of the function with most parameters
 *                in the expression.
 *
 *                Now extended to cope with in-line VALOFs, it will also
 *                search through VALOF bodies for both function and
 *                procedure instances as above.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE int findinstances (treenode * tptr)
{
	while (tptr != NULL) {
		/*DEBUG_MSG(("findinstances : %s\n", itagstring(TagOf(tptr)))); */
		switch (nodetypeoftag (TagOf (tptr))) {
			/*{{{  expressions */
			/*{{{  monadic operators */
		case MOPNODE:
			if (TagOf (tptr) == S_ELSIZE)
				return (-1);
			else
				return (int) max_INT32 (implicitparams (TagOf (tptr), MOpTypeOf (tptr)), findinstances (OpOf (tptr)));
			/*}}} */
			/*{{{  dyadic operators */
		case DOPNODE:
			{
				const int m = implicitparams (TagOf (tptr), DOpTypeOf (tptr));
				return (int) max_INT32 (m, max_INT32 (findinstances (LeftOpOf (tptr)), findinstances (RightOpOf (tptr))));
			}
			/*}}} */
			/*{{{  constant expression */
		case CONSTEXPNODE:
		case CONSTTABLENODE:
			/*case HIDDENPARAMNODE: */
		case LEAFNODE:
		case WORDNODE:	/* added for S_GUYCODE and S_GUYSTEP */
			return (-1);
			/*}}} */
			/*{{{  function instance */
		case INSTANCENODE:
			{
				const int pdreq = pdparams (tptr);
				/*DEBUG_MSG(("findinstances: pdparams returned %d\n", pdreq)); */
				return (int) max_INT32 (pdreq, max_INT32 (npparamsof (INameOf (tptr)), findinstances (IParamListOf (tptr))));
			}
			/*}}} */
			/*{{{  subscript or segment */
		case ARRAYSUBNODE:
			if (TagOf (tptr) == S_ARRAYITEM || TagOf (tptr) == S_RECORDITEM)
				return findinstances (ASExpOf (tptr));
			else
				return (findinstances (ASBaseOf (tptr)));

		case SEGMENTNODE:
			{
				INT32 m = findinstances (SStartExpOf (tptr));
				m = max_INT32 (m, findinstances (SLengthExpOf (tptr)));
				return (int) max_INT32 (m, findinstances (SNameOf (tptr)));
			}
			/*}}} */
			/*{{{  name */
		case NAMENODE:
			switch (TagOf (tptr)) {
			case T_TEMP:
			case T_PREEVALTEMP:
				return (findinstances (NDeclOf (tptr)));
			default:
				return (-1);
			}
			/*}}} */
			/*{{{  specification ... valof */
		case DECLNODE:
			{
				INT32 max_i = -1;
				for (; isspecification (tptr); tptr = DBodyOf (tptr))
					max_i = max_INT32 (max_i, findinstances (DValOf (tptr)));
				return (int) max_INT32 (max_i, findinstances (tptr));
			}
		case VALOFNODE:
			return (int) max_INT32 (findinstances (VLBodyOf (tptr)), findinstances (VLResultListOf (tptr)));
			/*}}} */
			/*}}} */
			/*{{{  processes  (inside a VALOF) */
			/*{{{  SEQ IF */
		case CNODE:
			return (findinstances (CBodyOf (tptr)));
			/*}}} */
			/*{{{  REPLSEQ REPLIF */
		case REPLCNODE:
			return (int) max_INT32 (findinstances (ReplCStartExpOf (tptr)),
						max_INT32 (findinstances (ReplCLengthExpOf (tptr)),
							   max_INT32 (((ReplCStepExpOf (tptr) == NULL) ? 0 : findinstances (ReplCStepExpOf (tptr))),
								      findinstances (ReplCBodyOf (tptr)))));
			/*}}} */
			/*{{{  WHILE CHOICE */
		case CONDNODE:
			return (int) max_INT32 (findinstances (CondGuardOf (tptr)), findinstances (CondBodyOf (tptr)));
			/*}}} */
			/*{{{  CASE */
		case ACTIONNODE:
			return (int) max_INT32 (findinstances (LHSOf (tptr)), findinstances (RHSOf (tptr)));
			/*}}} */
			/*{{{  LIST */
		case LISTNODE:
			{
				INT32 m = -1;
				for (; !EndOfList (tptr); tptr = NextItem (tptr))
					m = max_INT32 (m, findinstances (ThisItem (tptr)));
				return (int) m;
			}
			/*}}} */
			/*}}} */
		default:
			badtag (genlocn, TagOf (tptr), "findinstances");
		}
	}
	return (-1);
}

/*}}}*/

/*{{{  PRIVATE void loadparamtable(paramtable, nparams, instanceptr)*/
/*****************************************************************************
 *
 *  loadparamtable loads up the table 'paramtable' with lots of crucial
 *                 info. on the actual parameters contained in the tree
 *                 'instanceptr'. nparams is the number of actual parameters
 *                 including any hidden ones.
 *
 *****************************************************************************/
PRIVATE void loadparamtable (const int ptype, paraminfo_t paramtable[], int nparams, treenode * instanceptr)
{
	treenode *nptr = INameOf (instanceptr);
	treenode *aparams = IParamListOf (instanceptr);
	treenode *fparams = NParamListOf (nptr);
#ifdef MOBILES
	treenode *last_actual = NULL;
	treenode *last_formal = NULL;
#endif
	int i;
	DEBUG_MSG (("loadparamtable: calling proc/function %s, with %d params\n", WNameOf (translate_from_internal (NNameOf (nptr))),	/* MDP */
		    nparams));

#if 0
fprintf (stderr, "loadparamtable: calling proc/function %s, with %d params\n", WNameOf (translate_from_internal (NNameOf (nptr))), nparams);
fprintf (stderr, "loadparamtable: formal parameter list is (%p) :", fparams);
printtreenl (stderr, 4, fparams);
fprintf (stderr, "loadparamtable: actual parameter list is (%p) :", aparams);
printtreenl (stderr, 4, aparams);
#endif
	for (i = 0; i < nparams; i++) {
		/*{{{  set information for parameter i */
		/*{{{  skip over TIMER parameters */
		while (basetype (gettype (ThisItem (fparams))) == S_TIMER) {
			fparams = NextItem (fparams);
			aparams = NextItem (aparams);
		}
		/*}}} */
		{
			paraminfo_t *paraminfo = &(paramtable[i]);
			int base = i;

			paraminfo->pparamexp = ThisItemAddr (aparams);
			paraminfo->pformaltype = gettype (ThisItem (fparams));
			paraminfo->pevaluated = FALSE;
#ifdef MOBILES
			paraminfo->mobile_outslot = -4;
			paraminfo->mobile_remember = 0;
			if (ptype != PROC_NONE) {
				base = 3;
			}
#endif

			/* These are defaults, modified later if neccessary  */
			paraminfo->pnestedfunctions = FALSE;
			paraminfo->pmaxparamslotused = -1;
			paraminfo->pclone = FALSE;
			paraminfo->pslot = -255;
			paraminfo->pvalparam = FALSE;
			paraminfo->forkbarrier = FALSE;
			paraminfo->need_deref = FALSE;

			/*{{{  set up paraminfo->pmode to be P_EXP or P_PTR or P_TEMPPTR */
			if (ishiddenparam (ThisItem (fparams))) {
				/*{{{  its a hidden parameter */
				DEBUG_MSG (("loadparamtable: param %d is hidden (P_EXP)\n", i));
				paraminfo->pmode = P_EXP;
#ifdef MOBILES
#if 0
fprintf (stderr, "loadparamtable: last_actual =");
printtreenl (stderr, 4, last_actual);
fprintf (stderr, "loadparamtable: last_formal =");
printtreenl (stderr, 4, last_formal);
#endif
				if ((ptype == PROC_NONE) && (i <= REG_PARAMS) && ((last_actual ? TagOf (last_actual) == S_CLONE : 0))) {
					treenode *real_actual = OpOf (last_actual);
					if (isdynmobilearray (real_actual) && (TagOf (ThisItem (fparams)) == S_HIDDEN_PARAM)) {
						if (HDimensionOf (ThisItem (aparams)) == -1) {
							paraminfo->pmode = P_TEMPPTR;
						}
					}
				} else if (isdynmobilearray (last_actual) && isdynmobilearray (last_formal) && (TagOf (ThisItem (fparams)) == S_HIDDEN_PARAM)) {
					/* skip */
				} else if (isanychantype (last_actual) && isanychantype (last_formal) && (TagOf (ThisItem (fparams)) == S_HIDDEN_TYPE)) {
					/* better copy back the type */
					paraminfo->mobile_outslot = (base - 3);
					paraminfo->mobile_remember = 0;
				} else if (isdynmobilechantype (last_actual) && isanychantype (last_formal) && (TagOf (ThisItem (fparams)) == S_HIDDEN_TYPE)) {
					/* check the type instead of copying it back */
					paraminfo->mobile_outslot = (base - 3);
					paraminfo->mobile_remember = 2;
				} else if (isanyproctype (last_actual) && isanyproctype (last_formal) && (TagOf (ThisItem (fparams)) == S_HIDDEN_TYPE)) {
					/* better copy back the type */
					paraminfo->mobile_outslot = (base - 3);
					paraminfo->mobile_remember = 0;
				} else if (isdynmobileproctype (last_actual) && isanyproctype (last_formal) && (TagOf (ThisItem (fparams)) == S_HIDDEN_TYPE)) {
					/* check the type instead of copying it back */
					paraminfo->mobile_outslot = (base - 3);
					paraminfo->mobile_remember = 2;
				}

#endif
				if (TagOf (ThisItem (fparams)) == S_PARAM_FB) {
					/*{{{  FORK barrier is accessed as an expression*/
					paraminfo->pmode = P_EXP;
					paraminfo->forkbarrier = TRUE;
					/*}}}*/
				} else if (TagOf (ThisItem (fparams)) == S_PARAM_WS) {
					paraminfo->pmode = P_EXP;
				}
				/*}}} */
#ifdef MOBILES
			} else if (TagOf (ThisItem (fparams)) == S_PARAM_MPP) {
				/*{{{  it's the mobile activation "self" parameter */
				paraminfo->pmode = P_PTR;
				/*}}}*/
#endif
			} else {
				/*{{{  its a real parameter */
				treenode *const aparam = ThisItem (aparams);
				const BOOL pointerparam = ispointer (ThisItem (fparams));

				last_actual = aparam;
				last_formal = ThisItem (fparams);
#ifdef MOBILES
#if 0
fprintf (stderr, "loadparamtable: (real) param = %d, nparams = %d, aparam =", i, nparams);
printtreenl (stderr, 4, aparam);
fprintf (stderr, "  fparam =");
printtreenl (stderr, 4, ThisItem (fparams));
fprintf (stderr, "  ismobile (aparam) = %d, ismobile (fparam) = %d, islocal (aparam) = %d, ispointer (aparam) = %d, pointerparam=%d\n", ismobile (aparam), ismobile (ThisItem (fparams)),
	islocal (aparam, be_lexlevel), ispointer (aparam), pointerparam);
fprintf (stderr, "  isdynmobilearraytype (aparam) = %d, isdynmobilechantype (aparam) = %d\n", isdynmobilearraytype (aparam), isdynmobilechantype (aparam));
#endif
				if (isdynmobilechantype (aparam) && isanychantype (ThisItem (fparams))) {
					/* MOBILE.CHAN -- better remember it */
					paraminfo->mobile_outslot = (base - 3);
					paraminfo->mobile_remember = 1;
				} else if (isdynmobileproctype (aparam) && isanyproctype (ThisItem (fparams))) {
					/* MOBILE.PROC -- remember also */
					paraminfo->mobile_outslot = (base - 3);
					paraminfo->mobile_remember = 1;
				}
				
				if (ismobile (aparam) && ismobile (ThisItem (fparams))) {
					if (isdynmobilearray (aparam)) {
						paraminfo->mobile_outslot = (base - 2); /* offset by 1 */
					} else {
						paraminfo->mobile_outslot = (base - 3);
					}
					paraminfo->mobile_remember = 1;
				}

#endif
				if (pointerparam && !isaddressable (aparam)) {
					DEBUG_MSG (("loadparamtable: param %d is pointer and not addresable (P_TEMPPTR)\n", i));
					paraminfo->pmode = P_TEMPPTR;
				} else if ((ptype == PROC_NONE) && (i < REG_PARAMS) && (TagOf (aparam) == S_CLONE)) {
					paraminfo->pmode = P_TEMPPTR;
				} else {
					const int f = findinstances (aparam);
					DEBUG_MSG (("loadparamtable: param %d is pointer? %d (P_EXP/P_PTR)(findinstances %d)\n", i, pointerparam, f));
					paraminfo->pmode = (pointerparam) ? P_PTR : P_EXP;
					/* if this is a TypeAttr_placed something, use the expression.  aparam must be a name */
					/* FIXME: array slices.. */
					if ((nodetypeoftag (TagOf (aparam)) == NAMENODE) && (nodetypeoftag (TagOf (NTypeOf (aparam))) == TYPENODE) &&
						(TypeAttrOf (NTypeOf (aparam)) & TypeAttr_placed)) {

						paraminfo->pmode = P_EXP;
#if 0
fprintf (stderr, "set paraminfo->pmode to P_EXP in loadparamtable()..\n");
#endif
					} else if (pointerparam && (TagOf (aparam) == N_DECL)) {
						if (TagOf (NTypeOf (aparam)) == S_FULLBARRIER) {
							paraminfo->pmode = P_EXP;
						}
					}
					paraminfo->pnestedfunctions = (f >= 0);
					paraminfo->pmaxparamslotused = ((f >= 0) ? (int) max_INT32 (0, (int) (f - MAXREGS)) : -1);
				}
				if ((TagOf (ThisItem (fparams)) == N_VALPARAM) && pointerparam) {
					paraminfo->pvalparam = TRUE;
				}
#ifdef MOBILES
				/*{{{  mobile process types: check mode for indirect parameters*/
				if (NPSuspendsOf (nptr) && is_indirect_name (ThisItem (fparams)) && !is_indirect_name (ThisItem (aparams))) {
					paraminfo->pmode = P_PTR;
				} else if (!is_indirect_name (ThisItem (fparams)) && is_indirect_name (ThisItem (aparams))) {
#if 0
fprintf (stderr, "loadparamtable: using deref for parameter\n");
#endif
					paraminfo->need_deref = TRUE;
				}
				/*}}}*/
#endif
				/*}}} */
			}
			/*}}} */

			fparams = NextItem (fparams);
			aparams = NextItem (aparams);
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PRIVATEPARAM void mapvalofmain (treenode *tptr)*/
PRIVATEPARAM void mapvalofmain (treenode * const tptr)
{
	treenode *resultlist = VLResultListOf (tptr);
	treenode *savedtemplist = templist;
	templist = NULL;
	mapprocess (VLBodyOf (tptr));
/*walkpreprocesslist(resultlist); *//* already done in mappreprocess CON 5/10/90 */

	switch (valof_return_style (resultlist)) {
	case return_style_fpu:
		mapfpexp (ThisItemAddr (resultlist));
		break;
	case return_style_alu:
		mapexp (ThisItemAddr (resultlist));
		break;
	case return_style_other:
		mapassign (valofdestlist, (listitems (resultlist) == 1)
			   ? ThisItemAddr (resultlist)	/* bug TS/1442 28/10/91 */
			   : VLResultListAddr (tptr));
		break;
	}
	freetemplist ();
	templist = savedtemplist;
}

/*}}}*/

/*{{{  PUBLIC treenode *augmentparams (treenode *aptr, treenode *fptr, treenode *destlist, treenode *instance)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  augmentparams augments an actual parameter list 'aptr' using the
 *                augmented formal parameter list 'fptr' and, for a function,
 *                the list of destinations 'destlist'.  For a procedure
 *                call, 'destlist' will be null.
 *  updated (frmb): added `instance' parameter; necessary to fill in the gap
 *                of mobile process activations.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC treenode *augmentparams (treenode *aptr, treenode *fptr, treenode *destlist, treenode *instance)
{
	/*{{{  COMMENT remove TIMER parameters from the actual list */
  /**********************  Start comment out ****************************
  @*{{{  remove TIMER parameters from the actual list*@
  {
    treenode **paramptr = &aptr;
    while (!EndOfList(*paramptr))
      {
        if (basetype(gettype(ThisItem(*paramptr))) == S_TIMER)
          *paramptr = NextItem(*paramptr);
        else
          paramptr = NextItemAddr(*paramptr);
      }
  }
  @*}}}*@
   **********************   End comment out  ****************************/
	/*}}} */
	/*{{{  augment actual parameter list */
	{
		treenode *resultlist = aptr;
		treenode **a = &resultlist;
		treenode *lastactual = NULL;
		int arg_no = 0;

		/*{{{  debugging messages */
#if 0
fprintf (stderr, "gen8: augmentparams(): aptr (%p) = ", aptr);
printtreenl (stderr, 4, aptr);
fprintf (stderr, "gen8: augmentparams(): fptr (%p) = ", fptr);
printtreenl (stderr, 4, fptr);
fprintf (stderr, "gen8: augmentparams(): destlist = ");
printtreenl (stderr, 4, destlist);
fprintf (stderr, "gen8: augmentparams(): instance (%p) = ", instance);
printtreenl (stderr, 4, instance);
#endif
		/*}}} */

		for (; !EndOfList (fptr); fptr = NextItem (fptr)) {
			treenode *thisformal = ThisItem (fptr);
			treenode *thisactual = (!EmptyList (aptr)) ? ThisItem (aptr) : NULL;
			DEBUG_MSG (("augmentparams: thisformal is %s\n", itagstring (TagOf (thisformal))));
			switch (TagOf (thisformal)) {
				/*{{{  augment actual param list for this formal parameter */
				/*{{{  N_PARAM N_VALPARAM N_RESULTPARAM */
			case N_PARAM:
			case N_VALPARAM:
			case N_RESULTPARAM:
#if 0
fprintf (stderr, "augmentparams: real parameter:");
printtreenl (stderr, 4, thisactual);
#endif
				lastactual = thisactual;
				arg_no++;
				break;
				/*}}} */
				/*{{{  S_PARAM_STATICLINK */
			case S_PARAM_STATICLINK:
				if (!thisactual || (TagOf (thisactual) != S_PARAM_STATICLINK)) {
					wordnode *const sl_name = lookupword ("$sl", 3);
					treenode *const sl = newnamenode (S_PARAM_STATICLINK, NOPOSN, sl_name, NULL, NULL,
									  be_lexlevel, 0, NM_DEFAULT);

#if 0
fprintf (stderr, "augmentparams: static link\n");
#endif
					*a = addtofront (sl, *a);
					aptr = *a;
				}
				break;
				/*}}} */
				/*{{{  S_PARAM_VSP */
			case S_PARAM_VSP:
				if (!thisactual || (TagOf (thisactual) != S_PARAM_VSP)) {
					wordnode *const vs_name = lookupword ("$VSP", 4);
					treenode *const vs = newnamenode (S_PARAM_VSP, NOPOSN, vs_name, NULL, NULL,
									  be_lexlevel, 0, NM_DEFAULT);
					SetHDimension (vs, (int) vsp);
#if 0
fprintf (stderr, "augmentparams: vsp\n");
#endif
					*a = addtofront (vs, *a);
					aptr = *a;
				}
				break;
				/*}}} */
				/*{{{  S_PARAM_FB*/
			case S_PARAM_FB:
				if (!thisactual || ((TagOf (thisactual) != S_PARAM_FB) && !IForkOf (instance)) || ((TagOf (thisactual) != S_PARAM_FB) && (IForkOf (instance) && (DNameOf (CBodyOf (IForkOf (instance))) != thisactual)))) {
					if (IForkOf (instance)) {
						/* using a local FORKING block */
						*a = addtofront (DNameOf (CBodyOf (IForkOf (instance))), *a);
						aptr = *a;
					} else {
						/* need the parameter version */
						wordnode *const fb_name = lookupword ("$fork.barrier", 13);
						treenode *const fb = newnamenode (S_PARAM_FB, NOPOSN, fb_name, newleafnode (S_BARRIER, NOPOSN),
										NULL, be_lexlevel, 0, NM_DEFAULT);

						*a = addtofront (fb, *a);
						aptr = *a;
					}
				}
				break;
				/*}}}*/
#ifdef MOBILES
				/*{{{  S_PARAM_MSP */
			case S_PARAM_MSP:
				if (!thisactual || (TagOf (thisactual) != S_PARAM_MSP)) {
					wordnode *const ms_name = lookupword ("$msp", 4);
					treenode *const ms = newnamenode (S_PARAM_MSP, NOPOSN, ms_name, NULL, NULL,
									  be_lexlevel, 0, NM_DEFAULT);
					SetHDimension (ms, 0);
#if 0
fprintf (stderr, "augmentparams: msp\n");
#endif
					*a = addtofront (ms, *a);
					aptr = *a;
				}
				break;
				/*}}}  */
				/*{{{  S_PARAM_MPP */
			case S_PARAM_MPP:
				/* add instance name as a parameter -- mobile process activation param */
				if (isdynmobileproctype (INameOf (instance)) && (!thisactual || (thisactual != INameOf (instance)))) {
					treenode *name = INameOf (instance);

					/* this one is slightly special -- can't ever pass the instance name as a "regular" parameter */
					*a = addtofront (name, *a);
					aptr = *a;
				} else if (!thisactual || (TagOf (thisactual) != S_PARAM_MPP)) {
					wordnode *const mp_name = lookupword ("$mpp", 4);
					treenode *const mp = newnamenode (S_PARAM_MPP, NOPOSN, mp_name, NULL, NULL,
									  be_lexlevel, 0, NM_DEFAULT);
					*a = addtofront (mp, *a);
					aptr = *a;
				}
				break;
				/*}}}*/
#endif
				/*{{{  S_HIDDEN_PARAM */
			case S_HIDDEN_PARAM:
				if (!thisactual || (TagOf (thisactual) != S_HIDDEN_PARAM) || (HArgNoOf (thisactual) != arg_no)) {
					const int dimension = HDimensionOf (thisformal);
					treenode *newnode;

#if 0
fprintf (stderr, "augmentparams: hidden parameter\n");
fprintf (stderr, "lastactual is %p, dimension is %d:", lastactual, dimension);
printtreenl (stderr, 4, lastactual);
#endif
					newnode = hiddenparamnodeof (lastactual, dimension, arg_no);
#if 0
fprintf (stderr, "augmentparams: (hidden param) newnode = ");
printtreenl (stderr, 4, newnode);
#endif
					*a = addtofront (newnode, *a);
					aptr = *a;
				}
				break;
				/*}}} */
				/*{{{  S_HIDDEN_TYPE*/
			case S_HIDDEN_TYPE:
				if (!thisactual || (TagOf (thisactual) != S_HIDDEN_TYPE) || (HArgNoOf (thisactual) != arg_no)) {
					wordnode *const name = lookupword ("$htype", 6);
					treenode *newnode;

					newnode = newnamenode (S_HIDDEN_TYPE, NOPOSN, name, NULL, newleafnode (S_INT, NOPOSN), be_lexlevel, 0, NM_DEFAULT);
					SetHDimension (newnode, 0);
					SetHArgNo (newnode, arg_no);
					SetHExp (newnode, lastactual);

					*a = addtofront (newnode, *a);
					aptr = *a;
				}
				break;
				/*}}}*/
				/*{{{  S_FNFORMALRESULT */
			case S_FNFORMALRESULT:
				if (!thisactual || (TagOf (thisactual) != S_FNACTUALRESULT) || (HArgNoOf (thisformal) != HArgNoOf (thisactual))) {
					const int result_no = (int) HArgNoOf (thisformal);
					treenode *const this_dest = nth_listitem (destlist, result_no);
					wordnode *const arg_name = lookupword ("$actualresult", 13);
					treenode *const arg = newnamenode (S_FNACTUALRESULT, NOPOSN, arg_name, NULL,
									   this_dest, be_lexlevel, 0, NM_DEFAULT);

#if 0
fprintf (stderr, "augmentparams: fnformalresult\n");
#endif
					SetHArgNo (arg, result_no);
					*a = addtofront (arg, *a);
					aptr = *a;
				}
				break;
				/*}}} */
				/*{{{  S_PARAM_WS */
			case S_PARAM_WS:
				if (!thisactual || (TagOf (thisactual) != S_PARAM_WS)) {
					INT32 proc_ws, proc_vs;
					/* wordnode *const name = lookupword ("$ws.size", 8); */
					treenode *wsnode;
					
					getprocwsandvs (INameOf (instance), &proc_ws, &proc_vs);

					wsnode = newintconstant (proc_ws, S_INT);
#if 0
fprintf (stderr, "augmentparams: msp\n");
#endif
					*a = addtofront (wsnode, *a);
					aptr = *a;
				}
				break;
				/*}}}  */
			default:
				badtag (genlocn, TagOf (thisformal), "augmentparams");
				/*}}} */
			}
			if (!EmptyList (aptr))
			{
				/*{{{  update a and aptr */
				a = NextItemAddr (aptr);
				aptr = NextItem (aptr);
				/*}}} */
			}
		}
#if 0
fprintf (stderr, "gen8: augmentparams: resultlist = (%p) ", resultlist);
printtreenl (stderr, 4, resultlist);
#endif
		return (resultlist);
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE void mapparameter*/
PRIVATE void mapparameter (paraminfo_t * paraminfoptr, const int i, const BOOL make_temp)
{
	treenode **expptr = paraminfoptr->pparamexp;

	USE_VAR (i);		/* make sure there's no unused variable warning */
	DEBUG_MSG (("mapparameter: param %d, temporary?:%d\n", i, make_temp));
	mapexpopd (paraminfoptr->pmode, expptr);
	paraminfoptr->pevaluated = TRUE;
	if (make_temp) {
		*expptr = gettemp (*expptr, NM_WORKSPACE);
		upusecount (*expptr, 1);
		paraminfoptr->pmode = P_TEMP;
	}
}

/*}}}*/
/*{{{  PRIVATE BOOL preevalrealbyvalue*/
PRIVATE BOOL preevalrealbyvalue (paraminfo_t * paraminfoptr)
/* returns TRUE if we have to preevaluate a real parameter when FPU */
{
	treenode *exp = *paraminfoptr->pparamexp;
	return has_fpu_core && paraminfoptr->pmode == P_EXP &&	/* passed by value */
		isreal (ntypeof (exp)) &&	/* real-valued */
		!isaddressable (exp);	/* not addressable */
}

/*}}}*/
/*{{{  PUBLIC void maproutinename*/
PUBLIC void maproutinename (treenode *const nptr, treenode *const tptr, const BOOL set_wssize, const BOOL set_vssize, const int ptype)
{
#if 0
fprintf (stderr, "maproutinename: name=%s, recursive=%d, forked=%d, set_wssize=%d, set_vssize=%d\n", WNameOf (NNameOf (nptr)), (ptype & PROC_REC), (ptype & PROC_FORKED), set_wssize, set_vssize);
#endif
	if (set_wssize) {
		INT32 ws, vs;
#ifdef MOBILES
		INT32 ms;

		if (ptype == PROC_NONE) {
			if (enable_mobilespace) {
				getprocwsandvsandms (nptr, &ws, &vs, &ms);
			} else {
				getprocwsandvs (nptr, &ws, &vs);
			}
		}
#else
		if (ptype == PROC_NONE) {
			getprocwsandvs (nptr, &ws, &vs);
		}
#endif

		if (ptype & PROC_DYNCALL) {
			/* dynamically called PROCs need some special handling too */
			ws = DYNCALL_SETUP_SLOTS + DS_MIN;
			vs = 0;
#ifdef MOBILES
			/* these cannot use static mobilespace */
			ms = 0;
#endif
		} else if (ptype & PROC_FORKED) {
			/* forked (dynamic) calls to a PROC need to be handled specially */
			ws = FORK_SETUP_SLOTS + DS_MIN;
			vs = 0;
#ifdef MOBILES
			ms = 0;
			if (enable_mobilespace) {
				INT32 tws = 0, tvs = 0, tms = 0;

				getprocwsandvsandms (nptr, &tws, &tvs, &tms);
				if (tms) {
					ms = 1;
				}
			}
#endif
		} else if (ptype & PROC_REC) {
			/* recursive (dynamic) calls to a PROC/FUNCTION need to be handled a bit specially */
			ws = RECURSIVE_SLOTS;
			vs = 0;
#ifdef MOBILES
			ms = 0;
#endif
#ifdef MOBILES
		} else if (ptype & PROC_MPA) {
			ws = MPA_SETUP_SLOTS;
			vs = 0;
			ms = 0;
			/* FIXME: might need to handle mobilespace at some point.. */
#endif
		}

		if (insidealtguard) {
			/* A function call inside an ALT guard has its parameters stored
			   BELOW the alt's belowworkspace slots */
			datasize = max_INT32 (datasize, ws + max_INT32 (findinstances (tptr), REG_PARAMS) + INS_EXTRA + alt_ds);
		} else {
			datasize = max_INT32 (datasize, ws + REG_PARAMS + INS_EXTRA);
		}

		/* frmb: um, duh, we might need a staticlink! (for anything not recursive) */
		if (!separatelycompiled (nptr)) {
#if 0
fprintf (stderr, "maproutinename: name=%s, staticlinkused = %d, NPSLUsageOf (nptr) = %d\n", WNameOf (NNameOf (nptr)), staticlinkused, NPSLUsageOf (nptr));
#endif
			if (!(ptype & (PROC_REC | PROC_MPA))) {
				/* NPSLUsage won't be set yet.. -- activations can't have static-links */
				staticlinkused = (int) min_INT32 (staticlinkused, NPSLUsageOf (nptr));
			}
		}

		if (set_vssize) {
			/* allocate vectorspace for this name */
			maxvsp = max_INT32 (maxvsp, vsp + vs);
#ifdef MOBILES
			if (!enable_mobilespace) {
				/* ms = 0; */
			} else if ((ptype == PROC_NONE) && ms) {
				/* allocate mobilespace for this name */
				newmobileinstance (tptr, ms);
			} else if ((ptype & PROC_FORKED) && ms) {
				/* this ends up parking the ms-offset in current mobilespace for this in IRMSPOffset (when bound) */
				newmobileinstance (tptr, 1);
			} else if ((ptype & PROC_REC) && ms) {
				/* allocate new mobilespace for this name */
#if 0
fprintf (stderr, "allocating recursive ms with newmobileinstance(..,1)\n");
#endif
				newmobileinstance (tptr, 1);
			}
#endif
		}
#if 0
fprintf (stderr, "maproutinename: ws=%d, vs=%d, ms=%d, datasize=%d, staticlinkuserd=%d\n", (int)ws, (int)vs, (int)ms, (int)datasize, (int)staticlinkused);
#endif
	}
	/* don't add dynamic calls to the libentry table */
	if (isinlibrary (nptr) && !(ptype & PROC_DYNCALL)) {
		add_to_libentries (nptr);
	}
}

/*}}}*/
/*{{{  PUBLIC void mapinstance(tptr)           with inline fp*/
/*{{{  comment*/
/*****************************************************************************
 *
 * mapinstance allocates workspace required during the call of a procedure or
 *             function.
 *
 *****************************************************************************/
/* Order of loading parameters:
   if there are non-register parameters ...
     1.   Register function calls which use workspace parameter slots
          are preevaluated to temporaries.
     2.   Non-register function calls are evaluated right to left
          directly into their parameter slots if possible, otherwise
          into temporaries.
     3.   Any temporaries generated in (2) are moved to their parameter slots.
     4.   Any remaining non-register parameters are evaluated into
          their parameter slots.
     5.   Register parameters are loaded.
   else ...
     1.   Register parameters are loaded.
 */
/*}}}*/
PUBLIC void mapinstance (treenode *const tptr)
{
	treenode *const nptr = INameOf (tptr);
	int nparams = npparamsof (nptr);
	int saved_datasize = -1;
	const BOOL recursive = (nodetypeoftag (TagOf (tptr)) == INSTANCENODE) ? IRecursiveOf (tptr) : FALSE;
	const BOOL forked = (nodetypeoftag (TagOf (tptr)) == INSTANCENODE) ? IForkedOf (tptr) : FALSE;
	const BOOL dyncall = (nodetypeoftag (TagOf (tptr)) == INSTANCENODE) ? IDynmemOf (tptr) : FALSE;
	const BOOL mprocact = (nodetypeoftag (TagOf (tptr)) == INSTANCENODE) ? (isdynmobileproctype (INameOf (tptr))) : FALSE;
	int ptype = PROC_NONE;

	if (forked) {
		ptype |= PROC_FORKED;
	}
	if (recursive) {
		ptype |= PROC_REC;
	}
	if (dyncall) {
		ptype |= PROC_DYNCALL;
	}
	if (mprocact) {
		ptype |= PROC_MPA;
	}
	/*{{{  COMMENT  wsp optimisation */
  	/**********************  Start comment out ****************************
  	@*{{{  *@
  	int params_below_ws = nparams > (MAX_LOCAL_PARAMS + REG_PARAMS)
  	                       ? nparams - (MAX_LOCAL_PARAMS + REG_PARAMS)
  	                       : 0;
  	@*}}}*@
  	 **********************   End comment out  ****************************/
	/*}}} */

	/* DEBUG CODE */
#if 0
fprintf (stderr, "mapinstance: nparams = %d, recursive = %d, forked = %d, mprocact = %d, nptr =", nparams, recursive, forked, mprocact);
printtreenl (stderr, 4, nptr);
fprintf (stderr, "mapinstance: tptr =");
printtreenl (stderr, 4, tptr);
fprintf (stderr, "mapinstance: formal param list =");
printtreenl (stderr, 4, NParamListOf (nptr));
fprintf (stderr, "mapinstance: actual param list =");
printtreenl (stderr, 4, IParamListOf (tptr));
#endif
	/*int savedinsidealtguard; *//* no longer used - bug 1159 15/2/91 */
	DEBUG_MSG (("mapinstance: %s\n", WNameOf (translate_from_internal (NNameOf (nptr)))));	/* MDP */
	/* Workspace is pulled down alt_ds + nparams slots */

	maproutinename (nptr, tptr, TRUE, TRUE, ptype);

#if 0
	/* bug 1159 - CON - 15/2/91 - this isn't used;
	   nested functions still have to know that they're inside an ALT guard,
	   a) so that the datasize can be calculated;
	   b) so that they do not attempt to reserve their parameter slots
	 */
	savedinsidealtguard = insidealtguard;
	insidealtguard = FALSE;	/* Nested functions don't need to pull down wptr */
#endif
	/*{{{  check for inside ALT guard */
	if (insidealtguard) {
		/* bug TS/1893 14/01/93 */
		alt_check_enabling_functions (tptr);
	}
	/*}}}  */
	/*{{{  if this is a recursive call (mutual or self), nparams may be incorrectly zero */
	if (recursive && !nparams) {
		treenode *fparams = NParamListOf (nptr);
		while (!EmptyList (fparams)) {
			nparams++;
			fparams = NextItem (fparams);
		}
	}
	/*}}}  */
	/*{{{  parameter WS requirements are special for forked, dyn-call and recursive stuff */
	if (forked) {
		saved_datasize = datasize;
		datasize = MIN_FORK_SLOTS;
	} else if (dyncall) {
		saved_datasize = datasize;
		datasize = MIN_DYNCALL_SLOTS;
	} else if (recursive) {
		saved_datasize = datasize;
		datasize = MIN_RECURSIVE_SLOTS;
	}

	/*}}}  */
	/*{{{  for forked instances, map $fork.barrier */
	if (forked) {
		treenode *forking = IForkOf (tptr);

		if (forking) {
			treenode *forknode = DNameOf (CBodyOf (forking));

			mapexp (&forknode);
		}
	}
	/*}}}*/
	/*{{{  if a dynamic call at a specific address, map that*/
	if (dyncall && IDynaddrOf (tptr)) {
		mapexp (IDynaddrAddr (tptr));
	}
	/*}}}*/
	/*{{{  map parameter loading */
	if (nparams > 0) {
		int i;
		int freereg[MAXREGS];
		treenode *lowwsp = NULL;
		paraminfo_t *paramtable = (paraminfo_t *) memalloc (sizeof (paraminfo_t) * nparams);

		loadparamtable (ptype, paramtable, nparams, tptr);
		/*{{{  initialise freereg */
		for (i = 0; i < MAXREGS; i++) {
			freereg[i] = FALSE;
		}
		/*}}} */
		/*{{{  preevaluate non-addressable params passed by reference, into temps. */
		/* Preevaluate non-addressable params passed by reference :
		   sounds silly, but does actually occur for INT64/REAL64 expressions,
		   array constructors etc. */
		for (i = 0; i < nparams; i++) {
			if (paramtable[i].pmode == P_TEMPPTR) {
				/*{{{  evaluate to temporary */
				treenode **paramexp = paramtable[i].pparamexp;
				DEBUG_MSG (("mapinstance: %s, param %d must be preevaluated\n", WNameOf (translate_from_internal (NNameOf (nptr))), i));  /* MDP */
				if (TagOf (*paramexp) == S_CLONE) {
					mapparameter (&(paramtable[i]), i, TRUE);
					/* AFAIK we don't need to map any deeper */
				} else if (isconst (*paramexp)) {
					placeintable (*paramexp);
					paramtable[i].pmode = P_PTR;
				} else {
					const int type = ntypeof (*paramexp);
					*paramexp = gettemp (*paramexp, NM_WORKSPACE);	/* Andy's */
					mapsimpleassign (type, P_TEMP, paramexp, P_EXP, NDeclAddr (*paramexp));
					/* We don't set 'pevaluated' because we have yet to load a pointer to it */
				}
				/*}}} */
			}
		}
		/*}}} */
		/*{{{  preevaluate real non-addressable register params on fp processor */
		/* For a floating point processor only,
		   preevaluate non-addressable real-valued register parameters passed by
		   value  */
		if (has_fpu_core) {
			const int p = (int) min_INT32 (nparams, MAXREGS);
			for (i = 0; i < p; i++) {
				if (preevalrealbyvalue (&paramtable[i])) {
					/*{{{  evaluate to temporary */
					treenode **paramexp = paramtable[i].pparamexp;
					if (isconst (*paramexp)) {
						placeintable (*paramexp);
					} else {
						mapfpexp (paramexp);
						*paramexp = gettemp (*paramexp, NM_WORKSPACE);	/* Andy's */
						/* store into the temporary */
						paramtable[i].pevaluated = TRUE;
						paramtable[i].pmode = P_TEMP;
						freereg[i] = TRUE;	/* Flag temporary needs to be freed later */
					}
					/*}}} */
				}
			}
		}
		/*}}} */
#ifdef MOBILES
		/*{{{  pre-evaluate to temporaries any SIZE'd dynamic-mobile arrays*/
		for (i = 0; i < nparams; i++) {
			treenode **paramexp = paramtable[i].pparamexp;

			/* SIZEs of dynamic-mobile arrays don't get assigned into temporaries before this, do it here */
			if ((paramtable[i].pmode == P_EXP) && (TagOf (*paramexp) == S_SIZE)) {
				treenode *sizeopd = OpOf (*paramexp);

				/* look for a mobile at the base */
				while (TagOf (sizeopd) == S_ARRAYITEM) {
					sizeopd = ASBaseOf (sizeopd);
				}
				if (isdynmobilearraypiece (sizeopd)) {
					/* okay, make it into a temporary */

#if 0
fprintf (stderr, "mapinstance: found SIZE'd dynamic mobile array in parameters.\n");
#endif
					mapexp (paramexp);
					*paramexp = gettemp (*paramexp, NM_WORKSPACE);
					mapsimpleassign (S_INT, P_TEMP, paramexp, P_EXP, NDeclAddr (*paramexp));
					/* don't set pevaluated -- need to load it first */
				}
			} else if ((paramtable[i].pmode == P_EXP) && (TagOf (*paramexp) == S_ARRAYITEM) && ismobile (ASBaseOf (*paramexp)) && (TagOf (ASBaseOf (*paramexp)) == S_ARRAYITEM)) {
				
				mapparameter (&(paramtable[i]), i, TRUE);
			}
		}
		/*}}}*/
#endif
		if (nparams > REG_PARAMS) {
			/*{{{  preevaluate register parameters which use parameter slots */
			for (i = 0; i < MAXREGS; i++) {
				if (!paramtable[i].pevaluated && paramtable[i].pmaxparamslotused > 0) {
					mapparameter (&(paramtable[i]), i, TRUE);
					freereg[i] = TRUE;	/* Flag temporary needs to be freed later */
				}
			}
			/*}}} */
			/*{{{  evaluate non-register parameters containing function calls to param slots */
			for (i = nparams - 1; i >= MAXREGS; i--) {
				paraminfo_t *const paraminfoptr = &(paramtable[i]);
				if (paraminfoptr->pnestedfunctions) {
					const int next_slot = i + 1 - MAXREGS;	/* bug 1064 6/12/90 */
					if (paraminfoptr->pmaxparamslotused > next_slot /*(i - MAXREGS) */ ) {	/* bug 1064 */
						mapparameter (paraminfoptr, i, TRUE);
					} else if (insidealtguard || (i == (nparams - 1))) {
						mapparameter (paraminfoptr, i, FALSE);
					} else {
						/* we have to protect the slots already copied into */
						/* bug 1013 12/10/90 */
						treenode *const paramslots = alloc_fixedws (next_slot, (int) (nparams - MAXREGS - next_slot));
						mapparameter (paraminfoptr, i, FALSE);
						kill_var (paramslots);
					}
				}
			}
			/*}}} */
			/*{{{  reserve low workspace used by params */
			/* This test moved up here so that we have allocated these param
			   slots in advance of mapping the other params:
			   bug 1013 11/10/90
			 */
			if (!insidealtguard) {
				lowwsp = alloc_fixedws (0, (int) (nparams - MAXREGS));
			}
			/* Inside an ALT guard, all parameters are stored below normal workspace */
			/*}}} */
			/*{{{  evaluate all non-register parameters to parameter slots */
			for (i = nparams - 1; i >= MAXREGS; i--) {
				paraminfo_t *const paraminfoptr = &(paramtable[i]);
				if (paraminfoptr->pmode == P_TEMP) {
					/*  Move any parameters evaluated to temporaries to their real slot */
					/* and free the temporaries NO: bug 745 - must free later! 2/10/90 */
					DEBUG_MSG (("mapinstance: %s, param %d moved to real slot\n", WNameOf (translate_from_internal (NNameOf (nptr))),	/* MDP */
						    i));
					upusecount (*(paraminfoptr->pparamexp), 1);
				}
				if (!paraminfoptr->pevaluated)
					mapparameter (paraminfoptr, i, FALSE);
			}
			/*}}} */
		}

		/* We used to allocate param slots here, but that is too late.
		   they've now been moved forward a bit.
		   bug 1013 11/10/90
		 */

		/*{{{  load register parameters */
		if (recursive || forked || dyncall) {
			/* load them one-at-a-time, since we'll be storing in the other workspace */
			if (nparams >= 3) {
				mapexpopd (paramtable[2].pmode, paramtable[2].pparamexp);
			}
			if (nparams >= 2) {
				mapexpopd (paramtable[1].pmode, paramtable[1].pparamexp);
			}
			mapexpopd (paramtable[0].pmode, paramtable[0].pparamexp);
		} else {
			if (nparams >= 3) {
				SetILoadSeq (tptr, mapload3regs (paramtable[2].pmode, paramtable[2].pparamexp,
								 paramtable[1].pmode, paramtable[1].pparamexp, paramtable[0].pmode, paramtable[0].pparamexp));
			} else if (nparams == 2) {
				SetILoadSeq (tptr, mapload2regs (paramtable[1].pmode, paramtable[1].pparamexp, paramtable[0].pmode, paramtable[0].pparamexp));
			} else {		/* nparams == 1 */
				mapexpopd (paramtable[0].pmode, paramtable[0].pparamexp);
				SetILoadSeq (tptr, 1);
			}
		}
		/*}}} */
		/*{{{  free any temporaries holding register parameters */
		for (i = 0; i < MAXREGS; i++)
			if (freereg[i])
				freetemp (*(paramtable[i].pparamexp));
		/*}}} */
		/*{{{  free preevaluated, non-addressable param temps. */
		for (i = 0; i < nparams; i++) {
			int mode = paramtable[i].pmode;
			if ((mode == P_TEMPPTR) || ((i >= MAXREGS) && (mode == P_TEMP)))	/* bug 745 2/10/90 */
				freetemp (*(paramtable[i].pparamexp));
		}
		/*}}} */
		/*{{{  free low workspace used by parameters */
		if (lowwsp != NULL)
			kill_var (lowwsp);
		/*}}} */
		memfree (paramtable);
	}
	/*}}} */
	if (recursive || forked || dyncall) {
		SetIRPSlots (tptr, datasize);
		datasize = saved_datasize;
	}
	if (mprocact || forked || dyncall) {
		/* need low WS 0 to support GCALL return from MOBILE process */
		reservelowworkspace (1);
	}
/*insidealtguard = savedinsidealtguard; *//* bug 1159 - 15/2/91 */
#if 0
fprintf (stderr, "mapinstance done...: nparams = %d, recursive = %d, forked = %d, nptr =", nparams, recursive, forked);
printtreenl (stderr, 4, nptr);
fprintf (stderr, "mapinstance: tptr =");
printtreenl (stderr, 4, tptr);
fprintf (stderr, "mapinstance: formal param list =");
printtreenl (stderr, 4, NParamListOf (nptr));
fprintf (stderr, "mapinstance: actual param list =");
printtreenl (stderr, 4, IParamListOf (tptr));
#endif
}

/*}}}*/
/*{{{  PUBLIC void mapsuspend (treenode *tptr)*/
/*
 *	maps out a SUSPEND invocation
 */
PUBLIC void mapsuspend (treenode *tptr)
{
	reservelowworkspace (1);		/* could use a temporary */
	datasize = max_INT32 (datasize, DS_IO);
	return;
}
/*}}}*/
/*{{{  PRIVATE void tparamexpopd/trecparamexpopd*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tparamexpopd generates the operand (opdmode, opd) and stores it in
 *               local workspace 'paramslot'.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void tparamexpopd (const int opdmode, treenode * const opd, const BOOL copy, const int paramslot)
{
	gencomment0 ("{{{  parameter");
	if (has_fpu_core && opdmode == P_EXP && isreal (ntypeof (opd))
			&& !isaddressable (opd)) {	/* bug TS/1196 15/12/92 - take this out; go via the integer stack */

		tfpexp (opd, MANY_REGS, MANY_REGS);
		genprimary (I_LDLP, paramslot);
		gensecondary (I_FPSTNLSN);
	} else {
		if (!copy || ((TagOf (opd) == T_PREEVALTEMP) && (TagOf (NDeclOf (opd)) == S_HIDDEN_PARAM))) {
			texpopd_main (opdmode, opd, MANY_REGS, FALSE);
			genprimary (I_STL, paramslot);
		} else {
			int skiplab = newlab ();
			
			if (isdynmobilearray (opd)) {
				genprimary (I_LDL, paramslot + 1);
			} else {
				texpopd_main (opdmode, opd, MANY_REGS, FALSE);
			}

			gensecondary (I_DUP);
			genbranch (I_CJ, skiplab);

			if (isdynmobilearray (opd)) {
				genprimary (I_LDNL, 0);
			} else {
				gensecondary (I_MT_CLONE);
			}
			
			setlab (skiplab);
			genprimary (I_STL, paramslot);
			throw_the_result_away ();
		}
		if (ispointer (opd)) {
			gencomment0 ("PTR");
		}
	}
	gencomment0 ("}}}");
}
/*{{{  comment*/
/*****************************************************************************
 *
 *  trecparamexpopd generates the operand (opdmode, opd) and stores it in
 *                  dynamic workspace 'paramslot', pointed at by local
 *                  workspace offset RECURSIVE_WS.
 *                  if `isrec' is FALSE, this a FORKed instance parameter.
 *                  FORKed instances also run with "must copy VAL" indicator and
 *                  a pointer to store a workspace offset in "fork_freeslot".
 *                  "formaltype" has the type-tree for the formal.
 *  updated (frmb): tidied up "isrec" parameter into a more general "ptype"
 *                  bitfield of PROC_{NONE,REC,FORKED,MPA,DYNCALL}
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void trecstoreparam (const int type, const int slot, const BOOL copy, const BOOL move)
{
	if (type & PROC_FORKED) {
		genprimary (I_LDL, FORK_SETUP_TEMP);
		loadconstant (slot);
		if (copy) {
			gensecondary (I_PROC_MT_COPY);
		} else if (move) {
			gensecondary (I_PROC_MT_MOVE);
		} else {
			gensecondary (I_PROC_PARAM);
		}
	} else {
		if (copy) {
			const int skiplab = newlab ();
			
			gensecondary (I_DUP);
			genbranch (I_CJ, skiplab);
			gensecondary (I_MT_CLONE);
			setlab (skiplab);
		}

		if (type & PROC_REC) {
			genprimary (I_LDL, RECURSIVE_WS);
		} else if (type & PROC_MPA) {
			genprimary (I_LDL, MPA_SETUP_TEMP);
		} else if (type & PROC_DYNCALL) {
			genprimary (I_LDL, DYNCALL_SETUP_TEMP_WS);
		} else {
			geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "trecstoreparam");
		}

		genprimary (I_STNL, slot);

		throw_the_result_away ();
	}
}

PRIVATE void trecparamexpopd (const int opdmode, treenode *const opd, const BOOL copy, const int paramslot, const int ptype, treenode *formaltype, const BOOL do_copy_val, const BOOL need_deref)
{
	static int hps_opdmode[32];
	static treenode *hps_opd[32];
	static int hps_ptr = 0;

	gencomment0 ("{{{  fork/recursive parameter");

	if (has_fpu_core && (opdmode == P_EXP) && isreal (ntypeof (opd)) && !isaddressable (opd)) {
		tfpexp (opd, MANY_REGS, MANY_REGS);
		if (ptype & PROC_FORKED) {
			genprimary (I_LDLP, FORK_SETUP_TEMPVAL);
			gensecondary (I_FPSTNLSN);
			genprimary (I_LDL, FORK_SETUP_TEMPVAL);
			trecstoreparam (ptype, paramslot, FALSE, FALSE);
		} else {
			if (ptype & PROC_REC) {
				genprimary (I_LDL, RECURSIVE_WS);
			} else if (ptype & PROC_DYNCALL) {
				genprimary (I_LDL, DYNCALL_SETUP_TEMP_WS);
			} else if (ptype & PROC_MPA) {
				genprimary (I_LDL, MPA_SETUP_TEMP);
			}
			genprimary (I_LDNLP, paramslot);
			gensecondary (I_FPSTNLSN);
		}
	} else {
#if 0
if (ptype & PROC_MPA) {
fprintf (stderr, "gen8: trecparamexpopd (MPA): evaluating parameter for mode=%s, paramslot=%d, opd=", (opdmode == P_EXP) ? "EXP" : ((opdmode == P_PTR ? "PTR" : "??")), paramslot);
printtreenl (stderr, 4, opd);
fprintf (stderr, "gen8: trecparamexpopd (MPA): do_copy_val is %d.  formal type is:", do_copy_val);
printtreenl (stderr, 4, formaltype);
}
#endif
		if (TagOf (opd) == S_PARAM_VSP) {
			/*{{{  load vectorspace from RECURSIVE_VS/FORK_SETUP_TEMP_VS*/
			if (ptype & PROC_FORKED) {
				genprimary (I_LDLP, FORK_SETUP_TEMP_VS);
			} else if (ptype & PROC_REC) {
				genprimary (I_LDL, RECURSIVE_VS);
			} else if (ptype & PROC_MPA) {
				genprimary (I_LDL, MPA_SETUP_TEMP_VS);
			} else if (ptype & PROC_DYNCALL) {
				genprimary (I_LDL, DYNCALL_SETUP_TEMP_VS);
			}
			trecstoreparam (ptype, paramslot, FALSE, TRUE);
			gencomment0 ("VSP");
			/*}}}*/
#ifdef MOBILES
		} else if ((TagOf (opd) == S_PARAM_MSP) && enable_mobilespace) {
			/*{{{  load mobilespace from RECURSIVE_MS/FORK_SETUP_TEMP_MS*/
			if (ptype & PROC_FORKED) {
				genprimary (I_LDLP, FORK_SETUP_TEMP_MS);
			} else if (ptype & PROC_DYNCALL) {
				/* should never happen! */
				gensecondary (I_NULL);
			} else if (ptype & PROC_REC) {
				genprimary (I_LDL, RECURSIVE_MS);
			} else if (ptype & PROC_MPA) {
				genprimary (I_LDL, MPA_SETUP_TEMP_MS);
			}
			trecstoreparam (ptype, paramslot, FALSE, TRUE);
			gencomment0 ("MSP");
			/*}}}*/
#endif
		} else if (TagOf (opd) == S_PARAM_STATICLINK) {
			/*{{{  better not forget this one..!  usage checker has made sure whatever's referenced is #PRAGMA SHARED*/
			if (ptype & PROC_MPA) {
				 geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "trecparamexpopd");
			}
			loadstaticlink (instancedlevel);
			trecstoreparam (ptype, paramslot, FALSE, FALSE);
			gencomment0 ("STATICLINK");
			/*}}}*/
		} else if ((ptype & PROC_FORKED) && (opdmode == P_PTR) && do_copy_val) {
			/*{{{  a VAL parameter which needs copying*/
			treenode *basetype = formaltype;
			BOOL carried = FALSE, stored = FALSE;

			while (TagOf (basetype) == S_ARRAY) {
				basetype = ARTypeOf (basetype);
			}
#if 0
fprintf (stderr, "gen8: trecparamexpopd(): VAL needs copying: , hps_ptr = %d, basetype =\n", hps_ptr);
printtreenl (stderr, 4, basetype);
fprintf (stderr, "gen8: trecparamexpopd(): VAL needs copying: formaltype =\n");
printtreenl (stderr, 4, formaltype);
#endif

			while (hps_ptr && (TagOf (formaltype) == S_ARRAY)) {
				if (ARDimOf (formaltype) == -1) {
					hps_ptr--;
					if (carried) {
						genprimary (I_STL, FORK_SETUP_TEMPVAL);
						carried = FALSE;
						stored = TRUE;
					}
					texpopd_main (hps_opdmode[hps_ptr], hps_opd[hps_ptr], MANY_REGS, FALSE);
				} else {
					genprimary (I_LDC, ARDimOf (formaltype));
				}
				if (stored) {
					genprimary (I_LDL, FORK_SETUP_TEMPVAL);
					carried = TRUE;
				}
				if (carried) {
					gensecondary (I_PROD);
				}
				carried = TRUE;
				formaltype = ARTypeOf (formaltype);
			}

			if (carried) {
				genprimary (I_STL, FORK_SETUP_TEMPVAL);
				stored = TRUE;
			}

#if 0
fprintf (stderr, "gen8: done size fiddling, stored = %d, carried = %d\n", stored, carried);
#endif
			/* load source pointer */
			texpopd_main (opdmode, opd, MANY_REGS, FALSE);

			/* load source size */
			if (stored) {
				genprimary (I_LDL, FORK_SETUP_TEMPVAL);
				if (bytesin (basetype) != 1) {
					loadconstant (bytesin (basetype));
					gensecondary (I_PROD);
				}
			} else {
				loadconstant (bytesin (formaltype));
			}
			/* load type */
			loadconstant (MT_SIMPLE | MT_MAKE_TYPE (MT_DATA));
			gensecondary (I_MT_DCLONE);

			/* move resulting data */
			genprimary (I_STL, FORK_SETUP_TEMPVAL);
			genprimary (I_LDLP, FORK_SETUP_TEMPVAL);
			trecstoreparam (ptype, paramslot, FALSE, TRUE);
			/*}}}*/
		} else if (ptype & PROC_FORKED) {
			/*{{{  fork parameters*/
			if ((TagOf (opd) == S_HIDDEN_PARAM)) {
				if (HDimensionOf (opd) == -1) {
					/* note: assumed tree structure here */
					if (copy) {
						loadmobile_real (LeftOpOf (HExpOf (opd)));
					} else {
						loadmobilepointer (LeftOpOf (HExpOf (opd)));
					}
					trecstoreparam (ptype, paramslot, copy, TRUE);
				} else {
					texpopd_main (opdmode, opd, MANY_REGS, FALSE);
					trecstoreparam (ptype, paramslot, FALSE, FALSE);
					if ((TagOf (formaltype) == S_INT)) {
						hps_opdmode[hps_ptr] = opdmode;
						hps_opd[hps_ptr] = opd;
						hps_ptr++;
					}
				}
			} else if (!(isdynmobilearray (opd) && isdynmobilearraytype (formaltype))) {
				BOOL move = isdynmobilechantype (opd) || isdynmobileproctype (opd);
				BOOL xcopy = copy || (!move && ismobile (opd));
				if (move && !xcopy) {
					loadmobilepointer (opd);
				} else {
					texpopd_main (opdmode, opd, MANY_REGS, FALSE);
				}
				trecstoreparam (ptype, paramslot, xcopy, move);
				hps_ptr = 0;
			} else {
				hps_ptr = 0;
			}
			/*}}}*/
		} else {
			/*{{{  regular parameter*/
			if (copy && isdynmobilearray (opd) && isdynmobilearraytype (formaltype)) {
				int skiplab = newlab ();

				if (ptype & PROC_REC) {
					genprimary (I_LDL, RECURSIVE_WS);
				} else if (ptype & PROC_DYNCALL) {
					genprimary (I_LDL, DYNCALL_SETUP_TEMP_WS);
				} else if (ptype & PROC_MPA) {
					genprimary (I_LDL, MPA_SETUP_TEMP);
				}

				genprimary (I_LDNL, paramslot + 1);
				gensecondary (I_DUP);
				genbranch (I_CJ, skiplab);
				genprimary (I_LDNL, 0);
				trecstoreparam (ptype, paramslot, FALSE, FALSE);
				setlab (skiplab);
				throw_the_result_away ();
				throw_the_result_away ();
			} else {
				texpopd_main (opdmode, opd, MANY_REGS, FALSE);
				if (need_deref) {
					/* fairly special -- used when passing CHAN parameters whose actual is a pointer-pointer and
					 * the formal is just pointer */
					genprimary (I_LDNL, 0);
				}
				trecstoreparam (ptype, paramslot, copy, FALSE);
			}
			if (ispointer (opd)) {
				gencomment0 ("PTR");
			}
			/*}}}*/
		}
	}
	gencomment0 ("}}}");
}
/*}}}*/
/*{{{  PUBLIC INT32 get_altadjust*/
PUBLIC INT32 get_altadjust (treenode * const tptr)
{
	/* Inside an ALT guard, we have to pull wptr down below the ALT below
	   workspace, to avoid overwriting alt instruction temporaries. */
	int adjustment;
	adjustment = insidealtguard ? max_INT32 (0, (int) (findinstances (tptr) - REG_PARAMS)) + alt_ds : 0;
	if (needs_quadalign)
		adjustment = (adjustment + 1) & (~1);
	return adjustment;
}

/*}}}*/
/*{{{  PRIVATE BOOL safe_fn_call*/
PRIVATE BOOL safe_fn_call (treenode * const nptr)
/* returns TRUE if this fn is a function which returns with 'clean'
   error flags; otherwise we must consider the state of the fpu
   and/or integer error flags.

   Currently, we can assume that any single-valued REAL function on
   a processor with an FPU will leave the fp error flag clear,
   but no other type of call will.
   All calls will leave the integer error flag clear.
*/
{
	if ((NTypeOf (nptr) != NULL) && (TagOf (NTypeOf (nptr)) == S_FNTYPE)) {
		treenode *const result_list = FnTypeListOf (NTypeOf (nptr));

		return (listitems (result_list) == 1) && isreal (TagOf (ThisItem (result_list)));
	}
	return FALSE;
}

/*}}}*/
/*{{{  PRIVATE void tpreevaltemp (paraminfo_t *const paraminfoptr, paraminfo_t *const prev)*/
PRIVATE void tpreevaltemp (paraminfo_t *const paraminfoptr, paraminfo_t *const prev)
{
	treenode *const exp = *(paraminfoptr->pparamexp);
	treenode *const decl = nodetypeoftag (TagOf (exp)) == NAMENODE ? NDeclOf (exp) : exp;
	/* This used to always simplify if preeval returned TRUE.
	   However, this breaks when the argument was turned into a temp
	   by something other than mapinstance.
	   So we re-create the same tests as were done when mapping */
	/* bug 1000 3/10/90 CON */
	
	if (preeval (paraminfoptr->pmode, exp) && ((paraminfoptr->pmaxparamslotused > 0) || preevalrealbyvalue (paraminfoptr))) {
		// DEBUG_MSG (("tinstance: simplifying param %d cos preeval/nested\n", i));
#if 0
fprintf (stderr, "tinstance: parameter %d about to be simplified because preeval()\n", i);
#endif
		/* new_occam_line (exp, TRUE, FALSE, FALSE); */
		paraminfoptr->pmode = simplify (paraminfoptr->pmode, exp);
		paraminfoptr->pevaluated = TRUE;
		/* we use the fact that the mode has been changed to 'TEMP' to
		   remember to copy the temp into the actual param slot later */
	}
#ifdef MOBILES	/* fudged SIZE <dynamic mobile> things miss out in the above check -- pmaxparamslotsused is -1 */
	else if (preeval (paraminfoptr->pmode, exp) && (TagOf (decl) == S_SIZE) && (isdynmobilearraypiece (OpOf (NDeclOf (exp))))) {
#if 0
fprintf (stderr, "tinstance: parameter %d looks like a dynamic mobile array size, like we fudged earlier.  doing the simplification thing\n", i);
#endif
		/* new_occam_line (exp, TRUE, FALSE, FALSE); */
		paraminfoptr->pmode = simplify (paraminfoptr->pmode, exp);
		paraminfoptr->pevaluated = TRUE;
	}
	else if (preeval (paraminfoptr->pmode, exp) && (TagOf (decl) == S_CLONE)) {
		loadmobile_real (OpOf (NDeclOf (exp)));
		gensecondary (I_MT_CLONE);
		storeinname (exp, 0);
		paraminfoptr->pmode = P_TEMP;
		paraminfoptr->pevaluated = TRUE;
		paraminfoptr->pclone = TRUE;
		temp_mark_as_evaluated (exp);
	}
	else if (preeval (paraminfoptr->pmode, exp) && (TagOf (decl) == S_HIDDEN_PARAM)) {
		treenode *const pexp = *(prev->pparamexp);

		loadname (pexp, 0);
		gensecondary (I_DUP);
		genprimary (I_LDNL, 0);
		storeinname (pexp, 0);
		storeinname (exp, 0);
		paraminfoptr->pmode = P_TEMP;
		paraminfoptr->pevaluated = TRUE;
		paraminfoptr->pclone = TRUE;
		prev->pclone = FALSE;
		temp_mark_as_evaluated (exp);
	} else if (preeval (paraminfoptr->pmode, exp) && (TagOf (decl) == S_ARRAYITEM) && ismobile (ASBaseOf (decl)) && (TagOf (ASBaseOf (decl)) == S_ARRAYITEM)) {
		paraminfoptr->pmode = simplify (paraminfoptr->pmode, exp);
		paraminfoptr->pevaluated = TRUE;
	}
#endif
}
/*}}}*/
/*{{{  PUBLIC void tinstance(tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tinstance generates code for procedure or function instance tptr
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void tinstance (treenode *tptr)
{
	treenode *const iname = INameOf (tptr);
	const int nparams = npparamsof (iname);
	const BOOL recursive = (nodetypeoftag (TagOf (tptr)) == INSTANCENODE) ? IRecursiveOf (tptr) : FALSE;
	const BOOL forked = (nodetypeoftag (TagOf (tptr)) == INSTANCENODE) ? IForkedOf (tptr) : FALSE;
	const BOOL dyncall = (nodetypeoftag (TagOf (tptr)) == INSTANCENODE) ? IDynmemOf (tptr) : FALSE;
	const BOOL mprocact = (nodetypeoftag (TagOf (tptr)) == INSTANCENODE) ? isdynmobileproctype (INameOf (tptr)) : FALSE;
	const int recparamslots = IRPSlotsOf (tptr);
	/* Inside an ALT guard, we have to pull wptr down below the ALT below
	   workspace, to avoid overwriting alt instruction temporaries. */
	const INT32 altadjust = get_altadjust (tptr);
	const int savedinsidealtguard = insidealtguard;
	const int oldinstancedlevel = instancedlevel;
	int alloc_ws_slots = 0;
	int alloc_vs_slots = 0;
#ifdef MOBILES
	int alloc_ms_slots = 0;
#endif
	const int forked_s_lab = (forked ? newlab () : -1);
	int vsp_param_slot = -255;	/* should be safe..! */
#ifdef MOBILES
	int msp_param_slot = -255;		/* equally so */
	treenode **pclone_list[nparams];	/* list of cloned parameter pointers */
	int mparam_offsets[nparams];		/* list of FORKed process mobile parameters which need freeing
						 * value is -255 if unused, parameter slot for the FORKed process otherwise
						 */
#endif
	paraminfo_t *paramtable;
	int fb_param_slot	= -255;
	int ptype		= PROC_NONE;
	int i, pass;

	gencomment0 ("tinstance():");
	if (forked) {
		ptype |= PROC_FORKED;
	}
	if (recursive) {
		ptype |= PROC_REC;
	}
	if (dyncall) {
		ptype |= PROC_DYNCALL;
	}
	if (mprocact) {
		ptype |= PROC_MPA;
	}
	paramtable = NULL;
	/* instancedlevel is global variable containing
	   level of variables within instanced routine */
	instancedlevel = NLexLevelOf (iname) + 1;
	insidealtguard = FALSE;	/* Nested functions don't adjust wptr */
	/*{{{  COMMENT  wsp optimisation */
	/**********************  Start comment out ****************************
	@*{{{  adjust workspace downwards if necessary*@
	if (params_below_ws > 0)
	{
		genprimary(I_AJW, -params_below_ws);
		adjustworkspace(params_below_ws);
	}
	@*}}}*@
	 **********************   End comment out  ****************************/
	/*}}} */
	/*{{{  adjust wptr down if we are in an alt guard */
	genprimary (I_AJW, -altadjust);
	adjustworkspace (altadjust);
	new_occam_line (tptr, TRUE, altadjust != 0, FALSE);
	/* if (altadjust != 0 && debugoutput)
	   coder_genlocate(tptr, TRUE); */
	/*}}} */
	/*{{{  update the profiling count */
	if (cgraph_profiling) {
		ProfTabEntry *entry = get_proftab_entry (profile_table, tptr);
		if (entry != NULL) {
			tprofcountupdate (proftab_entry_number_ (entry));
			proftab_calling_nptr_ (entry) = current_routine_nptr;
		}
	}
	/*}}} */
	/*{{{  allocate memory and local setup for forked/recursive/dyncall/mprocact stuffs*/
	if ((ptype != PROC_NONE) && disable_dynmem) {
		generr (GEN_NO_DYNMEM);
	} else if (ptype & PROC_FORKED) {
		/*{{{  allocate memory for FORKed process*/
		INT32 proc_ws, proc_vs;
#ifdef MOBILES
		INT32 proc_ms;

		getprocwsandvsandms (iname, &proc_ws, &proc_vs, &proc_ms);
		if ((!enable_mobilespace) && (proc_ms != 0)) {
			generr (GEN_NO_MOBILESPACE);
		}
#else

		getprocwsandvs (iname, &proc_ws, &proc_vs);
#endif

		/*{{{  get memory requirements*/
		if (separatelycompiled (iname)) {
			alloc_ws_slots = (NPMaxwspOf (iname) + ((nparams < MAXREGS) ? MAXREGS : nparams) + proc_ws + MAXREGS) + 3;
		} else {
			alloc_ws_slots = (NPMaxwspOf (iname) + ((nparams < MAXREGS) ? MAXREGS : nparams) + proc_ws + MAXREGS) + 1;
		}
		alloc_vs_slots = proc_vs;
#ifdef MOBILES
		alloc_ms_slots = proc_ms;
#endif
#if 0
fprintf (stderr, "tinstance (forked): nparams = %d, alloc_ws_slots = %d, alloc_vs_slots = %d, alloc_ms_slots = %d, proc_ws = %d\n",
	nparams, alloc_ws_slots, alloc_vs_slots, alloc_ms_slots, proc_ws);
fprintf (stderr, "tinstance (forked): NPMaxwspOf(iname) = %d, recparamslots = %d, MAXREGS = %d, proc_ws = %d, proc_vs = %d, proc_ms = %d. tptr =",
		NPMaxwspOf(iname), recparamslots, MAXREGS, proc_ws, proc_vs, proc_ms);
printtreenl (stderr, 4, tptr);
#endif
		/*}}}*/
		/*{{{  allocate workspace and vectorspace*/
		#if 0
		genprimary (I_LDC, (alloc_ws_slots << 2));
		gensecondary (I_MALLOC);
		genprimary (I_ADC, ((alloc_ws_slots - head_offset) << 2));	/* point at top element */
		#endif
		loadconstant (alloc_ws_slots);
		loadconstant (0); /* flags */
		gensecondary (I_PROC_ALLOC);
		genprimary (I_STL, FORK_SETUP_TEMP);

		if (alloc_vs_slots) {
			genprimary (I_LDC, (alloc_vs_slots << WSH));
			gensecondary (I_MALLOC);
			genprimary (I_STL, FORK_SETUP_TEMP_VS);
		}
		/*}}}*/
#ifdef MOBILES
		/*{{{  if mobilespace required, sort out FORK_SETUP_TEMP_MS */
		if (alloc_ms_slots) {
			const int amslab = newlab ();
			const int smslab = newlab ();

			/* test to see if any blocks here */
			loadnewmsp (0);
			genprimary (I_LDNL, IRMSPOffsetOf (tptr));		/* load the pointer from mobilespace -- MINT if empty */
			gensecondary (I_MINT);
			gensecondary (I_DIFF);
			genbranch (I_CJ, amslab);
			throw_the_result_away ();

			/* already here, reload, store in FORK_SETUP_TEMP_MS and put follow-on pointer back in mobilespace */
			loadnewmsp (0);
			genprimary (I_LDNL, IRMSPOffsetOf (tptr));		/* load mobilespace ptr */
			gensecondary (I_DUP);
			genprimary (I_STL, FORK_SETUP_TEMP_MS);			/* store in temp MS slot */
			genprimary (I_LDNL, -1);				/* load the word at offset -1 (magic link field) */
			gencomment0 ("FORK\'d mobilespace link field");
			loadnewmsp (0);
			genprimary (I_STNL, IRMSPOffsetOf (tptr));		/* stuff back in mobilespace */
			genbranch (I_J, smslab);

			/* empty, allocate block */
			setlab (amslab);
			throw_the_result_away ();
			/* frmb (09/01/2005): new allocator doesn't necessarily have this extra word..! */
			genprimary (I_LDC, ((alloc_ms_slots + 1) * bytesperword));
			gensecondary (I_MALLOC);
			genprimary (I_LDC, 4);
			gensecondary (I_SUM);					/* offset by 4 */
			genprimary (I_STL, FORK_SETUP_TEMP_MS);			/* store in temp MS slot */
			/* put MINT in word 0 */
			gensecondary (I_MINT);
			genprimary (I_LDL, FORK_SETUP_TEMP_MS);
			genprimary (I_STNL, 0);

			/* put address of mobilespace slot in word -1, needed to put back on the "queue" later */
			setlab (smslab);
			loadnewmsp (0);
			genprimary (I_LDNLP, IRMSPOffsetOf (tptr));		/* load pointer to mobilespace word */
			genprimary (I_LDL, FORK_SETUP_TEMP_MS);
			genprimary (I_STNL, -1);				/* store in "link" field */
		}
		/*}}}*/
#endif	/* MOBILES */
		/*{{{  augment params here */
#if 0
fprintf (stderr, "tinstance: calling augmentparams()\n");
#endif
		SetIParamList (tptr, augmentparams (IParamListOf (tptr), NParamListOf (iname), NULL, tptr));
		/*}}}*/
		/*}}}*/
	} else if (ptype & PROC_DYNCALL) {
		/*{{{  allocate memory for dynamically called process (similar to RECURSIVE, except sizes loaded via name)*/
		/*{{{  find base address and put in local temporary*/
		char tmpname[128];
		int vsskiplab = newlab ();
		int tcskiplab = newlab ();
		treenode *nplist = NParamListOf (iname);
		treenode **savep_vsp = NULL;
		treenode *save_vsp = NULL;
		unsigned int thash;
		
		new_occam_line (tptr, TRUE, TRUE, FALSE);
		/* if the parameter list has a vectorspace pointer in it, remove for purposes of typehash generation */
		for (savep_vsp = &nplist; savep_vsp && !EndOfList (*savep_vsp); savep_vsp = NextItemAddr (*savep_vsp)) {
			save_vsp = ThisItem (*savep_vsp);

			if (TagOf (save_vsp) == S_PARAM_VSP) {
				break;		/* for() */
			}
		}
		if (savep_vsp && EndOfList (*savep_vsp)) {
			savep_vsp = NULL;
		}
		
		if (savep_vsp) {
			save_vsp = *savep_vsp;
			*savep_vsp = NextItem (save_vsp);
		}
		thash = typehash (nplist);
		if (savep_vsp) {
			*savep_vsp = save_vsp;
		}
#if 0
fprintf (stderr, "tinstance: dynamic call to [%s], typehash 0x%8.8X, paramlist:", WNameOf (NNameOf (iname)), thash);
printtreenl (stderr, 4, nplist);
#endif

		if (IDynaddrOf (tptr)) {
			texpopd (P_EXP, IDynaddrOf (tptr), MANY_REGS);
		} else {
			snprintf (tmpname, 127, "DCR_%s", WNameOf (NNameOf (iname)));
			genloadnamedlabptr (tmpname);
		}
		genprimary (I_STL, DYNCALL_SETUP_TEMP_NAME);

		alloc_ws_slots = 0;			/* meaningless here */
		alloc_vs_slots = 0;
		alloc_ms_slots = 0;

		/*}}}*/
		/*{{{  check that the type-hash matches (interface check)*/
		genprimary (I_LDL, DYNCALL_SETUP_TEMP_NAME);
		genprimary (I_LDNL, 3);					/* param typehash */
		loadconstant (thash);
		gensecondary (I_DIFF);
		genbranch (I_CJ, tcskiplab);
		throw_the_result_away ();
		gensecondary (I_SETERR);

		setlab (tcskiplab);

		/*}}}*/
		/*{{{  allocate workspace and vectorspace*/
		genprimary (I_LDL, DYNCALL_SETUP_TEMP_NAME);
		genprimary (I_LDNL, 1);					/* WS slots */
		genprimary (I_ADC, ((nparams < MAXREGS) ? MAXREGS : nparams) + MAXREGS + MIN_DYNCALL_SLOTS);	/* extra slots for params + state */
		loadconstant (4);
		gensecondary (I_PROD);
		gensecondary (I_MALLOC);
		genprimary (I_STL, DYNCALL_SETUP_TEMP_WSBASE);

		/* advance to base */
		genprimary (I_LDL, DYNCALL_SETUP_TEMP_NAME);
		genprimary (I_LDNL, 1);					/* WS slots */
		genprimary (I_ADC, ((nparams < MAXREGS) ? MAXREGS : nparams) + MAXREGS + MIN_DYNCALL_SLOTS - 1);	/* extra slots for params + state */
		loadconstant (4);
		gensecondary (I_PROD);
		genprimary (I_LDL, DYNCALL_SETUP_TEMP_WSBASE);
		gensecondary (I_SUM);					/* add to base addr */
		genprimary (I_STL, DYNCALL_SETUP_TEMP_WS);

		/* do vectorspace */
		loadconstant (0);					/* assume zero */
		genprimary (I_STL, DYNCALL_SETUP_TEMP_VS);

		genprimary (I_LDL, DYNCALL_SETUP_TEMP_NAME);
		genprimary (I_LDNL, 2);					/* VS slots */
		genbranch (I_CJ, vsskiplab);
		throw_the_result_away ();
		genprimary (I_LDL, DYNCALL_SETUP_TEMP_NAME);
		genprimary (I_LDNL, 2);					/* VS slots */
		loadconstant (4);
		gensecondary (I_PROD);
		gensecondary (I_MALLOC);
		genprimary (I_STL, DYNCALL_SETUP_TEMP_VS);

		setlab (vsskiplab);
		
		/*}}}*/

		/*{{{  augment params here */
#if 0
fprintf (stderr, "tinstance: calling augmentparams()\n");
#endif
		SetIParamList (tptr, augmentparams (IParamListOf (tptr), NParamListOf (iname), NULL, tptr));
		/*}}}*/
		/*}}}*/
	} else if (ptype & PROC_REC) {
		/*{{{  allocate memory for recursive process*/
		/*{{{  get memory requirements*/
		alloc_ws_slots = (NPMaxwspOf (iname) + ((recparamslots < MAXREGS) ? MAXREGS : recparamslots)+ NPDatasizeOf (iname) + MAXREGS);
		alloc_vs_slots = NPVSUsageOf (iname);
#ifdef MOBILES
		alloc_ms_slots = NPMSUsageOf (iname);
		if ((!enable_mobilespace) && (alloc_ms_slots != 0)) {
			generr (GEN_NO_MOBILESPACE);
		}
#endif
#if 0
fprintf (stderr, "tinstance: alloc_ws_slots = %d, alloc_vs_slots = %d, alloc_ms_slots = %d, NPDatasizeOf(iname) = %d\n",
	alloc_ws_slots, alloc_vs_slots, alloc_ms_slots, (int)NPDatasizeOf (iname));
#endif
		/*}}}*/
		/*{{{  allocate workspace and vectorspace*/
		/* oki, allocate */
		genprimary (I_LDC, (alloc_ws_slots << WSH));
		gensecondary (I_MALLOC);
		genprimary (I_ADC, ((alloc_ws_slots - 1) << WSH));	/* point at top element */
		genprimary (I_STL, RECURSIVE_WS);
		if (alloc_vs_slots) {
			genprimary (I_LDC, (alloc_vs_slots << WSH));
			gensecondary (I_MALLOC);
			genprimary (I_STL, RECURSIVE_VS);
		}
		/*}}}*/
		/*{{{  save current Wptr in new workspace*/
		/* stuff current Wptr into new workspace */
		genprimary (I_LDLP, 0);
		genprimary (I_LDL, RECURSIVE_WS);
		genprimary (I_STNL, RECURSIVE_SAVED_WS);
		/*}}}*/
		/*{{{  augment params here if vs/ms needed*/
		/* for VS/WS parameters will not be augmented yet. */
		{
#if 0
fprintf (stderr, "tinstance: calling augmentparams()\n");
#endif
			SetIParamList (tptr, augmentparams (IParamListOf (tptr), NParamListOf (iname), NULL, tptr));
		}
		/*}}}*/
		/*{{{  mobilespace handling for recursive PROCs*/
		/* need to have augmented params before attempting to handle mobile-space */
#ifdef MOBILES
		if (alloc_ms_slots) {
			int skiplab = newlab ();
			int alloclab = newlab ();
			treenode *msp_ptr, *aargs;

			/* maybe already here ? */
#if 0
fprintf (stderr, "tinstance: yikes, hope we mapped out mobilespace already..\n");
#endif
			/* find MSP parameter to recursive call */
			aargs = IParamListOf (tptr);
			msp_ptr = NULL;
			while (!EndOfList (aargs)) {
				if (TagOf (ThisItem (aargs)) == S_PARAM_MSP) {
					msp_ptr = ThisItem (aargs);
				}
				aargs = NextItem (aargs);
			}
			if (msp_ptr) {
				/* produce pointer to mobilespace slot */
				loadnewmsp (0);
				genprimary (I_LDNL, IRMSPOffsetOf (tptr));
				gencomment0 ("PTR MSP");
				gensecondary (I_MINT);
				gensecondary (I_DIFF);
				genbranch (I_CJ, alloclab);
				throw_the_result_away ();

				/* already here, load it into RECURSIVE_MS and skip allocation code */
				loadnewmsp (0);
				genprimary (I_LDNL, IRMSPOffsetOf (tptr));
				gencomment0 ("PTR MSP");
				genprimary (I_STL, RECURSIVE_MS);
				genbranch (I_J, skiplab);

				/* better allocate */
				setlab (alloclab);
#if 0
fprintf (stderr, "tinstance: looks like we need to allocate %d slots for recursive mobile-space.  aptr for MSP is", alloc_ms_slots);
printtreenl (stderr, 4, msp_ptr);
#endif
				genprimary (I_LDC, (alloc_ms_slots << 2));
				gensecondary (I_MALLOC);
				/* store MINT into first allocated word, causes initialisation of mobilespace */
				genprimary (I_STL, RECURSIVE_MS);	/* temporary */
				gensecondary (I_MINT);
				genprimary (I_LDL, RECURSIVE_MS);
				genprimary (I_STNL, 0);

				/* store allocated pointer in mobile-space */
				genprimary (I_LDL, RECURSIVE_MS);
				gencomment0 ("PTR MSP");
				loadnewmsp (0);
				genprimary (I_STNL, IRMSPOffsetOf (tptr));		/* store allocated space in mobilespace slot (IRSMPOffset) */
				setlab (skiplab);
				throw_the_result_away ();
			}
		}
#endif
		/*}}}*/
		/*}}}*/
	} else if (ptype & PROC_MPA) {
		/*{{{  setup memory for mobile process activation*/
		/* load pointer to desciptor block (instance name) */
		loadmobile (iname);
		genchecknotnull ();
		genprimary (I_LDNL, MPP_WSSIZE);
		genprimary (I_ADC, -(2 * bytesperword));
		genprimary (I_STL, MPA_SETUP_TEMP);
		loadmobile (iname);
		genprimary (I_LDNL, MPP_WSBASE);
		genprimary (I_LDL, MPA_SETUP_TEMP);
		gensecondary (I_SUM);
		genprimary (I_STL, MPA_SETUP_TEMP);

		/* setup vectorspace */
		loadmobile (iname);
		genprimary (I_LDNL, MPP_VSBASE);
		genprimary (I_STL, MPA_SETUP_TEMP_VS);

#ifdef MOBILES
		/* setup mobilespace */
		if (enable_mobilespace) {
			loadmobile (iname);
			genprimary (I_LDNL, MPP_MSBASE);
			genprimary (I_STL, MPA_SETUP_TEMP_MS);
			gencomment0 ("MPP allocation");
		}
#endif
		/*}}}*/
	}
	/*}}}*/
	/*{{{  load parameters */

	if (nparams > 0) {
		paramtable = (paraminfo_t *) memalloc (sizeof (paraminfo_t) * nparams);
		loadparamtable (ptype, paramtable, nparams, tptr);
	} else {
		paramtable = NULL;
	}
#ifdef MOBILES
	/*{{{  adjust any CLONEd parameters.  (only CLONEs left here are channel-types _AND_ dynamic MOBILEs).
	 * the original reference is stored in pclone_list.
	 */
	for (i = 0; i < nparams; i++) {
		pclone_list[i] = NULL;
		mparam_offsets[i] = -255;

		if ((TagOf (*(paramtable[i].pparamexp)) == S_CLONE)) {
			/*{{{  found one, save in list, adjust code-gen expression and generate code to up usage-count of chantype semaphore*/
			treenode *nptr;

			pclone_list[i] = paramtable[i].pparamexp;
			paramtable[i].pparamexp = OpAddr (*(paramtable[i].pparamexp));
			nptr = *(paramtable[i].pparamexp);

			if (isdynmobilechantype (nptr)) {
				paramtable[i].pclone = TRUE;
			} else if (isdynmobilearray (nptr) && isdynmobilearraytype (paramtable[i].pformaltype)) {
				if (ptype & PROC_FORKED) {
					paramtable[i+1].pclone = TRUE; /* hidden pointer */
				} else {
					paramtable[i].pclone = TRUE;
				}
			}
			/*}}}*/
		} else if ((ptype & PROC_FORKED) && paramtable[i].forkbarrier) {
			paramtable[i].pclone = TRUE;
		}
	}
	/*}}}*/
#endif
	/*{{{  check actual parameter dimensions */
	for (i = 0; i < nparams; i++) {
		treenode *ftype = paramtable[i].pformaltype;
		treenode *aparam = *(paramtable[i].pparamexp);
		treenode *atype = gettype ((TagOf (aparam) == S_FNACTUALRESULT) ? HExpOf (aparam) : aparam);
#if 0
fprintf (stderr, "tinstance: recursive=%d, recparamslots=%d, pmode=%s, ftype is", recursive, recparamslots, opdmode_string (paramtable[i].pmode));
printtreenl (stderr, 4, ftype);
fprintf (stderr, "  atype is");
printtreenl (stderr, 4, atype);
fprintf (stderr, "  actual param is");
printtreenl (stderr, 4, *(paramtable[i].pparamexp));
#endif
		while (TagOf (ftype) == S_ARRAY) {
			if (ARDimOf (atype) == (-1) && ARDimOf (ftype) != (-1)) {
				tcheckdim (ftype, atype);
			}
			ftype = ARTypeOf (ftype);
			atype = ARTypeOf (atype);
		}
	}
	/*}}} */
	/*{{{  preevaluate non-addressable params passed by reference, into temps. */
	/* Preevaluate non-addressable params passed by reference :
	   sounds silly, but does actually occur for INT64 expressions,
	   array constructors etc. */
	for (i = 0; i < nparams; i++) {
		if (paramtable[i].pmode == P_TEMPPTR) {
			/*{{{  evaluate to temporary */
			DEBUG_MSG (("tinstance: simplifying param %d cos P_TEMPPTR\n", i));
			/* new_occam_line (*(paramtable[i].pparamexp), TRUE, FALSE, FALSE); */
			simplify (P_EXP, *(paramtable[i].pparamexp));
			/* We don't set 'pevaluated' because we have yet
			   to load a pointer to the temporary */
			/*}}} */
		}
	}
	/*}}} */
#if 0
fprintf (stderr, "tinstance: nparams = %d:\n", nparams);
{ int i;
for (i=0; i<nparams; i++) {
fprintf (stderr, "--> parameter %d =", i);
printtreenl (stderr, 4, *(paramtable[i].pparamexp));
} }
#endif
	/*{{{  preevaluate any parameters requiring temporaries */
	for (i = 0; (i < nparams) && (i < MAXREGS); i++) {	/* register params */
		tpreevaltemp (&(paramtable[i]), &(paramtable[i - 1]));
	}
	/* modified 6/12/90 to ensure that these are done in the same order as
	   they were done while mapping - bug 1064 6/12/90 */
	for (i = nparams - 1; i >= MAXREGS; i--) {	/* non register params */
		tpreevaltemp (&(paramtable[i]), &(paramtable[i - 1]));
	}
	/*}}} */
	/*{{{  evaluate non-register parameters containing function calls to param slots */
	for (pass = 0; pass < 4; pass++) {
		for (i = nparams - 1; i >= 0; i--) {
			paraminfo_t *const paraminfoptr = &(paramtable[i]);
			int slot;

			/*{{{  calculate or load slot*/
			if (pass == 0) {
				slot = i - ((nparams > REG_PARAMS) ? nparams : REG_PARAMS);

				if (ptype & PROC_FORKED) {
					/* slot = ((1 - MIN_FORK_SLOTS) - nparams) + i; */
					slot += (alloc_ws_slots - MIN_FORK_SLOTS);
				} else if (ptype & PROC_DYNCALL) {
					slot += (1 - MIN_DYNCALL_SLOTS);
				} else if (ptype & PROC_REC) {
					slot += (1 - MIN_RECURSIVE_SLOTS);
				} else if (ptype & PROC_MPA) {
					slot += (1 - MIN_MPA_SLOTS);
				} else {
					slot = i - REG_PARAMS;
				}

				switch (TagOf (*(paraminfoptr->pparamexp))) {
					case S_PARAM_VSP: 
						vsp_param_slot = slot; 
						break;
					case S_PARAM_MSP: 
						msp_param_slot = slot; 
						break;
					case S_PARAM_FB:
						fb_param_slot = slot;
						break;
					default: 
						if (paraminfoptr->forkbarrier) {
							fb_param_slot = slot;
						}
						break;
				}

				paraminfoptr->pslot = slot;
			} else {
				slot = paraminfoptr->pslot;
			}
			/*}}}*/

			if ((ptype == PROC_NONE) && (i < REG_PARAMS)) {
				continue;
			}
			
			if ((pass == 1) && !paraminfoptr->pevaluated && paraminfoptr->pnestedfunctions) {
				DEBUG_MSG (("tinstance: doing param %d (nestedfunctions)\n", i));
				if (ptype != PROC_NONE) {
					trecparamexpopd (
						paraminfoptr->pmode, 
						*(paraminfoptr->pparamexp),
						paraminfoptr->pclone,
						slot, 
						ptype, 
						paraminfoptr->pformaltype, 
						paraminfoptr->pvalparam,
						paraminfoptr->need_deref
					);
				} else {
					tparamexpopd (
						paraminfoptr->pmode,
						*(paraminfoptr->pparamexp),
						paraminfoptr->pclone,
						slot
					);
				}
				paraminfoptr->pevaluated = TRUE;
			} else if ((pass == 2) && (!paraminfoptr->pevaluated || (paraminfoptr->pmode == P_TEMP))) {
				treenode *exp = *(paraminfoptr->pparamexp);

				DEBUG_MSG (("tinstance: doing param %d\n", i));
#if 0
fprintf (stderr, "tinstance: evaluating non-register param %d, exp = ", i);
printtreenl (stderr, 4, exp);
gencomment1 ("gen non-register param %d..", i);
#endif
				paraminfoptr->pmode = simplify (paraminfoptr->pmode, exp);	/* bug 1000 3/10/90 */
				if (ptype != PROC_NONE) {
					trecparamexpopd (
						paraminfoptr->pmode,
						exp,
						paraminfoptr->pclone,
						slot,
						ptype,
						paraminfoptr->pformaltype,
						paraminfoptr->pvalparam,
						paraminfoptr->need_deref
					);
				} else {
					tparamexpopd (
						paraminfoptr->pmode, 
						exp,
						paraminfoptr->pclone, 
						slot
					);
				}
				paraminfoptr->pevaluated = TRUE;
			} else if ((pass == 3) && (ptype & PROC_FORKED)) {
				treenode *exp = *(paraminfoptr->pparamexp);
				BOOL cloned = paraminfoptr->pclone;
				#ifdef MOBILES
				/*{{{  if we're passing a MOBILE as a parameter to a FORKed thing, clear the local reference (dynamic MOBILEs)*/
				if (isdynmobilechantype (exp) && !cloned) {
					gencleardynchantype (exp);
					mparam_offsets[i] = slot;
				} else if (isdynmobilechantype (exp)) {
					/* stays local, cleaned up through pclone_list */
					mparam_offsets[i] = slot;
				} else if (isdynmobilebarrier (exp)) {
					/* process has already been enrolled on the barrier -- just remember it here */
					mparam_offsets[i] = slot;
				} else if (isdynmobileproctype (exp) && !cloned) {
					gencleardynproctype (exp);
					mparam_offsets[i] = slot;
				} else if (isdynmobilearray (exp) && isdynmobilearraytype (paraminfoptr->pformaltype)) {
					/* Check for cloning in the next parameter, 
					 * the hidden real pointer to the array.
					 */
					if (!paramtable[i+1].pclone) {
						gencleardynarray (exp, FALSE);
						mparam_offsets[i] = slot;
					}
				} else if (isdynmobilearray (exp)) {
					/* skip */
				} else if (ismobile (exp)) {
					/* CGR FIXME: xxx */
					generr (GEN_NO_FMPARAM);
				}
				/*}}}*/
				#endif
			}
		}
	}
	/*}}} */
	/*{{{  add a fork barrier if required*/
	if ((ptype & PROC_FORKED) && (fb_param_slot == -255)) {
		gencomment0 ("{{{  add hidden fork barrier");
#if 0
fprintf (stderr, "gen8/tinstance(): adding hidden fork barrier, IForkOf (tptr) @%p\n", IForkOf (tptr));
#endif
		if (IForkOf (tptr)) {
			loadname (DNameOf (CBodyOf (IForkOf (tptr))), 0);
		} else {
			loadfb ();
		}
		genprimary (I_LDL, FORK_SETUP_TEMP);
		loadconstant ((alloc_ws_slots - MIN_FORK_SLOTS) + FORK_BARRIER);
		gensecondary (I_PROC_MT_COPY);
		gencomment0 ("}}}");
	}
	/*}}}*/
	/*{{{  load register parameters */
	if (ptype == PROC_NONE) {
		DEBUG_MSG (("tinstance: doing register params\n"));
		if (nparams == 1) {
			/* new_occam_line (*(paramtable[0].pparamexp), TRUE, FALSE, FALSE); */
			texpopd_main (paramtable[0].pmode, *(paramtable[0].pparamexp), MANY_REGS, FALSE);
			if (paramtable[0].need_deref) {
				genindirect (INDIR_AREG);
			}
		} else if (nparams == 2) {
			/* new_occam_line (*(paramtable[1].pparamexp), TRUE, FALSE, FALSE); */
			tload2regs (paramtable[1].pmode, *(paramtable[1].pparamexp), paramtable[0].pmode, *(paramtable[0].pparamexp), FALSE, FALSE);
			if (paramtable[1].need_deref) {
				genindirect (INDIR_BREG);
			}
			if (paramtable[0].need_deref) {
				genindirect (INDIR_AREG);
			}
		} else if (nparams >= 3) {
#if 0
fprintf (stderr, "tinstance: evaluating register param %d, exp = ", 2);
printtreenl (stderr, 4, *(paramtable[2].pparamexp));
fprintf (stderr, "tinstance: evaluating register param %d, exp = ", 1);
printtreenl (stderr, 4, *(paramtable[1].pparamexp));
fprintf (stderr, "tinstance: evaluating register param %d, exp = ", 0);
printtreenl (stderr, 4, *(paramtable[0].pparamexp));
gencomment0 ("gen non-register param 2..0");
#endif
			/* new_occam_line (*(paramtable[2].pparamexp), TRUE, FALSE, FALSE); */
			tload3regs (paramtable[2].pmode, *(paramtable[2].pparamexp),
				    paramtable[1].pmode, *(paramtable[1].pparamexp),
				    paramtable[0].pmode, *(paramtable[0].pparamexp), ILoadSeqOf (tptr), FALSE);
			if (paramtable[2].need_deref) {
				genindirect (INDIR_CREG);
			}
			if (paramtable[1].need_deref) {
				genindirect (INDIR_BREG);
			}
			if (paramtable[0].need_deref) {
				genindirect (INDIR_AREG);
			}
		}
	}
	/*}}} */
	checkerror ();
		/*}}}*/
	
	/*{{{  call routine */
	if (ptype & PROC_FORKED) {
		/*{{{  FORKed routine*/
		/* const int l_lab = newlab (); */
		const int p_lab = newlab ();
		int offset = alloc_ws_slots - (((nparams > REG_PARAMS) ? nparams : REG_PARAMS) + MIN_FORK_SLOTS + 1);

		/* start new process */
		gencomment0 ("{{{  start forked process");
		genloadlabptr (p_lab, NOLAB, "PTR");
		/* genlabeldiff (I_LDC, p_lab, l_lab); */
		genprimary (I_LDL, FORK_SETUP_TEMP);
		/* genprimary (I_ADC, -((((nparams > MAXREGS) ? nparams : MAXREGS) + MIN_FORK_SLOTS) * bytesperword));	*/
		loadconstant (offset);
		gensecondary (I_PROC_START);
		genbranch (I_J, forked_s_lab);
		gencomment0 ("}}}");

		/* forked process */
		gencomment0 ("{{{  forked process entry-point");
		setlab (p_lab);

		/* unpack dynamic mobile arrays */
		for (i = nparams - 1; i >= 0; i--) {
			paraminfo_t *const paraminfoptr = &(paramtable[i]);
			treenode *exp = *(paraminfoptr->pparamexp);
			if (isdynmobilearray (exp) && isdynmobilearraytype (paraminfoptr->pformaltype)) {
				int skiplab = newlab ();
				int slot = paramtable[i].pslot - offset;
				gencomment0 ("{{{  unpack dynamic mobile array parameter");
				genprimary (I_LDL, slot + 1);
				gensecondary (I_DUP);
				genbranch (I_CJ, skiplab);
				genprimary (I_LDNL, 0);
				genprimary (I_STL, slot);
				setlab (skiplab);
				throw_the_result_away ();
				throw_the_result_away ();
				gencomment0 ("}}}");
			}
		}

		/* call real process */
		genloadlabptr (NPLabelOf (iname), NOLAB, "PTR");
		gensecondary (I_GCALL);
		throw_the_result_away ();
		gencomment0 ("}}}");
		/*}}}*/
	} else if (ptype & PROC_DYNCALL) {
		/*{{{  dynamically instantiated routine call*/
		gencomment0 ("{{{  switch workspace (dyncall)");
		genprimary (I_LDL, DYNCALL_SETUP_TEMP_WS);
		gensecondary (I_GAJW);
		genprimary (I_STL, DYNCALL_SAVED_WS);

		genprimary (I_LDL, DYNCALL_SAVED_WS);
		genprimary (I_LDNL, DYNCALL_SETUP_TEMP_NAME);
		genprimary (I_LDNL, 0);				/* function entry-point address */

		/* advance target workspace to the point just past the parameters */
		genprimary (I_AJW, -(((nparams > MAXREGS) ? nparams : MAXREGS) + MIN_DYNCALL_SLOTS));
		gensecondary (I_GCALL);
		throw_the_result_away ();

		/* when we get back, still in the allocated workspace */
		gencomment0 ("}}}");
		/*}}}*/
	} else if (ptype & PROC_REC) {
		/*{{{  setup new workspace (recursive call)*/
		gencomment0 ("{{{  switch workspaces");
		genprimary (I_LDL, RECURSIVE_WS);
		gensecondary (I_GAJW);
		throw_the_result_away ();
		/* genprimary (I_STL, RECURSIVE_SAVED_WS); */
		/* advance target workspace to the point just past the parameters */
		genprimary (I_AJW, -(((nparams > MAXREGS) ? nparams : MAXREGS) + MIN_RECURSIVE_SLOTS));
		genloadlabptr (NPLabelOf (iname), NOLAB, "PTR");
		gensecondary (I_GCALL);
		throw_the_result_away ();
		gencomment0 ("}}}");
		/*}}}*/
	} else if (ptype & PROC_MPA) {
		/*{{{  call mobile process activation */
		gencomment0 ("{{{  mobile process activation");
		loadmobile (iname);
		gensecondary (I_DUP);
		gensecondary (I_DUP);
		genprimary (I_LDNL, MPP_WPTR);
		gensecondary (I_GAJW);			/* switch to mobile process workspace, our WS left in Areg, block-ptrs left in Breg, Creg */
		gensecondary (I_REV);
		genprimary (I_STNL, MPP_WPTR);		/* store our workspace pointer */
		genprimary (I_LDNL, MPP_IPTR);		/* load activation address */
		if (mpp_check_at_act) {
			genchecknotnull ();
		}
		gensecondary (I_GCALL);
		throw_the_result_away ();
		gencomment0 ("}}}");
		/*}}}*/
	} else {
		/*{{{  regular CALL*/
		if (NPSuspendsOf (iname)) {
			/* need code-mapping */
			gencomment0 (".MAGIC DOSUBCODEMAP");
		}
		genbranch (I_CALL, NPLabelOf (iname));
		/*}}}*/
	}
	gencomments ("Call %s", WNameOf (translate_from_internal (NNameOf (iname))));	/* MDP */
	if (ptype & PROC_FORKED) {
		/*{{{  clean-up after FORK call (MOBILE params, workspace, vectorspace, mobilespace)*/
		/* const int ls_lab = newlab (); */
		/* const int lf_lab = newlab (); */
		int base_offset = (alloc_ws_slots - (((nparams > REG_PARAMS) ? nparams : REG_PARAMS) + MIN_FORK_SLOTS)) + 3;

#if 0
		if (separatelycompiled (iname)) {
			wsadjust -= 2;
		}
#endif
		/* setlab (ls_lab); */
		new_occam_line (tptr, TRUE, altadjust != 0, FALSE);

		/*{{{  recover any MOBILE parameters appropiately*/
		for (i = 0; i < nparams; i++) {
			treenode *exp = *(paramtable[i].pparamexp);
			int slot = paramtable[i].pslot - base_offset;

			if ((paramtable[i].pslot == fb_param_slot)
					|| isdynmobilechantype (exp) || isanychantype (exp) 
					|| isdynmobilebarrier (exp) || isdynmobilearray (exp)) {
				/*{{{  free dynamic mobile something */
				const int skiplab = newlab ();
				gencomment0 ("{{{ free dynamic MOBILE parameter (after FORK)");
				if (isdynmobilearray (exp) && !paramtable[i].pvalparam) {
					genprimary (I_LDL, slot + 1);
				} else {
					genprimary (I_LDL, slot);
				}
				gensecondary (I_DUP);
				genbranch (I_CJ, skiplab);
				gensecondary (I_MT_RELEASE);
				setlab (skiplab);
				throw_the_result_away ();
				throw_the_result_away ();
				gencomment0 ("}}}");
				/*}}}*/
			} else if (isdynmobileproctype (exp)) {
				/*{{{  maybe free mobile process*/
				const int skiplab = newlab ();
				const int skiplab2 = newlab ();
				
				gencomment0 ("{{{ free dynamic MOBILE PROC TYPE parameter (after FORK)");
				genprimary (I_LDL, slot);
				genbranch (I_CJ, skiplab);
				throw_the_result_away ();

				/* free workspace first */
				genprimary (I_LDL, slot);
				genprimary (I_LDNL, MPP_WSBASE);
				gensecondary (I_MRELEASE);

				/* if vectorspace, free that */
				genprimary (I_LDL, slot);
				genprimary (I_LDNL, MPP_VSBASE);
				gensecondary (I_DUP);
				genbranch (I_CJ, skiplab2);
				gensecondary (I_MRELEASE);
				setlab (skiplab2);
				throw_the_result_away ();

				/* FIXME: mobilespace */

				/* free block itself */
				genprimary (I_LDL, slot);
				gensecondary (I_MRELEASE);

				setlab (skiplab);
				gencomment0 ("}}}");
				/*}}}*/
			} else if (paramtable[i].pmode == P_PTR && paramtable[i].pvalparam) {
				const int skiplab = newlab ();

				gencomment0 ("{{{  free copied VAL parameter (after FORK)");
				genprimary (I_LDL, slot);
				gensecondary (I_DUP);
				genbranch (I_CJ, skiplab);
				gensecondary (I_MRELEASE);
				setlab (skiplab);
				throw_the_result_away ();
				throw_the_result_away ();
				gencomment0 ("}}}");
			}
		}
		/*}}}*/
		/*{{{  free FORKed vectorspace if used*/
		if ((vsp_param_slot > -255) && alloc_vs_slots) {
			genprimary (I_LDL, vsp_param_slot - base_offset);
			gensecondary (I_MRELEASE);
		}
		/*}}}*/
#ifdef MOBILES
		/*{{{  free FORKed mobilespace to local freelist if used*/
		if ((msp_param_slot > -255) && alloc_ms_slots) {
			if (!enable_mobilespace) {
				generr (GEN_NO_MOBILESPACE);
			}
			
			genprimary (I_LDL, msp_param_slot - base_offset);
			/* oki, load link field and stuff it in local 0 -- can trash local workspace, it's done with :) Wptr[-1..0] is definitely valid */
			gensecondary (I_DUP);
			genprimary (I_LDNL, -1);	/* load link field (ptr into mobilespace) */
			genprimary (I_STL, 0);		/* store in local 0 */
			genprimary (I_LDL, 0);		/* load ptr in mobilespace where free blocks live */
			genprimary (I_LDNL, 0);		/* load pointer in/from mobilespace */
			gensecondary (I_REV);
			genprimary (I_STNL, -1);	/* store this pointer in the link field of the mobilespace block */
			/* stackdepth should be 0 here */
			genprimary (I_LDL, msp_param_slot - base_offset);
			genprimary (I_LDL, 0);		/* load ptr in mobilespace where free blocks live */
			genprimary (I_STNL, 0);		/* store our mobilespace there */
		}
		/*}}}*/
#endif
		if (fb_param_slot == (-255)) {
			/* release fork barrier */
			gencomment0 ("{{{  release fork barrier");
			genprimary (I_LDL, (alloc_ws_slots - base_offset) - MIN_FORK_SLOTS + FORK_BARRIER);
			gensecondary (I_MT_RELEASE);
			gencomment0 ("}}}");
		}
		/* release process workspace and end process */
		genprimary (I_LDLP, -base_offset);
		gensecondary (I_PROC_END);

		/* continue normally */
		gencomment0 ("forked SKIP resume point");
		setlab (forked_s_lab);
		/*}}}*/
	} else if (ptype & PROC_REC) {
		/* recover old workspace -- ret will have moved Wptr up 4 */
		genprimary (I_LDL, ((((nparams > MAXREGS) ? nparams : MAXREGS) + MIN_RECURSIVE_SLOTS) - 4));
		gensecondary (I_GAJW);
		gensecondary (I_POP);
		/* throw_the_result_away (); */
	} else if (ptype & PROC_DYNCALL) {
		/* recover old workspace -- ret will have moved Wptr up 4 */
		genprimary (I_LDL, (((nparams > MAXREGS) ? nparams : MAXREGS) + MIN_DYNCALL_SLOTS) - 4);
		gensecondary (I_GAJW);
		gensecondary (I_POP);

	} else if (ptype & PROC_MPA) {
		/* SKIP -- workspace recovered on MOBILE PROC return or SUSPEND */
	}
	/*}}} */
	/*{{{  COMMENT  wsp optimisation */
	/**********************  Start comment out ****************************/
	/*{{{  restore workspace if necessary*/
	/*if (params_below_ws > 0)*/
	/*{*/
	/*	genprimary(I_AJW, params_below_ws);*/
	/*	adjustworkspace(-params_below_ws);*/
	/*}*/
	/*}}}*/
	/**********************   End comment out  ****************************/
	/*}}} */
#ifdef MOBILES
	if (!(ptype & PROC_FORKED)) {
		/*{{{  return non-val MOBILEs to their pointer slots*/
		int dimcount = 0;
		treenode *foo = NULL;

		new_occam_line (tptr, TRUE, TRUE, FALSE);
		/* need to store any non-val MOBILEs back to their pointers */
		for (i = 0; i < nparams; i++) {
			if (paramtable[i].mobile_outslot > -4) {
				/* new_occam_line (*(paramtable[i].pparamexp), TRUE, FALSE, FALSE); */
				if (ptype & PROC_REC) {
					genprimary (I_LDL, RECURSIVE_WS);
					genprimary (I_LDNL, paramtable[i].pslot + paramtable[i].mobile_outslot);
				} else if (ptype & PROC_DYNCALL) {
					genprimary (I_LDL, DYNCALL_SETUP_TEMP_WS);
					genprimary (I_LDNL, paramtable[i].pslot + paramtable[i].mobile_outslot);
				} else if (ptype & PROC_MPA) {
					genprimary (I_LDL, MPA_SETUP_TEMP);
					genprimary (I_LDNL, paramtable[i].pslot + paramtable[i].mobile_outslot);
				} else {
					genprimary (I_LDL, paramtable[i].mobile_outslot);
				}
				/* this could get called to restore a dimension for dynamic MOBILEs */
				foo = *(paramtable[i].pparamexp);
#if 0
fprintf (stderr, "gen11: tinstance: restore MOBILE pointer/dimension from slot %d.  foo = ", paramtable[i].mobile_outslot);
printtreenl (stderr, 4, foo);
#endif
				if ((TagOf (foo) == T_TEMP) || (TagOf (foo) == T_PREEVALTEMP)) {
					foo = NDeclOf (foo);
#if 0
fprintf (stderr, "gen11: tinstance: restore found temporary.  new val (decl) is: ");
printtreenl (stderr, 4, foo);
#endif
				}
				if (paramtable[i].mobile_remember == 1) {
					/*{{{  restore mobile pointer*/
					storemobile (foo);
					gencomment1 ("restore MOBILE pointer for param %d", i);
					if (isdynmobilearray (foo)) {
						genmobileunpack (foo, FALSE, TRUE);
					}
					dimcount = 1;
					/*}}}*/
				} else if (paramtable[i].mobile_remember == 2) {
					/*{{{  don't restore hidden-type, but check it against what it ought to be*/
					int oklab = newlab ();

					texpopd_main (paramtable[i].pmode, *(paramtable[i].pparamexp), MAXREGS - 1, FALSE);
					gensecondary (I_DIFF);
					genbranch (I_CJ, oklab);
					gensecondary (I_SETERR);
					setlab (oklab);

					/*}}}*/
				} else {
					/*{{{  storing hidden dimension or hidden type*/
					if ((TagOf (foo) == S_HIDDEN_PARAM) && (TagOf (NDeclOf (foo)) == S_NTH_DIMENSION)) {
						treenode *nthdim = NDeclOf (foo);

						if (TagOf (RightOpOf (nthdim)) != S_CONSTEXP) {
							geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tinstance -- NTH_DIMENSION RHS not CONSTEXP");
						} else {
							storemobilesize (LeftOpOf (nthdim), dimcount);
							gencomment1 ("restore dynamic MOBILE dimension (nested) from slot %d", paramtable[i].mobile_outslot);
						}
						dimcount++;
					} else if (TagOf (foo) == S_HIDDEN_TYPE) {
						storehiddentypeof (HExpOf (foo), MAXREGS - 1);
						gencomment1 ("restore hidden type from slot %d", paramtable[i].mobile_outslot);
					} else {
						storemobilesize (foo, dimcount);
						gencomment1 ("restore dynamic MOBILE dimension from slot %d", paramtable[i].mobile_outslot);
						dimcount++;
					}
					/*}}}*/
				}
			} else if (pclone_list[i] || paramtable[i].pclone) {
				/*{{{  this was a cloned parameter, we should descope appropiately (and restore the original pparamexp)*/
				const int skiplab = newlab ();
				treenode *nptr = *(paramtable[i].pparamexp);
				int slot = paramtable[i].pslot;

				if (isdynmobilearray (nptr)) {
					slot = slot + 1;
				}

				if (ptype & PROC_REC) {
					genprimary (I_LDL, RECURSIVE_WS);
					genprimary (I_LDNL, slot);
				} else if (ptype & PROC_DYNCALL) {
					genprimary (I_LDL, DYNCALL_SETUP_TEMP_WS);
					genprimary (I_LDNL, slot);
				} else if (ptype & PROC_MPA) {
					genprimary (I_LDL, MPA_SETUP_TEMP);
					genprimary (I_LDNL, slot);
				} else {
					genprimary (I_LDL, slot);
				}

				gencomment0 ("{{{  descope cloned mobile");
				gensecondary (I_DUP);
				genbranch (I_CJ, skiplab);
				gensecondary (I_MT_RELEASE);
				setlab (skiplab);
				throw_the_result_away ();
				throw_the_result_away ();
				gencomment0 ("}}}");

				if (pclone_list[i]) {
					paramtable[i].pparamexp = pclone_list[i];
					pclone_list[i] = NULL;
				}
				/*}}}*/
			}
		}
		/*}}}*/
	}
#endif
	/*{{{  free memory associated with call (if recursive or dynamic) */
	if (ptype & PROC_REC) {
		if (alloc_vs_slots) {
			genprimary (I_LDL, RECURSIVE_VS);
			if (TagOf (tptr) != S_FINSTANCE) {
				gensecondary (I_MRELEASE);
			}
		}
		genprimary (I_LDL, RECURSIVE_WS);
		genprimary (I_ADC, -((alloc_ws_slots - 1) << 2));

		if (TagOf (tptr) != S_FINSTANCE) {
			gensecondary (I_MRELEASE);
		} else {
			if (alloc_vs_slots) {
				gensecondary (I_DUP);
				genprimary (I_STL, RECURSIVE_WS);
				genprimary (I_STNL, 0);
				genprimary (I_LDL, RECURSIVE_WS);
			}
		}
	} else if (ptype & PROC_DYNCALL) {
		int vsskiplab = newlab ();

		genprimary (I_LDL, DYNCALL_SETUP_TEMP_WSBASE);
		gensecondary (I_MRELEASE);

		genprimary (I_LDL, DYNCALL_SETUP_TEMP_VS);
		genbranch (I_CJ, vsskiplab);
		throw_the_result_away ();
		genprimary (I_LDL, DYNCALL_SETUP_TEMP_VS);
		gensecondary (I_MRELEASE);

		setlab (vsskiplab);
	}
	/*}}}  */
	/*{{{  adjust wptr up if we are in an alt guard */
	genprimary (I_AJW, altadjust);
	adjustworkspace (-altadjust);
	/*}}} */
	coder_return_from_call (safe_fn_call (iname));	/* bug TS/2005 15/12/92 */
	insidealtguard = savedinsidealtguard;
	instancedlevel = oldinstancedlevel;
	if (paramtable != NULL) {
		memfree (paramtable);
	}
}

/*}}}*/
/*{{{  PUBLIC void tsuspend (treenode *tptr)*/
/*
 *	generates code for a SUSPEND call
 */
PUBLIC void tsuspend (treenode *tptr)
{
	const int rlab = newlab();
	/* have local 0 for temporary */

	loadmpp ();
	gensecondary (I_DUP);
	gensecondary (I_DUP);
	genprimary (I_LDNL, MPP_WPTR);
	gensecondary (I_GAJW);			/* swap back into activating workspace */
	gensecondary (I_REV);
	genprimary (I_STNL, MPP_WPTR);		/* save our current Wptr */
	genprimary (I_STL, MPA_SETUP_TEMP_VS);	/* store mobile-process block here tmporarily */
	genloadlabptr (rlab, NOLAB, "PROCESS PTR");
	genprimary (I_LDL, MPA_SETUP_TEMP_VS);
	genprimary (I_STNL, MPP_IPTR);		/* save resume point */
	genprimary (I_LDL, MPA_SETUP_TEMP_VS);
	genprimary (I_LDNL, MPP_AIPTR);
	gensecondary (I_GCALL);			/* switch back to activating code */
	throw_the_result_away ();
	setlab (rlab);

	/* have return address in local 0, better store it */
	genprimary (I_LDL, 0);
	loadmpp ();
	genprimary (I_STNL, MPP_AIPTR);

	gencomment0 ("suspend");
	return;
}
/*}}}*/
/*{{{  PUBLIC void mapvalof(tptr, destlistptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  mapvalof maps out workspace requirement for the VALOF, 'tptr',
 *           whose result is either left in Areg, or stored in the list
 *           'destlistptr'.  N.B. If anything else is required (result
 *           in fpreg, or double or quad-length results), this must be
 *           dealt with expicitly elsewhere.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void mapvalof (treenode * tptr, treenode ** destlistptr)
{
	treenode **savedvalofdestlist = valofdestlist;
	valofdestlist = destlistptr;
	mapdeclandbody (tptr, mapvalofmain, FALSE, FALSE);
	valofdestlist = savedvalofdestlist;
}

/*}}}*/
/*{{{  PUBLIC void tvalof(tptr, destlist)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tvalof generates code for the VALOF, 'tptr', leaving the result in
 *         Areg if there is only one result and it will fit in Areg,
 *         otherwise storing each result in the corresponding item of
 *         'destlist'.  N.B. If anything else is required (result in fp reg,
 *         double and quad-length result) it should be dealt with explicitly
 *         elsewhere.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void tvalof (treenode * tptr, treenode * destlist)
{
	treenode *resultlist;
	treenode *specsptr = tptr;

	tptr = tspecs (tptr);
	resultlist = VLResultListOf (tptr);
	tprocess (VLBodyOf (tptr));

	tpreexp (resultlist);

	switch (valof_return_style (resultlist)) {
	case return_style_alu:
		texp (ThisItem (resultlist), MANY_REGS);
		break;
	case return_style_fpu:
	case return_style_other:
		tassign (destlist, (listitems (resultlist) == 1) ? ThisItem (resultlist) : resultlist);
		break;
	}
	tdespecs (specsptr);
}

/*}}}*/
/*{{{  PRIVATE rotaterightsequence*/
typedef enum {
	rotaterightsequence_ldl_ldl,
	rotaterightsequence_ldl_dup,
	rotaterightsequence_ldl_ldc
} rotaterightsequence_t;

PRIVATE rotaterightsequence_t rotaterightsequence (treenode * const arg, treenode * const places)
{
	if (issimplelocal (arg, be_lexlevel) || isconst (arg))
		return rotaterightsequence_ldl_ldl;
	if (has_dup && (regsfor (places) < 2))
		return rotaterightsequence_ldl_dup;
	return rotaterightsequence_ldl_ldc;
}

/*}}}*/
/*{{{  PUBLIC int mappredef (tptr, destlist)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  mappredef maps the evaluation of a predefined routine.
 *            Parameters are reordered where neccessary, and dummy parameters
 *            are inserted where neccessary.
 *            For a multiply-valued predef, 'destlist' is a list of the
 *            destinations which is reordered if neccessary.
 *
 *            Returns TRUE if done inline.
 *            Returns FALSE if lib call, with mapinstance stiil to be done.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC int mappredef (treenode * tptr, treenode * destlist)
{
	treenode *iname = INameOf (tptr);
	const int pdno = NModeOf (iname);
	treenode *paramlist = IParamListOf (tptr);
	treenode **param[MAX_PREDEFPARAMS];
	treenode **dest[MAX_PREDEFDESTS];

	if (pdinline (pdno)) {
		int loadseq = 0;
		int i;
		treenode *lastparam = NULL;

		/*{{{  load parameters into param */
		i = 0;
		while (!EndOfList (paramlist)) {
			param[i] = ThisItemAddr (paramlist);
			i++;
			paramlist = NextItem (paramlist);
		}
		lastparam = (i > 0) ? *(param[i-1]) : NULL;
		/*}}} */
		/*{{{  load destinations into dest */
		i = 0;
		while (!EndOfList (destlist)) {
			dest[i] = ThisItemAddr (destlist);
			i++;
			destlist = NextItem (destlist);
		}
		/*}}} */
		/*{{{  load parameters, perform operation, store results maybe */
		switch (pdno) {
			/*{{{  LONGADD CRCWORD CRCBYTE      load 2, 1, 0 */
		case PD_LONGADD:
		case PD_CRCWORD:
		case PD_CRCBYTE:
			loadseq = mapload3regs (P_EXP, param[2], P_EXP, param[1], P_EXP, param[0]);
			break;
			/*}}} */
			/*{{{  LONGSUM LONGPROD             load 2, 1, 0  store 1, 0 */
		case PD_LONGSUM:
		case PD_LONGPROD:
			loadseq = mapload3regs (P_EXP, param[2], P_EXP, param[1], P_EXP, param[0]);
			mapstoreregs (dest, 2);
			break;
			/*}}} */
			/*{{{  LONGSUB                      load 2, 0, 1 */
		case PD_LONGSUB:
			loadseq = mapload3regs (P_EXP, param[2], P_EXP, param[0], P_EXP, param[1]);
			break;
			/*}}} */
			/*{{{  LONGDIFF                     load 2, 0, 1  store 1, 0 */
		case PD_LONGDIFF:
			loadseq = mapload3regs (P_EXP, param[2], P_EXP, param[0], P_EXP, param[1]);
			mapstoreregs (dest, 2);
			break;
			/*}}} */
			/*{{{  LONGDIV                      load 0, 1, 2  store 0, 1 */
		case PD_LONGDIV:
			{
				treenode **temp;
				loadseq = mapload3regs (P_EXP, param[0], P_EXP, param[1], P_EXP, param[2]);
				temp = dest[0];
				dest[0] = dest[1];
				dest[1] = temp;
				mapstoreregs (dest, 2);
			}
			break;
			/*}}} */
			/*{{{  SHIFTRIGHT SHIFTLEFT         load 0, 1, 2  store 1, 0 */
		case PD_SHIFTRIGHT:
		case PD_SHIFTLEFT:
			loadseq = mapload3regs (P_EXP, param[0], P_EXP, param[1], P_EXP, param[2]);
			mapstoreregs (dest, 2);
			break;
			/*}}} */
			/*{{{  NORMALISE                    load 0, 1     store 2, 1, 0 */
		case PD_NORMALISE:
			loadseq = mapload2regs (P_EXP, param[0], P_EXP, param[1]);
			mapstoreregs (dest, 3);
			break;
			/*}}} */
			/*{{{  FRACMUL BITCOUNT             load 1, 0 */
		case PD_FRACMUL:
		case PD_BITCOUNT:
			loadseq = mapload2regs (P_EXP, param[1], P_EXP, param[0]);
			break;
			/*}}} */
			/*{{{  BITREVNBITS                  load 0, 1 */
		case PD_BITREVNBITS:
			loadseq = mapload2regs (P_EXP, param[0], P_EXP, param[1]);
			/*}}} */
			/*{{{  BITREVWORD                   load 0 */
		case PD_BITREVWORD:
			mapexp (param[0]);
			loadseq = 0;
			break;
			/*}}} */
			/*{{{  ABS ISNAN ...                load fp0 */
		case PD_ABS:
		case PD_DABS:
		case PD_ISNAN:
		case PD_DISNAN:
		case PD_DNOTFINITE:
		case PD_MULBY2:
		case PD_DMULBY2:
		case PD_DIVBY2:
		case PD_DDIVBY2:
		case PD_SQRT:
		case PD_DSQRT:
		case PD_FPINT:
		case PD_DFPINT:
			mapfpexp (param[0]);
			loadseq = 0;
			break;
			/*}}} */
			/*{{{  ORDERED DORDERED             load fp0, fp1 in either order */
		case PD_ORDERED:
		case PD_DORDERED:
			mapfpload2regs (param[0], param[1]);
			loadseq = 0;
			break;
			/*}}} */
			/*{{{  CAUSEERROR RESCHEDULE ...    nothing required */
		case PD_CAUSEERROR:
		case PD_WSSIZEOF:
		case PD_VSSIZEOF:	/* bug TS/1797 18/08/92 */
			break;
		case PD_RESCHEDULE:
			datasize = max_INT32 (datasize, DS_IO);
			break;
			/*}}} */
			/*{{{  ASSERT                       possibly something required */
		case PD_ASSERT:
			if (	/*NEED_ERRORS && *//* bug TS/2060 26/01/93 */
				   !ignore_assertions)
				mapexp (param[0]);
			break;
			/*}}} */
			/*{{{  GETPRI SETPRI INCPRI DECPRI  for process priority */
		case PD_GETPRI:
			mapstoreinopd (P_EXP, param[0]);
			break;
		case PD_SETPRI:
			mapexp (param[0]);
			loadseq = 0;
			break;
		case PD_INCPRI:
		case PD_DECPRI:
			break;
			/*}}}  */
			/*{{{  ASHIFTRIGHT ASHIFTLEFT       special */
		case PD_ASHIFTRIGHT:
		case PD_ASHIFTLEFT:
			/* param[0] has to be extended to double-length and so requires two
			   registers to hold, so we have to preevaluate the count (param[1])
			   needs more than MAXREGS - 2 */
			if (regsfor (*param[1]) > MAXREGS - 2)
				/*{{{  preevaluate param[1], the count */
			{
				treenode **count = param[1];
				mapexp (count);
				*count = gettemp (*param[1], NM_WORKSPACE);
				upusecount (*count, 2);
				mapexp (param[0]);
				freetemp (*count);
			}
			/*}}} */
			else
				/*{{{  load operand; load count */
			{
				mapexp (param[0]);
				mapexp (param[1]);
			}
			/*}}} */
			loadseq = 1;
			break;
			/*}}} */
			/*{{{  ROTATERIGHT ROTATELEFT       special */
		case PD_ROTATERIGHT:
		case PD_ROTATELEFT:
			{
				const int old = switch_to_temp_workspace ();
				treenode *zero = newconstant (0);
				switch_to_prev_workspace (old);
				if (pdno == PD_ROTATERIGHT) {
					switch (rotaterightsequence (*param[0], *param[1])) {
					case rotaterightsequence_ldl_ldl:
						loadseq = mapload3regs (P_EXP, param[0], P_EXP, param[0], P_EXP, param[1]);
						break;
					case rotaterightsequence_ldl_dup:
						mapexp (param[0]);
						mapexp (param[1]);
						break;
					case rotaterightsequence_ldl_ldc:
						loadseq = mapload3regs (P_EXP, param[0], P_EXP, &zero, P_EXP, param[1]);
						break;
					}
				} else
					loadseq = mapload3regs (P_EXP, &zero, P_EXP, param[0], P_EXP, param[1]);
			}
			break;
			/*}}} */
			/*{{{  UNPACKSN                     special */
		case PD_UNPACKSN:
			/* either
			   ldc 0; param0  or param0; ldc 0; rev
			   the order does not affect the mapping  */
			{
				treenode **temp;
				mapexp (param[0]);
				temp = dest[0];
				dest[0] = dest[2];
				dest[2] = temp;
				mapstoreregs (dest, 3);
			}
			break;
			/*}}} */
			/*{{{  ROUNDSN                      special */
		case PD_ROUNDSN:
			/* Yexp IS param[0], Yfrac IS param[1], Yguard IS param[2].
			   If Yfrac or Yguard require a certain amount of evaluation (more than
			   a simple load), then we load pointers to them into temporaries. */
			{
				int Yfracmode = P_EXP, Yguardmode = P_EXP;
				treenode **Yexp = param[0], **Yfrac = param[1], **Yguard = param[2];
				if (regsfor (*Yfrac) > 1)
					/*{{{  load Yfrac to temporary */
				{
					mapexp (Yfrac);
					*Yfrac = gettemp (*Yfrac, NM_WORKSPACE);
					upusecount (*Yfrac, 1);
					Yfracmode = P_TEMP;
				}
				/*}}} */
				if (regsfor (*Yguard) > 1)
					/*{{{  load Yguard to temporary */
				{
					mapexp (Yguard);
					*Yguard = gettemp (*Yguard, NM_WORKSPACE);
					upusecount (*Yguard, 1);
					Yguardmode = P_TEMP;
				}
				/*}}} */
				loadseq = mapload2regs (Yfracmode, Yfrac, Yguardmode, Yguard);
				mapexp (Yexp);
				/*{{{  reserve local 0 for postnormsn */
				if (!insidealtguard) {
					reservelowworkspace (1);	/* local 0 is used as a parameter to postnormsn */
				} else {
					/* If we are inside an ALT guard, we have to pull wptr down, so
					   that postnormsn's local 0 temporary doesn't overwrite the alt's
					   local 0 temporary. */
					datasize = max_INT32 (datasize, (int) (alt_ds + 1));
				}
				/*}}} */
				freeiftemp (*Yfrac);
				freeiftemp (*Yguard);
			}
			break;
			/*}}} */
			/*{{{  MINUSX                       special */
		case PD_MINUSX:
			if (!has_fpu_core) {
				/*{{{  inline without floating point */
				if (isconstexpnd (*param[0])) {
					treenode *pptr = *param[0];
					SetLoVal (pptr, LoValOf (pptr) ^ 0x80000000);
				}
				mapexp (param[0]);
				/*}}} */
			} else {
				/*{{{  inline with floating point */
				treenode **pptr = param[0];
				treenode *t = *pptr;
				if (isconstexpnd (t)) {
					SetLoVal (t, LoValOf (t) ^ 0x80000000);
					mapaddr (pptr);	/* Will put it in the constant table */
				} else {
					/*{{{  generate source to temp., load into integer register */
					*pptr = gettemp (*pptr, NM_WORKSPACE);
					if (isaddressable (NDeclOf (*pptr))) {
						mapaddr (NDeclAddr (*pptr));
					} else {
						mapsimpleassign (ntypeof (*pptr), P_TEMP, pptr, P_EXP, NDeclAddr (*pptr));
					}
					freetemp (*pptr);
					/*}}} */
				}
				/*}}} */
			}
			break;
			/*}}} */
			/*{{{  DMINUSX                      special */
		case PD_DMINUSX:
			/* must have has_fpu_core to reach here */
			{
				treenode **pptr = param[0];
				treenode *t = *pptr;
				if (isconst (t)) {
					SetHiVal (t, HiValOf (t) ^ 0x80000000);
					mapaddr (pptr);	/* Will put it in the constant table */
				} else {
					*pptr = gettemp (*pptr, NM_WORKSPACE);
					mapsimpleassign (ntypeof (*pptr), P_TEMP, pptr, P_EXP, NDeclAddr (*pptr));
					freetemp (*pptr);
				}
			}
			break;
			/*}}} */
			/*{{{  NOTFINITE                    special */
		case PD_NOTFINITE:
			if (has_fpu_core)
				mapfpexp (param[0]);
			else if (has_fp_support)
				mapexp (param[0]);
			else {	/* to work on any 32 bit processor */

#if 0				/* putting the constant into the constant table doesn't help */
				const int old = switch_to_temp_workspace ();
				treenode *const inf = newconstant (INFINITY);
				switch_to_prev_workspace (old);
				mapload2regs (P_EXP, &inf, P_EXP, param[0]);
#endif /* 0 */
				mapexp (param[0]);
			}
			break;
			/*}}} */
		#if 0
			/*{{{  LOADINPUTCHANNEL + friends   special */
		case PD_LOADINPUTCHANNEL:
		case PD_LOADOUTPUTCHANNEL:
			mapsimpleassign (S_INT, P_EXP, param[0], chanaspointer ? P_EXP : P_PTR, param[1]);
			break;
		case PD_LOADINPUTCHANNELVECTOR:
		case PD_LOADOUTPUTCHANNELVECTOR:
		case PD_LOADBYTEVECTOR:
			mapsimpleassign (S_INT, P_EXP, param[0], P_PTR, param[1]);
			break;
			/*}}} */
			/*{{{  KERNELRUN                    special */
		case PD_KERNELRUN:
			{
				SOURCEPOSN locn = LocnOf (tptr);
				treenode **code = param[0], **entryoffset = param[1], **workspace = param[2], *nparams = *(param[3]);
				if (!isconstexpnd (nparams) || LoValOf (nparams) < 3) {
					generr (GEN_KERNEL_RUN_ERROR);
				}
				/*{{{  overwrite the parameters we are given with what we want to load */
				*code = transformelement (be_trans_params,
							  newarraysubnode (S_ARRAYSUB, locn, *code, *entryoffset), RANGECHECKING, be_lexlevel);
				/* Note that S_ELSIZE is special and can break the tree structure - bug 1142 7/2/91 */
				*workspace = transformelement (be_trans_params,
							       newarraysubnode (S_ARRAYSUB, locn, *workspace,
										foldexp (newmopnode (S_ELSIZE, locn, *workspace, S_INT))),
							       FALSE, be_lexlevel);
				*entryoffset = newnamenode (N_LABELDEF, locn, tempname_p, newleafnode (S_LABEL, locn), NULL, 0, 0, NM_DEFAULT);
				/*}}} */
				loadseq = mapload3regs (P_PTR, code, P_PTR, entryoffset, P_PTR, workspace);
			}
			break;
			/*}}} */
		#endif
			/*{{{  MOVE2D CLIP2D DRAW2D         special */
		case PD_MOVE2D:
		case PD_CLIP2D:
		case PD_DRAW2D:
			{
				const SOURCEPOSN locn = LocnOf (tptr);
				/*{{{  abbreviate the parameters to sensible names */
				/* Note that S_ELSIZE is special and can break the tree structure - bug 1142 7/2/91 */
				treenode **source = param[0];
				treenode *sourcerows = foldexp (newmopnode (S_ELSIZE, locn, *source, S_INT));
				treenode *sourcestride = foldexp (newmopnode (S_ELSIZE, locn,
									      newarraysubnode (S_ARRAYSUB, locn, *source, dummyexp_p), S_INT));
				treenode **sx = param[1];
				treenode **sy = param[2];
				treenode **dst = param[3];
				treenode *dstrows = foldexp (newmopnode (S_ELSIZE, locn, *dst, S_INT));
				treenode *dststride = foldexp (newmopnode (S_ELSIZE, locn,
									   newarraysubnode (S_ARRAYSUB, locn, *dst, dummyexp_p), S_INT));
				treenode **dx = param[4];
				treenode **dy = param[5];
				treenode **width = param[6];
				treenode **length = param[7];
				/*}}} */
				int sxmode = P_EXP, symode = P_EXP, dxmode = P_EXP, dymode = P_EXP,
					widthmode = P_EXP, lengthmode = P_EXP, sourcemode = P_EXP, dstmode = P_EXP;
				treenode *sourceaddr = NULL, *dstaddr = NULL, *sxcheck = NULL, *sycheck = NULL, *dxcheck = NULL, *dycheck = NULL;
				int loadseq1;	/* load sequence for move2dinit parameters : we throw it */
				/* away here and recalculate it during code generation   */

#if 0
				fprintf (outfile, "original source tree is");
				printtree (0, *source);
				fputc ('\n', outfile);
#endif
				/* we have to simplify this first, so that we can't timeslice
				   between move2dinit and move2d etc. Bug 403 29/8/90
				   Note that some of the range checks depend upon the width.
				 */
				if (regsforopd (widthmode, *width) >= MAXREGS)
					widthmode = mapevalexp (widthmode, width);
				/*{{{  do some checking */
				{
					if (RANGECHECKING)
						/*{{{  simplify sx, sy, dx, dy, length, width */
					{
						sxmode = mapevalexp (sxmode, sx);
						symode = mapevalexp (symode, sy);
						dxmode = mapevalexp (dxmode, dx);
						dymode = mapevalexp (dymode, dy);
						lengthmode = mapevalexp (lengthmode, length);
					}
					/*}}} */
					/* I build the check trees whatever 'errormode' we are compiling
					   as they might need constant folding, and the constant folding
					   might generate a compile time error. */
					sxcheck = buildchecktree (*sx, sourcestride, *width, TRUE);
					sycheck = buildchecktree (*sy, sourcerows, *length, TRUE);
					dxcheck = buildchecktree (*dx, dststride, *width, FALSE);
					dycheck = buildchecktree (*dy, dstrows, *length, FALSE);
					if (RANGECHECKING)
						/*{{{  map the check expressions */
					{
						if (sxcheck != NULL)
							mapexp (&sxcheck);
						if (sycheck != NULL)
							mapexp (&sycheck);
						if (dxcheck != NULL)
							mapexp (&dxcheck);
						if (dycheck != NULL)
							mapexp (&dycheck);
					}
					/*}}} */
				}
				/*}}} */
				/*{{{  build sourceaddr and dstaddr */
				sourceaddr = transformelement (be_trans_params,
							       newarraysubnode (S_ARRAYSUB, locn,
										newarraysubnode (S_ARRAYSUB, locn, *source, *sy),
										*sx), FALSE, be_lexlevel);
				transsubscripts (be_trans_params, &sourceaddr, FALSE);	/* bug 1308 21/8/91 */
				dstaddr = transformelement (be_trans_params,
							    newarraysubnode (S_ARRAYSUB, locn,
									     newarraysubnode (S_ARRAYSUB, locn, *dst, *dy), *dx), FALSE, be_lexlevel);
				transsubscripts (be_trans_params, &dstaddr, FALSE);	/* bug 1308 21/8/91 */
				sourceaddr = newmopnode (S_ADDRESSOF, locn, sourceaddr, 0);
				dstaddr = newmopnode (S_ADDRESSOF, locn, dstaddr, 0);

				/* we have to simplify these first, so that we can't timeslice
				   between move2dinit and move2d etc. Bug 403 29/8/90
				 */
#if 0
				fprintf (outfile, "sourceaddr is");
				printtree (0, sourceaddr);
				fputc ('\n', outfile);
#endif
				if (regsforopd (sourcemode, sourceaddr) >= MAXREGS)
					sourcemode = mapevalexp (sourcemode, &sourceaddr);
				if (regsforopd (dstmode, dstaddr) >= MAXREGS)
					dstmode = mapevalexp (dstmode, &dstaddr);
				/*}}} */
				/*{{{  map the load for move2dinit */
				loadseq1 = mapload3regs (P_EXP, &sourcestride, P_EXP, &dststride, lengthmode, length);
				/*}}} */
				/*{{{  map the load for the move operation proper */
				loadseq = mapload3regs (sourcemode, &sourceaddr, dstmode, &dstaddr, widthmode, width);
				/*}}} */
				/*{{{  free temporaries used */
				freeiftemp (*sx);
				freeiftemp (*sy);
				freeiftemp (*dx);
				freeiftemp (*dy);
				freeiftemp (*length);
				freeiftemp (*width);
				freeiftemp (sourceaddr);
				freeiftemp (dstaddr);
				/*}}} */
				/*{{{  add new parameters to the parameter list */
				{
					treenode *newparams = newlistnode (S_LIST, locn, sourceaddr,
						newlistnode (S_LIST, locn, dstaddr,
							newlistnode (S_LIST, locn, sxcheck,
								newlistnode (S_LIST, locn, sycheck,
									newlistnode (S_LIST, locn, dxcheck,
										newlistnode (S_LIST, locn, dycheck, NULL))))));
					appendlist (newparams, IParamListOf (tptr));
				}
				/*}}} */
			}
			break;
			/*}}} */
		#if 0
			/*{{{  UPDATE_PROFCOUNT*/
		case PD_UPDATE_PROFCOUNT:
			break;
			/*}}}*/
		#endif
#ifdef MOBILES
			/*{{{  ALLOC_CHAN_TYPE*/
		case PD_ALLOC_CHAN_TYPE:
			/* reserve local 0 for a temporary */
			reservelowworkspace (1);
			mapexp (param[0]);
			mapexp (param[1]);
			break;
			/*}}}*/
#endif
			/*{{{  DETACH_DYNMOB ATTACH_DYNMOB*/
		#ifdef PD_ATTACH_DYNMOB
		case PD_ATTACH_DYNMOB:
			mapexp (param[0]);
			mapstoreinopd (P_EXP, param[2]);
			mapexp (param[1]);
			mapstoreinopd (P_EXP, param[2]);
			break;
		#endif
		#ifdef PD_DETACH_DYNMOB
		case PD_DETACH_DYNMOB:
			mapexp (param[0]);
			mapstoreinopd (P_EXP, param[1]);
			mapexp (param[0]);
			mapstoreinopd (P_EXP, param[2]);
			break;
		#endif
			/*}}}*/
			/*{{{  DECODE_DATA*/
		case PD_DECODE_DATA:
			/* added 4th parameter is size expression (in bytes) */
			mapaddr (param[0]);
			mapstoreinopd (P_EXP, param[1]);
			mapexp (param[3]);
			mapstoreinopd (P_EXP, param[2]);
			break;
			/*}}}*/
			/*{{{  PROTOCOL_HASH*/
		#ifdef PD_PROTOCOL_HASH
		case PD_PROTOCOL_HASH:
			/* if this gets here, it's because it's a run-time type */
			mapaddr (param[0]);
			break;
		#endif
			/*}}}*/
			/*{{{  LOAD_TYPE_DESC*/
		#ifdef PD_LOAD_TYPE_DESC
		case PD_LOAD_TYPE_DESC:
			/* doesn't need to touch the variable (yet) */
			// mapaddr (param[0]);
			break;
		#endif
			/*}}}*/
			/*{{{  REAL32SIN, REAL64SIN, REAL32COS, REAL64COS, REAL32TAN, REAL64TAN */
		case PD_REAL32SIN:
		case PD_REAL64SIN:
		case PD_REAL32COS:
		case PD_REAL64COS:
		case PD_REAL32TAN:
		case PD_REAL64TAN:
			mapfpexp (param[0]);
			loadseq = 0;
			break;
			/*}}}*/
			/*{{{  GETAFF SETAFF  for processor affinity */
		case PD_GETAFF:
			mapstoreinopd (P_EXP, param[0]);
			break;
		case PD_SETAFF:
			mapexp (param[0]);
			loadseq = 0;
			break;
			/*}}}*/
			/*{{{  KILLCALL  for blocking system calls */
		case PD_KILLCALL:
			mapexp (param[0]);
			mapexp (param[1]);
			break;
			/*}}}*/
			/*{{{  WAIT.FOR.INTERRUPT  for RMoX interrupt handling */
		case PD_WAIT_FOR_INTERRUPT:
			mapexp (param[1]);
			mapexp (param[0]);
			mapstoreinopd (P_EXP, param[2]);
			break;
			/*}}}*/
			/*{{{  BIND.MOBILE / BIND.MOBILE.HW for mobile manipulation */
		case PD_BIND_MOBILE:
		case PD_BIND_MOBILE_HW:
			mapexp (param[1]);
			mapexp (param[0]);
			mapstoreinopd (P_EXP, param[0]);
			break;
			/*}}}*/
			/*{{{  DMA.CAPABLE  query MOBILEs DMA capability */
		case PD_DMA_CAPABLE:
			mapexp (param[0]);
			break;
			/*}}}*/
			/*{{{  MAKE.DMA.CAPABLE  upgrade MOBILE to DMA capability */
		case PD_MAKE_DMA_CAPABLE:
			mapexp (param[0]);
			mapstoreinopd (P_EXP, param[0]);
			break;
			/*}}}*/
			/*{{{  MEMORY.BARRIER, READ.MEMORY.BARRIER, WRITE.MEMORY.BARRIER */
		case PD_MEMORY_BARRIER:
		case PD_READ_MEMORY_BARRIER:
		case PD_WRITE_MEMORY_BARRIER:
			break;
			/*}}}*/
			/*{{{  RESIZE.MOBILE.ARRAY.1D */
		case PD_RESIZE_MOBILE_ARRAY_1D:
			mapexp (param[1]);
			mapexp (param[0]);
			mapstoreinopd (P_EXP, param[0]);
			break;
			/*}}}*/
		default:
			badtag (LocnOf (tptr), TagOf (tptr), "mappredef");
			break;
		}
		/*}}} */
		SetILoadSeq (tptr, loadseq);
	} else {
		/*{{{  transform to call to library routine and map that */
		iname = libentry (pdlibname (NNameOf (iname), pdno), LocnOf (tptr));
		SetIName (tptr, iname);
		return (FALSE);	/* indicate that it is now a lib call */
		/*}}} */
	}
	return (TRUE);		/* indicate that it is done in line */
}

/*}}}*/
/*{{{  PUBLIC void tpredef (tptr, destlist)*/
/*****************************************************************************
 *
 *  tpredef generates code for the predefined routine call tptr.
 *          If there are multiple results, 'destlist' contains a list of
 *          destinations, otherwise 'destlist' is NULL.
 *
 *****************************************************************************/
PUBLIC void tpredef (treenode * tptr, treenode * destlist)
{
	treenode *iname = INameOf (tptr);
	const int loadseq = ILoadSeqOf (tptr);
	const int pdno = NModeOf (iname);
	treenode *paramlist = IParamListOf (tptr);
	treenode *param[MAX_PREDEFPARAMS];
	treenode **dest[MAX_PREDEFDESTS];
	int i;

	/*{{{  load parameters into param */
	for (i = 0; !EndOfList (paramlist); i++, paramlist = NextItem (paramlist)) {
		if (i >= MAX_PREDEFPARAMS) {
			err_abort ("tpredef");
		}
		param[i] = ThisItem (paramlist);
	}
	/*}}} */
	/*{{{  load destinations into dest */
	for (i = 0; !EndOfList (destlist); i++, destlist = NextItem (destlist)) {
		if (i >= MAX_PREDEFDESTS) {
			err_abort ("tpredef");
		}
		dest[i] = ThisItemAddr (destlist);
	}
	/*}}} */
	/*{{{  load parameters, perform operation, store results maybe */
	switch (pdno) {
		/*{{{  LONGADD CRCWORD CRCBYTE */
	case PD_LONGADD:
	case PD_CRCWORD:
	case PD_CRCBYTE:
		tload3regs (P_EXP, param[2], P_EXP, param[1], P_EXP, param[0], loadseq, TRUE);
		gensecondary (pdno == PD_LONGADD ? I_LADD : pdno == PD_CRCWORD ? I_CRCWORD : I_CRCBYTE);
		if (pdno != PD_LONGADD)	/* MDP */
			throw_nested_result_away ();	/* the predefines ignore Breg after operation */
		break;
		/*}}} */
		/*{{{  LONGSUM LONGPROD */
	case PD_LONGSUM:
	case PD_LONGPROD:
		tload3regs (P_EXP, param[2], P_EXP, param[1], P_EXP, param[0], loadseq, TRUE);
		/*{{{  T9000_alpha_badlmulpre */
		if ((pdno == PD_LONGPROD) && T9000_alpha_badlmulpre (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the args to settle");
		}
		/*}}} */
		/*{{{  T9000_gamma_badmul */
		if ((pdno == PD_LONGPROD) && T9000_gamma_badmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Workaround a slow multiplier");
		}
		/*}}} */
		gensecondary (pdno == PD_LONGSUM ? I_LSUM : I_LMUL);
		/*{{{  T9000_alpha_badlmul */
		if ((pdno == PD_LONGPROD) && T9000_alpha_badlmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the result to settle");
		}
		/*}}} */
		tstoreregs (dest, 2);
		break;
		/*}}} */
		/*{{{  LONGSUB */
	case PD_LONGSUB:
		tload3regs (P_EXP, param[2], P_EXP, param[0], P_EXP, param[1], loadseq, TRUE);
		gensecondary (I_LSUB);
		break;
		/*}}} */
		/*{{{  LONGDIFF */
	case PD_LONGDIFF:
		tload3regs (P_EXP, param[2], P_EXP, param[0], P_EXP, param[1], loadseq, TRUE);
		gensecondary (I_LDIFF);
		tstoreregs (dest, 2);
		break;
		/*}}} */
		/*{{{  LONGDIV */
	case PD_LONGDIV:
		{
			treenode **temp;
			tload3regs (P_EXP, param[0], P_EXP, param[1], P_EXP, param[2], loadseq, TRUE);
			temp = dest[0];
			dest[0] = dest[1];
			dest[1] = temp;
			gensecondary (I_LDIV);
			tstoreregs (dest, 2);
		}
		break;
		/*}}} */
		/*{{{  SHIFTRIGHT SHIFTLEFT */
	case PD_SHIFTRIGHT:
	case PD_SHIFTLEFT:
		/* TS/1979 30/11/92 - CON
		   NOTE that 'long' shifts are legal in this implementation,
		   though they may be slow. They produce the correct result (zero).
		 */
		tload3regs (P_EXP, param[0], P_EXP, param[1], P_EXP, param[2], loadseq, TRUE);
		gensecondary (pdno == PD_SHIFTRIGHT ? I_LSHR : I_LSHL);
		/*{{{  T9000_alpha_badlmul */
		if (T9000_alpha_badlmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the result to settle");
		}
		/*}}} */
		tstoreregs (dest, 2);
		break;
		/*}}} */
		/*{{{  NORMALISE */
	case PD_NORMALISE:
		tload2regs (P_EXP, param[0], P_EXP, param[1], FALSE, TRUE);
		/*{{{  T9000_alpha_badlmulpre */
		if (T9000_alpha_badlmulpre (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the args to settle");
		}
		/*}}} */
		gensecondary (I_NORM);
		/*{{{  T9000_alpha_badlmul */
		if (T9000_alpha_badlmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the result to settle");
		}
		/*}}} */
		tstoreregs (dest, 3);
		break;
		/*}}} */
		/*{{{  FRACMUL BITCOUNT */
	case PD_FRACMUL:
	case PD_BITCOUNT:
		tload2regs (P_EXP, param[1], P_EXP, param[0], FALSE, TRUE);
		/*{{{  T9000_gamma_badmul */
		if ((pdno == PD_FRACMUL) && T9000_gamma_badmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Workaround a slow multiplier");
		}
		/*}}} */
		gensecondary ((pdno == PD_FRACMUL) ? I_FMUL : I_BITCNT);
		/*{{{  T9000_alpha_badfmul */
		if ((pdno == PD_FRACMUL) && T9000_alpha_badfmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the result to settle");
		}
		/*}}} */
		break;
		/*}}} */
		/*{{{  BITREVNBITS */
	case PD_BITREVNBITS:
		/* TS/1979 30/11/92 - CON
		   NOTE that 'long' shifts are illegal in this implementation,
		   because the naive instruction sequence would produce an incorrect
		   result.
		 */
		tload2regs (P_EXP, param[0], P_EXP, param[1], FALSE, TRUE);
		if (!T9000_instruction_timings && !isconst (param[1]))
			checkerror ();	/* bug TS/1979 30/11/92 */
		gensecondary (I_BITREVNBITS);
		break;
		/*}}} */
		/*{{{  GETPRI / SETPRI / INCPRI / DECPRI */
	case PD_GETPRI:
		gensecondary (I_GETPRI);
		storeinopd (P_EXP, param[0], 0, MAXREGS - 1);
		break;
	case PD_SETPRI:
		texp (param[0], MANY_REGS);
		gensecondary (I_SETPRI);
		break;
	case PD_INCPRI:
		{
			const int skiplab = newlab ();
			gensecondary (I_GETPRI);
			gensecondary (I_DUP);
			genbranch (I_CJ, skiplab);
			genprimary (I_ADC, -1);
			gensecondary (I_SETPRI);
			setlab (skiplab);
			throw_the_result_away ();
			break;
		}
	case PD_DECPRI:
		gensecondary (I_GETPRI);
		genprimary (I_ADC, 1);
		gensecondary (I_SETPRI);
		break;
		/*}}}  */
		/*{{{  ASHIFTRIGHT ASHIFTLEFT */
	case PD_ASHIFTRIGHT:
	case PD_ASHIFTLEFT:
		/* param[0] has to be extended to double-length and so requires two
		   registers to hold, so we have to preevaluate the count (param[1])
		   if it needs more than MAXREGS - 2 */
		/* TS/1979 30/11/92 - CON
		   NOTE that 'long' shifts are illegal in this implementation,
		   because the naive instruction sequence would produce an incorrect
		   result.
		 */
		{
			int param1mode = P_EXP;
			if (preeval (param1mode, param[1]))
				/*{{{  preevaluate count to temporary */
			{
				texp (NDeclOf (param[1]), MANY_REGS);
				storeinname (param[1], 0);
				param1mode = P_TEMP;
			}
			/*}}} */

			texp (param[0], MANY_REGS);
			gensecondary (I_XDBLE);
			texpopd (param1mode, param[1], MAXREGS - 2);
			if (!T9000_instruction_timings && !isconst (param[1]))
				checkerror ();	/* bug TS/1979 30/11/92 */
			if (pdno == PD_ASHIFTRIGHT) {
				gensecondary (I_LSHR);
				/*{{{  T9000_alpha_badlmul */
				if (T9000_alpha_badlmul (&tx_global)) {
					gensecondary (I_NOP);
					gencomment0 ("Time for the result to settle");
				}
				/*}}} */
				throw_nested_result_away ();
			} else {
				gensecondary (I_LSHL);
				/*{{{  T9000_alpha_badlmul */
				if (T9000_alpha_badlmul (&tx_global)) {
					gensecondary (I_NOP);
					gencomment0 ("Time for the result to settle");
				}
				/*}}} */
				if (errormode & ERRORMODE_SHIFTCHECK)
					gensecondary (I_CSNGL);
				else
					throw_nested_result_away ();
			}
		}
		break;
		/*}}} */
		/*{{{  ROTATERIGHT ROTATELEFT */
	case PD_ROTATERIGHT:
	case PD_ROTATELEFT:
		/* TS/1979 30/11/92 - CON
		   NOTE that 'long' shifts are illegal in this implementation,
		   because the naive instruction sequence would produce an incorrect
		   result.
		 */
		{
			BOOL need_or = TRUE;
			const int old = switch_to_temp_workspace ();
			treenode *const zero = newconstant (0);
			switch_to_prev_workspace (old);
			if (pdno == PD_ROTATERIGHT) {
				switch (rotaterightsequence (param[0], param[1])) {
				case rotaterightsequence_ldl_ldl:
					tload3regs (P_EXP, param[0], P_EXP, param[0], P_EXP, param[1], loadseq, TRUE);
					need_or = FALSE;
					break;
				case rotaterightsequence_ldl_dup:
					texp (param[0], MANY_REGS);
					gensecondary (I_DUP);
					texp (param[1], 1);
					need_or = FALSE;
					break;
				case rotaterightsequence_ldl_ldc:
					tload3regs (P_EXP, param[0], P_EXP, zero, P_EXP, param[1], loadseq, TRUE);
					break;
				}
				if (!T9000_instruction_timings && !isconst (param[1]))
					checkerror ();	/* bug TS/1979 30/11/92 */
				gensecondary (I_LSHR);
			} else {
				tload3regs (P_EXP, zero, P_EXP, param[0], P_EXP, param[1], loadseq, TRUE);
				if (!T9000_instruction_timings && !isconst (param[1]))
					checkerror ();	/* bug TS/1979 30/11/92 */
				gensecondary (I_LSHL);
			}
			/*{{{  T9000_alpha_badlmul */
			if (T9000_alpha_badlmul (&tx_global)) {
				gensecondary (I_NOP);
				gencomment0 ("Time for the result to settle");
			}
			/*}}} */
			if (need_or)
				gensecondary (I_OR);
			else
				throw_nested_result_away ();
		}
		break;
		/*}}} */
		/*{{{  UNPACKSN */
	case PD_UNPACKSN:
		{
			const int old = switch_to_temp_workspace ();
			treenode *const zero = newconstant (0);
			switch_to_prev_workspace (old);
			tload2regs (P_EXP, zero, P_EXP, param[0], FALSE, TRUE);
			gensecondary (I_UNPACKSN);
			/*{{{  reorder destinations */
			{
				treenode **temp;
				temp = dest[0];
				dest[0] = dest[2];
				dest[2] = temp;
			}
			/*}}} */
			tstoreregs (dest, 3);
		}
		break;
		/*}}} */
		/*{{{  ROUNDSN */
	case PD_ROUNDSN:
		{
			int Yfracmode = P_EXP, Yguardmode = P_EXP;
			treenode *Yexp = param[0], *Yfrac = param[1], *Yguard = param[2];
			const int skiplab = newlab ();
			const int altadjust = insidealtguard ? alt_ds + 1 : 0;
			if (preeval (Yfracmode, Yfrac))
				/*{{{  load Yfrac to temporary */
			{
				texp (NDeclOf (Yfrac), MANY_REGS);
				storeinname (Yfrac, 0);
				Yfracmode = P_TEMP;
			}
			/*}}} */
			if (preeval (Yguardmode, Yguard))
				/*{{{  load Yguard to temporary */
			{
				texp (NDeclOf (Yguard), MANY_REGS);
				storeinname (Yguard, 0);
				Yguardmode = P_TEMP;
			}
			/*}}} */
			tload2regs (Yfracmode, Yfrac,	/*           Yfrac          */
				    Yguardmode, Yguard, TRUE, TRUE);	/*           Yguard         */
			gensecondary (I_OR);	/*           or             */
			genbranch (I_CJ, skiplab);	/*           cj     skiplab */
			/*{{{  adjust workspace for alt if necessary */
			genprimary (I_AJW, (int) (-altadjust));	/*           ajw    -n      */
			adjustworkspace (altadjust);
			/*}}} */
			texp (Yexp, MANY_REGS);	/*           Yexp           */
			genprimary (I_STL, 0);	/*           stl    0       */
			gencomment0 ("INT exponent");
			tload2regs (Yfracmode, Yfrac,	/*           Yfrac          */
				    Yguardmode, Yguard, FALSE, TRUE);	/*           Yguard         */
			/* T9000_alpha_badlmulpre: This can't be generated on a T9000 */
			gensecondary (I_NORM);	/*           norm           */
			/* T9000_alpha_badlmul: This can't be generated on a T9000 */
			gensecondary (I_POSTNORMSN);	/*           postnormsn     */
			/*{{{  adjust workspace for alt if necessary */
			genprimary (I_AJW, altadjust);	/*           ajw    n       */
			adjustworkspace ((int) (-altadjust));
			/*}}} */
			gensecondary (I_ROUNDSN);	/*           roundsn        */
			setlab (skiplab);	/* skiplab:                 */
		}
		break;
		/*}}} */
		/*{{{  CAUSEERROR */
	case PD_CAUSEERROR:
		if (!tx_global.hastserieserrors &&	/* bug TS/1547 07/04/92 */
		    !T9000_alpha_nocauseerror (&tx_global)) {	/* Section 5.8 of SW-0341-2 */
			genprimary (I_LDC, ERR_INTERR);
			gensecondary (I_CAUSEERROR);
		} else {
			new_occam_line (tptr, TRUE, TRUE, FALSE);
			gensecondary (I_SETERR);
		}
		checkerror ();
		break;
		/*}}} */
		/*{{{  BITREVWORD */
	case PD_BITREVWORD:
		texp (param[0], MANY_REGS);
		gensecondary (I_BITREVWORD);
		break;
		/*}}} */
		/*{{{  MINUSX */
	case PD_MINUSX:
		if (has_fpu_core)
			/*{{{  generate it inline with operand on floating point stack */
		{
			if (isconst (param[0]))
				tfpexp (param[0], MANY_REGS, MANY_REGS);
			else {
				treenode *temp = param[0];
				treenode *x = NDeclOf (temp);
				if (isaddressable (x))
					loadelement (x, 0, MANY_REGS, TRUE);
				else {
					tsimpleassign (ntypeof (temp), P_TEMP, temp, P_EXP, x, MANY_REGS);
					loadname (temp, 0);
				}
				gensecondary (I_MINT);
				/* looks OK for T9000_alpha_badmint bug */
				gensecondary (I_XOR);
				storeinname (temp, 0);
				tfpexp (temp, MANY_REGS, MANY_REGS);
			}
		}
		/*}}} */
		else {
			if (isconst (param[0]))
				texp (param[0], MANY_REGS);
			else {
				texp (param[0], MANY_REGS);
				gensecondary (I_MINT);
				gensecondary (I_XOR);
			}
		}
		break;
		/*}}} */
		/*{{{  DMINUSX */
	case PD_DMINUSX:
		/* has_fpu_core must be TRUE to reach here */
		{
			if (isconst (param[0]))
				tfpexp (param[0], MANY_REGS, MANY_REGS);
			else {
				int hi = swap_r64words ^ target_bigendian ? 0 : 1;	/* MDP */
				treenode *temp = param[0];
				tsimpleassign (S_REAL64, P_TEMP, temp, P_EXP, NDeclOf (temp), MANY_REGS);
				loadname (temp, hi);
				gensecondary (I_MINT);
				/* looks OK for T9000_alpha_badmint bug */
				gensecondary (I_XOR);
				storeinname (temp, hi);
				tfpexp (temp, MANY_REGS, MANY_REGS);
			}
		}
		break;
		/*}}} */
		/*{{{  DNOTFINITE ABS ISNAN MULBY2 DIVBY2 SQRT FPINT + double-length versions */
	case PD_ABS:
	case PD_DABS:
	case PD_ISNAN:
	case PD_DISNAN:
	case PD_DNOTFINITE:
	case PD_MULBY2:
	case PD_DMULBY2:
	case PD_DIVBY2:
	case PD_DDIVBY2:
	case PD_SQRT:
	case PD_DSQRT:
	case PD_FPINT:
	case PD_DFPINT:
		tfpexp (param[0], MANY_REGS, MANY_REGS);
		switch (pdno) {
		case PD_ABS:
		case PD_DABS:
			if (has_directfp) {
				gensecondary (I_FPABS);
			} else {
				genfpuentry (I_FPUABS);
			}
			break;
		case PD_ISNAN:
		case PD_DISNAN:
			gensecondary (I_FPNAN);
			genfppop ();	/* in case stack overflow must me avoided MDP/PZ */
			break;
		case PD_MULBY2:
		case PD_DMULBY2:
			if (has_directfp) {
				gensecondary (I_FPMULBY2);
			} else {
				genfpuentry (I_FPUMULBY2);
			}
			break;
		case PD_DIVBY2:
		case PD_DDIVBY2:
			if (has_directfp) {
				gensecondary (I_FPDIVBY2);
			} else {
				genfpuentry (I_FPUDIVBY2);
			}
			break;
		case PD_FPINT:
		case PD_DFPINT:
			gensecondary (I_FPINT);
			break;
		case PD_DNOTFINITE:
			gensecondary (I_FPNOTFINITE);
			genfppop ();	/* in case stack overflow must me avoided MDP/PZ */
			break;
		case PD_SQRT:
		case PD_DSQRT:
			if (has_fpsqrt) {
				gensecondary (I_FPSQRT);
			} else {
				/*{{{  fpusqrtfirst; fpusqrtstep; ...; fpusqrtlast */
				int count = (pdno == PD_SQRT) ? 2 : 5;
				genfpuentry (I_FPUSQRTFIRST);

				while (count-- > 0) {
					genfpuentry (I_FPUSQRTSTEP);
				}
				genfpuentry (I_FPUSQRTLAST);
				/*}}} */
			}
			break;
		}
		break;
		/*}}} */
		/*{{{  ORDERED DORDERED */
	case PD_ORDERED:
	case PD_DORDERED:
		tfpload2regs (param[0], param[1], MANY_REGS, MANY_REGS, TRUE);
		gensecondary (I_FPORDERED);
		genfppop ();
		genfppop ();	/* in case stack overflow must me avoided MDP/PZ */
		break;
		/*}}} */
		/*{{{  NOTFINITE */
	case PD_NOTFINITE:
		if (has_fpu_core) {
			tfpexp (param[0], MANY_REGS, MANY_REGS);
			gensecondary (I_FPNOTFINITE);
			genfppop ();	/* in case stack overflow must me avoided MDP/PZ */
		} else if (has_fp_support) {
			texp (param[0], MANY_REGS);
			gensecondary (I_LDINF);
			gensecondary (I_AND);
			gensecondary (I_LDINF);
			gensecondary (I_DIFF);
			genprimary (I_EQC, 0);
		} else {	/* to work on any 32 bit processor */

#if 0
			/* we couldn't get the infinity into the constant table */
			/* cos we re-generate the node here. We need to somehow use the */
			/* same one as was used when mapping */
			const int old = switch_to_temp_workspace ();
			treenode *const inf = newconstant (INFINITY);
			switch_to_prev_workspace (old);
			tload2regs (P_EXP, inf, P_EXP, param[0], FALSE, TRUE);
#endif /* 0 */
			texp (param[0], MANY_REGS);
			loadconstant (INFINITY);
			gensecondary (I_AND);
			genprimary (I_EQC, INFINITY);
		}
		break;
		/*}}} */
	#if 0
		/*{{{  LOADINPUTCHANNEL + friends */
	case PD_LOADINPUTCHANNEL:
	case PD_LOADOUTPUTCHANNEL:
		tsimpleassign (S_INT, P_EXP, param[0], chanaspointer ? P_EXP : P_PTR, param[1], MANY_REGS);
		break;
	case PD_LOADINPUTCHANNELVECTOR:
	case PD_LOADOUTPUTCHANNELVECTOR:
	case PD_LOADBYTEVECTOR:
		tsimpleassign (S_INT, P_EXP, param[0], P_PTR, param[1], MANY_REGS);
		break;
		/*}}} */
		/*{{{  KERNELRUN */
	case PD_KERNELRUN:
		{
			treenode *codeptr = param[0], *returnaddress = param[1], *workspaceend = param[2];
			const INT32 nparams = LoValOf (param[3]);
			const int returnlab = newlab ();
			SetNVOffset (returnaddress, returnlab);
			tload3regs (P_PTR, codeptr, P_PTR, returnaddress, P_PTR, workspaceend, loadseq, TRUE);
			gensecondary (I_GAJW);
			genprimary (I_AJW, -(nparams + 2));
			genprimary (I_STL, nparams + 1);
			genprimary (I_STL, 0);
			checkerror ();
			gensecondary (I_GCALL);
			throw_the_result_away ();	/* we do not use return address in Areg */
			setlab (returnlab);
			genprimary (I_LDL, nparams - 3);	/* was nparams + 3 ? */
			gensecondary (I_GAJW);
			throw_the_result_away ();
		}
		break;
		/*}}} */
	#endif
		/*{{{  MOVE2D CLIP2D DRAW2D */
	case PD_MOVE2D:
	case PD_CLIP2D:
	case PD_DRAW2D:
		{
			/*{{{  abbreviate the parameters to sensible names */
			treenode *source = param[0], *sourcestride = dimexpof (source, 1),
				*sx = param[1],
				*sy = param[2],
				*dst = param[3], *dststride = dimexpof (dst, 1),
				*dx = param[4],
				*dy = param[5],
				*width = param[6],
				*length = param[7],
				*sourceaddr = param[8],
				*dstaddr = param[9], *sxcheck = param[10], *sycheck = param[11], *dxcheck = param[12], *dycheck = param[13];
			/*}}} */
			int sxmode = P_EXP, symode = P_EXP, dxmode = P_EXP, dymode = P_EXP,
				widthmode = P_EXP, lengthmode = P_EXP, sourcemode = P_EXP, dstmode = P_EXP;
			int loadseq1, preeval_e2, preeval_e3;

			/* we have to simplify this first, so that we can't timeslice
			   between move2dinit and move2d etc. Bug 403 29/8/90
			   Note that some of the range checks depend upon the width.
			 */
			widthmode = simplify (widthmode, width);

			if (RANGECHECKING)
				/*{{{  do some checking */
			{
				sxmode = simplify (sxmode, sx);
				symode = simplify (symode, sy);
				dxmode = simplify (dxmode, dx);
				dymode = simplify (dymode, dy);
				lengthmode = simplify (lengthmode, length);
				if (sxcheck != NULL) {
					texp (sxcheck, MANY_REGS);
					gencomment0 ("Check sx");
					throw_the_result_away ();
				}
				if (sycheck != NULL) {
					texp (sycheck, MANY_REGS);
					gencomment0 ("Check sy");
					throw_the_result_away ();
				}
				if (dxcheck != NULL) {
					texp (dxcheck, MANY_REGS);
					gencomment0 ("Check dx");
					throw_the_result_away ();
				}
				if (dycheck != NULL) {
					texp (dycheck, MANY_REGS);
					gencomment0 ("Check dy");
					throw_the_result_away ();
				}
			}
			/*}}} */

			/* we have to simplify these first, so that we can't timeslice
			   between move2dinit and move2d etc. Bug 403 29/8/90
			 */
			sourcemode = simplify (sourcemode, sourceaddr);
			dstmode = simplify (dstmode, dstaddr);

			loadseq1 = giveorder (P_EXP, sourcestride, P_EXP, dststride, lengthmode, length, &preeval_e2, &preeval_e3);
			tload3regs (P_EXP, sourcestride, P_EXP, dststride, lengthmode, length, loadseq1, TRUE);
			gensecondary (I_MOVE2DINIT);

			tload3regs (sourcemode, sourceaddr, dstmode, dstaddr, widthmode, width, loadseq, TRUE);
			checkerror ();
			gensecondary ((pdno == PD_MOVE2D) ? I_MOVE2DALL : (pdno == PD_DRAW2D) ? I_MOVE2DNONZERO : I_MOVE2DZERO);
		}
		break;
		/*}}} */
		/*{{{  RESCHEDULE */
	case PD_RESCHEDULE:
		if (!tx_global.hastseriesscheduler) {
			gensecondary (I_TIMESLICE);
		} else {
#if 0
			genprimary (I_LDC, 0);	/* added MDP */
			genprimary (I_LDLP, 0);
			gensecondary (I_STARTP);
			gensecondary (I_STOPP);
#else
			/* new code added by frmb */
			genreschedule ();
#endif
		}
		genstartblock ();
		break;
		/*}}} */
		/*{{{  ASSERT */
	case PD_ASSERT:
		if (isconstexpnd (param[0])) {
			if (LoValOf (param[0]) == 0)	/* bug TS/1585 17/06/92 */
				generr (GEN_ASSERT_ERROR);
		} else if (	/*NEED_ERRORS && *//* bug TS/2060 26/01/93 */
				  !ignore_assertions) {
			const BOOL old_inside_assertion = inside_assertion;
			inside_assertion = TRUE;
			genboolassertion (param[0], TRUE, NULL, assert_errormode);
			inside_assertion = old_inside_assertion;
		}
		break;
		/*}}} */
		/*{{{  WSSIZEOF and VSSIZEOF */
	case PD_WSSIZEOF:
	case PD_VSSIZEOF:	/* bug TS/1797 18/08/92 */
		{
			INT32 ws, vs;
			treenode *const nptr = param[0];
			getprocwsandvs (nptr, &ws, &vs);
			genprimary (I_LDC, (pdno == PD_WSSIZEOF) ? ws : vs);
		}
		break;
		/*}}} */
		/*{{{  UPDATE_PROFCOUNT */
	#if 0
	case PD_UPDATE_PROFCOUNT:
		tprofcountupdate (LoValOf (param[0]));
		break;
	#endif
		/*}}} */
		/*{{{  ALLOC_CHAN_TYPE*/
#ifdef MOBILES
	case PD_ALLOC_CHAN_TYPE:
		{
			int chans, type_bytes, shared_server, shared_client;
			int pony_offset;
			int flags = 0;

			treenode *p0base = param[0];
			treenode *p1base = param[1];

			p0base = gettype_main_orig (param[0]);
			p1base = gettype_main_orig (param[1]);

			type_bytes = bytesin (p0base);
			shared_server = ((NTypeAttrOf (p0base) == (TypeAttr_marked_in | TypeAttr_shared)) ||
					(NTypeAttrOf (p1base) == (TypeAttr_marked_in | TypeAttr_shared)));
			shared_client = ((NTypeAttrOf (p0base) == (TypeAttr_marked_out | TypeAttr_shared)) ||
					(NTypeAttrOf (p1base) == (TypeAttr_marked_out | TypeAttr_shared)));
			if (kroc_chantype_knsf) {
				/* always need semaphores, plus extra bits here */
				flags |= MT_CB_SHARED | MT_CB_STATE_SPACE;
			} else if (shared_server || shared_client) {
				flags |= MT_CB_SHARED;
			}

			chans = (type_bytes / bytesperword);

			if (kroc_chantype_desc || kroc_chantype_knsf) {
				flags |= MT_CB_STATE_SPACE;
				pony_offset = chans;
			} else {
				pony_offset = 0x80000000;
			}

#if 0
fprintf (stderr, "tpredef (PD_ALLOC_CHAN_TYPE): p0base is: ");
printtreenl (stderr, 4, p0base);
fprintf (stderr, "tpredef (PD_ALLOC_CHAN_TYPE): p1base is: ");
printtreenl (stderr, 4, p1base);
fprintf (stderr, "tpredef: bytesin (NTypeOf (param[0])) = %d.  shared_server = %d, shared_client = %d.  extra_bytes = %d\n", type_bytes, shared_server, shared_client, extra_bytes);
fprintf (stderr, "tpredef: pony_offset = %d.  (param[0]) is ", pony_offset);
printtreenl (stderr, 4, (param[0]));
fprintf (stderr, "tpredef: NTypeAttrOf (NTypeOf (param[0])) = %d, NTypeAttrOf (NTypeOf (param[1])) = %d\n", NTypeAttrOf (NTypeOf (param[0])), NTypeAttrOf (NTypeOf (param[1])));
#endif

			/* maybe free existing channels involved */
			gencondfreedynchantype (param[0]);
			gencondfreedynchantype (param[1]);

			gencomment0 ("{{{ ALLOC.CHAN.TYPE:");

			/* allocate space and put in variables (type, ref-count and extra for sharing) */
			loadconstant (chans);
			loadconstant (MT_SIMPLE | MT_MAKE_TYPE (MT_CB) | flags);
			gensecondary (I_MT_ALLOC);
			genprimary (I_STL, 0);		/* store in local 0 */

			gencomment0 ("ALLOC.CHAN.TYPE: store pointer 0:");
			loadmobilepointer (param[0]);
			genprimary (I_LDL, 0);		/* load pointer */
			gensecondary (I_REV);
			genprimary (I_STNL, 0);		/* store in mobile */

			gencomment0 ("ALLOC.CHAN.TYPE: store pointer 1:");
			loadmobilepointer (param[1]);
			genprimary (I_LDL, 0);
			gensecondary (I_REV);
			genprimary (I_STNL, 0);

			if (kroc_chantype_desc || kroc_chantype_knsf) {
				gencomment0 ("ALLOC.CHAN.TYPE: initialise:");
			}

			if (kroc_chantype_desc) {
				treenode *mtype = NTypeOf (p0base);

				genloadlabptr (MTDLabOf (mtype), NOLAB, "MTD PTR");
				genprimary (I_LDL, 0);
				genprimary (I_STNL, pony_offset + PONY_TYPEDESC);
			}

			if (kroc_chantype_knsf) {
				/* initialise KRoC.net state fields */
				INT32 istate = 0;		/* unknown/invalid */

				gencomment0 ("ALLOC.CHAN.TYPE: (initialise KRoC.net state)");
				istate = (istate & 0xffff0000) | (shared_client ? 0x0002	/* shared, not claimed */
										: 0x0001);	/* not shared */
				istate = (istate & 0x0000ffff) | (shared_server ? 0x00020000	/* shared, not claimed */
										: 0x00010000);	/* not shared */
				loadconstant (istate);
				genprimary (I_LDL, 0);
				genprimary (I_STNL, pony_offset + PONY_STATE);
			}

			/* that ought to do it.. */
			gencomment0 ("}}}");
		}
		break;
#endif
		/*}}}*/
		/*{{{  DETACH_DYNMOB*/
	#ifdef PD_DETACH_DYNMOB
	case PD_DETACH_DYNMOB:
		/* param[0] is the mobile, param[1] address and param[2] count */
		loadmobile (param[0]);
		storeinopd (P_EXP, param[1], 0, MAXREGS - 1);
		loaddynmobilesize (param[0], 1);
		storeinopd (P_EXP, param[2], 0, MAXREGS - 1);
		genprimary (I_LDC, 0);
		storemobilesize (param[0], 1);	/* trash original MOBILE */
		break;
	#endif
		/*}}}*/
		/*{{{  ATTACH_DYNMOB*/
	#ifdef PD_ATTACH_DYNMOB
	case PD_ATTACH_DYNMOB:
		/* param[0] is the address, param[1] is the count and param[2] is the mobile */
#if 0
fprintf (stderr, "gen8: tpredef: ATTACH.DYNMOB: target (param[2]) = ");
printtreenl (stderr, 4, param[2]);
#endif
		gencondfreedynmobile (param[2]);
		texp (param[0], MANY_REGS);
		storemobile (param[2]);
		texp (param[1], MANY_REGS);
		storemobilesize (param[2], 1);
		break;
	#endif
		/*}}}*/
		/*{{{  DECODE_DATA*/
	case PD_DECODE_DATA:
		/* param[0] is the var, param[1] address and param[2] size (bytes); param[3] is the source size expression */
		texpopd (P_PTR, param[0], MANY_REGS);
		storeinopd (P_EXP, param[1], 0, MAXREGS - 1);
		texpopd (P_EXP, param[3], MANY_REGS);
		storeinopd (P_EXP, param[2], 0, MAXREGS - 1);
		break;
		/*}}}*/
		/*{{{  PROTOCOL_HASH*/
	#ifdef PD_PROTOCOL_HASH
	case PD_PROTOCOL_HASH:
		/* load run-time hash for parameter -- left on the stack */
#if 0
fprintf (stderr, "gen8: tpredef: PD_PROTOCOL_HASH: param[0] = ");
printtreenl (stderr, 4, param[0]);
#endif
		loadhiddentypeof (param[0], MANY_REGS);
		if (kroc_chantype_desc) {
			/* need to load typehash from inside descriptor */
			genprimary (I_LDNL, 2);
		}
		break;
	#endif
		/*}}}*/
		/*{{{  LOAD_TYPE_DESC*/
	#ifdef PD_LOAD_TYPE_DESC
	case PD_LOAD_TYPE_DESC:
		/* loads run-time type description */
		{
			treenode *mtype = param[0];

			switch (TagOf (mtype)) {
			case N_DECL:
			case N_PARAM:
			case N_ABBR:
			case N_RESULTPARAM:
				mtype = NTypeOf (mtype);
				break;
			}

			if (TagOf (mtype) == N_TYPEDECL) {
				mtype = NTypeOf (mtype);
			}

			if (isdeepmobiletype (mtype)) {
				genloadlabptr (MTDLabOf (mtype), NOLAB, "mobile type description");
			}
		}
		break;
	#endif
		/*}}}*/
		/*{{{  REAL32SIN, REAL64SIN, REAL32COS, REAL64COS, REAL32TAN, REAL64TAN */
	case PD_REAL32SIN:
	case PD_REAL64SIN:
	case PD_REAL32COS:
	case PD_REAL64COS:
	case PD_REAL32TAN:
	case PD_REAL64TAN:
		tfpexp (param[0], MANY_REGS, MANY_REGS);
		switch (pdno) {
		case PD_REAL32SIN:
			genextfpop (I_REAL32SIN);
			break;
		case PD_REAL64SIN:
			genextfpop (I_REAL64SIN);
			break;
		case PD_REAL32COS:
			genextfpop (I_REAL32COS);
			break;
		case PD_REAL64COS:
			genextfpop (I_REAL64COS);
			break;
		case PD_REAL32TAN:
			genextfpop (I_REAL32TAN);
			break;
		case PD_REAL64TAN:
			genextfpop (I_REAL64TAN);
			break;
		}
		break;
		/*}}}*/
		/*{{{  GETAFF / SETAFF */
	case PD_GETAFF:
		gensecondary (I_GETAFF);
		storeinopd (P_EXP, param[0], 0, MAXREGS - 1);
		break;
	case PD_SETAFF:
		texp (param[0], MANY_REGS);
		gensecondary (I_SETAFF);
		break;
		/*}}}*/
		/*{{{  KILLCALL */
	case PD_KILLCALL:
		texpopd (P_PTR, param[0], MANY_REGS);
		genkillcall ();
		texpopd (P_PTR, param[1], MANY_REGS - 1);
		genprimary (I_STNL, 0);
		break;
		/*}}}*/
		/*{{{  WAIT.FOR.INTERRUPT */
	case PD_WAIT_FOR_INTERRUPT:
		texp (param[1], MANY_REGS);
		texp (param[0], MANY_REGS);
		genwaitint ();
		storeinopd (P_EXP, param[2], 0, MAXREGS - 1);
		break;
		/*}}}*/
		/*{{{  BIND.MOBILE / BIND.MOBILE.HW */
	case PD_BIND_MOBILE:
	case PD_BIND_MOBILE_HW:
		texp (param[1], MANY_REGS);
		loadmobile_real (param[0]);
		genprimary (I_LDC, pdno == PD_BIND_MOBILE_HW ? MT_BIND_PHYSICAL : MT_BIND_VIRTUAL);
		gensecondary (I_MT_BIND);
		storemobile (param[0]);
		genmobileunpack (param[0], TRUE, FALSE);
		break;
		/*}}}*/
		/*{{{  DMA.CAPABLE */
	case PD_DMA_CAPABLE:
		/* loadmobile_real (param[0]); */
		{
			int skiplab = newlab ();
			
			loadmobile_real (param[0]);
			
			gensecondary (I_DUP);
			genbranch (I_CJ, skiplab);
			/* genchecknotnull (); */
			genprimary (I_LDNL, MTType); /* load type */
			genprimary (I_LDC, 0x1f1f | (MT_ARRAY_OPTS_DMA << MT_FLAGS_SHIFT) << 8);
			gensecondary (I_AND);
			genprimary (I_EQC, MT_MAKE_ARRAY_TYPE (1, MT_MAKE_ARRAY_OPTS (MT_ARRAY_OPTS_DMA, 0, 0)));
			gensecondary (I_DUP);
			setlab (skiplab);
			gensecondary (I_POP);
		}
		break;
		/*}}}*/
		/*{{{  MAKE.DMA.CAPABLE */
	case PD_MAKE_DMA_CAPABLE:
		gensecondary (I_NULL);
		loadmobile_real (param[0]);
		genprimary (I_LDC, MT_BIND_DMA);
		gensecondary (I_MT_BIND);
		storemobile (param[0]);
		genmobileunpack (param[0], TRUE, FALSE);
		break;
		/*}}}*/
		/*{{{  MEMORY.BARRIER */
	case PD_MEMORY_BARRIER:
		gensecondary (I_MB);
		break;
		/*}}}*/
		/*{{{  READ.MEMORY.BARRIER */
	case PD_READ_MEMORY_BARRIER:
		gensecondary (I_RMB);
		break;
		/*}}}*/
		/*{{{ WRITE.MEMORY.BARRIER */
	case PD_WRITE_MEMORY_BARRIER:
		gensecondary (I_WMB);
		break;
		/*}}}*/
		/*{{{  RESIZE.MOBILE.ARRAY.1D */
	case PD_RESIZE_MOBILE_ARRAY_1D:
		{
			treenode *basetype;
			int basebytes;
			mobilearray_base (param[0], &basetype, &basebytes);
			texp (param[1], MANY_REGS);
			if (!isscalartype (TagOf (basetype))) {
				loadconstant (basebytes);
				gensecondary (I_PROD);
			}
			loadmobile_real (param[0]);
			loadconstant (MT_RESIZE_DATA);
			gensecondary (I_MT_RESIZE);
			storemobile (param[0]);
			genmobileunpack (param[0], TRUE, FALSE);
			texp (param[1], MANY_REGS);
			storemobilesize (param[0], 1);
		}
		break;
		/*}}}*/
	default:
		badtag (LocnOf (tptr), TagOf (tptr), "tpredef");
		break;
	}
	/*}}} */
}

/*}}}*/



