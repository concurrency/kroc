/* $Id: gen5.c,v 1.8 1997/12/04 14:56:47 mdp2 Exp $ */

/*
 *	code generator - double-length expression generation
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
 *               ensure tidy disposal of unused carry/borrow for long PLUS/MINUS
 *               ensure tidy disposal of unused carry/borrow for long GT
 *               when eqc 0 is applied to bool, change to genboolinvert
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
#include "bind1def.h"
#include "bind2def.h"
#include "bind3def.h"
#include "gen1def.h"
#include "gen2def.h"
#include "gen4def.h"
#include "gen5def.h"
#include "gen8def.h"
#include "gen11def.h"
#include "gen12def.h"
#include "code1def.h"
/*}}}*/

/*{{{  typedefs*/
/*{{{  movelopd_t*/
typedef enum {
	movelopd_load_store,	/* load then store */
	movelopd_move,		/* block move */
	movelopd_fpu,		/* via FPU */
	movelopd_dup_dest,	/* use dup for dest */
	movelopd_dup_source	/* use dup for source */
} movelopd_t;
/*}}}*/
/*}}}*/

/*{{{  local variables*/
PRIVATE treenode **valofresult;
PRIVATE int valofresultmode;
/*}}}*/

/*{{{  support*/
/*{{{  PRIVATE BOOL issameopd (opdmode1, opd1, opdmode2, opd2)  ***/
/*****************************************************************************
 *
 *  issameopd returns TRUE if (opdmode1, opd1) and (opdmode2, opd2)
 *            represent the same variables.
 *
 *****************************************************************************/
PRIVATE BOOL issameopd (const int opdmode1, treenode * const opd1, const int opdmode2, treenode * const opd2)
{
	if ((opdmode1 == P_TEMP) || (opdmode2 == P_TEMP) || isconstopd (opdmode1, opd1) || isconstopd (opdmode2, opd2))
		return (FALSE);
	else if (isnotexpression (opd1) && isnotexpression (opd2))
		return (basedecl (opd1) == basedecl (opd2));
	else
		return (FALSE);
}

/*}}}*/
/*{{{  PRIVATE void gendoubleaddop (result.., left.., right.., ilow, ihigh)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  gendoubleaddop generates code for a double length additive operator.
 *                 ilow is the operation to be applied to the lower halves,
 *                 ihigh is the operation to be applied to the upper halves.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void gendoubleaddop (int resultmode, treenode * result, int leftmode, treenode * left, int rightmode, treenode * right, int ilow, int ihigh)
{
	BOOL lconst = isconstopd (leftmode, left);
	BOOL rconst = isconstopd (rightmode, right);

	/*{{{  do lower half */
	if (rconst && wordof (right, 0) == 0)
		/*{{{  optimise */
	{
		loadopd (leftmode, left, 0);	/* ld    leftlow        */
		storeinopd (resultmode, result, 0, MAXREGS - 1);	/* st    resultlow      */
		genprimary (I_LDC, 0);	/* ldc   0              */
	}
	/*}}} */
	else if ((ilow == I_LSUM) && lconst && (wordof (left, 0) == 0))
		/*{{{  optimise */
	{
		loadopd (rightmode, right, 0);	/* ld    rightlow       */
		storeinopd (resultmode, result, 0, MAXREGS - 1);	/* st    resultlow      */
		genprimary (I_LDC, 0);	/* ldc   0              */
	}
	/*}}} */
	else {
		genprimary (I_LDC, 0);	/* ldc   0              */
		loadopd (leftmode, left, 0);	/* ld    leftlow        */
		loadopd (rightmode, right, 0);	/* ld    rightlow       */
		gensecondary (ilow);	/* ilow                 */
		storeinopd (resultmode, result, 0, MAXREGS - 2);	/* st    resultlow      */
	}
	/*}}} */
	/*{{{  do upper half */
	loadopd (leftmode, left, 1);	/* ld    lefthigh       */
	loadopd (rightmode, right, 1);	/* ld    righthigh      */
	gensecondary (ihigh);	/* ihigh                */
	storeinopd (resultmode, result, 1, MAXREGS - 1);	/* st    resulthigh     */
	if ((ihigh == I_LSUM) || (ihigh == I_LDIFF)) {
		/* we do not use the carry/borrow result from ihigh */
		throw_the_result_away ();
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE BOOL crossoperator (op)*/
/*****************************************************************************
 *
 *  crossoperator returns TRUE if the double-length operator 'op' is a cross
 *                operator.
 *
 *****************************************************************************/
PRIVATE BOOL crossoperator (const int op)
{
	return ((op == S_TIMES) || (op == S_MULT));
}

/*}}}*/
/*{{{  PRIVATE int  complexity (tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  complexity is a heuristic which we use to decide which side of a
 *             (non-word length) dyadic operator to evaluate first.
 *             Basically,
 *               complexity(name) = 1
 *               complexity(constant) = 1
 *               complexity(monadic_op(op)) = complexity(op)
 *               complexity(dyadic_op(op1, op2)) =
 *                        (complexity(op1) == complexity(op2))      ?
 *                            complexity(op1) + 1                   :
 *                            max(complexity(op1), complexity(op2))
 *               complexity(anything else) = 100
 *
 *****************************************************************************/
/*}}}*/
PRIVATE int complexity (treenode * tptr)
{
	switch (TagOf (tptr)) {
		/*{{{  monadic operators */
	case S_NEG:
	case S_BITNOT:
		return (complexity (OpOf (tptr)));
		/*}}} */
		/*{{{  conversion */
	case S_EXACT:
	case S_ROUND:
	case S_TRUNC:
		return (complexity (OpOf (tptr)));
		/*}}} */
		/*{{{  dyadic operators */
	case S_ADD:
	case S_SUBTRACT:
	case S_MULT:
	case S_DIV:
	case S_REM:
	case S_BITAND:
	case S_BITOR:
	case S_XOR:
	case S_PLUS:
	case S_MINUS:
	case S_TIMES:
		{
			const int leftc = complexity (LeftOpOf (tptr));
			const int rightc = complexity (RightOpOf (tptr));
			return ((leftc == rightc) ? leftc + 1 : (int) max_INT32 (leftc, rightc));
		}
		/*}}} */
		/*{{{  shifts */
	case S_LSHIFT:
	case S_RSHIFT:
		return (complexity (LeftOpOf (tptr)));
		/*}}} */
		/*{{{  function instance/valof      NO LONGER USED */
#if 0				/* now use the default! */
	case S_FINSTANCE:
	case S_VALOF:
		return (100);
#endif
		/*}}} */
		/*{{{  elements */
	case S_ARRAYITEM:
	case S_RECORDITEM:
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_VALPARAM:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_DECL:
	case T_PREEVALTEMP:
	case N_REPL:
	case S_FNFORMALRESULT:	/* INSdi03203 */
	case S_CONSTEXP:
	case S_SIZE:		/* bug 1351 21/8/91 *//* all SIZEs are stored to temps */
		return (1);
	case T_TEMP:
		return (complexity (NDeclOf (tptr)));
		/*}}} */
	default:
		/*badtag(genlocn, TagOf(tptr), "complexity"); */
		return 100;	/* bug TS/1546 03/01/92 */
	}
/*return (0); *//* Not reached */
}

/*}}}*/
/*{{{  PRIVATE BOOL loadlopdptr (opdmode, opd)*/
/*****************************************************************************
 *
 *  loadlopdptr returns TRUE if we should preevaluate a pointer to double-
 *              length operand (opdmode, opd)
 *
 *****************************************************************************/
PRIVATE BOOL loadlopdptr (const int opdmode, treenode * const opd)
{
	return !(issimpleopd (opdmode, opd) || isconstopd (opdmode, opd) ||
		 ((opdmode == P_EXP) && (TagOf (opd) == S_ARRAYITEM || TagOf (opd) == S_RECORDITEM) && (ASExpOf (opd) == NULL)));
}

/*}}}*/
/*}}}*/

/*{{{  PRIVATEPARAM void maplvalof (treenode *valoftree)*/
PRIVATEPARAM void maplvalof (treenode * valoftree)
{
	mapprocess (VLBodyOf (valoftree));
	mapmovelopd (valofresultmode, valofresult, P_EXP, ThisItemAddr (VLResultListOf (valoftree)));
}

/*}}}*/
/*{{{  PRIVATEPARAM void mapqvalof (treenode *valoftree)*/
PRIVATEPARAM void mapqvalof (treenode * valoftree)
{
	mapprocess (VLBodyOf (valoftree));
	mapmoveqopd (valofresultmode, valofresult, P_EXP, ThisItemAddr (VLResultListOf (valoftree)));
}

/*}}}*/
/*{{{  PRIVATE int mappreparelopd (opdmode, opd, tempmade)*/
/*****************************************************************************
 *
 *  mappreparelopd allocates a temporary in which to load a pointer to opd
 *                 if neccesary.
 *
 *****************************************************************************/
PRIVATE int mappreparelopd (int opdmode, treenode ** opd, BOOL * const tempmade)
{
	DEBUG_MSG (("mappreparelopd: opdmode = %d\n", opdmode));
	if (opdmode != P_TEMP) {
		if (loadlopdptr (opdmode, *opd))
			/*{{{  load a pointer to opd into a temporary */
		{
			/* Note that here we load a pointer to the operand into
			   a temporary, but we still want to access the operand, not a
			   pointer to it, so we DON'T do
			   opdmode = tempmodeof(ptrmodeof(opd))   */
			DEBUG_MSG (("mappreparelopd: creating a pointer temporary\n"));
			mapexpopd (ptrmodeof (opdmode), opd);
			*opd = gettemp (*opd, NM_POINTER);
			upusecount (*opd, 1);
			opdmode = tempmodeof (opdmode);
			*tempmade = TRUE;
		}
		/*}}} */
		else {
			DEBUG_MSG (("mappreparelopd: NOT creating a pointer temporary\n"));
			mapexpopd (opdmode, opd);
		}
	} else
		upusecount (*opd, 1);
	return (opdmode);
}

/*}}}*/
/*{{{  PUBLIC BOOL temp_required_for_fn_dest*/
PUBLIC BOOL temp_required_for_fn_dest (treenode * const dest, treenode * const instance)
/*{{{  comment*/
/*
 *
 * This is called with a name of a variable which is to be used as the destination
 * of a function call. It also receives the 'instance' of the function call.
 *
 * It is already known that the destination must be passed as a pointer
 * to the function, either because there are too many parameters, or because
 * it doesn't fir into a register.
 *
 * This routine returns TRUE if there is a possible alias, which therefore
 * means that we should put the result into a temporary before copying it
 * into the real destination.
 *
 */
/*}}}*/
{
	treenode *paramlist;

	if (NPSafeFnResultOf (INameOf (instance))) {
		/* This flag is only set if we know that the function doesn't read
		   _anything_ after writing to this destination.
		   Therefore, if so, we can assume that no temporary is needed.
		   The original fix for TS/2024 broke in the last test in this
		   routine (the calls to usedin for each actual parameter) because
		   usedin always returns TRUE for aliasable vars.
		   Thus it was always creating a temp if there were any arguments
		   to the function!
		   CON - INSdi02356 15/07/93
		 */
		return FALSE;
	}

	if (!checkalias)
		return TRUE;	/* bug TS/1563 26/05/92 */

	{
		treenode *const basename = nameof (dest);
		if ((nodetypeoftag (TagOf (basename)) == NAMENODE) && NVAliasedOf (basename))
			return TRUE;
	}

	if (isafreevarof (basedecl (dest), INameOf (instance)))
		return TRUE;

	for (paramlist = IParamListOf (instance); !EndOfList (paramlist); paramlist = NextItem (paramlist))
		if (isaddressable (ThisItem (paramlist)) && usedin (dest, ThisItem (paramlist), be_lexlevel))
			return TRUE;

	return FALSE;
}

/*}}}*/
/*{{{  PRIVATE void mapfinstance*/
PRIVATE void mapfinstance (const int resultmode, treenode ** const result, treenode ** const opd)
{
	treenode *paramlist;
	BOOL maketempresult;

	if (TagOf (INameOf (*opd)) == N_PREDEFFUNCTION) {
		/*{{{  map the predef, or convert it to a library call */
		const int old = switch_to_temp_workspace ();
		treenode *dest = addtofront (*result, NULL);
		switch_to_prev_workspace (old);
		if (mappredef (*opd, dest)) {
			return;	/* it was done inline */
		}
		/*}}} */
	}

	paramlist = IParamListOf (*opd);
	maketempresult = ((resultmode == P_EXP) || (resultmode == P_PTR)) && temp_required_for_fn_dest (*result, *opd);
	if (maketempresult)
		/*{{{  generate fn. result to temporary, then after call move to result */
	{
		*opd = gettemp (*opd, NM_WORKSPACE);
		mapfinstance (P_TEMP, opd, NDeclAddr (*opd));
		mapmoveqopd (resultmode, result, P_TEMP, opd);
		freetemp (*opd);
	}
	/*}}} */
	else
		/*{{{  generate fn. to result */
	{
		/*{{{  augment actual parameters */
		{
			const int old = switch_to_temp_workspace ();
			treenode *dest = addtofront (*result, NULL);
			switch_to_prev_workspace (old);
			paramlist = augmentparams (paramlist, FnParamsOf (NTypeOf (INameOf (*opd))), dest, *opd);
			SetIParamList (*opd, paramlist);
		}
		/*}}} */
		mapinstance (*opd);
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE void mapqopandassign*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  mapqopandassign maps the evaluation of expression 'opd' into
 *                  (resultmode, result).
 *                  The calling of this routine is such that we can always
 *                  generate into result.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void mapqopandassign (int resultmode, treenode ** result, treenode ** opd)
{
	while (TRUE) {
		int op = TagOf (*opd);
		const SOURCEPOSN locn = LocnOf (*opd);
		const int type = ntypeof (*opd);
		if (isreal (type)) {
			/*{{{  pick out special real cases */
			switch (op) {
			default:
				break;	/* Fall out to other cases */
				/*{{{  ADD SUBTRACT MULT DIV REM */
			case S_ADD:
			case S_SUBTRACT:
			case S_MULT:
			case S_DIV:
			case S_REM:
				*opd = makedopfunction (*opd);
				break;
				/*}}} */
				/*{{{  NEG */
			case S_NEG:
				{
					treenode *const zeroopd = newrealconstant (0, 0, "0.0", type);
					*opd = newdopnode (S_SUBTRACT, locn, zeroopd, OpOf (*opd), type);
					*opd = makedopfunction (*opd);
				}
				break;
				/*}}} */
				/*{{{  EXACT ROUND TRUNC */
			case S_EXACT:
			case S_ROUND:
			case S_TRUNC:
				{
					treenode *source = OpOf (*opd);
					int sourcetype = ntypeof (source);
					int desttype = MOpTypeOf (*opd);
					if (sourcetype == S_INT)
						sourcetype = targetintsize;
#if 0				/* this was done in trans */
					if (sourcetype == desttype) {
						*opd = OpOf (*opd);	/* Remove the conversion */
						mapmoveqopd (resultmode, result, P_EXP, opd);
						return;
					} else
#endif
					{
						if (isshorttype (sourcetype))
							/*{{{  convert via source -> INT -> dest */
						{
							source = newmopnode (S_EXACT, locn, source, S_INT);
							SetOp (*opd, source);
							sourcetype = S_INT;
						}
						/*}}} */
						*opd = makeconversion (sourcetype, desttype, source, op, locn);
					}
				}
				break;
				/*}}} */
			}
			op = TagOf (*opd);
			/*}}} */
		}
		switch (op) {
		default:
			badtag (genlocn, op, "mapqopandassign");
			break;
			/*{{{  dyadics  -> function calls */
		case S_ADD:
		case S_SUBTRACT:
		case S_MULT:
		case S_DIV:
		case S_REM:
		case S_PLUS:
		case S_MINUS:
		case S_TIMES:
		case S_BITAND:
		case S_BITOR:
		case S_XOR:
		case S_LSHIFT:
		case S_RSHIFT:
			*opd = makedopfunction (*opd);
			break;
			/*}}} */
			/*{{{  monadics -> dyadic function calls */
		case S_UMINUS:
		case S_NEG:
			{
				const int mop_type = MOpTypeOf (*opd);
				*opd = newdopnode ((op == S_NEG) ? S_SUBTRACT : S_MINUS, locn, newintconstant (0, mop_type), OpOf (*opd), mop_type);
			}
			break;
			/*}}} */
			/*{{{  monadics -> monadic function calls */
		case S_BITNOT:
			{
				const int mop_type = MOpTypeOf (*opd);
				*opd = newinstancenode (S_FINSTANCE, locn,
							libentry (libcallstring (op, mop_type), locn), newlistnode (S_LIST, locn, OpOf (*opd), NULL));
			}
			break;
			/*}}} */
			/*{{{  conversions -> function calls */
		case S_EXACT:
		case S_ROUND:
		case S_TRUNC:
			{
				treenode *source = OpOf (*opd);
				int sourcetype = ntypeof (source);
				int desttype = MOpTypeOf (*opd);
#if 0				/* this was done in trans */
				if (sourcetype == desttype) {
					*opd = source;	/* Remove the conversion */
					mapmoveqopd (resultmode, result, P_EXP, opd);
					return;
				} else
#endif
				{
					if (isshorttype (sourcetype))
						/*{{{  convert source to an integer first */
					{
						source = newmopnode (S_EXACT, LocnOf (source), source, S_INT);
						sourcetype = S_INT;
					}
					/*}}} */
					*opd = makeconversion (sourcetype, desttype, source, op, locn);
				}
			}
			break;
			/*}}} */
			/*{{{  function call */
		case S_FINSTANCE:
			mapfinstance (resultmode, result, opd);
			return;
			/*}}} */
			/*{{{  specification .. VALOF */
		case S_VALABBR:
		case S_ABBR:
		case S_VALRETYPE:
		case S_RETYPE:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_DECL:
		case S_VALOF:
		case S_PRAGMA:	/* bug 829 19/9/91 */
		case S_TYPEDECL:
#ifdef MOBILES
		case S_FORWDECL:
		case S_PROCTYPEDECL:
		case S_MPROCDECL:
#endif
			{
				/* Save statics here, just in case we have valof within valof */
				treenode **savedvalofresult = valofresult;
				int savedvalofresultmode = valofresultmode;
				valofresult = result;
				valofresultmode = resultmode;
				mapdeclandbody (*opd, mapqvalof, FALSE, FALSE);
				valofresult = savedvalofresult;
				valofresultmode = savedvalofresultmode;
			}
			return;
			/*}}} */
		}
	}
}

/*}}}*/
/*{{{  PRIVATE void maplopandassign*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  maplopandassign maps the evaluation of expression 'opd' into
 *                  doubgle length (resultmode, result).
 *                  The calling of this routine is such that we can always
 *                  generate into result.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void maplopandassign (int resultmode, treenode ** result, int leftmode, int rightmode, treenode ** opd)
{
	const int op = TagOf (*opd);
	const SOURCEPOSN locn = LocnOf (*opd);
	const int type = ntypeof (*opd);
	if (isreal (type))
		/*{{{  pick out special real cases */
		switch (op) {
		default:
			break;	/* Fall out to other cases */
			/*{{{  ADD SUBTRACT MULT DIV REM */
		case S_ADD:
		case S_SUBTRACT:
		case S_MULT:
		case S_DIV:
		case S_REM:
			/*{{{  COMMENT what the tree looks like */
	  /**********************  Start comment out ****************************
          @*
                  rem                    finstance
                /    \       =>         /       \
               e1    e2          real64rem      list
                                                /   \
                                              e1    list
                                                    /   \
                                                  e2    NULL
          *@
           **********************   End comment out  ****************************/
			/*}}} */
			*opd = makedopfunction (*opd);
			maplopandassign (resultmode, result, P_EXP, P_EXP, opd);
			return;
			/*}}} */
			/*{{{  NEG */
		case S_NEG:
			{
				treenode *const zeroopd = newrealconstant (0, 0, "0.0", type);
				*opd = newdopnode (S_SUBTRACT, locn, zeroopd, OpOf (*opd), type);
				maplopandassign (resultmode, result, leftmode, rightmode, opd);
			}
			return;
			/*}}} */
			/*{{{  EXACT ROUND TRUNC */
		case S_EXACT:
		case S_ROUND:
		case S_TRUNC:
			/* the destination is a double-length real */
			{
				treenode *source = OpOf (*opd);
				int sourcetype = ntypeof (source);
				const int desttype = MOpTypeOf (*opd);
				if (sourcetype == S_INT)
					sourcetype = targetintsize;
#if 0				/* this was done in trans */
				if (sourcetype == desttype)
					/*{{{  ignore null conversion */
				{
					*opd = OpOf (*opd);	/* Remove the conversion */
					mapmovelopd (resultmode, result, leftmode, opd);
				}
				/*}}} */
				else
#endif
				{
					if (isshorttype (sourcetype))
						/*{{{  convert via source -> INT -> dest */
					{
						source = newmopnode (S_EXACT, locn, source, S_INT);
						SetOp (*opd, source);
						sourcetype = S_INT;
					}
					/*}}} */
					*opd = makeconversion (sourcetype, desttype, source, op, locn);
					maplopandassign (resultmode, result, leftmode, rightmode, opd);
				}
			}
			return;
			/*}}} */
		}
	/*}}} */
	/*{{{  cases */
	switch (op) {
		/*{{{  MULT DIV REM */
	case S_MULT:
	case S_DIV:
	case S_REM:
		/* Make a function call up, recurse to map it */
		/*{{{  what the transformed tree looks like */
		/*
		   result
		   .                            S_FINSTANCE
		   .             -\             /     \
		   S_MULT          -/  N_STDLIBFUNCDEF   LIST
		   /  \                      /         /   \
		   left right               name       left  LIST
		   /   \
		   right  NULL

		 */
		/*}}} */
		DEBUG_MSG (("maplopandassign: turning MULT/DIV/REM into fn, then recursing\n"));
		*opd = makedopfunction (*opd);
		maplopandassign (resultmode, result, leftmode, rightmode, opd);
		break;
		/*}}} */
		/*{{{  FINSTANCE */
	case S_FINSTANCE:
		mapfinstance (resultmode, result, opd);
		break;
		/*}}} */
		/*{{{  ADD SUBTRACT PLUS MINUS TIMES BITAND BITOR XOR */
	case S_ADD:
	case S_SUBTRACT:
	case S_PLUS:
	case S_MINUS:
	case S_TIMES:
	case S_BITAND:
	case S_BITOR:
	case S_XOR:
		{
			treenode **left = LeftOpAddr (*opd);
			treenode **right = RightOpAddr (*opd);
			const BOOL leftaddressable = isaddressableopd (leftmode, *left) || isconstopd (leftmode, *left);
			const BOOL rightaddressable = isaddressableopd (rightmode, *right) || isconstopd (rightmode, *right);
/*int swapped = FALSE; *//* never used - CON 8/2/91 */
			BOOL freelefttemp = FALSE;
			BOOL freerighttemp = FALSE;
			BOOL freeresulttemp = FALSE;

			DEBUG_MSG (("maplopandassign: inline-op: leftaddr?:%d rightaddr:?%d\n", leftaddressable, rightaddressable));

			/*{{{  prepare the result */
			resultmode = mappreparelopd (resultmode, result, &freeresulttemp);
			/*}}} */
			/*{{{  set up left and right operands */
			if (leftaddressable && rightaddressable)
				/*{{{  left and right addressable */
			{
				leftmode = mappreparelopd (leftmode, left, &freelefttemp);
				rightmode = mappreparelopd (rightmode, right, &freerighttemp);
			}
			/*}}} */
			else if (leftaddressable || rightaddressable)
				/*{{{  one side addressable, the other side not addressable */
			{
				if (rightaddressable)
					/*{{{  swap left and right */
				{
					/* NOTE that we don't actually swap the stuff on the tree! */
					treenode **tempptr;
					int tempmode;
					tempptr = left;
					left = right;
					right = tempptr;
					tempmode = leftmode;
					leftmode = rightmode;
					rightmode = tempmode;
/*swapped = TRUE; *//* never used */
				}
				/*}}} */

				/* TIMES uses a cross-multiply, so we cannot generate the
				   result into either of the operands directly: if either
				   of the operands is the same location as the result, we
				   must generate into a temporary.
				   For the other operations, we can generate one operand into the result
				   provided the other operand does not use the result. */
				if (usedinopd (resultmode, *result, *left, be_lexlevel) || crossoperator (op))
				 {	/* INSdi03354 */
					/*{{{  generate right into temporary */
					*right = gettemp (*right, NM_WORKSPACE);
					rightmode = P_TEMP;
					freerighttemp = TRUE;
					mapmovelopd (P_TEMP, right, P_EXP, NDeclAddr (*right));
					upusecount (*right, 1);
				}
				/*}}} */
				else
					/*{{{  generate right into result */
					mapmovelopd (resultmode, result, rightmode, right);
				/*}}} */
				leftmode = mappreparelopd (leftmode, left, &freelefttemp);
			}
			/*}}} */
			else
				/*{{{  neither left nor right addressable */
			{
				const BOOL usedinleft = usedinopd (resultmode, *result, *left, be_lexlevel);
				const BOOL usedinright = usedinopd (resultmode, *result, *right, be_lexlevel);

				DEBUG_MSG (("maplopandassign: inline-op: usedinleft?:%d usedinright:?%d\n", usedinleft, usedinright));

				if (usedinleft || usedinright || crossoperator (op))
				 {	/* INSdi02423 930810 */
					/*{{{  use two temporaries */
					/*{{{  comment explaining it - bug 1274 21/5/91 */
					/* bug 1274 - 21/5/91
					   There is a major problem if we recurse on the left and right
					   sub-expressions, where the 'destination' is used in one of the
					   expressions. It is because the mapper and the code generator
					   do not do exactly the same thing with double length function
					   instances, in the particular case where the result of the function
					   is aliased by one of the parameters.
					   (The example which first found the bug is:
					   var1 := (var1 * const1) + (var2 * const2), both double length,
					   where the multiplies get turned into function calls)

					   We get around this by making sure that the 'destination' can never
					   alias by always making the 'destination' of each sub-expression
					   into a temporary.

					   Note that the `simpler' case: "var1 := var1 * const1" works because
					   "movelopd" explicitly generates to a temporary and copies
					   the temporary back onto the result.
					 */
					/*}}} */

					const BOOL leftfirst = complexity (*left) <= complexity (*right);

					if (!leftfirst) {	/* first do the right */
						DEBUG_MSG (("maplopandassign: do right first\n"));
						*right = gettemp (*right, NM_WORKSPACE);
						rightmode = P_TEMP;
						freerighttemp = TRUE;
						mapmovelopd (P_TEMP, right, P_EXP, NDeclAddr (*right));
					}

					/*if (TRUE) */
					{
						DEBUG_MSG (("maplopandassign: making left into temporary\n"));
						*left = gettemp (*left, NM_WORKSPACE);
						leftmode = P_TEMP;
						freelefttemp = TRUE;
						mapmovelopd (P_TEMP, left, P_EXP, NDeclAddr (*left));
					}

					if (leftfirst) {	/* now do the right */
						DEBUG_MSG (("maplopandassign: now do the right\n"));
						*right = gettemp (*right, NM_WORKSPACE);
						rightmode = P_TEMP;
						freerighttemp = TRUE;
						mapmovelopd (P_TEMP, right, P_EXP, NDeclAddr (*right));
					}
				}
				/*}}} */
				else
					/*{{{  generate most complex operand to result, least complex to temporary */
				{
					if (complexity (*right) > complexity (*left))
						/*{{{  swap left and right */
					{
						/* NOTE that we don't actually swap the stuff on the tree! */
						treenode **tempptr;
						int tempmode;
						tempptr = left;
						left = right;
						right = tempptr;
						tempmode = leftmode;
						leftmode = rightmode;
						rightmode = tempmode;
/*swapped = TRUE; *//*never used */
					}
					/*}}} */

					/*{{{  generate left to result, right into temporary */
					{
						mapmovelopd (resultmode, result, leftmode, left);
						*right = gettemp (*right, NM_WORKSPACE);
						rightmode = P_TEMP;
						freerighttemp = TRUE;
						mapmovelopd (P_TEMP, right, P_EXP, NDeclAddr (*right));
					}
					/*}}} */
				}
				/*}}} */
			}
			/*}}} */
			/*}}} */
			/*{{{  perform operation */
			if (crossoperator (op)) {
				if (issameopd (resultmode, *result, leftmode, *left) || issameopd (resultmode, *result, rightmode, *right))
					/*{{{  generate into a temporary, then move to result */
				{
					DEBUG_MSG (("maplopandassign: creating temp for crossoperator\n"));
					*opd = gettemp (*opd, NM_WORKSPACE);
					upusecount (*opd, 2);
				}
				/*}}} */
			}
			/* other operations need no further mapping */
			/*}}} */
			/*{{{  free temporaries */
			if (freelefttemp) {
				freetemp (*left);
				DEBUG_MSG (("maplopandassign: freeing left\n"));
			}
			if (freerighttemp) {
				freetemp (*right);
				DEBUG_MSG (("maplopandassign: freeing right\n"));
			}
			if (freeresulttemp) {
				freetemp (*result);
				DEBUG_MSG (("maplopandassign: freeing result\n"));
			}
			/*}}} */
		}
		break;
		/*}}} */
		/*{{{  BITNOT */
	case S_BITNOT:
		{
			treenode **operand = OpAddr (*opd);
			int operandmode = leftmode;
			BOOL freeresulttemp = FALSE, freeoperandtemp = FALSE;
			/*{{{  prepare the result */
			resultmode = mappreparelopd (resultmode, result, &freeresulttemp);
			/*}}} */
			if (isaddressableopd (operandmode, *operand))
				operandmode = mappreparelopd (operandmode, operand, &freeoperandtemp);
			else
				mapmovelopd (resultmode, result, operandmode, operand);
			/* perform the operation */
			/*{{{  free temporaries */
			if (freeoperandtemp)
				freetemp (*operand);
			if (freeresulttemp)
				freetemp (*result);
			/*}}} */
		}
		break;
		/*}}} */
		/*{{{  NEG UMINUS */
	case S_NEG:
	case S_UMINUS:
		{
			const int mop_type = MOpTypeOf (*opd);
			const int tag = (op == S_NEG) ? S_SUBTRACT : S_MINUS;
			*opd = newdopnode (tag, locn, newconstant (0), OpOf (*opd), mop_type);
			maplopandassign (resultmode, result, leftmode, rightmode, opd);
		}
		break;
		/*}}} */
		/*{{{  LSHIFT RSHIFT */
	case S_LSHIFT:
	case S_RSHIFT:
		{
			treenode **left = LeftOpAddr (*opd);
			BOOL freeresulttemp = FALSE, freelefttemp = FALSE;
			/*{{{  prepare the result */
			resultmode = mappreparelopd (resultmode, result, &freeresulttemp);
			/*}}} */
			if (isaddressableopd (leftmode, *left) || isconstopd (leftmode, *left))
				leftmode = mappreparelopd (leftmode, left, &freelefttemp);
			else
				mapmovelopd (resultmode, result, leftmode, left);
			mapexpopd (rightmode, RightOpAddr (*opd));
			/* perform the operation */
			/*{{{  free temporaries */
			if (freelefttemp)
				freetemp (*left);
			if (freeresulttemp)
				freetemp (*result);
			/*}}} */
		}
		break;
		/*}}} */
		/*{{{  EXACT */
	case S_EXACT:
		{
			treenode **operand = OpAddr (*opd);
			int sourcetype = ntypeof (*operand);
			int desttype = MOpTypeOf (*opd);
			BOOL freeresulttemp = FALSE;
#if 0				/* this was done in trans */
			if (sourcetype == desttype)
			 {	/* null conversion */
				/*{{{  ignore null conversion */
				*opd = OpOf (*opd);	/* Remove the conversion */
				mapmovelopd (resultmode, result, leftmode, opd);
			}
			/*}}} */
			else
#endif
			if (fitsinregister (sourcetype)) {
				/*{{{  prepare the result */
				resultmode = mappreparelopd (resultmode, result, &freeresulttemp);
				/*}}} */
				mapexpopd (leftmode, operand);
				/* xdble */
				/*{{{  free temporaries */
				if (freeresulttemp)
					freetemp (*result);
				/*}}} */
			} else if (isquadlength (sourcetype))
				/*{{{  convert to a function call */
			{
				*opd = makeconversion (sourcetype, desttype, *operand, op, locn);
				maplopandassign (resultmode, result, P_EXP, P_EXP, opd);
			}
			/*}}} */
			else
				badtag (genlocn, op, "maplopandassign");
		}
		break;
		/*}}} */
		/*{{{  ROUND TRUNC */
	case S_ROUND:
	case S_TRUNC:
		/* The destination type is a double-length integer */
		{
			treenode *source = OpOf (*opd);
			int sourcetype = ntypeof (source);
			int desttype = MOpTypeOf (*opd);
#if 0				/* we've done this in trans */
			if (sourcetype == desttype)
				/*{{{  null conversion */
			{
				*opd = OpOf (*opd);	/* Remove the conversion */
				mapmovelopd (resultmode, result, leftmode, opd);
			}
			/*}}} */
#endif
			if (has_fpu_core && isreal (sourcetype) && !T9000_alpha_nofpconv (&tx_global)) {	/* Sections 4.2.11 and 4.2.12 of SW-0341-2 */
#if 0				/* This breaks when rhs is complicated: bug 739 24/9/90 */
				mapfpexp (OpAddr (*opd));
				resultmode = ptrmodeof (resultmode);
				mapexpopd (resultmode, result);
#else
				BOOL freeresulttemp = FALSE;
				resultmode = mappreparelopd (resultmode, result, &freeresulttemp);
				mapfpexp (OpAddr (*opd));
				if (freeresulttemp)
					freetemp (*result);
#endif
			} else {
				*opd = makeconversion (sourcetype, desttype, source, op, locn);
				maplopandassign (resultmode, result, leftmode, rightmode, opd);
			}
		}
		break;
		/*}}} */
		/*{{{  specification .. VALOF */
	case S_VALABBR:
	case S_ABBR:
	case S_VALRETYPE:
	case S_RETYPE:
	case S_PROCDEF:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
	case S_TPROTDEF:
	case S_SPROTDEF:
	case S_DECL:
	case S_VALOF:
	case S_PRAGMA:		/* bug 829 19/9/91 */
	case S_TYPEDECL:
#ifdef MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
	case S_MPROCDECL:
#endif
		{
			/* Save statics here, just in case we have valof within valof */
			treenode **savedvalofresult = valofresult;
			int savedvalofresultmode = valofresultmode;
			valofresult = result;
			valofresultmode = resultmode;
			mapdeclandbody (*opd, maplvalof, FALSE, FALSE);
			valofresult = savedvalofresult;
			valofresultmode = savedvalofresultmode;
		}
		break;
		/*}}} */
	default:
		badtag (genlocn, op, "maplopandassign");
	}
	/*}}} */
}

/*}}}*/

/*{{{  PRIVATE int preparelopd (opdmode, opd)*/
/*****************************************************************************
 *
 *  preparelopd generates any neccessary preparation code for (opdmode, opd)
 *              ie. loads a pointer to it into a temp.
 *
 *****************************************************************************/
PRIVATE int preparelopd (int opdmode, treenode * opd)
{
	if (preeval (opdmode, opd)) {
		loadelementpointer (NDeclOf (opd), 0, MANY_REGS);
		storeinname (opd, 0);
		opdmode = tempmodeof (opdmode);
	}
	return (opdmode);
}

/*}}}*/
/*{{{  PRIVATE void evallopd (opdmodevar, opdvar, destmode, dest)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  evallopd evaluates (*opdmodevar, *opdvar) either to a temporary or to
 *           (destmode, dest).
 *           A bit untidy because we get (destmode, dest) as parameters
 *           whether we need them or not.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void evallopd (int *opdmodevar, treenode ** opdvar, int destmode, treenode * dest)
{
	/*{{{  facetious comment */
	/* In a sensible language, opdmode and opd would be var. parameters,
	   here we have to pass them via pointers.
	   To make the code more legible, we load them into locals on entry, and
	   write them out again on exit */
	/*}}} */
	int opdmode = *opdmodevar;
	treenode *opd = *opdvar;

	if (preeval (opdmode, opd))
		/*{{{  generate to a temporary */
	{
		movelopd (P_TEMP, opd, opdmode, NDeclOf (opd));
		opdmode = P_TEMP;
	}
	/*}}} */
	else
		/*{{{  generate to (destmode, dest) */
	{
		if (dest == NULL)
			geninternal_is (GEN_BAD_OPD, destmode, "evallopd");
		movelopd (destmode, dest, opdmode, opd);
		opdmode = destmode;
		opd = dest;
	}
	/*}}} */

	*opdmodevar = opdmode;
	*opdvar = opd;
}

/*}}}*/
#if 0
/*{{{  PRIVATE void tcrossmultiply*/
PRIVATE void tcrossmultiply (const int resultmode, treenode * const result, const int leftmode, treenode * const left, const int rightmode, treenode * const right)
{
	const BOOL lconst = isconstopd (leftmode, left);
	const BOOL rconst = isconstopd (rightmode, right);

	const INT32 leftlowval = lconst ? wordof (left, 0) : 100;
	const INT32 lefthighval = lconst ? wordof (left, 1) : 100;
	const INT32 rightlowval = rconst ? wordof (right, 0) : 100;
	const INT32 righthighval = rconst ? wordof (right, 1) : 100;

	BOOL zerocarry = TRUE;

	genprimary (I_LDC, 0);	/* ldc  0         */
	/*{{{  ld rightlow; ld leftlow; lmul; st resultlow */
	if ((leftlowval == 0) || (rightlowval == 0))
		/*{{{  optimise */
		genprimary (I_LDC, 0);	/* ldc  0         */
	/*}}} */
	else if (leftlowval == 1)
		/*{{{  optimise */
		loadopd (rightmode, right, 0);	/* ld   rightlow  */
	/*}}} */
	else if (rightlowval == 1)
		/*{{{  optimise */
		loadopd (leftmode, left, 0);	/* ld   leftlow   */
	/*}}} */
	else
		/*{{{  basic code */
	{
		loadopd (rightmode, right, 0);	/* ld   rightlow  */
		loadopd (leftmode, left, 0);	/* ld   leftlow   */
		/*{{{  T9000_alpha_badlmulpre */
		if (T9000_alpha_badlmulpre (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the args to settle");
		}
		/*}}} */
		/*{{{  T9000_gamma_badmul */
		if (T9000_gamma_badmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Workaround a slow multiplier");
		}
		/*}}} */
		gensecondary (I_LMUL);	/* lmul           */
		/*{{{  T9000_alpha_badlmul */
		if (T9000_alpha_badlmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the result to settle");
		}
		/*}}} */
		zerocarry = FALSE;
	}
	/*}}} */
	storeinopd (resultmode, result, 0, MAXREGS - 2);	/* st   resultlow */

	/*}}} */
	/*{{{  ld rightlow; ld lefthigh; lmul */
	if ((lefthighval == 0) || (rightlowval == 0))
		/*{{{  optimise */
	{
		/* No code at all, use existing carry */
	}
	/*}}} */
	else if (zerocarry && (lefthighval == 1))
		/*{{{  optimise */
		loadopd (rightmode, right, 0);	/* ld   rightlow  */
	/*}}} */
	else if (zerocarry && (rightlowval == 1))
		/*{{{  optimise */
		loadopd (leftmode, left, 1);	/* ld   lefthigh  */
	/*}}} */
	else
		/*{{{  basic code */
	{
		loadopd (rightmode, right, 0);	/* ld   rightlow  */
		loadopd (leftmode, left, 1);	/* ld   lefthigh  */
		/*{{{  T9000_alpha_badlmulpre */
		if (T9000_alpha_badlmulpre (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the args to settle");
		}
		/*}}} */
		/*{{{  T9000_gamma_badmul */
		if (T9000_gamma_badmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Workaround a slow multiplier");
		}
		/*}}} */
		gensecondary (I_LMUL);	/* lmul           */
		/*{{{  T9000_alpha_badlmul */
		if (T9000_alpha_badlmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the result to settle");
		}
		/*}}} */
		zerocarry = FALSE;
	}
	/*}}} */
	/*}}} */
	/*{{{  ld righthigh; ld leftlow; lmul */
	if ((leftlowval == 0) || (righthighval == 0))
		/*{{{  optimise */
	{
		/* No code at all, use existing carry */
	}
	/*}}} */
	else if (zerocarry && (leftlowval == 1))
		/*{{{  optimise */
		loadopd (rightmode, right, 1);	/* ld   righthigh */
	/*}}} */
	else if (zerocarry && (righthighval == 1))
		/*{{{  optimise */
		loadopd (leftmode, left, 0);	/* ld   leftlow   */
	/*}}} */
	else
		/*{{{  basic code */
	{
		loadopd (rightmode, right, 1);	/* ld   righthigh */
		loadopd (leftmode, left, 0);	/* ld   leftlow   */
		/*{{{  T9000_alpha_badlmulpre */
		if (T9000_alpha_badlmulpre (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the args to settle");
		}
		/*}}} */
		/*{{{  T9000_gamma_badmul */
		if (T9000_gamma_badmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Workaround a slow multiplier");
		}
		/*}}} */
		gensecondary (I_LMUL);	/* lmul           */
		/*{{{  T9000_alpha_badlmul */
		if (T9000_alpha_badlmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the result to settle");
		}
		/*}}} */
	}
	/*}}} */
	/*}}} */
	storeinopd (resultmode, result, 1, MAXREGS - 1);	/* st   resulthigh */
}

/*}}}*/
#else
/*{{{  PRIVATE void tsinglelmul*/
PRIVATE void tsinglelmul (const BOOL carryin_exists, const BOOL carryout_required,
	const int lmode, treenode * const left, const int loffset, const int rmode, treenode * const right, const int roffset)
{
	if (!carryin_exists && carryout_required)
		genprimary (I_LDC, 0);
	loadopd (rmode, right, roffset);
	loadopd (lmode, left, loffset);

	if (!carryin_exists && !carryout_required) {
		/*{{{  T9000_gamma_badmul */
		if (T9000_gamma_badmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Workaround a slow multiplier");
		}
		/*}}} */
		gensecondary (I_PROD);
	} else {
		/*{{{  T9000_alpha_badlmulpre */
		if (T9000_alpha_badlmulpre (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the args to settle");
		}
		/*}}} */
		/*{{{  T9000_gamma_badmul */
		if (T9000_gamma_badmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Workaround a slow multiplier");
		}
		/*}}} */
		gensecondary (I_LMUL);
		/*{{{  T9000_alpha_badlmul */
		if (T9000_alpha_badlmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Time for the result to settle");
		}
		/*}}} */
		if (!carryout_required)
			throw_nested_result_away ();
	}
}

/*}}}*/
/*{{{  PRIVATE void tcrossmultiply*/
PRIVATE void tcrossmultiply (const int resultmode, treenode * const result, int leftmode, treenode * left, int rightmode, treenode * right)
{
	INT32 rightlowval;
	INT32 righthighval;
	BOOL rconst = isconstopd (rightmode, right);
	BOOL value_on_stack;

	/*{{{  make sure any constant is on the rhs */
	if (isconstopd (leftmode, left)) {
		int tempmode;
		treenode *tempnode;
		tempmode = leftmode;
		leftmode = rightmode;
		rightmode = tempmode;
		tempnode = left;
		left = right;
		right = tempnode;
		rconst = TRUE;
	}
	/*}}} */

	rightlowval = rconst ? wordof (right, 0) : 10;
	righthighval = rconst ? wordof (right, 1) : 10;

	/*{{{  low word */
	value_on_stack = FALSE;
	if (rightlowval == 0)
		genprimary (I_LDC, 0);
	else if (rightlowval == 1)
		loadopd (leftmode, left, 0);	/* ld   leftlow   */
	else {
		tsinglelmul (FALSE, TRUE, leftmode, left, 0, rightmode, right, 0);
		value_on_stack = TRUE;
	}
	storeinopd (resultmode, result, 0, MAXREGS - 2);	/* st   resultlow */
	/*}}} */
	/*{{{  high word */
	/* Remember that the high word consists of:
	   carry + (lefthi * rightlo) + (leftlo * righthi)
	   some of these terms will be known to be zero or a copy of the left.
	 */

	/*{{{  hi times lo */
	if (rightlowval == 1) {
		loadopd (leftmode, left, 1);
		if (value_on_stack)
			gensecondary (I_SUM);
		value_on_stack = TRUE;
	} else if (rightlowval != 0) {
		tsinglelmul (value_on_stack, FALSE, leftmode, left, 1, rightmode, right, 0);
		value_on_stack = TRUE;
	}
	/*}}} */
	/*{{{  lo times hi */
	if (righthighval == 1) {
		loadopd (leftmode, left, 0);
		if (value_on_stack)
			gensecondary (I_SUM);
		value_on_stack = TRUE;
	} else if (righthighval != 0) {
		tsinglelmul (value_on_stack, FALSE, leftmode, left, 0, rightmode, right, 1);
		value_on_stack = TRUE;
	}
	/*}}} */

	if (!value_on_stack)
		genprimary (I_LDC, 0);

	storeinopd (resultmode, result, 1, MAXREGS - 1);	/* st   resulthigh */
	/*}}} */
}

/*}}}*/
#endif
/*{{{  PRIVATE void tlopandassign (resultmode, result, leftmode, rightmode, opd)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tlopandassign generates code to evaluate opd and store it in
 *              double length (resultmode, result).
 *              opd is a non-addressable expression
 *              The calling of this function is such that we can always
 *              generate into result.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void tlopandassign (int resultmode, treenode *result, int leftmode, int rightmode, treenode *opd)
{
	const int op = TagOf (opd);
	/*{{{  print expression */
	if (diagnostics)
		commentexp (opd);
	/*}}} */
	switch (op) {
		/*{{{  ADD SUBTRACT PLUS MINUS TIMES BITAND BITOR XOR */
	case S_ADD:
	case S_SUBTRACT:
	case S_PLUS:
	case S_MINUS:
	case S_TIMES:
	case S_BITAND:
	case S_BITOR:
	case S_XOR:
		{
			treenode *left = LeftOpOf (opd);
			treenode *right = RightOpOf (opd);
			const BOOL leftaddressable = isaddressableopd (leftmode, left) || isconstopd (leftmode, left);
			const BOOL rightaddressable = isaddressableopd (rightmode, right) || isconstopd (rightmode, right);

			/*{{{  prepare the result */
			resultmode = preparelopd (resultmode, result);
			/*}}} */
			/*{{{  prepare left and right */
			if (leftaddressable && rightaddressable)
				/*{{{  both addressable */
			{
				leftmode = preparelopd (leftmode, left);
				rightmode = preparelopd (rightmode, right);
			}
			/*}}} */
			else if (leftaddressable)
				/*{{{  left addressable, right not addressable */
			{
				evallopd (&rightmode, &right, resultmode, result);
				leftmode = preparelopd (leftmode, left);
			}
			/*}}} */
			else if (rightaddressable)
				/*{{{  right addressable, left not addressable */
			{
				evallopd (&leftmode, &left, resultmode, result);
				rightmode = preparelopd (rightmode, right);
			}
			/*}}} */
			else
				/*{{{  neither addressable */
			{
				DEBUG_MSG (("tlopandassign: add, etc: neither addressable\n"));
				/* The mapper may have inserted a temporary on the rhs
				   to force evaluation before the lhs, because of
				   the destination variable being used in the lhs.
				   This bug fix ensures that the rhs is generated first in
				   such a case. - CON bug 1150 8/2/91 */
				/* NO - this breaks other things. We have to stop trying
				   to be so bloody clever, and simply insert a temporary on
				   both sides - CON 8/2/91 */
				if ((complexity (right) > complexity (left))
				    /*|| preeval(rightmode, right) */
					)
				{	/* bug 1150 8/2/91 */
					/*{{{  generate right, then left */
					DEBUG_MSG (("tlopandassign: right is > complexity than left\n"));
					evallopd (&rightmode, &right, resultmode, result);
					evallopd (&leftmode, &left, resultmode, result);
				}
				/*}}} */
				else
					/*{{{  generate left, then right */
				{
					DEBUG_MSG (("tlopandassign: right is <= complexity than left\n"));
					evallopd (&leftmode, &left, resultmode, result);
					evallopd (&rightmode, &right, resultmode, result);
				}
				/*}}} */
			}
			/*}}} */
			/*}}} */

			/*{{{  generate code for this operator */
			DEBUG_MSG (("tlopandassign: coding the inline dyadic operator\n"));
			switch (op) {
				/*{{{  ADD SUBTRACT PLUS MINUS */
			case S_ADD:
			case S_SUBTRACT:
			case S_PLUS:
			case S_MINUS:
				{
					int ilow, ihigh;
					if (op == S_ADD) {
						ilow = I_LSUM;
						ihigh = I_LADD;
					} else if (op == S_SUBTRACT) {
						ilow = I_LDIFF;
						ihigh = I_LSUB;
					} else if (op == S_PLUS) {
						ilow = I_LSUM;
						ihigh = I_LSUM;
					} else {	/* op == S_MINUS */
						ilow = I_LDIFF;
						ihigh = I_LDIFF;
					}
					gendoubleaddop (resultmode, result, leftmode, left, rightmode, right, ilow, ihigh);
				}
				break;
				/*}}} */
				/*{{{  TIMES */
			case S_TIMES:
				tcrossmultiply (resultmode, result, leftmode, left, rightmode, right);
				break;
				/*}}} */
				/*{{{  BITAND BITOR XOR */
			case S_BITAND:
			case S_BITOR:
			case S_XOR:
				{
					const INT32 identityval = (op == S_BITAND) ? (-1) : 0;
					const INT32 zeroval = (op == S_BITAND) ? 0 : (-1);
					const int iop = (op == S_BITAND) ? I_AND : (op == S_BITOR) ? I_OR : I_XOR;
					const BOOL lconst = isconstopd (leftmode, left);
					const BOOL rconst = isconstopd (rightmode, right);

					/*{{{  ld leftlow; ld rightlow; and; st resultlow */
					{
						const INT32 leftlowval = lconst ? wordof (left, 0) : 100;
						const INT32 rightlowval = rconst ? wordof (right, 0) : 100;

						if ((op != S_XOR) && ((leftlowval == zeroval) || (rightlowval == zeroval)))
							/*{{{  optimise */
							loadconstant (zeroval);
						/*}}} */
						else if (leftlowval == identityval)
							/*{{{  optimise */
							loadopd (rightmode, right, 0);	/* ld   rightlow  */
						/*}}} */
						else if (rightlowval == identityval)
							/*{{{  optimise */
							loadopd (leftmode, left, 0);	/* ld   leftlow   */
						/*}}} */
						else
							/*{{{  basic code */
						{
							loadopd (leftmode, left, 0);	/* ld   leftlow   */
							loadopd (rightmode, right, 0);	/* ld   rightlow  */
							gensecondary (iop);	/* and/or         */
						}
						/*}}} */
						storeinopd (resultmode, result, 0, MAXREGS - 1);	/* st   resultlow */
					}
					/*}}} */
					/*{{{  ld lefthigh; ld righthigh; and; st resulthigh */
					{
						const INT32 lefthighval = lconst ? wordof (left, 1) : 100;
						const INT32 righthighval = rconst ? wordof (right, 1) : 100;

						if ((op != S_XOR) && ((lefthighval == zeroval) || (righthighval == zeroval)))
							/*{{{  optimise */
							loadconstant (zeroval);
						/*}}} */
						else if (lefthighval == identityval)
							/*{{{  optimise */
							loadopd (rightmode, right, 1);	/* ld   righthigh */
						/*}}} */
						else if (righthighval == identityval)
							/*{{{  optimise */
							loadopd (leftmode, left, 1);	/* ld   lefthigh  */
						/*}}} */
						else
							/*{{{  basic code */
						{
							loadopd (leftmode, left, 1);	/* ld   lefthigh  */
							loadopd (rightmode, right, 1);	/* ld   righthigh */
							gensecondary (iop);	/* and/or         */
						}
						/*}}} */
						storeinopd (resultmode, result, 1, MAXREGS - 1);	/* st   resulthigh */
					}
					/*}}} */
				}
				break;
				/*}}} */
			}
			/*}}} */
		}
		break;
		/*}}} */
		/*{{{  BITNOT */
	case S_BITNOT:
		{
			treenode *operand = OpOf (opd);
			int operandmode = leftmode;

			/*{{{  prepare the result */
			resultmode = preparelopd (resultmode, result);
			/*}}} */
			if (isaddressableopd (operandmode, operand))
				operandmode = preparelopd (operandmode, operand);
			else
				evallopd (&operandmode, &operand, resultmode, result);
			loadopd (operandmode, operand, 0);
			gensecondary (I_NOT);
			storeinopd (resultmode, result, 0, MAXREGS - 1);
			loadopd (operandmode, operand, 1);
			gensecondary (I_NOT);
			storeinopd (resultmode, result, 1, MAXREGS - 1);
		}
		break;
		/*}}} */
		/*{{{  LSHIFT RSHIFT */
	case S_LSHIFT:
	case S_RSHIFT:
		{
			int shop = (op == S_LSHIFT) ? I_LSHL : I_LSHR;	/* lshl/lshr */
			treenode *left = LeftOpOf (opd);
			treenode *right = RightOpOf (opd);

			resultmode = preparelopd (resultmode, result);

			/*{{{  set up left */
			if (isaddressableopd (leftmode, left))
				leftmode = preparelopd (leftmode, left);
			else if (!isconstopd (leftmode, left))
				evallopd (&leftmode, &left, resultmode, result);
			/*}}} */

			/*{{{  ld lefthigh; ld leftlow; ld count */
			if (regsfor (right) <= 1)
				/*{{{  load count last */
			{
				loadopd (leftmode, left, 1);	/* ld   lefthigh  */
				loadopd (leftmode, left, 0);	/* ld   leftlow   */
				if (isconst (right) && !isinconstanttable (right))
					genshiftimmediate (shop, LoValOf (right));	/* MDP */
				else {
					texp (right, MAXREGS - 2);	/* ld   count     */
					if (!T9000_instruction_timings && !isconst (right))
						checkerror ();	/* bug TS/1979 30/11/92 */
					gensecondary (shop);	/* lshl/lshr       */
				}
			}
			/*}}} */
			else {
				if (regsfor (right) > 2)
					/*{{{  load count first */
				{
					texp (right, MANY_REGS);	/* ld   count     */
					loadopd (leftmode, left, 1);	/* ld   lefthigh  */
					gensecondary (I_REV);	/* rev            */
				}
				/*}}} */
				else {	/* (regsfor(right) = 2) */
					   /*{{{  load count second */
					loadopd (leftmode, left, 1);	/* ld   lefthigh  */
					texp (right, MAXREGS - 1);	/* ld   count     */
				}
				/*}}} */
				loadopd (leftmode, left, 0);	/* ld   leftlow   */
				gensecondary (I_REV);	/* rev            */
				if (!T9000_instruction_timings && !isconst (right))
					checkerror ();	/* bug TS/1979 30/11/92 */
				gensecondary (shop);	/* lshl/lshr       */
			}
			/*}}} */

			/*{{{  T9000_alpha_badlmul */
			if (T9000_alpha_badlmul (&tx_global)) {
				gensecondary (I_NOP);
				gencomment0 ("Time for the result to settle");
			}
			/*}}} */

			storeinopd (resultmode, result, 0, MAXREGS - 2);	/* st   resultlow  */
			storeinopd (resultmode, result, 1, MAXREGS - 1);	/* st   resulthigh */
		}
		break;
		/*}}} */
		/*{{{  EXACT */
	case S_EXACT:
		{
			treenode *right = OpOf (opd);
			const int sourcetype = ntypeof (right);
			if (fitsinregister (sourcetype))
				/*{{{  BOOL/BYTE/short INT/INT  to double-length */
			{
				resultmode = preparelopd (resultmode, result);
				texp (right, MANY_REGS);
				ttypeconversion (sourcetype, S_INT);
				gensecondary (I_XDBLE);
				storeinopd (resultmode, result, 0, MAXREGS - 2);
				storeinopd (resultmode, result, 1, MAXREGS - 1);
			}
			/*}}} */
			else if (isdoublelength (sourcetype));	/* We are there already */
			else
				/* quad-length to double-length done by function call
				   real to int either done by function call or not supported */
				badtag (genlocn, S_EXACT, "tlopandassign");
		}
		break;
		/*}}} */
		/*{{{  ROUND TRUNC */
		/* We must be on an fp processor and source must be real,
		   otherwise these conversions would have been turned into function calls */
	case S_ROUND:
	case S_TRUNC:
		{
			BOOL simple;
			resultmode = preparelopd (resultmode, result);	/* added 24/9/90 for bug 739 */
#if 0
			simple = issimplelocal (result, be_lexlevel);	/* bug 738 5/11/90 */
#else
			simple = issimplelocal (result, be_lexlevel)
				|| (T9000_instruction_timings && islocal (result, be_lexlevel));
#endif
			tfpexp (OpOf (opd), MANY_REGS, MANY_REGS);	/*        source       */
			if (op == S_TRUNC) {
				if (has_directfp)
					gensecondary (I_FPRZ);	/*        (fprz)       */
				else
					genfpuentry (I_FPURZ);
			}
			gensecondary (I_FPINT);	/*        fpint        */
			if (CONVERSIONCHECKING) {
				if (has_directfp)
					gensecondary (I_FPCHKI64);	/*        (fpchki64)   */
				else
					genfpuentry (I_FPUCHKI64);
			}
			resultmode = ptrmodeof (resultmode);
			loadopd (resultmode, result, 0);	/*        ldp   result */
			gensecondary (I_FPDUP);	/*        fpdup        */
			if (!simple)
				gensecondary (I_DUP);	/*        dup          */
			checkerror ();
			gensecondary (I_FPSTNLI32);	/*        fpstnli32    */
			if (has_directfp)
				gensecondary (I_FPEXPDEC32);	/*        fpexpdec32   */
			else
				genfpuentry (I_FPUEXPDEC32);
			mark_flag_clean (TRUE);	/* so we don't do another fpchkerr */
			if (simple)
				loadopd (resultmode, result, 1);	/*        ldp result+1  */
			else
				genprimary (I_LDNLP, 1);	/*        ldnlp 1      */
			gensecondary (I_FPSTNLI32);	/*        fpstnli32    */
		}
		break;
		/*}}} */
		/*{{{  FINSTANCE */
	case S_FINSTANCE:
		DEBUG_MSG (("tlopandassign - calling tinstance\n"));
		tinstance (opd);
		break;
		/*}}} */
		/*{{{  specification .. VALOF */
	case S_VALABBR:
	case S_ABBR:
	case S_VALRETYPE:
	case S_RETYPE:
	case S_PROCDEF:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
	case S_TPROTDEF:
	case S_SPROTDEF:
	case S_DECL:
	case S_VALOF:
	case S_PRAGMA:		/* bug 829 19/9/91 */
	case S_TYPEDECL:
#ifdef MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
	case S_MPROCDECL:
#endif
		{
			treenode *resultexp;
			treenode *specsptr = opd;

			opd = tspecs (opd);
			resultexp = ThisItem (VLResultListOf (opd));
			tprocess (VLBodyOf (opd));
			tpreexp (resultexp);
			movelopd (resultmode, result, P_EXP, resultexp);
			tdespecs (specsptr);
		}
		break;
		/*}}} */
	default:
		badtag (genlocn, op, "tlopandassign");
	}
}

/*}}}*/
/*{{{  PRIVATE movelopd_t categorise_movelopd*/
PRIVATE movelopd_t categorise_movelopd (treenode * const dest, treenode * const source)
{
	const BOOL const_source = isconst (source);
	const BOOL simple_dest = issimplelocal (dest, be_lexlevel);
	const BOOL simple_source = issimplelocal (source, be_lexlevel) || (const_source && !shouldbeinconstanttable (source));
	const BOOL local_dest = islocal (dest, be_lexlevel);
	const BOOL local_source = islocal (source, be_lexlevel) || const_source;

	if (simple_dest && simple_source)
		return movelopd_load_store;

	if ((code_style_flags & CODE_STYLE_SPACE_NOT_TIME) != 0)
		return movelopd_move;
	if (bytesperword == 2)
		return movelopd_move;	/* code size is important! */
	if (is_devaccess (dest) || is_devaccess (source))
		return movelopd_move;	/* we don't want to complicate things! */

	if (simple_dest && local_source)
		return movelopd_load_store;
	if (simple_source && local_dest)
		return movelopd_load_store;

	if (has_fpu_core && (ntypeof (source) == S_REAL64) && !uses_fpu (dest))
		return movelopd_fpu;

	if (local_dest && local_source)
		return movelopd_load_store;

	if (!T9000_instruction_timings && has_dup && local_dest)
		return movelopd_dup_source;

	if (!T9000_instruction_timings && has_dup && local_source)
		return movelopd_dup_dest;

	return movelopd_move;
}

/*}}}*/
/*{{{  PUBLIC void mapmovelopd (destmode, dest, sourcemode, source)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  mapmovelopd maps the move of the double length expression
 *              (sourcemode, source) to the double length variable
 *              (destmode, dest)
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void mapmovelopd (int destmode, treenode ** dest, int sourcemode, treenode ** source)
{
#ifdef DEBUG
	DEBUG_MSG (("mapmovelopd: destmode = %d, sourcemode = %d, source is", destmode, sourcemode));
	printtree (stdout, 0, *source);
	DEBUG_MSG (("\ndest is"));
	printtree (stdout, 0, *dest);
	DEBUG_MSG (("\n"));
#endif
	if (isaddressableopd (sourcemode, *source) || isconstopd (sourcemode, *source)) {
		BOOL freesourcetemp = FALSE, freedesttemp = FALSE;
		DEBUG_MSG (("mapmovelopd: source is addressable or const\n"));
		switch (categorise_movelopd (*dest, *source)) {
			/*{{{  load/store */
		case movelopd_load_store:
			sourcemode = mappreparelopd (sourcemode, source, &freesourcetemp);
			destmode = mappreparelopd (destmode, dest, &freedesttemp);
			/* perform the move */
			break;
			/*}}} */
			/*{{{  move */
		case movelopd_move:
			sourcemode = ptrmodeof (sourcemode);
			destmode = ptrmodeof (destmode);
			mapload2regs (sourcemode, source, destmode, dest);
			break;
			/*}}} */
			/*{{{  dup the dest */
		case movelopd_dup_dest:
			destmode = ptrmodeof (destmode);
			mapexpopd (destmode, dest);
			sourcemode = mappreparelopd (sourcemode, source, &freesourcetemp);
			break;
			/*}}} */
			/*{{{  dup the source */
		case movelopd_dup_source:
			sourcemode = ptrmodeof (sourcemode);
			mapexpopd (sourcemode, source);
			destmode = mappreparelopd (destmode, dest, &freedesttemp);
			break;
			/*}}} */
			/*{{{  via the fpu */
		case movelopd_fpu:
			mapfpassign (destmode, dest, sourcemode, source);
			break;
			/*}}} */
		}
		/*{{{  free temporaries */
		if (freesourcetemp)
			freetemp (*source);
		if (freedesttemp)
			freetemp (*dest);
		/*}}} */
	} else {
		DEBUG_MSG (("mapmovelopd: source is NOT addressable or const\n"));
		maplopandassign (destmode, dest, P_EXP, P_EXP, source);
	}
}

/*}}}*/
/*{{{  PUBLIC void movelopd (destmode, dest, sourcemode, source)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  movelopd generates code to move the double length expression
 *           (sourcemode, source) to the double length variable
 *           (destmode, dest)
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void movelopd (int destmode, treenode * dest, int sourcemode, treenode * source)
{
	if (isaddressableopd (sourcemode, source) || isconstopd (sourcemode, source)) {
		switch (categorise_movelopd (dest, source)) {
			/*{{{  load/store */
		case movelopd_load_store:
			sourcemode = preparelopd (sourcemode, source);
			destmode = preparelopd (destmode, dest);
			loadopd (sourcemode, source, 0);
			storeinopd (destmode, dest, 0, MAXREGS - 1);
			loadopd (sourcemode, source, 1);
			storeinopd (destmode, dest, 1, MAXREGS - 1);
			break;
			/*}}} */
			/*{{{  move */
		case movelopd_move:
			sourcemode = ptrmodeof (sourcemode);
			destmode = ptrmodeof (destmode);
			tload2regs (sourcemode, source, destmode, dest, FALSE, FALSE);
			loadconstant ((INT32) bytesperword * 2);
			if (is_devaccess (source) || is_devaccess (dest)) {
				/*{{{  T9000_gamma_baddevaccess */
				if (T9000_gamma_baddevaccess (&tx_global)) {
					genwarn (GEN_DEVMOVE_LATENCY_PROBLEM);
					gensecondary (I_INTDIS);
					gencomment0 ("bug fix for device access");
				}
				/*}}} */
				gensecondary (I_DEVMOVE);
				if (T9000_gamma_baddevaccess (&tx_global)) {
					gensecondary (I_INTENB);
					gencomment0 ("bug fix for device access");
				}
			} else
				gensecondary (I_MOVE);
			break;
			/*}}} */
			/*{{{  dup the dest */
		case movelopd_dup_dest:
			sourcemode = preparelopd (sourcemode, source);	/* probably not needed */
			destmode = ptrmodeof (destmode);
			texpopd (destmode, dest, MANY_REGS);
			gensecondary (I_DUP);
			loadopd (sourcemode, source, 0);
			gensecondary (I_REV);
			genprimary (I_STNL, 0);
			loadopd (sourcemode, source, 1);
			gensecondary (I_REV);
			genprimary (I_STNL, 1);
			break;
			/*}}} */
			/*{{{  dup the source */
		case movelopd_dup_source:
			destmode = preparelopd (destmode, dest);	/* probably not needed */
			sourcemode = ptrmodeof (sourcemode);
			texpopd (sourcemode, source, MANY_REGS);
			gensecondary (I_DUP);
			genprimary (I_LDNL, 0);
			storeinopd (destmode, dest, 0, MAXREGS - 2);
			genprimary (I_LDNL, 1);
			storeinopd (destmode, dest, 1, MAXREGS - 1);
			break;
			/*}}} */
			/*{{{  via the fpu */
		case movelopd_fpu:
			tfpassign (ntypeof (source), destmode, dest, sourcemode, source, MANY_REGS);
			break;
			/*}}} */
		}
	} else if (preeval (sourcemode, source)) {
		tlopandassign (P_TEMP, source, P_EXP, P_EXP, NDeclOf (source));
		movelopd (destmode, dest, P_TEMP, source);
	} else
		tlopandassign (destmode, dest, P_EXP, P_EXP, source);
	checkerror ();
}

/*}}}*/

/*{{{  PUBLIC void trellop(op, type, left, right, sense, genbool)   double-length*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  trellop generates code for relational operator 'op' into Areg, using at
 *          most 'regs' registers.
 *          If 'genbool' is TRUE, a strictly Boolean result is left in Areg.
 *          If 'sense' is FALSE, the result is inverted.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void trellop (const int op, const int type, treenode * left, treenode * right, BOOL sense, BOOL genbool)
{
	const BOOL leftaddressable = isaddressable (left) || isconst (left);
	const BOOL rightaddressable = isaddressable (right) || isconst (right);
	int leftmode = P_EXP, rightmode = P_EXP;
	USE_VAR (type);		/* stop unused variable warning */

	/*{{{  prepare left and right */
	if (leftaddressable && rightaddressable)
		/*{{{  both addressable */
	{
		leftmode = preparelopd (leftmode, left);
		rightmode = preparelopd (rightmode, right);
	}
	/*}}} */
	else if (leftaddressable)
		/*{{{  left addressable, right not addressable */
	{
		evallopd (&rightmode, &right, P_EXP, NULL);
		leftmode = preparelopd (leftmode, left);
	}
	/*}}} */
	else if (rightaddressable)
		/*{{{  right addressable, left not addressable */
	{
		evallopd (&leftmode, &left, P_EXP, NULL);
		rightmode = preparelopd (rightmode, right);
	}
	/*}}} */
	else
		/*{{{  neither addressable */
	{
		if (complexity (right) > complexity (left))
			/*{{{  generate right, then left */
		{
			evallopd (&rightmode, &right, P_EXP, NULL);
			evallopd (&leftmode, &left, P_EXP, NULL);
		}
		/*}}} */
		else
			/*{{{  generate left, then right */
		{
			evallopd (&leftmode, &left, P_EXP, NULL);
			evallopd (&rightmode, &right, P_EXP, NULL);
		}
		/*}}} */
	}
	/*}}} */
	/*}}} */
	switch (op) {
		/*{{{  S_EQ */
	case S_EQ:
		/*{{{  double length INT / quad length EQC */
		{
			if (TagOf (right) == S_CONSTEXP)
				/*{{{  swap left and right */
			{
				treenode *temp = left;
				left = right;
				right = temp;
			}
			/*}}} */

			if (TagOf (left) == S_CONSTEXP)
				/*{{{  test against constant */
			{
				/*{{{  do lower half */
				loadopd (rightmode, right, 0);
				if (wordof (left, 0) != 0) {
					loadconstant (wordof (left, 0));
					gensecondary (I_DIFF);
				}
				/*}}} */
				/*{{{  do upper half */
				loadopd (rightmode, right, 1);
				if (wordof (left, 1) != 0) {
					loadconstant (wordof (left, 1));
					gensecondary (I_DIFF);
				}
				/*}}} */

				gensecondary (I_OR);

				if (isquadlength (type))
					/*{{{  do upper two words */
				{
					/* bug INSdi02177 */
					loadopd (rightmode, right, 2);
					if (wordof (left, 2) != 0) {
						loadconstant (wordof (left, 2));
						gensecondary (I_DIFF);
					}
					gensecondary (I_OR);

					loadopd (rightmode, right, 3);
					if (wordof (left, 3) != 0) {
						loadconstant (wordof (left, 3));
						gensecondary (I_DIFF);
					}
					gensecondary (I_OR);
				}
				/*}}} */
			}
			/*}}} */
			else
				/*{{{  leftlo; rightlo; diff; lefthi; righthi; diff; or */
			{
				leftmode = preparelopd (leftmode, left);
				rightmode = preparelopd (rightmode, right);

				loadopd (leftmode, left, 0);
				loadopd (rightmode, right, 0);
				gensecondary (I_DIFF);
				loadopd (leftmode, left, 1);
				loadopd (rightmode, right, 1);
				gensecondary (I_DIFF);
				gensecondary (I_OR);
			}
			/*}}} */

			if (!sense)
				/*{{{  we already have !sense - do we convert to Boolean? */
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
		break;
		/*}}} */
		/*{{{  S_GR */
	case S_GR:
		/*{{{  double length INT */
		{
			const BOOL leftconst = (TagOf (left) == S_CONSTEXP);
			const BOOL rightconst = (TagOf (right) == S_CONSTEXP);

			if (leftconst && (wordof (left, 0) == 0) && (wordof (left, 1) == 0))
				/*{{{  optimise 0 > right to   ldc 0; righthi; gt */
			{
				loadconstant (0);	/*          ldc   0         */
				loadopd (rightmode, right, 1);	/*          ld    righthi   */
				gensecondary (I_GT);	/*          gt              */
				/*{{{  Bug fix for alpha */
				if (T9000_alpha_badgt (&tx_global)) {
					genprimary (I_EQC, 1);
					gencomment0 ("Silicon bug fix");
				}
				/*}}} */
				if (!sense)
					genboolinvert ();	/*          eqc   0         */
			}
			/*}}} */
			else
				/*{{{  standard code */
			{
				const int label1 = newlab ();
				const int label2 = newlab ();
				BOOL waiting_to_eqc = FALSE;

				/*{{{  lefthi; righthi; gt; eqc 0; cj label1 */
				loadopd (leftmode, left, 1);	/*          ld    lefthi    */
				loadopd (rightmode, right, 1);	/*          ld    righthi   */
				gensecondary (I_GT);	/*          gt              */
				/*{{{  Bug fix for alpha */
				if (T9000_alpha_badgt (&tx_global)) {
					genprimary (I_EQC, 1);
					gencomment0 ("Silicon bug fix");
				}
				/*}}} */
				genboolinvert ();
				gencontrolsplit (label1);	/*          cj    label1    */
				/*}}} */

				/*{{{  lefthi; righthi; diff; eqc 0; cj label2 */
				if (leftconst && (wordof (left, 1) == 0))
					/*{{{  righthi */
					loadopd (rightmode, right, 1);	/*          ld    righthi   */
				/*}}} */
				else
					/*{{{  lefthi; righthi; diff */
				{
					loadopd (leftmode, left, 1);	/*          ld    lefthi    */
					if (rightconst && (wordof (right, 1) == 0));	/* No need to diff with right */
					else
						/*{{{  righthi; diff */
					{
						loadopd (rightmode, right, 1);	/*          ld    righthi   */
						gensecondary (I_DIFF);	/*          diff            */
					}
					/*}}} */
				}
				/*}}} */
				genprimary (I_EQC, 0);	/*          eqc   0         */
				gencontrolsplit (label2);	/*          cj    label2    */
				/*}}} */

#if 0
				/*{{{  leftlo; mint; xor; rightlo; mint; xor; gt || leftlo; rightlo; gtu */
				{
					const BIT32 mint = has_gtu ? 0 :	/* bug 1387 10/09/91 */
						(targetintsize == S_INT16) ? MOSTNEG_INT16 : MOSTNEG_INT32;
					if (T9000_alpha_badmint (&tx_global) && !has_gtu)
						abort ();	/* defend against possible nasty combination */
					if (leftconst)
						loadconstant (wordof (left, 0) ^ mint);	/*        ldc   leflo (xor mint) */
					else {
						loadopd (leftmode, left, 0);	/*        ld    leftlo         */
						if (!has_gtu) {
							gensecondary (I_MINT);	/*        mint                 */
							gensecondary (I_XOR);	/*        xor                  */
						}
					}

					if (rightconst)
						loadconstant (wordof (right, 0) ^ mint);	/*        ldc rightlo (xor mint) */
					else {
						loadopd (rightmode, right, 0);	/*        ld    rightlo        */
						if (!has_gtu) {
							gensecondary (I_MINT);	/*        mint                 */
							gensecondary (I_XOR);	/*        xor                  */
						}
					}
					gensecondary (has_gtu ? I_GTU : I_GT);	/*        gt || gtu            */
					/*{{{  Bug fix for alpha */
					if (!has_gtu && T9000_alpha_badgt (&tx_global)) {
						genprimary (I_EQC, 1);
						gencomment0 ("Silicon bug fix");
					}
					/*}}} */
				}
				/*}}} */
#else /* bug INSdi02129 */
				/*{{{  leftlo '> (unsigned)' rightlo */
				{
					if (!has_gtu)
						genprimary (I_LDC, 1);
					loadopd (leftmode, left, 0);
					loadopd (rightmode, right, 0);
					if (has_gtu)
						gensecondary (I_GTU);
					else {
						gensecondary (I_LDIFF);
						gensecondary (I_REV);
						throw_nested_result_away ();
						/*genprimary(I_EQC, 0); */
						waiting_to_eqc = TRUE;
					}
				}
				/*}}} */
#endif

				if (!sense)
					/*{{{  label2: eqc 0; label1: */
				{
					if (waiting_to_eqc) {
						genboolinvert ();
						/*{{{  T9000_gamma_badeqc0 */
						if (T9000_gamma_badeqc0 (&tx_global)) {
							gensecondary (I_NOP);
							gencomment0 ("Incase of two eqc0's ");
						}
						/*}}} */
					}
					gencontroljoin (label2);
					genboolinvert ();
					gencontroljoin (label1);
				}
				/*}}} */
				else
					/*{{{  eqc 0; label1: eqc 0; label2: */
				{
					if (!waiting_to_eqc) {	/* no need for two consecutive eqc 0's */
						genboolinvert ();
						/*{{{  T9000_gamma_badeqc0 */
						if (T9000_gamma_badeqc0 (&tx_global)) {
							gensecondary (I_NOP);
							gencomment0 ("Incase of two eqc0's ");
						}
						/*}}} */
					}
					gencontroljoin (label1);
					genboolinvert ();
					gencontroljoin (label2);
				}
				/*}}} */
			}

			/*}}} */
		}
		/*}}} */
		break;
		/*}}} */
	default:
		geninternal_is (GEN_ERROR_IN_ROUTINE, 2, "trellop");
	}
	checkerror ();
}

/*}}}*/
/*{{{  PUBLIC void maprellop(op, type, left, right)                 double-length*/
/*****************************************************************************
 *
 *  maprellop maps workspace for generating relational operator 'op'
 *            of left and right into Areg.
 *
 *****************************************************************************/
PUBLIC void maprellop (int op, int type, treenode ** left, treenode ** right)
{
	const BOOL leftaddressable = isaddressable (*left) || isconst (*left);
	const BOOL rightaddressable = isaddressable (*right) || isconst (*right);
	BOOL freelefttemp = FALSE, freerighttemp = FALSE;
	int leftmode = P_EXP, rightmode = P_EXP;
	USE_VAR (type);		/* stop unused variable warnings */
	USE_VAR (op);		/* stop unused variable warnings */

	/*{{{  set up left and right operands */
	if (leftaddressable && rightaddressable)
		/*{{{  left and right addressable */
	{
		leftmode = mappreparelopd (leftmode, left, &freelefttemp);
		rightmode = mappreparelopd (rightmode, right, &freerighttemp);
	}
	/*}}} */
	else if (leftaddressable || rightaddressable)
		/*{{{  one side addressable, the other side not addressable */
	{
		if (rightaddressable)
			/*{{{  swap left and right */
		{
			treenode **tempptr;
			int tempmode;
			tempptr = left;
			left = right;
			right = tempptr;
			tempmode = leftmode;
			leftmode = rightmode;
			rightmode = tempmode;
		}
		/*}}} */

		/*{{{  generate right into temporary */
		{
			*right = gettemp (*right, NM_WORKSPACE);
			rightmode = P_TEMP;
			freerighttemp = TRUE;
			mapmovelopd (P_TEMP, right, P_EXP, NDeclAddr (*right));
		}
		/*}}} */
		leftmode = mappreparelopd (leftmode, left, &freelefttemp);
	}
	/*}}} */
	else
		/*{{{  neither left nor right addressable */
	{
		/* Make left the most complex side */
		if (complexity (*left) < complexity (*right)) {
			treenode **tempptr;
			int tempmode;
			tempptr = left;
			left = right;
			right = tempptr;
			tempmode = leftmode;
			leftmode = rightmode;
			rightmode = tempmode;
		}

		*left = gettemp (*left, NM_WORKSPACE);
		rightmode = P_TEMP;
		freelefttemp = TRUE;
		mapmovelopd (P_TEMP, left, P_EXP, NDeclAddr (*left));

		*right = gettemp (*right, NM_WORKSPACE);
		rightmode = P_TEMP;
		freerighttemp = TRUE;
		mapmovelopd (P_TEMP, right, P_EXP, NDeclAddr (*right));
	}
	/*}}} */
	/*}}} */
	/* perform the operation */
	/*{{{  free temporaries */
	if (freelefttemp)
		freetemp (*left);
	if (freerighttemp)
		freetemp (*right);
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC int addresslopd (opdmode, opd)*/
/*****************************************************************************
 *
 *  addresslopd takes the long operand (opdmode, opd), makes it addressable
 *              and returns the new opdmode.
 *
 *****************************************************************************/
PUBLIC int addresslopd (int opdmode, treenode * opd)
{
	if (isaddressableopd (opdmode, opd))
		/* Make a pointer to it if necessary */
		opdmode = preparelopd (opdmode, opd);
	else if (!isconstopd (opdmode, opd))
		/* Generate it into a temporary */
		evallopd (&opdmode, &opd, P_EXP, NULL);
	return (opdmode);
}

/*}}}*/
/*{{{  PUBLIC int mapaddresslopd (opdmode, opd)*/
/*****************************************************************************
 *
 *  mapaddresslopd maps workspace requirement for addressing (opdmode, opd)
 *                 see routine addresslopd.
 *
 *****************************************************************************/
PUBLIC int mapaddresslopd (int opdmode, treenode ** opd, int *freetempopd)
{
	if (isaddressableopd (opdmode, *opd) || isconstopd (opdmode, *opd))
		opdmode = mappreparelopd (opdmode, opd, freetempopd);
	else {
		/* Make a temporary and evaluate into that */
		*opd = gettemp (*opd, NM_WORKSPACE);
		*freetempopd = TRUE;
		mapmovelopd (P_TEMP, opd, opdmode, NDeclAddr (*opd));
		opdmode = P_TEMP;
	}
	return (opdmode);
}

/*}}}*/
/*{{{  PUBLIC void mapmoveqopd(destmode, dest, sourcemode, source)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  mapmoveqopd maps the move of the quadruple length expression
 *              (sourcemode, source) to the quadruple length variable
 *              (destmode, dest)
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void mapmoveqopd (int destmode, treenode ** dest, int sourcemode, treenode ** source)
{
	if (isaddressableopd (sourcemode, *source) || isconstopd (sourcemode, *source)) {
		mapmoveopd (destmode, dest, sourcemode, source);
	} else {
		mapqopandassign (destmode, dest, source);
	}
}

/*}}}*/
/*{{{  PUBLIC void moveqopd(destmode, dest, sourcemode, source)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  moveqopd generates code to move the quad-length expression
 *           (sourcemode, source) to the quad-length destination
 *           (destmode, dest).
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void moveqopd (int destmode, treenode *dest, int sourcemode, treenode *source)
{
	if (isaddressableopd (sourcemode, source)) {
		moveopd (destmode, dest, sourcemode, source);
	} else if (preeval (sourcemode, source)) {
		sourcemode = simplify (sourcemode, source);
		moveopd (destmode, dest, sourcemode, source);
	} else {
		/*{{{  function call or valof */
		/*{{{  print expression */
		if (diagnostics)
			commentexp (source);
		/*}}} */
		switch (TagOf (source)) {
		default:
			badtag (genlocn, TagOf (source), "moveqopd");
			break;
			/*{{{  function call */
		case S_FINSTANCE:
			tinstance (source);
			break;
			/*}}} */
			/*{{{  specification .. valof */
		case S_VALABBR:
		case S_ABBR:
		case S_VALRETYPE:
		case S_RETYPE:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_DECL:
		case S_VALOF:
		case S_PRAGMA:	/* bug 829 19/9/91 */
		case S_TYPEDECL:
#ifdef MOBILES
		case S_FORWDECL:
		case S_PROCTYPEDECL:
		case S_MPROCDECL:
#endif
			{
				treenode *resultexp;
				treenode *specsptr = source;

				source = tspecs (source);
				resultexp = ThisItem (VLResultListOf (source));
				tprocess (VLBodyOf (source));
				tdespecs (specsptr);
				tpreexp (resultexp);
				moveqopd (destmode, dest, P_EXP, resultexp);
			}
			break;
			/*}}} */
		}
	}
	/*}}} */
}

/*}}}*/
