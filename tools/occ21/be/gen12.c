/* $Id: gen12.c,v 1.5 1997/11/21 17:38:23 mdp2 Exp $ */

/*
 *	code generator - hardware floating point expression generation
 *	Copyright (C) 1987 Inmos limited
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

/*{{{  definitions*/
#define REAL32_2      0x40000000	/* 2.0(REAL32) */
#define REAL64_2_LOW  0			/* lower 32-bits of 2.0(REAL64) */
#define REAL64_2_HIGH 0x40000000	/* higher 32-bits of 2.0(REAL64) */
/*}}}*/

/*{{{  forward definitions*/
PRIVATE int fpregsfor (treenode * tptr);
/*}}}*/
/*{{{  PRIVATE BOOL isfpconst2_0*/
/*****************************************************************************
 *
 *  isfpconst2_0 returns TRUE if 'tptr' is a real (REAL32 or REAL64)
 *               constant of value 2.0
 *
 *****************************************************************************/
PRIVATE BOOL
isfpconst2_0 (const treenode * const tptr, const int type)
{
	return (TagOf (tptr) == S_CONSTEXP) &&
		(((type == S_REAL32) && (LoValOf (tptr) == REAL32_2)) || ((LoValOf (tptr) == REAL64_2_LOW) && (HiValOf (tptr) == REAL64_2_HIGH)));
}

/*}}}*/
/*{{{  PRIVATE BOOL isfpconst0_0*/
/*****************************************************************************
 *
 *  isfpconst0_0 returns TRUE if 'tptr' is a real (REAL32 or REAL64)
 *               constant of value 0.0
 *
 *****************************************************************************/
PRIVATE BOOL
isfpconst0_0 (const treenode * const tptr, const int type)
{
	return (TagOf (tptr) == S_CONSTEXP) && (LoValOf (tptr) == 0) && ((type == S_REAL32) || (HiValOf (tptr) == 0));
}

/*}}}*/
/*{{{  PRIVATE int fpregsfor_fpoperand (treenode *tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  fpregsfor_fpoperand returns the minimum number of floating point registers
 *                      required to load the expression 'tptr' onto the
 *                      fp stack.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE int
fpregsfor_fpoperand (treenode * tptr)
{
#if 0				/* bug 1405 27/9/91 */
	if (!isaddressable (tptr))
		return fpregsfor (tptr);
	else
#endif
		switch (TagOf (tptr)) {
			/*{{{  literals names  constant expression */
		case N_VALABBR:
		case N_ABBR:
		case N_VALRETYPE:
		case N_RETYPE:
		case N_DECL:
		case N_VALPARAM:
		case N_PARAM:
		case N_RESULTPARAM:
		case N_REPL:
		case T_PREEVALTEMP:	/* Preevaluated temporary */
		case S_CONSTEXP:
		case S_FNFORMALRESULT:	/* INSdi03203 */
			return 1;
			/*}}} */
			/*{{{  subscript */
		case S_ARRAYITEM:
		case S_RECORDITEM:
			return (ASExpOf (tptr) == NULL) ? 1 : (int) max_INT32 (1, fpregsfor (ASExpOf (tptr)));
			/*}}} */
			/*{{{  temporary */
		case T_TEMP:
			return fpregsfor (NDeclOf (tptr));
			/*}}} */
		default:
#if 0				/* bug 1405 27/9/91 */
			badtag (genlocn, TagOf (tptr), "fpregsfor");
			return (0);	/* Not reached */
#else
			if (isaddressable (tptr))
				badtag (genlocn, TagOf (tptr), "fpregsfor_fpoperand");
			return fpregsfor (tptr);
#endif
		}
}

/*}}}*/
/*{{{  PRIVATE int fpregsfordop (treenode *tptr)*/
/*****************************************************************************
 *
 *  fpregsfordop returns the number of floating point registers needed to
 *               evaluate floating dyadic operator 'tptr'.
 *
 *****************************************************************************/
PRIVATE int
fpregsfordop (treenode * const tptr)
{
	const int fpregsforleft = fpregsfor_fpoperand (LeftOpOf (tptr));
	const int fpregsforright = fpregsfor_fpoperand (RightOpOf (tptr));
	const int res = (fpregsforleft > fpregsforright) ? fpregsforleft : (fpregsforleft == fpregsforright) ? fpregsforleft + 1 : fpregsforright;
#ifdef DEBUG
/*DEBUG_MSG(("fpregsfordop: %s: returning %d\n", itagstring(TagOf(tptr)), res));*/
	DEBUG_MSG (("fpregsfordop: "));
	printexp (stdout, tptr);
	DEBUG_MSG ((", returning %d\n", res));
#endif
	return res;
}

/*}}}*/
/*{{{  PRIVATE int fpregsfor (treenode *tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  fpregsfor returns the minimum number of floating point registers
 *            required to evaluate the expression 'tptr'.
 *            NOT - 'evaluate' does NOT mean 'load'. Thus this counts
 *            FPU regs in, eg, a[INT TRUNC (x + y)].
 *            That's why names, etc, return zero. Bug 1405 27/9/91.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE int fpregsfor (treenode * tptr)
{
	switch (TagOf (tptr)) {
		/*{{{  monadic operators */
		/* None of these monadic operators can have REAL operands
		   (okay, so NEG can theoretically, but we transform the tree
		   to a subtract from zero for a real operand)  */
		/* NOPE - CO'N 24/03/92. The problem is that we call this function
		   before doing the tree transformation! */
	case S_NEG:
		{
			DEBUG_MSG (("fpregsfor: unary neg: real? %d\n", isreal (MOpTypeOf (tptr))));
			/* bug TS/1632 24/03/92 */
			/* bug INSdi03306 - if real, use 'fpregsfor_fpoperand' for op */
			if (isreal (MOpTypeOf (tptr)))
				return fpregsfor_fpoperand (OpOf (tptr)) + 1;
			return fpregsfor (OpOf (tptr));
		}
	case S_NOT:
	case S_UMINUS:
	case S_BITNOT:
		return fpregsfor (OpOf (tptr));
	case S_SIZE:
	case S_ELSIZE:
		return fpregsfor (dimexpof (OpOf (tptr), 0));
	case S_SEGSTART:
		return fpregsfor (SStartExpOf (OpOf (tptr)));
		/*}}} */
		/*{{{  dyadic operators */
#ifdef MOBILES
	case S_NTH_DIMENSION:
		return 0;	/* loads straight from wherever */
#endif
	case S_ADD:
	case S_SUBTRACT:
	case S_MULT:
	case S_DIV:
	case S_REM:
	case S_EQ:
	case S_NE:
	case S_LS:
	case S_LE:
	case S_GR:
	case S_GE:
		if (!isreal (DOpTypeOf (tptr)))
			return (int) max_INT32 (fpregsfor (LeftOpOf (tptr)), fpregsfor (RightOpOf (tptr)));
		else
			switch (TagOf (tptr)) {
				/*{{{  ADD SUBTRACT EQ NE LS LE GR GE */
			case S_ADD:
			case S_SUBTRACT:
			case S_EQ:
			case S_NE:
			case S_LS:
			case S_LE:
			case S_GR:
			case S_GE:
				return fpregsfordop (tptr);
				/*}}} */
				/*{{{  MULT */
			case S_MULT:
				if (isfpconst2_0 (LeftOpOf (tptr), DOpTypeOf (tptr)))
					return fpregsfor_fpoperand (RightOpOf (tptr));
				else if (isfpconst2_0 (RightOpOf (tptr), DOpTypeOf (tptr)))
					return fpregsfor_fpoperand (LeftOpOf (tptr));
				else
					return fpregsfordop (tptr);
				/*}}} */
				/*{{{  DIV */
			case S_DIV:
				if (isfpconst2_0 (RightOpOf (tptr), DOpTypeOf (tptr)))
					return fpregsfor_fpoperand (LeftOpOf (tptr));
				else
					return fpregsfordop (tptr);
				/*}}} */
				/*{{{  REM */
			case S_REM:
				if (has_fprem)
					return fpregsfordop (tptr);
				return MAXFPUREGS;
				/*}}} */
			}
	case S_AND:
	case S_OR:
	case S_BITAND:
	case S_BITOR:
	case S_XOR:
	case S_PLUS:
	case S_MINUS:
	case S_TIMES:
	case S_LSHIFT:
	case S_RSHIFT:
	case S_CSUB0:
	case S_CCNT1:
		/* None of these dyadic operators can have REAL operands */
		return (int) max_INT32 (fpregsfor (LeftOpOf (tptr)), fpregsfor (RightOpOf (tptr)));
	case S_EVAL:
		return fpregsfor (RightOpOf (tptr));
		/*}}} */
		/*{{{  conversion */
	case S_EXACT:
	case S_ROUND:
	case S_TRUNC:
		{
			treenode *const source = OpOf (tptr);
			const int sourcetype = ntypeof (source);
			const int desttype = MOpTypeOf (tptr);
			if (isreal (desttype) || isreal (sourcetype))
				/*{{{  we need fp registers to do the conversion */
				switch (desttype) {
				case S_REAL32:
				case S_REAL64:
					/*{{{  something to real conversion */
					if (sourcetype == S_INT64) {
						return (int) max_INT32 (2, fpregsfor (source));
					} else {
						return (int) max_INT32 (1, fpregsfor (source));
					}
					/*}}} */
				case S_INT64:	/* Must be converting from a real */
					return (int) max_INT32 (2, fpregsfor_fpoperand (source));
				case S_INT:
				case S_INT32:
					return (int) max_INT32 (1, fpregsfor_fpoperand (source));
				}
			/*}}} */
			else	/* no fp registers required */
				return fpregsfor (source);
		}
		/*}}} */
		/*{{{  literals names  constant expression */
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_DECL:
	case N_VALPARAM:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_REPL:
	case T_PREEVALTEMP:	/* Preevaluated temporary */
	case S_CONSTEXP:
	case S_PARAM_STATICLINK:
	case S_PARAM_VSP:
	case S_PARAM_FB:
	case S_PARAM_WS:
	case S_FNFORMALRESULT:
	case S_DUMMYEXP:
		return 0;
		/*}}} */
		/*{{{  function instance, specification ... valof */
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
	case S_FINSTANCE:
	case S_PRAGMA:		/* bug 829 19/9/91 */
	case S_TYPEDECL:
#ifdef MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
#endif
		return MAXFPUREGS;
		/*}}} */
		/*{{{  subscript */
	case S_ARRAYITEM:
	case S_SEGMENTITEM:
	case S_RECORDITEM:
		{
			treenode *const subscriptexp = (TagOf (tptr) == S_SEGMENTITEM) ? SSubscriptExpOf (tptr) : ASExpOf (tptr);
			return (subscriptexp != NULL) ? fpregsfor (subscriptexp) : 0;
		}
		/*}}} */
		/*{{{  constructor */
#if 0				/* no constructors are passed to the backend any more */
	case S_CONSTRUCTOR:
		{
			int maxfpr = 0;
			for (tptr = LitExpOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr))
				maxfpr = max_INT32 (maxfpr, fpregsfor (ThisItem (tptr)));
			return (maxfpr);
		}
#endif
		/*}}} */
		/*{{{  special parameter types */
	case S_HIDDEN_PARAM:
	case S_FNACTUALRESULT:
		return fpregsfor (HExpOf (tptr));
		/*}}} */
		/*{{{  temporary */
	case T_TEMP:
		return fpregsfor (NDeclOf (tptr));
		/*}}} */
	default:
		badtag (genlocn, TagOf (tptr), "fpregsfor");
	}
	return (0);		/* Not reached */
}

/*}}}*/
/*{{{  PUBLIC BOOL uses_fpu*/
PUBLIC BOOL
uses_fpu (treenode * const tptr)
{
	return fpregsfor (tptr) != 0;
}

/*}}}*/
/*{{{  PRIVATE int fprevsfor (treenode *tptr, int fpregs)*/
/*****************************************************************************
 *
 *  fprevsfor returns the minimum number of 'fprev' instructions necessary
 *            when evaluating expression 'tptr' in at most 'fpregs' floating
 *            registers.
 *
 *****************************************************************************/
PRIVATE int fprevsfor (treenode * tptr, int fpregs)
{
	switch (TagOf (tptr)) {
		/*{{{  monadic operators */
		/* None of these monadic operators can have REAL operands
		   (okay, so NEG can theoretically, but we transform the tree
		   to a subtract from zero for a real operand)  */
	case S_NEG:
	case S_NOT:
	case S_UMINUS:
	case S_BITNOT:
		return fprevsfor (OpOf (tptr), fpregs);
	case S_SIZE:
	case S_ELSIZE:
		return fprevsfor (dimexpof (OpOf (tptr), 0), fpregs);
	case S_SEGSTART:
		return fprevsfor (SStartExpOf (OpOf (tptr)), fpregs);
	case S_HIDDEN_PARAM:
	case S_FNACTUALRESULT:
		return fprevsfor (HExpOf (tptr), fpregs);
		/*}}} */
		/*{{{  dyadic operator which may have real operands */
	case S_ADD:
	case S_SUBTRACT:
	case S_MULT:
	case S_DIV:
	case S_REM:
	case S_EQ:
	case S_NE:
	case S_LS:
	case S_LE:
	case S_GR:
	case S_GE:
		{
			treenode *left = LeftOpOf (tptr), *right = RightOpOf (tptr);
			if (!isreal (DOpTypeOf (tptr)))
				return (int) max_INT32 (fprevsfor (left, fpregs), fprevsfor (right, fpregs));
			else {
				int revbase = commutes (tptr) ? 0 : 1;
				if (fpregsfor_fpoperand (left) == fpregs)
					/*{{{  left; right; op */
					return fprevsfor (left, fpregs) + fprevsfor (right, fpregs - 1);
				/*}}} */
				else if (fpregsfor_fpoperand (right) == fpregs)
					/*{{{  right; left; (rev; ) op */
					return fprevsfor (right, fpregs) + fprevsfor (left, fpregs - 1) + revbase;
				/*}}} */
				else
					/*{{{  return minimum number of revs for either left or right first */
					return (int) min_INT32 ((INT32) fprevsfor (left, fpregs) + fprevsfor (right, fpregs - 1),
								(INT32) fprevsfor (right, fpregs) + fprevsfor (left, fpregs - 1) + revbase);
				/*}}} */
			}
		}
	case S_EVAL:
		return fprevsfor (RightOpOf (tptr), fpregs);
		/*}}} */
		/*{{{  dyadic operator which cannot have real operands */
#ifdef MOBILES
	case S_NTH_DIMENSION:
		return 0;	/* loads straight from wherever */
#endif
	case S_AND:
	case S_OR:
	case S_BITAND:
	case S_BITOR:
	case S_XOR:
	case S_PLUS:
	case S_MINUS:
	case S_TIMES:
	case S_LSHIFT:
	case S_RSHIFT:
	case S_CSUB0:
	case S_CCNT1:
		/* None of these dyadic operators can have REAL operands */
		return (int) max_INT32 (fprevsfor (LeftOpOf (tptr), fpregs), fprevsfor (RightOpOf (tptr), fpregs));
		/*}}} */
		/*{{{  name temp constant finstance specification ... valof ... */
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_VALPARAM:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_DECL:
	case N_REPL:
	case T_TEMP:
	case T_PREEVALTEMP:
	case S_CONSTEXP:
	case S_PARAM_STATICLINK:
	case S_PARAM_VSP:
	case S_PARAM_FB:
	case S_PARAM_WS:
	case S_FNFORMALRESULT:
	case S_DUMMYEXP:
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
	case S_FINSTANCE:
	case S_PRAGMA:		/* bug 829 19/9/91 */
	case S_TYPEDECL:
#ifdef MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
#endif
		return 0;
		/*}}} */
		/*{{{  conversion */
	case S_EXACT:
	case S_ROUND:
	case S_TRUNC:
		return fprevsfor (OpOf (tptr), fpregs);
		/*}}} */
		/*{{{  subscript */
	case S_ARRAYITEM:
	case S_SEGMENTITEM:
	case S_RECORDITEM:
		{
			treenode *subscriptexp = (TagOf (tptr) == S_SEGMENTITEM) ? SSubscriptExpOf (tptr) : ASExpOf (tptr);
			return (subscriptexp != NULL) ? fprevsfor (subscriptexp, fpregs) : 0;
		}
		/*}}} */
		/*{{{  constructor */
#if 0				/* no constructors are passed to the backend any more */
	case S_CONSTRUCTOR:
		{
			int maxfpr = 0;
			for (tptr = LitExpOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr))
				maxfpr = max_INT32 (maxfpr, fprevsfor (ThisItem (tptr), fpregs));
			return maxfpr;
		}
#endif
		/*}}} */
	default:
		badtag (genlocn, TagOf (tptr), "fprevsfor");
	}
	return (0);		/* Not reached */
}

/*}}}*/
/*{{{  PRIVATE void compfpdop (int op)*/
/*****************************************************************************
 *
 *  compfpdop generates code for floating point operation, op.
 *
 *****************************************************************************/
PRIVATE void
compfpdop (int op)
{
	switch (op) {
	case S_ADD:
		gensecondary (I_FPADD);
		break;
	case S_SUBTRACT:
		gensecondary (I_FPSUB);
		break;
	case S_MULT:
		gensecondary (I_FPMUL);
		break;
	case S_DIV:
		gensecondary (I_FPDIV);
		break;
		/*{{{  S_REM */
	case S_REM:
		if (has_fprem)
			gensecondary (I_FPREM);
		else {
			const int loop = newlab ();
			const int next = newlab ();
			gensecondary (I_FPREMFIRST);	/*       fpremfirst       */
			genprimary (I_EQC, 0);	/*       eqc         0    */
			genbranch (I_CJ, next);	/*       cj          next */
			setlab (loop);	/* loop:                  */
			gensecondary (I_FPREMSTEP);	/*       fpremstep        */
			genbranch (I_CJ, loop);	/*       cj          loop */
			setlab (next);	/* next:                  */
			throw_the_result_away ();
		}
		break;
		/*}}} */
		/*{{{  S_GR */
	case S_GR:
		gensecondary (I_FPGT);
		if (T9000_alpha_badfpbool (&tx_global)) {
			genprimary (I_EQC, 1);
			gencomment0 ("Silicon bug fix");
		}
		break;
		/*}}} */
		/*{{{  S_EQ */
	case S_EQ:
		gensecondary (I_FPEQ);
		if (T9000_alpha_badfpbool (&tx_global)) {
			genprimary (I_EQC, 1);
			gencomment0 ("Silicon bug fix");
		}
		break;
		/*}}} */
	default:
		badtag (genlocn, op, "compfpdop");
	}
}

/*}}}*/
/*{{{  PRIVATEPARAM void mapfpvalof (treenode *tptr)*/
PRIVATEPARAM void
mapfpvalof (treenode * tptr)
{
	treenode **resultexpaddr = ThisItemAddr (VLResultListOf (tptr));
	treenode *savedtemplist = templist;
	templist = NULL;
	mapprocess (VLBodyOf (tptr));
	mappreprocess (*resultexpaddr);
	mapfpexp (resultexpaddr);
	freetemplist ();
	templist = savedtemplist;
}

/*}}}*/
/*{{{  PUBLIC void mapfpload2regs (treenode **left, treenode **right)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  mapfpload2regs maps loading two operands onto the floating stack
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void
mapfpload2regs (treenode ** left, treenode ** right)
{
#if 0				/* bug TS/1492 18/12/91 */
	if (issame (*left, *right))	/* use fpdup */
		mapfpexp (left);
	else
#endif
	{
		const int fpregsforleft = fpregsfor_fpoperand (*left);
		const int fpregsforright = fpregsfor_fpoperand (*right);
		DEBUG_MSG (("mapfpload2regs: left:%d, right:%d\n", fpregsforleft, fpregsforright));
		if (max_INT32 (fpregsforleft, fpregsforright) > MAXFPUREGS)
			/*{{{  need a temporary somewhere */
		{
			if (fpregsforright >= fpregsforleft)
				/*{{{  evaluate right first */
			{
				if (fpregsforleft >= MAXFPUREGS)
				 {	/* right must be stored in a temporary */
					/*{{{  right; stl temp; left */
					DEBUG_MSG (("mapfpload2regs: right; stl temp; left\n"));
					mapfpexp (right);
					*right = gettemp (*right, NM_WORKSPACE);
					upusecount (*right, 2);
					mapfpexp (left);
					freetemp (*right);
				}
				/*}}} */
				else
					/*{{{  right; left */
				{
					DEBUG_MSG (("mapfpload2regs: right; left\n"));
					mapfpexp (right);
					mapfpexp (left);
				}
				/*}}} */
			}
			/*}}} */
			else
				/*{{{  evaluate left first */
			{
				if (fpregsforright >= MAXFPUREGS)
				 {	/* left must be  stored in a temporary */
					/*{{{  left; stl temp; right */
					DEBUG_MSG (("mapfpload2regs: left; stl temp; right\n"));
					mapfpexp (left);
					*left = gettemp (*left, NM_WORKSPACE);
					upusecount (*left, 2);
					mapfpexp (right);
					freetemp (*left);
				}
				/*}}} */
				else
					/*{{{  left; right */
				{
					DEBUG_MSG (("mapfpload2regs: left; right\n"));
					mapfpexp (left);
					mapfpexp (right);
				}
				/*}}} */
			}
			/*}}} */
		}
		/*}}} */
		else if (fpregsforleft == MAXFPUREGS && fpregsforright == MAXFPUREGS)
			/*{{{  right; stl temp; left */
		{
			DEBUG_MSG (("mapfpload2regs: right; stl temp; left\n"));
			mapfpexp (right);
			*right = gettemp (*right, NM_WORKSPACE);
			upusecount (*right, 2);
			mapfpexp (left);
			freetemp (*right);
		}
		/*}}} */
		else
			/*{{{  no temporaries needed */
		{
			DEBUG_MSG (("mapfpload2regs: left; right\n"));
			mapfpexp (left);
			mapfpexp (right);
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void mapfpexp (treenode **tptr)*/
/*****************************************************************************
 *
 *  mapfpexp maps a floating point expression which is to be loaded into
 *           the fpu.
 *
 *****************************************************************************/
PUBLIC void mapfpexp (treenode ** tptr)
{
	treenode *const t = *tptr;
	const SOURCEPOSN locn = LocnOf (t);
	const int tag = TagOf (t);
#ifdef DEBUG
	DEBUG_MSG (("mapfpexp: "));
	printexp (stdout, t);
	DEBUG_MSG (("\n"));
#endif
	switch (tag) {
		/*{{{  monadic ops */
	case S_NEG:
		{
			const int type = MOpTypeOf (t);
			*tptr = newdopnode (S_SUBTRACT, locn, newrealconstant (0, 0, "0.0", type), OpOf (t), type);
			mapfpexp (tptr);
		}
		break;
		/*}}} */
		/*{{{  dyadic ops */
	case S_MULT:
		if (isfpconst2_0 (LeftOpOf (t), DOpTypeOf (t)))
			/*{{{  swap left and right, to get the 2.0 on the right */
		{
			treenode *const temp = LeftOpOf (t);
			SetLeftOp (*tptr, RightOpOf (t));
			SetRightOp (*tptr, temp);
		}
		/*}}} */
		/* !!! Fall through */
	case S_DIV:
		if (isfpconst2_0 (RightOpOf (t), DOpTypeOf (t)))
			/*{{{  we will use a mulby2 / divby2 instruction */
		{
			mapfpexp (LeftOpAddr (t));
			return;
		}
		/*}}} */
		/* !!! Fall through */
	case S_ADD:
	case S_SUBTRACT:
	case S_REM:
	case S_EQ:
	case S_NE:
	case S_GR:
	case S_GE:
	case S_LE:
	case S_LS:
		/*{{{  T9000_alpha_nofprem */
		if ((tag == S_REM) && T9000_alpha_nofprem (&tx_global)) {	/* Section 4.8 of SW-0341-2 */
			*tptr = makedopfunction (t);
			mapfpexp (tptr);
			return;
		}
		/*}}} */
		if ((tag == S_SUBTRACT) && isconst (RightOpOf (t)) && !isfpconst0_0 (RightOpOf (t), DOpTypeOf (t)))
			/*{{{  turn x-k into x+(-k) */
		{
			/* bug INSdi01885 24/03/93 - turn x - k into x + (-k) */
			if (DOpTypeOf (t) == S_REAL32)
				SetLoVal (RightOpOf (t), LoValOf (RightOpOf (t)) ^ 0x80000000);
			else
				SetHiVal (RightOpOf (t), HiValOf (RightOpOf (t)) ^ 0x80000000);
			SetTag (t, S_ADD);
		}
		/*}}} */
		else
			/*{{{  T9000_alpha_badfpsub */
		if ((tag == S_SUBTRACT) && T9000_alpha_badfpsub (&tx_global)) {
			/* convert x - y => x + (0 - y) */
			treenode *const left = LeftOpOf (t);
			treenode *const right = RightOpOf (t);
			if (isfpconst0_0 (left, DOpTypeOf (t)));	/* skip *//* prevents infinite recursion */
			else {
				treenode *const zero = newrealconstant (0, 0, "0.0", DOpTypeOf (t));
				treenode *const newrhs = newdopnode (S_SUBTRACT, LocnOf (t),
								     zero, right, DOpTypeOf (t));
				SetTag (t, S_ADD);
				SetRightOp (t, newrhs);
			}
		}
		/*}}} */
		mapfpload2regs (LeftOpAddr (t), RightOpAddr (t));
		break;
		/*}}} */
		/*{{{  name temp */
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_VALPARAM:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_DECL:
	case T_TEMP:
	case T_PREEVALTEMP:
	case S_ARRAYITEM:
	case S_RECORDITEM:
	case S_FNFORMALRESULT:	/*CON - Bug 135 */
		mapaddr (tptr);
		break;
		/*}}} */
		/*{{{  constant */
	case S_CONSTEXP:
		{
			const int type = ntypeof (t);
			if (isfpconst0_0 (t, type) && !T9000_alpha_nofpconst (&tx_global))	/* Section 4.11 of SW-0341-2 */
				;	/* skip *//* we can use FPLDZEROxx */
			else
				mapaddr (tptr);
		}
		break;
		/*}}} */
		/*{{{  conversions */
	case S_EXACT:
	case S_ROUND:
	case S_TRUNC:
		{
			treenode **sourceptr = OpAddr (t);
			int sourcetype = ntypeof (*sourceptr);
			/*int desttype = MOpTypeOf(t); */
#if 0				/* now done in trans */
			/*{{{  remove a null conversion */
			if (desttype == sourcetype) {
				*tptr = *sourceptr;
				mapfpexp (tptr);
				return;
			}
			/*}}} */
#endif
#if 0
fprintf (stderr, "gen12: mapfpexp(): EXACT/ROUND/TRUNC\n");
#endif
			if (isshorttype (sourcetype)) {
				/*{{{  convert an INT16/BYTE source to an INT32 source */
				DEBUG_MSG (("mapfpexp: converting INT16/BYTE->REAL into INT16/BYTE->INT->REAL\n"));
				*sourceptr = newmopnode (S_EXACT, locn, *sourceptr, S_INT);
				sourcetype = S_INT;
			}
			/*}}} */
			/*{{{  T9000_alpha_nofpconv */
			if ((MOpTypeOf (t) == S_REAL32) && T9000_alpha_nofpconv (&tx_global) && ((sourcetype == S_INT64) ||	/* Section 4.1 and 4.2.3 of SW-0341-2 */
												 (sourcetype == S_INT32) ||	/* Section         4.2.1 of SW-0341-2 */
												 (sourcetype == S_INT))) {	/* Section         4.2.1 of SW-0341-2 */
				*sourceptr = newmopnode (tag, locn, *sourceptr, S_REAL64);
				sourcetype = S_REAL64;
			}
			/*}}} */
			switch (sourcetype) {
				/*{{{  REAL32 REAL64 */
			case S_REAL32:
			case S_REAL64:
				mapfpexp (sourceptr);
				break;
				/*}}} */
				/*{{{  INT64 */
				/* treat INT64 like INT32, cos we can use 'dup' to save
				   using a temp */
			case S_INT64:
				if (isaddressable (*sourceptr))
					mapaddr (sourceptr);	/* bug INSdi02163 */
				else {
					/* INSdi02195 - do this slightly differently to INT32 */
					int sourcemode = P_PTR;
					BOOL freeopdtemp = FALSE;
					sourcemode = mapaddresslopd (sourcemode, sourceptr, &freeopdtemp);
					if (freeopdtemp)
						freetemp (*sourceptr);
				}
				break;
				/*}}} */
				/*{{{  INT32 INT */
			case S_INT32:
			case S_INT:
				/* ldptr source; fpi32torxx */
				if (isaddressable (*sourceptr)) {
					mapaddr (sourceptr);
				} else {
					mapexp (sourceptr);
					*sourceptr = gettemp (*sourceptr, NM_WORKSPACE);
					upusecount (*sourceptr, 2);
					freetemp (*sourceptr);
				}
				break;
				/*}}} */
			default:
				geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "mapfpexp");
				break;
			}
		}
		break;
		/*}}} */
		/*{{{  function call */
	case S_FINSTANCE:
		/* It must be a single-valued real function, so the result will
		   come back in the fp Areg */
		if (TagOf (INameOf (t)) == N_PREDEFFUNCTION)
			if (mappredef (t, NULL))
				return;	/* Exit cos it was an inline function */
		SetIParamList (t, augmentparams (IParamListOf (t), FnParamsOf (NTypeOf (INameOf (t))), NULL, t));
		mapinstance (t);
		return;
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
	case S_PRAGMA:		/* bug 829 19/9/91 */
	case S_TYPEDECL:
#ifdef MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
#endif
		mapdeclandbody (t, mapfpvalof, FALSE, FALSE);
		break;
		/*}}} */
	default:
		badtag (genlocn, tag, "mapfpexp");
	}
}

/*}}}*/
/*{{{  PUBLIC void mapfpassign(destmode, dest, sourcemode, source)*/
/*****************************************************************************
 *
 *  mapfpassign maps the assignment of a floating point expression into
 *              (destmode, dest).
 *
 *****************************************************************************/
PUBLIC void
mapfpassign (int destmode, treenode ** dest, int sourcemode, treenode ** source)
{
	USE_VAR (sourcemode);	/* stop unused variable warning */
	DEBUG_MSG (("mapfpassign: destmode: %s\n", opdmode_string (destmode)));
/*destmode = ptrmodeof(destmode); *//* bug 1315 22/8/91 */
	if (fpregsfor (*dest) < MAXFPUREGS)
		/*{{{  load source into fpu; load destptr into cpu; store */
	{
		DEBUG_MSG (("mapfpassign: part 1; ld source; ld addressof dest; store\n"));
		mapfpexp (source);
		destmode = ptrmodeof (destmode);
		mapexpopd (destmode, dest);
	}
	/*}}} */
	else if (regsfor (*source) < MAXREGS)
		/*{{{  destaddr; source; stind */
	{
		DEBUG_MSG (("mapfpassign: part 2; destaddr; source; store\n"));
		destmode = ptrmodeof (destmode);
		mapexpopd (destmode, dest);
		mapfpexp (source);
	}
	/*}}} */
	else
		/*{{{  source; st temp; destaddr; ld temp; stind */
	{
		DEBUG_MSG (("mapfpassign: part 3; source; st temp; destaddr; ld temp; store\n"));
		mapfpexp (source);
		*source = gettemp (*source, NM_WORKSPACE);
#if 0
		mapexpopd (destmode, dest);	/* this breaks if dest is double length */
#else /* bug 1315 22/8/91 */
#if 0
		fprintf (outfile, "source tree is now:\n");
		printtree (0, *source);
		fputc ('\n', outfile);
#endif
		sourcemode = tempmodeof (sourcemode);
		mapsimpleassign (ntypeof (*source), destmode, dest, sourcemode, source);
#endif
		freetemp (*source);
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE void tfpexpandop (treenode *tptr, int regs, int fpregs, int op)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tfpexp generates code to load floating point expression 'tptr', and
 *         perform floating point dyadic operation 'op'.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void tfpexpandop (treenode * const tptr, const int regs, const int fpregs, const int op)
{
	if ((op == S_ADD || op == S_MULT) && isaddressable (tptr)) {
#ifdef MOBILES
		const int ntype = ((ntypeof (tptr) == S_MOBILE) ? TagOf (follow_user_type (MTypeOf (NTypeOf (tptr)))) : ntypeof (tptr));
#else	/* !MOBILES */
		const int ntype = ntypeof (tptr);
#endif	/* MOBILES */
		const BOOL single = (ntype == S_REAL32);
		const int inst = (op == S_ADD) ? (single ? I_FPLDNLADDSN : I_FPLDNLADDDB)
			: (single ? I_FPLDNLMULSN : I_FPLDNLMULDB);

		loadelementpointer (tptr, 0, regs);
		gensecondary (inst);
	} else {
		tfpexp (tptr, regs, fpregs);
		compfpdop (op);
	}
}

/*}}}*/
/*{{{  PUBLIC void tfpload2regs (exp1, exp2, regs, fpregs, commutes)*/
/*****************************************************************************
 *
 *  tfpload2regs loads exp1 and exp2 into fpbreg and fpareg, using at most
 *               'regs' integer registers and 'fpregs' floating point
 *               registers.
 *               If 'can_commute' is TRUE the expressions may be loaded in
 *               reverse order.
 *
 *****************************************************************************/
PUBLIC void
tfpload2regs (treenode * exp1, treenode * exp2, int regs, int fpregs, BOOL can_commute)
{
	int fpr = (fpregs == MANY_REGS) ? MAXFPUREGS : fpregs;
	if (TagOf (exp2) == T_TEMP)
		/*{{{  exp2; ldlp temp; fpstnl; exp1; ldlp temp; fpldnl */
	{
		simplify (P_EXP, exp2);
		tfpexp (exp1, regs, fpregs);
		tfpexp (exp2, regs, fpr - 1);
	}
	/*}}} */
	else if (TagOf (exp1) == T_TEMP)
		/*{{{  exp1; ldlp temp; fpstnl; exp2; fpldnl; (fprev;) */
	{
		simplify (P_EXP, exp1);
		tfpexp (exp2, regs, fpregs);
		tfpexp (exp1, regs, fpr - 1);
		if (!can_commute)
			gensecondary (I_FPREV);
	}
	/*}}} */
	else
		/*{{{  don't need to introduce temporaries here */
	{
#if 0				/* bug TS/1492 18/12/91 */
		if (issame (exp1, exp2)) {
			/*if (warning_flags & WARNING_CSE) genwarning(GEN_CSE, 0, 0); */
			tfpexp (exp1, regs, fpr);
			gensecondary (I_FPDUP);
		} else
#endif
		{
			int fpregsforexp1 = fpregsfor (exp1), fpregsforexp2 = fpregsfor (exp2);

			/*{{{  decide whether to do exp1 or exp2 first, then do it */
			{
				int exp1first = TRUE;
				/*{{{  set exp1first TRUE if we do exp1 first, FALSE otherwise */
				if (max_INT32 (fpregsforexp1, fpregsforexp2) > MAXFPUREGS)
					exp1first = (fpregsforexp1 >= fpregsforexp2);
				else if (fpregsforexp1 == fpr)
					exp1first = TRUE;
				else if (fpregsforexp2 == fpr)
					exp1first = FALSE;
				else
					/*{{{  go into all the high-tech stuff of counting fprev instructions */
				{
					int fprevs_for_exp1_first = fprevsfor (exp1, fpr) + fprevsfor (exp2, fpr - 1);
					int fprevs_for_exp2_first = fprevsfor (exp2, fpr) + fprevsfor (exp1, fpr - 1) + (can_commute ? 0 : 1);
					if (fprevs_for_exp1_first > fprevs_for_exp2_first)
						exp1first = FALSE;
				}
				/*}}} */
				/*}}} */
				/*{{{  generate the expression */
				if (exp1first)
					/*{{{  exp1; exp2; */
				{
					tfpexp (exp1, regs, fpr);
					tfpexp (exp2, regs, fpr - 1);
				}
				/*}}} */
				else
					/*{{{  exp2; exp1; (fprev); */
				{
					tfpexp (exp2, regs, fpregs);
					tfpexp (exp1, regs, fpr - 1);
					if (!can_commute)
						gensecondary (I_FPREV);
				}
				/*}}} */
				/*}}} */
			}
			/*}}} */
		}
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void tfpexp (treenode *tptr, int regs, int fpregs)*/
/*****************************************************************************
 *
 *  tfpexp generates code for the floating point expression 'tptr', using
 *         at most 'regs' integer registers and 'fpregs' floating point
 *         registers.
 *
 *****************************************************************************/
PUBLIC void tfpexp (treenode * tptr, int regs, int fpregs)
{
	switch (TagOf (tptr)) {
		/*{{{  dyadic ops */
	case S_ADD:
	case S_SUBTRACT:
	case S_MULT:
	case S_DIV:
	case S_REM:
	case S_EQ:
	case S_GR:
		{
			const int op = TagOf (tptr);
			treenode *left = LeftOpOf (tptr), *right = RightOpOf (tptr);
			const BOOL commutative = commutes (tptr);
			const int fpr = (fpregs == MANY_REGS) ? MAXFPUREGS : fpregs;
			/*{{{  look for some optimisations */
			if ((op == S_MULT || op == S_DIV) && isfpconst2_0 (right, DOpTypeOf (tptr))) {
				tfpexp (left, regs, fpregs);
				if (has_directfp)
					gensecondary (op == S_MULT ? I_FPMULBY2 : I_FPDIVBY2);
				else
					genfpuentry (op == S_MULT ? I_FPUMULBY2 : I_FPUDIVBY2);
				return;
			}
			/*}}} */
			if (TagOf (right) == T_TEMP)
				/*{{{  right; ldlp temp; fpstnl; left; ldlp temp; fpldnlop */
			{
				simplify (P_EXP, right);
				tfpexp (left, regs, fpregs);
				tfpexpandop (right, regs, fpr - 1, op);
			}
			/*}}} */
			else if (TagOf (left) == T_TEMP)
				/*{{{  left; ldlp temp; fpstnl; right; fpldnl; (fprev;) op */
			{
				simplify (P_EXP, left);
				tfpexp (right, regs, fpregs);
				if (commutative)
					tfpexpandop (left, regs, fpr - 1, op);
				else {
					tfpexp (left, regs, fpregs);
					gensecondary (I_FPREV);
					compfpdop (op);
				}
			}
			/*}}} */
			else
				/*{{{  don't need to introduce temporaries here */
			{
#if 0				/* bug TS/1492 18/12/91 */
				if (issame (left, right)) {
					/*if (warning_flags & WARNING_CSE) genwarning(GEN_CSE, 0, 0); */
					tfpexp (left, regs, fpr);
					gensecondary (I_FPDUP);
					compfpdop (op);
				} else
#endif
				{
					const int fpregsforleft = fpregsfor (left);
					const int fpregsforright = fpregsfor (right);

					/*{{{  decide whether to do left or right first, then do it */
					{
						BOOL leftfirst = TRUE;
						/*{{{  set leftfirst TRUE if we do left-hand-side first, FALSE otherwise */
						if (max_INT32 (fpregsforleft, fpregsforright) > MAXFPUREGS)
							leftfirst = (fpregsforleft >= fpregsforright);
						else if (fpregsforleft == fpr)
							leftfirst = TRUE;
						else if (fpregsforright == fpr)
							leftfirst = FALSE;
						else
							/*{{{  go into all the high-tech stuff of counting fprev instructions */
						{
							int fprevs_for_left_first = fprevsfor (left, fpr) + fprevsfor (right, fpr - 1);
							int fprevs_for_right_first = fprevsfor (right, fpr) +
								fprevsfor (left, fpr - 1) + (commutative ? 0 : 1);
							if (fprevs_for_left_first > fprevs_for_right_first)
								leftfirst = FALSE;
						}
						/*}}} */
						/* If we are doing add or mul, and we think we ought to load left first,
						   BUT left is addressable and right is not, and we have enough registers
						   around, load in the opposite order so we can do the load and op in one
						   instruction. */
						if ((op == S_ADD || op == S_MULT) && leftfirst &&
						    isaddressable (left) && !isaddressable (right) && fpregsforright < fpr)
							leftfirst = FALSE;
						/*}}} */
						/*{{{  generate the expression */
						if (leftfirst)
							/*{{{  left; right; op */
						{
							tfpexp (left, regs, fpr);
							tfpexpandop (right, regs, fpr - 1, op);
						}
						/*}}} */
						else
							/*{{{  right; left; (fprev); op */
						{
							tfpexp (right, regs, fpregs);
							if (commutative)
								tfpexpandop (left, regs, fpr - 1, op);
							else {
								tfpexp (left, regs, fpr - 1);
								gensecondary (I_FPREV);
								compfpdop (op);
							}
						}
						/*}}} */
						/*}}} */
					}
					/*}}} */
				}
			}
			/*}}} */
		}
		break;
		/*}}} */
		/*{{{  conversions */
	case S_EXACT:
	case S_ROUND:
	case S_TRUNC:
		{
			treenode *const source = OpOf (tptr);
			const int sourcetype = ntypeof (source);
			const int desttype = MOpTypeOf (tptr);
			const int roundmode = TagOf (tptr);

			switch (sourcetype) {
				/*{{{  REAL32 REAL64 */
			case S_REAL32:
				/* desttype MUST be REAL64 otherwise conversion would have been removed */
				/* EXACT conversion, no possibility of floating point error */
				tfpexp (source, regs, fpregs);
				if (has_directfp) {
					gensecondary (I_FPR32TOR64);
				} else {
					genfpuentry (I_FPUR32TOR64);
				}
				break;
			case S_REAL64:
				/* desttype MUST be REAL32 otherwise conversion would have been removed */
				tfpexp (source, regs, fpregs);
				if (has_directfp) {
					if (roundmode == S_TRUNC) {
						gensecondary (I_FPRZ);
					}
					gensecondary (I_FPR64TOR32);
				} else {
					if (roundmode == S_TRUNC) {
						genfpuentry (I_FPURZ);
					}
					genfpuentry (I_FPUR64TOR32);
				}
				break;
				/*}}} */
				/*{{{  INT64 */
			case S_INT64:
				/* ROUND / TRUNC  no possibility of setting error */
				{
					const int sourcemode = ptrmodeof (addresslopd (P_EXP, source));
#if 0
					const BOOL simple = issimplelocal (source, be_lexlevel);	/* bug 738 5/11/90 */
#else
					/* bug INSdi02168 */
					const BOOL simple = issimplelocal (source, be_lexlevel)
						|| (T9000_instruction_timings && islocal (source, be_lexlevel));
#endif
					geni64tor (sourcemode, source, desttype, roundmode, simple);
				}
				break;
				/*}}} */
				/*{{{  INT32 INT */
			case S_INT32:
			case S_INT:
				{
					int sourcemode = P_EXP;
					sourcemode = simplify (sourcemode, source);
					loadopd (ptrmodeof (sourcemode), source, 0);
					if (desttype == S_REAL32)
						/* ROUND / TRUNC  no possibility of setting error */
					{
						if (roundmode == S_TRUNC) {
							if (has_directfp)
								gensecondary (I_FPRZ);
							else
								genfpuentry (I_FPURZ);
						}
						gensecondary (I_FPI32TOR32);
					} else
						/* EXACT  no possibility of setting error */
						gensecondary (I_FPI32TOR64);
				}
				break;
				/*}}} */
			default:
				geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "tfpexp");
				break;
			}
		}
		break;
		/*}}} */
		/*{{{  name temp */
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_VALPARAM:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_DECL:
	case T_TEMP:
	case T_PREEVALTEMP:
	case S_FNFORMALRESULT:	/* INSdi03203 */
#ifdef MOBILES
		{
			loadelementpointer (tptr, 0, regs);
#if 0
fprintf (stderr, "loading non-local FP thing, tptr =");
printtreenl (stderr, 4, tptr);
#endif
			if (TagOf (NTypeOf (tptr)) == S_MOBILE) {
				int basetype = TagOf (follow_user_type (MTypeOf (NTypeOf (tptr))));

				gensecondary ((basetype == S_REAL32) ? I_FPLDNLSN : I_FPLDNLDB);
			} else {
				gensecondary ((ntypeof (tptr) == S_REAL32) ? I_FPLDNLSN : I_FPLDNLDB);
			}
		}
#else	/* !MOBILES */
		loadelementpointer (tptr, 0, regs);
		gensecondary ((ntypeof (tptr) == S_REAL32) ? I_FPLDNLSN : I_FPLDNLDB);
#endif	/* !MOBILES */
		break;
		/*}}} */
		/*{{{  constant */
	case S_CONSTEXP:
		{
			const int type = ntypeof (tptr);
			if (isfpconst0_0 (tptr, type) && !T9000_alpha_nofpconst (&tx_global))	/* Section 4.11 of SW-0341-2 */
				gensecondary (type == S_REAL32 ? I_FPLDZEROSN : I_FPLDZERODB);
			else {
				if (!isinconstanttable (tptr)) {
					geninternal_is (GEN_ERROR_IN_ROUTINE, 2, "tfpexp");
				}
#ifdef MOBILES
				loadelementpointer (tptr, 0, regs);
#if 0
fprintf (stderr, "loading non-local FP thing, tptr =");
printtreenl (stderr, 4, tptr);
#endif
				gensecondary ((type == S_REAL32) ? I_FPLDNLSN : I_FPLDNLDB);
#else	/* !MOBILES */
				loadelementpointer (tptr, 0, regs);
				gensecondary ((type == S_REAL32) ? I_FPLDNLSN : I_FPLDNLDB);
#endif
			}
		}
		break;
		/*}}} */
		/*{{{  array item */
	case S_ARRAYITEM:
	case S_RECORDITEM:
		/* We can improve the code by using the FPLDNLxxI instruction */
		{
			const int type = ntypeof (tptr);
			if (ASExpOf (tptr) != NULL) {
				treenode *name = nameof (tptr);
				texp (ASExpOf (tptr), regs);
#if 0
				loadnamepointer (name, 0);
#else /* new version, fixing bug 570 CO'N 23/7/90 */
				if (ispointer (name)) {
					loadnamepointer (name, 0);
					genprimary (I_LDNLP, ASOffsetOf (tptr));
				} else {
					loadnamepointer (name, ASOffsetOf (tptr));
				}
#endif
				gensecondary ((type == S_REAL32) ? I_FPLDNLSNI : I_FPLDNLDBI);
			} else {
				/* Note that this adds in ASOffsetOf itself - CO'N 13/12/90 */
				loadelementpointer (tptr, 0, regs);
				gensecondary ((type == S_REAL32) ? I_FPLDNLSN : I_FPLDNLDB);
			}
		}
		break;
		/*}}} */
		/*{{{  function instance */
	case S_FINSTANCE:
		/* It must be a single-valued real function, so the result will
		   come back in the fp Areg */
		if (TagOf (INameOf (tptr)) == N_PREDEFFUNCTION)
			tpredef (tptr, NULL);
		else
			tinstance (tptr);
		{
			BOOL isr32 = (ntypeof (tptr) == S_REAL32) ? TRUE : FALSE;
			notefpresult (isr32 ? "s" : "d");
		}
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
	case S_PRAGMA:		/* bug 829 19/9/91 */
	case S_TYPEDECL:
#ifdef MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
#endif
		{
			treenode *resultexp;
			treenode *specsptr = tptr;

			tptr = tspecs (tptr);
			resultexp = ThisItem (VLResultListOf (tptr));
			tprocess (VLBodyOf (tptr));
			tdespecs (specsptr);
			tpreexp (resultexp);
			tfpexp (resultexp, regs, fpregs);
		}
		break;
		/*}}} */
	default:
		badtag (genlocn, TagOf (tptr), "tfpexp");
	}
}

/*}}}*/
/*{{{  PUBLIC void tfpassign (destmode, dest, sourcemode, source, regs)*/
/*{{{  COMMENT*/
/**********************  Start comment out ****************************
@*{{{  *@
@*****************************************************************************
 *
 *  tfpassign generates code to assign floating point expression
 *            (sourcemode, source) to (destmode, dest)
 *
 *****************************************************************************@
@*}}}*@
 **********************   End comment out  ****************************/
/*}}}*/
PUBLIC void
tfpassign (int type, int destmode, treenode * dest, int sourcemode, treenode * source, int regs)
{
	/* int r = (regs == MANY_REGS) ? MAXREGS : regs; */
/*destmode = ptrmodeof(destmode); *//* bug 1315 22/8/91 */

	if (preeval (sourcemode, source))
		/*{{{  generate source to temp; assign temp to dest */
	{
		sourcemode = simplify (sourcemode, source);
		tsimpleassign (ntypeof (dest), destmode, dest, sourcemode, source, regs);
	}
	/*}}} */
	else if (fpregsfor (dest) < MAXFPUREGS)
		/*{{{  load source into fpu; load destptr into cpu; store */
	{
		tfpexp (source, regs, MANY_REGS);
		destmode = ptrmodeof (destmode);
		loadopd (destmode, dest, 0);
		checkerror ();
		gensecondary ((type == S_REAL32) ? I_FPSTNLSN : I_FPSTNLDB);
	}
	/*}}} */
	else {			/* if (regsfor(source) < regs) */
		/*{{{  destaddr; source; stind */
		destmode = ptrmodeof (destmode);
		loadopd (destmode, dest, 0);
		tfpexp (source, regs, MANY_REGS);
		checkerror ();
		gensecondary ((type == S_REAL32) ? I_FPSTNLSN : I_FPSTNLDB);
	}
	/*}}} */
}

/*}}}*/
