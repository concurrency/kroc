/* $Id: gen4.c,v 1.8 1998/11/04 11:17:09 dcw Exp $ */

/*
 *	code generator - expression generation
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
 *               call gen_func_results on return from function
 *               call genshiftimmediate when operand is constant
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
#include "bind2def.h"
#include "gen1def.h"
#include "gen2def.h"
#include "gen4def.h"
#include "gen5def.h"
#include "gen7def.h"
#include "gen8def.h"
#include "gen11def.h"
#include "code1def.h"
/*}}}*/

/*{{{  forward*/
PRIVATE int revsfor (treenode * tptr, int regs);
/*}}}*/

/*{{{  PRIVATE BOOL timescanusebcnt*/
/*****************************************************************************
 *
 *  timescanusebcnt returns TRUE if value is a low power of bytesperword
 *
 *****************************************************************************/
PRIVATE BOOL timescanusebcnt (treenode * tptr)
{
	if (isconst (tptr)) {
		const INT32 v = LoValOf (tptr);
		const BOOL use_multiple_bcnts =	/* TS/1504 07/04/92 */
			!T9000_instruction_timings && ((code_style_flags & CODE_STYLE_SPACE_NOT_TIME) == 0);

		return ((bytesperword == 2) && (v == 2))
			|| ((bytesperword == 4) && ((v == 4) || (use_multiple_bcnts && ((v == 16) || (v == 64)))));
	}
	return FALSE;
}

/*}}}*/
/*{{{  PUBLIC INT32 lshiftcanusebcnt*/
PUBLIC INT32 lshiftcanusebcnt (const INT32 shiftcount)
/* Returns non-zero if we can use bcnt for shift.
   Returns the number of bcnt to use.
*/
{
	if (!kroc_flag) {	/* inappropriate for non-transputer target -- DCW */
		const BOOL use_multiple_bcnts = !T9000_instruction_timings &&	/* bug 1371 15/8/91 */
			((code_style_flags & CODE_STYLE_SPACE_NOT_TIME) == 0);	/* TS/1504 07/04/92 */

		/* We don't use multiple bcnts on a 16-bit processor */
		if (bytesperword == 2 && (shiftcount == 1 /*|| shiftcount == 2 || shiftcount == 3 */ ))
			return shiftcount;
		else if (bytesperword == 4 && (shiftcount == 2 || (use_multiple_bcnts && (shiftcount == 4 || shiftcount == 6))))
			return shiftcount / 2;
	}
	return 0;
}

/*}}}*/
/*{{{  register counting*/
/*{{{  PRIVATE int correct_for_bsub*/
PRIVATE int correct_for_bsub (const int regs, treenode * base, INT32 offset)
{
	if (use_bsub_not_adc && offset != 0) {
		const int b = bytesinscalar (basetype (gettype (base)));
		/*fprintf(outfile, "\n-- correct_for_bsub: offset: %ld, bytes: %d", offset, b); */
		if (b < bytesperword) {
			if (b == 2 && use_shortintops)
				offset *= 2;	/* turn into bytes */
			if ((offset & ((INT32) bytesperword - 1)) != 0)	/* not a whole number of words */
				return (int) max_INT32 (regs, 2);	/* uses 2 for ld address; ldc n; bsub; */
		}
	}
	return regs;
}

/*}}}*/
/*{{{  PRIVATE int regsforpair*/
/* This is used by various regsfor... routines.
   It returns the number of registers required to 'combine' two expressions.
*/
PRIVATE int regsforpair (const int r1, const int r2)
{
	if (r1 == r2)
		return r1 + 1;
	else
		return (int) max_INT32 (r1, r2);
}

/*}}}*/
/*{{{  PRIVATE int regsforaddr(tptr)*/
/*****************************************************************************
 *
 *  regsforaddr returns the minimum number of registers required to
 *              evaluate a pointer to the element or constant table 'tptr'.
 *
 *****************************************************************************/
PRIVATE int regsforaddr (treenode *tptr)
{
	switch (TagOf (tptr)) {
	default:
		return (regsfor (tptr));
		/*{{{  name */
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_DECL:
	case N_PARAM:
	case N_VALPARAM:
	case N_RESULTPARAM:
	case S_STRING:
	case S_CONSTCONSTRUCTOR:
	case N_LABELDEF:
	case N_PROCDEF:
#ifdef MOBILES
	case N_MPROCDECL:
#endif
	case N_LIBPROCDEF:	/* bug TS/1240 12/05/92 */
	case N_LIBMPROCDECL:
	case N_SFUNCDEF:
	case N_LFUNCDEF:
	case N_LIBFUNCDEF:
	case T_TEMP:
	case T_PREEVALTEMP:
		return 1;
		/*}}} */
		/*{{{  subscript */
	case S_ARRAYITEM:
	case S_RECORDITEM:
		{
			const int expregs = (ASExpOf (tptr) != NULL) ? regsfor (ASExpOf (tptr)) : 0;
			const int baseregs = regsforaddr (ASBaseOf (tptr));
			return correct_for_bsub (regsforpair (expregs, baseregs), ASBaseOf (tptr), ASOffsetOf (tptr));
		}
	case S_ARRAYSUB:
	case S_RECORDSUB:
		return regsforaddr (ASBaseOf (tptr));
		/*}}} */
		/*{{{  segment */
	case S_SEGMENTITEM:
		/* Return number of registers needed to load a pointer to the segment */
		{
			const int baseregs = regsforaddr (SNameOf (tptr));
			const int expregs = (SSubscriptExpOf (tptr) != NULL)
				? regsfor (SSubscriptExpOf (tptr)) : 0;
			return correct_for_bsub (regsforpair (expregs, baseregs), SNameOf (tptr), SOffsetOf (tptr));
		}
	case S_SEGMENT:
		return regsforaddr (SNameOf (tptr));
		/*}}} */
		/*{{{  constructor */
#if 0				/* no constructors are passed to the backend any more */
	case S_CONSTRUCTOR:	/* Added by CO'N 2/4/90 */
		return MAXREGS;
#endif
		/*}}} */
	}
}

/*}}}*/
/*{{{  PRIVATE int regsfordop (tptr)*/
/*****************************************************************************
 *
 *  regsfordop returns the minimum number of registers required to evaluate
 *             the dyadic operator tree 'tptr'.
 *
 *****************************************************************************/
PRIVATE int regsfordop (treenode * tptr)
{
#ifdef DEBUG
	const int r = regsforpair (regsfor (LeftOpOf (tptr)), regsfor (RightOpOf (tptr)));
	DEBUG_MSG (("regsfordop: %s needs %d\n", itagstring (TagOf (tptr)), r));
	return r;
#else
	if (t450a_workarounds && (((TagOf (tptr) == S_DIV) || (TagOf (tptr) == S_REM))))
		return MAXREGS;
	else
		return regsforpair (regsfor (LeftOpOf (tptr)), regsfor (RightOpOf (tptr)));
#endif
}

/*}}}*/
/*{{{  PRIVATE int regsforfpoperand (treenode *tptr)*/
/*****************************************************************************
 *
 *  regsforfpoperand returns the number of integer registers needed to
 *                   load tptr onto the floating point stack
 *
 *****************************************************************************/
PRIVATE int regsforfpoperand (treenode * tptr)
{
	return isaddressable (tptr) ? regsforaddr (tptr) : regsfor (tptr);
}

/*}}}*/
/*{{{  PUBLIC int regsfor (tptr)          with added inline fp*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  regsfor returns the minimum number of registers required to evaluate the
 *          expression 'tptr'. It only copes INT or shorter expressions on
 *          an integer processor, but will handle all reals on an fp processor
 *
 *****************************************************************************/
/*}}}*/
PUBLIC int regsfor (treenode * tptr)
{
	switch (TagOf (tptr)) {
	default:
		badtag (genlocn, TagOf (tptr), "regsfor");
		return -1;
		/*{{{  monadic operators */
	case S_NEG:
		if (isreal (MOpTypeOf (tptr)))	/* Added to fix bug 295, 30/4/90 by CO'N */
			return (has_fpu_core ? regsforfpoperand (OpOf (tptr)) : MAXREGS);
		/* We have a choice of
		   ldc 0; e; sub       or     e; not; adc 1
		 */
		/*return (istargetintsize(MOpTypeOf(tptr))) ? regsfor(OpOf(tptr)) : MAXREGS; */
		return (!fitsinword (MOpTypeOf (tptr)) || (!use_shortintops && isshortint (MOpTypeOf (tptr))))	/* bug 1345 24/7/91 */
			? MAXREGS : regsfor (OpOf (tptr));	/* bug 1339 18/7/91 */
	case S_UMINUS:
		/*return (istargetintsize(MOpTypeOf(tptr))) ? max_INT32(2, regsfor(OpOf(tptr))) */
		return fitsinword (MOpTypeOf (tptr)) ? (int) max_INT32 (2, regsfor (OpOf (tptr)))	/* bug 1339 18/7/91 */
			: MAXREGS;
	case S_NOT:
		return (istargetintsize (MOpTypeOf (tptr))) ? regsfor (OpOf (tptr)) : MAXREGS;
	case S_BITNOT:
		/*return (istargetintsize(MOpTypeOf(tptr))) ? regsfor(OpOf(tptr)) : MAXREGS; */
		return (fitsinword (MOpTypeOf (tptr))) ? regsfor (OpOf (tptr)) : MAXREGS;	/* bug 1339 18/7/91 */
	case S_SIZE:
	case S_ELSIZE:
		/* Our implementation guarantees that any array dimensions will be
		   preevaluated into temporaries if they need more than one register to
		   evaluate them. */
/*return regsfor(dimexpof(OpOf(tptr), 0)); *//* this should always be 1 */
#if 0
fprintf (stderr, "regsfor: S_SIZE/S_ELSIZE.  regsfor(dimexpof(OpOf (tptr))) = %d.  tptr = ", regsfor (dimexpof (OpOf (tptr), 0)));
printtreenl (stderr, 4, tptr);
#endif
		return 1;
	case S_SEGSTART:
		return regsfor (SStartExpOf (OpOf (tptr)));
	case S_ADDRESSOF:
		return regsforaddr (OpOf (tptr));
#ifdef MOBILES
	case S_ADDROF:
	case S_HWADDROF:
	case S_CLONE:
		return regsfor (OpOf (tptr));
#endif
	case S_TYPEHASHOF:
		switch (TagOf (OpOf (tptr))) {
		case N_PROCDEF:
		case N_LIBPROCDEF:
		case N_STDLIBPROCDEF:
		case N_SCPROCDEF:
			return 1;			/* these end up constant */
		default:
			return regsfor (OpOf (tptr));
		}
		break;
		/*}}} */
		/*{{{  dyadic operators */
	case S_ADD:
	case S_SUBTRACT:
	case S_MULT:
	case S_DIV:
	case S_REM:
	case S_RSHIFT:
	case S_EQ:
	case S_NE:
	case S_LS:
	case S_LE:
	case S_GR:
	case S_GE:
	case S_CSUB0:
	case S_CCNT1:
		if (isreal (DOpTypeOf (tptr))) {
			/*{{{  we do reals inline if we have the appropriate hardware */
			return (has_fpu_core && (TagOf (tptr) != S_REM || has_fprem))
				? (int) max_INT32 (regsforfpoperand (LeftOpOf (tptr)), regsforfpoperand (RightOpOf (tptr)))
				: MAXREGS;
			/*}}} */
		} else if (istargetintsize (DOpTypeOf (tptr)) || (use_shortintops && isshortint (DOpTypeOf (tptr)))) {		/* bug 1345 23/7/91 */
			/*{{{  inline code */
			treenode *left = LeftOpOf (tptr);
			treenode *right = RightOpOf (tptr);

			switch (TagOf (tptr)) {
				/*{{{  S_ADD */
			case S_ADD:
				/* add constant optimisation */
				if (isconst (left) && !isinconstanttable (left)) {
					return regsfor (right);
				}
				if (isconst (right) && !isinconstanttable (right)) {
					return regsfor (left);
				}
				break;
				/*}}} */
				/*{{{  S_SUBTRACT */
			case S_SUBTRACT:
				/* add constant optimisation */
				if (isconst (right) && !isinconstanttable (right))
					return regsfor (left);
				break;
				/*}}} */
			}
			return regsfordop (tptr);
			/*}}} */
		}
#ifdef OCCAM2_5
		else if (DOpTypeOf (tptr) == S_BYTE) {
			return regsfordop (tptr);
		}
#endif
		else {
			return MAXREGS;
		}
	case S_BITAND:
	case S_BITOR:
	case S_XOR:
	case S_PLUS:
	case S_MINUS:
	case S_TIMES:
	case S_LSHIFT:
#ifdef OCCAM2_5
		if (DOpTypeOf (tptr) == S_BYTE)
			return regsfordop (tptr);
#endif
		if (fitsinword (DOpTypeOf (tptr)))
		 {		/* bug 1339 19/7/91 */
			/*{{{  inline code */
			if (kroc_flag)	/* inappropriate for non-transputer target -- DCW */
				return regsfordop (tptr);
			else {
				treenode *left = LeftOpOf (tptr);
				treenode *right = RightOpOf (tptr);
				switch (TagOf (tptr)) {
					/*{{{  S_TIMES */
				case S_TIMES:
					/* bcnt optimisation */
					if (timescanusebcnt (left))
						return regsfor (right);
					if (timescanusebcnt (right))
						return regsfor (left);
					break;
					/*}}} */
					/*{{{  S_LSHIFT */
				case S_LSHIFT:	/* added for bug 1168 14/3/91 CON */
					/* bcnt optimisation */
					if (isconst (right) && lshiftcanusebcnt (LoValOf (right)))
						return regsfor (left);
					break;
					/*}}} */
				}
				return regsfordop (tptr);
			}
		}
		/*}}} */
		else
			return MAXREGS;
	case S_EVAL:
		return regsfor (RightOpOf (tptr));
	case S_AND:
	case S_OR:
		return (int) max_INT32 (regsfor (LeftOpOf (tptr)), regsfor (RightOpOf (tptr)));
		/*}}} */
		/*{{{  conversion */
	case S_EXACT:
	case S_ROUND:
	case S_TRUNC:
		{
			treenode *source = OpOf (tptr);
			const int sourcetype = ntypeof (source);
			const int desttype = MOpTypeOf (tptr);
			if (isreal (sourcetype) || isreal (desttype)) {
				if (has_fpu_core)
					/*{{{  use the specialised hardware */
					/* must be a null conversion or REAL32 to REAL64 */
				{
					if (isreal (desttype)) {
						if (sourcetype == S_INT64)
							return (int) max_INT32 (2, regsforfpoperand (source));
						else
							return regsforfpoperand (source);
					} else if (T9000_alpha_nofpconv (&tx_global))	/* Section 4.2.7, 4.2.8, 4.2.11, 4.2.12 of SW-0341-2 */
						return MAXREGS;	/* bug TS/1729 15/06/92 */
					else	/* desttype is integer (and fits in a word), sourcetype is real */
						return regsforfpoperand (source);
				}
				/*}}} */
				else
					return MAXREGS;
			}
			/*{{{  if double length or quadruple length  => MAXREGS */
			/* For double-length,
			   this is not optimal as there are some cases where we don't need all
			   the registers, eg. if the source only requires one register to load,
			   we only need two registers (load each half and csngl), if we aren't
			   checking, we only need to load the lower half anyway. */
			if (isdoublelength (sourcetype) || isquadlength (sourcetype))
				return (MAXREGS);
			/*}}} */

			/*if ((CONVERSIONCHECKING && hasgreaterrange(sourcetype, desttype)) ||
			   (hasgreaterrange(desttype, sourcetype) && issignedtype(sourcetype)))
			   return max_INT32(2, regsfor(OpOf(tptr))); */
			if ((CONVERSIONCHECKING || (!use_shortintops && isshortint (sourcetype)))	/* bug 1367 14/8/91 */
			    &&hasgreaterrange (sourcetype, desttype)) {
				if (use_shortintops && ((desttype == S_BYTE) || (desttype == S_INT16) || (desttype == S_UINT16)))
					return regsfor (OpOf (tptr));
				return (int) max_INT32 (2, regsfor (OpOf (tptr)));
			} else if (hasgreaterrange (desttype, sourcetype) && issignedtype (sourcetype)) {
				if (use_shortintops && ((sourcetype == S_INT16) || (sourcetype == S_UINT16))) {
					return regsfor (OpOf (tptr));
				}
				return (int) max_INT32 (2, regsfor (OpOf (tptr)));
			} else
				return regsfor (OpOf (tptr));
		}
		/*}}} */
		/*{{{  literals names  constant expression */
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_DECL:
	case N_PARAM:
	case N_VALPARAM:
	case N_RESULTPARAM:
		/* surely this is the same as 'needtemptoload' ? */
		/* See  if it has to be preevaluated to a temp before we can load it */
		return (!use_shortintops &&	/* always 1 register if has T9000 shorts */
			isshortint (TagOf (follow_user_type (NTypeOf (tptr)))) && ispointer (tptr)) ? MAXREGS : 1;
	case N_REPL:
	case S_CONSTEXP:
	case S_ASMNAME:
	case S_STRING:
	case S_CONSTCONSTRUCTOR:
		/* S_NULLARRAY added 11/02/2003 (frmb) */
	case S_NULLARRAY:
		return 1;
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
			 /** Need to think about floating point predefs on t8 */
	case S_PRAGMA:		/* bug 829 19/9/91 */
	case S_TYPEDECL:
#ifdef MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
	case S_MPROCDECL:
#endif
		return MAXREGS;
		/*}}} */
		/*{{{  subscript */
	case S_ARRAYITEM:
	case S_RECORDITEM:
		{
			const int expregs = (ASExpOf (tptr) != NULL) ? regsfor (ASExpOf (tptr)) : 0;
			if (!use_shortintops && isshortint (ntypeof (tptr)))
				/* It has to be preevaluated to a temp before we can load it */
				return (int) max_INT32 (MAXREGS, expregs);
			else {
				const int baseregs = regsforaddr (ASBaseOf (tptr));
				return correct_for_bsub (regsforpair (baseregs, expregs), ASBaseOf (tptr), ASOffsetOf (tptr));
			}
		}
		/*}}} */
		/*{{{  special parameter types */
	case S_PARAM_STATICLINK:
	case S_PARAM_VSP:
#ifdef MOBILES
	case S_PARAM_MSP:	/* only require at most 1 register */
	case S_PARAM_MPP:
	case S_PARAM_FB:
	case S_HIDDEN_TYPE:
#endif
	case S_FNFORMALRESULT:
		return 1;
#ifdef MOBILES
	case S_NTH_DIMENSION:
		return regsfor (LeftOpOf (tptr));
	case S_UNDEFINED:
		return regsfor (OpOf (tptr));
	case S_NEW_ARRAY:
		return regsfor (ARDimLengthOf (tptr));
	case S_ALLOC_PROC:
	case S_NEW_BARRIER:
		return 1;
#endif
	case S_HIDDEN_PARAM:
		return regsfor (HExpOf (tptr));
	case S_FNACTUALRESULT:
		return regsforaddr (HExpOf (tptr));
		/*}}} */
		/*{{{  temporary */
	case T_TEMP:
		return regsfor (NDeclOf (tptr));
	case T_PREEVALTEMP:	/* Preevaluated temporary */
		return 1;
		/*}}} */
		/*{{{  dummy expression */
	case S_DUMMYEXP:
		return 0;
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC int regsforstore (dest)*/
/*****************************************************************************
 *
 *  regsforstore returns the number of registers required to store the contents
 *               of Areg in dest (the number returned does not include the
 *               register holding the value to be stored) .
 *
 *****************************************************************************/
PUBLIC int regsforstore (treenode * dest)
{
	if (issimplelocal (dest, be_lexlevel))
		return (0);	/* Store local */
	else if (!use_shortintops && isshortint (ntypeof (dest)))	/* added 19/10/90 for bug 777 */
		return MAXREGS;	/* Store via a block move */
	else
		return (regsforaddr (dest));	/* Store through a pointer */
}

/*}}}*/
/*{{{  PUBLIC int regsforopd(opdmode, opd)*/
/*****************************************************************************
 *
 *  regsforopd returns the minimum number of registers required to evaluate
 *             the operand (opdmode, opd).
 *
 *****************************************************************************/
PUBLIC int regsforopd (int opdmode, treenode * opd)
{
	switch (opdmode) {
	case P_EXP:
		return (regsfor (opd));
	case P_PTR:
		return (regsforaddr (opd));
	case P_TEMP:
	case P_TEMPPTR:
		return (1);
	default:
		geninternal_is (GEN_BAD_OPD, opdmode, "regsforopd");
	}
	return (0);		/* Not reached */
}

/*}}}*/
/*{{{  PUBLIC int regsforaddropd (opdmode, opd)*/
/*****************************************************************************
 *
 *  regsforaddropd returns the minimum number of registers required to load
 *                 a pointer to (opdmode, opd).
 *
 *****************************************************************************/
PUBLIC int regsforaddropd (int opdmode, treenode * opd)
{
	switch (opdmode) {
	case P_TEMP:
		return 1;
	case P_EXP:
		return (regsforaddr (opd));
	default:
		geninternal_is (GEN_BAD_OPD, opdmode, "regsforaddropd");
	}
	return (0);		/* Not reached */
}

/*}}}*/
/*}}}*/
/*{{{  rev instruction counting*/
/*{{{  PRIVATE int revsfordop (left, right, commutes, regs)*/
/*****************************************************************************
 *
 *  revsfordop returns the minimum number of 'rev' instructions required
 *             to evaluate a dyadic operation upon the two operands 'left'
 *             and 'right', in at most 'regs' registers.
 *             if the dyadic operation is commutative, then 'op_commutes' is TRUE.
 *
 *****************************************************************************/
PRIVATE int revsfordop (treenode * left, treenode * right, BOOL op_commutes, int regs)
{
	/*int revbase = (op_commutes ? 0 : 1); */
	const int revbase = !op_commutes;	/* quicker */

	if (regsfor (left) == regs)
		/*{{{  must generate left; right; op */
		return (revsfor (left, regs) + revsfor (right, regs - 1));
	/*}}} */
	else if (regsfor (right) == regs)
		/*{{{  must generate right; left; (rev; ) op */
		return (revsfor (right, regs) + revsfor (left, regs - 1) + revbase);
	/*}}} */
	else
		/*{{{  return the minimum revs  for either left first or right first */
		return (int) min_INT32 ((INT32) revsfor (left, regs) + revsfor (right, regs - 1),
					(INT32) revsfor (right, regs) + revsfor (left, regs - 1) + revbase);
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE int revsfor (tptr, regs)*/
/*****************************************************************************
 *
 *  revsfor returns the minimum number  of 'rev' instructions neccessarry
 *          when evaluating expression 'tptr' in at most 'regs' registers.
 *
 *****************************************************************************/
PRIVATE int revsfor (treenode * tptr, int regs)
{
	switch (TagOf (tptr)) {
		/*{{{  monadic operators */
		/*{{{  NOT BITNOT UMINUS ADDRESSOF */
	case S_NOT:
	case S_BITNOT:
	case S_UMINUS:
	case S_ADDRESSOF:
	case S_NEG:
		/* FOR NEG: If we generate
		   ldc 0; e; sub         =>  revsfor(e, regs - 1)
		   e; not; adc 1         =>  revsfor (e, regs)
		 */
		return (revsfor (OpOf (tptr), regs));
		/*}}} */
#if 0				/* now combined with the other monadics */
		/*{{{  NEG */
	case S_NEG:
		return (revsfor (OpOf (tptr), regs));
		/*}}} */
#endif
		/*{{{  SIZE */
	case S_SIZE:
	case S_ELSIZE:
#ifdef MOBILES
		if (isdynmobilearray (OpOf (tptr))) {
			/* meaningless (i think) */
			return 0;
		} else {
			return revsfor (dimexpof (OpOf (tptr), 0), regs);
		}
#else	/* !MOBILES */
		return (revsfor (dimexpof (OpOf (tptr), 0), regs));
#endif	/* !MOBILES */
#ifdef MOBILES
	case S_NTH_DIMENSION:
		/* no REV instrs needed, at all :) */
		return 0;
	case S_ADDROF:
	case S_HWADDROF:
		if (isdynmobilearray (OpOf (tptr))) {
			return 0;
		} else {
			return revsfor (OpOf (tptr), regs);
		}
#endif	/* MOBILES */
	case S_TYPEHASHOF:
		switch (TagOf (OpOf (tptr))) {
		case N_PROCDEF:
		case N_LIBPROCDEF:
		case N_STDLIBPROCDEF:
		case N_SCPROCDEF:
		case N_INLINEPROCDEF:
			return 0;			/* these will be constant expressions */
		default:
			return revsfor (OpOf (tptr), regs);
		}
		break;
	case S_SEGSTART:
		return revsfor (SStartExpOf (OpOf (tptr)), regs);
		/*}}} */
		/*}}} */
		/*{{{  conversion */
	case S_EXACT:
		return (revsfor (OpOf (tptr), regs));
	case S_ROUND:
	case S_TRUNC:
#if 0				/* we've done this check in trans */
		if (ntypeof (OpOf (tptr)) == MOpTypeOf (tptr))
			return (revsfor (OpOf (tptr), regs));
		else
#endif
			return (MAXREGS);
		/*}}} */
		/*{{{  dyadic operators */
		/*{{{  S_ADD */
	case S_ADD:		/* add constant optimisation */
		{
			treenode *left = LeftOpOf (tptr), *right = RightOpOf (tptr);

			if (TagOf (left) == S_CONSTEXP)
				return (revsfor (right, regs));
			if (TagOf (right) == S_CONSTEXP)
				return (revsfor (left, regs));
			return (revsfordop (LeftOpOf (tptr), RightOpOf (tptr), TRUE, regs));
		}
		/*}}} */
		/*{{{  S_SUBTRACT */
	case S_SUBTRACT:	/* add constant optimisation */
		{
			treenode *left = LeftOpOf (tptr), *right = RightOpOf (tptr);

			if (TagOf (right) == S_CONSTEXP)
				return (revsfor (left, regs));
			return (revsfordop (left, right, FALSE, regs));
		}
		/*}}} */
	case S_MULT:
	case S_BITAND:
	case S_BITOR:
	case S_XOR:
	case S_PLUS:
		return (revsfordop (LeftOpOf (tptr), RightOpOf (tptr), TRUE, regs));
	case S_AND:
	case S_OR:
	case S_DIV:
	case S_REM:
	case S_LSHIFT:
	case S_RSHIFT:
	case S_MINUS:
	case S_TIMES:
	case S_EQ:
	case S_NE:
	case S_LS:
	case S_LE:
	case S_GR:
	case S_GE:
	case S_CSUB0:
	case S_CCNT1:
		return (revsfordop (LeftOpOf (tptr), RightOpOf (tptr), FALSE, regs));
	case S_EVAL:
		return revsfor (RightOpOf (tptr), regs);
		/*}}} */
		/*{{{  names  constant expression function instance temps arraydim ... */
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_PARAM:
	case N_VALPARAM:
	case N_RESULTPARAM:
	case N_DECL:
	case N_REPL:
	case S_CONSTEXP:
	case S_ASMNAME:
	case S_FINSTANCE:
	case S_VALABBR:
	case S_ABBR:
	case S_VALRETYPE:
	case S_RETYPE:		/* spec..valof */
	case S_PROCDEF:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
	case S_TPROTDEF:
	case S_SPROTDEF:
	case S_DECL:
	case S_VALOF:
	case T_TEMP:
	case T_PREEVALTEMP:
	case S_FNFORMALRESULT:	/* INSdi03203 */
	case S_SEGMENT:
	case S_PRAGMA:		/* bug 829 19/9/91 */
	case S_TYPEDECL:
#ifdef MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
	case S_MPROCDECL:
#endif
		return (0);
		/*}}} */
		/*{{{  array item */
	case S_ARRAYITEM:
	case S_RECORDITEM:
		{
			treenode *subscriptexp = ASExpOf (tptr);
			if (subscriptexp != NULL)
				return (revsfor (subscriptexp, regs));
			else
				return (0);
		}
		/*}}} */
		/*{{{  segment item */
	case S_SEGMENTITEM:	/* This is getting a bit complicated .. so give up */
		return 0;
		/*}}} */
	default:
		badtag (genlocn, TagOf (tptr), "revsfor");
	}
	return (0);		/* Not reached */
}

/*}}}*/
/*}}}*/
/*{{{  expressions*/
/*{{{  PRIVATE void tdimension(tptr, regs)*/
/*****************************************************************************
 *
 * tdimension   generates code to load the size of
 *              dimension of tptr into Areg
 *              At the moment this routine only copes with tptr being an
 *              element, though it could be expanded to work with any
 *              expression.
 *
 *****************************************************************************/
PRIVATE void tdimension (treenode * tptr, int regs)
{
#if 0
fprintf (stderr, "tdimension: regs = %d, tptr = ", regs);
printtreenl (stderr, 4, tptr);
#endif
#ifdef MOBILES
	if ((TagOf (tptr) == S_RECORDITEM) && ismobile (ASBaseOf (tptr)) && isdynmobilearray (ASIndexOf (tptr))) {
		/* loading dimension size of a nested MOBILE */
		loaddynmobilesize (tptr, 1);		/* first dimension is 1 here */
	} else
#endif
	{
		texp (dimexpof (tptr, 0), regs);
	}
}

/*}}}*/
/*{{{  PUBLIC void tcheckdim (type1, type2)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tcheckdim takes two array type trees, 'type1' and 'type2', and generates
 *                   code to check that a hidden dimensions in either type
 *                   matches the corresponding dimension in the other type.
 *                   Code to check a dimension is of the form
 *                     ld dim1; eqc dim2; ldc 1; ccnt1
 *                   or
 *                     ld dim1; ld dim2; diff; ldc 1; csub0
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void tcheckdim (treenode *type1, treenode *type2)
{
#if 0
fprintf (stderr, "gen4: tcheckdim: type1 =");
printtreenl (stderr, 4, type1);
fprintf (stderr, "----: tcheckdim: type2 =");
printtreenl (stderr, 4, type2);
#endif
#ifdef MOBILES
	/* step over any MOBILEs in the type(s) */
	if (TagOf (type1) == S_MOBILE) {
		type1 = MTypeOf (type1);
	}
	if (TagOf (type2) == S_MOBILE) {
		type2 = MTypeOf (type2);
	}
#endif
	if (ARDimOf (type2) != (-1)) {
		treenode *temp = type1;
		type1 = type2;
		type2 = temp;
	}
	if (ARDimOf (type1) != (-1)) {
		/*{{{  one dimension is constant */
		texpopd (P_EXP, ARDimLengthOf (type2), MANY_REGS);	/*        ld    dim2   */
		genprimary (I_EQC, ARDimOf (type1));			/*        eqc   dim1   */
		loadconstant (1);					/*        ldc   1      */
		gensecondary (I_CCNT1);					/*        ccnt1        */
		/*}}} */
	} else {
		/*{{{  both non-constant */
		tload2regs (P_EXP, ARDimLengthOf (type1),	/*         ld    dim1   */
			    P_EXP, ARDimLengthOf (type2),	/*         ld    dim2   */
			    TRUE, TRUE);
		gensecondary (I_DIFF);				/*         diff         */
		genprimary (I_LDC, 1);				/*         ldc   1      */
		gensecondary (I_CSUB0);				/*         csub0        */
		/*}}} */
	}
	throw_the_result_away ();
}

/*}}}*/
/*{{{  PUBLIC void tcheckdimensions (type1, type2)*/
/*****************************************************************************
 *
 *  tcheckdimensions takes two type trees, 'type1' and 'type2', and generates
 *                   code to check that any hidden dimensions in either type
 *                   match the corresponding dimensions in the other type.
 *                   Code to check a dimension is of the form
 *                     ld dim1; eqc dim2; ldc 1; ccnt1
 *                   or
 *                     ld dim1; ld dim2; diff; ldc 1; csub0
 *
 *****************************************************************************/
PUBLIC void tcheckdimensions (treenode *type1, treenode *type2)
{
#if 0
printf ("tcheckdimensions: type1 is");
printtree (stdout, 0, type1);
printf ("\n, type2 is");
printtree (stdout, 0, type2);
printf ("\n");
#endif
#ifdef MOBILES
	/* step over MOBILE types */
	if (TagOf (type1) == S_MOBILE) {
		type1 = MTypeOf (type1);
		if (TagOf (type2) == S_MOBILE) {
			type2 = MTypeOf (type2);

			/* sort this here..? */
			if ((TagOf (type1) == S_ARRAY) && !ARDimLengthOf (type1)) {
				return;
			} else if ((TagOf (type2) == S_ARRAY) && !ARDimLengthOf (type2)) {
				return;
			}
		}
	} else if (TagOf (type2) == S_MOBILE) {
		type2 = MTypeOf (type2);
	}
#endif
	while (TagOf (type1) == S_ARRAY) {
		if ((ARDimOf (type1) == (-1) || ARDimOf (type2) == (-1)) && !issame (ARDimLengthOf (type1), ARDimLengthOf (type2))) {
			tcheckdim (type1, type2);
		}
		type1 = follow_user_type (ARTypeOf (type1));
		type2 = follow_user_type (ARTypeOf (type2));
	}
}

/*}}}*/
/*{{{  PRIVATE void toverflowcheck*/
PRIVATE void toverflowcheck (const BOOL signed_short, const BOOL byte_short)
{
	if (NEED_ERRORS) {
		if (signed_short && !T9000_gamma_carryin (&tx_global))
			gensecondary (I_CS);	/* check still in range */
		else if (byte_short) {
			if (use_shortintops)
				gensecondary (I_CB);
			else {
				genprimary (I_LDC, 256);
				gensecondary (I_CSUB0);
			}
		}
	}
}

/*}}}*/
/*{{{  PRIVATE void toverflowmask*/
PRIVATE void toverflowmask (const BOOL signed_short, const BOOL byte_short, const BOOL signextend_result)
{
	if (signed_short && signextend_result)
		gensecondary (I_XSWORD);	/* re sign-extend */
	else if (byte_short) {
		genprimary (I_LDC, 255);
		gensecondary (I_AND);
	}
}

/*}}}*/
/*{{{  PUBLIC void t450_divrem_workaround(int op)*/
PUBLIC void t450_divrem_workaround (int op)
{
	gensecondary (I_REV);	/* move dividend to A */
	gencomment0 ("DIV REM Workaround for T450");
	genprimary (I_LDC, 1);	/* A = (dividend - 1) */
	gensecondary (I_DIFF);
	gensecondary (I_XDBLE);	/* dividend in A, sign extension in B */
	gensecondary (I_REV);	/* move sign extension into A */
	genprimary (I_CJ, 12);	/* if A is 0 then dividend was +ve - cj positive */
	gensecondary (I_NOT);	/* A = -dividend */
	gensecondary (I_REV);	/* swap dividend and divisor  */
	gensecondary (op);	/* A = (-dividend) op divisor */
	gensecondary (I_NOT);	/* sign change result */
	genprimary (I_LDC, 1);
	gensecondary (I_SUM);
	genprimary (I_LDC, 0);	/* Force following cj to jump */
	genprimary (I_CJ, 9);	/* cj to exit -- mustn't deschedule! */
	/* positive: */
	gensecondary (I_POP);	/* lose the 0 */
	genprimary (I_LDC, 1);
	gensecondary (I_SUM);	/* A = dividend */
	gensecondary (I_REV);	/* swap dividend and divisor; */
	gensecondary (op);	/* A = dividend op divisor */
	genprimary (I_LDC, 0);	/* match the cj stack! */
	gensecondary (I_POP);	/* lose the 0 */
}

/*}}}*/
/*{{{  PUBLIC void tdop (op, type, left, right, regs, commutative)*/
/*****************************************************************************
 *
 *  tdop generates code for the dyadic operator 'op' upon the operands
 *       'left' and 'right' of type 'type'.
 *       'commutative' is TRUE if 'op' is commutative.
 *
 *****************************************************************************/

/*{{{  comment on code sequences*/
/* The code sequences we consider are:
a  left; right; op
   The best sequence: if left uses MAXREGS or less, and right uses
   MAXREGS - 1 or less.
b  right; left; (rev; ) op
   As good as a for commutative operators, otherwise we have to introduce
   a 'rev' instruction.
c  right; stl temp; left; ldl temp; op
   If right uses more temporaries than left.
d  left; stl temp; right; ldl temp; (rev; ) op
   As good as c if op is commutative.  Generally we choose between
   c and d to minimise temporary usage.

                  regsfor(right)
                    >3   3  2  1
                   =l >l
              ------------------
        >3 =r |    c  d  !  !  !       ! = impossible
regsfor    >r |    c  !  d  a  a
(left)   3    |    !  c  c  a  a       MAXREGS = 3
         2    |    !  b  b  a  a
         1    |    !  b  b  a  a
*/
/*}}}*/
PUBLIC void tdop (const int op, const int type, treenode * left, treenode * right, const int regs, const BOOL commutative, const BOOL signextend_result)
{
	const int r = (regs == MANY_REGS) ? MAXREGS : regs;
	const BOOL signed_short = use_shortintops && isshortint (type);	/* bug 1345 23/7/91 */
#ifdef OCCAM2_5
	const BOOL byte_short = (type == S_BYTE);
#else
	const BOOL byte_short = FALSE;
#endif
	BOOL sign_check = FALSE;
	BOOL logical_op = FALSE;

	/*{{{  look for some optimisations */
	switch (op) {
	default:
		break;
		/*{{{  ADD */
	case S_ADD:
		/*{{{  see if one side is constant */
		{
			if (isconst (right)) {
				treenode *temp = right;
				right = left;
				left = temp;
			}
			if ((nodetypeoftag (TagOf (left)) == CONSTEXPNODE) && isconst (left) && !isinconstanttable (left)) {
				/*{{{  we can optimise */
				texp (right, regs);
				genprimary (I_ADC, LoValOf (left));
				toverflowcheck (signed_short, byte_short);	/* check still in range */
				return;
				/*}}} */
			}
		}
		/*}}} */
		break;
		/*}}} */
		/*{{{  PLUS */
	case S_PLUS:		/* bug TS/1764 31/07/92 */
		/*{{{  see if one side is constant */
		{
			if (isconst (right)) {
				treenode *temp = right;
				right = left;
				left = temp;
			}
			if (isconst (left) && !isinconstanttable (left)) {
				const INT32 v = LoValOf (left);
				if (kroc_flag && v != 0)
					break;	/* inappropriate for non-transputer target -- DCW */
				if ((v & ((INT32) bytesperword - 1)) == 0)
					/*{{{  we can optimise */
				{
					texp (right, regs);
					genprimary (I_LDNLP, v / bytesperword);
					toverflowmask (signed_short, byte_short, signextend_result);	/* re-signextend */
					return;
				}
				/*}}} */
			}
		}
		/*}}} */
		break;
		/*}}} */
		/*{{{  MULT */
	case S_MULT:
		if (isconst (left)) {
			treenode *temp = left;
			left = right;
			right = temp;
		}
		if (has_dup && !T9000_instruction_timings && isconst (right) && (LoValOf (right) == 2)) {	/* bug 1168 22/8/91 */
			texp (left, regs);
			gensecondary (I_DUP);
			gensecondary (I_ADD);
			toverflowcheck (signed_short, byte_short);	/* check still in range */
			return;
		}
		break;
		/*}}} */
		/*{{{  SUBTRACT */
	case S_SUBTRACT:
		if (isconst (right) && !isinconstanttable (right) && !ismint (LoValOf (right)))
			/*{{{  we can optimise */
		{
			texp (left, regs);
			genprimary (I_ADC, ((BIT32) 0 - (BIT32) LoValOf (right)));
			toverflowcheck (signed_short, byte_short);	/* check still in range */
			return;
		}
		/*}}} */
		break;
		/*}}} */
		/*{{{  MINUS */
	case S_MINUS:		/* bug TS/1764 31/07/92 */
		if (isconst (right) && !isinconstanttable (right) && !ismint (LoValOf (right))) {
			const INT32 v = LoValOf (right);
			if (kroc_flag && v != 0)
				break;	/* inappropriate for non-transputer target -- DCW */
			if ((v & ((INT32) bytesperword - 1)) == 0)
				/*{{{  we can optimise */
			{
				texp (left, regs);
				genprimary (I_LDNLP, (BIT32) 0 - (BIT32) (v / bytesperword));
				toverflowmask (signed_short, byte_short, signextend_result);	/* re-signextend */
				return;
			}
			/*}}} */
		}
		break;
		/*}}} */
		/*{{{  TIMES */
	case S_TIMES:
		{
			treenode *lf = left;
			treenode *rt = right;
			int exponent = 0;
			if (isconstexpnd (rt)) {
				lf = right;
				rt = left;
			}
			if (isconstexpnd (lf))
				exponent = whichpowerof2 (LoValOf (lf));
			if (exponent > 0) {
				/* genfastshift(rt, regs, exponent); */
				texp_main (rt, regs, FALSE);	/* don't bother sign-extending */
				genshiftimmediate (I_SHL, exponent);	/* MDP */
				toverflowmask (signed_short, byte_short, signextend_result);	/* re-signextend */
				return;
			}
		}
		break;
		/*}}} */
		/*{{{  LSHIFT,RSHIFT */
	case S_LSHIFT:		/* added for bug 1168 14/3/91 by CON */
	case S_RSHIFT:
		if (isconstexpnd (right)) {
			/* genfastshift(left, regs, LoValOf(right)); */
			texp_main (left, regs, FALSE);	/* don't bother sign-extending */
			genshiftimmediate (op == S_LSHIFT ? I_SHL : I_SHR, LoValOf (right));	/* MDP */
			toverflowmask (signed_short, byte_short, signextend_result);	/* re-signextend */
			return;
		}
		break;
		/*}}} */
		/*{{{  CSUB0/CCNT1 */
	case S_CSUB0:
	case S_CCNT1:
		if (disable_csub0) {
			texp (left, regs);
			return;
		}
		if (use_shortintops && isconstexpnd (right)) {	/* T9000 shorts 24/7/91 */
			if (LoValOf (right) == 0x100) {
				texp (left, regs);
				/*{{{  T9000_gamma_carryin */
				if (T9000_gamma_carryin (&tx_global)) {
					gensecondary (I_CSU);
					gencomment0 ("Allow for ALU CarryIn bug");
				}
				/*}}} */
				gensecondary (I_CBU);
				return;
			}
			if (LoValOf (right) == 0x10000) {
				texp (left, regs);
				gensecondary (I_CSU);
				return;
			}
		}
		break;
		/*}}} */
		/*{{{  DIV */
	case S_DIV:		/* INSdi02165 */
		if (isconstexpnd (right) && (LoValOf (right) == -1)) {
			/*{{{  we can optimise into -x */
			tdop (S_SUBTRACT, type, newconstant (0), left, regs, FALSE, signextend_result);
			return;
			/*}}} */
		}
		break;
		/*}}} */
		/*{{{  REM */
	case S_REM:		/* INSdi02165 */
		if (isconstexpnd (right) && (LoValOf (right) == 1 || LoValOf (right) == -1)) {
			if (!cancauseerror (left)) {
				genprimary (I_LDC, 0);
			} else {
				texp (left, regs);
				genprimary (I_LDC, 0);
				if (regs != MANY_REGS)	{
					/* must pop off the lhs */
					gensecondary (I_AND);
				}
			}
			return;
		}
		break;
		/*}}} */
	}
	/*}}} */

	/*{{{  check for sign extension properties of short ints */
	if (signed_short || byte_short)	{
		/* don't bother for any others! */
		switch (op) {
		case S_ADD:
		case S_SUBTRACT:
		case S_MULT:
		case S_DIV:
		case S_REM:
			sign_check = TRUE;
			break;
		case S_LSHIFT:
		case S_PLUS:
		case S_MINUS:
		case S_TIMES:
			logical_op = TRUE;	/* means sign extension of sub-ops not necessary */
			break;
		case S_BITAND:
		case S_BITOR:
		case S_XOR:
		case S_RSHIFT:
			if (signed_short)
				logical_op = TRUE;	/* means sign extension of sub-ops not necessary */
			/* no masking needed for BYTEs */
			break;
		default:
			break;
		}
	}
	/*}}} */

	if (TagOf (right) == T_TEMP && !needtemptoload (P_EXP, NDeclOf (right))) {
		/*{{{  right; stl temp; left; ldl temp; */
		simplify (P_EXP, right);
		texp_main (left, regs, !logical_op);
		loadname (right, 0);
		/*}}} */
	} else if (TagOf (left) == T_TEMP && !needtemptoload (P_EXP, NDeclOf (left))) {
		/*{{{  left; stl temp; right; ldl temp; (rev; ) */
		simplify (P_EXP, left);
		texp_main (right, regs, !logical_op);
		loadname (left, 0);
		if (!commutative)
			gensecondary (I_REV);
		/*}}} */
	} else {
		/*{{{  don't need to introduce temporaries here */
		const int regsforleft = regsfor (left);
		const int regsforright = regsfor (right);
		BOOL leftfirst = TRUE;

		/*{{{  set leftfirst TRUE if we generate lhs first, FALSE otherwise */
		if (max_INT32 (regsforleft, regsforright) > MAXREGS) {
			leftfirst = (regsforleft >= regsforright);
		} else if (regsforleft == r) {
			leftfirst = TRUE;
		} else if (regsforright == r) {
			leftfirst = FALSE;
		} else {
			/*{{{  either 'left; right; op'  or  'right; left; (rev; ) op' */
			const int revbase = commutative ? 0 : 1;
			const int revs_for_left_first = revsfor (left, r) + revsfor (right, r - 1);
			const int revs_for_right_first = revsfor (right, r) + revsfor (left, r - 1) + revbase;

			leftfirst = (revs_for_left_first <= revs_for_right_first);

			/* bug TS/2004 15/12/92 */
			/* if all else is equal, and the op is commutative, then
			   attempt better grouping for the T9000.
			   This code is also correct and efficient for T-series processors
			   so do it in all cases.
			   What we're doing here is looking at code of the form
			   x := a + (b + c), and ensuring that instead of
			   ldl a; ldl b; ldl c; add; add; stl x,
			   which groups as: [ldl ldl]; [ldl add]; [add stl]
			   we generate
			   ldl b; ldl c; add; ldl a; add; stl x,
			   which groups as: [ldl ldl add]; [ldl add stl]
			 */
			if ((revs_for_left_first == revs_for_right_first) && commutative) {
				const BOOL left_addressable = isaddressable (left);
				const BOOL right_addressable = isaddressable (right);
				if (left_addressable != right_addressable) {
					leftfirst = right_addressable;
				}
			}
			/*}}} */
		}
		/*}}} */

		if (leftfirst) {
			/*{{{  left; right; */
			texp_main (left, regs, !logical_op);
			texp_main (right, r - 1, !logical_op);
			/*}}} */
		} else {
			/*{{{  right; left; (rev; ) */
			texp_main (right, regs, !logical_op);
			texp_main (left, r - 1, !logical_op);
			if (!commutative) {
				gensecondary (I_REV);
			}
			/*}}} */
		}
	}
	/*}}} */

	switch (op) {
		/*{{{  arithmetic operators */
	case S_ADD:
		gensecondary (I_ADD);
		break;
	case S_SUBTRACT:
		gensecondary (I_SUB);
		break;
		/*{{{  multiply */
	case S_MULT:
		/*{{{  T9000_gamma_badmul */
		if (T9000_gamma_badmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Workaround a slow multiplier");
		}
		/*}}} */
		gensecondary (NEED_ERRORS ? I_MUL : I_PROD);
		break;
		/*}}} */
	case S_DIV:
		if (t450a_workarounds)
			t450_divrem_workaround (I_DIV);
		else
			gensecondary (I_DIV);
		break;
	case S_REM:
		if (t450a_workarounds)
			t450_divrem_workaround (I_REM);
		else
			gensecondary (I_REM);
		break;
		/*}}} */
		/*{{{  logical operators */
	case S_BITAND:
		gensecondary (I_AND);
		break;
	case S_BITOR:
		gensecondary (I_OR);
		break;
	case S_XOR:
		gensecondary (I_XOR);
		break;
		/*}}} */
		/*{{{  shifts */
	case S_LSHIFT:
		if (!T9000_instruction_timings && !isconst (right))
			checkerror ();	/* bug TS/1979 30/11/92 */
		gensecondary (I_SHL);
		break;
	case S_RSHIFT:
		if (!T9000_instruction_timings && !isconst (right))
			checkerror ();	/* bug TS/1979 30/11/92 */
		gensecondary (I_SHR);
		break;
		/*}}} */
		/*{{{  modulo arithmetic */
	case S_PLUS:
		if (kroc_flag)
			gensecondary (I_SUM);	/* keep it clean -- DCW */
		else
			/*gensecondary (I_SUM); */ gensecondary (I_BSUB);
		break;
	case S_MINUS:
		gensecondary (I_DIFF);
		break;
		/*{{{  prod */
	case S_TIMES:
		/*{{{  T9000_gamma_badmul */
		if (T9000_gamma_badmul (&tx_global)) {
			gensecondary (I_NOP);
			gencomment0 ("Workaround a slow multiplier");
		}
		/*}}} */
		gensecondary (I_PROD);
		break;
		/*}}} */
		/*}}} */
		/*{{{  subscript check */
	case S_CSUB0:
		gensecondary (I_CSUB0);
		break;
	case S_CCNT1:
		gensecondary (I_CCNT1);
		break;
		/*}}} */
		/*{{{  comparison */
	case S_GR:
		gensecondary (I_GT);
		if (T9000_alpha_badgt (&tx_global)) {
			genprimary (I_EQC, 1);
			gencomment0 ("Silicon bug fix");
		}
		break;
		/*}}} */
	default:
		badtag (genlocn, op, "tdop");
	}

	if (sign_check)
		toverflowcheck (signed_short, byte_short);	/* check still in range */
	else if (logical_op)
		toverflowmask (signed_short, byte_short, signextend_result);	/* re-signextend */
}

/*}}}*/
/*{{{  PUBLIC void ttypeconversion(sourcetype, desttype)*/
PUBLIC void ttypeconversion (const int sourcetype, const int desttype)
{
	const BOOL shrinking = hasgreaterrange (sourcetype, desttype);

	if (shrinking && !use_shortintops && isshortint (sourcetype))
		/*{{{  mask off the upper bytes */
	{
		/* bug 1367 14/8/91 */
		/* we have to mask off the rubbish upper bytes.
		   We needn't bother actually sign extending, because
		   either we are checking, in which case the check will see negative
		   shorts as large unsigned numbers,
		   or if we aren't checking, we assume that the number is a small
		   positive number anyway, and just mask by the smallest mask we can
		   get away with!
		 */
		/* If we are using shortintops, we keep all shorts on the stack as
		   sign-extended anyway.
		 */
		loadconstant (CONVERSIONCHECKING ? typemask (sourcetype) : (checkmask (desttype) - 1));
		gensecondary (I_AND);
	}
	/*}}} */

	if (CONVERSIONCHECKING && shrinking) {
		/*{{{  we have to check the range */
		/* we know that sourcetype is <= S_INT32, and desttype <= S_INT32(?) */
		/* T9000_alpha_noxsword bug; I_CBU and I_CS are OK */
		if (has_shortintops && (desttype == S_BYTE)) {
			/*{{{  T9000_gamma_carryin */
			if (T9000_gamma_carryin (&tx_global)) {
				gensecondary (I_CSU);
				gencomment0 ("Allow for ALU CarryIn bug");
			}
			/*}}} */
			gensecondary (I_CBU);
		} else if (has_shortintops && (desttype == S_INT16)) {
			if (!T9000_gamma_carryin (&tx_global)) {
				gensecondary (I_CS);
			}
		} else if (issignedtype (desttype)) {
			/*{{{  use cword */
			loadconstant (checkmask (desttype));
			gensecondary (I_CWORD);
			/*}}} */
		} else {
			/*{{{  use csub0 */
			loadconstant (checkmask (desttype));
			gensecondary (I_CSUB0);
			/*}}} */
		}
		/*}}} */
	} else if (hasgreaterrange (desttype, sourcetype) && issignedtype (sourcetype)) {
		/*{{{  extend to full word */
		/* desttype > sourcetype and sourcetype is signed, means that
		   sourcetype = S_INT16, or sourcetype = S_INT32.
		   In practice, only S_INT16 ever gets here! */
		if (use_shortintops && sourcetype == S_INT16) {
			/* we have already loaded the value as sign extended */
			/* gensecondary(I_XSWORD); */
		} else {
			loadconstant (typemask (sourcetype));
			gensecondary (I_AND);
			if (sourcetype == S_INT16 && T9000_alpha_noxsword (&tx_global))	/* OK if masked first */
				gensecondary (I_XSWORD);
			else {
				assert (sourcetype == S_INT16);
				genwidenshort ();
			}
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void texp_constanttable*/
PUBLIC void texp_constanttable (treenode * const tptr, const int inonlocal, const int word)
{
	const int type = ntypeof (tptr);
	int adjust = 0;
	
	tloadconsttable ();
	genprimary (inonlocal, CEOffsetOf (tptr) + word);

	if (inonlocal == I_LDNLP) {
		switch (type) {
			case S_BYTE:
			case S_BOOL:
			case S_INT16:
				if (target_bigendian) {
					adjust = bytesperword - bytesinscalar (type);
				}
				break;
		}
		if (adjust) {
			gencomment0 (".MAGIC UNCHECKED");
			genprimary (I_ADC, adjust);
		}
	}
}

/*}}}*/
/*{{{  PUBLIC void texp_main (treenode *tptr, const int regs, const BOOL signextend_result)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  texp generates code for the expression 'tptr'.  The type of tptr must be
 *       targetintsize or smaller.
 *       'regs' is the maximum number of registers available for the
 *       expression, it may take the special value MANY_REGS which means
 *       we can use all the registers, and must look out for temporaries, too.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void texp_main (treenode *tptr, const int regs, const BOOL signextend_result)
{
	BETRACE ("gen04: texp_main (enter): tptr=%p, regs=%d, signextend_result=%d", tptr, regs, (int)signextend_result);
#if 0
fprintf (stderr, "texp_main: regs = %d, signextend = %d, tptr =", regs, (int)signextend_result);
printtreenl (stderr, 4, tptr);
#endif
	/*{{{  write out the expression */
	if (diagnostics)
		commentexp (tptr);
	/*}}} */
	switch (TagOf (tptr)) {
		/*{{{  dyadic operator */
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
	case S_LSHIFT:
	case S_RSHIFT:
	case S_CSUB0:
	case S_CCNT1:
		tdop (TagOf (tptr), DOpTypeOf (tptr), LeftOpOf (tptr), RightOpOf (tptr), regs, commutes (tptr), signextend_result);
		break;
	case S_EVAL:
		texp_main (RightOpOf (tptr), regs, signextend_result);
		break;
		/*}}} */
		/*{{{  monadic operators and conversions */
#ifdef MOBILES
		/*{{{  NTH_DIMENSION */
	case S_NTH_DIMENSION:
		if (LoValOf (RightOpOf (tptr)) > 0) {
			loaddynmobilesize (LeftOpOf (tptr), LoValOf (RightOpOf (tptr)));
		} else {
			loadmobile_real (LeftOpOf (tptr));
		}
		break;
		/*}}}  */
		/*{{{  ADDROF HWADDROF */
	case S_ADDROF:
	case S_HWADDROF:
		genaddrof (tptr);
		break;
		/*}}}*/
		/*{{{  ADDROF */
	case S_CLONE:
		texp_main (OpOf (tptr), regs, signextend_result);
		break;
		/*}}}  */
		/*{{{  DEFINED*/
	case S_DEFINED:
		loadmobile_real (OpOf (tptr));		/* automatically the correct BOOL result */
		break;
		/*}}}*/
#endif
		/*{{{  NEG                                break */
	case S_NEG:
		/*{{{  simple inline code */
		{
			/* We have two options here :
			   1  e; not; adc 1    uses one register, but is 1 byte and 1 cycle longer
			   2  ldc 0; e; sub    uses two registers, but is shorter and faster and is
			   to be preferred if we have the registers available.
			 */
			treenode *e = OpOf (tptr);
			int r = (regs == MANY_REGS) ? MAXREGS : regs;
			if (regsfor (e) >= r) {
				/*{{{  need all registers, so use option 1 */
				texp (e, regs);
				gensecondary (I_NOT);
				genprimary (I_ADC, 1);
				/*}}} */
			} else {
				/*{{{  see if option 2 uses more rev operations */
				int revs_for_not = revsfor (e, MAXREGS), revs_for_sub = revsfor (e, MAXREGS - 1);

				if (revs_for_sub == revs_for_not) {
					/*{{{  we have the registers for    ldc 0; e; sub    so generate it */
					genprimary (I_LDC, 0);
					texp (e, r - 1);
					gensecondary (I_SUB);
					/*}}} */
				} else {
					/*{{{  option 2 uses more 'rev' operations, so generate option 1 */
					texp (e, regs);
					gensecondary (I_NOT);
					genprimary (I_ADC, 1);
					/*}}} */
				}
				/*}}} */
			}
			if (NEED_ERRORS && use_shortintops && isshortint (MOpTypeOf (tptr))	/* bug 1345 23/7/91 */
			    &&!T9000_gamma_carryin (&tx_global)) {
				gensecondary (I_CS);
			}
		}
		/*}}} */
		break;
		/*}}} */
		/*{{{  unary MINUS                        break */
	case S_UMINUS:
		tdop (S_MINUS, MOpTypeOf (tptr), newconstant (0), OpOf (tptr), regs, FALSE, signextend_result);
		break;
		/*}}} */
		/*{{{  BITNOT                             break */
	case S_BITNOT:
		texp_main (OpOf (tptr), regs, signextend_result);
		gensecondary (I_NOT);	/* automatically preserves sign-extendedness */
#ifdef OCCAM2_5
		if (MOpTypeOf (tptr) == S_BYTE) {
			toverflowmask (FALSE, TRUE, signextend_result);
		}
#endif
		break;
		/*}}} */
		/*{{{  SIZE ELSIZE SEGSTART               break */
	case S_SIZE:
#ifdef MOBILES
		if (isdynmobilearray (OpOf (tptr))) {
			/* load the count */
			loaddynmobilesize (OpOf (tptr), 1);
		} else {
			/* might have to look harder for multi-dimensional arrays */
			treenode *sop = OpOf (tptr);
			int dimension = 1;

			while (TagOf (sop) == S_ARRAYITEM) {
				sop = ASBaseOf (sop);
				dimension++;
			}

			if (isdynmobilearray (sop)) {
				/* need to wander the dimension tree again (evaluating the checks) */
				sop = OpOf (tptr);

				while (TagOf (sop) == S_ARRAYITEM) {
					/* perform the check, if it exists */
#if 0
fprintf (stderr, "texp: S_SIZE: wandering a dimension tree.  ASExpOf(sop) =");
if (ASExpOf (sop)) printtreenl (stderr, 4, ASExpOf (sop));
else fprintf (stderr, "\n    [NULL]\n");
#endif
					texp_main (ASExpOf (sop), regs, FALSE);
					/* throw_the_result_away (); */
					gensecondary (I_POP);
					sop = ASBaseOf (sop);
				}
				loaddynmobilesize (sop, dimension);
			} else {
				tdimension (OpOf (tptr), regs);
			}
		}
#else
		tdimension (OpOf (tptr), regs);
#endif
		break;
	case S_ELSIZE:
#ifdef MOBILES
		if (isdynmobilearray (OpOf (tptr))) {
			/* load the count */
			loaddynmobilesize (OpOf (tptr), 1);
		} else {
			tptr = OpOf (tptr);
			if ((TagOf (tptr) == S_SEGMENT) || (TagOf (tptr) == S_SEGMENTITEM)) {
				/* Evaluate a segment length into a temporary */
				simplify (P_EXP, SLengthExpOf (tptr));
			}
			tdimension (tptr, regs);
		}
#else
		tptr = OpOf (tptr);
		if ((TagOf (tptr) == S_SEGMENT) || (TagOf (tptr) == S_SEGMENTITEM)) {
			/* Evaluate a segment length into a temporary */
			simplify (P_EXP, SLengthExpOf (tptr));
		}
		tdimension (tptr, regs);
#endif
		break;
	case S_SEGSTART:
		tptr = OpOf (tptr);
		simplify (P_EXP, SStartExpOf (tptr));
		texp (SStartExpOf (tptr), regs);
		break;
		/*}}} */
		/*{{{  EXACT                              break */
	case S_EXACT:
		{
			int sourcetype = ntypeof (OpOf (tptr)), desttype = MOpTypeOf (tptr);
			treenode *source = OpOf (tptr);

			/*{{{  COMMENT check for evaluating source to a temporary */
	  /**********************  Start comment out ****************************
          @*{{{  check for evaluating source to a temporary*@
          if (has_fpu_core && preeval(P_EXP, source) &&
              needtemptoload(P_EXP, NDeclOf(source)))
            @* If source is a real to int conversion on an fp processor, then it will
               have to be evaluated to a temporary *@
            simplify(P_EXP, source);
          @*}}}*@
           **********************   End comment out  ****************************/
			/*}}} */

			if (isdoublelength (sourcetype))
				/*{{{  load and check it fits in a single word, change sourcetype to INT */
			{
				int sourcemode = P_EXP;
				if (CONVERSIONCHECKING || !isaddressable (source))
					sourcemode = addresslopd (sourcemode, source);
				if (CONVERSIONCHECKING) {
					loadopd (sourcemode, source, 1);
					loadopd (sourcemode, source, 0);
					gensecondary (I_CSNGL);
				} else
					loadopd (sourcemode, source, 0);
				sourcetype = S_INT;
			}
			/*}}} */
			else
				texp (source, regs);
			ttypeconversion (sourcetype, desttype);
		}
		break;
		/* One might reasonably ask,"What happened to ROUND and TRUNC?"
		   Well the answer is this: if we are on a T4, they will have been turned
		   into function calls; if we are on a T8, we must be doing a real to
		   integer conversion to expect one here,
		   so the source is evaluated on the fpu, then stored
		   to a temporary. We look for those round/truncs in routine
		   "assign", not here, which enables us to generate better code. */
		/*}}} */
		/*{{{  ADDRESSOF                          break */
	case S_ADDRESSOF:
		loadelementpointer (OpOf (tptr), 0, regs);
		break;
		/*}}} */
		/*}}} */
		/*{{{  Boolean operator */
	case S_NOT:
	case S_AND:
	case S_OR:
	case S_EQ:
	case S_NE:
	case S_LS:
	case S_LE:
	case S_GR:
	case S_GE:
		tbool (tptr, regs);	/* tbool calls texp recursively if needed */
		break;
		/*}}} */
		/*{{{  element */
	case N_DECL:
	case N_REPL:
	case N_ABBR:
	case N_VALABBR:
	case N_PARAM:
	case N_VALPARAM:
	case N_RESULTPARAM:
	case N_RETYPE:
	case N_VALRETYPE:
	case S_ARRAYITEM:
	case S_RECORDITEM:
	case S_SEGMENTITEM:	/* When we're retyping a segment to fit in a word */
	case T_PREEVALTEMP:	/* A pre-evaluated temporary */
	case S_STRING:
	case S_CONSTCONSTRUCTOR:
	case S_FNFORMALRESULT:	/* INSdi03203 */
		loadelement (tptr, 0, regs, signextend_result);
		break;
	case T_TEMP:
		if (needtemptoload (P_EXP, NDeclOf (tptr))) {
			simplify (P_EXP, tptr);
			loadname (tptr, 0);
		} else {
			loadelement (tptr, 0, regs, FALSE);	/* temps were sign extended when created */
		}
		break;
		/*}}} */
		/*{{{  function instance */
	case S_FINSTANCE:
		if (TagOf (INameOf (tptr)) == N_PREDEFFUNCTION) {
			tpredef (tptr, NULL);
		} else {
			const BOOL recursive = (nodetypeoftag (TagOf (tptr)) == INSTANCENODE) ? IRecursiveOf (tptr) : FALSE;
			const int nregresults = 1;
			const int temp = RECURSIVE_WS;

			tinstance (tptr);

			if (recursive) {
				/* A=ws ptr */
				genprimary (I_STL, temp);	/* => empty stack */
			}

			/* If assembly_output && 
			   ((alloc_strategy & ALLOC_NOCOMMENTS) == 0 */
			gencomment1 ("<%d reg results>", nregresults);
			gen_func_results (nregresults);

			/* clean-up recursive function ? */
			if (recursive) {
				treenode *const iname = INameOf (tptr);
				INT32 proc_ws, proc_vs;
				
				getprocwsandvs (iname, &proc_ws, &proc_vs);

				/* A=result */
				genprimary (I_LDL, temp);	/* => A=ws ptr, B=result */
				if (proc_vs) {
					gensecondary (I_REV);	/* => A=ws ptr, B=result */
					gensecondary (I_DUP);	/* => A=ws ptr, B=ws ptr, C=result */
					genprimary (I_STL, temp);/* => A=ws ptr, B=result */
					genprimary (I_STNL, 1);	/* => empty stack */
					genprimary (I_LDL, temp);/* => A=ws ptr */
					genprimary (I_LDNL, 0);	/* => A=vs ptr */
					gensecondary (I_MRELEASE); /* => empty stack */
					genprimary (I_LDL, temp);/* => A=ws ptr */
					gensecondary (I_DUP);	/* => A=ws ptr, B=ws ptr */
					genprimary (I_LDNL, 1);	/* => A=result, B=ws ptr */
				} else {
					gensecondary (I_REV);   /* => A=result, B=ws ptr */
				}
				genprimary (I_STL, temp); 	/* => A=ws ptr */
				gensecondary (I_MRELEASE);	/* => empty stack */
				genprimary (I_LDL, temp);	/* => A=result */
			}
		}
		break;
		/*}}} */
		/*{{{  specification ... valof */
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
		tvalof (tptr, NULL);
		break;
		/*}}} */
		/*{{{  special parameter values */
	case S_PARAM_STATICLINK:
		loadstaticlink (instancedlevel);	/* Uses one register */
		break;
	case S_PARAM_VSP:
		loadnewvsp (HDimensionOf (tptr));	/* Uses one register */
		break;
	case S_PARAM_FB:
		loadfb ();
		break;
#ifdef MOBILES
	case S_PARAM_MSP:
		loadnewmsp (HDimensionOf (tptr));	/* Uses one register */
		break;
	case S_HIDDEN_TYPE:
		loadhiddentypeof (HExpOf (tptr), regs);
		break;
#endif /* MOBILES */
	case S_TYPEHASHOF:
		{
			treenode *op = OpOf (tptr);

#if 0
fprintf (stderr, "texp_main(): S_TYPEHASHOF: OpOf (tptr) = ");
printtreenl (stderr, 4, op);
#endif
			switch (TagOf (op)) {
			case N_PROCDEF:
			case N_LIBPROCDEF:
			case N_STDLIBPROCDEF:
			case N_SCPROCDEF:
			case N_INLINEPROCDEF:
				/* loading typehash of a PROC, this means the paramter list normally */
				{
					treenode *param_list = NParamListOf (op);
					treenode **savep_vsp = NULL;
					treenode *save_vsp = NULL;
					unsigned int thash;
					
					/* if the parameter list has a vectorspace pointer in it, remove for purposes of typehash generation */
					for (savep_vsp = &param_list; savep_vsp && !EndOfList (*savep_vsp); savep_vsp = NextItemAddr (*savep_vsp)) {
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
						*savep_vsp = NULL;
					}
					thash = typehash (param_list);
					if (savep_vsp) {
						*savep_vsp = save_vsp;
					}

					loadconstant (thash);
				}
				break;
			default:
#ifdef MOBILES
				loadhiddentypeof (op, regs);
#else
				loadconstant (typehash (op));
#endif
				break;
			}
		}
		break;
	case S_HIDDEN_PARAM:
		texp_main (HExpOf (tptr), regs, signextend_result);
		break;
	case S_FNACTUALRESULT:
#if 0
		fprintf (outfile, "\ntexp_main: actual result: HExpOf is ");
		printtree (0, HExpOf (tptr));
		fputc ('\n', outfile);
#endif
		/* loadelementpointer (HExpOf (tptr), 0, regs); */
		loadelementpointer (tptr, 0, regs);
		gencomment0 ("actualresult");
		break;
		/*}}} */
		/*{{{  constant */
	case S_CONSTEXP:
		if (isinconstanttable (tptr)) {
			texp_constanttable (tptr, I_LDNL, 0);
		} else {
			loadconstant (LoValOf (tptr));
		}
		break;
		/*}}} */
		/*{{{  UDV constant*/
	case S_UDVCONSTEXP:
#if 0
fprintf (stderr, "texp_main(): S_UDVCONSTEXP: LoValOf (tptr) = %d.  CExpOf =", (int)(LoValOf (tptr)));
printtreenl (stderr, 4, CExpOf (tptr));
#endif
		if ((int)(LoValOf (tptr)) < 0) {
			texp_main (CExpOf (tptr), regs, signextend_result);
		} else {
			loadconstant (LoValOf (tptr));
		}
		break;
		/*}}}*/
		/*{{{  S_NULLARRAY*/
	case S_NULLARRAY:
		gensecondary (I_NULL);
		/*}}}*/
		/*{{{  special ASM name - NOT converted */
	case S_ASMNAME:
		generr_s (GEN_BAD_ASMNAME, WNameOf ((wordnode *) tptr));
		/*}}} */
	default:
		badtag (genlocn, TagOf (tptr), "texp_main");
	}
	BETRACE ("gen04: texp_main (leave)");
}

/*}}}*/
/*{{{  PUBLIC void texpopd_main (opdmode, opd, regs)         ***/
/*****************************************************************************
 *
 *  texpopd generates code to evaluate the operand (opdmode, opd), using
 *          at most 'regs' registers.
 *
 *****************************************************************************/
PUBLIC void texpopd_main (const int opdmode, treenode * const opd, const int regs, const BOOL signextend_result)
{
	BETRACE ("gen04: texpopd_main (enter): opdmode=%d, opd=%p, regs=%d, signextend_result=%d", opdmode, opd, regs, (int)signextend_result);
	switch (opdmode) {
	case P_TEMP:
		loadname (opd, 0);
		/*gencomment1 ("$temp%d", (BIT32)NVVarNumOf(opd)); */
		break;
	case P_TEMPPTR:
		loadnamepointer (opd, 0);
		/*gencomment1 ("$temp%d", (BIT32)NVVarNumOf(opd)); */
		break;
	case P_EXP:
		texp_main (opd, regs, signextend_result);
		break;
	case P_PTR:
		loadelementpointer (opd, 0, regs);
		break;
	default:
		geninternal_is (GEN_BAD_OPD, opdmode, "texpopd_main");
	}
	BETRACE ("gen04: texpopd_main (leave)");
}

/*}}}*/
/*}}}*/
/*{{{  loading registers*/
/*{{{  PUBLIC void tload2regs(e1mode, e1, e2mode, e2, commutative)*/
/*****************************************************************************
 *
 *  tload2regs generates code for loading (e1mode, e1) and (e2mode, e2)
 *             into Areg and Breg.
 *             If commutative is FALSE, e1 will be in Breg and e2 will be
 *             in Areg; otherwise they may be reversed.
 *
 *****************************************************************************/
PUBLIC void tload2regs (int e1mode, treenode * e1, int e2mode, treenode * e2, const BOOL commutative, const BOOL signextend_result)
{
#if 0
fprintf (stderr, "tload2regs (e1mode = %d), e1 =", e1mode);
printtreenl (stderr, 8, e1);
fprintf (stderr, "    (e2mode = %d), e2 =", e2mode);
printtreenl (stderr, 8, e2);
#endif
	if (preeval (e2mode, e2)) {
		/*{{{  e2; stl temp; e1; ldl temp */
		e2mode = simplify (e2mode, e2);
		texpopd_main (e1mode, e1, MANY_REGS, signextend_result);
		loadopd (e2mode, e2, 0);
		/*}}} */
	} else if (preeval (e1mode, e1)) {
		/*{{{  temp := e1; e2; ldl temp; (rev) | temp := e1; ldl temp; e2 */
		e1mode = simplify (e1mode, e1);
		if (regsforopd (e2mode, e2) < MAXREGS) {
			loadopd (e1mode, e1, 0);
			texpopd_main (e2mode, e2, MAXREGS - 1, signextend_result);
		} else {
			texpopd_main (e2mode, e2, MAXREGS - 1, signextend_result);
			loadopd (e1mode, e1, 0);
			if (!commutative)
				gensecondary (I_REV);
		}
		/*}}} */
	} else {
		/*{{{  count registers to decide which to evaluate first */
		if (regsforopd (e2mode, e2) >= MAXREGS) {
			texpopd_main (e2mode, e2, MANY_REGS, signextend_result);
			texpopd_main (e1mode, e1, MAXREGS - 1, signextend_result);
			if (!commutative)
				gensecondary (I_REV);
		} else {
			texpopd_main (e1mode, e1, MANY_REGS, signextend_result);
			texpopd_main (e2mode, e2, MAXREGS - 1, signextend_result);
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void tload3regs(e1mode, e1, e2mode, e2, e3mode, e3, loadseq)*/
/*****************************************************************************
 *
 *  tload3regs generates code for loading (e1mode, e1) into Creg,
 *             (e2mode, e2) into Breg, (e3mode, e3) into Areg.
 *             loadseq defines the loading sequence for e1, e2, e3.
 *
 *****************************************************************************/
PUBLIC void tload3regs (int e1mode, treenode * e1, int e2mode, treenode * e2, int e3mode, treenode * e3, const int loadseq, const BOOL signextend_result)
{
	e2mode = simplify (e2mode, e2);
	e3mode = simplify (e3mode, e3);

	/*{{{  simplify e1 */
	{
		/* This is done after the other two so that it matches what happens
		   in mapload3regs. It is all very vague.... - CON 12/6/91
		 */
		/*const int oldmode = e1mode; */
		e1mode = simplify (e1mode, e1);	/* bug 1298 12/6/91 */
		/*if (e1mode != oldmode)
		   fprintf(outfile, "\ntload3regs: simplified Areg"); */
	}
	/*}}} */

	switch (loadseq)
		/*{{{  do the loading */
	{
	case 1:
		/*{{{  e1; e2; e3 */
		texpopd_main (e1mode, e1, MANY_REGS, signextend_result);
		texpopd_main (e2mode, e2, MAXREGS - 1, signextend_result);
		texpopd_main (e3mode, e3, MAXREGS - 2, signextend_result);
		break;
		/*}}} */
	case 2:
		/*{{{  e1; e3; e2; rev */
		texpopd_main (e1mode, e1, MANY_REGS, signextend_result);
		texpopd_main (e3mode, e3, MAXREGS - 1, signextend_result);
		texpopd_main (e2mode, e2, MAXREGS - 2, signextend_result);
		gensecondary (I_REV);
		break;
		/*}}} */
	case 3:
		/*{{{  e2; e1; rev; e3 */
		texpopd_main (e2mode, e2, MANY_REGS, signextend_result);
		texpopd_main (e1mode, e1, MAXREGS - 1, signextend_result);
		gensecondary (I_REV);
		texpopd_main (e3mode, e3, MAXREGS - 2, signextend_result);
		break;
		/*}}} */
	case 4:
		/*{{{  e3; e1; rev; e2; rev */
		texpopd_main (e3mode, e3, MANY_REGS, signextend_result);
		texpopd_main (e1mode, e1, MAXREGS - 1, signextend_result);
		gensecondary (I_REV);
		texpopd_main (e2mode, e2, MAXREGS - 2, signextend_result);
		gensecondary (I_REV);
		break;
		/*}}} */
	}
	/*}}} */
}

/*}}}*/
/*}}}*/
/*{{{  storing registers*/
/*{{{  PUBLIC void tstoreregs (dest, nregs)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  tstoreregs stores up to MAXREGS register values to their destinations.
 *             'nregs' is the number of registers to be stored.
 *             'dest' is an array containing pointers to the trees representing
 *             the destinations.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void tstoreregs (treenode ** dest[MAXREGS], const int nregs)
{
	int regsfordest[MAXREGS];
	int i;
	int ndone;
	int topreg = nregs - 1;
	int freeregs = MAXREGS - nregs;
	for (i = 0; i < nregs; i++) {
		regsfordest[i] = regsforstore (*dest[i]);
		DEBUG_MSG (("tstoreregs: regsfordest %d is %d\n", i, regsfordest[i]));
	}
	for (ndone = 0; ndone < nregs; ndone++)
		/*{{{  (store top register) or (rev; store top register) */
	{
		if (TagOf (*dest[topreg]) == T_TEMP) {
			DEBUG_MSG (("tstoreregs: storing in temporary\n"));
			storeinopd (P_TEMP, *dest[topreg], 0, freeregs);
		} else if (regsfordest[topreg] <= freeregs) {
			DEBUG_MSG (("tstoreregs: storing directly\n"));
			storeinopd (P_EXP, *dest[topreg], 0, freeregs);
		} else {	/* regsfordest[topreg - 1] <= freeregs */

			DEBUG_MSG (("tstoreregs: doing rev ; store\n"));
			gensecondary (I_REV);
			storeinopd (P_EXP, *dest[topreg - 1], 0, freeregs);
			dest[topreg - 1] = dest[topreg];
			regsfordest[topreg - 1] = regsfordest[topreg];
		}
		topreg--;
		freeregs++;
	}
	/*}}} */
	/*{{{  move temporaries to real destinations */
	for (i = 0; i < nregs; i++)
		if (TagOf (*dest[i]) == T_TEMP) {
			treenode *destexp = NDeclOf (*dest[i]);
			DEBUG_MSG (("tstoreregs: moving temp %d to dest\n", i));
			tsimpleassign (ntypeof (destexp), P_EXP, destexp, P_TEMP, *dest[i], MANY_REGS);
		}
	/*}}} */
}

/*}}}*/
/*}}}*/
