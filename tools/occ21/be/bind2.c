/* $Id: bind2.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	expression mapping
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

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include "includes.h"
#include "generror.h"
#include "genhdr.h"
#include "objlib.h"
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
/*}}}*/

/*{{{  support routines */
/*{{{  PUBLIC void placeintable(cptr) */
/*****************************************************************************
 *
 *  placeintable places the constant node 'cptr' in the list of entries for
 *               the constant table
 *
 *****************************************************************************/
PUBLIC void placeintable (treenode *cptr)
{
	SetCENext (cptr, consttablechain);
	/* Setting this field to >= 0 indicates that the constant is in a table,
	   a sensible offset will be patched in later. */
	SetCEOffset (cptr, 0);
	consttablechain = cptr;
	upusecount (constantnptr, 1);
}

/*}}}*/
/*{{{  PUBLIC void getprocwsandvs */
PUBLIC void getprocwsandvs (treenode *nptr, INT32 *ws, INT32 *vs)
{
	if (separatelycompiled (nptr))
		getlibwsandvs (nptr, genlocn, configuring, ws, vs);
	else {
		*ws = NPDatasizeOf (nptr);
		*vs = NPVSUsageOf (nptr);
	}
}
/*}}}*/
#ifdef MOBILES
/*{{{  PUBLIC void getprocwsandvsandms */
PUBLIC void getprocwsandvsandms (treenode *nptr, INT32 *ws, INT32 *vs, INT32 *ms)
{
	getprocwsandvs (nptr, ws, vs);
	if (separatelycompiled (nptr)) {
		getlibwsandvsandms (nptr, genlocn, configuring, ws, vs, ms);
	} else {
		*ms = NPMSUsageOf (nptr);
	}
	return;
}
/*}}}  */
#endif
/*}}}*/

/*{{{  PRIVATE treenode *mapsubscript (treenode *tptr, BOOL map_address) */
/*{{{  comment */
/*****************************************************************************
 *
 *  mapsubscript walks a subscript tree inserting any temporaries needed.
 *               It also sets the global staticlinkused to the minimum
 *               lexical level of any names in the expression.
 *               (A subscript tree has ARRAYITEM or SEGMENTITEM as its root
 *               tag.)
 *               'map_address' is TRUE if we are mapping the loading of an
 *               address, FALSE if we are loading a value.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE treenode *mapsubscript (treenode *tptr, const BOOL map_address)
{
	treenode *result = tptr;
	const int mappingsegment = (TagOf (tptr) == S_SEGMENTITEM);
	treenode *basetemp = NULL;


#if 0
fprintf (stderr, "bind2: mapsubscript: map_address = %d, tptr = ", map_address);
printtreenl (stderr, 4, tptr);
#endif
	/* mapnestedsegments(tptr); */
	/*{{{  map the subscript */
	{
		treenode **nptr = &tptr;
		BOOL descending = TRUE;
		/*{{{  look for a constructor, and map if found */
		while (descending) {
			switch (nodetypeoftag (TagOf (*nptr))) {
			case ARRAYSUBNODE:
				nptr = ASBaseAddr (*nptr);
				break;
			case SEGMENTNODE:
				nptr = SNameAddr (*nptr);
				break;
			default:
				descending = FALSE;
				break;
			}
		}
#ifdef OCCAM2_5
		if (TagOf (*nptr) == S_FINSTANCE) {
			/*{{{  store in a temporary */
			*nptr = gettemp (*nptr, NM_WORKSPACE);
			/* Now we load each element of the constructor and store it in the temp */
			mapsimpleassign (ntypeof (NDeclOf (*nptr)), P_TEMP, nptr, P_EXP, NDeclAddr (*nptr));
			basetemp = *nptr;
			/*}}} */
		}
#endif
		/*}}} */
		{
			treenode **subscriptexp = mappingsegment ? SSubscriptExpAddr (tptr) : ASExpAddr (tptr);
			if (*subscriptexp != NULL) {
				mapexp (subscriptexp);
			}
			mapaddr (mappingsegment ? SNameAddr (tptr) : ASBaseAddr (tptr));

#if 0				/* This is now fixed in trans */
			DEBUG_MSG (("mapsubscript: address?:%d, null/const exp?:%d, offset?:%ld\n",
				    map_address, (*subscriptexp == NULL) || isconst (*subscriptexp),
				    mappingsegment ? SOffsetOf (tptr) : ASOffsetOf (tptr)));
			/* if we're taking the address of a[0], that's the same as address of a */
			if (map_address &&
			    ((*subscriptexp == NULL) || isconst (*subscriptexp)) && ((mappingsegment ? SOffsetOf (tptr) : ASOffsetOf (tptr)) == 0)) {
				DEBUG_MSG (("mapsubscript: taking address of zeroth element of array/segment\n"));
				result = nameof (tptr);	/* bug 1125 24/1/91 */
/* *nptr = NULL; freetree(&tptr); *//* freeup the memory */
			}
#endif

			if (!map_address && needtemptoload (P_EXP, tptr)) {
				DEBUG_MSG (("mapsubscript: creating a temporary cos needtemptoload\n"));
				result = gettemp (tptr, NM_WORKSPACE);
				upusecount (result, 2);
				freetemp (result);
			}
			if (basetemp != NULL) {
				freetemp (basetemp);
			}
		}
		/* if subscripting inside a MOBILE array, reserve local 0 for address temporary */
#ifdef MOBILES
		if ((TagOf (tptr) == S_ARRAYITEM) && ismobile (ASBaseOf (tptr)) && (TagOf (ASBaseOf (tptr)) == S_ARRAYITEM)) {
#if 0
fprintf (stderr, "bind2: reserving local 0 for nested MOBILE indexing temporary\n");
#endif
			reservelowworkspace (1);
		}
#endif
	}
	/*}}} */
	return result;
}

/*}}}*/
/*{{{  PRIVATE void mapdop(left, right, type) */
PRIVATE void mapdop (treenode ** left, treenode ** right, int type)
{
	USE_VAR (type);		/* stop unused variable warning */

#if 0
	if (FALSE)		/*(hasdup && issame(*left, *right)) */
		mapexp (left);
	else
#endif
	{
		const int regsforleft = regsfor (*left);
		const int regsforright = regsfor (*right);

		DEBUG_MSG (("mapdop: regsforleft: %d, regsforright: %d, type: %s\n", regsforleft, regsforright, itagstring (type)));

		if (max_INT32 (regsforleft, regsforright) > MAXREGS)
			/*{{{  left or right needs temporaries */
		{
			if (regsforright >= regsforleft)
				/*{{{  evaluate right first */
			{
				if (regsforleft >= MAXREGS)
				 {	/* right must be stored in a temporary */
					/*{{{  right; stl temp; left */
					mapexp (right);
					*right = gettemp (*right, NM_WORKSPACE);
					upusecount (*right, 2);
					mapexp (left);
					freetemp (*right);
				}
				/*}}} */
				else {	/* we can keep right on the stack */
					/*{{{  right; left */
					mapexp (right);
					mapexp (left);
				}
				/*}}} */
			}
			/*}}} */
			else
				/*{{{  evaluate left first */
			{
				if (regsforright >= MAXREGS)
				 {	/* left must be stored in a temporary */
					/*{{{  left; stl temp; right */
					mapexp (left);
					*left = gettemp (*left, NM_WORKSPACE);
					upusecount (*left, 2);
					mapexp (right);
					freetemp (*left);
				}
				/*}}} */
				else {	/* we can keep left on the stack */
					/*{{{  left; right */
					mapexp (left);
					mapexp (right);
				}
				/*}}} */
			}
			/*}}} */
		}
		/*}}} */
		else if ((regsforleft == MAXREGS) && (regsforright == MAXREGS))
			/*{{{  right; stl temp; left */
		{
			mapexp (right);
			*right = gettemp (*right, NM_WORKSPACE);
			upusecount (*right, 2);
			mapexp (left);
			freetemp (*right);
		}
		/*}}} */
		else
			/*{{{  no temporaries needed */
		{
			mapexp (left);
			mapexp (right);
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void mapbool(tptr) */
PUBLIC void mapbool (treenode ** tptr)
{
	switch (TagOf (*tptr)) {
		/*{{{  S_AND S_OR */
	case S_AND:
	case S_OR:
		mapbool (LeftOpAddr (*tptr));
		mapbool (RightOpAddr (*tptr));
		break;
		/*}}} */
		/*{{{  S_EQ S_NE S_GR S_LS S_GE S_LE S_AFTER */
	case S_EQ:
	case S_NE:
	case S_GR:
	case S_LS:
	case S_GE:
	case S_LE:
		{
			const int tag = TagOf (*tptr);
			const int type = DOpTypeOf (*tptr);
			const SOURCEPOSN locn = LocnOf (*tptr);
			const int libroutinetag = ((tag == S_EQ) || (tag == S_NE)) ? S_EQ : S_GR;
			if (has_fpu_core && isreal (type))
				mapfpexp (tptr);
			else if ((tag == S_EQ || tag == S_NE) && !use_shortintops && isshortint (type))
			 {	/* bug 1347 2/9/91 */
				/*{{{  short word equality - mask the operands */
				/* INSdi10983 07/04/93 - added calls to foldexp */
				SetLeftOp (*tptr, foldexp (newdopnode (S_BITAND, locn, LeftOpOf (*tptr), newconstant (typemask (type)), type)));
				SetRightOp (*tptr, foldexp (newdopnode (S_BITAND, locn, RightOpOf (*tptr), newconstant (typemask (type)), type)));
				mapdop (LeftOpAddr (*tptr), RightOpAddr (*tptr), type);
			}
			/*}}} */
			else if (isreal (type) || isquadlength (type) || (!use_shortintops && isshortint (type)))
			 {	/* bug 1345 24/7/91 */
				/*{{{  convert to a function call and map that */
				treenode *iname, *paramlist;
				treenode *op1 = LeftOpOf (*tptr), *op2 = RightOpOf (*tptr);
				/*{{{  swap operands if neccessary */
				if (((tag == S_EQ || tag == S_NE) && regsfor (op1) > regsfor (op2)) || tag == S_GE || tag == S_LS) {
					treenode *temp = op1;
					op1 = op2;
					op2 = temp;
				}
				/*}}} */
				/*{{{  construct function call */
				paramlist = newlistnode (S_LIST, locn, op1, newlistnode (S_LIST, locn, op2, NULL));
				iname = libentry (libcallstring (libroutinetag, type), locn);
				*tptr = newinstancenode (S_FINSTANCE, locn, iname, paramlist);
				/*}}} */
				/*{{{  invert function result if neccessary */
				if (tag == S_NE || tag == S_GE || tag == S_LE)
					*tptr = newmopnode (S_NOT, locn, *tptr, S_BOOL);
				/*}}} */
				mapexp (tptr);
			}
			/*}}} */
			else if (fitsinword (type))
				mapdop (LeftOpAddr (*tptr), RightOpAddr (*tptr), type);
			else
				maprellop (libroutinetag, type, LeftOpAddr (*tptr), RightOpAddr (*tptr));
		}
		break;
	case S_AFTER:
		assert (FALSE);	/* after is processd in trans */
		break;
		/*}}} */
		/*{{{  S_NOT */
	case S_NOT:
		mapbool (OpAddr (*tptr));
		break;
		/*}}} */
	default:
		mapexp (tptr);
		break;
	}
}

/*}}}*/
/*{{{  PUBLIC void mapexplist(tptr) */
/*****************************************************************************
 *
 *  mapexplist maps a list of expressions
 *
 *****************************************************************************/
PUBLIC void mapexplist (treenode * tptr)
{
	while (!EndOfList (tptr)) {
		mapexp (ThisItemAddr (tptr));
		tptr = NextItem (tptr);
	}
}

/*}}}*/
/*{{{  PRIVATE void mapconstant */
PRIVATE void mapconstant (treenode * t)
{
	if (shouldbeinconstanttable (t))
		placeintable (t);
}

/*}}}*/
/*{{{  PUBLIC void mapexp(tptr) */
/*{{{  comment */
/*****************************************************************************
 *
 *  mapexp walks an expression tree inserting any temporaries needed.
 *         It also sets the global staticlinkused to the minimum lexical
 *         level of any names in the expression.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void mapexp (treenode ** tptr)
{
	while (TRUE) {
		treenode *t = *tptr;
#if 0
fprintf (stderr, "mapexp: t = ");
printtreenl (stderr, 4, t);
#endif
		switch (TagOf (t)) {
			/*{{{  element                          return / break */
			/*{{{  name                             return */
			/*{{{  VALABBR VALRETYPE */
		case N_VALABBR:
		case N_VALRETYPE:
		case S_FNFORMALRESULT:
			if (!isconst (t))
				staticlinkused = (int) min_INT32 (staticlinkused, NLexLevelOf (t));
			upusecount (t, 1);
			return;
			/*}}} */
			/*{{{  ABBR RETYPE PARAM */
		case N_ABBR:
		case N_RETYPE:
		case N_DECL:
		case N_PARAM:
		case N_VALPARAM:
		case N_RESULTPARAM:
		case N_REPL:
			staticlinkused = (int) min_INT32 (staticlinkused, NLexLevelOf (t));
			upusecount (t, 1);
			if (needtemptoload (P_EXP, t)) {
				*tptr = gettemp (t, NM_WORKSPACE);
				upusecount (*tptr, 2);
				freetemp (*tptr);
			}
			return;
			/*}}} */
			/*}}} */
			/*{{{  S_ARRAYITEM S_SEGMENTITEM        return */
		case S_ARRAYITEM:
		case S_SEGMENTITEM:
		case S_RECORDSUB:
		case S_RECORDITEM:
			*tptr = mapsubscript (t, FALSE);
			return;
			/*}}} */
			/*}}} */
			/*{{{  dyadic operator                  return / break */
			/*{{{  error setting stuff */
		case S_ADD:
		case S_SUBTRACT:
		case S_MULT:
		case S_DIV:
		case S_REM:
		case S_RSHIFT:
		case S_CSUB0:
		case S_CCNT1:
			{
				const int type = DOpTypeOf (t);
				if (isreal (type) || (!use_shortintops && isshortint (type))) {
					/* bug 1345 23/7/91 */
					/*{{{  convert to a function call and map that */
					*tptr = makedopfunction (t);
					/*}}} */
				} else {
					/*{{{  mapdop; return */
#if 0				/* Removed 27/01/92 for bug TS/1581 */
					if (TagOf (t) == S_RSHIFT && isshortint (type)) {	/* bug 1345 24/7/91 */
						/* mask to ensure that the bits shifted in are zero */
						/* Note that the stuff in 'tdop' ensures that the masked
						   value is not signextended again!
						 */
						SetLeftOp (t, newdopnode (S_BITAND, LocnOf (t), LeftOpOf (t), newconstant (typemask (type)), type));
					}
#endif
					mapdop (LeftOpAddr (t), RightOpAddr (t), type);
					return;
					/*}}} */
				}
			}
			break;
			/*}}} */
			/*{{{  BIT operations */
		case S_BITAND:
		case S_BITOR:
		case S_XOR:
		case S_PLUS:
		case S_MINUS:
		case S_LSHIFT:
			/* bug 1339 - don't need to check for short int ops,
			   because we can now do everything in-line - CON 16/7/91
			 */
			mapdop (LeftOpAddr (t), RightOpAddr (t), DOpTypeOf (t));
			return;
			/*}}} */
			/*{{{  TIMES */
		case S_TIMES:
			{
				treenode **left = LeftOpAddr (t);
				treenode **right = RightOpAddr (t);
				const int type = DOpTypeOf (t);
				if (fitsinword (type)) {
					/* INSdi01874 */
					const int times_lhs_is_faster = 1 << (bytesinscalar (type) * 8 / 2);
					if ((isconstexpnd (*left) &&
					     (((INT32) LoValOf (*left) < TIMES_RHS_IS_FASTER) &&
					      (((INT32) LoValOf (*left) > 0) ||
					       (fast_negative_prod && ((INT32) LoValOf (*left) > (-TIMES_RHS_IS_FASTER))))))
					    ||
					    (isconstexpnd (*right) &&
					     (((INT32) LoValOf (*right) > times_lhs_is_faster) ||
					      ((INT32) LoValOf (*right) < 0 && !fast_negative_prod) ||
					      ((INT32) LoValOf (*right) < (-(INT32) times_lhs_is_faster))))) {
						/* swap the operands to get small numbers on rhs */
						treenode *temp;
						temp = *left;
						*left = *right;
						*right = temp;
					}
				}
				mapdop (left, right, type);
			}
			return;
			/*}}} */
			/*{{{  EVAL */
		case S_EVAL:
			tptr = RightOpAddr (t);
			break;
			/*}}} */
			/*}}} */
			/*{{{  monadic operator                 return / break */
			/*{{{  NEG UMINUS */
		case S_NEG:
		case S_UMINUS:
			{
				const int type = MOpTypeOf (t);
				/*if (isreal(type) || isshortint(type)) */
				if (isreal (type) || (TagOf (t) == S_NEG &&	/* bug 1339 16/7/91 */
						      !use_shortintops && isshortint (type)))
				 {	/* bug 1345 24/7/91 */
					/*{{{  convert to a function call and map that */
					/*{{{  COMMENT what the tree looks like */
		  /**********************  Start comment out ****************************
                  @*
                            neg               finstance
                             |       =>       /       \
                            op            real32op    list
                                                     /    \
                                                   zero   list
                                                         /    \
                                                      op.sub  list
                                                             /    \
                                                            op    NULL
                  
                                      =>      finstance
                                              /       \
                                          int16sub   list
                                                    /    \
                                                  zero  list
                                                        /   \
                                                       op   NULL
                  
                  *@
                   **********************   End comment out  ****************************/
					/*}}} */
					const int doptag = (TagOf (t) == S_NEG) ? S_SUBTRACT : S_MINUS;
					treenode *doptree = newdopnode (doptag, LocnOf (t), newconstant (0),
									OpOf (*tptr), type);
					*tptr = makedopfunction (doptree);
				}
				/*}}} */
				else
					tptr = OpAddr (t);
			}
			break;
			/*}}} */
#if 0				/* bug 1339 - treat BITNOT the same as NOT 16/7/91 */
			/*{{{  BITNOT */
		case S_BITNOT:
			{
				int type = MOpTypeOf (t);
				if (isshortint (type)) {
					SOURCEPOSN locn = LocnOf (t);
					*tptr = newinstancenode (S_FINSTANCE, locn,
								 libentry (libcallstring (TagOf (t), type), locn),
								 newlistnode (S_LIST, locn, OpOf (t), NULL));
				} else
					tptr = OpAddr (t);
			}
			break;
			/*}}} */
#endif
			/*{{{  NOT */
		case S_BITNOT:	/* added 16/7/91 for bug 1339 */
		case S_NOT:
			tptr = OpAddr (t);
			break;
			/*}}} */
#ifdef MOBILES
		case S_ADDROF:
		case S_HWADDROF:
		case S_CLONE:
		case S_DEFINED:
			tptr = OpAddr (t);
			break;
		case S_NEW_ARRAY:
			tptr = ARDimLengthAddr (t);
			break;
		case S_ALLOC_PROC:
		case S_NEW_BARRIER:
			return;
		case S_NTH_DIMENSION:
			/* don't need to do anything :) */
			return;
#endif
		case S_TYPEHASHOF:
			/* tptr = OpAddr (t); */
			return;
			/*{{{  SIZE ELSIZE SEGSTART */
		case S_SIZE:
#ifdef MOBILES
#if 0
fprintf (stderr, "mapexp: S_SIZE t =");
printtreenl (stderr, 4, t);
#endif
			/* if this is a dynamic mobile no mapping is needed */
			if (isdynmobilearray (OpOf (t))) {
				return;
			} else {
				/* now we support multi-dimensional dynamic MOBILEs, asking for the size
				 * of a particular dimension is fine, but we have to hunt down here for it,
				 * botching "isdynmobilearray" isn't sensible since subscriptions aren't MOBILE (unless MOBILE sub-typing is going on..!)
				 */
				treenode *sop = OpOf (t);

				while (TagOf (sop) == S_ARRAYITEM) {
					sop = ASBaseOf (sop);
				}
				if (isdynmobilearray (sop)) {
					/* we can pick the dimension up in one go */
					sop = OpOf (t);

					while (TagOf (sop) == S_ARRAYITEM) {
						/* map the CSUB0 component */
						mapexp (ASExpAddr (sop));
						sop = ASBaseOf (sop);
					}
					return;
				}
			}
#endif
			tptr = dimexpaddr (OpOf (t), 0);
			break;
		case S_ELSIZE:
#ifdef MOBILES
#if 0
fprintf (stderr, "mapexp: ELSIZE: OpOf (t) =");
printtreenl (stderr, 4, OpOf (t));
#endif
			if (isdynmobilearray (OpOf (t))) {
				return;
			}
#endif
			t = OpOf (t);
			if (TagOf (t) == S_SEGMENT || TagOf (t) == S_SEGMENTITEM)
				/*{{{  map the length expression */
			{
				treenode *lengthexp = SLengthExpOf (t);
				if (!isconst (lengthexp) && !issimplelocal (lengthexp, be_lexlevel))
					/*{{{  insert temporary on slength */
				{
					treenode *temp;
					mapexp (SLengthExpAddr (t));
					temp = gettemp (lengthexp, NM_WORKSPACE);
					SetTag (temp, T_PREEVALTEMP);
					addtemplist (temp);
					SetSLengthExp (t, temp);
				}
				/*}}} */
			}
			/*}}} */
			tptr = dimexpaddr (t, 0);
			break;
		case S_SEGSTART:
			tptr = SStartExpAddr (OpOf (t));
			break;
			/*}}} */
			/*{{{  ADDRESSOF                        return */
		case S_ADDRESSOF:
			mapaddr (OpAddr (*tptr));
			return;
			/*}}} */
			/*}}} */
			/*{{{  conversion                       return / break */
			/*{{{  S_EXACT */
		case S_EXACT:
			{
				treenode **source = OpAddr (t);
				int sourcetype = ntypeof (*source);
				int desttype = MOpTypeOf (t);
				/*fprintf(outfile, "Mapping an EXACT\n"); */
#if 0				/* We've removed 'nop' conversions in trans */
				if (isreal (sourcetype))
					/*{{{  real to real exact conversion */
					/* desttype must be REAL32 to reach here, so sourcetype must be REAL32 */
					*tptr = OpOf (t);
				/*}}} */
				else
#endif
				if (isdoublelength (sourcetype) && (CONVERSIONCHECKING || !isaddressable (*source))) {
					int sourcemode = P_EXP;
					BOOL freeopdtemp = FALSE;

					sourcemode = mapaddresslopd (sourcemode, source, &freeopdtemp);
					if (freeopdtemp) {
						freetemp (*source);
					}
					return;
				} else if (isquadlength (sourcetype)) {
					/*{{{  convert to a function call */
					/*if (isshortint(desttype) || istargetbytesize(desttype)) */
					if (isshorttype (desttype)) {	/* same thing but simpler */
						desttype = targetintsize;
					}

					*source = makeconversion (sourcetype, desttype, *source, S_EXACT, LocnOf (t));
					/*}}} */
				} else {
					tptr = OpAddr (t);
				}
			}
			break;
			/*}}} */
			/*{{{  S_ROUND S_TRUNC */
		case S_ROUND:
		case S_TRUNC:
			{
				treenode *source = OpOf (t);
				SOURCEPOSN locn = LocnOf (source);
				int sourcetype = ntypeof (source);
				int desttype = MOpTypeOf (t);

#if 0
fprintf (stderr, "bind2: mapexp: mapping ROUND/TRUNC\n");
#endif
				if (sourcetype == S_INT) {
					sourcetype = targetintsize;
				}
				if (desttype == S_INT) {
					desttype = targetintsize;
				}
#if 0				/* we've done this in trans */
				if (sourcetype == desttype)
					*tptr = source;	/* Remove the conversion totally */
				else
#endif
				if (desttype == S_REAL32) {
					/*{{{  handle conversion to REAL32 */
					if (isshorttype (sourcetype)) {
						/*{{{  convert via source -> INT -> dest */
						source = newmopnode (S_EXACT, locn, source, S_INT);
						SetOp (t, source);
						sourcetype = S_INT;
						/*}}} */
					}
					*tptr = makeconversion (sourcetype, desttype, source, TagOf (t), LocnOf (t));
					/*}}} */
				} else if (isreal (sourcetype)) {
					/*{{{  handle conversion from REAL32 or REAL64 */
#if 0
fprintf (stderr, "bind2: mapexp: conversion from REAL source\n");
#endif
					if (isshorttype (desttype)) {
						/*{{{  convert via source -> INT -> dest */
						treenode *conversion = newmopnode (S_EXACT, locn, t, desttype);
						SetMOpType (t, S_INT);
						*tptr = conversion;
						/*}}} */
					} else {
						if (has_fpu_core && !T9000_alpha_nofpconv (&tx_global)) {
							/* Sections 4.2.7, 4.2.8, 4.2.11, 4.2.12 of SW-0341-2 */
							/*{{{  generate source into fpu, store into a temp, load temp into cpu */
							mapfpexp (OpAddr (*tptr));
							*tptr = gettemp (*tptr, NM_WORKSPACE);
							upusecount (*tptr, 2);
							freetemp (*tptr);
							return;
							/*}}} */
						} else {
							*tptr = makeconversion (sourcetype, desttype, source, TagOf (t), LocnOf (t));
						}
					}
					/*}}} */
				} else {
					geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "mapexp-conversion");
				}
			}
			break;
			/*}}} */
			/*}}} */
			/*{{{  Boolean operator                 return */
		case S_AND:
		case S_OR:
		case S_EQ:
		case S_NE:
		case S_LS:
		case S_LE:
		case S_GR:
		case S_GE:
			mapbool (tptr);
			return;
			/*}}} */
			/*{{{  S_STRING S_CONSTCONSTRUCTOR      return */
		case S_STRING:
		case S_CONSTCONSTRUCTOR:
			SetCTNext (t, consttablechain);
			consttablechain = t;
			return;
			/*}}} */
			/*{{{  S_CONSTEXP                       return */
		case S_CONSTEXP:
			if (!isinconstanttable (t)) {
				mapconstant (t);
			}
			return;
			/*}}} */
			/*{{{  S_FINSTANCE                      return */
		case S_FINSTANCE:
			if ((TagOf (INameOf (t)) == N_PREDEFFUNCTION) && mappredef (t, NULL))
				return;	/* Exit if done inline */
			SetIParamList (t, augmentparams (IParamListOf (t), FnParamsOf (NTypeOf (INameOf (t))), NULL, t));
			mapinstance (t);
			return;
			/*}}} */
			/*{{{  specification ... valof          return */
		case S_VALABBR:
		case S_ABBR:
		case S_VALRETYPE:
		case S_RETYPE:
		case S_PROCDEF:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_DECL:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_VALOF:
#ifdef MOBILES
		case S_FORWDECL:
		case S_PROCTYPEDECL:
		case S_MPROCDECL:
#endif
			mapvalof (t, NULL);
			return;
			/*}}} */
			/*{{{  special parameter values         return / break */
		case S_PARAM_STATICLINK:
		case S_PARAM_VSP:
		case S_PARAM_FB:
		case S_PARAM_WS:
#ifdef MOBILES
		case S_PARAM_MSP:
		case S_PARAM_MPP:
		case S_HIDDEN_TYPE:
#endif
			return;
		case S_FNACTUALRESULT:
			/* Formal parameter */
			tptr = HExpAddr (t);
			if (TagOf (*tptr) == T_TEMP)
				return;
			/* Otherwise we load a pointer to the element */
			break;
		case S_HIDDEN_PARAM:
			tptr = HExpAddr (t);
			break;
			/*}}} */
			/*{{{  temporary                        return / break */
		case T_TEMP:
			/*{{{  COMMENT don't think this is necessary (or desireable) */
	    /**********************  Start comment out ****************************
            @*{{{  *@
            @* Someone has beaten mapexp to inserting temporaries : this can happen
               when mappreprocess inserts them on segment start and length expressions.
               Still, there might be more temporaries needed further down *@
            staticlinkused = min_INT32(staticlinkused, NLexLevelOf(t));
            upusecount(t, 1);
            tptr = NDeclAddr(t);
            break;
            @*}}}*@
             **********************   End comment out  ****************************/
			/*}}} */
		case T_PREEVALTEMP:
			staticlinkused = (int) min_INT32 (staticlinkused, NLexLevelOf (t));
			return;
			/*}}} */
			/*{{{  dummy expression                 return */
		case S_DUMMYEXP:
		case S_ASMNAME:
			return;
			/*}}} */
			/*{{{  UDV constant*/
		case S_UDVCONSTEXP:
			if ((int)(LoValOf (t)) == -1) {
				/* don't know -- evaluate expression */
				tptr = CExpAddr (t);
			}
			return;
			/*}}}*/
		default:
			badtag (genlocn, TagOf (t), "mapexp");
		}
	}
}

/*}}}*/
/*{{{  PUBLIC void mapaddr(tptr) */
/*{{{  comment */
/*****************************************************************************
*
*  mapaddr walks an element tree inserting any temporaries needed in
*          order to load the address of the element.
*          It also sets the global staticlinkused to the minimum lexical
*          level of any names in the element.
*
*****************************************************************************/
/*}}}*/
PUBLIC void mapaddr (treenode ** tptr)
{
	while (*tptr != NULL) {
		treenode *t = *tptr;
		switch (TagOf (t)) {
		default:
			badtag (genlocn, TagOf (t), "mapaddr");
			return;
#ifdef MOBILES
		case S_UNDEFINED:
			tptr = OpAddr (t);
			break;
		case S_ADDROF:
		case S_HWADDROF:
		case S_CLONE:
			tptr = OpAddr (t);
			break;
		case S_PARAM_MPP:
			return;
		case S_TYPEHASHOF:
			tptr = OpAddr (t);
			break;
#endif
		case S_PARAM_WS:
			return;
			/*{{{  element                          return / break */
			/*{{{  name                             return */
		case N_VALABBR:
		case N_VALRETYPE:
			if (!isconst (t)) {
				staticlinkused = (int) min_INT32 (staticlinkused, NLexLevelOf (t));
			}
			upusecount (t, 1);
			return;
		case N_ABBR:
		case N_RETYPE:
		case N_DECL:
		case N_VALPARAM:
		case N_PARAM:
		case N_RESULTPARAM:
		case N_REPL:
		case S_FNFORMALRESULT:
			staticlinkused = (int) min_INT32 (staticlinkused, NLexLevelOf (t));
			upusecount (t, 1);
			return;
		case N_LABELDEF:
			return;
		case N_PROCDEF:
		case N_MPROCDECL:
		case N_LIBPROCDEF:	/* bug TS/1240 12/05/92 */
		case N_LIBMPROCDECL:
		case N_LFUNCDEF:
		case N_SFUNCDEF:
		case N_LIBFUNCDEF:
			maproutinename (t, NULL, FALSE, FALSE, PROC_NONE);
			return;
			/*}}} */
			/*{{{  S_ARRAYITEM S_SEGMENTITEM        return */
		case S_ARRAYITEM:
		case S_SEGMENTITEM:
		case S_RECORDSUB:
		case S_RECORDITEM:
			*tptr = mapsubscript (t, TRUE);
			return;
			/*}}} */
			/*{{{  S_SEGMENT                                 break */
		case S_SEGMENT:
			/* We are nested inside an arrayitem or segmentitem tree */
			tptr = SNameAddr (t);
			break;
			/*}}} */
			/*{{{  S_ARRAYSUB                                break */
		case S_ARRAYSUB:
			/* We are nested inside an arrayitem or segmentitem tree */
			tptr = ASBaseAddr (t);
			break;
			/*}}} */
			/*}}} */
			/*{{{  S_STRING S_CONSTCONSTRUCTOR      return */
		case S_STRING:
		case S_CONSTCONSTRUCTOR:
			SetCTNext (t, consttablechain);
			consttablechain = t;
			return;
			/*}}} */
			/*{{{  S_CONSTRUCTOR / VALOF            return */
#if 0				/* no constructors are passed to the backend any more */
		case S_CONSTRUCTOR:
#if 0				/* bug TS/1442 28/10/91 */
		case S_VALABBR:
		case S_ABBR:
		case S_VALRETYPE:
		case S_RETYPE:
		case S_PROCDEF:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_DECL:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_VALOF:
#endif
			DEBUG_MSG (("mapaddr: CONSTRUCTOR\n"));
			/*printf("mapaddr: creating temp\n"); */
			*tptr = gettemp (t, NM_WORKSPACE);
			upusecount (*tptr, 1);
			mapsimpleassign (TagOf (gettype (t)), P_TEMP, tptr, P_EXP, NDeclAddr (*tptr));
			freetemp (*tptr);
			return;
#endif
			/*}}} */
			/*{{{  temporary                        return / break */
		case T_TEMP:
			/*{{{  COMMENT don't think this is necessary (or desireable) */
	  /**********************  Start comment out ****************************
          @*{{{  *@
            @* Someone has beaten mapexp to inserting temporaries : this can happen
               when mappreprocess inserts them on segment start and length expressions.
               Or when constructors have been put into temporaries.
               Still, there might be more temporaries needed further down *@
            staticlinkused = min_INT32(staticlinkused, NLexLevelOf(t));
            upusecount(t, 1);
            tptr = NDeclAddr(t);
            break;
          @*}}}*@
           **********************   End comment out  ****************************/
			/*}}} */
		case T_PREEVALTEMP:
			staticlinkused = (int) min_INT32 (staticlinkused, NLexLevelOf (t));
			return;
			/*}}} */
			/*{{{  S_CONSTEXP                       return */
		case S_CONSTEXP:
			/* We want to load the address of a constant, so it should go
			   into the constant table */
			if (!isinconstanttable (t)) {
				placeintable (t);
			}
			return;
			/*}}} */
			/*{{{  S_NULLARRAY			return */
		case S_NULLARRAY:
			return;
			/*}}}*/
		}
	}
}

/*}}}*/
/*{{{  PUBLIC void mapexpopd(mode, opd) */
PUBLIC void mapexpopd (int mode, treenode ** opd)
{
	switch (mode) {
	case P_PTR:
		mapaddr (opd);
		break;
	case P_EXP:
		if (has_fpu_core && isreal (ntypeof (*opd))) {
			mapfpexp (opd);
		} else {
			mapexp (opd);
		}
		break;
	case P_TEMP:
	case P_TEMPPTR:
		if (TagOf (*opd) == T_TEMP)
			upusecount (*opd, 1);
		break;
	}
}

/*}}}*/
/*{{{  PUBLIC void mapstoreinopd(mode, opd) */
/*****************************************************************************
 *
 *  mapstoreinopd maps the storing of Areg into (mode, opd)
 *
 *****************************************************************************/
/* We must ensure that the destination tree is walked over so that
   staticlinkused is correctly set, and constants are found, etc. */
PUBLIC void mapstoreinopd (const int mode, treenode ** const opd)
{
	switch (mode) {
	case P_EXP:
		mapexp (opd);
		break;
	case P_PTR:
		mapaddr (opd);
		break;
	case P_TEMP:
	case P_TEMPPTR:
		break;
	}
}

/*}}}*/
/*{{{  PUBLIC int mapload2regs(e1mode, e1, e2mode, e2) */
/*{{{  comment */
/*****************************************************************************
 *
 *  mapload2regs maps out workspace requirement for loading the expressions
 *               (e1mode, e1) into Breg and (e2mode, e2) into Areg.
 *               'Temp' nodes are inserted where needed.
 *               The required loading sequence is returned :
 *               ( 1 = e1; e2       2 = e2; e1; rev )
 *
 *****************************************************************************/
/*}}}*/
PUBLIC int mapload2regs (int e1mode, treenode ** e1, int e2mode, treenode ** e2)
{
#if 0
	if (FALSE) {		/*(hasdup && e1mode == e2mode && issame(*e1, *e2)) */
		/* The problem occurs if this mapping decides that e1 needs temporaries
		   in which case it modifies the tree. Then later on in tload2regs,
		   the trees are no longer identical, so it doesn't try doing
		   common subexpression elimination there!
		 */
		mapexpopd (e1mode, e1);
		return (1);
	} else {/* what's below */}
#endif

	int regsfore1 = regsforopd (e1mode, *e1);
	int regsfore2 = regsforopd (e2mode, *e2);
	if (max_INT32 (regsfore1, regsfore2) > MAXREGS) {
		/*{{{  we need temporaries */
		if (regsfore2 >= regsfore1) {
			if (regsfore1 >= MAXREGS) {
				/*{{{  e2; stl temp; e1; ldl temp */
				mapexpopd (e2mode, e2);
				*e2 = getopdtemp (e2mode, *e2);
				upusecount (*e2, 2);
				mapexpopd (e1mode, e1);
				freetemp (*e2);
				return (1);
				/*}}} */
			} else {
				/*{{{  e2; e1  -   temporaries introduced lower down */
				mapexpopd (e2mode, e2);
				mapexpopd (e1mode, e1);
				return (2);
				/*}}} */
			}
		} else if (regsfore2 >= MAXREGS) {
			/*{{{  e1; stl temp; e2; ldl temp */
			mapexpopd (e1mode, e1);
			*e1 = getopdtemp (e1mode, *e1);
			upusecount (*e1, 2);
			mapexpopd (e2mode, e2);
			freetemp (*e1);
			return (2);
			/*}}} */
		} else {
			/*{{{  e1; e2  -  temporaries introduced lower down */
			mapexpopd (e1mode, e1);
			mapexpopd (e2mode, e2);
			return (1);
			/*}}} */
		}
		/*}}} */
	} else if ((regsfore1 == MAXREGS) && (regsfore2 == MAXREGS)) {
		/*{{{  e2; stl temp; e1; ldl temp */
		mapexpopd (e2mode, e2);
		*e2 = getopdtemp (e2mode, *e2);
		upusecount (*e2, 2);
		mapexpopd (e1mode, e1);
		freetemp (*e2);
		return (1);
		/*}}} */
	} else {
		/*{{{  e1; e2 - no temporaries at all */
		mapexpopd (e1mode, e1);
		mapexpopd (e2mode, e2);
		return (1);
		/*}}} */
	}
	return (1);		/* just to satisfy the compilers */
}

/*}}}*/
/*{{{  PRIVATE int chop(x) */
PRIVATE int chop (int x)
{
	if (x <= 1)
		return (0);
	if (x == 2)
		return (1);
	return (2);
}

/*}}}*/
/*{{{  PUBLIC int giveorder(e1mode,e1,e2mode,e2,e3mode,e3,preeval_e2,preeval_e3) */
/* Return a loading sequence for loading the register stack with e1, e2, e3.
Set preeval_e2/preeval_e3 to TRUE if these expressions must be prevaluated
into temporaries.
*/
#define P2 0x20
#define P3 0x10
#define LS 0x0F

PRIVATE int regsaction[2][3][3] = {
	{
	 {1, 2, 4},
	 {1, P3 | 1, P3 | 1},
	 {3, P3 | 3, P3 | 3}
	 },
	{
	 {1, 2, P3 | 1},
	 {1, P3 | 1, P3 | 1},
	 {P2 | 1, P2 | 2, (P2 | P3 | 1)}
	 }
};

PUBLIC int giveorder (int e1mode, treenode * e1, int e2mode, treenode * e2, int e3mode, treenode * e3, BOOL * preeval_e2, BOOL * preeval_e3)
{
	int regsfore1, regsfore2, regsfore3;
	int tableentry;
	int e2temp = FALSE, e3temp = FALSE;

	/*{{{  force preevaluation if we need a temp. anyway */
	if (needtemptoload (e2mode, e2)) {
		e2temp = TRUE;
		e2mode = tempmodeof (e2mode);
		DEBUG_MSG (("giveorder: e2(B) is a temp\n"));
	}
	if (needtemptoload (e3mode, e3)) {
		e3temp = TRUE;
		e3mode = tempmodeof (e3mode);
		DEBUG_MSG (("giveorder: e3(A) is a temp\n"));
	}
	/*}}} */
	regsfore1 = regsforopd (e1mode, e1);
	regsfore2 = regsforopd (e2mode, e2);
	regsfore3 = regsforopd (e3mode, e3);
#if 0
fprintf (stderr, "giveorder: regsfor e1(C) [%d]: %d, e2(B) [%d]: %d, e3(A) [%d]: %d\n", e1mode, regsfore1, e2mode, regsfore2, e3mode, regsfore3);
/*	DEBUG_MSG (("giveorder: regsfor e1(C): %d, e2(B): %d, e3(A): %d\n", regsfore1, regsfore2, regsfore3)); */
#endif
	regsfore1 = ((regsfore1 < MAXREGS) ? 0 : 1);
	regsfore2 = chop (regsfore2);
	regsfore3 = chop (regsfore3);

	tableentry = regsaction[regsfore1][regsfore2][regsfore3];
	*preeval_e2 = ((tableentry & P2) != 0) || e2temp;
	*preeval_e3 = ((tableentry & P3) != 0) || e3temp;
	DEBUG_MSG (("giveorder: preeval e2(B)? %d, preeval e3(A)? %d\n", *preeval_e2, *preeval_e3));

	return (tableentry & LS);
}

/*}}}*/
/*{{{  PUBLIC int mapload3regs(e1mode, e1, e2mode, e2, e3mode, e3) */
/*{{{  words */
/*****************************************************************************
 *
 *  mapload3regs maps out workspace requirement for loading the expressions
 *               (e1mode, e1) into Creg, (e2mode, e2) into Breg and
 *               (e3mode, e3) into Areg.
 *               'Temp' nodes are inserted where needed.
 *               The required loading sequence is returned :
 *                  1 = e1; e2; e3
 *                  2 = e1; e3; e2; rev
 *                  3 = e2; e1; rev; e3
 *                  4 = e3; e1; rev; e2; rev
 *
 *****************************************************************************/
/*}}}*/
PUBLIC int mapload3regs (int e1mode, treenode ** e1, int e2mode, treenode ** e2, int e3mode, treenode ** e3)
{
	BOOL preeval_e2 = FALSE, preeval_e3 = FALSE;
	int loadseq;
	loadseq = giveorder (e1mode, *e1, e2mode, *e2, e3mode, *e3, &preeval_e2, &preeval_e3);
	/*{{{  map preevaluations into temporaries */
	if (preeval_e2) {
		mapexpopd (e2mode, e2);
		*e2 = getopdtemp (e2mode, *e2);
		e2mode = tempmodeof (e2mode);
		upusecount (*e2, 2);
	}
	if (preeval_e3) {
		mapexpopd (e3mode, e3);
		*e3 = getopdtemp (e3mode, *e3);
		e3mode = tempmodeof (e3mode);
		upusecount (*e3, 2);
	}
	/*}}} */
	/*{{{  map expressions */
	mapexpopd (e1mode, e1);
	if (!preeval_e2)
		mapexpopd (e2mode, e2);
	if (!preeval_e3)
		mapexpopd (e3mode, e3);
	/*}}} */
	/*{{{  insert 'temp' nodes into tree */
	if (preeval_e3)
		freetemp (*e3);
	if (preeval_e2)
		freetemp (*e2);
	/*}}} */

	return (loadseq);
}

/*}}}*/

/*{{{  PUBLIC void mapstoreregs (dest, nregs) */
/*{{{  comment */
/*****************************************************************************
 *
 *  mapstoreregs maps out the storing of up to MAXREGS register values to
 *               their destinations.
 *               'nregs' is the number of register values to be stored.
 *               'dest' is an array of pointers to trees representing the
 *               destinations.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void mapstoreregs (treenode ** dest[MAXREGS], const int nregs)
{
	int regsfordest[MAXREGS];
	int i;
	int ndone;
	int topreg = nregs - 1;
	int freeregs = MAXREGS - nregs;
	for (i = 0; i < nregs; i++) {
		regsfordest[i] = regsforstore (*dest[i]);
		DEBUG_MSG (("mapstoreregs: regsfordest %d is %d\n", i, regsfordest[i]));
	}
	for (ndone = 0; ndone < nregs; ndone++)
		/*{{{  (store top register) or (rev; store top register) */
	{
		if (regsfordest[topreg] <= freeregs)
			/*{{{  store top register */
		{
			DEBUG_MSG (("mapstoreregs: using store\n"));
			mapstoreinopd (P_EXP, dest[topreg]);
		}
		/*}}} */
		else if ((topreg > 0) && (regsfordest[topreg - 1] <= freeregs))
			/*{{{  rev; store top register */
		{
			DEBUG_MSG (("mapstoreregs: using rev; store\n"));
			mapstoreinopd (P_EXP, dest[topreg - 1]);
			dest[topreg - 1] = dest[topreg];
			regsfordest[topreg - 1] = regsfordest[topreg];	/* bug TS/1606 3/2/92 */
		}
		/*}}} */
		else
			/*{{{  store top register to temporary */
		{
			treenode **dptr = dest[topreg];
			DEBUG_MSG (("mapstoreregs: storing top to temporary\n"));
			*dptr = gettemp (*dptr, NM_WORKSPACE);
			upusecount (*dptr, 1);
		}
		/*}}} */
		topreg--;
		freeregs++;
	}
	/*}}} */
	/*{{{  move temporaries to real destinations, free temporaries */
	for (i = 0; i < nregs; i++) {
		treenode **dptr = dest[i];
		if (TagOf (*dptr) == T_TEMP) {
			DEBUG_MSG (("mapstoreregs: moving temp for dest %d\n", i));
			mapsimpleassign (ntypeof (*dptr), P_EXP, NDeclAddr (*dptr), P_TEMP, dptr);
			DEBUG_MSG (("mapstoreregs: freeing temp for dest %d\n", i));
			freetemp (*dptr);
		}
	}
	/*}}} */
}

/*}}}*/
