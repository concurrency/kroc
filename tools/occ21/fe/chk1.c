/*
 *	Occam 2 type checker
 *	Copyright (C) 1987 Inmos Limited
 *	Modifications 1999-2004 Fred Barnes  <frmb@kent.ac.uk>
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
#include <string.h>
#include "feinc.h"
#include "extlib.h"		/* IMPORTED */

#include "chkerror.h"
#include "predefhd.h"		/* checking of LOADINPUTCHANNEL etc */
#include "chkdef.h"
#include "chk1def.h"
#include "chk2def.h"
#include "chk4def.h"
#include "lexdef.h"		/* for strict_checking */

#include "syndef.h"		/* syn_lexlevel */
#include "mobiles.h"		/* mobile_getanontype */
#include "trandef.h"		/* ismobile, isdynmobile.. */

#ifdef OCCAM2_5
#define TYPE_COERCION TRUE
#else
#define TYPE_COERCION FALSE
#endif

/*}}}*/

/*{{{  global variables*/
PUBLIC SOURCEPOSN chklocn;
PUBLIC treenode *undeclaredp;
PUBLIC BOOL chk_permit_lonely_fields = FALSE;
#ifdef USER_DEFINED_OPERATORS
	extern BOOL user_defined_operators;
	PUBLIC BOOL modify_check = FALSE;
	PUBLIC treenode *left_type = NULL;
	PUBLIC treenode *right_type = NULL;
#endif
/* These leafnodes etc are only used for comparisons; hence they
   can remain in existence for as long as we like.
   It doesn't matter if we re-use them.
*/
PUBLIC treenode *intnodeptr;
PUBLIC treenode *uintnodeptr;
PUBLIC treenode *boolnodeptr;
PUBLIC treenode *channodeptr;
PUBLIC treenode *chanboolnodeptr;
PUBLIC treenode *unknownnodeptr;
PUBLIC treenode *nodenodeptr;
PUBLIC treenode *edgenodeptr;
PUBLIC treenode *arcnodeptr;

PRIVATE wordnode *udo_wordnodeptr;	/* added by Jim for user defined operators temp node */
#ifdef MOBILES
	PRIVATE treenode *mobilenodeptr;	/* added by Fred for MOBILEs */
#endif

PRIVATE treenode *bytenodeptr, *int16nodeptr, *int32nodeptr, *int64nodeptr,
	*uint16nodeptr, *uint32nodeptr, *uint64nodeptr,
	*real32nodeptr, *real64nodeptr;
PRIVATE treenode *controlportnodeptr;
PRIVATE treenode *routenodeptr;

/* This is initialised by the fe_ routines to point to the `currently active'
   frontend data structure.
*/
PUBLIC fe_handle_t *current_fe_handle;
PUBLIC const fe_data_t *current_fe_data;

PUBLIC treenode *checksamenew (treenode * t1, treenode * t2, treenode ** left, treenode ** right, BOOL * passed, treenode * default_type);

/* These are used for better reporting of type errors in nested proc/function
   calls */
typedef struct {
	const wordnode *fn_name;
	int paramno;
} typecheck_params_t;

PRIVATE const typecheck_params_t *typecheck_params;
/*}}}*/

/*{{{  PUBLIC void chkinit ()*/
PUBLIC void chkinit (void)
{
	if (boolnodeptr == NULL) {
		boolnodeptr = newleafnode (S_BOOL, NOPOSN);
		bytenodeptr = newleafnode (S_BYTE, NOPOSN);
		intnodeptr = newleafnode (S_INT, NOPOSN);
		int16nodeptr = newleafnode (S_INT16, NOPOSN);
		int32nodeptr = newleafnode (S_INT32, NOPOSN);
		int64nodeptr = newleafnode (S_INT64, NOPOSN);
		uintnodeptr = newleafnode (S_UINT, NOPOSN);
		uint16nodeptr = newleafnode (S_UINT16, NOPOSN);
		uint32nodeptr = newleafnode (S_UINT32, NOPOSN);
		uint64nodeptr = newleafnode (S_UINT64, NOPOSN);
		real32nodeptr = newleafnode (S_REAL32, NOPOSN);
		real64nodeptr = newleafnode (S_REAL64, NOPOSN);
		/*stringnodeptr  = newleafnode (S_STRING, NOPOSN); */
		nodenodeptr = newleafnode (S_NODE, NOPOSN);
		arcnodeptr = newleafnode (S_ARC, NOPOSN);
		edgenodeptr = newleafnode (S_EDGE, NOPOSN);
		controlportnodeptr = newleafnode (S_CONTROLPORT, NOPOSN);
		routenodeptr = newleafnode (S_ROUTE, NOPOSN);
		unknownnodeptr = newleafnode (S_UNKNOWN, NOPOSN);
		channodeptr = newtypenode (S_CHAN, NOPOSN, NULL, newleafnode (S_ANY, NOPOSN));
		chanboolnodeptr = newtypenode (S_CHAN, NOPOSN, NULL, newleafnode (S_BOOL, NOPOSN));

		undeclaredp = newleafnode (S_UNDECLARED, NOPOSN);
	}
	udo_wordnodeptr = newwordnode (S_NAME, NULL, 0, NULL);	/* temp word node for searching nametable */
	/* added by Jim 13/1/97 */
#ifdef MOBILES
	/* allow any mobile type (!) */
	mobilenodeptr = newtypenode (S_MOBILE, NOPOSN, NULL, newleafnode (S_ANY, NOPOSN));
#endif

	if (current_fe_data->fe_txlib->bpw != 2 && current_fe_data->fe_txlib->bpw != 4)
		err_abort ("chkinit");

	chklocn = NOPOSN;
}

/*}}}*/
/*{{{  PRIVATE treenode *typenodeptr (t)*/
PRIVATE treenode *typenodeptr (int t)
{
	switch (t)
		/*{{{  cases */
	{
	case S_BOOL:
		return boolnodeptr;
	case S_BYTE:
		return bytenodeptr;
	case S_INT:
		return intnodeptr;
	case S_INT16:
		return int16nodeptr;
	case S_INT32:
		return int32nodeptr;
	case S_INT64:
		return int64nodeptr;
	case S_UINT:
		return uintnodeptr;
	case S_UINT16:
		return uint16nodeptr;
	case S_UINT32:
		return uint32nodeptr;
	case S_UINT64:
		return uint64nodeptr;
	case S_REAL32:
		return real32nodeptr;
	case S_REAL64:
		return real64nodeptr;
	case S_CHAN:
		return channodeptr;
	case S_UNDECLARED:
		return undeclaredp;
	case S_NODE:
		return nodenodeptr;
	case S_ARC:
		return arcnodeptr;
	case S_EDGE:
		return edgenodeptr;
	case S_CONTROLPORT:
		return controlportnodeptr;
	case S_ROUTE:
		return routenodeptr;
#ifdef MOBILES
	case S_MOBILE:
		return mobilenodeptr;
#endif
	default:
		return NULL;
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC BOOL sametype(tag1, tag2)*/
/*****************************************************************************
 *
 *  sametype returns TRUE if the type tags 'tag1' and 'tag2' match: it
 *           matches an undeclared type with anything.
 *
 *****************************************************************************/
PUBLIC BOOL sametype (const int tag1, const int tag2)
{
	return ((tag1 == tag2) || (tag1 == S_UNDECLARED) || (tag2 == S_UNDECLARED));
}

/*}}}*/
/*{{{  PRIVATE treenode *arraytype (tptr, subscripts, str)*/
/* Name of the element we are subscripting. */
/* Move down array type tree 'tptr' 'subscripts' times.  If there aren't enough
dimensions in the array report error.
Return a tree representing the type subscripted 'subscripts' times.
*/
PRIVATE treenode *arraytype (treenode *tptr, int subscripts, const char *const str)
{
	while (subscripts > 0) {
		treenode *const real_type = follow_user_type (tptr);

		if (TagOf (real_type) == S_ARRAY) {
			subscripts--;
			tptr = ARTypeOf (real_type);
#ifdef MOBILES
		} else if (TagOf (real_type) == S_MOBILE) {
#if 0
printf ("arraytype: subscripts = %d, real_type = ", subscripts);
printtree (stdout, 4, real_type);
printf ("\n");
#endif
			tptr = MTypeOf (real_type);
#if 0
printf ("arraytype: after descending, tptr = ");
printtree (stdout, 4, tptr);
printf ("\n");
#endif
#endif
		} else {
			chkreport_s (CHK_INV_SUBSCRIPT, chklocn, str);
		}
	}
	return (tptr);
}

/*}}}*/
/*{{{  PUBLIC BOOL protocolinherits (treenode *child, treenode *ancestor)*/
PUBLIC BOOL protocolinherits (treenode *child, treenode *ancestor)
{
	if ((TagOf (child) != N_TPROTDEF) || (TagOf (ancestor) != N_TPROTDEF)) {
		return FALSE;
	}
	if (!NDeclOf (child) || !DExtraOf (NDeclOf (child))) {
		return FALSE;
	} else {
		treenode *t = DExtraOf (NDeclOf (child));

		for (; !EndOfList (t); t = NextItem (t)) {
			if (ThisItem (t) == ancestor) {
				return TRUE;
			}
		}
	}
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE BOOL protocolsequivalent*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  protocolsequivalent checks that the two protocols 'fprot' and 'aprot' are
 *                      equivalent. 'fprot' is the formal protcol (ie. protocol
 *                      of formal parameter, or lhs of abbreviation) , and
 *                      'aprot' is the actual protocol.
 *                      We enforce name equivalence rather than structural
 *                      equivalence.
 *                      if 'match_any' is TRUE,
 *                      an actual ANY protocol will match any formal protocol.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE BOOL protocolsequivalent (treenode * aprot, treenode * fprot, const BOOL match_any)
{
	if (match_any && TagOf (aprot) == S_ANY) {
		if ((TagOf (fprot) != S_ANY) && current_fe_data->fe_warning_chanofany)	/* WP 00026 31/03/92 */
			msg_out (SEV_WARN, CHK, CHK_CHAN_OF_ANY_PARAM_PASSING, chklocn);
		return TRUE;
	}

#if 0
printf ("protocolsequivalent: aprot = ");
printtree (stdout, 4, aprot);
printf ("\n  fprot = ");
printtree (stdout, 4, fprot);
printf ("\n");
fflush (stdout);
#endif
	while (TagOf (fprot) == TagOf (aprot)) {
		switch (TagOf (fprot)) {
			/*{{{  anarchic protocol */
		case S_ANY:
			return TRUE;
			/*}}} */
#ifdef MOBILES
			/*{{{  any channel-type protocol */
		case S_ANYCHANTYPE:
			{
				INT32 fattr = (LeafLinkOf (fprot) ? TypeAttrOf (LeafLinkOf (fprot)) : 0);
				INT32 aattr = (LeafLinkOf (aprot) ? TypeAttrOf (LeafLinkOf (aprot)) : 0);

				return (fattr == aattr);
			}
			/*}}}*/
			/*{{{  any mobile-process protocol / any mobile protocol */
		case S_ANYPROCTYPE:
		case S_ANYMOBILETYPE:
			/*}}}*/
			return TRUE;
			/*{{{  named protocol */
#endif
		case N_TPROTDEF:
		case N_SPROTDEF:
		case N_DECL:	/* undeclared protocol name */
#if defined(OCCAM2_5) && !defined(MOBILES)
		case N_TYPEDECL:
#endif
			return (fprot == aprot);
			/*}}} */
			/*{{{  N_TYPEDECL*/
#if defined(OCCAM2_5) && defined(MOBILES)
		case N_TYPEDECL:
		case N_PROCTYPEDECL:
			if (fprot == aprot) {
				return TRUE;
			} else if ((NNameOf (aprot) == NNameOf (fprot)) && (NTypeOf (aprot) == NTypeOf (fprot)) && (NScopeOf (aprot) == NScopeOf (fprot)) &&
					(NDeclOf (aprot) == NDeclOf (fprot))) {
				/* types not equivalent if there's a difference in direction or shared-ness though */
				if ((NTypeAttrOf (aprot) & (TypeAttr_marked_in | TypeAttr_marked_out | TypeAttr_shared)) !=
						(NTypeAttrOf (fprot) & (TypeAttr_marked_in | TypeAttr_marked_out | TypeAttr_shared))) {
					return FALSE;
				}
				return TRUE;
			}
			return FALSE;
#endif
			/*}}}*/
			/*{{{  simple protocol */
		case S_BOOL:
		case S_BYTE:
		case S_INT:
		case S_INT16:
		case S_INT32:
		case S_INT64:
		case S_UINT:
		case S_UINT16:
		case S_UINT32:
		case S_UINT64:
		case S_REAL32:
		case S_REAL64:
		case S_FULLBARRIER:
			return TRUE;
#ifdef MOBILES
		case S_MOBILE:
			if (TagOf (MTypeOf (fprot)) == TagOf (MTypeOf (aprot))) {
				fprot = MTypeOf (fprot);
				aprot = MTypeOf (aprot);
			} else {
				return FALSE;
			}
			break;
#endif
		case S_COLON2:
			/*{{{  if left tags match, move pointers to right-hand nodes */
			if (TagOf (LeftOpOf (fprot)) == TagOf (LeftOpOf (aprot))) {
				fprot = RightOpOf (fprot);
				aprot = RightOpOf (aprot);
			} else {
				return FALSE;
			}
			break;
			/*}}} */
			/*{{{  case S_ARRAY */
		case S_ARRAY:
			{
				/* Check dimension lengths are the same */
				const INT32 l1 = ARDimOf (fprot);
				const INT32 l2 = ARDimOf (aprot);

				if ((l1 == l2) || (l1 == -1) || (l2 == -1)) {
					fprot = ARTypeOf (fprot);
					aprot = ARTypeOf (aprot);
				} else {
					return FALSE;
				}
				break;
			}
			/*}}} */
			/*}}} */
		default:
			/*chkreport_i(CHK_UNKNOWN_TYPE, chklocn, TagOf(fprot)); */
			badtag (chklocn, TagOf (fprot), "protocolsequivalent");
		}
	}
	return FALSE;
}

/*}}}*/
/*{{{  PUBLIC BOOL typesequal (t1, t2)*/
/*
This was identical to typesequivalent, except for the way it treated
CHAN OF ANY protocols. This has now been parameterised.
CON 1/2/91
*/
/*}}}*/
/*{{{  PUBLIC BOOL typesequivalent (treenode *atype, treenode *ftype, const BOOL match_any)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  typesequivalent checks that the two types 'ftype' and 'atype' are
 *                  equivalent. 'ftype' is normally the formal type (ie. type
 *                  of formal parameter, or lhs of abbreviation) , and 'atype'
 *                  is the actual type, but this routine is also called
 *                  to match assignments, etc. (was 'typesequal').
 *                  match_any will be TRUE if processing an argument list or
 *                  abbreviation, or FALSE otherwise.
 *                  Return TRUE if they are equivalent, FALSE otherwise.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC BOOL typesequivalent (treenode *atype, treenode *ftype, const BOOL match_any)
{
	if (TagOf (atype) == S_UNDECLARED || TagOf (ftype) == S_UNDECLARED) {
		return TRUE;
	}
	while (TRUE) {
#if 0
fprintf (stderr, "typesequivalent: atype: %s: ", itagstring (TagOf (atype)));
printtreenl (stderr, 4, atype);
fprintf (stderr, "ftype: %s: ", itagstring (TagOf (ftype)));
printtreenl (stderr, 4, ftype);
#endif
		if (TagOf (atype) == TagOf (ftype)) {
			switch (TagOf (atype)) {
				/*{{{  primitive types */
			case S_BOOL:
			case S_BYTE:
			case S_INT:
			case S_INT16:
			case S_INT32:
			case S_INT64:
			case S_UINT:
			case S_UINT16:
			case S_UINT32:
			case S_UINT64:
			case S_UINTLIT:
			case S_REAL32:
			case S_REAL64:
			case S_UREALLIT:
			case S_TIMER:
			case S_FULLBARRIER:
			CASE_CONFIG_TYPE
				return (TRUE);
				/*}}} */
				/*{{{  chan and port */
			case S_CHAN:
			case S_PORT:
				/* possible check for protocol inherited stuff here -- we should know at least one direction */
				if ((TagOf (ProtocolOf (atype)) == N_TPROTDEF) && (TagOf (ProtocolOf (ftype)) == N_TPROTDEF)) {
					BOOL equiv = protocolsequivalent (ProtocolOf (atype), ProtocolOf (ftype), TRUE);
					BIT32 specattrs = (TypeAttrOf (ftype) & (TypeAttr_marked_in | TypeAttr_marked_out));

					if (equiv) {
						return equiv;
					}
					/* not equivalent, see if inheritance is involved */
					if (protocolinherits (ProtocolOf (atype), ProtocolOf (ftype))) {
						/* type of actual inherited from specification
						 * eg: CHAN FOO x! IS y!:    where y is BAR and BAR EXTENDS FOO
						 * allow output channels only (specialisation) */
						if (!specattrs) {
							chkreport (CHK_EXTENDS_NODIR, chklocn);
						} else if (specattrs & TypeAttr_marked_in) {
							chkreport (CHK_EXTENDS_SPECIALISATION, chklocn);
						}
						return TRUE;
					} else if (protocolinherits (ProtocolOf (ftype), ProtocolOf (atype))) {
						/* type of specification inherited from type of actual
						 * eg: CHAN BAR y? IS x?:    where x is FOO and BAR EXTENDS FOO
						 * allow input channels only (generalisation) */
						if (!specattrs) {
							chkreport (CHK_EXTENDS_NODIR, chklocn);
						} else if (specattrs & TypeAttr_marked_out) {
							chkreport (CHK_EXTENDS_GENERALISATION, chklocn);
						}
						return TRUE;
					} else {
						return FALSE;
					}
				} else {
					return protocolsequivalent (ProtocolOf (atype), ProtocolOf (ftype), TRUE);
				}
				/*}}} */
#ifdef MOBILES
				/*{{{  any channel-type, any proc-type*/
			case S_ANYCHANTYPE:
				{
					treenode *mftype = LeafLinkOf (ftype);
					treenode *matype = LeafLinkOf (atype);

					if (mftype && matype && ((TypeAttrOf (mftype) & (TypeAttr_shared | TypeAttr_marked_in | TypeAttr_marked_out)) ^
								(TypeAttrOf (matype) & (TypeAttr_shared | TypeAttr_marked_in | TypeAttr_marked_out)))) {
						return FALSE;
					}
					return TRUE;
				}
			case S_ANYPROCTYPE:
			case S_ANYMOBILETYPE:
				/* always good */
				return TRUE;
				/*}}}*/
				/*{{{  MOBILEs */
			case S_MOBILE:
#if 0
fprintf (stderr, "typesequivalent: both MOBILE.  ARDimLengthOf(MTypeOf(atype)) =");
printtreenl (stderr, 4, ARDimLengthOf (MTypeOf (atype)));
#endif
				if ((TagOf (MTypeOf (atype)) == S_ARRAY) && (TagOf (MTypeOf (ftype)) == S_ARRAY)) {
					if (ARDimLengthOf (MTypeOf (atype)) && (TagOf (ARDimLengthOf (MTypeOf (atype))) != S_NTH_DIMENSION) && !ARDimLengthOf (MTypeOf (ftype))) {
						return FALSE;
					} else if (!ARDimLengthOf (MTypeOf (atype)) && ARDimLengthOf (MTypeOf (ftype)) && (TagOf (ARDimLengthOf (MTypeOf (ftype))) != S_NTH_DIMENSION)) {
						return FALSE;
					}
				}
#if 0
fprintf (stderr, "typesequivalent: going into MOBILE sub-types\n");
#endif
				return typesequivalent (MTypeOf (atype), MTypeOf (ftype), match_any);
				/*}}}  */
#endif
				/*{{{  S_ARRAY */
			case S_ARRAY:
				{
					/* Check dimensions are the same */
					const INT32 l1 = ARDimOf (atype);
					const INT32 l2 = ARDimOf (ftype);
					if ((l1 == l2) || (l1 == -1) || (l2 == -1)) {
						atype = ARTypeOf (atype);
						ftype = ARTypeOf (ftype);
					} else {
						return FALSE;
					}
					break;
				}
				/*}}} */
				/*{{{  type names */
#ifdef OCCAM2_5
#ifdef MOBILES
			case N_TYPEDECL:
				/* we may have copied the type to cope with a SHARED specifier for protocols,
				 * in which case they (possibly) only differ by TypeAttr -- the subtype remains the same */
				/* --> this is a bit odd, but copytree() does not copy type-trees!  (understandably) */
#if 0
fprintf (stderr, "chk1: typesequivalent(): N_TYPEDECL: atype = ");
printtreenl (stderr, 4, atype);
fprintf (stderr, "                                   : ftype = ");
printtreenl (stderr, 4, ftype);
#endif
				if (atype == ftype) {
					return TRUE;
				} else if ((NNameOf (atype) == NNameOf (ftype)) && (NTypeOf (atype) == NTypeOf (ftype)) && (NScopeOf (atype) == NScopeOf (ftype)) && (NDeclOf (atype) == NDeclOf (ftype))) {
					/* types not equivalent if there's a difference in direction or shared-ness though */
					if ((NTypeAttrOf (atype) & (TypeAttr_marked_in | TypeAttr_marked_out | TypeAttr_shared)) !=
							(NTypeAttrOf (ftype) & (TypeAttr_marked_in | TypeAttr_marked_out | TypeAttr_shared))) {
						return FALSE;
					}
					return TRUE;
				} else if ((NNameOf (atype) == NNameOf (ftype)) && (NTypeOf (atype) == NTypeOf (ftype)) && (NScopeOf (atype) != NScopeOf (ftype)) && (NDeclOf (atype) == NDeclOf (ftype))) {
					/* frmb note: we allow scopes to be different because of FORWDECL'd channel-types, but must have the same declaration once fixed-up! */
					/* types not equivalent if there's a difference in direction or shared-ness though */
					if ((NTypeAttrOf (atype) & (TypeAttr_marked_in | TypeAttr_marked_out | TypeAttr_shared)) !=
							(NTypeAttrOf (ftype) & (TypeAttr_marked_in | TypeAttr_marked_out | TypeAttr_shared))) {
						return FALSE;
					}
					return TRUE;
				}
				/* could have anonymous channel-type involvement here (TYPEDECL), need to check for inherited protocols
				 * (actually allows it acrosss disjoint CHAN TYPEs too (with the same fields, and same or inherited protocols)
				 * -- types related by the PROTOCOL inheritance, irrespective of the CHAN TYPE
				 */
				if (isdynamicmobilechantype (NTypeOf (atype)) && isdynamicmobilechantype (NTypeOf (ftype))) {
					/*{{{  check compatibility of channels inside channel type*/
					treenode *artype = ARTypeOf (MTypeOf (NTypeOf (atype)));
					treenode *frtype = ARTypeOf (MTypeOf (NTypeOf (ftype)));

					/* check for basic channel-type compatibility */
					if ((NTypeAttrOf (atype) & (TypeAttr_marked_in | TypeAttr_marked_out | TypeAttr_shared)) !=
							(NTypeAttrOf (ftype) & (TypeAttr_marked_in | TypeAttr_marked_out | TypeAttr_shared))) {
						return FALSE;
					}
					while (artype && frtype && (TagOf (artype) == S_DECL) && (TagOf (frtype) == S_DECL)) {
						treenode *aatype = NTypeOf (DNameOf (artype));
						treenode *fftype = NTypeOf (DNameOf (frtype));
						int goodtype = 0;

#if 0
fprintf (stderr, "chk1: typesequivalent(): N_TYPEDECL: aatype = ");
printtreenl (stderr, 4, aatype);
fprintf (stderr, "chk1: typesequivalent(): N_TYPEDECL: fftype = ");
printtreenl (stderr, 4, fftype);
#endif
						if (protocolsequivalent (ProtocolOf (aatype), ProtocolOf (fftype), TRUE)) {
							goodtype = 1;
						} else if ((TagOf (ProtocolOf (aatype)) == N_TPROTDEF) && (TagOf (ProtocolOf (fftype)) == N_TPROTDEF)) {
							BIT32 specattrs = (TypeAttrOf (fftype) & (TypeAttr_marked_in | TypeAttr_marked_out));

							/* if we're dealing with the client-end, turn channel direction around */
#if 0
fprintf (stderr, "chk1: typesequivalent(): NTypeAttrOf (atype) = 0x%8.8x\n", (unsigned int)NTypeAttrOf (atype));
#endif
							if (NTypeAttrOf (atype) & TypeAttr_marked_out) {
								specattrs ^= (TypeAttr_marked_in | TypeAttr_marked_out);
							}

							/* not equivalent, see if inheritance is involved */
							if (protocolinherits (ProtocolOf (aatype), ProtocolOf (fftype))) {
								/* type of actual inherited from specification
								 * eg: CHAN FOO x! IS y!:    where y is BAR and BAR EXTENDS FOO
								 * allow output channels only (specialisation) */
								if (specattrs & TypeAttr_marked_in) {
									chkreport (CHK_EXTENDS_SPECIALISATION, chklocn);
								}
								goodtype = 1;
							} else if (protocolinherits (ProtocolOf (fftype), ProtocolOf (aatype))) {
								/* type of specification inherited from type of actual
								 * eg: CHAN BAR y? IS x?:    where x is FOO and BAR EXTENDS FOO
								 * allow input channels only (generalisation) */
								if (specattrs & TypeAttr_marked_out) {
									chkreport (CHK_EXTENDS_GENERALISATION, chklocn);
								}
								goodtype = 1;
							}
						}

						if (!goodtype) {
							return FALSE;
						}
						artype = DBodyOf (artype);
						frtype = DBodyOf (frtype);
					}

					/* got here, so good */
					return TRUE;
					/*}}}*/
				}
#if 0
fprintf (stderr, "chk1: typesequivalent(): N_TYPEDECL: NNameOf(atype)=%p, NNameOf(ftype)=%p; NTypeOf(atype)=%p, NTypeOf(ftype)=%p; NScopeOf(atype)=%d, NScopeOf(ftype)=%d; NDeclOf(atype)=%p, NDeclOf(ftype)=%p\n",
		NNameOf (atype), NNameOf (ftype), NTypeOf (atype), NTypeOf (ftype), NScopeOf (atype), NScopeOf (ftype), NDeclOf (atype), NDeclOf (ftype));
#endif
				return FALSE;
			case N_PROCTYPEDECL:
#else
			case N_TYPEDECL:
#endif
			case N_DECL:	/* undeclared type name */
				return atype == ftype;
#endif
				/*}}} */
#ifdef MOBILES
				/*{{{  anonymous chan-type misuse leads to ANONCHANTYPE in here, return FALSE */
			case S_ANONCHANTYPE:
#if 0
				fprintf (stderr, "woo, here!\n");
#endif
				return FALSE;
				/*}}}*/
#endif
			default:
				/*chkreport_i(CHK_UNKNOWN_TYPE, chklocn, TagOf(atype)); */
				badtag (chklocn, TagOf (atype), "typesequivalent");
			}
#ifdef MOBILES
		} else if (TagOf (follow_user_type (atype)) == S_MOBILE) {
			if ((TagOf (ftype) == S_ANYCHANTYPE) && isdynmobilechantypetype (atype)) {
				/*{{{  allow if the formal is ANYCHANTYPE (MOBILE.CHAN) and the actual is a mobile channel-end*/
				INT32 fattr = 0;
				INT32 aattr = 0;

#if 0
fprintf (stderr, "chk1: typesequivalent(): formal ANYCHANTYPE, actual type is");
printtreenl (stderr, 4, atype);
#endif
				if (LeafLinkOf (ftype)) {
					fattr = TypeAttrOf (LeafLinkOf (ftype));
				}
				if (TagOf (atype) == N_TYPEDECL) {
					aattr = NTypeAttrOf (atype);
				}

				if ((aattr & TypeAttr_shared) ^ (fattr & TypeAttr_shared)) {
					/* difference in shared attribute */
					return FALSE;
				}
				if (fattr & (TypeAttr_marked_in | TypeAttr_marked_out)) {
					/* formal has end specified, actual must match */
					if ((aattr & (TypeAttr_marked_in | TypeAttr_marked_out)) ^ (fattr & (TypeAttr_marked_in | TypeAttr_marked_out))) {
						return FALSE;
					}
				}
				/* otherwise ok */
				return TRUE;
				/*}}}*/
			} else if ((TagOf (ftype) == S_ANYPROCTYPE) && isdynmobileproctypetype (atype)) {
				/*{{{  allow if the formal is ANYPROCTYPE (MOBILE.PROC) and the actual is a mobile process*/
				return TRUE;
				/*}}}*/
			} else if ((TagOf (ftype) == S_ANYMOBILETYPE)) {
				/*{{{  allow if the formal is ANYMOBILETYPE*/
				return TRUE;
				/*}}}*/
			}
			/* if the formal is a named type, and the actual is its content equivalent, allow this (or vice versa) */
			if ((TagOf (atype) == N_TYPEDECL) && (TagOf (ftype) != N_TYPEDECL)) {
				if (typesequivalent (follow_user_type (atype), ftype, match_any)) {
					return TRUE;
				}
			} else if ((TagOf (atype) != N_TYPEDECL) && (TagOf (ftype) == N_TYPEDECL)) {
				if (typesequivalent (atype, follow_user_type (ftype), match_any)) {
					return TRUE;
				}
			}
			/* mobile type can be passed to non-mobile equivalent */
			return typesequivalent (MTypeOf (follow_user_type (atype)), ftype, match_any);
#endif
 		} else {
			/* check for wild-card types - S_UINTLIT, S_UREALLIT */
			const int tag1 = TagOf (atype);
			const int tag2 = TagOf (ftype);

			return (((tag1 == S_UINTLIT) && isint (tag2)) ||
				((tag1 == S_UREALLIT) && isreal (tag2)) ||
				((tag2 == S_UINTLIT) && isint (tag1)) || ((tag2 == S_UREALLIT) && isreal (tag1)));
		}
	}
}
/*}}}*/
/*{{{  PUBLIC BOOL spec_into_type (specptr, typeptr)*/
/*{{{  comment*/
/* Check that the types represented by the two trees specptr and typeptr are
the same.  If there are any missing array dimensions in the specifier tree,
copy them from the type tree.
The type tree may contain wildcard types (integer or real literals) - we
must check that the specifier tree fully defines these.

Return TRUE if the trees represent equivalent types.
*/
/*}}}*/
PUBLIC BOOL spec_into_type (treenode *specptr, treenode *typeptr)
{
	/*{{{  debugging */
#if 0
fprintf (stderr, "chk1: spec_into_type: specptr is ");
printtreenl (stderr, 4, specptr);
fprintf (stderr, "chk1: spec_into_type: typeptr is ");
printtreenl (stderr, 4, typeptr);
#endif
	/*}}} */

	while (TRUE)
	{
		/*{{{  loop body */
		const int spectag = TagOf (specptr);
		const int typetag = TagOf (typeptr);

		if (spectag == typetag) {
			/*{{{  switch on the tag */
			switch (spectag) {
				/*{{{  cases */
				/*{{{  primitive types S_UNDECLARED */
			case S_BOOL:
			case S_BYTE:
			case S_INT:
			case S_INT16:
			case S_INT32:
			case S_INT64:
			case S_UINT:
			case S_UINT16:
			case S_UINT32:
			case S_UINT64:
			case S_REAL32:
			case S_REAL64:
			case S_TIMER:
			case S_UNDECLARED:
			case S_FULLBARRIER:
				CASE_CONFIG_TYPE return (TRUE);
				/*}}} */
				/*{{{  chan */
			case S_CHAN:
			case S_PORT:
				/* possible check for protocol inherited stuff here -- we should know at least one direction */
				if ((TagOf (ProtocolOf (typeptr)) == N_TPROTDEF) && (TagOf (ProtocolOf (specptr)) == N_TPROTDEF)) {
					BOOL equiv = protocolsequivalent (ProtocolOf (typeptr), ProtocolOf (specptr), TRUE);
					BIT32 specattrs = (TypeAttrOf (specptr) & (TypeAttr_marked_in | TypeAttr_marked_out));

					if (equiv) {
						return equiv;
					}
					/* not equivalent, see if inheritance is involved */
					if (protocolinherits (ProtocolOf (typeptr), ProtocolOf (specptr))) {
						/* type of actual inherited from specification
						 * eg: CHAN FOO x! IS y!:    where y is BAR and BAR EXTENDS FOO
						 * allow output channels only (specialisation) */
						if (!specattrs) {
							chkreport (CHK_EXTENDS_NODIR, chklocn);
						} else if (specattrs & TypeAttr_marked_in) {
							chkreport (CHK_EXTENDS_SPECIALISATION, chklocn);
						}
						return TRUE;
					} else if (protocolinherits (ProtocolOf (specptr), ProtocolOf (typeptr))) {
						/* type of specification inherited from type of actual
						 * eg: CHAN BAR y? IS x?:    where x is FOO and BAR EXTENDS FOO
						 * allow input channels only (generalisation) */
						if (!specattrs) {
							chkreport (CHK_EXTENDS_NODIR, chklocn);
						} else if (specattrs & TypeAttr_marked_out) {
							chkreport (CHK_EXTENDS_GENERALISATION, chklocn);
						}
						return TRUE;
					} else {
						return FALSE;
					}
				} else {
					return protocolsequivalent (ProtocolOf (typeptr), ProtocolOf (specptr), TRUE);
				}
				/*}}} */
				/*{{{  S_ARRAY */
			case S_ARRAY:
				if (ARDimLengthOf (specptr) == NULL)
					/*{{{  fill in array dimension in the specifier */
				{
					treenode *lengthexp = ARDimLengthOf (typeptr);
					const INT32 dim = ARDimOf (typeptr);
					if ((lengthexp != NULL) && (TagOf (lengthexp) == S_CONSTEXP)) {
						lengthexp = newconstant (dim);
					} else {
						/* bug 878 - enabled 30/09/91 */
						lengthexp = copytree (lengthexp, syn_lexlevel);
					}
					/* bug 878 - This should theoretically be inserted to ensure
					   that the tree remains a tree.
					   However, this might break the backend, so I'm currently
					   not enabling it.
					   CON - 28/1/91
					 */
					SetARDim (specptr, dim);
					SetARDimLength (specptr, lengthexp);
				}
				/*}}} */
				else
					/*{{{  check array lengths are the same */
				if (ARDimLengthOf (typeptr) != NULL && ARDimOf (specptr) != ARDimOf (typeptr))
					return (FALSE);
				/*}}} */

				specptr = ARTypeOf (specptr);
				typeptr = ARTypeOf (typeptr);
				break;
				/*}}} */
				/*{{{  type name */
#ifdef OCCAM2_5
			case N_TYPEDECL:
#ifdef MOBILES
			case N_PROCTYPEDECL:
				/* frmb: added check for mobile channel-types */
				if ((specptr != typeptr) && (TagOf (NTypeOf (typeptr)) == S_MOBILE)) {
					if ((NNameOf (specptr) == NNameOf (typeptr)) && (NTypeOf (specptr) == NTypeOf (typeptr))) {
						return TRUE;
					}
				}
				return (specptr == typeptr);
#else
				return specptr == typeptr;
#endif
#endif
				/*}}} */
#ifdef MOBILES
			case S_MOBILE:
				/* can abbreviate mobiles OK (alias checker will catch badness) */
				specptr = MTypeOf (specptr);
				typeptr = MTypeOf (typeptr);
				break;
#endif
			default:
				chkreport (CHK_INVTYPE_SPEC, chklocn);
			/*}}} */
			}
			/*}}} */
		} else if (((typetag == S_UINTLIT) && isint (spectag)) || ((typetag == S_UREALLIT) && isreal (spectag))) {
			/*{{{  check wildcard types */
			/* We might still have a wildcard type in the type tree, if so
			   we must have a proper type in the specifier in order to resolve it.
			 */
			return (TRUE);
			/*}}} */
		} else if ((spectag == S_UNDECLARED) || (typetag == S_UNDECLARED)) {
			/*{{{  allow through UNDECLARED types */
			return (TRUE);
			/*}}} */
		} else if ((spectag != N_TYPEDECL) && (typetag == N_TYPEDECL)) {
			/*{{{  de-user-typing?*/
			typeptr = follow_user_type (typeptr);
			/*}}}*/
		} else if ((spectag == N_TYPEDECL) && (typetag != N_TYPEDECL)) {
			/*{{{  re-user-typing?*/
			specptr = follow_user_type (specptr);
			/*}}}*/
		} else {
			return (FALSE);
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PRIVATE treenode *newcheckdop (tptr, type, return left, right, passed)*/
#ifdef USER_DEFINED_OPERATORS
PRIVATE treenode *newcheckdop (treenode * const tptr, treenode * const default_type, treenode ** left, treenode ** right, BOOL * passed)
/* This returns a tree in the 'current' workspace (normally temp) */
{
	char buf[MAX_ERR_SIZE];
	treenode *temp = NULL;
	ftagstring (buf, TagOf (tptr), NULL, 0, NULL);
	temp = checksamenew (LeftOpOf (tptr), RightOpOf (tptr), left, right, passed, default_type);
#if 0
fprintf (stderr, "newcheckdop: temp = %p, passed = %d, tptr = ", temp, *passed);
printtreenl (stderr, 4, tptr);
#endif
	return temp;
}
#endif
/*}}}*/
/*{{{  PRIVATE treenode *checkdop (tptr, default_type)*/
#ifndef USER_DEFINED_OPERATORS
PRIVATE treenode *checkdop (treenode * const tptr, treenode * const default_type)
/* This returns a tree in the 'current' workspace (normally temp) */
{
	char buf[MAX_ERR_SIZE];
	ftagstring (buf, TagOf (tptr), NULL, 0, NULL);
	return checksame (LeftOpOf (tptr), RightOpOf (tptr), default_type, CHK_TYPES_DIFF, buf);
}
#endif
/*}}}*/
/*{{{  PRIVATE BOOL typeknown (tptr)*/
PRIVATE BOOL typeknown (treenode * tptr)
{
	if (tptr == NULL)
		return FALSE;	/* bug 1120 25/1/91 */
	while (TRUE)
		switch (TagOf (tptr))
			/*{{{  cases */
		{
			/*{{{  NEG BITNOT UMINUS */
		case S_NEG:
		case S_BITNOT:
		case S_UMINUS:
			tptr = OpOf (tptr);
			break;
			/*}}} */
		#ifdef MOBILES
		case S_MOBILE:
			tptr = MTypeOf (tptr);
			break;
		#endif	/* MOBILES */
			/*{{{  dyadic op */
		case S_ADD:
		case S_SUBTRACT:
		case S_MULT:
		case S_DIV:
		case S_REM:
		case S_BITAND:
		case S_BITOR:
		case S_XOR:
		case S_LSHIFT:
		case S_RSHIFT:
		case S_PLUS:
		case S_MINUS:
		case S_TIMES:
			if (typeknown (LeftOpOf (tptr)))
				return (TRUE);
			else
				tptr = RightOpOf (tptr);
			break;
			/*}}} */
			/*{{{  ARRAYSUB */
		case S_ARRAYSUB:
			tptr = ASBaseOf (tptr);
			break;
			/*}}} */
			/*{{{  VALOF */
		case S_VALOF:
			tptr = VLResultListOf (tptr);
			/* If it's a multi-valued valof we must know its type from the
			   element on the lhs of the assignment. */
			if (listitems (tptr) > 1)
				return (TRUE);
			else
				tptr = ThisItem (tptr);
			break;
			/*}}} */
			/*{{{  SEGMENT */
		case S_SEGMENT:
			tptr = SNameOf (tptr);
			break;
			/*}}} */
			/*{{{  CONSTRUCTOR */
		case S_CONSTRUCTOR:
#ifdef OCCAM2_5
			if (LitTypeOf (tptr) != NULL)
				return TRUE;
#endif
#if TYPE_COERCION
			/*{{{  look for _any_ known elements */
			for (tptr = LitExpOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr))
				if (typeknown (ThisItem (tptr)))
					return TRUE;
			return FALSE;
			/*}}} */
#else
			/*{{{  look at the first element */
			/* Just test the first element, all the others must be of the same
			   type */
			tptr = ThisItem (LitExpOf (tptr));
			break;
			/*}}} */
#endif
			/*}}} */
			/*{{{  ARRAYCONSTRUCTOR */
		case S_ARRAYCONSTRUCTOR:
			/* Just test the first element, all the others must be of the same
			   type */
			tptr = ReplCBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  UBYTELIT UINTLIT UREALLIT */
		case S_UBYTELIT:
		case S_UINTLIT:
		case S_UREALLIT:
			return LitTypeOf (tptr) != NULL;
			/*}}} */
		default:
			if (!isspecification (tptr))
				return (TRUE);
			tptr = DBodyOf (tptr);
			break;
		}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE void type_mismatch*/
PRIVATE void type_mismatch (const treenode *const expected_type, const BOOL recover)
{
	const err_severity_t sev = recover ? SEV_ERR : SEV_ERR_JMP;

	if (typecheck_params != NULL) {
		/* INSdi02233 */
		if (!typecheck_params->fn_name) {
			msg_out (sev, CHK, CHK_TYPE_MISMATCH, chklocn);
		} else {
			msg_out_is (sev, CHK, CHK_INVTYPE_PARAM, chklocn, typecheck_params->paramno, WNameOf (typecheck_params->fn_name));
		}
	} else if (isscalartype (TagOf (expected_type))) {
		msg_out_s (sev, CHK, CHK_TYPE_MISMATCH_EXPECTED, chklocn, tagstring (TagOf (expected_type)));
	} else {		/* we're not sure what to say, so say nothing about what type is expected */
		msg_out (sev, CHK, CHK_TYPE_MISMATCH, chklocn);
	}
	return;
}

/*}}}*/

/*{{{  User defined operator specific routines*/
#ifdef USER_DEFINED_OPERATORS
/*{{{  PRIVATE treenode *replace_resolved_lit_type(treenode *tptr, typetree)*/
PRIVATE treenode *replace_resolved_lit_type (treenode *op_tree, treenode *typenode)
{
	if (!op_tree || !typenode) {
		return undeclaredp;
	}
	if (((TagOf (op_tree) == S_UBYTELIT) || (TagOf (op_tree) == S_UINTLIT)) && LitUnspecOf (op_tree)) {
		treenode *const simplest_type = (TagOf (op_tree) == S_UBYTELIT) ? bytenodeptr : intnodeptr;
		return simplest_type;
	} else {
		if ((TagOf (op_tree) == S_UREALLIT) && LitUnspecOf (op_tree)) {
			return real32nodeptr;
		} else {
#ifdef MOBILES
			if (TagOf (typenode) == S_MOBILE) {
				return MTypeOf (typenode);
			}
#endif
			return typenode;
		}
	}
}

/*}}}*/
/*{{{  PRIVATE void replace_resolved_lit_type_in_node(treenode *tptr)*/
PRIVATE void replace_resolved_lit_type_in_node (treenode * op_tree)
{
	if (((TagOf (op_tree) == S_UBYTELIT) || (TagOf (op_tree) == S_UINTLIT)) && LitUnspecOf (op_tree)) {
		treenode *const simplest_type = (TagOf (op_tree) == S_UBYTELIT) ? bytenodeptr : intnodeptr;
		SetLitType (op_tree, simplest_type);
	}
	if ((TagOf (op_tree) == S_UREALLIT) && LitUnspecOf (op_tree)) {
		SetLitType (op_tree, real32nodeptr);
	}
}

/*}}}*/
/*{{{  PRIVATE void post_udo_error_check_on_literals(treenode *tptr, default_type)*/
PRIVATE void post_udo_error_check_on_literals (treenode * op_tree, treenode * default_type)
{
	if ((TagOf (op_tree) == S_UBYTELIT) || (TagOf (op_tree) == S_UINTLIT)) {
		treenode *littype = LitTypeOf (op_tree);
		/*{{{  check it is a valid type for a literal */
		switch (TagOf (follow_user_type (littype))) {
		case S_BYTE:
		case S_INT:
		case S_INT16:
		case S_INT32:
		case S_INT64:
		case S_UINT:
		case S_UINT16:
		case S_UINT32:
		case S_UINT64:
		case N_DECL:	/* undeclared user type */
			break;
		default:
			if (LitTypeOf (op_tree) == NULL)
				type_mismatch (default_type, FALSE);
			else
				chkreport (CHK_INV_TYPE_DECORATION, chklocn);
			break;
		}
		/*}}} */
	}
	if (TagOf (op_tree) == S_UREALLIT) {
		treenode *littype = LitTypeOf (op_tree);
		/*{{{  check it is a valid type for a literal */
		switch (TagOf (follow_user_type (littype))) {
		case S_REAL32:
		case S_REAL64:
		case N_DECL:	/* undeclared user type */
			break;
		default:
			if (LitTypeOf (op_tree) == NULL)
				type_mismatch (default_type, FALSE);
			else
				chkreport (CHK_INV_TYPE_DECORATION, chklocn);
			break;
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PRIVATE treenode *get_return_type(treenode *tptr)*/
PRIVATE treenode *get_return_type (treenode * tptr)
{
	treenode *temp = (ThisItem (FnTypeListOf (NTypeOf (INameOf (tptr)))));
	return temp;
}

/*}}}*/
/*{{{  PRIVATE char *generate_typename_strings(treenode *typetree, int *length)*/
/* This function takes a typetree and returns a string (and updates length) */
/* containing a textual version of the type ie S_INT -> "INT" */
/* or if user defined type then the name returned.  Used to generate */
/* name matches when searching for user defined operators */
PRIVATE char *generate_typename_strings (treenode * typetree, int *length)
{
	/*{{{  constant declarations of type strings */
	static const char *integer = "INT";
	static const char *integer16 = "INT16";
	static const char *integer32 = "INT32";
	static const char *integer64 = "INT64";
	static const char *uinteger = "UINT";
	static const char *uinteger16 = "UINT16";
	static const char *uinteger32 = "UINT32";
	static const char *uinteger64 = "UINT64";
	static const char *real32 = "REAL32";
	static const char *real64 = "REAL64";
	static const char *boolean = "BOOL";
	static const char *byte = "BYTE";
	static const char *nullptr = "NULL.POINTER";	/* scared to remove there for a reason */
	/*}}} */

	char *string_to_add;
	int length_of_addition = 0;
	int index = 0;
	/*{{{  if null pointer - yuck! */
	if (typetree == NULL) {
		*length = 12;
		return (char *) (nullptr);
	}
	/*}}} */

	switch (TagOf (typetree)) {
		/*{{{  switch on types */
		/*{{{  case N_TYPEDECL, N_PROCTYPEDECL, N_FIELD, N_DECL */
	case N_TYPEDECL:
#ifdef MOBILES
	case N_PROCTYPEDECL:
#endif
	case N_FIELD:
	case N_DECL:
		string_to_add = (char *) WNameOf ((wordnode *) NNameOf (typetree));
		length_of_addition = WLengthOf ((wordnode *) NNameOf (typetree));
		break;
		/*}}} */
		/*{{{  case U_INTLIT, S_INT */
	case S_UINTLIT:
	case S_INT:
		string_to_add = (char *) integer;
		length_of_addition = 3;
		break;
		/*}}} */
		/*{{{  case S_UINT*/
	case S_UINT:
		string_to_add = (char *)uinteger;
		length_of_addition = 4;
		break;
		/*}}}*/
		/*{{{  case S_INT16 */
	case S_INT16:
		string_to_add = (char *) integer16;
		length_of_addition = 5;
		break;
		/*}}} */
		/*{{{  case S_UINT16 */
	case S_UINT16:
		string_to_add = (char *) uinteger16;
		length_of_addition = 6;
		break;
		/*}}} */
		/*{{{  case S_INT32 */
	case S_INT32:
		string_to_add = (char *) integer32;
		length_of_addition = 5;
		break;
		/*}}} */
		/*{{{  case S_UINT32 */
	case S_UINT32:
		string_to_add = (char *) uinteger32;
		length_of_addition = 6;
		break;
		/*}}} */
		/*{{{  case S_INT64 */
	case S_INT64:
		string_to_add = (char *) integer64;
		length_of_addition = 5;
		break;
		/*}}} */
		/*{{{  case S_UINT64 */
	case S_UINT64:
		string_to_add = (char *) uinteger64;
		length_of_addition = 6;
		break;
		/*}}} */
		/*{{{  case S_BOOL */
	case S_BOOL:
		string_to_add = (char *) boolean;
		length_of_addition = 4;
		break;
		/*}}} */
		/*{{{  csae S_UBYTELIT, S_BYTE */
	case S_UBYTELIT:
	case S_BYTE:
		string_to_add = (char *) byte;
		length_of_addition = 4;
		break;		/* bugfix by Jim 3/2/97 */
		/*}}} */
		/*{{{  case S_UREALLIT, S_REAL32 */
	case S_UREALLIT:
	case S_REAL32:
		string_to_add = (char *) real32;
		length_of_addition = 6;
		break;
		/*}}} */
		/*{{{  case S_REAL64 */
	case S_REAL64:
		string_to_add = (char *) real64;
		length_of_addition = 6;
		break;
		/*}}} */
		/*{{{  COMMENT case S_UNKNOWN */
		/*case S_UNKNOWN: */
		/* Add error handling */
		/*string_to_add = "UNKNOWN"; */
		/*length_of_addition = 7; */
		/*break; */
		/*}}} */
		/*{{{  COMMENT case S_UNDECLARED */
		/*case S_UNDECLARED: */
		/* is this ok?? */
		/*string_to_add = "UNDECLARED"; */
		/*length_of_addition = 10; */
		/*break; */
		/*}}} */
		/*{{{  COMMENT case S_CHAN */
		/*case S_CHAN: */
		/* is this ok?? */
		/*string_to_add = "CHAN"; */
		/*length_of_addition = 4; */
		/*break; */
		/*}}} */
		/*{{{  case S_ARRAY (more complex) */
	case S_ARRAY:{
			int templength = 0;	/*int newtemp = 0; */
			char *tempstr;	/* part of bug fix */
			/* BUG FIX 201198 - see syn2.c for details */
			string_to_add = (char *) memalloc (256);
			for (index = 0; index < 256; string_to_add[index++] = 0);
			string_to_add = strncpy (string_to_add, "ARRAY.OF.", 9);
			templength += 9;
			/* All arrays treated as open now.... Jim 18/12/96 */
			tempstr = generate_typename_strings (ARTypeOf (typetree), &length_of_addition);	/* part of bug fix */
			string_to_add = strncat (string_to_add, tempstr, length_of_addition);
			length_of_addition += templength;
			break;
		}
		/*}}} */
		/*{{{  default leads to error report */
	default:
		string_to_add = "INVALID.TYPE";
		length_of_addition = 12;
		break;
		/*{{{  COMMENT used to generate error report - bug found by student (see RCS log) */
		/*printf("INTERNAL COMPILER ERROR - unknown base type in generate_typename_strings\n"); */
		/*printf("Value of invalid tag is:%d\n",TagOf(typetree)); */
		/*printtree(stdout,0,typetree); */
		/*printf("Contact kroc-bugs@kent.ac.uk\n"); */
		/*exit(1); */
		/*break; */
		/*}}} */
		/*}}} */
		/*}}} */
	}
	*length = length_of_addition;
	return (string_to_add);
}

/*}}}*/

/*{{{  forward declaration*/
/* forward type definition */
PRIVATE treenode *typecheck_main (treenode * tptr, treenode * const default_type);
/*}}}*/

/*{{{  User defined operator function name constructor functions*/

/*{{{  PRIVATE char *construct_name(default_type, tptr, length(returned))*/
/*{{{  COMMENT*/
/*This routine takes a tree containing a dop node with two subtree's*/
 /*and constructs a string from these three, together with the operator */
 /*(which is also in the dop node).  Form is: */
  /*udo.<string mapped from symbol>.<param1 type>.<param2 type> */
 /*Added by Jim Moores 11/96 */
 /**/
/*IMPORTATANT NOTE: the types of the parameters are taken from the global*/
/*variables left_type and right_type, which are set in newcheckdop - this is so*/
/*that typecheck_main does not need to be called more than once (it has side*/
/*effects on the type tree so it cannot be called more than once).*/
/*}}}*/
PRIVATE char *construct_name (treenode * default_type, treenode * tptr, int *length)
{
	int typenamelen = 0;
	int addition_len = 0;
	int index = 0;
	treenode *temp_type;
	char *name = NULL;
	char *tempstr;		/* BUG FIX 201198 see syn2.c */
	const int old = switch_to_real_workspace ();

	/*{{{  allocate memory (malloc) and zero it. */
	name = (char *) memalloc (256);
	for (index = 0; index < 256; name[index++] = 0);
	/*}}} */
	switch (TagOf (tptr)) {
		/*{{{  switch on operator symbols to generate mapped strings */
	case S_ADD:
		name = strcpy (name, "udo.ADD.\0");
		break;
	case S_SUBTRACT:
		name = strcpy (name, "udo.SUB.\0");
		break;
	case S_MULT:
		name = strcpy (name, "udo.MUL.\0");
		break;
	case S_DIV:
		name = strcpy (name, "udo.DIV.\0");
		break;
	case S_REM:
		name = strcpy (name, "udo.REM.\0");
		break;
	case S_PLUS:
		name = strcpy (name, "udo.PLU.\0");
		break;
	case S_MINUS:
		name = strcpy (name, "udo.MIN.\0");
		break;
	case S_TIMES:
		name = strcpy (name, "udo.TIM.\0");
		break;
	case S_AFTER:
		name = strcpy (name, "udo.AFT.\0");
		break;
	case S_BITAND:
		name = strcpy (name, "udo.BAN.\0");
		break;
	case S_BITOR:
		name = strcpy (name, "udo.BOR.\0");
		break;
	case S_OR:
		name = strcpy (name, "udo.ORo.\0");
		break;
	case S_AND:
		name = strcpy (name, "udo.AND.\0");
		break;
	case S_XOR:
		name = strcpy (name, "udo.XOR.\0");
		break;
	case S_LSHIFT:
		name = strcpy (name, "udo.LSH.\0");
		break;
	case S_RSHIFT:
		name = strcpy (name, "udo.RSH.\0");
		break;
	case S_EQ:
		name = strcpy (name, "udo.EQA.\0");
		break;
	case S_NE:
		name = strcpy (name, "udo.NEQ.\0");
		break;
	case S_LS:
		name = strcpy (name, "udo.LTH.\0");
		break;
	case S_LE:
		name = strcpy (name, "udo.LTE.\0");
		break;
	case S_GR:
		name = strcpy (name, "udo.GRT.\0");
		break;
	case S_GE:
		name = strcpy (name, "udo.GTE.\0");
		break;
	case S_ROTATELEFT:
		name = strcpy (name, "udo.RTL.\0");
		break;
	case S_ROTATERIGHT:
		name = strcpy (name, "udo.RTR.\0");
		break;

	case S_DQUESTIONM:
		name = strcpy (name, "udo.QMQ.\0");
		break;
	case S_DAMPERSAT:
		name = strcpy (name, "udo.ATA.\0");
		break;
	case S_DDOLLAR:
		name = strcpy (name, "udo.DOD.\0");
		break;
	case S_PERCENT:
		name = strcpy (name, "udo.PER.\0");
		break;
	case S_DPERCENT:
		name = strcpy (name, "udo.PPR.\0");
		break;
	case S_DAMPERSAND:
		name = strcpy (name, "udo.ANA.\0");
		break;
	case S_LEFTPERCENT:
		name = strcpy (name, "udo.LPR.\0");
		break;
	case S_PERCENTRIGHT:
		name = strcpy (name, "udo.PRR.\0");
		break;
	case S_LEFTAMPERSAND:
		name = strcpy (name, "udo.LAN.\0");
		break;
	case S_AMPERSANDRIGHT:
		name = strcpy (name, "udo.ANR.\0");
		break;
	case S_DPLUS:
		name = strcpy (name, "udo.PLP.\0");
		break;
	case S_DMINUS:
		name = strcpy (name, "udo.MIM.\0");
		break;
	case S_HAT:
		name = strcpy (name, "udo.HAT.\0");
		break;
	case S_DEXCLAIMATION:
		name = strcpy (name, "udo.EXE.\0");
		break;
	case S_DEQUALS:
		name = strcpy (name, "udo.EQE.\0");
		break;
	case S_LEFTAMPERSAT:
		name = strcpy (name, "udo.LAT.\0");
		break;
	case S_AMPERSATRIGHT:
		name = strcpy (name, "udo.ATR.\0");
		break;
	case S_AMPERSAT:
		name = strcpy (name, "udo.AMA.\0");
		break;
	default:
		name = strcpy (name, "udo.ERR.\0");
		break;
		printf ("INTERNAL COMPILER ERROR - contact kroc-bugs@kent.ac.uk\n");
		printf ("report as error in construct_name, in chk1.c - Unsupported operator in switch\n");
		exit (1);
		/*}}} */
	}
	typenamelen += 8;
	temp_type = replace_resolved_lit_type (LeftOpOf (tptr), left_type);
	/* BUG FIX 201198, evaluate generate... on a separate line */
	tempstr = generate_typename_strings (temp_type, &addition_len);
	name = strncat (name, tempstr, addition_len);	/* Jim */
	typenamelen += addition_len;
	name = strncat (name, ".", 1);
	typenamelen++;
	temp_type = replace_resolved_lit_type (RightOpOf (tptr), right_type);
	/* BUG FIX 201198, evaluate generate... on a separate line */
	tempstr = generate_typename_strings (temp_type, &addition_len);
	name = strncat (name, tempstr, addition_len);
	typenamelen += addition_len;
	*length = typenamelen;
	switch_to_prev_workspace (old);
	return name;
}

/*}}}*/
/*{{{  PUBLIC char *construct_mop_name(default_type, tptr, length(returned))*/
/*{{{  COMMENT*/
/*	This routine takes a tree containing a mop node with one type subtree
 *	and constructs a string from these three, together with the operator
 *	(which is also in the mop node).  Form is:
 *	udo.<string mapped from symbol>.<param1 type>.<param2 type> 
 *	Added by Jim Moores 11/96
 *	Made PUBLIC by Fred, 11/2000
 */
/*}}}*/

PUBLIC char *construct_mop_name (treenode *default_type, treenode *tptr, int *length)
{
	int typenamelen = 0;
	int addition_len = 0;
	int index = 0;
	char *name = NULL;
	char *tempstr;		/* BUG FIX 201198 see syn2.c */
	treenode *temp_type;

	/*{{{  allocate memory to name pointer and zero it */
	name = (char *) memalloc (256);
	/* for (index = 0; index < 256; name[index++]); 	-- Fred: name[] removed */
	for (index = 0; index < 256; index++);
	/*}}} */
	switch (TagOf (tptr)) {
		/*{{{  switch on operator symbols to generate strings */
	case S_NEG:
		name = strcpy (name, "udo.SUB.\0");
		break;
	case S_NOT:
		name = strcpy (name, "udo.NOT.\0");
		break;
	case S_BITNOT:
		name = strcpy (name, "udo.BNT.\0");
		break;
	case S_UMINUS:
		name = strcpy (name, "udo.MIN.\0");
		break;
		/* new operators */
	case S_M_DQUESTIONM:
		name = strcpy (name, "udo.QMQ.\0");
		break;
	case S_M_DAMPERSAT:
		name = strcpy (name, "udo.ATA.\0");
		break;
	case S_M_DDOLLAR:
		name = strcpy (name, "udo.DOD.\0");
		break;
	case S_M_PERCENT:
		name = strcpy (name, "udo.PER.\0");
		break;
	case S_M_DPERCENT:
		name = strcpy (name, "udo.PPR.\0");
		break;
	case S_M_DAMPERSAND:
		name = strcpy (name, "udo.ANA.\0");
		break;
	case S_M_LEFTPERCENT:
		name = strcpy (name, "udo.LPR.\0");
		break;
	case S_M_PERCENTRIGHT:
		name = strcpy (name, "udo.PRR.\0");
		break;
		/* should put <&, &> in here ?? */
	case S_M_LEFTAMPERSAND:
		name = strcpy (name, "udo.LAN.\0");
		break;
	case S_M_AMPERSANDRIGHT:
		name = strcpy (name, "udo.ANR.\0");
		break;
	case S_M_DPLUS:
		name = strcpy (name, "udo.PLP.\0");
		break;
	case S_M_DMINUS:
		name = strcpy (name, "udo.MIM.\0");
		break;
	case S_M_HAT:
		name = strcpy (name, "udo.HAT.\0");
		break;
	case S_M_DEXCLAIMATION:
		name = strcpy (name, "udo.EXE.\0");
		break;
	case S_M_DEQUALS:
		name = strcpy (name, "udo.EQE.\0");
		break;
	case S_M_LEFTAMPERSAT:
		name = strcpy (name, "udo.LAT.\0");
		break;
	case S_M_AMPERSATRIGHT:
		name = strcpy (name, "udo.ATR.\0");
		break;
	case S_M_AMPERSAT:
		name = strcpy (name, "udo.AMA.\0");
		break;
	case S_M_ROTATERIGHT:
		name = strcpy (name, "udo.RTR.\0");
		break;
	case S_M_ROTATELEFT:
		name = strcpy (name, "udo.RTL.\0");
		break;
	default:
		name = strcpy (name, "ERR.\0");
		break;
		/* the following is never reached */
		printf ("INTERNAL COMPILER ERROR - contact kroc-bugs@kent.ac.uk\n");
		printf ("report as error in construct_mop_name, in chk1.c - Unsupported operator in switch\n");
		exit (1);
		/*}}} */
	}

	typenamelen += 8;
	temp_type = replace_resolved_lit_type (OpOf (tptr), left_type);
	/* BUG FIX 201198 */
	tempstr = generate_typename_strings (temp_type, &addition_len);
	name = strncat (name, tempstr, addition_len);	/* Jim */
	typenamelen += addition_len;
	*length = typenamelen;
	return name;
}

/*}}}*/
/*}}}*/
/*{{{  Routines to convert operator nodes into function call nodes*/
/*{{{  PRIVATE void trans_mop_to_finstance (tptr)*/
PRIVATE void trans_mop_to_finstance (treenode * tptr, treenode * name)
{
	treenode *operand = NULL;
	treenode *listtemp;
	const int old = switch_to_real_workspace ();

#if 0
fprintf (stderr, "trans_mop_to_finstance (in).  tptr =");
printtreenl (stderr, 4, tptr);
#endif
	operand = OpOf (tptr);
	replace_resolved_lit_type_in_node (operand);
	listtemp = newlistnode (S_LIST, NOPOSN, operand, NULL);
	switch_to_prev_workspace (old);

	SetTag (tptr, S_FINSTANCE);
	SetIName (tptr, name);
	SetIParamList (tptr, listtemp);
	/* SetDOpType (tptr, 0);*/	/* clear field as 4 bytes, probably not necessary -- frmb: BROKEN! */
	SetILoadSeq (tptr, 0);
	SetIRecursive (tptr, 0);
	SetIForked (tptr, 0);
	SetIDynmem (tptr, 0);
	SetIDynaddr (tptr, 0);
	SetNUsed (INameOf (tptr), TRUE);
#if 0
fprintf (stderr, "trans_mop_to_finstance (out).  tptr =");
printtreenl (stderr, 4, tptr);
#endif
	return;
}

/*}}}*/
/*{{{  PRIVATE void trans_dop_to_finstance (tptr)*/
PRIVATE void trans_dop_to_finstance (treenode * tptr, treenode * name)
{
	treenode *left = NULL;
	treenode *right = NULL;
	treenode *listtemp;
	/*int operator_val = 0; */
	const int old = switch_to_real_workspace ();

	memfree (memalloc (512));
#if 0
fprintf (stderr, "trans_dop_to_finstance (in).  tptr =");
printtreenl (stderr, 4, tptr);
#endif
	left = LeftOpOf (tptr);
	right = RightOpOf (tptr);
	/*operator_val = DOpTypeOf(tptr); */
#if 0
fprintf (stderr, "trans_dop_to_finstance.  about to check malloc then replace_resolved_lit_type_in_node (left)\n");
memfree (memalloc (512));
#endif
	replace_resolved_lit_type_in_node (left);
#if 0
fprintf (stderr, "trans_dop_to_finstance.  about to check malloc then replace_resolved_lit_type_in_node (right)\n");
memfree (memalloc (512));
#endif
	replace_resolved_lit_type_in_node (right);
#if 0
fprintf (stderr, "trans_dop_to_finstance.  about to check malloc then allocate list node with left and right\n");
memfree (memalloc (512));
#endif
	listtemp = newlistnode (S_LIST, NOPOSN, left, newlistnode (S_LIST, NOPOSN, right, NULL));
#if 0
fprintf (stderr, "trans_dop_to_finstance.  about to check malloc then switch_to_prev_workspace(old)\n");
memfree (memalloc (512));
#endif
	switch_to_prev_workspace (old);
#if 0
fprintf (stderr, "trans_dop_to_finstance.  back to previous workspace now :), checking memalloc..\n");
memfree (memalloc (512));
fprintf (stderr, "trans_dop_to_finstance.  back to previous workspace now :), checking memalloc..\n");
#endif

	SetTag (tptr, S_FINSTANCE);
	SetIName (tptr, name);
	SetIParamList (tptr, listtemp);
	/* SetDOpType (tptr, 0); */	/* clear field as 4 bytes */
	SetILoadSeq (tptr, 0);
	SetIRecursive (tptr, 0);
	SetIForked (tptr, 0);
	SetIDynmem (tptr, 0);
	SetIDynaddr (tptr, 0);
	SetNUsed (INameOf (tptr), TRUE);
	memfree (memalloc (512));
#if 0
fprintf (stderr, "trans_dop_to_finstance (out).  tptr =");
printtreenl (stderr, 4, tptr);
#endif
	/*scopeandcheck(&tptr);
	   typecheck_main(tptr, default_type); */
}

/*}}}*/
/*}}}*/
#endif		/* USER_DEFINED_OPERATORS */
/*}}}*/

/*{{{  PRIVATE void mangle_tree_for_arrayconstructor (treenode *target, treenode *type, treenode *rname, treenode *start, ...)*/
/*
 *	void mangle_tree_for_arrayconstructor (treenode *target, treenode *type, treenode *rname, treenode *start,
 *		treenode *length, treenode *step, treenode *body, treenode *vnptr, treenode *vniptr)
 *	mangles tree to turn ARRAYCONSTRUCTOR node into a VALOF process
 */
PRIVATE void mangle_tree_for_arrayconstructor (treenode *target, treenode *type, treenode *rname, treenode *start,
					       treenode *length, treenode *step, treenode *body, treenode *vnptr, treenode *vniptr)
{
	int old = switch_to_real_workspace ();
	SOURCEPOSN deflocn = LocnOf (target);
	treenode *arrayidx, *assnode, *replnode, *resultlist, *valofnode;
	treenode *vnidecl;

#if 0
fprintf (stderr, "mangle_tree_for_arrayconstructor:.. stuff follows ..\n*** target = ");
printtreenl (stderr, 4, target);
fprintf (stderr, "type = ");
printtreenl (stderr, 4, type);
fprintf (stderr, "rname = ");
printtreenl (stderr, 4, rname);
fprintf (stderr, "start = ");
printtreenl (stderr, 4, start);
fprintf (stderr, "length = ");
printtreenl (stderr, 4, length);
fprintf (stderr, "step = ");
printtreenl (stderr, 4, step);
fprintf (stderr, "body = ");
printtreenl (stderr, 4, body);
fprintf (stderr, "vnptr (NLexLevelOf = %d) = ", NLexLevelOf (vnptr));
printtreenl (stderr, 4, vnptr);
fprintf (stderr, "vniptr (NLexLevelOf = %d) = ", NLexLevelOf (vniptr));
printtreenl (stderr, 4, vniptr);
#endif

#if 0
fprintf (stderr, "mangle_tree_for_arrayconstructor: start =");
printtreenl (stderr, 4, start);
fprintf (stderr, "mangle_tree_for_arrayconstructor: foldexp (start) =");
printtreenl (stderr, 4, foldexp (start));
#endif

	/* build value process */
	resultlist = newlistnode (S_LIST, deflocn, vnptr, NULL);
	arrayidx = newarraysubnode (S_ARRAYSUB, deflocn, vnptr, vniptr);
	assnode = newactionnode (S_ASS, deflocn, arrayidx, copytree (body, NLexLevelOf (vnptr)));

	/* "rname" has the user's relicator name, "vniptr" has our temporary name.  each are linked to their "right" decls */
	vnidecl = NDeclOf (vniptr);
	SetDVal (vnidecl, newdopnode (S_ADD, deflocn, (step ?
			newdopnode (S_MULT, deflocn, vniptr, step, S_INT)
			: vniptr), start, S_INT));
	SetDName (vnidecl, rname);
	SetDBody (vnidecl, assnode);
	SetTag (rname, N_VALABBR);
	SetTag (vnidecl, S_VALABBR);
	SetNDecl (rname, vnidecl);

	replnode = newreplcnode (S_REPLSEQ, deflocn, vniptr, newconstant (0), length, NULL, vnidecl);
	valofnode = newvalofnode (S_VALOF, deflocn, replnode, resultlist);

#if 0
fprintf (stderr, "mangle_tree_for_arrayconstructor: vnptr = ");
printtreenl (stderr, 4, vnptr);
fprintf (stderr, "    \"      \"                    : NDeclOf(vnptr) = ");
printtreenl (stderr, 4, NDeclOf (vnptr));
fprintf (stderr, "mangle_tree_for_arrayconstructor: vniptr = ");
printtreenl (stderr, 4, vniptr);
fprintf (stderr, "    \"      \"                    : NDeclOf(vniptr) = ");
printtreenl (stderr, 4, NDeclOf (vniptr));
#endif

	SetNType (vnptr, copytree (type, 0));

	SetTag (target, S_DECL);
	SetDName (target, vnptr);
	SetDVal (target, NULL);
	SetDBody (target, valofnode);

	SetNDecl (vniptr, replnode);
	SetTag (vniptr, N_REPL);
	SetNDecl (vnptr, target);

#if 0
fprintf (stderr, "mangle_tree_for_arrayconstuctor: new process =");
printtreenl (stderr, 4, target);
#endif
	switch_to_prev_workspace (old);
#if 0
fprintf (stderr, "mangle_tree_for_arrayconstructor: DValOf (vnidecl) = ");
printtreenl (stderr, 4, DValOf (vnidecl));
#endif
	return;
}
/*}}}*/

/*{{{  PUBLIC treenode *typecheck_main (tptr, default_type)*/
/*{{{  comment*/
/* Check that the types of expression operands are consistent.
Place type of operands in field of operator nodes.
tptr points to the root of the expression tree.
The value returned represents the type of the expression tree.
Insert the types of untyped literals.
*/
/* N.B. This function returns a pointer to a type tree which already
exists, so the return value should only be used for  comparsion and
not for inserting elsewhere on the tree.
To insert a type tree elsewhere, first copy it using function
'copytree'.
*/
/* default_type is a type tag used to resolve the type of
unttyped integer and real literals where possible.
N.B. TYPE COERCION IS CURRENTLY DISABLED
If it takes an incompatible type an error will
be reported.
If default_type is UNKNOWN and we find an untyped integer literal we assume the type
INT.
If default_type is UNKNOWN and we find an untyped real literal we report an error.
*/
/*}}}*/
PRIVATE treenode *typecheck_main (treenode *tptr, treenode *const default_type)
{
	treenode *t;		/* Type of the expression tree */
	int primitive_type;
	#ifdef USER_DEFINED_OPERATORS
		BOOL modify_check_for_udo = modify_check;	/* copy value to local */
		modify_check = FALSE;	/* prevents modify_check propagating in recursion */
		/* to typecheck_main() subexpressions */
	#endif
	if (tptr == NULL) {
		return undeclaredp;	/* bug 838 20/12/90 */
	}

#if 0
fprintf (stderr, "typecheck_main(): tptr =");
printtreenl (stderr, 4, tptr);
#endif
	switch (TagOf (tptr)) {
		/*{{{  S_ASINPUT, S_ASOUTPUT*/
	case S_ASINPUT:
	case S_ASOUTPUT:
		return undeclaredp;
		/*}}}*/
		/*{{{  monadic operators */
		/*{{{  case S_NEG */
	case S_NEG:
		{
			/*{{{  user defined operator code */
			#ifdef USER_DEFINED_OPERATORS
				int tempnamelen = 0;
				char *tempname = NULL;

				modify_check = TRUE;
				t = typecheck_main (OpOf (tptr), default_type);
				left_type = t;
				/* build function name */
				/*{{{  build function name (with literals resolving to int or byte) */
				{
					tempname = construct_mop_name (default_type, tptr, &tempnamelen);
				}
#if 0
fprintf (stderr, "typecheck_main(): S_NEG: checking for UDO tempname = [%s]\n", tempname);
#endif
				/*}}} */
				/* call searchforword so that it doesn't create a new entry in symbol table */
				if (user_defined_operators && searchforword (tempname, tempnamelen)) {
					if ((findname (lookupword (tempname, tempnamelen)) != NULL) && (user_defined_operators)) {
						/*{{{  function declared and in scope, transform to function and free mem. */
						/* Function declared and in scope (on scope stack) */
						/* so transform dop node to finstance node */
						trans_mop_to_finstance (tptr, findname (lookupword (tempname, tempnamelen)));
						memfree (tempname);
						/* the return type is exactly what was asked for *//* _NOT_ */
						/* return type should be return type of function substituted */
						return (get_return_type (tptr));
						/*}}} */
					}
					/* function delcared but not in scope - so free memory for string */
					memfree (tempname);
				} else {
					/*{{{  function not declared - generate error if types do not match. */
					memfree (tempname);
					if (t == NULL) {
						/*{{{  COMMENT types do not match */
						/*This bit is what would have been done by checkdop if we hadn't generated a */
						/*modified copy called newcheckdop - this is because we wan't to be able to */
						/*check for user defined operators if the types don't match - and generate the */
						/*error later if a user defined operator matching those types (and operator) */
						/*is not found. */
						/*}}} */
						char buf[MAX_ERR_SIZE];
						ftagstring (buf, TagOf (tptr), NULL, 0, NULL);
						chkreport_s (CHK_TYPES_DIFF, chklocn, buf);
						/*printf("whoops, an error occurred!\n");  subst in error call */
					}
					/*}}} */
				}
				/*{{{  operators not overloaded so perform full check on literals we fudged b4 */
				post_udo_error_check_on_literals (OpOf (tptr), default_type);
				/*}}} */
			#else
				/*}}} */
				/*{{{  normal code if user defined operators not enabled (by macro) */
				t = typecheck_main (OpOf (tptr), default_type);
			#endif
			/*}}} */
			/*{{{  normal code which is fallen through to by one of the above */
			primitive_type = TagOf (follow_user_type (t));
			switch (primitive_type) {
			case S_INT:
			case S_INT16:
			case S_INT32:
			case S_INT64:
			case S_UINT:
			case S_UINT16:
			case S_UINT32:
			case S_UINT64:
			case S_REAL32:
			case S_REAL64:
			case S_UNDECLARED:
				break;
			default:
				t = undeclaredp;	/* bug 1279 22/8/91 */
				primitive_type = S_UNDECLARED;
				chk_invtype (chklocn, S_NEG);
				break;
			}
			SetMOpType (tptr, primitive_type);
			return (t);
			/*}}} */
		}		/* end case */
		/*}}} */
		/*{{{  case S_BITNOT S_UMINUS */
	case S_BITNOT:
	case S_UMINUS:
		{
			/*{{{  user defined operator code */
			#ifdef USER_DEFINED_OPERATORS
				int tempnamelen = 0;
				char *tempname = NULL;

				modify_check = TRUE;
				t = typecheck_main (OpOf (tptr), default_type);
				left_type = t;	/* bugfix by Jim - allows construct_mop_name to "know" the type of the operand */
				/*{{{  build function name (with literals resolving to int or byte) */
				{
					tempname = construct_mop_name (default_type, tptr, &tempnamelen);
				}
				/*}}} */
				if (user_defined_operators && searchforword (tempname, tempnamelen))
					/* name in symbol table */
				{
					if (findname (lookupword (tempname, tempnamelen)) != NULL) {
						/*{{{  function delcared and on scope stack so insert function call */
						/* Function declared and in scope (on scope stack) */
						/* so transform dop node to finstance node */
						trans_mop_to_finstance (tptr, findname (lookupword (tempname, tempnamelen)));
						memfree (tempname);
						/* the return type is exactly what was asked for */
						return (get_return_type (tptr));
						/*}}} */
					}
					/* free up memory - function defined but not in scope */
					memfree (tempname);
				} else {
					/*{{{  function not there - generate type error if necessary */
					memfree (tempname);
					if (t == NULL) {
						char buf[MAX_ERR_SIZE];
						ftagstring (buf, TagOf (tptr), NULL, 0, NULL);
						chkreport_s (CHK_TYPES_DIFF, chklocn, buf);
						/*printf("whoops, an error occurred!\n");  subst in error call */
					}
					/*}}} */
				}
				/*{{{  operators not overloaded so perform full check on literals we fudged b4 */
				post_udo_error_check_on_literals (OpOf (tptr), default_type);
				/*}}} */
			#else
				/*}}} */
				/*{{{  alternative code if user defined operators turn off (as occ2.1) */
				t = typecheck_main (OpOf (tptr), default_type);
			#endif
			/*}}} */
			/*{{{  normal occ2.1 behaviour code, udo code falls through to this if fails */
			primitive_type = TagOf (follow_user_type (t));
			switch (primitive_type) {
			case S_INT:
			case S_INT16:
			case S_INT32:
			case S_INT64:
			case S_UINT:
			case S_UINT16:
			case S_UINT32:
			case S_UINT64:
			case S_UNDECLARED:
			#ifdef OCCAM2_5
				case S_BYTE:
			#endif	/* OCCAM2_5 */
				break;
			default:
				t = undeclaredp;	/* bug 1279 22/8/91 */
				primitive_type = S_UNDECLARED;
				chk_invtype (chklocn, TagOf (tptr));
				break;
			}
			SetMOpType (tptr, primitive_type);
			return (t);
			/*}}} */
		}		/* end case */
		/*}}} */
		/*{{{  case S_NOT */
	case S_NOT:
		{
			/*{{{  user defined operator code */
			#ifdef USER_DEFINED_OPERATORS
				int tempnamelen = 0;
				char *tempname = NULL;

				modify_check = TRUE;
				t = typecheck_main (OpOf (tptr), default_type);
				left_type = t;	/* bugfix - see similar above */
				/*{{{  build function name (with literals resolving to int or byte) */
				tempname = construct_mop_name (default_type, tptr, &tempnamelen);
				/*}}} */
				if (user_defined_operators && searchforword (tempname, tempnamelen))
					/* name in symbol table */
				{
					if (findname (lookupword (tempname, tempnamelen)) != NULL) {
						/*{{{  function declared and in scope so insert function call */
						/* Function declared and in scope (on scope stack) */
						/* so transform dop node to finstance node */
						trans_mop_to_finstance (tptr, findname (lookupword (tempname, tempnamelen)));
						memfree (tempname);
						return (get_return_type (tptr));
						/*}}} */
					}
					/* function declared but out of scope so free up string memory */
					memfree (tempname);
				} else {
					/*{{{  free memory and generate error if types don't match */
					memfree (tempname);
					if (t == NULL) {
						char buf[MAX_ERR_SIZE];
						ftagstring (buf, TagOf (tptr), NULL, 0, NULL);
						chkreport_s (CHK_TYPES_DIFF, chklocn, buf);
						/*printf("whoops, an error occurred!\n");  subst in error call */
					}
					/*}}} */
				}
				/*{{{  operators not overloaded so perform full check on literals we fudged b4 */
				post_udo_error_check_on_literals (OpOf (tptr), default_type);
				/*}}} */
			#else
				/*}}} */
				/*{{{  alternative (original code) if user defined operators disabled at compile */
				t = typecheck_main (OpOf (tptr), default_type);
			#endif
			/*}}} */
			/*{{{  normal occam 2.1 behaviour code - include type 'inheriting' feature */
			primitive_type = TagOf (follow_user_type (t));
			switch (primitive_type) {
			case S_BOOL:
			case S_UNDECLARED:
				break;
			default:
				t = undeclaredp;	/* bug 1279 22/8/91 */
				primitive_type = S_UNDECLARED;
				chk_invtype (chklocn, S_NOT);
				break;
			}
			SetMOpType (tptr, primitive_type);
			return (t);
			/*}}} */
		}		/* end case */
		/*}}} */
#ifdef MOBILES
		/*{{{  case S_DEFINED */
	case S_DEFINED:
		{
			t = typecheck_main (OpOf (tptr), unknownnodeptr);

			/* extra stuff in use4 checks what sort of MOBILE it is, and does appropriate stuff */
			primitive_type = TagOf (follow_user_type (t));
			if ((primitive_type != S_MOBILE) && (primitive_type != S_ANYCHANTYPE) && (primitive_type != S_ANYPROCTYPE) && (primitive_type != S_ANYMOBILETYPE)) {
				t = undeclaredp;
				chk_invtype (chklocn, S_DEFINED);
			} else {
				t = boolnodeptr;
			}
			SetMOpType (tptr, S_BOOL);
			return t;
		}
		/*}}}*/
#endif
		/*{{{  case S_SIZE */
	case S_SIZE:
		{
			INT32 specattr, actattr;

			/*{{{  check for SIZE type */
			#ifdef OCCAM2_5
				t = follow_user_type (OpOf (tptr));
				if (istypetag (TagOf (t))) {
					SetMOpType (tptr, TagOf (t));
					if (TagOf (t) == S_ARRAY) {
						const int old = switch_to_real_workspace ();
						ctype (OpOf (tptr));
						switch_to_prev_workspace (old);
						return intnodeptr;
					} else {
						chk_invtype (chklocn, S_SIZE);
						return undeclaredp;
					}
				}
			#endif
			/*}}} */
			actattr = 0;
			specattr = 0;
			switch (TagOf (OpOf (tptr))) {
				/*{{{  S_ASINPUT S_ASOUPUT  break */
			case S_ASINPUT:
			case S_ASOUTPUT:
				actattr = (TagOf (OpOf (tptr)) == S_ASINPUT) ? TypeAttr_marked_in : TypeAttr_marked_out;
				SetOp (tptr, OpOf(OpOf (tptr)));

				t = typecheck_main (OpOf (tptr), unknownnodeptr);
				if (t) {
					treenode *base, *type;

					base = OpOf (tptr);
					while (TagOf (base) == S_ARRAYSUB) {
						base = ASBaseOf (base);
					}
					switch (TagOf (base)) {
					case N_DECL:
					case N_PARAM:
					case N_RESULTPARAM:
					case N_ABBR:
						/* descend the type-tree */
						type = NTypeOf (base);

						while (TagOf (type) == S_ARRAY) {
							type = ARTypeOf (type);
						}
						if ((TagOf (type) != S_CHAN) && (TagOf (type) != S_ANONCHANTYPE)) {
							chkreport (CHK_INV_CHANTYPE_VAR, LocnOf (tptr));
						} else {
							specattr = (TypeAttrOf (type) & (TypeAttr_marked_in | TypeAttr_marked_out));
						}
						if (specattr) {
							/* must match up */
							if (specattr != actattr) {
								chkreport_s (CHK_BAD_CHAN_CONFLICT, LocnOf (tptr), WNameOf (NNameOf (base)));
							}
						}
						break;
					}
				}
				break;
				/*}}}*/
				/*{{{  default  break*/
			default:
				t = typecheck_main (OpOf (tptr), unknownnodeptr);
				break;
				/*}}}*/
			}
			primitive_type = TagOf (follow_user_type (t));
#ifdef MOBILES
			if (primitive_type == S_MOBILE) {
				primitive_type = TagOf (MTypeOf (follow_user_type (t)));
			}
#endif
			SetMOpType (tptr, primitive_type);
			switch (primitive_type) {
			case S_ARRAY:
				t = intnodeptr;	/* bug TS/1473 12/12/91 */
				break;
			case S_UNDECLARED:	/* bug TS/1473 12/12/91 */
				break;
			default:
				t = undeclaredp;	/* bug 1279 22/8/91 */
				chk_invtype (chklocn, S_SIZE);
				break;
			}
			return /*intnodeptr */ t;	/* bug TS/1473 12/12/91 */
		}
		/*}}} */
		/*{{{  case S_BYTESIN */
	case S_BYTESIN:
		/*{{{  check for BYTESIN type */
		#ifdef OCCAM2_5
			t = follow_user_type (OpOf (tptr));
			if (istypetag (TagOf (t))) {
				const int old = switch_to_real_workspace ();
				ctype (OpOf (tptr));
				switch_to_prev_workspace (old);
				SetMOpType (tptr, TagOf (t));
				return intnodeptr;
			}
		#endif
		/*}}} */

		t = typecheck_main (OpOf (tptr), unknownnodeptr);
		primitive_type = TagOf (follow_user_type (t));
		SetMOpType (tptr, S_INT);
		switch (primitive_type) {
		case S_UNDECLARED:
			break;
		default:
			t = intnodeptr;
			break;
		}
		return /*intnodeptr */ t;
		/*}}} */
		/*}}} */
		/*{{{  arithmetic operators */
		/*{{{  new operators [>, <], &&, ^ etc... */
		/*{{{  diadic (binary operator) version of code */
	#ifdef USER_DEFINED_OPERATORS
	#include "casedops.h"
		{
			BOOL passed;
			int tempnamelen = 0;
			char *tempname = NULL;
			t = newcheckdop (tptr, default_type, &left_type, &right_type, &passed);
			if (passed) {
				/*{{{  construct name (with literals resolving down to intlit or bytelit) */
				{
					tempname = construct_name (default_type, tptr, &tempnamelen);
				}
				/*}}} */
				if (user_defined_operators && searchforword (tempname, tempnamelen)) {	/* name in symbol table */
					if (findname (lookupword (tempname, tempnamelen)) != NULL) {
						/*{{{  function declared and in scope - so subst in function call. */
						/* Function declared and in scope (on scope stack) */
						/* so transform dop node to finstance node */
						trans_dop_to_finstance (tptr, findname (lookupword (tempname, tempnamelen)));
						memfree (tempname);	/* free up string space, as no longer needed */
						return (get_return_type (tptr));
						/*}}} */
					}
					/* function defined, but out of scope, so release string space */
					memfree (tempname);
				} else {
					/*{{{  name not in symbol table - not defined */
					memfree (tempname);	/* as name not in symbol table - not defined */
					/* so free up the string space allocated */
					if (t == NULL) {
						char buf[MAX_ERR_SIZE];
						ftagstring (buf, TagOf (tptr), NULL, 0, NULL);
						chkreport_s (CHK_TYPES_DIFF, chklocn, buf);
					}
					/*}}} */
				}
			}
			t = undeclaredp;	/* bug 1279 22/8/91 */
			chk_invtype (chklocn, TagOf (tptr));
			SetDOpType (tptr, S_UNDECLARED);
			return (t);
		}
	#endif
		/*}}} */
		/*{{{  moandic (unary oparator) version of code */
	#ifdef USER_DEFINED_OPERATORS
	#include "casemops.h"
		{
			BOOL passed;
			int tempnamelen = 0;
			char *tempname = NULL;

			passed = 0;
			modify_check = TRUE;
			t = typecheck_main (OpOf (tptr), default_type);
			left_type = t;	/* added by Jim to fix bug */
			/*{{{  build function name (with literals resolving to int or byte) */
			{
				tempname = construct_mop_name (default_type, tptr, &tempnamelen);
			}
			/*}}} */
			if (user_defined_operators && searchforword (tempname, tempnamelen)) {	/* name in symbol table */
				/*printf("In symbol table\n"); */
				if (findname (lookupword (tempname, tempnamelen)) != NULL) {
					/*{{{  function delcared and in scope */
					/* Function declared and in scope (on scope stack) */
					/* so transform dop node to finstance node */
					trans_mop_to_finstance (tptr, findname (lookupword (tempname, tempnamelen)));
					memfree (tempname);	/* free up string space, as no longer needed */
					return (get_return_type (tptr));
					/*}}} */
				}
				/* function defined, but out of scope, so release string space */
				memfree (tempname);
			} else {
				/*{{{  function not defined, generate error if types don't match */
				memfree (tempname);	/* as name not in symbol table - not defined */
				/* so free up the string space allocated */
				if (t == NULL) {
					char buf[MAX_ERR_SIZE];
					/* printf("error message beacause types are different...\n"); */
					ftagstring (buf, TagOf (tptr), NULL, 0, NULL);
					chkreport_s (CHK_TYPES_DIFF, chklocn, buf);
				}
				/*}}} */
			}
			/*{{{  oh dear, not in symbol table, so set return type to undelcared. */
			t = undeclaredp;	/* bug 1279 22/8/91 */
			chk_invtype (chklocn, TagOf (tptr));
			SetDOpType (tptr, S_UNDECLARED);
			return (t);
			/*}}} */
		}
	#endif
		/*}}} */
		/*}}} */
		/*{{{  case S_ADD S_SUBTRACT S_MULT S_DIV S_REM */
	case S_ADD:
	case S_SUBTRACT:
	case S_MULT:
	case S_DIV:
	case S_REM:
		{
			/*{{{  user defined operator code */
			#ifdef USER_DEFINED_OPERATORS
				BOOL passed;
				int tempnamelen = 0;
				char *tempname = NULL;
				t = newcheckdop (tptr, default_type, &left_type, &right_type, &passed);
				if (passed) {
					/*{{{  construct name (with literals resolving down to intlit or bytelit) */
					{
						tempname = construct_name (default_type, tptr, &tempnamelen);
					}
#if 0
fprintf (stderr, "typecheck_main(): S_(arith): checking for UDO tempname = [%s]\n", tempname);
#endif
					/*}}} */
					if (user_defined_operators && searchforword (tempname, tempnamelen)) {	/* name in symbol table */
						if (findname (lookupword (tempname, tempnamelen)) != NULL) {
							/*{{{  function delcared and in scope */
							/* Function declared and in scope (on scope stack) */
							/* so transform dop node to finstance node */
							trans_dop_to_finstance (tptr, findname (lookupword (tempname, tempnamelen)));
							memfree (tempname);	/* free up string space, as no longer needed */
							return (get_return_type (tptr));
							/*}}} */
						}
						/* function defined, but out of scope, so release string space */
						memfree (tempname);
					} else {
						/*{{{  function not declared, so generate error if types don't match */
						memfree (tempname);	/* as name not in symbol table - not defined */
						/* so free up the string space allocated */
						if (t == NULL) {
							char buf[MAX_ERR_SIZE];
							ftagstring (buf, TagOf (tptr), NULL, 0, NULL);
							chkreport_s (CHK_TYPES_DIFF, chklocn, buf);
							/*printf("whoops, an error occurred!\n");  subst in error call */
						}
						/*}}} */
					}
					/*{{{  now we know there is no overloading - do proper check on literals */
					post_udo_error_check_on_literals (LeftOpOf (tptr), default_type);
					post_udo_error_check_on_literals (RightOpOf (tptr), default_type);
					/*}}} */
			#else
				/*}}} */
				/*{{{  alternative code if user defined operators are diabled */
				t = checkdop (tptr, default_type);
			#endif
				primitive_type = TagOf (follow_user_type (t));
				/*}}} */
#ifdef MOBILES
				if (primitive_type == S_MOBILE) {
					t = MTypeOf (follow_user_type (t));

#if 0
fprintf (stderr, "typecheck_main: DOP operator, type tree =");
printtreenl (stderr, 4, t);
#endif
					primitive_type = TagOf (follow_user_type (t));
				}
#endif	/* MOBILES */
				/*{{{  old code to deal with occam 2.1 stuff if no udo (fallen through to) */
				switch (primitive_type)
					/*{{{  */
				{
				case S_INT:
				case S_INT16:
				case S_INT32:
				case S_INT64:
				case S_UINT:
				case S_UINT16:
				case S_UINT32:
				case S_UINT64:
				case S_REAL32:
				case S_REAL64:
				case S_UREALLIT:
				case S_UINTLIT:
				case S_UNDECLARED:
				#ifdef OCCAM2_5
					case S_BYTE:
				#endif
					break;
				default:
					t = undeclaredp;	/* bug 1279 22/8/91 */
					primitive_type = S_UNDECLARED;
					chk_invtype (chklocn, TagOf (tptr));
					break;
				}
				/*}}} */
				SetDOpType (tptr, primitive_type);
				return (t);
			#ifdef USER_DEFINED_OPERATORS
				}		/* end else */
			#endif
			/*}}} */
		}			/* end case */
		/*}}} */
		/*}}} */
		/*{{{  bitwise operators */
		/*{{{  case S_BITAND S_BITOR S_XOR and modulo operators */
	case S_BITAND:
	case S_BITOR:
	case S_XOR:
	case S_PLUS:
	case S_MINUS:
	case S_TIMES:
	{
		/*{{{  user defined operator code */
		#ifdef USER_DEFINED_OPERATORS
			BOOL passed;
			int tempnamelen = 0;
			char *tempname = NULL;
			t = newcheckdop (tptr, default_type, &left_type, &right_type, &passed);
			if (passed) {
				/*{{{  construct name (with literals resolving down to intlit or bytelit) */
				{
					tempname = construct_name (default_type, tptr, &tempnamelen);
				}
				/*}}} */
				tempname = construct_name (default_type, tptr, &tempnamelen);	/* build function name */
				if (user_defined_operators && searchforword (tempname, tempnamelen))
					/* name in symbol table */
				{
					if (findname (lookupword (tempname, tempnamelen)) != NULL)
						/*{{{  function declared and in scope */
					{
						/* Function declared and in scope (on scope stack) */
						/* so transform dop node to finstance node */
						trans_dop_to_finstance (tptr, findname (lookupword (tempname, tempnamelen)));
						memfree (tempname);
						return (get_return_type (tptr));
					}
					/*}}} */
					memfree (tempname);
				} else
					/*{{{  free up memory and generate error if types are different (no match) */
				{
					memfree (tempname);
					if (t == NULL) {
						char buf[MAX_ERR_SIZE];
						ftagstring (buf, TagOf (tptr), NULL, 0, NULL);
						chkreport_s (CHK_TYPES_DIFF, chklocn, buf);
						/*printf("whoops, an error occurred!\n");  subst in error call */
					}
				}
				/*}}} */
				/*{{{  now we know there is no overloading - do proper check on literals */
				post_udo_error_check_on_literals (LeftOpOf (tptr), default_type);
				post_udo_error_check_on_literals (RightOpOf (tptr), default_type);
				/*}}} */
		#else
			/*}}} */
			/*{{{  alternative code if user defined operators are diabled */
			t = checkdop (tptr, default_type);
			/*}}}  */
		#endif
		/*}}} */
		/*{{{  old occam 2.1 code to deal with cases when no user defined operator */
			primitive_type = TagOf (follow_user_type (t));
			switch (primitive_type) {
			case S_INT:
			case S_INT16:
			case S_INT32:
			case S_INT64:
			case S_UINT:
			case S_UINT16:
			case S_UINT32:
			case S_UINT64:
			case S_UINTLIT:
			case S_UNDECLARED:
			#ifdef OCCAM2_5
				case S_BYTE:
			#endif
				break;
			default:
				t = undeclaredp;	/* bug 1279 22/8/91 */
				primitive_type = S_UNDECLARED;
				chk_invtype (chklocn, TagOf (tptr));
				break;
			}
			SetDOpType (tptr, primitive_type);
			return (t);
		#ifdef USER_DEFINED_OPERATORS
			}			/* end else */
		#endif
		/*}}} */
		}				/* end case */

	      /*}}} */
	      /*{{{  case S_LSHIFT S_RSHIFT */
	case S_LSHIFT:
	case S_RSHIFT:
	{
		/*{{{  user defined operator code */
		#ifdef USER_DEFINED_OPERATORS
			BOOL passed;
			int tempnamelen = 0;
			char *tempname = NULL;
			treenode *rt;
			modify_check = TRUE;
			rt = typecheck_main (RightOpOf (tptr), intnodeptr);
			modify_check = TRUE;
			t = typecheck_main (LeftOpOf (tptr), default_type);
			left_type = t;
			right_type = rt;
			passed = TRUE;
			if (passed) {
				/*{{{  construct name (with literals resolving down to intlit or bytelit) */
				{
					tempname = construct_name (default_type, tptr, &tempnamelen);
				}
				/*}}} */
				if (user_defined_operators && searchforword (tempname, tempnamelen))
					/* name in symbol table */
				{
					if (findname (lookupword (tempname, tempnamelen)) != NULL)
						/*{{{  function declared and in scope */
					{
						/* Function declared and in scope (on scope stack) */
						/* so transform dop node to finstance node */
						trans_dop_to_finstance (tptr, findname (lookupword (tempname, tempnamelen)));
						memfree (tempname);
						return (get_return_type (tptr));
					}
					/*}}} */
					/* function not in scope, free memory and fall through */
					memfree (tempname);
				} else
					/*{{{  function not declared, generate error if types diff, fall through */
				{
					memfree (tempname);
					if (t == NULL) {
						char buf[MAX_ERR_SIZE];
						ftagstring (buf, TagOf (tptr), NULL, 0, NULL);
						chkreport_s (CHK_TYPES_DIFF, chklocn, buf);
						/*printf("whoops, an error occurred!\n");  subst in error call */
					}
				}
				/*}}} */
				/*{{{  now we know there is no overloading - do proper check on literals */
				post_udo_error_check_on_literals (LeftOpOf (tptr), default_type);
				post_udo_error_check_on_literals (RightOpOf (tptr), default_type);
				/*}}} */
		#else
			/*}}} */
			/*{{{  alternative code if user defined operators are disabled */
			treenode *rt = typecheck_main (RightOpOf (tptr), intnodeptr);
			t = typecheck_main (LeftOpOf (tptr), default_type);
		#endif
		/*}}} */
		/*{{{  old occam 2.1 code to deal with normal cases - (and a #ifdef'd }) */
		if (TagOf (rt) == S_INT) {
			/*{{{  check type of operand */
			primitive_type = TagOf (follow_user_type (t));
			switch (primitive_type) {
			case S_INT:
			case S_INT16:
			case S_INT32:
			case S_INT64:
			case S_UINT:
			case S_UINT16:
			case S_UINT32:
			case S_UINT64:
			case S_UINTLIT:
			case S_UNDECLARED:
			#ifdef OCCAM2_5
				case S_BYTE:
			#endif
				break;
			default:
				t = undeclaredp;	/* bug 1279 22/8/91 */
				primitive_type = S_UNDECLARED;
				chk_invtype (chklocn, TagOf (tptr));
				break;
			}
			SetDOpType (tptr, primitive_type);
			/*}}} */
		} else {
			/*chkreport (CHK_INVCOUNT, chklocn); */
			chkerr (CHK_INVCOUNT, chklocn);	/* bug TS/1956 10/11/92 */
			/* FIX: set operator type to something valid (avoids fallout later) */
			SetDOpType (tptr, S_UNDECLARED);
		}
		return (t);
		#ifdef USER_DEFINED_OPERATORS
			}				/* end else */
		#endif
	      /*}}} */
	}

	/*}}} */
	/*{{{  logical operators */
	/*{{{  case S_AND S_OR */
	case S_AND:
	case S_OR:
	{
		/*{{{  user defined operator code */
		#ifdef USER_DEFINED_OPERATORS
			BOOL passed;
			int tempnamelen = 0;
			char *tempname = NULL;
			t = newcheckdop (tptr, default_type, &left_type, &right_type, &passed);
			if (passed) {
				/*{{{  construct name (with literals resolving down to intlit or bytelit) */
				{
					tempname = construct_name (default_type, tptr, &tempnamelen);
				}
				/*}}} */
				if (user_defined_operators && searchforword (tempname, tempnamelen))
					/* name in symbol table */
				{
					if (findname (lookupword (tempname, tempnamelen)) != NULL)
						/*{{{  function defined and in scope */
					{
						/* Function declared and in scope (on scope stack) */
						/* so transform dop node to finstance node */
						trans_dop_to_finstance (tptr, findname (lookupword (tempname, tempnamelen)));
						memfree (tempname);
						/* the return type is exactly what was asked for */
						return (get_return_type (tptr));
					}
					/*}}} */
					/* function defined, but not in scope, so free up memory, fall through */
					memfree (tempname);
				} else
					/*{{{  function not defined - generate error if types diff, fall through */
				{
					memfree (tempname);
					if (t == NULL) {
						char buf[MAX_ERR_SIZE];
						ftagstring (buf, TagOf (tptr), NULL, 0, NULL);
						chkreport_s (CHK_TYPES_DIFF, chklocn, buf);
						/*printf("whoops, an error occurred!\n");  subst in error call */
					}
				}
				/*}}} */
				/*{{{  now we know there is no overloading - do proper check on literals */
				post_udo_error_check_on_literals (LeftOpOf (tptr), default_type);
				post_udo_error_check_on_literals (RightOpOf (tptr), default_type);
				/*}}} */
		#else
			/*}}} */
			/*{{{  alternative code if user defined operators are disabled */
			t = checkdop (tptr, default_type);
		#endif
			/*}}} */
			/*{{{  old occam 2.1 code to deal with `normal' case (no user defined operator) */
			switch (TagOf (t)) {
			case S_BOOL:
			case S_UNDECLARED:
				break;
			default:
				chk_invtype (chklocn, TagOf (tptr));
				break;
			}
			SetDOpType (tptr, S_BOOL);
			return boolnodeptr;
		#ifdef USER_DEFINED_OPERATORS
			}				/* end else */
		#endif
		/*}}} */
	}
	/*}}} */
	/*}}} */
	/*{{{  relational operators */
	/*{{{  case S_EQ S_NE S_LS S_LE S_GR S_GE */
	case S_EQ:
	case S_NE:
	case S_LS:
	case S_LE:
	case S_GR:
	case S_GE:
	{
		/*{{{  user defined operator code */
		#ifdef USER_DEFINED_OPERATORS
			BOOL passed;
			int tempnamelen = 0;
			char *tempname = NULL;
			t = newcheckdop (tptr, unknownnodeptr, &left_type, &right_type, &passed);
			if (passed) {
				/*{{{  construct name (with literals resolving down to intlit or bytelit) */
				{
					tempname = construct_name (default_type, tptr, &tempnamelen);
				}
				/*}}} */
				if (user_defined_operators && searchforword (tempname, tempnamelen))
					/* name in symbol table */
				{
					if (findname (lookupword (tempname, tempnamelen)) != NULL) {
						/* Function declared and in scope (on scope stack) */
						/* so transform dop node to finstance node */
						trans_dop_to_finstance (tptr, findname (lookupword (tempname, tempnamelen)));
						memfree (tempname);
						/* the return type is exactly what was asked for */
						return (get_return_type (tptr));
					}
					memfree (tempname);
				} else {
					memfree (tempname);
					if (t == NULL) {
						char buf[MAX_ERR_SIZE];
						ftagstring (buf, TagOf (tptr), NULL, 0, NULL);
						chkreport_s (CHK_TYPES_DIFF, chklocn, buf);
						/*printf("whoops, an error occurred!\n");  subst in error call */
					}
				}
				/*{{{  now we know there is no overloading - do proper check on literals */
				post_udo_error_check_on_literals (LeftOpOf (tptr), default_type);
				post_udo_error_check_on_literals (RightOpOf (tptr), default_type);
				/*}}} */
		#else
			/*}}} */
			/*{{{  alternative code if user defined operators are disabled */
			t = checkdop (tptr, unknownnodeptr);
		#endif
			/*}}} */
			/*{{{  old occam 2.1 code to deal with normal case (and some #ifdef'd closing }) */
			/*{{{  check type is correct */
			primitive_type = TagOf (follow_user_type (t));
			switch (primitive_type) {
			case S_BOOL:
				switch (TagOf (tptr)) {
				case S_EQ:
				case S_NE:
					break;
				default:
					t = undeclaredp;	/* bug 1279 22/8/91 */
					primitive_type = S_UNDECLARED;
					chk_invtype (chklocn, TagOf (tptr));
					break;
				}
				break;
			case S_BYTE:
			case S_INT:
			case S_INT16:
			case S_INT32:
			case S_INT64:
			case S_UINT:
			case S_UINT16:
			case S_UINT32:
			case S_UINT64:
			case S_REAL32:
			case S_REAL64:
			case S_UNDECLARED:
				break;
			default:
				t = undeclaredp;	/* bug 1279 22/8/91 */
				primitive_type = S_UNDECLARED;
				chk_invtype (chklocn, TagOf (tptr));
				break;
			}
			SetDOpType (tptr, primitive_type);

			/*}}} */
			return (boolnodeptr);
		#ifdef USER_DEFINED_OPERATORS
			}				/* end else */
		#endif
		/*}}} */
	}				/* end case */
	/*}}} */
	/*{{{  case S_AFTER */
	case S_AFTER:
	{
		/*{{{  user defined operator code */
		#ifdef USER_DEFINED_OPERATORS
			BOOL passed;
			int tempnamelen = 0;
			char *tempname = NULL;
			t = newcheckdop (tptr, unknownnodeptr, &left_type, &right_type, &passed);
			if (passed) {
				/*{{{  construct name (with literals resolving down to intlit or bytelit) */
				{
					tempname = construct_name (default_type, tptr, &tempnamelen);
				}
				/*}}} */
				if (user_defined_operators && searchforword (tempname, tempnamelen))
					/* name in symbol table */
				{
					if (findname (lookupword (tempname, tempnamelen)) != NULL)
						/*{{{  function declared and in scope */
					{
						/* Function declared and in scope (on scope stack) */
						/* so transform dop node to finstance node */
						trans_dop_to_finstance (tptr, findname (lookupword (tempname, tempnamelen)));
						memfree (tempname);
						/* the return type is exactly what was asked for */
						return (get_return_type (tptr));
					}
					/*}}} */
					/* function delcared but not in scope, free memory, fall through */
					memfree (tempname);
				} else
					/*{{{  function not defined so generate error if types diff, fall through */
				{
					memfree (tempname);
					if (t == NULL) {
						char buf[MAX_ERR_SIZE];
						ftagstring (buf, TagOf (tptr), NULL, 0, NULL);
						chkreport_s (CHK_TYPES_DIFF, chklocn, buf);
						/*printf("whoops, an error occurred!\n");  subst in error call */
					}
				}
				/*}}} */
				/*{{{  now we know there is no overloading - do proper check on literals */
				post_udo_error_check_on_literals (LeftOpOf (tptr), default_type);
				post_udo_error_check_on_literals (RightOpOf (tptr), default_type);
				/*}}} */
		#else
			/*}}} */
			/*{{{  alternative code if user defined operators disabled */
			t = checkdop (tptr, unknownnodeptr);
		#endif
			/*}}} */
			/*{{{  old occam 2.1 code to deal with normal case (and some #ifdef'd closing }) */
			primitive_type = TagOf (follow_user_type (t));
			switch (primitive_type) {
			case S_INT:
			case S_INT16:
			case S_INT32:
			case S_INT64:
			case S_UINT:
			case S_UINT16:
			case S_UINT32:
			case S_UINT64:
			case S_UNDECLARED:
			#ifdef OCCAM2_5
				case S_BYTE:
			#endif
				break;
			default:
				t = undeclaredp;	/* bug 1279 22/8/91 */
				primitive_type = S_UNDECLARED;
				chk_invtype (chklocn, S_AFTER);
				break;
			}
			SetDOpType (tptr, primitive_type);
			return (boolnodeptr);
		#ifdef USER_DEFINED_OPERATORS
			}				/* end else */
		#endif
		/*}}} */
	}				/* end case */
	/*}}} */
	/*}}} */
	/*{{{  mostpos, mostneg */
	/*{{{  case S_MOSTPOS S_MOSTNEG */
	case S_MOSTPOS:
	case S_MOSTNEG:
	{
		t = OpOf (tptr);
		primitive_type = TagOf (follow_user_type (t));
		switch (primitive_type) {
		case S_INT:
		case S_INT16:
		case S_INT32:
		case S_INT64:
		case S_UINT:
		case S_UINT16:
		case S_UINT32:
		case S_UINT64:
		#ifdef OCCAM2_5
			case S_BYTE:
		#endif
			break;
		default:
			t = undeclaredp;	/* bug 1279 22/8/91 */
			primitive_type = S_UNDECLARED;
			chk_invtype (chklocn, TagOf (tptr));
			break;
		}
		SetMOpType (tptr, primitive_type);
		return (t);
	}
	/*}}} */
	/*}}} */
#ifdef MOBILES
	/*{{{  ADDROF/HWADDROF operator */
	case S_ADDROF:
	case S_HWADDROF:
	{
		treenode *optype = typecheck_main (OpOf (tptr), default_type);

		if (TagOf (follow_user_type (optype)) != S_MOBILE) {
			chk_invtype (chklocn, TagOf (tptr));
			optype = undeclaredp;
		} else {
			optype = intnodeptr;
		}

		SetMOpType (tptr, S_INT);
		
		return optype;
	}
	/*}}}*/
	/*{{{  CLONE operator */
	case S_CLONE:
	{
		treenode *optype = typecheck_main (OpOf (tptr), default_type);
		if (TagOf (follow_user_type (optype)) != S_MOBILE) {
			chk_invtype (chklocn, TagOf (tptr));
			optype = undeclaredp;
		}
#if 0
fprintf (stderr, "typecheck_main: S_CLONE: OpOf =");
printtreenl (stderr, 4, OpOf (tptr));
#endif
		SetMOpType (tptr, TagOf (follow_user_type (optype)));
		return optype;
	}
	/*}}} */
	/*{{{  ALLOC_PROC, for creating dynamic MOBILE processes*/
	case S_ALLOC_PROC:
	{
		treenode *type;			/* resultant type */

		type = ARTypeOf (tptr);
		if ((TagOf (type) != N_MPROCDECL) && (TagOf (type) != N_LIBMPROCDECL)) {
			return type;
		}
		if (!NDeclOf (type) || !DExtraOf (NDeclOf (type))) {
			return type;
		}
		type = DExtraOf (NDeclOf (type));
#if 0
fprintf (stderr, "chk1: typecheck_main: S_ALLOC_PROC: type = ");
printtreenl (stderr, 4, type);
#endif
		return type;
	}
	/*}}}*/
	/*{{{  NEW_BARRIER, for creating dynamic MOBILE BARRIERs*/
	case S_NEW_BARRIER:
	{
		treenode *type;

		type = ARTypeOf (tptr);
		return type;
	}
	/*}}}*/
	/*{{{  NEW_ARRAY, for creating dynamic MOBILE arrays */
	case S_NEW_ARRAY:
	{
		/* need to construct the ARRAY type of this thing and return it... */
		treenode *list = ARDimLengthOf (tptr);		/* dimension list */
		treenode *final_type = ARTypeOf (tptr);		/* resultant type */
		treenode **first = NULL;
		treenode **orig = ARTypeAddr (tptr);
		const int old = switch_to_real_workspace ();

		while (!EmptyList (list)) {
			treenode *dimtype = typecheck_main (ThisItem (list), intnodeptr);

			if ((TagOf (dimtype) != S_UNDECLARED) && (TagOf (dimtype) != S_INT)) {
				chkreport (CHK_DYN_DIM_NOT_INT, chklocn);
			}
			final_type = newtypenode (S_ARRAY, NOPOSN, NULL, final_type);
			if (!first) {
				first = ARTypeAddr (final_type);
			}
			list = NextItem (list);
		}

		/* might need to go inside final_type and patch up nested MOBILE types (ASINPUT/ASOUTPUT on chan types) */
#if 0
fprintf (stderr, "typecheck_main: NEWARRAY: *first =");
printtreenl (stderr, 4, *first);
#endif
		if ((TagOf (*first) == S_ASINPUT) || (TagOf (*first) == S_ASOUTPUT)) {
			treenode *inner = OpOf (*first);
			int specattr = (TagOf (*first) == S_ASINPUT) ? TypeAttr_marked_in : TypeAttr_marked_out;

			if (TagOf (inner) != N_TYPEDECL) {
				chkreport (CHK_UNEXPECTED_CHANDIR, chklocn);
			} else {
				treenode *typecopy;
				
				typecopy = newnamenode (TagOf (inner), LocnOf (inner), NNameOf (inner), NTypeOf (inner),
						NDeclOf (inner), NLexLevelOf (inner), NScopeOf (inner), NModeOf (inner));
				if (OpTypeAttrOf (*first) & TypeAttr_shared) {
					specattr |= TypeAttr_shared;
				}
				SetNTypeAttr (typecopy, specattr);
				*first = typecopy;
#if 0
fprintf (stderr, "typecheck_main: NEWARRAY: *orig (before mod) =");
printtreenl (stderr, 4, *orig);
#endif
				*orig = typecopy;
#if 0
fprintf (stderr, "typecheck_main: NEWARRAY: *orig (after mod) =");
printtreenl (stderr, 4, *orig);
#endif
			}
		}
		switch_to_prev_workspace (old);
		return final_type;
	}
	/*}}}  */
#endif
	/*{{{  TYPEHASHOF operator */
	case S_TYPEHASHOF:
	{
		treenode *optype = intnodeptr;

		/* FIXME: should check that the operand is sensible */
		SetMOpType (tptr, S_INT);

		return optype;
	}
	/*}}}*/
	/*{{{  conversions */
	case S_EXACT:
	case S_ROUND:
	case S_TRUNC:
	{
		int ct = MOpTypeOf (tptr);	/* The type to convert to */
		int tt;			/* The source type */
		treenode *op = OpOf (tptr);
		treenode *type_tree;
		#ifdef OCCAM2_5
			if (ct == S_NAME) {	/* conversion to user defined type */
				treenode *const old_op = op;
				type_tree = ThisItem (op);
				op = ThisItem (NextItem (op));
				ct = TagOf (follow_user_type (type_tree));

				/*{{{  free up the superfluous list nodes */
				freenode (NextItemAddr (old_op));
				freenode (OpAddr (tptr));
				/*}}} */
				SetOp (tptr, op);
				SetMOpType (tptr, ct);
			} else {
				type_tree = typenodeptr (ct);
			}
		#else
			type_tree = typenodeptr (ct);
		#endif

		t = typecheck_main (op, unknownnodeptr);
		#ifdef MOBILES
			/* if t is a MOBILE something, move the type along */
			if (TagOf (follow_user_type (t)) == S_MOBILE) {
				t = MTypeOf (follow_user_type (t));
			}
		#endif
		tt = TagOf (follow_user_type (t));

		if (tt == S_UNDECLARED) {
			/* skip */ ;
		}
		#ifdef OCCAM2_5
		else if (ct == S_ARRAY) {
			chkerr (CHK_ARRAY_CONVERSION, chklocn);
		}
		#endif
		else if (TagOf (tptr) == S_EXACT) {
			/*{{{  check valid combinations for EXACT */
			if ((((tt == S_BOOL) || (tt == S_BYTE) || isint (tt)) && ((ct == S_BOOL) || (ct == S_BYTE) || isint (ct)))
				|| ((tt == S_REAL32) && isreal (ct))
				|| ((tt == S_REAL64) && (ct == S_REAL64))) {
				/* skip */ ;
			} else {
				char type_str[MAX_ERR_SIZE];

				if (TagOf (type_tree) == N_TYPEDECL) {
					sprintf (type_str, "\"%s\"", WNameOf (NNameOf (type_tree)));
				} else {
					ftagstring (type_str, ct, NULL, 0, NULL);
				}
				/*chkreport_s(CHK_INV_EXACT, chklocn, type_str); */
				chkerr_s (CHK_INV_EXACT, chklocn, type_str);	/* bug TS/1956 10/11/92 */
			}
			/*}}} */
		} else {
			/*{{{  check valid combinations for ROUND/TRUNC */
			#ifdef OCCAM2_5
				if ((isintorbyte (tt) && isreal (ct)) || (isreal (tt) && isintorbyte (ct)) || (isreal (tt) && isreal (ct)))
					/* skip */ ;
			#else
				if ((isint (tt) && isreal (ct)) || (isreal (tt) && isint (ct)) || (isreal (tt) && isreal (ct)))
					/* skip */ ;
			#endif
			else {
				/*chkreport(CHK_INV_ROUND_TRUNC, chklocn); */
				chkerr (CHK_INV_ROUND_TRUNC, chklocn);	/* bug TS/1956 10/11/92 */
			}
			/*}}} */
		}
		return type_tree;
	}
	/*}}} */
	/*{{{  offsetof */
	#ifdef OCCAM2_5
	case S_OFFSETOF:
		/* all the checking is done when the scoping is first done */
		return intnodeptr;
	#endif
	/*}}} */
	/*{{{  literals */
	case S_TRUE:
	case S_FALSE:
		return boolnodeptr;
	case S_ASMNAME:
		return intnodeptr;
	case S_CONSTEXP:
		return typecheck_main (CExpOf (tptr), default_type);
	/*{{{  S_STRING */
	case S_STRING:
	{
		treenode *aptr = ConstTableTypeOf (tptr);
		const BIT32 dim = WLengthOf (CTValOf (tptr));

		#ifdef OCCAM2_5
			if (aptr != NULL) {
				const int old = switch_to_temp_workspace ();
				treenode *stringtype = newtypenode (S_ARRAY, NOPOSN, newconstant (dim), bytenodeptr);
				SetARDim (stringtype, dim);
				switch_to_prev_workspace (old);
				if (!typesequivalent (stringtype, follow_user_type (aptr), FALSE)) {
					chkreport (CHK_BAD_STRING_TYPE, LocnOf (tptr));
				}
			} else {
				aptr = newtypenode (S_ARRAY, NOPOSN, newconstant (dim), bytenodeptr);
				SetARDim (aptr, dim);
				#if TYPE_COERCION
					if (TagOf (default_type) == N_TYPEDECL) {
						treenode *const default_shape = follow_user_type (default_type);
						if (typesequivalent (aptr, default_shape, FALSE)) {
							aptr = default_type;
						}
					}
				#endif	/* TYPE_CORRECTION */
			}
		#else
			aptr = newtypenode (S_ARRAY, NOPOSN, newconstant (dim), bytenodeptr);
			SetARDim (aptr, dim);
		#endif
		SetConstTableType (tptr, aptr);	/* MDP */
		return (aptr);
	}
	/*}}} */
	/*}}} */
	/*{{{  S_UBYTELIT / S_UINTLIT */
	case S_UBYTELIT:
	case S_UINTLIT:
	{
		treenode *littype;
		littype = LitTypeOf (tptr);
		if (littype == NULL) {
			/*{{{  take the default type */
			treenode *const simplest_type = (TagOf (tptr) == S_UBYTELIT) ? bytenodeptr : intnodeptr;
			treenode *target_type;
			int old;

			SetLitUnspec (tptr, TRUE);

#if 0
printf ("typecheck_main: %s ", itagstring (TagOf (tptr)));
printtree (stdout, 0, default_type);
printf ("\n");
#endif

			#if TYPE_COERCION
				target_type = (TagOf (default_type) == S_UNKNOWN) ? simplest_type : default_type;
			#else
				if ((TagOf (default_type) == S_UNKNOWN) || (TagOf (default_type) == TagOf (simplest_type)))
					target_type = simplest_type;
				else {
					target_type = unknownnodeptr;
					type_mismatch (default_type, FALSE);
				}
			#endif
			old = switch_to_real_workspace ();
			littype = copytree (target_type, 0);	/* lexlevel is irrelevant in type trees */
			switch_to_prev_workspace (old);
			/*}}} */
		}
		/*{{{  check it is a valid type for a literal */
		/*{{{  COMMENT let strange types through if check modified */
		/*The outmost if statement here let types through if the modify_check flag is up */
		/*and checks them later with post_udo_check() (or whatever its called) - this */
		/*constrasts to the earlier situation of only letting through records and arrays */
		/*I'm supprised it didn't come to light earlier. */
		/*}}} */
		/* if litype is MOBILE, make it the MOBILE type */
		#ifdef MOBILES
			if (TagOf (follow_user_type (littype)) == S_MOBILE) {
				littype = MTypeOf (follow_user_type (littype));
			}
		#endif
		#ifdef USER_DEFINED_OPERATORS
			if (!(modify_check_for_udo && user_defined_operators)) {
		#endif
		switch (TagOf (follow_user_type (littype))) {
		case S_BYTE:
		case S_INT:
		case S_INT16:
		case S_INT32:
		case S_INT64:
		case S_UINT:
		case S_UINT16:
		case S_UINT32:
		case S_UINT64:
		case N_DECL:	/* undeclared user type */
			break;
			/*{{{  COMMENT extra cases for user defined operators */
			/*#ifdef USER_DEFINED_OPERATORS */
			/*case S_RECORD: */
			/*case S_ARRAY: */
			/*{{{  COMMENT notes on why */
			/*The extra case statements are necessary because it is possible */
			/*that default_type may be a record or an array - in this case, because */
			/*the LitUnspec field of the litnode will be set (indicating the type was */
			/*not specified) - if the literal is being used in a user defined operator */
			/*then its type will be changed to its basic form (eg INT, BYTE, REAL32) by */
			/*the udo code.  If it turns out not to be used by a user defined operator */
			/*then this check is repeated - but this time not allowing through ARRAY's */
			/*or RECORDS (post_udo_check()) */
			/*}}} */
			/*if (modify_check_for_udo && user_defined_operators) */
			/*{ */
			/*break; */
			/*} */
			/*#endif */
			/*}}} */
		default:
			if (LitTypeOf (tptr) == NULL) {
				type_mismatch (default_type, FALSE);
			} else {
				chkreport (CHK_INV_TYPE_DECORATION, chklocn);
			}
			break;
		}
		#ifdef USER_DEFINED_OPERATORS
			}
		#endif
		/*}}} */
		SetLitType (tptr, littype);
		return littype;
	}
	/*}}} */
	/*{{{  S_UREALLIT */
	case S_UREALLIT:
	{
		treenode *littype;
		littype = LitTypeOf (tptr);
		if (littype == NULL)
		/*{{{  take the default type */
		#if TYPE_COERCION
		{
			const int old = switch_to_real_workspace ();
			#ifdef USER_DEFINED_OPERATORS
				SetLitUnspec (tptr, TRUE);
				if (modify_check_for_udo) {
					if (TagOf (default_type) == S_UNKNOWN) {
						littype = real32nodeptr;
					} else {
						littype = copytree (default_type, 0);	/* lexlevel is irrelevant in type trees */
					}
				} else
			#endif
				{
					littype = copytree (default_type, 0);	/* lexlevel is irrelevant in type trees */
				}
			switch_to_prev_workspace (old);
		}
		#else
			SetLitUnspec (tptr, TRUE);
			chkreport (CHK_UNRESOLVED_REALTYPE, chklocn);
		#endif
		/*}}} */

		/*{{{  check it is a valid type for a literal */
		/*{{{  COMMENT let strange types through if check modified */
		/*The outmost if statement here let types through if the modify_check flag is up */
		/*and checks them later with post_udo_check() (or whatever its called) - this */
		/*constrasts to the earlier situation of only letting through records and arrays */
		/*I'm supprised it didn't come to light earlier. */
		/*}}} */
		#ifdef USER_DEFINED_OPERATORS
			if (!(modify_check_for_udo && user_defined_operators)) {
		#endif
		switch (TagOf (follow_user_type (littype))) {
		case S_REAL32:
		case S_REAL64:
		case N_DECL:	/* undeclared user type */
			break;
			/*{{{  COMMENT extra cases for user defined operators */
			/*#ifdef USER_DEFINED_OPERATORS */
			/*case S_RECORD: */
			/*case S_ARRAY: */
			/*{{{  COMMENT notes on why */
			/*The extra case statements are necessary because it is possible */
			/*that default_type may be a record or an array - in this case, because */
			/*the LitUnspec field of the litnode will be set (indicating the type was */
			/*not specified) - if the literal is being used in a user defined operator */
			/*then its type will be changed to its basic form (eg INT, BYTE, REAL32) by */
			/*the udo code.  If it turns out not to be used by a user defined operator */
			/*then this check is repeated - but this time not allowing through ARRAY's */
			/*or RECORDS (post_udo_check()) */
			/*}}} */
			/*if (modify_check_for_udo && user_defined_operators) */
			/*break; */
			/*#endif */
			/*}}} */
		default:
			if (LitTypeOf (tptr) == NULL)
				type_mismatch (default_type, FALSE);
			else
				chkreport (CHK_INV_TYPE_DECORATION, chklocn);
			break;
		}
		#ifdef USER_DEFINED_OPERATORS
			}
		#endif
		/*}}} */
		SetLitType (tptr, littype);
		return littype;
	}
	/*}}} */
	/*{{{  name */
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_DECL:
	case N_REPL:
	case N_PARAM:
	case N_VALPARAM:
	case N_RESULTPARAM:
		return (NTypeOf (tptr));

	case N_TAGDEF:
		msg_out_s (SEV_WARN, CHK, CHK_TAG_IN_EXPRESSION, chklocn, WNameOf (NNameOf (tptr)));
		return bytenodeptr;

	/* All the following are invalid */
	case N_TPROTDEF:
	case N_SPROTDEF:
	case N_PROCDEF:
	case N_SCPROCDEF:
	case N_LIBPROCDEF:
	case N_LIBMPROCDECL:
	case N_INLINEPROCDEF:
	case N_PREDEFPROC:
	CASE_CONFIG_NAME
	case N_TYPEDECL:
#ifdef MOBILES
	case N_PROCTYPEDECL:
#endif
		type_mismatch (default_type, TRUE);
		return undeclaredp;
	case N_SFUNCDEF:
	case N_LFUNCDEF:
	case N_SCFUNCDEF:
	case N_LIBFUNCDEF:
	case N_INLINEFUNCDEF:
	case N_PREDEFFUNCTION:
		/* added for bug 1115 29/1/91 */
		/*chkreport_s(CHK_TOO_FEW_ACTUALS,chklocn, WNameOf(NNameOf(tptr))); */
		chkerr_s (CHK_TOO_FEW_ACTUALS, chklocn, WNameOf (NNameOf (tptr)));	/* bug TS/1956 10/11/92 */
		return undeclaredp;
	case N_FIELD:
		if (chk_permit_lonely_fields) {
			return NTypeOf (tptr);
		}
		/*chkreport_s(CHK_LONELY_FIELD, chklocn, WNameOf(NNameOf(tptr))); */
		chkerr_s (CHK_LONELY_FIELD, chklocn, WNameOf (NNameOf (tptr)));
		return undeclaredp;
	/*}}} */
	/*{{{  function instance */
	case S_FINSTANCE:
	{
		treenode *nptr = INameOf (tptr);
		treenode *p;

		checkparams (tptr, S_FUNCTION);	/* Check actual parameter types  */
		/*{{{  check to see if declared */
		if ((TagOf (nptr) == N_DECL) && (TagOf (NTypeOf (nptr)) == S_UNDECLARED))
			return (NTypeOf (nptr));
		/*}}} */
		p = FnTypeListOf (NTypeOf (nptr));	/* point p to the result types   */
		if (listitems (p) > 1) {	/* It is a multi-valued function */
			/*chkreport (CHK_INV_FUNC_LIST, chklocn); */
			chkerr (CHK_INV_FUNC_LIST, chklocn);	/* bug TS/1956 10/11/92 */
			return undeclaredp;
		} else {
			return (ThisItem (p));
		}
	}
	/*}}} */
	/*{{{  RECORDSUB */
	case S_RECORDSUB:
		if (TagOf (ASIndexOf (tptr)) == N_FIELD) {
			return (NTypeOf (ASIndexOf (tptr)));
		} else {
			return undeclaredp;
		}
	/*}}} */
	/*{{{  subscript */
	case S_ARRAYSUB:
	{
		treenode *default_base_type = default_type;
		treenode *nptr = tptr;
		treenode *st;
		int subscripts = 0;
		const char *str = NULL;	/* initialised to shut up gcc's optimiser */

		while (TagOf (nptr) == S_ARRAYSUB) {
			subscripts++;
			/*{{{  check subscript expression is of type INT */
			st = typecheck_main (ASIndexOf (nptr), intnodeptr);
			if ((TagOf (st) != S_UNDECLARED) && (TagOf (st) != S_INT)) {
				chkreport (CHK_ASUB_NOT_INT, chklocn);
			}
			/*}}} */
			nptr = ASBaseOf (nptr);
			if (TagOf (follow_user_type (default_base_type)) == S_ARRAY) {
				default_base_type = ARTypeOf (follow_user_type (default_base_type));
			}
		}

		st = typecheck_main (nptr, default_base_type);
		if (TagOf (st) == S_UNDECLARED) {
			return (st);
		}

		switch (TagOf (nptr)) {
		/*{{{  case S_SEGMENT */
		case S_SEGMENT:
		{
			treenode *const name = nameof (nptr);

			if ((TagOf (name) == S_CONSTRUCTOR) || (TagOf (name) == S_STRING) || (TagOf (name) == S_ARRAYCONSTRUCTOR)
			#ifdef OCCAM2_5
			    || (TagOf (name) == S_FINSTANCE)
			#endif
				) {
				str = "table";
			} else {
				str = WNameOf (NNameOf (name));
			}
		}
		break;
		/*}}} */
		/*{{{  case N_DECL, N_ABBR, etc. */
		case N_DECL:
		case N_ABBR:
		case N_VALABBR:
		case N_RETYPE:
		case N_VALRETYPE:
		case N_PARAM:
		case N_VALPARAM:
		case N_RESULTPARAM:
		case N_REPL:
		case N_FIELD:
			str = WNameOf (NNameOf (nptr));
			break;
		/*}}} */
		/*{{{  case S_CONSTRUCTOR case S_STRING case S_ARRAYCONSTRUCTOR */
		case S_STRING:
			str = "string";
			break;
		case S_CONSTRUCTOR:
			str = "table";
			break;
		/*}}} */
		/*{{{  RECORDSUB */
		case S_RECORDSUB:
			str = WNameOf (NNameOf (ASIndexOf (nptr)));
			break;
		/*}}} */
		/*{{{  FINSTANCE */
		#ifdef OCCAM2_5
		case S_FINSTANCE:
			str = "FUNCTION call";
			break;
		#endif
		/*}}} */
		/*{{{  arrayconstructor */
		case S_ARRAYCONSTRUCTOR:
			str = "array constructor";
			break;
		/*}}}  */
		default:
			/*badtag(chklocn /@LocnOf(nptr)@/, (BIT32)TagOf(nptr), "typecheck_main"); */
#if 0
fprintf (stderr, "eep, here!  nptr =");
printtreenl (stderr, 4, nptr);
#endif
			chkreport (CHK_NAMENOTARRAY, chklocn);	/* bug 1088 21/12/90 */
		}
		return arraytype (st, subscripts, str);
	}
	/*}}} */
	/*{{{  valof */
	/*case S_VALOF: *//* moved into 'specification' */
	/*}}} */
	/*{{{  segment */
	case S_SEGMENT:
	{
	#ifdef OCCAM2_5
		/*{{{  insert 'FROM 0' if necessary */
		if (SStartExpOf (tptr) == NULL) {
			const int old = switch_to_real_workspace ();
			SetSStartExp (tptr, newintconstant (0, S_INT));
			switch_to_prev_workspace (old);
		}
		/*}}} */
	#endif
	{
		treenode *aname = SNameOf (tptr);
		treenode *aptr;
		treenode *const sptr = typecheck_main (SStartExpOf (tptr), intnodeptr);
		treenode *const lptr = typecheck_main (SLengthExpOf (tptr), intnodeptr);
		treenode *bptr;

		/* skip over channel direction specifier if present */
		switch (TagOf (aname)) {
		case S_ASINPUT:
		case S_ASOUTPUT:
			aname = OpOf (aname);
			break;
		}
		aptr = follow_user_type (typecheck_main (aname, default_type));
		/*{{{  check the segmented operand is an array */
		if (TagOf (aptr) == S_UNDECLARED) {
			return (aptr);
		}
#ifdef MOBILES
		if (TagOf (aptr) == S_MOBILE) {
			aptr = follow_user_type (MTypeOf (aptr));
		}
#endif
		if (TagOf (aptr) != S_ARRAY)
			chkreport (CHK_INV_SEGMENT_OPERAND, chklocn);
		/*}}} */
		/*{{{  typecheck the start and length expressions, make sure they are INT */
		if ((TagOf (sptr) != S_UNDECLARED) && (TagOf (sptr) != S_INT)) {
			chkreport (CHK_ASUB_NOT_INT, chklocn);
		}
		if ((TagOf (lptr) != S_UNDECLARED) && (TagOf (lptr) != S_INT)) {
			chkreport (CHK_ASUB_NOT_INT, chklocn);
		}
		/*}}} */

		bptr = newtypenode (S_ARRAY, NOPOSN, NULL, ARTypeOf (aptr));
		#ifdef OCCAM2_5
		if ((SLengthExpOf (tptr) == NULL) && (ARDimOf (aptr) != -1) && isconst (SStartExpOf (tptr))) {
			/*{{{  insert constant 'FOR n' */
			const int old = switch_to_real_workspace ();

			if (TagOf (SStartExpOf (tptr)) != S_CONSTEXP) {
				SetSStartExp (tptr, newconstexp (SStartExpOf (tptr)));
			}
			SetSLengthExp (tptr, newintconstant ((BIT32) ARDimOf (aptr) - LoValOf (SStartExpOf (tptr)), S_INT));
			switch_to_prev_workspace (old);
			/*}}} */
		} else if (SLengthExpOf (tptr) == NULL) {
			/*{{{  insert variable 'FOR ((SIZE array) - start)*/
			const int old = switch_to_real_workspace ();
			treenode *tmpexp;

			tmpexp = newdopnode (S_SUBTRACT, NOPOSN, newmopnode (S_SIZE, NOPOSN, aname, S_INT), copytree (SStartExpOf (tptr), syn_lexlevel), S_INT);
#if 0
fprintf (stderr, "typecheck_main(): SEGMENT: no length, (maybe) want to insert a variable one!  tmpexp =");
printtreenl (stderr, 4, tmpexp);
#endif
			SetSLengthExp (tptr, tmpexp);
			switch_to_prev_workspace (old);
			/*}}}*/
		}
		#endif
		if (isconst (SLengthExpOf (tptr))) {
			/*{{{  fold the segment length expression and store in our new array node */
			if (TagOf (SLengthExpOf (tptr)) != S_CONSTEXP) {	/* it's not already folded */
				/*{{{  fold it */
				const int old = switch_to_real_workspace ();

				SetSLengthExp (tptr, newconstexp (SLengthExpOf (tptr)));
				switch_to_prev_workspace (old);
				/*}}} */
			}
			SetARDim (bptr, LoValOf (SLengthExpOf (tptr)));
			/*}}} */
		} else {
			SetARDim (bptr, -1);	/* Match anything */
		}
		SetARDimLength (bptr, SLengthExpOf (tptr));
		return (bptr);
	}
	}
	/*}}} */
	/*{{{  constructor */
	case S_CONSTRUCTOR:
	{
		/*{{{  decide what is the default type */
		#ifdef OCCAM2_5
			#ifdef USER_DEFINED_OPERATORS
				treenode *default_array_type;
				if (modify_check_for_udo) {
					default_array_type = (LitTypeOf (tptr) != NULL) ? LitTypeOf (tptr) : unknownnodeptr;
				} else {
					default_array_type = (LitTypeOf (tptr) != NULL) ? LitTypeOf (tptr) : default_type;
				}
			#else
				/*{{{  old code for this bit if no user_defined_operators */
				treenode *const default_array_type = (LitTypeOf (tptr) != NULL) ? LitTypeOf (tptr) : default_type;
				/*}}} */
			#endif
		#else
			treenode *const default_array_type = default_type;
		#endif
		/*}}} */

#if 0
fprintf (stderr, "typecheck_main: on S_CONSTRUCTOR:");
printtreenl (stderr, 4, tptr);
#endif

		/*{{{  check for record constructor */
		#ifdef OCCAM2_5
			if (TagOf (follow_user_type (default_array_type)) == S_RECORD) {
				/*{{{  record constructor */
				treenode *const rec_type = follow_user_type (default_array_type);
				treenode *decl_list = ARTypeOf (rec_type);
				treenode *exp_list = LitExpOf (tptr);
				for (; (decl_list != NULL) && !EndOfList (exp_list); decl_list = DBodyOf (decl_list), exp_list = NextItem (exp_list)) {
					/*{{{  check each field */
					treenode *const decl_type = NTypeOf (DNameOf (decl_list));
					treenode *const this_t = typecheck_main (ThisItem (exp_list), decl_type);
					if (!typesequivalent (decl_type, this_t, FALSE))
						chkreport_s (CHK_REC_LIT_TYPE_MISMATCH, LocnOf (exp_list), WNameOf (NNameOf (DNameOf (decl_list))));
					/*}}} */
				}
				/*}}} */
				/*{{{  check for too long / too small */
				if (decl_list != NULL)
					chkreport (CHK_REC_LIT_TOO_SMALL, LocnOf (LitExpOf (tptr)));
				if (!EndOfList (exp_list))
					chkreport (CHK_REC_LIT_TOO_LARGE, LocnOf (LitExpOf (tptr)));
				/*}}} */
				/*{{{  type coercion */
				if (LitTypeOf (tptr) == NULL) {
					#if TYPE_COERCION
						SetLitType (tptr, default_array_type);
					#else
						type_mismatch (default_array_type, FALSE);
					#endif
				}
				/*}}} */
				return default_array_type;
			}
		#endif
		/*}}} */
		/*{{{  otherwise - array constructor */
		{
			treenode *aptr = NULL;
			treenode *constructor_type = NULL;
			treenode *cptr;
			BIT32 clength;
			#if TYPE_COERCION
				int default_elt_number = -1;
			#endif
			treenode *default_elt_type;
			
			if (TagOf (follow_user_type (default_array_type)) == S_ARRAY) {
				default_elt_type = ARTypeOf (follow_user_type (default_array_type));
			} else {
				default_elt_type = default_array_type;
			}

			if ((TagOf (default_elt_type) == S_ASINPUT) || (TagOf (default_elt_type) == S_ASOUTPUT)) {
				default_elt_type = OpOf (default_elt_type);
			}

			/*{{{  pick a field with known type */
			#if TYPE_COERCION
			{
				int elt_number;
				BIT32 chandirspec = 0;		/* used to check that they're all the same here */

				for (elt_number = 0, cptr = LitExpOf (tptr);
				     !EndOfList (cptr) && (constructor_type == NULL); cptr = NextItem (cptr), elt_number++) {
					if (typeknown (ThisItem (cptr))) {
						treenode *item = ThisItem (cptr);

						#ifdef USER_DEFINED_OPERATORS
							modify_check = modify_check_for_udo;
						#endif
#if 0
fprintf (stderr, "typecheck_main: S_CONSTRUCTOR: current node is:");
printtreenl (stderr, 4, item);
#endif
						if ((TagOf (item) == S_ASINPUT) || (TagOf (item) == S_ASOUTPUT)) {
							BIT32 expected = ((TagOf (item) == S_ASINPUT) ? TypeAttr_marked_in : TypeAttr_marked_out);

							/* remove specifier */
							SetLeft (cptr, OpOf (item));
							item = OpOf (item);

#if 0
fprintf (stderr, "typecheck_main: S_CONSTRUCTOR: it was input or output, now:");
printtreenl (stderr, 4, item);
#endif
							constructor_type = typecheck_main (item, default_elt_type);
							if (!chandirspec) {
								chandirspec = expected;
							} else if (chandirspec != expected) {
								chkreport (CHK_CONSTRUCTOR_BAD_CHANDIR, LocnOf (cptr));
							}
							if (TagOf (constructor_type) == S_CHAN) {
								BIT32 cdhere = TypeAttrOf (constructor_type) & (TypeAttr_marked_in | TypeAttr_marked_out);

#ifdef MOBILES
								treenode *ibase = item;
								treenode *basetype;

								/* if the item is a mobile channel-type subscript on a CLIENT end, invert the chan-direction */
								while ((TagOf (ibase) == S_RECORDSUB) || (TagOf (ibase) == S_ARRAYSUB)) {
									ibase = ASBaseOf (ibase);
								}
								basetype = typecheck_main (ibase, unknownnodeptr);
								if ((TagOf (basetype) == N_TYPEDECL) && (TagOf (NTypeOf (basetype)) == S_MOBILE)) {
									if (NTypeAttrOf (basetype) & TypeAttr_marked_out) {
										cdhere ^= (TypeAttr_marked_in | TypeAttr_marked_out);
									}
								}
#if 0
fprintf (stderr, "typecheck_main: S_CONSTRUCTOR checking ASINPUT/ASOUTPUT: basetype of item = ");
printtreenl (stderr, 4, basetype);
#endif
#endif
#if 0
fprintf (stderr, "typecheck_main: S_CONSTRUCTOR checking ASINPUT/ASOUTPUT: expected = %x, cdhere = %x, item = ", expected, cdhere);
printtreenl (stderr, 4, item);
#endif
								if (cdhere) {
									if (((cdhere & TypeAttr_marked_in) && (expected != TypeAttr_marked_in)) ||
											((cdhere & TypeAttr_marked_out) && (expected != TypeAttr_marked_out))) {
#if 0
fprintf (stderr, "typecheck_main: fail at 2\n");
#endif
										chkreport (CHK_BAD_CHAN_CONFLICT_ANON, LocnOf (cptr));
									}
								}
							}
						} else {
							constructor_type = typecheck_main (item, default_elt_type);
						}
						/*{{{  COMMENT resolve untyped literals */
						/*#ifdef USER_DEFINED_OPERATORS */
						/*if (modify_check_for_udo) */
						/*{ */
						/*constructor_type = replace_resolved_lit_type(ThisItem(cptr),constructor_type); */
						/*replace_resolved_lit_type_in_node(ThisItem(cptr)); */
						/*} */
						/*#endif */
						/*}}} */
						if (TagOf (default_elt_type) == S_UNKNOWN) {
							default_elt_type = constructor_type;
						}
						default_elt_number = elt_number;
					}
				}
				if (chandirspec && constructor_type && (TagOf (constructor_type) == S_CHAN)) {
					/* copy the type and bung this in */
					const int old = switch_to_real_workspace ();

					constructor_type = copytree (constructor_type, syn_lexlevel);
					default_elt_type = constructor_type;
					switch_to_prev_workspace (old);
					SetTypeAttr (constructor_type, (TypeAttrOf (constructor_type) & ~(TypeAttr_marked_in | TypeAttr_marked_out))| chandirspec);
				}
#if 0
fprintf (stderr, "typecheck_main: S_CONSTRUCTOR: constructor type is:");
printtreenl (stderr, 4, constructor_type);
#endif
				/*printf("constructor_type is %s\n", itagstring(TagOf(constructor_type))); */
			}
			#endif
			/*}}} */

#if 0
fprintf (stderr, "typecheck_main: S_CONSTRUCTOR: LitExpOf (tptr) is:");
printtreenl (stderr, 4, LitExpOf (tptr));
#endif
			for (cptr = LitExpOf (tptr), clength = 0; !EndOfList (cptr); cptr = NextItem (cptr), clength++) {
				/*{{{  check one element of the table at a time */
				treenode *this_t;	/* was const expr */

				#ifdef OCCAM2_5
					/* bug INSdi03538 - don't call typecheck_main on the default
					   element more than once.
					 */
					#ifdef USER_DEFINED_OPERATORS
						modify_check = modify_check_for_udo;
					#endif
#if 0
fprintf (stderr, "Current node is:");
printtreenl (stderr, 4, ThisItem (cptr));
fprintf (stderr, "default_elt_type is:");
printtreenl (stderr, 0, default_elt_type);
#endif
					if (clength == default_elt_number) {
						this_t = constructor_type;
					} else {
						if (TagOf (default_elt_type) == S_CHAN) {
							/* check and remove any channel direction specifiers */
							BIT32 expected = TypeAttrOf (default_elt_type) & (TypeAttr_marked_in | TypeAttr_marked_out);
							treenode *item = ThisItem (cptr);

							if ((TagOf (item) == S_ASINPUT) || (TagOf (item) == S_ASOUTPUT)) {
								BIT32 spec = (TagOf (item) == S_ASINPUT) ? TypeAttr_marked_in : TypeAttr_marked_out;
								
								item = OpOf (item);
								SetLeft (cptr, item);
								if (expected && spec && (expected ^ spec)) {
#if 0
fprintf (stderr, "typecheck_main: fail at 3.  expected = %x, spec = %x, item = ", expected, spec);
printtreenl (stderr, 4, item);
#endif
									chkreport (CHK_BAD_CHAN_CONFLICT_ANON, LocnOf (cptr));
								}
							}
						}
						this_t = typecheck_main (ThisItem (cptr), default_elt_type);
					}
				#endif
				typecheck_main (ThisItem (cptr), default_elt_type);
				/*{{{  COMMENT coerce literal to INT/REAL32 if check modified */
				/*#ifdef USER_DEFINED_OPERATORS */
				/*if (modify_check_for_udo) */
				/*{ */
				/*this_t = replace_resolved_lit_type(ThisItem(cptr),this_t); */
				/*replace_resolved_lit_type_in_node(ThisItem(cptr)); */
				/*} */
				/*#endif */
				/*}}} */
				/*{{{  COMMENT more debugging code */
				/*fprintf(stderr,"\nAfter coersion routines called\n"); */
				/*printtree(stderr,0,ThisItem(cptr)); */
				/*fprintf(stderr,"\nthis_t is -------------\n"); */
				/*printtree(stderr,0,this_t); */
				/*fprintf(stderr,"\n--------- -------------\n"); */
				/*}}} */
				/*printf("this_t is %s\n", itagstring(TagOf(this_t))); */

				if (TagOf (this_t) == S_UNDECLARED) {
					if (aptr == NULL) {
						aptr = this_t;
					}
				}
				if (elementsin (this_t) == (-1)) {
					/* N.B. As an implementation restriction we don't allow any
					   open dimensions in the type of a constructor,
					   and therefore in the components of it. */
					/* bug TS/2058 29/01/93 - give a sensible line number for the error */
					/*chkreport(CHK_UNKNOWN_CONSTRUCTOR_SIZE, chklocn); */
					chkreport (CHK_UNKNOWN_CONSTRUCTOR_SIZE, LocnOf (cptr));
					{
						if (aptr == NULL) {
							aptr = this_t;
						}
					}
				} else if (constructor_type == NULL) {
					/* first element */
					constructor_type = this_t;
				} else if (!typesequivalent (constructor_type, this_t, FALSE)) {
					/* bug TS/2058 29/01/93 - give a sensible line number for the error */
					/*chkreport (CHK_CONSTRUCTOR_TYPE_MISMATCH, chklocn); */
					chkreport (CHK_CONSTRUCTOR_TYPE_MISMATCH, LocnOf (cptr));
				}
				/*}}} */
			}

			if (aptr == NULL) {
				/* everything ok so far */
				/*{{{  fabricate an array node */
				aptr = newtypenode (S_ARRAY, NOPOSN, newconstant (clength), constructor_type);
				SetARDim (aptr, clength);
				/*}}} */
			}

			/*{{{  check/recreate a qualified constructor type */
			#ifdef OCCAM2_5
				if (LitTypeOf (tptr) != NULL) {
					treenode *const decor_type = follow_user_type (LitTypeOf (tptr));
					if (!typesequivalent (aptr, decor_type, FALSE))
						chkreport (CHK_BAD_CONSTRUCTOR_TYPE, LocnOf (LitExpOf (tptr)));
					aptr = LitTypeOf (tptr);
				}
				#if TYPE_COERCION
					#ifdef USER_DEFINED_OPERATORS
						else if ((TagOf (default_type) == N_TYPEDECL) && (!modify_check_for_udo))
					#else
						else if (TagOf (default_type) == N_TYPEDECL)
					#endif
					{
						treenode *const default_shape = follow_user_type (default_type);
						if (typesequivalent (aptr, default_shape, FALSE))
							aptr = default_type;
					}
				#endif
			#endif
			/*}}} */
			/*if (modify_check_for_udo)
			   SetLitType(tptr,aptr); *//* this caused a quite serious bug */
#if 0
fprintf (stderr, "typecheck_main: S_CONSTRUCTOR: returning type:");
printtreenl (stderr, 4, aptr);
#endif
			return (aptr);
		}
		/*}}} */
	}
	/*}}} */
	/*{{{  null array*/
	case S_NULLARRAY:
	{
		treenode *aptr = NULL;

		if (TagOf (default_type) == S_ARRAY) {
			aptr = newtypenode (S_ARRAY, NOPOSN, newconstant (0), ARTypeOf (default_type));
			SetARDim (aptr, 0);
		} else {
			chkreport (CHK_TYPE_MISMATCH, LocnOf (tptr));
		}
		return aptr;
	}
	/*}}}*/
	/*{{{  structconstructor */
	#if 0
	case S_STRUCTCONSTRUCTOR:
		return (typenodeptr (MOpTypeOf (tptr)));
	#endif
	/*}}} */
	/*{{{  arrayconstructor */
	case S_ARRAYCONSTRUCTOR:
	{
		treenode *aptr, *st, *vvtmp, *vvitmp;

#if 0
fprintf (stderr, "typecheck_main: on S_ARRAYCONSTRUCTOR:");
printtreenl (stderr, 4, tptr);
#endif
		/*{{{  typecheck start, length, (+step) for INTness */
		st = typecheck_main (ReplCStartExpOf (tptr), intnodeptr);
		if (TagOf (st) == S_UNDECLARED) {
			st = intnodeptr;
		}
		if (TagOf (st) != S_INT) {
			chkreport (CHK_ASUB_NOT_INT, chklocn);
		}
		st = typecheck_main (ReplCLengthExpOf (tptr), intnodeptr);
		if (TagOf (st) == S_UNDECLARED) {
			st = intnodeptr;
		}
		if (TagOf (st) != S_INT) {
			chkreport (CHK_ASUB_NOT_INT, chklocn);
		}
		if (ReplCStepExpOf (tptr)) {
			st = typecheck_main (ReplCStepExpOf (tptr), intnodeptr);
			if (TagOf (st) == S_UNDECLARED) {
				st = intnodeptr;
			}
			if (TagOf (st) != S_INT) {
				chkreport (CHK_ASUB_NOT_INT, chklocn);
			}
		}
		/*}}}  */
		/* typecheck the expression.  default_type should have the desired resulting array type, or UNKNOWN */
		if (TagOf (default_type) == S_ARRAY) {
			aptr = typecheck_main (ReplCBodyOf (tptr), ARTypeOf (default_type));
		} else {
#if 0
			/* this should not happen.. -- um... */
fprintf (stderr, "typecheck_main: FIXME! default_type:");
printtreenl (stderr, 4, default_type);
			badtag (chklocn, TagOf (default_type), "typecheck_main: ARRAYCONSTRUCTOR");
#endif
			aptr = typecheck_main (ReplCBodyOf (tptr), default_type);
		}
		/* try and constant-fold the replicated expression */
		SetReplCBody (tptr, foldexp (ReplCBodyOf (tptr)));
#if 0
fprintf (stderr, "typecheck_main: S_ARRAYCONSTRUCTOR: here at 1!  tptr =");
printtreenl (stderr, 4, tptr);
fprintf (stderr, "typecheck_main: S_ARRAYCONSTRUCTOR: here at 2!\n");
#endif
		if (TagOf (aptr) == S_UNDECLARED) {
			return aptr;
		}
		aptr = newtypenode (S_ARRAY, NOPOSN, NULL, aptr);
#if 0
fprintf (stderr, "typecheck_main: S_ARRAYCONSTRUCTOR: here at 3!\n");
#endif

		if (isconst (ReplCLengthExpOf (tptr)) && (elementsin (chk_gettype (ReplCBodyOf (tptr))) != (-1))) {
#if 0
fprintf (stderr, "typecheck_main: S_ARRAYCONSTRUCTOR: here at 4!\n");
#endif
			/* constant fold length expression into replicator */
			if (TagOf (ReplCLengthExpOf (tptr)) != S_CONSTEXP) {
				int old = switch_to_real_workspace ();
				SetReplCLengthExp (tptr, newconstexp (ReplCLengthExpOf (tptr)));
				switch_to_prev_workspace (old);
			}
			SetARDimLength (aptr, copytree (ReplCLengthExpOf (tptr), 0));
			SetARDim (aptr, LoValOf (ReplCLengthExpOf (tptr)));
		} else {
			/* FIXME: can only have constant length expressions at the moment, due to allocation of the temporary..
			 * could use the dynamic memory stuff to provide the temporary array if necessary.. */
			if (0 && (TagOf (default_type) == S_ARRAY)) {
				SetARDimLength (aptr, ReplCLengthExpOf (tptr));
				SetARDim (aptr, ARDimOf (default_type));
			} else {
				chkreport (CHK_UNKNOWN_CONSTRUCTOR_SIZE, chklocn);
				SetARDim (aptr, -1);
			}
		}
#if 0
fprintf (stderr, "typecheck_main: S_ARRAYCONSTRUCTOR: here at 5!\n");
#endif
		/* perform mangling into VALOF process */
		vvtmp = ReplCTempOf (tptr);
		vvitmp = ThisItem (NextItem (vvtmp));
		vvtmp = ThisItem (vvtmp);
		SetNType (vvtmp, aptr);
#if 0
fprintf (stderr, "typecheck_main: before call to mangle_tree_for_arrayconstructor, tptr = ");
printtreenl (stderr, 4, tptr);
#endif
		mangle_tree_for_arrayconstructor (tptr, aptr, ReplCNameOf (tptr), ReplCStartExpOf (tptr), ReplCLengthExpOf (tptr), ReplCStepExpOf (tptr), ReplCBodyOf (tptr), vvtmp, vvitmp);
		return typecheck_main (tptr, default_type);
	}
	/*}}} */
	/*{{{  conditional expression */
	#ifdef CONDEXP
	case S_CONDEXP:
	{
		char buf[MAX_ERR_SIZE];

#if 0
fprintf (stderr, "typecheck_main: processing CONDEXP: ");
printtreenl (stderr, 4, tptr);
#endif
		t = typecheck_main (CondExpGuardOf (tptr), S_BOOL);
		if (TagOf (t) == S_UNDECLARED)
			t = boolnodeptr;
		if (TagOf (t) != S_BOOL)
			chkreport (CHK_CONDEXP_NOT_BOOL, chklocn);
		ftagstring (buf, S_CONDEXP, NULL, 0, NULL);
		t = checksame (CondExpTrueOf (tptr), CondExpFalseOf (tptr), type, CHK_TYPES_DIFF, buf);
		/*{{{  check type is legal */
		switch (TagOf (t)) {
		case S_BOOL:
		case S_BYTE:
		case S_INT:
		case S_INT16:
		case S_INT32:
		case S_INT64:
		case S_UINT:
		case S_UINT16:
		case S_UINT32:
		case S_UINT64:
		case S_REAL32:
		case S_REAL64:
		case S_UNDECLARED:
			CASE_CONFIG_TYPE break;
		default:
			t = undeclaredp;	/* bug 1279 22/8/91 */
			chk_invtype (chklocn, TagOf (tptr));
			break;
		}
		SetCondExpType (tptr, TagOf (t));
		/*}}} */
		return (t);
	}
	#endif
	/*}}} */
	/*{{{  specification */
	case S_VALABBR:
	case S_ABBR:
	case S_VALRETYPE:
	case S_RETYPE:
	case S_PROCDEF:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
	case S_TPROTDEF:
	case S_SPROTDEF:
#ifdef MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
	case S_MPROCDECL:
#endif
	case S_DECL:
	case S_PLACE:
	case S_VSPLACE:
	case S_WSPLACE:
	case S_PLACEON:
	case S_PRAGMA:			/* bug 829 20/9/91 */
	case S_VALOF:
		tptr = skipspecifications (tptr);
		/*{{{  now we must have a VALOF */
		if (TagOf (tptr) == S_VALOF) {
			treenode *const nptr = VLResultListOf (tptr);
			treenode *const res = typecheck_main (ThisItem (nptr), default_type);
			if (listitems (nptr) > 1) {	/* It is a multivalued VALOF */
				/*chkreport (CHK_INV_VALOF_LIST, chklocn); */
				chkerr (CHK_INV_VALOF_LIST, chklocn);	/* bug TS/1956 10/11/92 */
				return undeclaredp;
			} else {
				check_valof_types (tptr);	/* bug TS/1442 4/11/91 */
				return res;
			}
		} else {
			msg_out_i (SEV_INTERNAL, CHK, CHK_LOST_VALOF, chklocn, TagOf (tptr));
			return NULL;
		}
		/*}}} */
		break;
	/*}}} */
		/*{{{  LIST -- not expected here, but can happen in certain erroneous cases*/
	case S_LIST:
		return undeclaredp;
		/*}}}*/
	default:
		badtag (chklocn, TagOf (tptr), "typecheck_main");
		return (NULL);
	}
	return NULL;
}

/*}}}*/
/*{{{  PUBLIC treenode *typecheck (tptr, default_type)*/
PUBLIC treenode *typecheck (treenode * const tptr, treenode * const default_type)
{
	treenode *t;
	const typecheck_params_t *const saved_params = typecheck_params;

	/* Generate any temporary nodes in the temporary free space */
	const int old = switch_to_temp_workspace ();

	typecheck_params = NULL;
	t = typecheck_main (tptr, default_type);

	/* Switch back to real workspace */
	switch_to_prev_workspace (old);
	typecheck_params = saved_params;
	return (t);
}

/*}}}*/
/*{{{  PRIVATE treenode *typecheck_param (tptr, default_type)*/
PRIVATE treenode *typecheck_param (treenode * const tptr, treenode * const default_type, const wordnode * const fn_name, const int paramno)
{
	treenode *t;
	const typecheck_params_t *saved_params = typecheck_params;
	typecheck_params_t local_params;
	/* Generate any temporary nodes in the temporary free space */
	const int old = switch_to_temp_workspace ();

	/*printf("typecheck_param: type is %s\n", itagstring(type)); */

	local_params.fn_name = fn_name;
	local_params.paramno = paramno;
	typecheck_params = &local_params;

#if 0
fprintf (stderr, "typecheck_param: tptr =");
printtreenl (stderr, 4, tptr);
fprintf (stderr, "typecheck_param: default_type =");
printtreenl (stderr, 4, default_type);
#endif
	t = typecheck_main (tptr, default_type);

	/* Switch back to real workspace */
	switch_to_prev_workspace (old);
	typecheck_params = saved_params;

	return (t);
}

/*}}}*/

#ifdef USER_DEFINED_OPERATORS
/*{{{  PUBLIC treenode *checksamenew (t1, t2, RETURN(left_type, right_type), type, e, s)*/
/*{{{  COMMENT checksame's comment*/
 /*Type checks t1 and t2, */
/*and then checks that their types are equal,*/
/*and returns the common type (in 'current' workspace (normally temp)).*/
/*default_type is a type tag used to resolve untyped integer and real literals*/
/*where possible.*/
 /**/
/*Modifications for User defined operators - the types returned by the checking*/
/*of the left and right subtrees (by typecheck_main()) are saved in global*/
/*variables left_type and right_type for use by the udo code later (for udo*/
/*function name generation).  typecheck_main() cannot simply be called again*/
/*for the same expression tree because it has side effects.  Also error*/
/*conditions noted, but not raised yet - as some states that would normally*/
/*cause an error are not if a user defined operator has been defined (for*/
/*example if the types of the left and right subtree's are different).*/
/*Note the setting of the global variable modify_check to TRUE before calling*/
/*typecheck_main each time - this causes untyped literals to resolved, but*/
/*have a node flag set if the default_type was used to resolve it - if used*/
/*with a user defined operator this allows*/
	 /**/
/*}}}*/
PUBLIC treenode *checksamenew (treenode * t1, treenode * t2, treenode ** left, treenode ** right, BOOL * passed, treenode * default_type)
{
	treenode *l;
	BOOL swap;

	if ((TagOf (default_type) == S_UNKNOWN) && !typeknown (t1)) {
		/*{{{  swap t1 and t2 */
		treenode *const temp = t1;
		t1 = t2;
		t2 = temp;
		swap = TRUE;
		/*{{{  debugging code */
		/*fprintf(stderr,"swap done in checksamenew()\n"); */
		/*}}} */
		/*}}} */
	} else {
		swap = FALSE;
	}
	modify_check = TRUE;
	l = typecheck_main (t1, default_type);
	if (swap) {
		*right = l;
	} else {
		*left = l;
	}
	if (TagOf (l) == S_UNDECLARED) {	/* is this an error condition?? */
		modify_check = TRUE;
		typecheck (t2, default_type);
		*passed = TRUE;	/* FALSE;  I hope :-) */
		/* fprintf(stderr,"Suspected error point reached!"); */
		return l;
	} else {
		/*{{{  check right is the same as the left */
		treenode *temp;
		if (TagOf (default_type) == S_UNKNOWN) {
			/*default_type = basetype_tree(l); */
			default_type = l;
		}
		modify_check = TRUE;
		temp = typecheck_main (t2, default_type);

		if (swap) {
			*left = temp;
			*right = l;
		} else {
			/* adding to other (so both were done) cured serious bug (see rcslog) */
			*right = temp;
			*left = l;
		}
#if 0
fprintf (stderr, "chk1: in checksamenew..  l = ");
printtreenl (stderr, 4, l);
fprintf (stderr, "chk1: in checksamenew..  temp = ");
printtreenl (stderr, 4, temp);
#endif
#ifdef MOBILES
		if ((TagOf (l) == S_MOBILE) && (TagOf (temp) != S_MOBILE)) {
			if (typesequivalent (MTypeOf (l), temp, FALSE)) {
				*passed = TRUE;
				return MTypeOf (l);
			}
		} else if ((TagOf (l) != S_MOBILE) && (TagOf (temp) == S_MOBILE)) {
			if (typesequivalent (l, MTypeOf (temp), FALSE)) {
				*passed = TRUE;
				return l;
			}
		}
#endif
		if (typesequivalent (l, temp, FALSE)) {
			*passed = TRUE;
			return (l);
		} else {
			*passed = TRUE;
			/*chkreport_s(e, chklocn, s); */
			return (NULL);
		}
		/*}}} */
	}
}

/*}}}*/
#endif
/*{{{  PUBLIC treenode *checksame (t1, t2, type, e, s)*/
/*
 *	Type checks t1 and t2,
 *	and then checks that their types are equal,
 *	and returns the common type (in 'current' workspace (normally temp)).
 *	default_type is a type tag used to resolve untyped integer and real literals
 *	where possible.
 *
 *	for assignment checks, t1 is the lhs, t2 is the rhs
 */
PUBLIC treenode *checksame (treenode *t1, treenode *t2, treenode *default_type, const int e, const char *const s)
{
	treenode *l, *l2;

#if 0
printf ("checksame: t1 = ");
printtreenl (stdout, 2, t1);
printf ("t2 = ");
printtreenl (stdout, 2, t2);
printf ("default_type = ");
printtreenl (stdout, 2, default_type);
#endif
	if ((TagOf (default_type) == S_UNKNOWN) && !typeknown (t1)) {
		/*{{{  swap t1 and t2 */
		treenode *const temp = t1;
		t1 = t2;
		t2 = temp;
		/*}}} */
	}
	l = typecheck_main (t1, default_type);

	if (TagOf (l) == S_UNDECLARED) {
#if 0
printf ("MOBILES: TagOf(l) is S_UNDECLARED.  l = ");
printtree (stdout, 2, l);
printf ("\n");
#endif
		typecheck (t2, default_type);

		return l;
	} else {
		/*{{{  check right is the same as the left */
		if (TagOf (default_type) == S_UNKNOWN) {
			/*default_type = basetype_tree(l); */
			default_type = l;
		}
		l2 = typecheck_main (t2, default_type);
		#ifdef MOBILES
		{
			/* if t1 type is MOBILE and t2 type is not, type becomes the non-mobile version */
			treenode *const l_utype = follow_user_type (l);
			treenode *const l2_utype = follow_user_type (l2);

#if 0
printf ("MOBILES: l type-tree = ");
printtree (stdout, 2, l);
printf ("\nMOBILES: l2 type-tree = ");
printtree (stdout, 2, l2);
printf ("\n");
#endif
			/* do any-chan-type checks first */
			if ((TagOf (l) == S_ANYCHANTYPE) && (TagOf (l2) == S_ANYCHANTYPE)) {
				/* both, so must have same attributes */
				INT32 lattr = (LeafLinkOf (l) ? TypeAttrOf (LeafLinkOf (l)) : 0) & (TypeAttr_shared | TypeAttr_marked_in | TypeAttr_marked_out);
				INT32 l2attr = (LeafLinkOf (l2) ? TypeAttrOf (LeafLinkOf (l2)) : 0) & (TypeAttr_shared | TypeAttr_marked_in | TypeAttr_marked_out);

				if (lattr ^ l2attr) {
					chkreport (CHK_ANYCHANTYPE_INCOMPAT, chklocn);
					return NULL;
				}
				return l;
			} else if ((TagOf (l) == S_ANYCHANTYPE) && isdynmobilechantypetype (l2)) {
				/* assignment to any-chan-type from mobile channel-end */
				INT32 lattr = (LeafLinkOf (l) ? TypeAttrOf (LeafLinkOf (l)) : 0) & (TypeAttr_shared | TypeAttr_marked_in | TypeAttr_marked_out);
				INT32 l2attr = NTypeAttrOf (l2) & (TypeAttr_shared | TypeAttr_marked_in | TypeAttr_marked_out);

				if (((lattr & TypeAttr_shared) && !(l2attr & TypeAttr_shared)) ||
						(!(lattr & TypeAttr_shared) && (l2attr & TypeAttr_shared)) ||
						((lattr & TypeAttr_marked_in) && (l2attr & TypeAttr_marked_out)) ||
						((lattr & TypeAttr_marked_out) && (l2attr & TypeAttr_marked_in))) {
					chkreport_s (e, chklocn, s);
					return NULL;
				}
				return l;
			} else if ((TagOf (l2) == S_ANYCHANTYPE) && isdynmobilechantypetype (l)) {
				/* assignment from any-chan-type to a mobile channel-end */
				INT32 lattr = NTypeAttrOf (l) & (TypeAttr_shared | TypeAttr_marked_in | TypeAttr_marked_out);
				INT32 l2attr = (LeafLinkOf (l2) ? TypeAttrOf (LeafLinkOf (l2)) : 0) & (TypeAttr_shared | TypeAttr_marked_in | TypeAttr_marked_out);

				if (((lattr & TypeAttr_shared) && !(l2attr & TypeAttr_shared)) ||
						(!(lattr & TypeAttr_shared) && (l2attr & TypeAttr_shared)) ||
						((lattr & TypeAttr_marked_in) && (l2attr & TypeAttr_marked_out)) ||
						((lattr & TypeAttr_marked_out) && (l2attr & TypeAttr_marked_in))) {
					chkreport_s (e, chklocn, s);
					return NULL;
				}
				return l2;
			}

			/* any-proc-type checks next */
			if ((TagOf (l) == S_ANYPROCTYPE) && (TagOf (l2) == S_ANYPROCTYPE)) {
				return l;
			} else if ((TagOf (l) == S_ANYPROCTYPE) && isdynmobileproctypetype (l2)) {
				/* assignment to any-proc-type from mobile proc-type */
				return l;
			} else if ((TagOf (l2) == S_ANYPROCTYPE) && isdynmobileproctypetype (l)) {
				/* assignment from any-proc-type to mobile proc-type */
				return l2;
			}

			/* any-mobile-type checks */
			if ((TagOf (l) == S_ANYMOBILETYPE) && (TagOf (l2) == S_ANYPROCTYPE)) {
				return l;
			} else if ((TagOf (l) == S_ANYMOBILETYPE) && (TagOf (l2_utype) == S_MOBILE)) {
				/* assignment to any-proc-type from mobile proc-type */
				return l;
			} else if ((TagOf (l2) == S_ANYMOBILETYPE) && (TagOf (l_utype) == S_MOBILE)) {
				/* assignment from any-proc-type to mobile proc-type */
				return l2;
			}

			if ((TagOf (l_utype) == S_MOBILE) && (TagOf (l2_utype) != S_MOBILE)) {
				/* typesequivalent doesn't do RECORDs, but they've been type-checked already */
				if ((TagOf (MTypeOf (l_utype)) == S_RECORD) && (TagOf (l2_utype) == S_RECORD)) {
					return l2;
				} else if (typesequivalent (MTypeOf (l_utype), l2_utype, FALSE)) {
					return l2;
				}
			} else if ((TagOf (l_utype) != S_MOBILE) && (TagOf (l2_utype) == S_MOBILE)) {
				if ((TagOf (l_utype) == S_RECORD) && (TagOf (MTypeOf (l2_utype)) == S_RECORD)) {
					return l;
				} else if (typesequivalent (l_utype, MTypeOf (l2_utype), FALSE)) {
					return l;
				}
			}
		}
		#endif	/* MOBILES */
		if (typesequivalent (l, l2, FALSE)) {
			return (l);
		} else {
			chkreport_s (e, chklocn, s);
			return (NULL);
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *chk_gettype_main (treenode *tptr, BOOL orig_type)*/
/*{{{  comment*/
/* Assumes that the tree has already been type-checked. */
/* N.B. This function returns a pointer to a type tree which already
exists, so the return value should only be used for  comparsion and
not for inserting elsewhere on the tree.
To insert a type tree elsewhere, first copy it using function
'copytree' If orig_type is TRUE then we return the user defined type
 otherwise we use the function follow_user_type to return the primitive type.
*/
/*}}}*/
PUBLIC treenode *chk_gettype_main (treenode *tptr, BOOL orig_type)
{
	if (TagOf (tptr) == S_UNDEFINED) {
		/* these get inserted occasionally -- need to step over */
		tptr = OpOf (tptr);
	}
	switch (TagOf (tptr)) {
		/*{{{  node type */
		/*{{{  monadic operators */
		/*{{{  NEG BITNOT NOT UMINUS */
	case S_NEG:
	case S_BITNOT:
	case S_NOT:
	case S_UMINUS:
#ifdef MOBILES
	case S_DEFINED:
#endif
		return (typenodeptr (MOpTypeOf (tptr)));
		/*}}} */
		/*{{{  SIZE ELSIZE SEGSTART */
	case S_ADDRESSOF:
	case S_SIZE:
	case S_ELSIZE:
	case S_SEGSTART:
	case S_DUMMYEXP:
#ifdef OCCAM2_5
	case S_BYTESIN:
#endif
		return (intnodeptr);
		/*}}} */
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
	case S_LSHIFT:
	case S_RSHIFT:
	case S_AND:
	case S_OR:
	case S_PLUS:
	case S_MINUS:
	case S_TIMES:
	case S_CSUB0:
	case S_CCNT1:
	case S_OVERLAPCHECK:
	case S_EVAL:
		return (typenodeptr (DOpTypeOf (tptr)));
		/*}}} */
		/*{{{  relational operators */
	case S_EQ:
	case S_NE:
	case S_LS:
	case S_LE:
	case S_GR:
	case S_GE:
	case S_AFTER:
		return (boolnodeptr);
		/*}}} */
		/*{{{  mostpos, mostneg */
	case S_MOSTPOS:
	case S_MOSTNEG:
		return (orig_type ? OpOf (tptr) : follow_user_type (OpOf (tptr)));
		/*}}} */
		/*{{{  conversions */
	case S_EXACT:
	case S_ROUND:
	case S_TRUNC:
		return (typenodeptr (MOpTypeOf (tptr)));
		/*}}} */
		/*{{{  offsetof */
#ifdef OCCAM2_5
	case S_OFFSETOF:
		return intnodeptr;
#endif
		/*}}} */
		/*{{{  constant expression */
	case S_CONSTEXP:
		return (chk_gettype_main (CExpOf (tptr), orig_type));
		/*}}} */
		/*{{{  literals */
	case S_TRUE:
	case S_FALSE:
		return bytenodeptr;
	case S_UBYTELIT:
	case S_UINTLIT:
	case S_UREALLIT:
		return (orig_type ? LitTypeOf (tptr) : follow_user_type (LitTypeOf (tptr)));
	case S_STRING:
		{
			const BIT32 dim = WLengthOf (CTValOf (tptr));
			treenode *const aptr = newtypenode (S_ARRAY, NOPOSN, newconstant (dim), bytenodeptr);
			SetARDim (aptr, dim);
			return (aptr);
		}
	case S_ASMNAME:
		return (intnodeptr);
		/*}}} */
		/*{{{  name temporary */
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_DECL:
	case N_PARAM:
	case N_VALPARAM:
	case N_RESULTPARAM:
	case N_REPL:
	case N_FIELD:
	case T_TEMP:
	case T_PREEVALTEMP:
	case S_FNFORMALRESULT:
		return (orig_type ? NTypeOf (tptr) : follow_user_type (NTypeOf (tptr)));
	case N_TAGDEF:
		return (bytenodeptr);
		/*}}} */
		/*{{{  function instance */
	case S_FINSTANCE:
		{
			treenode *const nptr = INameOf (tptr);
			treenode *p;
			if (TagOf (nptr) == N_DECL)	/* undeclared - bug 557 23/7/90 */
				return undeclaredp;
			p = FnTypeListOf (NTypeOf (nptr));	/* point p to the result types */
			if (listitems (p) == 1)
				p = ThisItem (p);
			return (orig_type ? p : follow_user_type (p));
		}
		/*}}} */
		/*{{{  S_NULLARRAY */
	case S_NULLARRAY:
		{
			const int old = switch_to_real_workspace ();
			treenode *artype;
			
			artype =newtypenode (S_ARRAY, NOPOSN, newconstant(0), undeclaredp);
			switch_to_prev_workspace (old);
			return artype;
		}
		/*}}}*/
		/*{{{  subscript */
	case S_RECORDSUB:
	case S_RECORDITEM:
		return (orig_type ? NTypeOf (ASIndexOf (tptr)) : follow_user_type (NTypeOf (ASIndexOf (tptr))));
	case S_ARRAYSUB:
	case S_ARRAYITEM:
		{
			int subscripts = 0;
			while (TRUE) {
				switch (TagOf (tptr)) {
					/*{{{  case S_ARRAYSUB */
				case S_ARRAYSUB:
				case S_ARRAYITEM:
					subscripts++;
					tptr = ASBaseOf (tptr);
					break;
					/*}}} */
					/*{{{  case S_SEGMENT S_SEGMENTITEM */
				case S_SEGMENT:
				case S_SEGMENTITEM:
					if (subscripts == 0)
						return (chk_gettype_main (tptr, orig_type));
					tptr = SNameOf (tptr);
					break;
					/*}}} */
					/*{{{  case S_CONSTRUCTOR */
				case S_CONSTRUCTOR:
					if (subscripts == 0)
						return (chk_gettype_main (tptr, orig_type));
					else if (subscripts == 1)
						return (chk_gettype_main (ThisItem (LitExpOf (tptr)), orig_type));
					else {
						tptr = ThisItem (LitExpOf (tptr));
						subscripts--;
					}
					break;
					/*}}} */
					/*{{{  case S_ARRAYCONSTRUCTOR */
				case S_ARRAYCONSTRUCTOR:
					if (subscripts == 0)
						return (chk_gettype_main (tptr, orig_type));
					else if (subscripts == 1)
						return (chk_gettype_main (ReplCBodyOf (tptr), orig_type));
					else {
						tptr = ReplCBodyOf (tptr);
						subscripts--;
					}
					break;
					/*}}} */
					/*{{{  case S_CONSTCONSTRUCTOR */
				case S_CONSTCONSTRUCTOR:
					tptr = CTExpOf (tptr);
					break;
					/*}}} */
					/*{{{  case S_STRING */
				case S_STRING:
					return arraytype (chk_gettype_main (tptr, orig_type), subscripts, "string");
					/*}}} */
					/*{{{  name temporary */
				case N_DECL:
				case N_ABBR:
				case N_VALABBR:
				case N_RETYPE:
				case N_VALRETYPE:
				case N_PARAM:
				case N_VALPARAM:
				case N_RESULTPARAM:
				case N_REPL:
				case T_TEMP:
				case T_PREEVALTEMP:
				case N_FIELD:
				case S_FNFORMALRESULT:
					return (orig_type ?
						arraytype (NTypeOf (tptr), subscripts,
							   WNameOf (NNameOf (tptr))) :
						follow_user_type (arraytype (NTypeOf (tptr), subscripts, WNameOf (NNameOf (tptr)))));

					/*}}} */
					/*{{{  case S_RECORDSUB */
				case S_RECORDSUB:
				case S_RECORDITEM:
					return (orig_type ?
						arraytype (chk_gettype_main (tptr, orig_type),
							   subscripts,
							   WNameOf (NNameOf (ASIndexOf (tptr)))) :
						follow_user_type (arraytype (chk_gettype_main (tptr,
											       orig_type),
									     subscripts, WNameOf (NNameOf (ASIndexOf (tptr))))));
					/*}}} */
					/*{{{  case S_FINSTANCE */
#ifdef OCCAM2_5
				case S_FINSTANCE:
					return (orig_type ?
						arraytype (chk_gettype_main (tptr, orig_type),
							   subscripts, "FUNCTION call") :
						follow_user_type (arraytype (chk_gettype_main (tptr, orig_type), subscripts, "FUNCTION call")));
#endif
					/*}}} */
				default:
					badtag (chklocn, TagOf (tptr), "chk_gettype_main");
				}
			}
		}
		/*}}} */
		/*{{{  segment */
	case S_SEGMENT:
	case S_SEGMENTITEM:
		{
			treenode *const slength = newmopnode (S_ELSIZE, NOPOSN, tptr, 0);
			treenode *const aptr = chk_gettype_main (SNameOf (tptr), orig_type);
			treenode *bptr;

#ifdef MOBILES
			if (TagOf (aptr) == S_MOBILE) {
				bptr = newtypenode (S_ARRAY, NOPOSN, slength, ARTypeOf (MTypeOf (aptr)));
			} else {
				bptr = newtypenode (S_ARRAY, NOPOSN, slength, ARTypeOf (aptr));
			}
#else
			bptr = newtypenode (S_ARRAY, NOPOSN, slength, ARTypeOf (aptr));
#endif
#if 0
fprintf (stderr, "chk_gettype_main (%s): aptr =", itagstring (TagOf (tptr)));
printtreenl (stderr, 4, aptr);
fprintf (stderr, "chk_gettype_main (SEGMENT{,ITEM}): SLengthExpOf (tptr) = ");
printtreenl (stderr, 4, SLengthExpOf (tptr));
#endif

#if 1
			/* special case (for now..) */
			if (SLengthExpOf (tptr) && (TagOf (SLengthExpOf (tptr)) == S_UINTLIT)) {
				/* fold it first */
				BIT32 shi, slo;

				foldconstexp (SLengthExpOf (tptr), &shi, &slo, CHK_EXP_NOT_CONST, LocnOf (tptr));
				SetARDim (bptr, (int)slo);
			}
#endif
			SetARDim (bptr, isconst (SLengthExpOf (tptr)) ? LoValOf (SLengthExpOf (tptr)) : (-1));
			return (bptr);
		}
		/*}}} */
		/*{{{  constructor */
	case S_CONSTRUCTOR:
#ifdef OCCAM2_5
		if (LitTypeOf (tptr) != NULL)
			return ((orig_type) ? LitTypeOf (tptr) : follow_user_type (LitTypeOf (tptr)));
#endif
		{
			treenode *constructor_type, *aptr;
			BIT32 dim;
			tptr = LitExpOf (tptr);
			dim = listitems (tptr);
			constructor_type = chk_gettype_main (ThisItem (tptr), TRUE);
			aptr = newtypenode (S_ARRAY, NOPOSN, newconstant (dim), constructor_type);
			SetARDim (aptr, dim);
			return (aptr);
		}
		/*}}} */
		/*{{{  arrayconstructor */
	case S_ARRAYCONSTRUCTOR:
		{
			const INT32 dim = isconst (ReplCLengthExpOf (tptr)) ? LoValOf (ReplCLengthExpOf (tptr)) : (-1);
			treenode *const ctype = chk_gettype_main (ReplCBodyOf (tptr), TRUE);
			treenode *const aptr = newtypenode (S_ARRAY, NOPOSN, newconstant (dim), ctype);

			SetARDim (aptr, dim);
			return (aptr);
		}
		/*}}} */
		/*{{{  constant constructor */
	case S_CONSTCONSTRUCTOR:
		return (chk_gettype_main (CTExpOf (tptr), orig_type));
		/*}}} */
		/*{{{  conditional expression */
#ifdef CONDEXP
	case S_CONDEXP:
		return (typenodeptr (CondExpTypeOf (tptr)));
#endif
		/*}}} */
		/*{{{  specification */
	case S_VALABBR:
	case S_VALRETYPE:
	case S_TPROTDEF:
	case S_SPROTDEF:
	case S_DECL:
	case S_PROCDEF:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
#ifdef MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
	case S_MPROCDECL:
#endif
	case S_ABBR:
	case S_RETYPE:
	case S_VALOF:
	case S_PRAGMA:		/* bug 829 20/9/91 */
		return (chk_gettype_main (ThisItem (VLResultListOf (skipspecifications (tptr))), orig_type));

		/*}}} */
		/*{{{  backend parameters */
	case S_HIDDEN_PARAM:
	case S_PARAM_STATICLINK:
	case S_PARAM_VSP:
	case S_PARAM_FB:
	case S_PARAM_WS:
#ifdef MOBILES
	case S_PARAM_MSP:
	case S_HIDDEN_TYPE:
	case S_CLONE:
	case S_ADDROF:
	case S_HWADDROF:
#endif
	case S_TYPEHASHOF:
	case S_FNACTUALRESULT:
		return intnodeptr;
#ifdef MOBILES
	case S_PARAM_MPP:
		/* type-tree associated with this is good */
#if 0
fprintf (stderr, "chk_gettype_main: PARAM_MPP: returning NTypeOf (tptr) = ");
printtreenl (stderr, 4, NTypeOf (tptr));
#endif
		return NTypeOf (tptr);
#endif
		/*}}} */
		/*}}} */
	default:
		return undeclaredp;
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *chk_gettype (tptr)*/
PUBLIC treenode *chk_gettype (treenode *tptr)
{
	treenode *t;

	/* Generate any temporary nodes in the temporary free space */
	const int old = switch_to_temp_workspace ();

#if 0
fprintf (stderr, "chk_gettype: getting type of: ");
printtreenl (stderr, 4, tptr);
#endif
	t = chk_gettype_main (tptr, FALSE);

#if 0
fprintf (stderr, "chk_gettype: got type: ");
printtreenl (stderr, 4, t);
#endif
	/* Switch back to real workspace */
	switch_to_prev_workspace (old);

	return (t);
}

/*}}}*/
/*{{{  PUBLIC int chk_typeof (tptr)*/
/* Cut down version of chk_gettype which only returns scalar types or S_ARRAY */
/* Assumes that all type checking has been done - uses the type field     */
/* of operator nodes                                                      */
PUBLIC int chk_typeof (treenode * tptr)
{
	while (TRUE)
		switch (TagOf (tptr)) {
			/*{{{  cases */
			/*{{{  monadic operator */
		case S_MOSTPOS:
		case S_MOSTNEG:
		case S_NEG:
		case S_BITNOT:
		case S_UMINUS:
		case S_NOT:
		case S_EXACT:
		case S_ROUND:
		case S_TRUNC:
#ifdef MOBILES
		case S_CLONE:
		case S_DEFINED:
#endif
#ifdef USER_DEFINED_OPERATORS
	#include "casemops.h"
#endif
			return (MOpTypeOf (tptr));
#ifdef MOBILES
		case S_NEW_ARRAY:
		case S_ALLOC_PROC:
		case S_NEW_BARRIER:
			/* this generates a MOBILE */
			return S_MOBILE;
		case S_UNDEFINED:
			tptr = OpOf (tptr);
			break;
		case S_ADDROF:
		case S_HWADDROF:
		case S_NTH_DIMENSION:
			return S_INT;
#endif
		case S_TYPEHASHOF:
		case S_ADDRESSOF:
		case S_SIZE:
		case S_ELSIZE:
		case S_SEGSTART:
#ifdef OCCAM2_5
		case S_BYTESIN:
#endif
			return S_INT;
			/*}}} */
			/*{{{  dyadic operator */
		case S_ADD:
		case S_SUBTRACT:
		case S_MULT:
		case S_DIV:
		case S_REM:
		case S_BITAND:
		case S_BITOR:
		case S_XOR:
		case S_LSHIFT:
		case S_RSHIFT:
		case S_AND:
		case S_OR:
		case S_PLUS:
		case S_MINUS:
		case S_TIMES:
		case S_OVERLAPCHECK:
		case S_EVAL:
#ifdef USER_DEFINED_OPERATORS
#include "casedops.h"
#endif
			return (DOpTypeOf (tptr));
		case S_EQ:
		case S_NE:
		case S_LS:
		case S_LE:
		case S_GR:
		case S_GE:
		case S_AFTER:
			return S_BOOL;
		case S_CSUB0:
		case S_CCNT1:
#ifdef OCCAM2_5
		case S_OFFSETOF:
#endif
			return S_INT;
			/*}}} */
			/*{{{  literal */
		case S_TRUE:
		case S_FALSE:
			return (S_BOOL);
		case S_ASMNAME:
			return (S_INT);
		case S_UBYTELIT:
		case S_UINTLIT:
		case S_UREALLIT:
			if (!LitTypeOf (tptr)) {
				/* this can happen if an error was generated elsewhere -- typecheck will put the type in for us */
				treenode *type = typecheck (tptr, unknownnodeptr);

				return type ? TagOf (type) : S_UNDECLARED;
			}
			return TagOf (follow_user_type (LitTypeOf (tptr)));

		case S_STRING:
		case S_ARRAYCONSTRUCTOR:
		case S_NULLARRAY:		/* added 13/02/2003 (frmb) */
			return S_ARRAY;
		case S_CONSTCONSTRUCTOR:
			tptr = CTExpOf (tptr);
			break;
		case S_CONSTRUCTOR:
#ifdef OCCAM2_5
			if (LitTypeOf (tptr) != NULL) {
				return TagOf (follow_user_type (LitTypeOf (tptr)));
			}
#endif
			return S_ARRAY;
			/*}}} */
			/*{{{  name temporary function result */
		case N_VALABBR:
		case N_ABBR:
		case N_VALRETYPE:
		case N_RETYPE:
		case N_DECL:
		case N_PROCTYPEDECL:
		case N_PARAM:
		case N_VALPARAM:
		case N_RESULTPARAM:
		case N_FIELD:
		case N_REPL:
		case T_TEMP:
		case T_PREEVALTEMP:
		case N_LABELDEF:	/* CON 22/12/92 */
		case S_FNFORMALRESULT:
			return (TagOf (follow_user_type (NTypeOf (tptr))));
		case N_TAGDEF:
			return (S_BYTE);
			/*}}} */
			/*{{{  function instance */
		case S_FINSTANCE:
			/* return tag of result type (assume it's not multi-valued) */
			{
				treenode *const ftypelist = FnTypeListOf (NTypeOf (INameOf (tptr)));
				return TagOf (follow_user_type (ThisItem (ftypelist)));
			}
			/*}}} */
			/*{{{  subscript */
		case S_RECORDSUB:
		case S_RECORDITEM:
			return TagOf (follow_user_type (NTypeOf (ASIndexOf (tptr))));
		case S_ARRAYSUB:
		case S_ARRAYITEM:
			{
				int subscripts = 0;
				/*{{{  work down to the name */
				while (TRUE) {
					switch (TagOf (tptr)) {
					default:
						badtag (LocnOf (tptr), TagOf (tptr), "chk_typeof (2)");
						break;
						/*{{{  name temporary */
					case N_DECL:
					case N_ABBR:
					case N_VALABBR:
					case N_RETYPE:
					case N_VALRETYPE:
					case N_PARAM:
					case N_VALPARAM:
					case N_RESULTPARAM:
					case N_REPL:
					case T_TEMP:
					case T_PREEVALTEMP:
					case N_FIELD:
					case S_FNFORMALRESULT:
						return TagOf (follow_user_type (arraytype (NTypeOf (tptr), subscripts, WNameOf (NNameOf (tptr)))));

						/*}}} */
						/*{{{  S_ARRAYSUB */
					case S_ARRAYSUB:
					case S_ARRAYITEM:
						subscripts++;
						tptr = ASBaseOf (tptr);
						break;
						/*}}} */
						/*{{{  record sub */
					case S_RECORDSUB:
					case S_RECORDITEM:
						/*tptr = ASBaseOf(tptr); */
						tptr = ASIndexOf (tptr);
						break;
						/*}}} */
						/*{{{  S_SEGMENT S_SEGMENTITEM */
					case S_SEGMENT:
					case S_SEGMENTITEM:
						if (subscripts == 0)
							return (S_ARRAY);
						else
							tptr = SNameOf (tptr);
						break;
						/*}}} */
						/*{{{  S_STRING */
					case S_STRING:
						if (subscripts == 0)
							return S_ARRAY;
						else
							return S_BYTE;
						/*}}} */
						/*{{{  S_CONSTRUCTOR */
					case S_CONSTRUCTOR:
						if (subscripts == 0)
							return chk_typeof (tptr);
						else if (subscripts == 1)
							return (chk_typeof (ThisItem (LitExpOf (tptr))));
						else {
							subscripts--;
							tptr = ThisItem (LitExpOf (tptr));
						}
						break;
						/*}}} */
						/*{{{  S_ARRAYCONSTRUCTOR */
					case S_ARRAYCONSTRUCTOR:
						if (subscripts == 0)
							return (S_ARRAY);
						else if (subscripts == 1)
							return (chk_typeof (ReplCBodyOf (tptr)));
						else {
							subscripts--;
							tptr = ReplCBodyOf (tptr);
						}
						break;
						/*}}} */
						/*{{{  S_CONSTCONSTRUCTOR */
					case S_CONSTCONSTRUCTOR:
						tptr = CTExpOf (tptr);
						break;
						/*}}} */
						/*{{{  S_FNINSTANCE */
#ifdef OCCAM2_5
					case S_FINSTANCE:
						return TagOf (follow_user_type (arraytype (chk_gettype (tptr), subscripts, "FUNCTION call")));
#endif
						/*}}} */
					}
				}
				/*}}} */
			}
			/*}}} */
			/*{{{  segment */
		case S_SEGMENT:
		case S_SEGMENTITEM:
			return (S_ARRAY);
			/*}}} */
			/*{{{  valof, specification - must be preceding a VALOF */
		case S_VALABBR:
		case S_VALRETYPE:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_DECL:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_ABBR:
		case S_RETYPE:
		case S_VALOF:
		case S_PRAGMA:	/* bug 829 20/9/91 */
			tptr = ThisItem (VLResultListOf (skipspecifications (tptr)));
			break;
			/*}}} */
			/*{{{  constant expression */
		case S_CONSTEXP:
			tptr = CExpOf (tptr);
			break;
			/*}}} */
			/*{{{  backend parameters, bits and pieces */
		case S_HIDDEN_PARAM:
		case S_PARAM_STATICLINK:
		case S_PARAM_VSP:
#ifdef MOBILES
		case S_PARAM_MSP:
		case S_PARAM_MPP:
		case S_HIDDEN_TYPE:
#endif
		case S_PARAM_FB:
		case S_PARAM_WS:
		case S_DUMMYEXP:
			return S_INT;
		case S_FNACTUALRESULT:
			return S_UNKNOWN;
			/*}}} */
			/*}}} */
		default:
			badtag (LocnOf (tptr), TagOf (tptr), "chk_typeof (1)");
		}
}

/*}}}*/

#if defined(PD_DECODE_CHANNEL) && defined(PD_DECODE_CHANNEL3) && defined(PD_ENCODE_CHANNEL)
/*{{{  PRIVATE int check_encode_decode_linkprot (treenode *atype)*/
PRIVATE int check_encode_decode_linkprot (treenode *atype)
{
	/* allowed protocols:
	 *	<SPROTDEF> (INT; INT)
	 *	<SPROTDEF> (INT; INT; INT)
	 *	[2]INT
	 *	[3]INT
	 *	MOBILE [2]INT
	 *	MOBILE [3]INT
	 */
	if ((TagOf (atype) == S_CHAN) && (TagOf (ProtocolOf (atype)) == N_SPROTDEF)) {
		treenode *prot = NTypeOf (ProtocolOf (atype));

		if ((TagOf (prot) == S_LIST) && (TagOf (ThisItem (prot)) == S_INT) &&
				(TagOf (NextItem (prot)) == S_LIST) && (TagOf (ThisItem (NextItem (prot))) == S_INT)) {
			treenode *subsubprot = NextItem (NextItem (prot));

			if (!subsubprot) {
				return 0;
			}
			if ((TagOf (subsubprot) == S_LIST) && (TagOf (ThisItem (subsubprot)) == S_INT) && (NextItem (subsubprot) == NULL)) {
				return 0;
			}
		}
	}
	return -1;
}
/*}}}*/
#endif /* PD_DECODE_CHANNEL && PD_DECODE_CHANNEL3 && PD_ENCODE_CHANNEL */
/*{{{  PRIVATE INT check_decode_data_type (treenode *aparam, treenode *atype)*/
PRIVATE INT check_decode_data_type (treenode *aparam, treenode *atype)
{
#if 0
fprintf (stderr, "check_decode_data_type: aparam =");
printtreenl (stderr, 4, aparam);
fprintf (stderr, "check_decode_data_type: atype =");
printtreenl (stderr, 4, atype);
#endif
	atype = follow_user_type (atype);

	switch (TagOf (atype)) {
	case S_CHAN:
	case S_TIMER:
		return -1;
	case S_MOBILE:
		atype = MTypeOf (atype);
		if (TagOf (atype) == S_RECORD) {
			/* disallow mobile channel-types */
			atype = ARTypeOf (atype);
			if ((TagOf (atype) == S_DECL) && (TagOf (DNameOf (atype)) == N_FIELD)) {
				if (TagOf (NTypeOf (DNameOf (atype))) == S_CHAN) {
					return -1;
				}
			} else if ((TagOf (atype) == S_DECL) && (TagOf (DNameOf (atype)) == S_LIST) && (TagOf (ThisItem (DNameOf (atype))) == N_FIELD)) {
				if (TagOf (NTypeOf (ThisItem (DNameOf (atype)))) == S_CHAN) {
					return -1;
				}
			}
		}
		break;
	}
	return 0;
}
/*}}}*/
/*{{{  PUBLIC void checkparams (tptr, mode)*/
/*****************************************************************************
 *
 *  checkparams checks that the actual parameter types of procedure or
 *              function instance 'tptr' match the formal parameter types
 *              of the function/procedure.  'mode' is set to S_PROC if we
 *              are checking a procedure, 'S_FUNCTION' if we are checking
 *              a function.
 *
 *****************************************************************************/

/* S_PROC or S_FUNCTION */
PUBLIC void checkparams (treenode *tptr, const int mode)
{
	treenode *pname = INameOf (tptr);
	const BOOL forked = (mode == S_PROC) && IForkedOf (tptr);

	/*{{{  ignore undeclared functions and procedures */
	if ((TagOf (pname) == N_DECL) && (TagOf (NTypeOf (pname)) == S_UNDECLARED)) {
		/*{{{  check the parameter types */
		treenode *aparamptr;

		for (aparamptr = IParamListOf (tptr); !EndOfList (aparamptr); aparamptr = NextItem (aparamptr)) {
			treenode **nodeptr = ThisItemAddr (aparamptr);

			/* remove ASINPUT/ASOUTPUT things, ignoring types */
			switch (TagOf (*nodeptr)) {
			case S_ASINPUT:
			case S_ASOUTPUT:
				*nodeptr = OpOf (*nodeptr);
				break;
			}
			typecheck (ThisItem (aparamptr), unknownnodeptr);
		}
		return;
		/*}}} */
	}
	/*}}} */
	if (mode == S_PROC) {
		/*{{{  checking a procedure instance and the namenode isn't a procedure */
		switch (TagOf (pname)) {
		default:
#ifdef MOBILES
			if (ismobileprocvar (pname)) {
				break;		/* valid */
			}
#endif
			chkreport_s (CHK_NAME_NOT_PROC, chklocn, WNameOf (NNameOf ((pname))));
			break;
		case N_PROCDEF:
		case N_SCPROCDEF:
		case N_LIBPROCDEF:
		case N_LIBMPROCDECL:
		case N_STDLIBPROCDEF:
		case N_PREDEFPROC:
		case N_INLINEPROCDEF:
			break;
		}
		/*}}} */
	} else {
		/*{{{  checking a function instance and the namenode isn't a function */
		switch (TagOf (pname)) {
		default:
			chkreport_s (CHK_NAME_NOT_FUNCTION, chklocn, WNameOf (NNameOf (pname)));
			break;
		case N_SFUNCDEF:
		case N_LFUNCDEF:
		case N_SCFUNCDEF:
		case N_LIBFUNCDEF:
		case N_STDLIBFUNCDEF:
		case N_PREDEFFUNCTION:
		case N_INLINEFUNCDEF:
			break;
		}
		/*}}} */
	}
#if 0
fprintf (stderr, "checkparams: checking PROC/FUNCTION instance parameters: pname =");
printtreenl (stderr, 4, pname);
#endif
	/*{{{  check the parameter types */
	{
		treenode *fparamptr = NParamListOf (pname);
		treenode *aparamptr = IParamListOf (tptr);
		const int nfparams = listitems (fparamptr);
		const int naparams = listitems (aparamptr);

		if (naparams > nfparams) {
			chkreport_s (CHK_TOO_MANY_ACTUALS, chklocn, WNameOf (NNameOf (pname)));
		} else if (naparams < nfparams) {
			chkreport_s (CHK_TOO_FEW_ACTUALS, chklocn, WNameOf (NNameOf (pname)));
		} else {
			int paramno = 1;
			#if defined(PD_DECODE_CHANNEL) && defined(PD_DECODE_CHANNEL3) && defined(PD_ENCODE_CHANNEL)
			const BOOL is_xio = (TagOf (pname) == N_PREDEFPROC) && ((NModeOf (pname) == PD_DECODE_CHANNEL) || (NModeOf (pname) == PD_DECODE_CHANNEL3) || (NModeOf (pname) == PD_ENCODE_CHANNEL));
			#else
			const BOOL is_xio = FALSE;
			#endif

			/*{{{  quickly walk along the lists and check that ASINPUT/ASOUTPUT parameters are good */
			while (!EndOfList (fparamptr) && !EndOfList (aparamptr)) {
				treenode *fparam = ThisItem (fparamptr);
				treenode **paparam = ThisItemAddr (aparamptr);
				BIT32 specattr = 0;
				treenode *atype = NULL, *ftype, *baseexpr;
				int fdim, adim;
#ifdef MOBILES
				BOOL anonchantypeflag = FALSE;
#endif

#if 0
fprintf (stderr, "checkparams: checking for ASINPUT/ASOUTPUT on");
printtreenl (stderr, 4, *paparam);
#endif
				/*{{{  collect specifier from actual parameter*/
				/* need to step inside segments */
				if (TagOf (*paparam) == S_SEGMENT) {
					paparam = SNameAddr (*paparam);
				}
				switch (TagOf (*paparam)) {
				case S_ASINPUT:
					specattr = TypeAttr_marked_in;
					*paparam = OpOf (*paparam);
					break;
				case S_ASOUTPUT:
					specattr = TypeAttr_marked_out;
					*paparam = OpOf (*paparam);
					break;
				case S_CONSTRUCTOR:
					{
						treenode *cptr = LitExpOf (*paparam);

						/* don't remove these, handled in typecheck main above.  if there's a problem, at least one check will fail */
						while (!EndOfList (cptr)) {
							if (TagOf (ThisItem (cptr)) == S_ASINPUT) {
								specattr = TypeAttr_marked_in;
								break;
							} else if (TagOf (ThisItem (cptr)) == S_ASOUTPUT) {
								specattr = TypeAttr_marked_out;
								break;
							}
							cptr = NextItem (cptr);
						}
					}
					break;
				}
				/*}}}*/
				/*{{{  setup ftype, baseexpr and atype*/
				ftype = NTypeOf (fparam);
				fdim = 0;
				while (TagOf (ftype) == S_ARRAY) {
					ftype = ARTypeOf (ftype);
					fdim++;
				}
				adim = 0;
				for (baseexpr = *paparam; TagOf (baseexpr) == S_ARRAYSUB; baseexpr = ASBaseOf (baseexpr), adim--);
#ifdef MOBILES
				/* anonymous channel-types don't go through standard chan-type direction checking */
				if ((TagOf (baseexpr) == S_RECORDSUB) && (TagOf (ASIndexOf (baseexpr)) == N_FIELD) && (TagOf (NTypeOf (ASIndexOf (baseexpr))) == S_CHAN)) {
					atype = NTypeOf (ASIndexOf (baseexpr));
					baseexpr = ASBaseOf (baseexpr);
					anonchantypeflag = TRUE;
				} else		/* XXX: the switch() is intended to be for "else" only */
#endif
				{
#if 0
fprintf (stderr, "checkparams: HERE! baseexpr is:");
printtreenl (stderr, 4, baseexpr);
#endif
				switch (TagOf (baseexpr)) {
				case N_DECL:
				case N_ABBR:
				case N_PARAM:
				case N_RESULTPARAM:
					atype = NTypeOf (baseexpr);
#ifdef MOBILES
					/* frmb -- need to hop over MOBILE for MOBILE arrays, already MOBILE/non-MOBILE compatability */
					if (TagOf (atype) == S_MOBILE) {
						atype = MTypeOf (atype);
					}
#endif	/* MOBILES */
					while (TagOf (atype) == S_ARRAY) {
						atype = ARTypeOf (atype);
						adim++;
					}
					break;
				case S_CONSTRUCTOR:
					/* if there was anything interesting, it's in specattr, we just setup adim here */
					if (specattr) {
						treenode *xp = baseexpr;

						while (TagOf (xp) == S_CONSTRUCTOR) {
							xp = LitExpOf (xp);
							adim++;
						}
					}
					atype = baseexpr;	/* so its an S_CONSTRUCTOR */
					break;
				default:
					atype = undeclaredp;
					break;
				}
				}
				/*}}}*/
#if 0
fprintf (stderr, "checkparams: fdim = %d, ftype =", fdim);
printtreenl (stderr, 4, ftype);
fprintf (stderr, "checkparams: adim = %d, atype =", adim);
printtreenl (stderr, 4, atype);
#endif
				/*{{{  check channel specifier compatability*/
				if ((TagOf (ftype) == S_CHAN) && (TagOf (atype) == S_CHAN) && (fdim == adim)) {
					const BIT32 fattr = (TypeAttrOf (ftype) & (TypeAttr_marked_in | TypeAttr_marked_out));
					BIT32 aattr = (TypeAttrOf (atype) & (TypeAttr_marked_in | TypeAttr_marked_out));

#ifdef MOBILES
					if (anonchantypeflag == TRUE) {
						/* happens when we find anonymous channel-types here */
						treenode *mobiletype = NTypeOf (baseexpr);
#if 0
fprintf (stderr, "checkparams: HERE2! mobiletype (attrof = %d) is:", NTypeAttrOf (mobiletype));
printtreenl (stderr, 4, mobiletype);
#endif
						if (NTypeAttrOf (mobiletype) & TypeAttr_marked_out) {
							/* client side of a channel-type, so invert aattr */
							aattr ^= (TypeAttr_marked_in | TypeAttr_marked_out);
						}
						specattr = aattr;
					}
#endif
#if 0
fprintf (stderr, "checkparams: CHAN formal/actual; specattr = %d, fattr = %d, aattr = %d\n", (int)specattr, (int)fattr, (int)aattr);
#endif
					/* check compatability */
					if (specattr && ((specattr | fattr | aattr) != specattr)) {
						chkreport_s (CHK_BAD_CHAN_CONFLICT, LocnOf (tptr), WNameOf (NNameOf (baseexpr)));
					} else if (!specattr && strict_checking) {
						chkreport_s (CHK_NO_CHANDIR_SPEC, LocnOf (tptr), WNameOf (NNameOf (baseexpr)));
					}
				} else if ((TagOf (ftype) == S_CHAN) && (TagOf (atype) == S_CONSTRUCTOR) && (fdim == adim)) {
					const BIT32 fattr = (TypeAttrOf (ftype) & (TypeAttr_marked_in | TypeAttr_marked_out));
					BIT32 aattr = specattr;

#if 0
fprintf (stderr, "checkparams: check constructor: specattr = %x, fattr = %x, aattr = %x\n", specattr, fattr, aattr);
#endif
					/* check compatability */
					if (specattr && ((specattr | fattr | aattr) != specattr)) {
#if 0
fprintf (stderr, "checkparams: fail at 1\n");
#endif
						chkreport (CHK_BAD_CHAN_CONFLICT_ANON, LocnOf (baseexpr));
					}
				}
				/*}}}*/
#ifdef MOBILES
				/*{{{  check for SHARED compatability*/
				/* checked in typesequiv now */
#if 0
				/* check either both shared, or neither shared compatability (doesn't care about underlying type) */
				/* result parameters are illegal here, but we'll be forviging for later additions */
				/* don't process for PD_ALLOC_CHAN_TYPE though! */
				if (((TagOf (fparam) == N_PARAM) || (TagOf (fparam) == N_RESULTPARAM)) && (nodetypeoftag (TagOf (baseexpr)) == NAMENODE) &&
						((TagOf (pname) != N_PREDEFPROC) && (NModeOf (pname) != PD_ALLOC_CHAN_TYPE)) &&
						(TagOf (NTypeOf (baseexpr)) == N_TYPEDECL) && (TagOf (NTypeOf (fparam)) == N_TYPEDECL)) {
					const int fattr = NTypeAttrOf (NTypeOf (fparam));
					const int aattr = NTypeAttrOf (NTypeOf (baseexpr));

					if ((fattr & TypeAttr_shared) && !(aattr & TypeAttr_shared)) {
						chkreport_s (CHK_APARAM_NOTSHARED, LocnOf (tptr), WNameOf (NNameOf (baseexpr)));
					} else if ((aattr & TypeAttr_shared) && !(fattr & TypeAttr_shared)) {
						chkreport_s (CHK_FPARAM_NOTSHARED, LocnOf (tptr), WNameOf (NNameOf (baseexpr)));
					}
				}
#endif
				/*}}}*/
#endif
				fparamptr = NextItem (fparamptr);
				aparamptr = NextItem (aparamptr);
			}
			fparamptr = NParamListOf (pname);
			aparamptr = IParamListOf (tptr);
			/*}}}*/

#if 0
fprintf (stderr, "checkparams: about to check parameters proper.  pname =");
printtreenl (stderr, 4, pname);
#endif
			while (!EndOfList (fparamptr) && !EndOfList (aparamptr)) {
				/*{{{  check left-hand side, move to right hand side */
				BOOL ok = FALSE;
				treenode *const f = ThisItem (fparamptr);
				treenode *a = ThisItem (aparamptr);			/* auto-CLONE of dynamic mobile channel-types changes this */
				int atag = TagOf (a);					/* and this */
				/*{{{  special for bug TS/1797 18/08/92 */
				const BOOL proc_param = (TagOf (pname) == N_PREDEFFUNCTION)
					&& (NModeOf (pname) == PD_WSSIZEOF || NModeOf (pname) == PD_VSSIZEOF)
									/*&& (paramno == 1) */ ;
									/* always the case! */
				/*}}} */
				/* similar, for PROTOCOL.HASH, but it takes protocols/actuals instead */
				#if defined(PD_PROTOCOL_HASH) && defined(PD_LOAD_TYPE_DESC)
				const BOOL proto_param = (TagOf (pname) == N_PREDEFFUNCTION) && (NModeOf (pname) == PD_PROTOCOL_HASH);
				const BOOL anyname_param = (TagOf (pname) == N_PREDEFFUNCTION) && (NModeOf (pname) == PD_LOAD_TYPE_DESC);
				#else
				const BOOL proto_param = FALSE, anyname_param = FALSE;
				#endif

				treenode *atype = NULL;

				if (proc_param || proto_param || anyname_param) {
					atype = undeclaredp;		/* bug TS/1792 18/08/92 */
				} else {
					atype = typecheck_param (a, NTypeOf (f), NNameOf (pname), paramno);
				}
#if 0
fprintf (stderr, "checkparams: typecheck'd actual, (proc_param = %d, proto_param = %d, is_xio = %d, forked=%d, IForkOf(tptr)=%p), atype (%p) is: ", proc_param, proto_param, is_xio, forked, IForkOf (tptr), atype);
printtreenl (stderr, 4, atype);
fprintf (stderr, "   checkparams: formal type (%p) is: ", NTypeOf (f));
printtreenl (stderr, 4, NTypeOf (f));
fprintf (stderr, "   checkparams: btw, that actual param thing (a) is: ");
printtreenl (stderr, 4, a);
#endif

				if (anyname_param) {
					/*{{{  accept any name here*/
					ok = (nodetypeoftag (atag) == NAMENODE);

					#ifdef PD_LOAD_TYPE_DESC
					if (NModeOf (pname) == PD_LOAD_TYPE_DESC) {
						treenode *mtype = a;
						/* must be a deep mobile type to get code-generated */

						switch (TagOf (mtype)) {
						case N_PARAM:
						case N_DECL:
						case N_ABBR:
						case N_RESULTPARAM:
							mtype = NTypeOf (mtype);
							break;
						}

						if (TagOf (mtype) == N_TYPEDECL) {
							mtype = NTypeOf (mtype);
						}

						if (!isdeepmobiletype (mtype)) {
							ok = FALSE;
						}
					}
					#endif
					/*}}}*/
				} else if (proc_param) {
					/*{{{  special case for PROC name parameters! */
					/* currently only possible for WSSIZEOF and VSSIZEOF predefines */
					ok = (nodetypeoftag (atag) == NAMENODE)
						&& (nametypeoftag (atag) == NAMENODE_ROUTINE)
						&& !isinline (a)
						&& (atag != N_PREDEFPROC)
						&& (atag != N_PREDEFFUNCTION);
					/*}}} */
				} else if (proto_param) {
					/*{{{  special case for PROTOCOL -or- name parameters! */
					treenode *tohash = NULL;

					/* only for the PROTOCOL.HASH pre-define, actual parameter is 'a' */
#if 0
fprintf (stderr, "checkparams: PROTOCOL.HASH, parameter is: ");
printtreenl (stderr, 4, a);
#endif
					/* frmb: changed: now allow PROTOCOL.HASH on anything! -- except things that have it separately now (e.g. MOBILE.CHANs and MOBILE.PROCs) */
					switch (TagOf (a)) {
					case N_DECL:
					case N_PARAM:
					case N_RESULTPARAM:
					case N_ABBR:
						if ((TagOf (NTypeOf (a)) == S_ANYCHANTYPE) || (TagOf (NTypeOf (a)) == S_ANYPROCTYPE)) {
							tohash = NULL;
							ok = TRUE;
							break;		/* switch() */
						}
						/* fall through */
					default:
						tohash = a;
						ok = TRUE;
						break;
					}

					if (ok && tohash) {
						const int old = switch_to_real_workspace ();
						int hashcode = (int)typehash (tohash);
						treenode *cn;
						
						cn = newconstant (hashcode);

#if 0
fprintf (stderr, "chk1: hashcode = 0x%8.8x. cn =", hashcode);
printtreenl (stderr, 4, cn);
#endif
						SetRight (aparamptr, newlistnode (S_LIST, NOPOSN, cn, NULL));
						switch_to_prev_workspace (old);

						/* then advance aparamptr so we exit the loop next time */
						aparamptr = NextItem (aparamptr);
					}
					/*}}}*/
				} else if (!is_xio && typesequivalent (atype, NTypeOf (f), TRUE)) {
#if 0
fprintf (stderr, "checkparams: typesequivalent was good..\n");
#endif
					/*{{{  check a var param is a variable */
					ok = TRUE;
					if ((TagOf (f) == N_PARAM) || (TagOf (f) == N_RESULTPARAM)) {
#ifdef MOBILES
						/*{{{  check for CLONE'd parameter*/
						if (TagOf (a) == S_CLONE) {
							checkelement (OpOf (a), NTypeOf (f), paramno);
						} else if ((TagOf (f) != N_PARAM) || (TagOf (a) != S_NULLARRAY)) {
							checkelement (a, NTypeOf (f), paramno);
						}
						/*}}}*/
#else
						checkelement (a, NTypeOf (f), paramno);
#endif
					}
					/*}}} */
#ifdef MOBILES
					/*{{{  auto-clone-2 for shared channel types (other in scopeandcheck)*/
					/* but _don't_ do this for RESULT parameters..! */
					if ((TagOf (a) != S_CLONE) && isdynmobilechantype (a) && (TagOf (f) != N_RESULTPARAM)) {
						/* dynamic channel-type parameter -- shared compability already checked,
						 * if shared, insert CLONE operator */

						if (TagOf (atype) == S_ANYCHANTYPE) {
							/* do nothing for these */
						} else if (NTypeAttrOf (atype) & TypeAttr_shared) {
							treenode *tmp;
#if 0
fprintf (stderr, "*** checkparams: looks like I should auto-clone this one..\n");
#endif
							tmp = newmopnode (S_CLONE, LocnOf (tptr), a, S_MOBILE);
							SetLeft (aparamptr, tmp);
							a = tmp;
							atag = S_CLONE;
						}
					}
					/*}}}*/
#endif
					/*{{{  if free fork, make parameter is value or MOBILE */
					if (forked && !IForkOf (tptr)) {
						treenode *aname = a;
						int valid_param = 0;

						if (TagOf (aname) == S_CLONE) {
							/* skip over CLONE */
							aname = OpOf (aname);
						}
						if ((TagOf (f) == N_PARAM) || (TagOf (f) == N_RESULTPARAM)) {
							switch (TagOf (aname)) {
							case N_DECL:
							case N_PARAM:
							case N_RESULTPARAM:
							case N_ABBR:
							case S_ARRAYSUB:
							case S_RECORDSUB:
								if (isdynmobilechantype (aname)) {
									valid_param = 1;
								} else if (isdynmobilearray (aname)) {
									valid_param = 1;
								} else if (ismobile (aname)) {
									valid_param = 1;
								} else if (isanychantype (aname)) {
									valid_param = 1;
								} else if (isanyproctype (aname)) {
									valid_param = 1;
								}
								break;
							default:
#if 0
fprintf (stderr, "forked param for PARAM/RESULTPARAM formal is ");
printtreenl (stderr, 4, aname);
#endif
								break;
							}
						} else {
							valid_param = 1;
						}
						if (!valid_param) {
							msg_out_is (SEV_ERR_JMP, CHK, CHK_VAR_IN_FREE_FORK, chklocn, paramno, WNameOf (NNameOf (pname)));
						}
					}
					/*}}}*/
					/*{{{  if forked, check non-val params are scoped sensibly */
					if (forked && IForkOf (tptr)) {
						treenode *forknode = IForkOf (tptr);
						const int fork_lexlevel = NLexLevelOf (DNameOf (CBodyOf (forknode)));
						const int fork_scope = NScopeOf (DNameOf (CBodyOf (forknode)));

						if ((TagOf (f) == N_PARAM) || (TagOf (f) == N_RESULTPARAM)) {
							treenode *aname = a;
							int adim = 0;
							int valid_param = 0;
#ifdef MOBILES
							int cloned_param = 0;

							if (TagOf (aname) == S_CLONE) {
								/* skip over CLONE */
								aname = OpOf (aname);
								cloned_param = 1;
							}
#endif
							while ((TagOf (aname) == S_ARRAYSUB) || (TagOf (aname) == S_RECORDSUB) || (TagOf (aname) == S_SEGMENT)) {
								if (TagOf (aname) != S_SEGMENT) {
									aname = ASBaseOf (aname);
								} else {
									aname = SNameOf (aname);
								}
								adim++;
							}
							switch (TagOf (aname)) {
							case N_DECL:
							case N_PARAM:
							case N_RESULTPARAM:
							case N_ABBR:
#ifdef MOBILES
								/* if it's a whole variable (adim = 0), check for MOBILE variables */
								if (!adim && isdynmobilechantype (aname)) {
									valid_param = 1;
								} else if (!adim && isdynmobilearray (aname)) {
									valid_param = 1;
								} else if (!adim && isdynmobilebarrier (aname)) {
									valid_param = 1;
								} else if (!adim && ismobile (aname)) {
									/* unhandled currently.. */
#if 0
fprintf (stderr, "failed at 0. aname is:");
printtreenl (stderr, 4, aname);
#endif
									chkerr_i (CHK_FORK_PARAM_NOT_IMPLEMENTED, chklocn, paramno);
									valid_param = 1;
								}
#endif
								/* ensure anything non-mobile (valid_param=0) is explicitly SHARED */
								if (!valid_param && !NVSharedOf (aname)) {
									chkerr_i (CHK_FORK_PARAM_NOT_SHARED, chklocn, paramno);
								}
								if (!valid_param && ((NLexLevelOf (aname) > fork_lexlevel) || (NScopeOf (aname) > fork_scope))) {
									chkerr_s (CHK_NAME_INSIDE_FORKING, chklocn, WNameOf (NNameOf (aname)));
								}
								break;
							default:
								/* dunno, error. */
#if 0
fprintf (stderr, "failed at 1. aname is:");
printtreenl (stderr, 4, aname);
#endif
								chkerr_i (CHK_FORK_PARAM_NOT_IMPLEMENTED, chklocn, paramno);
								break;
							}
#if 0
fprintf (stderr, "checkparams: forked instance: (fork_lexlevel = %d, fork_scope = %d).  a(ctual-param) = ", fork_lexlevel, fork_scope);
printtreenl (stderr, 4, a);
fprintf (stderr, "checkparams: (NLexLeveOf(a) = %d, NScopeOf(a) = %d).\n", NLexLevelOf (a), NScopeOf (a));
#endif
						} else if (TagOf (f) == N_VALPARAM) {
							if (bytesin (NTypeOf (f)) > bytesperword) {
#if 0
fprintf (stderr, "failed at 2\n");
								chkerr_i (CHK_FORK_PARAM_NOT_IMPLEMENTED, chklocn, paramno);
#endif
							}
						}
					}
					/*}}}*/
				} else if (TagOf (pname) == N_PREDEFPROC /*|| TagOf(pname) == N_PREDEFFUNCTION */ ) {
					/*{{{  else we are checking some special predefined routines */
					/* For the predefined procedures LOAD.INPUT.CHANNEL, LOAD.OUTPUT.CHANNEL,
					   LOAD.INPUT.CHANNEL.VECTOR and LOAD.OUPUT.CHANNEL.VECTOR, we allow
					   any protocol on the channel parameter */
					
					switch (NModeOf (pname)) {
					#if 0
					case PD_LOADINPUTCHANNEL:
					case PD_LOADINPUTCHANNELVECTOR:
					case PD_LOADOUTPUTCHANNEL:
					case PD_LOADOUTPUTCHANNELVECTOR:
						if (basetype (NTypeOf (f)) == S_CHAN) {
							/*{{{  fudge the type, and see if it matches */
							const int old = switch_to_temp_workspace ();
							treenode *fudgedtype = copytree (atype, syn_lexlevel);
							treenode *t = fudgedtype;
							while (TagOf (t) == S_ARRAY) {
								t = ARTypeOf (t);
							}
							if (TagOf (t) == S_CHAN) {
								SetProtocol (t, newleafnode (S_ANY, LocnOf (t)));
							}
							ok = typesequivalent (NTypeOf (f), fudgedtype, TRUE);
							switch_to_prev_workspace (old);
							/*}}} */
						}
						break;
					#endif
					case PD_ALLOC_CHAN_TYPE:
						/*{{{  special handling, main check in chk4 */
						/* all this PROCs formals are channel-type vars */
						/* bit botchy doing this.. chk4 has already made sure that the variables
						 * are TYPEDECL types and have different specifiers */
#ifdef MOBILES
#if 0
fprintf (stderr, "foobar: here !?\n");
#endif
						if (TagOf (NTypeOf (atype)) != S_MOBILE) {
							chkerr_s (CHK_INV_CHANTYPE_ALLOC_VARTYPE, chklocn, WNameOf (NNameOf (pname)));
						} else {
							ok = TRUE;
						}
#if 0
fprintf (stderr, "chk1: checkparams: PD_ALLOC_CHAN_TYPE actual-param NTypeOf (type) is: ");
printtreenl (stderr, 4, NTypeOf (atype));
#endif
#endif	/* MOBILES */
						break;
						/*}}}*/
					#if defined(PD_DECODE_CHANNEL) && defined(PD_DECODE_CHANNEL3)
					case PD_DECODE_CHANNEL:
					case PD_DECODE_CHANNEL3:
						/*{{{  DECODE.CHANNEL[3] (CHAN * ?, CHAN INT ?, CHAN <int2/int3> ! [, CHAN BOOL ?]) */
#if 0
fprintf (stderr, "chk1: checkparams: PD_DECODE_CHANNEL: a =");
printtreenl (stderr, 4, a);
fprintf (stderr, "chk1:      \"     :       \"          : atype =");
printtreenl (stderr, 4, atype);
#endif
						switch (paramno) {
						case 1:
							/* check for any channel parameter */
							if (TagOf (atype) == S_CHAN) {
								ok = TRUE;
							}
							break;
						case 2:
							/* check for INT or BOOL channel parameter */
							if ((TagOf (atype) == S_CHAN) && ((TagOf (ProtocolOf (atype)) == S_INT) || (TagOf (ProtocolOf (atype)) == S_BOOL))) {
								ok = TRUE;
							}
							break;
						case 3:
							/* check for some channel carrying two/three INTs */
							if (!check_encode_decode_linkprot (atype)) {
								ok = TRUE;
							}
							break;
						case 4:
							/* check for BOOL channel parameter */
							if ((TagOf (atype) == S_CHAN) && (TagOf (ProtocolOf (atype)) == S_BOOL)) {
								ok = TRUE;
							}
							break;
						}
						break;
						/*}}}*/
					#endif /* PD_DECODE_CHANNEL && PD_DECODE_CHANNEL3 */
					#ifdef PD_ENCODE_CHANNEL
					case PD_ENCODE_CHANNEL:
						/*{{{  ENCODE.CHANNEL (CHAN <int2/int3> ?, CHAN INT ?, CHAN * !) */
#if 0
fprintf (stderr, "chk1: checkparams: PD_ENCODE_CHANNEL: a =");
printtreenl (stderr, 4, a);
fprintf (stderr, "chk1:      \"     :       \"          : atype =");
printtreenl (stderr, 4, atype);
#endif
						switch (paramno) {
						case 1:
							/* check for some channel carrying two/tree INTs */
							if (!check_encode_decode_linkprot (atype)) {
								ok = TRUE;
							}
							break;
						case 2:
							/* check for INT or BOOL channel parameter */
							if ((TagOf (atype) == S_CHAN) && ((TagOf (ProtocolOf (atype)) == S_INT) || (TagOf (ProtocolOf (atype)) == S_BOOL))) {
								ok = TRUE;
							}
							break;
						case 3:
							/* check for any channel parameter */
							if (TagOf (atype) == S_CHAN) {
								ok = TRUE;
							}
							break;
						}
						break;
						/*}}}*/
					#endif /* PD_ENCODE_CHANNEL */
					case PD_DECODE_DATA:
						/*{{{  DECODE.DATA (* data, RESULT INT addr, RESULT INT data) */
						switch (paramno) {
						case 1:
							/* check for valid "l-value" */
							if (!check_decode_data_type (a, atype)) {
								ok = TRUE;
							}
							break;
						case 2:
						case 3:
							/* must be valid INTs */
							if (TagOf (atype) == S_INT) {
								ok = TRUE;
							}
							break;
						}
						break;
						/*}}}*/
					default:
						break;
					}
					/*}}} */
				}

				if (!ok) {
					msg_out_is (SEV_ERR_JMP, CHK, CHK_INVTYPE_PARAM, chklocn, paramno, WNameOf (NNameOf (pname)));
				}
				/*{{{  move to right-hand side */
				fparamptr = NextItem (fparamptr);
				aparamptr = NextItem (aparamptr);
				paramno++;
				/*}}} */
				/*}}} */
			}
		}
	}
	/*}}} */
}

/*}}}*/

#ifdef MOBILES
/*{{{  PRIVATE void cinnermobiledecl (treenode *tptr, const BOOL mobile_top_level)*/
/* this performs checking inside type-trees.  mobile_top_level indicates exactly that. */
PRIVATE void cinnermobiledecl (treenode *tptr, const BOOL mobile_top_level)
{
	BOOL had_fixed_dim = FALSE;
	const SOURCEPOSN locn = LocnOf (tptr);

#if 0
fprintf (stderr, "chk1: cinnermobiledecl: tptr =");
printtreenl (stderr, 4, tptr);
#endif
	while (TRUE) {
		switch (TagOf (tptr)) {
		case S_MOBILE:
			if (!mobile_top_level) {
				chkreport (CHK_PROMOTE_MOBILE, chklocn);
			}
			tptr = MTypeOf (tptr);
			break;
		case S_RECORD:
			{
				treenode *decl = ARTypeOf (tptr);

				while (decl) {
					if (TagOf (decl) != S_DECL) {
						chkreport (CHK_ILLEGAL_FIELD_DECL, LocnOf (decl));
					} else if ((TagOf (DNameOf (decl)) != N_DECL) && (TagOf (DNameOf (decl)) != N_FIELD)) {
						chkreport (CHK_ILLEGAL_FIELD_DECL, LocnOf (decl));
					} else {
						treenode *type = NTypeOf (DNameOf (decl));

						cinnermobiledecl (type, mobile_top_level);
						decl = DBodyOf (decl);
					}
				}
			}
			return;
		case S_ARRAY:
			if (ARDimLengthOf (tptr)) {
				had_fixed_dim = TRUE;
			}
			tptr = ARTypeOf (tptr);
			break;
		case N_TYPEDECL:
			tptr = NTypeOf (tptr);
			break;
		case S_ASINPUT:
		case S_ASOUTPUT:
			if (had_fixed_dim) {
				/* fixed dimensions mean non-dynamic, but can only have dynamic channel-end types */
				chkreport (CHK_MOBILE_MIXED_DYNAMIC, locn);
			}
			/* these represent as-yet unsorted channel-direction
			 * specifiers (probably for mobile channel-types).
			 * Just step through these for now
			 */
			tptr = OpOf (tptr);
			break;
		default:
			return;
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE BOOL cmobiledecl (treenode *tptr)*/
/*
 *	this is used to check mobile type-trees for
 *	badly nested mobiles.  Doesn't fit too cleanly in ctypeorspec..
 *	returns TRUE for mobile types/declarations
 */
PRIVATE BOOL cmobiledecl (treenode *tptr)
{
	BOOL mobile_top_level = FALSE;

	/* step from namenode into type-tree */
	switch (TagOf (tptr)) {
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_ABBR:
		tptr = NTypeOf (tptr);
		break;
	}

#if 0
fprintf (stderr, "chk1: cmobiledecl(): tptr (type) = ");
printtreenl (stderr, 4, tptr);
#endif
	/* might possible have an un-processed ASINPUT/ASINPUT node in the way -- step over this (fixed later on) */
	if ((TagOf (tptr) == S_ASINPUT) || (TagOf (tptr) == S_ASOUTPUT)) {
		tptr = OpOf (tptr);
	}
	/* check for top-level MOBILE types */
	switch (TagOf (tptr)) {
	case S_MOBILE:
		mobile_top_level = TRUE;
		tptr = MTypeOf (tptr);
		break;
	case N_TYPEDECL:
		tptr = NTypeOf (tptr);
		if (TagOf (tptr) == S_MOBILE) {
			mobile_top_level = TRUE;
			tptr = MTypeOf (tptr);
		}
		break;
	}

	cinnermobiledecl (tptr, mobile_top_level);
	return mobile_top_level;
}
/*}}}*/
#endif
/*{{{  PRIVATE treenode *ctypeorspec (treenode * const tptr, BOOL typetree, const BOOL var_dim_permitted)*/
/*{{{  comment*/
/*
 * Check a type or specifier tree :
 * For a type tree, check array dimensions are exist and are
 * integer constant, fold them, and store value in array node.
 * 
 * For a specifier tree, if array dimensions exist do the above.
 * Check channel protocol names correspond to protocol definitions.
 * 
 * For channel simple protocols, check array dimensions as above
 * (apart from the first one after '::', which must be null).
 * Returns the checked type tree.
 * 
 * For a specifier tree, throw out PORTs.
 */
/*}}}*/
PRIVATE treenode *ctypeorspec (treenode * const tptr, BOOL typetree, const BOOL var_dim_permitted)
{
	treenode *typeptr = tptr;
#ifdef MOBILES
	BOOL mobile_top_level = FALSE;
#endif

	while (TRUE) {
		SOURCEPOSN local = LocnOf (typeptr);
#if 0
fprintf (stderr, "ctypeorspec (loop): mobile_top_level = %d, typetree = %d, typeptr =", (int)mobile_top_level, typetree);
printtreenl (stderr, 4, typeptr);
#endif

		switch (TagOf (typeptr)) {
			/*{{{  BOOL, BYTE, INT, INTn, REALn, TIMER, NODE UNDECLARED */
		case S_BOOL:
		case S_BYTE:
		case S_INT:
		case S_INT16:
		case S_INT32:
		case S_INT64:
		case S_UINT:
		case S_UINT16:
		case S_UINT32:
		case S_UINT64:
		case S_REAL32:
		case S_REAL64:
		case S_TIMER:
		case S_UNDECLARED:
		case S_BARRIER:
		case S_FULLBARRIER:
		CASE_CONFIG_TYPE
			return (tptr);
			/*}}} */
#ifdef MOBILES
			/*{{{  ANYCHANTYPE, ANYPROCTYPE, ANYMOBILETYPE */
		case S_ANYCHANTYPE:
		case S_ANYPROCTYPE:
		case S_ANYMOBILETYPE:
			return tptr;
			/*}}}*/
			/*{{{  MOBILE */
		case S_MOBILE:
			if (typeptr == tptr) {
				mobile_top_level = TRUE;
				/* quick peek inside, ban sillyness like MOBILE FLIP? */
				if ((TagOf (MTypeOf (typeptr)) == S_ASINPUT) || (TagOf (MTypeOf (typeptr)) == S_ASOUTPUT)) {
					chkreport (CHK_INNER_MOBILE, chklocn);
				}
			} else if (!mobile_top_level) {
				chkreport (CHK_PROMOTE_MOBILE, chklocn);
			} else {
				/* if top-level is a dynamic mobile array (ARDimLength == -1), allow a dynamic mobile array here too */
				if ((TagOf (tptr) == S_MOBILE) && (TagOf (MTypeOf (tptr)) == S_ARRAY) && (ARDimLengthOf (MTypeOf (tptr)) == NULL) &&
						(TagOf (MTypeOf (typeptr)) == S_ARRAY) && (ARDimLengthOf (MTypeOf (typeptr)) == NULL)) {
					/* allow this, MOBILE []MOBILE []BYTE type stuff */

				} else {
					/* fail */
#if 0
fprintf (stderr, "ctypeorspec: failing nested MOBILE here..\n");
#endif
					chkreport (CHK_INNER_MOBILE, chklocn);
				}
			}
			typeptr = MTypeOf (typeptr);
			break;
			/*}}}  */
			/*{{{  channel direction specifiers (happens when we encounter a CHAN TYPE variable declaration) */
		case S_ASINPUT:
			/* this is the server side */
			typeptr = OpOf (typeptr);
			if (TagOf (typeptr) != N_TYPEDECL) {
				chkreport (CHK_INV_CHANTYPE_VAR, local);
			}
			/* NTypeOf (typeptr) is a MOBILE */
			/* SetNTypeAttr (typeptr, NTypeAttrOf (typeptr) | TypeAttr_marked_in); */
			SetNMode (typeptr, NM_DEFAULT);
			return ctypeorspec (typeptr, typetree, var_dim_permitted);
			break;
		case S_ASOUTPUT:
			/* this is the client side */
			typeptr = OpOf (typeptr);
			if (TagOf (typeptr) != N_TYPEDECL) {
				chkreport (CHK_INV_CHANTYPE_VAR, local);
			}
			/* NTypeOf (typeptr) is a MOBILE */
			/* SetNTypeAttr (typeptr, NTypeAttrOf (typeptr) | TypeAttr_marked_out); */
			SetNMode (typeptr, NM_DEFAULT);
			return ctypeorspec (typeptr, typetree, var_dim_permitted);
			break;
			/*}}}*/
			/*{{{  COLON2  (counted array, happens when examining inside a MOBILE CHAN type)*/
		case S_COLON2:
			/* should already have checked it, so don't need to do anything here */
			return tptr;
			/*}}}*/
#endif
			/*{{{  ARRAY */
		case S_ARRAY:
			{
				treenode *atype;

				if (ARDimLengthOf (typeptr) != NULL) {
					/*{{{  check array dimension */
					if (TagOf (ARDimLengthOf (typeptr)) != S_CONSTEXP) {
						/*{{{  check the array dimension type and value */
						atype = typecheck (ARDimLengthOf (typeptr), intnodeptr);
						if (TagOf (atype) == S_INT) {
#ifdef OCCAM2_5
							if (var_dim_permitted && !isconst (ARDimLengthOf (typeptr))) {
								/*{{{  variable dimension */
								SetARDimLength (typeptr, foldexp (ARDimLengthOf (typeptr)));
								SetARDim (typeptr, -1);	/* Set dimension to match any */
								/*}}} */
							} else
#endif
							{
								/*{{{  fold the constant dimension */
								BIT32 reslo, reshi;
								treenode *dimptr = ARDimLengthOf (typeptr);
								foldconstexp (dimptr, &reshi, &reslo, CHK_ADIM_NOT_CONST, LocnOf (typeptr));
								/*{{{  check 0 < array dimension */
								{
									BOOL toosmall;

									I32ToI64 (&reshi, &reslo, reslo);
									Int64Gt (&toosmall, 0, 1, reshi, reslo);
									if (toosmall) {
										chkreport (CHK_ADIM_NEGATIVE, chklocn);
									}
									if (checkbounds (S_INT64,
											 current_fe_data->fe_txlib->bpw == 2 ? S_INT16 : S_INT32,
											 reshi, reslo, chklocn) != 0) {
										chkreport (CHK_ADIM_TOOBIG, chklocn);
									}
								}
								/*}}} */
								SetARDimLength (typeptr, newconstexpnode (S_CONSTEXP, chklocn, dimptr, reshi, reslo));
								SetARDim (typeptr, reslo);
								/*}}} */
							}
#ifndef OCCAM2_5
							USE_VAR (var_dim_permitted);
#endif
						} else if (TagOf (atype) == S_UNDECLARED) {
							SetARDim (typeptr, MOSTPOS_INT32);
						} else {
							chkreport (CHK_ADIM_NOT_INT, chklocn);
						}
					}
					/*}}} */
					/* else
					   This tree is shared with another parameter which has already been
					   checked, so we don't have to do any work here */
					/*}}} */
				} else {
					if (typetree) {
#ifdef MOBILES
						if (!mobile_top_level)
#endif
						/*{{{  null dimensions are illegal on type trees, unless it's a dynamic mobile */
							chkreport (CHK_ADIM_MISSING, LocnOf (typeptr));
						/*}}} */
					} else {
						SetARDim (typeptr, -1);	/* Set dimension to match any */
					}
				}
				typeptr = ARTypeOf (typeptr);	/* Move down to array type */

				break;
			}
			/*}}} */
			/*{{{  BUFFERED*/
		case S_BUFFERED:
			{
				treenode *atype;

				if (!ARDimLengthOf (typeptr)) {
					chkreport (CHK_BUFCHAN_MISSINGSIZE, chklocn);
				} else {
					atype = typecheck (ARDimLengthOf (typeptr), intnodeptr);

					if (TagOf (atype) == S_INT) {
						if (!isconst (ARDimLengthOf (typeptr))) {
							chkreport (CHK_BUFCHAN_NOTCONST, chklocn);
						} else {
							/*{{{  fold the constant size*/
							BIT32 reslo, reshi;
							treenode *dimptr = ARDimLengthOf (typeptr);

							foldconstexp (dimptr, &reshi, &reslo, CHK_BUFCHAN_NOTCONST, LocnOf (typeptr));

							/*{{{  check for a sensible size*/
							{
								BOOL toosmall;

								I32ToI64 (&reshi, &reslo, reslo);
								Int64Gt (&toosmall, 0, 1, reshi, reslo);

								if (toosmall) {
									chkreport (CHK_BUFCHAN_NEGATIVE, chklocn);
								}
							}
							/*}}} */
							SetARDimLength (typeptr, newconstexpnode (S_CONSTEXP, chklocn, dimptr, reshi, reslo));
							if (reslo > 16) {
								chkreport (CHK_BUFCHAN_TOOBIG, chklocn);
							}
							SetARDim (typeptr, reslo);

							/*}}} */
						}
					} else if (TagOf (atype) == S_UNDECLARED) {
						SetARDim (typeptr, MOSTPOS_INT32);
					} else {
						chkreport (CHK_BUFCHAN_NOTCONST, chklocn);		/* will suffice */
					}
				}
				typeptr = ARTypeOf (typeptr);	/* move down to CHAN */

				break;
			}
			/*}}}*/
			/*{{{  CHAN */
		case S_CHAN:
#ifdef MOBILES
		case S_ANONCHANTYPE:
#endif
			{
				treenode *pptr = ProtocolOf (typeptr);

				switch (TagOf (pptr)) {
					/*{{{  S_COLON2 */
				case S_COLON2:
					/* syn ensures that the left-hand type is valid, and that the right-hand
					   type is a NULL array. */
					{
						treenode *aptr = RightOpOf (pptr);
						SetARDim (aptr, -1);	/* Set dimension length to unknown */
						typetree = TRUE;	/* The simple protocol tree is a type tree */
						typeptr = ARTypeOf (aptr);
						break;
					}
					/*}}} */
					/*{{{  S_ARRAY */
				case S_ARRAY:
					typeptr = pptr;
					typetree = TRUE;
					break;
					/*}}} */
#ifdef MOBILES
					/*{{{  S_MOBILE*/
				case S_MOBILE:
					typeptr = MTypeOf (pptr);
					typetree = FALSE;	/* The simple protocol might be a specifier.. */
					mobile_top_level = TRUE;
					break;
					/*}}}*/
					/*{{{  N_TYPEDECL */
				case N_TYPEDECL:
					/* if the protocol is a mobile chan-type, direction must be present */
					if (TagOf (NTypeOf (pptr)) == S_MOBILE) {
						BOOL ctflag = FALSE;
						treenode *mtype = NTypeOf (pptr);

						/* chan-type if MOBILE RECORD with a CHAN sub-field */
						if (TagOf (MTypeOf (mtype)) == S_RECORD) {
							treenode *list = ARTypeOf (MTypeOf (mtype));
#if 0
fprintf (stderr, "ctypeorspec: CHAN/ANONCHANTYPE: TYPEDECL protocol, about to check fields: ");
printtreenl (stderr, 4, list);
#endif

							while (list && (TagOf (list) == S_DECL)) {
#if 0
fprintf (stderr, "ctypeorspec: CHAN/ANONCHANTYPE: TYPEDECL protocol, checking MOBILE RECORD field =");
printtreenl (stderr, 4, ThisItem (list));
#endif
								if ((TagOf (DNameOf (list)) == N_DECL) && (TagOf (NTypeOf (DNameOf (list))) == S_CHAN)) {
									ctflag = TRUE;
								}
								list = DBodyOf (list);
							}
						}
						if (ctflag) {
#if 0
fprintf (stderr, "ctypeorspec: CHAN/ANONCHANTYPE: TYPEDECL protocol, CHAN TYPE, pptr =");
printtreenl (stderr, 4, pptr);
#endif
							if (!(NTypeAttrOf (pptr) & (TypeAttr_marked_in | TypeAttr_marked_out))) {
								chkreport (CHK_NO_CHANTYPE_DIR, LocnOf (typeptr));
							}
						}
					}
					return tptr;
					/*}}}*/
#endif
					/*{{{  S_ANY name stype */
				case S_ANY:
				case N_SPROTDEF:
				case N_TPROTDEF:
				case S_BOOL:
				case S_BYTE:
				case S_INT:
				case S_INT16:
				case S_INT32:
				case S_INT64:
				case S_UINT:
				case S_UINT16:
				case S_UINT32:
				case S_UINT64:
				case S_REAL32:
				case S_REAL64:
				case S_UNDECLARED:
#ifndef MOBILES
				case N_TYPEDECL:
#else
				case S_ANYCHANTYPE:
				case S_ANYPROCTYPE:
				case S_ANYMOBILETYPE:
#endif
					return (tptr);
#ifdef MOBILES
				case N_PROCTYPEDECL:
					/* not allowed plain, must be MOBILE */
					chkreport (CHK_MPP_NOT_MOBILE, LocnOf (tptr));
					return undeclaredp;
#endif
				case N_DECL:	/* An undeclared protocol name */
					return undeclaredp;
					/*}}} */
				default:
#if 0
fprintf (stderr, "invalid channel protocol = ");
printtreenl (stderr, 4, pptr);
#endif
					chkreport (CHK_INV_PROT, chklocn);
				}
				break;
			}
			/*}}} */
			/*{{{  PORT */
		case S_PORT:
			{
				treenode *pptr = ProtocolOf (typeptr);
				switch (TagOf (pptr)) {
					/*{{{  S_ARRAY */
				case S_ARRAY:
					typeptr = pptr;
					typetree = TRUE;	/* The simple protocol is a type, not a specifier */
					break;
					/*}}} */
					/*{{{  stype */
				case S_BOOL:
				case S_BYTE:
				case S_INT:
				case S_INT16:
				case S_INT32:
				case S_INT64:
				case S_UINT:
				case S_UINT16:
				case S_UINT32:
				case S_UINT64:
				case S_REAL32:
				case S_REAL64:
					return (tptr);
					/*}}} */
				default:
					chkreport (CHK_INV_PORT_PROT, chklocn);
				}
				break;
			}
			/*}}} */
			/*{{{  type name */
#ifdef OCCAM2_5
		case N_TYPEDECL:
		case N_DECL:	/* undeclared name where type name is expected  */
			return tptr;
#endif
			/*}}} */
#ifdef MOBILES
			/*{{{  N_PROCTYPEDECL*/
		case N_PROCTYPEDECL:
			if (!mobile_top_level) {
				chkreport (CHK_MPP_NOT_MOBILE, chklocn);
			}
			return tptr;
			/*}}}*/
#endif
			/*{{{  RECORD type */
#ifdef OCCAM2_5
		case S_RECORD:
			{
				treenode *decl;
				/*{{{  check and un-merge the list of fields */
				/* you may declare fields like so:
				   DATA TYPE r
				     RECORD
				       INT x, y, z :
				   :
				   This pass turns this into multiple different fields */
				{
					treenode *next_decl;
					treenode *new = NULL;
					treenode **new_ptr = &new;

					if (ARTypeOf (typeptr) == NULL) {
						chkreport (CHK_NO_RECF_ZERO, LocnOf (typeptr));
					}
					for (decl = ARTypeOf (typeptr); decl != NULL; decl = next_decl) {
						next_decl = DBodyOf (decl);

						if (TagOf (decl) != S_DECL) {
							chkerr (CHK_ILLEGAL_FIELD_DECL, LocnOf (decl));
						} else {
							/*{{{  move this item onto new list */
							*new_ptr = decl;
							new_ptr = DBodyAddr (decl);
							/*}}} */

							if (TagOf (DNameOf (decl)) != N_DECL) {
								/*{{{  move the others one at a time */
								treenode *list = DNameOf (decl);
								treenode *const field_type = NTypeOf (ThisItem (list));

								/*{{{  turn the current one into a single */
								SetDName (decl, ThisItem (list));
								/*}}} */
								list = NextItem (list);	/* move to second on list */
								while (!EndOfList (list)) {
									treenode *const this_name = ThisItem (list);
									treenode *const next_name = NextItem (list);
									treenode *field;

									SetNType (this_name, copytree (field_type, NLexLevelOf (this_name)));
									SetTag (this_name, N_FIELD);	/* was N_DECL */
									(void) ctypeorspec (NTypeOf (this_name), typetree, var_dim_permitted);
									field = newdeclnode (S_DECL, LocnOf (this_name), this_name, NULL, NULL);
									*new_ptr = field;
									new_ptr = DBodyAddr (field);
									freenode (&list);
									list = next_name;
								}
								/*}}} */
							}
							SetTag (DNameOf (decl), N_FIELD);	/* was N_DECL */
							(void) ctypeorspec (NTypeOf (DNameOf (decl)), typetree, var_dim_permitted);
						}
					}
					SetARType (typeptr, new);
				}
				/*}}} */
			}
			return typeptr;
#endif
			/*}}} */
			/*{{{  PROTOCOL types*/
		case N_TPROTDEF:
		case N_SPROTDEF:
			return typeptr;
			/*}}}*/
		default:
#if 0
fprintf (stderr, "ctypeorspec: default case: typeptr = ");
printtreenl (stderr, 4, typeptr);
#endif
#ifdef OCCAM2_5
			if (nodetypeoftag (TagOf (typeptr)) == NAMENODE) {
				chkreport_s (CHK_BAD_TYPENAME, chklocn, WNameOf (NNameOf (typeptr)));
			} else
#endif
			{
				badtag (chklocn, TagOf (typeptr), "ctypeorspec");
			}
		}
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *ctype (tptr)*/
PUBLIC treenode *ctype (treenode * const tptr)
{
	treenode *t = ctypeorspec (tptr, TRUE, FALSE);

#ifdef MOBILES
	cmobiledecl (tptr);
#endif
	return t;
}

/*}}}*/
/*{{{  PUBLIC treenode *cspecifier (tptr)*/
PUBLIC treenode *cspecifier (treenode * const tptr)
{
	treenode *t = ctypeorspec (tptr, FALSE, FALSE);

#ifdef MOBILES
	cmobiledecl (tptr);
#endif
	return t;
}

/*}}}*/
/*{{{  PUBLIC treenode *cretypes*/
#ifdef OCCAM2_5
PUBLIC treenode *cretypes (treenode *const tptr, const BOOL reshapes)
{
#ifdef MOBILES
	/* ban any use of RETYPES/RESHAPES on a MOBILE..! */
	treenode *t;

#if 0
fprintf (stderr, "cretypes: tptr =");
printtreenl (stderr, 4, tptr);
#endif
	t = ctypeorspec (tptr, FALSE, reshapes);
	if (cmobiledecl (t)) {
		chkreport (reshapes ? CHK_MOBILE_RESHAPE : CHK_MOBILE_RETYPE, chklocn);
	}
	return t;
#else
	return ctypeorspec (tptr, FALSE, reshapes);
#endif
}
#endif
/*}}}*/

/*{{{  PUBLIC void check_adjacent_name*/
PUBLIC void check_adjacent_name (const SOURCEPOSN locn, treenode * nptr, treenode * const last_decl, treenode * const current_params, const err_severity_t severity)
{
	treenode *namelist;
	BOOL err = TRUE;

	if (last_decl != NULL) {
		namelist = DNameOf (last_decl);
	} else {
		namelist = current_params;
	}

	nptr = nameof (nptr);

	if (namelist != NULL) {
		if (TagOf (namelist) == S_LIST) {
			for (; !EndOfList (namelist); namelist = NextItem (namelist)) {
				if (nptr == ThisItem (namelist)) {
					err = FALSE;
				}
			}
		} else {
			err = nptr != namelist;
		}
	}

	if (err) {
		msg_out_s (severity, CHK, CHK_NOT_ADJACENT_NAME, locn, WNameOf (NNameOf (nptr)));
	}
}

/*}}}*/

/*{{{  fullbarrier checking/update routines*/
/*{{{  PUBLIC BOOL check_isfullbarrier (treenode *tptr)*/
/*
 *	returns TRUE if the given parameter is a barrier or mobile barrier
 */
PUBLIC BOOL check_isfullbarrier (treenode *tptr)
{
	treenode *type;

	if (!tptr) {
		return FALSE;
	}
	if (nodetypeoftag (TagOf (tptr)) != NAMENODE) {
		return FALSE;
	}
	type = NTypeOf (tptr);

	if (TagOf (type) == S_FULLBARRIER) {
		return TRUE;
	}
	if ((TagOf (type) == S_MOBILE) && (TagOf (MTypeOf (type)) == S_FULLBARRIER)) {
		return TRUE;
	}

	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL check_resignedfullbarrier (treenode *tptr)*/
/*
 *	returns TRUE if the given barrier is resigned
 */
PUBLIC BOOL check_resignedfullbarrier (treenode *tptr)
{
	if (!tptr) {
		return FALSE;
	}
	if (nodetypeoftag (TagOf (tptr)) != NAMENODE) {
		return FALSE;
	}

	if (NTypeAttrOf (tptr) & TypeAttr_resigned) {
		return TRUE;
	}

	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC void check_resignbarrier (treenode *tptr)*/
/*
 *	sets the resign flag on this barrier
 */
PUBLIC void check_resignbarrier (treenode *tptr)
{
	if (!tptr || (nodetypeoftag (TagOf (tptr)) != NAMENODE)) {
		msg_out (SEV_INTERNAL, CHK, CHK_BARRIER_INTERNAL, chklocn);
	}
	SetNTypeAttr (tptr, NTypeAttrOf (tptr) | TypeAttr_resigned);
	return;
}
/*}}}*/
/*{{{  PUBLIC void check_unresignbarrier (treenode *tptr)*/
/*
 *	unsets the resign flag on this barrier
 */
PUBLIC void check_unresignbarrier (treenode *tptr)
{
	if (!tptr || (nodetypeoftag (TagOf (tptr)) != NAMENODE)) {
		msg_out (SEV_INTERNAL, CHK, CHK_BARRIER_INTERNAL, chklocn);
	}
	SetNTypeAttr (tptr, NTypeAttrOf (tptr) & ~TypeAttr_resigned);
	return;
}
/*}}}*/
/*}}}*/


