/* $Id: chk2.c,v 1.5 1998/09/03 11:59:02 djb1 Exp $ */

/*
 *	Occam two semantic analyser
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

/*{{{  COMMENT The semantic analyser*/
/*
   The interface to the semantic analyser is via a set of functions
   defined in this module which check various parts of the parse
   tree built up by the parser.
   The semantic analyser makes few modifications to the parse tree,
   but does insert types into operator nodes, and constant values
   into constant expression nodes (subscript lengths in array declarations,
   values of selections in case processes), it also fills in types of
   abbreviations and untyped literals where deducible.
*/
/*}}}*/
/*{{{  COMMENT What the semantic analyser checks*/
/**********************  Start comment out ****************************
|*
The semantic analyser performs the following checks:

1  Assignments are balanced, and the types of the variables match the types of
   the expressions.

2  Abbreviations have only one name and the type of the expression/element
   matches the type specified. If no type is specified, the type of the rhs is
   inserted.

3  Retypes have only one name and the type of the expression/element matches
   the type specified.

4  Function result expression lists match the types of the functions.

5  Channel protocol equivalence is checked in abbreviations and retypes.

6  Array dimensions exist, and are constant expressions.

7  CASE processes: the selector is typechecked, each selection is typechecked
   and constant folded - each must have the same type as the selector,
   all selections are distinct, there is only one ELSE selection.

8  Formal parameter list specifiers are validated - var parameters to functions
   are thrown out.

9  Replicator start and count are type checked to be INT, a replicated PAR
   count is constant-folded.

10 Guards on IF, WHILE, and ALT are type checked to be BOOL.

11 Procedure actual parameters are type-checked against procedure formal
   parameters.

12 VAL TIMERs CHANs and PORTs are thrown out in abbreviations, retypes and
   parameter definitions.

13 Actual var params are checked to be elements.

14 Ports only have simple protocols.

15 Protocols are checked on inputs, outputs, case inputs, tagged inputs.
   Timer inputs and delayed inputs are checked to be INT.

16 Protocol definitions are checked to contain distinct tags, which are also
   distinct from the protocol name.

17 Actual parameters to functions are type-checked against function formal
   parameters.

18 ALT, PAR, !, ? are disallowed in VALOFs.  Write access to non-local
   variables is disallowed within VALOFs, and any specifications
   preceding them.  Calls to non-local procedures is disallowed in
   VALOFs.

19 Placed objects are decls only, the placement address is constant and of
   type INT.

20 The tags in a case input are all distinct.
*|
 **********************   End comment out  ****************************/
/*}}}*/

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "feinc.h"
#include "extlib.h"		/* IMPORTED */
#include "suplib.h"		/* IMPORTED */

#include "chkerror.h"
#include "chkdef.h"
#include "chk1def.h"
#include "chk2def.h"
#include "chk4def.h"
#include "predefhd.h"

#include "lexdef.h"		/* for strict_checking */
#include "syndef.h"		/* for syn_lexlevel */
#include "trandef.h"		/* for isdynmobilechantype/dynmobiledimensioncount */
/*}}}*/

/*{{{  definitions*/
#define MAX_TAGS 256		/* Maximum number of tags allowed in a protocol defn */

/* These structures are used when checking that
   cases and variants are  distinct. */
typedef struct {
	/*treenode *ca_decl; */
	SOURCEPOSN ca_locn;
	BIT32 ca_hi, ca_lo;
} casenode_t;

typedef struct {
/*treenode *vnt_decl; *//* The variant's declaration */
	treenode *vnt_tag;	/* The tag's namenode */
} vntnode_t;

/* Used for packing record structures: */
typedef struct {
	treenode *fld_nptr;
	int fld_bytes;
	int fld_element;
} fldnode_t;
/*}}}*/

/*{{{  protocol checking*/
/*{{{  checking definitions*/
/*{{{  PRIVATE void cdefsimpleprotocol (pptr)*/
/*****************************************************************************
 *
 *  cdefsimpleprotocol checks the definition of a simple protocol, 'pptr'.
 *
 *****************************************************************************/
PRIVATE void cdefsimpleprotocol (treenode * pptr, void *voidptr)
{
	voidptr = voidptr;
	switch (TagOf (pptr)) {
	case S_ANY:		/* bug 1395 30/09/91 */
#ifdef MOBILES
	case S_ANYCHANTYPE:
	case S_ANYPROCTYPE:
	case S_ANYMOBILETYPE:
#endif
		break;
	case S_COLON2:
		/* syn ensures that the left-hand side is primitive - so doesn't need
		   checking, and that the right-hand side is an array of unknown size -
		   we need to check the type of that array only */
		{
			treenode *aptr = RightOpOf (pptr);
			/* Fill in the unknown array dimension */
			SetARDim (aptr, -1);
			/* Check the type of the array */
			ctype (ARTypeOf (aptr));
		}
		break;
	default:
		ctype (pptr);
	}
}

/*}}}*/
/*{{{  PRIVATE void inlineseqprotocol (treenode *list, treenode *pptr) */
/*****************************************************************************
 *
 *  inlineseqprotocol inlines one sequential protocol within another.  This
 *  is simply a tree-copy and does not affect their distinctness (in PROTOCOL
 *  x IS y:  for example).
 *
 *****************************************************************************/
PRIVATE void inlineseqprotocol (treenode *list, treenode *pptr)
{
	treenode *const restoflist = NextItem (list);
	const int old = switch_to_real_workspace ();

#if 0
fprintf (stderr, "inlineseqprotocol(): here\n");
#endif
	if (TagOf (pptr) == S_LIST) {
		SetLeft (list, copytree (ThisItem (pptr), syn_lexlevel));
		SetRight (list, copytree (NextItem (pptr), syn_lexlevel));
		/* walk to end of list */
		while (!IsLastItem (list)) {
			list = NextItem (list);
		}
		SetRight (list, restoflist);
	} else {
		SetLeft (list, copytree (pptr, syn_lexlevel));
	}
	switch_to_prev_workspace (old);
	return;
}
/*}}}  */
/*{{{  PRIVATE void cdefseqprotocol (pptr)*/
/*****************************************************************************
 *
 *  cdefseqprotocol checks the definition of a sequential protocol, 'pptr'
 *
 *****************************************************************************/
PRIVATE void cdefseqprotocol (treenode *pptr)
{
	treenode *list = pptr;


#if 0
fprintf (stderr, "cdefseqprotocol: pptr = ");
printtreenl (stderr, 4, pptr);
#endif
	if (!list || (TagOf (list) != S_LIST)) {
		return;
	}
	while (!EndOfList (list)) {
		treenode *tptr = ThisItem (list);

		switch (TagOf (tptr)) {
		case N_SPROTDEF:
			/* expand protocol definition inline */
#if 0
fprintf (stderr, "cdefseqprotocol: inlineseqprotocol, tptr = ");
printtreenl (stderr, 4, tptr);
#endif
			inlineseqprotocol (list, NTypeOf (tptr));
			break;
		default:
			cdefsimpleprotocol (ThisItem (list), NULL);
			list = NextItem (list);
			break;
		}
	}
	/* walklist (cdefsimpleprotocol, pptr, NULL); */
}

/*}}}*/
/*{{{  PRIVATEPARAM void checktag (tagptr)*/
/*****************************************************************************
 *
 *  checktag checks that a symbol table entry for a tag, 'tagptr' is
 *           distinct from all other tags in THIS protocol.
 *  frmb - also does inlining of CASE PROTOCOLs
 *
 *****************************************************************************/
PRIVATEPARAM void checktag (treenode *const tagptr, void *const voidptr)
{
	treenode **tagpp = (treenode **)voidptr;
	treenode *taglist = *tagpp;

#if 0
fprintf (stderr, "checktag: tagptr = ");
printtreenl (stderr, 4, tagptr);
#endif
	/* leading sanity check: must be NULL, LIST or N_TPROTDEF as type on tag */
	if (NTypeOf (tagptr) && (TagOf (NTypeOf (tagptr)) != S_LIST) && (TagOf (NTypeOf (tagptr)) != N_TPROTDEF)) {
		/* means we have cases 'FROM X' where X isn't sensible! */
		chkreport (CHK_BAD_CASE_INCLUSION, LocnOf (tagptr));
	}

	if (NTypeOf (tagptr) && (TagOf (NTypeOf (tagptr)) == N_TPROTDEF)) {
		/* this could get a bit messy.. */
		treenode *tlist;
		const int old = switch_to_real_workspace ();
		treenode *restoflist, *copy, *walk, *walk2;
		SOURCEPOSN taglocn = LocnOf (tagptr);

#if 0
fprintf (stderr, "checktag: NTypeOf -> NTypeOf = ");
printtreenl (stderr, 4, NTypeOf (NTypeOf (tagptr)));
#endif
		for (tlist = taglist; !EndOfList (tlist) && (ThisItem (tlist) != tagptr); tlist = NextItem (tlist));
		/* we are the item at "tlist" */
		restoflist = NextItem (tlist);
		copy = copytree (NTypeOf (NTypeOf (tagptr)), syn_lexlevel);
#if 0
fprintf (stderr, "checktag: before tree mangle.  restoflist is:");
printtreenl (stderr, 4, restoflist);
fprintf (stderr, "checktag: before tree mangle.  copy is:");
printtreenl (stderr, 4, copy);
#endif
		/* relocate these things (!) */
		for (walk = copy; !EndOfList (walk); walk = NextItem (walk)) {
			SetLocn (walk, taglocn);
			SetLocn (ThisItem (walk), taglocn);
		}
		/* check all of them.  We shouldn't really be changing the list under walktree()'s feet, but hey-ho.. */
		for (walk2 = copy; !EndOfList (walk2); walk2 = NextItem (walk2)) {
			treenode *new_tag = ThisItem (walk2);

			for (walk = taglist; !EndOfList (walk) && (ThisItem (walk) != tagptr); walk = NextItem (walk)) {
#if 0
fprintf (stderr, "checktag: checking nested tags, existing =");
printtreenl (stderr, 4, ThisItem (walk));
fprintf (stderr, "                              , incomming =");
printtreenl (stderr, 4, new_tag);
#endif
				if (NNameOf (ThisItem (walk)) == (NNameOf (new_tag))) {
					chkreport_s (CHK_INDISTINCT_TAG, taglocn, WNameOf (NNameOf (new_tag)));
				}
			}
		}
#if 0
fprintf (stderr, "checktag: mid tree mangle.  copy is:");
printtreenl (stderr, 4, copy);
#endif
		/* stitch "copy" onto "tlist" */
		SetLeft (tlist, ThisItem (copy));
		SetRight (tlist, NextItem (copy));
		for (copy=tlist; !EndOfList (NextItem (copy)); copy = NextItem (copy));		/* walk to end of list */
		SetRight (copy, restoflist);
		switch_to_prev_workspace (old);
#if 0
fprintf (stderr, "checktag: mangled tree.  new list starting at \"tlist\" is:");
printtreenl (stderr, 4, tlist);
#endif
	} else {
		for (; !EndOfList (taglist); taglist = NextItem (taglist)) {
			treenode *const that_tag = ThisItem (taglist);
			if (that_tag == tagptr) {
				/* we've got all the way to 'this' one, without a duplicate */
				if (NTypeOf (tagptr) != NULL) {
					cdefseqprotocol (NTypeOf (tagptr));
				}
				return;
			} else if (NNameOf (that_tag) == NNameOf (tagptr)) {
				chkreport_s (CHK_INDISTINCT_TAG, LocnOf (tagptr), WNameOf (NNameOf (tagptr)));
			}
		}
	}
}

/*}}}*/
/*{{{  PRIVATE BOOL fixioprot (treenode **protptr)*/
/*
 *	fixes ASINPUT/ASOUTPUT nodes in protocol declarations
 */
PRIVATE BOOL fixioprot (treenode **protptr)
{
	if (((TagOf (*protptr) == S_ASINPUT) || (TagOf (*protptr) == S_ASOUTPUT)) && (TagOf (OpOf (*protptr)) == N_TYPEDECL)) {
		const int shared = (OpTypeAttrOf (*protptr) & TypeAttr_shared);
		const int is_input = (TagOf (*protptr) == S_ASINPUT);
		treenode *typedecl = OpOf (*protptr);

		*protptr = newnamenode (TagOf (typedecl), LocnOf (typedecl), NNameOf (typedecl), NTypeOf (typedecl),
				NDeclOf (typedecl), NLexLevelOf (typedecl), NScopeOf (typedecl), NModeOf (typedecl));
		SetNTypeAttr (*protptr, (is_input ? TypeAttr_marked_in : TypeAttr_marked_out) | shared);
		return TRUE;
	}
	return FALSE;
}
/*}}}*/
/*}}}*/
/*{{{  checking instances*/

/* forward decl. */
PRIVATE void checkvariable (treenode *tptr, void *const voidptr);

#ifdef MOBILES
/*{{{  PRIVATE BOOL checkmobilevariable (treenode *tptr)*/
PRIVATE BOOL checkmobilevariable (treenode *tptr)
{
	BOOL ok = FALSE;

#if 0
fprintf (stderr, "checkmobilevariable: tptr =");
printtreenl (stderr, 4, tptr);
#endif
	switch (TagOf (tptr)) {
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_ABBR:
		{
			treenode *type = NTypeOf (tptr);

			if (TagOf (type) != S_MOBILE) {
				/* maybe its mobile underneath.. */
				type = follow_user_type (type);
#if 0
fprintf (stderr, "checkmobilevariable: N_.., but non-mobile type.  followed type =");
printtreenl (stderr, 4, type);
#endif
			}
			if (TagOf (type) == S_MOBILE) {
				return TRUE;
			}
		}
		break;
	case S_ARRAYSUB:
	case S_RECORDSUB:
		/* both base and field (index) must be MOBILE */
		{
			treenode *type = chk_gettype (tptr);

#if 0
fprintf (stderr, "chk2: checkmobilevariable: ARRAYSUB/RECORDSUB: type =");
printtreenl (stderr, 4, type);
#endif
			if (TagOf (type) != S_MOBILE) {
				type = follow_user_type (type);
			}
			if (TagOf (type) == S_MOBILE) {
				return TRUE;
			}
		}
		break;
	}
	return ok;
}
/*}}}*/
/*{{{  PRIVATE BOOL cmobileio (treenode *tptr, int ionum) */
/*
 *	void cmobileio (treenode *tptr, int ionum)
 *	checks that `tptr' is a MOBILE variable, a MOBILE function, or CLONE <something>
 *	returns TRUE if checked OK
 */
PRIVATE BOOL cmobileio (treenode *tptr, int ionum)
{
	BOOL ok = TRUE;

#if 0
fprintf (stderr, "cmobileio: tptr =");
printtreenl (stderr, 4, tptr);
#endif
	switch (TagOf (tptr)) {
	case S_CLONE:
		ok = checkmobilevariable (OpOf (tptr));
		/* if this is a chan-type, it must be shared */
		{
			treenode *ntype = chk_gettype_main (OpOf (tptr), TRUE);

			if ((TagOf (ntype) == N_TYPEDECL) && (NTypeAttrOf (ntype) & (TypeAttr_marked_in | TypeAttr_marked_out))) {
				if (!(NTypeAttrOf (ntype) & TypeAttr_shared)) {
					chkerr_s (CHK_UNSHARED_CLONE, chklocn,
							(nodetypeoftag (TagOf (OpOf (tptr))) == NAMENODE) ? WNameOf (NNameOf (OpOf (tptr))) : "...");
					ok = FALSE;
				}
			}
		}
		break;
	case S_FINSTANCE:
		/* check return type of FUNCTION is a dynamic MOBILE */
#if 0
fprintf (stderr, "cmobileio: FINSTANCE, NTypeOf (INameOf (tptr)):");
printtreenl (stderr, 4, NTypeOf (INameOf (tptr)));
#endif
		ok = checkmobilevariable (tptr);
		if (!ok) {
			if (!ionum) {
				chkerr (CHK_MOBILE_REQUIRED, chklocn);
			} else {
				chkerr_i (CHK_MOBILE_IO, chklocn, ionum);
			}
		}
		break;
	default:
		ok = checkmobilevariable (tptr);
#if 0
fprintf (stderr, "cmobileio: (default): checkmobilevariable(tptr) = %d\n", ok);
#endif
		if (!ok) {
			if (!ionum) {
				chkerr (CHK_MOBILE_REQUIRED, chklocn);
			} else {
				chkerr_i (CHK_MOBILE_IO, chklocn, ionum);
			}
		}
		break;
	}
	return ok;
}
/*}}}  */
/*{{{  PRIVATE void cdynmobilechan (treenode *lhs, treenode *rhs, treenode *lhstype, treenode *rhstype) */
/*
 *	void cdynmobilechan (treenode *lhs, treenode *rhs, treenode *lhstype, treenode *rhstype)
 *	checks special-case dynamic MOBILE []CHAN allocation/assignment
 */
PRIVATE void cdynmobilechan (treenode *lhs, treenode *rhs, treenode *lhstype, treenode *rhstype)
{
#if 0
fprintf (stderr, "cdynmobilechan: lhs =");
printtreenl (stderr, 4, lhs);
fprintf (stderr, "     ...        rhs =");
printtreenl (stderr, 4, rhs);
#endif
	switch (TagOf (rhs)) {
	case N_DECL:
	case N_PARAM:
	case N_ABBR:
	case S_NEW_ARRAY:
		/* assignment from the same type is allowed (call to checksame later ensures this) */
		return;
	default:
		/* bleh, something nasty.. */
		checkvariable (lhs, NULL);
		checkvariable (rhs, NULL);
		break;
	}
	return;
}
/*}}}  */
#endif


/*{{{  PRIVATE void csimpleprotocol (treenode *const pptr, treenode *const iptr, const INT32 pitem, const int tag)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  csimpleprotocol checks the usage of a simple protocol.
 *                  'pptr' points to the protocol definition,
 *                  'iptr' points to the protocol use,
 *                  'pitem' is the item number in a sequential protocol.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void csimpleprotocol (treenode *const pptr, treenode *const iptr, const INT32 pitem, const int tag)
{
	const BOOL anyprotocol = (TagOf (pptr) == S_ANY);

#if 0
fprintf (stderr, "csimpleprotocol: tag = %s, pptr = ", tagstring (tag));
printtreenl (stderr, 4, pptr);
fprintf (stderr, "    iptr = ");
printtreenl (stderr, 4, iptr);
#endif
	if (TagOf (iptr) == S_COLON2) {
		/*{{{  check COLON2 */
		treenode *countexp, *counttype;
		treenode *t;
		if (!anyprotocol && (TagOf (pptr) != S_COLON2))
			chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
		/*{{{  check left-hand side */
		if (anyprotocol) {
			countexp = LeftOpOf (iptr);
			counttype = typecheck (countexp, unknownnodeptr);
			if (!isintorbyte (TagOf (counttype)))
				chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
		} else {
			treenode *const p = LeftOpOf (pptr);
			countexp = LeftOpOf (iptr);
			counttype = typecheck (countexp, p);
			if (!typesequivalent (p, counttype, TRUE))
				chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
		}
		/*}}} */
		/*{{{  check right-hand side */
		if (anyprotocol) {
/* Parser ensures that RHS is an array *//* oh no it doesn't - bug TS/1515 12/12/91 */
			t = typecheck (RightOpOf (iptr), unknownnodeptr);
			if (TagOf (t) != S_ARRAY)	/* bug TS/1515 12/12/91 */
				chkreport (CHK_BAD_COLON2, chklocn);
		} else {
			treenode *const p = RightOpOf (pptr);
			t = typecheck (RightOpOf (iptr), p);
			if (!typesequivalent (p, t, TRUE))
				chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
		}
		/*}}} */
		/*{{{  allow for special case */
		/* bug TS/1183 - convert
		   chan ? len :: [array FROM 0 FOR len]
		   into
		   chan ? len :: array
		 */
		{
			treenode *rhs = RightOpOf (iptr);
			if ((nodetypeoftag (TagOf (countexp)) == NAMENODE) && (TagOf (rhs) == S_SEGMENT) && (SLengthExpOf (rhs) == countexp)) {
				treenode *start = SStartExpOf (rhs);
				start = foldexp (start);
				SetSStartExp (rhs, start);
				if (isconst (start) && (LoValOf (start) == 0)) {
					SetRightOp (iptr, SNameOf (rhs));
					if (tag != S_OUTPUT)
						msg_out_s (SEV_WARN, CHK, CHK_COLON2_INPUT, chklocn, WNameOf (NNameOf (countexp)));
					freetree (&start);
					freenode (&rhs);
				}
			}
		}
		/*}}} */
		/*{{{  check a constant count */
		if (isconst (countexp)) {
			BIT32 counthi, countlo;
			int greater;
			foldconstexp (countexp, &counthi, &countlo, CHK_EXP_NOT_CONST, LocnOf (countexp));
			if (TagOf (counttype) != S_INT64) {
				I32ToI64 (&counthi, &countlo, countlo);
			}
			Int64Gt (&greater, 0, 0, counthi, countlo);
			if (greater) {
				chkreport_i (CHK_COUNT_OUT_OF_RANGE, chklocn, pitem);
			}
			if ((TagOf (t) != S_UNDECLARED) &&	/* INSdi02426 */
			    (ARDimOf (t) != (-1))) {	/* We know the array size */
				Int64Gt (&greater, counthi, countlo, 0, ARDimOf (t));
				if (greater) {
					chkreport_i (CHK_COUNT_OUT_OF_RANGE, chklocn, pitem);
				}
			}
		}
		/*}}} */
		/*}}} */
	} else {
		if (anyprotocol) {
			/*{{{  allow any protocol, as long as it has bytes */
			treenode *const type = typecheck (iptr, unknownnodeptr);

			if (bytesin (type) == 0) {
				chkreport (CHK_BAD_ANY_PROT, chklocn);
			}
			/*}}}*/
#ifdef MOBILES
		} else if (TagOf (pptr) == S_ANYCHANTYPE) {
			/*{{{  i/o on a "ANY CHAN TYPE" protocol */
			/* check that the RHS is a mobile-channel */
			treenode *const type = typecheck (iptr, unknownnodeptr);

			if (TagOf (type) == S_ANYCHANTYPE) {
				/* special case of input/output of a MOBILE.CHAN var */
				INT32 a_attr = LeafLinkOf (pptr) ? TypeAttrOf (LeafLinkOf (pptr)) : 0;
				INT32 e_attr = LeafLinkOf (type) ? TypeAttrOf (LeafLinkOf (type)) : 0;

				if (a_attr != e_attr) {
					chkreport (CHK_ANYCHANTYPE_SPECMISMATCH, chklocn);
				}
			} else if ((tag != S_OUTPUT) && (tag != S_TAGGED_INPUT) && (tag != S_X_TAGGED_INPUT) && (tag != S_X_CASE_INPUT) && (tag != S_CASE_INPUT)) {
				/* other input types restricted to CASE if not ANYCHANTYPE */
				chkreport (CHK_ANYCHANTYPE_SIMPLEINPUT, chklocn);
			} else if (!isdynmobilechantypetype (type)) {
				chkreport (CHK_ANYCHANTYPE_MISMATCH, chklocn);
			} else {
				/* if the ANYCHANTYPE is restricted, check that too */
				if (LeafLinkOf (pptr)) {
					INT32 attr = TypeAttrOf (LeafLinkOf (pptr));

					if (((attr & TypeAttr_shared) && !(NTypeAttrOf (type) & TypeAttr_shared)) ||
							(!(attr & TypeAttr_shared) && (NTypeAttrOf (type) & TypeAttr_shared)) ||
							((attr & TypeAttr_marked_in) && !(NTypeAttrOf (type) & TypeAttr_marked_in)) ||
							((attr & TypeAttr_marked_out) && !(NTypeAttrOf (type) & TypeAttr_marked_out))) {
						chkreport (CHK_ANYCHANTYPE_RESTRICTED, chklocn);
					}
				}
			}
			/*}}}*/
		} else if (TagOf (pptr) == S_ANYPROCTYPE) {
			/*{{{  i/o on a "MOBILE.PROC" protocol*/
			/* check that the RHS is a mobile process */
			treenode *const type = typecheck (iptr, unknownnodeptr);

			if (TagOf (type) == S_ANYPROCTYPE) {
				/* good */
			} else if ((tag != S_OUTPUT) && (tag != S_TAGGED_INPUT) && (tag != S_X_TAGGED_INPUT) && (tag != S_X_CASE_INPUT) && (tag != S_CASE_INPUT)) {
				/* other input types restricted to CASE if not ANYPROCTYPE */
				chkreport (CHK_ANYPROCTYPE_SIMPLEINPUT, chklocn);
			} else if (!isdynmobileproctypetype (type)) {
				chkreport (CHK_ANYPROCTYPE_MISMATCH, chklocn);
			}
			/*}}}*/
		} else if (TagOf (pptr) == S_ANYMOBILETYPE) {
			/*{{{  i/o on a "MOBILE.ANY" protocol*/
			/* check that the RHS is a mobile */
			treenode *const type = typecheck (iptr, unknownnodeptr);

			if (TagOf (type) == S_ANYMOBILETYPE) {
				/* good */
			} else if (TagOf (follow_user_type (type)) != S_MOBILE) {
				chkreport (CHK_ANYMOBILETYPE_MISMATCH, chklocn);
			}
			/*}}}*/
		} else if ((TagOf (iptr) == N_DECL) && (TagOf (NTypeOf (iptr)) == S_MOBILE) && (TagOf (follow_user_type (pptr)) != S_MOBILE)) {
			/*{{{  MOBILE i/o on non-mobile channel*/
#if 0
fprintf (stderr, "csimpleprotocol: MOBILE i/o on non-mobile channel.\n");
#endif
			/* MOBILE i/o on non-mobile channel */
			treenode *const type = typecheck (iptr, unknownnodeptr);

			if (!typesequivalent (pptr, MTypeOf (type), TRUE)) {
				chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
			}
			/*}}}*/
		} else if (TagOf (follow_user_type (pptr)) == S_MOBILE) {
			/*{{{  MOBILE i/o*/
			/* if outputting on a MOBILE channel, might need to mobileise (temporary, handled by tran) if non-mobile */
			if (tag == S_OUTPUT) {
				/*{{{  MOBILE output*/
				treenode *const rhstype = typecheck (iptr, unknownnodeptr);

#if 0
fprintf (stderr, "csimpleprotocol: MOBILE output on MOBILE channel.  rhstype (typecheck'd iptr) [%p] =", rhstype);
printtreenl (stderr, 4, rhstype);
fprintf (stderr, "csimpleprotocol: MOBILE output on MOBILE channel.  iptr [%p] =", iptr);
printtreenl (stderr, 4, iptr);
fprintf (stderr, "csimpleprotocol: MOBILE output on MOBILE channel.  pptr [%p] =", pptr);
printtreenl (stderr, 4, pptr);
if (nodetypeoftag (TagOf (iptr)) == NAMENODE) {
	fprintf (stderr, "csimpleprotocol: item is NAMENODE, NTypeOf(iptr) =");
	printtreenl (stderr, 4, NTypeOf (iptr));
}
#endif
				if ((TagOf (rhstype) == N_TYPEDECL) && (TagOf (pptr) == N_TYPEDECL) &&
						(TagOf (follow_user_type (rhstype)) == S_MOBILE) && (TagOf (follow_user_type (pptr)) == S_MOBILE)) {
#if 0
fprintf (stderr, "csimpleprotocol: MOBILE output on MOBILE channel.  follow_user_type (rhstype) (typecheck'd iptr) [%p] =", follow_user_type (rhstype));
printtreenl (stderr, 4, rhstype);
fprintf (stderr, "csimpleprotocol: MOBILE output on MOBILE channel.  follow_user_type (pptr) [%p] =", follow_user_type (pptr));
printtreenl (stderr, 4, pptr);
#endif
					if (follow_user_type (rhstype) != follow_user_type (pptr)) {
						chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
					}
					/* check for incompatability on direction or shared-ness */
					if ((NTypeAttrOf (rhstype) & (TypeAttr_shared | TypeAttr_marked_in | TypeAttr_marked_out)) != (NTypeAttrOf (pptr) & (TypeAttr_shared | TypeAttr_marked_in | TypeAttr_marked_out))) {
						chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
					}
				} else if (TagOf (follow_user_type (rhstype)) == S_MOBILE) {
					if (!typesequivalent (pptr, rhstype, TRUE)) {
						chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
					}
				} else {
					/* disallow communication of FULLBARRIER's */
					if (TagOf (follow_user_type (rhstype)) == S_FULLBARRIER) {
						chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
					} else if (!typesequivalent (MTypeOf (follow_user_type (pptr)), rhstype, TRUE)) {
						chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
					}
				}
				/*}}}*/
			} else {
				/*{{{  MOBILE input*/
				/* MOBILE input must involve something MOBILE */
				if (cmobileio (iptr, pitem)) {
					treenode *const rhstype = typecheck (iptr, unknownnodeptr);

#if 0
fprintf (stderr, "csimpleprotocol: MOBILE input on MOBILE channel.  rhstype (typecheck'd iptr) =");
printtreenl (stderr, 4, rhstype);
fprintf (stderr, "csimpleprotocol: MOBILE input on MOBILE channel.  pptr =");
printtreenl (stderr, 4, pptr);
#endif
					if ((TagOf (rhstype) == N_TYPEDECL) && (TagOf (pptr) == N_TYPEDECL) &&
							(TagOf (follow_user_type (rhstype)) == S_MOBILE) && (TagOf (follow_user_type (pptr)) == S_MOBILE)) {
#if 0
fprintf (stderr, "csimpleprotocol: MOBILE input on MOBILE channel.  follow_user_type (rhstype) (typecheck'd iptr) [%p] =", follow_user_type (rhstype));
printtreenl (stderr, 4, rhstype);
fprintf (stderr, "csimpleprotocol: MOBILE input on MOBILE channel.  follow_user_type (pptr) [%p] =", follow_user_type (pptr));
printtreenl (stderr, 4, pptr);
#endif
						if (follow_user_type (rhstype) != follow_user_type (pptr)) {
							chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
						}
						/* check for incompatability on direction or shared-ness */
						if ((NTypeAttrOf (rhstype) & (TypeAttr_shared | TypeAttr_marked_in | TypeAttr_marked_out)) != (NTypeAttrOf (pptr) & (TypeAttr_shared | TypeAttr_marked_in | TypeAttr_marked_out))) {
							chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
						}
					} else if (!typesequivalent (pptr, rhstype, TRUE)) {
#if 0
fprintf (stderr, "csimpleprotocol: typesequivalent failed!\n");
#endif
						chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
					}
				}
				if (check_isfullbarrier (iptr) && check_resignedfullbarrier (iptr)) {
					chkreport (CHK_BARRIER_RESIGNED, chklocn);
				}
				/*}}}*/
			}
			/*}}}*/
#endif
		} else {
			treenode *itype = typecheck (iptr, pptr);
			
#if 0
fprintf (stderr, "csimpleprotocol: MOBILE i/o ...?\n");
#endif
#if 0
fprintf (stderr, "csimpleprotocol: typecheck'd iptr, got: ");
printtreenl (stderr, 4, itype);
#endif
			if (!typesequivalent (pptr, itype, TRUE)) {
				chkreport_i (CHK_PTYPE_MISMATCH, chklocn, pitem);
			}
		}
	}
}

/*}}}*/
/*{{{  PRIVATE void cseqprotocol (pptr, iptr)*/
/*****************************************************************************
 *
 *  cseqprotocol checks the usage of a sequential protocol.
 *               'pptr' points to the protocol definition,
 *               'iptr' points to the protocol instance.
 *
 *****************************************************************************/

/* Protocol definition */
/* Protocol usage */
PRIVATE void cseqprotocol (treenode * pptr, treenode * iptr, const int tag)
{
	int pitem = 1;
	const BOOL anyprotocol = (TagOf (pptr) == S_ANY);
	while (!EndOfList (iptr) && (anyprotocol || !EndOfList (pptr)))
		/*{{{  check this item, move to next item */
	{
		if (anyprotocol)
			csimpleprotocol (pptr, ThisItem (iptr), pitem, tag);
		else {
			csimpleprotocol (ThisItem (pptr), ThisItem (iptr), pitem, tag);
			pptr = NextItem (pptr);
		}
		iptr = NextItem (iptr);
		pitem++;
	}
	/*}}} */
	if (!anyprotocol && !EndOfList (pptr))
		chkreport (CHK_TOO_FEW_PITEMS, chklocn);
	else if (!EndOfList (iptr))
		chkreport (CHK_TOO_MANY_PITEMS, chklocn);
}

/*}}}*/
/*{{{  PRIVATE void protocolcheck (protocol, instance)*/
/*****************************************************************************
 *
 *  protocolcheck checks the usage of a protocol.
 *                'protocol' points to the protocol definition,
 *                'instance' points to the protocol instance.
 *
 *****************************************************************************/

/* Tree representing protocol definition */
/* The protocol usage to be checked */
PRIVATE void protocolcheck (treenode * protocol, treenode * instance, const int tag)
{
#if 0
fprintf (stderr, "protocolcheck: tag = %d, protocol = ", tag);
printtreenl (stderr, 4, protocol);
fprintf (stderr, "protocolcheck: instance = ");
printtreenl (stderr ,4, instance);
#endif
	switch (TagOf (protocol)) {
		/*{{{  sequential protocol definition and ANY */
	case N_SPROTDEF:
		cseqprotocol (NTypeOf (protocol), instance, tag);
		break;
	case S_ANY:
		cseqprotocol (protocol, instance, tag);
		break;
		/*}}} */
		/*{{{  tagged protocol definition */
	case N_TPROTDEF:
		{
			treenode *instancetag;
			treenode *tagptr;
			treenode *sptr;
			BOOL found;

			/*{{{  find a tag in the definition which corresponds to instance tag */

			/*{{{  point instancetag to the first item of the instance list */
			if (TagOf (instance) == S_LIST)
				instancetag = ThisItem (instance);
			else
				instancetag = instance;

			if (TagOf (instancetag) != N_TAGDEF)
				chkreport (CHK_NOT_A_TAG, chklocn);
			/*}}} */

			tagptr = NTypeOf (protocol);
			found = FALSE;
			while (!EndOfList (tagptr) && !found) {
				if (ThisItem (tagptr) == instancetag) {
					tagptr = ThisItem (tagptr);
					found = TRUE;
				} else {
					tagptr = NextItem (tagptr);
				}
			}
			if ((found == FALSE) && (tagptr != instancetag))
				chkreport (CHK_BAD_PTAG, chklocn);
			/*}}} */

			/*{{{  see if there is a sequential protocol following the tag */
			sptr = NTypeOf (tagptr);
			if (EmptyList (sptr))
				/*{{{  there is no sequential protocol, check none is used */
			{
				if (!EmptyList (NextItem (instance)))
					chkreport (CHK_TOO_MANY_PITEMS, chklocn);
			}
			/*}}} */
			else
				/*{{{  there is a sequential protocol check it */
				cseqprotocol (sptr, NextItem (instance), tag);
			/*}}} */
			/*}}} */
			break;
		}
		/*}}} */
	default:
		/*{{{  simple protocol */
		{
			if (!EmptyList (NextItem (instance))) {
				chkreport (CHK_TOO_MANY_PITEMS, chklocn);
			} else {
				csimpleprotocol (protocol, ThisItem (instance), 1, tag);
			}
		}
		/*}}} */
	}
}

/*}}}*/
/*}}}*/
/*}}}*/
/*{{{  PRIVATE BOOL validval (tptr)*/
/*****************************************************************************
 *
 *  validval takes a type tree and returns TRUE if it is a legal type for a
 *           VAL abbreviation, retype or parameter.
 *
 *****************************************************************************/
PRIVATE BOOL validval (treenode * tptr)
{
	/* we can't use basetype for this loop, cos it goes inside PORTs */
	while (TagOf (tptr) == S_ARRAY)
		tptr = ARTypeOf (tptr);

	switch (TagOf (tptr)) {
	case S_CHAN:
	case S_PORT:
	case S_TIMER:
		return FALSE;
#ifdef OCCAM2_5
	case N_TYPEDECL:
		return validval (follow_user_type (tptr));
#endif
#ifdef MOBILES
	case S_MOBILE:
		return validval (MTypeOf (tptr));	/* follow mobile type */
#endif
	default:
		return !network_datatype (TagOf (tptr));
	}
}

/*}}}*/
/*{{{  PRIVATE BOOL iselement (tptr)*/
/*****************************************************************************
 *
 *  iselement takes an expression tree 'tptr' and returns TRUE if it is
 *            an element.
 *            if "modifiable" is set FALSE, tests that the element may be
 *            written to.
 *
 *****************************************************************************/
PRIVATE BOOL iselement (treenode * const tptr, const BOOL modifiable)
{
	treenode *const nptr = nameof (tptr);

	switch (TagOf (nptr)) {
	default:
		return FALSE;
	case N_ABBR:
	case N_RETYPE:
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
		/* This modifiablility test added 10/10/90 to prevent chan := chan */
		return (modifiable || validval (NTypeOf (nptr)));
	}
}

/*}}}*/
/*{{{  PRIVATE BOOL istableelement (tptr)*/
/*****************************************************************************
 *
 *  istableelement takes an expression tree 'tptr' and returns TRUE if it is a
 *            (modifiable) element or table of elements.
 *
 *****************************************************************************/
PRIVATE BOOL istableelement (treenode * tptr, const BOOL modifiable)
{
	switch (TagOf (tptr))
		/*{{{  cases */
	{
	default:
		return iselement (tptr, modifiable);
	case S_CONSTRUCTOR:
		/*{{{  look at list */
		{
			for (tptr = LitExpOf (tptr); !EndOfList (tptr); tptr = NextItem (tptr))
				if (!istableelement (ThisItem (tptr), modifiable))
					return FALSE;
			return TRUE;
		}
		/*}}} */
	case S_ARRAYCONSTRUCTOR:
		return istableelement (ReplCBodyOf (tptr), modifiable);
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATEPARAM void checkvariable (tptr)*/
/*****************************************************************************
 *
 *  checkvariable checks that the tree 'tptr' is writable.
 *
 *****************************************************************************/
PRIVATE void checkvariable (treenode *tptr, void *const voidptr)
{
	(void)voidptr;

#if 0
fprintf (stderr, "checkvariable: ");
printtreenl (stderr, 4, tptr);
#endif
	if (!iselement (tptr, FALSE)) {
		treenode *const nameptr = nameof (tptr);

		switch (TagOf (nameptr)) {
		case S_STRING:
			chkreport (CHK_BAD_DEST_STRING, chklocn);
			break;
		case S_CONSTCONSTRUCTOR:
		case S_CONSTRUCTOR:
			chkreport (CHK_BAD_DEST_CONSTRUCTOR, chklocn);
			break;
		default:
			chkreport_s (CHK_BAD_DEST, chklocn, WNameOf (NNameOf (nameptr)));
			break;
		}
	}
}

/*}}}*/
/*{{{  PRIVATEPARAM void checkprotvariable (tptr)*/
/*****************************************************************************
 *
 *  checkprotvariable checks that the tree 'tptr' is a valid input destination
 *
 *****************************************************************************/
PRIVATEPARAM void checkprotvariable (treenode *tptr, void *voidptr)
{
	voidptr = voidptr;
	if (TagOf (tptr) == S_COLON2) {
		checkvariable (LeftOpOf (tptr), NULL);
		checkvariable (RightOpOf (tptr), NULL);
	} else {
		checkvariable (tptr, NULL);
	}
}

/*}}}*/
/*{{{  PRIVATE void checkexpsandtypes(exps, types, e_bad_type, e_too_many_vars, ..*/
/*****************************************************************************
 *
 *  checkexpsandtypes takes a list of expressions 'exps' and a list of
 *                    types 'types', type checks each expression and
 *                    ensures it has the same type as the corresponding
 *                    entry of 'types'.
 *
 *****************************************************************************/
PRIVATE void checkexpsandtypes (treenode * exps, treenode * types, const int e_bad_multi_type, const int e_too_many_vars, const int e_too_few_vars)
{
	int varno = 1;
	while (!EndOfList (exps) && !EndOfList (types))
		/*{{{  check item on list, move to next item */
	{
		treenode *const type = ThisItem (types);
		if (!typesequivalent (type, typecheck (ThisItem (exps), type), TRUE)) {
			char buf[50];
			sprintf (buf, "%d", varno);
			chkreport_s (e_bad_multi_type, chklocn, buf);
		}
		exps = NextItem (exps);
		types = NextItem (types);
		varno++;
	}
	/*}}} */
	if (!EndOfList (exps))
		chkreport (e_too_many_vars, chklocn);
	else if (!EndOfList (types))
		chkreport (e_too_few_vars, chklocn);
}

/*}}}*/
/*{{{  PRIVATE void checkasslist(lhs, rhs)*/
/*****************************************************************************
 *
 *  checkasslist type checks the lists of assignments whose sources are 'rhs'
 *               and whose destinations are 'lhs'.
 *
 *****************************************************************************/
PRIVATE void checkasslist (treenode * lhs, treenode * rhs)
{
	int varno = 1;
	const int old = switch_to_temp_workspace ();
	while (!EndOfList (lhs) && !EndOfList (rhs)) {
		char buf[50];	/* big enough for a decimal number */
		treenode *const lhs_type = typecheck (ThisItem (lhs), unknownnodeptr);

		sprintf (buf, "%d", varno);
		checksame (ThisItem (lhs), ThisItem (rhs), unknownnodeptr, CHK_INVTYPE_MULTI_ASS, buf);


#ifdef MOBILES
		if (TagOf (follow_user_type (lhs_type)) == S_MOBILE) {
			treenode *const rhs_type = typecheck (ThisItem (rhs), unknownnodeptr);

			if ((TagOf (follow_user_type (lhs_type)) == S_MOBILE) && (TagOf (follow_user_type (rhs_type)) == S_MOBILE)) {
				cmobileio (ThisItem (rhs), 0);
			}
			if (check_isfullbarrier (lhs)) {
				if (check_resignedfullbarrier (lhs) || check_resignedfullbarrier (rhs)) {
					chkreport (CHK_BARRIER_RESIGNED, chklocn);
				}
			}
		}
#endif
		if (TagOf (follow_user_type (lhs_type)) == S_FULLBARRIER) {
			/* assignment to a BARRIER disallowed, but allow MOBILE BARRIER assignment */
			chkreport (CHK_BARRIER_ASSIGN, chklocn);
		}
		lhs = NextItem (lhs);
		rhs = NextItem (rhs);
		varno++;
	}
	switch_to_prev_workspace (old);
	if (!EndOfList (lhs)) {
		chkreport (CHK_TOO_MANY_VARS, chklocn);
	} else if (!EndOfList (rhs)) {
		chkreport (CHK_TOO_FEW_VARS, chklocn);
	}
}

/*}}}*/

/*{{{  PRIVATE void check_port*/
PRIVATE void check_port (treenode * const tptr)
/* Check that PORTs are PLACED.
   Only check for locals; formal params, abbreviations, etc, are OK.
*/
{
	treenode *const nptr = nameof (tptr);

#if 0
fprintf (stderr, "chk2: check_port(): tptr = ");
printtreenl (stderr, 4, tptr);
#endif
	if ((TagOf (nptr) == N_DECL) && (NModeOf (nptr) != NM_PLACED)) {
		/* new style PORT handling puts TypeAttr_placed on the TYPENODE of the name -- must be an S_ARRAY */
		if ((TagOf (NTypeOf (nptr)) == S_ARRAY) && (TypeAttrOf (NTypeOf (nptr)) & TypeAttr_placed)) {
			/* safe */
			return;
		}
		if (err_memo_err (current_fe_handle, CHK, CHK_UNPLACED_PORT, LocnOf (nptr))) {
			msg_out_s (SEV_WARN, CHK, CHK_UNPLACED_PORT, LocnOf (nptr), WNameOf (NNameOf (nptr)));
		}
	}
}

/*}}}*/
/*{{{  PRIVATE void checkvariant (treenode *const pptr, treenode *vptr)*/
/*****************************************************************************
 *
 *  checkvariant checks the variant tree 'vptr' against the channel protocol
 *               'pptr'.
 *
 *****************************************************************************/
PRIVATE void checkvariant (treenode *const pptr, treenode *vptr)
{
	/*{{{  skip over any leading specifications to the real variant */
	vptr = skipspecifications (vptr);
	/*}}} */
	chklocn = LocnOf (vptr);

	if ((TagOf (vptr) == S_VARIANT) || (TagOf (vptr) == S_X_VARIANT)) {
		protocolcheck (pptr, VRTaggedListOf (vptr), S_CASE_INPUT);
		walklist (checkprotvariable, NextItem (VRTaggedListOf (vptr)), NULL);
	} else {
		msg_out (SEV_INTERNAL, CHK, CHK_LOST_VARIANT, chklocn);
	}
}

/*}}}*/
/*{{{  PRIVATE int compvariants (v1, v2)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  compvariants takes two pointers to vntnodes, 'v1', 'v2', and returns
 *             1 if case v1 > case v2,
 *             0 if case v1 = case v2,
 *            -1 if case v1 < case v2.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE int compvariants (const void *const p1, const void *const p2)
{
	const vntnode_t *const v1 = p1;
	const vntnode_t *const v2 = p2;
	return ((NTValueOf (v1->vnt_tag) > NTValueOf (v2->vnt_tag)) ? 1 : (NTValueOf (v1->vnt_tag) == NTValueOf (v2->vnt_tag)) ? 0 : -1);
}
PRIVATE int compvariants_anychantype (const void *const p1, const void *const p2)
{
	const vntnode_t *const v1 = p1;
	const vntnode_t *const v2 = p2;
	const INT32 hash1 = (const INT32)(v1->vnt_tag);
	const INT32 hash2 = (const INT32)(v2->vnt_tag);

	return ((hash1 > hash2) ? 1 : (hash1 == hash2) ? 0 : -1);
}
PRIVATE int compvariants_anyproctype (const void *const p1, const void *const p2)
{
	const vntnode_t *const v1 = p1;
	const vntnode_t *const v2 = p2;
	const INT32 hash1 = (const INT32)(v1->vnt_tag);
	const INT32 hash2 = (const INT32)(v2->vnt_tag);

	return ((hash1 > hash2) ? 1 : (hash1 == hash2) ? 0 : -1);
}

/*}}}*/
/*{{{  PUBLIC void checkelement*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  checkelement makes sure the tptr is an element
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void checkelement (treenode * const tptr, treenode * type, const int paramno)
{
	while (TagOf (type) == S_ARRAY) {
		type = ARTypeOf (type);
	}

	if ((TagOf (type) == S_CHAN) && current_fe_data->fe_chanaspointer) {
		if (istableelement (tptr, TRUE)) {
			return;
		}
	} else if (TagOf (tptr) == S_NULLARRAY) {
		/* these are valid elements -- of zero size */
		return;
	} else if (iselement (tptr, TRUE)) {
		if (TagOf (type) == S_PORT) {
			check_port (tptr);	/* bug TS/1985 17/02/93 */
		}
		return;
	}
	if (paramno == 0) {
		chkreport (CHK_INV_ABBR, chklocn);
	} else {
		chkreport_i (CHK_INVVARIABLE_PARAM, chklocn, paramno);
	}
}

/*}}}*/
/*{{{  PRIVATE void checkforopendimslist(treenode *)*/
PRIVATE void checkforopendimslist (treenode * tptr)
/* This is used to check that we don't attempt to use 'open' arrays
   in multiple assignment */
{
	for (; !EndOfList (tptr); tptr = NextItem (tptr)) {
		int unknowndims = 0;
		treenode *type;
		for (type = chk_gettype (ThisItem (tptr)); TagOf (type) == S_ARRAY; type = ARTypeOf (type))
			if (ARDimLengthOf (type) == NULL)
				unknowndims++;
		if (unknowndims != 0)
			chkreport (CHK_INV_MULTIASSIGN, chklocn);
	}
}

/*}}}*/
/*{{{  PRIVATEPARAM void foldvariant (vptr)*/
/*****************************************************************************
 *
 *  foldvariant folds a protocol variant, 'vptr'.
 *
 *****************************************************************************/
PRIVATEPARAM void foldvariant (treenode * vptr, void *const voidptr)
{
	(void) voidptr;
	vptr = skipspecifications (vptr);
	if ((TagOf (vptr) == S_VARIANT) || (TagOf (vptr) == S_X_VARIANT)) {
		SetVRTaggedList (vptr, foldexplist (VRTaggedListOf (vptr)));
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *caction(tptr)                  actions*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  caction performs semantic checking on action tree 'tptr'
 *
 *****************************************************************************/
/*}}}*/
PUBLIC treenode *caction (treenode * tptr)
{
	jmp_buf savedenv;
	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	/*{{{  recover from errors */
	if (setjmp (env) != 0) {
		memcpy ((char *) env, (char *) savedenv, sizeof (env));
		return NULL;
	}
	/*}}} */
	chklocn = LocnOf (tptr);
	switch (TagOf (tptr)) {
		/*{{{  case S_ASS*/
	case S_ASS:
		{
			treenode *lhs = LHSOf (tptr);
			treenode *rhs = RHSOf (tptr);

#if 0
fprintf (stderr, "caction: S_ASS: tptr = ");
printtreenl (stderr, 4, tptr);
#endif
			if (TagOf (lhs) == S_LIST) {
				/*{{{  we have a multi-assignment */
				walklist ((void *)checkvariable, lhs, NULL);
				if (listitems (rhs) == 1) {
					/*{{{  multi-valued function or valof */
					/* if rhs is a multi-valued function or VALOF, there is only one expression
					   on rhs, otherwise there is a list of expressions */
					treenode *exp1 = ThisItem (rhs);

					if (TagOf (exp1) == S_FINSTANCE) {
						/*{{{  rhs is a multi-valued function call */
						treenode *nameptr = INameOf (exp1);
						treenode *rtypes;

						checkparams (exp1, S_FUNCTION);
						/*{{{  skip if FUNCTION is not defined */
						/*
						   This used to say:
						   if (TagOf(nameptr) &&
						   (TagOf(NTypeOf(nameptr)) == S_UNDECLARED))
						   WHAT was that supposed to mean? - CON 26/11/92
						 */
						if ((nameptr == NULL) || (NTypeOf (nameptr) == NULL) || (TagOf (NTypeOf (nameptr)) == S_UNDECLARED)) {
							memcpy ((char *) env, (char *) savedenv, sizeof (env));
							return NULL;
						}
						/*}}} */
						rtypes = FnTypeListOf (NTypeOf (nameptr));
						checkexpsandtypes (lhs, rtypes, CHK_INVTYPE_MULTI_ASS, CHK_TOO_MANY_VARS, CHK_TOO_FEW_VARS);
						/* fold lhs and rhs parameters */
						SetLHS (tptr, foldexplist (LHSOf (tptr)));
						SetIParamList (exp1, foldexplist (IParamListOf (exp1)));
						/*}}} */
					} else if (isspecification (exp1) || (TagOf (exp1) == S_VALOF)) {
						/*{{{  rhs is a valof */
						exp1 = skipspecifications (exp1);
						checkasslist (lhs, VLResultListOf (exp1));
						/* fold lhs and right hand side result list */
						SetLHS (tptr, foldexplist (LHSOf (tptr)));
						SetVLResultList (exp1, foldexplist (VLResultListOf (exp1)));
						check_valof_types (exp1);	/* bug TS/1442 4/11/91 */
						/*}}} */
					} else {
						chkreport (CHK_TOO_MANY_VARS, chklocn);
					}
					/*}}} */
				} else {
					/*{{{  list of simple assignments */
					checkasslist (lhs, rhs);
					SetLHS (tptr, foldexplist (LHSOf (tptr)));
					SetRHS (tptr, foldexplist (RHSOf (tptr)));
#if 0
fprintf (stderr, "caction: folded trees for assignment list\n");
#endif
					checkforopendimslist (lhs);
					checkforopendimslist (rhs);
					/*}}} */
				}
				/*}}} */
			} else {
				int old;
#ifdef MOBILES
				{
					treenode *lhs_type = typecheck (lhs, unknownnodeptr);
					/* follow_user_type (chk_gettype (lhs)); */

					lhs_type = follow_user_type (lhs_type);
					if (TagOf (lhs_type) == S_MOBILE) {
						/*{{{  check for MOBILE assignment */
						treenode *const rhs_type = typecheck (rhs, MTypeOf (lhs_type));

#if 0
fprintf (stderr, "caction: S_ASS (single): lhs_type =");
printtreenl (stderr, 4, lhs_type);
fprintf (stderr, "caction: S_ASS (single): rhs_type =");
printtreenl (stderr, 4, rhs_type);
#endif
						if ((TagOf (MTypeOf (lhs_type)) == S_ARRAY) && (TagOf (ARTypeOf (MTypeOf (lhs_type))) == S_CHAN)) {
							cdynmobilechan (lhs, rhs, lhs_type, rhs_type);
						} else {
							checkvariable (lhs, NULL);
							if (TagOf (follow_user_type (rhs_type)) == S_MOBILE) {
								/* would be a MOBILE swap */
								treenode *orig_type = chk_gettype_main (rhs, TRUE);

								if (isdynmobilechantype (lhs) && isdynmobilechantype (rhs) &&
										((TagOf (orig_type) == N_TYPEDECL) && (NTypeAttrOf (orig_type) & TypeAttr_shared))) {
									/* auto-CLONE (part 3), for mobile channel types, CLONE rather than swap */
									SetRHS (tptr, newmopnode (S_CLONE, NOPOSN, rhs, S_MOBILE));
								}
								cmobileio (rhs, 0);
							}
						}
						/*}}}*/
					} else if (TagOf (lhs_type) == S_FULLBARRIER) {
						/* allow MOBILE BARRIER assignment */
						chkreport (CHK_BARRIER_ASSIGN, chklocn);
					} else {
						checkvariable (lhs, NULL);
					}

					if (check_isfullbarrier (lhs)) {
						if (check_resignedfullbarrier (lhs) || check_resignedfullbarrier (rhs)) {
							chkreport (CHK_BARRIER_RESIGNED, chklocn);
						}
					}
				}
#else	/* !MOBILES */
				checkvariable (lhs, NULL);
#endif	/* !MOBILES */
#if 0
fprintf (stderr, "caction: S_ASS: single: rhs @= %p\n", rhs);
#endif
				/* initial decls can sometimes leave this as NULL */
				if (rhs) {
					if (TagOf (rhs) == S_LIST) {
						chkreport_s (CHK_INVTYPE_MULTI_ASS, chklocn, "");
					}
					old = switch_to_temp_workspace ();
					checksame (lhs, rhs, unknownnodeptr, CHK_INVTYPE_MULTI_ASS, "");
					switch_to_prev_workspace (old);
				}
				/*{{{  fold the left and right-hand sides */
				SetLHS (tptr, foldexp (LHSOf (tptr)));
				if (RHSOf (tptr)) {
					SetRHS (tptr, foldexp (RHSOf (tptr)));
				} else {
					SetRHS (tptr, unknownnodeptr);
				}
				/*}}} */
			}
		}
		break;
		/*}}} */
		/*{{{  case S_OUTPUT, S_TAGGED_INPUT, S_X_TAGGED_INPUT */
	case S_OUTPUT:
	case S_TAGGED_INPUT:
	case S_X_TAGGED_INPUT:
		/* Check that lhs is a channel.
		   Check that the rhs conforms to the channel protocol.
		 */
		{
			treenode *t;
			BIT32 cspec;

			/* Type check the channel */
			t = typecheck (LHSOf (tptr), unknownnodeptr);

#if 0
fprintf (stderr, "chk2: caction: comms: t (typechecked LHS) = ");
printtreenl (stderr, 4, t);
#endif
			/* anything about direction specifiers? */
			cspec = (TagOf (t) == S_CHAN) ? (TypeAttrOf (t) & (TypeAttr_marked_in | TypeAttr_marked_out)) : 0;
#ifdef MOBILES
			/* if we're the CLIENT end of a CHAN TYPE variable, invert the direction specifier on the channel (which is server relative) */
			if (TagOf (LHSOf (tptr)) == S_RECORDSUB) {
				treenode *base = ASBaseOf (LHSOf (tptr));
				treenode *basetype;

#if 0
fprintf (stderr, "caction: LHS of IO looks like a RECORDSUB.. cspec = 0x%x, NTypeAttrOf (chk_gettype_main (base, TRUE)) = 0x%x, base =", cspec, NTypeAttrOf (chk_gettype_main (base, TRUE)));
printtreenl (stderr, 4, base);
#endif
				basetype = chk_gettype_main (base, TRUE);
				if (NTypeAttrOf (basetype) & TypeAttr_marked_out) {
					cspec ^= (TypeAttr_marked_in | TypeAttr_marked_out);
				}
			}
			if ((cspec & TypeAttr_marked_in) && (TagOf (tptr) == S_OUTPUT)) {
				chkreport (CHK_CHANDIR_NOT_OUTPUT, chklocn);
			} else if ((cspec & TypeAttr_marked_out) && ((TagOf (tptr) == S_INPUT) || (TagOf (tptr) == S_X_INPUT))) {
				chkreport (CHK_CHANDIR_NOT_INPUT, chklocn);
			}

			/* auto-CLONE code for outputting SHARED mobile channel-types */
			if ((TagOf (t) == S_CHAN) && (TagOf (tptr) == S_OUTPUT)) {
				treenode *rhs = RHSOf (tptr);
				
				if (TagOf (rhs) == S_LIST) {
					for (; !EndOfList (rhs); rhs = NextItem (rhs)) {
						treenode *itm = ThisItem (rhs);

						if ((TagOf (itm) != S_CLONE) && isdynmobilechantype (itm) && !isanychantype (itm)) {
							treenode *itmtype = chk_gettype_main (itm, TRUE);

							if (NTypeAttrOf (itmtype) & TypeAttr_shared) {
								SetLeft (rhs, newmopnode (S_CLONE, LocnOf (tptr), itm, S_MOBILE));
							}
						}
					}
				}
			}
#else	/* !MOBILES */
			if ((cspec & TypeAttr_marked_in) && (TagOf (tptr) == S_OUTPUT)) {
				chkreport (CHK_CHANDIR_NOT_OUTPUT, chklocn);
			} else if ((cspec & TypeAttr_marked_out) && ((TagOf (tptr) == S_INPUT) || (TagOf (tptr) == S_X_INPUT))) {
				chkreport (CHK_CHANDIR_NOT_INPUT, chklocn);
			}
#endif	/* !MOBILES */
#if 0
fprintf (stderr, "caction: t (type of LHS) =");
printtreenl (stderr, 4, t);
#endif
			if ((TagOf (t) == S_CHAN) || (TagOf (t) == S_PORT)) {
				/*{{{  protocol check the right-hand side */
				treenode *const protocol = ProtocolOf (t);

				if ((TagOf (tptr) == S_TAGGED_INPUT) || (TagOf (tptr) == S_X_TAGGED_INPUT)) {
					/*{{{  check the protocol is a tagged protocol */
					if ((TagOf (protocol) != N_TPROTDEF) && (TagOf (protocol) != S_ANYCHANTYPE) && (TagOf (protocol) != S_ANYPROCTYPE)) {
						chkreport (CHK_BAD_TAGGED_INPUT_PROTOCOL, chklocn);
					}
					/*}}} */
				}
				protocolcheck (protocol, RHSOf (tptr), TagOf (tptr));
				/*{{{  check all the input destinations are variables */
				if ((TagOf (tptr) == S_TAGGED_INPUT) || (TagOf (tptr) == S_X_TAGGED_INPUT)) {
					/* But don't check the tag */
					walklist (checkprotvariable, NextItem (RHSOf (tptr)), NULL);
				}
				/*}}} */
				/*{{{  fold the left and right-hand sides */
				SetLHS (tptr, foldexp (LHSOf (tptr)));
				SetRHS (tptr, foldexplist (RHSOf (tptr)));
				/*}}} */
				/*{{{  check for non-exhaustive tagged input */
				if (((TagOf (tptr) == S_TAGGED_INPUT) || (TagOf (tptr) == S_X_TAGGED_INPUT)) && current_fe_data->fe_warning_tag_input) {
					/* bug TS/2048 22/01/93 */
					if (listitems (NTypeOf (protocol)) != 1) {
						msg_out (SEV_WARN, CHK, CHK_NON_EXHAUSTIVE_CASE_INPUT, chklocn);
					}
				}
				/*}}} */
				if (TagOf (t) == S_PORT) {
					check_port (LHSOf (tptr));	/* bug TS/1985 17/02/93 */
				}
				/*}}} */
			} else if (TagOf (t) != S_UNDECLARED) {
				chkreport (CHK_NOT_CHANNEL, chklocn);
			}
		}
		break;
		/*}}} */
		/*{{{  case S_CASE_INPUT S_X_CASE_INPUT */
	case S_CASE_INPUT:
	case S_X_CASE_INPUT:
		/* type check the channel, protocol check each variant on the variant list */
		{
			treenode *t;
			BIT32 cspec;

			/* Type check the channel */
			t = typecheck (LHSOf (tptr), unknownnodeptr);

#if 0
fprintf (stderr, "chk2: caction: [X_]CASE_INPUT: LHSOf (tptr) = ");
printtreenl (stderr, 4, LHSOf (tptr));
#endif
			/* anything about direction specifiers? */
			cspec = (TagOf (t) == S_CHAN) ? (TypeAttrOf (t) & (TypeAttr_marked_in | TypeAttr_marked_out)) : 0;
#ifdef MOBILES
			/* if we're the CLIENT end of a CHAN TYPE variable, invert the direction specifier on the channel (server relative) */
			if (TagOf (LHSOf (tptr)) == S_RECORDSUB) {
				treenode *base = ASBaseOf (LHSOf (tptr));

#if 0
fprintf (stderr, "chk2: HERE!\n");
#endif
				if (NTypeAttrOf (chk_gettype_main (base, TRUE)) & TypeAttr_marked_out) {
					cspec ^= (TypeAttr_marked_in | TypeAttr_marked_out);
				}
			}
#endif
			if (cspec & TypeAttr_marked_out) {
				chkreport (CHK_CHANDIR_NOT_INPUT, chklocn);
			}
			if ((TagOf (t) == S_CHAN) || (TagOf (t) == S_PORT)) {
				int nvariants = 0;
				treenode *variantlist = RHSOf (tptr);
				treenode *pptr = ProtocolOf (t);	/* Pointer to channel protocol */
				const BOOL isact = (TagOf (pptr) == S_ANYCHANTYPE);
				const BOOL isapt = (TagOf (pptr) == S_ANYPROCTYPE);
				int (*cmpvarfunc)(const void *, const void *) = isact ? compvariants_anychantype : (isapt ? compvariants_anyproctype : compvariants);

				/*{{{  check the protocol is a tagged protocol */
				if ((TagOf (pptr) != N_TPROTDEF) && !isact && !isapt) {
					chkreport (CHK_BAD_CASE_INPUT_PROTOCOL, chklocn);
				}
				/*}}} */
				/*{{{  check each variant on the variant list */
				while (!EndOfList (variantlist)) {
					checkvariant (pptr, ThisItem (variantlist));
					variantlist = NextItem (variantlist);
					nvariants++;
				}
				/*}}} */
				/*{{{  fold the left-hand side */
				SetLHS (tptr, foldexp (LHSOf (tptr)));
				/*}}} */
				/*{{{  fold the right-hand side */
				walklist ((void *) foldvariant, RHSOf (tptr), NULL);
				/*}}} */
				/*{{{  check all the tags are distinct */
				{	/* we add 1 to this to prevent problems with zero byte memalloc */
					vntnode_t *const varianttablebase = (vntnode_t *) memalloc (sizeof (vntnode_t) * (nvariants + 1));
					vntnode_t *variantptr = varianttablebase;

					/*{{{  walk variant list putting all variant tags in variant table */
					for (variantlist = RHSOf (tptr); !EndOfList (variantlist); variantlist = NextItem (variantlist), variantptr++) {
						treenode *thisvarianttag;
						treenode *thisvariant = ThisItem (variantlist);
						/*{{{  skip leading specifications on thisvariant */
						thisvariant = skipspecifications (thisvariant);
						/*}}} */
						thisvarianttag = ThisItem (VRTaggedListOf (thisvariant));
						if (isact) {
							/* variant tags are really type-hashes for ANY CHAN TYPE */
							treenode *type = chk_gettype_main (thisvarianttag, TRUE);

#if 0
fprintf (stderr, "adding variant.  typehash = 0x%8.8x, type = ", (unsigned int)typehash (type));
printtreenl (stderr, 4, type);
#endif
							variantptr->vnt_tag = (treenode *)(typehash (type));
						} else if (isapt) {
							/* tags are type-hashes for mobile process types */
							treenode *type = chk_gettype_main (thisvarianttag, TRUE);

							variantptr->vnt_tag = (treenode *)(typehash (type));
						} else {
							if (TagOf (thisvarianttag) == S_CONSTEXP) {
								thisvarianttag = CExpOf (thisvarianttag);
							}
							/*variantptr->vnt_decl = thisvariant; */
							variantptr->vnt_tag = thisvarianttag;
							/*printf("Tag value of %s is %ld\n",
							   WNameOf(NNameOf(thisvarianttag)), NTValueOf(thisvarianttag)); */
						}
					}
					/*}}} */
					/*{{{  sort variant table */
					sup_qsort (varianttablebase, nvariants, sizeof (vntnode_t), cmpvarfunc);
					/*}}} */
					/*{{{  check each variant is distinct */
					{
						int i;
						variantptr = varianttablebase;
						for (i = 0; i < (nvariants - 1); i++) {
							if (cmpvarfunc (variantptr, variantptr + 1) == 0) {
								chklocn = LocnOf (tptr);
								if (isact || isapt) {
									chkreport (CHK_MULTIPLE_VTYPES, chklocn);
								} else {
									chkreport_s (CHK_MULTIPLE_VARIANT, chklocn, WNameOf (NNameOf (variantptr->vnt_tag)));
								}
							}
							variantptr++;
						}
					}
					/*}}} */
					/*{{{  warn if there are any gaps */
					if (current_fe_data->fe_warning_case_input) {	/* bug TS/2048 22/01/93 */
						int i = 0;
						treenode *taglist;
						for (taglist = NTypeOf (pptr); !EndOfList (taglist); taglist = NextItem (taglist)) {
							if ((i < nvariants) && (varianttablebase[i].vnt_tag == ThisItem (taglist))) {
								i++;
							} else {
								chklocn = LocnOf (tptr);
								msg_out_s (SEV_WARN, CHK, CHK_MISSING_TAG_IN_CASE_INPUT, chklocn,
									   WNameOf (NNameOf (ThisItem (taglist))));
							}
						}
					}
					/*}}} */

					memfree (varianttablebase);
				}
				/*}}} */
			} else if (TagOf (t) != S_UNDECLARED) {
				chkreport (CHK_NOT_CHANNEL, chklocn);
			}
		}
		break;
		/*}}} */
		/*{{{  case S_INPUT */
	case S_INPUT:
		/* Check that lhs is a channel or a timer
		   Check that the rhs conforms to the channel protocol, or is a time.
		 */
#if 0
fprintf (stderr, "chk2: caction: LHSOf (tptr) =");
printtreenl (stderr, 4, LHSOf (tptr));
#endif
		{
			treenode *const t = typecheck (LHSOf (tptr), unknownnodeptr);
			BIT32 cspec;

#if 0
fprintf (stderr, "chk2: caction: t (typecheck'd LHS) =");
printtreenl (stderr, 4, t);
#endif
			/* anything about direction specifiers? */
			cspec = (TagOf (t) == S_CHAN) ? (TypeAttrOf (t) & (TypeAttr_marked_in | TypeAttr_marked_out)) : 0;
#ifdef MOBILES
			/* if we're the client end of a CHAN TYPE variable, invert the direction specifier on the channel (which is server relative) */
			if (TagOf (LHSOf (tptr)) == S_RECORDSUB) {
				treenode *base = ASBaseOf (LHSOf (tptr));
				treenode *nt = typecheck (base, unknownnodeptr);

#if 0
fprintf (stderr, "caction: LHS of IO looks like a RECORDSUB.. cspec = 0x%x, NTypeAttrOf (base) = 0x%x, base =", cspec, NTypeAttrOf (base));
printtreenl (stderr, 4, base);
fprintf (stderr, "caction:    \"     \"    : ntt (typecheck of base) =");
printtreenl (stderr, 4, nt);
#endif

				if (NTypeAttrOf (nt) & TypeAttr_marked_out) {
					cspec ^= (TypeAttr_marked_in | TypeAttr_marked_out);
				}
			}
#endif
			if (cspec & TypeAttr_marked_out) {
				chkreport (CHK_CHANDIR_NOT_INPUT, chklocn);
			}

			if ((TagOf (t) == S_CHAN) || (TagOf (t) == S_PORT)) {
				/*{{{  protocol check the channel usage, fold the rhs */
				treenode *protocol = ProtocolOf (t);
				/*{{{  check the protocol is not a tagged protocol */
				if (TagOf (protocol) == N_TPROTDEF) {
					chkreport (CHK_BAD_INPUT_PROTOCOL, chklocn);
				}
				/*}}} */
				protocolcheck (protocol, RHSOf (tptr), S_INPUT);
				walklist (checkprotvariable, RHSOf (tptr), NULL);
				SetRHS (tptr, foldexplist (RHSOf (tptr)));
				if (TagOf (t) == S_PORT) {
					check_port (LHSOf (tptr));	/* bug TS/1985 17/02/93 */
				}
				/*}}} */
			} else if (TagOf (t) == S_TIMER) {
				/*{{{  type check the time, fold the time */
				if (listitems (RHSOf (tptr)) > 1) {
					chkreport (CHK_INV_TIMER_INPUT, chklocn);
				} else {
					treenode *timeexp = ThisItem (RHSOf (tptr));
					treenode *timetype = typecheck (timeexp, intnodeptr);
					if (!sametype (TagOf (timetype), S_INT)) {
						chkreport (CHK_TIME_TYPE_MISMATCH, chklocn);
					}
					checkvariable (timeexp, NULL);
					NewItem (foldexp (timeexp), RHSOf (tptr));
				}
				/*}}} */
			} else if (TagOf (t) != S_UNDECLARED) {
				chkreport (CHK_NOT_CHANNEL_OR_TIMER, chklocn);
			}

			/*{{{  fold the left-hand side */
			SetLHS (tptr, foldexp (LHSOf (tptr)));
			/*}}} */

		}
		break;
		/*}}} */
		/*{{{  case S_X_INPUT */
	case S_X_INPUT:
		/* check that lhs is a channel and rhs conforms to the channel protocol
		 * check any nested processes */
		{
			treenode *const t = typecheck (LHSOf (tptr), unknownnodeptr);
			BIT32 cspec;

			/* anything about direction specifiers? */
			cspec = (TagOf (t) == S_CHAN) ? (TypeAttrOf (t) & (TypeAttr_marked_in | TypeAttr_marked_out)) : 0;
#ifdef MOBILES
			/* if we're the CLIENT end of a CHAN TYPE variable, invert the direction specifier on the channel (server relative) */
			if (TagOf (LHSOf (tptr)) == S_RECORDSUB) {
				treenode *base = ASBaseOf (LHSOf (tptr));

#if 0
fprintf (stderr, "chk2: HERE!\n");
#endif
				if (NTypeAttrOf (chk_gettype_main (base, TRUE)) & TypeAttr_marked_out) {
					cspec ^= (TypeAttr_marked_in | TypeAttr_marked_out);
				}
			}
#endif
			if (cspec & TypeAttr_marked_out) {
				chkreport (CHK_CHANDIR_NOT_INPUT, chklocn);
			}

#if 0
fprintf (stderr, "caction: checking extended input:");
printtreenl (stderr, 4, tptr);
#endif
			if (TagOf (t) == S_CHAN) {
				treenode *protocol = ProtocolOf (t);

				/*{{{  check not tagged or variant protocol*/
				if (TagOf (protocol) == N_TPROTDEF) {
					chkreport (CHK_BAD_INPUT_PROTOCOL, chklocn);
				}
				/*}}}*/
				protocolcheck (protocol, RHSOf (tptr), S_X_INPUT);
				walklist (checkprotvariable, RHSOf (tptr), NULL);
				SetRHS (tptr, foldexplist (RHSOf (tptr)));

			} else if (TagOf (t) != S_UNDECLARED) {
				chkreport (CHK_NOT_CHANNEL, chklocn);
			}
			/* fold LHS */
			SetLHS (tptr, foldexp (LHSOf (tptr)));
		}
		break;
		/*}}}  */
		/*{{{  case S_DELAYED_INPUT */
	case S_DELAYED_INPUT:
		/* Check that lhs is a timer
		   Check that there is only one rhs and it is a time.
		 */
		{
			treenode *const t = typecheck (LHSOf (tptr), unknownnodeptr);

			if (TagOf (t) == S_TIMER)
				/*{{{  type check the time */
			{
				treenode *timetype = typecheck (RHSOf (tptr), intnodeptr);
				if (!sametype (TagOf (timetype), S_INT))
					chkreport (CHK_TIME_TYPE_MISMATCH, chklocn);
				/*{{{  fold the left and right-hand sides */
				SetLHS (tptr, foldexp (LHSOf (tptr)));
				SetRHS (tptr, foldexp (RHSOf (tptr)));
				/*}}} */
			}
			/*}}} */
			else if (TagOf (t) != S_UNDECLARED)
				chkreport (CHK_NOT_TIMER, chklocn);

		}
		break;
		/*}}} */
	default:
		badtag (chklocn, TagOf (tptr), "caction");
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));

	DEBUG_MSG (("Checked action node ok near line %d\n", (int)LocnOf (tptr)));
	return (tptr);
}

/*}}}*/

/*{{{  PRIVATE BOOL illegal_retype(int type)*/
PRIVATE BOOL illegal_retype (const tagtype_t type)
/* returns TRUE if the type is not permitted in a RETYPE */
{
	return ((type == S_TIMER) || network_datatype (type));
}

/*}}}*/
/*{{{  PRIVATE void check_for_chanofany*/
#if 0
PRIVATE void check_for_chanofany (treenode * const nptr)
{
	if (current_fe_data->fe_warning_chanofany) {
		const treenode *const type = basetype_tree (NTypeOf (nptr));
		if ((TagOf (type) == S_CHAN) && (TagOf (ProtocolOf (type)) == S_ANY)) {
			/* we pass 'NOPOSN' to the memo function so that it doesn't just
			   restrict the 'memo' to this one line of source code */
			if (fe_memo_err (current_fe_handle, CHK, CHK_CHAN_OF_ANY_USED, NOPOSN))
				msg_out (SEV_WARN, CHK, CHK_CHAN_OF_ANY_USED, LocnOf (nptr));
		}
	}
}
#endif
/*}}}*/
/*{{{  PRIVATE void chk_invalid_val*/
PRIVATE void chk_invalid_val (void)
{
	if ((current_fe_data->fe_lang & FE_LANG_OCCAM) != 0) {
		chkreport (CHK_INV_VAL_OCCAM, chklocn);
	} else if ((current_fe_data->fe_lang & FE_LANG_NDL) != 0) {
		chkreport (CHK_INV_VAL_NDL, chklocn);
	} else {
		chkreport (CHK_INV_VAL_CONFIG, chklocn);
	}
}
/*}}}*/
/*{{{  PRIVATE void chk_invalid_result (void)*/
PRIVATE void chk_invalid_result (void)
{
	if ((current_fe_data->fe_lang & FE_LANG_OCCAM) != 0) {
		chkreport (CHK_INV_RESULT_OCCAM, chklocn);
	} else if ((current_fe_data->fe_lang & FE_LANG_NDL) != 0) {
		chkreport (CHK_INV_RESULT_NDL, chklocn);
	} else {
		chkreport (CHK_INV_RESULT_CONFIG, chklocn);
	}
}
/*}}}  */
/*{{{  PRIVATE void check_type_is_data_type*/
#ifdef OCCAM2_5
PRIVATE void check_type_is_data_type (treenode * type, const SOURCEPOSN locn, const BOOL datatype_wanted)
/* Checks that a type tree is actually a data type */
{
	while (TRUE) {
		switch (TagOf (type)) {
		case S_BYTE:
		case S_BOOL:
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
		case N_DECL:	/* undeclared user type */
			if (!datatype_wanted) {
				chkerr (CHK_BAD_USERTYPE, locn);
			}
			return;
		case S_CHAN:
			if (datatype_wanted) {
				chkerr (CHK_BAD_USERTYPE, locn);
			}
			/* better have a direction specifier! */
			if (!(TypeAttrOf (type) & (TypeAttr_marked_in | TypeAttr_marked_out))) {
				chkerr (CHK_NO_CHANDIR_SPEC_ANON, locn);
			}
			return;
		case N_TYPEDECL:
			check_type_is_data_type (NTypeOf (type), locn, datatype_wanted);
			return;
		case S_ARRAY:
			type = ARTypeOf (type);
			break;
		case S_RECORD:	/* record type */
			if (datatype_wanted != ((TypeAttrOf (type) & TypeAttr_datatype) != 0))
				chkerr (CHK_BAD_USERTYPE, locn);
			else
				for (type = ARTypeOf (type); type != NULL; type = DBodyOf (type))
					check_type_is_data_type (NTypeOf (DNameOf (type)), LocnOf (type), datatype_wanted);
			return;
		#ifdef MOBILES
		case S_MOBILE:
			type = MTypeOf (type);
			break;
		#endif
		default:
			chkerr (CHK_BAD_USERTYPE, locn);
			return;
		}
	}
}
#endif
/*}}}*/
/*{{{  PRIVATEPARAM int ctrace (treenode **tptr, void *voidptr)*/
/*
 *	checks a single trace component.  ensures that the events in the trace correspond
 *	to things actually in the channel-type
 */
PRIVATEPARAM int ctrace (treenode **tptr, void *voidptr)
{
	/* treenode *typedecl = (treenode *)voidptr; */
	treenode *tt = *tptr;

#if 0
fprintf (stderr, "ctrace: *tptr = ");
printtreenl (stderr, 4, *tptr);
#endif
	while (tt) {
		switch (TagOf (tt)) {
		case S_ASINPUT:
		case S_ASOUTPUT:
			{
				treenode *op = OpOf (tt);

				/* op should be an N_FIELD -- with checked direction-specifiers */
				if ((TagOf (op) != N_FIELD) || (TagOf (NTypeOf (op)) != S_CHAN)) {
					chkerr (CHK_BAD_TRACE_ELEMENT, LocnOf (tt));
					return STOP_WALK;
				}
				if ((TypeAttrOf (NTypeOf (op)) & (TypeAttr_marked_in | TypeAttr_marked_out)) ^
						((TagOf (tt) == S_ASINPUT) ? TypeAttr_marked_in : TypeAttr_marked_out)) {

					chkerr (CHK_BAD_TRACE_DIRECTION, LocnOf (tt));
					return STOP_WALK;
				}
				*tptr = op;
			}
			break;
		case N_FIELD:
			return CONTINUE_WALK;
		default:
			return CONTINUE_WALK;
		}

		tt = *tptr;
	}
	return CONTINUE_WALK;
}
/*}}}*/
/*{{{  PRIVATE void ctraces (treenode *const traces, treenode *const typedecl)*/
/*
 *	checks traces for mobile channel-types (just iterates)
 */
PRIVATE void ctraces (treenode *const traces, treenode *const typedecl)
{
	treenode *walk;

#if 0
fprintf (stderr, "ctraces: typedecl = ");
printtreenl (stderr, 4, typedecl);
#endif
	for (walk=traces; !EndOfList (walk); walk = NextItem (walk)) {
		modprewalktree (ThisItemAddr (walk), ctrace, (void *)typedecl);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATEPARAM int compfields*/
#ifdef OCCAM2_5
PRIVATEPARAM int compfields (const void *const p1, const void *const p2)
/*{{{  comment*/
/* Prioritise the fields of a record according to size, so that packing
   and/or accessing is more efficient.

   The basic heuristic is as follows:
   a) sort by size of field
   b) when equal, sort by scope
   This produces 'optimal' packing efficiency.
*/
/*}}}*/
{
	const fldnode_t *const v1 = p1;
	const fldnode_t *const v2 = p2;
	const INT32 v1_bytes = v1->fld_bytes;
	const INT32 v2_bytes = v2->fld_bytes;

	if (v1_bytes > v2_bytes)
		return 1;
	if (v1_bytes < v2_bytes)
		return -1;

	/* both are the same size - sort by scope */
	if (NScopeOf (v1->fld_nptr) > NScopeOf (v2->fld_nptr))
		return 1;
	if (NScopeOf (v1->fld_nptr) < NScopeOf (v2->fld_nptr))
		return -1;

	return 0;
}
#endif
/*}}}*/
/*{{{  PRIVATE void crecordtype (treenode *const tptr, treenode *const rhs_type, const BOOL ismobile, treenode *const traces)*/
#ifdef OCCAM2_5
PRIVATE void crecordtype (treenode *const tptr, treenode *const rhs_type, const BOOL ismobile, treenode *const traces)
{
	treenode *decl;
	int ndecls = 0;
	int ischantype = 0;

#if 0
fprintf (stderr, "crecord_type: rhs_type =");
printtreenl (stderr, 4, rhs_type);
#endif
	/*{{{  point all the sub-fields back at their master record */
	for (decl = ARTypeOf (rhs_type); decl != NULL; decl = DBodyOf (decl)) {
		SetNDecl (DNameOf (decl), tptr);
		SetNScope (DNameOf (decl), ndecls);
		ndecls++;
	}
	/*}}} */
	/*{{{  check for duplicate names */
	for (decl = ARTypeOf (rhs_type); decl != NULL; decl = DBodyOf (decl)) {
		wordnode *const this_name = NNameOf (DNameOf (decl));
		treenode *rest;
		for (rest = DBodyOf (decl); rest != NULL; rest = DBodyOf (rest)) {
			if (NNameOf (DNameOf (rest)) == this_name) {
				chkerr_s (CHK_DUPLICATE_FIELD_DECL, LocnOf (rest), WNameOf (this_name));
			}
		}
	}
	/*}}} */
#ifdef MOBILES
	/*{{{  if ismobile, check for presence of MOBILE sub-fields, ban if any.*/
	if (ismobile) {
		for (decl = ARTypeOf (rhs_type); decl != NULL; decl = DBodyOf (decl)) {
			treenode *dname = DNameOf (decl);

			if (TagOf (dname) == N_FIELD) {
				/* FIXME: allowing dynamic-mobile fields is probably OK.. */
				if (TagOf (NTypeOf (dname)) == S_MOBILE) {
					treenode *basetype = MTypeOf (NTypeOf (dname));		/* (MOBILE) <something> */

					/* if it's an unsized array, we'll allow it (tentatively) */
					if ((TagOf (basetype) == S_ARRAY) && (ARDimLengthOf (basetype) == NULL) && (ARDimOf (basetype) == -1)) {
						/* SKIP */
#if 0
fprintf (stderr, "crecordtype: found mobile sub-type..\n");
#endif
					} else {
						chkerr (CHK_INNER_MOBILE, LocnOf (decl));
					}
				} else if (TagOf (follow_user_type (NTypeOf (dname))) == S_MOBILE) {
					chkerr (CHK_INNER_MOBILE, LocnOf (decl));
				} else if (TagOf (NTypeOf (dname)) == S_CHAN) {
					ischantype = 1;
					/* check the channel-type */
#if 0
fprintf (stderr, "crecordtype(): channel-type, channel: NTypeOf (dname) =");
printtreenl (stderr, 4, NTypeOf (dname));
#endif
					cspecifier (ProtocolOf (NTypeOf (dname)));
					/* ctype (ProtocolOf (NTypeOf (dname))); */
				}
#if 0
fprintf (stderr, "crecordtype: found N_FIELD. dname =");
printtreenl (stderr, 4, dname);
#endif
			}
		}
		if (traces && !ischantype) {
			chkerr (CHK_TRACES_NOT_RECORD, LocnOf (traces));
		} else if (traces) {
			ctraces (traces, tptr);
#if 0
fprintf (stderr, "crecordtype: traces = ");
printtreenl (stderr, 4, traces);
#endif
		}
	}
	/*}}}*/
#endif
	/*{{{  insert size information for the record */
	{
		fldnode_t *const list = memalloc (sizeof (fldnode_t) * ndecls + 1);
		int i;
		const BOOL packed = (TypeAttrOf (rhs_type) & TypeAttr_packed) != 0;
		BOOL wordlen_sensitive = FALSE;
		/*{{{  copy field names into array for sorting */
		for (i = 0, decl = ARTypeOf (rhs_type); decl != NULL; decl = DBodyOf (decl), i++) {
			treenode *const field_nptr = DNameOf (decl);
			treenode *const field_type = NTypeOf (field_nptr);
			treenode *const field_base = basetype_tree (field_type);

			list[i].fld_nptr = field_nptr;

			if ((TagOf (field_type) == N_DECL) || (TagOf (field_type) == S_UNDECLARED)) {
				/* should have been resolved, if still name or undeclared, broken */
				list[i].fld_bytes = 1;
				list[i].fld_element = 1;
				continue;
			}

			list[i].fld_bytes = (int) bytesin (field_type);
			list[i].fld_element = (int) bytesin (field_base);

#ifdef MOBILES
			if ((TagOf (field_type) == S_MOBILE) && (list[i].fld_bytes == -1)) {
				/* dynamic mobile array (only permitted here) FIXME though later */
				if ((TagOf (MTypeOf (field_type)) == S_ARRAY) && (ARDimOf (MTypeOf (field_type)) == -1)) {
					list[i].fld_bytes = (current_fe_data->fe_txlib->bpw) * (1 + dynmobiledimensioncount (field_nptr));	/* one for pointer, one for each dimension-size */
				} else {
					/* should be caught earlier.. */
					chkerr (CHK_INNER_MOBILE, LocnOf (decl));
				}
			}
#if 0
fprintf (stderr, "crecordtype: list[%d].fld_bytes = %d, list[%d].fld_element = %d, list[%d].fld_nptr =", i, list[i].fld_bytes, i, list[i].fld_element, i);
printtreenl (stderr, 4, list[i].fld_nptr);
#endif
#endif
			if ((TagOf (field_base) == S_INT) ||
					((TagOf (field_base) == S_RECORD) && ((TypeAttrOf (field_base) & TypeAttr_wordlen) != 0))) {
				wordlen_sensitive = TRUE;
			}
		}
		/*}}} */
		/*{{{  if not PACKED, sort by size */
		if (!packed)
			sup_qsort (list, ndecls, sizeof (fldnode_t), compfields);
		/*}}} */
		/*{{{  allocate offsets and calculate total size */
		{
			const int bpw = current_fe_data->fe_txlib->bpw;
			int offset = 0;
			int padding;
			for (i = 0; i < ndecls; i++) {
				/*{{{  set/check offset of each field */
				treenode *const nptr = list[i].fld_nptr;
				const int element_size = list[i].fld_element;
				const int alignment = (element_size < bpw) ? element_size : (needs_quadalign & (element_size > bpw)) ? 2 * bpw : bpw;	/* MDP 28/11/95 */
				/* const int alignment    = (element_size < bpw) ? element_size : bpw; */
				if (current_fe_data->fe_align_fields) {

					/*padding = ((offset + (alignment - 1)) & (-alignment)) - offset; */
					/* other version, not relying on twos-complement */
					padding = (((offset + (alignment - 1)) / alignment) * alignment) - offset;
				} else {
					padding = 0;
				}

				if (packed && (padding != 0)) {
					/*{{{  alignment error */
					char number_s[50];
					sprintf (number_s, "%d", alignment);
					chkerr_ss (CHK_UNALIGNED_PACKED_FIELD, LocnOf (nptr), WNameOf (NNameOf (nptr)), number_s);
					/*}}} */
				} else {
					offset += padding;
				}

				/*{{{  set offset */
				SetNVOffset (nptr, offset);
				/*}}} */
				/*{{{  wordlen sensitivity */
				/* For the configurer:
				   we start off configuring thinking that we're on a 32-bit machine.
				   If we need to add more than one byte of padding, then this padding
				   would have been different on a 16-bit machine, so we have a
				   problem with mixed wordlength communication of this record type.
				 */
				if (padding > 1) {
					wordlen_sensitive = TRUE;
				}
				/*}}} */

				/*{{{  debugging */
				/*
				   printf("field: %10s, size:%3ld, elt:%3ld, align:%3ld, pad:%3ld, offset:%3ld\n",
				   WNameOf(NNameOf(list[i].fld_nptr)),
				   list[i].fld_bytes, element_size, alignment, padding, offset);
				 */
				/*}}} */
				offset += list[i].fld_bytes;
				/*}}} */
			}
			/*{{{  set/check size of whole record */
			if (current_fe_data->fe_align_records) {
				/*padding = ((offset + (bpw - 1)) & (-bpw)) - offset; */
				/* other version, not relying on twos-complement */
				padding = (((offset + (bpw - 1)) / bpw) * bpw) - offset;
			} else {
				padding = 0;
			}

			if (packed && (padding != 0)) {
				/*{{{  alignment error */
				chkerr_s (CHK_UNALIGNED_PACKED_RECORD, LocnOf (tptr), WNameOf (NNameOf (DNameOf (tptr))));
				/*}}} */
			}

			/*{{{  wordlen sensitivity */
			/* For the configurer:
			   we start off configuring thinking that we're on a 32-bit machine.
			   If we need to add more than one byte of padding, then this padding
			   would have been different on a 16-bit machine, so we have a
			   problem with mixed wordlength communication of this record type.
			 */
			if (padding > 1) {
				wordlen_sensitive = TRUE;
			}
			/*}}} */

			offset += padding;
			SetARDim (rhs_type, offset);
			/*printf("Record: size is %d\n", offset); */
			/*}}} */
		}
		/*}}} */
		/*{{{  Configurer - check wordlength sensitivity */
		if (wordlen_sensitive) {
			SetTypeAttr (rhs_type, TypeAttrOf (rhs_type) | TypeAttr_wordlen);
		}
		/*}}} */
		memfree (list);
	}
	/*}}} */
}
#endif
/*}}}*/
/*{{{  PRIVATE BOOL csameprot (treenode *p1, treenode *p2)*/
PRIVATE BOOL csameprot (treenode *p1, treenode *p2)
{
	if (TagOf (p1) != TagOf (p2)) {
		return FALSE;
	}
	switch (TagOf (p1)) {
	case N_TYPEDECL:
		/* same protocol may have different type declaration nodes,
		 * always the same underneath though
		 */
		if (p1 == p2) {
			return TRUE;
		}
		if (NTypeOf (p1) == NTypeOf (p2)) {
			if ((NTypeAttrOf (p1) && NTypeAttrOf (p2)) && (NTypeAttrOf (p1) ^ NTypeAttrOf (p2))) {
				/* incompatible channel-directions */
				return FALSE;
			}
			/* otherwise good :) */
			return TRUE;
		}
		break;
	case S_COLON2:
		/* check left and right */
		if (!csameprot (LeftOpOf (p1), LeftOpOf (p2))) {
			return FALSE;
		}
		if (!csameprot (RightOpOf (p1), RightOpOf (p2))) {
			return FALSE;
		}
		/* otherwise good */
		return TRUE;
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
	case S_BYTE:
	case S_BOOL:
		return TRUE;
	default:
		badtag (chklocn, TagOf (p1), "csameprot");
	}
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE BOOL csametprot (treenode *p1, treenode *p2)*/
/*
 *  checks that the two sequential protocols, given as
 *  their N_TAGDEFs, are the same.
 */
PRIVATE BOOL csametprot (treenode *p1, treenode *p2)
{
	if ((TagOf (p1) != N_TAGDEF) || (TagOf (p2) != N_TAGDEF)) {
		badtag (chklocn, TagOf (p1), "csametprot");
		return FALSE;
	}
	p1 = NTypeOf (p1);
	p2 = NTypeOf (p2);
	while (!EndOfList (p1) && !EndOfList (p2)) {
		treenode *r1 = ThisItem (p1);
		treenode *r2 = ThisItem (p2);
		BIT32 attr1 = 0;
		BIT32 attr2 = 0;

		if ((TagOf (p1) != S_LIST) || (TagOf (p2) != S_LIST)) {
			badtag (chklocn, TagOf (r1), "csametprot");
			return FALSE;
		}

		/* step over any INPUT/OUTPUT nodes */
		if ((TagOf (r1) == S_ASINPUT) || (TagOf (r1) == S_ASOUTPUT)) {
			attr1 = ((TagOf (r1) == S_ASINPUT) ? TypeAttr_marked_in : TypeAttr_marked_out);
			r1 = OpOf (r1);
		}
		if ((TagOf (r2) == S_ASINPUT) || (TagOf (r2) == S_ASOUTPUT)) {
			attr2 = ((TagOf (r2) == S_ASINPUT) ? TypeAttr_marked_in : TypeAttr_marked_out);
			r2 = OpOf (r2);
		}
		/* check them */
		if ((attr1 && attr2) && (attr1 ^ attr2)) {
			/* differing directions for something */
			return FALSE;
		}
		if (!csameprot (r1, r2)) {
			return FALSE;
		}

		p1 = NextItem (p1);
		p2 = NextItem (p2);
	}
	if (!EndOfList (p1) || !EndOfList (p2)) {
		return FALSE;
	}
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE int pi_merge (treenode *thistag, treenode *othertags, treenode ***leftovers)*/
/*
 *  performs a single protocol-inheritance merge.
 *  merges "othertags" into "thistag".  This doesn't assign values, rather
 *  it sets "leftovers" and returns the high-tag value.
 */
PRIVATE int pi_merge (treenode *thistag, treenode *othertags, treenode ***leftovers)
{
	int hval;
	treenode *other, *t;
	treenode **ipoint;

	hval = 0;
	ipoint = *leftovers;

	if (TagOf (NTypeOf (othertags)) != S_LIST) {
		/* knackered type -- presumably already generated an error */
		return 0;
	}
	/* find the highest tag value in "othertags" */
	for (t = NTypeOf (othertags); !EndOfList (t); t = NextItem (t)) {
		treenode *tag = ThisItem (t);

		if (NTValueOf (tag) > hval) {
			hval = NTValueOf (tag);
		}
	}
	/* find any (potentially) higher value in "thistag", up to "*ipoint" */
	for (t = NTypeOf (thistag); !EndOfList (t) && (t != *ipoint); t = NextItem (t)) {
		treenode *tag = ThisItem (t);

		if (NTValueOf (tag) > hval) {
			hval = NTValueOf (tag);
		}
	}

#if 0
fprintf (stderr, "chk2: pi_merge: NTypeOf (thistag) = ");
printtreenl (stderr, 4, NTypeOf (thistag));
fprintf (stderr, "chk2: pi_merge: NTypeOf (othertags) = ");
printtreenl (stderr, 4, NTypeOf (othertags));
fprintf (stderr, "chk2: pi_merge: *ipoint = ");
printtreenl (stderr, 4, *ipoint);
#endif
	/* iterate over othertags, checking to see if they're in the extended protocol */
	for (other = NTypeOf (othertags); !EndOfList (other); other = NextItem (other)) {
		treenode *prev_t, *next_t;
		BOOL past_ipoint = FALSE;

		/* check if in extended protocol */
		prev_t = NULL;
		for (t = NTypeOf (thistag), next_t = NULL; !EndOfList (t); prev_t = t, t = next_t) {
			next_t = NextItem (t);

			if (*ipoint && (ThisItem (t) == ThisItem(*ipoint))) {
				/* this ensures we don't change tag values already set (because they came from somewhere else..) */
				past_ipoint = TRUE;
			}
			if (NNameOf (ThisItem (t)) != NNameOf (ThisItem (other))) {
				/* check that if values are fixed, they're not the same */
				if ((NTypeAttrOf (ThisItem (t)) & TypeAttr_fixed) && (NTypeAttrOf (ThisItem (other)) & TypeAttr_fixed)) {
					if (NTValueOf (ThisItem (t)) == NTValueOf (ThisItem (other))) {
						chkreport_i (CHK_TAG_VALUE_EXISTS, LocnOf (ThisItem (t)), NTValueOf (ThisItem (t)));
					}
				}
				continue;
			} else {
				/* check that the same name has at least the same tag values, if specified for both */
				if ((NTypeAttrOf (ThisItem (t)) & TypeAttr_fixed) && (NTypeAttrOf (ThisItem (other)) & TypeAttr_fixed)) {
					if (NTValueOf (ThisItem (t)) != NTValueOf (ThisItem (other))) {
						chkreport_s (CHK_EXTENDS_ERROR, LocnOf (ThisItem (t)), "different tag values specified");
					}
				}
			}
#if 0
fprintf (stderr, "chk2: pi_merge: checking, ThisItem (*ipoint) = %p, ThisItem(t=%p)=%p: ", ThisItem (*ipoint), t, ThisItem(t));
printtreenl (stderr, 4, ThisItem(t));
fprintf (stderr, "chk2: pi_merge: checking, past_ipoint = %d, ThisItem(other=%p)=%p): ", past_ipoint, other, ThisItem(other));
printtreenl (stderr, 4, ThisItem(other));
fprintf (stderr, "chk2: pi_merge: checking, prev_t = %p, next_t=%p: ", prev_t, next_t);
printtreenl (stderr, 4, next_t);
#endif
			/* name match, check sequential protocol is good */
			if (!csametprot (ThisItem (t), ThisItem (other))) {
				chkreport_ss (CHK_EXTENDS_BADTAGMATCH, chklocn, WNameOf (NNameOf (othertags)), WNameOf (NNameOf (ThisItem (other))));
			}
			/* else remove from position and re-insert, setting to the other tag value */
			if (!past_ipoint) {
				/* just check they're the same */
				if (NTValueOf (ThisItem (t)) != NTValueOf (ThisItem (other))) {
					chkreport_s (CHK_EXTENDS_ERROR, chklocn, "impossible 1");
				}
			} else {
				/* re-arrange to put t at the insert-point */
				if (!prev_t) {
					/* first tag in the extended list, where ipoint must be */
					if (t != *ipoint) {
						chkreport_s (CHK_EXTENDS_ERROR, chklocn, "impossible 2");
					}
				} else {
					SetRight (prev_t, next_t);
					SetRight (t, *ipoint);
					SetNTValue (ThisItem (t), NTValueOf (ThisItem (other)));
					*ipoint = t;
				}
				ipoint = NextItemAddr (*ipoint);
			}
			break;
		}
#if 0
fprintf (stderr, "chk2: pi_merge: after check, NTypeOf (thistag) = ");
printtreenl (stderr, 4, NTypeOf (thistag));
fprintf (stderr, "chk2: pi_merge: after check, NTypeOf (othertags) = ");
printtreenl (stderr, 4, NTypeOf (othertags));
fprintf (stderr, "chk2: pi_merge: after check, *ipoint = ");
printtreenl (stderr, 4, *ipoint);
#endif
		if (EndOfList (t)) {
			/* this element doesn't exist in the other -- add it */
			*ipoint = newlistnode (S_LIST, NOPOSN, copytree (ThisItem (other), syn_lexlevel), *ipoint);
			ipoint = NextItemAddr (*ipoint);
		}
	}
	*leftovers = ipoint;
#if 0
fprintf (stderr, "chk2: pi_merge: after merge, NTypeOf (thistag) = ");
printtreenl (stderr, 4, NTypeOf (thistag));
fprintf (stderr, "chk2: pi_merge: after merge, NTypeOf (othertags) = ");
printtreenl (stderr, 4, NTypeOf (othertags));
fprintf (stderr, "chk2: pi_merge: after merge, *ipoint = ");
printtreenl (stderr, 4, *ipoint);
#endif
	return hval;
}
/*}}}*/
/*{{{  PRIVATE BOOL pi_valcheck (treenode *start, treenode *stop, treenode *other)*/
PRIVATE BOOL pi_valcheck (treenode *start, treenode *stop, treenode *other)
{
#if 0
fprintf (stderr, "chk2: pi_valcheck: start = ");
printtreenl (stderr, 4, start);
fprintf (stderr, "chk2: pi_valcheck: stop = ");
printtreenl (stderr, 4, stop);
fprintf (stderr, "chk2: pi_valcheck: NTypeOf (other) = ");
printtreenl (stderr, 4, NTypeOf (other));
#endif

	if (TagOf (other) != N_TPROTDEF) {
		badtag (chklocn, TagOf (other), "pi_valcheck");
	}
	if (start == stop) {
		return TRUE;
	} else {
		treenode *t, *s;

		for (s = NTypeOf (other); !EndOfList (s); s = NextItem (s)) {
			int val = NTValueOf (ThisItem (s));

			for (t = start; !EndOfList (t) && (t != stop); t = NextItem (t)) {
				if (NTValueOf (ThisItem (t)) == val) {
#if 0
fprintf (stderr, "chk2: pi_valcheck: collision, name comparison is %d\n", (NNameOf (ThisItem (t)) == NNameOf (ThisItem (s))));
#endif
					if (NNameOf (ThisItem (t)) != NNameOf (ThisItem (s))) {
						return FALSE;
					}
					break;	/* from inner loop */
				}
			}
		}
	}
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE int pi_assign (treenode *fromlist, int hval)*/
PRIVATE int pi_assign (treenode *fromlist, int hval)
{
	treenode *t;

	/* fill in the blanks */
	for (t = fromlist; !EndOfList (t); t = NextItem (t)) {
		treenode *tag = ThisItem (t);

		hval++;
		if (hval >= MAX_TAGS) {
			chkreport_s (CHK_EXTENDS_ERROR, chklocn, "too many tags!");
		}
		SetNTValue (tag, hval);
	}
	return hval;
}
/*}}}*/
/*{{{  PRIVATE int pi_reassign (treenode *fromlist, int hval, treenode *start, treenode *stop)*/
PRIVATE int pi_reassign (treenode *fromlist, int hval, treenode *start, treenode *stop)
{
	treenode *t;

	/* fill in the blanks */
	for (t = fromlist; !EndOfList (t); t = NextItem (t)) {
		treenode *tag = ThisItem (t);
		treenode *s;

		for (s=start; !EndOfList(s) && (s != stop); s = NextItem (s)) {
			if (NNameOf (tag) == NNameOf (ThisItem (s))) {
				/* local match, break */
				break;
			}
		}
		if (EndOfList (s) || (s == stop)) {
			hval++;
			if (hval >= MAX_TAGS) {
				chkreport_s (CHK_EXTENDS_ERROR, chklocn, "too many tags!");
			}
			SetNTValue (tag, hval);
		} else {
			SetNTValue (tag, NTValueOf (ThisItem (s)));
		}
	}
	return hval;
}
/*}}}*/
/*{{{  PRIVATE int pi_reassign2 (treenode *fromlist, int hval, treenode *start, treenode *stop, treenode *anclist)*/
PRIVATE int pi_reassign2 (treenode *fromlist, int hval, treenode *start, treenode *stop, treenode *anclist)
{
	treenode *t;

	/* this could be done more efficiently by TypeAttr_fixing the various N_TAGDEF nodes, ... */

	/* fill in the blanks */
	for (t = fromlist; !EndOfList (t); t = NextItem (t)) {
		treenode *tag = ThisItem (t);
		treenode *s, *rl;
		int fixed_val = 0;

		for (s=start; !EndOfList(s) && (s != stop); s = NextItem (s)) {
			if (NNameOf (tag) == NNameOf (ThisItem (s))) {
				/* local match, break */
				break;
			}
		}
		for (rl = anclist; !EndOfList (rl); rl = NextItem (rl)) {
			treenode *il;

			for (il = NTypeOf (ThisItem (rl)); !EndOfList (il); il = NextItem (il)) {
				treenode *atag = ThisItem (il);

				if (NNameOf (atag) == NNameOf (tag)) {
					/* ancestor, so can't do anything about this one */
					fixed_val = 1;
					goto next;
				}
			}
		}
next:
		/* but, if s is in an ancestor too, can't reassign the value (fixed) */
		if (EndOfList (s) || (s == stop)) {
			/* regular reassign */
			if (fixed_val) {
				/* this can't possibly be the case..! */
				chkreport_s (CHK_EXTENDS_ERROR, chklocn, "pi_reassign2: impossible!");
			}
			hval++;
			if (hval >= MAX_TAGS) {
				chkreport_s (CHK_EXTENDS_ERROR, chklocn, "too many tags!");
			}
			SetNTValue (tag, hval);
		} else {
			/* reassign to value in start-end */
			if (fixed_val && (NTValueOf (tag) != NTValueOf (ThisItem (s)))) {
				/* can't satify this, :( */
				chkreport_s (CHK_EXTENDS_ERROR, chklocn, "unsatisfiable constraints in protocol inheritance");
			} else if (!fixed_val) {
				SetNTValue (tag, NTValueOf (ThisItem (s)));
			}
		}
	}
	return hval;
}
/*}}}*/


/*{{{  PRIVATE void cprotocolinherit (treenode *thistag, *ancestors)*/
/*
 *  this implements protocol inheritance.  Syntax is something like:
 *    PROTOCOL A EXTENDS B, C, D
 *      CASE
 *        ...
 *    :
 *
 *  "thistag" is the NAMENODE of "A", "ancestors" is a list of
 *  NAMENODES for "B", etc.
 *
 *  first off, we try to inherit by simply allocating different
 *  tag values for the new name.  This works for single-inheritance
 *  nicely.
 */
PRIVATE void cprotocolinherit (treenode *thistag, treenode **ancestors)
{
	/* iterate over ancestors */
	if (!*ancestors || (TagOf (*ancestors) != S_LIST) || !ThisItem (*ancestors)) {
		chkreport (CHK_BAD_EXTENDS, chklocn);
	} else {
		treenode *t;

		/*{{{  check that ancestors are really tagged protocols*/
		for (t = *ancestors; !EndOfList (t); t = NextItem (t)) {
			treenode *s = ThisItem (t);

#if 0
fprintf (stderr, "cprotocolinherit(): checking ancestor:");
printtreenl (stderr, 4, s);
#endif
			switch (TagOf (s)) {
			case N_TPROTDEF:
				break;
			default:
				chkreport (CHK_BAD_EXTENDS, chklocn);
				return;
			}	
		}
		/*}}}*/
		/*{{{  remove common ancestors*/
		for (t = *ancestors; !EndOfList (t); t = NextItem (t)) {
			treenode *s, *prev_s, *next_s;

			for (s = *ancestors, prev_s = NULL; !EndOfList (s); prev_s = s, s = next_s) {
				next_s = NextItem (s);

				if ((s != t) && protocolinherits (ThisItem (t), ThisItem (s))) {
					/* s is a common ancestor, remove it */
					if (!prev_s) {
						/* removing head of list */
						*ancestors = next_s;
					} else {
						/* removing something in the middle */
						SetRight (prev_s, next_s);
					}
				}
			}
		}
		/*}}}*/
		/*{{{  merge ancestors into this protocol*/
		if (!NextItem (*ancestors)) {
			treenode **leftovers;
			int hval;

			/* single-ancestor -- easy :) */
			leftovers = NTypeAddr (thistag);
			hval = pi_merge (thistag, ThisItem (*ancestors), &leftovers);
			if (*leftovers) {
				hval = pi_assign (*leftovers, hval);
			}
			SetNTypeAttr (ThisItem (*ancestors), NTypeAttrOf (ThisItem (*ancestors)) | TypeAttr_fixed);
		} else {
			/* um, more complicated..! */
			/* split ancestors into three sub-lists:
			 *   - those that we definitely can't adjust (separate file, for example)
			 *   - those that we'd rather not modify (fixed)
			 *   - those that we can easily modify (non-fixed)
			 */
			treenode *hard_fixed = NULL;
			treenode *soft_fixed = NULL;
			treenode *bit_fixed = NULL;
			treenode *unfixed = NULL;
			treenode **leftovers;
			int hval = 0;

			for (t = *ancestors; !EndOfList (t); t = NextItem (t)) {
				treenode *anc = ThisItem (t);

#if 0
fprintf (stderr, "chk2: cprotocolinherit: anc = ");
printtreenl (stderr, 4, anc);
#endif
				if (TagOf (NTypeOf (anc)) == S_UNDECLARED) {
					/* undeclared, fail/next */
					continue;
				}
				if (FileNumOf (LocnOf (NDeclOf (anc))) != FileNumOf (LocnOf (NDeclOf (thistag)))) {
					/* ancestor defined in a different file, definitely can't change */
					hard_fixed = newlistnode (S_LIST, NOPOSN, anc, hard_fixed);
				} else if (NTypeAttrOf (anc) & TypeAttr_fixed) {
					/* not going to allow these at the moment, but possibly could in the future. */
					soft_fixed = newlistnode (S_LIST, NOPOSN, anc, soft_fixed);
				} else if (DExtraOf (NDeclOf (anc))) {
					/* things that aren't fixed, but which are decended from something else -- allow change if needed */
					bit_fixed = newlistnode (S_LIST, NOPOSN, anc, bit_fixed);
				} else {
					/* easily changable ancestor, add in scope order */
					treenode *walk, *prev;

					for (walk = unfixed, prev = NULL; !EndOfList (walk); prev = walk, walk = NextItem (walk)) {
						if (NScopeOf (anc) < NScopeOf (ThisItem (walk))) {
							break;		/* insert here */
						}
					}
					if (!prev) {
						unfixed = newlistnode (S_LIST, NOPOSN, anc, unfixed);
					} else {
						SetRight (prev, newlistnode (S_LIST, NOPOSN, anc, walk));
					}
				}
			}

#if 0
fprintf (stderr, "chk2: cprotocolinherit: hard_fixed = ");
printtreenl (stderr, 4, hard_fixed);
fprintf (stderr, "chk2: cprotocolinherit: soft_fixed = ");
printtreenl (stderr, 4, soft_fixed);
fprintf (stderr, "chk2: cprotocolinherit: unfixed = ");
printtreenl (stderr, 4, unfixed);
#endif
			/* start merging, and abort if it goes bad */
			leftovers = NTypeAddr (thistag);
			/* hard_fixed things in first */
			for (t = hard_fixed; !EndOfList (t); t = NextItem (t)) {
				if (!pi_valcheck (NTypeOf (thistag), *leftovers, ThisItem (t))) {
					chkreport_s (CHK_EXTENDS_ERROR, chklocn, "unsatisfiable constraints in inheritance");
				}
				hval = pi_merge (thistag, ThisItem (t), &leftovers);
				SetNTypeAttr (ThisItem (t), NTypeAttrOf (ThisItem (t)) | TypeAttr_fixed);
			}
			/* soft_fixed things in next */
			for (t = soft_fixed; !EndOfList (t); t = NextItem (t)) {
				if (!pi_valcheck (NTypeOf (thistag), *leftovers, ThisItem (t))) {
					chkreport_s (CHK_EXTENDS_ERROR, chklocn, "probably unsatisfiable constraints in inheritance");
				}
				hval = pi_merge (thistag, ThisItem (t), &leftovers);
				SetNTypeAttr (ThisItem (t), NTypeAttrOf (ThisItem (t)) | TypeAttr_fixed);
			}
			/* bit_fixed things in next, prepared to reassign these (but need to be carefule) */
			for (t = bit_fixed; !EndOfList (t); t = NextItem (t)) {
				if (!pi_valcheck (NTypeOf (thistag), *leftovers, ThisItem (t))) {
					/* reassign values to tags in "t", but avoid changing any tags that were inherited */
					hval = pi_reassign2 (NTypeOf (ThisItem (t)), hval, NTypeOf (thistag), *leftovers, DExtraOf (NDeclOf (ThisItem (t))));
					if (!pi_valcheck (NTypeOf (thistag), *leftovers, ThisItem (t))) {
						chkreport_s (CHK_EXTENDS_ERROR, chklocn, "cprotocolinherit: impossible 1");
					}
				}
				hval = pi_merge (thistag, ThisItem (t), &leftovers);
				SetNTypeAttr (ThisItem (t), NTypeAttrOf (ThisItem (t)) | TypeAttr_fixed);
			}
			/* then the unfixed things, which we're prepared to change */
			for (t = unfixed; !EndOfList (t); t = NextItem (t)) {
				if (!pi_valcheck (NTypeOf (thistag), *leftovers, ThisItem (t))) {
					/* reassign values to tags in "t" */
					hval = pi_reassign (NTypeOf (ThisItem (t)), hval, NTypeOf (thistag), *leftovers);
					if (!pi_valcheck (NTypeOf (thistag), *leftovers, ThisItem (t))) {
						chkreport_s (CHK_EXTENDS_ERROR, chklocn, "cprotocolinherit: impossible 2");
					}
				}
				hval = pi_merge (thistag, ThisItem (t), &leftovers);
				SetNTypeAttr (ThisItem (t), NTypeAttrOf (ThisItem (t)) | TypeAttr_fixed);
#if 0
fprintf (stderr, "chk2: cprotocolinherit: after unfixed merge, NTypeOf (thistag) = ");
printtreenl (stderr, 4, NTypeOf (thistag));
#endif
			}
			/* finish of fixing values */
			pi_assign (*leftovers, hval);
			/* chkreport_s (CHK_EXTENDS_ERROR, chklocn, "multiple inheritance not implemented yet.."); */
		}
		/*}}}*/
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void pi_flatten_ancestors (**aptr)*/
/*
 *	flattens ancestor tree (single level)
 */
PRIVATE void pi_flatten_ancestors (treenode **aptr)
{
	treenode **ipoint = aptr;
	treenode *t;

	for (t=*aptr; !EndOfList(t); t = NextItem(t)) {
		treenode *aprot = ThisItem (t);
		treenode *decl;
		
		if (TagOf (aprot) != N_TPROTDEF) {
			chkreport_s (CHK_EXTENDS_ERROR, chklocn, "inheritance error");
		}
		decl = NDeclOf (ThisItem (t));

		if (DExtraOf (decl)) {
			/* copy elements over */
			for (decl = DExtraOf (decl); !EndOfList (decl); decl = NextItem (decl)) {
				/* scan and make sure it's not already here */
				treenode *tmp;

				for (tmp=*aptr; !EndOfList(tmp); tmp = NextItem(tmp)) {
					if (ThisItem (tmp) == ThisItem (decl)) {
						break;
					}
				}
				if (EndOfList (tmp)) {
					*ipoint = newlistnode (S_LIST, NOPOSN, ThisItem (decl), *ipoint);
				}
			}
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void cvardecltype (treenode *tptr)*/
/*
 *	checks that the given specifier tree is valid for variable declarations
 */
PRIVATE void cvardecltype (treenode *tptr)
{
#if 0
fprintf (stderr, "cvardecltype(): tptr =");
printtreenl (stderr, 4, tptr);
#endif
	switch (TagOf (tptr)) {
	case N_SPROTDEF:
	case N_TPROTDEF:
		chkreport (CHK_PROTOCOL_AS_DECLTYPE, chklocn);
		break;
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC treenode *cdeclaration(tptr)             declarations*/
/*****************************************************************************
 *
 *  cdeclaration performs semantic checking on declaration tree 'tptr'
 *
 *****************************************************************************/
PUBLIC treenode *cdeclaration (treenode *volatile tptr)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;

	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0) {
		tptr = NULL;
	} else {
		chklocn = LocnOf (tptr);
		switch (TagOf (tptr)) {
			/*{{{  case S_VALABBR S_ABBR */
		case S_VALABBR:
		case S_ABBR:
			{
				treenode *t = NTypeOf (DNameOf (tptr));
				treenode *type, *basetype;
				treenode *abbrtype;
				BIT32 as_iospec = 0;
				int abbr_of_nothing = 0;

				/* We can't plug the result of typecheck in directly as it already
				   exists elsewhere on the tree */
				type = (t == NULL) ? unknownnodeptr : t;
				if (DValOf (tptr) == NULL) {
					break;
				}
#if 0
fprintf (stderr, "cdeclaration: (ABBR/VALABBR): before fixups and typecheck. type = ");
printtreenl (stderr, 4, type);
#endif
				/* might need to fix-up some channel-direction specifiers */
				if ((TagOf (type) == S_ASINPUT) || (TagOf (type) == S_ASOUTPUT)) {
					int specattr = (TagOf (type) == S_ASINPUT) ? TypeAttr_marked_in : TypeAttr_marked_out;
					treenode *rtype = ctype (type);

					if (OpTypeAttrOf (type) & TypeAttr_shared) {
						specattr |= TypeAttr_shared;
					}
					if (TagOf (rtype) != N_TYPEDECL) {
						chkreport (CHK_UNEXPECTED_CHANDIR, chklocn);
					} else {
						type = newnamenode (TagOf (rtype), LocnOf (rtype), NNameOf (rtype), NTypeOf (rtype),
								NDeclOf (rtype), NLexLevelOf (rtype), NScopeOf (rtype), NModeOf (rtype));
						SetNTypeAttr (type, specattr);
					}
					SetNType (DNameOf (tptr), type);
				}
#if 0
fprintf (stderr, "cdeclaration: (ABBR/VALABBR): before typecheck. type = ");
printtreenl (stderr, 4, type);
#endif
				/*{{{  extract any direction specifier*/
				if ((TagOf (DValOf (tptr)) == S_ASINPUT) || (TagOf (DValOf (tptr)) == S_ASOUTPUT)) {
					as_iospec = (TagOf (DValOf (tptr)) == S_ASOUTPUT) ? TypeAttr_marked_out : TypeAttr_marked_in;

					if (OpTypeAttrOf (DValOf (tptr)) & TypeAttr_shared) {
						as_iospec |= TypeAttr_shared;
					}
					SetDVal (tptr, OpOf (DValOf (tptr)));
				} else if (TagOf (DValOf (tptr)) == S_SEGMENT) {
					treenode *txptr = SNameOf (DValOf (tptr));

					/* might find a channel-direction specifier on this one */
					if ((TagOf (txptr) == S_ASINPUT) || (TagOf (txptr) == S_ASOUTPUT)) {
						as_iospec = (TagOf (txptr) == S_ASOUTPUT) ? TypeAttr_marked_out : TypeAttr_marked_in;
						if (OpTypeAttrOf (txptr) & TypeAttr_shared) {
							as_iospec |= TypeAttr_shared;
						}
						SetSName (DValOf (tptr), OpOf (txptr));
					}
				}
				/*}}}*/
#if 0
fprintf (stderr, "cdeclaration: (ABBR/VALABBR): before typecheck. DValOf(tptr) = ");
printtreenl (stderr, 4, DValOf(tptr));
#endif
				abbrtype = typecheck (DValOf (tptr), type);
				if (DValOf (tptr) && (TagOf (DValOf (tptr)) == S_NULLARRAY) && (TagOf (type) == S_ARRAY) && (ARDimLengthOf (type) == NULL)) {
					abbr_of_nothing = 1;
				}
#if 0
fprintf (stderr, "cdeclaration: (ABBR/VALABBR): DValOf (tptr) =");
printtreenl (stderr, 4, DValOf (tptr));
fprintf (stderr, "cdeclaration: (ABBR/VALABBR): type =");
printtreenl (stderr, 4, type);
fprintf (stderr, "cdeclaration: (ABBR/VALABBR): abbrtype =");
printtreenl (stderr, 4, abbrtype);
fprintf (stderr, "cdeclaration: (ABBR/VALABBR): as_iospec = %d\n", as_iospec);
#endif

				if (t == NULL) {
					SetNType (DNameOf (tptr), copytree (abbrtype, syn_lexlevel));
				} else if (!spec_into_type (cspecifier (t), abbrtype)) {
					chkreport (CHK_ABBR_TYPE_MISMATCH, chklocn);
				}
				/*{{{  check for valid channel direction specifiers */
				for (basetype = type; TagOf (basetype) == S_ARRAY; basetype = ARTypeOf (basetype));
				if (TagOf (basetype) == S_CHAN) {
					const BIT32 lhsspec = (TypeAttrOf (basetype) & (TypeAttr_marked_in | TypeAttr_marked_out));

					if (strict_checking && !lhsspec) {
						chkreport_s (CHK_NO_CHANDIR_SPEC, chklocn, WNameOf (NNameOf (DNameOf (tptr))));
					} else if (strict_checking && !as_iospec) {
						treenode *baseexpr;

						for (baseexpr = DValOf (tptr); TagOf (baseexpr) == S_ARRAYSUB; baseexpr = ASBaseOf (baseexpr));
						switch (TagOf (baseexpr)) {
						case N_DECL:
						case N_PARAM:
						case N_ABBR:
							chkreport_s (CHK_NO_CHANDIR_SPEC, chklocn, WNameOf (NNameOf (baseexpr)));
							break;
						default:
							chkreport (CHK_NO_CHANDIR_SPEC_ANON, chklocn);
							break;
						}
					} else if ((lhsspec && as_iospec) && (lhsspec != as_iospec)) {
						chkreport_s (CHK_BAD_CHAN_CONFLICT, chklocn, WNameOf (NNameOf (DNameOf (tptr))));
					} else if (!lhsspec && as_iospec) {
						/* inherit channel direction (actual -> abbrev. only though) */
						SetTypeAttr (basetype, TypeAttrOf (basetype) | as_iospec);
					}
				} else if (TagOf (basetype) == N_TYPEDECL) {
					/* check for shared incompatabilities, that may have gotten to here */
					if ((TagOf (type) == N_TYPEDECL) && (TagOf (abbrtype) == N_TYPEDECL)) {
						const BIT32 lhsspec = NTypeAttrOf (type) & (TypeAttr_marked_in | TypeAttr_marked_out | TypeAttr_shared);
						const BIT32 rhsspec = NTypeAttrOf (abbrtype) & (TypeAttr_marked_in | TypeAttr_marked_out | TypeAttr_shared);

						if ((lhsspec ^ rhsspec) & TypeAttr_shared) {
							if (lhsspec & TypeAttr_shared) {
								chkreport (CHK_ABBR_LHS_SHARED, chklocn);
							} else {
								chkreport (CHK_ABBR_RHS_SHARED, chklocn);
							}
						} else if (lhsspec ^ rhsspec) {
							chkreport (CHK_BAD_CHAN_CONFLICT_ANON, chklocn);
						}
					}
				}
				/*}}}  */
				/*{{{  check type is legal for a VAL or VAR */
				/* check for NULLARRAY on the RHS -- valid as long as type is an array whose size is NULL */
				if (abbr_of_nothing) {
					/* safe */
				} else if (TagOf (tptr) == S_VALABBR) {
					if (!validval (NTypeOf (DNameOf (tptr))))
						/*chkreport(CHK_INV_VAL, chklocn); */
						chk_invalid_val ();
				} else { /* if (TagOf(tptr) == S_ABBR) */
					checkelement (DValOf (tptr), NTypeOf (DNameOf (tptr)), 0);
				}
				/*}}} */
				/*{{{  fold the initialiser */
				if (TagOf (abbrtype) != S_UNDECLARED) {
					/* Force a constant fold here (if the initialiser is a name of an array
					   the constant fold would not be generated) */
					/* bug 1158 - note that this creates `copies' of tables here;
					   this needs to be done, or something else breaks;
					   the copies are `removed' just as they are generated
					   into the code buffer - CON 14/2/91 */
					if (isconst (DValOf (tptr))) {
						if (TagOf (DValOf (tptr)) != S_NULLARRAY) {
							SetDVal (tptr, newconstexp (DValOf (tptr)));
						}
					} else {
						/* We only check VAL abbreviations - if non-VAL abbreviations
						   are permitted (eg in the NDL reader, then they are dealt
						   with elsewhere. CON 01/05/92
						 */
						if ((TagOf (tptr) == S_VALABBR) && NLexLevelOf (DNameOf (tptr)) == 0)	/* bug TS/1652 03/04/92 */
							chkreport (CHK_OUTERLEVEL_CONSTANT, chklocn);
						SetDVal (tptr, foldexp (DValOf (tptr)));
					}
				}
				/*}}} */
#if 0
				check_for_chanofany (DNameOf (tptr));	/* WP 00026 30/03/92 */
#endif
			}
#if 0
fprintf (stderr, "cdeclaration: (ABBR/VALABBR): here!*n");
#endif
			break;
			/*}}} */
			/*{{{  case S_VALRETYPE S_RETYPE */
		case S_VALRETYPE:
		case S_RETYPE:
			{
				treenode *const nptr = DNameOf (tptr);
#ifdef OCCAM2_5
				const BOOL reshapes = NVReshapesOf (nptr);
#endif
				treenode *lhstype = NTypeOf (nptr);
				treenode *rhstype, *type;
				BIT32 as_iospec = 0;

#if 0
if (TagOf (lhstype) == S_CHAN) {
fprintf (stderr, "cdeclaration: RETYPE: lhstype = CHAN, shared = %d\n", TypeAttrOf (lhstype) & TypeAttr_shared);
}
#endif

				if (DValOf (tptr) == NULL) {
					break;
				}
				/* We don't have any idea what the type of the rhs of a RETYPE is,
				   so say S_UNKNOWN */
				/*{{{  extract any direction specifier*/
				if ((TagOf (DValOf (tptr)) == S_ASINPUT) || (TagOf (DValOf (tptr)) == S_ASOUTPUT)) {
					as_iospec = (TagOf (DValOf (tptr)) == S_ASOUTPUT) ? TypeAttr_marked_out : TypeAttr_marked_in;
					SetDVal (tptr, OpOf (DValOf (tptr)));
				} else if (TagOf (DValOf (tptr)) == S_SEGMENT) {
					treenode *txptr = SNameOf (DValOf (tptr));

					/* might find a channel-direction specifier on this one */
					if ((TagOf (txptr) == S_ASINPUT) || (TagOf (txptr) == S_ASOUTPUT)) {
						as_iospec = (TagOf (txptr) == S_ASOUTPUT) ? TypeAttr_marked_out : TypeAttr_marked_in;
						SetSName (DValOf (tptr), OpOf (txptr));
					}
				}
				/*}}}*/
				rhstype = typecheck (DValOf (tptr), unknownnodeptr);

				if (lhstype == NULL) {
					chkreport (CHK_RETYPE_NOTYPE, chklocn);
				}
				if (TagOf (rhstype) == S_UNDECLARED) {	/* Don't try any checking */
					break;
				}
#ifndef OCCAM2_5
				/*{{{  check there is at most one unknown dimension, work it out */
				/* If the RETYPE specifier has an array with null dimension,
				   we must work out the dimension for ourselves from the
				   size of the rhs. */
				{
					int unknowndims = 0;
					treenode *t;
					/*{{{  count unknown dimensions */
					for (t = lhstype; TagOf (t) == S_ARRAY; t = ARTypeOf (t)) {
						if (ARDimLengthOf (t) == NULL) {
							unknowndims++;
						}
					}
					/*}}} */
					/*printf("cdeclaration: reshapes?:%d, unknowndims:%d\n", reshapes, unknowndims); */
					if (unknowndims > 1) {
						chkreport (CHK_ADIM_MISSING, chklocn);
					} else if (unknowndims == 1) {
						/*{{{  fill in the missing dimension */
						BIT32 lbytes, rbytes;
						INT32 nelements = 1;
						treenode *unknowndimp = NULL;
						cspecifier (lhstype);
						/*{{{  look for unknown dimensions */
						for (t = lhstype; TagOf (t) == S_ARRAY; t = ARTypeOf (t)) {
							DEBUG_MSG (("cdeclaration: RETYPE; looking for unknown dim: t is %x\n", t));
							if (ARDimLengthOf (t) == NULL)
								unknowndimp = t;
							else if (check_array_overflow (chklocn, (BIT32) ARDimOf (t), (BIT32) nelements, TRUE));	/* skip - error has already been raised */
							else
								nelements *= ARDimOf (t);
						}
						/*}}} */

/* INSdi02341 *//* Check for outermost overflow */
						(void) check_array_overflow (chklocn, bytesin (t), nelements, TRUE);
						lbytes = (BIT32) nelements *bytesin (t);

						rbytes = bytesin (rhstype);
						if (rbytes == -1)
							SetARDim (unknowndimp, -1);
						else if ((rbytes % lbytes) == 0) {
							const INT32 dim = rbytes / lbytes;

/* INSdi02341 *//* Check for outermost overflow */
							(void) check_array_overflow (chklocn, 1, dim, TRUE);

							SetARDim (unknowndimp, dim);
							SetARDimLength (unknowndimp, newconstant (dim));
						} else {
							chkreport (CHK_RETYPE_TYPE_MISMATCH, chklocn);
						}
						/*}}} */
					} else {
						/*{{{  check the retype specifier and then check lh & rh sizes are equal */
						INT32 rhsize = bytesin (rhstype);
						INT32 lhsize;
						/*{{{  check the type tree is valid */
#ifdef OCCAM2_5
						if (reshapes)
							creshapes (lhstype);
						else
#endif
							ctype (lhstype);
						/*}}} */

#ifdef MOBILES
						/* special case for MOBILEs */
						if (TagOf (rhstype) == S_MOBILE) {
							rhssize = 4;
						}
#endif

						lhsize = bytesin (lhstype);
						/* check the left and right-hand sizes are equal */
						if (rhsize == -1) {	/* bug ts/2055 22/01/93 */
							/*{{{  check that it can fit */
							if ((lhsize % known_bytesin (rhstype)) != 0) {
								chkreport (CHK_RETYPE_TYPE_MISMATCH, chklocn);
							}
							/*}}} */
						}
#ifdef OCCAM2_5
						else if (reshapes);	/* skip */
#endif
						else if (lhsize != rhsize) {	/* If we know the right-hand size at compile time */
							chkreport (CHK_RETYPE_TYPE_MISMATCH, chklocn);
						}
						/*}}} */
					}
				}
				/*}}} */
#else
				/*{{{  check for unknown dimensions etc */
				{
					/*{{{  local variables */
					int opendims = 0;
					int vardims = 0;
					INT32 nelements = 1;
					INT32 rhsize = bytesin (rhstype);
					INT32 lhsize;
					treenode *arraybase;
					treenode *unknowndimp = NULL;
					/*}}} */
					/*{{{  check the type tree is valid */
					cretypes (lhstype, reshapes);
					/*}}} */
#ifdef MOBILES
					/* special case when RETYPEing a MOBILE */
#if 0
fprintf (stderr, "rhstype = ");
printtree (stderr, 4, rhstype);
fprintf (stderr, "\n");
#endif
#endif
					/*{{{  count variable and open dimensions */
					{
						treenode *t;
						for (t = lhstype; TagOf (t) == S_ARRAY; t = ARTypeOf (t))
							if (ARDimLengthOf (t) == NULL) {
								opendims++;
								unknowndimp = t;
							} else if (ARDimOf (t) == (-1))
								vardims++;
							else if (check_array_overflow (chklocn, (BIT32) ARDimOf (t), (BIT32) nelements, TRUE));	/* skip - error has already been raised */
							else
								nelements *= ARDimOf (t);
						arraybase = t;
					}
					/*}}} */

					if (opendims > 1)
						chkreport (CHK_ADIM_MISSING, chklocn);

					else if ((opendims + vardims) == 0)
						/*{{{  check the size is correct */
					{
						lhsize = bytesin (lhstype);
						if (rhsize == -1)
							/*{{{  check that it can fit */
						{
							if ((lhsize % known_bytesin (rhstype)) != 0)
								chkreport (CHK_RETYPE_TYPE_MISMATCH, chklocn);
						}
						/*}}} */
						else if (lhsize != rhsize) {
#if 0
fprintf (stderr, "rhsize = %d, lhsize = %d\n", (int)rhsize, (int)lhsize);
#endif
							chkreport (CHK_RETYPE_TYPE_MISMATCH, chklocn);
						}
					}
					/*}}} */

					else
						/*{{{  fill in the missing dimension if possible */
					{
/* INSdi02341 *//* Check for outermost overflow */
						(void) check_array_overflow (chklocn, bytesin (arraybase), nelements, TRUE);
						lhsize = (BIT32) nelements *bytesin (arraybase);

						if (rhsize == -1)
							/*{{{  nothing can be deduced */
						{
							if (unknowndimp != NULL)
								SetARDim (unknowndimp, -1);
						}
						/*}}} */
						else if ((rhsize % lhsize) != 0) {
							chkreport (CHK_RETYPE_TYPE_MISMATCH, chklocn);
						} else if (vardims == 0)
							/*{{{  calculate open dimension */
						{
							const INT32 dim = rhsize / lhsize;

/* INSdi02341 *//* Check for outermost overflow */
							(void) check_array_overflow (chklocn, 1, dim, TRUE);

							SetARDim (unknowndimp, dim);
							SetARDimLength (unknowndimp, newconstant (dim));
						}
						/*}}} */
						else if (opendims != 0)
							/*{{{  nothing can be deduced */
							SetARDim (unknowndimp, -1);
						/*}}} */
					}
					/*}}} */
				}
				/*}}} */
#endif
				/*{{{  check type is legal */
				/* VAL abbreviations may not be PORT, CHAN or TIMER */
				/* We also enforce that a var. RETYPE cannot be a PORT, CHAN or TIMER */
				/* OLD VERSION:
				   if (!validval(NTypeOf(nptr)) ||
				   !validval(rhstype))
				   chkreport (CHK_INV_RETYPE, chklocn);
				 */
				/* We enforce that CHANs can only be RETYPEd to CHANs etc */
				/* COMMENTED OUT AGAIN. New try below
				   if (!validval(NTypeOf(nptr)) ||
				   !validval(rhstype))
				   {
				   if (TagOf(tptr) == S_VALRETYPE)
				   chkreport (CHK_BAD_VALRETYPE, chklocn);
				   /@ TEMPORARY: ALLOW ANY RETYPEs
				   if (basetype(NTypeOf(nptr)) != basetype(rhstype))
				   chkreport (CHK_BAD_CHANRETYPE, chklocn);
				   @/
				   }
				 */
				{
					const int lhsbase = basetype (NTypeOf (nptr));
					const int rhsbase = basetype (rhstype);
					if (illegal_retype (lhsbase) || illegal_retype (rhsbase))
						chkreport (CHK_INV_RETYPE, chklocn);
					if ((rhsbase == S_CHAN) && (TagOf (tptr) != S_VALRETYPE) && (lhsbase != S_CHAN))
						chkreport (CHK_BAD_CHANRETYPE, chklocn);
					/*{{{  warn if REAL64 words are swapped */
					if (swap_r64words) {
						if ((lhsbase == S_REAL64) || (rhsbase == S_REAL64))
							msg_out (SEV_WARN, CHK, CHK_RETYPINGREAL64, chklocn);
					}
					/*}}} */
				}
				/*}}} */
				/*{{{  check for valid RESHAPES */
#ifdef OCCAM2_5
				if (reshapes) {
					treenode *const lhsbase = usertype_tree (NTypeOf (nptr));
					treenode *const rhsbase = usertype_tree (rhstype);
					if ((TagOf (NTypeOf (nptr)) != S_ARRAY)
					    || (TagOf (rhstype) != S_ARRAY)
					    || (TagOf (lhsbase) != TagOf (rhsbase))
					    || ((TagOf (lhsbase) == N_TYPEDECL) && (lhsbase != rhsbase)))
						chkreport (CHK_BAD_RESHAPES, chklocn);
				}
#endif
				/*}}} */
				/*{{{  fixup any direction-specifiers*/
				/*}}}*/
				/*{{{  check rhs is legal for a var */
				if (TagOf (tptr) == S_RETYPE) {
					/*checkvariable(DValOf(tptr)); */
					checkelement (DValOf (tptr), rhstype, 0);
				}
				/*}}} */
				/*{{{  fold the initialiser */
				/* Force a constant fold here (if the initialiser is a name of an array
				   the constant fold would not be generated) */
				/* bug 1158 - note that this creates `copies' of tables here;
				   this needs to be done, or something else breaks;
				   the copies are `removed' just as they are generated
				   into the code buffer - CON 14/2/91 */
				SetDVal (tptr, foldexpinto (isconst (DValOf (tptr)) ? newconstexp (DValOf (tptr))
							    : DValOf (tptr), TagOf (follow_user_type (lhstype))));
				/*{{{ MDP fix (bug TS/1652 03/04/92)*/
				if (!isconst (DValOf (tptr)) && NLexLevelOf (nptr) == 0) {	/* bug TS/1652 03/04/92 */
					chkreport (CHK_OUTERLEVEL_CONSTANT, chklocn);
				}
				/*}}}*/
				/*}}} */
				/*{{{  check for valid channel direction specifiers */
				for (type = lhstype; TagOf (type) == S_ARRAY; type = ARTypeOf (type));
				if (TagOf (type) == S_CHAN) {
					const BIT32 lhsspec = (TypeAttrOf (type) & (TypeAttr_marked_in | TypeAttr_marked_out));

					if (strict_checking && !lhsspec) {
						chkreport_s (CHK_NO_CHANDIR_SPEC, chklocn, WNameOf (NNameOf (DNameOf (tptr))));
					} else if (strict_checking && !as_iospec) {
						treenode *baseexpr;

						for (baseexpr = DValOf (tptr); TagOf (baseexpr) == S_ARRAYSUB; baseexpr = ASBaseOf (baseexpr));
						switch (TagOf (baseexpr)) {
						case N_DECL:
						case N_PARAM:
						case N_ABBR:
							chkreport_s (CHK_NO_CHANDIR_SPEC, chklocn, WNameOf (NNameOf (baseexpr)));
							break;
						default:
							chkreport (CHK_NO_CHANDIR_SPEC_ANON, chklocn);
							break;
						}
					} else if ((lhsspec && as_iospec) && (lhsspec != as_iospec)) {
						chkreport_s (CHK_BAD_CHAN_CONFLICT, chklocn, WNameOf (NNameOf (DNameOf (tptr))));
					} else if (!lhsspec && as_iospec) {
						/* inherit channel direction (actual -> retype. only though) */
						SetTypeAttr (type, TypeAttrOf (type) | as_iospec);
					}
				}
				/*}}}  */
#ifdef MOBILES
				/*{{{  check for a chan of channel type */
				if ((TagOf (type) == S_CHAN) && (TagOf (ProtocolOf (type)) == N_TYPEDECL)) {
					const int is_shared = (TypeAttrOf (type) & TypeAttr_shared);
					const int old = switch_to_real_workspace ();
#if 0
fprintf (stderr, "cdeclaration: RETYPE'd CHAN of some TYPEDECL.  is_shared = %d, type-tree =", is_shared);
printtreenl (stderr, 4, type);
#endif

					/* remove any SHARED indicator */
					SetTypeAttr (type, TypeAttrOf (type) & ~TypeAttr_shared);
					if (nodetypeoftag (TagOf (ProtocolOf (type))) == NAMENODE) {
						treenode *protocol = ProtocolOf (type);

						/* better create a copy of the protocol type before setting the shared bit */
						protocol = newnamenode (TagOf (protocol), LocnOf (protocol), NNameOf (protocol), NTypeOf (protocol),
								NDeclOf (protocol), NLexLevelOf (protocol), NScopeOf (protocol), NModeOf (protocol));
#if 0
fprintf (stderr, "cdeclaration: found %s of type CHAN, NTypeAttrOf (ProtocolOf (NTypeOf (dtptr))) = %d.  Protocol node at %p\n", WNameOf (NNameOf (dtptr)), NTypeAttrOf (ProtocolOf (NTypeOf (dtptr))), protocol);
#endif
						SetNTypeAttr (protocol, NTypeAttrOf (ProtocolOf (type)) | (is_shared ? TypeAttr_shared : 0));
						SetProtocol (type, protocol);
					} else {
						/* can't share this! */
						chkerr (CHK_UNSHARABLE_PROTOCOL, chklocn);
					}
					switch_to_prev_workspace (old);
				}
				/*}}}*/
#endif
#if 0
				check_for_chanofany (nptr);	/* WP 00026 30/03/92 */
#endif
			}
			break;

			/*}}} */
			/*{{{  case S_SFUNCDEF S_LFUNCDEF */
		case S_SFUNCDEF:
		case S_LFUNCDEF:
			/* Parameter specifiers have already been checked */
			/* VALOF contents and scope have already been checked */
			{
				treenode *const typesptr = FnTypeListOf (NTypeOf (DNameOf (tptr)));
				/*{{{  check that the function result types are all scalar */
#ifndef OCCAM2_5
				{
					/* bug 1267 - we must do this check even on separately compiled functions,
					   incase they were declared inside a PRAGMA EXTERNAL. - CON 16/5/91
					 */
					treenode *t;
					for (t = typesptr; !EndOfList (t); t = NextItem (t))
						if (!isscalartype_tree (ThisItem (t)))
							chkreport (CHK_BAD_FUNCTION_TYPE, chklocn);
				}
#endif
				/*}}} */

				if (!separatelycompiled (DNameOf (tptr))) {
					treenode *const valofptr = skipspecifications (DValOf (tptr));
					if (valofptr != NULL) {
						/*{{{  check the valof result types match the function formal types */
						{
							treenode *const resultlist = VLResultListOf (valofptr);
							chklocn = LocnOf (resultlist);
							checkexpsandtypes (resultlist, typesptr, CHK_INVTYPE_FRESULT,
									   CHK_TOOMANYEXPS, CHK_TOOFEWEXPS);
						}
						/*}}} */
						/*{{{  fold the result list */
						SetVLResultList (valofptr, foldexplist (VLResultListOf (valofptr)));
						/*}}} */
					}
				}
#ifdef OCCAM2_5
				if (isconst_fn (tptr)) {
					/*printf("cdeclaration: fn %s is constant\n", WNameOf(NNameOf(DNameOf(tptr)))); */
					SetNPConstantFn (DNameOf (tptr), TRUE);
				}
#endif
			}
			break;
			/*}}} */
			/*{{{  case S_DECL */
		case S_DECL:
			/* Check that the type tree is valid */
			{
				treenode *dtptr = DNameOf (tptr);
				treenode *rtype;

				/* Unfortunately, all the namenodes in the list point to the same
				   type tree, so we only have to check the type tree once. */

				if (TagOf (dtptr) == S_LIST) {
					dtptr = ThisItem (dtptr);
				}

				if (TagOf (dtptr) != N_DECL) {
					/* Should never happen if syn is correct */
					msg_out_s (SEV_INTERNAL, CHK, CHK_INV_DECL_TAG, chklocn, itagstring (TagOf (dtptr)));
				}

#ifdef MOBILES
				rtype = ctype (NTypeOf (dtptr));
				if (rtype != NTypeOf (dtptr)) {
					int was_input = 0;

#if 0
fprintf (stderr, "cdeclaration: rtype != orig-type, name is `%s\', type is: ", WNameOf (NNameOf (dtptr)));
printtreenl (stderr, 4, NTypeOf (dtptr));
#endif
					/* this happens when ASINPUT/ASOUTPUT nodes get mangled for channel types */
					switch (TagOf (NTypeOf (dtptr))) {
					case S_ASINPUT:
						was_input = 1;
					case S_ASOUTPUT:
						/* ctype has already checked that the sub-type is N_TYPEDECL */
						{
							treenode *xptr = DNameOf (tptr);
							const int is_shared_end = (OpTypeAttrOf (NTypeOf (dtptr))) & TypeAttr_shared;
							const int old = switch_to_real_workspace ();
							treenode *typecopy = newnamenode (TagOf (rtype), LocnOf (rtype), NNameOf (rtype), NTypeOf (rtype),
									NDeclOf (rtype), NLexLevelOf (rtype), NScopeOf (rtype), NModeOf (rtype));

#if 0
fprintf (stderr, "cdeclaration: set S_ASINPUT/S_ASOUTPUT, was_input = %d, is_shared_end = %d.\n", was_input, is_shared_end);
#endif
							SetNTypeAttr (typecopy, (was_input ? TypeAttr_marked_in : TypeAttr_marked_out) | (is_shared_end ? TypeAttr_shared : 0));
							if (TagOf (xptr) == S_LIST) {
								while (!EndOfList (xptr)) {
									SetNType (ThisItem (xptr), typecopy);
									SetNDecl (ThisItem (xptr), tptr);
									/* SetNTypeAttr (ThisItem (xptr), (was_input ? TypeAttr_marked_in : TypeAttr_marked_out) | (is_shared_end ? TypeAttr_shared : 0)); */
									xptr = NextItem (xptr);
								}
							} else {
								SetNType (xptr, typecopy);
								SetNDecl (xptr, tptr);
								/* SetNTypeAttr (xptr, (was_input ? TypeAttr_marked_in : TypeAttr_marked_out) | (is_shared_end ? TypeAttr_shared : 0)); */
							}
							switch_to_prev_workspace (old);
						}
						break;
					default:
						/*{{{  deal with dynamic MOBILE array sub-type oddities here (another route into nested MOBILE support..)*/
						{
							treenode *ntype = follow_user_type (NTypeOf (dtptr));
							const int old = switch_to_real_workspace ();

							if ((TagOf (ntype) == S_MOBILE) && (TagOf (MTypeOf (ntype)) == S_ARRAY) && !ARDimLengthOf (MTypeOf (ntype))) {
								treenode **mtptr = ARTypeAddr (MTypeOf (ntype));
								int specattr = 0;
								int isinput = 0;

								/* call ctype() to make sure.. */
								ctype (*mtptr);

								if (TagOf (*mtptr) == S_ASINPUT) {
									specattr = TypeAttr_marked_in;
									isinput = 1;
								} else if (TagOf (*mtptr) == S_ASOUTPUT) {
									specattr = TypeAttr_marked_out;
								}
								if (specattr) {
									treenode *typecopy = NULL;
									treenode *basetype = OpOf (*mtptr);

									if (OpTypeAttrOf (*mtptr) & TypeAttr_shared) {
										specattr |= TypeAttr_shared;
									}
									/* fold it up */
									typecopy = newnamenode (TagOf (basetype), LocnOf (basetype), NNameOf (basetype), NTypeOf (basetype),
											NDeclOf (basetype), NLexLevelOf (basetype), NScopeOf (basetype), NModeOf (basetype));
									SetNTypeAttr (typecopy, specattr);
									*mtptr = typecopy;
								}

#if 0
fprintf (stderr, "cdeclaration: wibble.  incomplete.  *mtptr is ");
printtreenl (stderr, 4, *mtptr);
#endif
							}
							switch_to_prev_workspace (old);
						}
						/*}}}*/
						break;
					}
				}
				/*{{{  check for a chan of channel type */
				if ((TagOf (NTypeOf (dtptr)) == S_CHAN) && (TagOf (ProtocolOf (NTypeOf (dtptr))) == N_TYPEDECL)) {
					const int is_shared = (TypeAttrOf (NTypeOf (dtptr)) & TypeAttr_shared);
					const int old = switch_to_real_workspace ();

					/* remove any SHARED indicator */
					SetTypeAttr (NTypeOf (dtptr), TypeAttrOf (NTypeOf (dtptr)) & ~TypeAttr_shared);
					if (nodetypeoftag (TagOf (ProtocolOf (NTypeOf (dtptr)))) == NAMENODE) {
						treenode *protocol = ProtocolOf (NTypeOf (dtptr));

						/* better create a copy of the protocol type before setting the shared bit */
						protocol = newnamenode (TagOf (protocol), LocnOf (protocol), NNameOf (protocol), NTypeOf (protocol),
								NDeclOf (protocol), NLexLevelOf (protocol), NScopeOf (protocol), NModeOf (protocol));
#if 0
fprintf (stderr, "cdeclaration: found %s of type CHAN, NTypeAttrOf (ProtocolOf (NTypeOf (dtptr))) = %d.  Protocol node at %p\n", WNameOf (NNameOf (dtptr)), NTypeAttrOf (ProtocolOf (NTypeOf (dtptr))), protocol);
#endif
						SetNTypeAttr (protocol, NTypeAttrOf (ProtocolOf (NTypeOf (dtptr))) | (is_shared ? TypeAttr_shared : 0));
						SetProtocol (NTypeOf (dtptr), protocol);
					} else {
						/* can't share this! */
						chkerr (CHK_UNSHARABLE_PROTOCOL, chklocn);
					}
					switch_to_prev_workspace (old);
				}
				/*}}}*/
#else
				ctype (NTypeOf (dtptr));
#endif
				/*}}} */
				cvardecltype (NTypeOf (dtptr));

#if 0
				check_for_chanofany (dtptr);	/* WP 00026 30/03/92 */
#endif
				break;
			}

			/*{{{  case S_PROCDEF */
		case S_PROCDEF:	/* Parameter specifiers have already been checked */
			CASE_CONFIG_SPEC	/* Nothing to do */
				break;
			/*}}} */
			/*{{{  case S_TPROTDEF */
		case S_TPROTDEF:
			{
				treenode **tagpp = NTypeAddr (DNameOf (tptr));
				treenode *taglist = *tagpp;

				walklist ((void *)checktag, taglist, (void *)tagpp);
				/*{{{  give each tag a value */
				if (DExtraOf (tptr)) {
					/* inheriting from another PROTOCOL(s) */
					cprotocolinherit (DNameOf (tptr), DExtraAddr (tptr));
					pi_flatten_ancestors (DExtraAddr (tptr));
				} else {
					int i;
					int had_fixed = 0;

					for (i = 0; !EndOfList (taglist) && (i < MAX_TAGS); taglist = NextItem (taglist), i++) {
						if (!had_fixed && (NTypeAttrOf (ThisItem (taglist)) & TypeAttr_fixed)) {
							if (i) {
								chkreport (CHK_PARTIAL_FIXED_TAGS, chklocn);
							}
							had_fixed = 1;
						} else if (had_fixed && !(NTypeAttrOf (ThisItem (taglist)) & TypeAttr_fixed)) {
							chkreport (CHK_PARTIAL_FIXED_TAGS, chklocn);
						} else if (!had_fixed) {
							SetNTValue (ThisItem (taglist), (BIT32) i);
						}
					}
					if (!EndOfList (taglist)) {
						chkreport_i (CHK_TOO_MANY_TAGS, chklocn, MAX_TAGS);
					}
				}
				/*}}} */
#ifdef MOBILES
				/*{{{  go through and sort out any TYPEDECLs encased in ASINPUT/ASOUTPUT nodes*/
				for (taglist = *tagpp; !EndOfList (taglist) && (TagOf (ThisItem (taglist)) == N_TAGDEF); taglist = NextItem (taglist)) {
					treenode *tagtype = NTypeOf (ThisItem (taglist));
#if 0
fprintf (stderr, "cdeclaration: S_TPROTDEF: ThisItem(taglist) is:");
printtreenl (stderr, 4, ThisItem(taglist));
#endif

					if (!tagtype) {
						continue;
					}
					if (TagOf (tagtype) == S_LIST) {
						while (!EndOfList (tagtype)) {
							fixioprot (ThisItemAddr (tagtype));

#if 0
fprintf (stderr, "cdeclaration: S_TPROTDEF: protocol is");
printtreenl (stderr, 4, protocol);
#endif
							tagtype = NextItem (tagtype);
						}
					} else {
						fixioprot (NTypeAddr (ThisItem (taglist)));
					}
				}
				/*}}}*/
#endif	/* MOBILES */
				/*{{{  check for any duplicate tag values (fixed ones)*/
				{
					char *alloctags = (char *)newvec (256);		/* current implementation only does BYTEs */
					int i;

#if 0
fprintf (stderr, "chk2: cdeclaration(): about to check for duplicate fixed tag values.  *tagpp = ");
printtreenl (stderr, 4, *tagpp);
#endif
					for (i=0; i<256; i++) {
						alloctags[i] = 0;
					}
					for (taglist = *tagpp; !EndOfList (taglist) && (TagOf (ThisItem (taglist)) == N_TAGDEF); taglist = NextItem (taglist)) {
#if 0
fprintf (stderr, "chk2: cdeclaration(): checking for duplicate fixed tag values.  ThisItem (taglist) = ");
printtreenl (stderr, 4, ThisItem (taglist));
#endif
						if (NTypeAttrOf (ThisItem (taglist)) & TypeAttr_fixed) {
							int tagval = NTValueOf (ThisItem (taglist));

							if ((tagval < 0) || (tagval > 255)) {
								chkreport_i (CHK_INVALID_TAG_VALUE, LocnOf (ThisItem (taglist)), tagval);
							} else if (alloctags[tagval]) {
								chkreport_i (CHK_TAG_VALUE_EXISTS, LocnOf (ThisItem (taglist)), tagval);
							} else {
								alloctags[tagval] = 1;
							}
						}
					}

					freevec (alloctags, 256);
				}
				/*}}}*/
			}
			break;
			/*}}} */
			/*{{{  case S_SPROTDEF */
		case S_SPROTDEF:
			{
				treenode *type = NTypeOf (DNameOf (tptr));

				cdefseqprotocol (type);
				if (TagOf (type) == S_LIST) {
					while (!EndOfList (type)) {
						fixioprot (ThisItemAddr (type));
						type = NextItem (type);
					}
				} else {
					fixioprot (NTypeAddr (DNameOf (tptr)));
				}
			}
			break;
			/*}}} */
			/*{{{  case S_TYPEDECL */
#ifdef OCCAM2_5
		case S_TYPEDECL:
			{
				treenode *rhs_type = NTypeOf (DNameOf (tptr));
				treenode *traces = NULL;
				BOOL ismobile = FALSE;
				ctype (rhs_type);

#ifdef MOBILES
				if (TagOf (rhs_type) == S_MOBILE) {
					ismobile = TRUE;
					traces = TypeTracesOf (rhs_type);
					rhs_type = MTypeOf (rhs_type);
				}
#endif
				if (TagOf (rhs_type) == S_RECORD) {
					const BOOL datatype_wanted = ((TypeAttrOf (rhs_type) & TypeAttr_datatype) != 0);

					check_type_is_data_type (rhs_type, chklocn, datatype_wanted);
					crecordtype (tptr, rhs_type, ismobile, ismobile ? traces : NULL);
				} else {
					check_type_is_data_type (rhs_type, chklocn, TRUE);
				}
			}
			break;
#endif
			/*}}} */
#ifdef MOBILES
			/*{{{  case S_FORWDECL */
		case S_FORWDECL:		/* forward declaration of a channel-type */
			break;
			/*}}}*/
			/*{{{  case S_PROCTYPEDECL */
		case S_PROCTYPEDECL:
			{
				treenode *rhs_type = NTypeOf (DNameOf (tptr));
				treenode *walk;

				if (!rhs_type) {
					/* nothing to do */
					break;
				}
				/* re-use cparmlist (below) */
				cparmlist (rhs_type, S_PROCDEF, chklocn, FALSE);

				/* fixup direction-specified parameters */
				for (walk = rhs_type; !EndOfList (walk); walk = NextItem (walk)) {
					treenode *thisparam = ThisItem (walk);
					treenode *type = NTypeOf (thisparam);

					/* might need to fix-up some channel-direction specifiers */
					if ((TagOf (type) == S_ASINPUT) || (TagOf (type) == S_ASOUTPUT)) {
						int specattr = (TagOf (type) == S_ASINPUT) ? TypeAttr_marked_in : TypeAttr_marked_out;
						int is_shared = (OpTypeAttrOf (type) & TypeAttr_shared);
						treenode *rtype = ctype (type);

						if (TagOf (rtype) != N_TYPEDECL) {
							chkreport (CHK_UNEXPECTED_CHANDIR, chklocn);
						} else {
							type = newnamenode (TagOf (rtype), LocnOf (rtype), NNameOf (rtype), NTypeOf (rtype),
									NDeclOf (rtype), NLexLevelOf (rtype), NScopeOf (rtype), NModeOf (rtype));
							SetNTypeAttr (type, specattr | is_shared);
						}
						SetNType (thisparam, type);
					}
				}
#if 0
fprintf (stderr, "chk2: cdeclaration (PROCTYPEDECL): after cparmlist: rhs_type = ");
printtreenl (stderr, 4, rhs_type);
#endif
			}
			break;
			/*}}}*/
			/*{{{  case S_MPROCDECL*/
		case S_MPROCDECL:
			if (DExtraOf (tptr)) {
				treenode *fparams, *eparams;

				if (TagOf (DExtraOf (tptr)) != N_PROCTYPEDECL) {
					if (nodetypeoftag (TagOf (DExtraOf (tptr))) == NAMENODE) {
						chkreport_s (CHK_MPD_BAD_IMPLEMENTS, chklocn, WNameOf (NNameOf (DExtraOf (tptr))));
					} else {
						chkreport_s (CHK_MPD_BAD_IMPLEMENTS_FOR, chklocn, WNameOf (NNameOf (DNameOf (tptr))));
					}
				}
				/* check that the formal parameter list of the PROC matches that of the PROC TYPE it implements */
				for (fparams = NTypeOf (DNameOf (tptr)), eparams = NTypeOf (DExtraOf (tptr));
						!EndOfList (fparams) && !EndOfList (eparams);
						fparams = NextItem (fparams), eparams = NextItem (eparams)) {
					treenode *fparm = ThisItem (fparams);
					treenode *eparm = ThisItem (eparams);
					
					/* check that these have the same type(s) */
#if 0
fprintf (stderr, "chk2: cdeclaration (MPROCDECL): fparam = ");
printtreenl (stderr, 4, fparm);
fprintf (stderr, "chk2: cdeclaration (MPROCDECL): eparam = ");
printtreenl (stderr, 4, eparm);
#endif
					if ((TagOf (fparm) != TagOf (eparm)) || (nodetypeoftag (TagOf (fparm)) != NAMENODE) || (nodetypeoftag (TagOf (eparm)) != NAMENODE)) {
						chkreport (CHK_MPD_BAD_IMPLEMENTS_PARAMS, chklocn);
					}
					if ((NTypeAttrOf (fparm) ^ NTypeAttrOf (eparm)) & TypeAttr_ufixed) {
						/* FIXED specifiers differ */
						chkreport (CHK_MPD_FIXED_MISMATCH, chklocn);
					}
					if (!typesequivalent (NTypeOf (eparm), NTypeOf (fparm), FALSE)) {
						chkreport (CHK_MPD_BAD_IMPLEMENTS_PARAMS, chklocn);
					}
				}
				if (!EndOfList (fparams) || !EndOfList (eparams)) {
					chkreport (CHK_MPD_BAD_IMPLEMENTS_PARAMS, chklocn);
				}
			}
			break;
			/*}}}*/
#endif
		default:
			badtag (chklocn, TagOf (tptr), "cdeclaration");
		}
		DEBUG_MSG (("Checked decl node ok near line %d\n", (int)LocnOf (tptr)));
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));

	return (tptr);
}
/*}}}*/

/*{{{  PUBLIC treenode *cparmlist(tptr, paramtype, locn)     parameters*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  cparmlist performs semantic checking on formal parameter list 'tptr'.
 *            paramtype is S_PROC if we are checking procedure parameters,
 *            otherwise we are checking function parameters.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC treenode *cparmlist (treenode * volatile tptr, const int paramtype, const SOURCEPOSN locn, const BOOL separatelycompiledsc)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;
	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0) {
		tptr = NULL;
	} else {
		treenode *t;
		chklocn = locn;
		for (t = tptr; !EndOfList (t); t = NextItem (t)) {
			treenode *const nptr = ThisItem (t);
#if 0
fprintf (stderr, "cparmlist: nptr =");
printtreenl (stderr, 4, nptr);
#endif
#ifdef MOBILES
			if (TagOf (nptr) == N_VALPARAM) {
				/* better make sure it's not MOBILE */
				treenode *ntype = NTypeOf (nptr);

				if (TagOf (ntype) == S_MOBILE) {
					chkreport (CHK_VAL_MOBILE, chklocn);
				} else if (TagOf (follow_user_type (ntype)) == S_MOBILE) {
					chkreport (CHK_VAL_MOBILE, chklocn);
				}
			}
#endif
			/*{{{  check it's not a var param to a function */
			if (((paramtype != S_PROCDEF) && (paramtype != S_MPROCDECL)) && ((TagOf (nptr) == N_PARAM) || (TagOf (nptr) == N_RESULTPARAM))) {
				chkreport (CHK_FN_VAR_PARAM, chklocn);
			}
			/*}}} */
			/*{{{  check the specifier */
			cspecifier (NTypeOf (nptr));
			/*}}} */
			/*{{{  check a CHAN, PORT or TIMER is not a val param (or result param) */
			if ((TagOf (nptr) == N_VALPARAM) && !validval (NTypeOf (nptr))) {
				/*chkreport (CHK_INV_VAL, chklocn); */
				chk_invalid_val ();
			} else if ((TagOf (nptr) == N_RESULTPARAM) && !validval (NTypeOf (nptr))) {
				chk_invalid_result ();
			}
			/*{{{  frmb -- if strict checking, make sure that channel params have direction specifiers */
			/* don't check seperately compiled stuff */
			if (strict_checking && !separatelycompiledsc) {
				treenode *type = basetype_tree (NTypeOf (nptr));

#if 0
fprintf (stderr, "cparmlist: strict_checking: TypeAttrOf (type) = %d, type (of nptr) is: ", TypeAttrOf (type));
printtreenl (stderr, 4, type);
fprintf (stderr, "cparmlist: nptr is:");
printtreenl (stderr, 4, nptr);
#endif
				if ((TagOf (type) == S_CHAN) && !(TypeAttrOf (type) & (TypeAttr_marked_in | TypeAttr_marked_out))) {
					chkreport_s (CHK_NO_CHANDIR_SPEC, chklocn, WNameOf (NNameOf (nptr)));
				}
			}
			/*}}}  */

			/*}}} */
#if 0
			if (!separatelycompiledsc)
				check_for_chanofany (nptr);	/* WP 00026 30/03/92 */
#endif
		}
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));

	DEBUG_MSG (("Checked paramlist node ok near line %d\n", (int)locn));
	return (tptr);
}

/*}}}*/
/*{{{  PUBLIC treenode *crepl(tptr)                    replicated nodes*/
/*****************************************************************************
 *
 *  crepl performs semantic checking on replicator tree 'tptr'
 *
 *****************************************************************************/
PUBLIC treenode *crepl (treenode * volatile tptr)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;
	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0)
		tptr = NULL;
	else {
		treenode *t;
		chklocn = LocnOf (tptr);
		/*{{{  typecheck and fold start expression */
		t = typecheck (ReplCStartExpOf (tptr), intnodeptr);
		if (TagOf (t) != S_UNDECLARED) {
			if (!sametype (TagOf (t), S_INT))
				chkreport (CHK_INVTYPE_STARTEXP, chklocn);
			/* Fold start expression */
			SetReplCStartExp (tptr, foldexp (ReplCStartExpOf (tptr)));
		}
		/*}}} */
		/*{{{  typecheck and fold length expression */
		t = typecheck (ReplCLengthExpOf (tptr), intnodeptr);
		if (TagOf (t) != S_UNDECLARED) {
			if (!sametype (TagOf (t), S_INT))
				chkreport (CHK_INVTYPE_LENGTHEXP, chklocn);

			SetReplCLengthExp (tptr, foldexp (ReplCLengthExpOf (tptr)));
		}
		/*}}} */
		/*{{{  typecheck and fold step expression */
		if (ReplCStepExpOf (tptr)) {
			t = typecheck (ReplCStepExpOf (tptr), intnodeptr);
			if (TagOf (t) != S_UNDECLARED) {
				if (!sametype (TagOf (t), S_INT))
					chkreport (CHK_INVTYPE_STEPEXP, chklocn);
				SetReplCStepExp (tptr, foldexp (ReplCStepExpOf (tptr)));
			}
		}
		/*}}} */
		/*{{{  Check for replicator negative / overflow */
		if (TagOf (ReplCLengthExpOf (tptr)) == S_CONSTEXP) {	/* if length is constant */
			BOOL toosmall;

			BIT32 *reshi = HiValAddr (ReplCLengthExpOf (tptr)), *reslo = LoValAddr (ReplCLengthExpOf (tptr));

			I32ToI64 (reshi, reslo, *reslo);
			Int64Gt (&toosmall, 0, 0, *reshi, *reslo);
			if (toosmall) {
				if (!current_fe_data->fe_disable_rangechecking_in_branches)
					chkerr (CHK_REPL_NEGATIVE, chklocn);
			} else if (TagOf (ReplCStartExpOf (tptr)) == S_CONSTEXP) {
				const BIT32 start = LoValOf (ReplCStartExpOf (tptr));
				const BIT32 length = LoValOf (ReplCLengthExpOf (tptr));
				const int type = current_fe_data->fe_txlib->bpw == 2 ? S_INT16 : S_INT32;
				BOOL error = FALSE;
				BIT32 temp;
				if (type == S_INT16) {
					/* we know that length isn't negative, so subtracting 1 is OK */
					Int16Minus (&temp, length, 1);
					Int16Add (&error, &temp, temp, start);
				} else {	/* type == S_INT32 */

					/* we know that length isn't negative, so subtracting 1 is OK */
					Int32Minus (&temp, length, 1);
					Int32Add (&error, &temp, temp, start);
				}
				/*printf("type: %d, start: %8X, length: %8X, temp: %8X, error: %d\n",
				   type, start, length, temp, error); */
				if (error)
					chkerr (CHK_REPL_OVERFLOW, chklocn);	/* bug TS/1975 26/11/92 */
			}
		}
		/*}}} */
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));

	DEBUG_MSG (("Checked repl node ok near line %d\n", (int)LocnOf (tptr)));
	return (tptr);
}

/*}}}*/

/*{{{  PRIVATE int csellist (type, tptr)*/
/*****************************************************************************
 *
 *  csellist type checks and constant folds a list of selections 'tptr' of
 *           type 'type', and returns the number of selections if they are
 *           all valid, or -1 if an error is found.
 *
 *****************************************************************************/

/* Type check and constant fold a list of selections */
PRIVATE int csellist (treenode * const type, treenode * tptr)
{
	BOOL alliswell = TRUE;
	int nselections = 0;
	if (TagOf (tptr) == S_ELSE)
		return 0;
	for (; !EndOfList (tptr); tptr = NextItem (tptr))
		/*{{{  type check and constant fold a selection */
	{
		treenode *selection = ThisItem (tptr);
		treenode *const t = typecheck (selection, type);

		if (TagOf (t) == S_UNDECLARED)
			alliswell = FALSE;
		else if (TagOf (t) == TagOf (type))
			/*{{{  constant fold selection */
		{
			selection = foldexp (selection);
			NewItem (selection, tptr);
			if (isconst (selection)) {
				if (TagOf (type) != S_INT64)
					I32ToI64 (HiValAddr (selection), LoValAddr (selection), LoValOf (selection));
			} else {
				alliswell = FALSE;
				chkreport (CHK_SELN_NOTCONST, chklocn);
			}
			nselections++;
		}
		/*}}} */
		else
			chkreport (CHK_INVTYPE_SELN, chklocn);
	}
	/*}}} */
	return (alliswell ? nselections : (-1));
}

/*}}}*/
/*{{{  PRIVATE casenode_t *addcaselist (caseptr, tptr)*/
/*****************************************************************************
 *
 *  addcaselist adds all the selections in the list 'tptr' to the case table
 *              beginning at 'caseptr', updates 'caseptr' and returns it.
 *
 *****************************************************************************/
PRIVATE casenode_t *addcaselist (casenode_t * caseptr, treenode * tptr, BOOL * const elseflag)
{
	SOURCEPOSN locn = LocnOf (tptr);
	if (!EmptyList (tptr)) {
		if (TagOf (tptr) == S_ELSE)
			/*{{{  check its the only one */
		{
			if (*elseflag)
				chkreport (CHK_TOO_MANY_ELSES, chklocn);
			else
				*elseflag = TRUE;
		}
		/*}}} */
		else
			while (!EndOfList (tptr))
				/*{{{  add this selection to casetable, move to next selection */
			{
				treenode *cptr = ThisItem (tptr);
				caseptr->ca_hi = HiValOf (cptr);
				caseptr->ca_lo = LoValOf (cptr);
				/*caseptr->ca_decl = cptr; */
				caseptr->ca_locn = locn;
				caseptr++;	/* Point to the next casenode structure */
				tptr = NextItem (tptr);
			}
		/*}}} */
	}
	return (caseptr);
}

/*}}}*/
/*{{{  PRIVATE int compcases (c1, c2)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  compcases takes two pointers to casenodes, 'c1', 'c2', and returns
 *             1 if case c1 > case c2,
 *             0 if case c1 = case c2,
 *            -1 if case c1 < case c2.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE int compcases (const void *const p1, const void *const p2)
{
	const casenode_t *const c1 = p1;
	const casenode_t *const c2 = p2;
	BOOL greater;
	Int64Gt (&greater, c1->ca_hi, c1->ca_lo, c2->ca_hi, c2->ca_lo);
	if (greater)
		return (1);
	else {
		BOOL equal;
		Int64Eq (&equal, c1->ca_hi, c1->ca_lo, c2->ca_hi, c2->ca_lo);
		if (equal)
			return (0);
		else
			return (-1);
	}
}

/*}}}*/

/*{{{  PUBLIC treenode *ccase(tptr)                    case process*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  ccase performs semantic checking on case tree 'tptr'.
 *
 *        1. Type check the selector, and ensure that it is of a legal type.
 *
 *        2. Check that each selection is a constant expression of the same
 *           type as the selector.
 *
 *        3. Check that all selections are distinct and that there is only one
 *           ELSE.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC treenode *ccase (treenode * volatile tptr)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;
	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0)
		tptr = NULL;
	else {
		treenode *sptr;
		treenode *type;
		BOOL checkdistinct = TRUE;
		int ncases = 0;
		chklocn = LocnOf (tptr);

		/*{{{  selector */
		type = typecheck (LHSOf (tptr), unknownnodeptr);
		/*{{{  check selector type is valid */
		switch (TagOf (follow_user_type (type))) {
		default:
			chkreport (CHK_INVTYPE_SELECTOR, chklocn);
			break;
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
			break;
		case S_UNDECLARED:
			memcpy ((char *) env, (char *) savedenv, sizeof (env));
			return (tptr);	/* Give up early */
		}
		/*}}} */
		/*{{{  fold selector */
		SetLHS (tptr, foldexp (LHSOf (tptr)));
		/*}}} */
		/*}}} */

		/*{{{  selections */
		/*{{{  type check and constant fold selections */
		for (sptr = RHSOf (tptr); !EndOfList (sptr); sptr = NextItem (sptr)) {
			treenode *selection = skipspecifications (ThisItem (sptr));
			int j;
			chklocn = LocnOf (selection);
			j = csellist (type, CondGuardOf (selection));
			if (j < 0)
				checkdistinct = FALSE;
			else
				ncases += j;
		}
		/*}}} */
		if (checkdistinct)
			/*{{{  check selections are distinct and there is only one ELSE */
		{
			BOOL elseflag = FALSE;
			/* make this 1 too large in case of zero length list of selections */
			casenode_t *casetablebase = (casenode_t *) memalloc (sizeof (casenode_t) * (ncases + 1));
			casenode_t *caseptr = casetablebase;

			/*{{{  build up the selections in the case table */
			for (sptr = RHSOf (tptr); !EndOfList (sptr); sptr = NextItem (sptr)) {
				treenode *selection = skipspecifications (ThisItem (sptr));
				chklocn = LocnOf (selection);
				caseptr = addcaselist (caseptr, CondGuardOf (selection), &elseflag);
			}
			/*}}} */
			/*{{{  sort the case table */
			sup_qsort (casetablebase, ncases, sizeof (casenode_t), compcases);
			/*}}} */
			/*{{{  check all selections are distinct */
			{
				int i;
				caseptr = casetablebase;
				for (i = 0; i < (ncases - 1); i++, caseptr++)
					if (compcases (caseptr, caseptr + 1) == 0) {
						/*chklocn = LocnOf(CExpOf(caseptr->ca_decl)); */
						chklocn = caseptr->ca_locn;
						chkreport (CHK_MULTIPLE_CASE, chklocn /*, caseptr->ca_lo */ );
					}
			}
			/*}}} */
			/* If we have an error we aren't going to reach here, so the
			   case table won't be freed up.
			   I don't worry about this, on the grounds that we're never going
			   to reach the backend anyway. */
			memfree (casetablebase);
		}
		/*}}} */
		/*}}} */

		DEBUG_MSG (("Checked case node ok near line %d\n", (int)LocnOf (tptr)));
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));

	return (tptr);
}

/*}}}*/
/*{{{  PUBLIC treenode *ccond(tptr)                    if and while*/
/*****************************************************************************
 *
 *  ccond performs semantic checking on guard tree 'tptr'
 *
 *****************************************************************************/
PUBLIC treenode *ccond (treenode * volatile tptr)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;

	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0)
		tptr = NULL;
	else {
		chklocn = LocnOf (tptr);

#if 0
fprintf (stderr, "ccond: checking condition, guard is:");
printtreenl (stderr, 4, CondGuardOf (tptr));
#endif
		if (!sametype (TagOf (typecheck (CondGuardOf (tptr), boolnodeptr)), S_BOOL))
			chkreport (CHK_INVTYPE_GUARD, chklocn);
		SetCondGuard (tptr, foldexp (CondGuardOf (tptr)));
		DEBUG_MSG (("Checked conditional node ok near line %d\n", (int)LocnOf (tptr)));
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));
	return (tptr);
}

/*}}}*/
/*{{{  PUBLIC treenode *calt(tptr)                     alternative*/
/*****************************************************************************
 *
 *  calt performs semantic checking on alternative tree 'tptr'.
 *
 *****************************************************************************/
/* Type check the guard, make sure it is of type BOOL */
/* The input has already been checked, but we must disallow PORT inputs */
PUBLIC treenode *calt (treenode * volatile tptr)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;
	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0)
		tptr = NULL;
	else {
		chklocn = LocnOf (tptr);

		/*{{{  type check and fold the guard */
		if (AltGuardOf (tptr) != NULL) {
			if (!sametype (TagOf (typecheck (AltGuardOf (tptr), boolnodeptr)), S_BOOL))
				chkreport (CHK_INVTYPE_GUARD, chklocn);
			SetAltGuard (tptr, foldexp (AltGuardOf (tptr)));
		}
		/*}}} */
		/*{{{  check that the input is not a PORT input */
		{
			treenode *const inputptr = AltInputOf (tptr);
			if ((inputptr != NULL) && (TagOf (inputptr) != S_SKIP)) {
				if (basetype (chk_gettype (LHSOf (inputptr))) == S_PORT)
					chkreport (CHK_ALT_INPUT_PORT, chklocn);
			}
		}
		/*}}} */
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));

	return (tptr);
}

/*}}}*/
/*{{{  PUBLIC treenode *cprocessor(tptr)*/
/*****************************************************************************
 *
 *  cprocessor performs semantic checking on processor statement 'tptr'
 *
 *****************************************************************************/
PUBLIC treenode *cprocessor (treenode * volatile tptr)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;

	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0)
		tptr = NULL;
	else {
		treenode *t;
		chklocn = LocnOf (tptr);
		t = typecheck (ProcessorExpOf (tptr), nodenodeptr);
		if (!sametype (TagOf (t), S_NODE))
			chkreport_s (CHK_INVTYPE_PROCEXP, chklocn, tagstring (S_NODE));
		SetProcessorExp (tptr, foldexp (ProcessorExpOf (tptr)));

#if 0				/*ifndef CONFIG */
		ptype = typeofprocessor (WNameOf (ProcessorTypeOf (tptr)));
		if (setprocessor (WNameOf (ProcessorTypeOf (tptr))) == 0) {
			chkreport_s (CHK_INVPROCTYPE, chklocn, WNameOf (ProcessorTypeOf (tptr)));
			tptr = NULL;
		}
#endif
		DEBUG_MSG (("Checked config node ok near line %d\n", (int)LocnOf (tptr)));
	}

	memcpy ((char *) env, (char *) savedenv, sizeof (env));
	return (tptr);
}

/*}}}*/
/*{{{  PUBLIC treenode *callocation(tptr)*/
/*****************************************************************************
 *
 *  callocation performs semantic checking on allocation tree 'tptr'
 *
 *****************************************************************************/
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
PUBLIC treenode *callocation (treenode *volatile tptr, treenode *last_decl)
{
	jmp_buf savedenv;
	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0) {
		tptr = NULL;
	} else {
		chklocn = LocnOf (tptr);
		switch (TagOf (tptr)) {
			/*{{{  S_PLACE                     break */
		case S_PLACE:
			/* check lhs is variable, channel, port, timer, or array */
			/* check rhs is of type INT */
			{
				treenode *t;
				treenode *pname = DNameOf (tptr);

				if (TagOf (pname) != N_DECL) {
					chkreport_s (CHK_BAD_PLACE_NAME, chklocn, WNameOf (NNameOf (pname)));
				}
				if (NModeOf (pname) != NM_DEFAULT) {
					chkreport_s (CHK_NAME_MULTIPLY_PLACED, chklocn, WNameOf (NNameOf (pname)));
				}

				t = typecheck (DValOf (tptr), intnodeptr);

				if ((TagOf (t) == S_INT) || (TagOf (t) == S_INT16) || (TagOf (t) == S_UINT) || (TagOf (t) == S_UINT16)) {
					SetDVal (tptr, foldexp (DValOf (tptr)));

					if (isconst (DValOf (tptr))) {
						/*{{{  constant PLACEment -- can only work for port-based I/O */
						treenode *ptype = NTypeOf (pname);

						while (TagOf (ptype) == S_ARRAY) {
							ptype = ARTypeOf (ptype);
						}

						SetDVal (tptr, newconstexp (DValOf (tptr)));
						SetTypeAttr (NTypeOf (pname), TypeAttrOf (NTypeOf (pname)) | TypeAttr_placed);
						/*}}}*/
					} else {
						/*{{{  non-constant PLACEment */
#if 0
fprintf (stderr, "callocation: non-constant PLACE, node is");
printtreenl (stderr, 4, tptr);
#endif
						/* if (TagOf (NTypeOf (pname)) != S_CHAN) {
							chkreport (CHK_INVTYPE_PLACEMENT, chklocn);
						} */
						if (nodetypeoftag (TagOf (NTypeOf (pname))) == TYPENODE) {
							if (no_placed_chans && (TagOf (NTypeOf (pname)) == S_CHAN)) {
								chkreport (CHK_INV_CHAN_PLACEMENT, chklocn);
							} else {
								SetTypeAttr (NTypeOf (pname), TypeAttrOf (NTypeOf (pname)) | TypeAttr_placed);
							}
						} else {
							chkreport (CHK_INV_VAR_PLACEMENT, chklocn);
						}
						/*}}}  */
					}
				} else if (TagOf (t) != S_UNDECLARED) {
					chkreport (CHK_INVTYPE_PLACEMENT, chklocn);
				}
			}
			break;
			/*}}} */
			/*{{{  S_WSPLACE S_VSPLACE         break */
		case S_WSPLACE:
		case S_VSPLACE:
			/* Check placed object is a variable array */
			{
				treenode *pname = DNameOf (tptr);
				/*{{{  check its a variable */
				if (TagOf (pname) != N_DECL)
					chkreport_s (CHK_BAD_PLACE_NAME, chklocn, WNameOf (NNameOf (pname)));
				/*}}} */
				/*{{{  check its an array if PLACEd IN */
				if (DValOf (tptr) == NULL)
					switch (chk_typeof (pname)) {
					case S_ARRAY:
#ifdef OCCAM2_5
					case S_RECORD:
#endif
						break;
					default:
						chkreport_s (CHK_BAD_WSPLACE, chklocn, WNameOf (NNameOf (pname)));
						break;
					}
				/*}}} */
				/*{{{  check it hasn't already been placed */
				if (NModeOf (pname) != NM_DEFAULT)
					chkreport_s (CHK_NAME_MULTIPLY_PLACED, chklocn, WNameOf (NNameOf (pname)));
				/*}}} */

				if (TagOf (tptr) == S_WSPLACE) {
					if (DValOf (tptr) != NULL)
						/*{{{  it's at a specific position */
					{
						treenode *t = typecheck (DValOf (tptr), intnodeptr);

						if (TagOf (t) == S_INT)
							/*{{{  fold the place expression */
						{
							INT32 placeaddr;
							SetDVal (tptr, newconstexp (DValOf (tptr)));
							placeaddr = LoValOf (DValOf (tptr));
							SetNMode (pname, NM_WSPLACED);
							SetNVOffset (pname, placeaddr);
						}
						/*}}} */
						else if (TagOf (t) != S_UNDECLARED)
							chkreport (CHK_INVTYPE_PLACEMENT, chklocn);
					}
					/*}}} */
					else
						SetNMode (pname, NM_WORKSPACE);
				}
				/* else TagOf(tptr) == S_VSPLACE */
				else
					SetNMode (pname, NM_VECSPACE);
			}
			break;
			/*}}} */
			/*{{{  S_PLACEON (CONFIG only) */
		case S_PLACEON:
			{
				treenode *t;
				treenode *source = DNameOf (tptr);
				while (!EndOfList (source)) {
					t = typecheck (ThisItem (source), channodeptr);
					if (!sametype (TagOf (t), S_CHAN))
						chkreport_s (CHK_INV_MAPPING_LHS, LocnOf (tptr), tagstring (S_CHAN));
					NewItem (foldexp (ThisItem (source)), source);
					source = NextItem (source);
				}
				t = typecheck (DValOf (tptr), arcnodeptr);
				if (!sametype (TagOf (t), S_ARC)) {
					chkreport (CHK_INV_PLACEON_RHS, LocnOf (tptr));
				}
				SetDVal (tptr, foldexp (DValOf (tptr)));
			}
			break;
			/*}}} */
		}
		/*{{{  check for bad PLACEments */
		{
#ifdef OCCAM2_5
			const err_severity_t sev = SEV_ERR;
#else
			const err_severity_t sev = SEV_WARN;
#endif
			treenode *namelist = DNameOf (tptr);
			if (TagOf (namelist) == S_LIST) {
				for (; !EndOfList (namelist); namelist = NextItem (namelist)) {
					check_adjacent_name (LocnOf (tptr), ThisItem (namelist), last_decl, NULL, sev);
				}
			} else {
				check_adjacent_name (LocnOf (tptr), namelist, last_decl, NULL, sev);
			}
		}
		/*}}} */
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));

	DEBUG_MSG (("Checked placement node ok near line %d\n", (int)LocnOf (tptr)));
	return (tptr);
}

/*}}}*/
/*{{{  PUBLIC treenode *cinstance(tptr)                procedure instance*/
/*****************************************************************************
 *
 *  cinstance performs semantic checking on instance tree 'tptr'
 *
 *****************************************************************************/
PUBLIC treenode *cinstance (treenode * volatile tptr)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;
	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0) {
		tptr = NULL;
	} else {
		treenode *const name = INameOf (tptr);

#if 0
fprintf (stderr, "cinstance: checking instance node");
printtreenl (stderr, 4, tptr);
#endif
		chklocn = LocnOf (tptr);
		checkparams (tptr, S_PROC);
		SetIParamList (tptr, foldexplist (IParamListOf (tptr)));

		if ((TagOf (name) == N_PREDEFPROC) && (NModeOf (name) == PD_ASSERT)) {
			treenode *const param = ThisItem (IParamListOf (tptr));
			if (isconst (param) && (LoValOf (param) == 0))
				chkreport (CHK_INV_ASSERT, chklocn);
		}
		DEBUG_MSG (("Checked instance node ok near line %d\n", (int)LocnOf (tptr)));
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));

	return (tptr);
}
/*}}}*/

/*{{{  PUBLIC treenode *cvalof*/
typedef struct {
	BIT32 scope;
	BOOL nestedproc;
	SOURCEPOSN locn;
} chkvalof_t;

/*{{{  PRIVATE void vscope (tptr, scope, locn, e)*/
/*****************************************************************************
 *
 *  vscope checks that the element 'tptr' is not in scope.
 *
 *****************************************************************************/

/* Error number */
PRIVATE void vscope (treenode * tptr, const SOURCEPOSN locn, const chkvalof_t * const chkvalof_data)
{
	tptr = nameof (tptr);
	if ((nodetypeoftag (TagOf (tptr)) == NAMENODE) &&	/* bug INSdi01253 */
	    (TagOf (NTypeOf (tptr)) != S_UNDECLARED) && (NScopeOf (tptr) < chkvalof_data->scope)) {
		if (chkvalof_data->nestedproc)
			chkreport_s (CHK_VALOF_CALL_SCOPE, chkvalof_data->locn, WNameOf (NNameOf (tptr)));
		else
			chkreport_s (CHK_VALOF_SCOPE, locn, WNameOf (NNameOf (tptr)));
	}
	return;
}

/*}}}*/
/*{{{  PRIVATEPARAM void do_checkvalof (tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  checkvalof takes a VALOF tree 'tptr' and checks
 *             (i)  that it does not contain ALT, PAR, input or output
 *             (ii) that all variables written to (lhs of assignment,
 *                  rhs of var. abbreviation or retype, var. params) are
 *                  in scope 'scope'.
 *
 *****************************************************************************/
/*{{{  note on VALOF scoping*/
/* The 'scope' parameter is the scope that we are checking; all variables
   assigned to must have a scope number greater than or equal to this value.
   We check that procedures which are called do not access local variables
   by recursively calling checkvalof on the body of the procedure with
   the scope of the procedure.
 */
/*}}}*/
/*}}}*/
PRIVATEPARAM int do_checkvalof (treenode * tptr, void *const voidptr)
{
	const chkvalof_t *const data = voidptr;
	switch (TagOf (tptr)) {
		/*{{{  default */
	default:
		break;
		/*}}} */
		/*{{{  PLACE WSPLACE VSPLACE */
	case S_PLACE:
	case S_WSPLACE:
	case S_VSPLACE:
	case S_PLACEON:

		if (TagOf (tptr) == S_PLACE) {	/* bug TS/1451 21/08/92 */
			if (data->nestedproc)
				msg_out_s (SEV_WARN, CHK, CHK_PLACE_IN_VALOF_CALL, data->locn, WNameOf (NNameOf (DNameOf (tptr))));
			else
				msg_out_s (SEV_WARN, CHK, CHK_PLACE_IN_VALOF, LocnOf (tptr), WNameOf (NNameOf (DNameOf (tptr))));
		}

		vscope (DNameOf (tptr), LocnOf (tptr), data);
		break;
		/*}}} */
		/*{{{  PINSTANCE */
	case S_PINSTANCE:
		/* Recursively check the procedure body with the procedure scope */
		/* Check that var params are in the VALOF scope */
		{
			treenode *actparamlist = IParamListOf (tptr);
			treenode *iname = INameOf (tptr);
			treenode *fparamlist = NTypeOf (iname);
			if (!separatelycompiled (iname) && TagOf (iname) != N_PREDEFPROC)
				/*{{{  check instanced procedure */
			{
				chkvalof_t chkvalof_data;
				/* This breaks if the PROC is declared INSIDE the valof, and accesses
				   a 'free' variable which is also declared INSIDE the VALOF.
				   We fix it by taking the outermost scope of the two */
				/*chkvalof_data.scope      = NScopeOf(iname); */
				chkvalof_data.scope = min_INT32 (data->scope, NScopeOf (iname));
				chkvalof_data.nestedproc = TRUE;
				chkvalof_data.locn = LocnOf (tptr);
				prewalkproctree (DValOf (NDeclOf (iname)), do_checkvalof, &chkvalof_data);
			}
			/*}}} */

			while (!EndOfList (fparamlist) && (TagOf (fparamlist) != S_UNDECLARED)) {
				if ((TagOf (ThisItem (fparamlist)) == N_PARAM) || (TagOf (ThisItem (fparamlist)) == N_RESULTPARAM)) {
					vscope (ThisItem (actparamlist), LocnOf (tptr), data);
				}

				fparamlist = NextItem (fparamlist);
				actparamlist = NextItem (actparamlist);
			}
		}
		break;
		/*}}} */
		/*{{{  ASS */
	case S_ASS:
		/* Check that each lhs element is in the VALOF scope */
		{
			treenode *lhs = LHSOf (tptr);
			if (TagOf (lhs) == S_LIST)
				for (; !EndOfList (lhs); lhs = NextItem (lhs))
					vscope (ThisItem (lhs), LocnOf (tptr), data);
			else
				vscope (lhs, LocnOf (tptr), data);
		}
		break;
		/*}}} */
		/*{{{  ABBR RETYPE */
	case S_ABBR:
	case S_RETYPE:
		vscope (DValOf (tptr), LocnOf (tptr), data);
		break;
		/*}}} */
		/*{{{  pars, inputs, outputs and alts */
	case S_X_INPUT:
	case S_INPUT:
	case S_OUTPUT:
	case S_CASE_INPUT:
	case S_X_CASE_INPUT:
	case S_DELAYED_INPUT:
	case S_TAGGED_INPUT:
	case S_X_TAGGED_INPUT:
	case S_ALT:
	case S_REPLALT:
	case S_PRIALT:
	case S_PRIREPLALT:

	case S_PAR:
	case S_REPLPAR:	/* bug TS/1623 09/04/92 */
	case S_PRIPAR:
	case S_PRIREPLPAR:
	case S_PLACEDPAR:
	case S_PLACEDREPLPAR:
		{
			char name[MAX_ERR_SIZE];
			ftagstring (name, TagOf (tptr), NULL, 0, NULL);
			if (data->nestedproc)
				chkreport_s (CHK_BAD_VALOF_CALL, data->locn, name);
			else
				chkreport_s (CHK_BAD_VALOF, LocnOf (tptr), name);
		}
		return STOP_WALK;
		/*}}} */
	}
	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PUBLIC void check_valof_types*/
PUBLIC void check_valof_types (treenode * const valof)
{
#ifdef OCCAM2_5
	USE_VAR (valof);
#else
	treenode *resultlist;
	for (resultlist = VLResultListOf (valof); !EndOfList (resultlist); resultlist = NextItem (resultlist)) {
		/* This test added 4/11/91 for bug TS/1442 */
		if (!isscalartype_tree (chk_gettype (ThisItem (resultlist))))
			chkreport (CHK_BAD_VALOF_TYPE, LocnOf (ThisItem (resultlist)));
	}
#endif
}

/*}}}*/
/*{{{  PUBLIC treenode *cvalof(tptr, scope)            valof*/
/*****************************************************************************
 *
 *  cvalof performs semantic checking on valof tree 'tptr'.
 *
 *****************************************************************************/

/*{{{  comment*/
/* Check that the VALOF (and immediately preceding specifications)
does not contain ALT, PAR, input or output
Check that all writeable variables (lhs of assignment, rhs of
var abbreviation or retype, and var params) are within the VALOF scope.
*/
/*{{{  COMMENT */
/**********************  Start comment out ****************************
|*{{{  *|
|* VALOF scoping is enforced in a rather obscure way: when cvalof is called
all names declared within and immediately before the VALOF (ie.
writeable variables) have been
descoped.  We go through the valof looking up all writeable variables
on the scope stack: if any are found they must be out of the VALOF
scope.
*|
|*}}}*|
**********************   End comment out  ****************************/
/*}}}*/
/* When cvalof is called all names declared within and immediately before
the VALOF (ie. writeable variables) have been
descoped. We check that all variables assigned within the VALOF have
a scope greater than or equal to the current scope.
*/
/*}}}*/
PUBLIC treenode *cvalof (treenode * volatile tptr, BIT32 scope)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;
	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0)
		tptr = NULL;
	else {
		chkvalof_t chkvalof_data;

		chklocn = LocnOf (tptr);

		chkvalof_data.scope = scope;
		chkvalof_data.nestedproc = FALSE;
		chkvalof_data.locn = NOPOSN;

		/*{{{  comment */
      /*****************************************************************************
       *
       *  checkvalof takes a VALOF tree 'tptr' and checks
       *             (i)  that it does not contain ALT, PAR, input or output
       *             (ii) that all variables written to (lhs of assignment,
       *                  rhs of var. abbreviation or retype, var. params) are
       *                  in scope 'scope'.
       *
       *****************************************************************************/
		/*{{{  note on VALOF scoping */
		/* The 'scope' parameter is the scope that we are checking; all variables
		   assigned to must have a scope number greater than or equal to this value.
		   We check that procedures which are called do not access local variables
		   by recursively calling checkvalof on the body of the procedure with
		   the scope of the procedure.
		 */
		/*}}} */
		/*}}} */
		prewalkproctree (tptr, do_checkvalof, &chkvalof_data);
		/*{{{  comment */
		/* bug TS/1442: 4/11/91 */
		/* I can't call check_valof_types here, because it hasn't all been through
		   typecheck_main yet.
		   This shows itself because UINTLITs haven't been turned into INTLITs etc
		   yet, so typeof crashes.
		   We have to delay the check for arrays being returned from VALOFs till
		   either typecheck_main itself, which is OK for single-result VALOFs,
		   or in 'caction' for multiple-result valofs, which can only appear
		   inside multiple assignments.
		   Conor O'Neill, 4/11/91
		 */
		/*}}} */

		DEBUG_MSG (("Checked valof node ok near line %d\n", (int)LocnOf (tptr)));
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));

	return (tptr);
}

/*}}}*/
/*}}}*/

/*{{{  PRIVATE void checkprimary*/
PRIVATE void checkprimary (const BOOL guy_not_asm, const BOOL addressof,
			   const BOOL loading_address, treenode * tptr, const SOURCEPOSN locn, treenode * const current_proc)
/* This checks that tptr is a valid primary operand, or op to ADDRESSOF */
{
	while (TRUE) {
		/*{{{  switch (TagOf(tptr)) */
		switch (TagOf (tptr)) {
		default:
			/* Allow expressions involving Special names in ASM primaries only */
			if (!guy_not_asm && !addressof && isscalartype (chk_typeof (tptr)) && wouldbeconst (tptr));	/* skip */
			else {
#if 0
fprintf (stderr, "chk2: checkprimary: operand (1) is: ");
printtreenl (stderr, 4, tptr);
#endif
				chkreport (CHK_BAD_GUY_OPERAND, locn);
			}
			return;
		case S_CONSTEXP:
		case N_DECL:
		case N_REPL:
		case N_ABBR:	/* bug 594 26/9/91 always OK cos always must be a variable */
		case N_RETYPE:
		case N_VALRETYPE:	/* bug 594 - give up on these! 26/9/91 */
			return;
		case N_PARAM:
		case N_RESULTPARAM:
		case N_VALPARAM:	/* bug TS/1409 19/05/92 */
			if (!addressof && isinline (current_proc) && !NVAssumeConstOf (tptr)) {	/* bug TS/2015 08/01/93 */
				chkreport_s (CHK_BAD_ASM_INLINE_OP, locn, WNameOf (NNameOf (tptr)));
			}
			return;
		case N_VALABBR:
#if 0
			/* this breaks some examples: Eg we should allow
			   LDL x, where VAL x IS array[i] :
			   But we should disallow:
			   LDL x, where VAL x IS "fred" :
			 */
			tptr = DValOf (NDeclOf (tptr));	/* bug 594 2/11/90 */
			break;
#else
			if (isconst (DValOf (NDeclOf (tptr)))) {	/* bug 594 26/09/91 */
				/* if it were a scalar constant, it would have been folded, so
				   we can assume that it is an array constant.
				   We can't do anything except load its address.
				 */
				if (!addressof && !loading_address) {
#if 0
fprintf (stderr, "chk2: checkprimary: operand (2) is: ");
printtreenl (stderr, 4, tptr);
#endif
					chkreport (CHK_BAD_GUY_OPERAND, locn);
				}
			}
			return;
#endif
		case S_ARRAYSUB:
		case S_RECORDSUB:
			/* Variable subscripts are OK in addressof */
			if (addressof)
				return;
			/* Constant subscripts in GUY, none at all in ASM */
			if (!guy_not_asm || !isconst (ASIndexOf (tptr))) {
#if 0
fprintf (stderr, "chk2: checkprimary: operand (3) is: ");
printtreenl (stderr, 4, tptr);
#endif
				chkreport (CHK_BAD_GUY_OPERAND, locn);
			}
			tptr = ASBaseOf (tptr);
			break;
		case S_CONSTCONSTRUCTOR:
		case S_STRING:	/* added bug 594 2/11/90 */
			if (addressof)
				return;
			if (guy_not_asm && loading_address)
				return;
			chkreport (CHK_BAD_GUY_OPERAND, locn);
			return;
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PRIVATE void checkinstruction*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  checkinstruction takes a GUYCODE or GUYSTEP node and checks that the
 *                   instruction is valid, and inserts the instruction value
 *                   into the node.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void checkinstruction (treenode *tptr, const BOOL guy_not_asm,
		  BOOL *primary,
		  BOOL *byte_directive, BOOL *word_directive,
		  BOOL *loading_address, BOOL *ptrs_allowed, BOOL *element_required, BOOL *labels_permitted, BOOL *const_required)
{
	/* add 1 to skip the leading '.' */
	const char *const instruction = WNameOf ((wordnode *) LeftOpOf (tptr)) + 1;
	int err, required_operands;
	int base_instruction;
	const int ival = (int) (current_fe_data->fe_check_asm_fn) (instruction, guy_not_asm,
								   *current_fe_data->fe_guyinserts,
								   &err, &required_operands,
								   &base_instruction,
								   primary,
								   byte_directive, word_directive,
								   loading_address,
								   ptrs_allowed, element_required, labels_permitted, const_required);
	const int operands = listitems (RightOpOf (tptr));	/* number of operands */

	/*{{{  check for errors */
	switch (err) {
	case 4:
		chkerr_s (CHK_UNIMPLEMENTED_ASMCODE, chklocn, instruction);
		break;
	case 2:
		msg_out_s (current_fe_data->fe_warning_bad_asm ? SEV_WARN : SEV_ERR, CHK, CHK_DISABLED_GUYCODE, chklocn, instruction);
		break;
	case 3:
		msg_out_s (current_fe_data->fe_warning_bad_asm ? SEV_WARN : SEV_ERR, CHK, CHK_INVALID_GUYCODE, chklocn, instruction);
		break;
	case 1:
		chkerr_s (CHK_BAD_GUYCODE, chklocn, instruction);
		break;
	}
	/*}}} */

	switch (err) {
		/*{{{  errors */
	case 4:		/* not implemented (eg ALIGN) */
	case 1:		/* unknown code */
		break;
		/*}}} */

	default:
		/* if error code is 2 (disabled instruction), or
		   3 (wrong target)
		   continue anyway
		 */
		if (((operands < required_operands) && (required_operands >= 0)) || ((operands == 0) && (required_operands < 0))) {
			chkerr_s (CHK_MISSING_OPERAND, chklocn, instruction);
		} else if ((operands > required_operands) && (required_operands >= 0)) {
			chkerr_s (CHK_EXTRANEOUS_OPERAND, chklocn, instruction);
		} else {
			SetDOpType (tptr, ival);
		}
		break;
	}
	return;
}

/*}}}*/
/*{{{  PUBLIC treenode *cguy_or_asm(tptr, guy_not_asm)           guycode*/
/*****************************************************************************
 *
 *  cguy_or_asm performs semantic checking on guycode tree tptr
 *
 *****************************************************************************/
PUBLIC treenode *cguy_or_asm (treenode * volatile tptr, const BOOL guy_not_asm, treenode * const current_proc)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;
	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0) {
		tptr = NULL;
	} else {
		BOOL element_required, ptrs_allowed, labels_permitted;
		BOOL primary, byte_directive, word_directive, loading_address;
		BOOL const_required;	/* bug TS/1166 14/04/92 */

		chklocn = LocnOf (tptr);
		if (TagOf (tptr) == S_GUYCODE) {
			/*{{{  S_GUYCODE */
			treenode *operand;
			checkinstruction (tptr, guy_not_asm,
					  &primary, &byte_directive, &word_directive,
					  &loading_address, &ptrs_allowed, &element_required, &labels_permitted, &const_required);

			for (operand = RightOpOf (tptr); !EndOfList (operand) && (TagOf (operand) == S_LIST); operand = NextItem (operand)) {
				/*{{{  type check and constant fold the list of operands */
				treenode *thisnode = ThisItem (operand);
				const BOOL addressof = TagOf (thisnode) == S_ADDRESSOF;
				treenode *thisop = addressof ? OpOf (thisnode) : thisnode;

				/*{{{  check for ADDRESSOF */
				if (addressof && !ptrs_allowed) {
#if 0
fprintf (stderr, "chk2: checkprimary: operand (4) is: ");
printtreenl (stderr, 4, operand);
#endif
					chkreport (CHK_BAD_GUY_OPERAND, chklocn);
				}
				/*}}} */
				if (thisop != NULL) {
					switch (TagOf (thisop)) {
					case N_LABELDEF:
					case N_PROCDEF:
#ifdef MOBILES
					case N_MPROCDECL:
#endif
					case N_LIBPROCDEF:	/* bug TS/1240 14/04/92 */
					case N_LIBMPROCDECL:
					case N_LFUNCDEF:
					case N_SFUNCDEF:
					case N_LIBFUNCDEF:
						if (labels_permitted	/* bug TS/1523 12/12/91 */
						    || addressof) {	/* bug TS/1240 12/05/92 */
							;	/* skip */
						} else {
#if 0
fprintf (stderr, "chk2: checkprimary: operand (4) is: ");
printtreenl (stderr, 4, operand);
#endif
							chkreport (CHK_BAD_GUY_OPERAND, chklocn);
						}
						break;
					default:
						typecheck (thisop, unknownnodeptr);
						/*{{{  constant fold the expression */
						thisop = foldexp (thisop);
						if (addressof) {
							SetOp (thisnode, thisop);	/* replace the target of addressof */
						} else {
							NewItem (thisop, operand);	/* replace the whole operand */
						}
						/*}}} */
						if (guy_not_asm) {	/* do all the old style checks */
							checkprimary (guy_not_asm, FALSE, loading_address, thisop, chklocn, current_proc);
						} else {
							/*{{{  do the ASM checks */
							const int bytes = bytesinscalar (TagOf (chk_gettype (thisop)));

							/* Check for stores to expressions */
							if (element_required && !iselement (thisop, FALSE)) {
								chkreport (CHK_ASM_BAD_STORE, chklocn);
							}

							/* Operands to primaries, or of ADDRESSOF, must be constant or name */
							if (primary || addressof) {
								checkprimary (guy_not_asm, addressof, loading_address, thisop, chklocn, current_proc);
							}

							/* Check that loads and stores fit in a word */
							if ((ptrs_allowed || element_required) &&
							    (!addressof) &&
							    ((bytes < 0) || (bytes > current_fe_data->fe_txlib->bpw))) {
								chkreport (CHK_ASM_TOO_BIG, chklocn);
							}

							if (const_required) {
								if (!isconst (thisop)) {
									chkreport (CHK_EXP_NOT_CONST, chklocn);
								}

								/* Check that the operand to BYTE or WORD is constant */
								if (byte_directive || word_directive) {
									if (basetype (chk_gettype (thisop)) != (byte_directive ? S_BYTE : S_INT)) {
										chkreport (CHK_TYPE_MISMATCH, chklocn);
									}
								} else if (chk_typeof (thisop) != S_INT) {
									chkreport_s (CHK_TYPE_MISMATCH_EXPECTED, chklocn, tagstring (S_INT));
								}
							}
							/*}}} */
						}
						break;
					}
				}
				/*}}} */
			}
			/*}}}*/
		} else {
			/*{{{  S_GUYSTEP  -- only for GUY code */
			checkinstruction (tptr, guy_not_asm,
					  &primary, &byte_directive, &word_directive,
					  &loading_address, &ptrs_allowed, &element_required, &labels_permitted, &const_required);
			/*}}}*/
		}
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));

	return (tptr);
}

/*}}}*/


