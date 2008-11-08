/* $Id: tran3.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	tree transformer; array subscript processing
 *	Copyright (C) 1994 Inmos Limited
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
#include "midinc.h"
#include "occompfe.h"
#include "harndef.h"

#include "trandef.h"
#include "tran1def.h"
/*}}}*/

/*{{{  PUBLIC  treenode *add_to */
/*****************************************************************************
 *
 *  add_to combines a new node, 'newnode', with an expression tree 't', using
 *         dyadic operator 'doptag'.  If the expression tree is NULL, then
 *         just the new node is returned.
 *
 *****************************************************************************/
PUBLIC treenode *add_to (treenode *const t, const int doptag, treenode *newnode)
{
	if (t != NULL) {
		newnode = newdopnode (doptag, NOPOSN, t, newnode, S_INT);
	}
	return (newnode);
}

/*}}}*/
/*{{{  PUBLIC  treenode *scaletreeof */
/*{{{  comment */
/*****************************************************************************
 *
 *  scaletreeof takes a type tree 'type' and returns an expression tree
 *              which generates the number of elements of the same size as
 *              base (where base is a scalar type) in 'type'.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC treenode *scaletreeof (treenode *type, const int base_bytes, const int lexlevel)
{
	INT32 constantpart = 1;
	treenode *t = NULL;
	type = follow_user_type (type);
#ifdef MOBILES
	if (TagOf (type) == S_MOBILE) {
		type = MTypeOf (type);
	}
#endif
	while (TagOf (type) == S_ARRAY) {
		if (ARDimOf (type) == (-1)) {
			/* copytree added for bug TS/1686, TS/1172, TS/1626 06/05/92 */
			t = add_to (t, S_TIMES, copytree (ARDimLengthOf (type), lexlevel));
		} else {
			constantpart *= ARDimOf (type);
		}
		type = follow_user_type (ARTypeOf (type));
#ifdef MOBILES
		if (TagOf (type) == S_MOBILE) {
			type = MTypeOf (type);
		}
#endif
	}

#if 0
fprintf (stderr, "tran3: scaletreeof: type = ");
printtreenl (stderr, 4, type);
#endif
	constantpart *= (bytesin (type) / base_bytes);

	if ((constantpart != 1) || (t == NULL)) {
		t = add_to (t, S_TIMES, newconstant (constantpart));
	}
	return (t);
}

/*}}}*/
/*{{{  PUBLIC  treenode *unknowndimsof */
PUBLIC treenode *unknowndimsof (treenode * type)
{
	treenode *t = NULL;

#ifdef MOBILES
	if (TagOf (type) == S_MOBILE) {
		type = MTypeOf (type);
	}
#endif
	for (; TagOf (type) == S_ARRAY;) {
		if (ARDimOf (type) == (-1)) {
			t = add_to (t, S_TIMES, ARDimLengthOf (type));
		}
		type = ARTypeOf (type);
#ifdef MOBILES
		if (TagOf (type) == S_MOBILE) {
			type = MTypeOf (type);
		}
#endif
	}
	return (t);
}

/*}}}*/

/*{{{  PRIVATE BOOL can_use_virtual_origin */
PRIVATE BOOL can_use_virtual_origin (treenode *subscript)
/* This returns TRUE if the subscript expression is of the form
   expression + constant, expression - constant, or constant + expression.
   This means that we can 'fold' out the constant into the array base address
   calculation; this is known as using a virtual origin for the array.
   CON 9/2/91.

   We do this when rangechecking is off, so that we needn't worry
   about its effects on subscript checking, etc.

   NOTE: If we determine that the result is TRUE, we `normalise' the subscript
   tree to make it expression + constant.
   (IE - we move the constant to the rhs, and make it always an addition).
*/
{
	if (NEED_ERRORS);	/* skip */
	else if (TagOf (subscript) == S_ADD) {
		treenode *l;
		treenode *r = RightOpOf (subscript);
		if (isconst (r))
			return TRUE;
		l = LeftOpOf (subscript);
		if (isconst (l)) {
			/* first we `normalise' the subscript tree, by moving the constant
			   to the right hand side
			 */
			SetRightOp (subscript, l);
			SetLeftOp (subscript, r);
			return TRUE;
		}
	} else if (TagOf (subscript) == S_SUBTRACT) {
		treenode *r = RightOpOf (subscript);
		if (isconstexpnd (r)) {
			/* we normalise the rhs to be an addition of the negated value */
			/* We rely here on the fact that a subscript expression
			   MUST be an integer, so we needn't bother with worrying
			   about the high part of the constant.
			 */
			SetTag (subscript, S_ADD);
			SetLoVal (r, ((BIT32) 0 - (BIT32) LoValOf (r)));
			return TRUE;
		}
	}
	return FALSE;
}

/*}}}*/
/*{{{  PRIVATE BOOL repl_for_size_array */
PRIVATE BOOL repl_for_size_array (treenode * tptr, treenode * size_exp)
{
	/* checks to see if len / subscript are of the form
	   SIZE array, SIZE array[0], SIZE array[0][0]
	   a[i],       a[...][i],     a[...][...][i]
	 */
	while (TRUE) {
		treenode *base = ASBaseOf (tptr);
		switch (nodetypeoftag (TagOf (size_exp))) {
		case NAMENODE:
			DEBUG_MSG (("repl_for_size_array: true?: %d\n", size_exp == base));
			return size_exp == base;

		case ARRAYSUBNODE:
			if ((nodetypeoftag (TagOf (base)) == ARRAYSUBNODE) &&
			    isconstexpnd (ASIndexOf (size_exp)) && (LoValOf (ASIndexOf (size_exp)) == 0)) {
				DEBUG_MSG (("repl_for_size_array: looping\n"));
				size_exp = ASBaseOf (size_exp);
				tptr = ASBaseOf (tptr);
			} else
				return FALSE;
			break;
		default:
			return FALSE;
		}
	}
}

/*}}}*/
/*{{{  PRIVATE BOOL range_check_replicator */
PRIVATE BOOL range_check_replicator (treenode * tptr, treenode * const repl, treenode * const dim, const BOOL dimisconst)
/* Optimisation for subscripting by replicators: */
{
	BOOL no_check_needed = FALSE;

	treenode *const start = ReplCStartExpOf (repl);
	treenode *const len = ReplCLengthExpOf (repl);

	if (dimisconst) {
		DEBUG_MSG (("range_check_replicator: checking for subscript by constant replicator\n"));
		no_check_needed = isconstexpnd (start) && isconstexpnd (len) && ((INT32) LoValOf (start) >= 0) &&	/* not negative */
			/* (we know that dim>=0 and len>=0) */
			(LoValOf (start) <= LoValOf (dim)) &&
			/* This line prevents the addition overflowing: */
			(LoValOf (len) <= LoValOf (dim)) && (LoValOf (start) + LoValOf (len) <= LoValOf (dim));
	} else {		/* bug TS/1909 23/10/92 */
		DEBUG_MSG (("range_check_replicator: checking for subscript by in-range repl\n"));
		no_check_needed = isconstexpnd (start) && (LoValOf (start) == 0) && (TagOf (len) == S_SIZE) && repl_for_size_array (tptr, OpOf (len));
	}

	return !no_check_needed;
}

/*}}}*/

/*{{{  PRIVATE treenode *calc_array_subscript */
PRIVATE treenode *calc_array_subscript (BIT32 *const offset_to_add, treenode *tptr, const BOOL rangechecking, const int my_lexlevel)
{
	treenode *const base = ASBaseOf (tptr);
	treenode *dim;
	BOOL dimisconst;
	treenode *exp_to_add = NULL;
	treenode *r = ASIndexOf (tptr);
#ifdef MOBILES
	(void)isdynmobilearray (base);		/* sets up NTH_DIMENSION if needed */
#endif

	dim = dimexpof (base, 0);
	dimisconst = isconst (dim);
	if (!dimisconst) {
		/*{{{  set up ELSIZE node */
/*shouldn't that be copytree(base, my_lexlevel) ? *//* CON - 24/1/91 */
		/* NO - this breaks all sorts of stuff. We capitulate, and state that
		   S_ELSIZE nodes are special and do not obey these rules! - CON 7/2/91 bug 1142 */
		dim = newmopnode (S_ELSIZE, LocnOf (tptr), base, S_INT);
	}
	/*}}} */
#if 0
fprintf (stderr, "calc_array_subscript().  base = ");
printtreenl (stderr, 4, base);
fprintf (stderr, "  calc_array_subscript:  dimisconst = %d, dim = ", dimisconst);
printtreenl (stderr, 4, dim);
fprintf (stderr, "  calc_array_subscript: tptr = ");
printtreenl (stderr, 4, tptr);
#endif
#if 0
fprintf (stderr, "calc_array_subscript(): dim is");
printtreenl (stderr, 0, dim);
fprintf (stderr, "OpOf(dim) = %p\n", OpOf (dim));
#endif

	if (isconstexpnd (r) && (dimisconst || !rangechecking)) {
		/*{{{  right is constant, and dimisconst, offset_to_add = right */
		/* rangechecking test added 24/1/91 for bug 1125 */
		/*(isconst(r) && dimisconst) */
		*offset_to_add = LoValOf (r);	/* subscript is constant */
		/*}}} */
	} else if (can_use_virtual_origin (r)) {
		/* Optimise a[i+1] */
		/*{{{  add expression and offset */
		*offset_to_add = LoValOf (RightOpOf (r));	/* add the constant */
		exp_to_add = copytree (LeftOpOf (r), my_lexlevel);
		/*}}} */
	} else {
		/*{{{  exp_to_add = checked right */
		treenode *skipped_r;
		r = copytree (r, my_lexlevel);	/* bug TS/1588 20/05/92 */
		skipped_r = skipevals (r);

		if (rangechecking && (!isconst (skipped_r) || !dimisconst)) {
			/*{{{  r = checked right */
			BOOL check_needed = TRUE;

/* shouldn't we have this here? - CON 24/1/91 *//* YES - bug 1142 5/2/91 */
			/* NO - do this earlier - bug TS/1588 20/05/92 */
/*r = copytree(r, my_lexlevel); *//* to be added into check exp */

			if (TagOf (skipped_r) == N_REPL) {
				check_needed = range_check_replicator (tptr, NDeclOf (skipped_r), dim, dimisconst);
			}
			if (check_needed) {
				/* bug 1107 - if dimisconst, we must make dim into
				   a copy of the dim tree */
				if (dimisconst) {
					dim = newconstant (LoValOf (dim));	/* bug 1107 14/1/91 */
				}
				r = newdopnode (S_CSUB0, LocnOf (tptr), r, dim, S_INT);
			}
		}
		/*}}} */

		*offset_to_add = 0;
		exp_to_add = r;
		/*}}} */
	}

	DEBUG_MSG (("calc_array_subscript: offset_to_add: %ld\n", *offset_to_add));
#if 0
fprintf (stderr, "calc_array_subscript (return, *offset_to_add = %d), exp_to_add =", *offset_to_add);
printtreenl (stderr, 4, exp_to_add);
#endif
	return exp_to_add;
}

/*}}}*/
/*{{{  PRIVATE treenode *calc_segment_subscript */
PRIVATE treenode *calc_segment_subscript (BIT32 * const offset_to_add, treenode * tptr, const BOOL rangechecking)
{
	treenode *const base = SNameOf (tptr);
	treenode *dim = dimexpof (base, 0);
	const BOOL dimisconst = isconst (dim);
	treenode *exp_to_add = NULL;

	if (!dimisconst) {
		/*{{{  set up ELSIZE node */
		/*shouldn't that be copytree(base, my_lexlevel) ? *//* CON - 24/1/91 */
		/* NO - this breaks all sorts of stuff. We capitulate, and state that
		   S_ELSIZE nodes are special and do not obey these rules! - CON 7/2/91 bug 1142 */
		dim = newmopnode (S_ELSIZE, LocnOf (tptr), base, S_INT);
#if 0
		printf ("dim is");
		printtree (stdout, 0, dim);
		printf ("\n");
#endif
		/*}}} */
	}
	if (rangechecking) {
		/*{{{  create a check tree, put it in the check field, or the start exp */
		treenode *const startexp = skipevals (SStartExpOf (tptr));
		treenode *const lengthexp = SLengthExpOf (tptr);
		const SOURCEPOSN locn = LocnOf (tptr);
		const BOOL baseknown = isconst (startexp);
		const BOOL sizeknown = dimisconst;
		const BOOL countknown = isconst (lengthexp);
		if (!baseknown || !countknown || !sizeknown) {
			/*{{{  what the tree looks like */
			/*
			   size is   PLUS    (may be constant)
			   /  \
			   dim   1

			   base is SEGSTART    (may be constant)
			   |
			   segment

			   count is ELSIZE    (may be constant)
			   |
			   segment

			   Basic tree is
			   CSUB0
			   /     \
			   count    MINUS
			   /     \
			   size   CSUB0
			   /   \
			   base  size

			   if size is not known at compile time,

			   if base is constant 0          if count is constant 0

			   CSUB0                          CSUB0
			   /   \                          /   \
			   count size                    base    size


			   if count is constant,

			   CSUB0
			   /   \
			   MINUS  base
			   /   \
			   size  count

			   if size is known at compile time and base is constant,

			   CSUB0
			   /   \
			   MINUS  count
			   /   \
			   size  base

			   'segment' is the original segment. Note that this introduces a cycle
			   into the tree, but only via the special ELSIZE and SEGSTART nodes.
			 */
			/*}}} */
			treenode *checkexp = NULL;
			treenode *size = sizeknown ? newconstant (LoValOf (dim) + 1)
				: newdopnode (S_PLUS, locn, dim, newconstant (1), S_INT);
			treenode *count = countknown ? newconstant (LoValOf (lengthexp)) :
				/* shouldn't we use copytree(tptr, my_lexlevel) instead of tptr here? CON 24/1/91 */
				/* NO - this breaks all sorts of stuff. We capitulate, and state that
				   S_ELSIZE nodes are special and do not obey these rules! - CON 7/2/91 bug 1142 */
				newmopnode (S_ELSIZE, locn, tptr, S_INT);

#if 0
			fprintf (outfile, "baseknown: %d, sizeknown: %d, countknown: %d\n", baseknown, sizeknown, countknown);
			fprintf (outfile, "base: ");
			printtree (outfile, 0, startexp);
			fprintf (outfile, "\nsize: ");
			printtree (outfile, 0, size);
			fprintf (outfile, "\ndim: ");
			printtree (outfile, 0, dim);
			fprintf (outfile, "\ncount: ");
			printtree (outfile, 0, count);
			fprintf (outfile, "\n");
#endif

			if (!sizeknown && baseknown && /*LoValOf(base) */ LoValOf (startexp) == 0) {
				/* bug TS/1965 24/11/92 */
				/*{{{  checkexp := count CSUB0 size */
				checkexp = newdopnode (S_CSUB0, locn, count, size, S_INT);
				/*}}} */
			} else if (countknown) {
				/*{{{  we can put the check expression in the start field */
				/* Put these checks into the start expression, so that we get better
				   code sequences */
				treenode *seg_base = SStartExpOf (tptr);
				if (!sizeknown && LoValOf (count) == 0) {
					/*{{{  checkexp := seg_base CSUB0 size */
					checkexp = newdopnode (S_CSUB0, locn, seg_base, size, S_INT);
					/*}}} */
				} else if (sizeknown) {
					/*{{{  checkexp := seg_base CSUB0 (size MINUS count) */
					SetLoVal (size, LoValOf (size) - LoValOf (count));
					checkexp = newdopnode (S_CSUB0, locn, seg_base, size, S_INT);
					/*}}} */
				} else {
					/*{{{  checkexp := seg_base CSUB0 (size MINUS count) */
					checkexp = newdopnode (S_MINUS, locn, size, count, S_INT);
					checkexp = newdopnode (S_CSUB0, locn, seg_base, checkexp, S_INT);
					/*}}} */
				}
				SetSStartExp (tptr, checkexp);
				checkexp = NULL;
				/*}}} */
			} else if (sizeknown && baseknown) {
				/*{{{  checkexp := count CSUB0 (size MINUS base) */
				SetLoVal (size, LoValOf (size) - LoValOf (startexp));
				checkexp = newdopnode (S_CSUB0, locn, count, size, S_INT);
				/*}}} */
			} else {
				/*{{{  checkexp := count CSUB0 (size MINUS (base CSUB0 size)) */
				treenode *seg_base;
				/*{{{  set up seg_base */
				if (baseknown) {
					seg_base = newconstexpnode (S_CONSTEXP, locn, dummyexp_p, 0, LoValOf (startexp));
				} else {
					/* shouldn't we use copytree(tptr, my_lexlevel) instead of tptr here? CON 24/1/91 */
					/* NO - this breaks all sorts of stuff. We capitulate, and state that
					   S_SEGSTART nodes are special and do not obey these rules! - CON 7/2/91 bug 1142 */
					seg_base = newmopnode (S_SEGSTART, locn, tptr, S_INT);
				}
				/*}}} */
				checkexp = newdopnode (S_CSUB0, locn, seg_base, size, S_INT);
				checkexp = newdopnode (S_MINUS, locn, size, checkexp, S_INT);
				checkexp = newdopnode (S_CSUB0, locn, count, checkexp, S_INT);
				/*}}} */
			}
			SetSCheckExp (tptr, checkexp);
		}
		/*}}} */
	}

	if (isconst (SStartExpOf (tptr))) {	/* subscript is constant */
		/*{{{  start exp is constant, offset_to_add = startexp */
		*offset_to_add = LoValOf (SStartExpOf (tptr));
		/*}}} */
	} else {
		/*{{{  add to exp */
		/* shouldn't we use copytree(tptr, my_lexlevel) instead of tptr here? CON 24/1/91 */
		/* NO - this breaks all sorts of stuff. We capitulate, and state that
		   S_SEGSTART nodes are special and do not obey these rules! - CON 7/2/91 bug 1142 */
		exp_to_add = newmopnode (S_SEGSTART, LocnOf (tptr), tptr, S_INT);
		*offset_to_add = 0;
		/*}}} */
	}

	return exp_to_add;
}

/*}}}*/
/*{{{  PRIVATE void      get_offset_and_subscript_bytes */
PRIVATE void get_offset_and_subscript_bytes (int *offsetunits, int *subscriptunits, treenode * type, treenode * tptr)
{
	treenode *const base_type = basetype_tree (type);
	int b, subscripttype, offsettype;


#if 0
fprintf (stderr, "tran3: get_offset_and_subscript_bytes: base_type = ");
printtreenl (stderr, 4, base_type);
#endif
#ifdef MOBILES
	if (TagOf (base_type) == S_MOBILE) {
#if 1
		b = bytesperword;
#else
		b = 1;
#endif
#if 0
fprintf (stderr, "tran3: get_offset_and_subscript_bytes: something MOBILE, b = %d\n", b);
#endif
	} else
#endif
	{
#ifdef OCCAM2_5
		b = (TagOf (base_type) == S_RECORD) ? bytesperword : (int) bytesin (base_type);
#else
		b = (int) bytesin (base_type);
#endif
	}

	/* frmb: unwound this mess.. */
	if (trans_params->scale_by_bytes) {
		subscripttype = S_BYTE;
	} else if (can_use_wsubdb (b, bytesperword, tptr)) {
		if (bytesperword == 2) {
			subscripttype = S_INT32;
		} else {
			subscripttype = S_INT64;
		}
	} else if (b >= bytesperword) {
		subscripttype = S_INT;
	} else if (use_shortintops && (b == 2)) {
		subscripttype = S_INT16;
	} else {
		subscripttype = S_BYTE;
	}
	
	/* Note that the offset scale is different from the subscript scale
	   when we use wsubdb (eg. subscripttype == S_INT64) */
	offsettype = (bytesinscalar (subscripttype) > bytesperword) ? S_INT : subscripttype;

	*offsetunits = bytesinscalar (offsettype);
	*subscriptunits = bytesinscalar (subscripttype);
}

/*}}}*/

/* version using full tree expansion, in bytes   */
/* necessary for occam 2.5, because of RECORDs.  */
/* However, use it always, because it is cleaner */
/*{{{  PRIVATE treenode *scaletreebytes */
/*{{{  comment */
/*****************************************************************************
 *
 *  scaletreebytes takes a type tree 'type' and returns an expression tree
 *              which generates the size of that type in bytes.
 *
 *  It purposely doesn't try to 'optimise' this, because that makes it
 *  harder to re-associate terms later on.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE treenode *scaletreebytes (treenode * type, const int lexlevel)
{
	treenode *sizetree;

#if 0
fprintf (stderr, "tran3: scaltreebytes: type (unfollowed) =");
printtreenl (stderr, 4, type);
#endif
	type = follow_user_type (type);

#if 0
fprintf (stderr, "tran3: scaltreebytes: type (followed) =");
printtreenl (stderr, 4, type);
#endif
#ifdef MOBILES
	if (TagOf (type) == S_MOBILE) {
		/* this is asking about scaling for MOBILEs within something -- they're pointers, so min. 4 (chan-types, static mobiles)
		 * or 4 + (4 * dimcount) for dynamic mobile arrays */
		sizetree = newconstant (bytesperword);
	} else
#endif
	if (TagOf (type) == S_ARRAY) {
		sizetree = add_to (copytree (ARDimLengthOf (type), lexlevel), S_TIMES, scaletreebytes (ARTypeOf (type), lexlevel));
	} else {
		sizetree = newconstant (bytesin (type));
	}
#if 0
fprintf (stderr, "tran3: scaltreebytes: resulting size =");
printtreenl (stderr, 4, sizetree);
#endif
	return sizetree;
}

/*}}}*/
/*{{{  PRIVATE treenode *reassociate_terms */
PRIVATE treenode *reassociate_terms (treenode * tptr)
/* This is used to re-factor the array subscript expressions so that
   they turn back into something useful.
   We have been given a 'sum of products' type expression.
   We want to try to eliminate the additions, ie move them further down
   the tree, so that the multiplications are only performed when
   necessary. This also has the benefit of ensuring that the final
   division phase is more likely to be able to adjust constants, rather
   than actually doing a divide at run-time.

a)   (x * size) + (y * size) => (x + y) * size
b)   (x * (dim * size)) + (y * size) => ((x * dim) + y) * size
c)   (x * (dim1 * (dim2 * ... * (dimn * size)))) + (y * size) =>
       ((x * (dim * (dim2 * ... * dimn))) * size) + (y * size)
d)   (x * c1) + (y * c2) => ((x * (c1/c2)) + y) * c2
       (Used when 'c1' is size of a record, where the size is calculated
       directly rather than as a product of terms, so we can't use
       part (c)
*/
{
	BOOL any_changes = TRUE;

#if 0				/*def DEBUG */
	printf ("reassociate_terms: original tree is:");
	printexp (stdout, tptr);
	printf ("\n");
#endif

	while (any_changes && (TagOf (tptr) == S_ADD) && (DOpTypeOf (tptr) == S_INT)) {
		treenode *lhs;
		treenode *rhs;
		treenode *t = tptr;
		any_changes = FALSE;
		while ((TagOf (LeftOpOf (t)) == S_ADD) && (DOpTypeOf (t) == S_INT))
			t = LeftOpOf (t);	/* t now points at the leftmost addition */
		lhs = LeftOpOf (t);
		rhs = RightOpOf (t);
		if ((TagOf (lhs) == S_TIMES) && (TagOf (rhs) == S_TIMES))
			/*{{{  look for possible re-association */
		{
			if (issame (RightOpOf (lhs), RightOpOf (rhs)))
				/*{{{  first version for association */
			{
				/* we have: (x * size) + (y * size) => (x + y) * size */
				treenode *size = RightOpOf (rhs);
				SetTag (t, S_TIMES);
				SetTag (lhs, S_ADD);
				freetree (RightOpAddr (lhs));
				SetRightOp (lhs, LeftOpOf (rhs));
				SetRightOp (t, size);
				freenode (&rhs);
				any_changes = TRUE;
			}
			/*}}} */

			else if ((TagOf (RightOpOf (lhs)) == S_TIMES) && issame (RightOpOf (RightOpOf (lhs)), RightOpOf (rhs)))
				/*{{{  second version for association */
			{
				/* (x * (dim * size)) + (y * size) => ((x * dim) + y) * size */
				treenode *size = RightOpOf (RightOpOf (t));
				treenode *dim = LeftOpOf (RightOpOf (lhs));
				freetree (RightOpAddr (RightOpOf (lhs)));

				/*printf("reassociate_terms: This isn't working yet!\n"); */

				SetRightOp (RightOpOf (lhs), dim);
				SetLeftOp (RightOpOf (lhs), LeftOpOf (lhs));

				SetTag (lhs, S_ADD);
				SetLeftOp (lhs, RightOpOf (lhs));
				SetRightOp (lhs, LeftOpOf (rhs));

				SetTag (t, S_TIMES);
				SetRightOp (t, size);
				freenode (&rhs);
				any_changes = TRUE;
			}
			/*}}} */

			else
				/*{{{  third version for association */
			{
				/* (x * (dim1 * (dim2 * ... * (dimn * size)))) + (y * size) =>
				   ((x * (dim * (dim2 * ... * dimn))) * size) + (y * size) */
				int nested = 0;
				treenode *dimexp = RightOpOf (lhs);
				BOOL found = FALSE;
				/*{{{  see if lhs size is a multiple of rhs size */
				while (!found && (TagOf (dimexp) == S_TIMES))
					if (issame (RightOpOf (dimexp), RightOpOf (rhs)))
						found = TRUE;
					else {
						dimexp = RightOpOf (dimexp);
						nested++;
					}
				/*}}} */
				if (found) {
					while (nested > 0)
						/*{{{  move one term across the lhs' multiplier */
					{
						dimexp = RightOpOf (lhs);
						SetRightOp (lhs, RightOpOf (dimexp));
						SetRightOp (dimexp, LeftOpOf (dimexp));
						SetLeftOp (dimexp, LeftOpOf (lhs));
						SetLeftOp (lhs, dimexp);
						nested--;
					}
					/*}}} */
					any_changes = TRUE;
				}
			}
			/*}}} */

#ifdef OCCAM2_5
			if (!any_changes && (TagOf (RightOpOf (lhs)) == S_CONSTEXP) && (TagOf (RightOpOf (rhs)) == S_CONSTEXP))
				/*{{{  fourth version */
			{
				/*  (x * c1) + (y * c2) => ((x * (c1/c2)) + y) * c2 */
				const INT32 c1 = LoValOf (RightOpOf (lhs));
				const INT32 c2 = LoValOf (RightOpOf (rhs));
				if ((c1 % c2) == 0) {
					SetLoVal (RightOpOf (lhs), c1 / c2);
					SetTag (t, S_TIMES);
					SetRightOp (t, RightOpOf (rhs));
					SetTag (rhs, S_ADD);
					SetRightOp (rhs, LeftOpOf (rhs));
					SetLeftOp (rhs, lhs);
					SetLeftOp (t, rhs);
					any_changes = TRUE;
				}
			}
			/*}}} */
#endif
		}
		/*}}} */

#if 0				/*def DEBUG */
		if (any_changes) {
			printf ("reassociate_terms: new tree is:");
			printexp (stdout, tptr);
			printf ("\n");
		}
#endif
	}
	return tptr;
}

/*}}}*/
/*{{{  PRIVATE treenode *merge_multiplies */
PRIVATE treenode *merge_multiplies (treenode * t)
/* This converts expressions of the form
     (x * const) * const
   =>
     x * (const * const)
   It also removes multiplies by 1.
   It also ripples constant terms up towards the 'root'.
*/
{
	switch (TagOf (t)) {
	case S_ADD:
		SetLeftOp (t, merge_multiplies (LeftOpOf (t)));
		SetRightOp (t, merge_multiplies (RightOpOf (t)));
		break;
	case S_TIMES:
		SetLeftOp (t, merge_multiplies (LeftOpOf (t)));
		SetRightOp (t, merge_multiplies (RightOpOf (t)));
		/*{{{  move any constant to the rhs */
		if (isconst (LeftOpOf (t))) {
			treenode *swap = LeftOpOf (t);
			SetLeftOp (t, RightOpOf (t));
			SetRightOp (t, swap);
		}
		/*}}} */
		if (DOpTypeOf (t) == S_INT) {
			treenode *lhs = LeftOpOf (t);
			treenode *rhs = RightOpOf (t);
			if (isconstexpnd (rhs))
				/*{{{  look for other constants */
			{
				if (LoValOf (rhs) == 1)
					/*{{{  remove the 'TIMES 1' */
				{
					treenode *temp = t;
					t = lhs;
					freetree (RightOpAddr (temp));
					freenode (&temp);
				}
				/*}}} */
				else if ((TagOf (lhs) == S_TIMES) && isconst (RightOpOf (lhs)))
					/*{{{  merge the multiplies */
				{
					SetLeftOp (t, LeftOpOf (lhs));
					SetLeftOp (lhs, rhs);
					SetRightOp (t, foldexp (lhs));
				}
				/*}}} */
			}
			/*}}} */
			else if ((TagOf (lhs) == S_TIMES) && isconst (RightOpOf (lhs)))
				/*{{{  move constant from lhs */
			{
				/* (x * const) * y => (x * y) * const */
				SetRightOp (t, RightOpOf (lhs));
				SetRightOp (lhs, rhs);
			}
			/*}}} */
			else if ((TagOf (rhs) == S_TIMES) && isconst (RightOpOf (rhs)))
				/*{{{  move constant from rhs */
			{
				/* x * (y * const) => (x * y) * const */
				SetLeftOp (t, rhs);
				SetRightOp (t, RightOpOf (rhs));
				SetRightOp (rhs, LeftOpOf (rhs));
				SetLeftOp (rhs, lhs);
			}
			/*}}} */
		}
	}
	return t;
}

/*}}}*/
/*{{{  PRIVATE treenode *divide_expression */
PRIVATE treenode *divide_expression (treenode * t, INT32 units /*, const SOURCEPOSN locn */ )
/* This is used to divide the finally subscript expression by the scale factor
   used appropriate to word subscripting, byte subscripting, etc.
   Instead of simply inserting a divide, it sees if there is already a
   constant multiplier, and scales that multiplier instead.

   In practice, there _always_ is a constant multiplier, so it never actually
   inserts a division!
*/
{
	if ((TagOf (t) == S_TIMES) && isconstexpnd (RightOpOf (t))) {
		const INT32 val = LoValOf (RightOpOf (t));
		BOOL free_top_multiply = FALSE;

		if (val == units)
			/*{{{  remove the multiply */
		{
			free_top_multiply = TRUE;
			units = 1;
		}
		/*}}} */
		else if ((val % units) == 0)
			/*{{{  adjust the multiply */
		{
			SetLoVal (RightOpOf (t), val / units);
			units = 1;
		}
		/*}}} */
		else if ((units % val) == 0)
			/*{{{  remove the multiply; scale down the divisor */
		{
			free_top_multiply = TRUE;
			units /= val;
		}
		/*}}} */

		if (free_top_multiply)
			/*{{{  remove the multiply */
		{
			treenode *temp = t;
			t = LeftOpOf (t);
			freetree (RightOpAddr (temp));
			freenode (&temp);
		}
		/*}}} */
	}
	if (units != 1)
		/*{{{  insert a real division */
	{
		/*printf("divide_expression: line: #%lX, dividing by: %ld\n",
		   locn, units); */
		t = add_to (t, S_DIV, newconstant (units));
	}
	/*}}} */
	return t;
}

/*}}}*/
/*{{{  PRIVATE treenode *transformsubscript */
/*{{{  comment */
/*****************************************************************************
 *
 *  transformsubscript takes a subscript tree 'tptr'
 *                     and transforms it into something more suitable
 *                     for code generation.
 *                     offset is a constant offset from the value of the
 *                     subscript tree to the actual item.
 *                     If rangechecking is TRUE, subscript checks are inserted
 *                     into the tree as required.
 *
 *                     Converts ARRAYITEMs back into ARRAYSUBs, and
 *                     SEGMENTITEMs back into SEGMENTs.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE treenode *transformsubscript (treenode * const tptr, BIT32 * const offset, BIT32 * const byte_offset, const BOOL rangechecking, const int my_lexlevel)
{
	treenode *t = NULL;	/* result tree */
	BIT32 offset_to_add = 0;
	treenode *exp_to_add = NULL;
	treenode *scale_to_add = NULL;

#if 0
fprintf (stderr, "transformsubscript (offset = %d): tptr =", (int)(*offset));
printtreenl (stderr, 4, tptr);
#endif
	switch (TagOf (tptr)) {
		/*{{{  S_ARRAYSUB  S_ARRAYITEM */
	case S_ARRAYSUB:
	case S_ARRAYITEM:
		SetTag (tptr, S_ARRAYSUB);	/* convert ARRAYITEMs back to ARRAYSUBs */
		SetASExp (tptr, NULL);
		SetASOffset (tptr, 0);

		t = transformsubscript (ASBaseOf (tptr), offset, byte_offset, rangechecking, my_lexlevel);

		exp_to_add = calc_array_subscript (&offset_to_add, tptr, rangechecking, my_lexlevel);
#ifdef MOBILES
		{
			treenode *type = gettype (tptr);
			treenode *basetype = gettype (ASBaseOf (tptr));

			/* if we have a nested MOBILE, fixup the base to be an ARRAYITEM */
			if ((TagOf (basetype) == S_MOBILE) && (TagOf (ASBaseOf (tptr)) == S_ARRAYSUB)) {
#if 0
fprintf (stderr, "transformsubscript: basetype = ");
printtreenl (stderr, 4, basetype);
fprintf (stderr, "transformsubscript: ASBaseOf (tptr) = ");
printtreenl (stderr, 4, ASBaseOf (tptr));
fprintf (stderr, "transformsubscript: exp_to_add = ");
printtreenl (stderr, 4, exp_to_add);
fprintf (stderr, "transformsubscript: t = ");
printtreenl (stderr, 4, t);
#endif
				/* need to fixup here */
				SetTag (ASBaseOf (tptr), S_ARRAYITEM);
				SetASExp (ASBaseOf (tptr), t);
				t = exp_to_add;
				exp_to_add = NULL;
			}

//			if (TagOf (type) == S_MOBILE) {
//				 type = MTypeOf (type);
//			}
			scale_to_add = type;
		}
#else	/* !MOBILES */
		scale_to_add = gettype (tptr);
#endif	/* !MOBILES */
		break;
		/*}}} */
		/*{{{  S_SEGMENT   S_SEGMENTITEM */
	case S_SEGMENT:
	case S_SEGMENTITEM:
		{
			treenode *type = follow_user_type (gettype (SNameOf (tptr)));

			SetTag (tptr, S_SEGMENT);	/* convert SEGMENTITEMs back to SEGMENTs */
			t = transformsubscript (SNameOf (tptr), offset, byte_offset, rangechecking, my_lexlevel);

			exp_to_add = calc_segment_subscript (&offset_to_add, tptr, rangechecking);
#ifdef MOBILES
			if (TagOf (type) == S_MOBILE) {
				type = MTypeOf (type);
			}
#endif	/* MOBILES */
			scale_to_add = ARTypeOf (type);
		}
		break;
		/*}}} */
		/*{{{  S_RECORDSUB S_RECORDITEM */
#ifdef OCCAM2_5
	case S_RECORDSUB:
	case S_RECORDITEM:
		SetTag (tptr, S_RECORDSUB);	/* convert RECORDITEMs back to RECORDSUBs */
		SetASExp (tptr, NULL);
		SetASOffset (tptr, 0);

		t = transformsubscript (ASBaseOf (tptr), offset, byte_offset, rangechecking, my_lexlevel);

		DEBUG_MSG (("transformsubscript: S_RECORDSUB: byte_offset: %ld\n", NVOffsetOf (ASIndexOf (tptr))));
		*byte_offset += NVOffsetOf (ASIndexOf (tptr));
		break;
#endif
		/*}}} */
		/*{{{  default; base is a name etc */
	default:
		*offset = 0;
		break;
		/*}}} */
	}

	/*{{{  scale offsets by size of object */
	/*{{{  scale by number of bytes */
	if (scale_to_add != NULL) {
		treenode *const scale = scaletreebytes (scale_to_add, my_lexlevel);
#if 0
fprintf (stderr, "transformsubscript-mid: scale_to_add = ");
printtreenl (stderr, 4, scale_to_add);
fprintf (stderr, "transformsubscript-mid: scale = ");
printtreenl (stderr, 4, scale);
#endif
		if (bytesin (scale_to_add) != 1) {
			if (offset_to_add != 0) {
				if (isconst (scale)) {
					offset_to_add *= LoValOf (foldexp (scale));
				} else {
					exp_to_add = add_to (exp_to_add, S_ADD, newconstant (offset_to_add));
					offset_to_add = 0;
				}
			}
			if (exp_to_add != NULL)
				exp_to_add = add_to (exp_to_add, S_TIMES, scale);
		}
	}
	/*}}} */
	/*{{{  add in bytes field */
	if (*byte_offset != 0) {
		offset_to_add += *byte_offset;
		*byte_offset = 0;
	}
	/*}}} */
	/*}}} */

#if 0
fprintf (stderr, "transformsubscript: before any addition: offset_to_add = %d, exp_to_add = ", offset_to_add);
printtreenl (stderr, 4, exp_to_add);
#endif
	*offset += offset_to_add;
	if (exp_to_add != NULL) {
		t = add_to (t, S_ADD, exp_to_add);
	}

	DEBUG_MSG (("transformsubscript: return:%s, offset: %ld, byte_offset: %ld\n", itagstring (TagOf (tptr)), *offset, *byte_offset));
#if 0
fprintf (stderr, "transformsubscript-out (offset = %d): t =", (int)(*offset));
printtreenl (stderr, 4, t);
#endif
	return (t);
}

/*}}}*/
/*{{{  PRIVATE treenode *scalesubscript (type, t, offset) */
/*{{{  comment */
/*****************************************************************************
 *
 *  scalesubscript takes a transformed subscript expression 't', together
 *                 with its constant offset '*offset' and scales it by
 *                 the size of an element of type 'type'.  Updates *offset,
 *                 and returns the scaled tree.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE treenode *scalesubscript (treenode * const type, treenode * t, treenode * const tptr,
		BIT32 * const offset, BIT32 byte_offset, const INT32 scale, const int my_lexlevel)
{
	int offsetunits, subscriptunits;
	USE_VAR (my_lexlevel);
	USE_VAR (scale);
	USE_VAR (byte_offset);

	get_offset_and_subscript_bytes (&offsetunits, &subscriptunits, type, tptr);

#if 0
fprintf (stderr, "tran3: scalesubscript (in): offset = %d, byte_offset = %d, scale = %d, offsetunits = %d, subscriptunits = %d, t =", *offset, byte_offset, scale, offsetunits, subscriptunits);
printtreenl (stderr, 4, t);
#endif
	DEBUG_MSG (("scalesubscript: scale=%ld, offset=%ld, offsetunits:%d, subscriptunits:%d\n", scale, *offset, offsetunits, subscriptunits));

	if (t != NULL) {
		t = reassociate_terms (t);
		t = foldexp (t);
		t = merge_multiplies (t);
	}
#if 0
fprintf (stderr, "tran3: scalesubscript (after meddling), t = ");
printtreenl (stderr, 4, t);
#endif

	if (t != NULL && (subscriptunits != 1)) {
		BOOL ok = TRUE;
		if (TagOf (tptr) == S_ARRAYSUB) {
			treenode * const base = ASBaseOf (tptr);
			if (TagOf (base) == S_ARRAYITEM) {
				ok = FALSE;
			}
		}
		if ((TagOf (type) == S_MOBILE) && (TagOf (MTypeOf (type)) == S_ARRAY)) {
			ok = FALSE;
		}
		if (ok) {
			t = divide_expression (t, subscriptunits /*, LocnOf(tptr) */ );
		}
	}

	/* bug INSdi03553 - ensure it is a signed divide! */
	*offset = (INT32) *offset / offsetunits;

	t = foldexp (t);
	DEBUG_MSG (("scalesubscript: final offset=%ld\n", *offset));
#if 0
fprintf (stderr, "tran3: scalesubscript (out): offset = %d, byte_offset = %d, scale = %d, t =", *offset, byte_offset, scale);
printtreenl (stderr, 4, t);
#endif
	return (t);
}

/*}}}*/
/*{{{  PUBLIC  treenode *transformelement */
/*{{{  comment */
/*****************************************************************************
 *
 *  transformelement  creates a subscript expression tree which includes
 *                    this node and any nested ARRAYSUB and SEGMENT nodes,
 *                    (or nested ARRAYITEM or SEGMENTITEM nodes, which are
 *                     converted back into their respective ARRAYSUB etc)
 *                    converts this node to an ARRAYITEM node if it was an
 *                    ARRAYSUB node, or a SEGMENTITEM node if it was a SEGMENT
 *                    node, and inserts the subscript expression into it.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC treenode *transformelement (const trans_params_t * const trans_params_in, treenode * tptr, const BOOL rangechecking, const int my_lexlevel)
{
	const trans_params_t *const saved_params = trans_params;
	BIT32 offset = 0;
	BIT32 byte_offset = 0;
	treenode *subscriptexp;

	trans_params = trans_params_in;

	DEBUG_MSG (("transformelement: %s\n", itagstring (TagOf (tptr))));
#if 0
fprintf (stderr, "transformelement (in): on %s: tptr =", itagstring (TagOf (tptr)));
printtreenl (stderr, 4, tptr);
#endif

	/*{{{  build the subscript expression tree */
	/*subscriptexp = transformsubscript(tptr, &offset, &useoffset,rangechecking); */
	subscriptexp = transformsubscript (tptr, &offset, &byte_offset, rangechecking, my_lexlevel);
#if 0
fprintf (stderr, "transformelement: offset = 0x%8.8x, byte_offset = 0x%8.8x, subscriptexp =", (unsigned int)offset, (unsigned int)byte_offset);
printtreenl (stderr, 4, subscriptexp);
#endif
	/*}}} */
	DEBUG_MSG (("transformelement: %s, offset:%ld, byte_offset:%ld\n", itagstring (TagOf (tptr)), offset, byte_offset));

	switch (TagOf (tptr)) {
		/*{{{  make ARRAYITEM node */
	case S_ARRAYSUB:
		{
			treenode *const type = gettype (tptr);
			int nelem = elementsin (type);

#if 0
fprintf (stderr, "transformelement (ARRAYSUB): nelem = %d, type = ", nelem);
printtreenl (stderr, 4, type);
#endif
#if 0
			/* re-enabled: if a dynamic mobile array sub-type, doing BSUB in the back-end, so _don't_ scale the subscript */
			/* FIXME: this stuff _really_ needs to be tidied up (either new-compiler time or make MOBILE handling distinct from ARRAY{SUB,ITEM} */
			if (isdynmobilearraytype (type)) {
				/* do nothing.. */
			} else {
				subscriptexp = scalesubscript (type, subscriptexp, tptr, &offset, byte_offset, nelem, my_lexlevel);
			}
#else
			subscriptexp = scalesubscript (type, subscriptexp, tptr, &offset, byte_offset, nelem, my_lexlevel);
#endif
			SetTag (tptr, S_ARRAYITEM);
			/* leave the ASBase and ASIndex fields unchanged */
			SetASExp (tptr, subscriptexp);
			SetASOffset (tptr, offset);
		}
		break;
		/*}}} */
		/*{{{  make RECORDITEM node */
#ifdef OCCAM2_5
	case S_RECORDSUB:
		{
			treenode *const type = gettype (tptr);

			subscriptexp = scalesubscript (type, subscriptexp, tptr, &offset, byte_offset, 1, my_lexlevel);
			SetTag (tptr, S_RECORDITEM);
			/* leave the ASBase and ASIndex fields unchanged */
			SetASExp (tptr, subscriptexp);
			SetASOffset (tptr, offset);
		}
		break;
#endif
		/*}}} */
		/*{{{  make SEGMENTITEM node */
	case S_SEGMENT:
		{
			treenode *const type = ARTypeOf (gettype (tptr));

#if 0
fprintf (stderr, "transformelement on SEGMENT, type =");
printtreenl (stderr, 4, type);
#endif
			subscriptexp = scalesubscript (type, subscriptexp, tptr, &offset, byte_offset, elementsin (type), my_lexlevel);
			SetTag (tptr, S_SEGMENTITEM);
			SetSSubscriptExp (tptr, subscriptexp);
			SetSOffset (tptr, offset);
		}
		break;
		/*}}} */
		/*{{{  default - error */
	default:
		badtag (LocnOf (tptr), TagOf (tptr), "transformelement");
		break;
		/*}}} */
	}

	trans_params = saved_params;
#if 0
fprintf (stderr, "transformelement (out): tptr now =");
printtreenl (stderr, 4, tptr);
#endif
	return (tptr);
}

/*}}}*/


