/* $Id: tran1.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	tree transformer
 *	Copyright (C) 1990 Inmos Limited
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
#include "compharn.h" /* IMPORTED */	/* testflag */
#include "extlib.h"		/* IMPORTED */

#include "trandef.h"
#include "tran1def.h"
#include "predefhd.h"
#include "mobiles.h"

/*}}}  */

/*{{{  constants */
/* This is used to optimise VALOFs which expand into constants. */
#define OPTIMISE_VALOFS TRUE

/* any number will do, but a high one makes them look like temporaries */
#define ANON_VAR_SCOPE 0xFFF
/*}}}  */

/*{{{  PUBLIC  variables */
/* just public to the other 'trans' modules */
PUBLIC const trans_params_t *trans_params;
/*}}}  */
/*{{{  PRIVATE variables */
PRIVATE BOOL inline_expansions;
PRIVATE BOOL optimise_valof = TRUE;
PRIVATE int overlap_checks;

/* bug 1324 24/10/91 */
typedef struct {
	treenode **insertpoint;
	BOOL insertvalof;
} trans_data_t;
PRIVATE trans_data_t trans_data;

PRIVATE BOOL no_anonymous_vars = FALSE;	/* bug 1455 5/11/91 */

PRIVATE int trans_lexlevel;
PRIVATE SOURCEPOSN trans_locn;

/* This flag is set while inside ASM, and inside the rhs of a RETYPE.
   If a channel is encountered while it is set, it is known that we can't
   perform fancy optimisations on that channel.
*/
PRIVATE BOOL trans_flag_scalar_chans;

PRIVATE treenode *inside_suspends = NULL;

/*}}}  */

/*{{{  forward declarations */
PRIVATE void trans (treenode **tptr);
PRIVATE void transexp (treenode **tptr, BOOL is_sub_expression);
PRIVATE void sub_transexp (treenode **tptr, BOOL is_sub_expression);
PRIVATE BOOL trans_cloned_output (treenode **tptr, treenode ***replacepoint);
PRIVATE void trans_anychantype_io (const int tag, treenode *const chan, treenode *const item, treenode **hashio, treenode **chanio, const SOURCEPOSN locn);
PRIVATE void trans_anyproctype_io (const int tag, treenode *const chan, treenode *const item, treenode **hashio, treenode **chanio, const SOURCEPOSN locn);
/*}}}  */

/*{{{  PRIVATE treenode *buildoverlapcheck (treenode *tptr) */
/*****************************************************************************
 *
 *  buildoverlapcheck takes an overlapcheck tree 'tptr' and returns an
 *                    expression tree which will perform the overlap check.
 *
 *****************************************************************************/
PRIVATE treenode *buildoverlapcheck (treenode * tptr)
{
	treenode *lh_subscripts = LeftOpOf (tptr);
	treenode *rh_subscripts = RightOpOf (tptr);
	SOURCEPOSN locn = LocnOf (tptr);
	treenode *result = NULL;
	while (!EndOfList (lh_subscripts) && !EndOfList (rh_subscripts))
		/*{{{  add this check to result */
	{
		treenode *lhs = ThisItem (lh_subscripts);
		treenode *rhs = ThisItem (rh_subscripts);
		treenode *checktree;
		if (TagOf (lhs) == S_FOR || TagOf (rhs) == S_FOR)
			/*{{{  checktree =  full check */
			/*{{{  COMMENT what the full check tree looks like */
	/**********************  Start comment out ****************************
	The full check is

			    BITAND
			 /         \
		      GR             GR
		     /  \           /  \
		   ADD  base2     ADD  base1
		 /     \        /     \
	      base1  count1  base2  count2
	If this is TRUE, we have an overlap.
	 **********************   End comment out  ****************************/
			/*}}}  */
		{
			treenode *base1, *count1, *base2, *count2;
			treenode *c1, *c2;
			/*{{{  set up base1, count1, base2, count2 */
			if (TagOf (lhs) == S_FOR) {
				base1 = LeftOpOf (lhs);
				count1 = RightOpOf (lhs);
			} else {
				base1 = lhs;
				count1 = newconstant (1);
			}
			if (TagOf (rhs) == S_FOR) {
				base2 = LeftOpOf (rhs);
				count2 = RightOpOf (rhs);
			} else {
				base2 = rhs;
				count2 = newconstant (1);
			}
			/*}}}  */
			c1 = newdopnode (S_ADD, locn, base1, count1, S_INT);
			c1 = newdopnode (S_GR, locn, c1, copytree (base2, trans_lexlevel), S_INT);
			c2 = newdopnode (S_ADD, locn, base2, count2, S_INT);
			c2 = newdopnode (S_GR, locn, c2, copytree (base1, trans_lexlevel), S_INT);

			/* bug 1117 29/1/91 - one half of these may be constant TRUE.
			   if so, we must constant fold it out:
			   (if it were constant FALSE, it would not have been generated
			   by the alias checker)
			   (similarly, they cannot both be constant TRUE)
			 */
			c1 = foldexp (c1);
			c2 = foldexp (c2);
			if (isconst (c1))	/* must be constant TRUE */
				checktree = c2;
			else if (isconst (c2))	/* must be constant TRUE */
				checktree = c1;
			else
				checktree = newdopnode (S_BITAND, locn, c1, c2, S_INT);
		}
		/*}}}  */
		/*{{{  else if both are constant */
#if 1				/* Constant FALSE possibilities are now removed in the frontend */
		/* No - the configurer can generate stuff which ends up looking
		   constant, so we allow for that here. bug TS/1495 31/01/92.
		 */
		else if (isconst (lhs) && isconst (rhs) && (LoValOf (lhs) != LoValOf (rhs)))
			/* don't need a check here */
			/* If they are different, we don't need a check at all */
			checktree = NULL;
#endif
		/*}}}  */
		else
			/*{{{  checktree = simple check */
			checktree = newdopnode (S_EQ, locn, lhs, rhs, S_INT);
		/*}}}  */
		if (checktree != NULL) {
			/* 1/11/90 Added for bug 1032 to prevent an alias check of
			   a[i][0] and a[j][0] inserting an AND TRUE for the 0 against 0 check.
			   Theoretically we should also check that the result of folding the
			   checktree is TRUE, but we've already removed all FALSE checks
			   in the frontend.
			 */
			checktree = foldexp (checktree);
			if ((TagOf (checktree) == S_CONSTEXP) /*&& (LoValOf(checktree) != 0) */ )
				checktree = NULL;	/* remove this check */
		}
		if (checktree != NULL)
			/*{{{  AND checktree into result */
		{
			if (result == NULL)
				result = checktree;
			else
				result = newdopnode (S_BITAND, locn, result, checktree, S_INT);
		}
		/*}}}  */
		lh_subscripts = NextItem (lh_subscripts);
		rh_subscripts = NextItem (rh_subscripts);
	}
	/*}}}  */
	if (result == NULL)
		result = dummyexp_p;
	else
		result = newdopnode (S_CSUB0, locn, result, newconstant (1), S_INT);

	/* Now we don't need to bother folding the whole thing, because we've
	   folded each check individually - 1/11/90 bug 1032 */
	/*result = foldexp(result); */
	/* fprintf(outfile, "\nOverlap tree generated :\n");
	   printtree(0, result); */
	return result;
}

/*}}}  */
/*{{{  PRIVATE void      overlapwarnings */
PRIVATE void overlapwarnings (const SOURCEPOSN locn)
{
	if ((overlap_checks > 0) && (warning_flags & WARNING_OVERLAPS)) {
		if (overlap_checks > 1)
			msg_out_i (SEV_WARN, TRANS, TRANS_RUN_TIME_OVERLAP_CHECKS, locn, overlap_checks);
		else
			msg_out (SEV_WARN, TRANS, TRANS_RUN_TIME_OVERLAP_CHECK, locn);
	}
}

/*}}}  */
/*{{{  PRIVATE void      set_abbreviation_mode */
PRIVATE void set_abbreviation_mode (treenode * const t)
{
	/* bug 1156 - 15/2/91 
	   We have to set the access mode of the variable */
	treenode *const nptr = DNameOf (t);
	switch (trans_params->abbrevmode_fn (t)) {
	case AM_CONST:
		/* Though it isn't really, just give it a valid mode */
		SetNMode (nptr, NM_WORKSPACE);
		break;
	case AM_ISRHS:
		SetNMode (nptr, NModeOf (nameof (DValOf (t))));
		break;
	case AM_PTR:
		SetNMode (nptr, NM_POINTER);
		break;
	case AM_VAL:
	case AM_COPYINOUT:
#ifdef MOBILES
		(void) isinmobilespace (nptr);	/* does SetNMode */
#endif
		(void) isinvectorspace (nptr);	/* does SetNMode */
		break;
	case AM_NOTHING:
		SetNMode (nptr, NM_DEFAULT);
		break;
	}
}

/*}}}  */
/*{{{  PUBLIC treenode *trans_create_declaration (treenode *** const insertpoint, BOOL *const insertvalof, const int lexlevel, const char *name, const int decltag, const int nametag, treenode * const typetree, treenode * const expression)*/
PUBLIC treenode *trans_create_declaration (treenode *** const insertpoint, BOOL *const insertvalof, const int lexlevel, const char *name,
			  const int decltag, const int nametag, treenode * const typetree, treenode * const expression)

/*{{{  comment */
/*
What this does is to create a new variable at some suitable scoping level,
so that it can be accessed by transformed code.
Thus we can insert an abbreviation of some complicated expression:

  x := y + complicated.expression
becomes:
  VAL anon IS complicated.expression :
  x := y + anon

This has uses when dealing with complicated bodies such as arrays whose sizes
aren't known at compile-time. Also used for constructors.

If the expression is 'inside' a short-circuiting expression,
we have to insert a VALOF around it:

  x := y OR (complicated.expression > 88)
becomes:
  x := y OR (VAL anon IS complicated.expression :
	     VALOF
	       SKIP
	       RESULT anon > 88
	    )

We have to be careful about where we insert these VALOFs, so that we don't
create a VALOF returning an array, unless the backend can handle them.
The example of AND/OR is OK, because their right-hand-sides are always
BOOL scalars.

The problem comes with counted arrays...

Conor O'Neill, 4/11/91
*/
/*}}}  */
{
	const SOURCEPOSN locn = LocnOf (**insertpoint);
	treenode *new_name;
	treenode *new_decl;
	char name_str[20];

	if (name == NULL) {
		static int anon_count = 0;	/* yes - static to create unique names */
		sprintf (name_str, "$anon%d", anon_count++);
		name = &name_str[0];
	}

	/* we create this as NM_WORKSPACE so that issimplelocal works on the
	   result; if it is placed into vectorspace then this is not so.
	 */
	new_name = newnamenode (nametag, locn, lookupword (name, strlen (name)), typetree, NULL, lexlevel, ANON_VAR_SCOPE,
				/*NM_DEFAULT */ NM_WORKSPACE);
	new_decl = newdeclnode (decltag, locn, new_name, expression, **insertpoint);
	SetNDecl (new_name, new_decl);
	**insertpoint = new_decl;
	*insertpoint = DBodyAddr (new_decl);

	if (insertvalof != NULL && *insertvalof) {	/* we must insert a dummy VALOF too */
		treenode *const valof = newvalofnode (S_VALOF, locn,
						      newleafnode (S_SKIP, locn),
						      newlistnode (S_LIST, locn, DBodyOf (new_decl), NULL));
		**insertpoint = valof;
		*insertvalof = FALSE;
	}
#if 0
fprintf (stderr, "trans_create_declaration: crumbs!  returning new_name (decl of) =");
printtreenl (stderr, 4, new_decl);
#endif
	return new_name;
}

/*}}}  */
/*{{{  PRIVATE treenode *create_declaration (const int decltag, const int nametag, treenode *const typetree, treenode *const expression)*/
PRIVATE treenode *create_declaration (const int decltag, const int nametag, treenode *const typetree, treenode *const expression)
{
	return trans_create_declaration (&trans_data.insertpoint,
					 &trans_data.insertvalof, trans_lexlevel, NULL, decltag, nametag, typetree, expression);
}

/*}}}  */
/*{{{  PRIVATE treenode *fold_type_tree (treenode *type)*/
PRIVATE treenode *fold_type_tree (treenode *type)
{
	treenode *t;

#if 0
fprintf (stderr, "fold_type_tree: type =");
printtreenl (stderr, 4, type);
#endif
	for (t = follow_user_type (type); TagOf (t) == S_ARRAY; t = follow_user_type (ARTypeOf (t))) {
		SetARDimLength (t, foldexp (ARDimLengthOf (t)));
	}
#if 0
fprintf (stderr, "fold_type_tree (return): type =");
printtreenl (stderr, 4, type);
#endif
	return type;
}

/*}}}  */
/*{{{  PRIVATE treenode *copy_type_tree (treenode *const type_tree, const int my_lexlevel)*/
PRIVATE treenode *copy_type_tree (treenode *const type_tree, const int my_lexlevel)
{
#ifdef OCCAM2_5
	if (TagOf (type_tree) == S_RECORD)
		/*{{{  return the name of the record type */
	{
		if (ARTypeOf (type_tree) != NULL)
			return DNameOf (NDeclOf (DNameOf (ARTypeOf (type_tree))));
	}
	/*}}}  */
#endif
	return copytree (type_tree, my_lexlevel);
}

/*}}}  */
/*{{{  PRIVATE treenode *create_abbreviation (treenode *const expression, const BOOL val)*/
PRIVATE treenode *create_abbreviation (treenode *const expression, const BOOL val)
{
	treenode *const exp_type = gettype (expression);
	treenode *const type = copy_type_tree (exp_type, trans_lexlevel);	/* bug TS/1604 3/3/92 */
	treenode *const new_name = create_declaration (val ? S_VALABBR : S_ABBR,
						       val ? N_VALABBR : N_ABBR,
						       fold_type_tree (type),
						       expression);
	set_abbreviation_mode (NDeclOf (new_name));
	return new_name;
}

/*}}}  */
/*{{{  PRIVATE treenode **create_local_abbreviation (treenode **var, treenode **tptr)*/
PRIVATE treenode **create_local_abbreviation (treenode **var, treenode **tptr)
{
	*var = create_abbreviation (*var, FALSE);
	while (isspecification (*tptr))
		tptr = DBodyAddr (*tptr);	/* skip over the new abbreviation */
	return tptr;
}

/*}}}  */
/*{{{  PRIVATE treenode *trans_getprotocol (treenode *type, BOOL *tagged_output)*/
PRIVATE treenode *trans_getprotocol (treenode *type, BOOL *tagged_output)
{
	switch (TagOf (type)) {
	case S_CHAN:
		type = ProtocolOf (type);
		if (TagOf (type) == N_SPROTDEF) {
			return NTypeOf (type);
		}
		if (TagOf (type) == N_TPROTDEF) {
			const int old = switch_to_temp_workspace ();
			treenode *protocol = newleafnode (S_BYTE, NOPOSN);

			switch_to_prev_workspace (old);

			*tagged_output = TRUE;
			return protocol;
		}
		return type;
	case S_PORT:
	case S_TIMER:
		return NULL;
	default:
		badtag (NOPOSN, TagOf (type), "trans_getprotocol");
	}
	return type;
}

/*}}}  */
/*{{{  PRIVATE treenode *create_commlist (const int tag, treenode *const chan, treenode *list, treenode *protocol, const BOOL tagged_output, const SOURCEPOSN locn)*/
PRIVATE treenode *create_commlist (const int tag, treenode *const chan, treenode *list, treenode *protocol, const BOOL tagged_output, const SOURCEPOSN locn)
{
	treenode *new = NULL;
	BOOL first = TRUE;

	for (; !EndOfList (list); list = NextItem (list)) {
		treenode *newcomm;
		treenode *thisprot;
		treenode *itype = NULL;

		if (tagged_output && first) {
			thisprot = protocol;
			protocol = NTypeOf (CExpOf (ThisItem (list)));
			first = FALSE;
		} else if (TagOf (protocol) == S_LIST) {
			thisprot = ThisItem (protocol);
			protocol = NextItem (protocol);
			itype = gettype_main_orig (ThisItem (list));
		} else {
			thisprot = protocol;
		}

		if (itype && (TagOf (itype) != S_ANYCHANTYPE) && (TagOf (thisprot) == S_ANYCHANTYPE)) {
			/* want these in reverse order.. */
			treenode *c1, *c2;

			trans_anychantype_io (tag, chan, ThisItem (list), &c1, &c2, locn);
			new = addtofront (c1, new);
			new = addtofront (c2, new);
		} else if (itype && (TagOf (itype) != S_ANYPROCTYPE) && (TagOf (thisprot) == S_ANYPROCTYPE)) {
			treenode *c1, *c2;

			trans_anyproctype_io (tag, chan, ThisItem (list), &c1, &c2, locn);
			new = addtofront (c1, new);
			new = addtofront (c2, new);
		} else {
			newcomm = newactionnode (tag, locn, chan, addtofront (ThisItem (list), NULL));
			SetActionType (newcomm, copytree (thisprot, trans_lexlevel));

			new = addtofront (newcomm, new);
		}
	}
	reverselist (&new);
	return new;
}

/*}}}  */
/*{{{  PRIVATE void      trans_constructor_assign */
PRIVATE void trans_constructor_assign (treenode ** tptr)
/* we have
    element := [ e0, e1, e2, e3, ..., en]
   we turn this into
    SEQ
      element[0] := e0
      ...
      element[n] := en
*/
{
	treenode *ass = *tptr;
	const SOURCEPOSN locn = LocnOf (ass);
	treenode *lhs = LHSOf (ass);
	treenode *rhs = RHSOf (ass);
	treenode *list;
#ifdef OCCAM2_5
	treenode *field_list = NULL;
#endif
	int subscript;

#if 0
fprintf (stderr, "trans_constructor_assign(): *tptr =");
printtreenl (stderr, 4, *tptr);
#endif
	/*if (!issimplelocal(lhs, trans_lexlevel)) */
	if (!issimplelocal (lhs, trans_lexlevel) && !islocal (lhs, trans_lexlevel))
		tptr = create_local_abbreviation (&lhs, tptr);

#ifdef OCCAM2_5
	if (fe_typeof (rhs) == S_RECORD) {
		treenode *const record_type = follow_user_type (fe_gettype (rhs));
		field_list = ARTypeOf (record_type);
	}
#endif

	for (list = LitExpOf (rhs), subscript = 0; !EndOfList (list); list = NextItem (list), subscript++) {
		treenode *newlhs;
		treenode *subass;
#ifdef OCCAM2_5
		if (field_list != NULL) {
			/*{{{  record subscription */
			newlhs = newarraysubnode (S_RECORDSUB, locn, lhs, DNameOf (field_list));
			field_list = DBodyOf (field_list);
			/*}}}  */
		} else
#endif
		{
			newlhs = newarraysubnode (S_ARRAYSUB, locn, lhs, newconstant (subscript));
		}
		subass = newactionnode (S_ASS, locn, newlhs, ThisItem (list));

		/* any sub-constructors are dealt with by the call to trans which is
		   done on the whole structure.
		 */

		NewItem (subass, list);
	}
	*tptr = newcnode (S_SEQ, locn, LitExpOf (rhs));

	freenode (&rhs);
	freenode (&ass);
#if 0
fprintf (stderr, "trans_constructor_assign(): returning!\n");
#endif
}

/*}}}  */
/*{{{  PRIVATE BOOL      trans_bool_case */
PRIVATE BOOL trans_bool_case (treenode ** const tptr)
/*{{{  comment about what this transformation is */
/* bug TS/1461 14/09/92 */
/*
  we turn
  CASE expression
    TRUE
      P1
    FALSE
      P2
  =>
  IF
    expression
      P1
    TRUE
      P2
and
  CASE expression
    FALSE
      P1
    TRUE
      P2
  =>
  IF
    expression
      P2
    TRUE
      P1
*/
/*}}}  */
{
	treenode *case_stmt = *tptr;
	treenode *lhs = LHSOf (case_stmt);
	treenode *rhs = RHSOf (case_stmt);
	treenode *first, *second;
	treenode *first_items, *second_items;

	/*{{{  check that we have the correct 'pattern' */
	if (listitems (rhs) != 2)
		return FALSE;

/* don't allow any leading specifications *//* (cos I'm lazy) */
	first = ThisItem (rhs);
	second = ThisItem (NextItem (rhs));
	if ((TagOf (first) != S_SELECTION) || (TagOf (second) != S_SELECTION))
		return FALSE;

	/* don't allow ELSE */
	first_items = CondGuardOf (first);
	second_items = CondGuardOf (second);
	if ((TagOf (first_items) == S_ELSE) || (TagOf (second_items) == S_ELSE))
		return FALSE;

	/*}}}  */

	/* We now know that we have two cases; the frontend will have ensured
	   that they are distinct; thus they must be TRUE and FALSE
	 */

	/*printf("In correct bool case\n"); */

	/*{{{  overwrite it with an IF */
	{
		const SOURCEPOSN locn = LocnOf (case_stmt);
		INT32 first_val, second_val;
		treenode *true_body, *false_body;
		treenode *first_guard, *second_guard, *if_stmt;

		first_val = LoValOf (ThisItem (first_items));
		second_val = LoValOf (ThisItem (second_items));

		if (first_val != 0) {
			true_body = CondBodyOf (first);
			false_body = CondBodyOf (second);
		} else {
			false_body = CondBodyOf (first);
			true_body = CondBodyOf (second);
		}

		first_guard = newcondnode (S_CHOICE, locn, lhs, true_body);
		second_guard = newcondnode (S_CHOICE, locn, newconstant (1), false_body);
		if_stmt = newcnode (S_IF, locn, newlistnode (S_LIST, locn, first_guard, newlistnode (S_LIST, locn, second_guard, NULL)));
		*tptr = if_stmt;
	}
	/*}}}  */

	/*{{{  clean up the CASE */
	/* clean up the mess */
	SetLHS (case_stmt, NULL);
	SetCondBody (first, NULL);
	SetCondBody (second, NULL);
	freetree (&case_stmt);
	/*}}}  */

	return TRUE;
}

/*}}}  */
/*{{{  PRIVATE void      create_initialised_constructor */
PRIVATE void create_initialised_constructor (treenode * const nptr)
{
	/* nptr is a VAL abbreviation, which we want to turn into an
	   initialised variable, with an explicit process which
	   does the initialisation.
	 */
	treenode *const spec = NDeclOf (nptr);
	SetTag (spec, S_DECL);
	SetTag (nptr, N_DECL);
	SetDVal (spec, newactionnode (S_ASS, LocnOf (nptr), nptr, DValOf (spec)));
	trans_constructor_assign (DValAddr (spec));
}
/*}}}  */
#ifdef MOBILES
/*{{{  PRIVATE BOOL      trans_cloned_output (treenode **tptr)*/
/*
 *
 *	for MOBILE output, we need to check for CLONEd things and transform:
 *	    foo.c ! CLONE foo.v
 *	into:
 *	    <typeof foo.v> tmp:
 *	    SEQ
 *	      tmp := CLONE foo.v
 *	      foo.c ! tmp
 *
 *	if we have something which looks like:
 *	    foo.c ! CLONE foo.v; 42; CLONE foo.w
 *	then we fiddle it into:
 *	    <typeof foo.v> tmp1:
 *	    <typeof foo.w> tmp2:
 *	    SEQ
 *	      tmp1 := CLONE foo.v
 *	      tmp2 := CLONE foo.w
 *	      foo.c ! tmp1; 42; tmp2
 *
 *	also, does a similar thing for non-mobile output on mobile channels.  transforms:
 *	    bar.c ! bar.nm
 *	into:
 *	    <typeof bar.c> tmp:
 *	    SEQ
 *	      tmp := bar.nm
 *	      bar.c ! tmp
 *
 *      and ditto for the list case.
 *
 *	returns TRUE if the tree was sufficiently altered to warrant re-processing.
 *	ths is called *before* the re-arranging that breaks down sequential IO
 */
PRIVATE BOOL trans_cloned_output (treenode **tptr, treenode ***replacepoint)
{
	treenode *comm = *tptr;
	treenode *rhs = RHSOf (comm);
	const SOURCEPOSN locn = LocnOf (comm);
	BOOL did_mangle = FALSE;
	treenode *ass_list = NULL;

#if 0
fprintf (stderr, "trans_cloned_output: transforming:");
printtreenl (stderr, 4, *tptr);
#endif
	*replacepoint = NULL;			/* used to point at where we left the output node */
	if (TagOf (rhs) == S_LIST) {
		/* walk the list */
		treenode *list;
		treenode *protocol = ActionTypeOf (comm);

		/* if no action type, extract from channel */
		if (!protocol) {
			treenode *chant = gettype (LHSOf (comm));
			if (!chant || ((TagOf (chant) != S_CHAN) && (TagOf (chant) != S_PORT))) {
				/* oops */
				msg_out (SEV_INTERNAL_BANNER, TRANS, TRANS_INTERNAL_ERROR, locn);
			}
			protocol = ProtocolOf (chant);
		}
		if (TagOf (protocol) == N_SPROTDEF) {
			protocol = NTypeOf (protocol);		/* sequential protocol */
		} else if (TagOf (protocol) == N_TPROTDEF) {
			/* sequential protocol is the type of the tag (first output) */
			treenode *outitem = ThisItem (rhs);

			/* which might (will) be a constant */
			if (TagOf (outitem) == S_CONSTEXP) {
				outitem = CExpOf (outitem);
			}
			if (TagOf (outitem) != N_TAGDEF) {
				msg_out (SEV_INTERNAL_BANNER, TRANS, TRANS_INTERNAL_ERROR, locn);
			}
			protocol = NTypeOf (outitem);

			/* skip over the tag output in the comms list */
			rhs = NextItem (rhs);
		}
#if 0
fprintf (stderr, "trans_cloned_output: output protocol/type:");
printtreenl (stderr, 4, protocol);
#endif

		for (list = rhs; !EmptyList (list) && protocol; list = NextItem (list)) {
			treenode **itemptr = ThisItemAddr (list);
			treenode *prot;

			if (TagOf (protocol) == S_LIST) {
				prot = follow_user_type (ThisItem (protocol));
				protocol = NextItem (protocol);
			} else {
				prot = follow_user_type (protocol);
			}
			if (TagOf (prot) == S_MOBILE) {
				/* this is going to be a mobile output -- inspect the item */
				treenode *itype = *itemptr;
				
				if (TagOf (itype) == S_CLONE) {
					itype = OpOf (itype);
				}
				itype = follow_user_type (gettype (itype));
#if 0
fprintf (stderr, "trans_cloned_output: output item type is: ");
printtreenl (stderr, 4, itype);
fprintf (stderr, "trans_cloned_output: output protocol is: ");
printtreenl (stderr, 4, prot);
fprintf (stderr, "trans_cloned_output: *itemptr is: ");
printtreenl (stderr, 4, *itemptr);
#endif
				if (TagOf (itype) != S_MOBILE) {
					/* non-mobile output on mobile channel, do the transform */
					treenode *mobile_tmp = create_declaration (S_DECL, N_DECL, copytree (prot, trans_lexlevel), NULL);
					treenode *ass_node;

					(void)isinmobilespace (mobile_tmp);
					ass_node = newactionnode (S_ASS, locn, mobile_tmp, *itemptr);
					ass_list = addtofront (ass_node, ass_list);
					*itemptr = mobile_tmp;
					did_mangle = TRUE;
				}
			}
			if (TagOf (*itemptr) == S_CLONE) {
				treenode *mobile_var = OpOf (*itemptr);

				/* FIXME: this [SetMOpType] should be done in the type-checker really..! */
				SetMOpType (*itemptr, S_MOBILE);

				if (!isdynmobilechantype (mobile_var)) {
					/* we need to process this one.. */
					treenode *mobile_tmp = create_declaration (S_DECL, N_DECL, copytree (gettype (mobile_var), trans_lexlevel), NULL);
					treenode *ass_node;

					(void)isinmobilespace (mobile_tmp);
					ass_node = newactionnode (S_ASS, locn, mobile_tmp, *itemptr);
					ass_list = addtofront (ass_node, ass_list);
					*itemptr = mobile_tmp;		/* change output item to new temporary */
					did_mangle = TRUE;
				}
			}
		}
		/* if did_mangle here, then tptr is likely to be different (list of declarations + comm mangled) and ass_list is the assignments list */
	} else {
		/* this should never happen, ho hum. */
		msg_out (SEV_INTERNAL_BANNER, TRANS, TRANS_RETYPE_TYPE_MISMATCH, locn);
#if 0
fprintf (stderr, "\n\n****** trans_cloned_output: impossible 1\n\n\n");
#endif
		return FALSE;
	}
	if (did_mangle) {
		treenode *proclist, *tl;

		/* skip new declarations and insert SEQ node */
		while (isspecification (*tptr)) {
			/* skip declaration */
			tptr = DBodyAddr (*tptr);
		}
		/* *tptr now at output node, find last item in assignment list and bung on modified output */
		for (proclist = ass_list; !IsLastItem (proclist); proclist = NextItem (proclist));
		tl = newlistnode (S_LIST, NOPOSN, *tptr, NULL);
		SetRight (proclist, tl);
		*replacepoint = ThisItemAddr (tl);

		*tptr = newcnode (S_SEQ, locn, ass_list);
	}
#if 0
if(did_mangle){
fprintf (stderr, "  into:");
printtreenl (stderr, 4, *tptr);
}
#endif
	return did_mangle;
}
/*}}}*/
/*{{{  PRIVATE treenode *create_mdim_sizetree (SOURCEPOSN locn, treenode *tptr)*/
/*
 *	for some tree structure, returns a constant tree structure
 *	representing the size IN ELEMENTS.  used by trans_process_dynmobile_assign()
 */
PRIVATE treenode *create_mdim_sizetree (SOURCEPOSN locn, treenode *tptr)
{
	treenode *t;

#if 0
fprintf (stderr, "create_mdim_sizetree: tptr =");
printtreenl (stderr, 4, tptr);
#endif
#if 1
	if (TagOf (tptr) == S_CONSTCONSTRUCTOR) {
		treenode **pt;

		/* build the size tree manually */
		int looping = 1;

		t = NULL;
		pt = &t;
		tptr = ConstTableTypeOf (tptr);

		for (; looping;) {
			if (!tptr) {
				/* oops! */
				msg_out (SEV_INTERNAL_BANNER, TRANS, TRANS_INTERNAL_ERROR, locn);
				return NULL;
			}
			switch (TagOf (tptr)) {
			case S_ARRAY:
				if (TagOf (ARTypeOf (tptr)) == S_ARRAY) {
					/* more dimensions, setup TIMES dopnode */
					*pt = newdopnode (S_TIMES, locn, newconstant (ARDimOf (tptr)), NULL, S_INT);
					pt = RightOpAddr (*pt);
				} else {
					/* last/single dimension, just count */
					*pt = newconstant (ARDimOf (tptr));
				}
				tptr = ARTypeOf (tptr);
				break;
			default:
				looping = 0;
				/* *pt = newconstant (bytesin (tptr)); */
				break;
			}
		}

	} else
#endif
	{
		t = newmopnode (S_SIZE, locn, tptr, S_INT);
	}
	if (isconst (t)) {
		/* fold up size expression if possible */
		t = foldexp (t);
	}
#if 0
fprintf (stderr, "created tree = ");
printtreenl (stderr, 4, t);
#endif

	return t;
}
/*}}}*/
/*{{{  PRIVATE BOOL      trans_process_dynmobile_assign (treenode **tptr)*/
/*
 *
 *	for dynamic MOBILE assignment, check for non-dynmob RHS and transform:
 *	    mob := non.mob
 *	into:
 *	    SEQ
 *	      mob := [SIZE non.mob]<mtypeof mob>
 *	      mob := non.mob
 *
 * 	returns a pointer to the original assignment (now buried in the newly created SEQ)
 */
PRIVATE treenode **trans_process_dynmobile_assign (treenode **tptr)
{
	treenode *rhs = RHSOf (*tptr);
	treenode **rhs_ptr = RHSAddr (*tptr);
	treenode **savedptr;
	const SOURCEPOSN locn = LocnOf (*tptr);
	treenode *dmobile_var, *dmobile_copy, *size_node, *ass_node, *proclist, *type_copy, *rhs_copy;
	BOOL isundef;
	int undef_count;

#if 0
fprintf (stderr, "trans_process_dynmobile_assign: transforming:");
printtreenl (stderr, 4, *tptr);
#endif
	if (TagOf (rhs) == S_LIST) {
		/* shouldn't happen, but just in case.. */
		rhs_ptr = ThisItemAddr (rhs);
		rhs = ThisItem (rhs);
	}
	if (TagOf (rhs) == S_NEW_ARRAY) {
		return NULL;
	}
	/* call trans on LHS first */
	transexp (LHSAddr (*tptr), FALSE);
	dmobile_var = LHSOf (*tptr);
	if (TagOf (dmobile_var) == S_UNDEFINED) {
		treenode **uaddr = LHSAddr (*tptr);

		isundef = TRUE;
		undef_count = MOpTypeOf (dmobile_var);
		dmobile_var = OpOf (dmobile_var);
		/* remove the UNDEFINED node from the original lhs -- FIXME: should deallocate it really.. */
		*uaddr = dmobile_var;
	} else {
		isundef = FALSE;
		undef_count = -1;
	}
	/* there's the possibility that the RHS is non-trivial, in which case extracting SIZE might be fun.. */
	while (isspecification (rhs)) {
		rhs = DBodyOf (rhs);
	}
	switch (TagOf (rhs)) {
	case S_VALOF:
		{
			treenode *temp;

			temp = ThisItem (VLResultListOf (rhs));
			switch (TagOf (temp)) {
			case N_DECL:
			case N_ABBR:
			case N_VALABBR:
				temp = NTypeOf (temp);
				break;
			default:
				badtag (trans_locn, TagOf (temp), "trans_process_dynmobile_assign:VALOF");
				break;
			}
#if 0
fprintf (stderr, "trans_process_dynmobile_assign: about to copy size_node from some VALOF process, NTypeOf (result) is ");
printtreenl (stderr, 4, temp);
#endif
			size_node = NULL;
			while (TagOf (temp) == S_ARRAY) {
				size_node = newlistnode (S_LIST, locn, copytree (ARDimLengthOf (temp), trans_lexlevel), size_node);
				temp = ARTypeOf (temp);
			}
			/* process the VALOF */
			sub_transexp (rhs_ptr, FALSE);
		}
		break;
	default:
		rhs_copy = copytree (rhs, trans_lexlevel);
		size_node = create_mdim_sizetree (locn, rhs_copy);
		/* process the RHS */
		sub_transexp (rhs_ptr, FALSE);
		break;
	}
	type_copy = MTypeOf (follow_user_type (gettype (dmobile_var)));
	while (TagOf (type_copy) == S_ARRAY) {
		type_copy = ARTypeOf (type_copy);
	}
	type_copy = copytree (type_copy, trans_lexlevel);
	size_node = newtypenode (S_NEW_ARRAY, locn, size_node, type_copy);
	dmobile_copy = copytree (dmobile_var, trans_lexlevel);
	if (isundef) {
		dmobile_copy = newmopnode (S_UNDEFINED, locn, dmobile_copy, undef_count);
	}
	ass_node = newactionnode (S_ASS, locn, dmobile_copy, size_node);
#if 0
fprintf (stderr, "trans_process_dynmobile_assign: before trans(ass_node), ass_node:");
printtreenl (stderr, 4, ass_node);
#endif
	/* transform the newly created assignment to get type-size multiplication in there */
	trans (&ass_node);
	proclist = addtofront (*tptr, NULL);
	savedptr = ThisItemAddr (proclist);
	proclist = addtofront (ass_node, proclist);
	*tptr = newcnode (S_SEQ, locn, proclist);
#if 0
fprintf (stderr, "trans_process_dynmobile_assign: done mangling assign, new tree is:");
printtreenl (stderr, 4, *tptr);
#endif
	return savedptr;
}/*}}}*/
/*{{{  PRIVATE void trans_anychantype_io (const int tag, treenode *const chan, treenode *const item, treenode **hashio, treenode **chanio, const SOURCEPOSN locn)*/
/*
 *	this breaks up communications for an ANYCHANTYPE (MOBILE.CHAN) protocol
 *	only ever used for output
 */
PRIVATE void trans_anychantype_io (const int tag, treenode *const chan, treenode *const item, treenode **hashio, treenode **chanio, const SOURCEPOSN locn)
{
	treenode *itype;
	treenode *hashnode;
	
	if (TagOf (item) == S_CLONE) {
		itype = gettype_main_orig (OpOf (item));
	} else {
		itype = gettype_main_orig (item);
	}

#if 0
fprintf (stderr, "tran1: trans_anychantype_io: tag = %s, chan = ", tagstring (tag));
printtreenl (stderr, 4, chan);
fprintf (stderr, "tran1: trans_anychantype_io: item = ");
printtreenl (stderr, 4, item);
fprintf (stderr, "tran1: trans_anychantype_io: itype = ");
printtreenl (stderr, 4, itype);
#endif
	hashnode = newmopnode (S_TYPEHASHOF, locn, item, S_INT);

	*hashio = newactionnode (tag, locn, chan, addtofront (hashnode, NULL));
	SetActionType (*hashio, newleafnode (S_INT, locn));
	*chanio = newactionnode (tag, locn, chan, addtofront (item, NULL));
	SetActionType (*chanio, itype);

	return;
}
/*}}}*/
/*{{{  PRIVATE void trans_anyproctype_io (const int tag, treenode *const chan, treenode *const item, treenode **hashio, treenode **chanio, const SOURCEPOSN locn)*/
/*
 *	this breaks up communications for an ANYPROCTYPE (MOBILE.PROC) protocol
 *	only ever used for output
 */
PRIVATE void trans_anyproctype_io (const int tag, treenode *const chan, treenode *const item, treenode **hashio, treenode **chanio, const SOURCEPOSN locn)
{
	treenode *itype;
	treenode *hashnode;

	/* CLONE not supported yet.. */
	itype = gettype_main_orig (item);

	hashnode = newmopnode (S_TYPEHASHOF, locn, item, S_INT);

	*hashio = newactionnode (tag, locn, chan, addtofront (hashnode, NULL));
	SetActionType (*hashio, newleafnode (S_INT, locn));
	*chanio = newactionnode (tag, locn, chan, addtofront (item, NULL));
	SetActionType (*chanio, itype);

	return;
}
/*}}}*/
#endif	/* MOBILES */
/*{{{  PRIVATE BOOL      trans_process_sequential_io */
PRIVATE BOOL trans_process_sequential_io (treenode ** tptr)
/* We turn sequential communications into a sequence of
   single communications
*/
/* returns TRUE if it has modified things substantially enough to need
   to recursively call trans on the result.
*/
{
	BOOL modified = FALSE;
	treenode *comm = *tptr;
	treenode *chan = LHSOf (comm);
	treenode *type = gettype (chan);	/* will always be CHAN OF/PORT OF/TIMER */
	const int n = listitems (RHSOf (comm));
	BOOL tagged_output = FALSE;
	treenode *protocol = trans_getprotocol (type, &tagged_output);
	const BOOL is_extended = (TagOf (comm) == S_X_INPUT);

#if 0
fprintf (stderr, "trans_process_sequential_io:  *tptr =");
printtreenl (stderr, 4, *tptr);
#endif
	if ((n > 1) && !issimplelocal (chan, trans_lexlevel)) {
		/*{{{  abbreviate the channel expression */
		tptr = create_local_abbreviation (&chan, tptr);
		modified = TRUE;
		/*}}}  */
	}
	if (n > 1) {
		/* Can't be a PORT or TIMER */
		/*{{{  turn it into a sequence of communications */
		const SOURCEPOSN locn = LocnOf (comm);
		treenode *rhs;
		treenode *new = NULL;
		treenode **replacepoint = NULL;

#ifdef MOBILES
		/* transform CLONEd outputs -- before breaking into list */
		if ((TagOf (comm) == S_OUTPUT) && trans_cloned_output (tptr, &replacepoint)) {
			modified = TRUE;
			/* watch-it: trans may have inserted declarations, modifying *tptr, for some MOBILE things, assignments may have appeared too
			 * -- original OUTPUT node is left stored at replacepoint */
		}
#endif
#if 0
fprintf (stderr, "trans_process_sequential_io: n>1, done any trans_cloned_output: *tptr =");
printtreenl (stderr, 4, *tptr);
#endif
		if (is_extended) {
			treenode *tmp;

			/* latch the during and after processes onto the last item */
			new = create_commlist (S_INPUT, chan, RHSOf (comm), protocol, tagged_output, locn);
			for (tmp = new; tmp && !IsLastItem (tmp); tmp = NextItem (tmp));
			if (tmp) {
				tmp = ThisItem (tmp);
				SetTag (tmp, S_X_INPUT);
				SetActionDuring (tmp, ActionDuringOf (comm));
				SetActionDuring (comm, NULL);
				/* for counted array stuff, we'll probably trans this twice, but non-fatal */
				trans (ActionDuringAddr (tmp));
				SetActionAfter (tmp, ActionAfterOf (comm));
				SetActionAfter (comm, NULL);
				trans (ActionAfterAddr (tmp));
				SetTag (comm, S_INPUT);
			}
		} else {
			new = create_commlist (TagOf (comm), chan, RHSOf (comm), protocol, tagged_output, locn);
		}
		if (replacepoint) {
			*replacepoint = newcnode (S_SEQ, locn, new);
		} else {
			*tptr = newcnode (S_SEQ, locn, new);
		}
		modified = TRUE;

		rhs = RHSOf (comm);
		while (rhs != NULL) {
			treenode *temp = NextItem (rhs);
			freenode (&rhs);
			rhs = temp;
		}
		freenode (&comm);
		/*}}}  */
	} else if (protocol != NULL) {
		/* NULL means PORT or TIMER */
		/*{{{  single communication */
		treenode **dummy;
		const SOURCEPOSN locn = LocnOf (comm);

		if (TagOf (protocol) == S_LIST) {
			protocol = ThisItem (protocol);
		}

		if ((TagOf (protocol) == S_ANYCHANTYPE) && (TagOf (comm) == S_OUTPUT) && (TagOf (gettype_main_orig (ThisItem (RHSOf (comm)))) != S_ANYCHANTYPE)) {
			treenode *c1, *c2;

			trans_anychantype_io (TagOf (comm), chan, ThisItem (RHSOf (comm)), &c1, &c2, locn);
			trans_cloned_output (&c2, &dummy);
			*tptr = newcnode (S_SEQ, locn, addtofront (c1, addtofront (c2, NULL)));
			modified = TRUE;
		} else if ((TagOf (protocol) == S_ANYPROCTYPE) && (TagOf (comm) == S_OUTPUT) && (TagOf (gettype_main_orig (ThisItem (RHSOf (comm)))) != S_ANYPROCTYPE)) {
			treenode *c1, *c2;

			trans_anyproctype_io (TagOf (comm), chan, ThisItem (RHSOf (comm)), &c1, &c2, locn);
			trans_cloned_output (&c2, &dummy);
			*tptr = newcnode (S_SEQ, locn, addtofront (c1, addtofront (c2, NULL)));
			modified = TRUE;
		} else {

			SetActionType (comm, copytree (protocol, trans_lexlevel));
#ifdef MOBILES
			if ((TagOf (comm) == S_OUTPUT) && trans_cloned_output (tptr, &dummy)) {
				modified = TRUE;
			}
#endif
		}
		if (TagOf (comm) == S_X_INPUT) {
			trans (ActionDuringAddr (comm));
			trans (ActionAfterAddr (comm));
		}
		/*}}}  */
	}
#if 0
fprintf (stderr, "trans_process_sequential_io (out): modified=%d, *tptr =", modified);
printtreenl (stderr, 4, *tptr);
#endif
	return modified;
}

/*}}}  */
/*{{{  PRIVATE int       trans_check_prot_empty_tags (treenode *prot)*/
/*
 *	this returns the number of empty (data-less) tags in a PROTOCOL
 */
PRIVATE int trans_check_prot_empty_tags (treenode *prot)
{
	treenode *tlist;
	int num_empty = 0;

	if (TagOf (prot) != N_TPROTDEF) {
		/* shouldn't ever happen.. */
		return -1;
	}
	tlist = NTypeOf (prot);

	while (!EmptyList (tlist)) {
		treenode *tag = ThisItem (tlist);

		if (TagOf (tag) == N_TAGDEF) {
			if (!NTypeOf (tag)) {
				num_empty++;
			}
		}
		tlist = NextItem (tlist);
	}
	return num_empty;
}
/*}}}*/
/*{{{  PRIVATE void      trans_process_tagged_input */
/*{{{  comment */
/* we turn:
  chan ? CASE tag ; exp1; exp2
into
  c IS chan :  -- optional
  BYTE b :
  SEQ
    c ? b
    IF
      b = tag
	SKIP
    c ? exp1
    c ? exp2
*/
/*}}}  */
PRIVATE void trans_process_tagged_input (treenode **tptr)
{
	treenode *comm = *tptr;
	treenode *chan = LHSOf (comm);
	const SOURCEPOSN locn = LocnOf (comm);
	treenode *byte_var;
	const BOOL is_extended = (TagOf (comm) == S_X_TAGGED_INPUT);
	const int n_items = listitems (RHSOf (comm));
	BOOL did_extend_tag_input = FALSE;
	treenode *prot = ProtocolOf (gettype (chan));
	const BOOL is_act = (TagOf (prot) == S_ANYCHANTYPE);

#if 0
fprintf (stderr, "transprocess_tagged_input(): prot = ");
printtreenl (stderr, 4, prot);
#endif
	/*{{{  channel and BYTE (or INT) temp declaration */
	if ((n_items > 1) && !issimplelocal (chan, trans_lexlevel)) {
		tptr = create_local_abbreviation (&chan, tptr);
	}
	byte_var = create_declaration (S_DECL, N_DECL, newleafnode (is_act ? S_INT : S_BYTE, locn), NULL);
	while (isspecification (*tptr)) {
		tptr = DBodyAddr (*tptr);	/* skip over the new declaration */
	}
	/*}}}  */

	{
		treenode *rhs = RHSOf (comm);
		treenode *tag = ThisItem (rhs);
		treenode *tagcomm;
		treenode *commlist;
		treenode *rhstype = NULL;
		int nitems;

		if (is_extended) {
			if ((n_items == 1) && !is_act) {
				tagcomm = newactionnodex (S_X_INPUT, locn, chan, addtofront (byte_var, NULL), ActionDuringOf (comm), ActionAfterOf (comm));
				commlist = create_commlist (S_INPUT, chan, NextItem (rhs), NTypeOf (CExpOf (tag)), FALSE, locn);
				SetTag (comm, S_TAGGED_INPUT);
				SetActionDuring (comm, NULL);
				SetActionAfter (comm, NULL);
			} else {
				treenode *tmp;

				tagcomm = newactionnode (S_INPUT, locn, chan, addtofront (byte_var, NULL));
				if (is_act) {
					rhstype = gettype_main_orig (ThisItem (rhs));

					SetActionFlags (tagcomm, ActionFlagsOf (tagcomm) | ActionFlag_anychantypehash);
					commlist = addtofront (newactionnode (S_INPUT, locn, chan, rhs), NULL);
					SetActionType (ThisItem (commlist), rhstype);
				} else {
					commlist = create_commlist (S_INPUT, chan, NextItem (rhs), NTypeOf (CExpOf (tag)), FALSE, locn);
				}
				for (tmp = commlist; tmp && !IsLastItem (tmp); tmp = NextItem (tmp));
				if (tmp) {
					tmp = ThisItem (tmp);
					SetTag (tmp, S_X_INPUT);
					SetActionDuring (tmp, ActionDuringOf (comm));
					SetActionAfter (tmp, ActionAfterOf (comm));
					trans (ActionDuringAddr (tmp));
					SetTag (comm, S_TAGGED_INPUT);
					SetActionDuring (comm, NULL);
					SetActionAfter (comm, NULL);
					trans (ActionAfterAddr (tmp));
				}
			}
		} else if (tagged_input_tag_xin && extended_input && !is_act) {
			/* do extended input on the tag, if it meets the criteria */
			treenode *chantype = gettype_main_orig (chan);
			int prot_empty_tags = 0;
			int this_empty = 0;

#if 0
fprintf (stderr, "trans_process_tagged_input(): chantype =");
printtreenl (stderr, 4, chantype);
fprintf (stderr, "trans_process_tagged_input(): chan =");
printtreenl (stderr, 4, chan);
#endif
			prot_empty_tags = trans_check_prot_empty_tags (ProtocolOf (chantype));
			if ((TagOf (CExpOf (tag)) == N_TAGDEF) && !NTypeOf (CExpOf (tag))) {
				this_empty = 1;
			}
#if 0
fprintf (stderr, "trans_process_tagged_intpu: tag = ");
printtreenl (stderr, 4, tag);
fprintf (stderr, "trans_process_tagged_input: protocol has %d empties.  this_empty = %d\n", prot_empty_tags, this_empty);
#endif
			if (prot_empty_tags > this_empty) {
				treenode *during_proc = newmopnode (S_X_FIRST_HALF, locn, newleafnode (S_SKIP, locn), S_CHAN);

				/* need to do extended input, since some are unhandled */
				tagcomm = newactionnodex (S_X_INPUT, locn, chan, addtofront (byte_var, NULL), during_proc, newleafnode (S_SKIP, locn));
				did_extend_tag_input = TRUE;
			} else {
				/* default behaviour */
				tagcomm = newactionnode (S_INPUT, locn, chan, addtofront (byte_var, NULL));
			}
			commlist = create_commlist (S_INPUT, chan, NextItem (rhs), NTypeOf (CExpOf (tag)), FALSE, locn);
		} else {
			tagcomm = newactionnode (S_INPUT, locn, chan, addtofront (byte_var, NULL));

			if (is_act) {
				rhstype = gettype_main_orig (ThisItem (rhs));

				SetActionFlags (tagcomm, ActionFlagsOf (tagcomm) | ActionFlag_anychantypehash);
				commlist = addtofront (newactionnode (S_INPUT, locn, chan, rhs), NULL);
				SetActionType (ThisItem (commlist), rhstype);
			} else {
				commlist = create_commlist (S_INPUT, chan, NextItem (rhs), NTypeOf (CExpOf (tag)), FALSE, locn);
			}
		}

		SetActionType (tagcomm, newleafnode (is_act ? S_INT : S_BYTE, locn));

		if (is_act) {
#if 0
fprintf (stderr, "ANYCHANTYPE tagged input: rhstype =");
printtreenl (stderr, 4, rhstype);
#endif
			tag = newconstant (typehash (rhstype));
			nitems = 2;
		} else {
			nitems = listitems (NTypeOf (ProtocolOf (gettype (chan))));
		}


/*		if (NEED_ERRORS && (listitems (NTypeOf (ProtocolOf (basetype_tree (NTypeOf (nameof (chan)))))) > 1)) */
		if (NEED_ERRORS && (nitems > 1)) {
			int bvtype = is_act ? S_INT : S_BYTE;
#if 0
			treenode *const eq = newdopnode (S_EQ, locn, byte_var, tag, S_BYTE);
#else
			treenode *const eq = (LoValOf (tag) == 0)
				? newmopnode (S_NOT, locn, byte_var, bvtype)
				: newdopnode (S_EQ, locn, byte_var, tag, bvtype);
#endif
			treenode *const guard = newcondnode (S_CHOICE, locn, eq, newleafnode (S_SKIP, locn));
			treenode *const if_test = newcnode (S_IF, locn, addtofront (guard, NULL));

			if (did_extend_tag_input) {
				treenode *during_proc = newmopnode (S_X_SECOND_HALF, locn, newleafnode (S_SKIP, locn), S_CHAN);
				treenode *after_proc = newleafnode (S_SKIP, locn);
				treenode *const inputnode = newactionnodex (S_X_INPUT, locn, copytree (chan, trans_lexlevel), NULL, during_proc, after_proc);

				*tptr = newcnode (S_SEQ, locn, addtofront (tagcomm, addtofront (if_test, addtofront (inputnode, commlist))));
			} else {
				*tptr = newcnode (S_SEQ, locn, addtofront (tagcomm, addtofront (if_test, commlist)));
			}
		} else {
			/* bug TS/2070 29/01/93 */
			*tptr = newcnode (S_SEQ, locn, addtofront (tagcomm, commlist));
		}


		/* now free up the old structure */
		if (!is_act) {
			treenode *temp = NextItem (rhs);
			while (!EndOfList (temp)) {
				treenode *temp2 = NextItem (temp);
				freenode (&temp);
				temp = temp2;
			}
			freenode (&rhs);
		}
		freenode (&comm);
	}
}

/*}}}  */
/*{{{  PRIVATE int trans_check_empty_tags (treenode *vlist)*/
/*
 *	returns number of empty tags in the variant list `vlist'.
 *	needed to decide if extended input does extended tag in..
 */
PRIVATE BOOL trans_check_empty_tags (treenode *vlist)
{
	int have_empty = 0;

	while (!EndOfList (vlist)) {
		treenode *const variant = skipspecifications (ThisItem (vlist));
		treenode *const explist = VRTaggedListOf (variant);

		if (IsLastItem (explist)) {
			have_empty++;
		}
		vlist = NextItem (vlist);
	}
	return have_empty;
}
/*}}}  */
/*{{{  PRIVATE void      trans_process_case_input */
/*{{{  comment */
/* we turn:
  chan ? CASE
    VAL zz IS 99 :
    tag0
      P
    tag1 ; x ; y
      Q
into
  c IS chan :  -- optional
  BYTE b :
  SEQ
    c ? b
    CASE b
      VAL zz IS 99 :
      tag0
	P
      tag1
	SEQ
	  c ? x
	  c ? y
	  Q
*/
/*
 * frmb: slight variation for ANY CHAN TYPE inputs
 */
/*}}}  */
PRIVATE void trans_process_case_input (treenode **tptr)
{
	treenode *comm = *tptr;
	treenode *chan = LHSOf (comm);
	const SOURCEPOSN locn = LocnOf (comm);
	treenode *byte_var;
	const BOOL is_extended = (TagOf (comm) == S_X_CASE_INPUT);
	const BOOL have_empty_tags = (is_extended && trans_check_empty_tags (RHSOf (comm)));
	BOOL did_extend_tag_input = FALSE;
	treenode *prot = ProtocolOf (gettype (chan));
	const BOOL is_act = (TagOf (prot) == S_ANYCHANTYPE);

#if 0
fprintf (stderr, "tran1: trans_process_case_input(): prot = ");
printtreenl (stderr, 4, prot);
#endif
	/*{{{  channel and BYTE temp declaration (or INT for any-chan-type) */
	if (!issimplelocal (chan, trans_lexlevel)) {
		tptr = create_local_abbreviation (&chan, tptr);
	}
	byte_var = create_declaration (S_DECL, N_DECL, newleafnode (is_act ? S_INT : S_BYTE, locn), NULL);
	while (isspecification (*tptr)) {
		tptr = DBodyAddr (*tptr);	/* skip over the new declaration */
	}
	/*}}}  */

	/*{{{  create a CASE statement */
	{
		treenode *tagcomm;

		if (have_empty_tags) {
			treenode *during_proc = newmopnode (S_X_FIRST_HALF, locn, newleafnode (S_SKIP, locn), S_CHAN);
			treenode *after_proc = newleafnode (S_SKIP, locn);

			/* do an extended input on the tag, and an extended input on the last data item */
#if 0
fprintf (stderr, "trans_process_case_input: looks like we have empty tags here, generating half extended input on tag\n");
#endif
			tagcomm = newactionnodex (S_X_INPUT, locn, chan, addtofront (byte_var, NULL), during_proc, after_proc);
		} else if (tagged_input_tag_xin && extended_input) {
			/* do an extended input on the tag, if the CASE has unhandled empties */
			int prot_empty_tags = trans_check_prot_empty_tags (ProtocolOf (NTypeOf (chan)));
			int this_empty = trans_check_empty_tags (RHSOf (comm));

#if 0
fprintf (stderr, "trans_process_case_input: protocol has %d empties.  this_empty = %d\n", prot_empty_tags, this_empty);
#endif
			if (prot_empty_tags > this_empty) {
				treenode *during_proc = newmopnode (S_X_FIRST_HALF, locn, newleafnode (S_SKIP, locn), S_CHAN);
				treenode *after_proc = newleafnode (S_SKIP, locn);

				tagcomm = newactionnodex (S_X_INPUT, locn, chan, addtofront (byte_var, NULL), during_proc, after_proc);
				did_extend_tag_input = TRUE;
			} else {
				/* default behaviour */
				tagcomm = newactionnode (S_INPUT, locn, chan, addtofront (byte_var, NULL));
			}
		} else {
			tagcomm = newactionnode (S_INPUT, locn, chan, addtofront (byte_var, NULL));
		}
		SetActionType (tagcomm, newleafnode (is_act ? S_INT : S_BYTE, locn));

		if (nodetypeoftag (S_CASE_INPUT) != nodetypeoftag (S_CASE)) {
			abort ();
		}

		if (is_act) {
			SetActionFlags (tagcomm, ActionFlagsOf (tagcomm) | ActionFlag_anychantypehash);
		}

		SetTag (comm, S_CASE);
		SetLHS (comm, byte_var);

		*tptr = newcnode (S_SEQ, locn, addtofront (tagcomm, addtofront (comm, NULL)));
	}
	/*}}}  */

	/*{{{  process each of the variants */
	{
		treenode *vlist;

		/* check that we can convert a S_VARIANT into an S_SELECTION safely */
		if (nodetypeoftag (S_VARIANT) != nodetypeoftag (S_SELECTION)) {
			abort (); 
		}

		for (vlist = RHSOf (comm); !EndOfList (vlist); vlist = NextItem (vlist)) {
			treenode *const variant = skipspecifications (ThisItem (vlist));
			treenode *exp = VRTaggedListOf (variant);
			treenode *etype = NULL;
			const SOURCEPOSN comm_locn = LocnOf (variant);	/* bug INSdi02088 05/04/93 */
			treenode *commlist;
			
			if (is_act) {
				etype = gettype_main_orig (ThisItem (exp));

				commlist = addtofront (newactionnode (S_INPUT, comm_locn, chan, exp), NULL);
#if 0
fprintf (stderr, "*** here: commlist = ");
printtreenl (stderr, 4, commlist);
fprintf (stderr, "*** here: etype = ");
printtreenl (stderr, 4, etype);
#endif
				SetActionType (ThisItem (commlist), etype);
				/* commlist = create_commlist (S_INPUT, chan, exp, fe_gettype (ThisItem (exp)), FALSE, comm_locn); */
				if (kroc_chantype_desc) {
					/*{{{  special handling for channel-type type descriptions*/
					/*
					 *	NOTE: this is Not Nice.  We have to match on the type-hash of the channel-type,
					 *	not the particular channel-end.  Thus, CASE input can't distinguish between ends
					 *	of different types..
					 */
					etype = NTypeOf (etype);
					/*}}}*/
				}
			} else {
				commlist = create_commlist (S_INPUT, chan, NextItem (exp), NTypeOf (CExpOf (ThisItem (exp))), FALSE, comm_locn);
			}

			if (is_extended) {
				/*{{{  handle extended input (During/After variants)*/
				treenode *tmp;
				treenode *const d_proc = VRDuringOf (variant);
				treenode *const a_proc = VRAfterOf (variant);

#if 0
fprintf (stderr, "trans_process_case_input: (is_extended) variant =");
printtreenl (stderr, 4, variant);
#endif
				SetVRDuring (variant, NULL);
				SetVRAfter (variant, NULL);
				SetTag (variant, S_VARIANT);
				SetVRBody (variant, newleafnode (S_SKIP, comm_locn));		/* replace variant body */
				for (tmp = skipspecifications (commlist); tmp && !IsLastItem (tmp); tmp = NextItem (tmp));
				if (tmp) {
					tmp = ThisItem (tmp);
					SetTag (tmp, S_X_INPUT);
					SetActionDuring (tmp, d_proc);
					SetActionAfter (tmp, a_proc);
					if (have_empty_tags) {
						/* make releasing node and bung it on the the front */
						treenode *during_proc = newmopnode (S_X_SECOND_HALF, locn, newleafnode (S_SKIP, locn), S_CHAN);
						treenode *after_proc = newleafnode (S_SKIP, locn);
						treenode *const inputnode = newactionnodex (S_X_INPUT, locn, copytree (chan, trans_lexlevel),
											    NULL, during_proc, after_proc);

						/* trans extended processes */
						trans (ActionDuringAddr (inputnode));
						trans (ActionAfterAddr (inputnode));
						commlist = newlistnode (S_LIST, locn, inputnode, commlist);
					}
				} else {
					/* no tags.  make releasing node */
					treenode *const inputnode = newactionnodex (S_X_INPUT, locn, copytree (chan, trans_lexlevel),
										    NULL, newmopnode (S_X_SECOND_HALF, locn, d_proc, S_CHAN), a_proc);
					/* trans extended processes */
					trans (ActionDuringAddr (inputnode));
					trans (ActionAfterAddr (inputnode));
					commlist = newlistnode (S_LIST, locn, inputnode, NULL);
				}
#if 0
fprintf (stderr, "trans_process_case_input: (is_extended) commlist =");
printtreenl (stderr, 4, commlist);
#endif
				/*}}}*/
			} else if (did_extend_tag_input) {
				/*{{{  ensure first thing done is release of the outputter*/
				treenode *during_proc = newmopnode (S_X_SECOND_HALF, locn, newleafnode (S_SKIP, locn), S_CHAN);
				treenode *after_proc = newleafnode (S_SKIP, locn);
				treenode *const tmp = newactionnodex (S_X_INPUT, locn, copytree (chan, trans_lexlevel), NULL, during_proc, after_proc);

#if 0
fprintf (stderr, "trans_process_case_input: (did_extend_tag_input): want to release outputter, commlist = ");
printtreenl (stderr, 4, commlist);
#endif
				commlist = addtofront (tmp, commlist);
				/*}}}*/
			}

			/*{{{  free up the old input list */
			if (!is_act) {
				treenode *t;
				
				t = NextItem (exp);
				while (!EndOfList (t)) {
					treenode *const temp = NextItem (t);
					freenode (&t);
					t = temp;
				}
			}

			if (is_act) {
#if 0
fprintf (stderr, "trans_process_case_input(): setting variant constant (typehash) = 0x%8.8x, etype =", typehash (etype));
printtreenl (stderr, 4, etype);
#endif
				SetVRTaggedList (variant, addtofront (newconstant (etype ? typehash(etype) : 0), NULL));
			} else {
				NewNextItem (NULL, exp);	/* now only one item in list */
			}

			/*}}}  */

			SetTag (variant, S_SELECTION);

			if (commlist != NULL) {
				SetCondBody (variant, newcnode (S_SEQ, comm_locn, appendlist (addtofront (CondBodyOf (variant), NULL), commlist)));
			}
		}
	}
	/*}}}  */

}

/*}}}  */
/*{{{  PRIVATE treenode *create_slice_io */
PRIVATE treenode *create_slice_io (const int tag, const SOURCEPOSN locn, treenode * const chan, treenode * const array, treenode * const length)
{
	treenode *array_slice;
	treenode *array_comm;

	array_slice = newsegmentnode (S_SEGMENT, locn, array, newconstant (0), length);
	if (tag == S_OUTPUT)
		array_slice = foldexp (array_slice);

	array_comm = newactionnode (tag, locn, chan, addtofront (array_slice, NULL));
	SetActionType (array_comm, copytree (gettype (array_slice), trans_lexlevel));
	return array_comm;
}

/*}}}  */
/*{{{  PRIVATE void      trans_process_counted_io */
PRIVATE void trans_process_counted_io (treenode ** tptr)
/*{{{  comment */
/* we turn:
  chan ? len :: array
into
  c IS chan      :  -- optional
  type input.var :  -- optional
  INT slice.var  :  -- optional
  SEQ
    c ? input.var
    len := input.var           -- optional
    slice.var := INT input.var -- optional
    IF
      slice.var <> 0
	c ? [array FROM 0 FOR slice.var]
      TRUE
	SKIP
*/
/* we turn:
  chan ! len :: array
into
  c IS chan       :  -- optional
  type output.var :  -- optional
  INT slice.var   :  -- optional
  SEQ
    output.var := len           -- optional
    c ! output.var
    slice.var := INT output.var -- optional
    IF                          -- optional
      slice.var <> 0            -- optional
	c ! [array FROM 0 FOR slice.var]
      TRUE
	SKIP
*/
/*}}}  */
{
	treenode *comm = *tptr;
	treenode *chan = LHSOf (comm);
	const SOURCEPOSN locn = LocnOf (comm);
	treenode *const length_exp = LeftOpOf (ThisItem (RHSOf (comm)));
	treenode *array = RightOpOf (ThisItem (RHSOf (comm)));
	const int length_type = ntypeof (length_exp);
	const BOOL nonsimple = !issimplelocal (length_exp, trans_lexlevel)
		|| (nodetypeoftag (TagOf (length_exp)) != NAMENODE);	/* bug INSdi01894 23/03/93 */
	const BOOL noninteger = (length_type != S_INT) && (length_type != targetintsize);
	treenode *p;

#if 0
fprintf (stderr, "trans_process_counted_io (in): *tptr =");
printtreenl (stderr, 4, *tptr);
#endif
	if (!issimplelocal (chan, trans_lexlevel)) {
		tptr = create_local_abbreviation (&chan, tptr);
	}

	if ((TagOf (comm) == S_INPUT) || (TagOf (comm) == S_X_INPUT)) {
		/*{{{  counted input */
		treenode *input_var;
		treenode *slice_var;
		treenode *length_comm;
		treenode *array_comm;
		treenode *guard;
		const BOOL is_extended = (TagOf (comm) == S_X_INPUT);

		/*{{{  length temps */
		if (nonsimple) {
			input_var = create_declaration (S_DECL, N_DECL, newleafnode (length_type, locn), NULL);
		} else {
			input_var = length_exp;
		}

		if (noninteger) {
			slice_var = create_declaration (S_DECL, N_DECL, newleafnode (S_INT, locn), NULL);
		} else {
			slice_var = input_var;
		}

		if (RANGECHECKING && !issimple (array))	{
			/* bug TS/1172 27/04/92 */
			array = create_abbreviation (array, FALSE);
		}
		/*}}}  */

		length_comm = newactionnode (S_INPUT, locn, chan, addtofront (input_var, NULL));
		SetActionType (length_comm, newleafnode (length_type, locn));

		if (is_extended) {
			array_comm = create_slice_io (S_X_INPUT, locn, chan, array, slice_var);
			SetActionDuring (array_comm, ActionDuringOf (comm));
			SetActionDuring (comm, NULL);
			SetActionAfter (array_comm, ActionAfterOf (comm));
			SetActionAfter (comm, NULL);
			SetTag (comm, S_INPUT);
			transexp (RHSAddr (array_comm), FALSE);
			trans (ActionDuringAddr (array_comm));
			trans (ActionAfterAddr (array_comm));
		} else {
			array_comm = create_slice_io (S_INPUT, locn, chan, array, slice_var);
		}

		guard = newdopnode (S_NE, locn, slice_var, newconstant (0), S_INT);
		p = addtofront (newcnode (S_IF, locn,
					  addtofront (newcondnode (S_CHOICE, locn, guard, array_comm),
						      addtofront (newcondnode (S_CHOICE, locn, newconstant (1) /*newleafnode(S_TRUE, locn) */ ,
									       newleafnode (S_SKIP, locn)), NULL))), NULL);

		if (noninteger) {
			p = addtofront (newactionnode (S_ASS, locn, slice_var, newmopnode (S_EXACT, locn, input_var, S_INT)), p);
		}
		if (nonsimple) {
			p = addtofront (newactionnode (S_ASS, locn, length_exp, input_var), p);
		}
		p = addtofront (length_comm, p);
		/*}}}  */
	} else {
		/*{{{  counted output */
		const BOOL constlen = isconst (length_exp);
		treenode *output_var;
		treenode *slice_var;
		treenode *length_comm;

		/*{{{  length temps */
		if (nonsimple && !constlen) {
			output_var = create_declaration (S_DECL, N_DECL, newleafnode (length_type, locn), NULL);
		} else {
			output_var = length_exp;
		}

		if (noninteger && !constlen) {
			slice_var = create_declaration (S_DECL, N_DECL, newleafnode (S_INT, locn), NULL);
		} else {
			slice_var = output_var;
		}

		if (RANGECHECKING && !isconst (array) && !issimple (array)) {
			/* bug TS/1172 27/04/92 */
			array = create_abbreviation (array, TRUE);
		}
		/*}}}  */

		length_comm = newactionnode (S_OUTPUT, locn, chan, addtofront (output_var, NULL));
		SetActionType (length_comm, newleafnode (length_type, locn));

		if (constlen && TagOf (length_exp) == S_CONSTEXP && LoValOf (length_exp) == 0 && HiValOf (length_exp) == 0) {
			p = NULL;
		} else {
			/*{{{  send a slice of the array */
			treenode *array_comm;
			if (constlen) {
				slice_var = newmopnode (S_EXACT, locn, copytree (length_exp, trans_lexlevel), S_INT);
				slice_var = foldexp (slice_var);
			}

			array_comm = create_slice_io (S_OUTPUT, locn, chan, array, slice_var);

			if (constlen) {
				p = addtofront (array_comm, NULL);
			} else {
				/*{{{  put in a test for zero length */
				treenode *const guard = newdopnode (S_NE, locn, slice_var, newconstant (0), S_INT);
				p = addtofront (newcnode (S_IF, locn,
							  addtofront (newcondnode (S_CHOICE, locn, guard, array_comm),
								      addtofront (newcondnode
										  (S_CHOICE, locn, newconstant (1) /*newleafnode(S_TRUE, locn) */ ,
										   newleafnode (S_SKIP, locn)), NULL))), NULL);

				if (noninteger) {
					p = addtofront (newactionnode (S_ASS, locn, slice_var, newmopnode (S_EXACT, locn, output_var, S_INT)), p);
				}
				/*}}}  */
			}
			/*}}}  */
		}
		p = addtofront (length_comm, p);
		if (nonsimple && !constlen) {
			p = addtofront (newactionnode (S_ASS, locn, output_var, length_exp), p);
		}
		/*}}}  */
	}

	while (isspecification (*tptr)) {
		tptr = DBodyAddr (*tptr);	/* skip over the new declaration */
	}
	*tptr = newcnode (S_SEQ, locn, p);

	freenode (ThisItemAddr (RHSOf (comm)));
	freenode (RHSAddr (comm));
	freetree (ActionTypeAddr (comm));
	freenode (&comm);

#if 0
fprintf (stderr, "trans_process_counted_io (out): *tptr =");
printtreenl (stderr, 4, *tptr);
#endif
}

/*}}}  */
/*{{{  PRIVATE void      swap_names */
/*{{{  swap_names_t declaration */
typedef struct {
	treenode *old_name;	/* name to be replaced */
	treenode *new_name;	/* name to replace it with */
} swap_names_t;
/*}}}  */
/*{{{  PRIVATE int do_swap_names */
PRIVATE int do_swap_names (treenode ** tptr, void *const voidptr)
{
	const swap_names_t *const pair = voidptr;
	const int tag = TagOf (*tptr);

	if (*tptr == pair->old_name) {	/* ie must be the old namenode */
		*tptr = pair->new_name;
#ifdef MOBILES
	} else if ((tag == S_PROCDEF) || (tag == S_LFUNCDEF) || (tag == S_SFUNCDEF) || (tag == S_MPROCDECL)) {
#else	/* !MOBILES */
	} else if ((tag == S_PROCDEF) || (tag == S_LFUNCDEF) || (tag == S_SFUNCDEF)) {
#endif	/* !MOBILES */
		/* bug TS/1780 24/07/92 */
		walk_free_vars (DNameOf (*tptr), do_swap_names, voidptr);
	}
	return CONTINUE_WALK;
}

/*}}}  */
/*{{{  PRIVATE void swap_names */
PRIVATE void swap_names (treenode ** tptr, treenode * const old, treenode * const new)
{
	swap_names_t pair;
	pair.old_name = old;
	pair.new_name = new;
	modprewalktree (tptr, do_swap_names, &pair);
}

/*}}}  */
/*}}}  */
/*{{{  PRIVATE void      trans_process_replpar */
PRIVATE void trans_process_replpar (treenode ** const tptr)
/*
   we transform

   PAR i = start FOR len
     ...  body (uses 'i')

into:

  PROC $anonrepl(VAL INT i)
    ...  body (uses new 'i')
  :
  PAR i = start FOR len
    $anonrepl(i)

Note that because lexlevels are incremented inside repl pars, we don't
need to adjust lex levels when doing this modification.
*/
{
	treenode *repl = *tptr;
	const SOURCEPOSN locn = LocnOf (repl);
	treenode *nptr;
	treenode *proc;
	treenode *formal;
	treenode *instance;

	/*{{{  create a proc */
	{
		treenode *const body = ReplCBodyOf (repl);
		char name_str[20];
		static int repl_anon_count = 0;	/* yes - static to create unique names */

		sprintf (name_str, "_anonrepl%d", repl_anon_count++);

		nptr = newnamenode (N_PROCDEF, locn, lookupword (name_str, strlen (name_str)),
				    NULL, NULL, trans_lexlevel, ANON_VAR_SCOPE, NM_DEFAULT);
		proc = newdeclnode (S_PROCDEF, locn, nptr, body, NULL);
		SetNDecl (nptr, proc);
		formal = newnamenode (N_VALPARAM, locn, NNameOf (ReplCNameOf (repl)),
				      newleafnode (S_INT, locn), proc, trans_lexlevel + 1, ANON_VAR_SCOPE, NM_WORKSPACE);
		SetNType (nptr, newlistnode (S_LIST, locn, formal, NULL));
	}
	/*}}}  */
	/*{{{  modify the body to use the new formal */
	{
		swap_names (&proc, ReplCNameOf (repl), formal);

#if 0
		printf ("trans_process_replpar: tree is");
		printtree (stdout, 0, proc);
		printf ("\n");
#endif

		mark_free_vars (proc, FALSE);
	}
	/*}}}  */
	/*{{{  call trans on the result */
	trans (&proc);
	/*}}}  */
	/*{{{  create an instance */
	instance = newinstancenode (S_PINSTANCE, locn, nptr, newlistnode (S_LIST, locn, ReplCNameOf (repl), NULL));
	SetNUsed (nptr, TRUE);
	trans_lexlevel++;
	trans (&instance);
	trans_lexlevel--;
	/*}}}  */
	/*{{{  bolt everything together */
	SetReplCBody (repl, instance);
	SetDBody (proc, repl);
	*tptr = proc;
	/*}}}  */
}

/*}}}  */
/*{{{  PRIVATE void trans_process_repl_for_one (treenode **tptr)*/
PRIVATE void trans_process_repl_for_one (treenode **tptr)
{
	treenode *repl = *tptr;
	const SOURCEPOSN locn = LocnOf (repl);
	treenode *const repl_start = ReplCStartExpOf (repl);
	treenode *const repl_nptr = ReplCNameOf (repl);
	treenode *repl_new;

	*tptr = ReplCBodyOf (repl);	/* skip past the replicator */

	/*printf("trans_process_repl_for_one: #%8X (%d)\n", locn, locn); */
	if (isconst (repl_start)) {
		repl_new = create_abbreviation (repl_start, TRUE);
	} else {
		treenode *ass;
		repl_new = create_declaration (S_DECL, N_DECL, newleafnode (S_INT, locn), NULL);
		ass = newactionnode (S_ASS, locn, repl_new, repl_start);
		SetDVal (NDeclOf (repl_new), ass);	/* initialised variable */
	}
	tptr = DBodyAddr (*tptr);	/* skip over the new declaration */

	swap_names (tptr, repl_nptr, repl_new);

	if (isconst (repl_start)) {
		foldtree (*tptr);	/* fold the constants */
	}

	freetree (ReplCLengthExpAddr (repl));
	freenode (&repl);

#if 0
fprintf (stderr, "tran1: trans_process_repl_for_one(): removed and folded replicator, repl_new =");
printtreenl (stderr, 4, repl_new);
fprintf (stderr, "tran1: trans_process_repl_for_one(): removed and folded replicator, DValOf (NDeclOf (repl_new)) =");
printtreenl (stderr, 4, DValOf (NDeclOf (repl_new)));
#endif
	return;
}

/*}}}  */
/*{{{  PUBLIC void transsubscripts (const trans_params_t *const trans_params_in, treenode **tptr, const BOOL create_temps)*/
/*****************************************************************************
 *
 *  transsubscripts performs trans on segments and subscripts.
 *                  For segments, temporaries are inserted where needed on
 *                  the start and length expressions.
 *                  No tree transformation is done, this is performed by
 *                  trans on the top-level subscript only.
 *
 * if create_temps is TRUE, it will insert temporaries for the start and
 * length if required.
 *
 *****************************************************************************/
PUBLIC void transsubscripts (const trans_params_t *const trans_params_in, treenode **tptr, const BOOL create_temps)
{
	const trans_params_t *const saved_params = trans_params;
	BOOL going = TRUE;
	trans_params = trans_params_in;

#if 0
fprintf (stderr, "transsubscripts in (tran1): *tptr =");
printtreenl (stderr, 4, *tptr);
#endif
	while (going) {
		treenode *const t = *tptr;
		switch (nodetypeoftag (TagOf (t))) {
			/*{{{  SEGMENT SEGMENTITEM */
		case SEGMENTNODE:
			{
				BOOL saved_optimise_valof;
				/*DEBUG_MSG(("transsubscripts: SEGMENTNODE. Now calling trans on SStartExp\n")); */
				sub_transexp (SStartExpAddr (t), TRUE);
				/*DEBUG_MSG(("transsubscripts: SEGMENTNODE. Now calling trans on SLengthExp\n")); */
				saved_optimise_valof = optimise_valof;
				optimise_valof = FALSE;	/* Fix for bug 634 */
#ifdef OCCAM2_5
				if (SLengthExpOf (t) == NULL) {
					/*{{{  insert the 'FOR' expression */
					/*if (!issimplelocal(SNameOf(t)))
					   SetSName(t, create_abbreviation(SNameOf(t), TRUE));
					 */
					if (!issimplelocal (SStartExpOf (t), trans_lexlevel) && !isconst (SStartExpOf (t))) {
						SetSStartExp (t, create_abbreviation (SStartExpOf (t), TRUE));
					}
					SetSLengthExp (t, newdopnode (S_SUBTRACT, LocnOf (t),
								newmopnode (S_ELSIZE, LocnOf (t), SNameOf (t), S_INT), SStartExpOf (t), S_INT));
					/*}}}  */
				}
#endif
				sub_transexp (SLengthExpAddr (t), TRUE);
				optimise_valof = saved_optimise_valof;
				if (TagOf (t) == S_SEGMENTITEM) {	/* Added by CO'N 6/4/90 */
					/*DEBUG_MSG(("transsubscripts: SEGMENTITEM. Now calling trans on SSubscriptExp and SCheckExp\n")); */
					sub_transexp (SSubscriptExpAddr (t), TRUE);
					sub_transexp (SCheckExpAddr (t), TRUE);
				}
				/*DEBUG_MSG(("transsubscripts: SEGMENTNODE. Recursing on SNameOf\n")); */
				transsubscripts (trans_params_in, SNameAddr (t), create_temps);
				if (create_temps && !no_anonymous_vars) {	/* bug TS/1455 5/11/91 */
					/*{{{  put temporaries on start and length expressions if necessary */
					treenode *lengthexp = SLengthExpOf (t);
					treenode *startexp = SStartExpOf (t);
					treenode *s = skipevals (startexp);
					if (!isconst (s) && !issimplelocal (s, trans_lexlevel))
						/* we would evaluate it twice - once in the range check and once in the
						   base, otherwise */
					{
						/*DEBUG_MSG(("transsubscripts: creating temp for startexp\n")); */
						SetSStartExp (t, create_abbreviation (startexp, TRUE));	/* bug TS/1324 24/10/91 */
					}
					/* we nearly always convert to a temporary here, incase the
					   value is required as a range check later */
					if (!isconst (lengthexp) && !issimplelocal (lengthexp, trans_lexlevel)) {
						/*DEBUG_MSG(("transsubscripts: creating temp for lengthexp\n")); */
						SetSLengthExp (t, create_abbreviation (lengthexp, TRUE));	/* bug TS/1324 24/10/91 */
					}
					/*}}}  */
				}
			}
			going = FALSE;
			break;
			/*}}}  */
			/*{{{  ARRAYSUB */
		case ARRAYSUBNODE:
			/*DEBUG_MSG(("transsubscripts: ARRAYSUBNODE. recursing on ASExpOf\n")); */
			sub_transexp (ASExpAddr (t), TRUE);
			/*DEBUG_MSG(("transsubscripts: ARRAYSUBNODE. recursing on ASIndexOf\n")); */
			sub_transexp (ASIndexAddr (t), TRUE);
			/*DEBUG_MSG(("transsubscripts: ARRAYSUBNODE. looping down ASBaseOf\n")); */
			tptr = ASBaseAddr (t);
			break;
			/*}}}  */
			/*{{{  CONSTRUCTOR */
		case LITNODE:
			if (TagOf (t) == S_CONSTRUCTOR) {
				sub_transexp (tptr, TRUE);
			}
			going = FALSE;
			break;
			/*}}}  */
			/*{{{  FINSTANCE */
#ifdef OCCAM2_5
		case INSTANCENODE:
			DEBUG_MSG (("transsubscripts: INSTANCENODE. calling sub_transexp on fn\n"));
			sub_transexp (tptr, TRUE);
			DEBUG_MSG (("transsubscripts: INSTANCENODE. creating abbr\n"));
			*tptr = create_abbreviation (*tptr, TRUE);
			going = FALSE;
			break;
#endif
			/*}}}  */
		default:
			going = FALSE;
			break;
		}
	}
	trans_params = saved_params;
#if 0
fprintf (stderr, "transsubscripts out (tran1): *tptr =");
printtreenl (stderr, 4, *tptr);
#endif
}

/*}}}  */
/*{{{  PRIVATE void trans_resultlist (treenode ** const valof)*/
PRIVATE void trans_resultlist (treenode ** const valof)
{
	treenode *list;
	const trans_data_t saved_skip_data = trans_data;
	const BOOL skip_valof = TagOf (VLBodyOf (*valof)) == S_SKIP;

	if (skip_valof) {
		/*{{{  arrange for insertions infront of the valof */
		trans_data.insertpoint = valof;
		trans_data.insertvalof = FALSE;
		/*DEBUG_MSG(("trans_resultlist: setting trans_data at %lX\n", trans_locn)); */
		/*}}}  */
	}

	for (list = VLResultListOf (*valof); !EndOfList (list); list = NextItem (list)) {
		const trans_data_t saved_data = trans_data;

		if (!skip_valof) {
			/*{{{  arrange for insertions inside the result list */
			trans_data.insertpoint = ThisItemAddr (list);
			trans_data.insertvalof = TRUE;	/* new valof will have same type as this one,
							   so we're not inserting array valofs unless
							   they're already permitted.
							   CON 4/11/91.
							 */
			/*DEBUG_MSG(("trans_resultlist: setting trans_data at %lX\n", trans_locn)); */
			/*}}}  */
		}

		sub_transexp (ThisItemAddr (list), TRUE);

		if (!skip_valof) {
			/*{{{  reset insertions */
			trans_data = saved_data;
			/*DEBUG_MSG(("trans_resultlist: resetting trans_data\n")); */
			/*}}}  */
		}
	}

	if (skip_valof) {
		/*{{{  reset insertions */
		trans_data = saved_skip_data;
		/*DEBUG_MSG(("trans_resultlist: resetting trans_data\n")); */
		/*}}}  */
	}
}

/*}}}  */
/*{{{  PRIVATE treenode **transspecs (treenode ** tptr)*/
PRIVATE treenode **transspecs (treenode ** tptr)
{
	const trans_data_t saved_data = trans_data;
	trans_data.insertvalof = FALSE;
	while (isspecification (*tptr)) {
		treenode *const t = *tptr;
		trans_locn = LocnOf (t);
		trans_data.insertpoint = tptr;
		/*DEBUG_MSG(("transspecs: setting trans_data at %lX on %s\n", trans_locn, itagstring(TagOf(t)))); */

		switch (TagOf (t)) {
			/*{{{  default - ignore */
		default:
#if 0
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_WSPLACE:
		case S_VSPLACE:
		case S_LABELDEF:
		case S_PRAGMA:	/* bug 829 19/9/91 */
		case S_TYPEDECL:
#ifdef MOBILES
		case S_FORWDECL:
		case S_PROCTYPEDECL:
#endif
#endif
			break;
			/*}}}  */
			/*{{{  PROCTYPEDECL*/
		case S_PROCTYPEDECL:
			{
				treenode *n = DNameOf (t);

				trans_lexlevel++;
				augmentproctype (n, trans_lexlevel);
				trans_lexlevel--;
			}
			break;
			/*}}}*/
			/*{{{  routine specification */
		case S_PROCDEF:
#ifdef MOBILES
		case S_MPROCDECL:
#endif
		case S_SFUNCDEF:
		case S_LFUNCDEF:
#if 0
fprintf (stderr, "transspecs: S_PROCDEF/SFUNCDEF/LFUNCDEF: (in) NTypeOf (DNameOf (t)) =");
printtreenl (stderr, 4, NTypeOf (DNameOf (t)));
#endif
			if (!isinline (DNameOf (t))) {	/* Added 29/5/90 by CO'N for bug 315 */
				treenode *n = DNameOf (t);
				treenode *saved_inside_suspends = inside_suspends;

				trans_lexlevel++;	/* Formal parameters are at a higher lexical level */
				augmentformals (n, trans_lexlevel);
#if 0
fprintf (stderr, "transspecs: S_PROCDEF/SFUNCDEF/LFUNCDEF: (after augmentformals) NTypeOf (n) =");
printtreenl (stderr, 4, NTypeOf (n));
#endif
				if (!separatelycompiled (n)) {
					if (NPSuspendsOf (n)) {
						inside_suspends = n;
					} else {
						inside_suspends = NULL;
					}
					trans (DValAddr (t));
				}
				trans_lexlevel--;

				inside_suspends = saved_inside_suspends;
			}
#if 0
fprintf (stderr, "transspecs: S_PROCDEF/SFUNCDEF/LFUNCDEF: (out) NTypeOf (DNameOf (t)) =");
printtreenl (stderr, 4, NTypeOf (DNameOf (t)));
#endif
			break;

			/*}}}  */
			/*{{{  DECL */
		case S_DECL:	/* we have to set the access mode */
			{
				/* moved from the mapper 15/2/91 for bug 1156 */
				treenode *nptr = DNameOf (t);
#ifdef MOBILES
				/*{{{  if declaration of an anonymous chan-type, trash it, update tptr and CONTINUE round the while() .*/
#if 0
				if ((TagOf (DNameOf (t)) == S_LIST) && (TagOf (NTypeOf (ThisItem (DNameOf (t)))) == S_ANONCHANTYPE)) {
					*tptr = DBodyOf (t);	/* link in body */
					continue;		/* to while() */
				} else if (TagOf (NTypeOf (DNameOf (t))) == S_ANONCHANTYPE) {
					*tptr = DBodyOf (t);
					continue;
				}
#endif

				/*}}}*/
#endif
				if (TagOf (nptr) == S_LIST) {
					for (; !EndOfList (nptr); nptr = NextItem (nptr)) {
#ifdef MOBILES
#if 0
printf ("transspecs: nptr = ");
printtree (stdout, 2, nptr);
printf ("\n");
#endif
						isinmobilespace (ThisItem (nptr));
#endif
						isinvectorspace (ThisItem (nptr));	/* does SetNMode */
						if (TagOf (NTypeOf (ThisItem (nptr))) == S_CHAN) {
							SetNChanMark (ThisItem (nptr), FALSE);
						}
					}
				} else {
#ifdef MOBILES
#if 0
printf ("transspecs: nptr = ");
printtree (stdout, 2, nptr);
printf ("\n");
#endif
					isinmobilespace (nptr);
#endif
					isinvectorspace (nptr);	/* does SetNMode */
					if (TagOf (NTypeOf (nptr)) == S_CHAN) {
						SetNChanMark (nptr, FALSE);
					}
				}
				trans (DValAddr (t));
			}
			break;

			/*}}}  */
			/*{{{  Abbreviations / RETYPEs */
		case S_VALABBR:
		case S_VALRETYPE:
		case S_ABBR:
		case S_RETYPE:
			{
				treenode **rhsaddr = DValAddr (t);
				treenode *const nptr = DNameOf (t);
				const int tag = TagOf (nptr);
				const BOOL retype = (tag == N_VALRETYPE) || (tag == N_RETYPE);
				SOURCEPOSN thislocn = trans_locn;
				const BOOL wasconstant = isconst (*rhsaddr);	/* bug 1040 13/11/90 */
				const BOOL saved_flag_scalar_chans = trans_flag_scalar_chans;
				trans_flag_scalar_chans = retype;
				if (TagOf (NTypeOf (nptr)) == S_CHAN)
					SetNChanMark (nptr, FALSE);

				if (!retype && (TagOf (*rhsaddr) == S_CONSTRUCTOR) && trans_params->abbr_constructors) {
					create_initialised_constructor (nptr);	/* bug TS/1263 11/05/92 */
#ifdef MOBILES
					(void) isinmobilespace (nptr);
#endif
					(void) isinvectorspace (nptr);
					trans (DValAddr (NDeclOf (nptr)));
					break;	/* break out of master switch statement */
				}

				/* if we're retyping, we consider the rhs to be a 'sub expression'
				   so that constructors, stc, get separated out.
				   bug TS/1263 11/05/92
				 */
				overlap_checks = 0;
				transexp (rhsaddr, retype);
				overlapwarnings (thislocn);

#ifdef MOBILES
				(void) isinmobilespace (nptr);
#endif
				(void) isinvectorspace (nptr);
				set_abbreviation_mode (t);

				/*{{{  fill in the unknown array dimensions */
				{
					treenode *type = NTypeOf (nptr);
					if (!retype) {
						/*{{{  copy dimensions from the type of the right-hand side */
						int dimension = 0;
						for (; TagOf (type) == S_ARRAY; dimension++, type = ARTypeOf (type))
							if (ARDimOf (type) == (-1))
								/* bug TS/1404 & TS/0149 30/09/91 28/10/91 */
							{
								treenode *newdim = dimexpof (*rhsaddr, dimension);
								if (!isconst (newdim) && !issimplelocal (newdim, trans_lexlevel)) {
									DEBUG_MSG (
										   ("transspecs: abbrev dim of '%s' is complicated\n",
										    WNameOf (NNameOf (nptr))));
									/*newdim = newtempnode(T_TEMP, newdim, NM_WORKSPACE); */
									newdim = create_abbreviation (newdim, TRUE);	/* bug TS/1404 28/10/91 */
								}
								SetARDimLength (type, newdim);
								if (isconst (newdim))
									SetARDim (type, LoValOf (newdim));
							}
						/*}}}  */
					} else {
						/*{{{  there is at most one hidden dimension in a retype */
#if 0				/* bug 1505 28/11/91 now superseeded */
						/*{{{  insert a temp for the hidden dimension */
						for (; TagOf (type) == S_ARRAY; type = ARTypeOf (type))
							if (ARDimOf (type) == (-1))
								/* Make up a temporary for the open dimension */
							{
								treenode *temp = newtempnode (T_TEMP /*T_PREEVALTEMP */ , dummyexp_p, NM_WORKSPACE);	/* Bug TS/1505 28/11/91 */
								DEBUG_MSG (
									   ("transspecs: creating temp dimension at trans_lexlevel %d\n",
									    trans_lexlevel));
								SetARDimLength (type, temp);
							}
						/*}}}  */
#else
						/* insert anonymous variables for hidden dimension */
						const INT32 scalarsize = bytesin (basetype_tree (type));
						INT32 lsize = scalarsize;
						treenode **opendimension = NULL;
#ifdef OCCAM2_5
						int vardimensions = 0;
#endif
						treenode *const rhstype = gettype (*rhsaddr);
						const INT32 rsize = known_bytesin (rhstype);
						const int old = switch_to_temp_workspace ();
						treenode *dim_exp = unknowndimsof (rhstype);
						switch_to_prev_workspace (old);

						/*{{{  look for an open dimension, calculate product of known dimensions */
						{
							treenode *tt;
							for (tt = type; TagOf (tt) == S_ARRAY; tt = ARTypeOf (tt))
								if (ARDimLengthOf (tt) == NULL)
									opendimension = ARDimLengthAddr (tt);
#ifdef OCCAM2_5
								else if (ARDimOf (tt) == (-1))
									vardimensions++;
#endif
								else
									lsize *= ARDimOf (tt);
						}
						/*}}}  */

#ifdef OCCAM2_5
						if (NVReshapesOf (nptr) && (vardimensions != 0))
							/*{{{  process 'RESHAPES' with var dims */
						{
							/*{{{  COMMENT */
							/* We have a RESHAPES such as:
							   [i+j][k+l]INT RESHAPES rhs :
							   or
							   [][k+l]INT RESHAPES rhs :

							   * we range check variable dimensions, to ensure non-negative
							   * we abbreviate all `complicated' variable dimensions,
							   to turn them all into local variables.
							   * we calculate known size of lhs type, in units of the type of the array
							   * we calculate known size of rhs type, in units of the type of the array
							   * if necessary, we calculate the open dimension by division.
							   and adjust the lhs to account for this
							   * we create an abbreviation to check that the two sizes
							   are equal.

							   IMPORTANT NOTE - I have not attempted to tweak all sorts
							   of optimisations out of this. I consider correctness
							   to be far more important. At least, when the algorithm is
							   this simple, there is a good chance that the result
							   is correct!

							   CON - 10/1/94.
							 */

							/*}}}  */
							/*{{{  variables */
							const INT32 known_lhsize = known_bytesin (type) / bytesin (basetype_tree (type));
							const INT32 known_rhsize = known_bytesin (rhstype) / bytesin (basetype_tree (rhstype));
							treenode *unknown_lhsize = NULL;
							treenode *lhsize_exp;
							treenode *rhsize_exp;
							treenode *a;

							/*}}}  */
							/*{{{  abbreviate any unknown dims if necessary */
							{
								treenode *tt;
								for (tt = type; TagOf (tt) == S_ARRAY; tt = ARTypeOf (tt)) {
									if ((ARDimLengthOf (tt) != NULL) && (ARDimOf (tt) == (-1)))
									{
										/*{{{  variable dimension */
										transexp (ARDimLengthAddr (tt), retype);
										a = ARDimLengthOf (tt);
										if (RANGECHECKING) {
											a = newdopnode (S_CSUB0, trans_locn, a,
													newconstant ((bytesperword == 2) ?
														     MOSTNEG_INT16 : MOSTNEG_INT32),
													S_INT);
										}
										if (!issimplelocal (a, trans_lexlevel)) {
											a = create_abbreviation (a, TRUE);
										}
										SetARDimLength (tt, a);
										unknown_lhsize = add_to (unknown_lhsize, S_TIMES, a);
										/*}}}  */
									}
								}
							}
							/*}}}  */
							/*{{{  calculate lhsize_exp */
							if (known_lhsize == 1) {
								lhsize_exp = unknown_lhsize;
							} else {
								lhsize_exp = add_to (unknown_lhsize, S_TIMES, newconstant (known_lhsize));
							}

							if ((opendimension != NULL) && (RANGECHECKING) &&
							    !isconst (lhsize_exp) && !issimplelocal (lhsize_exp, trans_lexlevel)) {
								lhsize_exp = create_abbreviation (lhsize_exp, TRUE);
							}
							/*}}}  */
							/*{{{  calculate rhsize_exp */
							dim_exp = copytree (dim_exp, trans_lexlevel);
							if (known_rhsize == 1) {
								rhsize_exp = dim_exp;
							} else {
								rhsize_exp = add_to (dim_exp, S_TIMES, newconstant (known_rhsize));
							}

							if ((opendimension != NULL) && (RANGECHECKING) &&
							    !isconst (rhsize_exp) && !issimplelocal (rhsize_exp, trans_lexlevel)) {
								rhsize_exp = create_abbreviation (rhsize_exp, TRUE);
							}
							/*}}}  */
							/*{{{  calculate open dimension */
							if (opendimension != NULL) {
								a = newdopnode (S_DIV, trans_locn,
										copytree (rhsize_exp, trans_lexlevel),
										copytree (lhsize_exp, trans_lexlevel), S_INT);
								*opendimension = create_abbreviation (a, TRUE);
								lhsize_exp = newdopnode (S_TIMES, trans_locn, *opendimension, lhsize_exp, S_INT);
							}
							/*}}}  */
							/*{{{  check total size */
							if (RANGECHECKING) {
								a = newdopnode (S_EQ, trans_locn, lhsize_exp, rhsize_exp, S_INT);
								a = newdopnode (S_CCNT1, trans_locn, a, newconstant (1), S_INT);
								(void) create_abbreviation (a, TRUE);
							}
							/*}}}  */
						}
						/*}}}  */
						else
#endif
						if ((opendimension != NULL) && (dim_exp == NULL))
							/*{{{  expansion of INLINE routine, dimensions are now known */
						{
							/* bug INSdi01929 */
							/* This is an expansion similar to:
							   [100]BYTE b :
							   []INT16 x16 RETYPES b:
							   ...
							   This has probably been created by expanding out an INLINE routine,
							   otherwise the front-end would have inserted the unknown type.
							 */

							if (rsize % lsize == 0) {
								const INT32 new_dim = rsize / lsize;
								treenode *tt;
								for (tt = type; TagOf (tt) == S_ARRAY; tt = ARTypeOf (tt))
									if (ARDimOf (tt) == (-1)) {
										SetARDimLength (tt, newconstant (new_dim));
										SetARDim (tt, new_dim);
									}
							} else {
								msg_out_s (SEV_FATAL, TRANS, TRANS_RETYPE_TYPE_MISMATCH, thislocn,
									   WNameOf (NNameOf (nptr)));
							}
						}
						/*}}}  */
						else if (opendimension != NULL)
						{
							/*{{{  handle the open dimension */
							dim_exp = copytree (dim_exp, trans_lexlevel);	/* copy into 'real' space */
							if (rsize % lsize == 0) {
								/*{{{  multiply dim_exp by rsize / lsize */
								if (rsize != lsize) {
									dim_exp = newdopnode (S_TIMES, thislocn, dim_exp, newconstant (rsize / lsize), S_INT);
								}
								/*}}}  */
							} else {
								/*{{{  check and calculate open dimension */
								/* lsize != 1, or we wouldn't have come into this branch
								   of the if */

								/* if lsize is a power of 2, this sets lsize_bit to the bit number */
								const int lsize_bit = whichpowerof2 (lsize);

								if (rsize != 1) {
									dim_exp = newdopnode (S_TIMES, thislocn, dim_exp, newconstant (rsize), S_INT);
								}
								if (RANGECHECKING) {
									/*{{{  check rhs size and lhs size match */
									BOOL copy = FALSE;
									treenode *temp1;
									treenode *temp2;
									if (TagOf (dim_exp) == S_ELSIZE) {
										const int this_old = switch_to_temp_workspace ();
										treenode *const exp = dimexpof (OpOf (dim_exp), 0);
										switch_to_prev_workspace (this_old);
										copy = issimplelocal (exp, trans_lexlevel);
									}

									if (issimplelocal (dim_exp, trans_lexlevel) || copy) {
										temp1 = dim_exp;
									} else {
										temp1 = create_abbreviation (dim_exp, TRUE);
									}

									temp2 = copy ? copytree (temp1, trans_lexlevel) : temp1;

									/* bug TS/1882 16/10/92 */
									/* This is definitely safe, because the expression of which we want the
									   remainder (temp1) is a size of an array, and therefore will always be
									   positive.
									 */
									if (lsize_bit >= 0) {
										dim_exp = newdopnode (S_BITAND, thislocn, temp1, newconstant (lsize - 1), S_INT);
									} else {
										dim_exp = newdopnode (S_REM, thislocn, temp1, newconstant (lsize), S_INT);
									}
#if 0
fprintf (stderr, "transspecs(): ABBR/RETYPE: dim_exp = ");
printtreenl (stderr, 4, dim_exp);
#endif

									dim_exp = newdopnode (S_CSUB0, thislocn, dim_exp, newconstant (1), S_INT);
									dim_exp = newdopnode (S_EVAL, thislocn, dim_exp, temp2, S_INT);
									/*}}}  */
								}

								/* bug TS/1882 16/10/92 */
								/* This is definitely safe, because the expression of which we want the
								   remainder (dim_exp) is either the size of an array,
								   or an eval node of temp2 which is a copy of temp1 which is a copy
								   of dim_exp, and therefore will always be positive.
								 */
								if (lsize_bit >= 0) {
									dim_exp = newdopnode (S_RSHIFT, thislocn, dim_exp, newconstant (lsize_bit), S_INT);
								} else {
									dim_exp = newdopnode (S_DIV, thislocn, dim_exp, newconstant (lsize), S_INT);
								}
								/*}}}  */
							}

							/*{{{  check if the open dimension could overflow */
							/*{{{  Comment about this */
							/*
							   Comment about the rationale behind the check.

							   Note that, according to bug TS/1359, we have arranged that:
							   1) On a 32-bit processor, no array may be larger than 1/2 memory size.
							   Thus the size of any array can be expressed as a positive number of
							   bytes, with no need to consider unsignedness.
							   2) on a 16-bit processor, we can have an array whose total size is
							   greater than half the memory size. Thus the total size in bytes
							   must be considered as an unsigned number, but in any case
							   will be less than 2^16.
							   3) Each dimension in a multi dimensioned array must have a positive size.
							   4) Therefore the only occasion where we need to consider inserting a
							   check that the calculated dimension is too large is:
							   a) on a 16-bit processor
							   b) where we're RETYPEing into a byte/bool array

							   CON, 19/10/92.
							 */
							/*}}}  */

							/* bug TS/1359 19/10/92 */
							if (RANGECHECKING && (bytesperword == 2) && (scalarsize * lsize == 1)) {
								dim_exp = newdopnode (S_CSUB0, thislocn, dim_exp, newconstant (0xFFFF8000), S_INT);
							}
							/*}}}  */

							/*{{{  create abbreviation if necessary */
							if (issimplelocal (dim_exp, trans_lexlevel)) {
								/* can happen eg:
								   PROC p ([]INT32 i32)
								   []INT retype RETYPES i32 :
								   etc.
								 */
								*opendimension = dim_exp;
							} else {
								*opendimension = create_abbreviation (dim_exp, TRUE);
							}
							/*}}}  */
							/*}}}  */
						} else if (dim_exp != NULL && (RANGECHECKING)) {
							/*{{{  check that the sizes match */
							/* bug TS/1980 21/01/93 - we must check that the size of the rhs
							   matches that specified in the lhs.
							 */
							/* we have an example such as:

							   PROC p ([][]INT array)
							   [100][2]BYTE b RETYPES array :

							   therefore we want to check that
							   lsize == rsize * (dim_exp)
							   or, more to the point, dim_exp == (lsize / rsize)
							   We create an anonymous variable with this expression
							   which ensures that the check is generated.
							 */
							treenode *check_exp;
							dim_exp = copytree (dim_exp, trans_lexlevel);	/* copy into 'real' space */
							if (lsize == rsize) {
								check_exp = dim_exp;
							} else {
								check_exp = newdopnode (S_EQ, thislocn, dim_exp, newconstant (lsize / rsize), S_INT);
							}
							check_exp = newdopnode (S_CCNT1, thislocn, check_exp, newconstant (1), S_INT);
							(void) create_abbreviation (check_exp, TRUE);
							/*}}}  */
						}
#endif
						/*}}}  */
					}
				}
				/*}}}  */

				if (!wasconstant && isconst (*rhsaddr)) {	/* bug 1040 13/11/90 */
					/* We keep on getting bugs because of non-constants in the
					   front-end being turned into constants in the back end
					   by INLINING functions.
					   This makes sure that we don't - bug 1040 13/11/90 */
					*rhsaddr = newvalofnode (S_VALOF, thislocn,
							newleafnode (S_SKIP, thislocn), newlistnode (S_LIST, thislocn, *rhsaddr, NULL));
				}

				trans_flag_scalar_chans = saved_flag_scalar_chans;
			}
			break;
			/*}}}  */
			/*{{{  PLACE */
		case S_PLACE:
#if 0
fprintf (stderr, "transspecs: S_PLACE, about to transexp (DValAddr), t =");
printtreenl (stderr, 4, t);
#endif
			transexp (DValAddr (t), FALSE);
			break;
			/*}}}  */
		}
		tptr = DBodyAddr (t);
	}
	trans_data = saved_data;
	return tptr;
}

/*}}}  */
#if defined(MOBILES)
/*{{{  PRIVATE void transform_mobile_segment (const trans_params_t *const trans_params_in, treenode **tptr)*/
/*
 *	this turns:
 *	  [nested-array ...]
 *	into:
 *	  (type) tmp$ IS nested-array:
 *	  [tmp$ ...]
 *
 *	if "nested-array" is complex (i.e. a nested MOBILE)
 *	
 *	returns TRUE if the tree was modified
 */
PRIVATE BOOL transform_mobile_segment (const trans_params_t *const trans_params_in, treenode **tptr)
{
	treenode *t = *tptr;
	treenode *sname = SNameOf (t);
	BOOL modified = FALSE;

	trans_params = trans_params_in;

	if (isdynmobilearray (sname) && ((TagOf (sname) == S_ARRAYSUB) || (TagOf (sname) == S_ARRAYITEM))) {
		treenode *abbr = create_abbreviation (sname, FALSE);
		treenode *abbrdecl = NDeclOf (abbr);

		SetSName (t, abbr);
		modified = TRUE;
		/* crude..  transform abbreviation */
		transspecs (&abbrdecl);
	}

	return modified;
}
/*}}}*/
#endif
/*{{{  PRIVATE void sub_transexp (treenode ** tptr, BOOL is_sub_expression)*/
PRIVATE void sub_transexp (treenode ** tptr, BOOL is_sub_expression)
/* This is trans for an expression. */
/* 'is_sub_expression' is FALSE if we're just about to store the expression:
   Bug TS/1324 24/10/91 */
{
	while (*tptr != NULL) {
		treenode *t;
		treenode **original_tptr = tptr;	/* used to optimise valofs */
		tptr = transspecs (tptr);
		t = *tptr;
		if (t == NULL) {
			return;
		}
		trans_locn = LocnOf (t);
#if 0
fprintf (stderr, "sub_transexp on: *tptr = ");
printtreenl (stderr, 4, *tptr);
#endif
		switch (nodetypeoftag (TagOf (t))) {
			/*{{{  cases */
			/*{{{  default - error */
		default:
			badtag (trans_locn, TagOf (t), "sub_transexp");
			return;
			/*}}}  */
			/*{{{  TYPE */
#ifdef MOBILES
		case TYPENODE:
			switch (TagOf (t)) {
				/*{{{  new dynamic mobile array */
			case S_NEW_ARRAY:
				/* change the dimension length (costant/variable/exp/whatever) into a
				 * multiplcation node between this (dimsize) and the size of the base type */
				{
					treenode *list = ARDimLengthOf (t);
					treenode *newsize;
					treenode *stype;
					
#if 0
fprintf (stderr, "sub_transexp: t is NEW_ARRAY.  ARTypeOf (t) = ");
printtreenl (stderr, 4, ARTypeOf (t));
fprintf (stderr, "sub_transexp: t is NEW_ARRAY.  follow_user_type (ARTypeOf (t)) = ");
printtreenl (stderr, 4, follow_user_type (ARTypeOf (t)));
#endif
					stype = follow_user_type (ARTypeOf (t));
					switch (TagOf (stype)) {
					case S_MOBILE:
						{
							treenode *subtype = MTypeOf (stype);
							BOOL is_chan_type = FALSE;

							if (TagOf (subtype) == S_RECORD) {
								treenode *fields = ARTypeOf (subtype);

								if (TagOf (fields) == S_LIST) {
									if ((TagOf (ThisItem (fields)) == S_DECL) && (TagOf (NTypeOf (DNameOf (ThisItem (fields)))) == S_CHAN)) {
										is_chan_type = TRUE;
									}
								} else if ((TagOf (fields) == S_DECL) && (TagOf (NTypeOf (DNameOf (fields))) == S_CHAN)) {
									is_chan_type = TRUE;
								}
								if (is_chan_type) {
									newsize = newconstant (bytesperword);
								} else {
									newsize = newconstant (bytesin (ARTypeOf (t)));
								}
							} else if (isdynmobilearraytype (stype)) {

								/* nested dynamic MOBILE array. */
#if 0
fprintf (stderr, "sub_transexp: NEW_ARRAY of dynmobilearray type.  dynmobiledimensioncount (stype) gives %d\n", dynmobiledimensioncount (stype));
#endif
								#if 0
								newsize = newconstant (bytesperword * (1 + dynmobiledimensioncount (stype)));
								#endif
								newsize = newconstant (bytesperword);
							} else {
								newsize = newconstant (bytesin (ARTypeOf (t)));
							}
						}
						break;
					case S_CHAN:
						/* need a temporary here.. */
						{
							treenode *xtmp = create_declaration (S_DECL, N_DECL, newleafnode (S_INT, NOPOSN), NULL);
							treenode *assnode = newactionnode (S_ASS, NOPOSN, xtmp, newconstant (bytesin (ARTypeOf (t))));
							treenode *proclist = newlistnode (S_LIST, NOPOSN, assnode, newlistnode (S_LIST, NOPOSN, DBodyOf (NDeclOf (xtmp)), NULL));
							treenode *seqnode = newcnode (S_SEQ, NOPOSN, proclist);

							SetDBody (NDeclOf (xtmp), seqnode);
							newsize = xtmp;
						}
						break;
					default:
						newsize = newconstant (bytesin (ARTypeOf (t)));
						break;
					}
					/* build the tree so that we leave the base-type size at the outermost level */
					if (TagOf (list) == S_LIST) {
						treenode *building = ThisItem (list);

						list = NextItem (list);
						while (!EmptyList (list)) {
							building = newdopnode (S_TIMES, LocnOf (t), ThisItem (list), building, S_INT);
							list = NextItem (list);
						}
						newsize = newdopnode (S_TIMES, LocnOf (t), building, newsize, S_INT);
					} else {
						newsize = newdopnode (S_TIMES, LocnOf (t), list, newsize, S_INT);
					}
					if (isconst (newsize)) {
						newsize = foldexp (newsize);
					}
					SetARDimLength (t, newsize);
					tptr = ARDimLengthAddr (t);

					if (TypeAttrOf (t) & TypeAttr_aligned) {
						sub_transexp (ARAlignmentAddr (t), is_sub_expression);
					}
				}
				break;
				/*}}}  */
				/*{{{  new MOBILE process*/
			case S_ALLOC_PROC:
				return;
				/*}}}*/
				/*{{{  new MOBILE BARRIER*/
			case S_NEW_BARRIER:
				return;
				/*}}}*/
			}
			break;
#endif
			/*}}}  */
			/*{{{  LIST */
		case LISTNODE:
			sub_transexp (ThisItemAddr (t), is_sub_expression);
			tptr = NextItemAddr (t);
			break;
			/*}}}  */
			/*{{{  monadic and conversion */
		case MOPNODE:
			switch (TagOf (t)) {
				/*{{{  default */
			default:
				tptr = OpAddr (t);
				is_sub_expression = TRUE;
				break;
				/*}}}  */
				/*{{{  checked -> unchecked */
			case S_NEG:
				if (!NEED_ERRORS) {	/* INSdi02354 */
					if (isshortint (MOpTypeOf (t)))
						SetTag (t, S_UMINUS);
				}
				tptr = OpAddr (t);
				is_sub_expression = TRUE;
				break;
				/*}}}  */
#ifdef MOBILES
				/*{{{  (CLONE (CLONE x)) -> (CLONE x) simplification */
			case S_CLONE:
				if (TagOf (OpOf (t)) == S_CLONE) {
					*tptr = OpOf (t);
				} else {
					tptr = OpAddr (t);
				}
				is_sub_expression = TRUE;
				break;
				/*}}}  */
#endif
				/*{{{  ELSISE, SEGSTART */
			case S_ELSIZE:
			case S_SEGSTART:
				return;
				/*}}}  */
				/*{{{  conversions */
			case S_EXACT:
			case S_ROUND:
			case S_TRUNC:	/* Added 20/6/90 for bug 334 */
				{
					const int sourcetype = ntypeof (OpOf (t));
					const int desttype = MOpTypeOf (t);
					if (((desttype == S_INT) ? targetintsize : desttype) == ((sourcetype == S_INT) ? targetintsize : sourcetype))
						*tptr = OpOf (t);
					else {
						tptr = OpAddr (t);
						is_sub_expression = TRUE;
					}
				}
				break;
				/*}}}  */
				/*{{{  BYTESIN */
#ifdef OCCAM2_5
			case S_BYTESIN:
				/* it must be a data object who's size isn't known;
				   otherwise it would have been constant folded.
				 */
				{
					treenode *type = gettype (OpOf (t));
					treenode *result = NULL;
					const INT32 base_bytes = known_bytesin (type);
					int dim_num;

#ifdef MOBILES

#if 0
fprintf (stderr, "tran1.c: sub_transexp() BYTESIN.  base_bytes = %d, OpOf (t) =", base_bytes);
printtreenl (stderr, 4, OpOf (t));
fprintf (stderr, "*** isdynmobilearray(OpOf(t)) = %d, isdynmobilearrapiece(OpOf(t)) = %d, type =", isdynmobilearray (OpOf (t)), isdynmobilearraypiece (OpOf (t)));
printtreenl (stderr, 4, type);
#endif
					if (TagOf (type) == S_MOBILE) {
						type = MTypeOf (type);
					}
					/* FIXME: nested mobiles */
#endif
					if (base_bytes != 1) {
						result = newconstant (base_bytes);
					}

					for (dim_num=1; TagOf (type) == S_ARRAY; type = follow_user_type (ARTypeOf (type)), dim_num++) {
						if (ARDimOf (type) == (-1)) {
							treenode *dimlength = ARDimLengthOf (type);

							if (!dimlength) {
								dimlength = newdopnode (S_NTH_DIMENSION, NOPOSN, copytree (OpOf (t), trans_lexlevel), newconstant (dim_num), S_INT);
							} else {
								dimlength = copytree (ARDimLengthOf (type), trans_lexlevel);
							}
#if 0
fprintf (stderr, "tran1: sub_transexp() BYTESIN.  dimlength = ");
printtreenl (stderr, 4, dimlength);
#endif
							result = add_to (result, S_MULT, dimlength);
						}
					}
#if 0
fprintf (stderr, "tran1: sub_transexp() BYTESIN.  put together multiplier tree, result = ");
printtreenl (stderr, 4, result);
#endif
					freetree (tptr);
					*tptr = result;
				}
				break;	/* re-iterate with the new tree */
#endif
				/*}}}  */
			}
			break;
			/*}}}  */
			/*{{{  literal nodes */
		case LITNODE:
			if (TagOf (t) == S_CONSTRUCTOR) {
				/*{{{  constructor simplification*/
				if (is_sub_expression && trans_params->abbr_constructors && !no_anonymous_vars) {	/* bug TS/1455 5/11/91 */
					treenode *name;
					/* we must run trans on the sub-expressions _after_
					   structuring the assignments. */
/*sub_transexp(OpAddr(t), FALSE); *//* will be a major expression of the abbr */

					name = create_abbreviation (t, TRUE);
					create_initialised_constructor (name);	/* bug TS/1263 11/05/92 */

#ifdef OCCAM2_5
					/* This can't happen - it is impossible for the abbreviation
					   to overwrite the node, because the abbreviation must precede the
					   whole process/expression.
					   NOT TRUE: it can now happen when a constructor is listed
					   as the result of a VALOF.
					 */
					if (*tptr == t)	{ /* the abbreviation has not overwritten this node */
						*tptr = name;
					} else {
						/* we have transformed the current node from 'expression' into
						   VAL name IS expression :
						   VALOF
						   SKIP
						   RESULT expression
						   We now need to overwrite the 'expression' in result list
						   with the name of the abbreviation.
						 */
						treenode *const resultlist = VLResultListOf (skipspecifications (*tptr));
						/*fprintf(outfile, "sub_transexp: trans const -> VALOF at trans_locn:%d\n", trans_locn); */
						NewItem (name, resultlist);
					}
#else
					*tptr = name;
#endif

					trans (DValAddr (NDeclOf (name)));	/* run this _after_ restructuring */

					return;
				}
				/*}}}*/
			}
			tptr = LitExpAddr (t);
			break;
			/*}}}  */
			/*{{{  dyadic */
		case DOPNODE:
			switch (TagOf (t)) {
				/*{{{  default - normal */
			default:
				sub_transexp (LeftOpAddr (t), TRUE);
				tptr = RightOpAddr (t);
				is_sub_expression = TRUE;
				break;
				/*}}}  */
				/*{{{  checked -> unchecked */
			case S_ADD:
			case S_SUBTRACT:
			case S_MULT:
				if (!NEED_ERRORS) {	/* INSdi02354 */
					const int type = DOpTypeOf (t);
					if (!isreal (type) && (TagOf (t) == S_MULT))
						SetTag (t, S_TIMES);
					else if (isshortint (type))
						SetTag (t, (TagOf (t) == S_ADD) ? S_PLUS : S_MINUS);
				}
				sub_transexp (LeftOpAddr (t), TRUE);
				tptr = RightOpAddr (t);
				is_sub_expression = TRUE;
				break;
				/*}}}  */
				/*{{{  shifts */
			case S_LSHIFT:
			case S_RSHIFT:
				if ((errormode & ERRORMODE_SHIFTCHECK) && !isconst (RightOpOf (t)))
					/*{{{  add tree to range check the shift count */
				{
					const INT32 shiftrange = (8 * (INT32) bytesinscalar (DOpTypeOf (t))) + 1;
					SetRightOp (*tptr, newdopnode (S_CSUB0, LocnOf (t), RightOpOf (t), newconstant (shiftrange), S_INT));
				}
				/*}}}  */
				sub_transexp (LeftOpAddr (t), TRUE);
				tptr = RightOpAddr (t);
				is_sub_expression = TRUE;
				break;
				/*}}}  */
				/*{{{  eval */
			case S_EVAL:
				if (TagOf (LeftOpOf (t)) == S_OVERLAPCHECK) {
					SetLeftOp (*tptr, buildoverlapcheck (LeftOpOf (t)));
					overlap_checks += (TagOf (LeftOpOf (t)) != S_DUMMYEXP);
				}
				if (TagOf (LeftOpOf (t)) == S_DUMMYEXP) {
					*tptr = RightOpOf (t);
					freenode (&t);
				} else {
					sub_transexp (LeftOpAddr (t), TRUE);
					tptr = RightOpAddr (t);
					is_sub_expression = TRUE;
				}
				break;
				/*}}}  */
				/*{{{  AFTER */
				/* bug TS/1568 16/01/92 turn (x AFTER y) into ((x MINUS y) > 0) */
			case S_AFTER:
				SetTag (t, S_MINUS);
#ifdef OCCAM2_5
				if (DOpTypeOf (t) == S_BYTE) {
					/* x AFTER y === (signed)(x MINUS y) > 0                  */
					/*           === ((x MINUS y) > 0) OR ((x MINUS y) < 128) */
					/*           === ((x MINUS y) MINUS 1) < 127              */
					/*           === (x MINUS (y PLUS 1)) < 127               */
					/* NOTE that the latter is more efficient if y is constant */
					if (isconst (RightOpOf (t)))
						SetRightOp (t, foldexp (newdopnode (S_PLUS, LocnOf (t), RightOpOf (t),
										    newintconstant (1, S_BYTE), S_BYTE)));
					else
						*tptr = newdopnode (S_MINUS, LocnOf (t), t, newintconstant (1, S_BYTE), S_BYTE);
					*tptr = newdopnode (S_LS, LocnOf (t), *tptr, newintconstant (127, S_BYTE), S_BYTE);
				} else
#endif
				{
					/* x AFTER y === (x MINUS y) > 0 */
					*tptr = newdopnode (S_GR, LocnOf (t), t, newintconstant (0, DOpTypeOf (t)), DOpTypeOf (t));
				}
				/* Now loop through to properly run trans on this expression */
				break;
				/*}}}  */
				/*{{{  AND/OR/COLON2 */
			case S_AND:
			case S_OR:
/*case S_COLON2: *//* removed for bug TS/1455 4/11/91 */
				sub_transexp (LeftOpAddr (t), TRUE);
#if 0
				/*{{{  attempt to optimise chains of ANDs and ORs */
				/* I tried very simply to optimise chains of AND or OR, where each
				   item is simple, but this de-optimises other stuff done in the
				   code generator to such a degree that there is no point.
				   Better luck next time.
				   CON - 30/9/91
				 */
				if (	/*TagOf(t) != S_COLON2 && *//* removed for bug TS/1455 */
					   issimplelocal (RightOpOf (t))) {
					/* we can possibly convert to a strict operation (faster) */
					/* This converts b AND c AND d AND e into:
					   b BITAND c AND d BITAND e
					   which can be faster
					 */
					const int new_tag = (TagOf (t) == S_AND) ? S_BITAND : S_BITOR;
					if (TagOf (LeftOpOf (t)) != new_tag) {
						DEBUG_MSG (("sub_transspec: converting short-circuit -> strict\n"));
						SetTag (t, new_tag);
					}
				}
				/*}}}  */
#endif
#if 0				/* this doesn't allow for the fact that they are short-circuiting,
				   so we mustn't insert abbreviations 'above' them.
				   Bug TS1324 24/10/91 */
				tptr = RightOpAddr (t);
				is_sub_expression = TRUE;
				break;
#else
				{
					const trans_data_t saved_data = trans_data;
					trans_data.insertvalof = TRUE;	/* OK, cos it will always be a scalar BOOL */
					trans_data.insertpoint = RightOpAddr (t);
/*sub_transexp(RightOpAddr(t), TagOf(t) != S_COLON2); *//* TS/1455 4/11/91 */
					sub_transexp (RightOpAddr (t), TRUE);
					trans_data = saved_data;
					return;
				}
#endif
				/*}}}  */
			}
			break;
			/*}}}  */
			/*{{{  function instance */
		case INSTANCENODE:
			trans (tptr);	/* use the same code as for procedure instances */
			return;
			/*}}}  */
			/*{{{  element */
		case ARRAYSUBNODE:
		case SEGMENTNODE:
#if 0
fprintf (stderr, "sub_transexp: ARRAYSUBNODE/SEGMENTNODE: (before) *tptr = ");
printtreenl (stderr, 4, *tptr);
#endif
			if (TagOf (t) == S_SEGMENT) {
				/* segments of nested MOBILEs cause problems.. */
				transform_mobile_segment (trans_params, tptr);

				/* skip over any inserted abbreviations (although they should not be here..!) */
				while (isspecification (*tptr)) {
					tptr = DBodyAddr (*tptr);
				}
			}
			transsubscripts (trans_params, tptr, TRUE);
#if 0
fprintf (stderr, "sub_transexp: ARRAYSUBNODE/SEGMENTNODE: (middle) *tptr = ");
printtreenl (stderr, 4, *tptr);
#endif
			/*{{{  look out for newly created abbreviations */
			/* bug INSdi03483 08/02/94 */
			while (isspecification (*tptr)) {
				tptr = DBodyAddr (*tptr);
			}
			if (TagOf (*tptr) == S_VALOF) {
				tptr = ThisItemAddr (VLResultListOf (*tptr));
			}
			/*}}}  */
			*tptr = transformelement (trans_params, *tptr, RANGECHECKING, trans_lexlevel);
#if 0
fprintf (stderr, "sub_transexp: ARRAYSUBNODE/SEGMENTNODE: (after) *tptr = ");
printtreenl (stderr, 4, *tptr);
#endif
			return;
			/*}}}  */
			/*{{{  name */
		case NAMENODE:
			if (trans_flag_scalar_chans && (NTypeOf (t) != NULL)	/* bug INSdi02497 2/9/93 */
			    &&(TagOf (NTypeOf (t)) == S_CHAN))
				SetNChanMark (t, TRUE);
			return;
			/*}}}  */
			/*{{{  constants and dummy expression */
		case WORDNODE:
		case CONSTEXPNODE:
		case CONSTTABLENODE:	/*case HIDDENPARAMNODE: */
		case LEAFNODE:	/* S_DUMMYEXP should be the only one */
			return;
			/*}}}  */
			/*{{{  valof */
		case VALOFNODE:	/* this must be an inline valof */
			trans (VLBodyAddr (t));
			/*transexp(VLResultListAddr(t), TRUE); */
			trans_resultlist (tptr);
#if OPTIMISE_VALOFS
			/*We optimise away specifications which have been left in front of
			   valofs which can be turned into constants:
			   (This can particularly occur with INLINE FUNCTIONs).
			   Eg:
			   z := increment(1)
			   which might be expanded into:
			   z := (VAL y IS 2 :
			   VALOF
			   SKIP
			   RESULT 2
			   )
			   which is now turned into:
			   z := 2
			 */
			if (optimise_valof) {	/* we turn this off while inside segment lengths */
				treenode *result = VLResultListOf (t);
				/* This only works for single valued functions, so we
				   might aswell optimise it just for that case */
#if 0				/* 20/9/90 */
				int constant = TRUE;
				int results = 0;
				while (constant && !EndOfList (result)) {
					constant = constant && isconst (ThisItem (result));
					result = NextItem (result);
					results++;
				}
				if (constant && (results == 1))
					*original_tptr = ThisItem (VLResultListOf (t));
#else
				if ((listitems (result) == 1) && isconst (ThisItem (result)))
					*original_tptr = ThisItem (result);
#endif
			}
#endif
			return;
			/*}}}  */
			/*}}}  */
		}
	}
}

/*}}}  */
/*{{{  PRIVATE void transexp (treenode **const tptr, const BOOL is_sub_expression) */
/* This simply calls sub_transexp, and folds the resulting expression tree,
   incase any functions have been in-lined, which provide opportunities for
   constant propagation.
*/
PRIVATE void transexp (treenode **const tptr, const BOOL is_sub_expression)
{
#if 0
fprintf (stderr, "tran2: transexp (enter): *tptr = ");
printtreenl (stderr, 4, *tptr);
#endif
	const BOOL temp = inline_expansions;
	inline_expansions = FALSE;
	sub_transexp (tptr, is_sub_expression);
	if (inline_expansions) {
		if (TagOf (*tptr) == S_LIST) {
			*tptr = foldexplist (*tptr);
		} else {
			*tptr = foldexp (*tptr);
		}
	}
	inline_expansions = temp;
#if 0
fprintf (stderr, "tran2: transexp (done): *tptr = ");
printtreenl (stderr, 4, *tptr);
#endif
}

/*}}}  */
/*{{{  PRIVATE treenode *trans_forked_instance (treenode *tptr)*/
/*
 * turns something like:
 *	FORK p (CLONE strs[i])
 * into:
 *	<typeof strs[i]> $anon:
 *	SEQ
 *	  $anon := CLONE strs[i]
 *	  FORK p ($anon)
 *
 * in cases where "strs[i]" is some complex MOBILE _data_ -- chan-types are simpler.
 * returns a possibly modified tree.  This is called before trans on the actual parameters
 */
PRIVATE treenode *trans_forked_instance (treenode *tptr)
{
	treenode *n = tptr;
	treenode *params;
	static int fanon_count = 0;		/* static to create unique names */
	char name_str[20];
	treenode *decls = NULL;
	treenode **declp = &decls;		/* collects temporary declarations */
	treenode *asslist = NULL;
	treenode **assp = &asslist;

#if 0
fprintf (stderr, "tran1: trans_forked_instance: tptr = ");
printtreenl (stderr, 4, tptr);
#endif
	for (params = IParamListOf (tptr); !EmptyList (params); params = NextItem (params)) {
		treenode *parm = ThisItem (params);

		if ((TagOf (parm) == S_CLONE) && ((TagOf (OpOf (parm)) == S_ARRAYITEM) || (TagOf (OpOf (parm)) == S_ARRAYSUB))) {
			treenode *type = gettype (OpOf (parm));

			if (isdynmobilearraytype (type)) {
				treenode *tname;
#if 0
fprintf (stderr, "tran1: trans_forked_instance: dynamic mobile array type = ");
printtreenl (stderr, 4, type);
#endif
				sprintf (name_str, "$fanon%d", fanon_count++);

				tname = newnamenode (N_DECL, NOPOSN, lookupword (name_str, strlen (name_str)), type, NULL, trans_lexlevel, ANON_VAR_SCOPE, NM_WORKSPACE);
				*declp = newdeclnode (S_DECL, NOPOSN, tname, NULL, NULL);
				SetNDecl (tname, *declp);
				declp = DBodyAddr (*declp);

				SetLeft (params, tname);		/* update parameter */

				/* create assignment node */
				*assp = newlistnode (S_LIST, NOPOSN, newactionnode (S_ASS, NOPOSN, tname, parm), *assp);
				assp = NextItemAddr (*assp);
			}
		}
	}

	if (asslist && decls) {
		/* created something */
		*declp = newcnode (S_SEQ, NOPOSN, asslist);
		*assp = newlistnode (S_LIST, NOPOSN, n, NULL);		/* attach original FORK call */
		n = decls;

#if 0
fprintf (stderr, "tran1: trans_forked_instance: inserted temporary, new tree is: ");
printtreenl (stderr, 4, n);
#endif
	}

	return n;
}
/*}}}*/
#if defined(PD_DECODE_CHANNEL) && defined(PD_DECODE_CHANNEL3) && defined(PD_ENCODE_CHANNEL)
/*{{{  PRIVATE int trans_encode_decode_comm_count (treenode *prot)*/
PRIVATE int trans_encode_decode_comm_count (treenode *prot)
{
	int count = 0;

	if (!prot) {
		return -1;
	}
#if 0
fprintf (stderr, "tran1: trans_encode_decode_comm_count(): prot = ");
printtreenl (stderr, 4, prot);
#endif
	while (prot) {
		switch (TagOf (prot)) {
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
		case S_ARRAY:		/* fixed-size array here */
			/* trivia */
			count++;
			prot = NULL;
			break;
		case S_COLON2:
			/* only one decode->encode communication for this */
			count++;
			prot = NULL;
			break;
		case S_MOBILE:
			if (isdynmobilearraytype (prot)) {
				if (dynmobiledimensioncount (prot) == 1) {
					/* special handling for single dimensional dynamic mobile arrays */
					count++;
				} else {
					count += 2;		/* dimensions, then data */
				}
			} else {
				/* FIXME: will need to handle nested MOBILEs here (urk..!) */
				count++;
			}
			prot = NULL;
			break;
		case N_TYPEDECL:
			if (TagOf (NTypeOf (prot)) == S_MOBILE) {
				prot = NTypeOf (prot);
			} else {
				/* regular block of data */
				count++;
				prot = NULL;
			}
			break;
		case N_PROCTYPEDECL:
			count++;
			prot = NULL;
			break;
		case S_LIST:
			count += trans_encode_decode_comm_count (ThisItem (prot));
			prot = NextItem (prot);
			break;
		case N_TAGDEF:
		case N_SPROTDEF:
			prot = NTypeOf (prot);
			break;
		case N_TPROTDEF:
			count++;		/* for the tag --- lots of potential tags, so don't do anything here! */
			prot = NULL;
			break;
		default:
			msg_out (SEV_ERR, TRANS, TRANS_INTERNAL_ERROR, NOPOSN);
			break;
		}
	}
	return count;
}
/*}}}*/
/*{{{  PRIVATE treenode *trans_encode_decode_mobile_decl (SOURCEPOSN locn, treenode **ldecls, int *ldeclc, treenode *typetree)*/
/*
 *	this creates a MOBILE temporary, currently only for encoding static MOBILE channels -- we must copy+swap..
 */
PRIVATE treenode *trans_encode_decode_mobile_decl (SOURCEPOSN locn, treenode **ldecls, int *ldeclc, treenode *typetree)
{
	treenode *name, *decl;
	static char str[16];
	int slen;

	sprintf (str, "ed.m.anon%d$", *ldeclc);
	if (*ldeclc < 10) {
		slen = 11;
	} else if (*ldeclc < 100) {
		slen = 12;
	} else {
		slen = 13;
	}
	*ldeclc = *ldeclc + 1;

	name = newnamenode (N_DECL, locn, lookupword (str, slen), typetree, NULL /* decl */, trans_lexlevel, ANON_VAR_SCOPE, NM_DEFAULT);
	decl = newdeclnode (S_DECL, locn, name, NULL /* val */, NULL /* body */);
	SetNDecl (name, decl);

	/* add this onto the front of ldecls */
	SetDBody (decl, *ldecls);
	*ldecls = decl;

	return name;
}
/*}}}*/
/*{{{  PRIVATE treenode *trans_encode_decode_mobile_protocol (BOOL is_encode, SOURCEPOSN locn, treenode *cin, treenode *cout, treenode *protocol, treenode *mprot,
 * 		treenode *a_temp, treenode *c_temp, treenode *n_temp, treenode **ldecls, int *ldeclc, int cnum)*/
PRIVATE treenode *trans_encode_decode_mobile_protocol (BOOL is_encode, SOURCEPOSN locn, treenode *cin, treenode *cout, treenode *protocol, treenode *mprot,
		treenode *a_temp, treenode *c_temp, treenode *n_temp, treenode **ldecls, int *ldeclc, int cnum, int eaflags)
{
	treenode *result = NULL;
	treenode *mtype = MTypeOf (mprot);
	treenode *cnumval = NULL;

	/* NOTE: *ldecls is a list holding any temporaries required for (static) MOBILEs, ldeclc counts names */

/*{{{  debugging*/
#if 0
fprintf (stderr, "trans_encode_decode_protocol: is_encode = %d, eaflags = %d, cin =", is_encode, eaflags);
printtreenl (stderr, 4, cin);
fprintf (stderr, "              \"             : cout =");
printtreenl (stderr, 4, cout);
fprintf (stderr, "              \"             : protocol =");
printtreenl (stderr, 4, protocol);
fprintf (stderr, "              \"             : mprot =");
printtreenl (stderr, 4, mprot);
fprintf (stderr, "              \"             : a_temp =");
printtreenl (stderr, 4, a_temp);
fprintf (stderr, "              \"             : c_temp =");
printtreenl (stderr, 4, c_temp);
#endif
/*}}}*/
	while (!result) {
		switch (TagOf (mtype)) {
			/*{{{  S_ARRAY  break*/
		case S_ARRAY:
			if ((ARDimLengthOf (mtype) == NULL) || (TagOf (ARDimLengthOf (mtype)) == S_NTH_DIMENSION)) {
				/* dynamic MOBILE array -- got suddenly complicated..  need to communicate dimesion sizes first (unless
				 * dimension count is 1).. (let gen10 deal with this.. ;)) ! */
				treenode *atype, *tvar;
				treenode *array = mtype;
				int dimcount = 0;

				while (TagOf (array) == S_ARRAY) {
					dimcount++;
					array = ARTypeOf (array);
				}

				if (is_encode) {
					/* we'll need a temporary dynamic MOBILE to output */
					tvar = trans_encode_decode_mobile_decl (locn, ldecls, ldeclc, mprot);
					tvar = newlistnode (S_LIST, locn, tvar, NULL);
				} else {
					tvar = NULL;
				}

				if ((dimcount > 1) && !cnum) {
					/* this only happens when the channel is a MOBILE type not contained as a sub-protocol,
					 * need _two_ communications, so up cnum -- gen10 handles separation.
					 */
					cnum++;
				}
				cnumval = newconstant (cnum);
				result = newactionnode (S_X_INPUT_OUTPUT, locn, cin, newlistnode (S_LIST, locn, cout, newlistnode (S_LIST, locn, cnumval,
							newlistnode (S_LIST, locn, a_temp, newlistnode (S_LIST, locn, c_temp, newlistnode (S_LIST, locn, n_temp, tvar /* listnode */))))));
				/* set the comm type to something meaningless (!): the number of dimensions TIMES base-type size -- gen10 takes care.. */
				atype = newdopnode (S_TIMES, NOPOSN, newconstant (dimcount), newconstant (bytesin (array)), S_INT);

				SetActionType (result, atype);
				SetActionFlags (result, ActionFlagsOf (result) | (is_encode ? ActionFlag_encode : ActionFlag_decode) | ActionFlag_dynmob | eaflags);
			} else {
				/* static MOBILE array */
				treenode *atype, *tvar;
				int tsize;
				treenode *array = mtype;

				tsize = 1;
				while (TagOf (array) == S_ARRAY) {
					treenode *folded;
					
					if ((TagOf (ARDimLengthOf (array)) == S_UINTLIT) && !(LitTypeOf (ARDimLengthOf (array)))) {
						/* set literal type.. */
						SetLitType (ARDimLengthOf (array), newleafnode (S_INT, NOPOSN));
					}
					folded = foldexp (ARDimLengthOf (array));

#if 0
	fprintf (stderr, "trans_encode_decode_mobile_protocol: folded dimlength =");
	printtreenl (stderr, 4, folded);
#endif

					if (!isconst (folded)) {
						return NULL;		/* huh..? */
					} else {
						SetARDimLength (array, folded);
					}

					tsize *= LoValOf (ARDimLengthOf (array));
					array = ARTypeOf (array);
				}
				tsize *= bytesin (array);		/* base-type */

				if (is_encode) {
					/* we'll need a temporary MOBILE */
					tvar = trans_encode_decode_mobile_decl (locn, ldecls, ldeclc, mprot);
					tvar = newlistnode (S_LIST, locn, tvar, NULL);
				} else {
					tvar = NULL;
				}

				/* this populates a_temp and c_temp */
				cnumval = newconstant (cnum);
				result = newactionnode (S_X_INPUT_OUTPUT, locn, cin, newlistnode (S_LIST, locn, cout, newlistnode (S_LIST, locn, cnumval,
							newlistnode (S_LIST, locn, a_temp, newlistnode (S_LIST, locn, c_temp, newlistnode (S_LIST, locn, n_temp, tvar /* listnode */))))));
				atype = newconstexpnode (S_CONSTEXP, locn, newmopnode (S_BYTESIN, locn, mprot, S_INT), 0, tsize);

				SetActionType (result, atype);
				SetActionFlags (result, ActionFlagsOf (result) | (is_encode ? ActionFlag_encode : ActionFlag_decode) | ActionFlag_mobile | eaflags);
			}
			break;
			/*}}}*/
			/*{{{  S_BOOL S_BYTE S_INT S_INT16 S_INT32 S_INT64 S_UINT S_INT16 S_UINT32 S_UINT64 S_REAL32 S_REAL64  break*/
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
			{
				treenode *atype, *tvar;

				if (is_encode) {
					/* we'll need a temporary MOBILE */
					tvar = trans_encode_decode_mobile_decl (locn, ldecls, ldeclc, mprot);
					tvar = newlistnode (S_LIST, locn, tvar, NULL);
				} else {
					tvar = NULL;
				}

				/* this populates a_temp and c_temp */
				cnumval = newconstant (cnum);
				result = newactionnode (S_X_INPUT_OUTPUT, locn, cin, newlistnode (S_LIST, locn, cout, newlistnode (S_LIST, locn, cnumval,
							newlistnode (S_LIST, locn, a_temp, newlistnode (S_LIST, locn, c_temp, newlistnode (S_LIST, locn, n_temp, tvar /* listnode */))))));
				atype = newconstexpnode (S_CONSTEXP, locn, newmopnode (S_BYTESIN, locn, mprot, S_INT), 0, bytesin (mtype));

				SetActionType (result, atype);
				SetActionFlags (result, ActionFlagsOf (result) | (is_encode ? ActionFlag_encode : ActionFlag_decode) | ActionFlag_mobile | eaflags);
			}
			break;
			/*}}}*/
			/*{{{  N_TYPEDECL  break*/
		case N_TYPEDECL:
			/* MOBILE <type> RECORD */
			if (TagOf (NTypeOf (mtype)) != S_RECORD) {
				mtype = follow_user_type (mtype);
			} else if (TypeAttrOf (NTypeOf (mtype)) & TypeAttr_chantype) {
				return NULL;
			} else {
				treenode *atype, *tvar;

				if (is_encode) {
					/* we'll need a temporary MOBILE */
					tvar = trans_encode_decode_mobile_decl (locn, ldecls, ldeclc, mprot);
					tvar = newlistnode (S_LIST, locn, tvar, NULL);
				} else {
					tvar = NULL;
				}

				/* this populates a_temp and c_temp */
				cnumval = newconstant (cnum);
				result = newactionnode (S_X_INPUT_OUTPUT, locn, cin, newlistnode (S_LIST, locn, cout, newlistnode (S_LIST, locn, cnumval,
							newlistnode (S_LIST, locn, a_temp, newlistnode (S_LIST, locn, c_temp, newlistnode (S_LIST, locn, n_temp, tvar /* listnode */))))));
				atype = newconstexpnode (S_CONSTEXP, locn, newmopnode (S_BYTESIN, locn, mprot, S_INT), 0, bytesin (mtype));

				SetActionType (result, atype);
				SetActionFlags (result, ActionFlagsOf (result) | (is_encode ? ActionFlag_encode : ActionFlag_decode) | ActionFlag_mobile | eaflags);
			}
			break;
			/*}}}*/
			/*{{{  N_PROCTYPEDECL  break*/
		case N_PROCTYPEDECL:
			/* MOBILE <proc-type> */
			{
				treenode *tvar;

				if (is_encode) {
					/* temporary mobile required */
					tvar = trans_encode_decode_mobile_decl (locn, ldecls, ldeclc, mprot);
					tvar = newlistnode (S_LIST, locn, tvar, NULL);
				} else {
					tvar = NULL;
				}

				/* this populates a_temp and c_temp */
				cnumval = newconstant (cnum);
				result = newactionnode (S_X_INPUT_OUTPUT, locn, cin, newlistnode (S_LIST, locn, cout, newlistnode (S_LIST, locn, cnumval,
								newlistnode (S_LIST, locn, a_temp, newlistnode (S_LIST, locn, c_temp, newlistnode (S_LIST, locn, n_temp, tvar /* listnode */))))));

				SetActionType (result, newtypenode (S_MOBILE, locn, NULL, mtype));
				SetActionFlags (result, ActionFlagsOf (result) | (is_encode ? ActionFlag_encode : ActionFlag_decode) | ActionFlag_mobile | ActionFlag_mobproc | eaflags);
			}
			break;
			/*}}}*/
		}
	}
	return result;
}
/*}}}*/
/*{{{  PRIVATE int trans_encode_decode_tag_first (treenode *tptr, int flags)*/
PRIVATE int trans_encode_decode_tag_first (treenode *tptr, int flags)
{
	while (tptr) {
		switch (TagOf (tptr)) {
		case S_LIST:
			if (trans_encode_decode_tag_first (ThisItem (tptr), flags)) {
				return 1;
			}
			tptr = NextItem (tptr);
			break;
		case S_SEQ:
			tptr = CBodyOf (tptr);
			break;
		case S_DECL:
			tptr = DBodyOf (tptr);
			break;
		case S_X_INPUT_OUTPUT:
			SetActionFlags (tptr, ActionFlagsOf (tptr) | flags);
			return 1;
		default:
			break;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  PRIVATE treenode *trans_encode_decode_protocol (BOOL is_encode, SOURCEPOSN locn, treenode *cin, treenode *cout, treenode *protocol, treenode *a_temp,
 * 	treenode *c_temp, treenode *n_temp, treenode **ldecls, int *ldeclc, int cnum, int eaflags)*/
PRIVATE treenode *trans_encode_decode_protocol (BOOL is_encode, SOURCEPOSN locn, treenode *cin, treenode *cout, treenode *protocol,
		treenode *a_temp, treenode *c_temp, treenode *n_temp, treenode **ldecls, int *ldeclc, int cnum, int eaflags)
{
	treenode *result = NULL;

#if 0
fprintf (stderr, "trans_encode_decode_protocol: is_encode = %d, eaflags = %d, cin =", is_encode, eaflags);
printtreenl (stderr, 4, cin);
fprintf (stderr, "              \"             : cout =");
printtreenl (stderr, 4, cout);
fprintf (stderr, "              \"             : protocol =");
printtreenl (stderr, 4, protocol);
fprintf (stderr, "              \"             : a_temp =");
printtreenl (stderr, 4, a_temp);
fprintf (stderr, "              \"             : c_temp =");
printtreenl (stderr, 4, c_temp);
fprintf (stderr, "              \"             : n_temp =");
printtreenl (stderr, 4, n_temp);
#endif

	switch (TagOf (protocol)) {
		/*{{{  BOOL BYTE INT INT16 INT32 INT64 UINT UINT16 UINT32 UINT64 REAL32 REAL64 ARRAY  break*/
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
	case S_ARRAY:		/* fixed-size array here */
		{
			treenode *atype, *cnumval;

			/* this populates a_temp and c_temp */
			cnumval = newconstant (cnum);
			result = newactionnode (S_X_INPUT_OUTPUT, locn, cin, newlistnode (S_LIST, locn, cout, newlistnode (S_LIST, locn, cnumval,
						newlistnode (S_LIST, locn, a_temp, newlistnode (S_LIST, locn, c_temp, newlistnode (S_LIST, locn, n_temp, NULL))))));
			atype = newconstexpnode (S_CONSTEXP, locn, newmopnode (S_BYTESIN, locn, protocol, S_INT), 0, bytesin (protocol));

			SetActionType (result, atype);
			SetActionFlags (result, ActionFlagsOf (result) | (is_encode ? ActionFlag_encode : ActionFlag_decode) | eaflags);
		}
		break;
		/*}}}*/
		/*{{{  COLON2  break*/
	case S_COLON2:
		{
			treenode *tmpname, *tmpdecl = NULL;
			treenode **ipoint, *tmp, *actype;
			treenode *ctype = LeftOpOf (protocol);
			treenode *atype = RightOpOf (protocol);
			treenode *cnumval = newconstant (cnum);

			if ((TagOf (atype) != S_ARRAY) || ARDimLengthOf (atype)) {
				/* very bad..! (should never happen) */
				msg_out (SEV_ERR, TRANS, is_encode ? TRANS_ENCODE_CHANNEL_ERROR : TRANS_DECODE_CHANNEL_ERROR, locn);
				break;
			}

			/* create a temporary for the count */
			tmpname = newnamenode (N_DECL, locn, lookupword ("cval$", 5), ctype, NULL /* decl */, trans_lexlevel, ANON_VAR_SCOPE, NM_WORKSPACE);
			tmpdecl = newdeclnode (S_DECL, locn, tmpname, NULL /* val */, NULL /* body */);
			SetNDecl (tmpname, tmpdecl);

			/* LeftOpOf and RightOpOf for COLON2 (dopnode) */
			result = newcnode (S_SEQ, locn, NULL);
			ipoint = CBodyAddr (result);
			SetDBody (tmpdecl, result);
			result = tmpdecl;
			/* result now pointing at S_DECL node, which is what we return */

			/* NOTE: we don't communicate the size separately now, but still need the INPUT_OUTPUT node to talk to occam..! */

			/* deal with the size (back-end makes sure it's stored in tmpname for us) */
			if (is_encode) {
				tmp = newactionnode (S_X_INPUT_OUTPUT, locn, cin, newlistnode (S_LIST, locn, cout, newlistnode (S_LIST, locn, cnumval,
							newlistnode (S_LIST, locn, a_temp, newlistnode (S_LIST, locn, c_temp, newlistnode (S_LIST, locn, n_temp,
							newlistnode (S_LIST, locn, tmpname, NULL)))))));
			} else {
				tmp = newactionnode (S_X_INPUT_OUTPUT, locn, cin, newlistnode (S_LIST, locn, cout, newlistnode (S_LIST, locn, cnumval, newlistnode (S_LIST, locn, tmpname, NULL))));
			}
			actype = newconstexpnode (S_CONSTEXP, locn, newmopnode (S_BYTESIN, locn, ctype, S_INT), 0, bytesin (ctype));
			SetActionType (tmp, actype);
			SetActionFlags (tmp, ActionFlagsOf (tmp) | (is_encode ? ActionFlag_encode : ActionFlag_decode) | ActionFlag_count | eaflags);

			/* attach to SEQ node */
			*ipoint = newlistnode (S_LIST, locn, tmp, NULL);
			ipoint = RightAddr (*ipoint);

			/* now the data */
			tmp = newactionnode (S_X_INPUT_OUTPUT, locn, cin, newlistnode (S_LIST, locn, cout, newlistnode (S_LIST, locn, cnumval,
						newlistnode (S_LIST, locn, a_temp, newlistnode (S_LIST, locn, c_temp, newlistnode (S_LIST, locn, n_temp, newlistnode (S_LIST, locn, tmpname, NULL)))))));
			actype = newdopnode (S_TIMES, locn, tmpname,
					newconstexpnode (S_CONSTEXP, locn, newmopnode (S_BYTESIN, locn, ARTypeOf (atype), S_INT), 0, bytesin (ARTypeOf (atype))), S_INT);
			SetActionType (tmp, actype);
			SetActionFlags (tmp, ActionFlagsOf (tmp) | (is_encode ? ActionFlag_encode : ActionFlag_decode) | ActionFlag_precount | eaflags);

			*ipoint = newlistnode (S_LIST, locn, tmp, NULL);
			ipoint = NULL;
#if 0
fprintf (stderr, "trans1: trans_encode_decode_protocol: COLON2: LeftOpOf (protocol) =");
printtreenl (stderr, 4, LeftOpOf (protocol));
fprintf (stderr, "trans1: trans_encode_decode_protocol: COLON2: RightOpOf (protocol) =");
printtreenl (stderr, 4, RightOpOf (protocol));
#endif
		}
		break;
		/*}}}*/
		/*{{{  MOBILE  break*/
	case S_MOBILE:
		{
			result = trans_encode_decode_mobile_protocol (is_encode, locn, cin, cout, protocol, protocol, a_temp, c_temp, n_temp, ldecls, ldeclc, cnum, eaflags);
		}
		break;
		/*}}}*/
		/*{{{  N_TYPEDECL  break*/
	case N_TYPEDECL:
		if (TagOf (NTypeOf (protocol)) == S_MOBILE) {
			result = trans_encode_decode_mobile_protocol (is_encode, locn, cin, cout, protocol, NTypeOf (protocol), a_temp, c_temp, n_temp, ldecls, ldeclc, cnum, eaflags);
		} else {
			treenode *atype;
			treenode *cnumval = newconstant (cnum);

			/* this populates a_temp and c_temp */
			result = newactionnode (S_X_INPUT_OUTPUT, locn, cin, newlistnode (S_LIST, locn, cout, newlistnode (S_LIST, locn, cnumval,
						newlistnode (S_LIST, locn, a_temp, newlistnode (S_LIST, locn, c_temp, newlistnode (S_LIST, locn, n_temp, NULL))))));
			atype = newconstexpnode (S_CONSTEXP, locn, newmopnode (S_BYTESIN, locn, protocol, S_INT), 0, bytesin (protocol));

			SetActionType (result, atype);
			if (is_encode) {
				SetActionFlags (result, ActionFlagsOf (result) | ActionFlag_encode | eaflags);
			} else {
				SetActionFlags (result, ActionFlagsOf (result) | ActionFlag_decode | eaflags);
			}
		}
		break;
		/*}}}*/
		/*{{{  N_PROCTYPEDECL*/
	case N_PROCTYPEDECL:
		result = trans_encode_decode_mobile_protocol (is_encode, locn, cin, cout, protocol, NTypeOf (protocol), a_temp, c_temp, n_temp, ldecls, ldeclc, cnum, eaflags);
		break;
		/*}}}*/
		/*{{{  N_TAGDEF N_SPROTDEF  break*/
	case N_TAGDEF:
	case N_SPROTDEF:
		{
			treenode *subtype = NTypeOf (protocol);

			if (!subtype) {
				/* must be an empty tag */
				result = newleafnode (S_SKIP, locn);
			} else {
				treenode *tmp = NULL;
				treenode **ipoint = NULL;
				int xcount;
				
				result = newcnode (S_SEQ, locn, NULL);
				ipoint = CBodyAddr (result);

				xcount = trans_encode_decode_comm_count (protocol);
				xcount--;
				while (!EndOfList (subtype)) {
					tmp = trans_encode_decode_protocol (is_encode, locn, cin, cout, ThisItem(subtype), a_temp, c_temp, n_temp, ldecls, ldeclc, xcount, eaflags);
					xcount -= trans_encode_decode_comm_count (ThisItem (subtype));
					subtype = NextItem (subtype);

					*ipoint = newlistnode (S_LIST, locn, tmp, NULL);
					ipoint = NextItemAddr (*ipoint);
				}
			}
		}
		break;
		/*}}}*/
		/*{{{  N_TPROTDEF  break*/
	case N_TPROTDEF:
		{
			treenode *subtype = NTypeOf (protocol);
			treenode *tagname = newnamenode (N_DECL, locn, lookupword ("tag$", 4), newleafnode (S_BYTE, NOPOSN), NULL /* decl */, trans_lexlevel, ANON_VAR_SCOPE, NM_WORKSPACE);
			treenode *tagdecl = newdeclnode (S_DECL, locn, tagname, NULL /* val */, NULL /* body */);
			treenode *seqnode = newcnode (S_SEQ, locn, NULL);
			treenode **ipoint = CBodyAddr (seqnode);	/* where we put a list of things */
			treenode *tag_input, *atype, *casenode;
			treenode *cnumval;
			treenode *taglengths;
			int ntags = 0;
    			// const int n_empty = trans_check_prot_empty_tags (protocol);

			SetNDecl (tagname, tagdecl);
			SetDBody (tagdecl, seqnode);
			result = tagdecl;

			if ((eaflags & ActionFlag_ed3seq) && !is_encode) {
				treenode *sst = subtype;
				treenode *tagszdecl, *ctab;
				int *vals;

				while (!EndOfList (sst)) {
					ntags++;
					sst = NextItem (sst);
				}
				vals = (int *)memalloc (ntags * sizeof (int));
				sst = subtype;
				ntags = 0;
				while (!EndOfList (sst)) {
					vals[ntags] = trans_encode_decode_comm_count (ThisItem (sst));
					ntags++;
					sst = NextItem (sst);
				}

				taglengths = newnamenode (N_VALABBR, locn, lookupword ("$taglengths", 11), newtypenode (S_ARRAY, NOPOSN, NULL, newleafnode (S_INT, NOPOSN)),
						NULL /* decl */, trans_lexlevel, ANON_VAR_SCOPE, NM_WORKSPACE);
				ctab = newconsttablenode (S_CONSTCONSTRUCTOR, locn, newwordnode (S_NAME, (char *)vals, ntags * sizeof(int), NULL), NULL);
				tagszdecl = newdeclnode (S_VALABBR, locn, taglengths, ctab, NULL /* body */);
				SetCTExp (ctab, taglengths);
				SetARDim (NTypeOf (taglengths), ntags);
				SetARDimLength (NTypeOf (taglengths), newconstant (ntags));
				SetNDecl (taglengths, tagszdecl);

#if 0
fprintf (stderr, "trans_encode_decode_protocol: N_TPROTDEF 3seq/decode/case handling.  tagszdecl =");
printtreenl (stderr, 4, tagszdecl);
#endif
				SetDBody (tagszdecl, tagdecl);
				result = tagszdecl;
				/* memfree (vals); */
			} else {
				taglengths = NULL;
			}

			/* set cnumval to newconstant(0) -- this is true for empty tags only at this point..! -- code in gen10 puts right */
			cnumval = newconstant (0);
			/* deal with the tag first */
			if (is_encode) {
				/* encode gets the real ones too for tagged input */
				tag_input = newactionnode (S_X_INPUT_OUTPUT, locn, cin, newlistnode (S_LIST, locn, cout, newlistnode (S_LIST, locn, cnumval,
							newlistnode (S_LIST, locn, a_temp, newlistnode (S_LIST, locn, c_temp, newlistnode (S_LIST, locn, n_temp,
							newlistnode (S_LIST, locn, tagname, NULL)))))));
			} else {
				cnumval = newarraysubnode (S_ARRAYSUB, locn, taglengths, tagname);
				tag_input = newactionnode (S_X_INPUT_OUTPUT, locn, cin, newlistnode (S_LIST, locn, cout, newlistnode (S_LIST, locn, cnumval,
								newlistnode (S_LIST, locn, tagname, NULL))));
			}
			atype = newconstexpnode (S_CONSTEXP, locn, newleafnode (S_DUMMYEXP, locn), 0, 1);
			SetActionType (tag_input, atype);
			SetActionFlags (tag_input, ActionFlagsOf (tag_input) | (is_encode ? ActionFlag_encode : ActionFlag_decode) | ActionFlag_case | eaflags);

			casenode = newactionnode (S_CASE, locn, tagname, NULL /* list of selections */);
			*ipoint = newlistnode (S_LIST, locn, tag_input, newlistnode (S_LIST, locn, casenode, NULL));
			
			ipoint = RHSAddr (casenode);
			while (!EndOfList (subtype)) {
				treenode *tag = ThisItem (subtype);
				treenode *coding = trans_encode_decode_protocol (is_encode, locn, cin, cout, tag /* N_TAGDEF */, a_temp, c_temp, n_temp, ldecls, ldeclc, 0, eaflags);
				treenode *selector = newlistnode (S_LIST, locn, newconstant (NTValueOf (tag)), NULL);

#if 0
				if (!is_encode) {
					trans_encode_decode_tag_first (coding, ActionFlag_precase);
				}
#endif
#if 0
fprintf (stderr, "encode/decode: N_TPROTDEF, coding =");
printtreenl (stderr, 4, coding);
#endif
				*ipoint = newlistnode (S_LIST, locn, newcondnode (S_SELECTION, locn, selector, coding), NULL);
				ipoint = NextItemAddr (*ipoint);
				subtype = NextItem (subtype);
			}
		}
		break;
		/*}}}*/
		/*{{{  default  break*/
	default:
		msg_out (SEV_ERR, TRANS, is_encode ? TRANS_ENCODE_CHANNEL_ERROR : TRANS_DECODE_CHANNEL_ERROR, locn);
		break;
		/*}}}*/
	}
	return result;
}
/*}}}*/
/*{{{  PRIVATE treenode *trans_encode_protocol (SOURCEPOSN locn, treenode *cin, treenode *cout, treenode *protocol, treenode *a_temp, treenode *c_temp, treenode **ldecls, int *ldeclc, int eaflags)*/
PRIVATE treenode *trans_encode_protocol (SOURCEPOSN locn, treenode *cin, treenode *cout, treenode *protocol, treenode *a_temp, treenode *c_temp, treenode *n_temp, treenode **ldecls, int *ldeclc, int eaflags)
{
	return trans_encode_decode_protocol (TRUE, locn, cin, cout, protocol, a_temp, c_temp, n_temp, ldecls, ldeclc, 0, eaflags);
}
/*}}}*/
/*{{{  PRIVATE treenode *trans_decode_protocol (SOURCEPOSN locn, treenode *cin, treenode *cout, treenode *protocol, treenode *a_temp, treenode *c_temp, int eaflags)*/
PRIVATE treenode *trans_decode_protocol (SOURCEPOSN locn, treenode *cin, treenode *cout, treenode *protocol, treenode *a_temp, treenode *c_temp, int eaflags)
{
	treenode *ldecls = NULL;
	int ldeclc = 0;
	treenode *result;

	result = trans_encode_decode_protocol (FALSE, locn, cin, cout, protocol, a_temp, c_temp, NULL, &ldecls, &ldeclc, 0, eaflags);
	/* decode might need temporaries now (for COLON2 handling) */
#if 0
	if (ldecls || ldeclc) {
		/* shouldn't happen -- decode needs no temporaries.. */
		msg_out (SEV_ERR, TRANS, TRANS_DECODE_CHANNEL_ERROR, locn);
	}
#endif
	return result;
}
/*}}}*/
/*{{{  PRIVATE treenode *trans_encode_decode_channel (int mode, treenode *instance)*/
/*{{{  comment*/
/*
 *	turns:
 *		DECODE.CHANNEL3 (in, term, out)
 *	into:
 *		BOOL $tmp:
 *		SEQ
 *		  $tmp := TRUE
 *		  WHILE $tmp
 *		    PRI ALT
 *		      term ??      -- with X_INPUT_OUTPUT
 *		        $tmp := FALSE
 *		      in ??        -- with X_INPUT_OUTPUT
 *		        .. protocol specific handling
 *
 *	and:
 *		DECODE.CHANNEL (in, term, out, go)
 *	into:
 *		BOOL $tmp:
 *		BOOL $tmp2:
 *		SEQ
 *		  $tmp := TRUE
 *		  $tmp2 := FALSE
 *		  WHILE $tmp
 *		    PRI ALT
 *		      term ??      -- with X_INPUT_OUTPUT
 *		        $tmp := FALSE
 *		      go ? $tmp2
 *		        SKIP
 *		      $tmp2 & in ??        -- with X_INPUT_OUTPUT
 *		        .. protocol specific handling
 *
 *	and:
 *		ENCODE.CHANNEL (in, term, out)
 *	into:
 *		something similar to the above, but with different protocol handling
 */
/*}}}*/
PRIVATE treenode *trans_encode_decode_channel (int mode, treenode *instance)
{
	treenode *decl, *a_decl, *c_decl, *n_decl;
	treenode *name, *a_name, *c_name, *n_name;
	treenode *assnode, *whilenode, *termalt, *inalt, *inputnode;
	treenode **pptr;
	const int old = switch_to_real_workspace ();
	const SOURCEPOSN locn = LocnOf (instance);
	treenode *params = IParamListOf (instance);
	treenode *param_in = ThisItem (params);
	treenode *param_term = ThisItem (NextItem (params));
	treenode *param_out = ThisItem (NextItem (NextItem (params)));
	int ldeclc = 0;
	treenode *last_decl, *linkchan;
	int extraflags;
	int have_go = (mode == PD_DECODE_CHANNEL);
	treenode *goalt = NULL;
	treenode *gname = NULL;
	treenode *gdecl = NULL;
	treenode *gassnode = NULL;
	treenode *ginputnode = NULL;
	treenode *param_go = NULL;

	/* can pick any old anonymous name in here :) */
	if (have_go) {
		gname = newnamenode (N_DECL, locn, lookupword ("ed.anon2$", 9), newleafnode (S_BOOL, NOPOSN), NULL /* decl */, trans_lexlevel, ANON_VAR_SCOPE, NM_WORKSPACE);
		gdecl = newdeclnode (S_DECL, locn, gname, NULL /* val */, NULL /* body */);
		SetNDecl (gname, gdecl);
		param_go = ThisItem (NextItem (NextItem (NextItem (params))));
	}

	name = newnamenode (N_DECL, locn, lookupword ("ed.anon$", 8), newleafnode (S_BOOL, NOPOSN), NULL /* decl */, trans_lexlevel, ANON_VAR_SCOPE, NM_WORKSPACE);
	decl = newdeclnode (S_DECL, locn, name, NULL /* val */, gdecl /* body */);
	SetNDecl (name, decl);

	a_name = newnamenode (N_DECL, locn, lookupword ("ed.addr$", 8), newleafnode (S_INT, NOPOSN), NULL /* decl */, trans_lexlevel, ANON_VAR_SCOPE, NM_WORKSPACE);
	a_decl = newdeclnode (S_DECL, locn, a_name, NULL /* val */, decl /* body */);
	SetNDecl (a_name, a_decl);

	c_name = newnamenode (N_DECL, locn, lookupword ("ed.count$", 9), newleafnode (S_INT, NOPOSN), NULL /* decl */, trans_lexlevel, ANON_VAR_SCOPE, NM_WORKSPACE);
	c_decl = newdeclnode (S_DECL, locn, c_name, NULL /* val */, a_decl /* body */);
	SetNDecl (c_name, c_decl);

	n_name = newnamenode (N_DECL, locn, lookupword ("ed.ccnt$", 8), newleafnode (S_INT, NOPOSN), NULL /* decl */, trans_lexlevel, ANON_VAR_SCOPE, NM_WORKSPACE);
	n_decl = newdeclnode (S_DECL, locn, n_name, NULL /* val */, c_decl /* body */);
	SetNDecl (n_name, n_decl);

	/* c_decl is the outermost thing, decl will be where we attach the new code */
	if (have_go) {
		last_decl = gdecl;
	} else {
		last_decl = decl;
	}

	/* build the SEQ, assignment and loop */

	assnode = newactionnode (S_ASS, locn, name, newconstant (1));
	if (have_go) {
		gassnode = newactionnode (S_ASS, locn, gname, newconstant (0));
	}
	whilenode = newcondnode (S_WHILE, locn, name, NULL /* body */);

	if (have_go) {
		pptr = DBodyAddr (gdecl);
	} else {
		pptr = DBodyAddr (decl);
	}
	if (have_go) {
		*pptr = newcnode (S_SEQ, locn, newlistnode (S_LIST, locn, assnode,
						newlistnode (S_LIST, locn, gassnode,
						newlistnode (S_LIST, locn, whilenode, NULL))));
	} else {
		*pptr = newcnode (S_SEQ, locn, newlistnode (S_LIST, locn, assnode,
						newlistnode (S_LIST, locn, whilenode, NULL)));
	}

	/* build the PRI ALT */

	termalt = newaltnode (S_ALTERNATIVE, locn, newconstant (1), NULL /* input */, NULL /* body */);
	if (have_go) {
		goalt = newaltnode (S_ALTERNATIVE, locn, newconstant (1), NULL /* input */, NULL /* body */);
		inalt = newaltnode (S_ALTERNATIVE, locn, gname, NULL /* input */, NULL /* body */);
	} else {
		inalt = newaltnode (S_ALTERNATIVE, locn, newconstant (1), NULL /* input */, NULL /* body */);
	}

	pptr = CondBodyAddr (whilenode);
	if (have_go) {
		*pptr = newcnode (S_PRIALT, locn, newlistnode (S_LIST, locn, termalt,
						newlistnode (S_LIST, locn, goalt,
						newlistnode (S_LIST, locn, inalt, NULL))));
	} else {
		*pptr = newcnode (S_PRIALT, locn, newlistnode (S_LIST, locn, termalt,
						newlistnode (S_LIST, locn, inalt, NULL)));
	}

	/* then the term guard and process */

	assnode = newactionnode (S_ASS, locn, name, newconstant (0));
	inputnode = newactionnode (S_X_INPUT_OUTPUT, locn, param_term, NULL);		/* XEND on term channel */

	pptr = AltBodyAddr (termalt);
	*pptr = newcnode (S_SEQ, locn, newlistnode (S_LIST, locn, assnode,		/* do assignment first */
					newlistnode (S_LIST, locn, inputnode, NULL)));	/* then XEND */
	SetAltChanExp (termalt, param_term);

	if (have_go) {
		gassnode = newactionnode (S_ASS, locn, gname, newconstant (1));
		ginputnode = newactionnode (S_X_INPUT_OUTPUT, locn, param_go, NULL);	/* XEND on go channel */

		pptr = AltBodyAddr (goalt);
		*pptr = newcnode (S_SEQ, locn, newlistnode (S_LIST, locn, gassnode,
					newlistnode (S_LIST, locn, ginputnode, NULL)));

		SetAltChanExp (goalt, param_go);
	}

	/* and the in guard and process */

	if (mode == PD_ENCODE_CHANNEL) {
		linkchan = param_in;
	} else {
		linkchan = param_out;
	}
	extraflags = 0;
	/* use link protocol to figure out extra ENCODE/DECODE flags */
	{
		treenode *lctype = gettype (linkchan);

		/* allowed protocols:
		 *	<SPROTDEF> (INT; INT)
		 *	<SPROTDEF> (INT; INT; INT)
		 *	[2]INT
		 *	[3]INT
		 *	MOBILE [2]INT
		 *	MOBILE [3]INT
		 */
		if ((TagOf (lctype) == S_CHAN) && (TagOf (ProtocolOf (lctype)) == N_SPROTDEF)) {
			treenode *prot = NTypeOf (ProtocolOf (lctype));

			if ((TagOf (prot) == S_LIST) && (TagOf (ThisItem (prot)) == S_INT) &&
					(TagOf (NextItem (prot)) == S_LIST) && (TagOf (ThisItem (NextItem (prot))) == S_INT)) {
				treenode *subsubprot = NextItem (NextItem (prot));

				if (subsubprot && (TagOf (subsubprot) == S_LIST) && (TagOf (ThisItem (subsubprot)) == S_INT) && (NextItem (subsubprot) == NULL)) {
					extraflags |= ActionFlag_ed3seq;
				}
			}
		}

#if 0
fprintf (stderr, "trans_encode_decode_channel: extraflags = 0x%8.8x, lctype =", (unsigned int)extraflags);
printtreenl (stderr, 4, lctype);
#endif
	}
	if (mode == PD_ENCODE_CHANNEL) {
		treenode *protocol = gettype (param_out);

		if (TagOf (protocol) != S_CHAN) {
			/* oops! */
			msg_out (SEV_ERR, TRANS, TRANS_ENCODE_CHANNEL_ERROR, locn);
			inputnode = newleafnode (S_SKIP, locn);
		} else {
			inputnode = trans_encode_protocol (locn, param_in, param_out, ProtocolOf (protocol), a_name, c_name, n_name, DBodyAddr (last_decl), &ldeclc, extraflags);
			if (!inputnode) {
				msg_out (SEV_ERR, TRANS, TRANS_ENCODE_CHANNEL_ERROR, locn);
				inputnode = newleafnode (S_SKIP, locn);
			}
			/* new declarations magically attached.. */
		}
	} else {
		treenode *protocol = gettype (param_in);

		if (TagOf (protocol) != S_CHAN) {
			/* oops! */
			msg_out (SEV_ERR, TRANS, TRANS_DECODE_CHANNEL_ERROR, locn);
			inputnode = newleafnode (S_SKIP, locn);
		} else {
			inputnode = trans_decode_protocol (locn, param_in, param_out, ProtocolOf (protocol), a_name, c_name, extraflags);
			if (!inputnode) {
				msg_out (SEV_ERR, TRANS, TRANS_DECODE_CHANNEL_ERROR, locn);
				inputnode = newleafnode (S_SKIP, locn);
			}
		}
	}

	pptr = AltBodyAddr (inalt);
	*pptr = newcnode (S_SEQ, locn, newlistnode (S_LIST, locn, inputnode, NULL));
	SetAltChanExp (inalt, param_in);

	switch_to_prev_workspace (old);
	return n_decl;
}
/*}}}*/
#endif /* PD_DECODE_CHANNEL && PD_DECODE_CHANNEL3 && PD_ENCODE_CHANNEL */
/*{{{  PRIVATE treenode *trans_suspended_instance (treenode *tptr)*/
/*
 *	transforms an instance of a PROC that suspends so that local channels
 *	passed as parameters are abbreviated first (need a pointer in WS)
 */
PRIVATE treenode *trans_suspended_instance (treenode *tptr)
{
	treenode *fparams = NParamListOf (INameOf (tptr));
	treenode *aparams = IParamListOf (tptr);

#if 0
fprintf (stderr, "tran1: trans_suspended_instance(): tptr = ");
printtreenl (stderr, 4, tptr);
fprintf (stderr, "tran1: trans_suspended_instance(): fparams = ");
printtreenl (stderr, 4, fparams);
#endif
	while (!EndOfList (fparams) && !EndOfList (aparams)) {
		treenode *fparm = ThisItem (fparams);
		treenode *aparm = ThisItem (aparams);

		if ((TagOf (fparm) == S_PARAM_MPP) || ishiddenparam (fparm)) {
			fparams = NextItem (fparams);
			continue;
		}
		if ((TagOf (fparm) == N_PARAM) && NVIndirectOf (fparm)) {
			/* indirect parameter expected, check real */
			if ((TagOf (aparm) == N_DECL) && islocal (aparm, trans_lexlevel)) {
				treenode *abbr = create_abbreviation (aparm, FALSE);

				SetLeft (aparams, abbr);
			}
		}
		aparams = NextItem (aparams);
		fparams = NextItem (fparams);
	}
	return tptr;
}
/*}}}*/
/*{{{  PRIVATE void trans (treenode **tptr)*/
/*{{{  comment */
/*****************************************************************************
 *
 *  trans takes a complete SC tree and performs the following transformations:
 *        1.  ARRAYSUB trees are expanded to ARRAYITEM trees
 *        2.  SEGMENT trees are expanded to SEGMENTITEM trees
 *        3.  Temporaries are inserted (but not scoped) on segment start and
 *            length trees if required
 *        4.  Hidden dimensions are inserted in all type trees.
 *        5.  Formal parameter lists are augmented with hidden dimensions
 *            and result pointers.
 *        6.  Checks are added to shift operators if required.
 *        7.  Inline routine calls are expanded out.
 *        8.  All variables have their SetNMode field set correctly.
 *        9.  Constructors are `abbreviated' out so that they only appear
 *            in abbreviations.
 *
 *****************************************************************************/
/*}}}  */
PRIVATE void trans (treenode **tptr)
{
	BOOL going = TRUE;
	while (going && (*tptr != NULL)) {
		/*{{{  save insertpoint */
		const trans_data_t saved_data = trans_data;
		/*}}}  */
		treenode *t;
		int tag;
		tptr = transspecs (tptr);
		t = *tptr;	/* t is never modified after this */
		if (t == NULL) {
			return;
		}
		tag = TagOf (t);
		trans_locn = LocnOf (t);
		/*{{{  update insertpoint for anonymous variables */
		/* if the tag is FINSTANCE, we were just called from sub_transexp */
		if (tag != S_FINSTANCE) {	/* bug TS/1453 1/11/91 */
			trans_data.insertpoint = tptr;
			trans_data.insertvalof = FALSE;
		}
		/*DEBUG_MSG(("trans: setting trans_data at %lX on %s\n", trans_locn, itagstring(tag))); */

#if 0
fprintf (stderr, "*** debug: TRANS on ");
printtreenl (stderr, 4, t);
#endif
		/*}}}  */
		switch (nodetypeoftag (tag)) {
			/*{{{  cases */
			/*{{{  default - error */
		default:
			badtag (trans_locn, tag, "trans");
			break;
			/*}}}  */
			/*{{{  X_FIRST_HALF X_SECOND_HALF */
		case MOPNODE:
			switch (tag) {
			case S_X_FIRST_HALF:
			case S_X_SECOND_HALF:
				/* silently step over these */
				tptr = OpAddr (t);
				break;
			default:
				badtag (trans_locn, tag, "trans");
				break;
			}
			break;
			/*}}}  */
			/*{{{  SEQ IF ALT PAR PRIALT PRIPAR GUY ASM FORKING CLAIM RESIGN*/
		case CNODE:
			switch (tag) {
			case S_FORKING:
				{
					/* remove the FORKING node if the $fork.barrier usecount is zero */
					treenode *b_nptr = DNameOf (CBodyOf (t));

#if 0
fprintf (stderr, "trans: FORKING: b_nptr (usecount %d) = ", NVUseCountOf (b_nptr));
printtreenl (stderr, 4, b_nptr);
#endif
					if (NVUseCountOf (b_nptr) == 0) {
						/* yes, unused.  update *tptr */
						*tptr = DBodyOf (CBodyOf (t));
					} else {
						tptr = CBodyAddr (t);
					}
				}
				break;
			case S_CLAIM:
				{
					/* call trans on the claimnode */
					transexp (CTempAddr (t), TRUE);
					tptr = CBodyAddr (t);
				}
				break;
			case S_PAR:
			case S_PRIPAR:
				if (inside_suspends) {
					/* late `scoping' of name for barrier resign */
					treenode *pdname = fe_lookupname (lookupword ("MPBARRESIGN", 11));
					treenode *body = CBodyOf (t);

					if (!NVUseCountOf (pdname)) {
						/* in theory, this only happens once per compilation unit, since MPBARRESIGN is a predefine */
						augmentformals (pdname, trans_lexlevel);
						SetNVUseCount (pdname, NVUseCountOf (pdname) + 1);
					}

					while (!EndOfList (body)) {
						treenode **bptr = ThisItemAddr (body);
						treenode *ires = newinstancenode (S_PINSTANCE, NOPOSN, pdname, NULL);

						*bptr = newcnode (S_SEQ, NOPOSN, newlistnode (S_LIST, NOPOSN, *bptr, newlistnode (S_LIST, NOPOSN, ires, NULL)));

						body = NextItem (body);
					}
#if 0
fprintf (stderr, "trans: PAR/PRIPAR inside SUSPENDs: pdname = ");
printtreenl (stderr, 4, pdname);
/*
fprintf (stderr, "trans: PAR/PRIPAR inside SUSPENDs: altered body = ");
printtreenl (stderr, 4, CBodyOf (t));
*/
#endif
				}
				tptr = CBodyAddr (t);
				break;
			case S_RESIGN:
				/* body should be a single process, reduce it LIST */
				if (TagOf (CBodyOf (t)) == S_LIST) {
					SetCBody (t, ThisItem (CBodyOf (t)));
				}
				tptr = CBodyAddr (t);
				break;
			default:
				tptr = CBodyAddr (t);
				break;
			}
			break;
			/*}}}  */
			/*{{{  REPLSEQ REPLIF REPLALT REPLPAR PRIREPLALT PRIREPLPAR */
		case REPLCNODE:
			transexp (ReplCStartExpAddr (t), FALSE);
			transexp (ReplCLengthExpAddr (t), FALSE);
			transexp (ReplCStepExpAddr (t), FALSE);
			while (isspecification (*tptr)) {
				tptr = DBodyAddr (*tptr);	/* skip over any new declarations */
			}
			if (parrepl (tag)) {
				if (inside_suspends) {
					/* late `scoping' of name for barrier resign */
					treenode *pdname = fe_lookupname (lookupword ("MPBARRESIGN", 11));
					treenode **bptr = ReplCBodyAddr (t);
					treenode *ires = NULL;
					
					if (!NVUseCountOf (pdname)) {
						/* in theory, only occurs once */
						augmentformals (pdname, trans_lexlevel);
						SetNVUseCount (pdname, NVUseCountOf (pdname) + 1);
					}
					ires = newinstancenode (S_PINSTANCE, NOPOSN, pdname, NULL);
					*bptr = newcnode (S_SEQ, NOPOSN, newlistnode (S_LIST, NOPOSN, *bptr, newlistnode (S_LIST, NOPOSN, ires, NULL)));
				}
				/*{{{  replicated PAR */
				if (trans_params->replpar_becomes_proc) {
					trans_process_replpar (tptr);
				} else {
					trans_lexlevel++;
					DEBUG_MSG (("trans: raising lex level as we enter a repl par\n"));
					trans (ReplCBodyAddr (t));
					trans_lexlevel--;
					DEBUG_MSG (("trans: lowering lex level as we leave a repl par\n"));
				}
				going = FALSE;
				/*}}}*/
			} else if ((tag == S_REPLSEQ) && (TagOf (ReplCLengthExpOf (t)) == S_CONSTEXP) && (LoValOf (ReplCLengthExpOf (t)) == 1)) {
				/*{{{  'fold' replicator FROM ... FOR 1 */
				/* bug INSdi01228 */
				/* Don't do this for replicated PAR because that would need
				   adjusting all the lexlevels of locally declared variables.
				   Repl IF and repl ALT have the problem about what to do
				   when they appear as the outermost construct of their type.
				 */
				trans_process_repl_for_one (tptr);
				/*}}}  */
			} else {
				tptr = ReplCBodyAddr (t);
			}
			break;

			/*}}}  */
			/*{{{  WHILE CHOICE */
		case CONDNODE:
			if (tag == S_VARIANT) {
				const BOOL saved_anonymous_vars = no_anonymous_vars;
				/* None inside the input list of a case input - bug 1455 5/11/91 */
				/* OK even if a CASE rather than CASE input, because they will be
				   constant anyway. */
				no_anonymous_vars = TRUE;
				transexp (VRTaggedListAddr (t), FALSE);
				no_anonymous_vars = saved_anonymous_vars;
			} else
				transexp (CondGuardAddr (t), FALSE);
			tptr = CondBodyAddr (t);
			break;
			/*}}}  */
			/*{{{  ALTERNATIVE */
		case ALTNODE:
			{
				/* modified 2/11/90 to create a copy of the channel expression
				   so that it isn't mapped twice; once when enabling, and
				   once when the input is actually performed */
				/* bug TS/1626 21/04/92; Now we move the input into the body of the ALT */
				treenode *input = AltInputOf (t);

				/*{{{  Set up Channel and Time expressions */
				if ((input != NULL) && (AltChanExpOf (t) == NULL)) {
					switch (inputtypeof (input)) {
					case INP_INPUT:
					case INP_CASE_INPUT:
					case INP_TAGGED_INPUT:
					case INP_X_INPUT:
					case INP_X_CASE_INPUT:
					case INP_X_TAGGED_INPUT:
						SetAltChanExp (t, copytree (LHSOf (input), trans_lexlevel));
						break;
					case INP_DELAYED_INPUT:
						SetAltChanExp (t, LHSOf (input));
						SetAltTimeExp (t, RHSOf (input));
						freenode (AltInputAddr (t));
						input = NULL;
						break;
					default:	/* various types of SKIP guard */
						break;	/* leave the channel expression as NULL */
					}
				}
				/*}}}  */
				/*{{{  Set up Guard */
				if (AltGuardOf (t) == NULL)
					SetAltGuard (t, newconstant (1));	/* bug TS/1584 29/01/92 */
				/*}}}  */
				/*{{{  Move the input to the body */
				/* bug TS/1626 21/04/92; Now we move the input into the body of the ALT */
				if (input != NULL) {
					treenode *newbody;
					if (AltBodyOf (t) == NULL)	/* case input, etc */
						newbody = input;
					else if (TagOf (AltBodyOf (t)) == S_SEQ) {
						newbody = AltBodyOf (t);
						SetCBody (newbody, addtofront (input, CBodyOf (newbody)));
					} else
						newbody = newcnode (S_SEQ, trans_locn, addtofront (input, addtofront (AltBodyOf (t), NULL)));
					SetAltInput (t, NULL);
					SetAltBody (t, newbody);
				}
				/*}}}  */

				transexp (AltGuardAddr (t), FALSE);
				transexp (AltChanExpAddr (t), FALSE);
				transexp (AltTimeExpAddr (t), FALSE);
/*trans(AltInputAddr(t)); *//* This will now be NULL */
				tptr = AltBodyAddr (t);
			}
			break;
			/*}}}  */
			/*{{{  LIST */
		case LISTNODE:
			trans (ThisItemAddr (t));
			tptr = NextItemAddr (t);
			break;
			/*}}}  */
			/*{{{  SKIP STOP END SUSPEND SYNC */
		case LEAFNODE:
			switch (tag) {
			case S_SUSPEND:
				/* transform into instance of PD_MPBARSYNC */
#if 0
fprintf (stderr, "S_SUSPEND: >>usecount = %d, LeafLinkOf (t) = ", NVUseCountOf (LeafLinkOf (t)));
printtreenl (stderr, 4, LeafLinkOf (t));
#endif
				if (!NVUseCountOf (LeafLinkOf (t))) {
					augmentformals (LeafLinkOf (t), trans_lexlevel);
					SetNVUseCount (LeafLinkOf (t), NVUseCountOf (LeafLinkOf (t)) + 1);
				}
				*tptr = newinstancenode (S_PINSTANCE, LocnOf (*tptr), LeafLinkOf (t), NULL);
				break;
			default:
				going = FALSE;
				break;
			}
			break;
			/*}}}  */
			/*{{{  instance */
		case INSTANCENODE:
			{
				treenode *tt;
				treenode *const name = INameOf (t);
				const SOURCEPOSN locn = trans_locn;
				const BOOL inside_fn = (tag == S_FINSTANCE);

				/*{{{  call trans on actual parameters */
				{
					/*
					 * we have to call trans on actual parameters first, so that
					 * we correctly expand actual parameters which are themselves
					 * instances of inlined functions - bug 1004 4/10/90
					 */
					const int saved_checks = overlap_checks;
					const BOOL saved_optimise_valof = optimise_valof;
					const BOOL dont_optimise_params =	/* bug 1179 26/9/91 */
						((TagOf (name) == N_PREDEFPROC) && (NModeOf (name) == PD_ASSERT));
					overlap_checks = 0;
					optimise_valof = !dont_optimise_params;	/* bug 1179 26/9/91 */

#ifdef MOBILES
					if (!inside_fn && IForkedOf (t)) {
						/* check for CLONEd parameters that we need to place in temporaries first */
						*tptr = trans_forked_instance (t);
						if (*tptr != t) {
							break;		/* from switch(), go round again */
						}
					}
#endif
					transexp (IParamListAddr (t), TRUE);	/* do params first */
					if (IDynaddrOf (t)) {
						transexp (IDynaddrAddr (t), TRUE);
					}
					/* added for bug TS/1530 11/12/91 */
					while (isspecification (*tptr))	{
						/* incase anon vars have been added */
						tptr = DBodyAddr (*tptr);
					}
					if (TagOf (*tptr) == S_VALOF) {
						tptr = ThisItemAddr (VLResultListOf (*tptr));
					}
					tt = *tptr;

					overlapwarnings (locn);	/* bug 1118 25/1/91 */
					overlap_checks = saved_checks;
					optimise_valof = saved_optimise_valof;
				}
				/*}}}  */

				if (isinline (name)) {
					/*{{{  expand out the inline routine definition */
					*tptr = expand_inline_instance (tt, locn, trans_lexlevel);
					foldtree (*tptr);	/* fold procedure instance */

#if 0
					printf ("trans: expanded body of INLINE routine is\n");
					printsrc (stdout, 0, *tptr);
					printf ("\n");
#endif

					inline_expansions = TRUE;
					if (inside_fn) {
						sub_transexp (tptr, TRUE);
					} else {
						trans (tptr);
					}
					/*}}}  */
				} else {
/* transexp(IParamListAddr(tt)); *//* done already */
					if (inside_fn && (TagOf (name) == N_PREDEFFUNCTION)) {
						/*{{{  special transformations for some predefines */
						switch (NModeOf (name)) {
							/*{{{  SHIFT predefines */
							/* TS/1979 30/11/92 - CON
							   Note that these are illegal in this implementation because the naive
							   instruction sequence would generate the incorrect result,
							   and we don't want to pessimise the 'quick' case to cope with the
							   nasty cases.

							   SHIFTLEFT and SHIFTRIGHT are permitted to have 'long' shift values, because
							   they still produce the correct results.
							 */
						case PD_ASHIFTLEFT:
						case PD_ASHIFTRIGHT:
						case PD_ROTATELEFT:
						case PD_ROTATERIGHT:
						case PD_BITREVNBITS:
							{
								/* insert a range check on the 'places' (second) parameter */
								treenode *const param2 = NextItem (IParamListOf (tt));

								if ((errormode & ERRORMODE_SHIFTCHECK) && (!isconst (ThisItem (param2)))) {
									/* sets ThisItem(param2) */
									NewItem (newdopnode (S_CSUB0, locn, ThisItem (param2),
										newconstant ((8 * (INT32) bytesinscalar (S_INT)) + 1), S_INT), param2);
								}
							}
							break;
							/*}}}  */
						default:
							break;
						}
						/*}}}  */
					} else if (TagOf (name) == N_PREDEFPROC) {
						#if defined(PD_DECODE_CHANNEL) && defined(PD_DECODE_CHANNEL3) && defined(PD_ENCODE_CHANNEL)
						/*{{{  more special transformations for some predefines*/
						switch (NModeOf (name)) {
							/*{{{  DECODE.CHANNEL, ENCODE.CHANNEL*/
						case PD_DECODE_CHANNEL:
						case PD_DECODE_CHANNEL3:
						case PD_ENCODE_CHANNEL:
							{
								/* replaces *tptr (this instance) with something appropriate -- calls trans recursively on this */
								*tptr = trans_encode_decode_channel (NModeOf (name), tt);
#if 0
fprintf (stderr, "trans: mangled for ENCODE/DECODE, *tptr =");
printtreenl (stderr, 4, *tptr);
#endif
								trans (tptr);
								/* *tptr = newleafnode (S_SKIP, LocnOf (tt)); */
							}
							break;
							/*}}}*/
						default:
							break;
						}
						/*}}}*/
						#endif /* PD_DECODE_CHANNEL && PD_DECODE_CHANNEL3 && PD_ENCODE_CHANNEL */
					}
				}

				/* check for indirect parameters to things that SUSPEND -- local channel vars need to be abbreviated so we can get a WS address */
				if (NPSuspendsOf (name)) {
					treenode *tmp = trans_suspended_instance (tt);

					while (isspecification (*tptr)) {
						tptr = DBodyAddr (*tptr);
					}
					*tptr = tmp;
#if 0
fprintf (stderr, "did trans_suspended_instance, *tptr now: ");
printtreenl (stderr, 4, *tptr);
#endif
				}

				/* check for instance of something that must be called dynamically */
				if (NPDyncallOf (name)) {
#if 0
fprintf (stderr, "trans: instance of NPDyncall thing, t =");
printtreenl (stderr, 4, t);
#endif
					switch (tag) {
					case S_PINSTANCE:
					case S_FINSTANCE:
						SetIDynmem (t, 1);
						break;
					}
				}
			}
			going = FALSE;
			break;

			/*}}}  */
			/*{{{  action */
		case ACTIONNODE:

			/*{{{  Source transformations for i/o */
			if ((ActionTypeOf (t) == NULL) && trans_params->simplify_structured_io) {
				BOOL break_out_of_switch;
				switch (tag) {
					/*{{{  sequential i/o */
				case S_INPUT:
				case S_X_INPUT:
				case S_OUTPUT:
					{
						const BOOL modified = trans_process_sequential_io (tptr);
#if 0
if (tag == S_OUTPUT) {
fprintf (stderr, "trans: ACTIONNODE: did OUTPUT with trans_process_sequential_io, modified = %d, *tptr =", modified);
printtreenl (stderr, 4, *tptr);
}
#endif
						break_out_of_switch = modified;
					}
					break;
					/*}}}  */
					/*{{{  tagged input */
				case S_TAGGED_INPUT:
				case S_X_TAGGED_INPUT:
					trans_process_tagged_input (tptr);
					break_out_of_switch = TRUE;
					break;

					/*}}}  */
					/*{{{  case input */
				case S_CASE_INPUT:
				case S_X_CASE_INPUT:
					trans_process_case_input (tptr);
					break_out_of_switch = TRUE;
					break;
					/*}}}  */
				default:
					break_out_of_switch = FALSE;
					break;
				}
				if (break_out_of_switch) {
					break;	/* break out of master switch statement */
				}
			}
			/*}}}  */
#ifdef MOBILES
			/*{{{  Source transformations for dynamic MOBILE assignment */
			if ((tag == S_ASS) && isdynmobilearray (LHSOf (t)) && (TagOf (RHSOf (t)) != S_CLONE) &&
					!isdynmobilearray (RHSOf (t)) && (TagOf (RHSOf (t)) != S_NEW_ARRAY)) {
				/* go mangle it */
				treenode **newtptr;

				newtptr = trans_process_dynmobile_assign (tptr);
				if (newtptr) {
					int newtag = TagOf (*newtptr);
					/*{{{  Source transformation for constructor assignment */
					if ((newtag == S_ASS) && (TagOf (RHSOf (*newtptr)) == S_CONSTRUCTOR) &&
							trans_params->abbr_constructors && !usedin (LHSOf (*newtptr), LitExpOf (RHSOf (*newtptr)), trans_lexlevel)) {
						trans_constructor_assign (newtptr);
						/* and trans it, won't be to a MOBILE LHS anymore */
						trans (newtptr);
					}
					/*}}}  */
					going = FALSE;
					break;	/* break out of master switch statement */
				}
			}
			/*}}}  */
#endif	/* MOBILES */
			/*{{{  Source transformations for counted array i/o */
			if (!EndOfList (RHSOf (t)) && (TagOf (RHSOf (t)) == S_LIST) &&	/* INSdi01982 */
			    (TagOf (ThisItem (RHSOf (t))) == S_COLON2) && trans_params->simplify_counted_io) {
				if ((tag == S_INPUT) || (tag == S_OUTPUT) || (tag == S_X_INPUT)) {
					trans_process_counted_io (tptr);
				} else {
					badtag (trans_locn, tag, "trans_counted_array");
				}
				break;	/* break out of master switch statement */
			}
			/*}}}  */
			/*{{{  Source transformation for constructor assignment */
			if ((tag == S_ASS) && (TagOf (RHSOf (t)) == S_CONSTRUCTOR) &&
					trans_params->abbr_constructors && !usedin (LHSOf (t), LitExpOf (RHSOf (t)), trans_lexlevel)) {
				trans_constructor_assign (tptr);
				break;	/* break out of master switch statement */
			}
			/*}}}  */
			/*{{{  Source transformation for BOOL CASE */
			if ((tag == S_CASE) && (ntypeof (LHSOf (t)) == S_BOOL)) {
				const BOOL break_out_of_switch = trans_bool_case (tptr);
				if (break_out_of_switch)
					break;	/* break out of master switch statement */
			}
			/*}}}  */
			/*{{{  Source transformation for extended input */
			if ((tag == S_X_INPUT) || (tag == S_X_TAGGED_INPUT)) {
				transexp (LHSAddr (t), FALSE);
				transexp (RHSAddr (t), FALSE);
				trans (ActionDuringAddr (t));
				tptr = ActionAfterAddr (t);
				break;	/* out of master switch() */
			}
			/*}}}  */
			/*{{{  source transformation for CASE/CASE-input */
			switch (tag) {
			case S_CASE:
			case S_CASE_INPUT:
			case S_X_CASE_INPUT:
				transexp (LHSAddr (t), FALSE);

				if ((tag == S_CASE)	/* added for bug 1052 - 23/11/90 */
				    &&isshortint (ntypeof (LHSOf (t)))) {
					SetLHS (*tptr, newmopnode (S_EXACT, LocnOf (t), LHSOf (t), S_INT));
					/* really we should constant fold here, but nobody uses
					   constants in CASE expressions! */
					/* Note too that we don't need to change the type of
					   all the selection values - they fall out in the wash */
				}
				tptr = RHSAddr (t);
				break;
			default:
				{
					const SOURCEPOSN thislocn = trans_locn;
					const BOOL saved_anonymous_vars = no_anonymous_vars;
					const BOOL abbreviate_constructors = (tag == S_OUTPUT) ||	/* bug TS/1238 11/12/91 */
						((tag == S_ASS) && ((TagOf (RHSOf (t)) == S_LIST) ||	/* bug TS/1263 11/05/92 */
								    (TagOf (RHSOf (t)) == S_CONSTRUCTOR)));	/* INSdi03379 */

					overlap_checks = 0;
					transexp (LHSAddr (t), FALSE);

					/* None inside the input list of an input - bug 1455 5/11/91 */
					if (!trans_params->simplify_structured_io)	/* bug TS/1626 means this is unnecessary! */
						no_anonymous_vars = (tag == S_INPUT) || (tag == S_TAGGED_INPUT) || (tag == S_X_INPUT) || (tag == S_X_TAGGED_INPUT);

					/*transexp(RHSAddr(t), FALSE); */
					transexp (RHSAddr (t), abbreviate_constructors);
					overlapwarnings (thislocn);
					no_anonymous_vars = saved_anonymous_vars;
				}
				going = FALSE;
				break;
			}
			/*}}}*/
			break;
			/*}}}  */
			/*{{{  valof */
		case VALOFNODE:	/* This must be a function body */
			trans (VLBodyAddr (t));
			/*transexp(VLResultListAddr(t), TRUE); */
			trans_resultlist (tptr);
			going = FALSE;
			break;
			/*}}}  */
			/*{{{  GUYCODE GUYSTEP */
		case DOPNODE:
			if (tag == S_GUYCODE) {
				const BOOL saved_flag_scalar_chans = trans_flag_scalar_chans;
				trans_flag_scalar_chans = TRUE;
				transexp (RightOpAddr (t), FALSE);
				trans_flag_scalar_chans = saved_flag_scalar_chans;
			}
			going = FALSE;
			break;
			/*}}}  */
		}
		/*}}}  */
		/*{{{  reset insertpoint */
		trans_data = saved_data;
		/*DEBUG_MSG(("trans: resetting trans_data\n")); */
		/*}}}  */
	}
}

/*}}}  */

/*{{{  PUBLIC void transmain */
PUBLIC void transmain (const trans_params_t * const trans_params_in, treenode ** tptr)
{
	jmp_buf savedenv;
	const int savedlexlevel = trans_lexlevel;
	const trans_params_t *const saved_params = trans_params;
	const BOOL saved_flag_scalar_chans = trans_flag_scalar_chans;
	trans_lexlevel = 0;
	trans_params = trans_params_in;
	trans_flag_scalar_chans = FALSE;

	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) == 0) {
		trans (tptr);
	} else {
		*tptr = NULL;
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));

	trans_flag_scalar_chans = saved_flag_scalar_chans;
	trans_lexlevel = savedlexlevel;
	trans_params = saved_params;
}

/*}}}  */
