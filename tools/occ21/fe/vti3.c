/* $Id: vti3.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	occam two virtual tree walk and copying routines
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
#include "midinc.h"
#include "vtierror.h"

#include "chkdef.h"		/* current_fe_handle etc */

#include "usehdr.h"		/* needed for usedef.h */
#include "usedef.h"		/* use_copy_free_vars */
/*}}}*/

/*{{{  tree copying */
/*{{{  comment */
/*
 *  These routines are used to make new copies of trees
 *  The standard routine is copytree which returns a copy of the
 *  tree whose root is passed to it as a parameter.
 *  Any declarations within the tree will be copied and references
 *  to those declarations changed to the new copy.
 *  It is assumed the parameter 'lexlevel' is set to the lexlevel of
 *  the new tree and will be used when declaring new copies of variables.
 *
 *  To make a copy of the tree and at the same time change
 *  some variable references to expressions (eg replace formal with
 *  actual parameters) first call initnametrans followed by addnametrance
 *  for each name to be translated and then call transcopytree to do
 *  the copy and translation.
 */
/*}}}*/

/*{{{  PRIVATE data */
/* This is 'realloc-ed' as necessary */
#define MAXNODECOPY 100
PRIVATE int sizenodecopy = MAXNODECOPY;

/* This is 'realloc-ed' as necessary */
#define MAXNODESTACK 10		/* changed to 10 16/1/91 */
PRIVATE int sizenodestack = MAXNODESTACK;

typedef struct
{
	treenode *old;
	treenode *new;
}
nametrans_t;

PRIVATE nametrans_t *nametranstable = NULL;
PRIVATE int numnametrans;
PRIVATE int nametransbase;
PRIVATE int nametransstackptr;
PRIVATE int *nametransstack = NULL;

PRIVATE SOURCEPOSN copylocn;	/* Locn of current treenode being copied */
PRIVATE SOURCEPOSN parentlocn;	/* Locn of 'inliner' instance */
PRIVATE int copy_lexlevel;	/* lexlevel of tree being copied */
/*}}}*/
/*{{{  FORWARD declarations */
PRIVATE void makecopy (treenode ** tptr);

/*}}}*/

/*{{{  PRIVATE int nodesize(t) */
PRIVATE int nodesize (const int t)
{
	switch (nodetypeoftag (t)) {
	default:
		badtag (copylocn, t, "nodesize");
		return 0;
	case ACTIONNODE:
		return TREENODEBASE + sizeof (struct actionnode_s);
	case ALTNODE:
		return TREENODEBASE + sizeof (struct altnode_s);
	case ARRAYSUBNODE:
		return TREENODEBASE + sizeof (struct arraysubnode_s);
	case CNODE:
		return TREENODEBASE + sizeof (struct cnode_s);
	case CONDNODE:
		return TREENODEBASE + sizeof (struct condnode_s);
#ifdef CONDEXP
	case CONDEXPNODE:
		return TREENODEBASE + sizeof (struct condexpnode_s);
#endif
	case CONSTEXPNODE:
		return TREENODEBASE + sizeof (struct constexpnode_s);
	case CONSTTABLENODE:
		return TREENODEBASE + sizeof (struct consttablenode_s);
	case DECLNODE:
		return TREENODEBASE + sizeof (struct declnode_s);
	case DOPNODE:
		return TREENODEBASE + sizeof (struct dopnode_s);
	case INSTANCENODE:
		return TREENODEBASE + sizeof (struct instancenode_s);
	case LEAFNODE:
		return TREENODEBASE + sizeof (struct leafnode_s);
	case LISTNODE:
		return TREENODEBASE + sizeof (struct listnode_s);
	case LITNODE:
		return TREENODEBASE + sizeof (struct litnode_s);
	case MOPNODE:
		return TREENODEBASE + sizeof (struct mopnode_s);
	case NAMENODE:
		return TREENODEBASE + sizeof (struct namenode_s);
	case PROCESSORNODE:
		return TREENODEBASE + sizeof (struct processornode_s);
	case REPLCNODE:
		return TREENODEBASE + sizeof (struct replcnode_s);
	case SEGMENTNODE:
		return TREENODEBASE + sizeof (struct segmentnode_s);
	case SPACENODE:
		return TREENODEBASE + sizeof (struct spacenode_s);
	case TYPENODE:
		return TREENODEBASE + sizeof (struct typenode_s);
	case VALOFNODE:
		return TREENODEBASE + sizeof (struct valofnode_s);
#if 1				/*def CONFIG */
	case CONFIGNODE:
		return TREENODEBASE + sizeof (struct confignode_s);
#endif
	}
}

/*}}}*/
/*{{{  PRIVATE treenode *lookupnametrans(tptr) */
PRIVATE treenode *lookupnametrans (treenode * tptr)
{
	int i;
	if (tptr == NULL)
		err_abort ("lookupnametrans");
	for (i = nametransbase; i < numnametrans; i++)
		if (nametranstable[i].old == tptr) {
			DEBUG_MSG (("lookupnametrans: (0x%x to 0x%x) ", tptr, nametranstable[i].new));
			return nametranstable[i].new;
		}
	return NULL;
}

/*}}}*/
/*{{{  PRIVATEPARAM int mod_name(treenode **nptr, void *const voidptr) */
PRIVATEPARAM int mod_name (treenode ** nptr, void *voidptr)
{
	USE_VAR (voidptr);	/* stop unused var warning */
	/*printf("mod_name: name is %s\n", WNameOf(NNameOf(*nptr))); */

	makecopy (nptr);

	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PRIVATE treenode *copyitem(t) */
PRIVATE treenode *copyitem (treenode * t)
{
	treenode *newnode = NULL;
	if (t != NULL) {
		const int n = nodesize (TagOf (t));
		newnode = (treenode *) newvec (n);
		memcpy (newnode, t, n);
		if (parentlocn != NOPOSN)
			SetLocn (newnode, parentlocn);
		if (nodetypeoftag (TagOf (t)) == NAMENODE)
			addnametrans (t, newnode);
	}
	return newnode;
}

/*}}}*/
/*{{{  PRIVATE void makecopy(tptr) */
/*****************************************************************************
 *
 *  makecopy takes a tree and produces a new copy of it
 *
 *****************************************************************************/
PRIVATE void makecopy (treenode ** tptr)
{
	while (*tptr != NULL) {
		treenode *t = *tptr;
		SOURCEPOSN newlocn;
		copylocn = LocnOf (t);
		newlocn = (parentlocn == NOPOSN) ? copylocn : parentlocn;
		switch (nodetypeoftag (TagOf (t))) {
			/*{{{  NAME: do not copy; return */
		case WORDNODE:
			return;
			/*}}} */
			/*{{{  namenode: look for copy; return */
		case NAMENODE:
			/* This added 19/6/90 by CO'N for expanding known replicators */
			if ((TagOf (t) == N_REPL) && (NReplKnownOf (t))) {
				*tptr = newconstant (NReplValueOf (t));
			} else {
				t = lookupnametrans (t);
				if (t != NULL)
					*tptr = t;
			}
			return;
			/*}}} */
		default:
			/*{{{  copy children */
			{
				t = copyitem (t);
				*tptr = t;
				switch (nodetypeoftag (TagOf (t))) {
					/*{{{  cases */
				default:
					badtag (copylocn, TagOf (t), "makecopy");
					/*{{{  action */
				case ACTIONNODE:
					makecopy (LHSAddr (t));
					makecopy (RHSAddr (t));
					makecopy (ActionDuringAddr (t));
					makecopy (ActionAfterAddr (t));
					return;
					/*}}} */
					/*{{{  alt */
				case ALTNODE:
					makecopy (AltGuardAddr (t));
					makecopy (AltInputAddr (t));
					tptr = AltBodyAddr (t);
					break;
					/*}}} */
					/*{{{  arraysub */
				case ARRAYSUBNODE:
					makecopy (ASBaseAddr (t));
					makecopy (ASIndexAddr (t));
					makecopy (ASExpAddr (t));
					tptr = ASLengthAddr (t);
					break;
					/*}}} */
					/*{{{  cnode */
				case CNODE:
					tptr = CBodyAddr (t);
					break;
					/*}}} */
					/*{{{  cond */
				case CONDNODE:
					makecopy (CondGuardAddr (t));
					makecopy (CondAfterAddr (t));
					tptr = CondBodyAddr (t);
					break;
					/*}}} */
					/*{{{  condexp */
#ifdef CONDEXP
				case CONDEXPNODE:
					makecopy (CondExpGuardAddr (t));
					makecopy (CondExpTrueAddr (t));
					tptr = CondExpFalseAddr (t);
					break;
#endif
					/*}}} */
					/*{{{  constexp */
				case CONSTEXPNODE:
					tptr = CExpAddr (t);
					break;
					/*}}} */
					/*{{{  consttable */
				case CONSTTABLENODE:
					tptr = CTExpAddr (t);
					break;
					/*}}} */
					/*{{{  decl */
				case DECLNODE:
					switch (TagOf (t)) {
					default:
						badtag (copylocn, TagOf (t), "makecopy-decl");
						/*{{{  S_DECL */
					case S_DECL:
						{
							treenode *name = DNameOf (t);
							if (TagOf (name) == S_LIST) {
								/* They all share the same type tree so do this once */
								treenode *typeptr = NTypeOf (ThisItem (name)), **lastitem = DNameAddr (t);
								makecopy (&typeptr);
								copylocn = LocnOf (t);
								newlocn = (parentlocn == NOPOSN) ? copylocn : parentlocn;
								while (!EndOfList (name)) {
									treenode *item = copyitem (ThisItem (name));
									treenode *l = newlistnode (S_LIST, newlocn, item, NULL);
									SetNDecl (item, t);
									SetNType (item, typeptr);
									SetNLexLevel (item, copy_lexlevel);
									*lastitem = l;
									lastitem = NextItemAddr (l);
									name = NextItem (name);
								}
							} else {
								treenode *nptr = copyitem (name);
								makecopy (DValAddr (t));
								SetDName (t, nptr);
								SetNDecl (nptr, t);
								SetNLexLevel (nptr, copy_lexlevel);
								makecopy (NTypeAddr (nptr));
							}
							tptr = DBodyAddr (t);
						}
						break;
						/*}}} */
						/*{{{  procedure or function */
					case S_PROCDEF:
#ifdef MOBILES
					case S_MPROCDECL:
#endif
					case S_SFUNCDEF:
					case S_LFUNCDEF:
						CASE_CONFIG_SPEC {
							treenode *name = DNameOf (t);
							treenode *nptr = copyitem (name);
							treenode *dummy = NULL;
							treenode **fparams = &dummy;
							SetDName (t, nptr);
							SetNDecl (nptr, t);
							SetNLexLevel (nptr, copy_lexlevel);
							copy_lexlevel++;
							switch (TagOf (t)) {
							case S_PROCDEF:
#ifdef MOBILES
							case S_MPROCDECL:
#endif
/*makecopy(NTypeAddr(nptr)); *//* Parameter list */
								fparams = NTypeAddr (nptr);
								break;
							case S_SFUNCDEF:
							case S_LFUNCDEF:
								{
									treenode *fntype = copyitem (NTypeOf (name));
									SetNType (nptr, fntype);
									makecopy (FnTypeListAddr (fntype));
									/*makecopy(FnParamsAddr(fntype)); */
									fparams = FnParamsAddr (fntype);
								}
								break;
							default:
								break;
							}
							{	/* copy formal parameter list */
								treenode *params = *fparams;
								while (!EndOfList (params)) {
									treenode *item = copyitem (ThisItem (params));
									treenode *l = newlistnode (S_LIST, newlocn, item, NULL);
									if (nodetypeoftag (TagOf (item)) == NAMENODE) {
										makecopy (NTypeAddr (item));
										SetNLexLevel (item, copy_lexlevel);
									}
									/* else it might be VSP etc */
									*fparams = l;
									fparams = NextItemAddr (l);
									params = NextItem (params);
								}
							}

							/* bug TS/1780 24/07/92 */
							use_copy_free_vars (name, nptr);
							fe_walk_free_vars (current_fe_handle, nptr, mod_name, NULL);

							makecopy (DValAddr (t));	/* Copy body of PROC/FUNCTION */
							copy_lexlevel--;
							makecopy (DBodyAddr (t));
						}
						return;
						/*}}} */
						/*{{{  others */
					case S_ABBR:
					case S_VALABBR:
					case S_RETYPE:
					case S_VALRETYPE:
					case S_TPROTDEF:
					case S_SPROTDEF:
/*case S_LABELDEF: *//* removed for bug 1112 16/1/91 */
					case S_PRAGMA:	/* bug 829 20/9/91 */
					case S_TYPEDECL:
#if MOBILES
					case S_FORWDECL:
					case S_PROCTYPEDECL:
#endif
						{
							treenode *nptr = copyitem (DNameOf (t));
							makecopy (DValAddr (t));
							SetDName (t, nptr);
							SetNDecl (nptr, t);
							SetNLexLevel (nptr, copy_lexlevel);
							makecopy (NTypeAddr (nptr));
							tptr = DBodyAddr (t);
						}
						break;
						/*}}} */
						/*{{{  labels */
					case S_LABELDEF:	/* added for bug 1112 16/1/91 */
						{
							makecopy (DNameAddr (t));	/* pick up the new name */
							makecopy (DValAddr (t));	/* always NULL for S_LABELDEF? */
							SetNDecl (DNameOf (t), t);
							SetNLexLevel (DNameOf (t), copy_lexlevel);
							makecopy (NTypeAddr (DNameOf (t)));
							tptr = DBodyAddr (t);	/* body will always be NULL */
						}
						break;
						/*}}} */
						/*{{{  PLACE */
					case S_PLACE:
					case S_WSPLACE:
					case S_VSPLACE:
#if 1				/*def CONFIG */
					case S_PLACEON:
#endif
						makecopy (DNameAddr (t));
						makecopy (DValAddr (t));
						tptr = DBodyAddr (t);
						break;
						/*}}} */
					}
					break;
					/*}}} */
					/*{{{  dopnode */
				case DOPNODE:
					makecopy (LeftOpAddr (t));
					tptr = RightOpAddr (t);
					break;
					/*}}} */
					/*{{{  instance */
				case INSTANCENODE:
					makecopy (INameAddr (t));
					tptr = IParamListAddr (t);
					break;
					/*}}} */
					/*{{{  litnode */
				case LITNODE:
					makecopy (LitExpAddr (t));
					tptr = LitTypeAddr (t);
					break;
					/*}}} */
					/*{{{  leaf */
				case LEAFNODE:
					tptr = LeafLinkAddr (t);
					break;
					/*}}} */
					/*{{{  list */
				case LISTNODE:
					makecopy (ThisItemAddr (t));
					tptr = NextItemAddr (t);
					t = *tptr;
					while (!EndOfList (t)) {
						t = newlistnode (S_LIST, newlocn, ThisItem (t), NextItem (t));
						*tptr = t;
						makecopy (ThisItemAddr (t));
						tptr = NextItemAddr (t);
						t = *tptr;
					}
					return;
					/*}}} */
					/*{{{  mop */
				case MOPNODE:
					tptr = OpAddr (t);
					break;
					/*}}} */
					/*{{{  processor */
				case PROCESSORNODE:
					makecopy (ProcessorExpAddr (t));
					copy_lexlevel++;
					makecopy (ProcessorBodyAddr (t));
					copy_lexlevel--;
					return;
					/*}}} */
					/*{{{  replc */
				case REPLCNODE:
					{
						treenode *name = copyitem (ReplCNameOf (t));
						SetReplCName (t, name);
						SetNDecl (name, t);
						SetNLexLevel (name, copy_lexlevel);
						makecopy (ReplCStartExpAddr (t));
						makecopy (ReplCLengthExpAddr (t));
						if (ReplCStepExpOf (t)) {
							makecopy (ReplCStepExpAddr (t));
						}
						makecopy (NTypeAddr (name));	/* bug TS/1825 18/08/92 */
						makecopy (ReplCTempAddr (t));
						if (parrepl (TagOf (t))) {
							copy_lexlevel++;
							makecopy (ReplCBodyAddr (t));
							copy_lexlevel--;
							return;
						}
						tptr = ReplCBodyAddr (t);
					}
					break;
					/*}}} */
					/*{{{  segment */
				case SEGMENTNODE:
					makecopy (SNameAddr (t));
					makecopy (SStartExpAddr (t));
					tptr = SLengthExpAddr (t);
					break;
					/*}}} */
					/*{{{  type */
				case TYPENODE:
					makecopy (ARDimLengthAddr (t));
					tptr = ARTypeAddr (t);
					break;
					/*}}} */
					/*{{{  valof */
				case VALOFNODE:
					makecopy (VLBodyAddr (t));
					tptr = VLResultListAddr (t);
					break;
					/*}}} */
					/*{{{  confignode */
#if 1				/*def CONFIG */
				case CONFIGNODE:
					makecopy (STDevAddr (t));
					makecopy (STAttrNameAddr (t));
					tptr = STAttrExpAddr (t);
					break;
#endif
					/*}}} */
					/*}}} */
				}
			}
			break;
			/*}}} */
		}
	}
}

/*}}}*/

/*{{{  PUBLIC void marknametrans() */
/* save current nametransbase on stack */
PUBLIC void marknametrans (void)
{
	if (nametransstackptr >= sizenodestack) {
		int *newtable;
		sizenodestack *= 2;	/* increase the array size */
		/* this is a realloc */
		newtable = memalloc (sizenodestack * sizeof (int));
		memcpy (newtable, nametransstack, nametransstackptr * sizeof (int));
		memfree (nametransstack);
		nametransstack = newtable;
	}

	/*if (nametransstackptr < sizenodestack) */
	{
		nametransstack[nametransstackptr++] = nametransbase;
		nametransbase = numnametrans;
	}
	/*else
	   vtiabort(VTI_NAMETRANS_OVERFLOW, 0); */
}

/*}}}*/
/*{{{  PUBLIC void freenametrans() */
/* restore nametransbase from stack */
PUBLIC void freenametrans (void)
{
	if (nametransstackptr > 0) {
		numnametrans = nametransbase;
		nametransbase = nametransstack[--nametransstackptr];
	} else
		vtiabort (VTI_NAMETRANS_UNDERFLOW, 0);
}

/*}}}*/
/*{{{  PUBLIC void addnametrans(o, n) */
/* Add a name translation to the table of name translations */
PUBLIC void addnametrans (treenode * old, treenode * new)
{
	if (old == NULL || new == NULL)
		err_abort ("addnametrans");

	if (numnametrans >= sizenodecopy) {
		nametrans_t *newtable;
		sizenodecopy *= 2;	/* increase the array size */
		/* this is a realloc */
		newtable = memalloc (sizenodecopy * sizeof (nametrans_t));
		memcpy (newtable, nametranstable, numnametrans * sizeof (nametrans_t));
		memfree (nametranstable);
		nametranstable = newtable;
	}

	/*if (numnametrans < sizenodecopy) */
	{
		DEBUG_MSG (("addnametrans: (0x%x to 0x%x) ", old, new));
		nametranstable[numnametrans].old = old;
		nametranstable[numnametrans].new = new;
		numnametrans++;
	}
	/*else
	   vtiabort(VTI_TOO_MANY_NAMETRANS, 0); */
}

/*}}}*/
/*{{{  PRIVATEPARAM int stacklabeldefs */
PRIVATEPARAM int stacklabeldefs (treenode * tptr, void *dummy)
/* This is used to scope ASM labels correctly when copying a tree */
{
	dummy = dummy;
	if (TagOf (tptr) == S_LABELDEF) {
		DEBUG_MSG (("stacklabeldefs: found label name (0x%x)\n", DNameOf (tptr)));
		/* only create a new name if the user hasn't created a new one already */
		if (lookupnametrans (DNameOf (tptr)) == NULL) {
			DEBUG_MSG (("stacklabeldefs: copying label name (0x%x)\n", DNameOf (tptr)));
			/* This adds the new name to the translation stack too */
			(void) copyitem (DNameOf (tptr));
		}
		return STOP_WALK;
	}
	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PUBLIC treenode *transcopytree(tptr) */
/* Requires call to initnametrans to be done by the caller */
PUBLIC treenode *transcopytree (treenode * tptr, const SOURCEPOSN locn, const int init_lexlevel)
{
	/* If locn is NOPOSN, will copy all location information from the old tree,
	   otherwise it marks all nodes with that locn.
	   CON 9/10/90
	 */

	treenode *newtree = tptr;
	const SOURCEPOSN saved_parentlocn = parentlocn;
	copy_lexlevel = init_lexlevel;
	parentlocn = locn;
	/* This test modified so that no need to import 'guyinserts' */
	if (TRUE) {		/*(guyinserts != 0) */
		/* added for bug 1112 16/1/91 */
		DEBUG_MSG (("transcopytree: looking for labeldefs\n"));
		prewalktree (newtree, stacklabeldefs, NULL);	/* scope ASM labels correctly */
	}
	makecopy (&newtree);
	parentlocn = saved_parentlocn;
	return newtree;
}

/*}}}*/
/*{{{  PUBLIC treenode *copytree(tptr) */
/* Does NOT require call to initnametrans to be done by the caller
 * we save any current translations so we may call copytree while
 * a list of translations is being built up
 */
PUBLIC treenode *copytree (treenode * tptr, const int init_lexlevel)
{
	treenode *newtree;
	marknametrans ();
	newtree = transcopytree (tptr, NOPOSN, init_lexlevel);
	freenametrans ();
	return newtree;
}

/*}}}*/

/*{{{  PUBLIC treenode *expand_inline_instance */
PUBLIC treenode *expand_inline_instance (treenode * const t, const SOURCEPOSN locn, const int lexlevel)
{
	/* If locn is NOPOSN, will copy all location information from the old tree,
	   otherwise it marks all nodes with that locn.
	   CON 9/10/90
	 */
	treenode *const name = INameOf (t);
	treenode *inlinetree = NULL;
	treenode **inlinetreeptr = &inlinetree;
	treenode *fparams;
	treenode *aparams;
	marknametrans ();
	for (fparams = NParamListOf (name), aparams = IParamListOf (t);
	     !EndOfList (fparams); fparams = NextItem (fparams), aparams = NextItem (aparams))
		/*{{{  make up an abbreviation for the formal parameter */
	{
		treenode *const aparam = ThisItem (aparams);
		treenode *const fparam = ThisItem (fparams);

		if (NVAssumeConstOf (fparam) && !isconst (aparam))
			msg_out_s (SEV_ERR_JMP, VTI, VTI_EXPAND_NOT_CONSTANT, locn, WNameOf (NNameOf (fparam)));	/* bug TS/2015 08/01/93 */

		if (		/*TagOf(aparam) == S_CONSTEXP || *//* removed - bug 1307 20/6/91 */
			   issimple (aparam))
			addnametrans (fparam, aparam);
		else {
			/* FIXME: need to handle N_RESULTPARAM in here */
			const int abbrtag = (TagOf (fparam) == N_PARAM) ? S_ABBR : S_VALABBR;
			const int nametag = (abbrtag == S_ABBR) ? N_ABBR : N_VALABBR;
			treenode *nptr = newnamenode (nametag, LocnOf (t), NNameOf (fparam),
						      copytree (NTypeOf (fparam), lexlevel),
						      NULL, lexlevel, 0, NM_DEFAULT);
			treenode *fabbrev = newdeclnode (abbrtag, LocnOf (t), nptr, aparam, NULL);
			SetNDecl (nptr, fabbrev);
			addnametrans (fparam, DNameOf (fabbrev));
			*inlinetreeptr = fabbrev;
			inlinetreeptr = DBodyAddr (fabbrev);
		}
	}
	/*}}} */
	/* we pass in the location of the instance, which makes
	   the whole copied tree look like it all appeared
	   on the same line of source as the instance.
	   CON 9/10/90 */
	*inlinetreeptr = transcopytree (DValOf (NDeclOf (name)), locn, lexlevel);
	freenametrans ();

	/*debug
	   fprintf(outfile, "Inline routine instance expanded to :\n");
	   printtree(0, inlinetree);
	 */

	return inlinetree;
}

/*}}}*/

/*{{{  PUBLIC void initcopytree() */
PUBLIC void initcopytree (void)
{
	/*printf("in initcopytree\n"); */
	nametransstackptr = 0;
	nametransbase = 0;
	numnametrans = 0;
	if (nametransstack == NULL)
		nametransstack = memalloc (sizenodestack * sizeof (int));
	if (nametranstable == NULL)
		nametranstable = memalloc (sizenodecopy * sizeof (nametrans_t));
}

/*}}}*/
/*}}}*/
/*{{{  PUBLIC void replace_all_names */
typedef struct {
	treenode *old;
	treenode *new;
} replace_t;
/*{{{  PRIVATEPARAM int do_replace_all_names */
PRIVATEPARAM int do_replace_all_names (treenode ** const tptr, void *const voidptr)
{
	if (*tptr == ((replace_t *) voidptr)->old) {
		*tptr = ((replace_t *) voidptr)->new;
	} else {
		switch (TagOf (*tptr)) {
		case S_PROCDEF:
#ifdef MOBILES
		case S_MPROCDECL:
#endif
		case S_LFUNCDEF:
		case S_SFUNCDEF:
			use_walk_free_vars (DNameOf (*tptr), do_replace_all_names, voidptr);
			break;
		default:
			break;
		}
	}
	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PUBLIC void replace_all_names */
PUBLIC void replace_all_names (treenode ** const tptr, treenode * const old, treenode * const new)
{
	replace_t control_data;
	control_data.old = old;
	control_data.new = new;

	modprewalktree (tptr, do_replace_all_names, &control_data);
}

/*}}}*/
/*}}}*/

/*{{{  tree walking */
/* None of these routines walk into the types in namenodes and
 * the expressions of temps and folded constants
 * Or the names of a declaration.
 */

/*{{{  PUBLIC void applytoexp(tptr, f1) */
/*****************************************************************************
 *
 *  applytoexp   applies the function f1 to names & expressions or
 *               lists of expressions in the node tptr.
 *
 *****************************************************************************/

PUBLIC void applytoexp (treenode * tptr, void (*f1) (treenode *, void *), void *const voidptr)
{
	if (tptr != NULL)
		switch (TagOf (tptr))
			/*{{{  cases */
		{
		default:
			break;
			/*{{{  decl */
		case S_DECL:
		case S_ABBR:
		case S_VALABBR:
		case S_RETYPE:
		case S_VALRETYPE:
			if (DValOf (tptr) != NULL)
				(*f1) (DValOf (tptr), voidptr);
			break;
			/*}}} */
			/*{{{  REPLSEQ REPLIF REPLALT REPLPAR */
		case S_REPLPAR:
		case S_REPLSEQ:
		case S_REPLIF:
		case S_REPLALT:
		case S_PRIREPLPAR:
		case S_PRIREPLALT:
		case S_PLACEDREPLPAR:
		case S_REPLDO:
			(*f1) (ReplCStartExpOf (tptr), voidptr);
			(*f1) (ReplCLengthExpOf (tptr), voidptr);
			if (ReplCStepExpOf (tptr)) {
				(*f1) (ReplCStepExpOf (tptr), voidptr);
			}
			break;
			/*}}} */
			/*{{{  WHILE CHOICE */
		case S_WHILE:
		case S_CHOICE:
			(*f1) (CondGuardOf (tptr), voidptr);
			break;
			/*}}} */
#ifdef CONDEXP
			/*{{{  CONDEXP */
		case S_CONDEXP:
			(*f1) (CondExpGuardOf (tptr), voidptr);
			(*f1) (CondExpTrueOf (tptr), voidptr);
			(*f1) (CondExpFalseOf (tptr), voidptr);
			break;
			/*}}} */
#endif
			/*{{{  ALTERNATIVE */
		case S_ALTERNATIVE:
			if (AltGuardOf (tptr) != NULL)
				(*f1) (AltGuardOf (tptr), voidptr);
			break;
			/*}}} */
			/*{{{  configuration */
			/*{{{  PLACE */
		case S_PLACE:
			(*f1) (DValOf (tptr), voidptr);
			break;
			/*}}} */
			/*{{{  PROCESSOR */
		case S_PROCESSOR:
			(*f1) (ProcessorExpOf (tptr), voidptr);
			break;
			/*}}} */
			/*}}} */
			/*{{{  PINSTANCE FINSTANCE */
		case S_PINSTANCE:
		case S_FINSTANCE:
			walklist (f1, IParamListOf (tptr), voidptr);
			walklist (f1, IDynaddrOf (tptr), voidptr);
			return;
			/*}}} */
			/*{{{  ASS */
		case S_ASS:
			if (TagOf (LHSOf (tptr)) == S_LIST)
				walklist (f1, LHSOf (tptr), voidptr);
			else
				(*f1) (LHSOf (tptr), voidptr);
			if (TagOf (RHSOf (tptr)) == S_LIST)
				walklist (f1, RHSOf (tptr), voidptr);
			else
				(*f1) (RHSOf (tptr), voidptr);
			break;
			/*}}} */
			/*{{{  OUTPUT INPUT TAGGED_INPUT DELAYED_INPUT X_INPUT X_TAGGED_INPUT */
		case S_OUTPUT:
		case S_INPUT:
		case S_TAGGED_INPUT:
		case S_DELAYED_INPUT:
		case S_X_INPUT:
		case S_X_TAGGED_INPUT:
			(*f1) (LHSOf (tptr), voidptr);
			if (TagOf (RHSOf (tptr)) == S_LIST) {
				walklist (f1, RHSOf (tptr), voidptr);
			} else {
				(*f1) (RHSOf (tptr), voidptr);
			}
			break;
			/*}}} */
			/*{{{  CASE CASE_INPUT */
		case S_CASE:
		case S_CASE_INPUT:
		case S_X_CASE_INPUT:
			(*f1) (LHSOf (tptr), voidptr);
			return;
			/*}}} */
			/*{{{  VARIANT */
		case S_VARIANT:
		case S_X_VARIANT:
			walklist (f1, VRTaggedListOf (tptr), voidptr);
			break;
			/*}}} */
			/*{{{  monadic */
		case S_NEG:
		case S_BITNOT:
		case S_NOT:
		case S_SIZE:
		case S_EXACT:
		case S_UMINUS:
		case S_UPLUS:
		case S_ROUND:
		case S_TRUNC:
		case S_ADDRESSOF:
#ifdef OCCAM2_5
		case S_BYTESIN:
#endif
		case S_TYPEHASHOF:
#ifdef MOBILES
		case S_ADDROF:
		case S_HWADDROF:
#endif
			(*f1) (OpOf (tptr), voidptr);
			break;
			/*}}} */
			/*{{{  dyadic */
		case S_AND:
		case S_OR:
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
		case S_EQ:
		case S_NE:
		case S_LS:
		case S_LE:
		case S_GR:
		case S_GE:
		case S_AFTER:
		case S_COLON2:
		case S_CSUB0:
		case S_CCNT1:
		case S_GUYCODE:
		case S_GUYSTEP:
		case S_OVERLAPCHECK:
		case S_FOR:
			/*case S_INTERVAL: */
#ifdef OCCAM2_5
		case S_OFFSETOF:
#endif
			(*f1) (LeftOpOf (tptr), voidptr);
			(*f1) (RightOpOf (tptr), voidptr);
			break;
			/*}}} */
			/*{{{  constructor */
		case S_CONSTRUCTOR:
			/* case S_STRUCTCONSTRUCTOR : */
			walklist (f1, LitExpOf (tptr), voidptr);
			break;
			/*}}} */
			/*{{{  arrayconstructor */
		case S_ARRAYCONSTRUCTOR:
			(*f1) (ReplCStartExpOf (tptr), voidptr);
			(*f1) (ReplCLengthExpOf (tptr), voidptr);
			if (ReplCStepExpOf (tptr)) {
				(*f1) (ReplCStepExpOf (tptr), voidptr);
			}
			(*f1) (ReplCBodyOf (tptr), voidptr);
			break;
			/*}}} */
			/*{{{  valof */
		case S_VALOF:
			walklist (f1, VLResultListOf (tptr), voidptr);
			break;
			/*}}} */
			/*{{{  subscript */
		case S_ARRAY:
			(*f1) (ARDimLengthOf (tptr), voidptr);
			break;

		case S_ARRAYSUB:
		case S_RECORDSUB:
			(*f1) (ASBaseOf (tptr), voidptr);
			(*f1) (ASIndexOf (tptr), voidptr);
			break;

		case S_ARRAYITEM:
		case S_RECORDITEM:
			(*f1) (ASExpOf (tptr), voidptr);
			break;
			/*}}} */
			/*{{{  segment */
		case S_SEGMENT:
		case S_SEGMENTITEM:
			(*f1) (SStartExpOf (tptr), voidptr);
			(*f1) (SLengthExpOf (tptr), voidptr);
			if (SSubscriptExpOf (tptr) != NULL)
				(*f1) (SSubscriptExpOf (tptr), voidptr);
			(*f1) (SCheckExpOf (tptr), voidptr);
			break;
			/*}}} */
#if 1				/*def CONFIG */
			/*{{{  confignode */
		case S_SET:
		case S_CONNECT:
		case S_MAP:
			(*f1) (STDevOf (tptr), voidptr);
			if (STAttrNameOf (tptr) != NULL)
				(*f1) (STAttrNameOf (tptr), voidptr);
			if (STAttrExpOf (tptr) != NULL)
				(*f1) (STAttrExpOf (tptr), voidptr);
			break;
			/*}}} */
#endif
		}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void prewalktree(tptr, f1) */
/*****************************************************************************
 *
 *  prewalktree does a walk of the tree 'tptr' applying f1() to each node of
 *  the tree before traversing its children.
 *           if f1 returns FALSE
 *             prewalk_tree will walk down the children this node
 *           else
 *             prewalk_tree simply returns
 *
 *
 *****************************************************************************/

PUBLIC void prewalktree (treenode *tptr, int (*f1)(treenode *, void *), void *const voidptr)
{
	while (tptr != NULL) {
		if ((*f1) (tptr, voidptr) != CONTINUE_WALK) {
			return;
		}
		switch (TagOf (tptr))
			/*{{{  cases */
		{
		default:
			return;
			/*{{{  decl */
		case S_DECL:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_ABBR:
		case S_VALABBR:
		case S_RETYPE:
		case S_VALRETYPE:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_LABELDEF:
		case S_PLACE:
		case S_WSPLACE:
		case S_VSPLACE:
#if 1				/*def CONFIG */
		case S_PLACEON:
			CASE_CONFIG_SPEC
#endif
		case S_PRAGMA:	/* bug 829 20/9/91 */
		case S_TYPEDECL:
#if MOBILES
		case S_FORWDECL:
		case S_PROCTYPEDECL:
		case S_MPROCDECL:
#endif
			prewalktree (DValOf (tptr), f1, voidptr);
			tptr = DBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  SEQ IF ALT PAR GUY */
		case S_PAR:
		case S_SEQ:
		case S_IF:
		case S_ALT:
		case S_PRIPAR:
		case S_PRIALT:
		case S_PLACEDPAR:
		case S_GUY:
		case S_ASM:
		case S_DO:
		case S_FORKING:
		case S_CLAIM:
			/*case S_BASICBLOCK: */
			tptr = CBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  REPLSEQ REPLIF REPLALT REPLPAR */
		case S_REPLPAR:
		case S_REPLSEQ:
		case S_REPLIF:
		case S_REPLALT:
		case S_PRIREPLPAR:
		case S_PRIREPLALT:
		case S_PLACEDREPLPAR:
		case S_REPLDO:
			prewalktree (ReplCStartExpOf (tptr), f1, voidptr);
			prewalktree (ReplCLengthExpOf (tptr), f1, voidptr);
			if (ReplCStepExpOf (tptr)) {
				prewalktree (ReplCStepExpOf (tptr), f1, voidptr);
			}
			tptr = ReplCBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  WHILE CHOICE */
		case S_WHILE:
		case S_CHOICE:
		case S_SELECTION:
			prewalktree (CondGuardOf (tptr), f1, voidptr);
			tptr = CondBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  CONDEXP */
#ifdef CONDEXP
		case S_CONDEXP:
			prewalktree (CondExpGuardOf (tptr), f1, voidptr);
			prewalktree (CondExpTrueOf (tptr), f1, voidptr);
			tptr = CondExpFalseOf (tptr);
			break;
#endif
			/*}}} */
			/*{{{  VARIANT X_VARIANT */
		case S_VARIANT:
			prewalktree (VRTaggedListOf (tptr), f1, voidptr);
			tptr = VRBodyOf (tptr);
			break;
		case S_X_VARIANT:
			prewalktree (VRTaggedListOf (tptr), f1, voidptr);
			prewalktree (VRDuringOf (tptr), f1, voidptr);
			tptr = VRAfterOf (tptr);
			break;
			/*}}} */
			/*{{{  ALTERNATIVE */
		case S_ALTERNATIVE:
			prewalktree (AltGuardOf (tptr), f1, voidptr);
			prewalktree (AltInputOf (tptr), f1, voidptr);
			tptr = AltBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  configuration */
#if 0				/* never used */
			/*{{{  PRI PLACED */
		case S_PRI:
		case S_PLACED:
			tptr = ConfigBodyOf (tptr);
			break;
			/*}}} */
#endif
			/*{{{  PROCESSOR */
		case S_PROCESSOR:
			prewalktree (ProcessorExpOf (tptr), f1, voidptr);
			tptr = ProcessorBodyOf (tptr);
			break;
			/*}}} */
			/*}}} */
			/*{{{  instance */
			/*{{{  PINSTANCE FINSTANCE */
		case S_PINSTANCE:
		case S_FINSTANCE:
			prewalktree (INameOf (tptr), f1, voidptr);
			prewalktree (IDynaddrOf (tptr), f1, voidptr);
			tptr = IParamListOf (tptr);
			break;
			/*}}} */
			/*}}} */
			/*{{{  action */
			/*{{{  ASS OUTPUT INPUT TAGGED_INPUT DELAYED_INPUT CASE CASEINPUT X_INPUT X_TAGGED_INPUT X_CASE_INPUT */
		case S_ASS:
		case S_OUTPUT:
		case S_INPUT:
		case S_TAGGED_INPUT:
		case S_DELAYED_INPUT:
		case S_CASE:
		case S_CASE_INPUT:
		case S_X_CASE_INPUT:
			prewalktree (LHSOf (tptr), f1, voidptr);
			tptr = RHSOf (tptr);
			break;
		case S_X_INPUT:
		case S_X_TAGGED_INPUT:
			prewalktree (LHSOf (tptr), f1, voidptr);
			prewalktree (RHSOf (tptr), f1, voidptr);
			prewalktree (ActionDuringOf (tptr), f1, voidptr);
			tptr = ActionAfterOf (tptr);
			break;
			/*}}} */
			/*}}} */
			/*{{{  monadic */
		case S_NEG:
		case S_BITNOT:
		case S_NOT:
		case S_SIZE:
		case S_EXACT:
		case S_ROUND:
		case S_TRUNC:
		case S_ADDRESSOF:
		case S_MOSTPOS:
		case S_MOSTNEG:
		case S_UMINUS:
		case S_UPLUS:
#ifdef OCCAM2_5
		case S_BYTESIN:
#endif
		case S_TYPEHASHOF:
			tptr = OpOf (tptr);
			break;
#ifdef MOBILES
		case S_CLONE:
		case S_ADDROF:
		case S_HWADDROF:
			tptr = OpOf (tptr);
			break;
		case S_NEW_ARRAY:
			if (TypeAttrOf (tptr) & TypeAttr_aligned) {
				prewalktree (ARAlignmentOf (tptr), f1, voidptr);
			}
			tptr = ARDimLengthOf (tptr);
			break;
#endif
			/*}}} */
			/*{{{  dyadic */
		case S_AND:
		case S_OR:
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
		case S_EQ:
		case S_NE:
		case S_LS:
		case S_LE:
		case S_GR:
		case S_GE:
		case S_AFTER:
		case S_COLON2:
		case S_CSUB0:
		case S_CCNT1:
		case S_GUYCODE:
		case S_GUYSTEP:
			/*case S_INTERVAL: */
		case S_OVERLAPCHECK:
		case S_FOR:
		case S_EVAL:
#ifdef OCCAM2_5
		case S_OFFSETOF:
#endif
			prewalktree (LeftOpOf (tptr), f1, voidptr);
			tptr = RightOpOf (tptr);
			break;
			/*}}} */
			/*{{{  constructor */
		case S_CONSTRUCTOR:
			/* case S_STRUCTCONSTRUCTOR: */
			tptr = LitExpOf (tptr);
			break;
			/*}}} */
			/*{{{  arrayconstructor */
		case S_ARRAYCONSTRUCTOR:
			prewalktree (ReplCStartExpOf (tptr), f1, voidptr);
			prewalktree (ReplCLengthExpOf (tptr), f1, voidptr);
			if (ReplCStepExpOf (tptr)) {
				prewalktree (ReplCStepExpOf (tptr), f1, voidptr);
			}
			tptr = ReplCBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  valof */
		case S_VALOF:
			prewalktree (VLBodyOf (tptr), f1, voidptr);
			tptr = VLResultListOf (tptr);
			break;
			/*}}} */
			/*{{{  subscript */
		case S_ARRAY:
			prewalktree (ARDimLengthOf (tptr), f1, voidptr);
			tptr = ARTypeOf (tptr);
			break;

		case S_ARRAYSUB:
		case S_RECORDSUB:
			prewalktree (ASBaseOf (tptr), f1, voidptr);
			tptr = ASIndexOf (tptr);
			break;

		case S_ARRAYITEM:
		case S_RECORDITEM:
			prewalktree (ASBaseOf (tptr), f1, voidptr);
			tptr = ASExpOf (tptr);
			break;
			/*}}} */
			/*{{{  segment */
		case S_SEGMENT:
		case S_SEGMENTITEM:
			prewalktree (SNameOf (tptr), f1, voidptr);
			prewalktree (SStartExpOf (tptr), f1, voidptr);
			prewalktree (SLengthExpOf (tptr), f1, voidptr);
			prewalktree (SSubscriptExpOf (tptr), f1, voidptr);
			tptr = SCheckExpOf (tptr);
			break;
			/*}}} */
			/*{{{  space usage node */
		case S_SPACEUSAGE:
			tptr = SpBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  hidden parameter node */
		case S_HIDDEN_PARAM:
		case S_FNFORMALRESULT:
		case S_FNACTUALRESULT:
		case S_HIDDEN_TYPE:
			tptr = HExpOf (tptr);
			break;
			/*}}} */
			/*{{{  S_LIST */
		case S_LIST:
			while (!EndOfList (tptr)) {
				prewalktree (ThisItem (tptr), f1, voidptr);
				tptr = NextItem (tptr);
			}
			return;
			/*}}} */
			/*{{{  S_FNTYPE */
		case S_FNTYPE:
			prewalktree (LeftOf (tptr), f1, voidptr);
			tptr = RightOf (tptr);
			break;
			/*}}} */
			/*{{{  confignode */
		case S_CONNECT:
		case S_SET:
		case S_MAP:
			prewalktree (ConnectFromEdgeOf (tptr), f1, voidptr);
			prewalktree (ConnectToEdgeOf (tptr), f1, voidptr);
			tptr = ConnectArcOf (tptr);
			break;
			/*}}} */
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void prewalkproctree(tptr, f1) */
/*****************************************************************************
 *
 *  prewalkproctree does a walk of the tree 'tptr' applying f1() to each node
 *  of the tree before traversing its children.
 *  It does not enter nested PROCs etc.
 *  It does not traverse expressions !.
 *           if f1 returns CONTINUE_WALK
 *             prewalk_tree will walk down the children this node
 *           else
 *             prewalk_tree simply returns
 *
 *
 *****************************************************************************/

PUBLIC void prewalkproctree (treenode * tptr, int (*f1) (treenode *, void *), void *const voidptr)
{
	while (tptr != NULL) {
		if ((*f1) (tptr, voidptr) != CONTINUE_WALK)
			return;
		switch (TagOf (tptr))
			/*{{{  cases */
		{
		default:
			return;
			/*{{{  decl */
		case S_DECL:
		case S_ABBR:
		case S_VALABBR:
		case S_RETYPE:
		case S_VALRETYPE:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_LABELDEF:
		case S_PLACE:
		case S_WSPLACE:
		case S_VSPLACE:
#if 1				/*def CONFIG */
		case S_PLACEON:
			CASE_CONFIG_SPEC
#endif
		case S_PRAGMA:	/* bug 829 20/9/91 */
		case S_TYPEDECL:
#if MOBILES
		case S_FORWDECL:
		case S_PROCTYPEDECL:
		case S_MPROCDECL:
#endif
			tptr = DBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  SEQ IF ALT PAR */
		case S_PAR:
		case S_SEQ:
		case S_IF:
		case S_ALT:
		case S_PRIPAR:
		case S_PRIALT:
		case S_PLACEDPAR:
		case S_GUY:
		case S_ASM:	/*case S_BASICBLOCK: */
		case S_DO:
		case S_FORKING:
		case S_CLAIM:
			tptr = CBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  REPLSEQ REPLIF REPLALT REPLPAR */
		case S_REPLPAR:
		case S_REPLSEQ:
		case S_REPLIF:
		case S_REPLALT:
		case S_PRIREPLPAR:
		case S_PRIREPLALT:
		case S_PLACEDREPLPAR:
		case S_REPLDO:
			tptr = ReplCBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  WHILE CHOICE */
		case S_WHILE:
		case S_CHOICE:
		case S_SELECTION:
			tptr = CondBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  VARIANT X_VARIANT */
		case S_VARIANT:
			tptr = VRBodyOf (tptr);
			break;
		case S_X_VARIANT:
			prewalkproctree (VRDuringOf (tptr), f1, voidptr);
			tptr = VRAfterOf (tptr);
			break;
			/*}}} */
			/*{{{  ALTERNATIVE */
		case S_ALTERNATIVE:
			prewalkproctree (AltInputOf (tptr), f1, voidptr);
			tptr = AltBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  configuration */
#if 0				/* never used */
			/*{{{  PRI PLACED */
		case S_PRI:
		case S_PLACED:
			tptr = ConfigBodyOf (tptr);
			break;
			/*}}} */
#endif
			/*{{{  PROCESSOR */
		case S_PROCESSOR:
			tptr = ProcessorBodyOf (tptr);
			break;
			/*}}} */
			/*}}} */
			/*{{{  PINSTANCE FINSTANCE */
		case S_PINSTANCE:
		case S_FINSTANCE:
			return;
			/*}}} */
			/*{{{  ASS OUTPUT INPUT TAGGED_INPUT DELAYED_INPUT */
		case S_ASS:
		case S_OUTPUT:
		case S_INPUT:
		case S_TAGGED_INPUT:
		case S_DELAYED_INPUT:
			return;
			/*}}}  */
			/*{{{  X_INPUT X_TAGGED_INPUT */
		case S_X_INPUT:
		case S_X_TAGGED_INPUT:
			prewalkproctree (ActionDuringOf (tptr), f1, voidptr);
			tptr = ActionAfterOf (tptr);
			break;
			/*}}} */
			/*{{{  CASE CASE_INPUT */
		case S_CASE:
		case S_CASE_INPUT:
		case S_X_CASE_INPUT:
			tptr = RHSOf (tptr);
			break;
			/*}}} */
			/*{{{  valof */
		case S_VALOF:
			tptr = VLBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  space usage node */
		case S_SPACEUSAGE:
			tptr = SpBodyOf (tptr);
			break;
			/*}}} */
			/*{{{  S_LIST */
		case S_LIST:
			while (!EndOfList (tptr)) {
				prewalkproctree (ThisItem (tptr), f1, voidptr);
				tptr = NextItem (tptr);
			}
			return;
			/*}}} */
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void modprewalktree(tptr, f1) */
/*****************************************************************************
 *
 *  modprewalktree does a walk of the tree 'tptr' applying f1() to each node of
 *  the tree before traversing its children.
 *
 *
 *****************************************************************************/

PUBLIC void modprewalktree (treenode ** tptr, int (*f1) (treenode **, void *), void *const voidptr)
{
	while (*tptr != NULL) {
		treenode *t;
		if ((*f1) (tptr, voidptr) != CONTINUE_WALK)
			return;
		t = *tptr;
		if (t != NULL)
			switch (TagOf (t))
				/*{{{  cases */
			{
			default:
				return;
				/*{{{  decl */
			case S_DECL:
			case S_PROCDEF:
			case S_SFUNCDEF:
			case S_LFUNCDEF:
			case S_ABBR:
			case S_VALABBR:
			case S_RETYPE:
			case S_VALRETYPE:
			case S_TPROTDEF:
			case S_SPROTDEF:
			case S_LABELDEF:
			case S_PLACE:
			case S_WSPLACE:
			case S_VSPLACE:
#if 1				/*def CONFIG */
			case S_PLACEON:
				CASE_CONFIG_SPEC
#endif
			case S_PRAGMA:	/* bug 829 20/9/91 */
			case S_TYPEDECL:
#if MOBILES
			case S_FORWDECL:
			case S_PROCTYPEDECL:
			case S_MPROCDECL:
#endif
				modprewalktree (DValAddr (t), f1, voidptr);
				tptr = DBodyAddr (t);
				break;
				/*}}} */
				/*{{{  SEQ IF ALT PAR */
			case S_SEQ:
			case S_PAR:
			case S_IF:
			case S_ALT:
			case S_PRIPAR:
			case S_PRIALT:
			case S_PLACEDPAR:
			case S_GUY:
			case S_ASM:	/*case S_BASICBLOCK: */
			case S_DO:
			case S_FORKING:
			case S_CLAIM:
				tptr = CBodyAddr (t);
				break;
				/*}}} */
				/*{{{  REPLSEQ REPLIF REPLALT REPLPAR */
			case S_REPLPAR:
			case S_REPLSEQ:
			case S_REPLIF:
			case S_REPLALT:
			case S_PRIREPLPAR:
			case S_PRIREPLALT:
			case S_PLACEDREPLPAR:
			case S_REPLDO:
				modprewalktree (ReplCStartExpAddr (t), f1, voidptr);
				modprewalktree (ReplCLengthExpAddr (t), f1, voidptr);
				if (ReplCStepExpOf (t)) {
					modprewalktree (ReplCStepExpAddr (t), f1, voidptr);
				}
				tptr = ReplCBodyAddr (t);
				break;
				/*}}} */
				/*{{{  WHILE CHOICE */
			case S_WHILE:
			case S_CHOICE:
			case S_SELECTION:
				modprewalktree (CondGuardAddr (t), f1, voidptr);
				tptr = CondBodyAddr (t);
				break;
				/*}}} */
				/*{{{  CONDEXP */
#ifdef CONDEXP
			case S_CONDEXP:
				modprewalktree (CondExpGuardAddr (t), f1, voidptr);
				modprewalktree (CondExpTrueAddr (t), f1, voidptr);
				tptr = CondExpFalseAddr (t);
				break;
#endif
				/*}}} */
				/*{{{  VARIANT X_VARIANT */
			case S_VARIANT:
				modprewalktree (VRTaggedListAddr (t), f1, voidptr);
				tptr = VRBodyAddr (t);
				break;
			case S_X_VARIANT:
				modprewalktree (VRTaggedListAddr (t), f1, voidptr);
				modprewalktree (VRDuringAddr (t), f1, voidptr);
				tptr = VRAfterAddr (t);
				break;
				/*}}} */
				/*{{{  ALTERNATIVE */
			case S_ALTERNATIVE:
				modprewalktree (AltGuardAddr (t), f1, voidptr);
				modprewalktree (AltInputAddr (t), f1, voidptr);
				tptr = AltBodyAddr (t);
				break;
				/*}}} */
				/*{{{  configuration */
#if 0				/* never used */
				/*{{{  PRI PLACED */
			case S_PRI:
			case S_PLACED:
				tptr = ConfigBodyAddr (t);
				break;
				/*}}} */
#endif
				/*{{{  PROCESSOR */
			case S_PROCESSOR:
				modprewalktree (ProcessorExpAddr (t), f1, voidptr);
				tptr = ProcessorBodyAddr (t);
				break;
				/*}}} */
				/*}}} */
				/*{{{  instance */
				/*{{{  PINSTANCE FINSTANCE */
			case S_PINSTANCE:
			case S_FINSTANCE:
				modprewalktree (INameAddr (t), f1, voidptr);
				modprewalktree (IDynaddrAddr (t), f1, voidptr);
				tptr = IParamListAddr (t);
				break;
				/*}}} */
				/*}}} */
				/*{{{  action */
				/*{{{  ASS OUTPUT INPUT TAGGED_INPUT DELAYED_INPUT CASE CASEINPUT X_CASE_INPUT */
			case S_ASS:
			case S_OUTPUT:
			case S_INPUT:
			case S_TAGGED_INPUT:
			case S_DELAYED_INPUT:
			case S_CASE:
			case S_CASE_INPUT:
			case S_X_CASE_INPUT:
				modprewalktree (LHSAddr (t), f1, voidptr);
				tptr = RHSAddr (t);
				break;
				/*}}} */
				/*{{{  X_INPUT X_TAGGED_INPUT */
			case S_X_INPUT:
			case S_X_TAGGED_INPUT:
				modprewalktree (LHSAddr (t), f1, voidptr);
				modprewalktree (RHSAddr (t), f1, voidptr);
				modprewalktree (ActionDuringAddr (t), f1, voidptr);
				tptr = ActionAfterAddr (t);
				break;
				/*}}}  */
				/*}}} */
				/*{{{  monadic */
			case S_NEG:
			case S_BITNOT:
			case S_NOT:
			case S_SIZE:
			case S_EXACT:
			case S_ROUND:
			case S_TRUNC:
			case S_ADDRESSOF:
			case S_MOSTPOS:
			case S_MOSTNEG:
			case S_UMINUS:
			case S_UPLUS:
#ifdef OCCAM2_5
			case S_BYTESIN:
#endif
				tptr = OpAddr (t);
				break;
				/*}}} */
				/*{{{  dyadic */
			case S_AND:
			case S_OR:
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
			case S_EQ:
			case S_NE:
			case S_LS:
			case S_LE:
			case S_GR:
			case S_GE:
			case S_AFTER:
			case S_COLON2:
			case S_CSUB0:
			case S_CCNT1:
			case S_GUYCODE:
			case S_GUYSTEP:
			case S_OVERLAPCHECK:
			case S_FOR:
			case S_EVAL:
				/*case S_INTERVAL : */
#ifdef OCCAM2_5
			case S_OFFSETOF:
#endif
				modprewalktree (LeftOpAddr (t), f1, voidptr);
				tptr = RightOpAddr (t);
				break;
				/*}}} */
				/*{{{  constructor */
			case S_CONSTRUCTOR:
				/* case S_STRUCTCONSTRUCTOR: */
				tptr = LitExpAddr (t);
				break;
				/*}}} */
				/*{{{  arrayconstructor */
			case S_ARRAYCONSTRUCTOR:
				modprewalktree (ReplCStartExpAddr (t), f1, voidptr);
				modprewalktree (ReplCLengthExpAddr (t), f1, voidptr);
				if (ReplCStepExpOf (t)) {
					modprewalktree (ReplCStepExpAddr (t), f1, voidptr);
				}
				tptr = ReplCBodyAddr (t);
				break;
				/*}}} */
				/*{{{  valof */
			case S_VALOF:
				modprewalktree (VLBodyAddr (t), f1, voidptr);
				tptr = VLResultListAddr (t);
				break;
				/*}}} */
				/*{{{  subscript */
			case S_ARRAY:
				modprewalktree (ARDimLengthAddr (t), f1, voidptr);
				tptr = ARTypeAddr (t);
				break;

			case S_ARRAYSUB:
			case S_RECORDSUB:
				modprewalktree (ASBaseAddr (t), f1, voidptr);
				tptr = ASIndexAddr (t);
				break;

			case S_ARRAYITEM:
			case S_RECORDITEM:
				modprewalktree (ASBaseAddr (t), f1, voidptr);
				tptr = ASExpAddr (t);
				break;
				/*}}} */
				/*{{{  segment */
			case S_SEGMENT:
			case S_SEGMENTITEM:
				modprewalktree (SNameAddr (t), f1, voidptr);
				modprewalktree (SStartExpAddr (t), f1, voidptr);
				modprewalktree (SLengthExpAddr (t), f1, voidptr);
				modprewalktree (SSubscriptExpAddr (t), f1, voidptr);
				tptr = SCheckExpAddr (t);
				break;
				/*}}} */
				/*{{{  space usage node */
			case S_SPACEUSAGE:
				tptr = SpBodyAddr (t);
				break;
				/*}}} */
				/*{{{  hidden parameter node */
			case S_HIDDEN_PARAM:
			case S_FNFORMALRESULT:
			case S_FNACTUALRESULT:
			case S_HIDDEN_TYPE:
				tptr = HExpAddr (t);
				break;
				/*}}} */
				/*{{{  S_LIST */
			case S_LIST:
				while (!EndOfList (t)) {
					modprewalktree (LeftAddr (t), f1, voidptr);
					t = NextItem (t);
				}
				return;
				/*}}} */
				/*{{{  S_FNTYPE */
			case S_FNTYPE:
				modprewalktree (LeftAddr (t), f1, voidptr);
				tptr = RightAddr (t);
				break;
				/*}}} */
				/*{{{  confignode */
			case S_CONNECT:
			case S_SET:
			case S_MAP:
				modprewalktree (ConnectFromEdgeAddr (t), f1, voidptr);
				modprewalktree (ConnectToEdgeAddr (t), f1, voidptr);
				tptr = ConnectArcAddr (t);
				break;
				/*}}} */
			}
		/*}}} */
	}
}

/*}}}*/
/*}}}*/

/*}}}*/
/*{{{  tree freeing */
#if 1				/*def CONFIG */
/*{{{  PUBLIC void freenode */
PUBLIC void freenode (treenode ** tptr)
{
	DEBUG_MSG (("freenode:%s ", itagstring (TagOf (*tptr))));
	DEBUG_MSG (("(0x%x) ", *tptr));
	freevec (*tptr, nodesize (TagOf (*tptr)));
	*tptr = NULL;
}

/*}}}*/
/*{{{  PUBLIC void freetree */
PUBLIC void freetree (treenode ** tptr)
{
	treenode *const t = *tptr;
	if (t == NULL)
		return;
	DEBUG_MSG (("freetree:%s (0x%x) ", itagstring (TagOf (t)), t));
	switch (nodetypeoftag (TagOf (t))) {
		/*{{{  default: error */
	default:
		badtag (LocnOf (t), TagOf (t), "freetree");
		break;
		/*}}} */
		/*{{{  actionnode */
	case ACTIONNODE:
		freetree (LHSAddr (t));
		freetree (RHSAddr (t));
		freetree (ActionDuringAddr (t));
		freetree (ActionAfterAddr (t));
		break;
		/*}}} */
		/*{{{  altnode */
	case ALTNODE:
		freetree (AltGuardAddr (t));
		freetree (AltInputAddr (t));
		freetree (AltBodyAddr (t));
		freetree (AltChanExpAddr (t));
		freetree (AltTimeExpAddr (t));
		break;
		/*}}} */
		/*{{{  typenode */
	case TYPENODE:
		freetree (ARDimLengthAddr (t));
		freetree (ARTypeAddr (t));
		break;
		/*}}} */
		/*{{{  arraysubnode */
	case ARRAYSUBNODE:
		freetree (ASBaseAddr (t));
		freetree (ASIndexAddr (t));
		freetree (ASExpAddr (t));
		freetree (ASLengthAddr (t));
		break;
		/*}}} */
		/*{{{  cnode */
	case CNODE:
		freetree (CBodyAddr (t));
		freetree (CTempAddr (t));
		break;
		/*}}} */
		/*{{{  condnode */
	case CONDNODE:
		freetree (CondGuardAddr (t));
		freetree (CondAfterAddr (t));
		freetree (CondBodyAddr (t));
		break;
		/*}}} */
		/*{{{  constexpnode */
	case CONSTEXPNODE:
		freetree (CExpAddr (t));
		break;
		/*}}} */
		/*{{{  consttablenode */
	case CONSTTABLENODE:
		freetree (CTExpAddr (t));
		break;
		/*}}} */
		/*{{{  declnode */
	case DECLNODE:
		/* The obvious thing to do is a simple tree walk.
		   But to avoid excessive recursion on lists of specifications,
		   we should process these specially.
		   But I'm too lazy to finish writing this, cos it will involve
		   pointer reversal and all that muck, to ensure that we never
		   access a treenode which has already been freed.
		   CON - 4/1/91.
		 */
#if 1
		/*{{{  simple version (but it works) */
		freetree (DValAddr (t));	/* freeup rhs of abbreviation, or PROC body */
		freetree (DBodyAddr (t));	/* this causes deep recursion */
		/* now freeup this namenode and its type tree */
		if (TagOf (DNameOf (t)) == S_LIST) {	/* multiple variable declaration */
			treenode *namelist = DNameOf (t);
			freetree (NTypeAddr (ThisItem (namelist)));	/* They all share a type tree */
			while (!EndOfList (namelist)) {
				treenode *this = namelist;
				freenode (ThisItemAddr (namelist));	/* freeup name node */
				namelist = NextItem (namelist);
				freenode (&this);	/* freeup list node */
			}
		} else {
			/*{{{  freeup formal parameter declarations */
			switch (TagOf (t)) {
			default:
				break;
				/*{{{  PROCDEF or FUNCDEF */
			case S_PROCDEF:
#ifdef MOBILES
			case S_MPROCDECL:
#endif
			case S_LFUNCDEF:
			case S_SFUNCDEF:
				{
					treenode *param = NTypeOf (DNameOf (t));
					if ((TagOf (t) != S_PROCDEF) && (TagOf (t) != S_MPROCDECL)) {
						/* We rely here on the fact that freetree and freenode
						   clear the pointer which is passed into them
						   after freeing up the space. This means that
						   the code later on which again clobbers the type
						   portion of this namenode doesn't clear this up twice.
						   CON - 14/2/91.
						   Hence the FnTypeList portion of the type is left NULL
						 */
						freetree (FnTypeListAddr (param));	/* return type list */
						param = FnParamsOf (param);	/* formal param list */
					}
					for (; !EndOfList (param); param = NextItem (param)) {
						const int tag = TagOf (ThisItem (param));
						if ((tag == N_PARAM) || (tag == N_VALPARAM) || (tag == N_RESULTPARAM)) {
							freetree (NTypeAddr (ThisItem (param)));	/* formal var's type */
						}
						freenode (ThisItemAddr (param));	/* formal var */
						/* see comment above about resetting all these pointers
						   to NULL; hence we simply get a list, where
						   each ThisItem is NULL */
					}
				}
				break;
				/*}}} */
			}
			/*}}} */
			switch (TagOf (t)) {
			default:
				freetree (NTypeAddr (DNameOf (t)));	/* Free up the type tree */
				freenode (DNameAddr (t));	/* freeup name node */
				break;
			case S_PLACE:
			case S_WSPLACE:
			case S_VSPLACE:
#if 1				/*def CONFIG */
			case S_PLACEON:
#endif
				break;
			}
		}
		break;
		/*}}} */
#else
		/*{{{  space efficient version (not finished) */
		{
			/* We must ensure that we don't free up a namenode before any
			   process which may refer to it.
			 */
			treenode *temp = t;
			while (isspecification (temp)) {
				freetree (DValAddr (temp));
				temp = DBodyOf (temp);
			}
			freetree (&temp);	/* free up the process after the list of decls */
			we must now free up the names, and the declnodes, in the correct order.return;
		}
		/*}}} */
#endif
		/*}}} */
		/*{{{  dopnode */
	case DOPNODE:
		freetree (LeftOpAddr (t));
		freetree (RightOpAddr (t));
		break;
		/*}}} */
		/*{{{  hiddenparamnode */
#if 0
	case HIDDENPARAMNODE:
		/* actual results have a 'cross-pointer' to the result expression */
		/* formal results have a 'cross-pointer' to the result type */
		if (TagOf (t) != S_FNACTUALRESULT && TagOf (t) != S_FNFORMALRESULT)
			freetree (HExpAddr (t));
		break;
#endif
		/*}}} */
		/*{{{  instancenode */
	case INSTANCENODE:
		freetree (INameAddr (t));
		freetree (IParamListAddr (t));
		break;
		/*}}} */
		/*{{{  leafnode */
	case LEAFNODE:
		/* don't freeup dummy expressions because they are shared */
		if (TagOf (t) == S_DUMMYEXP) {
			return;
		}
		freetree (LeafLinkAddr (t));
		break;
		/*}}} */
		/*{{{  listnode */
	case LISTNODE:
		{
			treenode *list = t;
			while (list != NULL) {
				treenode *this = list;
				freetree (LeftAddr (list));
				list = RightOf (list);
				freenode (&this);
			}
			*tptr = NULL;	/* destroy the reference to the list */
		}
		return;
		/*}}} */
		/*{{{  litnode */
	case LITNODE:
		freetree (LitExpAddr (t));
		freetree (LitTypeAddr (t));
		break;
		/*}}} */
		/*{{{  mopnode */
	case MOPNODE:
		/* bug 1142 - treat S_ELSIZE and S_SEGSTART specially 7/2/91 */
		if ((TagOf (t) != S_ELSIZE) && (TagOf (t) != S_SEGSTART))
			freetree (OpAddr (t));
		break;
		/*}}} */
		/*{{{  namenode */
	case NAMENODE:
#if 0				/* This breaks on TYPENODES (hidden parameters?) */
		if (TagOf (t) == T_TEMP || TagOf (t) == T_PREEVALTEMP)
			freetree (NTypeAddr (t));	/* freeup the expression for the temporary */
#endif
		return;		/* don't free the namenode here! */
		/*}}} */
		/*{{{  replcnode */
	case REPLCNODE:
		freetree (ReplCStartExpAddr (t));
		freetree (ReplCLengthExpAddr (t));
		if (ReplCStepExpOf (t)) {
			freetree (ReplCStepExpAddr (t));
		}
		freetree (ReplCBodyAddr (t));
		freetree (ReplCTempAddr (t));
		freetree (NTypeAddr (ReplCNameOf (t)));	/* The repl var's type tree */
		freenode (ReplCNameAddr (t));	/* freeup the name node explicitly */
		break;
		/*}}} */
		/*{{{  segmentnode */
	case SEGMENTNODE:
		freetree (SNameAddr (t));
		freetree (SStartExpAddr (t));
		freetree (SLengthExpAddr (t));
		freetree (SCheckExpAddr (t));
		freetree (SSubscriptExpAddr (t));
		freetree (SLengthAddr (t));
		break;
		/*}}} */
		/*{{{  spacenode */
	case SPACENODE:
		freetree (SpBodyAddr (t));
		break;
		/*}}} */
		/*{{{  valofnode */
	case VALOFNODE:
		freetree (VLBodyAddr (t));
		freetree (VLResultListAddr (t));
		break;
		/*}}} */
		/*{{{  wordnode */
	case WORDNODE:		/* This may happen for the body of a string */
		return;		/* Don't reclaim name table entries */
		/*}}} */
		/*{{{  confignode */
	case CONFIGNODE:
		freetree (STDevAddr (t));
		freetree (STAttrNameAddr (t));
		freetree (STAttrExpAddr (t));
		break;
		/*}}} */
		/*{{{  processornode */
	case PROCESSORNODE:
		freetree (ProcessorExpAddr (t));
/*freetree(ProcessorTypeAddr(t)); *//* That's a wordnode */
		freetree (ProcessorBodyAddr (t));
		break;
		/*}}} */
	}
	freenode (tptr);
}

/*}}}*/
#endif
/*}}}*/
