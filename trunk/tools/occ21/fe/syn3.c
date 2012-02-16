/* $Id: syn3.c,v 1.6 1997/09/17 08:31:43 jm40 Exp $ */

/*
 *	Occam two syntax analyser 3
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

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <string.h>
#include "feinc.h"

#include "lexdef.h"		/* for result_abbr_var */
#include "chkdef.h"		/* current_fe_handle etc */
#include "synerror.h"
#include "lexdef.h"
#include "syn1def.h"
#include "syn2def.h"
#include "syn3def.h"
#include "syndef.h"

/*}}}*/
/*{{{  reading predefined names*/
/*{{{  PRIVATE treenode *rpredefparam ()*/
/*****************************************************************************
 *
 *  rpredefparam reads in the specification of a parameter to a predefined
 *               PROC or FUNCTION.
 *
 *****************************************************************************/
PRIVATE treenode *rpredefparam (void)
{
	int ptag;

	if (symb == S_VAL) {
		ptag = N_VALPARAM;
		nextsymb ();
	} else if (symb == S_RESULT) {
		ptag = N_RESULTPARAM;
		nextsymb ();
	} else {
		ptag = N_PARAM;
	}
	return (declname (ptag, NOPOSN, NULL, rspecifier (), NULL));
}

/*}}}*/


/*{{{  PUBLIC treenode *rpredef ()*/
/*****************************************************************************
 *
 *  rpredef reads in and declares a single predefined name
 *           It does nothing sensible on error.
 *
 *****************************************************************************/
/*{{{  comment syntax*/
/*
   predef = ( FUNCTION specifier { ',' specifier } |
              PROC )
            '(' predefparam { ',' predefparam } ')' name ':'
          | field specifier name ':'

   predefparam = [ 'VAL' ] specifier
 */
/*}}}*/
PUBLIC treenode *rpredef (void)
{
	int ntag;
	wordnode *name;
	treenode *type, *nptr;
	treenode *ftypelist = NULL;	/* initialised to shut up gcc's optimiser */
	const int pdno = pdnumber;	/* ** SHC 6-Apr-1988 */
	BOOL forks = FALSE;
	BOOL suspends = FALSE;

	if (symb == S_END) {
		return NULL;
	}

	flocn = NOPOSN;		/* incase of any errors */

	if ((symb != S_FUNCTION) && (symb != S_PROC)) {
		/*{{{  special check for config NODE fields */
		if (symb == S_DOT) {
			ntag = N_FIELD;
			nextsymb ();
		} else {
			ntag = N_DECL;
		}
		type = rspecifier ();
		name = rname ();
		checkfor (S_COLON);
		checknewline ();
		/*}}} */
	} else {
		/*{{{  PROC or FUNCTION predefine */
		if (symb == S_FUNCTION) {
			/*{{{  set tag, read function type */
			ntag = N_PREDEFFUNCTION;
			nextsymb ();
			ftypelist = rlist (rspecifier, S_COMMA);
			/*}}} */
		} else if (symb == S_PROC) {
			/*{{{  set tag */
			ntag = N_PREDEFPROC;
			nextsymb ();
			/*}}} */
		} else {
			/*{{{  error */
			ntag = N_PREDEFPROC;	/* anything will do */
			synerr (SYN_PREDEF_ERROR, NOPOSN);
			longjmp (env, TRUE);
			/*}}} */
		}
		/*{{{  formal parameter list */
		checkfor (S_LPAREN);
		if (symb != S_RPAREN) {
			type = rlist (rpredefparam, S_COMMA);
		} else {
			type = NULL;
		}
		checkfor (S_RPAREN);
		/*}}} */
		if (symb == S_FORK) {
			forks = TRUE;
			nextsymb ();
		}
		if (symb == S_SUSPEND) {
			suspends = TRUE;
			nextsymb ();
		}
		name = rname ();
		checkfor (S_COLON);
		checknewline ();
		if (ntag == N_PREDEFFUNCTION) {
			type = newlistnode (S_FNTYPE, NOPOSN, ftypelist, type);
		}
		/*}}} */
	}

	/*{{{  make the namenode            **SHC 5-Apr-1988 */
	nptr = declname (ntag, NOPOSN, name, type, NULL);
	SetNMode (nptr, pdno);
	if (ntag == N_PREDEFPROC) {
		if (forks) {
			SetNPForks (nptr, 1);
		}
		if (suspends) {
			SetNPSuspends (nptr, 1);
		}
	}
#if 0
fprintf (stderr, "syn3: rpredef: declared name = ");
printtreenl (stderr, 4, nptr);
#endif
	/*}}} */

	return nptr;
}

/*}}}*/
/*}}}*/

/*{{{  PUBLIC treenode *rconfigdef ()*/
/* Terminates with symb at start of next line */
PUBLIC treenode *rconfigdef ()
{
	const int indent = symbindent;
	const SOURCEPOSN locn = flocn;
	wordnode *name;
	treenode *pbody;
	treenode *retptr;
	const int oldlexlevel = syn_lexlevel;
	const int thistag = symb;
	const char *const string = tagstring (thistag);	/* Any name will do, so we use the keyword */

	DEBUG_MSG (("rconfigdef: %s\n", string));
	syn_lexlevel++;
	foundroutine = TRUE;

	nextsymb ();
	/*if ((name = rname ()) == NULL) goto error; */
	if (symb == S_NAME) {
		name = lexword;
		nextsymb ();
	} else {
		/* invent a name */
		name = lookupword (string, strlen (string));
	}
	if (checknlindent (indent + 2)) {
		goto error2;
	}

	pbody = rprocess ();

	if (checkindent (indent)) {
		goto error2;
	}
	while (symb == S_COMMENT) {
		if (checknlindent (indent)) {
			goto error2;
		}
	}
	if (checkfor (S_COLON)) {
		goto error2;
	}

	syn_lexlevel = oldlexlevel;
	retptr = declare (thistag, locn, NULL, name, pbody);
	if (checknlindent (indent)) {
		skiplines (indent);
		return (NULL);
	}
	return retptr;

	/*error:
	   nextline (); */
      error2:
	msg_out_s (SEV_INFO, SYN, SYN_SKIPPING_DEFN, NOPOSN, string);
	skiplines (indent);
	if (symb == S_COLON) {
		nextline ();
	}
	syn_lexlevel = oldlexlevel;
	return NULL;
}

/*}}}*/
/*{{{  PRIVATE treenode *rset()*/
/* What we are parsing:
   SET element '(' name {',' name } ':=' exp {',' exp} ')'
NOW:
   SET element '(' element {',' element } ':=' exp {',' exp} ')'
*/
PRIVATE treenode *rset (void)
{
	treenode *node, *lhs, *rhs;

	nextsymb ();
	node = relement ();
	if ((node == NULL) || checkfor (S_LPAREN) || checklinebreak () || ((lhs = rlist (relement, S_COMMA)) == NULL)
	    || checkfor (S_ASS) || checklinebreak () || ((rhs = rlist (rexp, S_COMMA)) == NULL) || checkfor (S_RPAREN)) {

		nextline ();
		return NULL;
	}

	node = newconfignode (S_SET, flocn, node, lhs, rhs);
	checknewline ();
	return node;
}

/*}}}*/
/*{{{  PRIVATE treenode *rconnect()*/
PRIVATE treenode *rconnect (void)
{
	/*{{{  what we are parsing */
	/*
	   'CONNECT' element 'TO' element [ 'WITH' element ]
	 */
	/*}}} */
	treenode *fromedge, *toedge, *arc = NULL;

	nextsymb ();		/* Skip CONNECT */
	if (((fromedge = relement ()) == NULL) || checkfor (S_TO) || checklinebreak () || ((toedge = relement ()) == NULL)) {
		nextline ();
		return NULL;
	}
	if (symb == S_WITH) {
		nextsymb ();
		checklinebreak ();
		arc = relement ();
		if (arc == NULL) {
			nextline ();
			return NULL;
		}
	}
	fromedge = newconfignode (S_CONNECT, flocn, fromedge, toedge, arc);
	checknewline ();
	return fromedge;
}

/*}}}*/
/*{{{  PRIVATE treenode *rmap()*/
PRIVATE treenode *rmap (void)
{
	/*{{{  what we are parsing */
	/*
	   'MAP' {, element} 'ONTO' element
	 */
	/*}}} */
	treenode *sourcelist, *dest, *pri = NULL;

	nextsymb ();		/* Skip MAP */
	if (((sourcelist = rlist (relement, S_COMMA)) == NULL) || checkfor (S_ONTO) || checklinebreak () || ((dest = relement ()) == NULL)) {
		nextline ();
		return NULL;
	}
	if (symb == S_PRI) {
		nextsymb ();
		checklinebreak ();
		if ((pri = rexp ()) == NULL) {
			nextline ();
			return NULL;
		}
	}
	sourcelist = newconfignode (S_MAP, flocn, sourcelist, dest, pri);
	checknewline ();
	return sourcelist;
}

/*}}}*/

/*{{{  PRIVATE int rrepl(nptr, start, length, step)*/
/* parsing:      name '=' exp 'FOR' exp             */
/*      or:      name '=' exp 'FOR' exp 'STEP' exp  */
PUBLIC int rrepl (treenode ** nptr, treenode ** start, treenode ** length, treenode ** step)
{
	wordnode *name;

	if (((name = rname ()) == NULL) || ((lexmode == LEX_CSOURCE) ? checkfor (S_ASS) : checkfor (S_EQ)) || ((*start = rexp ()) == NULL) ||
	    (checkfor (S_FOR)) || (checklinebreak ()) || ((*length = rexp ()) == NULL)) {

		return TRUE;
	}

	if (symb == S_STEP) {
		if ((checkfor (S_STEP)) || ((*step = rexp ()) == NULL)) {
			return TRUE;
		}
	} else {
		*step = NULL;	/* set to NULL, and it will generate original code :-) */
	}
	*nptr = declname (N_REPL, flocn, name, newleafnode (S_INT, flocn), NULL);
	SetNReplKnown (*nptr, FALSE);	/* just make sure */

	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE treenode *rspecorelement (BOOL *const specflag, const int indent)*/
PRIVATE treenode *rspecorelement (BOOL *const specflag, const int indent)
{
	/*{{{  what we are parsing */
	/* Parse
	   element
	   | specification
	   * and return the parse tree.
	   * *specflag is TRUE  if a specification was found,
	   * FALSE if an element was found.
	 */
	/*}}} */
	const SOURCEPOSN locn = flocn;
	treenode *a;
	*specflag = FALSE;

#if 0
	/* this is only called from one place, and we know 'mustbespec' is false */
	if (mustbespec (symb)) {
		*specflag = TRUE;
		return (rspecification ());
	}
#endif
	switch (symb) {
	case S_NAME:
		/*{{{  S_NAME */
		{
			a = (treenode *) rname ();
			if (symb == S_IS) {
				*specflag = TRUE;
				return (rrestofspec (NULL, a, locn, indent, TRUE));
			}
#ifdef OCCAM2_5
			else if (symb == S_NAME) {
				/* parse the following:
				   type.name name {, name} :
				 */
				*specflag = TRUE;
				return rspecnameandbody (a, locn, indent, TRUE);
			}
			/* NOTE - we don't test for function declarations here.
			 * we treat "name FUNCTION ..." etc as an element here,
			 * and check for the 'FUNCTION' part later.
			 */
#endif
			else {
				if (symb == S_LBOX && ((a = rsubscript (a)) == NULL)) {
					goto error;
				}
				return a;
			}
			break;
		}
		/*}}} */
	case S_LBOX:
		/*{{{  S_LBOX */
		nextsymb ();
		if ((a = rexp ()) == NULL) {
			goto error;
		}
		if (symb == S_RBOX) {
			/*{{{  read declaration of array type, or function definition */
			treenode *spec;

			*specflag = TRUE;
			nextsymb ();
			if ((spec = rspecifier ()) == NULL) {
				goto error;
			}
			a = newtypenode (S_ARRAY, locn, a, spec);
			return (rspecnameandbody (a, locn, indent, TRUE));
			/*}}} */
#ifdef OCCAM2_5
		} else if ((symb == S_FROM) || (symb == S_FOR)) {
#else
		} else if (symb == S_FROM) {
#endif
			/*{{{  read segment */
			if ((a = rsegment (a)) == NULL) {
				goto error;
			}
			if (symb == S_LBOX && ((a = rsubscript (a)) == NULL)) {
				goto error;
			}
			return a;
			/*}}} */
		} else {
			synerr_e (SYN_E_RBOX_OR_FROM, flocn, symb);
			goto error;
		}
		break;
		/*}}} */
#if 0
	default:
		/* can never happen, cos this is only called in one place,
		 * and we know symb is either NAME or LBOX */
		synerr_e (SYN_E_ELEMENT_OR_SPEC, locn, symb);
		break;
#endif
	}
      error:
	if (*specflag) {
		nextline ();
		skiplines (indent);
	}
	return NULL;
}

/*}}}*/
/*{{{  PRIVATE treenode *rspecorelementlist*/
#ifdef OCCAM2_5
PRIVATE BOOL rspecorelementlist_found_spec;
PRIVATE int rspecorelementlist_indent;

/*{{{  PRIVATE treenode *rtypespecorelement (BOOL *const specflag, const int indent)*/
PRIVATE treenode *rtypespecorelement (BOOL *const specflag, const int indent)
{
	/*{{{  what we are parsing */
	/* Parse
	   element
	   | specification
	   * and return the parse tree.
	   * *specflag is TRUE  if a specification was found,
	   * FALSE if an element was found.
	 */
	/*}}} */
	const SOURCEPOSN locn = flocn;
	treenode *a;

	*specflag = FALSE;
	switch (symb) {
		/*{{{  S_BOOL S_BYTE S_INT S_INTn S_REALn S_TIMER S_CHAN S_PORT ... */
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
		/*}}} */
#ifndef RECORDS_OF_CHANNELS	/* Records of channels not yet supported */
	case S_CHAN:
#endif
	case S_PORT:
	case S_TIMER:
	case S_BOX:
		CASE_CONFIG_TYPE {
			/*{{{  COMMENT what we are parsing */
			/*Parse */
			/*specifier name 'IS' element ':'                         | */
			/*specifier name 'RETYPES' element ':'                    | */
			/*type {1 ',' name } ':'                                  | */
			/*type name                                               | */
			/*init.body */
			/*':' */
			/*{1 ',' type } 'FUNCTION' */
			/*':'                                                     | */
			/*{1 ',' type } 'FUNCTION' */
			 /**/
				/*types and specifiers are treated as equivalent */
				 /**/
				/*}}} */
				treenode * t;

			/*{{{  parse specifier */
			if ((t = rspecifier ()) == NULL) {
				nextline ();
				return NULL;
			}
			/*}}} */
			*specflag = TRUE;
			return t;
		}
	case S_NAME:
		/*{{{  S_NAME */
		{
			a = (treenode *) rname ();
			if (symb == S_IS) {
				*specflag = TRUE;
				return (rrestofspec (NULL, a, locn, indent, TRUE));
			} else if (symb == S_NAME) {
				/* parse the following:
				 *      type.name name {, name} :
				 */
				*specflag = TRUE;
				return rspecnameandbody (a, locn, indent, TRUE);
			} else {
				/* NOTE - we don't test for function declarations here.
				 * we treat "name FUNCTION ..." etc as an element here,
				 * and check for the 'FUNCTION' part later.
				 */
				if (symb == S_LBOX && ((a = rsubscript (a)) == NULL)) {
					goto error;
				}
				return a;
			}
			break;
		}
		/*}}} */
	case S_LBOX:
		/*{{{  S_LBOX */
		nextsymb ();
		if ((a = rexp ()) == NULL) {
			goto error;
		}
		if (symb == S_RBOX) {
			/*{{{  read declaration of array type, or function definition */
			treenode *spec;

			*specflag = TRUE;
			nextsymb ();
			if ((spec = rspecifier ()) == NULL) {
				goto error;
			}
			a = newtypenode (S_ARRAY, locn, a, spec);
			return a;
			/*}}} */
		} else if ((symb == S_FROM) || (symb == S_FOR)) {
			/*{{{  read segment */
			if ((a = rsegment (a)) == NULL) {
				goto error;
			}
			if (symb == S_LBOX && ((a = rsubscript (a)) == NULL)) {
				goto error;
			}
			return a;
			/*}}} */
		} else {
			synerr_e (SYN_E_RBOX_OR_FROM, flocn, symb);
			goto error;
		}
		break;
		/*}}} */
#if 0
	default:
		/* can never happen, cos this is only called in one place,
		 * and we know symb is either NAME or LBOX */
		synerr_e (SYN_E_ELEMENT_OR_SPEC, locn, symb);
		break;
#endif
	}
      error:
	if (*specflag) {
		nextline ();
		skiplines (indent);
	}
	return NULL;
}

/*}}}*/


/*{{{  PRIVATEPARAM treenode *sub_rspecorelementlist*/
PRIVATEPARAM treenode *sub_rspecorelementlist (void)
{
	BOOL specflag;
	treenode *tptr;

	tptr = rtypespecorelement (&specflag, rspecorelementlist_indent);
	if (specflag) {
		rspecorelementlist_found_spec = TRUE;
	}
	return tptr;
}

/*}}}*/


/*{{{  PRIVATE treenode *rspecorelementlist(BOOL *specflag, const int indent)*/
PRIVATE treenode *rspecorelementlist (BOOL * specflag, const int indent)
{
	/* parses a list for either the lhs of an action,
	 * or the lhs of a function definition
	 */
	treenode *list;
	const int saved_indent = rspecorelementlist_indent;
	const BOOL saved_found_spec = rspecorelementlist_found_spec;

	rspecorelementlist_indent = indent;
	rspecorelementlist_found_spec = FALSE;
	list = rlist (sub_rspecorelementlist, S_COMMA);
	*specflag = rspecorelementlist_found_spec;
	rspecorelementlist_found_spec = saved_found_spec;
	rspecorelementlist_indent = saved_indent;
	return list;
}

/*}}}*/
#endif
/*}}}*/
/*{{{  initial declaration stuff*/
/* first few used by RESULT abbreviations as well */
/*{{{  PRIVATE void temp_decl_list(normlist, init_type)*/
/*{{{  COMMENT what this does*/
/*This function takes a (possibly single item, not in a list) list of*/
/*indentifiers for an initialised declaration, and replaces it with a list of*/
/*generated declarations of the same length.  This function would normally be*/
/*used with a copy of the list of names - it is simply used as a template to*/
/*get the right length of list easily.*/
/*}}}*/
PRIVATE treenode *temp_decl_list (treenode *nl, treenode *init_type)
{
	treenode *normlist = nl;
	SOURCEPOSN locn = flocn;
	treenode *tptr = newdeclnode (S_DECL, locn, NULL, NULL, NULL);
	static int listcount = 0;		/* wholly unique please */

	if (TagOf (normlist) == S_LIST) {
		treenode *a = normlist;
		while (!EndOfList (a)) {
			/*{{{  replace each item with a generated delcaration */
			char *tempname = (char *)memalloc (12);
			wordnode *tempword;

			sprintf (tempname, "tmp$.%d", listcount);
			tempword = lookupword (tempname, strlen (tempname));
			NewItem (declname (N_DECL, locn, tempword, init_type, tptr), a);
			a = NextItem (a);
			listcount++;
			/*}}} */
		}
	} else {
		char *tempname = (char *)memalloc (12);

		sprintf (tempname, "tmp$.%d", listcount);
		normlist = declname (N_DECL, locn, lookupword (tempname, strlen (tempname)), init_type, tptr);
		listcount++;
	}
	SetDName (tptr, normlist);
	return tptr;
}

/*}}}*/
/*{{{  PRIVATE void generate_temp_name_list(normlist)*/
PRIVATE treenode *generate_temp_name_list (treenode * normlist)
{
	/*{{{  COMMENT */
	/*This function takes a list of identifiers and returns a list of the same */
	/*length containing numbered identifiers containing unacceptable characters */
	/*- this means that they cannot be accidentally genereated by the coder.*/
	/*they have the form tmp$<number>; so the list will be tmp$0, tmp$1, */
	/*tmp$2 .... */
	/*This is used to generate some temp variables for initial declarations */
	/*- it basically leaves the caller to copy the orgional tree - it just*/
	/*means I don't have to do all the list construction stuff. */
	/*}}} */
	static int listcount = 0;		/* unique please */

	if (TagOf (normlist) == S_LIST) {
		treenode *a = normlist;

		while (!EndOfList (a)) {
			/*{{{  replace each item with a generated _name_ (not declaration) */
			char *tempname = (char *) malloc (12);
			wordnode *tempword;

			sprintf (tempname, "tmp$.%d", listcount);
			tempword = lookupword (tempname, strlen (tempname));
			NewItem ((treenode *) tempword, a);

			a = NextItem (a);
			listcount++;
			/*}}} */
		}
	} else {
		char *tempname = (char *)malloc (12);

		sprintf (tempname, "tmp$.%d", listcount);
		normlist = (treenode *) lookupword (tempname, strlen(tempname));
		listcount++;
	}
	/*{{{  COMMENT debugging output code */
	/*fprintf(stderr,"from generate_temp_name_list():\n"); */
	/*printtree(stderr,0,normlist); */
	/*fprintf(stderr,"\n---\n"); */
	/*}}} */
	return normlist;
}

/*}}}*/
#ifdef INITIAL_DECL
/*{{{  PRIVATE treenode *gen_abbrv_and_process_for_initials(temp_decl, real_decl, type)*/
PRIVATE treenode *gen_abbrv_and_process_for_initials (treenode * temp_decls, treenode * real_decls, treenode * init_type)
{
	int locn = flocn;
	int indent = symbindent;
	treenode *a = temp_decls;
	treenode *b = DNameOf (real_decls);
	treenode *root;
	treenode **prev_ptr;

	/*{{{  COMMENT debugging output */
	/*fprintf(stderr,"output a:\n"); */
	/*printtree(stderr,0,a); */
	/*fprintf(stderr,"b:\n"); */
	/*printtree(stderr,0,b); */
	/*fprintf(stderr,"\n---\n"); */
	/*}}} */
	indent = indent;	/* causes usage of it! */
	prev_ptr = &root;
	if (TagOf (a) == S_LIST) {
		while (!EndOfList (a)) {
			*prev_ptr = newdeclnode (S_ABBR, locn, NULL, NULL, NULL);
			SetDName (*prev_ptr, declname (N_DECL, locn, (wordnode *) ThisItem (a), init_type, *prev_ptr));
			SetDVal (*prev_ptr, ThisItem (b));
			/*{{{  COMMENT debugging output */
			/*fprintf(stderr,"current node:"); printtree(stderr,0,*prev_ptr); */
			/*fprintf(stderr,"\n"); */
			/*}}} */
			/* INT <real name> IS <tmp name>: */
			prev_ptr = DBodyAddr (*prev_ptr);

			a = NextItem (a);
			b = NextItem (b);
		}
	} else {
		root = newdeclnode (S_ABBR, locn, NULL, NULL, NULL);
		SetDName (root, declname (N_DECL, locn, (wordnode *) a, init_type, root));
		SetDVal (root, b);
		prev_ptr = DBodyAddr (root);
	}
	*prev_ptr = rprocess ();
	return root;
}

/*}}}*/
/*{{{  PRIVATE treenode *rinitial ()*/
PRIVATE treenode *rinitial (void)
{
	/*{{{  delclarations */
	treenode *init_type;
	treenode *expression;
	treenode *nptr;
	treenode *tptr;
	wordnode *name;
	treenode *namelist;
	treenode *seq_list;
	treenode *seq_node;
	treenode *ass_node;
	treenode **procptr;
	/* treenode *abbrv_node; */
	treenode *abbrv_list;
	SOURCEPOSN locn = flocn;
	int indent = symbindent;
	treenode *rawtree;
	treenode *temp_rawtree;
	treenode *exptr;
	
	BOOL lhs_is_list = FALSE;
	/*}}} */

	nextsymb ();
	init_type = rinitialspec ();	/* get type */
	if (init_type == NULL) {
		/*{{{  skip line and return NULL */
		/*nextline ();
		   skiplines (indent); */
		return NULL;	/* some problem with the type */
		/*}}} */
	}

	/*{{{  get variable name and declare */
	name = rname ();
	if (symb == S_COMMA) {
		/*{{{  no lists error */
		treenode *list;
		nextsymb ();
		if (checklinebreak ()) {
			goto error2;
		}
		if ((list = rlist ((treenode * (*)())rname, S_COMMA)) == NULL) {
			goto error;
		}
		nptr = newlistnode (S_LIST, locn, (treenode *) name, list);
		lhs_is_list = TRUE;
		/*}}} */
	} else {
		nptr = (treenode *) name;
		if (!nptr) {
			goto error;
		}
	}
	namelist = nptr;
	rawtree = copytree (namelist, syn_lexlevel);
	temp_rawtree = copytree (namelist, syn_lexlevel);
	tptr = temp_decl_list (namelist, init_type);	/* delcare some temp variables */
	temp_rawtree = generate_temp_name_list (temp_rawtree);	/* also a stripped down version */
	/* only containing the names of the temp variables */
	/*{{{  COMMENT */
	/*tptr = newdeclnode (S_DECL, locn, NULL, NULL, NULL); */
	/*if (TagOf(namelist) == S_LIST) */
	/*{ */
	/*a = namelist; */
	/*while (!EndOfList(a)) */
	/*{{{  declare name on left, move to right */
	/*{ */
	/*treenode *nptr = ThisItem(a); */
	/*if (TagOf(nptr) == S_NAME) */
	/*NewItem(declname(N_DECL, locn, */
	/*(wordnode *)nptr, init_type, tptr), a); */
	/*else */
	/*{ */
	/*synerr_e(SYN_E_SPEC, locn, TagOf(nptr)); */
	/*goto error; */
	/*} */
	/*a = NextItem(a); */
	/*} */
	/*}}} */
	/*} */
	/*else */
	/*nptr = declname (N_DECL, locn, (wordnode *)namelist, init_type, tptr); */
	/*SetDName(tptr,nptr); */
	/*}}} */
	/*}}} */

	checkfor (S_IS);

	expression = rexp ();
	if (!expression) {
		goto error;
	}
	if (symb == S_COMMA) {
		/*{{{  parse list of exp's or error handle */
		treenode *list;
		nextsymb ();
		if (checklinebreak ()) {
			goto error2;
		}
		if ((list = rlist ((treenode * (*)())rexp, S_COMMA)) == NULL) {
			goto error;
		}
		exptr = newlistnode (S_LIST, locn, (treenode *) expression, list);
		/*}}} */
	} else {
		exptr = (treenode *) expression;
		if (lhs_is_list && (TagOf (exptr) != S_LIST)) {
			/* frmb: turn it into a list, fixes bug clj found */
			exptr = newlistnode (S_LIST, locn, exptr, NULL);
		}
	}

	checkfor (S_COLON);
	nextline ();
	if (checkindent (indent)) {
		return NULL;
	}
	/*{{{  COMMENT debugging code */
	/*fprintf(stderr,"Debugging Code Start (before assignment) \n"); */
	/*fprintf(stderr,"\n--- rawtree follows\n"); */
	/*printtree(stderr,0,rawtree); */
	/*fprintf(stderr,"\n--- temp_rawtree follows\n"); */
	/*printtree(stderr,0,temp_rawtree); */
	/*fprintf(stderr,"\n--- tptr follows\n"); */
	/*printtree(stderr,0,tptr); */
	/*fprintf(stderr,"\nDebugging Code End (before assignment)\n"); */
	/*}}} */

	ass_node = newactionnode (S_ASS, locn, (treenode *) temp_rawtree, exptr);
	/* assignment bit */
	/*{{{  COMMENT debugging code */
	/*fprintf(stderr,"Debugging Code Start (after assignment) \n"); */
	/*fprintf(stderr,"\n--- rawtree follows\n"); */
	/*printtree(stderr,0,rawtree); */
	/*fprintf(stderr,"\n--- temp_rawtree follows\n"); */
	/*printtree(stderr,0,temp_rawtree); */
	/*fprintf(stderr,"\n--- tptr follows\n"); */
	/*printtree(stderr,0,tptr); */
	/*fprintf(stderr,"\nDebugging Code End (after assignment)\n"); */
	/*}}} */
	abbrv_list = newlistnode (S_LIST, locn, gen_abbrv_and_process_for_initials (rawtree, tptr, init_type), NULL);
	seq_list = newlistnode (S_LIST, locn, ass_node, abbrv_list);
	seq_node = newcnode (S_SEQ, locn, seq_list);	/* create SEQ node */
	/* w = rproclist (rprocess, indent);  (...process) same indentation */
	procptr = DBodyAddr (tptr);	/* get pointer to pointer from decl to process */
	*procptr = seq_node;	/* link declaration on to front of process */
	return (tptr);

      error:
	nextline ();
      error2:
	skiplines (indent);
	return NULL;
}

/*}}}*/
#endif /* INITIAL_DECL */
/*}}}*/
/*{{{  PRIVATE treenode *gen_abbrv_and_process_for_results (treenode *temp_decls, treenode *real_decls, treenode *res_type)*/
/*
 *	generates abbreviations and process for RESULT abbreviations
 */
PRIVATE treenode *gen_abbrv_and_process_for_results (treenode *temp_decls, treenode *real_decls, treenode *res_type)
{
	int locn = flocn;
	int indent = symbindent;
	treenode *a = temp_decls;
	treenode *b = DNameOf (real_decls);
	treenode *root;
	treenode **prev_ptr;

	indent = indent;	/* causes usage of it! */
	prev_ptr = &root;
	root = newdeclnode (S_ABBR, locn, NULL, NULL, NULL);
	SetDName (root, declname (N_DECL, locn, (wordnode *) a, res_type, root));
	SetDVal (root, b);
	prev_ptr = DBodyAddr (root);
	*prev_ptr = rprocess ();
	return root;
}
/*}}}*/
/*{{{  PRIVATE treenode *rresultabbr (void) */
/*
 *	parses a RESULT abbreviation:
 *	    RESULT <type> <name> IS <var>:
 *	based on (and uses) Jim's stuff above, since they're pretty similar
 */
PRIVATE treenode *rresultabbr (void)
{
	treenode *res_type, *nptr;
	SOURCEPOSN locn = flocn;
	int indent = symbindent;

	nextsymb ();
	res_type = rresultspec ();
	if (!res_type) {
		return NULL;
	}
	nptr = (treenode *)rname ();
	if (!nptr) {
		goto error;
	}
	if (TagOf (nptr) == S_LIST) {
		synerr (SYN_TOO_MANY_NAMES, locn);
		goto error;
	}
	if (result_abbr_var) {
		treenode *elem, *tptr;
		treenode *namelist;
		treenode *rawtree, *temp_rawtree;
		treenode *ass_node, *abbrv_list, *seq_node;
		treenode **procptr;

		/* generate temporary names and declare */
		namelist = (treenode *)nptr;
		rawtree = copytree (namelist, syn_lexlevel);
		temp_rawtree = copytree (namelist, syn_lexlevel);
		tptr = temp_decl_list (namelist, res_type);		/* delcare some temp variables */
		temp_rawtree = generate_temp_name_list (temp_rawtree);	/* also a stripped down version */
		if (checkfor (S_IS)) {
			return NULL;
		}
		elem = relement ();			/* only allowed variables or subscriptions on the RHS */
		if (!elem) {
			return NULL;
		}
		if (checkfor (S_COLON)) {
			goto error2;
		}
		checknewline ();
		ass_node = newlistnode (S_LIST, locn, newactionnode (S_ASS, locn, elem, (treenode *)temp_rawtree), NULL);

		abbrv_list = gen_abbrv_and_process_for_results (rawtree, tptr, res_type);
		seq_node = newcnode (S_SEQ, locn, newlistnode (S_LIST, locn, abbrv_list, ass_node));	/* create SEQ process */
		/* w = rproclist (rprocess, indent);  (...process) same indentation */
		procptr = DBodyAddr (tptr);	/* get pointer to pointer from decl to process */
		*procptr = seq_node;	/* link declaration on to front of process */
		SetNVRDefined (DNameOf (abbrv_list), TRUE);
		SetNVEndPosn (DNameOf (abbrv_list), flocn);
#if 0
fprintf (stderr, "rresutabbr(): tree generated is:");
printtreenl (stderr, 4, tptr);
fprintf (stderr, "abbrv_list =");
printtreenl (stderr, 4, abbrv_list);
#endif

		return tptr;
	} else {
		treenode *elem, *tptr, *sub_proc;

		/* generate plain abbreviation */
		if (checkfor (S_IS)) {
			return NULL;
		}
		elem = relement ();
		if (!elem) {
			goto error;
		}
		if (checkfor (S_COLON)) {
			goto error;
		}
		if (checknlindent (indent)) {
			return NULL;
		}
		tptr = declare (S_ABBR, locn, res_type, (wordnode *)nptr, elem);
		SetNVRDefined (DNameOf (tptr), TRUE);
		sub_proc = rprocess ();
		SetNVEndPosn (DNameOf (tptr), flocn);
		SetDBody (tptr, sub_proc);
#if 0
fprintf (stderr, "rresutabbr(): tree generated is:");
printtreenl (stderr, 4, tptr);
#endif
		return tptr;
	}
error:
	nextline ();
error2:
	skiplines (indent);
	return NULL;
}
/*}}}  */
/*{{{  PRIVATE treenode *rplacedchan (void)*/
/* parsing:
 *	PLACED CHAN [OF] <protocol> <name> [AT] <element>:
 * returns a tree of the form:
 *	CHAN <protocol> <name>:
 *	PLACE <name> <element>:
 * have already seen PLACED, and symb is now CHAN
 */
PRIVATE treenode *rplacedchan (void)
{
	treenode *type, *name, *addr, *rdecl, *placenode;
	SOURCEPOSN locn = flocn;

	type = rspecifier ();
	if (!type) {
		synerr_e (SYN_E_SPECIFIER, flocn, symb);
		return NULL;
	}
	name = (treenode *)rname ();
	if (!name) {
		synerr_e (SYN_E_NAME, flocn, symb);
		return NULL;
	}
	/* maybe a channel direction specifier ? */
	if (symb == S_INPUT) {
		nextsymb ();
		SetTypeAttr (type, TypeAttrOf (type) | TypeAttr_marked_in);
	} else if (symb == S_OUTPUT) {
		nextsymb ();
		SetTypeAttr (type, TypeAttrOf (type) | TypeAttr_marked_out);
	}
	SetTypeAttr (type, TypeAttrOf (type) | TypeAttr_placed);
	/* optional AT */
	if (symb == S_AT) {
		nextsymb ();
	}
	if (!(addr = relement ())) {
		synerr_e (SYN_E_ELEMENT, flocn, symb);
		return NULL;
	}
	if (checkfor (S_COLON)) {
		return NULL;
	}
	checknewline ();
	rdecl = declare (S_DECL, locn, type, (wordnode *)name, NULL);
	placenode = newdeclnode (S_PLACE, locn, (treenode *)name, addr, NULL);
	SetDBody (rdecl, placenode);

#if 0
fprintf (stderr, "rplacedchan: returning rdecl =");
printtreenl (stderr, 4, rdecl);
#endif
	return rdecl;
}
/*}}}  */
/*{{{  PRIVATE treenode *rplacedarray (void)*/
/* parsing:
 *	PLACED "["<dim>"]"["["<dim>"]"...] <type> <name> [AT] <element>:
 * returns a tree of the form:
 *	[<dim>].. <type> <name>:
 *	PLACE <name> <element>:
 */
PRIVATE treenode *rplacedarray (void)
{
	treenode *type, *name, *addr, *rdecl, *placenode;
	SOURCEPOSN locn = flocn;

	type = rspecifier ();
	if (!type) {
		synerr_e (SYN_E_SPECIFIER, flocn, symb);
		return NULL;
	}
	name = (treenode *)rname ();
	if (!name) {
		synerr_e (SYN_E_NAME, flocn, symb);
		return NULL;
	}
	SetTypeAttr (type, TypeAttrOf (type) | TypeAttr_placed);
#if 0
fprintf (stderr, "rplacedarray: type = ");
printtreenl (stderr, 4, type);
#endif
	/* optional AT */
	if (symb == S_AT) {
		nextsymb ();
	}
	if (!(addr = rexp ())) {
		synerr_e (SYN_E_EXPR, flocn, symb);
		return NULL;
	}
	if (checkfor (S_COLON)) {
		return NULL;
	}
	checknewline ();
	rdecl = declare (S_DECL, locn, type, (wordnode *)name, NULL);
	placenode = newdeclnode (S_PLACE, locn, (treenode *)name, addr, NULL);
	SetDBody (rdecl, placenode);

#if 0
fprintf (stderr, "rplacedarray: returning rdecl = ");
printtreenl (stderr, 4, rdecl);
#endif
	return rdecl;
}
/*}}}*/
/*{{{  PRIVATE treenode *rplaceddecl (void)*/
/* parsing:
 *	PLACED <type> <name> [AT] <element>:
 * returns a tree of the form:
 *	<type> <name>:
 *	PLACE <name> <element>:
 */
 PRIVATE treenode *rplaceddecl (void)
 {
 	treenode *rdecl = NULL;
	treenode *type, *name, *placenode, *addr;
	SOURCEPOSN locn = flocn;

	type = rspecifier ();
	if (!type) {
		synerr_e (SYN_E_SPECIFIER, flocn, symb);
		return NULL;
	}

	name = (treenode *)rname ();
	if (!name) {
		synerr_e (SYN_E_NAME, flocn, symb);
		return NULL;
	}

#if 0
fprintf (stderr, "rplaceddecl: type = ");
printtreenl (stderr, 4, type);
#endif

	/* optional AT */
	if (symb == S_AT) {
		nextsymb ();
	}
	if (!(addr = rexp ())) {
		synerr_e (SYN_E_EXPR, flocn, symb);
		return NULL;
	}
	if (checkfor (S_COLON)) {
		return NULL;
	}
	checknewline ();
	rdecl = declare (S_DECL, locn, type, (wordnode *)name, NULL);
	placenode = newdeclnode (S_PLACE, locn, (treenode *)name, addr, NULL);
	SetDBody (rdecl, placenode);

	return rdecl;
 }
/*}}}*/
/*{{{  PRIVATE treenode *rprocessor ()*/
/**********************  Start comment out ****************************
processor = PROCESSOR element
process
**********************   End comment out  ****************************/
PRIVATE treenode *rprocessor (void)
{
	int indent = symbindent;
	treenode *e;
	treenode *t;
	SOURCEPOSN locn = flocn;

	nextsymb ();
	if ((e = relement ()) == NULL) {
		goto error;
	}
	if (checknlindent (indent + 2)) {
		goto error2;
	}
	syn_lexlevel++;
	t = newprocessornode (S_PROCESSOR, locn, e, NULL, rprocess ());
	syn_lexlevel--;
	return (t);

      error:
	nextline ();
      error2:
	skiplines (indent);
	return NULL;
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *rinputitem ()*/
PRIVATEPARAM treenode *rinputitem (void)
{
	treenode *a;

	/* read in  {1 ';' (variable [:: variable]) } */
	DEBUG_MSG (("rinputitem... "));
	if ((a = relement ()) == NULL) {
		return NULL;
	}
	if (symb == S_COLON2) {
		SOURCEPOSN locn = flocn;
		treenode *array;

		nextsymb ();
		if ((array = relement ()) == NULL) {
			return NULL;
		}
		a = newdopnode (S_COLON2, locn, a, array, 0);
	}

	return (a);
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *routputitem ()*/
PRIVATEPARAM treenode *routputitem (void)
{
	treenode *a;

	DEBUG_MSG (("routputitem... "));
	if ((a = rexp ()) == NULL) {
		return NULL;
	}
	if (symb == S_COLON2) {
		SOURCEPOSN locn = flocn;
		treenode *array;

		nextsymb ();
		if ((array = rexp ()) == NULL) {
			return NULL;
		}
		a = newdopnode (S_COLON2, locn, a, array, 0);
	}
	return (a);
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *rselection ()*/
PRIVATEPARAM treenode *rselection (void)
{
	int indent = symbindent;
	SOURCEPOSN locn;
	treenode *selroot;
	treenode **selptr;

	DEBUG_MSG (("rselection... "));
	while (symb == S_COMMENT) {
		if (checknlindent (indent)) {
			goto error;
		}
	}
	selptr = &selroot;
	/*{{{  parse selection */
	{
		int reading = TRUE;
		while (reading) {
			treenode *exp;
			locn = flocn;
			linebreakindent = (-1);
			if (!rleadingspecs (selptr, &exp, TRUE)) {
				/*{{{  skip specs */
				while (*selptr != NULL) {
					selptr = DBodyAddr (*selptr);
				}
				/*}}} */
				locn = flocn;	/* bug 873 28/1/91 */
				if (exp != NULL) {
					/*{{{  it was an expression - read rest of selection */
					if (TagOf (exp) != S_LIST) {
						treenode *e = NULL;
						if (symb == S_COMMA) {
							nextsymb ();
							if (checklinebreak ()) {
								return NULL;
							}
							if ((e = rlist (rexp, S_COMMA)) == NULL) {
								nextline ();
								goto error;
							}
						}
						*selptr = newlistnode (S_LIST, locn, exp, e);
					} else {
						*selptr = exp;	/* we have read all the expressions in */
					}
					reading = FALSE;
					/*}}} */
				} else if (symb == S_ELSE) {
					/*{{{  read body of else */
					*selptr = newleafnode (S_ELSE, locn);
					nextsymb ();
					reading = FALSE;
					/*}}} */
				} else {
					synerr_e (SYN_E_EXPR_OR_SPEC, locn, symb);
					nextline ();
				}
			} else {
				nextline ();
				return NULL;
			}
		}
	}
	/*}}} */
	if (checknlindent (indent + 2)) {
		goto error;
	}
	*selptr = newcondnode (S_SELECTION, locn, *selptr, rprocess ());
	return (selroot);
      error:
	skiplines (indent);
	return NULL;
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *rvariant ()*/
PRIVATEPARAM treenode *rvariant (void)
{
	treenode *varroot;
	treenode **varptr;
	int indent = symbindent;

	DEBUG_MSG (("rvariant... "));
	varptr = &varroot;
	/*{{{  parse variant */
	{
		treenode *exp;
		linebreakindent = (-1);
		if (!rleadingspecs (varptr, &exp, FALSE)) {
			SOURCEPOSN locn = flocn;	/* moved for bug 873 28/1/91 */
			/*{{{  skip specs */
			while (*varptr != NULL) {
				varptr = DBodyAddr (*varptr);
			}
			/*}}} */
			if (exp != NULL) {
				/*{{{  read rest of variant */
				treenode *a, *list = NULL;
				if (symb == S_SEMICOLON) {
					/*{{{  parse tagged list */
					nextsymb ();
					if (checklinebreak ()) {
						goto error2;
					}
					if ((list = rlist (rinputitem, S_SEMICOLON)) == NULL) {
						goto error;
					}
					/*}}} */
				}
				a = newlistnode (S_LIST, locn, exp, list);
				if (checknlindent (indent + 2)) {
					goto error2;
				}
				*varptr = newcondnode (S_VARIANT, locn, a, rprocess ());
				return (varroot);
				/*}}} */
			} else {
				synerr_e (SYN_E_EXPR_OR_SPEC, locn, symb);
			}
		}
	      error:
		nextline ();
	      error2:
		skiplines (indent);
		return NULL;
	}
	/*}}} */
}
/*}}}*/
/*{{{  PRIVATEPARAM treenode *rextendedvariant (void) */
PRIVATEPARAM treenode *rextendedvariant (void)
{
	treenode *varroot;
	treenode **varptr;
	int indent = symbindent;
	treenode *exp;

	DEBUG_MSG (("rextendedvariable... "));
	varptr = &varroot;
	linebreakindent = (-1);
	if (!rleadingspecs (varptr, &exp, FALSE)) {
		SOURCEPOSN locn = flocn;

		/*{{{  skip specs */
		while (*varptr != NULL) {
			varptr = DBodyAddr (*varptr);
		}
		/*}}}  */
		if (exp) {
			treenode *a, *d, *l;
			treenode *list = NULL;
			const int cur_indent = indent;

			if (symb == S_SEMICOLON) {
				/*{{{  parse tagged list */
				nextsymb ();
				if (checklinebreak ()) {
					goto error2;
				}
				if ((list = rlist (rinputitem, S_SEMICOLON)) == NULL) {
					goto error;
				}
				/*}}}  */
			}
			l = newlistnode (S_LIST, locn, exp, list);
			if (checknlindent (indent + 2)) {
				goto error2;
			}
			/* parse during process */
			d = rprocess ();
			if (symbindent == (cur_indent + 2)) {
				a = rprocess ();
			} else {
				a = newleafnode (S_SKIP, locn);
			}
			*varptr = newcondnodex (S_X_VARIANT, locn, l, d, a);
			return (varroot);
		} else {
			synerr_e (SYN_E_EXPR_OR_SPEC, locn, symb);
		}
	}
error:
	nextline ();
error2:
	skiplines (indent);
	return NULL;
}
/*}}}*/
/*{{{  PRIVATE treenode *rconstruct (readbody, c, replc)*/
PRIVATE treenode *rconstruct (treenode * (*readbody) (void), int c, int replc)
{
	int indent = lineindent;	/* Indent of first token of construct */
	SOURCEPOSN locn = flocn;	/* Location of start of construct */

	DEBUG_MSG (("rconstruct... "));
	nextsymb ();
#if 0
fprintf (stderr, "syn3: rconstruct: c = %d, replc = %d, symb = %d\n", c, replc, symb);
#endif
	ignore (S_COMMENT);	/* Ignore any trailing comment */

	/* disabled BARRIER declaration in PAR, now means EXTENDS */
#if 0
	if (symb == S_BARRIER) {
		/*{{{  barrier and no replicator*/
		treenode *w, *name;
		treenode *bname, *bextends;

		nextsymb ();
		name = rnamelist ();
		if (!name) {
			goto error;
		}
		/* might also have an EXTENDS bit */
		if (symb == S_EXTENDS) {
			nextsymb ();
			bextends = rnamelist ();
			if (!bextends) {
				goto error;
			}
			bextends = newmopnode (S_EXTENDS, locn, bextends, 0);
		} else {
			bextends = NULL;
		}
		if (checknlindent (indent + 2)) {
			goto error2;
		}
		/* nextline (); */
		w = rproclist (readbody, indent + 2);
		w = newcnode (c, locn, w);
		if (TagOf (name) == S_LIST) {
			treenode *walk = name;

#if 0
fprintf (stderr, "syn3: rconstruct: BARRIER namelist, name = ");
printtreenl (stderr, 4, name);
#endif
			/* turn into a list of declared names */
			while (!EndOfList (walk)) {
				wordnode *thisname = (wordnode *)ThisItem (walk);
				treenode *dname;

				dname = declname (N_DECL, locn, thisname, newleafnode (S_FULLBARRIER, NOPOSN), NULL);
				SetLeft (walk, dname);
				walk = RightOf (walk);
			}
			bname = name;
		} else {
			bname = declname (N_DECL, locn, (wordnode *)name, newleafnode (S_FULLBARRIER, NOPOSN), NULL);
		}
		if (bextends) {
			bname = newdopnode (S_BAREXTEND, locn, bname, bextends, 0);
		}
		SetCTemp (w, bname);

		return w;
		/*}}}*/
	} else
#endif
	if ((symb == S_BARRIER) || (symb == S_ENROLL)) {
		/*{{{  extending a barrier and no replicator*/
		treenode *w, *name;
		treenode *bname;

		nextsymb ();
		name = rnamelist ();
		if (!name) {
			goto error;
		}
		if (checknlindent (indent + 2)) {
			goto error2;
		}
		w = rproclist (readbody, indent + 2);
		w = newcnode (c, locn, w);
		bname = newmopnode (S_EXTENDS, locn, name, 0);
		SetCTemp (w, bname);

		return w;
		/*}}}*/
	} else if (symb == S_NEWLINE) {
		/*{{{  we do not have a replicator */
		treenode *w;

		nextline ();
		w = rproclist (readbody, indent + 2);
		return (newcnode (c, locn, w));
		/*}}} */
	} else {
		/*{{{  we have a replicator */
		treenode *start, *length, *step;
		treenode *nptr, *rcptr;
		treenode *bnode = NULL;
		treenode *bextends = NULL;

		if (rrepl (&nptr, &start, &length, &step)) {
			goto error;
		}
		/* modified: now only parses BARRIER and ENROLL, but that means EXTENDS really */
#if 0
		/* may possibly have either BARRIER or ENROLL */
		if (symb == S_BARRIER) {
			treenode *name;

			nextsymb ();
			name = rnamelist ();
			if (!name) {
				goto error;
			}
			if (TagOf (name) == S_LIST) {
				treenode *walk = name;

				/* turn into a list of declared names */
				while (!EndOfList (walk)) {
					wordnode *thisname = (wordnode *)LeftOf (walk);
					treenode *dname;

					dname = declname (N_DECL, locn, thisname, newleafnode (S_FULLBARRIER, NOPOSN), NULL);
					SetLeft (walk, dname);
					walk = RightOf (walk);
				}
				bnode = name;
			} else {
				bnode = declname (N_DECL, locn, (wordnode *)name, newleafnode (S_FULLBARRIER, NOPOSN), NULL);
			}
			/* might also have EXTENDS bit */
			if (symb == S_EXTENDS) {
				nextsymb ();
				bextends = rnamelist ();
				if (!bextends) {
					goto error;
				}
				bextends = newmopnode (S_EXTENDS, locn, bextends, 0);
			}
		} else
#endif
		if ((symb == S_BARRIER) || (symb == S_ENROLL)) {
			treenode *name;

			nextsymb ();
			name = rnamelist ();
			if (!name) {
				goto error;
			}
			bnode = newmopnode (S_EXTENDS, locn, name, 0);
		}
		if (checknlindent (indent + 2)) {
			goto error2;
		}

		/* Declare replicator variable */
		rcptr = newreplcnode (replc, locn, nptr, start, length, step, NULL);
		SetNDecl (nptr, rcptr);
		/* Increment lex level inside a replicated PAR */
		if (parrepl (replc)) {
			syn_lexlevel++;
		}
		SetReplCBody (rcptr, (*readbody) ());
		if (parrepl (replc)) {
			syn_lexlevel--;
		}

		if (bnode) {
			if (bextends) {
				SetReplCTemp (rcptr, newdopnode (S_BAREXTEND, locn, bnode, bextends, 0));
			} else {
				SetReplCTemp (rcptr, bnode);
			}
		}
		return (rcptr);

	error:
		nextline ();
	error2:
		skiplines (indent);
		return (newcnode (c, locn, NULL));
		/*}}} */
	}
}

/*}}}*/
/*{{{  PRIVATE treenode *raltbody (a, locn, indent)*/
PRIVATE treenode *raltbody (treenode *a, SOURCEPOSN locn, int indent)
{
	/*{{{  COMMENT what we are parsing */
	/*    Parse:
	 *	(  [ boolean '&' ] ( channel '?' {1 ';' input.item }  |
	 *	timer '?' 'AFTER' expression     |
	 *	channel '?' 'CASE' tagged.list   |
	 *	timer '?' variable               |
	 *	port '?' variable                )  |
	 *	boolean '&' 'SKIP'                                    )
	 *	  process                                                   |
	 *	[ boolean '&' ] channel '?' 'CASE'
	 *	  { variant }
	 *	channel '??' variable            |
	 *	[ boolean '&' ] channel '??' {1 ';' input.item }
	 *	[ boolean '&' ] channel '??' 'CASE' tagged.list
	 *	  process
	 *	  process
	 *	[ boolean '&' ] channel '??' 'CASE'
	 *	  { extended.variant }
	 */
	/*}}} */
	treenode *input = NULL;
	treenode *guard = NULL;
	BOOL is_extended = FALSE;
	treenode *target;

#if 0
fprintf (stderr, "raltbody: is_extended = %d, a is:", is_extended);
printtreenl (stderr, 4, a);
#endif
	/*{{{  a was the boolean, so now read in the channel into a */
	if (symb == S_AMPERSAND) {
		guard = a;
		nextsymb ();

		if (symb == S_SKIP) {
			/*{{{  there is no channel in the alt */
			nextsymb ();
			if (checknlindent (indent + 2)) {
				return NULL;
			}
			return (newaltnode (S_ALTERNATIVE, locn, guard, newleafnode (S_SKIP, locn), rprocess ()));
			/*}}} */
		} else {
			/*{{{  parse input */
			if ((a = relement ()) == NULL) {
				goto error;
			}
			/*}}} */
		}
	}
	/*}}} */
	/*{{{  read input */
	{
		SOURCEPOSN ilocn = flocn;
		BOOL skip_symb = FALSE;

		if (extended_input && (symb != S_INPUT) && (TagOf (a) == S_DQUESTIONM)) {
			treenode *list;

			/* have something like:
			 *	channel '??' something
			 * currently on what's after `something'
			 */
			is_extended = TRUE;

			if (symb == S_SEMICOLON) {
				nextsymb ();
				list = newlistnode (S_LIST, ilocn, RHSOf (a), rlist (rinputitem, S_SEMICOLON));
			} else {
				list = newlistnode (S_LIST, ilocn, RHSOf (a), NULL);
			}
			input = newactionnode (S_INPUT, ilocn, LHSOf (a), list);
			checknewline ();
			skip_symb = TRUE;
		} else if (extended_input && (symb == S_DQUESTIONM)) {
			/* have something like:
			 *	boolean & channel ?? something
			 * currently on the '??'
			 */
			is_extended = TRUE;
			nextsymb ();
		} else if (checkfor (S_INPUT)) {
			goto error;
		}
		if (!skip_symb) {
#if 0
fprintf (stderr, "*** HERE! (is_extended = %d, symb = %d)\n", is_extended, symb);
#endif
			switch (symb) {
				/*{{{  case S_CASE */
			case S_CASE:
				/*  We should have
				 *      [ boolean '&' ] channel '?' 'CASE' tagged.list         |
				 *      [ boolean '&' ] channel '?' 'CASE'
				 *      { variant }
				 * and we have got as far as the 'CASE'
				 */
				nextsymb ();
				if ((symb == S_COMMENT) || (symb == S_NEWLINE)) {
					/*{{{  look for variant list */
					treenode *vl;

					checknewline ();
					if (is_extended) {
						vl = rproclist (rextendedvariant, indent + 2);
						input = newactionnode (S_X_CASE_INPUT, ilocn, a, vl);
					} else {
						vl = rproclist (rvariant, indent + 2);
						input = newactionnode (S_CASE_INPUT, ilocn, a, vl);
					}
					return (newaltnode (S_ALTERNATIVE, locn, guard, input, NULL));
					/* Get out early because there is no process following a
					 * case input alternative */
					/*}}} */
				} else {
					/*{{{  look for tagged list */
					treenode *list;

					if ((list = rlist (rinputitem, S_SEMICOLON)) == NULL) {
						goto error;
					}
					input = newactionnode (S_TAGGED_INPUT, ilocn, a, list);
					checknewline ();
					/*}}} */
				}
				break;
				/*}}} */
				/*{{{  case S_AFTER */
			case S_AFTER:
				{
					treenode *exp;

					nextsymb ();
					if ((exp = rexp ()) == NULL) {
						goto error;
					}
					input = newactionnode (S_DELAYED_INPUT, ilocn, a, exp);
					checknewline ();
					break;
				}
				/*}}} */
				/*{{{  default */
			default:
				{
					treenode *list;

					if ((list = rlist (rinputitem, S_SEMICOLON)) == NULL) {
						goto error;
					}
					input = newactionnode (S_INPUT, ilocn, a, list);
					checknewline ();
				}
				/*}}} */
			}
		}
	}
	/*}}} */
	if (checkindent (indent + 2)) {
		return NULL;
	}
	if (is_extended) {
		const int cur_indent = indent;
		treenode *d_proc, *a_proc;

		d_proc = rprocess ();
		if (symbindent == (cur_indent + 2)) {
			a_proc = rprocess ();
		} else {
			a_proc = newleafnode (S_SKIP, locn);
		}
		switch (TagOf (input)) {
		case S_INPUT:
			SetTag (input, S_X_INPUT);
			SetActionDuring (input, d_proc);
			SetActionAfter (input, a_proc);
			SetActionFlags (input, ActionFlagsOf (input) | ActionFlag_skip_xable);
			break;
		case S_TAGGED_INPUT:
			SetTag (input, S_X_TAGGED_INPUT);
			SetActionDuring (input, d_proc);
			SetActionAfter (input, a_proc);
			SetActionFlags (input, ActionFlagsOf (input) | ActionFlag_skip_xable);
			break;
		default:
			/* ooops.. */
			goto error;
		}
		target = newaltnode (S_ALTERNATIVE, locn, guard, input, newleafnode (S_SKIP, locn));
	} else {
		target = newaltnode (S_ALTERNATIVE, locn, guard, input, rprocess ());
	}
#if 0
fprintf (stderr, "raltbody: target =");
printtreenl (stderr, 4, target);
#endif
	return target;

error:
	/*{{{  recover from error */
	{
		nextline ();
		skiplines (indent);
		return NULL;
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *ralternative ()*/
PRIVATEPARAM treenode *ralternative (void)
{
	int indent = symbindent;
	treenode *altroot;
	treenode **altptr;

	DEBUG_MSG (("ralternative... "));
	altptr = &altroot;
	/*{{{  parse the alternative */
	{
		treenode *exp;

		linebreakindent = (-1);
#if 0
fprintf (stderr, "ralternative: about to parse leading specifications\n");
#endif
		if (!rleadingspecs (altptr, &exp, FALSE)) {
			SOURCEPOSN locn = flocn;	/* moved for bug 873 28/1/91 */

#if 0
fprintf (stderr, "ralternative: leading specs OK\n");
#endif
			/*{{{  skip specs */
			while (*altptr != NULL) {
				altptr = DBodyAddr (*altptr);
			}
			/*}}}  */
			if (exp != NULL) {
				/*{{{  read rest of alternative */
				*altptr = raltbody (exp, locn, indent);
				return (altroot);
				/*}}}  */
			} else if ((symb == S_ALT) || (symb == S_PRI)) {
				/*{{{  another alt */
				int replsymb;

				if (symb == S_PRI) {
					/*{{{  check for ALT */
					nextsymb ();
					if (symb != S_ALT) {
						synetoken (S_ALT);
						goto error;
					}
					symb = S_PRIALT;
					replsymb = S_PRIREPLALT;
					/*}}}  */
				} else {
					replsymb = S_REPLALT;
				}
				*altptr = rconstruct (ralternative, symb, replsymb);
				return (altroot);
				/*}}}  */
			} else if (symb == S_SKIP) {
				/*{{{  SKIP guard without pre-condition */
				nextsymb ();
				if (!checknlindent (indent + 2)) {
					*altptr = newaltnode (S_ALTERNATIVE, locn, NULL, newleafnode (S_SKIP, locn), rprocess ());
					return altroot;
				} else {
					return NULL;
				}
				/*}}}  */
			} else {
				synerr_e (SYN_E_EXPR_OR_SPEC, locn, symb);
			}
		}
error:
		nextline ();
		return NULL;
	}
	/*}}}  */
}

/*}}}*/
/*{{{  PRIVATE treenode *rchoicebody (a, locn, indent)*/
/* Parse
	boolean
	process
 * the boolean is already in a
 */
PRIVATE treenode *rchoicebody (treenode * a, SOURCEPOSN locn, int indent)
{
	DEBUG_MSG (("rchoicebody... "));
	if (checknlindent (indent + 2)) {
		skiplines (indent);
		return NULL;
	}
	return (newcondnode (S_CHOICE, locn, a, rprocess ()));
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *rchoice ()*/
/*{{{  what we are parsing*/
/* Parse
 *	boolean
 *	process
 *	| conditional
 *	| specification
 *	choice
 */
/*}}}*/
PRIVATEPARAM treenode *rchoice (void)
{
	int indent = symbindent;
	treenode *choiceroot;	/* Root of the tree built up */
	treenode **chptr;	/* Pointer to next hole in the tree */

	DEBUG_MSG (("rchoice... "));
	chptr = &choiceroot;
	/*{{{  parse choice */
	{
		treenode *exp;
		linebreakindent = (-1);
		if (!rleadingspecs (chptr, &exp, FALSE)) {
			SOURCEPOSN locn = flocn;	/* moved for bug 873 28/1/91 */
			/*{{{  look for end of specs */
			while (*chptr != NULL) {
				chptr = DBodyAddr (*chptr);
			}
			/*}}} */
			if (exp != NULL) {
				/*{{{  read choice body */
				*chptr = rchoicebody (exp, locn, indent);
				return (choiceroot);
				/*}}} */
			} else if (symb == S_IF) {
				*chptr = (rconstruct (rchoice, S_IF, S_REPLIF));
				return (choiceroot);
			} else {
				synerr_e (SYN_E_EXPR_OR_SPEC, locn, symb);
			}
		}
		nextline ();
		return NULL;
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE treenode *raction_symb (treenode *lhs, int actsymb, SOURCEPOSN locn, int indent)*/
PRIVATE treenode *raction_symb (treenode *lhs, int actsymb, SOURCEPOSN locn, int indent)
{
	treenode *n = NULL, *rhs;

	DEBUG_MSG (("raction_symb... "));
	switch (actsymb) {
		/*{{{  case S_ASS*/
	case S_ASS:
		if (checklinebreak ()) {
			return NULL;
		}
		if (TagOf (lhs) == S_LIST) {
#ifdef MOBILES
			/* might find MOBILE keyword now */
			if (symb == S_MOBILE) {
				treenode *name1 = ThisItem (lhs);
				treenode *name2 = ThisItem (NextItem (lhs));
				wordnode *instance_name = lookupword ("ALLOC.CHAN.TYPE", 15);
				treenode *params;

				/* something of the form "x, y := MOBILE type" */
				nextsymb ();
				if ((rhs = (treenode *)rname ()) == NULL) {
					/* expected chan type name */
					synerr_e (SYN_E_CHANTYPE, locn, symb);
					goto error;
				}
				checknewline ();
				/* okay, generate an instance-node of "ALLOC.CHAN.TYPE" */
				params = newlistnode (S_LIST, NOPOSN, name1,
						newlistnode (S_LIST, NOPOSN, name2,
						newlistnode (S_LIST, NOPOSN, rhs, NULL)));
				return newinstancenode (S_PINSTANCE, locn, (treenode *)instance_name, params);
			}
#endif	/* MOBILES */
			rhs = rlist (rexp, S_COMMA);
		} else {
			rhs = rexp ();
		}
		if (!rhs) {
			goto error;
		}
		checknewline ();
		n = newactionnode (S_ASS, locn, lhs, rhs);
		break;
		/*}}}  */
		/*{{{  case S_OUTPUT*/
	case S_OUTPUT:
		/*{{{  check only one element on lhs */
		if (TagOf (lhs) == S_LIST) {
			synerr (SYN_LIST_IN_IO, locn);
			goto error;
		}
		/*}}} */
		if ((n = rlist (routputitem, S_SEMICOLON)) == NULL) {
			goto error;
		}
		n = newactionnode (S_OUTPUT, locn, lhs, n);
		checknewline ();
		break;
		/*}}}  */
		/*{{{  case S_INPUT*/
	case S_INPUT:
		/*{{{  check only one element on lhs */
		if (TagOf (lhs) == S_LIST) {
			synerr (SYN_LIST_IN_IO, locn);
			goto error;
		}
		/*}}} */
		switch (symb) {
			/*{{{  tagged or case input*/
		case S_CASE:
			nextsymb ();
			if ((symb == S_COMMENT) || (symb == S_NEWLINE)) {
				/*{{{  variant list*/
				treenode *vl;

				checknewline ();
				vl = rproclist (rvariant, indent + 2);
				n = newactionnode (S_CASE_INPUT, locn, lhs, vl);
				/*}}}  */
			} else {
				if ((n = rlist (rinputitem, S_SEMICOLON)) == NULL) {
					goto error;
				}
				n = newactionnode (S_TAGGED_INPUT, locn, lhs, n);
				checknewline ();
			}
			break;
			/*}}}  */
			/*{{{  delayed input*/
		case S_AFTER:
			nextsymb ();
			if ((n = rexp()) == NULL) {
				goto error;
			}
			n = newactionnode (S_DELAYED_INPUT, locn, lhs, n);
			checknewline ();
			break;
			/*}}}  */
			/*{{{  default*/
		default:
			if ((n = rlist (rinputitem, S_SEMICOLON)) == NULL) {
				goto error;
			}
			n = newactionnode (S_INPUT, locn, lhs, n);
			checknewline ();
			break;
			/*}}}  */
		}
		break;
		/*}}}  */
		/*{{{  case DQUESTIONM -- extended input*/
	case S_DQUESTIONM:
		if (extended_input) {
			/*{{{  check only one element on lhs */
			if (TagOf (lhs) == S_LIST) {
				synerr (SYN_LIST_IN_IO, locn);
				goto error;
			}
			/*}}} */
			switch (symb) {
			case S_CASE:
				nextsymb ();
				if ((symb == S_COMMENT) || (symb == S_NEWLINE)) {
					/*{{{  look for variant list */
					treenode *vl;

					checknewline ();
					vl = rproclist (rextendedvariant, indent + 2);
					n = newactionnode (S_X_CASE_INPUT, locn, lhs, vl);
					/*}}}  */
				} else {
					/*{{{  single variant*/
					treenode *b, *a;
					const int cur_indent = indent;

					if ((n = rlist (rinputitem, S_SEMICOLON)) == NULL) {
						goto error;
					}
					checknewline ();
					if (checkindent (cur_indent + 2)) {
						return NULL;
					}
					b = rprocess ();
					if (symbindent == (cur_indent + 2)) {
						a = rprocess ();
					} else {
						a = newleafnode (S_SKIP, locn);
					}
					n = newactionnodex (S_X_TAGGED_INPUT, locn, lhs, n, b, a);
					/*}}}  */
				}
				break;
			default:
				{
					treenode *b, *a;
					const int cur_indent = indent;

					n = rlist (rinputitem, S_SEMICOLON);
					if (!n) {
						goto error;
					}
					checknewline ();
					/* expect process indented underneath here */
					if (checkindent (cur_indent + 2)) {
						return NULL;
					}
					b = rprocess ();
					/* following "after" process ? */
					if (symbindent == (cur_indent + 2)) {
						a = rprocess ();
					} else {
						a = newleafnode (S_SKIP, locn);
					}
					n = newactionnodex (S_X_INPUT, locn, lhs, n, b, a);
				}
				break;
			}
			break;	/* out of switch() */
		}
		/*}}}  */
		/*{{{  default (fallen through from above)*/
	default:
		synerr_e (SYN_E_ACTION, locn, symb);
	error:
		nextline ();
		skiplines (indent);
		n = NULL;
		break;
		/*}}}*/
	}
	return n;
}
/*}}}  */
/*{{{  PRIVATE treenode *raction (lhs, locn, indent)*/
/* Syntax does not specify a case.input as an action, but as action is
 * only used once in syntax, we test for it here anyway                   */
PRIVATE treenode *raction (treenode * lhs, SOURCEPOSN locn, int indent)
{
	int csymb = symb;

	nextsymb ();
	return raction_symb (lhs, csymb, locn, indent);
}

/*}}}*/
/*{{{  PRIVATE wordnode *lookuplabel (char *labelname)*/
/*****************************************************************************
 *
 *  lookuplabel takes a string 'labelname' and looks it up as a label,
 *              ie. it prepends a ':' character and looks up this name.
 *
 *****************************************************************************/
PRIVATE wordnode *lookuplabel (wordnode * labelname)
{
	char n[MAXNAMELENGTH + 1];

	n[0] = ':';
	memcpy (&n[1], WNameOf (labelname), WLengthOf (labelname));
	return lookupword (n, WLengthOf (labelname) + 1);
}

/*}}}*/
/*{{{  PRIVATE treenode *rasmexp ()*/
/*****************************************************************************
 *
 *  rasmexp reads an expression, or ADDRESSOF expression
 *
 ******************************************************************************/
PRIVATE treenode *rasmexp (void)
{
	if (symb == S_NAME && (strcmp (WNameOf (lexword), "ADDRESSOF") == 0)) {
		nextsymb ();
		return (newmopnode (S_ADDRESSOF, flocn, rexp (), 0));
	}
	return (rexp ());
}

/*}}}*/
/*{{{  PRIVATE treenode *rguy_or_asm ()*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  rguy_or_asm reads a line of GUY or ASM code.
 *
 *    asmline :== instruction {, asm-expression }
 *                primaryname ':' labelname |
 *                ':' labelname
 *
 *    guyline :== primaryname expression |
 *                primaryname '.' labelname |
 *                ':' labelname |
 *                'STEP' secondaryname |
 *                secondaryname
 *
 *****************************************************************************/
/*}}}*/
PRIVATE treenode *rguy_or_asm (const BOOL guy_not_asm)
{
	SOURCEPOSN locn = flocn;
	int guy_tag = S_GUYCODE;

	/*{{{  check for STEP */
	{
		static wordnode *STEP_word = NULL;
		if (STEP_word == NULL) {
			STEP_word = lookupword ("STEP", 4);
		}

		if (guy_not_asm && (symb == S_NAME) && (lexword == STEP_word)) {
			guy_tag = S_GUYSTEP;
			nextsymb ();
		}
	}
	/*}}} */
#if 0
fprintf (stderr, "syn3: rguy_or_asm(): guy_not_asm = %d, symb = %d\n", (int)guy_not_asm, symb);
#endif
	switch (symb) {
		/*{{{  S_NAME ALT, AND, IN, NOT, OR, REM, NULL: primary or secondary instruction */
	case S_NAME:
	case S_ALT:
	case S_AND:
	case S_IN:
	case S_NOT:
	case S_OR:
	case S_REM:
	case S_BYTE:
		{
			wordnode *instruction;
			treenode *operand = NULL;
			BOOL ldlabeldiff;
			static wordnode *LDLABELDIFF_word = NULL;

			if (LDLABELDIFF_word == NULL) {
				LDLABELDIFF_word = lookupword ("LDLABELDIFF", 11);
			}

			ldlabeldiff = (lexword == LDLABELDIFF_word);

			{
				/* We hide the name behind a dot so that the resulting node MUST be
				 * a real S_NAME node, so that later treewalks don't get confused,
				 * when traversing the left hand node of a dopnode */
				char instname[MAXSTRING_SIZE + 1];

				instname[0] = '.';	/* strcpy(instname, "."); */
				/* strcat(instname, WNameOf(lexword)); */
				memcpy (&instname[1], WNameOf (lexword), WLengthOf (lexword));

				instruction = lookupword (instname, WLengthOf (lexword) + 1);
			}

			nextsymb ();
			ignore (S_COMMENT);
			if (symb != S_NEWLINE) {
				/*{{{  read the operand */
				if (ldlabeldiff) {
					/*{{{  :label1 - :label2 */
					wordnode *label1, *label2;

					if (checkfor (S_COLON)) {
						goto error;
					}
					if (symb != S_NAME) {
						/*{{{  report error, abort */
						synerr_e (SYN_E_NAME, locn, symb);
						goto error;
						/*}}} */
					}
					label1 = lookuplabel (lexword);
					nextsymb ();
					if (checkfor (S_SUBTRACT)) {
						goto error;
					}
					if (checkfor (S_COLON)) {
						goto error;
					}
					if (symb != S_NAME) {
						/*{{{  report error, abort */
						synerr_e (SYN_E_NAME, locn, symb);
						goto error;
						/*}}} */
					}
					label2 = lookuplabel (lexword);
					nextsymb ();
					operand = newlistnode (S_LIST, locn, (treenode *) label1,
							       newlistnode (S_LIST, locn, (treenode *) label2, NULL));
					/*}}} */
				} else if (symb == (guy_not_asm ? S_DOT : S_COLON)) {
					nextsymb ();
					if (symb != S_NAME) {
						/*{{{  report error, abort */
						synerr_e (SYN_E_NAME, locn, symb);
						goto error;
						/*}}} */
					}
					operand = newlistnode (S_LIST, locn, (treenode *) lookuplabel (lexword), NULL);
					nextsymb ();
				} else {
					operand = rlist (guy_not_asm ? rexp : rasmexp, S_COMMA);
					if (operand == NULL) {
						goto error;
					}
				}
				/*}}} */
			}
			checknewline ();

			return newdopnode (guy_tag, locn, (treenode *) instruction, operand, 0);
		}
		/*}}} */
		/*{{{  S_COLON  label definition */
	case S_COLON:
		{
			wordnode *labelname;

			nextsymb ();
			if (symb != S_NAME) {
				synerr_e (SYN_E_NAME, flocn, symb);
				goto error;
			}
			labelname = lookuplabel (lexword);
			nextline ();

			return declare (S_LABELDEF, locn, newleafnode (S_LABEL, locn), labelname, NULL);
		}
		/*}}} */
		/*{{{  INCLUDE etc */
	case S_INCLUDE:
	case S_SC:
	case S_USE:
	case S_IMPORT:
		{
			int indent = symbindent;

			if (!rfile ()) {
				skiplines (indent);
				if (lineindent < indent) {
					return NULL;
				}
			}
		}

		return rguy_or_asm (guy_not_asm);
		/*}}} */
	default:
		synerr (SYN_BAD_GUYLINE, flocn);
		goto error;
		break;
	}
error:
	nextline ();
	return NULL;
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *rguy ()*/
PRIVATEPARAM treenode *rguy (void)
{
	return rguy_or_asm (TRUE);
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *rasm ()*/
PRIVATEPARAM treenode *rasm (void)
{
	treenode *t;

	allow_asmnames = TRUE;
	t = rguy_or_asm (FALSE);
	allow_asmnames = FALSE;
	return t;
}

/*}}}*/
/*{{{  PUBLIC treenode *rprocess ()*/
PRIVATE int base_lexlevel;

PUBLIC treenode *rprocess (void)
{
	treenode *a;
	treenode *procroot;
	treenode **procptr = &procroot;

	a = NULL;		/* keep compiler happy */
	while (TRUE) {
		const SOURCEPOSN locn = flocn;	/* Location of first symbol of process */
		const int indent = symbindent;

		linebreakindent = (-1);
		DEBUG_MSG (("rprocess: symb is %s... ", tagstring (symb)));
		/*{{{  switch on current symb */
		if (mustbespec (symb) || isscalartype (symb)) {
			/*{{{  read specification */
			*procptr = rspecification ();
			if ((*procptr) == NULL) {
				goto error2;
			}
			procptr = DBodyAddr (*procptr);
			/*}}} */
		} else if (symb == S_FORK) {
			/*{{{  FORK -- must be instance*/
			treenode *pname, *forkcall;

			/* special case, expect a PROC instance next */
			nextsymb ();
			if ((pname = (treenode *)rname ()) == NULL) {
				synerr_e (SYN_E_FORK_PROCCALL, flocn, symb);
				goto error;
			}
			if (symb == S_LPAREN) {
				forkcall = rinstance (S_PINSTANCE, locn, pname);
				checknewline ();
				if (forkcall && (TagOf (forkcall) == S_PINSTANCE)) {
					SetIForked (forkcall, 1);
				}
				*procptr = forkcall;
				return (procroot);
			} else {
				synerr_e (SYN_E_FORK_PROCPARAMS, flocn, symb);
				goto error;
			}
			/*}}}*/
		} else if (symb == S_DYNCALL) {
			/*{{{  DYNCALL -- must be instance*/
			treenode *pname, *dyncall;

			/* special case, expect a PROC instance next */
			nextsymb ();
			if ((pname = (treenode *)rname ()) == NULL) {
				synerr_e (SYN_E_DYNCALL_PROCCALL, flocn, symb);
				goto error;
			}
			if (symb == S_LPAREN) {
				dyncall = rinstance (S_PINSTANCE, locn, pname);
				if (symb == S_AT) {
					treenode *calladdr;

					/* special case, allow dynamic call at a specific address */
					/* FIXME: drop some marker that says this is basically insecure */
					nextsymb ();
					calladdr = rexp ();

#if 0
fprintf (stderr, "rprocess(): DYNCALL, calladdr expr =\n");
printtreenl (stderr, 4, calladdr);
#endif
					if ((TagOf (dyncall) == S_PINSTANCE) || (TagOf (dyncall) == S_FINSTANCE)) {
						SetIDynaddr (dyncall, calladdr);
					}
				}
				checknewline ();
				if (dyncall && (TagOf (dyncall) == S_PINSTANCE)) {
					SetIDynmem (dyncall, 1);
				}
				*procptr = dyncall;
				return (procroot);
			} else {
				synerr_e (SYN_E_DYNCALL_PROCPARAMS, flocn, symb);
				goto error;
			}
			/*}}}*/
		} else if ((symb == S_NAME) || (symb == S_LBOX)) {
			/*{{{  S_NAME, S_LBOX   --  specification or action */
			BOOL specflag;

			if ((a = rspecorelement (&specflag, indent)) == NULL) {
				goto error2;
			}
			if (specflag) {
				*procptr = a;
				procptr = DBodyAddr (a);
			} else {
				if (symb == S_LPAREN) {
					/*{{{  parse procedure call; return */
					*procptr = rinstance (S_PINSTANCE, locn, a);
					checknewline ();
					return (procroot);
					/*}}} */
				} else {
					BOOL waslist = FALSE;

					/*{{{  parse variable or variable list followed by action; return */
					if (symb == S_COMMA) {
						/*{{{  parse list of names if present*/
						treenode *list;

						nextsymb ();
						if (checklinebreak ()) {
							goto error2;
						}
#ifdef OCCAM2_5
						{
							BOOL found_spec = FALSE;
							list = rspecorelementlist (&found_spec, indent);
						}
#else /* !OCCAM2_5 */
						list = rlist (relement, S_COMMA);
#endif /* !OCCAM2_5 */
						if (list == NULL) {
							goto error1;
						}
						a = newlistnode (S_LIST, locn, a, list);
						waslist = TRUE;
						/*}}}*/
					}

#ifdef OCCAM2_5
					if ((symb == S_FUNCTION) || (symb == S_INLINE)) {
						/*{{{  parsing function definition*/
						*procptr = rfunctiondef (a, locn, indent);
						if ((*procptr) == NULL) {
							goto error2;
						}
						procptr = DBodyAddr (*procptr);
						/*}}}*/
#ifdef MOBILES
					} else if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
						/* check for channel-type variables (decl looks like "name[?!] {1,name} :") */
						/* FIXME: need to be able to handle [const]FOO? x?" flavoured things as well..? */
						int csymb = symb;
						treenode *namelist = NULL;
						BOOL is_declaration = FALSE;

						lex_save_state ();
						/*{{{  look forward a bit to see if it's a declaration*/
						nextsymb ();
						if (symb == S_NAME) {
							nextsymb ();
							if ((symb == S_COMMA) || (symb == S_COLON) || (symb == S_IS)) {
								is_declaration = TRUE;
#if 0
fprintf (stderr, "syn3: well, looks like a chan-type declaration!\n");
#endif
							}
						}
						/*}}}*/
						lex_restore_state ();

						if (is_declaration) {
							/* it _must_ be a declaration line (splitting not allowed at the moment then!) */
							/* have parsed "name[?!] " and know about the : or IS on the end */
							nextsymb ();

							while ((symb != S_COLON) && (symb != S_IS)) {
								treenode *name;
								
								if ((name = (treenode *)rname ()) == NULL) {
									synerr_e (SYN_E_NAME, locn, symb);
									goto error;
								}
								namelist = newlistnode (S_LIST, locn, name, namelist);

								if (symb == S_COMMA) {
									nextsymb ();
								} else if ((symb != S_COLON) && (symb != S_IS)) {
									synerr_e (SYN_E_COLON, locn, symb);
									goto error;
								}
							}
							/* need to set channel direction specification in type */
							if (csymb == S_INPUT) {
								a = newmopnode (S_ASINPUT, locn, a, S_CHAN);
							} else {
								a = newmopnode (S_ASOUTPUT, locn, a, S_CHAN);
							}
							if ((symb == S_IS) && (TagOf (namelist) == S_LIST)) {
								/* need to reduce this to a single name */
								treenode *name = ThisItem (namelist);

								namelist = NextItem (namelist);
								if (!EndOfList (namelist)) {
									synerr (SYN_TOO_MANY_NAMES, locn);
								}
								namelist = name;
							}
							*procptr = rrestofspec (a, namelist, locn, indent, TRUE);
							procptr = DBodyAddr (*procptr);
						} else {
							nextsymb ();

							/* Something did used to be here ;) -- hence raction_symb(), but I removed it.. */
							*procptr = raction_symb (a, csymb, locn, indent);
							if ((*procptr) == NULL) {
								goto error2;
							}
							return (procroot);
						}
#endif
					} else
#endif
					{
						*procptr = raction (a, locn, indent);
						if ((*procptr) == NULL) {
							goto error2;
						}
						return (procroot);
					}
					/*}}} */
				}
			}
			/*}}} */
		} else {
			/* Read process */
			switch (symb) {
				/*{{{  S_INITIAL */
#ifdef INITIAL_DECL
			case S_INITIAL:
				if (initial_decl) {
					*procptr = rinitial ();
					if (*procptr == NULL) {
						goto error2;
					}
					return (procroot);
				}
				goto error1;
#endif /* INITIAL_DECL */
				/*}}} */
				/*{{{  S_RESULT */
			case S_RESULT:
				*procptr = rresultabbr ();
				if (*procptr == NULL) {
					goto error2;
				} else if (!result_abbr_var) {
					/* got a declnode back, fixup into procptr */
				}
				return (procroot);
				/*}}}  */
				/*{{{  S_STOP, S_SKIP, S_SUSPEND */
			case S_STOP:
			case S_SKIP:
			case S_SUSPEND:
				*procptr = newleafnode (symb, locn);
				nextsymb ();
				checknewline ();
				return (procroot);
				/*}}} */
				/*{{{  S_SYNC*/
			case S_SYNC:
				{
					treenode *name;

					*procptr = newleafnode (S_SYNC, locn);
					nextsymb ();
					if (!(name = (treenode *)rname ())) {
						goto error;
					}
					checknewline ();
					SetLeafLink (*procptr, name);

					return procroot;
				}
				/*}}}*/
				/*{{{  S_SEQ */
			case S_SEQ:
				*procptr = rconstruct (rprocess, S_SEQ, S_REPLSEQ);
				return (procroot);
				/*}}} */
				/*{{{  S_IF */
			case S_IF:
				*procptr = rconstruct (rchoice, S_IF, S_REPLIF);
				return (procroot);
				/*}}} */
				/*{{{  S_PAR */
			case S_PAR:
				*procptr = rconstruct (rprocess, S_PAR, S_REPLPAR);
				return (procroot);
				/*}}} */
				/*{{{  S_DO */
			case S_DO:
				*procptr = rconstruct (rprocess, S_DO, S_REPLDO);
				return (procroot);
				/*}}} */
				/*{{{  S_PRI */
			case S_PRI:
				{
					nextsymb ();
					if (symb == S_PAR) {
						*procptr = rconstruct (rprocess, S_PRIPAR, S_PRIREPLPAR);
					} else if (symb == S_ALT) {
						*procptr = rconstruct (ralternative, S_PRIALT, S_PRIREPLALT);
					} else {
						/*{{{  error */
						synerr_e (SYN_E_PAR_OR_ALT, flocn, symb);
						goto error;
						/*}}} */
					}
				}
				return (procroot);
				/*}}} */
				/*{{{  S_PLACED S_PROCESSOR */
			case S_PLACED:
				nextsymb ();
				switch (symb) {
				case S_PAR:
					/* traditional PLACED PAR construct */
					*procptr = rconstruct (rprocess, S_PLACEDPAR, S_PLACEDREPLPAR);
					return procroot;
				case S_CHAN:
					/* special PLACED CHAN (user-defined channel) construct */
					*procptr = rplacedchan ();
					if (!*procptr) {
						goto error2;
					}
					procptr = DBodyAddr (*procptr);		/* step to PLACE node */
					procptr = DBodyAddr (*procptr);		/* step to where process attaches */
					break;
				case S_LBOX:
					/* placed ARRAY construct -- included for RMoX */
					*procptr = rplacedarray ();
					if (!*procptr) {
						goto error2;
					}
					procptr = DBodyAddr (*procptr);		/* step to PLACE node */
					procptr = DBodyAddr (*procptr);		/* step to where process attaches */
					break;
				default:
					/* placed data construct -- included for RMoX */
					*procptr = rplaceddecl ();
					if (!*procptr) {
						goto error2;
					}
					procptr = DBodyAddr (*procptr);		/* step to PLACE node */
					procptr = DBodyAddr (*procptr);		/* step to where process attaches */
					break;
				}
				break;
			case S_PROCESSOR:
				/**procptr = rplacement();*/
				*procptr = rprocessor ();
				return (procroot);
				/*}}} */
				/*{{{  S_ALT */
			case S_ALT:
				*procptr = rconstruct (ralternative, S_ALT, S_REPLALT);
				return (procroot);
				/*}}} */
#ifdef MOBILES
				/*{{{  S_CLAIM*/
			case S_CLAIM:
				{
					const int saved_indent = symbindent;
					treenode *name, *p;
					int csymb = S_UNDEFINED;

					nextsymb ();
					if ((name = rexp ()) == NULL) {
						synerr_e (SYN_E_OPERAND, locn, symb);
						goto error;
					}
					/* we may have a direction specifier here, if we're dealing with an anonymous channel-type */
					if (symb == S_OUTPUT) {
						csymb = symb;
						name = newmopnode (S_ASOUTPUT, locn, name, S_CHAN);
						nextsymb ();
					} else if (symb == S_INPUT) {
						csymb = symb;
						name = newmopnode (S_ASINPUT, locn, name, S_CHAN);
						nextsymb ();
					}

					if (symb == S_COMMENT) {
						/* skip comment here if we have one */
						nextsymb ();
					}

					if (symb != S_NEWLINE) {
						/* might have a single input or output process (RHS) here */
						SOURCEPOSN claimlocn = locn;
						treenode *namecopy;
						treenode *pr;
						
						if (csymb == S_UNDEFINED) {
							/* attempt to extract INPUT/OUTPUT from name before copy */
							if (TagOf (name) == S_ASINPUT) {
								csymb = S_INPUT;
							} else if (TagOf (name) == S_ASOUTPUT) {	
								csymb = S_OUTPUT;
							} else {
								goto error2;
							}
							namecopy = copytree (OpOf (name), syn_lexlevel);
						} else {
							namecopy = copytree (name, syn_lexlevel);
						}
#if 0
fprintf (stderr, "rprocess(): in CLAIM, name =");
printtreenl (stderr, 4, name);
#endif
						pr = raction_symb (namecopy, csymb, locn, indent);

						if (!pr) {
							goto error2;
						}
						p = newcnode (S_CLAIM, claimlocn, newlistnode (S_LIST, NOPOSN, pr, NULL));
					} else {
						/* might actually have a subscript expression here or something.. */
						if (checknlindent (saved_indent + 2)) {
							goto error2;
						}
						p = newcnode (S_CLAIM, locn, newlistnode (S_LIST, NOPOSN, rprocess (), NULL));
					}
					*procptr = p;
					SetCTemp (p, name);
					return (procroot);
				}
				/*}}}*/
#endif
				/*{{{  S_FORKING*/
			case S_FORKING:
				{
					const int saved_indent = symbindent;

					nextsymb ();
					if (checknlindent (saved_indent + 2)) {
						goto error2;
					}
					*procptr = newcnode (S_FORKING, locn, newlistnode (S_LIST, NOPOSN, rprocess(), NULL));
					return (procroot);
				}
				/*}}}*/
				/*{{{  S_RESIGN*/
			case S_RESIGN:
				{
					const int saved_indent = symbindent;
					treenode *name, *p;

					nextsymb ();
					if ((name = (treenode *)rname ()) == NULL) {
						goto error;
					}
					if (checknlindent (saved_indent + 2)) {
						goto error2;
					}
					p = rprocess ();
					*procptr = newcnode (S_RESIGN, locn, newlistnode (S_LIST, NOPOSN, p, NULL));
					SetCTemp (*procptr, name);

					return procroot;
				}
				/*}}}*/
				/*{{{  S_WHILE */
			case S_WHILE:
				{
					const int saved_indent = symbindent;

					nextsymb ();
					if ((a = rexp ()) == NULL) {
						goto error;
					}
					if (checknlindent (saved_indent + 2)) {
						goto error2;
					}
					*procptr = newcondnode (S_WHILE, locn, a, rprocess ());	/* Read the body */
					return (procroot);
				}
				/*}}} */
				/*{{{  S_CASE */
			case S_CASE:
				/* Parse
				 *      'CASE' selector
				 *      { selection }
				 */
				{
					treenode *selector, *slist;

					nextsymb ();
					if ((selector = rexp ()) == NULL) {
						goto error;
					}
					checknewline ();
					slist = rproclist (rselection, indent + 2);
					*procptr = newactionnode (S_CASE, locn, selector, slist);
					return (procroot);
				}
				/*}}} */
				/*{{{  S_GUY, S_ASM */
			case S_GUY:
			case S_ASM:
				if (safe_code_only) {
					synerr (SYN_ASM_DISALLOWED, flocn);
					goto error;
				}
				if (current_fe_data->fe_noguy_yesasm && (symb == S_GUY)) {
					synerr (SYN_GUY_IS_OBSOLETE, flocn);
					goto error;
				}
				if (*current_fe_data->fe_guyinserts != 0) {
					const int this_symb = symb;
					const int asm_indent = symbindent;	/* Indent of GUY keyword */
					const SOURCEPOSN asm_locn = flocn;	/* Location of start of construct */

					nextsymb ();
					checknewline ();
					*procptr = newcnode (this_symb, asm_locn, rproclist ((this_symb == S_GUY) ? rguy : rasm, asm_indent + 2));
					return (procroot);
				} else {
					synerr (SYN_GUY_NOT_ENABLED, flocn);
					goto error;
				}
				/*}}} */
				/*{{{  S_INCLUDE S_SC S_USE S_IMPORT */
			case S_INCLUDE:
			case S_SC:
			case S_USE:
			case S_IMPORT:
				if (!rfile ())
					goto error2;
				break;
				/*}}} */
				/*{{{  S_OPTION */
			case S_OPTION:
				synerr (SYN_OPTION_IN_BAD_POS, flocn);
				nextline ();
				break;
				/*}}} */
				/*{{{  S_SET */
			case S_SET:
				*procptr = rset ();
				return (procroot);
				/*}}} */
				/*{{{  S_CONNECT */
			case S_CONNECT:
				*procptr = rconnect ();
				return (procroot);
				/*}}} */
				/*{{{  S_MAP */
			case S_MAP:
				*procptr = rmap ();
				return (procroot);
				/*}}} */
				/*{{{  default / errors */
			default:
				if (syn_lexlevel == base_lexlevel && symb == S_END) {
					if (lexmode == LEX_SOURCE && !foundroutine && ((current_fe_data->fe_lang & FE_LANG_OCCAM) != 0)) {
						/*synerr(SYN_SC_EMPTY, flocn); */
						msg_out (SEV_WARN, SYN, SYN_SC_EMPTY, NOPOSN);
					}
					*procptr = newleafnode (symb, locn);
					return (procroot);
				}
			error1:
#if 0
fprintf (stderr, "syn3: invalid process! (symb == %d)\n", symb);
#endif
				synerr (SYN_INVALID_PROCESS, flocn);
			error:
				nextline ();
			error2:
				skiplines (indent);
				if (lineindent < indent) {
					/*{{{  indentation shows that we are at end of SEQ etc, so return */
					return NULL;
					/*}}} */
					/*}}} */
				}
				/*}}} */
			}
		}
	}
}

/*}}}*/
/*{{{  PRIVATEPARAM void option_error*/
PRIVATEPARAM void option_error (const int c)
{
	synerr_i (SYN_BAD_OPTION, flocn, c);
}

/*}}}*/
/*{{{  PRIVATE void parse_option_directives*/
PRIVATE void parse_option_directives (void)
{
	while ((symb == S_COMMENT) || (symb == S_NEWLINE) || (symb == S_OPTION)) {
		if (symb == S_OPTION) {
			/*{{{  process option string */
			nextsymb ();	/* string with options */
			if (symb != S_STRING) {
				/*synerr(SYN_E_OPTION, flocn); */
				synerr_s (SYN_E_STRING_AFTER, flocn, tagstring (S_OPTION));
				nextline ();
			} else {
				const int ok = current_fe_data->fe_option_fn (literalv, literalp, option_error);

				if (ok && current_fe_data->fe_information) {
					fprintf (current_fe_data->fe_outfile, "%s \"%s\"\n", tagstring (S_OPTION), literalv);
				}
				nextsymb ();	/* should be end of line */
				checknewline ();
			}
			/*}}} */
		} else {
			nextline ();
		}
	}
}

/*}}}*/
/*{{{  PRIVATE treenode *define_builtins */
/* Wrap synthetic definitions around the AST for built-in protocols, etc. */
PRIVATE treenode *define_builtins (treenode *around)
{
	const SOURCEPOSN locn = NOPOSN;
	const char *signal_str = "SIGNAL";
	wordnode *signal_name;
	treenode *signal_tag, *signal_tags, *signal_decl;

	/* PROTOCOL SIGNAL
	     CASE
	       SIGNAL = 0
	   : */
	signal_name = lookupword (signal_str, strlen (signal_str));
	signal_tag = declname (N_TAGDEF, locn, signal_name, NULL, NULL);
	SetNTValue (signal_tag, 0);
	signal_tags = newlistnode (S_LIST, locn, signal_tag, NULL);
	signal_decl = declare (S_TPROTDEF, locn, signal_tags, signal_name, NULL);
	SetDBody (signal_decl, around);

	return signal_decl;
}
/*}}}*/
/*{{{  PUBLIC treenode *rscunit ()*/
PUBLIC treenode *rscunit (void)
{
	treenode *tptr, *t;

	/*{{{  COMMENT check indentation is zero at start */
	/***********************  Start comment out *****************************/
	/*@*{{{  check indentation is zero at start*@ */
	/*if (checkindent(0)) */
	/*return NULL; */
	/*@*}}}*@ */
	/***********************   End comment out  *****************************/
	/*}}} */

	base_lexlevel = syn_lexlevel;
	tptr = rprocess ();
	tptr = define_builtins (tptr);
	t = tptr;
	while (t != NULL && TagOf (t) != S_END) {
		switch (TagOf (t)) {
		default:
		      error:
			DEBUG_MSG (("Tag is %s\n", tagstring (TagOf (t))));
			synerr (SYN_SC_ERROR, LocnOf (t));
			return tptr;
		case S_VALABBR:
		case S_VALRETYPE:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_PRAGMA:	/* bug 829 19/9/91 */
#ifdef OCCAM2_5
		case S_TYPEDECL:
#endif
#ifdef MOBILES
		case S_FORWDECL:
		case S_PROCTYPEDECL:
		case S_MPROCDECL:
#endif
			t = DBodyOf (t);
			break;
		CASE_CONFIG_SPEC case S_DECL:
		case S_ABBR:
		case S_PLACEON:	/* leave these till later on */
			if ((current_fe_data->fe_lang & FE_LANG_OCCAM) != 0) {
				goto error;
			}
			t = DBodyOf (t);
			break;
		}
	}
	return tptr;
}

/*}}}*/
/*{{{  PUBLIC treenode *parse_file*/
PUBLIC treenode *parse_file (const char *const sourcefilename, const BOOL onlylex)
{
	treenode *progptr;
	jmp_buf savedenv;
	int use_lexmode;

	memcpy ((char *) savedenv, (char *) env, sizeof (env));

	if (setjmp (env) == 0) {
		progptr = NULL;
		if (strstr (sourcefilename, ".co") == sourcefilename + (strlen (sourcefilename) - 3)) {
			use_lexmode = LEX_CSOURCE;
		} else {
			use_lexmode = LEX_SOURCE;
		}
		if (open_file (sourcefilename, use_lexmode, 0)) {
			nextsymb ();
			if (onlylex) {
				/*{{{  print the lexemes */
				while ((symb != S_END) && (fe_get_errorcount (current_fe_handle) == 0)) {
					printlex (current_fe_data->fe_outfile);
					nextsymb ();
				}
				/*}}} */
			} else {
				/*{{{  lex and parse the program */
				parse_option_directives ();
				progptr = rscunit ();
				/*}}} */
			}
		} else {
			synerr_s (SYN_FILE_OPEN_ERROR, NOPOSN, sourcefilename);
		}
	} else {
		progptr = NULL;
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));

	/* This added to prevent later 'out of memory' errors reporting
	 * a silly line number - CON 22/1/91 */
	flocn = NOPOSN;

	return progptr;
}

/*}}}*/


