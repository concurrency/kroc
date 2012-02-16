/* $Id: syn1.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	Occam two syntax analyser
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
#include "feinc.h"
#include "synerror.h"
#include "syndef.h"
#include "lexdef.h"
#include "syn1def.h"
#include "chkdef.h"		/* current_fe_handle etc */
/*}}}*/

/*{{{  global variables */
PUBLIC int syn_lexlevel;

/* linebreakindent gives the minimum indentation at which the continuation
   of a line must be. A value of -1 means take the current line indentation.
   It is preserved across a VALOF, as these may have nested line breaks. */
PUBLIC int linebreakindent;

/* Init.ed to FALSE, set TRUE when a PROC or FUNCTION is read */
PUBLIC int foundroutine;

/*}}}*/

/*{{{  check for symbol */
/* return TRUE if required token not found */
/*{{{  PUBLIC void ignore (s) */
/* The symbol to be ignored */
PUBLIC void ignore (const int s)
{
	if (symb == s)
		nextsymb ();
}

/*}}}*/
/*{{{  PUBLIC void ignorecomments(i) */
/* Throw away any comments with an indentation >= i */
PUBLIC void ignorecomments (const int i)
{
	while ((symb == S_COMMENT) && (symbindent >= i))
		/* Throw away the comment */
		nextline ();
}

/*}}}*/
/*{{{  PUBLIC BOOL checkfor (s) */
PUBLIC BOOL checkfor (const int s)
{
	if (symb == s) {
		nextsymb ();
		return FALSE;
	} else {
		synetoken (s);
		return TRUE;
	}
}

/*}}}*/
/*{{{  PRIVATE BOOL badindenterr */
PRIVATE BOOL badindenterr (void)
/* returns TRUE if a real error, FALSE if just a warning */
{
#if 0
	synerr (SYN_BAD_INDENT, flocn);
	return TRUE;
#else
	const BOOL warn = current_fe_data->fe_warn_comment_indent && (symb == S_COMMENT);
	DEBUG_MSG (("badindenterr: warn?:%d\n", warn));
	msg_out (warn ? SEV_WARN : SEV_ERR, SYN, SYN_BAD_INDENT, flocn);	/* modified bug 853 18/1/91 */
	if (warn)
		ignorecomments (0);	/* ignore all following comments */
	return !warn;
#endif
}

/*}}}*/
/*{{{  PUBLIC BOOL checkindent (i) */
PUBLIC BOOL checkindent (int i)
{
	ignorecomments (i);
	if ((symb != S_END) && (symbindent != i)) {
		const int err = badindenterr ();
		DEBUG_MSG (("checkindent: err?%d\n", err));
		if (err)
			skiplines (i);
		return (err);
	} else
		return (FALSE);
}

/*}}}*/
/*{{{  PUBLIC void checknewline () */
PUBLIC void checknewline (void)
{
	if (symb == S_COMMENT)
		nextsymb ();
	if (symb != S_NEWLINE)
		synerr_e (SYN_E_NEWLINE, flocn, symb);
	nextline ();
}

/*}}}*/
/*{{{  PUBLIC BOOL checknlindent (i) */
/* Check next lexical token is a newline, ignore any following comments
at an indent >= i, check that the first token after the ignored comments
has an indent of i. */
PUBLIC BOOL checknlindent (int i)
{
	checknewline ();
	return checkindent (i);
}

/*}}}*/
/*{{{  PUBLIC BOOL checklinebreak () */
/* Returns TRUE if line appears to be split, and next line under indented */
PUBLIC BOOL checklinebreak (void)
{
	if (symb == S_COMMENT)
		nextsymb ();
	if (symb == S_NEWLINE) {
		if (linebreakindent < 0)
			linebreakindent = lineindent;
		nextline ();
		ignorecomments (linebreakindent);
		if (symbindent < linebreakindent) {
			DEBUG_MSG (("checklinebreak\n"));
			return badindenterr ();
		}
	}
	return FALSE;
}

/*}}}*/
/*}}}*/

/*{{{  PRIVATE int namenodetag (tag) */
/*****************************************************************************
 *
 *  namenodetag takes a name node tag, 'tag', and converts it to the
 *              appropriate tag for the file it is declared in
 *              (ie. SC, library, standard library, predefined name)
 *
 *****************************************************************************/
PRIVATE int namenodetag (int tag)
{
	switch (lexmode) {
	case LEX_SOURCE:
	case LEX_CSOURCE:
		return tag;
	case LEX_LIB:
	case LEX_EXTERNAL:
	case LEX_DEXTERNAL:
		switch (tag) {
		case N_PROCDEF:
			return N_LIBPROCDEF;
		case N_MPROCDECL:
			return N_LIBMPROCDECL;
		default:
			return N_LIBFUNCDEF;
		}
#if 0				/* LEX_SC is no longer used */
	case LEX_SC:
		return (tag == N_PROCDEF) ? N_SCPROCDEF : N_SCFUNCDEF;
#endif
	case LEX_STDLIB:
		return (tag == N_PROCDEF) ? N_STDLIBPROCDEF : N_STDLIBFUNCDEF;
	case LEX_PREDEFS:
		return (tag == N_PROCDEF) ? N_PREDEFPROC : N_PREDEFFUNCTION;
	default:
		return (0);	/* Not reached */
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *declare (stype, locn, t, name, e) */
/* Mode of declaration */
/* Location of declaration */
/* Tree representing type of declared object */
/* Pointer to node containing text of name */
/* Value of declaration, eg. rhs of abbrev */
PUBLIC treenode *declare (const int stype, const SOURCEPOSN locn, treenode * t, wordnode * name, treenode * e)
{
	treenode *tptr, *nptr;
	int ntype = 0;		/* initialised to shut up gcc's optimiser */
	/*{{{  set up ntype */
	switch (stype) {
	case S_VALABBR:
		ntype = N_VALABBR;
		break;
	case S_VALRETYPE:
		ntype = N_VALRETYPE;
		break;
	case S_DECL:
		ntype = N_DECL;
		break;
	case S_ABBR:
		ntype = N_ABBR;
		break;
	case S_RETYPE:
		ntype = N_RETYPE;
		break;
	case S_TPROTDEF:
		ntype = N_TPROTDEF;
		break;
	case S_SPROTDEF:
		ntype = N_SPROTDEF;
		break;
	case S_LABELDEF:
		ntype = N_LABELDEF;
		break;
	case S_PROCDEF:
		ntype = namenodetag (N_PROCDEF);
#if 0
fprintf (stderr, "syn1: declare(): PROCDEF/%d\n", ntype);
#endif
		break;
	case S_SFUNCDEF:
		ntype = namenodetag (N_SFUNCDEF);
		break;
	case S_LFUNCDEF:
		ntype = namenodetag (N_LFUNCDEF);
		break;
#if 1				/*def CONFIG */
	case S_NETWORK:
		ntype = N_NETWORK;
		break;
	case S_CONFIG:
		ntype = N_CONFIG;
		break;
	case S_MAPPING:
		ntype = N_MAPPING;
		break;
#endif
#ifdef OCCAM2_5
	case S_TYPEDECL:
		ntype = N_TYPEDECL;
		break;
#endif
#ifdef MOBILES
	case S_PROCTYPEDECL:
		ntype = N_PROCTYPEDECL;
		break;
	case S_MPROCDECL:
		ntype = namenodetag (N_MPROCDECL);
		break;
#endif
	default:
		/* internal error */
		msg_out_s (SEV_INTERNAL, SYN, SYN_ILLEGAL_DECL_TYPE, locn, itagstring (stype));
	}
	/*}}} */
	nptr = newnamenode (ntype, locn, name, t, NULL, syn_lexlevel, 0, NM_DEFAULT);
	tptr = newdeclnode (stype, locn, nptr, e, NULL);
	SetNDecl (nptr, tptr);
	return (tptr);
}

/*}}}*/

/*{{{  PUBLIC treenode *declname (ntype, locn, name, type, spec) */
/* Type of declaration - N_ABBR, N_VALABBR, N_DECL etc. */
/* Pointer to node containing text of name */
/* A tree structure representing type of declaration */
/* Pointer to the specification of the name on parse tree */
PUBLIC treenode *declname (int ntype, SOURCEPOSN locn, wordnode *name, treenode *type, treenode *spec)
{
	treenode *tmpnode;

	tmpnode = newnamenode (ntype, locn, name, type, spec, syn_lexlevel, 0, NM_DEFAULT);
	return tmpnode;
}

/*}}}*/

/*{{{  PUBLIC void syninit () */
PUBLIC void syninit (void)
{
	syn_lexlevel = 0;
	linebreakindent = (-1);
	foundroutine = FALSE;
/*nextsymb (); *//* don't need cos haven't started reading predefines */
}

/*}}}*/
