/* $Id: syn2.c,v 1.13 1998/09/03 11:41:28 djb1 Exp $ */

/*
 *	Occam two syntax analyser 2
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
#include "feinc.h"
#include "lexerror.h"
#include "synerror.h"
#include "lexdef.h"
#include "syn1def.h"
#include "syn2def.h"
#include "syn3def.h"
#include "syndef.h"
#include "chkdef.h"		/* current_fe_handle etc */
#include "objrdr.h"		/* needed for addtoentrypointlist (correct declaration of library routines) */
#include "objlib.h"		/* only needed cos objtrans.h requires it */
#include "objtrans.h"		/* #PRAGMA TRANSLATE */
#include "predefhd.h"		/* #PRAGMAs */
#ifdef MOBILES
#include "mobiles.h"		/* for anon chan-type helping (SHARED CHAN) */
#endif
/*}}}*/
/*{{{  #defines*/
#define STRING_RECORD_SIZE 1000
/*}}}*/
/*{{{  forward declarations*/
PRIVATE treenode *rspecorexpr (BOOL *const specflag, const int pindent, const BOOL exprlist_permitted, const BOOL fn_permitted);
#ifdef OCCAM2_5
PRIVATE treenode *rspecifier_constr (void);
PRIVATE treenode *rtypeoroperand (void);
PRIVATE treenode *rspecorexprlist (BOOL * specflag);
#endif
PRIVATE int rtypehash (treenode **tptr);

#ifdef USER_DEFINED_OPERATORS
extern BOOL user_defined_operators;
#endif

/*}}}*/
/*{{{  support*/
/*{{{  occam-pi forward declarations list (and functions) */
static treenode *forwdecls = NULL;

/*{{{  static void new_forwdecl (treenode *fdecl)*/
static void new_forwdecl (treenode *fdecl)
{
	forwdecls = newlistnode (S_LIST, NOPOSN, fdecl, forwdecls);
}
/*}}}*/
/*{{{  static int check_forwdecl (treenode *name)*/
static int check_forwdecl (treenode *name)
{
	treenode *list;

	for (list = forwdecls; !EndOfList (list); list = NextItem (list)) {
		treenode *thisforw = ThisItem (list);

		if (TagOf (thisforw) != S_FORWDECL) {
			msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, NOPOSN, "check_forwdecl");
#if 0
fprintf (stderr, "check_forwdecl: list item not S_FORWDECL.\n");
#endif
		} else if (DNameOf (thisforw) == name) {
			return 1;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  static treenode *get_forwdecl (treenode *name)*/
static treenode *get_forwdecl (treenode *name)
{
	treenode *list;

	for (list = forwdecls; !EndOfList (list); list = NextItem (list)) {
		treenode *thisforw = ThisItem (list);

		if (TagOf (thisforw) != S_FORWDECL) {
			msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, NOPOSN, "get_forwdecl");
#if 0
fprintf (stderr, "get_forwdecl: list item not S_FORWDECL.\n");
#endif
		} else if (DNameOf (thisforw) == name) {
			return thisforw;
		}
	}
	return NULL;
}
/*}}}*/
/*{{{  static void del_forwdecl (treenode *name)*/
static void del_forwdecl (treenode *name)
{
	treenode *list, *prev;

	prev = NULL;
	for (list = forwdecls; !EndOfList (list); prev = list, list = NextItem (list)) {
		treenode *thisforw = ThisItem (list);

		if (TagOf (thisforw) != S_FORWDECL) {
			msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, NOPOSN, "del_forwdecl");
#if 0
fprintf (stderr, "get_forwdecl: list item not S_FORWDECL.\n");
#endif
		} else if (DNameOf (thisforw) == name) {
			if (!prev) {
				forwdecls = NextItem (list);
			} else {
				SetRight (prev, NextItem (list));
			}
			return;
		}
	}
	return;
}
/*}}}*/

/*}}}*/
/*{{{  PUBLIC BOOL mustbespec (const int s)*/
/*
 *	returns true if the given symbol must be the start of a specification
 */
PUBLIC BOOL mustbespec (const int s)
{
	switch (s) {
	case S_VAL:
	case S_PROC:
	case S_PROTOCOL:
	case S_CHAN:
	case S_PORT:
	case S_TIMER:
	case S_BOX:
	case S_PLACE:
	case S_INLINE:
	CASE_CONFIG_SPEC case S_PRAGMA:	/* bug 829 19/9/91 */
	case S_HCOMMENT:	/* bug 829 20/9/91 */
	#ifndef ARRAYCONSTRUCTOR
		case S_HNETWORK:
	#endif	/* !ARRAYCONSTRUCTOR */
	#ifdef OCCAM2_5
		case S_DATA:
	#endif
	#ifdef MOBILES
		case S_MOBILE:
		case S_SHARED:
		case S_FIXED:
		case S_ANYCHANTYPE:
		case S_ANYPROCTYPE:
		case S_ANYMOBILETYPE:
	#endif
	case S_RECURSIVE:
	case S_REC:
	case S_BARRIER:
	case S_BUFFERED:
		return TRUE;
	default:
		return FALSE;
	}
}

/*}}}*/
/*{{{  PRIVATE BOOL mustbeexp (const int s)*/
/*
 *	returns true if the given symbol must be the start of an expression
 */
PRIVATE BOOL mustbeexp (const int s)
{
	switch (s) {
	case S_MOSTPOS:
	case S_MOSTNEG:
#ifdef MOBILES
	case S_CLONE:
	case S_DEFINED:
	case S_ADDROF:
	case S_HWADDROF:
#endif
	case S_TYPEHASHOF:
	case S_SUBTRACT:
	case S_MINUS:
	case S_BITNOT:
	case S_NOT:
	case S_SIZE:
	case S_LPAREN:
	case S_TRUE:
	case S_FALSE:
	case S_UBYTELIT:
	case S_UINTLIT:
	case S_UREALLIT:
	case S_STRING:
	case S_STRINGCONT:
	case S_VALOF:
#ifdef OCCAM2_5
	case S_BYTESIN:
	case S_OFFSETOF:
#endif
#include "casemops.h"
#include "casedops.h"
		/* may need dops too, Jim 20/2/97 */
		return TRUE;
	default:
		return FALSE;
	}
}

/*}}}*/
/*}}}*/
/*{{{  lists*/
/*{{{  PUBLIC treenode *rproclist (treenode *(*readitem) (void), const int indent)*/
/*****************************************************************************
 *
 *  rproclist reads a list of items. 'readitem' is a function called to read
 *            each item.  Each item of the list must have the same indentation
 *            as the first item: a smaller indentation terminates the list,
 *            a greater indentation is an error.
 *
 *****************************************************************************/
PUBLIC treenode *rproclist (treenode *(*readitem) (void), const int indent)
{
	treenode *root = NULL;
	treenode **pptr = &root;
	DEBUG_MSG (("rproclist... "));
	while (symbindent >= indent)
		/*{{{  create a list node and read another item */
	{
		DEBUG_MSG (("rproclist: looking for next process, symb:symbindent is %s:%d...\n", tagstring (symb), symbindent));
		switch (symb) {
		case S_COMMENT:
			nextline ();
			break;
		case S_INCLUDE:
		case S_SC:
		case S_USE:
		case S_IMPORT:	/* added for bug 1208 18/10/91 */
			(void) rfile ();	/* just loop even if there is an error */
			break;
		default:
			if (symbindent > indent)
				/*{{{  error so skip to next process if any */
			{
				synerr (SYN_BAD_INDENT, flocn);
				skiplines (indent);
			}
			/*}}} */
			else
				/*{{{  parse a process item, and append to list */
			{
				const SOURCEPOSN locn = flocn;
				treenode *const item = (*readitem) ();
				if (item != NULL)
					/*{{{  make new node */
				{
					*pptr = newlistnode (S_LIST, locn, item, NULL);
					pptr = NextItemAddr (*pptr);
				}
				/*}}} */
			}
			/*}}} */
			break;
		}
	}
	/*}}} */
	DEBUG_MSG (("rproclist: returning NOW... "));
	return (root);
}

/*}}}*/
/*{{{  PUBLIC treenode *rlist (treenode *(*p)(void), const int sep)*/
/*****************************************************************************
 *
 *  rlist reads a list of items.  'p' is a function which reads each item,
 *        and 'sep' is the token which separates each item.
 *        ie. parse   p { sep p }
 *
 *****************************************************************************/

/* may return NULL if error found */
PUBLIC treenode *rlist (treenode *(*p)(void), const int sep)
{
	treenode *tptr, *a, *item;
	SOURCEPOSN locn = flocn;
	DEBUG_MSG (("rlist... "));
	/*{{{  parse item */
	if ((item = (*p) ()) == NULL)
		return NULL;
	/*}}} */
	tptr = newlistnode (S_LIST, locn, item, NULL);
	a = tptr;
	while (symb == sep)
	/*{{{  do another item */
	{
		nextsymb ();
		/*{{{  ignore line break */
		if (checklinebreak ())
			return NULL;
		/*}}} */
		/*{{{  parse item */
		if ((item = (*p) ()) == NULL)
			return NULL;
		/*}}} */
		NewNextItem (newlistnode (S_LIST, locn, item, NULL), a);
		a = NextItem (a);
		locn = flocn;
	}
	/*}}} */
	return (tptr);
}

/*}}}*/
/*}}}*/

/* expression parsers may return NULL if an error is found , */
/* but will not skip to the end of a line,                   */
/* except when a VALOF is parsed.                            */
/*{{{  expressions*/
/*{{{  PRIVATE wordnode *rnameorstring (treenode *typelist, BOOL *overload, int *num_of_params)*/
/*{{{  COMMENT*/
 /*This is a modified version of the rname - it checks for a function being */
 /*declared surrounded by quotes - eg "+", and then generates a valid string */
 /*from that symbol - eg "+" -> udo.ADD. The caller then adds the type names */
 /*(as strings) to this to construct the overloading functions name. */
 /*overload becomes set if it is an overloaded function, and num_of_params */
 /*is set to the number of parameters expected by that operator - if it can */
 /*be either one or two (in the case of "-") then is becomes -1. */
/*}}}*/
#ifdef USER_DEFINED_OPERATORS
/*{{{  forward declaration*/
/* forward delcaration */
PRIVATE int param_type_string (treenode *typetree, char *rbuf, int *rlen, int rsize);

/*}}}*/


PRIVATE wordnode *rnameorstring (treenode *typelist, BOOL *overload, int *num_of_params)
{
	if (!user_defined_operators) {
		return (rname ());
	}
	DEBUG_MSG (("rnameorstring... "));
	if ((symb == S_NAME) || (symb == S_ASMNAME)) {
		wordnode *const name = lexword;
		nextsymb ();
		*overload = FALSE;
		return (name);
	} else {
		if (TagOf (typelist) == S_LIST) {
			synerr_e (SYN_UDO_MULTI_RET_PARAMS, flocn, symb);
			return NULL;
		}
		/* this next section of code converts a "+" or whatever as a function name */
		/* into a wordnode with a pseudo name - ie "+" -> udo.ADD. */
		if (symb == S_STRING) {	/* deal with overloaded operators *//* added by Jim Moores 18/10/96 */
			/*{{{  allocate memory to string */
			char *name = (char *)memalloc (256);	/* should change to a constant */
			/*}}} */
			int namelen = 0;
			wordnode *name_word = NULL;
			wordnode *wordtocompare = lookupword (literalv, literalp);
			/*{{{  zero string memory */
			int index = 0;
			for (index = 0; index < 256; name[index++] = 0);

			/*}}} */
			/*{{{  is it a binary operator?  Make string for it */
			if (wordtocompare == lookupword ("+", 1)) {
				name = strcpy (name, "udo.ADD.\0");
			} else if (wordtocompare == lookupword ("/", 1)) {
				name = strcpy (name, "udo.DIV.\0");
			} else if (wordtocompare == lookupword ("*", 1)) {
				name = strcpy (name, "udo.MUL.\0");
			} else if (wordtocompare == lookupword ("\\", 1)) {
				name = strcpy (name, "udo.REM.\0");
			} else if (wordtocompare == lookupword ("PLUS", 4)) {
				name = strcpy (name, "udo.PLU.\0");
			} else if (wordtocompare == lookupword ("TIMES", 5)) {
				name = strcpy (name, "udo.TIM.\0");
			} else if (wordtocompare == lookupword ("AFTER", 5)) {
				name = strcpy (name, "udo.AFT.\0");
			} else if (wordtocompare == lookupword ("AND", 3)) {
				name = strcpy (name, "udo.AND.\0");
			} else if (wordtocompare == lookupword ("OR", 2)) {
				name = strcpy (name, "udo.ORo.\0");
			} else if (wordtocompare == lookupword ("><", 2)) {
				name = strcpy (name, "udo.XOR.\0");
			} else if (wordtocompare == lookupword ("/\\", 2)) {
				name = strcpy (name, "udo.BAN.\0");
			} else if (wordtocompare == lookupword ("\\/", 2)) {
				name = strcpy (name, "udo.BOR.\0");
			} else if (wordtocompare == lookupword (">>", 2)) {
				name = strcpy (name, "udo.RSH.\0");
			} else if (wordtocompare == lookupword ("<<", 2)) {
				name = strcpy (name, "udo.LSH.\0");
			} else if (wordtocompare == lookupword ("=", 1)) {
				name = strcpy (name, "udo.EQA.\0");
			} else if (wordtocompare == lookupword ("<>", 2)) {
				name = strcpy (name, "udo.NEQ.\0");
			} else if (wordtocompare == lookupword ("<", 1)) {
				name = strcpy (name, "udo.LTH.\0");
			} else if (wordtocompare == lookupword (">", 1)) {
				name = strcpy (name, "udo.GRT.\0");
			} else if (wordtocompare == lookupword ("<=", 2)) {
				name = strcpy (name, "udo.LTE.\0");
			} else if (wordtocompare == lookupword (">=", 2)) {
				name = strcpy (name, "udo.GTE.\0");
			}
			/*}}} */
			if (name[0] != '\0') {	/* match found */
				*num_of_params = 2;
			} else {
				/*{{{  is it a unary operator?  Make string for it */
				if (wordtocompare == lookupword ("~", 1)) {
					name = strcpy (name, "udo.BNT.\0");
				}
				if (wordtocompare == lookupword ("NOT", 3)) {
					name = strcpy (name, "udo.NOT.\0");
				}
				/*}}} */
				if (name[0] != '\0') {	/* match found */
					*num_of_params = 1;
				} else {
					/*{{{  is it an operator that can be either?  Make string for it */
					if (wordtocompare == lookupword ("-", 1)) {	/* Special case, "-" can be monadic or diadic */
						name = strcpy (name, "udo.SUB.\0");
					} else if (wordtocompare == lookupword ("MINUS", 5)) {
						name = strcpy (name, "udo.MIN.\0");
					} else if (wordtocompare == lookupword ("??", 2)) {
						name = strcpy (name, "udo.QMQ.\0");
					} else if (wordtocompare == lookupword ("@@", 2)) {
						name = strcpy (name, "udo.ATA.\0");
					} else if (wordtocompare == lookupword ("$$", 2)) {
						name = strcpy (name, "udo.DOD.\0");
					} else if (wordtocompare == lookupword ("%", 1)) {
						name = strcpy (name, "udo.PER.\0");
					} else if (wordtocompare == lookupword ("%%", 2)) {
						name = strcpy (name, "udo.PPR.\0");
					} else if (wordtocompare == lookupword ("&&", 2)) {
						name = strcpy (name, "udo.ANA.\0");
					} else if (wordtocompare == lookupword ("<%", 2)) {
						name = strcpy (name, "udo.LPR.\0");
					} else if (wordtocompare == lookupword ("%>", 2)) {
						name = strcpy (name, "udo.PRR.\0");
					} else if (wordtocompare == lookupword ("<&", 2)) {
						name = strcpy (name, "udo.LAN.\0");
					} else if (wordtocompare == lookupword ("&>", 2)) {
						name = strcpy (name, "udo.ANR.\0");
					} else if (wordtocompare == lookupword ("[>", 2)) {
						name = strcpy (name, "udo.RTR.\0");
					} else if (wordtocompare == lookupword ("<]", 2)) {
						name = strcpy (name, "udo.RTL.\0");
					} else if (wordtocompare == lookupword ("++", 2)) {
						name = strcpy (name, "udo.PLP.\0");
					} else if (wordtocompare == lookupword ("^", 1)) {
						name = strcpy (name, "udo.HAT.\0");
					} else if (wordtocompare == lookupword ("!!", 2)) {
						name = strcpy (name, "udo.EXE.\0");
					} else if (wordtocompare == lookupword ("==", 2)) {
						name = strcpy (name, "udo.EQE.\0");
					} else if (wordtocompare == lookupword ("<@", 2)) {
						name = strcpy (name, "udo.LAT.\0");
					} else if (wordtocompare == lookupword ("@>", 2)) {
						name = strcpy (name, "udo.ATR.\0");
					} else if (wordtocompare == lookupword ("@", 1)) {
						name = strcpy (name, "udo.AMA.\0");
					}
					/*}}} */
					if (name[0] != '\0') {	/* match found */
						*num_of_params = -1;
					} else {
						/*{{{  otherwise its an error */
						synerr_e (SYN_UDO_NOT_SUPPORTED, flocn, symb);
						return NULL;
						/*}}} */
					}
				}
			}
			/*{{{  make wordnode and return it (with overload flag set) */
#if 0
fprintf (stderr, "syn2: rnameorstring(): returning overloaded operator name, namelen = %d, name = [%s]\n", namelen, name);
#endif
			namelen = 8;
			name_word = lookupword (name, namelen);
			nextsymb ();
			*overload = TRUE;
			return (name_word);
			/*}}} */
		} else {
			synerr_e (SYN_E_NAME, flocn, symb);
			return NULL;
		}
	}
}
#endif
/*}}}*/
/*{{{  PUBLIC wordnode *rname (void)*/
PUBLIC wordnode *rname (void)
{
	DEBUG_MSG (("rname... "));
	if ((symb == S_NAME) || (symb == S_ASMNAME)) {
		wordnode *const name = lexword;

		nextsymb ();
		switch (lexmode) {
		case LEX_SC:
		case LEX_LIB:
			/* might be immediately followed with a ":<typehash>" */
			if (symb == S_AMPERSAT) {
				char namebuf[128];
				int namelen = 0;
				wordnode *newname;

#if 0
fprintf (stderr, "rname(): got AMPERSAT after name in SC/LIB.\n");
#endif
				nextsymb ();
				if (symb != S_UINTLIT) {
					msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, NOPOSN, "rname(): SC/LIB missing UINTLIT after @");
				}
				namelen = sprintf (namebuf, "%s:%s", WNameOf (name), literalv);		/* lexer ensures literalv is NUL-terminated */
				nextsymb ();

				newname = lookupword (namebuf, namelen);
#if 0
fprintf (stderr, "rname(): got AMPERSAT after name in SC/LIB. new name is [%s]\n", WNameOf (newname));
#endif
				return newname;
			}
			break;
		default:
			break;
		}
		return name;
	} else {
		synerr_e (SYN_E_NAME, flocn, symb);
		return NULL;
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *rnamelist (void)*/
/*
 *	parses a comma-separated list of names
 */
PUBLIC treenode *rnamelist (void)
{
	treenode *nlist;

	DEBUG_MSG (("rnamelist... "));
	if (symb != S_NAME) {
		synerr_e (SYN_E_NAME, flocn, symb);
		return NULL;
	}
	nlist = (treenode *)rname ();
	if (symb == S_COMMA) {
		nlist = newlistnode (S_LIST, flocn, nlist, NULL);
	}
	while (symb == S_COMMA) {
		treenode *tname;

		nextsymb ();
		if (symb != S_NAME) {
			synerr_e (SYN_E_NAME, flocn, symb);
			return NULL;
		}
		tname = (treenode *)rname ();
		nlist = newlistnode (S_LIST, flocn, tname, nlist);
	}
	return nlist;
}
/*}}}*/
/*{{{  PUBLIC treenode *rsegment (treenode *const node)*/
PUBLIC treenode *rsegment (treenode *const node)
{
	/*{{{  parsing */
	/*
	   the '[' and element have already been parsed,
	   node is the element we are taking the slice of :

	   'FROM' exp 'FOR' exp ']'

	   occam2.5: permits the dropping of 'FROM' or 'FOR' expression,
	   but NOT both
	 */
	/*}}} */
	treenode *start = NULL;
	treenode *length = NULL;
	const SOURCEPOSN locn = flocn;
#ifdef OCCAM2_5
	if (symb != S_FOR) {
		if (checkfor (S_FROM) || checklinebreak () || ((start = rexp ()) == NULL))
			return NULL;
	}
	if (symb != S_RBOX) {
		if (checkfor (S_FOR) || checklinebreak () || ((length = rexp ()) == NULL))
			return NULL;
	}
	if (checkfor (S_RBOX))
		return NULL;
#else
	if (checkfor (S_FROM) ||
	    checklinebreak () ||
	    ((start = rexp ()) == NULL) || checkfor (S_FOR) || checklinebreak () || ((length = rexp ()) == NULL) || checkfor (S_RBOX)) return NULL;
#endif
	return newsegmentnode (S_SEGMENT, locn, node, start, length);
}

/*}}}*/
/*{{{  PRIVATE treenode *rendofconstructor (treenode *const node, const SOURCEPOSN locn)*/
PRIVATE treenode *rendofconstructor (treenode *const node, const SOURCEPOSN locn)
{
	treenode *type = NULL;
	treenode *connode = NULL;

#ifdef OCCAM2_5
	if (symb == S_LPAREN) {
		/*{{{  type decoration */
		nextsymb ();
		type = (treenode *) rname ();
		if (checkfor (S_RPAREN)) {
			return NULL;
		}
		/*}}} */
	}
#endif
	connode = newlitnode (S_CONSTRUCTOR, locn, node, type);
	return connode;
}

/*}}}*/
/*{{{  PRIVATE treenode *rconstructor (treenode *node)*/
PRIVATE treenode *rconstructor (treenode *node)
{
	/*{{{  parsing */
	/*
	   The opening box & first element have been parsed,
	   node is is the first element.
	   we do :
	   {',' exp} ']'
	 */
	/*}}} */
	if (symb == S_COMMA) {	/* read list of items */
		treenode *list;
		nextsymb ();
		if (checklinebreak ())
			return NULL;
		if ((list = rlist (rexp, S_COMMA)) == NULL)
			return NULL;
		node = newlistnode (S_LIST, flocn, node, list);
	} else {
		node = newlistnode (S_LIST, flocn, node, NULL);	/* one item list      */
	}
	if (checkfor (S_RBOX)) {
		return NULL;
	}
	return rendofconstructor (node, flocn);
}

/*}}}*/
/*{{{  PRIVATE treenode *rarrayconstructor (treenode *iexp)*/
PRIVATE treenode *rarrayconstructor (treenode *iexp)
{
	/* parsing:
	 *    '[' name '=' start 'FOR' length ['STEP' step] '|' exp ']'
	 *        ^^^^^^^^^^^^^^---(in iexp)
	 */
	treenode *nptr, *start, *length, *step, *exp, *rcptr;
	treenode *tmpdecl, *tmpidecl, *vdecl, *videcl;	/* used in chk1.c when re-writing this */
	static char *acname = "$actmp", *aciname = "$acitmp";
	int aclen = 6, acilen = 7;
	wordnode *tmpname = lookupword (acname, aclen);
	wordnode *tmpiname = lookupword (aciname, acilen);

#if 0
fprintf (stderr, "rarrayconstructor: iexp =");
printtreenl (stderr, 4, iexp);
#endif
	if (TagOf (LeftOpOf (iexp)) != S_NAME) {
		return NULL;
	}

	/* declare temporary result array */
	vdecl = newdeclnode (S_DECL, flocn, NULL, NULL, NULL);
	tmpdecl = declname (N_DECL, flocn, tmpname, NULL, vdecl);
	SetDName (vdecl, tmpdecl);

	/* declare temporary, gets manged into thing later.. */
	videcl = newdeclnode (S_VALABBR, flocn, NULL, NULL, NULL);
	tmpidecl = declname (N_VALABBR, flocn, tmpiname, newleafnode (S_INT, flocn), videcl);
	SetDName (videcl, tmpidecl);

	/* declare replicator variable */
	nptr = declname (N_REPL, flocn, (wordnode *)(LeftOpOf (iexp)), newleafnode (S_INT, flocn), NULL);

	/* nptr = LeftOpOf (iexp); */
	start = RightOpOf (iexp);
	if (checkfor (S_FOR)) {
		return NULL;
	}
	length = rexp ();
	if (symb == S_STEP) {
		nextsymb ();
		step = rexp ();
	} else {
		step = NULL;
	}
	if (checkfor (S_BAR)) {
		return NULL;
	}
	if (symb == S_NEWLINE) {
		nextsymb ();
	}
#if 0
fprintf (stderr, "*** about to rexp() for array expression.  syn_lexlevel is %d\n", syn_lexlevel);
#endif
	exp = rexp ();
	if (!nptr || !start || !length || !exp || checkfor (S_RBOX)) {
		return NULL;
	}
	rcptr = newreplcnode (S_ARRAYCONSTRUCTOR, flocn, nptr, start, length, step, exp);
	SetNDecl (nptr, rcptr);
	SetReplCTemp (rcptr, newlistnode (S_LIST, NOPOSN, tmpdecl, newlistnode (S_LIST, NOPOSN, tmpidecl, NULL)));
#if 0
fprintf (stderr, "*** done in rarrayconstructor.  syn_lexlevel is %d.  parsed expression is:", syn_lexlevel);
printtreenl (stderr, 4, exp);
#endif
	return rcptr;
}
/*}}}*/
/*{{{  PUBLIC treenode *rsubscript (treenode *a)*/
/* The element built up so far */
/* may return NULL */
/* Parse
'[' expression ']' { '[' expression ']' }
*/
PUBLIC treenode *rsubscript (treenode *a)
{
	SOURCEPOSN locn = flocn;
	treenode *exp;
	DEBUG_MSG (("rsubscript... "));
	if (symb != S_LBOX) {
		synetoken (S_LBOX);
		return NULL;
	}
	while (symb == S_LBOX) {
		nextsymb ();
		if ((exp = rexp ()) == NULL)
			return NULL;
		if (checkfor (S_RBOX))
			return NULL;
		a = newarraysubnode (S_ARRAYSUB, locn, a, exp);
		/* if (symb == S_LBOX) a = rsubscript (a); */
	}
	return (a);
}

/*}}}*/
/*{{{  PUBLIC treenode *relement (void)*/
/* may return NULL if error found */
PUBLIC treenode *relement (void)
{
	SOURCEPOSN locn = flocn;
	treenode *a;

	DEBUG_MSG (("relement... "));
	if (symb == S_NAME)
		a = (treenode *) rname ();	/* name or subscripted element */
	else if (symb == S_LBOX)
		/*{{{  segment */
	{
		treenode *name;
		nextsymb ();
		if ((name = relement ()) == NULL)
			return NULL;
		a = rsegment (name);
	}
	/*}}} */
	else {
		synerr_e (SYN_E_ELEMENT, locn, symb);
		return NULL;
	}

	if (a != NULL && symb == S_LBOX)
		a = rsubscript (a);

	return (a);
}

/*}}}*/
/*{{{  PUBLIC treenode *rinstance (int tag, SOURCEPOSN locn, treenode *name)*/

/* forward */
PRIVATE treenode *rprotocol (void);

PUBLIC treenode *rinstance (int tag, SOURCEPOSN locn, treenode *name)
{
	/*{{{  parsing */
	/*
	   Assume the name has already been read
	   '(' exp {',' exp } ')'
	 */
	/*}}} */
	treenode *params;
	if (checkfor (S_LPAREN)) {
		return NULL;
	}
	if (symb != S_RPAREN) {

		/* bug TS/1487 - remove all these bits from here! 26/11/91 */
/*const int saved_indent = linebreakindent; *//* bug TS/1457 11/11/91 */
/*linebreakindent = (-1); *//* bug TS/1251 25/9/91 *//* removed 26/11/91 */

		#if defined(PD_PROTOCOL_HASH)
		/* frmb: special case for PROTOCOL.HASH -- parse protocol instead (names sorted out in check) */
		if (TagOf (name) == S_NAME) {
			const char *tmpch = WNameOf ((wordnode *)name);

			/* grotty string comparison, but make it less painful by checking 1st char first.. */
			if ((*tmpch == 'P') && !strcmp (tmpch, get_predef_string (PD_PROTOCOL_HASH))) {
#if 0
fprintf (stderr, "rinstance: tag = %d, WTagOf ((wordnode *)name) = %d, name = ", tag, WTagOf ((wordnode *)name));
printtreenl (stderr, 4, name);
#endif
				params = rprotocol ();
				if (params) {
					params = newlistnode (S_LIST, flocn, params, NULL);
				}
			} else {
				params = rlist (rexp, S_COMMA);
			}
		} else {
			params = rlist (rexp, S_COMMA);
		}
		#else
		params = rlist (rexp, S_COMMA);
		#endif
/*linebreakindent = saved_indent; *//* added 11/11/91 *//* removed 26/11/91 */
		if (params == NULL) {
			return NULL;
		}
	} else {
		params = NULL;
	}
	if (checkfor (S_RPAREN)) {
		return NULL;
	}
	return newinstancenode (tag, locn, name, params);
}

/*}}}*/
/*{{{  PRIVATE treenode *rrestofoperand (treenode *tptr, const SOURCEPOSN locn)*/
PRIVATE treenode *rrestofoperand (treenode *tptr, const SOURCEPOSN locn)
{
#if 0
fprintf (stderr, "rrestofoperand: symb = %d\n", symb);
#endif
	switch (symb) {
	case S_LPAREN:
		tptr = rinstance (S_FINSTANCE, locn, tptr);
		break;
#ifndef OCCAM2_5
	case S_LBOX:
		tptr = rsubscript (tptr);
		break;
#endif
	}
#ifdef OCCAM2_5
	if (symb == S_LBOX) {
		/*  We expect either name[exp]... or type conversion of
		   the form name table[exp]. We have parsed already name and
		   the result is held in tptr  */
		treenode *exp;
		nextsymb ();
		if ((exp = rexp ()) == NULL) {
			return NULL;
		}
		if (symb == S_RBOX && TagOf (exp) != S_CONSTRUCTOR) {
			nextsymb ();
			tptr = newarraysubnode (S_ARRAYSUB, locn, tptr, exp);
			if (symb == S_LBOX)
				tptr = rsubscript (tptr);
			return tptr;
		}
		/* type conversion of the form: name table[exp] bug INSdi03578.
		   We have parsed already name and the result is held in tptr */
		else if (symb == S_RBOX && TagOf (exp) == S_CONSTRUCTOR) {
			/* A constructor with one  element which is a constructor. */
			nextsymb ();
			exp = rendofconstructor (newlistnode (S_LIST, flocn, exp, NULL), flocn);
		} else
			exp = rconstructor (exp);
		if (exp == NULL)
			return NULL;
		else if (symb == S_LBOX)
			exp = rsubscript (exp);
		if (tptr != NULL) {
			tptr = newlistnode (S_LIST, locn, tptr, newlistnode (S_LIST, locn, exp, NULL));
		}
		tptr = newmopnode (S_EXACT, locn, tptr, S_NAME);
	}
#endif
	return tptr;
}

/*}}}*/
/*{{{  PRIVATE treenode *rvalof (void)*/
/* Terminates with symb at start of new line */
/*{{{  what we are parsing*/
/*  Parse
valof = 'VALOF'
process
'RESULT' expression.list

including the newline following the expression list,
or
valof = specification
valof
*/
/*}}}*/
PRIVATE treenode *rvalof (void)
{
	int indent = symbindent;	/* The indent of the 'VALOF' keyword */
	int savedlinebreakindent;
	SOURCEPOSN locn = flocn;
	treenode *p, *n, *e;

	DEBUG_MSG (("rvalof... "));
	/*{{{  VALOF */
	if (checkfor (S_VALOF))
		goto error;
	if (checknlindent (indent + 2))
		goto error2;
	/*}}} */
	/*{{{  PROCESS */
	savedlinebreakindent = linebreakindent;
	linebreakindent = (-1);
	if (((p = rprocess ()) == NULL) || (checkindent (indent + 2))) {
		goto error2;
	}
	ignorecomments (indent + 2);
	/*}}} */
	/*{{{  RESULT list */
	if (checkfor (S_RESULT) || ((e = rlist (rexp, S_COMMA)) == NULL))
		goto error;
	/*}}} */
	n = newvalofnode (S_VALOF, locn, p, e);
	checknewline ();
	linebreakindent = savedlinebreakindent;
	return (n);

error:
	nextline ();
error2:
	skiplines (indent);
	return NULL;
}

/*}}}*/
/*{{{  PRIVATE treenode *rcondexp (treenode *node)*/
#ifdef CONDEXP
PRIVATE treenode *rcondexp (treenode *node)
{
	/*{{{  parsing */
	/*
	   the boolean expression has been parsed already :
	   '->' exp ',' exp
	   | '->' exp ',' conditional.expression
	 */
	/*}}} */
	treenode *xtrue, *xfalse;
	if (checkfor (S_RIGHTARROW) ||
	    checklinebreak () || ((xtrue = rexp ()) == NULL) || checkfor (S_COMMA) || checklinebreak () || ((xfalse = rexp ()) == NULL))
		return NULL;
	if (symb == S_RIGHTARROW)
		xfalse = rcondexp (xfalse);
	return newcondexpnode (S_CONDEXP, flocn, node, xtrue, xfalse);
}
#endif

/*}}}*/
/*{{{  PRIVATE treenode *rstring (void)*/
/* This routine reads a string, allowing for long strings.
   It does NOT advance the symbol to the next symbol after the string
*/
PRIVATE treenode *rstring (void)
{
	treenode *node;
	SOURCEPOSN locn = flocn;
	if (symb == S_STRING) {
		literalv[literalp] = '\0';
		node = newconsttablenode (S_STRING, locn, lookupword (literalv, literalp), NULL);
	} else {		/* (symb == S_STRINGCONT) */

		BOOL more = TRUE;
		int len = literalp;
		int stringsize = STRING_RECORD_SIZE;
		char *string = (char *)memalloc (stringsize);

		memcpy (string, literalv, literalp);
		nextsymb ();

		while (more) {
			if ((len + literalp) > stringsize) {
				/*{{{  make string a bit bigger */
				char *newstring;

				stringsize += STRING_RECORD_SIZE;
				newstring = memalloc (stringsize);
				memcpy (newstring, string, len);
				memfree (string);
				string = newstring;
				/*}}} */
			}
			memcpy (string + len, literalv, literalp);
			len += literalp;
			locn = flocn;
			if (symb == S_STRINGCONT) {
				nextsymb ();
			} else {	/* symb == S_STRING */
				more = FALSE;
			}
		}
		node = newconsttablenode (S_STRING, locn, lookupword (string, len), NULL);
		memfree (string);
	}
	return node;
}

/*}}}*/
/*{{{  PRIVATE treenode *roperand (void)*/
/* On error, leave symb unchanged, except when VALOF found */
/*{{{  what we are parsing*/
/* Parse
'(' expression ')'
| '(' valof ')'
| '(' conditional.expression ')'
| literal
| element
| '[' expression { ',' expression } ']'
| name '(' expression { ',' expression } ')'
*/
/* added by frmb:
| '[' variable '=' expression FOR expression {STEP 'expression'} | expression ']'
| name ?
| name !
*/
/*}}}*/
PRIVATE treenode *roperand (void)
{
	const SOURCEPOSN locn = flocn;
	treenode *node = NULL;
	DEBUG_MSG (("roperand... "));
	switch (symb) {
		/*{{{  case S_LPAREN could be expression, specification or valof */
	case S_LPAREN:
		{
			BOOL specflag = FALSE;
			treenode **a = &node;
			int pindent = symbindent;
			int indent;
			nextsymb ();
			indent = symbindent;	/* The indent of first symbol after left paren */
			/*{{{  read spec or expression into *a */
			{
				const BOOL fn_permitted = ((current_fe_data->fe_lang & FE_LANG_NDL) == 0);
				*a = rspecorexpr (&specflag, pindent, FALSE, fn_permitted);
				if (*a == NULL)
					if (!specflag)
						return NULL;	/* previous syntax error */
			}
			/*}}} */
			if (specflag)
				/*{{{  read further specifications, a valof, and check it */
			{
				while (symb != S_VALOF && symbindent == indent) {
					if (*a != NULL)
						a = DBodyAddr (*a);	/* move a along list */
					while (symb == S_COMMENT)	/* skip comments    */
						if (checknlindent (indent))
							return NULL;
					*a = rspecification ();	/* parse expression  */
				}
				if (symb != S_VALOF) {	/* Check for valof   */
					synetoken (S_VALOF);
					return NULL;
				}
				if (checkindent (indent))
					return NULL;
				if (*a != NULL)
					a = DBodyAddr (*a);	/* move a along list   */
				if ((*a = rvalof ()) == NULL)
					return NULL;	/* parse VALOF         */
				ignorecomments (pindent);	/* throw away comments */
			}
			/*}}} */
#ifdef CONDEXP
			if (symb == S_RIGHTARROW)
				*a = rcondexp (*a);
#endif
			if (checkfor (S_RPAREN))
				return NULL;
		}
		break;
		/*}}} */
		/*{{{  case S_TRUE FALSE */
	case S_TRUE:
	case S_FALSE:
		node = newleafnode (symb, locn);
		nextsymb ();
		break;
		/*}}} */
		/*{{{  S_UINTLIT S_UREALLIT S_UBYTELIT */
	case S_UINTLIT:
	case S_UREALLIT:
	case S_UBYTELIT:
		{
			const int s = symb;
			treenode *ss = NULL;
			wordnode *w = lookupword (literalv, literalp);
			nextsymb ();
			if (symb == S_LPAREN) {	/* '(' simple type ')' */
				/*{{{  we have a type specifier */
				nextsymb ();
#if 0
				if (((s == S_UINTLIT || s == S_UBYTELIT) &&
				     (symb == S_INT || symb == S_BYTE || symb == S_INT64 ||
				      symb == S_INT32 || symb == S_INT16)) || (s == S_UREALLIT && (symb == S_REAL32 || symb == S_REAL64)))
					/*s = littag(symb); */
					ss = newleafnode (symb, locn);
				else {
					synerr (SYN_ILLEGAL_CONVERSION, locn);
					return NULL;
				}
				nextsymb ();
#else
				ss = rspecifier ();
				if (ss == NULL) {
					return NULL;
				}
#endif
				if (checkfor (S_RPAREN)) {
					return NULL;
				}
				/*}}} */
			}
			node = newlitnode (s, locn, (treenode *) w, ss);
		}
		break;
		/*}}} */
		/*{{{  case S_STRING, S_STRINGCONT */
	case S_STRING:
	case S_STRINGCONT:
		node = rstring ();
		nextsymb ();
#ifdef OCCAM2_5
		if (symb == S_LPAREN)
			/*{{{  type decoration */
		{
			treenode *type = NULL;
			nextsymb ();
			type = (treenode *) rname ();
			if (checkfor (S_RPAREN))
				return NULL;
			SetConstTableType (node, type);
		}
		/*}}} */
#endif
		if (symb == S_LBOX)
			node = rsubscript (node);
		break;
		/*}}} */
		/*{{{  case S_NAME */
	case S_NAME:
	case S_ASMNAME:
		node = rrestofoperand ((treenode *) rname (), locn);
		break;
		/*}}} */
		/*{{{  case S_LBOX */
	case S_LBOX:
		nextsymb ();
		if ((node = rexp ()) == NULL) {
			return NULL;
		}
		if ((TagOf (node) == S_EQ) && (symb == S_FOR)) {
			/* probably a [i = s FOR l | exp] type thing */
			node = rarrayconstructor (node);
#if 0
fprintf (stderr, "roperand: rarrayconstructor returned");
printtreenl (stderr, 4, node);
#endif
		} else {
#ifdef OCCAM2_5
			if ((symb == S_FROM) || (symb == S_FOR))
#else
			if (symb == S_FROM)
#endif
			{
				node = rsegment (node);
			} else {
				node = rconstructor (node);
			}
			if ((node != NULL) && (symb == S_LBOX)) {
				node = rsubscript (node);
			}
		}
		break;
		/*}}} */
		/*{{{  BYTESIN and OFFSETOF */
#ifdef OCCAM2_5
	case S_BYTESIN:
		{
			treenode *op;

			nextsymb ();
			if (checklinebreak () || checkfor (S_LPAREN) || checklinebreak () || ((op = rtypeoroperand ()) == NULL) || checkfor (S_RPAREN)) {
				return NULL;
			}
			node = newmopnode (S_BYTESIN, locn, op, 0);
		}
		break;
	case S_OFFSETOF:
		{
			wordnode *name;
			wordnode *field;

			nextsymb ();
			if (checklinebreak () || checkfor (S_LPAREN) || checklinebreak ()) {
				return NULL;
			}
			name = rname ();
			if (checkfor (S_COMMA) || checklinebreak ()) {
				return NULL;
			}
			field = rname ();
			if (checkfor (S_RPAREN)) {
				return NULL;
			}
			node = newdopnode (S_OFFSETOF, locn, (treenode *) name, (treenode *) field, 0);
		}
		break;
#endif
	case S_BOX:
		{
			/* this now means empty array -- which is a valid operand */
			nextsymb ();
			return newleafnode (S_NULLARRAY, locn);
		}
		/*}}} */
#ifdef MOBILES
	case S_ADDROF:
	case S_HWADDROF:
#endif
	case S_TYPEHASHOF:
		{
			const int s = symb;
			treenode *op;

			nextsymb ();
			if (checklinebreak () || checkfor (S_LPAREN) || checklinebreak () || ((op = rtypeoroperand ()) == NULL) || checkfor (S_RPAREN)) {
				return NULL;
			}
			node = newmopnode (s, locn, op, 0);
		}
		break;
	default:
		synerr_e (SYN_E_OPERAND, flocn, symb);
		break;
	}
#ifdef OCCAM2_5
	if ((node != NULL) && (symb == S_LBOX)) {
		node = rsubscript (node);
	}
#endif
	return node;
}

/*}}}*/
/*{{{  PRIVATE treenode *rconversion (const int type_tag, treenode * const type, const SOURCEPOSN locn)*/
PRIVATE treenode *rconversion (const int type_tag, treenode * const type, const SOURCEPOSN locn)
{
	treenode *op;
	treenode *conversion;
	int tag = S_EXACT;

	if ((symb == S_ROUND) || (symb == S_TRUNC)) {
		tag = symb;
		nextsymb ();
		/*{{{  ignore line break */
		if (checklinebreak ())
			return NULL;
		/*}}} */
	}
	if ((op = roperand ()) == NULL)
		return NULL;
	if (type != NULL) {
		op = newlistnode (S_LIST, locn, type, newlistnode (S_LIST, locn, op, NULL));
	}
	conversion = newmopnode (tag, locn, op, type_tag);
	return (conversion);
}

/*}}}*/
/*{{{  PRIVATE treenode *rrestofexp (treenode *a, SOURCEPOSN locn)*/
/* May return NULL if error found */
/* Parse
{ dyadic.op operand }
*/
PRIVATE treenode *rrestofexp (treenode *a, SOURCEPOSN locn)
{
	BOOL assoc = TRUE;
	int assoc_symb = S_END;	/* anything except S_AND or S_OR *//* bug 1361 19/9/91 */
#if 0
fprintf (stderr, "rrestofexp: symb = %d\n", symb);
#endif
	/*{{{  build up expression */
	while (assoc) {
		treenode *operand;
		const int operator = symb;
		switch (symb) {
			/*{{{  S_AND S_OR */
		case S_AND:
		case S_OR:
			if (assoc_symb == S_END)	/* first time through */
				/* bug 1361 19/9/91 */
				assoc_symb = symb;
			else if (symb != assoc_symb)	/* mixtures of AND and OR */
				/* bug 1361 19/9/91 */
				return (a);	/* do the same as the default case */
			assoc = TRUE;
			break;
			/*}}} */
			/*{{{  ADD SUBTRACT MULT DIV REM BITAND BITOR XOR LSHIFT RSHIFT PLUS... */
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
#include "casedops.h"		/* added by Jim 2/2/97 */
			if ((symb == S_DQUESTIONM) && extended_input) {
				/* special case for this, means it might break ?? as a UDO.. */
				return a;
			}
			assoc = FALSE;
			break;
			/*}}} */
		default:
			return (a);
		};
		nextsymb ();
		if (checklinebreak ())
			return NULL;
		if ((operand = roperand ()) == NULL)
			return NULL;
		a = newdopnode (operator, locn, a, operand, 0);
	};
	/*}}} */
	return (a);
}

/*}}}*/
/*{{{  PRIVATE treenode *rtypeoroperand (void)*/
#ifdef OCCAM2_5
PRIVATE treenode *rtypeoroperand (void)
{
	const SOURCEPOSN locn = flocn;

	if (istypetag (symb) || symb == S_BOX)
		return rspecifier ();

	if (symb == S_LBOX)
		/*{{{  S_LBOX    we don't know which */
	{
		treenode *spec;
		treenode *e;
		nextsymb ();
		if ((e = rexp ()) == NULL)
			return NULL;
		if (symb == S_RBOX) {
			nextsymb ();
			spec = rspecifier_constr ();
			if (!spec) {
				/* One element constructor */
				e = rendofconstructor (newlistnode (S_LIST, flocn, e, NULL), flocn);
				return e;
			} else {
				return (newtypenode (S_ARRAY, locn, e, spec));
			}
		}
		if ((symb == S_FROM) || (symb == S_FOR))	/* Note we know we're in OCCAM2_5 */
			e = rsegment (e);
		else
			e = rconstructor (e);
		if (e != NULL && symb == S_LBOX)
			e = rsubscript (e);
		return e;
	}
	/*}}} */

	return roperand ();
}
#endif
/*}}}*/
/*{{{  PUBLIC treenode *rexp (void)*/
/* Expressions may be broken after an expression operator - the indentation
of the first symbol on the continuation line must be at least as great
as that of the first line.
*/
PUBLIC treenode *rexp (void)
{
	const SOURCEPOSN locn = flocn;
	DEBUG_MSG (("rexp... "));

	switch (symb) {
		/*{{{  case S_MOSTPOS S_MOSTNEG */
	case S_MOSTPOS:
	case S_MOSTNEG:
		{
			const int s = symb;
			treenode *op;
			nextsymb ();
			op = rspecifier ();
			return (op == NULL) ? NULL : newmopnode (s, locn, op, 0);
		}
		/*}}} */
#ifdef MOBILES
		/*{{{  case S_ADDROF S_HWADDROF S_TYPEHASHOF*/
	case S_ADDROF:
	case S_HWADDROF:
	case S_TYPEHASHOF:
		{
			treenode *op = roperand ();
			return (op == NULL ? NULL : rrestofexp (op, locn));
		}
		/*}}}*/
		/*{{{  case S_CLONE */
	case S_CLONE:
		{
			treenode *op;

			nextsymb ();
			op = roperand ();		/* pick up MOBILE variable */
			if (!op) {
				return NULL;
			} else {
				return newmopnode (S_CLONE, locn, op, 0);
			}
			break;
		}
		/*}}}  */
		/*{{{  case S_MOBILE */
	case S_MOBILE:
		/* this is the new way of handling dynamic mobile array allocation, eg, "array := MOBILE [dim]TYPE",
		 * and also for allocating MOBILE processes */
		{
			treenode *op, *dimlist;
			int dimcount = 0;

			dimlist = NULL;
			nextsymb ();
			if (symb == S_NAME) {
				/* should be a MOBILE process allocation */
				treenode *pname = (treenode *)rname ();
				op = newtypenode (S_ALLOC_PROC, locn, NULL, pname);
			} else if (symb == S_BARRIER) {
				/* MOBILE barrier allocation */
				nextsymb ();
				op = newtypenode (S_NEW_BARRIER, locn, NULL, newleafnode (S_FULLBARRIER, locn));
			} else {
				BOOL dma = FALSE;
				BOOL empty = FALSE;
				treenode *alignment = NULL;

				while (symb == S_ALIGNMENT || symb == S_DMA || symb == S_EMPTY) {
					if (symb == S_ALIGNMENT) {
						nextsymb ();
						if (symb == S_LPAREN) {
							nextsymb ();
							alignment = roperand ();
						} else {
							synerr_e (SYN_E_LPAREN, locn, symb);
							return NULL;
						}
						if (symb != S_RPAREN) {
							synerr_e (SYN_E_RPAREN, locn, symb);
							return NULL;
						}
						nextsymb ();
					} else if (symb == S_DMA) {
						nextsymb ();
						dma = TRUE;
					} else if (symb == S_EMPTY) {
						nextsymb ();
						empty = TRUE;
					}
				}
				if (symb != S_LBOX) {
					synerr_e (SYN_E_EXPR, locn, symb);
					return NULL;
				}
				/*{{{  collect dimensions*/
				while (symb == S_LBOX) {
					treenode *dimexpr;

					nextsymb ();
					dimexpr = rexp ();
					if (!dimexpr) {
						return NULL;
					}
					/* symb should be RBOX */
					if (symb != S_RBOX) {
						return NULL;
					}
					nextsymb ();

					/* save dimension expression */
					dimcount++;
					dimlist = newlistnode (S_LIST, locn, dimexpr, dimlist);
				}
				/*}}}*/
				/*{{{  handle base type and build NEW_ARRAY node*/
				/* should now have a type for the array */
				switch (symb) {
					/*{{{  BOOL/BYTE/INT/INT16/INT32/INT64/REAL32/REAL64*/
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
					op = newtypenode (S_NEW_ARRAY, locn, dimlist, newleafnode (symb, locn));
					nextsymb ();
					break;
					/*}}}*/
					/*{{{  CHAN: allow MOBILE []CHAN arrays! */
				case S_CHAN:
					nextsymb ();
					if (symb == S_OF) {
						/* optional OF */
						nextsymb ();
					}
					switch (symb) {
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
						op = newtypenode (S_NEW_ARRAY, locn, dimlist, newtypenode (S_CHAN, locn, NULL, newleafnode (symb, locn)));
						nextsymb ();
						break;
#ifdef OCCAM2_5
					case S_NAME:
						op = newtypenode (S_NEW_ARRAY, locn, dimlist, newtypenode (S_CHAN, locn, NULL, (treenode *)rname ()));
						nextsymb ();
						break;
#endif	/* OCCAM2_5 */
					default:
						synerr_e (SYN_E_PTYPE, locn, symb);
						op = NULL;
						break;
					}
					break;
					/*}}}  */
#ifdef OCCAM2_5
					/*{{{  SHARED*/
				case S_SHARED:
					{
						treenode *lname;

						/* must be a chan-type name up next */
						nextsymb ();
						lname = (treenode *)rname ();
						if (!lname) {
							synerr_e (SYN_E_CHANTYPE, locn, symb);
							return NULL;
						}
						if (symb == S_INPUT) {
							lname = newmopnode (S_ASINPUT, locn, lname, S_NAME);
							nextsymb ();
						} else if (symb == S_OUTPUT) {
							lname = newmopnode (S_ASOUTPUT, locn, lname, S_NAME);
							nextsymb ();
						}
						SetOpTypeAttr (lname, OpTypeAttrOf (lname) | TypeAttr_shared);
						op = newtypenode (S_NEW_ARRAY, locn, dimlist, lname);
					}
					break;
					/*}}}*/
					/*{{{  NAME*/
				case S_NAME:
					{
						treenode *lname = (treenode *)rname ();
						if (symb == S_INPUT) {
							/* might be allocation of a dynamic array of channel types */
							lname = newmopnode (S_ASINPUT, locn, lname, S_NAME);
							nextsymb ();
						} else if (symb == S_OUTPUT) {
							/* ditto */
							lname = newmopnode (S_ASOUTPUT, locn, lname, S_NAME);
							nextsymb ();
						}
						op = newtypenode (S_NEW_ARRAY, locn, dimlist, lname);
						/* nextsymb (); */
					}
					break;
					/*}}}*/
					/*{{{  MOBILE*/
				case S_MOBILE:
					{
						treenode *stype;
						treenode **basehook;

						/* explicit nested MOBILE allocation, eg: ".. := MOBILE [x]MOBILE .."  */
						/* only allowing MOBILE [x]MOBILE []<simple> at the moment */
						nextsymb ();
						if (symb != S_BOX) {
							synerr (SYN_MOBILE_SUBTYPE, locn);
							return NULL;
						}
						stype = newtypenode (S_ARRAY, locn, NULL, NULL);
						basehook = ARTypeAddr (stype);
						nextsymb ();
						while (symb == S_BOX) {
							stype = newtypenode (S_ARRAY, locn, NULL, stype);
							nextsymb ();
						}
						switch (symb) {
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
							*basehook = newleafnode (symb, locn);
							nextsymb ();
							break;
#ifdef OCCAM2_5
						case S_NAME:
							*basehook = (treenode *)rname ();
							nextsymb ();
							break;
#endif	/* OCCAM2_5 */
						default:
							synerr_e (SYN_E_PTYPE, locn, symb);
							return NULL;
							break;
						}
						op = newtypenode (S_NEW_ARRAY, locn, dimlist, newtypenode (S_MOBILE, locn, NULL, stype));
					}
					break;
					/*}}}*/
#endif	/* OCCAM2_5 */
				default:
					synerr_e (SYN_E_PTYPE, locn, symb);
					op = NULL;
					break;
				}
				/*}}}*/
				/*{{{  attributes */
				if (op != NULL) {
					if (alignment != NULL) {
						SetTypeAttr (op, TypeAttrOf (op) | TypeAttr_aligned);
						SetARAlignment (op, alignment);
					}
					if (dma) {
						SetTypeAttr (op, TypeAttrOf (op) | TypeAttr_dma);
					}
					if (empty) {
						SetTypeAttr (op, TypeAttrOf (op) | TypeAttr_empty);
					}
				}
				/*}}}*/
			}
			return op;
		}
		/*}}}  */
		/*{{{  case S_DEFINED */
	case S_DEFINED:
		{
			treenode *op;

			nextsymb ();
			op = roperand ();		/* pick up MOBILE variable */
			if (!op) {
				return NULL;
			} else {
				return newmopnode (S_DEFINED, locn, op, 0);
			}
			break;
		}
		/*}}}*/
#endif	/* MOBILES */
		/*{{{  case S_BOOL S_BYTE S_INT S_INTn S_REALn */
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
			const int s = symb;
			nextsymb ();
			return rconversion (s, NULL, locn);
		}
		/*}}} */
		/*{{{  LBOX TRUE FALSE BYTELIT UINTLIT UREALLIT STRING STRINGCONT LPAREN */
	case S_LBOX:
	case S_TRUE:
	case S_FALSE:
	case S_ASMNAME:
	case S_UBYTELIT:
	case S_UINTLIT:
	case S_UREALLIT:
	case S_STRING:
	case S_STRINGCONT:
	case S_LPAREN:
		{
			treenode *op = roperand ();

			if (op) {
				op = rrestofexp (op, locn);
			}
			return op;
		}
		/*}}} */
		/*{{{  case S_BOX */
	case S_BOX:
		{
			treenode *op;

			/* this is fairly special -- typechecks against any open array type. */
			op = newleafnode (S_NULLARRAY, locn);
			nextsymb ();
			return op;
		}
		/*}}}*/
		/*{{{  case S_NAME */
	case S_NAME:
#ifdef OCCAM2_5
		{
			treenode *op = (treenode *) rname ();
			switch (symb) {
			case S_ROUND:
			case S_TRUNC:	/* obviously conversions */
				/*{{{  or beginning of an operand */
				/* Anything which might start an operand is permitted here,
				   because we're looking at
				   expression = conversion
				   = name operand
				   or
				   expression = operand
				   = element | name ( expression )
				 */

			case S_NAME:
/*case S_LPAREN: *//* ambiguous, decode as fn instance, then decide
   later */
				/*}}} */
#if 0
			case S_LBOX:
#endif
			case S_TRUE:
			case S_FALSE:
			case S_UINTLIT:
			case S_UREALLIT:
			case S_UBYTELIT:
			case S_STRING:
			case S_STRINGCONT:
				return rconversion (S_NAME, op, locn);

			default:
				/* do the same as 'roperand', except that we have already
				   read the first name */
				op = rrestofoperand (op, locn);
#if 0
fprintf (stderr, "rexp: just did rrestofoperand (op, locn).  op now ");
printtreenl (stderr, 4, op);
#endif
				if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
					treenode *base;

					/* walk through array subscripts */
					for (base = op; TagOf (base) == S_ARRAYSUB; base = ASBaseOf (base));

					/* thing at the bottom must be a name */
					if (TagOf (base) == S_NAME) {
#if 0
fprintf (stderr, "rexp: found INPUT/OUTPUT after name");
printtreenl (stderr, 4, op);
#endif
						/* could be a direction specifier following a name */
						if (symb == S_INPUT) {
							op = newmopnode (S_ASINPUT, locn, op, S_NAME);
						} else {
							op = newmopnode (S_ASOUTPUT, locn, op, S_NAME);
						}
						nextsymb ();
					}
				}
				if (!op) {
					return NULL;
				} else {
					op = rrestofexp (op, locn);
#if 0
fprintf (stderr, "rexp: just did rrestofexp (op, locn).  op now ");
printtreenl (stderr, 4, op);
#endif
					return op;
				}
			} /* switch (symb) */
		}
#else
		{
			treenode *op = roperand ();
			return (op == NULL ? NULL : rrestofexp (op, locn));
		}
#endif

		/*}}} */
		/*{{{  case S_SUBTRACT S_MINUS S_BITNOT S_NOT S_SIZE */
	case S_SUBTRACT:
	case S_MINUS:
	case S_ADD:
	case S_BITNOT:
	case S_NOT:
#include "casedops.h"
		{
			treenode *op;
			int s = symb;
			if (s == S_SUBTRACT) {
				s = S_NEG;
			} else if (s == S_MINUS) {
				s = S_UMINUS;
			} else if (s == S_ADD) {
				/* oh well, we don't actually use S_UPLUS.. */
				nextsymb ();
				return rexp ();
			} else {
				/* added by Jim -- turns DOP operator into MOP operator */
#ifdef USER_DEFINED_OPERATORS
				switch (s) {
				case S_DQUESTIONM:
					s = S_M_DQUESTIONM;
					break;
				case S_DAMPERSAT:
					s = S_M_DAMPERSAT;
					break;
				case S_DDOLLAR:
					s = S_M_DDOLLAR;
					break;
				case S_PERCENT:
					s = S_M_PERCENT;
					break;
				case S_DPERCENT:
					s = S_M_DPERCENT;
					break;
				case S_DAMPERSAND:
					s = S_M_DAMPERSAND;
					break;
				case S_LEFTPERCENT:
					s = S_M_LEFTPERCENT;
					break;
				case S_PERCENTRIGHT:
					s = S_M_PERCENTRIGHT;
					break;
				case S_AMPERSANDRIGHT:
					s = S_M_AMPERSANDRIGHT;
					break;
				case S_LEFTAMPERSAND:
					s = S_M_LEFTAMPERSAND;
					break;
				case S_ROTATELEFT:
					s = S_M_ROTATELEFT;
					break;
				case S_ROTATERIGHT:
					s = S_M_ROTATERIGHT;
					break;
				case S_DPLUS:
					s = S_M_DPLUS;
					break;
				case S_DEXCLAIMATION:
					s = S_M_DEXCLAIMATION;
					break;
				case S_DEQUALS:
					s = S_M_DEQUALS;
					break;
				case S_LEFTAMPERSAT:
					s = S_M_LEFTAMPERSAT;
					break;
				case S_AMPERSATRIGHT:
					s = S_M_AMPERSATRIGHT;
					break;
				case S_AMPERSAT:
					s = S_M_AMPERSAT;
					break;
				case S_HAT:
					s = S_M_HAT;
					break;
				}
#endif
			}
			nextsymb ();
			if (checklinebreak ())
				return NULL;
			op = roperand ();
			return (op == NULL ? NULL : newmopnode (s, locn, op, 0));
		}
	case S_SIZE:
		{
			treenode *op;
			nextsymb ();
			if (checklinebreak ())
				return NULL;
#ifdef OCCAM2_5
			op = rtypeoroperand ();
#else
			op = roperand ();
#endif
			/* might have a channel-direction specifier here */
			if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
				op = newmopnode ((symb == S_INPUT) ? S_ASINPUT : S_ASOUTPUT, locn, op, S_CHAN);
				nextsymb ();
			}
			return (op == NULL ? NULL : newmopnode (S_SIZE, locn, op, 0));
		}
		/*}}} */
		/*{{{  BYTESIN and OFFSETOF */
#ifdef OCCAM2_5
	case S_BYTESIN:
	case S_OFFSETOF:
		{
			treenode *op = roperand ();
			return (op == NULL ? NULL : rrestofexp (op, locn));
		}
#endif
		/*}}} */
	default:
		synerr_e (SYN_E_EXPR, locn, symb);
		return NULL;
	}
}

/*}}}*/

/*{{{  PRIVATE treenode *rtypeetc (BOOL *const specflag, int *const error_code, const BOOL exprlist_permitted, const BOOL fn_permitted)*/
#ifdef OCCAM2_5
/*{{{  what we are parsing*/
/* This reads a line which has a type decl at the start
   (ie INT, BYTE, etc, or, more problematically, a name)
   and decides whether it is
     type TRUNC/ROUND operand
     type operand
     type FUNCTION etc
     type, type FUNCTION etc
     type name :
     type name, name :
     type name IS/RETYPES ...
     name IS ...
     name, exp, exp, ... <NL>   -- (CASE selection)

if exprlist_permitted is TRUE, we must allow for a list of expressions
  (eg selection list in a CASE)
if fn_permitted is TRUE, we can look for comma separated lists of names
   as fn definitions. We need this falg to be able to distinguish
   between names in a selection list, and named types in function definitions.

*/
/*}}}*/
#define DEBUG_RTYPEETC 0
PRIVATE treenode *rtypeetc (BOOL *const specflag, int *const error_code, const BOOL exprlist_permitted, const BOOL fn_permitted)
{
	SOURCEPOSN locn = flocn;
	const int indent = symbindent;
	const int ttag = symb;
	const BOOL line_starts_with_name = (ttag == S_NAME);
	treenode *type;

	*error_code = 0;

#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc... (symb = %d, exprlist_permitted = %d, fn_permitted = %d)\n", symb, exprlist_permitted, fn_permitted);
#endif
	/*{{{  read in user name/type, then move to next symbol */
	if (line_starts_with_name) {
		type = (treenode *) rname ();
	} else {
		nextsymb ();
		type = NULL;
	}
	/*}}} */

	if ((symb == S_TRUNC) || (symb == S_ROUND)) {
		/*{{{  conversion  so expression */
		treenode *const conversion = rconversion (ttag, type, locn);
		if (conversion == NULL) {	/*goto error; */
			*error_code = 1;
#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: returning NULL, failed rconversion()\n");
#endif
			return NULL;
		}
#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: returning something from rconversion()\n");
#endif
		return conversion;
		/*}}} */
	} else if (line_starts_with_name && (symb == S_IS)) {
		/*{{{  it was an abbreviation */
		treenode *rest = rrestofspec (NULL, type, locn, indent, TRUE);
		*specflag = TRUE;
#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: returning something from rrestofspec()\n");
#endif
		return rest;
		/*}}} */
	} else if ((fn_permitted && ((symb == S_COMMA) || (symb == S_FUNCTION) || (symb == S_INLINE) || (symb == S_RECURSIVE) || (symb == S_REC)))
		 || (exprlist_permitted && (symb == S_COMMA))) {

		/*{{{  it must be a function definition, or a CASE selection list */
		BOOL local_specflag = FALSE;
		treenode *a = line_starts_with_name ? type : newleafnode (ttag, locn);

		if (symb == S_COMMA) {
			/*{{{  read in the rest of the specifiers/expressions */
			treenode *list;
			nextsymb ();
			if (checklinebreak ()) {	/*goto error2; */
				*error_code = 2;
#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: returning NULL\n");
#endif
				return NULL;
			}
			list = (fn_permitted && exprlist_permitted)
				? rspecorexprlist (&local_specflag)
				: fn_permitted ? rlist (rspecifier, S_COMMA)
				: rlist (rexp, S_COMMA);
			if (list == NULL) {	/*goto error; */
				*error_code = 1;
#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: returning NULL\n");
#endif
				return NULL;
			}
			a = newlistnode (S_LIST, locn, a, list);
			/*}}} */
		}
		if (fn_permitted && ((symb == S_FUNCTION) || (symb == S_INLINE) || (symb == S_RECURSIVE) || (symb == S_REC))) {
			treenode *func = rfunctiondef (a, locn, indent);
			*specflag = TRUE;
#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: returning something from rfunctiondef()\n");
#endif
			return func;
		} else {
			/*{{{  must be an expression list */
#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: returning `a', assuming expression list\n");
#endif
			return a;	/* expression list */
			/*}}} */
		}
		/*}}} */
	} else if ((symb == S_NAME)) {
		/*{{{  could still be conversion or specification */
		treenode *list = NULL;
		wordnode *name = rname ();

#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: it was a NAME\n");
#endif
		if (symb == S_COMMA) {
			/*{{{  it must be a declaration */
			treenode *name_list;
			treenode *nptr;
			*specflag = TRUE;
			nextsymb ();
			if (checklinebreak ()) {	/*goto error2; */
				*error_code = 2;
				return NULL;
			}
			if ((name_list = rlist ((treenode * (*)())rname, S_COMMA)) == NULL) {	/*goto error; */
				*error_code = 1;
				return NULL;
			}
			nptr = newlistnode (S_LIST, locn, (treenode *) name, name_list);
			return rrestofspec (line_starts_with_name ? type : newleafnode (ttag, locn), nptr, locn, indent, TRUE);
			/*}}} */
		}
		/* We have type in atag, name in 'name' */
		/*{{{  we have reached the IS COLON or RETYPES moment of truth for spec */
		switch (symb) {
			/*{{{  next symbol is IS, RETYPES or COLON => specification */
		case S_IS:
		case S_RETYPES:
		case S_COLON:
		case S_RESHAPES:
			{
				treenode *rest;

				*specflag = TRUE;
				list = (treenode *)name;
				rest = rrestofspec (line_starts_with_name ? type : newleafnode (ttag, locn), list, locn, indent, TRUE);
#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: returning something from rrestofspec()\n");
#endif
				return rest;
			}
			/*}}} */
		default:
			/*{{{  it was an expression after all */
			{
				treenode *b;
				/*if (list == NULL) */
				/*{{{  read the rest of this expression */
				{
					/* we have got to: expression = name name
					   now we're checking that the second name isn't part of
					   a larger operand
					 */
					b = rrestofoperand ((treenode *) name, locn);
					if (b == NULL) {	/*goto error; */
						*error_code = 1;
						return NULL;
					}

					if (line_starts_with_name) {
						b = newlistnode (S_LIST, locn, type, newlistnode (S_LIST, locn, b, NULL));
					}
					b = newmopnode (S_EXACT, locn, b, ttag);
				}
				/*}}} */
#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: returning something from rrestofoperand()\n");
#endif
				return (b);
			}
			/*}}} */
		}
		/*}}} */
		/*}}} */
	} else {
		BOOL is_conversion = !line_starts_with_name;
#ifdef MOBILES
		BOOL is_decl = FALSE;

#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: else case, expression (possibly conversion) or chan-type decl\n");
#endif
		/*{{{  check for declaration of channel-types*/
		if (line_starts_with_name && ((symb == S_INPUT) || (symb == S_OUTPUT))) {
			/* this is either an input/output node, or a channel-type declaration */
			/*{{{  see if it must be a declaration or not*/
			lex_save_state ();
			nextsymb ();
			if (symb == S_NAME) {
				nextsymb ();
				if ((symb == S_COLON) || (symb == S_COMMA)) {
					is_decl = TRUE;
				}
			}
			lex_restore_state ();
			/*}}}*/
			if (is_decl) {
				/* oki, symb is still input/output, type is basetype */
				wordnode *name;

				if (symb == S_INPUT) {
					type = newmopnode (S_ASINPUT, flocn, type, S_MOBILE);
				} else {
					type = newmopnode (S_ASOUTPUT, flocn, type, S_MOBILE);
				}
				nextsymb ();

				*specflag = TRUE;
				/* definitely a name now! */
				if (symb != S_NAME) {
					synerr_e (SYN_E_NAME, flocn, symb);
					*error_code = 2;
					return NULL;
				}
				name = rname ();
				if (symb == S_COMMA) {
					/* declaring a list of these */
					treenode *name_list, *nptr;

					nextsymb ();
					if (checklinebreak ()) {
						*error_code = 2;
						return NULL;
					}
					if ((name_list = rlist ((treenode *(*)())rname, S_COMMA)) == NULL) {
						*error_code = 1;
						return NULL;
					}
					nptr = newlistnode (S_LIST, locn, (treenode *)name, name_list);
					return rrestofspec (type, nptr, locn, indent, TRUE);
				} else if (symb == S_COLON) {
					treenode *rest;

					rest = rrestofspec (type, (treenode *)name, locn, indent, TRUE);
					return rest;
				} else {
					*error_code = 1;
					return NULL;
				}
			}
		}
		/*}}}*/
#endif
		/*{{{  expression (possibly conversion) */

		/*{{{  check for start of another operand */
		if (line_starts_with_name) {
			/*{{{  check for start of next operand */
			switch (symb) {
				/* Anything which might start an operand is permitted here,
				   because we're looking at
				   expression = conversion
				   = name operand
				   or
				   expression = operand
				   = element | name ( expression )
				 */

			case S_NAME:
/*case S_LPAREN: *//* ambiguous, decode as fn instance, then decide
   later */
			case S_TRUE:
			case S_FALSE:
			case S_UINTLIT:
			case S_UREALLIT:
			case S_UBYTELIT:
			case S_STRING:
			case S_STRINGCONT:
				is_conversion = TRUE;
				break;
			default:
				break;
			}
			/*}}} */
		}
		/*}}} */

		if (is_conversion) {
			/*{{{  it must be a conversion */
			treenode *const conversion = rconversion (ttag, type, locn);
			if (conversion == NULL) {	/*goto error; */
				*error_code = 1;
#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: returning NULL, failed rconversion()\n");
#endif
				return NULL;
			}
#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: returning something from rconversion()\n");
#endif
			return (conversion);
			/*}}} */
		} else {
			/*{{{  it was an expression beginning with a name after all */
			treenode *const a = rrestofoperand (type, locn);
			if (a == NULL) {	/*goto error; */
				*error_code = 1;
#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: returning NULL, failed rrestofoperand()\n");
#endif
				return NULL;
			}
#if DEBUG_RTYPEETC
fprintf (stderr, "rtypeetc: did rrestofoperand(), about to rrestofexp()\n");
#endif
			return (rrestofexp (a, locn));
			/*}}} */
		}
		/*}}} */
	}
#if DEBUG_RTYPEETC
	fprintf (stderr, "yuk, got to the end of rtypeetc\n");
#endif
}
#endif
/*}}}*/
/*{{{  PRIVATE treenode *rspecorexpr (BOOL *const specflag, const int pindent, const BOOL exprlist_permitted, const BOOL fn_permitted)*/
/*{{{  comment*/
/* If (specflag), leaves symb on next line, otherwise leaves symb unchanged
 * note on pindent:
 *	Set to the indent of left parenthesis if we have been called
 *	from roperand, negative if called from anywhere else
 *	(rchoice or ralternative). pindent is only used if we find
 *	a VALOF: if it is negative it means that VALOF is not legal in
 *	this context.
 *
 * what we are parsing:
 *	Parse
 *	   expression
 *	 | specification
 *	   VALOF
 *	 | VALOF
 *	and return the parse tree.
 *
 *	specflag is TRUE  if a specification was found,
 *	FALSE if an expression was found.
 *	if exprlist_permitted is TRUE, we must allow for a list of expressions
 *	    (eg selection list in a CASE)
 *	if fn_permitted is TRUE, we can look for comma separated lists of names
 *	    as fn definitions. We need this falg to be able to distinguish
 *	    between names in a selection list, and named types in function definitions.
 */
/*}}}*/
PRIVATE treenode *rspecorexpr (BOOL *const specflag, const int pindent, const BOOL exprlist_permitted, const BOOL fn_permitted)
{
	SOURCEPOSN locn = flocn;
	const int indent = symbindent;
	*specflag = FALSE;

#if 0
fprintf (stderr, "rspecorexpr..  symb = %d\n", symb);
#endif
	switch (symb) {
		/*{{{  S_VALOF                              valof */
	case S_VALOF:
		{
			treenode *valof;

#if 0
fprintf (stderr, "rspecorexpr: it was a VALOF\n");
#endif
			if ((valof = rvalof ()) == NULL)
				goto error2;
			if (pindent >= 0) {
				if (checkindent (pindent))
					goto error2;
				while (symb == S_COMMENT)	/* Comments after result statement but */
					if (checknlindent (pindent))	/* before right bracket               */
						goto error2;
				return (valof);
			} else {
				synerr_e (SYN_E_EXPR, locn, S_VALOF);
				goto error2;
			}
		}
		/*}}} */
#ifndef OCCAM2_5
		/*{{{  S_BOOL S_BYTE S_INTn S_REALn         expression or specification */
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
			const int atag = symb;
			nextsymb ();

			if ((symb == S_TRUNC) || (symb == S_ROUND)) {
				/*{{{  conversion  so expression */
				treenode *op, *conversion;
				int ct = symb;
				nextsymb ();
				/*{{{  ignore line break */
				if (checklinebreak ())
					return NULL;
				/*}}} */
				if ((op = roperand ()) == NULL)
					goto error;
				conversion = newmopnode (ct, locn, op, atag);
				return (conversion);
				/*}}} */
			} else {
				/*{{{  could still be conversion or specification */
				if (((current_fe_data->fe_lang & FE_LANG_NDL) == 0) &&
				    ((symb == S_COMMA) || (symb == S_FUNCTION) || (symb == S_INLINE) || (symb == S_RECURSIVE) || (symb == S_REC))) {
					/*{{{  it must be a function definition */

					treenode *a = newleafnode (atag, locn);
					*specflag = TRUE;

					if (symb == S_COMMA) {
						/*{{{  read in the rest of the specifiers */
						treenode *list;
						nextsymb ();
						if (checklinebreak ()) {
							goto error2;
						}
						if ((list = rlist (rspecifier, S_COMMA)) == NULL) {
							goto error;
						}
						a = newlistnode (S_LIST, locn, a, list);
						/*}}} */
					}
					return (rfunctiondef (a, locn, indent));
					/*}}} */
				} else {
					if (symb != S_NAME) {
						/*{{{  it must be a conversion */
						treenode *op, *conversion;
						if ((op = roperand ()) == NULL) {
							goto error;
						}
						conversion = newmopnode (S_EXACT, locn, op, atag);
						return (conversion);
						/*}}} */
					} else {
						/*{{{  could still be conversion or specification */
						treenode *list = NULL;
						wordnode *name = rname ();
						if (symb == S_COMMA) {
#if 0				/* this is never passed as TRUE */
							if (explist) {
								/*{{{  still don't know */
								nextsymb ();
								if (checklinebreak ()) {
									goto error2;
								}
								if ((list = rlist (rexp, S_COMMA)) == NULL) {
									goto error;
								}
								/* we have type in atag, first name in name, expression list in list */
								/*}}} */
							} else
#endif	/* 0 */
							{
								/*{{{  it must be a declaration */
								treenode *name_list;
								treenode *nptr;
								*specflag = TRUE;
								nextsymb ();
								if (checklinebreak ())
									goto error2;
								if ((name_list = rlist ((treenode * (*)())rname, S_COMMA)) == NULL)
									goto error;
								nptr = newlistnode (S_LIST, locn, (treenode *) name, name_list);
								return (rrestofspec (newleafnode (atag, locn), nptr, locn, indent, TRUE));
								/*}}} */
							}
						}
						/* We have type in atag, name in 'name' */
						/*{{{  we have reached the IS COLON or RETYPES moment of truth for spec */
						switch (symb) {
							/*{{{  next symbol is IS, RETYPES or COLON => specification */
						case S_IS:
						case S_RETYPES:
						case S_COLON:
							*specflag = TRUE;
							if (list != NULL)
								list = newlistnode (S_LIST, locn, (treenode *) name, list);
							else
								list = (treenode *) name;
							return (rrestofspec (newleafnode (atag, locn), list, locn, indent, TRUE));
							/*}}} */
							/*{{{  it was an expression after all */
						default:
							{
								treenode *b;
								if (list == NULL)
									/*{{{  read the rest of this expression */
								{
									b = rrestofoperand ((treenode *) name, locn);
									if (b == NULL)
										goto error;
									b = newmopnode (S_EXACT, locn, b, atag);
								}
								/*}}} */
								else {
									b = newlistnode (S_LIST, locn,
											 newmopnode (S_EXACT, locn, (treenode *) name, atag), list);
								}
								return (b);
							}
							/*}}} */
						}
						/*}}} */
						/*}}} */
					}
				}
				/*}}} */
			}
		}
		/*}}} */
		/*{{{  S_NAME                               specification or expression */
	case S_NAME:
		/* Could be an abbreviation without a specifier, or an expression */
		{
			treenode *const name = (treenode *) rname ();
			USE_VAR (exprlist_permitted);	/* remove unused variable warning */
			USE_VAR (fn_permitted);	/* remove unused variable warning */

			if (symb == S_IS)
				/*{{{  it was an abbreviation */
			{
				*specflag = TRUE;
				return (rrestofspec (NULL, name, locn, indent, TRUE));
			}
			/*}}} */
			else
				/*{{{  it was an expression after all */
			{
				treenode *const a = rrestofoperand (name, locn);
				if (a == NULL)
					goto error;
				return (rrestofexp (a, locn));
			}
			/*}}} */
		}
		/*}}} */
#else	/* OCCAM2_5 */
		/*{{{  S_BOOL S_BYTE S_INTn S_REALn, S_NAME expression or specification */
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
	case S_NAME:
		{
			int error_code;
			treenode *const a = rtypeetc (specflag, &error_code,
						      exprlist_permitted, fn_permitted);

#if 0
fprintf (stderr, "rspecorexpr: it was a primative type or name, and rtypeetc() was called\n");
#endif
			if (a == NULL) {
				if (error_code == 1)
					goto error;
				if (error_code == 2)
					goto error2;
			}
			return a;
		}
		/*}}} */
#endif	/* OCCAM2_5 */
		/*{{{  S_LBOX                               specification or expression */
	case S_LBOX:
		/* Could be start of specifier in a specification,
		   or segment in an expression, or constructor in an expression */
		{
			treenode *a;
			nextsymb ();
			{
				if ((a = rexp ()) != NULL) {
					if (symb == S_RBOX)
						/*{{{  constructor with one item, or specifier in specification */
					{
						nextsymb ();
						switch (symb) {
							/*{{{  S_LBOX or primitive type specifier in specification */
						case S_LBOX:
						case S_CHAN:
						case S_PORT:
						case S_TIMER:
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
							CASE_CONFIG_TYPE
#ifdef OCCAM2_5
						case S_NAME:
#endif
							{
								treenode *spec;
								*specflag = TRUE;
								if ((spec = rspecifier ()) == NULL) {
									goto error2;
								}
								a = newtypenode (S_ARRAY, locn, a, spec);
								if (symb == S_NAME) {
									/*{{{  case S_NAME */
									wordnode *name = rname ();
									treenode *nptr;

									if (symb == S_COMMA) {
										treenode *list;
										nextsymb ();
										if (checklinebreak ())
											goto error2;
										if ((list = rlist ((treenode * (*)())rname, S_COMMA)) == NULL)
											goto error;
										nptr = newlistnode (S_LIST, locn, (treenode *) name, list);
									} else
										nptr = (treenode *) name;
									return rrestofspec (a, nptr, locn, indent, TRUE);
									/*}}} */
								} else {
									/*{{{  otherwise, possible FUNCTION */
									if ((current_fe_data->fe_lang & FE_LANG_NDL) == 0)
										switch (symb) {
											/*{{{  case S_COMMA */
										case S_COMMA:
											{
												treenode *list;
												nextsymb ();
												if (checklinebreak ())
													goto error2;
												if ((list = rlist (rspecifier, S_COMMA)) == NULL)
													goto error2;
												a = newlistnode (S_LIST, locn, a, list);
												return (rfunctiondef (a, locn, indent));
											}
											/*}}} */
											/*{{{  FUNCTION */
										case S_INLINE:
										case S_FUNCTION:
										case S_RECURSIVE:
										case S_REC:
											return (rfunctiondef (a, locn, indent));
											/*}}} */
										default:
											break;
										}
									synerr_e (SYN_E_SPEC, locn, symb);
									goto error;
									/*}}} */
								}
							}
							/*}}} */
						default:
							a = rendofconstructor (newlistnode (S_LIST, locn, a, NULL), locn);
							break;
						}
					}
					/*}}} */
					else if (symb == S_COMMA)
						a = rconstructor (a);
#ifdef OCCAM2_5
					else if ((symb == S_FROM) || (symb == S_FOR))
#else
					else if (symb == S_FROM)
#endif
						a = rsegment (a);
					else {
						synerr_e (SYN_E_EXPR_OR_SPEC, locn, symb);
						goto error;
					}
				}
			}
			if (a == NULL)
				goto error;
			if (symb == S_LBOX)
				a = rsubscript (a);
			return (rrestofexp (a, locn));
		}
		/*}}} */
	default:
#if 0
fprintf (stderr, "rspecorexpr: default case.. (symb = %d)\n", symb);
#endif
		if (mustbespec (symb)) {
			*specflag = TRUE;
			return (rspecification ());
		}
		if (mustbeexp (symb)) {
			return (rexp ());
		}
		synerr_e (SYN_E_EXPR_OR_SPEC, locn, symb);
	      error:
		if (*specflag)
			nextline ();
	      error2:
		if (*specflag)
			skiplines (indent);
		return NULL;
	}
}
/*}}}*/
/*}}}*/
/*{{{  specification or expression lists*/
#ifdef OCCAM2_5
PRIVATE BOOL rspecorexprlist_found_spec;
/*{{{  PRIVATEPARAM treenode *sub_rspecorexprlist ()*/
PRIVATEPARAM treenode *sub_rspecorexprlist ()
{
	BOOL specflag;
	treenode *tptr;
	tptr = rspecorexpr (&specflag, -1, FALSE, FALSE);
	if (specflag)
		rspecorexprlist_found_spec = TRUE;
	return tptr;
}

/*}}}*/
/*{{{  PRIVATE treenode *rspecorexprlist (BOOL *specflag)*/
PRIVATE treenode *rspecorexprlist (BOOL *specflag)
{
	/* parses a list for either a list of exprs (selection list)
	   or the lhs of a function definition
	 */
	treenode *list;
	const BOOL saved_found_spec = rspecorexprlist_found_spec;
	rspecorexprlist_found_spec = FALSE;
	list = rlist (sub_rspecorexprlist, S_COMMA);
	*specflag = rspecorexprlist_found_spec;
	rspecorexprlist_found_spec = saved_found_spec;
	return list;
}

/*}}}*/
#endif
/*}}}*/

/* These may return NULL, but will always move onto the next line */
/*{{{  declarations, abbreviations and retypes*/
/* All these functions actually declare items */
/*{{{  PRIVATE treenode *rvalabbr (treenode *t, wordnode *name, SOURCEPOSN locn, int indent)*/
/* Pointer to a tree representing type */
/* Pointer to node representing name */
/* File location of start of abbreviation */
/* Indent of the specifier statement */
PRIVATE treenode *rvalabbr (treenode *t, wordnode *name, SOURCEPOSN locn, int indent)
{
	treenode *e, *retptr;
	DEBUG_MSG (("rvalabbr... "));

	if ((e = rexp ()) == NULL)
		goto error;
	if (checkfor (S_COLON))
		goto error;
	if (checknlindent (indent))
		return NULL;
	retptr = declare (S_VALABBR, locn, t, name, e);
	return retptr;

      error:
	nextline ();
	skiplines (indent);
	if (e != NULL)
		retptr = declare (S_VALABBR, locn, t, name, e);
	return NULL;
}

/*}}}*/
/*{{{  PRIVATE treenode *rvalretype (treenode *const t, wordnode *const name,*/
/* Pointer to a tree representing type */
/* Pointer to node representing name */
/* File location of start of retype */
/* Indent of the retypes statement */
PRIVATE treenode *rvalretype (treenode *const t, wordnode *const name,
#ifdef OCCAM2_5
	    const BOOL reshapes,
#endif
	    const SOURCEPOSN locn, const int indent)
{
	treenode *e, *retptr;

	DEBUG_MSG (("rvalretype... "));

	if ((e = rexp ()) == NULL)
		goto error;
	if (checkfor (S_COLON))
		goto error;
	if (checknlindent (indent))
		return NULL;

	retptr = declare (S_VALRETYPE, locn, t, name, e);
#ifdef OCCAM2_5
	SetNVReshapes (DNameOf (retptr), reshapes);
#endif
	return retptr;

      error:
	nextline ();
	skiplines (indent);
	if (e != NULL)
		retptr = declare (S_VALRETYPE, locn, t, name, e);
	return NULL;
}

/*}}}*/
/*{{{  PUBLIC treenode *rrestofspec (treenode *const t, treenode *namelist, const SOURCEPOSN locn, const int indent, const BOOL check_trailing_indent)*/
/*
 * scoops up the remainder of a specification
 *
 * "t" is the tree-structure for the specifier (type), "namelist" is
 * a name or list of names that accompanied the specifier.  What comes
 * in "namelist" might be illegal -- checks here.
 * "locn", "indent" and "check_trailing_indent" give the location of
 * the start of the specification
 *
 * returns a declaration for the specifier or NULL on error
 */
PUBLIC treenode *rrestofspec (treenode *const t, treenode *namelist, const SOURCEPOSN locn, const int indent, const BOOL check_trailing_indent)
{
	treenode *a;

	DEBUG_MSG (("rrestofspec... "));

#if 0
fprintf (stderr, "rrestofspec...\n");
#endif
#if 0
if (TagOf (t) == S_CHAN) {
fprintf (stderr, "rrestofspec; entry: t=CHAN, shared = %d\n", TypeAttrOf (t) & TypeAttr_shared);
}
#endif
	switch (symb) {
		/*{{{  case S_COLON   declaration */
	case S_COLON:		/* Declaration */
		/* Lets through unsized arrays, eg. [] INT b :
		   These are thrown out by chk */
		{
			treenode *tptr;

			nextsymb ();
			checknewline ();
			if (check_trailing_indent && checkindent (indent)) {
				goto error2;
			}

			tptr = newdeclnode (S_DECL, locn, NULL, NULL, NULL);
			if (TagOf (namelist) == S_LIST) {
				a = namelist;
				while (!EndOfList (a)) {
					/*{{{  declare name on left, move to right */
					treenode *nptr = ThisItem (a);
					if (TagOf (nptr) == S_NAME) {
						treenode *ndecl, *type;
						
						type = t;
#if 0
fprintf (stderr, "syn2: rrestofspec(): processing decl. list, type = ");
printtreenl (stderr, 4, type);
#endif
						ndecl = declname (N_DECL, locn, (wordnode *)nptr, type, tptr);

						NewItem (ndecl, a);
					} else {
						synerr_e (SYN_E_SPEC, locn, TagOf (nptr));
						goto error;
					}
					a = NextItem (a);
					/*}}} */
				}
			} else {
				namelist = declname (N_DECL, locn, (wordnode *) namelist, t, tptr);
			}
			SetDName (tptr, namelist);
			return (tptr);
		}
		/*}}} */
		/*{{{  case S_IS case S_RETYPES : */
	case S_IS:
	case S_RETYPES:
#ifdef DECL_EQ
	case S_EQ:
#endif
#ifdef OCCAM2_5
	case S_RESHAPES:
#endif
		{
#ifdef OCCAM2_5
			const BOOL reshapes = (symb == S_RESHAPES);
#endif
			const int nodetype = (symb == S_IS ? S_ABBR :
#ifdef DECL_EQ
					      symb == S_EQ ? S_DECL :
#endif
					      S_RETYPE);
			if (TagOf (namelist) == S_NAME) {
				treenode *e;
				nextsymb ();
				if (checklinebreak () || ((e = roperand ()) == NULL)) {
					goto error;
				}
				/* could have a channel-direction specifier here, is legal */
				if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
					e = newmopnode ((symb == S_INPUT) ? S_ASINPUT : S_ASOUTPUT, locn, e, S_NAME);
					nextsymb ();
#if 0
fprintf (stderr, "rrestofspec: have input/output.  t =");
printtreenl (stderr, 4, t);
fprintf (stderr, "  namelist =");
printtreenl (stderr, 4, namelist);
fprintf (stderr, "  e =");
printtreenl (stderr, 4, e);
#endif
				}
				if (checkfor (S_COLON)) {
					goto error;
				}
				checknewline ();
				if (check_trailing_indent && checkindent (indent)) {
					goto error2;
				}
				e = declare (nodetype, locn, t, (wordnode *) namelist, e);
#if 0
if (TagOf (t) == S_CHAN) {
fprintf (stderr, "rrestofspec; after declare() of t=CHAN, shared = %d\n", TypeAttrOf (t) & TypeAttr_shared);
}
#endif
#ifdef OCCAM2_5
				SetNVReshapes (DNameOf (e), reshapes);
#endif
				return e;
			} else {
				synerr (SYN_TOO_MANY_NAMES, locn);
				goto error;
			}
		}
		/*}}} */
	default:
		if ((current_fe_data->fe_lang & FE_LANG_NDL) == 0) {
			synerr_e (SYN_E_COLON_IS_RETYPES, flocn, symb);
		} else {
			synerr_e (SYN_E_COLON_IS, flocn, symb);	/* bug TS/1466 12/11/91 */
		}
	error:
		nextline ();
	error2:
		skiplines (indent);
		return NULL;
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *rspecnameandbody (treenode *const t, const SOURCEPOSN locn, const int indent, const BOOL check_trailing_indent)*/
/*
 *	scoops up name and rest of a specification
 *	"t" is the specifier, "locn", "indent" and "check_trailing_indent" refer
 *	to the start of the specifier.
 *
 *	returns a declaration or NULL on error
 */
PUBLIC treenode *rspecnameandbody (treenode *const t, const SOURCEPOSN locn, const int indent, const BOOL check_trailing_indent)
{
#if 0
if (TagOf (t) == S_CHAN) {
fprintf (stderr, "rspecnameandbody; entry: t=CHAN (%p), shared = %d\n", t, TypeAttrOf (t) & TypeAttr_shared);
}
#endif
#if 0
fprintf (stderr, "rspecnameandbody: entry: locn = %d, indent = %d, check_tl = %d, [symb = %d], t =", (int)locn, indent, (int)check_trailing_indent, (int)symb);
printtreenl (stderr, 4, t);
#endif

 	if (symb == S_NAME) {
		/*{{{  S_NAME */
		wordnode *name = rname ();
		treenode *nptr;

		if (symb == S_COMMA) {
			treenode *list;
			nextsymb ();
			if (checklinebreak ()) {
				goto error2;
			}
			if ((list = rlist ((treenode *(*)())rname, S_COMMA)) == NULL) {
				goto error;
			}
			nptr = newlistnode (S_LIST, locn, (treenode *) name, list);
		} else if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
			if (TagOf (t) == S_ANONCHANTYPE) {
				/* this really doesn't make sense! */
				synerr (SYN_UNEXPECTED_CHANDIR, flocn);
				goto error;
			} else if (basetype (t) == S_CHAN) {
				treenode *const rtype = basetype_tree (t);

				SetTypeAttr (rtype, TypeAttrOf (rtype) | ((symb == S_INPUT) ? TypeAttr_marked_in : TypeAttr_marked_out));
				nextsymb ();
				nptr = (treenode *)name;
			} else {
				const BOOL isinput = (symb == S_INPUT);

#if 0
fprintf (stderr, "rspecnameandbody: parsed name and found input/output specifier.  t =");
printtreenl (stderr, 4, t);
#endif
				nextsymb ();
				/* just eat up the specifier and put an AS_INPUT/AS_OUTPUT on the name */
				if (isinput) {
					nptr = newmopnode (S_ASINPUT, locn, (treenode *)name, S_NAME);
				} else {
					nptr = newmopnode (S_ASOUTPUT, locn, (treenode *)name, S_NAME);
				}
			}
		} else {
			nptr = (treenode *)name;
		}
#if 0
fprintf (stderr, "rspecnameandbody: about to call rrestofspec, nptr is:");
printtreenl (stderr, 4, nptr);
fprintf (stderr, "      ....      : t is");
printtreenl (stderr, 4, t);
#endif
#if 0
if (TagOf (t) == S_CHAN) {
fprintf (stderr, "rspecnameandbody: before calling rrestofspec: t=CHAN (%p), shared = %d\n", t, TypeAttrOf (t) & TypeAttr_shared);
}
#endif
		return rrestofspec (t, nptr, locn, indent, check_trailing_indent);
		/*}}} */
	} else {
		if ((current_fe_data->fe_lang & FE_LANG_NDL) == 0) {
			switch (symb) {
				/*{{{  case S_COMMA     return */
			case S_COMMA:
				{
					treenode *list;

					nextsymb ();
					if (checklinebreak ()) {
						goto error2;
					}
					if ((list = rlist (rspecifier, S_COMMA)) == NULL) {
						goto error;
					}
					list = newlistnode (S_LIST, locn, t, list);

					return (rfunctiondef (list, locn, indent));
				}
				/*}}} */
				/*{{{  case S_FUNCTION  return */
			case S_FUNCTION:
			case S_INLINE:
			case S_RECURSIVE:
			case S_REC:
				return (rfunctiondef (t, locn, indent));
				/*}}} */
			default:
				break;
			}
		}
		synerr_e (SYN_E_NAME, locn, symb);
	error:
		nextline ();
	error2:
		skiplines (indent);
		return NULL;
	}
}
/*}}}*/
/*}}}*/
/*{{{  protocol definition and instance*/
/* Only protdef actually declares items */
/*{{{  PRIVATE treenode *rpsub ()*/
/* Read a subscripted protocol type */
/* On error, leave symb unchanged */
PRIVATE treenode *rpsub (void)
{
	SOURCEPOSN locn = flocn;
	DEBUG_MSG (("rpsub... "));

	switch (symb) {
		/*{{{  case S_LBOX */
	case S_LBOX:
		{
			treenode *e, *sub;
			nextsymb ();
			if ((e = rexp ()) == NULL)
				return NULL;
			if (checkfor (S_RBOX))
				return NULL;
			if ((sub = rpsub ()) == NULL)
				return NULL;
			return (newtypenode (S_ARRAY, locn, e, sub));
		}
		/*}}} */
		/*{{{  case S_BOOL S_BYTE S_INTn S_REALn */
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
			const int s = symb;
			nextsymb ();
			return (newleafnode (s, locn));
		}
		/*}}} */
#ifdef MOBILES
		/*{{{  case S_SHARED*/
	case S_SHARED:
		{
			treenode *name;

			/* expecting "SHARED <name>[?!]"  */
			nextsymb ();
			name = (treenode *)rname ();

			if (symb == S_INPUT) {
				nextsymb ();
				name = newmopnode (S_ASINPUT, locn, name, S_NAME);
			} else if (symb == S_OUTPUT) {
				nextsymb ();
				name = newmopnode (S_ASOUTPUT, locn, name, S_NAME);
			} else {
				synerr_e (SYN_E_CHANTYPEDIRSPEC, locn, symb);
			}
			SetOpTypeAttr (name, TypeAttr_shared);
			return name;
		}
		/*}}}*/
		/*{{{  case S_MOBILE*/
	case S_MOBILE:
		{
			treenode *s, *t;
			int dimcount;

			/* expecting "MOBILE []<rpsub>" */
			nextsymb ();
			if (symb != S_BOX) {
				synerr_e (SYN_E_BOX, locn, symb);
				s = NULL;
			} else {
				dimcount = 0;
				while (symb == S_BOX) {
					dimcount++;
					nextsymb ();
				}
				s = rpsub ();
				while (dimcount) {
					s = newtypenode (S_ARRAY, locn, NULL, s);
					dimcount--;
				}
			}
			t = newtypenode (S_MOBILE, locn, NULL, s);

			return t;
		}
		/*}}}*/
#endif
		/*{{{  case S_NAME */
#ifdef OCCAM2_5
	case S_NAME:
#ifdef MOBILES
		{
			treenode *name = (treenode *)rname ();

			if (symb == S_INPUT) {
				nextsymb ();
				name = newmopnode (S_ASINPUT, locn, name, S_NAME);
			} else if (symb == S_OUTPUT) {
				nextsymb ();
				name = newmopnode (S_ASOUTPUT, locn, name, S_NAME);
			}
			return name;
		}
#else
		return ((treenode *) rname ());
#endif
#endif
		/*}}} */
	default:
		synerr_e (SYN_E_PTYPE, flocn, symb);
		return NULL;
	}
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *rsimpleprotocol ()*/
/* On error, leave symb unchanged */
PRIVATEPARAM treenode *rsimpleprotocol (void)
{
	treenode *s, *t;
	SOURCEPOSN locn = flocn;

	DEBUG_MSG (("rsimpleprotocol... "));
	switch (symb) {
		/*{{{  case S_REAL32 S_REAL64 S_BOOL */
	case S_REAL32:
	case S_REAL64:
	case S_BOOL:
#ifndef MOBILES
	case S_ANY:		/* bug 1395 Added 30/09/91 by CON */
#else
#endif
		s = newleafnode (symb, locn);
		nextsymb ();
		return s;
		/*}}} */
		/*{{{  case S_ANYCHANTYPE*/
	case S_ANYCHANTYPE:
		{
			treenode *type = newtypenode (S_MOBILE, NOPOSN, NULL, NULL);
			INT32 typeattr = 0;

			s = newleafnode (symb, locn);
			nextsymb ();
			SetLeafLink (s, type);

			if (symb == S_INPUT) {
				nextsymb ();
				typeattr |= TypeAttr_marked_in;
			} else if (symb == S_OUTPUT) {
				nextsymb ();
				typeattr |= TypeAttr_marked_out;
			}

			SetTypeAttr (type, typeattr);
			return s;
		}
		/*}}}*/
		/*{{{  case S_ANYPROCTYPE S_ANYMOBILETYPE*/
	case S_ANYPROCTYPE:
	case S_ANYMOBILETYPE:
		s = newleafnode (symb, locn);
		nextsymb ();
		return s;
		/*}}}*/
#ifdef MOBILES
		/*{{{  case S_ANY*/
	case S_ANY:
		nextsymb ();
		if (symb == S_CHAN) {
			/* probably have: ANY CHAN TYPE, try and scoop it up */
			nextsymb ();
			if (symb != S_TYPE) {
				synerr_e (SYN_E_TYPE, locn, symb);
				return NULL;
			}
			nextsymb ();
			s = newleafnode (S_ANYCHANTYPE, locn);
		} else if (symb == S_PROC) {
			/* probably have: ANY PROC TYPE, try and scoop it up */
			nextsymb ();
			if (symb != S_TYPE) {
				synerr_e (SYN_E_TYPE, locn, symb);
				return NULL;
			}
			nextsymb ();
			s = newleafnode (S_ANYPROCTYPE, locn);
		} else {
			s = newleafnode (S_ANY, locn);
		}
		return s;
		/*}}}*/
#endif
		/*{{{  case S_BYTE S_INT S_INT16 S_INT32 S_INT64 */
	case S_BYTE:
	case S_INT:
	case S_INT16:
	case S_INT32:
	case S_INT64:
	case S_UINT:
	case S_UINT16:
	case S_UINT32:
	case S_UINT64:
		s = newleafnode (symb, locn);
		nextsymb ();
		if (symb == S_COLON2) {
			/*{{{  read in array following */
			SOURCEPOSN alocn;
			nextsymb ();
			alocn = flocn;
			if (checkfor (S_BOX))
				return NULL;
			switch (symb) {
				/*{{{  case S_BOOL S_BYTE S_INT S_INTn S_REALn */
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
				t = newleafnode (symb, alocn);
				nextsymb ();
				break;
				/*}}} */
				/*{{{  case S_LBOX */
			case S_LBOX:
				if ((t = rpsub ()) == NULL)
					return NULL;
				break;
				/*}}} */
				/*{{{  case S_NAME */
#ifdef OCCAM2_5
			case S_NAME:
				t = ((treenode *) rname ());
				break;
#endif
				/*}}} */
			default:
				synerr_e (SYN_E_PTYPE, flocn, symb);
				return NULL;
			}
			s = newdopnode (S_COLON2, locn, s, newtypenode (S_ARRAY, alocn, NULL, t), 0);
			/*}}} */
		}
		return s;
		/*}}} */
		/*{{{  case S_LBOX */
	case S_LBOX:
		return (rpsub ());
		/*}}} */
#ifdef MOBILES
		/*{{{  S_MOBILE*/
	case S_MOBILE:
		if (!mobile_data_types) {
			synerr (SYN_NO_MOBILES, locn);
		}
		nextsymb ();
		switch (symb) {
		case S_BARRIER:
			s = newtypenode (S_MOBILE, locn, NULL, newleafnode (S_FULLBARRIER, locn));
			nextsymb ();
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
		case S_REAL32:
		case S_REAL64:
			s = newtypenode (S_MOBILE, locn, NULL, newleafnode (symb, locn));
			nextsymb ();
			break;
		case S_LBOX:
			t = rpsub ();
			if (t == NULL) {
				return NULL;
			}
			s = newtypenode (S_MOBILE, locn, NULL, t);
			break;
#ifdef OCCAM2_5
		case S_NAME:
			s = newtypenode (S_MOBILE, locn, NULL, (treenode *)rname ());
			break;
#endif
		case S_BOX:
			{
				/* support multiple-dimensions (all must be open) */
				int dimcount = 1;

				nextsymb ();
				while (symb == S_BOX) {
					dimcount++;
					nextsymb ();
				}
				s = rpsub ();
				while (dimcount) {
					s = newtypenode (S_ARRAY, locn, NULL, s);
					dimcount--;
				}
				s = newtypenode (S_MOBILE, locn, NULL, s);
			}
			break;
		default:
			synerr_e (SYN_E_PTYPE, flocn, symb);
			return NULL;
		}
		return s;
		/*}}}*/
		/*{{{  S_BOX*/
	case S_BOX:
		/* dynamic MOBILE array channel */
		{
			/* support multiple-dimensions (all must be open) */
			int dimcount = 1;

			nextsymb ();
			while (symb == S_BOX) {
				dimcount++;
				nextsymb ();
			}
			s = rpsub ();
			while (dimcount) {
				s = newtypenode (S_ARRAY, locn, NULL, s);
				dimcount--;
			}
			return s;
		}
		/*}}}*/
		/*{{{  S_SHARED*/
	case S_SHARED:
		/* can only be channel-type */
		{
			treenode *name = NULL;
			
			nextsymb ();
			switch (symb) {
			case S_NAME:
				name = (treenode *)rname ();
				if (!name) {
					return NULL;
				}

				/* must have direction specifier now */
				if (symb == S_INPUT) {
					nextsymb ();
					name = newmopnode (S_ASINPUT, NOPOSN, name, S_CHAN);
					SetOpTypeAttr (name, TypeAttr_shared);
				} else if (symb == S_OUTPUT) {
					nextsymb ();
					name = newmopnode (S_ASOUTPUT, NOPOSN, name, S_CHAN);
					SetOpTypeAttr (name, TypeAttr_shared);
				} else {
					synerr_e (SYN_E_CHANTYPEDIRSPEC, locn, symb);
					return NULL;
				}
				break;
			case S_ANYCHANTYPE:
				/* SHARED MOBILE.CHAN protocol */
				{
					treenode *typenode = newtypenode (S_MOBILE, locn, NULL, NULL);
					INT32 typeattr = TypeAttr_shared;

					name = newleafnode (S_ANYCHANTYPE, locn);
					SetLeafLink (name, typenode);

					nextsymb ();
					/* and might have a direction-specifier too */
					if (symb == S_INPUT) {
						nextsymb ();
						typeattr |= TypeAttr_marked_in;
					} else if (symb == S_OUTPUT) {
						nextsymb ();
						typeattr |= TypeAttr_marked_out;
					}

					SetTypeAttr (typenode, typeattr);
				}
				break;
			default:
				synerr_e (SYN_E_CHANTYPE, locn, symb);
				break;
			}
			return name;
		}
		/*}}}*/
#endif /* MOBILES */
		/*{{{  case S_NAME */
#ifdef OCCAM2_5
	case S_NAME:
		{
			treenode *name = (treenode *)rname ();

			if (!name) {
				return NULL;
			}
			rtypehash (&name);
#ifdef MOBILES
			/* might have a direction specifier here if the NAME is a channel-type */
			if (symb == S_INPUT) {
				nextsymb ();
				name = newmopnode (S_ASINPUT, NOPOSN, name, S_CHAN);
			} else if (symb == S_OUTPUT) {
				nextsymb ();
				name = newmopnode (S_ASOUTPUT, NOPOSN, name, S_CHAN);
			}
#endif
			return name;
		}
#endif
		/*}}} */
		/*{{{  case S_BARRIER */
	case S_BARRIER:
		s = newleafnode (S_FULLBARRIER, locn);
		nextsymb ();
		return s;
		/*}}}*/
	default:
#if 0
fprintf (stderr, "rsimpleprotocol: here!\n");
#endif
		synerr_e (SYN_E_PTYPE, locn, symb);
		return NULL;
	}
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *rtaggedprotocol ()*/
/* Terminates with symb at start of next line */
/* Parse:
 *	tagged.protocol { tagged.protocol }
 *   where
 *	tagged.protocol = tag {0, ';' simple.protocol }
 *			| tag = val {0, ';' simple.protocol }
 *	                | FROM name
 */
PRIVATEPARAM treenode *rtaggedprotocol (void)
{
	wordnode *name;
	treenode *taglist;
	int indent = symbindent;
	SOURCEPOSN locn;
	BOOL fromtag = FALSE;
	treenode *decl;
	int fixedval = -1;

	linebreakindent = (-1);
	DEBUG_MSG (("rtaggedprotocol... "));
	while (symb == S_COMMENT) {
		if (checknlindent (indent)) {
			goto error2;
		}
	}
	locn = flocn;
	/*{{{  read tagged list */
	if (symb == S_FROM) {
		fromtag = TRUE;
		nextsymb ();
	}
	if ((name = rname ()) == NULL) {
		goto error;
	}
	if ((symb == S_EQ) && !fromtag) {
		/* fixed value */
		wordnode *n;
		treenode *litval = NULL;

		nextsymb ();
		if (symb != S_UINTLIT) {
			synerr (SYN_NONCONST_TAGVAL, locn);
		}
		n = lookupword (literalv, literalp);
		litval = newlitnode (S_UINTLIT, locn, (treenode *)n, newleafnode (S_INT, locn));
		fixedval = eval_const_treenode (litval);
		nextsymb ();

#if 0
fprintf (stderr, "rtaggedprotocol: fixed tag value (fixedval = %d): litval = ", fixedval);
printtreenl (stderr, 4, litval);
#endif
	}
	if (symb == S_SEMICOLON) {
		nextsymb ();
		if (checklinebreak ()) {
			goto error2;
		}
		taglist = rlist (rsimpleprotocol, S_SEMICOLON);
		if (taglist == NULL) {
			goto error;
		}
	} else if (fromtag) {
		taglist = (treenode *)name;
	} else {
		taglist = NULL;
	}
	/*}}} */
	checknewline ();
	decl = declname (N_TAGDEF, locn, name, taglist, NULL);

	if (fixedval >= 0) {
		SetNTValue (decl, fixedval);
		SetNTypeAttr (decl, NTypeAttrOf (decl) | TypeAttr_fixed);
	} else {
		SetNTValue (decl, -1);
	}

	return decl;

      error:
	nextline ();
      error2:
	skiplines (indent);
	return NULL;
}

/*}}}*/
/*{{{  PRIVATE treenode *rprotdef ()*/
/* Terminates with symb at start of next line */
PRIVATE treenode *rprotdef (void)
{
	const int indent = symbindent;
	const SOURCEPOSN locn = flocn;
	wordnode *name;
	treenode *ename;

	DEBUG_MSG (("rprotdef... "));
	if (checkfor (S_PROTOCOL)) {
		goto error;
	}
	name = rname ();
	ename = NULL;
	if (symb == S_EXTENDS) {
		/* protocol extension (maybe a list) */

		nextsymb ();
		while ((symb != S_NEWLINE) && (symb != S_COMMENT)) {
			wordnode *tname = rname ();

			if (!tname) {
				synerr_e (SYN_E_NAME, locn, symb);
				goto error;
			}
			ename = newlistnode (S_LIST, locn, (treenode *)tname, ename);
			if (symb == S_COMMA) {
				nextsymb ();
			} else {
				break;
			}
		}
		if (symb == S_COLON) {
			/* tag-less extended protocol */
			treenode *retptr = declare (S_TPROTDEF, locn, NULL, name, NULL);

			nextsymb ();
			if (checknlindent (indent)) {
				return NULL;
			}
			SetDExtra (retptr, ename);
			return retptr;
		}
	}
	ignore (S_COMMENT);
	if (symb == S_NEWLINE) {
		/*{{{  we have a tagged protocol */
		treenode *tlist;	/* Pointer to list of tagged protocols */
		treenode *retptr;

		/* revised by analogy with RECORD */
		/* nextsymb ();
		   if (checkindent (indent + 2)) */
		if (checknlindent (indent + 2)) {
			goto error2;
		}
		while (symb == S_COMMENT) {
			if (checknlindent (indent + 2)) {
				goto error2;
			}
		}
		if (checkfor (S_CASE)) {
			goto error;
		}
		checknewline ();

		tlist = rproclist (rtaggedprotocol, indent + 4);

		if (checkindent (indent)) {
			goto error2;
		}
		/*{{{  check for : but continue if not found */
		if (checkfor (S_COLON)) {
			skiplines (indent);
			goto cont;
		}
		/*}}} */
		if (checknlindent (indent)) {
			goto error2;
		}

	cont:
		retptr = declare (S_TPROTDEF, locn, tlist, name, NULL);
#if 0
fprintf (stderr, "parsing protdecl: retptr = ");
printtreenl (stderr, 4, retptr);
fprintf (stderr, "parsing protdecl: ename = ");
printtreenl (stderr, 4, ename);
#endif
		if (TagOf (retptr) == S_TPROTDEF) {
			SetDExtra (retptr, ename);
		}
		return retptr;
		/*}}} */
	} else {
		/*{{{  we have a sequential protocol */
		treenode *p;	/* Pointer to sequential protocol */

		if (checkfor (S_IS) || checklinebreak () || ((p = rlist (rsimpleprotocol, S_SEMICOLON)) == NULL) || checkfor (S_COLON)) {
			goto error;
		}
		if (checknlindent (indent)) {
			goto error2;
		}
		if (ename) {
			synerr (SYN_EXTENDS_NOT_TAGGED, locn);
			goto error2;
		}

		return declare (S_SPROTDEF, locn, p, name, NULL);
		/*}}} */
	}

error:
	nextline ();
error2:
	skiplines (indent);
	if (symb == S_COLON) {
		nextline ();
	}
	return NULL;
}

/*}}}*/
/*{{{  PRIVATE treenode *rprotocol ()*/
/* On error, leaves protocol unchanged */
PRIVATE treenode *rprotocol (void)
{
	DEBUG_MSG (("rprotocol... "));
	switch (symb) {
#ifndef MOBILES
	case S_ANY:
		{
			const SOURCEPOSN locn = flocn;

			nextsymb ();
			return newleafnode (S_ANY, locn);
		}
#else	/* MOBILES */
	case S_ANY:
		{
			const SOURCEPOSN locn = flocn;

			nextsymb ();
			if (symb == S_CHAN) {
				/* probably have: ANY CHAN TYPE, scoop it up */
				nextsymb ();
				if (symb != S_TYPE) {
					synerr_e (SYN_E_TYPE, flocn, symb);
					return NULL;
				}
				nextsymb ();
				return newleafnode (S_ANYCHANTYPE, locn);
			} else if (symb == S_PROC) {
				/* probably have: ANY PROC TYPE, scoop it up */
				nextsymb ();
				if (symb != S_TYPE) {
					synerr_e (SYN_E_TYPE, flocn, symb);
					return NULL;
				}
				nextsymb ();
				return newleafnode (S_ANYPROCTYPE, locn);
			}
			return (newleafnode (S_ANY, locn));
		}
	case S_ANYCHANTYPE:
	case S_ANYPROCTYPE:
	case S_ANYMOBILETYPE:
		{
			const int s = symb;
			const SOURCEPOSN locn = flocn;
			treenode *node;

			nextsymb ();
			node = newleafnode (s, locn);

			return node;
		}
#endif

#ifndef OCCAM2_5
		/* this is no longer necessary when rsimpleprotocol is now permitted
		   to read a name */
	case S_NAME:
		return ((treenode *) rname ());
#endif
#ifdef MOBILES
	case S_MOBILE:
		if (!mobile_data_types) {
			synerr (SYN_NO_MOBILES, flocn);
		}
		nextsymb ();
		return newtypenode (S_MOBILE, flocn, NULL, rsimpleprotocol ());
#endif
	default:
		return (rsimpleprotocol ());
	}
}

/*}}}*/
/*}}}*/
/*{{{  PRIVATE treenode *rspecifier_constr ()*/
#ifdef OCCAM2_5
/* On error, leaves symb unchanged */
/* Actually allows PORTs, although they are strictly illegal */
PRIVATE treenode *rspecifier_constr (void)
{
	const SOURCEPOSN locn = flocn;
	DEBUG_MSG (("rspecifier... "));
	switch (symb) {
		/*{{{  BOOL BYTE INT INT16 INT32 INT64 REAL32 REAL64 */
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
		CASE_CONFIG_TYPE {
			treenode *const s = newleafnode (symb, locn);
			nextsymb ();
			return (s);
		}
		/*}}} */
		/*{{{  S_CHAN S_PORT */
	case S_CHAN:
	case S_PORT:
		{
			treenode *prot;
			const int ss = symb;

			nextsymb ();
			/* optional OF */
			if (symb == S_OF) {
				nextsymb ();
			}
			/* if (checkfor (S_OF))
				return NULL; */
			if ((prot = rprotocol ()) == NULL)
				return NULL;
			return (newtypenode (ss, locn, NULL, prot));
		}
		/*}}} */
		/*{{{  S_LBOX */
	case S_LBOX:
		{
			treenode *spec;
			treenode *e;
			nextsymb ();
			if ((e = rexp ()) == NULL || checkfor (S_RBOX) || (spec = rspecifier ()) == NULL)
				return NULL;
			return (newtypenode (S_ARRAY, locn, e, spec));
		}
		/*}}} */
		/*{{{  S_BOX */
	case S_BOX:
		{
			treenode *spec;
			nextsymb ();
			if ((spec = rspecifier ()) == NULL)
				return NULL;
			return (newtypenode (S_ARRAY, locn, NULL, spec));
		}
		/*}}} */
		/*{{{  S_NAME */
#ifdef OCCAM2_5
	case S_NAME:
		return (treenode *) rname ();
#endif
		/*}}} */
	default:
		return NULL;
	};
}
#endif
/*}}}*/
/*{{{  PRIVATE treenode *rtrace (const int indent)*/
/*
 *	parses a trace definition, e.g.:
 *	
 *	  in? -> out!
 *	  in -> out
 *
 *	  in? CASE
 *	    bar -> out! bar.reply
 *	    foo -> out! -> out! foo.ack
 *
 *	more generally:
 *
 *	  FIXME..
 */
PRIVATE treenode *rtrace (const int indent)
{
	treenode *trace = NULL;
	treenode **traceptr = &trace;
	int done = 0;
	int saved_locn = flocn;

	while (!done) {
		SOURCEPOSN locn = flocn;
		treenode *thisone = NULL;

		switch (symb) {
		case S_NAME:
			{
				wordnode *name = rname ();

				if (!name) {
					synerr (SYN_BAD_TRACE, flocn);
					goto error;
				}
				/* direction-specifier ? */
				switch (symb) {
				case S_INPUT:
				case S_OUTPUT:
					thisone = newmopnode ((symb == S_INPUT) ? S_ASINPUT : S_ASOUTPUT, locn, (treenode *)name, S_CHAN);
					nextsymb ();
					break;
				default:
					thisone = (treenode *)name;
					break;
				}
			}
			break;
		case S_NEWLINE:
			nextsymb ();
			done = 1;
			break;
		case S_COMMENT:
			ignorecomments (indent);
			done = 1;
			break;
		default:
			synerr_e (SYN_E_TRACEDEF, locn, symb);
			goto error;
			break;
		}

		if (thisone) {
			/* got something here */
			*traceptr = newlistnode (S_LIST, saved_locn, thisone, *traceptr);
			traceptr = NextItemAddr (*traceptr);

			/* sequential/parallel behaviour ? */
			if (symb == S_SEQCOMP) {
				nextsymb ();
			} else if (symb == S_PARCOMP) {
				nextsymb ();
			}
		} else if (!done) {
			synerr (SYN_BAD_TRACE, locn);
			goto error;
		}
	}
error:
	return trace;
}
/*}}}*/
/*{{{  PRIVATE treenode *rtypedecl*/
#ifdef OCCAM2_5
/* Terminates with symb at start of next line */
PRIVATE treenode *rtypedecl (const BOOL datatype, const int indent)
{
	const SOURCEPOSN locn = flocn;
	wordnode *name;
#ifdef MOBILES
	BOOL recursive = FALSE;
	treenode *traces = NULL;
	treenode **tracesptr = &traces;
#endif

	DEBUG_MSG (("rtypedecl... "));
#ifdef MOBILES
	if (!datatype && ((symb == S_RECURSIVE) || (symb == S_REC))) {
		/* recursive channel type declaration */
		recursive = TRUE;
		nextsymb ();
		if (checkfor (S_CHAN)) {
			goto error;
		}
	}
#endif
	if (datatype) {
		if (checkfor (S_DATA)) {
			goto error;
		}
	}
	if (checkfor (S_TYPE)) {
		goto error;
	}
	name = rname ();
	if (!name) {
		/* oops, something not quite right here! */
		goto error;
	}
#ifdef MOBILES
	if ((symb == S_COLON) && !datatype) {
		/* special forward declaration of channel-type */
		treenode *decl;
		
		nextsymb ();
		ignore (S_COMMENT);
		checknewline ();
		decl = newdeclnode (S_FORWDECL, locn, (treenode *)name, NULL, NULL);
		new_forwdecl (decl);
		return decl;
	}
#endif
	ignore (S_COMMENT);
	if (!datatype || (symb == S_NEWLINE)) {
		/*{{{  we have a record style CHAN TYPE */
		treenode *rbody = NULL;
		treenode **rbodyptr = &rbody;
		BOOL packed = FALSE;
#ifdef MOBILES
		BOOL mobile = FALSE;
#endif
		if (checknlindent (indent + 2)) {
			goto error2;
		}
		while (symb == S_COMMENT) {
			if (checknlindent (indent + 2)) {
				goto error2;
			}
		}
		/*{{{  optional PACKED/MOBILE keyword */
		while (datatype) {
			if (symb == S_PACKED) {
				packed = TRUE;
#ifdef MOBILES
			} else if (symb == S_MOBILE) {
				if (!mobile_data_types) {
					synerr (SYN_NO_MOBILES, flocn);
				}
				mobile = TRUE;
#endif
			} else {
				break;		/* from while() */
			}
			nextsymb ();
		}
		if (!datatype) {
#ifdef MOBILES
			/* CHAN TYPEs _must_ be MOBILE */
			if (checkfor (S_MOBILE)) {
				goto error;
			}
			mobile = TRUE;
#endif
		}
		/*}}} */
		/*{{{  SCALAR type ?*/
		/*}}}  */
		if (checkfor (S_RECORD)) {
			goto error;
		}
		checknewline ();

		while (ignorecomments (indent + 4), (symbindent == (indent + 4))) {
			SOURCEPOSN this_locn = flocn;

			/**rbodyptr = rspecification ();*/
			treenode *t = rspecifier ();
#if 0
fprintf (stderr, "rtypedecl: t = rspecnameandbody (rspecifier()) = ");
printtreenl (stderr, 4, t);
#endif
			if ((wordnode *)t == name) {
				/* this happens if a RECURSIVE name is used but isn't a channel (error) */
				synerr_s (SYN_EXPECTED_CHAN, this_locn, tagstring (TagOf (t)));
				goto error;
			}
			if (t != NULL) {
				t = rspecnameandbody (t, this_locn, indent, FALSE);
			}
			if (t != NULL) {
				*rbodyptr = t;
				rbodyptr = DBodyAddr (t);
			} else {
				nextsymb ();
				skiplines (indent + 4);
			}
		}

		ignorecomments (indent + 2);
		if (symbindent == (indent + 2)) {
			if (symb == S_TRACES) {
				nextsymb ();
				checknewline ();
				/* expecting a list of traces at indent + 4 */

				while (ignorecomments (indent + 4), (symbindent == (indent + 4))) {
					SOURCEPOSN this_locn = flocn;
					treenode *trace;

					/* parse this trace */
					trace = rtrace (indent + 4);
					if (!trace) {
						nextsymb ();
						skiplines (indent + 4);
					} else {
						*tracesptr = newlistnode (S_LIST, this_locn, trace, NULL);
						tracesptr = NextItemAddr (*tracesptr);
					}
				}
#if 0
fprintf (stderr, "syn2: here! traces =");
printtreenl (stderr, 4, traces);
#endif
			}
		}
		if (checkindent (indent)) {
			goto error2;
		}
		/*{{{  check for : but continue if not found */
		if (checkfor (S_COLON)) {
			skiplines (indent);
			goto cont;
		}
		/*}}} */
		if (checknlindent (indent)) {
			goto error2;
		}

cont:
		rbody = newtypenode (S_RECORD, locn, NULL, rbody);
		SetTypeAttr (rbody, TypeAttrOf (rbody)
			     | (datatype ? TypeAttr_datatype : TypeAttr_chantype)
			     | (packed ? TypeAttr_packed : 0));
#ifdef MOBILES
		if (mobile) {
			rbody = newtypenode (S_MOBILE, locn, NULL, rbody);
			if (recursive) {
				SetTypeAttr (rbody, TypeAttrOf (rbody) | TypeAttr_recursive);
			}
			if (traces) {
				SetTypeTraces (rbody, traces);
			}
		} else if (recursive) {
			/* recursive type without mobile-ness ? */
			synerr (SYN_REC_TYPE_NOT_MOBILE, locn);
			goto error;
		}
#endif
		if (mobile) {
			/* maybe sort out forward declarations */
			rbody = declare (S_TYPEDECL, locn, rbody, name, NULL);
			if (check_forwdecl ((treenode *)name)) {
				treenode *forwdecl = get_forwdecl ((treenode *)name);

#if 0
fprintf (stderr, "rtypedecl: have forward declarations for name!\n");
#endif
				SetDName (forwdecl, DNameOf (rbody));
				del_forwdecl ((treenode *)name);
			}
		} else {
			rbody = declare (S_TYPEDECL, locn, rbody, name, NULL);
		}
		return rbody;
		/*}}} */
	} else {
		/*{{{  we have a normal data type */
		treenode *p;	/* Pointer to sequential protocol */

		if (checkfor (S_IS) || checklinebreak ()) {
			goto error;
		}
		if ((p = rspecifier ()) == NULL) {
			nextline ();
			skiplines (indent);
			if (symb == S_COLON) {
				nextline ();
			}
			return declare (S_TYPEDECL, locn, newleafnode (S_INT, NOPOSN), name, NULL);
		}
		if (checkfor (S_COLON)) {
			nextline ();
			skiplines (indent);
			if (symb == S_COLON)
				nextline ();
			return declare (S_TYPEDECL, locn, p, name, NULL);
		}
		if (checknlindent (indent))
			goto error2;

		return declare (S_TYPEDECL, locn, p, name, NULL);
	}
	/*}}} */

error:
	nextline ();
error2:
	skiplines (indent);
	if (symb == S_COLON)
		nextline ();
	return NULL;
}
#endif

/*}}}*/
/*{{{  PUBLIC treenode *rinitialspec ()*/
#ifdef INITIAL_DECL
/*
 *	treenode *rinitialspec (void)
 *	parses INITIAL specification
 *	On error, leaves symb unchanged
 */
PUBLIC treenode *rinitialspec (void)
{
	const SOURCEPOSN locn = flocn;
	DEBUG_MSG (("rinitialspec... "));
	switch (symb) {
		/* allow udt's, base types, and fixed size arrays */
		/*{{{  BOOL BYTE INT INT16 INT32 INT64 REAL32 REAL64 */
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
		CASE_CONFIG_TYPE {
			treenode *const s = newleafnode (symb, locn);
			nextsymb ();
			return (s);
		}
		/*}}} */
		/*{{{  S_LBOX */
	case S_LBOX:
		{
			treenode *spec;
			treenode *e;
			nextsymb ();
			if ((e = rexp ()) == NULL || checkfor (S_RBOX) || (spec = rspecifier ()) == NULL)
				return NULL;
			return (newtypenode (S_ARRAY, locn, e, spec));
		}
		/*}}} */
		/*{{{  S_BOX */
	case S_BOX:
		{
			synerr (SYN_INITDECL_ADIM_MISSING, flocn);
			return NULL;
			/*{{{  COMMENT */
			/*treenode *spec; */
			/*nextsymb (); */
			/*if ((spec = rspecifier ()) == NULL) return NULL; */
			/*return (newtypenode(S_ARRAY, locn, NULL, spec)); */
			/*}}} */
		}
		/*}}} */
		/*{{{  S_NAME */
#ifdef OCCAM2_5
	case S_NAME:
		{
			treenode *name = (treenode *)rname ();

			if (symb == S_INPUT) {
				name = newmopnode (S_ASINPUT, flocn, name, S_CHAN);
				nextsymb ();
				return name;
			} else if (symb == S_OUTPUT) {
				name = newmopnode (S_ASOUTPUT, flocn, name, S_CHAN);
				nextsymb ();
				return name;
			} else {
				return name;
			}
		}
#endif
		/*}}} */
#ifdef MOBILES
		/*{{{  S_MOBILE */
	case S_MOBILE:
		{
			treenode *spec;

			nextsymb ();
			spec = rspecifier ();
			if (!spec) {
				return NULL;
			}
			return newtypenode (S_MOBILE, locn, NULL, spec);
		}
		/*}}}  */
#endif
	default:
		synerr_e (SYN_E_SPECIFIER, flocn, symb);
		return NULL;
	};
}
#endif
/*}}}*/
/*{{{  PUBLIC treenode *rresultspec (void) */
/*
 *	treenode *rresultspec (void)
 *	parses RESULT specification
 *	on error, leaves symb unchanged
 */
PUBLIC treenode *rresultspec (void)
{
	const SOURCEPOSN locn = flocn;
	DEBUG_MSG (("rresultspec... "));

	switch (symb) {
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
	CASE_CONFIG_TYPE
		{
			treenode *const s = newleafnode (symb, locn);

			nextsymb ();
			return s;
		}
	case S_LBOX:
		{
			treenode *spec;
			treenode *e;

			nextsymb ();
			e = rexp ();
			if (!e) {
				return NULL;
			}
			if (checkfor (S_RBOX)) {
				return NULL;
			}
			spec = rspecifier ();
			if (!spec) {
				return NULL;
			}
			return newtypenode (S_ARRAY, locn, e, spec);
		}
	case S_BOX:
		{
			treenode *spec;
			nextsymb ();
			if ((spec = rspecifier ()) == NULL) {
				return NULL;
			}
			return (newtypenode (S_ARRAY, locn, NULL, spec));
		}
#ifdef OCCAM2_5
	case S_NAME:
		return (treenode *)rname ();
#endif
	default:
		synerr_e (SYN_E_SPECIFIER, flocn, symb);
		return NULL;
	}
}
/*}}}  */
/*{{{  PRIVATE void rtypehash (treenode **tptr)*/
/*
 *	parses a typehash attached to a name-node
 *	(usually) for external declaration processing.
 *	Returns non-zero if a declaration was found and parsed.
 */
PRIVATE int rtypehash (treenode **tptr)
{
	SOURCEPOSN locn = flocn;

	if ((lexmode != LEX_DEXTERNAL) || (symb != S_AMPERSAT)) {
		return 0;
	}

	nextsymb ();
	if (symb != S_UINTLIT) {
		synerr_e (SYN_E_TYPEHASH, flocn, symb);
	} else {
		treenode *val = roperand ();

#if 0
fprintf (stderr, "rtypehash(): got specifier, value =");
printtreenl (stderr, 4, val);
#endif
		*tptr = newdopnode (S_TYPEHASHCHECK, locn, *tptr, val, 0);
	}

	return 1;
}
/*}}}*/
/*{{{  PUBLIC treenode *rspecifier ()*/
/* On error, leaves symb unchanged */
/* Actually allows PORTs, although they are strictly illegal */
PUBLIC treenode *rspecifier (void)
{
	const SOURCEPOSN locn = flocn;
	DEBUG_MSG (("rspecifier... "));
	switch (symb) {
		/*{{{  BOOL BYTE INT INT16 INT32 INT64 REAL32 REAL64 */
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
		CASE_CONFIG_TYPE {
			treenode *const s = newleafnode (symb, locn);
			nextsymb ();
			return (s);
		}
		/*}}} */
		/*{{{  BARRIER*/
	case S_BARRIER:
		{
			treenode *const s = newleafnode (S_FULLBARRIER, locn);
			nextsymb ();
			return s;
		}
		/*}}}*/
		/*{{{  S_CHAN (!MOBILES) S_PORT */
#ifndef MOBILES
	case S_CHAN:
#endif
	case S_PORT:
		{
			treenode *prot, *typenode;
			const int ss = symb;

			nextsymb ();
			/* optional OF */
			if (symb == S_OF) {
				nextsymb ();
			}
			/* if (checkfor (S_OF))
				return NULL; */
			if ((prot = rprotocol ()) == NULL) {
				return NULL;
			}
			typenode = newtypenode (ss, locn, NULL, prot);

			return typenode;
		}
		/*}}} */
		/*{{{  S_BUFFERED*/
	case S_BUFFERED:
		{
			treenode *size, *cspec, *typenode;

			nextsymb ();
			if (symb != S_LPAREN) {
				synerr_e (SYN_E_LPAREN, flocn, symb);
			}
			nextsymb ();
			size = roperand ();
			if (!size) {
				synerr_e (SYN_E_EXPR, flocn, symb);
			}
			if (symb != S_RPAREN) {
				synerr_e (SYN_E_RPAREN, flocn, symb);
			}
			nextsymb ();
			cspec = rspecifier ();
			if (!cspec) {
				synerr_e (SYN_E_SPECIFIER, flocn, symb);
			}

			typenode = newtypenode (S_BUFFERED, locn, size, cspec);

			return typenode;
		}
		/*}}}*/
#ifdef MOBILES			/* records of channels */
		/*{{{  S_CHAN (MOBILES)*/
	case S_CHAN:
		nextsymb ();
		{
			treenode *t;
			treenode *prot;
			BOOL is_shared = FALSE;

			/* optional OF */
			if (symb == S_OF) {
				nextsymb ();
			}
			/* chan of shared somethings ? */
			if (symb == S_SHARED) {
				is_shared = TRUE;
				nextsymb ();
			}
			if ((prot = rprotocol ()) == NULL) {
				return NULL;
			}
#if 0
fprintf (stderr, "rspecifier: (CHAN) got prot = ");
printtreenl (stderr, 4, prot);
#endif
			if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
				/* must be a channel of a chan-type, this is the client/server specifier */
				prot = newmopnode ((symb == S_INPUT) ? S_ASINPUT : S_ASOUTPUT, locn, prot, S_CHAN);
				nextsymb ();
			}
			t = newtypenode (S_CHAN, locn, NULL, prot);
			/* if shared, bung this in the TypeAttr of the channel (will get pushed into the protocol later) */
			if (is_shared) {
#if 0
fprintf (stderr, "rspecifier: putting SHARED in TypeAttr of CHAN\n");
#endif
				SetTypeAttr (t, TypeAttrOf (t) | TypeAttr_shared);
			}
			return t;
		}
		/*}}}*/
#endif
		/*{{{  S_LBOX */
	case S_LBOX:
		{
			treenode *spec;
			treenode *e;
			nextsymb ();
			if ((e = rexp ()) == NULL || checkfor (S_RBOX) || (spec = rspecifier ()) == NULL)
				return NULL;
			return (newtypenode (S_ARRAY, locn, e, spec));
		}
		/*}}} */
		/*{{{  S_BOX */
	case S_BOX:
		{
			treenode *spec;
			nextsymb ();
			if ((spec = rspecifier ()) == NULL)
				return NULL;
			return (newtypenode (S_ARRAY, locn, NULL, spec));
		}
		/*}}} */
#ifdef MOBILES
		/*{{{  S_MOBILE*/
	case S_MOBILE:
		{
			treenode *spec;

			if (!mobile_data_types) {
				synerr (SYN_NO_MOBILES, locn);
			}
			nextsymb ();
			spec = rspecifier ();
			if (!spec) {
				return NULL;
			}
			return (newtypenode (S_MOBILE, locn, NULL, spec));
		}
		/*}}}  */
		/*{{{  S_SHARED*/
	case S_SHARED:
		/* parsing "SHARED typename[!?] name"
		 *    or possibly
		 * "SHARED MOBILE.CHAN name"
		 */
		{
			treenode *name;

			nextsymb ();
			switch (symb) {
			case S_NAME:
				if ((name = (treenode *)rname ()) == NULL) {
					synerr_e (SYN_E_SPECIFIER, flocn, symb);
					return NULL;
				}
				rtypehash (&name);
				/* ought to have a channel-direction specifier (for the type) here */
				if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
					name = newmopnode ((symb == S_INPUT) ? S_ASINPUT : S_ASOUTPUT, flocn, name, S_CHAN);
					SetOpTypeAttr (name, OpTypeAttrOf (name) | TypeAttr_shared);		/* mark as shared */
					nextsymb ();
				} else {
					synerr_e (SYN_E_CHANTYPEDIRSPEC, locn, symb);
					return NULL;
				}
				break;
			case S_ANYCHANTYPE:
				{
					treenode *type = newtypenode (S_MOBILE, locn, NULL, NULL);
					INT32 typeattr = TypeAttr_shared;

					name = newleafnode (S_ANYCHANTYPE, locn);
					SetLeafLink (name, type);
					nextsymb ();

					if (symb == S_INPUT) {
						nextsymb ();
						typeattr |= TypeAttr_marked_in;
					} else if (symb == S_OUTPUT) {
						nextsymb ();
						typeattr |= TypeAttr_marked_out;
					}
					SetTypeAttr (type, typeattr);
				}
				break;
			default:
				synerr_e (SYN_E_CHANTYPE, locn, symb);
				return NULL;
			}

			return name;
		}
		break;
		/*}}}*/
		/*{{{  S_ANYCHANTYPE*/
	case S_ANYCHANTYPE:
		/* parsing "MOBILE.CHAN[?!] name" */
		{
			treenode *name, *type;

			type = newtypenode (S_MOBILE, locn, NULL, NULL);
			name = newleafnode (S_ANYCHANTYPE, locn);
			SetLeafLink (name, type);
			nextsymb ();
			if (symb == S_INPUT) {
				nextsymb ();
				SetTypeAttr (type, TypeAttr_marked_in);
			} else if (symb == S_OUTPUT) {
				nextsymb ();
				SetTypeAttr (type, TypeAttr_marked_out);
			}

			return name;
		}
		/*}}}*/
		/*{{{  S_ANYPROCTYPE S_ANYMOBILETYPE*/
	case S_ANYPROCTYPE:
	case S_ANYMOBILETYPE:
		/* parsing "MOBILE.PROC name" or "MOBILE.ANY" name */
		{
			const int s = symb;
			treenode *name;

			nextsymb ();
			name = newleafnode (s, locn);
			return name;
		}
		/*}}}*/
#endif	/* MOBILES */
		/*{{{  S_NAME */
#ifdef OCCAM2_5
	case S_NAME:
		{
			treenode *name;

			name = (treenode *)rname ();

			if ((lexmode == LEX_DEXTERNAL) && (symb == S_AMPERSAT)) {
				/* means there is a typehash attached to the name */
				rtypehash (&name);
			}
#ifdef MOBILES
			if (symb == S_INPUT) {
				name = newmopnode (S_ASINPUT, locn, name, S_NAME);
				nextsymb ();
			} else if (symb == S_OUTPUT) {
				name = newmopnode (S_ASOUTPUT, locn, name, S_NAME);
				nextsymb ();
			}
#endif
			return name;
		}
#endif
		/*}}} */
	default:
#if 0
fprintf (stderr, "rspecifier: SYN_E_SPEC, got %s\n", tagstring(symb));
#endif
		synerr_e (SYN_E_SPECIFIER, flocn, symb);
		return NULL;
	};
}

/*}}}*/

/* procedure and function definitions */
/* Only rprocdef and rfunctiondef actually declare items */
/*{{{  PRIVATE treenode *rparam(t, valparam)*/
/*****************************************************************************
 *
 *  rparam reads a parameter specification, ie. it parses
 *         [ [ 'VAL' ] specifier ] name
 *         t is the specifier of the previous parameter in the list (if this
 *         specifier is omitted) valparam is the tag of the previous parameter
 *         in the list.
 *
 *****************************************************************************/

/* On error, leaves symb unchanged */
PRIVATE treenode *rparam (treenode *t, int valparam)
{
	SOURCEPOSN locn = flocn;
	wordnode *name = NULL;
	treenode *nptr;
#ifdef MOBILES
	int chantype_flag = 0;
#endif
	int undefined_flag = 0;
	int fixed_flag = 0;
	wordnode *undefined_word = lookupword ("UNDEFINED", 9);

#if 0
fprintf (stderr, "rparam: symb = %d\n", symb);
if (symb == S_NAME) {
	fprintf (stderr, "   --> name, lexword = %p, undefined_word = %p, WNameOf(lexword): %s\n", (void *)lexword, (void *)undefined_word, WNameOf (lexword));
}
#endif
	/*{{{  if we have the UNDEFINED keyword, swallow it up */
	if ((symb == S_UNDEFINED) || ((symb == S_NAME) && (lexword == undefined_word))) {
		undefined_flag = 1;
		nextsymb ();
	}
	/*}}}*/
	/*{{{  swallow up FIXED keyword too (won't occur with UNDEFINED!) */
	if (symb == S_FIXED) {
		fixed_flag = 1;
		nextsymb ();
	}
	/*}}}*/
	/*{{{  check for a new specifier */
	switch (symb) {
		/*{{{  case S_VAL */
	case S_VAL:		/* Read in new VAL specifier */
		valparam = N_VALPARAM;
		nextsymb ();
		/*{{{  parse specifier */
		if ((t = rspecifier ()) == NULL)
			return NULL;
		/*}}} */
		break;
		/*}}} */
#ifdef MOBILES
		/*{{{  case S_SHARED*/
	case S_SHARED:
		/*
		 * parsing "SHARED typename[!?] name"
		 * or:     "SHARED CHAN protocol name[!?]"
		 */
		valparam = N_PARAM;
		nextsymb ();
		/* might be an anonymous channel-type */
		if (symb == S_CHAN) {
			/*{{{  anonymous channel-type*/
			treenode *protocol;

			nextsymb ();
			if (symb == S_OF) {
				/* optional OF */
				nextsymb ();
			}
			if ((protocol = rprotocol()) == NULL) {
				return NULL;
			}
			t = newtypenode (S_ANONCHANTYPE, locn, NULL, protocol);
			/*}}}*/
		} else if (symb == S_ANYCHANTYPE) {
			/*{{{  any channel-type*/
			treenode *type = newtypenode (S_MOBILE, NOPOSN, NULL, NULL);
			INT32 typeattr = TypeAttr_shared;

			t = newleafnode (S_ANYCHANTYPE, locn);
			SetLeafLink (t, type);

			nextsymb ();
			if (symb == S_INPUT) {
				nextsymb ();
				typeattr |= TypeAttr_marked_in;
			} else if (symb == S_OUTPUT) {
				nextsymb ();
				typeattr |= TypeAttr_marked_out;
			}
			SetTypeAttr (type, typeattr);
			/*}}}*/
		} else {
			/*{{{  regular channel-type*/
			if ((t = (treenode *)rname ()) == NULL) {
				synerr_e (SYN_E_SPECIFIER, flocn, symb);
				return NULL;
			}
			rtypehash (&t);
			/* ought to have a channel-direction specifier (for the type) here */
			if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
				t = newmopnode ((symb == S_INPUT) ? S_ASINPUT : S_ASOUTPUT, flocn, t, S_CHAN);
				SetOpTypeAttr (t, OpTypeAttrOf (t) | TypeAttr_shared);		/* mark as shared */
				name = NULL;
				nextsymb ();
				chantype_flag = 1;
			} else {
				synerr_e (SYN_E_CHANTYPEDIRSPEC, flocn, symb);
				return NULL;
			}
			/*}}}*/
		}
		break;
		/*}}}*/
#endif
		/*{{{  case S_RESULT */
	case S_RESULT:
		valparam = N_RESULTPARAM;
		nextsymb ();
		t = rspecifier ();
		if (!t) {
			return NULL;
		}
		break;
		/*}}}  */
		/*{{{  case S_CHAN*/
	case S_CHAN:
		{
			BOOL marked_shared = FALSE;

			valparam = N_PARAM;
			/*{{{  parse specifier*/
			nextsymb ();
			if (symb == S_OF) {
				/* optional OF */
				nextsymb ();
			}
#ifdef MOBILES
			if (symb == S_SHARED) {
				/* it's a chan of shared something, _must_ be a name (but we'll do by rprotocol() anyhow) */
				treenode *protocol;

				marked_shared = TRUE;
				nextsymb ();
				if ((protocol = rprotocol()) == NULL) {
					return NULL;
				}
				t = newtypenode (S_CHAN, locn, NULL, protocol);
			} else
#endif
			{
				/* parse protocol as per usual */
				treenode *protocol;

				if ((protocol = rprotocol()) == NULL) {
					return NULL;
				}
				t = newtypenode (S_CHAN, locn, NULL, protocol);
			}
			/*}}}*/
#ifdef MOBILES
#if 0
fprintf (stderr, "syn2: rparam: CHAN, type (CHAN protocol): ");
printtreenl (stderr, 4, t);
#endif
#if 0
			/* frmb: rprotocol() now scoops this up */
			/* might have "CHAN FOO? name?" type parameter */
			if (marked_shared && (symb != S_INPUT) && (symb != S_OUTPUT)) {
				synerr_e (SYN_E_CHANTYPEDIRSPEC, flocn, symb);
				return NULL;
			}
			if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
				SetProtocol (t, newmopnode ((symb == S_INPUT) ? S_ASINPUT : S_ASOUTPUT, locn, ProtocolOf (t), S_CHAN));
				nextsymb ();
			}
#endif
			if (TagOf (ProtocolOf (t)) == S_ANYCHANTYPE) {
				treenode *mnode;

				if (!LeafLinkOf (ProtocolOf (t))) {
					SetLeafLink (ProtocolOf (t), newtypenode (S_MOBILE, locn, NULL, NULL));
				}
				mnode = LeafLinkOf (ProtocolOf (t));

				/* check for INPUT/OUTPUT for these ones here */
				if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
					SetTypeAttr (mnode, TypeAttrOf (mnode) | ((symb == S_INPUT) ? TypeAttr_marked_in : TypeAttr_marked_out));
					nextsymb ();
				}
				if (marked_shared) {
					SetTypeAttr (mnode, TypeAttrOf (mnode) | TypeAttr_shared);
				}
			} else if (marked_shared) {
				/* marked INPUT/OUTPUT node as shared */
				SetOpTypeAttr (ProtocolOf (t), TypeAttr_shared);
			}
#endif
		}
		break;
		/*}}}*/
		/*{{{  case S_LBOX BOOL BYTE INT INT16 INT32 INT64 REAL32 REAL64 PORT TIMER RESOURCE MOBILE*/
	case S_LBOX:
	case S_BOX:
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
	case S_PORT:
	case S_TIMER:		/* Read in new var specifier */
	case S_BARRIER:
#ifdef MOBILES
	case S_MOBILE:
#endif
	CASE_CONFIG_TYPE
		valparam = N_PARAM;
		/*{{{  parse specifier */
		if ((t = rspecifier ()) == NULL) {
			return NULL;
		}
		/*}}} */
		break;
		/*}}} */
#ifdef MOBILES
		/*{{{  any mobile channel*/
	case S_ANYCHANTYPE:
		{
			treenode *type = newtypenode (S_MOBILE, NOPOSN, NULL, NULL);
			INT32 typeattr = 0;

			valparam = N_PARAM;
			nextsymb ();
			t = newleafnode (S_ANYCHANTYPE, locn);
			SetLeafLink (t, type);

			if (symb == S_INPUT) {
				nextsymb ();
				typeattr |= TypeAttr_marked_in;
			} else if (symb == S_OUTPUT) {
				nextsymb ();
				typeattr |= TypeAttr_marked_out;
			}

			SetTypeAttr (type, typeattr);
		}
		break;
		/*}}}*/
		/*{{{  any mobile process / any mobile */
	case S_ANYPROCTYPE:
	case S_ANYMOBILETYPE:
		{
			const int s = symb;
			valparam = N_PARAM;
			nextsymb ();
			t = newleafnode (s, locn);
		}
		break;
		/*}}}*/
#endif
		/*{{{  default (probably a name)*/
	default:
		{
			BOOL copy_type_tree = TRUE;
			/*{{{  look for type.name name */
#ifdef OCCAM2_5
			/* look for type.name name */
			if ((name = rname ()) == NULL) {
				return NULL;
			}
			rtypehash ((treenode **)&name);
			if (symb == S_NAME) {
				t = (treenode *) name;
				name = NULL;	/* force the next read of name */
				copy_type_tree = FALSE;
				valparam = N_PARAM;
			}
#endif
			/*}}} */
			if (t == NULL) {
				/*{{{  We have had no specifier*/
#ifdef MOBILES
				if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
					/* probably a channel-type direction specifier.  should have a name next */
					const int csymb = symb;

					nextsymb ();
					if (symb == S_NAME) {
						t = newmopnode ((csymb == S_INPUT) ? S_ASINPUT : S_ASOUTPUT, locn, (treenode *)name, S_CHAN);
						name = NULL;	/* force the next read of name */
						copy_type_tree = FALSE;
						valparam = N_PARAM;
						chantype_flag = 1;
					}
				} else
#endif
				{
					synerr_e (SYN_E_SPECIFIER, locn, symb);
					return NULL;
				}
				/*}}}*/
#ifdef MOBILES
			} else if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
				/*{{{  another channel-type direction specifier?  should have a name next*/
				const int csymb = symb;

				nextsymb ();
				if (symb == S_NAME) {
					t = newmopnode ((csymb == S_INPUT) ? S_ASINPUT : S_ASOUTPUT, locn, (treenode *)name, S_CHAN);
					name = NULL;	/* force re-read of name */
					copy_type_tree = FALSE;
					valparam = N_PARAM;
					chantype_flag = 1;
				} else if (((symb == S_COMMA) || (symb == S_RPAREN)) && copy_type_tree) {
					treenode *ttype;

					/* this wasn't a chan-type direction specifier */
					t = copytree (t, syn_lexlevel);
					/* make sure we insert CHAN attributes */
					ttype = t;
					while (TagOf (ttype) == S_ARRAY) {
						ttype = ARTypeOf (ttype);
					}
					if ((TagOf (ttype) == S_CHAN) || (TagOf (ttype) == S_ANONCHANTYPE) || (TagOf (ttype) == S_PORT)) {
						SetTypeAttr (ttype, (TypeAttrOf (ttype) & ~(TypeAttr_marked_in | TypeAttr_marked_out)) |
								((csymb == S_INPUT) ? TypeAttr_marked_in : TypeAttr_marked_out));
					}
				}
				/*}}}*/
#endif
			} else if (copy_type_tree) {
				/*{{{  make a new copy of the type tree*/
				treenode *ttype;

				/* make a new copy of t */
				t = copytree (t, syn_lexlevel);
				/* make sure we remove any CHAN attributes, if they're there */
				ttype = t;
				while (TagOf (ttype) == S_ARRAY) {
					ttype = ARTypeOf (ttype);
				}
				if ((TagOf (ttype) == S_CHAN) || (TagOf (ttype) == S_PORT)) {
					SetTypeAttr (ttype, TypeAttrOf (ttype) & ~(TypeAttr_marked_in | TypeAttr_marked_out));
				}
				/*}}}*/
			}
		}
		break;
		/*}}} */
	}
	/*}}} */
	if (name == NULL) {	/* if not null, we have already read the name! */
		if ((name = rname ()) == NULL) {
			return NULL;
		}
	}
	/*{{{  possible channel-direction specifier ? */
	if (t) {			/* fix: only if t is good, however */
		treenode *itype = t;

		while (TagOf (itype) == S_ARRAY) {
			itype = ARTypeOf (itype);
		}
		if (((TagOf (itype) == S_CHAN) || (TagOf (itype) == S_ANONCHANTYPE)) && ((symb == S_INPUT) || (symb == S_OUTPUT))) {
			SetTypeAttr (itype, TypeAttrOf (itype) & ~(TypeAttr_marked_in | TypeAttr_marked_out));
			/* have a channel direction specifier */
			if (symb == S_INPUT) {
				SetTypeAttr (itype, TypeAttrOf (itype) | TypeAttr_marked_in);
			} else {
				SetTypeAttr (itype, TypeAttrOf (itype) | TypeAttr_marked_out);
			}
#if 0
fprintf (stderr, "*** channel direction specifier found.  type (%p) is now ", itype);
printtreenl (stderr, 4, itype);
#endif
			nextsymb ();
		}
	}
#if 0
	if (chantype_flag && ((symb == S_INPUT) || (symb == S_OUTPUT))) {
		/* make sure direction specifiers match */
		if (((TagOf (t) == S_ASINPUT) && (symb == S_OUTPUT)) || ((TagOf (t) == S_ASOUTPUT) && (symb == S_INPUT))) {
			synerr_s (SYN_BAD_CHAN_CONFLICT, locn, WNameOf((wordnode *)name));
			return NULL;
		}
		nextsymb ();
	}
#endif
	/*}}}  */
	nptr = declname (valparam, locn, name, t, NULL);
	if (undefined_flag) {
		SetNTypeAttr (nptr, NTypeAttrOf (nptr) | TypeAttr_undefined);
	}
	if (fixed_flag) {
		SetNTypeAttr (nptr, NTypeAttrOf (nptr) | TypeAttr_ufixed);
	}
	return (newlistnode (S_LIST, locn, nptr, NULL));
}

/*}}}*/
/*{{{  PRIVATE treenode *rparmlist (void)*/
/*****************************************************************************
 *
 *  rparmlist reads in a formal parameter list,
 *            ie. it parses
 *              [ 'VAL' ] specifier name { ',' [ [ 'VAL' ] specifier ] name }
 *
 *****************************************************************************/
/* On error, leaves symb unchanged */
PRIVATE treenode *rparmlist (void)
{
	treenode *tptr, *a;
	treenode *t = NULL;
	int valparam = N_PARAM;

	if ((tptr = rparam (t, valparam)) == NULL) {
		return NULL;
	}
	a = tptr;
	while (symb == S_COMMA) {
		/*{{{  read another parameter */
		treenode *b;

		valparam = TagOf (ThisItem (a));
		t = NTypeOf (ThisItem (a));
		nextsymb ();
		if (checklinebreak ()) {
			return NULL;
		}
		if ((b = rparam (t, valparam)) == NULL) {
			return NULL;
		}
		NewNextItem (b, a);
		a = b;
		/*}}} */
	}
	return (tptr);
}

/*}}}*/
/*{{{  PRIVATE treenode *rdescriptorline (void)*/
PRIVATE treenode *rdescriptorline (void)
{
	treenode *n = NULL;
	wordnode *name = rname ();

	if (name != NULL) {
		if (symb == S_INPUT || symb == S_OUTPUT) {
			n = newactionnode (symb, flocn, (treenode *) name, NULL);
			nextsymb ();
			checknewline ();
		} else {
			synerr (SYN_BAD_DESCRIPTOR, flocn);
			nextline ();
		}
	} else {
		nextline ();
	}
	return n;
}

/*}}}*/
/*{{{  PRIVATE treenode *rdescriptorbody (int indent)*/
PRIVATE treenode *rdescriptorbody (int indent)
{
	treenode *pbody;
	int tag;
	SOURCEPOSN locn;

#if 0
fprintf (stderr, "syn2: rdescriptorbody ... indent = %d, symbindent = %d\n", indent, symbindent);
#endif
	if (checknlindent (indent + 2)) {
		goto error2;
	}

#if 0
fprintf (stderr, "syn2: rdescriptorbody ... indent = %d, symbindent = %d\n", indent, symbindent);
#endif
	locn = flocn;
	if (symb == S_PRI) {
		/*{{{  PRI PAR */
		tag = S_PRIPAR;
		nextsymb ();
		if (checkfor (S_PAR)) {
			goto error2;
		}
		/*}}} */
	} else {
		/*{{{  SEQ */
		if (checkfor (S_SEQ)) {
			goto error2;
		}
		tag = S_SEQ;
		/*}}} */
	}

	checknewline ();
	pbody = rproclist (rdescriptorline, indent + 4);

	checkindent (indent);
	if (checkfor (S_COLON)) {
		return (NULL);
	}
	pbody = newcnode (tag, locn, pbody);
	return (pbody);

error2:
	skiplines (indent);
	return NULL;
}

/*}}}*/
/*{{{  PRIVATE treenode *rproctypedef (const BOOL recursive, const int indent)*/
/*
 *	parses a PROC TYPE declaration.  Might also be RECURSIVE PROC TYPE
 */
PRIVATE treenode *rproctypedef (const BOOL recursive, const int indent)
{
	SOURCEPOSN locn = flocn;
	wordnode *name;
	treenode *params;
	treenode *decl;

	/* symb should be on the TYPE */
	if (symb == S_TYPE) {
		nextsymb ();
	}
	name = rname ();
	if (!name) {
		synerr_e (SYN_E_NAME, locn, symb);
		nextline ();
		return NULL;
	}
	if (symb == S_IS) {
		/* allow optional IS here */
		nextsymb ();
	}
	/* allow line-break */
	if (symb == S_NEWLINE) {
		nextsymb ();
		ignorecomments (indent);
	}
	if (checkfor (S_LPAREN)) {
		nextline ();
		return NULL;
	}
	if (symb != S_RPAREN) {
		params = rparmlist ();
	} else {
		params = NULL;
	}
	if (checkfor (S_RPAREN) || checkfor (S_COLON)) {
		nextline ();
		return NULL;
	}
	ignore (S_COMMENT);
	checknewline ();
#if 0
fprintf (stderr, "rproctypedef: name = %s, params = ", WNameOf (name));
printtreenl (stderr, 4, params);
#endif
	if (!mobile_proc_types) {
		synerr (SYN_NO_MPROCS, locn);
	}
	decl = declare (S_PROCTYPEDECL, locn, params, name, NULL);

	if (decl && recursive) {
		SetNTypeAttr (DNameOf (decl), NTypeAttrOf (DNameOf (decl)) | TypeAttr_recursive);
	}
#if 0
fprintf (stderr, "rproctypedef: decl =");
printtreenl (stderr, 4, decl);
#endif
	return decl;
}
/*}}}*/
/*{{{  PRIVATE treenode *rprocdef (void)*/

/* Terminates with symb at start of next line */
PRIVATE treenode *rprocdef (void)
{
	const int indent = symbindent;
	SOURCEPOSN locn = flocn;
	SOURCEPOSN endlocn;
	wordnode *name = NULL;	/* initialised to shut up gcc's optimiser */
	treenode *params = NULL, *pbody;
	treenode *retptr;
	BOOL inline_decl = FALSE;
	int parsedheader = FALSE;
	const int oldlexlevel = syn_lexlevel;
	BOOL recursive = FALSE;
	BOOL forks = FALSE;
	BOOL suspends = FALSE;
	BOOL dyncall = FALSE;

	DEBUG_MSG (("rprocdef... "));
	foundroutine = TRUE;
	syn_lexlevel++;

	/*{{{  parse and declare procedure */
	/*{{{  read proc header */
	/*{{{  inline xor recursive ? */
	if (symb == S_INLINE) {
		inline_decl = current_fe_data->fe_allow_inlines;
		nextsymb ();
	} else if ((symb == S_RECURSIVE) || (symb == S_REC)) {
		recursive = TRUE;
		nextsymb ();
	}
	/*}}} */
	if (checkfor (S_PROC)) {
		goto error;
	}
#ifdef MOBILES
	if (symb == S_TYPE) {
		/* have a PROC TYPE definition */
		syn_lexlevel = oldlexlevel;
		return rproctypedef (recursive, indent);
	}
#endif
	name = rname ();
	if ((name == NULL) || checkfor (S_LPAREN) || checklinebreak ()) {
		goto error;
	}
	if (symb != S_RPAREN) {
		if ((params = rparmlist ()) == NULL) {
			goto error;
		}
	}
	if (checkfor (S_RPAREN)) {
		goto error;
	}
	/*}}} */
	/*{{{  check for FORK/SUSPEND*/
	if (symb == S_FORK) {
		nextsymb ();
		forks = TRUE;
	}
	if (symb == S_SUSPEND) {
		nextsymb ();
		suspends = TRUE;
	}
	/*}}}*/
	parsedheader = TRUE;

	/*{{{  read body */
	switch (lexmode) {
	case LEX_SOURCE:
	case LEX_CSOURCE:
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
		endlocn = flocn;
		break;
	default:
		pbody = rdescriptorbody (indent);
		endlocn = NOPOSN;
		break;
	}
	if (pbody == NULL) {
		goto error2;
	}
	/*}}} */
	/*{{{  if externally read parameters, move TypeAttr_ufixed to TypeAttr_fixed*/
	switch (lexmode) {
	case LEX_SOURCE:
	case LEX_CSOURCE:
		break;
	default:
		{
			treenode *plist;

			for (plist = params; !EndOfList (plist); plist = NextItem (plist)) {
				treenode *parm = ThisItem (plist);

				switch (TagOf (parm)) {
				case N_PARAM:
				case N_RESULTPARAM:
				case N_VALPARAM:
					/* should not see TypeAttr_fixed here -- added by the usage-checker, but incase .. */
					if ((NTypeAttrOf (parm) & TypeAttr_ufixed) && !(NTypeAttrOf (parm) & TypeAttr_fixed)) {
						SetNTypeAttr (parm, NTypeAttrOf (parm) ^ (TypeAttr_ufixed | TypeAttr_fixed));
					}
					break;
				default:
					break;
				}
			}
		}
		break;
	}
	/*}}}*/
	/*{{{  if DEXTERNAL, make sure we always call instances dynamically*/
	if (lexmode == LEX_DEXTERNAL) {
		dyncall = 1;
	}
	/*}}}*/

	syn_lexlevel = oldlexlevel;
	retptr = declare (S_PROCDEF, locn, params, name, pbody);
	{
		treenode *const nptr = DNameOf (retptr);
#if 0
fprintf (stderr, "rprocdef(): lexmode = %d, nptr =", (int)lexmode);
printtreenl (stderr, 4, nptr);
#endif

		if (endlocn != NOPOSN) {
			DEBUG_MSG (("locn = %8lX, endlocn = %8lX\n", locn, endlocn));
			SetNPEndposn (nptr, endlocn);
		}
		if (separatelycompiled (nptr)) {
			addtoentrypointlist (nptr);
		}
		if (inline_decl) {
			SetTag (nptr, N_INLINEPROCDEF);
		}
		if (recursive) {
			SetNPRecursive (nptr, 1);
		}
		if (forks) {
#if 0
fprintf (stderr, "syn2: rprocdef: FORKs!\n");
#endif
			SetNPForks (nptr, 1);
		}
		if (dyncall) {
			SetNPDyncall (nptr, 1);
		}
		if (suspends) {
			SetNPSuspends (nptr, 1);
		}
	}
	if (checknlindent (indent)) {
		skiplines (indent);
		return (NULL);
	}
	return retptr;
	/*}}} */

error:
	nextline ();
error2:
	msg_out_s (SEV_INFO, SYN, SYN_SKIPPING_DEFN, NOPOSN, "PROC");
	skiplines (indent);
	if (symb == S_COLON) {
		nextline ();
	}
	syn_lexlevel = oldlexlevel;
	if (parsedheader && params != NULL) {
		retptr = declare (S_PROCDEF, locn, params, name, newleafnode (S_SKIP, locn));
		if (separatelycompiled (DNameOf (retptr))) {
			addtoentrypointlist (DNameOf (retptr));
		}
	}
	return NULL;
}

/*}}}*/
#ifdef MOBILES
/*{{{  PRIVATE treenode *rmobileprocdef (const int baseindent)*/
/*
 * this parses a MOBILE PROC definition:
 *	MOBILE PROC name (..params..) IMPLEMENTS ptype
 *	  ... body of process
 *	:
 *
 * all are optional, chk warns about empty implementations
 * when called, we are at the S_PROC -- S_MOBILE already digested
 */
PRIVATE treenode *rmobileprocdef (const int baseindent)
{
	wordnode *name;
	SOURCEPOSN locn = flocn;
	SOURCEPOSN endlocn = NOPOSN;
	treenode *decl = NULL;
	treenode *params = NULL;
	wordnode *iname = NULL;
	treenode *body;
	const int oldlexlevel = syn_lexlevel;
	BOOL suspends = FALSE;
	BOOL forks = FALSE;

	syn_lexlevel++;
	if (symb == S_PROC) {
		nextsymb ();
	}
	name = rname ();
	if (!name) {
		synerr_e (SYN_E_NAME, flocn, symb);
		goto error;
	}
	foundroutine = TRUE;
	/* then should have a formal-parameter list */
	if (checkfor (S_LPAREN)) {
		goto error;
	}
	if (symb == S_RPAREN) {
		/* no parameters */
		nextsymb ();
	} else {
		params = rparmlist ();
		if (!params) {
			goto error;
		}
		if (checkfor (S_RPAREN)) {
			goto error;
		}
	}
	/* allow newline before IMPLEMENTS */
	if (symb == S_NEWLINE) {
		nextsymb ();
		ignorecomments (baseindent);
	}
	/* should have an IMPLEMENTS now */
	if (checkfor (S_IMPLEMENTS)) {
		goto error;
	}
	iname = rname ();
	if (!iname) {
		synerr_e (SYN_E_NAME, flocn, symb);
		goto error;
	}
	/* if this is external, may have SUSPEND and/or FORK */
	if (symb == S_FORK) {
		nextsymb ();
		forks = TRUE;
	}
	if (symb == S_SUSPEND) {
		nextsymb ();
		suspends = TRUE;
	}

	switch (lexmode) {
	case LEX_SOURCE:
	case LEX_CSOURCE:
		if (checknlindent (baseindent + 2)) {
			goto error;
		}
		body = rprocess ();
		if (checkindent (baseindent)) {
			goto error;
		}
		while (symb == S_COMMENT) {
			if (checknlindent (baseindent)) {
				goto error;
			}
		}
		if (checkfor (S_COLON)) {
			goto error;
		}
		endlocn = flocn;
		break;
	default:
		body = rdescriptorbody (baseindent);
		endlocn = NOPOSN;
		break;
	}
	if (!body) {
		goto error;
	}

	syn_lexlevel = oldlexlevel;

	if (!mobile_proc_types) {
		synerr (SYN_NO_MPROCS, locn);
	}
	decl = declare (S_MPROCDECL, locn, params, name, body);
	SetDExtra (decl, (treenode *)iname);		/* attach IMPLEMENTs name here */

	if (suspends) {
		SetNPSuspends (DNameOf (decl), 1);
	}
	if (forks) {
		SetNPForks (DNameOf (decl), 1);
	}
	if (endlocn != NOPOSN) {
		DEBUG_MSG (("locn = %8lX, endlocn = %8lX\n", locn, endlocn));
		SetNPEndposn (DNameOf (decl), endlocn);
	}
	if (separatelycompiled (DNameOf (decl))) {
		addtoentrypointlist (DNameOf (decl));
	}

	if (checknlindent (baseindent)) {
		skiplines (baseindent);
		return (NULL);
	}
#if 0
fprintf (stderr, "rmobileprocdef: decl =");
printtreenl (stderr, 4, decl);
#endif
	return decl;
error:
	msg_out_s (SEV_INFO, SYN, SYN_SKIPPING_DEFN, NOPOSN, "MOBILE PROC");
	skiplines (baseindent);
	if (symb == S_COLON) {
		nextline ();
	}
	syn_lexlevel = oldlexlevel;
	return NULL;
}
/*}}}*/
/*{{{  PRIVATE void set_user_fixed (treenode *const spec)*/
/*
 *	sets the user-specified FIXED flag on a mobile variable.
 *	called with the whole specification.
 */
PRIVATE void set_user_fixed (treenode *const spec)
{
#if 0
fprintf (stderr, "syn2: set_user_fixed(): spec = ");
printtreenl (stderr, 4, spec);
#endif
	switch (TagOf (spec)) {
	case S_ABBR:
	case S_DECL:
		{
			treenode *name = DNameOf (spec);

			if (TagOf (name) == S_LIST) {
				while (!EndOfList (name)) {
					treenode *tnme = ThisItem (name);

					SetNTypeAttr (tnme, NTypeAttrOf (tnme) | TypeAttr_ufixed);
					name = NextItem (name);
				}
			} else {
				SetNTypeAttr (name, NTypeAttrOf (name) | TypeAttr_ufixed);
			}
		}
		break;
	default:
		synerr (SYN_MISPLACED_FIXED, LocnOf (spec));
		break;
	}
	return;
}
/*}}}*/
#endif
/*{{{  PRIVATE char *param_type_string (treenode *typetree, char *rbuf, int *rlen, int rsize)*/
/* Extracts string if type tree is user defined type, eg "COMPLEX", */
/* or, if the type is a base type (INT, REAL, etc) then it generates */
/* a string from that type instead, returns it and updates length */
/*
 *	FRMB update: fixed this so that the buffer goes in as a parameter
 */
#ifdef USER_DEFINED_OPERATORS
PRIVATE int param_type_string (treenode *typetree, char *rbuf, int *rlen, int rsize)
{
	int xlen = 0;

	switch (TagOf (typetree)) {
	case S_NAME:
		{
			char *name = (char *)WNameOf ((wordnode *)typetree);
			int nlen = WLengthOf ((wordnode *)typetree);

			xlen = (nlen < (rsize - *rlen)) ? nlen : (rsize - *rlen);
			strncpy (rbuf + *rlen, name, xlen);
		}
		break;
	case S_INT:
		xlen = snprintf (rbuf + *rlen, rsize - *rlen, "INT");
		break;
	case S_INT16:
		xlen = snprintf (rbuf + *rlen, rsize - *rlen, "INT16");
		break;
	case S_INT32:
		xlen = snprintf (rbuf + *rlen, rsize - *rlen, "INT32");
		break;
	case S_INT64:
		xlen = snprintf (rbuf + *rlen, rsize - *rlen, "INT64");
		break;
	case S_UINT:
		xlen = snprintf (rbuf + *rlen, rsize - *rlen, "UINT");
		break;
	case S_UINT16:
		xlen = snprintf (rbuf + *rlen, rsize - *rlen, "UINT16");
		break;
	case S_UINT32:
		xlen = snprintf (rbuf + *rlen, rsize - *rlen, "UINT32");
		break;
	case S_UINT64:
		xlen = snprintf (rbuf + *rlen, rsize - *rlen, "UINT64");
		break;
	case S_BOOL:
		xlen = snprintf (rbuf + *rlen, rsize - *rlen, "BOOL");
		break;
	case S_BYTE:
		xlen = snprintf (rbuf + *rlen, rsize - *rlen, "BYTE");
		break;
	case S_REAL32:
		xlen = snprintf (rbuf + *rlen, rsize - *rlen, "REAL32");
		break;
	case S_REAL64:
		xlen = snprintf (rbuf + *rlen, rsize - *rlen, "REAL64");
		break;
	case S_ARRAY:
		{
			int vlen;

			xlen = snprintf (rbuf + *rlen, rsize - *rlen, "ARRAY.OF.");
			vlen = *rlen + xlen;
			xlen += param_type_string (ARTypeOf (typetree), rbuf, &vlen, rsize);
		}
		break;
	default:
		msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, NOPOSN, "unknown base type in param_type_string");
		return 0;
	}
	*rlen = *rlen + xlen;

	return xlen;
}
#endif
/*}}}*/
/*{{{  PUBLIC treenode *rfunctiondef (typelist, locn, indent)*/
/* Terminates with symb at start of next line */
/* The return types of the function */
/* The file location of the function header */
/* The indent of the function header */
PUBLIC treenode *rfunctiondef (treenode *typelist, SOURCEPOSN locn, int indent)
{
	int t;			/* Tag used for treenode */
	wordnode *name;		/* Name of the function */
	treenode *params = NULL, *fbody;	/* Parameter list and function body */
	treenode *retptr;	/* Parse tree node returned */
	treenode *fntype;	/* Funtion type node */
	BOOL inline_decl = FALSE;
	BOOL recursive = FALSE;
	BOOL dyncall = FALSE;
	SOURCEPOSN endlocn;
	const int oldlexlevel = syn_lexlevel;
#ifdef USER_DEFINED_OPERATORS
	BOOL operator_decl = FALSE;
	int num_of_params = 0;
#endif
	DEBUG_MSG (("rfunctiondef... "));

	foundroutine = TRUE;
	syn_lexlevel++;
	/*{{{  extract function return type as a string */
	/*}}} */
	/*{{{  read function header */
	/*{{{  inline xor recursive ? */
	if (symb == S_INLINE) {
		inline_decl = current_fe_data->fe_allow_inlines;
		nextsymb ();
	} else if ((symb == S_RECURSIVE) || (symb == S_REC)) {
		recursive = TRUE;
		nextsymb ();
	}
	/*}}} */
	if (checkfor (S_FUNCTION) ||
#ifdef USER_DEFINED_OPERATORS
	    (name = rnameorstring (typelist, &operator_decl, &num_of_params)) == NULL ||	/* added by Jim 17/10/96, rname b4 */
#else
	    (name = rname ()) == NULL ||
#endif
	    checkfor (S_LPAREN) || checklinebreak ())
		goto error;
	if (symb != S_RPAREN)
		/*{{{  parse and declare the parameter list */
	{
		if ((params = rparmlist ()) == NULL)
			goto error;
	}
	/*}}} */
#ifdef USER_DEFINED_OPERATORS
	/*{{{  check for correct number of parameters if an overload function */
	if (operator_decl) {
		int params_declared;
		if (params == NULL) {
			params_declared = 0;
		} else {
			if (RightOf (params) == NULL) {
				params_declared = 1;
			} else {
				if (RightOf (RightOf (params)) == NULL) {
					params_declared = 2;
				} else {
					params_declared = 3;	/* or more */
				}
			}
		}
		if ((params_declared == 0) || (params_declared == 3)) {
			switch (num_of_params) {
			case 1:
				synerr_e (SYN_UDO_ONE_PARAM, flocn, symb);
				goto error;
			case 2:
				synerr_e (SYN_UDO_TWO_PARAMS, flocn, symb);
				goto error;
			case -1:
				synerr_e (SYN_UDO_ONE_OR_TWO_PARAMS, flocn, symb);
				goto error;
			}
		} else {
			if (params_declared == 1) {
				if (num_of_params == 1 || num_of_params == -1) {
					/* monadic operators */
					/* BUG FIX 201198: if you call a function with an address of 
					   a variable used later on in the parameter sequence, the
					   compiler does not guarantee the evaluation order on Linux
					   eg.  
					   f(g(&t), t)
					   If g updates t then the second t is not necessarily the
					   updated t.

					   FRMB note: I think this is not guaranteed in C generally ..
					 */
					int index = 0;
					char *namestring = (char *)memalloc (256);
					int namelength = 0;

					for (index = 0; index < 256; namestring[index++] = 0);

					namestring = strncpy (namestring, WNameOf (name), WLengthOf (name));
					namelength += WLengthOf (name);

					param_type_string (NTypeOf (LeftOf (params)), namestring, &namelength, 255);

					name = lookupword (namestring, namelength);
				} else {	/* declared one param, but need two */
					synerr_e (SYN_UDO_TWO_PARAMS, flocn, symb);
					goto error;
				}
			} else {
				if (params_declared == 2) {
					if ((num_of_params == 2) || (num_of_params == -1)) {	/* else code in here to extract name */
						/* code to extract parameter names */
						/* BUG FIX 201198: if you call a function with an address of 
						   a variable used later on in the parameter sequence, the
						   compiler does not guarantee the evaluation order on Linux
						   eg.  
						   f(g(&t), t)
						   If g updates t then the second t is not necessarily the
						   updated t.

					   	   FRMB note: I think this is not guaranteed in C generally ..
						 */
						int index = 0;
						char *namestring = (char *)memalloc (256);
						int namelength = 0;

						for (index = 0; index < 256; namestring[index++] = 0);

						namestring = strncpy (namestring, WNameOf (name), WLengthOf (name));
						namelength += WLengthOf (name);

						param_type_string (NTypeOf (LeftOf (params)), namestring, &namelength, 255);
						if (namelength < 255) {
							namestring[namelength] = '.';
							namelength++;
							namestring[namelength] = '.';
						}
						param_type_string (NTypeOf (LeftOf (RightOf (params))), namestring, &namelength, 255);
#if 0
fprintf (stderr, "syn2: rfunctiondef(): UDO: modified code gave namelength = %d, namestring = [%s]\n", namelength, namestring);
#endif

						name = lookupword (namestring, namelength);
					} else {
						synerr_e (SYN_UDO_ONE_PARAM, flocn, symb);
						goto error;
					}
				}
			}
		}
	}
#endif
	/*}}} */
	/*{{{  check for ) */
	if (checkfor (S_RPAREN))
		goto error;
	/*}}} */
	/* convert typelist to a list, if not one already */
	if (TagOf (typelist) != S_LIST)
		typelist = newlistnode (S_LIST, locn, typelist, NULL);
	fntype = newlistnode (S_FNTYPE, locn, typelist, params);
	/*}}} */
	if (symb == S_IS)
		/*{{{  short function definition */
	{
		t = S_SFUNCDEF;
		nextsymb ();
		/*{{{  ignore line break */
		if (checklinebreak ())
			goto error;
		/*}}} */
		/*{{{  parse expression list */
		if ((fbody = rlist (rexp, S_COMMA)) == NULL) {
			nextline ();
			goto error;
		}
		/*}}} */
		fbody = newvalofnode (S_VALOF, locn, newleafnode (S_SKIP, locn), fbody);
		/*{{{  check for : */
		if (checkfor (S_COLON)) {
			nextline ();
			goto error;
		}
		/*}}} */
		endlocn = flocn;
		/*}}} */
	} else {
		/*{{{  long or descriptor function definition */
		t = S_LFUNCDEF;
		switch (lexmode) {
		case LEX_SOURCE:
		case LEX_CSOURCE:
		{
			/*{{{  long function definition */
			treenode **fbodyptr = &fbody;
			/*{{{  check nl, indent */
			if (checknlindent (indent + 2))
				goto error;
			/*}}} */

			while (ignorecomments (indent + 2), symb != S_VALOF && symbindent == indent + 2) {
				/*{{{  parse leading specification */
				*fbodyptr = rspecification ();
				if (*fbodyptr != NULL) {
					fbodyptr = DBodyAddr (*fbodyptr);
				}
				/*}}} */
			}

			/*{{{  parse VALOF */
			if ((*fbodyptr = rvalof ()) == NULL)
				goto error;
			/*}}} */
			/*{{{  check indent */
			if (checkindent (indent))
				goto error;
			/*}}} */
			while (symb == S_COMMENT)
				/*{{{  check nl, indent */
				if (checknlindent (indent))
					goto error;
			/*}}} */
			/*{{{  check for : */
			if (checkfor (S_COLON))
				goto error;
			/*}}} */
			endlocn = flocn;
		}
			break;
			/*}}} */
		default:
		{
			/*{{{  descriptor function definition */
			fbody = rdescriptorbody (indent);
			if (fbody == NULL)
				goto error;
			endlocn = NOPOSN;
		}
			break;
			/*}}} */
		}
		/*}}} */
	}
	/*{{{  if DEXTERNAL, make sure we always call instances dynamically*/
	if (lexmode == LEX_DEXTERNAL) {
		dyncall = 1;
	}

	/*}}}*/

	syn_lexlevel = oldlexlevel;
	retptr = declare (t, locn, fntype, name, fbody);
	{
		treenode *const nptr = DNameOf (retptr);
		if (endlocn != NOPOSN) {
			SetNPEndposn (nptr, endlocn);
		}
		if (separatelycompiled (nptr)) {
			addtoentrypointlist (nptr);
		}
		if (inline_decl) {
			SetTag (nptr, N_INLINEFUNCDEF);
		}
		if (recursive) {
			SetNPRecursive (nptr, 1);
		}
		if (dyncall) {
			SetNPDyncall (nptr, 1);
		}
	}
	if (checknlindent (indent)) {
		skiplines (indent);
		return (NULL);
	}
	return retptr;

      error:
	/*{{{  discard function definition */
	msg_out_s (SEV_INFO, SYN, SYN_SKIPPING_DEFN, NOPOSN, "FUNCTION");
	skiplines (indent);
	if (symb == S_COLON)
		nextline ();
	syn_lexlevel = oldlexlevel;
	return NULL;
	/*}}} */

}

/*}}}*/
/*{{{  PUBLIC BOOL rfile ()*/
/*
 *	this is called to handle #INCLUDE, #USE directives.
 *	Expecting a string as the next symbol.
 */
PUBLIC BOOL rfile (void)
{
	BOOL entered = FALSE;
	const int s = symb;
	const int indent = lineindent;

	nextsymb ();
	if (symb != S_STRING) {
		synerr (SYN_MISSING_FILENAME, flocn);
	} else if (s == S_SC) {
		synerr_s (SYN_SC_IS_OBSOLETE, flocn, literalv);
	} else {
		/*{{{  try and open the file */
		char litbuf[MAXSTRING_SIZE];
		int litlen = 0;
		int mode;
		const SOURCEPOSN litlocn = flocn;

		litlen = strlen (literalv);
		if (litlen >= MAXSTRING_SIZE) {
			litlen = MAXSTRING_SIZE - 1;
		}
		memcpy (litbuf, literalv, litlen);
		litbuf[litlen] = '\0';

		lex_save_state ();
		nextsymb ();
		while (symb == S_STRING) {
			/* absorb next literal, add to string */
			int blen = strlen (literalv);

			if ((litlen + blen) >= MAXSTRING_SIZE) {
				blen = (MAXSTRING_SIZE - 1) - litlen;
			}
			if (blen > 0) {
				memcpy (litbuf + litlen, literalv, blen);
				litlen += blen;
				litbuf[litlen] = '\0';
			}
			lex_restore_state ();
			nextsymb ();
			lex_save_state ();
			nextsymb ();
		}
		lex_restore_state ();

		switch (s) {
		case S_INCLUDE:
			if (strstr (litbuf, ".co") == (litbuf + (litlen - 3))) {
				mode = LEX_CSOURCE;
			} else {
				mode = LEX_SOURCE;	/* #INCLUDE MUST specify the extension */
			}
			break;
		default:
			/* S_SC, S_IMPORT or S_USE, all are descriptor files */
			mode = ((s == S_SC) ? LEX_SC : LEX_LIB);
			current_fe_data->fe_process_filename_fn (litbuf, MAXSTRING_SIZE);
			break;
		}

		entered = open_file (litbuf, mode, indent);

		if (!entered) {
			synerr_s (SYN_FILE_OPEN_ERROR, litlocn, litbuf);
		} else {
			if (current_fe_data->fe_information) {
				fprintf (current_fe_data->fe_outfile, "%s \"%s\"\n", tagstring (s), litbuf);
			}
			nextsymb ();
			while (symb == S_NEWLINE) {	/* Throw away leading blank lines */
				nextsymb ();
			}
			ignorecomments (0);	/* Throw away leading comments */
		}
		/*}}} */
	}
	if (!entered) {
		nextline ();
	}
	return entered;
}

/*}}}*/
/*{{{  pragma reading*/
/*{{{  PRIVATEPARAM treenode *rname_treenode*/
PRIVATEPARAM treenode *rname_treenode (void)
{
	return (treenode *) rname ();
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *rpragma_external*/
PRIVATEPARAM treenode *rpragma_external (wordnode * const pragma_name, const pragma_name_tag_t pragma_name_tag, int tag, const int indent)
{
	const SOURCEPOSN locn = flocn;
	treenode *tptr = NULL;
	USE_VAR (tag);		/* prevent unused variable warning */
	if ((symb == S_STRING) || (symb == S_STRINGCONT)) {
		treenode *const str = rstring ();
		const BOOL ok = open_file (WNameOf (CTValOf (str)), (pragma_name_tag == pragma_name_dexternal) ? LEX_DEXTERNAL : LEX_EXTERNAL, indent);
		if (ok) {
			treenode *const pragma_nptr = declname (N_DECL, locn, pragma_name, NULL, NULL);
			SetNMode (pragma_nptr, pragma_name_tag);
			tptr = newdeclnode (S_PRAGMA, locn, pragma_nptr, str, NULL);

			if (current_fe_data->fe_information) {
				fprintf (current_fe_data->fe_outfile, "%s %s \"%s\"\n",
					 tagstring (S_PRAGMA), WNameOf (pragma_name), WNameOf (CTValOf (str)));
			}
			nextsymb ();
		} else
			nextline ();
	} else {
		synwarn_s (SYN_BAD_PRAGMA_DIRECTIVE, flocn, WNameOf (pragma_name));
		nextline ();
	}
#if 0
fprintf (stderr, "rpragma_external: returning tptr =");
printtreenl (stderr, 4, tptr);
#endif
	return tptr;
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *rpragma_string*/
PRIVATEPARAM treenode *rpragma_string (wordnode * const pragma_name, const pragma_name_tag_t pragma_name_tag, int tag, const int indent)
{
	treenode *tptr = NULL;
	treenode *str = NULL;
	wordnode *name = pragma_name;
	BOOL ok = TRUE;
	switch (symb) {
	case S_STRING:
	case S_STRINGCONT:
		str = rstring ();
		nextsymb ();
		break;
/*case S_NEWLINE: *//* Now we allow anything, (eg comments) */
	default:
		/*{{{  what to do if there's no string */
		if (pragma_name == NULL) {	/* called from rsubpragma */
			synerr_s (SYN_E_STRING_AFTER, flocn, tagstring (tag));
			ok = FALSE;
		} else if (pragma_name_tag != pragma_name_linkage) {
			synwarn_s (SYN_BAD_PRAGMA_DIRECTIVE, flocn, WNameOf (pragma_name));
			ok = FALSE;
		}
		/*}}} */
		break;
	}

	if (pragma_name == NULL) {	/* called from rsubpragma */
		const char *const string = tagstring (tag);
		name = lookupword (string, strlen (string));
	}

	if (ok) {
		treenode *const pragma_nptr = declname (N_DECL, flocn, name, NULL, NULL);
		SetNMode (pragma_nptr, pragma_name_tag);
		tptr = newdeclnode (S_PRAGMA, flocn, pragma_nptr, str, NULL);

		/*{{{  #NETWORK may have multiple strings */
		if (pragma_name_tag == pragma_name_hardware) {
			/* more than one string in the list */
			SetDVal (tptr, newlistnode (S_LIST, flocn, str, NULL));	/* turn rhs into a list */
			while (symb == S_STRING) {
				str = rstring ();
				SetDVal (tptr, appendnode (str, DValOf (tptr)));
				nextsymb ();
			}
		}
		/*}}} */

		/*{{{  information */
		if (current_fe_data->fe_information) {
			FILE *const of = current_fe_data->fe_outfile;
			treenode *temp = DValOf (tptr);
			if (pragma_name == NULL)
				fprintf (of, "%s", tagstring (tag));
			else
				fprintf (of, "%s %s", tagstring (S_PRAGMA), WNameOf (pragma_name));
			if (TagOf (temp) == S_LIST) {
				for (; !EndOfList (temp); temp = NextItem (temp))
					fprintf (of, " \"%s\"", WNameOf (CTValOf (ThisItem (temp))));
			} else
				fprintf (of, " \"%s\"", WNameOf (CTValOf (temp)));
			fputc ('\n', of);
		}
		/*}}} */
	}
	checknlindent (indent);
	return tptr;
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *rpragma_translate*/
PRIVATEPARAM treenode *rpragma_translate (wordnode * const pragma_name, const pragma_name_tag_t pragma_name_tag, int tag, const int indent)
{
	treenode *tptr = NULL;
	wordnode *wptr = rname ();	/* if ok, this also skips past the name */
	USE_VAR (tag);		/* prevent unused variable warning */
	if (wptr != NULL) {
		if (symb == S_STRING) {
			const BOOL ok = setup_translation (wptr, literalv, literalp, flocn);
			nextsymb ();	/* skip over the string */
			if (ok) {
				treenode *const pragma_nptr = declname (N_DECL, flocn, pragma_name, NULL, NULL);
				SetNMode (pragma_nptr, pragma_name_tag);
				tptr = newdeclnode (S_PRAGMA, flocn, pragma_nptr,
						    addtofront (newconsttablenode (S_STRING, flocn, wptr, NULL),
								addtofront (newconsttablenode
									    (S_STRING, flocn, lookupword (literalv, literalp), NULL), NULL)), NULL);

				if (current_fe_data->fe_information) {
					fprintf (current_fe_data->fe_outfile, "%s %s %s \"%s\"\n",
						 tagstring (S_PRAGMA), WNameOf (pragma_name), WNameOf (wptr), literalv);
				}
			}
		} else if (symb == S_STRINGCONT)
			synerr_i (SYN_STRING_TOO_LONG, flocn, MAXSTRING_SIZE);
		else
			synwarn_s (SYN_BAD_PRAGMA_DIRECTIVE, flocn, WNameOf (pragma_name));
	}
#if 0
	/*nextline(); */
	checknewline ();
	USE_VAR (indent);	/* avoid unused variable warning */
#else
	checknlindent (indent);
#endif
	return tptr;
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *rpragma_namelist*/
PRIVATEPARAM treenode *rpragma_namelist (wordnode * const pragma_name, const pragma_name_tag_t pragma_name_tag, int tag, const int indent)
{
	treenode *tptr = NULL;
	treenode *const rhs = rlist (rname_treenode, S_COMMA);

	USE_VAR (tag);		/* prevent unused variable warning */
	if (rhs != NULL) {
		treenode *const pragma_nptr = declname (N_DECL, flocn, pragma_name, NULL, NULL);
		SetNMode (pragma_nptr, pragma_name_tag);
		tptr = newdeclnode (S_PRAGMA, flocn, pragma_nptr, rhs, NULL);

		if (current_fe_data->fe_information) {
#if 0
			fprintf (current_fe_data->fe_outfile, "%s %s %s\n", tagstring (S_PRAGMA), WNameOf (pragma_name), WNameOf (rhs));
#endif
			treenode *n;
			fprintf (current_fe_data->fe_outfile, "%s %s", tagstring (S_PRAGMA), WNameOf (pragma_name));
			for (n = rhs; !EndOfList (n); n = NextItem (n)) {
				fprintf (current_fe_data->fe_outfile, "%s %s", n == rhs ? "" : ",", WNameOf ((wordnode *) ThisItem (n)));
			}
			fputc ('\n', current_fe_data->fe_outfile);
		}
	}
	if (indent >= 0) {
		checknlindent (indent);
	} else {
		checknewline ();
	}
	return tptr;
}

/*}}}*/
/*{{{  PRIVATEPARAM treenode *rpragma_name*/
PRIVATEPARAM treenode *rpragma_name (wordnode * const pragma_name, const pragma_name_tag_t pragma_name_tag, int tag, const int indent)
{
	treenode *tptr = NULL;
	wordnode *const rhs = rname ();	/* if ok, this also skips past the name */
	USE_VAR (tag);		/* prevent unused variable warning */
	if (rhs != NULL) {
		treenode *const pragma_nptr = declname (N_DECL, flocn, pragma_name, NULL, NULL);
		SetNMode (pragma_nptr, pragma_name_tag);
		tptr = newdeclnode (S_PRAGMA, flocn, pragma_nptr, (treenode *) rhs, NULL);

		if (current_fe_data->fe_information) {
			fprintf (current_fe_data->fe_outfile, "%s %s %s\n", tagstring (S_PRAGMA), WNameOf (pragma_name), WNameOf (rhs));
		}
	}
	checknlindent (indent);
	return tptr;
}

/*}}}*/

/*{{{  pragma_list*/
typedef enum
{
	A = FE_LANG_ALL,	/* all languages */
	O = FE_LANG_OCCAM,	/* occam compiler */
	C = FE_LANG_CONFIG2 | FE_LANG_CONFIG3,	/* configurer */
	CC = FE_LANG_OCCAM | C,	/* compiler and configurer */
	CN = FE_LANG_NDL | C,	/* compiler and configurer */
	C3 = FE_LANG_CONFIG3	/* Version 3 configurer */
}
lang_t;

PRIVATE const struct
{
	const char *pragma_name;
	treenode *(*pragma_fn) (wordnode * const pragma_name, const pragma_name_tag_t pragma_tag, int tag, const int indent);
	lang_t pragma_lang;
	pragma_name_tag_t pragma_name_tag;
}
pragma_list[] = {

	/* No #PRAGMA EXTERNAL when configuring, because that requires linking */
	/* No point in having LINKAGE pragma when configuring, cos we don't link
	   after configuring - CON 11/10/90 */

	  { "EXTERNAL", rpragma_external, O, pragma_name_external}
	, { "DEXTERNAL", rpragma_external, O, pragma_name_dexternal}
	, { "EXPORT", rpragma_namelist, CC, pragma_name_export}
	, { "LINKAGE", rpragma_string, O, pragma_name_linkage}
	, { "SHARED", rpragma_namelist, O, pragma_name_shared}
	, { "PERMITALIASES", rpragma_namelist, O, pragma_name_aliased}
	, { "ASSUMECONSTANT", rpragma_namelist, O, pragma_name_assumeconst}
	, { "COMMENT", rpragma_string, CC, pragma_name_comment}
	, { "TRANSLATE", rpragma_translate, CC, pragma_name_translate}
	, { "HARDWARE", rpragma_string, C3, pragma_name_hardware}
	, { "NESTEDTIMER", rpragma_name, O, pragma_name_nestedtimer}
	, { "NESTEDPLACE", rpragma_name, O, pragma_name_nestedplace}
	, { "NESTEDPORT", rpragma_name, O, pragma_name_nestedport}
	, { "BADLYBEHAVED", rpragma_name, O, pragma_name_badlybehaved}
	, { "DEFINED", rpragma_namelist, O, pragma_name_defined}
	, { "UNDEFINED", rpragma_namelist, O, pragma_name_undefined}
	, { "IOSPACE", rpragma_namelist, O, pragma_name_iospace}
	, { "DYNCALL", rpragma_namelist, CC, pragma_name_dyncall}
	, { "FORMALMODEL", rpragma_string, O, pragma_name_formalmodel}
	, { "FMTYPES", rpragma_namelist, CC, pragma_name_fmtypes}
};

/*}}}*/

/*{{{  PRIVATE treenode *rpragma*/
PRIVATE treenode *rpragma (void)
{
	wordnode *wptr;
	int i;
/*  const int indent = lineindent;*//* bug 829 20/9/91 */
	const int indent = symbindent;

	nextsymb ();		/* skip past the #PRAGMA */

	/* frmb: SHARED pragma now appears as S_SHARED, so do the right thing */
	if (symb == S_SHARED) {
		nextsymb ();
		wptr = lookupword ("SHARED", 6);
		if (current_fe_data->fe_lang & O) {
			return rpragma_namelist (wptr, pragma_name_shared, 0, indent);
		}
		synwarn_s (SYN_BAD_PRAGMA_NAME, flocn, WNameOf (wptr));
		nextline ();
		return NULL;
	}
	/* and similarly with DEFINED.. */
	if (symb == S_DEFINED) {
		nextsymb ();
		wptr = lookupword ("DEFINED", 7);
		if (current_fe_data->fe_lang & O) {
			return rpragma_namelist (wptr, pragma_name_defined, 0, -1);		/* don't care about the indentation following this */
		}
		synwarn_s (SYN_BAD_PRAGMA_NAME, flocn, WNameOf (wptr));
		nextline ();
		return NULL;
	}
	/* and with DYNCALL */
	if (symb == S_DYNCALL) {
		nextsymb ();
		wptr = lookupword ("DYNCALL", 7);
		if (current_fe_data->fe_lang & O) {
			return rpragma_namelist (wptr, pragma_name_dyncall, 0, -1);
		}
		synwarn_s (SYN_BAD_PRAGMA_NAME, flocn, WNameOf (wptr));
		nextline ();
		return NULL;
	}
	wptr = rname ();	/* if ok, this also skips past the name */
	if (wptr == NULL)
		nextline ();	/* we just supplied an error message */
	else {
		for (i = 0; i < (sizeof (pragma_list) / sizeof (pragma_list[0])); i++)
			if (((pragma_list[i].pragma_lang & current_fe_data->fe_lang) != 0) &&
			    (strcmp (WNameOf (wptr), pragma_list[i].pragma_name) == 0))
					return pragma_list[i].pragma_fn (wptr, pragma_list[i].pragma_name_tag, 0, indent);

		/*synerror(SYN_BAD_PRAGMA_NAME, flocn, (BIT32)symb); */
		synwarn_s (SYN_BAD_PRAGMA_NAME, flocn, WNameOf (wptr));
		nextline ();
	}

	return NULL;
}

/*}}}*/
/*{{{  PRIVATE treenode *rsubpragma*/
PRIVATE treenode *rsubpragma (const pragma_name_tag_t pragma_name_tag)
{
	const int tag = symb;
	const int indent = symbindent;
	nextsymb ();		/* skip past the #COMMENT etc */
	return rpragma_string (NULL, pragma_name_tag, tag, indent);
}

/*}}}*/
/*}}}*/

/* Skips to new line on error */
/*{{{  PUBLIC treenode *rspecification ()*/
/* Terminates with symb at start of next line */
PUBLIC treenode *rspecification (void)
{
	const int indent = symbindent;
	SOURCEPOSN locn = flocn;
#ifdef MOBILES
	BOOL fixedflag = FALSE;
#endif

	DEBUG_MSG (("rspecification... "));

#if 0
fprintf (stderr, "rspecification...\n");
#endif
	linebreakindent = (-1);	/* bug TS/1493 27/11/91 */

#ifdef MOBILES
	/*{{{  deal with fixed attribute if present*/
	if (symb == S_FIXED) {
		/* swallow this up and check later */
		fixedflag = TRUE;
		nextsymb ();
	}
	/*}}}*/
#endif
	switch (symb) {
		/*{{{  S_VAL */
	case S_VAL:
		/* Parse
		   'VAL' [specifier] name 'IS' expression ':'
		   or 'VAL' specifier name 'RETYPES' expression ':'
		 */
		{
			treenode *spec;
			wordnode *name;
			nextsymb ();
			/*{{{  read in specifier, if present */
			switch (symb) {
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
			case S_CHAN:
			case S_PORT:
			case S_LBOX:
			case S_TIMER:
			case S_BOX:
#ifdef MOBILES
			case S_MOBILE:
#endif	/* MOBILES */
			CASE_CONFIG_TYPE
				/*{{{  parse specifier */
				spec = rspecifier ();
				if (spec == NULL) {
					goto error;
				}
				/*}}} */
				break;
			default:
				spec = NULL;
			};
			/*}}} */
			/*{{{  parse name */
			if ((name = rname ()) == NULL)
				goto error;
			/*}}} */
#ifdef OCCAM2_5
			/*{{{  allow for VAL type.name name IS ... */
			if ((spec == NULL) && (symb == S_NAME)) {
				spec = (treenode *) name;
				name = rname ();
				if (name == NULL)
					goto error;
			}
			/*}}} */
#endif
			switch (symb) {
				/*{{{  parse val abbreviation */
			case S_IS:
				nextsymb ();
				/*{{{  ignore line break */
				if (checklinebreak ())
					goto error;
				/*}}} */
				return (rvalabbr (spec, name, locn, indent));
				/*}}} */
				/*{{{  parse val retype */
#ifdef OCCAM2_5
				/*{{{  RETYPES / RESHAPES */
			case S_RETYPES:
			case S_RESHAPES:
				{
					const BOOL reshapes = (symb == S_RESHAPES);
					nextsymb ();
					/*{{{  ignore line break */
					if (checklinebreak ())
						goto error;
					/*}}} */
					return rvalretype (spec, name, reshapes, locn, indent);
				}
				/*}}} */
#else
				/*{{{  RETYPES */
			case S_RETYPES:
				nextsymb ();
				/*{{{  ignore line break */
				if (checklinebreak ())
					goto error;
				/*}}} */
				return rvalretype (spec, name, locn, indent);
				/*}}} */
#endif

				/*}}} */
				/*{{{  error */
			default:
				if ((current_fe_data->fe_lang & FE_LANG_NDL) != 0)
					synetoken (S_IS);	/* bug TS/1466 12/11/91 */
				else
					synerr_e (SYN_E_IS_OR_RETYPES, locn, symb);
				goto error;
				/*}}} */
			}
		}
		/*}}} */
		/*{{{  S_PROC */
	case S_INLINE:
	case S_PROC:
#ifndef MOBILES
	case S_RECURSIVE:
	case S_REC:
#endif
		return (rprocdef ());
		/*}}} */
		/*{{{  S_PROTOCOL */
	case S_PROTOCOL:
		return (rprotdef ());
		/*}}} */
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
#ifndef MOBILES			/* don't allow records of channels and catch CHAN here */
	case S_CHAN:
#endif
#ifdef MOBILES
	case S_ANYCHANTYPE:	/* scoop-up in rspecifier() */
	case S_ANYPROCTYPE:
	case S_ANYMOBILETYPE:
#endif
	case S_PORT:
	case S_TIMER:
	case S_LBOX:
	case S_BOX:
	case S_BARRIER:
	case S_BUFFERED:

	CASE_CONFIG_TYPE
		{
			/*{{{  COMMENT what we are parsing
			 * Parse:
			 *	specifier name 'IS' element ':'                         |
			 *	specifier name 'RETYPES' element ':'                    |
			 *	type {1 ',' name } ':'                                  |
			 *	type name                                               |
			 *	init.body
			 *	':'
			 *	{1 ',' type } 'FUNCTION' name '(' {0 ',' formal } ')'
			 *	  function.body 
			 *	':'                                                     | 
			 *	{1 ',' type } 'FUNCTION' name '(' {0 ',' formal } ')' 'IS' exprlist ':'		|
			 *	BUFFERED (<const-exp>) CHAN ..
			 *
			 *	types and specifiers are treated as equivalent
			 */
			/*}}}*/
			treenode *t, *spec;

			/*{{{  parse specifier */
			if ((t = rspecifier ()) == NULL) {
				goto error;
			}
			/*}}} */
			spec = rspecnameandbody (t, locn, indent, TRUE);

			return spec;
		}
		/*}}} */
#ifdef MOBILES
		/*{{{  S_MOBILE*/
	case S_MOBILE:		/* either a regular MOBILE declaration or a MOBILE PROC declaration */
		{
			treenode *spec;
			treenode *t;

			if (!mobile_data_types) {
				synerr (SYN_NO_MOBILES, locn);
			}
			nextsymb ();
			if (symb == S_PROC) {
				/* assuming we have a MOBILE PROC definition */
				spec = rmobileprocdef (indent);
			} else {
				spec = rspecifier ();
				if (!spec) {
					goto error;
				}

				t = newtypenode (S_MOBILE, locn, NULL, spec);
				spec = rspecnameandbody (t, locn, indent, TRUE);
			}
			
			if (fixedflag) {
				set_user_fixed (spec);
			}
			return spec;
		}
		/*}}}*/
		/*{{{  S_CHAN  (mobiles)*/
	case S_CHAN:		/* records of channels */
		nextsymb ();
		if (symb == S_TYPE) {
			return rtypedecl (FALSE, indent);
		} else {
			/* same as in rspecifier() */
			treenode *t;
			treenode *prot;
			BOOL is_shared = FALSE;

			/* optional OF */
			if (symb == S_OF) {
				nextsymb ();
			}
			/* maybe it's a chan of a shared chan-type ? */
			if (symb == S_SHARED) {
				is_shared = TRUE;
				nextsymb ();
			}
			if ((prot = rprotocol ()) == NULL) {
				goto error;
			}
			if (TagOf (prot) == S_ANYCHANTYPE) {
				/* slightly different handling for these -- attributes hang under it in a MOBILE typenode */
#if 0
fprintf (stderr, "rspecification: (CHAN) got ANYCHANTYPE: prot = ");
printtreenl (stderr, 4, prot);
#endif
				if (!LeafLinkOf (prot)) {
					SetLeafLink (prot, newtypenode (S_MOBILE, locn, NULL, NULL));
				}
				if (is_shared) {
					SetTypeAttr (LeafLinkOf (prot), TypeAttrOf (LeafLinkOf (prot)) | TypeAttr_shared);
				}
				if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
					SetTypeAttr (LeafLinkOf (prot), TypeAttrOf (LeafLinkOf (prot)) | ((symb == S_INPUT) ? TypeAttr_marked_in : TypeAttr_marked_out));
					nextsymb ();
				}
				t = newtypenode (S_CHAN, locn, NULL, prot);
			} else {
				if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
					/* must be channel of a chan-type, this is the client/server specifier */
					prot = newmopnode ((symb == S_INPUT) ? S_ASINPUT : S_ASOUTPUT, locn, prot, S_CHAN);
					nextsymb ();
#if 0
fprintf (stderr, "rspecification: found INPUT/OUTPUT before calling rspecnameandbody.  prot = \n");
printtreenl (stderr, 4, prot);
#endif
				}
				t = newtypenode (S_CHAN, locn, NULL, prot);
				/* if shared chan-type, bung this info in the TypeAttr of the channel (will get pushed into the protocol later) */
				if (is_shared) {
#if 0
fprintf (stderr, "rspecification: putting TypeAttr_shared in CHAN\n");
#endif
					SetTypeAttr (t, TypeAttrOf (t) | TypeAttr_shared);
				}
			}
			return rspecnameandbody (t, locn, indent, TRUE);
		}
		/*}}}*/
		/*{{{  S_RECURSIVE/S_REC*/
	case S_RECURSIVE:
	case S_REC:
		{
			BOOL is_chantype = FALSE;

			/* whichever one we decide (PROCDEF or TYPEDECL), parser wants to be at "recursive" -- use lexer abuse.. */
			lex_save_state ();
			nextsymb ();
			if (symb == S_CHAN) {
				is_chantype = TRUE;
			}
			lex_restore_state ();
			if (!is_chantype) {
				return rprocdef ();
			}
			/* right, recursive chan-type declaration */
			return rtypedecl (FALSE, indent);
		}
		/*}}}*/
#endif	/* MOBILES */
		/*{{{  S_NAME */
	case S_NAME:
		{
			wordnode *const name = rname ();
			treenode *spec;

			switch (symb) {
			case S_IS:
				spec = rrestofspec (NULL, (treenode *) name, locn, indent, TRUE);
				break;
#ifdef OCCAM2_5
			case S_NAME:
			case S_FUNCTION:
			case S_INLINE:
			case S_COMMA:
			case S_RECURSIVE:
			case S_REC:
				spec = rspecnameandbody ((treenode *) name, locn, indent, TRUE);
				break;
#endif
#ifdef MOBILES
			case S_INPUT:
			case S_OUTPUT:
				/* this happens in "SHARED FOO[?!] name, name..:", also "FIXED NAME[?!] ..:" */
				{
					const int csymb = symb;
					treenode *type = (treenode *)name;
					treenode *namelist = NULL;

					nextsymb ();
					/* scoop up names until we get to a ":" or "IS" */

					while ((symb != S_COLON) && (symb != S_IS)) {
						treenode *vname;

						if ((vname = (treenode *)rname()) == NULL) {
							/* expected name */
							synerr_e (SYN_E_NAME, locn, symb);
							goto error;
						}
						namelist = newlistnode (S_LIST, locn, vname, namelist);
						if (symb == S_COMMA) {
							nextsymb ();
						} else if ((symb != S_COLON) && (symb != S_IS)) {
							synerr_e (SYN_E_COLON_IS, locn, symb);
							goto error;
						}
					}

					/* set chan-dir spec */
					type = newmopnode ((csymb == S_INPUT) ? S_ASINPUT : S_ASOUTPUT, locn, type, S_CHAN);

					spec = rrestofspec (type, ((symb == S_IS) ? ThisItem (namelist) : namelist), locn, indent, FALSE);
				}
				break;
#endif
			default:
				synetoken (S_IS);
				goto error;
			}

#ifdef MOBILES
			if (fixedflag) {
				set_user_fixed (spec);
			}
#endif
			return spec;
		}
		/*}}} */
		/*{{{  S_NETWORK: */
	CASE_CONFIG_SPEC
		return (rconfigdef ());
		/*}}} */
		/*{{{  S_PLACE */
	case S_PLACE:
		{
			treenode *n;
			treenode *e = NULL;
			int tag;
			nextsymb ();
			/*{{{  parse name */
			if ((n = relement ()) == NULL) {
				goto error;
			}
			/*}}} */
			/* might have a channel-direction specifier */
			if (symb == S_INPUT) {
				nextsymb ();
				n = newmopnode (S_ASINPUT, locn, n, S_NAME);
			} else if (symb == S_OUTPUT) {
				nextsymb ();
				n = newmopnode (S_ASOUTPUT, locn, n, S_NAME);
			}
			if (symb == S_AT) {
				/*{{{  PLACE name AT [ WORKSPACE ] expression : */
				if (TagOf (n) != S_NAME) {
					synerr (SYN_PLACE_NAME, flocn);
					goto error;
				}
				nextsymb ();
				if (symb == S_WORKSPACE) {
					tag = S_WSPLACE;
					nextsymb ();
				} else
					tag = S_PLACE;
				if ((e = rexp ()) == NULL)
					goto error;
				/*}}} */
			} else if (symb == S_IN) {
				/*{{{  PLACE name IN ( WORKSPACE | VECSPACE ) : */
				if (TagOf (n) != S_NAME) {
					synerr (SYN_PLACE_NAME, flocn);
					goto error;
				}
				nextsymb ();
				if (symb == S_WORKSPACE)
					tag = S_WSPLACE;
				else if (symb == S_VECSPACE)
					tag = S_VSPLACE;
				else {
					synerr_e (SYN_E_WS_OR_VS, flocn, symb);
					goto error;
				}
				nextsymb ();
				/*}}} */
			} else if (((current_fe_data->fe_lang & (FE_LANG_CONFIG2 | FE_LANG_CONFIG3)) != 0) && ((symb == S_ON) || (symb == S_COMMA))) {
				/*{{{  PLACE {1 ',' element} ON element : */
				treenode *list = NULL;
				if (symb == S_COMMA) {
					nextsymb ();
					if (checklinebreak ())
						goto error2;
					if ((list = rlist (relement, S_COMMA)) == NULL)
						goto error;
				}
				n = newlistnode (S_LIST, flocn, n, list);
				if (checkfor (S_ON))
					goto error;
				if ((e = relement ()) == NULL)
					goto error;
				tag = S_PLACEON;
				/*}}} */
			} else {
				/*{{{  may optional AT, so expect some placement expression, or WORKSPACE [expression]: or VECTORSPACE: */
				switch (symb) {
				case S_WORKSPACE:
					tag = S_WSPLACE;
					nextsymb ();
					if (symb != S_COLON) {
						if ((e = rexp()) == NULL) {
							goto error;
						}
					}
					break;
				case S_VECSPACE:
					tag = S_VSPLACE;
					break;
				default:
					if ((e = rexp ()) == NULL) {
						goto error;
					}
					tag = S_PLACE;
					break;
				}
				/*}}} */
			}
			/*{{{  check for : */
			if (checkfor (S_COLON)) {
				goto error;
			}
			/*}}} */
			/*{{{  check nl, indent */
			if (checknlindent (indent)) {
				goto error2;
			}
			/*}}} */
			return newdeclnode (tag, locn, n, e, NULL);
		}
		/*}}} */
		/*{{{  S_PRAGMA */
	case S_PRAGMA:		/* bug 829 19/9/91 */
		{
			treenode *tptr = rpragma ();

			if (tptr == NULL) {
				goto error2;
			}
#if 0
fprintf (stderr, "rspecification(): parsed PRAGMA, got:\n");
printtreenl (stderr, 4, tptr);
#endif
			return tptr;
		}
		/*}}} */
		/*{{{  S_HCOMMENT */
	case S_HCOMMENT:	/* bug 829 20/9/91 */
		{
			treenode *const tptr = rsubpragma (pragma_name_comment);
			if (tptr == NULL)
				goto error2;
			return tptr;
		}
		/*}}} */
#ifndef ARRAYCONSTRUCTOR
		/*{{{  S_HNETWORK */
	case S_HNETWORK:
		{
			treenode *const tptr = rsubpragma (pragma_name_hardware);
			if (tptr == NULL)
				goto error2;
			if ((current_fe_data->fe_lang & FE_LANG_CONFIG3) == 0) {
				synwarn_s (SYN_BAD_PRAGMA_NAME, LocnOf (tptr), tagstring (S_HNETWORK));
				return NULL;
			}
			return tptr;
		}
		/*}}} */
#endif	/* !ARRAYCONSTRUCTOR */
		/*{{{  S_INCLUDE S_SC S_USE etc */
	case S_INCLUDE:
	case S_SC:
	case S_USE:
	case S_IMPORT:
		rfile ();
		return rspecification ();
		/*}}} */
		/*{{{  S_DATA */
#ifdef OCCAM2_5
	case S_DATA:
		return rtypedecl (TRUE, indent);
#endif
		/*}}} */
#ifdef MOBILES
		/*{{{  S_SHARED*/
	case S_SHARED:
		{
			treenode *realspec;
			int extra_bits = 0;

			nextsymb ();
			/* scoop up any ! or ? specifier on SHARED */
			if (symb == S_INPUT) {
				extra_bits = TypeAttr_marked_in;
				nextsymb ();
			} else if (symb == S_OUTPUT) {
				extra_bits = TypeAttr_marked_out;
				nextsymb ();
			}

			if (symb == S_CHAN) {
				/* this is NOT a shared channel-type (!), which shows up as symb == S_NAME).
				 * we parse anonymous channel-types here:
				 *   SHARED CHAN [OF] <protocol> {1,<name>}:
				 * ends up as:
				 *   SHARED $anon.<protocol>! {1, <name>$cli}:
				 *   SHARED $anon.<protocol>? {1, <name>$svr}:
				 */
				treenode *t;
				treenode *prot;
				BOOL is_shared = FALSE;

				nextsymb ();
				/* optional OF */
				if (symb == S_OF) {
					nextsymb ();
				}
				/* chan of shared somethings ? */
				if (symb == S_SHARED) {
					is_shared = TRUE;
					nextsymb ();
				}
				if ((prot = rprotocol ()) == NULL) {
					return NULL;
				}
				if ((symb == S_INPUT) || (symb == S_OUTPUT)) {
#if 0
fprintf (stderr, "syn2: INPUT/OUTPUT in SHARED CHAN ... declaration.  prot =");
printtreenl (stderr, 4, prot);
#endif
					prot = newmopnode ((symb == S_INPUT) ? S_ASINPUT : S_ASOUTPUT, NOPOSN, prot, S_CHAN);
					nextsymb ();
				}
				t = newtypenode (S_ANONCHANTYPE, locn, NULL, prot);
				if (extra_bits) {
					/* this indicates something SHARED at one end only */
#if 0
fprintf (stderr, "syn2: created S_ANONCHANTYPE node, extra_bits = 0x%8.8x\n", (unsigned int)extra_bits);
#endif
					SetTypeAttr (t, TypeAttrOf (t) | extra_bits);
				}
				/* if shared, bung this in the TypeAttr of the channel (will get pushed into the protocol later) */
				if (is_shared) {
					SetTypeAttr (t, TypeAttrOf (t) | TypeAttr_shared);
				}

				realspec = rspecnameandbody (t, locn, indent, TRUE);
#if 0
fprintf (stderr, "syn2: parsing anonymous CT ?, realspec =");
printtreenl (stderr, 4, realspec);
#endif
				/* fixup for anonymous channel-type */
			} else if (symb == S_ANYCHANTYPE) {
				/*{{{  parsing "SHARED MOBILE.CHAN[?!]" declaration*/
				treenode *t, *type;
				INT32 bits;

				type = newtypenode (S_MOBILE, locn, NULL, NULL);
				bits = TypeAttr_shared;
				t = newleafnode (S_ANYCHANTYPE, locn);
				SetLeafLink (t, type);

				nextsymb ();
				if (symb == S_INPUT) {
					bits |= TypeAttr_marked_in;
					nextsymb ();
				} else if (symb == S_OUTPUT) {
					bits |= TypeAttr_marked_out;
					nextsymb ();
				}
				SetTypeAttr (type, bits);

#if 0
fprintf (stderr, "syn2: got SHARED ANYCHANTYPE, about to rspecnameandbody().  t = ");
printtreenl (stderr, 4, t);
#endif
				realspec = rspecnameandbody (t, locn, indent, TRUE);
				/*}}}*/
			} else if (symb == S_NAME) {
				/*{{{  parsing shared channel-type declaration */
#if 0
fprintf (stderr, "rspecification: found S_SHARED, calling rspecification() again\n");
#endif
				realspec = rspecification ();
#if 0
fprintf (stderr, "rspecification: found S_SHARED on realspec = ");
printtreenl (stderr, 4, realspec);
#endif
				if (realspec) {
					treenode *name = DNameOf (realspec);

					if (TagOf (name) == S_LIST) {
						int first = 1;

						while (!EndOfList (name)) {
							treenode *thisone = ThisItem (name);
							treenode *thistype = NTypeOf (thisone);

#if 0
fprintf (stderr, "rspecification: trolling declaration declname (in list) %p = ", thisone);
printtreenl (stderr, 4, thisone);
fprintf (stderr, "rspecification: trolling declaration declname (in list) type %p = ", thistype);
printtreenl (stderr, 4, thistype);
#endif
							if ((TagOf (thistype) == S_ASINPUT) || (TagOf (thistype) == S_ASOUTPUT)) {
								if (!first) {
									treenode *newtype = newmopnode (TagOf (thistype), LocnOf (thistype), copytree (OpOf (thistype), syn_lexlevel), MOpTypeOf (thistype));

									SetNType (thisone, newtype);
#if 0
fprintf (stderr, "rspecification: copied type declaration from %p to %p: ", thistype, newtype);
printtreenl (stderr, 4, newtype);
#endif
									thistype = newtype;
								}
								SetOpTypeAttr (thistype, OpTypeAttrOf (thistype) | TypeAttr_shared);
							}
							name = NextItem (name);
							first = 0;
						}
					} else {
#if 0
fprintf (stderr, "rspecification: trolling declaration declname = ");
printtreenl (stderr, 4, name);
#endif
						if ((TagOf (NTypeOf (name)) == S_ASINPUT) || (TagOf (NTypeOf (name)) == S_ASOUTPUT)) {
							SetOpTypeAttr (NTypeOf (name), OpTypeAttrOf (NTypeOf (name)) | TypeAttr_shared);
						}
					}
				}
				/*}}}*/
			} else {
				synerr_e (SYN_E_SPEC, flocn, symb);
				goto error;
			}
#if 0
fprintf (stderr, "rspecification: returning realspec =");
printtreenl (stderr, 4, realspec);
#endif
			if (fixedflag) {
				set_user_fixed (realspec);
			}

			return realspec;
		}
		/*}}}*/
#endif	/* MOBILES */
	default:
#if 0
fprintf (stderr, "rspecification: SYN_E_SPEC, got %s\n", tagstring(symb));
#endif
		synerr_e (SYN_E_SPEC, locn, symb);
error:
		nextline ();
error2:
		skiplines (indent);
		return NULL;
	}
}

/*}}}*/
/*{{{  PUBLIC BOOL rleadingspecs(specroot, exp)*/
/* Reads a specifications before a selection, alternative or choice
* return the first expression found.
* specs is left pointing to the root of the spec list.
* readerror is set true if error reading expression
*/
PUBLIC BOOL rleadingspecs (treenode ** specroot, treenode ** exp, const BOOL exprlist_permitted)
{
	treenode *s, **lasts = specroot;
	const int indent = symbindent;
	DEBUG_MSG (("rleadingspecs... "));
#if 0
fprintf (stderr, "rleadingspecs...\n");
#endif
	while (TRUE)
		switch (symb) {
			/*{{{  S_INCLUDE S_SC S_USE S_IMPORT */
		case S_INCLUDE:
		case S_SC:
		case S_USE:
		case S_IMPORT:
			if (!rfile ()) {
				skiplines (indent);
				if (lineindent < indent) {
					*exp = NULL;
					*lasts = NULL;
					return FALSE;
				}
			}
			break;
			/*}}} */
			/*{{{  primitive type S_NAME S_LBOX type = specification or expression */
		case S_NAME:
		case S_LBOX:
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
				BOOL specflag = FALSE;	/* Assume no leading specification */
				const BOOL fn_permitted = ((current_fe_data->fe_lang & FE_LANG_NDL) == 0);

#if 0
fprintf (stderr, "rleadingspecs: about to rspecorexpr()\n");
#endif
				s = rspecorexpr (&specflag, -1, exprlist_permitted, fn_permitted);
				if (specflag) {
					*lasts = s;
					if (*lasts != NULL) {
						lasts = DBodyAddr (*lasts);
					}
				} else {	/* exp or error reading exp */
					*lasts = NULL;
					*exp = s;
					return (s == NULL);
				}
				break;
			}
			/*}}} */
		default:
			if (mustbespec (symb)) {
				/*{{{  read spec; break */
				*lasts = rspecification ();
				if (*lasts != NULL) {
					lasts = DBodyAddr (*lasts);
				}
				break;	/* out of switch() */
				/*}}} */
			}
			if (mustbeexp (symb)) {
				/*{{{  read expression; return */
				*lasts = NULL;
				*exp = rexp ();
				return (*exp == NULL);
				/*}}} */
			}
			DEBUG_MSG (("rleadingspecs: returning FALSE... "));
			*lasts = NULL;
			*exp = NULL;
			return FALSE;
		}
}

/*}}}*/
