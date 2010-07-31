/* $Id: chk4.c,v 1.3 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	Occam two checker - driver for scoping and semantic analysis
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

/* #define DEBUG  */

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <string.h>
#include "feinc.h"
#include "lexdef.h"		/* init_predefs_lexer */
#include "chkerror.h"
#include "syndef.h"		/* rpredef */
#include "chk1def.h"
#include "chk2def.h"
#include "chk4def.h"
#include "chkdef.h"
#include "objlib.h"		/* Used for checking validity of library entries */
#include "mobiles.h"		/* for mobile_getanontype() */
#include "trandef.h"		/* for identifying mobile types */

#include "predefhd.h"		/* setting up HOSTEDGE for configurer */
/*}}}  */

/*{{{  definitions*/
/*#define NAME_STACK_SIZE 2000*/
/*#define UNDEC_NAME_STACK_SIZE 50*/

#define DEFAULT_NAMESTACK_SIZE_MAIN  256	/* this is extended when necessary */
#define DEFAULT_NAMESTACK_SIZE_ATTR   32	/* this is extended when necessary */
#define DEFAULT_NAMESTACK_SIZE_UNDEC   1	/* this is extended when necessary */

/*{{{  struct namestackentry*/
typedef struct namestackentry {
	wordnode *wordptr;
	treenode *namedesc;
	/*struct namestackentry *n_next; */
} namestackentry_t;

typedef int scopestack_t;

typedef struct {
	namestackentry_t *base;
	namestackentry_t *top;
	scopestack_t count;
	int max;
} namestack_t;

/*}}}  */
/*}}}  */

/*{{{  PRIVATE variables*/
PRIVATE namestack_t namestack_main;
PRIVATE namestack_t namestack_undeclared;

PRIVATE BOOL insidepripar, nestedpripar;
/*PRIVATE BOOL insideinline;*//* no longer necessary to distinguish */
PRIVATE BOOL insideseparatelycompiledsc = FALSE;
PRIVATE BOOL nestedtimer;	/* There is a TIMER inside this routine */
PRIVATE BOOL nestedplace;	/* There is a PLACE AT inside this routine */
PRIVATE BOOL nestedport;	/* There is a PORT inside this routine */
PRIVATE scopestack_t label_scope;	/* used for testing for duplicate label names */


PRIVATE int configcheckstate;
PRIVATE int configcheckscope;
PRIVATE int configcount;
PRIVATE int networkcount;
PRIVATE int mappingcount;

PRIVATE namestack_t namestack_attributes;


PRIVATE int check_lexlevel;

/* used during fork handling */
PRIVATE treenode *forking_node;
PRIVATE int forking_lexlevel = -1;
/*}}}  */

/*{{{  forward declarations*/
PUBLIC void scopeandcheck (treenode ** tptr);
PRIVATEPARAM int configcheck (treenode * tptr, void *);
/*}}}  */

/*{{{  name stack handling*/
/*{{{  namestack abstraction - realloced array*/
/*{{{  PRIVATE void newnamestackentry()*/
PRIVATE void newnamestackentry (namestack_t * const namestack, treenode * defn, wordnode * name)
{
	if (namestack->count == namestack->max - 1) {
		/*{{{  full stack - realloc double the size */
		const int new_max = namestack->max * 2;
		namestackentry_t *const new = memalloc (new_max * sizeof (namestackentry_t));
		memcpy (new, namestack->base, namestack->max * sizeof (namestackentry_t));
		namestack->top = new + (namestack->top - namestack->base);
		memfree (namestack->base);
		namestack->base = new;
		namestack->max = new_max;
		/*printf("doubling namestack to %d\n", new_max); */
		/*}}}  */
	}

	namestack->top++;
	namestack->top->wordptr = name;
	namestack->top->namedesc = defn;
	namestack->count++;
}

/*}}}  */
/*{{{  PRIVATE void delete_namestackentry*/
PRIVATE void delete_namestackentry (namestack_t * const namestack)
{
	namestack->top--;
	namestack->count--;

	return;
}

/*}}}  */

/*{{{  PRIVATE void init_namestack*/
PRIVATE void init_namestack (namestack_t * const namestack, const size_t size)
{
	namestack->max = size;
	namestack->base = (size == 0) ? NULL : memalloc (size * sizeof (namestackentry_t));
	namestack->top = namestack->base;
	namestack->count = 0;

	if (size != 0) {
		namestack->base->namedesc = NULL;
	}
}

/*}}}  */
/*{{{  PRIVATE void walk_namestack*/
PRIVATE void walk_namestack (namestack_t * const namestack, void (*f) (wordnode *, treenode *, void *), void *const voidptr)
{
	int i;

	for (i = namestack->count; i > 0; i--) {
		namestackentry_t *const nmptr = &namestack->base[i];
		f (nmptr->wordptr, nmptr->namedesc, voidptr);
	}
}

/*}}}  */
/*{{{  PRIVATE treenode *searchnamestack (n)*/
PRIVATE treenode *searchnamestack (register namestack_t * const namestack, register wordnode * const n)
{
	register namestackentry_t *t = namestack->top;

	/* we MUST ensure that it's on the stack somewhere */
	namestack->base->wordptr = n;	/* its namedesc is already NULL */

	/* NOTE - this loop is very speed critical. It is called thousands
	   of times! */
	while (t->wordptr != n) {
		t--;
	}
	return t->namedesc;
}

/*}}}  */
/*}}}  */

/*{{{  PRIVATE BOOL is_typed_name (wordnode *n)*/
/*
 *	returns TRUE if the name is decorated with a type-hash
 *	(generated by external code)
 */
PRIVATE BOOL is_typed_name (wordnode *n)
{
	int i;
	const char *name = WNameOf (n);

	for (i=0; (i<WLengthOf (n)) && (name[i] != ':'); i++);
	if ((i < WLengthOf (n)) && (name[i+1] == '#')) {
		return TRUE;
	}
	return FALSE;
}
/*}}}*/
/*{{{  PUBLIC treenode *findname (wordnode *n)*/
PUBLIC treenode *findname (wordnode *n)
{
	treenode *nptr;

	/* Look up the name pointed to by n on the name stack.
	   Return NULL if not found */
	if (is_typed_name (n)) {
		wordnode *tmpname = lookupword (WNameOf (n), WLengthOf (n) - 10);

		nptr = searchnamestack (&namestack_main, tmpname);
		if (nptr) {
			const char *exthashb = WNameOf (n) + (WLengthOf (n) - 8);
			unsigned int thishash = (unsigned int)typehash_names (nptr);
			const char *hextab = "0123456789ABCDEF";
			int i;

			for (i=7; i>=0; i--) {
				if (exthashb[i] != hextab[(thishash & 0x0f)]) {
					break;
				}
				thishash >>= 4;
			}
			if (i >= 0) {
				/* different! */
#if 0
fprintf (stderr, "chk4: findname(): parameter hashes differ! [%s] vs [%8.8X].  found nptr =", exthashb, (unsigned int)typehash_names (nptr));
printtreenl (stderr, 4, nptr);
#endif
				chkreport_ss (CHK_EXTERN_TYPE_MISMATCH, chklocn, WNameOf (tmpname), exthashb);
			}
#if 0
fprintf (stderr, "chk4: findname(): was typed, tmpname = [%s]. typehash(nptr) = 0x%8.8x, nptr = ", WNameOf (tmpname), nptr ? typehash (nptr) : 0);
printtreenl (stderr, 4, nptr);
#endif
		}
	} else {
		nptr = searchnamestack (&namestack_main, n);
	}

	return nptr;
}
/*}}}  */
/*{{{  PRIVATEPARAM void addname (nptr)*/
PRIVATEPARAM void addname (treenode * const nptr, void *const voidptr)
{
	USE_VAR (voidptr);	/* remove unused variable warning */

#if 0
fprintf (stderr, "chk4: addname: nptr = ");
printtreenl (stderr, 4, nptr);
#endif
	/*{{{  (optionally) warn about descoping names */
	{
		const BOOL param = ((TagOf (nptr) == N_PARAM) || (TagOf (nptr) == N_VALPARAM) || (TagOf (nptr) == N_RESULTPARAM));

		if (((param && current_fe_data->fe_warning_descoped_p && !insideseparatelycompiledsc)
					|| (!param && current_fe_data->fe_warning_descoped_n))
					&& (findname (NNameOf (nptr)) != NULL)) {
			msg_out_s (SEV_WARN, CHK, CHK_NAME_IS_DESCOPED, LocnOf (nptr), WNameOf (NNameOf (nptr)));
		}
	}
	/*}}}  */

	SetNScope (nptr, namestack_main.count);
	newnamestackentry (&namestack_main, nptr, NNameOf (nptr));

	DEBUG_MSG (("addname: %s, ptr is %x, namep is %d\n", WNameOf (namestack_main.top->wordptr), (unsigned int)namestack_main.top, namestack_main.count));
}

/*}}}  */
/*{{{  PRIVATE treenode *addundeclaredname (name)*/
/* Pointer to node containing text of name */
PRIVATE treenode *addundeclaredname (wordnode * const name)
{
	/*treenode *const nptr = declname(N_DECL, chklocn, name, undeclaredp, NULL); */
	treenode *const nptr = newnamenode (N_DECL, chklocn, name, undeclaredp, NULL, 0, 0, NM_DEFAULT);

	newnamestackentry (&namestack_undeclared, nptr, name);

	return nptr;
}

/*}}}  */
/*{{{  PRIVATE treenode *findundeclaredname (n)*/
#if 0
PRIVATE treenode *findundeclaredname (wordnode * n)
{
	/* Look up the name pointed to by n on the name stack.
	   Return NULL if not found */
	return searchnamestack (&namestack_undeclared, n);
}
#else
#define findundeclaredname(n) searchnamestack(&namestack_undeclared, (n))
#endif
/*}}}  */
/*{{{  PUBLIC treenode *lookupname (wordnode *n, treenode *tptr)*/
PUBLIC treenode *lookupname (wordnode *n, treenode *tptr)
{
	/* Look up the name pointed to by n on the name stack.
	   If found, return a pointer to the name record,
	   otherwise make new entry. */
	/* tptr is the namenode returned from findname previously */
	if (tptr == NULL) {
		/*{{{  check to see if this undeclared variable has already been seen */
		const SOURCEPOSN locn = err_sourcefileof (chklocn);
		treenode *old;

		tptr = findundeclaredname (n);
		if (tptr == NULL) {
			/*{{{  Never been seen before, create an undeclared name node */
			tptr = addundeclaredname (n);
			chkerr_s (CHK_NAME_NOT_DECLARED, locn, WNameOf (n));
			/*}}}  */
		}
		old = NDeclOf (tptr);

		/* This test added 16/10/90 so we don't repeat previous lines */
		if ((old == NULL) || (LocnOf (old) != locn)) {	/* add to list of undeclared instances */
			treenode *dptr = newlistnode (S_LIST, locn, NULL, old);
			SetNDecl (tptr, dptr);
		}
		/*}}}  */
	}
	return (tptr);
}

/*}}}  */
/*{{{  PRIVATE void printlinenums (file, listptr)*/
PRIVATE void printlinenums (FILE * file, treenode * listptr)
{
	int current_file = (-1);
	while (listptr != NULL) {
		if (current_file != (-1)) {
			fputc (',', file);
		}
		if (FileNumOf (LocnOf (listptr)) != current_file) {
			current_file = FileNumOf (LocnOf (listptr));
			fprintf (file, " \"%s\" line%s ",
				 fe_lookupfilename (current_fe_handle, current_file),
				 ((NextItem (listptr) != NULL) && (FileNumOf (LocnOf (NextItem (listptr))) == current_file)) ? "s" : "");
		}
		fprintf (file, "%d", FileLineOf (LocnOf (listptr)));
		listptr = NextItem (listptr);
	}
}

/*}}}  */
/*{{{  PRIVATE void sub_printundeclarednames*/
PRIVATE void sub_printundeclarednames (wordnode * const name, treenode * const nptr, void *const voidptr)
{
	if (TagOf (NTypeOf (nptr)) == S_UNDECLARED) {
		FILE *const file = voidptr;

		reverselist (NDeclAddr (nptr));
		fprintf (file, "%s undeclared on", WNameOf (name));
		printlinenums (file, NDeclOf (nptr));
		fputc ('\n', file);
	}
}

/*}}}  */
/*{{{  PUBLIC void printundeclarednames*/
PUBLIC void printundeclarednames (FILE * const file)
{
	walk_namestack (&namestack_undeclared, sub_printundeclarednames, file);
}

/*}}}  */
/*{{{  PUBLIC int inscope (nptr)                   NEVER USED*/
#if 0				/* inscope is never used */
/* Return TRUE if the symbol nptr is in scope, FALSE otherwise */
PUBLIC int inscope (treenode * nptr)
{
	return (nptr == findname (NNameOf (nptr)));
}
#endif /* inscope is never used */

/*}}}  */

/*{{{  NODE attribute handling (Configurer/NDL reader)*/
PRIVATE const int controlport_attrs[] = { PD_ATTR_CONTROL, PD_ATTR_DATA };
PRIVATE const int route_attrs[] = { PD_ATTR_OUTWARD, PD_ATTR_RETURN };
/*{{{  PRIVATE BOOL in_attribute_list*/
PRIVATE BOOL in_attribute_list (const int attr, const int attrs[], const int n)
{
	int i;

	for (i = 0; i < n; i++) {
		if (attr == attrs[i]) {
			return TRUE;
		}
	}

	return FALSE;
}

/*}}}  */

/*{{{  PRIVATE treenode *find_attribute (n)*/
#if 0
PRIVATE treenode *find_attribute (wordnode * n)
{
	/* Look up the name pointed to by n on the name stack.
	   Return NULL if not found */

	return searchnamestack (&namestack_attributes, n);
}
#else
#define find_attribute(n) searchnamestack(&namestack_attributes, (n))
#endif
/*}}}  */
/*{{{  PRIVATE void add_attribute*/
PRIVATE void add_attribute (treenode * nptr)
{
	newnamestackentry (&namestack_attributes, nptr, NNameOf (nptr));
}

/*}}}  */
/*{{{  PRIVATE init_attributes*/
PRIVATE void init_attributes (void)
{
	/* prepare the attributes for S_NODE */
	init_namestack (&namestack_attributes, ((current_fe_data->fe_lang & FE_LANG_OCCAM) != 0) ? 0 : DEFAULT_NAMESTACK_SIZE_ATTR);
}

/*}}}  */

/*}}}  */

/*{{{  PRIVATE int current_scope()*/
#if 0				/* make it a macro instead */
PRIVATE BIT32 current_scope (void)
{
	return namestack_main.count;
}
#else
#define current_scope() namestack_main.count
#endif
/*}}}  */
/*{{{  PRIVATE scopestack_t markscopenames()*/
#if 0				/* make it a macro instead */
PRIVATE scopestack_t markscopenames (void)
{
	DEBUG_MSG (
		   ("markscopenames: namestack is %x (%s), namep is %d\n", namestack_main.top, WNameOf (namestack_main.top->wordptr),
		    namestack_main.count));
	return namestack_main.count;
}
#else
#define markscopenames() namestack_main.count
#endif
/*}}}  */
/*{{{  PRIVATE void chk_descopename*/
PRIVATE void chk_descopename (treenode *nptr)
{
	if (!NUsedOf (nptr) && ((NTypeOf (nptr) == NULL) || TagOf (NTypeOf (nptr)) != S_UNDECLARED)) {
		char *namestring = NULL;
		const SOURCEPOSN locn = LocnOf (nptr);

		if ((WTagOf (NNameOf (nptr)) == S_ASINPUT) || (WTagOf (NNameOf (nptr)) == S_ASOUTPUT)) {
			/* should just have a name behind here, but it's an error anyway! */
			msg_out (SEV_ERR, CHK, CHK_UNEXPECTED_DIRSPEC, locn);
			return;
		} else {
			namestring = (char *)WNameOf (NNameOf (nptr));
		}

		switch (TagOf (nptr)) {
			/*{{{  cases */
		default:
			break;
		case N_DECL:
		case N_ABBR:
		case N_RETYPE:
			if (current_fe_data->fe_warning_unused_v) {
				msg_out_s (SEV_WARN, CHK, CHK_NAME_NOT_USED, locn, namestring);
			}
			break;
		case N_PARAM:
		case N_VALPARAM:
		case N_RESULTPARAM:
			if (current_fe_data->fe_warning_unused_p) {
				int suppress = 0;

				/* XXX: Extremely ugly hack.
				 * Something earlier on in the usage checker
				 * sometimes misreports unused arguments for
				 * PROCs imported from libraries. This
				 * suppresses the warning for things that came
				 * from .lib files.
				 */
				if (locn != NOPOSN) {
					const char *fn = fe_lookupfilename (current_fe_handle, FileNumOf (locn));
					if (strstr (fn, ".lib") == (fn + strlen (fn) - 4))
						suppress = 1;
				}

				if (!suppress)
					msg_out_s (SEV_WARN, CHK, CHK_PARAM_NOT_USED, locn, namestring);
			}
			break;
		case N_VALABBR:
		case N_VALRETYPE:
			if ((current_fe_data->fe_warning_unused_v) && !isconst (nptr)) {
				msg_out_s (SEV_WARN, CHK, CHK_NAME_NOT_USED, locn, namestring);
			}
			break;
		case N_INLINEPROCDEF:	/* bug TS/2045 18/01/93 */
		case N_INLINEFUNCDEF:
		case N_PROCDEF:
#ifdef MOBILES
		case N_MPROCDECL:
#endif
		case N_SFUNCDEF:
		case N_LFUNCDEF:
			if ((NLexLevelOf (nptr) != 0) && (current_fe_data->fe_warning_unused_r)) {
				msg_out_s (SEV_WARN, CHK, CHK_ROUTINE_NOT_USED, locn, namestring);
			}
			break;
			/*}}}  */
		}
	}
}

/*}}}  */
/*{{{  PRIVATE void descopenames (scopestack_t n)*/
PRIVATE void descopenames (const scopestack_t n)
{
	DEBUG_MSG (("descopenames: marker is %d\n", n));
	if (namestack_main.count < n) {
		err_abort ("descopenames");
	}
	while (namestack_main.count > n) {
		DEBUG_MSG (("descopenames: removing %x (%s) (%d)\n",
			    (unsigned int)namestack_main.top, WNameOf (namestack_main.top->wordptr), namestack_main.count));
		chk_descopename (namestack_main.top->namedesc);
		delete_namestackentry (&namestack_main);
	}
}

/*}}}  */

/*}}}  */

/*{{{  PRIVATEPARAM int decllab(treenode *tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  decllab is the routine called by the tree walker on each node
 *          when we are declaring GUY code labels.
 *          Don't go into routine bodies, but do go into expressions.
 *
 *****************************************************************************/
/*}}}  */
PRIVATEPARAM int decllab (treenode * tptr, void *voidptr)
{
	/* We do successive specifications in a loop to avoid excessive recursion */
	if (isspecification (tptr)) {
		/* S_LABELDEF will only ever appear as a specification on its own */
		if (TagOf (tptr) == S_LABELDEF) {
			treenode *const nptr = DNameOf (tptr);
			const treenode *const prev = findname (NNameOf (nptr));
			if (prev != NULL && NScopeOf (prev) >= label_scope)	/* bug 1348 21/8/91 */
				chkerr_s (CHK_DUPLICATE_LABEL, LocnOf (tptr), WNameOf (NNameOf (nptr)));
			else
				addname (nptr, NULL);
			return STOP_WALK;	/* body of S_LABELDEF is always NULL */
		}
		tptr = skipspecifications (tptr);	/* skip nested PROCs etc */
		prewalktree (tptr, decllab, voidptr);
		return STOP_WALK;
	} else
		return CONTINUE_WALK;
}

/*}}}  */
/*{{{  PRIVATE void decllabels(treenode *tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  decllabels takes a parse tree and brings into scope all the labels in
 *             that tree. It does not look inside nested routines.
 *
 *****************************************************************************/
/*}}}  */
PRIVATE void decllabels (treenode * tptr)
{
	const scopestack_t saved_label_scope = label_scope;
	label_scope = current_scope ();
	prewalktree (tptr, decllab, NULL);
	label_scope = saved_label_scope;
}

/*}}}  */
/*{{{  PRIVATE void scopeandchecksc (tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  scopeandchecksc applies scoping and type checking to separately
 *                  compiled routine body tptr.
 *
 *****************************************************************************/
/*}}}  */
PRIVATE void scopeandchecksc (treenode ** tptr)
{
	while (*tptr != NULL) {
		treenode *t = *tptr;
		if (TagOf (t) != S_NAME)
			chklocn = LocnOf (t);
		switch (TagOf (t)) {
		default:
			return;
			/*{{{  SEQ PRIPAR          break */
		case S_SEQ:
			tptr = CBodyAddr (t);
			break;
		case S_PRIPAR:
			nestedpripar = TRUE;
			tptr = CBodyAddr (t);
			break;
			/*}}}  */
			/*{{{  OUTPUT, INPUT       break */
		case S_OUTPUT:
		case S_INPUT:
			tptr = LHSAddr (t);
			break;
			/*}}}  */
			/*{{{  NAME                return */
		case S_NAME:
			{
				wordnode *n = (wordnode *) t;
				*tptr = lookupname (n, findname (n));
			}
			return;
			/*}}}  */
			/*{{{  LIST                break */
		case S_LIST:
			scopeandchecksc (ThisItemAddr (t));
			tptr = NextItemAddr (t);
			break;
			/*}}}  */
		}
	}
}

/*}}}  */

/*{{{  PRIVATE treenode *cset(tptr)*/
/*****************************************************************************
 *
 *  cset performs semantic checking on a set node
 *
 *****************************************************************************/
PRIVATE treenode *cset (treenode * volatile tptr)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;
	DEBUG_MSG (("cset\n"));
	memcpy ((char *) savedenv, (char *) env, sizeof (env));

	if (setjmp (env) != 0) {
		tptr = NULL;
	} else {
		int set_type;	/* type of the SET expression */
		chklocn = LocnOf (tptr);

		/*{{{  check that it is a NODE/ROUTE */
		{
			treenode *t = typecheck (STDevOf (tptr), unknownnodeptr);
			if (!sametype (TagOf (t), S_NODE) && !sametype (TagOf (t), S_ROUTE))
				chkreport_ss (CHK_INVTYPE_CONFIG, chklocn, "SET", "NODE");
			set_type = TagOf (t);
		}
		/*}}}  */
		/*{{{  check the attribute settings */
		{
#if 1
			/* we're basically doing a checkasslist here, but we can't call
			   typecheck_main on lhs cos fields on their own are disallowed */
			int varno = 1;
			treenode *lhs = STAttrNameOf (tptr);
			treenode *rhs = STAttrExpOf (tptr);

			while (!EndOfList (lhs) && !EndOfList (rhs)) {
				/*{{{  check one attribute */
				treenode *type;
				treenode *attr;
				/*{{{  get type of lhs */
				{
					chk_permit_lonely_fields = TRUE;
#if 0				/* This only allows simple names here */
/*
					{
						wordnode *n = (wordnode *) ThisItem (lhs);
						NewItem (lookupname (n, find_attribute (n)), lhs);
						attr = ThisItem (lhs);
						if (TagOf (attr) != N_FIELD)
							chkreport_s (CHK_INVTYPE_ATTR, chklocn, WNameOf (NNameOf (attr)));
						type = NTypeOf (attr);
					}
*/
#else /* !0 */
					{
						wordnode *n;
						treenode **t = ThisItemAddr (lhs);

						/*{{{  loop through subscripts */
						while (TagOf (*t) == S_ARRAYSUB)
							t = ASBaseAddr (*t);
						/*}}}  */
						/*{{{  scope the attribute etc */
						if (TagOf (*t) != S_NAME)
							chkreport (CHK_ILLEGAL_CONFIG_CONSTRUCT, chklocn);
						n = (wordnode *) (*t);
						*t = lookupname (n, find_attribute (n));

						attr = *t;
						if (TagOf (attr) != N_FIELD)
							chkreport_s (CHK_INVTYPE_ATTR, chklocn, WNameOf (NNameOf (attr)));

						scopeandcheck (ThisItemAddr (lhs));
						chklocn = LocnOf (tptr);	/* somehow this gets hit by scopeandcheck */

						/*}}}  */

					}
					type = typecheck (ThisItem (lhs), unknownnodeptr);
#endif	/* !0 */
					chk_permit_lonely_fields = FALSE;
				}
				/*}}}  */
				/*{{{  check rhs is same type as lhs */
				{
					if (!typesequivalent (type, typecheck (ThisItem (rhs), type), FALSE)) {
						char buf[20];	/* enough for a decimal number */
						sprintf (buf, "%d", varno);
						chkreport_s (CHK_INVTYPE_MULTI_ASS, chklocn, buf);
					} else if (set_type != S_UNDECLARED) {
						/* Some attributes are only permitted for ROUTEs,
						   and vice-versa. Hence route_attr must be TRUE iff
						   the type is S_ROUTE */
						const BOOL route_attr = in_attribute_list (NModeOf (attr), route_attrs,
											   sizeof (route_attrs) / sizeof (route_attrs[0]));
						if (route_attr != sametype (set_type, S_ROUTE))
							chkerr_ss (CHK_FIELD_NOT_VALID, LocnOf (tptr),
								   WNameOf (NNameOf (attr)), tagstring (set_type));
					}

					/*{{{  you may not SET a link, etc */
					switch (TagOf (basetype_tree (type))) {
					case S_INT:
					case S_BYTE:
					case S_BOOL:
					case S_NODE:
					case S_ARC:
						break;
					default:
						chkreport_s (CHK_FIELD_NOT_SET, chklocn, WNameOf (NNameOf (attr)));
						break;
					}
					/*}}}  */

				}
				/*}}}  */

				lhs = NextItem (lhs);
				rhs = NextItem (rhs);
				varno++;
				/*}}}  */
			}

			if (!EndOfList (lhs)) {
				chkreport (CHK_TOO_MANY_VARS, chklocn);
			} else if (!EndOfList (rhs)) {
				chkreport (CHK_TOO_FEW_VARS, chklocn);
			}
#else	/* !1 */
			checkasslist (STAttrNameOf (tptr), STAttrExpOf (tptr));
#endif	/* !1 */
		}
		/*}}}  */
		/*{{{  fold the expressions */
		SetSTDev (tptr, foldexp (STDevOf (tptr)));
		if (STAttrExpOf (tptr) != NULL)
			SetSTAttrExp (tptr, foldexplist (STAttrExpOf (tptr)));
		/*}}}  */
	}

	memcpy ((char *) env, (char *) savedenv, sizeof (env));
	DEBUG_MSG (("cset finished\n"));
	chk_permit_lonely_fields = FALSE;	/* incase we aborted via an error */
	return (tptr);
}

/*}}}  */
/*{{{  PRIVATE treenode *cconnect(tptr)*/
/*****************************************************************************
 *
 *  cconnect performs semantic checking on a connect node
 *
 *****************************************************************************/
PRIVATE treenode *cconnect (treenode * volatile tptr)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;
	DEBUG_MSG (("cconnect\n"));
	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0) {
		tptr = NULL;
	} else {
		treenode *t;
		chklocn = LocnOf (tptr);

		/*{{{  check the two EDGEs */
		t = typecheck (ConnectFromEdgeOf (tptr), edgenodeptr);
		if (!sametype (TagOf (t), S_EDGE))
			chkreport_ss (CHK_INVTYPE_CONFIG, chklocn, "CONNECT", "EDGE");
		SetConnectFromEdge (tptr, foldexp (ConnectFromEdgeOf (tptr)));

		t = typecheck (ConnectToEdgeOf (tptr), edgenodeptr);
		if (!sametype (TagOf (t), S_EDGE))
			chkreport_ss (CHK_INVTYPE_CONFIG, chklocn, "CONNECT", "EDGE");
		SetConnectToEdge (tptr, foldexp (ConnectToEdgeOf (tptr)));
		/*}}}  */

		if (ConnectArcOf (tptr) != NULL) {
			/*{{{  check the ARC */
			t = typecheck (ConnectArcOf (tptr), arcnodeptr);
			if (!sametype (TagOf (t), S_ARC)) {
				chkreport_ss (CHK_INVTYPE_CONFIG, chklocn, "WITH", "ARC");
			}
			SetConnectArc (tptr, foldexp (ConnectArcOf (tptr)));
			/*}}}  */
		}
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));
	DEBUG_MSG (("cconnect finished\n"));

	return (tptr);
}

/*}}}  */
/*{{{  PRIVATE treenode *cmap(tptr)*/
/*****************************************************************************
 *
 *  cmap performs semantic checking on a map node
 *
 *****************************************************************************/
PRIVATE treenode *cmap (treenode * volatile tptr)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;
	DEBUG_MSG (("cmap\n"));
	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0)
		tptr = NULL;
	else {
		treenode *t, *maplist;
		treenode *sourcetype = NULL;	/* initialised to shut up gcc's optimiser */
		chklocn = LocnOf (tptr);

		if ((current_fe_data->fe_lang & FE_LANG_NDL) != 0) {
			/*{{{  Checks for NDL MAP statement */
			/*{{{  check the destination of the mapping */
			t = typecheck (MapDestOf (tptr), nodenodeptr);
			if (!sametype (TagOf (t), S_EDGE)) {
				chkreport (CHK_INV_RHS_NDL_MAPPING, chklocn);
			}
			SetMapDest (tptr, foldexp (MapDestOf (tptr)));
			/*}}}  */
			/*{{{  check the source */
			maplist = MapSourceOf (tptr);
			if (!EndOfList (NextItem (maplist))) {
				chkreport (CHK_NDL_MAPPING_TOO_MANY_SOURCES, chklocn);
			} else {
				t = typecheck (ThisItem (maplist), edgenodeptr);
				if (!sametype (TagOf (t), TagOf (edgenodeptr))) {
					chkreport (CHK_INV_LHS_NDL_MAPPING, chklocn);
				}
				NewItem (foldexp (ThisItem (maplist)), maplist);
			}
			/*}}}  */
			/*}}}  */
		} else {
			/*{{{  Checks for configurer MAP statement */
			/*{{{  check the destination of the mapping */
			t = typecheck (MapDestOf (tptr), nodenodeptr);
			if (sametype (TagOf (t), S_NODE)) {
				sourcetype = nodenodeptr;
			} else if (sametype (TagOf (t), S_ARC)) {
				sourcetype = channodeptr;
			} else {
				chkreport (CHK_INV_MAPPING_RHS, chklocn);
			}
			SetMapDest (tptr, foldexp (MapDestOf (tptr)));
			/*}}}  */
			/*{{{  check the sources */
			for (maplist = MapSourceOf (tptr); !EndOfList (maplist); maplist = NextItem (maplist)) {
				t = typecheck (ThisItem (maplist), sourcetype);
				if (!sametype (TagOf (t), TagOf (sourcetype))) {
					chkreport_s (CHK_INV_MAPPING_LHS, chklocn, tagstring (TagOf (sourcetype)));
				}
				NewItem (foldexp (ThisItem (maplist)), maplist);
			}
			/*}}}  */
			/*{{{  check the priority */
			if (MapPriOf (tptr) != NULL) {
				if (TagOf (sourcetype) != S_NODE) {
					chkreport (CHK_INV_MAPPING_NOPRI, chklocn);
				}
				t = typecheck (MapPriOf (tptr), intnodeptr);
				if (!sametype (TagOf (t), S_INT)) {
					chkreport (CHK_INV_MAPPING_PRI, chklocn);
				}
				SetMapPri (tptr, foldexp (MapPriOf (tptr)));
			}
			/*}}}  */
			/*}}}  */
		}
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));
	DEBUG_MSG (("cmap finished\n"));

	return (tptr);
}

/*}}}  */

/*{{{  PRIVATE void lookup_special_namespace*/
PRIVATE void lookup_special_namespace (treenode ** tptr, treenode * const type_tree)
{
	wordnode *const name = (wordnode *) * tptr;
	treenode *nptr;
	switch (TagOf (type_tree)) {
		/*{{{  NODE attributes */
	case S_NODE:
	case S_CONTROLPORT:
		nptr = lookupname (name, find_attribute (name));
		if (TagOf (nptr) == N_FIELD)
			/*{{{  check validity of that attribute */
		{
			/* Some attributes are only permitted for CONTROLPORT,
			   and vice-versa. Hence controlport_attr must be TRUE iff
			   type is S_CONTROLPORT */
			const BOOL controlport_attr = in_attribute_list (NModeOf (nptr), controlport_attrs,
									 sizeof (controlport_attrs) / sizeof (controlport_attrs[0]));
			if (controlport_attr != (TagOf (type_tree) == S_CONTROLPORT))
				chkerr_ss (CHK_FIELD_NOT_VALID, LocnOf (*tptr), WNameOf (NNameOf (nptr)), tagstring (TagOf (type_tree)));
		}
		/*}}}  */
		*tptr = nptr;
		break;
		/*}}}  */
		/*{{{  fields of RECORDs */
#ifdef OCCAM2_5
	case S_RECORD:
		/*{{{  look for a field name */
		{
			treenode *t;
			for (nptr = NULL, t = ARTypeOf (type_tree); nptr == NULL && t != NULL; t = DBodyOf (t))
				if (NNameOf (DNameOf (t)) == name)
					nptr = DNameOf (t);
		}
		/*}}}  */

		if (nptr == NULL)
			scopeandcheck (tptr);
		else
			*tptr = nptr;
		break;
#endif
		/*}}}  */
		/*{{{  fields of tagged PROTOCOLs */
#ifdef OCCAM2_5
	case N_TPROTDEF:
		/*{{{  look for a field of a tagged protocol */
		{
			treenode *t;
			for (nptr = NULL, t = NTypeOf (type_tree); nptr == NULL && !EndOfList (t); t = NextItem (t))
				if (NNameOf (ThisItem (t)) == name)
					nptr = ThisItem (t);
		}
		/*}}}  */
		if (nptr != NULL)
			*tptr = nptr;
		break;
#endif
		/*}}}  */
	default:
		break;
	}
}

/*}}}  */

/*{{{  PRIVATE treenode *csubscript*/
/*****************************************************************************
 *
 *  csubscript turns array subscripting into record subscripting if necessary
 *
 *****************************************************************************/
PRIVATE treenode *csubscript (treenode * const tptr)
{
#ifdef OCCAM2_5
	if (TRUE)
#else
	if ((current_fe_data->fe_lang & FE_LANG_OCCAM) == 0)	/* Allow records */
#endif
	{
		treenode *const type_tree = typecheck (ASBaseOf (tptr), unknownnodeptr);
		treenode *sub_tree = follow_user_type (type_tree);

#ifdef MOBILES
		/* if sub_tree is a MOBILE type, follow it */
		if (TagOf (sub_tree) == S_MOBILE) {
			sub_tree = follow_user_type (MTypeOf (sub_tree));
		}
#endif

		/*{{{  debugging */
#ifdef DEBUG
		DEBUG_MSG (("csubscript: tree of Base is"));
		printtree (stdout, 0, ASBaseOf (tptr));
		DEBUG_MSG (("\ncsubscript: sub type tree of Base is"));
		printtree (stdout, 0, sub_tree);
		DEBUG_MSG (("\n"));
#endif
		/*}}}  */
		switch (TagOf (sub_tree)) {
			/*{{{  some sort of record type */
		case S_NODE:
		case S_CONTROLPORT:
#ifdef OCCAM2_5
		case S_RECORD:
#endif
			if (TagOf (ASIndexOf (tptr)) == S_NAME)
				lookup_special_namespace (ASIndexAddr (tptr), sub_tree);
			else
				scopeandcheck (ASIndexAddr (tptr));

			if (TagOf (ASIndexOf (tptr)) != N_FIELD)
				/*{{{  error */
			{
				chkerr_s (CHK_RSUB_NOT_FIELD, LocnOf (tptr), (nodetypeoftag (TagOf (type_tree)) == NAMENODE)
					  ? WNameOf (NNameOf (type_tree)) : tagstring (TagOf (type_tree)));
			}
			/*}}}  */

			SetTag (tptr, S_RECORDSUB);
			break;
			/*}}}  */
		default:
			scopeandcheck (ASIndexAddr (tptr));
			break;
		}
	} else {
		scopeandcheck (ASIndexAddr (tptr));
	}
#ifdef DEBUG
	DEBUG_MSG (("csubscript: tree after processing is"));
	printtree (stdout, 0, tptr);
	DEBUG_MSG (("\n"));
#endif
	return (tptr);
}
/*}}}*/
/*{{{  PRIVATE treenode *csubscriptarrayconstructor (treenode *tptr)*/
/*	this is used to handle subscripted array-constructors
 *	we turn:
 *	    [i = s FOR l STEP t | exp][j]
 *	into:
 *	    VAL INT i IS ((j * t) + s):
 *	    VALOF
 *	      IF
 *	        j < l
 *	          SKIP
 *	      RESULT exp
 *	the compiler has been known to optimise away constants though,
 *	thus checks in here to make sure "0 <= j < l"
 */
PRIVATE treenode *csubscriptarrayconstructor (treenode *tptr)
{
	int old = switch_to_real_workspace ();
	SOURCEPOSN tlocn = LocnOf (tptr);
	treenode *acnode = ASBaseOf (tptr);
	treenode *idxnode = ASIndexOf (tptr);
	treenode *lengthnode = ReplCLengthExpOf (acnode);
	treenode *rctemp = ReplCTempOf (acnode);	/* two temporaries, want 2nd one */
	treenode *s_decl, *n_decl;			/* of the VAL INT */
	treenode *enode, *cnode, *inode, *valofnode;

#if 0
fprintf (stderr, "csubscriptarrayconstructor: tptr =");
printtreenl (stderr, 4, tptr);
#endif
	n_decl = ReplCNameOf (acnode);
	s_decl = NDeclOf (ThisItem (NextItem (rctemp)));
	SetNDecl (n_decl, s_decl);
	SetTag (n_decl, N_VALABBR);
	/* SetNName (n_decl, NNameOf (ReplCNameOf (acnode))); */
	/* s_decl = NDeclOf (n_decl); */

	SetDName (s_decl, n_decl);
	SetDVal (s_decl, newdopnode (S_ADD, tlocn, (!ReplCStepExpOf (acnode) ?
		copytree (idxnode, syn_lexlevel) :
		newdopnode (S_TIMES, tlocn, copytree (idxnode, syn_lexlevel), ReplCStepExpOf (acnode), S_INT)), ReplCStartExpOf (acnode), S_INT));
#if 0
fprintf (stderr, "csubscriptarrayconstructor: DValOf (s_decl) = ");
printtreenl (stderr, 4, DValOf (s_decl));
#endif

	SetASIndex (tptr, NULL);
	SetReplCStartExp (acnode, NULL);
	SetReplCLengthExp (acnode, NULL);
	SetReplCStepExp (acnode, NULL);

	scopeandcheck (&lengthnode);
	scopeandcheck (&idxnode);
	/* build check expression */
	if (isconst (lengthnode) && isconst (idxnode)) {
		int idxval, lengthval;

		/* typecheck lengthnode and idxnode for INT-ness */
		if (!typecheck (lengthnode, intnodeptr)) {
			chkerr (CHK_ADIM_NOT_INT, tlocn);
		} else if (!typecheck (idxnode, intnodeptr)) {
			chkerr (CHK_ASUB_NOT_INT, tlocn);
		}

		idxval = (int)eval_const_treenode (idxnode);
		lengthval = (int)eval_const_treenode (lengthnode);

		if ((idxval < 0) || (idxval >= lengthval)) {
			chkerr (CHK_SUBSCRIPT_OUT_OF_RANGE, tlocn);
		}
		inode = newleafnode (S_SKIP, tlocn);
	} else {
		enode = newdopnode (S_LS, tlocn, idxnode, lengthnode, S_INT);
		cnode = newcondnode (S_CHOICE, tlocn, enode, newleafnode (S_SKIP, tlocn));
		inode = newcnode (S_IF, tlocn, newlistnode (S_LIST, tlocn, cnode, NULL));
	}

	valofnode = newvalofnode (S_VALOF, tlocn, inode, newlistnode (S_LIST, tlocn, ReplCBodyOf (acnode), NULL));

	SetDBody (s_decl, valofnode);
	SetReplCBody (acnode, NULL);

	switch_to_prev_workspace (old);
#if 0
fprintf (stderr, "csubscriptarrayconstructor: returning expression =");
printtreenl (stderr, 4, s_decl);
#endif
	return s_decl;
}
/*}}}  */
/*{{{  PRIVATE treenode *csegmentarrayconstructor (treenode *tptr)*/
/*	mangles segments of array constructors into something useful:
 *	    [[i = s0 FOR l0 STEP t0 | e(i)] FOR l1]
 *	into:
 *	    [i = s0 FOR (l0 - l1) STEP t0 | e(i)]
 *
 *	    [[i = s0 FOR l0 STEP t0 | e(i)] FROM s1]
 *	into:
 *	    [i = ((s1 * t0) + s0) FOR (l0 - s1) STEP t0 | e(i)]
 *
 *	    [[i = s0 FOR l0 STEP t0 | e(i)] FROM s1 FOR l1]
 *	into:
 *	    [i = ((s1 * t0) + s0) FOR l1 STEP t0 | e(i)]
 */
PRIVATE treenode *csegmentarrayconstructor (treenode *tptr)
{
	SOURCEPOSN locn = LocnOf (tptr);
	treenode *acnode = SNameOf (tptr);
	treenode *seg_start = SStartExpOf (tptr);
	treenode *seg_length = SLengthExpOf (tptr);
	const int old = switch_to_real_workspace ();

	if (!seg_start && !seg_length) {
		/* yuk, shouldn't ever get this, but ignore if do, will bail out later */
		SetSName (tptr, ReplCBodyOf (acnode));
		switch_to_prev_workspace (old);
		return tptr;
	} else if (!seg_start) {
		treenode *newlength = newdopnode (S_SUBTRACT, locn, ReplCLengthExpOf (acnode), seg_length, S_INT);

		SetReplCLengthExp (acnode, newlength);
	} else if (!seg_length) {
		treenode *newstart = newdopnode (S_ADD, locn, ReplCStartExpOf (acnode), (ReplCStepExpOf (acnode) ?
			newdopnode (S_MULT, locn, copytree (ReplCStepExpOf (acnode), syn_lexlevel), seg_start, S_INT) :
			seg_start), S_INT);
		treenode *newlength = newdopnode (S_SUBTRACT, locn, ReplCLengthExpOf (acnode), copytree (seg_start, syn_lexlevel), S_INT);

#if 0
fprintf (stderr, "csegmentarrayconstructor (1): newstart = ");
printtreenl (stderr, 4, newstart);
#endif
		SetReplCStartExp (acnode, newstart);
		SetReplCLengthExp (acnode, newlength);
	} else {
		treenode *newstart = newdopnode (S_ADD, locn, ReplCStartExpOf (acnode), (ReplCStepExpOf (acnode) ?
			newdopnode (S_MULT, locn, copytree (ReplCStepExpOf (acnode), syn_lexlevel), seg_start, S_INT) :
			seg_start), S_INT);
/*		treenode *newlength = newdopnode (S_SUBTRACT, locn, ReplCLengthExpOf (acnode), copytree (seg_length, syn_lexlevel), S_INT); */

#if 0
fprintf (stderr, "csegmentarrayconstructor (2): newstart = ");
printtreenl (stderr, 4, newstart);
#endif
		SetReplCStartExp (acnode, newstart);
/*		SetReplCLengthExp (acnode, newlength); */
		SetReplCLengthExp (acnode, copytree (seg_length, syn_lexlevel));
	}
	switch_to_prev_workspace (old);
#if 0
fprintf (stderr, "csegmentarrayconstructor (returning): acnode =");
printtreenl(stderr, 4, acnode);
#endif
	return acnode;
}
/*}}}  */
/*{{{  PRIVATE treenode *cpragma*/
PRIVATE treenode *cpragma (treenode * volatile tptr, treenode * last_decl, treenode * current_params)
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp'
*/
{
	jmp_buf savedenv;
	memcpy ((char *) savedenv, (char *) env, sizeof (env));
	if (setjmp (env) != 0)
		tptr = NULL;
	else {
		switch ((pragma_name_tag_t) NModeOf (DNameOf (tptr))) {
			/*{{{  NOUSAGECHECK/NOTALIASED */
		case pragma_name_shared:
		case pragma_name_aliased:
		case pragma_name_assumeconst:
			{
				treenode *l;
				for (l = DValOf (tptr); !EndOfList (l); l = NextItem (l)) {
					treenode *const pname = ThisItem (l);
					switch (TagOf (pname)) {
					case N_DECL:
					case N_PARAM:
					case N_VALPARAM:
					case N_RESULTPARAM:
					case N_ABBR:
					case N_VALABBR:
					case N_RETYPE:
					case N_VALRETYPE:
						check_adjacent_name (LocnOf (tptr), pname, last_decl, current_params, SEV_ERR);
						break;
					default:
						chkreport_ss (CHK_BAD_PRAGMA_SHARED_NAME, chklocn,
							      WNameOf (NNameOf (DNameOf (tptr))), WNameOf (NNameOf (pname)));
						break;
					}
					switch ((pragma_name_tag_t) NModeOf (DNameOf (tptr))) {
					case pragma_name_shared:
						SetNVShared (pname, TRUE);
						break;
					case pragma_name_aliased:
						SetNVAliased (pname, TRUE);
						SetNVShared (pname, TRUE);	/* set shared for notaliased too! */
						break;
					case pragma_name_assumeconst:
						SetNVAssumeConst (pname, TRUE);
						break;
					default:
						break;
					}
				}
			}
			break;
			/*}}}  */
			/*{{{  NESTEDTIMER/NESTEDPLACE/NESTEDPORT/BADLYBEHAVED */
		case pragma_name_nestedtimer:
		case pragma_name_nestedplace:
		case pragma_name_nestedport:
		case pragma_name_badlybehaved:
			{
				treenode *const pname = DValOf (tptr);
				if (nametypeoftag (TagOf (pname)) != NAMENODE_ROUTINE)
					chkreport_ss (CHK_BAD_PRAGMA_SHARED_NAME, chklocn,
						      WNameOf (NNameOf (DNameOf (tptr))), WNameOf (NNameOf (pname)));
				check_adjacent_name (LocnOf (tptr), pname, last_decl, NULL, SEV_ERR);
				switch (NModeOf (DNameOf (tptr))) {
				case pragma_name_nestedtimer:
					SetNPNestedTimer (pname, TRUE);
					break;
				case pragma_name_nestedplace:
					SetNPNestedPlace (pname, TRUE);
					break;
				case pragma_name_nestedport:
					SetNPNestedPort (pname, TRUE);
					break;
				case pragma_name_badlybehaved:
					SetNPNestedTimer (pname, TRUE);
					SetNPNestedPlace (pname, TRUE);
					SetNPNestedPort (pname, TRUE);
					break;
				default:
					break;
				}
			}
			break;
			/*}}}  */
			/*{{{  DYNCALL, EXPORT*/
		case pragma_name_dyncall:
		case pragma_name_export:
			{
				treenode *l;

				for (l = DValOf (tptr); !EndOfList (l); l = NextItem (l)) {
					treenode *pname = ThisItem (l);

					switch (TagOf (pname)) {
					case N_PROCDEF:
					case N_LFUNCDEF:
					case N_SFUNCDEF:
					case N_LIBPROCDEF:
					case N_LIBFUNCDEF:
						break;
					default:
						if (nodetypeoftag (TagOf (pname)) == NAMENODE) {
							chkreport_s (CHK_BAD_DYNCALL_NAMEDTYPE, chklocn, WNameOf (NNameOf (pname)));
						} else {
							chkreport (CHK_BAD_DYNCALL_TYPE, chklocn);
						}
						break;
					}
				}
			}
			break;
			/*}}}*/
			/*{{{  FMTYPES*/
		case pragma_name_fmtypes:
			{
				treenode *l;

				for (l = DValOf (tptr); !EndOfList (l); l = NextItem (l)) {
					treenode *pitem = ThisItem (l);

					switch (TagOf (pitem)) {
					case N_TYPEDECL:
						break;
					default:
						if (nodetypeoftag (TagOf (pitem)) == NAMENODE) {
							chkreport_s (CHK_BAD_FMTYPES_NAMEDTYPE, chklocn, WNameOf (NNameOf (pitem)));
						} else {
							chkreport (CHK_BAD_FMTYPES_TYPE, chklocn);
						}
						break;
					}
				}
			}
			break;
			/*}}}*/
			/*{{{  everything else */
		case pragma_name_linkage:
		case pragma_name_translate:
		case pragma_name_external:
		case pragma_name_dexternal:
		case pragma_name_comment:
		case pragma_name_hardware:
		case pragma_name_defined:
		case pragma_name_undefined:
		case pragma_name_iospace:
		case pragma_name_formalmodel:
			break;
			/*}}}  */
		}
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));
	return tptr;
}

/*}}}  */
/*{{{  PRIVATE void scope_predefs*/
PRIVATE void scope_predefs (void)
{
	BOOL more_predefs = TRUE;
	DEBUG_MSG (("scope_predefs\n"));
	init_predefs_lexer ();
	while (more_predefs) {
		treenode *const nptr = rpredef ();
		if (nptr == NULL) {
			more_predefs = FALSE;
		} else {
			DEBUG_MSG (("scope_predefs: %s\n", WNameOf (NNameOf (nptr))));
#if 0				/* use the normal list, rather than have individual entries */
			switch (NModeOf (nptr)) {
			case PD_HOSTEDGE:
				fe_set_predefname (current_fe_handle, nptr, fe_predefname_HOST_EDGE);
				break;
			case PD_DEFAULTNODE:
				fe_set_predefname (current_fe_handle, nptr, fe_predefname_DEFAULT_NODE);
				break;
			default:
				break;
			}
#endif

			if (TagOf (nptr) == N_FIELD) {
				add_attribute (nptr);	/* not globally scoped */
			} else {
				addname (nptr, NULL);	/* Declare the predefined name to the scoper */

/* Add to the list of visible predefines *//* bug TS/1625 16/03/92 */
				fe_set_predefname (current_fe_handle,
						   addtofront (nptr, fe_get_predefname (current_fe_handle, fe_predefname_LIST)), fe_predefname_LIST);
			}
		}
	}
}

/*}}}  */
/*{{{  PRIVATE void fixup_anonchantype_end (treenode **ptptr)*/
/*
 *	fixes up an anonymous channel-type parameter, turning it into the
 *	appropriate server or client end
 */
PRIVATE void fixup_anonchantype_end (treenode **ptptr)
{
	int marked_attr;
	treenode *tptr = *ptptr;

	if (nodetypeoftag (TagOf (tptr)) != NAMENODE) {
		msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, NOPOSN, "fixup_anonchantype_end(): not namenode");
		return;
	}
	marked_attr = TypeAttrOf (NTypeOf (tptr)) & (TypeAttr_marked_in | TypeAttr_marked_out);

#if 0
fprintf (stderr, "fixup_anonchantype_end(): param of S_ANONCHANTYPE.  marked_attr = %d, TypeAttrOf (...) = %d, NTypeOf (tptr) =", marked_attr, TypeAttrOf (NTypeOf (tptr)));
printtreenl (stderr, 4, NTypeOf (tptr));
#endif

	scopeandcheck (NTypeAddr (tptr));

#if 0
fprintf (stderr, "fixup_anonchantype_end(): after scopeandcheck on NTypeOf param, =");
printtreenl (stderr, 4, NTypeOf (tptr));
#endif

	if ((TagOf (ProtocolOf (NTypeOf (tptr))) == N_DECL) && (TagOf (NTypeOf (ProtocolOf (NTypeOf (tptr)))) == S_UNDECLARED)) {
		/* undeclared protocol, just reduce to CHAN */
		SetTag (NTypeOf (tptr), S_CHAN);
	} else {
		wordnode *endname;
		treenode *endvar, *chandecl, *chanvar;
		treenode *anontype, *iotype;
		treenode *protocol = ProtocolOf (NTypeOf (tptr));

		anontype = mobile_getanontype (protocol, check_lexlevel);
		scopeandcheck (&anontype);

#if 0
fprintf (stderr, "scopeprocorfunc: S_ANONCHANTYPE of something.  mobile_getanontype() returned:");
printtreenl (stderr, 4, anontype);
if (anontype && (TagOf (anontype) == N_TYPEDECL)) {
fprintf (stderr, "scopeprocorfunc: S_ANONCHANTYPE of something.  NTypeOf (anontype) =");
printtreenl (stderr, 4, NTypeOf (anontype));
}
#endif
		if (marked_attr & TypeAttr_marked_in) {
			/* create a server end */
			endname = mobile_getanonname_svr (NNameOf (tptr));
			iotype = newmopnode (S_ASINPUT, NOPOSN, anontype, S_CHAN);
		} else if (marked_attr & TypeAttr_marked_out) {
			/* create a client end */
			endname = mobile_getanonname_cli (NNameOf (tptr));
			iotype = newmopnode (S_ASOUTPUT, NOPOSN, anontype, S_CHAN);
		} else {
			/* error -- must specify channel direction */
			chkerr_s (CHK_NO_CHANDIR_SPEC, LocnOf (tptr), WNameOf (NNameOf (tptr)));
			iotype = NULL;
			endname = NULL;
		}
		if (iotype) {
			SetOpTypeAttr (iotype, TypeAttr_shared);

			endvar = newnamenode (N_PARAM, NOPOSN, endname, iotype, NULL, check_lexlevel, 0, NM_DEFAULT);
			chandecl = newdeclnode (S_DECL, NOPOSN, tptr, NULL, NULL);
			SetNDecl (endvar, NDeclOf (tptr));
			SetNUsed (endvar, TRUE);

			chanvar = tptr;
			*ptptr = endvar;
			SetNDecl (chanvar, chandecl);
			/* connect parameter to channel declaration node */
			SetDBody (chandecl, endvar);
			addname (chanvar, NULL);
#if 0
fprintf (stderr, "scopeprocorfunc: S_ANONCHANTYPE mangled.  tptr now reads:");
printtreenl (stderr, 4, tptr);
fprintf (stderr, "scopeproforfunc: S_ANONCHANTYPE, original channel declaration now:");
printtreenl (stderr, 4, chandecl);
#endif
		}	/* else errored */
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void cparamdecltype (treenode *tptr)*/
/*
 *	checks that the given specifier tree is valid for parameters
 */
PRIVATE void cparamdecltype (treenode *tptr)
{
	switch (TagOf (tptr)) {
	case N_SPROTDEF:
	case N_TPROTDEF:
		chkreport (CHK_PROTOCOL_AS_PARAMTYPE, chklocn);
		break;
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void scopeprocorfunc*/
/*
 *	scopes and checks a PROC or FUNCTION declaration.
 */
PRIVATE treenode *current_proc;
PRIVATE treenode *current_params;

PRIVATE void scopeprocorfunc (treenode *const tptr)
{
	treenode *const name = DNameOf (tptr);
	treenode *params = NParamListOf (name);
	const scopestack_t namestackmarker = markscopenames ();
	const BOOL oldinsidepripar = insidepripar;
	const BOOL oldnestedpripar = nestedpripar;
	const BOOL oldnestedtimer = nestedtimer;
	const BOOL oldnestedplace = nestedplace;
	const BOOL oldnestedport = nestedport;
	/*int oldinsideinline = insideinline; */
	const int old_lexlevel = check_lexlevel;
	treenode *const old_proc = current_proc;


#if 0
fprintf (stderr, "scopeprocorfunc(): here, params are:");
printtreenl (stderr, 4, params);
#endif
	insidepripar = FALSE;
	nestedpripar = FALSE;
	nestedtimer = FALSE;
	nestedplace = FALSE;
	nestedport = FALSE;
	/*insideinline = inline(name); */
	insideseparatelycompiledsc = separatelycompiled (name);
	current_proc = name;
	check_lexlevel++;
	treenode *p;

	/*{{{  scope and check result types */
#ifdef OCCAM2_5
	if ((NTypeOf (name) != NULL) && (TagOf (NTypeOf (name)) == S_FNTYPE)) {	/* its a function */
		treenode *fn_type;
		scopeandcheck (FnTypeListAddr (NTypeOf (name)));
		for (fn_type = FnTypeListOf (NTypeOf (name)); !EndOfList (fn_type); fn_type = NextItem (fn_type))
			ctype (ThisItem (fn_type));
	}
#endif
	/*}}}  */
	/*{{{  if MOBILE PROC, check the IMPLEMENTS*/
	if ((TagOf (tptr) == S_MPROCDECL) && DExtraOf (tptr)) {
		check_lexlevel--;		/* scope at a higher lex-level */
		scopeandcheck (DExtraAddr (tptr));
		check_lexlevel++;
	}
	/*}}}*/

	/*{{{  scope parameters */
	for (p = params; !EndOfList (p); p = NextItem (p)) {
		treenode *thisparam = ThisItem (p);

#ifdef MOBILES
		if (!NTypeOf (thisparam)) {
			chkreport (CHK_PARAM_MISSING_TYPE, chklocn);
		}
		/* capture any dynamic mobile chan-type parameters, which will still say S_ASINPUT/S_ASOUTPUT from the parser */
#if 0
fprintf (stderr, "scopeprocorfunc: checking param = ");
printtreenl (stderr, 4, thisparam);
fprintf (stderr, "scopeprocorfunc: type tree = ");
printtreenl (stderr, 4, NTypeOf (thisparam));
#endif
		if (TagOf (NTypeOf (thisparam)) == S_ANONCHANTYPE) {
			/*{{{  anonymous channel-types*/
			fixup_anonchantype_end (ThisItemAddr (p));
			thisparam = ThisItem (p);
			/*}}}*/
		}
		/* also sort out CHAN of chan-type parameters, which have been specifier-fixed by now */
		if ((TagOf (NTypeOf (thisparam)) == S_CHAN) &&
				((TagOf (ProtocolOf (NTypeOf (thisparam))) == S_ASINPUT) || (TagOf (ProtocolOf (NTypeOf (thisparam))) == S_ASOUTPUT))) {
			const BOOL was_input = (TagOf (ProtocolOf (NTypeOf (thisparam))) == S_ASINPUT);
			const BOOL shared = ((OpTypeAttrOf (ProtocolOf (NTypeOf (thisparam))) & TypeAttr_shared) == TypeAttr_shared);
			treenode *curtype, *typecopy;

			/* temporarily fix type for scoping */
			SetProtocol (NTypeOf (thisparam), OpOf (ProtocolOf (NTypeOf (thisparam))));
			scopeandcheck (NTypeAddr (thisparam));

			if (TagOf (ProtocolOf (NTypeOf (thisparam))) == N_TYPEDECL) {
				const int old = switch_to_real_workspace ();
				/* okay, it's a channel of a type-decl, sort it */
#if 0
fprintf (stderr, "scopeprocorfunc: found CHAN of chan-type, type after scoping is: ");
printtreenl (stderr, 4, NTypeOf (thisparam));
#endif
				curtype = ProtocolOf (NTypeOf (thisparam));
				typecopy = newnamenode (TagOf (curtype), LocnOf (curtype), NNameOf (curtype), NTypeOf (curtype),
						NDeclOf (curtype), NLexLevelOf (curtype), NScopeOf (curtype), NModeOf (curtype));
				SetNTypeAttr (typecopy, (was_input ? TypeAttr_marked_in : TypeAttr_marked_out) | (shared ? TypeAttr_shared : 0));
				SetProtocol (NTypeOf (thisparam), typecopy);
				switch_to_prev_workspace (old);
			}
		} else if ((TagOf (NTypeOf (thisparam)) == S_ASINPUT) || (TagOf (NTypeOf (thisparam)) == S_ASOUTPUT)) {
			/* which way ? */
			const BOOL was_input = (TagOf (NTypeOf (thisparam)) == S_ASINPUT);
			/* shared ? */
			const BOOL shared = ((OpTypeAttrOf (NTypeOf (thisparam)) & TypeAttr_shared) == TypeAttr_shared);
			/* const int old = switch_to_real_workspace (); */
			treenode *curtype, *typecopy;

			/* temporarily fix type for scoping */
			SetNType (thisparam, OpOf (NTypeOf (thisparam)));
			scopeandcheck (NTypeAddr (thisparam));
			if (TagOf (NTypeOf (thisparam)) == N_TYPEDECL) {
				/* okay, it's a typedecl, sort it */
				const int old = switch_to_real_workspace ();
				curtype = NTypeOf (thisparam);
				typecopy = newnamenode (TagOf (curtype), LocnOf (curtype), NNameOf (curtype), NTypeOf (curtype),
						NDeclOf (curtype), NLexLevelOf (curtype), NScopeOf (curtype), NModeOf (curtype));

				SetNTypeAttr (typecopy, (was_input ? TypeAttr_marked_in : TypeAttr_marked_out) | (shared ? TypeAttr_shared : 0));
				SetNType (thisparam, typecopy);
				switch_to_prev_workspace (old);
			}
			/* switch_to_prev_workspace (old); */
		} else {
			scopeandcheck (NTypeAddr (thisparam));
		}
#if 0
fprintf (stderr, "scopeprocorfunc: new param = ");
printtreenl (stderr, 4, thisparam);
fprintf (stderr, "   scopeprocorfunc: type-tree = ");
printtreenl (stderr, 4, NTypeOf (thisparam));
#endif

#else	/* !MOBILES */
		scopeandcheck (NTypeAddr (thisparam));
#endif	/* !MOBILES */
		cparamdecltype (NTypeOf (thisparam));
	}
	/*}}}  */
	params = (treenode *) cparmlist (params, TagOf (tptr), LocnOf (tptr), insideseparatelycompiledsc);
	walklist (addname, params, NULL);	/* Add parameters  */
	SetNParamList (name, params);
	current_params = params;

	if (*current_fe_data->fe_guyinserts != 0) {
		/* Bring guy labels into scope */
		decllabels (DValOf (tptr));
	}
#if 0
fprintf (stderr, "scopeprocorfunc: separatelycompiled(name) = %d, name =", separatelycompiled (name));
printtreenl (stderr, 4, name);
#endif
	if (separatelycompiled (name)) {
		/*{{{  mark parameters as used, scopeandcheck the body */
		for (; !EndOfList (params); params = NextItem (params)) {
			SetNUsed (ThisItem (params), TRUE);
		}
		scopeandchecksc (DValAddr (tptr));
		/*}}}  */
	} else {
		if (NPRecursiveOf (name)) {
			/* bring name into scope.. */
			addname (name, NULL);
		}
		scopeandcheck (DValAddr (tptr));	/* Check procedure/function */
	}
	cdeclaration (tptr);
	/*{{{  Nested properties such as PRI PARs */
	if (nestedpripar)
		SetNPNestedPriPar (name, TRUE);
	if (nestedtimer)
		SetNPNestedTimer (name, TRUE);
	if (nestedplace)
		SetNPNestedPlace (name, TRUE);
	if (nestedport)
		SetNPNestedPort (name, TRUE);
#if 0
	if (nestedtimer)
		printf ("scopeprocorfunc: %s has a nestedtimer\n", WNameOf (NNameOf (name)));
	if (nestedplace)
		printf ("scopeprocorfunc: %s has a nestedplace\n", WNameOf (NNameOf (name)));
	if (nestedport)
		printf ("scopeprocorfunc: %s has a nestedport\n", WNameOf (NNameOf (name)));
#endif
	/*}}}  */
	current_proc = old_proc;
	current_params = NULL;
	check_lexlevel = old_lexlevel;
	insidepripar = oldinsidepripar;
	nestedpripar = oldnestedpripar;
	/*insideinline = oldinsideinline; */
	nestedtimer = oldnestedtimer;
	nestedplace = oldnestedplace;
	nestedport = oldnestedport;
	insideseparatelycompiledsc = FALSE;
	descopenames (namestackmarker);	/* Descope names   */
}

/*}}}  */
/*{{{  PRIVATE void permit_hwname*/
PRIVATE void permit_hwname (treenode * const tptr, const int tag, const int fe_tag)
{
	if ((current_fe_data->fe_lang & FE_LANG_CONFIG3) != 0) {
		treenode *const bname = nameof (tptr);	/* skip thru subscripts */
		wordnode *const wname = (wordnode *) bname;
		treenode *const tname = findname (wname);

		if ((tname == NULL) && (findundeclaredname (wname) == NULL)) {	/* not in scope */
			treenode *const nptr = addundeclaredname (wname);
			treenode *type = newleafnode (tag, NOPOSN);
			treenode *bptr;
			for (bptr = tptr; TagOf (bptr) == S_ARRAYSUB; bptr = ASBaseOf (bptr))
				type = newtypenode (S_ARRAY, NOPOSN, NULL, type);
			SetNType (nptr, type);
#ifdef DEBUG
			DEBUG_MSG (("permit_hwname: creating %s, tptr is", WNameOf (wname)));
			printtree (stdout, 0, tptr);
			DEBUG_MSG (("\npermit_hwname: type is"));
			printtree (stdout, 0, type);
			DEBUG_MSG (("\n"));
#endif

			fe_set_predefname (current_fe_handle, addtofront (nptr, fe_get_predefname (current_fe_handle, fe_tag)), fe_tag);
		}
	}
}

/*}}}  */
/*{{{  PRIVATE void scope_offsetof*/
#ifdef OCCAM2_5
PRIVATE void scope_offsetof (treenode * const tptr)
{
	treenode *type;
	scopeandcheck (LeftOpAddr (tptr));
	type = follow_user_type (LeftOpOf (tptr));
	if (TagOf (type) == S_RECORD) {
		lookup_special_namespace (RightOpAddr (tptr), type);
		if (TagOf (RightOpOf (tptr)) != N_FIELD)
			chkerr_ss (CHK_OFFSETOF_NOT_FIELD, LocnOf (tptr), WNameOf (NNameOf (RightOpOf (tptr))), WNameOf (NNameOf (LeftOpOf (tptr))));
	} else
		chkerr_s (CHK_OFFSETOF_NOT_RECORD, LocnOf (tptr), WNameOf (NNameOf (LeftOpOf (tptr))));
}
#endif
/*}}}  */
/*{{{  PRIVATE void scope_communication*/
#ifdef OCCAM2_5
PRIVATE void scope_communication (treenode * const tptr)
/* This routine scopes the tag values for tagged protocols.
   It leaves anything it doesn't recognise alone, so that
   'scopeandcheck' can be run afterwards.
*/
{
	treenode *const channel = LHSOf (tptr);
	treenode *const type_tree = typecheck (channel, unknownnodeptr);
	if ((TagOf (type_tree) == S_CHAN) && (TagOf (ProtocolOf (type_tree)) == N_TPROTDEF)) {
		treenode *const rhs = RHSOf (tptr);
		switch (TagOf (tptr)) {
			/*{{{  output/ tagged input */
		case S_OUTPUT:
		case S_TAGGED_INPUT:
		case S_X_TAGGED_INPUT:
			if (!EndOfList (rhs) && (TagOf (ThisItem (rhs)) == S_NAME)) {
				lookup_special_namespace (ThisItemAddr (rhs), ProtocolOf (type_tree));
			}
			break;
			/*}}}  */
			/*{{{  case input */
		case S_CASE_INPUT:
		case S_X_CASE_INPUT:
			{
				treenode *variantlist;
				for (variantlist = rhs; !EndOfList (variantlist); variantlist = NextItem (variantlist)) {
					treenode *const variant = skipspecifications (ThisItem (variantlist));
					if (variant != NULL) {
						treenode *const this_variant = VRTaggedListOf (variant);
						if (!EndOfList (this_variant) && (TagOf (ThisItem (this_variant)) == S_NAME))
							lookup_special_namespace (ThisItemAddr (this_variant), ProtocolOf (type_tree));
					}
				}
			}
			break;
			/*}}}  */
		}
	}
}
#endif
/*}}}  */

/*{{{  PRIVATE void check_nested_properties_place*/
PRIVATE void check_nested_properties_place (treenode * tptr)
/* bug TS/1983 17/12/92 */
{
	treenode *type = NTypeOf (DNameOf (tptr));
	while (TagOf (type) == S_ARRAY)
		type = ARTypeOf (type);

	if ((TagOf (type) == S_PORT) || (TagOf (type) == S_CHAN))
		nestedport = TRUE;

	nestedplace = TRUE;
}

/*}}}  */
/*{{{  PRIVATE void check_nested_properties_decl*/
PRIVATE void check_nested_properties_decl (treenode * tptr)
/* bug TS/1983 17/12/92 */
{
	treenode *name = DNameOf (tptr);
	if (TagOf (name) == S_LIST)
		name = ThisItem (name);
	if (basetype (NTypeOf (name)) == S_TIMER)
		nestedtimer = TRUE;
}
/*}}}  */
/*{{{  PRIVATE treenode *reldop_list_transform (treenode *opnode, treenode *l_list, treenode *r_list, int conn_op) */
PRIVATE treenode *reldop_list_transform (treenode *opnode, treenode *l_list, treenode *r_list, int conn_op)
{
	treenode *newtree = NULL;

	while (!EndOfList (l_list) && !EndOfList (r_list)) {
		treenode *tmp;

		tmp = newdopnode (TagOf (opnode), LocnOf (opnode), ThisItem (l_list), ThisItem (r_list), 0);
		if (newtree) {
			newtree = newdopnode (S_AND, LocnOf (opnode), newtree, tmp, 0);
		} else {
			newtree = tmp;
		}
		l_list = NextItem (l_list);
		r_list = NextItem (r_list);
	}
	if (!EndOfList (l_list) || !EndOfList (r_list)) {
		chkerr (CHK_BAD_LIST_LENGTHS, chklocn);
	}
	return newtree;
}
/*}}}  */
/*{{{  PRIVATE void caltskip (treenode *guard_list, int symb) */
/***********************************************************************************
 *
 *  cprialtskip checks that no SKIP guards occur in a PRI ALT, except as the last
 *
 ***********************************************************************************/
PRIVATE void caltskip (treenode *guard_list, int symb)
{
	if (TagOf (guard_list) == S_LIST) {
		treenode *list = guard_list;

		while (!EmptyList (list)) {
			if ((TagOf (ThisItem (list)) == S_ALTERNATIVE) && (TagOf (AltInputOf (ThisItem (list))) == S_SKIP)) {
				treenode *const guard = AltInputOf (ThisItem (list));
				const BOOL true_guard = ((AltGuardOf (ThisItem (list)) == NULL) ||
					(TagOf (AltGuardOf (ThisItem (list))) == S_TRUE));
				const BOOL null_guard = (AltGuardOf (ThisItem (list)) == NULL);

				if ((symb == S_ALT) && true_guard) {
					/* any SKIP guard in an ALT is complained about (!) */
					if (strict_checking) {
						chkerr (CHK_SKIP_IN_ALT, LocnOf (guard));
					} else {
						chkwarn (CHK_SKIP_IN_ALT, LocnOf (guard));
					}
				} else if (null_guard && (!IsLastItem (list) || (symb != S_PRIALT))) {
					if (strict_checking) {
						chkerr (CHK_NO_SKIP_PRECOND, LocnOf (guard));
					} else {
						chkwarn (CHK_NO_SKIP_PRECOND, LocnOf (guard));
					}
				}
				if (true_guard && !IsLastItem (list) && (symb == S_PRIALT)) {
					if (strict_checking) {
						chkerr (CHK_SKIP_NOT_LAST_PRI, LocnOf (guard));
					} else {
						chkwarn (CHK_SKIP_NOT_LAST_PRI, LocnOf (guard));
					}
				}
			}
			list = NextItem (list);
		}
	}
}
/*}}}  */
#ifdef MOBILES
/*{{{  PRIVATE void fold_type_direction (treenode *nptr)*/
PRIVATE void fold_type_direction (treenode *nptr)
{
	switch (TagOf (nptr)) {
	case N_DECL:
	case N_PARAM:
	case N_ABBR:
	case N_RESULTPARAM:
		{

			/* names have been scoped by this point (obviously N_...) */
#if 0
			treenode *type = NTypeOf (nptr);
fprintf (stderr, "chk4: fold_type_direction: type = ");
printtreenl (stderr, 4, type);
#endif
		}
		break;
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC BOOL ismobileprocvar (treenode *nptr)*/
/*
 *	returns TRUE if nptr is a MOBILE process variable
 */
PUBLIC BOOL ismobileprocvar (treenode *nptr)
{
	if (!nptr) {
		return FALSE;
	}
	if (nodetypeoftag (TagOf (nptr)) != NAMENODE) {
		return FALSE;
	}
	if (NTypeOf (nptr) && (TagOf (NTypeOf (nptr)) == S_MOBILE) && (TagOf (MTypeOf (NTypeOf (nptr))) == N_PROCTYPEDECL)) {
		return TRUE;
	}
	return FALSE;
}
/*}}}*/
#endif
/*{{{  PRIVATE void scopeandcheck*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  scopeandcheck applies scoping and type checking to parse tree
 *
 *****************************************************************************/
/*}}}  */
PUBLIC void scopeandcheck (treenode **tptr)
{
	static BOOL guy_not_asm = FALSE;	/* YES - static but local to this function! */
	if (tptr != NULL) {
		while (*tptr != NULL) {
			treenode *const t = *tptr;
			const int tag = TagOf (t);
			const int nodetype = nodetypeoftag (tag);
			if ((nodetype != WORDNODE) && (nodetype != NAMENODE)) {
				chklocn = LocnOf (t);
			}
			DEBUG_MSG (("scopeandcheck: %s at %d\n", itagstring (tag), (int) chklocn));
#if 0
fprintf (stderr, "scopeandcheck: in: *tptr =");
printtreenl (stderr, 4, *tptr);
#endif
			switch (nodetype) {
			default:
				return;
				/*{{{  DECLNODE */
			case DECLNODE:
				{
					const scopestack_t namestackmarker = markscopenames ();
					treenode **root = tptr;
					treenode *tt = t;
					treenode *last_decl = NULL;
					treenode *saved_forking_node = forking_node;		/* don't want this inside PROC decls */

					for (; (*tptr != NULL) && isspecification (*tptr); tptr = DBodyAddr (tt)) {
						treenode *name;
						
#ifdef MOBILES
						/*{{{  mangle anonymous channel types here -- before we scope the declaration involved */
						if (TagOf (*tptr) == S_DECL) {
							treenode *vname = DNameOf (*tptr);
							treenode *namelist;

							if ((TagOf (vname) == S_ASINPUT) || (TagOf (vname) == S_ASOUTPUT)) {
								vname = OpOf (vname);
							}
							if (TagOf (vname) == S_LIST) {
								/* they'll all share the same type, just pick the first out */
								namelist = vname;
								vname = ThisItem (vname);
							} else {
								namelist = newlistnode (S_LIST, NOPOSN, vname, NULL);
							}
							if (TagOf (NTypeOf (vname)) == S_ANONCHANTYPE) {
								/*{{{  anonymous channel-type*/
								/* first thing we do is scope-and-check the PROTOCOL, no point if it's not valid */
								scopeandcheck (NTypeAddr (vname));
#if 0
fprintf (stderr, "scopeandcheck: S_DECL: S_ANONCHANTYPE, before: *tptr = ");
printtreenl (stderr, 4, *tptr);
fprintf (stderr, "scopeandcheck: S_DECL: S_ANONCHANTYPE, before: namelist = ");
printtreenl (stderr, 4, namelist);
#endif
								if ((TagOf (ProtocolOf (NTypeOf (vname))) == N_DECL) &&
										(TagOf (NTypeOf (ProtocolOf (NTypeOf (vname)))) == S_UNDECLARED)) {
									TagOf (NTypeOf (vname)) = S_CHAN;
								} else {
									/*{{{  get anonymous type, create client+server variables*/
									treenode *anontype, *iotype, *clivar, *svrvar;
									treenode *clidecl, *svrdecl;
									wordnode *cliname, *svrname;
									treenode *clivarlist, *svrvarlist;
									treenode *allocnode, *seqnode, *paramlist, *alloclist;
									treenode **nextalloc;
									treenode *protocol = ProtocolOf (NTypeOf (vname));
									int sbits = TypeAttrOf (NTypeOf (vname)) & (TypeAttr_marked_in | TypeAttr_marked_out);

#if 0
fprintf (stderr, "scopeandcheck: S_DECL: S_ANONCHANTYPE (sbits=0x%8.8x) of:", (unsigned int)sbits);
printtreenl (stderr, 4, protocol);
#endif
									anontype = mobile_getanontype (protocol, check_lexlevel);
									scopeandcheck (&anontype);
#if 0
fprintf (stderr, "scopeandcheck: S_DECL: S_ANONCHANTYPE, mobile_getanontype() returned (after applying NTypeOf ():");
printtreenl (stderr, 4, NTypeOf (anontype));
#endif
									/* process all variables */
									clivarlist = NULL;
									svrvarlist = NULL;
									alloclist = NULL;
									nextalloc = NULL;
									while (!EndOfList (namelist)) {
										treenode *rname = ThisItem (namelist);

#if 0
fprintf (stderr, "scopeandcheck: S_DECL: S_ANONCHANTYPE: processing name, rname =");
printtreenl (stderr, 4, rname);
#endif
										cliname = mobile_getanonname_cli ((TagOf (rname) == S_NAME) ? (wordnode *)rname : NNameOf (rname));
										iotype = newmopnode (S_ASOUTPUT, NOPOSN, anontype, S_CHAN);
										if (!sbits || (sbits & TypeAttr_marked_out)) {
											SetOpTypeAttr (iotype, TypeAttr_shared);
										}
										clivar = newnamenode (N_DECL, NOPOSN, cliname, iotype, NULL, check_lexlevel, 0, NM_DEFAULT);
										clivarlist = newlistnode (S_LIST, NOPOSN, clivar, clivarlist);

										svrname = mobile_getanonname_svr ((TagOf (rname) == S_NAME) ? (wordnode *)rname : NNameOf (rname));
										iotype = newmopnode (S_ASINPUT, NOPOSN, anontype, S_CHAN);
										if (!sbits || (sbits & TypeAttr_marked_in)) {
											SetOpTypeAttr (iotype, TypeAttr_shared);
										}
										svrvar = newnamenode (N_DECL, NOPOSN, svrname, iotype, NULL, check_lexlevel, 0, NM_DEFAULT);
										svrvarlist = newlistnode (S_LIST, NOPOSN, svrvar, svrvarlist);
										/* better create the initialisation */
										paramlist = newlistnode (S_LIST, NOPOSN, (treenode *)cliname,
												newlistnode (S_LIST, NOPOSN, (treenode *)svrname,
												newlistnode (S_LIST, NOPOSN, anontype, NULL)));
										allocnode = newinstancenode (S_PINSTANCE, NOPOSN, (treenode *)lookupword ("ALLOC.CHAN.TYPE",15), paramlist);
										if (!nextalloc && !alloclist) {
											/* hang onto this one, cos it's where we want to attach the existing process */
											alloclist = newlistnode (S_LIST, LocnOf (*tptr), allocnode, alloclist);
											nextalloc = NextItemAddr (alloclist);
										} else {
											alloclist = newlistnode (S_LIST, LocnOf (*tptr), allocnode, alloclist);
										}

										namelist = NextItem (namelist);
									}
									clidecl = newdeclnode (S_DECL, NOPOSN, clivarlist, NULL, NULL);
									svrdecl = newdeclnode (S_DECL, NOPOSN, svrvarlist, NULL, NULL);
									
									while (!EndOfList (clivarlist) && !EndOfList (svrvarlist)) {
										clivar = ThisItem (clivarlist);
										SetNDecl (clivar, clidecl);
										svrvar = ThisItem (svrvarlist);
										SetNDecl (svrvar, svrdecl);

										clivarlist = NextItem (clivarlist);
										svrvarlist = NextItem (svrvarlist);
									}

									seqnode = newcnode (S_SEQ, NOPOSN, alloclist);
									*nextalloc = newlistnode (S_LIST, NOPOSN, DBodyOf (*tptr), NULL);
									SetDBody (svrdecl, seqnode);
									SetDBody (clidecl, svrdecl);
									SetDBody (*tptr, clidecl);
#if 0
fprintf (stderr, "scopeandcheck: S_DECL: S_ANONCHANTYPE, after: *tptr = ");
printtreenl (stderr, 4, *tptr);
#endif
									/*}}}*/
								}
								/*}}}  */
							}
						}
						/*}}}*/
#endif

						tt = *tptr;
						name = DNameOf (tt);
						chklocn = LocnOf (tt);

						switch (TagOf (tt)) {
							/*{{{  ABBR, VALABBR, RETYPE, VALRETYPE, TPROTDEF, SPROTDEF, TYPEDECL */
						case S_ABBR:
						case S_VALABBR:
						case S_RETYPE:
						case S_VALRETYPE:
						case S_SPROTDEF:
							CASE_CONFIG_SPEC
#ifdef OCCAM2_5
						case S_TYPEDECL:
#endif
							{
								BOOL did_scope_name = FALSE;

#ifdef MOBILES
								if (TagOf (tt) == S_TYPEDECL) {
#if 0
fprintf (stderr, "scopeandcheck: S_TYPEDECL: name =");
printtreenl (stderr, 4, name);
fprintf (stderr, "scopeandcheck: S_TYPEDECL: NTypeOf(name) =");
printtreenl (stderr, 4, NTypeOf(name));
fprintf (stderr, "scopeandcheck: S_TYPEDECL: DValOf(tt) =");
printtreenl (stderr, 4, DValOf (tt));
#endif
									if (NTypeOf (name) && (TagOf (NTypeOf (name)) == S_MOBILE) && (TypeAttrOf (NTypeOf (name)) & TypeAttr_recursive)) {
										/* recursive type, so bring name into scope early */
										addname (name, NULL);
										did_scope_name = TRUE;
									}
								}
#endif
								scopeandcheck (NTypeAddr (name));
								scopeandcheck (DValAddr (tt));
								if ((TagOf (tt) == S_TYPEDECL) && NTypeOf (name) && (TagOf (NTypeOf (name)) == S_MOBILE)) {
									/*{{{  check any TRACEs*/
									if (TypeTracesOf (NTypeOf (name))) {
										/* traces will contain field names from the record, scope them temporarily */
										scopestack_t tracesstackmarker = markscopenames ();
										treenode *record = MTypeOf (NTypeOf (name));

										if (TagOf (record) != S_RECORD) {
											chkreport (CHK_TRACES_NOT_RECORD, LocnOf (TypeTracesOf (NTypeOf (name))));
										} else {
											treenode *sl = ARTypeOf (record);
											
#if 0
fprintf (stderr, "chk4: scopeandcheck: TRACES: ARTypeOf (record) = ");
printtreenl (stderr, 4, ARTypeOf (record));
#endif
											while (sl) {
												/* should be a declaration */
												treenode *name = DNameOf (sl);
#if 0
fprintf (stderr, "chk4: scopeandcheck: TRACES: name = ");
printtreenl (stderr, 4, name);
#endif
												addname (name, NULL);
												sl = DBodyOf (sl);
											}
										}
										scopeandcheck (TypeTracesAddr (NTypeOf (name)));
										descopenames (tracesstackmarker);
									}
									/*}}}*/
								}
								if (cdeclaration (tt) != NULL) {
									if (!did_scope_name) {
										addname (name, NULL);
									}
									last_decl = tt;	/* bug TS/1910 26/01/93 */
								}
							}
							break;
							/*}}}  */
							/*{{{  PROCTYPEDECL*/
#ifdef MOBILES
						case S_PROCTYPEDECL:
							{
								BOOL did_scope_name = FALSE;

								if (NTypeAttrOf (name) & TypeAttr_recursive) {
									addname (name, NULL);
									did_scope_name = TRUE;
								}

								/* scope parameters */
								if (NTypeOf (name)) {
									treenode *tl;
									
									for (tl = NTypeOf (name); !EndOfList (tl); tl = NextItem (tl)) {
										treenode **itemp = ThisItemAddr (tl);

										switch (TagOf (*itemp)) {
										case N_PARAM:
										case N_VALPARAM:
										case N_RESULTPARAM:
											if (TagOf (NTypeOf (*itemp)) == S_ANONCHANTYPE) {
												fixup_anonchantype_end (itemp);
											} else {
												scopeandcheck (NTypeAddr (*itemp));
											}
											break;
										default:
											scopeandcheck (itemp);
											break;
										}	
									}
								}
								if (cdeclaration (tt) != NULL) {
									if (!did_scope_name) {
										addname (name, NULL);
									}
									last_decl = tt;
								}
							}
							break;
#endif
							/*}}}*/
							/*{{{  TPROTDEF*/
						case S_TPROTDEF:
							{
								treenode *n;
								treenode **extrap = DExtraAddr (tt);

								/* walk the sub-lists */
								for (n = NTypeOf (name); !EndOfList (n); n = NextItem (n)) {
#ifdef OCCAM2_5
									if (current_fe_data->fe_visible_tags)
										addname (ThisItem (n), NULL);
#else
									addname (ThisItem (n), NULL);
#endif
									scopeandcheck (NTypeAddr (ThisItem (n)));
								}
								/* if there are any extras (protocol inheritance), scope them */
								if (*extrap) {
									scopeandcheck (extrap);
								}
								if ((treenode *) cdeclaration (tt) != (treenode *) NULL) {	/* Scope the protocol name */
									addname (name, NULL);
									last_decl = tt;	/* bug TS/1910 26/01/93 */
								}
							}
							break;
							/*}}}*/
							/*{{{  PROCDEF FUNCDEF MPROCDECL */
						case S_PROCDEF:
						case S_SFUNCDEF:
						case S_LFUNCDEF:
#ifdef MOBILES
						case S_MPROCDECL:
#endif
							/* FORK barrier will be placed automatically for these */
							forking_node = NULL;
							scopeprocorfunc (tt);
							addname (DNameOf (tt), NULL);
							last_decl = tt;	/* bug TS/1910 26/01/93 */
							break;
							/*}}}  */
							/*{{{  DECL */
						case S_DECL:
							{
#if 0
fprintf (stderr, "scopeandcheck: S_DECL: name =");
printtreenl (stderr, 4, name);
fprintf (stderr, "scopeandcheck: NTypeOf (name) =");
printtreenl (stderr, 4, NTypeOf (name));
#endif
#ifdef MOBILES
								if ((TagOf (name) == S_ASINPUT) || (TagOf (name) == S_ASOUTPUT)) {
#if 0
fprintf (stderr, "scopeandcheck: S_DECL: had ASINPUT/ASOUTPUT, adjusting DNameOf.\n");
#endif
									name = OpOf (name);
									SetDName (tt, name);
								}

#endif
#ifdef DECL_EQ
								if (DValOf (tt) == NULL) {
									synerror (TRUE, CHK_MISSING_INIT, LocnOf (tt), 0);
								} else
#endif
								if (TagOf (name) == S_LIST) {
									/* They all share the same type tree so do this once */
									treenode **typeaddr;
									
									typeaddr = NTypeAddr (ThisItem (name));
									scopeandcheck (typeaddr);
									/* if the type is a typedecl, this hasn't scoped any trailing
									   declarations; so we do it directly:
									 */
									for (; !EndOfList (name); name = NextItem (name)) {
										SetNType (ThisItem (name), *typeaddr);
									}
								} else {
									scopeandcheck (NTypeAddr (name));
								}
								if ((treenode *) cdeclaration (tt) != (treenode *) NULL) {	/* Check type and name */
									name = DNameOf (tt);
#ifdef MOBILES
									if ((TagOf (name) == S_ASINPUT) || (TagOf (name) == S_ASOUTPUT)) {
										name = OpOf (name);
									}
#endif
									if (TagOf (name) == S_LIST) {
										treenode *list = name;

										while (!EndOfList (list)) {
											addname (ThisItem (list), NULL);
											list = NextItem (list);
										}
									} else {
										addname (name, NULL);
									}
									check_nested_properties_decl (tt);
									last_decl = tt;	/* bug TS/1910 26/01/93 */
								}
							}
							break;
							/*}}}  */
							/*{{{  PLACE WSPLACE VSPLACE */
						case S_PLACE:
						case S_WSPLACE:
						case S_VSPLACE:
						case S_PLACEON:
							if (TagOf (tt) == S_PLACEON)
								permit_hwname (PlaceExpOf (tt), S_ARC, fe_predefname_HARDWARE_ARC);
							scopeandcheck (DNameAddr (tt));
							scopeandcheck (PlaceExpAddr (tt));
							*tptr = (treenode *) callocation (tt, last_decl);
							if (*tptr != NULL && TagOf (tt) == S_PLACE)	/* PLACE ... AT */
								check_nested_properties_place (*tptr);
							break;
							/*}}}  */
							/*{{{  labeldef */
						case S_LABELDEF:
							/*if (insideinline)
							   chkrecoverreport(CHK_LABEL_INSIDE_INLINE, chklocn, 0); */
							break;
							/*}}}  */
							/*{{{  PRAGMA */
						case S_PRAGMA:	/* bug 829 19/9/91 */
							switch ((pragma_name_tag_t) NModeOf (name)) {
							case pragma_name_shared:
							case pragma_name_aliased:
							case pragma_name_nestedtimer:
							case pragma_name_nestedplace:
							case pragma_name_nestedport:
							case pragma_name_badlybehaved:
							case pragma_name_assumeconst:
							case pragma_name_defined:
							case pragma_name_undefined:
							case pragma_name_iospace:
							case pragma_name_dyncall:
							case pragma_name_export:
							case pragma_name_fmtypes:
								scopeandcheck (DValAddr (tt));
								break;
							case pragma_name_linkage:
							case pragma_name_translate:
							case pragma_name_external:
							case pragma_name_dexternal:
							case pragma_name_comment:
							case pragma_name_hardware:
							case pragma_name_formalmodel:
								break;
							}
							cpragma (tt, last_decl, current_params);
							break;
							/*}}}  */
							/*{{{  FORWDECL*/
						case S_FORWDECL:
							if (TagOf (DNameOf (tt)) == N_TYPEDECL) {
								addname (DNameOf (tt), NULL);
							} else if (TagOf (DNameOf (tt)) == S_NAME) {
								chkerr_s (CHK_MISSING_FORWDECL, chklocn, WNameOf((wordnode *)DNameOf(tt)));
							}
							break;
							/*}}}*/
						default:
							badtag (chklocn, TagOf (tt), "scopeandcheck");
						}
					}
					current_params = NULL;	/* bug TS/1910 26/01/93 */
					forking_node = saved_forking_node;
					if ((*tptr != NULL) && (TagOf (*tptr) == S_VALOF)) {
						/*{{{  do checking */
						/* Scope and check body and result list here so we can include the
						 * preceeding specs in check of valof
						 */
						scopestack_t valofnamestackmarker = markscopenames ();
						treenode *const ttt = *tptr;

						scopeandcheck (VLBodyAddr (ttt));
						descopenames (valofnamestackmarker);	/* Descope names within Valof */
						scopeandcheck (VLResultListAddr (ttt));
						descopenames (namestackmarker);	/* Descope names before Valof */
						*root = (treenode *) cvalof (*root, current_scope ());
						/*}}}  */
					} else {
						scopeandcheck (tptr);
						descopenames (namestackmarker);
					}
					return;
				}
				/*}}}  */
				/*{{{  LEAFNODE  (SUSPEND/SYNC)*/
			case LEAFNODE:
				switch (tag) {
					/*{{{  pre-define scope check for SUSPEND*/
				case S_SUSPEND:
					{
						treenode *pdname = (treenode *)lookupword ("MPBARSYNC", 9);

						scopeandcheck (&pdname);
#if 0
fprintf (stderr, "scopeandcheck (SUSPEND).  pdname = ");
printtreenl (stderr, 4, pdname);
#endif
						if (nodetypeoftag (TagOf (pdname)) == NAMENODE) {
							/* hang onto this */
							SetLeafLink (t, pdname);
						}
					}
					break;
					/*}}}*/
					/*{{{  handling for SYNC*/
				case S_SYNC:
					{
						treenode *bar;

						scopeandcheck (LeafLinkAddr (t));
#if 0
fprintf (stderr, "scopeandcheck (SYNC).  LeafLinkOf (t) = ");
printtreenl (stderr, 4, LeafLinkOf (t));
#endif
						bar = LeafLinkOf (t);
						if (!check_isfullbarrier (bar)) {
							chkreport (CHK_SYNC_NOT_BARRIER, chklocn);
						} else if (check_resignedfullbarrier (bar)) {
							chkreport (CHK_BARRIER_RESIGNED, chklocn);
						}
					}
					break;
					/*}}}*/
				}
				return;
				/*}}}*/
				/*{{{  Constructor */
			case CNODE:
				current_params = NULL;	/* bug TS/1910 26/01/93 */
				if (!listitems (CBodyOf (t))) {
					switch (tag) {
					case S_PAR:
						chkwarn (CHK_EMPTY_PAR, chklocn);
						break;
					case S_SEQ:
						chkwarn (CHK_EMPTY_SEQ, chklocn);
						break;
					case S_IF:
						chkwarn (CHK_EMPTY_IF, chklocn);
						break;
					case S_ALT:
					case S_PRIALT:
						chkwarn (CHK_EMPTY_ALT, chklocn);
						break;
					}
				}
				/* if PAR, check for BARRIER spec */
				if (tag == S_PAR) {
					treenode *bar = CTempOf (t);
					treenode *declbars, *extbars;

					if (bar && (TagOf (bar) == S_BAREXTEND)) {
						/* got both */
						declbars = LeftOpOf (bar);
						extbars = RightOpOf (bar);
					} else if (bar && (TagOf (bar) == S_EXTENDS)) {
						/* got extensions only */
						declbars = NULL;
						extbars = bar;
					} else if (bar) {
						/* must be declarations only */
						declbars = bar;
						extbars = NULL;
					} else {
						declbars = NULL;
						extbars = NULL;
					}

					/* look at extensions first */
					if (extbars && (TagOf (extbars) == S_EXTENDS)) {
						/*{{{  extending a barrier, just scope and check it*/
#if 0
fprintf (stderr, "scopeandcheck: PAR: EXTENDS: extbars = ");
printtreenl (stderr, 4, extbars);
#endif
						scopeandcheck (OpAddr (extbars));
						extbars = OpOf (extbars);

						if (!extbars) {
							chkreport (CHK_EXTENDS_NOT_BARRIER, chklocn);
						} else if (TagOf (extbars) == S_LIST) {
							treenode *walk;

							for (walk=extbars; !EndOfList (walk); walk = RightOf (walk)) {
								treenode *thisbar = ThisItem (walk);

								if (!check_isfullbarrier (thisbar)) {
									chkreport (CHK_EXTENDS_NOT_BARRIER, chklocn);
								} else if (check_resignedfullbarrier (thisbar)) {
									chkreport (CHK_BARRIER_RESIGNED, chklocn);
								}
							}
						} else if (!check_isfullbarrier (extbars)) {
							chkreport (CHK_EXTENDS_NOT_BARRIER, chklocn);
						} else if (check_resignedfullbarrier (extbars)) {
							chkreport (CHK_BARRIER_RESIGNED, chklocn);
						}

						/*}}}*/
					} else if (extbars) {
						/* badness of some kind */
						chkreport (CHK_BARRIER_BAD_EXTENDS, chklocn);
					}

					if (declbars) {
						/*{{{  check barriers*/
						if (TagOf (declbars) == S_LIST) {
							treenode *walk;

							for (walk = declbars; !EndOfList (walk); walk = NextItem (walk)) {
								if (!check_isfullbarrier (ThisItem (walk))) {
									chkreport (CHK_BARRIER_BAD_DECL, chklocn);
								}
							}
						} else if (!check_isfullbarrier (declbars)) {
							chkreport (CHK_BARRIER_BAD_DECL, chklocn);
						}
						/*}}}*/
						{
							/*{{{  add name, scope and check - return*/
							const scopestack_t namestackmarker = markscopenames ();

							if (TagOf (declbars) == S_LIST) {
								treenode *walk;

								for (walk=declbars; !EndOfList (walk); walk = RightOf (walk)) {
									addname (ThisItem (walk), NULL);
								}
							} else {
								addname (declbars, NULL);
							}
							scopeandcheck (CBodyAddr (t));
							descopenames (namestackmarker);
							return;
							/*}}}*/
						}
					}

					if (extbars) {
						scopeandcheck (CBodyAddr (t));
						return;
					}
				}

				if (tag == S_PRIPAR) {
					/*{{{  PRI PAR - return*/
					/* return removed by frmb: want this to fall through now */
					const BOOL savedinsidepripar = insidepripar;

					if (insidepripar) {
						chkerr (CHK_NESTED_PRI_PAR, chklocn);
					}
					insidepripar = TRUE;
					nestedpripar = TRUE;
					scopeandcheck (CBodyAddr (t));
					if (listitems (CBodyOf (t)) != 2) {
						chkerr (CHK_INV_PRI_PAR, LocnOf (t));
					}
					insidepripar = savedinsidepripar;
					return;
					/*}}}  */
				} else if (((current_fe_data->fe_lang & FE_LANG_OCCAM) != 0) && (tag == S_PLACEDPAR)) {
					/* bug TS/1623 09/04/92 */
					chkerr_s (CHK_NOT_IMPLEMENTED, chklocn, tagstring (S_PLACEDPAR));
				} else if (((tag == S_PRIALT) || (tag == S_ALT)) && listitems (CBodyOf (t))) {
					/*{{{  check that any guards (except the last) are not [TRUE &] SKIP */
					caltskip (CBodyOf (t), tag);
					/*}}}  */
				} else if (tag == S_FORKING) {
					/*{{{  FORKING node - return */
					{
						wordnode *barrier_name = lookupword ("$fork.barrier", 13);
						treenode *new_name, *new_decl;
						static treenode *typetree = NULL;
						treenode *old_forking_node = forking_node;
						int old_forking_lexlevel = forking_lexlevel;
						const int old = switch_to_real_workspace ();
						
						if ((TagOf (CBodyOf (t)) == S_DECL) && (NNameOf (DNameOf (CBodyOf (t))) == barrier_name)) {
							/* already checked this! */
							new_name = DNameOf (CBodyOf (t));
						} else {
							treenode *body = CBodyOf (t);

							if (!typetree) {
								/* create new type-tree for the forking barrier type (added S_BARRIER as a leafnode), but won't parse directly) */
								typetree = newleafnode (S_BARRIER, NOPOSN);
							}
							new_name = newnamenode (N_DECL, chklocn, barrier_name, typetree, NULL, check_lexlevel, 0xfff, NM_WORKSPACE);
							/* get rid of the intermediate LIST node */
							if (TagOf (body) == S_LIST) {
								new_decl = newdeclnode (S_DECL, chklocn, new_name, NULL, ThisItem (CBodyOf (t)));
							} else {
								new_decl = newdeclnode (S_DECL, chklocn, new_name, NULL, CBodyOf (t));
							}
							SetNDecl (new_name, new_decl);
							SetNUsed (new_name, TRUE);
							SetNVUseCount (new_name, 0);
							SetCBody (t, new_decl);
						}
						switch_to_prev_workspace (old);

						forking_node = t;
						forking_lexlevel = check_lexlevel;
#if 0
fprintf (stderr, "scopeandcheck(): FORKING: set forking_node to %p before body-check\n", t);
#endif
						/* scope-and-check contained process */
						scopeandcheck (CBodyAddr (t));

#if 0
fprintf (stderr, "scopeandcheck(): FORKING: after body check, forking_node is %p\n", t);
#endif
						forking_node = old_forking_node;
						forking_lexlevel = old_forking_lexlevel;

						/* were there any forks in it ? */
						/* frmb (05/01/2005): moved into use2 since we need to take into account PROC calls that FORK */
#if 0
						if (!NVUseCountOf (new_name)) {
							chkwarn (CHK_EMPTY_FORKING, LocnOf (t));
							/* gets removed in trans */
						}
#endif
						return;
					}
					/*}}}*/
				}
#ifdef MOBILES
				else if (tag == S_CLAIM) {
					/*{{{  CLAIM node - maybe return*/
					int anon_ct_iospec = 0;		/* used for anonymous channel-type handing */
					int *sharedattrptr = NULL;

					/* first, is temporary input/output node ? */
					if (TagOf (CTempOf (t)) == S_ASINPUT) {
						anon_ct_iospec |= TypeAttr_marked_in;
						SetCTemp (t, OpOf (CTempOf (t)));
					} else if (TagOf (CTempOf (t)) == S_ASOUTPUT) {
						anon_ct_iospec |= TypeAttr_marked_out;
						SetCTemp (t, OpOf (CTempOf (t)));
					}

					/* handle CLAIM, scopeandcheck the temp value */
					scopeandcheck (CTempAddr (t));

#if 0
fprintf (stderr, "scopeandcheck: CLAIM node.  CTempOf (t) [after scopeandcheck] =");
printtreenl (stderr, 4, CTempOf (t));
#endif
					/* is it a shared something ? */
					switch (TagOf (CTempOf (t))) {
						/*{{{  DECL, PARAM, RESULTPARAM, ABBR*/
					case N_DECL:
					case N_PARAM:
					case N_RESULTPARAM:
					case N_ABBR:
						{
							treenode *ntype = NTypeOf (CTempOf (t));
							int errored = 0;

							/* right, is it an anonymous channel-type being claimed ? */
							if (TagOf (ntype) == S_ANONCHANTYPE) {
								treenode *newtemp = CTempOf (t);

								/* yes..  oki, check for direction specifier on variable */
								if (!anon_ct_iospec) {
									chkerr_s (CHK_CLAIM_ANON_NODIRSPEC, chklocn, WNameOf (NNameOf (CTempOf (t))));
									errored = 1;
								} else {
									/* subsitute in the right half for CLAIMing */
									if (anon_ct_iospec & TypeAttr_marked_out) {
										/* client side */
										newtemp = mobile_getanon_fromvar (CTempOf (t), FALSE, FALSE, *tptr);
									} else {
										/* server side */
										newtemp = mobile_getanon_fromvar (CTempOf (t), TRUE, FALSE, *tptr);
									}
								}
								if (!newtemp) {
									/* NULL, means it doesn't exist really */
									chkerr (CHK_CLAIM_VAR_BADDIRSPEC, chklocn);
									errored = 1;
								} else {
									SetCTemp (t, newtemp);
#if 0
fprintf (stderr, "scopeandcheck: CLAIM node.  Adjusted CTemp to newtemp =");
printtreenl (stderr, 4, newtemp);
fprintf (stderr, "scopeandcheck: CLAIM node.  Adjusted CTemp NTypeOf (newtemp) =");
printtreenl (stderr, 4, NTypeOf (newtemp));
#endif
									ntype = NTypeOf (newtemp);
								}
							}

							if (ntype && isdynmobilearraytype (ntype)) {
								/* claiming an array of channel-ends */
								ntype = ARTypeOf (MTypeOf (ntype));
#if 0
fprintf (stderr, "chk4: scopeandcheck(): CLAIM node.  claiming dynmobilearraytype, ntype now:");
printtreenl (stderr, 4, ntype);
#endif
							}

							/* if we screwed up, don't bother generating any more errors regarding this */
							if (!errored) {
								if (TagOf (ntype) == S_CHAN) {
									/* this can happen if the SHARED attribute is missed for an anonymous channel-type,
									 * generate a more meaningful error
									 */
									chkerr (CHK_CLAIM_VAR_ISCHAN, chklocn);
								} else if (TagOf (ntype) != N_TYPEDECL) {
									chkerr_s (CHK_CLAIM_VAR_NOTCHANTYPE, chklocn, WNameOf (NNameOf (CTempOf (t))));
								} else if (!(NTypeAttrOf (ntype) & TypeAttr_shared)) {
									if (TagOf (ntype) != S_UNDECLARED) {
										chkerr_s (CHK_CLAIM_VAR_NOTSHARED, chklocn, WNameOf (NNameOf (CTempOf (t))));
									}
								} else if ((TagOf (ntype) == N_TYPEDECL) && (NTypeAttrOf (ntype) & TypeAttr_shared)) {
									sharedattrptr = NTypeAttrAddr (ntype);
								}
							}
						}
						break;
						/*}}}*/
						/*{{{  ARRAYSUB, RECORDSUB*/
					case S_ARRAYSUB:
					case S_RECORDSUB:
						{
							treenode *ntype = typecheck (CTempOf (t), unknownnodeptr);

#if 0
fprintf (stderr, "scopeandcheck: CLAIM node.  CTempOf(t) was a record/array sub.  typecheck result (ntype) is:");
printtreenl (stderr, 4, ntype);
#endif
							if ((TagOf (ntype) != N_TYPEDECL) || !(NTypeAttrOf (ntype) & TypeAttr_shared)) {
								if (TagOf (ntype) != S_UNDECLARED) {
									chkerr_s (CHK_CLAIM_VAR_NOTSHARED, chklocn, WNameOf (NNameOf (basedecl (CTempOf (t)))));
								}
							} else if ((TagOf (ntype) == N_TYPEDECL) && (NTypeAttrOf (ntype) & TypeAttr_shared)) {
								sharedattrptr = NTypeAttrAddr (ntype);
							}
						}
						break;
						/*}}}*/
					default:
						chkerr_s (CHK_CLAIM_VAR_BADTYPE, chklocn, "<unknown>");
						break;
					}
					/* get rid of any intermediate LIST node */
					if (TagOf (CBodyOf (t)) == S_LIST) {
						SetCBody (t, ThisItem (CBodyOf (t)));
					}

					if (sharedattrptr) {
						/* change the variable to unshared inside the CLAIM -- checked FIXED things later */
						*sharedattrptr = (*sharedattrptr & ~TypeAttr_shared);
						scopeandcheck (CBodyAddr (t));
						*sharedattrptr = (*sharedattrptr | TypeAttr_shared);
						return;
					}
					/*}}}*/
				}
#endif
				else if (tag == S_RESIGN) {
					/*{{{  barrier RESIGN, check and return*/
					treenode *bar;

					if (CTempOf (t)) {
						scopeandcheck (CTempAddr (t));
					}
					bar = CTempOf (t);
					if (!check_isfullbarrier (bar)) {
						chkreport (CHK_RESIGN_NOT_BARRIER, chklocn);
					} else if (check_resignedfullbarrier (bar)) {
						chkreport (CHK_BARRIER_RESIGNED, chklocn);
					}

					/* flag the barrier with TypeAttr_resigned and check body */
					{
						check_resignbarrier (bar);
						scopeandcheck (CBodyAddr (t));
						check_unresignbarrier (bar);
					}

					return;
					/*}}}*/
				}

				guy_not_asm = (tag == S_GUY);
				if (guy_not_asm && current_fe_data->fe_warning_guy) {
					/* we pass 'NOPOSN' to the memo function so that it doesn't just
					   restrict the 'memo' to this one line of source code */
					if (fe_memo_err (current_fe_handle, CHK, CHK_WARN_GUY, NOPOSN)) {
						msg_out (SEV_WARN, CHK, CHK_WARN_GUY, chklocn);
					}
				}

				tptr = CBodyAddr (t);
				break;
				/*}}}  */
				/*{{{  REPLSEQ REPLPAR REPLIF REPLALT PRIREPLALT   return */
			case REPLCNODE:
				{
					/*int savedinsidepripar = insidepripar; */
					const BOOL replpar = parrepl (tag);
					const scopestack_t namestackmarker = markscopenames ();

#if 0
fprintf (stderr, "um, in REPLCNODE scopeandcheck..\n");
#endif
					current_params = NULL;	/* bug TS/1910 26/01/93 */
					scopeandcheck (ReplCStartExpAddr (t));
					scopeandcheck (ReplCLengthExpAddr (t));
					scopeandcheck (ReplCStepExpAddr (t));
					if ((treenode *) crepl (t) != (treenode *) NULL) {
						const int old_lexlevel = check_lexlevel;

#if 0
						/*{{{  Check repl is constant (Compiler and configurer only) */
						if (((current_fe_data->fe_lang & FE_LANG_NDL) == 0) && replpar && !isconst (ReplCLengthExpOf (t)))
							chkerr (CHK_REPL_NOT_CONST, chklocn);
						/*}}}  */
#endif	/* no longer a restriction :) */
						/*{{{  REPLPRIPAR / PLACEDREPLPAR is not implemented */
						if ((tag == S_PRIREPLPAR) || (((current_fe_data->fe_lang & FE_LANG_OCCAM) != 0) && (tag == S_PLACEDREPLPAR))) {	/* bug TS/1623 09/04/92 */
							chkerr_s (CHK_NOT_IMPLEMENTED, chklocn, tagstring (tag));
						}
						/*}}}  */
						check_lexlevel += replpar;
#if 0
fprintf (stderr, "REPLCNODE: ReplCNameOf (t) =");
printtreenl (stderr, 4, ReplCNameOf (t));
#endif
						addname (ReplCNameOf (t), NULL);

						/* if a replicated PAR, check for BARRIER or EXTENDs */
						if (replpar && ReplCTempOf (t)) {
							treenode *bar = ReplCTempOf (t);
							treenode *declbars, *extbars;

							if (bar && (TagOf (bar) == S_BAREXTEND)) {
								/* got both */
								declbars = LeftOpOf (bar);
								extbars = RightOpOf (bar);
							} else if (bar && (TagOf (bar) == S_EXTENDS)) {
								/* got extensions only */
								declbars = NULL;
								extbars = bar;
							} else if (bar) {
								/* must be declarations only */
								declbars = bar;
								extbars = NULL;
							} else {
								declbars = NULL;
								extbars = NULL;
							}

							/* look at extensions first */
							if (extbars && (TagOf (extbars) == S_EXTENDS)) {
								/*{{{  extending a barrier, just scope and check it*/
#if 0
fprintf (stderr, "scopeandcheck: PAR: EXTENDS: extbars = ");
printtreenl (stderr, 4, extbars)
#endif
								scopeandcheck (OpAddr (extbars));
								extbars = OpOf (extbars);

								if (!extbars) {
									chkreport (CHK_EXTENDS_NOT_BARRIER, chklocn);
								} else if (TagOf (extbars) == S_LIST) {
									treenode *walk;

									for (walk=extbars; !EndOfList (walk); walk = RightOf (walk)) {
										treenode *thisbar = ThisItem (walk);

										if (!check_isfullbarrier (thisbar)) {
											chkreport (CHK_EXTENDS_NOT_BARRIER, chklocn);
										} else if (check_resignedfullbarrier (thisbar)) {
											chkreport (CHK_BARRIER_RESIGNED, chklocn);
										}
									}
								} else if (!check_isfullbarrier (extbars)) {
									chkreport (CHK_EXTENDS_NOT_BARRIER, chklocn);
								} else if (check_resignedfullbarrier (extbars)) {
									chkreport (CHK_BARRIER_RESIGNED, chklocn);

								}
								/*}}}*/
							} else if (extbars) {
								/* badness of some kind */
								chkreport (CHK_BARRIER_BAD_EXTENDS, chklocn);
							}

							/* then scope + check declarations, if any */
							if (declbars) {
								/*{{{  check declared things are barriers*/
								if (TagOf (declbars) == S_LIST) {
									treenode *walk;

									for (walk = declbars; !EndOfList (walk); walk = NextItem (walk)) {
										if (!check_isfullbarrier (ThisItem (walk))) {
											chkreport (CHK_BARRIER_BAD_DECL, chklocn);
										}
									}
								} else if (!check_isfullbarrier (declbars)) {
									chkreport (CHK_BARRIER_BAD_DECL, chklocn);
								}
								/*}}}*/
								{
									/*{{{  add name, scope and check - return*/
									const scopestack_t namestackmarker = markscopenames ();

									if (TagOf (declbars) == S_LIST) {
										treenode *walk;

										for (walk=declbars; !EndOfList (walk); walk = RightOf (walk)) {
											addname (ThisItem (walk), NULL);
										}
									} else {
										addname (declbars, NULL);
									}
									scopeandcheck (ReplCBodyAddr (t));
									descopenames (namestackmarker);
									/*}}}*/
								}
							} else {
								scopeandcheck (ReplCBodyAddr (t));
							}

						} else {
							scopeandcheck (ReplCBodyAddr (t));
						}

						if (tag == S_ARRAYCONSTRUCTOR) {
							treenode *list = ReplCTempOf (t);

#if 0
fprintf (stderr, "chk4: scopeandcheck on S_ARRAYCONSTRUCTOR! list is ");
printtreenl (stderr, 4, list);
#endif
							while (!EmptyList (list)) {
								addname (ThisItem (list), NULL);
								SetNUsed (ThisItem (list), TRUE);
								list = NextItem (list);
							}
							/* scopeandcheck (ReplCTempAddr (t)); */
						}
						check_lexlevel = old_lexlevel;
					}
					descopenames (namestackmarker);
				}
				return;
				/*}}}  */
				/*{{{  WHILE CHOICE                                return */
			case CONDNODE:
				scopeandcheck (CondGuardAddr (t));
				if (tag == S_X_VARIANT) {
					scopeandcheck (VRDuringAddr (t));
					scopeandcheck (VRAfterAddr (t));
				} else {
					scopeandcheck (CondBodyAddr (t));
				}
				if ((tag != S_SELECTION) && (tag != S_VARIANT) && (tag != S_X_VARIANT)) {
					*tptr = (treenode *) ccond (t);
				}
				return;
				/*}}}  */
				/*{{{  ALTERNATIVE                                 return */
			case ALTNODE:
				scopeandcheck (AltGuardAddr (t));
				scopeandcheck (AltInputAddr (t));
				scopeandcheck (AltBodyAddr (t));
				*tptr = (treenode *) calt (t);
				return;
				/*}}}  */
				/*{{{  PINSTANCE FINSTANCE                         return */
			case INSTANCENODE:
				{
					treenode **params = IParamListAddr (t);
					treenode *name;

					scopeandcheck (INameAddr (t));
#if 0
fprintf (stderr, "scopeandcheck: INSTANCENODE (did name), t =");
printtreenl (stderr, 4, t);
#endif
					/*{{{  strange, mode of pname doesn't get set right for encode/decode/etc., do it here */
					if (TagOf (INameOf (t)) == N_PREDEFPROC) {
						#if defined(PD_DECODE_CHANNEL) && defined(PD_DECODE_CHANNEL3) && defined(PD_ENCODE_CHANNEL)
						if (NNameOf (INameOf (t)) == lookupword ("ENCODE.CHANNEL", 14)) {
							SetNMode (INameOf (t), PD_ENCODE_CHANNEL);
						} else if (NNameOf (INameOf (t)) == lookupword ("DECODE.CHANNEL", 14)) {
							/* check for extra "go" parameter here */
							SetNMode (INameOf (t), PD_DECODE_CHANNEL);
							if (listitems (*params) == 3) {
								treenode *decode3 = (treenode *)lookupword ("DECODE.CHANNEL3", 15);

								SetIName (t, decode3);
								scopeandcheck (INameAddr (t));
								SetNMode (INameOf (t), PD_DECODE_CHANNEL3);
							}
						} else
						#endif /* PD_DECODE_CHANNEL && PD_DECODE_CHANNEL3 && PD_ENCODE_CHANNEL */
						if (NNameOf (INameOf (t)) == lookupword ("DECODE.DATA", 11)) {
							SetNMode (INameOf (t), PD_DECODE_DATA);
						}
					}
					/*}}}*/
					if (*params != NULL) {
#ifdef MOBILES
						if ((TagOf (t) == S_PINSTANCE) && (TagOf (INameOf (t)) == N_PREDEFPROC) && (NModeOf (INameOf (t)) == PD_ALLOC_CHAN_TYPE)) {
							/*{{{  ALLOC.CHAN.TYPE check (if so, typecheck 3rd param against first two)*/
							treenode *plist, *n1, *n2, *type;
							treenode *n1_type, *n2_type;

							scopeandcheck (params);
							plist = IParamListOf (t);
							if ((TagOf (plist) == S_LIST) &&
									!EmptyList (NextItem (plist)) &&
									!EmptyList (NextItem (NextItem (plist)))) {
								/* have at least three things in the list. */
								n1 = ThisItem (plist);
								n2 = ThisItem (NextItem (plist));
								type = ThisItem (NextItem (NextItem (plist)));

								fold_type_direction (n1);
								fold_type_direction (n2);

								n1_type = typecheck (n1, unknownnodeptr);
								n2_type = typecheck (n2, unknownnodeptr);
#if 0
fprintf (stderr, "chk4: scopeandcheck: ALLOC.CHAN.TYPE(): n1 is: ");
printtreenl (stderr, 4, n1);
fprintf (stderr, "chk4: scopeandcheck: ALLOC.CHAN.TYPE(): n1_type is: ");
printtreenl (stderr, 4, n1_type);
fprintf (stderr, "chk4: scopeandcheck: ALLOC.CHAN.TYPE(): n2 is: ");
printtreenl (stderr, 4, n2);
fprintf (stderr, "chk4: scopeandcheck: ALLOC.CHAN.TYPE(): n2_type is: ");
printtreenl (stderr, 4, n2_type);
#endif
								if ((TagOf (n1_type) != N_TYPEDECL) || (TagOf (n2_type) != N_TYPEDECL)) {
									chkerr (CHK_INV_CHANTYPE_ALLOC_TARGET, LocnOf (t));
								} else if (TagOf (type) != N_TYPEDECL) {
									chkerr (CHK_INV_CHANTYPE_ALLOC_TYPE, LocnOf (t));
								} else {
									/* okay, got two names and a sensible type */

									if (NNameOf (n1_type) != NNameOf (type)) {
										chkerr_s (CHK_INV_CHANTYPE_ALLOC_VARTYPE, chklocn, WNameOf (NNameOf (n1)));
									} else if (NNameOf (n2_type) != NNameOf (type)) {
										chkerr_s (CHK_INV_CHANTYPE_ALLOC_VARTYPE, chklocn, WNameOf (NNameOf (n2)));
									} else {
										/* check that both have direction specifiers in the type and that they are different */
										if (((NTypeAttrOf (n1_type) | NTypeAttrOf (n2_type)) & (TypeAttr_marked_in | TypeAttr_marked_out)) !=
												(TypeAttr_marked_in | TypeAttr_marked_out)) {
											chkerr (CHK_INV_CHANTYPE_ALLOC_TARGET, LocnOf (t));
										}
										SetRight (NextItem (plist), NULL);
#if 0
fprintf (stderr, "found ALLOC_CHAN_TYPE in INSTANCENODE!.  type = \n");
printtreenl (stderr, 4, type);
fprintf (stderr, "(n1) = ");
printtreenl (stderr, 4, (n1));
fprintf (stderr, "NTypeOf (n1) = ");
printtreenl (stderr, 4, NTypeOf (n1));
fprintf (stderr, "(n2) = ");
printtreenl (stderr, 4, (n2));
fprintf (stderr, "NTypeOf (n2) = ");
printtreenl (stderr, 4, NTypeOf (n2));
#endif
									}
								}
							} else {
								chkerr (CHK_INV_CHANTYPE_ALLOC, LocnOf (t));
							}
							/*}}}*/
						#if defined(PD_DECODE_CHANNEL) && defined(PD_DECODE_CHANNEL3) && defined(PD_ENCODE_CHANNEL)
						} else if ((TagOf (t) == S_PINSTANCE) && (TagOf (INameOf (t)) == N_PREDEFPROC) &&
								((NModeOf (INameOf (t)) == PD_DECODE_CHANNEL) || (NModeOf (INameOf (t)) == PD_DECODE_CHANNEL3) || (NModeOf (INameOf (t)) == PD_ENCODE_CHANNEL))) {
							/*{{{  ENCODE.CHANNEL/DECODE.CHANNEL chan-dir check (first and second must be INPUT, third must be OUTPUT) -- see also USE */
							treenode *plist;
							int paramno = 1;
							int is_decode = (NModeOf (INameOf (t)) == PD_DECODE_CHANNEL) || (NModeOf (INameOf (t)) == PD_DECODE_CHANNEL3);

							scopeandcheck (params);
#if 0
fprintf (stderr, "found ENCODE/DECODE.CHANNEL in INSTANCENODE!.  params (after scopeandcheck) are: ");
printtreenl (stderr, 4, *params);
#endif
							plist = IParamListOf (t);
							while (plist && (paramno < 4)) {
								treenode *param = ThisItem (plist);

								if (paramno < 3) {
									/* ban outputs */
									if (TagOf (param) == S_ASOUTPUT) {
										chkerr_i (is_decode ? CHK_DECODE_BAD_CHANDIR : CHK_ENCODE_BAD_CHANDIR, chklocn, paramno);
										param = NULL;
									} else if (TagOf (param) == S_ASINPUT) {
										param = OpOf (param);
									}
									/* check any specifier on the actual parameter itself */
									while (param) {
										switch (TagOf (param)) {
										case S_ARRAYSUB:
										case S_ARRAYITEM:
										case S_RECORDSUB:
										case S_RECORDITEM:
											param = ASBaseOf (param);
											break;
										case N_PARAM:
										case N_ABBR:
										case N_DECL:		/* silly.. */
#if 0
fprintf (stderr, "N_... in scopeandcheck, NTypeOf (param) =");
printtreenl (stderr, 4, NTypeOf (param));
#endif
											if ((TagOf (NTypeOf (param)) == S_CHAN) && (TypeAttrOf (NTypeOf (param)) & TypeAttr_marked_out)) {
												chkerr_i (is_decode ? CHK_DECODE_BAD_CHANDIR : CHK_ENCODE_BAD_CHANDIR, chklocn, paramno);
											}
											/* fall through */
										default:
											param = NULL;
											break;
										}
									}
								} else {
									/* ban inputs */
									if (TagOf (param) == S_ASINPUT) {
										chkerr_i (is_decode ? CHK_DECODE_BAD_CHANDIR : CHK_ENCODE_BAD_CHANDIR, chklocn, paramno);
										param = NULL;
									} else if (TagOf (param) == S_ASOUTPUT) {
										param = OpOf (param);
									}
									/* check any specifier on the actual parameter itself */
									while (param) {
										switch (TagOf (param)) {
										case S_ARRAYSUB:
										case S_ARRAYITEM:
										case S_RECORDSUB:
										case S_RECORDITEM:
											param = ASBaseOf (param);
											break;
										case N_PARAM:
										case N_ABBR:
										case N_DECL:		/* silly.. */
											if ((TagOf (NTypeOf (param)) == S_CHAN) && (TypeAttrOf (NTypeOf (param)) & TypeAttr_marked_in)) {
												chkerr_i (is_decode ? CHK_DECODE_BAD_CHANDIR : CHK_ENCODE_BAD_CHANDIR, chklocn, paramno);
											}
											/* fall through */
										default:
											param = NULL;
											break;
										}
									}
								}

								plist = NextItem (plist);
								paramno++;
							}
							/*}}}*/
						#endif /* PD_DECODE_CHANNEL && PD_DECODE_CHANNEL3 && PD_ENCODE_CHANNEL */
						} else if ((TagOf (t) == S_PINSTANCE) && ((TagOf (INameOf (t)) == N_PROCDEF) ||
									ismobileprocvar (INameOf (t)) ||			/* frmb: more complex to find these */
									(TagOf (INameOf (t)) == N_SCPROCDEF) || (TagOf (INameOf (t)) == N_LIBPROCDEF) || (TagOf (INameOf (t)) == N_LIBMPROCDECL))) {
							/*{{{  scope and check PROC parameters*/
							treenode *aparam = IParamListOf (t);
							treenode *fparam;
							
							if (ismobileprocvar (INameOf (t))) {
								/* FIXME: should probably wrap this up somewhere.. */
								fparam = NTypeOf (MTypeOf (NTypeOf (INameOf (t))));
							} else {
								fparam = NTypeOf (INameOf (t));
							}

							while (!EndOfList (aparam) && !EndOfList (fparam)) {
								treenode *ap;

								/* scope actual parameter */
								scopeandcheck (ThisItemAddr (aparam));

								/* only investigate further if we're using an anon chan-type as the actual param */
								ap = ThisItem (aparam);
								/* if the actual param is a BARRIER, make sure it's not been resigned from */
								if (((TagOf (ap) == N_DECL) || (TagOf (ap) == N_PARAM)) && check_isfullbarrier (ap)) {
									if (check_resignedfullbarrier (ap) && !IForkedOf (t)) {
										chkreport (CHK_BARRIER_RESIGNED, chklocn);
									}
								}
								if ((TagOf (ap) == S_ASINPUT) || (TagOf (ap) == S_ASOUTPUT)) {
									/* just walk over this for now */
									ap = OpOf (ap);
								}
#if 0
fprintf (stderr, "scopeandcheck: INSTANCENODE: checking param ap=");
printtreenl (stderr, 4, ap);
#endif
								if (((TagOf (ap) == N_DECL) || (TagOf (ap) == N_PARAM)) && (TagOf (NTypeOf (ap)) == S_ANONCHANTYPE)) {
									/*{{{  handle anonymous channel-type */
									treenode *ctvar;	/* channel-type variable */

									if (TagOf (ThisItem (aparam)) == S_ASINPUT) {
										/* specifically after server end */
										ctvar = mobile_getanon_fromvar (ap, TRUE, TRUE, t);
									} else if (TagOf (ThisItem (aparam)) == S_ASOUTPUT) {
										/* specifically after client end */
										ctvar = mobile_getanon_fromvar (ap, FALSE, TRUE, t);
									} else {
										/* insist on direction for now.. */
										chkerr_s (CHK_NO_CHANDIR_SPEC, chklocn, WNameOf (NNameOf (ap)));
										ctvar = NULL;
									}
									if (ctvar) {
										treenode *fptype = NTypeOf (ThisItem (fparam));

										/* now, do something depending on the formal type -- ctvar is the record subscript expression */
										if ((TagOf (fptype) == N_TYPEDECL) && (TagOf (NTypeOf (fptype)) == S_MOBILE) &&
												(NTypeAttrOf (fptype) & TypeAttr_shared)) {
											/* looks like an anonymous channel-type -- subsitute just plain var (must always be SHARED!) */
											/* auto-CLONE. (there's another in checkparams for handling regular SHARED CHAN TYPEs) */
											SetLeft (aparam, newmopnode (S_CLONE, NOPOSN, ASBaseOf (ctvar), S_MOBILE));
											scopeandcheck (ThisItemAddr (aparam));
										} else if (TagOf (fptype) == S_CHAN) {
											/* expecting regular channel -- no CLONE! */
											SetLeft (aparam, ctvar);
											scopeandcheck (ThisItemAddr (aparam));
										}
#if 0
fprintf (stderr, "scopeandcheck: INSTANCENODE: param checking; ThisItem(aparam) =");
printtreenl (stderr, 4, ThisItem (aparam));
fprintf (stderr, "scopeandcheck: INSTANCENODE: param checking; ctvar =");
printtreenl (stderr, 4, ctvar);
fprintf (stderr, "scopeandcheck: INSTANCENODE: param checking; fptype [attr=%d] =", NTypeAttrOf (fptype));
printtreenl (stderr, 4, fptype);
#endif
									} else {
#if 0
fprintf (stderr, "scopeandcheck: INSTANCENODE: param checking; ap =");
printtreenl (stderr, 4, ap);
fprintf (stderr, "scopeandcheck: INSTANCENODE: param checking; ctvar = NULL\n");
#endif
										/* will throw up a type error later */
									}
									/*}}}*/
								}
								aparam = NextItem (aparam);
								fparam = NextItem (fparam);
							}
							/*}}}*/
						} else {
							scopeandcheck (params);
						}
#else	/* !MOBILES */
						scopeandcheck (params);
#endif	/* !MOBILES */
					}

					name = INameOf (t);
					/* check for a recursive instance.. */
					if ((name == current_proc) && NPRecursiveOf (current_proc)) {
						SetIRecursive (t, 1);
					}
					/*{{{  if it is actually a type conversion => RETURN */
					if ((TagOf (name) == N_TYPEDECL) && (listitems (IParamListOf (t)) == 1)) {
						const SOURCEPOSN locn = LocnOf (t);
						treenode *const explist = IParamListOf (t);
						freenode (tptr);
						*tptr = newmopnode (S_EXACT, locn, newlistnode (S_LIST, locn, name, explist), S_NAME);
						return;
					}
					/*}}}  */

					if (IDynaddrOf (t)) {
						if (IDynmemOf (t) == FALSE) {
							chkerr_s (CHK_CALLAT_NOT_DYNAMIC, chklocn, WNameOf (NNameOf (name)));
						}

						scopeandcheck (IDynaddrAddr (t));
					}


					if (insidepripar && (TagOf (name) != N_DECL) && NPNestedPriParOf (name)) {
						/* N.B. tag is N_DECL if the lookupname failed */
						chkerr_s (CHK_NESTED_PRI_PROC, chklocn, WNameOf (NNameOf (name)));
					}

					/*{{{  check for 'nested' attributes */
					/* bug TS/1982 17/12/92 */
					if (NPNestedPriParOf (name)) {
						nestedpripar = TRUE;
					}

					/* bug TS/1983 17/12/92 */
					if (NPNestedTimerOf (name)) {
						nestedtimer = TRUE;
					}
					if (NPNestedPlaceOf (name)) {
						nestedplace = TRUE;
					}
					if (NPNestedPortOf (name)) {
						nestedport = TRUE;
					}

#if 0
					printf ("instance: \"%s\", nestedtimer: %d, nestedplace: %d, nestedport: %d\n",
						WNameOf (NNameOf (name)), NPNestedTimerOf (name), NPNestedPlaceOf (name), NPNestedPortOf (name));
#endif
					/*}}}  */

					if (((current_fe_data->fe_lang & FE_LANG_OCCAM) != 0) && separatelycompiled (name)) {
						checklibproctype (name, LocnOf (t), FALSE);
					}

#ifdef MOBILES
					/* check that a MOBILE process activation is not FORKed (for now) */
					if ((tag == S_PINSTANCE) && ismobileprocvar (INameOf (t)) && IForkedOf (t)) {
						chkreport (CHK_MPP_FORKED, chklocn);
					}
#endif

					/* make sure that anything FORKED is _not_ INLINE, or a pre-define */
					if ((tag == S_PINSTANCE) && IForkedOf (t)) {
						if (TagOf (name) == N_INLINEPROCDEF) {
							chkreport_s (CHK_NO_INLINE_FORK, chklocn, WNameOf (NNameOf (name)));
						} else if ((TagOf (name) == N_PREDEFPROC)) {
							chkreport_s (CHK_NO_PREDEF_FORK, chklocn, WNameOf (NNameOf (name)));
						}
					}

					/* check that a FORK'd PROC is within a FORKING */
					if ((tag == S_PINSTANCE) && IForkedOf (t)) {
#if 0
fprintf (stderr, "scopeandcheck(): INSTANCE, IForkedOf set, forking_node = %p\n", forking_node);
#endif
						if (!forking_node && 0 /* FIXME: compiling for dynamic-process library */) {
							chkerr (CHK_NO_LOCAL_FORKING, chklocn);
						} else if (!forking_node) {
							/* parameterised fork (added by back-end later) */
							SetIFork (t, NULL);
						} else {
							treenode *fname = DNameOf (CBodyOf (forking_node));

							SetIFork (t, forking_node);
							/* increment the NVUseCount for the FORKING magic variable.  Bit grotty, but sufficient */
							SetNVUseCount (fname, NVUseCountOf (fname) + 1);
						}
					}
					/* check for instance requiring barrier */
					if ((tag == S_PINSTANCE) && NPForksOf (INameOf (t))) {
						if (forking_node) {
							treenode *fname = DNameOf (CBodyOf (forking_node));

							SetIFork (t, forking_node);			/* yes -- set IForkOf .. */
							SetNVUseCount (fname, NVUseCountOf (fname) + 1);
						}
					}

					if (tag == S_PINSTANCE) {
						*tptr = (treenode *)cinstance (t);
					}
					return;
				}
				/*}}}  */
				/*{{{  CASE_INPUT, DELAYED_INPUT, OUTPUT, INPUT, TAGGED_INPUT, ASS, CASE, X_INPUT, X_TAGGED_INPUT, X_CASE_INPUT  return */
			case ACTIONNODE:
				scopeandcheck (LHSAddr (t));
#ifdef MOBILES
				/*{{{  find and adjust anonymous channel-type on LHS here (communication only) */
				switch (TagOf (t)) {
				case S_OUTPUT:
				case S_INPUT:
				case S_TAGGED_INPUT:
				case S_X_TAGGED_INPUT:
				case S_CASE_INPUT:
				case S_X_CASE_INPUT:
				case S_X_INPUT:
					if ((TagOf (LHSOf (t)) == N_DECL) || (TagOf (LHSOf (t)) == N_PARAM)) {
						if (TagOf (NTypeOf (LHSOf (t))) == S_ANONCHANTYPE) {
							treenode *newlhs;

							if (TagOf (t) == S_OUTPUT) {
								newlhs = mobile_getanon_fromvar (LHSOf (t), FALSE, TRUE, t);
							} else {
								newlhs = mobile_getanon_fromvar (LHSOf (t), TRUE, TRUE, t);
							}
#if 0
fprintf (stderr, "scopeandcheck: ACTIONNODE (comms): LHSOf (t) =");
printtreenl (stderr, 4, LHSOf (t));
fprintf (stderr, "   \"     \"   : ACTIONNODE (comms): newlhs =");
printtreenl (stderr, 4, newlhs);
#endif
							SetLHS (t, newlhs);
							scopeandcheck (LHSAddr (t));
						}
					}
					break;
				default:
					if ((TagOf (LHSOf (t)) == N_DECL) || (TagOf (LHSOf (t)) == N_PARAM)) {
						if (TagOf (NTypeOf (LHSOf (t))) == S_ANONCHANTYPE) {
							/* bad use of anonymous channel-type */
							if (TagOf (t) == S_ASS) {
								chkerr_s (CHK_ANONCHANTYPE_ASSIGN, chklocn, WNameOf (NNameOf (LHSOf (t))));
							}
						}
					}
					break;
				}
				/*}}}*/
#endif
				/*{{{  fix CASE-less variant input */
				/* We must do this after we've scoped the LHS, but before we scope the RHS */
				if ((TagOf (t) == S_INPUT) || (TagOf (t) == S_X_INPUT)) {
					treenode *const lhstype = typecheck (LHSOf (t), unknownnodeptr);
					if ((TagOf (lhstype) == S_CHAN) || (TagOf (lhstype) == S_PORT)) {
						treenode *protocol = ProtocolOf (lhstype);
						if (TagOf (protocol) == N_TPROTDEF) {
							/* This is a plain input on a channel/port that carries a tagged protocol;
							   turn it into the equivalent tagged (? CASE foo) input */
							SetTag (t, (TagOf (t) == S_INPUT) ? S_TAGGED_INPUT : S_X_TAGGED_INPUT);
						}
					}
				}
				/*}}}*/
				/*{{{  check for special scope rules */
#ifdef OCCAM2_5
				switch (TagOf (t)) {
				case S_OUTPUT:
				case S_TAGGED_INPUT:
				case S_X_TAGGED_INPUT:
				case S_CASE_INPUT:
				case S_X_CASE_INPUT:
					scope_communication (t);
					break;
				default:
					break;
				}
#endif
				/*}}}  */

#if 0
fprintf (stderr, "scopeandcheck: ACTIONNODE: about to scopeandcheck: ");
printtreenl (stderr, 4, RHSOf (t));
#endif
				scopeandcheck (RHSAddr (t));
#if 0
fprintf (stderr, "+++ scopeandcheck: ACTIONNODE: done that, left with: ");
printtreenl (stderr, 4, RHSOf (t));
#endif
				/* *tptr = (tag == S_CASE) ? (treenode *) ccase (t) : (treenode *) caction (t); */
				if (tag == S_CASE) {
					*tptr = (treenode *)ccase (t);
				} else {
					*tptr = (treenode *)caction (t);
				}
				/* maybe scope processes inside extended input */
				switch (TagOf (t)) {
				case S_X_INPUT:
				case S_X_TAGGED_INPUT:
					scopeandcheck (ActionDuringAddr (t));
					scopeandcheck (ActionAfterAddr (t));
					break;
				}
				return;
				/*}}}  */
				/*{{{  expression */
				/*{{{  monadics            return */
			case MOPNODE:
#if 0
if (TagOf (t) == S_CLONE) {
	fprintf (stderr, "scopeandcheck: MOPNODE (CLONE): OpOf (t) =");				
	printtreenl (stderr, 4, OpOf (t));
}
#endif
				tptr = OpAddr (t);
				break;
				/*}}}  */
				/*{{{  literals */
			case LITNODE:
				if (tag == S_CONSTRUCTOR) {
					scopeandcheck (LitExpAddr (t));
				}
				tptr = LitTypeAddr (t);
				break;
				/*}}}  */
				/*{{{  consttablenode */
#ifdef OCCAM2_5
			case CONSTTABLENODE:
				if (tag == S_STRING && ConstTableTypeOf (t) != NULL) {
					tptr = ConstTableTypeAddr (t);
				} else {
					return;
				}
				break;
#endif
				/*}}}  */
				/*}}}  */
				/*{{{  dyadics             break */
			case DOPNODE:
				switch (tag) {
				case S_TYPEHASHCHECK:
					/* expecting some type-tree on the left, integer on the right */
					{
						treenode **lhs = LeftOpAddr (t);
						treenode **rhs = RightOpAddr (t);
						treenode *lhsv = NULL;

						scopeandcheck (lhs);
						scopeandcheck (rhs);

						/* FIXME: this needs checking, may not be able to do much until the back-end has been through it,
							as it is after this where certain type-hash affecting things are set! */
#if 0
fprintf (stderr, "scopeandcheck(): removing typehashcheck for:");
printtreenl (stderr, 4, *lhs);
fprintf (stderr, "scopeandcheck(): against typehash:", mtypehash);
printtreenl (stderr, 4, *rhs);
#endif

						lhsv = *lhs;
						*lhs = NULL;
						freenode (tptr);
						*tptr = lhsv;
						return;
					}
					break;
				case S_GUYCODE:
				case S_GUYSTEP:
					scopeandcheck (RightOpAddr (t));
					/* guy_not_asm was set up by enclosing S_GUY or S_ASM */
					*tptr = (treenode *) cguy_or_asm (t, guy_not_asm, current_proc);
					return;
#ifdef OCCAM2_5
				case S_OFFSETOF:
					scope_offsetof (t);
					return;
#endif
					/* relational-operator manglings, added by Fred, 08/02/2001
					 * turns:
					 *	(a, b, ...) op (c, d, ...)
					 * into:
					 *	(a op c) AND (b op d)
					 */
				case S_EQ:
				case S_NE:
					if ((TagOf (LeftOpOf (t)) == S_LIST) && (TagOf (RightOpOf (t)) == S_LIST)) {
						*tptr = reldop_list_transform (t, LeftOpOf (t), RightOpOf (t), S_AND);
						/* loops around to process new tree */
						break;
					}
					/* XXX: fall through */
				default:
					/* doesn't make sense to have a list on _either_ side here */
					if ((TagOf (LeftOpOf (t)) == S_LIST) || (TagOf (RightOpOf (t)) == S_LIST)) {
						chkreport (CHK_UNEXPECTED_LIST, chklocn);
					}
					scopeandcheck (LeftOpAddr (t));
					tptr = RightOpAddr (t);
					break;
				}
				break;
				/*}}}  */
				/*{{{  VALOF                       return */
			case VALOFNODE:
				{
					const scopestack_t namestackmarker = markscopenames ();
					scopeandcheck (VLBodyAddr (t));
					descopenames (namestackmarker);	/* Descope names within Valof */
					scopeandcheck (VLResultListAddr (t));
					*tptr = (treenode *) cvalof (t, current_scope ());
				}
				return;
				/*}}}  */
				/*{{{  conditional expression */
#ifdef CONDEXP
			case CONDEXPNODE:
				{
					scopeandcheck (CondExpGuardAddr (t));
					scopeandcheck (CondExpTrueAddr (t));
					tptr = CondExpFalseAddr (t);
				}
				break;
#endif

				/*}}}  */

				/*{{{  element */
			case ARRAYSUBNODE:
#if 0
fprintf (stderr, "scopeandcheck: ARRAYSUBNODE =");
printtreenl (stderr, 4, t);
#endif
				/* special handling for subscripted array-constructor */
				if (TagOf (ASBaseOf (t)) == S_ARRAYCONSTRUCTOR) {
					*tptr = csubscriptarrayconstructor (t);
#if 0
fprintf (stderr, "scopeandcheck: hacked subscript node into:");
printtreenl (stderr, 4, *tptr);
#endif
				} else {
					scopeandcheck (ASBaseAddr (t));
					/* array-constructor inside ? */
					if (TagOf (ASBaseOf (t)) == S_ARRAYCONSTRUCTOR) {
						*tptr = csubscriptarrayconstructor (t);
					} else {
						*tptr = csubscript (t);	/* convert to S_RECORDSUB if necessary */
						return;
					}
				}
				break;
			case SEGMENTNODE:
				if (TagOf (SNameOf (t)) == S_ARRAYCONSTRUCTOR) {
					*tptr = csegmentarrayconstructor (t);
				} else {
					scopeandcheck (SNameAddr (t));
					scopeandcheck (SStartExpAddr (t));
					scopeandcheck (SLengthExpAddr (t));
					tptr = SSubscriptExpAddr (t);
				}
				break;
				/*}}}  */
				/*{{{  type */
			case TYPENODE:
				switch (tag) {
					/*{{{  record */
				case S_RECORD:
					{
						treenode *field;
						for (field = ARTypeOf (t); field != NULL; field = DBodyOf (field)) {
							treenode *this_field = DNameOf (field);

							if (TagOf (this_field) == S_LIST) {
								this_field = ThisItem (this_field);
							}
							scopeandcheck (NTypeAddr (this_field));
						}
					}
					return;
					/*}}}  */
					/*{{{  array */
				case S_ARRAY:
					scopeandcheck (ARDimLengthAddr (t));
					tptr = ARTypeAddr (t);
					break;
					/*}}}  */
					/*{{{  buffered channel*/
				case S_BUFFERED:
					scopeandcheck (ARDimLengthAddr (t));		/* size expression */
					tptr = ARTypeAddr (t);
					break;
					/*}}}*/
					/*{{{  channel / port */
				case S_CHAN:
#ifdef MOBILES
				case S_ANONCHANTYPE:
#if 0
fprintf (stderr, "scopeandcheck: TYPENODE/CHAN: shared = %d. t =", TypeAttrOf (t) & TypeAttr_shared);
printtreenl (stderr, 4, t);
#endif
					/* if the protocol looks like ASINPUT/ASOUTPUT, scopeandcheck the sub-protocol, then put in the resulting type-decl --
					 * this must be a CHAN of some CHAN TYPE if this is the case.. */
					if (TagOf (ProtocolOf (t)) == S_ASINPUT) {
						const BIT32 subshared = OpTypeAttrOf (ProtocolOf (t)) & TypeAttr_shared;

						SetProtocol (t, OpOf (ProtocolOf (t)));
						scopeandcheck (ProtocolAddr (t));
#if 0
fprintf (stderr, "scopeandcheck: TYPENODE/CHAN: ASINPUT, scoped protocol subshared = %d, PProtocolOf(t) =", subshared);
printtreenl (stderr, 4, ProtocolOf (t));
#endif
						if (TagOf (ProtocolOf (t)) == N_TYPEDECL) {
							treenode *ntype = ProtocolOf (t);

							SetProtocol (t, newnamenode (TagOf (ntype), LocnOf (ntype), NNameOf (ntype), NTypeOf (ntype),
										NDeclOf (ntype), NLexLevelOf (ntype), NScopeOf (ntype), NModeOf (ntype)));
							SetNTypeAttr (ProtocolOf (t), TypeAttr_marked_in | (TypeAttrOf (t) & TypeAttr_shared) | subshared);
						} else {
							chkerr (CHK_UNEXPECTED_CHANDIR, chklocn);
						}
						return;
					} else if (TagOf (ProtocolOf (t)) == S_ASOUTPUT) {
						const BIT32 subshared = OpTypeAttrOf (ProtocolOf (t)) & TypeAttr_shared;

						SetProtocol (t, OpOf (ProtocolOf (t)));
						scopeandcheck (ProtocolAddr (t));
#if 0
fprintf (stderr, "scopeandcheck: TYPENODE/CHAN: ASOUTPUT, scoped protocol subshared = %d, PProtocolOf(t) =", subshared);
printtreenl (stderr, 4, ProtocolOf (t));
#endif
						if (TagOf (ProtocolOf (t)) == N_TYPEDECL) {
							treenode *ntype = ProtocolOf (t);

							SetProtocol (t, newnamenode (TagOf (ntype), LocnOf (ntype), NNameOf (ntype), NTypeOf (ntype),
										NDeclOf (ntype), NLexLevelOf (ntype), NScopeOf (ntype), NModeOf (ntype)));
							SetNTypeAttr (ProtocolOf (t), TypeAttr_marked_out | (TypeAttrOf (t) & TypeAttr_shared) | subshared);
						} else {
							chkerr (CHK_UNEXPECTED_CHANDIR, chklocn);
						}
						return;
					}
#endif	/* MOBILES */
				case S_PORT:
					tptr = ProtocolAddr (t);
					break;
					/*}}}  */
				#ifdef MOBILES
					/*{{{  mobile */
				case S_MOBILE:
					/* nested MOBILE fixup */
#if 0
fprintf (stderr, "scopeandcheck: *tptr (MOBILE) = ");
printtreenl (stderr, 4, *tptr);
#endif
					/* scope first */
					scopeandcheck (MTypeAddr (t));

					if ((TagOf (MTypeOf (t)) == S_ARRAY) && !ARDimLengthOf (t)) {
						/* MOBILE array of some form */
						treenode *subtype = ARTypeOf (MTypeOf (t));

						if ((TagOf (subtype) == S_ASINPUT) || (TagOf (subtype) == S_ASOUTPUT)) {
							/* which way ? */
							const BOOL was_input = (TagOf (subtype) == S_ASINPUT);
							/* shared ? */
							const BOOL shared = ((OpTypeAttrOf (subtype) & TypeAttr_shared) == TypeAttr_shared);
							/* const int old = switch_to_real_workspace (); */
							treenode *curtype, *typecopy;

							if (TagOf (OpOf (subtype)) == N_TYPEDECL) {
								/* typedecl, sort it */
								const int old = switch_to_real_workspace ();
								curtype = OpOf (subtype);
								typecopy = newnamenode (TagOf (curtype), LocnOf (curtype), NNameOf (curtype), NTypeOf (curtype),
									NDeclOf (curtype), NLexLevelOf (curtype), NScopeOf (curtype), NModeOf (curtype));

								SetNTypeAttr (typecopy, (was_input ? TypeAttr_marked_in : TypeAttr_marked_out) | (shared ? TypeAttr_shared : 0));
								SetARType (MTypeOf (t), typecopy);
								switch_to_prev_workspace (old);
							}
						}
					}
					return;
				case S_NEW_ARRAY:
					scopeandcheck (ARTypeAddr (t));
					if (TypeAttrOf (t) & TypeAttr_aligned) {
						scopeandcheck (ARAlignmentAddr (t));
					}
					tptr = ARDimLengthAddr (t);
					break;
				case S_ALLOC_PROC:
					scopeandcheck (ARTypeAddr (t));
#if 0
fprintf (stderr, "scopeandcheck: S_ALLOC_PROC: ARTypeOf (t) = ");
printtreenl (stderr, 4, ARTypeOf (t));
#endif
					/* type must be a mobile-proc */
					if (ARTypeOf (t) && (TagOf (ARTypeOf (t)) != N_MPROCDECL) && (TagOf (ARTypeOf (t)) != N_LIBMPROCDECL) && (TagOf (ARTypeOf (t)) != S_UNDECLARED)) {
						/* not a mobile process */
						chkreport ((TagOf (ARTypeOf (t)) == N_PROCTYPEDECL) ? CHK_MPD_ALLOC_PROCTYPEDECL : CHK_MPD_ALLOC_NOT_MPROCDECL, chklocn);
					}
					return;
				case S_NEW_BARRIER:
					/* already know what it is! */
					return;
					/*}}}  */
				#endif
				default:
					badtag (chklocn, tag, "scopeandcheck");
				}
				break;
				/*}}}  */
				/*{{{  PROCESSOR */
			case PROCESSORNODE:
				{
					const int old_lexlevel = check_lexlevel;
					permit_hwname (ProcessorExpOf (t), S_NODE, fe_predefname_HARDWARE_NODE);
					scopeandcheck (ProcessorExpAddr (t));
					check_lexlevel++;
					scopeandcheck (ProcessorBodyAddr (t));
					check_lexlevel = old_lexlevel;
					SetProcessorScope (t, current_scope ());
					*tptr = (treenode *) cprocessor (t);
				}
				return;
				/*}}}  */
				/*{{{  NAME */
			case WORDNODE:
				if (tag != S_ASMNAME) {
					wordnode *const n = (wordnode *) t;
					treenode *const nptr = lookupname (n, findname (n));
					SetNUsed (nptr, TRUE);
					*tptr = nptr;

					/*printf("scopeandcheck: name: %s, lexlevel %d, current level %d\n",
					   WNameOf(NNameOf(*tptr)), NLexLevelOf(*tptr), check_lexlevel); */

					if ((TagOf (nptr) == N_LABELDEF) && (NLexLevelOf (nptr) != check_lexlevel)) {
						chkerr (CHK_NONLOCAL_LABEL, chklocn);	/* bug TS/1524 13/12/91 */
					}
				}
				return;
				/*}}}  */
				/*{{{  list */
			case LISTNODE:
				{
					treenode *tt;
					for (tt = t; !EndOfList (tt); tt = NextItem (tt)) {
						scopeandcheck (ThisItemAddr (tt));
					}
				}
				return;
				/*}}}  */
				/*{{{  config */
			case CONFIGNODE:
				if (tag == S_SET)
					/*{{{  allow for hardware names */
					permit_hwname (STDevOf (t), S_NODE, fe_predefname_HARDWARE_NODE);
				/*}}}  */
				scopeandcheck (STDevAddr (t));	/* also MapSourceAddr */
				if (tag == S_MAP)
					/*{{{  allow for hardware names */
				{
					treenode *const name = nameof (ThisItem (MapSourceOf (t)));
					if (basetype (NTypeOf (name)) == S_CHAN)
						permit_hwname (MapDestOf (t), S_ARC, fe_predefname_HARDWARE_ARC);
					else
						permit_hwname (MapDestOf (t), S_NODE, fe_predefname_HARDWARE_NODE);
				}
				/*}}}  */
				if (tag != S_SET)
					scopeandcheck (STAttrNameAddr (t));
				scopeandcheck (STAttrExpAddr (t));
				switch (tag) {
				case S_CONNECT:
					*tptr = cconnect (t);
					break;
				case S_MAP:
					*tptr = cmap (t);
					break;
				case S_SET:
					*tptr = cset (t);
					break;
				}
				return;
				/*}}}  */
			}
#if 0
fprintf (stderr, "scopeandcheck: out: *tptr (decl + body) =");
printtreenl (stderr, 4, *tptr);
#endif
		}
	}
}

/*}}}  */


/*{{{  configcheck*/
#if 1				/*def CONFIG */
/* Checking states:
 S_END  outermost VALs
 S_NETWORK   inside a network description
 S_MAPPING   inside a MAPPING construct
 S_CONFIG    inside a CONFIG construct
 S_PROCESSOR inside a PROCESSOR construct
*/
/*{{{  PRIVATE void invalidconfigconstruct()*/
PRIVATE void invalidconfigconstruct (treenode * tptr)
{
	if (configcheckstate == S_END)
		chkerr (CHK_ILLEGAL_CONFIG_CONSTRUCT, LocnOf (tptr));
	else
		chkerr_s (CHK_ILLEGAL_CONSTRUCT, LocnOf (tptr), tagstring (configcheckstate));
}

/*}}}  */
/*{{{  PRIVATE void changestate()*/
PRIVATE void changestate (treenode * tptr, treenode * new_tptr, int req_state, int new_state)
{
	if (configcheckstate == req_state) {
		DEBUG_MSG (("changestate: from %s to %s\n", tagstring (req_state), tagstring (new_state)));
		configcheckstate = new_state;
		prewalkproctree (new_tptr, configcheck, NULL);
		configcheckstate = req_state;
		DEBUG_MSG (("changestate: back %s to %s\n", tagstring (new_state), tagstring (req_state)));
	} else
		invalidconfigconstruct (tptr);
}

/*}}}  */
/*{{{  PRIVATEPARAM int configcheck()*/
PRIVATEPARAM int configcheck (treenode * tptr, void *voidptr)
{
	/* rscunit has ensured that if we're genuinely at the top level, all
	   we will see are S_VALABBR, S_VALRETYPE, S_TPROTDEF, S_SPROTDEF,
	   S_PROCDEF, S_LFUNCDEF, S_SFUNCDEF, S_NETWORK, S_CONFIG, S_MAPPING
	 */
	DEBUG_MSG (("configcheck: %s\n", itagstring (TagOf (tptr))));
	/*{{{  Some node types are always valid */
	switch (nodetypeoftag (TagOf (tptr))) {
	case CONSTEXPNODE:
	case CONSTTABLENODE:
	case LITNODE:
	case MOPNODE:
	case DOPNODE:
	case SEGMENTNODE:
	case NAMENODE:
		return CONTINUE_WALK;
	default:
		break;
	}
	/*}}}  */
	switch (TagOf (tptr)) {
		/*{{{  default: PROCESSOR only */
	default:
		if (configcheckstate != S_PROCESSOR)
			invalidconfigconstruct (tptr);
		break;
		/*}}}  */
		/*{{{  S_CONFIG */
	case S_CONFIG:
		configcount++;
		if (configcount > 1)
			chkerr_s (CHK_DUPLICATE_CONSTRUCT, LocnOf (tptr), tagstring (S_CONFIG));
		changestate (tptr, DValOf (tptr), S_END, S_CONFIG);
		break;		/* continue after the CONFIG */
		/*}}}  */
		/*{{{  S_NETWORK */
	case S_NETWORK:
		networkcount++;
		if (networkcount > 1)
			chkerr_s (CHK_DUPLICATE_CONSTRUCT, LocnOf (tptr), tagstring (S_NETWORK));
		changestate (tptr, DValOf (tptr), S_END, S_NETWORK);
		break;		/* continue after the NETWORK */
		/*}}}  */
		/*{{{  S_MAPPING */
	case S_MAPPING:
		mappingcount++;
		if (mappingcount > 1)
			chkerr_s (CHK_DUPLICATE_CONSTRUCT, LocnOf (tptr), tagstring (S_NETWORK));
		changestate (tptr, DValOf (tptr), S_END, S_MAPPING);
		break;		/* continue after the MAPPING */
		/*}}}  */
		/*{{{  S_PROCESSOR */
	case S_PROCESSOR:
		configcheckscope = ProcessorScopeOf (tptr);
		changestate (tptr, ProcessorBodyOf (tptr), S_CONFIG, S_PROCESSOR);
		return STOP_WALK;
		/*}}}  */
		/*{{{  NETWORK or MAPPING only */
	case S_DO:
	case S_REPLDO:
	case S_SET:
	case S_MAP:
		if (configcheckstate != S_NETWORK && configcheckstate != S_MAPPING)
			invalidconfigconstruct (tptr);
		break;
		/*}}}  */
		/*{{{  CONFIG or PROCESSOR only */
	case S_PAR:
	case S_REPLPAR:
		if (configcheckstate != S_CONFIG && configcheckstate != S_PROCESSOR)
			invalidconfigconstruct (tptr);
		break;
		/*}}}  */
		/*{{{  toplevel or CONFIG only */
	case S_PLACEON:
#ifdef OCCAM2_5
	case S_TYPEDECL:
#endif
		if (configcheckstate != S_CONFIG && configcheckstate != S_END)
			invalidconfigconstruct (tptr);
		break;
		/*}}}  */
		/*{{{  nowhere except config */
	case S_PLACEDPAR:
	case S_PLACEDREPLPAR:
		if (configcheckstate != S_CONFIG)
			invalidconfigconstruct (tptr);
		break;
		/*}}}  */
		/*{{{  nowhere except network */
	case S_CONNECT:
	case S_RECORDSUB:
		if (configcheckstate != S_NETWORK)
			invalidconfigconstruct (tptr);
		break;
		/*}}}  */
		/*{{{  PINSTANCE */
	case S_PINSTANCE:	/* bug TS/1465 12/11/91 */
		if (((current_fe_data->fe_lang & FE_LANG_NDL) == 0)
		    && (configcheckstate != S_PROCESSOR))
			invalidconfigconstruct (tptr);
		break;
		/*}}}  */
		/*{{{  PROC/FUNCTION */
	case S_PROCDEF:
	case S_LFUNCDEF:
	case S_SFUNCDEF:
#ifdef MOBILES
	case S_MPROCDECL:
#endif
		DEBUG_MSG (("configcheck: PROC/FUNCTION %s (%s)\n", WNameOf (NNameOf (DNameOf (tptr))), itagstring (TagOf (DNameOf (tptr)))));
		if ((current_fe_data->fe_lang & FE_LANG_NDL) != 0) {	/* bug TS/1465 12/11/91 */
			changestate (tptr, DValOf (tptr), configcheckstate, S_NETWORK);
		} else if (separatelycompiled (DNameOf (tptr))) {
			if (configcheckstate == S_NETWORK || configcheckstate == S_MAPPING) {
				invalidconfigconstruct (tptr);
			}
		} else {	/* locally defined */

			if (configcheckstate != S_PROCESSOR) {
				invalidconfigconstruct (tptr);
			}
			prewalkproctree (DValOf (tptr), configcheck, voidptr);
		}
		break;
		/*}}}  */
		/*{{{  declarations */
	case S_DECL:
	case S_ABBR:		/* case S_RETYPE: */
		{
			treenode *name = DNameOf (tptr);
			int type;
			if (TagOf (name) == S_LIST)
				name = ThisItem (name);
			type = basetype (NTypeOf (name));
			if (((configcheckstate == S_END || configcheckstate == S_NETWORK) &&
			     network_datatype (type)) ||
			    ((configcheckstate == S_CONFIG || configcheckstate == S_END) &&
			     (type == S_CHAN)) || ((configcheckstate == S_PROCESSOR) && !network_datatype (type)));
			else
				invalidconfigconstruct (tptr);
		}
		break;
		/*}}}  */
		/*{{{  PLACEMENTs */
	case S_PLACE:
	case S_WSPLACE:
	case S_VSPLACE:
		if (configcheckstate != S_PROCESSOR)
			invalidconfigconstruct (tptr);
		else if (NScopeOf (DNameOf (tptr)) < configcheckscope)
			chkerr_s (CHK_ILLEGAL_PLACE, LocnOf (tptr), WNameOf (NNameOf (DNameOf (tptr))));
		break;
		/*}}}  */
		/*{{{  Always valid */
	case S_VALABBR:
	case S_VALRETYPE:
	case S_TPROTDEF:
	case S_SPROTDEF:
	case S_IF:		/*case S_REPLIF: */
	case S_CHOICE:
	case S_SKIP:
	case S_STOP:
	case S_LIST:
	case S_END:
	case S_ARRAYSUB:
	case S_PRAGMA:		/* bug 829 20/9/91 */
		break;
		/*}}}  */
	}
	return CONTINUE_WALK;
}

/*}}}  */
#endif
/*}}}  */
/*{{{  PUBLIC treenode *chk_lookupname (wordnode *w)*/
PUBLIC treenode *chk_lookupname (wordnode *w)
{
	return lookupname (w, findname (w));
}
/*}}}*/

/*{{{  PUBLIC void scopeandcheck_main()*/
PUBLIC void scopeandcheck_main (treenode ** const tptr, const BOOL allowpredefs, const BOOL toplevel)
{
	jmp_buf savedenv;
	memcpy ((char *) savedenv, (char *) env, sizeof (env));

	if (setjmp (env) == 0) {
		if (allowpredefs) {
			scope_predefs ();
		}
		check_lexlevel = 0;
		scopeandcheck (tptr);
		/*{{{  check for high level semantics */
		if (((current_fe_data->fe_lang & (FE_LANG_NDL | FE_LANG_CONFIG2 | FE_LANG_CONFIG3)) != 0) &&
		    (fe_get_errorcount (current_fe_handle) == 0) && toplevel) {
			configcheckstate = S_END;	/*(compilemode == COMP_PROGRAM ? S_END : S_PROCESSOR); */
			configcount = 0;
			networkcount = 0;
			mappingcount = 0;
			prewalkproctree (*tptr, configcheck, NULL);
			if (configcount == 0 && ((current_fe_data->fe_lang & FE_LANG_NDL) == 0))	/* bug TS/1465 12/11/91 */
				chkerr_s (CHK_ZERO_CONSTRUCT, NOPOSN, tagstring (S_CONFIG));
			if (networkcount == 0 && ((current_fe_data->fe_lang & FE_LANG_CONFIG3) == 0))
				chkerr_s (CHK_ZERO_CONSTRUCT, NOPOSN, tagstring (S_NETWORK));
		}
		/*}}}  */
	}
	memcpy ((char *) env, (char *) savedenv, sizeof (env));

	/* This added to prevent later 'out of memory' errors reporting
	   a silly line number - CON 22/1/91 */
	flocn = NOPOSN;
	chklocn = NOPOSN;

	return;
}

/*}}}  */

/*{{{  PUBLIC void scopeinit ()*/
PUBLIC void scopeinit (void)
{
	/*{{{  initialise real namestack */
	init_namestack (&namestack_main, DEFAULT_NAMESTACK_SIZE_MAIN);
	/*}}}  */
	/*{{{  initialise undeclared namestack */
	init_namestack (&namestack_undeclared, DEFAULT_NAMESTACK_SIZE_UNDEC);
	/*}}}  */
	insidepripar = FALSE;
	current_proc = NULL;
	current_params = NULL;
	/*insideinline = FALSE; */
	init_attributes ();

	return;
}

/*}}}  */
