/* $Id: use1.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	Routines to support usage and alias checkers
 *	Copyright (C) 1987, 1988 Inmos Limited
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

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include "feinc.h"
#include "useerror.h"
#include "predefhd.h"
#include "usehdr.h"
#include "usedef.h"
#include "use1def.h"
#include "use2def.h"
#include "use4def.h"
#include "chkerror.h"
#include "chkdef.h"
#include "trandef.h"
/*}}}*/
/*{{{  global variables */
PUBLIC errorlist *uerrors = NULL;
PUBLIC treenode *enclosing_par = NULL;
/*}}}*/

/*{{{  DEBUG_MSG */
#undef DEBUG_MSG
#ifdef DEBUG
#define DEBUG_MSG(X) { if (current_fe_data->fe_debuguse) printf X; }
#else
#define DEBUG_MSG(X)
#endif
/*}}}*/

/*{{{  PUBLIC void usereport (n, locn, p1) */
/*****************************************************************************
 *
 *  usereport adds an error report to the error list
 *
 *****************************************************************************/
PUBLIC void usereport (const BOOL warning, const int n, const SOURCEPOSN locn, treenode * p1)
{
	errorlist *ptr, **prev;
	prev = &uerrors;
	ptr = *prev;
	/*{{{  search for first error report on same line */
	while ((ptr != NULL) && (ERLocnOf (ptr) < locn)) {
		prev = &ERNextOf (ptr);
		ptr = *prev;
	}
	/*}}} */
	/*{{{  check error has not been reported before - return if it has */
	while ((ptr != NULL) && (ERLocnOf (ptr) == locn)) {
		if ((ERCodeOf (ptr) == n) && (ERP1Of (ptr) == p1))
			return;	/* already reported */
		prev = &ERNextOf (ptr);
		ptr = *prev;
	}
	/*}}} */
	/*{{{  insert new error record */
	DEBUG_MSG (("usereport: ===== Error is generated now =====\n"));
	*prev = (errorlist *) newvec (sizeof (errorlist));
	if (n < 0 || n > 127) {
		err_abort ("usereport");	/* make sure it fits into a char */
	}
	SetERNext (*prev, ptr);
	SetERWarn (*prev, warning);
	SetERCode (*prev, n);
	SetERLocn (*prev, locn);
	SetERP1 (*prev, p1);
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC const char *usetagstring (n) */
#ifdef DEBUG
/*****************************************************************************
 *
 *  return ascii string for specified tag
 *
 *****************************************************************************/
PUBLIC const char *usetagstring (treenode * n)
{
	static char tagstring_s[MAXSTRING_SIZE];
	treenode *name;
	switch (nodetypeoftag (TagOf (n))) {
	default:
		return itagstring (TagOf (n));
		/*{{{  cases */
	case DECLNODE:
		name = DNameOf (n);
		if (TagOf (name) == S_LIST) {
			name = ThisItem (name);
		}
		break;
	case INSTANCENODE:
		name = INameOf (n);
		break;
	case REPLCNODE:
		name = ReplCNameOf (n);
		break;
	case NAMENODE:
		name = n;
		break;
		/*}}} */
	}
	sprintf (tagstring_s, "%s %s", itagstring (TagOf (n)), WNameOf (NNameOf (name)));
	return tagstring_s;
}
#endif
/*}}}*/
/*{{{  PRIVATE void printsublist (slist, name) */
/*****************************************************************************
 *
 *  print list of subscripts accessed by free variable
 *
 *****************************************************************************/
PRIVATE void printsublist (FILE * fptr, subscrlist * slist, const char *const name)
{
	if (slist != NULL) {
		fprintf (fptr, "%s: ", name);
		for (; slist != NULL; slist = SLNextOf (slist))
			if ((SLFirstOf (slist) == 0) && (SLLastOf (slist) == MAXINDEX))
				fprintf (fptr, "all ");
			else
				fprintf (fptr, "%d-%d ", SLFirstOf (slist), SLLastOf (slist));
	}
}

/*}}}*/
/*{{{  PUBLIC void printfreelist (list) */
/*****************************************************************************
 *
 *  Print the free variable list
 *
 *****************************************************************************/
PUBLIC void printfreelist (FILE * fptr, varlist * list)
{
	if (list == NULL)
		fprintf (fptr, "(none)");
	for (; list != NULL; list = VLNextOf (list)) {
		fprintf (fptr, "'%s' (", WNameOf (NNameOf (VLNameOf (list))));
		printsublist (fptr, VLReadOf (list), "read");
		printsublist (fptr, VLWrittenOf (list), "written");
		printsublist (fptr, VLInputOf (list), "input");
		printsublist (fptr, VLOutputOf (list), "output");
		fprintf (fptr, ")");
	}
	fprintf (fptr, "\n");
}

/*}}}*/
/*{{{  PRIVATE BOOL is_free (n, scope) */
/*****************************************************************************
 *
 *  is_free returns TRUE if the name 'n' is declared outside of the
 *  scope 'scope'.
 *
 *****************************************************************************/
PRIVATE BOOL is_free (treenode * n, int scope)
{
	return ((scope < 0) || (NScopeOf (n) < scope));
}

/*}}}*/
/*{{{  PRIVATE subscrlist *allocate_subscr () */
/*****************************************************************************
 *
 *  Allocate a record for storing subscript info
 *  Recycle any previously used records if available
 *
 *****************************************************************************/
PRIVATE subscrlist *allocate_subscr (void)
{
	return newvec (sizeof (subscrlist));
}

/*}}}*/
/*{{{  PRIVATE void release_subscr (subscr_node) */
/*****************************************************************************
 *
 *  Return subscript info record to free list
 *
 *****************************************************************************/
PRIVATE void release_subscr (subscrlist * subscr_node)
{
	freevec (subscr_node, sizeof (subscrlist));
}

/*}}}*/
/*{{{  PRIVATE subscrlist *add_subscr_rec (start, end, link) */
/*****************************************************************************
 *
 *  Allocate and initialise a new subscript info record
 *
 *****************************************************************************/
PRIVATE subscrlist *add_subscr_rec (INT32 start, INT32 end, subscrlist * link)
{
	subscrlist *p;
	p = allocate_subscr ();
	SetSLFirst (p, start);
	SetSLLast (p, end);
	SetSLNext (p, link);
	return (p);
}

/*}}}*/
/*{{{  PRIVATE void add_subscr (vptr, start, end) */
/*****************************************************************************
 *
 *  Add new accesses from 'start' to 'end' to subscript access list 'vptr'
 *
 *****************************************************************************/
PRIVATE void add_subscr (subscrlist ** vptr, INT32 start, INT32 end)
{
	subscrlist **prev, *ptr;
	prev = vptr;
	ptr = *prev;
	while (TRUE) {
		if (ptr == NULL)
			/*{{{  add new record and return */
		{
			*prev = add_subscr_rec (start, end, NULL);
			return;
		}
		/*}}} */
		else if (end < (SLFirstOf (ptr) - 1))
			/*{{{  add new record and return */
		{
			*prev = add_subscr_rec (start, end, ptr);
			return;
		}
		/*}}} */
		else if (end <= SLLastOf (ptr))
			/*{{{  update first and return */
		{
			if (start < SLFirstOf (ptr))
				SetSLFirst (ptr, start);
			return;
		}
		/*}}} */
		else if (start <= (SLLastOf (ptr) + 1))
			/*{{{  update list and return */
		{
			if (start < SLFirstOf (ptr))
				SetSLFirst (ptr, start);
			SetSLLast (ptr, end);
			while ((SLNextOf (ptr) != NULL) && (end >= (SLFirstOf (SLNextOf (ptr)) - 1))) {
				subscrlist *next;
				INT32 next_end;
				next = SLNextOf (ptr);
				next_end = SLLastOf (next);
				if (end < next_end)
					SetSLLast (ptr, next_end);
				SetSLNext (ptr, SLNextOf (next));
				release_subscr (next);
			}
			return;
		}
		/*}}} */
		else
			/*{{{  loop */
		{
			prev = &SLNextOf (ptr);
			ptr = *prev;
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC BOOL subscr_overlaps */
/*****************************************************************************
 *
 *  Test if the range 'start' to 'end' overlaps any subscript accesses in
 *  the list 'sptr'
 *
 *****************************************************************************/
PUBLIC BOOL subscr_overlaps (const subscrlist * sptr, const INT32 start, const INT32 end)
{
	for (; sptr != NULL; sptr = SLNextOf (sptr)) {
		if (end < SLFirstOf (sptr))
			return (FALSE);
		if (start <= SLLastOf (sptr))
			return (TRUE);
	}
	return (FALSE);
}

/*}}}*/
/*{{{  PRIVATE subsclist *merge_subscr_info (from_list, to_list) */
/*****************************************************************************
 *
 *  Merge the accesses in 'from_list' into 'to_list' and return the new list
 *  formed.  'from_list' is not affected.
 *
 *****************************************************************************/
PRIVATE subscrlist *merge_subscr_info (subscrlist * from_list, subscrlist * to_list)
{
	for (; from_list != NULL; from_list = SLNextOf (from_list))
		add_subscr (&to_list, SLFirstOf (from_list), SLLastOf (from_list));
	return (to_list);
}

/*}}}*/
/*{{{  PRIVATE void release_subscripts (subscr) */
/*****************************************************************************
 *
 *  Return all subscript info records in the list 'subscr' to free list
 *
 *****************************************************************************/
PRIVATE void release_subscripts (subscrlist * subscr)
{
	while (subscr != NULL) {
		subscrlist *next_subscr = SLNextOf (subscr);
		release_subscr (subscr);
		subscr = next_subscr;
	}
}

/*}}}*/
/*{{{  PUBLIC abbrevlist *new_abbrev_node (n, next, first, last, loc, ...) */
/*****************************************************************************
 *
 *  Allocate a record for storing abbreviation info
 *  Recycle any previously used records if available
 *  Initialise the record to the values given
 *
 *****************************************************************************/
PUBLIC abbrevlist *new_abbrev_node (treenode * n, abbrevlist * next, INT32 first, INT32 last, SOURCEPOSN locn, treenode * subscripts)
{
	abbrevlist *const list = (abbrevlist *) newvec (sizeof (abbrevlist));
	SetALNext (list, next);
	SetALName (list, n);
	SetALLoc (list, locn);
	SetALFirst (list, first);
	SetALLast (list, last);
	SetALSubscripts (list, subscripts);
	return (list);
}

/*}}}*/
/*{{{  PUBLIC abbrevlist *release_headof_abbrevlist (alist) */
/*****************************************************************************
 *
 *  Return the first abbreviation info record in 'alist' to the free list
 *  and return the next element in the list to the caller
 *
 *****************************************************************************/
PUBLIC abbrevlist *release_headof_abbrevlist (abbrevlist * alist)
{
	abbrevlist *const next = ALNextOf (alist);
	freevec (alist, sizeof (abbrevlist));
	return (next);
}

/*}}}*/
/*{{{  PUBLIC BOOL inabbrevlist */
/*****************************************************************************
 *
 *  inabbrevlist returns TRUE if the namenode 'n' is already in the abbreviated
 *  variable list 'alist'.
 *
 *****************************************************************************/
PUBLIC BOOL inabbrevlist (treenode * const n, const abbrevlist * alist, const INT32 start, const INT32 end)
{
	for (; alist != NULL; alist = ALNextOf (alist))
		if ((ALNameOf (alist) == n) &&
		    ((ALFirstOf (alist) < 0) || (start < 0) || ((start <= ALLastOf (alist)) && (end >= ALFirstOf (alist))))) return (TRUE);
	return (FALSE);
}

/*}}}*/
/*{{{  PRIVATE varlist *new_list_node (n, next) */
/*****************************************************************************
 *
 *  Allocate a record for storing variable access info
 *  Recycle any previously used records if available
 *  Initialise the record
 *
 *****************************************************************************/
PRIVATE varlist *new_list_node (treenode * const n, varlist * const next)
{
	varlist *const list = (varlist *) newvec (sizeof (varlist));

	SetVLName (list, n);
	SetVLNext (list, next);
	SetVLRead (list, NULL);
	SetVLWritten (list, NULL);
	SetVLInput (list, NULL);
	SetVLOutput (list, NULL);

	return (list);
}

/*}}}*/
/*{{{  PUBLIC BOOL invarlist */
/*****************************************************************************
 *
 *  invarlist returns TRUE if the namenode 'n' is already in the free variable
 *  list 'vlist'.
 *
 *****************************************************************************/
PUBLIC BOOL invarlist (treenode * const n, const varlist * vlist, const use_mode_t use_mode, const INT32 start, const INT32 end)
{
	for (; vlist != NULL; vlist = VLNextOf (vlist))
		if (VLNameOf (vlist) == n) {
			subscrlist *s = NULL;	/* initialised to shut up gcc's optimiser */
			switch (use_mode) {
			case EXP_READ:
				s = VLReadOf (vlist);
				break;
			case EXP_WRITTEN:
				s = VLWrittenOf (vlist);
				break;
			case CHAN_INPUT:
			case CHAN_XINPUT:
				s = VLInputOf (vlist);
				break;
			case CHAN_OUTPUT:
				s = VLOutputOf (vlist);
				break;
			}
			return (subscr_overlaps (s, start, end));
		}
	return (FALSE);
}

/*}}}*/

/*{{{  PRIVATE varlist *addtolist (n, vlist, use_mode, start_sub, end_sub) */
/*****************************************************************************
 *
 *  addtolist adds the name 'n' with access range 'start_sub' to 'end_sub'
 *  and usage mode 'use_mode' to 'vlist' and returns the new list.
 *
 *****************************************************************************/
PRIVATE varlist *addtolist (treenode * n, varlist * vlist, const use_mode_t use_mode, const INT32 start_sub, const INT32 end_sub)
{
	varlist *list;
#ifdef DEBUG
	if (current_fe_data->fe_debuguse) {
		fprintf (current_fe_data->fe_outfile, "Adding %s[", WNameOf (NNameOf (n)));
		if (start_sub == 0 && end_sub == MAXINDEX)
			fputs ("all", current_fe_data->fe_outfile);
		else
			fprintf (current_fe_data->fe_outfile, "%ld-%ld", start_sub, end_sub);
		fputs ("] to list ", current_fe_data->fe_outfile);
		switch (use_mode) {
		case EXP_READ:
			fputs ("EXP_READ\n", current_fe_data->fe_outfile);
			break;
		case EXP_WRITTEN:
			fputs ("EXP_WRITTEN\n", current_fe_data->fe_outfile);
			break;
		case CHAN_INPUT:
			fputs ("CHAN_INPUT\n", current_fe_data->fe_outfile);
			break;
		case CHAN_XINPUT:
			fputs ("CHAN_XINPUT\n", current_fe_data->fe_outfile);
			break;
		case CHAN_OUTPUT:
			fputs ("CHAN_OUTPUT\n", current_fe_data->fe_outfile);
			break;
		}
	}
#endif
#if 0
fprintf (stderr, "addtolist: use_mode = %d, start_sub = %d, end_sub = %d, n =", (int)use_mode, (int)start_sub, (int)end_sub);
printtreenl (stderr, 4, n);
#endif
	for (list = vlist; list != NULL; list = VLNextOf (list)) {
		if (VLNameOf (list) == n) {
			switch (use_mode) {
			case EXP_READ:
				add_subscr (&VLReadOf (list), start_sub, end_sub);
				break;
			case EXP_WRITTEN:
				add_subscr (&VLWrittenOf (list), start_sub, end_sub);
				break;
			case CHAN_INPUT:
			case CHAN_XINPUT:
				add_subscr (&VLInputOf (list), start_sub, end_sub);
				break;
			case CHAN_OUTPUT:
				add_subscr (&VLOutputOf (list), start_sub, end_sub);
				break;
			}
			return (vlist);
		}
	}

	list = new_list_node (n, vlist);
	switch (use_mode) {
	case EXP_READ:
		add_subscr (&VLReadOf (list), start_sub, end_sub);
		break;
	case EXP_WRITTEN:
		add_subscr (&VLWrittenOf (list), start_sub, end_sub);
		break;
	case CHAN_INPUT:
	case CHAN_XINPUT:
		add_subscr (&VLInputOf (list), start_sub, end_sub);
		break;
	case CHAN_OUTPUT:
		add_subscr (&VLOutputOf (list), start_sub, end_sub);
		break;
	}
	return (list);
}

/*}}}*/
/*{{{  PRIVATE varlist *release_headof_varlist (vlist) */
/*****************************************************************************
 *
 *  Return first record on variable access list 'vlist' to the free pool
 *  and return next item on list to the caller
 *
 *****************************************************************************/
PRIVATE varlist *release_headof_varlist (varlist * vlist)
{
	varlist *const next = VLNextOf (vlist);
	release_subscripts (VLReadOf (vlist));
	release_subscripts (VLWrittenOf (vlist));
	release_subscripts (VLInputOf (vlist));
	release_subscripts (VLOutputOf (vlist));

	freevec (vlist, sizeof (varlist));
	return (next);
}

/*}}}*/
/*{{{  PUBLIC void release_varlist (vlist) */
/*****************************************************************************
 *
 *  Release all records in variable access list 'vlist'
 *
 *****************************************************************************/
PUBLIC void release_varlist (varlist * vlist)
{
	while (vlist != NULL)
		vlist = release_headof_varlist (vlist);
}

/*}}}*/
/*{{{  PUBLIC varlist *merge (vlist1, scope, vlist2) */
/*****************************************************************************
 *
 *  merge takes two lists of free variables, 'vlist1' and 'vlist2' and a scope
 *  'scope'.  It adds to the front of 'vlist2' all those variables in 'vlist1'
 *  which are not already in 'vlist2' and are outside 'scope'.
 *  The resultant list is returned.
 *  'vlist1' is not changed.
 *
 *****************************************************************************/
PUBLIC varlist *merge (varlist * vlist1, int scope, varlist * vlist2)
{
	for (; vlist1 != NULL; vlist1 = VLNextOf (vlist1)) {
		treenode *n = VLNameOf (vlist1);
		if (is_free (n, scope)) {
			varlist *list = vlist2;
			while ((list != NULL) && (VLNameOf (list) != n))
				list = VLNextOf (list);
			if (list == NULL) {
				vlist2 = new_list_node (n, vlist2);
				list = vlist2;
			}
			SetVLRead (list, merge_subscr_info (VLReadOf (vlist1), VLReadOf (list)));
			SetVLWritten (list, merge_subscr_info (VLWrittenOf (vlist1), VLWrittenOf (list)));
			SetVLInput (list, merge_subscr_info (VLInputOf (vlist1), VLInputOf (list)));
			SetVLOutput (list, merge_subscr_info (VLOutputOf (vlist1), VLOutputOf (list)));
		}
	}
	return (vlist2);
}

/*}}}*/
/*{{{  PRIVATE subscrlist *copy_subscrlist */
PRIVATE subscrlist *copy_subscrlist (subscrlist * old_subscrlist)
{
	subscrlist *n = NULL;
	for (; old_subscrlist != NULL; old_subscrlist = SLNextOf (old_subscrlist))
		n = add_subscr_rec (SLFirstOf (old_subscrlist), SLLastOf (old_subscrlist), n);
	return n;
}

/*}}}*/
/*{{{  PUBLIC void use_copy_free_vars */
PUBLIC void use_copy_free_vars (treenode * const orig_nptr, treenode * const new_nptr)
{
	varlist *vlist;
	varlist *nlist = NULL;

	for (vlist = NFreeVarsOf (orig_nptr); vlist != NULL; vlist = VLNextOf (vlist)) {
		nlist = new_list_node (VLNameOf (vlist), nlist);
		SetVLRead (nlist, copy_subscrlist (VLReadOf (vlist)));
		SetVLWritten (nlist, copy_subscrlist (VLWrittenOf (vlist)));
		SetVLInput (nlist, copy_subscrlist (VLInputOf (vlist)));
		SetVLOutput (nlist, copy_subscrlist (VLOutputOf (vlist)));
	}
	SetNFreeVars (new_nptr, nlist);
}

/*}}}*/
/*{{{  PUBLIC void subscripts_accessed (n, first, last) */
/*****************************************************************************
 *
 *  'n' points to an expression tree containing only subscripts and slices of
 *  an element with compile-time evaluable subscripts.
 *  'first' is set to the subscript of the first accessed element of the
 *  array treated as a single dimensional array and 'last' is set to the last.
 *  if check_subscripts is TRUE, it will report subscript errors
 *  (incase of expanded replicators, etc).
 *
 *****************************************************************************/
PUBLIC void subscripts_accessed (treenode * const n, INT32 * const first, INT32 * const last, const BOOL check_subscripts)
{
	switch (TagOf (n)) {
	case S_RECORDSUB:
		/*{{{  record */
		/*{{{  better attempt */
		{
			subscripts_accessed (ASBaseOf (n), first, last, check_subscripts);
			if (*first < 0);	/* skip */
			else {
				const INT32 subscript = NVOffsetOf (ASIndexOf (n));
				treenode *const type = chk_gettype (ASBaseOf (n));
				const INT32 size = bytesin (type);
				const INT32 sub_size = bytesin (NTypeOf (ASIndexOf (n)));
				if (size < 0) {	/* Configurer types only */
					*first = 0;	/* Don't really know why, */
					*last = sub_size - 1;	/* but this works!        */
				} else {
					const INT32 ncopies = ((*last + 1) - *first) / size;	/* should be 1 */
					const INT32 start = *first + subscript * ncopies;
					const INT32 length = sub_size * ncopies;
					DEBUG_MSG (("subscripts_accessed: recordsub: size:%ld, ncopies;%ld, subscript:%ld, sub_size:%ld\n",
						    size, ncopies, subscript, sub_size));
					*first = start;
					*last = start + (length - 1);
				}
			}
		}
		/*}}} */
		break;
		/*}}} */
	case S_ARRAYSUB:
		/*{{{  subscript */
		{
			subscripts_accessed (ASBaseOf (n), first, last, check_subscripts);
			if (*first < 0);	/* skip */
			else if (!is_evaluable (ASIndexOf (n)))
				*first = (-1);
			else {
				const INT32 subscript = eval_const_treenode (ASIndexOf (n));
				treenode *const type = chk_gettype (ASBaseOf (n));
				DEBUG_MSG (("subscripts_accessed: arraysub; subscript:%ld, ARDim:%ld\n", subscript, ARDimOf (type)));
				if (check_subscripts &&	/* added 16/10/90 for bug 1017 */
				    ((subscript < 0) || (subscript >= ARDimOf (type))))
					/*{{{  subscript error */
				{
					*first = (-1);
					if (check_subscripts)
						chkerr_i (CHK_SUBSCRIPT_RANGE, LocnOf (n), subscript);
				}
				/*}}} */
				else {
					const INT32 size = elementsin (chk_gettype (n));
					if (size == (-1))
						*first = (-1);
					else if (ARDimOf (type) < 0) {	/* added 16/10/90 for bug 1017 */
						*first += (size * subscript);
						*last = *first + size - 1;
					} else {
						const INT32 start = *first + (size * subscript);
						const INT32 length = ((*last + 1) - *first) / ARDimOf (type);
						*first = start;
						*last = start + (length - 1);
					}
				}
			}
			break;
		}
		/*}}} */
	case S_SEGMENT:
		/*{{{  segment */
		{
			subscripts_accessed (SNameOf (n), first, last, check_subscripts);
			if (*first < 0);	/* skip */
			else if (!is_evaluable (SStartExpOf (n)) || !is_evaluable (SLengthExpOf (n)))
				*first = (-1);
			else {
				const INT32 start = eval_const_treenode (SStartExpOf (n));
				const INT32 slength = eval_const_treenode (SLengthExpOf (n));
				treenode *const type = chk_gettype (SNameOf (n));
				if (check_subscripts &&	/* bug 1017 16/10/90 */
				    ((start < 0) || (slength < 0) || ((start + slength) > ARDimOf (type)))) {
					*first = (-1);
					if (check_subscripts) {
						if ((start < 0) || (start > ARDimOf (type)))
							chkerr_i (CHK_SEG_START_RANGE, LocnOf (n), start);
						else if ((slength < 0) || (slength > ARDimOf (type)))
							chkerr_i (CHK_SEG_LENGTH_RANGE, LocnOf (n), slength);
						else
							chkerr_i (CHK_SEG_RANGE, LocnOf (n), start + slength);
					}
				} else if (ARDimOf (type) < 0) {	/* added 16/10/90 bug 1017 */
					*first += start;
					*last = *first + slength - 1;	/* quick and dirty approximation */
				} else {
					const INT32 size = elementsin (type);
					if (size == (-1))
						*first = (-1);
					else if (ARDimOf (type) == 0)	/* bug TS/1771 29/07/92 */
						;	/* leave *first and *last as they are */
					else {
						const INT32 base = *first + ((size / ARDimOf (type)) * start);
						const INT32 length = (((*last + 1) - *first) / ARDimOf (type)) * slength;
						*first = base;
						*last = base + (length - 1);
					}
				}
			}
			break;
		}
		/*}}} */
	default:
		/*{{{  decl node */
		*first = 0;
		*last = elementsin (NTypeOf (n)) - 1;
		if (*last < 0) {
			*last = MAXINDEX;
		}
		break;
		/*}}} */
	}
	DEBUG_MSG (("subscripts_accessed: %s, first:%ld, last:%ld\n", usetagstring (n), *first, *last));
}

/*}}}*/
/*{{{  PUBLIC BOOL name_used_in_exp (treenode *const name, treenode *const tptr)*/
PRIVATE BOOL name_used_in_exp_result;
PRIVATE treenode *name_used_in_exp_name;

PRIVATEPARAM int do_name_used_in_exp (treenode *const tptr, void *const voidptr)
{
	USE_VAR (voidptr);	/* prevent unused name warning */
	if (tptr == name_used_in_exp_name) {
		name_used_in_exp_result = TRUE;
		return STOP_WALK;
	}
	return CONTINUE_WALK;
}

PUBLIC BOOL name_used_in_exp (treenode *const name, treenode *const tptr)
{
	name_used_in_exp_name = name;
	name_used_in_exp_result = FALSE;
	prewalktree (tptr, do_name_used_in_exp, NULL);
	return name_used_in_exp_result;
}

/*}}}*/
/*{{{  PUBLIC int chan_use (treenode *n, treenode *name)*/
PRIVATE treenode *chan_use_name;
PRIVATE int chan_use_result;
/*{{{  PRIVATEPARAM int dochan_use (treenode * n, void *const voidptr)*/
/*****************************************************************************
 *
 *  searches tree 'n' for uses of channel 'name' and returns a value indicating
 *  whether it is unused, used for input, used for output or both
 *
 *****************************************************************************/
PRIVATEPARAM int dochan_use (treenode * n, void *const voidptr)
{
	USE_VAR (voidptr);	/* prevent unused warning */

	DEBUG_MSG (("dochan_use: %s\n", usetagstring (n)));
#if 0
fprintf (stderr, "dochan_use: n is");
printtreenl (stderr, 4, n);
#endif
	switch (TagOf (n)) {
		/*{{{  PINSTANCE */
	case S_PINSTANCE:
		{
			treenode *flist, *formal;
			/*{{{  check free vars */
			if ((TagOf (INameOf (n)) == N_PROCDEF) || (TagOf (INameOf (n)) == N_INLINEPROCDEF) || (TagOf (INameOf (n)) == N_MPROCDECL)) {
				varlist *freevars = NFreeVarsOf (INameOf (n));
				while ((freevars != NULL) && (VLNameOf (freevars) != chan_use_name)) {
					freevars = VLNextOf (freevars);
				}
				if (freevars != NULL) {
					if (VLInputOf (freevars) != NULL) {
						chan_use_result |= CHAN_USE_INPUT;
					}
					if (VLOutputOf (freevars) != NULL) {
						chan_use_result |= CHAN_USE_OUTPUT;
					}
				}
			}
			/*}}} */
			if (TagOf (INameOf (n)) == N_PREDEFPROC) {	/* bug 1035 1/11/90 */
				/*{{{  check params */
				#if 0
				const int pdno = NModeOf (INameOf (n));
				switch (pdno) {
				case PD_LOADINPUTCHANNEL:
				case PD_LOADINPUTCHANNELVECTOR:
				case PD_LOADOUTPUTCHANNEL:
				case PD_LOADOUTPUTCHANNELVECTOR:
					{
						const int usemode = (pdno == PD_LOADINPUTCHANNEL || pdno == PD_LOADINPUTCHANNELVECTOR) ?
							CHAN_USE_INPUT : CHAN_USE_OUTPUT;
						flist = NTypeOf (INameOf (n));
						n = IParamListOf (n);
						while (n != NULL && chan_use_result != CHAN_USE_BIDIRECTIONAL) {
							formal = ThisItem (flist);
#if 0
							if ((TagOf (formal) == N_PARAM) && (nameof (ThisItem (n)) == chan_use_name))
#else /* INSdi02352 */
							if ((TagOf (formal) == N_PARAM) && (name_used_in_exp (chan_use_name, ThisItem (n))))
#endif
								chan_use_result |= usemode;
							flist = NextItem (flist);
							n = NextItem (n);
						}
					}
					break;
				default:
					break;
				}
				#endif
				/*}}} */
			} else {
				/*{{{  check parameters */
				flist = NParamListOf (INameOf (n));
				n = IParamListOf (n);
				while (!EndOfList (flist)) {
					formal = ThisItem (flist);
					if (isnamedformal (formal)) {
#if 0
						if ((TagOf (formal) == N_PARAM) && (nameof (ThisItem (n)) == chan_use_name))
#endif
						if ((TagOf (formal) == N_PARAM) && (name_used_in_exp (chan_use_name, ThisItem (n)))) {
							if (ParamInputOn (formal)) {
								chan_use_result |= CHAN_USE_INPUT;
							}
							if (ParamOutputOn (formal)) {
								chan_use_result |= CHAN_USE_OUTPUT;
							}
						}
						n = NextItem (n);
					}
					flist = NextItem (flist);
				}
				/*}}} */
			}
			return STOP_WALK;
		}
		/*}}} */
		/*{{{  output */
	case S_OUTPUT:
		if (name_used_in_exp (chan_use_name, LHSOf (n))) {
			chan_use_result |= CHAN_USE_OUTPUT;
		}
		return STOP_WALK;
		/*}}} */
		/*{{{  input */
	case S_INPUT:
	case S_TAGGED_INPUT:
	case S_X_INPUT:
	case S_X_TAGGED_INPUT:
	case S_DELAYED_INPUT:
		if (name_used_in_exp (chan_use_name, LHSOf (n))) {
			chan_use_result |= CHAN_USE_INPUT;
		}
		return STOP_WALK;

	case S_CASE_INPUT:
	case S_X_CASE_INPUT:
		if (name_used_in_exp (chan_use_name, LHSOf (n))) {
			chan_use_result |= CHAN_USE_INPUT;
		}
		break;
		/*}}} */
		/*{{{  abbr */
	case S_ABBR:
		{
			const int type = basetype (NTypeOf (DNameOf (n)));
			if (((type == S_CHAN) || (type == S_PORT)) && name_used_in_exp (chan_use_name, DValOf (n)))
#if 0
			{
				treenode *const oldname = chan_use_name;
				chan_use_name = DNameOf (n);
				prewalkproctree (DBodyOf (n), dochan_use, voidptr);
				chan_use_name = oldname;
			}
#else
				chan_use_result |= chan_use (DBodyOf (n), DNameOf (n));
#endif
		}
		n = DBodyOf (n);
		break;
		/*}}} */
	}
	return (chan_use_result == CHAN_USE_BIDIRECTIONAL) ? STOP_WALK : CONTINUE_WALK;
}

/*}}}*/
/*{{{  PUBLIC int chan_use (treenode *n, treenode *name)*/
/*****************************************************************************
 *
 *  searches tree 'n' for uses of channel 'name' and returns a value indicating
 *  whether it is unused, used for input, used for output or both
 *
 *****************************************************************************/
PUBLIC int chan_use (treenode *n, treenode *name)
{
	treenode *const saved_name = chan_use_name;
	const int saved_result = chan_use_result;
	int temp_result;

	chan_use_name = name;
	chan_use_result = CHAN_UNUSED;
	prewalkproctree (n, dochan_use, NULL);
	DEBUG_MSG (("chan_use returns: in?:%d, out?:%d\n", (chan_use_result & CHAN_USE_INPUT) != 0, (chan_use_result & CHAN_USE_OUTPUT) != 0));
	temp_result = chan_use_result;

	chan_use_name = saved_name;
	chan_use_result = saved_result;

	return temp_result;
}

/*}}}*/
/*}}}*/

/*{{{  PUBLIC BOOL barrier_extended_or_defined (treenode *const bname, treenode *const epar, const BOOL chk_extended, const BOOL chk_defined)*/
/*****************************************************************************
 * this checks to see if a barrier is defined and/or extended by an
 * enclosing PAR or PAR replicator
 *****************************************************************************/
PUBLIC BOOL barrier_extended_or_defined (treenode *const bname, treenode *const epar, const BOOL chk_extended, const BOOL chk_defined)
{
	treenode *ctemp;
	treenode *declbars, *extbars;

	if (!bname || !epar || ((nodetypeoftag (TagOf (epar)) != CNODE) && (nodetypeoftag (TagOf (epar)) != REPLCNODE))) {
		return FALSE;
	}
	if (nodetypeoftag (TagOf (epar)) == CNODE) {
		ctemp = CTempOf (epar);
	} else {
		ctemp = ReplCTempOf (epar);
	}
	if (!ctemp) {
		/* enclosing PAR, but it doesn't do anything barrier-ish */
		return FALSE;
	}
	if (TagOf (ctemp) == S_BAREXTEND) {
		declbars = LeftOpOf (ctemp);
		extbars = RightOpOf (ctemp);
	} else if (TagOf (ctemp) == S_EXTENDS) {
		declbars = NULL;
		extbars = ctemp;
	} else {
		declbars = ctemp;
		extbars = NULL;
	}

	if (chk_extended && extbars && (TagOf (extbars) == S_EXTENDS)) {
		/* check for barrier extension */
		extbars = OpOf (extbars);

		if (TagOf (extbars) == S_LIST) {
			treenode *walk;

			for (walk = extbars; !EndOfList (walk); walk = NextItem (walk)) {
				treenode *thisbar = ThisItem (walk);

				if (thisbar == bname) {
					return TRUE;
				}
			}
		} else if (extbars == bname) {
			return TRUE;
		}
	}

	if (chk_defined && declbars) {
		/* check for barrier declaration */

		if (TagOf (declbars) == S_LIST) {
			treenode *walk;

			for (walk = declbars; !EndOfList (walk); walk = NextItem (walk)) {
				treenode *thisbar = ThisItem (walk);

				if (thisbar == bname) {
					return TRUE;
				}
			}
		} else if (declbars == bname) {
			return TRUE;
		}
	}
	return FALSE;
}
/*}}}*/

/*{{{  forward references */
FORWARD PRIVATEPARAM int do_freevarsin (treenode * n, void *);
FORWARD PRIVATEPARAM void do_freevarsinexptree (treenode * n, void *);
/*}}}*/
/*{{{  PRIVATE variables */
PRIVATE BOOL freevarsin_usage_check;
PRIVATE int freevarsin_scope;
PRIVATE int freevarsin_use_mode;
PRIVATE varlist *freevarsin_vlist;
/*}}}*/
/*{{{  PRIVATE int chan_direction_of (treenode *tptr)*/
/*
 *	returns the associated channel-direction of something
 */
PRIVATE int chan_direction_of (treenode *tptr)
{
	int invert = 0;
	treenode *orig_tptr = tptr;
	int done = 0;

	while (!done) {
		switch (TagOf (tptr)) {
		case N_DECL:
		case N_PARAM:
		case N_RESULTPARAM:
		case N_ABBR:
		case N_RETYPE:
			tptr = NTypeOf (tptr);
			done = 1;
			break;
		case S_RECORDSUB:
		case S_RECORDITEM:
#if 0
fprintf (stderr, "chan_direction_of: checking recordsub/item, base is:");
printtreenl (stderr, 4, ASBaseOf (tptr));
#endif
			if (isdynmobilechantype (ASBaseOf (tptr))) {
				/* subscripting a mobile channel */
				treenode *basetype = chk_gettype_main (ASBaseOf (tptr), TRUE);

				/* basetype should be the type-declaration */
#if 0
fprintf (stderr, "chan_direction_of: it's a mobile channel-type! basetype =");
printtreenl (stderr, 4, basetype);
/*
if (nodetypeoftag (TagOf (ASBaseOf (tptr))) == NAMENODE) {
fprintf (stderr, "chan_direction_of: NTypeAttrOf (ASBaseOf (tptr)) = %d, NTypeAttrOf (NTypeOf (ASBaseOf (tptr))) = %d\n", NTypeAttrOf (ASBaseOf (tptr)),
		NTypeAttrOf (NTypeOf (ASBaseOf (tptr))));
}
*/
#endif
				if (TagOf (basetype) != N_TYPEDECL) {
					msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, LocnOf (tptr), "chan_direction_of -- chan-type not TYPEDECL");
				} else if (NTypeAttrOf (basetype) & TypeAttr_marked_out) {
					/* client-end of channel type, invert */
					invert = TypeAttr_marked_in | TypeAttr_marked_out;
				}
			}
			tptr = chk_gettype (orig_tptr);
			done = 1;
			break;
		case S_ARRAYSUB:
		case S_ARRAYITEM:
			tptr = chk_gettype (orig_tptr);
			done = 1;
			break;
		case S_SEGMENT:
		case S_SEGMENTITEM:
			tptr = chk_gettype (orig_tptr);
			done = 1;
			break;
		case S_CONSTRUCTOR:
			/* try and find one that has got direction-specified fields */
			{
				treenode *litlist = LitExpOf (tptr);

#if 0
fprintf (stderr, "chan_direction_of: CONSTRUCTOR, litlist = ");
printtreenl (stderr, 4, litlist);
#endif
				while (!EndOfList (litlist)) {
					treenode *item = ThisItem (litlist);

					litlist = NextItem (litlist);
					if ((TagOf (item) == S_RECORDSUB) || (TagOf (item) == S_RECORDITEM)) {
						/* use this one */
						tptr = item;
						break;		/* while() */
					}
				}
				if (EndOfList (litlist)) {
					tptr = chk_gettype (orig_tptr);
					done = 1;
				}
			}
			break;
		default:
			done = 1;
			break;
		}
	}

#if 0
fprintf (stderr, "chan_direction_of: tptr now: ");
printtreenl (stderr, 4, tptr);
#endif
	while (TagOf (tptr) == S_ARRAY) {
		tptr = ARTypeOf (tptr);
	}
	if (TagOf (tptr) == S_NULLARRAY) {
		return 0;
	}
	if (isdynmobilechantypetype (tptr)) {
		return 0;
	}
	if (TagOf (tptr) != S_CHAN) {
		msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, LocnOf (tptr), "chan_direction_of -- not channel");
	} else {
		return (TypeAttrOf (tptr) ^ invert);
	}
	return 0;
}
/*}}}*/
/*{{{  PRIVATEPARAM int do_freevarsinexp (n) */
/*****************************************************************************
 *
 *  freevarsinexp takes a treenode, 'n', a scope level, 'scope', a list of
 *  free variables, 'vlist', and a usage mode 'use_mode'.
 *  It adds to 'vlist' any variables used in tree 'n' of scope less than
 *  'scope', which aren't already in 'vlist'.
 *
 *****************************************************************************/
PRIVATEPARAM int do_freevarsinexp (treenode * n, void *voidptr)
{
#if 0
fprintf (stderr, "do_freevarsinexp: n =");
printtreenl (stderr, 4, n);
#endif
	if (n != NULL) {
		DEBUG_MSG (("do_freevarsinexp: %s\n", usetagstring (n)));
		switch (TagOf (n)) {
			/*{{{  FINSTANCE */
		case S_FINSTANCE:
			{
				treenode *flist, *formal;
				const int old_mode = freevarsin_use_mode;
				varlist *freevars = NFreeVarsOf (INameOf (n));
				freevarsin_use_mode = EXP_READ;
				freevarsin_vlist = merge (freevars, freevarsin_scope, freevarsin_vlist);
				/*{{{  check parameters */
				flist = NParamListOf (INameOf (n));
				n = IParamListOf (n);
				while (!EndOfList (flist)) {
					formal = ThisItem (flist);
					if (isnamedformal (formal)) {
						prewalktree (ThisItem (n), do_freevarsinexp, voidptr);
						n = NextItem (n);
					}
					flist = NextItem (flist);
				}
				/*}}} */
				freevarsin_use_mode = old_mode;
			}
			break;
			/*}}} */
			/*{{{  subscripting / slicing */
		case S_ARRAYSUB:
		case S_SEGMENT:
		case S_RECORDSUB:
			{
				treenode *const init_n = n;
				BOOL all_subs_constant = TRUE;
				const int old_mode = freevarsin_use_mode;
				freevarsin_use_mode = EXP_READ;
				/*{{{  check subscripts */
				{
					BOOL looping = TRUE;
					while (looping) {
						switch (TagOf (n)) {
						default:
							looping = FALSE;
							break;
						case S_ARRAYSUB:
							/*{{{  subscript */
							all_subs_constant &= is_evaluable (ASIndexOf (n));
							prewalktree (ASIndexOf (n), do_freevarsinexp, voidptr);
							n = ASBaseOf (n);
							break;
							/*}}} */
						case S_RECORDSUB:	/* bug INSdi01984 */
							/*{{{  record */
							n = ASBaseOf (n);
							break;
							/*}}} */
						case S_SEGMENT:
							/*{{{  segment */
							all_subs_constant &= is_evaluable (SStartExpOf (n)) && is_evaluable (SLengthExpOf (n));
							prewalktree (SStartExpOf (n), do_freevarsinexp, voidptr);
							prewalktree (SLengthExpOf (n), do_freevarsinexp, voidptr);
							n = SNameOf (n);
							break;
							/*}}} */
						}
					}
				}
				/*}}} */
				freevarsin_use_mode = old_mode;
				switch (TagOf (n)) {
					/*{{{  default : */
				default:
/*if (!(freevarsin_usage_check && (basetype(NTypeOf(n)) == S_TIMER))) *//* de Morgan's law */
					/* bug INSdi01282 - always check - even for TIMERs */
					{
						INT32 start, end = (-1);
						if (all_subs_constant /* && (elementsin(NTypeOf(n)) >= 0) */ )	/* bug 646 and 1380 17/10/91 */
							subscripts_accessed (init_n, &start, &end, FALSE);
						else
							start = (-1);
						DEBUG_MSG (
							   ("do_freevarsinexp; arraysub, start:%ld, end:%ld, scope:%d\n", start, end,
							    freevarsin_scope));
						if (is_free (n, freevarsin_scope)) {
							if (start >= 0)
								freevarsin_vlist = addtolist (n, freevarsin_vlist, freevarsin_use_mode, start, end);
							else
								freevarsin_vlist = addtolist (n, freevarsin_vlist, freevarsin_use_mode, 0, MAXINDEX);
						}
					}
					break;
					/*}}} */
					/*{{{  case S_CONSTRUCTOR : case S_ARRAYCONSTRUCTOR : */
				case S_CONSTRUCTOR:
#ifdef OCCAM2_5
				case S_FINSTANCE:
#endif
					do_freevarsinexptree (n, voidptr);
					break;
					/*}}} */
					/*{{{  case S_STRING : case S_CONSTCONSTRUCTOR */
				case S_STRING:
				case S_CONSTCONSTRUCTOR:
					break;
					/*}}} */
				}
				return STOP_WALK;
			}
			/*}}} */
			/*{{{  name node */
		case N_VALABBR:
		case N_ABBR:
		case N_VALRETYPE:
		case N_RETYPE:
		case N_DECL:
		case N_VALPARAM:
		case N_PARAM:
		case N_RESULTPARAM:
		case N_REPL:
			/* Whole variable is being used */
			/*if (!freevarsin_usage_check || basetype(NTypeOf(n)) != S_TIMER) */
			/* bug INSdi01282 - always check - even for TIMERs */
			if (is_free (n, freevarsin_scope)) {
				freevarsin_vlist = addtolist (n, freevarsin_vlist, freevarsin_use_mode, 0, MAXINDEX);
			}
			/* if replicator, set NameUsed */
			if (TagOf (n) == N_REPL) {
				SetNVNameUsed (n, TRUE);
			}
			return STOP_WALK;
			/*}}} */
			/*{{{  specification / VALOF */
		case S_ABBR:
		case S_RETYPE:
		case S_VALABBR:
		case S_VALRETYPE:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_DECL:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_VALOF:
		CASE_CONFIG_SPEC case S_PRAGMA:	/* bug 829 20/9/91 */
		case S_TYPEDECL:
#if MOBILES
		case S_FORWDECL:
		case S_PROCTYPEDECL:
		case S_MPROCDECL:
#endif
			prewalkproctree (n, do_freevarsin, voidptr);
			return STOP_WALK;
			/*}}} */
		}
	}
	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PRIVATEPARAM void do_freevarsinexptree(n) */
PRIVATEPARAM void do_freevarsinexptree (treenode * n, void *const voidptr)
{
	prewalktree (n, do_freevarsinexp, voidptr);
}

/*}}}*/
/*{{{  PRIVATE int do_freevarsin_if */
PRIVATE int do_freevarsin_if (treenode * tptr, void *const voidptr)
/*
  Interprets an IF construct.
  Returns TRUE if a branch was chosen
*/
{
	DEBUG_MSG (("do_freevarsin_if: %s\n", usetagstring (tptr)));
	tptr = CBodyOf (tptr);
	while (!EndOfList (tptr)) {
		treenode *t = ThisItem (tptr);
		while (isspecification (t)) {
			if (do_freevarsin (t, voidptr) != CONTINUE_WALK) {
				tptr = NextItem (tptr);	/* point at next guard */
				if (EndOfList (tptr))
					return FALSE;
				t = ThisItem (tptr);
			} else
				t = DBodyOf (t);
		}
		DEBUG_MSG (("do_freevarsin_if: (after spec): %s\n", usetagstring (t)));
		if (TagOf (t) == S_CHOICE) {
			prewalktree (CondGuardOf (t), do_freevarsinexp, voidptr);
			if (is_evaluable (CondGuardOf (t))) {	/* we can check intelligently */
				if (eval_const_treenode (CondGuardOf (t)) != 0) {
					DEBUG_MSG (("do_freevarsin_if: evaluable guard (TRUE)\n"));
					prewalkproctree (CondBodyOf (t), do_freevarsin, voidptr);
					return TRUE;
				}
				DEBUG_MSG (("do_freevarsin_if: evaluable guard (FALSE)\n"));
			} else {	/* process this guard anyway */

				DEBUG_MSG (("do_freevarsin_if: unevaluable guard\n"));
				prewalkproctree (CondBodyOf (t), do_freevarsin, voidptr);
				/* and drop through to next guard */
			}
		} else if (TagOf (t) == S_REPLIF) {
			/* don't attempt intelligent processing of a replicated if */
			prewalkproctree (t, do_freevarsin, voidptr);
			/* and drop through to next guard */
		} else {	/* S_IF */

			if (do_freevarsin_if (t, voidptr))
				return TRUE;
		}
		tptr = NextItem (tptr);	/* point at next guard */
	}
	return FALSE;
}

/*}}}*/
/*{{{  PRIVATEPARAM int *do_freevarsin (n) */
/*****************************************************************************
 *
 *  do_freevarsin takes a treenode, 'n', a scope level, 'freevarsin_scope',
 *  and a list of free variables, 'freevarsin_vlist',
 *  and adds to 'freevarsin_vlist' any variables used in tree
 *  'n' of freevarsin_scope less than 'scope',
 *  which aren't already in freevarsin_freevarsin_vlist.
 *
 *****************************************************************************/
PRIVATEPARAM int do_freevarsin (treenode * n, void *const voidptr)
{
	/* Only used while checking which vars are written by a CASE_INPUT */
	static BOOL vars_modified_by_case_input = FALSE;	/* yes, I MEAN static */

	const int old_mode = freevarsin_use_mode;
	freevarsin_use_mode = EXP_READ;
	DEBUG_MSG (("do_freevarsin: %s\n", usetagstring (n)));
	switch (TagOf (n)) {
		/*{{{  IF */
	case S_IF:
		if (freevarsin_usage_check) {
			(void) do_freevarsin_if (n, voidptr);
			return STOP_WALK;
		}
		return CONTINUE_WALK;
		/*}}} */
		/*{{{  PAR PRIPAR PLACEDPAR */
	case S_PAR:
	case S_PRIPAR:
	case S_PLACEDPAR:
	case S_DO:
		{
			treenode *saved_enclosing_par = enclosing_par;
			treenode *bar = CTempOf (n);
			treenode *extbars = NULL;
#if 0
fprintf (stderr, "do_freevarsin: PAR: freevarsin_usage_check = %d\n", (int)freevarsin_usage_check);
#endif

			if (bar && (TagOf (bar) == S_BAREXTEND)) {
				extbars = RightOpOf (bar);
			} else if (bar && (TagOf (bar) == S_EXTENDS)) {
				extbars = bar;
			}

			/* check for barrier EXTENDs */
			if (extbars && (TagOf (extbars) == S_EXTENDS)) {
				const int saved_mode = freevarsin_use_mode;

				extbars = OpOf (extbars);

				if (!enclosing_par || ((nodetypeoftag (TagOf (enclosing_par)) == CNODE) && !CTempOf (enclosing_par)) ||
						((nodetypeoftag (TagOf (enclosing_par)) == REPLCNODE) && !ReplCTempOf (enclosing_par))) {
					/* no enclosing PAR, or doesn't deal with extensions */
					freevarsin_use_mode = EXP_WRITTEN;
					if (TagOf (extbars) == S_LIST) {
						treenode *walk;

						for (walk=extbars; !EndOfList (walk); walk = NextItem (walk)) {
							prewalktree (ThisItem (walk), do_freevarsinexp, voidptr);
						}
					} else {
						prewalktree (extbars, do_freevarsinexp, voidptr);
					}
				} else if (TagOf (extbars) == S_LIST) {
					/* list of extended barriers */
					treenode *walk;

					for (walk=extbars; !EndOfList (walk); walk = NextItem (walk)) {
						treenode *thisbar = ThisItem (walk);

						if (!barrier_extended_or_defined (thisbar, enclosing_par, TRUE, TRUE)) {
							freevarsin_use_mode = EXP_WRITTEN;
							prewalktree (thisbar, do_freevarsinexp, voidptr);
						}
					}
				} else if (barrier_extended_or_defined (extbars, enclosing_par, TRUE, TRUE)) {
					/* skip */
				} else {
					freevarsin_use_mode = EXP_WRITTEN;
					prewalktree (extbars, do_freevarsinexp, voidptr);
				}
				
				freevarsin_use_mode = saved_mode;
			}

			enclosing_par = n;
			if (freevarsin_usage_check) {
				freevarsin_vlist = use_freevarsin (n, freevarsin_scope, freevarsin_vlist);
			} else {
				prewalkproctree (CBodyOf (n), do_freevarsin, voidptr);
			}


			enclosing_par = saved_enclosing_par;
			return STOP_WALK;
		}
		/*}}} */
		/*{{{  REPLPAR PRIREPLPAR PLACEDREPLPAR */
	case S_REPLPAR:
	case S_PRIREPLPAR:
	case S_PLACEDREPLPAR:
	case S_REPLDO:
		{
			treenode *saved_enclosing_par = enclosing_par;
			treenode *bar = ReplCTempOf (n);
			treenode *extbars = NULL;

			if (bar && (TagOf (bar) == S_BAREXTEND)) {
				extbars = RightOpOf (bar);
			} else if (bar && (TagOf (bar) == S_EXTENDS)) {
				extbars = bar;
			}

			/* check for barrier EXTENDs */
			if (extbars && (TagOf (extbars) == S_EXTENDS)) {
				const int saved_mode = freevarsin_use_mode;

				extbars = OpOf (extbars);

				if (!enclosing_par || ((nodetypeoftag (TagOf (enclosing_par)) == CNODE) && !CTempOf (enclosing_par)) ||
						((nodetypeoftag (TagOf (enclosing_par)) == REPLCNODE) && !ReplCTempOf (enclosing_par))) {
					/* no enclosing PAR, or doesn't deal with extensions */
					freevarsin_use_mode = EXP_WRITTEN;
					if (TagOf (extbars) == S_LIST) {
						treenode *walk;

						for (walk=extbars; !EndOfList (walk); walk = NextItem (walk)) {
							prewalktree (ThisItem (walk), do_freevarsinexp, voidptr);
						}
					} else {
						prewalktree (extbars, do_freevarsinexp, voidptr);
					}
				} else if (TagOf (extbars) == S_LIST) {
					/* list of extended barriers */
					treenode *walk;

					for (walk=extbars; !EndOfList (walk); walk = NextItem (walk)) {
						treenode *thisbar = ThisItem (walk);

						if (!barrier_extended_or_defined (thisbar, enclosing_par, TRUE, TRUE)) {
							freevarsin_use_mode = EXP_WRITTEN;
							prewalktree (thisbar, do_freevarsinexp, voidptr);
						}
					}
				} else if (barrier_extended_or_defined (extbars, enclosing_par, TRUE, TRUE)) {
					/* skip */
				} else {
					freevarsin_use_mode = EXP_WRITTEN;
					prewalktree (extbars, do_freevarsinexp, voidptr);
				}
				
				freevarsin_use_mode = saved_mode;
			}

			if (freevarsin_scope < 0) {
				freevarsin_scope = NScopeOf (ReplCNameOf (n));
				DEBUG_MSG (("do_freevarsin: setting freevarsin_scope to %d\n", freevarsin_scope));
			}

			enclosing_par = n;
#if defined(CONFIG2) || defined(CONFIG3)
			/*{{{  Nicks version of what should happen */
			freevarsin_vlist = freevarsinexp (ReplCStartExpOf (n), freevarsin_scope, freevarsin_vlist, EXP_READ, freevarsin_usage_check);
			freevarsin_vlist = freevarsinexp (ReplCLengthExpOf (n), freevarsin_scope, freevarsin_vlist, EXP_READ, freevarsin_usage_check);
			if (ReplCStepExpOf (n)) {
				freevarsin_vlist = freevarsinexp (ReplCStepExpOf (n), freevarsin_scope, freevarsin_vlist, EXP_READ, freevarsin_usage_check);
			}

			if (		/* freevarsin_usage_check             && */
				   is_evaluable (ReplCStartExpOf (n)) &&
				   is_evaluable (ReplCLengthExpOf (n)) && ((ReplCStepExpOf (n) == NULL) ? TRUE : is_evaluable (ReplCStepExpOf (n)))) {
				/*{{{  base & count both known */
				{
					INT32 base = eval_const_treenode (ReplCStartExpOf (n));
					INT32 count = eval_const_treenode (ReplCLengthExpOf (n));
					INT32 step;
					if (ReplCStepExpOf (n) != NULL) {
						step = eval_const_treenode (ReplCStepExpOf (n));
					} else {
						step = 1;
					}
					treenode *name = (treenode *) ReplCNameOf (n);
					n = ReplCBodyOf (n);
					SetNReplKnown (name, TRUE);
					while (count > 0) {
						SetNReplValue (name, base);
						prewalkproctree (n, do_freevarsin, voidptr);
						base += step;
						count--;
					}
					SetNReplKnown (name, FALSE);
				}
				/*}}} */
				enclosing_par = saved_enclosing_par;
				return STOP_WALK;
			} else {
				SetNReplKnown (ReplCNameOf (n), FALSE);
				prewalkproctree (ReplCBodyOf (n), do_freevarsin, voidptr);

				enclosing_par = saved_enclosing_par;
				return STOP_WALK;
			}
			/*}}} */
#else
			/*{{{  The old version left for occam compiler */
			if (freevarsin_usage_check) {
				freevarsin_vlist = use_freevarsin (n, freevarsin_scope, freevarsin_vlist);
				enclosing_par = saved_enclosing_par;
				return STOP_WALK;
			}
			/*}}} */
#endif
		}
		break;
		/*}}} */
		/*{{{  REPLSEQ REPLIF REPLALT PRIREPLALT */
	case S_REPLSEQ:
	case S_REPLIF:
	case S_REPLALT:
	case S_PRIREPLALT:
		if (freevarsin_scope < 0) {
			freevarsin_scope = NScopeOf (ReplCNameOf (n));
			DEBUG_MSG (("do_freevarsin: setting freevarsin_scope to %d\n", freevarsin_scope));
		}
		freevarsin_vlist = freevarsinexp (ReplCStartExpOf (n), freevarsin_scope, freevarsin_vlist, EXP_READ, freevarsin_usage_check);
		freevarsin_vlist = freevarsinexp (ReplCLengthExpOf (n), freevarsin_scope, freevarsin_vlist, EXP_READ, freevarsin_usage_check);
		if (ReplCStepExpOf (n)) {
			freevarsin_vlist = freevarsinexp (ReplCStepExpOf (n), freevarsin_scope, freevarsin_vlist, EXP_READ, freevarsin_usage_check);
		}

		if (freevarsin_usage_check &&
		    is_evaluable (ReplCStartExpOf (n)) &&
		    is_evaluable (ReplCLengthExpOf (n)) && ((ReplCStepExpOf (n) == NULL) ? TRUE : is_evaluable (ReplCStepExpOf (n))))
			/* Added by CO'N 1/3/90 for bug 55 (ie to speed things up) */
/* && goes_par(ReplCBodyOf(n)) *//* removed again for bug 1025 */
		{
			/*{{{  base & count (&step if there) known & body goes PAR */
			{
				INT32 base = eval_const_treenode (ReplCStartExpOf (n));
				INT32 count = eval_const_treenode (ReplCLengthExpOf (n));
				INT32 step;
				treenode *name = (treenode *) ReplCNameOf (n);
				if (ReplCStepExpOf (n) != NULL) {
					step = eval_const_treenode (ReplCStepExpOf (n));
				} else {
					step = 1;
				}
				n = ReplCBodyOf (n);
				SetNReplKnown (name, TRUE);
				while (count > 0) {
					SetNReplValue (name, base);
					prewalkproctree (n, do_freevarsin, voidptr);
					base += step;
					count--;
				}
				SetNReplKnown (name, FALSE);
			}
			/*}}} */
			return STOP_WALK;
		} else {
			SetNReplKnown (ReplCNameOf (n), FALSE);
			return CONTINUE_WALK;
		}
		/*}}} */
		/*{{{  SYNC*/
	case S_SYNC:
		{
			treenode *bar = LeafLinkOf (n);

#if 0
fprintf (stderr, "do_freevarsin: SYNC: LeafLinkOf (n) = ");
printtreenl (stderr, 4, LeafLinkOf (n));
#endif
			if (check_isfullbarrier (bar)) {
				/* check to see if enclosing PAR defines or extends the barrier */
				if (bar && barrier_extended_or_defined (bar, enclosing_par, TRUE, TRUE)) {
					/* yes, it does -- SKIP */
				} else {
					/* no, it doesn't */
					freevarsin_use_mode = EXP_WRITTEN;
					prewalktree (bar, do_freevarsinexp, voidptr);
				}
			} else {
				freevarsin_use_mode = EXP_WRITTEN;
				prewalktree (bar, do_freevarsinexp, voidptr);
			}

			freevarsin_use_mode = old_mode;
		}
		return CONTINUE_WALK;
		/*}}}*/
		/*{{{  PINSTANCE */
	case S_PINSTANCE:
		{
			treenode *flist, *formal;
			varlist *freevars = NFreeVarsOf (INameOf (n));
			#if 0
			BOOL is_predefproc = TagOf (INameOf (n)) == N_PREDEFPROC;
			#endif
			SOURCEPOSN errlocn = LocnOf (n);

			/* pre-defs, etc. don't have free variable lists so ... */
			if ((TagOf (INameOf (n)) == N_PROCDEF) || (TagOf (INameOf (n)) == N_INLINEPROCDEF) || udv_thingisdynmobileproctype (INameOf (n))) {
				freevarsin_vlist = merge (freevars, freevarsin_scope, freevarsin_vlist);
			}

			/* updated (frmb): access to a non-local FORK barrier is handled in the back-end now */
#if 0
			/* if forked, check to see if there's a local FORKING node */
			if (forked) {
				treenode *forknode = IForkOf (n);

				if (forknode) {
					/* if !forknode, global FORK */
					freevarsin_use_mode = EXP_WRITTEN;
					prewalktree (DNameOf (CBodyOf (forknode)), do_freevarsinexp, voidptr);
				}
			}
#endif
			if (udv_thingisdynmobileproctype (INameOf (n))) {
				/* name is a free variable! */
				freevarsin_use_mode = EXP_READ;
				prewalktree (INameOf (n), do_freevarsinexp, voidptr);
			}
			#if 0
			if (is_predefproc &&
			    (NModeOf (INameOf (n)) == PD_LOADINPUTCHANNEL ||
			     NModeOf (INameOf (n)) == PD_LOADINPUTCHANNELVECTOR ||
			     NModeOf (INameOf (n)) == PD_LOADOUTPUTCHANNEL || NModeOf (INameOf (n)) == PD_LOADOUTPUTCHANNELVECTOR))
				/* bug 1035 1/11/90 */
			{
				/*{{{  check params */
				const int pdno = NModeOf (INameOf (n));
				freevarsin_use_mode = (pdno == PD_LOADINPUTCHANNEL || pdno == PD_LOADINPUTCHANNELVECTOR) ? CHAN_INPUT : CHAN_OUTPUT;
				flist = NTypeOf (INameOf (n));
				n = IParamListOf (n);
				while (n != NULL) {
					int type;

					formal = ThisItem (flist);
					type = basetype (NTypeOf (formal));
					if ((type == S_CHAN) || (type == S_PORT)) {
						prewalktree (ThisItem (n), do_freevarsinexp, voidptr);
					}
					flist = NextItem (flist);
					n = NextItem (n);
				}
				/*}}} */
			} else 
			#endif
			{
				/*{{{  check parameters */
				flist = NParamListOf (INameOf (n));
				n = IParamListOf (n);
				while (!EndOfList (flist)) {
					formal = ThisItem (flist);
					if (isnamedformal (formal)) {	/* bug INSdi01935 31/03/93 */
						const int type = basetype (NTypeOf (formal));
						if ((type == S_CHAN) || (type == S_PORT)) {
							/*{{{  input/output */
							const BOOL input = ParamInputOn (formal);
							const BOOL output = ParamOutputOn (formal);
							treenode *f_basetype = basetype_tree (NTypeOf (formal));
							treenode *a_basetype = ThisItem (n);
							int dirattrs = 0;

							switch (TagOf (a_basetype)) {
							case N_DECL:
							case N_PARAM:
							case N_ABBR:
							case N_RETYPE:
								a_basetype = NTypeOf (a_basetype);
								break;
							}
							a_basetype = basetype_tree (a_basetype);

#if 0
fprintf (stderr, "checking channel in PINSTANCE, actual is:");
printtreenl (stderr, 4, ThisItem (n));
fprintf (stderr, "formal is:");
printtreenl (stderr, 4, formal);
fprintf (stderr, "formal type-tree is:");
printtreenl (stderr, 4, f_basetype);
fprintf (stderr, "actual type-tree is:");
printtreenl (stderr, 4, a_basetype);
fprintf (stderr, "actual gettype() is:");
printtreenl (stderr, 4, a_type);
fprintf (stderr, "chan_direction_of (ThisItem (n)) = %d\n", chan_direction_of (ThisItem (n)));
#endif
							dirattrs = chan_direction_of (ThisItem (n));

							if ((((TypeAttrOf (f_basetype) | dirattrs) & (TypeAttr_marked_in | TypeAttr_marked_out)) ==
								(TypeAttr_marked_in | TypeAttr_marked_out))) {
								/* conflict between direction specifiers */
								usereport (FALSE, USE_BAD_CHAN_CONFLICT, errlocn, ThisItem (n));
#if 0
fprintf (stderr, "error reported!\n");
#endif
							} else if (input || output) {
								if (input) {
#if 0
fprintf (stderr, "input\n");
#endif
									freevarsin_use_mode = CHAN_INPUT;
									if (dirattrs & TypeAttr_marked_out) {
										usereport (FALSE, USE_BAD_CHAN_OUT, errlocn, ThisItem (n));
									}
									prewalktree (ThisItem (n), do_freevarsinexp, voidptr);
								}
								if (output) {
#if 0
fprintf (stderr, "output\n");
#endif
									freevarsin_use_mode = CHAN_OUTPUT;
									if (dirattrs & TypeAttr_marked_in) {
										usereport (FALSE, USE_BAD_CHAN_IN, errlocn, ThisItem (n));
									}
									prewalktree (ThisItem (n), do_freevarsinexp, voidptr);
								}
							} else
								/*{{{  NOT USED add to EXP_READ list */
							{
								freevarsin_use_mode = EXP_READ;
								prewalktree (ThisItem (n), do_freevarsinexp, voidptr);
							}
							/*}}} */
							/*}}} */
						} else {
							int docheck = 1;

							/*{{{  read/written */
							if (check_isfullbarrier (ThisItem (n))) {
								treenode *bar = ThisItem (n);

#if 0
fprintf (stderr, "do_freevarsin: PINSTANCE: fullbarier param, bar = ");
printtreenl (stderr, 4, bar);
#endif
								if (!enclosing_par || ((nodetypeoftag (TagOf (enclosing_par)) == CNODE) && !CTempOf (enclosing_par)) ||
										((nodetypeoftag (TagOf (enclosing_par)) == REPLCNODE) && !ReplCTempOf (enclosing_par))) {
									freevarsin_use_mode = EXP_WRITTEN;
								} else if (barrier_extended_or_defined (bar, enclosing_par, TRUE, TRUE)) {
									/* skip */
									docheck = 0;
								} else {
									freevarsin_use_mode = EXP_WRITTEN;
								}
							} else {
								switch (type) {
								CASE_CONFIG_TYPE	/* bug INSdi01988 08/04/93 */
									freevarsin_use_mode = EXP_READ;
									break;
								default:
									if (TagOf (formal) == N_VALPARAM) {
										freevarsin_use_mode = EXP_READ;
									} else {
#ifdef MOBILES
										if (TagOf (ThisItem (n)) == S_CLONE) {
											freevarsin_use_mode = EXP_READ;
										} else {
											freevarsin_use_mode = EXP_WRITTEN;
										}
#else
										freevarsin_use_mode = EXP_WRITTEN;
#endif
									}
									break;
								}
							}

							if (docheck) {
								prewalktree (ThisItem (n), do_freevarsinexp, voidptr);
							}
							/*}}} */
						}
						n = NextItem (n);
					}
					flist = NextItem (flist);
				}
				/*}}} */
			}
		}
		freevarsin_use_mode = old_mode;
		return CONTINUE_WALK;
		/*}}} */
		/*{{{  ASS CASE OUTPUT INPUT TAGGED_INPUT X_INPUT X_TAGGED_INPUT DELAYED_INPUT */
	case S_ASS:
	case S_OUTPUT:
	case S_INPUT:
	case S_TAGGED_INPUT:
	case S_DELAYED_INPUT:	/* case S_CASE_INPUT: */
	case S_X_INPUT:
	case S_X_TAGGED_INPUT:
		{
			int rightmode = EXP_READ;
			switch (TagOf (n)) {
			case S_ASS:
				freevarsin_use_mode = EXP_WRITTEN;
				break;
			case S_OUTPUT:
				freevarsin_use_mode = CHAN_OUTPUT;
				/*{{{  check that the channel isn't marked for input */
				{
					treenode *c_type, *chanptr;
					int is_array = 0;

					chanptr = LHSOf (n);
					while (TagOf (chanptr) == S_ARRAYSUB) {
						chanptr = ASBaseOf (chanptr);
						is_array++;
					}
					if ((TagOf (chanptr) == N_ABBR) || (TagOf (chanptr) == N_PARAM)) {
						c_type = NTypeOf (chanptr);
	
						while (TagOf (c_type) == S_ARRAY) {
							c_type = ARTypeOf (c_type);
						}
						if (TypeAttrOf (c_type) & TypeAttr_marked_in) {
							if ((TagOf (chanptr) == N_PARAM) && is_array) {
								usereport (FALSE, USE_BAD_CHAN_ARRAY_PARAM_IN, LocnOf (n), chanptr);
							} else if (TagOf (chanptr) == N_PARAM) {
								usereport (FALSE, USE_BAD_CHAN_PARAM_IN, LocnOf (n), chanptr);
							} else if (is_array) {
								usereport (FALSE, USE_BAD_CHAN_ARRAY_ABBR_IN, LocnOf (n), chanptr);
							} else {
								usereport (FALSE, USE_BAD_CHAN_ABBR_IN, LocnOf (n), chanptr);
							}
						}
					}
				}
				/*}}}  */
				break;
			case S_DELAYED_INPUT:
				freevarsin_use_mode = CHAN_INPUT;
				break;
			case S_INPUT:
			case S_TAGGED_INPUT:	/* case S_CASE_INPUT: */
			case S_X_INPUT:
			case S_X_TAGGED_INPUT:
				freevarsin_use_mode = CHAN_INPUT;
				rightmode = EXP_WRITTEN;
				/*{{{  check that the channel isn't marked for output */
				{
					treenode *c_type, *chanptr;
					int is_array = 0;

					chanptr = LHSOf (n);
					while (TagOf (chanptr) == S_ARRAYSUB) {
						chanptr = ASBaseOf (chanptr);
						is_array++;
					}
#if 0
fprintf (stderr, "checking input, chanptr is");
printtreenl (stderr, 4, chanptr);
#endif
					if (((TagOf (chanptr) == N_ABBR) || (TagOf (chanptr) == N_PARAM)) && (basetype (chanptr) == S_CHAN)) {
						c_type = NTypeOf (chanptr);

						while (TagOf (c_type) == S_ARRAY) {
							c_type = ARTypeOf (c_type);
						}
						if (TypeAttrOf (c_type) & TypeAttr_marked_out) {
							if ((TagOf (chanptr) == N_PARAM) && is_array) {
								usereport (FALSE, USE_BAD_CHAN_ARRAY_PARAM_OUT, LocnOf (n), chanptr);
							} else if (TagOf (chanptr) == N_PARAM) {
								usereport (FALSE, USE_BAD_CHAN_PARAM_OUT, LocnOf (n), chanptr);
							} else if (is_array) {
								usereport (FALSE, USE_BAD_CHAN_ARRAY_ABBR_OUT, LocnOf (n), chanptr);
							} else {
								usereport (FALSE, USE_BAD_CHAN_ABBR_OUT, LocnOf (n), chanptr);
							}
						}
					}
				}
				/*}}}  */
				break;
			}
#if 0
fprintf (stderr, "calling prewalktree (do_freevarsinexp) freevarsin_use_mode=%d on ", freevarsin_use_mode);
printtreenl (stderr, 4, LHSOf(n));
#endif
			prewalktree (LHSOf (n), do_freevarsinexp, voidptr);
#ifdef MOBILES
			/* safe to do this ;) -- already type-checked */
			switch (TagOf (n)) {
			case S_ASS:
			case S_OUTPUT:
				if ((TagOf (follow_user_type (chk_gettype (LHSOf(n)))) == S_MOBILE) &&
					(TagOf (follow_user_type (chk_gettype (RHSOf(n)))) == S_MOBILE)) {
					/* mobile assignment, RHS is written */
					rightmode = EXP_WRITTEN;
				}
				break;
			}
#endif
			freevarsin_use_mode = rightmode;
#if 0
fprintf (stderr, "calling prewalktree (do_freevarsinexp) freevarsin_use_mode=%d on ", freevarsin_use_mode);
printtreenl (stderr, 4, RHSOf(n));
#endif
			prewalktree (RHSOf (n), do_freevarsinexp, voidptr);
			freevarsin_use_mode = old_mode;
		}
		if (freevarsin_usage_check &&	/* bug 1183 19/3/91 */
		    ((TagOf (n) == S_INPUT) || (TagOf (n) == S_TAGGED_INPUT) || (TagOf (n) == S_ASS) ||	/* bug INSdi02237 */
		    (TagOf (n) == S_X_INPUT) || (TagOf (n) == S_X_TAGGED_INPUT))) {
			freevarsin_vlist = use_freevarsin (n, freevarsin_scope, freevarsin_vlist);
		}
		return CONTINUE_WALK;
		/*}}} */
		/*{{{  CASE_INPUT X_CASE_INPUT */
	case S_CASE_INPUT:
	case S_X_CASE_INPUT:
		{
			treenode *tptr;
			freevarsin_use_mode = CHAN_INPUT;
			prewalktree (LHSOf (n), do_freevarsinexp, voidptr);

			/* Added a walk over rhs of the input, BUT NOT the 'bodies' */
			freevarsin_use_mode = EXP_WRITTEN;
			for (tptr = RHSOf (n); !EndOfList (tptr); tptr = NextItem (tptr)) {
				/* We allow for the fact that the variant may be preceded by
				   specifications, by setting this flag.
				   Inside here, we just want to look at the input part, not the
				   attached processes. */

				int saved = vars_modified_by_case_input;
				vars_modified_by_case_input = TRUE;
				prewalktree (ThisItem (tptr), do_freevarsin, voidptr);
				vars_modified_by_case_input = saved;
			}

			freevarsin_use_mode = old_mode;

			if (freevarsin_usage_check)	/* bug 1183 19/3/91 */
				freevarsin_vlist = use_freevarsin (n, freevarsin_scope, freevarsin_vlist);
		}
		return CONTINUE_WALK;
		/*}}} */
		/*{{{  VARIANT X_VARIANT */
	case S_VARIANT:
	case S_X_VARIANT:
		/* Added 24/4/90 by CO'N for bug 289 */
		walklist (do_freevarsinexptree, VRTaggedListOf (n), voidptr);
		if (vars_modified_by_case_input)
			return STOP_WALK;	/* Don't look at the 'body' of the variant */
		return CONTINUE_WALK;
		/*}}} */
		/*{{{  abbr / retype */
	case S_ABBR:
	case S_RETYPE:
		{
			BOOL pretend_val = FALSE;
			const int type = basetype (NTypeOf (DNameOf (n)));
			if (freevarsin_scope < 0) {
				freevarsin_scope = NScopeOf (DNameOf (n));
				DEBUG_MSG (("do_freevarsin: setting freevarsin_scope to %d\n", freevarsin_scope));
			}
			switch (type) {
			default:
				/*{{{  WRITTEN */
				freevarsin_use_mode = EXP_WRITTEN;
				prewalktree (DValOf (n), do_freevarsinexp, voidptr);
				break;
				/*}}} */
			case S_CHAN:
			case S_PORT:
				/*{{{  INPUT / OUTPUT */
				{
					const int use = chan_use (DBodyOf (n), DNameOf (n));

#if 0
fprintf (stderr, "abbr/retype, DNameOf(n) =");
printtreenl (stderr, 4, DNameOf (n));
fprintf (stderr, " ... DValOf(n) =");
printtreenl (stderr, 4, DValOf (n));
#endif
					/* propagate marked channel in/out type attributes
					 * -- do this here, seen before in/out is checked
					 */
					/*{{{  frmb */
					{
						treenode *c_type = DValOf (n);
						treenode *t_type = NTypeOf (DNameOf (n));

						while (TagOf (c_type) == S_ARRAYSUB) {
							c_type = ASBaseOf (c_type);
						}
#if 0
fprintf (stderr, "c_type now");
printtreenl (stderr, 4, c_type);
#endif
						switch (TagOf (c_type)) {
						case N_PARAM:
						case N_ABBR:
							c_type = NTypeOf (c_type);
							while (TagOf (c_type) == S_ARRAY) {
								c_type = ARTypeOf (c_type);
							}
							if (TypeAttrOf (c_type) & (TypeAttr_marked_in | TypeAttr_marked_out)) {
								/* check for conflicts when propagating */
								if ((TypeAttrOf (c_type) & (TypeAttr_marked_in | TypeAttr_marked_out)) ^
									(TypeAttrOf (t_type) & (TypeAttr_marked_in | TypeAttr_marked_out))) {

									usereport (FALSE, USE_BAD_CHAN_CONFLICT, LocnOf (n), DNameOf (n));
									/* error.. */
								}
#if 0
fprintf (stderr, "prop type attr!\n");
#endif
								SetTypeAttr (t_type, (TypeAttrOf (t_type) | TypeAttrOf (c_type)));
							}
							break;
						}
					}
					/*}}}  */
					if ((use & CHAN_USE_INPUT) != 0)
						/*{{{  INPUT */
					{
						freevarsin_use_mode = CHAN_INPUT;
						prewalktree (DValOf (n), do_freevarsinexp, voidptr);
					}
					/*}}} */
					if ((use & CHAN_USE_OUTPUT) != 0)
						/*{{{  OUTPUT */
					{
						freevarsin_use_mode = CHAN_OUTPUT;
						prewalktree (DValOf (n), do_freevarsinexp, voidptr);
					}
					/*}}} */
					if (use == CHAN_UNUSED)
						/*{{{  NOT USED add to EXP_READ list */
					{
						freevarsin_use_mode = EXP_READ;
						prewalktree (DValOf (n), do_freevarsinexp, voidptr);
					}
					/*}}} */
				}
				break;
				/*}}} */
				CASE_CONFIG_TYPE	/* bug INSdi10988 08/04/93 */
					/*{{{  READ - same as VAL abbreviation */
					pretend_val = TRUE;
				break;
				/*}}} */
			}
			if (!pretend_val) {
				freevarsin_use_mode = old_mode;
				return CONTINUE_WALK;
			}
		}
		break;
		/*}}} */
		/*{{{  valabbr / valretype */
	case S_VALABBR:
	case S_VALRETYPE:
		if (freevarsin_scope < 0) {
			freevarsin_scope = NScopeOf (DNameOf (n));
			DEBUG_MSG (("do_freevarsin: setting freevarsin_scope to %d\n", freevarsin_scope));
		}
		break;
		/*}}} */
		/*{{{  others */
	case S_DECL:
	case S_TPROTDEF:
	case S_SPROTDEF:
	case S_PROCDEF:
	case S_SFUNCDEF:
	case S_LFUNCDEF:
	case S_TYPEDECL:
#if MOBILES
	case S_FORWDECL:
	case S_PROCTYPEDECL:
	case S_MPROCDECL:
#endif
		if (freevarsin_scope < 0) {
			if (TagOf (DNameOf (n)) == S_LIST) {
				freevarsin_scope = NScopeOf (ThisItem (DNameOf (n)));
			} else {
				freevarsin_scope = NScopeOf (DNameOf (n));
			}
			DEBUG_MSG (("do_freevarsin: setting freevarsin_scope to %d\n", freevarsin_scope));
		}
		return CONTINUE_WALK;
	case S_PRAGMA:		/* bug 829 20/9/91 */
		return CONTINUE_WALK;	/* INSdi03370 */
		/*}}} */
	}
	applytoexp (n, do_freevarsinexptree, voidptr);
	freevarsin_use_mode = old_mode;
	return CONTINUE_WALK;
}

/*}}}*/

/*{{{  PUBLIC varlist *freevarsinexp (n, scope, vlist, use_mode, usage_check) */
/*****************************************************************************
 *
 *  freevarsinexp takes a treenode, 'n', a scope level, 'scope', a list of
 *  free variables, 'vlist', and a usage mode 'use_mode'.
 *  It adds to 'vlist' any variables used in tree 'n' of scope less than
 *  'scope', which aren't already in 'vlist'.
 *
 *****************************************************************************/
PUBLIC varlist *freevarsinexp (treenode * n, const int scope, varlist * vlist, const use_mode_t use_mode, const BOOL usage_check)
{
	const int old_freevarsin_scope = freevarsin_scope;
	const int old_freevarsin_use_mode = freevarsin_use_mode;
	const BOOL old_freevarsin_usage_check = freevarsin_usage_check;
	varlist *const old_freevarsin_vlist = freevarsin_vlist;

	freevarsin_scope = scope;
	DEBUG_MSG (("freevarsinexp: setting freevarsin_scope to %d\n", freevarsin_scope));
	freevarsin_vlist = vlist;
	freevarsin_use_mode = use_mode;
	freevarsin_usage_check = usage_check;

	DEBUG_MSG (("freevarsinexp\n"));
	prewalktree (n, do_freevarsinexp, NULL);

	vlist = freevarsin_vlist;
	freevarsin_scope = old_freevarsin_scope;
	freevarsin_vlist = old_freevarsin_vlist;
	freevarsin_use_mode = old_freevarsin_use_mode;
	freevarsin_usage_check = old_freevarsin_usage_check;
	return vlist;
}

/*}}}*/
/*{{{  PUBLIC varlist *freevarsin (n, scope, vlist, usage_check) */
/*****************************************************************************
 *
 *  freevarsin takes a treenode, 'n', a scope level, 'scope', and a list of
 *  free variables, 'vlist', and adds to 'vlist' any variables used in tree
 *  'n' of scope less than 'scope', which aren't already in vlist.
 *
 *****************************************************************************/
PUBLIC varlist *freevarsin (treenode * const n, const int scope, varlist * vlist, const BOOL usage_check)
{
	const int old_freevarsin_scope = freevarsin_scope;
	varlist *const old_freevarsin_vlist = freevarsin_vlist;
	const BOOL old_freevarsin_usage_check = freevarsin_usage_check;
	freevarsin_scope = scope;
	freevarsin_vlist = vlist;
	freevarsin_usage_check = usage_check;

#if 0
fprintf (stderr, "use1: freevarsin on:");
printtreenl (stderr, 4, n);
#endif
	DEBUG_MSG (("freevarsin\n"));
	prewalkproctree (n, do_freevarsin, NULL);

	vlist = freevarsin_vlist;
	freevarsin_scope = old_freevarsin_scope;
	freevarsin_vlist = old_freevarsin_vlist;
	freevarsin_usage_check = old_freevarsin_usage_check;
	return (vlist);
}

/*}}}*/
