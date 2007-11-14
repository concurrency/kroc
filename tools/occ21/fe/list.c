/* $Id: list.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	list handling
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


/*{{{  include files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include "midinc.h"
/*#include "listhdr.h"*//* included inside "midinc.h" */
/*}}}*/

/*{{{  PUBLIC int listitems (tptr) */
PUBLIC int listitems (treenode * tptr)
{
	int l = 0;
	for (; !EndOfList (tptr); tptr = NextItem (tptr))
		l++;
	return (l);
}

/*}}}*/
/*{{{  PUBLIC treenode *nth_listitem */
PUBLIC treenode *nth_listitem (treenode * list, int n)
{
	while ((n > 0) && !EndOfList (list)) {
		n--;
		list = NextItem (list);
	}
	if (EndOfList (list))
		msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FATAL_ABORT, NOPOSN, "nth_listitem");
	return ThisItem (list);
}

/*}}}*/
/*{{{  PUBLIC treenode *lastnode (list) */
/*****************************************************************************
 *
 *  lastnode returns a pointer to the last node on the list 'list'
 *
 *****************************************************************************/
PUBLIC treenode *lastnode (treenode * list)
{
	if (EmptyList (list))
		return (NULL);
	else {
		while (!(EndOfList (NextItem (list))))
			list = NextItem (list);
		return (list);
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *appendnode (node, list) */
/*****************************************************************************
 *
 *  appendnode appends 'node' to the list 'list'
 *
 *****************************************************************************/
PUBLIC treenode *appendnode (treenode * node, treenode * list)
{
	treenode *const n = newlistnode (S_LIST, NOPOSN, node, NULL);
	if (EmptyList (list))
		return (n);
	else {
		treenode *const l = lastnode (list);
		NewNextItem (n, l);
		return (list);
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *appendlist (newlist, list) */
/*****************************************************************************
 *
 *  appendlist appends the list 'newlist' to the end of list 'list'
 *
 *****************************************************************************/
PUBLIC treenode *appendlist (treenode * newlist, treenode * list)
{
	if (EmptyList (newlist))
		return (list);
	else if (EmptyList (list))
		return (newlist);
	else {
		treenode *l = lastnode (list);
		NewNextItem (newlist, l);
		return (list);
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *insertnode (node, list)      NEVER USED */
#if 0				/* insertnode is never used */
/*****************************************************************************
 *
 *  insertnode inserts node into list
 *
 *****************************************************************************/
PUBLIC treenode *insertnode (treenode * node, treenode * list)
{
	treenode *n = newlistnode (S_LIST, NOPOSN, node, NULL);
	if (EmptyList (list))
		return (n);
	else {
		treenode *nextl = NextItem (list);
		NewNextItem (n, list);
		NewNextItem (nextl, n);
		return (list);
	}
}
#endif /* insertnode is never used */
/*}}}*/
/*{{{  PUBLIC treenode *addtofront (node, list) */
/*****************************************************************************
 *
 *  addtofront makes node the first item of list and returns the new list
 *
 *****************************************************************************/
PUBLIC treenode *addtofront (treenode *node, treenode *list)
{
	return (newlistnode (S_LIST, NOPOSN, node, list));
}

/*}}}*/
/*{{{  PUBLIC treenode *insertlist  (newlist, originallist) */
/*****************************************************************************
 *
 *  insertlist inserts newlist into originallist
 *
 *****************************************************************************/
PUBLIC treenode *insertlist (treenode *newlist, treenode *originallist)
{
	if (EmptyList (originallist))
		return (newlist);
	else if (EmptyList (newlist))
		return (originallist);
	else {
		treenode *nextl = NextItem (originallist);
		NewNextItem (newlist, originallist);
		appendlist (nextl, newlist);
		return (originallist);
	}
}

/*}}}*/
/*{{{  PUBLIC void      reverselist  (list) */
/*****************************************************************************
 *
 *  reverselist simply reverses the list (in place)
 *
 *****************************************************************************/
PUBLIC void reverselist (treenode ** list)
{
	treenode *oldlist = *list;
	treenode *newlist = NULL;
	while (!EmptyList (oldlist)) {
		treenode *const next = NextItem (oldlist);
		NewNextItem (newlist, oldlist);	/* oldlist->next := newlist */
		newlist = oldlist;
		oldlist = next;
	}
	*list = newlist;
}

/*}}}*/
/*{{{  PUBLIC void walklist (p, list, param) */
/*****************************************************************************
 *
 *  walklist walks along a list applying p to each node
 *
 *****************************************************************************/
PUBLIC void walklist (void (*p)(treenode *, void *), treenode *list, void *const voidptr)
{
	if ((list != NULL) && (TagOf (list) == S_LIST))
		for (; !EndOfList (list); list = NextItem (list))
			(*p) (ThisItem (list), voidptr);
}

/*}}}*/
/*{{{  PUBLIC void modwalklist (p, list)             NEVER USED */
#if 0				/* modwalklist is never used */
/*****************************************************************************
 *
 *  modwalklist walks along a list applying p to the address of each node
 *
 *****************************************************************************/
PUBLIC void modwalklist (void (*p)(treenode **, void *), treenode *list, void *const voidptr)
{
	if (list != NULL && TagOf (list) == S_LIST)
		for (; !EndOfList (list); list = NextItem (list))
			(*p) (ThisItemAddr (list), voidptr);
}
#endif /* modwalklist is never used */

/*}}}*/
