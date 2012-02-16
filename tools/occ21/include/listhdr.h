/* $Id: listhdr.h,v 1.1 1996/04/15 10:52:12 djb1 Exp $ */

/*
 *	list processing definitions
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

/*{{{  macros */
#define ThisItem(L) (LeftOf(L))
#define NextItem(L) (RightOf(L))
#define EndOfList(L) ((L) == NULL)
#define EmptyList(L) ((L) == NULL)
#define NewItem(I,L) (SetLeft(L,I))
#define NewNextItem(I,L) (SetRight(L,I))
#define IsLastItem(L) (RightOf(L) == NULL)

#define ThisItemAddr(L) (LeftAddr(L))
#define NextItemAddr(L) (RightAddr(L))

/*}}}*/

/*{{{  function prototypes */
int listitems (treenode *tptr);
treenode *nth_listitem(treenode *list, int n);
treenode *lastnode (treenode *list);
treenode *appendnode (treenode *node,treenode *list);
treenode *appendlist (treenode *newlist,treenode *list);
/*treenode *insertnode (treenode *node,treenode *list);*/ /* unused */
treenode *addtofront (treenode *node,treenode *list);
treenode *insertlist (treenode *newlist,treenode *originallist);
void reverselist (treenode **list);

/*void walklist (void (*p)(treenode *),treenode *list, BIT32 param);*/
void walklist (void (*p)(treenode *, void *), treenode *list, void *);
#if 0 /* never used */
void modwalklist  (void (*p)(treenode **, void *), treenode *list, void *);
#endif
/*}}}*/

/*{{{  side-effects pragmas */
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (listitems)
#pragma IMS_nosideeffects (lastnode)
#endif
/*}}}*/


