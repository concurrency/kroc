/* $Id: use1def.h,v 1.1 1996/04/15 10:52:23 djb1 Exp $ */

/*
 *	use1 definitions
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

/*{{{  variables */
extern errorlist *uerrors;
/*}}}*/

/*{{{  function headers */
void usereport       (BOOL warning, int n, BIT32 locn, treenode *p1);
const char *usetagstring(treenode *n);
BOOL subscr_overlaps (const subscrlist *sptr, INT32 start, INT32 end);
BOOL inabbrevlist    (treenode *n, const abbrevlist *alist,
                      INT32 start, INT32 end);
BOOL invarlist       (treenode *n, const varlist *vlist, use_mode_t use_mode,
                      INT32 start, INT32 end);
void release_varlist (varlist *vlist);
varlist *freevarsinexp(treenode *n, int scope, varlist *vlist,
                       use_mode_t use_mode, int usage_check);
abbrevlist *new_abbrev_node (treenode *n, abbrevlist *next, INT32 first, INT32 last ,
                             SOURCEPOSN locn, treenode *subscripts );
abbrevlist *release_headof_abbrevlist(abbrevlist *alist);
/*}}}*/

/*{{{  side-effects pragmas */
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (subscr_overlaps)
#pragma IMS_nosideeffects (inabbrevlist)
#pragma IMS_nosideeffects (invarlist)
#endif
/*}}}*/


