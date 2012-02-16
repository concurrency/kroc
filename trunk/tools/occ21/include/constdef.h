/* $Id: constdef.h,v 1.1 1996/04/15 10:52:01 djb1 Exp $ */

/*
 *	constant folder definitions
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

void foldconstexp (treenode *tptr,BIT32 *reshi,BIT32 *reslo,int err_msg, SOURCEPOSN err_locn);
treenode *foldexpinto (treenode *tptr, int type);
treenode *newconstexp (treenode *tptr);

int checkbounds(int sourcetype,int desttype,BIT32 shi,BIT32 slo, SOURCEPOSN locn);

BOOL wouldbeconst (treenode *tptr);
BOOL isconst (treenode *tptr);

treenode *foldexp (treenode *tptr);
treenode *foldexplist (treenode *tptr);
void foldtree (treenode *tptr);

BOOL is_evaluable         (treenode *n);
INT32 eval_const_treenode (treenode *n);

#ifdef OCCAM2_5
BOOL isconst_fn(treenode *const tptr);
#endif
