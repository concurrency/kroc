/* $Id: gen5def.h,v 1.1 1996/04/15 10:52:06 djb1 Exp $ */

/*
 *	gen5 declarations
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

/*{{{  routines */
void mapmovelopd (int destmode, treenode **dest,
                 int sourcemode, treenode **source);
void movelopd (int destmode, treenode *dest,
              int sourcemode, treenode *source);
void trellop (int op, int type, treenode *left, treenode *right,
             int sense, int genbool);
void maprellop (int op, int type, treenode **left, treenode **right);
int addresslopd (int opdmode, treenode *opd);
int mapaddresslopd (int opdmode, treenode **opd, int *freetempopd);
void mapmoveqopd (int destmode, treenode **dest,
                  int sourcemode, treenode **source);
void moveqopd (int destmode, treenode *dest,
               int sourcemode, treenode *source);
BOOL temp_required_for_fn_dest(treenode *dest, treenode *instance);

/*}}}*/
