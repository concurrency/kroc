/* $Id: syn1def.h,v 1.2 1998/09/03 11:35:49 djb1 Exp $ */

/*
 *	syn1 definitions
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


/*{{{  global variables */
EXTERN int foundroutine;
/*}}}*/
/*{{{  function headers */
void ignore (const int s);
void ignorecomments (const int i);
BOOL checkfor (const int s);
BOOL checkindent (int i);
void checknewline (void);
BOOL checknlindent (int i);
BOOL checklinebreak (void);

treenode *declare  (const int stype, const SOURCEPOSN locn,treenode *t,wordnode *name,treenode *e);
treenode *declname (int ntype,SOURCEPOSN locn,wordnode *name,treenode *type,treenode *spec);

/*}}}*/

#define synetoken(T) msg_out_synetoken(symb, flocn, (T))
