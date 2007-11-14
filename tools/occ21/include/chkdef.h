/* $Id: chkdef.h,v 1.1 1996/04/15 10:52:00 djb1 Exp $ */

/*
 *	chk externally visible definition
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


/*{{{  external variables */

extern       fe_handle_t  *current_fe_handle;
extern const fe_data_t    *current_fe_data;
/*}}}*/

/*{{{  useful type checking functions */
/*treenode *gettype_main (treenode *tptr);*/
/*treenode *gettype (treenode *tptr);*/
extern treenode *chk_gettype_main(treenode *tptr, BOOL orig_type);
extern treenode *chk_gettype(treenode *tptr);
extern int chk_typeof (treenode *tptr);
extern BOOL ismobileprocvar (treenode *nptr);		/* chk4.c */
extern treenode *chk_lookupname (wordnode *w);		/* chk4.c */

extern BOOL check_isfullbarrier (treenode *tptr);
extern BOOL check_resignedfullbarrier (treenode *tptr);
extern void check_resignbarrier (treenode *tptr);
extern void check_unresignbarrier (treenode *tptr);

/*}}}*/

/*{{{  access to type checker */
extern void scopeandcheck_main (treenode **tptr, BOOL allowpredefs, BOOL toplevel);

extern void printundeclarednames (FILE *file);

/*}}}*/
/*{{{  initialisation functions */
extern void scopeinit (void);
extern void chkinit   (void);
/*}}}*/

