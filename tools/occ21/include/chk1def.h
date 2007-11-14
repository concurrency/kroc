/* $Id: chk1def.h,v 1.2 1998/09/03 12:00:30 djb1 Exp $ */

/*
 *	chk1 definitions
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
extern SOURCEPOSN chklocn;
extern treenode *undeclaredp;
extern BOOL chk_permit_lonely_fields;

extern treenode *intnodeptr;
extern treenode *boolnodeptr;
extern treenode *channodeptr;
extern treenode *unknownnodeptr;
extern treenode *nodenodeptr;
extern treenode *edgenodeptr;
extern treenode *arcnodeptr;

/*}}}*/

/*{{{  routines */
extern treenode *checksame (treenode *t1, treenode *t2, treenode *default_type, const int e, const char *const s);
extern treenode *ctype (treenode *const tptr);
extern treenode *cspecifier (treenode *const tptr);
#ifdef OCCAM2_5
extern treenode *cretypes (treenode *const tptr, const BOOL reshapes);
#endif
extern void checkparams (treenode *tptr, const int mode);

extern BOOL sametype (const int tag1, const int tag2);
extern BOOL protocolinherits (treenode *child, treenode *ancestor);
extern BOOL spec_into_type (treenode *specptr,treenode *typeptr);
extern BOOL typesequivalent (treenode *ftype,treenode *atype, const BOOL match_any);

extern treenode *typecheck (treenode *const tptr, treenode *const default_type);

extern void check_adjacent_name(const SOURCEPOSN locn, treenode *nptr, treenode *const last_decl, treenode *const current_params, const err_severity_t severity);

extern BOOL check_isfullbarrier (treenode *tptr);
extern BOOL check_resignedfullbarrier (treenode *tptr);
extern void check_resignbarrier (treenode *tptr);
extern void check_unresignbarrier (treenode *tptr);

/*}}}*/


