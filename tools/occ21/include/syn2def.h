/* $Id: syn2def.h,v 1.4 1998/09/03 11:41:18 djb1 Exp $ */

/*
 *	syn2 definitions
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

/*{{{  routines*/
treenode *rproclist (treenode *(*readitem)(void), const int indent);
treenode *rlist (treenode *(*p)(void), const int sep);
wordnode *rname (void);
treenode *rnamelist (void);
treenode *rsubscript (treenode *a);
treenode *relement (void);
treenode *rexp (void);
/*treenode *rvalabbr (treenode *t,wordnode *name,SOURCEPOSN locn,int indent);*/ /* now PRIVATE */
treenode *rrestofspec (treenode *const t, treenode *namelist, const SOURCEPOSN locn, const int indent, const BOOL check_trailing_indent);
treenode *rspecnameandbody (treenode *const t, const SOURCEPOSN locn, const int indent, const BOOL check_trailing_indent);
/*treenode *rsimpleprotocol (void);*/ /* now PRIVATE */
/*treenode *rtaggedprotocol (void);*/ /* now PRIVATE */
treenode *rspecifier (void);
#ifdef INITIAL_DECL
treenode *rinitialspec(void); /* prototype added by Jim */
#endif
treenode *rresultspec (void); /* prototype added by Fred */
treenode *rfunctiondef ( treenode *typelist , SOURCEPOSN locn , int indent );
treenode *rspecification (void);
treenode *rinstance (int tag, SOURCEPOSN locn, treenode *name);
BOOL rfile   (void);
/*int rpragma (void);*/ /* now PRIVATE */
treenode *rsegment (treenode *const node);
/*treenode *rconstructor (treenode *node);*/ /* now PRIVATE */
BOOL rleadingspecs (treenode **specroot, treenode **exp, const BOOL exprlist_permitted);
BOOL mustbespec (const int s);
/*int mustbeexp (int s);*/ /* now PRIVATE */
/*}}}*/

/*{{{  side-effects pragmas*/
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (mustbespec)
#endif
/*}}}*/


