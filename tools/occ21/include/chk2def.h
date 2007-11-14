/* $Id: chk2def.h,v 1.3 1998/09/03 11:58:27 djb1 Exp $ */

/*
 *	chk2 definitions
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

PUBLIC void check_valof_types(treenode *const valof);

PUBLIC treenode *caction  (treenode *tptr);
/* 'volatile' is inserted just to prevent an erroneous gcc warning:
   warning: argument `tptr' may be clobbered by `longjmp' */
PUBLIC treenode *cdeclaration  (treenode *volatile tptr);
PUBLIC treenode *cparmlist(treenode *volatile tptr, int paramtype, SOURCEPOSN locn, BOOL separatelycompiledsc);
PUBLIC treenode *crepl  (treenode *volatile tptr);
PUBLIC treenode *ccase  (treenode *volatile tptr);
PUBLIC treenode *ccond  (treenode *volatile tptr);
PUBLIC treenode *calt   (treenode *volatile tptr);
PUBLIC treenode *cprocessor  (treenode *volatile tptr);
PUBLIC treenode *callocation (treenode *volatile tptr, treenode *last_decl);
PUBLIC treenode *cinstance  (treenode *volatile tptr);
PUBLIC treenode *cvalof  (treenode *volatile tptr, BIT32 scope);
PUBLIC treenode *cguy_or_asm(treenode *volatile tptr, BOOL guy_not_asm, treenode *current_proc);


PUBLIC void checkelement (treenode *tptr, treenode *type, int paramno);
/* PUBLIC void checkasslist ( treenode *lhs , treenode *rhs ); */

