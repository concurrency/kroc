/* $Id: gen12def.h,v 1.1 1996/04/15 10:52:05 djb1 Exp $ */

/*
 *	gen12 declarations
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

PUBLIC BOOL uses_fpu(treenode *tptr);
PUBLIC void mapfpload2regs (treenode **left, treenode **right);
PUBLIC void mapfpexp (treenode **tptr);
PUBLIC void tfpexp (treenode *tptr, int regs, int fpregs);
PUBLIC void tfpload2regs (treenode *exp1, treenode *exp2,
                                int regs, int fpregs, BOOL op_commutes);
PUBLIC void mapfpassign (int destmode, treenode **dest,
                               int sourcemode, treenode **source);
PUBLIC void tfpassign (int type, int destmode, treenode *dest,
                              int sourcemode, treenode *source, int regs);
