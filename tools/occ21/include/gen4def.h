/* $Id: gen4def.h,v 1.1 1996/04/15 10:52:06 djb1 Exp $ */

/*
 *	gen4 declarations
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
int regsfor (treenode *tptr);
/*int regsforaddr (treenode *tptr);*/ /* now PRIVATE */
int regsforstore (treenode *dest);
int regsforopd (int opdmode, treenode *opd);
int regsforaddropd (int opdmode, treenode *opd);
INT32 lshiftcanusebcnt(const INT32 shiftcount);
void tnestedsegments (treenode *tptr, int regs);
/*void tdimensionplusn (treenode *tptr, int n, int regs);*/ /* unused */
/*void tdimension (treenode *tptr, int regs);*/ /* now PRIVATE */
void tcheckdim (treenode *type1, treenode *type2);
void tcheckdimensions (treenode *type1, treenode *type2);
void tdop (int op, int type, treenode *left, treenode *right,
           int regs, BOOL commutative, BOOL signextend_result);
void ttypeconversion (int sourcetype, int desttype);

void texp_constanttable(treenode *tptr, const int inonlocal, const int word);
void texp_main (treenode *tptr, int regs, BOOL signextend_result);
/*void texp    (treenode *tptr, int regs);*/
#define texp(tptr,regs) texp_main(tptr,regs,TRUE)

void texpopd_main (int opdmode, treenode *opd, int regs, BOOL signextend_result);
/*void texpopd (int opdmode, treenode *opd, int regs);*/
#define texpopd(opdmode,opd,regs) texpopd_main(opdmode,opd,regs,TRUE)

void tload2regs (int e1mode, treenode *e1,
                       int e2mode, treenode *e2,
                       BOOL commutative, BOOL signextend_result);
void tload3regs (int e1mode, treenode *e1,
                       int e2mode, treenode *e2,
                       int e3mode, treenode *e3,
                       int loadseq, BOOL signextend_result);
void tstoreregs (treenode **dest[MAXREGS], int nregs);
void t450_divrem_workaround(int op);
/*}}}  */

