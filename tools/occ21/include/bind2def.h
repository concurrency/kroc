/* $Id: bind2def.h,v 1.1 1996/04/15 10:51:58 djb1 Exp $ */

/*
 *	bind2 declarations
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
#ifdef MOBILES
void getprocwsandvsandms (treenode *nptr, INT32 *ws, INT32 *vs, INT32 *ms);
#endif
void placeintable (treenode *cptr);
void getprocwsandvs (treenode *nptr, INT32 *ws, INT32 *vs);
void mapbool (treenode **tptr);
void mapexplist (treenode *tptr);
void mapexp (treenode **tptr);
void mapaddr (treenode **tptr);
void mapexpopd (int mode, treenode **opd);
void mapstoreinopd (int mode, treenode **opd);
int mapload2regs (int e1mode, treenode **e1,int e2mode, treenode **e2);
int giveorder (int e1mode, treenode *e1,
                     int e2mode, treenode *e2,
                     int e3mode, treenode *e3,
                     BOOL *preeval_e2, BOOL *preeval_e3);
int mapload3regs (int e1mode, treenode **e1,
                        int e2mode, treenode **e2,
                        int e3mode, treenode **e3);
void mapstoreregs (treenode **dest[MAXREGS], int nregs);

/*}}}*/
