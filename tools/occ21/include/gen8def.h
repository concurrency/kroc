/* $Id: gen8def.h,v 1.1 1996/04/15 10:52:07 djb1 Exp $ */

/*
 *	gen8 declarations
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


/*treenode *hiddenparamsof (treenode *aparam,treenode *fparam);*/
treenode *augmentparams (treenode *aptr, treenode *fptr, treenode *destlist, treenode *instance);
void augmentformals  (treenode *nptr, int current_lexlevel);		/* in trans */
void maproutinename(treenode *nptr, treenode *tptr, BOOL set_wssize, BOOL set_vssize, int ptype);
void mapinstance (treenode *tptr);
INT32 get_altadjust(treenode *tptr);
void tinstance (treenode *tptr);
void mapsuspend (treenode *tptr);
void tsuspend (treenode *tptr);
void mapvalof (treenode *tptr,treenode **destlistptr);
void tvalof (treenode *tptr,treenode *destlist);
int  mappredef (treenode *tptr, treenode *destlist);
void tpredef (treenode *tptr, treenode *destlist);
