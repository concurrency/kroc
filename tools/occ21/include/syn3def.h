/* $Id: syn3def.h,v 1.2 1997/09/04 13:48:01 jm40 Exp $ */

/*
 *	syn3 definitions
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


treenode *rprocess(void);

#if 1 /*def CONFIG*/
treenode *rconfigdef (void);
#endif
#ifdef INITIAL_DECL
extern BOOL initial_decl;
#endif

