/* $Id: syndef.h,v 1.2 1998/09/03 12:03:18 djb1 Exp $ */

/*
 *	syntax analyser definitions visible to rest of compiler
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

extern int syn_lexlevel;    /* Used in chk and use */
extern int linebreakindent; /* Used in objrdr */

treenode *rscunit (void);   /* Called by objrdr */
treenode *rpredef (void);   /* Called by chk    */
treenode *parse_file(const char *const sourcefilename, const BOOL onlylex); /* Called by occamfe */
int rrepl (treenode ** nptr, treenode **start, treenode **length, treenode **step);

void syninit (void); /* Called by occamfe */

