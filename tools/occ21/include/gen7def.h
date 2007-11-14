/* $Id: gen7def.h,v 1.1 1996/04/15 10:52:07 djb1 Exp $ */

/*
 *	gen7 declarations
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

void tbool  (treenode *tptr, int regs);
void tguard (treenode *tptr, BOOL fallthroughsense, int otherlabel);

void genboolassertion(treenode *tptr, BOOL correctsense,
                      treenode *error_tptr, int mode);

