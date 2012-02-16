/*
 *	use4def.h -- publically visible stuff in use4.c
 *	Copyright (C) 2001-2004 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __USE4DEF_H
#define __USE4DEF_H

PUBLIC void undefinedcheck (treenode *n);
PUBLIC BOOL udv_thingisdynmobileproctype (treenode *n);
PUBLIC BOOL udv_thingisdynmobilechantype (treenode *n);
PUBLIC char *use_exprstring (treenode *expr);

#endif	/* !__USE4DEF_H */
