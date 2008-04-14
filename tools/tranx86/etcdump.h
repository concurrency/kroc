/*
 *	etcdump.h - interface to etcdump.c
 *	Copyright (C) 2000 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __ETCDUMP_H
#define __ETCDUMP_H

#ifndef __TSTACK_H
#include "tstack.h"
#endif	/* !__TSTACK_H */

extern int dump_textual_etc (etc_chain *, char *);
extern char *string_of_primary (tstack *stack, int prim, int operand);
extern char *string_of_secondary (tstack *stack, int esfunc);
extern int dump_comments (etc_chain *);

#endif	/* !__ETCDUMP_H */

