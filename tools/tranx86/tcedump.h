/*
 *	tcedump.h - interface to tcedump.c
 *	Copyright (C) 2004 Christian Jacobsen
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

#ifndef __TCEDUMP_H
#define __TCEDUMP_H

#ifndef __TSTACK_H
#include "tstack.h"
#endif	/* !__TSTACK_H */

#define SECTION_SIZE_LEAP 4096

#define PFX_BYTE  251
#define PFX_INT16 252
#define PFX_INT32 253
#define PFX_INT64 254
#define PFX_NEG   255

typedef struct sect_t {
	char *ptr;
	int alloc, idx;
} sect;

extern int dump_binary_etc (etc_chain *, char *);

#endif	/* !__TCEDUMP_H */

