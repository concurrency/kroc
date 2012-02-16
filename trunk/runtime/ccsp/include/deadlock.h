/*
 *	Interface to the deadlock detection stuff
 *	Copyright (C) 2000 Fred Barnes
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

#ifndef _DEADLOCK_H
#define _DEADLOCK_H

#ifndef __DEADLOCK_C
extern void ccsp_deadlock_init (void);
extern void ccsp_give_ws_code (char *ws, int ws_bytes, unsigned char *codeptr);
extern void ccsp_take_ws (char *ws);
extern void ccsp_kernel_deadlock (void);
#endif

#define MAX_WORKSPACES	128

#define DLOP_INVALID	0
#define DLOP_IN		1
#define DLOP_OUT	2
#define DLOP_OUTBYTE	3
#define DLOP_OUTWORD	4
#define DLOP_ALTWT	5
#define DLOP_TALTWT	6

#include <arch/deadlock.h>

#endif	/* !_DEADLOCK_H */

