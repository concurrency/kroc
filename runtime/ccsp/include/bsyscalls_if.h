/*
 *	bsyscalls_if.h -- interface for using blocking system-calls
 *	Copyright (C) 2002-2004 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __BSYSCALLS_IF_H
#define __BSYSCALLS_IF_H

extern int bsyscalls_create_clones (void);
extern void bsyscall_dispatch (bsc_batch_t *);
extern int bsyscall_kill (word *);
extern void bsyscalls_destroy_clones (void);
extern void bsyscalls_adjust_pending (int);
extern int bsyscalls_pending (void);

#endif	/* !__BSYSCALLS_IF_H */

