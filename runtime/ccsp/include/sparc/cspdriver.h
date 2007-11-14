/*
 *	cspdriver.h -- linux kernel cspdriver support
 *	Copyright (C) 2002 Fred Barnes <frmb2@ukc.ac.uk>
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

#ifndef __CSPDRIVER_H
#define __CSPDRIVER_H

extern int cspdriver_enabled;

extern void initialise_cspdriver (void);
extern void shutdown_cspdriver (void);
extern int cspdriver_safe_pause (unsigned long timeout);

#ifdef BLOCKING_SYSCALLS
extern int cspdriver_initialise_bsc_support (void);
extern int cspdriver_bsc_callin (void *addr, int length, int resync);
extern int cspdriver_bsc_dispatch (void **addr, int length);
extern int cspdriver_bsc_num_buffers (void);
#endif /* BLOCKING_SYSCALLS */

#endif	/* !__CSPDRIVER_H */

