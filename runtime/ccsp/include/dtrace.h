/*
 *	dtrace.h -- debugging traces for CCSP
 *	Copyright (C) 2006-2008 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __DTRACE_H
#define __DTRACE_H

#if defined(ENABLE_DTRACES)

extern int init_dtraces (void);
extern void do_dtrace (const char *fmt, ...);
extern void rt_dtrace (void *wptr, unsigned int dta, unsigned int dtb);


#endif	/* defined(ENABLE_DTRACES) */

#endif	/* !__DTRACE_H */

