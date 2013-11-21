/*
 *	armccsp.h -- runtime system definitions
 *	Copyright (C) 2013 Fred Barnes, University of Kent <frmb@kent.ac.uk>
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
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 */

#ifndef __ARMCCSP_H
#define __ARMCCSP_H

extern void armccsp_fatal (const char *fmt, ...);
extern void armccsp_error (const char *fmt, ...);
extern void armccsp_warning (const char *fmt, ...);

extern void *armccsp_smalloc (const int bytes);
extern void armccsp_sfree (void *ptr);
extern void armccsp_sfreep (void **pptr);


/* define this to enable various debugging things in the run-time */
// #define CCSP_DEBUG


#endif	/* !__ARMCCSP_H */

