/*
 *	ccsp_stats.h -- CCSP statistics interface
 *	Copyright (C) 2007 Carl Ritson <cgr@kent.ac.uk>
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

#ifndef __CCSP_STATS_H
#define __CCSP_STATS_H

#define CCSP_STATS_VERSION_1_00	0x100
#define CCSP_STATS_VERSION	CCSP_STATS_VERSION_1_00

typedef struct _ccsp_stats_t ccsp_stats_t;

struct _ccsp_stats_t {
	unsigned long proc_start;
	unsigned long proc_end;
	unsigned long startp;
	unsigned long endp;
};

extern int ccsp_get_stats (int version, ccsp_stats_t *stats);

#endif /* __CCSP_STATS_H */

