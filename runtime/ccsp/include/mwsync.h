/*
 *	mwsync.h -- multiway synchronisation structure for run-time
 *	Copyright (C) 2006 Fred Barnes <frmb@kent.ac.uk>
 *	Based on notes from Peter Welch
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

#ifndef __MWSYNC_H
#define __MWSYNC_H

struct TAG_mwsync;
struct TAG_mwsyncwait;


typedef struct TAG_mwsyncwait {
	struct TAG_mwsyncwait *next;
	unsigned int *wptr;
	unsigned int priority;
} mwsyncwait_t;

typedef struct TAG_mwsync {
	int ref_count;
	int enroll_count;
	int down_count;
	mwsyncwait_t *qfptr;
	mwsyncwait_t *qbptr;
} mwsync_t;

/* these are workspace offsets used by ALTing processes involved in multi-way syncs */
#define MWSyncTemp 0
/* #define MWSyncSavedPriority 1 */		/* not used anymore: does it with a mutex in the run-time */
#define MWSyncChosen 1


/* this structure lives in the run-time, providing the global ALT lock */

typedef struct TAG_mwsyncglock {
	int value;				/* mutex value */
	word *qfptr;				/* suspended process queue */
	word *qbptr;
	int count;				/* disabling count */
} mwsyncglock_t;

extern mwsyncglock_t mwaltlock;


#endif	/* !__MWSYNC_H */

