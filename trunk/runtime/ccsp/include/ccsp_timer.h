/*
 *	Portable timer interface definitions
 *	Copyright (C) 1996-1999 Jim Moores
 *	Modifications (C) 2001-2005 Fred Barnes  <frmb@kent.ac.uk>
 *	Modifications (C) 2007 Carl Ritson <cgr@kent.ac.uk>
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

#ifndef __CCSP_TIMER_H
#define __CCSP_TIMER_H

#ifdef CCSP_CIF

typedef word Time;
#ifndef INLINE
#define INLINE inline
#endif

#else /* !CCSP_CIF */

#if defined(HAVE_CONFIG_H)
#include <config.h>
#endif

#include <arch/timer.h>
#include <inlining.h>
#include <rts.h>

#endif /* !CCSP_CIF */

#define GetTimeField(X) ((Time)((X)[Time_f]))
#define SetTimeField(X,V) ((word *)(X))[Time_f] = (V)
#define IncTimeField(X) (((word *)(X))[Time_f])++

static INLINE Time Time_PLUS (Time t1, Time t2)
{
	return (t1 + t2);
}

static INLINE Time Time_MINUS (Time t1, Time t2)
{
	return (t1 - t2);
}

static INLINE bool Time_AFTER (Time t1, Time t2)
{
	Time temp_time = t2 - t1;

	if (temp_time & (1 << ((sizeof(Time) * 8) - 1))) {
		return true;
	}

	return false;
}

#ifndef CCSP_CIF

#ifndef ENABLE_CPU_TIMERS
static TRIVIAL int Time_PastTimeout (sched_t *sched)
{
	Time now 	= Time_GetTime (sched);
	tqnode_t *tn	= sched->tq_fptr;
	
	if (!Time_AFTER (tn->time, now)) {
		return true;
	}

	return false;
}

static INLINE void Time_SetAlarm (sched_t *sched, Time t)
{
	ccsp_set_next_alarm (sched, t);
}
#endif /* !ENABLE_CPU_TIMERS */

static INLINE bool Time_PollAlarm (sched_t *sched)
{
	return att_val (&(sched->sync)) & SYNC_TIME ? true : false;
}

static INLINE void Time_ResetAlarm (sched_t *sched)
{
	/* ccsp_set_next_alarm (sched, 0); */
}

static INLINE void Time_SetTimeoutN (sched_t *sched, Time now, Time timeout)
{
	#ifdef ENABLE_CPU_TIMERS
	Time_SetPastTimeout (sched, Time_MINUS (timeout, now));
	#else
	Time_SetAlarm (sched, Time_MINUS (timeout, now));
	#endif
}

static TRIVIAL void Time_SetTimeout (sched_t *sched, Time timeout)
{
	Time now = Time_GetTime (sched);
	if (Time_AFTER (timeout, now)) {
		Time_SetTimeoutN (sched, now, timeout);
	} else {
		Time_SetTimeoutN (sched, now, now + 1);
	}
}

#endif /* !CCSP_CIF */

#endif /* __CCSP_TIMER_H */

