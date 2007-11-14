/*
 *	Portable timer interface definitions (frmb made i386 specific..)
 *	Copyright (C) 1996-1999 Jim Moores
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

#ifndef TIMER_HEADER
#define TIMER_HEADER

typedef int Time;

#include <rts.h>
#include <i386/sched.h>

#define GetTimeField(X) ((Time)((X)[Time_f]))

extern sync_type sf;

/* define as extern __inline__ */

#ifdef __TIMER_C
Time Time_GetTime (void)
#else
extern __inline__ Time Time_GetTime (void)
#endif
{
	#ifdef ENABLE_CPU_TIMERS
		Time time_in_us;

		__asm__ __volatile__ ("			\n" \
			"	pushl	%%edx		\n" \
			"	pushl	%%ebx		\n" \
			"	pushl	%%edx		\n" \
			"				\n" \
			"	.byte	0x0f, 0x31	\n" \
			"	movl	%%edx, %%ebx	\n" \
			"	mull	glob_cpufactor	\n" \
			"	movl	%%ebx, %%eax	\n" \
			"	movl	%%edx, %%ebx	\n" \
			"	mull	glob_cpufactor	\n" \
			"	addl	%%ebx, %%eax	\n" \
			"	movl	%%eax, (%%ecx)	\n" \
			"				\n" \
			"	popl	%%edx		\n" \
			"	popl	%%ebx		\n" \
			"	popl	%%edx		\n" \
			: : "c" (&time_in_us) : "cc", "memory");

		return time_in_us;
	#else
		return rtime();
	#endif
}


#ifdef __TIMER_C
Time Time_PLUS(Time t1, Time t2)
#else
extern __inline__ Time Time_PLUS(Time t1, Time t2)
#endif
{
	return (t1+t2);
}


#ifdef __TIMER_C
Time Time_MINUS(Time t1, Time t2)
#else
extern __inline__ Time Time_MINUS(Time t1, Time t2)
#endif
{
	return (t1-t2);
}


#ifdef __TIMER_C
bool Time_AFTER(Time t1, Time t2)
#else
extern __inline__ bool Time_AFTER(Time t1, Time t2)
#endif
{
	Time temp_time;

	temp_time = t2 - t1;
	if (temp_time & 0x80000000) {
		return 1;
	}
	return 0;
}


#ifdef __TIMER_C
bool Time_PollAlarm(void)
#else
extern __inline__ bool Time_PollAlarm(void)
#endif
{
	return (sf.u.s.tim_sync);
}


#ifdef __TIMER_C
void Time_SetAlarm(Time t)
#else
extern __inline__ void Time_SetAlarm(Time t)
#endif
{
	set_alarm (t);
}


#ifdef __TIMER_C
void Time_ResetAlarm(void)
#else
extern __inline__ void Time_ResetAlarm(void)
#endif
{
	set_alarm (0);
}

#endif /* timer.h */
