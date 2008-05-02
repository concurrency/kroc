/*
 *	Portable timer interface definitions (frmb made i386 specific..)
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

#ifndef I386_TIMER_H
#define I386_TIMER_H

#if defined(HAVE_CONFIG_H)
#include <config.h>
#endif

#include <sched_types.h>
#include <i386/ccsp_types.h>

static inline Time Time_GetTime (sched_t *sched)
{
	#ifdef ENABLE_CPU_TIMERS
	Time time_in_us;

	__asm__ __volatile__ ("			\n"
		"	.byte	0x0f, 0x31	\n" /* rdtsc                         */
		"	movl	%%edx, %%ebx	\n"
		"	mull	%%ecx		\n" /* r_a = low word * cpu_factor   */
		"	movl	%%ebx, %%eax	\n" 
		"	movl	%%edx, %%ebx	\n" /* save r_a[high]                */
		"	mull	%%ecx		\n" /* r_b = high word * cpu_factor  */
		"	addl	%%ebx, %%eax	\n" /* result = r_b[low] + r_b[high] */
		: "=a" (time_in_us)
		: "c" (sched->cpu_factor)
		: "ebx", "edx"
	);

	return time_in_us;
	#else
	return ccsp_rtime ();
	#endif
}

#ifdef ENABLE_CPU_TIMERS
/*
 *	this checks to see whether the time stored in sched->timeout is <= the current time
 */
static inline int Time_PastTimeout (sched_t *sched)
{
	unsigned int *timeout = sched->timeout.tsc;
	unsigned int tsc[2];
	
	__asm__ __volatile__ ("			\n"
		"	.byte	0x0f, 0x31	\n"
		: "=a" (tsc[0]), "=d" (tsc[1])
	);

	return (tsc[1] > timeout[1]) || (tsc[1] == timeout[1] && tsc[0] > timeout[0]);
}

/*
 *	this sets the value in sched->timeout (to something in the future only)
 */
static inline void Time_SetPastTimeout (sched_t *sched, Time delay)
{
	unsigned int *timeout = sched->timeout.tsc;

	__asm__ __volatile__ ("				\n"
		"	mull	%2			\n" /* result = delay * cpu_khz */
		"	movl	%%edx, %%ecx		\n" /* result /= 1024           */
		"	shr	$10, %%ecx		\n" /*  "                       */
		"	movl	%%eax, %%ebx		\n" /*  "                       */
		"	shr	$10, %%ebx		\n" /*  "                       */
		"	andl	$0x003fffff, %%ebx	\n" /*  "                       */
		"	shl	$22, %%edx		\n" /*  "                       */
		"	orl	%%edx, %%ebx		\n" /*  " (side-effect CF = 0)  */
		"	.byte	0x0f, 0x31		\n" /* rdtsc                    */
		"	addl	%%ebx, %%eax		\n" /* result += rdtsc          */
		"	adcl	%%ecx, %%edx		\n" /*  "                       */
		: "=a" (timeout[0]), "=d" (timeout[1])
		: "g" (sched->cpu_khz), "a" (delay)
		: "ebx", "ecx"
	);
}
#endif /* ENABLE_CPU_TIMERS */

#endif /* I386_TIMER_H */

