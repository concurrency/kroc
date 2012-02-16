/*
 *	CCSP internal definitions and function prototypes
 *	Copyright (C) 1995, 1996, 1997 D.J. Beckett
 *	Modifications for RMoX (C) 2002 Brian Vinter
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

/*
 * $Source: /proj/ofa/rts/RCS/rts.h,v $
 *
 * $Id: rts.h,v 1.22 1997/08/12 14:39:16 djb1 Exp $
 *
 * (C) Copyright 1995,1996,1997 D.J. Beckett <D.J.Beckett@ukc.ac.uk>
 * University of Kent at Canterbury
 */

#ifndef RTS_H
#define RTS_H

#ifdef HAVE_CONFIG_H
	#include <config.h>
#endif

#if !defined(RMOX_BUILD)
	#include <stdio.h>
	#if defined(HAVE_STRING_H)
		#include <string.h>
	#elif defined(HAVE_STRINGS_H)
		#include <strings.h>
	#endif
	#if defined(HAVE_UNISTD_H)
		#include <unistd.h>
	#endif
	#if defined(HAVE_STDLIB_H)
		#include <stdlib.h>
	#endif

	#include <sys/types.h>

	#if defined(HAVE_SYS_WAIT_H)
		#include <sys/wait.h>
	#endif
	#if defined(TIME_WITH_SYS_TIME)
		#include <sys/time.h>
		#include <time.h>
	#elif defined(HAVE_SYS_TIME_H)
		#include <sys/time.h>
	#else
		#include <time.h>
	#endif

	#if defined(HAVE_SYS_TIMERS_H)
		#include <sys/timers.h>
	#endif

	#include <signal.h>

	#if defined(HAVE_ERRNO_H)
		#include <errno.h>
	#endif
#endif

#include <ukcthreads_types.h>
#include <sched_types.h>

/*{{{ globals */
#ifndef RTS_MAIN
#ifdef DEBUG_RTS
extern bool ccsp_trace;
#endif
extern bool ccsp_ignore_errors;
extern void (*ccsp_exit_handler)(int, bool);
extern bool (*ccsp_external_event_hook)(void);
extern char *ccsp_branding;
#endif
/*}}}*/

/*{{{ defines */
#ifdef DEBUG_RTS
#define RTS_TRACING ccsp_trace
#else
#define RTS_TRACING 0
#endif

#ifdef RMOX_BUILD
	#define MESSAGE(fmt,args...) printk(fmt,##args)
	#define MESSAGE0(fmt) printk((fmt))
	#define MESSAGETO(stream,fmt,args...) printk(fmt,##args)
	#define FFLUSH(stream)
#else /* !RMOX_BUILD */
	#define MESSAGE(fmt,args...) fprintf(stderr,fmt,##args)
	#define MESSAGE0(fmt) fprintf(stderr,(fmt))
	#define MESSAGETO(stream,fmt,args...) fprintf(stream,fmt,##args)
	#define FFLUSH(stream) fflush(stream)
#endif /* !RMOX_BUILD */

#define BMESSAGE(fmt,args...) MESSAGE("%s: " fmt, ccsp_branding, ##args)
#define BMESSAGE0(fmt) MESSAGE("%s: " fmt, ccsp_branding)
/*}}}*/

/*{{{ dtrace.c */
int ccsp_init_dtraces (void);
/*}}}*/

/*{{{ timercal.c */
#ifdef ENABLE_CPU_TIMERS
void ccsp_initial_cpu_speed (unsigned int *factor, unsigned int *khz);
int ccsp_calibrate_timers (void);
#endif
/*}}}*/

/*{{{ userproc.c */
void ccsp_kernel_exit (int exit_status, int iptr);
void ccsp_bad_exit (void);
void ccsp_dead (int erfl);
void ccsp_dead_quiet (int erfl);
#if !defined(RMOX_BUILD)
void ccsp_set_next_alarm (sched_t *sched, unsigned int usecs);
void ccsp_wake_thread (sched_t *scheduler, int sync_bit);
unsigned int ccsp_rtime (void);
void ccsp_init_signal_pipe (sched_t *sched);
void ccsp_safe_pause (sched_t *sched);
void ccsp_safe_pause_timeout (sched_t *sched);
#endif /* !RMOX_BUILD */
unsigned int ccsp_spin_us (void);
void ccsp_new_thread (void);
void ccsp_start_threads (void);
bool ccsp_user_process_init (void);
/*}}}*/

#endif /* rts.h */

