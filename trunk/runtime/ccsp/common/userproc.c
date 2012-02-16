/*
 *	CCSP user process
 *	Copyright (C) 1995, 1996, 1997  D.C. Wood, P.H. Welch, D.J. Beckett
 *	Modification for RMOX (C) 2002 Brian Vinter / Fred Barnes
 *	Modifications Copyright (C) 2000-2006 Fred Barnes
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
 * This is the UNIX process that runs the occam code and traps its
 * termination cleanly or via signals.  The co-process handles blocking
 * on fd 0 (the keyboard) and uses a UNIX signal to deliver notification
 * of a new keypress.
 *
 * $Source: /proj/ofa/rts/RCS/userproc.c,v $
 *
 * $Id: userproc.c,v 1.11 1997/08/12 14:38:28 djb1 Exp $
 *
 * (C) Copyright 1995,1996,1997
 *    D.C. Wood <D.C.Wood@ukc.ac.uk>
 *    P.H. Welch <P.H.Welch@ukc.ac.uk>
 *    Dave Beckett <D.J.Beckett@ukc.ac.uk>
 * University of Kent at Canterbury
 *
 * Modifications Copyright (C) 2000-2006 Fred Barnes
 */

/*{{{ includes */
#ifdef HAVE_CONFIG_H
	#include <config.h>
#endif

#if defined(RMOX_BUILD)
	#include <rmox_if.h>
#else	/* !RMOX_BUILD */
	#include <unistd.h>
	#ifdef HAVE_STDLIB_H
		#include <stdlib.h>
	#endif
	#include <fcntl.h>
	#ifdef USE_PTHREADS
		#include <pthread.h>
	#endif
	#ifdef HAVE_SYS_PARAM_H
		#include <sys/param.h>
	#endif
	#ifdef TARGET_OS_DARWIN
		#include <sys/sysctl.h>
	#endif
	#include <errno.h>
#endif	/* !RMOX_BUILD */

#include <kernel.h>
#include <rts.h>

#if (defined(USEFUL_SEGFAULT) || defined(USEFUL_FPEDEBUG)) && !defined(RMOX_BUILD)
	#include <signal.h>
#endif

#if !defined(RMOX_BUILD) && defined(ENABLE_CPU_TIMERS)
	#include <sys/time.h>
	//#include <arch/timer.h>
#endif

#include <dmem_if.h>
#include <ccsp_if.h>
#include <arch/asm_ops.h>
#include <arch/atomics.h>
#include <ccsp_timer.h>

#if !defined(RMOX_BUILD)
	#ifdef BLOCKING_SYSCALLS
		#include <bsyscalls_if.h>
	#endif	/* BLOCKING_SYSCALLS */
#endif
/*}}}*/

/*{{{  private declarations*/
#if !defined(RMOX_BUILD)
static volatile int faulted;
static unsigned int min_sleep; /* microseconds */
static unsigned int quantum;
#endif

#if !defined(RMOX_BUILD)
	#ifdef USEFUL_SEGFAULT
	static struct sigaction segv_action;
	#endif
	#ifdef USEFUL_FPEDEBUG
	static struct sigaction fpe_action;
	#endif
#endif
/*}}}*/

/*{{{  static void userproc_exit (int exit_status, bool dump_core) */
/*
 *	local exit function
 */
static void userproc_exit (int exit_status, bool dump_core) 
{
	if (RTS_TRACING) {
		MESSAGE ("USERPROC: Exit %d\n", exit_status);
		FFLUSH (stderr);
	}

	if (RTS_TRACING) {
		MESSAGE ("USERPROC: About to kill KBDPROC\n");
		FFLUSH (stderr);
	}

	#if defined(RMOX_BUILD) && defined(BLOCKING_SYSCALLS)
	bsyscalls_destroy_clones ();
	#endif

	if (exit_status) {
		ccsp_show_last_debug_insert ();
	}

	#ifdef DM_DEBUG
	dm_debug_dump ();
	#endif
	dmem_shutdown ();

	ccsp_exit_handler (exit_status, dump_core);
}
/*}}}*/
/*{{{  void ccsp_kernel_exit (int exit_status, int iptr)*/
/*
 *	called from run-time kernel to exit
 */
void ccsp_kernel_exit (int exit_status, int iptr)
{
	#ifndef ALONE
	if (iptr != 0) {
		/* MESSAGE ("STOPped at %08x\n", iptr); */
	}
	#endif
	#if defined(RMOX_BUILD)
	MESSAGE ("CCSP: kernel exit at 0x%8.8x\n", (unsigned int)iptr);
	#endif
	userproc_exit (exit_status, false);
}
/*}}}*/
/*{{{  void ccsp_bad_exit (void)*/
/*
 *	bad exit (causes core-dump)
 */
void ccsp_bad_exit (void)
{
	if (RTS_TRACING) {
		BMESSAGE ("userproc: I've been murdered!\n");
	}
	#if defined(RMOX_BUILD)
	MESSAGE ("CCSP: bad exit in kernel\n");
	#endif
	userproc_exit (-8, 1);
}
/*}}}*/
/*{{{  static void set_error_flag (int erfl)*/
static void set_error_flag (int erfl)
{       
	if (erfl) {
		BMESSAGE ("error flag set\n");
		FFLUSH (stderr);
	}
}
/*}}}*/
/*{{{  void ccsp_dead (int erfl)*/
void ccsp_dead (int erfl)
{       
	MESSAGE0 ("\n");
	BMESSAGE ("program deadlocked (no processes to run)\n");
	FFLUSH (stderr);              /* still in raw mode */
	set_error_flag (erfl);
	userproc_exit (2, 0);
}
/*}}}*/
/*{{{  void ccsp_dead_quiet (int erfl)*/
void ccsp_dead_quiet (int erfl)
{
	FFLUSH (stderr);
	set_error_flag (erfl);
	userproc_exit (2, 0);
}
/*}}}*/

#if !defined(RMOX_BUILD)
/*{{{  void ccsp_wake_thread (sched_t scheduler)*/
void ccsp_wake_thread (sched_t *scheduler, int sync_bit)
{
	unsigned int data = 0;
	
	att_safe_clear_bit (&sleeping_threads, scheduler->index);
	weak_write_barrier ();
	att_safe_set_bit (&(scheduler->sync), sync_bit);
	serialise ();
	write (scheduler->signal_in, &data, 1);
}
/*}}}*/

#ifdef USEFUL_SEGFAULT
/*{{{  static void user_segv_handler (int sig, siginfo_t *siginfo, void *sptr)*/
/*
 *	verbose SIGSEGV handler
 */
static void user_segv_handler (int sig, siginfo_t *siginfo, void *sptr)
{
	if (siginfo->si_signo != SIGSEGV) {
		BMESSAGE ("in user_segv_handler (), but not segv ?\n");
	} else {
		BMESSAGE ("segmentation fault:\n");
		MESSAGE ("    errno = %d\n", siginfo->si_errno);
		MESSAGE ("    origin = %d (", siginfo->si_code);
		switch (siginfo->si_code) {
		case SI_USER:
			MESSAGE ("user)\n");
			break;
		case SI_QUEUE:
			MESSAGE ("queue)\n");
			break;
		case SI_TIMER:
			MESSAGE ("timer)\n");
			break;
		case SI_MESGQ:
			MESSAGE ("mesgq)\n");
			break;
		case SI_ASYNCIO:
			MESSAGE ("asyncio)\n");
			break;
		case SI_SIGIO:
			MESSAGE ("sigio)\n");
			break;
		default:
			if (siginfo->si_code > 0) {
				MESSAGE ("kernel)\n");
			} else {
				MESSAGE ("[dunno])\n");
			}
			break;
		}
		MESSAGE ("    fault at 0x%x\n", (unsigned int)siginfo->si_addr);
		MESSAGE ("    sptr at  0x%x\n", (unsigned int)sptr);
	}
	if (!faulted) {
		faulted = 1;
		userproc_exit (1, 1);
	} else {
		kill (getpid (), SIGKILL);
	}
	return;
}
/*}}}*/
#endif	/* USEFUL_SEGFAULT */
#ifdef USEFUL_FPEDEBUG
/*{{{  static void user_fpe_handler (int sig, siginfo_t *siginfo, void *sptr)*/
/*
 *	verbose SIGFPE handler
 */
static void user_fpe_handler (int sig, siginfo_t *siginfo, void *sptr)
{
	if (siginfo->si_signo != SIGFPE) {
		BMESSAGE ("in user_fpe_handler (), but not FPE ?\n");
	} else {
		BMESSAGE ("floating point exception:\n");
		switch (siginfo->si_code) {
		case FPE_INTDIV:
			MESSAGE ("    integer division by zero\n");
			break;
		case FPE_INTOVF:
			MESSAGE ("    integer overflow\n");
			break;
		case FPE_FLTDIV:
			MESSAGE ("    floating point divide by zero\n");
			break;
		case FPE_FLTOVF:
			MESSAGE ("    floating point overflow\n");
			break;
		case FPE_FLTUND:
			MESSAGE ("    floating point underflow\n");
			break;
		case FPE_FLTRES:
			MESSAGE ("    inexact result\n");
			break;
		case FPE_FLTINV:
			MESSAGE ("    invalid operation\n");
			break;
		case FPE_FLTSUB:
			MESSAGE ("    subscript out of range\n");
			break;
		default:
			MESSAGE ("    unspecified\n");
			break;
		}
		MESSAGE ("    fault at 0x%x\n", (unsigned int)siginfo->si_addr);
		MESSAGE ("    sptr at  0x%x\n", (unsigned int)sptr);
	}
	userproc_exit (1, 0);
	return;
}
/*}}}*/
#endif	/* USEFUL_FPEDEBUG */
#if defined(SIGNAL_TYPE_SYSV)
/*{{{  void user_trap_handler (int sig)*/
void user_trap_handler (int sig)
{
	if (sig == SIGSEGV) {
		BMESSAGE ("Segmentation fault.\n");
	} else {
		BMESSAGE ("Range error / STOP executed (signal %d)\n", sig);
	}
	if (!faulted) {
		faulted = 1;
		userproc_exit (1, 1);
	} else {
		/* Faulted during fault.. */
		kill (getpid(), SIGKILL);
	}
} /* user_trap_handler */
/*}}}*/
/*{{{  void user_fp_handler (int sig)*/
void user_fp_handler (int sig)
{
	userproc_exit (41, 0);
}
/*}}}*/
#else
/* trap codes */
#define _SETERR         16      /* unconditional (SETERR) */
#define _OFLOW          17      /* arithmetic overflow */
#define _RANGE          18      /* range check (CSUB0, CCNT1, CSNGL, CWORD) */
#define _FLOAT          19      /* floating-point (FPCHKERR, FPSETERR) */
#define _UNREC          20      /* unrecognized instruction */
#define _UNIMP          21      /* unimplemented instruction */

/*{{{  void user_trap_handler (int sig, int code, struct sigcontext *scp, char *addr)*/
void user_trap_handler (int sig, int code, struct sigcontext *scp, char *addr)
{
	/*      fprintf (stderr, "sig = 0x%x, code = 0x%x\n", sig, code);       */
	BMESSAGE ("");
	switch (code & 0x1f) {           /* why? */
	case _SETERR:
		MESSAGE ("Set error");
		break;
	case _OFLOW:
		MESSAGE ("Integer overflow");
		break;
	case _RANGE:
		MESSAGE ("Range error");
		break;
	case _FLOAT:
		MESSAGE ("Floating-point error");
		break;
	case _UNREC:
		MESSAGE ("Unrecognized instruction");
		break;
	case _UNIMP:
		MESSAGE ("Unimplemented instruction");
		break;
	default:
		MESSAGE ("Unrecognized trap number 0x%x", code & 0x1f);
		break;
	}
	MESSAGE ("\n");
	FFLUSH (stderr);
	userproc_exit (code, 1);
} /* user_trap_handler */
/*}}}*/
/*{{{  void user_fp_handler (int sig, int code, struct sigcontext *scp, char *addr)*/
void user_fp_handler (int sig, int code, struct sigcontext *scp, char *addr)
{
	switch(code) {
	#ifdef FPE_INTOVF_TRAP
	case FPE_INTOVF_TRAP:
		BMESSAGE ("floating point error - integer overflow\n");
		break;
	#endif      
	#ifdef FPE_STARTSIG_TRAP
	case FPE_STARTSIG_TRAP:
		BMESSAGE ("floating point error - process using fp\n");
		break;
	#endif      
	#ifdef FPE_INTDIV_TRAP
	case FPE_INTDIV_TRAP:
		BMESSAGE ("floating point error - integer divide by zero\n");
		break;
	#endif      
	#ifdef FPE_FLTINEX_TRAP
	case FPE_FLTINEX_TRAP:
		BMESSAGE ("floating point error - floating inexact result\n");
		break;
	#endif      
	#ifdef FPE_FLTDIV_TRAP
	case FPE_FLTDIV_TRAP:
		BMESSAGE ("floating point error - floating divide by zero\n");
		break;
	#endif      
	#ifdef FPE_FLTUND_TRAP
	case FPE_FLTUND_TRAP:
		BMESSAGE ("floating point error - floating underflow\n");
		break;
	#endif      
	#ifdef FPE_FLTOPERR_TRAP
	case FPE_FLTOPERR_TRAP:
		BMESSAGE ("floating point error - floating operand error\n");
		break;
	#endif      
	#ifdef FPE_FLTOVF_TRAP
	case FPE_FLTOVF_TRAP:
		BMESSAGE ("floating point error - floating overflow\n");
		break;
	#endif      
	default:
		BMESSAGE ("Unknown floating point error number 0x%x\n", code);
		break;
	}
	userproc_exit (42, 1);
}
/*}}}*/
#endif /* SIGNAL_TYPE */

/*{{{  static void user_tim_handler (int sig)*/
/*
 *	timer signal (SIGALRM) handler
 *	SIGALRM  handler - only set during hibernate
 */
static void user_tim_handler (int sig)
{
	int threads = att_val (&enabled_threads);
	
	if (RTS_TRACING) {
		MESSAGE ("USERPROC: Alarm ringing (SIGALRM).\n");
		FFLUSH (stderr);
	}

	while (threads) {
		int n 		= bsf (threads);
		sched_t *s 	= schedulers[n];

		threads &= ~(s->id);

		ccsp_wake_thread (s, SYNC_TIME_BIT);
	}
	
	#if defined(SIGNAL_TYPE_SYSV)
		signal (SIGALRM, user_tim_handler);
	#endif /* SIGNAL_TYPE */
}
/*}}}*/
/*{{{  unsigned int ccsp_rtime (void)*/
unsigned int ccsp_rtime (void)
{
	struct timeval tp;

	gettimeofday (&tp, 0);
	
	#ifdef DEBUG_RTS
		fprintf(stderr, "ccsp_rtime: time returning is %d\n", tp.tv_sec * 1000000 + tp.tv_usec);
	#endif
	
	return (tp.tv_sec * 1000000) + tp.tv_usec;
}
/*}}}*/
/*{{{  void ccsp_set_next_alarm (sched_t *sched, unsigned int usecs)*/
void ccsp_set_next_alarm (sched_t *sched, unsigned int usecs)
{
	unsigned int next_alarm;
	struct itimerval itv;
	int ret;

	getitimer (ITIMER_REAL, &itv);

	next_alarm = ((unsigned int) (itv.it_value.tv_sec * 1000000U)) + 
		((unsigned int) itv.it_value.tv_usec);

	while (usecs && (next_alarm == 0 || next_alarm > usecs)) {
		itv.it_interval.tv_sec 	= 0;
		itv.it_interval.tv_usec = 0;

		#ifdef SOLARIS_TIMER_BUG
		itv.it_value.tv_sec 	= (usecs + quantum) / 1000000;
		itv.it_value.tv_usec	= (usecs + quantum) % 1000000;
		#else
		itv.it_value.tv_sec 	= usecs / 1000000;
		itv.it_value.tv_usec	= usecs % 1000000;
		#endif

		if ((ret = setitimer (ITIMER_REAL, &itv, &itv)) < 0) {
			BMESSAGE ("unable to set interval timer [%d] (%d)\n", usecs, ret);
			userproc_exit (1, 0);
		}
		
		next_alarm = usecs;
		usecs = ((unsigned int) (itv.it_value.tv_sec * 1000000U)) + 
			((unsigned int) itv.it_value.tv_usec);
	}
}
/*}}}*/
/*{{{  void ccsp_init_signal_pipe (sched_t *sched)*/
void ccsp_init_signal_pipe (sched_t *sched)
/*
 *	Called from run-time kernel
 */
{
	int fds[2], ret;
	
	if ((ret = pipe (fds)) < 0) {
		BMESSAGE ("unable allocate signalling pipe for scheduler [%p] (%d)\n", sched, ret);
		userproc_exit (1, 0);
	}

	sched->signal_in = fds[1];
	sched->signal_out = fds[0];
	
	if ((ret = fcntl(sched->signal_in, F_SETFL, O_NONBLOCK)) < 0) {
		BMESSAGE ("unable to make signalling pipe unblocking [%p] (%d)\n", sched, ret);
		userproc_exit (1, 0);
	}
}
/*}}}*/
/*{{{  void ccsp_safe_pause (sched_t *sched)*/
void ccsp_safe_pause (sched_t *sched)
{
	unsigned int buffer, sync;

	#ifdef DEBUG_RTS
	fprintf(stderr, "USERPROC: ccsp_safe_pause() entered\n");
	#endif
	
	while (!(sync = att_safe_swap (&(sched->sync), 0))) {
		serialise ();
		read (sched->signal_out, &buffer, 1);
		serialise ();
	}

	/* restore detected flags */
	att_safe_or (&(sched->sync), sync);

	#ifdef DEBUG_RTS
	fprintf(stderr, "USERPROC: ccsp_safe_pause about to exit (return 0)\n");
	#endif
}
/*}}}*/
/*{{{  void ccsp_safe_pause_timeout (sched_t *sched)*/
void ccsp_safe_pause_timeout (sched_t *sched)
{
	unsigned int sync;
	Time now;

	#ifdef DEBUG_RTS
	fprintf(stderr, "USERPROC: ccsp_safe_pause_timeout() entered\n");
	#endif

	if (sched->tq_fptr == NULL) {
		return;
	} else if (Time_PastTimeout (sched)) {
		return;
	}

	now = Time_GetTime(sched);
	if (Time_AFTER (sched->tq_fptr->time, now)) {
		unsigned int usecs = Time_MINUS (sched->tq_fptr->time, now);

		if (usecs < min_sleep) {
			while (!(sync = att_safe_swap (&(sched->sync), 0))) {
				int i = 10;
				
				while (i--) {
					idle_cpu ();
				}

				if (Time_PastTimeout (sched)) {
					break;
				}

				serialise ();
			}

			if (sync) {
				/* restore detected flags */
				att_safe_or (&(sched->sync), sync);
			}
		} else {
			ccsp_set_next_alarm (sched, usecs);
			ccsp_safe_pause (sched);
		}
	}

	#ifdef DEBUG_RTS
	fprintf(stderr, "USERPROC: ccsp_safe_pause_timeout() about to exit (return 0)\n");
	#endif
}
/*}}}*/
/*{{{  static bool set_user_process_signals (void)*/
/*
 *	sets up signal handling for CCSP
 */
static bool set_user_process_signals (void)
{
	/* Modifies kernel tim_sync variable */
	signal (SIGALRM, user_tim_handler);
	#if 0
		/* Called by user if they panic */
		signal (SIGPANIC, (SigParam)user_panic_handler);
	#endif

	#ifdef BLOCKING_SYSCALLS
	/* can safely ignore child processes exiting.. */
	signal (SIGCHLD, SIG_IGN);
	#endif

	/* 'Error' signals */
	signal (SIGILL, user_trap_handler);  /* illegal instruction */
	signal (SIGBUS, user_trap_handler);  /* bus error */
	faulted = 0;
	#ifdef USEFUL_SEGFAULT
		segv_action.sa_sigaction = user_segv_handler;
		segv_action.sa_flags = SA_SIGINFO;
		sigemptyset (&segv_action.sa_mask);
		if (sigaction (SIGSEGV, &segv_action, NULL) < 0) {
			BMESSAGE ("unable to sigaction SEGV, using normal.\n");
			signal (SIGSEGV, user_trap_handler);
		}
	#else	/* !USEFUL_SEGFAULT */
		#ifndef DEFAULT_CORES
			signal (SIGSEGV, user_trap_handler); /* segmentation violation */
		#endif
	#endif	/* !USEFUL_SEGFAULT */
	#if defined(TARGET_CPU_ALPHA)
		signal (SIGTRAP, user_trap_handler);
	#endif
	#ifdef USEFUL_FPEDEBUG
		fpe_action.sa_sigaction = user_fpe_handler;
		fpe_action.sa_flags = SA_SIGINFO;
		sigemptyset (&fpe_action.sa_mask);
		if (sigaction (SIGFPE, &fpe_action, NULL) < 0) {
			BMESSAGE ("unable to sigaction FPE, using normal.\n");
			signal (SIGFPE, user_fp_handler);
		}
	#else	/* !USEFUL_FPEDEBUG */
		signal (SIGFPE, user_fp_handler);
	#endif	/* !USEFUL_FPEDEBUG */

	return true; /* FIXME: do more checking? */
}
/*}}}*/
#endif	/* !RMOX_BUILD */

#if !defined(RMOX_BUILD)
/*{{{  static int cpu_count (void)*/
static int cpu_count (void)
{
	char *envstr;
	int cpus = 1;
	
	if ((envstr = getenv ("CCSP_RUNTIME_THREADS")) != NULL) {
		cpus = (int) strtol (envstr, NULL, 10);
	} else {
		#if defined(TARGET_OS_DARWIN)
		size_t len = sizeof(cpus);
		int mib[2] = { CTL_HW, HW_NCPU }; 
		if (sysctl (mib, 2, &cpus, &len, 0, 0) < 0) {
			cpus = 1;
		} else if (len != sizeof(cpus)) {
			cpus = 1;
		}
		#elif defined(_SC_NPROCESSORS_ONLN) 
		cpus = sysconf(_SC_NPROCESSORS_ONLN);
		#endif 
	}

	if (cpus <= 0) {
		cpus = 1;
	} else if (cpus > MAX_RUNTIME_THREADS) {
		cpus = MAX_RUNTIME_THREADS;
	}

	return cpus;
}
/*}}}*/
/*{{{  unsigned int ccsp_spin_us (void)*/
unsigned int ccsp_spin_us (void)
{
	char *envstr;
	int cpus = cpu_count ();

	if (cpus < 2) {
		return 0;
	} else if ((envstr = getenv ("CCSP_SCHEDULER_SPIN")) != NULL) {
		long spin = strtol (envstr, NULL, 10);
		if (spin >= 0) {
			return (unsigned int) spin;
		}
	}

	return 16;
}
/*}}}*/
/*{{{  static void setup_min_sleep (void)*/
static void setup_min_sleep (void)
{
	char *envstr;

	if ((envstr = getenv ("CCSP_MINIMUM_SLEEP")) != NULL) {
		long us = strtol (envstr, NULL, 10);
		if (us >= 0) {
			min_sleep = us;
			return;
		}
	}

	min_sleep = quantum ? quantum / 4 : CCSP_MINIMUM_SLEEP_US;
}
/*}}}*/
#else /* RMOX_BUILD */
/*{{{  unsigned int ccsp_spin_us (void)*/
unsigned int ccsp_spin_us (void)
{
	return 0;
}
/*}}}*/
#endif /* RMOX_BUILD */

#if defined(ENABLE_MP) && defined(USE_PTHREADS)
/*{{{  static void *user_thread (void *arg)*/
static void *user_thread (void *arg)
{
	ccsp_kernel_entry (NotProcess_p, NotProcess_p);
	return NULL;
} 
/*}}}*/
/*{{{  void ccsp_new_thread (void)*/
void ccsp_new_thread (void)
{
	pthread_t thread;
	pthread_create (&thread, NULL, user_thread, NULL);
}
/*}}}*/
#elif defined(ENABLE_MP) && defined(RMOX_BUILD)
/* FIXME: needs relevant support for MP RMoX */
void ccsp_new_thread (void)
{
	ccsp_kernel_entry (NotProcess_p, NotProcess_p);
	return;
}
#else /* !ENABLE_MP */
/*{{{  void ccsp_new_thread (void)*/
void ccsp_new_thread (void)
{
	return;
}
/*}}}*/
#endif /* !ENABLE_MP */

/*{{{ PROC C.ccsp.new.thread () */
void _ccsp_new_thread (int *ws) {
	ccsp_new_thread ();
}
/*}}}*/
#if !defined(RMOX_BUILD)
/*{{{  void ccsp_start_threads (void)*/
void ccsp_start_threads (void)
{
	int cpus = cpu_count ();
	
	/* start cpus - 1 threads (as one is already running) */
	while (--cpus) {
		ccsp_new_thread ();
	}
}
/*}}}*/
#else /* RMOX_BUILD */
/*{{{  void ccsp_start_threads (void)*/
void ccsp_start_threads (void)
{
	return;
}
/*}}}*/
#endif /* RMOX_BUILD */

/*{{{  void ccsp_user_process_init (void)*/
bool ccsp_user_process_init (void)
{
	#if defined(TARGET_OS_DARWIN)
	{
		struct clockinfo clockrate;
		size_t len = sizeof(clockrate);
		int mib[2] = { CTL_KERN, KERN_CLOCKRATE };
		
		clockrate.tick = 0;
		sysctl (mib, 2, &clockrate, &len, 0, 0);

		quantum = clockrate.tick;
	}
	#elif defined(HZ) || defined(SOLARIS_TIMER_BUG)
	quantum = 1000000U / HZ;
	#elif !defined(RMOX_BUILD)
	quantum = 0;
	#endif

	#if !defined(RMOX_BUILD)
	setup_min_sleep ();

	if (!set_user_process_signals ()) {
		return false;
	}
	#endif

	if (RTS_TRACING) {
		MESSAGE ("USERPROC: synchronised ok\n");
		FFLUSH (stderr);
	}

	return true;
}
/*}}}*/

