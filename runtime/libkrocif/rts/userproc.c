/*
 *	KRoC user process
 *	Copyright (C) 1995, 1996, 1997  D.C. Wood, P.H. Welch, D.J. Beckett
 *	Modification for OOS (C) 2002 Brian Vinter / Fred Barnes
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

#ifdef HAVE_CONFIG_H
	#include <config.h>
#endif

#include <unistd.h>
#ifdef HAVE_STDLIB_H
	#include <stdlib.h>
#endif
#include <fcntl.h>
#include <setjmp.h>
#ifdef USE_PTHREADS
	#include <pthread.h>
#endif
#ifdef __APPLE__
	#include <sys/sysctl.h>
#endif
#include <errno.h>

#include <rts.h>

#if (defined(USEFUL_SEGFAULT) || defined(USEFUL_FPEDEBUG))
	#include <signal.h>
#endif

#define MESSAGE(fmt,args...) fprintf (stderr,fmt,##args)
#define FFLUSH(stream) fflush(stream)

/*{{{  global variables*/
FILE *kroc_in	= NULL;
FILE *kroc_out	= NULL;
FILE *kroc_err	= NULL;
/*}}}*/
/*{{{  private variables*/
static sigjmp_buf signal_jump_buffer;
static int stdin_is_tty;
/*}}}*/

/*{{{  static void userproc_exit (int exit_status, bool dump_core) */
/*
 *	local exit function
 */
static void userproc_exit (int exit_status, bool dump_core) 
{
	ccsp_exit (exit_status, dump_core);
}
/*}}}*/
/*{{{  void user_bad_exit (void)*/
/*
 *	bad exit (causes core-dump)
 */
void user_bad_exit (void)
{
	userproc_exit (-8, 1);
}
/*}}}*/
/*{{{  void user_good_exit (void)*/
/*
 *	good exit (status 0)
 */
void user_good_exit (void)
{
	userproc_exit (0, 0);
}
/*}}}*/
/*{{{  static void user_signal_good_exit (int sig)*/
/*
 *	good exit from signal handler
 */
static void user_signal_good_exit (int sig)
{
	siglongjmp (signal_jump_buffer, 1);
}
/*}}}*/
/*{{{  static void user_signal_quit_exit (int sig)*/
/*
 *	sigquit exit (status 0, but with deadlock stuff)
 */
static void user_signal_quit_exit (int sig)
{
	siglongjmp (signal_jump_buffer, 3);
}
/*}}}*/

/*{{{  static void user_cont_signal (int sig)*/
static void user_cont_signal (int sig)
{
	/* Advanced Programming in the UNIX Environment, Stevens; P321 */
	sigset_t mask;

	/* unblock SIGCONT since it is blocked while we're handling it */
	sigemptyset (&mask);
	sigaddset (&mask, SIGCONT);
	sigprocmask (SIG_UNBLOCK, &mask, NULL);

	if (stdin_is_tty) {
		/* Save terminal state */
		save_tty_state ();
	}

	/* Do default SIGCONT action - continue the process */
	signal (SIGCONT, SIG_DFL);
	kill (getpid(), SIGCONT);
}
/*}}}*/
/*{{{  static void user_stop_signal (int sig)*/
static void user_stop_signal (int sig)
{
	/* Advanced Programming in the UNIX Environment, Stevens; P321 */

	/* set continue handler for after stop */
	signal (SIGCONT, user_cont_signal);

	if (stdin_is_tty && occam_uses_keyboard ()) {
		restore_tty_state ();
	}

	/* kill_kbdio (); */

	/* Do the default SIGTSTP action - stop this process */
	kill (getpid(), SIGSTOP);

	#if defined(SIGNAL_TYPE_SYSV)
	/* Re-establish signal handler */
	signal (SIGTSTP, user_stop_signal);
	#endif
}
/*}}}*/

/*{{{  static void set_user_process_signals (void)*/
/*
 *	sets up signal handling for KRoC
 */
static void set_user_process_signals (void)
{
	/* Process termination signals */
	signal (SIGHUP, user_signal_good_exit);	/* hangup */
	#if 0
	signal (SIGEMT, user_bad_exit);		/* emulator trap */
	#endif
	signal (SIGTERM, user_signal_good_exit); /* software termination */
	signal (SIGPIPE, user_signal_good_exit); /* broken pipe -- usually from "prog | head -10" or similar */

	/* Only catch if not being ignored */
	if (signal (SIGINT, SIG_IGN) != SIG_IGN) {
		signal (SIGINT, user_signal_good_exit);	/* interrupt (^C) */
	}
	if (signal (SIGQUIT, SIG_IGN) != SIG_IGN) {
		signal (SIGQUIT, user_signal_quit_exit); /* quit */
	}

	/* stop signal generated from keyboard (^Z) */
	/* Only catch this if running with job-control */
	if (signal (SIGTSTP, SIG_IGN) == SIG_DFL) {
		signal (SIGTSTP, user_stop_signal);
	}
}
/*}}}*/

/* user-process code below here */

/*{{{  void user_process (bool is_a_tty)*/
void user_process (bool is_a_tty)
{
	static int sigjmpcode;

	/* kroc channels */
	kroc_in = stdin;
	kroc_out = stdout;
	kroc_err = stderr;

	/* user_process - run occam */
	stdin_is_tty = is_a_tty;

	sigjmpcode = sigsetjmp (signal_jump_buffer, 0);
	if (sigjmpcode) {
		goto signalled;
	}
	set_user_process_signals ();

	_occ_enter ();

	/* bad exit - fell through */
	userproc_exit (1, 0);

signalled:
	switch (sigjmpcode) {
		case 1:
			/* good exit */
			userproc_exit (0, 0);
			break;
		case 2:
			/* bad exit */
			userproc_exit (1, 0);
			break;
		default:
			/* quit exit or other */
			userproc_exit (0, 0);
			break;
	}
}
/*}}}*/

