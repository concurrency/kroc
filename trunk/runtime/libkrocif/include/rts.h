/*
 *	KRoC C scheduler definitions and function prototypes
 *	Copyright (C) 1995, 1996, 1997 D.J. Beckett
 *	Modifications for oos (C) 2002 Brian Vinter
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

#include <sys/resource.h>
#include <sys/termios.h>

#if defined(HAVE_SYS_TIMERS_H)
	#include <sys/timers.h>
#endif

#include <signal.h>

#if defined(HAVE_ERRNO_H)
	#include <errno.h>
#endif

#include <ccsp.h>
#include <tlpcodes.h>
#include <kroc_io.h>

#define MESSAGE(fmt,args...) fprintf (stderr,fmt,##args)
#define FFLUSH(stream) fflush(stream)

/* tty chars */
#define NULL_CHAR       (char)0
#define END_CHAN        (char)255

/*{{{ occam_entry.c */
extern int using_keyboard;
extern int using_screen;
extern int using_error;
extern int do_print_memstats;
extern int occam_uses_keyboard (void);
extern void _occ_enter (void);
extern int _occ_exit (void);
/*}}}*/

/*{{{ userproc.c */
extern FILE *kroc_in;
extern FILE *kroc_out;
extern FILE *kroc_err;

extern void user_process (bool is_a_tty);
/*}}}*/

/*{{{ rtsmain.c */
extern char *short_cmdline;
extern char *long_cmdline;

extern int kroc_argc;
extern char **kroc_argv;

extern void user_bad_exit (void);
extern void user_good_exit (void);

extern int main (int argc, char **argv);
/*}}}*/

/*{{{ tty.c */
extern void save_tty_state (void);
extern void restore_tty_state (void);
/*}}}*/

#endif /* rts.h */

