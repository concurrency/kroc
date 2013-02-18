/*
 *	KRoC scheduler startup code for occam
 *	Copyright (C) 1995, 1996, 1997 D.C.Wood, P.H.Welch
 *	Modified for OOS by Brian Vinter, 2002
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
 * $Source: /proj/ofa/rts/RCS/rtsmain.c,v $
 *
 * $Id: rtsmain.c,v 1.17 1997/09/03 15:42:33 djb1 Exp $
 *
 * (C) Copyright 1995,1996,1997 D.C. Wood <D.C.Wood@ukc.ac.uk> and 
 *                              P.H. Welch <P.H.Welch@ukc.ac.uk>
 * University of Kent at Canterbury
 *
 * Based on work by: Peter Shephard @ QMC Sept. 1984 for
 * UNIX OPS 2.0 (Release 2.0.1 : 24th. July, 1986)
 *
 * OOS hacks by Brian Vinter and Fred Barnes
 */

#define RTS_MAIN

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <rts.h>
#include <kroc_io.h>

/*{{{  common variables */
char *short_cmdline	= NULL;
char *long_cmdline	= NULL;

int kroc_argc		= 0;
char **kroc_argv	= NULL;

static int stdin_is_tty;
/*}}}*/

/*{{{  static void savecmdline (int argc, char **argv)*/
static void savecmdline (int argc, char **argv)
{
	static char cmdline[512];
	int i = 0;

	cmdline[0] = '\0';
	long_cmdline = &cmdline[0];
	short_cmdline = &cmdline[0];
	while (i < argc) {
		strcat (cmdline, argv[i]);
		i++;
		if (i < argc) {
			strcat (cmdline, " ");  /* separate arguments except the last one */
		}
		if (i == 1) {
			short_cmdline = &cmdline[strlen(cmdline)];
		}
	}
}
/*}}}*/

/*{{{  static void exit_handler (int status, bool core)*/
static void exit_handler (int status, bool core)
{
	if (stdin_is_tty && occam_uses_keyboard ()) {
		restore_tty_state ();
	}

	ccsp_default_exit_handler (status, core);
}
/*}}}*/

/*{{{  int main (int argc, char **argv)*/
/*
 *	start here
 */
#if defined(__GNUC__) && !defined(HOSTOS_CYGWIN)
__attribute__ ((weak))
#endif
int main (int argc, char **argv)
{
	kroc_argc = argc;
	kroc_argv = argv;
  
	savecmdline (argc, argv);

	stdin_is_tty = isatty (0);

	if (stdin_is_tty && occam_uses_keyboard ()) {
		save_tty_state ();
	} else {
		stdin_is_tty = 0;
	}

	ccsp_set_branding ("KRoC");

	if (!ccsp_init()) {
		if (stdin_is_tty && occam_uses_keyboard ()) {
			restore_tty_state ();
		}
		exit (1);
	}

	ccsp_set_exit_handler (exit_handler);

	if (occam_uses_keyboard ()) {
		ccsp_set_external_event_hook (true, process_blocked_on_kbd, kbd_ready);
	}

	init_kbdio (stdin_is_tty);
	user_process (stdin_is_tty);

	return 0;
}
/*}}}*/

