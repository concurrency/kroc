/*
 *	KRoC TTY support
 *	Copyright (C) 1995, 1996, 1997  D.J. Beckett
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
 * $Source: /proj/ofa/rts/RCS/tty.c,v $
 *
 * $Id: tty.c,v 1.5 1997/09/03 15:41:21 djb1 Exp $
 *
 * (C) Copyright 1995,1996,1997 D.J. Beckett <D.J.Beckett@ukc.ac.uk>
 * University of Kent at Canterbury
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <rts.h>
#include <assert.h>

#ifdef HAVE_SYS_IOCTL
#include <sys/ioctl.h>
#endif

static struct termios saved_termios;
static int tty_saved = FALSE;

void save_tty_state (void)
{
	struct termios term;
  
	/*  reset termio stuff */
	if (tcgetattr (0, &term) != 0) {
		fprintf (stderr, "KRoC: failed to get tty state!\n");
		return;
	}
	tty_saved = TRUE;

	saved_termios = term;
  
	/* Modify basic terminal input control
     
	Turn off:
		INLCR  : Map NL to CR on input
		IGNCR  : Ignore of CR on input
		ICRNL  : Map CR to NL on input
     
	Turn off
		ICANON : Canonical input (erase, kill, NL, EOF, EOL).
		If this is off, a read request is not satisfied until either:
			a) The minimum number of characters specified by MIN are
			   received
			OR
			b) The timeout value specified by TIME has expired since the
			   last character was received. (0.1 sec)
		ECHO   : Echoing characters as received
	*/

	#if 0
		term.c_iflag &= ~(INLCR | IGNCR | ICRNL);
	#endif
	term.c_lflag &= ~(ICANON | ECHO);

	term.c_cc[VMIN]     = 1;    /* Minimum read value (see above) */
	term.c_cc[VTIME]    = 100;  /* Intercharacter timer val (see above) in .1s */

	#if 0
		/* Disable signal-generating input characters */
		term.c_cc[VINTR]    = 0;    /* Sends SIGINT   (^C)                */
		term.c_cc[VQUIT]    = 0;    /* Sends SIGQUIT  (^| or ^V)          */
		term.c_cc[VSUSP]    = 0;    /* Sends SIGTSTP  (^Z)                */
		/*        VDSUSP               Sends SIGTSTP  (^Y)                */
  
		/* Disable input editting characters */
		/* In ICANON mode */
		term.c_cc[VERASE]   = 0;    /* Erase          (Backspace)         */
		term.c_cc[VKILL]    = 0;    /* Kill line      (^U)                */
		/*        VEOF                 End of file    (^D)                */
		/*        VEOL                 End of line    (not normally used) */
		term.c_cc[VEOL2]    = 0;    /* Additional EOL (not normally used) */
		term.c_cc[VREPRINT] = 0;    /* Reprint        (^R)                */
		term.c_cc[VWERASE]  = 0;    /* Word erase     (^W)                */
		/*        VSWTCH               ?              (not normally used) */
  
		/* Disable others input characters */
		term.c_cc[VSTART]   = 0;    /* Resume output  (^Q)                */
		term.c_cc[VSTOP]    = 0;    /* Stop output    (^S)                */
		term.c_cc[VDISCARD] = 0;    /* Discard output (^O)                */
		term.c_cc[VLNEXT]   = 0;    /* Literal next   (^V)                */
	#endif

	if (tcsetattr (0, TCSAFLUSH, &term) != 0) {
		fprintf (stderr, "KRoC: failed to set tty state!\r\n");
	}
	return;  
}
  

void restore_tty_state (void)
{
	if (tty_saved) {
		/* output the VT220 "visible cursor" sequence */
		fprintf (stdout, "\x1b[?25h");
		fflush (stdout);
		if (tcsetattr (0, TCSAFLUSH, &saved_termios) != 0) {
			fprintf (stderr, "KRoC: failed to restore tty state!\r\n");
		}
		tty_saved = FALSE;
	}
	return;
}

