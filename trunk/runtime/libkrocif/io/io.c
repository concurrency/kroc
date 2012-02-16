/*
 *	krocif.c -- kroc interface (separated out from CCSP run-time)
 *	Copyright (C) 2006 Fred Barnes <frmb@kent.ac.uk>
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>

/* link with CCSP */
#include <ccsp.h>

/* local headers */
#include <tlpcodes.h>
#include <kroc_io.h>

/* constants */
#define KBD_WORKSPACE_WORDS 18
#define SCR_WORKSPACE_WORDS 36
#define ERR_WORKSPACE_WORDS 12

static int stdin_is_tty;

extern int using_keyboard;
extern FILE *kroc_out, *kroc_err;

/*{{{  workspaces/channels/etc. for keyboard, screen and error processes*/
static word *kbd_workspace_bottom;
static word *scr_workspace_bottom;
static word *err_workspace_bottom;

/* pointers to the right offsets into each workspace for convenience */
static word *kbd_ws;
static word *scr_ws;
static word *err_ws;

/* actual locations holding channel-words -- large if SHARED anon-ct */
static word **kbd_chan;
static word **scr_chan;
static word **err_chan;

static word *kbd_termchan;

/* defined in generate code */
extern void O_kroc_screen_process (void);
extern void O_kroc_error_process (void);
extern void O_kroc_keyboard_process (void);
/*}}}*/

/*{{{  static word **setup_chan (word init_state) */
static word **setup_chan (word init_state)
{
	mt_cb_t *cb = ccsp_mt_alloc (
		MT_SIMPLE | MT_MAKE_TYPE (MT_CB) | MT_CB_SHARED | MT_CB_STATE_SPACE, 
		1
	);

	/* cb->channels[0] = NotProcess_p; */
	mt_cb_get_pony_state (cb)->state = init_state;

	return (word **) cb;
}
/*}}}*/

/*{{{  void init_occam_io (int tlpiface)*/
/*
 *	initialises the occam IO workspaces
 */
int init_occam_io (int tlpiface)
{
	int i;

	#if defined(DM_DEBUG) && (DM_DEBUG == 1)
		extadd_ord_mem (_kbd_workspace_bottom, 13 * sizeof(word), MODE_READ | MODE_WRITE);
		extadd_ord_mem (_scr_workspace_bottom, 128 * sizeof(word), MODE_READ | MODE_WRITE);
		extadd_ord_mem (_err_workspace_bottom, 12 * sizeof(word), MODE_READ | MODE_WRITE);
		extadd_ord_mem (kbd_chan, 8 * sizeof(word), MODE_READ | MODE_WRITE);
		extadd_ord_mem (scr_chan, 8 * sizeof(word), MODE_READ | MODE_WRITE);
		extadd_ord_mem (err_chan, 8 * sizeof(word), MODE_READ | MODE_WRITE);
	#endif

	kbd_workspace_bottom = (word *) dmem_alloc (KBD_WORKSPACE_WORDS * sizeof(word));
	for (i = 0; i < KBD_WORKSPACE_WORDS; ++i) {
		kbd_workspace_bottom[i] = 0;
	}

	scr_workspace_bottom = (word *) dmem_alloc (SCR_WORKSPACE_WORDS * sizeof(word));
	for (i = 0; i < SCR_WORKSPACE_WORDS; ++i) {
		scr_workspace_bottom[i] = 0;
	}

	err_workspace_bottom = (word *) dmem_alloc (ERR_WORKSPACE_WORDS * sizeof(word));
	for (i = 0; i < ERR_WORKSPACE_WORDS; ++i) {
		err_workspace_bottom[i] = 0;
	}
	
	kbd_chan = setup_chan (0x00020001); /* server shared not claimed, client unshared */
	scr_chan = setup_chan (0x00010002); /* client shared not claimed, server unshared */
	err_chan = setup_chan (0x00010002); /* client shared not claimed, server unshared */

	kbd_ws = &(kbd_workspace_bottom[KBD_WORKSPACE_WORDS - 4]);
	kbd_ws[-3] = 0;
	kbd_ws[-2] = (word) NotProcess_p;
	kbd_ws[-1] = (word) O_kroc_keyboard_process;
	kbd_ws[0] = 0;
	kbd_ws[1] = (word) kbd_chan;
	kbd_ws[2] = (word) &kbd_termchan;
	kbd_termchan = NotProcess_p;

	scr_ws = &(scr_workspace_bottom[SCR_WORKSPACE_WORDS - 4]);
	scr_ws[-3] = 0;
	scr_ws[-2] = (word) NotProcess_p;
	scr_ws[-1] = (word) O_kroc_screen_process;
	scr_ws[0] = 0;
	scr_ws[1] = (word) scr_chan;
	scr_ws[2] = 0;

	err_ws = &(err_workspace_bottom[ERR_WORKSPACE_WORDS - 4]);
	err_ws[-3] = 0;
	err_ws[-2] = (word) NotProcess_p;
	err_ws[-1] = (word) O_kroc_error_process;
	err_ws[0] = 0;
	err_ws[1] = (word) err_chan;
	err_ws[2] = 0;

	return 0;
}
/*}}}*/
/*{{{  bool kbd_ready (void)*/
/*
 *	returns true if the keyboard handler is blocked on the keyboard
 *	channel.
 */
bool kbd_ready (void)
{
	if ((kbd_chan[0] != NotProcess_p) && (kbd_chan[0] >= kbd_workspace_bottom) && (kbd_chan[0] <= kbd_ws)) {
		return true;
	}
	return false;
}
/*}}}*/
/*{{{  bool process_blocked_on_kbd (void)*/
/*
 *	returns true if a process other than the keyboard handler is
 *	blocked on the keyboard channel.
 */
bool process_blocked_on_kbd (void)
{
	if ((kbd_chan[0] != NotProcess_p) && ((kbd_chan[0] < kbd_workspace_bottom) || (kbd_chan[0] > kbd_ws))) {
		return true;
	}
	return false;
}
/*}}}*/


/*{{{  word *kbd_chan_addr (void)*/
word *kbd_chan_addr (void)
{
	return (word *) kbd_chan;
}
/*}}}*/
/*{{{  word *scr_chan_addr (void)*/
word *scr_chan_addr (void)
{
	return (word *) scr_chan;
}
/*}}}*/
/*{{{  word *err_chan_addr (void)*/
word *err_chan_addr (void)
{
	return (word *) err_chan;
}
/*}}}*/

/*{{{  word *kbd_workspace (void)*/
word *kbd_workspace (void)
{
	return (word *) kbd_ws;
}
/*}}}*/
/*{{{  word *scr_workspace (void)*/
word *scr_workspace (void)
{
	return (word *) scr_ws;
}
/*}}}*/
/*{{{  word *err_workspace (void)*/
word *err_workspace (void)
{
	return (word *) err_ws;
}
/*}}}*/


/*{{{  void init_kbdio (int is_a_tty)*/
/*
 *	initialises keyboard (or input) stuff
 */
void init_kbdio (int is_a_tty)
{
	stdin_is_tty = is_a_tty;
	return;
}
/*}}}*/
#if 0
/*{{{  int kill_kbdio (void)*/
/*
 *	kills the "keyboard process" (which might be blocked in terminal read)
 */
int kill_kbdio (void)
{
	word ws_arry[2];
	int status;

	if (using_keyboard) {
		ws_arry[0] = (word) &kbd_termchan;
		ws_arry[1] = (word) &status;
		_killcall (ws_arry);
	} else {
		status = 0;
	}
	return status;
}
/*}}}*/
#endif
/*{{{  void _read_keyboard (int *wsptr)*/
/*
 *	reads the keyboard and places a byte at (char *)wsptr[0]
 */
void _read_keyboard (int *wsptr)
{
	int *ch = (int *)(wsptr[0]);
	int c_read, n;
	unsigned char tty_char;

	if (!stdin_is_tty) {
		c_read = getc (stdin);
		n = (c_read == EOF) ? 0 : 1;
		tty_char = (unsigned char)c_read;
	} else {
		n = read (0, &tty_char, 1);
	}
	if (n < 1) {
		*ch = -1;
	} else {
		*ch = (int)((word) tty_char);
	}
	return;
}
/*}}}*/

/*  output handling*/
#define KROC_OUTPUT_BUFFER_SIZE 256

/*{{{  void _write_screen (int *wsptr)*/
/*
 *	krocif.s calls this to print something on the screen channel
 *	PROC C.write.screen (VAL []BYTE buffer)
 */
void _write_screen (int *wsptr)
{
	const char *buffer = (char *)(wsptr[0]);

	fputs (buffer, kroc_out);
	fflush (kroc_out);
	return;
}
/*}}}*/
/*{{{  void _write_error (int *wsptr)*/
/*
 *	krocif.s calls this to print something on the error channel
 *	PROC C.write.error (VAL BYTE ch)
 */
void _write_error (int *wsptr)
{
	fputc ((int)(wsptr[0]), kroc_err);
	fflush (kroc_err);
	return;
}
/*}}}*/
/*{{{  void _out_stderr (int *wsptr)*/
/*
 *	this is available for debugging
 *	PROC C.out.stderr (VAL []BYTE str)
 */
void _out_stderr (int *wsptr)
{
	const char *buf = (char*)(wsptr[0]);
	int slen = (int)(wsptr[1]);

	fflush (kroc_err);
	write (fileno (kroc_err), buf, slen);
	return;
}
/*}}}*/
/*{{{  void _out_stderr_int (int *wsptr)*/
/*
 *	this is available for debugging
 *	PROC C.out.stderr.int (VAL INT n)
 */
void _out_stderr_int (int *wsptr)
{
	const int n = (int)(wsptr[0]);

	fprintf (kroc_err, "%d", n);
	fflush (kroc_err);
	return;
}
/*}}}*/

