/*
 *	Occam entry point
 *	Copyright (C) 1996-1999 Jim Moores
 *	Based on the KRoC/sparc kernel Copyright (C) 1994-2000 D.C. Wood and P.H. Welch
 *	Modifications copyright (C) 1999-2005 Fred Barnes  <frmb@kent.ac.uk>
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

/*{{{  includes*/
#include <stdlib.h>
#include <stdio.h>
#include <setjmp.h>

#ifdef USE_PTHREADS
	#include <pthread.h>
#endif

#include <rts.h>
/*}}}*/
/*{{{  local/global vars*/

#if !defined(__GNUC__)
#warning "Unable to define weak symbols"
#define NO_WEAK_SYMBOLS
#endif

#ifdef NO_WEAK_SYMBOLS
#define __EXTERN_FUNCTION(X) extern X;
#define __EXTERN_VARIABLE(X) extern X;
#else
#define __WEAK __attribute__ ((weak))
#define __EXTERN_FUNCTION(X) __WEAK X {}
#define __EXTERN_VARIABLE(X) X __WEAK = 0;
#endif

static byte *ws, *vs, *ms;

static word kse_ptrs[3];	/* indirect top-level params for top-level SUSPEND */

/* want these available to the IO code */
int using_keyboard = 0;
int using_screen = 0;
int using_error = 0;
int do_print_memstats = 0;
/*}}}*/

#define DEBUG_PRINT(fmt,...)	fprintf (stderr, fmt, ## __VA_ARGS__) /* ISO C */
#define WS_SEP			32
#define VS_SEP			64

/*{{{  void occam_parse_tlp (char **tlp_desc)*/
void occam_parse_tlp (char **tlp_desc)
{
	int i;
	for (i = 0; tlp_desc[i] != NULL; ++i) {
		const char *name = (const char *) tlp_desc[i];
		
		if (strcmp (name, "keyboard?") == 0) {
			using_keyboard = 1;
		} else if (strcmp (name, "screen!") == 0) {
			using_screen = 1;
		} else if (strcmp (name, "error!") == 0) {
			using_error = 1;
		}
	}
}
/*}}}*/

/*{{{  int occam_uses_keyboard (void)*/
/*
 *	returns non-zero if the keyboard channel was specified at the top-level
 */
int occam_uses_keyboard (void)
{
	return using_keyboard;
}
/*}}}*/

/*{{{  void occam_start (void)*/
/*
 *	called to initialise and run the occam program
 */
void occam_entry (char **tlp_desc,
		void *start_proc, void *code_exit, 
		int ws_words, int vs_words, int ms_words)
{
	word *Wptr, *Fptr, *Bptr;
	word *fbar = NULL;
	int initial_ws = ws_words + WS_SEP + vs_words + VS_SEP; 
	int wspptr, i;

	#ifdef DEBUG_KERNEL
	DEBUG_PRINT ("in _enter()\n");
	#endif

	init_occam_io ();

	ws = (byte *) dmem_alloc (initial_ws * sizeof (word));
	if (!ws) {
		DEBUG_PRINT ("KRoC: fatal: unable to allocate workspace/vectorspace (%d words)\n", initial_ws);
		user_bad_exit ();
	} else {
		/* initialise the workspace/vectorspace with (MOSTNEG INT) */
		for (i = 0; i < initial_ws; i++) {
			((word *) ws)[i] = MostNeg;
		}
		
		#if defined(DM_DEBUG) && (DM_DEBUG == 1)
		extadd_ord_mem (ws, (ws_words + WS_SEP) * sizeof (word), MODE_READ | MODE_WRITE);
		if (vs_words) {
			extadd_ord_mem (vs, vs_words * sizeof (word), MODE_READ | MODE_WRITE);
		}
		#endif
	}
	if (vs_words) {
		vs = ws + ((ws_words + WS_SEP + (VS_SEP / 2)) * sizeof (word));
	} else {
		vs = NULL;
	}

	if (ms_words) {
		ms = (byte *) dmem_alloc (ms_words * sizeof (word));
		#if defined(DM_DEBUG) && (DM_DEBUG == 1)
			extadd_ord_mem (ms, ms_words * sizeof (word), MODE_READ | MODE_WRITE);
		#endif
		/* initialise mobile-space to MOSTNEG INT */
		for (i = 0; i < ms_words; i++) {
			((word *) ms)[i] = MostNeg;
		}
		#ifdef DEBUG_KERNEL
		DEBUG_PRINT ("KRoC: passing mobilespace pointer to application (%p for %d words)\n", ms, _msbytes >> 2);
		#endif
	} else {
		ms = NULL;
	}
	
	Wptr = (word *) ws;

	#ifdef DEBUG_KERNEL
	DEBUG_PRINT ( "allocated %d bytes for workspace and vectorspace.\n", initial_ws);
	#endif

	Wptr 		= Wptr + ws_words + (WS_SEP / 2);
	Wptr[Iptr] 	= (word) start_proc;
	Wptr[0]		= (word) code_exit;
	wspptr 		= 1;

	#if 0
	/* check for any MPP barrier at the top-level */
	if (_occam_tlp_iface & TLP_MPP_BARRIER) {
		Wptr[wspptr++] 	= (word) NULL;
		kse_ptrs[0] 	= (word) kbd_chan_addr ();
		kse_ptrs[1] 	= (word) scr_chan_addr ();
		kse_ptrs[2] 	= (word) err_chan_addr ();
		#if defined(DM_DEBUG) && (DM_DEBUG == 1)
		extadd_ord_mem (kse_ptrs, 3 * sizeof (int), MODE_READ | MODE_WRITE);
		#endif
	}
	#endif
	
	/* do_print_memstats = (_occam_tlp_iface & TLP_PRINT_MEMSTATS); */
	
	for (i = 0; tlp_desc[i] != NULL; ++i) {
		const char *name = (const char *) tlp_desc[i];
		if (strcmp (name, "keyboard?") == 0) {
			Wptr[wspptr++] = (word) kbd_chan_addr ();
		} else if (strcmp (name, "screen!") == 0) {
			Wptr[wspptr++] = (word) scr_chan_addr ();
		} else if (strcmp (name, "error!") == 0) {
			Wptr[wspptr++] = (word) err_chan_addr ();
		} else if (strcmp (name, "VSPTR") == 0) {
			Wptr[wspptr++] = (word) vs;
		} else if (strcmp (name, "MSPTR") == 0) {
			Wptr[wspptr++] = (word) ms;
		} else if (strcmp (name, "FBARPTR") == 0) {
			if (fbar == NULL) {
				fbar = (word *) ccsp_mt_alloc (MT_MAKE_BARRIER (MT_BARRIER_FORKING), 0);
			}
			Wptr[wspptr++] = (word) fbar;
		} else {
			Wptr[wspptr++] = 0;
		}
	}
	
	Fptr = NotProcess_p;
	Bptr = NotProcess_p;

	#if defined(ENABLE_DTRACES)
	/* set process/application workspace */
	do_dtrace ("IWAL", ws, ws_words * sizeof (word));
	#endif

	/* put the screen and error processes on the run-queue.  init_occam_io makes sure
	 * that they are correctly setup (and will jump into the actual PROCs) */
	Fptr = scr_workspace ();
	Bptr = err_workspace ();
	Fptr[Link] = (word) Bptr;
	Bptr[Link] = NotProcess_p;
	Fptr[Priofinity] = 0;
	Bptr[Priofinity] = 0;

	if (using_keyboard) {
		/* place keyboard process on the run queue */
		Bptr[Link] 		= (word) kbd_workspace ();
		Bptr 			= (word *) Bptr[Link];
		Bptr[Link]		= NotProcess_p;
		Bptr[Priofinity]	= 0;
	}

	#ifdef DEBUG_KERNEL
	DEBUG_PRINT ("about to call _occam_start()\n");
	#endif

	//ccsp_set_error_mode (_occam_errormode);
	ccsp_kernel_entry (Wptr, Fptr);
	
	if (ms) {
		dmem_release (ms);
	}
	
	dmem_release (ws);

	ws = vs = ms = NULL;
}
/*}}}*/

