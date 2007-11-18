/*
 *	CCSP library core interface code
 *	Copyright (C) 1995, 1996, 1997 D.C.Wood, P.H.Welch
 *	Modified for RMOX by Brian Vinter, 2002
 *	Modification Copyright (C) 2007 Carl Ritson <cgr@kent.ac.uk>
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
 * RMOX hacks by Brian Vinter and Fred Barnes
 */

#define RTS_MAIN

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
	#include <string.h>

	#ifdef ENABLE_CPU_TIMERS
		#include <pwd.h>
		#include <sys/types.h>
	#endif

#endif	/* !RMOX_BUILD */

#include <kernel.h>
#include <bsyscalls_if.h>
#include <deadlock.h>
#include <dmem_if.h>
#include <rts.h>

#if defined(ENABLE_DTRACES) && !defined(RMOX_BUILD)
	#include <dtrace.h>
#endif

/*{{{  RTS variables*/
#ifdef DEBUG_RTS
bool ccsp_trace				= false;
#endif
bool ccsp_ignore_errors			= false;
void (*ccsp_exit_handler)(int, bool)	= NULL;
char *ccsp_branding			= "CCSP";

static bool ee_bsc 			= false;
static bool (*ee_blocked)(void)		= NULL;
static bool (*ee_ready)(void)		= NULL;
/*}}}*/

/*{{{  void ccsp_set_branding (char *str)*/
void ccsp_set_branding (char *str)
{
	ccsp_branding = str;
}
/*}}}*/

/*{{{  void ccsp_set_error_mode (int val)*/
void ccsp_set_error_mode (int val)
{
	if (val) {
		ccsp_ignore_errors = true;
	}
}
/*}}}*/

/*{{{  bool ccsp_blocked_on_external_event (void) */
bool ccsp_blocked_on_external_event (void)
{
	if (ee_blocked != NULL) {
		return ee_blocked ();
	} else {
		return false;
	}
}
/*}}}*/

/*{{{  bool ccsp_external_event_is_bsc (void) */
bool ccsp_external_event_is_bsc (void)
{
	return ee_bsc;
}
/*}}}*/

/*{{{  bool ccsp_external_event_is_ready (void) */
bool ccsp_external_event_is_ready (void)
{
	if (ee_ready != NULL) {
		return ee_ready ();
	} else {
		return false;
	}
}
/*}}}*/

/*{{{  void ccsp_set_external_event_hook (bool bsc, bool (*blocked)(void), bool (*ready)(void))*/
void ccsp_set_external_event_hook (bool bsc, bool (*blocked)(void), bool (*ready)(void))
{
	ee_bsc		= bsc;
	ee_blocked	= blocked;
	ee_ready	= ready;
}
/*}}}*/

/*{{{  void ccsp_default_exit_handler (int status, bool core)*/
void ccsp_default_exit_handler (int status, bool core)
{
	#if defined(GENERATE_CORES)
	if (core) {
		MESSAGE ("fatal error code %d, core dumped\n", exit_status);
		FFLUSH (stderr);
		abort ();
	}
	#endif

	#if !defined(RMOX_BUILD)
	_exit (status);
	#else
	panic ("exiting CCSP");
	#endif
}
/*}}}*/

/*{{{  void ccsp_set_exit_handler (void (*func)(int, bool))*/
void ccsp_set_exit_handler (void (*func)(int, bool))
{
	if (func != NULL) {
		ccsp_exit_handler = func;
	} else {
		ccsp_exit_handler = ccsp_default_exit_handler;
	}
}
/*}}}*/

/*{{{  void ccsp_exit (int status, bool dump_core)*/
void ccsp_exit (int status, bool dump_core)
{
	ccsp_kernel_exit (status, dump_core);
}
/*}}}*/

/*{{{  bool ccsp_init (void)*/
bool ccsp_init (void)
{
	#ifdef DEBUG_RTS
	ccsp_trace = true;
	#endif

	dmem_init ();

	ccsp_set_exit_handler (NULL);
  
	#ifdef ENABLE_CPU_TIMERS
	if (!ccsp_calibrate_timers ()) {
		BMESSAGE ("unable to automatically detect CPU speed on this machine.\n");
		MESSAGE ("Please create a file .kroc_clock, ~/.kroc_clock or /etc/kroc_clock, containing\n");
		MESSAGE ("your CPU speed in MHz.\n");
		return false;
	}
	#endif

	#if defined(ENABLE_DTRACES)
	ccsp_init_dtraces ();
	#endif

	ccsp_kernel_init ();
	ccsp_deadlock_init ();

	#if defined(BLOCKING_SYSCALLS)
	if (bsyscalls_create_clones ()) {
		BMESSAGE ("unable to create clones for blocking syscalls.\n");
		return false;
	}
	#endif

	return ccsp_user_process_init ();
}
/*}}}*/

