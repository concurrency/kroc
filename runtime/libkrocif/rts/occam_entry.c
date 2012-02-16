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

/* Things that will be defined by the program we're linked into. */

__EXTERN_FUNCTION(void _occam_start (void))	/* occam entry point */

__EXTERN_VARIABLE(word _wsbytes)		/* occam workspace size */
__EXTERN_VARIABLE(word _vsbytes)		/* occam vectorspace size */
__EXTERN_VARIABLE(word _msbytes)		/* occam mobilespace size */
__EXTERN_VARIABLE(int _occam_tlp_iface)		/* translator added description of the top-level process */
__EXTERN_VARIABLE(int _occam_errormode)		/* non-zero for STOP errormode */

static byte *ws, *vs, *ms;

static word kse_ptrs[3];	/* indirect top-level params for top-level SUSPEND */

/* want these available to the IO code */
int using_keyboard = 0;
int using_screen = 0;
int using_error = 0;
int do_print_memstats = 0;
/*}}}*/

#define DEBUG_PRINT(fmt,...)	fprintf (stderr, fmt, ## __VA_ARGS__) /* ISO C */
#define INITIAL_WS_SEP		128
#define INITIAL_VS_SEP		256
#define INITIAL_WS              ((_wsbytes + INITIAL_WS_SEP + _vsbytes + INITIAL_VS_SEP))

/*{{{  int occam_uses_keyboard (void)*/
/*
 *	returns non-zero if the keyboard channel was specified at the top-level
 */
int occam_uses_keyboard (void)
{
	switch (_occam_tlp_iface & ~TLP_SHAREDMASK & ~TLP_EFLAGMASK) {		/* mask out shared -- and EFLAGs */
	case TLP_KYB:
	case TLP_KYBSCR:
	case TLP_KYBERR:
	case TLP_KYBSCRERR:
		using_keyboard = 1;
		return 1;
	}
	return 0;
}
/*}}}*/

/*{{{  void _occ_enter (void)*/
/*
 *	called to initialise and run the occam program
 */
void _occ_enter (void)
{
	word *Wptr, *Fptr, *Bptr;
	int wspptr, i;

	#ifdef DEBUG_KERNEL
	DEBUG_PRINT ("in _enter()\n");
	#endif

	init_occam_io (_occam_tlp_iface);

	ws = (byte *) dmem_alloc (INITIAL_WS);
	if (!ws) {
		DEBUG_PRINT ("KRoC: fatal: unable to allocate workspace/vectorspace (%d bytes)\n", INITIAL_WS);
		user_bad_exit ();
	} else {
		/* initialise the workspace/vectorspace with (MOSTNEG INT) */

		for (i=0; i < (INITIAL_WS >> 2); i++) {
			((word *) ws)[i] = MostNeg;
		}
		
		#if defined(DM_DEBUG) && (DM_DEBUG == 1)
		extadd_ord_mem (ws, _wsbytes + INITIAL_WS_SEP, MODE_READ | MODE_WRITE);
		if (_vsbytes) {
			extadd_ord_mem (ws + (_wsbytes + INITIAL_WS_SEP + 128), _vsbytes, MODE_READ | MODE_WRITE);
		}
		#endif
	}
	Wptr = (word *)ws;

	#ifdef DEBUG_KERNEL
	DEBUG_PRINT ( "allocated %d bytes for workspace and vectorspace.\n", INITIAL_WS );
	#endif

	Wptr = (word *)((byte *)Wptr + _wsbytes + (INITIAL_WS_SEP >> 1));
	wspptr = 1;

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
	do_print_memstats = (_occam_tlp_iface & TLP_PRINT_MEMSTATS);
	switch (_occam_tlp_iface & ~TLP_EFLAGMASK) {
	case TLP_FSTS:
		Wptr[wspptr++] = (word) NULL;		/* causes fault */
		Wptr[wspptr++] = (word) NULL;		/* causes fault */
		break;
	case TLP_FSTSMEM:
		Wptr[wspptr++] = (word) NULL;		/* causes fault */
		Wptr[wspptr++] = (word) NULL;		/* causes fault */
		Wptr[wspptr++] = (word) NULL;		/* causes fault */
		break;
	case TLP_FSTSSIZEDMEM:
		Wptr[wspptr++] = (word) NULL;		/* causes fault */
		Wptr[wspptr++] = (word) NULL;		/* causes fault */
		Wptr[wspptr++] = (word) NULL;		/* causes fault */
		Wptr[wspptr++] = 0;			/* dimemsion */
		break;
	default:
		switch (_occam_tlp_iface & ~TLP_SHAREDMASK & ~TLP_EFLAGMASK) {		/* mask out shared -- and EFLAGs */
		case TLP_NULL:
			break;
		case TLP_KYB:
			Wptr[wspptr++] = (_occam_tlp_iface & TLP_MPP_BARRIER) ? (word)&(kse_ptrs[0]) : (word) kbd_chan_addr ();
			using_keyboard = 1;
			break;
		case TLP_SCR:
			Wptr[wspptr++] = (_occam_tlp_iface & TLP_MPP_BARRIER) ? (word)&(kse_ptrs[1]) : (word) scr_chan_addr ();
			using_screen = 1;
			break;
		case TLP_ERR:
			Wptr[wspptr++] = (_occam_tlp_iface & TLP_MPP_BARRIER) ? (word)&(kse_ptrs[2]) : (word) err_chan_addr ();
			using_error = 1;
			break;
		case TLP_KYBSCR:
			Wptr[wspptr++] = (_occam_tlp_iface & TLP_MPP_BARRIER) ? (word)&(kse_ptrs[0]) : (word) kbd_chan_addr ();
			Wptr[wspptr++] = (_occam_tlp_iface & TLP_MPP_BARRIER) ? (word)&(kse_ptrs[1]) : (word) scr_chan_addr ();
			using_keyboard = using_screen = 1;
			break;
		case TLP_KYBERR:
			Wptr[wspptr++] = (_occam_tlp_iface & TLP_MPP_BARRIER) ? (word)&(kse_ptrs[0]) : (word) kbd_chan_addr ();
			Wptr[wspptr++] = (_occam_tlp_iface & TLP_MPP_BARRIER) ? (word)&(kse_ptrs[2]) : (word) err_chan_addr ();
			using_keyboard = using_error = 1;
			break;
		case TLP_SCRERR:
			Wptr[wspptr++] = (_occam_tlp_iface & TLP_MPP_BARRIER) ? (word)&(kse_ptrs[1]) : (word) scr_chan_addr ();
			Wptr[wspptr++] = (_occam_tlp_iface & TLP_MPP_BARRIER) ? (word)&(kse_ptrs[2]) : (word) err_chan_addr ();
			using_screen = using_error = 1;
			break;
		case TLP_KYBSCRERR:
			Wptr[wspptr++] = (_occam_tlp_iface & TLP_MPP_BARRIER) ? (word)&(kse_ptrs[0]) : (word) kbd_chan_addr ();
			Wptr[wspptr++] = (_occam_tlp_iface & TLP_MPP_BARRIER) ? (word)&(kse_ptrs[1]) : (word) scr_chan_addr ();
			Wptr[wspptr++] = (_occam_tlp_iface & TLP_MPP_BARRIER) ? (word)&(kse_ptrs[2]) : (word) err_chan_addr ();
			using_keyboard = using_screen = using_error = 1;
			break;
		default:
			fprintf (stderr, "KRoC: fatal: unknown interface code for top-level occam process (%d)\n", _occam_tlp_iface);
			user_bad_exit();
			break;
		}
		break;
	}

	if (_vsbytes) {
		vs = ws + (_wsbytes + INITIAL_WS_SEP + 128);
		Wptr[wspptr++] = (word) vs;
	}

	if (_msbytes) {
		ms = (byte *) dmem_alloc (_msbytes);
		#if defined(DM_DEBUG) && (DM_DEBUG == 1)
			extadd_ord_mem (ms, _msbytes, MODE_READ | MODE_WRITE);
		#endif
		/* initialise mobile-space to MOSTNEG INT */
		for (i = 0; i < (_msbytes >> 2); i++) {
			((word *) ms)[i] = MostNeg;
		}
		Wptr[wspptr++] = (word) ms;
	} else {
		ms = NULL;
	}
	
	#ifdef DEBUG_KERNEL
	DEBUG_PRINT ("KRoC: passing mobilespace pointer to application (%p for %d words)\n", ms, _msbytes >> 2);
	#endif

	/* if the FORK barrier is required, add it */
	if (_occam_tlp_iface & TLP_FORK_BARRIER) {
		Wptr[wspptr++] = (word) ccsp_mt_alloc (MT_MAKE_BARRIER (MT_BARRIER_FORKING), 0);
	}
	
	#ifdef DEBUG_KERNEL
	DEBUG_PRINT ("Wptr=%p  ws=%p  vs=%p  _wsbytes=%d  _vsbytes=%d\n", Wptr, ws, vs, _wsbytes, _vsbytes );
	#endif

	Fptr = NotProcess_p;
	Bptr = NotProcess_p;

	#if defined(ENABLE_DTRACES)
	/* set process/application workspace */
	do_dtrace ("IWAL", ws, _wsbytes);
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

	ccsp_set_error_mode (_occam_errormode);
	ccsp_occam_entry (ws, _wsbytes + INITIAL_WS_SEP, (word) _occam_start, Wptr, Fptr);
}
/*}}}*/

/*{{{  int _occ_exit (void)*/
/*
 *	cleans up after the occam program has finished
 */
int _occ_exit (void)
{
	if (ms) {
		dmem_release (ms);
	}
	
	dmem_release (ws);

	ws = vs = NULL;

	return 0;
}
/*}}}*/

