/*
 *	Kernel entry points
 *	Copyright (C) 1996-1999 Jim Moores
 *	Based on the KRoC/sparc kernel Copyright (C) 1994-2000 D.C. Wood and P.H. Welch
 *	Modifications Copyright (C) 1999-2005 Fred Barnes  <frmb@kent.ac.uk>
 *	Modifications Copyright (C) 2007 Carl Ritson <cgr@kent.ac.uk>
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
#ifdef RMOX_BUILD
	#include <rmox_if.h>
#else	/* !RMOX_BUILD */
	#include <stdlib.h>
	#include <stdio.h>
#endif	/* !RMOX_BUILD */

#include <kernel.h>
#include <arch/sched_asm_inserts.h>
#include <deadlock.h>
#include <kiface.h>
/*}}}*/

/*{{{  void ccsp_kernel_entry (word *wptr, word *fptr)*/
void ccsp_kernel_entry (word *wptr, word *fptr)
{
	word stack = (word) (((byte *)&fptr) + sizeof(word));

	stack -= sizeof(sched_t);	/* allocate stack for sched_t structure */
	stack &= 0xfffff000;		/* align the stack on a page boundary */

	K_ENTRY (ccsp_calltable[K_RTTHREADINIT], stack, wptr, fptr);
}
/*}}}*/

/*{{{  void ccsp_occam_entry (word *wptr, word *fptr)*/
/* Entry point to CCSP for occam programs. */
void ccsp_occam_entry (void *ws, unsigned int ws_bytes, word iptr, word *wptr, word *fptr)
{
	wptr[Iptr] = iptr;
	ccsp_give_ws_code ((char *) ws, (int) ws_bytes, (unsigned char *) iptr);
	ccsp_kernel_entry (wptr, fptr);
}
/*}}}*/


/*{{{  void ccsp_cif_bcall0_stub (word *arg)*/
void ccsp_cif_bcall0_stub (word *arg)
{
	typedef word (*_func_ptr)(void);
	_func_ptr func = (_func_ptr) arg[0];
	arg[0] = func();
}
/*}}}*/

/*{{{  void ccsp_cif_bcall1_stub (word *arg)*/
void ccsp_cif_bcall1_stub (word *arg)
{
	typedef word (*_func_ptr)(word);
	_func_ptr func = (_func_ptr) arg[0];
	arg[0] = func(arg[1]);
}
/*}}}*/

/*{{{  void ccsp_cif_bcalln_stub (word *arg)*/
void ccsp_cif_bcalln_stub (word *arg)
{
	void *func	= (void *) arg[0];
	word argc	= arg[1];
	word *argv	= (word *) arg[2];
	word ret;

	K_CIF_BCALLN (func, argc, argv, ret);

	arg[0] = ret;
}
/*}}}*/

