/*
 *	gen13.c -- workspace size/offset routines
 *	Copyright (C) 2003 Fred Barnes <frmb2@ukc.ac.uk>
 *	With bits from genhdr.h (C) Inmos
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

/*{{{  includes*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include "includes.h"
#include "generror.h"
#include "instruct.h"
#include "genhdr.h"
/*}}}*/

/*{{{  int genhdr_ds_min (void)*/
/*
 *	all processes require at least this many slots
 */
int genhdr_ds_min (void)
{
	int slots;

#ifdef PROCESS_PRIORITY
	slots = 3;
#else
	slots = 2;
#endif
	slots += L_process;
	return slots;
}
/*}}}*/
/*{{{  int genhdr_ds_io (void)*/
/*
 *	for proceses performing IO
 */
int genhdr_ds_io (void)
{
	int slots;

	slots = genhdr_ds_min () + 1;
	return slots;
}
/*}}}*/
/*{{{  int genhdr_ds_wait (void)*/
/*
 *	for processes that wait on the timer queue
 */
int genhdr_ds_wait (void)
{
	int slots;

	slots = genhdr_ds_min () + 3;
	return slots;
}
/*}}}*/
/*{{{  int genhdr_w_time_slot (void)*/
/*
 *	workspace slot used by ALT pre-enabling for timers
 */
int genhdr_w_time_slot (void)
{
	int slot;

#ifdef PROCESS_PRIORITY
	slot = -6;
#else
	slot = -5;
#endif

	return slot;
}
/*}}}*/

