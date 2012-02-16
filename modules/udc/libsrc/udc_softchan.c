/*
 *	udc_softchan.c -- C stuff for emulating a soft-channel (example really..)
 *	Copyright (C) 2002 Fred Barnes <frmb2@ukc.ac.uk>
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>

#include "udc.h"
#include "spunixhdr.h"		/* for call_occam_exit() */


/*
 *	int udc_softchan_verify (ext_chan_t *chan, unsigned int hashcode)
 *	dummy, but required
 */
static int udc_softchan_verify (ext_chan_t *chan, unsigned int hashcode)
{
	return 0;
}


/*
 *	int udc_softchan_write (ext_chan_t *chan, char *ptr, int bytes)
 *	called when channel output happens
 */
static int udc_softchan_write (ext_chan_t *chan, char *ptr, int bytes)
{
	if (!chan->userptr) {
		/* nothing here yet.. */
		chan->userptr = ptr;
		return 1;	/* block */
	} else if (chan->userptr == chan) {
		/* ALT waiting, wake it up */
		ccsp_udc_resume_process (chan);
		chan->userptr = ptr;
		return 1;	/* block */
	} else {
		/* reader waiting */
		memcpy (chan->userptr, ptr, bytes);
		chan->userptr = NULL;
		ccsp_udc_resume_process (chan);
		return 0;
	}
}


/*
 *	int udc_softchan_read (ext_chan_t *chan, char *ptr, int bytes)
 *	called when channel input happens
 */
static int udc_softchan_read (ext_chan_t *chan, char *ptr, int bytes)
{
	if (!chan->userptr) {
		/* nothing here yet.. */
		chan->userptr = ptr;
		return 1;	/* block */
	} else {
		/* writer waiting */
		memcpy (ptr, chan->userptr, bytes);
		chan->userptr = NULL;
		ccsp_udc_resume_process (chan);
		return 0;
	}
}


/*
 *	int udc_softchan_enable (ext_chan_t *chan)
 *	called when enabled in an ALT
 */
static int udc_softchan_enable (ext_chan_t *chan)
{
	if (chan->userptr) {
		/* channel ready */
		return 1;
	}
	/* we're ALTing.. */
	chan->userptr = (void *)chan;
	return 0;
}


/*
 *	int udc_softchan_disable (ext_chan_t *chan)
 *	called when disabled in an ALT
 */
static int udc_softchan_disable (ext_chan_t *chan)
{
	if (chan->userptr == (void *)chan) {
		/* still us.. */
		chan->userptr = NULL;
		return 0;
	}
	/* writer arrived */
	return 1;
}


/*
 *	void real_udc_softchan_alloc (int *addr)
 *	allocates and initialises the channel
 */
static __inline__ void real_udc_softchan_alloc (int *addr)
{
	ext_chan_t *chan = ccsp_udc_alloc_extchan (0);

	chan->magic = UDC_MAGIC;
	chan->flags = UDC_NONE;

	chan->chan_verify = udc_softchan_verify;
	chan->chan_read = udc_softchan_read;
	chan->chan_write = udc_softchan_write;
	chan->chan_alt_enable = udc_softchan_enable;
	chan->chan_alt_disable = udc_softchan_disable;

	*addr = (int)chan;
	return;
}


/*
 *	void real_udc_softchan_free (int *addr)
 *	frees the channel
 */
static __inline__ void real_udc_softchan_free (int *addr)
{
	ext_chan_t *chan = (ext_chan_t *)(*addr);

	ccsp_udc_free_extchan (chan);
	*addr = 0;
}


/* occam call points */
void _udc_softchan_alloc (int *ws) { real_udc_softchan_alloc ((int *)(ws[0])); }
void _udc_softchan_free (int *ws) { real_udc_softchan_free ((int *)(ws[0])); }



