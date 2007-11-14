/*
 *	udc_bufchan.c -- C stuff for a buffered channel
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
#include <unistd.h>
#include <sys/types.h>

#include "udc.h"
#include "spunixhdr.h"		/* for call_occam_exit */

/* buffer state */
typedef struct {
	int size, head, tail, count;
	int *databuf;
	char *waiting;
} udc_bufchan_t;


/*
 *	int udc_bufchan_verify (ext_chan_t *chan, unsigned int hashcode)
 *	checks for a good channel type
 */
static int udc_bufchan_verify (ext_chan_t *chan, unsigned int hashcode)
{
	return (hashcode == UDC_CHANTYPE_INT) ? 0 : 1;
}


/*
 *	int udc_bufchan_write (ext_chan_t *chan, char *ptr, int bytes)
 *	called when writing to the buffer
 */
static int udc_bufchan_write (ext_chan_t *chan, char *ptr, int bytes)
{
	udc_bufchan_t *priv = (udc_bufchan_t *)chan->userptr;

	if (priv->count == priv->size) {
		/* buffer full, block */
		priv->waiting = ptr;
		return 1;
	}
	if (priv->waiting == (char *)priv) {
		/* ALT waiting, wake it up */
		priv->waiting = NULL;
		ccsp_udc_resume_process (chan);
	} else if (priv->waiting) {
		/* reader waiting -- assert buffer empty */
		*(int *)priv->waiting = *(int *)ptr;
		priv->waiting = NULL;
		ccsp_udc_resume_process (chan);
		return 0;
	}
	priv->databuf[priv->head] = *(int *)ptr;
	priv->head++;
	priv->head %= priv->size;
	priv->count++;
	return 0;
}


/*
 *	int udc_bufchan_read (ext_chan_t *chan, char *ptr, int bytes)
 *	called when reading from the buffer
 */
static int udc_bufchan_read (ext_chan_t *chan, char *ptr, int bytes)
{
	udc_bufchan_t *priv = (udc_bufchan_t *)chan->userptr;

	if (!priv->count) {
		/* buffer empty, block */
		priv->waiting = ptr;
		return 1;
	}
	if (priv->waiting) {
		/* writer waiting -- assert buffer full */
		*(int *)ptr = priv->databuf[priv->tail];
		priv->tail++;
		priv->tail %= priv->size;
		priv->databuf[priv->head] = *(int *)priv->waiting;
		priv->head++;
		priv->head %= priv->size;
		priv->waiting = NULL;
		ccsp_udc_resume_process (chan);
		return 0;
	}
	*(int *)ptr = priv->databuf[priv->tail];
	priv->tail++;
	priv->tail %= priv->size;
	priv->count--;
	return 0;
}


/*
 *	int udc_bufchan_enable (ext_chan_t *chan)
 *	called when enabled in an ALT
 */
static int udc_bufchan_enable (ext_chan_t *chan)
{
	udc_bufchan_t *priv = (udc_bufchan_t *)chan->userptr;

	if (priv->count || priv->waiting) {
		/* channel ready */
		return 1;
	}
	/* go for ALT! */
	priv->waiting = (char *)priv;
	return 0;
}


/*
 *	int udc_bufchan_disable (ext_chan_t *chan)
 *	called when disabled in an ALT
 */
static int udc_bufchan_disable (ext_chan_t *chan)
{
	udc_bufchan_t *priv = (udc_bufchan_t *)chan->userptr;

	if (priv->waiting == (char *)priv) {
		/* still us.. */
		priv->waiting = NULL;
		return 0;
	}
	/* writer arrived */
	return 1;
}


/*
 *	void real_udc_bufchan_alloc (int count, int *addr)
 *	initialises the channel
 */
static __inline__ void real_udc_bufchan_alloc (int count, int *addr)
{
	ext_chan_t *chan = ccsp_udc_alloc_extchan (sizeof (udc_bufchan_t));
	udc_bufchan_t *priv = (udc_bufchan_t *)chan->userptr;

	if (count <= 0) {
		fprintf (stderr, "be serious about buffer size, please! (was %d)\n", count);
		ccsp_udc_free_extchan (chan);
		call_occam_exit ();
	}

	chan->magic = UDC_MAGIC;
	chan->flags = UDC_NONE;

	chan->chan_verify = udc_bufchan_verify;
	chan->chan_read = udc_bufchan_read;
	chan->chan_write = udc_bufchan_write;
	chan->chan_alt_enable = udc_bufchan_enable;
	chan->chan_alt_disable = udc_bufchan_disable;

	priv->size = count;
	priv->head = 0;
	priv->tail = 0;
	priv->count = 0;
	/* udc_bufchan_verify will make sure we're an INT channel */
	priv->databuf = (int *)malloc (priv->size * sizeof (int));
	if (!priv->databuf) {
		fprintf (stderr, "out of memory!\n");
		ccsp_udc_free_extchan (chan);
		call_occam_exit ();
	}
	priv->waiting = NULL;

	*addr = (int)chan;
	return;
}


/*
 *	void real_udc_bufchan_free (int *addr)
 *	frees the channel
 */
static __inline__ void real_udc_bufchan_free (int *addr)
{
	ext_chan_t *chan = (ext_chan_t *)(*addr);
	udc_bufchan_t *priv = (udc_bufchan_t *)chan->userptr;

	free (priv->databuf);
	priv->databuf = NULL;
	ccsp_udc_free_extchan (chan);
	*addr = 0;
	return;
}


/* occam call point */
void _udc_bufchan_alloc (int *ws) { real_udc_bufchan_alloc ((int)(ws[0]), (int *)(ws[1])); }
void _udc_bufchan_free (int *ws) { real_udc_bufchan_free ((int *)(ws[0])); }



