/*
 *	cspdrvc.c -- C world portion of occam interface to cspdriver
 *	Copyright (C) 2001 Fred Barnes <frmb2@ukc.ac.uk>
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
#include <sys/ioctl.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

#include "udc.h"
#include "cspdrvc.h"
#include "cspioctl.h"
#include "dmem_if.h"

static int csp_fd = -1;


/*
 *	int cspdrv_chan_read (ext_chan_t *chan, char *ptr, int len)
 *	channel read
 */
static int cspdrv_chan_read (ext_chan_t *chan, char *ptr, int len)
{
	ucsp_datainfo dinf = {(int)chan->userptr, ptr, len};

#if 0
fprintf (stderr, "[%d] IOCTL_READ_CHANNEL (%d, %p, %d)\n",
getpid(), dinf.chanid, dinf.data, dinf.length);
#endif
	if (ioctl (csp_fd, IOCTL_READ_CHANNEL, (char *)&dinf)) {
		fprintf (stderr, "cspdrv: unable to read channel: %s\n", strerror (errno));
	}
	return 0;
}


/*
 *	int cspdrv_chan_write (ext_chan_t *chan, char *ptr, int len)
 *	channel write
 */
static int cspdrv_chan_write (ext_chan_t *chan, char *ptr, int len)
{
	ucsp_datainfo dinf = {(int)chan->userptr, ptr, len};

#if 0
fprintf (stderr, "[%d] IOCTL_WRITE_CHANNEL (%d, %p, %d)\n",
getpid(), dinf.chanid, dinf.data, dinf.length);
#endif
	if (ioctl (csp_fd, IOCTL_WRITE_CHANNEL, (char *)&dinf)) {
		fprintf (stderr, "cspdrv: unable to write channel: %s\n", strerror (errno));
	}
	return 0;
}


/*
 *	int cspdrv_chan_min (ext_chan_t *chan, void **pptr)
 *	MOBILE input
 */
static int cspdrv_chan_min (ext_chan_t *chan, void **pptr)
{
	int *wsptr = (int *)pptr;
	char *addr = (char *)(wsptr[0]);
	int mobile_size = wsptr[1];
	ucsp_datainfo dinf = {(int)chan->userptr, addr, mobile_size};

#if 1
fprintf (stderr, "[%d] IOCTL_READ_CHANNEL (%d, %p, %d) for MIN\n",
getpid(), dinf.chanid, dinf.data, dinf.length);
#endif
	if (ioctl (csp_fd, IOCTL_READ_CHANNEL, (char *)&dinf)) {
		fprintf (stderr, "cspdrv: unable to read channel: %s\n", strerror (errno));
	}
	return 0;
}


/*
 *	int cspdrv_chan_mout (ext_chan_t *chan, void **pptr)
 *	MOBILE output
 */
static int cspdrv_chan_mout (ext_chan_t *chan, void **pptr)
{
	int *wsptr = (int *)pptr;
	char *addr = (char *)(wsptr[0]);
	int mobile_size = wsptr[1];
	ucsp_datainfo dinf = {(int)chan->userptr, addr, mobile_size};

#if 1
fprintf (stderr, "[%d] IOCTL_WRITE_CHANNEL (%d, %p, %d) for MOUT\n",
getpid(), dinf.chanid, dinf.data, dinf.length);
#endif
	if (ioctl (csp_fd, IOCTL_WRITE_CHANNEL, (char *)&dinf)) {
		fprintf (stderr, "cspdrv: unable to write channel: %s\n", strerror (errno));
	}
	return 0;
}


/*
 *	int cspdrv_chan_min64 (ext_chan_t *chan, unsigned long long *ptr)
 *	dynamic MOBILE input (single dimension)
 *	needs mobile-size-field support
 */
static int cspdrv_chan_min64 (ext_chan_t *chan, unsigned long long *ptr)
{
	int *wsptr = (int *)ptr;
	char **addr = (char **)wsptr;
	int *dim = (int *)(&(wsptr[1]));
	int type_size = wsptr[2];
	ucsp_datainfo dinf = {(int)chan->userptr, (char *)dim, sizeof (int)};

#if 1
fprintf (stderr, "[%d] IOCTL_READ_CHANNEL (%d, %p, %d) for MIN64 dimension\n",
getpid(), dinf.chanid, dinf.data, dinf.length);
#endif
	if (ioctl (csp_fd, IOCTL_READ_CHANNEL, (char *)&dinf)) {
		fprintf (stderr, "cspdrv: unable to read channel: %s\n", strerror (errno));
	}
#if 1
fprintf (stderr, "[%d] IOCTL_READ_CHANNEL: got dimension: %d\n", getpid(), *dim);
#endif
	*addr = (char *)dmem_alloc ((*dim) * type_size);

	dinf.data = *addr;
	dinf.length = (*dim) * type_size;
#if 1
fprintf (stderr, "[%d] IOCTL_READ_CHANNEL (%d, %p, %d) for MIN64\n",
getpid(), dinf.chanid, dinf.data, dinf.length);
#endif
	if (ioctl (csp_fd, IOCTL_READ_CHANNEL, (char *)&dinf)) {
		fprintf (stderr, "cspdrv: unable to read channel: %s\n", strerror (errno));
	}
	return 0;
}


/*
 *	int cspdrv_chan_mout64 (ext_chan_t *chan, unsigned long long *ptr)
 *	dynamic MOBILE output (single dimension)
 *	needs mobile-size-field support
 */
static int cspdrv_chan_mout64 (ext_chan_t *chan, unsigned long long *ptr)
{
	int *wsptr = (int *)ptr;
	char *addr = (char *)(wsptr[0]);
	int *dim = (int *)(&(wsptr[1]));
	int type_size = wsptr[2];
	int mobile_size = (*dim) * type_size;
	ucsp_datainfo dinf = {(int)chan->userptr, (char *)dim, sizeof (int)};

#if 1
fprintf (stderr, "[%d] IOCTL_WRITE_CHANNEL (%d, %p, %d) for MOUT64 dimension (%d)\n",
getpid(), dinf.chanid, dinf.data, dinf.length, *dim);
#endif
	if (ioctl (csp_fd, IOCTL_WRITE_CHANNEL, (char *)&dinf)) {
		fprintf (stderr, "cspdrv: unable to write channel: %s\n", strerror (errno));
	}

	dinf.data = addr;
	dinf.length = mobile_size;
#if 1
fprintf (stderr, "[%d] IOCTL_WRITE_CHANNEL (%d, %p, %d) for MOUT64\n",
getpid(), dinf.chanid, dinf.data, dinf.length);
#endif
	if (ioctl (csp_fd, IOCTL_WRITE_CHANNEL, (char *)&dinf)) {
		fprintf (stderr, "cspdrv: unable to write channel: %s\n", strerror (errno));
	}
	dmem_release ((void *)addr);
	return 0;
}


/*
 *	int cspdrv_chan_minn (ext_chan_t *chan, unsigned int *ptr, int dimcount)
 *	dynamic MOBILE input (multi-dimensional)
 *	needs mobile-size-field support
 */
static int cspdrv_chan_minn (ext_chan_t *chan, unsigned int *ptr, int dimcount)
{
	int *wsptr = (int *)ptr;
	char **addr = (char **)wsptr;
	int *dim = (int *)(&(wsptr[1]));
	int type_size = wsptr[dimcount + 1];
	int mobile_size, d;
	ucsp_datainfo dinf = {(int)chan->userptr, (char *)dim, sizeof (int) * dimcount};

#if 1
fprintf (stderr, "[%d] IOCTL_READ_CHANNEL (%d, %p, %d) for MINN dimensions\n",
getpid(), dinf.chanid, dinf.data, dinf.length);
#endif
	if (ioctl (csp_fd, IOCTL_READ_CHANNEL, (char *)&dinf)) {
		fprintf (stderr, "cspdrv: unable to read channel: %s\n", strerror (errno));
	}

	for (mobile_size = type_size, d = 0; d < dimcount; mobile_size *= dim[d], d++);
	*addr = (char *)dmem_alloc (mobile_size);

	dinf.data = *addr;
	dinf.length = mobile_size;
#if 1
fprintf (stderr, "[%d] IOCTL_READ_CHANNEL (%d, %p, %d) for MINN\n",
getpid(), dinf.chanid, dinf.data, dinf.length);
#endif
	if (ioctl (csp_fd, IOCTL_READ_CHANNEL, (char *)&dinf)) {
		fprintf (stderr, "cspdrv: unable to read channel: %s\n", strerror (errno));
	}
	return 0;
}


/*
 *	int cspdrv_chan_moutn (ext_chan_t *chan, unsigned int *ptr, int dimcount)
 *	dynamic MOBILE output (multi-dimensional)
 *	needs mobile-size-field support
 */
static int cspdrv_chan_moutn (ext_chan_t *chan, unsigned int *ptr, int dimcount)
{
	int *wsptr = (int *)ptr;
	char *addr = (char *)(wsptr[0]);
	int *dim = (int *)(&(wsptr[1]));
	int type_size = wsptr[dimcount + 1];
	int mobile_size, d;
	ucsp_datainfo dinf = {(int)chan->userptr, (char *)dim, sizeof (int) * dimcount};

#if 1
fprintf (stderr, "[%d] IOCTL_WRITE_CHANNEL (%d, %p, %d) for MOUTN dimensions\n",
getpid(), dinf.chanid, dinf.data, dinf.length);
#endif
	if (ioctl (csp_fd, IOCTL_WRITE_CHANNEL, (char *)&dinf)) {
		fprintf (stderr, "cspdrv: unable to write channel: %s\n", strerror (errno));
	}

	for (mobile_size = type_size, d = 0; d < dimcount; mobile_size *= dim[d], d++);
	dinf.data = addr;
	dinf.length = mobile_size;
#if 1
fprintf (stderr, "[%d] IOCTL_WRITE_CHANNEL (%d, %p, %d) for MOUTN\n",
getpid(), dinf.chanid, dinf.data, dinf.length);
#endif
	if (ioctl (csp_fd, IOCTL_WRITE_CHANNEL, (char *)&dinf)) {
		fprintf (stderr, "cspdrv: unable to write channel: %s\n", strerror (errno));
	}
	return 0;
}


/*
 *	int cspdrv_chan_verify (ext_chan_t *chan, unsigned int hashcode)
 *	channel verify
 */
static int cspdrv_chan_verify (ext_chan_t *chan, unsigned int hashcode)
{
	ucsp_chaninfo cinf;

	cinf.chanid = (int)chan->userptr;
	cinf.dir = (int)hashcode;
	return ioctl (csp_fd, IOCTL_TYPE_CHANNEL, (char *)&cinf);
}


/*
 *	void cspdrv_chan_alter (void *arg, int *wakeflag)
 *	called as the blocking system-call thread
 */
static void cspdrv_chan_alter (void *arg, int *wakeflag)
{
	ext_chan_t *chan = (ext_chan_t *)arg;
	ucsp_altinfo ainf = {(int)chan->userptr, 0};

	if (ioctl (csp_fd, IOCTL_ALTWT, (char *)&ainf)) {
		fprintf (stderr, "cspdrv: unable to ALTWT on channel (aborting): %s\n", strerror (errno));
		_exit (0);
	}
	*wakeflag = 1;
	return;
}


/*
 *	int cspdrv_chan_enable (ext_chan_t *chan)
 *	channel ALT enable
 */
static int cspdrv_chan_enable (ext_chan_t *chan)
{
	ucsp_altinfo ainf = {(int)chan->userptr, 0};

	if (ioctl (csp_fd, IOCTL_ENABLE_CHANNEL, (char *)&ainf)) {
		fprintf (stderr, "cspdrv: unable to enable channel: %s\n", strerror (errno));
		return 0;
	}
	if (ainf.ready) {
		return 1;
	}
	/* dispatch blocking ALT */
	ccsp_udc_start_alter (chan, cspdrv_chan_alter, (int *)chan);
	return 0;
}


/*
 *	called to disable a channel
 *	channel ALT disable
 */
static int cspdrv_chan_disable (ext_chan_t *chan)
{
	ucsp_altinfo ainf = {(int)chan->userptr, 0};

	if (ioctl (csp_fd, IOCTL_DISABLE_CHANNEL, (char *)&ainf)) {
		fprintf (stderr, "cspdrv: unable to disable channel: %s\n", strerror (errno));
		return 0;
	}
	if (ainf.ready) {
		/* thingie has terminated, or doing so */
		return 1;
	}
	if (ccsp_udc_kill_alter (chan) < 0) {
		return 1;
	}
	return 0;
}


/*
 *	int cspdrv_open (void)
 *	opens the csplib device
 */
static int cspdrv_open (void)
{
	if (csp_fd > -1) {
		return 0;
	}
	csp_fd = open ("/dev/cspdrv", O_RDWR);
	if (csp_fd < 0) {
		csp_fd = -1;
		return 1;
	}
	return 0;
}


/*
 *	void cspdrv_init (char *name, int namelen, int dir, int *addr)
 *	grabs a channel
 */
static __inline__ void cspdrv_init (char *name, int namelen, int dir, int *addr)
{
	ucsp_chaninfo cinf;
	ext_chan_t *chan = ccsp_udc_alloc_extchan (0);

	if (cspdrv_open ()) {
		ccsp_udc_free_extchan (chan);
		*addr = 0;
		return;
	}
	if (namelen >= (sizeof (cinf.name) - 1)) {
		namelen = (sizeof (cinf.name) - 1);
	}
	memcpy (cinf.name, name, namelen);
	cinf.name[namelen] = '\0';
	cinf.dir = (dir == CSP_CHAN_READ) ? CSP_CHANNEL_READER : CSP_CHANNEL_WRITER;
	if (ioctl (csp_fd, IOCTL_GET_CHANNEL, (char *)&cinf)) {
		*addr = 0;
		return;
	}

	chan->userptr = (void *)cinf.chanid;
	chan->magic = UDC_MAGIC;
	chan->flags = UDC_BLOCKING;
	chan->chan_verify = cspdrv_chan_verify;

	if (dir == CSP_CHAN_READ) {
		chan->chan_read = cspdrv_chan_read;
		chan->chan_write = NULL;
		chan->chan_min = cspdrv_chan_min;
		chan->chan_mout = NULL;
		chan->chan_min64 = cspdrv_chan_min64;
		chan->chan_mout64 = NULL;
		chan->chan_minn = cspdrv_chan_minn;
		chan->chan_moutn = NULL;
	} else {
		chan->chan_read = NULL;
		chan->chan_write = cspdrv_chan_write;
		chan->chan_min = NULL;
		chan->chan_mout = cspdrv_chan_mout;
		chan->chan_min64 = NULL;
		chan->chan_mout64 = cspdrv_chan_mout64;
		chan->chan_minn = NULL;
		chan->chan_moutn = cspdrv_chan_moutn;
	}
	chan->chan_alt_enable = cspdrv_chan_enable;
	chan->chan_alt_disable = cspdrv_chan_disable;
	*addr = (int)chan;
	return;
}


/*
 *	void cspdrv_release (int *addr)
 *	releases a channel
 */
static __inline__ void cspdrv_release (int *addr)
{
	ext_chan_t *chan = (ext_chan_t *)(*addr);
	ucsp_chaninfo cinf;

	if (csp_fd < 0) {
		return;
	}
	cinf.chanid = (int)chan->userptr;
	ioctl (csp_fd, IOCTL_RELEASE_CHANNEL, (char *)&cinf);
	*addr = 0;
	return;
}


/* occam entry points */
void _cspdrvlib_init (int *ws)
{
	cspdrv_init ((char *)(ws[0]), (int)(ws[1]), (int)(ws[2]), (int *)(ws[3]));
}
void _cspdrvlib_release (int *ws)
{
	cspdrv_release ((int *)(ws[0]));
}


