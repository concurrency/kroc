/*
 *	udc_sockchan.c -- socket disguised as a user-defined channel
 *	semantically broken, since reads/writes aren't synchronised
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
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <unistd.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <string.h>
#include <errno.h>

#include "udc.h"
#include "spunixhdr.h"		/* for call_occam_exit */

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 128
#endif

/* private data */
typedef struct {
	int fd;
	char host[MAXHOSTNAMELEN];
	struct sockaddr_in sin;
	int altflag;
} udc_sockchan_t;


/*
 *	int udc_sockchan_verify (ext_chan_t *chan, unsigned int hashcode)
 *	verifies a channel -- dummy here, just returns 0
 */
static int udc_sockchan_verify (ext_chan_t *chan, unsigned int hashcode)
{
	return 0;
}


/*
 *	int udc_sockchan_read (ext_chan_t *chan, char *ptr, int len)
 *	reads from the socket
 */
static int udc_sockchan_read (ext_chan_t *chan, char *ptr, int len)
{
	int got = 0;
	int r;
	udc_sockchan_t *priv = (udc_sockchan_t *)chan->userptr;

	while (got < len) {
		r = read (priv->fd, ptr + got, len - got);
		if (!r) {
			/* EOF from remote */
			fprintf (stderr, "sockchan: unexpected EOF in udc_sockchan_read.\n");
			/* this exits the bsyscall, leaving the occam process trapped */
			_exit (0);
		} else if (r < 0) {
			/* error */
			fprintf (stderr, "sockchan: read error from socket: %s\n", strerror (errno));
			_exit (0);
		}
		got += r;
	}
	return 0;
}


/*
 *	int udc_sockchan_write (ext_chan_t *chan, char *ptr, int len)
 *	writes to the socket
 */
static int udc_sockchan_write (ext_chan_t *chan, char *ptr, int len)
{
	int gone = 0;
	int r;
	udc_sockchan_t *priv = (udc_sockchan_t *)chan->userptr;

	while (gone < len) {
		r = write (priv->fd, ptr + gone, len - gone);
		if (r < 0) {
			/* write error */
			fprintf (stderr, "sockchan: write error from socket: %s\n", strerror (errno));
			_exit (0);
		} else if (!r) {
			/* shouldn't happen! */
			fprintf (stderr, "sockchan: write returned 0!\n");
			_exit (0);
		}
		gone += r;
	}
	return 0;
}


/*
 *	void udc_bsyscall_alter (void *arg, int *wakeflag)
 *	called as the blocking system-call thread
 *	The *arg is actually a workspace pointer containing "arg" passed (!)
 */
static void udc_bsyscall_alter (void *arg, int *wakeflag)
{
	ext_chan_t *chan = (ext_chan_t *)arg;
	udc_sockchan_t *priv = (udc_sockchan_t *)chan->userptr;
	fd_set readset;

#if 0
fprintf (stderr, "udc_bsyscall_alter: about to select() on fd %d\n", priv->fd);
#endif
	FD_ZERO (&readset);
	FD_SET (priv->fd, &readset);
	if (select (priv->fd + 1, &readset, NULL, NULL, NULL) != 1) {
		/* shouldn't ever happen, but possible. */
		fprintf (stderr, "sockchan: select returned <> 1 in udc_bsyscall_alter()\n");
		_exit (0);
	}
	/* socket has data ready */
	priv->altflag = 1;
#if 0
fprintf (stderr, "udc_bsyscall_alter: looks like it got ready, returning.\n");
#endif
	*wakeflag = 1;
	return;
}


/*
 *	int udc_sockchan_enable (ext_chan_t *chan)
 *	called when enabling in an ALT
 */
static int udc_sockchan_enable (ext_chan_t *chan)
{
	udc_sockchan_t *priv = (udc_sockchan_t *)chan->userptr;
	struct timeval tv = {0, 0};
	fd_set readset;
	int i;

	/* first, poll the socket with select to see if it's ready */
	FD_ZERO (&readset);
	FD_SET (priv->fd, &readset);
	i = select (priv->fd + 1, &readset, NULL, NULL, &tv);
	if (i < 0) {
		/* error, maybe disconnected.. */
		fprintf (stderr, "sockchan: unable to poll socket: %s\n", strerror (errno));
		call_occam_exit ();
	}
	if (i) {
		/* channel ready on enable */
		priv->altflag = -1;
		return 1;
	}
	priv->altflag = 0;	/* waiting.. */
	/* dispatch blocking alt */
#if 0
fprintf (stderr, "sockchan: dispatching blocking ALTer (func = %p, arg = %p)\n", (void *)udc_bsyscall_alter, (void *)chan);
#endif
#ifdef BLOCKING_SYSCALLS
	ccsp_udc_start_alter (chan, udc_bsyscall_alter, (int *)chan);
#else
	fprintf (stderr, "sockchan: no bsyscalls support!\n");
	call_occam_exit ();
#endif

	return 0;
}


/*
 *	int udc_sockchan_disable (ext_chan_t *chan)
 *	called when disabled in an ALT
 */
static int udc_sockchan_disable (ext_chan_t *chan)
{
	udc_sockchan_t *priv = (udc_sockchan_t *)chan->userptr;

	switch (priv->altflag) {
	case -1:
	case 1:
		/* was ready at enable or alter finished */
		return 1;
	case 0:
		/* still waiting */
#ifdef BLOCKING_SYSCALLS
		if (ccsp_udc_kill_alter (chan) < 0) {
			/* it already finished */
			return 1;
		}
#else
		fprintf (stderr, "sockchan: no bsyscalls support!\n");
		call_occam_exit ();
#endif
		return 0;
	default:
		fprintf (stderr, "sockchan: oh dear, something has gone wrong..\n");
		return 0;
	}
}


/*
 *	void real_udc_sockchan_alloc (char *hname, int hlen, int port, int *addr)
 *	initialises the channel
 */
static __inline__ void real_udc_sockchan_alloc (char *hname, int hlen, int port, int *addr)
{
	ext_chan_t *chan = ccsp_udc_alloc_extchan (sizeof (udc_sockchan_t));
	udc_sockchan_t *priv;
	struct hostent *hp;

	if (!hlen || (port < 0)) {
		fprintf (stderr, "bad hostname or port!\n");
		ccsp_udc_free_extchan (chan);
		call_occam_exit ();
	}
	priv = (udc_sockchan_t *)chan->userptr;

	chan->magic = UDC_MAGIC;
	chan->flags = UDC_BLOCKING | UDC_BLOCKING_ALT;

	chan->chan_verify = udc_sockchan_verify;
	chan->chan_read = udc_sockchan_read;
	chan->chan_write = udc_sockchan_write;
	chan->chan_alt_enable = udc_sockchan_enable;
	chan->chan_alt_disable = udc_sockchan_disable;

	priv->altflag = 0;
	priv->fd = socket (PF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (priv->fd < 0) {
		fprintf (stderr, "sockchan: unable to create socket: %s\n", strerror (errno));
		ccsp_udc_free_extchan (chan);
		call_occam_exit ();
	}
	strncpy (priv->host, hname, (hlen < MAXHOSTNAMELEN) ? hlen : (MAXHOSTNAMELEN - 1));
	hp = gethostbyname (priv->host);
	if (!hp) {
		fprintf (stderr, "sockchan: unable to lookup address of %s\n", priv->host);
		close (priv->fd);
		ccsp_udc_free_extchan (chan);
		call_occam_exit ();
	}
	priv->sin.sin_family = AF_INET;
	priv->sin.sin_port = htons (port);
	memcpy (&(priv->sin.sin_addr), hp->h_addr, sizeof (priv->sin.sin_addr));
	/* try and connect */
	if (connect (priv->fd, (struct sockaddr *)&priv->sin, sizeof (priv->sin)) < 0) {
		fprintf (stderr, "sockchan: unable to connect to remote host: %s\n", strerror (errno));
		close (priv->fd);
		ccsp_udc_free_extchan (chan);
		call_occam_exit ();
	}
	/* cool */
	*addr = (int)chan;
	return;
}


/*
 *	void real_udc_sockchan_free (int *addr)
 *	frees the channel
 */
static __inline__ void real_udc_sockchan_free (int *addr)
{
	ext_chan_t *chan = (ext_chan_t *)(*addr);
	udc_sockchan_t *priv = (udc_sockchan_t *)chan->userptr;

	close (priv->fd);
	ccsp_udc_free_extchan (chan);
	return;
}


/* occam call points */
void _udc_sockchan_alloc (int *ws) { real_udc_sockchan_alloc ((char *)(ws[0]), (int)(ws[1]),
	(int)(ws[2]), (int *)(ws[3])); }
void _udc_sockchan_free (int *ws) { real_udc_sockchan_free ((int *)(ws[0])); }


