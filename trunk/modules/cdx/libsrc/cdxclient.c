/*
 *	cdxclient.c -- cluster DXRaster client library
 *	Copyright (C) 2005-2006 Fred Barnes <frmb@kent.ac.uk>
 *	                        Adam Sampson <ats1@kent.ac.uk>
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
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include "cdxclient.h"


struct cdxclient {
	int fd;
	int use_tcp;
	struct sockaddr_in addr;
	unsigned char *buf;
	int buf_size;
};

static int full_write (int fd, const char *buf, int len) {
	while (len > 0) {
		int rc = write (fd, buf, len);
		if (rc < 0)
			return -1;
		buf += rc;
		len -= rc;
	}
	return 0;
}

struct cdxclient *cdxclient_new (const char *host, int port,
                                 int width, int height, int depth) {
	struct hostent *he;
	struct cdxclient *c;

	c = malloc (sizeof *c);
	if (c == NULL)
		goto failed_alloc;

	c->use_tcp = 1;
	c->fd = socket (PF_INET, c->use_tcp ? SOCK_STREAM : SOCK_DGRAM, 0);
	if (c->fd < 0)
		goto failed_socket;

	he = gethostbyname (host);
	if (he == NULL)
		goto failed_lookup;
	memcpy (&c->addr.sin_addr, he->h_addr_list[0], sizeof c->addr);
	c->addr.sin_port = htons (port);
	c->addr.sin_family = AF_INET;

	if (c->use_tcp
	    && connect(c->fd, (struct sockaddr *) &c->addr, sizeof c->addr) < 0)
		goto failed_lookup;

	c->buf = NULL;
	c->buf_size = 0;

	return c;

failed_lookup:
	close (c->fd);
failed_socket:
	free (c);
failed_alloc:
	return NULL;
}

int cdxclient_send (struct cdxclient *c, int frame, int offset,
                    const unsigned char *data, int data_size) {
#define buf c->buf
#define buf_size c->buf_size
	/* FIXME Could use sendmsg to avoid copying */
	int message_size = data_size + 8;
	/* Transfer mode: 0 = uncompressed; 1 = run-length compressed. */
	int mode = 0;

	if (buf == NULL || buf_size < message_size) {
		unsigned char *new_buf = realloc (buf, message_size + sizeof(int));
		if (new_buf == NULL)
			return -1;
		buf = new_buf;
		buf_size = message_size;
	}

	/* Try to compress the data, provided it's a set of words. */
	if (data_size % 4 == 0) {
		const int *idata = (const int *) data, *pi = idata, *pd;
		const int isize = data_size / 4;
		int *odata = (int *) (buf + 8), *po = odata;
		int runlength;

		mode = 1;
		while (1) {
			if (po == odata + isize) {
				/* Not worth compressing. */
				mode = 0;
				break;
			}
			if (pi == idata + isize) {
				/* Reached the end of the input. */
				break;
			}
			/* Find the next entry that's different. */
			for (pd = pi; *pd == *pi; ) {
				/* (... or off the end of the input.) */
				if (++pd == idata + isize)
					break;
			}
			runlength = pd - pi;
			if (runlength > 1 || (*pi & 0x42) == 0x42) {
				/* Found a run (or some real data that would
				 * look like a run if we didn't encode it).
				 * Encode it. */
				*po++ = (runlength << 8) | 0x42;
				if (po == odata + isize) {
					/* Not worth compressing. */
					mode = 0;
					break;
				}
				*po++ = *pi;
				pi += runlength;
			} else {
				*po++ = *pi++;
			}
		}

		if (mode != 0)
			message_size = (po - odata) * 4 + 8;
	}

	((int *) buf)[0] = frame;
	((int *) buf)[1] = offset | (mode << 30);
	if (mode == 0)
		memcpy (buf + 8, data, data_size);

	if (c->use_tcp) {
		memmove (buf + sizeof(int), buf, message_size);
		*(int *) buf = message_size;
		if (full_write (c->fd, buf, message_size + sizeof(int)) < 0)
			return -1;
	} else {
		if (sendto (c->fd, buf, message_size, 0,
			    (struct sockaddr *) &c->addr, sizeof c->addr) < 0)
			return -1;
	}

	return 0;
#undef buf
#undef buf_size
}

int cdxclient_close (struct cdxclient *c) {
	if (close (c->fd) < 0)
		return -1;
	free (c->buf);
	free (c);
	return 0;
}


