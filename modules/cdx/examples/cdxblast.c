/*
 *	cdxblast.c -- cluster DXRaster "data blaster"
 *	Copyright (C) 2005 Fred Barnes <frmb@kent.ac.uk>
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


/*{{{  includes, etc.*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdarg.h>
#include <errno.h>
#include "cdxclient.h"


/*}}}*/

#define string_dup(x) strdup(x)
#define smalloc(x) malloc(x)

/*{{{  int main (int argc, char **argv)*/
/*
 *	start here
 */
int main (int argc, char **argv)
{
	int width, height, bpp;
	char *host;
	int port;

	struct cdxclient *cdx;

	unsigned char *buf;
	int buflen;


	if (argc != 6) {
		fprintf (stderr, "usage: %s <host> <port> <width> <height> <bpp>\n", *argv);
		exit (EXIT_FAILURE);
	}
	host = string_dup (argv[1]);
	if (sscanf (argv[2], "%d", &port) != 1) {
		fprintf (stderr, "%s: bad port [%s]\n", *argv, argv[2]);
		exit (EXIT_FAILURE);
	}
	if (sscanf (argv[3], "%d", &width) != 1) {
		fprintf (stderr, "%s: bad width [%s]\n", *argv, argv[3]);
		exit (EXIT_FAILURE);
	}
	if (sscanf (argv[4], "%d", &height) != 1) {
		fprintf (stderr, "%s: bad height [%s]\n", *argv, argv[4]);
		exit (EXIT_FAILURE);
	}
	if (sscanf (argv[5], "%d", &bpp) != 1) {
		fprintf (stderr, "%s: bad bpp [%s]\n", *argv, argv[5]);
		exit (EXIT_FAILURE);
	}

	cdx = cdxclient_new (host, port, width, height, bpp);

	/* create buffer */
	buflen = (width * height * (bpp >> 3));
	printf ("%s: using %d byte buffer.\n", *argv, buflen);
	buf = (unsigned char *)smalloc (buflen + 5);

	/* do timed sends */
	{
		struct timeval tv;
		unsigned char cval = 0x00;
		int i;
		int frame = 0;
		int dpsize = width * (bpp >> 3) / 2;
		printf("dpsize = %d\n", dpsize);

		for (;;) {
			int count = 0;
#if 0
			for (i=0; i<buflen;) {
				buf[i++] = cval++ >> 4;
				buf[i++] = cval++ << 4;
				buf[i++] = cval++ ^ 0xf0;
				buf[i++] = cval++ ^ 0x0f;
			}
			cval++;
#else
			for (i=0; i<buflen;) {
				const unsigned char v = (frame + ((i / 4) / width)) % 128;
				buf[i++] = v + 128 * (cval == 0);
				buf[i++] = v + 128 * (cval == 1);
				buf[i++] = v + 128 * (cval == 2);
				buf[i++] = 0;
			}
			cval = (cval + 1) % 3;
#endif
			frame++;

			for (i=0; i<buflen; i+=dpsize) {
				int plen;

				if ((buflen - i) < dpsize) {
					plen = (buflen - i);
				} else {
					plen = dpsize;
				}

				if (cdxclient_send (cdx, frame, i,
				                    buf + i, plen) < 0) {
					fprintf (stderr, "%s: send failed: %s\n", *argv, strerror (errno));
					exit (EXIT_FAILURE);
				}
				++count;
			}
			printf("Sent %d packets in frame %d\n", count, frame);

			/* Excessively weird behaviour: put this select in, and
			 * you get packets being dropped, even if the data rate
			 * is fairly low... */
			tv.tv_sec = 0;
			tv.tv_usec = 100000;		/* 10 frames/sec */
			//select (0, NULL, NULL, NULL, &tv);
		}
	}
	cdxclient_close (cdx);

	return 0;
}
/*}}}*/

