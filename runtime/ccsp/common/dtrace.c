/*
 *	dtrace.c -- debugging traces (for use with debugging tools)
 *	Copyright (C) 2006-2008 Fred Barnes <frmb@kent.ac.uk>
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

#if !defined(RMOX_BUILD) && defined(ENABLE_DTRACES)

/*{{{  general includes*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/fcntl.h>
#include <unistd.h>
#include <stdarg.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>

#include <kernel.h>
#include <rts.h>
#include <dmem_if.h>
#include <dtrace.h>

#ifndef UNIX_PATH_MAX
#define UNIX_PATH_MAX 108
#endif
/*}}}*/

/*{{{  private data*/
static int dtraces_enabled = 0;
static int dtrace_fd = -1;
static char dtrace_buf[1024];
static int dtrace_write (const char *buf, int len);
/*}}}*/


/*{{{  int ccsp_init_dtraces (void)*/
/*
 *	initialises the dtraces feature;  settings are taken from environment variables
 *	returns 0 on success, non-zero on failure (non-fatal)
 */
int ccsp_init_dtraces (void)
{
	char *envptr, *ch;
	int i, x;

	envptr = getenv ("DTRACED");
	if (!envptr) {
		envptr = getenv ("dtraced");
	}
	if (!envptr) {
		BMESSAGE ("no dtraced environment given, not using dtraces\n");
		dtraces_enabled = 0;
		return 0;
	}
#if 0
MESSAGE ("got dtraced environment [%s]\n", envptr);
#endif

	/* see if there's a ':' in it (TCP) */
	for (ch=envptr; (*ch != ':') && (*ch != '\0'); ch++);
	if (*ch == ':') {
		/*{{{  using TCP socket*/
		struct sockaddr_in sin;
		struct hostent *hp;
		char hnbuf[64];
		int port;
		socklen_t slen = sizeof (struct sockaddr_in);
		int nlen;

		nlen = ((int)(ch - envptr) < 64) ? (int)(ch - envptr) : 63;
		strncpy (hnbuf, envptr, nlen);
		hnbuf[nlen] = '\0';
		hp = gethostbyname (hnbuf);
		if (!hp) {
			BMESSAGE ("failed to resolve dtraced host [%s]: %s\n", hnbuf, hstrerror (h_errno));
			dtraces_enabled = 0;
			return 0;
		}
		if (sscanf (ch + 1, "%d", &port) != 1) {
			BMESSAGE ("bad port [%s]\n", ch + 1);
			dtraces_enabled = 0;
			return 0;
		}

		memcpy (&sin.sin_addr, hp->h_addr_list[0], hp->h_length);
		sin.sin_family = AF_INET;
		sin.sin_port = htons (port);

		dtrace_fd = socket (PF_INET, SOCK_STREAM, 0);
		if (dtrace_fd < 0) {
			BMESSAGE ("failed to create dtrace socket: %s\n", strerror (errno));
			dtraces_enabled = 0;
			return 0;
		}

		if (connect (dtrace_fd, (struct sockaddr *)&sin, slen)) {
			BMESSAGE ("failed to connect to dtraced host: %s\n", strerror (errno));
			dtraces_enabled = 0;
			close (dtrace_fd);
			dtrace_fd = -1;
			return 0;
		}
		/*}}}*/
	} else {
		/*{{{  using UNIX socket*/
		struct sockaddr_un sun;
		socklen_t slen = sizeof (struct sockaddr_un);

		sun.sun_family = AF_UNIX;
		strncpy (sun.sun_path, envptr, (strlen (envptr) < UNIX_PATH_MAX) ? strlen (envptr) : (UNIX_PATH_MAX - 1));

		dtrace_fd = socket (PF_UNIX, SOCK_STREAM, 0);
		if (dtrace_fd < 0) {
			BMESSAGE ("failed to create dtrace socket: %s\n", strerror (errno));
			dtraces_enabled = 0;
			return 0;
		}

		if (connect (dtrace_fd, (struct sockaddr *)&sun, slen)) {
			BMESSAGE ("failed to connect to dtraced host: %s\n", strerror (errno));
			dtraces_enabled = 0;
			close (dtrace_fd);
			dtrace_fd = -1;
			return 0;
		}
		/*}}}*/
	}

	/* if we get this far, connected to host :) */
	dtraces_enabled = 1;

	/* tell the dtrace daemon that we're an application */
	x = snprintf ((char *)dtrace_buf, 1024, "APP %d", getpid ());
	for (i=0; i<kroc_argc; i++) {
		x += snprintf ((char *)dtrace_buf + x, 1024 - x, " \"%s\"", kroc_argv[i]);
	}
	x += snprintf ((char *)dtrace_buf + x, 1024 - x, "\n");
	if (dtrace_write ((const char *)dtrace_buf, x) < 0) {
		BMESSAGE ("failed to establish link with dtrace host, not using dtraces\n");
		dtraces_enabled = 0;
		return 0;
	}

	return 0;
}
/*}}}*/
/*{{{  static int dtrace_write (const char *buf, int len)*/
/*
 *	writes a trace to the trace daemon, waits until its all gone or errors
 *	returns number of bytes written on success, < 0 on error
 */
static int dtrace_write (const char *buf, int len)
{
	int gone = 0;
	int left = len;

	while (left) {
		int i = write (dtrace_fd, buf + gone, left);

		if (i < 0) {
			BMESSAGE ("error writing to dtrace host: %s\n", strerror (errno));
			dtraces_enabled = 0;
			close (dtrace_fd);
			dtrace_fd = -1;
			return -1;
		} else if (!i ) {
			BMESSAGE ("empty write to dtrace host\n");
			dtraces_enabled = 0;
			close (dtrace_fd);
			dtrace_fd = -1;
			return -1;
		}
		/* else wrote something */
		left -= i;
		gone += i;
	}

	return gone;
}
/*}}}*/
/*{{{  static int dtrace_mkhex (void *addr, int alen, char *bufptr, int rev)*/
/*
 *	generates hex versions of values
 *	returns number of bytes added to the buffer
 */
static int dtrace_mkhex (void *addr, int alen, char *bufptr, int rev)
{
	static char *hexstring = "0123456789abcdef";
	int i;

	if (rev) {
		for (i=alen-1; i>=0; i--) {
			unsigned char ch = *(unsigned char *)(addr + i);

			bufptr[0] = hexstring[(ch >> 4) & 0x0f];
			bufptr[1] = hexstring[ch & 0x0f];
			bufptr += 2;
		}
	} else {
		for (i=0; i<alen; i++) {
			unsigned char ch = *(unsigned char *)(addr + i);

			bufptr[0] = hexstring[(ch >> 4) & 0x0f];
			bufptr[1] = hexstring[ch & 0x0f];
			bufptr += 2;
		}
	}
	return alen * 2;
}
/*}}}*/
/*{{{  void do_dtrace (const char *dfmt, ...)*/
/*
 *	generates a trace -- the operation of this is wholly synchronous, whatever is calling
 *	will be held up until the trace is sent.  NOTE: "dfmt" is a debugging format specification _not_ a printf one!
 */
void do_dtrace (const char *dfmt, ...)
{
	const char *p;
	char *x = dtrace_buf;
	int xlen = 0;
	va_list ap;
	int i = 0;
	unsigned int v;
	char *ch;

	if (!dtraces_enabled) {
		return;
	}

	/* put format at start of buffer */
	xlen = strlen (dfmt);
	strcpy (x, dfmt);
	x += xlen;

	/* pad to multiples of 4 spaces */
	for (; xlen & 0x03; xlen++, x++) {
		*x = ' ';
	}

	va_start (ap, dfmt);
	for (p = dfmt + 2; *p != '\0'; p++) {
		switch (*p) {
		case 'W':		/* Wptr */
		case 'A':		/* address */
		case 'L':		/* length */
			v = va_arg (ap, unsigned int);
#if defined(TARGET_BIGENDIAN)
			i = dtrace_mkhex (&v, sizeof (unsigned int), x, 0);
#else
			i = dtrace_mkhex (&v, sizeof (unsigned int), x, 1);
#endif
			x += i;
			xlen += i;
			break;
		case 'N':		/* name (null-terminated string) */
			ch = va_arg (ap, char *);
			v = strlen (ch);
#if defined(TARGET_BIGENDIAN)
			i = dtrace_mkhex (&v, sizeof (unsigned int), x, 0);	/* drop size */
#else
			i = dtrace_mkhex (&v, sizeof (unsigned int), x, 1);	/* drop size */
#endif
			x += i;
			xlen += i;
			i = dtrace_mkhex ((void *)ch, v, x, 0);			/* drop name-bytes */
			x += i;
			xlen += i;

			/* pad to multiples of 4 spaces */
			for (; xlen & 0x03; xlen++, x++) {
				*x = ' ';
			}
			break;
		default:
			BMESSAGE ("do_dtrace(): confused.. (%c)\n", *p);
			break;
		}
	}
	va_end (ap);

	*x = '\0';

	if (xlen) {
		/* write it out -- with a newline on the end */
		*x = '\n';
		x++;
		*x = '\0';
		xlen++;

		dtrace_write (dtrace_buf, xlen);
	}

	return;
}
/*}}}*/
/*{{{  void rt_dtrace (void *wptr, unsigned int dta, unsigned int dtb)*/
/*
 *	generates a run-time trace (from the compiler/translator)
 */
void rt_dtrace (void *wptr, unsigned int dta, unsigned int dtb)
{
	/* dta has some meaningful code in it */
	switch (dta) {
		/*{{{  1 -- named process entry*/
	case 1:
		do_dtrace ("PEWN", wptr, (char *)dtb);
		break;
		/*}}}*/
	}

	return;
}
/*}}}*/



#else	/* defined(RMOX_BUILD) || !defined(ENABLE_DTRACES) */

/*{{{  void dtraces_not_supported (void)*/
/*
 *	dummy function
 */
void dtraces_not_supported (void)
{
	return;
}
/*}}}*/


#endif	/* defined(RMOX_BUILD) || !defined(ENABLE_DTRACES) */

