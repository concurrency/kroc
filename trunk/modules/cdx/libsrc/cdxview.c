/*
 *	cdxview.c -- cluster DXRaster display server
 *	Copyright (C) 2005 Fred Barnes <frmb@kent.ac.uk>
 *	                   Adam Sampson <ats1@kent.ac.uk>
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
#include <unistd.h>
#include <errno.h>
#include <SDL.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>


#define MAX(a, b) ((a) > (b) ? (a) : (b))

/*}}}*/
/*{{{  display parameters*/
#define WIDTH 640
#define HEIGHT 480
#define DEPTH 32
/* The number of frames to buffer in memory -- i.e. how wide a spread of client
 * responses we're expecting (given network packet reordering). */
#define NFRAMES 5


/*}}}*/
/*{{{  local vars*/

static SDL_Surface *frames[NFRAMES];
static int packet_count[NFRAMES];
static int low_frame = 0;
static SDL_Surface *display = NULL;


/*}}}*/


/*{{{  void die (const char *msg, const char *err)*/
/*
 *	exits with an error message
 */
void die (const char *msg, const char *err)
{
	fprintf (stderr, "%s%s%s\n", msg, (err == NULL) ? "" : ": ", (err == NULL) ? "" : err);
	exit (EXIT_FAILURE);
}
/*}}}*/
/*{{{  int full_read (int fd, char *buf, int size)*/
/*
 *	does a full read from a socket
 *	returns 0 on success, non-zero on error
 */
int full_read (int fd, char *buf, int size)
{
	while (size > 0) {
		int n = read(fd, buf, size);

		if (n <= 0) {
			return -1;
		}
		buf += n;
		size -= n;
	}
	return 0;
}
/*}}}*/
/*{{{  void handle_message (const char *buf, int len, struct sockaddr_in *from_addr)*/
/*
 *	deals with an incoming message
 */
void handle_message (const char *buf, int len, struct sockaddr_in *from_addr)
{
	unsigned int frame, offset, mode, row, col;
	const Uint8 bpp = DEPTH / 8;
	SDL_Surface *dest;
	static int *odata = NULL;
	static int odata_size = 0;

	/* Pull the header off the message. */
	if (len < 8) {
		die ("Excessively short message", NULL);
	}
	frame = ((unsigned int *) buf)[0];
	offset = ((unsigned int *) buf)[1];
	mode = offset >> 30;
	offset &= 0x3FFFFFFF;
	buf += 8;
	len -= 8;

	/* Figure out where this should be displayed. */
	row = (offset / bpp) / WIDTH;
	col = (offset / bpp) % WIDTH;
	if ((row > HEIGHT) || (col > WIDTH)) {
		die ("Position is off screen", NULL);
	}

	/* If we get a Frame 1 from a client, restart from scratch. */
	if (low_frame > 1 && frame == 1) {
		int i;

		for (i = 0; i < NFRAMES; i++) {
			SDL_FillRect(frames[i], NULL, 0xFF000000);
			packet_count[i] = 0;
		}
		low_frame = 0;
	}

	/* FIXME This doesn't deal with wraparound. */
	if (frame >= (low_frame + NFRAMES)) {
		int new_low = frame + 1 - NFRAMES;
		int ncycle = new_low - low_frame;
		int i;
		SDL_Surface *tmp[NFRAMES];

		if (ncycle > NFRAMES) {
			ncycle = NFRAMES;
		}

		/* Display the newest recycled frame. */
		printf("Showing frame %d -- %d packets received\n",
				low_frame + ncycle - 1, packet_count[ncycle - 1]);
		SDL_BlitSurface (frames[ncycle - 1], NULL, display, NULL);
		SDL_Flip (display);

		/* Clear the frames that are to be reused. */
		for (i = 0; i < ncycle; i++) {
			SDL_FillRect(frames[i], NULL, 0);
			tmp[i] = frames[i];
		}

		/* Move all the other frames down. */
		for (i = ncycle; i < NFRAMES; i++) {
			frames[i - ncycle] = frames[i];
			packet_count[i - ncycle] = packet_count[i];
		}

		/* Move the reused ones back up to the top. */
		for (i = 0; i < ncycle; i++) {
			frames[i + NFRAMES - ncycle] = tmp[i];
			packet_count[i + NFRAMES - ncycle] = 0;
		}

		low_frame = new_low;
	} else if (frame < low_frame) {
		printf ("Frame %d out of buffered range -- dropping\n", frame);
		return;
	}
	dest = frames[frame - low_frame];
	++packet_count[frame - low_frame];

	switch (mode) {
	case 0:
		/* Uncompressed -- nothing to do. */
		break;
	case 1:
		{
			/* Run-length compressed. */
			const int *idata = (const int *) buf, *pi = idata;
			const int isize = len / 4;
			int *po = odata;

			if (len % 4 != 0) {
				die ("Mode 1 packet not whole number of words", NULL);
			}
			for (;;) {
				int count;

				if (pi == (idata + isize)) {
					/* End of data. */
					break;
				}
				if ((*pi & 0xFF) == 0x42) {
					/* Compressed run. */
					count = *pi >> 8;
					++pi;
				} else {
					/* Regular data. */
					count = 1;
				}
				if (pi > idata + isize) {
					die ("End of packet while uncompressing", NULL);
				}
				if ((odata == NULL) || ((po + count) >= (odata + odata_size))) {
					int pos = po - odata;

					/* Output buffer full -- extend it. */
					odata_size = (pos + count + 1) * 2;
					odata = realloc(odata, odata_size * sizeof *odata);
					if (odata == NULL) {
						die("realloc failed", strerror(errno));
					}

					po = odata + pos;
				}
				for (; count-- > 0; *po++ = *pi);
				++pi;
			}

			buf = (const char *) odata;
			len = (po - odata) * 4;
			break;
		}
	default:
		die ("Unknown compression mode in packet", NULL);
		break;
	}

	/* Copy the provided data into place, respecting the pitch of the
	 * display (i.e. skipping over the gaps that might be at the ends of
	 * lines. */
	while (len > 0) {
		int thisline = ((WIDTH - col) * bpp);

		if (len < thisline) {
			thisline = len;
		}
		memcpy (dest->pixels + (row * dest->pitch) + (bpp * col), buf, thisline);
		len -= thisline;
		buf += thisline;
		++row;
		col = 0;
	}
}
/*}}}*/

#define MAX_CLIENTS 40

/*{{{  int main (int argc, char **argv)*/
/*
 *	start here :)
 */
int main (int argc, char **argv)
{
	int sock, listen_fd, i, client_fds[MAX_CLIENTS];
	struct sockaddr_in bind_addr;

	for (i = 0; i < MAX_CLIENTS; i++) {
		client_fds[i] = -1;
	}

	/* Start SDL. */
	if (SDL_Init(SDL_INIT_VIDEO) < 0) {
		die ("Cannot initialise SDL", SDL_GetError ());
	}
	atexit(SDL_Quit);

	/* Create our output window. */
	SDL_WM_SetCaption ("cdxview", "cdxview");
	display = SDL_SetVideoMode (WIDTH, HEIGHT, DEPTH, SDL_DOUBLEBUF);
	if (display == NULL) {
		die ("Cannot create surface", SDL_GetError());
	}
	SDL_UpdateRect (display, 0, 0, 0, 0);

	/* Set up the buffered frames. */
	for (i = 0; i < NFRAMES; i++) {
		frames[i] = SDL_CreateRGBSurface (0, WIDTH, HEIGHT, DEPTH, 0, 0, 0, 0);
		if (frames[i] == NULL) {
			die ("Cannot create surface", SDL_GetError());
		}
		SDL_FillRect (frames[i], NULL, 0xFF000000);
		packet_count[i] = 0;
	}

	/* Create and bind the receiving socket. */
	sock = socket (PF_INET, SOCK_DGRAM, 0);
	if (sock < 0) {
		die ("Cannot create socket", strerror (errno));
	}

	bind_addr.sin_family = AF_INET;
	bind_addr.sin_port = htons (2345);
	bind_addr.sin_addr.s_addr = INADDR_ANY;
	if (bind(sock, (struct sockaddr *) &bind_addr, sizeof bind_addr) < 0) {
		die ("Cannot bind socket", strerror (errno));
	}

	listen_fd = socket (PF_INET, SOCK_STREAM, 0);
	if (listen_fd < 0) {
		die ("Cannot create socket", strerror (errno));
	}

	i = 1;
	if (setsockopt (listen_fd, SOL_SOCKET, SO_REUSEADDR, &i, sizeof i) < 0) {
		die ("Unable to set SO_REUSEADDR", strerror (errno));
	}

	printf ("listening on *:2345\n");
	bind_addr.sin_family = AF_INET;
	bind_addr.sin_port = htons(2345);
	bind_addr.sin_addr.s_addr = INADDR_ANY;
	if (bind (listen_fd, (struct sockaddr *) &bind_addr, sizeof bind_addr) < 0) {
		die ("Cannot bind socket", strerror (errno));
	}

	if (listen (listen_fd, 10) < 0) {
		die ("listen failed", strerror (errno));
	}

	for (;;) {
		char buf[65536];
		ssize_t len;
		struct sockaddr_in from_addr;
		socklen_t from_len = sizeof from_addr;
		SDL_Event event;
		fd_set rfds;
		struct timeval tv = {0, 100000};
		int rc, max_fd;

		/* Check for keypresses. */
		while (SDL_PollEvent (&event)) {
			switch (event.type) {
			case SDL_KEYDOWN:
				switch (event.key.keysym.sym) {
				case SDLK_f:
					SDL_WM_ToggleFullScreen(display);
					break;
				case SDLK_q:
					return 0;
				default:
					/* do nothing */;
				}
				break;
			case SDL_QUIT:
				return 0;
			}
		}

		FD_ZERO (&rfds);
		FD_SET (sock, &rfds);
		FD_SET (listen_fd, &rfds);
		max_fd = MAX (sock, listen_fd);
		for (i = 0; i<MAX_CLIENTS; i++) {
			if (client_fds[i] != -1) {
				FD_SET (client_fds[i], &rfds);
				max_fd = MAX (max_fd, client_fds[i]);
			}
		}
		rc = select (max_fd + 1, &rfds, NULL, NULL, &tv);
		if (rc < 0) {
			die("select failed", strerror(errno));
		} else if (rc == 0) {
			/* Timeout -- do nothing. (This is so that we go round
			 * the loop and check events periodically even if no
			 * packets are coming in.) */
			continue;
		}

		if (FD_ISSET(sock, &rfds)) {
			/* Got a packet. */
			len = recvfrom(sock, buf, sizeof buf, 0, (struct sockaddr *) &from_addr, &from_len);
			if (len < 0) {
				die ("Cannot receive from socket", strerror (errno));
			}

			handle_message (buf, len, &from_addr);
		}
		for (i = 0; i < MAX_CLIENTS; i++) {
			if (client_fds[i] != -1 && FD_ISSET(client_fds[i], &rfds)) {
				int msglen;

				if ((full_read (client_fds[i], (char *) &msglen, sizeof msglen) < 0) ||
						(msglen > sizeof buf) ||
						(full_read(client_fds[i], buf, msglen) < 0)) {
					printf ("[%5d] Disconnect\n", client_fds[i]);
					close (client_fds[i]);
					client_fds[i] = -1;
				}
				handle_message (buf, msglen, &from_addr);
			}
		}
		if (FD_ISSET (listen_fd, &rfds)) {
			struct sockaddr_in client_addr;
			socklen_t size = sizeof client_addr;

			int fd = accept (listen_fd,
			                (struct sockaddr *) &client_addr,
			                &size);
			if (fd < 0) {
				die ("accept failed", strerror (errno));
			}
			printf ("[%5d] Connect from %08x\n", fd, client_addr.sin_addr.s_addr);

			for (i = 0; i < MAX_CLIENTS; i++) {
				if (client_fds[i] == -1) {
					client_fds[i] = fd;
					break;
				}
			}
			if (i == MAX_CLIENTS) {
				die ("Too many connections", NULL);
			}
		}
	}

	return 0;
}

/*}}}*/


