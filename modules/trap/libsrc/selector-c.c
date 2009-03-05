/*
 *	An IO scheduler for occam-pi (C support code)
 *	Copyright (C) 2008, 2009 Adam Sampson <ats@offog.org>
 *
 *	This library is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU Lesser General Public
 *	License as published by the Free Software Foundation, either
 *	version 2 of the License, or (at your option) any later version.
 *
 *	This library is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *	Lesser General Public License for more details.
 *
 *	You should have received a copy of the GNU Lesser General Public
 *	License along with this library.  If not, see
 *	<http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <poll.h>
#include <errno.h>

#include <cif.h>

/*{{{  types */
typedef struct {
	int fd;
	int events;
	mt_cb_t *signal;
} new_fd;
/*}}}*/
/*{{{  occam constants */
enum selector_events {
	SELECT_read = 1,
	SELECT_write = 2
};

enum selector_signal_req_tags {
	SSREQ_ready = 0,
	SSREQ_shutdown
};

enum selector_signal_resp_tags {
	SSRESP_again = 0,
	SSRESP_remove
};

enum selector_req_tags {
	SREQ_add = 0,
	SREQ_shutdown
};
/*}}}*/
/*{{{  selector_make_nonblocking */
void selector_make_nonblocking (Workspace wptr)
{
	int fd = ProcGetParam (wptr, 0, int);

	int flags = fcntl (fd, F_GETFL);

	if (flags == -1 || fcntl (fd, F_SETFL, flags | O_NONBLOCK) != 0) {
		perror ("fcntl");
		SetErr ();
	}
}
/*}}}*/
/*{{{  req_handler */
static void req_handler (Workspace wptr)
{
	mt_cb_t *svr	= ProcGetParam (wptr, 0, mt_cb_t *);
	Channel *new	= ProcGetParam (wptr, 1, Channel *);
	int pipe_w	= ProcGetParam (wptr, 2, int);

	Channel *req = &(svr->channels[0]);

	while (1) {
		char tag;

		ChanInChar (wptr, req, &tag);
		switch (tag) {
		case SREQ_add: {
			new_fd n;
			int dummy;

			ChanInInt (wptr, req, &(n.fd));
			ChanInInt (wptr, req, &(n.events));
			MTChanIn (wptr, req, (void **) &(n.signal));

			if (write (pipe_w, "x", 1) != 1) {
				perror ("write to pipe");
				SetErr ();
			}

			ChanOutInt (wptr, new, (int) &n);
			ChanInInt (wptr, new, &dummy);

			break;
		}
		case SREQ_shutdown: {
			/* FIXME: implement */
			SetErr ();
			break;
		}
		}
	}
}
/*}}}*/
/*{{{  resize_fds */
static void resize_fds (struct pollfd **fds, mt_cb_t ***signals, int *size, int new_size)
{
	struct pollfd *new_fds = realloc (*fds, new_size * sizeof (*new_fds));
	mt_cb_t **new_signals = realloc (*signals, new_size * sizeof (*new_signals));

	if (new_size != 0 && (new_fds == NULL || new_signals == NULL)) {
		perror ("realloc");
		SetErr ();
	}

	*fds = new_fds;
	*signals = new_signals;
	*size = new_size;
}
/*}}}*/
/*{{{  poll_loop */
static void poll_loop (Workspace wptr)
{
	Channel *new	= ProcGetParam (wptr, 0, Channel *);
	int pipe_r	= ProcGetParam (wptr, 1, int);

	struct pollfd *fds = NULL;
	mt_cb_t **signals = NULL;
	int num_fds = 1;
	int size_fds = 0;

	resize_fds (&fds, &signals, &size_fds, 256);
	fds[0].fd = pipe_r;
	fds[0].events = POLLIN;

	while (1) {
		int num_ready, i;

		/* We try a non-blocking poll first, and if nothing's ready
		 * immediately then we do a blocking call.
		 * This avoids the BSC overhead when there's something that can
		 * fire immediately (which is quite often the case -- for
		 * example, the first write to a socket).
		 *
		 * It would be nicer to avoid the blocking call entirely in the
		 * future.
		 */
		num_ready = poll (fds, num_fds, 0);
		if (num_ready == 0) {
			num_ready = BlockingCallN (wptr, poll, 3,
			                           fds, num_fds, -1);
		}

		/* If we're interrupted by a signal, just try again. */
		if (num_ready == -1 && errno == EINTR)
			continue;

		if (num_ready == -1) {
			perror ("poll");
			SetErr ();
		}

		for (i = 0; num_ready > 0 && i < num_fds; i++) {
			if (fds[i].revents == 0)
				continue;

			if (i == 0) {
				char c;
				new_fd *n;

				if (read (pipe_r, &c, 1) != 1) {
					perror ("read pipe");
					SetErr ();
				}

				ChanInInt (wptr, new, (int *) &n);

				if (num_fds == size_fds)
					resize_fds (&fds, &signals, &size_fds, 2 * size_fds);

				fds[num_fds].fd = n->fd;
				fds[num_fds].events = (n->events & SELECT_read ? POLLIN : 0)
				                      | (n->events & SELECT_write ? POLLOUT : 0);
				fds[num_fds].revents = 0;
				signals[num_fds] = n->signal;
				++num_fds;

				ChanOutInt (wptr, new, 42);
			} else {
				Channel *signal = &(signals[i]->channels[0]);
				Channel *ack = &(signals[i]->channels[1]);
				char tag;

				ChanOutChar (wptr, signal, SSREQ_ready);
				ChanInChar (wptr, ack, &tag);
				switch (tag) {
				case SSRESP_again:
					break;
				case SSRESP_remove:
					MTRelease (wptr, signals[i]);
					signals[i] = NULL;
					if (i != num_fds - 1) {
						fds[i] = fds[num_fds - 1];
						signals[i] = signals[num_fds - 1];
					}
					--num_fds;
					break;
				}
			}

			--num_ready;
		}
	}
}
/*}}}*/
/*{{{  selector_server */
void selector_server (Workspace wptr)
{
	mt_cb_t *svr = ProcGetParam (wptr, 0, mt_cb_t *);

	int pipe_fds[2];
	Channel new;
	Workspace p1, p2;
	word stack_p1[WORKSPACE_SIZE (3, 4096)];
	word stack_p2[WORKSPACE_SIZE (2, 4096)];

	if (pipe (pipe_fds) != 0) {
		perror ("pipe");
		SetErr ();
	}
	ChanInit (wptr, &new);

	p1 = LightProcInit (wptr, stack_p1, 3, 4096);
	ProcParam (wptr, p1, 0, svr);
	ProcParam (wptr, p1, 1, &new);
	ProcParam (wptr, p1, 2, pipe_fds[1]);

	p2 = LightProcInit (wptr, stack_p2, 2, 4096);
	ProcParam (wptr, p2, 0, &new);
	ProcParam (wptr, p2, 1, pipe_fds[0]);

	ProcPar (wptr, 2, p1, req_handler, p2, poll_loop);

	close (pipe_fds[0]);
	close (pipe_fds[1]);
}
/*}}}*/
