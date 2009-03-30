/*
 *	netbard.c -- server for simple network barriers
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

void die(const char *msg, const char *err) {
	fprintf(stderr, "%s%s%s\n",
	        msg, (err == NULL) ? "" : ": ", (err == NULL) ? "" : err);
	exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {
	int port, enrolled, count, i;
	int listen_fd, *client_fds;
	struct sockaddr_in bind_addr;

	if (argc < 3)
		die("Usage: netbard PORT ENROLLED", NULL);
	port = atoi(argv[1]);
	count = enrolled = atoi(argv[2]);

	client_fds = malloc(enrolled * sizeof *client_fds);
	if (client_fds == NULL)
		die("malloc failed", strerror(errno));
	for (i = 0; i < enrolled; ++i)
		client_fds[i] = -1;

	listen_fd = socket(PF_INET, SOCK_STREAM, 0);
	if (listen_fd < 0)
		die("Cannot create socket", strerror(errno));

	i = 1;
	if (setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &i, sizeof i) < 0)
		die("Unable to set SO_REUSEADDR", strerror(errno));

	bind_addr.sin_family = AF_INET;
	bind_addr.sin_port = htons(port);
	bind_addr.sin_addr.s_addr = INADDR_ANY;
	if (bind(listen_fd,
	         (struct sockaddr *) &bind_addr, sizeof bind_addr) < 0)
		die("Cannot bind socket", strerror(errno));

	if (listen(listen_fd, enrolled) < 0)
		die("listen failed", strerror(errno));

	while (1) {
		fd_set rfds;
		int max_fd = listen_fd, rc;

		FD_ZERO(&rfds);
		FD_SET(listen_fd, &rfds);
		for (i = 0; i < enrolled; i++) {
			if (client_fds[i] != -1) {
				FD_SET(client_fds[i], &rfds);
				if (client_fds[i] > max_fd)
					max_fd = client_fds[i];
			}
		}
		rc = select(max_fd + 1, &rfds, NULL, NULL, NULL);
		if (rc < 0)
			die("select failed", strerror(errno));
		for (i = 0; i < enrolled; i++) {
			if (client_fds[i] != -1
			    && FD_ISSET(client_fds[i], &rfds)) {
				int rc;
				char c;
				rc = read(client_fds[i], &c, 1);
				if (rc == 0) {
					printf("[%5d] Close\n", client_fds[i]);
					close(client_fds[i]);
					client_fds[i] = -1;
					continue;
				}
				if (rc < 0)
					die("read failed", strerror(errno));
				printf("[%5d] Sync %c\n", client_fds[i], c);
				--count;
			}
			if (count == 0) {
				int j;
				printf("[-----] Barrier complete\n");
				for (j = 0; j < enrolled; j++) {
					if (write(client_fds[j], "X", 1) != 1)
						die("write failed",
						    strerror(errno));
				}
				count = enrolled;
			}
		}
		if (FD_ISSET(listen_fd, &rfds)) {
			struct sockaddr_in client_addr;
			socklen_t size = sizeof client_addr;

			int fd = accept(listen_fd,
			                (struct sockaddr *) &client_addr,
			                &size);
			if (fd < 0)
				die("accept failed", strerror(errno));
			printf("[%5d] Connect from %08x\n",
			       fd, client_addr.sin_addr.s_addr);
			for (i = 0; i < enrolled; i++) {
				if (client_fds[i] == -1) {
					client_fds[i] = fd;
					break;
				}
			}
			if (i == enrolled)
				die("Too many connections", NULL);
			if (write(fd, &i, sizeof i) != sizeof i)
				die("write failed", strerror(errno));
			if (write(fd, &enrolled, sizeof enrolled)
			    != sizeof enrolled)
				die("write failed", strerror(errno));
		}
	}

	return 0;
}
