/*
 *	ts: timestamp pipe
 *	Copyright (C) 2008  Carl Ritson <cgr@kent.ac.uk>
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>

int main (int argc, char *argv[])
{
	struct timeval tval;
	char buf[1024];
	int len = 0;
	int new_line = 1;

	for (;;) {
		int c = fgetc (stdin);

		if (new_line) {
			gettimeofday (&tval, NULL);
		}

		buf[len++] = c;
		
		if (c == '\n' || c == EOF || len >= sizeof (buf)) {
			if (c == EOF && len <= 1)
				break;
			fprintf (
				stdout, "%lu.%06lu ", 
				(unsigned long) tval.tv_sec, 
				(unsigned long) tval.tv_usec
			);
			if (c == EOF) {
				fwrite (buf, len - 1, 1, stdout);
				break;
			} else {
				fwrite (buf, len, 1, stdout);
			}
			len = 0;
			new_line = 1;
		}
	}

	return 0;
}
