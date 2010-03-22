/* 
 *  nxt.c -- part of brickload NXT/RCX firmware and bytecode tool
 *  Copyright (C) 2010  Carl Ritson <cgr@kent.ac.uk>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "brickload.h"

int boot_nxt (brick_t *b, uint8_t *firmware, size_t len) {
	int ret = -1;
	int r;

	fprintf (stdout, "Trying to SAMBA NXT @%08x\n", b->id);

	r = b->open (b);
	if (!r) {
		uint8_t buf[2];

		buf[0] = 'N';
		buf[1] = '#';

		r = b->write (b, buf, 2, 0);
		if (r == 2) {
			r = b->read (b, buf, 2, 0);
			if (r == 2) {
				fprintf (stdout, "Got handshake bytes %02x %02x.\n", buf[0], buf[1]);
			} else {
				fprintf (stdout, "Error reading handshake response.\n");
			}
		} else {
			fprintf (stdout, "Error writing handshake.\n");
		}
		
		b->close (b);
	} else {
		fprintf (stdout, "Unable to open brick.\n");
	}

	return ret;
}
