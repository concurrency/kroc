/* 
 *  brickload.c -- part of brickload NXT/RCX firmware and bytecode tool
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

static void do_list (void) {
	brick_t *list;
	void *usb = init_usb ();
	int i;
	
	if (usb == NULL) {
		return;
	}

	/* Find NXTs */
	list = find_usb_devices (usb, 0x0694, 0x0002, LEGO_NXT);
	if (list != NULL) {
		fprintf (stdout, "-- NXT Bricks --\n");
		for (i = 0; list[i].type == LEGO_NXT; ++i) {
			fprintf (stderr, "NXT %p %04x %04x\n", 
				list[i].handle,
				list[i].vendor,
				list[i].product
			);
			list[i].release (&(list[i]));
		}
		free (list);
	} else {
		fprintf (stdout, "No bricks found.\n");
	}

	free_usb (usb);
}

static void usage (const char *prog_name) {
	fprintf (stderr, "NXT/RCX Firmware and Bytecode Loading Tool\n\n");
	fprintf (stderr, "Usage: %s <verb> <options>, where <verb> is one of:\n\n",
		prog_name);
	fprintf (stderr, "    list          (List available bricks)\n");
	fprintf (stderr, "    sambaNXT      (Load NXT firmware by SAMBA)\n");
	fprintf (stderr, "    sambaRXT      (Load RXT firmware by SAMBA)\n");
	fprintf (stderr, "    loadNXT       (Load bytecode to NXT)\n");
	fprintf (stderr, "    loadRXT       (Load bytecode to RXT)\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "%s <verb> with no options gives help on the verb\n\n",
		prog_name);
}

static void not_implemented (void) {
	fprintf (stderr, "Sorry, this feature is not yet implemented.\n");
	exit (1);
}

int main (int argc, char *argv[]) {
	const char *prog_name = "brickload";
	if (argc < 2) {
		usage (prog_name);
		return 1;
	} else {
		const char *verb = argv[1];
		if (strcmp (verb, "list") == 0) {
			do_list ();
		} else if (strcmp (verb, "sambaNXT") == 0) {
			not_implemented ();
		} else if (strcmp (verb, "sambaRXT") == 0) {
			not_implemented ();
		} else if (strcmp (verb, "loadNXT") == 0) {
			not_implemented ();
		} else if (strcmp (verb, "loadRXT") == 0) {
			not_implemented ();
		} else {
			usage (prog_name);
			return 1;
		}
	}
	return 0;
}
