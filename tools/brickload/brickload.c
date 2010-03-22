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

static const char *prog_name = "brickload";

brick_t *merge_brick_lists (brick_t *a, brick_t *b) {
	if (a == NULL && b == NULL) {
		return NULL;
	} else if (a == NULL && b != NULL) {
		return b;
	} else if (a != NULL && b == NULL) {
		return a;
	} else {
		brick_t *n_list;
		int a_count = 0, b_count = 0;

		while (a[a_count].type != NULL_BRICK)
			a_count++;
		while (b[b_count].type != NULL_BRICK)
			b_count++;
		
		n_list = (brick_t *) malloc (sizeof (brick_t) * (a_count + b_count + 1));
		memcpy (&(n_list[0]), &(a[0]), sizeof (brick_t) * a_count);
		memcpy (&(n_list[a_count]), &(b[0]), sizeof (brick_t) * b_count);
		n_list[a_count + b_count].type = NULL_BRICK;

		free (a);
		free (b);

		return n_list;
	}
}

void free_brick_list (brick_t *list) {
	if (list != NULL) {
		int i;

		for (i = 0; list[i].type != NULL_BRICK; ++i) {
			list[i].release (&(list[i]));
		}

		free (list);
	}
}

static brick_t *find_brick_by_id (brick_t *list, const char *id) {
	/* FIXME: implement */
	return NULL;
}

static int do_list (void) {
	brick_t *list 	= NULL;
	void *usb 	= init_usb ();
	int i;
	
	if (usb == NULL) {
		return -1;
	}

	/* Give RCX towers a kick, so we can find their interfaces */
	configure_rcx_towers (usb);

	list = merge_brick_lists (list,
		find_usb_devices (usb, LEGO_VENDOR_ID, LEGO_PRODUCT_TOWER, 0x1, 0x0, LEGO_RCX)
	);
	list = merge_brick_lists (list,
		find_usb_devices (usb, LEGO_VENDOR_ID, LEGO_PRODUCT_NXT, 0x1, 0x0, LEGO_NXT)
	);
	list = merge_brick_lists (list,
		find_usb_devices (usb, LEGO_VENDOR_ID, LEGO_PRODUCT_NXOS, 0x1, 0x0, LEGO_NXT | NXOS_BRICK)
	);
	list = merge_brick_lists (list,
		find_usb_devices (usb, ATMEL_VENDOR_ID, ATMEL_PRODUCT_SAMBA, 0x1, 0x1, LEGO_NXT | SAMBA_BRICK)
	);

	if (list != NULL) {
		fprintf (stdout, "-- Bricks --\n");
		for (i = 0; list[i].type != NULL_BRICK; ++i) {
			int type 	= list[i].type & BRICK_TYPE_MASK;
			int flags 	= list[i].type & BRICK_FLAG_MASK;
			switch (type) {
				case LEGO_RCX:
					fprintf (stdout, "RCX TOWER");
					break;
				case LEGO_NXT:
					fprintf (stdout, "NXT      ");
					break;
				default:
					fprintf (stdout, "UNKNOWN  ");
					break;
			}
			fprintf (stdout, " @%08x", list[i].id);
			if (flags & NXOS_BRICK)
				fprintf (stdout, " NXOS");
			if (flags & SAMBA_BRICK)
				fprintf (stdout, " SAMBA");
			fprintf (stdout, "\n");
		}
		free_brick_list (list);
	} else {
		fprintf (stdout, "No bricks found.\n");
	}

	free_usb (usb);

	return 0;
}

static int do_sambaNXT (int argc, char *argv[]) {
	int ret	= -1;

	if (argc == 0) {
		fprintf (stderr, "Usage: %s sambaNXT <firmware> [<brick-id>]\n", prog_name);
		fprintf (stderr, "Boot NXT using firmware via SAMBA.\n");
		fprintf (stderr, "    e.g. %s sambaNXT tvm-nxt.bin @00000001\n", prog_name);
	} else {
		brick_t *list 	= NULL;
		brick_t *b	= NULL;
		void *usb 	= init_usb ();
		
		if (usb == NULL) {
			return -1;
		}

		/* Get SAMBAing NXTs */
		list = merge_brick_lists (list,
			find_usb_devices (usb, ATMEL_VENDOR_ID, ATMEL_PRODUCT_SAMBA, 0x1, 0x1, LEGO_NXT | SAMBA_BRICK)
		);
		
		if (list != NULL) {
			if (argc >= 2) {
				b = find_brick_by_id (list, argv[1]); 
			} else {
				b = &(list[0]);
			}

			if (b != NULL) {
				/* load firmware */
				if (boot_nxt (b, NULL, 0) == 0) {
					ret = 0;
				}
			} else if (argc >= 2) {
				fprintf (stderr, "NXT %s not found (check SAMBA mode?)\n", argv[1]);
			}

			free_brick_list (list);
		} else {
			fprintf (stderr, "No SAMBAing NXT bricks found.\n");
		}
		
		free_usb (usb);

	}
	return ret;
}

static void usage (void) {
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

static int not_implemented (void) {
	fprintf (stderr, "Sorry, this feature is not yet implemented.\n");
	return 1;
}

int main (int argc, char *argv[]) {
	if (argc < 2) {
		usage ();
		return 1;
	} else {
		const char *verb = argv[1];
		int ret = 1;

		if (strcmp (verb, "list") == 0) {
			ret = do_list ();
		} else if (strcmp (verb, "sambaNXT") == 0) {
			ret = do_sambaNXT (argc - 2, &(argv[2]));
		} else if (strcmp (verb, "sambaRXT") == 0) {
			ret = not_implemented ();
		} else if (strcmp (verb, "loadNXT") == 0) {
			ret = not_implemented ();
		} else if (strcmp (verb, "loadRXT") == 0) {
			ret = not_implemented ();
		} else {
			usage ();
		}

		return (ret < 0 ? -ret : ret);
	}
	return 0;
}
