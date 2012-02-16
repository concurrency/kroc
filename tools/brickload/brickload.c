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

tbc_t *load_tbc (const char *fn) {
	tbc_t	*tbc = NULL;
	long	eof;
	FILE	*fh;

	if ((fh = fopen (fn, "rb")) == NULL) {
		fprintf (stderr, "Error; unable to open %s: %s (%d)\n", fn, strerror (errno), errno);
		return NULL;
	}
	
	/* Get file length */
	fseek (fh, 0L, SEEK_END);
	eof = ftell (fh);
	fseek (fh, 0L, SEEK_SET);

	/* Sanity check */
	if (eof < 32L) {
		fprintf (stderr, "Error; bytecode file seems too small: %s\n", fn);
		goto out;
	}
	if (eof > (1024L * 1024L)) {
		fprintf (stderr, "Error; bytecode file seems too big: %s\n", fn);
		goto out;
	}
	
	tbc = (tbc_t *) malloc (sizeof (tbc_t) + eof);
	tbc->data = ((uint8_t *) tbc) + sizeof (tbc_t);
	tbc->len = eof;

	if (fread (tbc->data, 1, tbc->len, fh) < tbc->len) {
		fprintf (stderr, "Error; unable to read bytecode from %s: %s (%d)\n", 
			fn, strerror (errno), errno);
		free (tbc);
		tbc = NULL; 
		goto out;
	}

	if (memcmp (tbc->data, "tenc", 4) == 0) {
		tbc->type = TBC_16BIT;
	} else if (memcmp (tbc->data, "TEnc", 4) == 0) {
		tbc->type = TBC_32BIT;
	} else {
		fprintf (stderr, "Error; TBC header invalid: %02x%02x%02x%02x\n",
			tbc->data[0], tbc->data[1], tbc->data[2], tbc->data[3]
		);
		free (tbc);
		tbc = NULL;
	}

out:
	fclose (fh);
	return tbc;
}

static brick_t *find_brick_by_id (brick_t *list, const char *id) {
	uint32_t n_id = 0;

	if (id[0] == '@') {
		n_id = strtol (&(id[1]), NULL, 16); 
	} else {
		n_id = strtol (id, NULL, 16); 
	}

	if (list != NULL) {
		int i;
		
		for (i = 0; list[i].type != NULL_BRICK; ++i) {
			if (list[i].id == n_id)
				return &(list[i]);
		}
	}
	
	return NULL;
}

static brick_t *find_brick_by_type (brick_t *list, const int type) {
	if (list != NULL) {
		int i;
		
		for (i = 0; list[i].type != NULL_BRICK; ++i) {
			if (list[i].type == type)
				return &(list[i]);
		}
	}
	
	return NULL;
}

static brick_t *list_bricks (void *usb) {
	brick_t *list = NULL;

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

	return list;
}

static int do_list (void) {
	brick_t *list 	= NULL;
	void *usb 	= init_usb ();
	int i;
	
	if (usb == NULL) {
		return -1;
	}

	list = list_bricks (usb);

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
			if (type == LEGO_RCX) {
				char buf[32];
				if (get_rcx_version_str (&(list[i]), buf) == 0)
					fprintf (stdout, " - %s", buf);
			}
			fprintf (stdout, "\n");
		}
		free_brick_list (list);
	} else {
		fprintf (stdout, "No bricks found.\n");
	}

	free_usb (usb);

	return 0;
}

static int do_bootNXT (int argc, char *argv[]) {
	int ret	= -1;

	if (argc == 0) {
		fprintf (stderr, "Usage: %s bootNXT <firmware> [<brick-id>]\n", prog_name);
		fprintf (stderr, "Boot NXT using firmware via SAMBA.\n");
		fprintf (stderr, "    e.g. %s bootNXT tvm-nxt.bin @00000001\n", prog_name);
	} else {
		brick_t *list 	= NULL;
		brick_t *b	= NULL;
		void *usb 	= init_usb ();
		
		if (usb == NULL) {
			return -1;
		}

		list = list_bricks (usb); 
		
		if (argc >= 2) {
			b = find_brick_by_id (list, argv[1]); 
		} else {
			b = find_brick_by_type (list, LEGO_NXT | SAMBA_BRICK);
		}

		if (b != NULL) {
			nxt_firmware_t *fw = load_nxt_firmware (argv[0]);
			
			if (fw != NULL) {
				if (boot_nxt (b, fw) == 0) {
					ret = 0;
				}
			}
		} else if (argc >= 2) {
			fprintf (stderr, "NXT %s not found (check SAMBA mode?)\n", argv[1]);
		} else {
			fprintf (stderr, "No suitable NXT bricks found (check SAMBA mode?)\n");
		}

		free_brick_list (list);
		free_usb (usb);

	}
	return ret;
}

static int do_sendTBC (int argc, char *argv[]) {
	int ret	= -1;

	if (argc == 0) {
		fprintf (stderr, "Usage: %s sendTBC <file> [<NXT | RCX | brick-id>]\n", prog_name);
		fprintf (stderr, "Send bytecode to NXT/RCX running TVM.\n");
		fprintf (stderr, "    e.g. %s sendTBC bump-and-wander.tbc @00000001\n", prog_name);
		fprintf (stderr, "    e.g. %s sendTBC sonar-test.tbc NXT\n", prog_name);
	} else {
		brick_t *list 	= NULL;
		brick_t *b	= NULL;
		void *usb 	= init_usb ();
		int search_type	= NULL_BRICK;	
		
		if (usb == NULL) {
			return -1;
		}

		list = list_bricks (usb);
	
		if (argc >= 2) {
			if (strcasecmp (argv[1], "NXT") == 0) {
				search_type = LEGO_NXT | NXOS_BRICK;
			} else if (strcasecmp (argv[1], "RCX") == 0) {
				search_type = LEGO_RCX;
			} else {
				b = find_brick_by_id (list, argv[1]);
			}
		}

		if (search_type != NULL_BRICK) {
			b = find_brick_by_type (list, search_type);
		} else if (list != NULL) {
			b = &(list[0]);
		}

		if (b != NULL) {
			tbc_t *tbc = load_tbc (argv[0]); 
			if (tbc != NULL) {
				switch (b->type & BRICK_TYPE_MASK) {
					case LEGO_NXT:
						if (tbc->type == TBC_32BIT) { 
							if (send_tbc_to_nxt (b, tbc) == 0) {
								ret = 0;
							}
						} else {
							fprintf (stderr, "Error; NXT needs 32bit bytecode\n");
						}
						break;
					case LEGO_RCX:
						if (tbc->type == TBC_16BIT) { 
							if (send_tbc_to_rcx (b, tbc) == 0) {
								ret = 0;
							}
						} else {
							fprintf (stderr, "Error; RCX needs 16bit bytecode\n");
						}
						break;
				}
			}
		} else if (argc >= 2) {
			fprintf (stderr, "%s not found (check TVM running?)\n", argv[1]);
		} else {
			fprintf (stderr, "No suitable bricks found.\n");
		}
		
		free_brick_list (list);
		free_usb (usb);

	}
	return ret;
}

static int do_bootRCX (int argc, char *argv[]) {
	int ret	= -1;

	if (argc == 0) {
		fprintf (stderr, "Usage: %s bootRCX <firmware> [<brick-id>]\n", prog_name);
		fprintf (stderr, "Boot RCX using firmware via IR Tower.\n");
		fprintf (stderr, "    e.g. %s bootRCX tvm-rcx.bin @00000001\n", prog_name);
	} else {
		brick_t *list 	= NULL;
		brick_t *b	= NULL;
		void *usb 	= init_usb ();
		
		if (usb == NULL) {
			return -1;
		}

		list = list_bricks (usb); 
		
		if (argc >= 2) {
			b = find_brick_by_id (list, argv[1]); 
		} else {
			b = find_brick_by_type (list, LEGO_RCX);
		}

		if (b != NULL) {
			rcx_firmware_t *fw = load_rcx_firmware (argv[0]);
			
			if (fw != NULL) {
				fprintf (stdout, "Firmware loaded, base %04x, start %04x, length %d.\n",
					fw->addr, fw->start_addr, (int) fw->len);
				if (boot_rcx (b, fw) == 0) {
					ret = 0;
				}
			}
		} else if (argc >= 2) {
			fprintf (stderr, "RCX %s not found\n", argv[1]);
		} else {
			fprintf (stderr, "No RCX towers found\n");
		}

		free_brick_list (list);
		free_usb (usb);

	}
	return ret;
}

static void usage (void) {
	fprintf (stderr, "NXT/RCX Firmware and Bytecode Loading Tool\n\n");
	fprintf (stderr, "Usage: %s <verb> <options>, where <verb> is one of:\n\n",
		prog_name);
	fprintf (stderr, "    list          (List available bricks)\n");
	fprintf (stderr, "    bootNXT       (Boot NXT firmware by SAMBA)\n");
	fprintf (stderr, "    bootRCX       (Boot RCX firmware by IR Tower)\n");
	fprintf (stderr, "    sendTBC       (Send bytecode to brick)\n");
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
		} else if (strcmp (verb, "bootNXT") == 0) {
			ret = do_bootNXT (argc - 2, &(argv[2]));
		} else if (strcmp (verb, "bootRCX") == 0) {
			ret = do_bootRCX (argc - 2, &(argv[2]));
		} else if (strcmp (verb, "sendTBC") == 0) {
			ret = do_sendTBC (argc - 2, &(argv[2]));
		} else {
			usage ();
		}

		return (ret < 0 ? -ret : ret);
	}
	return 0;
}
