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

static uint32_t le32_to_uint32 (uint8_t *le) {
	return (((uint32_t) le[3]) << 24) | (((uint32_t) le[2]) << 16) | 
		(((uint32_t) le[1]) << 8) | ((uint32_t) le[0]);
}

nxt_firmware_t *load_nxt_firmware (const char *fn) {
	/* NXOS binary footer is:
	 * 	magic		(4 bytes, little-endian) 
	 * 	in-ram size	(")
	 * 	in-rom size	(")
	 * 	write address	(")
	 *	boot address	(")
	 *	samba flag	(1 byte)
	 * Total: 21 bytes
	 */
	const long 	bin_footer_len	= 21;
	
	nxt_firmware_t 	*fw		= NULL;
	uint32_t	tmp;
	uint8_t 	buf[32];	/* must be big enough for footer */
	long		eof;
	FILE 		*fh;

	if ((fh = fopen (fn, "rb")) == NULL) {
		/* FIXME: textual error number? (cross platform?) */
		fprintf (stderr, "Error; unable to open %s: %d\n", fn, errno);
		return NULL;
	}
	
	/* Find end of file minus footer length */
	fseek (fh, -bin_footer_len, SEEK_END);
	eof = ftell (fh);

	if (fread (buf, 1, bin_footer_len, fh) < bin_footer_len) {
		/* FIXME: textual error number? (cross platform?) */
		fprintf (stderr, "Error; unable to read firmware footer from %s: %d\n", fn, errno);
		goto errout;
	}

	tmp = le32_to_uint32 (&(buf[0]));
	if (tmp != 0xDEADBEEF) {
		/* FIXME: textual error number? (cross platform?) */
		fprintf (stderr, "Error; firmware magic does not match for %s: %08x\n", fn, tmp);
		goto errout;
	}

	/* Sanity check */
	if ((eof < 0L) || eof > (1024L * 1024L)) {
		fprintf (stderr, "Error; firmware file seems too big: %s\n", fn);
		goto errout;
	}

	fw = (nxt_firmware_t *) malloc (sizeof (nxt_firmware_t) + eof);
	fw->in_ram	= le32_to_uint32 (&(buf[4]));
	fw->in_rom	= le32_to_uint32 (&(buf[8]));
	fw->write_addr	= le32_to_uint32 (&(buf[12]));
	fw->boot_addr	= le32_to_uint32 (&(buf[16]));
	fw->len		= eof;
	fw->data 	= ((uint8_t *)fw) + (sizeof (nxt_firmware_t));
	
	fseek (fh, 0L, SEEK_SET);
	if (fread (fw->data, fw->len, 1, fh) != 1) {
		/* FIXME: textual error number? (cross platform?) */
		fprintf (stderr, "Error; unable to read firmware data from %s: %d\n", fn, errno);
		goto errout;
	}
	fclose (fh);

	return fw; 
errout:
	if (fw != NULL)
		free (fw);
	fclose (fh);
	return NULL;
}

/** SAMBA notes
 * 'N#' 	=> '\n\r'	Handshake
 * 'S%08x,%08x'	=> ()		Send buffer to $address for $length
 * 'R%08x,%08x' => ()		Read buffer from $address for $length
 * 'G%08x'	=> ()		Jump to $address
 */

static int samba_handshake (brick_t *b) {
	uint8_t buf[2];
	int r;

	buf[0] = 'N';
	buf[1] = '#';

	r = b->write (b, buf, 2, 0);
	if (r == 2) {
		r = b->read (b, buf, 2, 0);
		if (r != 2) {
			fprintf (stderr, "Error reading handshake response\n");
		} else if ((buf[0] != '\n') || (buf[1] != '\r')) {
			fprintf (stderr, "Error bad handshake response: %02x %02x\n",
				buf[0], buf[1]);
		} else {
			/* OK */
			return 0;
		}
	} else {
		fprintf (stderr, "Error writing handshake\n");
	}

	return -1;
}

static int samba_write_buffer (brick_t *b, uint32_t addr, uint32_t len, uint8_t *data) {
	return -1;
}

static int samba_read_buffer (brick_t *b, uint32_t addr, uint32_t len, uint8_t *data) {
	return -1;
}

static int samba_jump (brick_t *b, uint32_t jump) {
	return -1;
}

int boot_nxt (brick_t *b, nxt_firmware_t *fw) {
	int ret = -1;
	int r;

	fprintf (stdout, "Trying to SAMBA NXT @%08x...\n", b->id);

	r = b->open (b);
	if (!r) {
		if (samba_handshake (b) == 0) {
			uint8_t *buf = (uint8_t *) malloc (fw->len);

			fprintf (stdout, "Handshake complete; sending firmware...\n");
			if (samba_write_buffer (b, fw->write_addr, fw->len, fw->data) == 0) {
				fprintf (stdout, "Firmware loaded to NXT; verifying...\n");
				r = samba_read_buffer (b, fw->write_addr, fw->len, buf);
				if ((r == 0) && (memcmp (buf, fw->data, fw->len) == 0)) {
					fprintf (stdout, "Firmware verified; jumping to bootstrap\n");
					if (samba_jump (b, fw->boot_addr) == 0) {
						ret = 0;
					}
				}
			}
			
			free (buf);
		}
		b->close (b);
	} else {
		fprintf (stderr, "Error; unable to open brick.\n");
	}

	return ret;
}
