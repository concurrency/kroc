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
#include "flash_driver.h"

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
		fprintf (stderr, "Error; unable to open %s: %s (%d)\n", fn, strerror (errno), errno);
		return NULL;
	}
	
	/* Find end of file minus footer length */
	fseek (fh, -bin_footer_len, SEEK_END);
	eof = ftell (fh);

	if (fread (buf, 1, bin_footer_len, fh) < bin_footer_len) {
		fprintf (stderr, "Error; unable to read firmware footer from %s: %s (%d)\n", 
			fn, strerror (errno), errno);
		goto errout;
	}

	tmp = le32_to_uint32 (&(buf[0]));
	if (tmp != 0xDEADBEEF) {
		fprintf (stderr, "Error; firmware magic does not match for %s: %08x\n", fn, tmp);
		goto errout;
	}

	/* Sanity check */
	if ((eof < 0L) || (eof > (1024L * 1024L))) {
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
		fprintf (stderr, "Error; unable to read firmware data from %s: %s (%d)\n", 
			fn, strerror (errno), errno);
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
 * 'S%08x,%08x#'=> ()		Send buffer to $address for $length
 * 'R%08x,%08x#'=> data		Read buffer from $address for $length
 * 'G%08x#'	=> ()		Jump to $address
 */

static int samba_handshake (brick_t *b) {
	uint8_t buf[2];
	int r;

	buf[0] = 'N';
	buf[1] = '#';

	r = b->write (b, buf, 2, 1000);
	if (r == 2) {
		r = b->read (b, buf, 2, 1000);
		if (r != 2) {
			fprintf (stderr, "Error reading SAMBA handshake response\n");
		} else if ((buf[0] != '\n') || (buf[1] != '\r')) {
			fprintf (stderr, "Error; bad SAMBA handshake response: %02x %02x\n",
				buf[0], buf[1]);
		} else {
			/* OK */
			return 0;
		}
	} else {
		fprintf (stderr, "Error writing SAMBA handshake\n");
	}

	return -1;
}

static int samba_write_buffer (brick_t *b, uint32_t addr, uint32_t len, const uint8_t *data) {
	char cmd_buf[24];
	int cmd_len, r;

	cmd_len = sprintf (cmd_buf, "S%08x,%08x#", addr, len);

	if ((r = b->write (b, (uint8_t *) cmd_buf, cmd_len, 1000)) == cmd_len) {
		if ((r = b->write (b, data, len, 1000)) == len) {
			return 0;
		} else {
			fprintf (stderr, "Error writing SAMBA data: %d\n", r);
		}
	} else {
		fprintf (stderr, "Error writing SAMBA command: %d\n", r);
	}
	
	return -1;
}

static int samba_read_buffer (brick_t *b, uint32_t addr, uint32_t len, uint8_t *data) {
	char cmd_buf[24];
	int cmd_len, r;

	cmd_len = sprintf (cmd_buf, "R%08x,%08x#", addr, len);

	if ((r = b->write (b, (uint8_t *) cmd_buf, cmd_len, 1000)) == cmd_len) {
		if ((r = b->read (b, data, len, 1000)) == len) {
			return 0;
		} else {
			fprintf (stderr, "Error reading SAMBA data: %d\n", r);
		}
	} else {
		fprintf (stderr, "Error writing SAMBA command: %d\n", r);
	}
	
	return -1;
}

static int samba_write_word (brick_t *b, uint32_t addr, uint32_t word)
{
	char cmd_buf[24];
	int cmd_len, r;

	cmd_len = sprintf (cmd_buf, "W%08x,%08x#", addr, word);

	if ((r = b->write (b, (uint8_t *) cmd_buf, cmd_len, 1000)) == cmd_len) {
		return 0;
	} else {
		fprintf (stderr, "Error writing SAMBA command: %d\n", r);
	}
	
	return -1;
}

static int samba_jump (brick_t *b, uint32_t addr) {
	char cmd_buf[24];
	int cmd_len, r;

	cmd_len = sprintf (cmd_buf, "G%08x#", addr);

	if ((r = b->write (b, (uint8_t *) cmd_buf, cmd_len, 1000)) == cmd_len) {
		return 0;
	} else {
		fprintf (stderr, "Error writing SAMBA command: %d\n", r);
	}
	
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
					fprintf (stdout, "Firmware verified; jumping to bootstrap...\n");
					if (samba_jump (b, fw->boot_addr) == 0) {
						fprintf (stdout, "Booted firmware on NXT.\n");
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

int flash_nxt (brick_t *b, nxt_firmware_t *fw) {
	int ret = -1;
	int i, r;

	fprintf (stdout, "Trying to SAMBA NXT @%08x...\n", b->id);

	r = b->open (b);
	if (!r) {
		if (samba_handshake (b) == 0) {
			uint8_t *data = fw->data;
			size_t len = fw->len;

			fprintf (stdout, "Handshake complete; sending flash driver...\n");
			if (samba_write_buffer (b, FLASH_DRIVER_ADDR, sizeof (flash_driver), flash_driver) == 0) {
				int pages = (fw->len + (PAGE_SIZE - 1)) / PAGE_SIZE;

				fprintf (stdout, "Flash loader sent to NXT.\n");

				for (i = 0; i < pages; ++i) {
					uint8_t buf[PAGE_SIZE];

					if (len > PAGE_SIZE) {
						memcpy (buf, data, PAGE_SIZE);
						data += PAGE_SIZE;
						len -= PAGE_SIZE;
					} else {
						memcpy (buf, data, len);
						memset (buf + len, 0xff, PAGE_SIZE - len);
					}

					fprintf (stdout, "Flashing page %d...\n", i);
					if (samba_write_word (b, PAGE_N_ADDR, i) != 0) {
						fprintf (stderr, "Error; write failed.\n");
						break;
					}
					if (samba_write_buffer (b, PAGE_BUF_ADDR, PAGE_SIZE, buf) != 0) {
						fprintf (stderr, "Error; write failed.\n");
						break;
					}
					if (samba_jump (b, FLASH_DRIVER_ADDR) != 0) {
						fprintf (stderr, "Error; flash jump failed.\n");
						break;
					}
				}

				if (i == pages) {
					uint8_t *buf = (uint8_t *) malloc (fw->len);

					fprintf (stdout, "Verifying firmware...\n");
					r = samba_read_buffer (b, fw->write_addr, fw->len, buf);
					if ((r == 0) && (memcmp (buf, fw->data, fw->len) == 0)) {
						fprintf (stdout, "Firmware verified.\n");
						if (samba_jump (b, fw->boot_addr) == 0) {
							fprintf (stdout, "Booted firmware on NXT.\n");
							ret = 0;
						}
					}

					free (buf);
				}
			}
		}
		b->close (b);
	} else {
		fprintf (stderr, "Error; unable to open brick.\n");
	}

	return ret;
}

int send_tbc_to_nxt (brick_t *b, tbc_t *tbc) {
	const unsigned int pkt_size = 64;
	int ret = -1;
	int r;

	fprintf (stdout, "Trying to send TBC to NXT @%08x...\n", b->id);
	
	r = b->open (b);
	if (!r) {
		unsigned int pos = 0;
		uint8_t buf[4];

		fprintf (stdout, "Sending bytecode\n");
		while (pos < tbc->len) {
			unsigned int pkt = (tbc->len - pos) > pkt_size ? pkt_size : tbc->len - pos;

			/* Send a packet */
			if ((r = b->write (b, tbc->data + pos, pkt, 1000)) == pkt) {
				fprintf (stdout, ".");
				fflush (stdout);

				/* Wait for ACK */
				if ((r = b->read (b, buf, 4, 1000)) == 4) {
					fprintf (stdout, "+");
					fflush (stdout);
					pos += pkt;
				} else {
					break;
				}
			} else {
				break;
			}
		}

		if (pos == tbc->len) {
			fprintf (stdout, "\n");
			fprintf (stdout, "Bytecode sent to NXT.\n");
		} else {
			fprintf (stderr, "Error sending bytecode at position %d: %d\n", pos, r);
		}
		b->close (b);
	} else {
		fprintf (stderr, "Error; unable to open brick.\n");
	}
	
	return ret;
}
