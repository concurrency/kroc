/* 
 *  rxt.c -- part of brickload NXT/RCX firmware and bytecode tool
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

#define TOWER_REQUEST_RESET		0x04
#define TOWER_REQUEST_GET_VERSION	0xfd

#define USB_TYPE_VENDOR			(0x02 << 5)
#define USB_DIR_IN			0x80
#define USB_RECIP_DEVICE		0x00

#define PKT_SIZE			8
#define IR_BLOCK_SIZE			200

void configure_rcx_towers (void *usb) {
	brick_t *list = find_usb_devices (usb, LEGO_VENDOR_ID, LEGO_PRODUCT_TOWER, 0x0, 0x0, LEGO_RCX);

	if (list != NULL) {
		int i;
		
		for (i = 0; list[i].type != NULL_BRICK; ++i) {
			brick_t *b = &(list[i]);
			int config = b->get_config (b);

			if (config >= 0 && config != 1) {
				b->set_config (b, 1);
				/* FIXME: check return value and report? */
			}
		}

		free_brick_list (list);
	}
}

int send_tbc_to_rcx (brick_t *b, tbc_t *tbc) {
	return -1;
}

int get_rcx_version_str (brick_t *b, char *str) {
	int ret;
	
	if ((ret = b->open (b)) == 0) {
		uint8_t buf[8];
		
		ret = b->control (b, 
			USB_TYPE_VENDOR | USB_DIR_IN | USB_RECIP_DEVICE,
			TOWER_REQUEST_GET_VERSION,
			0,
			0,
			buf, 8,
			1000
		); 

		if (ret == 8) {
			sprintf (str, 
				"version %d.%d (build %d)",
				buf[4], buf[5], (buf[6] << 8) | buf[7]
			);
			ret = 0;
		} else {
			ret = -1;
		}

		b->close (b);
	}
	
	return ret;
}

/* precondition: brick is open */
static int reset_tower (brick_t *b) {
	uint8_t buf[4];
	
	return b->control (b, 
		USB_TYPE_VENDOR | USB_DIR_IN | USB_RECIP_DEVICE,
		TOWER_REQUEST_RESET,
		0,
		0,
		buf, 8,
		1000
	); 
}

static int send_to_rcx (brick_t *b, uint8_t *data, size_t len) {
	uint8_t buf_bytes[PKT_SIZE * 8];
	uint8_t *buf 	= &(buf_bytes[0]);
	uint8_t *m_buf 	= NULL;
	int pos		= 0;
	int sum		= 0;
	int ret		= 0;

	if (((len * 2) + 5) > sizeof (buf_bytes))
		m_buf = buf = (uint8_t *) malloc ((len * 2) + 5);

	buf[pos++] = 0x55;
	buf[pos++] = 0xff;
	buf[pos++] = 0x00;
	while (len--) {
		uint8_t byte = *(data++);
		buf[pos++] = byte;
		buf[pos++] = ~byte;
		sum += byte;
	}
	buf[pos++] = (uint8_t) sum;
	buf[pos++] = (uint8_t) (~sum);

	len = pos;
	pos = 0;
	while (len > 0) {
		int bytes = len > PKT_SIZE ? PKT_SIZE : len;

		ret = b->write (b, buf + pos, bytes, 0);

		if (ret > 0) {
			len -= ret;
			pos += ret;
		} else {
			break;
		}
	}

	if (m_buf != NULL)
		free (m_buf);

	return ret;
}

static int recv_from_rcx (brick_t *b, uint8_t *data, size_t len) {
	uint8_t buf_bytes[PKT_SIZE * 8];
	uint8_t *buf 	= &(buf_bytes[0]);
	uint8_t *m_buf 	= NULL;
	int pos 	= 0;
	int ret		= -1;
	int i;

	if (((len * 2) + 5) > sizeof (buf_bytes))
		m_buf = buf = (uint8_t *) malloc ((len * 2) + 5);

	len = (len * 2) + 5;

	while (pos < len) {
		ret = b->read (b, buf + pos, 
			(len - pos) > PKT_SIZE ? PKT_SIZE : (len - pos), 
			1000
		);
		
		if (ret < 0)
			break;

		//for (i = 0; i < ret; ++i)
		//	fprintf (stderr, "byte %i = %02x\n", pos + i, buf[pos + i]);
		
		pos += ret;

		/* do header search */
		while (pos >= 3) {
			if (buf[0] != 0x55 || buf[1] != 0xff || buf[2] != 0x00) {
				memmove (buf, buf + 1, pos - 1);
				pos--;
			} else {
				break;
			}
		}
	}

	if (ret >= 0) {
		if (pos < 5) {
			/* short message */
			ret = -2;
		} else {
			int dlen 	= 0;
			int sum		= 0;

			for (i = 3; i < pos - 2; i += 2) {
				data[dlen++] = buf[i];
			}

			/* Regular message */
			for (i = 0; i < dlen; ++i) {
				sum += data[i];
			}
			if (buf[pos - 2] == (sum & 0xff)) {
				/* message OK */
				ret = dlen;
			} else {
				ret = -1;
			}
		}
	}

	if (m_buf != NULL)
		free (m_buf);

	return ret;
}

typedef struct _srec_t srec_t;
struct _srec_t {
	int 		type;
	uint32_t 	addr;
	int		len;
	uint8_t 	data[256];
	uint8_t		sum;
	srec_t		*next;
};

static int digit_to_int (char digit) {
	if (isdigit (digit))
		return digit - '0';
	else
		return 0;
}

static int hexdigit_to_int (char digit) {
	if (isdigit (digit)) {
		return digit - '0';
	} else {
		switch (tolower (digit)) {
			case 'a': return 10;
			case 'b': return 11;
			case 'c': return 12;
			case 'd': return 13;
			case 'e': return 14;
			case 'f': return 15;
			default:
				return 0;
		}
	}
}

static srec_t *load_srec (const char *line, const int len) {
	srec_t 	*r	= NULL;
	int	sum	= 0;
	int i, p;

	if (len < 5) {
		goto errout; /* too short */
	}
	if (line[0] != 'S') {
		goto errout; /* invalid header */
	}
	if (!isdigit (line[1])) {
		goto errout; /* invalid type */
	}
	
	r 	= (srec_t *) malloc (sizeof (srec_t));
	r->type	= digit_to_int (line[1]);
	r->len	= (hexdigit_to_int (line[2]) << 4) | (hexdigit_to_int (line[3]));
	if ((r->len + 4) > len) {
		goto errout; /* data longer than line */
	}
	
	sum = r->len;
	for (i = 0, p = 4; i < r->len - 1; i += 1, p += 2) {
		char digit_h = line[p];
		char digit_l = line[p + 1];
		/* FIXME: valid digits */
		r->data[i] = (hexdigit_to_int (digit_h) << 4) | (hexdigit_to_int (digit_l));
		sum += r->data[i];
	}
	r->sum	= (hexdigit_to_int (line[p]) << 4) | (hexdigit_to_int (line[p + 1]));
	r->len -= 1;

	if (((sum + r->sum) & 0xff) != 0xff) {
		goto errout; /* invalid checksum */
	}

	switch (r->type) {
		case 0: case 1: case 5: case 9:
			i = 2;
			break;
		case 2: case 8:
			i = 3;
			break;
		case 3: case 7:
			i = 4;
			break;
		default:
			i = 0;
			break;
	}

	r->addr = 0;
	r->len -= i;
	for (p = 0; p < i; ++p)
		r->addr = (r->addr << 8) | r->data[p];
	memmove (r->data, r->data + i, r->len);

	r->next = NULL;
	return r;
errout:
	if (r != NULL)
		free (r);
	return NULL;
}

rcx_firmware_t *load_rcx_firmware (const char *fn) {
	rcx_firmware_t 	*fw		= NULL;
	unsigned int	u_addr		= 0;
	unsigned int	l_addr		= -1;
	unsigned int	s_addr		= 0;
	srec_t		*sr		= NULL;
	srec_t		 *lr		= NULL;
	int		ln		= 0;
	char	 	buf[512];	/* must be big enough for a line */
	FILE 		*fh;

	if ((fh = fopen (fn, "r")) == NULL) {
		fprintf (stderr, "Error; unable to open %s: %s (%d)\n", fn, strerror (errno), errno);
		return NULL;
	}

	while (!feof (fh)) {
		srec_t *r	= NULL;
		int 	p 	= 0;
		
		while ((buf[p] = fgetc (fh)) != EOF) {
			if (buf[p] == '\n') {
				ln++;
				break;
			}
			p++;
			if (p >= (sizeof (buf)))
				break;
		}

		r = load_srec (buf, p);
		
		if (r != NULL) {
			if (sr == NULL) {
				sr = lr = r;
			} else {
				lr->next = r;
				lr = r;
			}

			if (r->type == 1) {
				if (r->addr < l_addr)
					l_addr = r->addr;
				if ((r->addr + r->len) > u_addr)
					u_addr = r->addr + r->len;
			} else if (r->type == 9) {
				s_addr = r->addr;
			}
		} else if (p > 0) {
			fprintf (stderr, "Warning; unable to parse %s, line %d\n", fn, ln);
		}
	}
	
	fclose (fh);

	if (sr == NULL) {
		fprintf (stderr, "Error; no SREC data found in %s\n", fn);
		return NULL;
	} else if ((u_addr - l_addr) > 0x7000) {
		fprintf (stderr, "Error; SREC image too large in %s\n", fn);
		return NULL;
	}

	fw 		= (rcx_firmware_t *) malloc (sizeof (rcx_firmware_t) + (u_addr - l_addr));
	fw->addr	= l_addr;
	fw->start_addr	= s_addr;
	fw->data 	= ((uint8_t *)fw) + (sizeof (rcx_firmware_t));
	fw->len		= u_addr - l_addr;

	memset (fw->data, 0, fw->len);
	lr = sr;
	while (lr != NULL) {
		srec_t *nr = lr->next;

		if (lr->type == 1) {
			memcpy (fw->data + (lr->addr - fw->addr), lr->data, lr->len);
		}
		
		free (lr);
		lr = nr;
	}

	return fw;
}

static int awake_up_rcx (brick_t *b) {
	int 		retries = 5;
	uint8_t 	buf[8];
	int		ret;

	while (retries--) {
		fprintf (stdout, "Wake up brick...");
		fflush (stdout);

		buf[0] = retries & 1 ? 0x10 : 0x18;
		if ((ret = send_to_rcx (b, buf, 1)) < 0)
			return ret;
		if ((ret = recv_from_rcx (b, buf, 1)) > 0) {
			fprintf (stdout, " OK.\n");
			return 0;
		}

		fprintf (stdout, " no response.\n");
	}

	return ret;
}

static int delete_firmware (brick_t *b) {
	int 		retries = 5;
	uint8_t 	buf[8];
	int		ret;

	while (retries--) {
		fprintf (stdout, "Delete firmware...");
		fflush (stdout);

		buf[0] = 0x65;
		buf[1] = 1;
		buf[2] = 3;
		buf[3] = 5;
		buf[4] = 7;
		buf[5] = 11;

		if ((ret = send_to_rcx (b, buf, 6)) < 0)
			return ret;
		if ((ret = recv_from_rcx (b, buf, 1)) > 0) {
			fprintf (stdout, " OK.\n");
			return 0;
		}

		fprintf (stdout, " no response.\n");
	}

	return ret;
}

static int upload_firmware (brick_t *b, rcx_firmware_t *fw) {
	int retries = 5;
	uint8_t buf[IR_BLOCK_SIZE + 8];
	int addr, block, pos;
	int i, ret, sum;

	ret = ((fw->start_addr + fw->len) < 0xcc00) ? fw->len : 0xcc00 - fw->start_addr;
	for (i = 0, sum = 0; i < ret; ++i) {
		sum += fw->data[i];
	}

	while (retries--) { 
		fprintf (stdout, "Upload firmware header...");
		fflush (stdout);

		buf[0] = 0x75;
		buf[1] = (fw->start_addr >> 0) 	& 0xff;
		buf[2] = (fw->start_addr >> 8) 	& 0xff;
		buf[3] = (sum >> 0)		& 0xff;
		buf[4] = (sum >> 8)		& 0xff;
		buf[5] = 0;

		if ((ret = send_to_rcx (b, buf, 6)) < 0)
			return ret;
		if ((ret = recv_from_rcx (b, buf, 2)) > 0) {
			fprintf (stdout, " OK.\n");
			break;
		}

		fprintf (stdout, " no response.\n");
		ret = ret >= 0 ? -1 : ret;
	}

	if (ret < 0)
		return ret;

	fprintf (stdout, "Sending firmware to RCX...\n");
	addr	= fw->addr;
	block	= 1;
	pos	= 0;
	while (pos < fw->len) {
		uint8_t rbuf[8];
		int pkt = (fw->len - pos) > IR_BLOCK_SIZE ? IR_BLOCK_SIZE : fw->len - pos;

		fprintf (stdout, "@% 5d for % 3d (block % 3d) - % 3d%%", pos, pkt, block, (int) ((pos * 100) / fw->len));
		fflush (stdout);

		i = 0;
		buf[i++] = 0x45 | ((block & 1) << 3);
		if ((fw->len - pos) > pkt) {
			buf[i++] = (block >> 0) & 0xff;
			buf[i++] = (block >> 8) & 0xff;
			block++;
		} else {
			buf[i++] = 0;
			buf[i++] = 0;
		}
		buf[i++] = (pkt >> 0) & 0xff;
		buf[i++] = (pkt >> 8) & 0xff; /* always 0 if IR_BLOCK_SIZE < 256 */

		sum = 0;
		while (pkt--) {
			sum += fw->data[pos];
			buf[i++] = fw->data[pos++];
		}
		buf[i++] = sum & 0xff;

		retries = 10;
		while (retries--) {
			if ((ret = send_to_rcx (b, buf, i)) < 0)
				return ret;
			if ((ret = recv_from_rcx (b, rbuf, 2)) > 0) {
				fprintf (stdout, " OK\n");
				break;
			}
		}
		
		if (ret < 0) {
			fprintf (stdout, " error\n");
			break;
		}
	}

	return ret < 0 ? ret : 0;
}

static int unlock_firmware (brick_t *b) {
	int 		retries = 3;
	uint8_t 	buf[32];
	int		ret;

	while (retries--) {
		int retries = 3;
		
		fprintf (stdout, "Unlock firmware...\n");

		buf[0] = 0xa5;
		buf[1] = 76;
		buf[2] = 69;
		buf[3] = 71;
		buf[4] = 79;
		buf[5] = 174;

		if ((ret = send_to_rcx (b, buf, 6)) < 0)
			return ret;

		while (retries--) {
			if ((ret = recv_from_rcx (b, buf, 26)) > 0) {
				fprintf (stdout, "Firmware unlocked.\n");
				return 0;
			}
		}
		
		fprintf (stdout, "No response.\n");
	}

	return ret;
}

int boot_rcx (brick_t *b, rcx_firmware_t *fw) {
	int ret;

	fprintf (stdout, "Trying to upload firmware to RCX @%08x...\n", b->id);
	
	if ((ret = b->open (b)) == 0) {
		reset_tower (b);

		if ((ret = awake_up_rcx (b)) == 0) {
			fprintf (stdout, "RCX awake; deleting firmware...\n");
			if ((ret = delete_firmware (b)) == 0) {
				fprintf (stdout, "Firmware deleted; uploading firmware...\n");
				if ((ret = upload_firmware (b, fw)) == 0) {
					fprintf (stdout, "Firmware uploaded; unlocking firmware...\n");
					if ((ret = unlock_firmware (b) == 0)) {
						fprintf (stderr, "Booted firmware on RCX.\n");
					}
				}
			}
		} else {
			fprintf (stderr, "Error; RCX not responding (%d).\n", ret);
		}

		b->close (b);
	} else {
		fprintf (stderr, "Error; unable to open brick (%d).\n", ret);
	}

	return ret;
}
