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

		fprintf (stderr, "ret = %d\n", ret);

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
	int i, ret;

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

		for (i = 0; i < ret; ++i)
			fprintf (stderr, "byte %i = %02x\n", pos + i, buf[pos + i]);
		
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

void ping_rcx (brick_t *b) {
	int ret;

	if ((ret = b->open (b)) == 0) {
		uint8_t buf[8];
		
		fprintf (stderr, "brick open\n");
		ret = reset_tower (b);
		fprintf (stderr, "tower reset = %d\n", ret);

		fprintf (stderr, "sending ping...\n");
		buf[0] = 0x10;
		ret = send_to_rcx (b, buf, 1);
		fprintf (stderr, "sent ping = %d\n", ret);
		
		fprintf (stderr, "receive response...\n");
		ret = recv_from_rcx (b, buf, 1);
		fprintf (stderr, "recv = %d\n", ret);

		b->close (b);
	} else {
		fprintf (stderr, "brick open failed = %d\n", ret);
	}
}

