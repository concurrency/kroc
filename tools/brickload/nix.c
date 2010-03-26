/* 
 *  nix.c -- part of brickload NXT/RCX firmware and bytecode tool
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

#ifdef OS_NIX

#ifdef HAVE_USB_H 
#include <usb.h>
#endif

typedef struct _usb_intf_t {
	struct usb_device 	*dev;
	int 			configuration;
	int			interface;
	int			ep_type;
} usb_intf_t;

static int init_count = 0;

void *init_usb (void) {
	if (init_count == 0) {
		usb_init ();
		usb_find_busses ();
		usb_find_devices ();
	}
	init_count++;
	return &init_count;
}

void free_usb (void *usb) {
	return;
}

static int get_dev_config (brick_t *brick) {
	return -1;
}

static int set_dev_config (brick_t *brick, int configuration) {
	return -1;
}

static int open_intf (brick_t *brick) {
	return -1;
}

static int close_intf (brick_t *brick) {
	return -1;
}

static int read_intf (brick_t *brick, uint8_t *data, size_t len, uint32_t timeout) {
	return -1;
}

static int write_intf (brick_t *brick, uint8_t *data, size_t len, uint32_t timeout) {
	return -1;
}

static void release_intf (brick_t *b) {
	if (b->handle != NULL) {
		free (b->handle);
		b->handle = NULL;
	}
}

brick_t *find_usb_devices (
		void *usb, 
		int32_t vendor, 	int32_t product,
		int32_t configuration, 	int32_t interface,
		brick_type_t type) {
	struct usb_bus *busses 	= usb_get_busses ();
	struct usb_bus *bp;
	brick_t *list;
	int count;

	count = 0;
	for (bp = busses; bp != NULL; bp = bp->next) {
		struct usb_device *dp;
		for (dp = bp->devices; dp != NULL; dp = dp->next) {
			if (dp->descriptor.idVendor == vendor && dp->descriptor.idProduct == product) {
				if (configuration == 0) {
					count++;
				} else {
					int c;
					for (c = 0; c < dp->descriptor.bNumConfigurations; ++c) {
						if (dp->config[c].bConfigurationValue == configuration
								&& interface >= 0 
								&& interface <= dp->config[c].bNumInterfaces) {
							count++;
							break;
						}
					}
				}
			}
		}
	}

	if (count == 0) {
		return NULL;
	}

	list = malloc (sizeof (brick_t) * (count + 1));
	memset (list, 0, sizeof (brick_t) * (count + 1));
	
	count = 0;
	for (bp = busses; bp != NULL; bp = bp->next) {
		struct usb_device *dp;
		for (dp = bp->devices; dp != NULL; dp = dp->next) {
			if (dp->descriptor.idVendor == vendor && dp->descriptor.idProduct == product) {
				struct usb_endpoint_descriptor *in_ep = NULL, *out_ep = NULL;
				usb_intf_t *h 	= NULL;
				brick_t *b 	= &(list[count]);
				int found	= 0;

				if (configuration == 0) {
					found = 1;
				} else {
					int c;
					for (c = 0; c < dp->descriptor.bNumConfigurations; ++c) {
						if (dp->config[c].bConfigurationValue == configuration
								&& interface >= 0 
								&& interface <= dp->config[c].bNumInterfaces) {
							struct usb_interface_descriptor *intf = 
								dp->config[c].interface->altsetting;
							int i;
							for (i = 0; i < intf->bNumEndpoints; ++i) {
								if (intf->endpoint[i].bEndpointAddress & USB_ENDPOINT_DIR_MASK) {
									if (out_ep == NULL)
										out_ep = &(intf->endpoint[i]);
								} else {
									if (in_ep == NULL)
										in_ep = &(intf->endpoint[i]);
								}
							}
							found = 1;
						}
					}
				}

				if (found) {
					h 			= (usb_intf_t *) malloc (sizeof (usb_intf_t));
					h->dev			= dp;
					h->configuration	= configuration;
					h->interface		= interface;
					if (in_ep != NULL)
						h->ep_type	= in_ep->bmAttributes & USB_ENDPOINT_TYPE_MASK;

					b->id			= (count + 1); /* FIXME: do something better */
					b->type			= type;
					if (in_ep != NULL)
						b->in_ep	= in_ep->bEndpointAddress;
					if (out_ep != NULL)
						b->out_ep	= out_ep->bEndpointAddress;
					b->handle		= h;
					b->get_config		= get_dev_config;
					b->set_config		= set_dev_config;
					b->open			= open_intf;
					b->close		= close_intf;
					b->read			= read_intf;
					b->write		= write_intf;
					b->release		= release_intf;
					
					count++;
				}
			}
		}
	}

	return list;
}

#endif /* OS_NIX */
