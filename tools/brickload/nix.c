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
							found = 1;
						}
					}
				}

				if (found) {
					h 			= (usb_intf_t *) malloc (sizeof (usb_intf_t));
					h->dev			= dp;
					h->configuration	= configuration;
					h->interface		= interface;

					b->type			= type;
					b->handle		= h;
					b->release		= release_intf;
					
					count++;
				}
			}
		}
	}

	return list;
}

#endif /* OS_NIX */
