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

#ifdef USB_API_1_0

#if defined(HAVE_LIBUSB_H) 
#include <libusb.h>
#elif defined(HAVE_LIBUSB_LIBUSB_H) 
#include <libusb/libusb.h>
#elif defined(HAVE_LIBUSB_1_0_LIBUSB_H) 
#include <libusb-1.0/libusb.h>
#else
#error "No libusb.h available."
#endif

typedef struct _usb_intf_t {
	libusb_device 		*dev;
	libusb_device_handle	*dev_h;
	int 			configuration;
	int			interface;
} usb_intf_t;

void *init_usb (void) {
	libusb_context *usb_c = NULL;
	if (libusb_init (&usb_c) == 0) {
		return usb_c;
	} else {
		return NULL;
	}
}

void free_usb (void *usb) {
	libusb_exit ((libusb_context *) usb);
}

static int get_dev_config (brick_t *brick) {
	usb_intf_t *intf = (usb_intf_t *) brick->handle;
	int ret = -1;

	if ((ret = libusb_open (intf->dev, &(intf->dev_h))) == 0) {
		int configuration;
		
		ret = libusb_get_configuration (intf->dev_h, &configuration);
		
		libusb_close (intf->dev_h);
		intf->dev_h = NULL;

		if (ret == 0) {
			return configuration;
		}
	}

	fprintf (stderr, "intf error = %d\n", ret);

	return -1;
}

static int set_dev_config (brick_t *brick, int configuration) {
	usb_intf_t *intf = (usb_intf_t *) brick->handle;
	int ret;

	if ((ret = libusb_open (intf->dev, &(intf->dev_h))) == 0) {
		ret = libusb_set_configuration (intf->dev_h, configuration);
		
		libusb_close (intf->dev_h);
		intf->dev_h = NULL;
		
		if (ret == 0) {
			return 0;
		}
	}
	
	fprintf (stderr, "intf error = %d\n", ret);

	return -1;
}

static int open_intf (brick_t *brick) {
	usb_intf_t *intf = (usb_intf_t *) brick->handle;
	int ret;

	/* can't open interface on device */
	if (intf->configuration == 0)
		return -1;

	if ((ret = libusb_open (intf->dev, &(intf->dev_h))) == 0) {
		if ((ret = libusb_claim_interface (intf->dev_h, intf->interface)) == 0) {
			return 0;
		}
		libusb_close (intf->dev_h);
		intf->dev_h = NULL;
	}
	
	fprintf (stderr, "intf error = %d\n", ret);
	return -1;
}

static int close_intf (brick_t *brick) {
	usb_intf_t *intf = (usb_intf_t *) brick->handle;
	
	if (intf->dev_h != NULL) {
		libusb_release_interface (intf->dev_h, intf->interface);
		libusb_close (intf->dev_h);
		intf->dev_h = NULL;
		return 0;
	} else {
		return -1;
	}
}

static int control_msg (brick_t *brick, int req_type, int req, int value, int index, uint8_t *data, size_t len, uint32_t timeout) {
	usb_intf_t *intf = (usb_intf_t *) brick->handle;
	int ret;
	
	if (intf->dev_h == NULL)
		return -1;
	
	if ((ret = libusb_control_transfer (intf->dev_h, req_type, req, value, index, (unsigned char *) data, len, timeout)) >= 0) {
		return ret;
	} else {
		fprintf (stderr, "control error = %d\n", ret);
		return -1;
	}
}

static int read_intf (brick_t *brick, uint8_t *data, size_t len, uint32_t timeout) {
	usb_intf_t *intf = (usb_intf_t *) brick->handle;
	int ret = LIBUSB_ERROR_PIPE, transferred = 0;
	
	if (intf->dev_h == NULL)
		return -1;

	switch (brick->ep_type) {
		case LIBUSB_TRANSFER_TYPE_BULK:
			ret = libusb_bulk_transfer (intf->dev_h, brick->in_ep, (unsigned char *) data, len, &transferred, timeout); 
			if (ret == LIBUSB_ERROR_TIMEOUT || ret == LIBUSB_ERROR_PIPE)
				libusb_clear_halt (intf->dev_h, brick->in_ep);
			break;
		
		case LIBUSB_TRANSFER_TYPE_INTERRUPT:
			ret = libusb_interrupt_transfer (intf->dev_h, brick->in_ep, (unsigned char *) data, len, &transferred, timeout); 
			if (ret == LIBUSB_ERROR_TIMEOUT || ret == LIBUSB_ERROR_PIPE)
				libusb_clear_halt (intf->dev_h, brick->in_ep);
			break;
		
		default:
			break;
	}
	
	if (ret < 0) {
		fprintf (stderr, "read error = %d\n", ret);
		return -1;
	} else {
		return transferred;
	}
}

static int write_intf (brick_t *brick, const uint8_t *data, size_t len, uint32_t timeout) {
	usb_intf_t *intf = (usb_intf_t *) brick->handle;
	int ret = LIBUSB_ERROR_PIPE, transferred = 0;
	
	if (intf->dev_h == NULL)
		return -1;

	switch (brick->ep_type) {
		case LIBUSB_TRANSFER_TYPE_BULK:
			ret = libusb_bulk_transfer (intf->dev_h, brick->out_ep, (unsigned char *) data, len, &transferred, timeout); 
			if (ret == LIBUSB_ERROR_TIMEOUT || ret == LIBUSB_ERROR_PIPE)
				libusb_clear_halt (intf->dev_h, brick->out_ep);
			break;
		
		case LIBUSB_TRANSFER_TYPE_INTERRUPT:
			ret = libusb_interrupt_transfer (intf->dev_h, brick->out_ep, (unsigned char *) data, len, &transferred, timeout); 
			break;
		
		default:
			break;
	}
	
	if (ret < 0) {
		fprintf (stderr, "write error = %d\n", ret);
		return -1;
	} else {
		return transferred;
	}
}

static void release_intf (brick_t *b) {
	if (b->handle != NULL) {
		usb_intf_t *intf = (usb_intf_t *) b->handle;
		
		libusb_unref_device (intf->dev);
		if (intf->dev_h != NULL)
			libusb_close (intf->dev_h);
		free (intf);

		b->handle = NULL;
	}
}

brick_t *find_usb_devices (
		void *usb, 
		int32_t vendor, 	int32_t product,
		int32_t configuration, 	int32_t interface,
		brick_type_t type) {
	libusb_context 	*usb_c		= (libusb_context *) usb;
	libusb_device 	**devices	= NULL;
	brick_t 	*list;
	int i, count, n_devices;
	
	n_devices = libusb_get_device_list (usb_c, &devices);
	
	count = 0;
	for (i = 0; i < n_devices; ++i) {
		struct libusb_device_descriptor desc;

		libusb_get_device_descriptor (devices[i], &desc);

		if (desc.idVendor == vendor && desc.idProduct == product) {
			if (configuration == 0) {
				count++;
			} else {
				struct libusb_config_descriptor *cfg_desc;
				
				if (libusb_get_config_descriptor_by_value (devices[i], configuration, &cfg_desc) == 0) {
					if (interface >= 0 && interface <= cfg_desc->bNumInterfaces) {
						count++;
					}
					libusb_free_config_descriptor (cfg_desc);
				}
			}
		}
	}
	
	if (count == 0) {
		libusb_free_device_list (devices, 1);
		return NULL;
	}

	list = malloc (sizeof (brick_t) * (count + 1));
	memset (list, 0, sizeof (brick_t) * (count + 1));
	
	count = 0;
	for (i = 0; i < n_devices; ++i) {
		struct libusb_device_descriptor desc;
		brick_t *b = &(list[count]);
		int ep_type = -1, in_ep = -1, out_ep = -1;
		int found = 0;
		
		libusb_get_device_descriptor (devices[i], &desc);
		
		if (desc.idVendor == vendor && desc.idProduct == product) {
			if (configuration == 0) {
				found = 1;
			} else {
				struct libusb_config_descriptor *cfg_desc;
				
				if (libusb_get_config_descriptor_by_value (devices[i], configuration, &cfg_desc) == 0) {
					if (interface >= 0 && interface <= cfg_desc->bNumInterfaces) {
						const struct libusb_interface_descriptor *intf = 
							cfg_desc->interface[interface].altsetting;
						int j;

						for (j = 0; j < intf->bNumEndpoints; ++j) {
							if ((intf->endpoint[j].bEndpointAddress & LIBUSB_ENDPOINT_DIR_MASK) == LIBUSB_ENDPOINT_IN) {
								if (in_ep < 0) {
									in_ep = intf->endpoint[j].bEndpointAddress;
									ep_type = intf->endpoint[j].bmAttributes & LIBUSB_TRANSFER_TYPE_MASK;
								}
							} else {
								if (out_ep < 0) {
									out_ep = intf->endpoint[j].bEndpointAddress;
									ep_type = intf->endpoint[j].bmAttributes & LIBUSB_TRANSFER_TYPE_MASK;
								}
							}
						}
						
						found = 1;
					}
					
					libusb_free_config_descriptor (cfg_desc);
				}
			}
		}

		if (found) {
			usb_intf_t *h 		= (usb_intf_t *) malloc (sizeof (usb_intf_t));

			h->dev			= libusb_ref_device (devices[i]);
			h->dev_h		= NULL;
			h->configuration	= configuration;
			h->interface		= interface;

			b->id			= (count + 1); /* FIXME: do something better */
			b->type			= type;
			b->in_ep		= in_ep;
			b->out_ep		= out_ep;
			b->ep_type		= ep_type;
			b->handle		= h;
			b->get_config		= get_dev_config;
			b->set_config		= set_dev_config;
			b->open			= open_intf;
			b->close		= close_intf;
			b->control		= control_msg;
			b->read			= read_intf;
			b->write		= write_intf;
			b->release		= release_intf;
			
			count++;
		}
	}
	list[count].type = NULL_BRICK;
	libusb_free_device_list (devices, 1);

	return list;
}

#else /* USB_API_0_1 */

#ifdef HAVE_USB_H 
#include <usb.h>
#else
#error "No usb.h available."
#endif

typedef struct _usb_intf_t {
	struct usb_device 	*dev;
	usb_dev_handle		*dev_h;
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

static int get_dev_config (brick_t *brick) {
	usb_intf_t 	*intf	= (usb_intf_t *) brick->handle;
	usb_dev_handle 	*dev 	= usb_open (intf->dev);
	
	if (dev != NULL) {
		uint8_t buf[2];
		int ret;

		ret = usb_control_msg (dev, 
			USB_TYPE_STANDARD | USB_RECIP_DEVICE | USB_ENDPOINT_IN,
			USB_REQ_GET_CONFIGURATION,
			0,
			0,
			(char *) buf,
			1,
			0
		);

		if (ret == 1) {
			ret = buf[0];
		} else {
			ret = -1;
		}

		usb_close (dev);
		return ret;
	} else {
		fprintf (stderr, "intf error = %d\n", errno);
		return -1;
	}
}

static int set_dev_config (brick_t *brick, int configuration) {
	usb_intf_t 	*intf	= (usb_intf_t *) brick->handle;
	usb_dev_handle 	*dev 	= usb_open (intf->dev);
	
	if (dev != NULL) {
		int ret = usb_set_configuration (dev, configuration);
		usb_close (dev);
		return ret;
	} else {
		fprintf (stderr, "intf error = %d (%s)\n", errno, strerror (errno));
		return -1;
	}
}

static int open_intf (brick_t *brick) {
	usb_intf_t *intf = (usb_intf_t *) brick->handle;

	/* can't open interface on device */
	if (intf->configuration == 0)
		return -1;

	if (intf->dev_h == NULL) {
		intf->dev_h = usb_open (intf->dev);
	}
	
	if (intf->dev_h != NULL) {
		return 0;
	} else {
		fprintf (stderr, "intf error = %d (%s)\n", errno, strerror (errno));
		return -1;
	}
}

static int close_intf (brick_t *brick) {
	usb_intf_t *intf = (usb_intf_t *) brick->handle;
	
	if (intf->dev_h != NULL) {
		usb_close (intf->dev_h);
		intf->dev_h = NULL;
		return 0;
	} else {
		return -1;
	}
}

static int control_msg (brick_t *brick, int req_type, int req, int value, int index, uint8_t *data, size_t len, uint32_t timeout) {
	usb_intf_t *intf = (usb_intf_t *) brick->handle;
	int ret;
	
	if (intf->dev_h == NULL)
		return -1;
	
	if ((ret = usb_control_msg (intf->dev_h, req_type, req, value, index, (char *) data, len, timeout)) >= 0) {
		return ret;
	} else {
		fprintf (stderr, "control error = %d (%s)\n", errno, strerror (errno));
		return -1;
	}
}

static int read_intf (brick_t *brick, uint8_t *data, size_t len, uint32_t timeout) {
	usb_intf_t *intf = (usb_intf_t *) brick->handle;
	int ret = -1;
	
	if (intf->dev_h == NULL)
		return -1;

	switch (brick->ep_type) {
		case USB_ENDPOINT_TYPE_BULK:
			ret = usb_bulk_read (intf->dev_h, brick->in_ep, (char *) data, len, timeout); 
			if (ret == (-ETIMEDOUT)) 
				usb_clear_halt (intf->dev_h, brick->in_ep);
			break;
		
		case USB_ENDPOINT_TYPE_INTERRUPT:
			ret = usb_interrupt_read (intf->dev_h, brick->in_ep, (char *) data, len, timeout); 
			if (ret == (-ETIMEDOUT)) 
				usb_clear_halt (intf->dev_h, brick->in_ep);
			break;
		
		default:
			break;
	}
	
	if (ret < 0) {
		fprintf (stderr, "read error = %d (%s)\n", errno, strerror (errno));
		return -1;
	} else {
		return ret;
	}
}

static int write_intf (brick_t *brick, const uint8_t *data, size_t len, uint32_t timeout) {
	usb_intf_t *intf = (usb_intf_t *) brick->handle;
	int ret = -1;
	
	if (intf->dev_h == NULL)
		return -1;

	switch (brick->ep_type) {
		case USB_ENDPOINT_TYPE_BULK:
			ret = usb_bulk_write (intf->dev_h, brick->out_ep, (char *) data, len, timeout);
			if (ret == (-ETIMEDOUT)) 
				usb_clear_halt (intf->dev_h, brick->out_ep);
			break;
		
		case USB_ENDPOINT_TYPE_INTERRUPT:
			ret = usb_interrupt_write (intf->dev_h, brick->out_ep, (char *) data, len, timeout); 
			break;
		
		default:
			break;
	}
	
	if (ret < 0) {
		fprintf (stderr, "write error = %d (%s)\n", errno, strerror (errno));
		return -1;
	} else {
		return ret;
	}
}

static void release_intf (brick_t *b) {
	if (b->handle != NULL) {
		usb_intf_t *intf = (usb_intf_t *) b->handle;

		if (intf->dev_h != NULL)
			usb_close (intf->dev_h);
		free (intf);

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
								dp->config[c].interface[interface].altsetting;
							int i;
							for (i = 0; i < intf->bNumEndpoints; ++i) {
								if ((intf->endpoint[i].bEndpointAddress & USB_ENDPOINT_DIR_MASK) == USB_ENDPOINT_IN) {
									if (in_ep == NULL)
										in_ep = &(intf->endpoint[i]);
								} else {
									if (out_ep == NULL)
										out_ep = &(intf->endpoint[i]);
								}
							}
							found = 1;
						}
					}
				}

				if (found) {
					h 			= (usb_intf_t *) malloc (sizeof (usb_intf_t));
					h->dev			= dp;
					h->dev_h		= NULL;
					h->configuration	= configuration;
					h->interface		= interface;
					if (in_ep != NULL)
						b->ep_type	= in_ep->bmAttributes & USB_ENDPOINT_TYPE_MASK;
					else
						b->ep_type	= USB_ENDPOINT_TYPE_BULK; 

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
					b->control		= control_msg;
					b->read			= read_intf;
					b->write		= write_intf;
					b->release		= release_intf;
					
					count++;
				}
			}
		}
	}
	list[count].type = NULL_BRICK;

	return list;
}

#endif /* USB_API_0_1 */

#endif /* OS_NIX */
