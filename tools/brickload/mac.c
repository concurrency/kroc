/* 
 *  mac.c -- part of brickload NXT/RCX firmware and bytecode tool
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

#ifdef OS_MAC

#include <CoreFoundation/CoreFoundation.h>

#include <IOKit/IOKitLib.h>
#include <IOKit/IOMessage.h>
#include <IOKit/IOCFPlugIn.h>
#include <IOKit/usb/IOUSBLib.h>

#include <mach/mach.h>


typedef struct _usb_handle_t {
	mach_port_t	port;
} usb_handle_t;


void *init_usb (void) {
	usb_handle_t *h = (usb_handle_t *) malloc (sizeof (usb_handle_t));
	kern_return_t kr;

	kr = IOMasterPort (MACH_PORT_NULL, &(h->port));
	if (kr || !h->port) {
		fprintf (stderr, "Unable to make I/O Kit port: %08x\n", kr);
		free (h);
		return NULL;
	}

	return h;
}

static int get_dev_config (brick_t *brick) {
	IOUSBDeviceInterface **dev = (IOUSBDeviceInterface **) brick->handle;
	IOReturn r;
	UInt8 config;

	assert (dev != NULL);
	
	r = (*dev)->GetConfiguration (dev, &config);
	if (r) {
		return -1;
	} else {
		return (int) config;
	}
}

static int set_dev_config (brick_t *brick, int configuration) {
	IOUSBDeviceInterface **dev = (IOUSBDeviceInterface **) brick->handle;
	IOReturn r;
	
	assert (dev != NULL);

	r = (*dev)->USBDeviceOpen (dev);
	if (!r) {
		r = (*dev)->SetConfiguration (dev, (UInt8) configuration);
		(*dev)->USBDeviceClose (dev);
		
		if (!r) {
			return -2;
		}
	} else {
		return -1;
	}

	return 0;
}

static IOUSBDeviceInterface **dev_from_intf (IOUSBInterfaceInterface182 **intf) {
	IOUSBDeviceInterface	**dev 			= NULL;
	IOCFPlugInInterface	**plugInInterface 	= NULL;
	io_service_t		device;
	kern_return_t		kr;
	HRESULT			result;
	SInt32			score;

	kr = (*intf)->GetDevice (intf, &device);

	if (kr || !device) {
		return NULL;
	}

	kr = IOCreatePlugInInterfaceForService (
		device, kIOUSBDeviceUserClientTypeID, kIOCFPlugInInterfaceID,
		&plugInInterface, &score
	);
	
	if (kr || !plugInInterface) {
		return NULL;
	}

	result = (*plugInInterface)->QueryInterface (
			plugInInterface,
			CFUUIDGetUUIDBytes (kIOUSBDeviceInterfaceID),
			(LPVOID *) &dev
	);
	(*plugInInterface)->Release (plugInInterface);

	if (!result) {
		return dev;
	} else {
		return NULL;
	}
}

static int open_intf (brick_t *brick) {
	IOUSBInterfaceInterface182 **intf = (IOUSBInterfaceInterface182 **) brick->handle;
	IOReturn r;

	assert (intf != NULL);

	r = (*intf)->USBInterfaceOpen (intf);
	if (!r) {
		UInt8 n_endpoints = 0;
		int i;
		
		brick->in_ep	= -1;
		brick->out_ep	= -1;

		(*intf)->GetNumEndpoints (intf, &n_endpoints);
		for (i = 0; i < n_endpoints; ++i) {
			UInt16 	maxPacketSize;
			UInt8 	direction, number, transferType, interval;

			(*intf)->GetPipeProperties (
				intf,
				(UInt8) i,
				&direction, &number, &transferType,
				&maxPacketSize,
				&interval
			);

			switch (direction) {
				case kUSBIn:
					if (brick->in_ep < 0)
						brick->in_ep = i;
					break;
				case kUSBOut:
					if (brick->out_ep < 0)
						brick->out_ep = i;
					break;
			}
		}

		/* defaults */
		if (brick->in_ep < 0)
			brick->in_ep = 2;
		if (brick->out_ep < 0)
			brick->out_ep = 1;

		return 0;
	} else {
		fprintf (stderr, "intf error = %08x\n", r);
		return -1;
	}
}

static int close_intf (brick_t *brick) {
	IOUSBInterfaceInterface182 **intf = (IOUSBInterfaceInterface182 **) brick->handle;
	IOReturn r;
	
	assert (intf != NULL);
	
	r = (*intf)->USBInterfaceClose (intf);
	if (r) {
		fprintf (stderr, "intf error = %08x\n", r);
		return -1;
	} else {
		return 0;
	}
}

static int read_intf (brick_t *brick, uint8_t *data, size_t len, uint32_t timeout) {
	IOUSBInterfaceInterface182	**intf 	= (IOUSBInterfaceInterface182 **) brick->handle;
	UInt32 				size	= len;
	IOReturn 			r;

	assert (intf != NULL);

	if (timeout) {
		r = (*intf)->ReadPipeTO (intf, brick->in_ep, data, &size,
			timeout, /* no data timeout */
			timeout + ((timeout * (len / 1024)) / 1024) /* completion timeout related to data size */
		);
		/* Fallback for interrupt based pipes */
		if (r == kIOReturnBadArgument) {
			r = (*intf)->ReadPipe (intf, brick->in_ep, data, &size);
		} else if (r != kIOReturnSuccess) {
			(*intf)->ClearPipeStall (intf, brick->in_ep);
		}
	} else {
		r = (*intf)->ReadPipe (intf, brick->in_ep, data, &size);
	}

	if (r) {
		fprintf (stderr, "read error = %08x\n", r);
		return -1;
	} else {
		return (int) size;
	}
}

static int write_intf (brick_t *brick, uint8_t *data, size_t len, uint32_t timeout) {
	IOUSBInterfaceInterface182	**intf = (IOUSBInterfaceInterface182 **) brick->handle;
	IOReturn r;

	assert (intf != NULL);

	if (timeout) {
		r = (*intf)->WritePipeTO (intf, brick->out_ep, data, len,
			timeout, /* no data timeout */
			timeout + ((timeout * (len / 1024)) / 1024) /* completion timeout related to data size */
		);
		/* Fallback for interrupt based pipes */
		if (r == kIOReturnBadArgument) {
			r = (*intf)->WritePipe (intf, brick->out_ep, data, len);
		} else if (r != kIOReturnSuccess) {
			(*intf)->ClearPipeStall (intf, brick->out_ep);
		}
	} else {
		r = (*intf)->WritePipe (intf, brick->out_ep, data, len);
	}

	if (r) {
		fprintf (stderr, "write error = %08x\n", r);
		return -1;
	} else {
		return len;
	}
}

static void release_intf (brick_t *brick) {
	IOUSBInterfaceInterface182	**intf	= (IOUSBInterfaceInterface182 **) brick->handle;
	if (intf != NULL) {
		(*intf)->Release (intf);
		brick->handle = NULL;
	}
}

static void release_dev (brick_t *brick) {
	IOUSBDeviceInterface **dev = (IOUSBDeviceInterface **) brick->handle;
	if (dev != NULL) {
		(*dev)->Release (dev);
		brick->handle = NULL;
	}
}

brick_t *find_usb_devices (
		void *usb, 
		int32_t vendor, 	int32_t product,
		int32_t configuration, 	int32_t interface,
		brick_type_t type) {
	usb_handle_t 			*h = (usb_handle_t *) usb;
	CFMutableDictionaryRef 		matching;
	kern_return_t			kr;
	io_iterator_t			devices;
	io_service_t			device;
	brick_t				*bricks 	= NULL;
	int				count;

	if (configuration) {
		matching = IOServiceMatching (kIOUSBInterfaceClassName);
	} else {
		matching = IOServiceMatching (kIOUSBDeviceClassName);
	}
	
	if (!matching) {
		return NULL;
	}

	CFDictionarySetValue (matching, CFSTR (kUSBVendorID),
				CFNumberCreate (kCFAllocatorDefault, 
					kCFNumberSInt32Type, &vendor));
	CFDictionarySetValue (matching, CFSTR (kUSBProductID),
				CFNumberCreate (kCFAllocatorDefault, 
					kCFNumberSInt32Type, &product));
	if (configuration) {
		CFDictionarySetValue (matching, CFSTR (kUSBConfigurationValue),
					CFNumberCreate (kCFAllocatorDefault, 
						kCFNumberSInt32Type, &configuration));
		CFDictionarySetValue (matching, CFSTR (kUSBInterfaceNumber),
					CFNumberCreate (kCFAllocatorDefault, 
						kCFNumberSInt32Type, &interface));
	}
	
	kr = IOServiceGetMatchingServices (h->port, matching, &devices);

	if (kr) {
		fprintf (stderr, "IOService matching error = %08x\n", kr);
		return NULL;
	}
	
	count = 0;
	while ((device = IOIteratorNext (devices)))
		count++;
	
	if (count > 0) {
		int i = 0;

		IOIteratorReset (devices);

		bricks = malloc ((sizeof (brick_t)) * (count + 1));
		memset ((void *) bricks, 0, (sizeof (brick_t )) * (count + 1));
		while ((device = IOIteratorNext (devices))) {
			IOUSBInterfaceInterface182	**intf 			= NULL;
			IOUSBDeviceInterface		**dev 			= NULL;
			IOCFPlugInInterface		**plugInInterface 	= NULL;
			brick_t				*b			= &(bricks[i]);
			HRESULT				result;
			SInt32				score;

			if (configuration) {
				kr = IOCreatePlugInInterfaceForService (
					device, kIOUSBInterfaceUserClientTypeID, kIOCFPlugInInterfaceID,
					&plugInInterface, &score
				);
			} else {
				kr = IOCreatePlugInInterfaceForService (
					device, kIOUSBDeviceUserClientTypeID, kIOCFPlugInInterfaceID,
					&plugInInterface, &score
				);
			}

			if (kr || !plugInInterface) {
				continue;
			}

			if (configuration) {
				result = (*plugInInterface)->QueryInterface (
						plugInInterface,
						CFUUIDGetUUIDBytes (kIOUSBInterfaceInterfaceID182),
						(LPVOID *) &intf
				);
			} else {
				result = (*plugInInterface)->QueryInterface (
						plugInInterface,
						CFUUIDGetUUIDBytes (kIOUSBDeviceInterfaceID),
						(LPVOID *) &dev
				);
			}
				
			(*plugInInterface)->Release (plugInInterface);
			
			if (result || !(dev || intf)) {
				continue;
			}

			b->type			= type;
			if (configuration) {
				b->handle	= intf;
				b->open		= open_intf;
				b->close	= close_intf;
				b->read		= read_intf;
				b->write	= write_intf;
				b->release 	= release_intf;

				(*intf)->GetLocationID (intf, &(b->id));
			} else {
				b->handle	= dev;
				b->get_config	= get_dev_config;
				b->set_config	= set_dev_config;
				b->release 	= release_dev;
				
				(*dev)->GetLocationID (dev, &(b->id));
			}	
			i			= i + 1;
		}
		bricks[i].type = NULL_BRICK;
	}

	return bricks;
}

void free_usb (void *usb) {
	usb_handle_t *h = (usb_handle_t *) usb;
	mach_port_deallocate (mach_task_self (), h->port);
	free (h);
}

#endif /* OS_MAC */
