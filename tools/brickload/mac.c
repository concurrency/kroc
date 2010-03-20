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

static void release_brick (brick_t *brick) {
}

brick_t *find_usb_devices (void *usb, int32_t vendor, int32_t product, brick_type_t type) {
	usb_handle_t 			*h = (usb_handle_t *) usb;
	CFMutableDictionaryRef 		matching;
	kern_return_t			kr;
	io_iterator_t			devices;
	io_service_t			device;
	brick_t				*bricks 	= NULL;
	int				count;

	matching = IOServiceMatching (kIOUSBDeviceClassName);
	if (!matching) {
		return NULL;
	}

	CFDictionarySetValue (matching, CFSTR (kUSBVendorName),
				CFNumberCreate (kCFAllocatorDefault, 
					kCFNumberSInt32Type, &vendor));
	CFDictionarySetValue (matching, CFSTR (kUSBProductName),
				CFNumberCreate (kCFAllocatorDefault, 
					kCFNumberSInt32Type, &product));
	
	kr = IOServiceGetMatchingServices (h->port, matching, &devices);

	if (kr) {
		fprintf (stderr, "IOService matching error = %08x\n", kr);
		/* FIXME: free matching */
		return NULL;
	}
	
	count = 0;
	while (device = IOIteratorNext (devices))
		count++;
	
	if (count > 0) {
		int i = 0;

		IOIteratorReset (devices);

		bricks = malloc ((sizeof (brick_t)) * (count + 1));
		while (device = IOIteratorNext (devices)) {
			IOCFPlugInInterface	**plugInInterface 	= NULL;
			IOUSBDeviceInterface	**dev 			= NULL;
			HRESULT			result;
			SInt32			score;

			device 			= IOIteratorNext (devices);
			kr			= IOCreatePlugInInterfaceForService (
				device, kIOUSBDeviceUserClientTypeID, kIOCFPlugInInterfaceID,
				&plugInInterface, &score
			);

			if (!plugInInterface) {
				fprintf (stderr, "X1 %08x\n", kr);
				continue;
			}

			result = (*plugInInterface)->QueryInterface (
					plugInInterface,
					CFUUIDGetUUIDBytes (kIOUSBDeviceInterfaceID),
					(LPVOID *) &dev
			);
			(*plugInInterface)->Release (plugInInterface);
			
			if (result || !dev) {
				fprintf (stderr, "X2\n");
				continue;
			}

			bricks[i].type			= type;
			bricks[i].handle		= dev;
			bricks[i].release 		= release_brick;
			i				= i + 1;

			(*dev)->GetDeviceVendor (dev, &(bricks[i].vendor));
			(*dev)->GetDeviceProduct (dev, &(bricks[i].product));
		}
		bricks[i].type = NULL_BRICK;
	}

	/* FIXME: free matching */
	return bricks;
}

void free_usb (void *usb) {
	usb_handle_t *h = (usb_handle_t *) usb;
	mach_port_deallocate (mach_task_self (), h->port);
	free (h);
}

#endif /* OS_MAC */
