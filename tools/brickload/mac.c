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

static void release_intf_brick (brick_t *brick) {
	IOUSBInterfaceInterface	**intf = (IOUSBInterfaceInterface **) brick->handle;
	if (intf != NULL) {
		(*intf)->Release (intf);
		brick->handle = NULL;
	}
}

static void release_dev_brick (brick_t *brick) {
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
	while (device = IOIteratorNext (devices))
		count++;
	
	if (count > 0) {
		int i = 0;

		IOIteratorReset (devices);

		bricks = malloc ((sizeof (brick_t)) * (count + 1));
		memset ((void *) bricks, 0, (sizeof (brick_t )) * (count + 1));
		while (device = IOIteratorNext (devices)) {
			IOUSBInterfaceInterface	**intf 			= NULL;
			IOUSBDeviceInterface	**dev 			= NULL;
			IOCFPlugInInterface	**plugInInterface 	= NULL;
			HRESULT			result;
			SInt32			score;

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
						CFUUIDGetUUIDBytes (kIOUSBInterfaceInterfaceID),
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

			bricks[i].type			= type;
			if (configuration) {
				bricks[i].handle	= intf;
				bricks[i].release 	= release_intf_brick;
			} else {
				bricks[i].handle	= dev;
				bricks[i].release 	= release_dev_brick;
			}	
			i				= i + 1;
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
