/*
 * native.m
 * part of the occPlug plugin for the jEdit text editor
 * Copyright (C) 2010 Christian L. Jacobsen
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

#include <sys/types.h>

#include <Foundation/Foundation.h>
#include <CoreFoundation/CoreFoundation.h>

#include <IOKit/IOKitLib.h>
#include <IOKit/usb/IOUSBLib.h>
#include <IOKit/serial/IOSerialKeys.h>

#include "org_transterpreter_occPlug_hosts_Native.h"

static void JNU_ThrowByName(JNIEnv *env, const char *name, const char *msg)
{
	jclass cls = (*env)->FindClass(env, name);
	/* if cls is NULL, an exception has already been thrown */
	if (cls != NULL) {
		(*env)->ThrowNew(env, cls, msg);
	}
	/* free the local ref */
	(*env)->DeleteLocalRef(env, cls);
}

static bool find_devices(io_iterator_t *it)
{
	kern_return_t           kernResult;
	CFMutableDictionaryRef  classesToMatch;

	classesToMatch = IOServiceMatching(kIOUSBDeviceClassName);
	if (classesToMatch == NULL)
	{
		NSLog(@"native: IOServiceMatching returned null");
		return false;
	}

	kernResult = IOServiceGetMatchingServices(kIOMasterPortDefault, classesToMatch, it);
	if (KERN_SUCCESS != kernResult)
	{
		NSLog(@"native: IOServiceGetMatchingServices failed, returning %d", kernResult);
		return false;
	}

	return true;
}

static bool get_int32_property(io_object_t service, CFStringRef property, int32_t *number, JNIEnv *env, const char *exception_text)
{
	CFNumberRef cf_num_prop;

	cf_num_prop = (CFNumberRef) IORegistryEntryCreateCFProperty(service, property, kCFAllocatorDefault, 0);
	if (cf_num_prop && (CFGetTypeID(cf_num_prop) !=  CFNumberGetTypeID()))
	{
		JNU_ThrowByName(env, "java/lang/RuntimeException", exception_text);
		return false;
	}
	CFNumberGetValue(cf_num_prop, kCFNumberSInt32Type, number);
	CFRelease(cf_num_prop);

	return true;
}

static char *get_string_property(io_object_t service, CFStringRef property, JNIEnv *env, const char *exception_text)
{
	CFStringRef cf_str_prop;
	char *str_ptr = NULL;

	cf_str_prop = (CFStringRef) IORegistryEntryCreateCFProperty(service, property, kCFAllocatorDefault, 0);
	if (cf_str_prop && (CFGetTypeID(cf_str_prop) !=  CFStringGetTypeID()))
	{
		JNU_ThrowByName(env, "java/lang/RuntimeException", exception_text);
		return NULL;
	}
	int cf_str_len = CFStringGetLength(cf_str_prop) * 2 + 1; /* * 2 as CFStringGetLength returns # of UTF-16 code paris */
	str_ptr = malloc(cf_str_len);
	if(str_ptr == NULL)
	{
		CFRelease(cf_str_prop);
		JNU_ThrowByName(env, "java/lang/RuntimeException", exception_text);
		return NULL;
	}
	if(!CFStringGetCString(cf_str_prop, str_ptr, cf_str_len, kCFStringEncodingUTF8))
	{
		/* FIXME: make it possible to differentiate between these
		 * exceptions */
		CFRelease(cf_str_prop);
		JNU_ThrowByName(env, "java/lang/RuntimeException", exception_text);
		return NULL;
	}
	CFRelease(cf_str_prop);

	return str_ptr;
}

JNIEXPORT jobject JNICALL Java_org_transterpreter_occPlug_hosts_Native__1getSerialPorts (JNIEnv *env, jclass this)
{
	jclass      dict_class;
	jmethodID   dict_init;
	jmethodID   dict_put;
	jclass      integer_class;
	jmethodID   integer_init;
	jobject     result = NULL;

	dict_class = (*env)->FindClass(env, "java/util/HashMap");
	if (dict_class == NULL) {
		goto error; /* exception thrown */
	}
	dict_init = (*env)->GetMethodID(env, dict_class, "<init>", "()V");
	if (dict_init == NULL) {
		goto error; /* exception thrown */
	}
	dict_put = (*env)->GetMethodID(env, dict_class, "put", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
	if (dict_put == NULL) {
		goto error; /* exception thrown */
	}
	integer_class = (*env)->FindClass(env, "java/lang/Integer");
	if (integer_class == NULL) {
		goto error; /* exception thrown */
	}
	integer_init = (*env)->GetMethodID(env, integer_class, "<init>", "(I)V");
	if (integer_init == NULL) {
		goto error; /* exception thrown */
	}

	io_iterator_t iterator;
	if(!find_devices(&iterator))
	{
		JNU_ThrowByName(env, "java/lang/RuntimeException", "native: find_devices failed");
		goto error;
	}

	if((result = (*env)->NewObject(env, dict_class, dict_init)) == NULL)
	{
		goto error; /* exception thrown */
	}

	io_object_t service;
	while ((service = IOIteratorNext(iterator)))
	{
		int32_t     vendor;
		int32_t     product;
		CFStringRef cf_bsd_path;

		if(!get_int32_property(service, CFSTR(kUSBVendorID), &vendor, env, "native: vendor is not a number"))
		{
			goto loop_error; /* exception thrown */
		}

		if(!get_int32_property(service, CFSTR(kUSBProductID), &product, env, "native: vendor is not a number"))
		{
			goto loop_error; /* exception thrown */
		}

		/* FIXME: Make this more flexible, make it so something can be
		 * passed from Java */
		if(vendor != 0x2341 && !(vendor == 0x0403 && product == 0x6001))
		{
			/* FIXME: This returns a result */
			IOObjectRelease(service);
			continue;
		}

		cf_bsd_path = IORegistryEntrySearchCFProperty(service, kIOServicePlane, CFSTR(kIODialinDeviceKey), NULL, kIORegistryIterateRecursively);
		if(cf_bsd_path == NULL) 
		{
			NSLog(@"Did not find a bsd path for a device that probably ought to have one?");
			IOObjectRelease(service);
			continue;
		}

		char *str;
		jstring j_name;
		jobject j_obj;
		jobject entry_dict;
		if((entry_dict = (*env)->NewObject(env, dict_class, dict_init)) == NULL)
		{
			goto loop_error; /* exception thrown */
		}

#define make_str(str) \
		if((j_obj = (*env)->NewStringUTF(env, str)) == NULL) \
		{ \
			goto loop_error; /* exception thrown */ \
		}; \
		free(str);
#define make_int(integer) \
		if((j_obj = (*env)->NewObject(env, integer_class, integer_init, integer)) == NULL) \
		{ \
			goto loop_error; /* exception thrown */ \
		}
#define make_name(name) \
		if((j_name = (*env)->NewStringUTF(env, name)) == NULL) \
		{ \
			goto loop_error; /* exception thrown */ \
		}
#define put_entry() \
		(*env)->CallObjectMethod(env, entry_dict, dict_put, j_name, j_obj); \
		if ((*env)->ExceptionCheck(env)) \
		{ \
			goto loop_error; /* exception thrown */ \
		}

		/********************************************
		 * Vendor ID
		 ********************************************/
		make_int(vendor);
		make_name(kUSBVendorID);
		put_entry();


		/********************************************
		 * Product ID
		 ********************************************/
		make_int(product);
		make_name(kUSBProductID);
		put_entry();


		/********************************************
		 * Vendor Name
		 ********************************************/
		if((str = get_string_property(service, CFSTR(kUSBVendorString), env, "vendor name")) == NULL)
		{
			goto loop_error; /* exception thrown */
		}
		make_str(str);
		make_name(kUSBVendorString);
		put_entry();


		/********************************************
		 * Product Name
		 ********************************************/
		if((str = get_string_property(service, CFSTR(kUSBProductString), env, "product name")) == NULL)
		{
			goto loop_error; /* exception thrown */
		}
		make_str(str);
		make_name(kUSBProductString);
		put_entry();

		/********************************************
		 * Product Serial Number
		 ********************************************/
		if((str = get_string_property(service, CFSTR(kUSBSerialNumberString), env, "serial number")) == NULL)
		{
			goto loop_error; /* exception thrown */
		}
		make_str(str);
		make_name(kUSBSerialNumberString);
		put_entry();


		/********************************************
		 * BSD path and adding to result dictionary 
		 ********************************************/
		int cf_bsd_path_len = CFStringGetLength(cf_bsd_path) * 2 + 1; /* * 2 as CFStringGetLength returns # of UTF-16 code paris */
		char c_bsd_path[cf_bsd_path_len];
		if(!CFStringGetCString((CFStringRef) cf_bsd_path, c_bsd_path, cf_bsd_path_len, kCFStringEncodingUTF8))
		{
			JNU_ThrowByName(env, "java/lang/RuntimeException", "native: could not convert bsd_path_string to c string");
			goto loop_error;
		}
		CFRelease(cf_bsd_path);

		jstring j_bsd_path;
		if((j_bsd_path = (*env)->NewStringUTF(env, c_bsd_path)) == NULL)
		{
			goto loop_error; /* exception thrown */
		}
		(*env)->CallObjectMethod(env, result, dict_put, j_bsd_path, entry_dict);
		if((*env)->ExceptionCheck(env))
		{
			goto loop_error; /* exception thrown */
		}

		/********************************************
		 * Done
		 ********************************************/

		IOObjectRelease(service);
	}

	/* FIXME: Do I need to release the iterator? */

	return result;
loop_error:
	IOObjectRelease(service);
error:
	return NULL;
}
