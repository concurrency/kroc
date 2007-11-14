/*
 *	ciftrace.h -- tracing macros for CIF applications
 *	Copyright (C) 2005-2006 Adam Sampson <ats@offog.org>
 *	Computing Laboratory, University of Kent, Canterbury, UK
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *	MA 02110-1301, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include "cifccsp.h"

static int ciftrace_enabled = -1;

static void ciftrace_startup ()
{
	ciftrace_enabled = (getenv("CIFTRACE_ENABLED") != NULL);
}

#define XTRACE(format, args...) do { \
		if (ciftrace_enabled == -1) { \
			ciftrace_startup (); \
		} \
		if (ciftrace_enabled) { \
			fprintf (stderr, format, ##args); \
		} \
	} while (0)
#define CTRACE(format, args...) do { \
		if (ciftrace_enabled == -1) { \
			ciftrace_startup (); \
		} \
		if (ciftrace_enabled) { \
			EXTERNAL_CALLN (fprintf, stderr, CIFTRACE_NAME ": %s:%5d: " format, __FILE__, __LINE__, ##args); \
		} \
	} while (0)
#define CFATAL(format, args...) do { \
		ProcAfter (250000); \
		EXTERNAL_CALLN (fprintf, stderr, "\n" CIFTRACE_NAME ": fatal error: " format, ##args); \
		SetErr (); \
	} while (0)

