/*
 *	ciftrace.h -- tracing macros for CIF applications
 *	Copyright (C) 2005, 2006, 2008 Adam Sampson <ats@offog.org>
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
#include <cif.h>

static int ciftrace_enabled = -1;

static void ciftrace_startup ()
{
	ciftrace_enabled = (getenv("CIFTRACE_ENABLED") != NULL);
}

#define CTRACE(format, n, args...) do { \
		if (ciftrace_enabled == -1) { \
			ciftrace_startup (); \
		} \
		if (ciftrace_enabled) { \
			ExternalCallN (fprintf, n + 4, stderr, CIFTRACE_NAME ": %s:%5d: " format, __FILE__, __LINE__, ##args); \
		} \
	} while (0)
#define CFATAL(format, n, args...) do { \
		CTRACE (">>> fatal error here <<<\n", 0); \
		TimerDelay (wptr, 250000); \
		ExternalCallN (fprintf, n + 2, stderr, "\n" CIFTRACE_NAME ": fatal error: " format, ##args); \
		SetErrW (wptr); \
	} while (0)

