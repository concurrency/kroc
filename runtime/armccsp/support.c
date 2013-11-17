/*
 *	support.c -- assorted support routines for ARM/CCSP
 *	Copyright (C) 2013 Fred Barnes, University of Kent <frmb@kent.ac.uk>
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
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>
#include <errno.h>

#include <sys/types.h>
#include <unistd.h>

#include <armccsp.h>


/*{{{  void armccsp_fatal (const char *fmt, ...)*/
/*
 *	called on serious (fatal) error
 *	cannot return in any meaningful way
 */
void armccsp_fatal (const char *fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	fprintf (stderr, "CCSP (fatal): ");
	vfprintf (stderr, fmt, ap);
	fprintf (stderr, "\n");
	va_end (ap);

	_exit (1);
}
/*}}}*/
/*{{{  void armccsp_error (const char *fmt, ...)*/
/*
 *	called on non-fatal error
 */
void armccsp_error (const char *fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	fprintf (stderr, "CCSP (error): ");
	vfprintf (stderr, fmt, ap);
	fprintf (stderr, "\n");
	va_end (ap);

	return;
}
/*}}}*/
/*{{{  void armccsp_warning (const char *fmt, ...)*/
/*
 *	called on warning
 */
void armccsp_warning (const char *fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	fprintf (stderr, "CCSP (warning): ");
	vfprintf (stderr, fmt, ap);
	fprintf (stderr, "\n");
	va_end (ap);

	return;
}
/*}}}*/


/*{{{  void *armccsp_smalloc (const int bytes)*/
/*
 *	allocates memory (from system heap)
 *	returns pointer to the block
 */
void *armccsp_smalloc (const int bytes)
{
	void *ptr;

	if (bytes <= 0) {
		armccsp_warning ("impossible allocation! (%d bytes)", bytes);
		ptr = NULL;
	} else {
		ptr = malloc (bytes);
		if (!ptr) {
			armccsp_fatal ("malloc(%d) failed: %s", bytes, strerror (errno));
		}
	}
#ifdef CCSP_DEBUG
	fprintf (stderr, "armccsp_smalloc(): allocated %d bytes at %p - %p\n", bytes, ptr, (ptr + (bytes - 1)));
#endif
	return ptr;
}
/*}}}*/
/*{{{  void armccsp_sfree (void *ptr)*/
/*
 *	frees memory (to system heap)
 */
void armccsp_sfree (void *ptr)
{
	if (!ptr) {
		armccsp_warning ("attempt free NULL!");
	} else {
		free (ptr);
	}
}
/*}}}*/
/*{{{  void armccsp_sfreep (void **ptr)*/
/*
 *	frees memory (to system heap)
 */
void armccsp_sfreep (void **ptr)
{
	if (!*ptr) {
		armccsp_warning ("attempt free NULL!");
	} else {
		free (*ptr);
		*ptr = NULL;
	}
}
/*}}}*/


