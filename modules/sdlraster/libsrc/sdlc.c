/*
 *	sdlc.c -- C component of sdlraster
 *	Copyright (C) 2005 Fred Barnes  <frmb@kent.ac.uk>
 *	Copyright (C) 2007 Carl Ritson <cgr@kent.ac.uk>
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
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* NOTE: this assumes a 24/32-bit visual.  Will break if used in 8/16-bit modes */
/* unless SDL transparently maps it away */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static int buffer_size = 0;
static int *buffers[64];
static int *update_buffer = NULL;

static inline void sdlraster_set_buffer (int n, int *ptr, int x, int y)
{
	buffers[n] = ptr;
	buffer_size = x * y * sizeof(int);
}

static inline void sdlraster_set_update_buffer (int *ptr)
{
	update_buffer = ptr;
}

static inline void sdlraster_clear (int *ptr, int x, int y)
{
	memset (ptr, 0, x * y * sizeof(int));
}

static inline void sdlraster_update (int *ptr)
{
	if (update_buffer != NULL) {
		memcpy (update_buffer, ptr, buffer_size);
	}
}

static inline void sdlraster_update_n (int n)
{
	if ((update_buffer != NULL) && (buffers[n] != NULL)) {
		memcpy (update_buffer, buffers[n], buffer_size);
	}
}

void _sdlraster_set_buffer (int *ws)
{
	sdlraster_set_buffer (ws[0], (int *) ws[1], ws[2], ws[3]);
}

void _sdlraster_set_update_buffer (int *ws)
{
	sdlraster_set_update_buffer ((int *) ws[0]);
}

void _sdlraster_clear (int *ws)
{
	sdlraster_clear ((int *) ws[0], ws[1], ws[2]);
}

void _sdlraster_update (int *ws)
{
	sdlraster_update ((int *) ws[0]);
}

void _sdlraster_update_n (int *ws)
{
	sdlraster_update_n (ws[0]);
}

