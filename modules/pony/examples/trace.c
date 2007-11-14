/*
 *	trace.c -- tracing message outputter
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

/*{{{  PROC C.trace (VAL []BYTE message) */
/* FIXME Temporary hack to let me print trace messages immediately. */
void _trace (int *ws)
{
	fwrite ((const char *) ws[0], 1, ws[1], stderr);
	fputc ('\n', stderr);
}
/*}}}*/

void _trace_i (int *ws)
{
	fwrite ((const char *) ws[0], 1, ws[1], stderr);
	fprintf (stderr, "%d\n", ws[2]);
}

void _trace_x (int *ws)
{
	fwrite ((const char *) ws[0], 1, ws[1], stderr);
	fprintf (stderr, "0x%8.8x\n", (unsigned int)(ws[2]));
}

void _trace_results (int *ws)
{
	int i;
	int *data = (int *) ws[0];

	for (i = 0; i < ws[1]; i++) {
		if (i > 0) {
			printf ("\t");
		}
		printf ("%d", data[i]);
	}
	printf ("\n");
}

void _printarg (int *ws)
{
	fprintf (stderr, "printarg: %08x type %08x\n", ws[0], ws[1]);
}

/* This does nothing, but it's easy to grep for in .s files. */
void _mark (int *ws)
{
}

