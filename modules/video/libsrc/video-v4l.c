/*
 *	video-v4l.c -- C interface to V4L2
 *	Copyright (C) 2008 Fred Barnes <frmb@kent.ac.uk>
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <unistd.h>
#include <fcntl.h>

#include <asm/types.h>
#include <linux/videodev2.h>


static inline void video_v4l2_open (char *fname, int flen, int *fd) /*{{{*/
{
	char pbuffer[FILENAME_MAX];
	int x;

	if (flen >= (FILENAME_MAX - 1)) {
		x = FILENAME_MAX - 1;
	} else {
		x = flen;
	}
	memcpy (pbuffer, fname, x);
	pbuffer[x] = '\0';
	*fd = open (pbuffer, O_RDWR);
}
/*}}}*/

/*{{{  PROC ..video.v4l2.open (VAL []BYTE device, RESULT INT fd)*/
void _video_v4l2_open (int *w)			{ video_v4l2_open ((char *)(w[0]), (int)(w[1]), (int *)(w[2])); }
/*}}}*/

