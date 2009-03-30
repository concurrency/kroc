/*
 *	video-v4l.h -- private definitions for occam-pi V4L2 library
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

#ifndef __VIDEO_V4L_H
#define __VIDEO_V4L_H

#define OPI_VIDEO_TYPE_CAMERA	0x1
#define OPI_VIDEO_TYPE_TUNER	0x2
#define OPI_VIDEO_FLAG_AUDIO	0x1

#define OPI_VIDEO_PAL_INVALID	0
#define OPI_VIDEO_PAL_GRAY	1
#define OPI_VIDEO_PAL_HI240	2
#define OPI_VIDEO_PAL_RGB565	3
#define OPI_VIDEO_PAL_RGB24	4
#define OPI_VIDEO_PAL_RGB32	5
#define OPI_VIDEO_PAL_RGB555	6
#define OPI_VIDEO_PAL_YUV422	7
#define OPI_VIDEO_PAL_YUYV	8
#define OPI_VIDEO_PAL_UYVY	9
#define OPI_VIDEO_PAL_YUV420	10
#define OPI_VIDEO_PAL_YUV411	11
#define OPI_VIDEO_PAL_YUV422P	13
#define OPI_VIDEO_PAL_YUV411P	14
#define OPI_VIDEO_PAL_YUV420P	15
#define OPI_VIDEO_PAL_YUV410P	16
#define OPI_VIDEO_PAL_BGR24	17
#define OPI_VIDEO_PAL_BGR32	18

#define OPI_VIDEO_IDENTITY_NAMEMAX 64

typedef struct opi_video_identity {
	char name[OPI_VIDEO_IDENTITY_NAMEMAX];
	int namelen;
} opi_video_identity_t;

#define OPI_VIDEO_INPUT_NAMEMAX 64

typedef struct opi_video_input {
	int id;
	char name[OPI_VIDEO_INPUT_NAMEMAX];
	int namelen;
	int type;
	int flags;
	int minw;
	int minh;
	int maxw;
	int maxh;
} opi_video_input_t;

typedef struct opi_video_picture {
	int brightness;
	int hue;
	int colour;
	int contrast;
	int whiteness;
} opi_video_picture_t;

typedef struct opi_video_frameinfo {
	int width;
	int height;
	int format;
	int bytes;
} opi_video_frameinfo_t;

#endif	/* !__VIDEO_V4L_H */

