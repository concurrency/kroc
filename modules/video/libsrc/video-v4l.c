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

#include <errno.h>

#include <sys/ioctl.h>
#include <sys/mman.h>

#include <asm/types.h>
#include <linux/videodev.h>
#include <linux/videodev2.h>

#include <libv4lconvert.h>

#include "video-v4l.h"

#define MAX_NBUFS 16

typedef struct opi_video_device {
	int fd;
	int api;
	int caps;

	int input;
	int use_mmap;
	int oneshot;
	int n_buffers;
	
	struct v4l2_format src;
	struct v4l2_format dst;

	struct v4lconvert_data *convert;
	
	struct {
		int	idx;
		void 	*addr;
		int 	size;
	} buffers[MAX_NBUFS];
} opi_video_device_t;

static inline opi_video_device_t *video_initstruct (void) /*{{{*/
{
	opi_video_device_t *dev = malloc (sizeof (opi_video_device_t));
	int i;

	dev->fd		= -1;
	dev->api	= 0;
	dev->caps 	= 0;
	dev->input	= -1;
	dev->use_mmap	= -1;
	dev->oneshot	= 0;
	dev->n_buffers	= 0;
	dev->convert	= NULL;

	for (i = 0; i < MAX_NBUFS; ++i) {
		dev->buffers[i].idx = -1;
		dev->buffers[i].addr = NULL;
		dev->buffers[i].size = 0;
	}

	return dev;
}
/*}}}*/
static inline int video_open (opi_video_device_t *dev, char *fname, int fnamelen) /*{{{*/
{
	if (dev->fd < 0) {
		char pbuffer[FILENAME_MAX];
		int x;

		if (fnamelen >= FILENAME_MAX) {
			x = FILENAME_MAX - 1;
		} else {
			x = fnamelen;
		}
		memcpy (pbuffer, fname, x);
		pbuffer[x] = '\0';

		dev->fd = open (pbuffer, O_RDWR);
		if (dev->fd >= 0) {
			struct v4l2_capability v2_cap;
			struct video_capability v1_cap;
			int r;

			/* see if it's a V1 or V2 device by prodding the V2 IOCTL */
			while (((r = ioctl (dev->fd, VIDIOC_QUERYCAP, &v2_cap)) == -1) && (errno == EINTR));		/* retry */
			if (r < 0) {
				/* try V1 API */
				while (((r = ioctl (dev->fd, VIDIOCGCAP, &v1_cap)) == -1) && (errno == EINTR));		/* retry */
				if (r < 0) {
					/* not a V4L device! */
					close (dev->fd);
					dev->fd = -1;
					return 0;
				} else {
					dev->api = 1;
				}
			} else {
				dev->api = 2;
				dev->caps = v2_cap.capabilities;
			}

			return 1;
		}
	}

	return 0;
}
/*}}}*/
static inline int video_close (opi_video_device_t *dev) /*{{{*/
{
	if (dev->fd >= 0) {
		if (close (dev->fd) == 0) {
			dev->fd = -1;
			return 1;
		}
	}
	return 0;
}
/*}}}*/
static inline opi_video_device_t *video_freestruct (opi_video_device_t *dev) /*{{{*/
{
	free (dev);
	return NULL;
}
/*}}}*/

static inline int video_identity (opi_video_device_t *dev, opi_video_identity_t *ident) /*{{{*/
{
	if (dev->fd < 0) {
		/* not open */
	} else if (dev->api == 1) {
		struct video_capability vcap;
		int r;

		while (((r = ioctl (dev->fd, VIDIOCGCAP, &vcap)) == -1) && (errno == EINTR));				/* retry */
		if (r >= 0) {
			ident->namelen = snprintf (ident->name, OPI_VIDEO_IDENTITY_NAMEMAX - 1, "%s", vcap.name);
			return 1;
		}
	} else if (dev->api == 2) {
		struct v4l2_capability v2_cap;
		int r;

		while (((r = ioctl (dev->fd, VIDIOC_QUERYCAP, &v2_cap)) == -1) && (errno == EINTR));			/* retry */
		if (r >= 0) {
			ident->namelen = snprintf (ident->name, OPI_VIDEO_IDENTITY_NAMEMAX - 1, "%s:%s:%s",
						v2_cap.driver, v2_cap.card, v2_cap.bus_info);
			return 1;
		}
	}
	return 0;
}
/*}}}*/
static inline int video_numinputs (opi_video_device_t *dev) /*{{{*/
{
	int num = 0;
	if (dev->fd < 0) {
		/* not open */
	} else if (dev->api == 1) {
		struct video_capability vcap;
		int r;

		while (((r = ioctl (dev->fd, VIDIOCGCAP, &vcap)) == -1) && (errno == EINTR));				/* retry */
		if (r >= 0) {
			num = vcap.channels;
		}
	} else if (dev->api == 2) {
		struct v4l2_input input;

		input.index = 0;
		while (ioctl (dev->fd, VIDIOC_ENUMINPUT, &input) >= 0)
			input.index++;

		num = input.index;
	}
	return num;
}
/*}}}*/
static inline void video_getinputs (opi_video_device_t *dev, opi_video_input_t *inputs, int numinputs) /*{{{*/
{
	int i;
	
	/* initialise structures */
	for (i = 0; i < numinputs; ++i) {
		inputs[i].id		= -1;
		inputs[i].namelen	= 0;
		inputs[i].name[0]	= '\0';
		inputs[i].type		= 0;
		inputs[i].flags		= 0;
		inputs[i].minw		= 0;
		inputs[i].minh		= 0;
		inputs[i].maxw		= 0;
		inputs[i].maxh		= 0;
	}

	if (dev->fd < 0) {
		/* nothing */
	} else if (dev->api == 1) {
		struct video_capability vcap;
		int r;

		while (((r = ioctl (dev->fd, VIDIOCGCAP, &vcap)) == -1) && (errno == EINTR));				/* retry */
		if (r >= 0) {
			int channo = 0;

			for (i = 0; (i < vcap.channels) && (channo < numinputs); i++) {
				struct video_channel cinf;

				cinf.channel = i;
				while (((r = ioctl (dev->fd, VIDIOCGCHAN, &cinf)) == -1) && (errno == EINTR));		/* retry */
				if (r >= 0) {
					/* this one */
					inputs[channo].id = i;
					inputs[channo].namelen = snprintf (inputs[channo].name, OPI_VIDEO_INPUT_NAMEMAX, "%s", cinf.name);
					if (cinf.type == VIDEO_TYPE_TV)
						inputs[channo].type = OPI_VIDEO_TYPE_TUNER;
					else if (cinf.type == VIDEO_TYPE_CAMERA)
						inputs[channo].type = OPI_VIDEO_TYPE_CAMERA;
					if (cinf.flags & VIDEO_VC_AUDIO)
						inputs[channo].flags |= OPI_VIDEO_FLAG_AUDIO;
					inputs[channo].minw = vcap.minwidth;
					inputs[channo].minh = vcap.minheight;
					inputs[channo].maxw = vcap.maxwidth;
					inputs[channo].maxh = vcap.maxheight;

					channo++;
				}
			}
		}
	} else if (dev->api == 2) {
		struct v4l2_input input;
		int idx, oidx, r;
		
		if (ioctl (dev->fd, VIDIOC_G_INPUT, &oidx) < 0)
			oidx = -1;

		for (idx = 0; idx < numinputs; idx++) {
			struct v4l2_fmtdesc desc;
			struct v4l2_format  fmt;

			inputs[idx].id 	= idx;
			input.index 	= idx;
			
			while (((r = ioctl (dev->fd, VIDIOC_S_INPUT, &idx)) == -1) && (errno == EINTR));
			if (r < 0)
				continue;
			while (((r = ioctl (dev->fd, VIDIOC_ENUMINPUT, &input)) == -1) && (errno == EINTR));
			if (r < 0)
				continue;

			inputs[idx].namelen = snprintf (inputs[idx].name, OPI_VIDEO_INPUT_NAMEMAX, "%s", input.name);
			if (input.type == V4L2_INPUT_TYPE_TUNER)
				inputs[idx].type = OPI_VIDEO_TYPE_TUNER;
			else if (input.type == V4L2_INPUT_TYPE_CAMERA)
				inputs[idx].type = OPI_VIDEO_TYPE_CAMERA;
			if (input.audioset)
				inputs[idx].flags |= OPI_VIDEO_FLAG_AUDIO;
			
			memset (&desc, 0, sizeof (desc));
			memset (&fmt, 0, sizeof (fmt));

			desc.index	= 0;
			desc.type 	= V4L2_BUF_TYPE_VIDEO_CAPTURE;
			if (ioctl (dev->fd, VIDIOC_ENUM_FMT, &desc) < 0)
				continue;

			fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
			fmt.fmt.pix.width       = 10000;
			fmt.fmt.pix.height      = 10000;
			fmt.fmt.pix.pixelformat = desc.pixelformat;
			if (ioctl (dev->fd, VIDIOC_TRY_FMT, &fmt) < 0)
				continue;

			inputs[idx].minw = 48;
			inputs[idx].minh = 32;
			inputs[idx].maxw = fmt.fmt.pix.width;
			inputs[idx].maxh = fmt.fmt.pix.height;
		}
		
		if (oidx >= 0)
			ioctl (dev->fd, VIDIOC_S_INPUT, &oidx);
	}
}
/*}}}*/
static inline int video_currentinput (opi_video_device_t *dev) /*{{{*/
{
	if (dev->fd < 0) {
		/* not open */
	} else if (dev->api == 1) {
		if (dev->input == -1) {
			struct video_channel cinf;
			int r;

			cinf.channel = 0;
			while (((r = ioctl (dev->fd, VIDIOCGCHAN, &cinf)) == -1) && (errno == EINTR));		/* retry */

			if (r >= 0) {
				while (((r = ioctl (dev->fd, VIDIOCSCHAN, &cinf)) == -1) && (errno == EINTR));		/* retry */
				if (r >= 0) {
					dev->input = cinf.channel;
				}
			}
		}
	} else if (dev->api == 2) {
		int idx;
		if (ioctl (dev->fd, VIDIOC_G_INPUT, &idx) < 0)
			dev->input = idx;
	}
	return dev->input;
}
/*}}}*/
static inline int video_setinput (opi_video_device_t *dev, opi_video_input_t *input) /*{{{*/
{
	if (dev->fd < 0) {
		/* not open */
	} else if (dev->api == 1) {
		struct video_channel cinf;
		int r;

		/* get current channel properties */
		cinf.channel = input->id;
		while (((r = ioctl (dev->fd, VIDIOCGCHAN, &cinf)) == -1) && (errno == EINTR));		/* retry */

		if ((r >= 0) && (cinf.channel == input->id)) {
			while (((r = ioctl (dev->fd, VIDIOCSCHAN, &cinf)) == -1) && (errno == EINTR));		/* retry */
			if (r >= 0) {
				dev->input = input->id;
				return 1;
			}
		}
	} else if (dev->api == 2) {
		int idx = input->id;
		int r;

		while (((r = ioctl (dev->fd, VIDIOC_S_INPUT, &idx)) == -1) && (errno == EINTR));		/* retry */

		if (r >= 0) {
			dev->input = idx;
			return 1;
		}
	}
	return 0;
}
/*}}}*/

static int pal_convert_v4l1[] = {
	OPI_VIDEO_PAL_GRAY, 	VIDEO_PALETTE_GREY,
	OPI_VIDEO_PAL_HI240, 	VIDEO_PALETTE_HI240,
	OPI_VIDEO_PAL_RGB565, 	VIDEO_PALETTE_RGB565,
	OPI_VIDEO_PAL_RGB24, 	VIDEO_PALETTE_RGB24,
	OPI_VIDEO_PAL_RGB32, 	VIDEO_PALETTE_RGB32,
	OPI_VIDEO_PAL_RGB555, 	VIDEO_PALETTE_RGB555,
	OPI_VIDEO_PAL_YUV422, 	VIDEO_PALETTE_YUV422, 
	OPI_VIDEO_PAL_YUYV, 	VIDEO_PALETTE_YUYV,
	OPI_VIDEO_PAL_UYVY, 	VIDEO_PALETTE_UYVY,
	OPI_VIDEO_PAL_YUV420,	VIDEO_PALETTE_YUV420,
	OPI_VIDEO_PAL_YUV411,	VIDEO_PALETTE_YUV411,
	OPI_VIDEO_PAL_YUV422P,	VIDEO_PALETTE_YUV422P,
	OPI_VIDEO_PAL_YUV411P,	VIDEO_PALETTE_YUV411P,
	OPI_VIDEO_PAL_YUV420P, 	VIDEO_PALETTE_YUV420P,
	OPI_VIDEO_PAL_YUV410P,	VIDEO_PALETTE_YUV410P,
	OPI_VIDEO_PAL_INVALID, 	0
};

static int pal_convert_v4l2[] = {
	OPI_VIDEO_PAL_GRAY,	V4L2_PIX_FMT_GREY,
	OPI_VIDEO_PAL_HI240,	V4L2_PIX_FMT_HI240,
	OPI_VIDEO_PAL_RGB565,	V4L2_PIX_FMT_RGB565,
	OPI_VIDEO_PAL_RGB24,	V4L2_PIX_FMT_RGB24,
	OPI_VIDEO_PAL_RGB32,	V4L2_PIX_FMT_RGB32,
	OPI_VIDEO_PAL_BGR24,	V4L2_PIX_FMT_BGR24,
	OPI_VIDEO_PAL_BGR32,	V4L2_PIX_FMT_BGR32,
	OPI_VIDEO_PAL_RGB555,	V4L2_PIX_FMT_RGB555,
	OPI_VIDEO_PAL_YUV422,	V4L2_PIX_FMT_YUYV,
	OPI_VIDEO_PAL_YUYV,	V4L2_PIX_FMT_YUYV,
	OPI_VIDEO_PAL_UYVY, 	V4L2_PIX_FMT_UYVY,
	OPI_VIDEO_PAL_YUV420,	V4L2_PIX_FMT_YUV420,
	OPI_VIDEO_PAL_YUV422P,	V4L2_PIX_FMT_YUV422P,
	OPI_VIDEO_PAL_YUV411P,	V4L2_PIX_FMT_YUV411P,
	OPI_VIDEO_PAL_INVALID,	0
};

static int search_and_return (int *array, int stride, int search, int ret, int end, int fail, int value)
{
	int i;
	for (i = 0; array[i + search] != end; i += stride) {
		if (array[i + search] == value)
			return array[i + ret];
	}
	return fail;
}

static inline int convert_pal_opi_to_v4l1 (int v)
{
	return search_and_return (pal_convert_v4l1, 2, 0, 1, OPI_VIDEO_PAL_INVALID, 0, v);
}
static inline int convert_pal_v4l1_to_opi (int v)
{
	return search_and_return (pal_convert_v4l1, 2, 1, 0, 0, OPI_VIDEO_PAL_INVALID, v);
}
static inline int convert_pal_opi_to_v4l2 (int v)
{
	return search_and_return (pal_convert_v4l2, 2, 0, 1, OPI_VIDEO_PAL_INVALID, 0, v);
}
static inline int convert_pal_v4l2_to_opi (int v)
{
	return search_and_return (pal_convert_v4l2, 2, 1, 0, 0, OPI_VIDEO_PAL_INVALID, v);
}

static int _v4l2_get_control (opi_video_device_t *dev, unsigned int id)
{
	struct v4l2_control ctrl;
	ctrl.id		= id;
	ctrl.value	= 0;
	ioctl (dev->fd, VIDIOC_G_CTRL, &ctrl);
	return ctrl.value;
}

static int _v4l2_set_control (opi_video_device_t *dev, unsigned int id, signed int value)
{
	struct v4l2_control ctrl;
	int r;
	ctrl.id		= id;
	ctrl.value	= value;
	while (((r = ioctl (dev->fd, VIDIOC_S_CTRL, &ctrl)) < 0) && (errno == EINTR));
	return r;
}

static inline void video_getframeinfo (opi_video_device_t *dev, opi_video_frameinfo_t *finfo) /*{{{*/
{
	finfo->width	= 0;
	finfo->height	= 0;
	finfo->format	= 0;

	if (dev->fd < 0) {
		/* nothing */
	} else if (dev->api == 1) {
		struct video_picture pict;
		struct video_window vwin;
		int r;

		while (((r = ioctl (dev->fd, VIDIOCGWIN, &vwin)) == -1) && (errno == EINTR));		/* retry */
		if (r < 0)
			return;

		finfo->width = vwin.width;
		finfo->height = vwin.height;
		
		while (((r = ioctl (dev->fd, VIDIOCGPICT, &pict)) == -1) && (errno == EINTR));		/* retry */
		if (r < 0)
			return;

		finfo->format = convert_pal_v4l1_to_opi (pict.palette);
	} else if (dev->api == 2) {
		struct v4l2_format fmt;
		int r;
		
		memset (&fmt, 0, sizeof (fmt));
		fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

		while (((r = ioctl (dev->fd, VIDIOC_G_FMT, &fmt)) == -1) && (errno == EINTR));		/* retry */
		if (r < 0)
			return;

		finfo->width	= fmt.fmt.pix.width;
		finfo->height	= fmt.fmt.pix.height;
		finfo->format	= convert_pal_v4l2_to_opi (fmt.fmt.pix.pixelformat);
	}
}
/*}}}*/
static inline int video_setframeinfo (opi_video_device_t *dev, opi_video_frameinfo_t *finfo) /*{{{*/
{
	if (dev->fd < 0) {
		/* nothing */
	} else if (dev->api == 1) {
		struct video_picture pict;
		struct video_window vwin;
		int r;

		while (((r = ioctl (dev->fd, VIDIOCGWIN, &vwin)) == -1) && (errno == EINTR));		/* retry */
		if (r < 0)
			return 0;
		while (((r = ioctl (dev->fd, VIDIOCGPICT, &pict)) == -1) && (errno == EINTR));		/* retry */
		if (r < 0)
			return 0;

		vwin.width = finfo->width;
		vwin.height = finfo->height;
		pict.palette = convert_pal_v4l1_to_opi (finfo->format);
		
		while (((r = ioctl (dev->fd, VIDIOCSWIN, &vwin)) == -1) && (errno == EINTR));		/* retry */
		if (r < 0)
			return 0;
		while (((r = ioctl (dev->fd, VIDIOCSPICT, &pict)) == -1) && (errno == EINTR));		/* retry */
		if (r >= 0)
			return 1;
	} else if (dev->api == 2) {
		struct v4l2_format fmt;
		int r;
		
		memset (&fmt, 0, sizeof (fmt));
		fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

		while (((r = ioctl (dev->fd, VIDIOC_G_FMT, &fmt)) == -1) && (errno == EINTR));		/* retry */
		if (r < 0)
			return 0;

		fmt.fmt.pix.width	= finfo->width;
		fmt.fmt.pix.height	= finfo->height;
		fmt.fmt.pix.pixelformat = convert_pal_opi_to_v4l2 (finfo->format);

		while (((r = ioctl (dev->fd, VIDIOC_S_FMT, &fmt)) == -1) && (errno == EINTR));		/* retry */
		if (r >= 0)
			return 1;
	}
	return 0;
}
/*}}}*/
static inline void video_getpicture (opi_video_device_t *dev, opi_video_picture_t *pict) /*{{{*/
{
	if (dev->fd < 0) {
		/* nothing */
	} else if (dev->api == 1) {
		struct video_picture buf;
		int r;

		while (((r = ioctl (dev->fd, VIDIOCGPICT, &buf)) == -1) && (errno == EINTR));		/* retry */
		pict->brightness 	= buf.brightness;
		pict->hue		= buf.hue;
		pict->contrast		= buf.contrast;
		pict->whiteness		= buf.whiteness;
		pict->colour		= buf.colour;
	} else if (dev->api == 2) {
		pict->brightness	= _v4l2_get_control (dev, V4L2_CID_BRIGHTNESS);
		pict->hue		= _v4l2_get_control (dev, V4L2_CID_HUE);
		pict->contrast		= _v4l2_get_control (dev, V4L2_CID_CONTRAST);
		pict->whiteness		= _v4l2_get_control (dev, V4L2_CID_WHITENESS);
		pict->colour		= 0;
	}
}
/*}}}*/
static inline int video_setpicture (opi_video_device_t *dev, opi_video_picture_t *pict) /*{{{*/
{
	if (dev->fd < 0) {
		return 0; /* not open */
	} else if (dev->api == 1) {
		struct video_picture buf;
		int r;
		
		while (((r = ioctl (dev->fd, VIDIOCGPICT, &buf)) == -1) && (errno == EINTR));		/* retry */
		if (r < 0)
			return 0;
		
		buf.brightness	= pict->brightness;
		buf.hue		= pict->hue;
		buf.contrast	= pict->contrast;
		buf.whiteness	= pict->whiteness;
		buf.colour	= pict->colour;

		while (((r = ioctl (dev->fd, VIDIOCSPICT, &buf)) == -1) && (errno == EINTR));		/* retry */
		if (r < 0)
			return 0;
	} else if (dev->api == 2) {
		_v4l2_set_control (dev, V4L2_CID_BRIGHTNESS, pict->brightness);
		_v4l2_set_control (dev, V4L2_CID_HUE, pict->hue);
		_v4l2_set_control (dev, V4L2_CID_CONTRAST, pict->contrast);
		_v4l2_set_control (dev, V4L2_CID_WHITENESS, pict->whiteness);
	}
	
	return 1;
}
/*}}}*/
static inline int video_initio (opi_video_device_t *dev, int oneshot, opi_video_frameinfo_t *finfo) /*{{{*/
{
	dev->oneshot = oneshot;
	dev->convert = v4lconvert_create (dev->fd);

	if (dev->fd < 0) {
		/* nothing */
	} else if (dev->api == 1) {
		struct video_mbuf vmbuf;
		int r;
		
		memset (&(dev->src), 0, sizeof (dev->src));
		dev->src.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
		dev->src.fmt.pix.width = finfo->width;
		dev->src.fmt.pix.height = finfo->height;
		dev->src.fmt.pix.pixelformat = convert_pal_opi_to_v4l2 (finfo->format);
		dev->src.fmt.pix.colorspace = V4L2_COLORSPACE_SMPTE170M;
		
		memcpy (&(dev->dst), &(dev->src), sizeof (dev->dst));
		dev->dst.fmt.pix.pixelformat = V4L2_PIX_FMT_RGB24;
		dev->dst.fmt.pix.colorspace = V4L2_COLORSPACE_SRGB;

		vmbuf.size = 0;
		vmbuf.frames = 0;

		while (((r = ioctl (dev->fd, VIDIOCGMBUF, &vmbuf)) == -1) && (errno == EINTR));		/* retry */
		if (r >= 0) {
			/* got memory-map buffer info */
			dev->buffers[0].size = vmbuf.size;
			dev->buffers[0].addr = mmap (NULL, vmbuf.size, 
								PROT_READ | PROT_WRITE, 
								MAP_SHARED, 
								dev->fd, 0);
			if (dev->buffers[0].addr == MAP_FAILED) {
				/* failed to mmap, default to non-mmap */
				dev->use_mmap = 0;
			} else {
				dev->use_mmap = 1;
				dev->n_buffers = 1;
			}
		} else {
			dev->use_mmap = 0;
		}
		return 1;
	} else if (dev->api == 2) {
		int r;

		memset (&(dev->src), 0, sizeof (dev->src));
		dev->src.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
		while (((r = ioctl (dev->fd, VIDIOC_G_FMT, &(dev->src))) == -1) && (errno == EINTR));		/* retry */
		
		memcpy (&(dev->dst), &(dev->src), sizeof (dev->dst));
		dev->dst.fmt.pix.pixelformat = V4L2_PIX_FMT_RGB24;
		dev->dst.fmt.pix.colorspace = V4L2_COLORSPACE_SRGB;
		
		if (dev->caps & V4L2_CAP_STREAMING) {
			struct v4l2_requestbuffers req;
			struct v4l2_buffer buffer;
			int i;

			memset (&req, 0, sizeof (req));
			req.type	= V4L2_BUF_TYPE_VIDEO_CAPTURE;
			req.memory	= V4L2_MEMORY_MMAP;
			req.count 	= oneshot ? 1 : MAX_NBUFS;

			if (ioctl (dev->fd, VIDIOC_REQBUFS, &req) < 0)
				return 0;

			if (req.count < 1)
				return 0;

			dev->n_buffers = 0;
			for (i = 0; i < req.count; ++i) {
				void *addr;

				memset (&buffer, 0, sizeof (buffer));
				buffer.type	= req.type;
				buffer.memory	= V4L2_MEMORY_MMAP;
				buffer.index	= i;

				if (ioctl (dev->fd, VIDIOC_QUERYBUF, &buffer) < 0)
					continue;

				addr = mmap (NULL, buffer.length,
							PROT_READ | PROT_WRITE,
							MAP_SHARED,
							dev->fd, buffer.m.offset);

				if (addr == MAP_FAILED) {
					continue;
				} else {
					int idx = dev->n_buffers++;
					dev->buffers[idx].addr = addr;
					dev->buffers[idx].size = buffer.length;
					if (!oneshot) {
						while (((r = ioctl (dev->fd, VIDIOC_QBUF, &buffer)) == -1) && (errno == EINTR));	/* retry */
					}
				}
			}

			if (dev->n_buffers <= 0)
				return 0;

			dev->use_mmap = 1;
		} else {
			dev->use_mmap = 0;
		}
		return 1;
	}
	return 0;
}
/*}}}*/
static inline void video_shutdownio (opi_video_device_t *dev) /*{{{*/
{
	if (dev->convert != NULL)
		v4lconvert_destroy (dev->convert);

	if (dev->fd < 0) {
		/* nothing */
	} else if (dev->use_mmap < 0) {
		/* nothing */
	} else if (dev->use_mmap) {
		int i;
		if (dev->api == 2 && !dev->oneshot) {
			int type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
			int r;
			while (((r = ioctl (dev->fd, VIDIOC_STREAMOFF, &type)) == -1) && (errno == EINTR));	/* retry */
		}
		for (i = 0; i < dev->n_buffers; ++i) {
			munmap (dev->buffers[i].addr, dev->buffers[i].size);
			dev->buffers[i].idx = -1;
			dev->buffers[i].addr = NULL;
			dev->buffers[i].size = 0;
		}
		dev->use_mmap = -1;
		dev->n_buffers = 0;
	} else {
		/* FIXME! */
	}
}
/*}}}*/

static void rgb24_rgb32 (int width, int height, unsigned char *buffer)
{
	unsigned char *src = buffer + (width * height * 3);
	unsigned int *dst = ((unsigned int *) buffer) + (width * height);
	int i;

	while (src > buffer) {
		src -= 3;
		dst -= 1;
		(*dst) = (src[0] << 16) | (src[1] << 8) | (src[2] << 0);
	}
}

static inline int video_grabframe (opi_video_device_t *dev, int raw, opi_video_frameinfo_t *finfo, int *buffer, int bufsize) /*{{{*/
{
	if (dev->fd < 0) {
		/* not open */
	} else if (dev->api == 1) {
		if (dev->use_mmap == 1) {
			struct video_mmap vmmap;
			int bnum = 0;
			int r;
			
			vmmap.frame = 0;
			vmmap.width = finfo->width;
			vmmap.height = finfo->height;
			vmmap.format = convert_pal_opi_to_v4l1 (finfo->format);
			
			while (((r = ioctl (dev->fd, VIDIOCMCAPTURE, &vmmap)) == -1) && (errno == EINTR));	/* retry */
			if (r >= 0) {
				while (((r = ioctl (dev->fd, VIDIOCSYNC, &bnum)) == -1) && (errno == EINTR));		/* retry */
				if (r >= 0) {
					if (raw) {
						memcpy (buffer, dev->buffers[0].addr, bufsize);
					} else {
						v4lconvert_convert (
							(struct v4lconvert_data *) dev->convert,
							&(dev->src), &(dev->dst),
							(unsigned char *) dev->buffers[0].addr, dev->buffers[0].size, 
							(unsigned char *) buffer, bufsize);
						rgb24_rgb32 (finfo->width, finfo->height, (unsigned char *) buffer);
					}
					return 1;
				}
			}
		} else {
			/* FIXME! */
		}
	} else if (dev->api == 2) {
		if (dev->use_mmap == 1) {
			struct v4l2_buffer buf;
			int result = 0;
			int r;
			
			memset (&buf, 0, sizeof (buf));
			buf.type	= V4L2_BUF_TYPE_VIDEO_CAPTURE;
			buf.memory	= V4L2_MEMORY_MMAP;
			
			if (dev->oneshot) {
				buf.index = 0;

				while (((r = ioctl (dev->fd, VIDIOC_QBUF, &buf)) == -1) && (errno == EINTR));	/* retry */
				if (r < 0)
					return 0;
				while (((r = ioctl (dev->fd, VIDIOC_STREAMON, &(buf.type))) == -1) && (errno == EINTR));	/* retry */
				if (r < 0)
					return 0;
			}

			while (((r = ioctl (dev->fd, VIDIOC_DQBUF, &buf)) == -1) && (errno == EINTR));	/* retry */
			if (r >= 0) {
				int idx = buf.index;

				if (raw) {
					memcpy (buffer, dev->buffers[idx].addr, bufsize);
				} else {
					v4lconvert_convert (
						(struct v4lconvert_data *) dev->convert,
						&(dev->src), &(dev->dst),
						(unsigned char *) dev->buffers[idx].addr, dev->buffers[idx].size, 
						(unsigned char *) buffer, bufsize);
					rgb24_rgb32 (finfo->width, finfo->height, (unsigned char *) buffer);
				}

				if (!dev->oneshot)
					while (((r = ioctl (dev->fd, VIDIOC_QBUF, &buf)) == -1) && (errno == EINTR));  /* retry */
				
				result = 1;
			}
			
			if (dev->oneshot) {
				while (((r = ioctl (dev->fd, VIDIOC_STREAMOFF, &(buf.type))) == -1) && (errno == EINTR));	/* retry */
			}

			return result;
		} else {
			/* FIXME! */
		}
	}
	return 0;
}
/*}}}*/


/*{{{  PROC ..video.initstruct (RESULT VIDEO.DEVICE vdev)*/
void _video_initstruct (int *w)			{ *((int *)w[0]) = (int) video_initstruct (); }
/*}}}*/
/*{{{  PROC ..video.open (VAL VIDEO.DEVICE vdev, RESULT BOOL ok)*/
void _video_open (int *w)			{ *((int *)w[3]) = video_open ((opi_video_device_t *)(w[0]), (char *)(w[1]), (int)(w[2])); }
/*}}}*/
/*{{{  PROC ..video.close (VAL VIDEO.DEVICE vdev, RESULT BOOL ok)*/
void _video_close (int *w)			{ *((int *)w[1]) = video_close ((opi_video_device_t *)(w[0])); }
/*}}}*/
/*{{{  PROC ..video.freestruct (RESULT VIDEO.DEVICE vdev)*/
void _video_freestruct (int *w)			{ *((int *)w[0]) = (int) video_freestruct ((opi_video_device_t *)(*((int *)w[0]))); }
/*}}}*/

/*{{{  PROC ..video.identity (VAL VIDEO.DEVICE vdev, RESULT VIDEO.IDENTITY ident, RESULT BOOL ok)*/
void _video_identity (int *w)			{ *((int *)w[2]) = video_identity ((opi_video_device_t *)(w[0]), (opi_video_identity_t *)(w[1])); }
/*}}}*/
/*{{{  PROC ..video.numinputs (VAL VIDEO.DEVICE vdev, RESULT INT num)*/
void _video_numinputs (int *w)			{ *((int *)w[1]) = video_numinputs ((opi_video_device_t *)(w[0])); }
/*}}}*/
/*{{{  PROC ..video.getinputs (VAL VIDEO.DEVICE vdev, []VIDEO.INPUT inputs)*/
void _video_getinputs (int *w)		{ video_getinputs ((opi_video_device_t *)(w[0]), (opi_video_input_t *)(w[1]), (int)(w[2])); }
/*}}}*/
/*{{{  PROC ..video.currentinput (VAL VIDEO.DEVICE vdev, RESULT INT input)*/
void _video_currentinput (int *w)		{ *((int *)w[1]) = video_currentinput ((opi_video_device_t *)(w[0])); }
/*}}}*/
/*{{{  PROC ..video.setinput (VAL VIDEO.DEVICE vdev, VIDEO.INPUT input, RESULT BOOL ok)*/
void _video_setinput (int *w)			{ *((int *)w[2]) = video_setinput ((opi_video_device_t *)(w[0]), (opi_video_input_t *)(w[1])); }
/*}}}*/
/*{{{  PROC ..video.getframeinfo (VAL VIDEO.DEVICE vdev, RESULT VIDEO.FRAMEINFO frameinfo)*/
void _video_getframeinfo (int *w)		{ video_getframeinfo ((opi_video_device_t *)(w[0]), (opi_video_frameinfo_t *)(w[1])); }
/*}}}*/
/*{{{  PROC ..video.setframeinfo (VAL VIDEO.DEVICE vdev, VIDEO.FRAMEINFO frameinfo, RESULT BOOL ok)*/
void _video_setframeinfo (int *w)		{ *((int *)w[2]) = video_setframeinfo ((opi_video_device_t *)(w[0]), (opi_video_frameinfo_t *)(w[1])); }
/*}}}*/
/*{{{  PROC ..video.getpicture (VAL VIDEO.DEVICE vdev, RESULT VIDEO.PICTURE picture)*/
void _video_getpicture (int *w)			{ video_getpicture ((opi_video_device_t *)(w[0]), (opi_video_picture_t *)(w[1])); }
/*}}}*/
/*{{{  PROC ..video.setpicture (VAL VIDEO.DEVICE vdev, VIDEO.PICTURE picture, RESULT BOOL ok)*/
void _video_setpicture (int *w)			{ *((int *)w[2]) = video_setpicture ((opi_video_device_t *)(w[0]), (opi_video_picture_t *)(w[1])); }
/*}}}*/
/*{{{  PROC ..video.initio (VAL VIDEO.DEVICE vdev, VAL BOOL oneshot, RESULT VIDEO.FRAMEINFO finfo, RESULT BOOL ok)*/
void _video_initio (int *w)			{ *((int *)w[3]) = video_initio ((opi_video_device_t *)(w[0]), (int)(w[1]), (opi_video_frameinfo_t *)(w[2])); }
/*}}}*/
/*{{{  PROC ..video.shutdownio (VAL VIDEO.DEVICE vdev)*/
void _video_shutdownio (int *w)			{ video_shutdownio ((opi_video_device_t *)(w[0])); }
/*}}}*/
/*{{{  PROC ..video.grabframe (VAL VIDEO.DEVICE vdev, VAL BOOL raw, VIDEO.FRAMEINFO finfo, []BYTE buffer, RESULT BOOL ok)*/
void _video_grabframe (int *w)			{ *((int *)w[5]) = video_grabframe ((opi_video_device_t *)(w[0]), (int)(w[1]), (opi_video_frameinfo_t *)(w[2]), (int *)(w[3]), (int)(w[4])); }
/*}}}*/

