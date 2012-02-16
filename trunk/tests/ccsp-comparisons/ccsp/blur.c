#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <string.h>

#include <cif.h>

#define STACK_SIZE	256

typedef struct _image_t {
	int width;
	int height;
	unsigned char data[1];
} image_t;

static inline unsigned int get_pixel (image_t *img, int x, int y)
{
	return (unsigned int) img->data[(img->width * y) + x];
}

static inline void set_pixel (image_t *img, int x, int y, unsigned int v)
{
	img->data[(img->width * y) + x] = (unsigned char) (v < 255 ? v : 255);
}

static inline unsigned int add_pixel (const int kw, int x, int y, int p_x, int p_y, int pixel)
{
	int d_x = x - p_x;
	int d_y = y - p_y;
	unsigned int d = (d_x * d_x) + (d_y * d_y);

	if (d == 0)
		return pixel * kw;
	else
		return (pixel * kw) / d;
}

static void blur_image (Workspace wptr, const int kw, image_t *in, image_t *out)
{
	int width = in->width < out->width ? in->width : out->width;
	int height = in->height < out->height ? in->height : out->height;
	int y;

	for (y = 0; y < height; ++y) {
		int x;
		for (x = 0; x < width; ++x) {
			unsigned int sum = 0;
			unsigned int c = 0;
			int i, j;

			for (j = 0; j < kw; ++j) {
				int p_y = y + j - (kw >> 1);
				
				if (p_y < 0 || p_y >= height)
					continue;

				for (i = 0; i < kw; ++i) {
					int p_x = x + i - (kw >> 1);

					if (p_x < 0 || p_x >= width)
						continue;

					unsigned int p = get_pixel (in, p_x, p_y);
					sum += add_pixel (kw, x, y, p_x, p_y, p);
					c++;
				}
			}

			set_pixel (out, x, y, sum / c);
		}

		Reschedule (wptr);
	}
}

static image_t *image_alloc (Workspace wptr, int width, int height)
{
	image_t *img = (image_t *) MAlloc (
		wptr, 
		sizeof (image_t) + (width * height)
	);

	img->width = width;
	img->height = height;

	return img;
}

static void image_free (Workspace wptr, image_t *img)
{
	MTRelease (wptr, img);
}

static void element (Workspace wptr)
{
	mt_cb_t *svr = ProcGetParam (wptr, 0, mt_cb_t *);

	for (;;) {
		image_t *img = NULL;

		MTLock (wptr, svr, MT_CB_CLIENT);
		MTChanIn (wptr, &(svr->channels[0]), (void *) &img);
		MTUnlock (wptr, svr, MT_CB_CLIENT);

		if (img != NULL) {
			image_t *buf = image_alloc (wptr, img->width, img->height);
			blur_image (wptr, 9, img, buf);
			image_free (wptr, img);
			image_free (wptr, buf);
		} else {
			break;
		}
	}
}

static void server (Workspace wptr, int frames, int elements, mt_cb_t *svr)
{
	int width = 512, height = 512;
	int i;

	fprintf (stdout, "start\n");
	fflush (stdout);

	for (i = 0; i < frames; ++i) {
		image_t *img = image_alloc (wptr, width, height);
		memset (img->data, 128, width * height);
		MTChanOut (wptr, &(svr->channels[0]), (void *) &img);
	}

	for (i = 0; i < elements; ++i) {
		image_t *img = NULL;
		MTChanOut (wptr, &(svr->channels[0]), (void *) &img);
	}
	
	fprintf (stdout, "end\n");
	fflush (stdout);
}

static void proc_main (Workspace wptr)
{
	LightProcBarrier bar;
	int frames = ProcGetParam (wptr, 0, int);
	int elements = ProcGetParam (wptr, 1, int);
	
	mt_cb_t *chan = MTAllocChanType (wptr, 1, true);
	word	**workspace = MAlloc (wptr, sizeof (word *) * elements);
	int i;

	LightProcBarrierInit (wptr, &bar, elements);

	for (i = 0; i < elements; ++i) {
		Workspace ws = workspace[i] = MAlloc (wptr, sizeof (word) * WORKSPACE_SIZE (1, STACK_SIZE));
		
		ws = LightProcInit (wptr, ws, 1, STACK_SIZE);
		ProcParam (wptr, ws, 0, (word) chan);
		LightProcStart (wptr, &bar, ws, element);
	}

	server (wptr, frames, elements, chan);

	LightProcBarrierWait (wptr, &bar);

	Shutdown (wptr);
}

int main (int argc, char *argv[])
{
	Workspace p;
	int elements = 1;
	int frames = 64;

	if (argc >= 2)
		frames = atoi (argv[1]);
	if (argc >= 3)
		elements = atoi (argv[2]);

	if (!ccsp_init ())
		return 1;

	p = ProcAllocInitial (2, 1024 * 1024);
	ProcParam (p, p, 0, frames);
	ProcParam (p, p, 1, elements);
	ProcStartInitial (p, proc_main);

	return 0;
}
