#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <string.h>

#include <pthread.h>

typedef struct _image_t image_t;
struct _image_t {
	int width;
	int height;
	image_t *next;
	unsigned char data[1];
};

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

static void blur_image (const int kw, image_t *in, image_t *out)
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
	}
}

static image_t *image_alloc (int width, int height)
{
	image_t *img = (image_t *) malloc (
		sizeof (image_t) + (width * height)
	);

	img->width = width;
	img->height = height;
	img->next = NULL;

	return img;
}

static void image_free (image_t *img)
{
	free (img);
}

static pthread_mutex_t 	lock;
static pthread_cond_t 	want_work;
static pthread_cond_t 	work_available;
static pthread_cond_t 	worker_terminated;
static volatile int 	complete = 0;
static volatile int	workers = 0;
static image_t *volatile next_image = NULL;

static void *element (void *p)
{
	for (;;) {
		image_t *img = NULL;

		pthread_mutex_lock (&lock);
		while (!complete && next_image == NULL) {
			pthread_cond_signal (&want_work);
			pthread_cond_wait (&work_available, &lock);
		}
		
		img = next_image;

		if (img == NULL) {
			break;
		} else {
			next_image = img->next;
		}
		pthread_mutex_unlock (&lock);

		{
			image_t *buf = image_alloc (img->width, img->height);
			blur_image (9, img, buf);
			image_free (img);
			image_free (buf);
		}
	}

	workers--;
	pthread_cond_signal (&worker_terminated);
	pthread_mutex_unlock (&lock);

	return NULL;
}

static void server (int frames, int elements, int queuing)
{
	int width = 512, height = 512;
	int i;

	fprintf (stdout, "start\n");
	fflush (stdout);

	for (i = 0; i < frames; ++i) {
		image_t *img = image_alloc (width, height);
		memset (img->data, 128, width * height);
		pthread_mutex_lock (&lock);
		if (next_image != NULL) {
			if (queuing) {
				img->next = next_image;
			} else {
				pthread_cond_wait (&want_work, &lock);
			}
		}
		next_image = img;
		pthread_cond_signal (&work_available);
		pthread_mutex_unlock (&lock);
	}

	pthread_mutex_lock (&lock);
	while (next_image != NULL)
		pthread_cond_wait (&want_work, &lock);
	complete = 1;
	pthread_cond_broadcast (&work_available);
	while (workers > 0)
		pthread_cond_wait (&worker_terminated, &lock);
	pthread_mutex_unlock (&lock);

	fprintf (stdout, "end\n");
	fflush (stdout);
}

static void proc_main (int frames, int elements, int queuing)
{
	int i;

	pthread_mutex_init (&lock, NULL);
	pthread_cond_init (&want_work, NULL);
	pthread_cond_init (&work_available, NULL);
	pthread_cond_init (&worker_terminated, NULL);

	workers = elements;

	for (i = 0; i < elements; ++i) {
		pthread_t thread;
		pthread_create (&thread, NULL, element, NULL);
	}

	server (frames, elements, queuing);
}

int main (int argc, char *argv[])
{
	int elements = 1;
	int frames = 64;
	int queuing = 0;

	if (argc >= 2)
		frames = atoi (argv[1]);
	if (argc >= 3)
		elements = atoi (argv[2]);
	if (argc >= 4)
		queuing = atoi (argv[3]);

	proc_main (frames, elements, queuing);

	return 0;
}
