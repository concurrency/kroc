/*
 *	mandelcuda.cu -- CUDA mandelbrot code for use with occam-pi
 *	Copyright (C) 2013 Fred Barnes <frmb@kent.ac.uk>
 *	Derived from earlier occam-pi Mandelbrot code by Jim Moores and David Wood.
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
#include <cuda_runtime.h>

#include <dmem_if.h>
#include <ocuda_ctypes.h>

#define MANDEL_ABSSQLIM 32.0

/*{{{  mandelcuda_ainfo_t: type to define various device-specific settings that can be passed to/from occam*/
typedef struct TAG_mandelcuda_ainfo {
	ocuda_devinfo_t *device;		/* ocuda device structure */
	int *dev_fb;				/* device allocated framebuffer memory */
	int width;				/* width of fractal in pixels */
	int height;				/* height of fractal in pixels */
	int fbsize;				/* dev_fb size in bytes */
	double *dev_parms;			/* device allocated parameters memory */
	int parmssize;				/* dev_parms size in bytes */
	int *dev_iparms;			/* device allocated integer parameters memory */
	int iparmssize;				/* dev_iparms size in bytes */
	int *dev_colour;			/* device allocated colouring-in array */
	int coloursize;				/* dev_colour size in bytes */
} mandelcuda_ainfo_t;


/*}}}*/
/*{{{  complex64_t: 64-bit complex number*/
typedef struct TAG_complex64 {
	double real;
	double imag;
} complex64_t;

/*}}}*/


__device__ inline double complex64abssq (const complex64_t c) /*{{{*/
{
	return (c.real * c.real) + (c.imag * c.imag);
}
/*}}}*/
__device__ inline complex64_t complex64add (const complex64_t x, const complex64_t y) /*{{{*/
{
	complex64_t tmp;

	tmp.real = x.real + y.real;
	tmp.imag = x.imag + y.imag;

	return tmp;
}
/*}}}*/
__device__ inline complex64_t complex64mul (const complex64_t x, const complex64_t y) /*{{{*/
{
	complex64_t tmp;

	tmp.real = (x.real * y.real) - (x.imag * y.imag);
	tmp.imag = (x.imag * y.real) + (x.real * y.imag);
	return tmp;
}
/*}}}*/
__device__ inline int mandelbrot_calc (const complex64_t c) /*{{{*/
{
	int count = 1;
	complex64_t iter = c;

	while ((complex64abssq (iter) < MANDEL_ABSSQLIM) && (count < 256)) {
		iter = complex64add (complex64mul (iter, iter), c);
		count++;
	}
	return count;
}
/*}}}*/

/*{{{  __global__ void mandelbrot_kernel (const double *dparms, int *framebuffer, const int *iparms, const int *colouring)*/
/*
 *	This implements the GPU kernel, extracts various settings from parameters, calculates a single pixel and plants in framebuffer.
 */
__global__ void mandelbrot_kernel (const double *dparms, int *framebuffer, const int *iparms, const int *colouring)
{
	int j = (blockIdx.y * blockDim.y) + threadIdx.y;
	int i = (blockIdx.x * blockDim.x) + threadIdx.x;
	int width = iparms[0];
	int height = iparms[1];
	double y = (double)(j - (height / 2));
	double x = (double)(i - (width / 2));
	complex64_t c;
	int val, pixel;

	y = (y / dparms[2]) - dparms[1];
	x = (x / (dparms[2] * 2.0)) - dparms[0];
	c.real = x;
	c.imag = y;

	val = mandelbrot_calc (c);
	if (val >= 256) {
		val = 255;
	}
	pixel = colouring[(val + iparms[2]) & 0xff];
	/*
	pixel = 0xf0000000 >> (val * 2);

	if (pixel & 0xffffff) {
		// rotate low-order 24 bits by iparms[2]
		pixel = (pixel >> iparms[2]) | (pixel << (24 - iparms[2]));
	}
	*/

	framebuffer[(j * width) + i] = pixel;
}

/*}}}*/

static inline void real_cuda_allocmem (ocuda_devinfo_t *device, void **ainfo, const int *diparms) /*{{{*/
{
	mandelcuda_ainfo_t *minfo = (mandelcuda_ainfo_t *)dmem_alloc (sizeof (mandelcuda_ainfo_t));

	minfo->device = device;
	minfo->width = diparms[0];
	minfo->height = diparms[1];
	minfo->fbsize = minfo->width * minfo->height * sizeof (int);
	minfo->parmssize = 3 * sizeof (double);
	minfo->iparmssize = 3 * sizeof (int);
	minfo->coloursize = 240 * sizeof (int);

	cudaSetDevice (minfo->device->dnum);

	cudaMalloc ((void **)&minfo->dev_fb, minfo->fbsize);
	cudaMalloc ((void **)&minfo->dev_parms, minfo->parmssize);
	cudaMalloc ((void **)&minfo->dev_iparms, minfo->iparmssize);
	cudaMalloc ((void **)&minfo->dev_colour, minfo->coloursize);

	*ainfo = (void *)minfo;
}

/*}}}*/
static inline void real_cuda_freemem (void **ainfo) /*{{{*/
{
	mandelcuda_ainfo_t *minfo = (mandelcuda_ainfo_t *)(*ainfo);

	if (!minfo) {
		return;
	}

	cudaSetDevice (minfo->device->dnum);

	cudaFree (minfo->dev_colour);
	cudaFree (minfo->dev_iparms);
	cudaFree (minfo->dev_parms);
	cudaFree (minfo->dev_fb);

	dmem_release (minfo);
	*ainfo = NULL;
}

/*}}}*/
static inline void real_cuda_updatecolour (void **ainfo, const int *colouring) /*{{{*/
{
	mandelcuda_ainfo_t *minfo = (mandelcuda_ainfo_t *)(*ainfo);

	cudaSetDevice (minfo->device->dnum);
	cudaMemcpy (minfo->dev_colour, colouring, minfo->coloursize, cudaMemcpyHostToDevice);
	get_last_cuda_error ("cudaMemcpy(HostToDevice)");
}
/*}}}*/


static inline void real_cuda_mandelbrot (void **ainfo, const double *dparms, int *framebuffer, const int *diparms) /*{{{*/
{
	mandelcuda_ainfo_t *minfo = (mandelcuda_ainfo_t *)(*ainfo);
	int ydim = 16;
	int xdim = (minfo->device->max_threads_per_block / ydim);

	cudaSetDevice (minfo->device->dnum);
	cudaMemcpy (minfo->dev_parms, dparms, minfo->parmssize, cudaMemcpyHostToDevice);
	cudaMemcpy (minfo->dev_iparms, diparms, minfo->iparmssize, cudaMemcpyHostToDevice);
	get_last_cuda_error ("cudaMemcpy(HostToDevice)");

	dim3 dim_block (xdim, ydim);
	dim3 dim_grid (minfo->width / dim_block.x, minfo->height / dim_block.y);

	mandelbrot_kernel <<< dim_grid, dim_block >>> (minfo->dev_parms, minfo->dev_fb, minfo->dev_iparms, minfo->dev_colour);

	cudaMemcpy (framebuffer, minfo->dev_fb, minfo->fbsize, cudaMemcpyDeviceToHost);
	get_last_cuda_error ("cudaMemcpy(DeviceToHost)");
}

/*}}}*/


extern "C" {
	/* PROC C.cuda.allocmem (OCUDA.DEVINFO device, MANDELCUDA.AINFO ainfo, VAL []INT iparams) */
	__host__ void _cuda_allocmem (int *ws) { real_cuda_allocmem ((ocuda_devinfo_t *)(ws[0]), (void **)(ws[1]), (int *)(ws[2])); }

	/* PROC C.cuda.freemem (MANDELCUDA.AINFO ainfo) */
	__host__ void _cuda_freemem (int *ws) { real_cuda_freemem ((void **)(ws[0])); }

	/* PROC C.cuda.updatecolour (MANDELCUDA.AINFO ainfo, VAL []INT colouring) */
	__host__ void _cuda_updatecolour (int *ws) { real_cuda_updatecolour ((void **)(ws[0]), (int *)(ws[1])); }

	/* PROC C.cuda.mandelbrot (MANDELCUDA.AINFO ainfo, VAL []REAL64 settings, [][]INT fb, VAL []INT iparams) */
	__host__ void _cuda_mandelbrot (int *ws) {
		real_cuda_mandelbrot ((void **)(ws[0]), (double *)(ws[1]), (int *)(ws[3]), (int *)(ws[6])); }
}

