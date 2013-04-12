/*
 *	ocuda_extcode.cu -- occam-pi/CUDA interface glue code
 *	Copyright (C) 2013 Fred Barnes <frmb@kent.ac.uk>
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

#include "ocuda_ctypes.h"

#define MAX_GPU_INSTANCES	(8)

/* per-thread context */
static ocuda_devinfo_t devinfo[MAX_GPU_INSTANCES];

int __get_last_cuda_error (const char *msg, const char *file, const int line) /*{{{*/
{
	cudaError_t err = cudaGetLastError ();

	if (err != cudaSuccess) {
		fprintf (stderr, "%s(%d): CUDA error: %s: (%d): %s\n", file, line, msg, (int)err, cudaGetErrorString (err));
		return 1;
	}
	return 0;
}
/*}}}*/

#define get_last_cuda_error(MSG) __get_last_cuda_error (MSG, __FILE__, __LINE__)

static inline void real_cuda_init (int *d_count, int *sp) /*{{{*/
{
	int ndevices, i;

	*sp = get_last_cuda_error ("initialise");
	if (*sp) {
		return;
	}

	cudaGetDeviceCount (&ndevices);
	if (get_last_cuda_error ("cudaGetDeviceCount()")) {
		return;
	}

	if (*d_count == 0) {
		*d_count = ndevices;
	}

	for (i=0; i<(*d_count); i++) {
		cudaDeviceProp prop;

		cudaGetDeviceProperties (&prop, i);
		if (get_last_cuda_error ("cudaGetDeviceProperties()")) {
			return;
		}

		cudaSetDevice (i);					/* create stream(s) in device context */
		devinfo[i].dnum = i;
		// cudaStreamCreate (&(devinfo[i].stream0));
		// cudaStreamCreate (&(devinfo[i].stream1));
		// devinfo[i].stream0 = 0;
		// devinfo[i].stream1 = 0;
		// if (get_last_cuda_error ("cudaCreateStream()")) {
		// 	return;
		// }

		devinfo[i].cversion = ((prop.major << 16) | (prop.minor & 0xffff));
		if (strlen (prop.name) > 64) {
			memcpy (devinfo[i].dname, prop.name, 64);
			devinfo[i].dname_len = 64;
		} else {
			devinfo[i].dname_len = strlen (prop.name);
			memcpy (devinfo[i].dname, prop.name, devinfo[i].dname_len);
		}
		fprintf (stderr, "CUDA device %d: \"%s\" (%d.%d) init\n", devinfo[i].dnum, prop.name, (devinfo[i].cversion >> 16) & 0xffff, (devinfo[i].cversion & 0xffff));
	}

	return;
}
/*}}}*/
static inline void real_cuda_devinfo (int dnum, ocuda_devinfo_t *result, int *res) /*{{{*/
{
	if ((dnum < 0) || (dnum >= MAX_GPU_INSTANCES)) {
		memset (result, 0, sizeof (ocuda_devinfo_t));
		*res = 1;
		return;
	}
	memcpy (result, &(devinfo[dnum]), sizeof (ocuda_devinfo_t));
	*res = 0;
}
/*}}}*/

extern "C" {
	/* PROC C.cuda.init (INT d.count, RESULT INT res) */
	__host__ void _cuda_init (int *ws) { real_cuda_init ((int *)(ws[0]), (int *)(ws[1])); }
	/* PROC C.cuda.devinfo (VAL INT dnum, RESULT OCUDA.DEVINFO data, RESULT INT res) */
	__host__ void _cuda_devinfo (int *ws) { real_cuda_devinfo ((int)(ws[0]), (ocuda_devinfo_t *)(ws[1]), (int *)(ws[2])); }
}


