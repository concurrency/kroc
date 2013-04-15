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

#include <dmem_if.h>

#include "ocuda_ctypes.h"

#define MAX_GPU_INSTANCES	(8)
#undef OCUDA_DEBUG

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


static inline void real_cuda_init (int *d_count, int *sp) /*{{{*/
{
	int ndevices, i, dcnt;

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

	for (i=0,dcnt=0; dcnt<(*d_count); dcnt++) {
		cudaDeviceProp prop;
		int j;

		cudaGetDeviceProperties (&prop, dcnt);
		if (get_last_cuda_error ("cudaGetDeviceProperties()")) {
			continue;					/* just skip this one */
		}

		cudaSetDevice (dcnt);					/* create stream(s) in device context */
		devinfo[i].dnum = dcnt;
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
		devinfo[i].cmode = prop.computeMode;
		devinfo[i].global_mem = (prop.totalGlobalMem / 1024);
		devinfo[i].shared_mem_per_block = (prop.sharedMemPerBlock / 1024);
		devinfo[i].regs_per_block = prop.regsPerBlock;
		devinfo[i].warp_size = prop.warpSize;
		devinfo[i].mem_pitch = prop.memPitch;
		devinfo[i].max_threads_per_block = prop.maxThreadsPerBlock;
		for (j=0; j<3; j++) {
			devinfo[i].max_threads_dim[j] = prop.maxThreadsDim[j];
			devinfo[i].max_grid_size[j] = prop.maxGridSize[j];
		}
		devinfo[i].clock_rate = prop.clockRate;
		devinfo[i].total_const_mem = (prop.totalConstMem / 1024);
		devinfo[i].mproc_count = prop.multiProcessorCount;
		devinfo[i].max_threads_per_mproc = prop.maxThreadsPerMultiProcessor;
		devinfo[i].integrated = prop.integrated;
		devinfo[i].can_map_host_memory = prop.canMapHostMemory;
		devinfo[i].concur_kernels = prop.concurrentKernels;
		devinfo[i].async_engine_count = prop.asyncEngineCount;
		devinfo[i].pci_bus_id = prop.pciBusID;
		devinfo[i].pci_device_id = prop.pciDeviceID;
		devinfo[i].pci_domain_id = prop.pciDomainID;
		devinfo[i].tcc_driver = prop.tccDriver;
		devinfo[i].unified_addressing = prop.unifiedAddressing;
		devinfo[i].mem_clock_rate = prop.memoryClockRate;
		devinfo[i].mem_bus_width = prop.memoryBusWidth;
		devinfo[i].l2_cache_size = (prop.l2CacheSize / 1024);

#if defined(OCUDA_DEBUG)
		fprintf (stderr, "CUDA device %d: \"%s\" (%d.%d) init\n", devinfo[i].dnum, prop.name, (devinfo[i].cversion >> 16) & 0xffff, (devinfo[i].cversion & 0xffff));
#endif
		i++;
	}
	*d_count = i;

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


