/*
 *	ocuda_ctypes.h -- occam-pi/CUDA C types.
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

#ifndef __OCUDA_CTYPES_H
#define __OCUDA_CTYPES_H

/* defines a CUDA device (basic) */

typedef struct TAG_ocuda_devinfo {
	int dnum;
	char dname[64];
	int dname_len;
	int cversion;
	int cmode;

	int global_mem;
	int shared_mem_per_block;
	int regs_per_block;
	int warp_size;
	int mem_pitch;
	int max_threads_per_block;
	int max_threads_dim[3];
	int max_grid_size[3];
	int clock_rate;
	int total_const_mem;
	int mproc_count;
	int max_threads_per_mproc;
	int integrated;
	int can_map_host_memory;
	int concur_kernels;
	int async_engine_count;
	int pci_bus_id;
	int pci_device_id;
	int pci_domain_id;
	int tcc_driver;
	int unified_addressing;
	int mem_clock_rate;
	int mem_bus_width;
	int l2_cache_size;
} ocuda_devinfo_t;

/* available to other CUDA code: */

int __get_last_cuda_error (const char *, const char *, const int);
#define get_last_cuda_error(MSG) __get_last_cuda_error (MSG, __FILE__, __LINE__)


#endif	/* !__OCUDA_CTYPE_H */

