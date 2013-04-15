/*
 *	occoids_cuda.cu -- boid GPU code, 15th version
 *	Copyright (C) 2012-2013 Fred Barnes, University of Kent <frmb@kent.ac.uk>
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
#include <math_functions.h>
#include <math_constants.h>

#include <dmem_if.h>
#include <ocuda_ctypes.h>

#include "occoids_ctypes.h"


/*{{{  dev_jobinfo_t: instance parameter (per chunk of work)*/

/* (this avoids trying to pass too many parameters to the GPU kernel) */

typedef struct TAG_dev_jobinfo {
	int astart, acount;		/* start and count in the global agent array */
	int act_cycle;			/* activity cycle (0 or 1) */
} dev_jobinfo_t;

/*}}}*/
/*{{{  constjobinfo_t: constant information for a series of runs*/

typedef struct TAG_constjobinfo {
	genparms_t *host_parms;
	int parms_size;

	agent_info_t *host_all_agents[2];
	int all_agents_size;

	int *host_viewable;
	int viewable_size;

	int *host_viewcount;
	int viewcount_size;

	int *host_viewmap;
	int viewmap_size;
} constjobinfo_t;

/*}}}*/
/*{{{  dev_constjobinfo_t: constant job information pointers*/

typedef struct TAG_dev_constjobinfo {
	genparms_t *dev_parms;			/* simulation parameters */
	agent_info_t *dev_all_agents0;		/* agent data */
	agent_info_t *dev_all_agents1;		/* agent data */
	int *dev_viewable;			/* viewable information */
	int *dev_viewcount;			/* how many in each location's viewer */
	int *dev_viewmap;			/* view-map (how the world is connected) */
} dev_constjobinfo_t;

/*}}}*/

__device__ inline float angle_diff (const float a, const float b) /*{{{*/
{
	float r = a - b;

	if (r < -CUDART_PI_F) {
		r += (2.0 * CUDART_PI_F);
	} else if (r > CUDART_PI_F) {
		r -= (2.0 * CUDART_PI_F);
	}
	r = fabsf (r);
	return r;
}
/*}}}*/
__device__ inline float vec_mag2 (vector_t vec) /*{{{*/
{
	return (vec.x * vec.x) + (vec.y * vec.y);
}
/*}}}*/
__device__ inline void boid_combined_rules (const genparms_t *devparms, int my_id, agent_info_t *result, agent_info_t *all_agents, int nagents, int *viewable, int nview, int *viewcount, int nvcount, int *viewmap, int vmstride) /*{{{*/
{
	float my_angle = atan2f (all_agents[my_id].vel.x, all_agents[my_id].vel.y);
	float my_vel2 = vec_mag2 (all_agents[my_id].vel);
	bool not_moving = (my_vel2 < devparms->epsilon);

	int nboids = 0, nobst = 0;

	vector_t com = {0.0, 0.0};			// centre of mass
	vector_t rpush = {0.0, 0.0};			// repulsion push
	vector_t opush = {0.0, 0.0};			// obstacle push
	vector_t perc_vel = {0.0, 0.0};			// perceived velocity

	vector_t accel = {0.0, 0.0};

	int i, v;
	int cell = all_agents[my_id].locn & 0x00ffffff;
	int boundary = (all_agents[my_id].locn >> 28) & 0x0f;

	result->type_id = all_agents[my_id].type_id;
	result->locn = all_agents[my_id].locn;
	result->vel = all_agents[my_id].vel;
	result->radius = all_agents[my_id].radius;
	result->colour = all_agents[my_id].colour;

	/* for each cell in the viewable space */
	for (v = 0; v <= vmstride; v++) {
		int cidx, cvcount, cvstart;

		if (v < vmstride) {
			cidx = viewmap[(cell * vmstride) + v];
		} else {
			cidx = cell;
		}

		cvcount = viewcount[cidx];
		cvstart = (cidx * devparms->vperboid);

		/* for each thing in the viewable cell */
		for (i=0; i<cvcount; i++) {
			int a_id = viewable[cvstart + i];

			if (a_id == my_id) {
				/* can't see ourselves */
			} else {
				vector_t rel_pos;
				bool visible;
				int atype = (all_agents[a_id].type_id >> 24) & 0xff;

				/*{{{  calculate relative position*/
				rel_pos.x = all_agents[a_id].pos.x - all_agents[my_id].pos.x;
				rel_pos.y = all_agents[a_id].pos.y - all_agents[my_id].pos.y;

				if ((boundary & 0x05) == 0) {
					/* not at left/right edge */
				} else if (rel_pos.x > 2.0) {
					/* can't possibly be this much */
					rel_pos.x -= (float)devparms->width;
				} else if (rel_pos.x < -2.0) {
					rel_pos.x += (float)devparms->width;
				}
				if ((boundary & 0x0a) == 0) {
					/* not at top/bottom edge */
				} else if (rel_pos.y > 2.0) {
					/* can't possibly be this much */
					rel_pos.y -= (float)devparms->height;
				} else if (rel_pos.y < -2.0) {
					rel_pos.y += (float)devparms->height;
				}
				/*}}}*/
				/*{{{  is it visible?*/
				if (vec_mag2 (rel_pos) > devparms->vrad_sq) {
					visible = false;		/* too far away */
				} else if (atype == 2) {
					visible = true;			/* obstacles always visible */
				} else if (not_moving) {
					visible = true;			/* not moving, look around */
				} else if (angle_diff (atan2f (rel_pos.x, rel_pos.y), my_angle) > devparms->vmaxdiff) {
					visible = false;		/* out of field-of-view */
				} else {
					visible = true;
				}
				/*}}}*/

				if (visible && (atype == 1)) {
					/*{{{  boid*/
					nboids++;
					/* centre of mass (1) */
					com.x += rel_pos.x;
					com.y += rel_pos.y;
					/* repulsion (1) */
					if (vec_mag2 (rel_pos) < devparms->rdist_sq) {
						rpush.x -= rel_pos.x;
						rpush.y -= rel_pos.y;
					}
					/* mean velocity (1) */
					perc_vel.x += all_agents[a_id].vel.x;
					perc_vel.y += all_agents[a_id].vel.y;
					/*}}}*/
				} else if (visible && (atype == 2)) {
					/*{{{  obstacle*/
					float dist;

					nobst++;
					/* obstacle rule (1) */
					dist = sqrtf (vec_mag2 (rel_pos)) - all_agents[a_id].radius;

					if (dist < 0.0) {
						/* past soft threshold, push back hard */
						opush.x -= rel_pos.x;
						opush.y -= rel_pos.y;
					} else if (dist < devparms->sthres) {
						/* inside soft threshold, push back a variable amount */
						float scl = (1.0 - (dist / devparms->sthres));

						opush.x -= (rel_pos.x * scl);
						opush.y -= (rel_pos.y * scl);
					}
					/*}}}*/
				}
			}
		}
	}

	/*{{{  centre of mass (2)*/
	if (nboids > 0) {
		com.x /= (float)nboids;
		com.y /= (float)nboids;

		accel.x += (com.x / devparms->cmfrac);
		accel.y += (com.y / devparms->cmfrac);
	}

	/*}}}*/
	/*{{{  repulsion (2)*/
	accel.x += (rpush.x / devparms->rfrac);
	accel.y += (rpush.y / devparms->rfrac);

	/*}}}*/
	/*{{{  mean velocity (2)*/
	if (nboids > 0) {
		perc_vel.x /= (float)nboids;
		perc_vel.y /= (float)nboids;
	}

	perc_vel.x -= result->vel.x;
	perc_vel.y -= result->vel.y;
	accel.x += (perc_vel.x / devparms->mvfrac);
	accel.y += (perc_vel.y / devparms->mvfrac);

	/*}}}*/
	/*{{{  obstacle rule (2)*/
	if (nobst > 0) {
		accel.x += (opush.x / devparms->ofrac);
		accel.y += (opush.y / devparms->ofrac);
	}

	/*}}}*/
	/*{{{  accelerate!*/
	result->vel.x += (accel.x / devparms->saccel);
	result->vel.y += (accel.y / devparms->saccel);

	if (fabsf (result->vel.x) < devparms->epsilon) {
		result->vel.x = 0.0;
	}
	if (fabsf (result->vel.y) < devparms->epsilon) {
		result->vel.y = 0.0;
	}

	my_vel2 = vec_mag2 (result->vel);
	if (my_vel2 > devparms->slimit_sq) {
		float tmag = my_vel2 / devparms->slimit_sq;

		result->vel.x = result->vel.x / tmag;
		result->vel.y = result->vel.y / tmag;
	}

	/*}}}*/
	/*{{{  compute new position and movement info*/
	result->pos.x = all_agents[my_id].pos.x + result->vel.x;
	result->pos.y = all_agents[my_id].pos.y + result->vel.y;

#if 0
	/* round new position into grid */
	if (result->pos.x < 0.0) {
		result->pos.x += (float)devparms->width;
	} else if (result->pos.x >= (float)devparms->width) {
		result->pos.x -= (float)devparms->width;
	}
	if (result->pos.y < 0.0) {
		result->pos.y += (float)devparms->height;
	} else if (result->pos.y >= (float)devparms->height) {
		result->pos.y -= (float)devparms->height;
	}

	/* compute delta -- note: always positive if wrapping */
	int xdelta = (int)(truncf (result->pos.x) - truncf (all_agents[my_id].pos.x));
	int ydelta = (int)(truncf (result->pos.y) - truncf (all_agents[my_id].pos.y));

	if ((xdelta == -1) || (xdelta > 2)) {
		/* moving left */
		result->locn |= 0x01000000;
	} else if ((xdelta == 1) || (xdelta < -2)) {
		/* moving right */
		result->locn |= 0x04000000;
	}
	if ((ydelta == -1) || (ydelta > 2)) {
		/* moving up */
		result->locn |= 0x02000000;
	} else if ((ydelta == 1) || (ydelta < -2)) {
		/* moving down */
		result->locn |= 0x08000000;
	}
#endif

	/*}}}*/
}
/*}}}*/
__global__ void boid_gpu5_kernel2 (dev_constjobinfo_t cinfo, dev_jobinfo_t jinfo) /*{{{*/
{
	int i = (blockIdx.x * blockDim.x) + threadIdx.x;
	int my_id;
	agent_info_t *arry, *rarry;

	if (i >= jinfo.acount) {
		return;				/* out of range */
	}

	my_id = jinfo.astart + i;
	if (jinfo.act_cycle == 0) {
		arry = cinfo.dev_all_agents0;
		rarry = cinfo.dev_all_agents1;
	} else {
		arry = cinfo.dev_all_agents1;
		rarry = cinfo.dev_all_agents0;
	}

	if (((arry[my_id].type_id >> 24) & 0xff) == 1) {
		boid_combined_rules (cinfo.dev_parms, my_id, &(rarry[my_id]),
					arry, cinfo.dev_parms->maxagents,
					cinfo.dev_viewable, cinfo.dev_parms->viewsize,
					cinfo.dev_viewcount, cinfo.dev_parms->vperboid,
					cinfo.dev_viewmap, cinfo.dev_parms->ndir);
	}
}
/*}}}*/

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

static inline void real_cuda_alloc_global (ocuda_devinfo_t *device, int all_agents_addr_0, int all_agents_addr_1, int viewable_addr, int viewcount_addr, int viewmap_addr, int parms_addr, int *cpuptr, int *gpuptr) /*{{{*/
{
	constjobinfo_t *hjob;
	dev_constjobinfo_t *djob;

	cudaSetDevice (device->dnum);

	hjob = (constjobinfo_t *)malloc (sizeof (constjobinfo_t));
	if (!hjob) {
		fprintf (stderr, "real_cuda_alloc_global(): malloc() failed!\n");
		*cpuptr = 0;
		*gpuptr = 0;
		return;
	}

	hjob->host_parms = (genparms_t *)parms_addr;
	hjob->parms_size = sizeof (genparms_t);
	hjob->host_all_agents[0] = (agent_info_t *)all_agents_addr_0;
	hjob->host_all_agents[1] = (agent_info_t *)all_agents_addr_1;
	hjob->all_agents_size = (sizeof (agent_info_t) * hjob->host_parms->maxagents);
	hjob->host_viewable = (int *)viewable_addr;
	hjob->viewable_size = (sizeof (int) * hjob->host_parms->viewsize);
	hjob->host_viewcount = (int *)viewcount_addr;
	hjob->viewcount_size = (sizeof (int) * hjob->host_parms->nloc);
	hjob->host_viewmap = (int *)viewmap_addr;
	hjob->viewmap_size = (sizeof (int)  * hjob->host_parms->nloc * hjob->host_parms->ndir);

	*cpuptr = (int)hjob;

	djob = (dev_constjobinfo_t *)malloc (sizeof (dev_constjobinfo_t));
	if (!djob) {
		fprintf (stderr, "real_cuda_alloc_global(): malloc() failed!\n");
		*gpuptr = 0;
		return;
	}

	cudaMalloc ((void **)&(djob->dev_parms), hjob->parms_size);
	cudaMalloc ((void **)&(djob->dev_all_agents0), hjob->all_agents_size);
	cudaMalloc ((void **)&(djob->dev_all_agents1), hjob->all_agents_size);
	cudaMalloc ((void **)&(djob->dev_viewable), hjob->viewable_size);
	cudaMalloc ((void **)&(djob->dev_viewcount), hjob->viewcount_size);
	cudaMalloc ((void **)&(djob->dev_viewmap), hjob->viewmap_size);

	if (get_last_cuda_error ("malloc")) {
		*gpuptr = 0;
		free (djob);
		return;
	}

	*gpuptr = (int)djob;

#if 0
fprintf (stderr, "real_cuda_alloc_global(): dev_all_agents0 = %p, dev_all_agents1 = %p, host_all_agents[0] = %p, host_all_agents[1] = %p, maxagents = %d, all_agents_size = %d\n",
		djob->dev_all_agents0, djob->dev_all_agents1, hjob->host_all_agents[0], hjob->host_all_agents[1], hjob->host_parms->maxagents, hjob->all_agents_size);
#endif

	return;
}
/*}}}*/
static inline void real_cuda_free_global (ocuda_devinfo_t *device, int *cpuptr, int *gpuptr) /*{{{*/
{
	constjobinfo_t *hjob = (constjobinfo_t *)(*cpuptr);
	dev_constjobinfo_t *djob = (dev_constjobinfo_t *)(*gpuptr);

	cudaSetDevice (device->dnum);

	if (djob) {
		if (djob->dev_all_agents0) {
			cudaFree (djob->dev_all_agents0);
		}
		if (djob->dev_all_agents1) {
			cudaFree (djob->dev_all_agents1);
		}
		if (djob->dev_viewcount) {
			cudaFree (djob->dev_viewcount);
		}
		if (djob->dev_viewable) {
			cudaFree (djob->dev_viewable);
		}
		if (djob->dev_viewmap) {
			cudaFree (djob->dev_viewmap);
		}
		if (djob->dev_parms) {
			cudaFree (djob->dev_parms);
		}

		free (djob);
		*gpuptr = 0;
	}
	if (hjob) {
		free (hjob);
		*cpuptr = 0;
	}

	return;
}
/*}}}*/
static inline void real_cuda_copy_viewmap (ocuda_devinfo_t *device, int cpuptr, int gpuptr) /*{{{*/
{
	constjobinfo_t *hjob = (constjobinfo_t *)cpuptr;
	dev_constjobinfo_t *djob = (dev_constjobinfo_t *)gpuptr;

	cudaSetDevice (device->dnum);
	get_last_cuda_error ("setdevice");

	cudaMemcpy (djob->dev_viewmap, hjob->host_viewmap, hjob->viewmap_size, cudaMemcpyHostToDevice);
	get_last_cuda_error ("memcpy");

	return;
}
/*}}}*/
static inline void real_cuda_copy_global (ocuda_devinfo_t *device, int act_cycle, int cpuptr, int gpuptr) /*{{{*/
{
	constjobinfo_t *hjob = (constjobinfo_t *)cpuptr;
	dev_constjobinfo_t *djob = (dev_constjobinfo_t *)gpuptr;

	cudaSetDevice (device->dnum);
	get_last_cuda_error ("setdevice");

#if 0
fprintf (stderr, "real_cuda_copy_global(): copying!\n");
#endif
	if (act_cycle == 0) {
		cudaMemcpy (djob->dev_all_agents0, hjob->host_all_agents[0], hjob->all_agents_size, cudaMemcpyHostToDevice);
	} else {
		cudaMemcpy (djob->dev_all_agents1, hjob->host_all_agents[1], hjob->all_agents_size, cudaMemcpyHostToDevice);
	}
	cudaMemcpy (djob->dev_viewable, hjob->host_viewable, hjob->viewable_size, cudaMemcpyHostToDevice);
	cudaMemcpy (djob->dev_viewcount, hjob->host_viewcount, hjob->viewcount_size, cudaMemcpyHostToDevice);
	cudaMemcpy (djob->dev_parms, hjob->host_parms, hjob->parms_size, cudaMemcpyHostToDevice);
	get_last_cuda_error ("memcpy");

#if 0
	if (get_last_cuda_error ("memcpy")) {
		fprintf (stderr, "real_cuda_copy_global(): arf: dev_all_agents=%p, dev_viewable=%p, dev_viewcount=%p, dev_parms=%p\n",
				djob->dev_all_agents, djob->dev_viewable, djob->dev_viewcount, djob->dev_parms);
	}
#endif

	return;
}
/*}}}*/
static inline void real_cuda_copyback_global (ocuda_devinfo_t *device, int act_cycle, int start, int count, int cpuptr, int gpuptr) /*{{{*/
{
	constjobinfo_t *hjob = (constjobinfo_t *)cpuptr;
	dev_constjobinfo_t *djob = (dev_constjobinfo_t *)gpuptr;
	int copysize = count * sizeof (agent_info_t);

	cudaSetDevice (device->dnum);
	get_last_cuda_error ("setdevice");

	if (act_cycle == 0) {
#if 0
fprintf (stderr, "real_cuda_copyback_global(): dst=%p, src=%p, size=%d\n", &(hjob->host_all_agents[1][start]), &(djob->dev_all_agents1[start]), copysize);
#endif
		cudaMemcpy (&(hjob->host_all_agents[1][start]), &(djob->dev_all_agents1[start]), copysize, cudaMemcpyDeviceToHost);
	} else {
		cudaMemcpy (&(hjob->host_all_agents[0][start]), &(djob->dev_all_agents0[start]), copysize, cudaMemcpyDeviceToHost);
	}
	if (get_last_cuda_error ("memcpy")) {
		fprintf (stderr, "act_cycle = %d, dev_all_agents0 = %p, dev_all_agents1 = %p, host_all_agents[0] = %p, host_all_agents[1] = %p, all_agents_size = %d\n",
				act_cycle, djob->dev_all_agents0, djob->dev_all_agents1, hjob->host_all_agents[0],
				hjob->host_all_agents[1], hjob->all_agents_size);
	}
	return;
}
/*}}}*/
static inline void real_cuda_doboids1 (ocuda_devinfo_t *device, int act_cycle, int astart0, int acount0, int gpuptr) /*{{{*/
{
	dev_jobinfo_t djob0;
	dev_constjobinfo_t *cjinfo = (dev_constjobinfo_t *)gpuptr;

	cudaSetDevice (device->dnum);

	/* populate for-device structure */

	djob0.astart = astart0;
	djob0.acount = acount0;
	djob0.act_cycle = act_cycle;

	/* go run it! */
	int threadsPerBlock = 256;
	int blocksPerGrid0 = (acount0 + threadsPerBlock - 1) / threadsPerBlock;

#if 0
fprintf (stderr, "real_cuda_doboids1 [%d]: launching kernel (%d,%d) args (->dev_all_agents=%p, ->nagents=%d)\n", device,
			blocksPerGrid0, threadsPerBlock, cjinfo->dev_all_agents, djob0.nagents);
#endif
	boid_gpu5_kernel2 <<< blocksPerGrid0, threadsPerBlock >>> (*cjinfo, djob0);
	// boid_gpu5_kernel2 <<< blocksPerGrid0, threadsPerBlock, 0, devinfo[device].stream0 >>> (*cjinfo, djob0);
	// cudaStreamSynchronize (devinfo[device].stream0);
	get_last_cuda_error ("exec");

	/* copy the results back */
	// cudaMemcpyAsync (ljob0.host_res_vel, djob0.dev_res_vel, ljob0.res_vel_size, cudaMemcpyDeviceToHost, devinfo[device].stream0);
	// cudaMemcpy (ljob0.host_res_vel, djob0.dev_res_vel, ljob0.res_vel_size, cudaMemcpyDeviceToHost);
	// cudaStreamSynchronize (devinfo[device].stream0);
	// get_last_cuda_error ("memcpy");

	// cudaFree (djob0.dev_res_vel);

	return;
}
/*}}}*/
static inline void real_cuda_runcycle (ocuda_devinfo_t *device, int act_cycle, int start, int count, int cpuptr, int gpuptr) /*{{{*/
{
	real_cuda_copy_global (device, act_cycle, cpuptr, gpuptr);
	real_cuda_doboids1 (device, act_cycle, start, count, gpuptr);
	real_cuda_copyback_global (device, act_cycle, start, count, cpuptr, gpuptr);
}
/*}}}*/

extern "C" {
	/* PROC C.cuda.alloc.global (OCUDA.DEVINFO device, VAL INT all.agents.addr.0, all.agents.addr.1, viewable.addr, viewcount.addr, viewmap.addr, parms.addr, RESULT CPUPTR cpu, gpu) */
	__host__ void _cuda_alloc_global (int *ws) { real_cuda_alloc_global ((ocuda_devinfo_t *)(ws[0]), (int)(ws[1]), (int)(ws[2]), (int)(ws[3]), (int)(ws[4]), (int)(ws[5]), (int)(ws[6]), (int *)(ws[7]), (int *)(ws[8])); }
	/* PROC C.cuda.free.global (OCUDA.DEVINFO device, CPUPTR cpu, CPUPTR gpu) */
	__host__ void _cuda_free_global (int *ws) { real_cuda_free_global ((ocuda_devinfo_t *)(ws[0]), (int *)(ws[1]), (int *)(ws[2])); }
	/* PROC C.cuda.copy.global (OCUDA.DEVINFO device, VAL CPUPTR cpu, gpu) */
	__host__ void _cuda_copy_viewmap (int *ws) { real_cuda_copy_viewmap ((ocuda_devinfo_t *)(ws[0]), (int)(ws[1]), (int)(ws[2])); }
	/* PROC C.cuda.copy.global (OCUDA.DEVINFO device, VAL INT act.cycle, VAL CPUPTR cpu, gpu) */
	__host__ void _cuda_copy_global (int *ws) { real_cuda_copy_global ((ocuda_devinfo_t *)(ws[0]), (int)(ws[1]), (int)(ws[2]), (int)(ws[3])); }
	/* PROC [BC].cuda.doboids1 (OCUDA.DEVINFO device, VAL INT act.cycle, astart0, acount0, VAL CPUPTR gpu) */
	__host__ void _cuda_doboids1 (int *ws) { real_cuda_doboids1 ((ocuda_devinfo_t *)(ws[0]), (int)(ws[1]), (int)(ws[2]), (int)(ws[3]), (int)(ws[4])); }
	/* PROC c.cuda.copyback.global (OCUDA.DEVINFO device, VAL INT act.cycle, astart, acount, VAL CPUPTR cpu, gpu) */
	__host__ void _cuda_copyback_global (int *ws) { real_cuda_copyback_global ((ocuda_devinfo_t *)(ws[0]), (int)(ws[1]), (int)(ws[2]), (int)(ws[3]), (int)(ws[4]), (int)(ws[5])); }

	/* PROC [BC].cuda.runcycle (OCUDA.DEVINFO device, VAL INT act.cycle, astart0, acount0, VAL CPUPTR cpu, gpu) */
	__host__ void _cuda_runcycle (int *ws) { real_cuda_runcycle ((ocuda_devinfo_t *)(ws[0]), (int)(ws[1]), (int)(ws[2]), (int)(ws[3]), (int)(ws[4]), (int)(ws[5])); }
}

