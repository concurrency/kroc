/*
 *	ccsp.h -- CCSP header for use in external code
 *	Copyright (C) 2007 Carl Ritson <cgr@kent.ac.uk>
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
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef __CCSP_H
#define __CCSP_H

#define MT_DEFINES 1

#ifdef __GNUC__
#define _PACK_STRUCT __attribute__ ((packed))
#else
#warning "Unable to enforce alignment and packing on structures."
#define _PACK_STRUCT
#endif

#include "ccsp_config.h"
#include "ukcthreads_types.h"
#include "ccsp_consts.h"
#include "mobile_types.h"
#include "ccsp_pony.h"
#include "dmem_if.h"
#include "kiface.h"
#include "ccsp_if.h"
#include "ccsp_stats.h"

typedef struct _ccsp_sched_t {
	unsigned int    stack;
	word		cparam[5];
	void		*calltable[K_MAX_SUPPORTED];
	word		mdparam[32];
	unsigned int	index;
	unsigned int	id;
	unsigned int	cpu_factor;
	unsigned int	cpu_khz;
	int		signal_in;
	int		signal_out;
	void		*allocator;
	unsigned int	spin;
	word		pad[8];
} _PACK_STRUCT ccsp_sched_t;

#undef _PACK_STRUCT

#if defined(USE_TLS)
  extern __thread	ccsp_sched_t 	*_ccsp_scheduler;
  #define ccsp_scheduler		_ccsp_scheduler
#elif defined(ENABLE_MP)
  ccsp_sched_t 				*local_scheduler (void);
  #define ccsp_scheduler		(local_scheduler ())
#else
  extern ccsp_sched_t			*_ccsp_scheduler;
  #define ccsp_scheduler		_ccsp_scheduler
#endif

extern void			**_ccsp_calltable;

#endif	/* !__CCSP_H */

