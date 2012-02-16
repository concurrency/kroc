/*
 *	Scheduler defintions (i386 kernel)
 *	Copyright (C) 1998 Jim Moores
 *	Modifications Copyright (C) 1999-2002 Fred Barnes  <frmb@kent.ac.uk>
 *	Modifications Copyright (C) 2007 Carl Ritson <cgr@kent.ac.uk>
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef __KERNEL_H
#define __KERNEL_H

#include <sched_types.h>

/* PUBLIC variables that are used elsewhere in the run time system. */
#if defined(USE_TLS)
  extern __thread sched_t 	*_ccsp_scheduler;
  #define _local_scheduler	_ccsp_scheduler
#elif defined(ENABLE_MP)
  #define _local_scheduler	local_scheduler ();
#else
  extern sched_t			*_ccsp_scheduler;
  #define _local_scheduler	_ccsp_scheduler
#endif

extern ccsp_global_t 		_ccsp;
#define enabled_threads 	(_ccsp.enabled_threads)
#define idle_threads		(_ccsp.idle_threads)
#define sleeping_threads	(_ccsp.sleeping_threads)
#define schedulers		(_ccsp.schedulers)
#define ccsp_shutdown		(_ccsp.shutdown)
#define ccsp_calltable		(_ccsp.calltable)

extern void 			**_ccsp_calltable;

sched_t *local_scheduler (void);
void ccsp_kernel_init (void);

int not_on_any_queue (unsigned int ws_base, unsigned int ws_limit);
int remove_from_any_queue (unsigned int ws_base, unsigned int ws_limit);
void do_queue_process (word *process);

#define SCHED_POLICY_US 1		/* suspend other and reschedule invoking process */
#define SCHED_POLICY_OTHER 2		/* suspend invoking process and reschedule other */
#define SCHED_POLICY_NEITHER_US 3	/* suspend both, us first on the run-queue */
#define SCHED_POLICY_NEITHER_OTHER 4	/* suspend both, other first on the run-queue */

#define DEFAULT_SCHED_POLICY SCHED_POLICY_US

#if defined(RMOX_BUILD)
  extern word *inttab[RMOX_NUM_INTERRUPTS];
  extern volatile word intcount[RMOX_NUM_INTERRUPTS];
#endif

#endif	/* !__KERNEL_H */

