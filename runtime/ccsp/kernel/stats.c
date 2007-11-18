/*
 *	CCSP kernel statistics
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/*{{{  includes*/
#ifdef RMOX_BUILD
	#include <rmox_if.h>
#else	/* !RMOX_BUILD */
	#include <stdlib.h>
	#include <stdio.h>
#endif	/* !RMOX_BUILD */

#include <kernel.h>
#include <ccsp_stats.h>
/*}}}*/

int ccsp_get_stats (int version, ccsp_stats_t *stats) {
	if (version == CCSP_STATS_VERSION_1_00) {
		unsigned long proc_start	= 0;
		unsigned long proc_end		= 0;
		unsigned long startp		= 0;
		unsigned long endp		= 0;
		int threads 			= att_val (&(enabled_threads));
		int i;

		for (i = 0; i < MAX_RUNTIME_THREADS; ++i) {
			sched_t *sched = schedulers[i];
			if (threads & (1 << i)) {
				proc_start	+=
					(unsigned long) atw_val (&(sched->stats.proc_start));
				proc_end	+=
					(unsigned long) atw_val (&(sched->stats.proc_end));
				startp		+=
					(unsigned long) atw_val (&(sched->stats.startp));
				endp		+=
					(unsigned long) atw_val (&(sched->stats.endp));
			}
		}

		stats->proc_start	= proc_start;
		stats->proc_end		= proc_end;
		stats->startp		= startp;
		stats->endp		= endp;

		return 0;
	} else {
		return -1;
	}
}

/* #PRAGMA EXTERNAL "C.ccsp.get.process.count (INT count) = 0" */
void _ccsp_get_process_count (int ws[]) {
	int *count = (int *) ws[0];

	ccsp_stats_t stats;

	if (ccsp_get_stats (CCSP_STATS_VERSION_1_00, &stats) == -1) {
		*count = -1;
	} else {
		*count = (stats.proc_start - stats.proc_end)
		         + (stats.startp - stats.endp);
	}
}
