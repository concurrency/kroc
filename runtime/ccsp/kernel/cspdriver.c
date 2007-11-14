/*
 *	cspdriver.c -- code to utilise the linux cspdriver
 *	Copyright (C) 2002 Fred Barnes <frmb2@ukc.ac.uk>
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

#ifndef OOS_BUILD
#ifdef LINUX_CSPDRIVER

#include <stdio.h>
#include <string.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <errno.h>

#include <rts.h>
#include <sched_consts.h>
#include <sched_types.h>
#include <arch/cspioctl.h>


/* grotty.. */
extern sync_type sf;

int cspdriver_enabled;		/* scheduler and other things can test this for run-time support */

static int cdrv_fd;
static int realtime;


/*
 *	void initialise_cspdriver (void)
 *	initialises this stuff
 */
void initialise_cspdriver (void)
{
	char *path = getenv ("CSPDRIVER_PATH");
	char *trealtime = getenv("CSPDRIVER_REALTIME");

	realtime = (trealtime != NULL);

	if (path && (!strcmp (path, "NULL") || !strcmp (path, "null"))) {
		cspdriver_enabled = 0;
		cdrv_fd = -1;
	} else if (!path) {
		cdrv_fd = open ("/dev/cspdrv", O_RDWR);
		cspdriver_enabled = (cdrv_fd >= 0);
	} else {
		cdrv_fd = open (path, O_RDWR);
		cspdriver_enabled = (cdrv_fd >= 0);
		if (!cspdriver_enabled) {
			cdrv_fd = open ("/dev/cspdrv", O_RDWR);
			cspdriver_enabled = (cdrv_fd >= 0);
		}
	}
	return;
}


/*
 *	void shutdown_cspdriver (void)
 *	closes the device
 */
void shutdown_cspdriver (void)
{
	if (cspdriver_enabled) {
		close (cdrv_fd);
		cspdriver_enabled = 0;
	}
	return;
}


/*
 *	int cspdriver_safe_pause (unsigned long timeout)
 *	initiates a kernel pause-for-event
 */
int cspdriver_safe_pause (unsigned long timeout)
{
	int i;
	ucsp_waitinfo wi;

	wi.timeout = timeout;
	wi.sf_addr = (int *)&sf.u.combined_sync;
	if (realtime) {
		i = ioctl (cdrv_fd, IOCTL_REALTIME_PAUSE, (char *)&wi);
	} else {
		i = ioctl (cdrv_fd, IOCTL_SAFE_PAUSE, (char *)&wi);
	}
	return i;
}


#ifdef BLOCKING_SYSCALLS
/*
 *	int cspdriver_initialise_bsc_support (void)
 *	initialises blocking sys-call support for the CSP driver
 */
int cspdriver_initialise_bsc_support (void)
{
	ucsp_bsciinfo ii;
	unsigned int osync, nsync;

	if (!cspdriver_enabled) {
		errno = ENOSYS;
		return -1;
	}
	ii.sf_addr = (unsigned int *)&sf.u.combined_sync;
	osync = (unsigned int)(sf.u.combined_sync);
	sf.u.s.block_sync = 1;
	nsync = (unsigned int)(sf.u.combined_sync);
	sf.u.s.block_sync = 0;

	if (osync == nsync) {
		osync = (unsigned int)(sf.u.combined_sync);
		sf.u.s.block_sync = 1;
	}
	ii.bsc_bits = (osync ^ nsync);
	ii.bsc_mask = (osync ^ nsync);

	return ioctl (cdrv_fd, IOCTL_BSC_SETUP, (char *)&ii);
}


/*
 *	int cspdriver_bsc_callin (void **addr, int length, int resync)
 *	callin for a clone process
 */
int cspdriver_bsc_callin (void **addr, int length, int resync)
{
	ucsp_bscdinfo di;
	int i;

	if (!cspdriver_enabled) {
		errno = ENOSYS;
		return -1;
	}
	di.address = *addr;
	di.length = length;
	do {
		di.resync = resync;
		i = ioctl (cdrv_fd, IOCTL_BSC_CALLIN, (char *)&di);
		/* if we were interrupted, must reset the resynch flag -- well, don't *have* to, but better.. */
		resync = 0;
	} while ((i < 0) && (errno == EINTR));
	*addr = di.address;
	return i;
}


/*
 *	int cspdriver_bsc_dispatch (void *addr, int length)
 *	dispatch for kernel process
 */
int cspdriver_bsc_dispatch (void *addr, int length)
{
	ucsp_bscdinfo di;
	int i;

	if (!cspdriver_enabled) {
		errno = ENOSYS;
		return -1;
	}
	di.address = addr;
	di.length = length;
	do {
		i = ioctl (cdrv_fd, IOCTL_BSC_DISPATCH, (char *)&di);
	} while ((i < 0) && (errno == EINTR));
	return i;
}


/*
 *	int cspdriver_bsc_num_buffers (void)
 *	gets the number of buffers in use by the underlying device
 */
int cspdriver_bsc_num_buffers (void)
{
	if (!cspdriver_enabled) {
		errno = ENOSYS;
		return -1;
	}
	return ioctl (cdrv_fd, IOCTL_BSC_GET_BUFCOUNT, NULL);
}
#endif	/* BLOCKING_SYSALLS */

#endif	/* LINUX_CSPDRIVER */
#endif	/* !OOS_BUILD */

