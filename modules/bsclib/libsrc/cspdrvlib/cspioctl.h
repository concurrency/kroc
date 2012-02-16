/*
 *	cspioctl.h -- IOCTLs for the CSP/Linux driver.  Shared by user + kernel
 *	Copyright (C) 2001 Fred Barnes <frmb2@ukc.ac.uk>
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

#ifndef __CSPIOCTL_H
#define __CSPIOCTL_H

#define IOCTL_GET_VERSION	0x1000
#define IOCTL_GET_CHANNEL	0x1001		/* ucsp_chaninfo *info */
#define IOCTL_TYPE_CHANNEL	0x1002		/* ucsp_chaninfo *info */
#define IOCTL_RELEASE_CHANNEL	0x1003		/* ucsp_chaninfo *info */

typedef struct {
	char name[32];		/* only for GET_CHANNEL */
	int dir;		/* READER/WRITER for GET_CHANNEL, hashcode for TYPE_CHANNEL */
	int chanid;		/* return for GET_CHANNEL, param for TYPE_CHANNEL/RELEASE_CHANNEL */
} ucsp_chaninfo;

#define CSP_CHANNEL_READER 0
#define CSP_CHANNEL_WRITER 1

#define IOCTL_READ_CHANNEL	0x1010		/* ucsp_datainfo *data */
#define IOCTL_WRITE_CHANNEL	0x1011		/* ucsp_datainfo *data */
#define IOCTL_ENABLE_CHANNEL	0x1012		/* ucsp_altinfo *data */
#define IOCTL_DISABLE_CHANNEL	0x1013		/* ucsp_altinfo *data */
#define IOCTL_ALTWT		0x1014		/* ucsp_altinfo *data */

#define IOCTL_SAFE_PAUSE	0x1020		/* ucsp_waitinfo *data */

typedef struct {
	int *sf_addr;
	unsigned long timeout;			/* timeout in micro-seconds */
} ucsp_waitinfo;

typedef struct {
	int chanid;
	char *data;
	int length;
} ucsp_datainfo;

typedef struct {
	int chanid;
	int ready;
} ucsp_altinfo;

#define CSP_CHANNEL_MAXSIZE 8192		/* uper limit */


#endif	/* !__CSPIOCTL_H */

