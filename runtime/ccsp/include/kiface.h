/*
 *	kiface.h - run-time kernel interface
 *	Copyright 	(C) 2000 Fred Barnes <frmb@kent.ac.uk>
 *	Modifications 	(C) 2007 Carl Ritson <cgr@kent.ac.uk>
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

#ifndef __KIFACE_H
#define __KIFACE_H

/* forms of passing arguments */
#define ARGS_ON_STACK		1
#define ARGS_IN_REGS		2

/* forms of kernel call */
#define KCALL_CALL		1	/* plain call */
#define KCALL_STOREIP_JUMP	2	/* store return addr in Iptr and jump */
#define KCALL_REGIP_JUMP	3	/* store return addr in next param and jump */
#define KCALL_JUMP 		4	/* plain jump (non-return only!) */

/* support level of call */
#define KCALL_UNDEFINED		0
#define KCALL_SUPPORTED		1
#define KCALL_DEPRECATED	2
#define KCALL_UNSUPPORTED	3

typedef struct {
	int call_offset;
	char *entrypoint;
	int input_mode;
	int call_mode;
	int output_mode;
	int input_count;
	int output_count;
	int support;
} ccsp_entrytype;

/* call table maximum size */
#define K_MAX_SUPPORTED		192

/* include auto-generated interface */
#include "kitable.h"

#endif	/* !__KIFACE_H */

