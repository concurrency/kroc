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

/* support level of call */
#define KCALL_UNDEFINED		0
#define KCALL_SUPPORTED		1
#define KCALL_DEPRECATED	2
#define KCALL_UNSUPPORTED	3

typedef struct {
	int call_offset;
	char *entrypoint;
	int input;
	int output;
	int support;
} ccsp_entrytype;

/* call table maximum size */
#define K_MAX_SUPPORTED		122

/* include auto-generated interface */
#include "kitable.h"

#endif	/* !__KIFACE_H */

