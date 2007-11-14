/*
 *	Architecture dependent types (i386 version)
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

#ifndef __I386_TYPES_H
#define __I386_TYPES_H

#if defined(HAVE_CONFIG_H)
#include <config.h>
#endif

#ifdef __GNUC__
#define _PACK_STRUCT __attribute__ ((packed))
#else
#define _PACK_STRUCT
#endif

typedef int Time;
typedef struct _cputime_t { 
	unsigned int tsc[2]; 
} _PACK_STRUCT cputime_t;

#undef _PACK_STRUCT

#endif /* !__I386_TYPES_H */

