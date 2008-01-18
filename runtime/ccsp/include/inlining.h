/*
 *	CCSP inlining support macros
 *	Copyright (C) 2007  Carl Ritson <cgr@kent.ac.uk>
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

#ifndef __INLINING_H
#define __INLINING_H

/*{{{  forceful inlining */
#ifdef __GNUC__
#define INLINE __attribute__ ((always_inline)) inline
#else
#define INLINE inline
#endif
/*}}}*/

/*{{{  quiet unuse */
#ifdef __GNUC__
#define UNUSED_OK __attribute__ ((unused))
#else
#define UNUSED_OK
#endif
/*}}}*/

/*{{{  size optimisation */
#ifdef OPTIMISE_SIZE
#define TRIVIAL UNUSED_OK
#define HOT UNUSED_OK
#define WARM UNUSED_OK
#define TEPID UNUSED_OK
#define COLD UNUSED_OK
#endif
/*}}}*/

/*{{{  defaults */
#ifndef TRIVIAL
#define TRIVIAL INLINE
#endif
#ifndef HOT
#define HOT INLINE
#endif
#ifndef WARM
#define WARM inline
#endif
#ifndef TEPID
#define TEPID UNUSED_OK
#endif
#ifndef COLD
#define COLD UNUSED_OK
#endif
/*}}}*/

#endif	/* !__INLINING_H */

