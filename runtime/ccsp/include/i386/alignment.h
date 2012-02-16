/*
 *	IA32 alignment macros
 *	Copyright (C) 2007      Carl Ritson  <cgr@kent.ac.uk>
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

#ifndef I386_ALIGNMENT_H
#define I386_ALIGNMENT_H

/*{{{  alignment helper macro */
#if defined(__GNUC__) && !defined(__ALIGN)
#define __ALIGN(X) __attribute__ ((aligned (X)))
#else
#define __ALIGN(X)
#endif /* defined(__GNUC__) */
/*}}}*/

/*{{{  cacheline defines */
#define CACHELINE_BYTES		64
#define CACHELINE_WORDS		16
/*}}}*/

/*{{{  cacheline alignment macro */
#if MAX_RUNTIME_THREADS > 1

#if defined(TARGET_OS_DARWIN)
#define CACHELINE_ALIGN 	__ALIGN(16) /* most Darwin gives */
#else /* !defined(TARGET_OS_DARWIN) */
#define CACHELINE_ALIGN 	__ALIGN(64)
#endif /* defined(TARGET_OS_DARWIN) */

#else /* MAX_RUNTIME_THREADS <= 1 */

#define CACHELINE_ALIGN

#endif /* MAX_RUNTIME_THREADS > 1 */ 
/*}}}*/

#endif /* I386_ALIGNMENT_H */

