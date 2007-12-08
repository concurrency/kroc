/*
 *	nmem.c -- new dynamic memory allocator
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
#if defined(RMOX_BUILD)
	#include <rmox_if.h>
#else	/* !RMOX_BUILD */
	#include <stdio.h>
	#if defined(HAVE_STDLIB_H)
		#include <stdlib.h>
	#endif
	#include <stdio.h>
	#if defined(HAVE_MALLOC_H)
		#include <malloc.h>
	#endif
	#if defined(DMEM_USE_MMAP)
		#include <sys/types.h>
		#include <sys/mman.h>
		#include <unistd.h>
		#include <fcntl.h>
	#endif
	#include <errno.h>

	#if defined(DMEM_TRACE)
		#define TRACE(fmt,args...) fprintf(trace, fmt "\n", ##args)
	#else
		#define TRACE(fmt,args...)
	#endif
#endif	/* !RMOX_BUILD */

#include <kernel.h>
#include <rts.h>

#include <dmem_if.h>
#include <typedesc.h>
#include <arch/asm_ops.h>
#include <arch/atomics.h>
/*}}}*/

/*{{{  documentation
 *
 * -- Memory
 *
 * Memory is divided into 64KiB "blocks".  A block is divided
 * into memory allocations called "objects", or used as part of
 * a "large object" spanning multiple blocks.
 *
 * A table of meta-data descriptors is allocated, one for each 
 * block in the usable address space of the machine.  For 64bit
 * architectures it is envisaged that only a small junk of the
 * overall address space will be usable (e.g. 32GiB).  When 
 * running under a 32bit OS, a table suitable for representing
 * 4GiB of memory will be allocated.  For direct hardware usage,
 * i.e. RMoX, the table will only be the size of the physical
 * system memory.
 *
 * The block descriptor is represented by the following 
 * structure:
 *
 * struct block_desc_t {
 * 	word status;
 * 	word next;
 * 	word free_count;
 * 	word free_list;
 * };
 *
 * The 'status' word indicates the block "state":
 *   - unused,
 *   - online (in use by an allocator),
 *   - dirty (not in use, but has remaining allocations),
 *   - clean (has been used, but is free reuse in any class).
 *
 * For online and dirty blocks, the status also indicates the
 * size class the allocation is being used for.
 *
 * The expected state follow is as follows:
 *
 * unused -> online -> dirty -> clean
 *  /^\       /^\         |      |
 *   |         |__________/      |
 *    \_________\________________/
 *                               
 * When a block is brought online it is dedicated to a size
 * class of "objects", or used as part of a large object.
 *
 * There are 1023 size classes.  Each size class is 64 bytes
 * larger than the previous.  The smallest class is 64 bytes,
 * the largest 65536 bytes (64KiB).
 *
 * For online blocks, the present owner (allocator) is
 * also stored in the status word.
 * 
 * A block used as part of a large object allocation may have 
 * a size class assigned to the its tail ("tail packing") in 
 * order to further minimise fragmentation.
 *
 * -- Allocator
 *
 * Each scheduler is complemented by its own allocator,
 * it is expected there will be one scheduler for each
 * execution unit.
 *
 * An allocator uses a single block of memory to maintain
 * its internal state.  Although additional memory maybe
 * required as described later.
 *
 * The allocator has a free list of objects for each size
 * class, an active block pointer for each size class and
 * a pointer to a structure describing free objects for
 * dirty blocks which were recently freed by this allocator.
 *
 * struct class_desc_t {
 * 	word		active_block;
 * 	free_object_t	*free_list;
 *	dirty_block_t	*dirty_free;
 * };
 *
 */
