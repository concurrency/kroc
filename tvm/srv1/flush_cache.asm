/*
 *  flush_cache.asm - code to flush cache lines on Blackfin processors
 *    Copyright (C) 2008  Carl Ritson <cgr@kent.ac.uk>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details (www.gnu.org/licenses)
 */

#include "bfin_config.h"
#include "asmmacros.h"

/*
Prototype       : void flush_cache (void *data, unsigned int length);

                 *data -> Pointer to data to flush.
                 *length -> Length in bytes of data.

Registers Used  : R0-R2, P0, LC0.
*/

.text;

.align 8;
.global _flush_cache;
_flush_cache:
        /* r0 = data, r1 = length */
	r2 = 0x1f (x);
	r2 = ~r2;

	r0 = r0 & r2;
	p0 = r0

	r1 += 32;
	r1 = r1 & r2;
	r1 >>= 5;

	LC0 = r1;
	loop __flush_cache LC0;
	loop_begin __flush_cache;
	flushinv [p0++];
	loop_end __flush_cache;

	rts;
