/*
tvm - pool_alloc.c
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2004-2008 Christian L. Jacobsen, Matthew C. Jadud

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "transputer.h"
#include "pool_alloc.h"
#include "interpreter.h"

#ifdef __MOBILE_PI_SUPPORT__

#include <stdlib.h>
#include <string.h>

#ifdef MSP430
#undef malloc
#include <sys/types.h>
void *malloc ();
/* Allocate an N-byte block of memory from the heap. If N is zero, allocate a 1-byte block. */
void * rpl_malloc (size_t n) { if (n == 0) n = 1; return malloc (n); }
#endif

/* From Fred */
static inline int SlotToSize (int p)
{
  int s;

  s = (4 << (p >> 1));
  if (p & 1) {
    s += (2 << (p >> 1));
  }
  return s;
}

void *palloc(int size)
{
	void *addr = malloc(size);

	if (addr == NULL)
		exit_runloop(EXIT_BAD_ADDR);

	return addr;
}

void *palloc_pool(int index)
{
	return palloc(SlotToSize(index));
}

void pfree(void *addr)
{
	free(addr);
}

void pfree_pool(int index, void *addr)
{
	pfree(addr);
}

#endif

