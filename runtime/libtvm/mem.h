/*
tvm - mem.h
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

#ifndef MEM_H
#define MEM_H

#include "transputer.h"
#include "instructions.h"

#define SwapTwoBytes(data) \
(  (((data) >> 8) & 0x00FF) | (((data) << 8) & 0xFF00))

#define SwapFourBytes(data) \
( (((data) >> 24) & 0x000000FF) | (((data) >>  8) & 0x0000FF00) | \
	(((data) <<  8) & 0x00FF0000) | (((data) << 24) & 0xFF000000) )


#define ASSERT_WORD_ALIGNED(wordptr, WL, where) \
	if(((int)wordptr % WL) != 0) \
	{ \
		/*printf("Unaligned access while %s at %p\n", where, wordptr); */\
		exit_runloop(666); /* EXIT_ALIGN_ERROR */  \
	}

#if defined(MEMORY_INTF_BIGENDIAN)
#	include "mem_bigendian.h"
#elif defined(MEMORY_INTF_ARRAY)
#	include "mem_array.h"
#else
#	include "mem_native.h"
#endif

#if defined(USE_CUSTOM_COPY_DATA)
#define copy_data USE_CUSTOM_COPY_DATA
#elif defined(USE_MEMCPY) && defined(WORDPTRS_REAL)
#include <string.h>
#define copy_data memcpy
#else
TVM_HELPER_PROTO void copy_data(BYTEPTR write_start, BYTEPTR read_start, UWORD num_bytes);
#endif /* !(defined USE_CUSTOM_COPY_DATA) */

static inline void swap_data_word(WORDPTR a_ptr, WORDPTR b_ptr)
{
	WORD a_data = read_word(a_ptr);
	WORD b_data = read_word(b_ptr);
	write_word(b_ptr, a_data);
	write_word(a_ptr, b_data);
}

#endif
