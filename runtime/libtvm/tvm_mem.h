/*
tvm - tvm_mem.h
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

#ifndef TVM_MEM_H
#define TVM_MEM_H

#include "tvm.h"

/*{{{  Byte swapping */
#ifndef SwapTwoBytes
#define SwapTwoBytes(data) \
	((((data) >> 8) & 0x00FF) | (((data) << 8) & 0xFF00))
#endif /* SwapTwoBytes */

#ifndef SwapFourBytes
#define SwapFourBytes(data) \
	((((data) >> 24) & 0x000000FF) | (((data) >>  8) & 0x0000FF00) | \
	 (((data) <<  8) & 0x00FF0000) | (((data) << 24) & 0xFF000000) )
#endif /* !SwapFourBytes */
/*}}}*/

/*{{{  Memory interface */
#include "tvm_mem_intf.h"
/*}}}*/

/*{{{  void tvm_copy_data(BYTEPTR write_start, BYTEPTR read_start, UWORD num_bytes) */
#if defined(TVM_CUSTOM_COPY_DATA)
void (*tvm_copy_data)(BYTEPTR write_start, BYTEPTR read_start, UWORD num_bytes);
#elif defined(TVM_USE_MEMCPY) && defined(WORDPTRS_REAL)
#include <string.h>
#define tvm_copy_data memcpy
#else
void tvm_copy_data(BYTEPTR write_start, BYTEPTR read_start, UWORD num_bytes);
#endif /* !(defined USE_CUSTOM_COPY_DATA) */
/*}}}*/

/*{{{  void swap_data_word(WORDPTR a_ptr, WORDPTR b_ptr) */
TVM_UNUSED_OK
static TVM_INLINE void swap_data_word(WORDPTR a_ptr, WORDPTR b_ptr)
{
	WORD a_data = read_word(a_ptr);
	WORD b_data = read_word(b_ptr);
	write_word(b_ptr, a_data);
	write_word(a_ptr, b_data);
}
/*}}}*/

#endif /* !TVM_MEM_H */
