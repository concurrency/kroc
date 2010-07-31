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

/*{{{  memcpy/memset */
#if defined(TVM_USE_MEMCPY) || defined(TVM_USE_MEMSET)
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#endif

#if defined(TVM_USE_MEMCPY) && defined(TVM_USE_MEMSET)

#define _tvm_memcpy memcpy
#define _tvm_memset memset
#define tvm_memcpy memcpy
#define tvm_memset memset

#else

extern BYTEPTR _tvm_memcpy (BYTEPTR dst, BYTEPTR src, UWORD n);
extern BYTEPTR _tvm_memset (BYTEPTR s, WORD c, UWORD n);

#if defined(TVM_CUSTOM_MEM_OPS)
#define tvm_memcpy(DST,SRC,N)	ectx->memcpy((DST), (SRC), (N))
#define tvm_memset(S,C,N)	ectx->memset((S), (C), (N))
#else
#define tvm_memcpy 		_tvm_memcpy
#define tvm_memset		_tvm_memset
#endif /* !TVM_CUSTOM_MEM_OPS */

#endif /* !TVM_USE_MEMCPY */
/*}}}*/

/*{{{  copy_type_shadow/fill_type_shadow */
#ifdef TVM_TYPE_SHADOW
extern void copy_type_shadow (ECTX ectx, BYTEPTR dst, BYTEPTR src, WORD count);
extern void fill_type_shadow (ECTX ectx, BYTEPTR ptr, WORD count, WORD type);
#else /* !TVM_TYPE_SHADOW */
#define copy_type_shadow(CTX,DST,SRC,CNT) \
	do { } while (0)
#define fill_type_shadow(CTX,PTR,CNT,VAL) \
	do { } while (0)
#endif /* !TVM_TYPE_SHADOW */
/*}}}*/

/*{{{  void swap_data_word (ECTX ectx, WORDPTR a_ptr, WORDPTR b_ptr) */
TVM_UNUSED_OK
static TVM_INLINE void swap_data_word (ECTX ectx, WORDPTR a_ptr, WORDPTR b_ptr)
{
	WORD a_data = read_word (a_ptr);
	WORD b_data = read_word (b_ptr);
	write_word (b_ptr, a_data);
	write_word (a_ptr, b_data);
	#ifdef TVM_TYPE_SHADOW
	a_data = read_type (ectx, a_ptr);
	b_data = read_type (ectx, b_ptr);
	write_type (ectx, b_ptr, a_data);
	write_type (ectx, a_ptr, b_data);
	#endif /* TVM_TYPE_SHADOW */
	(void) ectx;
}
/*}}}*/

/*{{{  Memory allocator selection */
#if defined(TVM_USE_MALLOC)
#include <stdlib.h>

TVM_UNUSED_OK
static TVM_INLINE void *tvm_malloc (ECTX ectx, UWORD bytes)
{
	return (void *) malloc (bytes);
}

TVM_UNUSED_OK
static TVM_INLINE void tvm_free (ECTX ectx, void *data)
{
	free (data);
}
#elif defined(TVM_USE_TLSF)
extern size_t tlsf_init_memory_pool (size_t mem_pool_size, void *mem_pool);
extern size_t tlsf_get_used_size (void *mem_pool);
extern void tlsf_destroy_memory_pool (void *mem_pool);
extern void *tlsf_malloc (size_t size, void *mem_pool);
extern void tlsf_free (void *ptr, void *mem_pool);
extern void *tlsf_realloc (void *ptr, size_t new_size, void *mem_pool);

TVM_UNUSED_OK
static TVM_INLINE void *tvm_malloc (ECTX ectx, UWORD bytes)
{
	return (void *) tlsf_malloc (bytes, ectx->mem_pool);
}

TVM_UNUSED_OK
static TVM_INLINE void tvm_free (ECTX ectx, void *data)
{
	tlsf_free (data, ectx->mem_pool);
}
#endif /* TVM_USE_TLSF */
/*}}}*/

#endif /* !TVM_MEM_H */
