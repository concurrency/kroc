/*
tvm - ins_mobile.c
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2004-2008 Christian L. Jacobsen, Matthew C. Jadud, Carl G. Ritson

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

#include "tvm.h"
#include "instructions.h"
#include "ext_chan.h"

#include "scheduler.h"

#include "ins_barrier.h"
#include "ins_chan.h"
#include "ins_mobile.h"
#include "ins_pri.h"
#include "ins_sec.h"

#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_OCCAM_PI)

#if defined(TVM_DYNAMIC_MEMORY) && defined(TVM_USE_MALLOC)
#include <stdlib.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#endif /* TVM_DYNAMIC_MEMORY && TVM_USE_MALLOC */

/* stddef.h is freestanding, so should be safe in any build */
#include <stddef.h>
#ifndef offsetof
#define offsetof(t,f) ((WORD) (&((((t *)(0))->f))))
#endif

#define word_offset(type, field) \
	(offsetof(type, field) >> WSH)
#define wordptr_offset(ptr, type, field) \
	wordptr_plus((ptr), word_offset(type, field))
#define read_offset(ptr, type, field) \
	read_word(wordptr_offset(ptr,type,field))
#define write_offset(ptr, type, field, data) \
	write_word(wordptr_offset(ptr,type,field), (WORD) (data))
#define read_type(ptr) \
	(read_word (wordptr_minus ((ptr), 1)))

/*{{{  static WORDPTR mt_alloc_array_int (UWORD type, UWORD size, UWORD init, UWORD *shift)*/
static WORDPTR mt_alloc_array_int (UWORD type, UWORD size, UWORD init, UWORD *shift)
{
	BYTEPTR data;
	WORDPTR ma;
	UWORD alignment		= 0;
	UWORD dimensions	= MT_ARRAY_DIM(type);
	UWORD dma		= 0;
	UWORD inner_type	= MT_ARRAY_INNER_TYPE(type);
	UWORD meta_words	= dimensions + MT_ARRAY_PTR_OFFSET + 1;
	UWORD size_shift	= WSH;
	UWORD bytes;

	if (MT_TYPE(inner_type) == MT_ARRAY_OPTS) {
		if (MT_FLAGS(inner_type) & MT_ARRAY_OPTS_DMA) {
			/* allocate space for hardware address */
			dma = dimensions;
			meta_words += 1;
		}
		alignment	= (1 << MT_ARRAY_OPTS_ALIGN(inner_type)) - 1;
		inner_type	= MT_ARRAY_OPTS_INNER(inner_type);
	}

	if (MT_TYPE(inner_type) == MT_NUM) {
		size_shift = mt_num_size_shift (MT_NUM_TYPE(inner_type));

		if ((size_shift > WSH) && !alignment) {
			alignment = (1 << size_shift) - 1;
		}
	}

	bytes		= (size << size_shift) + alignment + (meta_words << WSH);
	ma		= (WORDPTR) TVM_MALLOC (bytes);
	if (size) {
		data	= byteptr_plus ((BYTEPTR) wordptr_plus (ma, meta_words), alignment);
		data	= (BYTEPTR) (((UWORD) data) & (~alignment));
	} else {
		data	= NULL_P;
	}
	write_offset (ma, mt_array_internal_t, size, size);
	write_offset (ma, mt_array_internal_t, type, type);
	write_offset (ma, mt_array_internal_t, array.data, data);
	
	if (dma) {
		/* this may need virt->phys mapping */
		write_offset (ma, mt_array_internal_t, array.dimensions[dma], data);
	}

	if (init && (MT_TYPE(inner_type) != MT_NUM)) {
		while (size--) {
			write_word (data, (WORD) NULL_P);
			data = (BYTEPTR) wordptr_plus ((WORDPTR) data, 1);
		}
	}

	*shift = size_shift;

	return ma;
}
/*}}}*/
/*{{{  static WORDPTR mt_alloc_array (UWORD type, UWORD size)*/
static WORDPTR mt_alloc_array (UWORD type, UWORD size)
{
	UWORD size_shift = 0;

	return wordptr_plus (
		mt_alloc_array_int (type, size, 1, &size_shift), 
		MT_ARRAY_PTR_OFFSET
	);
}
/*}}}*/
/*{{{  static WORDPTR mt_alloc_cb (UWORD type, UWORD channels)*/
static WORDPTR mt_alloc_cb (UWORD type, UWORD channels)
{
	WORDPTR cb, i_cb;
	UWORD words = channels;
	UWORD i;

	if (type & MT_CB_STATE_SPACE) {
		words += 5;
	}

	if (type & MT_CB_SHARED) {
		words += MT_CB_SHARED_PTR_OFFSET;

		i_cb 	= (WORDPTR) TVM_MALLOC (words << WSH);
		tvm_sem_init (wordptr_offset(i_cb, mt_cb_shared_internal_t, sem[0]));
		tvm_sem_init (wordptr_offset(i_cb, mt_cb_shared_internal_t, sem[1]));
		write_offset (i_cb, mt_cb_shared_internal_t, ref_count, 2);
		write_offset (i_cb, mt_cb_shared_internal_t, type, type);
		cb	= wordptr_plus (i_cb, MT_CB_SHARED_PTR_OFFSET);
	} else {
		words += MT_CB_PTR_OFFSET;
	
		i_cb 	= (WORDPTR) TVM_MALLOC (words << WSH);
		write_offset (i_cb, mt_cb_internal_t, ref_count, 2);
		write_offset (i_cb, mt_cb_internal_t, type, type);
		cb	= wordptr_plus (i_cb, MT_CB_PTR_OFFSET);
	}

	for (i = 0; i < channels; ++i) {
		write_word (wordptr_plus (cb, i), (WORD) NOT_PROCESS_P);
	}

	return cb;
}
/*}}}*/
/*{{{  static WORDPTR mt_alloc_barrier (UWORD type)*/
static WORDPTR mt_alloc_barrier (UWORD type)
{	
	WORDPTR mb	= TVM_MALLOC (sizeof (mt_barrier_internal_t));
	WORDPTR bar	= wordptr_offset(mb, mt_barrier_internal_t, barrier);

	write_offset (mb, mt_barrier_internal_t, ref_count, 1);
	write_offset (mb, mt_barrier_internal_t, type, type);

	switch (MT_BARRIER_TYPE(type)) {
		case MT_BARRIER_FULL:
			tvm_bar_init (bar, 1);
			break;
		case MT_BARRIER_FORKING:
			write_word (wordptr_plus (bar, 0), (WORD) NOT_PROCESS_P);
			write_word (wordptr_plus (bar, 1), 0);
			break;
	}
	
	return wordptr_plus (mb, MT_BARRIER_PTR_OFFSET);
}
/*}}}*/
/*{{{  TVM_HELPER WORDPTR mt_alloc_data (ECTX ectx, UWORD type, UWORD size)*/
TVM_HELPER WORDPTR mt_alloc_data (ECTX ectx, UWORD type, UWORD size)
{	
	WORDPTR md;
	UWORD bytes = (size + (sizeof(UWORD) - 1)) & (~(sizeof(UWORD) - 1));
	
	bytes += MT_DATA_PTR_OFFSET << WSH;

	md = (WORDPTR) TVM_MALLOC (bytes);
	write_offset (md, mt_data_internal_t, size, size);
	write_offset (md, mt_data_internal_t, type, type);

	return wordptr_plus (md, MT_DATA_PTR_OFFSET);
}
/*}}}*/
/*{{{   TVM_HELPER int mt_release_simple (ECTX ectx, WORDPTR ptr, UWORD type)*/
TVM_HELPER int mt_release_simple (ECTX ectx, WORDPTR ptr, UWORD type)
{
	int ret = ECTX_CONTINUE;

	switch (MT_TYPE(type)) {
		case MT_ARRAY:
			{
				WORDPTR ma = wordptr_minus (ptr, MT_ARRAY_PTR_OFFSET);
				UWORD inner_type = MT_ARRAY_INNER_TYPE(type);

				if (MT_TYPE(inner_type) == MT_ARRAY_OPTS) {
					inner_type = MT_ARRAY_OPTS_INNER(inner_type);
				}

				if (MT_TYPE(inner_type) != MT_NUM) {
					WORDPTR data;
					UWORD size;

					size = read_offset (ma, mt_array_internal_t, size);
					data = (WORDPTR) read_offset (ma, mt_array_internal_t, array.data);

					while (size--) {
						WORDPTR inner_ptr = (WORDPTR) read_word (data);
						if (inner_ptr != (WORDPTR) NULL_P) {
							mt_release (ectx, inner_ptr);
						}
						data = wordptr_plus (data, 1);
					}
				}

				TVM_FREE (ma);
			}
			break;
		case MT_CB:
			{
				WORDPTR cb = wordptr_minus (ptr, MT_CB_PTR_OFFSET);
				UWORD refs = (UWORD) read_offset (cb, mt_cb_internal_t, ref_count);

				if (refs <= 1) {
					if (type & MT_CB_SHARED) {
						TVM_FREE (wordptr_minus (ptr, MT_CB_SHARED_PTR_OFFSET));
					} else {
						TVM_FREE (cb);
					}
				} else {
					write_offset (cb, mt_cb_internal_t, ref_count, refs - 1);
				}
			}
			break;
		case MT_BARRIER:
			{
				WORDPTR mb = wordptr_minus (ptr, MT_BARRIER_PTR_OFFSET);
				UWORD refs = (UWORD) read_offset (mb, mt_barrier_internal_t, ref_count);

				if (refs > 1) {
					write_offset (mb, mt_barrier_internal_t, ref_count, refs - 1);
					if (MT_BARRIER_TYPE(type) == MT_BARRIER_FULL) {
						ret = tvm_bar_resign (ectx, ptr, 1);
					}
				} else {
					if (MT_BARRIER_TYPE(type) == MT_BARRIER_FORKING) {
						WORD process = read_word (ptr);
						if (process != NOT_PROCESS_P) {
							ADD_TO_QUEUE_ECTX ((WORDPTR) process, ret);
						}
					}
					TVM_FREE (mb);
				}
			}
			break;
		case MT_DATA:
			{
				WORDPTR md = wordptr_minus (ptr, MT_DATA_PTR_OFFSET);
				TVM_FREE (md);
			}
			break;
		default:
			ret = ectx->set_error_flag (ectx, EFLAG_MT);
			break;
	}

	return ret;
}
/*}}}*/
/*{{{  TVM_HELPER int mt_release (ECTX ectx, WORDPTR ptr)*/
TVM_HELPER int mt_release (ECTX ectx, WORDPTR ptr)
{
	UWORD type = read_type (ptr);

	if (type & MT_SIMPLE) {
		return mt_release_simple (ectx, ptr, type);
	} else {
		return ectx->set_error_flag (ectx, EFLAG_MT);
	}
}
/*}}}*/
/*{{{  static int mt_clone_array (ECTX ectx, WORDPTR ptr, UWORD type, WORDPTR *ret)*/
static int mt_clone_array (ECTX ectx, WORDPTR ptr, UWORD type, WORDPTR *ret)
{
	WORDPTR src = wordptr_minus (ptr, MT_ARRAY_PTR_OFFSET);
	WORDPTR dst, dst_data, dst_dim, src_data, src_dim;
	UWORD dimensions	= MT_ARRAY_DIM(type);
	UWORD inner_type	= MT_ARRAY_INNER_TYPE(type);
	UWORD size		= (UWORD) read_offset (src, mt_array_internal_t, size);
	UWORD size_shift;

	dst = mt_alloc_array_int (type, size, 0, &size_shift);

	dst_dim = wordptr_offset (dst, mt_array_internal_t, array.dimensions);
	src_dim = wordptr_offset (src, mt_array_internal_t, array.dimensions);

	while (dimensions--) {
		write_word (dst_dim, read_word (src_dim));
		dst_dim = wordptr_plus (dst_dim, 1);
		src_dim = wordptr_plus (src_dim, 1);
	}
	
	dst_data = (WORDPTR) read_offset (dst, mt_array_internal_t, array.data);
	src_data = (WORDPTR) read_offset (src, mt_array_internal_t, array.data);

	if (MT_TYPE(inner_type) == MT_ARRAY_OPTS) {
		inner_type = MT_ARRAY_OPTS_INNER(inner_type);
	}

	if (MT_TYPE(inner_type) == MT_NUM) {
		tvm_copy_data ((BYTEPTR) dst_data, (BYTEPTR) src_data, size << size_shift);
	} else {
		while (size--) {
			WORDPTR inner_ptr = (WORDPTR) read_word (src_data);
			
			if (inner_ptr != (WORDPTR) NULL_P) {
				WORDPTR src_ptr = inner_ptr;
				int eret;
				if ((eret = mt_clone (ectx, src_ptr, &inner_ptr))) {
					return eret;
				}
			}
			write_word (dst_data, (WORD) inner_ptr);

			dst_data = wordptr_plus (dst_dim, 1);
			src_data = wordptr_plus (src_dim, 1);
		}
	}

	*ret = wordptr_plus (dst, MT_ARRAY_PTR_OFFSET);
	return ECTX_CONTINUE;
}	
/*}}}*/
/*{{{  static int mt_clone_simple (ECTX ectx, WORDPTR ptr, UWORD type, WORDPTR *ret)*/
static int mt_clone_simple (ECTX ectx, WORDPTR ptr, UWORD type, WORDPTR *ret)
{
	switch (MT_TYPE(type)) {
		case MT_ARRAY:
			return mt_clone_array (ectx, ptr, type, ret);
		case MT_CB:
			{
				WORDPTR cb = wordptr_minus (ptr, MT_CB_PTR_OFFSET);
			 	UWORD refs = (UWORD) read_offset (cb, mt_cb_internal_t, ref_count);
				
				write_offset (cb, mt_cb_internal_t, ref_count, refs + 1);
			}
			*ret = ptr;
			break;
		case MT_BARRIER:
			{
				WORDPTR mb = wordptr_minus (ptr, MT_BARRIER_PTR_OFFSET);
			 	UWORD refs = (UWORD) read_offset (mb, mt_barrier_internal_t, ref_count);
				
				write_offset (mb, mt_barrier_internal_t, ref_count, refs + 1);
				if (MT_BARRIER_TYPE(type) == MT_BARRIER_FULL) {
					return tvm_bar_enroll (ectx, ptr, 1);
				}
			}
			*ret = ptr;
			break;
		case MT_DATA:
			{
				WORDPTR src = wordptr_minus (ptr, MT_DATA_PTR_OFFSET);
				WORDPTR dst;
				UWORD size = (UWORD) read_offset (src, mt_data_internal_t, size);

				dst = mt_alloc_data (ectx, type, size);
				tvm_copy_data ((BYTEPTR) dst, (BYTEPTR) ptr, size);

				*ret = dst;
			}
			break;
		default:
			*ret = (WORDPTR) NULL_P;
			return ectx->set_error_flag (ectx, EFLAG_MT);
	}
	return ECTX_CONTINUE;
}
/*}}}*/
/*{{{  TVM_HELPER int mt_clone (ECTX ectx, WORDPTR ptr, WORDPTR *ret)*/
TVM_HELPER int mt_clone (ECTX ectx, WORDPTR ptr, WORDPTR *ret)
{
	UWORD type = read_type (ptr);

	if (type & MT_SIMPLE) {
		return mt_clone_simple (ectx, ptr, type, ret);
	} else {
		*ret = (WORDPTR) NULL_P;
		return ectx->set_error_flag (ectx, EFLAG_MT);
	}
}
/*}}}*/
/*{{{  static void mt_io_update_shared_cb (WORDPTR *ptr)*/
static void mt_io_update_shared_cb (WORDPTR *ptr)
{
	WORDPTR cb = wordptr_minus (*ptr, MT_CB_PTR_OFFSET);
	UWORD refs = (UWORD) read_offset (cb, mt_cb_internal_t, ref_count);
	
	write_offset (cb, mt_cb_internal_t, ref_count, refs + 1);
}
/*}}}*/
/*{{{  static int mt_io_update_barrier (WORDPTR *ptr)*/
static int mt_io_update_barrier (ECTX ectx, WORDPTR *ptr)
{
	WORDPTR mb = wordptr_minus (*ptr, MT_BARRIER_PTR_OFFSET);
	UWORD refs = (UWORD) read_offset (mb, mt_barrier_internal_t, ref_count);
	UWORD type = read_type (*ptr);
	
	write_offset (mb, mt_barrier_internal_t, ref_count, refs + 1);
	if (MT_BARRIER_TYPE(type) == MT_BARRIER_FULL) {
		return tvm_bar_enroll (ectx, *ptr, 1);
	} else {
		return ECTX_CONTINUE;
	}
}
/*}}}*/
/*{{{  static int mt_io_update_array (ECTX ectx, WORDPTR *ptr, UWORD inner)*/
static int mt_io_update_array (ECTX ectx, WORDPTR *ptr, UWORD inner)
{
	WORDPTR ma	= wordptr_minus (ptr, MT_ARRAY_PTR_OFFSET);
	WORDPTR data	= (WORDPTR) read_offset (ma, mt_array_internal_t, array.data);
	UWORD size	= (UWORD) read_offset (ma, mt_array_internal_t, size);
	int ret 	= ECTX_CONTINUE;

	switch (MT_TYPE(inner)) {
		case MT_ARRAY:
			inner = MT_ARRAY_INNER_TYPE (inner);
			while (size--) {
				WORDPTR inner_ptr = (WORDPTR) read_word (data);
				if (inner_ptr != (WORDPTR) NULL_P) {
					if ((ret = mt_io_update_array (ectx, (WORDPTR *) data, inner))) {
						break;
					}
				}
				data = wordptr_plus (data, 1);
			}
			break;
		case MT_CB:
			if (!(inner & MT_CB_SHARED)) {
				break;
			}
			while (size--) {
				WORDPTR inner_ptr = (WORDPTR) read_word (data);
				if (inner_ptr != (WORDPTR) NULL_P) {
					mt_io_update_shared_cb ((WORDPTR *) data);
				}
				data = wordptr_plus (data, 1);
			}
			break;
		case MT_BARRIER:
			while (size--) {
				WORDPTR inner_ptr = (WORDPTR) read_word (data);
				if (inner_ptr != (WORDPTR) NULL_P) {
					if ((ret = mt_io_update_barrier (ectx, (WORDPTR *) data))) {
						break;
					}
				}
				data = wordptr_plus (data, 1);
			}
			break;
		case MT_MT:
			while (size--) {
				WORDPTR inner_ptr = (WORDPTR) read_word ((WORDPTR *) data);
				if (inner_ptr != (WORDPTR) NULL_P) {
					UWORD move;
					if ((ret = mt_io_update (ectx, (WORDPTR *) data, &move))) {
						break;
					}	
				}
				data = wordptr_plus (data, 1);
			}
			break;
		case MT_DATA:
			break;
		default:
			ret = ectx->set_error_flag (ectx, EFLAG_MT);
			break;
	}
	return ret;
}
/*}}}*/
/*{{{  TVM_HELPER int mt_io_update (ECTX ectx, WORDPTR *ptr, UWORD *move)*/
TVM_HELPER int mt_io_update (ECTX ectx, WORDPTR *ptr, UWORD *move)
{
	UWORD type = read_type (*ptr);

	if (type & MT_SIMPLE) {
		if (MT_TYPE(type) == MT_ARRAY) {
			UWORD temp = type;

			do {
				temp = MT_ARRAY_INNER_TYPE (temp);
				if (MT_TYPE(temp) == MT_ARRAY_OPTS) {
					temp = MT_ARRAY_OPTS_INNER(temp);
				}
				if (MT_TYPE(temp) == MT_NUM) {
					return MT_TRUE;
				}	
			} while (MT_TYPE(temp) == MT_ARRAY);

			*move = MT_TRUE;
			return mt_io_update_array (ectx, ptr, MT_ARRAY_INNER_TYPE(type));
		} else if (MT_TYPE(type) == MT_CB) {
			if (type & MT_CB_SHARED) {
				*move = MT_FALSE;
				mt_io_update_shared_cb (ptr);
			} else {
				*move = MT_TRUE;
			}
		} else if (MT_TYPE(type) == MT_BARRIER) {
			*move = MT_FALSE;
			return mt_io_update_barrier (ectx, ptr);
		} else if (MT_TYPE(type) == MT_DATA) {
			*move = MT_TRUE;
		} else {
			*move = MT_FALSE;
		}
	} else {
		*move = MT_TRUE;
		return ectx->set_error_flag (ectx, EFLAG_MT);
	}
	
	return ECTX_CONTINUE;
}
/*}}}*/
/*{{{  TVM_HELPER int mt_chan_io (ECTX ectx, WORDPTR dst, WORDPTR src)*/
TVM_HELPER int mt_chan_io (ECTX ectx, WORDPTR dst, WORDPTR src)
{
	/* Read pointer to mobile type */
	WORDPTR ptr = (WORDPTR) read_word (src);
	/* Is there anything there? */
	if (ptr != (WORDPTR) NULL_P) {
		UWORD move;
		int ret;
		/* Is defined, so update it */
		if ((ret = mt_io_update (ectx, &ptr, &move))) {
			return ret;
		}
		if (move == MT_TRUE) {
			/* Pointer moved, delete old reference */
			write_word (src, (WORD) NULL_P);
		}
	}
	/* Write out possibly new pointer to mobile type */
	write_word (dst, (WORD) ptr);
	return ECTX_CONTINUE;
}
/*}}}*/
/*{{{  TVM_HELPER int mt_alloc (ECTX ectx, UWORD type, UWORD size, WORDPTR *ret)*/
TVM_HELPER int mt_alloc (ECTX ectx, UWORD type, UWORD size, WORDPTR *ret)
{
	if (type & MT_SIMPLE) {
		switch (MT_TYPE(type)) {
			case MT_ARRAY:
				*ret = mt_alloc_array (type, size);
				return ECTX_CONTINUE;
			case MT_CB:
				*ret = mt_alloc_cb (type, size);
				return ECTX_CONTINUE;
			case MT_BARRIER:
				*ret = mt_alloc_barrier (type);
				return ECTX_CONTINUE;
			case MT_DATA:
				*ret = mt_alloc_data (ectx, type, size);
				return ECTX_CONTINUE;
			default:
				break;
		}
	}
	
	/* Should not end up here. */
	*ret = (WORDPTR) NULL_P;
	return ectx->set_error_flag (ectx, EFLAG_MT);
}
/*}}}*/

/* 0x11 - 0x21 0xF1 - mreleasep - dynamic process release */
TVM_INSTRUCTION (ins_mreleasep)
{
	/* Find the pointer to the allocated block, size comes in from AREG */
	BYTEPTR ptr = ((BYTEPTR)WPTR) + (AREG * TVM_WORD_LENGTH);
	//printf(">mreleasep\n");
	//printf("  ptr = 0x%08x\n", (WORD) ptr);
	mt_release_simple(ectx, (WORDPTR) ptr, MT_MAKE_TYPE(MT_DATA));

	RUN_NEXT_ON_QUEUE_RET();
	//printf("<mreleasep\n");
}

#if 0
/* 0x62 - 0x26 0xF2 - minn - multi dimension mobile array input */
TVM_INSTRUCTION (ins_minn)
{
	/* Set up the AREG to be in bytes (what in expects) rather than 
	 * words (what minn gets)
	 */
	AREG = AREG << WSH;

	return ins_in(ectx);
}

/* 0x64 - 0x26 0xF4 - moutn - multi dimension mobile array output */
TVM_INSTRUCTION (ins_moutn)
{
	/* Set up the AREG to be in bytes (what out expects) rather than 
	 * words (what moutn gets)
	 */
	AREG = AREG << WSH;

	return ins_out(ectx);
}
#endif

/* 0xE2 - 0x2E 0xF2 - malloc - dynamic memory allocation */
TVM_INSTRUCTION (ins_malloc)
{
	WORDPTR ptr = (WORDPTR)NULL_P;
	UWORD size = AREG;
	
	if(size != 0)
	{
		ptr = mt_alloc_data(ectx, MT_SIMPLE | MT_MAKE_TYPE(MT_DATA), size);
	}

	STACK_RET((WORD)ptr, UNDEFINE(BREG), UNDEFINE(CREG));
}

/* 0xE3 - 0x2E 0xF3 - mrelease - dynamic memory release */
TVM_INSTRUCTION (ins_mrelease)
{
	if(AREG == (WORD)NULL_P)
	{
		return ectx->set_error_flag(ectx, EFLAG_DMEM);
	}

	return mt_release_simple(ectx, (WORDPTR)AREG, MT_MAKE_TYPE(MT_DATA));
}

#if 0
/* 0xE4 - 0x2E 0xF4 - min - mobile input */
TVM_INSTRUCTION (ins_min)
{
	WORDPTR chan_ptr	= (WORDPTR)AREG;
	WORDPTR data_ptr	= (WORDPTR)BREG;

	return chan_swap(ectx, chan_ptr, data_ptr);
}

/* 0xE5 - 0x2E 0xF5 - mout - mobile output */
TVM_INSTRUCTION (ins_mout)
{
	WORDPTR chan_ptr = (WORDPTR)AREG;
	WORDPTR data_ptr = (WORDPTR)BREG;

	return chan_swap(ectx, chan_ptr, data_ptr);
}

/* 0xE6 - 0x2E 0xF6 - min64 - dynamic mobile array input */
TVM_INSTRUCTION (ins_min64)
{
	BYTEPTR data_ptr = (BYTEPTR)BREG;
	WORDPTR chan_ptr = (WORDPTR)AREG;

	return chan_in(ectx, 8, chan_ptr, data_ptr);
}

/* 0xE7 - 0x2E 0xF7 - mout64 - dynamic mobile array output */
TVM_INSTRUCTION (ins_mout64)
{
	BYTEPTR data_ptr = (BYTEPTR)BREG;
	WORDPTR chan_ptr = (WORDPTR)AREG;

	return chan_out(ectx, 8, chan_ptr, data_ptr);
}

/* 0xEA - 0x2E 0xFA - xmin - Extended Mobile Input */
TVM_INSTRUCTION (ins_xmin)
{
	WORDPTR chan_addr = (WORDPTR) AREG;
	WORDPTR data_ptr = (WORDPTR) BREG;
	WORDPTR other_ptr;
	WORDPTR other_WPTR;

	other_WPTR = (WORDPTR) read_word(chan_addr);
	other_ptr = (WORDPTR) WORKSPACE_GET(other_WPTR, WS_POINTER);

	swap_data_word(data_ptr, other_ptr);

	UNDEFINE_STACK_RET();
}

/* 0xEB - 0x2E 0xFB - xmin64 - Extended Dynamic Mobile Array Input */
TVM_INSTRUCTION (ins_xmin64)
{
	/* Push 8 (byte count) onto stack. */
	STACK(8, AREG, BREG);
	/* Do an XIN */
	return ins_xin(ectx);
}

/* 0x65 - 0x26 0xF5 - xminn - Extended multi-dim Dynamic Mobile Array Input */
TVM_INSTRUCTION (ins_xminn)
{
	/* Convert word count to byte count */
	AREG = AREG << WSH;
	/* Do an XIN */
	return ins_xin(ectx);
}
#endif

/* 0x238 - 0x22 0x23 0xF8 - mt_alloc - allocate a mobile type */
TVM_INSTRUCTION (ins_mt_alloc)
{
	WORDPTR ptr;
	int ret;

	if ((ret = mt_alloc (ectx, (UWORD) AREG, (UWORD) BREG, &ptr))) {
		return ret;
	}
	
	STACK_RET (ptr, UNDEFINE(BREG), UNDEFINE(CREG));
}

/* 0x239 - 0x22 0x23 0xF9 - mt_release - release a mobile type */
TVM_INSTRUCTION (ins_mt_release)
{
	return mt_release (ectx, (WORDPTR) AREG);
}

/* 0x23A - 0x22 0x23 0xFA - mt_clone - clone a mobile type */
TVM_INSTRUCTION (ins_mt_clone)
{
	WORDPTR ptr;
	int ret;

	if ((ret = mt_clone (ectx, (WORDPTR) AREG, &ptr))) {
		return ret;
	}

	STACK_RET (AREG, UNDEFINE(BREG), UNDEFINE(CREG));
}

/* 0x23B - 0x22 0x23 0xFB - mt_in - mobile type channel input */
TVM_INSTRUCTION (ins_mt_in)
{
	WORDPTR chan_ptr	= (WORDPTR) AREG;
	WORDPTR dst		= (WORDPTR) BREG;
	WORDPTR other_WPTR;
	int ret;
	
	UNDEFINE_STACK ();

	if ((ret = chan_io_begin (ectx, 0, chan_ptr, (BYTEPTR) dst, &other_WPTR)))
	{
		return ret;
	}
	else if (other_WPTR != NOT_PROCESS_P)
	{
		/* Get pointer to source pointer */
		WORDPTR src = (WORDPTR) WORKSPACE_GET (other_WPTR, WS_POINTER);
		/* Do input */
		if ((ret = mt_chan_io (ectx, dst, src))) {
			return ret;
		}
		/* Complete channel operation */
		return chan_io_end (ectx, chan_ptr, other_WPTR);
	}

	return ECTX_CONTINUE;
}

/* 0x23C - 0x22 0x23 0xFC - mt_out - mobile type channel output */
TVM_INSTRUCTION (ins_mt_out)
{
	WORDPTR chan_ptr	= (WORDPTR) AREG;
	WORDPTR src		= (WORDPTR) BREG;
	WORDPTR other_WPTR;
	int ret;
	
	UNDEFINE_STACK ();

	if ((ret = chan_io_begin (ectx, 1, chan_ptr, (BYTEPTR) src, &other_WPTR)))
	{
		return ret;
	}
	else if (other_WPTR != NOT_PROCESS_P)
	{
		/* Get pointer to destination */
		WORDPTR dst = (WORDPTR) WORKSPACE_GET (other_WPTR, WS_POINTER);
		/* Do output */
		if ((ret = mt_chan_io (ectx, dst, src))) {
			return ret;
		}
		/* Complete channel operation */
		return chan_io_end (ectx, chan_ptr, other_WPTR);
	}

	return ECTX_CONTINUE;
}

/* 0x23D - 0x22 0x23 0xFD - mt_xchg - mobile type channel exchange */
TVM_INSTRUCTION (ins_mt_xchg)
{
	WORDPTR chan_ptr = (WORDPTR) AREG;
	WORDPTR data_ptr = (WORDPTR) BREG;

	return chan_swap (ectx, chan_ptr, data_ptr);
}

/* 0x23E - 0x22 0x23 0xFE - mt_lock - lock a mobile type */
TVM_INSTRUCTION (ins_mt_lock)
{
	WORDPTR mt = (WORDPTR) BREG;
	WORDPTR cb = wordptr_minus (mt, MT_CB_SHARED_PTR_OFFSET);
	WORD lock = AREG << 1;

	return tvm_sem_claim (ectx, wordptr_plus (wordptr_offset (cb, mt_cb_shared_internal_t, sem), lock));
}

/* 0x23F - 0x22 0x23 0xFF - mt_unlock - unlock a mobile type */
TVM_INSTRUCTION (ins_mt_unlock)
{
	WORDPTR mt = (WORDPTR) BREG;
	WORDPTR cb = wordptr_minus (mt, MT_CB_SHARED_PTR_OFFSET);
	WORD lock = AREG << 1;

	return tvm_sem_release (ectx, wordptr_plus (wordptr_offset (cb, mt_cb_shared_internal_t, sem), lock));
}

/* 0x240 - 0x22 0x24 0xF0 - mt_enroll - enroll processes on a mobile type */
TVM_INSTRUCTION (ins_mt_enroll)
{
	WORDPTR mt = (WORDPTR) BREG;
	UWORD count = (UWORD) AREG;
	UWORD type = read_type (mt);
	
	switch (MT_BARRIER_TYPE (type)) {
		case MT_BARRIER_FULL:
			return tvm_bar_enroll (ectx, mt, count);
	}
	
	return ectx->set_error_flag (ectx, EFLAG_MT);
}

/* 0x241 - 0x22 0x24 0xF1 - mt_resign - resign process from a mobile type */
TVM_INSTRUCTION (ins_mt_resign)
{
	WORDPTR mt = (WORDPTR) BREG;
	UWORD count = (UWORD) AREG;
	UWORD type = read_type (mt);
	
	switch (MT_BARRIER_TYPE (type)) {
		case MT_BARRIER_FULL:
			return tvm_bar_resign (ectx, mt, count);
	}
	
	return ectx->set_error_flag (ectx, EFLAG_MT);
}

/* 0x242 - 0x22 0x24 0xF2 - mt_sync - synchronise on a mobile type */
TVM_INSTRUCTION (ins_mt_sync)
{
	WORDPTR mt = (WORDPTR) AREG;
	UWORD type = read_type (mt);
	
	switch (MT_BARRIER_TYPE (type)) {
		case MT_BARRIER_FULL:
			return tvm_bar_sync (ectx, mt);
		case MT_BARRIER_FORKING:
			{
				WORDPTR mb = wordptr_minus (mt, MT_BARRIER_PTR_OFFSET);
				UWORD refs = (UWORD) read_offset (mb, mt_barrier_internal_t, ref_count);
				
				if (refs <= 1) {
					TVM_FREE (mb);
				} else {
					WORKSPACE_SET (WPTR, WS_IPTR, (WORD) IPTR);
					write_offset (mb, mt_barrier_internal_t, ref_count, refs - 1);
					write_word (mt, (WORD) WPTR);
					RUN_NEXT_ON_QUEUE_RET();
				}
			}
			break;
		default:
			return ectx->set_error_flag (ectx, EFLAG_MT);
	}

	UNDEFINE_STACK_RET ();
}

/* 0x243 - 0x22 0x24 0xF3 - mt_xin - mobile type channel extended input */
TVM_INSTRUCTION (ins_mt_xin)
{
	WORDPTR chan_ptr	= (WORDPTR) AREG;
	WORDPTR dst		= (WORDPTR) BREG;
	WORDPTR other_WPTR	= (WORDPTR) read_word (chan_ptr);
	WORDPTR src		= (WORDPTR) WORKSPACE_GET (other_WPTR, WS_POINTER);

	/* Do input */
	return mt_chan_io (ectx, dst, src);
}

/* 0x244 - 0x22 0x24 0xF4 - mt_xout - mobile type channel extended output */
TVM_INSTRUCTION (ins_mt_xout)
{
	/* XXX: extended output not yet supported */
	return ECTX_INS_UNSUPPORTED;
}

/* 0x245 - 0x22 0x24 0xF5 - mt_xxchg - mobile type channel extended exchange */
TVM_INSTRUCTION (ins_mt_xxchg)
{
	WORDPTR chan_ptr = (WORDPTR) AREG;
	WORDPTR data_ptr = (WORDPTR) BREG;
	WORDPTR other_ptr;
	WORDPTR other_WPTR;

	other_WPTR = (WORDPTR) read_word (chan_ptr);
	other_ptr = (WORDPTR) WORKSPACE_GET (other_WPTR, WS_POINTER);

	swap_data_word (data_ptr, other_ptr);

	UNDEFINE_STACK_RET ();
}

/* 0x246 - 0x22 0x24 0xF6 - mt_dclone - clone data into a mobile type */
TVM_INSTRUCTION (ins_mt_dclone)
{
	WORDPTR dst	= (WORDPTR) NULL_P;
	WORDPTR src	= (WORDPTR) CREG;
	UWORD bytes	= (UWORD) BREG;
	UWORD type	= (UWORD) AREG;

	if (type == (MT_SIMPLE | MT_MAKE_TYPE (MT_DATA))) {
		if (bytes) {
			dst = mt_alloc_data (ectx, type, bytes);
			tvm_copy_data ((BYTEPTR) dst, (BYTEPTR) src, bytes);
		}
	} else {
		STACK ((WORD) dst, UNDEFINE(BREG), UNDEFINE(CREG));
		return ectx->set_error_flag (ectx, EFLAG_MT);
	}

	STACK_RET ((WORD) dst, UNDEFINE(BREG), UNDEFINE(CREG));
}

/* 0x247 - 0x22 0x24 0xF7 - mt_bind - bind a mobile type in some way to a bit of data */
TVM_INSTRUCTION (ins_mt_bind)
{
	BYTEPTR data	= (BYTEPTR) CREG;
	WORDPTR ptr	= (WORDPTR) BREG;
	UWORD bind_type	= (UWORD) AREG;
	UWORD type;
	int ret;

	type = read_type (ptr);

	if ((type & MT_SIMPLE) && (MT_TYPE(type) == MT_ARRAY)) {
		UWORD dimensions = MT_ARRAY_DIM(type);

		if (bind_type == MT_BIND_VIRTUAL || bind_type == MT_BIND_PHYSICAL) {
			BYTEPTR phys_addr, virt_addr;

			if (bind_type == MT_BIND_VIRTUAL) {
				virt_addr = data;
				phys_addr = virt_addr; /* translate if different */
			} else {
				phys_addr = data;
				virt_addr = phys_addr; /* translate if different */
			}

			if (MT_TYPE(MT_ARRAY_INNER_TYPE(type)) == MT_ARRAY_OPTS) {
				UWORD flags = MT_FLAGS(MT_ARRAY_INNER_TYPE(type));
				
				if (flags & MT_ARRAY_OPTS_SEPARATED) {
					BYTEPTR data = (BYTEPTR) read_offset (ptr, mt_array_t, data);
					if (data != NULL_P) {
						if ((ret = mt_release (ectx, (WORDPTR) data))) {
							return ret;
						}
					}
				}

				if (flags & MT_ARRAY_OPTS_DMA) {
					write_offset (ptr, mt_array_t, dimensions[dimensions], (WORD) phys_addr);
				}
			}

			write_offset (ptr, mt_array_t, data, (WORD) virt_addr);
		} else if (bind_type == MT_BIND_DMA) {
			WORDPTR ma	= wordptr_minus (ptr, MT_ARRAY_PTR_OFFSET);
			UWORD align	= 0;
			UWORD flags	= 0;
			UWORD inner	= MT_ARRAY_INNER_TYPE(type);
			int dma_ready	= 0;

			if (MT_TYPE(inner) == MT_ARRAY_OPTS) {
				if (MT_FLAGS(inner) & MT_ARRAY_OPTS_DMA) {
					/* already capable */
					dma_ready = 1;
				} else {
					align = MT_ARRAY_OPTS_ALIGN(inner);
					flags = MT_FLAGS(inner);
					inner = MT_ARRAY_OPTS_INNER(inner);
				}
			}
			
			if (dma_ready) {
				BYTEPTR data = (BYTEPTR) read_offset (ma, mt_array_internal_t, array.data);
				/* translate 'data' if different */

				write_offset (ma, mt_array_internal_t, type, MT_MAKE_ARRAY_TYPE (dimensions, MT_MAKE_ARRAY_OPTS (flags | MT_ARRAY_OPTS_DMA, align, inner)));
				write_offset (ma, mt_array_internal_t, array.dimensions[dimensions], (WORD) data);
			} else {
				UWORD old_type = read_type (ptr);
				WORDPTR new_ptr;

				write_offset (ma, mt_array_internal_t, type, MT_MAKE_ARRAY_TYPE (dimensions, MT_MAKE_ARRAY_OPTS (flags | MT_ARRAY_OPTS_DMA, align, inner)));
				if ((ret = mt_clone (ectx, ptr, &new_ptr))) {
					return ret;
				}
				write_offset (ma, mt_array_internal_t, type, old_type);
				
				if ((ret = mt_release (ectx, ptr))) {
					return ret;
				}

				ptr = new_ptr;
			}
		} else {
			STACK (NULL_P, UNDEFINE(BREG), UNDEFINE(CREG));
			return ectx->set_error_flag (ectx, EFLAG_MT);
		}
	} else {
		STACK (NULL_P, UNDEFINE(BREG), UNDEFINE(CREG));
		return ectx->set_error_flag (ectx, EFLAG_MT);
	}

	STACK_RET ((WORD) ptr, UNDEFINE(BREG), UNDEFINE(CREG));
}


/*{{{  void *tvm_mt_alloc (ECTX ectx, UWORD type, UWORD size)*/
void *tvm_mt_alloc (ECTX ectx, UWORD type, UWORD size)
{
	WORDPTR ptr;
	if (!mt_alloc (ectx, type, size, &ptr)) {
		return (void *) ptr;
	} else {
		return NULL;
	}
}
/*}}}*/

/*{{{  void *tvm_mt_release (ECTX ectx, void *ptr)*/
void tvm_mt_release (ECTX ectx, void *ptr)
{
	mt_release (ectx, (WORDPTR) ptr);
}
/*}}}*/

#endif /* TVM_DYNAMIC_MEMORY && TVM_OCCAM_PI */

