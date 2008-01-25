/*
tvm - ins_mt.c
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

#include "transputer.h"
#include "instructions.h"
#include "ext_chan.h"

#include "mem.h"
#include "pool_alloc.h"
#include "scheduler.h"

#include "ins_barrier.h"
#include "ins_chan.h"
#include "ins_fred.h"
#include "ins_mt.h"
#include "ins_pri.h"
#include "ins_sec.h"

#ifdef __MOBILE_PI_SUPPORT__

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
	ma		= (WORDPTR) palloc (bytes);
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

		i_cb 	= (WORDPTR) palloc (words << WSH);
		tvm_sem_init (wordptr_offset(i_cb, mt_cb_shared_internal_t, sem[0]));
		tvm_sem_init (wordptr_offset(i_cb, mt_cb_shared_internal_t, sem[1]));
		write_offset (i_cb, mt_cb_shared_internal_t, ref_count, 2);
		write_offset (i_cb, mt_cb_shared_internal_t, type, type);
		cb	= wordptr_plus (i_cb, MT_CB_SHARED_PTR_OFFSET);
	} else {
		words += MT_CB_PTR_OFFSET;
	
		i_cb 	= (WORDPTR) palloc (words << WSH);
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
	WORDPTR mb	= palloc (sizeof (mt_barrier_internal_t));
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
/*{{{  TVM_HELPER WORDPTR mt_alloc_data (UWORD type, UWORD size)*/
TVM_HELPER WORDPTR mt_alloc_data (UWORD type, UWORD size)
{	
	WORDPTR md;
	UWORD bytes = (size + (sizeof(UWORD) - 1)) & (~(sizeof(UWORD) - 1));
	
	bytes += MT_DATA_PTR_OFFSET << WSH;

	md = (WORDPTR) palloc (bytes);
	write_offset (md, mt_data_internal_t, size, size);
	write_offset (md, mt_data_internal_t, type, type);

	return wordptr_plus (md, MT_DATA_PTR_OFFSET);
}
/*}}}*/
/*{{{   TVM_HELPER void mt_release_simple (WORDPTR ptr, UWORD type)*/
TVM_HELPER void mt_release_simple (WORDPTR ptr, UWORD type)
{
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
							mt_release (inner_ptr);
						}
						data = wordptr_plus (data, 1);
					}
				}

				pfree (ma);
			}
			break;
		case MT_CB:
			{
				WORDPTR cb = wordptr_minus (ptr, MT_CB_PTR_OFFSET);
				UWORD refs = (UWORD) read_offset (cb, mt_cb_internal_t, ref_count);

				if (refs <= 1) {
					if (type & MT_CB_SHARED) {
						pfree (wordptr_minus (ptr, MT_CB_SHARED_PTR_OFFSET));
					} else {
						pfree (cb);
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
						tvm_bar_resign (ptr, 1);
					}
				} else {
					if (MT_BARRIER_TYPE(type) == MT_BARRIER_FORKING) {
						WORD process = read_word (ptr);
						if (process != NOT_PROCESS_P) {
							just_add_to_queue (process);
						}
					}
					pfree (mb);
				}
			}
			break;
		case MT_DATA:
			{
				WORDPTR md = wordptr_minus (ptr, MT_DATA_PTR_OFFSET);
				pfree (md);
			}
			break;
		default:
			set_error_flag (EFLAG_MT);
			break;
	}
}
/*}}}*/
/*{{{  TVM_HELPER void mt_release (WORDPTR ptr)*/
TVM_HELPER void mt_release (WORDPTR ptr)
{
	UWORD type = read_type (ptr);

	if (type & MT_SIMPLE) {
		mt_release_simple (ptr, type);
	} else {
		set_error_flag (EFLAG_MT);
	}
}
/*}}}*/
/*{{{  static WORDPTR mt_clone_array (WORDPTR ptr, UWORD type)*/
static WORDPTR mt_clone_array (WORDPTR ptr, UWORD type)
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
		copy_data ((BYTEPTR) dst_data, (BYTEPTR) src_data, size << size_shift);
	} else {
		while (size--) {
			WORDPTR inner_ptr = (WORDPTR) read_word (src_data);
			
			if (inner_ptr != (WORDPTR) NULL_P) {
				inner_ptr = mt_clone (inner_ptr);
			}
			write_word (dst_data, (WORD) inner_ptr);

			dst_data = wordptr_plus (dst_dim, 1);
			src_data = wordptr_plus (src_dim, 1);
		}
	}

	return wordptr_plus (dst, MT_ARRAY_PTR_OFFSET);
}	
/*}}}*/
/*{{{  static WORDPTR mt_clone_simple (WORDPTR ptr, UWORD type)*/
static WORDPTR mt_clone_simple (WORDPTR ptr, UWORD type)
{
	switch (MT_TYPE(type)) {
		case MT_ARRAY:
			return mt_clone_array (ptr, type);
		case MT_CB:
			{
				WORDPTR cb = wordptr_minus (ptr, MT_CB_PTR_OFFSET);
			 	UWORD refs = (UWORD) read_offset (cb, mt_cb_internal_t, ref_count);
				
				write_offset (cb, mt_cb_internal_t, ref_count, refs + 1);
			}
			return ptr;
		case MT_BARRIER:
			{
				WORDPTR mb = wordptr_minus (ptr, MT_BARRIER_PTR_OFFSET);
			 	UWORD refs = (UWORD) read_offset (mb, mt_barrier_internal_t, ref_count);
				
				write_offset (mb, mt_barrier_internal_t, ref_count, refs + 1);
				if (MT_BARRIER_TYPE(type) == MT_BARRIER_FULL) {
					tvm_bar_enroll (ptr, 1);
				}
			}
			return ptr;
		case MT_DATA:
			{
				WORDPTR src = wordptr_minus (ptr, MT_DATA_PTR_OFFSET);
				WORDPTR dst;
				UWORD size = (UWORD) read_offset (src, mt_data_internal_t, size);

				dst = mt_alloc_data (type, size);
				copy_data ((BYTEPTR) dst, (BYTEPTR) ptr, size);

				return dst;
			}
		default:
			set_error_flag (EFLAG_MT);
			return (WORDPTR) NULL_P;
	}
}
/*}}}*/
/*{{{  TVM_HELPER WORDPTR mt_clone (WORDPTR ptr)*/
TVM_HELPER WORDPTR mt_clone (WORDPTR ptr)
{
	UWORD type = read_type (ptr);

	if (type & MT_SIMPLE) {
		return mt_clone_simple (ptr, type);
	} else {
		set_error_flag (EFLAG_MT);
		return (WORDPTR) NULL_P;
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
/*{{{  static void mt_io_update_barrier (WORDPTR *ptr)*/
static void mt_io_update_barrier (WORDPTR *ptr)
{
	WORDPTR mb = wordptr_minus (*ptr, MT_BARRIER_PTR_OFFSET);
	UWORD refs = (UWORD) read_offset (mb, mt_barrier_internal_t, ref_count);
	UWORD type = read_type (*ptr);
	
	write_offset (mb, mt_barrier_internal_t, ref_count, refs + 1);
	if (MT_BARRIER_TYPE(type) == MT_BARRIER_FULL) {
		tvm_bar_enroll (*ptr, 1);
	}
}
/*}}}*/
/*{{{  static void mt_io_update_array (WORDPTR *ptr, UWORD inner)*/
static void mt_io_update_array (WORDPTR *ptr, UWORD inner)
{
	WORDPTR ma	= wordptr_minus (ptr, MT_ARRAY_PTR_OFFSET);
	WORDPTR data	= (WORDPTR) read_offset (ma, mt_array_internal_t, array.data);
	UWORD size	= (UWORD) read_offset (ma, mt_array_internal_t, size);

	switch (MT_TYPE(inner)) {
		case MT_ARRAY:
			inner = MT_ARRAY_INNER_TYPE (inner);
			while (size--) {
				WORDPTR inner_ptr = (WORDPTR) read_word (data);
				if (inner_ptr != (WORDPTR) NULL_P) {
					mt_io_update_array ((WORDPTR *) data, inner);
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
					mt_io_update_barrier ((WORDPTR *) data);
				}
				data = wordptr_plus (data, 1);
			}
			break;
		case MT_MT:
			while (size--) {
				WORDPTR inner_ptr = (WORDPTR) read_word ((WORDPTR *) data);
				if (inner_ptr != (WORDPTR) NULL_P) {
					mt_io_update ((WORDPTR *) data);
				}
				data = wordptr_plus (data, 1);
			}
			break;
		case MT_DATA:
			break;
		default:
			set_error_flag (EFLAG_MT);
			break;
	}
}
/*}}}*/
/*{{{  TVM_HELPER UWORD mt_io_update (WORDPTR *ptr)*/
TVM_HELPER UWORD mt_io_update (WORDPTR *ptr)
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

			mt_io_update_array (ptr, MT_ARRAY_INNER_TYPE(type));

			return MT_TRUE;
		} else if (MT_TYPE(type) == MT_CB) {
			if (type & MT_CB_SHARED) {
				mt_io_update_shared_cb (ptr);
				return MT_FALSE;
			} else {
				return MT_TRUE;
			}
		} else if (MT_TYPE(type) == MT_BARRIER) {
			mt_io_update_barrier (ptr);
			return MT_FALSE;
		} else if (MT_TYPE(type) == MT_DATA) {
			return MT_TRUE;
		}
	} else {
		set_error_flag (EFLAG_MT);
	}
	
	return MT_FALSE;
}
/*}}}*/
/*{{{  TVM_HELPER void mt_chan_io (WORDPTR dst, WORDPTR src)*/
TVM_HELPER void mt_chan_io (WORDPTR dst, WORDPTR src)
{
	/* Read pointer to mobile type */
	WORDPTR ptr = (WORDPTR) read_word (src);
	/* Is there anything there? */
	if (ptr != (WORDPTR) NULL_P) {
		/* Is defined, so update it */
		if (mt_io_update (&ptr)) {
			/* Pointer moved, delete old reference */
			write_word (src, (WORD) NULL_P);
		}
	}
	/* Write out possibly new pointer to mobile type */
	write_word (dst, (WORD) ptr);
}
/*}}}*/
/*{{{  TVM_HELPER WORDPTR mt_alloc (UWORD type, UWORD size)*/
TVM_HELPER WORDPTR mt_alloc (UWORD type, UWORD size)
{
	if (type & MT_SIMPLE) {
		switch (MT_TYPE(type)) {
			case MT_ARRAY:
				return mt_alloc_array (type, size);
			case MT_CB:
				return mt_alloc_cb (type, size);
			case MT_BARRIER:
				return mt_alloc_barrier (type);
			case MT_DATA:
				return mt_alloc_data (type, size);
			default:
				break;
		}
	}
	
	/* Should not end up here. */
	set_error_flag (EFLAG_MT);
	
	return (WORDPTR) NULL_P;
}
/*}}}*/

/* 0x238 - 0x22 0x23 0xF8 - mt_alloc - allocate a mobile type */
TVM_INSTRUCTION void ins_mt_alloc(void)
{
	areg = (WORD) mt_alloc ((UWORD) areg, (UWORD) breg);
	STACK (areg, UNDEFINE(breg), UNDEFINE(creg));
}

/* 0x239 - 0x22 0x23 0xF9 - mt_release - release a mobile type */
TVM_INSTRUCTION void ins_mt_release(void)
{
	mt_release ((WORDPTR) areg);
	UNDEFINE_STACK ();
}

/* 0x23A - 0x22 0x23 0xFA - mt_clone - clone a mobile type */
TVM_INSTRUCTION void ins_mt_clone(void)
{
	areg = (WORD) mt_clone ((WORDPTR) areg);
	STACK (areg, UNDEFINE(breg), UNDEFINE(creg));
}

/* 0x23B - 0x22 0x23 0xFB - mt_in - mobile type channel input */
TVM_INSTRUCTION void ins_mt_in(void)
{
	WORDPTR chan_ptr		= (WORDPTR) areg;
	WORDPTR dst		= (WORDPTR) breg;
	WORDPTR other_wptr	= chan_io_begin (0, chan_ptr, (BYTEPTR) dst);

	if (other_wptr != NOT_PROCESS_P)
	{
		/* Get pointer to source pointer */
		WORDPTR src = (WORDPTR) WORKSPACE_GET (other_wptr, WS_CHAN);
		/* Do input */
		mt_chan_io (dst, src);
		/* Complete channel operation */
		chan_io_end(chan_ptr, other_wptr);
	}

	UNDEFINE_STACK ();
}

/* 0x23C - 0x22 0x23 0xFC - mt_out - mobile type channel output */
TVM_INSTRUCTION void ins_mt_out(void)
{
	WORDPTR chan_ptr		= (WORDPTR) areg;
	WORDPTR src		= (WORDPTR) breg;
	WORDPTR other_wptr	= chan_io_begin(1, chan_ptr, (BYTEPTR) src);

	if (other_wptr != NOT_PROCESS_P)
	{
		/* Get pointer to destination */
		WORDPTR dst = (WORDPTR) WORKSPACE_GET (other_wptr, WS_CHAN);
		/* Do output */
		mt_chan_io (dst, src);
		/* Complete channel operation */
		chan_io_end(chan_ptr, other_wptr);
	}

	UNDEFINE_STACK ();
}

/* 0x23D - 0x22 0x23 0xFD - mt_xchg - mobile type channel exchange */
TVM_INSTRUCTION void ins_mt_xchg(void)
{
	WORDPTR chan_ptr = (WORDPTR) areg;
	WORDPTR data_ptr = (WORDPTR) breg;

	chan_swap (chan_ptr, data_ptr);
}

/* 0x23E - 0x22 0x23 0xFE - mt_lock - lock a mobile type */
TVM_INSTRUCTION void ins_mt_lock(void)
{
	WORDPTR mt = (WORDPTR) breg;
	WORDPTR cb = wordptr_minus (mt, MT_CB_SHARED_PTR_OFFSET);
	WORD lock = areg << 1;

	tvm_sem_claim (wordptr_plus (wordptr_offset (cb, mt_cb_shared_internal_t, sem), lock));

	UNDEFINE_STACK ();
}

/* 0x23F - 0x22 0x23 0xFF - mt_unlock - unlock a mobile type */
TVM_INSTRUCTION void ins_mt_unlock(void)
{
	WORDPTR mt = (WORDPTR) breg;
	WORDPTR cb = wordptr_minus (mt, MT_CB_SHARED_PTR_OFFSET);
	WORD lock = areg << 1;

	tvm_sem_release (wordptr_plus (wordptr_offset (cb, mt_cb_shared_internal_t, sem), lock));

	UNDEFINE_STACK ();
}

/* 0x240 - 0x22 0x24 0xF0 - mt_enroll - enroll processes on a mobile type */
TVM_INSTRUCTION void ins_mt_enroll(void)
{
	WORDPTR mt = (WORDPTR) breg;
	UWORD count = (UWORD) areg;
	UWORD type = read_type (mt);
	
	switch (MT_BARRIER_TYPE (type)) {
		case MT_BARRIER_FULL:
			tvm_bar_enroll (mt, count);
			break;
		default:
			set_error_flag (EFLAG_MT);
			break;
	}
	
	UNDEFINE_STACK ();
}

/* 0x241 - 0x22 0x24 0xF1 - mt_resign - resign process from a mobile type */
TVM_INSTRUCTION void ins_mt_resign(void)
{
	WORDPTR mt = (WORDPTR) breg;
	UWORD count = (UWORD) areg;
	UWORD type = read_type (mt);
	
	switch (MT_BARRIER_TYPE (type)) {
		case MT_BARRIER_FULL:
			tvm_bar_resign (mt, count);
			break;
		default:
			set_error_flag (EFLAG_MT);
			break;
	}

	UNDEFINE_STACK ();
}

/* 0x242 - 0x22 0x24 0xF2 - mt_sync - synchronise on a mobile type */
TVM_INSTRUCTION void ins_mt_sync(void)
{
	WORDPTR mt = (WORDPTR) areg;
	UWORD type = read_type (mt);
	
	switch (MT_BARRIER_TYPE (type)) {
		case MT_BARRIER_FULL:
			tvm_bar_sync (mt);
			break;
		case MT_BARRIER_FORKING:
			{
				WORDPTR mb = wordptr_minus (mt, MT_BARRIER_PTR_OFFSET);
				UWORD refs = (UWORD) read_offset (mb, mt_barrier_internal_t, ref_count);
				
				if (refs <= 1) {
					pfree (mb);
				} else {
					WORKSPACE_SET (wptr, WS_IPTR, (WORD) iptr);
					write_offset (mb, mt_barrier_internal_t, ref_count, refs - 1);
					write_word (mt, (WORD) wptr);
					iptr = run_next_on_queue ();
				}
			}
			break;
		default:
			set_error_flag (EFLAG_MT);
			break;
	}

	UNDEFINE_STACK ();
}

/* 0x243 - 0x22 0x24 0xF3 - mt_xin - mobile type channel extended input */
TVM_INSTRUCTION void ins_mt_xin(void)
{
	WORDPTR chan_ptr		= (WORDPTR) areg;
	WORDPTR dst		= (WORDPTR) breg;
	WORDPTR other_wptr	= (WORDPTR) read_word (chan_ptr);
	WORDPTR src		= (WORDPTR) WORKSPACE_GET (other_wptr, WS_CHAN);

	/* Do input */
	mt_chan_io (dst, src);

	UNDEFINE_STACK ();
}

/* 0x244 - 0x22 0x24 0xF4 - mt_xout - mobile type channel extended output */
TVM_INSTRUCTION void ins_mt_xout(void)
{
	/* XXX: extended output not yet supported */
	ins_not_implemented ();
}

/* 0x245 - 0x22 0x24 0xF5 - mt_xxchg - mobile type channel extended exchange */
TVM_INSTRUCTION void ins_mt_xxchg(void)
{
	WORDPTR chan_ptr = (WORDPTR) areg;
	WORDPTR data_ptr = (WORDPTR) breg;
	WORDPTR other_ptr;
	WORDPTR other_wptr;

	other_wptr = (WORDPTR) read_word (chan_ptr);
	other_ptr = (WORDPTR) WORKSPACE_GET (other_wptr, WS_CHAN);

	swap_data_word (data_ptr, other_ptr);

	UNDEFINE_STACK ();
}

/* 0x246 - 0x22 0x24 0xF6 - mt_dclone - clone data into a mobile type */
TVM_INSTRUCTION void ins_mt_dclone(void)
{
	WORDPTR dst = (WORDPTR) NULL_P;
	WORDPTR src = (WORDPTR) creg;
	UWORD bytes = (UWORD) breg;
	UWORD type = (UWORD) areg;

	if (type == (MT_SIMPLE | MT_MAKE_TYPE (MT_DATA))) {
		if (bytes) {
			dst = mt_alloc_data (type, bytes);
			copy_data ((BYTEPTR) dst, (BYTEPTR) src, bytes);
		}
	} else {
		set_error_flag (EFLAG_MT);
	}

	STACK ((WORD) dst, UNDEFINE(breg), UNDEFINE(creg));
}

/* 0x247 - 0x22 0x24 0xF7 - mt_bind - bind a mobile type in some way to a bit of data */
TVM_INSTRUCTION void ins_mt_bind(void)
{
	BYTEPTR data	= (BYTEPTR) creg;
	WORDPTR ptr	= (WORDPTR) breg;
	UWORD bind_type	= (UWORD) areg;
	UWORD type;

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
						mt_release ((WORDPTR) data);
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
				new_ptr = mt_clone (ptr);
				write_offset (ma, mt_array_internal_t, type, old_type);
				
				mt_release (ptr);

				ptr = new_ptr;
			}
		} else {
			set_error_flag (EFLAG_MT);
		}
	} else {
		set_error_flag (EFLAG_MT);
	}

	STACK ((WORD) ptr, UNDEFINE(breg), UNDEFINE(creg));
}


/*{{{  void *tvm_mt_alloc (UWORD type, UWORD size)*/
void *tvm_mt_alloc (UWORD type, UWORD size)
{
	return (void *) mt_alloc (type, size);
}
/*}}}*/

/*{{{  void *tvm_mt_release (void *ptr)*/
void tvm_mt_release (void *ptr)
{
	mt_release ((WORDPTR) ptr);
}
/*}}}*/

#endif /* __MOBILE_PI_SUPPORT__ */

