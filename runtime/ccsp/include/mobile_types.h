/*
 *	mobile_types.h -- mobile type constants and structures 
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

#ifndef __MOBILE_TYPES_H
#define __MOBILE_TYPES_H

/*{{{  structure packing */
#if defined(MT_DEFINES) && !defined(_PACK_STRUCT)
#ifdef __GNUC__
#define _PACK_STRUCT __attribute__ ((packed))
#else
#warning "Unable to enforce alignment and packing on structures."
#define _PACK_STRUCT
#endif
#endif /* defined(MT_DEFINES) && !defined(_PACK_STRUCT) */
/*}}}*/

/*{{{  type definitions */
#if defined(MT_DEFINES)
#if defined(MT_TVM)

#define word WORD

typedef struct _mt_barrier_t { 
	WORD data[4];
} _PACK_STRUCT mt_barrier_t;

typedef struct _mt_sem_t {
	WORD data[2];
} _PACK_STRUCT mt_sem_t;

#elif defined(MT_CCSP)

typedef ccsp_barrier_t mt_barrier_t;
typedef ccsp_sem_t mt_sem_t;

#else /* !MT_TVM && !MT_CCSP */

struct _mt_barrier_t { word _do_not_use[1]; };
struct _mt_sem_t { word _do_not_use[1]; };

typedef struct _mt_barrier_t mt_barrier_t;
typedef struct _mt_sem_t mt_sem_t;

#endif /* !MT_TVM && !MT_CCSP */
#endif /* MT_DEFINES */
/*}}}*/

/* generation 2 mobile types for describing all data */
/* 
 * The mobile type is a structure contained in a single
 * unsigned machine word.
 * 
 * N                     5             1             0 LSB
 * | type specific flags | type number | simple flag |
 * 
 * If the LSB is set then the word is a self-contained
 * description of a mobile type or a so call "simple"
 * mobile type.  If the simple flag is 0 then the word is
 * a pointer to an mobile type descriptor (described below).
 *
 * For simple (self-contained) types, bits 1 to 4 (inclusive) define 
 * the type number (0 to 15), and remaining bits, 5 to N - 1 (inclusive) 
 * are flags specific to that type.
 *
 * Structured mobile types are described by mobile type descriptors,
 * see type 6 below for a description of mobile type descriptors.
 */

#define MT_SIMPLE	0x1
#define MT_TYPE_SHIFT	1
#define MT_TYPE(X)	(((X) >> MT_TYPE_SHIFT) & 0xf)
#define MT_MAKE_TYPE(X)	((X) << MT_TYPE_SHIFT)
#define MT_FLAGS_SHIFT	5
#define MT_FLAGS(X)	((X) >> MT_FLAGS_SHIFT)
#define MTType		(-1) /* offset of type word */

/*{{{ TYPE 0: numeric data 
 *  - flag bits 0-2 code the type:
 *    0 = BYTE,
 *    1 = INT16,
 *    2 = INT32,
 *    3 = INT64,
 *    4 = REAL32,
 *    5 = REAL64,
 *    6 = reserve for future use,
 *    7 = next 8 flag bits indicate type.
 */
#define MT_NUM			0
#define MT_NUM_TYPE(X)		mt_num_type(MT_FLAGS(X))
#define MT_MAKE_NUM(I)		\
	(MT_SIMPLE | MT_MAKE_TYPE(MT_NUM) | ((I) << MT_FLAGS_SHIFT))
#define MT_NUM_BYTE		0
#define MT_NUM_INT16		1
#define MT_NUM_INT32		2
#define MT_NUM_INT64		3
#define MT_NUM_REAL32		5
#define MT_NUM_REAL64		6

#ifdef MT_DEFINES
/*{{{  static inline word mt_num_type (word flags)*/
static inline word mt_num_type (word flags)
{
	if ((flags & 0x7) < 0x7) {
		return flags & 0x7;
	} else {
		return (flags >> 3) & 0xff;
	}
}
/*}}}*/
/*{{{  static inline word mt_num_size_shift (word type)*/
/*
 * Give size of the given numeric type as log2 bytes.
 */
static inline word mt_num_size_shift (word type)
{
	return (type >> 2) + (type & 0x3);
}
/*}}}*/
#endif /* MT_DEFINES */

/*}}}*/
/*{{{ TYPE 1: mobile array
 *   - flag bits 0-2 code number of dimensions - 1,
 *   - remaining flag bits contain element type.
 *
 *   In memory arrays are allocated as:
 *
 *     struct mt_array_t {
 *       void *data;
 *       word dimensions[];
 *     };
 *   
 *   Maintaining the values in dimensions is the
 *   responsibility of the allocating code.
 *
 *   Where possible the element type should be coded
 *   as completely as possible, to prevent wasteful
 *   walking of structures by the run-time.
 */
#define MT_ARRAY		1
#define MT_ARRAY_DIM(X)		(1 + (MT_FLAGS(X) & 0x7))
#define MT_ARRAY_INNER_TYPE(X)	((X) >> (MT_FLAGS_SHIFT + 3))
#define MT_MAKE_ARRAY_TYPE(D,I)		\
	(MT_SIMPLE			| \
	 MT_MAKE_TYPE (MT_ARRAY)	| \
	 ((D - 1) << MT_FLAGS_SHIFT)	| \
	 ((I) << (MT_FLAGS_SHIFT + 3)))

#ifdef MT_DEFINES
/* externally visible array structure */
typedef struct _mt_array_t {
	void *data;
	word dimensions[];
} _PACK_STRUCT mt_array_t;

/* internally visible array structure */
#define MT_ARRAY_PTR_OFFSET 	2
typedef struct _mt_array_internal_t {
	word		size;
	word		type;

	mt_array_t	array;
} _PACK_STRUCT mt_array_internal_t;
#endif /* MT_DEFINES */

/*}}}*/
/*{{{ TYPE 2: mobile channel bundle
 *   - flag bit 0 indicates sharing (0 = unshared),
 *   - flag bit 1 indicate whether to allocate additional space
 *     for pony/etc support (0 = don't allocate).
 *   - flag bits 3-11 code number of channels.
 */
#define MT_CB			2
#define MT_CB_SHARED		(0x1 << MT_FLAGS_SHIFT)
#define MT_CB_STATE_SPACE	(0x2 << MT_FLAGS_SHIFT)
#define MT_CB_CHANNELS_SHIFT	(MT_FLAGS_SHIFT + 3)
#define MT_CB_CHANNELS(X)	(((X) >> MT_CB_CHANNELS_SHIFT) & 0xFF)
/* lock constants */
#define MT_CB_CLIENT		0x0
#define MT_CB_SERVER		0x1

#ifdef MT_DEFINES
/* externally visible channel bundle structure */
typedef struct _mt_cb_t {
	/* channels should be defined as [],
	 * however C99 prohibits this because
	 * no other elements precede it.
	 */
	
	word		channels[255];
	/* word		g1_type; */

	/* a spare word is allocated beyond
	 * the channel array to hold the
	 * generation 1 typing field.
	 */
} _PACK_STRUCT mt_cb_t;

/* internally visible channel bundle structures */
#define MT_CB_PTR_OFFSET 	2
typedef struct _mt_cb_internal_t {
	word		ref_count;
	word		type;

	mt_cb_t		cb;
} _PACK_STRUCT mt_cb_internal_t;

#define MT_CB_SHARED_PTR_OFFSET \
	(2 + (2 * (sizeof(mt_sem_t) / sizeof(word))))
typedef struct _mt_cb_shared_internal_t {
	mt_sem_t	sem[2];
	word		ref_count;
	word		type;

	mt_cb_t		cb;
} _PACK_STRUCT mt_cb_shared_internal_t;
#endif /* MT_DEFINES */

/*}}}*/
/*{{{ TYPE 3: mobile barrier
 *   - flags bits 0-2 code type.
 */
#define MT_BARRIER		3
#define MT_BARRIER_TYPE(X)	MT_FLAGS(X)
#define MT_MAKE_BARRIER(T)	\
	(MT_SIMPLE | MT_MAKE_TYPE (MT_BARRIER) | (T) << MT_FLAGS_SHIFT)
#define MT_BARRIER_FULL		0
#define MT_BARRIER_FORKING	1
#define MT_BARRIER_MPROC	2

#ifdef MT_DEFINES

/* internally visible barrier structure */
#define MT_BARRIER_PTR_OFFSET	2
typedef struct _mt_barrier_internal_t {
	word		ref_count;
	word		type;

	mt_barrier_t	barrier;
} _PACK_STRUCT mt_barrier_internal_t;
#endif /* MT_DEFINES */

/*}}}*/
/*{{{ TYPE 4: mobile process
 *   - presently undefined.
 */
#define MT_PROCESS		4
/*}}}*/
/*{{{ TYPE 5: mobile type
 *   - flag bits are unused, should be 0.
 *     In future they maybe used as hinting
 *     information, e.g. to hint the likely
 *     type of this mobile.
 *
 *   Codes any mobile type, this is generally the
 *   inner type of arrays holding other mobiles.
 *
 *   Where possible a more specific type should
 *   be used in preference to this more general
 *   definition.
 */
#define MT_MT			5
/*}}}*/
/*{{{ TYPE 6: mobile type descriptor
 *   - flag bits are unused, should be 0.
 */
#define MT_DESC			6

#ifdef MT_DEFINES
/* externally visible mobile type structure */
typedef struct _mt_desc_t {
	word	n_descriptors;
	word	descriptors[];
} _PACK_STRUCT mt_desc_t;

/* internally visible mobile type structure */
#define MT_DESC_PTR_OFFSET 	5
#define MT_DESC_FLAG_STATIC	0x1	/* type not internally mobile */
#define MT_DESC_FLAG_SIMPLE	0x2	/* type only contains mobile data */
typedef struct _mt_desc_internal_t {
	word		ref_count; 	/* unsure about this */
	word		hash;
	word		instance_bytes;
	word		flags;
	word		type;

	mt_desc_t	mt;
} _PACK_STRUCT mt_desc_internal_t;
#endif /* MT_DEFINES */

/*}}}*/
/*{{{ TYPE 7: untyped mobile data
 *	- no flags presently defined, but may be used in
 *	  future to define sharing, invariants, etc.
 */
#define MT_DATA			7

#ifdef MT_DEFINES
/* externally visible mobile data structure */
typedef struct _mt_data_t {
	word		data[1];
} _PACK_STRUCT mt_data_t;

/* internally visible mobile data structure */
#define MT_DATA_PTR_OFFSET 	2
typedef struct _mt_data_internal_t {
	word		size;
	word		type;

	mt_data_t	data;
} _PACK_STRUCT mt_data_internal_t;
#endif /* MT_DEFINES */

/*}}}*/
/*{{{ TYPE 8: fixed array type
 *   - special type for coding mobile type descriptors,
 *     flag bits indicate array length (in elements),
 *     the following word indicates element type.
 *
 *   This should be used to code all static data in
 *   complex mobile types.
 */
#define MT_FARRAY		8
#define MT_FARRAY_LEN(X) 	MT_FLAGS(X)
/*}}}*/
/*{{{ TYPE 9: array options
 *   - special type for coding mobile arrays with
 *     alignment and flags such as DMA.
 *     Flag bits code options:
 *      bit 0 = DMA,
 *      bit 1 = data is separately allocated (so free as well),
 *      bit 2 = unused.
 *     With the next 4 bits coding a power of 2 
 *     alignment, after which follows the
 *     arrays inner type.
 */
#define MT_ARRAY_OPTS		9
#define MT_ARRAY_OPTS_DMA	0x1
#define MT_ARRAY_OPTS_SEPARATED	0x2
#define MT_ARRAY_OPTS_ALIGN(X)	(((X) >> (MT_FLAGS_SHIFT + 3)) & 0xf)
#define MT_ARRAY_OPTS_INNER(X)	((X) >> (MT_FLAGS_SHIFT + 7))
#define MT_MAKE_ARRAY_OPTS(F,A,I)	\
	(MT_SIMPLE				| \
	 MT_MAKE_TYPE (MT_ARRAY_OPTS)		| \
	 ((F) << MT_FLAGS_SHIFT)		| \
	 (((A) & 0xf) << (MT_FLAGS_SHIFT + 3))	| \
	 ((I) << (MT_FLAGS_SHIFT + 7)))
/*}}}*/

/*{{{  Constants for mobile type binding */
#define MT_BIND_VIRTUAL		1	/* bind a physical address */
#define MT_BIND_PHYSICAL	2	/* bind a virtual address */
#define MT_BIND_DMA		3	/* re-bind memory so it's DMA capable */
/*}}}*/

/*{{{  Constants for mobile type resizing */
#define MT_RESIZE_DATA		1	/* resize data */
/*}}}*/

/* Examples of generation 2 encoding:
 *
 *  MOBILE []BYTE
 *   is MT_ARRAY with flags = 0, and MT_NUM with flags = 0.
 *
 *  MOBILE []MOBILE BARRIER
 *   is MT_ARRAY with flags = 0, and MT_BARRIER.
 *
 *  MOBILE [][][]INT32
 *   is MT_ARRAY with flags = 2, and MT_NUM with flags = 2.
 *
 *  DATA TYPE FOO
 *    RECORD
 *      INT32 x:
 *      INT32 y:
 *  :
 *  MOBILE []FOO:
 *   is MT_ARRAY with flags = 0, and MT_NUM with flags = 0.
 *   i.e. an array of bytes, the allocating code then sets up
 *    the dimension field appropriately.
 *  
 *  -- imagined syntax
 *  DATA TYPE BAR
 *    MOBILE RECORD
 *      [32]BYTE x:
 *      MOBILE []BYTE y:
 *      INT32 z:
 *  :
 *   Is coded by a mobile type descriptor with as follows:
 *     n_descriptors = 4,
 *     descriptors[] = {
 *       MT_FARRAY with flags = 32,
 *       MT_NUM with flags = 0,
 *       MT_ARRAY with flags = 0 and MT_NUM with flags = 0,
 *       MT_NUM with flags = 2
 *     }
 *   However there are several valid encodings, e.g.:
 *     n_descriptors = 5,
 *     descriptors[] = {
 *       MT_FARRAY with flags = 32,
 *       MT_NUM with flags = 0,
 *       MT_MT,
 *       MT_FARRAY with flags = 1,
 *       MT_NUM with flags = 2
 *     }
 *   The compiler/application is free to use any valid encoding,
 *   but should generally try to use the simplest possible.
 */

#endif	/* !__MOBILE_TYPES_H */

