/*
 *	dmem.c -- dynamic memory allocator
 *	Copyright (C) 2000-2005 Fred Barnes  <frmb@kent.ac.uk>
 *	Modifications (C) 2002 Brian Vinter
 *	Based on Per Brinch Hansen's parallel allocator as adapted by David Wood
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

#define USE_MALLOC_INSTEAD
//#define DMEM_TRACE

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/*{{{  includes*/
#if defined(OOS_BUILD)
	#include <oos_funcs.h>
#else	/* !OOS_BUILD */
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
#endif	/* !OOS_BUILD */

#include <kernel.h>
#include <rts.h>

#include <dmem_if.h>
#include <typedesc.h>
#include <arch/asm_ops.h>
#include <arch/atomics.h>
/*}}}*/

/*{{{ constants */
#define BLOCK_SHIFT		20
#define BLOCK_SIZE_MB		(1U << (BLOCK_SHIFT - 20))
#define BLOCK_SIZE 		(1U << BLOCK_SHIFT)
#define MIN_ALLOC		8
#define INIT_BLOCKS 		MIN_ALLOC
#define MAX_BLOCKS 		(4096 / BLOCK_SIZE_MB)

#if MAX_RUNTIME_THREADS > 1
#define MIN_POOL_BITS	6
#else
#define MIN_POOL_BITS	5
#endif
#define N_POOLS 	((BLOCK_SHIFT * 2) - ((MIN_POOL_BITS * 2) - 1))

#define SMASH_BITS	5
#define SMASH_AREAS	(1U << SMASH_BITS)
#define SMASH_AREA_SIZE (BLOCK_SIZE / SMASH_AREAS)
#define SMASH_MASK	((1U << SMASH_BITS) - 1U)
#define SMASH_MAX_POOL	16

enum block_type {
	BT_INVALID	= 0x00,
	BT_FREE		= 0x01,
	BT_ALLOCATED	= 0x20,
	BT_SMASHED	= 0x21,
	BT_DIRTY	= 0x22,
	BT_CODE		= 0x40,
	BT_MAP		= 0x80
};
/*}}}*/
/*{{{ type definitions */
#ifdef __GNUC__
#define _PACK_STRUCT __attribute__ ((packed))
#else
#warning "Unable to enforce alignment and packing on structures."
#define _PACK_STRUCT
#endif

typedef struct _allocation_t	allocation_t;
typedef struct _allocator_t	allocator_t;
typedef struct _block_desc_t	block_desc_t;
typedef struct _block_map_t	block_map_t;

struct _allocation_t {
	allocation_t	*next;
	allocation_t	*gnext;
	word		size;
} _PACK_STRUCT;

struct _allocator_t {
	word		number;
	block_desc_t	*smash_list;
	allocation_t	*pool[N_POOLS];
#if N_POOLS <= 30
	word		pad[32 - (N_POOLS + 2)];
#else
	word		pad[64 - (N_POOLS + 2)];
#endif
} _PACK_STRUCT;

struct _block_desc_t {
	/* primary descriptor */
	word		number;
	word		type;
	word		pool;
	block_desc_t	*next;
	
	/* allocation chain */
	word		first;
	word		last;
	word		pad0[10];

	/* pool components */
	word		taken;
	word		in_use;
	allocation_t	*free;
	word		pad1[13];

	/* per-thread scratch space */
	allocation_t	*scratch[MAX_RUNTIME_THREADS];
	word		pad2[32 - MAX_RUNTIME_THREADS];
} _PACK_STRUCT;

struct _block_map_t {
	block_desc_t	*free;
	word		allocators;
	word		zero_block;
	word		pad0[29];

	block_desc_t	*pool[N_POOLS];
#if N_POOLS <= 32
	word		pad1[32 - N_POOLS];
#else
	word		pad1[64 - N_POOLS];
#endif

	allocator_t	allocator[MAX_RUNTIME_THREADS];

	block_desc_t	desc[MAX_BLOCKS];
} _PACK_STRUCT;
/*}}}*/
/*{{{  variables */
static block_map_t *bmap = NULL;
#if defined(DMEM_TRACE)
static FILE *trace	 = NULL;
#endif
/*}}}*/
/*{{{  atomics and helpers */
#define PTR_MASK		(~((1 << MIN_POOL_BITS) - 1))
#define	PTR_VAL(X)		((void *) (((word) (X)) & PTR_MASK))
#define READ_RAW_PTR(P)		((void *) (atw_val ((word *) &(P))))
#define READ_PTR(P)		(PTR_VAL (READ_RAW_PTR (P)))
#define MAKE_PTR(P,I)		((void *) (((word) PTR_VAL ((P))) | (I)))
#define CAS_PTR(P,O,N)		(atw_cas ((word *) &(P), (word) (O), (word) (N)))
#define SWAP_PTR(P,N)		((void *) atw_swap ((word *) &(P), (word) (N)))
/*}}}*/
/*{{{  inline functions */
static inline word block_number (void *ptr) {
	return ((word) ptr) >> BLOCK_SHIFT;
}

static inline void *block_address (word bn) {
	return (void *)(bn << BLOCK_SHIFT);
}

static inline word size_to_pool (word size) {
	word msb = bsr (size) & 0x1f;
	word pool, mask, temp;

	pool = msb - MIN_POOL_BITS;
	mask = ~ (one_if_z (pool - 1, 1 << ((sizeof(word) * 8) - 1)) - 1);
	pool = (pool & mask);
	temp = (1 << (msb - 1));
	pool <<= 1;					/* increase by 2 for every power2 */
	pool += one_if_nz (size, temp) & mask;		/* move up one if half-power2 */
	pool += one_if_nz (size, temp - 1) & mask;	/* move up one if not exact fit */
	pool += one_if_z (pool, ~1) & mask; 		/* never use pool 1 */

	return pool;
}

static inline word pool_to_size (word pool) {
	word msb = (pool >> 1) + MIN_POOL_BITS;
	return (1 << msb) | ((pool & 1) << (msb - 1));
}

static inline word pool_group_size (word pool) {
	word mask = one_if_nz (pool, ~0xf) - 1;
	return ((0x800 >> (pool >> 1)) & mask) | (1 & (~mask));
}

static inline word smashed_address_to_pool (void *ptr) {
	word addr = (word) ptr;
	word area = (addr >> (BLOCK_SHIFT - SMASH_BITS)) & SMASH_MASK;
	area >>= 1;
	return area + (1 & (one_if_z (area, 0xf) - 1));
}
/*}}}*/
/*{{{  block allocator (malloc driven)*/
#if !defined(OOS_BUILD)
/*{{{  static inline void *malloc_blocks (word count)*/
static void *malloc_blocks (word count)
{
	#if 0
	void *mem = memalign (BLOCK_SIZE, BLOCK_SIZE * count);
	TRACE ("+ blocks: %d => %p", count, mem);
	return mem;
	#endif
}
/*}}}*/
/*{{{  static void release_free_blocks (allocator_t *al, block_desc_t *first, block_desc_t *last)*/
static void release_free_blocks (allocator_t *al, block_desc_t *first, block_desc_t *last)
{
	void *ptr;
	do {
		ptr		= READ_RAW_PTR (bmap->free);
		last->next	= (block_desc_t *) PTR_VAL (ptr);
		TRACE ("- block: %p", PTR_VAL(ptr));
	} while (!CAS_PTR (bmap->free, ptr, MAKE_PTR (first, al->number)));
}
/*}}}*/
/*{{{  static void release_pool_block (allocator_t *al, block_desc_t *block)*/
static void release_pool_block (allocator_t *al, block_desc_t *block)
{
	block->type = BT_FREE;
	weak_write_barrier ();
	release_free_blocks (al, block, block);
}
/*}}}*/
/*{{{  static void release_continguous_blocks (block_desc_t *first)*/
static void release_continguous_blocks (block_desc_t *first)
{
	block_desc_t *b = first;
	
	do {
		b->type = BT_INVALID;
		b 	= b->next;
	} while (b != NULL);
	
	TRACE ("- blocks: %p", block_address (first->number));
	free (block_address (first->number));
}
/*}}}*/
/*{{{  static void init_blocks (void *memory, word count, block_desc_t **first, block_desc_t **last)*/
static void init_blocks (void *memory, word count, block_desc_t **first, block_desc_t **last)
{
	word first_bn 	= block_number (memory);
	word last_bn 	= first_bn + (count - 1);
	word bn		= first_bn;

	*first	= &(bmap->desc[first_bn]);
	*last	= &(bmap->desc[last_bn]);

	while (count--) {
		block_desc_t *desc = &(bmap->desc[bn]);
		int i;

		desc->type	= BT_FREE;
		desc->next	= &(bmap->desc[bn + 1]);
		desc->first	= first_bn;
		desc->last	= last_bn;

		desc->taken	= 0;
		desc->in_use	= 0;
		desc->free	= NULL;

		for (i = 0; i < MAX_RUNTIME_THREADS; ++i)
			desc->scratch[i] = NULL;

		bn++;
	}

	(*last)->next = NULL;
}
/*}}}*/
/*{{{  static block_desc_t *allocate_pool_block (allocator_t *al)*/
static block_desc_t *allocate_pool_block (allocator_t *al)
{
	block_desc_t *block, *last;
	unsigned char *blocks;
	void *ptr;

	while (PTR_VAL (ptr = READ_RAW_PTR (bmap->free)) != NULL) {
		void *our_ptr 	= MAKE_PTR (ptr, al->number);
		block 		= (block_desc_t *) PTR_VAL (ptr);

		if (ptr != our_ptr) {
			if (!CAS_PTR (bmap->free, ptr, our_ptr)) {
				continue;
			}
		}
		
		if (CAS_PTR (bmap->free, our_ptr, MAKE_PTR (block->next, al->number))) {
			block->type = BT_ALLOCATED;
			block->next = NULL;
			return block;
		}
	}
	
	blocks = malloc_blocks (MIN_ALLOC);
				
	init_blocks (blocks, MIN_ALLOC, &block, &last);
	release_free_blocks (al, block->next, last);

	block->type = BT_ALLOCATED;
	block->next = NULL;

	TRACE ("%p + pool block: %d", al, block->number);

	return block;
}
/*}}}*/
/*{{{  static block_desc_t *allocate_contiguous_blocks (word count)*/
static block_desc_t *allocate_contiguous_blocks (word count)
{
	block_desc_t *first, *last;
	unsigned char *blocks = malloc_blocks (count);

	init_blocks (blocks, count, &first, &last);

	last = first;
	do {
		last->type = BT_ALLOCATED;
		last = last->next;
	} while (last != NULL);

	return first;
}
/*}}}*/
/*{{{  static void init_block_map (void)*/
static void init_block_map (void)
{
	block_desc_t *first, *last;
	unsigned char *blocks = malloc_blocks (INIT_BLOCKS);
	word i;

	bmap = (block_map_t *) blocks;
	bmap->free 		= NULL;
	bmap->allocators 	= 0;

	for (i = 0; i < N_POOLS; ++i) {
		bmap->pool[i]	= NULL;
	}

	for (i = 0; i < MAX_RUNTIME_THREADS; ++i) {
		allocator_t *al	= &(bmap->allocator[i]);
		word j;

		al->number = i;
		for (j = 0; j < N_POOLS; ++j) {
			al->pool[j] = NULL;
		}
	}

	for (i = 0; i < MAX_BLOCKS; ++i) {
		block_desc_t *desc = &(bmap->desc[i]);
		desc->number	= i;
		desc->type 	= BT_INVALID;
		desc->next	= NULL;
	}
	
	init_blocks (blocks, INIT_BLOCKS, &first, &last);

	for (i = 0; i < ((sizeof (block_map_t) + (BLOCK_SIZE - 1)) / BLOCK_SIZE); ++i) {
		block_desc_t *next = first->next;
		first->type = BT_MAP;
		first->next = NULL;
		first = next;
	}

	bmap->free = first;
}
/*}}}*/
#endif
/*}}}*/
/*{{{  memory allocator*/
/*{{{  static void smash_block (allocator_t *al, block_desc_t *block)*/
static void smash_block (allocator_t *al, block_desc_t *block)
{
	unsigned char *mem = (unsigned char *) block_address (block->number);
	word i;

	block->type 	= BT_SMASHED;
	block->pool	= 0;
	block->taken	= 0;
	block->in_use 	= 0;
	block->free	= NULL;

	for (i = 0; i < SMASH_AREAS; ++i) {
		word 		pn 	= smashed_address_to_pool (mem);
		word 		size 	= pool_to_size (pn);
		word 		offset	= 0;
		allocation_t	 *ap 	= (allocation_t *) mem;

		while ((offset + size) <= SMASH_AREA_SIZE) {
			ap		= (allocation_t *) (mem + offset);
			offset		+= size;
			ap->next 	= (allocation_t *) (mem + offset);
			ap->size 	= 0; /* intentionally hide size of smash group */
		}

		ap->next 	= al->pool[pn];
		al->pool[pn] 	= (allocation_t *) mem;
		block->in_use	+= offset;
		mem 		+= SMASH_AREA_SIZE;
	}
}
/*}}}*/
/*{{{  static void scavenge_smash_list (allocator_t *al) */
static void scavenge_smash_list (allocator_t *al)
{
	block_desc_t *b = al->smash_list;
	do {	
		if (READ_PTR (b->free) != NULL) {
			allocation_t *p = PTR_VAL (SWAP_PTR (b->free, MAKE_PTR (NULL, al->number)));
			word bytes 	= 0;
			while (p != NULL) {
				allocation_t 	*end 	= p;
				word 		pn 	= smashed_address_to_pool ((void *) p);

				bytes 		+= p->size * pool_to_size (pn); 

				while (end->next != NULL) {
					end->size 	= 0;
					end 		= end->next;
				}
				
				end->size 	= 0;
				end->next 	= al->pool[pn];
				al->pool[pn]	= p;
				p 		= p->gnext;
			}
			atw_add (&(b->in_use), bytes);
		}
		
		b = b->next;
	} while (b != NULL);
}
/*}}}*/
/*{{{  static allocation_t *allocate_to_pool (allocator_t *al, word pn)*/
static allocation_t *allocate_to_pool (allocator_t *al, word pn)
{
	TRACE ("%p allocate_to_pool: %d", al, pn);
	for (;;) {
		block_desc_t *block;
		allocation_t *mem = NULL;

		if ((block = (block_desc_t *) READ_PTR (bmap->pool[pn])) != NULL) {
			word group, taken;
			void *ptr;

			atw_inc (&(block->in_use));

			if (atw_val (&(block->type)) < BT_ALLOCATED || atw_val (&(block->pool)) != pn) {
				if (atw_dec_z (&(block->in_use))) {
					strong_read_barrier ();
					if (atw_val (&(block->type)) == BT_DIRTY) {
						release_pool_block (al, block);
					}
				}
				continue;
			}

			while (PTR_VAL (ptr = READ_RAW_PTR (block->free)) != NULL) {
				void *our_ptr 	= MAKE_PTR (ptr, al->number);
				mem 		= (allocation_t *) PTR_VAL (ptr);

				if (ptr != our_ptr) {
					if (!CAS_PTR (block->free, ptr, our_ptr)) {
						continue;
					}
				}
				
				if (CAS_PTR (block->free, our_ptr, MAKE_PTR (mem->gnext, al->number))) {
					if (mem->size > 1) {
						atw_add (&(block->in_use), mem->size - 1);
					}
					return mem;
				}
			}
			
			group = pool_group_size (pn);

			while ((taken = atw_val (&(block->taken)))) {
				word offset = taken - group;
				
				if (atw_cas (&(block->taken), taken, offset)) {
					unsigned char *bp = (unsigned char *) block_address (block->number);
					word size = pool_to_size (pn);
					allocation_t *ap;

					atw_add (&(block->in_use), group - 1);
					mem 	= (allocation_t *) (bp + (offset * size));
					ap	= mem;
					
					while (--group) {
						ap->next = (allocation_t *) (((unsigned char *) ap) + size);
						ap->size = group + 1;
						ap = ap->next;
					}

					ap->next = NULL;
					ap->size = 1;

					return mem;
				}
			}

			atw_set (&(block->type), BT_DIRTY);
			CAS_PTR (bmap->pool[pn], block, NULL);
			if (atw_dec_z (&(block->in_use))) {
				release_pool_block (al, block);
			}
		} else {
			word size 	= pool_to_size (pn);
			block 		= allocate_pool_block (al);
			block->pool 	= pn;
			block->taken 	= BLOCK_SIZE / size;
			block->in_use	= 0;
			block->free	= NULL;

			weak_write_barrier ();

			if (!CAS_PTR (bmap->pool[pn], NULL, block)) {
				release_pool_block (al, block);
			}
		}
	}
}
/*}}}*/
/*{{{ */
static void release_from_pool (allocator_t *al, word pn)
{
	allocation_t	*head 	= NULL;
	allocation_t 	*p 	= al->pool[pn];
	word 		aln	= al->number;

	do {
		allocation_t *n = p->next;
		block_desc_t *b	= &(bmap->desc[block_number (p)]);

		if (b->scratch[aln] != NULL) {
			allocation_t *s = b->scratch[aln];
			word size 	= s->size;
			p->next 	= s->next;
			p->size 	= size;
			s->next 	= p;
			s->size		= size + 1;
		} else {
			b->scratch[aln] = p;
			p->next 	= NULL;
			p->gnext 	= head;
			p->size 	= 1;
			head 		= p;
		}

		p = n;
	} while (p != NULL);

	p = head;
	do {
		allocation_t 	*n 	= p->gnext;
		block_desc_t 	*b 	= &(bmap->desc[block_number (p)]);
		word 		size 	= p->size;
		void 		*ptr;

		b->scratch[aln]	= NULL;
		compiler_barrier ();

		do {
			ptr		= READ_RAW_PTR (b->free);
			p->gnext	= (allocation_t *) PTR_VAL (ptr);
		} while (!CAS_PTR (b->free, ptr, MAKE_PTR (p, al->number)));

		if (atw_val (&(b->type)) != BT_SMASHED) {
			if (atw_sub_z (&(b->in_use), size)) {
				strong_read_barrier ();
				if (atw_val (&(b->type)) == BT_DIRTY) {
					release_pool_block (al, b);
				}
			}
		} else {
			atw_sub (&(b->in_use), size * pool_to_size (pn));
		}

		p = n;
	} while (p != NULL);
}
/*}}}*/
/*{{{  void *dmem_new_allocator (void)*/
void *dmem_new_allocator (void)
{
#ifndef USE_MALLOC_INSTEAD
	while ((~bmap->allocators) != 0) {
		word n = bsf (~bmap->allocators);
		if (!(bmap->allocators & (1 << n))) {
			bmap->allocators |= (1 << n);
			allocator_t 	*al 	= &(bmap->allocator[n]);
			block_desc_t 	*block 	= allocate_pool_block (al);

			smash_block (al, block);
			al->smash_list = block;
			
			TRACE ("%p init", al);
			return (void *) al;
		}
	}
	
	fprintf (stderr, "dmem: allocator limited reached.\n");
#endif

	return NULL;
}
/*}}}*/
/*{{{  void *dmem_thread_alloc (void *allocator, word size)*/
void *dmem_thread_alloc (void *allocator, word size)
{
#ifndef USE_MALLOC_INSTEAD
	allocator_t *al = (allocator_t *) allocator;

	if (size == 0) {
		/* hot-path */
		return &(bmap->zero_block);
	} else if (size <= (BLOCK_SIZE >> 1)) {
		allocation_t *mem;
		word pn = size_to_pool (size);

		if (al->pool[pn] == NULL) {
			if (pn <= SMASH_MAX_POOL) {
				scavenge_smash_list (al);
			}
			if (al->pool[pn] == NULL) {
				al->pool[pn] = allocate_to_pool (al, pn);
				if (al->pool[pn] == NULL) {
					fprintf (stderr, "dmem: failed to allocate %d bytes (pool = %d)", size, pn);
					return NULL;
				}
			}
		}

		mem 		= al->pool[pn];
		al->pool[pn] 	= mem->next;

		TRACE ("%p + %p, %d (%p) %d", al, mem, pn, al->pool[pn], size);

		memset (mem, 0, pool_to_size (pn));

		return (void *) mem;
	} else {
		block_desc_t *ptr;
		word blocks = (size >> BLOCK_SHIFT) + one_if_nz (size, BLOCK_SIZE - 1);
		void *mem;

		ptr = allocate_contiguous_blocks (blocks);
		if (ptr == NULL) {
			fprintf (stderr, "dmem: failed to allocate %d bytes (large allocation).\n", size);
			return NULL;
		}

		mem = block_address (ptr->number);
		do {
			ptr->pool = size;
			ptr = ptr->next;
		} while (ptr != NULL);

		TRACE ("%p L+ %p, %d %d", al, mem, blocks, size);

		memset (mem, 0, blocks << BLOCK_SHIFT);
		
		return mem;
	}
#else
	//return memalign (64, size);
	return malloc (size);
#endif
}
/*}}}*/
/*{{{  void dmem_thread_alloc2 (void *allocator, void **pptr)*/
void dmem_thread_alloc2 (void *allocator, void **pptr)
{
	word size = *((word *) pptr);
	*pptr = dmem_thread_alloc (allocator, size);
}
/*}}}*/
/*{{{  void dmem_thread_release (void *allocator, void *ptr)*/
void dmem_thread_release (void *allocator, void *ptr)
{
#ifndef USE_MALLOC_INSTEAD
	allocator_t *al = (allocator_t *) allocator;
	allocation_t *mem = (allocation_t *) ptr;
	word bn = block_number (mem);
	word pn = bmap->desc[bn].pool;

	if (pn < N_POOLS) {
		/* pool allocation */
		if (bmap->desc[bn].type == BT_SMASHED) {
			pn = smashed_address_to_pool (ptr);
		}

		TRACE ("%p - %p, %d (%p)", al, mem, pn, al->pool[pn]);

		if (al->pool[pn] != NULL) {
			word group = pool_group_size (pn);

			if (al->pool[pn]->size < group) {
				/* pool within bounds; release to pool */
				mem->next = al->pool[pn];
				mem->size = al->pool[pn]->size + 1;
				al->pool[pn] = mem;
				return;
			}
			
			/* pool overflow; release pool back to blocks */
			release_from_pool (al, pn);
		}

		/* empty pool */
		mem->next = NULL;
		mem->size = 1;
		al->pool[pn] = mem;
	} else {
		/* large (multi-block) allocation */
		block_desc_t *b = &(bmap->desc[bn]);
		TRACE ("%p L- %p", al, ptr);
		release_continguous_blocks (b);
	}
#else
	free (ptr);
#endif
}
/*}}}*/
/*{{{  void *dmem_thread_realloc (void *allocator, void *ptr, word size)*/
void *dmem_thread_realloc (void *allocator, void *ptr, word size)
{
	word bn = block_number (ptr);
	word pn = bmap->desc[bn].pool;
	word old_size;
	void *new;

	if (pn < N_POOLS) {
		if (bmap->desc[bn].type == BT_SMASHED) {
			pn = smashed_address_to_pool (ptr);
		}

		if (pool_to_size (pn) == size_to_pool (size)) {
			return ptr;
		} else {
			old_size = pool_to_size (pn);
		}
	} else {
		old_size = pn;
	}

	new = dmem_thread_alloc (allocator, size); /* FIXME: allocate without memset */
	memcpy (new, ptr, old_size);
	dmem_thread_release (allocator, ptr);
	
	return new;
}
/*}}}*/
/*{{{  void *dmem_alloc (word size)*/
void *dmem_alloc (word size)
{
	sched_t *sched = local_scheduler ();
	if (sched != NULL) {
		return dmem_thread_alloc (sched->allocator, size);
	} else {
		return malloc (size);
	}
}
/*}}}*/
/*{{{  void dmem_alloc2 (void **pptr)*/
void dmem_alloc2 (void **pptr)
{
	sched_t *sched = local_scheduler ();
	if (sched != NULL) {
		dmem_thread_alloc2 (sched->allocator, pptr);
	} else {
		return;
	}
}
/*}}}*/
/*{{{  void dmem_release (void *ptr)*/
void dmem_release (void *ptr)
{
	sched_t *sched = local_scheduler ();
	if (sched != NULL) {
		dmem_thread_release (sched->allocator, ptr);
	} else {
		free (ptr);
	}
}
/*}}}*/
/*}}}*/

#if defined(DM_DEBUG) && (DM_DEBUG==1) && !defined(OOS_BUILD)

#if defined(DM_DEBUG_LIBELF) && (DM_DEBUG_LIBELF==1)
	#define USING_LIBELF

	#include <sys/types.h>
	#include <unistd.h>
	#include <fcntl.h>
	#include <gelf.h>
	#include <libelf.h>
#endif	/* defined(DM_DEBUG_LIBELF) && (DM_DEBUG_LIBELF==1) */


/*{{{  memory debugging routines*/
/*{{{  constants/structures for memory debugger*/
#define DM_MAGIC ((void *)0xffffffff)

typedef struct TAG_ord_mem_block {
	void *ptr;
	int size;
	struct TAG_ord_mem_block *next;
	void *spare;
	int age;
	int mode;
	int mdparam[6];
} ord_mem_block;
/*}}}*/
/*{{{  static vars*/
static ord_mem_block *memout = NULL;
/*}}}*/
/*{{{  static void add_ord_mem (void *ptr, int size)*/
/*
 *	adds a block to the allocator.  this is called locally and
 *	gets READ/WRITE/BALLOC flags
 */
static void add_ord_mem (void *ptr, int size)
{
	ord_mem_block *tmp = (ord_mem_block *)locked_malloc (sizeof (ord_mem_block));
	ord_mem_block *walk, *prev;
	sched_t *sched = local_scheduler ();
	int i;

	if (!tmp) {
		MESSAGE0 ("add_ord_mem: malloc() failed!\n");
		ccsp_kernel_exit (1, 0);
	}
	tmp->ptr = ptr;
	tmp->size = size;
	tmp->next = NULL;
	tmp->spare = DM_MAGIC;
	tmp->age = 0;
	tmp->mode = MODE_READ | MODE_WRITE | MODE_BALLOC;
	for (i = 0; i < 6; ++i) {
		tmp->mdparam[i] = sched->mdparam[i];
	}
	/* insert into ordered list */
	for (walk=memout, prev=NULL; walk && ((unsigned int)ptr > (unsigned int)walk->ptr); prev=walk, walk=walk->next);
	if (!prev) {
		tmp->next = memout;
		memout = tmp;
	} else {
		tmp->next = prev->next;
		prev->next = tmp;
	}
	if (!size) {
		MESSAGE0 ("add_ord_mem: warning: 0 sized allocation.\n");
	}
	return;
}
/*}}}*/
/*{{{  void extadd_ord_mem (void *ptr, int size, int mode)*/
/*
 *	this is used to add memory blocks that are allocated
 *	through other means -- code sections, malloc()'d bits, etc.
 */
void extadd_ord_mem (void *ptr, int size, int mode)
{
	ord_mem_block *tmp = (ord_mem_block *)locked_malloc (sizeof (ord_mem_block));
	ord_mem_block *walk, *prev;
	sched_t *sched = local_scheduler ();
	int i;

	if (!tmp) {
		MESSAGE0 ("add_ord_mem: malloc() failed!\n");
		ccsp_kernel_exit (1, 0);
	}
	tmp->ptr = ptr;
	tmp->size = size;
	tmp->next = NULL;
	tmp->spare = DM_MAGIC;
	tmp->age = 0;
	tmp->mode = mode;
	for (i = 0; i < 6; ++i) {	
		tmp->mdparam[i] = mdparam[i];
	}
	/* insert into ordered list */
	for (walk=memout, prev=NULL; walk && ((unsigned int)ptr > (unsigned int)walk->ptr); prev=walk, walk=walk->next);
	if (!prev) {
		tmp->next = memout;
		memout = tmp;
	} else {
		tmp->next = prev->next;
		prev->next = tmp;
	}
	if (!size) {
		MESSAGE0 ("extadd_ord_mem: warning: 0 sized allocation.\n");
	}
	return;
}
/*}}}*/
/*{{{  void extdel_ord_mem (void *ptr)*/
/*
 *	called to remove externally allocated memory from the memory debugger
 */
void extdel_ord_mem (void *ptr)
{
	ord_mem_block *walk, *last, *tmp;

	tmp = NULL;
	for (walk=memout, last=NULL; walk && ((unsigned int)ptr > (unsigned int)walk->ptr); last=walk, walk=walk->next);
	if (!walk) {
		MESSAGE ("extdel_ord_mem: attempt to release %p, ", ptr);
		if (last && ((unsigned int)ptr < ((unsigned int)last->ptr + last->size))) {
			MESSAGE ("%d bytes into block at %p (%d bytes)\n", (unsigned int)ptr - (unsigned int)last->ptr, last->ptr, last->size);
		} else if (last) {
			MESSAGE ("%d bytes past end of block %p (%d bytes)\n", (unsigned int)ptr - ((unsigned int)last->ptr + last->size), last->ptr, last->size);
		} else {
			MESSAGE0 ("no allocations ?\n");
		}
		ccsp_show_last_debug_insert ();
		ccsp_kernel_exit (1, 0);
	} else if ((unsigned int)ptr < (unsigned int)walk->ptr) {
		MESSAGE ("extdel_ord_mem: attempt to release %p, %d bytes before block at %p (%d bytes)\n", ptr, (unsigned int)walk->ptr - (unsigned int)ptr, walk->ptr, walk->size);
		ccsp_show_last_debug_insert ();
		ccsp_kernel_exit (1, 0);
	} else if ((unsigned int)ptr > (unsigned int)walk->ptr) {
		MESSAGE ("extdel_ord_mem: attempt to release %p, ", ptr);
		if ((unsigned int)ptr >= ((unsigned int)walk->ptr + walk->size)) {
			MESSAGE ("%d bytes past end of block at %p (%d bytes)\n", (unsigned int)ptr - ((unsigned int)walk->ptr + walk->size), walk->ptr, walk->size);
		} else {
			MESSAGE ("%d bytes into block at %p (%d bytes)\n", (unsigned int)ptr - (unsigned int)walk->ptr, walk->ptr, walk->size);
		}
	}	/* else good! */
	if ((walk->mode & MODE_BALLOC)) {
		MESSAGE ("extdel_ord_mem: block at %p was allocated by internal allocator (%d bytes)\n", ptr, walk->size);
	}
	if (walk == memout) {
		tmp = walk;
		memout = walk->next;
	} else {
		tmp = walk;
		last->next = walk->next;
	}
	if (tmp) {
		locked_free (tmp);
	}
}
/*}}}*/
#if defined(DM_DEBUG_LIBELF) && (DM_DEBUG_LIBELF==1)
/*{{{  void extadd_elf_mem (char *argvz)*/
/*
 *	scans the named file for data and code sections,
 *	then adds those to the memory debugger.
 *	.text and .rodata are marked as read-only
 *	.data is marked read-write
 */
void extadd_elf_mem (char *argvz)
{
	int fd;
	Elf *elf;

	/* oki, ELF here we come..! (trying to extract address/size of the constant data area) */
	elf_version (EV_CURRENT);
	fd = open (argvz, O_RDONLY);
	if (fd < 0) {
		BMESSAGE ("unable to open executable (%s) for dynamic memory debugger\n", argvz);
		return;
	}
	elf = elf_begin (fd, ELF_C_READ_MMAP, NULL);
	if (elf) {
		Elf_Scn *scn;
		GElf_Shdr shdr;
		size_t shstrndx;

		if (elf_kind (elf) != ELF_K_ELF) {
			BMESSAGE ("dynamic memory debugger cannot handle archive type files yet! (%s)\n", argvz);
			elf_end (elf);
			close (fd);
			return;
		}
		/* get section header string table */
		if (elf_getshstrndx (elf, &shstrndx) < 0) {
			BMESSAGE ("cannot get ELF section header string table from %s\n", argvz);
			elf_end (elf);
			close (fd);
			return;
		}
		scn = NULL;
		while ((scn = elf_nextscn (elf, scn)) != NULL) {
			GElf_Shdr *pshdr = gelf_getshdr (scn, &shdr);

			if ((pshdr->sh_flags & SHF_ALLOC)) {
				const char *secstr = elf_strptr (elf, shstrndx, pshdr->sh_name);
				void *secaddr = ((void *)((unsigned int)pshdr->sh_addr));
				int seclen = (int)pshdr->sh_size;
				int addmode = 0;

				if (!strcmp (secstr, ".rodata") || !strcmp (secstr, ".text")) {
					/* rodata and code are read-only */
					addmode = MODE_READ;
				} else if (!strcmp (secstr, ".data")) {
					/* allow data to be read/write */
					addmode = MODE_READ | MODE_WRITE;
				}
				if (addmode) {
#if 0
fprintf (stderr, "extadd_elf_mem: adding section [%s] at %p for %d bytes\n", secstr, (void *)((unsigned int)pshdr->sh_addr), (int)pshdr->sh_size);
#endif
					extadd_ord_mem (secaddr, seclen, addmode);
				}
			}
		}
		elf_end (elf);
	}
	close (fd);
	return;
}
/*}}}*/
/*{{{  void extdel_elf_mem (char *argvz)*/
/*
 *	this is used to delete memory blocks that came from ELF files,
 *	see extadd_elf_mem() above
 */
void extdel_elf_mem (char *argvz)
{
	int fd;
	Elf *elf;

	/* oki, ELF here we come..! (trying to extract address/size of the constant data area) */
	elf_version (EV_CURRENT);
	fd = open (argvz, O_RDONLY);
	if (fd < 0) {
		BMESSAGE ("unable to open executable (%s) for dynamic memory debugger\n", argvz);
		return;
	}
	elf = elf_begin (fd, ELF_C_READ_MMAP, NULL);
	if (elf) {
		Elf_Scn *scn;
		GElf_Shdr shdr;
		size_t shstrndx;

		if (elf_kind (elf) != ELF_K_ELF) {
			BMESSAGE ("dynamic memory debugger cannot handle archive type files yet! (%s)\n", argvz);
			elf_end (elf);
			close (fd);
			return;
		}
		/* get section header string table */
		if (elf_getshstrndx (elf, &shstrndx) < 0) {
			BMESSAGE ("cannot get ELF section header string table from %s\n", argvz);
			elf_end (elf);
			close (fd);
			return;
		}
		scn = NULL;
		while ((scn = elf_nextscn (elf, scn)) != NULL) {
			GElf_Shdr *pshdr = gelf_getshdr (scn, &shdr);

			if ((pshdr->sh_flags & SHF_ALLOC)) {
				const char *secstr = elf_strptr (elf, shstrndx, pshdr->sh_name);
				void *secaddr = ((void *)((unsigned int)pshdr->sh_addr));

				if (!strcmp (secstr, ".rodata") || !strcmp (secstr, ".text") || !strcmp (secstr, ".data")) {
					extdel_ord_mem (secaddr);
				}
			}
		}
		elf_end (elf);
	}
	close (fd);
	return;

}
/*}}}*/
#else
/*{{{  dummies for extadd_elf_mem/extdel_elf_mem*/
void extadd_elf_mem (char *argvz)
{
	return;
}
void extdel_elf_mem (char *argvz)
{
	return;
}
/*}}}*/
#endif
/*{{{  static int del_ord_mem (void *ptr)*/
/*
 *	removes a block from the memory debugger.  this is called locally.
 *	if this returns non-zero, the pointer passed should be freed proper (onto a free-list, etc.)
 *	if zero, the block should stay allocated -- the memory debugger will deal with its eventual release
 */
static int del_ord_mem (void *ptr)
{
	ord_mem_block *walk, *last, *tmp;

	tmp = NULL;
	for (walk=memout, last=NULL; walk && ((unsigned int)ptr > (unsigned int)walk->ptr); last=walk, walk=walk->next);
	if (!walk) {
		MESSAGE ("del_ord_mem: attempt to release %p, ", ptr);
		if (last && ((unsigned int)ptr < ((unsigned int)last->ptr + last->size))) {
			MESSAGE ("%d bytes into block at %p (%d bytes)\n", (unsigned int)ptr - (unsigned int)last->ptr, last->ptr, last->size);
		} else if (last) {
			MESSAGE ("%d bytes past end of block %p (%d bytes)\n", (unsigned int)ptr - ((unsigned int)last->ptr + last->size), last->ptr, last->size);
		} else {
			MESSAGE0 ("no allocations ?\n");
		}
		ccsp_show_last_debug_insert ();
		ccsp_kernel_exit (1, 0);
	} else if ((unsigned int)ptr < (unsigned int)walk->ptr) {
		MESSAGE ("del_ord_mem: attempt to release %p, %d bytes before block at %p (%d bytes)\n", ptr, (unsigned int)walk->ptr - (unsigned int)ptr, walk->ptr, walk->size);
		ccsp_show_last_debug_insert ();
		ccsp_kernel_exit (1, 0);
	} else if ((unsigned int)ptr > (unsigned int)walk->ptr) {
		MESSAGE ("del_ord_mem: attempt to release %p, ", ptr);
		if ((unsigned int)ptr >= ((unsigned int)walk->ptr + walk->size)) {
			MESSAGE ("%d bytes past end of block at %p (%d bytes)\n", (unsigned int)ptr - ((unsigned int)walk->ptr + walk->size), walk->ptr, walk->size);
		} else {
			MESSAGE ("%d bytes into block at %p (%d bytes)\n", (unsigned int)ptr - (unsigned int)walk->ptr, walk->ptr, walk->size);
		}
	}	/* else good! */
	if (*((unsigned int *)ptr + 1) != 0xffffffff) {
		word smds[6];
		int i;

		/* magic has been trashed.. :( */
		MESSAGE ("del_ord_mem: block at %p has trashed magic (0x%8.8x, idx=0x%8.8x), %d bytes\n", ptr, *(unsigned int *)((char *)ptr + sizeof (int)), *(unsigned int *)(ptr), walk->size);
		/* find out where it came from.. :) */
		for (i = 0; i < 6; ++i) {
			smds[i] = sched->mdparam[i];
			sched->mdparam[i] = walk->mdparam[i];
		}
		ccsp_show_last_debug_insert ();
		for (i = 0; i < 6; ++i) {
			sched->mdparam[i] = smds[i];
		}
	}
	if ((walk->mode & MODE_BALLOC) == 0) {
		MESSAGE ("del_ord_mem: attempt to release %p, but externally allocated.\n", ptr);
		ccsp_show_last_debug_insert ();
		ccsp_kernel_exit (1, 0);
	} else if ((walk->mode & MODE_TRASHED)) {
		MESSAGE ("del_ord_mem: attempt to release %p, but links were trashed.\n", ptr);
		ccsp_show_last_debug_insert ();
		ccsp_kernel_exit (1, 0);
	}
	if (walk == memout) {
		tmp = walk;
		memout = walk->next;
	} else {
		tmp = walk;
		last->next = walk->next;
	}
	if (tmp) {
		locked_free (tmp);
	}
	return 1;
}
/*}}}*/
/*{{{  void verify_ord_mem (void *ptr, int dmcount)*/
/*
 *	this is called at run-time to verify a particular address for read/write
 */
void verify_ord_mem (void *ptr, int dmcount)
{
	ord_mem_block *walk, *prev;
	int mode = dmcount & 0x0f;

	/* dmcount is shifted.. */
	dmcount >>= 4;
#if 0
fprintf (stderr, "verify_ord_mem (%p, %d) in.\n", ptr, dmcount);
#endif
	for (walk=memout, prev=NULL; walk && ((unsigned int)ptr >= (unsigned int)(walk->ptr)); prev=walk, walk=walk->next);
	walk = prev;
	if (!walk && !prev) {
		/* strange.. */
		MESSAGE ("verify_ord_mem [%d]: %p invalid (no blocks?)\n", dmcount, ptr);
		ccsp_show_last_debug_insert ();
	} else if (!walk) {
		/* beyond last allocated block */
		MESSAGE ("verify_ord_mem [%d]: %p is %d bytes beyond last known block\n", dmcount, ptr, (unsigned int)ptr - (((unsigned int)prev->ptr) + prev->size));
		ccsp_show_last_debug_insert ();
	} else if (((unsigned int)ptr >= (unsigned int)walk->ptr)) {
		/* in this block */
		if ((unsigned int)ptr >= (((unsigned int)walk->ptr) + walk->size)) {
			/* trampling off the block-end.. */
			MESSAGE ("verify_ord_mem [%d]: %p is %d bytes past the end of %p (%d bytes)", dmcount, ptr,
					(((unsigned int)ptr) - ((unsigned int)(walk->ptr) + walk->size)), walk->ptr, walk->size);
			if (walk->next) {
				MESSAGE (", next block at %p (%d bytes)\n", walk->next->ptr, walk->next->size);
			} else {
				MESSAGE0 ("\n");
			}
			ccsp_show_last_debug_insert ();
		}
		if ((mode & ADDR_TARGET)) {
			/* check target */
			if ((walk->mode & MODE_BALLOC) && ((unsigned int)ptr <= (((unsigned int)walk->ptr) + sizeof(int)))) {
				/* trampling on memory-allocator bits.. */
				MESSAGE ("verify_ord_mem [%d] (write): %p is in the first 2 words of block at %p (%d bytes)\n", dmcount, ptr, walk->ptr, walk->size);
				walk->mode |= MODE_TRASHED;
				ccsp_show_last_debug_insert ();
			}
			if (!(walk->mode & MODE_WRITE)) {
				MESSAGE ("verify_ord_mem [%d] (write): %p is in a read-only block at %p (%d bytes)\n", dmcount, ptr, walk->ptr, walk->size);
				ccsp_show_last_debug_insert ();
			}
		} else if ((mode & ADDR_SOURCE)) {
			/* check source */
			if (!(walk->mode & MODE_READ)) {
				MESSAGE ("verify_ord_mem [%d] (read): %p is in a non-readable block at %p (%d bytes)\n", dmcount, ptr, walk->ptr, walk->size);
				ccsp_show_last_debug_insert ();
			}
		}
	}
#if 0
fprintf (stderr, "verify_ord_mem (%p, %d) out.\n", ptr, dmcount);
#endif
	return;
}
/*}}}*/
/*{{{  void dm_debug_dump (void)*/
/*
 *	dumps the `live' memory map (on stderr)
 */
void dm_debug_dump (void)
{
	ord_mem_block *walk;

	for (walk = memout; walk; walk = walk->next) {
		MESSAGE ("%c%c%c%c 0x%8.8x 0x%-6x %d\n",
				(walk->mode & MODE_READ) ? 'r' : '-',
				(walk->mode & MODE_WRITE) ? 'w' : '-',
				(walk->mode & MODE_BALLOC) ? 'b' : '-',
				(walk->mode & MODE_TRASHED) ? 't' : '-',
				(unsigned int)(walk->ptr), (unsigned int)(walk->size),
				walk->age);
	}
	return;
}
/*}}}*/


/*}}}*/
#endif	/* defined(DM_DEBUG) && !defined(OOS_BUILD)*/


/*{{{  void dmem_init (void)*/
/*
 *	initialises the allocator
 */
void dmem_init (void)
{
#if defined(DMEM_TRACE)
	trace = fopen ("dmem.trace", "w");
#endif
#if !defined(USE_MALLOC_INSTEAD)
	init_block_map ();
#endif
}
/*}}}*/
/*{{{  void dmem_shutdown (void)*/
/*
 *	shuts down the pool allocator
 */
void dmem_shutdown (void)
{
}
/*}}}*/

/*{{{  static const char *get_insert_pos ()*/
/*
 *	get the current insert debugging position
 */
static const char *get_insert_pos ()
{
	static char s[80];
	sched_t *sched = local_scheduler ();
	void (*setup_fcn)(void);
	int file_num, line_num;
	char *insert_file;

	if ((sched->mdparam[0] != 0xffffffff) && (sched->mdparam[1] != 0xffffffff) && sched->mdparam[0] && sched->mdparam[1]) {
		word orig0 = sched->mdparam[0], orig1 = sched->mdparam[1];

		file_num = (sched->mdparam[0] >> 16) & 0xffff;
		line_num = sched->mdparam[0] & 0xffff;
		/* mdparam[1] holds address of setup code */
		setup_fcn = (void (*)(void))sched->mdparam[1];
		setup_fcn ();
		insert_file = (char *)sched->mdparam[0];
		if ((file_num >= *(int *)(insert_file)) || (file_num < 0)) {
			snprintf (s, sizeof s, "invalid_insert_file_%d:%d", file_num, line_num);
		} else {
			insert_file += *(int *)(insert_file + (4 * (file_num+1)));
			snprintf (s, sizeof s, "%s:%d", insert_file, line_num);
		}

		sched->mdparam[0] = orig0;
		sched->mdparam[1] = orig1;
	} else {
		snprintf (s, sizeof s, "no_insert_info:0");
	}
	return s;
}
/*}}}*/

