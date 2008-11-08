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

#if defined(ALLOC_BLOCK) || defined(ALLOC_SBLOCK)
/*{{{  block allocator constants/types*/

/* the available address-space is divided into two halves, the
 * first 2^SMALL_SLAB_SHIFT bytes, and the rest.
 */

#define ADDRSPACE_SHIFT 32					/* in a 32-bit address space */

#define SMALL_SLAB_LSHIFT 24					/* first 16 MiB */
#define SMALL_SLAB_LBYTES (1 << SMALL_SLAB_SHIFT)
#define SMALL_SLAB_SHIFT 20					/* 1 MiB small slabs */
#define SMALL_SLAB_BYTES (1 << SMALL_SLAB_SHIFT)
#define LARGE_SLAB_SHIFT 24					/* 16 MiB large slabs */
#define LARGE_SLAB_BYTES (1 << LARGE_SLAB_SHIFT)

#define NUM_SMALL_SLABS (1 << (SMALL_SLAB_LSHIFT - SMALL_SLAB_SHIFT))
#define NUM_LARGE_SLABS (1 << (ADDRSPACE_SHIFT - LARGE_SLAB_SHIFT))

#define SLAB_SPLIT_SHIFT 3					/* splits are into 8 */
#define SLAB_SPLIT_MASK ((1 << SLAB_SPLIT_SHIFT) - 1)

#define POOL_SMASH_SHIFT 18					/* pools come from 256k chunks */
#define POOL_SMASH_BYTES (1 << POOL_SMASH_SHIFT)

#define FIRST_POOL_SHIFT 4					/* 16 bytes */
#define FIRST_POOL_BYTES (1 << FIRST_POOL_SHIFT)

#define LAST_POOL_SHIFT 16					/* 64 KiB */
#define LAST_POOL_BYTES (1 << LAST_POOL_SHIFT)

#define NUM_POOLS (((LAST_POOL_SHIFT - FIRST_POOL_SHIFT) * 2) + 1)


#define SLABTYPE_INVALID 0
#define SLABTYPE_SPLIT 1
#define SLABTYPE_POOLS 2
#define SLABTYPE_RESERVED 3
#define SLABTYPE_MASK 0x03

static unsigned int slab_smallslabs[NUM_SMALL_SLABS];
static unsigned int slab_largeslabs[NUM_LARGE_SLABS];

typedef struct TAG_scraplist {
	struct TAG_scraplist *next;				/* next block or NULL */
	int left;						/* number of bytes left in this block */
} scraplist_t;

typedef struct TAG_allocator {
	unsigned int *pools[NUM_POOLS];				/* dynamic memory pools */
	scraplist_t *scrap_list;				/* scrap memory list */
	sched_t *sched;						/* scheduler this belongs to (NULL if shared) */
} allocator_t;

#if defined(ALLOC_SBLOCK)
static allocator_t shalloc = {
	pools: {NULL, },
	scrap_list: NULL,
	sched: NULL
};
#endif

/*}}}*/
/*{{{  block allocator routines*/

static inline int size_to_pool (const int bytes) /*{{{*/
{
	int rb, pool;

	if (bytes < FIRST_POOL_BYTES) {
		return 0;					/* smallest */
	} else if (bytes > LAST_POOL_BYTES) {
		return -1;					/* out of range */
	}
	rb = bsr (bytes - 1);					/* MSB: 31 -> 0 */
	pool = ((rb + 1) - FIRST_POOL_SHIFT) * 2;		/* at most this pool */
	if (!((bytes - 1) & (1 << (rb - 1)))) {
		pool--;						/* fits in the pool below */
	}
	return pool;	
}
/*}}}*/
static inline int pool_to_size (const int pool) /*{{{*/
{
	int bytes;

	bytes = 1 << ((pool >> 1) + FIRST_POOL_SHIFT);
	if (pool & 1) {
		bytes |= (bytes >> 1);
	}
	return bytes;
}
/*}}}*/
static void slab_map_init (void) /*{{{*/
{
	int i;

	for (i=0; i<NUM_SMALL_SLABS; i++) {
		slab_smallslabs[i] = SLABTYPE_INVALID;
	}
	for (i=0; i<NUM_LARGE_SLABS; i++) {
		slab_largeslabs[i] = SLABTYPE_INVALID;
	}
}
/*}}}*/
static inline unsigned int addr_to_slabid (const void *ptr) /*{{{*/
{
	if ((unsigned int)ptr < SMALL_SLAB_LBYTES) {
		/* small slab */
		return slab_smallslabs[(unsigned int)ptr >> SMALL_SLAB_SHIFT];
	}
	/* large slab */
	return slab_largeslabs[(unsigned int)ptr >> LARGE_SLAB_SHIFT];
}
/*}}}*/
static inline unsigned int *addr_to_slabidptr (const void *ptr) /*{{{*/
{
	if ((unsigned int)ptr < SMALL_SLAB_LBYTES) {
		/* small slab */
		return &(slab_smallslabs[(unsigned int)ptr >> SMALL_SLAB_SHIFT]);
	}
	/* large slab */
	return &(slab_largeslabs[(unsigned int)ptr >> LARGE_SLAB_SHIFT]);
}
/*}}}*/
static inline int addr_to_slabsizeshift (const void *ptr) /*{{{*/
{
	if ((unsigned int)ptr < SMALL_SLAB_LBYTES) {
		/* small slab */
		return SMALL_SLAB_SHIFT;
	}
	/* large slab */
	return LARGE_SLAB_SHIFT;
}
/*}}}*/


static inline int slab_markregion (const void *addr, const int bytes, unsigned int type) /*{{{*/
{
	unsigned int *sidp = addr_to_slabidptr (addr);
	int shft = addr_to_slabsizeshift (addr);

	while ((*sidp & SLABTYPE_MASK) == SLABTYPE_SPLIT) {
		/* split slab: divided into fixed number of chunks */
		unsigned int *sarry = (unsigned int *)(*sidp & ~SLABTYPE_MASK);
		int sidx;

		shft -= SLAB_SPLIT_SHIFT;
		sidx = ((unsigned int)addr >> shft) & SLAB_SPLIT_MASK;

		sidp = &(sarry[sidx]);
	}

	/* shft gives the slab-size of the interesting one, sidp is the starting slab */
}
/*}}}*/

static allocator_t *poolmem_newallocator (sched_t *sched) /*{{{*/
{
	allocator_t *a = (allocator_t *)malloc (sizeof (allocator_t));
	int i;

	a->sched = sched;
	a->scrap_list = NULL;
	for (i=0; i<NUM_POOLS; i++) {
		a->pools[i] = NULL;
	}

	return a;
}
/*}}}*/
static inline void *poolmem_alloc (allocator_t *alc, const int bytes) /*{{{*/
{
	int pool = size_to_pool (bytes);
	unsigned int *ptr, *next;

	if (pool < 0) {
		/* revert to underlying malloc() for large things */
		return malloc (bytes);
	}
	ptr = alc->pools[pool];

	if (!ptr) {
		if ((pool < (NUM_POOLS - 1)) && alc->pools[pool+1]) {
			/* use the next one up */
			pool++;
			ptr = alc->pools[pool];
		} else {
			BMESSAGE ("poolmem_alloc(): OOM! (%d) bytes, pool %d\n", bytes, pool);
			/* no memory here, go and find some! */
			/* FIXME: */
			return NULL;
		}
	}

	next = (unsigned int *)(ptr[0]);
	alc->pools[pool] = next;

	return ptr;
}
/*}}}*/
static inline void poolmem_release (allocator_t *alc, const void *ptr) /*{{{*/
{
	unsigned int sid = addr_to_slabid (ptr);
	int shft = addr_to_slabsizeshift (ptr);

	while ((sid & SLABTYPE_MASK) == SLABTYPE_SPLIT) {
		/* split slab: divided into fixed number of chunks */
		unsigned int *sarry = (unsigned int *)(sid & ~SLABTYPE_MASK);
		int sidx;

		shft -= SLAB_SPLIT_SHIFT;
		sidx = ((unsigned int)ptr >> shft) & SLAB_SPLIT_MASK;

		sid = sarry[sidx];
	}

	switch (sid & SLABTYPE_MASK) {
	case SLABTYPE_INVALID:
		BMESSAGE ("poolmem_release(): attempt to free %p from invalid slab\n", ptr);
		return;
	case SLABTYPE_RESERVED:
		/* revert to underlying free() */
		free ((void *)ptr);
		return;
	case SLABTYPE_POOLS:
		{
			/* pool memory: always chopped up in 256k chunks, or less if SPLIT slabs */
			int pool;

			if (shft <= POOL_SMASH_SHIFT) {
				/* slab was smaller than the default pool smash, will be broken up smaller */
				pool = (sid >> 8) & 0xff;
			} else {
				unsigned char *parry = (unsigned char *)(sid & ~SLABTYPE_MASK);
				int pidx = ((unsigned int)ptr >> POOL_SMASH_SHIFT) & ((1 << (shft - POOL_SMASH_SHIFT)) - 1);

				pool = (int)parry[pidx];
			}

			if (pool == 0xff) {
				BMESSAGE ("poolmem_release(): attempt to free %p from unallocated pool\n", ptr);
			} else if ((pool < 0) || (pool >= NUM_POOLS)) {
				BMESSAGE ("poolmem_release(); attempt to free %p from invalid pool %d\n", ptr, pool);
			} else {
				unsigned int *tblk = (unsigned int *)ptr;

				tblk[0] = (unsigned int)alc->pools[pool];
				alc->pools[pool] = tblk;
			}
		}
		return;
	}
}
/*}}}*/


/*}}}*/
#endif	/* defined(ALLOC_BLOCK) || defined(ALLOC_SBLOCK) */





/*{{{  void *dmem_new_allocator (void)*/
void *dmem_new_allocator (void)
{
#if defined(ALLOC_MALLOC)
	return NULL;
#elif defined(ALLOC_BLOCK)
	sched_t *sched = local_scheduler ();
	allocator_t *a = poolmem_newallocator (sched);

	BMESSAGE ("dmem_new_allocator(): allocated at %p for scheduler %p\n", a, sched);
	return (void *)a;
#elif defined(ALLOC_SBLOCK)
	return (void *)&shalloc;
#endif
}
/*}}}*/
/*{{{  void *dmem_thread_alloc (void *allocator, word size)*/
void *dmem_thread_alloc (void *allocator, word size)
{
#if defined(ALLOC_MALLOC)
	return malloc (size);
#elif defined(ALLOC_BLOCK)
	allocator_t *a = (allocator_t *)allocator;

BMESSAGE ("dmem_thread_alloc(): here (BLOCK)! (%d bytes, allocator at %p)\n", size, a);
	if (!a) {
		/* means we don't have an allocator setup yet */
		return malloc (size);
	} else {
		return poolmem_alloc (a, size);
	}
#elif defined(ALLOC_SBLOCK)
	allocator_t *a = (allocator_t *)allocator;

BMESSAGE ("dmem_thread_alloc(): here (SBLOCK)! (%d bytes, allocator at %p)\n", size, a);
	if (!a) {
		return poolmem_alloc (&shalloc, size);
	} else {
		return poolmem_alloc (a, size);
	}
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
#if defined(ALLOC_MALLOC)
	free (ptr);
#elif defined(ALLOC_BLOCK) || defined(ALLOC_SBLOCK)
	allocator_t *a = (allocator_t *)allocator;
#endif
}
/*}}}*/
/*{{{  void *dmem_thread_realloc (void *allocator, void *ptr, word size)*/
void *dmem_thread_realloc (void *allocator, void *ptr, word size)
{
	#if 0
	new = dmem_thread_alloc (allocator, size); /* FIXME: allocate without memset */
	memcpy (new, ptr, old_size);
	dmem_thread_release (allocator, ptr);
	return new;
	#endif
	return NULL;
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

#if defined(DM_DEBUG) && (DM_DEBUG==1) && !defined(RMOX_BUILD)

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
#endif	/* defined(DM_DEBUG) && !defined(RMOX_BUILD)*/


/*{{{  void dmem_init (void)*/
/*
 *	initialises the allocator
 */
void dmem_init (void)
{
#if defined(DMEM_TRACE)
	trace = fopen ("dmem.trace", "w");
#endif
#if defined(ALLOC_BLOCK) || defined(ALLOC_SBLOCK)
	slab_map_init ();
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

