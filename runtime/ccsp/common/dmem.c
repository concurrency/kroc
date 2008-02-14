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

/*{{{  void *dmem_new_allocator (void)*/
void *dmem_new_allocator (void)
{
	return NULL;
}
/*}}}*/
/*{{{  void *dmem_thread_alloc (void *allocator, word size)*/
void *dmem_thread_alloc (void *allocator, word size)
{
	return malloc (size);
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
	free (ptr);
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

