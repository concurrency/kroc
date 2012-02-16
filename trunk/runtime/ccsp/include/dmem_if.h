/*
 *	dmem_if.h -- dynamic memory public interface
 *	Copyright (C) 2002-2005 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __DMEM_IF_H
#define __DMEM_IF_H

#include "ukcthreads_types.h"

/* DM_DEBUG moved to configure */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#if defined(DM_DEBUG) && (DM_DEBUG == 1)
#define ADDR_INVALID	0x00

#define ADDR_TARGET	0x01
#define ADDR_SOURCE	0x02

#define MODE_WRITE	0x01
#define MODE_READ	0x02
#define MODE_BALLOC	0x04
#define MODE_TRASHED	0x08

extern void extadd_ord_mem (void *, int, int);
extern void extdel_ord_mem (void *);
extern void extadd_elf_mem (char *);
extern void extdel_elf_mem (char *);
extern void dm_debug_dump (void);
#endif /* DM_DEBUG */

extern void dmem_init (void);
extern void dmem_shutdown (void);
extern void *dmem_new_allocator (void);

extern void *dmem_thread_alloc (void *, word);
extern void dmem_thread_alloc2 (void *, void **);
extern void dmem_thread_release (void *, void *);
extern void *dmem_thread_realloc (void *, void *, word);

extern void *dmem_alloc (word);
extern void dmem_alloc2 (void **);
extern void dmem_release (void *);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#define dmem_locked_malloc(X) dmem_alloc((X))
#define dmem_locked_free(X) dmem_release((X))
#define locked_malloc(X) dmem_locked_malloc(X)
#define locked_free(X) dmem_locked_free(X)

#endif	/* !__DMEM_IF_H */

