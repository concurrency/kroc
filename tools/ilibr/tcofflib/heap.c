/*
 *	heap allocation functions
 *	Copyright (C) 1990 Inmos Limited
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

/* Copyright 1990 INMOS Limited */
/* CMSIDENTIFIER
   PLAY:HEAP_C@507.AAAA-FILE;1(20-FEB-92)[FROZEN] */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#ifdef __STDC__
#include <stdlib.h>
#include <stddef.h>
#endif
#include "toolkit.h"

/*{{{   PUBLIC struct s_heap *start_heap (obj_size, n_obj)   */
PUBLIC struct s_heap *start_heap (obj_size, n_obj)
size_t obj_size;
int n_obj;
{
  struct s_heap *heap;
  heap = malloc_chk (sizeof (struct s_heap));
  heap->obj_size = obj_size;
  heap->n_obj = n_obj;
  heap->next_block = 0;
  heap->next_free_mem = n_obj; /* to cause initial allocation */
  heap->cblock = NULL;
  heap->blocks = NULL;
  heap->free_chain = NULL;
  return (heap);
}
/*}}}*/
/*{{{   PUBLIC void *alt_malloc (heap)   */
PUBLIC void *alt_malloc (heap)
struct s_heap *heap;
{
  void *res;
  char *blk;
  struct s_free_chain *tmp;
  if (heap->free_chain != NULL)  /* use a freed item */
  {
    res = heap->free_chain->mem;
    tmp = heap->free_chain;
    heap->free_chain = heap->free_chain->next;
    /*printf ("using freed mem %p\n", res);*/
    free_chk (tmp);
  }
  else if (heap->next_free_mem != heap->n_obj) /* use a new item */
  {
    blk = (char *) heap->cblock;
    res = (void *) &blk[heap->obj_size * heap->next_free_mem++];
    /*printf ("using new mem %d %p\n", heap->next_free_mem - 1, res);*/
  }
  else            /* create new block and use first item */
  {
    heap->cblock = malloc_chk (heap->n_obj * heap->obj_size);
    heap->blocks = realloc_chk (heap->blocks, (heap->next_block + 1) * sizeof (void *));
    heap->blocks[heap->next_block++] = heap->cblock;
    heap->next_free_mem = 1;
    res = heap->cblock;      /* or &heap->cblock[0] */
    /*printf ("using new block, mem 0 %p\n", res);*/
  }
  return (res);
}
/*}}}*/
/*{{{   PUBLIC void *alt_free (heap, mem)   */
PUBLIC void alt_free (heap, mem)
struct s_heap *heap;
void *mem;
{
  struct s_free_chain *tmp;
  /*printf ("freeing $%p\n", mem);*/
  tmp = malloc_chk (sizeof (struct s_free_chain));
  tmp->mem = mem;
  tmp->next = heap->free_chain;
  heap->free_chain = tmp;
}
/*}}}*/
/*{{{   PUBLIC void print_heap_info (heap)   */
PUBLIC void print_heap_info (heap)
struct s_heap *heap;
{
  long int free, items, max_items, room, mem;
  struct s_free_chain *tmp;
  tmp = heap->free_chain;
  free = 0;
  while (tmp != NULL)
  {
    free++;
    tmp = tmp->next;
  }
  if (heap->next_block == 0) max_items = 0;
  else
    max_items = (long) heap->n_obj * (heap->next_block - 1L) + heap->next_free_mem;
  items = max_items - free;
  room = (long) heap->n_obj * heap->next_block;
  mem = room * heap->obj_size + free * sizeof (struct s_free_chain);
  printf ("\tspace in heap for %ld items\n", room);
  printf ("\tsize of item %d bytes\n", heap->obj_size);
  printf ("\tpeak allocation %ld items\n", max_items);
  printf ("\tcurrent allocation %ld items\n", items);
  printf ("\tno of free items %ld\n", free);
  printf ("\ttotal memory used: %ld bytes\n", mem);
}
/*}}}*/
