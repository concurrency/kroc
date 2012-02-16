/*  $Id: alloc1.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $    */

/*
 *	Allocation routines
 *	Copyright (C) 1994 Inmos Limited
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


/* Ade 31/1/94 : initialise return result in realloc_debug */
/* Ade 24/6/94 : added CVS */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "toolkit.h"

/*{{{   struct s_pointer_list   */
struct s_pointer_list
{
  void *addr;
  int freed;
  struct s_pointer_list *next;
};
/*}}}*/
PRIVATE struct s_pointer_list *ptr_list = NULL;

/*{{{   PRIVATE static void mk_pointer (ptr)   */
PRIVATE void mk_pointer (ptr)
void *ptr;
{
  struct s_pointer_list *entry;
  if ((entry = (void *) malloc (sizeof (struct s_pointer_list))) == NULL)
  {
    fprintf (stderr, "out of memory\n");
    exit (EXIT_FAILURE);
  }
  entry->addr = ptr;
  entry->freed = FALSE;
  entry->next = ptr_list;
  ptr_list = entry;
}
/*}}}*/
/*{{{   PUBLIC void check_ptr (ptr)   */
PUBLIC void check_ptr (ptr)
void *ptr;
{
  struct s_pointer_list *entry;
  int ok;
  entry = ptr_list;
  ok = FALSE;
  while ((entry != NULL) && (!ok))
  {
    if (ptr == (void *) entry->addr)
    {
      if (entry->freed) fprintf (stderr, "Pointer check failed: Pointer has been freed\n");
      ok = TRUE;
    }
    entry = entry->next;
  }
  if (ok) printf ("ptr ok ");
  if (!ok)
  {
    fprintf (stderr, "Pointer check failed: imaginary pointer\n");
    exit (EXIT_FAILURE);
  }
}
/*}}}*/
/*{{{   PUBLIC void *malloc_debug (mem)   */
PUBLIC void *malloc_debug (mem)
size_t mem;
{
  void *res;
  res = (void *) malloc (mem);
  if (res == NULL)
  {
    fprintf (stderr, "out of memory\n");
    exit (EXIT_FAILURE);
  }
  mk_pointer (res);
  return (res);
}
/*}}}*/
/*{{{   PUBLIC void *calloc_debug (n, size)   */
PUBLIC void *calloc_debug (n, size)
size_t n, size;
{
  void *res;
  res = (void *) calloc (n, size);
  if (res == NULL)
  {
    fprintf (stderr, "out of memory\n");
    exit (EXIT_FAILURE);
  }
  mk_pointer (res);
  return (res);
}
/*}}}*/
/*{{{   PUBLIC void *realloc_debug (ptr, mem)   */
PUBLIC void *realloc_debug (ptr, mem)
void *ptr;
size_t mem;
{
  void *res = NULL;
  struct s_pointer_list *entry;
  int ok;
  entry = ptr_list;
  ok = FALSE;
  if (ptr == NULL) 
  {
    printf ("Note: reallocing null pointer\n");
    return (malloc_debug (mem));
  }
  while ((entry != NULL) && (!ok))
  {
    if (ptr == (void *) entry->addr)
    {
      if (entry->freed)
      {
        fprintf (stderr, "realloc freed memory\n");
        exit (EXIT_FAILURE);
      }
      res = (void *) realloc (ptr, mem);
      if (res == NULL)
      {
        fprintf (stderr, "out of memory\n");
        exit (EXIT_FAILURE);
      }
      if (entry->addr != res)
      {
        entry->freed = TRUE;
        mk_pointer (res);
      }
      ok = TRUE;
    }
    entry = entry->next;
  }
  if (!ok)
  {
    fprintf (stderr, "reallocing imaginary pointer\n");
    exit (EXIT_FAILURE);
  }
  return (res);
}
/*}}}*/
/*{{{   PUBLIC void free_debug (ptr)   */
PUBLIC void free_debug (ptr)
void *ptr;
{
  struct s_pointer_list *entry;
  int ok;
  if (ptr == NULL)
  {
    free (ptr);
    printf ("Note: freeing null pointer\n");
    return;
  }
  entry = ptr_list;
  ok = FALSE;
  while ((entry != NULL) && (!ok))
  {
    if (ptr == (void *) entry->addr)
    {
      if (entry->freed)
      {
        fprintf (stderr, "Pointer being freed twice\n");
        exit (EXIT_FAILURE);
      }
      free (ptr);
      entry->freed = TRUE;
      ok = TRUE;
    }
    entry = entry->next;
  }
  if (!ok)
  {
    fprintf (stderr, "Freeing imaginary pointer\n");
    exit (EXIT_FAILURE);
  }
}
/*}}}*/
