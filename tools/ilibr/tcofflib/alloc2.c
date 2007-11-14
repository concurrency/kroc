/*
 *	Allocation routines
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

#ifndef DEBUG_POINTERS
/* Copyright 1990 INMOS Limited */
/* CMSIDENTIFIER
   PLAY:ALLOC2_C@496.AAAA-FILE;1(20-FEB-92)[FROZEN] */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#include "toolkit.h"

/*{{{   PUBLIC void *malloc_chk (mem)   */
PUBLIC void *malloc_chk (mem)
size_t mem;
{
  void *res;
  res = (void *) malloc (mem);
  if (res == NULL)
  {
    fprintf (stderr, "out of memory\n");
    exit (EXIT_FAILURE);
  }
  return (res);
}
/*}}}*/
/*{{{   PUBLIC void *calloc_chk (n, size)   */
PUBLIC void *calloc_chk (n, size)
size_t n, size;
{
  void *res;
  res = (void *) calloc (n, size);
  if (res == NULL)
  {
    fprintf (stderr, "out of memory\n");
    exit (EXIT_FAILURE);
  }
  return (res);
}
/*}}}*/
/*{{{   PUBLIC void *realloc_chk (ptr, mem)   */
PUBLIC void *realloc_chk (ptr, mem)
void *ptr;
size_t mem;
{
  void *res;
  res = (void *) realloc ((void *) ptr, mem);
  if (res == NULL)
  {
    fprintf (stderr, "out of memory\n");
    exit (EXIT_FAILURE);
  }
  return (res);
}
/*}}}*/
/*{{{  PUBLIC void free_chk (ptr) */
PUBLIC void free_chk (ptr)
void *ptr;
{
  if (ptr != NULL) free (ptr);
}
/*}}}*/
#endif
