/*
 *	LFF I/O functions
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
   PLAY:LFF_IO_C@510.AAAA-FILE;1(20-FEB-92)[FROZEN] */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#include "toolkit.h"

/*{{{   PUBLIC long int lff_getl_test (fs, ok)   */
PUBLIC long int lff_getl_test (fs, ok)
FILE *fs;
int *ok;
{
  long int val;
  register int c;
  *ok = TRUE;
  val = 0L;
  if ((c = fgetc (fs)) == EOF) *ok = FALSE;
  if ((c & 0X40) == 0) val = 0L;
  else val = -1;
  while (((c & 0X80) != 0) && *ok)
  {
    val = (val << 7L) | (((long int) c) & 0X7FL);
    if ((c = fgetc (fs)) == EOF) *ok = FALSE;
  }
  val = (val << 7) | c;
  return (val);
}

/*}}}*/
/*{{{   PUBLIC char *lff_gets_test (fs, l, ok)   */
PUBLIC char *lff_gets_test (fs, l, ok)
FILE *fs;
long int *l;
int *ok;
{
  char *str, *res;
  int n, c, s;
  *ok = TRUE;
  *l = lff_getl_test (fs, ok);
  s = (int) *l;
  res = str = malloc_chk (1 + (s * sizeof (char)));
  for (n = 0; n < s; n++)
  {
    if ((c = fgetc (fs)) == EOF) *ok = FALSE;
    else *str++ = (char) c;
  }
  *str = '\0';
  return (res);
}

/*}}}*/
