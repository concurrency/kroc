/*  $Id: lff_io.c,v 1.3 1997/03/25 14:52:40 djb1 Exp $    */

/*
 *	LFF i/o
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


/*
 * Ade 24/6/94 - added CVS
 * Ade  8/9/94 - include tcofflib.h
 */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include "toolkit.h"
#include "tcofflib.h"

/*{{{   PUBLIC long int lff_getl_test (fs, ok)   */
PUBLIC long int lff_getl_test (fs, ok)
FILE *fs;
int *ok;
{
  long int val;
  register int c;
  *ok = TRUE;
  val = 0L;
  c = fgetc (fs);
  if (feof(fs)) *ok = FALSE;
  if ((c & 0X40) == 0) val = 0L;
  else val = -1;
  while (((c & 0X80) != 0) && *ok)
  {
    val = (val << 7L) | (((long int) c) & 0X7FL);
    c = fgetc (fs);
    if (feof(fs)) *ok = FALSE;
  }
  val = (val << 7) | c;
  return (val);
}

/*}}}  */
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
    c = fgetc (fs);
    if (feof(fs)) *ok = FALSE;
    else *str++ = (char) c;
  }
  *str = '\0';
  return (res);
}

/*}}}  */
