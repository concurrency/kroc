/*
 *	command line interface
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
   PLAY:COMMAND1_C@498.AAAA-FILE;1(20-FEB-92)[FROZEN] */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <string.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#include "toolkit.h"

/*{{{   PRIVATE int check (c)   */
PRIVATE int check (c)
const char c;
{
  switch (c)
  {
    case '/':
    case '\\':
    case ':':
    case ']':
        return(TRUE);
        break;
    default:
        return (FALSE);
        break;
  }
}
/*}}}*/
/*{{{   PUBLIC char *strip_path (string)   */
PUBLIC char *strip_path (string)
char const *string;
{
  int i, len;
  char *ptr, *start, *temp;

  /* copy string into temp so I can fiddle with it */
  len = 0;
  while (string[len++] != '\0');
  temp = malloc_chk (len * sizeof (char));
  for (i = 0; i < len; i++) temp[i] = string[i];

  /* find the start of the file name */
  ptr = &temp[len - 1];
  while ((ptr != temp) && !check (*ptr)) ptr--;
  if (check (*ptr)) start = ++ptr;
  else start = temp;
  start = str_duplicate (start);
  free_chk (temp);
  return (start);
}
/*}}}*/
/*{{{   PUBLIC char *derive_string (string, full, extension)   */
PUBLIC char *derive_string (string, full, extension)
char const *string, *extension;
int full;
{
  int i, len;
  char *ptr, *start, *temp;

  if (!full) temp = strip_path (string);
  else 
  {
    len = 0;
    while (string[len++] != '\0');
    temp = malloc_chk (len * sizeof (char));
    for (i = 0; i < len; i++) temp[i] = string[i];
  
  }

  start = temp; 


  /* knock off the extension - if any */
  len = strlen (start);
  ptr = &start[len - 1];
  while ((ptr != start) && (*ptr != '.') && !check(*ptr)) ptr--;
  if (*ptr == '.') *ptr = '\0';

  /* glue on new extension */
  ptr = str_concat (start, extension);
  free_chk (temp);
  return (ptr);
}
/*}}}*/

