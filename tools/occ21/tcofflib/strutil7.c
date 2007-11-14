/*  $Id: strutil7.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $    */

/*
 *	TCOFF string utilties
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


/* Ade 24/6/94 : added CVS */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#ifdef STD_C
#include <stdlib.h>
#endif
#include "toolkit.h"

/*{{{   PUBLIC char *str_concat (str1, str2)   */
PUBLIC char *str_concat (str1, str2)
char const *str1, *str2;
{
  char *res;
  int i, l1 = 0, l2 = 0;
  while (str1[l1] != '\0') l1++;
  while (str2[l2] != '\0') l2++;
  res = malloc_chk ((1 + l1 + l2) * sizeof (char));
  for (i = 0; i < l1; i++) res[i] = str1[i];
  for (i = 0; i < l2; i++) res[l1 + i] = str2[i];
  res[l1 + l2] = '\0';
  return (res);
}
/*}}}*/
