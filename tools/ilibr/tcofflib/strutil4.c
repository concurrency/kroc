/*
 *	String utilities
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
   PLAY:STRUTIL4_C@518.AAAA-FILE;3(25-JAN-93)[FROZEN] */
/* ahp : 930126 : Removed reliance on redefinition of ANSI functions */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#include <ctype.h>
#include "toolkit.h"

/*{{{   PRIVATE int to_lower (k) */
PRIVATE int to_lower (k)
char k;
{
  return (isupper(k) ? tolower(k) : k);
}
/*}}}*/

/*{{{   PUBLIC int str_semicmp_lc (str1, str2)   */
PUBLIC int str_semicmp_lc (str1, str2)
char const *str1, *str2;
{
  char *s1, *s2;
  int matched;
  s1 = (char *) str1;
  s2 = (char *) str2;
  matched = TRUE;
  while (matched && (*s1 != '\0'))
    if (to_lower (*s1++) != to_lower (*s2++)) matched = FALSE;
  return (matched);
}
/*}}}*/

