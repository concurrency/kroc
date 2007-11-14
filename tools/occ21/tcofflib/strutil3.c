/*  $Id: strutil3.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $    */

/*
 *	TCOFF string utilities
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

PUBLIC int str_semicmp (str1, str2)
char const *str1, *str2;
{
  char *s1, *s2;
  int res;
  s1 = (char *) str1;
  s2 = (char *) str2;
  res = TRUE;
  while (res && (*s1 != '\0'))
    if (*s1++ != *s2++) res = FALSE;
  return (res);
}
