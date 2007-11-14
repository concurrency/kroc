/*  $Id: strutil6.c,v 1.1 1996/04/15 10:54:19 djb1 Exp $    */

/*
 *	TCOFF string utilites
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

#include <stdio.h>
#ifdef STD_C
#include <stdlib.h>
#endif
#include <string.h>
#include "toolkit.h"

/*{{{   PUBLIC char *str_duplicate (str)   */
PUBLIC char *str_duplicate (str)
char const *str;
{
  int l;
  char *newstr;
  l = strlen (str);
  newstr = (malloc_chk (1 + l * sizeof (char)));
  (void) strcpy (newstr, str);
  return (newstr);
}
/*}}}*/
