/* $Id: strupr.c,v 1.1 1996/04/15 10:54:03 djb1 Exp $ */

/*
 *	C support library for toolset products
 *	Copyright (C) 1993 Inmos Limited
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


#include <ctype.h>
#include "suplib.h"

/*
 *  sup_strupr(s) replaces each lower-case character in the string s with
 *                the corresponding upper-case character.
 *                Non-lower-case characters are not altered.
 *                A pointer to s is returned.
 */

char *sup_strupr(char *s)
{
  char *t;
  for (t = s; *t != '\0'; t++)
    if (islower(*t))
      *t = toupper(*t);
  return s;
}


