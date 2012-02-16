/*  $Id: command2.c,v 1.1 1996/04/15 10:54:16 djb1 Exp $    */

/*
 *	TCOFF stuff
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
#include <string.h>
#ifdef STD_C
#include <stdlib.h>
#endif
#include "toolkit.h"
/*{{{   PUBLIC void extract_bool_opts (ac, av, switchars, options, args)   */
PUBLIC void extract_bool_opts (ac, av, switchars, options, args)
int *ac;
char ***av;
char *switchars, **options;
int *args[];
{
  int i, j, k, l, m;
  char *str, **argv;
  argv = *av;
  for (k = 0; options[k] != NULL; k++) *args[k] = FALSE;
  l = strlen (switchars);
  m = 1;
  for (i = 1; i < *ac; i++)
  /*{{{   check args   */
  {
    for (j = 0; (j < l) && (switchars[j] != *argv[i]); j++);
    if (j != l)
    {
      str = &argv[i][1];
      if (*str != '\0')
      {
        for (k = 0; (options[k] != NULL) && !strcmp_lc (str, options[k]); k++);
        if (options[k] != NULL)
          *args[k] = TRUE;
        else argv[m++] = argv[i];
      }
      else argv[m++] = argv[i];
    }
    else argv[m++] = argv[i];
  }
  /*}}}*/
  *ac = m;
}
/*}}}*/
