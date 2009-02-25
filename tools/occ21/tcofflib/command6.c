/*  $Id: command6.c,v 1.1 1996/04/15 10:54:16 djb1 Exp $    */

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
#include <stdlib.h>
#include <ctype.h>
#include "toolkit.h"

/*{{{   PUBLIC void env_to_option (ac, av, env)   */
PUBLIC void env_to_option (ac, av, env)
int *ac;
char const **av[];
char *env;
{
  int i, argc2, finished;
  char const **argv; 
  char *line, *str, *ptr;
  char const **argv2;
  argc2 = 0;
  argv = *av;
  line = (char *) getenv (env);
  if (line != NULL)
  {
    line = str_duplicate (line);
    argv2 = malloc_chk (++argc2 * sizeof (char *));
    argv2[0] = argv[0];
    /*{{{   copy in env line   */
    str = line;
    finished = FALSE;
    while (!finished)
    {
      while (isspace (*str)) str++;
      if (*str == '\0') finished = TRUE;
      else
      {
        ptr = str;
        while ((*ptr != '\0') && !isspace (*ptr)) ptr++;
        if (*ptr == '\0') finished = TRUE;
        else *ptr = '\0';
        argv2 = realloc_chk (argv2, (argc2 + 1) * sizeof (char *));
        argv2[argc2++] = str;
        str = ++ptr;
      }
    }
    /*}}}*/
    /*{{{   copy normal args   */
    i = 1;
    while (i < *ac)
    {
      argv2 = realloc_chk (argv2, (argc2 + 1) * sizeof (char *));
      argv2[argc2++] = argv[i++];
    }
    /*}}}*/
    *ac = argc2;
    *av = argv2;
  }
}
/*}}}*/
