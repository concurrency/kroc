/*
 *	Command line interface
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
   PLAY:COMMAND5_C@502.AAAA-FILE;1(20-FEB-92)[FROZEN] */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <string.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#include <ctype.h>
#include "toolkit.h"
#define MAX_LINE 256
/*{{{   PUBLIC int unfold_arg_list (ac, av, switchars, options, path)   */
PUBLIC int unfold_arg_list (ac, av, switchars, options, path)
int *ac;
char ***av;
char *switchars, **options, *path;
{
  int i, j, k, l, argc2, eol, going, res;
  FILE *fs;
  char **argv, *str, *newopt, **argv2, line[MAX_LINE];
  res = 0;
  argv = *av;
  argc2 = 0;
  argv2 = malloc_chk (++argc2 * sizeof (char *));
  argv2[0] = argv[0];
  i = 1;
  l = strlen (switchars);
  while (i < *ac)
  {
    /*{{{   copy args until indirection switch or end of args   */
    going = TRUE;
    while (going)
      if (i == *ac) going = FALSE;
      else
      {
        for (j = 0; (j < l) && (switchars[j] != *argv[i]); j++);
        if (j != l)
        {
          for (k = 0; ((options[k] != NULL) &&
                       !(str_semicmp_lc (options[k], &argv[i][1])));
                               k++);
          if (options[k] != NULL) going = FALSE;
          /*{{{   else copy arg   */
          else
          {
            argv2 = realloc_chk (argv2, (argc2 + 1) * sizeof (char *));
            argv2[argc2++] = argv[i++];
          }
          /*}}}  */
        }
        /*{{{   else copy arg   */
        else
        {
          argv2 = realloc_chk (argv2, (argc2 + 1) * sizeof (char *));
          argv2[argc2++] = argv[i++];
        }
        /*}}}  */
      }
    /*}}}  */
    if (i < *ac)
    /*{{{   if indirection load in options   */
    {
      l = strlen (options[k]);
      str = &argv[i][l + 1];
      l = strlen (str);
      if (l == 0)
      {
        if ((i + 1) < *ac)
        {
          str = argv[++i];
          i++;
        }
        else
        {
          argv2 = realloc_chk (argv2, (argc2 + 1) * sizeof (char *));
          argv2[argc2++] = argv[i++];
        }
      }
      else i++;
      if (*str != '\0')
      {
        fs = pathopen (str, path, line, "r");
        if (fs == NULL)
        {
          fprintf (stderr, "Could not open indirect file %s\n", str);
          exit (EXIT_FAILURE);
        }
        res++;
        while (fgets (line, MAX_LINE, fs) != NULL)
        {
          str = line;
          eol = FALSE;
          while (!eol)
          {
            while (isspace (*str)) str++;
            if (*str == '\0') eol = TRUE;
            else
            {
              newopt = str;
              while ((!isspace (*str)) && (*str != '\0')) str++;
              if (*str != '\0') *str++ = '\0';
              argv2 = realloc_chk (argv2, (argc2 + 1) * sizeof (char *));
              argv2[argc2++] = str_duplicate (newopt);
            }
          }
        }
        fclose (fs);
      }
    }
    /*}}}  */
  }
  *ac = argc2;
  *av = argv2;
  return (res);
}
/*}}}  */
