/*
 *	Hex dumping routines
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
   PLAY:HEXDUMP_C@508.AAAA-FILE;1(20-FEB-92)[FROZEN] */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#ifdef __STDC__
#include <stdlib.h>
#endif
#include <string.h>
#include "toolkit.h"

#ifdef __STDC__
extern int dis_get_char (void);
#else
extern int dis_get_char ();
#endif

/*{{{   PUBLIC void hexdump (pos, fs)   */
PUBLIC void hexdump (pos, fs)
const long int pos;
FILE *fs;
{
  long int posn;
  char string[17], *ptr;
  int c, i, k;
  posn = pos;
  c = dis_get_char ();
  while (c != EOF)
  {
    fprintf (fs, "%08lX", posn);
    ptr = string;
    for (k = 0; k < 4; k++)
    {
      fprintf (fs, " ");
      for (i = 0; i < 4; i++)
      {
        /*{{{   print char   */
        if ((c >= ' ') && (c <= '~')) /* printable */
        {
          *ptr++ = (char) c;
          fprintf (fs, "%02X", c);
        }
        else if (c == EOF)            /* eof */
        {
          *ptr++ = ' ';
          fprintf (fs, "  ");
        }
        else                          /* non printable */
        {
          *ptr++ = '.';
          fprintf (fs, "%02X", c);
        }
        /*}}}*/
        posn++;
        if (c != EOF) c = dis_get_char ();
      }
    }
    *ptr = '\0';
    fprintf (fs, "      %s\n", string);
  }
}
/*}}}*/
