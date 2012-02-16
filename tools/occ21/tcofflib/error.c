/*  $Id: error.c,v 1.3 1998/06/04 15:13:45 djb1 Exp $    */

/*
 *	TCOFF error handling
 *	Copyright (C) 1994 Inmos limited
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
#include <stdlib.h>
#include <stdarg.h>
#include "toolkit.h"

#ifdef NO_STDERR_STREAM
#define ERR_STREAM stdout
#else
#define ERR_STREAM stderr
#endif

PUBLIC int error (int level, char const *progname, char const *filename, char const *fmt, ...)
{
  va_list args;
  int stop;
  switch (level)
  {
    case (ERR_WARNING): fprintf (ERR_STREAM,"Warning-"); stop = FALSE; break;
    case (ERR_ERROR):   fprintf (ERR_STREAM,"Error-");   stop = FALSE; break;
    case (ERR_SERIOUS): fprintf (ERR_STREAM,"Serious-"); stop = TRUE;  break;
    case (ERR_FATAL):   fprintf (ERR_STREAM,"Fatal-");   stop = TRUE;  break;
    default: fprintf (ERR_STREAM,"Fatal-%s-unknown-error-type\n",progname);
             exit (EXIT_FAILURE);
             break;
  }

  if (*progname != '\0') fprintf (ERR_STREAM,"%s", progname);
  if (*filename != '\0') fprintf (ERR_STREAM,"-%s", filename);
  if (*fmt != '(') fprintf (ERR_STREAM, "-");
  va_start (args, fmt);
  vfprintf (ERR_STREAM, fmt, args);
  fprintf (ERR_STREAM,"\n");
  va_end (args);

  if (stop) exit (EXIT_FAILURE);
  return (EXIT_FAILURE);
}
