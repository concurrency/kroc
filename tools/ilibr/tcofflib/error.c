/*
 *	Error handling function
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
   PLAY:ERROR_C@506.AAAA-FILE;1(20-FEB-92)[FROZEN] */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "toolkit.h"

#define ERR_STREAM stderr

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
