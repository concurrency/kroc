/*{{{  module header */

/*
 *	INMOS search path routines
 *	Copyright (C) 1988-1990 Inmos Limited
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

/*}}}*/

#ifndef _POPENH
/*{{{  not included header file */
#define _POPENH

/*{{{  include files */
#ifndef _IMSTYPEH
#include "imstype.h"
#endif
#ifndef _IMSOPENH
#include "imsopen.h"
#endif
#ifndef _IMSMISCH
#include "imsmisc.h"
#endif
#ifndef _IMSSTDH
#include "imsstd.h"
#endif
/*}}}*/

/*{{{  constant definitions */
#define POPEN_TEXT_MODE   0
#define POPEN_BINARY_MODE 1

#define SYS_SEARCH_PATH "ISEARCH" /* INMOS search path name */
/*}}}*/

/*{{{  function definitions */
#define POPEN_READ_TEXT(Source, Variable, Stream) Stream = POpenRead(Source, Variable, NULL, POPEN_TEXT_MODE)

#define POPEN_READ_BINARY(Source, Variable, Stream) Stream = POpenRead(Source, Variable, NULL, POPEN_BINARY_MODE)

#define popen_read(Source, Variable, Destination, Mode) POpenRead(Source, Variable, Destination, Mode)

#define POPEN_RELATIVE_READ_TEXT(Source, Variable, Relative, Stream) Stream = POpenRelativeRead(Source, Variable, Relative, NULL, POPEN_TEXT_MODE)

#define POPEN_RELATIVE_READ_BINARY(Source, Variable, Relative, Stream) Stream = POpenRelativeRead(Source, Variable, Relative, NULL, POPEN_BINARY_MODE)

#define popen_relative_read(Source, Variable, Relative, Destination, Mode) POpenRelativeRead(Source, Variable, Destination, Relative, Mode)
/*}}}*/

/*{{{  function prototypes */
/*{{{  from fname.c */
EXTERNAL CHAR *LffCreateFileName PARMS((CHAR *, VAL CHAR *, VAL INT, VAL INT, VAL BOOL));

EXTERNAL CHAR *LffCreateSimpleFileName PARMS((CHAR *, VAL CHAR *, VAL BOOL));

EXTERNAL CHAR *TcoffCreateFileName PARMS((CHAR *, VAL CHAR *, VAL BIT32 *, VAL BOOL));

EXTERNAL CHAR *TcoffCreateSimpleFileName PARMS((CHAR *, VAL CHAR *, VAL BOOL));
/*}}}*/

/*{{{  from popen.c */
EXTERNAL FILE *_POpenRead PARMS((VAL CHAR *, VAL CHAR *, CHAR *, VAL INT));

EXTERNAL FILE *POpenRead PARMS((VAL CHAR *, VAL CHAR *, CHAR **, VAL INT));

EXTERNAL FILE *POpenRelativeRead PARMS((VAL CHAR *, VAL CHAR *, VAL CHAR *, CHAR **, VAL INT));
/*}}}*/
/*}}}*/
/*}}}*/
#endif
