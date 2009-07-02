/*{{{  module header*/

/*
 *	Standard INMOS header for includes
 *	Copyright (C) 1987 Inmos Limited
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

/*}}}  */

#ifndef _IMSSTDH
/*{{{  not included header file*/
#define _IMSSTDH

/*{{{  standard includes*/
#ifndef IMS_INCLUDED_STDIO_H
#include <stdio.h>
#endif
#include <errno.h>
#include <string.h>
#include <ctype.h>
/*}}}  */

/*{{{  host compiler macros*/
/*{{{  MS C specific macros*/
#ifdef COMPILER_IS_MSC
/*{{{  */
#include <stdlib.h>
#include <stdarg.h>

#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0

#define exit_repeat(Status) exit(Status)
/*}}}  */
#endif
/*}}}  */

/*{{{  DEC C specific macros*/
#ifdef HOST_OS_IS_VMS
/*{{{  */
#include <ssdef.h>
#include <stdlib.h>
#include <stdarg.h>

#define EXIT_FAILURE SS$_ABORT
#define EXIT_SUCCESS SS$_NORMAL

#define exit_repeat(Status) exit(Status)
/*}}}  */
#endif
/*}}}  */

/*{{{  3L C specific macros*/
#ifdef COMPILER_IS_LLL
/*{{{  */
#include <stdlib.h>
#ifdef target_cpu_alpha
#include <stdarg.h>
#else
#include <varargs.h>

#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0
#endif
#define exit_repeat(Status) exit(Status)
/*}}}  */
#endif
/*}}}  */

/*{{{  GNU C specific macros*/
#ifdef HOST_OS_IS_UNIX
/*{{{  */
/*{{{  include <stdlib.h>*/
#include <stdlib.h>

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

/*
extern int unlink (const char *);
#define remove(String) unlink(String)

extern void bcopy (const char *, char *, const int);
REMOVED MDP 6/2/95 */
#ifdef target_cpu_alpha
#define memmove(Block1, Block2, Length) bcopy((const char *) Block2, (char *) Block1, (const int) Length)
#endif
/*}}}  */
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0
#endif
/* included in stdlib.h on OSF/1 */

#define exit_repeat(Status) exit(Status)
/*}}}  */
#endif
/*}}}  */

/*{{{  IMS C specific macros*/
#ifdef IMS
/*{{{  */
#include <stdlib.h>
#include <stdarg.h>

/*{{{  define exit_repeat*/
#include <misc.h>
#include <host.h>
/*}}}  */
/*}}}  */
#endif
/*}}}  */

/*{{{  WATCOM C specific macros*/
#ifdef COMPILER_IS_WATCOM
/*{{{  */
#include <stdlib.h>
#include <stdarg.h>

#define exit_repeat(Status) exit(Status)
/*}}}  */
#endif
/*}}}  */
/*}}}  */
/*}}}  */
#endif
