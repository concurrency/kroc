/*{{{  module header */

/*
 *	Standard INMOS header file
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

#ifndef _IMSMISCH
/*{{{  not included header file */
#define _IMSMISCH

/*{{{  C scope definitions */
#define PUBLIC
#define PRIVATE  static
#define EXTERNAL extern
#define FORWARD
#define EXTERN   extern
/*}}}  */

/*{{{  constant definitions */
#define MAX_STRING_SIZE 256
#define MAX_BLOCK_SIZE  1024
/*}}}  */

/*{{{  host compiler macros */
#undef _HOST_SEEN

/*{{{  UNIX macros */
#ifdef HOST_OS_IS_UNIX
#define _HOST_SEEN

/* HMM */
#define ANSI /* ANSI C conformant compiler */

#define PRIVATEPARAM PRIVATE

#endif /* HOST_OS_IS_UNIX */
/*}}}  */

/*{{{  MS C specific macros */
#if defined(HOST_OS_IS_MSDOS) && defined(COMPILER_IS_MSC) && !defined(_HOST_SEEN)
#define _HOST_SEEN
/*{{{   */
#define ANSI /* ANSI C conformant compiler */

#define PRIVATEPARAM PRIVATE
/*}}}  */
#endif
/*}}}  */

/*{{{  DEC C VAX specific macros */
#if defined(HOST_OS_IS_VMS) && !defined(_HOST_SEEN)
#define _HOST_SEEN
/*{{{   */
#define ANSI /* ANSI C conformant compiler */

#define PRIVATEPARAM PRIVATE
/*}}}  */
#endif
/*}}}  */

/*{{{  3L C specific macros */
#if defined(COMPILER_IS_LLL) && !defined(_HOST_SEEN)
#define _HOST_SEEN
/*{{{   */

#define PRIVATEPARAM
/*}}}  */
#endif
/*}}}  */


/*{{{  IMS C specific macros */
#if defined(IMS) && !defined(_HOST_SEEN)
#define _HOST_SEEN
/*{{{   */
#define ANSI /* ANSI C conformant compiler */

#define PRIVATEPARAM PRIVATE
/*}}}  */
#endif
/*}}}  */

/*{{{  WATCOM C specific macros */
#if defined(HOST_OS_IS_MSDOS) && defined(COMPILER_IS_WATCOM) && !defined(_HOST_SEEN)
#define _HOST_SEEN
/*{{{   */
#define ANSI /* ANSI C conformant compiler */

#define PRIVATEPARAM PRIVATE
/*}}}  */
#endif
/*}}}  */
/*}}}  */

/*{{{  C portability macros */
#ifdef ANSI
/*{{{   */
#define PARMS(X) X

#define VAL const
/*}}}  */
#else
/*{{{   */
#define PARMS(X) ()

#define VAL
/*}}}  */
#endif
/*}}}  */
/*}}}  */
#endif
