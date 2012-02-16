/* $Id: arg0.h,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	arglib local header file
 *	Copyright (C) 1989, 1990, 1994 Inmos limited
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

/* History */

/* bas: First version written 940809 - moved from arg.c */

#ifndef __arg0_h
#define __arg0_h

/*{{{  Defines */
#define NUL          '\0'
#define QUOTE        '"'
#define FILE_MARKER  '@'
#define COMMENT      '#'
#define BAR          '|'
#define NL           '\n'

/* The following define the strings used to denote the host machine on */
/* help pages. They are defined carefully to be the same length.       */

#define ARCH_OS_STRING TARGET_CANONICAL
/*}}}  */
/*{{{  Typedefs */
typedef enum {
  false = 0,
  true = 1
} bool;

typedef enum {
  ident = 0,
  prefix = 1,
  noteq = 2
} str_compare;
/*}}}  */
/*{{{  Prototypes */
char const *arg2_copy_token (char const *const st, char const *const en);
str_compare arg2_compare (char const *const sub, char const *const str);
char *arg2_new_string (char const *const old_str);
/*}}}  */

#endif
