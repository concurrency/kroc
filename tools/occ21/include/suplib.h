/*{{{  module header */

/*
 *	Supplimentary functions
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

#ifndef _SUPLIBH
#define _SUPLIBH

#include <string.h>

#ifdef target_cpu_alpha
#define sup_qsort qsort
/* avoid problem of casting pointers to integers */
#else
/*{{{  sup_qsort */
/*
 *  sup_qsort() is compatible with the ANSI standard qsort(), except that
 *              it has the same effect on all hosts.
 *              (The ANSI standard does not define whether qsort() does
 *              or does not reorder equal keys, so different implementations
 *              on different hosts may or may not reorder equal keys.
 *              It is not defined whether sup_qsort() does or does not
 *              reorder equal keys either, but it is guaranteed that its
 *              behaviour is the same on all supported hosts.)
 */

extern void sup_qsort(void * /*base*/, size_t /*nmemb*/, size_t /*size*/,
                      int (* /*compar*/)(const void *, const void *));
/*}}}  */
#endif

/*{{{  sup_strlwr */
/*
 *  sup_strlwr(s) replaces each upper-case character in the string s with
 *                the corresponding lower-case character.
 *                Non-upper-case characters are not altered.
 *                A pointer to s is returned.
 */

extern char *sup_strlwr(char * /*s*/);
/*}}}  */

/*{{{  sup_strupr */
/*
 *  sup_strupr(s) replaces each lower-case character in the string s with
 *                the corresponding upper-case character.
 *                Non-lower-case characters are not altered.
 *                A pointer to s is returned.
 */

extern char *sup_strupr(char * /*s*/);
/*}}}  */

#endif
