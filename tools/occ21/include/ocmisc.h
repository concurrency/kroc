/*
 *	Handle different implementations of C
 *	Copyright (C) 1991 Inmos Limited
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


char *strlwr (char *s);
char *strupr (char *s);

/* We have a local copy of qsort so that its behaviour is identical on
   all machines
*/
void local_qsort (void *base, size_t nmemb, size_t size,
            int (* compar)(const void *, const void *));

#if defined(COMPILER_IS_LLL)
#ifndef target_cpu_alpha
char *strrchr ();
#endif
#endif

#if 0 /*defined(SUN) || defined(GNU)*/
PUBLIC const char *strstr (const char *a, const char *b);
#endif
