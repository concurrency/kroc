/*
 *	support.h - support interface
 *	Copyright (C) 2000 Fred Barnes <frmb@kent.ac.uk>
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


#ifndef __SUPPORT_H
#define __SUPPORT_H

#ifdef TRACE_MEMORY
	#define smalloc(X) ss_malloc(__FILE__,__LINE__,X)
	#define srealloc(X,A,B) ss_realloc(__FILE__,__LINE__,X,A,B)
	#define sfree(X) ss_free(__FILE__,__LINE__,X)

	extern void *ss_malloc (char *, int, size_t);
	extern void *ss_realloc (char *, int, void *, size_t, size_t);
	extern void ss_free (char *, int, void *);
	extern void ss_cleanup (void);
#else
	extern void *smalloc (size_t);
	extern void *srealloc (void *, size_t, size_t);
	extern void sfree (void *);
#endif

extern int decode_hex_byte (char b1, char b2, unsigned char *tptr);
extern char *string_ndup (char *, int);
extern char *string_dup (char *);
extern void *mem_ndup (void *, int);
extern char **split_string (char *str, int copy);
extern char **split_stringsc (char *str, int copy);
extern int string_dequote (char *str);
extern int string_dequote2 (char *str, int *len);


#endif	/* !__SUPPORT_H */

