/*
 *	support.h - support interface
 *	Copyright (C) 2000-2003 Fred Barnes <frmb@kent.ac.uk>
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
extern char *string_ndup (char *, int);
extern char *string_dup (char *);
extern void *mem_ndup (void *, int);

/* fresh dynamic array stuff: HACK */
#define DA_CUR(NAME) NAME ## _cur
#define DA_MAX(NAME) NAME ## _max
#define DA_PTR(NAME) NAME
#define DA_NTHITEM(NAME,N) (NAME)[(N)]
#define DYNARRAY(TYPE,NAME) int DA_CUR(NAME), DA_MAX(NAME); TYPE* DA_PTR(NAME)
#define DYNSARRAY(TYPE,NAME) int DA_CUR(NAME), DA_MAX(NAME); struct TYPE* DA_PTR(NAME)
#define STATICDYNARRAY(TYPE,NAME) static int DA_CUR(NAME), DA_MAX(NAME); static TYPE* DA_PTR(NAME)

extern void da_init (int *cur, int *max, void ***array);
extern void da_additem (int *cur, int *max, void ***array, void *item);
extern int da_maybeadditem (int *cur, int *max, void ***array, void *item);
extern void da_delitem (int *cur, int *max, void ***array, int idx);
extern void da_rmitem (int *cur, int *max, void ***array, void *item);
extern void da_trash (int *cur, int *max, void ***array);

#define dynarray_init(ARRAY) da_init(&(DA_CUR(ARRAY)), &(DA_MAX(ARRAY)), (void ***)&(DA_PTR(ARRAY)))
#define dynarray_add(ARRAY,ITEM) da_additem(&(DA_CUR(ARRAY)), &(DA_MAX(ARRAY)), (void ***)&(DA_PTR(ARRAY)), (void *)(ITEM))
#define dynarray_maybeadd(ARRAY,ITEM) da_maybeadditem(&(DA_CUR(ARRAY)), &(DA_MAX(ARRAY)), (void ***)&(DA_PTR(ARRAY)), (void *)(ITEM))
#define dynarray_delitem(ARRAY,IDX) da_delitem(&(DA_CUR(ARRAY)), &(DA_MAX(ARRAY)), (void ***)&(DA_PTR(ARRAY)), IDX)
#define dynarray_rmitem(ARRAY,ITEM) da_rmitem(&(DA_CUR(ARRAY)), &(DA_MAX(ARRAY)), (void ***)&(DA_PTR(ARRAY)), (void *)(ITEM))
#define dynarray_trash(ARRAY) da_trash(&(DA_CUR(ARRAY)), &(DA_MAX(ARRAY)), (void ***)&(DA_PTR(ARRAY)))

extern int decode_hex_byte (char b1, char b2, unsigned char *tptr);
extern int parse_uint16hex (char *ch);
extern char *mkhexbuf (unsigned char *buffer, int buflen);
extern char **split_string (char *str);
extern char *decode_hexstr (char *str, int *slen);
extern int time_after (struct timeval *t1, struct timeval *t2);
extern void time_minus (struct timeval *t1, struct timeval *t2, struct timeval *t3);
extern void time_setin (struct timeval *t1, struct timeval *t2);

#endif	/* !__SUPPORT_H */

