/*
 *	compdyn.c -- compiler dynamics for occ21 (dynamic arrays, string-hashes, pointer-hashes)
 *	Copyright (C) 2009 Fred Barnes <frmb@kent.ac.uk>
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
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#include "midinc.h"
#include "occompfe.h"
#include "harndef.h"
#include "suplib.h"

#include "compdyn.h"

#include <stdarg.h>


/*{{{  void *smalloc (size_t length)*/
/*
 *	allocates some memory
 */
void *smalloc (size_t length)
{
	void *tmp;
	const int old = switch_to_real_workspace ();

	tmp = (void *)memalloc (length);
	memset (tmp, 0, length);

	switch_to_prev_workspace (old);
	return tmp;
}
/*}}}*/
/*{{{  void *srealloc (void *ptr, size_t old_size, size_t new_size)*/
/*
 *	re-allocates a memory block, moving it entirely if necessary
 */
void *srealloc (void *ptr, size_t old_size, size_t new_size)
{
	void *tmp;

	if (!ptr || !old_size) {
		tmp = smalloc (new_size);
	} else {
		/* malloc-malloc reallocation */
		tmp = smalloc (new_size);
		if (new_size > old_size) {
			memcpy (tmp, ptr, old_size);
		} else {
			memcpy (tmp, ptr, new_size);
		}
	}
	return tmp;
}
/*}}}*/
/*{{{  void sfree (void *ptr)*/
/*
 *	frees previously allocated memory
 */
void sfree (void *ptr)
{
	if (ptr) {
		memfree (ptr);
	}
	return;
}
/*}}}*/

/*{{{  static void compdyn_fatal (const char *s, ...)*/
/*
 *	fatal error handling
 */
static void compdyn_fatal (const char *s, ...)
{
	static char msgbuf[1024];
	va_list ap;

	va_start (ap, s);
	vsnprintf (msgbuf, 1023, s, ap);
	va_end (ap);

	err_abort (msgbuf);
}
/*}}}*/

/*{{{  char *string_ndup (const char *str, int length)*/
/*
 *	duplicates a chunk of string
 */
char *string_ndup (const char *str, int length)
{
	char *tmp;

	tmp = (char *)smalloc (length + 1);
	memcpy (tmp, str, length);
	tmp[length] = '\0';
	return tmp;
}
/*}}}*/
/*{{{  char *string_dup (const char *str)*/
/*
 *	duplicates a string
 */
char *string_dup (const char *str)
{
	return string_ndup (str, strlen (str));
}
/*}}}*/
/*{{{  void *mem_ndup (const void *ptr, int length)*/
/*
 *	duplicates a bit of memory
 */
void *mem_ndup (const void *ptr, int length)
{
	void *tmp;

	tmp = smalloc (length);
	memcpy (tmp, ptr, length);
	return tmp;
}
/*}}}*/

/*{{{  void da_init (int *cur, int *max, void ***array)*/
/*
 *	initialises a dynamic array
 */
void da_init (int *cur, int *max, void ***array)
{
	*cur = 0;
	*max = 0;
	*array = NULL;
	return;
}
/*}}}*/
/*{{{  void da_additem (int *cur, int *max, void ***array, void *item)*/
/*
 *	adds an item to a dynamic array (at the end of it)
 */
void da_additem (int *cur, int *max, void ***array, void *item)
{
	if (*max == 0) {
		*array = (void **)smalloc (8 * sizeof (void *));
		*max = 8;
	} else if (*cur == *max) {
		*array = (void **)srealloc ((void *)(*array), *max * sizeof(void *), (*max + 8) * sizeof(void *));
		*max = *max + 8;
	}
	(*array)[*cur] = item;
	*cur = *cur + 1;
	return;
}
/*}}}*/
/*{{{  void da_insertitem (int *cur, int *max, void ***array, void *item, int idx)*/
/*
 *	inserts an item in a dynamic array (first position is 0)
 */
void da_insertitem (int *cur, int *max, void ***array, void *item, int idx)
{
	int i;

	if (*max == 0) {
		*array = (void **)smalloc (8 * sizeof (void *));
		*max = 8;
	} else if (*cur == *max) {
		*array = (void **)srealloc ((void *)(*array), *max * sizeof(void *), (*max + 8) * sizeof(void *));
		*max = *max + 8;
	}
	if (idx > *cur) {
		idx = *cur;
	}
	for (i = (*cur - 1); i >= idx; i--) {
		(*array)[i+1] = (*array)[i];
	}
	(*array)[idx] = item;
	*cur = *cur + 1;
	return;
}
/*}}}*/
/*{{{  int da_maybeadditem (int *cur, int *max, void ***array, void *item)*/
/*
 *	adds an item to a dynamic array, but only if it's not there already
 */
int da_maybeadditem (int *cur, int *max, void ***array, void *item)
{
	int idx;

	for (idx = 0; idx < *cur; idx++) {
		if ((*array)[idx] == item) {
			return 0;
		}
	}
	da_additem (cur, max, array, item);
	return 1;
}
/*}}}*/
/*{{{  void da_delitem (int *cur, int *max, void ***array, int idx)*/
/*
 *	removes an item from a dynamic array, at a specified index
 */
void da_delitem (int *cur, int *max, void ***array, int idx)
{
	if (idx >= *cur) {
		compdyn_fatal ("item at index %d in array at %p (%d,%d) does not exist!", idx, *array, *cur, *max);
	}
	if (idx == (*cur - 1)) {
		*cur = *cur - 1;
	} else {
		void **walk;
		int i = (*cur - idx) - 1;

		for (walk = (*array) + idx; i > 0; walk++, i--) {
			walk[0] = walk[1];
		}
		*cur = *cur - 1;
	}
	return;
}
/*}}}*/
/*{{{  void da_rmitem (int *cur, int *max, void ***array, void *item)*/
/*
 *	removes an item from a dynamic array, based on its value
 *	(scans array and calls da_delitem)
 */
void da_rmitem (int *cur, int *max, void ***array, void *item)
{
	int idx;

	for (idx = 0; idx < *cur; idx++) {
		if ((*array)[idx] == item) {
			da_delitem (cur, max, array, idx);
			idx--;
		}
	}
	return;
}
/*}}}*/
/*{{{  void da_trash (int *cur, int *max, void ***array)*/
/*
 *	trashes a dynamic array and resets it to zero.  doesn't do anything with any contents
 */
void da_trash (int *cur, int *max, void ***array)
{
	if (*max && *array) {
		sfree (*array);
	}
	*array = NULL;
	*cur = 0;
	*max = 0;
	return;
}
/*}}}*/
/*{{{  void da_qsort (void **array, int first, int last, int (*cfcn)(void *, void *))*/
/*
 *	does a quick-sort on a dynamic array using the compare function given
 */
void da_qsort (void **array, int first, int last, int (*cfcn)(void *, void *))
{
	int i, j;
	void *pivot;

#if 0
fprintf (stderr, "da_qsort(): array=0x%8.8x, first=%d, last=%d\n", (unsigned int)array, first, last);
#endif
	pivot = array[(first + last) >> 1];
	i = first;
	j = last;
	while (i <= j) {
		void *tmp;

#if 0
fprintf (stderr, "da_qsort(): i=%d, j=%d, pivot=(0x%8.8x), array[i]=(0x%8.8x), array[j]=(0x%8.8x)\n", i, j, (unsigned int)pivot, (unsigned int)array[i], (unsigned int)array[j]);
#endif
		while (cfcn (array[i], pivot) < 0) {
			i++;
		}
		while (cfcn (array[j], pivot) > 0) {
			j--;
		}
		if (i <= j) {
			tmp = array[i];
			array[i] = array[j];
			array[j] = tmp;
			i++;
			j--;
		}
	}
	if (j > first) {
		da_qsort (array, first, j, cfcn);
	}
	if (i < last) {
		da_qsort (array, i, last, cfcn);
	}
	return;
}
/*}}}*/
/*{{{  void da_setsize (int *cur, int *max, void ***array, int size)*/
/*
 *	sets the size of a dynamic array
 */
void da_setsize (int *cur, int *max, void ***array, int size)
{
	if (size < 0) {
		compdyn_fatal ("dynarray_setsize to %d", size);
	} else if (!size) {
		da_trash (cur, max, array);
	} else {
		if (*max == 0) {
			*array = (void **)smalloc (size * sizeof (void *));
			*max = size;
			*cur = size;
		} else if (size > *max) {
			*array = (void **)srealloc ((void *)(*array), *max * sizeof(void *),
					size * sizeof(void *));
			*max = size;
			*cur = size;
		} else {
			*cur = size;
		}
	}
}
/*}}}*/
/*{{{  void da_setmax (int *cur, int *max, void ***array, int size)*/
/*
 *	sets the maximum size of a dynamic array
 */
void da_setmax (int *cur, int *max, void ***array, int size)
{
	if (size < 0) {
		compdyn_fatal ("dynarray_setmax to %d", size);
	} else if (!size) {
		da_trash (cur, max, array);
	} else {
		if (*max == 0) {
			*array = (void **)smalloc (size * sizeof (void *));
			*max = size;
			*cur = 0;
		} else if (size > *max) {
			*array = (void **)srealloc ((void *)(*array), *max * sizeof(void *),
					size * sizeof(void *));
			*max = size;
		} else if (size < *max) {
			/* making the array smaller */
			*array = (void **)srealloc ((void *)(*array), *max * sizeof(void *),
					size * sizeof(void *));
			*max = size;
			if (*cur > *max) {
				*cur = *max;
			}
		}
	}
}
/*}}}*/
/*{{{  void da_copy (int srccur, int srcmax, void **srcarray, int *dstcur, int *dstmax, void ***dstarray)*/
/*
 *	copies a dynamic array (and its contents)
 */
void da_copy (int srccur, int srcmax, void **srcarray, int *dstcur, int *dstmax, void ***dstarray)
{
	if (!srccur) {
		/* nothing to copy! */
		return;
	}
	if (!(*dstmax)) {
		/* empty destination, allocate it */
		*dstarray = (void **)smalloc (srcmax * sizeof (void *));
		*dstmax = srcmax;
		*dstcur = 0;
	} else if ((*dstcur + srccur) >= *dstmax) {
		/* destination (maybe) needs reallocating */
		*dstarray = (void **)srealloc ((void *)(*dstarray), (*dstmax) * sizeof(void *),
				(*dstcur + srccur + 1) * sizeof(void *));
		*dstmax = (*dstcur + srccur + 1);
	}
	/* stick elements from srcarray[0..srccur-1] into dstarray[dstcur..] */
	memcpy (&((*dstarray)[*dstcur]), &((srcarray)[0]), srccur * sizeof (void *));
	*dstcur = *dstcur + srccur;

	return;
}
/*}}}*/


/*{{{  void sh_init (int *bsizes, void ***table, char ***keys, void **fnptr, int *sizep, int *bitsizep, int bitsize)*/
/*
 *	initialises a string-hash
 */
void sh_init (int *bsizes, void ***table, char ***keys, void **fnptr, int *sizep, int *bitsizep, int bitsize)
{
	int i;
	int size = (1 << bitsize);

	*sizep = size;
	*bitsizep = bitsize;
	*fnptr = (void *)sh_lookup;

	for (i=0; i<size; i++) {
		bsizes[i] = 0;
		table[i] = NULL;
		keys[i] = NULL;
	}
	return;
}
/*}}}*/
/*{{{  void sh_sinit (int *bsizes, void ***table, char ***keys, int bitsize)*/
/*
 *	initialises a string-hash
 */
void sh_sinit (int *bsizes, void ***table, char ***keys, int bitsize)
{
	int i;
	int size = (1 << bitsize);

	for (i=0; i<size; i++) {
		bsizes[i] = 0;
		table[i] = NULL;
		keys[i] = NULL;
	}
	return;
}
/*}}}*/
/*{{{  static unsigned int sh_hashcode (char *str, int bitsize)*/
/*
 *	returns a hash-code for some string
 */
static unsigned int sh_hashcode (char *str, int bitsize)
{
	unsigned int hc = (0x55a55a55 << bitsize);

	for (; *str != '\0'; str++) {
		unsigned int chunk = (unsigned int)*str;
		unsigned int top;

		hc ^= chunk;
		top = (hc >> ((sizeof(unsigned int) * 8) - bitsize));
		hc <<= (bitsize >> 1);
		hc ^= top;
	}
	return hc;
}
/*}}}*/
/*{{{  void sh_insert (int *bsizes, void ***table, char ***keys, int bitsize, void *item, char *key)*/
/*
 *	inserts an item into a string-hash
 */
void sh_insert (int *bsizes, void ***table, char ***keys, int bitsize, void *item, char *key)
{
	unsigned int hcode = sh_hashcode (key, bitsize);
	int bucket = hcode & ((1 << bitsize) - 1);

#if 0 && defined(DEBUG)
fprintf (stderr, "sh_insert: adding item [%s] (0x%8.8x)\n", key, (unsigned int)item);
#endif
	if (!bsizes[bucket] || !table[bucket] || !keys[bucket]) {
		table[bucket] = (void **)smalloc (sizeof (void *));
		table[bucket][0] = item;

		keys[bucket] = (char **)smalloc (sizeof (char *));
		keys[bucket][0] = key;

		bsizes[bucket] = 1;
	} else {
		table[bucket] = (void **)srealloc ((void *)table[bucket],
					bsizes[bucket] * sizeof(void *),
					(bsizes[bucket] + 1) * sizeof(void *));
		table[bucket][bsizes[bucket]] = item;
		keys[bucket] = (char **)srealloc ((void *)keys[bucket],
					bsizes[bucket] * sizeof(char *),
					(bsizes[bucket] + 1) * sizeof(char *));
		keys[bucket][bsizes[bucket]] = key;

		bsizes[bucket]++;
	}
	return;
}
/*}}}*/
/*{{{  void sh_remove (int *bsizes, void ***table, char ***keys, int bitsize, void *item, char *key)*/
/*
 *	removes an item from a string-hash
 */
void sh_remove (int *bsizes, void ***table, char ***keys, int bitsize, void *item, char *key)
{
	unsigned int hcode = sh_hashcode (key, bitsize);
	int bucket = hcode & ((1 << bitsize) - 1);

	if (!bsizes[bucket] || !table[bucket] || !keys[bucket]) {
		return;
	} else {
		int i;

		for (i=0; i<bsizes[bucket]; i++) {
			if ((sh_hashcode (keys[bucket][i], bitsize) == hcode) && (item == table[bucket][i]) && (!strcmp (keys[bucket][i], key))) {
				/* shuffle up others */
				for (; i<(bsizes[bucket] - 1); i++) {
					table[bucket][i] = table[bucket][i+1];
					keys[bucket][i] = keys[bucket][i+1];
				}

				/* nullify the last entries */
				table[bucket][i] = NULL;
				keys[bucket][i] = NULL;

				/* now we have to make smaller */
				if (bsizes[bucket] == 1) {
					sfree (table[bucket]);
					sfree (keys[bucket]);
					table[bucket] = NULL;
					keys[bucket] = NULL;
				} else {
					table[bucket] = (void **)srealloc ((void *)table[bucket], bsizes[bucket] * sizeof (void *),
							(bsizes[bucket] - 1) * sizeof (void *));
					keys[bucket] = (char **)srealloc ((void *)keys[bucket], bsizes[bucket] * sizeof (void *),
							(bsizes[bucket] - 1) * sizeof (void *));
				}
				bsizes[bucket] = bsizes[bucket] - 1;

				return;
			}
		}
	}
	compdyn_fatal ("sh_remove(): item [0x%8.8x:%s] not in stringhash", (unsigned int)item, key);
	return;
}
/*}}}*/
/*{{{  void *sh_lookup (int *bsizes, void ***table, char ***keys, int bitsize, char *match)*/
/*
 *	looks up an item in a string-hash
 */
void *sh_lookup (int *bsizes, void ***table, char ***keys, int bitsize, char *match)
{
	unsigned int hcode = sh_hashcode (match, bitsize);
	int bucket = hcode & ((1 << bitsize) - 1);

	if (!bsizes[bucket] || !table[bucket] || !keys[bucket]) {
		return NULL;
	} else {
		int i;

		for (i=0; i<bsizes[bucket]; i++) {
			if ((sh_hashcode (keys[bucket][i], bitsize) == hcode) && (!strcmp (keys[bucket][i], match))) {
#if 0 && defined(DEBUG)
fprintf (stderr, "sh_lookup: match for [%s] found item 0x%8.8x\n", match, (unsigned int)(table[bucket][i]));
#endif
				return table[bucket][i];
			}
		}
	}
	return NULL;
}
/*}}}*/
/*{{{  void sh_dump (FILE *stream, int *bsizes, void ***table, char ***keys, int size)*/
/*
 *	dumps the contents of a string-hash (debugging)
 */
void sh_dump (FILE *stream, int *bsizes, void ***table, char ***keys, int size)
{
	int i;

	fprintf (stream, "hash-table size: %d buckets\n", size);
	for (i=0; i < size; i++) {
		fprintf (stream, "bucket %d:\tsize %d:\t", i, bsizes[i]);
		if (bsizes[i]) {
			int j;

			for (j=0; j<bsizes[i]; j++) {
				fprintf (stream, "%s%s (0x%8.8x)", (!j ? "" : ", "), keys[i][j], (unsigned int)(table[i][j]));
			}
		}
		fprintf (stream, "\n");
	}
}
/*}}}*/
/*{{{  void sh_walk (int *bsizes, void ***table, char ***keys, int size, void (*func)(void *, char *, void *), void *p)*/
/*
 *	walks the contents of a string-hash
 */
void sh_walk (int *bsizes, void ***table, char ***keys, int size, void (*func)(void *, char *, void *), void *p)
{
	int i, j;

	for (i=0; i<size; i++) {
		if (bsizes[i]) {
			for (j=0; j<bsizes[i]; j++) {
				func (table[i][j], keys[i][j], p);
			}
		}
	}
	return;
}
/*}}}*/
/*{{{  void sh_trash (int *bsizes, void ***table, char ***keys, int size)*/
/*
 *	destroys a string-hash
 */
void sh_trash (int *bsizes, void ***table, char ***keys, int size)
{
	int i;

	for (i=0; i<size; i++) {
		if (table[i]) {
			sfree (table[i]);
		}
		if (keys[i]) {
			sfree (keys[i]);
		}
	}

	return;
}
/*}}}*/


/*{{{  void ph_init (int *bsizes, void ***table, void ***keys, int *szptr, int *bszptr, void **fnptr, int bitsize)*/
/*
 *	initialises a pointer-hash
 */
void ph_init (int *bsizes, void ***table, void ***keys, int *szptr, int *bszptr, void **fnptr, int bitsize)
{
	int i;
	int size = (1 << bitsize);

	*szptr = size;
	*bszptr = bitsize;
	*fnptr = (void *)ph_lookup;
	for (i=0; i<size; i++) {
		bsizes[i] = 0;
		table[i] = NULL;
		keys[i] = NULL;
	}
	return;
}
/*}}}*/
/*{{{  void ph_sinit (int *bsizes, void ***table, void ***keys, int bitsize)*/
/*
 *	initialises a pointer-hash
 */
void ph_sinit (int *bsizes, void ***table, void ***keys, int bitsize)
{
	int i;
	int size = (1 << bitsize);

	for (i=0; i<size; i++) {
		bsizes[i] = 0;
		table[i] = NULL;
		keys[i] = NULL;
	}
	return;
}
/*}}}*/
/*{{{  static unsigned int ph_hashcode (void *ptr, int bitsize)*/
/*
 *	returns a pointer-hash for some pointer
 */
static unsigned int ph_hashcode (void *ptr, int bitsize)
{
	int i;
	unsigned int hc = (0x56756789 << bitsize);
	char *data = (char *)&ptr;

	for (i=0; i<sizeof(int); i++) {
		unsigned int chunk = (unsigned int)data[i];
		unsigned int top;

		hc ^= chunk;
		top = (hc >> ((sizeof (unsigned int) * 8) - bitsize));
		hc <<= (bitsize >> 1);
		hc ^= top;
	}
	return hc;
}
/*}}}*/
/*{{{  void ph_insert (int *bsizes, void ***table, void ***keys, int bitsize, void *item, void *key)*/
/*
 *	inserts an item into a pointer-hash
 */
void ph_insert (int *bsizes, void ***table, void ***keys, int bitsize, void *item, void *key)
{
	unsigned int hcode = ph_hashcode (key, bitsize);
	int bucket = hcode & ((1 << bitsize) - 1);

	if (!bsizes[bucket] || !table[bucket] || !keys[bucket]) {
		table[bucket] = (void **)smalloc (sizeof (void *));
		table[bucket][0] = item;

		keys[bucket] = (void **)smalloc (sizeof (void *));
		keys[bucket][0] = key;

		bsizes[bucket] = 1;
	} else {
		table[bucket] = (void **)srealloc ((void *)table[bucket],
					bsizes[bucket] * sizeof(void *),
					(bsizes[bucket] + 1) * sizeof(void *));
		table[bucket][bsizes[bucket]] = item;
		keys[bucket] = (void **)srealloc ((void *)keys[bucket],
					bsizes[bucket] * sizeof(void *),
					(bsizes[bucket] + 1) * sizeof(void *));
		keys[bucket][bsizes[bucket]] = key;

		bsizes[bucket]++;
	}
	return;
}
/*}}}*/
/*{{{  void ph_remove (int *bsizes, void ***table, void ***keys, int bitsize, void *item, void *key)*/
/*
 *	removes an item from a pointer-hash
 */
void ph_remove (int *bsizes, void ***table, void ***keys, int bitsize, void *item, void *key)
{
	unsigned int hcode = ph_hashcode (key, bitsize);
	int bucket = hcode & ((1 << bitsize) - 1);

	if (!bsizes[bucket] || !table[bucket] || !keys[bucket]) {
		return;
	} else {
		int i;

		for (i=0; i<bsizes[bucket]; i++) {
			if ((ph_hashcode (keys[bucket][i], bitsize) == hcode) && (item == table[bucket][i]) && (!strcmp (keys[bucket][i], key))) {
				/* shuffle up others */
				for (; i<(bsizes[bucket] - 1); i++) {
					table[bucket][i] = table[bucket][i+1];
					keys[bucket][i] = keys[bucket][i+1];
				}

				/* nullify the last entries */
				table[bucket][i] = NULL;
				keys[bucket][i] = NULL;

				/* now we have to make smaller */
				if (bsizes[bucket] == 1) {
					sfree (table[bucket]);
					sfree (keys[bucket]);
					table[bucket] = NULL;
					keys[bucket] = NULL;
				} else {
					table[bucket] = (void **)srealloc ((void *)table[bucket], bsizes[bucket] * sizeof (void *),
							(bsizes[bucket] - 1) * sizeof (void *));
					keys[bucket] = (void **)srealloc ((void *)keys[bucket], bsizes[bucket] * sizeof (void *),
							(bsizes[bucket] - 1) * sizeof (void *));
				}
				bsizes[bucket] = bsizes[bucket] - 1;

				return;
			}
		}
	}
	compdyn_fatal ("ph_remove(): item [0x%8.8x:%s] not in pointerhash", (unsigned int)item, (char *)key);
	return;
}
/*}}}*/
/*{{{  void *ph_lookup (int *bsizes, void ***table, void ***keys, int bitsize, void *match)*/
/*
 *	looks up an item in a pointer-hash
 */
void *ph_lookup (int *bsizes, void ***table, void ***keys, int bitsize, void *match)
{
	unsigned int hcode = ph_hashcode (match, bitsize);
	int bucket = hcode & ((1 << bitsize) - 1);

	if (!bsizes[bucket] || !table[bucket] || !keys[bucket]) {
		return NULL;
	} else {
		int i;

		for (i=0; i<bsizes[bucket]; i++) {
			if ((ph_hashcode (keys[bucket][i], bitsize) == hcode) && (keys[bucket][i] == match)) {
#if 0 && defined(DEBUG)
fprintf (stderr, "ph_lookup: match for [%s] found item 0x%8.8x\n", match, (unsigned int)(table[bucket][i]));
#endif
				return table[bucket][i];
			}
		}
	}
	return NULL;
}
/*}}}*/
/*{{{  void ph_dump (FILE *stream, int *bsizes, void ***table, void ***keys, int size)*/
/*
 *	dumps the contents of a pointer-hash (debugging)
 */
void ph_dump (FILE *stream, int *bsizes, void ***table, void ***keys, int size)
{
	int i;

	fprintf (stream, "hash-table size: %d buckets\n", size);
	for (i=0; i < size; i++) {
		fprintf (stream, "bucket %d:\tsize %d:\t", i, bsizes[i]);
		if (bsizes[i]) {
			int j;

			for (j=0; j<bsizes[i]; j++) {
				fprintf (stream, "%s0x%8.8x (0x%8.8x)", (!j ? "" : ", "), (unsigned int)(keys[i][j]), (unsigned int)(table[i][j]));
			}
		}
		fprintf (stream, "\n");
	}
}
/*}}}*/
/*{{{  void ph_walk (int *bsizes, void ***table, void ***keys, int size, void (*func)(void *, void *, void *), void *p)*/
/*
 *	walks the contents of a pointer-hash
 */
void ph_walk (int *bsizes, void ***table, void ***keys, int size, void (*func)(void *, void *, void *), void *p)
{
	int i, j;

	for (i=0; i<size; i++) {
		if (bsizes[i]) {
			for (j=0; j<bsizes[i]; j++) {
				func (table[i][j], keys[i][j], p);
			}
		}
	}
	return;
}
/*}}}*/
/*{{{  void ph_lwalk (int *bsizes, void ***table, void ***keys, int bitsize, void *match, void (*func)(void *, void *, void *), void *p)*/
/*
 *	walks the contents of a pointer-hash for items that match
 */
void ph_lwalk (int *bsizes, void ***table, void ***keys, int bitsize, void *match, void (*func)(void *, void *, void *), void *p)
{
	unsigned int hcode = ph_hashcode (match, bitsize);
	int bucket = hcode & ((1 << bitsize) - 1);

	if (!bsizes[bucket] || !table[bucket] || !keys[bucket]) {
		return;
	} else {
		int i;

		for (i=0; i<bsizes[bucket]; i++) {
			if ((ph_hashcode (keys[bucket][i], bitsize) == hcode) && (!strcmp (keys[bucket][i], match))) {
				func (table[bucket][i], keys[bucket][i], p);
			}
		}
	}
	return;
}
/*}}}*/
/*{{{  void ph_trash (int *bsizes, void ***table, void ***keys, int size)*/
/*
 *	destroys a string-hash
 */
void ph_trash (int *bsizes, void ***table, void ***keys, int size)
{
	int i;

	for (i=0; i<size; i++) {
		if (table[i]) {
			sfree (table[i]);
		}
		if (keys[i]) {
			sfree (keys[i]);
		}
	}

	return;
}
/*}}}*/

