/*
 *	support.c - support functions
 *	Copyright (C) 2000-2004 Fred Barnes <frmb@kent.ac.uk>
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

/*{{{  includes, etc.*/
#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include <sys/time.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "support.h"
#include "global.h"


/*}}}*/

#ifdef TRACE_MEMORY
/*{{{  memory-trace related stuff*/

#define SS_FILE_SIZE 64

typedef struct TAG_ss_memblock {
	struct TAG_ss_memblock *next, *prev;
	void *ptr;
	unsigned int vptr;
	size_t size;
	int line;
	char file[SS_FILE_SIZE];
} ss_memblock;

static ss_memblock *ss_head = NULL;
static ss_memblock *ss_tail = NULL;
static int ss_numalloc = 0;
static int ss_numfree = 0;
static int ss_numrealloc = 0;
static int ss_numhardrealloc = 0;


/*{{{  void ss_cleanup (void)*/
/*
 *	called on program exit to note what didn't get cleaned up
 */
void ss_cleanup (void)
{
	ss_memblock *tmpblk;

	fprintf (stderr, "%d blocks allocated\n", ss_numalloc);
	fprintf (stderr, "%d blocks freed\n", ss_numfree);
	fprintf (stderr, "%d blocks re-allocated\n", ss_numrealloc);
	fprintf (stderr, "%d blocks coppied for re-allocation\n", ss_numhardrealloc);
	fprintf (stderr, "left-over memory blocks:\n");
	for (tmpblk = ss_head; tmpblk; tmpblk = tmpblk->next) {
		fprintf (stderr, "0x%-8x  %-8d  %s:%d\n", tmpblk->vptr, tmpblk->size, tmpblk->file, tmpblk->line);
	}
	return;
}
/*}}}*/
/*{{{  void ss_insert_blk (ss_memblock *blk)*/
/*
 *	inserts a memory block in the list
 */
void ss_insert_blk (ss_memblock *blk)
{
	ss_memblock *tb;

	if (!ss_head && !ss_tail) {
		ss_head = ss_tail = blk;
	} else if (!ss_head || !ss_tail) {
		fprintf (stderr, "%s: fatal: ss_head = %p, ss_tail = %p\n", progname, ss_head, ss_tail);
		_exit (1);
	} else {
		for (tb=ss_head; tb && (tb->vptr < blk->vptr); tb = tb->next);
		if (!tb) {
			/* insert at end */
			blk->prev = ss_tail;
			ss_tail->next = blk;
			ss_tail = blk;
		} else if (!tb->prev) {
			/* insert at start */
			blk->next = ss_head;
			ss_head->prev = blk;
			ss_head = blk;
		} else {
			/* insert before `tb' */
			blk->prev = tb->prev;
			blk->next = tb;
			tb->prev->next = blk;
			tb->prev = blk;
		}
	}
	return;
}
/*}}}*/
/*{{{  void ss_remove_blk (ss_memblock *blk)*/
/*
 *	removes (disconnects) a memory block from the list
 */
void ss_remove_blk (ss_memblock *blk)
{
	if (blk->prev && blk->next) {
		blk->prev->next = blk->next;
		blk->next->prev = blk->prev;
	} else if (!blk->prev) {
		ss_head = blk->next;
		ss_head->prev = NULL;
	} else if (!blk->next) {
		ss_tail = blk->prev;
		ss_tail->next = NULL;
	} else if (!blk->prev && !blk->next) {
		ss_head = ss_tail = NULL;
	}
	return;
}
/*}}}*/
/*}}}*/
#endif	/* TRACE_MEMORY */

/*{{{  void *smalloc (size_t length)*/
/*
 *	allocates some memory
 */
#ifdef TRACE_MEMORY
void *ss_malloc (char *file, int line, size_t length)
#else
void *smalloc (size_t length)
#endif
{
	void *tmp;

	tmp = malloc (length);
	if (!tmp) {
		mkoccdeps_fatal ("out of memory! (%d bytes)\n", length);
		_exit (1);
	}
	memset (tmp, 0, length);
	#ifdef TRACE_MEMORY
	{
		ss_memblock *tmpblk;

		ss_numalloc++;
		tmpblk = (ss_memblock *)malloc (sizeof (ss_memblock));
		if (!tmpblk) {
			mkoccdeps_fatal ("out of memory! (%d bytes)\n", length);
			_exit (1);
		}
		tmpblk->prev = tmpblk->next = NULL;
		tmpblk->ptr = tmp;
		tmpblk->vptr = (unsigned int)tmp;
		tmpblk->size = length;
		strncpy (tmpblk->file, file, SS_FILE_SIZE);
		tmpblk->line = line;
		ss_insert_blk (tmpblk);
	}
	#endif	/* TRACE_MEMORY */
	return tmp;
}
/*}}}*/
/*{{{  void *srealloc (void *ptr, size_t old_size, size_t new_size)*/
/*
 *	re-allocates a memory block, moving it entirely if necessary
 */
#ifdef TRACE_MEMORY
void *ss_realloc (char *file, int line, void *ptr, size_t old_size, size_t new_size)
#else
void *srealloc (void *ptr, size_t old_size, size_t new_size)
#endif
{
	void *tmp;

	if (!ptr || !old_size) {
		tmp = smalloc (new_size);
	} else {
		tmp = realloc (ptr, new_size);
		if (!tmp) {
			tmp = smalloc (new_size);
			if (new_size > old_size) {
				memcpy (tmp, ptr, old_size);
			} else {
				memcpy (tmp, ptr, new_size);
			}
			sfree (ptr);
			#ifdef TRACE_MEMORY
				ss_numhardrealloc++;
			#endif	/* TRACE_MEMORY */
		} else {
			#ifdef TRACE_MEMORY
				ss_memblock *tmpblk;

				/* relocate block */
				ss_numrealloc++;
				for (tmpblk=ss_head; tmpblk; tmpblk = tmpblk->next) {
					if (tmpblk->ptr == ptr) {
						break;
					}
				}
				if (!tmpblk) {
					fprintf (stderr, "%s: serious: attempt to srealloc() non-allocated memory (%p) in %s:%d\n", progname, ptr, file, line);
				} else {
					ss_remove_blk (tmpblk);
					if (tmpblk->size != old_size) {
						fprintf (stderr, "%s: serious: inconsistency in old_size (specified %d, allocated %d) (%p) in %s:%d\n", progname, old_size, tmpblk->size, ptr, file, line);
						fprintf (stderr, "%s: serious: memory was allocated in %s:%d\n", progname, tmpblk->file, tmpblk->line);
					}
					tmpblk->prev = tmpblk->next = NULL;
					tmpblk->ptr = tmp;
					tmpblk->vptr = (unsigned int)tmp;
					tmpblk->size = new_size;
					ss_insert_blk (tmpblk);
				}
			#endif	/* TRACE_MEMORY */
		}
		if (new_size > old_size) {
			memset (tmp + old_size, 0, new_size - old_size);
		}
	}
	return tmp;
}
/*}}}*/
/*{{{  void sfree (void *ptr)*/
/*
 *	frees previously allocated memory
 */
#ifdef TRACE_MEMORY
void ss_free (char *file, int line, void *ptr)
#else
void sfree (void *ptr)
#endif
{
	if (ptr) {
		#ifdef TRACE_MEMORY
			ss_memblock *tmpblk;

			ss_numfree++;
			for (tmpblk = ss_head; tmpblk; tmpblk = tmpblk->next) {
				if (tmpblk->ptr == ptr) {
					break;
				}
			}
			if (!tmpblk) {
				fprintf (stderr, "%s: serious: attempt to sfree() non-allocated memory (%p) in %s:%d\n", progname, ptr, file, line);
			} else {
				ss_remove_blk (tmpblk);
				free (tmpblk);
			}
		#endif
		free (ptr);
	}
	return;
}
/*}}}*/

/*{{{  char *string_ndup (char *str, int length)*/
/*
 *	duplicates a chunk of string
 */
char *string_ndup (char *str, int length)
{
	char *tmp;

	tmp = (char *)smalloc (length + 1);
	memcpy (tmp, str, length);
	tmp[length] = '\0';
	return tmp;
}
/*}}}*/
/*{{{  char *string_dup (char *str)*/
/*
 *	duplicates a string
 */
char *string_dup (char *str)
{
	return string_ndup (str, strlen (str));
}
/*}}}*/
/*{{{  void *mem_ndup (void *ptr, int length)*/
/*
 *	duplicates a bit of memory
 */
void *mem_ndup (void *ptr, int length)
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
		fprintf (stderr, "%s: fatal: item at index %d in array at %p (%d,%d) does not exist!\n", progname, idx, *array, *cur, *max);
		exit (EXIT_FAILURE);
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
/*{{{  int decode_hex_byte (char b1, char b2, unsigned char *tptr)*/
/*
 *	turns 2 hex characters into an unsigned byte.  returns 0 on success, -1 on error
 */
int decode_hex_byte (char b1, char b2, unsigned char *tptr)
{
	*tptr = 0;
	if ((b1 >= '0') && (b1 <= '9')) {
		*tptr = ((b1 - '0') << 4);
	} else if ((b1 >= 'a') && (b1 <= 'f')) {
		*tptr = (((b1 - 'a') + 10) << 4);
	} else if ((b1 >= 'A') && (b1 <= 'F')) {
		*tptr = (((b1 - 'A') + 10) << 4);
	} else {
		return -1;
	}
	if ((b2 >= '0') && (b2 <= '9')) {
		*tptr |= ((b2 - '0') & 0x0f);
	} else if ((b2 >= 'a') && (b2 <= 'f')) {
		*tptr |= (((b2 - 'a') + 10) & 0x0f);
	} else if ((b2 >= 'A') && (b2 <= 'F')) {
		*tptr |= (((b2 - 'A') + 10) & 0x0f);
	} else {
		return -1;
	}
	return 0;
}
/*}}}*/
/*{{{  int parse_uint16hex (char *ch)*/
/*
 *	parses 4 hex digits to form an unsigned 16-bit number.  returns in a 32-bit word
 */
int parse_uint16hex (char *ch)
{
	int w = 0;
	unsigned char v;

	if (decode_hex_byte (ch[0], ch[1], &v)) {
		return 0;
	}
	w |= v;
	w <<= 8;
	if (decode_hex_byte (ch[2], ch[3], &v)) {
		return 0;
	}
	w |= v;
	return w;
}
/*}}}*/
/*{{{  char *mkhexbuf (unsigned char *buffer, int buflen)*/
/*
 *	turns a byte buffer into a nice hex string
 */
char *mkhexbuf (unsigned char *buffer, int buflen)
{
	static char *hexstr = "0123456789abcdef";
	char *str = (char *)smalloc ((buflen << 1) + 1);
	int i;

	for (i=0; i<buflen; i++) {
		str[i << 1] = hexstr [(int)(buffer[i] >> 4)];
		str[(i << 1) + 1] = hexstr [(int)(buffer[i] & 0x0f)];
	}
	str[i << 1] = '\0';
	return str;
}
/*}}}*/
/*{{{  char **split_string (char *str)*/
/*
 *	splits a string up, returns an array of pointers into the original string
 */
char **split_string (char *str)
{
	char **bits;
	int nbits;
	char *ch, *start;

	/* skip any leading whitespace */
	for (ch=str; (*ch == ' ') || (*ch == '\t'););
	for (nbits=0; *ch != '\0'; ch++) {
		for (; (*ch != '\0') && (*ch != ' ') && (*ch != '\t'); ch++);
		nbits++;
		for (; (*ch == ' ') || (*ch == '\t'); ch++);
	}
	bits = (char **)smalloc ((nbits + 1) * sizeof (char *));
	bits[nbits] = NULL;

	/* skip any leading whitespace */
	for (ch=str; (*ch == ' ') || (*ch == '\t'); ch++);
	for (nbits=0; *ch != '\0';) {
		start = ch;
		for (; (*ch != '\0') && (*ch != ' ') && (*ch != '\t'); ch++);
		bits[nbits] = string_ndup (start, (int)(ch - start));
		if (*ch != '\0') {
			*ch = '\0';
			ch++;
		}
		nbits++;
		for (; (*ch == ' ') || (*ch == '\t'); ch++);
	}

	return bits;
}
/*}}}*/
/*{{{  char *decode_hexstr (char *str, int *slen)*/
/*
 *	this turns a string of HEX values into a regular string (undoes "mkhexbuf")
 *	returns NULL on error.  stores the resulting string length in `*slen'
 */
char *decode_hexstr (char *str, int *slen)
{
	int len, i;
	char *newstr;

	*slen = 0;
	if (!str) {
		return NULL;
	}
	len = strlen (str);
	if (len & 0x01) {
		return NULL;
	}
	*slen = (len >> 1);
	newstr = (char *)smalloc (*slen + 1);
	newstr[*slen] = '\0';
	for (i=0; i<*slen; i++) {
		if (decode_hex_byte (str[(i << 1)], str[(i << 1) + 1], (unsigned char *)newstr + i) < 0) {
			sfree (newstr);
			*slen = 0;
			return NULL;
		}
	}
	return newstr;
}
/*}}}*/
/*{{{  int time_after (struct timeval *t1, struct timeval *t2)*/
/*
 *	determines whether t1 is after t2
 */
int time_after (struct timeval *t1, struct timeval *t2)
{
	return ((t1->tv_sec > t2->tv_sec) || ((t1->tv_sec == t2->tv_sec) && (t1->tv_usec > t2->tv_usec)));
}
/*}}}*/
/*{{{  void time_minus (struct timeval *t1, struct timeval *t2, struct timeval *t3)*/
/*
 *	subtracts t2 from t1 and places the resulting time in t3
 */
void time_minus (struct timeval *t1, struct timeval *t2, struct timeval *t3)
{
	t3->tv_sec = t1->tv_sec - t2->tv_sec;
	t3->tv_usec = t1->tv_usec - t2->tv_usec;
	while (t3->tv_usec < 0) {
		t3->tv_usec += 1000000;
		t3->tv_sec--;
	}
	return;
}
/*}}}*/
/*{{{  void time_setin (struct timeval *t1, struct timeval *t2)*/
/*
 *	adds t2 to the current time and places the result in t1
 */
void time_setin (struct timeval *t1, struct timeval *t2)
{
	struct timeval now;

	gettimeofday (&now, NULL);
	t1->tv_sec = now.tv_sec + t2->tv_sec;
	t1->tv_usec = now.tv_usec + t2->tv_usec;
	while (t1->tv_usec > 1000000) {
		t1->tv_sec++;
		t1->tv_usec -= 1000000;
	}
	return;
}
/*}}}*/


