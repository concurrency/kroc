/*
 *	support.c - support functions
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


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "main.h"
#include "support.h"


#ifdef TRACE_MEMORY
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
		fprintf (stderr, "%s: out of memory!\n", progname);
		_exit (1);
	}
	memset (tmp, 0, length);
	#ifdef TRACE_MEMORY
	{
		ss_memblock *tmpblk;

		ss_numalloc++;
		tmpblk = (ss_memblock *)malloc (sizeof (ss_memblock));
		if (!tmpblk) {
			fprintf (stderr, "%s: out of memory!\n", progname);
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
/*{{{  char **split_string (char *str, int copy)*/
/*
 *	splits a string up, returns an array of pointers
 *	if "copy" is non-zero, original string is unaffected -- returned bits are all copies
 *	otherwise original string is munged and returned bits point into it.
 */
char **split_string (char *str, int copy)
{
	char **bits;
	int nbits;
	char *ch, *start;
	int instring = 0;

	/* skip any leading whitespace */
	for (ch=str; (*ch == ' ') || (*ch == '\t'); ch++);
	for (nbits=0; *ch != '\0';) {
		for (; (*ch != '\0') && (instring || ((*ch != ' ') && (*ch != '\t'))); ch++) {
			if (*ch == '\"') {
				instring = !instring;
			}
			/* skip non-whitespace */
		}
		nbits++;
		for (; (*ch == ' ') || (*ch == '\t'); ch++);				/* skip whitespace */
	}
#if 0
fprintf (stderr, "split_string: splitting [%s] into %d bits\n", str, nbits);
#endif
	bits = (char **)smalloc ((nbits + 1) * sizeof (char *));
	bits[nbits] = NULL;

	/* skip any leading whitespace */
	instring = 0;
	for (ch=str; (*ch == ' ') || (*ch == '\t'); ch++);
	for (nbits=0; *ch != '\0';) {
		start = ch;
		for (; (*ch != '\0') && (instring || ((*ch != ' ') && (*ch != '\t'))); ch++) {
			if (*ch == '\"') {
				instring = !instring;
			}
		}
		if (copy) {
			bits[nbits] = string_ndup (start, (int)(ch - start));
			if (*ch != '\0') {
				ch++;
			}
		} else {
			bits[nbits] = start;
			if (*ch != '\0') {
				*ch = '\0';
				ch++;
			}
		}
		nbits++;
		for (; (*ch == ' ') || (*ch == '\t'); ch++);
	}

	return bits;
}
/*}}}*/
/*{{{  char **split_stringsc (char *str, int copy)*/
/*
 *	splits a string up (whitespace and commas considered separators), returns an array of pointers
 *	if "copy" is non-zero, original string is unaffected -- returned bits are all copies
 *	otherwise original string is munged and returned bits point into it.
 */
char **split_stringsc (char *str, int copy)
{
	char **bits;
	int nbits;
	char *ch, *start;
	int instring = 0;

	/* skip any leading whitespace */
	for (ch=str; (*ch == ' ') || (*ch == '\t') || (*ch == ','); ch++);
	for (nbits=0; *ch != '\0';) {
		for (; (*ch != '\0') && (instring || ((*ch != ' ') && (*ch != '\t') && (*ch != ','))); ch++) {
			if (*ch == '\"') {
				instring = !instring;
			}
			/* skip non-whitespace */
		}
		nbits++;
		for (; (*ch == ' ') || (*ch == '\t') || (*ch == ','); ch++);				/* skip whitespace */
	}
#if 0
fprintf (stderr, "split_string: splitting [%s] into %d bits\n", str, nbits);
#endif
	bits = (char **)smalloc ((nbits + 1) * sizeof (char *));
	bits[nbits] = NULL;

	/* skip any leading whitespace */
	instring = 0;
	for (ch=str; (*ch == ' ') || (*ch == '\t') || (*ch == ','); ch++);
	for (nbits=0; *ch != '\0';) {
		start = ch;
		/* for (; (*ch != '\0') && (*ch != ' ') && (*ch != '\t') && (*ch != ','); ch++); */
		for (; (*ch != '\0') && (instring || ((*ch != ' ') && (*ch != '\t') && (*ch != ','))); ch++) {
			if (*ch == '\"') {
				instring = !instring;
			}
		}
		if (copy) {
			bits[nbits] = string_ndup (start, (int)(ch - start));
			if (*ch != '\0') {
				ch++;
			}
		} else {
			bits[nbits] = start;
			if (*ch != '\0') {
				*ch = '\0';
				ch++;
			}
		}
		nbits++;
		for (; (*ch == ' ') || (*ch == '\t') || (*ch == ','); ch++);
	}

	return bits;
}
/*}}}*/
/*{{{  int string_dequote2 (char *str, int *len)*/
/*
 *	removes quotes from a string and puts right escaped characters (escaped with backslash)
 *	modifies the string passed (only ever gets shorter);  if no quotes, will not de-escape characters.
 *	puts resulting string length in 'len' if non-NULL.
 *	returns 0 on success, non-zero on failure
 */
int string_dequote2 (char *str, int *len)
{
	int slen;
	char *ch, *dh;
	int rval = 0;

	if (!str) {
		return -1;
	}
	if (*str != '\"') {
		/* unquoted string -- leave it alone */
		return 0;
	}
	slen = strlen (str);
	if ((slen == 1) || (str[slen-1] != '\"')) {
		/* mangled quotes -- leave it alone */
		return 0;
	}

#if 0
fprintf (stderr, "string_dequote(): on [%s]\n", str);
#endif
	/* copy back and handle escapes */
	for (dh=str, ch=str+1, slen -= 2; slen && (*ch != '\0'); ch++, dh++, slen--) {
		if (*ch == '\\') {
			ch++;
			slen--;
			switch (*ch) {
			default:	/* (assume) simple escaped character */
				*dh = *ch;
				break;
			case 'n':	/* newline character */
				*dh = '\n';
				break;
			case 'r':	/* CR */
				*dh = '\r';
				break;
			case 't':	/* tab */
				*dh = '\t';
				break;
			case 'x':	/* hexidecimal pair */
				if (decode_hex_byte (ch[1], ch[2], (unsigned char *)dh)) {
					/* busted hex format, but linger on (and keep the hex chars) */
					rval = -1;
					*dh = 'x';
				} else {
					ch += 2;
					slen -= 2;
				}
				break;
			}
		} else {
			*dh = *ch;
		}
	}
	*dh = '\0';
#if 0
fprintf (stderr, "string_dequote(): output --> [%s]\n", str);
#endif

	if (len) {
		*len = (int)(dh - str);
	}

	return rval;
}
/*}}}*/
/*{{{  int string_dequote (char *str)*/
/*
 *	removes quotes from a string and puts right escaped characters (escaped with backslash)
 *	modifies the string passed (only ever gets shorter);  if no quotes, will not de-escape characters.
 *	returns 0 on success, non-zero on failure
 */
int string_dequote (char *str)
{
	return string_dequote2 (str, NULL);
}
/*}}}*/


