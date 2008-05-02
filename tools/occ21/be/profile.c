
/*
 *	Code to manage the profiler
 *	Copyright (C) 1994 Inmos Limited
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
#  include <config.h>
#endif
#include <stdlib.h>
#include <string.h>
#include "includes.h"
#include "profile.h"
#include "predefhd.h"


/*{{{  definitions */
#define PROFTAB_INITIAL_STRPOOL_SIZE   512
#define PROFTAB_STRPOOL_SIZE_INCREMENT 512
/*}}}*/
/*{{{  Global variables */
ProfTab *profile_table;
INT32 profile_count_table_label;
/*}}}*/

/*{{{  static StrPoolP strpool_create(StrPoolI initial_image_allocated_size, etc. */
static StrPoolP
strpool_create (StrPoolI initial_image_allocated_size,
		StrPoolI amount_to_increase_image_allocation_by,
		void *(*alloc_fn) (size_t size, void *user_data), void (*free_fn) (void *block, size_t size, void *user_data), void *user_data)
{
	StrPoolP result;
	result = alloc_fn (sizeof (struct StrPool), user_data);
	if (result != NULL) {
		if (initial_image_allocated_size == 0)
			initial_image_allocated_size = 1;
		result->image = alloc_fn ((size_t) initial_image_allocated_size, user_data);
		if (result->image != NULL) {
			result->image_len = 1;
			result->image[0] = '\0';
			result->image_allocated_size = initial_image_allocated_size;
			result->amount_to_increase_image_allocation_by = amount_to_increase_image_allocation_by;
			result->alloc_fn = alloc_fn;
			result->free_fn = free_fn;
		} else {
			free_fn (result, sizeof (struct StrPool), user_data);
			result = NULL;
		}
	}
	return result;
}

/*}}}*/

/*{{{  static  const char *strpool_lookup(StrPoolP pool, const char *string) */
static const char *
strpool_lookup (StrPoolP pool, const char *string)
{
	const size_t len = (size_t) pool->image_len;
	const char *start = pool->image;
	const char *match;

	for (match = memchr (start, string[0], len); match != NULL; match = memchr (match + 1, string[0], len - (match - start)))
		if (strcmp (match, string) == 0)
			return match;

	return NULL;
}

/*}}}*/

#if 0
/* NOT USED */
/*{{{  static const char *strpool_entry(StrPoolP pool, StrPoolI index) */
static const char *strpool_entry (StrPoolP pool, StrPoolI index)
{
	return &pool->image[index];
}
#endif

/*}}}*/

/*{{{  StrPoolI strpool_add(StrPoolP pool, const char *string) */
StrPoolI strpool_add (StrPoolP pool, const char *string)
{
	const char *match = strpool_lookup (pool, string);
	if (match == NULL) {
		const size_t stringlen = strlen (string) + 1;
		const size_t required_size = (size_t) pool->image_len + stringlen;
		const StrPoolI posn = pool->image_len;
		if (required_size > pool->image_allocated_size) {
			/* Extend the image */
			const size_t extra_allocation_needed = required_size - (size_t) pool->image_allocated_size;
			const size_t chunks_to_allocate =
				(extra_allocation_needed /
				 (size_t) pool->amount_to_increase_image_allocation_by) +
				((extra_allocation_needed % (size_t) pool->amount_to_increase_image_allocation_by) != 0);
			const size_t amount_to_increase_by = chunks_to_allocate * (size_t) pool->amount_to_increase_image_allocation_by;
			const char *old_image = pool->image;
			const size_t new_allocated_size = (size_t) pool->image_allocated_size + amount_to_increase_by;
			pool->image = pool->alloc_fn (new_allocated_size, pool->user_data);
			memcpy (pool->image, old_image, (size_t) pool->image_len);
			pool->free_fn (pool->image, (size_t) pool->image_allocated_size, pool->user_data);
			pool->image_allocated_size = (StrPoolI) new_allocated_size;
		}

		memcpy (&pool->image[posn], string, stringlen);
		pool->image_len += stringlen;
		return posn;
	} else
		return (StrPoolI) (match - pool->image);
}

/*}}}*/

#if 0
/* NOT USED */
/*{{{  static  void strpool_discard(StrPoolP *pool) */
static void strpool_discard (StrPoolP * pool)
{
	(*pool)->free_fn ((*pool)->image, (size_t) ((*pool)->image_allocated_size), (*pool)->user_data);
	(*pool)->free_fn (*pool, sizeof (struct StrPool), (*pool)->user_data);
	*pool = NULL;
}
#endif

/*}}}*/

/*{{{  StrPoolI strpool_size(StrPoolP pool) */
StrPoolI strpool_size (StrPoolP pool)
{
	return pool->image_len;
}

/*}}}*/

/*{{{  const char *strpool_image(StrPoolP pool) */
const char *strpool_image (StrPoolP pool)
{
	return (const char *) (pool->image);
}

/*}}}*/
/*{{{  Profile table manipulation */
/*{{{ BOOL valid_proftable_index(ProfTab *table, INT32 index) */
BOOL valid_proftable_index (ProfTab * table, INT32 index)
{
	ProfTabEntry *pentry = (table->entry_list_head) + index;
	return (index < table->count_table_size && index >= 0 && proftab_entry_number_ (pentry) == index);
}

/*}}}*/
/*{{{  static ProfTabEntry *add_proftab_entry(ProfTab * table, ProfTabEntryTag tag) */
static ProfTabEntry *add_proftab_entry (ProfTab * table, ProfTabEntryTag tag, treenode * tptr)
{
	ProfTabEntry *new_entry = (ProfTabEntry *) table->alloc_fn (sizeof (ProfTabEntry), NULL);
	proftab_next_ (new_entry) = NULL;
	if (table->entry_list_head == NULL)
		table->entry_list_head = new_entry;
	else
		table->entry_list_tail->next = new_entry;
	table->entry_list_tail = new_entry;
	proftab_tag_ (new_entry) = tag;
	proftab_tptr_ (new_entry) = tptr;
	proftab_entry_number_ (new_entry) = table->count_table_size;
	table->count_table_size++;
	return new_entry;
}

/*}}}*/

/*{{{  static ProfTabEntry *proftab_add_sourcepos_count(ProfTab * table, */
static ProfTabEntry *proftab_add_sourcepos_count (ProfTab * table, ProfTabEntryTag tag, const char *name, INT32 line, treenode * tptr)
{
	ProfTabEntry *new_entry = add_proftab_entry (table, tag, tptr);
	const StrPoolI file_name = (name == NULL) ? 0 : strpool_add (table->strpool, name);
	proftab_file_name_ (new_entry) = file_name;
	proftab_line_number_ (new_entry) = line;
	return new_entry;
}

/*}}}*/

/*{{{  ProfTabEntry *proftab_add_line_count(ProfTab * table, etc. */
ProfTabEntry *proftab_add_line_count (ProfTab *table, const char *name, INT32 line, treenode *tptr)
{
	return proftab_add_sourcepos_count (table, ProfLineCount, name, line, tptr);
}

/*}}}*/

/*{{{  ProfTabEntry *proftab_add_routine_count(ProfTab * table, etc. */
ProfTabEntry *proftab_add_routine_count (ProfTab *table, treenode *tptr)
{
	ProfTabEntry *new_entry = add_proftab_entry (table, ProfRoutineCount, tptr);
	return new_entry;
}

/*}}}*/

/*{{{  ProfTabEntry *proftab_add_call_count(ProfTab * table, etc. */
ProfTabEntry *proftab_add_call_count (ProfTab *table, treenode *tptr)
{
	ProfTabEntry *new_entry = add_proftab_entry (table, ProfCallCount, tptr);
	return new_entry;
}

/*}}}*/

/*{{{  ProfTab * proftab_create(BIT32 version, etc. */
ProfTab *proftab_create (ProfTabContents contents,
		void *alloc_fn (size_t size, void *user_data), void free_fn (void *block, size_t size, void *user_data), void *user_data)
{
	ProfTab *table = (ProfTab *) alloc_fn (sizeof (ProfTab), user_data);
	table->alloc_fn = alloc_fn;
	table->free_fn = free_fn;
	table->user_data = user_data;
	table->contents = contents;
	table->count_table_size = 0;
	table->strpool = strpool_create (PROFTAB_INITIAL_STRPOOL_SIZE, PROFTAB_STRPOOL_SIZE_INCREMENT, alloc_fn, free_fn, user_data);
	table->entry_list_head = NULL;
	table->entry_list_tail = NULL;
	if (cgraph_profiling || sampling_profiling) {
		int i;
		RoutineInfoEntry **hash_table;
		table->rout_hash_table = memalloc (ROUTINE_INFO_HASH_TABLE_SIZE * sizeof (table->rout_hash_table));
		hash_table = table->rout_hash_table;

		for (i = 0; i < ROUTINE_INFO_HASH_TABLE_SIZE; i++)
			hash_table[i] = NULL;
	} else
		table->rout_hash_table = NULL;
	return table;
}

/*}}}*/
/*{{{  hash_function(address) macro */
/******************************************************************************
*
*  hash_function(address)
*  address = address of a tree node.
*  The hashing algorithm is performed on the address of a tree node. The
*  tree node pointer is cast into a BIT32 type and the following actions are
*  taken. The address word is shifted two bits right thereby removing the two
*  least significant bits. The resulting address word is then ANDed with 255
*  to produce an eight bit number which is used to index the hash table.
*  Andy Whitlow 19.10.88
*
*  Modified to shift by 5 instead; 9/10/90 CON
*
******************************************************************************/
#define HASH_MASK 255
#define hash_function(address) (int)((((unsigned long)(address)) >> 5) & HASH_MASK)
/*}}}*/
/*{{{  void set_proftab_routine_address(ProfTab *table, treenode *nptr, ...) */
void set_proftab_routine_address (ProfTab *table, treenode *nptr, INT32 address)
{
	RoutineInfoEntry *rptr;
	RoutineInfoEntry **hash_table = table->rout_hash_table;
	const int hash_value = hash_function (nptr);
	/* If that address is already there, ignore it */
	for (rptr = hash_table[hash_value]; rptr != NULL; rptr = rptr->next)
		if (rptr->nptr == nptr)
			return;

	rptr = (RoutineInfoEntry *) table->alloc_fn (sizeof (*rptr), NULL);
	rptr->next = hash_table[hash_value];
	hash_table[hash_value] = rptr;
	rptr->nptr = nptr;
	rptr->address = address;
}

/*}}}*/
/*{{{  void get_proftab_routine_address(ProfTab *table, treenode *nptr) */
INT32 get_proftab_routine_address (ProfTab *table, treenode *nptr)
{
	RoutineInfoEntry *rptr;
	RoutineInfoEntry **hash_table = table->rout_hash_table;
	const int hash_value = hash_function (nptr);
	for (rptr = hash_table[hash_value]; rptr != NULL; rptr = rptr->next)
		if (rptr->nptr == nptr)
			return (rptr->address);
#if 0
	msg_out_s (SEV_INTERNAL, GEN, GEN_INDEX_HASH_TABLE_ERR, LocnOf (nptr), itagstring (TagOf (address)));
#endif
	return (0);		/* not reached */
}

/*}}}*/
/*{{{  treenode *add_profcountupd_nd(int count, treenode *tptr) */
treenode *add_profcountupd_nd (ProfTabEntry *profcount, treenode *tptr)
{
	#if 0
	const SOURCEPOSN locn = LocnOf (tptr);
	treenode *argls = addtofront (newintconstant (profcount->entry_number, S_INT),
				      NULL);
	treenode *procnamend = get_predefname (PD_UPDATE_PROFCOUNT);
	return newcnode (S_SEQ, locn, newlistnode (S_LIST, locn,
						   newinstancenode (S_PINSTANCE, locn, procnamend, argls), newlistnode (S_LIST, locn, tptr, NULL)));
	#else
	return NULL;
	#endif
}
/*}}}*/
/*{{{  ProfTabEntry *get_proftab_entry(INT32 index) */
ProfTabEntry *get_proftab_entry (ProfTab *prof_table, treenode *tptr)
{
	ProfTabEntryTag typeofentry;
	ProfTabEntry *pt;
	if (nodetypeoftag (TagOf (tptr)) == NAMENODE)
		typeofentry = ProfRoutineCount;
	else if (nodetypeoftag (TagOf (tptr)) == INSTANCENODE)
		typeofentry = ProfCallCount;
	else
		typeofentry = ProfLineCount;
	for (pt = prof_table->entry_list_head; pt != NULL; pt = proftab_next_ (pt))
		if (proftab_tag_ (pt) == typeofentry && proftab_tptr_ (pt) == tptr)
			return pt;
#if 0
	if (pt == NULL) {
		printtree (outfile, 0, tptr);
		fputc ('\n', outfile);
	}
#endif
	return NULL;
}

/*}}}*/
/*{{{  prof_memalloc,  prof_memfree */
/* prof_memalloc and  prof_memfree are wrappers to memalloc/memfree to allow
   them to be passed as allocation functions to library routines. */

void *profi_memalloc (size_t s, void *data)
{
	IGNORE (data);
	return memalloc (s);
}

void profi_memfree (void *p, size_t s, void *data)
{
	IGNORE (s);
	IGNORE (data);
	memfree (p);
}

/*}}}*/
