/*
 *	Occam two compiler string pool (name table handling)
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

/*#define DEBUG*/

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <string.h>
#include "midinc.h"
/*}}}  */

#define HASHSIZE 511		/* 257 */

/* Note that these are initialised to all NULL because they are static
   variables */
PRIVATE wordnode *nametable[HASHSIZE];

/*{{{  PUBLIC wordnode *lookupword(c, len) */
/* Search for the name at *c in the symbol table. Return the node if found,
otherwise create a node and return that */
PUBLIC wordnode *lookupword (const char *const string, const int len)
{
	register wordnode *wptr;
	register unsigned int hashval;

	/*{{{  calculate hash value */
#if 1
	{
		/* Must be unsigned to work on a SPARC: */
		register const unsigned char *const c = (const unsigned char *) string;

		if (len > 4)

			/* Most common case */
			/* modified 15/10/90 to look at the 'middle' char, and to include the
			   length, also the third from last char - CON */
			/* Changed again to look at the first 4, the last 4, and a middle one */
			hashval = c[0] + c[1] + c[2] + c[3] + c[len >> 1] + c[len - 4] + c[len - 3] + c[len - 2] + c[len - 1] + len;
		else if (len == 1)
			hashval = c[0];
		else if (len == 2)
			hashval = c[0] + c[1];
		else if (len == 3)
			hashval = c[0] + c[1] + c[2];
		else
			hashval = c[0] + c[1] + c[2] + c[3];
		hashval %= HASHSIZE;
	}
#else
	/* This is what is used in the INMOS C compiler */
	/* It looks a bit slow to me! (CON 18/10/90) */
	{
		int i;
		char *s;
		hashval = 1;
		for (i = 0, s = string; i < len; i++, s++) {
			const register unsigned int temp = (hashval << 7);
			hashval = ((hashval >> 25) ^ (temp >> 1) ^ (temp >> 4) ^ *s) & 0x7fffffff;
		}
		hashval %= HASHSIZE;
	}
#endif
	/*}}}  */
	/*{{{  search for name and if found, return */
	for (wptr = nametable[hashval]; wptr != NULL; wptr = WNextOf (wptr))
		if ((WLengthOf (wptr) == len) && (memcmp (string, WNameOf (wptr), len) == 0))
			return wptr;
	/*}}}  */
	/*{{{  create a new node and return it */
	{
		/* The node must be 'permanently' available, so we ensure that we're
		   not in `temporary' workspace
		   CON 11/01/93
		 */
		const int old = switch_to_real_workspace ();
		char *const nameptr = newvec (len + 1);
		/*{{{  copy string into wordnode */
		memcpy (nameptr, string, len);
		nameptr[len] = '\0';
		/*}}}  */
		wptr = newwordnode (S_NAME, nameptr, len, nametable[hashval]);
		switch_to_prev_workspace (old);
		nametable[hashval] = wptr;
		return (wptr);
	}
	/*}}}  */
}

/*}}}  */

#ifdef USER_DEFINED_OPERATORS
/*{{{  PUBLIC BOOL searchforword(c, len) */
/* Search for the name at *c in the symbol table. Return TRUE if found,
otherwise return FALSE, added by Jim to implement overloaded operators */
PUBLIC BOOL searchforword (const char *const string, const int len)
{
	register wordnode *wptr;
	register unsigned int hashval;

	/*{{{  calculate hash value */
#if 1
	{
		/* Must be unsigned to work on a SPARC: */
		register const unsigned char *const c = (const unsigned char *) string;

		if (len > 4)

			/* Most common case */
			/* modified 15/10/90 to look at the 'middle' char, and to include the
			   length, also the third from last char - CON */
			/* Changed again to look at the first 4, the last 4, and a middle one */
			hashval = c[0] + c[1] + c[2] + c[3] + c[len >> 1] + c[len - 4] + c[len - 3] + c[len - 2] + c[len - 1] + len;
		else if (len == 1)
			hashval = c[0];
		else if (len == 2)
			hashval = c[0] + c[1];
		else if (len == 3)
			hashval = c[0] + c[1] + c[2];
		else
			hashval = c[0] + c[1] + c[2] + c[3];
		hashval %= HASHSIZE;
	}
#else
	/* This is what is used in the INMOS C compiler */
	/* It looks a bit slow to me! (CON 18/10/90) */
	{
		int i;
		char *s;
		hashval = 1;
		for (i = 0, s = string; i < len; i++, s++) {
			const register unsigned int temp = (hashval << 7);
			hashval = ((hashval >> 25) ^ (temp >> 1) ^ (temp >> 4) ^ *s) & 0x7fffffff;
		}
		hashval %= HASHSIZE;
	}
#endif
	/*}}}  */
	/*{{{  search for name and if found, return */
	for (wptr = nametable[hashval]; wptr != NULL; wptr = WNextOf (wptr))
		if ((WLengthOf (wptr) == len) && (memcmp (string, WNameOf (wptr), len) == 0))
			return TRUE;
	return FALSE;
	/*}}}  */
}

/*}}}  */
#endif
/*{{{  PUBLIC void clear_keywords */
PUBLIC void clear_keywords (void)
/* This resets the tags on all strings in the string pool, so that
   none are accidentally thought to be keywords.
*/
{
	int i;
	for (i = 0; i < HASHSIZE; i++) {
		wordnode *wptr;
		for (wptr = nametable[i]; wptr != NULL; wptr = WNextOf (wptr))
			SetWTag (wptr, S_NAME);
	}
}

/*}}}  */
