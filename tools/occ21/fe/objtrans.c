/* $Id: objtrans.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	occam two object file symbol translation
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
#include "feinc.h"

#include "objlib.h"
#include "objtrans.h"

#include "synerror.h"

#include "chkdef.h"		/* for current_fe_handle */
/*}}}*/

/*{{{  #PRAGMA Translate support structures */
typedef struct translate_s	/* used for name translation (#PRAGMA TRANSLATE) */
{
	struct translate_s *n_next;	/* next on chain */
	wordnode *n_name;
	struct translate_s *n_pair;	/* translation name */
}
translate_t;

/* One copy of this structure is hung off the current_fe_handle,
   for `global' information for this parse. */
struct fe_translate_data_s
{
	translate_t *trans_internals;	/* Name pairs for name */
	translate_t *trans_externals;	/* translations */

	/* These are kept simply to allow error checking on #PRAGMA translate */
	libentry_t *globalnames;	/* Names already #USE-d */
	libentry_t *thrownentries;	/* Those names not in occam syntax */
};

/*}}}*/

/*{{{  PRIVATE search_translations */
PRIVATE const translate_t *search_translations (const translate_t * ptr, wordnode * const name);
#ifdef _ICC			/* bug INSdi01953 05/04/93 */
#pragma IMS_nosideeffects(search_translations)
#endif
PRIVATE const translate_t *
search_translations (const translate_t * ptr, wordnode * const name)
{
	DEBUG_MSG (("search_translations: name is \"%s\"\n", WNameOf (name)));
	while ((ptr != NULL) && (name != ptr->n_name)) {
		ptr = ptr->n_next;
	}
	return (ptr);
}

/*}}}*/
/*{{{  PRIVATE lookup_translations */
PRIVATE wordnode *lookup_translations (const translate_t * const from, wordnode * const name);
#ifdef _ICC			/* bug INSdi01953 05/04/93 */
#pragma IMS_nosideeffects(lookup_translations)
#endif
PRIVATE wordnode *
lookup_translations (const translate_t * const from, wordnode * const name)
{
	const translate_t *const ptr = search_translations (from, name);
	return (ptr == NULL) ? name : ptr->n_pair->n_name;
}

/*}}}*/

/*{{{  PUBLIC local_translate_from_internal */
PUBLIC wordnode *
local_translate_from_internal (const fe_translate_data_t * const ptr, wordnode * const name)
{
	return lookup_translations (ptr->trans_internals, name);
}

/*}}}*/

/*{{{  PUBLIC translate_descriptor */
PUBLIC char *translate_descriptor (wordnode ** name, const int index, char *descriptor, const int len)
/* This takes a descriptor string, with name starting at the index'th
   character.
   If the name requires translating, it creates a new descriptor with
   a different name inserted.
   The old descriptor is freed up.
*/
{
	const fe_translate_data_t *const ptr = fe_translates (current_fe_handle);
	wordnode *const old_name = *name;
	wordnode *local = lookup_translations (ptr->trans_externals, old_name);
	if (local != *name) {	/* we've got to translate the name */
		char *str = memalloc (len + (WLengthOf (local) - WLengthOf (old_name)) + 1);
		DEBUG_MSG (("Translating input \"%s\" to \"%s\"\n", WNameOf (old_name), WNameOf (local)));
		strncpy (str, descriptor, index);
		str[index] = '\0';
		strcat (str, WNameOf (local));
		strcat (str, descriptor + (index + WLengthOf (old_name)));
		memfree (descriptor);
		descriptor = str;
		*name = local;
	}
	return descriptor;
}

/*}}}*/
/*{{{  PUBLIC throwawayname */
PUBLIC void
throwawayname (wordnode * name)
{
	fe_translate_data_t *const ptr = fe_translates (current_fe_handle);
	if (search_libentries (ptr->thrownentries, name) == NULL) {
		libentry_t *libentry = (libentry_t *) newvec (sizeof (libentry_t));
		libentry->l_name = name;
		libentry->l_next = ptr->thrownentries;
		ptr->thrownentries = libentry;
	}
}

/*}}}*/
/*{{{  PUBLIC rememberallnames */
PUBLIC void
rememberallnames (libentry_t * libentries)
{
	fe_translate_data_t *const ptr = fe_translates (current_fe_handle);
	while (libentries != NULL) {
		libentry_t *t = libentries->l_next;
		libentries->l_next = ptr->globalnames;
		ptr->globalnames = libentries;
		libentries = t;
	}
}

/*}}}*/
/*{{{  PUBLIC setup_translation */
PUBLIC BOOL
setup_translation (wordnode * const in_name, const char *const ex_name, const int ex_len, const SOURCEPOSN locn)
{
	fe_translate_data_t *const ptr = fe_translates (current_fe_handle);
	wordnode *const trans_name = lookupword (ex_name, ex_len);
	DEBUG_MSG (("setup_translation %s to \"%s\":%d\n", WNameOf (in_name), ex_name, ex_len));
	/*{{{  Check for NUL/Space character in string */
	{
		int i;
		for (i = 0; i < ex_len; i++)
			switch (ex_name[i]) {
			case '\0':
				msg_out_ss (SEV_WARN, SYN, SYN_TRANSLATE_ILLEGAL_CHAR, locn, WNameOf (in_name), "NUL");
				return FALSE;
			case ' ':
			case '(':
			case ')':
				{
					char msg[4];
					msg[0] = '\'';
					msg[1] = ex_name[i];
					msg[2] = '\'';
					msg[3] = '\0';
					msg_out_ss (SEV_WARN, SYN, SYN_TRANSLATE_ILLEGAL_CHAR, locn, WNameOf (in_name), msg);
					return FALSE;
				}
			}
	}
	/*}}} */
	/*{{{  Check if this internal name has already been translated */
	{
		const translate_t *const in = search_translations (ptr->trans_internals, in_name);
		if (in != NULL) {	/* Already translated this internal name */
			if ((in->n_pair->n_name) == trans_name)
				return (TRUE);	/* We've already set up an identical translation */
			/* This 'internal' symbol has already been used in a translation */
			synwarn_s (SYN_TRANSLATE_DUPLICATE_IN, locn, WNameOf (in_name));
			return (FALSE);
		}
	}
	/*}}} */
	/*{{{  Check if this external name has already been translated */
	if (search_translations (ptr->trans_externals, trans_name) != NULL) {	/* This 'external' symbol has already been used in a translation */
		synwarn_s (SYN_TRANSLATE_DUPLICATE_EX, locn, ex_name);
		return (FALSE);
	}
	/*}}} */
	/*{{{  Check if we've already loaded the required library module */
	/*if (search_libentries(globalnames, trans_name) != NULL) */
	if ((search_libentries (ptr->globalnames, in_name) != NULL) ||
	    (search_libentries (ptr->globalnames, trans_name) != NULL) || (search_libentries (ptr->thrownentries, trans_name) != NULL)) {	/* This 'external' symbol has already been loaded by a #USE */
		synwarn_s (SYN_TRANSLATE_SEQUENCE, locn, ex_name);
		return (FALSE);
	}
	/*}}} */
	/*{{{  Store the details in the translation table */
	{
		/* use newvec here rather than memalloc, cos we never free the space */
		translate_t *in, *ex;
		in = (translate_t *) newvec (sizeof (translate_t));
		ex = (translate_t *) newvec (sizeof (translate_t));
		in->n_name = in_name;
		in->n_pair = ex;
		in->n_next = ptr->trans_internals;
		ex->n_name = trans_name;
		ex->n_pair = in;
		ex->n_next = ptr->trans_externals;
		ptr->trans_internals = in;
		ptr->trans_externals = ex;
	}
	/*}}} */
	return (TRUE);
}

/*}}}*/

/*{{{  PUBLIC init_translate */
PUBLIC void
init_translate (fe_translate_data_t ** ptr)
{
	fe_translate_data_t *local = newvec (sizeof (fe_translate_data_t));

	local->trans_internals = NULL;
	local->trans_externals = NULL;
	local->globalnames = NULL;
	local->thrownentries = NULL;

	*ptr = local;
}

/*}}}*/
