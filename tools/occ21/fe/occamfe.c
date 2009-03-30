/* $Id: occamfe.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	Occam two frontend interface
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

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "feinc.h"

#include "objrdr.h"		/* init_obj_reader */
#include "objlib.h"		/* needed to parse objtrans.h */
#include "objtrans.h"		/* init_translate */
#include "lexdef.h"		/* lexinit, lexfinish, lex_totallines */
#include "syndef.h"		/* syninit, parse_file */
#include "chkdef.h"		/* scopeandcheck_main */
#include "usehdr.h"		/* needed for usedef.h */
#include "usedef.h"		/* alias_and_usage_check, formalmodelcheck */
#include "mobiles.h"		/* any mobile stuff */
/*}}}*/

/* special.. */
extern BOOL preproc_dump;

/*{{{  local state */
/* These are used to control access to 'current_fe_handle' */
/*{{{  PRIVATE void         fe_set_local_state */
PRIVATE void fe_set_local_state (fe_handle_t * const new)
{
	/*printf("fe_set_local_state: new is %8X\n", new); */
	current_fe_handle = new;
	current_fe_data = (new == NULL) ? NULL : new->data_ptr;
}

/*}}}*/
/*{{{  PRIVATE fe_handle_t *fe_save_local_state */
PRIVATE fe_handle_t *fe_save_local_state (fe_handle_t * const new)
{
	fe_handle_t *const old = current_fe_handle;

	fe_set_local_state (new);
	return old;
}

/*}}}*/
/*{{{  PRIVATE void         fe_restore_local_state */
#if 0
PRIVATE void fe_restore_local_state (fe_handle_t * const old)
{
	fe_set_local_state (old);
}
#else
#define fe_restore_local_state(h) fe_set_local_state(h)
#endif
/*}}}*/
/*}}}*/

/*{{{  PUBLIC fe_handle_t *fe_open */
PUBLIC fe_handle_t *fe_open (const fe_data_t *const data)
{
	fe_handle_t *handle;
	static const fe_handle_t init_copy = { 0 };	/* will be initialised to zeroes and NULLs */

	/* we insert an initialiser to make icc put this into the text section;
	 * otherwise it thinks that it might be a tentative declaration with a
	 * different initialiser later, which might consist of non-NULL function
	 * pointers, etc.
	 */
	vtiinit (data->fe_outfile, data->fe_noslot_value);
	/*handle = newvec(sizeof(fe_handle_t)); */
	handle = memalloc (sizeof (fe_handle_t));
	*handle = init_copy;

	handle->data_ptr = data;
	handle->env_addr = &env;

	fe_set_local_state (handle);

	if (data->fe_lang == 0) {
		err_abort ("fe_open");
	}

	init_obj_reader ();
	init_translate (&handle->translates);
	declkeywords (data->fe_lang);
	lexinit ();
	syninit ();
	scopeinit ();
	chkinit ();

	/*{{{  initialise the sourcehash stuff */
	handle->sourcehash = 0xdefaced;

	handle->sourcehash ^= data->fe_txlib->ptype;
	handle->sourcehash ^= data->fe_txlib->pattr;
	handle->sourcehash ^= data->fe_errormode;
	handle->sourcehash ^= (int) ((data->fe_checkalias << 1) | data->fe_checkusage);
	if (data->fe_hash_version_string) {
		fe_sourcehash (handle, data->fe_toolversion, strlen (data->fe_toolversion));
	}
	/*}}} */

	return handle;
}

/*}}}*/
/*{{{  PUBLIC void         fe_close */
PUBLIC void fe_close (fe_handle_t ** handle)
{
	/*fe_handle_t *const old = fe_save_local_state(*handle); */
	fe_set_local_state (*handle);
	if (*handle != NULL) {
		lexfinish ((*handle)->data_ptr->fe_outfile);

		err_free_data (*handle);
		memfree (*handle);
	}
	*handle = NULL;

	/*fe_restore_local_state(old); */
	fe_set_local_state (NULL);
}

/*}}}*/
/*{{{  PUBLIC void         fe_parse */
PUBLIC void fe_parse (fe_handle_t * const handle)
{
	const fe_data_t *const data = handle->data_ptr;

	DEBUG_MSG (("fe_parse: current_fe_handle is #%X\n", (unsigned int)current_fe_handle));
	fe_set_local_state (handle);
	handle->tree = parse_file (data->fe_sourcefilename, data->fe_onlylex);
	handle->lines = lex_totallines ();
	if (preproc_dump) {
		preproc_dump_defines (data->fe_outfile);
	}
	return;
}

/*}}}*/
/*{{{  PUBLIC void         fe_scope_and_check */
PUBLIC void fe_scope_and_check (fe_handle_t * const handle)
{
	const fe_data_t *const data = handle->data_ptr;

	DEBUG_MSG (("fe_scope_and_check: current_fe_handle is #%X\n", (unsigned int)current_fe_handle));
	fe_set_local_state (handle);
	scopeandcheck_main (&handle->tree, data->fe_allowpredefs, TRUE);
	printundeclarednames (data->fe_errfile);
	freeup_temp_workspace ();

}

/*}}}*/
/*{{{  PUBLIC void         fe_alias_and_usage_check */
PUBLIC void fe_alias_and_usage_check (fe_handle_t * const handle)
{
	const fe_data_t *const data = handle->data_ptr;

	DEBUG_MSG (("fe_alias_and_usage_check: current_fe_handle is #%X\n", (unsigned int)current_fe_handle));
	fe_set_local_state (handle);
#if 0
fprintf (stderr, "fe_alias_and_usage_check: before check, tree = ");
printtreenl (stderr, 4, handle->tree);
#endif
	alias_and_usage_check (handle->tree, data->fe_checkalias, data->fe_checkusage, data->fe_information ? data->fe_outfile : NULL);
	freeup_temp_workspace ();
}

/*}}}*/
/*{{{  PUBLIC void         fe_formalmodel_check (fe_handle_t *const handle)*/
PUBLIC void fe_formalmodel_check (fe_handle_t *const handle)
{
	const fe_data_t *const data = handle->data_ptr;
	const char *handle_filename = fe_lookupfilename (handle, 0);
	char *fm_filename = (char *)memalloc (strlen (handle_filename) + 5);

	DEBUG_MSG (("fe_formalmodel_check: current_fe_handle is #%X\n", (unsigned int)current_fe_handle));
	fe_set_local_state (handle);

	sprintf (fm_filename, "%s", handle_filename ?: "default.occ");
	if ((strlen (fm_filename) > 4) && (fm_filename[strlen(fm_filename) - 4] == '.')) {
		int offs = strlen (fm_filename) - 4;

		sprintf (fm_filename + offs, ".cspx");
	} else {
		int offs = strlen (fm_filename);

		sprintf (fm_filename + offs, ".cspx");
	}
		
	formalmodelcheck (handle->tree, data->fe_formalmodel, fm_filename, handle);

	memfree (fm_filename);
	freeup_temp_workspace ();
}
/*}}}*/
/*{{{  PUBLIC treenode    *fe_loadstdlib */
PUBLIC treenode *fe_loadstdlib (fe_handle_t * const handle, const char *filename)
{
	fe_handle_t *const old = fe_save_local_state (handle);
	treenode *res;

	DEBUG_MSG (("fe_loadstdlib: current_fe_handle is #%X\n", (unsigned int)current_fe_handle));
	res = local_loadstdlib (filename);
	fe_restore_local_state (old);
	return res;
}

/*}}}*/

/*{{{  access functions */
/*{{{  PUBLIC treenode    *fe_get_treeroot */
PUBLIC treenode *fe_get_treeroot (const fe_handle_t * const handle)
{
	return handle->tree;
}

/*}}}*/
/*{{{  PUBLIC int          fe_get_totallines */
PUBLIC int fe_get_totallines (const fe_handle_t * const handle)
{
	return handle->lines;
}

/*}}}*/
/*{{{  PUBLIC const fe_data_t *fe_get_data_ptr */
PUBLIC const fe_data_t *fe_get_data_ptr (const fe_handle_t * const handle)
{
	return handle->data_ptr;
}

/*}}}*/
/*{{{  PUBLIC jmp_buf     *fe_get_env_addr */
PUBLIC jmp_buf *fe_get_env_addr (const fe_handle_t * const handle)
{
	return handle->env_addr;
}

/*}}}*/
/*{{{  PUBLIC void         fe_set_user_ptr */
PUBLIC void fe_set_user_ptr (fe_handle_t * const handle, void *const user_ptr)
{
	handle->user_ptr = user_ptr;
}

/*}}}*/
/*{{{  PUBLIC void        *fe_get_user_ptr */
PUBLIC void *fe_get_user_ptr (const fe_handle_t * const handle)
{
	return handle->user_ptr;
}

/*}}}*/
/*}}}*/
/*{{{  usage information */
PUBLIC BOOL fe_paraminputoroutputon (fe_handle_t * const handle, treenode * n, const BOOL inputnotoutput)
{
	BOOL res;
	fe_handle_t *const old = fe_save_local_state (handle);

	DEBUG_MSG (("fe_paraminputoroutputon: current_fe_handle is #%X\n", (unsigned int)current_fe_handle));
	res = local_paraminputoroutputon (n, inputnotoutput);
	fe_restore_local_state (old);
	return res;
}

PUBLIC BOOL fe_isafreevarof (fe_handle_t * const handle, treenode * v, treenode * n)
{
	BOOL res;
	fe_handle_t *const old = fe_save_local_state (handle);

	DEBUG_MSG (("fe_isafreevarof: current_fe_handle is #%X\n", (unsigned int)current_fe_handle));
	res = local_isafreevarof (v, n);
	fe_restore_local_state (old);
	return res;
}

PUBLIC void fe_mark_free_vars (fe_handle_t * const handle, treenode * tptr, const BOOL check_usage)
{
	fe_handle_t *const old = fe_save_local_state (handle);

	DEBUG_MSG (("fe_mark_free_vars: current_fe_handle is #%X\n", (unsigned int)current_fe_handle));
	use_mark_free_vars (tptr, check_usage);
	fe_restore_local_state (old);
}

PUBLIC void fe_walk_free_vars (fe_handle_t * handle, treenode * nptr, int (*fn) (treenode **, void *), void *const voidptr)
{
	fe_handle_t *const old = fe_save_local_state (handle);

	DEBUG_MSG (("fe_walk_free_vars: current_fe_handle is #%X\n", (unsigned int)current_fe_handle));
	use_walk_free_vars (nptr, fn, voidptr);
	fe_restore_local_state (old);
}

PUBLIC char *fe_udvstatestringof (treenode *const nptr)
{
	return use_udvstatestringof (nptr);
}

/*}}}*/
/*{{{  filename handling */
/*{{{  PRIVATE filetablestruct_t *lookupfilenameptr(n) */
PRIVATE const filetablestruct_t *lookupfilenameptr (const fe_handle_t * const handle, int n);
#ifdef _ICC			/* bug INSdi01953 05/04/93 */
#pragma IMS_nosideeffects(lookupfilenameptr)
#endif
PRIVATE const filetablestruct_t *lookupfilenameptr (const fe_handle_t * const handle, int n)
{
	const filetablestruct_t *ptr = handle->filenames;

	/* The list is in reverse order of files, so we want the n'th from last */
	n = (handle->numfiles - n) - 1;
	while (n-- > 0) {
		ptr = ptr->ftnext;
	}
	return ptr;
}

/*}}}*/
/*{{{  PUBLIC int   fe_addfilename(name, mode, parent, parentline) */
PUBLIC int fe_addfilename (fe_handle_t * const handle, wordnode * const name, const int issource, const int parent, const int parentline)
{
	filetablestruct_t *file = (filetablestruct_t *) newvec (sizeof (filetablestruct_t));

	file->ftname = name;
	file->ftissource = issource;
	file->ftparent = parent;
	file->ftparentline = parentline;

	file->ftnext = handle->filenames;
	handle->filenames = file;

	return handle->numfiles++;
}

/*}}}*/
/*{{{  PUBLIC const char *fe_lookupfilename(n) */
PUBLIC const char *fe_lookupfilename (const fe_handle_t *const handle, const int n)
{
	if ((n >= 0) && (n < handle->numfiles)) {
		return (WNameOf (lookupfilenameptr (handle, n)->ftname));
	} else {
		return NULL;
	}
}

/*}}}*/
/*{{{  PUBLIC BOOL  fe_fileissource(n) */
PUBLIC BOOL fe_fileissource (const fe_handle_t * const handle, const int n)
{
	if (!handle) {
		return FALSE;
	}
	return lookupfilenameptr (handle, n)->ftissource;
}

/*}}}*/
/*{{{  PUBLIC int   fe_parentoffile(n) */
PUBLIC int fe_parentoffile (const fe_handle_t * const handle, const int n)
{
	return (lookupfilenameptr (handle, n)->ftparent);
}

/*}}}*/
/*{{{  PUBLIC int   fe_parentposnoffile(n) */
PUBLIC int fe_parentposnoffile (const fe_handle_t * const handle, const int n)
{
	return (lookupfilenameptr (handle, n)->ftparentline);
}

/*}}}*/
/*{{{  PUBLIC int   fe_numberoffiles() */
PUBLIC int fe_numberoffiles (const fe_handle_t * const handle)
{
	return handle->numfiles;
}

/*}}}*/
/*{{{  PUBLIC SOURCEPOSN fe_sourcefileof */
PUBLIC SOURCEPOSN fe_sourcefileof (fe_handle_t * const handle, const SOURCEPOSN old_locn)
{
	SOURCEPOSN res;
	fe_handle_t *const old = fe_save_local_state (handle);

	DEBUG_MSG (("fe_sourcefileof: current_fe_handle is #%X\n", (unsigned int)current_fe_handle));
	res = err_sourcefileof (old_locn);
	fe_restore_local_state (old);
	return res;
}

/*}}}*/
/*}}}*/
/*{{{  translation access for the backend */
/*{{{  PUBLIC fe_translate_data_t *fe_translates */
PUBLIC fe_translate_data_t *fe_translates (const fe_handle_t * const handle)
{
	return handle->translates;
}

/*}}}*/
/*{{{  PUBLIC wordnode *fe_translate_from_internal */
PUBLIC wordnode *fe_translate_from_internal (const fe_handle_t * const handle, wordnode * name)
{
	return local_translate_from_internal (fe_translates (handle), name);
}

/*}}}*/
/*}}}*/
/*{{{  source code hash functions */
/*{{{  PUBLIC BIT32 fe_get_sourcehash */
PUBLIC BIT32 fe_get_sourcehash (const fe_handle_t * const handle)
{
	return handle->sourcehash;
}

/*}}}*/
/*{{{  PUBLIC void  fe_sourcehash */
/* We have now arranged things so that we already know the length,
 * and we also know that string is maximally aligned, because it
 * was allocated via malloc.
 * Hence we can hash this one word at a time.
 */
PUBLIC void fe_sourcehash (fe_handle_t * const handle, const char *const string, const int len)
{
	register BIT32 hash = handle->sourcehash;	/* copy into temporary for fast access */

#if 0
	/*{{{  char at a time */
	{
		register const char *s = string;

		while (*s != '\0') {
			hash ^= (*s++);
		}
	}
	/*}}} */
#else
	/*{{{  word at a time */
	{
		register int words = len / (sizeof (int));
		register const int *const ptr = (const int *) string;
		register const char *const s = string;
		register int i;

		for (i = 0; i < words; i++) {
			hash ^= ptr[i];
		}

		/* now do the last few a character at a time */
		for (i = words * sizeof (int); i < len; i++) {
			hash ^= s[i];
		}
	}
	/*}}} */
#endif
	handle->sourcehash = (hash & 0x80000000) ? ((hash << 1) + 1) : (hash << 1);	/* rotate */
}

/*}}}*/
/*}}}*/
/*{{{  special predefine handling */
PUBLIC void fe_set_predefname (fe_handle_t * const handle, treenode * const nptr, const int predef)
{
	handle->predefname[predef] = nptr;
}


PUBLIC treenode *fe_get_predefname (const fe_handle_t * const handle, const int predef)
{
	return handle->predefname[predef];
}

/*}}}*/
/*{{{  pre-processor defining*/
PUBLIC BOOL fe_preproc_cmdline_define (const char *arg)
{
	return preproc_cmdline_define (arg);
}
/*}}}*/
/*{{{  error handling */
PUBLIC void fe_set_errorcount (fe_handle_t * const handle, const int value)
{
	handle->errorcount = value;
}


PUBLIC int fe_get_errorcount (const fe_handle_t * const handle)
{
	return handle->errorcount;
}


PUBLIC BOOL fe_memo_err (fe_handle_t * const handle, const int class, const int err, const SOURCEPOSN locn)
{
	return err_memo_err (handle, class, err, locn);
}

/*}}}*/
/*{{{  saving state */
/* This is done 'around' the NDL reader call to restore some of
 * the global state.
 * If necessary, we can pass back a structure containing copies of various
 * globals; these can then be restored later, if required.
 */

PUBLIC const void *fe_save_state (const fe_handle_t * const fe_handle)
{
	return fe_handle;
}


PUBLIC void fe_restore_state (fe_handle_t * const fe_handle, const void *const state)
{
	const fe_handle_t *const old_state = state;

	if (fe_handle != old_state) {
		err_abort ("fe_restore_state");
	}
	/* The error functions need to access current_fe_handle etc */
	fe_set_local_state (fe_handle);
	DEBUG_MSG (("fe_restore_state: current_fe_handle is #%X\n", (unsigned int)current_fe_handle));

	declkeywords (current_fe_data->fe_lang);
}

/*}}}*/
/*{{{  type checker access functions */
PUBLIC treenode *fe_gettype_main (treenode * const tptr)
{
	return chk_gettype_main (tptr, FALSE);
}

PUBLIC treenode *fe_gettype_main_orig (treenode *const tptr)
{
	return chk_gettype_main (tptr, TRUE);
}

PUBLIC treenode *fe_gettype (treenode * const tptr)
{
	return chk_gettype (tptr);
}

PUBLIC int fe_typeof (treenode * const tptr)
{
	return chk_typeof (tptr);
}

PUBLIC treenode *fe_lookupname (wordnode *const w)
{
	return chk_lookupname (w);
}
/*}}}*/

