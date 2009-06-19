/* $Id: occompfe.c,v 1.4 1997/11/21 17:43:28 mdp2 Exp $ */

/*
 *	compiler frontend interface
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

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include "includes.h"

#include "instdef.h"		/* for ASM check instruction */
#include "generror.h"		/* for access to genmessagestring */
#include "genhdr.h"		/* for access to NO_SLOT */
#include "version.h"
/*}}}*/

/*{{{  local data*/
PRIVATE fe_handle_t *frontend_handle;
/* fe_data is made static so that it still exists while the backend
   is executing; thus various flags will still be ok. */

/* initialise it to default values: */
PRIVATE fe_data_t fe_data = { 0 };

/*PRIVATE const occam_parms_t *saved_parms;*//* INSdi02218 */
PRIVATE void (*saved_abort_fn) (void);
/*}}}*/

/*{{{  PRIVATEPARAM error_msg_printer*/
PRIVATEPARAM void error_msg_printer (const fe_handle_t * const fe_handle,
		   const err_severity_t severity, const char *message, const char *filename, const int line_number)
{
	const fe_data_t *const local_fe_data = fe_get_data_ptr (fe_handle);
	FILE *const local_errfile = severity == SEV_INFO_NOTERR ? local_fe_data->fe_outfile : local_fe_data->fe_errfile;
	const char *severity_description;
	/*{{{  setup severity description */
	severity_description =
		(severity <= SEV_INFO) ? "Information"
		: (severity <= SEV_WARN) ? "Warning" : (severity <= SEV_ERR_JMP) ? "Error" : (severity <= SEV_ABORT) ? "Serious"	/* bug INSdi01976 */
		: "Fatal";

	/*}}} */
	if (severity != SEV_INTERNAL_BANNER) {
		fprintf (local_errfile, "%s-%s", severity_description, TOOL);
		if (filename != NULL) {
			fprintf (local_errfile, "-%s", filename);
			if (line_number != 0)
				fprintf (local_errfile, "(%d)", line_number);
		}
	}
	if (severity != SEV_INTERNAL_BANNER)
		fprintf (local_errfile, "- %s\n", message);
	else
		fprintf (local_errfile, "%s\n", message);
}

/*}}}*/
/*{{{  PRIVATEPARAM local_abort_fn*/
PRIVATEPARAM void local_abort_fn (const fe_handle_t * fe_handle)
{
	USE_VAR (fe_handle);	/* stop unused var warning */
#if 0
	if (fe_handle != NULL)
		((user_data_t *) fe_get_user_ptr (fe_handle))->abort_fn ();
	else
		user_data.abort_fn ();
#endif
	/*saved_parms->abort_fn(); */
	saved_abort_fn ();
}

/*}}}*/

/*{{{  PRIVATE printerrors()*/
PRIVATE void printerrors (const char *const string, const int errors)
{
	if (information) {
		fputs (string, outfile);
		if (errors != 0)
			fprintf (outfile, " - %d error%s\n", errors, (errors > 1) ? "s" : "");
		else
			fputs (" ok\n", outfile);
	}
}

/*}}}*/

/*{{{  PUBLIC treenode *occam_compiler_frontend*/
PUBLIC treenode *occam_compiler_frontend (const occam_parms_t * const parms)
{
	fe_handle_t *handle;
	fe_data_t *const data = &fe_data;

	/*{{{  open frontend */
	data->fe_lang = parms->language;
	data->fe_toolversion = TOOL_VERSION;
	data->fe_toolname = TOOL;
	data->fe_pathname = parms->pathname;
	data->fe_txlib = &tx_global;
	data->fe_information = information;
	data->fe_outfile = outfile;
	data->fe_errfile = parms->errfile;
	data->fe_brieferrors = parms->brieferrors;
	data->fe_crasherrors = parms->crasherrors;
	data->fe_checkalias = *parms->checkalias;	/* may be reset after #OPTION directive */
	data->fe_checkusage = *parms->checkusage;	/* may be reset after #OPTION directive */
	data->fe_formalmodel = *parms->formalmodel;	/* may be reset after #OPTION directive */
	data->fe_fm_collct = *parms->fm_collct;		/* may be reset after #OPTION directive */
	data->fe_fm_toplevelonly = parms->fm_toplevelonly;
	data->fe_fm_nocr = parms->fm_nocr;
	data->fe_fm_inlinecr = parms->fm_inlinecr;
	data->fe_fm_comm = parms->fm_comm;
	data->fe_error_occurred = parms->error_occurred;
	data->fe_errormode = errormode;
	data->fe_noslot_value = NO_SLOT;
	data->fe_hash_version_string = parms->hash_version_string;
	data->fe_error_msg_fn = error_msg_printer;
	data->fe_backenderror_fn = parms->cleanup_after_error;
	data->fe_abort_fn = local_abort_fn;

	data->fe_warning_descoped_n = (warning_flags & WARNING_DESCOPED_N) != 0;
	data->fe_warning_descoped_p = (warning_flags & WARNING_DESCOPED_P) != 0;
	data->fe_warning_unused_v = (warning_flags & WARNING_UNUSED_V) != 0;
	data->fe_warning_unused_p = (warning_flags & WARNING_UNUSED_P) != 0;
	data->fe_warning_unused_r = (warning_flags & WARNING_UNUSED_R) != 0;
	data->fe_warning_chanofany = (warning_flags & WARNING_CHANOFANY) != 0;	/* WP 00026 30/03/92 */
	data->fe_warning_guy = (warning_flags & WARNING_GUY) != 0;	/* TS/1664  21/08/92 */
	data->fe_warning_case_input = (warning_flags & WARNING_CASE_INPUT) != 0;	/* TS/2048 25/01/93 */
	data->fe_warning_tag_input = (warning_flags & WARNING_TAG_INPUT) != 0;	/* TS/2048 25/01/93 */
	data->fe_warning_bad_place = (warning_flags & WARNING_BAD_PLACE) != 0;	/* TS/2048 25/01/93 */
	data->fe_warning_bad_asm = (warning_flags & WARNING_BAD_ASM) != 0;
	data->fe_warn_on_all_errors = parms->warn_on_all_errors;

#ifdef OCCAM2_5
	data->fe_align_fields = TRUE;	/* align all word and 16 bit objects */
	data->fe_align_records = TRUE;	/* round to word length */
#endif

	/*saved_parms = parms; */
	saved_abort_fn = parms->abort_fn;	/* INSdi02218 */

	handle = fe_open (data);

	/*fe_set_user_ptr(handle, &user_data); */

	/* install the error messages for the backend */
	err_install_messages (handle, GEN, genmessagestring);
	err_install_messages (handle, TRANS, transmessagestring);

	/*}}} */

	/*{{{  parse */
	data->fe_sourcefilename = sourcefilename;
	data->fe_onlylex = parms->onlylex;
	data->fe_guyinserts = parms->guyinserts;
	data->fe_noguy_yesasm = parms->noguy_yesasm;
	data->fe_option_fn = parms->process_option;
	data->fe_allow_inlines = parms->allow_inlines;
	data->fe_ignore_comments = parms->ignore_comments;
	data->fe_suppress_compat = parms->suppress_compat;
	data->fe_visible_tags = parms->visible_tags;
	data->fe_warn_comment_indent = parms->warn_comment_indent;
	data->fe_process_filename_fn = parms->process_filename_fn;
	data->fe_read_all_libs = parms->read_all_libs;

	fe_parse (handle);

	/* These can be modified by the #OPTION directive! */
	data->fe_checkalias = *parms->checkalias;
	data->fe_checkusage = *parms->checkusage;

	printerrors ("Parsed", fe_get_errorcount (handle));
	/*}}} */
/*{{{    typecheck*/
	if (!parms->onlylex && !parms->nochecking) {
		const int s = fe_get_errorcount (handle);

		data->fe_allowpredefs = parms->allowpredefs;
		data->fe_check_asm_fn = lookupinstruction;
		data->fe_chanaspointer = chanaspointer;
		data->fe_rangechecking = RANGECHECKING;

#if defined(CONFIG2) || defined(CONFIG3)
		data->fe_disable_rangechecking_in_branches = TRUE;
#endif
		fe_scope_and_check (handle);

#if defined(CONFIG2) || defined(CONFIG3)
		data->fe_disable_rangechecking_in_branches = FALSE;
#endif

		printerrors ("Type checked", fe_get_errorcount (handle) - s);
	}
/*}}}*/
	/*{{{  print number of errors etc */
	if (fe_get_errorcount (handle) != 0) {
		fprintf (outfile, "%d error", fe_get_errorcount (handle));
		if (fe_get_errorcount (handle) != 1)
			fputc ('s', outfile);
		fprintf (outfile, " found in source\n");
	}

	if (information && fe_get_errorcount (handle) == 0)
		fprintf (outfile, "Syntax tree occupies %ld bytes\n", tablesize ());

	/*}}} */
	/*{{{  alias check */
	if (fe_get_errorcount (handle) == 0 && !parms->onlylex && parms->call_alias_check) {
		data->fe_debuguse = parms->debuguse;
		data->fe_warn_on_usage_error = parms->warn_on_usage_error;
		fe_alias_and_usage_check (handle);
	}
	/*}}} */
	/*{{{  formal model check/generation*/
	if ((fe_get_errorcount (handle) == 0) && !parms->onlylex && !parms->nochecking) {
		fe_formalmodel_check (handle);
	}
	/*}}}*/
	/*{{{  information */
	if (!configuring && information) {
		if (fe_get_errorcount (handle) == 0)
			fputs ("No errors found in source\n", outfile);
		fprintf (outfile, "Read %d lines of source, syntax tree occupies %ld bytes\n", fe_get_totallines (handle), tablesize ());
	}
	/*}}} */

	set_vtilocn (NULL);

	frontend_handle = handle;	/* save it incase the backend needs access */

	return fe_get_treeroot (handle);
}

/*}}}*/

/*{{{  PUBLIC void      cleanup_occam_compiler_frontend*/
PUBLIC void cleanup_occam_compiler_frontend (void)
{
	fe_close (&frontend_handle);
}

/*}}}*/
/*{{{  PUBLIC fe_handle_t *be_get_fe_handle*/
PUBLIC fe_handle_t *be_get_fe_handle (void)
{
	return frontend_handle;
}

/*}}}*/

/*{{{  loadstdlib*/
PUBLIC treenode *loadstdlib (const char *const filename)
{
	return fe_loadstdlib (frontend_handle, filename);
}

/*}}}*/
/*{{{  filename access*/
PUBLIC const char *lookupfilename (const int n)
{
	return fe_lookupfilename (frontend_handle, n);
}
PUBLIC int parentoffile (const int n)
{
	return fe_parentoffile (frontend_handle, n);
}
PUBLIC int parentposnoffile (const int n)
{
	return fe_parentposnoffile (frontend_handle, n);
}
PUBLIC BOOL fileissource (const int n)
{
	return fe_fileissource (frontend_handle, n);
}
PUBLIC int numberoffiles (void)
{
	return fe_numberoffiles (frontend_handle);
}
PUBLIC SOURCEPOSN sourcefileof (const SOURCEPOSN locn)
{
	return fe_sourcefileof (frontend_handle, locn);
}

/*}}}*/
/*{{{  translate access for the backend*/
/*{{{  PUBLIC translate_from_internal*/
PUBLIC wordnode *translate_from_internal (wordnode * name)
{
	return fe_translate_from_internal (frontend_handle, name);
}

/*}}}*/
/*}}}*/
/*{{{  sourcehash access for the backend*/
PUBLIC BIT32 get_sourcehash (void)
{
	return fe_get_sourcehash (frontend_handle);
}

/*}}}*/
/*{{{  usage information*/
PUBLIC BOOL paraminputon (treenode * const n)
{
	return fe_paraminputoroutputon (frontend_handle, n, TRUE);
}

PUBLIC BOOL paramoutputon (treenode * const n)
{
	return fe_paraminputoroutputon (frontend_handle, n, FALSE);
}

PUBLIC BOOL isafreevarof (treenode * const v, treenode * const n)
{
	return fe_isafreevarof (frontend_handle, v, n);
}

PUBLIC void mark_free_vars (treenode * const tptr, const BOOL check_usage)
{
	fe_mark_free_vars (frontend_handle, tptr, check_usage);
}

PUBLIC void walk_free_vars (treenode * const nptr, int (*fn) (treenode **, void *), void *const voidptr)
{
	fe_walk_free_vars (frontend_handle, nptr, fn, voidptr);
}

/*}}}*/
/*{{{  list of predefines*/
PUBLIC treenode *get_list_of_predefs (void)
{
	return fe_get_predefname (frontend_handle, fe_predefname_LIST);
}

/*}}}*/
/*{{{  Returns the namenode of the predefine*/
PUBLIC treenode *get_predefname (int pdno)
{
	treenode *predefs = get_list_of_predefs ();
	for (; predefs != NULL; predefs = NextItem (predefs)) {
		if (NModeOf (ThisItem (predefs)) == pdno)
			return (ThisItem (predefs));
	}
	return NULL;
}

/*}}}*/
/*{{{  typechecker access functions*/
PUBLIC int ntypeof (treenode * const tptr)
{
	return fe_typeof (tptr);
}
PUBLIC treenode *gettype_main (treenode * const tptr)
{
	return fe_gettype_main (tptr);
}
PUBLIC treenode *gettype_main_orig (treenode *const tptr)
{
	return fe_gettype_main_orig (tptr);
}
PUBLIC treenode *gettype (treenode * const tptr)
{
	return fe_gettype (tptr);
}

/*}}}*/
/*{{{  error handling*/
PUBLIC BOOL memo_err (const int class, const int err, const SOURCEPOSN locn)
{
	return fe_memo_err (frontend_handle, class, err, locn);
}

/*}}}*/
