/* $Id: objwrt.c,v 1.4 1997/10/07 14:14:21 mdp2 Exp $ */

/*
 *	object file writing
 *	Copyright (C) 1987, 1991 Inmos Limited
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
#ifdef target_cpu_alpha
#define LONG64
#endif
#include <stdio.h>
#include <string.h>
#include "includes.h"
#include "tcoff.h"		/* IMPORTED */
#include "tcofflib.h"		/* IMPORTED */

#include "objwrt.h"
#include "objlib.h"

#include "version.h"
#include "profile.h"

#include "arg.h"

/*}}}*/

/*{{{  constant definitions*/
/* we run the configurer after linking, so all the object files will
   be linked already, and we have to generate an object file
   which looks like it has been linked.
*/
#define LINKED_OUTPUT_FILE (configuring)

typedef struct commentchain_s	/* used for remembering #COMMENTs */
{
	struct commentchain_s *next;	/* ptr to next on chain */
	char *stringptr;	/* the actual string */
}
commentchain_t;

/*}}}*/

/*{{{  public variables*/
PUBLIC BIT32 object_file_wrt_flags = OBJ_FILE_WRT_DEFAULT;

/*}}}*/
/*{{{  private variables*/
/* These are remembered when encountered by a #COMMENT, so that they can
   be inserted into the object file
*/
PRIVATE commentchain_t *commentchain = NULL;	/* list of #COMMENTs */
PRIVATE BOOL reversed_commentchain = FALSE;
/* so that we only write the comment string once */
PRIVATE BOOL written_comment = FALSE;

PRIVATE int refno;		/* Number of library entry point references */

#define INVALID_SYMBOL_ID (-1)	/* Any negative value */
PRIVATE int exported_origin_id;
PRIVATE int local_text;
PRIVATE size_t code_size;

/* This is used to remember the start position of the current module,
   so that the value may be written into its own linker map.
*/
PRIVATE long int seek_position_of_startmodule;

/* These implement #PRAGMA LINKAGE [ "string" ] */
/* Name of text section in TCOFF file */
PRIVATE const char *text_name = "text%base";
PRIVATE const char *const text_name_priority = "pri%text%base";

PRIVATE int language_name;	/* Name to be stored in descriptor */

/*}}}*/

/*{{{  PUBLIC void process_hcomment*/
PUBLIC void process_hcomment (const wordnode * const string)
{
	const int len = WLengthOf (string);
	char *const copy = (char *) memalloc (len + 1);
	commentchain_t *const ptr = (commentchain_t *) memalloc (sizeof (commentchain_t));
	memcpy (copy, WNameOf (string), len + 1);	/* include trailing '\0' */
	ptr->stringptr = copy;
	ptr->next = commentchain;
	commentchain = ptr;
	DEBUG_MSG (("process_hcomment; string: \"%s\", len:%d\n", WNameOf (string), len));
}

/*}}}*/

/*{{{  low-level object file writing           for the backend*/
/*{{{  PRIVATE void write_error ()*/
PRIVATE void write_error (void)
{
	msg_out_i (SEV_ABORT, ANY_MODULE, ANY_OBJFILE_WRITE_ERROR, NOPOSN, ferror (objfile));
}

/*}}}*/

/*{{{  PRIVATE int p_symbol_id (fs, type, scope, string)*/
PRIVATE int p_symbol_id (FILE * fs, const long int type, const long int scope, const char *const string, long int origin_id)
{
	if (type != 0L)
		tcoff_putrec (fs, SECTION_TAG, "%ld%ld%s", type, scope, string);
	else if (origin_id != INVALID_SYMBOL_ID)
		tcoff_putrec (fs, SPECIFIC_SYMBOL_TAG, "%ld%s%ld", scope, string, origin_id);
	else
		tcoff_putrec (fs, SYMBOL_TAG, "%ld%s", scope, string);
	return (refno++);
}

/*}}}*/
/*{{{  PRIVATE put_descriptor (id, str, ws, vs);*/
#ifdef MOBILES
PRIVATE void put_descriptor (const long int id, const char *const str, const long int ws, const long int vs, const long int ms)
#else
PRIVATE void put_descriptor (const long int id, const char *const str, const long int ws, const long int vs)
#endif
{
	const long int str_len = strlen (str);
#ifdef MOBILES
	const long int pseudo_str_len = tcoff_sizel (ws) + tcoff_sizel (vs) + tcoff_sizel (ms) + str_len;
#else
	const long int pseudo_str_len = tcoff_sizel (ws) + tcoff_sizel (vs) + str_len;
#endif
	const long int rec_len = tcoff_sizel (id) + tcoff_sizel (language_name) + tcoff_sizel (pseudo_str_len) + pseudo_str_len;
	size_t written;
	tcoff_putl (objfile, DESCRIPTOR_TAG);
	tcoff_putl (objfile, rec_len);
	tcoff_putl (objfile, id);
	tcoff_putl (objfile, language_name);
	tcoff_putl (objfile, pseudo_str_len);	/* ws + vs + str */
	tcoff_putl (objfile, ws);
	tcoff_putl (objfile, vs);
#ifdef MOBILES
	tcoff_putl (objfile, ms);
#endif
	written = fwrite (str, sizeof (char), (size_t) str_len, objfile);
	if (written != str_len)
		write_error ();
}

/*}}}*/
/*}}}*/
/*{{{  high level object file writing          for the code emitter*/
/*{{{  PUBLIC void write_id() ->LINKABLE, START_MODULE, etc */
PUBLIC void write_id (char *const libname, const BOOL write_lib_id)
{
	char unique_id[MAX_FILENAME_LENGTH + 20];	/* used for assembling unique module id */
	DEBUG_MSG (("write_id\n"));
	(void) libname;		/* stop warning */
	(void) write_lib_id;	/* ditto */

	/*{{{  Calculate the "unique" origin string */
	sprintf (unique_id, "%s:%08X", sourcefilename, get_sourcehash ());
	/*}}} */
	/*{{{  Write the standard preamble */
	tcoff_putrec (objfile, LINKED_OUTPUT_FILE ? LINKED_UNIT_TAG : LINKABLE_TAG, "");
	seek_position_of_startmodule = ftell (objfile);

	tcoff_putrec (objfile, START_MODULE_TAG, "%ld%ld%ld%s", processortype, processorattr, LINKED_OUTPUT_FILE ? LANG_LINKED : language_name,
/*unique_id *//* "" */ chanaspointer ? "" : "Old chans");
	tcoff_putrec (objfile, VERSION_TAG, "%s%s", TOOL, sourcefilename);
	/*}}} */
	/*{{{  Write the compiler's version string */
	if (!written_comment) {
		extern arg2_descriptor cloptions[];
		char info_line[100];
		arg2_get_info_line (USING_ARG_VERSION, cloptions, info_line);
		/*tcoff_putrec (objfile, COMMENT_TAG, "%ld%ld%s", FALSE, TRUE, C_COMPATIBILITY); */
		tcoff_putrec (objfile, COMMENT_TAG, "%ld%ld%s", FALSE, TRUE, info_line);
		if (tx_global.hast9alphaprobs)
			tcoff_putrec (objfile, COMMENT_TAG, "%ld%ld%s", FALSE, TRUE, "T9000 Alpha");
		written_comment = TRUE;
	}
	/*}}} */
	/*{{{  Record the target endian */
	{
		char str[32];
		sprintf (str, ".ENDIAN %s", target_bigendian ? "big" : "little");
		tcoff_putrec (objfile, COMMENT_TAG, "%ld%ld%s", FALSE, TRUE, str); 
	}
	/*}}}*/
	/*{{{  Define the standard symbols */
	{
		const int text_base = p_symbol_id (objfile, EXECUTE_SECTION | READ_SECTION, EXPORT_USAGE,
						   objwrt_get_section_name (), INVALID_SYMBOL_ID);

		tcoff_putrec (objfile, SET_LOAD_POINT_TAG, "%ld", text_base);
		if (!LINKED_OUTPUT_FILE) {
			local_text = p_symbol_id (objfile, 0L, LOCAL_USAGE, "local%text", INVALID_SYMBOL_ID);
			tcoff_putrec (objfile, DEFINE_LABEL_TAG, "%ld", local_text);
		}
	}
	/*}}} */
	/*{{{  Export origin symbol */
	if (!(object_file_wrt_flags & OBJ_FILE_WRT_NO_EXPORT_ORIGINS)) {
		exported_origin_id = p_symbol_id (objfile, 0L, ORIGIN_USAGE | EXPORT_USAGE, unique_id, INVALID_SYMBOL_ID);
		if (LINKED_OUTPUT_FILE) {
			exported_origin_id = INVALID_SYMBOL_ID;	/* don't use it for export */
		} else {
#ifdef MOBILES
			put_descriptor (exported_origin_id, "", ORIGIN_WS, 0, 0);
#else
			put_descriptor (exported_origin_id, "", ORIGIN_WS, 0);
#endif
		}
	} else
		exported_origin_id = INVALID_SYMBOL_ID;
	/*}}} */
	/*{{{  Write #COMMENTs */
	if (!reversed_commentchain)
		/*{{{  reverse the comment chain */
	{
		commentchain_t *reverser = NULL;
		/* reverse the commentchain to get them back into correct order */
		while (commentchain != NULL) {
			commentchain_t *next = commentchain->next;
			commentchain->next = reverser;
			reverser = commentchain;
			commentchain = next;
		}
		commentchain = reverser;
		reversed_commentchain = TRUE;
	}
	/*}}} */

	while (commentchain != NULL) {
		commentchain_t *temp = commentchain;
		DEBUG_MSG (("Writing comment to object file: \"%s\"\n", commentchain->stringptr));
		tcoff_putrec (objfile, COMMENT_TAG, "%ld%ld%s", FALSE, TRUE, commentchain->stringptr);
		commentchain = commentchain->next;
		if (!configuring) {
			/* we must not free this up when configuring, cos we must
			   write lots of object files */
			memfree (temp->stringptr);
			memfree (temp);
		}
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void write_entry_desc*/
PUBLIC void write_entry_desc (treenode * const nptr, const INT32 offset)
{
	int id;
	const char *desc_buffer;
	const wordnode *const nameptr = translate_from_internal (NNameOf (nptr));

	DEBUG_MSG (("write_entry_desc: for \"%s\"\n", WNameOf (NNameOf (nptr))));

	desc_buffer = create_descriptor_string (be_get_fe_handle (), nptr, nameptr,
						(object_file_wrt_flags & OBJ_FILE_WRT_SHORT_DESCRIPTORS) != 0,
						(object_file_wrt_flags & OBJ_FILE_WRT_MERGE_DESCRIPTORS) != 0,
						(object_file_wrt_flags & OBJ_FILE_WRT_SPACES_DESCRIPTORS) != 0);

	/*{{{  Write out the symbol and workspace/vectorspace */
	{
		id = p_symbol_id (objfile, 0L, EXPORT_USAGE | ROUTINE_USAGE, WNameOf (nameptr), exported_origin_id);
		if (LINKED_OUTPUT_FILE)
			tcoff_putrec (objfile, DEFINE_SYMBOL_TAG, "%ld%ld%ld", id, CO_VALUE_TAG, offset);
		else {
			int label_ws = INVALID_SYMBOL_ID;
			int label_vs = INVALID_SYMBOL_ID;	/* initialised to shut up gcc's optimiser */
#ifdef MOBILES
			int label_ms = INVALID_SYMBOL_ID;
#endif

			if ((object_file_wrt_flags & OBJ_FILE_WRT_NO_WS_SYMBOLS) == 0) {
				const int len = WLengthOf (nameptr);
				char *const str = memalloc (len + 4);
				memcpy (str, WNameOf (nameptr), len);
				memcpy (&str[len], "'ws", 4);	/* include trailing NUL */
				label_ws = p_symbol_id (objfile, 0L, EXPORT_USAGE | UNINDEXED_USAGE, str, exported_origin_id);
				memcpy (&str[len], "'vs", 4);	/* include trailing NUL */
				label_vs = p_symbol_id (objfile, 0L, EXPORT_USAGE | UNINDEXED_USAGE, str, exported_origin_id);
#ifdef MOBILES
				memcpy (&str[len], "'ms", 4);	/* include trailing \0 */
				label_ms = p_symbol_id (objfile, 0L, EXPORT_USAGE | UNINDEXED_USAGE, str, exported_origin_id);
#endif
				memfree (str);
			}

			tcoff_putrec (objfile, DEFINE_SYMBOL_TAG, "%ld%ld%ld%ld%ld%ld", id, PLUS_OP, SV_VALUE_TAG, local_text, CO_VALUE_TAG, offset);

			if ((object_file_wrt_flags & OBJ_FILE_WRT_NO_WS_SYMBOLS) == 0) {
				tcoff_putrec (objfile, DEFINE_SYMBOL_TAG, "%ld%ld%ld", label_ws, CO_VALUE_TAG, NPDatasizeOf (nptr));
				tcoff_putrec (objfile, DEFINE_SYMBOL_TAG, "%ld%ld%ld", label_vs, CO_VALUE_TAG, NPVSUsageOf (nptr));
#ifdef MOBILES
				tcoff_putrec (objfile, DEFINE_SYMBOL_TAG, "%ld%ld%ld", label_ms, CO_VALUE_TAG, NPMSUsageOf (nptr));
#endif
			}
		}
	}
	/*}}} */


	/*{{{  Write out the descriptor */
#ifdef MOBILES
#if 0
fprintf (stderr, "writing out descriptor (lots of this (!)).  NPDatasizeOf(nptr)=%d, NPVSUsageOf(nptr)=%d\n", NPDatasizeOf(nptr), NPMSUsageOf(nptr));
#endif
	put_descriptor (id, desc_buffer, NPDatasizeOf (nptr), NPVSUsageOf (nptr), NPMSUsageOf (nptr));
#else
	put_descriptor (id, desc_buffer, NPDatasizeOf (nptr), NPVSUsageOf (nptr));
#endif
	/*}}} */

	/*{{{  Define main if necessary */
	if (LINKED_OUTPUT_FILE) {
		tcoff_putrec (objfile, DEFINE_MAIN_TAG, "%ld", id);
	}
	/*}}} */
}

/*}}}*/

/*{{{  PUBLIC void write_lcal_entry*/
PUBLIC void write_local_entry (treenode * const nptr, const INT32 offset)
{
	int id;
	const wordnode *nameptr;
	DEBUG_MSG (("write_local_entry: for \"%s\"\n", WNameOf (NNameOf (nptr))));

	/*{{{  Write out the symbol */
	if (!LINKED_OUTPUT_FILE) {
		nameptr = translate_from_internal (NNameOf (nptr));
		id = p_symbol_id (objfile, 0L, LOCAL_USAGE | ROUTINE_USAGE, WNameOf (nameptr), exported_origin_id);
		tcoff_putrec (objfile, DEFINE_SYMBOL_TAG, "%ld%ld%ld%ld%ld%ld", id, PLUS_OP, SV_VALUE_TAG, local_text, CO_VALUE_TAG, offset);
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void write_library_calls*/
/*****************************************************************************
 *
 *  write_library_calls writes out the patch information for all library
 *                      routine calls on 'libcalls'
 *
 *****************************************************************************/
PUBLIC void write_library_calls (const BOOL reverse, treenode * libcalls, const int instruction)
{
	if (!LINKED_OUTPUT_FILE) {	/* don't import anything if linked */
		DEBUG_MSG (("write_library_calls\n"));
		if (reverse)
			/*{{{  Reverse the list of library calls */
		{
			treenode *reverser = NULL;
			/* reverse the list of calls so that we need less 'ADJUST_POINT' tags */
			/* If you wanted, this bit could simply be left out.                  */
			while (libcalls != NULL) {
				treenode *next = NLEntryNextOf (libcalls);
				SetNLEntryNext (libcalls, reverser);
				reverser = libcalls;
				libcalls = next;
			}
			libcalls = reverser;
			/* Now the list is the same, but reversed */
		}
		/*}}} */

		{
			INT32 adjust;
			INT32 pos = code_size;
			for (; libcalls != NULL; libcalls = NLEntryNextOf (libcalls))
				/*{{{  Insert a linker patch directive */
			{
				const BOOL allow_fpu_call = configuring;
				int origin_id = INVALID_SYMBOL_ID;
				wordnode *extern_name;
				module_t *module = get_procdef (libcalls, NOPOSN, FALSE, allow_fpu_call)->p_module;
				DEBUG_MSG (("Libcall: \"%s\", refno %d, ", WNameOf (NNameOf (libcalls)), refno));

				/*{{{  Translate name if necessary */
				extern_name = translate_from_internal (NNameOf (libcalls));
#ifdef DEBUG
				if (extern_name != NNameOf (libcalls))
					DEBUG_MSG (("Translated to: \"%s\"\n", WNameOf (extern_name)));
#endif
				/*}}} */
				/*{{{  Insert adjustment if necessary */
				adjust = (INT32) NLEntryOffsetOf (libcalls) - pos;
				pos = (INT32) NLEntryOffsetOf (libcalls) + libpatchsize;
				if (adjust != 0)
					tcoff_putrec (objfile, ADJUST_POINT_TAG, "%ld%ld", CO_VALUE_TAG, adjust);
				/*}}} */
				/*{{{  Set up the correct origin symbol value */
				if (object_file_wrt_flags & OBJ_FILE_WRT_NO_IMPORT_ORIGINS)
					DEBUG_MSG (("Not importing names\n"));
				else if (module->m_id_val == ORIGIN_NOT_FOUND)
					/* imported from a lib, but no origin there */
					DEBUG_MSG (("No origin\n"));
				else {	/* there is an origin for this entryname */

					if (module->m_id_val == ORIGIN_FOUND) {	/* not yet imported */
						DEBUG_MSG (("First time found: \"%s\", set to %d, ", WNameOf (module->m_name), refno));
						module->m_id_val = p_symbol_id (objfile, 0L,
										IMPORT_USAGE | ORIGIN_USAGE, WNameOf (module->m_name),
										INVALID_SYMBOL_ID);
					}
					origin_id = module->m_id_val;
				}
				/*}}} */
				/*{{{  Write out the linker patch directive */
				{
					const int label = p_symbol_id (objfile, 0L, IMPORT_USAGE,
								       WNameOf (extern_name), origin_id);
					tcoff_putrec (objfile, LOAD_PREFIX_TAG, "%ld%ld%ld%ld%ld%ld%ld",
						      libpatchsize, AP_VALUE_TAG,
						      MINUS_OP, SV_VALUE_TAG, label, LP_VALUE_TAG, (long int) instruction);
				}
				/*}}} */
			}
			/*}}} */

			/*{{{  Adjust back to the end of the text section */
			adjust = code_size - pos;
			if (adjust != 0)
				tcoff_putrec (objfile, ADJUST_POINT_TAG, "%ld%ld", CO_VALUE_TAG, adjust);
			/*}}} */
		}
	}
}

/*}}}*/

/*{{{  PUBLIC void write_asm_desc*/
PUBLIC void write_asm_desc (FILE * const asm_file, treenode * const nptr)
{
	const wordnode *const nameptr = translate_from_internal (NNameOf (nptr));
	const char *const name = WNameOf (nameptr);
	const char *desc_buffer;
	fprintf (asm_file, "\n\tglobal $%s", name);
	fprintf (asm_file, "\n$%s:", name);

	desc_buffer = create_descriptor_string (be_get_fe_handle (), nptr, nameptr,
						(object_file_wrt_flags & OBJ_FILE_WRT_SHORT_DESCRIPTORS) != 0,
						(object_file_wrt_flags & OBJ_FILE_WRT_MERGE_DESCRIPTORS) != 0,
						(object_file_wrt_flags & OBJ_FILE_WRT_SPACES_DESCRIPTORS) != 0);

#ifdef MOBILES
	fprintf (asm_file, "\n\tdescriptor $%s \"%s\" %s %d %d %d ",
		 name, name, language_name == LANG_OCCAM ? "occam" : "occam_harness", NPDatasizeOf (nptr), NPVSUsageOf (nptr), NPMSUsageOf (nptr));
#else
	fprintf (asm_file, "\n\tdescriptor $%s \"%s\" %s %d %d ",
		 name, name, language_name == LANG_OCCAM ? "occam" : "occam_harness", NPDatasizeOf (nptr), NPVSUsageOf (nptr));
#endif
	fprintf (asm_file, "\"%s\"\n", desc_buffer);
}

/*}}}*/

/*{{{  PUBLIC INT32 add_to_module_list*/
PUBLIC INT32 add_to_module_list (treenode ** list_ptr, treenode * lib)
/* Takes a list of modules already attached to that processor,
   and a library entrypoint.
   returns the address of the library entrypoint
*/
{
	const BOOL allow_fpu_call = configuring;
	treenode *list;
	procdef_t *const procdef = get_procdef (lib, NOPOSN, FALSE, allow_fpu_call);
	module_t *const module = procdef->p_module;
	BOOL found = FALSE;
	INT32 module_address = 0;

	for (list = *list_ptr; !EndOfList (list) && !found; list_ptr = NextItemAddr (list), list = NextItem (list)) {
		module_t *thismodule = (module_t *) ThisItem (list);
		found = (module == thismodule);
		module_address -= thismodule->m_size;
	}

	if (!found) {		/* Not already on list */
		/* add to the end of the list */
		*list_ptr = addtofront ((treenode *) module, NULL);
		module_address -= module->m_size;
	}

	return (module_address + procdef->p_offset);
}

/*}}}*/
/*{{{  PUBLIC void write_linker_map*/
PUBLIC void write_linker_map (const char *const filename, const INT32 toplevel_size, const long int seek_pos, treenode * const module_list)
{
	/* Each line looks like:
	   SC filename (nnnnnnnn) nnnnnnnn nnnnnnnn : -
	   Hence we need enough string space for filenames plus 36 or so.
	   plus first line looks like:
	   LINKER TCOFF (blurb)
	 */
	const int len_per_line = 40;
	int len = 50 + (strlen (filename) + len_per_line);
	treenode *list = module_list;
	INT32 tot_size = toplevel_size;
	INT32 start;
	char *buf;
	char *ptr;

	for (; !EndOfList (list); list = NextItem (list)) {
		module_t *module = (module_t *) ThisItem (list);
		len += strlen (lookupfilename (module->m_filenum)) + len_per_line;
		tot_size += module->m_size;
	}

	buf = memalloc (len);
	sprintf (buf, "LINKER TCOFF (%s)\n", TOOL);
	/*sprintf(buf, "LINKER TCOFF\n"); */
	ptr = buf + strlen (buf);
	start = tot_size - toplevel_size;
	sprintf (ptr, "SC %s (%ld) %d %d : -\n", filename, seek_pos, start, start + toplevel_size - 1);
	ptr += strlen (ptr);

	for (list = module_list; !EndOfList (list); list = NextItem (list)) {
		const module_t *const module = (module_t *) ThisItem (list);
		start -= module->m_size;
		sprintf (ptr, "PRE %s (%ld) %d %d : -\n",
			 lookupfilename (module->m_filenum), module->m_seek_ptr, start, start + module->m_size - 1);
		ptr += strlen (ptr);
	}

	tcoff_putrec (objfile, COMMENT_TAG, "%ld%ld%s", FALSE, TRUE, buf);
	memfree (buf);
}

/*}}}*/

/*{{{  PUBLIC void write_debug_string(string, length)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  write_debug_string outputs a debug record to the object file.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void write_debug_string (const char string[], const int length)
{
	size_t written;
	long int rec_len;
	rec_len = tcoff_sizel (FALSE) + tcoff_sizel (FALSE) + tcoff_sizel ((long int) length) + (long int) length;
	tcoff_putl (objfile, COMMENT_TAG);
	tcoff_putl (objfile, rec_len);
	tcoff_putl (objfile, FALSE);
	tcoff_putl (objfile, FALSE);
	tcoff_putl (objfile, (long int) length);
	written = fwrite (string, sizeof (char), (size_t) length, objfile);
	if (written != length)
		write_error ();
}

/*}}}*/
/*{{{  PUBLIC void write_code_block_start*/
PUBLIC void write_code_block_start (size_t length)
{
	if (length > 0) {
		long int rec_len;
		DEBUG_MSG (("write_code_block_start: writing total of %u bytes\n", length));
		rec_len = tcoff_sizel ((long int) length) + (long int) length;
		tcoff_putl (objfile, LOAD_TEXT_TAG);
		tcoff_putl (objfile, rec_len);
		tcoff_putl (objfile, (long int) length);
		/* following calls to write_code_block will write the data */
	}
	code_size = length;
}

/*}}}*/
/*{{{  PUBLIC void write_code_block*/
PUBLIC void write_code_block (const size_t buflen, const BYTE * const buffer)
{
	DEBUG_MSG (("write_code_block: buflen is %u\n", buflen));
	if (buflen > 0) {
		/* The tag, etc, has already been written by write_code_block_start */
		const size_t written = fwrite (buffer, sizeof (char), buflen, objfile);
		if (written != buflen)
			write_error ();
	}
}

/*}}}*/
/*{{{  PUBLIC void write_end_module*/
PUBLIC void write_end_module (void)
{
	tcoff_putrec (objfile, END_MODULE_TAG, "");
}

/*}}}*/

/*{{{  PUBLIC long int saved_seek_position*/
PUBLIC long int saved_seek_position (void)
{
	return seek_position_of_startmodule;
}

/*}}}*/
/*}}}*/

/*{{{  linkage section name handling*/
/*{{{  PUBLIC setup_text_linkage_name*/
PUBLIC void setup_text_linkage_name (const wordnode * const name)
{
	DEBUG_MSG (("setup_text_linkage_name \"%s\"\n", WNameOf (name)));

	if (name == NULL)
		text_name = text_name_priority;
	else {
		const int len = WLengthOf (name);
		char *const local_text_name = memalloc (1 + len);
		memcpy (local_text_name, WNameOf (name), len + 1);	/* copy the trailing NUL byte too */
		text_name = local_text_name;
	}
}

/*}}}*/
/*{{{  PUBLIC const char *objwrt_get_section_name*/
PUBLIC const char *objwrt_get_section_name (void)
{
	return (object_file_wrt_flags & OBJ_FILE_WRT_PRI_TEXT_SECTION) ? text_name_priority : text_name;
}

/*}}}*/
/*}}}*/

/*{{{  init_obj_writer*/
PUBLIC void init_obj_writer (void)
{
	language_name = ((object_file_wrt_flags & OBJ_FILE_WRT_OCCAM_HARNESS) != 0) ? LANG_OCCAM_HARNESS : LANG_OCCAM;

	refno = 0;
}

/*}}}*/

/*{{{  code to write profile section*/
/*{{{  text buffer handling*/
#define BUFFER_SIZE 10240
#define TCOFF_BUFFER_EXTENSION_SIZE 80
/* textbuffer contains text which will be output using a LOAD_TEXT
   directive.  It is used for  the text of the profile section.
   bufferpos is the next free position in textbuffer */
PRIVATE unsigned char *textbuffer;
PRIVATE int bufferpos;

/*{{{  tcoff buffer handling*/
PRIVATE const long int bytes_to_tag[] = {
	0L,			/* not used */
	251L,			/* 1 byte  use 1 byte   */
	252L,			/* 2 bytes use 2 bytes  */
	253L,			/* 3 bytes use 4 bytes  */
	253L,			/* 4 bytes use ...      */
	254L,			/* 5 bytes use 8 bytes  */
	254L,			/* 6 bytes use ...      */
	254L,			/* 7 bytes use ...      */
	254L			/* 8 bytes use ...      */
};
/*}}}*/
/*{{{  typedef TcoffBuffer*/
typedef struct {
	unsigned char *data;
	size_t size;
	size_t index;
} TcoffBuffer;
/*}}}*/
/*{{{  PRIVATE TcoffBuffer *new_tcoff_buffer(void)*/
PRIVATE TcoffBuffer *new_tcoff_buffer (void)
{
	TcoffBuffer *new_buffer = (TcoffBuffer *) memalloc (sizeof (TcoffBuffer));
	new_buffer->data = (unsigned char *) memalloc (TCOFF_BUFFER_EXTENSION_SIZE);
	new_buffer->size = TCOFF_BUFFER_EXTENSION_SIZE;
	new_buffer->index = 0;
	return new_buffer;
}

/*}}}*/
/*{{{  PRIVATE void tcoff_putl_fn(void (*putfn)(void *, unsigned char), void *data, long int l)*/
PRIVATE void tcoff_putl_fn (void (*putfn) (void *, unsigned char), void *data, long int l)
{
	int size, i;
	long int n, bytes;
	if (l < 0L) {
		putfn (data, 255);
		l = ~l;
	}
	if ((0L <= l) && (l <= 250L))
		putfn (data, (unsigned char) l);
	else {
		size = 1;
		n = l >> 8;
		while (n != 0L) {
			n = n >> 8;
			size++;
		}
		putfn (data, (unsigned char) bytes_to_tag[size]);
		bytes = 1L << (bytes_to_tag[size] - 251);
		for (i = 0; i < bytes; i++) {
			putfn (data, (unsigned char) (l & 0xFFL));
			l = l >> 8;
		}
	}
}

/*}}}*/
/*{{{  PRIVATE void tcoff_putfn(void *data, unsigned char c)*/
PRIVATE void tcoff_putfn (void *data, unsigned char c)
{
	TcoffBuffer *buffer = (TcoffBuffer *) data;
	if (buffer->index >= buffer->size) {
		unsigned char *new_buffer = memalloc (buffer->size + TCOFF_BUFFER_EXTENSION_SIZE);
		memcpy (new_buffer, buffer->data, buffer->size);
		memfree (buffer->data);
		buffer->data = new_buffer;
		buffer->size += TCOFF_BUFFER_EXTENSION_SIZE;
	}
	buffer->data[buffer->index++] = c;
}

/*}}}*/
#if 0
/* not used */
/*{{{  PRIVATE void discard_tcoff_buffer(TcoffBuffer *buffer)*/
PRIVATE void discard_tcoff_buffer (TcoffBuffer * buffer)
{
	memfree (buffer->data);
	memfree (buffer);
}

/*}}}*/
#endif
/*}}}*/
/*{{{  PRIVATE void trace(char *str, ...)*/
PRIVATE void trace (const char *str, ...)
{
	va_list arglist;

	va_start (arglist, str);
	fputs ("Trace: ", outfile);
	vfprintf (outfile, str, arglist);
	va_end (arglist);
	putc ('\n', outfile);
}

/*}}}*/
/*{{{  PRIVATE void init_text_buffer(void)*/
PRIVATE void init_text_buffer (void)
{
	textbuffer = (unsigned char *) memalloc (BUFFER_SIZE);
	bufferpos = 0;
}

/*}}}*/
/*{{{  PRIVATE void free_text_buffer(void)*/
PRIVATE void free_text_buffer (void)
{
	memfree (textbuffer);
}

/*}}}*/
/*{{{  PRIVATE void flushtextbuffer (void)*/
PRIVATE void flushtextbuffer (void)
{
	if (bufferpos > 0) {
		const long int rec_len = tcoff_sizel ((long) bufferpos) + (long) bufferpos;
		tcoff_putl (objfile, LOAD_TEXT_TAG);
		tcoff_putl (objfile, rec_len);
		tcoff_putl (objfile, (long) bufferpos);
		fwrite (textbuffer, sizeof (char), bufferpos, objfile);
		bufferpos = 0;
	}
}

/*}}}*/
/*{{{  PRIVATE void write_text_block(bufptr, buffer)*/
/******************************************************************************
 *                                                                            *
 * write_text_block  Add numbytes of text from buffer to the textbuffer.      *
 *                   if the textbuffer gets full then flush it to the file.   *
 *                                                                            *
 ******************************************************************************/
PRIVATE void write_text_block (INT32 numbytes, const unsigned char buffer[])
{
	INT32 buffer_ptr = 0;
	/*{{{   trace */
	if (trace_prof_image)
		trace ("TEXT : %ld bytes", (long) numbytes);
	/*}}} */
	while (buffer_ptr < numbytes) {
		textbuffer[bufferpos++] = buffer[buffer_ptr++];
		if (bufferpos == BUFFER_SIZE)
			flushtextbuffer ();
	}
}

/*}}}*/
/*{{{  PRIVATE void bufwrt_int32value(INT32 value)*/
PRIVATE void bufwrt_int32value (INT32 value)
{
	int i;
	unsigned char ar[4];
	if (trace_prof_image)
		trace ("VALUE : %ld ", (long) value);
	for (i = 0; i < PROFTAB_INT32_SIZE; i++) {
		ar[i] = (unsigned char) (value & 0xff);
		value >>= 8;
	}
	write_text_block (PROFTAB_INT32_SIZE, ar);
}

/*}}}*/
/*{{{  PRIVATE void objwrt_proftab_hdr(ProfTab *prof_table)*/
PRIVATE void objwrt_proftab_hdr (ProfTab * prof_table)
{
	const StrPoolI section_name = strpool_add (prof_table->strpool, objwrt_get_section_name ());
	const StrPoolI sourcefile_index = strpool_add (profile_table->strpool,
						       sourcefilename);
	bufwrt_int32value (PROFTAB_VERSION | PROFTAB_CODE_OFFSETS_REL_TO_END);
	bufwrt_int32value ( /* PROFTAB_HEADER_SIZE */ 40);
	bufwrt_int32value ( /* PROFTAB_HEADER_SIZE */ 40 +
			   (prof_table->count_table_size * PROFTAB_ENTRY_SIZE) +
			   PROFTAB_STRPOOL_EXTRA + pad_to_mcword (strpool_size (prof_table->strpool)));
	bufwrt_int32value (
			   (((prof_table->contents & ProfLineInfo) ? PROFTAB_LINE_INFO : 0) |
			    ((prof_table->contents & ProfBranchInfo) ? PROFTAB_BRANCH_INFO : 0) |
			    ((prof_table->contents & ProfRoutineInfo) ? PROFTAB_ROUTINE_INFO : 0) |
			    ((prof_table->contents & ProfCallInfo) ? PROFTAB_CALL_INFO : 0)
			   ));
	bufwrt_int32value ((INT32) section_name);
	bufwrt_int32value (profile_count_table_label);
	bufwrt_int32value (PROFTAB_PRIORITY_ALL);
	bufwrt_int32value (prof_table->count_table_size);
	bufwrt_int32value (PROFTAB_ENTRY_SIZE);
	bufwrt_int32value (sourcefile_index);
}

/*}}}*/
/*{{{  PRIVATE void objwrt_buffer(INT32 len, char *buffer) */
PRIVATE void objwrt_strbuffer (INT32 len, const char *buffer)
{
	if (len > 0) {
		const long int rec_len = tcoff_sizel ((long) len) + (long) len;
		tcoff_putl (objfile, LOAD_TEXT_TAG);
		tcoff_putl (objfile, rec_len);
		tcoff_putl (objfile, (long) len);
		fwrite (buffer, sizeof (char), len, objfile);
	}
}

/*}}}*/
/*{{{  PRIVATE void objwrt_proftab_strpool (ProfTab *prof_table)*/
PRIVATE void objwrt_proftab_strpool (ProfTab * prof_table)
{
	StrPoolI i;
	char *pad_buffer;
	const StrPoolI pool_size = strpool_size (prof_table->strpool);
	const StrPoolI padded_pool_size = (StrPoolI) pad_to_mcword (pool_size);
	const StrPoolI padding_size = padded_pool_size - pool_size;
	const char *pool_image = strpool_image (prof_table->strpool);
	bufwrt_int32value (padded_pool_size);
	flushtextbuffer ();
	objwrt_strbuffer (pool_size, pool_image);
	if (padding_size > 0) {
		pad_buffer = (char *) memalloc (padded_pool_size);
		for (i = 0; i < padding_size; i++)
			pad_buffer[i] = 0;
		objwrt_strbuffer (padding_size, pad_buffer);
	}
}

/*}}}*/
/*{{{  PUBLIC void write_profile_section(ProfTab *prof_table)*/
PUBLIC void write_profile_section (ProfTab * prof_table)
{
	ProfTabEntry *pe;
	long int profile_section_id;
	int end_code_section_label = 0;
	ProfTabEntry *pe_ls = prof_table->entry_list_head;
	TcoffBuffer *expr_buffer = new_tcoff_buffer ();
	if (cgraph_profiling)
		/* to resolve the addresses of separately compiled routines */
	{
		end_code_section_label = p_symbol_id (objfile, 0L, LOCAL_USAGE, "end%code%text", INVALID_SYMBOL_ID);
		tcoff_putrec (objfile, DEFINE_LABEL_TAG, "%ld", end_code_section_label);
	}
	profile_section_id = p_symbol_id (objfile, DEBUG_SECTION, EXPORT_USAGE, PROFILE_SECTION_NAME, INVALID_SYMBOL_ID);
	tcoff_putrec (objfile, SET_LOAD_POINT_TAG, "%ld", profile_section_id);
	init_text_buffer ();
	objwrt_proftab_hdr (prof_table);
	for (pe = pe_ls; pe != NULL; pe = proftab_next_ (pe))
		switch (proftab_tag_ (pe)) {
		case ProfLineCount:
			bufwrt_int32value (PROFILE_LINE_COUNT);
			bufwrt_int32value (proftab_file_name_ (pe));
			bufwrt_int32value (proftab_line_number_ (pe));
			break;
		case ProfRoutineCount:
			bufwrt_int32value (PROFILE_ROUTINE_COUNT);
			bufwrt_int32value (get_proftab_routine_address (profile_table, proftab_tptr_ (pe)));
			bufwrt_int32value (0);
			break;
		case ProfCallCount:
			{
				treenode *called_rout = INameOf (proftab_tptr_ (pe));
				bufwrt_int32value (PROFILE_CALL_COUNT);
				bufwrt_int32value (get_proftab_routine_address (profile_table, proftab_calling_nptr_ (pe)));
				if (!separatelycompiled (called_rout))
					bufwrt_int32value (get_proftab_routine_address (profile_table, called_rout));
				else {
					/* LOAD_EXPR (record length) size expression */
					long int rec_len;
					int called_rout_label;
					const BOOL allow_fpu_call = configuring;
					int origin_id = INVALID_SYMBOL_ID;
					wordnode *extern_name = translate_from_internal (NNameOf (called_rout));
					module_t *module = get_procdef (called_rout, NOPOSN, FALSE,
									allow_fpu_call)->p_module;
					/*{{{  Set up the correct origin symbol value */
					if (object_file_wrt_flags & OBJ_FILE_WRT_NO_IMPORT_ORIGINS)
						DEBUG_MSG (("Not importing names\n"));
					else if (module->m_id_val == ORIGIN_NOT_FOUND)
						/* imported from a lib, but no origin there */
						DEBUG_MSG (("No origin\n"));
					else {	/* there is an origin for this entryname */

						if (module->m_id_val == ORIGIN_FOUND) {	/* not yet imported */
							DEBUG_MSG (("First time found: \"%s\", set to %d, ", WNameOf (module->m_name), refno));
							module->m_id_val = p_symbol_id (objfile, 0L,
											IMPORT_USAGE | ORIGIN_USAGE, WNameOf (module->m_name),
											INVALID_SYMBOL_ID);
						}
						origin_id = module->m_id_val;
					}
					/*}}} */
					called_rout_label = p_symbol_id (objfile, 0L, IMPORT_USAGE, WNameOf (extern_name), origin_id);
					flushtextbuffer ();

					expr_buffer->index = 0;
					tcoff_putl_fn (tcoff_putfn, expr_buffer, MINUS_OP);
					tcoff_putl_fn (tcoff_putfn, expr_buffer, SV_VALUE_TAG);
					tcoff_putl_fn (tcoff_putfn, expr_buffer, called_rout_label);
					tcoff_putl_fn (tcoff_putfn, expr_buffer, SV_VALUE_TAG);
					tcoff_putl_fn (tcoff_putfn, expr_buffer, end_code_section_label);
					rec_len = tcoff_sizel (PROFTAB_INT32_SIZE) + expr_buffer->index;
					tcoff_putl (objfile, LOAD_EXPR_TAG);
					tcoff_putl (objfile, rec_len);
					tcoff_putl (objfile, PROFTAB_INT32_SIZE);
					fwrite (expr_buffer->data, sizeof (unsigned char), expr_buffer->index, objfile);
					if (trace_prof_image) {
						trace ("LOAD_EXPR rec_len = %ld size = %ld", (long) rec_len, (long) PROFTAB_INT32_SIZE);
						trace ("Expr. buffer length = %ld", expr_buffer->index);
					}
				}
				break;
			}
		};
	flushtextbuffer ();
	/* discard_tcoff_buffer(expr_buffer); */
	objwrt_proftab_strpool (prof_table);
	free_text_buffer ();
}

/*}}}*/
