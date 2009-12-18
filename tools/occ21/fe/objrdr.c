/* $Id: objrdr.c,v 1.3 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	occam 2 object file reading
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
#define USELIBNAME		/* save library name for imports in .tco file */

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "feinc.h"

#include "objrdr.h"
#include "objlib.h"
#include "objtrans.h"

#include "tcoff.h"		/* IMPORTED */
#include "tcofflib.h"		/* IMPORTED */
#include "popen_re.h"		/* IMPORTED */

#include "lexdef.h"
#include "lexerror.h"
#include "synerror.h"
#include "chkerror.h"
#include "syndef.h"
#include "chkdef.h"
#include "usehdr.h"
#include "usedef.h"
/*}}}  */

/*{{{  constant definitions */
/*#define MAX_NESTED_SCS 50*//* #SC is now obsolete */
/*#define MAX_ENTRYPOINTS 200*//* No longer hard-coded */

/* we run the configurer after linking, so all the object files will
   be linked already, and we have to generate an object file
   which looks like it has been linked.
*/
#define LINKED_INPUT_FILE ((current_fe_data->fe_lang & (FE_LANG_CONFIG2 | FE_LANG_CONFIG3)) != 0)

/*{{{  #SC (obsolete) */
#if 0				/* #SC is obsolete */
typedef struct			/* used for remembering nested SCs (obsolete) */ {
	int sc_filenum;		/* Index into file table for file containing SC code */
	BIT32 sc_codesize;	/* Length of SC code, in bytes */
	treenode *sc_entrypoints;	/* List of SC entrypoints */
} sc_t;
#endif /* 0 */
/*}}}  */

/*}}}  */

/*{{{  private variables */
/*{{{  #SC support (obsolete) */
#if 0				/* #SC is obsolete */
PRIVATE sc_t sctable[MAX_NESTED_SCS];	/* list of all #SC stuff */
PRIVATE int sctableptr;		/* (obsolete)             */
#endif /* 0 */

/*}}}  */

/* The parser builds up a list of the entrypoints onto here, so that we
   can patch back the workspace and vectorspace requirements.
   It was also (historically) used to patch #SC offsets */
/* This is `valid' all the time we're reading a library, until
   patchdescriptors has been called */
PRIVATE treenode *entrypointlist;

/* Global list of #USEd names, used to find modules when writing object file */
/* These are `valid' all the time we're reading a library, until
   patchdescriptors has been called */
PRIVATE libentry_t *libentries;	/* List for just this library when reading */
PRIVATE module_t *localmodules;	/* Just this library ditto */
#ifdef USELIBNAME
PRIVATE wordnode *libwname;	/* library file name */
#endif
PRIVATE long int current_module_seekpos;	/* Used as pseudo seek_val if not in a lib */
PRIVATE BIT32 module_instr;	/* details of current module */
PRIVATE BIT32 module_attr;
PRIVATE BOOL in_descriptor;
PRIVATE int descriptor_indent;	/* Use cos new descriptors don't need spaces */
PRIVATE char *current_descriptor;
PRIVATE char *sub_desc;
PRIVATE BOOL reading_module;
PRIVATE int module_level;
PRIVATE BOOL library_eof;

/* Linked input files only: */
PRIVATE INT32 current_symbol_value;
PRIVATE INT32 current_module_size;
PRIVATE module_t *current_module;
PRIVATE BOOL file_is_a_library;
PRIVATE BOOL in_linked_text_section;
PRIVATE BOOL already_read_a_text_segment;
PRIVATE const char *const linked_text_section_name = "linked%text";
/*}}}  */

/*{{{  low level tcoff Input & supporting functions */
/*{{{  PRIVATE long int           getl_from_tcoff (fs) */
PRIVATE long int getl_from_tcoff (FILE * fs)
{
	int ok;
	const long int n = tcoff_getl_test (fs, &ok);
	if (!ok)
		lexfatal (LEX_EOF, /*NOPOSN*/ flocn);	/* bug 1362 13/8/91 */
	return (n);
}

/*}}}  */
/*{{{  PRIVATE unsigned long int getul_from_tcoff (fs) */
PRIVATE unsigned long int getul_from_tcoff (FILE * fs)
{
	int i, c;
	unsigned long int l;
	l = 0;
	for (i = 0; i < 4; i++) {
		c = getc (fs);
		if (feof (fs))
			lexfatal (LEX_EOF, /*NOPOSN*/ flocn);	/* bug 1362 13/8/91 */
		else
			c &= 0xFF;
		l = l | (((unsigned long int) c) << (8 * i));
	}
	return (l);
}

/*}}}  */
/*{{{  PRIVATE char          *getbytes_from_tcoff (fs, len) */
PRIVATE char *getbytes_from_tcoff (FILE *fs, const int len)
{
	int i, c;
	char *str = memalloc (sizeof (char) * len + 1);
	for (i = 0; i < len; i++) {
		c = fgetc (fs);
		if (feof (fs))
			lexfatal (LEX_EOF, /*NOPOSN*/ flocn);	/* bug 1362 13/8/91 */
		str[i] = c & 0xFF;
	}
	str[i] = '\0';
	return str;
}

/*}}}  */
/*{{{  PRIVATE void        throw_bytes_from_tcoff (fs, len) */
PRIVATE void throw_bytes_from_tcoff (FILE *fs, const long int l)
{
	const int res = fseek (fs, l, SEEK_CUR);
	if (res != 0) {
		lexfatal (LEX_EOF, /*NOPOSN*/ flocn);
	}
}

/*}}}  */
/*{{{  PRIVATE void       throw_record_from_tcoff (fs) */
PRIVATE void throw_record_from_tcoff (FILE *fs)
{
	const long int l = getl_from_tcoff (fs);
	throw_bytes_from_tcoff (fs, l);
}

/*}}}  */
/*}}}  */

/*{{{  PRIVATE BOOL process_externalname */
#ifdef MOBILES
PRIVATE BOOL process_externalname (wordnode *name,
		      const long int seek_val, const BIT32 ws, const BIT32 vs, const BIT32 ms,
		      const BIT32 instr, const BIT32 attr, const BIT32 hash, const int in_libindex)
#else
PRIVATE BOOL process_externalname (wordnode *name,
		      const long int seek_val, const BIT32 ws, const BIT32 vs,
		      const BIT32 instr, const BIT32 attr, const BIT32 hash, const int in_libindex)
#endif
{
	libentry_t *libentry;
	procdef_t *procdef;
	module_t *module;
	const BOOL compatible = compatible_call (instr, attr);
	DEBUG_MSG (("process_externalname: name is \"%s\", seek_val: %ld, ws %ld, vs %ld\n", WNameOf (name), seek_val, ws, vs));
	if (!compatible) {
		DEBUG_MSG (("instr/attr is %08X/%08X, should be %08X/%08X\n", instr, attr, current_fe_data->fe_txlib->ptype, current_fe_data->fe_txlib->pattr));
	}

	/*{{{  Find this entrypoint's libentry record */
	libentry = search_libentries (libentries, name);
	if (libentry == NULL) {
		DEBUG_MSG (("Creating a libentry node; adding to libentries\n"));
		/* use newvec here rather than memalloc, cos we never free the space */
		libentry = (libentry_t *) newvec (sizeof (libentry_t));
		libentry->l_name = name;
		libentry->l_procdefs = NULL;
		libentry->l_bits = 0;
		libentry->l_hash = hash;
		libentry->l_next = libentries;
		libentries = libentry;
	} else if (libentry->l_hash != hash)
		/*synwarn1(SYN_DIFFERENT_DESCRIPTORS, flocn, (BIT32)(WNameOf(name))); */
		synerr_s (SYN_DIFFERENT_DESCRIPTORS, flocn, WNameOf (name));

	libentry->l_bits |= (compatible * LIBENTRY_BIT_COMPATIBLE);
	/*}}}  */
	/*{{{  Find this entrypoint's module */
	module = search_modulechain (localmodules, seek_val);
	if (module == NULL) {
		DEBUG_MSG (("Creating a module node\n"));
		/* use newvec here rather than memalloc, cos we never free the space */
		module = (module_t *) newvec (sizeof (module_t));
		module->m_name = lookupword ("?", 1);
		module->m_seek_ptr = seek_val;
		module->m_id_val = ORIGIN_NOT_FOUND;
		module->m_filenum = currentfilenum;
		module->m_instr = instr;
		module->m_attr = attr;
		module->m_size = 0;
		module->m_config = NULL;
		module->m_filestr = NULL;
		module->m_next = localmodules;
		localmodules = module;
	}
	if (ws == ORIGIN_WS) {
		DEBUG_MSG (("Marking origin as found\n"));
		module->m_name = name;
		module->m_id_val = ORIGIN_FOUND;
	}
	/*}}}  */
	/*{{{  Set this module's offset */
	if (LINKED_INPUT_FILE) {
		current_module = module;

		if (file_is_a_library && !in_libindex) {
			procdef_t *this_procdef = libentry->l_procdefs;
			while ((this_procdef != NULL) && (this_procdef->p_module->m_seek_ptr != seek_val))
				this_procdef = this_procdef->p_next;
			if (this_procdef != NULL) {
				DEBUG_MSG (("Setting offset to %ld\n", current_symbol_value));
				this_procdef->p_offset = current_symbol_value;	/* value comes before descriptor */
			}
			return TRUE;	/* don't send the descriptor */
		}
	}
	/*}}}  */
	/*{{{  Save this processor type's workspace and vectorspace etc */
	/* If we're compiling rather than configuring, just add the FIRST
	   compatible definition, to save a little space (and time later) */
	/* (was SINGLE_PROCDEF) */
	if (((current_fe_data->fe_lang & FE_LANG_OCCAM) == 0) || ((libentry->l_procdefs == NULL) && compatible)) {
		procdef = (procdef_t *) newvec (sizeof (procdef_t));

		procdef->p_ws = ws;
		procdef->p_vs = vs;
#ifdef MOBILES
		procdef->p_ms = ms;
#endif
		if (LINKED_INPUT_FILE) {
			procdef->p_offset = current_symbol_value;	/* value comes before descriptor */
		}
		procdef->p_module = module;
		procdef->p_next = libentry->l_procdefs;
		libentry->l_procdefs = procdef;
	}
	/*}}}  */
	/*{{{  Send the descriptor to the lexer if necessary */
	if ((ws != ORIGIN_WS) && ((libentry->l_bits & LIBENTRY_BIT_DESC_SENT) == 0)
	    /* when configuring, we MUST read all libs, since we don't yet know
	       what processor types will be needed */
	    && (((current_fe_data->fe_lang & (FE_LANG_CONFIG2 | FE_LANG_CONFIG3)) != 0) || compatible || current_fe_data->fe_read_all_libs)
		) {
		libentry->l_bits |= LIBENTRY_BIT_DESC_SENT;
		return FALSE;
	} else
		return TRUE;

	/*}}}  */
}

/*}}}  */
/*{{{  PRIVATE wordnode *desc_name */
PRIVATE wordnode *desc_name (char *desc, int *startptr)
/* This parses a descriptor to find the symbol name */
{
	char *str;
	char *end;
	char missing_bit;
	wordnode *name;

#if 0
fprintf (stderr, "desc_name: desc: %s\n", desc);
#endif
	/* ignore MOBILE PROC here for now -- treat as regular PROC */
	str = (char *)strstr (desc, "PROC ");
	if (str == desc) {
		/* bingo */
		/* could have said (*str != NULL) but 'PROC ' might occur in
		   a function name ?! */
		str += 5;	/* 5 is strlen("PROC "); */
	} else {
		str = (char *) strstr (desc, " FUNCTION ");
		if (str != NULL) {	/* bingo */
			str += 10;	/* 10 is strlen(" FUNCTION "); */
		} else if ((char *)strstr (desc, "MOBILE PROC ") == desc) {
			str = desc + 12;
		} else {
			return (NULL);
		}
	}
	while (*str == ' ') {
		str++;
	}
	end = str;
	while ((*end != ' ') && (*end != '(') && (end != '\0')) {
		end++;
	}
	if (*end == '\0') {
		return (NULL);
	}

	missing_bit = *end;
	*end = '\0';
	name = lookupword (str, end - str);
	*end = missing_bit;
	*startptr = str - desc;
	return (name);
}

/*}}}  */
/*{{{  PRIVATE BOOL is_occam_syntax */
PRIVATE BOOL is_occam_syntax (const char *s)
{
	if (isalpha (*s)) {
		for (s++; *s != '\0'; s++)
			if (!isalnum (*s) && ((*s) != '.'))
				return (FALSE);
		return (TRUE);
	}
	return (FALSE);
}

/*}}}  */
/*{{{  PRIVATE char *process_descstring (infile) */
PRIVATE char *process_descstring (FILE * input_file, const long int seek_val, const BOOL in_libindex, const BIT32 instr, const BIT32 attr, const BOOL in_stdlib)
{
	int len;
	int index;
	long int ws, vs;
#ifdef MOBILES
	long int ms;
#endif
	wordnode *name;
	char *descriptor;
	BIT32 hash = 0xdeaddead;

	DEBUG_MSG (("process_descstring: seek_val: %ld, in_libindex: %d\n", seek_val, in_libindex));

	/*{{{  Read a descriptor from the input file */
	len = (int) getl_from_tcoff (input_file);
	ws = getl_from_tcoff (input_file);
	vs = getl_from_tcoff (input_file);
#ifdef MOBILES
	ms = getl_from_tcoff (input_file);
	len -= ((int)tcoff_sizel (ws) + (int)tcoff_sizel (vs) + (int)tcoff_sizel (ms));
#else
	len -= ((int)tcoff_sizel (ws) + (int)tcoff_sizel (vs));
#endif
	descriptor = getbytes_from_tcoff (input_file, len);
	/*}}}  */

	/*{{{  Get the name from the descriptor */
	if (ws == ORIGIN_WS) {	/* Special ORIGIN descriptor */
		char *str;
#ifdef USELIBNAME
		char *longstr;
		int longlen;
#endif
		if (!in_libindex) {	/* ignore origin descriptors unless in lib index */
			memfree (descriptor);
			return (NULL);
		}
		len = (int) getl_from_tcoff (input_file);
		str = getbytes_from_tcoff (input_file, len);
#ifdef USELIBNAME
		if (libwname != NULL) {
			longlen = len + strlen (WNameOf (libwname)) + 2;
			longstr = memalloc (sizeof (char) * (longlen + 1));
			sprintf (longstr, "{%s}%s", WNameOf (libwname), str);
			name = lookupword (longstr, longlen);
			DEBUG_MSG (("Found an exported library ORIGIN (%s)\n", longstr));
			memfree (longstr);
		} else
#endif
		{
			name = lookupword (str, len);
			DEBUG_MSG (("Found an exported ORIGIN (%s)\n", str));
		}
		memfree (str);
		index = 0;
	} else {
		if (in_libindex) {
			throw_record_from_tcoff (input_file);	/* string */
		}
		name = desc_name (descriptor, &index);	/* index is used for translating */
	}
	if (name == NULL) {
		lexfatal_s (LEX_TCOFF_BAD_DESC, flocn, descriptor);
	}
	/*}}}  */

	/*{{{  Check that standard libraries don't have vectorspace */
	if (in_stdlib && (vs != 0L)) {
		/*synerror (SYN_STDLIB_HAS_VS, liblocn, (BIT32)(WNameOf(name))); */
		/*genwarn_s(GEN_STDLIB_HAS_VS, WNameOf(name)); */
		msg_out_s (SEV_WARN, SYN, SYN_STDLIB_HAS_VS, NOPOSN, WNameOf (name));
	}
	/*}}}  */

	/*{{{  Process and translate the name */
	{
		/* This modifies the descriptor if it was translated by #PRAGMA TRANSLATE */
		descriptor = translate_descriptor (&name, index, descriptor, len);

		if ((ws != ORIGIN_WS) && !in_stdlib && !is_occam_syntax (WNameOf (name))) {
			throwawayname (name);	/* Simply ignore it */
			memfree (descriptor);
			DEBUG_MSG (("Ignoring name %s\n", WNameOf (name)));
			return (NULL);
		}
	}
	/*}}}  */

	/*{{{  Calculate the descriptor's hash value */
	{
		char *s = descriptor;
		for (; (*s) != '\0'; s++)
			hash = (hash << 1) ^ (*s);
	}
	/*}}}  */

	/*{{{  Abort if this one should not be processed */
#ifdef MOBILES
	if (process_externalname (name, seek_val, ws, vs, ms, instr, attr, hash, in_libindex))	/* This one can be junked */
#else
	if (process_externalname (name, seek_val, ws, vs, instr, attr, hash, in_libindex))	/* This one can be junked */
#endif
	{
		memfree (descriptor);
		return (NULL);
	}
	/*}}}  */

#if 0
fprintf (stderr, "process_descstring(): returning: %s\n", descriptor);
#endif
	return (descriptor);
}

/*}}}  */

/*{{{  PUBLIC FILE *open_descfile(name)         for the lexer */
/*{{{  comment */
/*****************************************************************************
 *
 *  open_descfile opens the file with name 'name' as a descriptor file
 *                'mode' gives the descriptor file mode, either LEX_LIB,
 *                LEX_STDLIB or LEX_SC.
 *
 *****************************************************************************/
/*}}}  */
PUBLIC FILE *open_descfile (const char *name, const int mode)
{
	FILE *fptr = NULL;
	(void) mode;		/* stop warning */
	DEBUG_MSG (("open_descfile: %s\n", name));
#if 0				/* sctable no longer used */
	if (sctableptr >= MAX_NESTED_SCS)
		lexfatal_s (LEX_TOO_MANY_NESTED_SCS, flocn, name);
	else
#endif
	{
		fptr = popen_relative (name, current_fe_data->fe_pathname, NULL, "rb", NULL, memalloc, memfree, NULL);
		if (fptr != NULL) {
#if 0
			/* playing around on a Sun; it seems that any non-default
			   buffering just goes slower.
			 */
			{
				const int err = setvbuf (fptr, memalloc (1024), _IOFBF, 1024);
				if (err)
					abort ();
			}
#endif
			library_eof = FALSE;
			{
				long int tag = getl_from_tcoff (fptr);
				if (LINKED_INPUT_FILE) {
					if (tag != LINKED_UNIT_TAG)
						lexfatal_s (LEX_TCOFF_NOT_LINKED, flocn, name);
				} else {
					if (tag != LINKABLE_TAG)
						lexfatal_s (LEX_TCOFF_WRONG_FORMAT, flocn, name);
				}
				throw_record_from_tcoff (fptr);
			}
			module_level = 0;
			in_descriptor = FALSE;
			reading_module = FALSE;
#ifdef USELIBNAME
			libwname = NULL;	/* will be set up by LIB_INDEX_START */
#endif
			file_is_a_library = FALSE;
		}
	}
	return (fptr);
}

/*}}}  */
/*{{{  PUBLIC char *readdescriptorline (line)   for the lexer */
/*{{{  comment */
/*****************************************************************************
 *
 *  readdescriptorline reads a line of occam source from the infile, returns
 *                     a pointer to the line, or NULL if eof or error.
 *                     Any descriptor information found is stored on the
 *                     descriptor stack.
 *
 *****************************************************************************/
/*}}}  */
PUBLIC char *readdescriptorline (char *line, const BOOL in_stdlib, int *const line_len)
{
	DEBUG_MSG (("readdescriptorline: "));
	if (!library_eof && !in_descriptor) {
		/*{{{  get new descriptor */
		BOOL ok = TRUE;
		BOOL finished = FALSE;
		DEBUG_MSG (("Looking for next descriptor\n"));
		while (ok && !finished) {
			BOOL skip_this_record = FALSE;
			INT32 x = tcoff_getl_test (infile, &ok);
			/*DEBUG_MSG(("S,T: %ld, %ld, ", ftell(infile), x)); */
			if (ok)
				switch ((int) x) {
#ifdef USELIBNAME
					/*{{{  case LIB_INDEX_START_TAG:   record library file name */
				case LIB_INDEX_START_TAG:
					{
						const char *libname = fe_lookupfilename (current_fe_handle, currentfilenum);
						libwname = lookupword (libname, strlen (libname)), throw_record_from_tcoff (infile);
						DEBUG_MSG (("Index start %s\n", libname));
						break;
					}
					/*}}}  */
#endif
					/*{{{  case INDEX_ENTRY_TAG      check if valid entrypoint */
				case INDEX_ENTRY_TAG:
					{
						INT32 lang, seek_val;
						BIT32 instr, attr;
						DEBUG_MSG (("Found an index entry\n"));
						file_is_a_library = TRUE;
						(void) getl_from_tcoff (infile);	/* length */
						seek_val = getul_from_tcoff (infile);	/* position */
						instr = getl_from_tcoff (infile);	/* transputer function */
						attr = getl_from_tcoff (infile);	/* error mode */
						lang = getl_from_tcoff (infile);	/* language */
						/*{{{  get descriptor                     descriptor */
						if ((lang == LANG_OCCAM) /* && compatible_call(instr, attr) */ ) {
							current_descriptor = process_descstring (infile, seek_val, TRUE, instr, attr, in_stdlib);
							if (current_descriptor != NULL) {
								in_descriptor = TRUE;
								descriptor_indent = 0;
								finished = TRUE;
							}
						} else {
							throw_record_from_tcoff (infile);	/* descriptor */
							throw_record_from_tcoff (infile);	/* string */
						}
						/*}}}  */
					}
					break;
					/*}}}  */
					/*{{{  case START_MODULE_TAG:    lex level++; check if valid module */
				case START_MODULE_TAG:
					{
						BIT32 instr, attr;
						long int seekpos = ftell (infile);
						module_level++;
						DEBUG_MSG (("Found startmodule, level is now: %d\n", module_level));
						(void) getl_from_tcoff (infile);	/* length */
						instr = getl_from_tcoff (infile);	/* transputer function */
						attr = getl_from_tcoff (infile);	/* error mode */
						(void) getl_from_tcoff (infile);	/* language */
						throw_record_from_tcoff (infile);	/* string */

						if (module_level == 1) {	/* ignore nested modules */
							current_module_seekpos = seekpos - tcoff_sizel (START_MODULE_TAG);
							reading_module = /*compatible_call(module_instr, module_attr) */ TRUE;
							module_instr = instr;
							module_attr = attr;
							current_module_size = 0;
							current_module = NULL;
							in_linked_text_section = FALSE;
							already_read_a_text_segment = FALSE;
						}
					}
					break;
					/*}}}  */
					/*{{{  case SYMBOL_TAG           (looking for origins) */
				case SYMBOL_TAG:
					/* We don't need to bother about SPECIFIC_SYMBOL_TAG cos we're only
					   interested in actual origin symbols here, which are always defined
					   using a SYMBOL_TAG */
					if (reading_module && (module_level == 1)) {
						(void) getl_from_tcoff (infile);	/* length */
						x = getl_from_tcoff (infile);	/* usage */
						if ((x | (EXPORT_USAGE | ORIGIN_USAGE)) == x) {
							char *name;
							DEBUG_MSG (("Found an exported ORIGIN tag\n"));
							x = getl_from_tcoff (infile);	/* len */
							name = getbytes_from_tcoff (infile, (int) x);
#ifdef MOBILES
							(void) process_externalname (lookupword (name, strlen (name)),
										     current_module_seekpos, ORIGIN_WS, 0, 0,
										     module_instr, module_attr, 0, FALSE);
#else
							(void) process_externalname (lookupword (name, strlen (name)),
										     current_module_seekpos, ORIGIN_WS, 0,
										     module_instr, module_attr, 0, FALSE);
#endif
							memfree (name);
						} else
							throw_record_from_tcoff (infile);	/* string */
					} else
						throw_record_from_tcoff (infile);
					break;
					/*}}}  */
					/*{{{  case DESCRIPTOR_TAG:      load descriptor (if in valid module) */
				case DESCRIPTOR_TAG:
					if (reading_module && (module_level == 1)) {
						INT32 lang;
						DEBUG_MSG (("Found a descriptor\n"));
						(void) getl_from_tcoff (infile);	/* length */
						(void) getl_from_tcoff (infile);	/* id */
						lang = getl_from_tcoff (infile);	/* language */
						/*{{{  get descriptor            descriptor */
						if (lang == LANG_OCCAM) {
							current_descriptor = process_descstring (infile, current_module_seekpos, FALSE,
												 module_instr, module_attr, in_stdlib);
							if (current_descriptor != NULL) {
								in_descriptor = TRUE;
								descriptor_indent = 0;
								finished = TRUE;
							}
						} else {
							throw_record_from_tcoff (infile);
						}
						/*}}}  */
					} else {
						throw_record_from_tcoff (infile);
					}
					break;
					/*}}}  */
					/*{{{  case END_MODULE_TAG:      lex level--; */
				case END_MODULE_TAG:
					module_level--;
					DEBUG_MSG (("Found endmodule, level is now %d\n", module_level));
					if (module_level < 0)
						lexfatal (LEX_TCOFF_UNMATCHED_ENDMODULE, flocn /*0 */ );
					throw_record_from_tcoff (infile);
					if (LINKED_INPUT_FILE)
						if ((module_level == 0) && (current_module != NULL))
							current_module->m_size = current_module_size;
					break;
					/*}}}  */
					/*{{{  case LIB_INDEX_END_TAG:   stop reading */
				case LIB_INDEX_END_TAG:
					throw_record_from_tcoff (infile);
					if (!LINKED_INPUT_FILE)
						ok = FALSE;	/* stop reading at the end of the index */
					break;
					/*}}}  */
					/*{{{  case DEFINE_SYMBOL_TAG: */
				case DEFINE_SYMBOL_TAG:
					if (LINKED_INPUT_FILE) {
						long int expression;
						(void) getl_from_tcoff (infile);	/* length */
						(void) getl_from_tcoff (infile);	/* id */
						expression = getl_from_tcoff (infile);	/* CO_VALUE_TAG */
						if (expression != CO_VALUE_TAG)
							err_abort ("readdescriptorline");
						current_symbol_value = getl_from_tcoff (infile);
					} else
						skip_this_record = TRUE;
					break;
					/*}}}  */
					/*{{{  case LOAD_TEXT_TAG: */
				case LOAD_TEXT_TAG:
					if (LINKED_INPUT_FILE && in_linked_text_section && !already_read_a_text_segment) {
						long int len;
						(void) getl_from_tcoff (infile);	/* record length */
						len = getl_from_tcoff (infile);	/* code length */
						throw_bytes_from_tcoff (infile, len);	/* the code */
						current_module_size += len;
						already_read_a_text_segment = TRUE;
						DEBUG_MSG (("Found a LOAD_TEXT tag: size %ld, current size is now: %ld\n", len, current_module_size));
					} else
						skip_this_record = TRUE;
					break;
					/*}}}  */
					/*{{{  case SECTION_TAG          (looking for linked text section) */
				case SECTION_TAG:
					if (LINKED_INPUT_FILE) {
						(void) getl_from_tcoff (infile);	/* length */
						x = getl_from_tcoff (infile);	/* type   */
						(void) getl_from_tcoff (infile);	/* usage  */
						if ((x & EXECUTE_SECTION) != 0) {
							char *name;
							x = getl_from_tcoff (infile);	/* len */
							name = getbytes_from_tcoff (infile, (int) x);
							DEBUG_MSG (("Found an EXECUTABLE section \"%s\"\n", name));
							in_linked_text_section = strcmp (name, linked_text_section_name) == 0;
							memfree (name);
						} else
							throw_record_from_tcoff (infile);	/* string */
					} else
						throw_record_from_tcoff (infile);
					break;
					/*}}}  */
					/*{{{  default:  throw away all other records */
				default:
					skip_this_record = TRUE;
					break;
					/*}}}  */
				}
			if (skip_this_record)
				throw_record_from_tcoff (infile);
		}
		if (!ok)
			library_eof = TRUE;
		sub_desc = current_descriptor;
		/*}}}  */
	}

	if (library_eof) {
		/*{{{  return null, finished getting descriptors */
		DEBUG_MSG (("Returning NULL\n"));
		*line_len = 0;
		return (NULL);
		/*}}}  */
	} else {
		/*{{{  return next line of descriptor */
		/*{{{  comment */
		/* Descriptors have the format: prototype (including formal param list)
		   which may be spread over multiple lines,
		   keyword (either SEQ or PRI PAR)
		   channel usage (eg c? or d!)
		   colon (:)

		   The compiler wants to see indentation like this:
		   PROC p (params,
		   more params)
		   SEQ
		   chan?
		   chan!
		   :

		   This is how descriptors always used to be generated in the past.
		   However, we now generate them like so, to save space in the object file:

		   PROC p (params,
		   more params)
		   SEQ
		   chan?
		   chan!
		   :--optional keywords
		   more optional stuff

		   We therefore have to re-insert the indentation here, if we've found
		   one of the new ones. We must still be compatible with the old ones.
		   We use a variable 'descriptor_indent' to control this.
		 */
		/*}}}  */
		const char *const end = strchr (sub_desc, '\n');
		const int len = (end == NULL) ? strlen (sub_desc) : end - sub_desc;
		/* len now is the length of this line in the descriptor */

		/* descriptor indent holds the expected indentation for this line */

		/* If there are already spaces there, or if we've hit the colon,
		   no more indentation is required.
		 */
		if ((sub_desc[0] == ' ') || (sub_desc[0] == ':'))
			descriptor_indent = 0;

		memset (line, ' ', descriptor_indent);	/* insert the current indentation */
		memcpy (&line[descriptor_indent], sub_desc, len);
		line[descriptor_indent + len] = '\n';
		line[descriptor_indent + len + 1] = '\0';
		*line_len = descriptor_indent + len + 1;

		if ((end == NULL) || (sub_desc[0] == ':')) {
			/* bug TS/1983 17/12/92 */
			/* Look for extra attributes */
			if (sub_desc[0] == ':' && sub_desc[1] == '-') {
				if (strstr (sub_desc, tagstring (S_TIMER)) != NULL)
					libentries->l_bits |= LIBENTRY_BIT_NESTED_TIMER;
				if (strstr (sub_desc, tagstring (S_PLACE)) != NULL)
					libentries->l_bits |= LIBENTRY_BIT_NESTED_PLACE;
				if (strstr (sub_desc, tagstring (S_PORT)) != NULL)
					libentries->l_bits |= LIBENTRY_BIT_NESTED_PORT;
			}

			in_descriptor = FALSE;
			memfree (current_descriptor);
		} else {
			if ((descriptor_indent == 0) && (len != 0) && (sub_desc[len - 1] == ')')) {
				/* we're at the end of the formal parameter list */
				descriptor_indent = 2;	/* next line requires indenting */
			} else if (descriptor_indent == 2) {
				/* we've just found the keyword; channels are next */
				descriptor_indent = 4;
			} else if ((descriptor_indent == 0) && (len > 11)) {
				/* check for a required descriptor indent for MOBILE PROC
				 * types -- line doesn't end in `)'
				 */
				char *ch;

				if ((ch = strstr (sub_desc, ") IMPLEMENTS ")) && (ch < (sub_desc + len)) && (ch > (sub_desc + 2))) {
					/* yes, MOBILE PROC declaration */
					descriptor_indent = 2;
				} else if ((ch = strstr (sub_desc, ") FORK")) && (ch < (sub_desc + len)) && (ch > (sub_desc + 2))) {
					/* and also checking for regular PROCs that FORK */
					descriptor_indent = 2;
				} else if ((ch = strstr (sub_desc, ") SUSPEND")) && (ch < (sub_desc + len)) && (ch > (sub_desc + 2))) {
					/* .. or suspend */
					descriptor_indent = 2;
				}
#if 0
fprintf (stderr, "objrdr: here! -- len = %d, sub_desc = [%s]\n", len, sub_desc);
#endif
			}

			sub_desc = &sub_desc[len + 1];
		}

		DEBUG_MSG (("Returning %s", line));
		return (line);
		/*}}}  */
	}
}

/*}}}  */

/*{{{  EXTERNAL handling  */

PRIVATE char *extern_buf;
PRIVATE int extern_buf_len;
PRIVATE int extern_status;

#define EXTERNAL_VALUE_ERR  (-1L)
#define DEFAULT_EXTERNAL_WS EXTERNAL_VALUE_ERR
#define DEFAULT_EXTERNAL_VS 0
#define DEFAULT_EXTERNAL_MS 0

PRIVATE const char *get_optional_num (const char *ptr, INT32 *res, char c) /*{{{*/
{
	/*  return negative if error */
	INT32 n = 0;
	while (*ptr == ' ') {
		ptr++;
	}
	if (*ptr == c) {
		ptr++;		/* skip past the matched character */
		while (*ptr == ' ') {
			ptr++;
		}
		while ((*ptr >= '0') && (*ptr <= '9')) {
			if (((BIT32) n) > 0x7FFFFFFF) {
				n = EXTERNAL_VALUE_ERR;
				break;
			}
			n = (n * 10) + ((INT32) (*ptr) - '0');
			ptr++;
		}
	} else if (*ptr == '\0') {
		n = (*res);
	} else {
		n = EXTERNAL_VALUE_ERR;
	}
	(*res) = n;

	return (ptr);
}
/*}}}*/
PRIVATE const char *get_optional_string (const char *ptr, const char *sptr, int *res) /*{{{*/
{
	/* if found, sets 'res' to non-zero, else zero */
	while (*ptr == ' ') {
		ptr++;
	}
	for (; (*sptr != '\0') && (*sptr == *ptr); sptr++, ptr++);
	if ((*sptr == '\0') && ((*ptr == ' ') || (*ptr == '\t') || (*ptr == '\n') || (*ptr == '\0'))) {
		*res = 1;
	} else {
		*res = 0;
	}
	return (ptr);
}
/*}}}*/
PUBLIC BOOL init_external (const char *string, const int dynext) /*{{{*/
{
	wordnode *name = NULL;
	const char *ptr;
	int len = strlen (string);
	INT32 ws = DEFAULT_EXTERNAL_WS, vs = DEFAULT_EXTERNAL_VS;
#ifdef MOBILES
	INT32 ms = DEFAULT_EXTERNAL_MS;
#endif
	int dummy;
	int does_fork = 0;

	/* PARSE: "PROC P () = 1", or "PROC P () = 1 , 10" */
	/* for mobiles, parse an extra one */

	for (len = strlen (string); (len > 0) && (string[len] != ')'); len--);

	/* look for 'FORK' */
	if (string[len] == ')') {
		int i = len + 1;

		for (; (string[i] == ' ') || (string[i] == '\t'); i++);
		if (!strncmp (string + i, "FORK", 4)) {
			does_fork = 1;
			len = i+3;
		}
	}

	extern_buf = memalloc (len + 3);
	memcpy (extern_buf, string, len + 1);

	extern_buf[len + 1] = '\n';
	extern_buf[len + 2] = '\0';
	extern_buf_len = len + 2;
	extern_status = 0;

#if 0
fprintf (stderr, "init_external(): string is [%s]\n", string);
fprintf (stderr, "init_external(): line   is [%s]\n", extern_buf);
#endif
	DEBUG_MSG (("init_external: string is \"%s\"\n", string));
	DEBUG_MSG (("init_external: line   is \"%s\"\n", extern_buf));

	ptr = &string[len + 1];
	ptr = get_optional_num (ptr, &ws, '=');
	ptr = get_optional_num (ptr, &vs, ',');
#ifdef MOBILES
	ptr = get_optional_num (ptr, &ms, ',');
#endif
	while (*ptr == ' ') {
		ptr++;
	}

	if (dynext) {
		/* dynamic external definition -- workspace and vectorspace are found separately, minimal amount to start with */
		if ((ws != DEFAULT_EXTERNAL_WS) || (vs != DEFAULT_EXTERNAL_VS)) {
			/* not expecting these to be specified */
			synwarn_s (SYN_BAD_PRAGMA_DIRECTIVE, flocn, "DEXTERNAL");
			return FALSE;
		}
		ws = 1;
		vs = 1;
#ifdef MOBILES
		ms = 0;
#endif
	}
#ifdef MOBILES
	DEBUG_MSG (("ws : %ld, vs : %ld, ms : %d\n", ws, vs, ms));
#else
	DEBUG_MSG (("ws : %ld, vs : %ld\n", ws, vs));
#endif
	if ((ws >= 0) && (vs >= 0) && (*ptr == '\0')) {
		name = desc_name (extern_buf, &dummy);	/* Don't bother translating these */
	}
	if (name == NULL) {
		synwarn_s (SYN_BAD_PRAGMA_DIRECTIVE, flocn, "EXTERNAL");
		return FALSE;
	}
	if ((WLengthOf (name) >= 2) && (!strncmp (WNameOf (name), "C.", 2))) {
		if (ws < 3) {
			ws = 3;
		}
	} else if ((WLengthOf (name) >= 3) && (!strncmp (WNameOf (name), "B.", 2) || !strncmp (WNameOf (name), "BX.", 3))) {
		/* EXTERNAL blocking system-call definition -- check that "ws" meets the minimum requirements, and fix if it doesn't. */
		#ifdef PROCESS_PRIORITY
		const int min_ws = 6;
		#else
		const int min_ws = 5;
		#endif	/* !PROCESS_PRIORITY */

		if (ws < min_ws) {
			/* synwarn_s ((min_ws == 3) ? SYN_BSYSCALL_NOTMINWS3 : SYN_BSYSCALL_NOTMINWS2, flocn, WNameOf (name)); */
			ws = min_ws;
		}
#if 0
fprintf (stderr, "init_external: found bsyscall.  name is %*s, ws=%d, vs=%d, ms=%d\n", WLengthOf (name), WNameOf (name), ws, vs, ms);
#endif
	} else if ((WLengthOf (name) >= 5) && (!strncmp (WNameOf (name), "CIF.", 4))) {
		ws += 16;
	}
#ifdef MOBILES
	(void) process_externalname (name, 0, ws, vs, ms, current_fe_data->fe_txlib->ptype, current_fe_data->fe_txlib->pattr, 0, FALSE);
#else
	(void) process_externalname (name, 0, ws, vs, current_fe_data->fe_txlib->ptype, current_fe_data->fe_txlib->pattr, 0, FALSE);
#endif
	extern_status = 1;
	return TRUE;
}

/*}}}  */
/*{{{  PUBLIC const char *readexternalline (int *const line_len)*/
/*{{{  comment */
/*****************************************************************************
 *
 *  readexternalline reads a line of occam source from the saved #EXTERNAL, returns
 *                     a pointer to the line, or NULL if eof or error.
 *
 *****************************************************************************/
/*}}}  */
PUBLIC const char *readexternalline (int *const line_len)
{
	const char *line;
	switch (extern_status++) {
	case 1:
		line = extern_buf;
		*line_len = extern_buf_len;
		break;
	case 2:
		memfree (extern_buf);
		extern_buf = NULL;
		line = "  SEQ\n";
		*line_len = 6;
		break;
	case 3:
		line = ":\n";
		*line_len = 2;
		break;
	default:
		extern_status = 0;
		line = NULL;
		*line_len = 0;
		break;
	}
	DEBUG_MSG (("readexternalline: returning :%s", line == NULL ? "NULL" : line));
	return (line);
}

/*}}}  */

/* end of #PRAGMA external stuff */
/*}}}  */

/*{{{  PUBLIC void addtoentrypointlist (nptr)  for the parser */
/*****************************************************************************
 *
 *  addtoentrypointlist adds nptr to the entrypointlist
 *
 *****************************************************************************/
PUBLIC void addtoentrypointlist (treenode * nptr)
{
	DEBUG_MSG (("addtoentrypointlist: adding %lx (\"%s\") (was %lx)\n", (BIT32) nptr, WNameOf (NNameOf (nptr)), (BIT32) entrypointlist));
	SetNLEntryNext (nptr, entrypointlist);
	entrypointlist = nptr;
}

/*}}}  */
/*{{{  PUBLIC void patchdescriptors     **     for the lexer */
/*{{{  comment */
/*****************************************************************************
 *
 *  patchdescriptors patches the namenodes of the entry points in the
 *                   'entrypointlist' using information in the
 *                   entry point table, empties the entry point table,
 *                   and adds the entry point list to the entry for this
 *                   SC in the SC table. If the entry point list is for a
 *                   library, it is thrown away.
 *
 *****************************************************************************/
/*}}}  */
PUBLIC void patchdescriptors (const char *const filename)
{
	treenode *ep = entrypointlist;
	DEBUG_MSG (("patchdescriptors\n"));

	while (ep != NULL)
		/*{{{  find this ep in the entry point table */
	{
		treenode *epnext = NLEntryNextOf (ep);	/* must save it before it is overwritten */
		libentry_t *thisentry;
		DEBUG_MSG (("Patching descriptor for \"%s\"\n", WNameOf (NNameOf (ep))));
		thisentry = search_libentries (libentries, NNameOf (ep));
		if (thisentry == NULL)
			lexerr_s (LEX_MISSING_ENTRYPOINT, flocn, WNameOf (NNameOf (ep)));
		else
			/*{{{  patch in offset and space usage for the entrypoint */
		{
			/* the offset is obsolete cos #SC is obsolete */
			/*SetNSCEntryOffset(ep, thisentry->n_offset); */
			/*SetNPDatasize(ep, thisentry->n_ws); */
			/*SetNPVSUsage(ep,  thisentry->n_vs); */

			{
				/* reverse the procdefs so that the most suitable is at */
				/* the start of the list */
				/* This is because they were encountered in the correct order */
				/* but added directly onto the list thus creating the list */
				/* in reverse order */
				procdef_t *p = thisentry->l_procdefs;
				procdef_t *reverser = NULL;
				while (p != NULL) {
					procdef_t *temp = p->p_next;
					p->p_next = reverser;
					reverser = p;
					p = temp;
				}
				thisentry->l_procdefs = reverser;
			}

			SetNLExternal (ep, thisentry);
			/*if (mode != LEX_SC) */
			SetNLEntryNext (ep, NULL);

			/* bug TS/1983 17/12/92 */
			SetNPNestedTimer (ep, (thisentry->l_bits & LIBENTRY_BIT_NESTED_TIMER) != 0);
			SetNPNestedPlace (ep, (thisentry->l_bits & LIBENTRY_BIT_NESTED_PLACE) != 0);
			SetNPNestedPort (ep, (thisentry->l_bits & LIBENTRY_BIT_NESTED_PORT) != 0);

#if 0
			printf ("patchdescriptors: \"%s\", nestedtimer: %d, nestedplace: %d, nestedport: %d\n",
				WNameOf (NNameOf (ep)), NPNestedTimerOf (ep), NPNestedPlaceOf (ep), NPNestedPortOf (ep));
#endif
		}
		/*}}}  */
		ep = epnext;
	}
	/*}}}  */

	/*{{{  #SC (obsolete) */
#if 0				/* #SC is no longer supported */
	if (mode == LEX_SC)
		/*{{{  Finish this entry in the SC table */
	{
		sc_t *scentry = &(sctable[sctableptr]);
		scentry->sc_filenum = currentfilenum;
		scentry->sc_entrypoints = entrypointlist;
		sctableptr = sctableptr + 1;
	}
	/*}}}  */
#endif
	/*}}}  */

	/*{{{  Delete this library's entry points */
	entrypointlist = NULL;
	/*}}}  */

	/*{{{  Cleanup local entrynames */
	{
		unsigned int module_found = 0;
		libentry_t *temp;
		for (temp = libentries; temp != NULL; temp = temp->l_next)
			module_found |= (temp->l_bits & LIBENTRY_BIT_COMPATIBLE);

		if (((current_fe_data->fe_lang & FE_LANG_OCCAM) != 0) && (module_found == 0))
			synwarn_s (SYN_NOTHING_IN_LIB, NOPOSN, filename);

		rememberallnames (libentries);
		libentries = NULL;

		localmodules = NULL;
		current_module_seekpos = 0;
	}

	/*}}}  */
}

/*}}}  */
/*{{{  PUBLIC treenode *local_loadstdlib (filename) */
/*****************************************************************************
 *
 *  loadstdlib loads the extended type operation library and stores
 *              the entry points in the table stdlibentries.
 *              stdlibcount is set to the number of entries in the table.
 *
 *****************************************************************************/
PUBLIC treenode *local_loadstdlib (const char *filename)
{
	const BOOL entered = open_file (filename, LEX_STDLIB, 0);
	const int savedlinebreakindent = linebreakindent;
	treenode *stdlibtree;
	if (!entered)
		lexfatal_s (LEX_FILE_OPEN_ERROR, NOPOSN, filename);
	linebreakindent = (-1);
	nextsymb ();
	stdlibtree = rscunit ();
	scopeandcheck_main (&stdlibtree, FALSE, FALSE);	/* no predefs, no toplevel check */
	/* Set up free variable lists and channel directions */
	alias_and_usage_check (stdlibtree, FALSE, FALSE, NULL);
	linebreakindent = savedlinebreakindent;
	return (stdlibtree);
}

/*}}}  */

/*{{{  #SC handling (obsolete) */
#if 0				/* #SC is no longer supported */
/*{{{  PUBLIC INT32 sc_size()                         for the code emitter */
/*****************************************************************************
 *
 *  sc_size returns the total code size of all nested SCs
 *
 *****************************************************************************/
PUBLIC INT32 sc_size (void)
{
	INT32 size = 0;
	int i;
	for (i = 0; i < sctableptr; i++)
		size += sctable[i].sc_codesize;
	return (size);
}

/*}}}  */
/*{{{  PUBLIC void apply_to_sc_entries(p)      for the code generator */
/*****************************************************************************
 *
 *  apply_to_sc_entries calls the function 'p' for the list of entry points
 *                      for each nested SC. The second parameter passed to 'p'
 *                      is the total code size for all SCs previous to the
 *                      one containing the current entry points, ie. The amount
 *                      of code before the current SC.
 *
 *****************************************************************************/
PUBLIC void apply_to_sc_entries (void (*p) ())
{
	int i;
	INT32 scsize = 0;
	DEBUG_MSG (("apply_to_sc_entries\n"));
	for (i = 0; i < sctableptr; i++) {
		(*p) (sctable[i].sc_entrypoints, scsize);
		scsize += sctable[i].sc_codesize;
	}
}

/*}}}  */
#endif
/*}}}  */

/*{{{  PUBLIC void init_obj_reader() */
/*****************************************************************************
 *
 *  init_obj_reader initialises the object file reader
 *
 *****************************************************************************/
PUBLIC void init_obj_reader ()
{
	current_module_seekpos = 0;

/*sctableptr = 0; *//* #SC is no longer supported */
	entrypointlist = NULL;
	extern_buf = NULL;
	extern_status = 0;
	libentries = NULL;
	localmodules = NULL;
}

/*}}}  */
