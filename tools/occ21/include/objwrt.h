/* $Id: objwrt.h,v 1.2 1997/10/07 14:15:04 mdp2 Exp $ */

/*
 *	object file writing
 *	Copyright (C) 1987 Inmos Limited
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

/*{{{  data declarations*/
extern BIT32 object_file_wrt_flags;

#define OBJ_FILE_WRT_NO_IMPORT_ORIGINS      0x01
#define OBJ_FILE_WRT_NO_EXPORT_ORIGINS      0x02
#define OBJ_FILE_WRT_PRI_TEXT_SECTION       0x04
#define OBJ_FILE_WRT_OCCAM_HARNESS          0x08
#define OBJ_FILE_WRT_NO_WS_SYMBOLS          0x10
#define OBJ_FILE_WRT_MERGE_DESCRIPTORS      0x20
#define OBJ_FILE_WRT_SHORT_DESCRIPTORS      0x40
#define OBJ_FILE_WRT_SPACES_DESCRIPTORS     0x80
#define OBJ_FILE_WRT_DEFAULT                0  /* All off */

/*}}}*/
/*{{{  functions*/
void process_hcomment (const wordnode *string);

void write_id (char *libname, int in_lib);
void write_entry_desc (treenode *nptr, INT32 offset/*, int pass*/);
void write_local_entry (treenode *const nptr, const INT32 offset);
void write_asm_desc(FILE *const /*asm_file*/, treenode *const /*nptr*/);
void write_library_calls (const BOOL reverse, treenode *libcalls,int instruction);
void write_debug_string (const char *string, int length);

void write_code_block_start (size_t length);
void write_code_block (size_t buflen, const BYTE *buffer);
void write_end_module (void);

INT32 add_to_module_list (treenode **list_ptr, treenode *lib);
long int saved_seek_position (void);
void write_linker_map (const char *filename, INT32 size, long int seek_pos, treenode *module_list);

void setup_text_linkage_name (const wordnode *name);
const char *objwrt_get_section_name(void);


void init_obj_writer(void);
/*}}}*/

/*{{{  side-effects pragmas*/
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (saved_seek_position)
#pragma IMS_nosideeffects (objwrt_get_section_name)
#endif
/*}}}*/

