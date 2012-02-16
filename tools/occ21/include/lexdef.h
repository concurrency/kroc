/* $Id: lexdef.h,v 1.3 1998/09/03 11:34:05 djb1 Exp $ */

/*
 *	lex definitions visible to rest of compiler
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

/*{{{  external data */
extern FILE *infile;

extern int symb;
extern int symbindent, lineindent;

extern wordnode *lexword;
extern char literalv[];
extern int literalp;
extern int pdnumber;

/* FIXME: move these out of here and into somewhere sane, like the compiler parameter struct */
#ifdef USER_DEFINED_OPERATORS
extern BOOL user_defined_operators;	/* enable user-defined operators */
#endif
#ifdef MOBILES
extern BOOL mobile_data_types;		/* enable MOBILE data types */
extern BOOL mobile_proc_types;		/* enable MOBILE process types */
#endif
extern BOOL strict_checking;		/* strict checks */
extern BOOL safe_code_only;		/* do not allow inline ASM and other unsafe things */
extern BOOL extended_input;		/* enable extended inputs */
extern BOOL result_abbr_var;		/* generate variable + copy for result abbreviations */
extern BOOL no_placed_chans;		/* disable PLACE'd channels (user-defined channels) */
extern BOOL tagged_input_tag_xin;	/* whether to XIN on the tag byte (only if extended_input is enabled) */
extern BOOL enable_dtraces;		/* whether to enable debugging traces (flag in misc/harness.c) */
extern BOOL no_undefinedness_check;	/* if TRUE disables the undefinedness checker */
extern BOOL formal_model;		/* if TRUE, generate formal model (.cspx style) */

/*extern char current_filename[];*/ /* Now PRIVATE */
extern SOURCEPOSN flocn;

extern int lexmode;
/* File reading modes */
#define LEX_SOURCE 1
#define LEX_SC 2
#define LEX_LIB 3
#define LEX_PREDEFS 4
#define LEX_STDLIB 5
#define LEX_EXTERNAL 6		/* treated like a pseudo file */
#define LEX_DEXTERNAL 7		/* treated like a pseudo file */
#define LEX_CSOURCE 8		/* C-like occam syntax */

extern int currentfilenum;
/*extern int current_file;*/ /* unused */

extern int allow_asmnames;
/*}}}*/

/*{{{  external routines */
void lexinit   (void);
void lexfinish (FILE *fptr);
int lex_totallines(void);

BOOL open_file (const char *const name, const int mode, const int fileindent);

void printlex (FILE *fptr);
void nextsymb (void);
void nextline (void);
void skiplines(const int indent);

void lex_save_state (void);
void lex_restore_state (void);

void init_predefs_lexer(void);
const char *get_predef_string(const int pdno);

BOOL preproc_cmdline_define (const char *arg);
void preproc_dump_defines (FILE *fptr);
/*}}}*/

/*{{{  side-effects pragmas */
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (lexfinish)
#pragma IMS_nosideeffects (lex_totallines)
#pragma IMS_nosideeffects (get_predef_string)
#endif
/*}}}*/


