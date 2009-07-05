/* $Id: occamfe.h,v 1.4 1998/09/03 12:01:52 djb1 Exp $ */

/*
 *	abstract interface to the frontend (lexer, parser + checker)
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


/*{{{  fe_lang_t values*/
/* These are set up as bit patterns so that tests on combinations are easy.
   I use the fact that enums can have values attached to them,
   even though I think that it is a horrible feature! (ugh).
*/
typedef enum
  {
    FE_LANG_OCCAM   = 1, /* occam 2 frontend */
    FE_LANG_CONFIG2 = 2, /* occam 2 configurer Version 2 (with both h/w and s/w) */
    FE_LANG_CONFIG3 = 4, /* oocam 2 configurer Version 3 (#HARDWARE etc) */
    FE_LANG_NDL     = 8  /* Network Description Language (hardware) */
  } fe_lang_t;

#define FE_LANG_ALL (FE_LANG_OCCAM | FE_LANG_CONFIG2 | FE_LANG_CONFIG3 | FE_LANG_NDL)
/*}}}*/

/*{{{  data structures*/
/*{{{  fe_handle_t*/
/* This is the abstract handle type which is exported to the harness
   and backend */
typedef struct fe_handle_s fe_handle_t;
/*}}}*/

/*{{{  fe_check_asm_fn_t*/
/* This is the type of the function used to check ASM instructions */
typedef INT32 (*fe_check_asm_fn_t)(const char *inst, BOOL guy_not_asm,
                         int guyinserts,
                         int *err, int *ops,
                         int *base_instr, BOOL *primary,
                         BOOL *byte_directive, BOOL *word_directive,
                         BOOL *loading_address,
                         BOOL *ptrs, BOOL *elems, BOOL *labels,
                         BOOL *const_required);
/*}}}*/

/*{{{  fe_error_msg_fn_t*/
/* This is the type of the function used to actually print error messages */
typedef void (*fe_error_msg_fn_t)(const fe_handle_t *handle,
                                  err_severity_t severity,
                                  const char *message,
                                  const char *filename, int line_number);
/* if filename is NULL, the filename portion should be ommitted */
/*}}}*/

/*{{{  fe_translate_data_t*/
/* This is the abstract type of the structure used to hold details
   of #PRAGMA TRANSLATE.
   It shouldn't really be exported, but it has to be so that the frontend
   can get access to its own data structure via current_fe_handle.
*/
typedef struct fe_translate_data_s fe_translate_data_t;
/*}}}*/

/*{{{  fe_data_t*/
/* The caller of fe_open must allocate a data object of type fe_data_t,
   and must initialise the appropriate slots before calling each
   abstract function.
   (This prevents having to pass hundreds of parameters)

   The fe_... module guarantees not to modify the structure.
*/
typedef struct
  {
    fe_lang_t          fe_lang;          /* Whether compiler, configurer, etc */
    const char        *fe_sourcefilename;/* Filename to be read */
    const char        *fe_pathname;      /* Environment var. name */
    const char        *fe_toolversion;   /* Version string for the compiler etc */
    const char        *fe_toolname;      /* not currently used */
    FILE              *fe_outfile;       /* Where to send info messages */
    FILE              *fe_errfile;       /* Where to send error messages */
    txlib_t           *fe_txlib;         /* pointer to txlib structure for bpw etc */
    BOOL               fe_information;   /* TRUE if info is to be displayed */
    BOOL               fe_brieferrors;   /* TRUE if single line errors are required */
                                         /* FALSE if source window is required too */
    BOOL               fe_crasherrors;   /* TRUE if we want to crash on all errors (for debugging) */
    BOOL               fe_onlylex;       /* TRUE if we are just looking at the lexer output */
    BOOL               fe_allowpredefs;  /* TRUE if we want to permit predefines */
    BOOL               fe_chanaspointer; /* TRUE if channels are implemented as pointers */
    BOOL               fe_checkalias;    /* TRUE if we want to alias check the source */
    BOOL               fe_checkusage;    /* TRUE if we want to usage check the source */
    BOOL               fe_formalmodel;   /* TRUE if we want to formal model check/generate the source */
    BOOL               fe_fm_collct;     /* if TRUE, collapse CHAN TYPE PROTOCOLs into single types */
    BOOL               fe_fm_toplevelonly; /* if TRUE, only generate models for things in the top-level file (not included material) */
    BOOL               fe_fm_nocr;       /* if TRUE, do not generate separate claim/release events */
    BOOL               fe_fm_inlinecr;   /* if TRUE, inline claim/release events into channel-types */
    BOOL               fe_fm_comm;       /* if TRUE, include acquire/lose events for channel-ends */
    BOOL               fe_debuguse;      /* TRUE if we want to display usage check diagnostics */
    BOOL               fe_rangechecking; /* TRUE if range checking is required */
    BOOL               fe_disable_rangechecking_in_branches; /* TRUE when configurer/ndlrdr want to disable
                                                                checks for unused IF branches */
    BOOL               fe_allow_inlines;      /* if FALSE, ignores the inline keyword */
    BOOL               fe_ignore_comments;    /* make the lexer completely ignore comments */
    BOOL               fe_suppress_compat;    /* suppress call compatibility checks */
    BOOL               fe_warn_comment_indent;/* warn (not error) about illegal comment indentation */
    BOOL               fe_warn_on_usage_error;/* warn (not error) on alias/usage errors */
    BOOL               fe_warn_on_all_errors; /* warn (not error) on ALL errors */
    BOOL               fe_read_all_libs;      /* even if wrong processor type */
    BOOL               fe_hash_version_string;/* whether to hash the version string into sourcehash */
    BOOL               fe_visible_tags;       /* whether to make PROTOCOL tags visible */
    BOOL               fe_warning_descoped_n; /* warn when descoped by a name */
    BOOL               fe_warning_descoped_p; /* warn when descoped by a param */
    BOOL               fe_warning_unused_v;   /* warn about unused variables */
    BOOL               fe_warning_unused_p;   /* warn about unused parameters */
    BOOL               fe_warning_unused_r;   /* warn about unused routines */
    BOOL               fe_warning_chanofany;  /* warn about CHAN OF ANY */
    BOOL               fe_warning_guy;        /* warn about GUY code */ /* bug TS/1664 21/08/92 */
    BOOL               fe_warning_case_input; /* warn about unused CASE input tags */
    BOOL               fe_warning_tag_input;  /* warn about unused tagged input tags */
    BOOL               fe_warning_bad_place;  /* warn about badly placed PLACE statements */
    BOOL               fe_warning_bad_asm;    /* warn about illegal ASM instrs */
#ifdef OCCAM2_5
    BOOL               fe_align_fields;       /* Use padding to align fields of records */
    BOOL               fe_align_records;      /* Use padding to align record sizes */
#endif
    int                fe_errormode;          /* Hashed into source function */
    BOOL              *fe_error_occurred;     /* if not NULL, set TRUE by error fn */
    const int         *fe_guyinserts;         /* It's a pointer cos #OPTION can change it */
    BOOL               fe_noguy_yesasm;       /* GUY is obsolete - use ASM */
    int                fe_noslot_value;       /* Value used for initialising names */
    fe_check_asm_fn_t  fe_check_asm_fn;       /* Function to return ASM instruction properties */
    int              (*fe_option_fn) (const char *s, int len, void(*error_fn)(const int));
                                              /* Function to process #OPTION */
    void             (*fe_process_filename_fn)(char *s, int maxlen);
                                              /* process #USE filenames (get correct suffix) */
    fe_error_msg_fn_t  fe_error_msg_fn;       /* called to print the error message */
    void             (*fe_backenderror_fn)(void);
                                              /* called after a backend error (deletes object file) */
    void             (*fe_abort_fn)(const fe_handle_t *);
                                              /* what to do after fatal error */
  } fe_data_t;
/*}}}*/
/*}}}*/

/*{{{  routines*/
/*{{{  main processing functions*/
/* set up the following for _all_ functions:
   fe_lang, fe_error_msg_fn,
   fe_brieferrors, fe_backenderror, fe_checkalias, fe_checkusage, fe_formalmodel,
   fe_errormode, fe_toolversion, fe_toolname, fe_information,
   ALL fe_warning... flags, fe_warn_on_all_errors
   fe_pathname, fe_outfile, fe_errfile, fe_abort_fn, fe_error_occurred
*/

fe_handle_t *fe_open(const fe_data_t *const data);
void         fe_close(fe_handle_t **handle);


/* set up the following before calling fe_parse:
   fe_sourcefilename, fe_onlylex, fe_guyinserts, fe_noguyyesasm, fe_option_fn,
   fe_process_filename_fn, fe_allow_inlines, fe_ignore_comments,
   fe_warn_comment_indent
*/
void         fe_parse(fe_handle_t *const handle);


/* set up the following before calling fe_scope_and_check:
   fe_allowpredefs, fe_check_asm_fn, fe_chanaspointer,
   fe_rangechecking
*/
void         fe_scope_and_check(fe_handle_t *const handle);


/* set up the following before calling fe_alias_and_usage_check:
   fe_debuguse, fe_warn_on_usage_error
*/
void         fe_alias_and_usage_check(fe_handle_t *const handle);


void         fe_formalmodel_check (fe_handle_t *const handle);


treenode    *fe_loadstdlib(fe_handle_t *const handle, const char *filename);

/*}}}*/

/*{{{  access functions*/
treenode        *fe_get_treeroot  (const fe_handle_t *const handle);
int              fe_get_totallines(const fe_handle_t *const handle);
const fe_data_t *fe_get_data_ptr  (const fe_handle_t *const handle);
jmp_buf         *fe_get_env_addr  (const fe_handle_t *const handle);
void            *fe_get_user_ptr  (const fe_handle_t *const handle);

void             fe_set_user_ptr  (fe_handle_t *const handle, void *const user_ptr);

/*}}}*/

/*{{{  file handling functions*/
int fe_addfilename     (fe_handle_t *const handle,
                        wordnode *const name, const int issource,
                        const int parent, const int parentline);
const char *fe_lookupfilename  (const fe_handle_t *const handle, const int n);
BOOL  fe_fileissource    (const fe_handle_t *const handle, const int n);
int   fe_parentoffile    (const fe_handle_t *const handle, const int n);
int   fe_parentposnoffile(const fe_handle_t *const handle, const int n);
int   fe_numberoffiles   (const fe_handle_t *const handle);
SOURCEPOSN fe_sourcefileof(fe_handle_t *const handle, const SOURCEPOSN old_locn);

/*}}}*/
/*{{{  usage information*/
void fe_mark_free_vars(fe_handle_t *const handle, treenode *tptr, const BOOL check_usage);
BOOL fe_paraminputoroutputon(fe_handle_t *const handle, treenode *n, const BOOL inputnotoutput);
BOOL fe_isafreevarof(fe_handle_t *const handle, treenode *v, treenode *n);
void fe_walk_free_vars(fe_handle_t *handle, treenode *nptr,
     int (*fn)(treenode **, void *), void *const voidptr);
char *fe_udvstatestringof (treenode *const nptr);
/*}}}*/
/*{{{  translation access for the backend*/
fe_translate_data_t *fe_translates(const fe_handle_t *const handle);
wordnode *fe_translate_from_internal(const fe_handle_t *const handle,
                                     wordnode *name);
/*}}}*/
/*{{{  source code hash functions*/
BIT32 fe_get_sourcehash(const fe_handle_t *const handle);
void  fe_sourcehash(fe_handle_t *const handle, const char *const string, const int len);
/*}}}*/
/*{{{  special predefine handling*/
void      fe_set_predefname(fe_handle_t *const handle, treenode *const nptr, const int predef);
treenode *fe_get_predefname(const fe_handle_t *const handle, const int predef);

#define fe_predefname_LIST           0 /* List of all visible predefines */
#define fe_predefname_HARDWARE_NODE  1 /* NODEs imported from #NETWORK */
#define fe_predefname_HARDWARE_ARC   2 /* ARCs  imported from #NETWORK */
#define fe_max_predefnames           3

/*}}}*/
/*{{{  special pre-processor handling */
BOOL fe_preproc_cmdline_define (const char *arg);
/*}}}*/
/*{{{  error handling*/
void fe_set_errorcount(fe_handle_t *const handle, const int value);
int  fe_get_errorcount(const fe_handle_t *const handle);
BOOL fe_memo_err(fe_handle_t *const handle, const int class, const int err, const SOURCEPOSN locn);

/*}}}*/
/*{{{  saving state*/
const void *fe_save_state   (const fe_handle_t *const fe_handle);
void        fe_restore_state(fe_handle_t *const fe_handle, const void *const state);
/* You must not rely on the contents of state being valid after a restore! */
/*}}}*/

/*{{{  misc access functions*/

treenode *fe_gettype_main(treenode *const tptr);
treenode *fe_gettype_main_orig (treenode *const tptr);
treenode *fe_gettype(treenode *const tptr);
int       fe_typeof(treenode *const tptr);
treenode *fe_lookupname (wordnode *const w);
/*}}}*/
/*}}}*/

/*{{{  side-effects pragmas*/
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (fe_get_treeroot)
#pragma IMS_nosideeffects (fe_get_totallines)
#pragma IMS_nosideeffects (fe_get_data_ptr)
#pragma IMS_nosideeffects (fe_get_env_addr)
#pragma IMS_nosideeffects (fe_get_user_ptr)
#pragma IMS_nosideeffects (fe_lookupfilename)
#pragma IMS_nosideeffects (fe_fileissource)
#pragma IMS_nosideeffects (fe_parentoffile)
#pragma IMS_nosideeffects (fe_parentposnoffile)
#pragma IMS_nosideeffects (fe_numberoffiles)
#pragma IMS_nosideeffects (fe_translates)
#pragma IMS_nosideeffects (fe_get_sourcehash)
#pragma IMS_nosideeffects (fe_get_predefname)
#pragma IMS_nosideeffects (fe_get_errorcount)
#pragma IMS_nosideeffects (fe_save_state)
#endif
/*}}}*/

