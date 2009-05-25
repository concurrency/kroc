/* $Id: occompfe.h,v 1.3 1998/06/04 16:05:09 djb1 Exp $ */

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

/*{{{  parameter block structure */
typedef struct {
  int language;
  const char *sourcefilename;
  const char *pathname;
  FILE *errfile;
  BOOL onlylex;
  BOOL call_alias_check;
  BOOL allowpredefs;
  BOOL brieferrors;
  BOOL crasherrors;
  BOOL debuguse;
  BOOL nochecking;
  BOOL allow_inlines;
  BOOL ignore_comments;
  BOOL warn_comment_indent;
  BOOL warn_on_usage_error;
  BOOL warn_on_all_errors;
  BOOL read_all_libs;
  BOOL hash_version_string;
  BOOL visible_tags;		/* whether to make PROTOCOL tags visible */
  BOOL suppress_compat;
  const BOOL *checkalias;	/* This can be modified while parsing by #OPTION */
  const BOOL *checkusage;	/* This can be modified while parsing by #OPTION */
  const BOOL *formalmodel;	/* This can be modified while parsing by #OPTION */
  const BOOL *fm_collct;	/* This can be modified while parsing by #OPTION */
  BOOL fm_toplevelonly;
  BOOL fm_nocr;
  BOOL fm_inlinecr;
  BOOL fm_comm;
  BOOL *error_occurred;		/* Set TRUE by the error routine if an error occurs */
  int (*process_option)(const char *s, int len, void (*error_fn)(int));
  const int *guyinserts;	/* it's a pointer cos #OPTION can change it */
  BOOL noguy_yesasm;		/* GUY is obsolete - use ASM */
  void (*process_filename_fn)(char *s, int maxlen);
  void (*cleanup_after_error)(void);
  void (*abort_fn)(void);
} occam_parms_t;

/*}}}*/
/*{{{  routines */
/*{{{  main routines */
treenode *occam_compiler_frontend(const occam_parms_t *occam_parms);

void cleanup_occam_compiler_frontend(void);
fe_handle_t *be_get_fe_handle(void);

/*}}}*/
/*{{{  loadstdlib */
treenode *loadstdlib(const char *filename);
/*}}}*/
/*{{{  usage information */
BOOL isafreevarof(treenode *v, treenode *n);
BOOL paraminputon(treenode *n);
BOOL paramoutputon(treenode *n);
void mark_free_vars(treenode *tptr, BOOL check_usage);
void walk_free_vars(treenode *nptr, int (*fn)(treenode **, void *), void *);

/*}}}*/
/*{{{  filename access */
/* These get data about the 'current' parse tree */
const char *lookupfilename (int n);
int parentoffile     (int n);
int parentposnoffile (int n);
BOOL fileissource    (int n);
int numberoffiles    (void);
SOURCEPOSN sourcefileof(SOURCEPOSN locn);
/*}}}*/
/*{{{  translate access for the backend */
wordnode *translate_from_internal(wordnode *name);
/*}}}*/
/*{{{  sourcehash access for the backend */
BIT32 get_sourcehash (void);
/*}}}*/
/*{{{  predefines */
treenode *get_list_of_predefs(void);
treenode *get_predefname(int pdno);
/*}}}*/
/*{{{  error handling */
BOOL memo_err(int class, int err, SOURCEPOSN locn);
/*}}}*/

/*{{{  backdoor access to the frontend */
int ntypeof (treenode *);
treenode *gettype_main(treenode *tptr);
treenode *gettype_main_orig (treenode *tptr);
treenode *gettype(treenode *tptr);

/*}}}*/
/*}}}*/

/*{{{  side-effects pragmas */
#if defined(_ICC) && !defined(CHECK_TREEACCESSES)
#pragma IMS_nosideeffects (be_get_fe_handle)
#pragma IMS_nosideeffects (lookupfilename)
#pragma IMS_nosideeffects (parentoffile)
#pragma IMS_nosideeffects (parentposnoffile)
#pragma IMS_nosideeffects (fileissource)
#pragma IMS_nosideeffects (numberoffiles)
#pragma IMS_nosideeffects (get_sourcehash)
#pragma IMS_nosideeffects (get_list_of_predefs)
#endif
/*}}}*/


