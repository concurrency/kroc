/* $Id: harndef.h,v 1.2 1996/11/05 11:55:10 mdp2 Exp $ */

/*
 *	compiler harness definitions
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

/*{{{  global variables */
/*extern FILE *infile;*/ /* never used in backend */
extern FILE *outfile;
/*extern FILE *errfile;*/ /* now PRIVATE */
extern FILE *objfile;
extern char sourcefilename[];
extern char objfilename[];
extern char extlibfilename[];
extern char vlibsfilename[];
extern char pathname[];
#ifdef COMPILING_TO_JCODE
extern char asmfilename[]; /* used in compile.c */
#endif

extern char cfbfilename[]; /* used by the configurer only */
extern char deblibname[]; /* used by the configurer only */
extern char prolibname[]; /* used by the configurer only */
extern char syslibname[]; /* used by the configurer only */
extern char virlibname[]; /* used by the configurer only */

/* Processor attributes */
extern int targetintsize;
extern int wordshift;
struct instr_class
{
  BOOL hasprocessmodel;
  BOOL haserrorhandling;
  BOOL common_t450_hseries;
};
extern struct instr_class  instr_class;
extern int errormode;
extern int assert_errormode;
extern BOOL inside_assertion;
/*extern int compilemode;*/ /* never used */
extern BOOL vsenabled;
/*extern BOOL information;*/ /* This is now included from compharn.h */
extern BOOL disassemble;
extern BOOL assembly_output;
extern BOOL diagnostics;
extern BOOL ims_asm_output; /* true for INMOS assembler or ETC assembler */
extern BOOL debugoutput;
extern BOOL minimal_debugoutput; /* Just enough for backtracing */
extern BOOL iobycall;
extern BOOL realign_virtual_channels;
extern BOOL chanaspointer;
extern BOOL stdlibsenabled;
extern BOOL vlibsenabled;
extern BOOL checkalias; /* bug TS/1563 means that the backend needs it! */
extern BOOL checkusage;
extern BOOL fast_negative_prod;
extern BOOL ignore_assertions;
extern BOOL extended_input;
extern BOOL tagged_input_tag_xin;
extern BOOL preproc_dump; /* flag to get pre-processor to dump its values (in the form of DEFINEs) */


/*{{{  MDP ad hoc extras*/
extern BOOL etc_output;
extern BOOL asm_uppercase;
extern BOOL tcoff_without_code;
extern BOOL use_condflags;
/*}}}  */

extern BOOL hidelibnames;       /* Whether to use the 'hidden' REALOP etc */
extern int libpatchsize;        /* How many bytes to use for libpatch */
extern int code_style_flags;    /* Flags for variations in code style */
extern int warning_flags;       /* Which warnings are enabled */
extern char *predefsuffix;      /* Extra suffix for predefined routines */
extern char *compilersuffix;    /* Extra suffix for compiler library names */
extern char *vlibsuffix;        /* Extra suffix for i/o library names */

extern BOOL stop_after_trans;
extern BOOL prsrc;
extern BOOL prtree;
extern BOOL prxmltree;
/*extern BOOL testflag;*/ /* This is now included from compharn.h */

extern treenode *dummyexp_p;

extern const BOOL configuring; /* TRUE if configuring (backend reads it) */

extern struct map_handle_s *mapfile_handle; /* NULL if no mapfile */

extern BOOL mobile_size_field;	/* whether to include an extra word in MOBILE-related workspace for the underlying type size */

extern BOOL barrier_rbpe;	/* barrier resign-before-PAR-end */
/*}}}  */

/*{{{  T9000_alpha */
/* The idea is that when we get a T9000 gamma, etc, we can have another
   flag 'T9000_gamma', and update all these 'bug' macros to specify which
   bugs/ommissions appear on both/either.
*/
/* NO - we now have moved all this into 'txlib', so it is modified according
   to flags in 'tx_global'.
*/

/*}}}  */

/*{{{  use_shortintops (T9000_alpha) */
#if 0
/* This is turned off for Alpha. When we remove all the T9000 Alpha stuff,
   change this to */
#define use_shortintops has_shortintops
#endif
extern BOOL use_shortintops;
extern BOOL t450a_workarounds;
/*}}}  */

/*{{{  procedures */
void setprocessordefault (void);
/*BIT32 typeofprocessor (char *name);*/ /* unused */

void allocateworkspace (treenode *treeroot );
void codegenerate (BOOL open_file, treenode *treeroot );
/*}}}  */

/*{{{  error modes */
#define ERRORMODE_RANGECHECK            (0x1)
#define ERRORMODE_CONVERSIONCHECK       (0x2)
#define ERRORMODE_NEED_ERRORS           (0x4)
#define ERRORMODE_TIMESLICECHECK        (0x8)
#define ERRORMODE_NEED_STOPERR          (0x10)
#define ERRORMODE_STOP_IS_SETERR        (0x20)
#define ERRORMODE_STOP_IS_STOPP         (0x40)
#define ERRORMODE_NEED_FPCHKERR         (0x80) /* added 11/3/91 for H1L-process */

/* These flags are all coupled together: */
#define ERRORMODE_SHIFTCHECK ERRORMODE_NEED_ERRORS
#define ERRORMODE_ALIGNCHECK ERRORMODE_NEED_ERRORS
#define ERRORMODE_REPLCHECK  ERRORMODE_NEED_ERRORS

#define ERRORMODE_HALT                  (0x1000)
#define ERRORMODE_STOP                  (0x2000)
#define ERRORMODE_REDUCED               (0x4000) /* no longer used */
#define ERRORMODE_UNIVERSAL             (0x8000)

#define RANGECHECKING           (errormode & ERRORMODE_RANGECHECK)
#define CONVERSIONCHECKING      (errormode & ERRORMODE_CONVERSIONCHECK)
#define NEED_ERRORS             (errormode & ERRORMODE_NEED_ERRORS)
/*}}}  */

/*{{{  warning flags */
#define WARNING_OVERLAPS    (0x01)  /* Run-time overlap checks */
#define WARNING_UNUSED_V    (0x02)  /* Unused variables  */
#define WARNING_UNUSED_R    (0x04)  /* Unused routines   */
/*#define WARNING_CSE       (0x04)*//* Common subexpression elimination UNUSED */
#define WARNING_UNUSED_P    (0x08)  /* Unused parameters */
#define WARNING_DESCOPED_N  (0x10)  /* Warn when descoping a name by a name*/
#define WARNING_DESCOPED_P  (0x20)  /* Warn when descoping a name by a param*/
#define WARNING_CHANOFANY   (0x40)  /* Warn on CHAN OF ANY */
#define WARNING_GUY         (0x80)  /* Warn on GUY */
#define WARNING_CASE_INPUT (0x100)  /* Warn on unused CASE input tags */
#define WARNING_TAG_INPUT  (0x200)  /* Warn on unused tagged input tags */
#define WARNING_BAD_PLACE  (0x400)  /* Warn on badly placed PLACE statements */
#define WARNING_BAD_ASM    (0x800)  /* Warn on illegal ASM instructions */
#define WARNING_ALIGNMENT (0x1000)  /* Warn on alignment check in RETYPE */

#define WARNING_DEFAULT  ( \
   WARNING_UNUSED_V \
 | WARNING_UNUSED_R \
 | WARNING_UNUSED_P \
 | WARNING_CHANOFANY\
 | WARNING_GUY )
/*}}}  */

/*{{{  code style bits */
#define CODE_STYLE_DEFAULT         0   /* All off */
#define CODE_STYLE_ALT_PRI_PAR     0x1 /* Use ALT for PRI PAR */
#define CODE_STYLE_CJ_NOT_J        0x2 /* Use ldc 0; cj rather than j */
#define CODE_STYLE_NO_NULL_TRAPS   0x4 /* Don't worry about NULL trap handler */
#define CODE_STYLE_NO_PREAMBLE     0x8 /* No pre/postamble for PROCs */
#define CODE_STYLE_SPACE_NOT_TIME 0x10 /* "Optimise" for space not time */

/*}}}  */


