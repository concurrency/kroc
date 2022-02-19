/* $Id: harness.c,v 1.16 1998/10/05 11:45:39 dcw Exp $ */

/*
 *	compiler harness
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

/*#define DEBUG*/

/*{{{  include files*/

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "tcoff.h" /* IMPORTED */
#include "suplib.h" /* IMPORTED */

#ifdef IMS
  #include <host.h> /* used for determining errfile */
  #include <misc.h> /* for setting special error actions */
#endif

#include "includes.h"

#include "instdef.h" /* for FULLGUYS and SEQUENTIALGUYS */
#include "maplib.h"  /* for writing mapfiles */ /* IMPORTED */
#include "objwrt.h"

#include "genhdr.h"
#include "bind1def.h"

#if defined(COMPILING_TO_JCODE)
  #include "compile.h"
#else
  #include "gen2def.h" /* for access to be_abbrevmode */
  #include "bind3def.h"
  #include "code1def.h"
  #include "debugdef.h"
#endif

#include "trandef.h"
#include "gen1def.h"
#include "gen8def.h"

#include "arg.h"

#include "version.h"

#if defined(CONFIG2) || defined(CONFIG3)
  #include "cfpub.h"
#endif

#if defined(HOST_OS_IS_UNIX)
#include <unistd.h>
#endif

/*}}}*/

/*{{{  definitions*/
/*{{{  Errormode attributes*/
/*{{{  errmodes maps command line string to error mode*/
#define HALT_MODE             ( ERRORMODE_HALT            \
                              | ERRORMODE_RANGECHECK      \
                              | ERRORMODE_CONVERSIONCHECK \
                              | ERRORMODE_NEED_ERRORS     \
                              | ERRORMODE_NEED_FPCHKERR   \
                              | ERRORMODE_STOP_IS_SETERR )

#define STOP_MODE             ( ERRORMODE_STOP            \
                              | ERRORMODE_RANGECHECK      \
                              | ERRORMODE_CONVERSIONCHECK \
                              | ERRORMODE_NEED_ERRORS     \
                              | ERRORMODE_TIMESLICECHECK  \
                              | ERRORMODE_NEED_FPCHKERR   \
                              | ERRORMODE_NEED_STOPERR    \
                              | ERRORMODE_STOP_IS_STOPP )

#define UNIVERSAL_MODE        ( ERRORMODE_UNIVERSAL       \
                              | ERRORMODE_RANGECHECK      \
                              | ERRORMODE_CONVERSIONCHECK \
                              | ERRORMODE_NEED_ERRORS     \
                              | ERRORMODE_TIMESLICECHECK  \
                              | ERRORMODE_NEED_FPCHKERR   \
                              | ERRORMODE_NEED_STOPERR    \
                              | ERRORMODE_STOP_IS_SETERR  \
                              | ERRORMODE_STOP_IS_STOPP )

/* These are disabled by the U flag */
#define ERRORMODE_OFF_MASK    ( ERRORMODE_RANGECHECK      \
                              | ERRORMODE_CONVERSIONCHECK \
                              | ERRORMODE_NEED_ERRORS     \
                              | ERRORMODE_TIMESLICECHECK  \
                              | ERRORMODE_NEED_FPCHKERR   \
                              | ERRORMODE_NEED_STOPERR)

/* These are disabled on the H1 L-process */
#define H1_ERRORMODE_OFF_MASK ( ERRORMODE_TIMESLICECHECK \
                              | ERRORMODE_NEED_FPCHKERR  \
                              | ERRORMODE_NEED_STOPERR   \
                              | ERRORMODE_STOP_IS_STOPP ) /* added bug TS/1528 11/12/91 */
#define H1_ERRORMODE_ON_MASK  ( ERRORMODE_STOP_IS_SETERR) /* added bug TS/1528 11/12/91 */

PRIVATE struct errmode_struct
  {
    int emode;        /* Mode used internally to this compiler */
    int eattr;        /* Bits to OR into the TCOFF attributes */
    char emodechar;   /* Command line character */
  } errmodes[] =
  {
    { HALT_MODE,      ATTRIB_HALT,      'H'},
    { STOP_MODE,      ATTRIB_STOP,      'S'},
  /*{ REDUCED_MODE,   ?,                'U'},*/
    { UNIVERSAL_MODE, ATTRIB_UNIVERSAL, 'X'}
  };
#define DEFAULT_ERROR_MODE "H"
#define UNKNOWN_ERROR_MODE 0 /* dissimilar to HALT_MODE etc */
/*}}}*/
/* It is used for a further classification of the transputer family
   instruction set */
PUBLIC struct instr_class  instr_class;
/*}}}*/

/*{{{  File names and suffixes*/
/* These are chosen at run-time: */
#if defined(CONFIG2) || defined(CONFIG3)
  #define SOURCEFILE_EXTENSION  ".pgm"
  /*#define DEFAULT_OBJ_EXTENSION ".lib"*/
  /*#define DEFAULT_OBJ_EXTENSION ".clu"*/
  #define CFB_FILE_EXTENSION    ".cfb"
  #define DEFAULT_OBJ_EXTENSION ".clu"

/*#define TCOFF_VLIB_FILENAME   "virtuall.lib"*/
/*{{{  TCOFF_VLIB_FILENAME - different foreach configurer*/
#if defined(CONFIG2)
  #define TCOFF_VLIB_FILENAME   "occonfio.lib"
#else
  #define TCOFF_VLIB_FILENAME   "onconfio.lib"
#endif
/*}}}*/
#else
  #define SOURCEFILE_EXTENSION  ".occ"
  #define DEFAULT_ETC_OBJ_EXTENSION ".tce"
  #define DEFAULT_ETC_ASM_EXTENSION ".etc"
  #define DEFAULT_OBJ_EXTENSION ".tco"
  #define DEFAULT_ASM_EXTENSION ".t"
#endif
  #define TCOFF_VLIB_FILENAME   "virtual.lib"

#define DEFAULT_PATHNAME "ISEARCH"

/*}}}*/

/*{{{  Option strings*/
/* We allow for the fact that we may add default suffixes to any option
   to create a filename */
#define MAX_OPTION_LENGTH (MAX_FILENAME_LENGTH - 5)

/*}}}*/

typedef enum { HELP_BASIC, HELP_LONG, HELP_ZED } help_page_t;

/*}}}*/
/*{{{  global variables*/
/*{{{  Definitions of compharn variables*/
PUBLIC BOOL information = FALSE;
PUBLIC BOOL testflag    = FALSE; /* a generic test flag for debugging */

#if defined(COMPILING_TO_JCODE)
PUBLIC struct optimisation_struct optimisations;
#endif

/*}}}*/
/*{{{  File names etc*/
/*PUBLIC FILE *infile; */ /* never used in backend */
PUBLIC FILE *outfile;
PUBLIC FILE *outxmlfile = NULL;
/*PUBLIC FILE *errfile;*/ /* Now PRIVATE */
PUBLIC FILE *objfile;
PRIVATE char rootfilename[MAX_FILENAME_LENGTH];
PUBLIC char extlibfilename[MAX_FILENAME_LENGTH];
PUBLIC char sourcefilename[MAX_FILENAME_LENGTH] = "";
PUBLIC char objfilename[MAX_FILENAME_LENGTH]    = "";
PUBLIC char tasmfilename[MAX_FILENAME_LENGTH]    = "";
/*PUBLIC char extlibfilename[13];*/ /* 7 or 8 chars, plus a dot, plus 3 suffix, plus nul */
PRIVATE BOOL extlibfilename_overruled = FALSE;
/*PUBLIC char vlibsfilename[13];*/ /* 8 chars, plus a dot, plus 3 suffix, plus nul */
PUBLIC char vlibsfilename[MAX_FILENAME_LENGTH]  = TCOFF_VLIB_FILENAME;

#if !(defined(CONFIG2) || defined(CONFIG3))
PRIVATE char mapfilefilename[MAX_FILENAME_LENGTH] = ""; /* WP/0010 18/03/92 */
#endif
PUBLIC map_handle_t *mapfile_handle; /* Initialised to NULL */

#if defined(COMPILING_TO_JCODE)
PUBLIC FILE *objstream;
PUBLIC FILE *errstream;
`/*PRIVATE char toolname[MAX_FILENAME_LENGTH]    = "oc";*/
/*#define toolname "oc"*/
#endif
/*}}}*/
/*{{{  Processor attributes*/
PUBLIC int targetintsize;
PUBLIC int wordshift;
PUBLIC BOOL fast_negative_prod;
/*}}}*/
/*{{{  Error modes*/
PUBLIC int errormode        = UNKNOWN_ERROR_MODE;
PUBLIC int assert_errormode = UNKNOWN_ERROR_MODE; /* INSdi01337 */
PUBLIC BOOL inside_assertion = FALSE;
/*}}}*/
/*{{{  Other flags for parametrising compiler execution*/
#if defined(CONFIG2) || defined(CONFIG3)
/*PUBLIC int compilemode     = COMP_PROGRAM;*/ /* never used */
PUBLIC BOOL stdlibsenabled  = FALSE; /* Whether to allow access to standard libs */
#else
/*PUBLIC int compilemode     = COMP_SC;*/ /* never used */
PUBLIC BOOL stdlibsenabled  = TRUE; /* Whether to allow access to standard libs */
#endif
PUBLIC BOOL vlibsenabled    = TRUE; /* Whether to allow access to virtual libs */
PUBLIC BOOL vsenabled       = TRUE;
PUBLIC BOOL diagnostics     = FALSE;
PUBLIC BOOL disassemble     = FALSE;
PUBLIC BOOL ims_asm_output  = FALSE; /* true if we want the output to go through the INMOS assembler */
#if defined(CODE_GEN_KROC_ASM)
PUBLIC BOOL assembly_output = TRUE; /* ASMONLY */
#else
PUBLIC BOOL assembly_output = FALSE;
#endif

/*{{{  MDP extras*/
PUBLIC BOOL etc_output = FALSE; /* switched on by /etc flag */
PUBLIC BOOL asm_uppercase   = FALSE;
PUBLIC BOOL use_condflags = FALSE; /* ASMONLY simpler code if target has condition flags */
#if defined(CODE_GEN_KROC_ASM)
PUBLIC BOOL tcoff_without_code = TRUE; /* ASMONLY */
#else
PUBLIC BOOL tcoff_without_code = FALSE;
#endif
/*}}}*/

#if defined(USER_DEFINED_OPERATORS)
PUBLIC BOOL user_defined_operators = FALSE;
#endif
#if defined(MOBILES)
PUBLIC BOOL enable_mobilespace = FALSE;
PUBLIC BOOL mobile_data_types = FALSE;
PUBLIC BOOL mobile_proc_types = FALSE;
#endif
PUBLIC BOOL strict_checking = FALSE;
PUBLIC BOOL safe_code_only = FALSE;
PUBLIC BOOL extended_input = FALSE;
PUBLIC BOOL result_abbr_var = FALSE;
PUBLIC BOOL reverse_alt_disable = FALSE;
PUBLIC BOOL disable_dynmem = FALSE;
PUBLIC BOOL no_placed_chans = FALSE;
PUBLIC BOOL mobile_size_field = FALSE;
PUBLIC BOOL main_dynentry = FALSE;
#if defined(INITIAL_DECL)
PUBLIC BOOL initial_decl = FALSE;
#endif
PUBLIC BOOL tagged_input_tag_xin = FALSE;
PUBLIC BOOL enhanced_alt_enable = FALSE;
PUBLIC BOOL alt_preenable = FALSE;
PUBLIC BOOL preproc_dump = FALSE;
PUBLIC BOOL mpp_check_at_act = FALSE;
PUBLIC BOOL map_all_procs = FALSE;
#if defined(TARGET_IS_BIGENDIAN)
PUBLIC BOOL target_bigendian = TRUE;
#else
PUBLIC BOOL target_bigendian = FALSE;
#endif
#if defined(TARGET_ACCESSALIGNED)
PUBLIC BOOL target_accessaligned = TRUE;
#else
PUBLIC BOOL target_accessaligned = FALSE;
#endif
PUBLIC BOOL enable_dtraces = FALSE;		/* debugging traces (requires run-time support) */
PUBLIC BOOL no_undefinedness_check = FALSE;
PUBLIC BOOL formal_model = FALSE;		/* whether or not to dump a .cspx file with a CSP model of the source */
PUBLIC BOOL fm_collct = FALSE;			/* whether or not to collapse PROTOCOLs in a CHAN TYPE during formal-model generation */


PUBLIC BOOL debugoutput     = TRUE; /* Whether to insert debug info into object file */
PUBLIC BOOL minimal_debugoutput = FALSE; /* Just enough for backtracing - only valid if*/
                                        /* debugoutput is TRUE                        */

PUBLIC BOOL sampling_profiling = FALSE;
PUBLIC BOOL  line_profiling = FALSE;
PUBLIC BOOL  cgraph_profiling = FALSE;
PUBLIC BOOL  trace_prof_image = FALSE; /* to trace profiling info */

PUBLIC BOOL  t450a_workarounds = FALSE;
/* removed source_output 14/3/91 */
/* PUBLIC BOOL source_output   = FALSE;*/ /* This forces full debug output (to screen)     */
PUBLIC  BOOL stop_after_trans          = FALSE;
PUBLIC  BOOL iobycall                  = TRUE;
PUBLIC  BOOL realign_virtual_channels  = FALSE;
PUBLIC  BOOL chanaspointer             = TRUE;
PUBLIC  BOOL prtree                    = FALSE;
PUBLIC  BOOL prxmltree		       = FALSE;
PUBLIC  BOOL prsrc                     = FALSE;
PUBLIC  BOOL checkalias                = TRUE;
PUBLIC  BOOL checkusage                = TRUE;
PUBLIC  BOOL ignore_assertions         = FALSE;

PUBLIC BOOL hidelibnames    = TRUE; /* Use REAL32OP% etc rather than REAL32OP */
PUBLIC int libpatchsize     = 0;    /* How many bytes for library patch */
                                    /* Any number except 8, overridden by setprocessor */
PUBLIC int code_style_flags = CODE_STYLE_DEFAULT; /* Flags for variations in code style */
PUBLIC int warning_flags    = WARNING_DEFAULT;    /* Which warnings are enabled */

/*PUBLIC char pathname[MAX_FILENAME_LENGTH] = "";*/      /* ISEARCH or ZI option */
PUBLIC char pathname[MAX_FILENAME_LENGTH] = DEFAULT_PATHNAME;      /* ISEARCH or ZI option */

PUBLIC char *compilersuffix = NULL;
PUBLIC char *predefsuffix   = NULL;
PUBLIC char *vlibsuffix     = NULL;

PUBLIC treenode *dummyexp_p;

#if defined (COMPILING_TO_JCODE)
PUBLIC bool add_outer_loop = FALSE;
PUBLIC bool  asmout = TRUE;
PUBLIC int32 sizeof_int_var = 0;
PUBLIC BOOL cse_heuristics_enabled = TRUE;
PUBLIC BOOL peepholer_enabled = TRUE;
PUBLIC BOOL tailcall_opt_enabled = TRUE;
PUBLIC BOOL comp_mapfile_required = FALSE;
PUBLIC BOOL compactable_output = FALSE;
#endif

#if defined(CONFIG2) || defined(CONFIG3)
PUBLIC const BOOL configuring = TRUE;

PUBLIC char cfbfilename[MAX_FILENAME_LENGTH] = ""; /* config binary filename */
PUBLIC char  deblibname[MAX_FILENAME_LENGTH] = ""; /* debug    lib name */
PUBLIC char  prolibname[MAX_FILENAME_LENGTH] = ""; /* profiler lib name */
PUBLIC char  syslibname[MAX_FILENAME_LENGTH] = ""; /* system   lib name */
PUBLIC char  virlibname[MAX_FILENAME_LENGTH] = ""; /* virtual  lib name */

#else
PUBLIC const BOOL configuring = FALSE;
#endif

PUBLIC BOOL barrier_rbpe = FALSE;

/*}}}*/

/*}}}*/
/*{{{  T9000 Alpha bits*/
PUBLIC BOOL use_shortintops;
PUBLIC BOOL force_shortintops = FALSE;

/**************************************************************************
HISTORY of T9000 Alpha modifications; search for 'T9000_alpha' & 'T9000_gamma'.
Section nunbers refer to document SW-0341-2.

T9000_alpha_nofpconv:
4.1:    14/05/92: FPADDDBSN:    INT64 -> REAL32 => INT64 -> REAL64 -> REAL32
4.2.1:  14/05/92: Conversion:   INT32 -> REAL32 => INT32 -> REAL64 -> REAL32
4.2.3:  14/05/92: Conversion:   INT64 -> REAL32 => INT64 -> REAL64 -> REAL32
4.2.7:  14/05/92: Conversion:   REAL32 -> INT32 => library call
4.2.8:  14/05/92: Conversion:   REAL64 -> INT32 => library call
4.2.11: 14/05/92: Conversion:   REAL32 -> INT64 => library call
4.2.12: 14/05/92: Conversion:   REAL64 -> INT64 => library call
T9000_alpha_nofprem:
4.8:    15/05/92: FPREM:        REAL32 and REAL64 remainder => library call
T9000_alpha_nofpconst:
4.11:   15/05/92: FpConst:      FPLDZEROSN and FPLDZERODB invalid
T9000_alpha_lprocess:
5.1:    14/05/92: G-processes:  Set L_process bit in PRI PAR.
T9000_alpha_nodevaccess:
5.7:    08/07/92: Dev access:   Don't use dev access for alpha.
T9000_alpha_nocauseerror:
5.8:    14/05/92: causeerror:   Don't use CAUSEERROR for STOP and CAUSEERROR()
T9000_alpha_nocrc:
5.10:   15/05/92: CRC:          Use lib call for CRCWORD and CRCBYTE

T9000_alpha_badsttimer:         'sttimer' doesn't store the correct value.
        No fix needed
T9000_alpha_nolsx:              'lsx' and 'lbx' don't sign extend properly.
        18/03/93                Disabled shortintops
T9000_alpha_noxsword:           'xsword' and 'xbword' don't sign extend properly.
        18/03/93                Disabled shortintops
T9000_alpha_badalt:             Lots of problems with enable and disable instructions.
        22/03/93                Inserted fancy sequence for disc.
T9000_alpha_badmint:            'mint' groups badly in some circumstances.
        18/03/93                Disabled constant sequences which use it; checked all others
T9000_alpha_badfpsub:           'fpsub' of x and zero produces -x.
        18/03/93                (x - y) => (x + (0 - y))
T9000_alpha_badgt:              'gt; cj' breaks if grouped together
        18/03/93                gt  => gt; eqc 1
T9000_alpha_badfpbool:          'fpgt; cj' breaks if grouped together; similar for all fp tests.
        18/03/93                fpgt => fpgt; eqc 1  etc.
T9000_alpha_badlmul:            'lmul' is broken; follow it by a 'nop'.
        02/04/93                added the nop
T9000_alpha_badlmulpre:         'lmul' is broken; precede it by a 'nop'.
        02/04/93                added the nop
T9000_alpha_badfmul:            'fmul' is broken when followed by 'cj'.
        --/--/93                Done
T9000_alpha_badrunp:            'runp' et al. is broken; follow it by a 'nop'
                                03/11/93 added the nop

T9000_gamma_nobitcnt:           'bitcnt' is broken
        04/11/93                modified to use the library routine.
T9000_gamma_carryin:            ALU has carry problems
        07/12/93                Done.
T9000_gamma_badmul:             Multiply needs nop in front
        04/11/93                Done
T9000_gamma_badeqc0:            Can't have two eqc 0 instrs
        24/11/93                Done

Library routines required:

#PRAGMA TRANSLATE REAL32TOINT32 "REAL32TOINT32%CHK"
#PRAGMA TRANSLATE REAL64TOINT32 "REAL64TOINT32%CHK"
#PRAGMA TRANSLATE REAL32TOINT64 "REAL32TOINT64%CHK"
#PRAGMA TRANSLATE REAL64TOINT64 "REAL64TOINT64%CHK"
#PRAGMA TRANSLATE REAL32REM "REAL32REMERR%CHK"
#PRAGMA TRANSLATE REAL64REM "REAL64REMERR%CHK"
#PRAGMA TRANSLATE CRCBYTE "CRCBYTE%O"
#PRAGMA TRANSLATE CRCWORD "CRCWORD%O"

INT32 FUNCTION REAL32TOINT32(VAL INT mode, VAL REAL32 x) IS 0(INT32) :
INT32 FUNCTION REAL64TOINT32(VAL INT mode, VAL REAL64 x) IS 0(INT32) :
INT64 FUNCTION REAL32TOINT64(VAL INT mode, VAL REAL32 x) IS 0(INT64) :
INT64 FUNCTION REAL64TOINT64(VAL INT mode, VAL REAL64 x) IS 0(INT64) :

REAL32 FUNCTION REAL32REM(VAL REAL32 x, y) IS 0.0(REAL32) :
REAL64 FUNCTION REAL64REM(VAL REAL64 x, y) IS 0.0(REAL64) :

INT FUNCTION CRCBYTE(VAL INT DATA, VAL INT CRCIn, VAL INT generator) IS 0 :
INT FUNCTION CRCWORD(VAL INT DATA, VAL INT CRCIn, VAL INT generator) IS 0 :

PLUS: all INT16 routines
PLUS: FPINT and DFPINT

PLUS: BITCNT

**************************************************************************/

/*}}}*/
/*{{{  local variables*/
PRIVATE char objectfileext[5] = DEFAULT_OBJ_EXTENSION;
PRIVATE char tasmfileext[5] = DEFAULT_ASM_EXTENSION;

#define MAX_LIBNAMESSUFFIX 10
PRIVATE char compilersuffixstring[MAX_LIBNAMESSUFFIX];
PRIVATE char   predefsuffixstring[MAX_LIBNAMESSUFFIX];
PRIVATE char     vlibsuffixstring[MAX_LIBNAMESSUFFIX];

PRIVATE BOOL generatecode      = TRUE;
PRIVATE BOOL call_alias_check  = TRUE;
PRIVATE BOOL zinfo             = FALSE;  /* Whether to display z option help page */
PRIVATE BOOL long_help         = FALSE;
PRIVATE BOOL stop_after_map    = FALSE;
PRIVATE BIT32 processorattr_errmode;
PRIVATE BOOL permit_variableio = TRUE;
PRIVATE BOOL allowpredefs     = TRUE; /* Whether to allow predefines */
PRIVATE BOOL onlylex          = FALSE;
PRIVATE int  guyinserts       = 0;
PRIVATE BOOL brieferrors      = FALSE;
PRIVATE BOOL crasherrors      = FALSE;
PRIVATE BOOL debuguse         = FALSE;
PRIVATE BOOL nochecking       = FALSE;
PRIVATE BOOL allow_inlines    = TRUE;
PRIVATE BOOL lexer_ignore_comments = FALSE; /* make the lexer completely ignore comments */
PRIVATE BOOL visible_tags          = FALSE; /* whether to make PROTOCOL tags visible */
PRIVATE BOOL warn_comment_indent   = FALSE; /* warn (not error) for comment indentation errors */
PRIVATE BOOL warn_on_usage_error   = FALSE; /* warn (not error) for alias/usage errors */
PRIVATE BOOL read_all_libs         = FALSE; /* even if for wrong processor type */
PRIVATE BOOL warn_on_all_errors    = FALSE; /* turn ALL errors into warnings */
PRIVATE BOOL hash_version_string   = TRUE;
PRIVATE BOOL alpha                 = FALSE;
PRIVATE BOOL gamma                 = FALSE;
PRIVATE BOOL gammae                = FALSE;
PRIVATE BOOL print_all_messages    = FALSE;
PRIVATE BOOL setymode              = FALSE;
PRIVATE BOOL t450a_workarounds_flag = TRUE;
PRIVATE BOOL suppress_call_compatibility_check = FALSE;
PRIVATE BOOL fm_toplevelonly       = FALSE; /* whether to consider the top-level only in formal-model generation (requires -zfm) */
PRIVATE BOOL fm_nocr               = FALSE; /* whether to avoid generating separate claim/release events (as parameters/vars) for shared channel-ends */
PRIVATE BOOL fm_inlinecr           = FALSE; /* whether to inline claim/release events into the channel-type */
PRIVATE BOOL fm_comm               = FALSE; /* whether to include acquire/lose events for channel-ends */

PRIVATE FILE *errfile;

#if defined(CONFIG2) || defined(CONFIG3)
PRIVATE cf_params_t cf_params; /* all initialised to 'off' */
PRIVATE BOOL advanced_debug    = FALSE; /* TRUE if Inquest debugger */
PRIVATE BOOL interactive_debug = FALSE; /* TRUE if idebug debugger */
#if defined(CONFIG2)
PRIVATE BOOL virtual_routing   = TRUE;
#endif
#endif

PRIVATE int errormodes_off_mask = 0;

#ifdef IMS
  PRIVATE BOOL repeat_loop = FALSE;  /* Used for option 'XM' repeating */
  PRIVATE BOOL load_only   = FALSE;  /* ditto */

  #define HOSTED HELP_LONG
#else
  #define HOSTED HELP_ZED
#endif

#if defined(HOST_OS_IS_UNIX) && !defined(WIN32)
/* These are used to provide heap space usage statistics */
PRIVATE void *original_sbrk;
PRIVATE BOOL memstats        = FALSE;
#endif

#if defined(COMPILING_TO_JCODE)
PRIVATE bool suppress_asm, asmtostdout;

#define ASMFILE_EXTENSION ".s"
#define TUNKNOWN -1
#define T212 2
#define T414 4
#define T425 9
#define T800 8
#define TA   10
#define TB   11
#define TC   12
#define T225 13
#define T805 14
#define H1   15
#define ZH1  16
#define MAX_ARGC 30
/* #define MAX_FILENAME_LENGTH 256 */
PRIVATE char  *asm_argv[MAX_ARGC];
PRIVATE int  asm_argc;
PUBLIC char asmfilename[MAX_FILENAME_LENGTH];
/*PRIVATE bool debug_harness;*/

#endif

/*}}}*/
/*{{{  setup arglib stuff*/

#if defined(OC)
	PRIVATE char tail_help_line[] = "Also full target processor name required\nA complete list of options is provided by the HELP option";
#elif defined(CONFIG2) || defined(CONFIG3) || defined(NDLRDR)
	PRIVATE char tail_help_line[] = "A complete list of options is provided by the HELP option";
#endif

arg2_help_page_type help_level = 0;
extern arg2_help_page_info oc_help_info;
/*}}}*/

/*{{{  extra definitions for COMPILING_TO_JCODE*/
#if defined(COMPILING_TO_JCODE)
	extern int asm_main (int argc, char **argv);
#endif
/*}}}*/

/*{{{  PRIVATEPARAM void print_memstats*/
PRIVATEPARAM void print_memstats(void)
{
	#if defined(HOST_OS_IS_UNIX) && !defined(WIN32)
	if (memstats) {
		fprintf(outfile, "Memory usage: (current tree %ld) (sbrk %d)\n", tablesize(), sbrk(0) - original_sbrk);
	}
	#endif
}
/*}}}*/
/*{{{  PRIVATE void end_compiler*/
PRIVATE void end_compiler (const int result)
{
	print_memstats ();
	cleanup_occam_compiler_frontend (); /* used to output some debugging messages */
	vtifinish (outfile);                /* ditto */
	#ifdef IMS
		if (repeat_loop) {
			exit_repeat(result);
		}
	#endif
	exit(result);
}
/*}}}*/
/*{{{  PRIVATEPARAM void abort_compiler*/
PRIVATEPARAM void abort_compiler(void)
{
	end_compiler(EXIT_FAILURE);
}
/*}}}*/
/*{{{  PRIVATE void harnesserror*/
PRIVATE void harnesserror (const char *const s)
{
	fprintf(errfile, "Serious-%s-%s\n", TOOL, s);
	end_compiler(EXIT_FAILURE);
}
/*}}}*/
/*{{{  PRIVATE void harnesserror_s*/
PRIVATE void harnesserror_s (const char *const s, const char * p1)
{
	fprintf(errfile, "Serious-%s-%s (%s)\n", TOOL, s, p1);
	end_compiler(EXIT_FAILURE);
}
/*}}}*/
/*{{{  PRIVATE void harnesserror_sssss*/
PRIVATE void harnesserror_sssss (const char *const s, const char * s1, const char * s2, const char * s3, const char * s4)
{
	fprintf(errfile, "Serious-%s-%s %s%s%s%s\n", TOOL, s, s1, s2, s3, s4);
	end_compiler(EXIT_FAILURE);
}
/*}}}*/
/*{{{  PRIVATE void harnesserror_sd*/
/* not used */
#if 0
PRIVATE void harnesserror_sd (const char *const s, const char * p1, const BIT32 p2)
{
	fprintf(errfile, "Serious-%s-%s %s(%d)\n", TOOL, s, p1, p2);
	end_compiler(EXIT_FAILURE);
}
#endif
/*}}}*/
/*{{{  PRIVATE void harnesserror_toobig*/
PRIVATE void harnesserror_toobig (const char *const s, const int p1, const int p2)
{
	fprintf(errfile, "Serious-%s-%s (%d > %d)\n", TOOL, s, p1, p2);
	end_compiler(EXIT_FAILURE);
}
/*}}}*/

/*{{{  COMPILING_TO_JCODE functions*/
#if defined(COMPILING_TO_JCODE)
/*{{{  PUBLIC void next_asm_argv(const char *const s)*/
PUBLIC void next_asm_argv(const char *const s)
{
	char *str = (char *)memalloc(strlen(s) + 1);

	strcpy(str, s);
	if (asm_argc < MAX_ARGC) {
		asm_argv[asm_argc++] = str;
	} else {
		harnesserror("Too many assembler arguments");
	}
}
/*}}}*/


/*{{{  PRIVATE void errormode_asm_arg()*/
PRIVATE void errormode_asm_arg()
{
	if ((errormode & ERRORMODE_HALT) != 0) {
		next_asm_argv("-h");
	} else if ((errormode & ERRORMODE_STOP) != 0) {
		next_asm_argv("-s");
	} else if ((errormode & ERRORMODE_UNIVERSAL) != 0) {
		next_asm_argv("-u");
	}
}
/*}}}*/


/*{{{  PRIVATE void rm_asmfile(void)*/
PRIVATE void rm_asmfile(void)
{
	#if 0
		const int status = remove(asmfilename);

		if ((status != 0) && debug_harness) {
			harnesserror_sd("Cannot delete temporary file ", asmfilename, status);
		}
	#else
		(void)remove (asmfilename);
	#endif
}
/*}}}*/
#endif
/*}}}*/


/*{{{  Processor types*/
/* It is assumed elsewhere that the default is a 32-bit processor (I think!) */
/* (Eg. in the configurer) */
#undef DEFAULT_PROCESSOR_TYPE
#if defined(CONFIG2)                             /* we no longer have a default processor */
	#define DEFAULT_PROCESSOR_TYPE "T414"    /* but we'll pretend to have one for the configurer */
#endif
#if defined(CONFIG3)
	#define DEFAULT_PROCESSOR_TYPE "T9000"
#endif

/*{{{  PRIVATE void seterrormode*/
PRIVATE void seterrormode(void)
{
	errormode &= ~errormodes_off_mask;
	if (L_process) {
		errormode |=  H1_ERRORMODE_ON_MASK; /* bug TS/1528 11/12/91 */
		assert_errormode &= ~H1_ERRORMODE_OFF_MASK; /* INSdi02127 */
		assert_errormode |=  H1_ERRORMODE_ON_MASK;  /* INSdi02127 */
	}
}
/*}}}*/
/*{{{  PRIVATEPARAM void setprocessorattr(void)*/
/* Add error mode and iocall to processor attributes */
PRIVATEPARAM void setprocessorattr(void)
/* This sets the attribute TCOFF word, plus it also sets up other
   processor-specific backend globals
*/
{
	/* leave it as 8 if explicitly set */
	libpatchsize  = (libpatchsize == 8) ? 8 : (bytesperword == 2) ? 4 : 6;
	targetintsize = (bytesperword == 2) ? S_INT16 : S_INT32;
	wordshift     = (bytesperword == 2) ? 1 : 2;
	fast_negative_prod = has_dup; /* Not the T414 and T212 */

	instr_class.hasprocessmodel = tx_global.hasrmccore || tx_global.hseries;
	instr_class.haserrorhandling = tx_global.hasrmccore || tx_global.hseries;
	instr_class.common_t450_hseries = tx_global.hasrmccore ||tx_global.hseries;
	#if !defined(COMPILING_TO_JCODE)
		/*use_bsub_not_adc = H_series;*/
		use_bsub_not_adc = T9000_instruction_timings && ((code_style_flags & CODE_STYLE_SPACE_NOT_TIME) == 0);
		/* bug TS/1504 07/04/92 */
	#endif
	use_bsub_not_adc |= kroc_flag; /* inappropriate for non-transputer target -- DCW */

	if (alpha && gamma) {
		harnesserror("Cannot specify both ALPHA and GAMMA on command line");
	}
	if (alpha && gammae) {
		harnesserror("Cannot specify both ALPHA and GAMMAE on command line");
	}
	if (gamma && gammae) {
		harnesserror("Cannot specify both GAMMA and GAMMAE on command line");
	}
	if (H_series && alpha) {
		tx_global.hast9alphaprobs = TRUE;
	}
	if (H_series && (gamma || gammae)) {
		/* make sure that all the bits are OK incase we used '-gamma(e)'
		 * rather than '-t9000gamma(e)'
		 */
		if (gamma) {
			setprocessor("T9000GAMMA");
		} else {
			setprocessor("T9000GAMMAE");
		}
	}
	#if !defined(COMPILING_TO_JCODE)
		if (T9000_alpha_nofprem(&tx_global)) {
			/* Section 4.8  of SW-0341-2 */ /* bug TS/1729 15/06/92 */
			has_fprem = FALSE;
		}
		if (T9000_alpha_nocrc(&tx_global)) {
			/* Section 5.10 of SW-0341-2 */
			has_crc   = FALSE;
		}
	#endif
	use_shortintops = has_shortintops;
	if (T9000_alpha_nolsx(&tx_global) || T9000_alpha_noxsword(&tx_global)) {
		use_shortintops = FALSE;
	}
	if (force_shortintops) {
		has_shortintops = TRUE;
		use_shortintops = TRUE;
	}
	if (!extlibfilename_overruled) {
		#if 0
		const char *extlibname = H_series ? "occam9.lib" : (bytesperword == 2 ? "occam2.lib" : (has_fpu_core ? "occam8.lib" : (tx_global.hasrmccore ? "occam450.lib" : "occama.lib")));
		#endif
		const char *extlibname = "forall.lib";

		strcpy(extlibfilename, extlibname);
	}
	/* NOTE - value of L_process MUST be exactly 1 or 0 */
	/*assert((L_process & ~1) == 0);*/
	if ((L_process & ~1) != 0) {
		abort();
	}

	if (L_process) {
		code_style_flags |= CODE_STYLE_ALT_PRI_PAR;
		errormodes_off_mask |= H1_ERRORMODE_OFF_MASK;
	}

	{
		BOOL require_call_io;
		#if defined(CONFIG2)
			/* bug INSdi01578 - require call_io if virtual routing is used! */
			require_call_io = interactive_debug || virtual_routing || (!setymode);
		#else
			require_call_io = !setymode;
		#endif

		if ((processortype & ARCH_T) || (processortype & ARCH_H)) {
			processorattr = (processorattr & ~(ATTRIB_IO_MASK | ATTRIB_ERROR_MASK))
				| processorattr_errmode | (require_call_io ? ATTRIB_CALL_IO : ATTRIB_INSTR_IO);
		} else {
			abort();
		}
	}
	if ((processorattr & ATTRIB_WORD_64) != 0) {
		needs_quadalign = TRUE;
	}
}
/*}}}*/
/*{{{  PUBLIC void setprocessordefault()*/
#ifdef DEFAULT_PROCESSOR_TYPE
PUBLIC void setprocessordefault(void)
{
	setprocessor(DEFAULT_PROCESSOR_TYPE);
	setprocessorattr();
}
#endif
/*}}}*/
/*{{{  PUBLIC BIT32 typeofprocessor ( char *name )   NEVER USED*/
#if 0 /* typeofprocessor is never used */
/* Return processor type from processor string */
PUBLIC BIT32 typeofprocessor (const char *const name)
{
	int i;

	for (i = 0; i < (sizeof(ptypes) / sizeof(struct proctype_struct)); i++) {
		if (strcmp(name, ptypes[i].pstring) == 0) {
			return (ptypes[i].pinstr);
		}
	}
	return (0);
}
#endif /* typeofprocessor is never used */
/*}}}*/
/*}}}*/

/*{{{  option handling*/
/*{{{  prototypes*/
BOOL parse_command_line(int argc, const char *argv[]);
/*
   most of the option functions have the same arg list, imposed by arglib.
   many are not used. see the spec doc for details
*/
PRIVATE arg_control optzed(const char *, const char *, void *);
PRIVATE arg_control optwarnings(const char *, const char *, void *);
PRIVATE arg_control set_flag(const char *, const char *, void *);
PRIVATE arg_control clear_flag(const char *, const char *, void *);
PRIVATE arg_control set_help_use(const char *, const char *, void *);
PRIVATE arg_control set_help_long(const char *, const char *, void *);
PRIVATE arg_control set_help_extended(const char *, const char *, void *);
PRIVATE arg_control finish_command_line_args(const char *, const char *, void *);
/*}}}*/
/*{{{  functions to set options*/
/*{{{  PRIVATE const char * upper*/
PRIVATE const char *upper(const char *opt)
{
	char *x = (char *)memalloc (strlen(opt)+1);
	char *c = x;

	strcpy(c, opt);
	opt = c;
	do {
		if (islower(*c)) {
			*c = toupper(*c);
		}
	} while (*c++);
	return(x);
}
/*}}}*/
/*{{{  PRIVATE arg_control optseterrormode*/
PRIVATE arg_control optseterrormode ( const char *opt, const char *arg, void *data )
{
	int i;

	/*{{{  assertions*/
	#if 0 /* NOT T9000_HAS_Y */

	/* Elsewhere we rely on these assertions being TRUE, so that we
	 * don't keep having to check for ARCH_T and ARCH_H.
	 * We then have to wrap them up in #ifs, so that we don't get lots
	 * of warning: statement with no effect.
	 */
	#if H_ATTRIB_UNIVERSAL != ATTRIB_UNIVERSAL
		assert (H_ATTRIB_UNIVERSAL == ATTRIB_UNIVERSAL);
	#endif

	#if H_ATTRIB_HALT != ATTRIB_HALT
		assert (H_ATTRIB_HALT == ATTRIB_HALT);
	#endif

	#if H_ATTRIB_STOP != ATTRIB_STOP
		assert (H_ATTRIB_STOP == ATTRIB_STOP);
	#endif
	#endif
	/*}}}*/

	opt = upper(opt);
	for (i = 0; i < (sizeof(errmodes) / sizeof(struct errmode_struct)); i++) {
		if (opt[0] == errmodes[i].emodechar) {
			if ((errormode != UNKNOWN_ERROR_MODE) && (errormode != errmodes[i].emode)) {
				harnesserror("Duplicate error modes on command line");
			}
			errormode = errmodes[i].emode;
			assert_errormode = errormode; /* INSdi01337 */
			processorattr_errmode = errmodes[i].eattr;
			return TRUE;
		}
	}
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE void finish_command_line_args*/
PRIVATE arg_control finish_command_line_args(const char *opt, const char *arg, void *data)
{
	DEBUG_MSG(("finish_command_line_args, processortype: %x\n", processortype));

	if (sourcefilename[0] == '\0') {
		return(arg_continue);
	}

	if (processortype == UNKNOWN_PROCESSOR_TYPE) {
	#ifdef DEFAULT_PROCESSOR_TYPE
		setprocessor(DEFAULT_PROCESSOR_TYPE);
	#else
		harnesserror("Target processor must be specified");
	#endif
	}
	if (errormode == UNKNOWN_ERROR_MODE) {
		(void)optseterrormode (DEFAULT_ERROR_MODE, NULL, NULL);
	}
	has_variableio = has_variableio && permit_variableio;

	needs_quadalign |= swap_r64words | kroc_flag; /* always harmless; may be useful -- DCW */
	setprocessorattr(); /* merges extra attributes with the bit pattern */
	seterrormode();     /* merges error modes etc with the bit pattern */
	t450a_workarounds = has_rmc_core && t450a_workarounds_flag;

	/*{{{  extra stuff for the KRoC target processor*/
		/*{{{  KROC or anything else (see table in GEN2)*/
		if (etc_output) {
			has_pop = TRUE;             /* simplifies stack handling */
			/* was target_cpu_386 - hack for Linux version */
			has_fmul = TRUE;            /* we support this now. */
			has_fprem = has_fpu_core;   /* avoids need to implement fpremstep exactly as tptr */
			has_i64tor = has_fpu_core;  /* allows optimised long int to real conversion */
			has_ordered = has_fpu_core; /* detection of NaN, etc by instructions */
			has_move2d = FALSE;         /* avoids need to implement as instructions */
			has_bitops = FALSE;         /* avoids need to implement as instructions */
			has_crc = FALSE;            /* avoids need to implement as instructions */
		} else {
			has_i64tor = FALSE;
			has_ordered = has_fpu_core;
		}
		has_sqrt = has_fpu_core;
		has_fpint = has_fpu_core;
		asm_uppercase = FALSE;
		#ifdef NEED_QUAD_ALIGNMENT
			needs_quadalign = TRUE;
		#endif
		/*}}}*/
	/*}}}*/
	#if defined(CONFIG2) || defined(CONFIG3)
		if (!interactive_debug && !advanced_debug) {
			(void)cf_set_param(&cf_params, "NI");
		}
		#if defined(CONFIG2)
			iobycall = interactive_debug || virtual_routing || iobycall;
		#else
			iobycall = iobycall;
		#endif
		{
			const char *msg = cf_check_options(&cf_params);

			if (msg != NULL) {
				harnesserror(msg);
			}
		}
	#endif
	realign_virtual_channels = H_series && iobycall;
	return (arg_continue);
}
/*}}}*/

#if !(defined(CONFIG2) || defined(CONFIG3))
/*{{{  PRIVATE arg_control optprocessor*/
PRIVATE arg_control optprocessor ( const char *opt, const char *arg, void *data )
{
	const BIT32 old = processortype;
	const BIT32 new = setprocessor(upper(opt));

	if ((old != UNKNOWN_PROCESSOR_TYPE) && (old != new)) {
		harnesserror("Duplicate processor types on command line");
	}
	if (new == UNKNOWN_PROCESSOR_TYPE) {
		harnesserror_s("Unknown processor type ", opt);
	}
	return (new != UNKNOWN_PROCESSOR_TYPE);
}
/*}}}*/
/*{{{  PRIVATE arg_control optetc*/
PRIVATE arg_control optetc ( const char *opt, const char *arg, void *data )
{
	BIT32 new = processortype;

	if (processortype == UNKNOWN_PROCESSOR_TYPE) {
		new = optprocessor("T8", arg, data);
	}
	etc_output = TRUE;
	return (new != UNKNOWN_PROCESSOR_TYPE);
}
/*}}}*/
/*{{{  PRIVATE arg_control optnoalias*/
PRIVATE arg_control optnoalias (const char *opt, const char *arg, void *data)
{
	checkalias = FALSE;
	checkusage = FALSE;
	return (arg_continue);
}
/*}}}*/
#endif
/*{{{  PRIVATE arg_control optsetymode*/
PRIVATE arg_control optsetymode ( const char *opt, const char *arg, void *data )
{
	iobycall = FALSE;
	setymode = TRUE;
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control optfullguys*/
PRIVATE arg_control optfullguys ( const char *opt, const char *arg, void *data )
{
	guyinserts = FULLGUYS;
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control optseqguys*/
PRIVATE arg_control optseqguys ( const char *opt, const char *arg, void *data )
{
	guyinserts = SEQUENTIALGUYS;
	return (arg_continue);
}
/*}}}*/
#if 0
/*{{{  PRIVATE arg_control optmakelib*/
PRIVATE arg_control optmakelib ( const char *opt, const char *arg, void *data )
{
	compilemode = COMP_LIB;
	return TRUE;
}
/*}}}*/
#endif /* 0 */
#if !defined(COMPILING_TO_JCODE)
/*{{{  PRIVATE arg_control optcodesize*/
PRIVATE arg_control optcodesize (const char *opt, const char *arg, void *data)
{
	const size_t len = strlen(arg);

	/*printf("optcodesize: arg is \"%s\"\n", arg);*/
	if (len > 0 && (len == strspn(arg, "0123456789"))) {
		req_code_size = atoi(arg);
		return (arg_continue);
	}
	harnesserror("Code size not correctly specified");
	return (arg_terminate);
}
/*}}}*/
#endif
/*{{{  PRIVATE arg_control optzsuffix*/
PRIVATE arg_control optzsuffix(const char *opt, const char *arg, void *data)
{
	opt++;                          /* lose the switch character */
	opt = upper(opt);
	if (strlen(arg) < MAX_LIBNAMESSUFFIX) {
		if (opt[2] == 'I') {
			/* ZLIS */
			strcpy(vlibsuffixstring, arg);
			vlibsuffix = &vlibsuffixstring[0];
		} else if (opt[3] == 'P') {
			/* ZLCP */
			strcpy(predefsuffixstring, arg);
			predefsuffix = &predefsuffixstring[0];
		} else {
			/* if (opt[3] == 'S') */ /* ZLCS */
			strcpy(compilersuffixstring, arg);
			compilersuffix = &compilersuffixstring[0];
		}
		return TRUE;
	} else {
		harnesserror("Suffix too long");
		return FALSE;
	}
}
/*}}}*/
/*{{{  PRIVATE BOOL set_filename*/
PRIVATE BOOL set_filename(char *const target_filename, const char *const err_string, const BOOL permit_duplicate, const char *opt)
{
	if (strlen(opt) > 0) {
		/* check for duplicate on all non-Z command line options */
		if (!permit_duplicate && (target_filename[0] != '\0')) {
			/* INSdi03554 */
			harnesserror_sssss("Duplicate ", err_string, " file name: \"", opt, "\"");
			return FALSE;
		}
		if (strlen(opt) > MAX_OPTION_LENGTH) {
			harnesserror_toobig("filename too long", strlen(opt), MAX_OPTION_LENGTH);
			return FALSE;
		}
		strcpy(target_filename, opt);
		return TRUE;
	} else {
		harnesserror_s("Missing file name", err_string);
		return FALSE;
	}
}
/*}}}*/
/*{{{  PRIVATE arg_control optobjfilename*/
PRIVATE arg_control optobjfilename ( const char *opt, const char *arg, void *data )
{
	#if defined(CONFIG2) || defined(CONFIG3)
		return set_filename (cfbfilename, "object", FALSE, arg);
	#else
		return set_filename (objfilename, "object", FALSE, arg);
	#endif
}
/*}}}*/
/*{{{  PRIVATE arg_control optoutputfilename*/
PRIVATE arg_control optoutputfilename ( const char *opt, const char *arg, void *data )
{
	/* printf("setting optoutputfilename %s\n", arg); */
	if (strlen(arg) > 0 && arg[0] != '-') {
		outfile = fopen(arg, "w");
		if (outfile == NULL) {
			harnesserror_s("Cannot open output file ", arg);
			return FALSE;
		}
		(void)setvbuf (outfile, NULL, _IOLBF, BUFSIZ);
		errfile = outfile;

		#if defined(CONFIG2) || defined(CONFIG3)
			/* we re-initialise outfile incase we have changed it
			 * with a command line parameter */
			cf_params.cf_outfile = outfile;
		#endif
		return (arg_continue);
	} else {
		harnesserror("Missing output file name");
		return (arg_terminate);
	}
}
/*}}}*/
/*{{{  PRIVATE arg_control optxmlfilename (const char *opt, const char *arg, void *data)*/
PRIVATE arg_control optxmlfilename (const char *opt, const char *arg, void *data)
{
	if ((strlen (arg) > 0) && (arg[0] != '-')) {
		outxmlfile = fopen (arg, "w");
		if (!outxmlfile) {
			harnesserror_s ("cannot open XML output file ", arg);
			return FALSE;
		}
		(void)setvbuf (outxmlfile, NULL, _IOLBF, BUFSIZ);

		return arg_continue;
	} else {
		harnesserror ("missing XML output file name");
		return arg_terminate;
	}
}
/*}}}*/
/*{{{  PRIVATE arg_control optextlibfilename*/
PRIVATE arg_control optextlibfilename(const char *opt, const char *arg, void *data)
{
	extlibfilename_overruled = TRUE;
	return set_filename(extlibfilename, "compiler library", TRUE, arg);
}
/*}}}*/
/*{{{  PRIVATE arg_control optvlibsfilename*/
PRIVATE arg_control optvlibsfilename(const char *opt, const char *arg, void *data)
{
	return set_filename(vlibsfilename, "io library", TRUE, arg);
}
/*}}}*/
#if !(defined(CONFIG2) || defined(CONFIG3))
/*{{{  PRIVATE arg_control optmapfilefilename*/
PRIVATE arg_control optmapfilefilename(const char *opt, const char *arg, void *data)
{
	return set_filename(mapfilefilename, "map file", TRUE, arg);
}
/*}}}*/
#endif
/*{{{  PRIVATE arg_control optnovecspace*/
PRIVATE arg_control optnovecspace ( const char *opt, const char *arg, void *data )
{
	vsenabled = FALSE;
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control optnorangecheck*/
PRIVATE arg_control optnorangecheck ( const char *opt, const char *arg, void *data )
{
	errormodes_off_mask |= ERRORMODE_RANGECHECK;
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control optnoanycheck*/
PRIVATE arg_control optnoanycheck ( const char *opt, const char *arg, void *data )
{
	errormodes_off_mask |= ERRORMODE_OFF_MASK;
	return TRUE;
}
/*}}}*/
#if defined(CONFIG2) || defined(CONFIG3)
/*{{{  PRIVATE arg_control optrom*/
PRIVATE arg_control optrom (const char *opt, const char *arg, void *data)
{
	(void)cf_set_param (&cf_params, "RM");
	opt = upper (opt);
	if (opt[1] == 'O') {
		(void)cf_set_param (&cf_params, opt);
	}
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control optconfig*/
PRIVATE arg_control optconfig(const char *opt, const char *arg, void *data)
{
	(void)cf_set_param (&cf_params, opt);
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control optadvanced*/
PRIVATE arg_control optadvanced(const char *opt, const char *arg, void *data)
{
	(void)cf_set_param(&cf_params, opt);
	advanced_debug = TRUE;
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control optdynamic*/
PRIVATE arg_control optdynamic(const char *opt, const char *arg, void *data)
{
	(void)cf_set_param (&cf_params, "GA");
	(void)cf_set_param (&cf_params, opt);
	advanced_debug = TRUE;
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control optsyslibname*/
PRIVATE arg_control optsyslibname (const char *opt, const char *arg, void *data)
{
	return set_filename((char *)syslibname, "system library", TRUE, arg);
}
/*}}}*/
/*{{{  PRIVATE arg_control optviribname*/
PRIVATE arg_control optvirlibname (const char *opt, const char *arg, void *data)
{
	return set_filename(virlibname, "virtual routing library", TRUE, arg);
}
/*}}}*/
/*{{{  PRIVATE arg_control optdeblibname*/
PRIVATE arg_control optdeblibname (const char *opt, const char *arg, void *data)
{
	return set_filename(deblibname, "debug library", TRUE, arg);
}
/*}}}*/
/*{{{  PRIVATE arg_control optprolibname*/
PRIVATE arg_control optprolibname (const char *opt, const char *arg, void *data)
{
	return set_filename(prolibname, "profiler library", TRUE, arg);
}
/*}}}*/
#endif
#if defined(CONFIG2)
/*{{{  PRIVATE arg_control optinteractive*/
PRIVATE arg_control optinteractive(const char *opt, const char *arg, void *data)
{
	interactive_debug = TRUE;
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control optnovirtualroute*/
PRIVATE arg_control optnovirtualroute(const char *opt, const char *arg, void *data)
{
	(void)cf_set_param (&cf_params, opt);
	virtual_routing = FALSE;
	return TRUE;
}
/*}}}*/
#endif
#if defined(CONFIG3)
/*{{{  PRIVATE arg_control optreservedsize*/
PRIVATE arg_control optreservedsize(const char *opt, const char *arg, void *data)
{
	const size_t len = strlen(arg);

	/*printf("optreservedsize: arg is \"%s\"\n", arg);*/
	if (len > 0 && (len == strspn(arg, "0123456789"))) {
		cf_params.cf_reservedsize = atoi(arg);
		cf_params.cf_reservedsizeset = TRUE;
		return TRUE;
	}
	harnesserror("Reserved memory size not correctly specified");
	return FALSE;
}
/*}}}*/
#endif
/*{{{  PRIVATE arg_control optwarnings (Wn options and NWn options)*/
PRIVATE arg_control optwarnings(const char *opt, const char *arg, void *data )
{
	/* Z prefix shift the rest by 1 */
	const int i = ((islower(opt[0]) ? toupper(opt[0]) : opt[0]) == 'Z') ? 1 : 0;
	/* skip any 'N' prefix */
	const int j = ((islower(opt[i]) ? toupper(opt[i]) : opt[i]) == 'W') ? i+1 : i+2;

	opt = upper(opt);
	switch (opt[j]) {
	/*{{{  WA...*/
	case 'A' :
		if (opt[j+2] == 'I') {
			/* "WALIGN" */
			warning_flags |= WARNING_ALIGNMENT;
		} else {
			/* "WALL" */
			/* turn on all warnings */
			(void)optwarnings ("WD", arg, data);
			(void)optwarnings ("WO", arg, data);
			(void)optwarnings ("WQUAL", arg, data);
			(void)optwarnings ("WALIGN", arg, data);
		}
		break;
	/*}}}*/
	/*case 'C' : warning_flags &= ~WARNING_CSE;                           break;*/
	case 'C':
		warning_flags &= ~WARNING_CHANOFANY;
		break;
	case 'D':
		warning_flags |= (WARNING_DESCOPED_N | WARNING_DESCOPED_P);
		break;
	case 'F':
		warning_flags &= ~WARNING_DESCOPED_P;
		break;
	case 'G':
		warning_flags &= ~WARNING_GUY;
		break;
	case 'O':
		warning_flags |=  WARNING_OVERLAPS;
		break;
	case 'P':
		warning_flags &= ~WARNING_UNUSED_P;
		break;
	case 'T':
		warning_flags |=  WARNING_TAG_INPUT;
		break;
	case 'U':
		warning_flags &= ~(WARNING_UNUSED_V | WARNING_UNUSED_R);
		break;
	case 'Q':
		warning_flags |= (WARNING_CASE_INPUT | WARNING_BAD_PLACE);
		break;
	}
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control optfavourspace*/
PRIVATE arg_control optfavourspace ( const char *opt, const char *arg, void *data )
{
	code_style_flags |= CODE_STYLE_SPACE_NOT_TIME;
	#if defined(COMPILING_TO_JCODE)
		opt_prefer_time_to_space(FALSE);
	#endif
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control optfavourtime*/
PRIVATE arg_control optfavourtime ( const char *opt, const char *arg, void *data )
{
	code_style_flags &= ~CODE_STYLE_SPACE_NOT_TIME;
	#if defined(COMPILING_TO_JCODE)
		opt_prefer_time_to_space(TRUE);
	#endif
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control oc_optimisations*/
PRIVATE arg_control oc_optimisations(const char *opt, const char *arg, void *data)
{
	#if defined(COMPILING_TO_JCODE)
		/* opt[0]=='O' */
		switch (opt[1]) {
		case '0':
			OPTIMISATION_LEVEL_0_INIT;
			break;
		case '1':
			OPTIMISATION_LEVEL_1_INIT;
			break;
		case '2':
			OPTIMISATION_LEVEL_2_INIT;
			break;
		}
	#endif
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control set_flag*/
PRIVATE arg_control set_flag(const char *opt, const char *arg, void *data)
{
	/* printf("set flag: %s\n", opt); */
	*((BOOL *)data) = TRUE;
	return (arg_continue);
}
/*}}}*/
/*{{{  PRIVATE arg_control clear_flag*/
PRIVATE arg_control clear_flag(const char *opt, const char *arg, void *data)
{
	/* printf("clear flag: %s\n", opt); */
	*((int *)data) = FALSE;
	return (arg_continue);
}
/*}}}*/
/*{{{  PRIVATE arg_control get_source_file*/
PRIVATE arg_control get_source_file(const char *opt, const char *arg, void *data)
{
	if (sourcefilename[0] == '\0') {
		if (strlen(opt) > MAX_OPTION_LENGTH) {
			harnesserror_toobig("Source filename too long", strlen(opt), MAX_OPTION_LENGTH);
			return (arg_terminate);
		}
		strcpy(sourcefilename, opt);
	} else {
		harnesserror_s("Duplicate source file name:", opt);
		return (arg_terminate);
	}
	/* printf("sourcefile: %s\n", opt); */
	return (arg_continue);
}
/*}}}*/
/*{{{  PRIVATE arg_control optzed*/
PRIVATE arg_control optzed ( const char *opt, const char *arg, void *data )
{
	opt = upper(opt);
	/* opt[0]=='Z' */
	switch (opt[1]) {
	default:
		zinfo = TRUE;
		break;
	/*{{{  ZB*/
	case 'B':
		#if defined(COMPILING_TO_JCODE)
			/* The Booleans are used by the asm files
			 * of the "C" compiler */
			asmtostdout =  TRUE;
			suppress_asm = TRUE;
		#else
			assembly_output = TRUE;
		#endif
		break;
	/*}}}*/
	/*{{{  ZD... and ZE...*/
	#if !defined(COMPILING_TO_JCODE)
	case 'D':
		disassemble = diagnostics = TRUE;
		break;
	case 'E':
		if (opt[2] == 'R') {
			/* ZER64 */
			swap_r64words = TRUE;
		} else {
			hidelibnames = FALSE;
		}
		break;
	#else
	case 'D':
		if (opt[2] == '\0') {
			disassemble = diagnostics = TRUE;
		} else {
			set_flags_opt(&opt[2], FALSE);
		}
		break;
	case 'E':
		if (opt[2] == '\0') {
			hidelibnames = FALSE;
		} else {
			set_flags_opt(&opt[2], TRUE);
		}
		break;
	#endif
	/*}}}*/
	/*{{{  ZH*/
	case 'H':
		object_file_wrt_flags |= OBJ_FILE_WRT_OCCAM_HARNESS;
		break;
	/*}}}*/
	/*{{{  ZK*/
	case 'K':
		kroc_flag = TRUE;
		/* omit transputer optimizations */
		break;
	/*}}}*/
	/*{{{  ZL*/
	case 'L':
		onlylex = TRUE;
		(void)set_flag (NULL,NULL,&brieferrors);
		break;
	/*}}}*/
	/*{{{  ZMEM*/
	#if defined(HOST_OS_IS_UNIX) && !defined(WIN32)
	case 'M':
		memstats = TRUE;
		break;
	#endif
	/*}}}*/
	/*{{{  ZQ*/
	case 'Q':
		/*{{{  ZQA*/
		if (opt[2] == 'A' && opt[3] == '\0') {
			needs_quadalign = TRUE;
		/*}}}*/
		} else {
			#if defined(COMPILING_TO_JCODE)
			debug_set(&opt[2]);
			#endif
		}
		break;
	/*}}}*/
	/*{{{  ZU...*/
	case 'U':
		switch(opt[2]) {
		case 'S':
			force_shortintops = TRUE;
			break;
		}
		break;
	/*}}}*/
	/*{{{  ZV*/
	case 'V':
		iobycall = FALSE;
		/* Leave 'setymode' alone */
		break;
	/*}}}*/
	/*{{{  ZX*/
	case 'X':
		libpatchsize = 8;
		break;
	/*}}}*/
	/*{{{  ZA...*/
	case 'A':
		switch(opt[2]) {
		case '\0':
			assembly_output = diagnostics = TRUE;
			break;
		#if defined(COMPILING_TO_JCODE)
		default:
		{
			char str[MAX_OPTION_LENGTH];
			strcpy(str, "-");
			next_asm_argv(strcat(str, opt + 2));
		}
			break;
		#else
		case 'M':
			alloc_strategy |= ALLOC_MAXBELOW;
			break;
		case 'N':
			alloc_strategy |= ALLOC_NODIVBYSIZE;
			break;
		case 'S':
			/*alloc_strategy |= ALLOC_BYSCOPE;*/
			switch(opt[3]) {
			default:
				alloc_strategy |= ALLOC_BYSCOPE;
				break;
			case 'M':
				ims_asm_output = assembly_output = TRUE;
				break;
			}
			break;
		#endif
		}
		break;
	/*}}}*/
	/*{{{  ZC...*/
	case 'C':
		{
			BOOL ok = FALSE;
			
#if defined(CONFIG2) || defined(CONFIG3)
			ok = cf_set_param(&cf_params, arg);

			if (ok) {
				break;		/* switch() */
			}
#endif
			if (!ok) {
				/* must be one of the other ZC's (e.g. ZCXDIV/ZCXREM/ZCTT/ZCTUIO/ZCTKNSF) */
				switch (opt[3]) {
				case 'D':
					kroc_complex_div = TRUE;
					ok = TRUE;
					break;
				case 'R':
					kroc_complex_rem = TRUE;
					ok = TRUE;
					break;
				case 'T':			/* -zctt */
					kroc_chantype_desc = TRUE;
					ok = TRUE;
					break;
				case 'U':			/* -zctuio */
					kroc_chantype_uio = TRUE;
					ok = TRUE;
					break;
				case 'K':			/* -zctknsf */
					kroc_chantype_knsf = TRUE;
					ok = TRUE;
					break;
				default:
					harnesserror_s ("Unknown compiler option: ", opt);
					break;
				}
			}
#if defined(CONFIG2) || defined(CONFIG3)
			if (!ok) {
				harnesserror_s("Unknown configurer diagnostic: ", arg);
			}
#endif
		}
		break;
	/*}}}*/
	/*{{{  ZO...*/
	case 'O':
		switch(opt[2]) {
			/* removed source_output 14/3/91 */
			/*default: minimal_debugoutput = FALSE;*/ /* need full debug info */
			/*     source_output = assembly_output = debugoutput = TRUE;        break;*/
		#if !defined(COMPILING_TO_JCODE)
		case 'D':
			object_file_wrt_flags |= OBJ_FILE_WRT_MERGE_DESCRIPTORS;
			break;
		#endif
		case 'E':
			object_file_wrt_flags |= OBJ_FILE_WRT_NO_EXPORT_ORIGINS;
			break;
		case 'I':
			object_file_wrt_flags |= OBJ_FILE_WRT_NO_IMPORT_ORIGINS;
			break;
		case 'L':
			object_file_wrt_flags |= OBJ_FILE_WRT_PRI_TEXT_SECTION;
			break;
		#if !defined(COMPILING_TO_JCODE)
		case 'N':
			object_file_wrt_flags |= OBJ_FILE_WRT_SHORT_DESCRIPTORS;
			break;
		#endif
		case 'P':
			(void)optfavourspace (NULL, NULL, NULL);
			break;
		case 'R':
			read_all_libs = TRUE;
			break;
		case 'V':
			hash_version_string = FALSE;
			break;
		case 'W':
			object_file_wrt_flags |= OBJ_FILE_WRT_NO_WS_SYMBOLS;
			break;
		#if !defined(COMPILING_TO_JCODE)
		case 'Y':
			object_file_wrt_flags |= OBJ_FILE_WRT_SPACES_DESCRIPTORS;
			break;
		#endif
		}
		break;
	/*}}}*/
	/*{{{  ZP...*/
	case 'P':
		switch(opt[2]) {
		case 'R':
			print_all_messages = TRUE;
			break;
		#if !defined(COMPILING_TO_JCODE)
		case 'N':
			code_style_flags |= CODE_STYLE_NO_NULL_TRAPS;
			break;
		#else
		case 'S':
			pragma_set(arg);
			break;
		#endif
		case 'D':
			trace_prof_image = TRUE;
			break;
		}
		break;
	/*}}}*/
	/*{{{  ZN...*/
	case 'N' :
		switch (opt[2]) {
		case 'A':
			code_style_flags |= CODE_STYLE_NO_PREAMBLE;
			break;
		/*{{{  'C'*/
		case 'C':
			switch (opt[3]) {
			default:
				permit_variableio = FALSE;
				break;
			case 'C':
				suppress_call_compatibility_check = TRUE;
				break;
			}
			break;
		case 'D':
			debugoutput = FALSE;
			break;
		/*}}}*/
		/*{{{  'E'*/
		case 'E' :
			switch (opt[3]) {
			case 'A':
				warning_flags |= WARNING_BAD_ASM;
				break;
			case 'C':
				lexer_ignore_comments = TRUE;
				break;
			case 'R':
				warn_on_all_errors = TRUE;
				break;
			}
			break;
		/*}}}*/
		case 'J':
			code_style_flags |= CODE_STYLE_CJ_NOT_J;
			break;
		case 'I':
			allow_inlines = FALSE;
			break;
		#if !defined(COMPILING_TO_JCODE)
		case 'L':
			alloc_strategy |= ALLOC_NOLINENUMS;
			break;
		case 'M':
			alloc_strategy |= ALLOC_NOCOMMENTS;
			break;
		case 'O':
			alloc_strategy |= ALLOC_NOOPERANDS;
			break;
		#endif
		case 'P':
			allowpredefs = FALSE;
			break;
		#if !defined(COMPILING_TO_JCODE)
		case 'V':
			alloc_strategy |= ALLOC_NOVSOPT;
			break;
		#endif
		}
		break;
	/*}}}*/
	/*{{{  ZS...*/
	case 'S':
		switch(opt[2]) {
		case 'C':
			call_alias_check = FALSE;
			(void)clear_flag (NULL, NULL, &generatecode);
			break;
		case 'E':
			crasherrors = TRUE;
			break;
		case 'M':
			stop_after_map = TRUE;
			break;
		case 'P':
			call_alias_check = FALSE;
			nochecking = TRUE;
			(void)clear_flag (NULL, NULL, &generatecode);
			break;
		case 'R':
			prsrc = TRUE;
			diagnostics= TRUE;
			break;
		case 'T':
			stop_after_trans = TRUE;
			break;
		case 'U':
			debuguse = TRUE;
			(void)clear_flag (NULL ,NULL, &generatecode);
			break;
		}
		break;
	/*}}}*/
	/*{{{  ZT...*/
	case 'T':
		switch(opt[2]) {
		default:
			prtree = TRUE;
			diagnostics= TRUE;
			break;
		case 'S':
			testflag = TRUE;
			break;
		case 'X':
			prxmltree = TRUE;
			break;
		case '4':
			t450a_workarounds_flag = FALSE;
			break;
		}
		break;
	/*}}}*/
	/*{{{  ZW...*/
	case 'W':
		switch(opt[2]) {
		default:
			chanaspointer = FALSE;
			break;
		case 'A':
			warn_on_usage_error = TRUE;
			break;
		case 'C':
			warn_comment_indent = TRUE;
			break;
		}
		break;
		#if 0
		case 'W':
			pragma_set(arg);
			break;
		#endif
	/*}}}*/
	/*{{{  ZY*/
	case 'Y':
		visible_tags = TRUE;
		break;
	/*}}}*/
	/*{{{  ZZ...*/
	case 'Z':
		switch (opt[2]) {
		case '\0':
			code_style_flags |= CODE_STYLE_ALT_PRI_PAR;
			break;
		case 'S':
			has_sincostan = TRUE;
			break;
		}
		break;
	/*}}}*/
	}
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE arg_control optpathname*/
PRIVATE arg_control optpathname (const char *opt, const char *arg, void *data)
{
	return set_filename (pathname, "(env var name)", TRUE, arg);
}
/*}}}*/
/*{{{  PRIVATE arg_control optdefine (const char *opt, const char *arg, void *data)*/
PRIVATE arg_control optdefine (const char *opt, const char *arg, void *data)
{
	if (fe_preproc_cmdline_define (arg) == TRUE) {
		return (arg_continue);
	}
	return (arg_terminate);
}
/*}}}*/

#if 0
/*{{{  PRIVATE BOOL optexecute*/
PRIVATE arg_control optexecute(const char *opt, const char *arg, void *data  )
{
	#ifdef IMS
	opt = upper(opt);
	repeat_loop = (opt[1] == 'M');

	#if defined(CONFIG2) || defined(CONFIG3)
		/* This is used because the backend can abort directly if it gets a
		 * fatal error */
		/*cf_params.cf_repeat = repeat_loop;*/
		if (repeat_loop) {
			(void)cf_set_param(&cf_params, "L");
		}
	#endif
	#endif	/* IMS */
	return TRUE;
}
/*}}}*/
#endif

#if !(defined(CONFIG2) || defined(CONFIG3))
/*{{{  PRIVATE arg_control optzgrp*/
PRIVATE arg_control optzgrp(const char *opt, const char *arg, void *data)
{
	optzed ("ZB",NULL,NULL);
	optzed ("ZNL",NULL,NULL);
	optzed ("ZNM",NULL,NULL);
	optzed ("ZNO",NULL,NULL);
	return TRUE;
}
/*}}}*/
#endif
/*}}}*/

/*}}}*/
/*{{{  option error handling functions*/
/*{{{  PRIVATE arg_control general_error*/
PRIVATE arg_control general_error(const char *opt, const char *arg, void *data)
{
	harnesserror_s ("Invalid command line option: ", opt);
	return (arg_terminate);
}
/*}}}*/


/*{{{  PRIVATE arg_control syntax_error*/
PRIVATE arg_control syntax_error(const char *opt, const char *arg, void *data)
{
	harnesserror_s ("Syntax error in command line option: ", opt);
	return (arg_terminate);
}
/*}}}*/


/*{{{  PRIVATE arg_control file_error*/
PRIVATE arg_control file_error(const char *opt, const char *arg, void *data)
{
	harnesserror_s ("Cannot read indirect file: ", opt);
	return (arg_continue);
}
/*}}}*/
/*}}}*/
/*{{{  option table*/
#define HELP_USE (arg2_help_short|arg2_help_long|arg2_help_extended) /* basic help page */
#define HELP_FUL (arg2_help_long|arg2_help_extended)           /* help page with -help option */
#define HELP_ZED (arg2_help_extended)                          /* help from -z option */

const arg2_descriptor cloptions[] = {
	/*{{{  a - e*/
	#if !(defined(CONFIG2) || defined(CONFIG3))
	{"AXP",       arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to DEC Alpha AXP"},
	{"ALPHA",     arg2_single,    &alpha,         set_flag,           HELP_FUL, "alpha revision of chip"},
	{"A",         arg2_single,    NULL,           optnoalias,         HELP_FUL, "disable alias checking"},
	#endif
	{"B",         arg2_single,    &brieferrors,   set_flag,           HELP_FUL, "display brief error messages"},
	#if !defined(COMPILING_TO_JCODE)
	{"CODE",      arg2_operand,   NULL,           optcodesize,        HELP_FUL, "<size>|set code buffer to <size> K-bytes"},
	#endif
	#if defined(COMPILING_TO_JCODE)
	{"COMPACT",   arg2_single,    &compactable_output, set_flag,      HELP_FUL, "produce compactable code"},
	#endif
	{"C",         arg2_single,    &generatecode,  clear_flag,         HELP_FUL, "check only"},
	{"DEF",       arg2_operand,   NULL,           optdefine,	  HELP_FUL, "<name[=val]>|define <name[=val]> (pre-processor)"},
	{"DTRACES",   arg2_single,    &enable_dtraces, set_flag,          HELP_FUL, "enable debugging traces"},
	#if !(defined(CONFIG2) || defined(CONFIG3))
	{"D",         arg2_single,    &minimal_debugoutput, set_flag,     HELP_USE, "minimum debugging data"},
	/* {"D",        arg2_single,    &debugoutput,    clear_flag,        HELP_USE, "Disable all debug output"}, */
	{"ETC",       arg2_single,    NULL,           optetc,             HELP_FUL, "target to ETC code"},
	{"E",         arg2_single,    &stdlibsenabled, clear_flag,        HELP_FUL, "disable compiler libraries"},
	#endif
	/*}}}*/
	/*{{{  g - k*/
	#if defined(CONFIG2) || defined(CONFIG3)
	{"GD",        arg2_single,    NULL,           optdynamic,         HELP_USE, "enable dynamic debugging"},
	#endif
	#if !(defined(CONFIG2) || defined(CONFIG3))
	{"GAMMAE",    arg2_single,    &gammae,        set_flag,           HELP_FUL, "gamma-E revision of chip"},
	{"GAMMA",     arg2_single,    &gamma,         set_flag,           HELP_FUL, "gamma revision of chip"},
	#endif
	#if defined(CONFIG2) || defined(CONFIG3)
	{"GA",        arg2_single,    NULL,           optadvanced,        HELP_USE, "enable interactive debugging"},
	#endif
	{"G",         arg2_single,    NULL,           optseqguys,         HELP_FUL, "sequential code insertion"},
	{"HELP",      arg2_single,    NULL,           set_help_long,      HELP_USE, "display a full help page"},
	#if 0 /*!(defined(CONFIG2) || defined(CONFIG3))*/
	{"H1G",       arg2_single,    NULL,           optprocessor,       HELP_ZED, "Experimental H1 G-Process"},
	{"H1L",       arg2_single,    NULL,           optprocessor,       HELP_ZED, "Target To T9000 L-Process"},
	#endif
	{"H",         arg2_single,    NULL,           optseterrormode,    HELP_USE, "halt error mode (default)"},
	#if defined(INITIAL_DECL)
	{"INIT",      arg2_single,    &initial_decl,  set_flag,           HELP_FUL, "enable initialising declarations"},
	#endif
	{"I",         arg2_single,    &information,   set_flag,           HELP_USE, "output information"},
	/*{"KA",      arg2_single,    NULL,           optnoanycheck,      HELP_FUL, "Disable All Error Checking"}, */
	{"K",         arg2_single,    NULL,           optnorangecheck,    HELP_FUL, "disable range checking"},
	/*}}}*/
	/*{{{  l - n*/
	/* {"L",        arg2_single,    NULL,           optmakelib,         HELP_FUL, "Create A Library"}, */
	#if defined(IMS)
	{"L",         arg2_single,    &load_only,     set_flag,           HELP_FUL, "load compiler and do nothing"},
	#endif
	#if !(defined(CONFIG2) || defined(CONFIG3))
	{"M212",      arg2_single,    NULL,           optprocessor,       HELP_ZED, "target to M212"},
	#endif
	#ifdef MOBILES
	{"MOBILES",   arg2_single,    &mobile_data_types,      set_flag,  HELP_FUL, "enable mobile data types"},
	{"MPT",       arg2_single,    &mobile_proc_types,      set_flag,  HELP_FUL, "enable mobile process types"},
	{"MSF",       arg2_single,    &mobile_size_field,      set_flag,  HELP_FUL, "enable mobile type-size field (needed for user-defined channels)"},
	{"MSP",       arg2_single,    &enable_mobilespace,     set_flag,  HELP_FUL, "enable mobilespace (for fixed-size mobiles)"},
	#endif
	{"NA",        arg2_single,    &ignore_assertions, set_flag,       HELP_FUL, "disable run-time ASSERT checks"},
	{"NDM",       arg2_single,    &disable_dynmem, set_flag,          HELP_FUL, "disable dynamic memory support"},
	#if defined(CONFIG3)
	{"NG",        arg2_single,    NULL,           optconfig,          HELP_USE, "no default guardian"},
	#endif
	#if defined(COMPILING_TO_JCODE)
	{"NOCOMPACT", arg2_single,    &compactable_output,    clear_flag, HELP_FUL, "do not produce compactable code"},
	#endif
	{"NPC",       arg2_single,    &no_placed_chans,       set_flag,   HELP_FUL, "disable PLACE\'d channels"},
	{"NU",        arg2_single,    &no_undefinedness_check, set_flag,  HELP_FUL, "disable undefineness check"},
	#if defined(CONFIG2)
	{"NV",        arg2_single,    NULL,           optnovirtualroute,  HELP_USE, "disable software virtual routing"},
	#endif
	{"NWCA",      arg2_single,    NULL,           optwarnings,        HELP_FUL, "no warnings on CHAN ANY"},
	#if !defined(COMPILING_TO_JCODE)
	{"NWGY",      arg2_single,    NULL,           optwarnings,        HELP_FUL, "no warnings on GUY construct"},
	#endif
	{"NWP",       arg2_single,    NULL,           optwarnings,        HELP_FUL, "no unused parameter warnings"},
	{"NWU",       arg2_single,    NULL,           optwarnings,        HELP_FUL, "no unused name warnings"},
	#if !(defined(CONFIG2) || defined(CONFIG3))
	{"N",         arg2_single,    &checkusage,    clear_flag,         HELP_FUL, "disable usage checking"},
	#endif
	/*}}}*/
	/*{{{  o - s*/
	#if defined(COMPILING_TO_JCODE)
	{"O0",        arg2_single,    NULL,           oc_optimisations,   HELP_USE, "disable all optimisation"},
	{"O1",        arg2_single,    NULL,           oc_optimisations,   HELP_USE, "enable local optimisation"},
	{"O2",        arg2_single,    NULL,           oc_optimisations,   HELP_USE, "enable all optimisation"},
	#else
	{"O0",        arg2_single,    NULL,           oc_optimisations,   HELP_ZED, "(compatibility with new compiler)"},
	{"O1",        arg2_single,    NULL,           oc_optimisations,   HELP_ZED, "(compatibility with new compiler)"},
	{"O2",        arg2_single,    NULL,           oc_optimisations,   HELP_ZED, "(compatibility with new compiler)"},
	#endif
	{"O",         arg2_operand,   NULL,           optobjfilename,     HELP_USE, "<file>|specify <file> as output file"},
	#if defined(CONFIG2) || defined(CONFIG3)
	{"PRE",       arg2_single,    NULL,           optconfig,          HELP_USE, "enable execution profiling"},
	{"PRU",       arg2_single,    NULL,           optconfig,          HELP_USE, "enable utilisation profiling"},
	#endif
	#if !(defined(CONFIG2) || defined(CONFIG3))
	{"PR",        arg2_single,    &sampling_profiling, set_flag,      HELP_FUL, "enable routine profiling"},
	{"PL",        arg2_single,    &line_profiling,     set_flag,      HELP_FUL, "enable line profiling"},
	{"PG",        arg2_single,    &cgraph_profiling,   set_flag,      HELP_FUL, "enable call graph profiling"},
	{"P",         arg2_operand,   NULL,           optmapfilefilename, HELP_USE, "<file>|specify <file> as map file name"},
	#endif
	{"QS",        arg2_single,    NULL,           optfavourspace,     HELP_FUL, "favour space efficiency"},
	{"QT",        arg2_single,    NULL,           optfavourtime,      HELP_FUL, "favour time efficiency (default)"},
	#if 0
	{"Q",         arg2_operand,   NULL,           optprogramblocksize,HELP_FUL, "<size>|Set Block Sizeto <size>"},
	#endif
	#if defined(CONFIG2) || defined(CONFIG3)
	{"RA",        arg2_single,    NULL,           optrom,             HELP_USE, "configure for ROM running in RAM"},
	{"RE",        arg2_single,    NULL,           optconfig,          HELP_USE, "enable memory layout reordering"},
	{"RO",        arg2_single,    NULL,           optrom,             HELP_USE, "configure for ROM running in ROM"},
	#endif
	{"REVALT",    arg2_single,    &reverse_alt_disable, set_flag,     HELP_FUL, "use reversed ALT disable sequence"},
	#if defined(CONFIG3)
	{"RV",        arg2_operand,   NULL,           optreservedsize,    HELP_FUL, "<size>|specify reserved memory size as <size>"},
	#endif
	{"R",         arg2_operand,   NULL,           optoutputfilename,  HELP_FUL, "<file>|redirect screen output to <file>"},
	{"SAFE",      arg2_single,    &safe_code_only,        set_flag,   HELP_FUL, "safe code only"},
	{"STRICT",    arg2_single,    &strict_checking,       set_flag,   HELP_FUL, "strict usage checking"},
	{"S",         arg2_single,    NULL,           optseterrormode,    HELP_USE, "stop Error Mode"},
	/*}}}*/
	/*{{{  t*/
	#if !(defined(CONFIG2) || defined(CONFIG3))
	/* bug TS/1940 - 02/11/92 changed T2 etc to say T2 class etc */
	#ifdef TARGET_ACCESSALIGNED
	{"TAL",       arg2_single,    &target_accessaligned, set_flag,    HELP_FUL, "target requires aligned memory accesses (default)"},
	#else	/* !TARGET_ACCESSALIGNED */
	{"TAL",       arg2_single,    &target_accessaligned, set_flag,    HELP_FUL, "target requires aligned memory accesses"},
	#endif	/* !TARGET_ACCESSALIGNED */
	{"TA",        arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to TA processor class"},
	#ifdef TARGET_BIGENDIAN
	{"TBE",       arg2_single,    &target_bigendian, set_flag,        HELP_FUL, "target to big-endian processor (default)"},
	#else	/* !TARGET_BIGENDIAN */
	{"TBE",       arg2_single,    &target_bigendian, set_flag,        HELP_FUL, "target to big-endian processor"},
	#endif	/* !TARGET_BIGENDIAN */
	{"TB",        arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to TB processor class"},
	#ifdef TARGET_BIGENDIAN
	{"TLE",       arg2_single,    &target_bigendian, clear_flag,      HELP_FUL, "target to little-endian processor"},
	#else	/* !TARGET_BIGENDIAN */
	{"TLE",       arg2_single,    &target_bigendian, clear_flag,      HELP_FUL, "target to little-endian processor (default)"},
	#endif	/* !TARGET_BIGENDIAN */
	#ifdef TARGET_ACCESSALIGNED
	{"TUL",       arg2_single,    &target_accessaligned, clear_flag,  HELP_FUL, "target permits unaligned memory accesses"},
	#else	/* !TARGET_ACCESSALIGNED */
	{"TUL",       arg2_single,    &target_accessaligned, clear_flag,  HELP_FUL, "target permits unaligned memory accesses (default)"},
	#endif	/* !TARGET_ACCESSALIGNED */
	/*{"TC",        arg2_single,    NULL,           optprocessor,       HELP_FUL, "Target To TC Processor Class"}, */
	{"T212",      arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T212"},
	{"T222",      arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T222"},
	{"T225",      arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T225"},
	{"T2",        arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T2 processor class"},
	{"T3",        arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T3 processor class"},
	{"T400",      arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T400"},
	{"T414",      arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T414"},
	{"T425",      arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T425"},
	{"T450",      arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T450"},
	{"T4",        arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T4 Processor Class"},
	{"T5",        arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T5 Processor Class"},
	{"T800",      arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T800"},
	{"T801",      arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T801"},
	{"T805",      arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T805"},
	{"T8",        arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T8 Processor Class"},
	{"T9000GAMMAE",arg2_single,   NULL,           optprocessor,       HELP_FUL, "target to T9000 GammaE"},
	{"T9000GAMMA",arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T9000 Gamma"},
	{"T9000",     arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T9000 L-Process"},
	{"T9",        arg2_single,    NULL,           optprocessor,       HELP_FUL, "target to T9 Processor Class"},
	#endif
	{"T",         arg2_single,    &tcoff_without_code,   set_flag,    HELP_FUL, "write TCOFF file without code"},
	/*}}}*/
	/*{{{  u - y*/
	#ifdef USER_DEFINED_OPERATORS
	{"UDO",       arg2_single,    &user_defined_operators, set_flag,  HELP_FUL, "enable user defined operators"},
	#endif
	/*{"U",         arg2_single,    NULL,           optseterrormode,    HELP_FUL, "Undefined Error Mode"}, */
	{"U",         arg2_single,    NULL,           optnoanycheck,      HELP_FUL, "disable run-time error checking"},
	{"V",         arg2_single,    NULL,           optnovecspace,      HELP_USE, "disable separate vector space"},
	{"WALIGN",    arg2_single,    NULL,           optwarnings,        HELP_FUL, "warn on alignment checks"},
	{"WALL",      arg2_single,    NULL,           optwarnings,        HELP_USE, "enable all warnings"},
	/*{"WC",        arg2_single,    NULL,           optwarnings,        HELP_ZED, "No Common Subexp Elim Warnings"}, */
	{"WD",        arg2_single,    NULL,           optwarnings,        HELP_FUL, "provide descoped name warnings"},
	{"WO",        arg2_single,    NULL,           optwarnings,        HELP_FUL, "provide overlap check warnings"},
	{"WQUAL",     arg2_single,    NULL,           optwarnings,        HELP_FUL, "enable software quality warnings"},
	{"W",         arg2_single,    NULL,           optfullguys,        HELP_FUL, "full code insertion"},
	#if 0
	{"XO",        arg2_single,    NULL,           optexecute,         HELP_ZED, "execute once only"},
	{"XM",        arg2_single,    NULL,           optexecute,         HELP_ZED, "execute many times"},
	#endif
	{"XF",        arg2_operand,   NULL,           optxmlfilename,     HELP_ZED, "XML output filename"},
	{"XIN",       arg2_single,    &extended_input,set_flag,           HELP_FUL, "enable extended input"},
	{"XTAG",      arg2_single,    &tagged_input_tag_xin,set_flag,     HELP_FUL, "enable extended input on tagged/case input"},
	#if !defined(CONFIG3)
	{"X",         arg2_single,    NULL,           optseterrormode,    HELP_USE, "universal error mode"},
	#endif
	{"Y",         arg2_single,    NULL,           optsetymode,        HELP_USE, "disable library I/O"},
	/*}}}*/
	/*{{{  z*/
	/* Hidden options marked as 'used' are _required_ by some other tools,
	 * so cannot be arbitrarily changed.
	 * Others might have been used too. */
	/*{{{  za - zc*/
	#if !defined(COMPILING_TO_JCODE)
	{"ZAM",       arg2_single,    NULL,           optzed,             HELP_ZED, "allocate max below WS"},
	{"ZAND",      arg2_single,    NULL,           optzed,             HELP_ZED, "don't allocate divided by size"},
	{"ZASM",      arg2_single,    NULL,           optzed, /* used */  HELP_ZED, "assembler output (partly)"}, /* used to build C release */
	{"ZAS",       arg2_single,    NULL,           optzed,             HELP_ZED, "allocate vars in scope order"},
	#else
	{"ZAQC",      arg2_single,    NULL,           optzed,             HELP_ZED, "assembler code generator diagnostics"},
	{"ZAQE",      arg2_single,    NULL,           optzed,             HELP_ZED, "assembler expression parsing diagnostics"},
	{"ZAQG",      arg2_single,    NULL,           optzed,             HELP_ZED, "assembler code expander diagnostics"},
	{"ZAQL",      arg2_single,    NULL,           optzed,             HELP_ZED, "assembler lexical analyser diagnostics"},
	{"ZAQM",      arg2_single,    NULL,           optzed,             HELP_ZED, "assembler storage allocation diagnostics"},
	{"ZAQS",      arg2_single,    NULL,           optzed,             HELP_ZED, "assembler syntax analyser diagnostics"},
	{"ZAQV",      arg2_single,    NULL,           optzed,             HELP_ZED, "assembler address evaluator diagnostics"},
	{"ZAQY",      arg2_single,    NULL,           optzed,             HELP_ZED, "assembler symbol table diagnostics"},
	{"ZAQ1",      arg2_single,    NULL,           optzed,             HELP_ZED, "object file records"},
	{"ZAQ2",      arg2_single,    NULL,           optzed,             HELP_ZED, "object file low-level output"},
	{"ZAQ3",      arg2_single,    NULL,           optzed,             HELP_ZED, "code output"},
	#endif
	{"ZA",        arg2_single,    NULL,           optzed,             HELP_ZED, "assembler & diagnostics"},
	{"ZB",        arg2_single,    NULL,           optzed,             HELP_ZED, "assembler output only"},
	#if defined(CONFIG2) || defined(CONFIG3)
	{"ZCDL",      arg2_operand,   NULL,           optdeblibname,      HELP_ZED, "<lib>|change debug library to <lib>"},
	{"ZCPL",      arg2_operand,   NULL,           optprolibname,      HELP_ZED, "<lib>|change profiler library to <lib>"},
	{"ZCSL",      arg2_operand,   NULL,           optsyslibname,      HELP_ZED, "<lib>|change system library to <lib>"},
	#endif
	{"ZCTKNSF",   arg2_single,    NULL,           optzed,             HELP_ZED, "KRoC.nsf channel-type state field"},
	{"ZCTUIO",    arg2_single,    NULL,           optzed,             HELP_ZED, "channel-type user operations"},
	{"ZCTT",      arg2_single,    NULL,           optzed,             HELP_ZED, "channel-type type descriptions"},
	#if defined(CONFIG2) || defined(CONFIG3)
	{"ZCVL",      arg2_operand,   NULL,           optvirlibname,      HELP_ZED, "<lib>|change virtual library to <lib>"},
	#endif
	{"ZCXDIV",    arg2_single,    NULL,           optzed,		  HELP_ZED, "allocate temporaries for complex DIV operations"},
	{"ZCXREM",    arg2_single,    NULL,           optzed,		  HELP_ZED, "allocate temporaries for complex REM operations"},
	#if defined(CONFIG2) || defined(CONFIG3)
	{"ZC",        arg2_both,      NULL,           optzed,             HELP_ZED, "config option (ZC HELP for info)"},
	#endif
	/*}}}*/

	/*{{{  zd - zf*/
	#if !defined(COMPILING_TO_JCODE)
	{"ZDME",      arg2_single,    &main_dynentry, set_flag,           HELP_ZED, "dynamic top-level entry point"},
	{"ZD",        arg2_single,    NULL,           optzed,             HELP_ZED, "disassemble after crunch"},
	{"ZEN",       arg2_single,    &enhanced_alt_enable, set_flag,     HELP_FUL, "use enhanced ALT enabling"},
	{"ZEP",       arg2_single,    &alt_preenable, set_flag,           HELP_FUL, "use ALT pre-enabling sequence"},
	{"ZER64",     arg2_single,    NULL,           optzed,             HELP_ZED, "swap words of REAL64s"},
	{"ZE",        arg2_single,    NULL,           optzed,             HELP_ZED, "visible compiler library names"},
	#else
	{"ZDCH",      arg2_single,    NULL,           optzed,             HELP_ZED, "disable CSE heuristics"},
	{"ZDCP",      arg2_single,    NULL,           optzed,             HELP_ZED, "disable constant propagation"},
	{"ZDC",       arg2_single,    NULL,           optzed,             HELP_ZED, "disable common subexpression elimination"},
	{"ZDD",       arg2_single,    NULL,           optzed,             HELP_ZED, "disable dead code elimination"},
	{"ZDFO",      arg2_single,    NULL,           optzed,             HELP_ZED, "disable overlay of formals"},
	{"ZDF",       arg2_single,    NULL,           optzed,             HELP_ZED, "disable flowgraph optimisation"},
	{"ZDL",       arg2_single,    NULL,           optzed,             HELP_ZED, "disable loop-invariant code motion optimisation"},
	{"ZDP",       arg2_single,    NULL,           optzed,             HELP_ZED, "disable peephole optimisation"},
	{"ZDSE",      arg2_single,    NULL,           optzed,             HELP_ZED, "disable automatic discovery of side-effect free routines"},
	{"ZDS",       arg2_single,    NULL,           optzed,             HELP_ZED, "disable strength reduction optimisation"},
	{"ZDT",       arg2_single,    NULL,           optzed,             HELP_ZED, "disable tailcall optimisation"},
	{"ZDW",       arg2_single,    NULL,           optzed,             HELP_ZED, "allocate workspace disjointly"},
	{"ZEC",       arg2_single,    NULL,           optzed,             HELP_ZED, "enable common subexpression elimination"},
	{"ZECH",      arg2_single,    NULL,           optzed,             HELP_ZED, "enable CSE heuristics"},
	{"ZECP",      arg2_single,    NULL,           optzed,             HELP_ZED, "enable constant propagation"},
	{"ZED",       arg2_single,    NULL,           optzed,             HELP_ZED, "enable dead code elimination"},
	{"ZEF",       arg2_single,    NULL,           optzed,             HELP_ZED, "enable flowgraph optimisation"},
	{"ZEFO",      arg2_single,    NULL,           optzed,             HELP_ZED, "enable overlay of formals"},
	{"ZEL",       arg2_single,    NULL,           optzed,             HELP_ZED, "enable loop-invariant code motion optimisation"},
	{"ZEN",       arg2_single,    &enhanced_alt_enable, set_flag,     HELP_FUL, "use enhanced ALT enabling"},
	{"ZEPP",      arg2_single,    &alt_preenable, set_flag,           HELP_FUL, "use ALT pre-enabling sequence"},
	{"ZEP",       arg2_single,    NULL,           optzed,             HELP_ZED, "enable peephole optimisation"},
	{"ZES",       arg2_single,    NULL,           optzed,             HELP_ZED, "enable strength reduction optimisation"},
	{"ZESE",      arg2_single,    NULL,           optzed,             HELP_ZED, "enable automatic discovery of side-effect free routine"},
	{"ZET",       arg2_single,    NULL,           optzed,             HELP_ZED, "enable tailcall optimisation"},
	{"ZEW",       arg2_single,    NULL,           optzed,             HELP_ZED, "allocate workspace by variable colouring"},
	{"ZE",        arg2_single,    NULL,           optzed,             HELP_ZED, "visible compiler library names"},
	#endif
	{"ZFMCCT",    arg2_single,    &fm_collct,     set_flag,           HELP_ZED, "collapse channel-type protocols in formal model"},
	{"ZFMCOM",    arg2_single,    &fm_comm,       set_flag,           HELP_ZED, "include acquire/lose events for mobile channel-ends"},
	{"ZFMICR",    arg2_single,    &fm_inlinecr,   set_flag,           HELP_ZED, "inline claim/release for mobile channel-type ends"},
	{"ZFMNCR",    arg2_single,    &fm_nocr,       set_flag,           HELP_ZED, "no separate claim/release infrastructure for channel-ends"},
	{"ZFMTLO",    arg2_single,    &fm_toplevelonly,set_flag,          HELP_ZED, "do not generate formal models for #INCLUDEd material"},
	{"ZFM",       arg2_single,    &formal_model,  set_flag,           HELP_ZED, "generate formal model"},
	/*}}}*/

	/*{{{  zg - zl*/
	#if defined(CONFIG2)
	{"ZGI",       arg2_single,    NULL,           optinteractive,     HELP_ZED, "idebug interactive debugging"},
	#endif
	#if !(defined(CONFIG2) || defined(CONFIG3))
	{"ZGRP",      arg2_single,    NULL,           optzgrp,            HELP_ZED, "output suitable for the grouper"},
	#endif
	#if 0 /* !(defined(CONFIG2) || defined(CONFIG3)) */
	{"ZH1G",      arg2_single,    NULL,           optprocessor,       HELP_ZED, "Experimental H1 G-Process (T805)"},
	{"ZH1L",      arg2_single,    NULL,           optprocessor,       HELP_ZED, "Experimental H1 L-Process (T805)"},
	#endif
	{"ZH",        arg2_single,    NULL,           optzed, /* used */  HELP_ZED, "mark output as an occam harness"}, /* used for C run-time harness */
	{"ZK",        arg2_single,    NULL,           optzed,             HELP_ZED, "KRoC special"},
	{"ZI",        arg2_operand,   NULL,           optpathname,/*used*/HELP_ZED, "<path>|change default pathname to <path>"}, /* used for building compiler libs */
	{"ZLCP",      arg2_both,      NULL,           optzsuffix,/*used*/ HELP_ZED, "<sfx>|specify <sfx> as compiler predef suffix"}, /* used to build FORTRAN compiler libs */
	{"ZLCS",      arg2_both,      NULL,           optzsuffix,/*used*/ HELP_ZED, "<sfx>|specify <sfx> as compiler library suffix"}, /* used to build FORTRAN compiler libs */
	{"ZLC",       arg2_operand,   NULL,           optextlibfilename,  HELP_ZED, "<lib>|change compiler library to <lib>"}, /* used to build FORTRAN compiler libs */
	{"ZLIS",      arg2_both,      NULL,           optzsuffix,/*used*/ HELP_ZED, "<sfx>|specify <sfx> as I/O library suffix"}, /* used to build FORTRAN compiler libs */
	{"ZLI",       arg2_operand,   NULL,           optvlibsfilename,   HELP_ZED, "<lib>|change compiler I/O library to <lib>"}, /* used to build FORTRAN compiler libs */
	#if 0
	{"ZL",        arg2_single,    NULL,           optzed,             HELP_ZED, "Display Lex Output"},
	#endif
	/*}}}*/

	/*{{{  zm - zo*/
	#if defined(HOST_OS_IS_UNIX) && !defined(WIN32)
	{"ZMEM",      arg2_single,    NULL,           optzed,             HELP_ZED, "display memory (sbrk) statistics"},
	#endif
	#ifdef MOBILES
	{"ZMPPA",     arg2_single,    &mpp_check_at_act, set_flag,        HELP_ZED, "check for terminated MOBILE PROC at activation"},
	{"ZMP",       arg2_single,    &map_all_procs, set_flag,           HELP_ZED, "map all PROCs"},
	#endif
	{"ZNA",       arg2_single,    NULL,           optzed,             HELP_ZED, "no outermost preamble/postamble"},
	{"ZNCC",      arg2_single,    NULL,           optzed,             HELP_ZED, "no call compatibility checks"},
	{"ZNC",       arg2_single,    NULL,           optzed,             HELP_ZED, "no variable I/O for counted arrays"},
	{"ZND",       arg2_single,    NULL,           optzed,             HELP_ZED, "no debug information at all"},
	{"ZNEAS",     arg2_single,    NULL,           optzed,             HELP_ZED, "no errors for illegal ASM instrs"},
	{"ZNEC",      arg2_single,    NULL,           optzed,             HELP_ZED, "no comment indentation errors"},
	{"ZNER",      arg2_single,    NULL,           optzed,             HELP_ZED, "no errors at all (beware!)"},
	{"ZNI",       arg2_single,    NULL,           optzed,             HELP_ZED, "disable INLINE"},
	{"ZNJ",       arg2_single,    NULL,           optzed, /* used */  HELP_ZED, "use CJ Not J (mostly)"}, /* used in insight kernel */
	#if !defined(COMPILING_TO_JCODE)
	{"ZNL",       arg2_single,    NULL,           optzed,             HELP_ZED, "no disassembly line numbers"},
	{"ZNM",       arg2_single,    NULL,           optzed,             HELP_ZED, "no disassembly comments"},
	{"ZNO",       arg2_single,    NULL,           optzed,             HELP_ZED, "no disassembly operands"},
	#endif
	{"ZNP",       arg2_single,    NULL,           optzed,             HELP_ZED, "no predefines allowed"},
	#if !defined(COMPILING_TO_JCODE)
	{"ZNV",       arg2_single,    NULL,           optzed,             HELP_ZED, "don't opt when vsoffset_flag is zero"},
	#endif
	{"ZNWF",      arg2_single,    NULL,           optwarnings,        HELP_ZED, "no formal param descope warnings"},
	/* removed source_output 14/3/91 */
	/*{"ZO",        arg2_single,    NULL,           optzed,             HELP_ZED, "Display Source / Disassembly"}, */
	{"ZOD",       arg2_single,    NULL,           optzed,             HELP_ZED, "merge types in descriptors"},
	{"ZOE",       arg2_single,    NULL,           optzed,             HELP_ZED, "disable export of origin symbol"},
	{"ZOI",       arg2_single,    NULL,           optzed,             HELP_ZED, "disable imported origin checks"},
	{"ZOL",       arg2_single,    NULL,           optzed,             HELP_ZED, "prioritise linkage"},
	{"ZON",       arg2_single,    NULL,           optzed,             HELP_ZED, "short names in descriptors"},
	{"ZOPS",      arg2_single,    NULL,           optzed,             HELP_ZED, "try to `optimise' for space"},
	{"ZOR",       arg2_single,    NULL,           optzed,             HELP_ZED, "read in incompatible lib entries"},
	#if !defined(COMPILING_TO_JCODE)
	{"ZOS",       arg2_operand,   NULL,           optcodesize,        HELP_ZED, "same as option CODE"},
	#endif
	{"ZOV",       arg2_single,    NULL,           optzed,             HELP_ZED, "don't hash version into origin"},
	{"ZOW",       arg2_single,    NULL,           optzed,             HELP_ZED, "don't output WS and VS symbols"},
	{"ZOYS",      arg2_single,    NULL,           optzed,             HELP_ZED, "yes - add spaces in descriptors"},
	/*}}}*/

	/*{{{  zp - zr*/
	#if !defined(COMPILING_TO_JCODE)
	{"ZPNN",      arg2_single,    NULL,           optzed,             HELP_ZED, "ignore NULL trap handlers"},
	#endif
	{"ZPP",       arg2_single,    &preproc_dump,  set_flag,           HELP_ZED, "show pre-processor defines"},
	#if defined(COMPILING_TO_JCODE)
	{"ZPS",       arg2_both,      NULL,           optzed,             HELP_ZED, "internal pragma setting for detailed info"},
	#endif
	{"ZPRM",      arg2_single,    NULL,           optzed,             HELP_ZED, "print all messages"},
	{"ZPD",       arg2_single,    NULL,           optzed,             HELP_ZED, "profiling trace information"},
	{"ZQA",       arg2_single,    NULL,           optzed,             HELP_ZED, "quad-align double-word types"},
	#if defined(COMPILING_TO_JCODE)
	{"ZQC",       arg2_single,    NULL,           optzed,             HELP_ZED, "CSE diagnostics"},
	{"ZQE",       arg2_single,    NULL,           optzed,             HELP_ZED, "strength reduction diagnostics"},
	{"ZQG",       arg2_single,    NULL,           optzed,             HELP_ZED, "J-Code generator diagnostics"},
	{"ZQJ",       arg2_single,    NULL,           optzed,             HELP_ZED, "workspace allocation diagnostics"},
	{"ZQK2",      arg2_single,    NULL,           optzed,             HELP_ZED, "peephole optimiser diagnostics"},
	{"ZQK",       arg2_single,    NULL,           optzed,             HELP_ZED, "local code generator diagnostics"},
	{"ZQN",       arg2_single,    NULL,           optzed,             HELP_ZED, "optimisation state diagnostics"},
	{"ZQR",       arg2_single,    NULL,           optzed,             HELP_ZED, "register allocator diagnostics"},
	{"ZQU",       arg2_single,    NULL,           optzed,             HELP_ZED, "compiler internal storage allocation diagnostics"},
	{"ZQW",       arg2_single,    NULL,           optzed,             HELP_ZED, "compiler internal storage allocation diagnostics"},
	{"ZQ",        arg2_both,      NULL,           optzed,             HELP_ZED, "OC debug flags"},
	#endif
	{"ZRPE",      arg2_single,    &barrier_rbpe,  set_flag,           HELP_ZED, "resign barriers before PAR end"},
	{"ZRV",       arg2_single,    &result_abbr_var,	set_flag,	  HELP_ZED, "generate new variable(s) for RESULT abbreviations"},
	/*}}}*/

	/*{{{  zs - zz*/
	/*{arg_string,  arg_type,       arg_data,       arg_function,       arg_help_page,      arg_help_text }   */
	{"ZSC",       arg2_single,    NULL,           optzed,             HELP_ZED, "stop after type check"},
	{"ZSE",       arg2_single,    NULL,           optzed,             HELP_ZED, "stop after all errors"},
	{"ZSM",       arg2_single,    NULL,           optzed,             HELP_ZED, "stop after mapping"},
	{"ZSP",       arg2_single,    NULL,           optzed,             HELP_ZED, "stop after parse"},
	{"ZSRC",      arg2_single,    NULL,           optzed,             HELP_ZED, "print source tree"},
	{"ZST",       arg2_single,    NULL,           optzed,             HELP_ZED, "stop after trans"},
	{"ZSU",       arg2_single,    NULL,           optzed,             HELP_ZED, "stop after & debug usage check"},
	{"ZT450AWA",  arg2_single,    NULL,           optzed,             HELP_ZED, "disable T450 workarounds"},
	{"ZTST",      arg2_single,    NULL,           optzed,             HELP_ZED, "generic test flag"},
	{"ZTX",	      arg2_single,    NULL,           optzed,             HELP_ZED, "print XML tree"},
	{"ZT",        arg2_single,    NULL,           optzed,             HELP_ZED, "print tree"},
	{"ZUS",       arg2_single,    NULL,           optzed,             HELP_ZED, "force enable T9 short ops"},
	{"ZV",        arg2_single,    NULL,           optzed, /* used */  HELP_ZED, "disable library I/O"}, /* used to build virtual.lib */
	{"ZWAU",      arg2_single,    NULL,           optzed,             HELP_ZED, "warn on alias and usage checks"},
	{"ZWC",       arg2_single,    NULL,           optzed,             HELP_ZED, "warn on comment indentation"},
	{"ZWTAG",     arg2_single,    NULL,           optwarnings,        HELP_ZED, "warn unused tags in tagged input"},
	{"ZW",        arg2_single,    NULL,           optzed, /* used */  HELP_ZED, "do channels not by pointer"}, /* used to build debugger & bootstraps */
	{"ZX",        arg2_single,    NULL,           optzed,             HELP_ZED, "8 byte libpatch"},
	{"ZYPT",      arg2_single,    NULL,           optzed,             HELP_ZED, "yes: visible PROTOCOL tags"},
	{"ZZS",       arg2_single,    NULL,           optzed,             HELP_ZED, "hardware supports fpsin/fpcos/fptan instructions"},
	{"ZZ",        arg2_single,    NULL,           optzed,             HELP_ZED, "use ALT for PRI PAR"},
	/*}}}*/
	{"Z",         arg2_single,    NULL,           set_help_extended,  HELP_ZED, "display hidden option info"},
	/*}}}*/
	/*{{{  others*/
	{ NULL,       arg2_error,     NULL,           general_error,     arg2_help_na,  NULL },
	{ NULL,       arg2_syntax,    NULL,           syntax_error,      arg2_help_na,  NULL },
	{ NULL,       arg2_file_err,  NULL,           file_error,        arg2_help_na,  NULL },
	{ NULL,       arg2_help,      NULL,           set_help_use,      arg2_help_na,  NULL },
	{ NULL,       arg2_token,     NULL,           get_source_file,   arg2_help_na,  NULL },
	{ NULL,       arg2_help_info, &oc_help_info,  NULL,              arg2_help_na,  NULL },
	{ NULL,       arg2_tail_line, tail_help_line, NULL,              HELP_USE,      NULL },
	{ NULL,       arg2_end,       NULL,           finish_command_line_args, arg2_help_na,  NULL },
	/*}}}*/
};
/*}}}*/
/*{{{  print help functions*/
PRIVATE void display_help(arg2_help_page_type level)
{
	arg2_print_help_page (USING_ARG_VERSION, level, cloptions);
}


PRIVATE arg_control set_help_use(const char *opt, const char *arg, void *data)
{
	help_level = arg2_help_short;
	return (arg_continue);
}


PRIVATE arg_control set_help_long(const char *opt, const char *arg, void *data)
{
	if (help_level != arg2_help_extended) {
		/* don't override a previous -z option */
		help_level = arg2_help_long;
	}
	return (arg_continue);
}


PRIVATE arg_control set_help_extended(const char *opt, const char *arg, void *data)
{
	help_level = arg2_help_extended;
	return (arg_continue);
}
/*}}}*/
/*{{{  Parse the arguments*/
BOOL parse_command_line( int argc, const char *argv[] )
{
	arg_parse_result status = arg_parse_ok;
	/*{{{  Initialise the global command line*/

	generatecode      = TRUE;
	call_alias_check  = TRUE;
	zinfo             = FALSE;  /* Whether to display z option help page */
	long_help         = FALSE;
	stop_after_map    = FALSE;
	permit_variableio = TRUE;
	allowpredefs      = TRUE; /* Whether to allow predefines */
	onlylex           = FALSE;
	guyinserts        = 0;
	brieferrors       = FALSE;
	crasherrors       = FALSE;
	debuguse          = FALSE;
	nochecking        = FALSE;
	allow_inlines     = TRUE;
	lexer_ignore_comments = FALSE; /* make the lexer completely ignore comments */
	visible_tags          = FALSE; /* whether to make PROTOCOL tags visible */
	warn_comment_indent   = FALSE; /* warn (not error) for comment indentation errors */
	warn_on_usage_error   = FALSE; /* warn (not error) for alias/usage errors */
	read_all_libs         = FALSE; /* even if for wrong processor type */
	warn_on_all_errors    = FALSE; /* turn ALL errors into warnings */
	hash_version_string   = TRUE;
	alpha                 = FALSE;
	gamma                 = FALSE;
	gammae                = FALSE;
	print_all_messages    = FALSE;
	setymode              = FALSE;
	swap_r64words         = FALSE;
	needs_quadalign       = FALSE;
	kroc_flag             = FALSE;
	kroc_complex_div	= FALSE;
	kroc_complex_rem	= FALSE;
	kroc_chantype_desc	= FALSE;
	kroc_chantype_uio	= FALSE;
	kroc_chantype_knsf	= FALSE;
	has_sincostan		= FALSE;
	#ifdef USER_DEFINED_OPERATORS
		user_defined_operators = FALSE;
	#endif
	#ifdef MOBILES
		enable_mobilespace = FALSE;
		mobile_data_types = FALSE;
	#endif
	#ifdef INITIAL_DECL
		initial_decl = FALSE;
	#endif
	/*}}}*/

	if (getenv(TOOL_OPTENV)) {
		status = arg2_parse_and_env_var_parse(USING_ARG_VERSION, argc-1, &argv[1], TOOL_OPTENV, cloptions);
	} else {
		status = arg2_parse(USING_ARG_VERSION, argc-1, &argv[1], cloptions);
	}
	if (status == arg_parse_error) {
		return FALSE;
	}
	if (sourcefilename[0] == '\0' && !information) {
		display_help (help_level ? help_level : arg2_help_short);
	}
	/*{{{  special checks for some options*/
	if (kroc_chantype_knsf && !kroc_chantype_desc) {
		harnesserror ("-zctknsf without -zctt");
		return FALSE;
	} else if (kroc_chantype_uio && !kroc_chantype_desc) {
		harnesserror ("-zxtuio without -zctt");
		return FALSE;
	}
	/*}}}*/
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE BOOL process_option*/
/*  This is called with an #OPTION string */
PRIVATE BOOL process_option (const char *const s, const int len, void (*error_fn)(int))
{
	BOOL ok = TRUE;
	int i;

	for (i = 0; i < len; i++) {
		/* use toupper safely incase we're in SunOS */
		switch (islower(s[i]) ? toupper(s[i]) : s[i]) {
		default:
			error_fn(s[i]);
			ok = FALSE;
			break;
		case ' ':
			break;
		#if !(defined(CONFIG2) || defined(CONFIG3))
		case 'A':
			(void)optnoalias (NULL, NULL, NULL);
			break;
		case 'E':
			(void)clear_flag ("-E", NULL, &stdlibsenabled);
			break;
		#endif
		case 'G':
			(void)optseqguys (NULL, NULL, NULL);
			break;
		case 'K':
			(void)optnorangecheck (NULL, NULL, NULL);
			(void)seterrormode ();
			break;
		#if !(defined(CONFIG2) || defined(CONFIG3))
		case 'N':
			(void)clear_flag (NULL, NULL, &checkusage);
			break;
		#endif
		case 'U':
			(void)optnoanycheck (NULL, NULL, NULL);
			(void)seterrormode ();
			break;
		case 'V':
			(void)optnovecspace (NULL, NULL, NULL);
			break;
		case 'W':
			(void)optfullguys (NULL, NULL, NULL);
			break;
		case 'Y':
			(void)optsetymode (NULL, NULL, NULL);
			(void)setprocessorattr ();
			break;
		}
	}
	return ok;
}
/*}}}*/
/*{{{  PRIVATE void host_setup (void)*/
PRIVATE void host_setup ( void )
{
#ifdef IMS
	int host, os, board;

	host_info(&host, &os, &board);
	switch (os) {
	case _IMS_OS_VMS:
		errfile = stderr;
		break;
	case _IMS_OS_DOS:
		errfile = stdout;
		break;
	default:
		errfile = stderr;
		break;
	}
	(void)set_abort_action (ABORT_HALT);
#else
	#if defined(HOST_OS_IS_MSDOS)
		errfile = stdout;
	#elif defined(HOST_OS_IS_VMS)
		errfile = stderr;
	#else
		errfile = stderr;
	#endif
#endif
}
/*}}}*/
/*{{{  PRIVATE char *find_extension*/
/* if extension is TRUE, returns the address of the dot,
 * if extension is FALSE, returns the address of the last char of the directory,
 * returns NULL if an extension, or a directory, was never found.
 */
PRIVATE const char *find_extension(const char *const filename, const BOOL extension)
{
	int i;

	for (i = strlen(filename); i > 0; i--) {
		switch(filename[i]) {
		/*{{{  see if we have hit extension*/
		case '.' :
			if (extension) {
				return &(filename[i]);
			}
			break;
		case '[': case ']':
		case '/': case '\\':
		case ':':
		case '<': case '>':
			if (!extension) {
				return &(filename[i]);
			}
			return (NULL);
		default:
			break;
		/*}}}*/
		}
	}
	return (NULL);
}
/*}}}*/
/*{{{  PRIVATE char *copy_fileroot*/
PRIVATE const char *copy_fileroot(char *const dest, const char *const src)
{
	const char *const extension = find_extension (src, TRUE);
	const int root_length = (extension == NULL) ? strlen(src) : extension - src;

	/*strncpy(dest, src, root_length);*/
	memcpy(dest, src, root_length);
	dest[root_length] = '\0';
	return extension;
}
/*}}}*/
/*{{{  PRIVATE BOOL setup_filenames*/
PRIVATE BOOL setup_filenames (void)
{
	BOOL equal_names;

	/* printf("setup_filenames....\n"); */
	{
		const char *const extension = copy_fileroot(rootfilename, sourcefilename);

		if (extension == NULL) {
			/* tack on default extension to source file */
			strcat(sourcefilename, SOURCEFILE_EXTENSION);
		}
		if (etc_output) {
			assembly_output = FALSE;
			strcpy (objectfileext, DEFAULT_ETC_OBJ_EXTENSION);
			strcpy (tasmfileext, DEFAULT_ETC_ASM_EXTENSION);
		} else {
			ims_asm_output = assembly_output;
		}
	}
	#if 0 /* defined(CONFIG2) || defined(CONFIG3) */
		if (cfbfilename[0] == '\0') {
			const char *const dir_end = find_extension(rootfilename, FALSE);

			strcpy(cfbfilename, (dir_end == NULL) ? rootfilename : (dir_end + 1));
			strcat(cfbfilename, CFB_FILE_EXTENSION);
		}
		/* make objfilename same as cfbfilename but with different suffix */
		{
			(void)copy_fileroot (objfilename, cfbfilename);
			strcat(objfilename, objectfileext);
		}
		equal_names = (strcmp(cfbfilename, sourcefilename) == 0) || (strcmp(objfilename, sourcefilename) == 0);
	#else
		if (assembly_output || etc_output) {
			/*{{{  adapted treatment of -o in presence of -zb (explicit or implicit)*/
			const BOOL use_oname = (objfilename[0] != '\0');
			const char *const dir_end = find_extension (rootfilename, FALSE);

			if (use_oname) {
				strcpy (tasmfilename, objfilename);
			} else {
				strcpy (tasmfilename, (dir_end == NULL) ? rootfilename : (dir_end + 1));
				strcat(tasmfilename, tasmfileext);
			}
			if (assembly_output || ims_asm_output) {
				/* i.e. not .tce only */
				outfile = fopen (tasmfilename, "w");
				if (outfile == NULL) {
					harnesserror_s("Cannot open output file ", tasmfilename);
					return FALSE;
				}
				(void)setvbuf (outfile, NULL, _IOLBF, BUFSIZ);
			}
			#if defined(COMPILER_IS_MSC) || defined(COMPILER_IS_WATCOM) || defined(HOST_OS_IS_LLL)
				errfile = stdout;
			#else
				errfile = stderr;
			#endif
#if 0
fprintf (stderr, "setup_filenames(): about to trash objfilename=[%s]\n", objfilename);
#endif
			if (!use_oname) {
				/* if already given, don't trample on it! */
				strcpy(objfilename, (dir_end == NULL) ? rootfilename : (dir_end + 1));
				strcat(objfilename, objectfileext);
			}
			/*}}}*/
		} else if (objfilename[0] == '\0') {
			const char *const dir_end = find_extension(rootfilename, FALSE);

			strcpy(objfilename, (dir_end == NULL) ? rootfilename : (dir_end + 1));
			strcat(objfilename, objectfileext);
		}
#if 0
fprintf (stderr, "setup_filenames(): sourcefilename=[%s] objfilename=[%s]\n", sourcefilename, objfilename);
#endif
		equal_names = strcmp(objfilename, sourcefilename) == 0;
	#endif

	if (equal_names) {
		harnesserror("Output file is the same as the input file");
		return (FALSE);
	}
	return (TRUE);
}
/*}}}*/
/*{{{  PRIVATEPARAM void add_default_extension(filename)*/
/*****************************************************************************
 *  add_default_extension takes the source string, and if it doesn't have an
 *                extension, adds the same extension as the object file
 *****************************************************************************/
PRIVATEPARAM void add_default_extension (char *const filename, const int maxlen)
{
	if (find_extension(filename, TRUE) == NULL) {
		const char *const default_start = find_extension(objfilename, TRUE);

		if (default_start != NULL) {
			strcat(filename, default_start);
			if (strlen(filename) >= maxlen) {
				abort();
			}
		}
	}
}
/*}}}*/
/*{{{  PRIVATE const char *errormodestring*/
PRIVATE const char *errormodestring(void)
{
	if (errormode & ERRORMODE_HALT) {
		return "HALT";
	} else if (errormode & ERRORMODE_STOP) {
		return "STOP";
	} else if (errormode & ERRORMODE_REDUCED) {
		return "REDUCED";
	} else {
		return "UNIVERSAL";
	}
}
/*}}}*/
/*{{{  PRIVATEPARAM void cleanup_after_error*/
PRIVATEPARAM void cleanup_after_error(void)
{
	if (!assembly_output && !disassemble && (objfile != NULL)) {
		fclose(objfile);
		remove(objfilename);
	}
}
/*}}}*/
/*{{{  PRIVATE treenode *call_occam_frontend*/
PRIVATE treenode *call_occam_frontend(BOOL *const error_occurred)
{
	occam_parms_t parms;

	#if defined(CONFIG2)
		parms.language            = FE_LANG_CONFIG2;
	#elif defined(CONFIG3)
		parms.language            = FE_LANG_CONFIG3;
	#else
		parms.language            = FE_LANG_OCCAM;
	#endif

	parms.sourcefilename      = sourcefilename;
	parms.pathname            = pathname;
	parms.errfile             = errfile;
	parms.onlylex             = onlylex;
	parms.call_alias_check    = call_alias_check;
	parms.allowpredefs        = allowpredefs;
	parms.brieferrors         = brieferrors;
	parms.crasherrors         = crasherrors;
	parms.debuguse            = debuguse;
	parms.nochecking          = nochecking;
	parms.allow_inlines       = allow_inlines;
	parms.ignore_comments     = lexer_ignore_comments;
	parms.visible_tags        = visible_tags;
	parms.warn_comment_indent = warn_comment_indent;
	parms.warn_on_usage_error = warn_on_usage_error;
	parms.warn_on_all_errors  = warn_on_all_errors;
	parms.read_all_libs       = read_all_libs;
	parms.hash_version_string = hash_version_string;
	parms.checkalias          = &checkalias;	/* may be modified by #OPTION */
	parms.checkusage          = &checkusage;	/* may be modified by #OPTION */
	parms.formalmodel         = &formal_model;	/* may be modified by #OPTION */
	parms.fm_collct           = &fm_collct;		/* may be modified by #OPTION */
	parms.fm_toplevelonly     = fm_toplevelonly;
	parms.fm_nocr             = fm_nocr;
	parms.fm_inlinecr         = fm_inlinecr;
	parms.fm_comm             = fm_comm;
	parms.error_occurred      = error_occurred;	/* is set if any error happens */
	parms.process_option      = process_option;
	parms.guyinserts          = &guyinserts;
	#if defined(COMPILING_TO_JCODE)
		parms.noguy_yesasm        = TRUE;
	#else
		parms.noguy_yesasm        = FALSE;
	#endif
	parms.process_filename_fn = add_default_extension;
	parms.cleanup_after_error = cleanup_after_error;
	parms.abort_fn            = abort_compiler;
	parms.suppress_compat     = suppress_call_compatibility_check;

	return occam_compiler_frontend (&parms);
}
/*}}}*/

#if defined(CONFIG2) || defined(CONFIG3)
/*{{{  PRIVATE void call_configurer*/
PRIVATE void call_configurer(treenode *treeroot, BOOL *const error_occurred)
{
	cf_handle_t *cf_handle;

	#if defined(CONFIG2)
		cf_params.cf_lang              = FE_LANG_CONFIG2;
		cf_params.cf_backend_fn        = cf_config2_backend;
	#else
		cf_params.cf_lang              = FE_LANG_CONFIG3;
		cf_params.cf_read_hardware_fn  = cf_config3_read_hardware;
		cf_params.cf_backend_fn        = cf_config3_backend;
	#endif

	cf_params.cf_setprocessorattr_fn = setprocessorattr;
	cf_params.cf_memstats_fn         = print_memstats;
	cf_params.cf_txlib               = &tx_global;
	cf_params.cf_sourcefile          = sourcefilename;
	cf_params.cf_pathname            = pathname;

	cf_handle = cf_open(be_get_fe_handle(), &cf_params);
	{
		const int res = cf_configure(cf_handle, treeroot);

		if (res != EXIT_SUCCESS) {
			*error_occurred = TRUE;
		}
	}
	cf_close(&cf_handle);
}
/*}}}*/
#endif

/*{{{  PRIVATE void print_source_if_required*/
PRIVATE void print_source_if_required(treenode *const treeroot)
{
	if (prtree) {
		/* was outfile for both, not stdout - Jim 14/10/96 */
		printtree(stdout, 0, treeroot);
	}
	if (prsrc) {
		printsrc (outfile, 0, treeroot);
	}
}
/*}}}*/
/*{{{  PRIVATE void print_xml_if_required (treenode *const treeroot)*/
/*
 *	prints the parse tree in XML if required
 */
PRIVATE void print_xml_if_required (treenode *const treeroot)
{
	if (prxmltree) {
		if (outxmlfile) {
			xml_printtree_wrapper (outxmlfile, 0, treeroot, sourcefilename);
			fflush (outxmlfile);
		} else {
			xml_printtree_wrapper (stdout, 0, treeroot, sourcefilename);
		}
	}
	return;
}
/*}}}*/

#if !defined(COMPILING_TO_JCODE)
/*{{{  PUBLIC void allocateworkspace(treeroot)*/
/* Compile the code for treeroot */
PUBLIC void allocateworkspace ( treenode *treeroot )
{
	/*{{{  set up trans parameters*/
	/* bug TS/2113 24/02/93 */
	/* work around a bug in the VAX compiler which means that it doesn't
	 * initialise automatic structs correctly.
	 */
	static const trans_params_t init = {0}; /* initialised to all OFF */
	trans_params_t trans_params = init; /* initialised to all OFF */

	trans_params.simplify_structured_io = TRUE;
	trans_params.simplify_counted_io    = !has_variableio;
	trans_params.abbr_constructors      = TRUE;
	trans_params.replpar_becomes_proc   = FALSE;
	trans_params.abbrevmode_fn          = be_abbrevmode;
	/*}}}*/

	/*{{{  call trans*/
	if (diagnostics) {
		fputs("Calling trans\n", outfile);
	}
	transmain(&trans_params, &treeroot); /* Transform tree */
	be_trans_params = &trans_params;
	/* this is valid for trans and mapper phase */

	freeup_temp_workspace();
	if (treeroot == NULL) {
		stop_after_trans = TRUE;
		return;
	}
	/*}}}*/

	/*{{{  info to screen*/
	#if !(defined(CONFIG2) || defined(CONFIG3))
		if (information) {
			fprintf(outfile, "Syntax tree transformed ok, now occupies %ld bytes\n", tablesize());
		}
	#endif
	if (diagnostics) {
		print_source_if_required(treeroot);
	}
	if (stop_after_trans) {
		return;
	}
	/*}}}*/

	/*{{{  allocate workspace*/
	#if !(defined(CONFIG2) || defined(CONFIG3))
		if (information) {
			fputs("Allocating workspace\n", outfile);
		}
	#endif
	{
		/* trans_params must still be in scope here */
		jmp_buf savedenv; /* INSdi03390 */

		memcpy ((char *)savedenv, (char *)env, sizeof(env));
		if (setjmp(env) == 0) {
			mapmain(treeroot); /* Allocate workspace */
		} else {
			end_compiler(EXIT_FAILURE);
		}
		memcpy ((char *)env, (char *)savedenv, sizeof(env));
	}
	if (diagnostics) {
		print_source_if_required(treeroot);
	}
	/*}}}*/
}
/*}}}*/
/*{{{  PUBLIC void codegenerate(open_file, treeroot)*/
/* Compile the code for treeroot */
PUBLIC void codegenerate (BOOL open_file, treenode *const treeroot )
{
	/*{{{  open object file*/
	if (tcoff_without_code || (!assembly_output && !disassemble)) {
		/* MDP */
		if (open_file) {
			objfile = open_object_file (objfilename);
		}
		write_id(sourcefilename, FALSE /*compilemode == COMP_LIB*/);
	}
	/*}}}*/
	/*{{{  debug info*/
	if (debugoutput) {
		#if !(defined(CONFIG2) || defined(CONFIG3))
			if (information) {
				fputs("Writing debug information\n", outfile);
			}
		#endif
		debugmain(treeroot);
	}
	/*}}}*/
	/*{{{  message onto the screen*/
	#if !(defined(CONFIG2) || defined(CONFIG3))
		if (information) {
			fputs("Generating code\n", outfile);
		}
	#endif
	/*}}}*/
	/*{{{  generate code*/
	{
		jmp_buf savedenv; /* INSdi03390 */

		memcpy ((char *)savedenv, (char *)env, sizeof(env));
		if (setjmp (env) == 0) {
			tmain (treeroot);   /* Generate code */
		} else {
			end_compiler (EXIT_FAILURE);
		}
		memcpy ((char *)env, (char *)savedenv, sizeof(env));
	}
	/*}}}*/
	/*{{{  finish up*/
	if (assembly_output || disassemble) {
		fputc('\n', outfile);
	} else if (open_file) {
		close_object_file (objfile, objfilename);
	}
	#if !(defined(CONFIG2) || defined(CONFIG3))
		if (information) {
			fputs("Code generated ok\n", outfile);
		}
	#endif
	freeup_temp_workspace();
	/*}}}*/
}
/*}}}*/
/*{{{  mapfile handling*/
#if !(defined(CONFIG2) || defined(CONFIG3))
/*{{{  PRIVATEPARAM void *map_malloc*/
PRIVATEPARAM void *map_malloc(map_handle_t *handle, const size_t size)
{
	handle = handle;
	return newvec(size);
}
/*}}}*/


/*{{{  PRIVATEPARAM void map_free*/
PRIVATEPARAM void map_free(map_handle_t *handle, void *const data, const size_t size)
{
	handle = handle;
	freevec(data, size);
}
/*}}}*/


/*{{{  PRIVATE void open_mapfile*/
PRIVATE void open_mapfile(treenode *const tptr)
{
	if (mapfilefilename[0] != '\0') {
		BOOL ok;

		mapfile_handle = map_open(NULL, map_malloc, map_free, sup_qsort);
		ok = map_open_file(mapfile_handle, mapfilefilename, sourcefilename, processorstring(processortype, processorattr), errormodestring(), TOOL_VERSION);
		if (!ok) {
			harnesserror_s("Cannot open mapfile: ", mapfilefilename);
		} else {
			debug_write_mapfile(tptr);
			(void)map_open_section (mapfile_handle, map_sectiontype_text, objwrt_get_section_name(), 0, NULL, 0, 0, 0, TRUE);
		}
	}
}
/*}}}*/


/*{{{  PRIVATE void close_mapfile*/
PRIVATE void close_mapfile(void)
{
	if (mapfile_handle != NULL) {
		BOOL ok;

		map_close_section (mapfile_handle, NULL);
		ok = map_close (&mapfile_handle, NULL);
		if (!ok) {
			harnesserror_s("Cannot close mapfile: ", mapfilefilename);
		}
	}
}
/*}}}*/
#endif
/*}}}*/

#if !(defined(CONFIG2) || defined(CONFIG3))
/*{{{  PRIVATE void generate_code_direct*/
PRIVATE void generate_code_direct(treenode *treeroot)
{
	beinit ();
	allocateworkspace (treeroot);
	open_mapfile (treeroot);
	freeup_temp_workspace ();
	if (!stop_after_trans && !stop_after_map) {
		codegenerate (TRUE, treeroot);
	}
	freeup_temp_workspace ();
	close_mapfile ();
}
/*}}}*/
#endif
#endif

#if defined(COMPILING_TO_JCODE)
/*{{{  PRIVATE void compile_to_jcode*/
PRIVATE void compile_to_jcode(treenode *treeroot)
{
	int asm_status;

	init_errorcheck_state();
	beinit();
	optimisations.allow_formals_overlay = FALSE;
	if (!asmtostdout) {
		tmpnam(asmfilename);
		asmout = FALSE;
		if (asmfilename == NULL) {
			harnesserror("Cannot create temporary file");
		}
	} else {
		strcpy(asmfilename, "\0");
	}
	#if 0
	/*{{{  commented out*/
	{
		strcpy(asmfilename, assembly_name ? rootfilename : tmpnam(tempfilename));
		if (assembly_name) {
			strcat(asmfilename, ASMFILE_EXTENSION);
		} else if (!asmtostdout) {
			/* good job we're not trying to compile this... -- frmb */
			ask the compiler to output compacted
			intermediate file
			next_comp_argv("-ZG");
		}
	}
	/*}}}*/
	#endif
	asmstream = asmtostdout ? stdout : fopen(asmfilename, "wb");
	if (asmstream != NULL) {
		/* We have to supply the assembler with the
		 * following information */
		next_asm_argv("-f");
		{
			char processorname[MAX_OPTION_LENGTH];

			strcpy (processorname, "-");
			strcat (processorname,
			processorstring (processortype, processorattr));
			next_asm_argv (processorname);
		}
		next_asm_argv("-a");
		next_asm_argv(sourcefilename);
		next_asm_argv("-n");
		/*next_asm_argv(toolname);*/
		next_asm_argv(TOOL);
		next_asm_argv("-o");
		next_asm_argv(objfilename);
		next_asm_argv( asmfilename);
		errormode_asm_arg();
		if (setymode) {
			next_asm_argv("-y");
		}
		sizeof_int_var = (int32)bytesperword;
		cg (treeroot);
		if (!suppress_asm && !stop_after_trans ) {
			cc_close(&asmstream, asmfilename);
			asm_status = asm_main(asm_argc, asm_argv);
			rm_asmfile();
			if (asm_status != EXIT_SUCCESS) {
				end_compiler(EXIT_FAILURE);
			}
		}
	} else {
		harnesserror_s("Cannot open assembler file: ", asmfilename);
	}
}
/*}}}*/
#endif

/*{{{  PUBLIC int main*/
int main (const int argc, const char *argv[]); /* prototype to shut up gcc's warning */

PUBLIC int main (const int argc, const char *argv[])
{
	/*{{{  initialise memstats*/

	#if defined(HOST_OS_IS_UNIX) && !defined(WIN32)
		original_sbrk = sbrk(0);
	#endif

	/*}}}*/
	/*{{{  set up outfile*/

	#if defined(COMPILING_TO_JCODE)
		init_errstream();
	#endif
	outfile = stdout; /* can't do this as a static initialisation on the VAX */
	#if defined(CONFIG2) || defined(CONFIG3)
		cf_params.cf_outfile = outfile;
	#endif

	/*}}}*/
	/*{{{  set version*/

	host_setup();
	#if OLD
	#if !defined(COMPILING_TO_JCODE)
		if (!setup_version_string()) {
			harnesserror("Version string overflow");
			end_compiler(EXIT_FAILURE);
		}
	#endif
	#endif

	/*}}}*/
	/*{{{  read in the arguments and go*/

	{
		/*{{{  set default arguments*/

		#if defined(COMPILING_TO_JCODE)
			suppress_asm = FALSE;
			asmtostdout = FALSE;
			OPTIMISATION_DEFAULT_INIT;
			asm_argv[0]  = "asm";
			asm_argc = 1;
			init_options();
		#endif
		processortype = UNKNOWN_PROCESSOR_TYPE;

		/*}}}*/

		if (parse_command_line(argc, argv)) {
			/*{{{  start up the compiler*/
			/*{{{  whether to display info*/

			const BOOL display_info = (information || (assembly_output
			#if !defined(COMPILING_TO_JCODE)
				&& ((alloc_strategy & ALLOC_NOCOMMENTS) == 0)
			#endif
				) || disassemble);

			/* printf("starting compiler...\n"); */
			if (display_info) {
				{
					char info[100];
					arg2_get_info_line(USING_ARG_VERSION, cloptions, info);
					#ifndef IDENTIFY
						if (information)
					#endif
					{
						if (assembly_output) {
							fputs(COMMENT_INTRO, outfile);
						}
						fputs(info, outfile); fputs("\n", outfile);
					}
				}
			}
			/*}}}*/
			#ifdef IMS
				if (load_only) {
					end_compiler(EXIT_SUCCESS);
				}
			#endif
			if ((sourcefilename[0] != '\0') && setup_filenames()) {
				/* we had a source file specified */
				/*{{{  do the compile*/
				if (outfile != NULL) {
					treenode *treeroot;
					BOOL error_occurred = FALSE; /* may get set if an error occurs */

					if (display_info && (assembly_output || (etc_output && ims_asm_output))) {
						/*{{{  say what we are doing*/
						if (ims_asm_output) {
							fputs(COMMENT_INTRO, outfile);
						}
						#if defined(CONFIG2) || defined(CONFIG3)
							fputs("Configuring (", outfile);
						#else
							fputs(generatecode ? "Compiling" : "Checking", outfile);
							fprintf(outfile, " (%s,", processorstring(processortype, processorattr));
						#endif
						fprintf(outfile, "%s) \"%s\"", errormodestring(), sourcefilename);
						if (generatecode && !assembly_output && !disassemble) {
						#if defined(CONFIG2) || defined(CONFIG3)
							fprintf(outfile, " to \"%s\" and \"%s\"", cfbfilename, objfilename);
						#else
							fprintf(outfile, " to \"%s\"", objfilename);
						#endif
						}
						fputc('\n', outfile);
						/*}}}*/
					}
					treeroot = call_occam_frontend(&error_occurred);
					/*printtree(stdout,0,treeroot);  Mod by Jim 14/10/96 because of problems with output files */
					if (generatecode && !error_occurred) {
						print_xml_if_required (treeroot);

						/*{{{  code generate*/
						(void)switch_to_real_workspace ();
						init_obj_writer();
						dummyexp_p = newleafnode(S_DUMMYEXP, NOPOSN);
						#if defined(CONFIG2) || defined(CONFIG3)
							call_configurer(treeroot, &error_occurred);
						#elif !defined(COMPILING_TO_JCODE)
							generate_code_direct(treeroot);
						#else
							compile_to_jcode(treeroot);
						#endif
						/*}}}*/
					} else {
						print_source_if_required(treeroot);
					}
					if (print_all_messages) {
						err_print_messages(outfile);
					}
					if (!error_occurred) {
						end_compiler(EXIT_SUCCESS);
					}
				} else {
					harnesserror("Cannot open output file");
				}
			}
			/*}}}*/
		} else if (sourcefilename[0] == '\0') {
			end_compiler(EXIT_SUCCESS);
		}
		/*}}}*/
	}

	/*}}}*/
	end_compiler(EXIT_FAILURE);
	return (0); /* not reached */
}
/*}}}*/

