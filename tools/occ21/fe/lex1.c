/* $Id: lex1.c,v 1.5 1997/09/16 15:58:52 djb1 Exp $ */


/*
 *	occam 2.5 lexical analyser
 *	Copyright (C) 1987 Inmos Limited
 *	Hacked around by Fred Barnes
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
/*#define DEBUG_NEXTSYMB*/

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#include <string.h>

#include "popen_re.h"		/* IMPORTED */

#include "feinc.h"
#include "lexerror.h"
#include "lexdef.h"
#include "chkdef.h"		/* current_fe_handle etc */
#include "objrdr.h"
#include "predefhd.h"
#include "genhdr.h"		/* to load pre-processor with some back-end constants */
#include "compharn.h"		/* for more pre-processor loading */

#include "version.h"		/* for pre-processor reporting */
/*}}}*/
/*{{{  definitions */
/* String termination character,
   mustn't clash with any valid occam character */
#define CLOSE_QUOTE 257

#define MINLEGALCH 32
#define MAXLEGALCH 126

/*{{{  filestruct_t */
/* This is used to keep track of the nesting of #INCLUDE files */

typedef struct filestruct_s {
	struct filestruct_s *next;
	FILE *fileptr;
	char fmode;
	SOURCEPOSN ffileposn;
	int fbaseindent;
	int fstackposition;
	int fpp_iflevel;
	int fpp_relax;
	char fname[MAX_FILENAME_LENGTH];
} filestruct_t;
/*}}}*/
/*}}}*/

/*{{{  global variables */
PUBLIC FILE *infile;

PUBLIC int symb;
PUBLIC int symbindent, lineindent;

PUBLIC wordnode *lexword;
PUBLIC char literalv[MAXSTRING_SIZE + 1];
PUBLIC int literalp;
PUBLIC int pdnumber;		/* **SHC 5-Apr-1988 */

PRIVATE char current_filename[MAX_FILENAME_LENGTH];
/*PUBLIC int current_file;*/
PUBLIC SOURCEPOSN flocn = NOPOSN;

PUBLIC int lexmode;

PUBLIC int currentfilenum;
PUBLIC int allow_asmnames;

#ifdef USER_DEFINED_OPERATORS
extern BOOL user_defined_operators;
#endif
/*}}}*/
/*{{{  locate saved-state for globals */
PRIVATE int ls_valid = 0;	/* default invalid */

PRIVATE int ls_symb, ls_symbindent, ls_lineindent;
PRIVATE wordnode *ls_lexword;
PRIVATE char ls_literalv[MAXSTRING_SIZE + 1];
PRIVATE int ls_literalp, ls_pdnumber;
PRIVATE SOURCEPOSN ls_flocn;
PRIVATE int ls_allow_asmnames;

/*}}}*/
/*{{{  PRIVATE void lex_save_global_state (void)*/
PRIVATE void lex_save_global_state (void)
{
	ls_symb = symb;
	ls_symbindent = symbindent;
	ls_lineindent = lineindent;
	ls_lexword = lexword;
	memcpy (ls_literalv, literalv, MAXSTRING_SIZE + 1);
	ls_literalp = literalp;
	ls_pdnumber = pdnumber;
	ls_flocn = flocn;
	ls_allow_asmnames = allow_asmnames;
	return;
}
/*}}}*/
/*{{{  PRIVATE int lex_restore_global_state (void)*/
PRIVATE int lex_restore_global_state (void)
{
	symb = ls_symb;
	symbindent = ls_symbindent;
	lineindent = ls_lineindent;
	lexword = ls_lexword;
	memcpy (literalv, ls_literalv, MAXSTRING_SIZE + 1);
	literalp = ls_literalp;
	pdnumber = ls_pdnumber;
	flocn = ls_flocn;
	allow_asmnames = ls_allow_asmnames;
	return 0;
}
/*}}}*/
/*  local variables */
/*{{{  local state*/
PRIVATE int ch;
PRIVATE int stringcontinuation;
PRIVATE int insertlength;
PRIVATE int linep;
PRIVATE int currentindent;
PRIVATE int baseindent;
PRIVATE int string_indent;	/* bug 1251 25/9/91 *//* indentation of line containing the start of the string */
PRIVATE int co_indent;		/* indent from lineindent for C-style occam */

PRIVATE int totallines;

/* TRUE when inputline cannot read any more from file */
PRIVATE BOOL endoffile;

/* TRUE when the lexer has sent an S_END symbol: any call to nextsymb
after this is an unexpected end of file. */
PRIVATE BOOL sentend;
/*}}}*/
/*{{{  local saved-state for local state*/
PRIVATE int ls_ch, ls_stringcontinuation, ls_insertlength, ls_linep, ls_currentindent, \
		ls_baseindent, ls_string_indent, ls_co_indent, ls_totallines;
PRIVATE BOOL ls_endoffile, ls_sentend;
/*}}}*/
/*{{{  local line buffer handling */
PRIVATE int linecount;		/* lines in this file */

PRIVATE int linebuffer_len;	/* length of current line */
PRIVATE const char *linebuffer_ptr;	/* ptr to current line */

PRIVATE BOOL eof;		/* TRUE when last symb was END     */
PRIVATE BOOL newlineflag;	/* TRUE when last symb was NEWLINE */
PRIVATE BOOL desc_eof;		/* TRUE when we hit the end of a descriptor file */
PRIVATE filestruct_t *filestack = NULL;
PRIVATE int filestackptr = 0;
/*}}}*/
/*{{{  local saved-state for buffer handling*/
PRIVATE int ls_linecount, ls_linebuffer_len;
PRIVATE const char *ls_linebuffer_ptr;
PRIVATE BOOL ls_eof, ls_newlineflag, ls_desc_eof;
/*}}}*/
/*{{{  PRIVATE void lex_save_local_state (void)*/
PRIVATE void lex_save_local_state (void)
{
	ls_ch = ch;
	ls_stringcontinuation = stringcontinuation;
	ls_insertlength = insertlength;
	ls_linep = linep;
	ls_currentindent = currentindent;
	ls_baseindent = baseindent;
	ls_string_indent = string_indent;
	ls_co_indent = co_indent;
	ls_totallines = totallines;
	ls_endoffile = endoffile;
	ls_sentend = sentend;
	ls_linecount = linecount;
	ls_linebuffer_len = linebuffer_len;
	ls_linebuffer_ptr = linebuffer_ptr;
	ls_eof = eof;
	ls_newlineflag = newlineflag;
	ls_desc_eof = desc_eof;
	return;
}
/*}}}*/
/*{{{  PRIVATE int lex_restore_local_state (void)*/
PRIVATE int lex_restore_local_state (void)
{
	ch = ls_ch;
	stringcontinuation = ls_stringcontinuation;
	insertlength = ls_insertlength;
	linep = ls_linep;
	currentindent = ls_currentindent;
	baseindent = ls_baseindent;
	string_indent = ls_string_indent;
	co_indent = ls_co_indent;
	totallines = ls_totallines;
	endoffile = ls_endoffile;
	sentend = ls_sentend;
	if ((linecount != ls_linecount) || (linebuffer_len != ls_linebuffer_len) ||
			(linebuffer_ptr != ls_linebuffer_ptr)) {
		/* dammit, got a new line in..! --> shouldn't happen, abort */
		return 1;
	}
	eof = ls_eof;
	newlineflag = ls_newlineflag;
	desc_eof = ls_desc_eof;
	return 0;
}
/*}}}*/

/*{{{  local C-occam stuff handling */
PRIVATE int brace_counter;			/* counts `{' and '}' */
#define MAX_PROC_DEPTH 256
PRIVATE BOOL processing_decl;			/* set after seeing '{' -- to handle declarations */
PRIVATE int proc_decl_stack[MAX_PROC_DEPTH];	/* tracks where we should turn '}' into NEWLINE + ':' */
PRIVATE int proc_decl_ptr;			/* pointer into above */
PRIVATE int do_force_newline;			/* set after seeing various things to force a 1-newline/2-colon-newline token */
PRIVATE BOOL just_seen_prockeyword;		/* set after seeing S_PROC */

/*}}}  */

/*{{{  forward declarations */
PRIVATE void bufinit (void);
PRIVATE int readhex (void);
PRIVATE int readchex (void);
PRIVATE void preproc_add_define (wordnode *name, int len, int vtype, void *vptr);
PRIVATE char *new_string (char *str);
/*}}}*/

/*{{{  PUBLIC void lex_save_state (void)*/
PUBLIC void lex_save_state (void)
{
	lex_save_global_state ();
	lex_save_local_state ();
	ls_valid = 1;
	return;
}
/*}}}*/
/*{{{  PUBLIC void lex_restore_state (void)*/
PUBLIC void lex_restore_state (void)
{	
	int i;

	if (!ls_valid) {
		lexfatal (LEX_NO_SAVE_STATE, flocn);
	} else {
		i = lex_restore_global_state ();
		i += lex_restore_local_state ();
		if (i) {
			lexfatal (LEX_SAVE_STATE_ERROR, flocn);
		}
		ls_valid = 0;
	}
	return;
}
/*}}}*/

/*{{{  (private) pre-processor stuff*/
static wordnode *pp_defined = NULL;
static wordnode *pp_true = NULL;
static wordnode *pp_false = NULL;
static wordnode *pp_not = NULL;
static wordnode *pp_and = NULL;
static wordnode *pp_or = NULL;

#define PP_VAL_NONE 0
#define PP_VAL_INT 1
#define PP_VAL_STRING 2

/* some operator constants */
#define PP_OP_NONE 0
#define PP_OP_EQ 1
#define PP_OP_NE 2
#define PP_OP_LT 3
#define PP_OP_LE 4
#define PP_OP_GT 5
#define PP_OP_GE 6

/* defined variables */
struct TAG_pp_dlist {
	wordnode *name;
	int namelen;
	int valtype;			/* PP_VAL_... */
	void *valdata;			/* pointer to something */
	struct TAG_pp_dlist *next;
};
typedef struct TAG_pp_dlist pp_dlist_t;

struct TAG_pp_ifstack {
	int indent;			/* indentation level of the #IF/... */
	SOURCEPOSN locn;		/* location of the #IF which started it */
	BOOL hadtrue;			/* indicates whether to test #ELIFs/#ELSE */
	BOOL skipinput;			/* if TRUE, skip all non pre-processor things (ie, comment out) */
	BOOL relaxed;			/* if TRUE, the contents of this IF are relaxed (pulled backwards for indentation) */
	struct TAG_pp_ifstack *next;
};
typedef struct TAG_pp_ifstack pp_ifstack_t;

/* these three are separately handled -- never placed in the list directly */
static pp_dlist_t *pp_xfile = NULL;		/* current filename */
static pp_dlist_t *pp_xline = NULL;		/* current line-number */
static pp_dlist_t *pp_xfilestack = NULL;	/* current file-stack depth (top-level unit is 0) */

static pp_dlist_t *pp_dlist = NULL;	/* nothing defined to start with */
static pp_ifstack_t *pp_ifstack = NULL;
static int pp_iflevel = 0;		/* how many #IFs we're dealing with currently */
static int pp_relax = 0;		/* non-zero if RELAXing code (to undo indentation) */

#define MAX_CMDLINE_DEFINES 128
static char *pp_cmdline_defines[MAX_CMDLINE_DEFINES];
static int pp_cmdline_ndef = 0;
/*}}}*/

/*{{{  error handler */
/*{{{  PUBLIC void nextline () */
/* Skip to start of next line */
PUBLIC void nextline (void)
{
	while (symb != S_NEWLINE) {
		nextsymb ();
	}
	while (symb == S_NEWLINE) {
		nextsymb ();
	}
}

/*}}}*/


/*{{{  PUBLIC void skiplines (indent) */
PUBLIC void skiplines (const int indent)
{
	/*int i = 0; */

	if (symbindent > indent) {
		/* bug 1018 2/11/90 */
		nextline ();	/* force at least a single new line, even if this line starts ok */
	}

	while (lineindent > indent) {
		nextline ();
		/*++i; */
	}
#if 0
	if (i != 0) {
		fprintf (errfile, "Skipped %d line%s\n", i, (i = 1) ? "" : "s");
	}
#endif
	while (symb == S_COMMENT) {
		nextline ();
	}
}

/*}}}*/
/*}}}*/
/*{{{  predefine handling */
/*{{{  predef_lang_t */
typedef enum {
	A = FE_LANG_ALL,	/* all languages */
	O = FE_LANG_OCCAM,	/* compiler only */
	C = FE_LANG_CONFIG2 | FE_LANG_CONFIG3,	/* configurer only */
	CC = FE_LANG_OCCAM | C,	/* compiler and configurer */
	N = FE_LANG_NDL,	/* NDL reader only */
	CN = FE_LANG_CONFIG2 | N,	/* configurer H/W and NDL reader */
	C2 = FE_LANG_CONFIG2	/* Version 2 configurer only */
} predef_lang_t;
/*}}}*/
/*{{{  predefined routines defns. */
/*{{{  structure format */
PRIVATE const struct predeflinestruct
{
	predef_lang_t langs;
	int pnumber;
	const char *pline;
}
predeflines[] =
/*}}}*/
/* These don't HAVE to be in the correct order, but it is clearer */
{
	/*{{{  longarithmetic */
	  { CC, PD_LONGADD, "FI(VI,VI,VI):LONGADD"}
	, { CC, PD_LONGSUM, "FI,I(VI,VI,VI):LONGSUM"}
	, { CC, PD_LONGSUB, "FI(VI,VI,VI):LONGSUB"}
	, { CC, PD_LONGDIFF, "FI,I(VI,VI,VI):LONGDIFF"}
	, { CC, PD_LONGPROD, "FI,I(VI,VI,VI):LONGPROD"}
	, { CC, PD_LONGDIV, "FI,I(VI,VI,VI):LONGDIV"}
	, /*}}} */
		/*{{{  shift */
	  { CC, PD_SHIFTRIGHT, "FI,I(VI,VI,VI):SHIFTRIGHT"}
	, { CC, PD_SHIFTLEFT, "FI,I(VI,VI,VI):SHIFTLEFT"}
	, /*}}} */
		/*{{{  normalise,fracmul */
	  { CC, PD_NORMALISE, "FI,I,I(VI,VI):NORMALISE"}
	, { CC, PD_FRACMUL, "FI(VI,VI):FRACMUL"}
	, /*}}} */
		/*{{{  shift,arithmeticshift,rotate */
	  { CC, PD_ASHIFTRIGHT, "FI(VI,VI):ASHIFTRIGHT"}
	, { CC, PD_ASHIFTLEFT, "FI(VI,VI):ASHIFTLEFT"}
	, { CC, PD_ROTATERIGHT, "FI(VI,VI):ROTATERIGHT"}
	, { CC, PD_ROTATELEFT, "FI(VI,VI):ROTATELEFT"}
	, /*}}} */
		/*{{{  causeerror */
	  { CC, PD_CAUSEERROR, "P():CAUSEERROR"}
	, /*}}} */
	#if 0
		/*{{{  kernelrun,loadchannel,loadchannelvector,loadbytevector */
	  { CC, PD_KERNELRUN, "P(V[]Y,VI,[]I,VI):KERNEL.RUN"}
	, { CC, PD_LOADINPUTCHANNEL, "P(I,C):LOAD.INPUT.CHANNEL"}
	, { CC, PD_LOADOUTPUTCHANNEL, "P(I,C):LOAD.OUTPUT.CHANNEL"}
	, { CC, PD_LOADINPUTCHANNELVECTOR, "P(I,[]C):LOAD.INPUT.CHANNEL.VECTOR"}
	, { CC, PD_LOADOUTPUTCHANNELVECTOR, "P(I,[]C):LOAD.OUTPUT.CHANNEL.VECTOR"}
	, { CC, PD_LOADBYTEVECTOR, "P(I,V[]Y):LOAD.BYTE.VECTOR"}
	, /*}}} */
	#endif
		/*{{{  unpacksnroundsn */
	  { CC, PD_UNPACKSN, "FI,I,I(VI):UNPACKSN"}
	, { CC, PD_ROUNDSN, "FI(VI,VI,VI):ROUNDSN"}
	, /*}}} */
		/*{{{  draw2d,clip2d,move2d */
	  { CC, PD_DRAW2D, "P(V[][]Y,VI,VI,[][]Y,VI,VI,VI,VI):DRAW2D"}
	, { CC, PD_CLIP2D, "P(V[][]Y,VI,VI,[][]Y,VI,VI,VI,VI):CLIP2D"}
	, { CC, PD_MOVE2D, "P(V[][]Y,VI,VI,[][]Y,VI,VI,VI,VI):MOVE2D"}
	, /*}}} */
		/*{{{  crcbyteandword */
	  { CC, PD_CRCWORD, "FI(VI,VI,VI):CRCWORD"}
	, { CC, PD_CRCBYTE, "FI(VI,VI,VI):CRCBYTE"}
	, /*}}} */
		/*{{{  bitops */
	  { CC, PD_BITCOUNT, "FI(VI,VI):BITCOUNT"}
	, { CC, PD_BITREVWORD, "FI(VI):BITREVWORD"}
	, { CC, PD_BITREVNBITS, "FI(VI,VI):BITREVNBITS"}
	, /*}}} */
		/*{{{  floatingpoint */
	  { CC, PD_ABS, "FR3(VR3):ABS"}
	, { CC, PD_ISNAN, "FB(VR3):ISNAN"}
	, { CC, PD_NOTFINITE, "FB(VR3):NOTFINITE"}
	, { CC, PD_ORDERED, "FB(VR3,VR3):ORDERED"}
	, { CC, PD_MINUSX, "FR3(VR3):MINUSX"}
	, { CC, PD_MULBY2, "FR3(VR3):MULBY2"}
	, { CC, PD_DIVBY2, "FR3(VR3):DIVBY2"}
	, { CC, PD_SQRT, "FR3(VR3):SQRT"}
	, { CC, PD_FPINT, "FR3(VR3):FPINT"}
	, { CC, PD_DABS, "FR6(VR6):DABS"}
	, { CC, PD_DISNAN, "FB(VR6):DISNAN"}
	, { CC, PD_DNOTFINITE, "FB(VR6):DNOTFINITE"}
	, { CC, PD_DORDERED, "FB(VR6,VR6):DORDERED"}
	, { CC, PD_DMINUSX, "FR6(VR6):DMINUSX"}
	, { CC, PD_DMULBY2, "FR6(VR6):DMULBY2"}
	, { CC, PD_DDIVBY2, "FR6(VR6):DDIVBY2"}
	, { CC, PD_DSQRT, "FR6(VR6):DSQRT"}
	, { CC, PD_DFPINT, "FR6(VR6):DFPINT"}
	, { CC, PD_SCALEB, "FR3(VR3,VI):SCALEB"}
	, { CC, PD_DSCALEB, "FR6(VR6,VI):DSCALEB"}
	, { CC, PD_COPYSIGN, "FR3(VR3,VR3):COPYSIGN"}
	, { CC, PD_DCOPYSIGN, "FR6(VR6,VR6):DCOPYSIGN"}
	, { CC, PD_NEXTAFTER, "FR3(VR3,VR3):NEXTAFTER"}
	, { CC, PD_DNEXTAFTER, "FR6(VR6,VR6):DNEXTAFTER"}
	, { CC, PD_LOGB, "FR3(VR3):LOGB"}
	, { CC, PD_DLOGB, "FR6(VR6):DLOGB"}
	, { CC, PD_FLOATINGUNPACK, "FI,R3(VR3):FLOATING.UNPACK"}
	, { CC, PD_DFLOATINGUNPACK, "FI,R6(VR6):DFLOATING.UNPACK"}
	, { CC, PD_ARGUMENTREDUCE, "FB,I32,R3(VR3,VR3,VR3):ARGUMENT.REDUCE"}
	, { CC, PD_DARGUMENTREDUCE, "FB,I32,R6(VR6,VR6,VR6):DARGUMENT.REDUCE"}
	, /*}}} */
		/*{{{  IEEEarithmetic */
	  { CC, PD_REAL32OP, "FR3(VR3,VI,VR3):REAL32OP"}
	, { CC, PD_REAL64OP, "FR6(VR6,VI,VR6):REAL64OP"}
	, { CC, PD_IEEE32OP, "FB,R3(VR3,VI,VI,VR3):IEEE32OP"}
	, { CC, PD_IEEE64OP, "FB,R6(VR6,VI,VI,VR6):IEEE64OP"}
	, { CC, PD_REAL32REM, "FR3(VR3,VR3):REAL32REM"}
	, { CC, PD_REAL64REM, "FR6(VR6,VR6):REAL64REM"}
	, { CC, PD_REAL32EQ, "FB(VR3,VR3):REAL32EQ"}
	, { CC, PD_REAL64EQ, "FB(VR6,VR6):REAL64EQ"}
	, { CC, PD_REAL32GT, "FB(VR3,VR3):REAL32GT"}
	, { CC, PD_REAL64GT, "FB(VR6,VR6):REAL64GT"}
	, { CC, PD_IEEECOMPARE, "FI(VR3,VR3):IEEECOMPARE"}
	, { CC, PD_DIEEECOMPARE, "FI(VR6,VR6):DIEEECOMPARE"}
	, { CC, PD_IEEE32REM, "FB,R3(VR3,VR3):IEEE32REM"}
	, { CC, PD_IEEE64REM, "FB,R6(VR6,VR6):IEEE64REM"}
	, /*}}} */
		/*{{{  reschedule/assert */
	  { CC, PD_RESCHEDULE, "P():RESCHEDULE"}
	, { CC, PD_ASSERT, "P(VB):ASSERT"}
	, { CC, PD_WSSIZEOF, "FI(VI):WSSIZEOF"}
	, { CC, PD_VSSIZEOF, "FI(VI):VSSIZEOF"}
	, /*}}} */
	#if 0
		/*{{{  Used by the compiler to update profile counts */
	  { CC, PD_UPDATE_PROFCOUNT, "P(VI):UPDATE.PROFCOUNT"}
	, /*}}} */
	#endif
	#if 0
		/*{{{  configurer s/w attributes */
	  { C, PD_IMS_GET_PDATA, "FI():IMS.GET.PDATA"}
	, /*}}} */
	#endif
	  /*{{{  priority stuff added by frmb */
	  { CC, PD_GETPRI, "P(=I):GETPRI"}
	, { CC, PD_SETPRI, "P(VI):SETPRI"}
	, { CC, PD_INCPRI, "P():INCPRI"}
	, { CC, PD_DECPRI, "P():DECPRI"}
	, { CC, PD_INCPRI, "P():RAISE.PRIORITY"}
	, { CC, PD_DECPRI, "P():LOWER.PRIORITY"}
	  /*}}}  */
	  /*{{{  mobile chantype allocation stuff added by frmb */
	, { CC, PD_ALLOC_CHAN_TYPE, "P(C,C):ALLOC.CHAN.TYPE"}		/* this says CHAN, CHAN; but it's not.  real checks in chk1/chk4 */
	  /*}}}*/
	  /*{{{  encode/decode and friends added by frmb */
	#ifdef PD_PROTOCOL_HASH
	, { CC, PD_PROTOCOL_HASH, "FI(C):PROTOCOL.HASH"}		/* will actually accept any type/protocol here, real checks in chk1/chk4 */
	#endif
	#ifdef PD_DECODE_CHANNEL
	, { CC, PD_DECODE_CHANNEL, "P(C,C,C,C):DECODE.CHANNEL"}		/* chan *, CHAN INT, CHAN [some seq protocol carrying 2 INTs], CHAN BOOL */
	#endif
	#ifdef PD_DECODE_CHANNEL3
	, { CC, PD_DECODE_CHANNEL3, "P(C,C,C):DECODE.CHANNEL3"}		/* chan *, CHAN INT, CHAN [ ditto                           ] */
	#endif
	#ifdef PD_ENCODE_CHANNEL
	, { CC, PD_ENCODE_CHANNEL, "P(C,C,C):ENCODE.CHANNEL"}		/* CHAN [some seq protocol carrying 2 INTs], CHAN INT, CHAN * */
	#endif
	#ifdef MOBILES		/* need mobile support for these two */
	#ifdef PD_DETACH_DYNMOB
	, { CC, PD_DETACH_DYNMOB, "P(M[]Y,I,I):DETACH.DYNMOB"}		/* lets hope they don't get it wrong...! */
	#endif
	#ifdef PD_ATTACH_DYNMOB
	, { CC, PD_ATTACH_DYNMOB, "P(VI,VI,M[]Y):ATTACH.DYNMOB"}	/* ditto. */
	#endif
	#endif	/* MOBILES */
	,  { CC, PD_DECODE_DATA, "P(I,I,I):DECODE.DATA"}
	  /*}}}*/
	  /*{{{  occampi stuff*/
	, { CC, PD_MPBARSYNC, "P()S:MPBARSYNC"}
	, { CC, PD_MPBARRESIGN, "P()S:MPBARRESIGN"}
	, { CC, PD_MPPSERIALISE, "P(m,M[]Y):MPP.SERIALISE"}
	, { CC, PD_MPPDESERIALISE, "P(M[]Y,m):MPP.DESERIALISE"}
	, { CC, PD_MPPCHECKROUTINE, "P(V[]Y,B):MPP.CHECKROUTINE"}
	, { CC, PD_MPPLOADLIBRARY, "P(V[]Y,B):MPP.LOADLIBRARY"}
	, { CC, PD_MPPUNLOADLIBRARY, "P(V[]Y,B):MPP.UNLOADLIBRARY"}
	#if 0
	, { CC, PD_LOAD_TYPE_DESC, "FI(C):LOAD.TYPE.DESC"}		/* will accept any mobile name/type, channel just used for plugging */
	#endif
	, /*}}}*/
	  /*{{{  more floating-point*/
	  { CC, PD_REAL32SIN, "FR3(VR3):SIN"}
	, { CC, PD_REAL64SIN, "FR6(VR6):DSIN"}
	, { CC, PD_REAL32COS, "FR3(VR3):COS"}
	, { CC, PD_REAL64COS, "FR6(VR6):DCOS"}
	, { CC, PD_REAL32TAN, "FR3(VR3):TAN"}
	, { CC, PD_REAL64TAN, "FR6(VR6):DTAN"}
	, /*}}}*/
	  /*{{{  affinity stuff added by CGR */
	  { CC, PD_GETAFF, "P(=I):GETAFF"}
	, { CC, PD_SETAFF, "P(VI):SETAFF"}
	, /*}}}  */
	  /*{{{  blocking system calls */
	  { CC, PD_KILLCALL, "P(C,I):KILLCALL"}
	, /*}}}*/
	  /*{{{  RMoX interrupt handling by CGR */
	  { CC, PD_WAIT_FOR_INTERRUPT, "P(VI,VI,=I):WAIT.FOR.INTERRUPT"}
	, /*}}}*/
	  /*{{{  mobile manipulation by CGR */
	  { CC, PD_BIND_MOBILE, "P(a,VI):BIND.MOBILE"}
	, { CC, PD_BIND_MOBILE_HW, "P(a,VI):BIND.MOBILE.HW"}
	, { CC, PD_DMA_CAPABLE, "FB(a):DMA.CAPABLE"}
	, { CC, PD_MAKE_DMA_CAPABLE, "P(a):MAKE.DMA.CAPABLE"}
	, { CC, PD_RESIZE_MOBILE_ARRAY_1D, "P(a,VI):RESIZE.MOBILE.ARRAY.1D"}
	, /*}}}*/
	  /*{{{  memory barriers by CGR */
	  { CC, PD_MEMORY_BARRIER, "P():MEMORY.BARRIER"}
	, { CC, PD_READ_MEMORY_BARRIER, "P():READ.MEMORY.BARRIER"}
	, { CC, PD_WRITE_MEMORY_BARRIER, "P():WRITE.MEMORY.BARRIER"}
	, /*}}}*/
		/*{{{  configurer attributes */
	  { CN, PD_ATTR_LINK, ". [4]E :link"}
	, { CN, PD_ATTR_TYPE, ". []Y :type"}
	, { CN, PD_ATTR_MEMSIZE, ". I :memsize"}
	, { CN, PD_ATTR_ROMSIZE, ". I :romsize"}
	, { CN, PD_ATTR_ROOT, ". B :root"}
	, { CN, PD_ATTR_MEMSTART, ". I :memstart"}
	, { CN, PD_ATTR_NUM_LINKS, ". I :numlinks"}
	, { C, PD_ATTR_ORDER_CODE, ". I :order.code"}
	, { C, PD_ATTR_ORDER_VS, ". I :order.vs"}
	, { C, PD_ATTR_LOCATION_CODE, ". I :location.code"}
	, { C, PD_ATTR_LOCATION_WS, ". I :location.ws"}
	, { C, PD_ATTR_LOCATION_VS, ". I :location.vs"}
	, { C, PD_ATTR_RESERVED, ". I :reserved"}
	, { C2, PD_ATTR_TOLERANCE, ". I :tolerance"}
	, { C2, PD_ATTR_LINKQUOTA, ". I :linkquota"}
	, { C2, PD_ATTR_ROUTECOST, ". I :routecost"}
	, { C, PD_ATTR_ORDER_WS, ". I :order.ws"}
	, { C, PD_ATTR_NODEBUG, ". B :nodebug"}
	, { C, PD_ATTR_NOPROFILE, ". B :noprofile"}
	, { C2, PD_HOSTEDGE, "E :HOST"}
	, { N, PD_DEFAULTNODE, "N :DEFAULT"}
	, /*}}} */
		/*{{{  NDL reader attributes */
	{ N, PD_ATTR_LINK_SPEED_MULTIPLY, ". I :link.speed.multiply"}
	, { N, PD_ATTR_LINK_SPEED_DIVIDE, ". []I :link.speed.divide"}
	, { N, PD_ATTR_CONTROL, ". E :control"}
	, { N, PD_ATTR_CONTROL_UP, ". E :control.up"}
	, { N, PD_ATTR_CONTROL_DOWN, ". E :control.down"}
	, { N, PD_ATTR_CONTROL_SPEED_DIVIDE, ". [2]I :control.speed.divide"}
	, { N, PD_ATTR_MEMCONFIG, ". []Y :memconfig"}
	, { N, PD_ATTR_PREAMBLE, ". []Y :preamble"}
	, { N, PD_ATTR_BOOTSTRAP, ". []Y :bootstrap"}
	, { N, PD_ATTR_MEMORY, ". [][3]I :memory"}
	, { N, PD_ATTR_CACHESIZE, ". I :cachesize"}
	, /*{ N,PD_ATTR_LINK_TXMODE          , ". [4]B :link.Txmode"}, */
	{ N, PD_ATTR_LINK_BYTE_MODE, ". [4]B :link.byte.mode"}
	, { N, PD_ATTR_EVENT_OUT, ". [4]B :event.out"}
	, { N, PD_ATTR_LOCAL_ROM, ". B :local.rom"}
	, { N, PD_ATTR_SYSTEM_ROM, ". B :system.rom"}
	, { N, PD_ATTR_PMI_CONFIG_INROM, ". B :pmi.config.inrom"}
	, { N, PD_ATTR_LINKSET_INROM, ". B :linkset.inrom"}
	, { N, PD_ATTR_BOOTSTRAP_INROM, ". B :bootstrap.inrom"}
	, { N, PD_ATTR_CACHE_CONFIG_INROM, ". B :cache.config.inrom"}
	, /*{ N,PD_ATTR_INTERVAL             , ". [][2]I :interval"}, */
	{ N, PD_ATTR_LINK_SELECT, ". []I :link.select"}
	, { N, PD_ATTR_DISCARD, ". []B :discard"}
	, { N, PD_ATTR_DELETE, ". []B :delete"}
	, { N, PD_ATTR_RANDOM, ". []B :random"}
	, /*{ N,PD_ATTR_RANDOMSEED           , ". I :randomseed"}, */
	{ N, PD_ATTR_RANDOM_INTERVAL, ". [2]I :random.interval"}
	, { N, PD_ATTR_NODE, ". N :node"}
	, { N, PD_ATTR_LINKS, ". [][2]I :links"}
	, { N, PD_ATTR_START_CODE, ". Y :start.code"}
	, { N, PD_ATTR_IDENTIFY_CODE, ". Y :identify.code"}
	, { N, PD_ATTR_IDENTIFY_RESPONSE, ". Y :identify.response"}
	, { N, PD_ATTR_EXPECTED_IDENTITY, ". I :expected.identity"}
	, { N, PD_ATTR_ERROR_RESPONSE, ". Y :error.response"}
	, { N, PD_ATTR_ERROR_HANDSHAKE, ". Y :error.handshake"}
	, { N, PD_ATTR_INITIALISATION, ". []I :initialisation"}
	, { N, PD_ATTR_DATA, ". E :data"}
	, { N, PD_ATTR_OUTWARD, ". []A :outward"}
	, { N, PD_ATTR_RETURN, ". []A :return"}
	, { N, PD_ATTR_LINK_GROUPS, ". [][2]I :link.groups"}
	, { N, PD_ATTR_INTERVAL_SEPARATOR, ". []I :interval.separator"}
	, { N, PD_ATTR_ROM_FILE, ". []Y :rom.file"}
	, { N, PD_ATTR_REBOOT_FROM_LINK, ". B :reboot.from.link"}
	, { N, PD_ATTR_BOOT_FROM_ROM, ". B :boot.from.rom"}
	, { N, PD_ATTR_MODE, ". I :mode"}
	, { N, PD_ATTR_DS_LINK, ". [4]E :ds.link"}
	, /*}}} */
	{ A, 0, ""}
};

/*}}}*/
/*{{{  predecode */
PRIVATE char *addto (char *l, const char *s)
{
	while (*s != '\0') {
		*(l++) = *(s++);
	}
	return l;
}

/* Decode the predefine string into line */
PRIVATE void predecode (char *l, const char *s)
{
	while (*s != '\0' && *s != ':') {
		/*{{{  decode */
		const char *expand;

		switch (*s) {
		case 'B':
			expand = "BOOL";
			break;
		case 'C':
			expand = "CHAN OF ANY";
			break;
		case 'F':
			expand = "FUNCTION ";
			break;
		case 'I':
			expand = "INT";
			break;
		case 'P':
			expand = "PROC";
			break;
		case 'R':
			expand = (*++s == '3' ? "REAL32" : "REAL64");
			break;
		case 'V':
			expand = "VAL ";
			break;
#ifdef MOBILES
		case 'M':
			expand = "MOBILE ";
			break;
		case 'm':
			expand = "MOBILE.PROC";
			break;
		case 'S':
			expand = "SUSPEND ";
			break;
		case 'a':
			expand = "MOBILE.ANY";
			break;
#endif
		case 'Y':
			expand = "BYTE";
			break;
		case '=':
			expand = "RESULT ";
			break;
		case '-':
			expand = "UNDEFINED ";
			break;
#if 1				/*def CONFIG */
		case 'A':
			expand = "ARC";
			break;
		case 'E':
			expand = "EDGE";
			break;
		case 'N':
			expand = "NODE";
			break;
#endif
		default:
			expand = NULL;
			break;
		}
		if (expand != NULL) {
			l = addto (l, expand);
		} else {
			*(l++) = *s;
		}
		s++;
		/*}}} */
	}
	if (*s == ':') {
		s++;
	}
	l = addto (l, s);
	l = addto (l, ":\n");
	*l = '\0';
}
/*}}}*/
/*{{{  PRIVATE char *readpredefline (line) */
/*{{{  comment */
/*****************************************************************************
 *
 *  readpredefline copies a predefined name specification into line,
 *                 and returns a pointer to the first character of line.
 *
*****************************************************************************/
/*}}}*/
PRIVATE char *readpredefline (char *const line, int *const line_len)
{
	if (linecount < ((sizeof (predeflines) / sizeof (struct predeflinestruct)) - 1)) {
		if ((predeflines[linecount].langs & current_fe_data->fe_lang) != 0) {
#if 0 /* defined(MOBILES) */
			if (mobile_data_types || ((predeflines[linecount].pnumber != PD_ATTACH_DYNMOB) && (predeflines[linecount].pnumber != PD_DETACH_DYNMOB))) {
				predecode (line, predeflines[linecount].pline);
			}
#else
			predecode (line, predeflines[linecount].pline);
#endif
#if 0
fprintf (stderr, "readpredefline: line = %s", line);
#endif
		} else {
			strcpy (line, "\n");
		}
		pdnumber = predeflines[linecount].pnumber;	/* **SHC 5-Apr-1988 */
		if (!has_sincostan) {
			switch (pdnumber) {
			case PD_REAL32SIN:
			case PD_REAL64SIN:
			case PD_REAL32COS:
			case PD_REAL64COS:
				strcpy (line, "\n");
				break;
			}
		}
		if (!mobile_data_types) {
			/* if one of these is a MOBILE procedure from the forall library, ignore */
			switch (pdnumber) {
			case PD_MPBARSYNC:
			case PD_MPBARRESIGN:
				strcpy (line, "\n");
				break;
			}
		}
		if (!mobile_proc_types) {
			switch (pdnumber) {
			case PD_MPPSERIALISE:
			case PD_MPPDESERIALISE:
			case PD_MPPCHECKROUTINE:
			case PD_MPPLOADLIBRARY:
			case PD_MPPUNLOADLIBRARY:
				strcpy (line, "\n");
				break;
			}
		}
		*line_len = strlen (line);
		return (line);
	}
	return (NULL);
}
/*}}}*/
/*{{{  PUBLIC void init_predefs_lexer */
PUBLIC void init_predefs_lexer (void)
{
	lexmode = LEX_PREDEFS;
	strcpy (current_filename, "");
	desc_eof = FALSE;
	bufinit ();
	nextsymb ();
	if (symb == S_NEWLINE) {
		nextline ();
	}
}
/*}}}*/
/*{{{  PUBLIC const char *get_predef_string */
PUBLIC const char *get_predef_string (const int pdno)
{
	int i;

	for (i = 0; i < ((sizeof (predeflines) / sizeof (struct predeflinestruct)) - 1); i++) {
		if (predeflines[i].pnumber == pdno) {
			const char *const s = strrchr (predeflines[i].pline, ':');

			if (s == NULL) {
				return NULL;
			}
			return s + 1;	/* skip past the colon char */
		}
	}
	return NULL;
}

/*}}}*/
/*}}}*/

/* This version allows lines of arbitrary length */

/*This _doesn't_ impose a hard limit on the length of lines;
they are read in in buffer chunks of this size.
*/
#define LINEMAX             255

PRIVATE char *linebuffer = NULL;
PRIVATE size_t linesize = LINEMAX;


/*{{{  PRIVATE char *readsourceline */
PRIVATE char *readsourceline (int *const line_len)
{
	char *ptr;
	int spaces = 0;
	BOOL expandtabs = TRUE;	/* expand only until first non-whitespace character */
	char buf[LINEMAX];
	const char *tab_ptr = NULL;
	register const char *src_ptr = buf;
	register char *local_linebuffer = linebuffer;
	register int dest_col = 0;

	ptr = fgets (buf, LINEMAX, infile);
	if (ptr == NULL) {
		return NULL;
	}

	while (TRUE) {
		register int c = *src_ptr++;
		switch (c) {
		case '\n':	/* hit the end of a line */
		      eol_label:
			local_linebuffer[dest_col] = '\n';
			local_linebuffer[dest_col + 1] = '\0';
			*line_len = dest_col + 1;
			return local_linebuffer;
		case '\0':	/* hit the end of a buffer before end of line */
			DEBUG_MSG (("readsourceline: reading more from file\n"));
			ptr = fgets (buf, LINEMAX, infile);
			/*if (ptr == NULL) return NULL; */
			if (ptr == NULL) {
				goto eol_label;	/* INSdi03520 allow for final line with no lf */
			}
			src_ptr = buf;
			break;
		case '\t':
			DEBUG_MSG (("readsourceline: found a tab\n"));
			if (lexmode == LEX_CSOURCE) {
				/* ignore it */
				break;
			} else if (expandtabs) {
				src_ptr--;	/* point back to the tab character */
				if (spaces != 0) {
					c = ' ';
					spaces--;
					goto addchar_label;
				}
				if (src_ptr == tab_ptr) {
					/* at the end of the last tab */
					src_ptr++;	/* finished 'processing' the tab, now move on */
				} else {
					tab_ptr = src_ptr;	/* remember this position */
					spaces = TABSIZE - (dest_col % TABSIZE);
				}
				break;	/* break out of the whole switch */
				/* else we have finished all the spaces for this tab */
			}
			DEBUG_MSG (("readsourceline: dropping through from tab\n"));
			/* FALL THROUGH */
		default:
			expandtabs = FALSE;	/* don't expand tabs once a char has been seen */
			/* FALL THROUGH */
		case ' ':
		      addchar_label:
			local_linebuffer[dest_col++] = c;
			if (dest_col >= linesize) {
				char *const new = memalloc (linesize * 2 + 2);	/* 2 extra for "\n\0" */

				DEBUG_MSG (("readsourceline: extending linebuffer to %ld\n", (long)(linesize * 2)));
				memcpy (new, linebuffer, linesize);
				memfree (linebuffer);
				linebuffer = new;
				local_linebuffer = linebuffer;
				linesize *= 2;
			}
			break;
		}
	}
}
/*}}}*/
/*{{{  PRIVATE void readline () */
/*{{{  comment */
/*****************************************************************************
 *
 *  readline updates lineptr to point to a structure containing
 *           the next line of source, reading
 *           a line from the input file if neccessary.
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void readline (void)
{
	do {
		DEBUG_MSG (("readline: calling inputline\n"));
		/*{{{  input a line */
		/*****************************************************************************
		 *
		 *  inputline reads a text line from the input file into a structure element
		 *            of array buf.
		 *
		 *****************************************************************************/
		{
			const char *ptr;

			/*{{{  read in line */
			switch (lexmode) {
			default:	/*case LEX_SOURCE: case LEX_CSOURCE */
				ptr = readsourceline (&linebuffer_len);
				break;
			case LEX_SC:
			case LEX_LIB:
			case LEX_STDLIB:
				ptr = readdescriptorline (linebuffer, lexmode == LEX_STDLIB, &linebuffer_len);
				break;
#if 1 /*ndef CONFIG*/		/* no #PRAGMA EXTERNAL when configuring */
			case LEX_EXTERNAL:
			case LEX_DEXTERNAL:
				ptr = readexternalline (&linebuffer_len);
				break;
#endif
			case LEX_PREDEFS:
				ptr = readpredefline (linebuffer, &linebuffer_len);
				break;
			}
			/*}}} */
			if (ptr == NULL) {
				/*{{{  end of file */
#if 0				/* This changed 23/4/90 by CO'N */
				if ((lexmode == LEX_PREDEFS) || (lexmode == LEX_EXTERNAL) || feof (infile)) {
					endoffile = TRUE;
				} else {
					lexfatal_i (LEX_READ_ERROR, flocn, ferror (infile));
				}
#else
				if (((lexmode == LEX_SOURCE) || (lexmode == LEX_CSOURCE)) && (!feof (infile))) {
					lexfatal_i (LEX_READ_ERROR, flocn, ferror (infile));
				} else {
					endoffile = TRUE;
				}
#endif
				linebuffer_len = 0;
				DEBUG_MSG (("inputline: end of file\n"));
				/*}}} */
			} else {
				/*{{{  calculate the length of the line */
				switch (lexmode) {
				case LEX_STDLIB:
				case LEX_PREDEFS:
					break;
				default:
					fe_sourcehash (current_fe_handle, ptr, linebuffer_len);
					break;
				}
				/*}}} */
			}
			linebuffer_ptr = ptr;
		}
		/*}}} */
		eof |= endoffile;
		linecount++;

		if ((lexmode == LEX_SOURCE) || (lexmode == LEX_CSOURCE)) {
			if (pp_xline) {
				pp_xline->valdata = (void *)linecount;
			}
			totallines++;
		}
		/* totallines += (lexmode == LEX_SOURCE); */
	} while (!eof && (linebuffer_len == 0));

	linep = 0;
	co_indent = -1;
#if 0
fprintf (stderr, "readline: line is [%s]\n", linebuffer_ptr);
#endif
}
/*}}}*/
/*{{{  PRIVATE int rch () */
/* Read a character from the input file */
#ifdef __GNUC__
PRIVATE inline int rch (void)
#else
PRIVATE int rch (void)
#endif
{
	if (linep >= linebuffer_len) {
		readline ();
	}

	currentindent = linep + baseindent - (pp_relax * 2);
	co_indent++;
	return (eof ? EOF : linebuffer_ptr[linep++]);
}
/*}}}*/
/*{{{  PRIVATE int rsch() */
/* Read a character from the input file, give error message if hit EOF */
PRIVATE int rsch (void)
{
	const int c = rch ();

	if (c == EOF) {
		lexfatal (LEX_EOF, flocn);
	}
	return (c);
}
/*}}}*/
/*{{{  PRIVATE void bufinit () */
PRIVATE void bufinit (void)
{
	linecount = 0;
	sentend = FALSE;
	eof = FALSE;
	endoffile = FALSE;
	readline ();
	symbindent = 0;
	lineindent = 0;
	co_indent = 0;
	newlineflag = TRUE;
	stringcontinuation = FALSE;
	ch = rch ();
}
/*}}}*/
/*{{{  PRIVATE void suspend_file() */
/*****************************************************************************
*
*  suspend_file saves the current state of infile in the filestack.
*
*****************************************************************************/
PRIVATE void suspend_file (const char *const name, const int mode, FILE * const new_fptr)
{
	if (infile != NULL) {
		filestruct_t *fptr = memalloc (sizeof (filestruct_t));

		fptr->fileptr = infile;
		strcpy (fptr->fname, current_filename);
		fptr->fmode = lexmode;
		SetFileNum (fptr->ffileposn, currentfilenum);
		SetFileLine (fptr->ffileposn, linecount);
		fptr->fbaseindent = baseindent;
		fptr->fpp_iflevel = pp_iflevel;
		fptr->fpp_relax = pp_relax;
		fptr->fstackposition = filestackptr;
		fptr->next = filestack;
		filestack = fptr;
		filestackptr++;
		DEBUG_MSG (("suspend_file: new filestackptr is %d, name is %s, saved baseindent is %d\n", filestackptr, name, baseindent));

#if 0
fprintf (stderr, "suspend_file(): filestackptr is %d\n", filestackptr);
#endif
		if (pp_xfilestack) {
			pp_xfilestack->valdata = (void *)filestackptr;
		}
	}
	if ((mode != LEX_EXTERNAL) && (mode != LEX_DEXTERNAL)) {
		infile = new_fptr;
		strcpy (current_filename, name);
	}
	currentfilenum = fe_addfilename (current_fe_handle, ((mode == LEX_EXTERNAL) || (mode == LEX_DEXTERNAL)) ?
					 NULL : lookupword (name, strlen (name)), ((mode == LEX_SOURCE) || (mode == LEX_CSOURCE)), currentfilenum, FileLineOf (flocn));
	SetFileLine (flocn, 0);
	SetFileNum (flocn, currentfilenum);
}
/*}}}*/
/*{{{  PRIVATE void patchup() */
PRIVATE void patchup ()
{
	switch (lexmode) {
	case LEX_SC:		/* obsolete */
	case LEX_LIB:
	case LEX_STDLIB:
	case LEX_EXTERNAL:
	case LEX_DEXTERNAL:
		patchdescriptors (current_filename);
		break;
	default:
		break;
	}
}
/*}}}*/
/*{{{  PRIVATE void resume_file() */
/*{{{  comment */
/*****************************************************************************
*
*  resume_file resumes the top entry of the file stack
*
*****************************************************************************/
/*}}}*/
PRIVATE void resume_file (void)
{
	patchup ();

	/*{{{  pop the details off the stack */
	{
		filestruct_t *fptr = filestack;

		if ((lexmode != LEX_EXTERNAL) && (lexmode != LEX_DEXTERNAL)) {
			fclose (infile);
		}
		strcpy (current_filename, fptr->fname);
		infile = fptr->fileptr;
		preproc_add_define (lookupword ("FILE", 4), 4, PP_VAL_STRING, new_string (current_filename));

		/* check for unbalanced pre-processor stuff */
		if (pp_iflevel) {
			lexfatal (LEX_PP_UNBALANCED_IF, flocn);
		}

		pp_iflevel = fptr->fpp_iflevel;
		pp_relax = fptr->fpp_relax;

		/* We resume a line after where we suspended */
		lexmode = fptr->fmode;
		baseindent = fptr->fbaseindent;
		flocn = fptr->ffileposn;
		DEBUG_MSG (("resume_file: setting baseindent to %d\n", baseindent));
		{
			filestruct_t *fnext = filestack->next;

			memfree (filestack);	/* freeup the top of the stack */
			filestack = fnext;
			filestackptr--;

#if 0
fprintf (stderr, "resume_file(): filestackptr is %d\n", filestackptr);
#endif
			if (pp_xfilestack) {
				pp_xfilestack->valdata = (void *)filestackptr;
			}
		}
	}
	/*}}} */

	bufinit ();
	linecount = FileLineOf (flocn) + 1;
	currentfilenum = FileNumOf (flocn);
	return;
}
/*}}}*/
/*{{{  PRIVATE void close_file() */
/*{{{  comment */
/*****************************************************************************
*
*  close_file closes the currently open file
*
*****************************************************************************/
/*}}}*/
PRIVATE void close_file (void)
{
	DEBUG_MSG (("close_file\n"));
	patchup ();
	if (lexmode != LEX_PREDEFS) {
		fclose (infile);
	}
	infile = NULL;
	sentend = FALSE;
	eof = FALSE;
	endoffile = FALSE;
}
/*}}}*/
/*{{{  PUBLIC BOOL open_file(name, mode) */
/*{{{  comment */
/*****************************************************************************
 *
 *  open_file opens the file of name 'name', in mode 'mode', returns TRUE
 *            if opened ok, otherwise returns FALSE.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC BOOL open_file (const char *const name, const int mode, const int fileindent)
{
	FILE *fptr = NULL;
	BOOL external_ok = FALSE;

	DEBUG_MSG (("open_file: filename: %s, fileindent: %d\n", name, fileindent));
#if 0
fprintf (stderr, "lex1: open_file: [%s], fileindent = %d\n", name, fileindent);
#endif
	if ((strlen (name) >= MAX_FILENAME_LENGTH) && (mode != LEX_EXTERNAL) && (mode != LEX_DEXTERNAL)) {
		/* bug 331 29/8/90 */
		lexerr_i (LEX_FILE_LENGTH_ERROR, flocn, MAX_FILENAME_LENGTH);
	} else {
		switch (mode) {
		case LEX_SOURCE:
		case LEX_CSOURCE:
			fptr = popen_relative (name, current_fe_data->fe_pathname, NULL, "r", NULL, memalloc, memfree, NULL);
			break;
		case LEX_SC:
		case LEX_LIB:
		case LEX_STDLIB:
			fptr = open_descfile (name, mode);
			break;
		case LEX_EXTERNAL:
			external_ok = init_external (name, 0);
			break;
		case LEX_DEXTERNAL:
			external_ok = init_external (name, 1);
			break;
		}
	}
	if ((fptr != NULL) || external_ok) {
		/*{{{  set up input buffers */
		suspend_file (name, mode, fptr);
		baseindent = fileindent;
		pp_relax = 0;			/* reset these */
		pp_iflevel = 0;
		lexmode = mode;
		desc_eof = FALSE;
		preproc_add_define (lookupword ("FILE", 4), 4, PP_VAL_STRING, new_string ((char *)name));
		bufinit ();
		return (TRUE);
		/*}}} */
	} else {
		return (FALSE);
	}
}
/*}}}*/
/*{{{  PRIVATE int readtag (name) */
PRIVATE int readtag (char *const name)
{
	register int i = 0;
	register int temp_ch = ch;

	if (lexmode != LEX_STDLIB) {
		while (isalnum (temp_ch) || (temp_ch == '.') || ((lexmode == LEX_CSOURCE) ? (temp_ch == '_') : 0)) {
			if (i < (MAXNAMELENGTH - 1)) {
				/* added for bug 1020 18/10/90 */
				name[i++] = temp_ch;
			}
			temp_ch = rch ();
		}
	} else {
		/* lexmode == LEX_STDLIB */
		while (isalnum (temp_ch) || (temp_ch == '.') || (temp_ch == '@') || (temp_ch == '%') || (temp_ch == '_') || (temp_ch == '|')
		       || (temp_ch == '$')) {
			if (i < (MAXNAMELENGTH - 1)) {
				/* added for bug 1020 18/10/90 */
				name[i++] = temp_ch;
			}
			temp_ch = rch ();
		}
	}
	ch = temp_ch;
	name[i] = '\0';
	if (i >= (MAXNAMELENGTH - 1)) {
		/* added for bug 1020 18/10/90 */
		lexwarn_i (LEX_NAME_TRUNCATED, flocn, MAXNAMELENGTH - 1);
	}
	return i;
}
/*}}}*/
/*{{{  PRIVATE void readdecimal () */
/* Read decimal digits from the input file and place them in
 * literalv from position literalp onwards.
 * Update literalp. */
PRIVATE void readdecimal (void)
{
	int temp_ch = ch;

	if (isdigit (temp_ch)) {
		while (isdigit (temp_ch)) {
			if (literalp < (MAXSTRING_SIZE - 5)) {
				/* bug 1020 18/10/90 */
				literalv[literalp++] = temp_ch;
			}
			temp_ch = rch ();
		}
		ch = temp_ch;
		/* we subtract 5 here to allow for sign characters, exponents, etc */
		if (literalp >= (MAXSTRING_SIZE - 5)) {
			lexwarn_i (LEX_NUMBER_TRUNCATED, flocn, MAXSTRING_SIZE - 5);
		}
	} else {
		lexerr_i (LEX_NUMBER_ERROR, flocn, temp_ch);
		literalv[literalp++] = '0';
	}
}
/*}}}*/
/*{{{  PRIVATE void rexponent () */
/* Read an exponent from the input file and place it in literalv
from literalp onwards */
PRIVATE void rexponent (void)
{
	literalv[literalp++] = 'E';
	ch = rch ();
	if ((ch == '+') || (ch == '-')) {
		literalv[literalp++] = ch;
		ch = rch ();
	} else {
		lexerr (LEX_SIGN_ERROR, flocn);
		/*lexerror (LEX_NUMBER_ERROR, flocn, 0); */
		literalv[literalp++] = '+';
	}
	readdecimal ();
}
/*}}}*/
/*{{{  PRIVATE int hexval () */
/* Return the hexadecimal value of the ASCII hex digit in ch */
PRIVATE int hexval (void)
{
	if (isdigit (ch)) {
		return (ch - '0');
	}
	if ((ch >= 'A') && (ch <= 'F')) {
		return (ch - 'A' + 10);
	}
	lexerr_i (LEX_E_HEXDIGIT, flocn, ch);
	return (0);
}
/*}}}*/
/*{{{  PRIVATE int rescapech () */
PRIVATE int rescapech (const BOOL inside_string)
/* This routine modified for bug 1399 25/9/91 - and merged string and character
processing */
{
	char comment_char;

	if (lexmode == LEX_CSOURCE) {
		comment_char = '/';
	} else {
		comment_char = '-';
	}
	switch (ch) {
		/*{{{  '*' - special character (and '\\' in LEX_CSOURCE) */
	case '\\':
		if (lexmode != LEX_CSOURCE) {
			goto do_default_case;
		}
		/* fall through */
	case '*':
		ch = rsch ();
		/*{{{  skip over carriage-returns */
		while (ch == '\r' && inside_string) {
			ch = rsch ();	/* bug TS/2013 05/01/93 */
		}
		/*}}} */
		if (inside_string) {
			BOOL repeating;

			switch (ch) {
				/*{{{  '\n' */
			case '\n':
				ch = rsch ();
				do {
					repeating = FALSE;
					while ((ch == ' ') || (ch == '\n')) {
						/* bug 1103 10/1/91 */
						ch = rsch ();
					}
					SetFileLine (flocn, linecount);
					if (ch == comment_char) {
						/* check for comments */
						/*{{{  check indent of comment  */
						symbindent = currentindent;
						/*lineindent = symbindent; */
						if (symbindent < string_indent) {
							lexerr (LEX_BAD_INDENT, flocn);
						}
						lineindent = string_indent;	/* bug 1251 25/9/91 */

						/*}}} */
						ch = rsch ();
						if (ch != comment_char) {
							lexerr (LEX_BAD_STRING_CONTINUATION, flocn);
						}
						while (ch != '\n') {
							ch = rsch ();
						}
						repeating = TRUE;
					}
				} while (repeating);
				if (ch != '*') {
					lexerr (LEX_BAD_STRING_CONTINUATION, flocn);
				}
				/*{{{  check indent of string continuation */
				symbindent = currentindent;
				/*lineindent = symbindent; */
				if (symbindent < string_indent) {
					lexerr (LEX_BAD_INDENT, flocn);
				}
				lineindent = string_indent;	/* bug 1251 25/9/91 */

				/*}}} */
				ch = rsch ();
				return (rescapech (inside_string));
				/*}}} */
				/*{{{  l L */
			case 'l':
			case 'L':
				if (!stringcontinuation && (literalp == 0)) {
					insertlength = TRUE;
					return (' ');
				} else {
					lexerr_i (LEX_BAD_STRING_LENGTH_POSN, flocn, ch);
					return ('?');
				}
				/*}}} */
			default:
				break;
			}
		}
		if (lexmode == LEX_CSOURCE) {
			switch (ch) {
			case 'n':
				return '\n';
			case 'r':
				return '\r';
			case '\'':
				return '\'';
			case 't':
				return '\t';
			case '"':
				return '"';
			case '\\':
				return '\\';
			default:
				lexerr_i (LEX_ILLEGAL_STRING_CHAR, flocn, ch);
				return (ch);
			}
		} else {
			switch (ch) {
			case 'c':
			case 'C':
				return ('\r');
			case 'n':
			case 'N':
				return ('\n');
			case 't':
			case 'T':
				return ('\t');
			case 's':
			case 'S':
				return (' ');
			case '\'':
				return ('\'');
			case '"':
				return ('"');
			case '*':
				return ('*');
			case '#':
				{
					int h;
	
					ch = rsch ();
					h = hexval ();
					ch = rsch ();
					return ((h << 4) | hexval ());
				}
			default:
				lexerr_i (LEX_ILLEGAL_STRING_CHAR, flocn, ch);
				return (ch);
			}
		}
		break;
		/*}}} */
		/*{{{  ''' or '"' */
	case '\'':
		if (inside_string) {
			/* technically not a problem, generate a warning for it though */
			lexwarn (LEX_ESCAPE_SQUOTE, flocn);
			return ch;
		} else {
			/* processing byte literal */
			lexwarn (LEX_ESCAPE_BLSQUOTE, flocn);
			return ch;
		}
		break;
	case '"':
		if (inside_string) {
			return CLOSE_QUOTE;
		} else {
			lexwarn (LEX_ESCAPE_BLDQUOTE, flocn);
			return ch;
		}
		break;
		/*}}} */
		/*{{{  default */
	default:
	do_default_case:
		if (ch >= MINLEGALCH && ch <= MAXLEGALCH) {
			return ch;
		}
		break;
		/*}}} */
	}

	if (inside_string) {
		lexerr_i (LEX_STRING_ERROR, flocn, ch);
		return CLOSE_QUOTE;
	} else {
		lexerr (LEX_CHAR_ERROR, flocn);
		return (ch);
	}
}
/*}}}*/
/*{{{  PRIVATE int readnumber () */
/* Read a number from the input file.
 * The number may be an integer or a real.
 * A string representing the number is placed in literalv.
 * Return S_UREALLIT or S_UINTLIT */
PRIVATE int readnumber (void)
{
	literalp = 0;
	readdecimal ();
	if (ch == '.') {
		/*{{{  read rest of decimal number */
		literalv[literalp++] = '.';
		ch = rch ();
		readdecimal ();
		if (ch == 'E') {
			rexponent ();
		}
		literalv[literalp] = '\0';
		return (S_UREALLIT);
		/*}}} */
	} else if ((ch == 'x') && (lexmode == LEX_CSOURCE)) {
		/*{{{  read rest of hex number */
		literalv[literalp++] = 'x';
		ch = rch ();
		readchex ();
		literalv[literalp] = '\0';
		return (S_UINTLIT);
		/*}}}  */
	} else {
		/*{{{  return integer */
		literalv[literalp] = '\0';
		return (S_UINTLIT);
		/*}}} */
	}
}
/*}}}*/
/*{{{  PRIVATE int readhex () */
PRIVATE int readhex (void)
{
	int temp_ch = ch;

	while (isdigit (temp_ch) || ((temp_ch >= 'A') && (temp_ch <= 'F'))) {
		if (literalp < (MAXSTRING_SIZE - 1)) {
			/* bug 1020 18/10/90 */
			literalv[literalp++] = temp_ch;
		}
		temp_ch = rch ();
	}
	ch = temp_ch;
	if (literalp == 1) {
		lexerr_i (LEX_E_HEXDIGIT, flocn, ch);
		/*lexerror(LEX_NUMBER_ERROR, flocn, 0); */
		return (S_ILLEGALSYMB);
	} else if (literalp >= (MAXSTRING_SIZE - 1)) {
		lexwarn_i (LEX_NUMBER_TRUNCATED, flocn, MAXSTRING_SIZE);
	}
	literalv[literalp] = '\0';
	return (S_UINTLIT);
}
/*}}}*/
/*{{{  PRIVATE int readchex () */
PRIVATE int readchex (void)
{
	int temp_ch = ch;

	while (isdigit (temp_ch) || ((temp_ch >= 'a') && (temp_ch <= 'f')) || ((temp_ch >= 'A') && (temp_ch <= 'F'))) {
		if ((temp_ch >= 'a') && (temp_ch <= 'f')) {
			temp_ch = ((temp_ch - 'a') + 'A');
		}
		if (literalp < (MAXSTRING_SIZE - 1)) {
			literalv[literalp++] = temp_ch;
		}
		temp_ch = rch ();
	}
	ch = temp_ch;
	if (literalp == 1) {
		lexerr_i (LEX_E_HEXDIGIT, flocn, ch);
		return S_ILLEGALSYMB;
	} else if (literalp >= (MAXSTRING_SIZE - 1)) {
		lexwarn_i (LEX_NUMBER_TRUNCATED, flocn, MAXSTRING_SIZE);
	}
	literalv[literalp] = '\0';
	return S_UINTLIT;
}
/*}}}  */
/*{{{  PRIVATE int rhashword (matchstring, s) */
/* Read characters from the input stream and ensure that they match successive
characters of the string 'matchstring'.  If the whole of matchstring is
matched return 's', otherwise return S_ILLEGALSYMB. */
PRIVATE int rhashword (const char *matchstring, const int s)
{
	int temp_ch = rch ();

	while (*matchstring != '\0' && temp_ch == *matchstring) {
		temp_ch = rch ();
		matchstring++;
	}
	ch = temp_ch;
	if (*matchstring == '\0') {
		return s;
	} else {
		lexerr (LEX_HASHWORD_ERROR, flocn);
		return S_ILLEGALSYMB;
	}
}
/*}}}*/
/*{{{  PRIVATE int readbyteliteral () */
/* Read a byte literal string from the input file and place it in literalv.
 * Return S_UBYTELIT */
PRIVATE int readbyteliteral (void)
{
	ch = rescapech (FALSE);
	literalv[0] = '\'';
	literalv[1] = ch;
	literalv[2] = '\'';
	literalv[3] = '\0';
	literalp = 3;
	ch = rsch ();
	if (ch == '\'') {
		ch = rch ();
	} else {
		symbindent = currentindent;
		lexerr_i (LEX_E_QUOTE, flocn, ch);
		return S_ILLEGALSYMB;
	}
	return (S_UBYTELIT);
}
/*}}}*/
/*{{{  PRIVATE int readstringliteral () */
PRIVATE int readstringliteral (void)
{
	insertlength = FALSE;
	literalp = 0;
	if (!stringcontinuation) {
		/* ch has already been escaped */
		string_indent = lineindent;	/* bug 1251 25/9/91 *//* moved up one line 27/11/91 for bug TS/1493 */
		ch = rescapech (TRUE);
		DEBUG_MSG (("readstringliteral: setting string_indent to %d ", string_indent));
	}
	stringcontinuation = FALSE;

	while ((ch != CLOSE_QUOTE) && (literalp < MAXSTRING_SIZE)) {
		/*{{{  read a character into literalp */
		literalv[literalp++] = ch;
		ch = rsch ();
		ch = rescapech (TRUE);
		/*}}} */
	}
	literalv[literalp] = '\0';
	stringcontinuation = (ch != CLOSE_QUOTE);
	if (insertlength) {
		/*{{{  insert the length if the string is short enough */
		if (!stringcontinuation) {
			literalv[0] = literalp - 1;
		} else {
			lexerr (LEX_BAD_STRING_LENGTH, flocn);
			literalv[0] = '?';
		}
		/*}}} */
	}

	if (stringcontinuation) {
		return (S_STRINGCONT);
	} else {
		/*{{{  read another character, return S_STRING */
		ch = rch ();
		return (S_STRING);
		/*}}} */
	}
}
/*}}}*/
/*{{{  PRIVATE char *new_string (char *str)*/
/* performs a string copy, using memalloc allocation */
PRIVATE char *new_string (char *str)
{
	const int i = strlen (str);
	char *tmp = (char *)memalloc (i + 1);

	memcpy (tmp, str, i);
	tmp[i] = '\0';

	return tmp;
}
/*}}}*/
/*{{{  PRIVATE int preproc_relax (void)*/
PRIVATE int preproc_relax (void)
{
	if (!pp_iflevel) {
		lexfatal (LEX_PP_LOOSE_RELAX, flocn);
		return -1;
	}
	if (pp_ifstack->relaxed) {
		lexfatal (LEX_PP_OVER_RELAX, flocn);
		return -1;
	}
	pp_ifstack->relaxed = TRUE;
	pp_relax++;
	return 0;
}
/*}}}*/
/*{{{  PRIVATE int preproc_unrelax (void)*/
PRIVATE int preproc_unrelax (void)
{
	if (!pp_iflevel) {
		lexfatal (LEX_PP_RELAX_ERROR, flocn);
		return -1;
	}
	if (pp_ifstack->relaxed) {
		pp_relax--;
		pp_ifstack->relaxed = FALSE;
		return 1;
	}
	return 0;
}
/*}}}*/
/*{{{  PRIVATE void preproc_dump_sdefine (pp_dlist_t *item, FILE *fptr)*/
/* dumps a single define */
PRIVATE void preproc_dump_sdefine (pp_dlist_t *item, FILE *fptr)
{
	if (!item || !fptr) {
		return;
	}
	switch (item->valtype) {
	case PP_VAL_NONE:
		fprintf (fptr, "#DEFINE %s\n", WNameOf (item->name));
		break;
	case PP_VAL_INT:
		fprintf (fptr, "#DEFINE %s %d\n", WNameOf (item->name), (int)(item->valdata));
		break;
	case PP_VAL_STRING:
		fprintf (fptr, "#DEFINE %s \"%s\"\n", WNameOf (item->name), (char *)(item->valdata));
		break;
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void preproc_dump_defines (FILE *fptr)*/
/* dumps the defines (for info/debugging) */
PUBLIC void preproc_dump_defines (FILE *fptr)
{
	pp_dlist_t *dltmp;

	if (pp_xfile) {
		preproc_dump_sdefine (pp_xfile, fptr);
	}
	if (pp_xline) {
		preproc_dump_sdefine (pp_xline, fptr);
	}
	if (pp_xfilestack) {
		preproc_dump_sdefine (pp_xfilestack, fptr);
	}
	for (dltmp = pp_dlist; dltmp; dltmp = dltmp->next) {
		preproc_dump_sdefine (dltmp, fptr);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void preproc_add_define (wordnode *name, int len, int vtype, void *vptr)*/
/*
 *	does the actual adding of a name/value to the defined list -- can be
 *	called as a result of command-line options
 */
PRIVATE void preproc_add_define (wordnode *name, int len, int vtype, void *vptr)
{
	pp_dlist_t *dltmp;
	int ord_flag = 0;

	if (pp_xfile && (pp_xfile->name == name)) {
		dltmp = pp_xfile;
		ord_flag = 1;
	} else if (pp_xline && (pp_xline->name == name)) {
		dltmp = pp_xline;
		ord_flag = 2;
	} else if (pp_xfilestack && (pp_xfilestack->name == name)) {
		dltmp = pp_xfilestack;
		ord_flag = 3;
	} else {
		/* just make sure it's not here already */
		for (dltmp = pp_dlist; dltmp; dltmp = dltmp->next) {
			if (dltmp->name == name) {
				break;
			}
		}
	}
	if (!ord_flag) {
		/* ordinary define */
		if (!dltmp) {
			dltmp = (pp_dlist_t *)memalloc (sizeof (pp_dlist_t));
			dltmp->name = name;
			dltmp->namelen = len;
			dltmp->next = pp_dlist;
			dltmp->valtype = PP_VAL_NONE;
			dltmp->valdata = NULL;
			pp_dlist = dltmp;
		}
	}
	if (dltmp) {
		if (dltmp->valtype == PP_VAL_STRING) {
			/* free old string */
			memfree ((void *)dltmp->valdata);
		}
		dltmp->valtype = vtype;
		dltmp->valdata = vptr;
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE int preproc_get_define (wordnode *name)*/
/*
 *	extracts the value of a define (through the ## operator)
 *	returns the type (S_STRING, S_UINTLIT or S_ILLEGALSYMB)
 */
PRIVATE int preproc_get_define (wordnode *name)
{
	pp_dlist_t *dltmp;

	if (pp_xfile && (pp_xfile->name == name)) {
		dltmp = pp_xfile;
	} else if (pp_xline && (pp_xline->name == name)) {
		dltmp = pp_xline;
	} else if (pp_xfilestack && (pp_xfilestack->name == name)) {
		dltmp = pp_xfilestack;
	} else {
		for (dltmp = pp_dlist; dltmp && (dltmp->name != name); dltmp = dltmp->next);
	}
	if (!dltmp) {
		return S_ILLEGALSYMB;
	}
	switch (dltmp->valtype) {
	case PP_VAL_INT:
		literalp = sprintf (literalv, "%d", (int)(dltmp->valdata));
		return S_UINTLIT;
	case PP_VAL_STRING:
		literalp = sprintf (literalv, "%s", (char *)(dltmp->valdata));
		return S_STRING;
	}
	literalp = 0;
	return S_SKIP;
}
/*}}}*/
/*{{{  PUBLIC BOOL preproc_cmdline_define (const char *arg)*/
PUBLIC BOOL preproc_cmdline_define (const char *arg)
{
	pp_cmdline_defines[pp_cmdline_ndef] = (char *)memalloc (strlen (arg) + 1);
	strcpy (pp_cmdline_defines[pp_cmdline_ndef], arg);
	pp_cmdline_ndef++;
	if (pp_cmdline_ndef >= MAX_CMDLINE_DEFINES) {
		return FALSE;
	}
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE BOOL pending_cmdline_define (char *arg)*/
PRIVATE BOOL pending_cmdline_define (char *arg)
{
	char name[MAXNAMELENGTH];
	char val[MAXNAMELENGTH];
	int namelen, vallen;
	char *ch = arg;
	wordnode *lexname;
	int valtype = PP_VAL_NONE;
	int valint;

	namelen = 0;
	while ((*ch != '=') && (*ch != '\0')) {
		name[namelen++] = *ch;
		ch++;
		if (namelen >= MAXNAMELENGTH) {
			return FALSE;
		}
	}
	name[namelen] = '\0';
	lexname = lookupword (name, namelen);
	if (!lexname) {
		/* something went wrong */
		return FALSE;
	}
	vallen = 0;
	valint = 0;
	if (*ch == '=') {
		ch++;
		while (*ch != '\0') {
			val[vallen++] = *ch;
			if ((*ch >= '0') && (*ch <= '9')) {
				if (valtype == PP_VAL_NONE) {
					valtype = PP_VAL_INT;
				}
				if (valtype == PP_VAL_INT) {
					valint *= 10;
					valint += (int)((*ch) - '0');
				}
			} else {
				valtype = PP_VAL_STRING;
			}
			ch++;
			if (vallen >= MAXNAMELENGTH) {
				return FALSE;
			}
		}
		val[vallen] = '\0';
	}
	switch (valtype) {
	case PP_VAL_NONE:
		preproc_add_define (lexname, namelen, PP_VAL_NONE, NULL);
		break;
	case PP_VAL_INT:
		preproc_add_define (lexname, namelen, PP_VAL_INT, (void *)valint);
		break;
	case PP_VAL_STRING:
		ch = (char *)memalloc (vallen + 1);
		memcpy (ch, val, vallen);
		ch[vallen] = '\0';
		preproc_add_define (lexname, namelen, PP_VAL_STRING, (void *)ch);
		break;
	}
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE void preproc_process_pending (void)*/
PRIVATE void preproc_process_pending (void)
{
	int i;

	for (i=0; i<pp_cmdline_ndef; i++) {
		if (pending_cmdline_define (pp_cmdline_defines[i]) == FALSE) {
			lexfatal_s (LEX_PP_BADCMDLINEDEF, NOPOSN, pp_cmdline_defines[i]);
		}
		memfree ((void *)(pp_cmdline_defines[i]));
	}
	pp_cmdline_ndef = 0;
	return;
}
/*}}}*/
/*{{{  PRIVATE BOOL preproc_is_defined (wordnode *name)*/
/*
 *	tests to see if something is defined or not
 */
PRIVATE BOOL preproc_is_defined (wordnode *name)
{
	pp_dlist_t *dltmp;

	if (pp_xfile && (pp_xfile->name == name)) {
		return TRUE;
	} else if (pp_xline && (pp_xline->name == name)) {
		return TRUE;
	} else if (pp_xfilestack && (pp_xfilestack->name == name)) {
		return TRUE;
	}

	for (dltmp = pp_dlist; dltmp; dltmp = dltmp->next) {
		if (dltmp->name == name) {
			return TRUE;
		}
	}
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE pp_dlist_t *preproc_find_define (wordnode *name)*/
PRIVATE pp_dlist_t *preproc_find_define (wordnode *name)
{
	pp_dlist_t *dltmp;

	if (pp_xfile && (pp_xfile->name == name)) {
		return pp_xfile;
	} else if (pp_xline && (pp_xline->name == name)) {
		return pp_xline;
	} else if (pp_xfilestack && (pp_xfilestack->name == name)) {
		return pp_xfilestack;
	}

	for (dltmp = pp_dlist; dltmp; dltmp = dltmp->next) {
		if (dltmp->name == name) {
			return dltmp;
		}
	}
	return NULL;
}
/*}}}*/
/*{{{  PRIVATE BOOL preproc_compare (int op, pp_dlist_t *left, pp_dlist_t *right)*/
PRIVATE BOOL preproc_compare (int op, pp_dlist_t *left, pp_dlist_t *right)
{
	if (!left || !right) {
		return FALSE;
	}
	if (left->valtype != right->valtype) {
		lexfatal (LEX_PP_BAD_COMPARE, flocn);
		return FALSE;
	}
	if ((left->valtype == PP_VAL_NONE) || (right->valtype == PP_VAL_NONE)) {
		lexfatal (LEX_PP_BAD_COMPARE, flocn);
		return FALSE;
	}
	switch (left->valtype) {
	case PP_VAL_INT:
		{
			const int vleft = (int)left->valdata;
			const int vright = (int)right->valdata;

			switch (op) {
			case PP_OP_EQ:	return (vleft == vright);
			case PP_OP_NE:	return (vleft != vright);
			case PP_OP_GT:	return (vleft > vright);
			case PP_OP_GE:	return (vleft >= vright);
			case PP_OP_LT:	return (vleft < vright);
			case PP_OP_LE:	return (vleft <= vright);
			}
			lexfatal (LEX_PP_BAD_COMPARE, flocn);
			return FALSE;
		}
		break;
	case PP_VAL_STRING:
		{
			const char *sleft = (char *)left->valdata;
			const char *sright = (char *)right->valdata;

			switch (op) {
			case PP_OP_EQ:	return !strcmp (sleft, sright);
			case PP_OP_NE:	return strcmp (sleft, sright);
			}
			lexfatal (LEX_PP_BAD_COMPARE, flocn);
			return FALSE;
		}
	}
	lexfatal (LEX_PP_BAD_COMPARE, flocn);
	return FALSE;
}
/*}}}*/
/*{{{  PRIVATE BOOL preproc_parse (int err, BOOL *valid_val, BOOL allow_dyadic, const BOOL error)*/
/*
 *	parses pre-processor expression and returns it, leaving valid_val TRUE.
 *	`allow_dyadic' indicates whether we're permitting dyadic operators, not true on tail-recursive call
 *	Returns the value of the evaluated expression (gets called recursively)
 *	On error, lexfatal(err)'s and leaves valid_val FALSE
 */
PRIVATE BOOL preproc_parse (int err, BOOL *valid_val, BOOL allow_dyadic, const BOOL error)
{
	BOOL pval = FALSE;
	char name[MAXNAMELENGTH];
	int len;
	wordnode *lexname;
	BOOL lval;
	pp_dlist_t *pvar = NULL;
	SOURCEPOSN pvar_locn = NOPOSN;

	/* skip any leading whitespace */
	while ((ch == ' ') || (ch == '\t')) {
		ch = rch ();
	}
	/* parse something */
	symbindent = currentindent;			/* help errors get in the right place */
	if ((ch >= 'A') && (ch <= 'Z')) {
		pvar_locn = flocn;
		len = readtag (name);
		lexname = lookupword (name, len);
		if (lexname == pp_defined) {
			/* skip whitespace */
			while ((ch == ' ') || (ch == '\t')) {
				ch = rch ();
			}
			if (ch != '(') {
				lexfatal (err, flocn);
				*valid_val = FALSE;
				return FALSE;
			}
			ch = rch ();
			/* pickup name into name/len/lexname */
			len = readtag (name);
			lexname = lookupword (name, len);
			if (ch != ')') {
				lexfatal (err, flocn);
				*valid_val = FALSE;
				return FALSE;
			}
			ch = rch ();
			pval = preproc_is_defined (lexname);
		} else if (lexname == pp_true) {
			pval = TRUE;
		} else if (lexname == pp_false) {
			pval = FALSE;
		} else if (lexname == pp_not) {
			pval = preproc_parse (err, valid_val, FALSE, error);
			if (!(*valid_val)) {
				return FALSE;
			}
			pval = !pval;
			allow_dyadic = FALSE;
		} else {
			/* might be a name, if about to compare in with a dyadic op. */
			pvar = preproc_find_define (lexname);

			if (!allow_dyadic || (allow_dyadic && !pvar)) {
				*valid_val = FALSE;
				if (error) {
					lexfatal (err, flocn);
					return FALSE;
				}
			}
		}
	} else if ((ch >= 'a') && (ch <= 'z') && allow_dyadic) {
		/* might specifically be talking about a name for use with a dyadic op */
		pvar_locn = flocn;
		len = readtag (name);
		lexname = lookupword (name, len);

		pvar = preproc_find_define (lexname);
		if (!pvar) {
			*valid_val = FALSE;
			if (error) {
				lexfatal (err, flocn);
				return FALSE;
			}
			pval = FALSE;
		}
	} else if (ch == '(') {
		ch = rch ();
		/* parse sub-expression */
		pval = preproc_parse (err, valid_val, TRUE, error);
		if (error && !(*valid_val)) {
			return FALSE;
		}
		if (ch != ')') {
			symbindent = currentindent;
			lexfatal (LEX_PP_EXPECTED_RPAREN, flocn);
			*valid_val = FALSE;
			return FALSE;
		}
		ch = rch ();
	} else {
		lexfatal (err, flocn);
		*valid_val = FALSE;
		return FALSE;
	}
	/* skip over any trailing whitespace */
	while ((ch == ' ') || (ch == '\t')) {
		ch = rch ();
	}
	/* oki, might have a dyadic operator, if allowed here */
	if (allow_dyadic && ((ch == 'A') || (ch == 'O'))) {
		symbindent = currentindent;
		len = readtag (name);
		lexname = lookupword (name, len);
		if (lexname == pp_and) {
			/* don't generate errors if the LHS is FALSE -- just parse the RHS, we know the outcome.. */
			lval = preproc_parse (err, valid_val, FALSE, pval ? error : FALSE);
			if (pval && !(*valid_val)) {
				return FALSE;
			} else if (pval) {
				pval = (pval && lval);
			}
			/* otherwise leave as FALSE */
		} else if (lexname == pp_or) {
			/* don't generate errors if the LHS is TRUE -- just parse the RHS, we know the outcome.. */
			lval = preproc_parse (err, valid_val, FALSE, pval ? FALSE : error);
			if (!pval && !(*valid_val)) {
				return FALSE;
			} else if (!pval) {
				pval = lval;
			}
			/* otherwise leave as TRUE */
		} else {
			lexfatal (err, flocn);
			*valid_val = FALSE;
			return FALSE;
		}
		/* skip over any trailing whitespace */
		while ((ch == ' ') || (ch == '\t')) {
			ch = rch ();
		}
	} else if (allow_dyadic && ((ch == '=') || (ch == '<') || (ch == '>'))) {
		int op = PP_OP_NONE;
		pp_dlist_t tmpvar;
		pp_dlist_t *qvar = NULL;

		if (ch == '=') {
			ch = rch ();
			op = PP_OP_EQ;
		} else if (ch == '<') {
			ch = rch ();
			if (ch == '>') {
				ch = rch();
				op = PP_OP_NE;
			} else if (ch == '=') {
				ch = rch();
				op = PP_OP_LE;
			} else {
				op = PP_OP_LT;
			}
		} else if (ch == '>') {
			ch = rch ();
			if (ch == '=') {
				ch = rch ();
				op = PP_OP_GE;
			} else {
				op = PP_OP_GT;
			}
		}
		/* ch must be whitespace */
		if ((ch != ' ') && (ch != '\t')) {
			lexfatal (err, flocn);
			*valid_val = FALSE;
			return FALSE;
		}
		/* which means pvar must be defined..! */
		if (!pvar) {
			lexfatal (err, pvar_locn);
			*valid_val = FALSE;
			return FALSE;
		}
		while ((ch == ' ') || (ch == '\t')) {
			ch = rch ();
		}
		/* right, extract the RHS -- allow either name, number of string */
		tmpvar.name = lookupword ("$PPTEMP", 7);
		tmpvar.namelen = 7;
		tmpvar.valtype = PP_VAL_NONE;
		tmpvar.valdata = NULL;
		tmpvar.next = NULL;
		if ((ch >= '0') && (ch <= '9')) {
			int val = 0;

			/* integer */
			while ((ch >= '0') && (ch <= '9')) {
				val *= 10;
				val += (ch - '0');
				ch = rch ();
			}
			tmpvar.valdata = (void *)val;
			tmpvar.valtype = PP_VAL_INT;
		} else if (ch == '\"') {
			static char tstr[128];
			int tlen = 0;

			/* string */
			ch = rch ();
			while ((ch != '\"') && (ch != '\n') && (ch != EOF)) {
				tstr[tlen++] = ch;
				if (tlen >= 127) {
					lexfatal (err, flocn);
					*valid_val = FALSE;
					return FALSE;
				}
				ch = rch ();
			}
			if (ch != '\"') {
				lexfatal (err, flocn);
				*valid_val = FALSE;
				return FALSE;
			} else {
				ch = rch ();
			}
			tstr[tlen] = '\0';
			tmpvar.valdata = (void *)memalloc (tlen + 1);
			strncpy ((char *)tmpvar.valdata, tstr, tlen);
			tmpvar.valtype = PP_VAL_STRING;
		} else if (((ch >= 'a') && (ch <= 'z')) || ((ch >= 'A') && (ch <= 'Z'))) {
			len = readtag (name);
			lexname = lookupword (name, len);

			qvar = preproc_find_define (lexname);
			if (!qvar) {
				lexfatal (err, flocn);
				*valid_val = FALSE;
				return FALSE;
			}
		} else {
			lexfatal (err, flocn);
			*valid_val = FALSE;
			return FALSE;
		}
		/* rightho, have operator in "op", pvar is LHS and (qvar xor &tmpvar) is RHS */
		pval = preproc_compare (op, pvar, qvar ? qvar : &tmpvar);
		pvar = NULL;
		if ((tmpvar.valtype == PP_VAL_STRING) && tmpvar.valdata) {
			memfree (tmpvar.valdata);
			tmpvar.valdata = NULL;
			tmpvar.valtype =PP_VAL_NONE;
		}
	}
	if (pvar) {
		lexfatal (err, flocn);
		*valid_val = FALSE;
		return FALSE;
	}
	*valid_val = TRUE;
	return pval;
}
/*}}}*/
/*{{{  PRIVATE void preproc_define (void)*/
/*
 *	called to process a #DEFINE directive
 */
PRIVATE void preproc_define (void)
{
	char name[MAXNAMELENGTH];
	int len;
	wordnode *lexname;
	int qstr = 0;		/* quoted string */

	if ((ch != ' ') && (ch != '\t')) {
		/* malformed #DEFINE */
		lexfatal (LEX_PP_BAD_DEFINE, flocn);
		return;
	}
	while ((ch == ' ') || (ch == '\t')) {
		ch = rsch ();
	}
	

	if (((ch >= 'a') && (ch <= 'z')) || ((ch >= 'A') && (ch <= 'Z'))) {
		char val[MAXNAMELENGTH];
		int vallen, valtype;

		/* oki, scoop up name */
		len = readtag (name);
		lexname = lookupword (name, len);

		/* skip over any whitespace */
		while ((ch == ' ') || (ch == '\t')) {
			ch = rch ();
		}

		vallen = 0;
		valtype = PP_VAL_NONE;
		if (ch == '\"') {
			qstr = 1;
			ch = rsch ();
			ch = rescapech (TRUE);
		}
		while ((ch != '\n') && (ch != EOF) && (qstr ? (ch != CLOSE_QUOTE) : TRUE)) {
			val[vallen++] = ch;
			if (vallen >= MAXNAMELENGTH) {
				lexfatal (LEX_PP_BAD_DEFINE, flocn);
			}
			if ((ch >= '0') && (ch <= '9')) {
				if (valtype == PP_VAL_NONE) {
					valtype = PP_VAL_INT;
				}
			} else {
				valtype = PP_VAL_STRING;
			}
			ch = rsch ();
			if (qstr) {
				ch = rescapech (qstr);
			}
		}
		if (qstr && (ch == CLOSE_QUOTE)) {
			ch = rsch ();
			qstr = 0;
		} else if (qstr) {
			lexfatal (LEX_PP_BAD_DEFINE, flocn);
		}

		switch (valtype) {
		case PP_VAL_NONE:
			preproc_add_define (lexname, len, PP_VAL_NONE, NULL);
			break;
		case PP_VAL_INT:
			{
				int i, v;

				for (i = v = 0; i < vallen; i++) {
					v *= 10;
					v += (int)(val[i] - '0');
				}
				preproc_add_define (lexname, len, PP_VAL_INT, (void *)v);
			}
			break;
		case PP_VAL_STRING:
			{
				char *str = (char *)memalloc (vallen + 1);

				memcpy (str, val, vallen);
				str[vallen] = '\0';
				preproc_add_define (lexname, len, PP_VAL_STRING, (void *)str);
			}
			break;
		}
	} else {
		symbindent = currentindent;
		lexfatal (LEX_PP_ILLEGAL_NAME, flocn);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void preproc_undef (void)*/
PRIVATE void preproc_undef (void)
{
	char name[MAXNAMELENGTH];
	int len;
	wordnode *lexname;
	pp_dlist_t *dltmp, *walk;

	if ((ch != ' ') && (ch != '\t')) {
		/* malformed #UNDEF */
		lexfatal (LEX_PP_BAD_UNDEF, flocn);
		return;
	}
	while ((ch == ' ') || (ch == '\t')) {
		ch = rch ();
	}
	if (((ch >= 'a') && (ch <= 'z')) || ((ch >= 'A') && (ch <= 'Z'))) {
		/* oki, scoop up name */
		len = readtag (name);
		lexname = lookupword (name, len);

		/* search for it */
		walk = NULL;
		dltmp = pp_dlist;
		while (dltmp && (dltmp->name != lexname)) {
			walk = dltmp;
			dltmp = dltmp->next;
		}
		if (!dltmp) {
			/* didn't find it -- SKIP */
		} else if (!walk) {
			/* removing from front of list */
			pp_dlist = dltmp->next;
			memfree ((void *)dltmp);
		} else {
			/* removing from middle/end of list */
			walk->next = dltmp->next;
			memfree ((void *)dltmp);
		}
		/* skip to end-of-line */
		while ((ch != '\n') && (ch != EOF)) {
			ch = rch ();
		}
	} else {
		symbindent = currentindent;
		lexfatal (LEX_PP_ILLEGAL_NAME, flocn);
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void preproc_if (void)*/
PRIVATE void preproc_if (void)
{
	pp_ifstack_t *istack;
	BOOL val, valid_val;

	if ((ch != ' ') && (ch != '\t')) {
		/* malformed #IF */
		lexfatal (LEX_PP_BAD_IF, flocn);
		return;
	}
	while ((ch == ' ') || (ch == '\t')) {
		ch = rch ();
	}

	istack = (pp_ifstack_t *)memalloc (sizeof (pp_ifstack_t));
	istack->indent = lineindent;
	istack->locn = flocn;
	istack->hadtrue = FALSE;
	istack->skipinput = FALSE;
	istack->relaxed = FALSE;
	istack->next = NULL;

	/* oki, attempt to parse the pre-processor directive */
	val = preproc_parse (LEX_PP_BAD_IF, &valid_val, TRUE, TRUE);
	if (valid_val == TRUE) {
		if (val) {
			istack->hadtrue = TRUE;
			istack->skipinput = FALSE;
		} else {
			istack->skipinput = TRUE;
		}
		/* oki, put this in the IF stack */
		if (pp_ifstack) {
			if (istack->indent < pp_ifstack->indent) {
				lexwarn (LEX_PP_NESTED_IF_OUTDENTS, flocn);
			}
		}
		istack->next = pp_ifstack;
		pp_ifstack = istack;
		pp_iflevel++;
	} else {
		lexfatal (LEX_PP_BAD_IF, flocn);
		return;
	}

	/* anything left on the line is garbage (except comments, duh..) */
	if (ch == '-') {
		ch = rch ();
		if (ch == '-') {
			/* comment -- skip to end-of-line */
			while ((ch != '\n') && (ch != EOF)) {
				ch = rch ();
			}
		}
	}
	if ((ch != '\n') && (ch != EOF)) {
#if 0
fprintf (stderr, "lex1.c: preproc_if(): found garbage, ch = %d (%c)\n", (int)ch, ch);
#endif
		symbindent = currentindent;
		lexerr (LEX_PP_GARBAGE, flocn);

		/* skip to end-of-line */
		while ((ch != '\n') && (ch != EOF)) {
			ch = rch ();
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void preproc_endif (void)*/
PRIVATE void preproc_endif (void)
{
	pp_ifstack_t *istack;

	if (!pp_iflevel) {
		lexfatal (LEX_PP_UNBALANCED_ENDIF, flocn);
		return;
	}
	if (preproc_unrelax () > 0) {
		lineindent += 2;		/* NOTE: not nice.. */
	}
	if (lineindent != pp_ifstack->indent) {
		lexwarn_s (LEX_PP_IF_INDENT, flocn, "#ENDIF");
	}
	/* remove top item from pp_ifstack */
	istack = pp_ifstack;
	pp_ifstack = istack->next;
	memfree ((void *)istack);
	pp_iflevel--;

	/* skip whitespace */
	while ((ch == ' ') || (ch == '\t')) {
		ch = rch ();
	}
	if (ch == '-') {
		ch = rch ();
		if (ch == '-') {
			/* comment -- skip to end-of-line */
			while ((ch != '\n') && (ch != EOF)) {
				ch = rch ();
			}
		}
	}
	/* skip to end-of-line */
	if ((ch != '\n') && (ch != EOF)) {
		symbindent = currentindent;
		lexerr (LEX_PP_GARBAGE, flocn);

		/* skip to end-of-line */
		while ((ch != '\n') && (ch != EOF)) {
			ch = rch ();
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void preproc_else (void)*/
PRIVATE void preproc_else (void)
{
	if (!pp_iflevel) {
		lexfatal (LEX_PP_UNBALANCED_ELSE, flocn);
		return;
	}
	if (preproc_unrelax () > 0) {
		lineindent += 2;		/* NOTE: not nice.. */
	}
	if (lineindent != pp_ifstack->indent) {
		lexwarn_s (LEX_PP_IF_INDENT, flocn, "#ELSE");
	}
	/* do the condition thing */
	if (!pp_ifstack->hadtrue) {
		pp_ifstack->hadtrue = TRUE;
		pp_ifstack->skipinput = FALSE;
	} else {
		pp_ifstack->skipinput = TRUE;
	}

	/* skip whitespace */
	while ((ch == ' ') || (ch == '\t')) {
		ch = rch ();
	}
	if (ch == '-') {
		ch = rch ();
		if (ch == '-') {
			/* comment -- skip to end-of-line */
			while ((ch != '\n') && (ch != EOF)) {
				ch = rch ();
			}
		}
	}
	/* skip to end-of-line */
	if ((ch != '\n') && (ch != EOF)) {
		symbindent = currentindent;
		lexerr (LEX_PP_GARBAGE, flocn);

		/* skip to end-of-line */
		while ((ch != '\n') && (ch != EOF)) {
			ch = rch ();
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void preproc_elif (void)*/
PRIVATE void preproc_elif (void)
{
	BOOL val, valid_val;

	if (!pp_ifstack) {
		lexfatal (LEX_PP_UNBALANCED_ELIF, flocn);
		return;
	}
	if (preproc_unrelax () > 0) {
		lineindent += 2;		/* NOTE: not nice.. */
	}
	if (lineindent != pp_ifstack->indent) {
		lexwarn_s (LEX_PP_IF_INDENT, flocn, "#ELIF");
	}
	if ((ch != ' ') && (ch != '\t')) {
		/* malformed #ELIF */
		lexfatal (LEX_PP_BAD_ELIF, flocn);
		return;
	}
	while ((ch == ' ') || (ch == '\t')) {
		ch = rch ();
	}
	/* oki, attempt to parse the pre-processor directive */
	val = preproc_parse (LEX_PP_BAD_ELIF, &valid_val, TRUE, TRUE);
	if (valid_val == TRUE) {
		if (val) {
			if (!pp_ifstack->hadtrue) {
				pp_ifstack->hadtrue = TRUE;
				pp_ifstack->skipinput = FALSE;
			} else {
				pp_ifstack->skipinput = TRUE;
			}
		} else {
			pp_ifstack->skipinput = TRUE;
		}
	} else {
		lexfatal (LEX_PP_BAD_ELIF, flocn);
		return;
	}

	/* anything left on the line is garbage (except comments) */
	if (ch == '-') {
		ch = rch ();
		if (ch == '-') {
			/* comment -- skip to end-of-line */
			while ((ch != '\n') && (ch != EOF)) {
				ch = rch ();
			}
		}
	}
	if ((ch != '\n') && (ch != EOF)) {
		symbindent = currentindent;
		lexerr (LEX_PP_GARBAGE, flocn);

		/* skip to end-of-line */
		while ((ch != '\n') && (ch != EOF)) {
			ch = rch ();
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void preproc_warning (void)*/
/*
 *	generates a warning message (from #WARNING)
 */
PRIVATE void preproc_warning (void)
{
	char name[MAXNAMELENGTH + 1];
	int len;

	/* skip any whitespace */
	while ((ch == ' ') || (ch == '\t')) {
		ch = rch ();
	}
	/* scoop up chars into name */
	for (len = 0; (ch != '\n') && (ch != EOF);) {
		name[len++] = ch;
		if (len >= MAXNAMELENGTH) {
			break;
		}
		ch = rch ();
	}
	name[len] = '\0';
	lexwarn_s (LEX_PP_USER_WARNING, flocn, name);
	/* skip to end-of-line */
	while ((ch != '\n') && (ch != EOF)) {
		ch = rch ();
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void preproc_error (void)*/
/*
 *	generates an error message (from #ERROR)
 */
PRIVATE void preproc_error (void)
{
	char name[MAXNAMELENGTH + 1];
	int len;

	/* skip any whitespace */
	while ((ch == ' ') || (ch == '\t')) {
		ch = rch ();
	}
	/* scoop up chars into name */
	for (len = 0; (ch != '\n') && (ch != EOF);) {
		name[len++] = ch;
		if (len >= MAXNAMELENGTH) {
			break;
		}
		ch = rch ();
	}
	name[len] = '\0';
	lexerr_s (LEX_PP_USER_ERROR, flocn, name);
	/* skip to end-of-line */
	while ((ch != '\n') && (ch != EOF)) {
		ch = rch ();
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void preproc_builtin (void)*/
/*
 *	generates the built-in defines and adds to the list
 */
PRIVATE void preproc_builtin (void)
{
	wordnode *tw;

	if (pp_xfile) {
		if (pp_xfile->valtype == PP_VAL_STRING) {
			memfree (pp_xfile->valdata);
			pp_xfile->valdata = NULL;
		}
		memfree (pp_xfile);
	}
	tw = lookupword ("FILE", 4);
	pp_xfile = (pp_dlist_t *)memalloc (sizeof (pp_dlist_t));
	pp_xfile->name = tw;
	pp_xfile->namelen = 4;
	pp_xfile->next = pp_dlist;
	pp_xfile->valtype = PP_VAL_NONE;
	pp_xfile->valdata = NULL;
	preproc_add_define (tw, 4, PP_VAL_STRING, new_string (""));

	if (pp_xline) {
		memfree (pp_xline);
	}
	pp_xline = (pp_dlist_t *)memalloc (sizeof (pp_dlist_t));
	tw = lookupword ("LINE", 4);
	pp_xline->name = tw;
	pp_xline->namelen = 4;
	pp_xline->next = pp_dlist;
	pp_xline->valtype = PP_VAL_NONE;
	pp_xline->valdata = NULL;
	preproc_add_define (tw, 4, PP_VAL_INT, (void *)0);

	if (pp_xfilestack) {
		memfree (pp_xfilestack);
	}
	pp_xfilestack = (pp_dlist_t *)memalloc (sizeof (pp_dlist_t));
	tw = lookupword ("FILESTACK", 9);
	pp_xfilestack->name = tw;
	pp_xfilestack->namelen = 9;
	pp_xfilestack->next = pp_dlist;
	pp_xfilestack->valtype = PP_VAL_NONE;
	pp_xfilestack->valdata = NULL;
	preproc_add_define (tw, 9, PP_VAL_INT, (void *)filestackptr);

#ifdef PROCESS_PRIORITY
	/* the number of available process priorities */
	tw = lookupword ("PROCESS.PRIORITY", 16);
	preproc_add_define (tw, 16, PP_VAL_INT, (void *)32);
	/* the number of affinity bits available */
	tw = lookupword ("PROCESSOR.AFFINITY", 18);
	preproc_add_define (tw, 18, PP_VAL_INT, (void *)(sizeof(int) * 8));
#endif
#ifdef OCCAM2_5
	tw = lookupword ("OCCAM2.5", 9);
	preproc_add_define (tw, 9, PP_VAL_NONE, NULL);
#endif
#ifdef USER_DEFINED_OPERATORS
	tw = lookupword ("USER.DEFINED.OPERATORS", 22);
	preproc_add_define (tw, 22, PP_VAL_NONE, NULL);
#endif
#ifdef MOBILES
	tw = lookupword ("MOBILES", 7);
	preproc_add_define (tw, 7, PP_VAL_NONE, NULL);
	if (mobile_data_types) {
		tw = lookupword ("MOBILE.DATA.TYPES", 17);
		preproc_add_define (tw, 17, PP_VAL_NONE, NULL);
	}
	if (mobile_proc_types) {
		tw = lookupword ("MOBILE.PROC.TYPES", 17);
		preproc_add_define (tw, 17, PP_VAL_NONE, NULL);
	}
#endif
	if (extended_input) {
		tw = lookupword ("EXTENDED.RENDEZVOUS", 19);
		preproc_add_define (tw, 19, PP_VAL_NONE, NULL);
	}
#ifdef INITIAL_DECL
	tw = lookupword ("INITIAL.DECL", 12);
	preproc_add_define (tw, 12, PP_VAL_NONE, NULL);
#endif
#ifdef NEED_QUAD_ALIGNMENT
	tw = lookupword ("NEED.QUAD.ALIGNMENT", 19);
	preproc_add_define (tw, 19, PP_VAL_NONE, NULL);
#endif
#ifdef V_VERSION
	tw = lookupword ("VERSION", 7);
	preproc_add_define (tw, 7, PP_VAL_STRING, new_string (V_VERSION));
#endif
#ifdef TARGET_CANONICAL
	tw = lookupword ("TARGET.CANONICAL", 16);
	preproc_add_define (tw, 16, PP_VAL_STRING, new_string (TARGET_CANONICAL));
#endif
#ifdef TARGET_CPU
	tw = lookupword ("TARGET.CPU", 10);
	preproc_add_define (tw, 10, PP_VAL_STRING, new_string (TARGET_CPU));
#endif
#ifdef TARGET_OS
	tw = lookupword ("TARGET.OS", 9);
	preproc_add_define (tw, 9, PP_VAL_STRING, new_string (TARGET_OS));
#endif
#ifdef TARGET_VENDOR
	tw = lookupword ("TARGET.VENDOR", 13);
	preproc_add_define (tw, 13, PP_VAL_STRING, new_string (TARGET_VENDOR));
#endif
	tw = lookupword ("TARGET.BYTES.PER.WORD", 21);
	preproc_add_define (tw, 21, PP_VAL_INT, (void *)bytesperword);
	tw = lookupword ("TARGET.BITS.PER.WORD", 20);
	preproc_add_define (tw, 20, PP_VAL_INT, (void *)(bytesperword * 8));
	if (target_bigendian) {
		tw = lookupword ("TARGET.BIGENDIAN", 16);
		preproc_add_define (tw, 16, PP_VAL_NONE, NULL);
	}
	if (target_accessaligned) {
		tw = lookupword ("TARGET.ACCESSALIGNED", 20);
		preproc_add_define (tw, 20, PP_VAL_NONE, NULL);
	}
	if (has_fpu_core) {
		tw = lookupword ("TARGET.HAS.FPU", 14);
		preproc_add_define (tw, 14, PP_VAL_NONE, NULL);
	}
	/* load in back-end MPP constants */
	{
		static char *mppnames[] = {"MPP.WPTR", "MPP.IPTR", "MPP.AIPTR", "MPP.MAPCHAIN", "MPP.BARRIER", "MPP.TYPEHASH", "MPP.CODEMAP", "MPA.TEMP.VS", NULL};
		static int mppnlens[] = {8, 8, 9, 12, 11, 12, 11, 11};
		static int mppvals[] = {MPP_WPTR, MPP_IPTR, MPP_AIPTR, MPP_MAPCHAIN, MPP_BARRIER, MPP_TYPEHASH, MPP_CODEMAP, MPA_SETUP_TEMP_VS};
		int i;

		for (i=0; mppnames[i]; i++) {
			tw = lookupword (mppnames[i], mppnlens[i]);
			preproc_add_define (tw, mppnlens[i], PP_VAL_INT, (void *)(mppvals[i]));
		}
	}

	return;
}
/*}}}*/

#ifdef DEBUG_NEXTSYMB
#define nextsymb local_nextsymb
#endif

/*{{{  PRIVATE void nextcsymb (void)*/
/*{{{  comment */
/*****************************************************************************
 *
 *  nextcsymb sets symb and symbindent to value and indentation of next symbol
 *            from source file.  May also set lineindent (not dependant on
 *            newlines here!).
 *
 *****************************************************************************/
/*}}}*/
PRIVATE void nextcsymb (void)
{
	int temp_ch;

	while (TRUE) {
		/*{{{  find the next symbol */
		if (sentend) {
			lexfatal (LEX_EOF, flocn);
		}

		if (newlineflag) {
			/*{{{  skip leading whitespace and set lineindent */
			newlineflag = FALSE;
			temp_ch = ch;
			while ((temp_ch == ' ') || (temp_ch == '\t')) {
				/* skip leading spaces */
				temp_ch = rch ();
			}
			ch = temp_ch;
			/*}}} */
		}

		symbindent = lineindent + co_indent;
		SetFileLine (flocn, linecount);

		switch (do_force_newline) {
		case 2:
			symb = S_COLON;
			do_force_newline = 1;
			co_indent++;
			return;
		case 1:
			/* eat up any whitespace */
			while ((ch == ' ') || (ch == '\t')) {
				ch = rch ();
			}
			symb = S_NEWLINE;
			do_force_newline = 0;
			co_indent = 0;
			return;
		}

#if 0
fprintf (stderr, "lex1: lexing [%c] @ %d... ", (char)ch, symbindent);
#endif
		if (stringcontinuation) {
			/*{{{  read the continuing string */
			symb = readstringliteral ();
			return;
			/*}}} */
		} else {
			/*{{{  switch on ch */
			switch (ch) {
				/*{{{  newline                              return */
			case '\n':
				ch = rch ();
				newlineflag = TRUE;
				/* loop around */
				break;
				/*}}} */
				/*{{{  space / tab                  break */
			case ' ':
			case '\t':
			case '\r':	/* bug TS/2013 04/01/93 */
				do {
					temp_ch = rch ();
				} while ((temp_ch == ' ') || (temp_ch == '\t'));
				ch = temp_ch;
				break;
				/*}}} */
				/*{{{  0 1 2 3 4 5 6 7 8 9                  return */
			case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
				symb = readnumber ();
				return;
				/*}}} */
				/*{{{  a - z  A - Z                         return */
				/*{{{  case a - z A - Z */
			case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
			case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
			case 'u': case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B': case 'C': case 'D':
			case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
			case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
			case 'Y': case 'Z':
				/*}}} */
				{
					char name[MAXNAMELENGTH];
					const int len = readtag (name);
					char name2[MAXNAMELENGTH];
					int i;

					for (i=0; i<len; i++) {
						if ((name[i] >= 'a') && (name[i] <= 'z')) {
							name2[i] = name[i] - 0x20;
						} else {
							break;
						}
					}
					if (i == len) {
						lexword = lookupword (name2, len);
						symb = WTagOf (lexword);
						if (symb != S_NAME) {
							switch (symb) {
							case S_PROC:
								just_seen_prockeyword = TRUE;
								/* FALL THROUGH */
							case S_SKIP:
							case S_STOP:
							case S_SUSPEND:
							case S_IF:
							case S_FUNCTION:
							case S_ALT:
								processing_decl = FALSE;
								break;
							}
							return;
						}
					}
					lexword = lookupword (name, len);
					symb = WTagOf (lexword);
					return;
				}
				/*}}} */
				/*{{{  dot                                  return */
			case '.':	/* All special ASM names begin with dot */
				if (allow_asmnames) {
					/* simply read a name */
					char name[MAXNAMELENGTH];
					const int len = readtag (name);

					lexword = lookupword (name, len);
					symb = WTagOf (lexword);
				} else {
					ch = rch ();
					symb = S_DOT;
				}
				return;
				/*}}} */
				/*{{{  ( ) [ ] + , = ! ? ~ & ;              return */
			case '(':
				ch = rch ();
				symb = S_LPAREN;
				processing_decl = FALSE;
				return;
			case ')':
				ch = rch ();
				symb = S_RPAREN;
				processing_decl = FALSE;
				return;
			case ']':
				ch = rch ();
				symb = S_RBOX;
				return;
			case '+':
				ch = rch ();
				/*{{{  user defined operator additions (for ++) */
#ifdef USER_DEFINED_OPERATORS
				if ((ch == '+') && user_defined_operators) {
					ch = rch ();
					symb = S_DPLUS;
					return;
				}
#endif
				/*}}} */
				symb = S_ADD;
				return;
			case ',':
				ch = rch ();
				symb = S_COMMA;
				return;
			case '=':
				ch = rch ();
				if (ch == '=') {
					ch = rch ();
#ifdef USER_DEFINED_OPERATORS
					if (ch == '=') {
						ch = rch ();
						symb = S_DEQUALS;
						return;
					}
#endif
					symb = S_EQ;
					return;
				}
				symb = S_ASS;
				processing_decl = FALSE;
				return;
			case '!':
				ch = rch ();
				/*{{{  user defined opeartor additions (for !!) */
#ifdef USER_DEFINED_OPERATORS
				if ((ch == '!') && user_defined_operators) {
					ch = rch ();
					symb = S_DEXCLAIMATION;
					return;
				}
#endif
				/*}}} */
				processing_decl = FALSE;
				symb = S_OUTPUT;
				return;
				/*{{{  case '%' and '^' (user defined operators only) */
#ifdef USER_DEFINED_OPERATORS
			case '^':
				/* Added by Jim */
				ch = rch ();
				symb = S_HAT;
				return;
			case '%':
				if (user_defined_operators) {
					ch = rch ();
					if (ch == '%') {
						ch = rch ();
						symb = S_DPERCENT;
						return;
					} else {
						if (ch == '>') {
							ch = rch ();
							symb = S_PERCENTRIGHT;
							return;
						}
					}
					symb = S_PERCENT;
					return;
				}
#endif
				/*}}} */
			case '?':	/* added by Jim */
				ch = rch ();
				/*{{{  user defined operator additions (for ??) */
#ifdef USER_DEFINED_OPERATORS
				if ((ch == '?') && user_defined_operators) {
					ch = rch ();
					symb = S_DQUESTIONM;
				} else {
#else
				{
#endif
					/*}}} */
					processing_decl = FALSE;
					symb = S_INPUT;
				}
				return;
				/*{{{  case '@' for @@,  @>,  @ */
#ifdef USER_DEFINED_OPERATORS
			case '@':
				if (user_defined_operators) {
					ch = rch ();
					if (ch == '@') {
						ch = rch ();
						symb = S_DAMPERSAT;
						return;
					} else {
						if (ch == '>') {
							ch = rch ();
							symb = S_AMPERSATRIGHT;
							return;
						}
					}
					symb = S_AMPERSAT;
					return;
				}
#endif
				/*}}} */
			case '~':
				ch = rch ();
				symb = S_BITNOT;
				return;
			case '&':
				ch = rch ();
				/*{{{  user defined operator additions for &, &&, &> etc */
#ifdef USER_DEFINED_OPERATORS
				if ((ch == '&') && user_defined_operators) {
					ch = rch ();
					symb = S_DAMPERSAND;
					return;
				} else {
					if ((ch == '>') && user_defined_operators) {
						ch = rch ();
						symb = S_AMPERSANDRIGHT;
						return;
					}
				}
#endif
				/*}}} */
				symb = S_AMPERSAND;
				return;
			case ';':
				if (processing_decl && (lexmode == LEX_CSOURCE)) {
					/* part of a declaration (map to S_COLON -> S_NEWLINE) */
					ch = rch ();
					do_force_newline = 2;
				} else if (lexmode == LEX_CSOURCE) {
					/* end of some process (map to S_NEWLINE) */
					ch = rch ();
					do_force_newline = 1;
				} else {
					ch = rch ();
					symb = S_SEMICOLON;
					return;
				}
				break;
				/*}}} */
				/*{{{  *                                    return */
			case '*':
#if 0				/* we've checked for this outside the switch */
				if (stringcontinuation) {
					ch = rsch ();
					symb = readstringliteral ();
					return;
				} else
#endif
				{
					ch = rch ();
					symb = S_MULT;
					return;
				}
				/*}}} */
				/*{{{  [                                    return */
			case '[':
				ch = rch ();
				if (ch == ']') {
					ch = rch ();
					symb = S_BOX;
					return;
				}
				/*{{{  user defined operator extensions for [> */
#ifdef USER_DEFINED_OPERATORS
				if ((ch == '>') && (user_defined_operators)) {
					/* Added by Jim for rotates 17/1/97 */
					ch = rch ();
					symb = S_ROTATERIGHT;
					return;
				}
#endif
				/*}}} */
				symb = S_LBOX;
				return;
				/*}}} */
				/*{{{  :                                    return */
			case ':':
				ch = rch ();
				if (ch == ':') {
					ch = rch ();
					symb = S_COLON2;
					return;
				};
				symb = S_COLON;
				return;
				/*}}} */
				/*{{{  $                                    return */
			case '$':
				ch = rch ();
#ifdef USER_DEFINED_OPERATORS
				if ((ch == '$') && (user_defined_operators)) {
					/* Added by Jim */
					ch = rch ();
					symb = S_DDOLLAR;
					return;
				}
#endif
				literalv[0] = '#';
				literalp = 1;
				symb = readhex ();
				return;
				/*}}} */
				/*{{{  #                                    return */
			case '#':
				{
					/* note that #INCLUDE is always allowed! */
					const BOOL directives_allowed = (current_fe_data->fe_lang & FE_LANG_NDL) == 0;

					literalv[0] = '#';
					literalp = 1;
					switch (ch = rch ()) {
						/*{{{  c                  #comment                 return */
					case 'c':
						symb = rhashword ("omment", S_HCOMMENT);
						return;
						/*}}} */
						/*{{{  i                  #include or #import      return */
					case 'i':
						ch = rch ();
						if (ch == 'n') {
							/* #include is always allowed */
							symb = rhashword ("clude", S_INCLUDE);
						} else if (ch == 'm' && directives_allowed) {
							symb = rhashword ("port", S_IMPORT);
						} else {
							lexerr (LEX_HASHWORD_ERROR, flocn);
							symb = S_ILLEGALSYMB;
						}
						return;
						/*}}} */
						/*{{{  #option, #pragma, #sc, #use                 return */
					case 'n':
					case 'o':
					case 'p':
					case 's':
					case 'u':
						if (directives_allowed) {
							switch (ch) {
#ifndef ARRAYCONSTRUCTOR
							case 'n':
								symb = rhashword ("etwork", S_HNETWORK);
								break;
#endif	/* !ARRAYCONSTRUCTOR */
							case 'o':
								symb = rhashword ("ption", S_OPTION);
								break;
							case 'p':
								symb = rhashword ("ragma", S_PRAGMA);
								break;
							case 's':
								symb = rhashword ("c", S_SC);
								break;
							case 'u':
								symb = rhashword ("se", S_USE);
								break;
							}
						} else {
							lexerr (LEX_HASHWORD_ERROR, flocn);
							symb = S_ILLEGALSYMB;
						}
						return;
						/*}}} */
						/*{{{  default            error                    return */
					default:
						lexerr (LEX_HASHWORD_ERROR, flocn);
						symb = S_ILLEGALSYMB;
						return;
						/*}}} */
					}
				}
				break;	/* just to make sure */
				/*}}} */
				/*{{{  -                            break / return */
			case '-':
				ch = rsch ();
				if (ch == '-') {
					/* Read past comment */
					literalp = 0;
					temp_ch = rsch ();
					while (temp_ch != '\n') {
						/* don't copy into literalv, so it can't overflow:
						   bug 1020 18/10/90 */
						/* literalv[literalp++] = ch; */
						temp_ch = rsch ();
					}
					ch = '\n';
					if (!current_fe_data->fe_ignore_comments) {
						/* bug 853 18/1/91 */
						symb = S_COMMENT;
						return;
					}
					/* otherwise drop round loop */
				} else if (ch == '>') {
					ch = rsch ();
					symb = S_SEQCOMP;
					return;
				} else {
					symb = S_SUBTRACT;
					return;
				}
				break;
				/*}}} */
				/*{{{  /                                    return */
			case '/':
				ch = rch ();
				if (ch == '\\') {
					ch = rch ();
					symb = S_BITAND;
					return;
				} else if ((ch == '/') && (lexmode == LEX_CSOURCE)) {
					/* eat comment */
					literalp = 0;
					temp_ch = rsch ();
					while (temp_ch != '\n') {
						temp_ch = rsch ();
					}
					ch = '\n';
					if (!current_fe_data->fe_ignore_comments) {
						symb = S_COMMENT;
						return;
					}
				} else {
					symb = S_DIV;
					return;
				}
				break;
				/*}}} */
				/*{{{  \                                    return */
			case '\\':
				if ((ch = rch ()) == '/') {
					ch = rch ();
					symb = S_BITOR;
					return;
				};
				symb = S_REM;
				return;
				/*}}} */
				/*{{{  <                                    return */
			case '<':
				switch (ch = rch ()) {
				case '<':
					ch = rch ();
					symb = S_LSHIFT;
					return;
				case '=':
					ch = rch ();
					symb = S_LE;
					return;
				case '>':
					ch = rch ();
					symb = S_NE;
					return;
					/*{{{  user defined operator extensions (for &>, [>, %>, @>) */
#ifdef USER_DEFINED_OPERATORS
				case ']':
					if (user_defined_operators) {
						ch = rch ();
						symb = S_ROTATELEFT;
						return;
					}
				case '@':
					if (user_defined_operators) {
						ch = rch ();
						symb = S_LEFTAMPERSAT;
						return;
					}
				case '%':
					if (user_defined_operators) {
						ch = rch ();
						symb = S_LEFTPERCENT;
						return;
					}
				case '&':
					if (user_defined_operators) {
						ch = rch ();
						symb = S_LEFTAMPERSAND;
						return;
					}
#endif
					/*}}} */
				default:
					symb = S_LS;
					return;
				};
				/*}}} */
				/*{{{  >                                    return */
			case '>':
				switch (ch = rch ()) {
				case '<':
					ch = rch ();
					symb = S_XOR;
					return;
				case '=':
					ch = rch ();
					symb = S_GE;
					return;
				case '>':
					ch = rch ();
					symb = S_RSHIFT;
					return;
				default:
					symb = S_GR;
					return;
				};
				/*}}} */
				/*{{{  '                                    return */
			case '\'':
				ch = rsch ();
				symb = readbyteliteral ();
				return;
				/*}}} */
				/*{{{  "                                    return */
			case '"':
				ch = rsch ();
				symb = readstringliteral ();
				return;
				/*}}} */
				/*{{{  | (pipe/bar)                         return */
			case '|':
				switch (ch = rch ()) {
				case '|':
					ch = rch ();
					symb = S_PARCOMP;
					break;
				default:
					symb = S_BAR;
					break;
				}
				return;
				/*}}}  */
				/*{{{  { and } in LEX_CSOURCE */
			case '{':
				ch = rch ();
				do_force_newline = 1;
				lineindent += 2;
#if 0
fprintf (stderr, "lex1: saw open-brace, setting lineindent to %d... ", lineindent);
#endif
				brace_counter++;
				if (just_seen_prockeyword) {
					proc_decl_stack[proc_decl_ptr] = brace_counter;
					proc_decl_ptr++;
					just_seen_prockeyword = FALSE;
				}
				processing_decl = TRUE;
				break;
			case '}':
				ch = rch ();
				lineindent -= 2;
#if 0
fprintf (stderr, "lex1: saw close-brace, setting line-indent to %d...", lineindent);
#endif
				if ((proc_decl_ptr > 0) && (proc_decl_stack[proc_decl_ptr-1] == brace_counter)) {
#if 0
fprintf (stderr, "lex1: closing brace of proc... ");
#endif
					do_force_newline = 2;
					proc_decl_ptr--;
				} else {
					do_force_newline = 1;
				}
				brace_counter--;
				co_indent = 0;
				break;
				/*}}}  */
				/*{{{  EOF                          break / return */
			case EOF:
				if (filestackptr > 0) {
					resume_file ();
				} else {
					close_file ();
					symb = S_END;
					sentend = TRUE;
					return;
				}
				break;
				/*}}} */
				/*{{{  default                      break */
			default:
				lexerr_i (LEX_ILLEGAL_CHAR, flocn, ch);
				symb = S_ILLEGALSYMB;
				ch = rch ();
				DEBUG_MSG (("nextsymb: looping for next char... "));
				break;
				/*}}} */
			}
			/*}}} */
		}
		/*}}} */
	}
}
/*}}}*/
/*{{{  PUBLIC void nextsymb (void)*/
/*{{{  comment */
/*****************************************************************************
 *
 *  nextsymb sets symb and symbindent to value and indentation of next symbol
 *           from source file.  If we start a new line, lineindent is set
 *           to the indentation of the first symbol on the line.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void nextsymb (void)
{
	int temp_ch;

	if (lexmode == LEX_CSOURCE) {
		nextcsymb ();
		return;
	}
	while (TRUE) {
		pp_ifstack_t *ifp;
		BOOL skipping;

		/*{{{  find the next symbol */
		if (sentend) {
			lexfatal (LEX_EOF, flocn);
		}

		if (newlineflag) {
			/*{{{  skip leading spaces/whitespace and set lineindent */
			newlineflag = FALSE;
			while (ch == ' ') { 
				ch = rch (); 
			} 
			lineindent = currentindent;
#if 0
fprintf (stderr, "nextsymb (newlineflag): lineindent set to %d\n", lineindent);
#endif
			DEBUG_MSG (("nextsymb: setting lineindent to: %d\n", lineindent));
			/*}}} */
		}

		symbindent = currentindent;
		SetFileLine (flocn, linecount);

		/*{{{  are any of the #IFs we're currently in false? */
		skipping = FALSE;
		for (ifp = pp_ifstack; ifp != NULL; ifp = ifp->next) {
			if (ifp->skipinput) {
				skipping = TRUE;
				break;
			}
		}
		/*}}}*/

		if (stringcontinuation) {
			/*{{{  read the continuing string */
			symb = readstringliteral ();
			return;
			/*}}} */
		} else if (skipping) {
			/*{{{  skip over input searching for more pre-processor directives -- still need to process nested #IF,#ELSE,#ELIF,#ENDIF */
			if (ch == '#') {
				literalv[0] = '#';
				literalp = 1;
				ch = rch ();
				switch (ch) {
				case 'I':
					ch = rch ();
					if (ch == 'F') {
						symb = rhashword ("", S_PREPROC);
						if (symb == S_PREPROC) {
							preproc_if ();
						}
					}
					break;
				case 'E':
					ch = rch ();
					if (ch == 'L') {
						ch = rch ();
						if (ch == 'S') {
							symb = rhashword ("E", S_PREPROC);
							if (symb == S_PREPROC) {
								preproc_else ();
							}
							break;
						} else if (ch == 'I') {
							symb = rhashword ("F", S_PREPROC);
							if (symb == S_PREPROC) {
								preproc_elif ();
							}
							break;
						}
					} else if (ch == 'N') {
						symb = rhashword ("DIF", S_PREPROC);
						if (symb == S_PREPROC) {
							preproc_endif ();
						}
					}
					break;
				}
			}
			/* if there was a directive, we handled it.  now just skip to end-of-line */
			while ((ch != '\n') && (ch != EOF)) {
				ch = rch ();
			}
			if (ch == '\n') {
				newlineflag = TRUE;
				ch = rch ();
			} else if (ch == EOF) {
				/* this is actually very bad -- we should never encounter EOF while skipping */
				lexfatal (LEX_PP_UNBALANCED_IF, pp_ifstack->locn);

				if (filestackptr > 0) {
					resume_file ();
				} else {
					close_file ();
					symb = S_END;
					sentend = TRUE;
					return;
				}
			}
			/* then we just loop around for the next bit */
			/*}}}*/
		} else {
			/*{{{  switch on ch */
			switch (ch) {
				/*{{{  newline                              return */
			case '\n':
				symb = S_NEWLINE;
				if (ls_valid) {
					/* we're peeking ahead, so don't pull in the next line! 
					 * causes nextsymb() to persistently return S_NEWLINE, but heyho!*/
				} else {
					newlineflag = TRUE;
					ch = rch ();
				}
				return;
				/* otherwise loop around */
				break;
				/*}}} */
				/*{{{  space / tab                  break */
			case ' ':
				while ((temp_ch = rch ()) == ' ');	/* SKIP */
				ch = temp_ch;
				break;
			case '\t':
			case '\r':	/* bug TS/2013 04/01/93 */
				ch = rch ();
				break;
				/*}}} */
				/*{{{  0 1 2 3 4 5 6 7 8 9                  return */
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				symb = readnumber ();
				return;
				/*}}} */
				/*{{{  a - z  A - Z                         return */
				/*{{{  case a - z A - Z */
			case 'a':
			case 'b':
			case 'c':
			case 'd':
			case 'e':
			case 'f':
			case 'g':
			case 'h':
			case 'i':
			case 'j':
			case 'k':
			case 'l':
			case 'm':
			case 'n':
			case 'o':
			case 'p':
			case 'q':
			case 'r':
			case 's':
			case 't':
			case 'u':
			case 'v':
			case 'w':
			case 'x':
			case 'y':
			case 'z':
			case 'A':
			case 'B':
			case 'C':
			case 'D':
			case 'E':
			case 'F':
			case 'G':
			case 'H':
			case 'I':
			case 'J':
			case 'K':
			case 'L':
			case 'M':
			case 'N':
			case 'O':
			case 'P':
			case 'Q':
			case 'R':
			case 'S':
			case 'T':
			case 'U':
			case 'V':
			case 'W':
			case 'X':
			case 'Y':
			case 'Z':
				/*}}} */
				{
					char name[MAXNAMELENGTH];
					const int len = readtag (name);

					lexword = lookupword (name, len);
					symb = WTagOf (lexword);
					return;
				}
				/*}}} */
				/*{{{  dot                                  return */
			case '.':	/* All special ASM names begin with dot */
				if (allow_asmnames) {
					/* simply read a name */
					char name[MAXNAMELENGTH];
					const int len = readtag (name);

					lexword = lookupword (name, len);
					symb = WTagOf (lexword);
				} else {
					ch = rch ();
					symb = S_DOT;
				}
				return;
				/*}}} */
				/*{{{  ( ) [ ] + , = ! ? ~ & ;              return */
			case '(':
				ch = rch ();
				symb = S_LPAREN;
				return;
			case ')':
				ch = rch ();
				symb = S_RPAREN;
				return;
			case ']':
				ch = rch ();
				symb = S_RBOX;
				return;
			case '+':
				ch = rch ();
				/*{{{  user defined operator additions (for ++) */
#ifdef USER_DEFINED_OPERATORS
				if ((ch == '+') && user_defined_operators) {
					ch = rch ();
					symb = S_DPLUS;
					return;
				}
#endif
				/*}}} */
				symb = S_ADD;
				return;
			case ',':
				ch = rch ();
				symb = S_COMMA;
				return;
			case '=':
				ch = rch ();
				/*{{{  user defined operator additions (for ==) */
#ifdef USER_DEFINED_OPERATORS
				if ((ch == '=') && user_defined_operators) {
					ch = rch ();
					symb = S_DEQUALS;
					return;
				}
#endif
				/*}}} */
				symb = S_EQ;
				return;
			case '!':
				ch = rch ();
				/*{{{  user defined opeartor additions (for !!) */
#ifdef USER_DEFINED_OPERATORS
				if ((ch == '!') && user_defined_operators) {
					ch = rch ();
					symb = S_DEXCLAIMATION;
					return;
				}
#endif
				/*}}} */
				symb = S_OUTPUT;
				return;
				/*{{{  case '%' and '^' (user defined operators only) */
#ifdef USER_DEFINED_OPERATORS
			case '^':
				/* Added by Jim */
				ch = rch ();
				symb = S_HAT;
				return;
			case '%':
				if (user_defined_operators) {
					ch = rch ();
					if (ch == '%') {
						ch = rch ();
						symb = S_DPERCENT;
						return;
					} else {
						if (ch == '>') {
							ch = rch ();
							symb = S_PERCENTRIGHT;
							return;
						}
					}
					symb = S_PERCENT;
					return;
				}
#endif
				/*}}} */
			case '?':	/* added by Jim */
				ch = rch ();
				/*{{{  user defined operator additions (for ??) */
				switch (ch) {
#ifdef USER_DEFINED_OPERATORS
				case '?':
					if (user_defined_operators) {
						ch = rch ();
						symb = S_DQUESTIONM;
						return;
					}
					/* fall through */
#endif
				/*}}}  */
				default:
					symb = S_INPUT;
					break;
				}
				return;
				/*{{{  case '@' for @@,  @>,  @ */
#ifdef USER_DEFINED_OPERATORS
			case '@':
				if (user_defined_operators) {
					ch = rch ();
					if (ch == '@') {
						ch = rch ();
						symb = S_DAMPERSAT;
						return;
					} else {
						if (ch == '>') {
							ch = rch ();
							symb = S_AMPERSATRIGHT;
							return;
						}
					}
					symb = S_AMPERSAT;
					return;
				}
#endif
				/*}}} */
			case '~':
				ch = rch ();
				symb = S_BITNOT;
				return;
			case '&':
				ch = rch ();
				/*{{{  user defined operator additions for &, &&, &> etc */
#ifdef USER_DEFINED_OPERATORS
				if ((ch == '&') && user_defined_operators) {
					ch = rch ();
					symb = S_DAMPERSAND;
					return;
				} else {
					if ((ch == '>') && user_defined_operators) {
						ch = rch ();
						symb = S_AMPERSANDRIGHT;
						return;
					}
				}
#endif
				/*}}} */
				symb = S_AMPERSAND;
				return;
			case ';':
				ch = rch ();
				symb = S_SEMICOLON;
				return;
				/*}}} */
				/*{{{  *                                    return */
			case '*':
#if 0				/* we've checked for this outside the switch */
				if (stringcontinuation) {
					ch = rsch ();
					symb = readstringliteral ();
					return;
				} else
#endif
				{
					ch = rch ();
					symb = S_MULT;
					return;
				}
				/*}}} */
				/*{{{  [                                    return */
			case '[':
				ch = rch ();
				if (ch == ']') {
					ch = rch ();
					symb = S_BOX;
					return;
				}
				/*{{{  user defined operator extensions for [> */
#ifdef USER_DEFINED_OPERATORS
				if ((ch == '>') && (user_defined_operators)) {
					/* Added by Jim for rotates 17/1/97 */
					ch = rch ();
					symb = S_ROTATERIGHT;
					return;
				}
#endif
				/*}}} */
				symb = S_LBOX;
				return;
				/*}}} */
				/*{{{  :                                    return */
			case ':':
				ch = rch ();
				if (ch == '=') {
					ch = rch ();
					symb = S_ASS;
					return;
				};
				if (ch == ':') {
					ch = rch ();
					symb = S_COLON2;
					return;
				};
				symb = S_COLON;
				return;
				/*}}} */
				/*{{{  $                                    return */
			case '$':
				ch = rch ();
#ifdef USER_DEFINED_OPERATORS
				if ((ch == '$') && (user_defined_operators)) {
					/* Added by Jim */
					ch = rch ();
					symb = S_DDOLLAR;
					return;
				}
#endif
				literalv[0] = '#';
				literalp = 1;
				symb = readhex ();
				return;
				/*}}} */
				/*{{{  #                                    return */
			case '#':
				{
					/* note that #INCLUDE is always allowed! */
					const BOOL directives_allowed = (current_fe_data->fe_lang & FE_LANG_NDL) == 0;

					literalv[0] = '#';
					literalp = 1;
					switch (ch = rch ()) {
						/*{{{  0-9, A, B, F must be a hex number     return */
					case '0':
					case '1':
					case '2':
					case '3':
					case '4':
					case '5':
					case '6':
					case '7':
					case '8':
					case '9':
					case 'A':
					case 'B':
					case 'F':
						symb = readhex ();
						return;
						/*}}} */
						/*{{{  #                  ##name                   return */
					case '#':
						{
							char name[MAXNAMELENGTH];
							wordnode *tlexword;
							int len;
							
							ch = rch ();
							len = readtag (name);
							tlexword = lookupword (name, len);
							symb = preproc_get_define (tlexword);
							if (symb == S_ILLEGALSYMB) {
								lexfatal_s (LEX_PP_NOSUCHNAME, flocn, name);
							} else if (symb == S_SKIP) {
								lexfatal_s (LEX_PP_NOSUCHVAL, flocn, name);
								symb = S_ILLEGALSYMB;
							}
						}
						return;
						/*}}}*/
						/*{{{  C  hex number or   #COMMENT                 return */
					case 'C':
						if ((ch = rch ()) == 'O' && directives_allowed) {
							symb = rhashword ("MMENT", S_HCOMMENT);
						} else {
							literalv[literalp++] = 'C';
							symb = readhex ();
						}
						return;
						/*}}} */
						/*{{{  D  hex number or   #DEFINE (always allowed)   break/return */
					case 'D':
						if (currentindent != (lineindent + 1)) {
							/* #DEFINE can only occur as the first thing on a line */
							symb = readhex ();
						} else if ((ch = rch ()) == 'E') {
							/* yuk..! */
							if ((ch = rch()) == 'F') {
								if ((ch = rch()) == 'I') {
									symb = rhashword ("NE", S_PREPROC);
									if (symb == S_PREPROC) {
										preproc_define ();
										if (ch == '\n') {
											newlineflag = TRUE;
											ch = rch ();
										}
										break;			/* from switch(), loop for next symbol */
									}
								} else {
									literalv[literalp++] = 'D';
									literalv[literalp++] = 'E';
									literalv[literalp++] = 'F';
									symb = readhex ();
								}
							} else {
								literalv[literalp++] = 'D';
								literalv[literalp++] = 'E';
								symb = readhex ();
							}
						} else {
							literalv[literalp++] = 'D';
							symb = readhex ();
						}
						return;
						/*}}}*/
						/*{{{  E  hex number or   #ELIF,#ELSE,#ENDIF      return/break */
					case 'E':
						if (currentindent != (lineindent + 1)) {
							/* #ELIF, #ELSE and #ENDIF have to be first on the line */
							symb = readhex ();
						} else {
							ch = rch ();
							if (ch == 'L') {
								ch = rch ();
								if (ch == 'S') {
									symb = rhashword ("E", S_PREPROC);
									if (symb == S_PREPROC) {
										preproc_else ();
									}
								} else if (ch == 'I') {
									symb = rhashword ("F", S_PREPROC);
									if (symb == S_PREPROC) {
										preproc_elif ();
									}
								} else {
									lexerr (LEX_HASHWORD_ERROR, flocn);
									symb = S_ILLEGALSYMB;
								}
							} else if (ch == 'N') {
								symb = rhashword ("DIF", S_PREPROC);
								if (symb == S_PREPROC) {
									preproc_endif ();
								}
							} else if (ch == 'R') {
								symb = rhashword ("ROR", S_PREPROC);
								if (symb == S_PREPROC) {
									preproc_error ();
								}
							} else {
								literalv[literalp++] = 'E';
								symb = readhex ();
							}
							if (symb == S_PREPROC) {
								if (ch == '\n') {
									newlineflag = TRUE;
									ch = rch ();
								}
								break;					/* from switch(), loop for next symbol */
							}
						}

						return;
						/*}}}*/
						/*{{{  E  hex number or   #EXTERNAL                return */
#if 0				/* #EXTERNAL replaced by #PRAGMA */
					case 'E':
						if ((ch = rch ()) == 'X' && directives_allowed) {
							symb = rhashword ("TERNAL", S_EXTERNAL);
						} else {
							literalv[literalp++] = 'E';
							symb = readhex ();
						}
						return;
#endif
						/*}}} */
						/*{{{  I                  #INCLUDE or #IMPORT      return, #IF (always allowed) break */
					case 'I':
						ch = rch ();
						if (ch == 'N') {
							/* #INCLUDE is always allowed */
							symb = rhashword ("CLUDE", S_INCLUDE);
						} else if (ch == 'M' && directives_allowed) {
							symb = rhashword ("PORT", S_IMPORT);
						} else if ((ch == 'F') && (currentindent == (lineindent + 2))) {
							symb = rhashword ("", S_PREPROC);
							if (symb == S_PREPROC) {
								preproc_if ();
								if (ch == '\n') {
									newlineflag = TRUE;
									ch = rch ();
								}
								break;					/* from switch(), loop for next symbol */
							}
						} else {
							lexerr (LEX_HASHWORD_ERROR, flocn);
							symb = S_ILLEGALSYMB;
						}
						return;
						/*}}} */
						/*{{{  R                  #RELAX                   return */
					case 'R':
						symb = rhashword ("ELAX", S_PREPROC);
						if (symb == S_PREPROC) {
							/* relax indentation */
							if (preproc_relax () < 0) {
								symb = S_ILLEGALSYMB;
								return;
							}
							break;						/* from switch(), loop for next symbol */
						} else {
							lexerr (LEX_HASHWORD_ERROR, flocn);
							symb = S_ILLEGALSYMB;
						}
						return;
						/*}}}*/
						/*{{{  #USE, #UNDEF (always allowed)               break/return */
					case 'U':
						ch = rch ();
						if (ch == 'N') {
							/* #UNDEF is always allowed */
							symb = rhashword ("DEF", S_PREPROC);
							if (symb == S_PREPROC) {
								preproc_undef ();
								if (ch == '\n') {
									newlineflag = TRUE;
									ch = rch ();
								}
								break;					/* from switch(), loop for next symbol */
							}
						} else if ((ch == 'S') && directives_allowed) {
							symb = rhashword ("E", S_USE);
						} else {
							lexerr (LEX_HASHWORD_ERROR, flocn);
							symb = S_ILLEGALSYMB;
						}
						return;
						/*}}}*/
						/*{{{  #WARNING (always allowed)                   break/return */
					case 'W':
						symb = rhashword ("ARNING", S_PREPROC);
						if (symb == S_PREPROC) {
							preproc_warning ();
							if (ch == '\n') {
								newlineflag = TRUE;
								ch = rch ();
							}
							break;						/* from switch(), loop for next symbol */
						} else {
							lexerr (LEX_HASHWORD_ERROR, flocn);
							symb = S_ILLEGALSYMB;
						} 
						return;
						/*}}}*/
						/*{{{  #OPTION, #PRAGMA, #SC                       return */
					case 'N':
					case 'O':
					case 'P':
					case 'S':
						if (directives_allowed) {
							switch (ch) {
#ifndef ARRAYCONSTRUCTOR
							case 'N':
								symb = rhashword ("ETWORK", S_HNETWORK);
								break;
#endif	/* !ARRAYCONSTRUCTOR */
							case 'O':
								symb = rhashword ("PTION", S_OPTION);
								break;
							case 'P':
								symb = rhashword ("RAGMA", S_PRAGMA);
								break;
							case 'S':
								symb = rhashword ("C", S_SC);
								break;
							}
						} else {
							lexerr (LEX_HASHWORD_ERROR, flocn);
							symb = S_ILLEGALSYMB;
						}
						return;
						/*}}} */
						/*{{{  a, b, c, d, e, f   badly formed hex number  return */
					case 'a':
					case 'b':
					case 'c':
					case 'd':
					case 'e':
					case 'f':
						lexerr_i (LEX_E_HEXDIGIT, flocn, ch);
						/*lexerror(LEX_NUMBER_ERROR, flocn, 0); */
						symb = S_ILLEGALSYMB;
						return;
						/*}}} */
						/*{{{  default            error                    return */
					default:
						lexerr (LEX_HASHWORD_ERROR, flocn);
						symb = S_ILLEGALSYMB;
						return;
						/*}}} */
					}
				}
				break;	/* just to make sure */
				/*}}} */
				/*{{{  -                            break / return */
			case '-':
				ch = rsch ();
				if (ch == '-') {
					/* Read past comment */
					literalp = 0;
					temp_ch = rsch ();
					while (temp_ch != '\n') {
						/* don't copy into literalv, so it can't overflow:
						   bug 1020 18/10/90 */
						/* literalv[literalp++] = ch; */
						temp_ch = rsch ();
					}
					ch = '\n';
					if (!current_fe_data->fe_ignore_comments) {
						/* bug 853 18/1/91 */
						symb = S_COMMENT;
						return;
					}
					/* otherwise drop round loop */
				} else if (ch == '>') {
					ch = rsch ();
					symb = S_SEQCOMP;
					return;
				} else {
					symb = S_SUBTRACT;
					return;
				}
				break;
				/*}}} */
				/*{{{  /                                    return */
			case '/':
				ch = rch ();
				if (ch == '\\') {
					ch = rch ();
					symb = S_BITAND;
					return;
				} else {
					symb = S_DIV;
					return;
				}
				break;
				/*}}} */
				/*{{{  \                                    return */
			case '\\':
				if ((ch = rch ()) == '/') {
					ch = rch ();
					symb = S_BITOR;
					return;
				};
				symb = S_REM;
				return;
				/*}}} */
				/*{{{  <                                    return */
			case '<':
				switch (ch = rch ()) {
				case '<':
					ch = rch ();
					symb = S_LSHIFT;
					return;
				case '=':
					ch = rch ();
					symb = S_LE;
					return;
				case '>':
					ch = rch ();
					symb = S_NE;
					return;
					/*{{{  user defined operator extensions (for &>, [>, %>, @>) */
#ifdef USER_DEFINED_OPERATORS
				case ']':
					if (user_defined_operators) {
						ch = rch ();
						symb = S_ROTATELEFT;
						return;
					}
				case '@':
					if (user_defined_operators) {
						ch = rch ();
						symb = S_LEFTAMPERSAT;
						return;
					}
				case '%':
					if (user_defined_operators) {
						ch = rch ();
						symb = S_LEFTPERCENT;
						return;
					}
				case '&':
					if (user_defined_operators) {
						ch = rch ();
						symb = S_LEFTAMPERSAND;
						return;
					}
#endif
					/*}}} */
				default:
					symb = S_LS;
					return;
				};
				/*}}} */
				/*{{{  >                                    return */
			case '>':
				switch (ch = rch ()) {
				case '<':
					ch = rch ();
					symb = S_XOR;
					break;
				case '=':
					ch = rch ();
					symb = S_GE;
					break;
				case '>':
					ch = rch ();
					symb = S_RSHIFT;
					break;
				default:
					symb = S_GR;
					break;
				}
				return;
				/*}}} */
				/*{{{  | (pipe/bar)                         return */
			case '|':
				switch (ch = rch ()) {
				case '|':
					ch = rch ();
					symb = S_PARCOMP;
					break;
				default:
					symb = S_BAR;
					break;
				}
				return;
				/*}}}  */
				/*{{{  '                                    return */
			case '\'':
				ch = rsch ();
				symb = readbyteliteral ();
				return;
				/*}}} */
				/*{{{  "                                    return */
			case '"':
				ch = rsch ();
				symb = readstringliteral ();
				return;
				/*}}} */
				/*{{{  EOF                          break / return */
			case EOF:
				if (filestackptr > 0) {
					resume_file ();
				} else {
					/* check for unbalanced pre-processor stuffs */
					if (pp_ifstack) {
						lexfatal (LEX_PP_UNBALANCED_IF, pp_ifstack->locn);
					}
					close_file ();
					symb = S_END;
					sentend = TRUE;
					return;
				}
				break;
				/*}}} */
				/*{{{  default                      break */
			default:
				lexerr_i (LEX_ILLEGAL_CHAR, flocn, ch);
				symb = S_ILLEGALSYMB;
				ch = rch ();
				DEBUG_MSG (("nextsymb: looping for next char... "));
				break;
				/*}}} */
			}
			/*}}} */
		}
		/*}}} */
	}
}
/*}}}*/

#ifdef DEBUG_NEXTSYMB
#undef nextsymb
PUBLIC void nextsymb (void)
{
	local_nextsymb ();
	printlex (current_fe_data->fe_outfile);
}
#endif

/*{{{  PUBLIC void printlex () */
PUBLIC void printlex (FILE * fptr)
{
#ifdef DEBUG_NEXTSYMB
	fprintf (fptr, "%d,%d:", lineindent, symbindent);
#endif
	switch (symb) {
	default:
		fprintf (fptr, "%s ", tagstring (symb));
		break;
		/*{{{  S_NEWLINE; */
	case S_NEWLINE:
		fprintf (fptr, "NEWLINE\n");
		break;
		/*}}} */
		/*{{{  S_END */
	case S_END:
		fprintf (fptr, "END\n");
		break;
		/*}}} */
		/*{{{  S_UBYTELIT */
	case S_UBYTELIT:
		fprintf (fptr, "BYTELIT:");
		printbyte (fptr, literalv[1]);
		break;
		/*}}} */
		/*{{{  S_INTLIT */
#if 0
	case S_INTLIT:
		fprintf (fptr, "INTLIT:%.*s ", literalp, literalv);
		break;
#endif
		/*}}} */
		/*{{{  S_UINTLIT */
	case S_UINTLIT:
		fprintf (fptr, "UINTLIT:%.*s ", literalp, literalv);
		break;
		/*}}} */
		/*{{{  S_NAME */
	case S_NAME:
		fprintf (fptr, "NAME:%s ", WNameOf (lexword));
		break;
		/*}}} */
		/*{{{  S_STRING */
	case S_STRING:
		fprintf (fptr, "STRING:");
		printstring (fptr, literalv, literalp);
		break;
		/*}}} */
		/*{{{  S_STRINGCONT */
	case S_STRINGCONT:
		fprintf (fptr, "STRINGCONT:");
		printstring (fptr, literalv, literalp);
		break;
		/*}}} */
		/*{{{  S_UREALLIT */
	case S_UREALLIT:
		fprintf (fptr, "UREALLIT:%.*s ", literalp, literalv);
		break;
		/*}}} */
		/*{{{  S_COMMENT */
	case S_COMMENT:
		fprintf (fptr, "-- %.*s", literalp, literalv);
		break;
		/*}}} */
	}
}
/*}}}*/
/*{{{  PUBLIC void lexinit () */
PUBLIC void lexinit (void)
{
	flocn = NOPOSN;
	set_vtilocn (&flocn);
	filestackptr = 0;
	baseindent = 0;
	currentfilenum = (-1);
	SetFileLine (flocn, 0);
	SetFileNum (flocn, currentfilenum);
	allow_asmnames = FALSE;
	totallines = 0;
	if (linebuffer == NULL) {
		linebuffer = memalloc (linesize + 2);	/* 2 extra for "\n\0" */
	}
	init_lex_error (&symb, &symbindent, &baseindent, &lexword, &literalp, literalv);
	/* predefines are now read in the checker */
	brace_counter = 0;
	proc_decl_ptr = 0;
	processing_decl = FALSE;
	do_force_newline = FALSE;
	just_seen_prockeyword = FALSE;
	if (!pp_defined) {
		/* initialise the pre-processor */
		pp_defined = lookupword ("DEFINED", 7);
		pp_true = lookupword ("TRUE", 4);
		pp_false = lookupword ("FALSE", 5);
		pp_not = lookupword ("NOT", 3);
		pp_and = lookupword ("AND", 3);
		pp_or = lookupword ("OR", 2);
		preproc_process_pending ();
		preproc_builtin ();
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void lexfinish */
PUBLIC void lexfinish (FILE * fptr)
{
	USE_VAR (fptr);		/* stop unused variable warning */
}
/*}}}*/
/*{{{  PUBLIC int lex_totallines */
PUBLIC int lex_totallines (void)
{
	return totallines;
}
/*}}}*/


