/* $Id: vti2.c,v 1.6 1997/04/30 11:19:55 jm40 Exp $ */

/*
 *	Virtual tree handler - pretty printer and node types
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
#include "midinc.h"
/*#include "usehdr.h"*//* PROC/FUNC usage info */
/*}}}*/

/*{{{  typedef valid_t*/
/* This is used to specify which keywords are valid for which languages */
typedef enum {
	NONE = 0,		/* not a keyword for any language */
	A = FE_LANG_ALL,	/* used by all `languages' */
	C = FE_LANG_CONFIG2 | FE_LANG_CONFIG3,	/* configurer */
	CC = FE_LANG_OCCAM | C,	/* compiler and configurer */
	HW = FE_LANG_NDL | FE_LANG_CONFIG2,	/* configurer (HW)and NDL reader */
	N = FE_LANG_NDL,	/* NDL reader only */
	CN = C | N		/* SET etc. */
} valid_t;
/*}}}*/

/*{{{  Keywords and all names for nodes and tags*/
#ifdef CAN_USE_INLINE
PUBLIC const vti_keyword_t vti_keyword_table[] =
#else
PRIVATE const vti_keyword_t vti_keyword_table[] =
#endif
{
	{000, NULL, NULL, NULL, NULL, NONODE, NONE},
	{S_ADD, NULL, "+", "ADD", "\"+\"", DOPNODE, NONE},
	{S_AFTER, "AFTER", NULL, NULL, NULL, DOPNODE, A},
	{S_ALT, "ALT", NULL, NULL, NULL, CNODE, CC},
	{S_AMPERSAND, NULL, "&", "AMPERSAND", "\"&\"", NONODE, NONE},
	{S_AND, "AND", NULL, NULL, NULL, DOPNODE, A},
	{S_ANY, "ANY", NULL, NULL, NULL, LEAFNODE, CC},
	{S_ASS, NULL, ":=", "ASSIGN", "\":=\"", ACTIONNODE, NONE},
	{S_AT, "AT", NULL, NULL, NULL, NONODE, CC},
	{S_BITAND, "BITAND", NULL, NULL, NULL, DOPNODE, A},
	{S_BITNOT, "BITNOT", NULL, NULL, NULL, MOPNODE, A},
	{S_BITOR, "BITOR", NULL, NULL, NULL, DOPNODE, A},
	{S_BOOL, "BOOL", NULL, NULL, NULL, LEAFNODE, A},
	{S_BOX, NULL, "[]", "BOX", "\"[]\"", NONODE, NONE},
	{S_BYTE, "BYTE", NULL, NULL, NULL, LEAFNODE, A},
#if 0
	{S_BYTELIT, NULL, "BYTE literal", "BYTELIT", "BYTE literal %s", LITNODE, NONE},
#else
	{15, NULL, NULL, NULL, NULL, NONODE, NONE},
#endif
	{S_CASE, "CASE", NULL, NULL, NULL, ACTIONNODE, CC},
	{S_CHAN, "CHAN", NULL, NULL, NULL, TYPENODE, CC},
	{S_COLON, NULL, ":", "COLON", "\":\"", NONODE, NONE},
	{S_COLON2, NULL, "::", "COLON2", "\"::\"", DOPNODE, NONE},
	{S_COMMA, NULL, ",", "COMMA", "\",\"", NONODE, NONE},
	{S_DIV, NULL, "/", "DIV", "\"/\"", DOPNODE, NONE},
	{S_ELSE, "ELSE", NULL, NULL, NULL, LEAFNODE, CC},
	{S_EQ, NULL, "=", "EQ", "\"=\"", DOPNODE, NONE},
	{S_FALSE, "FALSE", NULL, NULL, NULL, LEAFNODE, A},
	{S_FOR, "FOR", NULL, NULL, NULL, DOPNODE, A},
	{S_FROM, "FROM", NULL, NULL, NULL, NONODE, A},
	{S_FUNCTION, "FUNCTION", NULL, NULL, NULL, NONODE, CC},
	{S_GE, NULL, ">=", "GE", "\">=\"", DOPNODE, NONE},
	{S_GR, NULL, ">", "GR", "\">\"", DOPNODE, NONE},
	{S_IF, "IF", NULL, NULL, NULL, CNODE, A},
	{S_INPUT, NULL, "?", "INPUT", "\"?\"", ACTIONNODE, NONE},
	{S_INT, "INT", NULL, NULL, NULL, LEAFNODE, A},
	{S_INT16, "INT16", NULL, NULL, NULL, LEAFNODE, A},
	{S_INT32, "INT32", NULL, NULL, NULL, LEAFNODE, A},
	{S_INT64, "INT64", NULL, NULL, NULL, LEAFNODE, A},
#if 0
	{S_INTLIT, NULL, "INT literal", "INTLIT", "INT literal \"%s\"", LITNODE, NONE},
	{S_INT16LIT, NULL, "INT16 literal", "INT16LIT", "INT16 literal \"%s\"", LITNODE, NONE},
	{S_INT32LIT, NULL, "INT32 literal", "INT32LIT", "INT32 literal \"%s\"", LITNODE, NONE},
	{S_INT64LIT, NULL, "INT64 literal", "INT64LIT", "INT64 literal \"%s\"", LITNODE, NONE},
#else
	{36, NULL, NULL, NULL, NULL, NONODE, NONE},
	{37, NULL, NULL, NULL, NULL, NONODE, NONE},
	{38, NULL, NULL, NULL, NULL, NONODE, NONE},
	{39, NULL, NULL, NULL, NULL, NONODE, NONE},
#endif
	{S_IS, "IS", NULL, NULL, NULL, NONODE, A},
	{S_LBOX, NULL, "[", "LBOX", "\"[\"", NONODE, NONE},
	{S_LE, NULL, "<=", "LE", "\"<=\"", DOPNODE, NONE},
	{S_LPAREN, NULL, "(", "LPAREN", "\"(\"", NONODE, NONE},
	{S_LS, NULL, "<", "LS", "\"<\"", DOPNODE, NONE},
	{S_LSHIFT, NULL, "<<", "LSHIFT", "\"<<\"", DOPNODE, NONE},
	{S_MINUS, "MINUS", NULL, NULL, NULL, DOPNODE, A},
	{S_MOSTNEG, "MOSTNEG", NULL, NULL, NULL, MOPNODE, A},
	{S_MOSTPOS, "MOSTPOS", NULL, NULL, NULL, MOPNODE, A},
	{S_MULT, NULL, "*", "MULT", "\"*\"", DOPNODE, NONE},
	{S_NAME, NULL, "name", "NAME", "name \"%s\"", WORDNODE, NONE},
	{S_NE, NULL, "<>", "NE", "\"<>\"", DOPNODE, NONE},
	{S_NOT, "NOT", NULL, NULL, NULL, MOPNODE, A},
	{S_OF, "OF", NULL, NULL, NULL, NONODE, CC},
	{S_OR, "OR", NULL, NULL, NULL, DOPNODE, A},
	{S_OUTPUT, NULL, "!", "OUTPUT", "\"!\"", ACTIONNODE, NONE},
	{S_PAR, "PAR", NULL, NULL, NULL, CNODE, CC},
	{S_PLACE, "PLACE", NULL, NULL, NULL, DECLNODE, CC},
	{S_PLACED, "PLACED", NULL, NULL, NULL, NONODE, CC},
	{S_PLUS, "PLUS", NULL, NULL, NULL, DOPNODE, A},
	{S_PORT, "PORT", NULL, NULL, NULL, TYPENODE, CC},
	{S_PRI, "PRI", NULL, NULL, NULL, NONODE, CC},
	{S_PROC, "PROC", NULL, NULL, NULL, NONODE, A},
	{S_PROCESSOR, "PROCESSOR", NULL, NULL, NULL, PROCESSORNODE, C},
	{S_PROTOCOL, "PROTOCOL", NULL, NULL, NULL, NONODE, CC},
	{S_RBOX, NULL, "]", "RBOX", "\"]\"", NONODE, NONE},
	{S_REAL32, "REAL32", NULL, NULL, NULL, LEAFNODE, A},
	{S_REAL64, "REAL64", NULL, NULL, NULL, LEAFNODE, A},
	{S_UREALLIT, NULL, "real literal", "UREALLIT", "real literal \"%s\"", LITNODE, NONE},
#if 0
	{S_REAL32LIT, NULL, "REAL32 literal", "REAL32LIT", "REAL32 literal \"%s\"", LITNODE, NONE},
	{S_REAL64LIT, NULL, "REAL64 literal", "REAL64LIT", "REAL64 literal \"%s\"", LITNODE, NONE},
#else
	{69, NULL, NULL, NULL, NULL, NONODE, NONE},
	{70, NULL, NULL, NULL, NULL, NONODE, NONE},
#endif
	{S_REM, "REM", "\\", "REM", "\"\\\"", DOPNODE, A},
	{S_RESULT, "RESULT", NULL, NULL, NULL, NONODE, CC},
	{S_RETYPES, "RETYPES", NULL, NULL, NULL, NONODE, CC},
	{S_ROUND, "ROUND", NULL, NULL, NULL, MOPNODE, A},
	{S_RPAREN, NULL, ")", "RPAREN", "\")\"", NONODE, NONE},
	{S_RSHIFT, NULL, ">>", "RSHIFT", "\">>\"", DOPNODE, NONE},
	{S_SEMICOLON, NULL, ";", "SEMICOLON", "\";\"", NONODE, NONE},
	{S_SEQ, "SEQ", NULL, NULL, NULL, CNODE, CC},
	{S_SIZE, "SIZE", NULL, NULL, NULL, MOPNODE, A},
	{S_SKIP, "SKIP", NULL, NULL, NULL, LEAFNODE, A},
	{S_STOP, "STOP", NULL, NULL, NULL, LEAFNODE, A},
	{S_STRING, NULL, "string", "STRING", "string \"%s\"", CONSTTABLENODE, NONE},
	{S_SUBTRACT, NULL, "-", "SUBTRACT", "\"-\"", DOPNODE, NONE},
	{S_TIMER, "TIMER", NULL, NULL, NULL, LEAFNODE, CC},
	{S_TIMES, "TIMES", NULL, NULL, NULL, DOPNODE, A},
	{S_TRUE, "TRUE", NULL, NULL, NULL, LEAFNODE, A},
	{S_TRUNC, "TRUNC", NULL, NULL, NULL, MOPNODE, A},
	{S_VAL, "VAL", NULL, NULL, NULL, NONODE, A},
	{S_VALOF, "VALOF", NULL, NULL, NULL, VALOFNODE, CC},
	{S_WHILE, "WHILE", NULL, NULL, NULL, CONDNODE, CC},
	{S_XOR, NULL, "><", "XOR", "\"><\"", DOPNODE, NONE},

	{S_NEWLINE, NULL, "new line", NULL, "end of line", NONODE, NONE},
	{S_END, NULL, "end of file", "END", NULL, LEAFNODE, NONE},
	{S_COMMENT, NULL, "comment", NULL, NULL, NONODE, NONE},

	{S_UINTLIT, NULL, "integer literal", "UINTLIT", "integer literal \"%s\"", LITNODE, NONE},
	{S_STRINGCONT, NULL, NULL, "STRINGCONT", NULL, NONODE, NONE},
	{S_UBYTELIT, NULL, "byte literal", "UBYTELIT", "byte literal %s", LITNODE, NONE},

	{S_ILLEGALSYMB, NULL, NULL, "ILLEGALSYMB", NULL, NONODE, NONE},
	{S_WORKSPACE, "WORKSPACE", NULL, NULL, NULL, NONODE, CC},
	{S_VECSPACE, "VECSPACE", NULL, NULL, NULL, NONODE, CC},
	{S_IN, "IN", NULL, NULL, NULL, NONODE, CC},

	{S_SC, NULL, "#SC", NULL, NULL, NONODE, NONE},
	{S_INCLUDE, NULL, "#INCLUDE", NULL, NULL, NONODE, NONE},
	{S_USE, NULL, "#USE", NULL, NULL, NONODE, NONE},
	{S_INLINE, "INLINE", NULL, NULL, NULL, NONODE, CC},
	{S_GUY, "GUY", NULL, NULL, NULL, CNODE, CC},
	{S_DOT, ".", NULL, NULL, "\".\"", NONODE, CC},
	{S_STEP, "STEP", NULL, NULL, NULL, REPLCNODE, CC},
	{S_IMPORT, NULL, "#IMPORT", NULL, NULL, NONODE, NONE},
	{S_OPTION, NULL, "#OPTION", NULL, NULL, NONODE, NONE},
	{S_HCOMMENT, NULL, "#COMMENT", NULL, NULL, NONODE, NONE},
	{S_HNETWORK, NULL, "#NETWORK", NULL, NULL, NONODE, NONE},
	{S_ARRAYCONSTRUCTOR, NULL, NULL, "ARRAYCONSTRUCTOR", NULL, REPLCNODE, NONE},

#ifdef CONDEXP
	/* not defined anymore -- frmb */
	{S_RIGHTARROW, NULL, "->", "RIGHTARROW", "\"->\"", NONODE, NONE},
#endif

#if 1				/*def CONFIG */
	{S_SET, "SET", NULL, NULL, NULL, CONFIGNODE, CN},
	{S_ARC, "ARC", NULL, NULL, NULL, LEAFNODE, HW},
	{S_EDGE, "EDGE", NULL, NULL, NULL, LEAFNODE, HW},
	{S_NODE, "NODE", NULL, NULL, NULL, LEAFNODE, CN},
	{S_CONFIG, "CONFIG", NULL, NULL, NULL, DECLNODE, C},
	{S_NETWORK, "NETWORK", NULL, NULL, NULL, DECLNODE, HW},
	{N_CONFIG, NULL, NULL, "N_CONFIG", NULL, NAMENODE, C},
	{N_NETWORK, NULL, NULL, "N_NETWORK", NULL, NAMENODE, HW},
	{S_CONNECT, "CONNECT", NULL, NULL, NULL, CONFIGNODE, HW},
	{S_TO, "TO", NULL, NULL, NULL, NONODE, HW},
	{S_WITH, "WITH", NULL, NULL, NULL, NONODE, HW},
	{S_MAPPING, "MAPPING", NULL, NULL, NULL, DECLNODE, C},
	{N_MAPPING, NULL, NULL, "N_MAPPING", NULL, NAMENODE, C},
	{S_MAP, "MAP", NULL, NULL, NULL, CONFIGNODE, CN},
	{S_ON, "ON", NULL, NULL, NULL, NONODE, C},
	{S_ONTO, "ONTO", NULL, NULL, NULL, NONODE, CN},
#else
	{114, NULL, NULL, NULL, NULL, NONODE, NONE},
	{115, NULL, NULL, NULL, NULL, NONODE, NONE},
	{116, NULL, NULL, NULL, NULL, NONODE, NONE},
	{117, NULL, NULL, NULL, NULL, NONODE, NONE},
	{118, NULL, NULL, NULL, NULL, NONODE, NONE},
	{119, NULL, NULL, NULL, NULL, NONODE, NONE},
	{120, NULL, NULL, NULL, NULL, NONODE, NONE},
	{121, NULL, NULL, NULL, NULL, NONODE, NONE},
	{122, NULL, NULL, NULL, NULL, NONODE, NONE},
	{123, NULL, NULL, NULL, NULL, NONODE, NONE},
	{124, NULL, NULL, NULL, NULL, NONODE, NONE},
	{125, NULL, NULL, NULL, NULL, NONODE, NONE},
	{126, NULL, NULL, NULL, NULL, NONODE, NONE},
	{127, NULL, NULL, NULL, NULL, NONODE, NONE},
	{128, NULL, NULL, NULL, NULL, NONODE, NONE},
	{129, NULL, NULL, NULL, NULL, NONODE, NONE},
#endif

	{S_LIST, NULL, "LIST", "LIST", NULL, LISTNODE, NONE},
	{S_DELAYED_INPUT, NULL, NULL, "DELAYED_INPUT", NULL, ACTIONNODE, NONE},
	{S_CASE_INPUT, NULL, NULL, "CASE_INPUT", NULL, ACTIONNODE, NONE},
	{S_VALABBR, NULL, NULL, "VALABBR", NULL, DECLNODE, NONE},
	{S_VALRETYPE, NULL, NULL, "VALRETYPE", NULL, DECLNODE, NONE},
	{S_DECL, NULL, "DECL", "DECL", NULL, DECLNODE, NONE},
	{S_ABBR, NULL, "ABBR", "ABBR", NULL, DECLNODE, NONE},
	{S_RETYPE, NULL, NULL, "RETYPE", NULL, DECLNODE, NONE},
	{S_TPROTDEF, NULL, NULL, "TPROTDEF", NULL, DECLNODE, NONE},
	{S_SPROTDEF, NULL, NULL, "SPROTDEF", NULL, DECLNODE, NONE},
	{S_PROCDEF, NULL, NULL, "PROCDEF", NULL, DECLNODE, NONE},
	{S_SFUNCDEF, NULL, NULL, "SFUNCDEF", NULL, DECLNODE, NONE},
	{S_LFUNCDEF, NULL, NULL, "LFUNCDEF", NULL, DECLNODE, NONE},
	{S_ARRAYSUB, NULL, NULL, "ARRAYSUB", NULL, ARRAYSUBNODE, NONE},
	{S_NEG, NULL, "-", "NEG", NULL, MOPNODE, NONE},

	{145, NULL, NULL, NULL, NULL, NONODE, NONE},

	{S_EXACT, NULL, NULL, "EXACT", NULL, MOPNODE, NONE},
	{S_SEGMENT, NULL, NULL, "SEGMENT", NULL, SEGMENTNODE, NONE},
	{S_VARIANT, NULL, "variant", "VARIANT", NULL, CONDNODE, NONE},
	{S_ARRAY, NULL, "ARRAY", NULL, NULL, TYPENODE, NONE},
	{S_FNTYPE, NULL, NULL, "FNTYPE", NULL, LISTNODE, NONE},

	{N_VALABBR, NULL, "VAL abbreviation", "N_VALABBR", NULL, NAMENODE, NONE},
	{N_VALRETYPE, NULL, "VAL retype", "N_VALRETYPE", NULL, NAMENODE, NONE},
	{N_ABBR, NULL, "abbreviation", "N_ABBR", NULL, NAMENODE, NONE},
	{N_RETYPE, NULL, "retype", "N_RETYPE", NULL, NAMENODE, NONE},
	{N_DECL, NULL, "declaration", "N_DECL", NULL, NAMENODE, NONE},
	{N_TAGDEF, NULL, "tag", "N_TAGDEF", NULL, NAMENODE, NONE},
	{N_SPROTDEF, NULL, "protocol", "N_SPROTDEF", NULL, NAMENODE, NONE},
	{N_TPROTDEF, NULL, "protocol", "N_TPROTDEF", NULL, NAMENODE, NONE},
	{N_PROCDEF, NULL, "procedure", "N_PROCDEF", NULL, NAMENODE, NONE},
	{N_SFUNCDEF, NULL, "function", "N_SFUNCDEF", NULL, NAMENODE, NONE},
	{N_LFUNCDEF, NULL, "function", "N_LFUNCDEF", NULL, NAMENODE, NONE},
	{N_VALPARAM, NULL, "VAL parameter", "N_VALPARAM", NULL, NAMENODE, NONE},
	{N_REPL, NULL, "replicator", "N_REPL", NULL, NAMENODE, NONE},

	{S_ALTERNATIVE, NULL, NULL, "ALTERNATIVE", NULL, ALTNODE, NONE},
	{S_CONSTRUCTOR, NULL, NULL, "CONSTRUCTOR", NULL, LITNODE, NONE},
	{N_PARAM, NULL, "parameter", "N_PARAM", NULL, NAMENODE, NONE},
	{S_CONSTEXP, NULL, NULL, "CONSTEXP", NULL, CONSTEXPNODE, NONE},

	{168, NULL, NULL, NULL, NULL, NONODE, NONE},

	{S_UNDECLARED, NULL, NULL, "UNDECLARED", NULL, LEAFNODE, NONE},
	{S_WSPLACE, NULL, NULL, "WSPLACE", NULL, DECLNODE, NONE},
	{S_VSPLACE, NULL, NULL, "VSPLACE", NULL, DECLNODE, NONE},
	{N_SCPROCDEF, NULL, NULL, "N_SCPROCDEF", NULL, NAMENODE, NONE},
	{N_SCFUNCDEF, NULL, NULL, "N_SCFUNCDEF", NULL, NAMENODE, NONE},
	{N_LIBPROCDEF, NULL, NULL, "N_LIBPROCDEF", NULL, NAMENODE, NONE},
	{N_LIBFUNCDEF, NULL, NULL, "N_LIBFUNCDEF", NULL, NAMENODE, NONE},
	{N_PREDEFFUNCTION, NULL, NULL, "N_PREDEFFUNCTION", NULL, NAMENODE, NONE},
	{N_PREDEFPROC, NULL, NULL, "N_PREDEFPROC", NULL, NAMENODE, NONE},
	{N_STDLIBPROCDEF, NULL, NULL, "N_STDLIBPROCDEF", NULL, NAMENODE, NONE},
	{N_STDLIBFUNCDEF, NULL, NULL, "N_STDLIBFUNCDEF", NULL, NAMENODE, NONE},
	{N_LABELDEF, NULL, NULL, "N_LABELDEF", NULL, NAMENODE, NONE},
	{S_LABELDEF, NULL, NULL, "LABELDEF", NULL, DECLNODE, NONE},

	{S_UMINUS, NULL, "MINUS", "UMINUS", NULL, MOPNODE, NONE},
	{S_GUYCODE, NULL, NULL, "GUYCODE", NULL, DOPNODE, NONE},
	{S_GUYSTEP, NULL, NULL, "GUYSTEP", NULL, DOPNODE, NONE},
	{S_LABEL, NULL, NULL, "LABEL", NULL, LEAFNODE, NONE},
	{S_PRIPAR, NULL, "PRI PAR", "PRIPAR", NULL, CNODE, NONE},
	{S_PRIREPLPAR, NULL, "Replicated PRI PAR", "PRIREPLPAR", NULL, REPLCNODE, NONE},
	{S_PRIALT, NULL, NULL, "PRIALT", NULL, CNODE, NONE},
	{S_PRIREPLALT, NULL, NULL, "PRIREPLALT", NULL, REPLCNODE, NONE},
	{S_PLACEDPAR, NULL, "PLACED PAR", "PLACEDPAR", NULL, CNODE, NONE},
	{S_PLACEDREPLPAR, NULL, "Replicated PLACED PAR", "PLACEDREPLPAR", NULL, REPLCNODE, NONE},
	{S_TAGGED_INPUT, NULL, NULL, "TAGGED_INPUT", NULL, ACTIONNODE, NONE},
	{S_REPLSEQ, NULL, NULL, "REPLSEQ", NULL, REPLCNODE, NONE},
	{S_REPLPAR, NULL, "Replicated PAR", "REPLPAR", NULL, REPLCNODE, NONE},
	{S_REPLIF, NULL, NULL, "REPLIF", NULL, REPLCNODE, NONE},
	{S_REPLALT, NULL, NULL, "REPLALT", NULL, REPLCNODE, NONE},
	{S_CHOICE, NULL, NULL, "CHOICE", NULL, CONDNODE, NONE},
	{S_SELECTION, NULL, NULL, "SELECTION", NULL, CONDNODE, NONE},
	{S_FINSTANCE, NULL, "FINSTANCE", "FINSTANCE", NULL, INSTANCENODE, NONE},
	{S_PINSTANCE, NULL, "PINSTANCE", "PINSTANCE", NULL, INSTANCENODE, NONE},

#ifdef CONDEXP
	{S_CONDEXP, NULL, "conditional expression", "CONDEXP", NULL, CONDEXPNODE, NONE},
#else
	{201, NULL, NULL, NULL, NULL, NONODE, NONE},
#endif
	{202, NULL, NULL, NULL, NULL, NONODE, NONE},

	{N_INLINEPROCDEF, NULL, NULL, "N_INLINEPROCDEF", NULL, NAMENODE, NONE},
	{N_INLINEFUNCDEF, NULL, NULL, "N_INLINEFUNCDEF", NULL, NAMENODE, NONE},

	{S_RECORDSUB, NULL, NULL, "RECORDSUB", NULL, ARRAYSUBNODE, NONE},
	{N_FIELD, NULL, NULL, "N_FIELD", NULL, NAMENODE, NONE},

#if 1				/*def CONFIG */
	{S_DO, "DO", NULL, NULL, NULL, CNODE, CN},
	{S_REPLDO, NULL, NULL, "REPLDO", NULL, REPLCNODE, NONE},
	{S_PLACEON, NULL, NULL, "PLACEON", NULL, DECLNODE, NONE},
#else
	{207, NULL, NULL, NULL, NULL, NONODE, NONE},
	{208, NULL, NULL, NULL, NULL, NONODE, NONE},
	{209, NULL, NULL, NULL, NULL, NONODE, NONE},
#endif

	{S_UNKNOWN, NULL, "unknown.type", "UNKNOWN", NULL, LEAFNODE, NONE},
	{S_CONSTCONSTRUCTOR, NULL, NULL, "CONSTCONSTRUCTOR", NULL, CONSTTABLENODE, NONE},

	{212, NULL, NULL, NULL, NULL, NONODE, NONE},

	{S_CONSTPTR, NULL, NULL, "CONSTPTR", NULL, CONSTTABLENODE, NONE},	/* used in constant folding */
	{S_CSUB0, NULL, "csub0", "CSUB0", NULL, DOPNODE, NONE},
	{T_TEMP, NULL, "$temp", "T_TEMP", NULL, NAMENODE, NONE},

	{S_DUMMYEXP, NULL, "dummy expression", "DUMMYEXP", NULL, LEAFNODE, NONE},
	{S_ARRAYITEM, NULL, NULL, "ARRAYITEM", NULL, ARRAYSUBNODE, NONE},
	{S_SEGMENTITEM, NULL, NULL, "SEGMENTITEM", NULL, SEGMENTNODE, NONE},
	{T_PREEVALTEMP, NULL, "$preevaltemp", "T_PREEVALTEMP", NULL, NAMENODE, NONE},
	{S_CCNT1, NULL, "ccnt1", "CCNT1", NULL, DOPNODE, NONE},
	{S_FNFORMALRESULT, NULL, "$formal.result", "FNFORMALRESULT", NULL, NAMENODE, NONE},
	{S_FNACTUALRESULT, NULL, "$actual.result", "FNACTUALRESULT", NULL, NAMENODE, NONE},
	{S_ELSIZE, NULL, "ELSIZE", NULL, NULL, MOPNODE, NONE},
	{S_SEGSTART, NULL, "SEGSTART", NULL, NULL, MOPNODE, NONE},
	{S_HIDDEN_PARAM, NULL, NULL, "HIDDENPARAM", NULL, NAMENODE, NONE},
	{S_PARAM_STATICLINK, NULL, NULL, "STATICLINK", NULL, /*LEAFNODE*/ NAMENODE, NONE},
	{S_PARAM_VSP, NULL, NULL, "VSP", NULL, /*LEAFNODE*/ NAMENODE, NONE},
	{S_EVAL, NULL, "EVAL", NULL, NULL, DOPNODE, NONE},
	{S_OVERLAPCHECK, NULL, NULL, "OVERLAPCHECK", NULL, DOPNODE, NONE},

	{230, NULL, NULL, NULL, NULL, NONODE, NONE},

	{S_ADDRESSOF, NULL, "ADDRESSOF", NULL, NULL, MOPNODE, NONE},

	{232, NULL, NULL, NULL, NULL, NONODE, NONE},
	{233, NULL, NULL, NULL, NULL, NONODE, NONE},
	{234, NULL, NULL, NULL, NULL, NONODE, NONE},
	{235, NULL, NULL, NULL, NULL, NONODE, NONE},

	{S_SPACEUSAGE, NULL, NULL, "SPACEUSAGE", NULL, SPACENODE, NONE},
	{T_REGTEMP, NULL, NULL, "T_REGTEMP", NULL, NAMENODE, NONE},
	{T_RESERVEDWS, NULL, NULL, "T_RESERVEDWS", NULL, NAMENODE, NONE},
#if 0
	{S_BASICBLOCK, NULL, NULL, "BASICBLOCK", NULL, CNODE, NONE},
#else
	{239, NULL, NULL, NULL, NULL, NONODE, NONE},
#endif

#if 0				/* bug 829 19/9/91 */
	{S_PRAGMA, NULL, "#PRAGMA", NULL, NULL, NONODE, NONE},
#else
	{S_PRAGMA, NULL, "#PRAGMA", NULL, NULL, DECLNODE, NONE},
#endif
	{S_ASM, "ASM", NULL, NULL, NULL, CNODE, CC},
	{S_ASMNAME, NULL, "asmname", "ASMNAME", NULL, WORDNODE, NONE},

	{S_CONTROLPORT, "CONTROLPORT", NULL, NULL, NULL, LEAFNODE, N},
	{S_ROUTE, "ROUTE", NULL, NULL, NULL, LEAFNODE, N},
#ifdef OCCAM2_5
	{S_DATA, "DATA", NULL, NULL, NULL, NONODE, CC},
	{S_TYPE, "TYPE", NULL, NULL, NULL, NONODE, CC},
	{S_RECORD, "RECORD", NULL, NULL, NULL, TYPENODE, CC},
	{S_TYPEDECL, NULL, NULL, "TYPEDECL", NULL, DECLNODE, NONE},
	{N_TYPEDECL, NULL, "typedeclaration", "N_TYPEDECL", NULL, NAMENODE, NONE},
	{S_RECORDITEM, NULL, NULL, "RECORDITEM", NULL, ARRAYSUBNODE, NONE},
	{S_PACKED, "PACKED", NULL, NULL, NULL, NONODE, CC},
	{S_BYTESIN, "BYTESIN", NULL, NULL, NULL, MOPNODE, CC},
	{S_OFFSETOF, "OFFSETOF", NULL, NULL, NULL, DOPNODE, CC},
	{S_RESHAPES, "RESHAPES", NULL, NULL, NULL, NONODE, CC}
#endif
#ifdef USER_DEFINED_OPERATORS
	,
/* Note: operators containing dollars are removed for now - may clash with $ */
/* used for hex */
/* New operator symbols - the following are for diadic versions... */
	{255, NULL, NULL, NULL, NULL, NONODE, NONE},
	{S_DQUESTIONM, NULL, "??", "DBLQUEST", "\"??\"", DOPNODE, NONE},
	{S_DAMPERSAT, NULL, "@@", "DAMPERSAT", "\"@@\"", DOPNODE, NONE},
	{S_DDOLLAR, NULL, "$$", "DDOLLAR", "\"$$\"", DOPNODE, NONE},
/* percent and double percent cannot display symbols in strings because */
/* of usage of %d or %s for fields (%% does not work with routine) */
	{S_PERCENT, NULL, "%", "PERCENT", "\"%%\"", DOPNODE, NONE},
	{S_DPERCENT, NULL, "%%", "DPERCENT", "\"%%%%\"", DOPNODE, NONE},
	{S_DAMPERSAND, NULL, "&&", "DAMPERSAND", "\"&&\"", DOPNODE, NONE},
#if 0
	{S_LEFTDOLLAR, NULL, "<$", "LDOLLAR", "\"<$\"", DOPNODE, NONE},
	{S_DOLLARRIGHT, NULL, "$>", "DOLLARR", "\"$>\"", DOPNODE, NONE},
#else
	{262, NULL, NULL, NULL, NULL, NONODE, NONE},
	{263, NULL, NULL, NULL, NULL, NONODE, NONE},
#endif
	{S_LEFTPERCENT, NULL, "<%", "LPERCENT", "\"<%\"", DOPNODE, NONE},
	{S_PERCENTRIGHT, NULL, "%>", "PERCENTR", "\"%>\"", DOPNODE, NONE},
	{S_LEFTAMPERSAND, NULL, "<&", "LAMPERSAND", "\"<&\"", DOPNODE, NONE},
	{S_AMPERSANDRIGHT, NULL, "&>", "AMPERSANDR", "\"&>\"", DOPNODE, NONE},
	{S_DPLUS, NULL, "++", "DPLUS", "\"++\"", DOPNODE, NONE},
#if 0
	{S_DMINUS, NULL, "--", "DMINUS", "\"--\"", DOPNODE, NONE},
#else
	{269, NULL, NULL, NULL, NULL, NONODE, NONE},
#endif
	{S_HAT, NULL, "^", "HAT", "\"^\"", DOPNODE, NONE},
	{S_DEXCLAIMATION, NULL, "!!", "DEXCLAIM", "\"!!\"", DOPNODE, NONE},
	{S_DEQUALS, NULL, "==", "DEQUALS", "\"==\"", DOPNODE, NONE},
	{S_LEFTAMPERSAT, NULL, "<@", "LAMPERSAT", "\"<@\"", DOPNODE, NONE},
	{S_AMPERSATRIGHT, NULL, "@>", "AMPERSATR", "\"@>\"", DOPNODE, NONE},
#endif
	{S_AMPERSAT, NULL, "@", "AMPERSAT", "\"@\"", DOPNODE, NONE},
#ifdef USER_DEFINED_OPERATORS
#if 0
	{S_DOLLAR, NULL, "$", "DOLLAR", "\"$\"", DOPNODE, NONE},
#else
	{276, NULL, NULL, NULL, NULL, NONODE, NONE},
#endif
	{S_ROTATELEFT, NULL, "<]", "ROTL", "\"<]\"", DOPNODE, NONE},
	{S_ROTATERIGHT, NULL, "[>", "ROTR", "\"[>\"", DOPNODE, NONE},

/* ... and these are for monadic operators */
	{S_M_DQUESTIONM, NULL, "??", "M_DBLQUEST", "\"??\"", MOPNODE, NONE},
	{S_M_DAMPERSAT, NULL, "@@", "M_DAMPERSAT", "\"@@\"", MOPNODE, NONE},
	{S_M_DDOLLAR, NULL, "$$", "M_DDOLLAR", "\"$$\"", MOPNODE, NONE},
	{S_M_PERCENT, NULL, "%", "M_PERCENT", "\"%%\"", MOPNODE, NONE},
	{S_M_DPERCENT, NULL, "%%", "M_DPERCENT", "\"%%%%\"", MOPNODE, NONE},
	{S_M_DAMPERSAND, NULL, "&&", "M_DAMPERSAND", "\"&&\"", MOPNODE, NONE},
#if 0
	{S_M_LEFTDOLLAR, NULL, "<$", "LDOLLAR", "\"<$\"", MOPNODE, NONE},
	{S_M_DOLLARRIGHT, NULL, "$>", "DOLLARR", "\"$>\"", MOPNODE, NONE},
#else
	{285, NULL, NULL, NULL, NULL, NONODE, NONE},
	{286, NULL, NULL, NULL, NULL, NONODE, NONE},
#endif
	{S_M_LEFTPERCENT, NULL, "<%", "M_LPERCENT", "\"<%\"", MOPNODE, NONE},
	{S_M_PERCENTRIGHT, NULL, "%>", "M_PERCENTR", "\"%>\"", MOPNODE, NONE},
	{S_M_LEFTAMPERSAND, NULL, "<&", "M_LAMPERSAND", "\"<&\"", MOPNODE, NONE},
	{S_M_AMPERSANDRIGHT, NULL, "&>", "M_AMPERSANDR", "\"&>\"", MOPNODE, NONE},
	{S_M_DPLUS, NULL, "++", "M_DPLUS", "\"++\"", MOPNODE, NONE},
#if 0
	{S_M_DMINUS, NULL, "--", "DMINUS", "\"--\"", MOPNODE, NONE},
#else
	{292, NULL, NULL, NULL, NULL, NONODE, NONE},
#endif
	{S_M_HAT, NULL, "^", "M_HAT", "\"^\"", MOPNODE, NONE},
	{S_M_DEXCLAIMATION, NULL, "!!", "M_DEXCLAIM", "\"!!\"", MOPNODE, NONE},
	{S_M_DEQUALS, NULL, "==", "M_DEQUALS", "\"==\"", MOPNODE, NONE},
	{S_M_LEFTAMPERSAT, NULL, "<@", "M_LAMPERSAT", "\"<@\"", MOPNODE, NONE},
	{S_M_AMPERSATRIGHT, NULL, "@>", "M_AMPERSATR", "\"@>\"", MOPNODE, NONE},
	{S_M_AMPERSAT, NULL, "@", "M_AMPERSAT", "\"@\"", MOPNODE, NONE},
#if 0
	{S_M_DOLLAR, NULL, "$", "DOLLAR", "\"$\"", MOPNODE, NONE},
#else
	{299, NULL, NULL, NULL, NULL, NONODE, NONE},
#endif
	{S_M_ROTATELEFT, NULL, "<]", "M_ROTL", "\"<]\"", MOPNODE, NONE},
	{S_M_ROTATERIGHT, NULL, "[>", "M_ROTR", "\"[>\"", MOPNODE, NONE}
#endif

#ifdef INITIAL_DECL
	, {S_INITIAL, "INITIAL", NULL, NULL, NULL, NONODE, A}
#else
	, {302, NULL, NULL, NULL, NONODE, NONE}
#endif
	, {303, NULL, NULL, NULL, NONODE, NONE}
	, {304, NULL, NULL, NULL, NONODE, NONE}
	, {305, NULL, NULL, NULL, NONODE, NONE}
#ifdef MOBILES
	, {S_MOBILE, "MOBILE", NULL, NULL, NULL, TYPENODE, CC}
	, {S_CLONE, "CLONE", NULL, NULL, NULL, MOPNODE, CC}
	, {S_PARAM_MSP, NULL, NULL, "MSP", NULL, /*LEAFNODE*/ NAMENODE, NONE}
	, {S_NEW_ARRAY, NULL, NULL, "NEWARRAY", NULL, TYPENODE, NONE}
	, {S_UNDEFINED, NULL, NULL, "UNDEFINED", NULL, MOPNODE, NONE}
#else
	, {306, NULL, NULL, NULL, NONODE, NONE}
	, {307, NULL, NULL, NULL, NONODE, NONE}
	, {308, NULL, NULL, NULL, NONODE, NONE}
	, {309, NULL, NULL, NULL, NONODE, NONE}
	, {310, NULL, NULL, NULL, NONODE, NONE}
#endif
	, {311, NULL, NULL, NULL, NONODE, NONE}
	, {S_BAR, NULL, "|", "BAR", "\"|\"", NONODE, NONE}
	, {N_RESULTPARAM, NULL, "RESULT parameter", "N_RESULTPARAM", NULL, NAMENODE, NONE}
	, {S_RECURSIVE, "RECURSIVE", NULL, NULL, NULL, NONODE, CC}
	, {S_REC, "REC", NULL, NULL, NULL, NONODE, CC}
	, {S_ASINPUT, NULL, NULL, "\"?\"", NULL, MOPNODE, NONE}
	, {S_ASOUTPUT, NULL, NULL, "\"!\"", NULL, MOPNODE, NONE}
	/* extended input/rendezvous stuff */
	, {S_X_INPUT, NULL, NULL, "X_INPUT", NULL, ACTIONNODE, NONE}
	, {S_X_TAGGED_INPUT, NULL, NULL, "X_TAGGED_INPUT", NULL, ACTIONNODE, NONE}
	, {S_X_CASE_INPUT, NULL, NULL, "X_CASE_INPUT", NULL, ACTIONNODE, NONE}
	, {S_X_VARIANT, NULL, "extended variant", "X_VARIANT", NULL, CONDNODE, NONE}
#ifdef USER_DEFINED_OPERATORS
	, {322, NULL, NULL, NULL, NONODE, NONE}
#else	/* !USER_DEFINED_OPERATORS */
	, {S_DQUESTIONM, NULL, "??", "DBLQUEST", "\"??\"", DOPNODE, NONE}
#endif	/* !USER_DEFINED_OPERATORS */
	, {S_X_FIRST_HALF, NULL, NULL, "FIRST_HALF", NULL, MOPNODE, NONE}
	, {S_X_SECOND_HALF, NULL, NULL, "SECOND_HALF", NULL, MOPNODE, NONE}
	, {S_SCALAR, NULL, NULL, "SCALAR", NULL, DECLNODE, NONE}
	, {N_SCALAR, NULL, "scalar", "N_SCALAR", NULL, NAMENODE, NONE}
	, {S_SLEEP, "SLEEP", NULL, NULL, NULL, NONODE, NONE}
#ifdef MOBILES
	, {S_NTH_DIMENSION, NULL, NULL, "NTH_DIMENSION", NULL, DOPNODE, NONE}
#else	/* !MOBILES */
	, {328, NULL, NULL, NULL, NONODE, NONE}
#endif	/* !MOBILES */
	, {S_FORKING, "FORKING", NULL, NULL, NULL, CNODE, CC}
	, {S_FORK, "FORK", NULL, NULL, NULL, NONODE, CC}
	, {S_BARRIER, "BARRIER", NULL, NULL, NULL, LEAFNODE, CC}
	, {S_SHARED, "SHARED", NULL, NULL, NULL, NONODE, CC}
	, {S_CLAIM, "CLAIM", NULL, NULL, NULL, CNODE, CC}
	, {S_UPLUS, NULL, "PLUS", "UPLUS", NULL, MOPNODE, NONE}
	, {S_ANONCHANTYPE, NULL, NULL, "ANONCHANTYPE", NULL, TYPENODE, NONE}
	, {S_PREPROC, NULL, "#PREPROC", NULL, NULL, NONODE, NONE}
	, {S_X_INPUT_OUTPUT, NULL, NULL, "X_INPUT_OUTPUT", NULL, ACTIONNODE, NONE}
	, {S_NULLARRAY, NULL, "[]", "NULLARRAY", "\"[]\"", LEAFNODE, NONE}
#ifdef MOBILES
	, {S_DEFINED, "DEFINED", NULL, NULL, NULL, MOPNODE, CC}
#else	/* !MOBILES */
	, {339, NULL, NULL, NULL, NONODE, NONE}
#endif	/* !MOBILES */
	, {S_UDVCONSTEXP, NULL, NULL, "UDVCONSTEXP", NULL, CONSTEXPNODE, NONE}
	, {S_FORWDECL, NULL, NULL, "FORWDECL", NULL, DECLNODE, NONE}
	, {S_EXTENDS, "EXTENDS", NULL, NULL, NULL, MOPNODE, CC}				/* EXTENDS also used for PAR barrier extension */
#ifdef MOBILES
	, {S_PROCTYPEDECL, NULL, NULL, "PROCTYPEDECL", NULL, DECLNODE, NONE}
	, {N_PROCTYPEDECL, NULL, "proctypedeclaration", "N_PROCTYPEDECL", NULL, NAMENODE, NONE}
	, {S_MPROCDECL, NULL, NULL, "MPROCDECL", NULL, DECLNODE, NONE}
	, {N_MPROCDECL, NULL, "mobileproctypedeclaration", "N_MPROCDECL", NULL, NAMENODE, NONE}
	, {S_ALLOC_PROC, NULL, NULL, "ALLOC_PROC", NULL, TYPENODE, NONE}
	, {S_IMPLEMENTS, "IMPLEMENTS", NULL, NULL, NULL, NONODE, CC}
	, {S_PARAM_MPP, NULL, NULL, "MPP", NULL, /*LEAFNODE*/ NAMENODE, NONE}
	, {S_SUSPEND, "SUSPEND", NULL, NULL, NULL, LEAFNODE, A}
	, {S_PARAM_FB, NULL, NULL, "FB", NULL, /*LEAFNODE*/ NAMENODE, NONE}
	, {S_TRACES, "TRACES", NULL, NULL, NULL, NONODE, CC}
	, {S_SEQCOMP, NULL, "->", "SEQCOMP", "\"->\"", NONODE, NONE}
	, {S_PARCOMP, NULL, "||", "PARCOMP", "\"||\"", NONODE, NONE}
#else	/* !MOBILES */
	, {343, NULL, NULL, NULL, NONODE, NONE}
	, {344, NULL, NULL, NULL, NONODE, NONE}
	, {345, NULL, NULL, NULL, NONODE, NONE}
	, {346, NULL, NULL, NULL, NONODE, NONE}
	, {347, NULL, NULL, NULL, NONODE, NONE}
	, {348, NULL, NULL, NULL, NONODE, NONE}
	, {349, NULL, NULL, NULL, NONODE, NONE}
	, {350, NULL, NULL, NULL, NONODE, NONE}
	, {351, NULL, NULL, NULL, NONODE, NONE}
	, {352, NULL, NULL, NULL, NONODE, NONE}
	, {353, NULL, NULL, NULL, NONODE, NONE}
	, {354, NULL, NULL, NULL, NONODE, NONE}
#endif	/* !MOBILES */
	, {N_LIBMPROCDECL, NULL, NULL, "N_LIBMPROCDECL", NULL, NAMENODE, NONE}
	, {S_FIXED, "FIXED", NULL, NULL, NULL, NONODE, CC}
	, {S_ANYCHANTYPE, "MOBILE.CHAN", NULL, "ANYCHANTYPE", NULL, LEAFNODE, CC}
	, {S_SYNC, "SYNC", NULL, NULL, NULL, LEAFNODE, CC}
	, {S_RESIGN, "RESIGN", NULL, NULL, NULL, CNODE, CC}
	, {S_FULLBARRIER, NULL, "FULLBARRIER", NULL, NULL, LEAFNODE, NONE}
	, {S_BAREXTEND, NULL, "BARRIER.EXTEND", NULL, NULL, DOPNODE, NONE}
	, {S_NEW_BARRIER, NULL, NULL, "NEW_BARRIER", NULL, TYPENODE, NONE}
	, {S_HIDDEN_TYPE, NULL, NULL, "HIDDENTYPE", NULL, NAMENODE, NONE}
	, {S_TYPEHASHOF, "TYPEHASHOF", NULL, "TYPEHASHOF", NULL, MOPNODE, CC}
	, {S_ANYPROCTYPE, "MOBILE.PROC", NULL, "ANYPROCTYPE", NULL, LEAFNODE, CC}
	, {S_ENROLL, "ENROLL", NULL, "ENROLL", NULL, NONODE, CC}
	, {S_BUFFERED, "BUFFERED", NULL, "BUFFERED", NULL, TYPENODE, CC}
	, {S_PARAM_WS, NULL, NULL, "WS.SIZE", NULL, /*LEAFNODE*/ NAMENODE, NONE}
	, {S_ALIGNMENT, "ALIGNED", NULL, "ALIGNED", NULL, TYPENODE, CC}
	, {S_DMA, "DMA", NULL, "DMA", NULL, TYPENODE, CC}
	, {S_EMPTY, "EMPTY", NULL, "EMPTY", NULL, TYPENODE, CC}
	, {S_ADDROF, "ADDROF", NULL, NULL, NULL, MOPNODE, CC}
	, {S_HWADDROF, "HWADDROF", NULL, NULL, NULL, MOPNODE, CC}
	, {S_ANYMOBILETYPE, "MOBILE.ANY", NULL, "ANYMOBILETYPE", NULL, LEAFNODE, CC}

	, {S_UINT, "UINT", NULL, NULL, NULL, LEAFNODE, A}
	, {S_UINT16, "UINT16", NULL, NULL, NULL, LEAFNODE, A}
	, {S_UINT32, "UINT32", NULL, NULL, NULL, LEAFNODE, A}
	, {S_UINT64, "UINT64", NULL, NULL, NULL, LEAFNODE, A}

	, {S_DYNCALL, "DYNCALL", NULL, NULL, NULL, NONODE, CC}
	, {S_TYPEHASHCHECK, "TYPEHASHCHECK", NULL, NULL, NULL, DOPNODE, CC}
};

#define MAX_TAG ((int)(sizeof(vti_keyword_table)/sizeof(vti_keyword_table[0])))

/*}}}*/
/*{{{  Special ASM keywords*/
#ifdef MOBILES
PRIVATE const char *asmnames[ASMNAMES_COUNT] = { ".WSSIZE", ".VSPTR", ".STATIC", ".FB", ".MSPTR", ".MPPTR" };
#else
PRIVATE const char *asmnames[ASMNAMES_COUNT] = { ".WSSIZE", ".VSPTR", ".STATIC", ".FB" };
#endif
PUBLIC INT32 asmvalues[ASMNAMES_COUNT];
PUBLIC int asmvalids[ASMNAMES_COUNT];
/*}}}*/

/*{{{  PUBLIC vars*/
/* This are initialised by vtiinit, and used for vti diagnostics */
PUBLIC INT32 vti_no_slot_value;

#ifdef CAN_USE_INLINE
PUBLIC int vti_max_tag;
#endif
/*}}}*/

/*{{{  PRIVATE void dec (s, item)*/
PRIVATE void dec (const char *const s, const int item)
{
	wordnode *const wptr = lookupword (s, strlen (s));
	SetWTag (wptr, item);
}

/*}}}*/
/*{{{  PUBLIC void declkeywords*/
/* Insert the occam reserved words in the symbol table */
PUBLIC void declkeywords (const fe_lang_t fe_lang)
{
	int i;

	clear_keywords ();	/* clear any outstanding keyword tags */

	for (i = 0; i < MAX_TAG; i++) {
		if (vti_keyword_table[i].s_tag != i)
			err_abort ("declkeywords");

		if ((vti_keyword_table[i].s_keyword != NULL) && ((vti_keyword_table[i].s_valid_lang & fe_lang) != 0))
			dec (vti_keyword_table[i].s_keyword, i);
	}

	for (i = 0; i < ASMNAMES_COUNT; i++) {
		dec (asmnames[i], S_ASMNAME);
		asmvalids[i] = ASMNAME_INVALID;	/* currently unknown value */
	}
}

/*}}}*/
/*{{{  PUBLIC char *keywordstring (t) occam keyword    NEVER USED*/
#if 0				/* keywordstring is never used */
PUBLIC char *keywordstring (const int t)
{
	if (t < 0 || t >= MAX_TAG)
		return NULL;
	return vti_keyword_table[t].s_keyword;
}
#endif /* keywordstring is never used */
/*}}}*/
/*{{{  PUBLIC void init_nodetypeoftag*/
PUBLIC void init_nodetypeoftag (void)
{
#ifdef CAN_USE_INLINE
	vti_max_tag = MAX_TAG;
#endif
}

/*}}}*/
/*{{{  PUBLIC nodetypeoftag_t nodetypeoftag*/
#ifndef CAN_USE_INLINE
PUBLIC nodetypeoftag_t nodetypeoftag (const int t)
{
	if ((t < 0) || (t >= MAX_TAG)) {
		return NONODE;
	}
	return vti_keyword_table[t].s_nodetype;
}
#endif
/*}}}*/
/*{{{  PUBLIC nametypeoftag_t nametypeoftag*/
PUBLIC nametypeoftag_t nametypeoftag (const int tag)
{
	switch (tag) {
	case N_DECL:
	case N_REPL:
	case N_ABBR:
	case N_PARAM:
	case N_RETYPE:
	case N_VALABBR:
	case N_VALPARAM:
	case N_VALRETYPE:
	case N_RESULTPARAM:
	case T_PREEVALTEMP:
	case T_REGTEMP:
	case T_RESERVEDWS:
	case T_TEMP:
	case S_FNFORMALRESULT:
	case S_FNACTUALRESULT:
	case S_HIDDEN_PARAM:
	case S_PARAM_STATICLINK:
	case S_PARAM_VSP:
	case S_PARAM_FB:
	case S_PARAM_WS:
#ifdef MOBILES
	case S_HIDDEN_TYPE:
	case S_PARAM_MSP:
	case S_PARAM_MPP:
#endif
		return NAMENODE_VAR;

	case N_INLINEFUNCDEF:
	case N_INLINEPROCDEF:
	case N_LFUNCDEF:
	case N_LIBFUNCDEF:
	case N_LIBPROCDEF:
	case N_LIBMPROCDECL:
	case N_PREDEFFUNCTION:
	case N_PREDEFPROC:
	case N_PROCDEF:
	case N_SCFUNCDEF:
	case N_SCPROCDEF:
	case N_SFUNCDEF:
	case N_STDLIBFUNCDEF:
	case N_STDLIBPROCDEF:
#ifdef MOBILES
	case N_MPROCDECL:
#endif
		return NAMENODE_ROUTINE;

	case N_TAGDEF:
	case N_SPROTDEF:
	case N_TPROTDEF:
	case N_LABELDEF:
#if 1				/*def CONFIG */
	CASE_CONFIG_NAME case N_FIELD:
#endif
#ifdef OCCAM2_5
	case N_TYPEDECL:
#endif
#ifdef MOBILES
	case N_PROCTYPEDECL:
#endif
		return NAMENODE_MISC;

	default:
		badtag (NOPOSN, tag, "nametypeoftag");
		return 0;
	}
}

/*}}}*/
/*{{{  PUBLIC       treenode *checknode_fn*/
PUBLIC treenode *checknode_fn (treenode *const tptr, const nodetypeoftag_t nodetype, const char *const file, const int line)
{
	if (!tptr) {
		// msg_out_s (SEV_INTERNAL, ANY_MODULE, ANY_FILEINFO, NOPOSN, "checknode_fn() NULL");
		err_abort ("checknode_fn()");
		return NULL;
	}
	if (nodetypeoftag (TagOf (tptr)) != nodetype) {
		msg_out_is (SEV_INFO, ANY_MODULE, ANY_FILEINFO, NOPOSN, line, file);
		badtag (LocnOf (tptr), TagOf (tptr), "checknode_fn");
	}
	return tptr;
}
/*}}}*/
/*{{{  PUBLIC const treenode *checkread_fn*/
PUBLIC const treenode *checkread_fn (const treenode *const tptr, const nodetypeoftag_t nodetype, const char *const file, const int line)
{
	if (nodetypeoftag (TagOf (tptr)) != nodetype) {
		msg_out_is (SEV_INFO, ANY_MODULE, ANY_FILEINFO, NOPOSN, line, file);
		badtag (LocnOf (tptr), TagOf (tptr), "checkread_fn");
	}
	return tptr;
}
/*}}}*/
/*{{{  PUBLIC const char *tagstring(t)      occam tag*/
PUBLIC const char *tagstring (const int t)
{
	if (t >= 0 && t < MAX_TAG) {
		const char *s;
		s = vti_keyword_table[t].s_tagstr;
		if (s != NULL) {
			return s;
		}

		s = vti_keyword_table[t].s_keyword;
		if (s != NULL) {
			return s;
		}
	}

	{
		static char a_tagstring[50];
		sprintf (a_tagstring, "tagstring: unknown tag %d", t);
		return a_tagstring;
	}
}

/*}}}*/
/*{{{  PUBLIC const char *itagstring (t)    internal tag*/
PUBLIC const char *itagstring (const int t)
{
	if (t >= 0 && t < MAX_TAG) {
		const char *s = vti_keyword_table[t].s_itagstr;
		if (s != NULL)
			return s;
	}
	return tagstring (t);
}

/*}}}*/
/*{{{  PRIVATE getstrchar*/
/* return a string for byte b */
PRIVATE const char *getstrchar (const int b)
{
	const char *s;
	switch (b) {
	case '\r':
		s = "*c";
		break;
	case '\n':
		s = "*n";
		break;
	case '\t':
		s = "*t";
		break;
	case '\'':
		s = "*'";
		break;
	case '"':
		s = "*\"";
		break;
	case '*':
		s = "**";
		break;
	default:
		{
			static char string[5];	/* enough for *#nn with a NUL on the end */
			if ((b >= 32) && (b <= 127)) {
				string[0] = b;
				string[1] = '\0';
			} else
				sprintf (string, "*#%02X", b & 0xff);
			return string;
		}
	}
	return s;
}

/*}}}*/
/*{{{  PUBLIC void ftagstring (t)     more data for errors*/
PUBLIC void ftagstring (char *const string, const int t, const wordnode * current_word, const int current_literalp, const char *const current_literalv)
{
	if (t >= 0 && t < MAX_TAG) {
		const char *s = vti_keyword_table[t].s_ftagstr;
		if (s != NULL)
			/*{{{  use this value */
		{
#if 0				/* bug 319 5/11/90 */
			/* Using sprintf breaks if the literal contains a NUL character */
			/* However, the result of ftagstring is also processed by printf type
			   functions, so we really need to correct them all! */
			/* NO: this is the only place where literalv is accessed.
			   Just expand the non-printing chars here. CO'N 5/11/90 */
			/* now allows use of %% in string to print % character - Jim 24/2/97 */
			sprintf (string, s, (t == S_NAME) ? WNameOf (current_word) : current_literalv);
#else
			char *dest = string;
			while ((*s) != '\0') {
				if ((*s) == '%') {
					if ((*(s + 1)) != '%') {	/* not %% */
						const int len = (t == S_NAME) ? WLengthOf (current_word)
							: current_literalp;
#if 1				/* can't use memcpy cos we need to expand stuff */
						const char *c = (t == S_NAME) ? WNameOf (current_word)
							: current_literalv;
						int i;
						for (i = 0; i < len; i++)
							/* *dest++ = c[i]; */
						{
							const char *expanded = getstrchar (c[i]);
							const int expanded_len = strlen (expanded);
							memcpy (dest, expanded, expanded_len);
							dest += expanded_len;
						}
#else
						memcpy (dest, (t == S_NAME) ? WNameOf (current_word)
							: current_literalv, len);
						dest += len;
#endif
						s += 2;	/* skip over the '%s' */
					} else {	/* is %% */
						*dest++ = *s++;
						s++;	/* skip extra % */
					}
				} else	/* not % at all */
					*dest++ = *s++;
			}
			*dest = '\0';
#endif
			return;
		}
		/*}}} */
	}
	strcpy (string, tagstring (t));
}

/*}}}*/
/*{{{  PRIVATE void ptag (t)          occam tag*/
PRIVATE void ptag (FILE * const fptr, const int t)
{
	fprintf (fptr, "%s ", tagstring (t));
}

/*}}}*/
/*{{{  PRIVATE void pitag (t)         internal tag*/
PRIVATE void pitag (FILE * const fptr, const int t)
{
	fprintf (fptr, "%s ", itagstring (t));
}

/*}}}*/
/*{{{  PUBLIC int which_asmname*/
PUBLIC int which_asmname (treenode * tptr)
{
	int i;
	for (i = 0; i < ASMNAMES_COUNT; i++)
		if (strcmp (asmnames[i], WNameOf ((wordnode *) tptr)) == 0)
			return i;
	return 0;		/* should never be reached */
}

/*}}}*/

/*{{{  PRIVATE printstrchar*/
/* Print out the byte b */
PRIVATE void printstrchar (FILE * const fptr, const int b)
{
	fputs (getstrchar (b), fptr);
}

/*}}}*/
/*{{{  PUBLIC printbyte*/
/* Print out the byte b */
PUBLIC void printbyte (FILE * const fptr, const int b)
{
	putc ('\'', fptr);
	printstrchar (fptr, b);
	fprintf (fptr, "\' ");
}

/*}}}*/
/*{{{  PUBLIC printstring*/
/* Print out the string in s[0..l] */
PUBLIC void printstring (FILE * const fptr, const char *const s, const int l)
{
	int i;
	putc ('"', fptr);
	for (i = 0; i < l; i++) {
		printstrchar (fptr, s[i]);
	}
	fprintf (fptr, "\" ");
}

/*}}}*/
/*{{{  PRIVATE void setsrcindent*/
PRIVATE void setsrcindent (FILE * const fptr, const int i)
{
	int n = 0;

	for (; n < (i - (TABSIZE - 1)); n += TABSIZE)
		putc ('\t', fptr);
	for (; n < i; n++)
		putc (' ', fptr);
}

/*}}}*/
/*{{{  PRIVATE void setindent (i)*/
PRIVATE void setindent (FILE * const fptr, const int i)
{
	putc ('\n', fptr);
	setsrcindent (fptr, i);
}

/*}}}*/

/*{{{  printdecllist  forward declaration*/
PRIVATE void printdecllist (FILE * const fptr, const int indent, treenode * n);
PRIVATE void printsrctype (FILE * const fptr, treenode * type);
/*}}}*/
/*{{{  printmode()*/
PRIVATE void printmode (FILE * const fptr, const int mode)
{
	const char *s;
	switch (mode) {
	case NM_DEFAULT:
		s = "NM_DEFAULT";
		break;
	case NM_WORKSPACE:
		s = "NM_WORKSPACE";
		break;
	case NM_VECSPACE:
		s = "NM_VECSPACE";
		break;
	case NM_PLACED:
		s = "NM_PLACED";
		break;
	case NM_POINTER:
		s = "NM_POINTER";
		break;
/*case NM_INLINE:    s = "NM_INLINE";    break; *//* never used */
	case NM_WSPLACED:
		s = "NM_WSPLACED";
		break;
	default:
		s = "unknown";
		break;
	}
	fprintf (fptr, " Mode %s", s);
}

/*}}}*/
/*{{{  PRIVATEPARAM int print_free_var*/
struct print_free_vars_s
{
	FILE *fptr;
	int indent;
	BOOL any_free_vars;
};

PRIVATEPARAM int print_free_var (treenode ** nptr, void *const voidptr)
{
	struct print_free_vars_s *const info = voidptr;

	if (!info->any_free_vars)
		fprintf (info->fptr, "Free variables:");
	info->any_free_vars = TRUE;

	printtree (info->fptr, info->indent, *nptr);

	return CONTINUE_WALK;
}

/*}}}*/
/*{{{  PRIVATE void printdecl (FILE *const fptr, const int indent, treenode *const n)*/
PRIVATE void printdecl (FILE *const fptr, const int indent, treenode *const n)
{
	int dtype = TagOf (n);

	if (nodetypeoftag (dtype) == NAMENODE) {
		wordnode *wptr = NNameOf (n);
		treenode *type = NTypeOf (n);

		if ((WTagOf (wptr) == S_ASINPUT) || (WTagOf (wptr) == S_ASOUTPUT)) {
			treenode *op = (treenode *)wptr;
			int asinput = (WTagOf (wptr) == S_ASINPUT);

			op = OpOf (op);
			printtree (fptr, indent, op);
			if (asinput) {
				fprintf (fptr, "? ");
			} else {
				fprintf (fptr, "! ");
			}
		} else {
			printstring (fptr, WNameOf (wptr), WLengthOf (wptr));
		}

		DEBUG_MSG (("(0x%x)", (unsigned int)n));
		fprintf (fptr, ", lexlevel = %d, scope = %d,", NLexLevelOf (n), NScopeOf (n));
		printmode (fptr, NModeOf (n));
		if (NTypeAttrOf (n)) {
			if (NTypeAttrOf (n) & TypeAttr_marked_in) {
				fprintf (fptr, ", [C?] ");
			} else if (NTypeAttrOf (n) & TypeAttr_marked_out) {
				fprintf (fptr, ", [C!] ");
			}
			if (NTypeAttrOf (n) & TypeAttr_shared) {
				fprintf (fptr, "[S] ");
			}
			if (NTypeAttrOf (n) & TypeAttr_claimed) {
				fprintf (fptr, "[*] ");
			}
			if (NTypeAttrOf (n) & TypeAttr_undefined) {
				fprintf (fptr, "[U] ");
			}
			if (NTypeAttrOf (n) & TypeAttr_fixed) {
				fprintf (fptr, "[F] ");
			}
			if (NTypeAttrOf (n) & TypeAttr_ufixed) {
				fprintf (fptr, "[UF] ");
			}
			if (NTypeAttrOf (n) & TypeAttr_placed) {
				fprintf (fptr, "[P] ");
			}
			if (NTypeAttrOf (n) & TypeAttr_resigned) {
				fprintf (fptr, "[RB] ");
			}
		}
		if ((TagOf (n) == N_PARAM) || (TagOf (n) == N_ABBR)) {
			if (NVIndirectOf (n)) {
				fprintf (fptr, "[~~] ");
			}
		}
		switch (dtype) {
		case N_TPROTDEF:
		case N_PROCDEF:
#ifdef MOBILES
		case N_MPROCDECL:
#endif
		case N_INLINEPROCDEF:
			printdecllist (fptr, indent + 2, type);	/* tags or parameters */
			break;
		case N_SFUNCDEF:
		case N_LFUNCDEF:
		case N_INLINEFUNCDEF:
			DEBUG_MSG ((" %s:(0x%x) ", itagstring (TagOf (type)), (unsigned int)type));
			printtree (fptr, indent + 2, FnTypeListOf (type));	/* result types */
			printdecllist (fptr, indent + 2, FnParamsOf (type));	/* parameters */
			break;
		case N_ABBR:
			fprintf (fptr, " rdef = %d,", NVRDefinedOf (n));
			printtree (fptr, indent + 2, type);
			break;
		case N_TAGDEF:
			fprintf (fptr, " ntval = %d,", (int)NTValueOf (n));
			printtree (fptr, indent + 2, type);
			break;
		default:
			printtree (fptr, indent + 2, type);
			break;
		}

		switch (dtype) {
		case N_PROCDEF:
#ifdef MOBILES
		case N_MPROCDECL:
#endif
		case N_SFUNCDEF:
		case N_LFUNCDEF:
		case N_INLINEPROCDEF:
		case N_INLINEFUNCDEF:
			/*{{{  print out the free variable list */
#if 0
			{
				varlist *vlist = NFreeVarsOf (n);
				if (vlist == (varlist *) (-1))
					vlist = NULL;
				fprintf (fptr, "%sFree variables:", (vlist == NULL) ? "No " : "");
				while (vlist != NULL) {
					printtree (fptr, indent + 4, VLNameOf (vlist));
					vlist = VLNextOf (vlist);
				}
			}
#else

			{
				struct print_free_vars_s info;

				/* will be used for the 'free variables' message: */
				setindent (fptr, indent + 2);

				info.fptr = fptr;
				info.indent = indent + 4;
				info.any_free_vars = FALSE;

				fe_walk_free_vars (NULL, n, print_free_var, &info);

				if (!info.any_free_vars) {
					fprintf (fptr, "No Free variables");
				}
			}
#endif
			break;
			/*}}} */
		default:
			break;
		}
	} else {
		printtree (fptr, indent, n);
	}
}

/*}}}*/
/*{{{  PRIVATE void printdecllist (indent, n)*/
PRIVATE void printdecllist (FILE * const fptr, const int indent, treenode * n)
{
	while (!EndOfList (n))
		/*{{{  print left, move to right */
	{
		setindent (fptr, indent);
		pitag (fptr, S_LIST);
		DEBUG_MSG (("(0x%x) ", (unsigned int)n));
		printdecl (fptr, indent + 2, ThisItem (n));
		n = NextItem (n);
	}
	/*}}} */
	setindent (fptr, indent);
	fprintf (fptr, "NULL");
}

/*}}}*/
/*{{{  PUBLIC void printexp (FILE *const fptr, treenode *tptr)*/
PUBLIC void printexp (FILE *const fptr, treenode *tptr)
{
	while (tptr != NULL)
		switch (TagOf (tptr))
			/*{{{  cases */
		{
			/*{{{  monadic operator */
		case S_MOSTPOS:
		case S_MOSTNEG:
			ptag (fptr, TagOf (tptr));
			/*ptag(fptr, TagOf(OpOf(tptr))); */
			/*return; */
			tptr = OpOf (tptr);
			break;
		case S_NEG:
		case S_BITNOT:
		case S_NOT:
		case S_SIZE:
		case S_UMINUS:
		case S_UPLUS:
		case S_ADDRESSOF:
#ifdef MOBILES
		case S_CLONE:
		case S_ADDROF:
		case S_HWADDROF:
		case S_TYPEHASHOF:
#endif
			ptag (fptr, S_LPAREN);
			ptag (fptr, TagOf (tptr));
			printexp (fptr, OpOf (tptr));
			ptag (fptr, S_RPAREN);
			return;
#ifdef OCCAM2_5
		case S_BYTESIN:
			ptag (fptr, TagOf (tptr));
			ptag (fptr, S_LPAREN);
			printexp (fptr, OpOf (tptr));
			ptag (fptr, S_RPAREN);
			return;
#endif
		case S_ELSIZE:
			tptr = dimexpof (OpOf (tptr), 0);
			break;
		case S_SEGSTART:
			tptr = SStartExpOf (OpOf (tptr));
			break;
			/*}}} */
			/*{{{  conversion */
		case S_EXACT:
		case S_ROUND:
		case S_TRUNC:
			ptag (fptr, S_LPAREN);
			ptag (fptr, MOpTypeOf (tptr));
			if (TagOf (tptr) != S_EXACT)
				ptag (fptr, TagOf (tptr));
			printexp (fptr, OpOf (tptr));
			ptag (fptr, S_RPAREN);
			return;
			/*}}} */
			/*{{{  dyadic operator */
		case S_AND:
		case S_OR:
		case S_ADD:
		case S_SUBTRACT:
		case S_MULT:
		case S_DIV:
		case S_REM:
		case S_BITAND:
		case S_BITOR:
		case S_XOR:
		case S_LSHIFT:
		case S_RSHIFT:
		case S_PLUS:
		case S_MINUS:
		case S_TIMES:
		case S_EQ:
		case S_NE:
		case S_LS:
		case S_LE:
		case S_GR:
		case S_GE:
		case S_AFTER:
		case S_CSUB0:
		case S_CCNT1:
		case S_EVAL:	/*case S_POSTEVAL: */
		case S_OVERLAPCHECK:
		case S_FOR:
#ifdef MOBILES
		case S_NTH_DIMENSION:
#endif
			ptag (fptr, S_LPAREN);
			printexp (fptr, LeftOpOf (tptr));
			ptag (fptr, TagOf (tptr));
			printexp (fptr, RightOpOf (tptr));
			ptag (fptr, S_RPAREN);
			return;
#ifdef OCCAM2_5
		case S_OFFSETOF:
			ptag (fptr, TagOf (tptr));
			ptag (fptr, S_LPAREN);
			printexp (fptr, LeftOpOf (tptr));
			ptag (fptr, S_COMMA);
			printexp (fptr, RightOpOf (tptr));
			ptag (fptr, S_RPAREN);
			return;
#endif

			/*}}} */
			/*{{{  literal */
			/*{{{  Bool literals */
		case S_TRUE:
		case S_FALSE:
			ptag (fptr, TagOf (tptr));
			return;
			/*}}} */
			/*{{{  byte int and real literals */
#if 0
		case S_INTLIT:
		case S_INT16LIT:
		case S_INT32LIT:
		case S_INT64LIT:
		case S_REAL32LIT:
		case S_REAL64LIT:
		case S_BYTELIT:
#endif
		case S_UBYTELIT:
		case S_UINTLIT:
		case S_UREALLIT:
		case S_CONSTRUCTOR:
			if (TagOf (tptr) == S_CONSTRUCTOR) {
				ptag (fptr, S_LBOX);
				printexp (fptr, LitExpOf (tptr));
				ptag (fptr, S_RBOX);
			} else {
				wordnode *const nptr = (wordnode *) StringPtrOf (tptr);
				if (nptr != NULL)
					printstring (fptr, WNameOf (nptr), WLengthOf (nptr));
			}
			if (LitTypeOf (tptr) != NULL) {
				ptag (fptr, S_LPAREN);
				/*ptag(fptr, TagOf(LitTypeOf(tptr))); */
				printsrctype (fptr, LitTypeOf (tptr));
				ptag (fptr, S_RPAREN);
			}
			return;
			/*}}} */
			/*{{{  string */
		case S_STRING:
			{
				wordnode *const nptr = (wordnode *) CTValOf (tptr);
				printstring (fptr, WNameOf (nptr), WLengthOf (nptr));
			}
			return;
			/*}}} */
			/*{{{  constconstructor */
		case S_CONSTCONSTRUCTOR:
			tptr = CTExpOf (tptr);
			break;
			/*}}} */
			/*}}} */
			/*{{{  function instance */
		case S_FINSTANCE:
			printexp (fptr, INameOf (tptr));
			ptag (fptr, S_LPAREN);
			printexp (fptr, IParamListOf (tptr));
			ptag (fptr, S_RPAREN);
			return;
			/*}}} */
			/*{{{  subscript */
		case S_ARRAYSUB:
		case S_RECORDSUB:
			printexp (fptr, ASBaseOf (tptr));
			ptag (fptr, S_LBOX);
			printexp (fptr, ASIndexOf (tptr));
			ptag (fptr, S_RBOX);
			return;
			/*}}} */
			/*{{{  transformed subscript */
		case S_ARRAYITEM:
		case S_RECORDITEM:
			printexp (fptr, ASBaseOf (tptr));
			ptag (fptr, S_LBOX);
			if (ASExpOf (tptr) != NULL) {
				if (ASOffsetOf (tptr) != 0)
					ptag (fptr, S_LPAREN);
				printexp (fptr, ASExpOf (tptr));
				if (ASOffsetOf (tptr) != 0)
					ptag (fptr, S_RPAREN);
			}
			if (ASOffsetOf (tptr) != 0) {
				if (ASExpOf (tptr) != NULL)
					ptag (fptr, S_PLUS);
				fprintf (fptr, "%ld ", (long) ASOffsetOf (tptr));
			}
			ptag (fptr, S_RBOX);
			return;
			/*}}} */
			/*{{{  segment */
		case S_SEGMENT:
		case S_SEGMENTITEM:
			ptag (fptr, S_LBOX);
			printexp (fptr, SNameOf (tptr));
			ptag (fptr, S_FROM);
			printexp (fptr, SStartExpOf (tptr));
			ptag (fptr, S_FOR);
			printexp (fptr, SLengthExpOf (tptr));
			ptag (fptr, S_RBOX);
			return;
			/*}}} */
			/*{{{  name */
		case N_VALABBR:
		case N_VALRETYPE:
		case N_ABBR:
		case N_RETYPE:
		case N_DECL:
		case N_TAGDEF:
		case N_SPROTDEF:
		case N_TPROTDEF:
		case N_VALPARAM:
		case N_PARAM:
		case N_RESULTPARAM:
		case N_PROCDEF:
		case N_SFUNCDEF:
		case N_LFUNCDEF:
		case N_REPL:
		case N_SCFUNCDEF:
		case N_LIBFUNCDEF:
		case N_SCPROCDEF:
		case N_LIBPROCDEF:
		case N_LIBMPROCDECL:
		case N_STDLIBFUNCDEF:
		case N_STDLIBPROCDEF:
		case N_PREDEFFUNCTION:
		case N_PREDEFPROC:
		case N_INLINEPROCDEF:
		case N_INLINEFUNCDEF:
		case N_FIELD:
		case N_LABELDEF:
		case N_TYPEDECL:
#ifdef MOBILES
		case N_PROCTYPEDECL:
		case N_MPROCDECL:
#endif
			{
				wordnode *nptr = NNameOf (tptr);
				printstring (fptr, WNameOf (nptr), WLengthOf (nptr));
			}
			return;
			/*}}} */
			/*{{{  list */
		case S_LIST:
			{
				printexp (fptr, ThisItem (tptr));
				tptr = NextItem (tptr);
				if (!EndOfList (tptr))
					ptag (fptr, S_COMMA);
			}
			break;
			/*}}} */
			/*{{{  specification valof */
		case S_VALABBR:
		case S_VALRETYPE:
		case S_DECL:
		case S_ABBR:
		case S_RETYPE:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_PROCDEF:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_VALOF:
		case S_PRAGMA:	/* bug 829 20/9/91 */
		case S_TYPEDECL:
#if MOBILES
		case S_FORWDECL:
		case S_PROCTYPEDECL:
		case S_MPROCDECL:
#endif
			tptr = skipspecifications (tptr);
			ptag (fptr, S_VALOF);
			fprintf (fptr, "... ");
			ptag (fptr, S_RESULT);
			tptr = VLResultListOf (tptr);
			break;
			/*}}} */
			/*{{{  constant expression */
		case S_CONSTEXP:
		case S_UDVCONSTEXP:
			if (CExpOf (tptr) != NULL && TagOf (CExpOf (tptr)) != S_DUMMYEXP)
				printexp (fptr, CExpOf (tptr));
			else
				fprintf (fptr, "%ld ", (long) LoValOf (tptr));
			return;
			/*}}} */
			/*{{{  temp */
		case T_TEMP:
		case T_PREEVALTEMP:
			fprintf (fptr, "$%stemp%d ", (TagOf (tptr) == T_TEMP) ? "" : "preeval", NVVarNumOf (tptr));
			return;
			/*}}} */
			/*{{{  special parameters */
		case S_HIDDEN_PARAM:
			fprintf (fptr, "hiddenparam");
			return;
		case S_PARAM_STATICLINK:
			fprintf (fptr, "staticlink");
			return;
		case S_PARAM_VSP:
			fprintf (fptr, "vsptr");
			return;
		case S_PARAM_FB:
			fprintf (fptr, "fb");
			return;
		case S_PARAM_WS:
			fprintf (fptr, "ws");
			return;
		#ifdef MOBILES
		case S_PARAM_MSP:
			fprintf (fptr, "msptr");
			return;
		case S_PARAM_MPP:
			fprintf (fptr, "mpptr");
			return;
		case S_HIDDEN_TYPE:
			fprintf (fptr, "hiddentype");
			return;
		#endif
			/*}}} */
			/*{{{  array dimension */
		case S_ARRAY:
			fputc ('[', fptr);
			printexp (fptr, ARDimLengthOf (tptr));
			fputc (']', fptr);
			tptr = ARTypeOf (tptr);
			/*}}} */
		default:
			ptag (fptr, TagOf (tptr));
			return;
		}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void printtreenl (fptr, indent, n) */
PUBLIC void printtreenl (FILE *const fptr, int indent, treenode *n)
{
	printtree (fptr, indent, n);
	fprintf (fptr, "\n");
	return;
}
/*}}}  */
/*{{{  PUBLIC void printtree(indent, n)*/
PUBLIC void printtree (FILE * const fptr, int indent, treenode * n)
{
	static treenode *cnodeoftemp = NULL;

	while (n != NULL) {
		setindent (fptr, indent);
		pitag (fptr, TagOf (n));

		if (n == cnodeoftemp) {
			fprintf (fptr, " [recursive tree!]");
			return;
		}
		DEBUG_MSG (("(0x%x) ", (unsigned int)n));
		switch (nodetypeoftag (TagOf (n))) {
			/*{{{  cases */
			/*{{{  leaf node */
		case LEAFNODE:
			if ((TagOf (n) == S_ANYCHANTYPE) && LeafLinkOf (n)) {
				treenode *link = LeafLinkOf (n);

				if (TagOf (link) != S_MOBILE) {
					fprintf (fptr, " [leaflink not MOBILE!]");
				} else {
					pitag (fptr, TagOf (link));
					if (TypeAttrOf (link) & TypeAttr_shared) {
						fprintf (fptr, " [S]");
					}
					if (TypeAttrOf (link) & TypeAttr_marked_in) {
						fprintf (fptr, " [?]");
					}
					if (TypeAttrOf (link) & TypeAttr_marked_out) {
						fprintf (fptr, " [!]");
					}
				}
			} else if ((TagOf (n) == S_SYNC) && LeafLinkOf (n)) {
				treenode *bar = LeafLinkOf (n);

				printtree (fptr, indent + 1, bar);
			}
			return;
			/*}}} */
			/*{{{  construct node */
		case CNODE:
			indent += 2;
			if (CTempOf (n) != NULL) {
				treenode *saved_cot = cnodeoftemp;

				/* this sometimes results in forever recursion in ALTs... */
				cnodeoftemp = n;
				printtree (fptr, indent, CTempOf (n));
				cnodeoftemp = saved_cot;
			}
			n = CBodyOf (n);
			break;
			/*}}} */
			/*{{{  replicated construct node */
		case REPLCNODE:
			indent += 2;
			if (ReplCTempOf (n) != NULL) {
				treenode *saved_cot = cnodeoftemp;

				/* recursion prevention... */
				cnodeoftemp = n;
				printtree (fptr, indent, ReplCTempOf (n));
				cnodeoftemp = saved_cot;
			}
			printtree (fptr, indent, ReplCNameOf (n));
			printtree (fptr, indent, ReplCStartExpOf (n));
			printtree (fptr, indent, ReplCLengthExpOf (n));
			if (ReplCStepExpOf (n)) {
				printtree (fptr, indent, ReplCStepExpOf (n));
			}
			n = ReplCBodyOf (n);
			break;
			/*}}} */
			/*{{{  condition node */
		case CONDNODE:
			indent += 2;
			printtree (fptr, indent, CondGuardOf (n));
			if (TagOf (n) == S_X_VARIANT) {
				printtree (fptr, indent, VRDuringOf (n));
				n = VRAfterOf (n);
			} else {
				n = CondBodyOf (n);
			}
			break;
			/*}}} */
			/*{{{  instance node */
		case INSTANCENODE:
			if (isinline (INameOf (n))) {
				fprintf (fptr, " INLINE");
			}
			if (IForkedOf (n)) {
				fprintf (fptr, " FORKED");
			}
			if (IDynmemOf (n)) {
				fprintf (fptr, " DYNCALL");
			}
			if (IDynaddrOf (n)) {
				fprintf (fptr, " AT");
				printtree (fptr, indent + 2, IDynaddrOf (n));
			}

			indent += 2;
			printtree (fptr, indent, INameOf (n));
			n = IParamListOf (n);
			break;
			/*}}} */
			/*{{{  action node */
		case ACTIONNODE:
			if (ActionFlagsOf (n)) {
				fprintf (fptr, " [");
				if (ActionFlagsOf (n) & ActionFlag_skip_xable) {
					fprintf (fptr, "skip-xable ");
				}
				if (ActionFlagsOf (n) & ActionFlag_mobile) {
					fprintf (fptr, "mobile ");
				}
				if (ActionFlagsOf (n) & ActionFlag_dynmob) {
					fprintf (fptr, "dynmob ");
				}
				if (ActionFlagsOf (n) & ActionFlag_count) {
					fprintf (fptr, "count ");
				}
				if (ActionFlagsOf (n) & ActionFlag_case) {
					fprintf (fptr, "case ");
				}
				if (ActionFlagsOf (n) & ActionFlag_ed3seq) {
					fprintf (fptr, "3seq ");
				}
				if (ActionFlagsOf (n) & ActionFlag_edacomm) {
					fprintf (fptr, "acomm ");
				}
				if (ActionFlagsOf (n) & ActionFlag_edmcomm) {
					fprintf (fptr, "mcomm ");
				}
				if (ActionFlagsOf (n) & ActionFlag_precount) {
					fprintf (fptr, "precount ");
				}
				if (ActionFlagsOf (n) & ActionFlag_encode) {
					fprintf (fptr, "encode");
				}
				if (ActionFlagsOf (n) & ActionFlag_decode) {
					fprintf (fptr, "decode");
				}
				fprintf (fptr, "]");
			}
			indent += 2;
			printtree (fptr, indent, LHSOf (n));
			printtree (fptr, indent, RHSOf (n));
			switch (TagOf (n)) {
			case S_X_INPUT:
			case S_X_TAGGED_INPUT:
				printtree (fptr, indent, ActionTypeOf (n));
				printtree (fptr, indent, ActionDuringOf (n));
				n = ActionAfterOf (n);
				break;
			default:
				n = ActionTypeOf (n);
				break;
			}
			break;
			/*}}} */
			/*{{{  declaration node */
		case DECLNODE:
			if (isinline (DNameOf (n))) {
				fprintf (fptr, "INLINE");
			}
			if (TagOf (DNameOf (n)) == S_LIST) {
				printdecllist (fptr, indent + 2, DNameOf (n));
			} else {
				switch (TagOf (DNameOf (n))) {
				case N_PROCDEF:
				case N_MPROCDECL:
				case N_LIBPROCDEF:
				case N_LIBMPROCDECL:
					if (NPRecursiveOf (DNameOf (n))) {
						fprintf (fptr, " (recursive)");
					}
					if (NPForksOf (DNameOf (n))) {
						fprintf (fptr, " (forks)");
					}
					if (NPSuspendsOf (DNameOf (n))) {
						fprintf (fptr, " (suspends)");
					}
					if (NPDyncallOf (DNameOf (n))) {
						fprintf (fptr, " (dyncall)");
					}
					if (NFMCheckOf (DNameOf (n))) {
						fprintf (fptr, " [fmcheck]");
					}
					break;
				default:
#ifdef OCCAM2_5
					if ((nodetypeoftag (TagOf (DNameOf (n))) == NAMENODE) && NVReshapesOf (DNameOf (n))) {
						fprintf (fptr, " (RESHAPES)");
					}
#endif
					break;
				}
				printdecl (fptr, indent + 2, DNameOf (n));
			}
			if (DValOf (n) != NULL) {
				printtree (fptr, indent + 2, DValOf (n));
			}
			if (DExtraOf (n) != NULL) {
				printtree (fptr, indent + 2, DExtraOf (n));
			}
			/* indent += 2; */
			n = DBodyOf (n);
			break;
			/*}}} */
			/*{{{  list node */
		case LISTNODE:
			if (TagOf (n) == S_FNTYPE) {
				indent += 2;
				printtree (fptr, indent, FnTypeListOf (n));
				n = FnParamsOf (n);
			} else {
				/*indent += 2;
				   printtree(fptr, indent, ThisItem(n)); */
				printtree (fptr, indent + 2, ThisItem (n));
				n = NextItem (n);
			}
			break;
			/*}}} */
			/*{{{  monadic operator node */
		case MOPNODE:
			indent += 2;
			switch (TagOf (n)) {
			default:
				/*setindent(fptr, indent); */
				fputs ("( ", fptr);
				pitag (fptr, MOpTypeOf (n));
				fputc (')', fptr);

				if (OpTypeAttrOf (n) & TypeAttr_shared) {
					fprintf (fptr, " [S]");
				}
				if (OpTypeAttrOf (n) & TypeAttr_undefined) {
					fprintf (fptr, " [U]");
				}
				n = OpOf (n);
				break;
			case S_ELSIZE:
				fprintf (fptr, "(dimexpof(tptr, 0) is:)");
				n = dimexpof (OpOf (n), 0);
				break;
			case S_SEGSTART:
				fprintf (fptr, "(Seg start is:)");
				n = SStartExpOf (OpOf (n));
				break;
			}
			break;
			/*}}} */
			/*{{{  dyadic operator node */
		case DOPNODE:
			indent += 2;
			if ((TagOf (n) == S_GUYCODE) || (TagOf (n) == S_GUYSTEP)) {
				/*setindent(fptr, indent); */
				fprintf (fptr, "Code: #%X, (%s)", DOpTypeOf (n), WNameOf ((wordnode *) LeftOpOf (n)));
			} else {
				/*setindent(fptr, indent); */
				fputs ("( ", fptr);
				pitag (fptr, DOpTypeOf (n));
				fputc (')', fptr);
				printtree (fptr, indent, LeftOpOf (n));
			}
			n = RightOpOf (n);
			break;
			/*}}} */
			/*{{{  literal node */
		case LITNODE:
			if (TagOf (n) == S_CONSTRUCTOR) {
				indent += 2;
				printtree (fptr, indent, LitExpOf (n));
				n = LitTypeOf (n);
			} else {
				wordnode *nptr = StringPtrOf (n);
				printstring (fptr, WNameOf (nptr), WLengthOf (nptr));
				if (LitTypeOf (n) != NULL)
					fprintf (fptr, "(%s)", itagstring (TagOf (LitTypeOf (n))));
				return;
			}
			break;
			/*}}} */
			/*{{{  valof node */
		case VALOFNODE:
			indent += 2;
			printtree (fptr, indent, VLBodyOf (n));
			n = VLResultListOf (n);
			break;
			/*}}} */
			/*{{{  segment node */
		case SEGMENTNODE:
			indent += 2;
			if (TagOf (n) == S_SEGMENTITEM)
				fprintf (fptr, "SOffsetOf: %d", (int) (SOffsetOf (n)));
			printtree (fptr, indent, SNameOf (n));
			printtree (fptr, indent, SStartExpOf (n));
			printtree (fptr, indent, SLengthExpOf (n));
			if (TagOf (n) == S_SEGMENTITEM) {
				printtree (fptr, indent, SSubscriptExpOf (n));
				printtree (fptr, indent, SLengthOf (n));
			}
			n = SCheckExpOf (n);
			break;
			/*}}} */
			/*{{{  name node */
		case NAMENODE:
			switch (TagOf (n)) {
			case T_TEMP:
			case T_REGTEMP:
			case T_PREEVALTEMP:
				fprintf (fptr, "%d, lexlevel = %d ", NVVarNumOf (n), NLexLevelOf (n));
				/*fprintf(fptr, "lexlevel = %d ", NLexLevelOf(n)); */
				if ((TagOf (n) != T_REGTEMP) && (NVOffsetOf (n) != vti_no_slot_value)) {
					fprintf (fptr, "Offset = %d ", NVOffsetOf (n));
				}
				fprintf (fptr, "Type = ");
				pitag (fptr, TagOf (NTypeOf (n)));
				printmode (fptr, NModeOf (n));
				n = NDeclOf (n);
				break;
			case S_FNFORMALRESULT:
			case S_FNACTUALRESULT:
			case S_HIDDEN_PARAM:
			case S_PARAM_STATICLINK:
			case S_PARAM_VSP:
			case S_PARAM_FB:
			case S_PARAM_WS:
		#ifdef MOBILES
			case S_PARAM_MSP:
			case S_PARAM_MPP:
			case S_HIDDEN_TYPE:
		#endif
				fprintf (fptr, "Dim: %d, offset: %d, ArgNo: %d", HDimensionOf (n), NVOffsetOf (n), HArgNoOf (n));
				/* bug 1012 - if we try to see what's inside an actual result,
				   we get stuck into an infinite loop */
				if (TagOf (n) == S_FNACTUALRESULT)
					return;
				indent += 2;
				n = HExpOf (n);
				break;
			default:
				{
					wordnode *nptr = NNameOf (n);
					if (nptr == NULL) {
						fputs ("(name:NULL) ", fptr);
					} else {
						printstring (fptr, WNameOf (nptr), WLengthOf (nptr));
					}
					fprintf (fptr, "Type = ");
					if (NTypeOf (n) == NULL) {
						fprintf (fptr, "NULL ");
					} else {
						treenode *type = NTypeOf (n);

						pitag (fptr, TagOf (type));
						while (TagOf (type) == S_ARRAY) {
							type = ARTypeOf (type);
						}
						if ((TagOf (type) == S_CHAN) || (TagOf (type) == S_PORT) || (TagOf (type) == S_MOBILE)) {
							if (TypeAttrOf (type) & TypeAttr_marked_in) {
								fprintf (fptr, "? ");
							} else if (TypeAttrOf (type) & TypeAttr_marked_out) {
								fprintf (fptr, "! ");
							}
							if (TypeAttrOf (type) & TypeAttr_placed) {
								fprintf (fptr, "[P] ");
							}
						}
					}
					if (NTypeAttrOf (n)) {
						if (NTypeAttrOf (n) & TypeAttr_marked_in) {
							fprintf (fptr, "[C?] ");
						} else if (NTypeAttrOf (n) & TypeAttr_marked_out) {
							fprintf (fptr, "[C!] ");
						}
						if (NTypeAttrOf (n) & TypeAttr_shared) {
							fprintf (fptr, "[S] ");
						}
						if (NTypeAttrOf (n) & TypeAttr_claimed) {
							fprintf (fptr, "[*] ");
						}
						if (NTypeAttrOf (n) & TypeAttr_undefined) {
							fprintf (fptr, "[U] ");
						}
						if (NTypeAttrOf (n) & TypeAttr_fixed) {
							fprintf (fptr, "[F] ");
						}
						if (NTypeAttrOf (n) & TypeAttr_ufixed) {
							fprintf (fptr, "[UF] ");
						}
						if (NTypeAttrOf (n) & TypeAttr_placed) {
							fprintf (fptr, "[P] ");
						}
						if (NTypeAttrOf (n) & TypeAttr_resigned) {
							fprintf (fptr, "[RB] ");
						}
					}
					switch (TagOf (n)) {
					case N_PARAM:
					case N_VALPARAM:
					case N_RESULTPARAM:
					case N_ABBR:
					case N_DECL:
						if (NUndefOf (n)) {
							fprintf (fptr, "[%s] ", fe_udvstatestringof (n));
						}
						if (NVIndirectOf (n)) {
							fprintf (fptr, "[~~] ");
						}
						break;
					}
					if (NFMCheckOf (n)) {
						fprintf (fptr, "[fmcheck] ");
					}
					if (TagOf (n) == N_TAGDEF) {
						fprintf (fptr, "[TV:%d] ", (int)NTValueOf (n));
					}
					printmode (fptr, NModeOf (n));
					return;
				}
			}
			break;
			/*}}} */
			/*{{{  word node */
		case WORDNODE:
			{
				wordnode *w = (wordnode *) n;
				printstring (fptr, WNameOf (w), WLengthOf (w));
			}
			return;
			/*}}} */
			/*{{{  type node */
		case TYPENODE:
			indent += 2;

			/* do type attributes if any */
			if (TypeAttrOf (n) & TypeAttr_shared) {
				fprintf (fptr, " [S]");
			}
			if (TypeAttrOf (n) & TypeAttr_fixed) {
				fprintf (fptr, " [F]");
			}
			if (TypeAttrOf (n) & TypeAttr_ufixed) {
				fprintf (fptr, " [UF]");
			}
			if (TypeAttrOf (n) & TypeAttr_placed) {
				fprintf (fptr, " [P]");
			}

			switch (TagOf (n)) {
			case S_BUFFERED:
				fprintf (fptr, "(%d) ", (int)ARDimOf (n));
				n = ARTypeOf (n);
				break;
			case S_CHAN:
			case S_PORT:
				if (TypeAttrOf (n) & TypeAttr_marked_in) {
					fprintf (fptr, "? ");
				} else if (TypeAttrOf (n) & TypeAttr_marked_out) {
					fprintf (fptr, "! ");
				}
				n = ProtocolOf (n);
				break;
			#ifdef MOBILES
			case S_MOBILE:
				if (TypeTracesOf (n)) {
					printtree (fptr, indent, MTypeOf (n));
					setindent (fptr, indent);
					fprintf (fptr, "TRACES");
					indent += 2;
					n = TypeTracesOf (n);
				} else {
					n = MTypeOf (n);
				}
				break;
			#endif
			default:
				fprintf (fptr, "ARDimOf: %d", (int) ARDimOf (n));
				printtree (fptr, indent, ARDimLengthOf (n));
				n = ARTypeOf (n);
			}
			break;
			/*}}} */
			/*{{{  arraysubnode */
		case ARRAYSUBNODE:
			{
				const BOOL item = (TagOf (n) == S_ARRAYITEM) || (TagOf (n) == S_RECORDITEM);
				indent += 2;
				if (item)
					fprintf (fptr, "ASOffsetOf: %d", ASOffsetOf (n));
				printtree (fptr, indent, ASBaseOf (n));
				if (item) {
					printtree (fptr, indent, ASIndexOf (n));
					printtree (fptr, indent, ASExpOf (n));
					n = ASLengthOf (n);
				} else
					n = ASIndexOf (n);
			}
			break;
			/*}}} */
			/*{{{  alternative node */
		case ALTNODE:
			indent += 2;
			printtree (fptr, indent, AltGuardOf (n));
			printtree (fptr, indent, AltChanExpOf (n));	/* added 2/11/90 bug 779 */
			printtree (fptr, indent, AltInputOf (n));
			n = AltBodyOf (n);
			break;
			/*}}} */
			/*{{{  constant expression node */
		case CONSTEXPNODE:
			fprintf (fptr, "offset: %d", CEOffsetOf (n));
			indent += 2;
			setindent (fptr, indent);
			fprintf (fptr, "%d (Lo:#%X, Hi:#%X)", (int) LoValOf (n), (int) LoValOf (n), (int) HiValOf (n));
			n = CExpOf (n);
			break;
			/*}}} */
			/*{{{  constant table node */
		case CONSTTABLENODE:
			if (TagOf (n) == S_STRING) {
				wordnode *const nptr = (wordnode *) CTValOf (n);
				printstring (fptr, WNameOf (nptr), WLengthOf (nptr));
				return;
			} else if (TagOf (n) == S_CONSTPTR) {
				int i;
				const BYTE *b = (const BYTE *) CTPtrOf (n);
				fprintf (fptr, "(#%p) ", (const void *) b);
				for (i = 0; i < 8; i++)
					fprintf (fptr, "#%-3x", *b++);
				fprintf (fptr, " ...");
				return;
			} else {	/*S_CONSTCONSTRUCTOR */

				wordnode *nptr = (wordnode *) CTValOf (n);
				int i;
				const BYTE *b = (const BYTE *) WNameOf (nptr);
				for (i = 0; i < WLengthOf (nptr); i++) {
					if ((i % 16) == 0)
						setindent (fptr, indent + 2);
					fprintf (fptr, "#%-3x", *b++);
				}
				indent += 2;
				printtree (fptr, indent, ConstTableTypeOf (n));
				n = CTExpOf (n);
				break;
			}
			/*}}} */
			/*{{{  PROCESSOR */
		case PROCESSORNODE:
			fprintf (fptr, "%s, scope: %d",
				 ProcessorTypeOf (n) == NULL ? "NULL" : WNameOf (ProcessorTypeOf (n)), (int) ProcessorScopeOf (n));
			indent += 2;
			printtree (fptr, indent, ProcessorExpOf (n));
			n = ProcessorBodyOf (n);
			break;
			/*}}} */
			/*{{{  space usage node */
		case SPACENODE:
			fprintf (fptr, "maxwsp = %d, datasize = %d, vsusage = %d, nestedvs = %d", SpMaxwspOf (n), SpDatasizeOf (n), SpVSUsageOf (n), SpNestedVSOf (n));
#ifdef MOBILES
			fprintf (fptr, ", msusage = %d, nestedms = %d, wsmap = %p", SpMSUsageOf (n), SpNestedMSOf (n), SpWSMapOf (n));
#endif
			n = SpBodyOf (n);
			break;
			/*}}} */
			/*{{{  conditional expression node */
#ifdef CONDEXP
		case S_CONDEXPNODE:
			indent += 2;
			printtree (fptr, indent, CondExpGuardOf (n));
			printtree (fptr, indent, CondExpTrueOf (n));
			n = CondExpFalseOf (n);
			break;
#endif

			/*}}} */
			/*{{{  confignode */
		case CONFIGNODE:
			indent += 2;
			printtree (fptr, indent, STDevOf (n));
			printtree (fptr, indent, STAttrNameOf (n));
			n = STAttrExpOf (n);
			break;
			/*}}} */
			/*}}} */
		default:
			fprintf (fptr, "printtree: Unknown tag %d", TagOf (n));
			return;
		}
	}
	setindent (fptr, indent);
	fprintf (fptr, "NULL");
}

/*}}}*/
/*{{{  PUBLIC void printstatement (indent, n)     NEVER USED*/
#if 0				/* printstatement is never used */
PUBLIC void printstatement (FILE * const fptr, int indent, treenode * n)
{
	setindent (fptr, indent);
	switch (TagOf (n)) {
		/*{{{  leafnode */
	case S_SEQ:
	case S_IF:
	case S_PAR:
	case S_DO:
	case S_ALT:
	case S_GUY:
	case S_ASM:
	case S_STOP:
	case S_SKIP:
		ptag (fptr, TagOf (n));
		break;
		/*}}} */
		/*{{{  pri and placed */
	case S_PRIPAR:
	case S_PRIALT:
		ptag (fptr, S_PRI);
		putc (' ', fptr);
		ptag (fptr, TagOf (n) == S_PRIPAR ? S_PAR : S_ALT);
		break;
	case S_PLACEDPAR:
		ptag (fptr, S_PLACED);
		putc (' ', fptr);
		ptag (fptr, S_PAR);
		break;
		/*}}} */
		/*{{{  replicated construct node */
	case S_REPLSEQ:
	case S_REPLPAR:
	case S_REPLIF:
	case S_REPLALT:
	case S_PRIREPLPAR:
	case S_PRIREPLALT:
	case S_PLACEDREPLPAR:
	case S_REPLDO:
		{
			int tag1 = 0, tag2 = 0;
			switch (TagOf (n))
				/*{{{  set up tag1 and tag2 */
			{
			case S_REPLSEQ:
				tag2 = S_SEQ;
				break;
			case S_REPLPAR:
				tag2 = S_PAR;
				break;
			case S_REPLIF:
				tag2 = S_IF;
				break;
			case S_REPLALT:
				tag2 = S_ALT;
				break;
			case S_REPLDO:
				tag2 = S_DO;
				break;
			case S_PRIREPLPAR:
				tag1 = S_PRI;
				tag2 = S_PAR;
				break;
			case S_PRIREPLALT:
				tag1 = S_PRI;
				tag2 = S_ALT;
				break;
			case S_PLACEDREPLPAR:
				tag1 = S_PLACED;
				tag2 = S_PAR;
				break;
			}
			/*}}} */
			if (tag1 != 0)
				/*{{{  print tag1 */
			{
				ptag (fptr, tag1);
				putc (' ', fptr);
			}
			/*}}} */
			ptag (fptr, tag2);
			printexp (fptr, ReplCNameOf (n));
			fputs (" = ", fptr);
			printexp (fptr, ReplCStartExpOf (n));
			fputs (" FOR ", fptr);
			printexp (fptr, ReplCLengthExpOf (n));
			if (ReplCStepExpOf (n)) {
				fputs (" STEP ", fptr);
				printexp (fptr, ReplCStepExpOf (n));
			}
		}
		break;
		/*}}} */
		/*{{{  condition node */
	case S_WHILE:
		ptag (fptr, TagOf (n));
		putc (' ', fptr);
		printexp (fptr, CondGuardOf (n));
		break;

	case S_CHOICE:
	case S_SELECTION:
		printexp (fptr, CondGuardOf (n));
		break;
		/*}}} */
		/*{{{  instance node */
	case S_FINSTANCE:
	case S_PINSTANCE:
		printexp (fptr, n);
		break;
		/*}}} */
		/*{{{  action node */
		/*{{{  S_ASS */
	case S_ASS:
		printexp (fptr, LHSOf (n));
		fputs (" := ", fptr);
		printexp (fptr, RHSOf (n));
		break;
		/*}}} */
		/*{{{  S_OUPUT */
	case S_OUTPUT:
		printexp (fptr, LHSOf (n));
		fputs (" ! ", fptr);
		printexp (fptr, RHSOf (n));
		break;

		/*}}} */
		/*{{{  S_INPUT S_TAGGED_INPUT  S_DELAYED_INPUT */
	case S_INPUT:
	case S_TAGGED_INPUT:
	case S_DELAYED_INPUT:
		printexp (fptr, LHSOf (n));
		fputs (" ? ", fptr);
		printexp (fptr, RHSOf (n));
		break;
		/*}}} */
		/*{{{  S_X_INPUT S_X_TAGGED_INPUT */
	case S_X_INPUT:
	case S_X_TAGGED_INPUT:
		printexp (fptr, LHSOf (n));
		fputs (" ?? ", fptr);
		printexp (fptr, RHSOf (n));
		break;
		/*}}}  */
		/*}}} */
		/*{{{  case */
	case S_CASE:
		ptag (fptr, TagOf (n));
		putc (' ', fptr);
		printexp (fptr, LHSOf (n));
		break;
		/*}}} */
		/*{{{  declaration node */
	case S_DECL:
		if (TagOf (DNameOf (n)) == S_LIST)
			printexp (fptr, NTypeOf (ThisItem (DNameOf (n))));
		else
			printexp (fptr, NTypeOf (DNameOf (n)));
		putc (' ', fptr);
		printexp (fptr, DNameOf (n));
		fputs (" :", fptr);
		break;
	case S_VALABBR:
	case S_ABBR:
		if (TagOf (n) == S_VALABBR)
			fputs ("VAL ", fptr);
		printexp (fptr, NTypeOf (DNameOf (n)));
		putc (' ', fptr);
		printexp (fptr, DNameOf (n));
		fputs (" IS ", fptr);
		printexp (fptr, DValOf (n));
		fputs (" :", fptr);
		break;
		/*}}} */
		/*{{{  place node */
	case S_PLACE:
		ptag (fptr, TagOf (n));
		putc (' ', fptr);
		printexp (fptr, DNameOf (n));
		fputs (" AT ", fptr);
		printexp (fptr, DValOf (n));
		fputs (" :", fptr);
		break;
	case S_WSPLACE:
	case S_VSPLACE:
		ptag (fptr, S_PLACE);
		putc (' ', fptr);
		printexp (fptr, DNameOf (n));
		fputs (" IN ", fptr);
		ptag (fptr, TagOf (n));
		fputs (" :", fptr);
		break;
	case S_PLACEON:
		something here.
			/*}}} */
			/*{{{  valof node */
	case S_VALOF:
		ptag (fptr, TagOf (n));
		break;
		/*}}} */
		/*{{{  alternative node */
	case S_ALTERNATIVE:
		printexp (fptr, AltGuardOf (n));
		fputs (" & ", fptr);
		printstatement (0, AltInputOf (n));
		break;
		/*}}} */
	default:
		return;
	}
}
#endif /* printstatement is never used */

/*}}}*/

/*{{{  PRIVATE void printsrcexp*/
PRIVATE void
printsrcexp (FILE * const fptr, treenode * tptr)
{
	if (tptr == NULL)
		fputs ("<NULL>", fptr);
	else if (TagOf (tptr) == S_LIST) {
		for (; !EndOfList (tptr); tptr = NextItem (tptr)) {
			printsrcexp (fptr, ThisItem (tptr));
			if (!EndOfList (NextItem (tptr)))
				fputs (", ", fptr);
		}
	} else if (TagOf (tptr) == S_COLON2) {
		printexp (fptr, LHSOf (tptr));
		fputs (" :: ", fptr);
		printexp (fptr, RHSOf (tptr));
	} else if (isspecification (tptr) || TagOf (tptr) == S_VALOF) {
		fputs ("VALOF ...\n", fptr);
		printsrc (fptr, 10, tptr);
	}
#if 0
	{
		pitag (fptr, S_VALOF);
		fputc ('\n', fptr);
		printsrc (fptr, 10, VLBodyOf (tptr));
		setsrcindent (fptr, 10);
		fputs ("RESULT ", fptr);
		printsrcexp (fptr, VLResultListOf (tptr));
		fputc ('\n', fptr);
	}
#endif
	else
	printexp (fptr, tptr);
}

/*}}}*/
/*{{{  PRIVATE void printprocesslist*/
PRIVATE void printprocesslist (FILE * const fptr, const int indent, treenode * n)
{
	for (; !EndOfList (n); n = NextItem (n))
		printsrc (fptr, indent, ThisItem (n));
}

/*}}}*/
/*{{{  PRIVATE void printsrctype*/
PRIVATE void printsrctype (FILE * const fptr, treenode * type)
{
	while (type != NULL)
		switch (nodetypeoftag (TagOf (type))) {
			/*{{{  LEAFNODE */
		case LEAFNODE:
			pitag (fptr, TagOf (type));
			return;
			/*}}} */
			/*{{{  TYPENODE */
		case TYPENODE:
			switch (TagOf (type)) {
			case S_BUFFERED:
				pitag (fptr, TagOf (type));
				fprintf (fptr, "(%d) ", (int)ARDimOf (type));
				printsrctype (fptr, ARTypeOf (type));
				fputc (' ', fptr);
				return;
			case S_CHAN:
			case S_PORT:
				pitag (fptr, TagOf (type));
				fputs ("OF ", fptr);
				printsrctype (fptr, ProtocolOf (type));
				fputc (' ', fptr);
				return;
			default:
				if (ARDimLengthOf (type) == NULL) {
					fputs ("[]", fptr);
				} else if (ARDimOf (type) == (-1)) {
					fputc ('[', fptr);
					printsrcexp (fptr, ARDimLengthOf (type));
					fputc (']', fptr);
				} else {
					fprintf (fptr, "[%d]", ARDimOf (type));
				}
				type = ARTypeOf (type);
				break;
			}
			break;
			/*}}} */
			/*{{{  LISTNODE */
		case LISTNODE:
			{
				BOOL first = TRUE;
				for (; !EndOfList (type); type = NextItem (type)) {
					if (!first)
						fputs (" ; ", fptr);
					printsrctype (fptr, ThisItem (type));
					first = FALSE;
				}
			}
			return;
			/*}}} */
			/*{{{  NAMENODE */
		case NAMENODE:
			fputs (WNameOf (NNameOf (type)), fptr);
			fputc (' ', fptr);
			return;
			/*}}} */
			/*{{{  DOPNODE */
		case DOPNODE:	/* COLON2 */
			printsrctype (fptr, LeftOpOf (type));
			fputs ("::", fptr);
			type = RightOpOf (type);
			break;
			/*}}} */
			/*{{{  WORDNODE */
		case WORDNODE:
			fprintf (fptr, "WORD{\"%s\"} ", WNameOf ((wordnode *) type));
			return;
			/*}}} */
		default:
			fputs ("???? ", fptr);
			return;
		}
	fputs ("<NULL>", fptr);
}

/*}}}*/
/*{{{  PRIVATE void printsrcparams*/
PRIVATE void printsrcparams (FILE * const fptr, treenode * params)
{
	BOOL first = TRUE;
	fputc ('(', fptr);
	for (; !EndOfList (params); params = NextItem (params))
		if (isnamedformal (ThisItem (params))) {
			if (!first)
				fputs (", ", fptr);
			if (TagOf (ThisItem (params)) == N_VALPARAM)
				fputs ("VAL ", fptr);
			printsrctype (fptr, NTypeOf (ThisItem (params)));
			/*fprintf(fptr, " %s", WNameOf(NNameOf(ThisItem(params)))); */
			fputs (WNameOf (NNameOf (ThisItem (params))), fptr);
			first = FALSE;
		}
	fputs (")\n", fptr);
}

/*}}}*/
/*{{{  PRIVATE void printsrcdecl*/
PRIVATE void printsrcdecl (FILE * const fptr, const int indent, treenode * const tptr, treenode * const nptr)
{
	treenode *const type = NTypeOf (nptr);
	const char *const name = WNameOf (NNameOf (nptr));
	switch (TagOf (tptr)) {
		/*{{{  S_DECL */
	case S_DECL:
		printsrctype (fptr, type);
		/*fprintf(fptr, " %s", name); */
		fputs (name, fptr);
		if (DValOf (tptr) == NULL)
			fputs (":\n", fptr);
		else {
			fputc ('\n', fptr);
			printsrc (fptr, indent + 2, DValOf (tptr));
			setsrcindent (fptr, indent);
			fputs (":\n", fptr);
		}
		break;
		/*}}} */
		/*{{{  S_PROCDEF, S_LFUNCDEF, S_SFUNCDEF */
	case S_PROCDEF:
	case S_LFUNCDEF:
	case S_SFUNCDEF:
		if (TagOf (tptr) != S_PROCDEF) {
			treenode *types;
			BOOL first = TRUE;
			for (types = FnTypeListOf (type); !EndOfList (types); types = NextItem (types)) {
				if (!first) {
					fputc (',', fptr);
				}
				printsrctype (fptr, ThisItem (types));
				first = FALSE;
			}
			fputc (' ', fptr);
		}
		fprintf (fptr, "%s%s %s ", isinline (nptr) ? "INLINE " : "", (TagOf (tptr) == S_PROCDEF) ? "PROC" : "FUNCTION", name);
		printsrcparams (fptr, NParamListOf (nptr));
		if (NPForksOf (nptr)) {
			fprintf (fptr, " (forks)");
		}
		if (NPSuspendsOf (nptr)) {
			fprintf (fptr, " (suspends)");
		}
		if (NPRecursiveOf (nptr)) {
			fprintf (fptr, " (recursive)");
		}
		if (NPDyncallOf (nptr)) {
			fprintf (fptr, " (dyncall)");
		}
		if (NFMCheckOf (nptr)) {
			fprintf (fptr, " [fmcheck]");
		}
		printsrc (fptr, indent + 2, DValOf (tptr));
		setsrcindent (fptr, indent);
		fputs (":\n", fptr);
		break;
		/*}}} */
		/*{{{  S_ABBR / RETYPE */
	case S_ABBR:
	case S_VALABBR:
	case S_RETYPE:
	case S_VALRETYPE:
		if (TagOf (nptr) == N_VALABBR || TagOf (nptr) == N_VALRETYPE)
			fputs ("VAL ", fptr);
		printsrctype (fptr, type);
		fprintf (fptr, " %s %s ", name, (TagOf (nptr) == N_ABBR || TagOf (nptr) == N_VALABBR) ? "IS"
#ifdef OCCAM2_5
			 : NVReshapesOf (nptr) ? "RESHAPES"
#endif
			 : "RETYPES");
		printsrcexp (fptr, DValOf (tptr));
		fputs (":\n", fptr);
		break;
		/*}}} */
		/*{{{  S_SPROTDEF */
	case S_SPROTDEF:
		fprintf (fptr, "PROTOCOL %s IS ", name);
		printsrctype (fptr, type);
		fputs (":\n", fptr);
		break;
		/*}}} */
		/*{{{  S_TPROTDEF */
	case S_TPROTDEF:
		{
			treenode *branch;
			fprintf (fptr, "PROTOCOL %s\n", name);
			setsrcindent (fptr, indent + 2);
			fputs ("CASE\n", fptr);
			for (branch = type; !EndOfList (branch); branch = NextItem (branch)) {
				setsrcindent (fptr, indent + 4);
				printsrcexp (fptr, ThisItem (branch));
				if (!EndOfList (NTypeOf (ThisItem (branch)))) {
					fputs ("; ", fptr);
					printsrctype (fptr, NTypeOf (ThisItem (branch)));
				}
				fputc ('\n', fptr);
			}
			setsrcindent (fptr, indent);
			fputs (":\n", fptr);
		}
		break;
		/*}}} */
		/*{{{  S_PRAGMA */
	case S_PRAGMA:
		fprintf (fptr, "#PRAGMA %s ", name);
		printsrcexp (fptr, DValOf (tptr));
		fputc ('\n', fptr);
		break;
		/*}}} */
		/*{{{  S_LABELDEF: */
	case S_LABELDEF:
		fprintf (fptr, "%s\n", name);
		break;
		/*}}} */
		/*{{{  S_CONFIG: S_NETWORK: S_MAPPING: */
	case S_CONFIG:
	case S_NETWORK:
	case S_MAPPING:
		fputs (itagstring (TagOf (tptr)), fptr);
		fputc ('\n', fptr);
		printsrc (fptr, indent + 2, DValOf (tptr));
		setsrcindent (fptr, indent);
		fputs (":\n", fptr);
		break;
		/*}}} */
#if MOBILES
		/*{{{  S_FORWDECL */
	case S_FORWDECL:
		fprintf (fptr, "%s TYPE %s:\n", ((TagOf (type) == S_RECORD) && ((TypeAttrOf (type) & TypeAttr_chantype) != 0)) ? "CHAN" : "DATA", name);
		break;
		/*}}}*/
#endif
		/*{{{  S_TYPEDECL */
#ifdef OCCAM2_5
	case S_TYPEDECL:
		fprintf (fptr, "%s TYPE %s", ((TagOf (type) == S_RECORD) && ((TypeAttrOf (type) & TypeAttr_chantype) != 0)) ? "CHAN" : "DATA", name);
		if (TagOf (type) == S_RECORD) {
			fputc ('\n', fptr);
			setsrcindent (fptr, indent + 2);
			fputs ("RECORD\n", fptr);
			printsrc (fptr, indent + 4, ARTypeOf (type));
			setsrcindent (fptr, indent);
			fputs (":\n", fptr);
		} else {
			fputs (" IS ", fptr);
			printsrctype (fptr, type);
			fputs (":\n", fptr);
		}
		break;
#endif
		/*}}} */
#ifdef MOBILES
		/*{{{  S_PROCTYPEDECL*/
	case S_PROCTYPEDECL:
		fprintf (fptr, "PROC TYPE %s IS ", name);
		printsrcparams (fptr, type);
		fprintf (fptr, ":\n");
		break;
		/*}}}*/
		/*{{{  S_MPROCDECL*/
	case S_MPROCDECL:
		fprintf (fptr, "MOBILE PROC %s ", name);
		printsrcparams (fptr, NParamListOf (nptr));
		if (DExtraOf (tptr)) {
			fprintf (fptr, " IMPLEMENTS %s", WNameOf (NNameOf (DExtraOf (tptr))));
		}
		if (NPForksOf (nptr)) {
			fprintf (fptr, " (forks)");
		}
		if (NPSuspendsOf (nptr)) {
			fprintf (fptr, " (suspends)");
		}
		if (NPRecursiveOf (nptr)) {
			fprintf (fptr, " (recursive)");
		}
		if (NPDyncallOf (nptr)) {
			fprintf (fptr, " (dyncall)");
		}
		printsrc (fptr, indent + 2, DValOf (tptr));
		setsrcindent (fptr, indent);
		fprintf (fptr, ":\n");
		break;
		/*}}}*/
#endif
	default:
		fprintf (fptr, "%s printsrcdecl: Unknown decl :\n", itagstring (TagOf (nptr)));
		break;
	}
	/*fprintf(fptr, "decl at indent %d\n", indent); */
}

/*}}}*/
/*{{{  PUBLIC void printsrc(indent, n)*/
PUBLIC void printsrc (FILE * const fptr, int indent, treenode * n)
{
	while (n != NULL) {
		setsrcindent (fptr, indent);
		switch (nodetypeoftag (TagOf (n))) {
			/*{{{  cases */
			/*{{{  declaration node */
		case DECLNODE:
			if (TagOf (DNameOf (n)) == S_LIST) {
				treenode *list;
				for (list = DNameOf (n); !EndOfList (list); list = NextItem (list)) {
					printsrcdecl (fptr, indent, n, ThisItem (list));
					if (!EndOfList (NextItem (list))) {
						setsrcindent (fptr, indent);
					}
				}
			} else {
				printsrcdecl (fptr, indent, n, DNameOf (n));
			}
			n = DBodyOf (n);
			break;
			/*}}} */
			/*{{{  leaf node */
		case LEAFNODE:
			switch (TagOf (n)) {
			case S_END:
				break;
			case S_SKIP:
			case S_STOP:
			case S_SUSPEND:
				pitag (fptr, TagOf (n));
				fputc ('\n', fptr);
				break;
			default:
				pitag (fptr, TagOf (n));
				break;
			}
			return;
			/*}}} */
			/*{{{  construct node */
		case CNODE:
			pitag (fptr, TagOf (n));
			fputc ('\n', fptr);
			printprocesslist (fptr, indent + 2, CBodyOf (n));
			return;
			/*}}} */
			/*{{{  replicated construct node */
		case REPLCNODE:
			pitag (fptr, TagOf (n));
			fprintf (fptr, " %s = ", WNameOf (NNameOf (ReplCNameOf (n))));
			printsrcexp (fptr, ReplCStartExpOf (n));
			fputs (" FOR ", fptr);
			printsrcexp (fptr, ReplCLengthExpOf (n));
			if (ReplCStepExpOf (n)) {
				fputs (" STEP ", fptr);
				printsrcexp (fptr, ReplCStepExpOf (n));
			}
			fputc ('\n', fptr);
			indent += 2;
			n = ReplCBodyOf (n);
			break;
			/*}}} */
			/*{{{  condition node */
		case CONDNODE:
			if (TagOf (n) == S_WHILE) {
				pitag (fptr, S_WHILE);
			}
			printsrcexp (fptr, CondGuardOf (n));
			fputc ('\n', fptr);
			indent += 2;
			if (TagOf (n) == S_X_VARIANT) {
				/* extended variant */
				printsrc (fptr, indent, VRDuringOf (n));
				n = VRAfterOf (n);
			} else {
				n = CondBodyOf (n);
			}
			break;
			/*}}} */
			/*{{{  instance node */
		case INSTANCENODE:
			{
				BOOL first = TRUE;
				/*pitag(fptr, TagOf(n)); */
				fprintf (fptr, "%s (", WNameOf (NNameOf (INameOf (n))));

				for (n = IParamListOf (n); !EndOfList (n); n = NextItem (n)) {
					if (!first)
						fputs (", ", fptr);
					printsrcexp (fptr, ThisItem (n));
					first = FALSE;
				}
				fputs (")\n", fptr);
			}
			return;
			/*}}} */
			/*{{{  action node */
		case ACTIONNODE:
			if (TagOf (n) == S_CASE) {
				pitag (fptr, S_CASE);
				fputc (' ', fptr);
				printsrcexp (fptr, LHSOf (n));
				fputc ('\n', fptr);
				printprocesslist (fptr, indent + 2, RHSOf (n));
				return;
			}
			printsrcexp (fptr, LHSOf (n));
			switch (TagOf (n)) {
			case S_CASE_INPUT:
				fputs (" ? CASE\n", fptr);
				printprocesslist (fptr, indent + 2, RHSOf (n));
				return;
			case S_X_CASE_INPUT:
				fputs (" ?? CASE\n", fptr);
				printprocesslist (fptr, indent + 2, RHSOf (n));
				return;
			case S_ASS:
				fputs (" := ", fptr);
				break;
			case S_DELAYED_INPUT:
				fputs (" ? AFTER ", fptr);
				break;
			case S_INPUT:
				fputs (" ? ", fptr);
				break;
			case S_X_INPUT:
				fputs (" ?? ", fptr);
				printsrcexp (fptr, RHSOf (n));
				fputc ('\n', fptr);
				printsrc (fptr, indent + 2, ActionDuringOf (n));
				printsrc (fptr, indent + 2, ActionAfterOf (n));
				return;
			case S_OUTPUT:
				fputs (" ! ", fptr);
				break;
			case S_TAGGED_INPUT:
				fputs (" ? CASE ", fptr);
				break;
			case S_X_TAGGED_INPUT:
				fputs (" ?? CASE ", fptr);
				printsrcexp (fptr, RHSOf (n));
				fputc ('\n', fptr);
				printsrc (fptr, indent + 2, ActionDuringOf (n));
				printsrc (fptr, indent + 2, ActionAfterOf (n));
				return;
			}
			printsrcexp (fptr, RHSOf (n));
			if (ActionTypeOf (n) != NULL) {
				fputs (" ( ", fptr);
				printsrctype (fptr, ActionTypeOf (n));
				fputs (")", fptr);
			}
			fputc ('\n', fptr);
			return;
			/*}}} */
			/*{{{  dyadic operator node */
		case DOPNODE:
			/* S_GUYCODE or S_GUYSTEP */
			fprintf (fptr, "Code: #%X, (%s) ", DOpTypeOf (n), WNameOf ((wordnode *) LeftOpOf (n)));
			printsrcexp (fptr, RightOpOf (n));
			fputc ('\n', fptr);
			return;
			/*}}} */
			/*{{{  alternative node */
		case ALTNODE:
			printsrcexp (fptr, AltGuardOf (n));
			fputs (" & ", fptr);
			printsrcexp (fptr, AltChanExpOf (n));
			if (AltTimeExpOf (n) != NULL) {
				fputs ("? AFTER ", fptr);
				printsrcexp (fptr, AltTimeExpOf (n));
			} else if (AltInputOf (n) == NULL)
				fputs ("; <NULL>\n", fptr);
			else {
				fputs (";\n", fptr);
				printsrc (fptr, indent, AltInputOf (n));
			}
			indent += 2;
			n = AltBodyOf (n);
			break;
			/*}}} */
			/*{{{  space usage node */
		case SPACENODE:
			/*fprintf(fptr, "maxwsp = %ld, datasize = %ld", SpMaxwspOf(n),
			   SpDatasizeOf(n)); */
			n = SpBodyOf (n);
			break;
			/*}}} */
			/*{{{  valof node */
		case VALOFNODE:
			pitag (fptr, TagOf (n));
			fputc ('\n', fptr);
			indent += 2;
			printsrc (fptr, indent, VLBodyOf (n));
			setsrcindent (fptr, indent);
			fputs ("RESULT ", fptr);
			printsrcexp (fptr, VLResultListOf (n));
			fputc ('\n', fptr);
			return;
			/*}}} */
			/*{{{  config node */
		case CONFIGNODE:
			pitag (fptr, TagOf (n));
			printsrcexp (fptr, ConnectFromEdgeOf (n));
			fputs (", ", fptr);
			printsrcexp (fptr, ConnectToEdgeOf (n));
			if (ConnectArcOf (n) != NULL) {
				fputs (", ", fptr);
				printsrcexp (fptr, ConnectArcOf (n));
			}
			fputc ('\n', fptr);
			return;
			/*}}} */
#if 0
			/*{{{  never used */
			/*{{{  list node */
#if 0
		case LISTNODE:
			if (TagOf (n) == S_FNTYPE) {
				indent += 2;
				printsrc (fptr, indent, FnTypeListOf (n));
				n = FnParamsOf (n);
			} else {
				/*indent += 2;
				   printsrc(fptr, indent, ThisItem(n)); */
				printsrc (fptr, indent + 2, ThisItem (n));
				n = NextItem (n);
			}
			break;
#endif
			/*}}} */
			/*{{{  monadic operator node */
		case MOPNODE:
			indent += 2;
			switch (TagOf (n)) {
			default:
				setindent (fptr, indent);
				pitag (fptr, MOpTypeOf (n));
				n = OpOf (n);
				break;
			case S_ELSIZE:
				n = dimexpof (OpOf (n), 0);
				break;
			case S_SEGSTART:
				n = SStartExpOf (OpOf (n));
				break;
			}
			break;
			/*}}} */

			/*{{{  literal node */
		case LITNODE:
			if (TagOf (n) == S_CONSTRUCTOR) {
				indent += 2;
				n = LitExpOf (n);
			} else {
				wordnode *nptr = StringPtrOf (n);
				printstring (fptr, WNameOf (nptr), WLengthOf (nptr));
				if (LitTypeOf (n) != NULL)
					printsrctype (fptr, LitTypeOf (n));
				return;
			}
			break;
			/*}}} */
			/*{{{  segment node */
		case SEGMENTNODE:
			indent += 2;
			if (TagOf (n) == S_SEGMENTITEM)
				fprintf (fptr, "SOffsetOf: %d", (int) (SOffsetOf (n)));
			printsrc (fptr, indent, SNameOf (n));
			printsrc (fptr, indent, SStartExpOf (n));
			printsrc (fptr, indent, SLengthExpOf (n));
			if (TagOf (n) == S_SEGMENTITEM) {
				printsrc (fptr, indent, SSubscriptExpOf (n));
				printsrc (fptr, indent, SLengthOf (n));
			}
			n = SCheckExpOf (n);
			break;
			/*}}} */

			/*{{{  name node */
		case NAMENODE:
			if ((TagOf (n) == T_TEMP) || (TagOf (n) == T_REGTEMP) || (TagOf (n) == T_PREEVALTEMP)) {
				fprintf (fptr, "%ld, lexlevel = %d ", NVVarNumOf (n), NLexLevelOf (n));
				/*fprintf(fptr, "lexlevel = %d ", NLexLevelOf(n)); */
				if ((TagOf (n) != T_REGTEMP) && (NVOffsetOf (n) != vti_no_slot_value))
					fprintf (fptr, "Offset = %ld ", NVOffsetOf (n));
				fprintf (fptr, "Type = ");
				pitag (fptr, TagOf (NTypeOf (n)));
				printmode (fptr, NModeOf (n));
				n = NDeclOf (n);
			} else {
				wordnode *nptr = NNameOf (n);
				printstring (fptr, WNameOf (nptr), WLengthOf (nptr));
				fprintf (fptr, "Type = ");
				if (NTypeOf (n) == NULL)
					fprintf (fptr, "NULL ");
				else
					pitag (fptr, TagOf (NTypeOf (n)));
				printmode (fptr, NModeOf (n));
				return;
			}
			break;
			/*}}} */
			/*{{{  word node */
		case WORDNODE:
			{
				wordnode *w = (wordnode *) n;
				printstring (fptr, WNameOf (w), WLengthOf (w));
			}
			return;
			/*}}} */
			/*{{{  arraysubnode */
		case ARRAYSUBNODE:
			indent += 2;
			if (TagOf (n) == S_ARRAYITEM || TagOf (n) == S_RECORDITEM)
				fprintf (fptr, "ASOffsetOf: %d", (int) (ASOffsetOf (n)));
			printsrc (fptr, indent, ASBaseOf (n));
			if (TagOf (n) == S_ARRAYITEM || TagOf (n) == S_RECORDITEM) {
				printsrc (fptr, indent, ASExpOf (n));
				n = ASLengthOf (n);
			} else
				n = ASIndexOf (n);
			break;
			/*}}} */

			/*{{{  constant expression node */
		case CONSTEXPNODE:
			fprintf (fptr, "offset: %ld", CEOffsetOf (n));
			indent += 2;
			setindent (fptr, indent);
			fprintf (fptr, "%d (Lo:#%X, Hi:#%X)", (int) LoValOf (n), (int) LoValOf (n), (int) HiValOf (n));
			n = CExpOf (n);
			break;
			/*}}} */
			/*{{{  constant table node */
		case CONSTTABLENODE:
			if (TagOf (n) == S_STRING) {
				wordnode *nptr = (wordnode *) CTValOf (n);
				printstring (fptr, WNameOf (nptr), WLengthOf (nptr));
				return;
			} else {	/*S_CONSTCONSTRUCTOR */

				wordnode *nptr = (wordnode *) CTValOf (n);
				int i;
				const BYTE *b = (const BYTE *) WNameOf (nptr);
				for (i = 0; i < WLengthOf (nptr); i++) {
					if ((i % 16) == 0)
						setindent (fptr, indent + 2);
					fprintf (fptr, "#%-3x", *b++);
				}
				indent += 2;
				printsrctype (fptr, ConstTableTypeOf (n));
				n = CTExpOf (n);
				break;
			}
			/*}}} */
			/*{{{  hidden parameter node */
		case HIDDENPARAMNODE:
			fprintf (fptr, "Dim/offset: %d", HDimensionOf (n));
			/* bug 1012 - if we try to see what's inside an actual result,
			   we get stuck into an infinite loop */
			if (TagOf (n) == S_FNACTUALRESULT)
				return;
			indent += 2;
			n = HExpOf (n);
			break;
			/*}}} */
			/*{{{  PROCESSOR */
		case PROCESSORNODE:
			fprintf (fptr, "%s, scope: %d",
				 ProcessorTypeOf (n) == NULL ? "NULL" : WNameOf (ProcessorTypeOf (n)), (int) ProcessorScopeOf (n));
			indent += 2;
			printsrc (fptr, indent, ProcessorExpOf (n));
			n = ProcessorBodyOf (n);
			break;
			/*}}} */

			/*{{{  conditional expression node */
#ifdef CONDEXP
		case S_CONDEXPNODE:
			indent += 2;
			printsrc (fptr, indent, CondExpGuardOf (n));
			printsrc (fptr, indent, CondExpTrueOf (n));
			n = CondExpFalseOf (n);
			break;
#endif

			/*}}} */
			/*{{{  confignode */
		case CONFIGNODE:
			indent += 2;
			printsrc (fptr, indent, STDevOf (n));
			printsrc (fptr, indent, STAttrNameOf (n));
			n = STAttrExpOf (n);
			break;
			/*}}} */
			/*}}} */
#endif
			/*}}} */
		default:
			fprintf (fptr, "printsrc: Unknown tag %s\n", itagstring (TagOf (n)));
			return;
		}
	}
}

/*}}}*/
