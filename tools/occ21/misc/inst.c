/* $Id: inst.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	Instruction handling for GUY code inserts
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

/*{{{  include files */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include <string.h>
#include "suplib.h" /* IMPORTED */

#include "midinc.h"

#include "instdef.h"
#include "instruct.h"

#include "harndef.h"
/*}}}  */

/*{{{  definitions */
#define N_PRIMARIES 16
/*}}}  */
/*{{{  private variables */
/* This is just used when disassembling; if it is necessary to increase
   this because a new instruction is invented which has a longer name,
   then it is ok to simply increase it.
   CON - 7/5/91
*/
#define MAX_INSTRUCTION_NAME_LEN 20

PRIVATE const BOOL trueval = TRUE;
PRIVATE char instname[MAX_INSTRUCTION_NAME_LEN];
/*}}}  */
/*{{{  instruction table */
#if 0
#define OPRD_ATTR (0x000F) /* Count of expected number of operands */
#define ELEM_ATTR (0x0010) /* Set if this is a store */
#define PTRS_ATTR (0x0100) /* Set if this allows ADDRESSOF in ASM */
#define GUYS_ATTR (0x1000) /* Set if only allowed in FULL GUYs */
#else
#define OPRD_ATTR (0x000F) /* Count of expected number of operands */
#define ELEM_ATTR (0x0010) /* Set if this is a store */
#define PTRS_ATTR (0x0020) /* Set if this allows ADDRESSOF in ASM */
#define GUYS_ATTR (0x0040) /* Set if only allowed in FULL GUYs */
#endif
/* (when = OPRD_ATTR, it means 'any number' */
PRIVATE const struct instruction_struct
  {
    int ivalue;
    const char *iname;
    const BOOL *ivalid;
    int   iattr; /* 0xGABC, where A=ptrs_allowed,
				  B=element_required, C=operands,
				  G=Full guys/asm only */
  } instructions[] =
  {
    /*{{{  primaries */
    { I_LDL     | I_PRIMARY, "LDL",  &trueval, 1 },
    { I_STL     | I_PRIMARY, "STL",  &trueval, 1 },
    { I_LDLP    | I_PRIMARY, "LDLP", &trueval, 1 },
    { I_LDNL    | I_PRIMARY, "LDNL", &trueval, 1 },
    { I_STNL    | I_PRIMARY, "STNL", &trueval, 1 },
    { I_LDNLP   | I_PRIMARY, "LDNLP",&trueval, 1 },
    { I_EQC     | I_PRIMARY, "EQC",  &trueval, 1 },
    { I_LDC     | I_PRIMARY, "LDC",  &trueval, 1 },
    { I_ADC     | I_PRIMARY, "ADC",  &trueval, 1 },
    { I_J       | I_PRIMARY, "J",    &trueval, 1 },
    { I_CJ      | I_PRIMARY, "CJ",   &trueval, 1 },
    { I_CALL    | I_PRIMARY, "CALL", &trueval, GUYS_ATTR | 1 },
    { I_AJW     | I_PRIMARY, "AJW",  &trueval, GUYS_ATTR | 1 },
    { I_PFIX    | I_PRIMARY, "PFIX", &trueval, GUYS_ATTR | 1 },
    { I_NFIX    | I_PRIMARY, "NFIX", &trueval, GUYS_ATTR | 1 },
    { I_OPR     | I_PRIMARY, "OPR",  &trueval, GUYS_ATTR | 1 },
    /*}}}  */
    /*{{{  secondaries */
    /*{{{  rev, ret, ldpi, gajw, gcall, mint, lend */
    { I_REV,   "REV",   &trueval, 0 },
    { I_RET  , "RET",   &trueval, GUYS_ATTR },
    { I_LDPI,  "LDPI",  &trueval, 0 },
    { I_GAJW , "GAJW",  &trueval, GUYS_ATTR },
    { I_GCALL, "GCALL", &trueval, GUYS_ATTR },
    { I_MINT,  "MINT",  &trueval, 0 },
    { I_LEND , "LEND",  &trueval, GUYS_ATTR },
    /*}}}  */
    /*{{{  csub0, ccnt1 */
    { I_CSUB0,   "CSUB0",   &trueval, 0 },
    { I_CCNT1,   "CCNT1",   &trueval, 0 },
    /*}}}  */
    /*{{{  testerr, seterr, stoperr, clrhalterr, sethalterr, testhalterr */
    { I_TESTERR,    "TESTERR",     &tx_global.hastserieserrors, 0 },
    { I_SETERR,     "SETERR",      &tx_global.hastserieserrors, 0 },
    { I_STOPERR   , "STOPERR",     &tx_global.hastserieserrors, GUYS_ATTR },
    { I_CLRHALTERR, "CLRHALTERR",  &tx_global.hastserieserrors, GUYS_ATTR },
    { I_SETHALTERR, "SETHALTERR",  &tx_global.hastserieserrors, GUYS_ATTR },
    { I_TESTHALTERR,"TESTHALTERR", &tx_global.hastserieserrors, 0 },
    /*}}}  */
    /*{{{  bsub, wsub, bcnt, wcnt */
    { I_BSUB,    "BSUB",    &trueval, 0 },
    { I_WSUB,    "WSUB",    &trueval, 0 },
    { I_BCNT,    "BCNT",    &trueval, 0 },
    { I_WCNT,    "WCNT",    &trueval, 0 },
    /*}}}  */
    /*{{{  lb, sb, move */
    { I_LB,      "LB",      &trueval, 0 },
    { I_SB,      "SB",      &trueval, 0 },
    { I_MOVE,    "MOVE",    &trueval, 0 },
    /*}}}  */
    /*{{{  and, or, xor, not, shl, shr, add, sub, mul, div, rem, gt, diff, sum, prod */
    { I_AND,     "AND",     &trueval, 0 },
    { I_OR,      "OR",      &trueval, 0 },
    { I_XOR,     "XOR",     &trueval, 0 },
    { I_NOT,     "NOT",     &trueval, 0 },
    { I_SHL,     "SHL",     &trueval, 0 },
    { I_SHR,     "SHR",     &trueval, 0 },
    { I_ADD,     "ADD",     &trueval, 0 },
    { I_SUB,     "SUB",     &trueval, 0 },
    { I_MUL,     "MUL",     &trueval, 0 },
    { I_DIV,     "DIV",     &trueval, 0 },
    { I_REM,     "REM",     &trueval, 0 },
    { I_GT,      "GT",      &trueval, 0 },
    { I_DIFF,    "DIFF",    &trueval, 0 },
    { I_SUM,     "SUM",     &trueval, 0 },
    { I_PROD,    "PROD",    &trueval, 0 },
    /*}}}  */
    /*{{{  startp, endp, runp, stopp, ldpri */
    { I_STARTP , "STARTP",  &trueval, GUYS_ATTR },
    { I_ENDP   , "ENDP",    &trueval, GUYS_ATTR },
    { I_RUNP   , "RUNP",    &trueval, GUYS_ATTR },
    { I_STOPP  , "STOPP",   &trueval, GUYS_ATTR },
    { I_LDPRI  , "LDPRI",   &trueval, 0 },
    /*}}}  */
    /*{{{  in, out, outword, outbyte, resetch */
    { I_IN     , "IN",      &trueval, GUYS_ATTR },
    { I_OUT    , "OUT",     &trueval, GUYS_ATTR },
    { I_OUTWORD, "OUTWORD", &trueval, GUYS_ATTR },
    { I_OUTBYTE, "OUTBYTE", &trueval, GUYS_ATTR },
    { I_RESETCH, "RESETCH", &trueval, GUYS_ATTR },
    /*}}}  */
    /*{{{  alt, altwt, altend, enbs, diss, enbc, ldtimer, tin, talt, taltwt,enbt,dist */
    { I_ALT    , "ALT",     &trueval, GUYS_ATTR },
    { I_ALTWT  , "ALTWT",   &trueval, GUYS_ATTR },
    { I_ALTEND , "ALTEND",  &trueval, GUYS_ATTR },
    { I_ENBS   , "ENBS",    &trueval, GUYS_ATTR },
    { I_DISS   , "DISS",    &trueval, GUYS_ATTR },
    { I_ENBC   , "ENBC",    &trueval, GUYS_ATTR },
    { I_DISC   , "DISC",    &trueval, GUYS_ATTR },
    { I_LDTIMER, "LDTIMER", &trueval, 0 },
    { I_TIN    , "TIN",     &trueval, GUYS_ATTR },
    { I_TALT   , "TALT",    &trueval, GUYS_ATTR },
    { I_TALTWT , "TALTWT",  &trueval, GUYS_ATTR },
    { I_ENBT   , "ENBT",    &trueval, GUYS_ATTR },
    { I_DIST   , "DIST",    &trueval, GUYS_ATTR },
    /*}}}  */
    /*{{{  xword, cword, xdble, csngl, norm */
    { I_XWORD,   "XWORD",   &trueval, 0 },
    { I_CWORD,   "CWORD",   &trueval, 0 },
    { I_XDBLE,   "XDBLE",   &trueval, 0 },
    { I_CSNGL,   "CSNGL",   &trueval, 0 },
    { I_NORM,    "NORM",    &trueval, 0 },
    /*}}}  */
    /*{{{  ladd, lsub, lsum, ldiff, lmul, ldiv, lshl,lshr */
    { I_LADD,    "LADD",    &trueval, 0 },
    { I_LSUB,    "LSUB",    &trueval, 0 },
    { I_LSUM,    "LSUM",    &trueval, 0 },
    { I_LDIFF,   "LDIFF",   &trueval, 0 },
    { I_LMUL,    "LMUL",    &trueval, 0 },
    { I_LDIV,    "LDIV",    &trueval, 0 },
    { I_LSHL,    "LSHL",    &trueval, 0 },
    { I_LSHR,    "LSHR",    &trueval, 0 },
    /*}}}  */
    /*{{{  testpranal, saveh, savel, sthf, sthb, stlf, stlb, trap, sttimer */
    { I_TESTPRANAL, "TESTPRANAL", &trueval, 0 },
    { I_SAVEH,      "SAVEH",      &tx_global.hastseriesscheduler, GUYS_ATTR },
    { I_SAVEL,      "SAVEL",      &tx_global.hastseriesscheduler, GUYS_ATTR },
    { I_STHF ,      "STHF",       &tx_global.hastseriesscheduler, GUYS_ATTR },
    { I_STHB ,      "STHB",       &tx_global.hastseriesscheduler, GUYS_ATTR },
    { I_STLF ,      "STLF",       &tx_global.hastseriesscheduler, GUYS_ATTR },
    { I_STLB ,      "STLB",       &tx_global.hastseriesscheduler, GUYS_ATTR },
    { I_TRAP ,      "TRAP",       &trueval, GUYS_ATTR },
    { I_NULL ,      "NULL",       &trueval, 0 },
    { I_STTIMER,    "STTIMER",    &trueval, 0 },
    /*}}}  */
    /*{{{  unpacksn, roundsn, postnormsn, ldinf, cflerr */
    { I_UNPACKSN,   "UNPACKSN",   &has_fp_support, 0 },
    { I_ROUNDSN,    "ROUNDSN",    &has_fp_support, 0 },
    { I_POSTNORMSN, "POSTNORMSN", &has_fp_support, 0 },
    { I_LDINF,      "LDINF",      &has_fp_support, 0 },
    { I_CFLERR,     "CFLERR",     &has_fp_support, 0 },
    /*}}}  */
    /*{{{  fmul */
    { I_FMUL,    "FMUL",    &has_fmul, 0 },
    /*}}}  */
    /*{{{  start, testhardchan, testldd, testlde, testlds, teststd, testste, teststs */
    { I_START       , "START",        &trueval, GUYS_ATTR },
    { I_TESTHARDCHAN, "TESTHARDCHAN", &trueval, GUYS_ATTR },
    { I_TESTLDD     , "TESTLDD",      &trueval, GUYS_ATTR },
    { I_TESTLDE     , "TESTLDE",      &trueval, GUYS_ATTR },
    { I_TESTLDS     , "TESTLDS",      &trueval, GUYS_ATTR },
    { I_TESTSTD     , "TESTSTD",      &trueval, GUYS_ATTR },
    { I_TESTSTE     , "TESTSTE",      &trueval, GUYS_ATTR },
    { I_TESTSTS     , "TESTSTS",      &trueval, GUYS_ATTR },
    /*}}}  */
    /*{{{  move2dinit, move2dall, move2dnonzero, move2dzero */
    { I_MOVE2DINIT,    "MOVE2DINIT",    &has_move2d, 0 },
    { I_MOVE2DALL,     "MOVE2DALL",     &has_move2d, 0 },
    { I_MOVE2DNONZERO, "MOVE2DNONZERO", &has_move2d, 0 },
    { I_MOVE2DZERO,    "MOVE2DZERO",    &has_move2d, 0 },
    /*}}}  */
    /*{{{  crcword, crcbyte, bitcnt, bitrevword, bitrevnbits */
    { I_CRCWORD,     "CRCWORD",     &has_crc, 0 },
    { I_CRCBYTE,     "CRCBYTE",     &has_crc, 0 },
    { I_BITCNT,      "BITCNT",      &has_bitops, 0 },
    { I_BITREVWORD,  "BITREVWORD",  &has_bitops, 0 },
    { I_BITREVNBITS, "BITREVNBITS", &has_bitops, 0 },
    /*}}}  */
    /*{{{  dup, wsubdb */
    { I_DUP,     "DUP",      &has_dup, 0 },
    { I_WSUBDB,  "WSUBDB",   &has_wsubdb, 0 },
    /*}}}  */
    /*{{{  KRoC dynamic memory support: mnew, mfree, malloc, mrelease */
#if 0
    { I_MNEW,		"MNEW",		&trueval, 0 },
    { I_MFREE,		"MFREE",	&trueval, 0 },
#endif
    { I_MALLOC,		"MALLOC",	&trueval, 0 },
    { I_MRELEASE,	"MRELEASE",	&trueval, 0 },
    { I_MRELEASEP,	"MRELEASEP",	&trueval, 0 },
    /*}}}  */
#if 0 /* def MOBILES */
    /*{{{  MOBILE input/output */
    { I_MIN,		"MIN",		&trueval, 0 },
    { I_MOUT,		"MOUT",		&trueval, 0 },
    { I_MIN64,		"MIN64",	&trueval, 0 },
    { I_MOUT64,		"MOUT64",	&trueval, 0 },
    { I_MINN,		"MINN",		&trueval, 0 },
    { I_MOUTN,		"MOUTN",	&trueval, 0 },
    /*}}}  */
#endif
    /*{{{  extended rendezvous stuff */
    { I_XABLE,		"XABLE",	&trueval, 0 },
    { I_XIN,		"XIN",		&trueval, 0 },
#if 0
    { I_XMIN,		"XMIN",		&trueval, 0 },
    { I_XMIN64,		"XMIN64",	&trueval, 0 },
    { I_XMINN,		"XMINN",	&trueval, 0 },
#endif
    { I_XEND,		"XEND",		&trueval, 0 },
    /*}}}  */
    /*{{{  modified ALT stuff */
    { I_NDISC,		"NDISC",	&trueval, 0 },
    { I_NDIST,		"NDIST",	&trueval, 0 },
    { I_NDISS,		"NDISS",	&trueval, 0 },
    { I_ENBC3,		"ENBC3",	&trueval, 0 },
    { I_ENBT3,		"ENBT3",	&trueval, 0 },
    { I_ENBS3,		"ENBS3",	&trueval, 0 },
    /*}}}  */
    /*{{{  special (external) channel handling */
    { I_EXTIN,		"EXTIN",	&trueval, 0 },
    { I_EXTOUT,		"EXTOUT",	&trueval, 0 },
    { I_EXTVRFY,	"EXTVRFY",	&trueval, 0 },
    { I_EXTENBC,	"EXTENBC",	&trueval, 0 },
    { I_EXTNDISC,	"EXTNDISC",	&trueval, 0 },
#if 0
    { I_EXTMIN,		"EXTMIN",	&trueval, 0 },
    { I_EXTMOUT,	"EXTMOUT",	&trueval, 0 },
    { I_EXTMIN64,	"EXTMIN64",	&trueval, 0 },
    { I_EXTMOUT64,	"EXTMOUT64",	&trueval, 0 },
    { I_EXTMINN,	"EXTMINN",	&trueval, 0 },
    { I_EXTMOUTN,	"EXTMOUTN",	&trueval, 0 },
#endif
    /*}}}  */
    /*{{{  inline fp codes */
    { I_FPLDNLDBI,  "FPLDNLDBI",  &has_fpu_core, 0 },
    { I_FPCHKERR,   "FPCHKERR",   &tx_global.hastseriesfperrors, 0 },
    { I_FPSTNLDB,   "FPSTNLDB",   &has_fpu_core, 0 },
    { I_FPLDNLSNI,  "FPLDNLSNI",  &has_fpu_core, 0 },
    { I_FPADD,      "FPADD",      &has_fpu_core, 0 },
    { I_FPSTNLSN,   "FPSTNLSN",   &has_fpu_core, 0 },
    { I_FPSUB,      "FPSUB",      &has_fpu_core, 0 },
    { I_FPLDNLDB,   "FPLDNLDB",   &has_fpu_core, 0 },
    { I_FPMUL,      "FPMUL",      &has_fpu_core, 0 },
    { I_FPDIV,      "FPDIV",      &has_fpu_core, 0 },
    { I_FPLDNLSN,   "FPLDNLSN",   &has_fpu_core, 0 },
    { I_FPREMFIRST, "FPREMFIRST", &tx_global.hasfpremfirst, 0 },
    { I_FPREMSTEP,  "FPREMSTEP",  &tx_global.hasfpremfirst, 0 },
    { I_FPNAN,      "FPNAN",      &has_fpu_core, 0 },
    { I_FPORDERED,  "FPORDERED",  &has_fpu_core, 0 },
    { I_FPNOTFINITE,"FPNOTFINITE",&has_fpu_core, 0 },
    { I_FPGT,       "FPGT",       &has_fpu_core, 0 },
    { I_FPEQ,       "FPEQ",       &has_fpu_core, 0 },
    { I_FPI32TOR32, "FPI32TOR32", &has_fpu_core, 0 },
    { I_FPI32TOR64, "FPI32TOR64", &has_fpu_core, 0 },
    { I_FPB32TOR64, "FPB32TOR64", &has_fpu_core, 0 },
    { I_FPRTOI32,   "FPRTOI32",   &has_fpu_core, 0 },
    { I_FPSTNLI32,  "FPSTNLI32",  &has_fpu_core, 0 },
    { I_FPLDZEROSN, "FPLDZEROSN", &has_fpu_core, 0 },
    { I_FPLDZERODB, "FPLDZERODB", &has_fpu_core, 0 },
    { I_FPINT,      "FPINT",      &has_fpu_core, 0 },
    { I_FPDUP,      "FPDUP",      &has_fpu_core, 0 },
    { I_FPREV,      "FPREV",      &has_fpu_core, 0 },
    { I_FPLDNLADDDB,"FPLDNLADDDB",&has_fpu_core, 0 },
    { I_FPLDNLMULDB,"FPLDNLMULDB",&has_fpu_core, 0 },
    { I_FPLDNLADDSN,"FPLDNLADDSN",&has_fpu_core, 0 },
    { I_FPLDNLMULSN,"FPLDNLMULSN",&has_fpu_core, 0 },
    /*}}}  */
    /*{{{  fptesterr */
    { I_FPTESTERR,  "FPTESTERR",  &has_fptesterr, 0 },
    /*}}}  */

    /*{{{  fp codes, full guy only */
    { I_FPSTTEST, "FPSTTEST", &has_fpu_core, GUYS_ATTR },
    { I_FPLDTEST, "FPLDTEST", &has_fpu_core, GUYS_ATTR },
    { I_FPENTRY3, "FPENTRY3", &tx_global.hasfpentry, GUYS_ATTR },
    { I_FPENTRY2, "FPENTRY2", &tx_global.hasfpentry, GUYS_ATTR },
    { I_FPENTRY , "FPENTRY",  &tx_global.hasfpentry, GUYS_ATTR },
    /*}}}  */
    /*{{{  priority support */
    { I_GETPRI, "GETPRI", &trueval, 0},
    { I_SETPRI, "SETPRI", &trueval, 0},
    /*}}}  */
    /*{{{  debugger support */
    { I_BREAK        , "BREAK",         &has_debug_support, GUYS_ATTR },
    { I_CLRJ0BREAK   , "CLRJ0BREAK",    &has_debug_support, GUYS_ATTR },
    { I_SETJ0BREAK   , "SETJ0BREAK",    &has_debug_support, GUYS_ATTR },
    { I_TESTJ0BREAK  , "TESTJ0BREAK",   &has_debug_support, GUYS_ATTR },
    { I_TIMERDISABLEH, "TIMERDISABLEH", &has_timer_disable, GUYS_ATTR },
    { I_TIMERDISABLEL, "TIMERDISABLEL", &has_timer_disable, GUYS_ATTR },
    { I_TIMERENABLEH , "TIMERENABLEH",  &has_timer_disable, GUYS_ATTR },
    { I_TIMERENABLEL , "TIMERENABLEL",  &has_timer_disable, GUYS_ATTR },
    { I_LDMEMSTARTVAL, "LDMEMSTARTVAL", &has_ldmemstartval, 0 },
    /*}}}  */
    /*{{{  pop */
    { I_POP,     "POP",           &has_pop, 0 },
    /*}}}  */
    /*{{{  loader support */
    { I_LDDEVID, "LDDEVID",       &has_lddevid, 0 },
    /*}}}  */
    /*{{{  Recoded FPU instructions */
    /*{ I_FPSETERR    , "FPSETERR",     &has_directfp, 0 },*/ /* bug TS/1547 12/05/92 */
    { I_FPRN        , "FPRN",         &has_directfp, 0 },
    { I_FPDIVBY2    , "FPDIVBY2",     &has_directfp, 0 },
    { I_FPMULBY2    , "FPMULBY2",     &has_directfp, 0 },
    { I_FPRP        , "FPRP",         &has_directfp, 0 },
    { I_FPRM        , "FPRM",         &has_directfp, 0 },
    { I_FPRZ        , "FPRZ",         &has_directfp, 0 },
    { I_FPR32TOR64  , "FPR32TOR64",   &has_directfp, 0 },
    { I_FPR64TOR32  , "FPR64TOR32",   &has_directfp, 0 },
    { I_FPEXPDEC32  , "FPEXPDEC32",   &has_directfp, 0 },
    { I_FPEXPINC32  , "FPEXPINC32",   &has_directfp, 0 },
    { I_FPABS       , "FPABS",        &has_directfp, 0 },
    /*{ I_FPCLRERR    , "FPCLRERR",     &has_directfp, 0 },*/ /* bug TS/1547 12/05/92 */
    /*{ I_FPNOROUND   , "FPNOROUND",    &has_directfp, 0 },*/ /* replaced with FPADDDBSN 24/6/91 */
    { I_FPADDDBSN   , "FPADDDBSN",    &has_directfp, 0 },     /* new 24/6/91 */
    { I_FPCHKI32    , "FPCHKI32",     &has_directfp, 0 },
    { I_FPCHKI64    , "FPCHKI64",     &has_directfp, 0 },
    /*}}}  */
    /*{{{  New sequential code */
    { I_GTU         , "GTU",          &has_gtu, 0 },
    { I_XBWORD      , "XBWORD",       &has_shortintops, 0 },
    { I_LBX         , "LBX",          &has_shortintops, 0 },
    { I_CB          , "CB",           &has_shortintops, 0 },
    { I_CBU         , "CBU",          &has_shortintops, 0 },
    { I_SSUB        , "SSUB",         &has_shortintops, 0 },
    { I_CIR         , "CIR",          &has_shortintops, 0 },
    { I_SS          , "SS",           &has_shortintops, 0 },
    { I_LS          , "LS",           &has_shortintops, 0 },
    { I_CIRU        , "CIRU",         &has_shortintops, 0 },
    { I_XSWORD      , "XSWORD",       &has_shortintops, 0 },
    { I_LSX         , "LSX",          &has_shortintops, 0 },
    { I_CS          , "CS",           &has_shortintops, 0 },
    { I_CSU         , "CSU",          &has_shortintops, 0 },
    { I_FPREM       , "FPREM",        &has_fprem, 0 },
    { I_FPRANGE     , "FPRANGE",      &has_fprem, 0 },
    { I_FPSQRT      , "FPSQRT",       &has_fpsqrt, 0 },
    { I_FPGE        , "FPGE",         &has_directfp, 0 },
    { I_FPLG        , "FPLG",         &has_directfp, 0 },
    /*}}}  */
    /*{{{  Error handling */
    { I_LDFLAGS     , "LDFLAGS",      &H_series, 0 },
    { I_STFLAGS     , "STFLAGS",      &H_series, 0 },
    { I_LDTH        , "LDTH",         &instr_class.haserrorhandling, 0 },
    { I_SYSCALL     , "SYSCALL",      &H_series, 0 },
    { I_SELTH       , "SELTH",        &instr_class.haserrorhandling, 0 },
    { I_GOPROT      , "GOPROT",       &H_series, 0 },
    { I_TRET        , "TRET",         &instr_class.haserrorhandling, 0 },
    { I_LDSHADOW    , "LDSHADOW",     &instr_class.haserrorhandling, 0 },
    { I_STSHADOW    , "STSHADOW",     &instr_class.haserrorhandling, 0 },
    /*}}}  */
    /*{{{  Process model */
    { I_SETTIMESLICE, "SETTIMESLICE", &instr_class.hasprocessmodel, 0 },
    /*{ I_LDPROC      , "LDPROC",     &H_series, 0 },*/ /* removed bug TS/1539 07/04/92 */
    { I_SWAPQUEUE   , "SWAPQUEUE",    &instr_class.hasprocessmodel, 0 },
    { I_SWAPTIMER   , "SWAPTIMER",    &instr_class.hasprocessmodel, 0 },
    { I_INSERTQUEUE , "INSERTQUEUE",  &instr_class.hasprocessmodel, 0 },
    { I_TIMESLICE   , "TIMESLICE",    &instr_class.hasprocessmodel, 0 },
    /*}}}  */
    /*{{{  Communication */
    { I_INSPHDR     , "INSPHDR",      &H_series, 0 },
    { I_READBFR     , "READBFR",      &H_series, 0 },
    { I_LDCNT       , "LDCNT",        &H_series, 0 },
    { I_LDCHSTATUS  , "LDCHSTATUS",   &H_series, 0 },
    { I_READHDR     , "READHDR",      &H_series, 0 },
    { I_WRITEHDR    , "WRITEHDR",     &H_series, 0 },
    { I_INITVLCB    , "INITVLCB",     &H_series, 0 },
    { I_SETCHMODE   , "SETCHMODE",    &H_series, 0 },
    { I_SETHDR      , "SETHDR",       &H_series, 0 },
    { I_SWAPBFR     , "SWAPBFR",      &H_series, 0 },
    { I_LDVLCB      , "LDVLCB",       &H_series, 0 },
    { I_STVLCB      , "STVLCB",       &H_series, 0 },
    { I_VIN         , "VIN",          &H_series, 0 },
    { I_VOUT        , "VOUT",         &H_series, 0 },
    { I_STOPCH      , "STOPCH",       &H_series, 0 },
    /*}}}  */
    /*{{{  Semaphores and resources */
    { I_SIGNAL      , "SIGNAL",       &instr_class.common_t450_hseries, 0 },
    { I_WAIT        , "WAIT",         &instr_class.common_t450_hseries, 0 },
    { I_GRANT       , "GRANT",        &H_series, 0 },
    { I_ENBG        , "ENBG",         &H_series, 0 },
    { I_DISG        , "DISG",         &H_series, 0 },
    /*}}}  */
    /*{{{  Cache and configuration */
    { I_LDCONF      , "LDCONF",       &H_series, 0 },
    { I_STCONF      , "STCONF",       &H_series, 0 },
    { I_FDCA        , "FDCA",         &H_series, 0 },
    { I_ICA         , "ICA",          &H_series, 0 },
    /*}}}  */
    /*{{{  Added 13/12/90  */
    { I_INTDIS      , "INTDIS",       &instr_class.common_t450_hseries, 0 },
    { I_INTENB      , "INTENB",       &instr_class.common_t450_hseries, 0 },
    { I_CHANTYPE    , "CHANTYPE",     &H_series, 0 },
    { I_FPLDALL     , "FPLDALL",      &H_series, 0 },
    { I_FPSTALL     , "FPSTALL",      &H_series, 0 },
    { I_FDCL        , "FDCL",         &H_series, 0 },
    { I_ICL         , "ICL",          &H_series, 0 },
    { I_LDRESPTR    , "LDRESPTR",     &H_series, 0 },
    { I_STRESPTR    , "STRESPTR",     &H_series, 0 },
    { I_ERDSQ       , "ERDSQ",        &H_series, 0 },
    { I_IRDSQ       , "IRDSQ",        &H_series, 0 },
    { I_MKRC        , "MKRC",         &H_series, 0 },
    { I_UNMKRC      , "UNMKRC",       &H_series, 0 },
    { I_STMOVE2DINIT, "STMOVE2DINIT", &H_series, 0 },
    /*}}}  */
    /*{{{  Added 15/02/91 - swapgstatus */
    /*{ I_SWAPGSTATUS , "SWAPGSTATUS",  &H_series, 0 },*/ /* removed bug TS/1539 07/04/92 */
    /*}}}  */
    /*{{{  Added 08/11/91 - nop */
    { I_NOP,          "NOP",          &instr_class.common_t450_hseries, 0 }, /* "bug" TS/1468 08/11/91 */
    /*}}}  */
    /*{{{  Added 27/11/91 - device access */
    { I_DEVLB,        "DEVLB",        &tx_global.hasdevaccess, 0 }, /* "bug" TS/1498 27/11/91 */
    { I_DEVSB,        "DEVSB",        &tx_global.hasdevaccess, 0 }, /* "bug" TS/1498 27/11/91 */
    { I_DEVLS,        "DEVLS",        &tx_global.hasdevaccess, 0 }, /* "bug" TS/1498 27/11/91 */
    { I_DEVSS,        "DEVSS",        &tx_global.hasdevaccess, 0 }, /* "bug" TS/1498 27/11/91 */
    { I_DEVLW,        "DEVLW",        &tx_global.hasdevaccess, 0 }, /* "bug" TS/1498 27/11/91 */
    { I_DEVSW,        "DEVSW",        &tx_global.hasdevaccess, 0 }, /* "bug" TS/1498 27/11/91 */
    { I_DEVMOVE,      "DEVMOVE",      &tx_global.hasdevaccess, 0 }, /* "bug" TS/1498 27/11/91 */

    /*}}}  */
    /*{{{  Added 22/01/92 - ldprodid and causeerror */
    { I_CAUSEERROR,   "CAUSEERROR",   &instr_class.common_t450_hseries, 0 }, /* "bug" TS/1547 22/01/92 */
    { I_LDPRODID,     "LDPRODID",     &instr_class.common_t450_hseries, 0 }, /* "bug" TS/1536 22/01/92 */

    /*}}}  */
    /*{{{  Added 19/03/92 - restart */
    { I_RESTART,      "RESTART",      &instr_class.common_t450_hseries, 0 }, /* "bug" TS/1547 19/03/92 */
    /*}}}  */
    /*{{{  Added 17/09/92 - testdelay */
    /*{ I_TESTDELAY,    "TESTDELAY",    &H_series, 0 },*/ /* "bug" TS/1874 17/09/92 */
    /*}}}  */
    /*}}}  */
    /*{{{  fpu entry codes */
    { I_FPUSQRTFIRST,"FPUSQRTFIRST",&tx_global.hasfpentry, 0 },
    { I_FPUSQRTSTEP, "FPUSQRTSTEP", &tx_global.hasfpentry, 0 },
    { I_FPUSQRTLAST, "FPUSQRTLAST", &tx_global.hasfpentry, 0 },
    { I_FPURP,       "FPURP",       &tx_global.hasfpentry, 0 },
    { I_FPURM,       "FPURM",       &tx_global.hasfpentry, 0 },
    { I_FPURZ,       "FPURZ",       &tx_global.hasfpentry, 0 },
    { I_FPUR32TOR64, "FPUR32TOR64", &tx_global.hasfpentry, 0 },
    { I_FPUR64TOR32, "FPUR64TOR32", &tx_global.hasfpentry, 0 },
    { I_FPUEXPDEC32, "FPUEXPDEC32", &tx_global.hasfpentry, 0 },
    { I_FPUEXPINC32, "FPUEXPINC32", &tx_global.hasfpentry, 0 },
    { I_FPUABS,      "FPUABS",      &tx_global.hasfpentry, 0 },
    { I_FPUNOROUND,  "FPUNOROUND",  &tx_global.hasfpentry, 0 },
    { I_FPUCHKI32,   "FPUCHKI32",   &tx_global.hasfpentry, 0 },
    { I_FPUCHKI64,   "FPUCHKI64",   &tx_global.hasfpentry, 0 },
    { I_FPUDIVBY2,   "FPUDIVBY2",   &tx_global.hasfpentry, 0 },
    { I_FPUMULBY2,   "FPUMULBY2",   &tx_global.hasfpentry, 0 },
    { I_FPURN,       "FPURN",       &tx_global.hasfpentry, 0 },
    { I_FPUSETERR,   "FPUSETERR",   &tx_global.hasfpentry, 0 },
    { I_FPUCLRERR,   "FPUCLRERR",   &tx_global.hasfpentry, 0 },
    /*}}}  */
    /*{{{  RMC (T450) instructions */
    { I_CLOCKENB,    "CLOCKENB",    &tx_global.hasrmccore, 0 },
    { I_CLOCKDIS,    "CLOCKDIS",    &tx_global.hasrmccore, 0 },
    { I_LDCLOCK,     "LDCLOCK",     &tx_global.hasrmccore, 0 },
    { I_STCLOCK,     "STCLOCK",     &tx_global.hasrmccore, 0 },
    /* { I_SLMUL,       "SLMUL",       &tx_global.hasrmccore, 0 }, */
    /* { I_SULMUL,      "SULMUL",      &tx_global.hasrmccore, 0 }, */
    /* { I_SATADD,      "SATADD",      &tx_global.hasrmccore, 0 }, */
    /* { I_SATSUB,      "SATSUB",      &tx_global.hasrmccore, 0 }, */
    /* { I_SATMUL,      "SATMUL",      &tx_global.hasrmccore, 0 }, */
    { I_GINTDIS,     "GINTDIS",     &tx_global.hasrmccore, 0 },
    { I_GINTENB,     "GINTENB",     &tx_global.hasrmccore, 0 },
    { I_LDTRAPPED,   "LDTRAPPED",   &tx_global.hasrmccore, 0 },
    { I_STTRAPPED,   "STTRAPPED",   &tx_global.hasrmccore, 0 },
    { I_TRAPENB,     "TRAPENB",     &tx_global.hasrmccore, 0 },
    { I_TRAPDIS,     "TRAPDIS",     &tx_global.hasrmccore, 0 },
    /* { I_LDTRAPH,     "LDTRAPH",     &tx_global.hasrmccore, 0 }, */
    /* { I_STTRAPH,     "STTRAPH",     &tx_global.hasrmccore, 0 }, */
    { I_IRET,        "IRET",        &tx_global.hasrmccore, 0 }, 
    /*}}}  */
    /*{{{  pseudo ops */
    { I_AJWRET | I_PSEUDO_OP, "AJWRET",  &trueval, GUYS_ATTR },
    { I_LDRETP | I_PSEUDO_OP, "LDRETP",  &trueval, 0 },
    { I_LD     | I_PSEUDO_OP, "LD",      &trueval, PTRS_ATTR | 1 },
    { I_LDAB   | I_PSEUDO_OP, "LDAB",    &trueval, PTRS_ATTR | 2 },
    { I_LDABC  | I_PSEUDO_OP, "LDABC",   &trueval, PTRS_ATTR | 3 },
    { I_ST     | I_PSEUDO_OP, "ST",      &trueval, ELEM_ATTR | 1 },
    { I_STAB   | I_PSEUDO_OP, "STAB",    &trueval, ELEM_ATTR | 2 },
    { I_STABC  | I_PSEUDO_OP, "STABC",   &trueval, ELEM_ATTR | 3 },
    { I_BYTE   | I_PSEUDO_OP, "BYTE",    &trueval, OPRD_ATTR }, /* unlimited operands */
    { I_WORD   | I_PSEUDO_OP, "WORD",    &trueval, OPRD_ATTR }, /* ditto */
    { I_ALIGN  | I_PSEUDO_OP, "ALIGN",   &trueval, 0 },
    { I_LDLABELDIFF | I_PSEUDO_OP, "LDLABELDIFF", &trueval, 2 },
    { I_RESERVELOWWS | I_PSEUDO_OP, "RESERVELOWWS", &trueval, 1 },
    { I_LDLABELP | I_PSEUDO_OP, "LDLABELP", &trueval, 1 },
    { I_THROWAWAY | I_PSEUDO_OP, "THROWAWAY", &trueval, 0},
    /*}}}  */
    /*{{{  KRoC extras*/
    { I_IOR, "IOR", &trueval, GUYS_ATTR },
    { I_IOW, "IOW", &trueval, GUYS_ATTR },
    { I_IOR8, "IOR8", &trueval, GUYS_ATTR },
    { I_IOW8, "IOW8", &trueval, GUYS_ATTR },
    { I_IOR16, "IOR16", &trueval, GUYS_ATTR },
    { I_IOW16, "IOW16", &trueval, GUYS_ATTR },
    { I_IOR32, "IOR32", &trueval, GUYS_ATTR },
    { I_IOW32, "IOW32", &trueval, GUYS_ATTR },
    { I_WSMAP | I_PSEUDO_OP, "WSMAP",	&trueval, GUYS_ATTR | 2 },
    { I_CODEMAP | I_PSEUDO_OP, "CODEMAP", &trueval, GUYS_ATTR },
    /*}}}*/
    /*{{{  priority support */
    { I_GETAFF, "GETAFF", &trueval, 0 },
    { I_SETAFF, "SETAFF", &trueval, 0 },
    { I_GETPAS, "GETPAS", &trueval, 0 },
    /*}}}  */
    /*{{{  new process API */
    { I_PROC_ALLOC, "PROC.ALLOC", &trueval, GUYS_ATTR },
    { I_PROC_PARAM, "PROC.PARAM", &trueval, GUYS_ATTR },
    { I_PROC_MT_COPY, "PROC.MT.COPY", &trueval, GUYS_ATTR },
    { I_PROC_MT_MOVE, "PROC.MT.MOVE", &trueval, GUYS_ATTR },
    { I_PROC_START, "PROC.START", &trueval, GUYS_ATTR },
    { I_PROC_END, "PROC.END", &trueval, GUYS_ATTR },
    /*}}}*/
    /*{{{  mobile types */
    { I_MT_ALLOC, "MT.ALLOC", &trueval, GUYS_ATTR },
    { I_MT_RELEASE, "MT.RELEASE", &trueval, GUYS_ATTR },
    { I_MT_CLONE, "MT.CLONE", &trueval, GUYS_ATTR },
    { I_MT_IN, "MT.IN", &trueval, GUYS_ATTR },
    { I_MT_XCHG, "MT.XCHG", &trueval, GUYS_ATTR },
    { I_MT_LOCK, "MT.LOCK", &trueval, GUYS_ATTR },
    { I_MT_UNLOCK, "MT.UNLOCK", &trueval, GUYS_ATTR },
    { I_MT_ENROLL, "MT.ENROLL", &trueval, GUYS_ATTR },
    { I_MT_RESIGN, "MT.RESIGN", &trueval, GUYS_ATTR },
    { I_MT_SYNC, "MT.SYNC", &trueval, GUYS_ATTR },
    { I_MT_XIN, "MT.XIN", &trueval, GUYS_ATTR },
    { I_MT_XOUT, "MT.XOUT", &trueval, GUYS_ATTR },
    { I_MT_XXCHG, "MT.XXCHG", &trueval, GUYS_ATTR },
    { I_MT_DCLONE, "MT.DCLONE", &trueval, GUYS_ATTR },
    /*}}}*/
  };
/*}}}  */

/*{{{  PUBLIC INT32 lookupinstruction (char *inst) */
/*{{{  comment */
/*****************************************************************************
 *
 *  lookupinstruction looks up the instruction name 'inst' in the instruction
 *                    table and returns the instruction value if found,
 *                    otherwise an error value :
 *                      INSTRUCTION_NOT_VALID means the instruction exists
 *                        on some processors, but not on the current target.
 *                      INSTRUCTION_NOT_ENABLED means the instruction exists
 *                        but cannot be used in the current guyinserts mode.
 *                      INSTRUCTION_NOT_DECLARED means that the instruction
 *                        does not exist for any target processor.
 *
 *****************************************************************************/
/*}}}  */
PUBLIC INT32 lookupinstruction(
  const char *const inst, const BOOL guy_not_asm,
  const int guyinserts,
  int *err, int *operands,
  int *base_instruction,
  BOOL *primary,
  BOOL *byte_directive,
  BOOL *word_directive,
  BOOL *loading_address,
  BOOL *ptrs_allowed,
  BOOL *element_required,
  BOOL *labels_permitted, /* bug TS/1523 12/12/91 */
  BOOL *const_required)   /* bug TS/1166 14/04/92 */
{
  int i;
  /*{{{  initialise all output parameters */
  *err              = 0;
  *operands         = 0;
  *base_instruction = 0;
  *primary          = FALSE;
  *byte_directive   = FALSE;
  *word_directive   = FALSE;
  *loading_address  = FALSE;
  *ptrs_allowed     = FALSE;
  *element_required = FALSE;
  *labels_permitted = FALSE;
  *const_required   = FALSE;
  /*}}}  */
  for (i = 0; i < (sizeof(instructions) / sizeof(struct instruction_struct));
       i++)
    if (strcmp(instructions[i].iname, inst) == 0)
      /*{{{  we've found the correct instruction */
      {
	/* if the instruction is defined for this target, and legal
	   in our guyinserts mode ... */
	const int instval     = instructions[i].ivalue;
	const int attr        = instructions[i].iattr;
	const int instruction = instval & INST_MASK;
	const BOOL pseudo     = (instval & I_PSEUDO_OP) != 0;

	if ((guyinserts == SEQUENTIALGUYS) && (attr & GUYS_ATTR))
	  *err = 2;
	if (!*(instructions[i].ivalid) ||
	    (guy_not_asm & pseudo))
	  *err = 3;
	if (pseudo && (instruction == I_ALIGN))
	  *err = 4;

	*base_instruction = instruction;
	*primary          = (instval & I_PRIMARY) != 0;
	*byte_directive   = pseudo && (instruction == I_BYTE);
	*word_directive   = pseudo && (instruction == I_WORD);
	*const_required   = *byte_directive | *word_directive |
			    (instruction == I_RESERVELOWWS) | (instruction == I_WSMAP);
	*loading_address  = (instruction == I_LDLP) ||
			    (instruction == I_LDNLP);

	*ptrs_allowed     = (attr & PTRS_ATTR) != 0;
	*element_required = (attr & ELEM_ATTR) != 0;
	*operands         = (attr & OPRD_ATTR);
	if (*operands == OPRD_ATTR) *operands = (-1);

	*labels_permitted = *primary | (pseudo & (instruction == I_LDLABELDIFF)) | (pseudo & (instruction == I_LDLABELP));

	return (instval);
      }
      /*}}}  */
  *err = 1;
  return (0);
}
/*}}}  */
/*{{{  PUBLIC const char *primaryname(int inst) */
/*****************************************************************************
 *
 *  primaryname takes a primary instruction number 'inst' and returns
 *              the instruction name.
 *
 *****************************************************************************/
PUBLIC const char *primaryname ( int inst )
{
  int i;
  for (i = 0; i < N_PRIMARIES; i++)
    if ((instructions[i].ivalue & ~I_PRIMARY) == inst)
      {
	if (strlen(instructions[i].iname) >= (MAX_INSTRUCTION_NAME_LEN - 1))
	  abort();
	strcpy(instname, instructions[i].iname);
	if (asm_uppercase)
	  sup_strupr(instname);
	else
	  sup_strlwr(instname);
	return instname;
      }
  sprintf(instname, "???%d", inst);
  return instname;
}
/*}}}  */
/*{{{  PUBLIC const char *secondaryname (int inst) */
/*****************************************************************************
 *
 *  secondaryname takes a secondary instruction number 'inst' and
 *                returns the instruction name.
 *
 *****************************************************************************/
PUBLIC const char *secondaryname ( int inst )
{
  int i;
  for (i = N_PRIMARIES;
       i < (sizeof(instructions) / sizeof(struct instruction_struct));
       i++)
    if (instructions[i].ivalue == inst)
      {
	if (strlen(instructions[i].iname) >= (MAX_INSTRUCTION_NAME_LEN - 1))
	  abort();
	strcpy(instname, instructions[i].iname);
	if (asm_uppercase)
	  sup_strupr(instname);
	else
	  sup_strlwr(instname);
	return instname;
      }
  sprintf(instname, "opr %d", inst);
  return instname;
}
/*}}}  */
