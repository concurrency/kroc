/* $Id: txlib.h,v 1.3 1998/10/05 11:44:44 dcw Exp $ */

/*
 *	transputer specific flags for compiler
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

/* Version 1.5 */
/* further mods MDP 28/11/95 for 64-bit machines */

/*{{{  history */
/* V1.5 - 03/03/94 - Ade - Added pstring field to txlib_t */
/* V1.4 - 03/03/94 - CON - Added 'hasdevaccess' field */
/* V1.3 - 08/02/94 - CON - Updated list of T9 gamma bugs */
/* V1.2 - 15/12/93 - CON - No CRC for T9000 Gamma */
/* V1.1 - 07/12/93 - CON - Added T9000 Alpha/Gamma workarounds */
/* V1.0 -          - CON - 1st version */
/*}}}  */

#ifndef TXLIB_H
#define TXLIB_H

/*{{{  typedef txlib_t */
typedef struct
  {
    /*{{{  Basic target processor flags */
    /* These hold the basic TCOFF bit-patterns for the target processor */
    BIT32 ptype;
    BIT32 pattr;

    BOOL  tseries;     /* a T-series processor (T800  etc.; also RMC) */
    BOOL  hseries;     /* a H-series processor (T9000 etc) */
    BOOL  lprocess;    /* a H-series L-process */

    int   bpw;         /* Number of bytes in target processor's word */
    /*}}}  */
    /*{{{  Instruction timings */
    BOOL t9000_timings; /* using a T9000 pipeline */
    /*}}}  */
    /*{{{  Target instruction set flags */
    BOOL  hasfpucore;       /* fpu is present */
    BOOL  hasfpsupport;     /* TRUE for T414 type processors */
    BOOL  hasfmul;          /* has fmul instruction */
    BOOL  hasdup;           /* has dup instruction */
    BOOL  haswsubdb;        /* has wsubdb instruction */
    BOOL  hasmove2d;        /* has 2d block move instructions */
    BOOL  hascrc;           /* has crc instructions */
    BOOL  hasbitops;        /* has bitcount instructions */
    BOOL  hasfptesterr;     /* has fptesterr instruction */
    BOOL  haslddevid;       /* has lddevid instruction */
    BOOL  hasdebugsupport;  /* has j0 style break instructions */
    BOOL  hastimerdisable;  /* has timer disable instructions */
    BOOL  hasldmemstartval; /* has ldmemstartval instruction */
    BOOL  haspop;           /* has pop instruction */
    BOOL  hasdirectfp;      /* should not use fpentry */
    BOOL  hasfprem;         /* has fprem instruction */
    BOOL  hasfpsqrt;        /* has fpsqrt instruction */
    BOOL  hasshortintops;   /* has 'ls', 'lsx', etc */
    BOOL  hasvariableio;    /* has vin and vout instructions */
    BOOL  hasgtu;           /* has gtu instruction */
    /* extras for 64bit targets, etc MDP 28/11/95*/
    BOOL  hasi64tor;
    BOOL  i64opsbyfunc;
    BOOL  needsquadalign;
    BOOL  hassqrt;
    BOOL  hasfpint;
    BOOL  hasordered;
    BOOL  swapr64words;
    /* DCW 19/9/98 */
    BOOL  kroc_flag;        /* omit transputer-dependent optimizations */
    /* FRMB 20/12/2004 */
    BOOL  kroc_complex_div;	/* generated DIV instruction is complex */
    BOOL  kroc_complex_rem;	/* generated REM instruction is complex */
    /* FRMB 31/03/2005 */
    BOOL  kroc_chantype_desc;	/* whether to include channel-type description and operation-pointer in mobile channel-types */
    BOOL  kroc_chantype_uio;	/* whether to do user-defined operations on mobile channel-types */
    BOOL  kroc_chantype_knsf;	/* whether to allocate a state-field and semaphore above the client/server semaphore in mobile channel-types */
    /* FRMB 20/11/2005 */
    BOOL  hassincostan;		/* whether hardware has SIN/COS/TAN */
    /*}}}  */
    /*{{{  Configuration details */
    BOOL  specifiable_memstart; /* Added for ST20, allows memstart to be specified to configurer */
    BOOL  specifiable_links;    /* Added for ST20, allows number of links to be specified to configurer */

    int   links;   /* Number of links on target processor */
    BOOL  isclass; /* Target is a processor class */
    int   cfbtype; /* Type for configuration binary file */

    INT32 mint;     /* value of MOSTNEG INT on target processor */
                                      /* (0x8000 or 0x80000000) */
    int   memstart; /* Number of words below MemStart */
    int   ram;      /* Number of bytes of on-chip RAM */

    /*}}}  */
    /*{{{  C compiler specifics */
    int ptype_symbol; /* initialiser for _PTYPE C preprocessor symbol */
    /*}}}  */
    /*{{{  extra for T9000 */
    /* at the moment I am adding these on at the end, so as not to break
       any existing stuff which has already been compiled.
       If this is of no concern to you, move these to more appropriate
       places in this structure.
       CON - 12/05/92
    */

    /* Some processors may have instructions that satisfy both conditions
       of a BOOL here, eg. RMC has sethalterr as well as trap handling.
       In these cases, we set the flag according to the run-time model
       that we actually use; ie. even though the RMC has trap handling, we
       don't automatically set this up in our run-time model, we still use
       sethalterr, so for RMC hastserieserrors will be TRUE.
       SHC - 08/03/94.
     */

    BOOL  hasfpentry;          /* has 'fpu...' instructions */
    BOOL  hasfpremfirst;       /* has fpremfirst and fpremstep */

    BOOL  hastserieserrors;    /* run-time model uses sethalterr etc */
    BOOL  hastseriesscheduler; /* run-time model uses saveh etc., not
                                  swapqueue, intenb/intdis etc. */
    BOOL  hastseriesfperrors;  /* run-time model uses fpchkerr etc */

    BOOL  hast9gammaeprobs;    /* has T9000 gamma E problems */
    BOOL  hast9gammaprobs;     /* has T9000 gamma problems */
    BOOL  hast9alphaprobs;     /* has T9000 alpha problems */

    BOOL  hasdevaccess;        /* has device access instrs for volatile/PORT */
    /*}}}  */
    /*{{{  extra for RMC */
    BOOL  rmc_timings;         /* Using an RMC style CPU */
    BOOL  hasrmccore;          /* Has rmc core instructions */
    /*}}}  */
    const char *pstring;        /* Processor string */
  } txlib_t;
/*}}}  */

/*{{{  definitions */
#define UNKNOWN_PROCESSOR_TYPE 0
/* dissimilar to all TCOFF processortype bit-patterns */
/* Really needs to be zero so that global vars are automatically initialised
   to this value */
/*}}}  */

/*{{{  Function definitions */
/*{{{  tx_setprocessor */
BIT32 tx_setprocessor (txlib_t *tx, const char *string);
/* This routine should be called with a target processor type specified
   as an upper case string (eg "T414").
   It checks whether this string is valid; if not, it returns
     UNKNOWN_PROCESSOR_TYPE
   If it is valid, it returns the new TCOFF 'processortype' value.
   It also sets up all the flags to their correct values
   (including the processortype value).
   All BOOL flags can then be assumed to have the value TRUE or FALSE.
*/
/*}}}  */
/*{{{  tx_testprocessor */
BOOL tx_testprocessor(const char *string);
/* This routine takes the same parameter as setprocessor(), but merely
   confirms whether that processor type is valid or not.
   This routine has no side-effects;
   it DOES NOT affect any of the flag settings.
*/
/*}}}  */
/*{{{  tx_processorstring */
const char *tx_processorstring(BIT32 type, BIT32 attr);
/* This takes a processortype and processorattr pair, and determines
   the name of that processor type or class. It returns a pointer to
   a string holding an upper case version of the name.
   If the processor type isn't recognised, it returns NULL.
   This routine has no side-effects.
*/
/*}}}  */
/*{{{  tx_processorstring_not_class */
const char *tx_processorstring_not_class(BIT32 type, BIT32 attr);
/* This takes a processortype and processorattr pair, and determines
   the name of that processor type (NOT class). It returns a pointer to
   a string holding an upper case version of the name.
   If the processor type isn't recognised, it returns NULL.
   This routine has no side-effects.
*/
/*}}}  */
/*{{{  tx_processorstring_from_devid */
const char *tx_processorstring_from_devid(const int deviceid);
/* This takes a processor lddevid value, and determines
   the name of that processor type or class. It returns a pointer to
   a string holding an upper case version of the name.
   If the processor type isn't recognised, it returns NULL.
   This routine has no side-effects.
*/
/*}}}  */
/*{{{  tx_processorstring_from_txlib */
const char *tx_processorstring_from_txlib(txlib_t *tx);
/* This takes a txlib structure and determines
   the name of that processor type or class. The tx argument must be
   that returned from a previous call to tx_setprocessor(), if it is
   NULL then a NULL result is returned.
   This routine has no side-effects.
   It is used to distinguish between processor types that may have the
   same processortype and processorattr values, such as ST20 and T450.
*/
/*}}}  */
/*}}}  */

/*{{{  side-effects pragmas */
#ifdef _ICC
#pragma IMS_nosideeffects (tx_testprocessor)
#pragma IMS_nosideeffects (tx_processorstring)
#pragma IMS_nosideeffects (tx_processorstring_not_class)
#pragma IMS_nosideeffects (tx_processorstring_from_devid)
#pragma IMS_nosideeffects (tx_processorstring_from_txlib)
#endif
/*}}}  */

/*{{{  Backwards compatibility */
/*{{{  global structure */
extern txlib_t tx_global;
/*}}}  */
/*{{{  access macros to global structure */
/*{{{  Basic target processor flags */
#define processortype          tx_global.ptype
#define processorattr          tx_global.pattr
#define processorpstring       tx_global.pstring

#define T_series               tx_global.tseries
#define H_series               tx_global.hseries
#define L_process              tx_global.lprocess

#define bytesperword           tx_global.bpw
/*}}}  */
/*{{{  Instruction timings */
#define T9000_instruction_timings tx_global.t9000_timings
/*}}}  */
/*{{{  Target instruction set flags */
#define has_fpu_core           tx_global.hasfpucore
#define has_fp_support         tx_global.hasfpsupport
#define has_fmul               tx_global.hasfmul
#define has_dup                tx_global.hasdup
#define has_wsubdb             tx_global.haswsubdb
#define has_move2d             tx_global.hasmove2d
#define has_crc                tx_global.hascrc
#define has_bitops             tx_global.hasbitops
#define has_fptesterr          tx_global.hasfptesterr
#define has_lddevid            tx_global.haslddevid
#define has_debug_support      tx_global.hasdebugsupport
#define has_timer_disable      tx_global.hastimerdisable
#define has_ldmemstartval      tx_global.hasldmemstartval
#define has_pop                tx_global.haspop
#define has_directfp           tx_global.hasdirectfp
#define has_fprem              tx_global.hasfprem
#define has_fpsqrt             tx_global.hasfpsqrt
#define has_shortintops        tx_global.hasshortintops
#define has_variableio         tx_global.hasvariableio
#define has_gtu                tx_global.hasgtu
#define has_rmc_core           tx_global.hasrmccore
/* extras added MDP 28/11/95 */
#define has_i64tor             tx_global.hasi64tor
#define i64ops_byfunc          tx_global.i64opsbyfunc
#define needs_quadalign        tx_global.needsquadalign
#define has_sqrt               tx_global.hassqrt
#define has_fpint              tx_global.hasfpint
#define has_ordered            tx_global.hasordered
#define swap_r64words          tx_global.swapr64words
/* DCW 29/9/98 */
#define kroc_flag              tx_global.kroc_flag
/* FRMB 20/12/2004 */
#define kroc_complex_div	tx_global.kroc_complex_div
#define kroc_complex_rem	tx_global.kroc_complex_rem
/* FRMB 31/03/2005 */
#define kroc_chantype_desc	tx_global.kroc_chantype_desc
#define kroc_chantype_uio	tx_global.kroc_chantype_uio
#define kroc_chantype_knsf	tx_global.kroc_chantype_knsf
/* FRMB 30/11/2005 */
#define has_sincostan		tx_global.hassincostan
/*}}}  */
/*{{{  Configuration details */
#define target_no_of_links     tx_global.links
#define target_is_class        tx_global.isclass
#define target_config_type     tx_global.cfbtype

#define target_mint            tx_global.mint
#define target_MemStart_words  tx_global.memstart
#define target_RAM             tx_global.ram

/*}}}  */
/*{{{  C compiler specifics */
#define predefined_PTYPE_value tx_global.ptype_symbol
/*}}}  */
/*}}}  */

#define setprocessor(s)               tx_setprocessor(&tx_global, s)
#define test_setprocessor(s)          tx_testprocessor(s)
#define processorstring(p,a)          tx_processorstring(p,a)
#define processorstring_from_devid(d) tx_processorstring_from_devid(d)
/*}}}  */

/*{{{  T9000 Alpha and Gamma workarounds */
#define T9000_alpha_nofpconv(S)     ((S)->hast9alphaprobs | (S)->hast9gammaprobs | (S)->hast9gammaeprobs) /* (bug 172) (Alpha FPU) No FP conversions */
#define T9000_alpha_nofprem(S)      ((S)->hast9alphaprobs | (S)->hast9gammaprobs | (S)->hast9gammaeprobs) /* (bug 173) (Alpha FPU) No FP remainder */
#define T9000_alpha_nofpconst(S)    ((S)->hast9alphaprobs) /* No fpldzero */
#define T9000_alpha_nodevaccess(S)  ((S)->hast9alphaprobs) /* (Alpha CPU) No Device access */
#define T9000_alpha_nocauseerror(S) ((S)->hast9alphaprobs) /* (Alpha CPU) No Causeerror -> use seterr */
#define T9000_alpha_nocrc(S)        ((S)->hast9alphaprobs | (S)->hast9gammaprobs | (S)->hast9gammaeprobs ) /* (bug 168) No CRC instructions */
#define T9000_alpha_lprocess(S)     ((S)->hast9alphaprobs) /* (Alpha CPU) Needs L-process bit */

#define T9000_alpha_badsttimer(S)   ((S)->hast9alphaprobs) /* (Bug 001) sttimer is dodgy */
#define T9000_alpha_nolsx(S)        ((S)->hast9alphaprobs) /* (Bug 020) lsx, lbx don't sign extend */
#define T9000_alpha_noxsword(S)     ((S)->hast9alphaprobs) /* (Bug 020) xsword, xbword don't work */
#define T9000_alpha_badalt(S)       ((S)->hast9alphaprobs) /* (Bug 060) ALT is broken */
#define T9000_alpha_badmint(S)      ((S)->hast9alphaprobs) /* (Bug 067) mint groups wrongly */
#define T9000_alpha_badfpsub(S)     ((S)->hast9alphaprobs) /* (Bug 068) fpsub x - 0 is broken */
#define T9000_alpha_badgt(S)        ((S)->hast9alphaprobs) /* (Bug 075) gt; cj is broken */
#define T9000_alpha_badfpbool(S)    ((S)->hast9alphaprobs | (S)->hast9gammaprobs | (S)->hast9gammaeprobs) /* (Bug 103) fptest; cj is broken */
#define T9000_alpha_badlmul(S)      ((S)->hast9alphaprobs) /* (Bug 073) lmul/etc is broken; needs a nop following */
#define T9000_alpha_badlmulpre(S)   ((S)->hast9alphaprobs) /* (Bug 080) lmul/norm is broken; needs a nop preceeding */
#define T9000_alpha_badfmul(S)      ((S)->hast9alphaprobs) /* (Bug 088) fmul is broken; needs a nop following */
#define T9000_alpha_badrunp(S)      ((S)->hast9alphaprobs | (S)->hast9gammaprobs) /* (bug 158/162) runp conflicts with scheduler */

#define T9000_gamma_nobitcnt(S)     ((S)->hast9gammaprobs) /* (Bug 133) bitcnt is broken */
#define T9000_gamma_carryin(S)      ((S)->hast9gammaprobs) /* (Bug 134) ALU has carry problems */
#define T9000_gamma_badmul(S)       ((S)->hast9gammaprobs) /* (Bug 139) Multiply needs nop in front */
#define T9000_gamma_badeqc0(S)      ((S)->hast9gammaprobs | (S)->hast9gammaeprobs) /* (Bug 157) Can't have two eqc 0 instrs */
#define T9000_gamma_baddevaccess(S) ((S)->hast9gammaprobs) /* (Bug 181) Needs nop before device access */
#define T9000_gamma_badldnlp(S)     ((S)->hast9gammaprobs) /* (Bug 183) Needs nop between ldnlp and stnl */

/*}}}  */

#endif
