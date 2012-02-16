/* $Id: version.h,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	compiler version definitions
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



/*{{{  VERSION */

/* C_COMPATIBILITY is no longer used */
/* define C_COMPATIBILITY "occam 2 product compiler (31st May 1988)" */
/*char *C_COMPATIBILITY = "occam 2 product compiler (26th January 1990)";*/

/* The version string is supposed to look like: rr.vv.mm
   where rr is the release number, vv is the version number, mm is the maintenance number
*/

/*{{{  OLD versions */
#if 0 /* old versions */
/*{{{  pre Dx205A */
#define V_VERSION "Version 04.08.17"
#define V_VERSION "Version Alpha V4.8.17"
#define V_VERSION "Version 00.48.55"
#define V_VERSION "Version 02.00.01"
#define V_VERSION "Version 02.00.04" /* beta release Oct 1990*/
#define V_VERSION "Version 2.00.16" /* beta-2 release Dec 1990*/
#define V_VERSION "Version 2.00.23" /* beta-3 release Jan 1991*/
#define V_VERSION "Version 2.00.28" /* last release before the product */
#define V_VERSION "Version 2.01.00" /* product release (?)*/
/*}}}  */
/*{{{  Dx205A           (2.01.01) */
#define V_VERSION "Version 2.01.01" /* product release Feb 1991 */
/*}}}  */
/*{{{  1991             (2.01.04 -> 2.01.35) */
#define V_VERSION "Version 2.01.04" /* copied to Spiros 25/04/91 */
#define V_VERSION "Version 2.01.06" /* copied to Spiros 01/05/91 */
#define V_VERSION "Version 2.01.09" /* put into develop 24/05/91 */
#define V_VERSION "Version 2.01.10" /* put into develop 12/06/91 */
				    /* now created 'misc' source directory */
#define V_VERSION "Version 2.01.11" /* put into develop 17/06/91 */
#define V_VERSION "Version 2.01.12" /* put into develop 01/07/91 */
#define V_VERSION "Version 2.01.13" /* peeled off H1 alpha 11/07/91 */
#define V_VERSION "Version 2.01.14" /* peeled again        12/07/91 */
#define V_VERSION "Version 2.1.14a" /* extra fix for bug 1344 23/07/91 */
#define V_VERSION "Version 2.01.15" /* Added T9000 shorts  22/07/91 */
#define V_VERSION "Version 2.01.16" /* 24/07/91 Added T9000 signed short arith */
#define V_VERSION "Version 2.01.17" /* 25/07/91 T9000 xsword elimination */
#define V_VERSION "Version 2.01.18" /* 26/07/91 more short word stuff */
#define V_VERSION "Version 2.01.19" /* 03/09/91 more short word stuff */
#define V_VERSION "Version 2.01.20" /* 18/09/91 misc bug fixes */
#define V_VERSION "Version 2.01.21" /* 25/09/91 misc bug fixes -> to Spiros */
#define V_VERSION "Version 2.01.22" /* 10/10/91 NDL reader checks added */
#define V_VERSION "Version 2.01.23" /* 15/10/91 NDL reader DEFAULT added */
#define V_VERSION "Version 2.01.24" /* 24/10/91 installed in trial */
#define V_VERSION "Version 2.01.25" /* 29/10/91 Saved before starting Version 3 configurer */
#define V_VERSION "Version 2.01.26" /* 01/11/91 Fixed bug TS/1453 */ /* BROKEN */
#define V_VERSION "Version 2.01.27" /* 05/11/91 Fixed bug TS/1455 */
#define V_VERSION "Version 2.01.28" /* 11/11/91 Added 'nop' opcode */
#define V_VERSION "Version 2.01.29" /* 25/11/91 -> trial. Started V3 configurer */ /* BROKEN */
#define V_VERSION "Version 2.01.30" /* 26/11/91 Fixed - Indentation problems */
#define V_VERSION "Version 2.01.31" /* 26/11/91 -> trial. */ /* BROKEN */
#define V_VERSION "Version 2.01.32" /* 27/11/91 DEV instrs, indent fixed(?) */ /* BROKEN RETYPEs */
#define V_VERSION "Version 2.01.33" /* 02/12/91 -> trial. */ /* H1 alpha2? */
#define V_VERSION "Version 2.01.34" /* 09/12/91 failed to build with opt. C compiler */
#define V_VERSION "Version 2.01.35" /* 12/12/91 -> Spiros */ /* merged trans */
/*}}}  */
/*{{{  1992             (2.01.36 -> 2.01.78) */
#define V_VERSION "Version 2.01.36" /* 03/01/92 -> trial. trans, inst -> misc */
#define V_VERSION "Version 2.01.37" /* 17/01/92 misc bug fixes */
#define V_VERSION "Version 2.01.38" /* 20/01/92 Put through icc3 */
#define V_VERSION "Version 2.01.39" /* 27/01/92 Passes with icc3 ; misc bug fixes */
#define V_VERSION "TEST Vn 2.01.40" /* 28/01/92 SAME as 2.01.39; Sun 3 -> Rank Cintel */
#define V_VERSION "Version 2.01.41" /* 29/01/92 New NDL release, minor bug fixes */
#define V_VERSION "Version 2.01.42" /* 30/01/92 Fixed configurer bug */ /* BROKEN configurer */
#define V_VERSION "TEST Vn 2.01.43" /* 30/01/92 SAME as 2.01.42; Supplied to Marconi */ /* BROKEN configurer */
#define V_VERSION "Version 2.01.44" /* 27/02/92 Fixed configurer bug */
#define V_VERSION "TEST Vn 2.01.45" /* 27/02/92 SAME as 2.01.44; Supplied to Marconi */
#define V_VERSION "Version 2.01.46" /* 02/03/92 -> t9trial. New NDL attributes */
#define V_VERSION "Version 2.01.47" /* 11/03/92 -> trial. */
#define V_VERSION "Version 2.01.48" /* 17/03/92 -> Spiros */
#define V_VERSION "Version 2.01.49" /* 26/03/92 modified txlib to a structure */
#define V_VERSION "Version 2.01.50" /* 01/04/92 CHAN OF ANY stuff */
#define V_VERSION "Version 2.01.51" /* 07/04/92 zops stuff */
#define V_VERSION "Version 2.01.52" /* 23/04/92 misc */
#define V_VERSION "Version 2.01.53" /* 27/04/92 comms simplification */
#define V_VERSION "Version 2.01.54" /* 05/05/92 Fixed a bug in comms */
#define V_VERSION "Version 2.01.55" /* 06/05/92 TP version failed */ /* BROKEN */
#define V_VERSION "Version 2.01.56" /* 08/05/92 -> develop Fixed bug in comms */
#define V_VERSION "Version 2.01.57" /* 18/05/92 -> develop */
#define V_VERSION "Version 2.01.58" /* 18/05/92 -> Spiros */
#define V_VERSION "Version 2.01.59" /* 26/05/92 */
#define V_VERSION "Version 2.01.60" /* 11/06/92 -> trial and develop */
#define V_VERSION "Version 2.01.61" /* 16/06/92 -> trial and develop */
#define V_VERSION "Version 2.01.62" /* 24/06/92 -> develop */
#define V_VERSION "TEST Vn 2.01.63" /* 24/06/92 SAME as 2.01.62; tp -> Paul Henderson */
#define V_VERSION "Version 2.01.64" /* 30/06/92 -> trial and develop */
#define V_VERSION "Version 2.01.65" /* 06/07/92 -> trial and develop REPL->PROC */
#define V_VERSION "Version 2.01.66" /* 07/07/92 -> trial, configurer build */
#define V_VERSION "Version 2.01.67" /* 09/07/92 -> Spiros, develop, started T9000 PORTs */
#define V_VERSION "Version 2.01.68" /* 27/07/92 -> Spiros, develop, trial */
#define V_VERSION "Version 2.01.69" /* 31/07/92 T9ALPHA4 -> trial, t9trial and develop. Fixed timer ALT bug */
#define V_VERSION "Version 2.01.70" /* 10/08/92 -> trial. Automated cgtests, and LDNLP for PLUS */
#define V_VERSION "Version Alpha V 2.01.71" /* 17/08/92 ALPHA 1 -> trial. Automated hwtests */
#define V_VERSION "Version Alpha V 2.01.72" /* 20/08/92 -> develop. First virtual routing! */
#define V_VERSION "Version Alpha V 2.01.73" /* 24/08/92 -> trial. Continued virtual routing */
#define V_VERSION "Version Alpha V 2.01.74" /* 02/09/92 -> trial. Continued virtual routing */
#define V_VERSION "Version Alpha V 2.01.75" /* 07/09/92 -> trial. Continued virtual routing */
#define V_VERSION "Version Alpha V 2.01.76" /* 09/09/92 -> trial. */
#define V_VERSION "Version Alpha V 2.01.77" /* 14/09/92 Advanced toolset config stuff */
#define V_VERSION "Version Beta Vn 2.01.78" /* 14/09/92 -> trial. First Beta, TS/1461 */
/*}}}  */
/*{{{  1992 Dx305A beta (2.01.79 -> 2.01.94) */
#define V_VERSION "Version Beta Vn 2.01.79" /* 12/10/92 -> trial. Dx305 BETA TS/1870 */
#define V_VERSION "Version Beta Vn 2.01.80" /* 02/11/92 -> trial. misc changes */
#define V_VERSION "Version Beta Vn 2.01.81" /* 10/11/92 -> trial. optimising gcc */
#define V_VERSION "Version Beta Vn 2.01.82" /* 24/11/92 -> trial. TS/1968 */
#define V_VERSION "Version Beta Vn 2.01.83" /* 30/11/92 -> Spiros. TS/1975 */
#define V_VERSION "Version Beta Vn 2.01.84" /* 30/11/92 -> trial. TS/1979 */
#define V_VERSION "Version Beta Vn 2.01.85" /* 09/12/92 -> develop TS/1996 */
#define V_VERSION "Version Beta Vn 2.01.86" /* 11/12/92 -> trial, TS/1988, TS/2002 */
#define V_VERSION "Version Beta Vn 2.01.87" /* 15/12/92 -> occonf -> develop, TS/2005, TS/2006 */
#define V_VERSION "Version Beta Vn 2.01.88" /* 18/12/92 -> develop. New descriptors */
#define V_VERSION "Version Beta Vn 2.01.89" /* 21/12/92 -> dev,trial,t9trial */
#define V_VERSION "Version Beta Vn 2.01.90" /* 04/01/93 -> Spiros,dev,trial, TS/2012 */
#define V_VERSION "Version Beta Vn 2.01.91" /* 08/01/93 BROKEN -> Marconi,dev,trial, TS/2023 */
#define V_VERSION "Version Beta Vn 2.01.92" /* 14/01/93 -> trial TS/2035 */
#define V_VERSION "Version Beta Vn 2.01.93" /* 15/01/93 -> Marconi,trial */
#define V_VERSION "Version Beta Vn 2.01.94" /* 26/01/93 -> trial, dev */
/*}}}  */
/*{{{  1993 Dx305A beta (2.02.02 -> 2.02.09) WITH Licence manager */
#define V_VERSION "Version Beta Vn 2.02.02" /* 04/02/93 -> trial,dev ADDED LICENCE MANAGER */
#define V_VERSION "Version Beta Vn 2.02.03" /* 05/02/93 -> trial, Freespace stuff */
#define V_VERSION "Version Beta Vn 2.02.04" /* 11/02/93 -> trial, PC makefile */
#define V_VERSION "Version Beta Vn 2.02.05" /* 12/02/93 -> trial. VAX makefile */
#define V_VERSION "Version 2.02.06" /* 17/02/93 -> trial. Licence manager on */
#define V_VERSION "Version 2.02.07" /* 17/02/93 -> dev*/
#define V_VERSION "Version 2.02.08" /* 18/02/93 -> trial,dev PC Beta? */
#define V_VERSION "Version 2.02.09" /* 24/02/93 -> trial,dev PC beta? VAX prod? */
/*}}}  */
/*{{{  Dx305A           (2.02.10) */
#define V_VERSION "Version 2.02.10" /* 25/02/93 -> Dx305 PRODUCT (PC beta) */
/*}}}  */
/*{{{  1993 post Dx305A (2.02.11 -> 2.02.67) */
#define V_VERSION "Version 2.02.11" /* 17/03/93 -> dev*/
#define V_VERSION "Version 2.02.12" /* 18/03/93 -> t9trial. Alpha: gt bug; fpgt bug; mint bug*/
#define V_VERSION "Version 2.02.13" /* 18/03/93 -> t9trial. Alpha: fpsub bug */
#define V_VERSION "Version 2.02.14" /* 18/03/93 -> t9trial. Alpha: lsx bug */
#define V_VERSION "Version 2.02.15" /* 19/03/93 -> t9trial+trial. Optimised Alpha lsx*/
#define V_VERSION "Version 2.02.16" /* 19/03/93 -> t9trial. Alpha: disc bug*/
#define V_VERSION "Version 2.02.17" /* 22/03/93 -> t9trial+dev Alpha: disc + INT->REAL*/
#define V_VERSION "Version 2.02.18" /* 22/03/93 -> ... ldconf for ALT */
#define V_VERSION "Version 2.02.19" /* 23/03/93 -> t9trial,trial. INSdi01902 */
#define V_VERSION "Version 2.02.20" /* 23/03/93 -> t9trial. Alpha: FPINT predef */
#define V_VERSION "Version 2.02.21" /* 01/04/93 -> Spiros */
#define V_VERSION "Version 2.02.22" /* 06/04/93 -> trial */
#define V_VERSION "Version 2.02.23" /* 13/04/93 -> trial */
#define V_VERSION "Version 2.02.24" /* 13/04/93 -> Spiros*/
#define V_VERSION "Version 2.02.25" /* 20/04/93 -> dev, trial. LAST befor T9000-Y */
#define V_VERSION "Version 2.02.26" /* 20/04/93 -> trial. NEW T9000 Y bit*/
#define V_VERSION "Version 2.02.27" /* 29/04/93 -> dev. First onconf */
#define V_VERSION "Version 2.02.28" /* 05/05/93 -> trial. onconf */
#define V_VERSION "Version 2.02.29" /* 11/05/93 -> trial, dev BROKEN */
#define V_VERSION "Version 2.02.30" /* 12/05/93 -> trial */
#define V_VERSION "Version 2.02.31" /* 17/05/93 -> trial, develop */
#define V_VERSION "Version 2.02.32" /* 25/05/93 -> trial, develop */
#define V_VERSION "Version 2.02.33" /* 28/05/93 -> trial, develop */
#define V_VERSION "Version 2.02.34" /* 08/06/93 -> trial, develop */
#define V_VERSION "Version 2.02.35" /* 14/06/93 -> trial. */
#define V_VERSION "Version 2.02.36" /* 18/06/93 -> develop. onconf fix */
#define V_VERSION "Version 2.02.37" /* 18/06/93 -> trial. */
#define V_VERSION "Version 2.02.38" /* 24/06/93 -> trial. */
#define V_VERSION "Version 2.02.39" /* 05/07/93 -> trial, develop */
#define V_VERSION "Version 2.02.40" /* 05/07/93 -> Spiros*/
#define V_VERSION "Version 2.02.41" /* 06/08/93 -> trial, develop */
#define V_VERSION "Version 2.02.42" /* 12/08/93 -> trial, develop */
#define V_VERSION "Version 2.02.43" /* 24/08/93 -> trial */
#define V_VERSION "Version 2.02.44" /* 27/08/93 -> trial, develop */
#define V_VERSION "Version 2.02.45" /* 17/09/93 -> trial*/
#define V_VERSION "Version 2.02.46" /* 08/10/93 -> trial. Started OCCAM 2.5 */
#define V_VERSION "Version 2.02.47" /* 11/10/93 -> Spiros. More OCCAM 2.5 */
#define V_VERSION "Version 2.02.48" /* 26/10/93 -> Nick. New NDL attributes */
#define V_VERSION "Version 2.02.49" /* 29/10/93 -> trial. */
#define V_VERSION "Version 2.02.50" /* 01/11/93 -> Malc - possible T9 bug fix*/
#define V_VERSION "Version 2.02.51" /* 05/11/93 -> trial. */
#define V_VERSION "Version 2.02.52" /* 05/11/93 -> started changing constructors to litnode */
#define V_VERSION "Version 2.02.53" /* 17/11/93 -> trial. finished changing constructors to litnode */
#define V_VERSION "Version 2.02.54" /* 18/11/93 -> trial. Removed fn result optimisation */
#define V_VERSION "Version 2.02.55" /* 25/11/93 -> trial. Absolute mem location for T9 configurer */
#define V_VERSION "Version 2.02.56" /* 02/12/93 -> trial. T9000 Gamma mods*/
#define V_VERSION "Version 2.02.57" /* 06/12/93 -> trial. Fixed PRAGMA EXTERNAL */
#define V_VERSION "Version 2.02.58" /* 07/12/93 -> trial. T9 Gamma stuff in txlib */
#define V_VERSION "Version 2.02.59" /* 08/12/93 -> harness -> Spiros (T9Gamma in txlib) */
#define V_VERSION "Version 2.02.60" /* 09/12/93 -> trial. */
#define V_VERSION "Version 2.02.61" /* 14/12/93 -> trial. NDL reader, no procs */
#define V_VERSION "Version 2.02.62" /* 15/12/93 -> trial. More minor T9gamma stuff */
#define V_VERSION "Version 2.02.63" /* 16/12/93 -> trial. T9000 gamma configurer */
#define V_VERSION "Version 2.02.64" /* 16/12/93 -> trial. INSdi03371 */
#define V_VERSION "Version 2.02.65" /* 16/12/93 -> trial. Gamma timer workaround */
#define V_VERSION "Version 2.02.66" /* 20/12/93 -> trial. Fixed tests*/
#define V_VERSION "Version 2.02.67" /* 21/12/93 -> saved namestack stuff */
/*}}}  */
/*{{{  1994 post Dx305A (2.02.68 -> 2.02.70) */
#define V_VERSION "Version 2.02.68" /* 11/01/94 -> trial. Optimising namestack handling */
#define V_VERSION "Version 2.02.69" /* 13/01/94 -> trial. Fixed namestack bug*/
#define V_VERSION "Version 2.02.70" /* 19/01/94 -> trial. Copied by Nick and Gaj */
/*}}}  */
/*{{{  1994 post Dx305A (2.03.00 -> 2.03.03) */
#define V_VERSION "Version 2.03.00" /* 21/01/94 -> trial. Fixed VAX/PC makefiles */
#define V_VERSION "Version 2.03.01" /* 26/01/94 -> sources. Started fixing trans */
#define V_VERSION "Version 2.03.02" /* 01/02/94 -> trial. */
#define V_VERSION "Version 2.03.03" /* 04/02/94 -> trial. Nearly occam 2.5 ready!*/
/*}}}  */
/*{{{  1994 occam 2.5   (2.03.04 -> 2.03.44) */
#define V_VERSION "Version 2.03.04" /* 04/02/94 -> OCCAM2.5 Alpha 1 */
#define V_VERSION "Version 2.03.05" /* 08/02/94 -> Alpha1a */
#define V_VERSION "Version 2.03.06" /* 08/02/94 -> trial. C100 stuff */
#define V_VERSION "Version 2.03.07" /* 09/02/94 -> trial & Nick. occam2.5 configurer */
#define V_VERSION "Version 2.03.08" /* 10/02/94 -> saved before merge with Spiros */
#define V_VERSION "Version 2.03.09" /* 11/02/94 -> Spiros */
#define V_VERSION "Version 2.03.10" /* 16/02/94 -> trial, with Nick's NDL changes */
#define V_VERSION "Version 2.03.11" /* 16/02/94 -> trial, Spiros. New fold markers */
#define V_VERSION "Version 2.03.12" /* 22/02/94 -> trial. */
#define V_VERSION "Version 2.03.13" /* 22/02/94 -> Spiros */
#define V_VERSION "Version 2.03.14" /* 23/02/94 -> Before SCCS version stuff added */
#define V_VERSION "Version 2.03.15" /* 01/03/94 -> trial, Spiros, Michael Poole */
#define V_VERSION "Version 2.03.16" /* 04/03/94 -> trial. */
#define V_VERSION "Version 2.03.17" /* 04/03/94 -> trial. */
#define V_VERSION "Version 2.03.18" /* Conor left */
#define V_VERSION "Version 2.03.19" /* 17/03/94 -> Nick NDL checks for C104 partitionee attributes. */
#define V_VERSION "Version 2.03.20" /* 21/03/94 -> Nick Updated ONCONF to allow placement on any ARC, and on ROUTES. */
#define V_VERSION "Version 2.03.21" /* 08/04/94 Support for T450 */
#define V_VERSION "Version 2.03.22" /* Fix bugs in debug info and in
				       "protocolsequivalent" in "chk1.c"
				       OCCAM2.5 fe */
#define V_VERSION "Version 2.03.23" /* Fix bug in routine "rsimpleprotocol"
				       in "syn2.c" OCCAM2.5 fe */
#define V_VERSION "Version 2.03.24" /* 20/04/94 -> Nick Updated ONCONF to
				       allow many channels to host, also
				       fixed EDGE <-> EDGE connection bug */
#define V_VERSION "Version 2.03.25" /* 28/04/94 fix bugs in routine
					 "issame" in file "vti4.c" and in
					 routine "trans_process_counted_io"
					 in file "tran1.c" */
#define V_VERSION "Version 2.03.26" /* 5/05/94 fix a bug in routine
					 "rtypedecl" in file "syn2.c" and
					 work on command line options in
					 "harness.c" */
#define V_VERSION "Version 2.03.27" /* 9/05/94 fix a bug in routine
					 "mapprocess" in file "bind1.c" */
#define V_VERSION "Version 2.03.28" /* 2/06/94 fix a bug in routine
					 "do_freevarsin" in file "use1.c"
					 and a bug in routine "transspec"
					 in file "tran1.c" */
#define V_VERSION "Version 2.03.29" /* 06/06/94 Nick Inserted implementation
					 of PDATA in NDL configurer */
#define V_VERSION "Version 2.03.30" /* 10/06/94 fix a bug in parser
					 by introducing the routine
					 "rtypespecorelement" in syn3.c */
#define V_VERSION "Version 2.03.31" /* 14/06/94 fix a bug in overlap check
					 for OCCAM2.5 in use3.c */
#define V_VERSION "Version 2.03.32" /* 29/09/94 fix a bug for generating
					 the correct type info for an array
					 temp used to hold the value of
					 a constructor. The following files
					 have been  changed chk1.c, occamfe.c,
					 chkdef.h */
#define V_VERSION "Version 2.03.33" /* 29/09/94 fix the bug INSdi03440.
					 The following file has been changed chk2.c */
#define V_VERSION "Version 2.03.34" /* 12/10/94 Modified messages for placement
					 below memstart, changed bind.c generror.c
					 and generror.h, also disabled workarounds
					 for bug INSdi03437, which converted T9000
					 to T9000GAMMA for gamma toolset, now fixed
					 in HDB, files cfwrmdb1.c, objlib.c */
#define V_VERSION "Version 2.03.35" /* 12/10/94 added NDL Inquest support;
					 T-series debugger now Inquest (idebug
					 support a Z option); Y option changed
					 to reflect i/o mode not interactive
					 debugging; relax restrictions when
					 booting from ROM and Inquest; removed
					 NDL NV option.
					 Files: cfbe1.c, cferr.h, cferr.c, cfpub.h,
					 cfsup1.c, cfwrmdb1.c, cfwrsdb1.c, harness.c. */
#define V_VERSION "Version 2.03.36" /* 18/10/94 Modified error handling for
					 untyped nodes in a connect statement. */
#define V_VERSION "Version 2.03.37" /* 21/10/94 Change the opcodes of
					 the t450 instructions ldtraph and 
					 sttraph */
#define V_VERSION "Version 2.03.38" /* 10/11/94 Fixed bug regarding 
				       segmentation fault following 
				       "No direction known for channel"
				       warning. */
#define V_VERSION "Version 2.03.39" /* 21/11/94 Fix the bug INSdi03578.
					 The file syn2.c was modified.
					 The routines "rrestofoperand" and
					 "rtypeoroperand" were modified a
					  new routine "rspecifier_constr" was
					 written. */
#define V_VERSION "Version 2.03.40" /* 25/11/94 Fix the bug INSdi04125.
					 The file err2.c was modified. */
#define V_VERSION "Version 2.03.41" /* 5/12/94 Fix the bug INSdi03569.
					 The files chk1.c,  err2.c and
					 chkerror.h were  modified. */
#define V_VERSION "Version 2.03.42" /* 6/12/94 Fix the bug INSdi03394 which
				       is related with the CHAN OF ANY.
				       The file chk2.c was modified. */
#define V_VERSION "Version 2.03.43" /* 9/12/94 fix the Bug INSdi04134. 
				       The following files were modified
				       tran1.c, tran3.c, bind1.c, bind2.c, 
				       gen1.c, gen4.c, gen7.c, gen8.c.*/
#define V_VERSION "Version 2.03.44" /* 25/11/94 Fix the bug INSdi01310 
						  and INSdi01311. */
/*}}}  */
/*{{{  1995 occam 2.5   (2.03.45 -> ...    ) */
#define V_VERSION "Version 2.03.45" /* 5/1/95 implement the string 
			       decoration */
#define V_VERSION "2.03.46" /* 26/1/95 fchange to use arglib, 
				 {up,down}grade name to occam 2.1 */
#define V_VERSION "2.03.47" /* 16/2/95   Changed to incorporate ST20 attributes
					 for ndlrdr and occonf */
#define V_VERSION "2.03.48" /* 24/2/95   Modify the file syn2.c to fix a
					 bug in the routine "rspecification"
					 Modify the files bind1.c and objwrt.c
					 to improve profiling info
					 and provide the proper profiling info
					 header */
#define V_VERSION "2.03.49" /* 3/3/95   Fix a bug in harness relating with
					processing of #OPTION N. Remove the
					default processor. Fix the oc -z help
					page */
#define V_VERSION "2.03.50" /* 24/3/95 Implement  the div, rem
				       workaround for t450_a silicon. */
#define V_VERSION "2.03.51" /* 30/3/95   Implement  workaround for alu, 
					 multicycle alu op pairs
					 in the t450_a silicon. */
#define V_VERSION "2.03.52" /* 20/7/95  Fix a bug in objrdr.c related with
					#PRAGMA EXTERNAL directive.       */
#define V_VERSION "2.03.53" /* 7/7/95  Enabled GD option for occonf */
#define V_VERSION "2.03.54" /* 2/8/95 Work on producing a banner for 
					compiler interal errors
					instead of core dump. */
#define V_VERSION "2.03.55" /* 10/8/95 change the occonf : configurer
				       into occonf : occam 2.1 configurer */
#define V_VERSION "2.03.56" /* 6/9/95 remove the licence manager code */
#define V_VERSION "2.03.57" /* 26/9/95 fix bugs with ST20 memstart and
			       numlinks attribute handling in occonf */
#define V_VERSION "2.03.58" /* 5/10/95 fix bug with cfbe1.c when a node
				 of no type is encountered */

/*}}}  */
#endif /* old versions */

#if 0 /* old joc versions */
#define V_VERSION "Version 3.00.04" /* joc passes all the cgtests and
					 o25tests -V option is tested OK */
#define V_VERSION "Version 3.00.05" /* fix a bug in opt. related with
					 the flowgraph in PARs fix the
					 enhancement Bug INSdi03743    */
#define V_VERSION "Version 3.00.06" /* 2/06/94 fix a bug in routine
					 "do_freevarsin" in file "use1.c"
					 and a bug in routine "transspec"
					 in file "tran1.c". Work on
					 command line option in "harness.c" */
#endif /* old versions */
/*}}}  */

#if defined COMPILING_TO_JCODE
  #define V_VERSION "Version 3.00.07" /* 3/3/95 Merge with the latest work
					 in the common be, ocfe, ocmisc,
					 implement debugging info. */ 
#elif defined (NDLRDR)
  #define V_VERSION "2.03.59"
#elif defined (CONFIG2)
  #define V_VERSION "2.03.59"
#elif defined (CONFIG3)
  #define V_VERSION "2.03.59"
#elif defined(CODE_GEN_KROC_ASM)
  #define V_VERSION "OFA " VERSION "K"
#else
  #define V_VERSION "2.03.59" /* 27/10/95 fix a bug in constant folding
				 of constructors */
#endif
/*}}}  */

/*{{{  arglib stuff */
#define USING_ARG_VERSION  1

#ifdef NDLRDR
  #define TOOL         "ndlrdr"
  #define TOOL_LONG    "NDL reader"
  #define TOOL_TOOLSET "NDL reader"
  #define TOOL_OPTENV  "NDLRDRARG"
#else
#if defined (CONFIG2)
#ifdef OCCAM2_5
  #define TOOL_LONG    "occam 2.1 configurer"
#else
  #define TOOL_LONG    "occam 2 configurer"
#endif
  #define TOOL         "occonf"
  #define TOOL_TOOLSET TOOL_LONG
  #define TOOL_OPTENV  "OCCONFARG"
#endif
#if defined (CONFIG3)
#ifdef OCCAM2_5
  #define TOOL_LONG    "occam 2.1 NDL configurer"
#else
  #define TOOL_LONG    "occam 2 NDL configurer"
#endif
  #define TOOL         "onconf"
  #define TOOL_TOOLSET TOOL_LONG 
  #define TOOL_OPTENV  "ONCONFARG"
#endif

#ifdef OC
#ifdef OCCAM2_5
  #if defined COMPILING_TO_JCODE
    #define TOOL_LONG    "occam 2.1 optimising compiler"
    #define TOOL_TOOLSET "occam 2.1 optimising compiler"
  #else
    #define TOOL_LONG    "occam 2.1 compiler"
    #define TOOL_TOOLSET TOOL_LONG  
  #endif
#else
  #if defined COMPILING_TO_JCODE
    #define TOOL_LONG   "occam 2 optimising compiler"
    #define TOOL_TOOLSET "occam 2 optimising compiler"
  #else
    #define TOOL_LONG    "occam 2 compiler"
    #define TOOL_TOOLSET "occam 2 compiler"
  #endif
#endif
  #define TOOL         "occ21"
  #define TOOL_OPTENV  "OCARG"
#endif
#endif

#define TOOL_VERSION     V_VERSION
#define TOOL_DATE         __DATE__
#define TOOL_COPYRIGHT   "1995,1996,1997"
#define TOOL_USAGE       TOOL " filename { -option }"
/*}}}  */

