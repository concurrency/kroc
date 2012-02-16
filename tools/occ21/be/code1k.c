/*
 *	See below for description
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

/*
 *
 *  code1   - instruction generation(, code adjustment, code output)
 *  code1k  - throws away everything not used by kroc
 *            derived from Mar 94 version of code1.c not fully updated to
 *            match Nov 1995 version
 *  now (9/12/96) includes code developed in .etc version:
 *  code1e  - outputs code in ETC binary (+ optional text) rather than text
 */

/*#define DEBUG*/

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#include <string.h>
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif
#include "includes.h"
#include "genhdr.h"
#include "instruct.h"
#include "instdef.h"
#include "generror.h"
#include "lexdef.h"		/* for some settings */

#include "objwrt.h"
#include "gen1def.h"
#include "gen2def.h"
#include "gen4def.h"
#include "gen11def.h"
#include "code1def.h"
#include "bind2def.h"
#include "bind3def.h"
#include "debugdef.h"
#include "mobiles.h"

/*}}}*/

#ifdef BACKENDTRACE
	char betrace_buf[512];
#endif

/*{{{  definitions*/
/* 22/01/2001 frmb -- increased limite (more output then before) */
/* The actual code buffer size is CODE_SIZE * sizeof(INT32) */
#define CONFIGURING_CODE_SIZE 20000	/* approx  80K bytes */
#ifdef BACKENDTRACE
#define COMPILING_CODE_SIZE   2097152	/* approx 8192K (8M) bytes */
#else
#define COMPILING_CODE_SIZE   524288	/* approx 2048K (2M) bytes */
#endif


PUBLIC int req_code_size = 0;	/* code size requested (in K) */
/* PRIVATE int max_code_size;     buffer size (in INT32s)    */

#define MAX_BYTES_IN_INSTRUCTION 8	/* 32 bit target wordsize maximum */
#define MAX_CONSTANT_SIZE        8	/* 8 bytes for a REAL64 etc */

/*{{{  segment definitions*/
typedef struct seginfo_struct {
	struct seginfo_struct *seg_next;
	treenode *seg_name;
} seginfo_t;
/*}}}*/

/*}}}*/
/*{{{  global variables*/
PRIVATE volatile int dead;
PRIVATE treenode *libentrychain;
PRIVATE INT32 const_top;
PRIVATE BOOL showtstack;
PRIVATE BOOL assembly_optimise;
PRIVATE SOURCEPOSN oldlocn;
PRIVATE char *procname;
PRIVATE FILE *etcfile;
PRIVATE BOOL in_comment = FALSE;

/*{{{  STOP on error handling*/
typedef enum {
	ERROR_STATUS_START,	/* At the start of a block    */
	ERROR_STATUS_CLEAN,	/* Error is known to be clear */
	ERROR_STATUS_DIRTY	/* Error may have been set    */
} error_status_t;

PRIVATE error_status_t int_error_status;
PRIVATE error_status_t fpu_error_status;

#define LOCALISE_ERROR_HANDLING TRUE
/* TRUE if we want to wait until generating an FPU instruction before clearing
   the FPU error flag
   BUT we need a way to clear FPU error without clobbering integer stack.
   There isn't one, cos both FPTESTERR and FPUCLRERR hit 1 stack slot.
   AHAH! We now use the fact that FPU loads always use a CPU slot,
   so we can use that after the load has happened!  CO'N 12/2/90
   We can't use this system for normal errors, cos the 'TESTERR' instruction
   clobbers the stack
*/

#if LOCALISE_ERROR_HANDLING
#define ERROR_STATUS_FP_INIT ERROR_STATUS_START
#else
#define ERROR_STATUS_FP_INIT ERROR_STATUS_CLEAN
#endif
/*}}}*/

/*{{{  data for segment handling*/
PRIVATE seginfo_t *seginfo;
/*}}}*/
/*}}}*/

/*{{{  etc output routines*/
/*{{{  definition of ETC specials*/
/*
-- These are introduced by OPR instructions with negative prefixes
-- 6D F0 flags a following J, CJ or CALL to have numeric label operand
--          or 2 LDCs with operands L1, L2 to signify LDC L1-L2
-- 6D F1 flags a following LDLP to introduce a pair of LOOPEND labels
--          [end, top] encoded in LDC instructions. LDLP defines control block.
-- 6D F2 flags a following LDLP to introduce a pair of LOOPEND3 labels
--          [end, top] encoded in LDC instructions.  LDLP defines control block.
-- 6D F3 flags a following LDLP to introduce a pair of reverse-LOOPEND labels
--          [end, top] encoded in LDC instructions. LDLP defines control block.
-- 6D F4 flags a following LDC to set the mobile-space requirements (words)
-- 6D F5 flags a following set of LDCs to introduce the mobile-space map
--       <msp-offset> <count>, then for each `count':
--           <slot-offset> <data-offset>
--         * a negative slot-offset indicates MINT initialisation at <data-offset>
--         * a data-offset of 0 indicates dynamic mobile array initialisation at offset <slot-offset>
--         * anything else is normal mobilespace initialisation
-- 6D F7 flags a following pair of LDCs to load the workspace-map of a process.
--	 first LDC is the MPP offset (or NO_SLOT == MINT for none), second is a
--	 label where the map is found
-- 6D F8 flags a following pair of LDCs to unload the workspace-map of a process.
--	 first LDC is the MPP offset (or NO_SLOT == MINT for none), second is a
--	 label where the map is found
--
--
-- 6E F1 flags a following LDC to be a name length followed by that number
--       of bytes as a STUBNAME for calling a #USE'd PROC
-- 6E F2 flags a following LDC to be a name length followed by that number
--       of bytes as a GLOBNAME for making a PROC name exportable
-- 6E F3 flags a following LDC to be a name length followed by that number
--       of bytes as a JUMPENTRY name for a jump to that entrypoint
-- 6E F4 flags a following LDC to be a name length followed by that number
--       of bytes as the name of a PROC/FUNCTION being entered
-- 6E F5 flags a following LDC to be a name length followed by that number
--       of bytes as the name of a source file
-- 6E F6 flags a following LDC to be a name length followed by that number
--       of bytes as a compiler comment (gencommentX)
-- 6E F7 flags a following LDC to be a length followed by that number of
--	 bytes making a CODEMAP entry
-- 6E F8 flags a following LDC to be for a data length followed by that number
--       of data bytes
-- 6E F9 flags a following LDC to be a name length followed by that number
--	 of bytes as a label name to be loaded
-- 6E FA flags a following LDC to be a name length followed by that number
--	 of bytes to load a code-map by name
-- 6E FB flags a following LDC to be a name length followed by that number
--       of bytes as a GLOBNAMEEND to mark the end of an exported PROC
--
--
-- 6F F0 flags a following LDC to define a magic message number
-- 6F F1 flags a following LDC to define a TSDEPTH
-- 6F F2 flags a following LDC to define FUNCRESULTS expected in registers
-- 6F F3 flags a following LDC to define FUNCRETURN with register results
-- 6F F4 flags a following LDC to define ENDWS increment
-- 6F F5 flags a following LDC to define REALRESULT (s,d)
-- 6F F6 flags a following LDC to define SETLAB label number setting
-- 6F F7 flags a following LDC to define SECTIONLAB label number setting
-- 6F F8 flags a following LDC to define ALIGN requirement
-- 6F F9 flags a following LDC to define a source line number
-- 6F FA flags a following LDC to define a DEBUGLINE reference
-- 6F FB flags a following LDC to define SETWS workspace size
-- 6F FC flags a following LDC to define SETVS vectorspace size
-- 6F FD flags a following LDC as right operand of a SLL operation
-- 6F FE flags a following LDC as right operand of a SRL operation
*/


/*}}}*/


PRIVATE BYTE *codebuff = NULL;	/* Buffer to hold code */
PRIVATE int ctop;

PRIVATE int max_code_size = COMPILING_CODE_SIZE * 4;



/*{{{  add_code*/
PRIVATE void add_code (BYTE byteval)
{
	if ((ctop + 1) >= max_code_size) {
		/* make buffer larger */
		int newcodesize = max_code_size * 2;
		BYTE *newcodebuff = memalloc (newcodesize);

		memset (newcodebuff, '\0', newcodesize);
		memcpy (newcodebuff, codebuff, ctop);

		memfree (codebuff);
		codebuff = newcodebuff;
		max_code_size = newcodesize;
	}

	codebuff[ctop++] = byteval;
}

/*}}}*/
/*{{{  PRIVATE int assemble_instruction(v)*/
/***************************************************************************
 *
 * assemble_instruction adds the prefix bytes required for operand to the
 *        buffer, and returns the length
 *
 **************************************************************************/
PRIVATE int assemble_instruction (const INT32 instruction, INT32 operand)
{
	BYTE buf[MAX_BYTES_IN_INSTRUCTION];
	int ptr, len;

	if (operand > 0) {	/* most common */
		ptr = 0;
		while (operand > 0) {
			buf[ptr++] = (BYTE) (I_PFIX | (operand & 0xf));
			operand >>= 4;
		}
	} else if (operand == 0) {
		buf[0] = 0;
		ptr = 1;
	} else if (operand >= -16) {
		buf[1] = I_NFIX | 0;
		buf[0] = (BYTE) (operand & 0xf);
		ptr = 2;
	} else {
		ptr = 0;
		operand = ~operand;
		while (operand > 15) {
			buf[ptr++] = (BYTE) (I_PFIX | ((~operand) & 0xf));
			operand >>= 4;
		}
		buf[ptr++] = (BYTE) (I_NFIX | operand);
	}

	buf[0] = (BYTE) (instruction | (int) (buf[0] & 0xf));

	len = ptr;
	while (--ptr >= 0)
		add_code (buf[ptr]);
	return len;
}

/*}}}*/
/*{{{  defines for ETC*/

#define SPE_INVALID 0
#define SPE_BOOLINVERT 1
#define SPE_STARTTABLE 2
#define SPE_WIDENSHORT 3
#define SPE_LOOPHEADBOT 4
#define SPE_CONTRSPLIT 5
#define SPE_CONTRJOIN 6
#define SPE_I64TOREAL 7
#define SPE_NOTPROCESS 8
#define SPE_FPPOP 9
#define SPE_CHECKNOTNULL 10
#define SPE_SEMCLAIM 11
#define SPE_SEMRELEASE 12
#define SPE_SEMINIT 13
#define SPE_RESCHEDULE 14
#define SPE_INDIRECT_AREG 15
#define SPE_INDIRECT_BREG 16
#define SPE_INDIRECT_CREG 17
#define SPE_RMWSMAP 18
#define SPE_MPPCLONE 19
#define SPE_MPPSERIALISE 20
#define SPE_MPPDESERIALISE 21
#define SPE_LOADCODEMAP 22
#define SPE_FBARINIT 23
#define SPE_FBARSYNC 24
#define SPE_FBARRESIGN 25
#define SPE_FBARENROLL 26
/* #define SPE_MTNEW 27 */
/* #define SPE_MTFREE 28 */
/* #define SPE_MTCLONE 29 */
#define SPE_R32SIN 30
#define SPE_R64SIN 31
#define SPE_R32COS 32
#define SPE_R64COS 33
#define SPE_DTRACE 34
#define SPE_KILLCALL 35
#define SPE_WAIT_FOR_INTERRUPT 36
#define SPE_R32TAN 37
#define SPE_R64TAN 38

#define TSDEPTH 1
#define FUNCRESULTS 2
#define FUNCRETURN 3
#define ENDWS 4
#define REALRESULT 5
#define SETLAB 6
#define SECTIONLAB 7
#define ALIGN 8
#define LINENUM 9
#define DEBUGLINE 10
#define SETWS 11
#define SETVS 12
#define SLLIMM 13
#define SRLIMM 14
#define LOOPHEADTOP 15

#define STUBNAME 64
#define GLOBNAME 65
#define JUMPENTRY 66
#define PROCNAME 67
#define SOURCEFILE 68
#define OCCCOMMENT 69
#define CODEMAP 70
#define DATABYTES 71
#define MESSAGEBYTES 72
#define LOADLABELNAME 73
#define LOADCODEMAPNAME 74
#define GLOBNAMEEND 75
/*}}}*/

/*{{{  etc_pinst*/
PRIVATE void etc_pinst (const int pinst, const INT32 opd)
{
	assemble_instruction (pinst, opd);
	if (ims_asm_output) {
		fprintf (etcfile, "  %-8s %d\n", primaryname (pinst), (int)opd);
	}
}

/*}}}*/
/*{{{  etc_sinst*/
PRIVATE void etc_sinst (const int sinst)
{
	assemble_instruction (I_OPR, sinst);
	if (ims_asm_output) {
		fprintf (etcfile, "  %-8s\n", secondaryname (sinst));
	}
}

/*}}}*/
/*{{{  etc_linst*/
PRIVATE void etc_linst (const int linst, const int label)
{
	add_code (0x6D);
	add_code (0xF0);
	assemble_instruction (linst, label);
	if (ims_asm_output) {
		fprintf (etcfile, "  %-8s L%d\n", primaryname (linst), label);
	}
}

/*}}}*/
/*{{{  assemble_namebytes*/
PRIVATE void assemble_namebytes (const BYTE opbyte, const char *name)
{
	int i;
	int len = strlen (name);
	add_code (0x6E);
	add_code (opbyte);
	assemble_instruction (I_LDC, len);
	for (i = 0; i < len; i++) {
		add_code (name[i]);
	}
}

/*}}}*/
/*{{{  assemble_databytes*/
PRIVATE void assemble_databytes (const int len, const BYTE * data, const int swap)
{
	int mask = target_bigendian ? bytesinscalar (swap) - 1 : 0;
	int i;

	add_code (0x6E);
	add_code (0xF8);
	assemble_instruction (I_LDC, len);

	for (i = 0; i < len; i++) {
		add_code (data[i ^ mask]);
	}
}

/*}}}*/
/*{{{  etc_oinst*/
PRIVATE void etc_oinst (const int oinst, const char *pname)
{
	assemble_namebytes ((BYTE) (oinst + (0xF1 - STUBNAME)), pname);
	if (ims_asm_output) {
		/*{{{  switch arrording to which operation */
		switch (oinst) {
		case STUBNAME:
			fprintf (etcfile, "  %-8s $%s\n", "extern", pname);
			break;
		case GLOBNAME:
			fprintf (etcfile, "  %-8s $%s\n$%s:\n", "global", pname, pname);
			break;
		case JUMPENTRY:
			fprintf (etcfile, "  %-8s $%s\n", "j", pname);
			break;
		case PROCNAME:
			fprintf (etcfile, "  %-8s %s\n", "PROCENT", pname);
			break;
		case SOURCEFILE:
			fprintf (etcfile, "  %-8s \"%s\"\n", "SOURCEFILE", pname);
			break;
		case OCCCOMMENT:
			fprintf (etcfile, "  %-8s \"%s\"\n", "OCCCOMMENT", pname);
			break;
		case CODEMAP:
			fprintf (etcfile, "  %-8s \"%s\"\n", "CODEMAP", pname);
			break;
		case DATABYTES:
			fprintf (etcfile, "  %-8s \"%s\"\n", "DATABYTES", pname);
			break;
		case LOADLABELNAME:
			fprintf (etcfile, "  %-8s \"%s\"\n", "LOADLABELNAME", pname);
			break;
		case LOADCODEMAPNAME:
			fprintf (etcfile, "  %-8s \"%s\"\n", "LOADCODEMAPNAME", pname);
			break;
		case GLOBNAMEEND:
			fprintf (etcfile, "  %-8s \"%s\"\n", "GLOBNAMEEND", pname);
			break;
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  etc_llinst*/
PRIVATE void etc_llinst (const int llinst, const int label1, const int label2)
{
	assert (llinst == I_LDC);
	add_code (0x6D);
	add_code (0xF0);
	assemble_instruction (llinst, label1);
	assemble_instruction (llinst, label2);
	if (ims_asm_output) {
		/*{{{  pseudo assembly language */
		if (label2 == NOLAB) {
			fprintf (etcfile, "  LDA      L%d\n", label1);
		} else {
			fprintf (etcfile, "  LDC      L%d-L%d\n", label1, label2);
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  etc_leinst*/
PRIVATE void etc_leinst (const int wsoff, const int endlabel, const int looplabel)
{
	add_code (0x6D);
	add_code (0xF1);
	assemble_instruction (I_LDLP, wsoff);	/* ws offset of first of 2-word loop block */
	assemble_instruction (I_LDC, endlabel);	/* where to jump at end of loop */
	assemble_instruction (I_LDC, looplabel);	/* where to loop back to at top */
	if (ims_asm_output) {
		/*{{{  pseudo assembly language */
		fprintf (etcfile, "  LOOPEND      %d,L%d,L%d\n", wsoff, endlabel, looplabel);
		/*}}} */
	}
}

/*}}}*/
/*{{{  etc_le3inst*/
PRIVATE void etc_le3inst (const int wsoff, const int endlabel, const int looplabel)
{
	add_code (0x6d);
	add_code (0xf2);
	assemble_instruction (I_LDLP, wsoff);	/* ws offset of 4-word loop block */
	assemble_instruction (I_LDC, endlabel);	/* get-out-of-loop label */
	assemble_instruction (I_LDC, looplabel);	/* start-of-loop label */
	if (ims_asm_output) {
		/*{{{  pseudo assembly language */
		fprintf (etcfile, "  LOOPEND3     %d,L%d,L%d\n", wsoff, endlabel, looplabel);
		/*}}} */
	}
	return;
}

/*}}}*/
/*{{{  etc_lebinst*/
PRIVATE void etc_lebinst (const int wsoff, const int endlabel, const int looplabel)
{
	add_code (0x6d);
	add_code (0xf3);
	assemble_instruction (I_LDLP, wsoff);	/* ws offset of first of 2-word loop block */
	assemble_instruction (I_LDC, endlabel);	/* where to jump at end of loop */
	assemble_instruction (I_LDC, looplabel);	/* where to loop back to at top */
	if (ims_asm_output) {
		/*{{{  pseudo assembly language */
		fprintf (etcfile, "  LOOPEND      %d,L%d,L%d\n", wsoff, endlabel, looplabel);
		/*}}} */
	}
}

/*}}}*/
#ifdef MOBILES
/*{{{  etc_msusageinst*/
PRIVATE void etc_msusageinst (const int msusage)
{
	BETRACE ("code1: etc_msusageinst (enter): msusage=%d", msusage);
	add_code (0x6d);
	add_code (0xf4);
	assemble_instruction (I_LDC, msusage);
	if (ims_asm_output) {
		fprintf (etcfile, "  SETMS        %d\n", msusage);
	}
	BETRACE ("code1: etc_msusageinst (leave)");
	return;
}
/*}}}  */
/*{{{  etc_ldc_for_mobileinit */
PRIVATE void etc_ldc_for_mobileinit (const int val)
{
	assemble_instruction (I_LDC, val);
	return;
}
/*}}}  */
/*{{{  genmobileinit */
PUBLIC void genmobileinit (const int msp_offset, treenode *nptr)
{
	BETRACE ("code1: genmobileinit (enter): msp_offset=%d, nptr=%p", msp_offset, nptr);
	add_code (0x6d);
	add_code (0xf5);
	assemble_instruction (I_LDC, msp_offset);
	mobilegeninitsequence (nptr, etc_ldc_for_mobileinit, 1);
	BETRACE ("code1: genmobileinit (leave)");
	return;
}
/*}}}  */
/*{{{  genmobileinitdynpar */
PUBLIC void genmobileinitdynpar (const int msp_offset, treenode *sptr)
{
	BETRACE ("code1: genmobileinitdynpar (enter): msp_offset=%d, sptr=%p", msp_offset, sptr);
	add_code (0x6d);
	add_code (0xf5);
	assemble_instruction (I_LDC, msp_offset);
	mobilegeninitsequence (sptr, etc_ldc_for_mobileinit, 0);
	BETRACE ("code1: genmobileinitdynpar (leave)");
	return;
}
/*}}}  */
#endif
/*{{{  etc_specop*/
PRIVATE void etc_specop (const int specinst)
{
	const char *specopname[] = { "",
		"BOOLINVERT",
		"STARTTABLE",
		"WIDENSHORT",
		"LOOPHEADBOT",
		"CONTRSPLIT",
		"CONTRJOIN",
		"I64TOREAL",
		"NOTPROCESS",
		"FPPOP",
		"CHECKNOTNULL",
		"SEMCLAIM",
		"SEMRELEASE",
		"SEMINIT",
		"RESCHEDULE",
		"INDIRECT_AREG",
		"INDIRECT_BREG",
		"INDIRECT_CREG",
		"RMWSMAP",
		"MPPCLONE",
		"MPPSERIALISE",
		"MPPDESERIALISE",
		"LOADCODEMAP",
		"FBARINIT",
		"FBARSYNC",
		"FBARRESIGN",
		"FBARENROLL",
		"MTNEW",
		"MTFREE",
		"MTCLONE",
		"R32SIN",
		"R64SIN",
		"R32COS",
		"R64COS",
		"DTRACE",
		"KILLCALL",
		"WAIT_FOR_INTERRUPT",
		"R32TAN",
		"R64TAN"
	};
	add_code (0x6f);
	add_code (0xf0);
	assemble_instruction (I_LDC, specinst);
	if (ims_asm_output) {
		fprintf (etcfile, "  %-8s\n", specopname[specinst]);
	}
	return;
}

/*}}}*/
/*{{{  etc_cinst*/
PRIVATE void etc_cinst (const int cinst, const int pval)
{
	const char *specialname[] = { "",
		"TSDEPTH",
		"FUNCRESULTS",
		"FUNCRETURN",
		"ENDWS",
		"REALRESULT",
		"SETLAB",
		"SECTIONLAB",
		"ALIGN",
		"LINENUM",
		"DEBUGLINE",
		"SETWS",
		"SETVS",
		"SLLIMM",
		"SRLIMM",
		"LOOPHEADTOP"
	};
	add_code (0x6F);
	add_code (0xF1 + (cinst - TSDEPTH));
	assemble_instruction (I_LDC, pval);
	if (ims_asm_output) {
		/*{{{  pseudo assembly language */
		switch (cinst) {
		case SETLAB:
		case SECTIONLAB:
			fprintf (etcfile, "L%d:\n", pval);
			break;
		default:
			fprintf (etcfile, "  %-8s %d\n", specialname[cinst], pval);
			break;
		}
		/*}}} */
	}
}

/*}}}*/
/*}}}*/
/*{{{  private routines*/
/*{{{  tstack diagnostics*/
/* a value between -3,3 is the change in depth of tstack */
/* a value >= 16 is 16 more than the new ts_depth to be set */
/*{{{  primaries*/
const int tsdiff_prim[16] = { 16, 1, 0, 0,	/* 00 j, ldlp, pfix, ldnl */
	1, 0, 0, 1,		/* 04 ldc, ldnlp, nfix, ldl */
	0, 16, -1, 0,		/* 08 adc, call, cj, ajw */
	0, -1, -2, 0
};				/* 0C eqc, stl, stnl, opr */
/*}}}*/
/*{{{  secondaries*/
const int tsdiff_sec[0xB0] = { \
	0, 0, -1, 16,		/* 00 rev, lb, bsub, endp */
	-1, -1, 0, 16,		/* 04 diff, add, gcall, in */
	-1, -1, -1, 16,		/* 08 prod, gt, wsub, out  */
	-1, 16, 16, 16,		/* 0C sub, startp, outbyte, outword */
	0, 16, 0, -1,		/* 10 seterr, mreleasep, resetch, csub0 */
	-2, 16, -2, -1,		/* 14 extvrfy, stopp, ladd, stlb */
#ifdef PROCESS_PRIORITY
	-2, 1, -1, 0,		/* 18 sthf, norm, ldiv, ldpi */
#else
	-1, 1, -1, 0,		/* 18 sthf, norm, ldiv, ldpi */
#endif
	-1, 1, 1, -1,		/* 1C stlf, xdble, ldpri, rem */
	16, 16, 1, 0,		/* 20 ret, lend, ldtimer, . */
	0, 0, 0, 0,		/* 24 ., ., ., . */
	0, 1, 1, 16,		/* 28 ., testerr, testpranal, tin */
	-1, 0, -2, -2,		/* 2C div, ., dist, disc */
	-1, -1, 0, -1,		/* 30 diss, lmul, not, xor */
	0, -1, -1, -1,		/* 34 bcnt, lshr, lshl, lsum */
	-2, 0, -1, -2,		/* 38 lsub, runp, xword, sb */
#ifdef PROCESS_PRIORITY
	0, -1, -2, 1,		/* 3C gajw, savel, saveh, wcnt */
#else
	0, -1, -1, 1,		/* 3C gajw, savel, saveh, wcnt */
#endif
	-1, -1, 1, 0,		/* 40 shr, shl, mint, alt */
	16, 0, -1, -1,		/* 44 altwt, altend, and, enbt */
	-1, 0, -3, -1,		/* 48 enbc, enbs, move, or */
	-1, -1, 0, -1,		/* 4C csngl, ccnt1, talt, ldiff */
#ifdef PROCESS_PRIORITY
	-2, 16, -1, -1,		/* 50 sthb, taltwt, sum, mul */
#else
	-1, 16, -1, -1,		/* 50 sthb, taltwt, sum, mul */
#endif
	-1, 0, -1, 0,		/* 54 sttimer, stoperr, cword, clrhalterr */
	0, 1, 1, 16,		/* 58 sethalterr, testhalterr, dup, move2dinit */
	16, 16, 16, 0,		/* 5C move2dall, move2dnonzero, move2dzero, . */
	16, 16, 16, 19,		/* 60 extin, extout, minn, unpacksn */
	16, 16, -1, -2,		/* 64 moutn, xminn, extenbc, extndisc */
	0, 0, 0, 0,		/* 68 ., ., ., . */
	19, -2, 0, 0,		/* 6C postnormsn, roundsn, ., . */
	-2, 0, -1, 0,		/* 70 enbc3, ldinf, fmul, cflerr */
	-1, -1, -1, 0,		/* 74 crcword, crcbyte, bitcnt, bitrevword */
	-1, -1, 0, 0,		/* 78 bitrevnbits, pop, ., . */
	0, 0, 0, 0,		/* 7C ., ., ., . */
	0, -1, -2, 0,		/* 80 ., wsubdb, fpldnldbi, fpchkerr */
	-1, 0, -2, 0,		/* 84 fpstnldb, ., fpldnlsni, fpadd */
	-1, 0, -1, 0,		/* 88 fpstnlsn, fpsub, fpldnldb, fpmul */
	0, 0, -1, 1,		/* 8C fpdiv, ., fpldnlsn, fpremfirst  */
	1, 1, 1, 1,		/* 90 fpremstep, fpnan, fpordered, fpnotfinite */
	1, 1, -1, 0,		/* 94 fpgt, fpeq, fpi32tor32, . */
	-1, -2, -1, 0,		/* 98 fpi32tor64, enbt3, fpb32tor64, . */
	1, 0, -1, 0,		/* 9C fptesterr, fprtoi32, fpstnli32, fpldzerosn */
	0, 0, 1, 0,		/* A0 fpldzerodb, fpint, getpri, fpdup */
	0, 16, -1, 0,		/* A4 fprev, setpri, fpldnladddb, . */
	-1, 0, -1, -1,		/* A8 fpldnlmuldb, ., fpldnladdsn, fpentry */
	-1, -1, 0, 0		/* AC fpldnlmulsn, enbs3, ., . */
};
const int tsdiff_sec_ecodes[0x10] = { \
	0, -1, 0, -1,		/* E0 mnew, mfree, malloc, mrelease */
	16, 16, 16, 16,		/* E4 min, mout, min64, mout64 */
	16, 16, 16, 16,		/* E8 xable, xin, xmin, xmin64 */
	-1, -2, -2, -1		/* EC xend, ndisc, ndist, ndiss */
};
static int tsdiff_sec_twocodes[] = { \
	16, 16, 16, 16,	/* 200: in8, in32, out8, out32 */
	0, 16, 16, 16,	/* 204: xstl, ncall, nret, nwsadj */
	16, 0, 0, -2,	/* 208: nstartp, nneg, nlw, nsw */
	0, -2, -2, 16,	/* 20c: naltend, nmwenb, nmwdis, nmwaltwt */
	0, 16, 0, -1,	/* 210: nmwalt, nmwaltend, fppop, mws_binit */
	-2, -1, -2, -3,	/* 214: mws_pbrilnk, mws_pbrulnk, mws_ppilnk, mws_pbenroll */
	16, 16, 16, 16,	/* 218: mws_pbresign, mws_pbadjsync, mws_sync, mws_altlock */
	0, 0, 0, -2,	/* 21c: mws_altunlock, mws_alt, mws_altend, mws_enb */
	-2, 0, 0, 0,	/* 220: mws_dis, mws_altpostlock, mws_ppbaseof, mws_ppparof */
	-3, -3, -2, -2,	/* 224: ior, iow, ior8, iow8 */
	-2, -2, -2, -2,	/* 228: ior16, iow16, ior32, iow32 */
	-2, -2, -1, -1,	/* 22c: enbc2, enbt2, enbs2, proc_alloc */
	-3, -3, -3, 16, /* 230: proc_param, proc_mt_copy, proc_mt_move, proc_start */
	16, 1, 16, 1,	/* 234: proc_end, getaff, setaff, getpas */
	-1, -1, 0, 16,	/* 238: mt_alloc, mt_release, mt_clone, mt_in */
	16, 16, 16, 16,	/* 23c: mt_out, mt_xchg, mt_lock, mt_unlock */
	-2, -2, 16, 16,	/* 240: mt_enroll, mt_resign, mt_sync, mt_xin */
	16, 16, -2, -2,	/* 244: mt_xout, mt_xxchg, mt_dclone, mt_bind */
	0, 0, 0, 16,    /* 248: mb, rmb, wmb, ext_mt_in */
	16, -2          /* 24c: ext_mt_out, mt_resize */
};
/*}}}*/
PRIVATE char tstack[4];
PRIVATE int ts_depth;
/*{{{  void check_tsdepth (void)*/
PRIVATE void check_tsdepth (void)
{
	if (ts_depth < 0) {
		genwarn (GEN_TSTACK_UNDERFLOW);
		/* fprintf (stderr, "\n-- stackdepth =  underflow !!!"); */
		if (assembly_output) {
			fprintf (outfile, "\n-- stackdepth =  underflow !!!");
		}
		if (etc_output) {
			etc_cinst (TSDEPTH, ts_depth);
		}
		ts_depth = 0;
	} else if (ts_depth > 3) {
		genwarn (GEN_TSTACK_OVERFLOW);
		/* fprintf (stderr, "\n-- stackdepth =  overflow !!!"); */
		if (assembly_output) {
			fprintf (outfile, "\n-- stackdepth =  overflow !!!");
		}
		if (etc_output) {
			etc_cinst (TSDEPTH, ts_depth);
		}
		ts_depth = 3;
	}
}

/*}}}*/
/*{{{  void note_tstack (void)*/
PRIVATE void note_tstack (void)
{
	tstack[0] = (ts_depth >= 1) ? 'A' : '~';
	tstack[1] = (ts_depth >= 2) ? 'B' : '~';
	tstack[2] = (ts_depth >= 3) ? 'C' : '~';
	tstack[3] = '\0';
}

/*}}}*/
/*{{{  void undefine_tstack (void)*/
PRIVATE void undefine_tstack (void)
{
	if (etc_output && (ts_depth != 0)) {
		etc_cinst (TSDEPTH, 0);
	}
	ts_depth = 0;
}

/*}}}*/
#if 0
/* NOT USED */
/*{{{  void pop_tstack (void)*/
PRIVATE void pop_tstack (void)
{
	ts_depth--;
}
#endif

/*}}}*/
/*}}}*/
/*{{{  PRIVATE void diag_write_spaces*/
PRIVATE void diag_write_spaces (void)
{
	fputc ('\t', outfile);
}

/*}}}*/
/*{{{  PRIVATE void diag_write_nl*/
PRIVATE void diag_write_nl (void)
{
	fputc ('\n', outfile);
	in_comment = FALSE;
	diag_write_spaces ();
}

/*}}}*/
/*{{{  PRIVATE void diag_write_comment*/
PRIVATE void diag_write_comment (void)
{
	if (in_comment) {
		fputc (' ', outfile);
	} else {
		diag_write_spaces ();
		fputs ("-- ", outfile);
		in_comment = TRUE;
	}
}

/*}}}*/
/*{{{  PRIVATE void diag_write_nl_comment*/
PRIVATE void diag_write_nl_comment (void)
{
	fputc ('\n', outfile);
	in_comment = FALSE;
	diag_write_comment ();
}

/*}}}*/
/*{{{  PRIVATE void diag_write_nl_string*/
PRIVATE void diag_write_nl_string (const char *str)
{
	diag_write_nl ();
	fputs (str, outfile);
}

/*}}}*/
/*{{{  PRIVATE topcrease*/
PRIVATE void topcrease ()
{
	fputs ("\n--{{{  ", outfile);
}

/*}}}*/
/*{{{  PRIVATE bottomcrease*/
PRIVATE void bottomcrease ()
{
	fputs ("\n--}}}", outfile);
}

/*}}}*/
/*{{{  PUBLIC void gencommentn(c, ...)*/
/***************************************************************************
 *
 *  gencommentn writes the printf format string 'c' (with parameters p1, p2,
 *             pn) as a comment to the debugging file.
 *
 **************************************************************************/
/* Write a comment */
PUBLIC void gencomment0 (const char *const c)
{
	if (assembly_output && ((alloc_strategy & ALLOC_NOCOMMENTS) == 0)) {
		diag_write_comment ();
		fputs (c, outfile);
	} else if (etc_output && codebuff) {
		etc_oinst (OCCCOMMENT, c);
	}
}

/* Write a comment */
PUBLIC void gencomment1 (const char *const c, const BIT32 p1)
{
	static char x_buffer[1024];

	if (assembly_output && ((alloc_strategy & ALLOC_NOCOMMENTS) == 0)) {
		diag_write_comment ();
		fprintf (outfile, c, p1);
	} else if (etc_output && codebuff) {
		if (strlen (c) < 1000) {
			sprintf (x_buffer, c, p1);
			etc_oinst (OCCCOMMENT, x_buffer);
		}
	}
}
PUBLIC void gencomments (const char *const c, const char *p1)
{
	static char x_buffer[1024];

	if (assembly_output && ((alloc_strategy & ALLOC_NOCOMMENTS) == 0)) {
		diag_write_comment ();
		fprintf (outfile, c, p1);
	} else if (etc_output && codebuff) {
		if ((strlen (c) + (p1 ? strlen (p1):0)) < 1024) {
			sprintf (x_buffer, c, p1);
			etc_oinst (OCCCOMMENT, x_buffer);
		}
	}
}

#ifdef HAVE_STDARG_H
PUBLIC void gencommentv (const char *const fmt, ...)
{
	static char x_buffer[1024];
	va_list ap;

	va_start (ap, fmt);
	if (assembly_output && ((alloc_strategy & ALLOC_NOCOMMENTS) == 0)) {
		diag_write_comment ();
		vfprintf (outfile, fmt, ap);
	} else if (etc_output && codebuff) {
		vsnprintf (x_buffer, 1023, fmt, ap);
		etc_oinst (OCCCOMMENT, x_buffer);
	}

	va_end (ap);
	return;
}
#endif

/*}}}*/
/*}}}*/
/*{{{  PUBLIC genstartconstblock(c)*/
PUBLIC void genstartconstblock (const char *c)
{
	if (assembly_optimise) {
		topcrease ();
		fprintf (outfile, "Constant Block %s", c);
	}
}

/*}}}*/
/*{{{  PUBLIC genendconstblock*/
PUBLIC void genendconstblock ()
{
	if (assembly_optimise) {
		bottomcrease ();
	}
}

/*}}}*/

/*{{{  PRIVATE BOOL name_is_visible*/
PRIVATE BOOL name_is_visible (const wordnode * const name, const seginfo_t * rest)
/* Returns TRUE if the name is not descoped by a later externally visible name */
{
	for (; rest != NULL; rest = rest->seg_next) {
		if (name == NNameOf (rest->seg_name)) {
			return FALSE;
		}
	}
	return TRUE;
}

/*}}}*/
/*{{{  PRIVATE void output_entry_points*/
/*{{{  comment*/
/***************************************************************************
 *
 *  output_entry_points writes all the declared entry points into the
 *                      object file.
 *
 **************************************************************************/
/*}}}*/
PRIVATE void output_entry_points (void)
{
	seginfo_t *seg;

	/* first reverse the list back into the correct order */
	{
		seginfo_t *p = seginfo, *reverser = NULL, *temp;
		while (p != NULL) {
			temp = p->seg_next;
			p->seg_next = reverser;
			reverser = p;
			p = temp;
		}
		seginfo = reverser;
	}

	for (seg = seginfo; seg != NULL; seg = seg->seg_next) {
		treenode *const n = seg->seg_name;
		const INT32 offset = ZERO32;	/* we do not need this with no binary code */
		if (name_is_visible (NNameOf (n), seg->seg_next)) {
			if (information) {
		#ifdef MOBILES
				fprintf (outfile, "%s %s requires %ld workspace, %ld vectorspace and %ld mobilespace words\n",
					 ((TagOf (n) == N_PROCDEF) || (TagOf (n) == N_MPROCDECL)) ? "PROC" : "FUNCTION", WNameOf (translate_from_internal (NNameOf (n))),	/* MDP */
					 (long) NPDatasizeOf (n), (long) NPVSUsageOf (n), (long)NPMSUsageOf (n));
		#else
				fprintf (outfile, "%s %s requires %ld workspace and %ld vectorspace words\n",
					 ((TagOf (n) == N_PROCDEF) || (TagOf (n) == N_MPROCDECL)) ? "PROC" : "FUNCTION", WNameOf (translate_from_internal (NNameOf (n))),	/* MDP */
					 (long) NPDatasizeOf (n), (long) NPVSUsageOf (n));
		#endif
			}
			write_entry_desc (n, offset);
		}
	}
}

/*}}}*/

/*{{{  PUBLIC void compress_code()                       called from GEN1*/
PUBLIC void compress_code (void)
{
	if (diagnostics) {
		gencomment0 ("compress code");
	}
}

/*}}}*/
/*{{{  run time locate information*/
/*{{{  PUBLIC void new_occam_line       from GEN1t,7t,8t,10t*/
/*****************************************************************************
 *
 *  combines coder_genlocate and mark_occam_line
 *
 *****************************************************************************/
PUBLIC void new_occam_line (treenode * laddress, BOOL write_linenumber, BOOL needs_locate, BOOL clear_tstack)
{
	dead = dead & (!inside_asm);
	if (debugoutput && needs_locate) {
		/* add_debug_info(C_LOCATE, laddress, 0); */
		if (etc_output) {
			const int debug_linemark_index = (laddress == NULL) ? 0 : get_from_index_table (laddress);
			etc_cinst (DEBUGLINE, debug_linemark_index);
		}
	}
	/*{{{  write linenumber */
	if (write_linenumber) {
		const SOURCEPOSN locn = sourcefileof (LocnOf (laddress));
		if (locn != oldlocn) {
			if (assembly_output && ((alloc_strategy & ALLOC_NOLINENUMS) == 0)) {
				diag_write_nl_string ("-- ");
				fprintf (outfile, "\"%s\": %d", lookupfilename (FileNumOf (locn)), FileLineOf (locn));
				DEBUG_MSG ((" addr: %lX", (BIT32) laddress));
			}
			if (etc_output) {
				if ((oldlocn == NOPOSN) || (FileNumOf (oldlocn) != FileNumOf (locn))) {
					etc_oinst (SOURCEFILE, lookupfilename (FileNumOf (locn)));
				}
				etc_cinst (LINENUM, FileLineOf (locn));
			}
			oldlocn = locn;
		}
	}
	/*}}} */
	/*{{{  reset tstack */
	if (clear_tstack && ts_depth > 0) {
		if (diagnostics) {
			gencomment1 (" stack depth = %d", ts_depth);
		}
		undefine_tstack ();
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void coder_genlocate - takes treenode address as its param*/
#if 0
/*****************************************************************************
 *
 *  coder_genlocate inserts a runtime locate item record into the code buffer
 *
 *****************************************************************************/
PUBLIC void coder_genlocate (treenode * const addr, const BOOL write_linenumber)
{
	/* add_debug_info(C_LOCATE, addr, 0); */
	/*{{{  debugging */
	if ( /*diagnostics */ assembly_output && write_linenumber &&
	    ((alloc_strategy & ALLOC_NOLINENUMS) == 0)) {
		const SOURCEPOSN locn = sourcefileof (LocnOf (addr));
		/*diag_write_nl_string("-- Locate"); */
		diag_write_nl_string ("-- ");
		fprintf (outfile, "\"%s\": %d", lookupfilename (FileNumOf (locn)), FileLineOf (locn));
		DEBUG_MSG ((" addr: %lX", (BIT32) addr));
	}
	/*}}} */
	/*{{{  source output */
	/* removed source_output 14/3/91 */
	/*if (source_output)
	   output_source_code(get_from_index_table(addr), C_LOCATE); */
	/*}}} */
}
#endif
/*}}}*/
/*{{{  PUBLIC void coder_genaddrfix (address) - takes treenode address as its param  from GEN1*/
/*****************************************************************************
 *
 *  coder_genaddrfix inserts an addressfix item record into the code buffer
 *
 *****************************************************************************/
PUBLIC void coder_genaddrfix (treenode * addr, const int label)
{
	/*{{{  debugging */
	if (diagnostics) {
		diag_write_nl_string ("-- Addressfix");
	}
	/*}}} */
}

/*}}}*/
/*{{{  PRIVATE void coder_genlibrpatch ()*/
/*****************************************************************************
 *
 *  coder_genlibrpatch inserts a libpatch item record into the code buffer
 *
 *****************************************************************************/
PRIVATE void coder_genlibrpatch (void)
{
	/*{{{  debugging */
	if (diagnostics) {
		diag_write_nl_string ("-- Libpatch");
	}
	/*}}} */
}

/*}}}*/
/*}}}*/
/*{{{  instruction generation*/
/*{{{  PRIVATE void clean_fpu_flag*/
PRIVATE void clean_fpu_flag (const int instruction)
{
	if (inside_asm) {
		return;
	}

	if (has_fpu_core && (fpu_error_status == ERROR_STATUS_START) && (errormode & ERRORMODE_TIMESLICECHECK)) {
		/* This can be called with either FPTESTERR or FPUCLRERR,
		   depending on whether we're doing localised error handling.
		 */
		if (instruction & I_FPU_ENTRY_BIT) {	/* bug 1206 13/3/91 */
			genfpuentry (instruction);
		} else {
			gensecondary (instruction);
		}
		fpu_error_status = ERROR_STATUS_CLEAN;
	}
}

/*}}}*/
/*{{{  PRIVATE void dirty_error_flag*/
/*****************************************************************************
*
*  dirty_error_flag is used incase we've never got around to clearing
*                   the error flag in STOP mode.
*
******************************************************************************/
PRIVATE void dirty_error_flag (const BOOL fpu_not_int)
{
	/* We cannot localise handling of the normal error flag, cos
	   the TESTERR instruction clobbers the stack;
	   For the FPU we can use FPUCLRERR which leaves all stacks alone */

	if (inside_asm) {
		return;
	}

	if (fpu_not_int) {
		fpu_error_status = ERROR_STATUS_DIRTY;
	} else {
		int_error_status = ERROR_STATUS_DIRTY;
	}
}

/*}}}*/
/*{{{  PUBLIC void markdeadcode*/
PUBLIC void markdeadcode ()
{
	dead = !inside_asm;
}

/*}}}*/
/*{{{  PUBLIC void genstartblock()                           called from GEN1,9,10*/
/*****************************************************************************
*
*  genstartblock is used in STOP mode to remember to clear the error flag
*                after a process could have been timesliced by a jump
*                to the start of the block
*
******************************************************************************/
PUBLIC void genstartblock (void)
{
	/* We cannot localise handling of the normal error flag, cos
	   the TESTERR instruction clobbers the stack;
	   For the FPU we can use FPUCLRERR which leaves all stacks alone */

	if (inside_asm) {
		return;
	}

	int_error_status = ERROR_STATUS_CLEAN;
	if (diagnostics) {
		gencomment0 ("genstartblock");
	}
	if (errormode & ERRORMODE_TIMESLICECHECK) {
		gencomment0 ("kroc/frmb: not doing TESTERR -- this is broken!");
		/* gensecondary (I_TESTERR); */
	}

	fpu_error_status = ERROR_STATUS_START;
#if !LOCALISE_ERROR_HANDLING
	clean_fpu_flag (I_FPTESTERR);	/* FPTESTERR is faster than FPUCLRERR */
#endif
}

/*}}}*/
/*{{{  PUBLIC void coder_return_from_call                    called from GEN8,10*/
PUBLIC void coder_return_from_call (const BOOL clean_fpu)
{
	/* this is called after a 'call' instruction to reset any information
	   about the error flags.
	   Our current model assumes that the integer error flag will always
	   be OK.
	   If 'clean_fpu' is FALSE, we have to make sure that we clean the
	   FPU error flag before we next use it.
	 */
	if (diagnostics) {
		gencomment0 ("coder return from call");
	}
	if (!clean_fpu) {
		fpu_error_status = ERROR_STATUS_START;
	}
}

/*}}}*/
/*{{{  PUBLIC void mark_flag_clean                           called from GEN5*/
PUBLIC void mark_flag_clean (const BOOL fpu_not_int)
{
	if (diagnostics) {
		gencomment0 ("mark flag clean");
	}
	if (fpu_not_int) {
		fpu_error_status = ERROR_STATUS_CLEAN;
	} else {
		int_error_status = ERROR_STATUS_CLEAN;
	}
}

/*}}}*/
/*{{{  PUBLIC void genprimary (instruction, operand)         called from GEN**/
/*{{{  comment*/
/***************************************************************************
 *
 *  genprimary adds code byte representing primary (instruction, operand)
 *             to the code buffer
 *
 **************************************************************************/
/*}}}*/
PUBLIC void genprimary (const int instruction, const INT32 operand)
{
	if (!dead) {
		note_tstack ();
		if (instruction == I_ADC) {
			dirty_error_flag (FALSE);
		}
		/* code_primary(instruction, operand); */
		if ((operand == ZERO32) && ((instruction == I_LDNLP) || (instruction == I_AJW) || (instruction == I_ADC))) {
			/* Null instruction */
			return;
		} else {
			if (etc_output) {
				etc_pinst (instruction, operand);
			}
			if (assembly_output) {
				diag_write_nl ();
				if (showtstack) {
					fprintf (outfile, "%3s ", tstack);
				}

				fprintf (outfile, "%-8s", primaryname (instruction));
				if ((alloc_strategy & ALLOC_NOOPERANDS) == 0) {
					fprintf (outfile, "%ld", (long) operand);
					/* display large(ish) numbers in hex too */
					if (operand > 4000 || operand < -4000) {
						gencomment1 ("#%lX", operand);
					}
				}
			}
			/*{{{  adjust ts_depth */
			{
				int diff = tsdiff_prim[instruction >> 4];
				if (diff >= 16) {
					ts_depth = diff - 16;
				} else {
					ts_depth += diff;
				}
				check_tsdepth ();
			}
			/*}}} */
		}
	}
}

/*}}}*/
/*{{{  PUBLIC void genfpuentry (int instruction)             called from GEN1,5,8,11,12*/
/*****************************************************************************
 *
 *  genfpuentry generates an fpuentry instruction
 *
 *****************************************************************************/
PUBLIC void genfpuentry (const int instruction)
{
	if (!dead) {
		/*{{{  check whether instruction may set error */
		switch (instruction) {
		default:
			break;
		case I_FPUSQRTFIRST:
		case I_FPUMULBY2:
		case I_FPUDIVBY2:
		case I_FPUEXPINC32:
		case I_FPUEXPDEC32:
		case I_FPUABS:
		case I_FPUSETERR:
		case I_FPUR32TOR64:
		case I_FPUR64TOR32:
		case I_FPUCHKI32:
		case I_FPUCHKI64:
			dirty_error_flag (TRUE);
			break;
		}
		/*}}} */
		note_tstack ();
		if (assembly_output) {
			diag_write_nl ();
			if (showtstack) {
				fprintf (outfile, "%3s ", tstack);
			}
			fprintf (outfile, "%-8s", secondaryname (instruction));
		}
		if (etc_output) {
			int fpinst = 0;
			switch (instruction) {
			case I_FPUSQRTFIRST:
				fpinst = I_FPSQRT;
				break;
			case I_FPURZ:
				fpinst = I_FPRZ;
				break;
			case I_FPURM:
				fpinst = I_FPRM;
				break;
			case I_FPURP:
				fpinst = I_FPRP;
				break;
			case I_FPURN:
				fpinst = I_FPRN;
				break;
			case I_FPUEXPDEC32:
				fpinst = I_FPEXPDEC32;
				break;
			case I_FPUEXPINC32:
				fpinst = I_FPEXPINC32;
				break;
			case I_FPUABS:
				fpinst = I_FPABS;
				break;
			case I_FPUCHKI64:
				fpinst = I_FPCHKI64;
				break;
			case I_FPUCHKI32:
				fpinst = I_FPCHKI32;
				break;
			case I_FPUDIVBY2:
				fpinst = I_FPDIVBY2;
				break;
			case I_FPUMULBY2:
				fpinst = I_FPMULBY2;
				break;
			case I_FPUR32TOR64:
				fpinst = I_FPR32TOR64;
				break;
			case I_FPUR64TOR32:
				fpinst = I_FPR64TOR32;
				break;
			case I_FPUNOROUND:
				{
					gensecondary (I_FPRZ);
					fpinst = I_FPR64TOR32;
					break;
				}
			case I_FPUSETERR:
			case I_FPUCLRERR:
			case I_FPUSQRTSTEP:
			case I_FPUSQRTLAST:
				fpinst = 0;
				break;
			default:
				printf ("Trying to generate FPU instr %d\n", instruction);
				assert (FALSE);
				break;
			}
			if (fpinst != 0) {
				gensecondary (fpinst);
			}
		}
		/*
		   code_primary(I_LDC, (int)(instruction & ~I_FPU_ENTRY_BIT));
		   code_secondary(I_FPENTRY);
		 */
	}
}

/*}}}*/
/*{{{  PUBLIC void genbranch (instruction, label)            called from GEN**/
/***************************************************************************
 *
 *  genbranch adds a record (representing a primary 'instruction' with
 *            operand the offset between the next instruction and the
 *            label 'label') to the code buffer.
 *
 **************************************************************************/
PUBLIC void genbranch (const int instruction, const int label)
{
	if (!dead) {
		if (instruction == I_ADC)
			/* Only happens inside ASM - eg ADC :label, or ADC routine.name */
			dirty_error_flag (FALSE);
		else
			checkerror ();	/* INSdi02485 *//* check before any transfer of control */
		note_tstack ();
		/*{{{  adjust ts_depth */
		{
			int diff = tsdiff_prim[instruction >> 4];
			if (diff >= 16)
				ts_depth = diff - 16;
			else
				ts_depth += diff;
			check_tsdepth ();
		}
		/*}}} */
		{
			int tstk;
			memcpy ((char *) &tstk, tstack, 4);
			/* label_ref(instruction, label, tstk); */
		}
		if (assembly_output) {
			diag_write_nl ();
			if (showtstack)
				fprintf (outfile, "%3s ", tstack);
			fprintf (outfile, "%-8sL%d", primaryname (instruction), ((alloc_strategy & ALLOC_NOOPERANDS) == 0) ? label : 0);
		}
		if (etc_output) {
			etc_linst (instruction, label);
		}
		if (instruction == I_J)
			markdeadcode ();
	}
}

/*}}}*/
/*{{{  PUBLIC void gencondbranch (jumpcondop, label)         called from GEN7t*/
/***************************************************************************
 *
 *  gencondbranch adds a record (representing a conditional branch with
 *            operand the offset between the next instruction and the
 *            label 'label') to the code buffer.
 *  equivalent to genbranch (I_CJ, label) in the absence of support for
 *            more than one conditional branch test
 *  in the general case jumpcondop may be S_EQ, S_NE, S_GR or S_LE
 *
 **************************************************************************/
PUBLIC void gencondbranch (const int jumpcondop, const int label)
{
	assert (jumpcondop == S_EQ);
	genbranch (I_CJ, label);
}

/*}}}*/
/*{{{  PUBLIC void genjump                                   called from GEN1,10*/
PUBLIC void genjump (const int label)
/* This is used to insert a jump. If code style flag is set,
   creates a non-timeslicable jump using ldc 0; cj
*/
{
	if ((code_style_flags & CODE_STYLE_CJ_NOT_J) != 0) {
		genprimary (I_LDC, 0);
		genbranch (I_CJ, label);
	} else {
		genbranch (I_J, label);
	}
}

/*}}}*/
/*{{{  PUBLIC void genlabeldiff (instruction, label1, label2)       from GEN1,9,10,11*/
/***************************************************************************
 *
 *  genlabeldiff (representing a primary 'instruction' with
 *               operand the offset between 'label1' and 'label2')
 *
 **************************************************************************/
PUBLIC void genlabeldiff (const int instruction, const int label1, const int label2)
{
	if (!dead) {
		if (instruction == I_ADC) {
			dirty_error_flag (FALSE);
		}
		note_tstack ();
		/* label_diff(instruction, label1, label2); */
		if (assembly_output) {
			const BOOL labels = ((alloc_strategy & ALLOC_NOOPERANDS) == 0);
			diag_write_nl ();
			if (showtstack)
				fprintf (outfile, "%3s ", tstack);
			fprintf (outfile, "%-8sL%d - L%d", primaryname (instruction), labels ? label1 : 0, labels ? label2 : 0);
		}
		if (etc_output) {
			etc_llinst (instruction, label1, label2);
		}

		/*{{{  adjust ts_depth */
		{
			int diff = tsdiff_prim[instruction >> 4];
			if (diff >= 16) {
				ts_depth = diff - 16;
			} else {
				ts_depth += diff;
			}
			check_tsdepth ();
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void genloadnamedlabptr (const char *name)*/
/*
 *	loads a named label pointer
 *	(needed for mobile process initialisation)
 */
PUBLIC void genloadnamedlabptr (const char *name)
{
	if (etc_output) {
		/*{{{  adjust stack depth*/
		ts_depth++;
		check_tsdepth ();
		/*}}}*/
		etc_oinst (LOADLABELNAME, name);
	} else if (assembly_output) {
		fprintf (outfile, "\n.loadlabeladdress \"%s\"", name);
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void genloadlabptr (thatlab, thislab)          called from GEN1t*/
PUBLIC void genloadlabptr (const int thatlab, const int thislab, const char *text)
{
	if (etc_output) {
		/*{{{  adjust ts_depth */
		ts_depth++;
		check_tsdepth ();
		/*}}} */
		etc_llinst (I_LDC, thatlab, NOLAB);
	} else {
		genlabeldiff (I_LDC, thatlab, thislab);
		gensecondary (I_LDPI);
	}
	if (text != NULL) {
		gencomment0 (text);
	}
	if (!etc_output) {
		setlab (thislab);
	}
	return;
}

/*}}}*/
/*{{{  PRIVATE void gensettsdepth (regresults)*/
PRIVATE void gensettsdepth (const int regresults)
/* called after a function instance which leaves results on tstack */
{
	if (assembly_output && ((alloc_strategy & ALLOC_NOCOMMENTS) == 0)) {
		fprintf (outfile, "\n-- stackdepth =  %d", regresults);
	} else if (etc_output) {
		gencomment1 ("stackdepth = %d", regresults);
	}
	ts_depth += regresults;
}

/*}}}*/
/*{{{  PUBLIC void gen_func_results (results)                called from GEN4t,11t*/
PUBLIC void gen_func_results (int results)
{
	if (assembly_optimise) {
		diag_write_nl_comment ();
		fprintf (outfile, "... genfuncresults (%d)", results);
	} else if (etc_output) {
		gencomment1 ("genfuncresults (%d)", results);
		etc_cinst (FUNCRESULTS, results);
	}
	gensettsdepth (results);
	/* assume they are already in simulated tstack registers */
}

/*}}}*/
/*{{{  PUBLIC void genprocentry (const int tag, int ws)      called from GEN1*/
PUBLIC void genprocentry (const int tag, int ws)
{
	if (tag == S_MPROCDECL) {
		/* on proc entry (via GCALL), have Wptr[0] holding the return-address */
		genprimary (I_LDL, ws);
		loadmpp ();
		genprimary (I_STNL, MPP_AIPTR);
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void genprocreturn (tag, elocn, regs, ws)    called from GEN1t*/
PUBLIC void genprocreturn (const int tag, SOURCEPOSN elocn, const int regs, const int ws)
{
	int mpplab = newlab ();

	dead = FALSE;		/* must generate even if preceding code is dead */
	/* this is to facilitate sensible disassembly only! */
	if ((tag == S_PROCDEF) || (tag == S_MPROCDECL)) {
		/*{{{  pre-comment */
		if (assembly_optimise) {
			topcrease ();
			fprintf (outfile, "genprocreturn (%d, %d)", regs, ws);
		}
		/*}}} */
	} else {
		/*{{{  pre-comment */
		if (assembly_optimise) {
			topcrease ();
			fprintf (outfile, "genfuncreturn (%d, %d)", regs, ws);
		}
		/*}}} */
		if (etc_output) {
			etc_cinst (FUNCRETURN, regs);
		}
		/* check_tsdepth_ge (regs); */
		if (diagnostics) {
			gencomment1 (">FUNCTION return with %d reg results", regs);
		}
		/* here do anything required to save register results */
	}
	if (tag == S_MPROCDECL) {
		/* generate GAJW/GCALL to get back */
		loadmpp ();
		gensecondary (I_DUP);
		gensecondary (I_DUP);
		genprimary (I_LDNL, MPP_WPTR);
		gensecondary (I_GAJW);
		/* our current Wptr left in Areg, Breg/Creg has MPP */
		gensecondary (I_REV);
		genprimary (I_STNL, MPP_WPTR);		/* save current Wptr */
		/* back in activating workspace now */
		genprimary (I_STL, MPA_SETUP_TEMP_VS);
		if (mpp_check_at_act) {
			/* nullify re-instance IPtr, caller checks */
			gensecondary (I_NULL);
		} else {
			genloadlabptr (mpplab, NOLAB, "PROCESS PTR");
		}
		genprimary (I_LDL, MPA_SETUP_TEMP_VS);
		genprimary (I_STNL, MPP_IPTR);		/* set our next invocation to "stop" process */
		genprimary (I_LDL, MPA_SETUP_TEMP_VS);
		genprimary (I_LDNL, MPP_AIPTR);
		gensecondary (I_GCALL);
		undefine_tstack ();

		if (!mpp_check_at_act) {
			/* dead re-activation code */
			setlab (mpplab);
			etc_cinst (LINENUM, FileLineOf (elocn));
			gensecondary (I_SETERR);
		}

	} else {
		genprimary (I_AJW, ws);	/* maybe include adjustment implicit in RET */
		gensecondary (I_RET);
	}
	/*{{{  post-comment */
	if (assembly_optimise) {
		bottomcrease ();
	}
	/*}}} */
	undefine_tstack ();
}

/*}}}*/
/*{{{  PUBLIC void notefpresult (type) MDP                     from GEN12*/
PUBLIC void notefpresult (const char *type)
/* called after a function instance which leaves results on fpstack */
{
	if (assembly_output && ((alloc_strategy & ALLOC_NOCOMMENTS) == 0)) {
		fprintf (outfile, "\n-- fpstack =  %s", type);
	}
	if (etc_output) {
		etc_cinst (REALRESULT, *type == 's' ? 1 : 2);
	}
}

/*}}}*/
/*{{{  PUBLIC void gensecondary (instruction)          called from GEN**/
/***************************************************************************
 *
 *  gensecondary adds the secondary 'instruction' to the code buffer.
 *
 **************************************************************************/
PUBLIC void gensecondary (int instruction)
{
	if (!dead) {
		/*{{{  check whether instruction may set error */
		switch (instruction) {
		default:
			break;
		case I_CSUB0:
		case I_CCNT1:
		case I_SETERR:
		case I_STOPERR:
		case I_ADD:
		case I_SUB:
		case I_MUL:
		case I_DIV:
		case I_REM:
		case I_CWORD:
		case I_CSNGL:
		case I_LADD:
		case I_LSUB:
		case I_LDIV:
		case I_FMUL:
		case I_CFLERR:
		case I_FPCHKERR:
		case I_LDCNT:	/* variableio 11/3/91 */
		case I_CB:
		case I_CBU:
		case I_CIR:
		case I_CIRU:
		case I_CS:
		case I_CSU:	/* Added 5/9/91 */
		case I_CAUSEERROR:	/* bug TS/1547 07/04/92 */
			dirty_error_flag (FALSE);
			break;

		case I_FPADD:
		case I_FPSUB:
		case I_FPMUL:
		case I_FPDIV:
		case I_FPREMFIRST:
		case I_FPLDNLADDSN:
		case I_FPLDNLADDDB:
		case I_FPLDNLMULSN:
		case I_FPLDNLMULDB:
		case I_FPGT:
		case I_FPEQ:
		case I_FPINT:
		case I_FPRTOI32:

		case I_FPSQRT:
		case I_FPREM:
		case I_FPRANGE:
		case I_FPMULBY2:
		case I_FPDIVBY2:
		case I_FPEXPINC32:
		case I_FPEXPDEC32:
		case I_FPABS:
/*case I_FPSETERR: *//* bug TS/1547 12/05/92 */
		case I_FPR32TOR64:
		case I_FPR64TOR32:
		case I_FPCHKI32:
		case I_FPCHKI64:
		case I_FPADDDBSN:
			dirty_error_flag (TRUE);
			break;
		}
		/*}}} */
		note_tstack ();
		/* code_secondary(instruction); */
		/*{{{  print out the instruction */
		if (assembly_output) {
			diag_write_nl ();
			if (instruction & I_STEP_BIT) {
				fputs ("step ", outfile);
				instruction &= ~(I_STEP_BIT);
			}
			if (showtstack) {
				fprintf (outfile, "%3s ", tstack);
			}
			fprintf (outfile, "%-8s", secondaryname (instruction));
		}
		if (etc_output) {
			etc_sinst (instruction);
		}
		/*}}} */
		/*{{{  check if we have been timesliced, also for loading FPU values */
		switch (instruction) {
		default:
			break;
		case I_OUTWORD:
		case I_OUTBYTE:
		case I_OUT:
		case I_IN:
		case I_TIN:
		case I_ALTWT:
		case I_TALTWT:
		case I_VIN:
		case I_VOUT:	/* variableio 3/11/91 */
#ifdef MOBILES
		#if 0
		case I_MIN:
		case I_MOUT:
		case I_MIN64:
		case I_MOUT64:
		case I_XMIN:
		case I_XMIN64:
		case I_EXTMOUT:
		case I_EXTMIN:
		case I_EXTMOUTN:
		case I_EXTMINN:
		#endif
		case I_MT_IN:
		case I_MT_OUT:
		case I_MT_XCHG:
		case I_MT_LOCK:
		case I_MT_UNLOCK:
		case I_MT_SYNC:
		case I_MT_XIN:
		case I_MT_XOUT:
		case I_MT_XXCHG:
		case I_EXT_MT_IN:
		case I_EXT_MT_OUT:
#endif	/* MOBILES */
		case I_XIN:
		case I_EXTIN:
		case I_EXTOUT:
#ifdef PROCESS_PRIORITY
		case I_SETPRI:
#endif
		case I_STARTP:
		case I_PROC_START:
		case I_SETAFF:
			genstartblock ();
			break;
			/* We rely on the fact that after an FPU load, there is always
			   a spare register, so an FPUCLRERR is OK.
			   Note that regsfor gives 1 for CONSTEXP, so the loads
			   of zeroes are OK too. */
		case I_FPLDNLSN:
		case I_FPLDNLDB:
		case I_FPLDNLSNI:
		case I_FPLDNLDBI:
		case I_FPLDNLADDSN:
		case I_FPLDNLMULSN:
		case I_FPLDNLADDDB:
		case I_FPLDNLMULDB:
		case I_FPLDZEROSN:
		case I_FPLDZERODB:
		case I_FPI32TOR32:
		case I_FPB32TOR64:
		case I_FPI32TOR64:	/* bug TS/2006 15/12/92 */
			clean_fpu_flag (I_FPUCLRERR);
			break;
			/* NO - we can't blatantly clobber stopp, because we use it in
			   PRIPAR and in RESCHEDULE, so we do this in the STOP code */
		case I_SETERR:	/* added for bug 1057 27/11/90 */
			if (errormode & ERRORMODE_HALT) {
				markdeadcode ();
			}
			break;
		}
		/*}}} */
		/*{{{  adjust ts_depth */
		if ((instruction <= 0xAF) || ((instruction >= 0xE0) && (instruction <= 0xEF)) || (instruction == I_NULL) || ((instruction >= 0x200) && (instruction <= 0x2FF))) {
			int diff = 0;

			if (instruction <= 0xAF) {
				diff = tsdiff_sec[instruction];
			} else if ((instruction >= 0xE0) && (instruction <= 0xEF)) {
				diff = tsdiff_sec_ecodes[instruction - 0xE0];
			} else if (instruction == I_NULL) {
				diff = 1;
			} else if ((instruction >= 0x200) && (instruction <= 0x2FF)) {
				diff = tsdiff_sec_twocodes[instruction - 0x200];
			}
			if (diff >= 16) {
				ts_depth = diff - 16;
			} else {
				ts_depth += diff;
			}
			check_tsdepth ();
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void checkerror_controlled               called from GEN7*/
PUBLIC void checkerror_controlled (const int mode)
/* Checks the error flag, controlled by a supplied 'errormode' flag */
{
	if (diagnostics)
		gencomment0 ("checkerror controlled");
	if ((mode & ERRORMODE_NEED_FPCHKERR) && (fpu_error_status == ERROR_STATUS_DIRTY)) {
		/* This will automagically dirty the other flag */
		gensecondary (I_FPCHKERR);
		fpu_error_status = ERROR_STATUS_CLEAN;
	}
	if ((mode & ERRORMODE_NEED_STOPERR) && (int_error_status == ERROR_STATUS_DIRTY)) {
		gensecondary (I_STOPERR);
		int_error_status = ERROR_STATUS_CLEAN;
	}
}

/*}}}*/
/*{{{  PUBLIC void checkerror                          called from GEN**/
PUBLIC void checkerror (void)
/* Checks the error flag, controlled by the normal 'errormode' flag */
{
	if (!inside_asm && NEED_ERRORS) {
		checkerror_controlled (errormode);
	} else if (!inside_assertion) {
		if (diagnostics) {
			gencomment0 ("check error");
		}
		if (fpu_error_status == ERROR_STATUS_DIRTY) {	/* INSdi02121 */
			fpu_error_status = ERROR_STATUS_CLEAN;
		}
		if (int_error_status == ERROR_STATUS_DIRTY) {	/* INSdi02121 */
			int_error_status = ERROR_STATUS_CLEAN;
		}
	}
}

/*}}}*/
/*{{{  PUBLIC void setlab (label)                      called from GEN**/
/***************************************************************************
 *
 *  setlab adds a record representing the setting of label 'label' to the
 *         code buffer.
 *
 **************************************************************************/
PUBLIC void setlab (const int label)
{
	dead = FALSE;
	if (assembly_output && ((alloc_strategy & ALLOC_NOCOMMENTS) == 0)) {
		fprintf (outfile, "\nL%d:", ((alloc_strategy & ALLOC_NOOPERANDS) == 0) ? label : 0);
		in_comment = FALSE;
	}
	if (etc_output) {
		etc_cinst (SETLAB, label);
	}
}

/*}}}*/
/*{{{  PUBLIC void setsectionlab (label)               called from GEN1*/
/***************************************************************************
 *
 *  setsectionlab adds a record representing the setting of label 'label'
 *                to the code buffer, and marks this label as indicating
 *                the start of the current code section (routine, table...)
 *
 **************************************************************************/
PUBLIC void setsectionlab (const int label)
{
	dead = FALSE;
	if (assembly_output && ((alloc_strategy & ALLOC_NOCOMMENTS) == 0)) {
		fprintf (outfile, "\nL%d:", ((alloc_strategy & ALLOC_NOOPERANDS) == 0) ? label : 0);
		in_comment = FALSE;
		if (diagnostics) {
			gencomment0 ("section label");
		}
	}
	if (etc_output) {
		etc_cinst (SECTIONLAB, label);
	}
}

/*}}}*/
/*{{{  PUBLIC void commentexp (tptr)                   called from GEN4,5,6*/
/***************************************************************************
 *
 *  commentexp writes the expression 'tptr' as a comment to the debugging
 *             file.
 *
 **************************************************************************/
PUBLIC void commentexp (treenode * const tptr)
{
	if (assembly_output) {
		if (TagOf (tptr) != S_CONSTEXP) {
			switch (nodetypeoftag (TagOf (tptr))) {
			case NAMENODE:	/*case HIDDENPARAMNODE: */
				break;
			default:
				diag_write_nl_comment ();
				printexp (outfile, tptr);
				break;
			}
		}
	}
}

/*}}}*/
/*{{{  PRIVATE seginfo_t *add_to_seginfo_list*/
PRIVATE seginfo_t *add_to_seginfo_list (seginfo_t * const list, treenode * const nptr)
{
	seginfo_t *const seg = (seginfo_t *) newvec (sizeof (seginfo_t));
	seg->seg_next = list;
	seg->seg_name = nptr;
	return seg;
}

/*}}}*/
/*{{{  PUBLIC void coder_add_mapfile_entry*/
PUBLIC void coder_add_mapfile_entry (treenode * const nptr)
{
	new_occam_line (nptr, TRUE, FALSE, FALSE);	/* write line number to output */
	/* remember the PROC/FUNCTION name for use in subsequent PROCENT directive */
	procname = (char *) WNameOf (NNameOf (nptr));
}

/*}}}*/
/*{{{  PUBLIC void coder_add_entry                     called from GEN1*/
PUBLIC void coder_add_entry (treenode *const nptr)
{
	if (diagnostics) {
		gencomment0 ("coder add entry");
	}
	if (NLexLevelOf (nptr) == 0) {
		/* only lexlevel 0 routines */
		if (assembly_output) {
			write_asm_desc (outfile, nptr);
		}
		if (etc_output) {
			const wordnode *const nameptr = translate_from_internal (NNameOf (nptr));

			etc_oinst (GLOBNAME, WNameOf (nameptr));
			etc_cinst (SETWS, NPDatasizeOf (nptr));
			etc_cinst (SETVS, NPVSUsageOf (nptr));
#ifdef MOBILES
			etc_msusageinst (NPMSUsageOf (nptr));
#endif
			if (ims_asm_output) {
				write_asm_desc (etcfile, nptr);
			}
		}
		seginfo = add_to_seginfo_list (seginfo, nptr);
	}
	coder_add_mapfile_entry (nptr);	/* all routines */
	return;
}

/*}}}*/
/*{{{  PUBLIC void coder_finish_entry (treenode *const nptr)*/
/*
 *	this is called at the end of a PROC, used by mobile processes
 *	to size-up the code
 */
PUBLIC void coder_finish_entry (treenode *const nptr)
{
#if 0
fprintf (stderr, "code1k: coder_finish_entry: nptr = ");
printtreenl (stderr, 4, nptr);
#endif
	if (((TagOf (nptr) == N_MPROCDECL) || (NPSuspendsOf (nptr))) && (NLexLevelOf (nptr) == 0)) {
		if (etc_output) {
			const wordnode *const nameptr = translate_from_internal (NNameOf (nptr));

			etc_oinst (CODEMAP, WNameOf (nameptr));
		}
	}
	if (NLexLevelOf (nptr) == 0 && etc_output) {
		const wordnode *const nameptr = translate_from_internal (NNameOf (nptr));
	
		etc_oinst (GLOBNAMEEND, WNameOf (nameptr));
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE gentypecomment (type_tptr, type_string)*/
PRIVATE void gentypecomment (treenode * type_tptr, const char *type_string)
{
	treenode *type = type_tptr;
	if (type == NULL)
		gencomment0 (type_string);	/* MDP */
	else {
		type = follow_user_type (type);
		if (TagOf (type) == S_ARRAY)
			type = ARTypeOf (type);
		if (type == NULL)
			gencomment0 (type_string);	/* MDP */
		else
			gencomment0 (tagstring (TagOf (type)));	/* MDP */
	}
}

/*}}}*/
/*{{{  PUBLIC void add_const_block(l, p)               called from GEN1t*/
/***************************************************************************
 *
 *  add_const_block adds l bytes of constants, pointed to by p, to the code
 *         buffer. Additional nulls are added to make a multiple of 4.
 *
 **************************************************************************/
PUBLIC void add_const_block (const int l, const BYTE * const p, treenode * type_tptr, const char *type_string, const int swap)
{
	int bytes_done = 0;
	int bytes_to_do;
	int nulls;
	BYTE cbuff[256];
	BYTE *pb;

#if 0
fprintf (stderr, "add_const_block(): l = %d\n", l);
#endif
	if (bytesperword == 2) {
		bytes_to_do = (l + 1) & (~1);	/* round up to a multiple of 2 */
	} else {
		bytes_to_do = (l + 3) & (~3);	/* round up to a multiple of 4 */
	}
	nulls = bytes_to_do - l;


	if (diagnostics) {
		gencomment1 ("add_const_block (%d bytes)", l);
	}
	while (bytes_done < bytes_to_do) {
		int n = bytes_to_do - bytes_done;
		if (n < 256) {
			int j;
			memcpy (&cbuff[0], &p[bytes_done], n - nulls);
			for (j = 1; j <= nulls; j++)
				cbuff[n - j] = '\0';
			pb = &cbuff[0];
		} else {
			n = 256;
			pb = (BYTE *) & (p[bytes_done]);
		}
		/*{{{  add_code_block (n, pb); */
		{
			int mask = target_bigendian ? bytesinscalar (swap) - 1 : 0;
			int i;
			assemble_databytes (n, pb, swap);
			if (etc_output && ims_asm_output) {
				/*{{{  print the block to outfile in etc form */
				{
					for (i = 0; i < n; i++) {
						if ((i & 7) == 0) {
							fprintf (etcfile, "byte");
						}
						fprintf (etcfile, " #%.2X%c", pb[i ^ mask], (i & 7) == 7 ? '\n' : ',');
					}
				}
				if ((n & 7) != 0) {
					fprintf (etcfile, "\n");
				}
				/*}}} */
			} else if (assembly_output && ((alloc_strategy & ALLOC_NOCOMMENTS) == 0)) {
				/*{{{  print the block to outfile */
				BOOL type_done = FALSE;
				for (i = 0; i < n; i++) {
					if ((i & 0x7) == 0) {
						if (i == 8) {
							gentypecomment (type_tptr, type_string);
							type_done = TRUE;
						}
						diag_write_nl_string (ims_asm_output ? "byte" : ".BYTE");
					}
					/*fprintf (outfile, "#%-3x", pb[i]); */
					if (ims_asm_output) {
						fprintf (outfile, "%s #%.2X", (i & 0x7) == 0 ? "" : ",", pb[i ^ mask]);
					} else {
						fprintf (outfile, " #%.2X", pb[i ^ mask]);
					}
				}
				if (!type_done) {
					gentypecomment (type_tptr, type_string);
				}
				/*}}} */
			}
		}
		/*}}} */
		bytes_done += n;
	}
}

/*}}}*/
/*{{{  PUBLIC void coder_jumptoentrypoint              called from GEN1*/
PUBLIC void coder_jumptoentrypoint (const treenode * const nptr)
{
	const char *extname = WNameOf (translate_from_internal (NNameOf (nptr)));	/* MDP */
	if (assembly_output) {
		if (diagnostics) {
			gencomment0 ("coder jumptoentrypoint");
		}
		diag_write_nl ();
		fprintf (etcfile, "%-8s$%s", "j", WNameOf (translate_from_internal (NNameOf (nptr))));	/* MDP */
	}
	if (etc_output) {
		etc_oinst (JUMPENTRY, extname);
	}
}

/*}}}*/
/*{{{  PUBLIC void coder_mark_alignment                called from GEN1*/
PUBLIC void coder_mark_alignment (void)
{
	if (assembly_output) {
		diag_write_nl ();
		fputs ("align", outfile);
	}
	if (etc_output)
		etc_cinst (ALIGN, needs_quadalign ? 3 : 2);
}

/*}}}*/
/*{{{  PUBLIC int get_padlen()*/
PUBLIC int get_padlen (const int len, const int alignment)
{
	if (alignment > 1)
		return (alignment - (len & (alignment - 1))) & (alignment - 1);
	else
		return 0;
}

/*}}}*/
/*{{{  PRIVATE void subwordalign*/
PRIVATE void subwordalign (const int padlen, const BYTE * const padbytes)
{
	if (padlen > 0) {
		add_const_block (padlen, padbytes, NULL, "padding", S_BYTE);
	}
}

/*}}}*/
/*{{{  PRIVATE void genpadding*/
PRIVATE void genpadding (const int len)
{
	static BYTE padding[4] = { 0, 0, 0, 0 };
	if (len > bytesperword) {
		err_abort ("genpadding");
	}
	subwordalign (len, padding);
}

/*}}}*/
/*{{{  PUBLIC int endofsection_padlen*/
PUBLIC int endofsection_padlen (const int alignment)
/* This returns the number of bytes which have to be added to the previous
   section to make it end on that alignment
*/
{
	return 0;		/* we do not know */
}

/*}}}*/
/*{{{  PUBLIC void alignendofsection()                 called from GEN1*/
/***************************************************************************
 *
 *  alignendofsection ensures that the last byte of the
 *               current code section is word aligned.
 *               It must be called after the code has been put in the
 *               code buffer, but before it is compressed.
 *
 *               In effect, it ensures that the end of the previous section
 *               is on a word boundary.
 *
 **************************************************************************/
PUBLIC void alignendofsection (const int alignment)
{
	if (diagnostics) {
		gencomment0 ("alignendofsection");
	}
	genpadding (endofsection_padlen (alignment));
}

/*}}}*/
/*{{{  PUBLIC void endsectionalign(wlen)              called from GEN1*/
PUBLIC void endsectionalign (const int wlen)
{
/* this is not needed as all instrs are lwords and all const-blocks are rounded up */
}

/*}}}*/
/*{{{  PUBLIC void alignwholemodule()                  called from GEN1*/
PUBLIC void alignwholemodule (void)
/* This is used at the end of the object file to ensure that the whole
   object file is word aligned
*/
{
	coder_mark_alignment ();
}

/*}}}*/
/*{{{  PUBLIC int genconstant(cptr)                    called from GEN1*/
/***************************************************************************
 *
 *  genconstant adds bytes representing the constant tptr to the code
 *              buffer (least significant byte first). Returns the
 *              number of WORDS written to the code.
 *
 **************************************************************************/
PUBLIC int genconstant (treenode * tptr)
{
	BYTE v[MAX_CONSTANT_SIZE];
	BIT32 l, h;
	int i;
	const treenode *ty = gettype (tptr);
	const int w = (int) wordsin ((treenode *) ty);
	int swap = targetintsize;

#if 0
fprintf (stderr, "genconstant: bytesperword = %d, w = %d, tptr = ", bytesperword, w);
printtreenl (stderr, 4, tptr);
#endif
	
	if (swap_r64words & (TagOf (ty) == S_REAL64)) {
		l = HiValOf (tptr);
		h = LoValOf (tptr);
	} else {
		l = LoValOf (tptr);
		h = HiValOf (tptr);
	}

	if (bytesperword == sizeof (BIT32)) {
		/* generating for host word-size */
		for (i = 0; i < bytesperword; i++) {
			v[i] = (BYTE) (l & 0xff);
			l >>= 8;
		}

		for (i = bytesperword; i < (bytesperword * 2); i++) {
			v[i] = (BYTE) (h & 0xff);
			h >>= 8;
		}
	} else {
		/* generating for target-size != host-size */
		/* in practice this is probably going to be generating 16-bit code on a 32-bit platform */
		int bc;
		BIT32 tval;

		for (bc = 0; bc < (w * bytesperword); bc++) {
			if (bc == 0) {
				tval = l;
			} else if (bc == 4) {
				tval = h;
			}

			v[bc] = (BYTE)(tval & 0xff);
			tval >>= 8;
		}
	}

#if 0
fprintf (stderr, "genconstant: v = [");
{int j; for (j=0; j<(w * bytesperword); j++) { fprintf (stderr, "%2.2X ", v[j]); }}
fprintf (stderr, "]\n");
#endif

	if (isreal (TagOf (ty)) && has_fpu_core) {
		swap = TagOf (ty);
	}

	add_const_block (w * bytesperword, v, NULL, tagstring (TagOf (ty)), swap);	/* MDP */

	return (w);
}

/*}}}*/
/*{{{  PUBLIC void genloopend (wsoffset, endlabel, looplabel) called from GEN1t*/
PUBLIC void genloopend (const int wsoffset, const int endlabel, const int looplabel)
{
	/*{{{  pre-comment */
	if (assembly_optimise) {
		topcrease ();
		fprintf (outfile, "genloopend (%d, %d, %d)", wsoffset, endlabel, looplabel);
	}
	/*}}} */
	if (etc_output)
		etc_leinst (wsoffset, endlabel, looplabel);
	if (assembly_output) {
		genprimary (I_LDLP, wsoffset + REPL_BASE);
		genlabeldiff (I_LDC, endlabel, looplabel);	/*      ldc endlabel - looplabel */
		gensecondary (I_LEND);	/*          lend                 */
		setlab (endlabel);	/* endlabel:                     */
	}
	/*{{{  post-comment */
	if (assembly_optimise)
		bottomcrease ();
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void genloopendb (wsoffset, endlabel, looplabel) called from GEN1t*/
PUBLIC void genloopendb (const int wsoffset, const int endlabel, const int looplabel)
{
	/* generates a backwards loop */
	/*{{{  pre-comment */
	if (assembly_optimise) {
		topcrease ();
		fprintf (outfile, "genloopend (%d, %d, %d)", wsoffset, endlabel, looplabel);
	}
	/*}}} */
	if (etc_output)
		etc_lebinst (wsoffset, endlabel, looplabel);
	if (assembly_output) {
		/* don't ever want to execute this */
		gensecondary (I_STOPERR);
	}
	/*{{{  post-comment */
	if (assembly_optimise)
		bottomcrease ();
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void genloopend2 (wsoffset, endlabel, looplabel) called from GEN1t*/
PUBLIC void genloopend3 (const int wsoffset, const int endlabel, const int looplabel)
{
	/*{{{  pre-comment */
	if (assembly_optimise) {
		topcrease ();
		fprintf (outfile, "genloopend3 (%d, %d, %d)", wsoffset, endlabel, looplabel);
	}
	/*}}} */
	if (etc_output) {
		etc_le3inst (wsoffset, endlabel, looplabel);
	}
	if (assembly_output) {
		gensecondary (I_STOPERR);
	}
	/*{{{  post-comment */
	if (assembly_optimise) {
		bottomcrease ();
	}
	/*}}} */
	return;
}

/*}}}*/
/*{{{  PUBLIC void genaltend (altcount, altendlab)     called from GEN10t*/
PUBLIC void genaltend (const int altcount, const int altendlab)
{
	/*{{{  pre-comment */
	if (assembly_optimise) {
		topcrease ();
		fprintf (outfile, "genaltend (%d, %d)", altcount, altendlab);
	}
	/*}}} */
	if (altcount != 0)
		gensecondary (I_ALTEND);
	setlab (altendlab);
	/*{{{  post-comment */
	if (assembly_optimise)
		bottomcrease ();
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC noteprocentry ()                         called from GEN1t*/
PUBLIC void noteprocentry (void)
{
	/*{{{  remark-comment */
	if (assembly_optimise) {
		diag_write_nl ();
		fprintf (outfile, "--... noteprocentry %s", procname);
	}
	/*}}} */
	if (etc_output) {
		etc_oinst (PROCNAME, procname);
	}
}

/*}}}*/
/*{{{  PRIVATE void genloadcasescale ()*/
/***************************************************************************
 *
 *  gencasescale adds a record representing a primary 'instruction' I_LDC
 *               to the code buffer, whose operand is the size
 *               in bytes of a forthcoming jump table.
 *
 **************************************************************************/
PRIVATE void genloadcasescale ()
{
	/*{{{  remark-comment */
	if (assembly_optimise) {
		diag_write_nl ();
		fprintf (outfile, "--... genloadcasescale");
	}
	/*}}} */
	if (!dead) {
		note_tstack ();
		/* scale_ref(I_LDC); */
		if (assembly_output) {
			diag_write_nl ();
			if (showtstack) {
				fprintf (outfile, "%3s ", tstack);
			}
			fprintf (outfile, "ldc     case.scale");
		}
		if (etc_output) {
			etc_pinst (I_LDC, 4);
		}
	}
}

/*}}}*/
/*{{{  PUBLIC void genstartjumptable ()                called from GEN9*/
/***************************************************************************
 *
 *  genstartjumptable adds a record to the code buffer marking the start
 *                    of a jump table.
 *
 **************************************************************************/
PUBLIC void genstartjumptable (void)
{
#if 0
fprintf (stderr, "genstartjumptable()... dead = %d\n", dead);
#endif
	/*{{{  pre-comment */
	if (assembly_optimise) {
		topcrease ();
		fprintf (outfile, "genstartjumptable");
	}
	/*}}} */
	if (!dead) {
		const int jlab = newlab ();
		const int l = newlab ();

		if (etc_output) {
			etc_specop (SPE_STARTTABLE);	/* subsumes whole table jump */
		} else {
			genloadcasescale ();	/*           ldc   casescale       */
			gensecondary (I_PROD);	/*           prod                  */
			/* now Areg will contain offset into table of jumps immediately following */
			genloadlabptr (jlab, l, " PTR table base");
			gensecondary (I_BSUB);	/*           bsub                  */
			gensecondary (I_GCALL);	/*           gcall                 */
			setlab (jlab);	/* jlab:                           */
		}
		/* table_start(); */
	}
	/*{{{  post-comment */
	if (assembly_optimise) {
		bottomcrease ();
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void genjumptableentry (instruction, label)     from GEN9*/
/***************************************************************************
 *
 *  genjumptableentry adds a record representing a jump table entry
 *                    to the code buffer.
 *
 **************************************************************************/
PUBLIC void genjumptableentry (const int instruction, const int label)
{
#if 0
fprintf (stderr, "genjumptableentry(): dead = %d, instruction = %d, label = %d\n", dead, instruction, label);
#endif
	dead = 0;
	genbranch (instruction, label);
	return;
}

/*}}}*/
/*{{{  PUBLIC void genendjumptable ()                  called from GEN9*/
/***************************************************************************
 *
 *  genendjumptable adds a record to the code buffer marking the end
 *                  of a jump table.
 *
 **************************************************************************/
PUBLIC void genendjumptable (void)
{
	/*{{{  remark-comment */
	if (assembly_optimise) {
		diag_write_nl ();
		fprintf (outfile, "--... genendjumptable");
	}
	/*}}} */
	if (!dead) {
		/* table_end(); */
	}
}

/*}}}*/
/*{{{  PUBLIC void genboolinvert ()            called from GEN5t, 7t*/
/*****************************************************************************
 *
 *  genboolinvert generates code to invert a known boolean
 *
 *****************************************************************************/
PUBLIC void genboolinvert (void)
{
	if (etc_output) {
		etc_specop (SPE_BOOLINVERT);
	} else {
		/*{{{  pre-comment */
		if (assembly_optimise) {
			topcrease ();
			fprintf (outfile, "genboolinvert");
		}
		/*}}} */
		genprimary (I_EQC, ZERO32);
		/*{{{  post-comment */
		if (assembly_optimise) {
			bottomcrease ();
		}
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC void genloadconstant*/
/*****************************************************************************
 *
 *  genloadconstant loads the value of the constant c onto the expression stack
 *
 *****************************************************************************/
PUBLIC void genloadconstant (const INT32 c)
{
	/*{{{  pre-comment */
	if (assembly_optimise) {
		topcrease ();
		fprintf (outfile, "genloadconstant (%ld)", (long) c);
	}
	/*}}} */
	if (kroc_flag) {	/* inappropriate for non-transputer target -- DCW */
		genprimary (I_LDC, c);
	} else {
		switch (constformat_of_const (targetintsize, c, 0)) {
		case constformat_mint32_ldnlp:
			gensecondary (I_MINT);
			genprimary (I_LDNLP, (INT32) ((BIT32) c - (BIT32) MOSTNEG_INT32) / 4 /*bytesperword */ );
			break;
		case constformat_mint32_adc:
			gensecondary (I_MINT);
			genprimary (I_ADC, (INT32) ((BIT32) c - (BIT32) MOSTNEG_INT32));
			break;
		case constformat_mint32_not:
			gensecondary (I_MINT);
			gensecondary (I_NOT);
			break;
		case constformat_ldinf_ldnlp:
			gensecondary (I_LDINF);
			genprimary (I_LDNLP, (INT32) ((BIT32) c - (BIT32) INFINITY) / 4 /*bytesperword */ );
			break;
		case constformat_ldinf_adc:
			gensecondary (I_LDINF);
			genprimary (I_ADC, (INT32) ((BIT32) c - (BIT32) INFINITY));
			break;
		case constformat_mint16_ldnlp:
			gensecondary (I_MINT);
			genprimary (I_LDNLP, (INT32) ((BIT32) c - (BIT32) MOSTNEG_INT16) / 2 /*bytesperword */ );
			break;
		case constformat_mint16_adc:
			gensecondary (I_MINT);
			genprimary (I_ADC, (INT32) ((BIT32) c - (BIT32) MOSTNEG_INT16));
			break;
		case constformat_ldc:
		case constformat_table:
			genprimary (I_LDC, c);
			break;
		}
	}
	/*{{{  post-comment */
	if (assembly_optimise) {
		bottomcrease ();
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void genshiftimmediate (op,shiftcount)*/
PUBLIC void genshiftimmediate (const int op, const int shiftcount)
/* generate fast code for ((RegA) << shiftcount) == ((RegA) * 2^shiftcount)
   (where shiftcount is a non-negative constant)
*/
{
	/*{{{  pre-comment */
	if (assembly_optimise) {
		topcrease ();
		fprintf (outfile, "genshiftimmediate %s (%d)", secondaryname (op), shiftcount);
	}
	/*}}} */
	if (shiftcount == 0) {
		/* nothing needed */
	} else if ((op == I_LSHR) || (op == I_LSHL)) {
		/*{{{  long shift */
		/* Here use longshiftimmediate ETC ops when defined! */
		genloadconstant (shiftcount);
		gensecondary (op);
		/*}}} */
	} else {
		/*{{{  short shift */
		if (etc_output) {
			/*{{{  etc output */
			/*{{{  assembly output */
			if (assembly_output) {
				/* should be extended with tstack stuff for completeness */
				diag_write_nl ();
				fprintf (outfile, "%-8s", primaryname (I_LDC));
				if ((alloc_strategy & ALLOC_NOOPERANDS) == 0) {
					fprintf (outfile, "%ld", (long) shiftcount);
				}
				diag_write_nl ();
				fprintf (outfile, "%-8s", secondaryname (op));
			}
			/*}}} */
			if (shiftcount == 32) {	/* turn anything into zero */
				etc_sinst (I_POP);
				etc_pinst (I_LDC, 0);
			} else if (op == I_SHR) {
				/*{{{  rightshift */
				etc_cinst (SRLIMM, shiftcount);
				/*}}} */
			} else {
				/*{{{  leftshift = mul by power of 2 */
				etc_cinst (SLLIMM, shiftcount);
				/*}}} */
			}
			/*}}} */
		} else {
			/*{{{  transputer output */
			if (op == I_SHR) {
				/*{{{  rightshift */
				genloadconstant (shiftcount);
				gensecondary (I_SHR);
				/*}}} */
			} else if (kroc_flag) {	/* inappropriate for non-transputer target -- DCW */
				/*{{{  leftshift */
				genloadconstant (shiftcount);
				gensecondary (I_SHL);
				/*}}} */
			} else {
				/*{{{  leftshift = mul by power of 2 (with several optimisations) */
				/* this code takes advantage of special transputer instructions */
				const INT32 bcnts = lshiftcanusebcnt (shiftcount);

				if (bcnts > 0) {
					/*{{{  we can use bcnt */
					int i;
					for (i = 0; i < bcnts; i++) {
						gensecondary (I_BCNT);
					}
					/*}}} */
				} else if ((((code_style_flags & CODE_STYLE_SPACE_NOT_TIME) != 0) || bytesperword == 2) && (shiftcount <= 3)) {
					/* it is smaller to use prod, and code size mattters on 16-bit */
					genloadconstant ((INT32) 1 << shiftcount);
					gensecondary (I_PROD);
				} else if (has_dup && !T9000_instruction_timings && (shiftcount == 1)) {	/* bug 1168 22/08/91 */
					gensecondary (I_DUP);
					gensecondary (I_BSUB);
				} else if (has_wsubdb && (shiftcount == 3)) {
					/* we've already checked that bytesperword != 2 when shiftcount == 3 */

					genloadconstant (0);
					gensecondary (I_WSUBDB);	/* bug TS/1168 16/01/92 */
				} else {
					/* Bug 1168 - 14/3/91 - use a shift cos its quicker */
					/* This 'loadconstant' is a small +ve integer, so will not need
					   a constant table
					 */
					genloadconstant (shiftcount);
					gensecondary (I_SHL);
				}
				/*}}} */
			}
			/*}}} */
		}
		/*}}} */
	}
	/*{{{  post-comment */
	if (assembly_optimise) {
		bottomcrease ();
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void gennotprocess ()            called from GEN1t*/
/*****************************************************************************
 *
 *  gennotprocess generates ldc,NOT_PROCESS for chan initialisation etc
 *  (generates 0 and not MINT for ETC)
 *
 *****************************************************************************/
PUBLIC void gennotprocess (void)
{
	int NOT_PROCESS = targetintsize == S_INT16 ? MOSTNEG_INT16 : MOSTNEG_INT32;
	/*{{{  remark-comment */
	if (assembly_optimise) {
		diag_write_nl ();
		fprintf (outfile, "--... gennotprocess");
	}
	/*}}} */
	if (etc_output) {
		note_tstack ();
		etc_specop (SPE_NOTPROCESS);
		ts_depth++;
	} else {
		genloadconstant (NOT_PROCESS);
	}
}

/*}}}*/
/*{{{  PUBLIC void genfppop ()            called from GEN8t*/
/*****************************************************************************
 *
 *  genfppop is called when a value can usefully be popped off top of FP stack
 *
 *****************************************************************************/
PUBLIC void genfppop (void)
{
	if (etc_output) {
		etc_specop (SPE_FPPOP);
	}
}

/*}}}*/
/*{{{  PUBLIC void genwidenshort ()            called from GEN4t*/
/*****************************************************************************
 *
 *  genwidenshort generates code to widen from INT16 to INT32
 *
 *****************************************************************************/
PUBLIC void genwidenshort (void)
{
	/*{{{  assembly output */
	if (assembly_output) {
		/* should be extended with tstack stuff for completeness */
		diag_write_nl ();
		fprintf (outfile, "%-8s", primaryname (I_LDC));
		if ((alloc_strategy & ALLOC_NOOPERANDS) == 0) {
			fprintf (outfile, "%ld", (long) INT16_CHECK_MASK);
		}
		gencomment0 ("widenshort");
		diag_write_nl ();
		fprintf (outfile, "%-8s", secondaryname (I_XWORD));
	}
	/*}}} */
	if (etc_output) {
		etc_specop (SPE_WIDENSHORT);
	}
}

/*}}}*/
/*{{{  PUBLIC void gencompare (relop,flags)    called from GEN4t*/
PUBLIC void gencompare (const int relop, const BOOL flags)
/* generates code to compare Breg, Areg
   if flags is TRUE result could be left in condition flags, else Areg */
{
	const char *opname;
	switch (relop) {
	case S_GR:
		opname = "gt";
		break;
	case S_LE:
		opname = "le";
		break;
	case S_NE:
		opname = "ne";
		break;
	default:
	case S_EQ:
		opname = "eq";
		break;
	}
	/*{{{  pre-comment */
	if (assembly_optimise) {
		topcrease ();
		fprintf (outfile, "gencompare (%s,%s)", opname, flags ? "condflags" : "Areg");
	}
	/*}}} */
	switch (relop) {
	case S_GR:
		gensecondary (I_GT);
		break;
	case S_LE:
		gensecondary (I_GT);
		genboolinvert ();
		break;
	case S_EQ:
		gensecondary (I_DIFF);
		genprimary (I_EQC, 0);
		break;
	case S_NE:
		gensecondary (I_DIFF);
		genprimary (I_EQC, 0);
		genboolinvert ();
		break;
	}
	/*{{{  post-comment */
	if (assembly_optimise)
		bottomcrease ();
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void gen_init_chanarray*/
PUBLIC void gen_init_chanarray (const int pstride, const int cstride, const int elems)
/* generate a loop to initialise arrays of pointers and channels */
/* pointer to array has already been loaded into tstack */
{
	const BIT32 base = 0;	/* Use bottom two words of workspace for */
	const BIT32 count = 1;	/* base and count */
	int looplab;
	/*{{{  pre-comment */
	if (assembly_optimise) {
		topcrease ();
		fprintf (outfile, "geninitchanarray (%d, %d, %d)", pstride, cstride, elems);
	}
	/*}}} */
	genprimary (I_STL, base);	/*          stl  base      */
	gencomment0 ("PTR base");
	genprimary (I_LDC, elems);	/*          ldc  elements  */
	gencomment0 ("elements");
	genprimary (I_STL, count);	/*          stl  count     */
	gencomment0 ("INT count");
	looplab = newlab ();
	setlab (looplab);	/* looplab:                */
	genprimary (I_LDL, count);	/*          ldl   count     */
	gencomment0 ("INT count");
	genprimary (I_ADC, -1);	/*          adc   -1        */
	genprimary (I_STL, count);	/*          stl   count     */
	gencomment0 ("INT count");
	if (chanaspointer) {
		genprimary (I_LDL, base);	/*          ldl   base      */
		gencomment0 ("PTR base");
		genprimary (I_LDNLP, elems * pstride);	/*      ldnlp elements */
		gencomment0 ("elements");
		genprimary (I_LDL, base);	/*          ldl   base      */
		gencomment0 ("PTR base");
		genprimary (I_STNL, 0);	/*          stnl  0         */
		gencomment0 ("init channel pointer");
	}
	gennotprocess ();
	genprimary (I_LDL, base);	/*          ldl   base      */
	gencomment0 ("PTR base");
	genprimary (I_STNL, chanaspointer ? elems : 0);	/*     stnl  e         */
	gencomment0 ("init channel word");
	genprimary (I_LDL, base);	/*          ldl   base      */
	gencomment0 ("PTR base");
	genprimary (I_LDNLP, 1);	/*          ldnlp 1        */
	genprimary (I_STL, base);	/*          stl   base      */
	gencomment0 ("PTR base");
	genprimary (I_LDL, count);	/*          ldl   count     */
	gencomment0 ("INT count");
	genprimary (I_EQC, ZERO32);	/*          eqc   0         */
	genbranch (I_CJ, looplab);	/*          cj    looplab   */
	/*{{{  post-comment */
	if (assembly_optimise)
		bottomcrease ();
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC geni64tor*/
PUBLIC void geni64tor (const int sourcemode, treenode * const source, const int desttype, const int roundmode, const int simple)
{
	loadopd (sourcemode, source, 0);	/*  ldp source   */
	if (etc_output && has_i64tor) {
		if (desttype == S_REAL32) {
			gencomment0 ("geni64tor32");
			etc_specop (SPE_I64TOREAL);	/*  load int64 to FregA as REAL32 */
			etc_cinst (REALRESULT, 1);
		} else {
			gencomment0 ("geni64tor64");
			etc_specop (SPE_I64TOREAL);	/*  load int64 to FregA as REAL64 */
			etc_cinst (REALRESULT, 2);
		}
		ts_depth--;				/* takes register of top-of-stack */
	} else {
		if (!simple) {
			gensecondary (I_DUP);	/*  dup          */
		}
		gensecondary (I_FPB32TOR64);	/*  fpb32tor64   */
		if (desttype == S_REAL32 && !has_directfp) {
			genfpuentry (I_FPUNOROUND);	/*  fpunoround   */
		}
		if (simple) {
			loadopd (sourcemode, source, 1);	/*  ldp source+1 */
		} else {
			genprimary (I_LDNLP, 1);	/*  ldnlp  1     */
		}
		gensecondary (I_FPI32TOR64);	/*  fpi32or64    */
		if (has_directfp) {
			gensecondary (I_FPEXPINC32);	/*  fpexpinc32   */
		} else {
			genfpuentry (I_FPUEXPINC32);
		}
		if (desttype == S_REAL32 && !has_directfp) {
			genfpuentry (I_FPUNOROUND);	/*  fpunoround   */
		}
		if (roundmode == S_TRUNC) {
			if (has_directfp) {
				gensecondary (I_FPRZ);	/*  fprz         */
			} else {
				genfpuentry (I_FPURZ);
			}
		}
		if (desttype == S_REAL32 && has_directfp) {
			gensecondary (I_FPADDDBSN);	/*  fpadddbsn    */
		} else {
			gensecondary (I_FPADD);	/*  fpadd        */
		}
	}
}

/*}}}*/
/*{{{  PUBLIC void gentstop (address)*/
PUBLIC void gentstop (treenode * const address)
{
	new_occam_line (address, TRUE, TRUE, TRUE);
	/* if (debugoutput)
	   coder_genlocate(address, TRUE); */
	if (errormode & ERRORMODE_STOP_IS_SETERR) {
		if (H_series) {
			genloadconstant (ERR_INTERR);
			gensecondary (I_CAUSEERROR);
		} else {
			gensecondary (I_SETERR);
		}
	} else if (errormode & ERRORMODE_STOP_IS_STOPP) {
		gensecondary (I_STOPP);
	} else if (errormode & ERRORMODE_STOP) {
		gensecondary (I_STOPERR);
	}
	markdeadcode ();
}

/*}}}*/
/*{{{  PUBLIC BOOL genreplicatorcheck (must_check, poslab, zerolab)*/
PUBLIC BOOL genreplicatorcheck (const BOOL must_check, const int poslab, const int zerolab)
/* generates code to jump to poslab if Areg is positive, zerolab if it is
   zero, and (if must_check is true) to stop if it is negative.
   returns TRUE iff an explicit jump to bodylab has been generated
   (some implementations (eg 386) might do this if must_check was TRUE)
   (note that one 386 version had loop incrementing etc at head of loop) */
/* This simplified version assumes poslab = bodylab = next instruction */
{
	/*{{{  pre-comment */
	if (assembly_optimise) {
		topcrease ();
		fprintf (outfile, "genreplicator%s (%d, %d)", must_check ? "check" : "skip", poslab, zerolab);
	}
	/*}}} */
	if (must_check) {
		/*{{{  check the replicator (in Areg) is > 0 */
		gensecondary (I_MINT);	/*          mint ( = mostpos PLUS 1) */
		gensecondary (I_CSUB0);	/*          csub0                */
		gencomment0 ("error if negative");
		checkerror ();
		/*}}} */
	}
	genbranch (I_CJ, zerolab);	/*          cj    zerolab         */
	/*{{{  post-comment */
	if (assembly_optimise) {
		bottomcrease ();
	}
	/*}}} */
	return FALSE;
}

/*}}}*/
/*{{{  PUBLIC void genloopheadtop (wsoffset)      called from GEN1t*/
PUBLIC void genloopheadtop (const int wsoffset)
{
	/*{{{  pre-comment */
	if (assembly_optimise) {
		topcrease ();
		fprintf (outfile, "genloophead (%d)", wsoffset);
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void genloopheadbottom ()           called from GEN1t*/
PUBLIC void genloopheadbottom ()
{
	/*{{{  post-comment */
	if (assembly_optimise) {
		bottomcrease ();
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void gengcall (const BOOL savereturn)*/
void gengcall (const BOOL savedreturn)
{
	gensecondary (I_GCALL);
}

/*}}}*/
/*{{{  PUBLIC void genchecknotnull (void)*/
/*
 *	genchecknotnull (): checks that Areg is not NULL (before de-referencing it..)
 */
PUBLIC void genchecknotnull (void)
{
	if (dead) {
		return;
	}
	if (etc_output) {
		etc_specop (SPE_CHECKNOTNULL);
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void gensemop (int op)*/
PUBLIC void gensemop (int op)
{
	if (dead) {
		return;
	}
	switch (op) {
	case SEMOP_CLAIM:
	case SEMOP_RELEASE:
	case SEMOP_INIT:
		if (etc_output) {
			note_tstack ();
			switch (op) {
			case SEMOP_CLAIM:	etc_specop (SPE_SEMCLAIM);
						break;
			case SEMOP_RELEASE:	etc_specop (SPE_SEMRELEASE);
						break;
			case SEMOP_INIT:	etc_specop (SPE_SEMINIT);
						break;
			}
			ts_depth--;
		} else {
			geninternal_s (GEN_INTERNAL_ERROR, "semaphore operation outside of ETC");
		}
		break;
	case SEMOP_ISWAITING:
		geninternal_s (GEN_INTERNAL_ERROR, "semaphore operation not implemented");
		break;
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void genreschedule (void)*/
PUBLIC void genreschedule (void)
{
	if (dead) {
		return;
	}
	if (etc_output) {
		note_tstack ();
		etc_specop (SPE_RESCHEDULE);
	} else {
		geninternal_s (GEN_INTERNAL_ERROR, "reschedule operation not implemented");
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void genindirect (int reg)*/
/*
 *	generates an indirection, used by mobile processes.
 *	reg is INDIR_[ABC]REG
 */
PUBLIC void genindirect (int reg)
{
	if (dead) {
		return;
	}
	switch (reg) {
	case INDIR_AREG:
		etc_specop (SPE_INDIRECT_AREG);
		break;
	case INDIR_BREG:
		etc_specop (SPE_INDIRECT_BREG);
		break;
	case INDIR_CREG:
		etc_specop (SPE_INDIRECT_CREG);
		break;
	default:
		geninternal_s (GEN_INTERNAL_ERROR, "genindirect: invalid register");
		break;
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void genloadwsmap (int mpp_offset, int maplab)*/
/*
 *	generates a "load workspace-map" instruction.  mobile processes
 *	get an offset for the MPP, others that are mapped have NO_SLOT
 */
PUBLIC void genloadwsmap (int mpp_offset, int maplab)
{
	BETRACE ("code1: genloadwsmap (enter): mpp_offset=%d, maplab=%d", mpp_offset, maplab);
	if (!dead) {
		add_code (0x6d);
		add_code (0xf7);
		assemble_instruction (I_LDC, mpp_offset);
		assemble_instruction (I_LDC, maplab);
	}
	BETRACE ("code1: genloadwsmap (leave)");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genunloadwsmap (int mpp_offset, int maplab)*/
/*
 *	generates an "unload workspace-map" instruction.  mobile processes
 *	get an offset for the MPP, others get NO_SLOT
 */
PUBLIC void genunloadwsmap (int mpp_offset, int maplab)
{
	BETRACE ("code1: genunloadwsmap (enter): mpp_offset=%d, maplab=%d", mpp_offset, maplab);
	if (!dead) {
		add_code (0x6d);
		add_code (0xf8);
		assemble_instruction (I_LDC, mpp_offset);
		assemble_instruction (I_LDC, maplab);
	}
	BETRACE ("code1: genunloadwsmap (leave)");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genrmwsmap (void)*/
/*
 *	generates a "remove workspace-map" instruction.  mobile processes
 *	get an offset for the MPP, others get NO_SLOT.  MPP is passed on the stack.
 */
PUBLIC void genrmwsmap (void)
{
	BETRACE ("code1: genrmwsmap (enter)");
	if (!dead) {
		etc_specop (SPE_RMWSMAP);
		ts_depth--;
	}
	BETRACE ("code1: genrmwsmap (leave)");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genmppclone (void)*/
/*
 *	generates a "clone mobile process" instruction.  MPP is passed on the stack
 *	and a new MPP is returned on the stack
 */
PUBLIC void genmppclone (void)
{
	BETRACE ("code1: genmppclone (enter)");
	if (!dead) {
		etc_specop (SPE_MPPCLONE);
	}
	BETRACE ("code1: genmppclone (leave)");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genmppserialise (void)*/
/*
 *	generates a "serialise mobile process" instruction.
 *	MPP is passed on the stack along with a pointer to an address and size vars where we park a MOBILE []BYTE
 *	(with the serialised process in)
 */
PUBLIC void genmppserialise (void)
{
	BETRACE ("code1: genmppserialise (enter)");
	if (!dead) {
		etc_specop (SPE_MPPSERIALISE);
		ts_depth -= 3;
	}
	BETRACE ("code1: genmppserialise (leave)");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genmppdeserialise (void)*/
/*
 *	generates a "de-serialise mobile process" instruction.
 *	Three pointers are passed on the stack: one to the MPP slot and one each for the
 *	addr/size of the MOBILE []BYTE array that we are restoring from
 */
PUBLIC void genmppdeserialise (void)
{
	BETRACE ("code1: genmppdeserialise (enter)");
	if (!dead) {
		etc_specop (SPE_MPPDESERIALISE);
		ts_depth -= 3;
	}
	BETRACE ("code1: genmppdeserialise (leave)");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genloadcodemap (treenode *const nptr)*/
/*
 *	loads a pointer to the code-map of a mobile process.
 *	this is done by outputting a LOADCODEMAP special followed by an "LDC <label>".
 *	the label is the entry-point of the routine, _not_ that of the code-map (generated later)
 */
PUBLIC void genloadcodemap (treenode *const nptr)
{
	BETRACE ("code1: genloadcodemap (enter)");
	if (!dead) {
		if (NPLabelOf (nptr)) {
			etc_specop (SPE_LOADCODEMAP);
			genprimary (I_LDC, NPLabelOf (nptr));
		} else {
			/* load by name */
			/*{{{  adjust stack depth*/
			ts_depth++;
			check_tsdepth ();
			/*}}}*/
			etc_oinst (LOADCODEMAPNAME, WNameOf (NNameOf (nptr)));
		}
	}
	BETRACE ("code1: genloadcodemap (leave)");
	return;
}
/*}}}*/
#if 0
/*{{{  PUBLIC void genfbarinit (void)*/
/*
 *	generates an "initialise full barrier" instruction
 *	address of the barrier is passed on the stack
 */
PUBLIC void genfbarinit (void)
{
	BETRACE ("code1: genfbarinit (enter)");
	if (!dead) {
		etc_specop (SPE_FBARINIT);
		ts_depth--;
	}
	BETRACE ("code1: genfbarinit (leave)");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genfbarsync (void)*/
/*
 *	generates a "full barrier sync" instruction
 *	address of the barrier is passed on the stack
 */
PUBLIC void genfbarsync (void)
{
	BETRACE ("code1: genfbarsync (enter)");
	if (!dead) {
		etc_specop (SPE_FBARSYNC);
		ts_depth--;
	}
	BETRACE ("code1: genfbarsync (leave)");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genfbarresign (void)*/
/*
 *	generates a "resign from barrier" instruction
 *	address of the barrier and number of processes to
 *	resign are passed on the stack
 */
PUBLIC void genfbarresign (void)
{
	BETRACE ("code1: genfbarresign (enter)");
	if (!dead) {
		etc_specop (SPE_FBARRESIGN);
		ts_depth -= 2;
	}
	BETRACE ("code1: genfbarresign (leave)");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genfbarenroll (void)*/
/*
 *	generates an "enroll on barrier" instruction
 *	address of the barrier and number of processes to
 *	enroll are passed on the stack (already adjusted for inner PARs, etc.)
 */
PUBLIC void genfbarenroll (void)
{
	BETRACE ("code1: genfbarenroll (enter)");
	if (!dead) {
		etc_specop (SPE_FBARENROLL);
		ts_depth -= 2;
	}
	BETRACE ("code1: genfbarenroll (leave)");
	return;
}
/*}}}*/
#endif
/*{{{  PUBLIC void gendtrace (void)*/
/*
 *	generates a debugging traces special
 *	expects Areg to contain trace identifier, Breg to contain address/value
 */
PUBLIC void gendtrace (void)
{
	BETRACE ("code1: gendtrace (enter)");
	if (!dead && enable_dtraces) {
		etc_specop (SPE_DTRACE);
		ts_depth -= 2;
	}
	BETRACE ("code1: gendtrace (leave)");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genkillcall (void)*/
/*
 *	generates a killcall special
 *	expects Areg to contain killchan, 
 *	returns in Areg the result
 */
PUBLIC void genkillcall (void)
{
	BETRACE ("code1: genkillcall (enter)");
	if (!dead) {
		etc_specop (SPE_KILLCALL);
	}
	BETRACE ("code1: genkillcall (leave)");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genwaitint (void)*/
/*
 *	generates a "wait for interrupt" special
 *	expects Areg to contain interrupt number,
 *	Breg to contain processor mask,
 *	returns in Areg the result
 */
PUBLIC void genwaitint (void)
{
	BETRACE ("code1: genwaitint (enter)");
	if (!dead) {
		etc_specop (SPE_WAIT_FOR_INTERRUPT);
		ts_depth--;
	}
	BETRACE ("code1: genwaitint (leave)");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genmobiletypedescription (treenode *const mtype)*/
/*
 *	generates a type-map for some MOBILE type
 */
PUBLIC void genmobiletypedescription (treenode *const mtype)
{
	unsigned int mtypedescbuf[1024];		/* a 4k buffer is hopefully enough..! */
	int mtypelen = 0;

	BETRACE ("code1: genmobiletypedescription (enter)");
	mobile_typedescfixup_start (MTDLabOf (mtype));
	mobile_gettypedesc (mtype, mtypedescbuf, &mtypelen, 1000);		/* bit less.. */

#if 0
fprintf (stderr, "code1k: genmobiletypedescription: coding at label %d.  mtype =", MTDLabOf (mtype));
printtreenl (stderr, 4, mtype);
#endif
	mobile_typedescfixup_finish (MTDLabOf (mtype), gencomment0);
	setlab (MTDLabOf (mtype));
	add_const_block (mtypelen * bytesperword, (unsigned char *)mtypedescbuf, NULL, "mtdesc", S_BYTE);

	BETRACE ("code1: genmobiletypedescription (leave)");
	return;
}
/*}}}*/
/*{{{  PUBLIC void genextfpop (int op)*/
/*
 *	generates an extended floating-point instruction
 */
PUBLIC void genextfpop (int op)
{
	BETRACE ("code1: genextfpop (enter)");
	if (!dead) {
		switch (op) {
		case I_REAL32SIN:
			etc_specop (SPE_R32SIN);
			break;
		case I_REAL64SIN:
			etc_specop (SPE_R64SIN);
			break;
		case I_REAL32COS:
			etc_specop (SPE_R32COS);
			break;
		case I_REAL64COS:
			etc_specop (SPE_R64COS);
			break;
		case I_REAL32TAN:
			etc_specop (SPE_R32TAN);
			break;
		case I_REAL64TAN:
			etc_specop (SPE_R64TAN);
			break;
		}
	}
	BETRACE ("code1: genextfpop (leave)");
	return;
}
/*}}}*/
/*}}}*/
/*{{{  PUBLIC void throw_the_result_away ()*/
PUBLIC void throw_the_result_away (void)
{
	int old_ts_depth = ts_depth;
	new_occam_line (NULL, FALSE, FALSE, TRUE);	/* may generate a TSDEPTH etc instr */
	if (old_ts_depth > 0) {
		gencomment0 ("throw the result away (stackdepth = 0)");
	} else {
		gencomment0 ("tstack empty (stackdepth = 0)");
	}
}

/*}}}*/
/*{{{  PUBLIC void throw_nested_result_away ()*/
PUBLIC void throw_nested_result_away (void)
{
	gencomment0 ("throw nested result away (stackdepth -= 1)");
	ts_depth--;
	if (etc_output) {
		etc_cinst (TSDEPTH, ts_depth);
	}
}

/*}}}*/
/*{{{  PUBLIC void gencontrolsplit (label)*/
PUBLIC void gencontrolsplit (const int label)
{
	/* placeholder for possible need to note that tstack must match on two routes */
	if (assembly_optimise) {
		gencomment0 ("control-split");
	}
	if (etc_output) {
		etc_specop (SPE_CONTRSPLIT);
	}
	genbranch (I_CJ, label);	/* maybe merge with CONTRSPLIT ? */
}

/*}}}*/
/*{{{  PUBLIC void gencontroljoin (label)*/
PUBLIC void gencontroljoin (const int label)
{
	/* placeholder for possible need to note that tstack must match on two routes */
	if (assembly_optimise) {
		gencomment0 ("control-join");
	}
	if (etc_output) {
		etc_specop (SPE_CONTRJOIN);
	}
	setlab (label);		/* maybe merge with CONTRJOIN ? */
}

/*}}}*/
/*{{{  library handling*/
/*{{{  PUBLIC void add_to_libentries(nptr)               called from GEN8,10*/
/*****************************************************************************
 *
 *  add_to_libentries adds the library entrypoint 'nptr' to the list of used
 *                    library entry points if it is not aready there.
 *
 *****************************************************************************/
PUBLIC void add_to_libentries (treenode * nptr)
{
	treenode *l = libentrychain;

#if 0
fprintf (stderr, "code1k: add_to_libentries: nptr = ");
printtreenl (stderr, 4, nptr);
#endif
	if (diagnostics) {
		gencomment0 ("add to libentries");
	}
	while (l != NULL) {
		if (l == nptr) {
			return;
		}
		l = NLEntryNextOf (l);
	}
	SetNLEntryNext (nptr, libentrychain);
	libentrychain = nptr;	/* Add to the entry point list */
}

/*}}}*/
/*{{{  PUBLIC void genlibstubs()                         called from GEN1*/
/*****************************************************************************
 *
 *  genlibstubs adds a stub for each library call to the code buffer
 *
 *****************************************************************************/
PUBLIC void genlibstubs (void)
{
	const int stubsize = libpatchsize;
	treenode *libentry;
	for (libentry = libentrychain; libentry != NULL; libentry = NLEntryNextOf (libentry)) {
		const char *name = WNameOf (translate_from_internal (NNameOf (libentry)));	/* MDP */
		const int l = newlab ();
		SetNPLabel (libentry, l);	/* give the entry point a label */
		setsectionlab (l);
		if (etc_output) {
			etc_oinst (STUBNAME, name);
		}
		if (diagnostics) {
			gencomments ("Stub for library call %s", name);
		}
#if 0
fprintf (stderr, "code1k: stub for library call %s\n", name);
#endif
		/*{{{  debugging */
		if (debugoutput) {
			coder_genlibrpatch ();
		}
		/*}}} */
		/*{{{  assembly_output */
		if (assembly_output) {
			diag_write_nl ();
			fprintf (outfile, "%-8s$%s", "extern", name);
			diag_write_nl ();
			fprintf (outfile, "%-8s%d j codefix $%s 0", "patch", stubsize, name);
		}
		/*}}} */
	}
}

/*}}}*/
/*}}}*/
/*{{{  PUBLIC  int genproftable(INT32 size)              dummy*/
/*****************************************************************************
 *
 *  genproftable creates and initializes the profile table in the code buffer
 *
 *****************************************************************************/
PUBLIC int genproftable (INT32 proftsize)
{
	/* assert (!(sampling_profiling || line_profiling || cgraph_profiling )); */
	return -1;
}

/*}}}*/

/*{{{  PUBLIC void write_object_file()                   called from GEN1*/
/*****************************************************************************
 *
 * write_object_file writes out the object file and closes it
 *
 *****************************************************************************/
PUBLIC void write_object_file (void)
{
	if (tcoff_without_code || !assembly_output) {
		if (debugoutput)
			flush_debug_buffers ();
		if (etc_output) {
			write_code_block_start ((size_t) ctop);
			write_code_block (ctop, &codebuff[0]);
		}

		write_library_calls (!etc_output, libentrychain, I_J);
		output_entry_points ();

		write_end_module ();
	}
}

/*}}}*/
/*{{{  PUBLIC void close_object_file()*/
/*{{{  comment*/
/*****************************************************************************
*
*  close the object file - called by the harness
*
******************************************************************************/
/*}}}*/
PUBLIC void close_object_file (FILE * fptr, const char *const filename)
{
	if (fclose (fptr) == EOF)
		/*generr_s(GEN_CANNOT_CLOSE_OBJECT_FILE, filename); */
		msg_out_s (SEV_FATAL, GEN, GEN_CANNOT_CLOSE_OBJECT_FILE, NOPOSN, filename);
}

/*}}}*/
/*{{{  PUBLIC FILE *open_object_file()*/
/*****************************************************************************
 *
 * open_object_file opens the object file for writing
 *
 *****************************************************************************/
PUBLIC FILE *open_object_file (const char *const filename)
{
	FILE *const fptr = fopen (filename, "wb");
	if (fptr == NULL) {
		/*generr_s(GEN_CANNOT_OPEN_OBJECT_FILE, filename); */
		msg_out_s (SEV_FATAL, GEN, GEN_CANNOT_OPEN_OBJECT_FILE, NOPOSN, filename);
	}
	return fptr;
}

/*}}}*/
/*{{{  PUBLIC void initcode()                            called from GEN1*/
/***************************************************************************
 *
 * initcode initialises the coder.
 *
 **************************************************************************/
PUBLIC void initcode (void)
{
	etcfile = outfile;
	if (etc_output) {
		gencomment0 ("extended transputer code asm output\n");
	} else {
		gencomment0 ("commented transputer code asm output\n");
	}
	if (swap_r64words) {
		in_comment = FALSE;	/* force comment introducer */
		gencomment0 ("REAL64 words swapped and quadaligned\n");
	}
	if (kroc_flag) {
		in_comment = FALSE;
		gencomment0 ("Special KRoC version\n");
	}
	if (codebuff == NULL) {
		codebuff = (BYTE *)memalloc (max_code_size);
	}
	oldlocn = NOPOSN;
	ctop = 0;
	seginfo = NULL;
	libentrychain = NULL;
	dead = FALSE;
	const_top = -1;
	int_error_status = ERROR_STATUS_CLEAN;
	fpu_error_status = ERROR_STATUS_FP_INIT;
	undefine_tstack ();
	showtstack = diagnostics;
	assembly_optimise = assembly_output;
}

/*}}}*/
/*{{{  PUBLIC void freecode*/
/* This is only called if configuring is TRUE */
PUBLIC void freecode (void)
{
#if 0
	if (code != NULL) {
		memfree (code);
		code = NULL;
	}
#endif
	return;
}

/*}}}*/

