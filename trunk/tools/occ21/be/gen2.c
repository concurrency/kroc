/* $Id: gen2.c,v 1.4 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	code generator - support routines
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
#include "includes.h"
#include "extlib.h"		/* IMPORTED */

#include "genhdr.h"
#include "generror.h"
#include "predefhd.h"
#include "gen1def.h"
#include "gen2def.h"
#include "gen4def.h"
#include "bind1def.h"
#include "bind2def.h"
#include "bind3def.h"
#include "code1def.h"
/*}}}*/

/*{{{  PRIVATE variables*/
PRIVATE BOOL real32isaword;
/*}}}*/

/*{{{  constants*/
#define BOOL_CHECK_MASK      0x0002
#define BYTE_CHECK_MASK      0x0100
#define INT16_CHECK_MASK     0x8000
#define INT32_CHECK_MASK 0x80000000

/*{{{  tables for routines libcallstring and libconvertstring*/
#define N_LIBOPS 16		/* number of different operations */
#define N_LIBTYPES 5		/* number of different data types */
PRIVATE const int libops[N_LIBOPS] = { S_ADD, S_MULT, S_PLUS, S_BITAND, S_BITOR, S_XOR,
	S_SUBTRACT, S_DIV, S_REM, S_TIMES, S_MINUS,
	S_LSHIFT, S_RSHIFT, S_BITNOT, S_GR, S_EQ
};
PRIVATE const int libtypes[N_LIBTYPES] =
	{ S_INT16, S_INT32, S_INT64, S_REAL32, S_REAL64 };

PRIVATE const char *const libstrings[N_LIBOPS][N_LIBTYPES] = {
	/* "bug" 1065 - these are all changed to add %CHK on the end,
	   in accordance with the new naming decision. CON, 6/12/90
	 */
	{"INT16ADD%CHK", NULL, "INT64ADD%CHK", "REAL32OPERR%CHK", "REAL64OPERR%CHK"},
	{"INT16MUL%CHK", "INT32MUL%CHK", "INT64MUL%CHK", "REAL32OPERR%CHK", "REAL64OPERR%CHK"},
	{"INT16PLUS%CHK", NULL, "INT64PLUS%CHK", NULL, NULL},
	{"INT16BITAND%CHK", NULL, "INT64BITAND%CHK", NULL, NULL},
	{"INT16BITOR%CHK", NULL, "INT64BITOR%CHK", NULL, NULL},
	{"INT16XOR%CHK", NULL, "INT64XOR%CHK", NULL, NULL},
	{"INT16SUB%CHK", NULL, "INT64SUB%CHK", "REAL32OPERR%CHK", "REAL64OPERR%CHK"},
	{"INT16DIV%CHK", "INT32DIV%CHK", "INT64DIV%CHK", "REAL32OPERR%CHK", "REAL64OPERR%CHK"},
	{"INT16REM%CHK", "INT32REM%CHK", "INT64REM%CHK", "REAL32REMERR%CHK", "REAL64REMERR%CHK"},
	{"INT16TIMES%CHK", NULL, "INT64TIMES%CHK", NULL, NULL},
	{"INT16MINUS%CHK", NULL, "INT64MINUS%CHK", NULL, NULL},
	{"INT16LSHIFT%CHK", NULL, "INT64LSHIFT%CHK", NULL, NULL},
	{"INT16RSHIFT%CHK", NULL, "INT64RSHIFT%CHK", NULL, NULL},
	{"INT16BITNOT%CHK", NULL, "INT64BITNOT%CHK", NULL, NULL},
	{"INT16GT%CHK", NULL, "INT64GT%CHK", "REAL32GTERR%CHK", "REAL64GTERR%CHK"},
	{"INT16EQ%CHK", NULL, "INT64EQ%CHK", "REAL32EQERR%CHK", "REAL64EQERR%CHK"}
};

PRIVATE const char *const convertstrings[N_LIBTYPES][N_LIBTYPES] = {
	/* "bug" 1065 - these are all changed to add %CHK on the end,
	   in accordance with the new naming decision. CON, 6/12/90
	 */
	{NULL, NULL, "INT16TOINT64%CHK", "INT16TOREAL32%CHK", "INT16TOREAL64%CHK"},
	{NULL, NULL, "INT32TOINT64%CHK", "INT32TOREAL32%CHK", "INT32TOREAL64%CHK"},
	{"INT64TOINT16%CHK", "INT64TOINT32%CHK", NULL, "INT64TOREAL32%CHK", "INT64TOREAL64%CHK"},
	{"REAL32TOINT16%CHK", "REAL32TOINT32%CHK", "REAL32TOINT64%CHK", NULL, "REAL32TOREAL64%CHK"},
	{"REAL64TOINT16%CHK", "REAL64TOINT32%CHK", "REAL64TOINT64%CHK", "REAL64TOREAL32%CHK", NULL}
};
/* any of these which use a total (including hidden parameters) of
   more than 3 parameter slots:
   REAL32OPERR on 16bit, or REAL64OPERR on non has_fpu_core,
   must be explicitly checked in implicitparams because it checks how many
   param slots are used by dyadic operators, etc, and it is called
   before they have been converted into library calls.

   Really, they should be treated in a similar way to the predefines,
   but since there are only two cases, there seemed no point.
*/
/*}}}*/
/*{{{  predefined routines defns.*/
#define MAX_PREDEFS_NAMELEN 20
PRIVATE const BOOL trueval = TRUE;
PRIVATE const BOOL falseval = FALSE;
PRIVATE const struct
{
	const BOOL *is_inline;	/* Whether it is implemented inline */
	const int slots;	/* No of parameter slots required for library call */
	/* (including hidden params)             */
	/* Not needed if never compiled as lib call */
	/* Hex values, 0xabc; a=16bit, b=32bit, c=has_fpu_core */
}
pdinlines[] =
{
	/*{{{  long arithmetic */
	{ &trueval, 0},			/* LONGADD */
	{ &trueval, 0},			/* LONGSUM */
	{ &trueval, 0},			/* LONGSUB */
	{ &trueval, 0},			/* LONGDIFF */
	{ &trueval, 0},			/* LONGPROD */
	{ &trueval, 0},			/* LONGDIV */
		/*}}} */
		/*{{{  shift */
	{ &trueval, 0},			/* SHIFTRIGHT */
	{ &trueval, 0},			/* SHIFTLEFT   */
		/*}}} */
		/*{{{  normalise, fracmul */
	{ &trueval, 0},			/* NORMALISE */
	{ &has_fmul, 0x222},			/* FRACMUL */
		/*}}} */
		/*{{{  arithmetic shift, rotate */
	{ &trueval, 0},			/* ASHIFTRIGHT */
	{ &trueval, 0},			/* ASHIFTLEFT */
	{ &trueval, 0},			/* ROTATERIGHT */
	{ &trueval, 0},			/* ROTATELEFT */
		/*}}} */
		/*{{{  causeerror */
	{ &trueval, 0},			/* CAUSEERROR */
		/*}}} */
		/*{{{  kernel run, load channel, load channel vector, load byte vector */
	{ &trueval, 0},			/* KERNELRUN */
	{ &trueval, 0},			/* LOADINPUTCHANNEL */
	{ &trueval, 0},			/* LOADOUTPUTCHANNEL */
	{ &trueval, 0},			/* LOADINPUTCHANNELVECTOR */
	{ &trueval, 0},			/* LOADOUTPUTCHANNELVECTOR */
	{ &trueval, 0},			/* LOADBYTEVECTOR */
		/*}}} */
		/*{{{  unpacksn roundsn */
	{ &has_fp_support, 0x111},	/* UNPACKSN */
	{ &has_fp_support, 0x333},	/* ROUNDSN */
		/* ROUNDSN is treated specially cos it needs Wptr+0; see below */
		/*}}} */
		/*{{{  draw2d, clip2d, move2d */
	{ &has_move2d, 0xCCC},		/* DRAW2D */
	{ &has_move2d, 0xCCC},		/* CLIP2D */
	{ &has_move2d, 0xCCC},		/* MOVE2D */
		/*}}} */
		/*{{{  crc byte and word */
	{ &has_crc, 0x333},		/* CRCWORD */
	{ &has_crc, 0x333},		/* CRCBYTE */
		/*}}} */
		/*{{{  bit ops */
	{ &has_bitops, 0x222},		/* BITCOUNT */
	{ &has_bitops, 0x111},		/* BITREVWORD */
	{ &has_bitops, 0x222},		/* BITREVNBITS */
		/*}}} */
		/*{{{  floating point */
	{ &has_fpu_core, 0x211},	/* ABS */
	{ &has_ordered, 0x111},		/* ISNAN */
	{ &real32isaword, 0x111},	/* NOTFINITE */
	{ &has_ordered, 0x222},		/* ORDERED */
	{ &real32isaword, 0x211},	/* MINUSX */
	{ &has_fpu_core, 0x211},	/* MULBY2 */
	{ &has_fpu_core, 0x211},	/* DIVBY2 */
	{ &has_sqrt, 0x211},		/* SQRT */
	{ &has_fpint, 0x211},		/* FPINT */
	{ &has_fpu_core, 0x221},	/* DABS */
	{ &has_ordered, 0x111},		/* DISNAN */
	{ &has_ordered, 0x111},		/* DNOTFINITE */
	{ &has_ordered, 0x222},		/* DORDERED */
	{ &has_fpu_core, 0x221},	/* DMINUSX */
	{ &has_fpu_core, 0x221},	/* DMULBY2 */
	{ &has_fpu_core, 0x221},	/* DDIVBY2 */
	{ &has_sqrt, 0x221},		/* DSQRT */
	{ &has_fpint, 0x221},		/* DFPINT */
	{ &falseval, 0x322},		/* SCALEB */
	{ &falseval, 0x332},		/* DSCALEB */
	{ &falseval, 0x322},		/* COPYSIGN */
	{ &falseval, 0x332},		/* DCOPYSIGN */
	{ &falseval, 0x322},		/* NEXTAFTER */
	{ &falseval, 0x332},		/* DNEXTAFTER */
	{ &falseval, 0x211},		/* LOGB */
	{ &falseval, 0x221},		/* DLOGB */
	{ &falseval, 0x211},		/* FLOATINGUNPACK */
	{ &falseval, 0x222},		/* DFLOATINGUNPACK */
	{ &falseval, 0x533},		/* ARGUMENTREDUCE */
	{ &falseval, 0x544},		/* DARGUMENTREDUCE */
		/*}}} */
		/*{{{  IEEE arithmetic */
	{ &falseval, 0x433},		/* REAL32OP */
	{ &falseval, 0x443},		/* REAL64OP */
	{ &falseval, 0x544},		/* IEEE32OP */
	{ &falseval, 0x555},		/* IEEE64OP */
	{ &falseval, 0x322},		/* REAL32REM */
	{ &falseval, 0x332},		/* REAL64REM */
	{ &falseval, 0x222},		/* REAL32EQ */
	{ &falseval, 0x222},		/* REAL64EQ */
	{ &falseval, 0x222},		/* REAL32GT */
	{ &falseval, 0x222},		/* REAL64GT */
	{ &falseval, 0x222},		/* IEEECOMPARE */
	{ &falseval, 0x222},		/* DIEEECOMPARE */
	{ &falseval, 0x322},		/* IEEE32REM */
	{ &falseval, 0x333},		/* IEEE64REM */
		/*}}} */
		/*{{{  reschedule/assert */
	{ &trueval, 0},			/* RESCHEDULE */
	{ &trueval, 0},			/* ASSERT */
		/*}}} */
		/*{{{  WSSIZE and VSSIZE */
	{ &trueval, 0},			/* WSSIZE */
	{ &trueval, 0},			/* VSSIZE */
	{ &trueval, 0},			/* UPDATE_PROFCOUNT */
	/*}}} */
	{ &trueval, 0},			/* IMS_GET_PDATA */
#ifdef PROCESS_PRIORITY
	{ &trueval, 0},			/* GETPRI */
	{ &trueval, 0},			/* SETPRI */
	{ &trueval, 0},			/* INCPRI */
	{ &trueval, 0},			/* DECPRI */
#else	/* !PROCESS_PRIORITY */
	{ &falseval, 0},		/* GETPRI */
	{ &falseval, 0},		/* SETPRI */
	{ &falseval, 0},		/* INCPRI */
	{ &falseval, 0},		/* DECPRI */
#endif	/* !PROCESS_PRIORITY */
#ifdef MOBILES
	{ &trueval, 0}			/* ALLOC.CHAN.TYPE */
#else	/* !MOBILES */
	{ &falseval, 0}			/* ALLOC.CHAN.TYPE */
#endif	/* !MOBILES */
	, { &trueval, 0}		/* PROTOCOL.HASH */
	, { &trueval, 0}		/* DECODE.CHANNEL */
	, { &trueval, 0}		/* DECODE.CHANNEL3 */
	, { &trueval, 0}		/* ENCODE.CHANNEL */
	, { &trueval, 0}		/* DETACH.DYNMOB */
	, { &trueval, 0}		/* ATTACH.DYNMOB */
	, { &trueval, 0}		/* DECODE.DATA */
	, { &falseval, 0x888}		/* MPBARSYNC */
	, { &falseval, 0x888}		/* MPBARRESIGN */
	, { &falseval, 0}		/* MPPSERIALISE */
	, { &falseval, 0}		/* MPPDESERIALISE */
	, { &falseval, 0}		/* MPPCHECKROUTINE */
	, { &falseval, 0}		/* MPPLOADLIBRARY */
	, { &falseval, 0}		/* MPPUNLOADLIBRARY */
	, { &falseval, 0}		/* PGRP.NEWGRP */
	, { &falseval, 0}		/* PGRP.INFO */
	, { &trueval, 0}		/* LOAD.TYPE.DESC */
		/*{{{  more floating point*/
	, { &has_sincostan, 0x211}	/* SIN */
	, { &has_sincostan, 0x221}	/* DSIN */
	, { &has_sincostan, 0x211}	/* COS */
	, { &has_sincostan, 0x221}	/* DCOS */
		/*}}}*/
		/*{{{  processor affinity*/
	, { &trueval, 0}		/* GETAFF */
	, { &trueval, 0}		/* SETAFF */
		/*}}}*/
		/*{{{  blocking system calls*/
	, { &trueval, 0}		/* KILLCALL */
		/*}}}*/
		/*{{{  RMoX interrupt handling*/
	, { &trueval, 0}		/* WAIT.FOR.INTERRUPT */
		/*}}}*/
		/*{{{  MOBILE manipulation*/
	, { &trueval, 0}		/* BIND.MOBILE */
	, { &trueval, 0}		/* BIND.MOBILE.HW */
	, { &trueval, 0}		/* DMA.CAPABLE */
	, { &trueval, 0}		/* MAKE.DMA.CAPABLE */
		/*}}}*/
		/*{{{  yet more floating point*/
	, { &has_sincostan, 0x211}	/* TAN */
	, { &has_sincostan, 0x221}	/* DTAN */
		/*}}}*/
		/*{{{  memory barriers */
	, { &trueval, 0}		/* MEMORY.BARRIER */
	, { &trueval, 0}		/* READ.MEMORY.BARRIER */
	, { &trueval, 0}		/* WRITE.MEMORY.BARRIER */
		/*}}}*/
		/*{{{  (more) MOBILE manipulation*/
	, { &trueval, 0}		/* RESIZE.MOBILE.ARRAY.1D */
		/*}}}*/
};

/*}}}*/
/*}}}*/

/*{{{  routines*/
/*{{{  PUBLIC wordnode *processlibname*/
PUBLIC wordnode *processlibname (const char *const str, const int suffix_len, const char *new_suffix)
/* This 'un-hides' the name if required.
   Or adds the new suffix if that is required.
   The default is 'hidden' because that is what would normally be wanted.
*/
{
	int len = strlen (str);
	if (!hidelibnames)
		len -= suffix_len;	/* remove the '%' suffix */
	else if (new_suffix != NULL) {
		char buffer[MAX_PREDEFS_NAMELEN + 15];	/* allow plenty of space */
		const int old_len = len - suffix_len;
		const int extra_len = strlen (new_suffix);
		if ((old_len + extra_len) >= (MAX_PREDEFS_NAMELEN + 10))
			err_abort ("processlibname");
		memcpy (buffer, str, old_len);
		memcpy (&buffer[old_len], new_suffix, extra_len);
/*buffer[old_len+extra_len] = '\0'; *//* probably not necessary really */
		return lookupword (buffer, old_len + extra_len);
	}
	return lookupword (str, len);
}

/*}}}*/
/*{{{  PUBLIC BOOL pdinline*/
PUBLIC BOOL pdinline (const int pdno)
{
	real32isaword = bytesperword == 4;

	/*{{{  T9000_alpha bugs and workarounds */
	switch (pdno) {
	case PD_FPINT:
	case PD_DFPINT:
		if (T9000_alpha_nofpconv (&tx_global))
			return FALSE;
		break;
	case PD_BITCOUNT:
		if (T9000_gamma_nobitcnt (&tx_global))
			return FALSE;
		break;
	default:
		break;
	}
	/*}}} */

	if ((pdno < 0) || (pdno >= (sizeof (pdinlines) / sizeof (pdinlines[0])))) {
		err_abort ("pdinline");
	}
	return *(pdinlines[pdno].is_inline);
}

/*}}}*/
/*{{{  PUBLIC wordnode *pdlibname*/
PUBLIC wordnode *pdlibname (wordnode * const pdname, const int pdno)
{
	if (!hidelibnames || (pdinlines[pdno].slots == 0)) {
		return pdname;
	} else {
		char buffer[MAX_PREDEFS_NAMELEN + 2];
		const int len = WLengthOf (pdname);
		if (len >= MAX_PREDEFS_NAMELEN)
			err_abort ("pdlibname");
		memcpy (buffer, WNameOf (pdname), len);
		buffer[len] = '%';
		buffer[len + 1] = 'O';	/* "bug" 1065 - converted to %O rather than just % */
		/* CON 6/12/90 */
		buffer[len + 2] = '\0';
		DEBUG_MSG (("pdlibname: %s:%d\n", buffer, len));
		return processlibname (buffer, 2, predefsuffix);
	}
}

/*}}}*/
/*{{{  PUBLIC int pdparams*/
PUBLIC int pdparams (const treenode * tptr)
{
	/* This returns the number of param slots (including hidden params)
	   required by a predefine library call. -1 otherwise.
	   This is used by findinstances, cos at that stage the hidden params
	   haven't yet been augmented */

	int pdno;
	if ((TagOf (INameOf (tptr)) != N_PREDEFFUNCTION) && (TagOf (INameOf (tptr)) != N_PREDEFPROC))
		return (-1);
	pdno = NModeOf (INameOf (tptr));

	if (has_fp_support && pdno == PD_ROUNDSN)
		return (4);	/* ensure that Wptr+0 is left clear */
	if (pdinline (pdno))
		return (-1);

	if (has_fpu_core)	/* T8 */
		return (pdinlines[pdno].slots & 0xF);	/* Bottom hex digit */
	else if (targetintsize == S_INT32)	/* T4 */
		return ((pdinlines[pdno].slots >> 4) & 0xF);	/* Middle hex digit */
	else			/* T2 */
		return ((pdinlines[pdno].slots >> 8) & 0xF);	/* Top hex digit */
}

/*}}}*/
/*{{{  PUBLIC treenode *constantmatch(c, clist)*/
/*****************************************************************************
 *
 *  constantmatch takes a constant expression node c, and a list of
 *                constant nodes (scalars and vectors): if a constant
 *                expression node which matches c (ie. has same size and
 *                value) is found on the list before reaching c, then
 *                a pointer to that node is returned, otherwise NULL is
 *                returned.
 *
 *  Modified 18/10/90 as part of bug 1024: Now don't worry about the size;
 *  Hence a REAL32 can be the first half of a REAL64..
 *  MDP: this is not safe if we are swapping REAL64 words!
 *  CGR: ignore the above, redone to support endianness.
 *
 *****************************************************************************/
PUBLIC treenode *constantmatch (treenode * c, treenode * clist)
{
	const treenode *typeofc = gettype (c);
	const INT32 bytesin_c = TagOf (c) == S_CONSTEXP ? bytesin (typeofc) : 0;
	const int byteinint = bytesinscalar (targetintsize);
	
	while ((clist != c) && (clist != NULL))
		switch (TagOf (clist)) {
			/*{{{  S_CONSTEXP                    break / return */
		case S_CONSTEXP:
			if ((TagOf (c) == S_CONSTEXP) && (LoValOf (c) == LoValOf (clist))) {
				const treenode *typeinlist = gettype (clist);
				const int bytesin_list = bytesin (typeinlist);
				
				if (target_bigendian && has_fpu_core && (TagOf (typeinlist) == S_REAL64)) {
					if ((TagOf (typeofc) == S_REAL64) && (HiValOf (c) == HiValOf (clist))) {
						return (clist);
					}
				} else if (bytesin_c <= bytesin_list) {
					if ((bytesin_c <= byteinint) || (HiValOf (c) == HiValOf (clist))) {
						return (clist);
					}
				}
			}
			clist = CENextOf (clist);
			break;
			/*}}} */
			/*{{{  S_STRING S_CONSTCONSTRUCTOR   break / return */
		case S_STRING:
		case S_CONSTCONSTRUCTOR:
			if ((TagOf (c) != S_CONSTEXP) && (CTValOf (c) == CTValOf (clist))) {
				return (clist);
			}
			clist = CTNextOf (clist);
			break;
			/*}}} */
		default:
			badtag (genlocn, TagOf (clist), "constantmatch");
		}
	return (NULL);
}

/*}}}*/
/*{{{  PUBLIC void declare_sc_entries (nptr)           NEVER USED*/
#if 0				/* never used */
/*****************************************************************************
 *
 *  declare_sc_entries takes a list of SC entrypoints 'nptr' and
 *                     allocates each entry point a label and offset
 *                     of the label from the beginning of this module's code
 *
 *                     Warn about entry points with the same name, and don't
 *                     declare the descoped ones.
 *
 *****************************************************************************/
PUBLIC void declare_sc_entries (treenode * nptr, INT32 prevcodesize)
{
	while (nptr != NULL) {
		int lab = newlab ();
		SetNSCEntryLabel (nptr, (INT32) lab);
		SetNSCEntryOffset (nptr, NSCEntryOffsetOf (nptr) + prevcodesize);
		declarelabel (lab, NSCEntryOffsetOf (nptr));
		nptr = NSCEntryNextOf (nptr);
	}
}
#endif

/*}}}*/
/*{{{  PUBLIC BOOL cancauseerror(treenode *tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  cancauseerror returns TRUE if execution of the tree 'tptr' could possibly
 *                set the error flag.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC BOOL cancauseerror (treenode * tptr)
{
	if (!(NEED_ERRORS))
		return FALSE;
	switch (TagOf (tptr)) {
	default:
		return TRUE;
		/*{{{  monadics whose operands might ... */
	case S_UMINUS:
	case S_BITNOT:
	case S_NOT:
	case S_SIZE:
		return cancauseerror (OpOf (tptr));
		/*{{{  ROUND TRUNC EXACT */
	case S_ROUND:
	case S_TRUNC:
	case S_EXACT:
		{
			int sourcetype = ntypeof (OpOf (tptr));
			int desttype = MOpTypeOf (tptr);
			if (sourcetype == S_INT)
				sourcetype = targetintsize;
			if (desttype == S_INT)
				desttype = targetintsize;
			if (TagOf (tptr) == S_EXACT) {
				if (isreal (sourcetype) || isreal (desttype) || hasgreaterrange (desttype, sourcetype))
					return cancauseerror (OpOf (tptr));
				else
					return TRUE;
			} else {
				if ((sourcetype == desttype) ||
				    (sourcetype == S_INT16 && isreal (desttype)) ||
				    (sourcetype == S_INT32 && desttype == S_REAL64)) return cancauseerror (OpOf (tptr));
				else
					return TRUE;
			}
		}
		/*}}} */
		/*}}} */
		/*{{{  dyadics whose operands might ... */
	case S_AND:
	case S_OR:
	case S_BITAND:
	case S_BITOR:
	case S_XOR:
	case S_PLUS:
	case S_MINUS:
	case S_TIMES:
	case S_EQ:
	case S_NE:
	case S_LS:
	case S_LE:
	case S_GR:
	case S_GE:
		return cancauseerror (LeftOpOf (tptr)) || cancauseerror (RightOpOf (tptr));
	case S_AFTER:
		assert (FALSE);	/* after is processd in trans */
		return TRUE;

		/*}}} */
		/*{{{  constants and names which can't */
	case S_DUMMYEXP:
	case S_CONSTEXP:
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
	case N_VALPARAM:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_DECL:
	case N_REPL:
	case S_FNACTUALRESULT:
	case S_FNFORMALRESULT:
	case S_HIDDEN_PARAM:
		return FALSE;
		/*}}} */
	}
}

/*}}}*/
/*{{{  PUBLIC BOOL isconversion(treenode *tptr)*/
/*****************************************************************************
 *
 *  isconversion returns TRUE if 'tptr' is a conversion tree.
 *
 *****************************************************************************/
PUBLIC BOOL isconversion (treenode * tptr)
{
	const int tag = TagOf (tptr);
	return (tag == S_EXACT || tag == S_TRUNC || tag == S_ROUND);
}

/*}}}*/
/*{{{  PUBLIC BOOL needtemptoload(opdmode, opd)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  needtemptoload returns TRUE if we have to move (opdmode, opd) to a
 *                 temporary before we can load it.
 *                 Also decides, for real values on an fp processor, whether
 *                 we need to generate (opdmode, opd) to a temporary in order
 *                 to load it into an integer register.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC BOOL needtemptoload (const int opdmode, treenode * const opd)
{
	if (opdmode == P_EXP) {
		if (has_fpu_core && ((ntypeof (opd) == S_REAL32 && !isaddressable (opd)) || (isconversion (opd) && isreal (ntypeof (OpOf (opd))))))
			return TRUE;
		if (use_shortintops)	/* T9000 shorts 17/7/91 */
			return FALSE;
		switch (TagOf (opd)) {
			/*{{{  PARAM ABBR RETYPE DECL */
		case N_PARAM:
		case N_RESULTPARAM:
		case N_ABBR:
		case N_RETYPE:
		case N_DECL:
			{
				treenode *type = gettype (opd);
				if (TagOf (type) == S_PORT)
					type = ProtocolOf (type);
				return isshortint (TagOf (type)) && (ispointer (opd) /*|| isplaced(opd) */ );	/* bug TS/1467 8/11/91 */
			}
			/*}}} */
		case S_ARRAYITEM:
		case S_RECORDITEM:
			return isshortint (ntypeof (opd));
		default:
			return FALSE;
		}
	}
	return FALSE;
}

/*}}}*/
/*{{{  PUBLIC treenode *firstresultof(tptr)*/
/*****************************************************************************
 *
 * firstresultof takes a formal parameter list 'tptr' and returns a list whose
 *               head is the first FNFORMALRESULT node on 'tptr'.
 *
 *****************************************************************************/
PUBLIC treenode *firstresultof (treenode * tptr)
{
	while (!EndOfList (tptr) && (TagOf (ThisItem (tptr)) != S_FNFORMALRESULT)) {
		DEBUG_MSG (("firstresultof: skipping %s\n", itagstring (TagOf (ThisItem (tptr)))));
		tptr = NextItem (tptr);
	}
	DEBUG_MSG (("firstresultof: returning %s\n", EndOfList (tptr) ? "End of list" : itagstring (TagOf (ThisItem (tptr)))));
	return (tptr);
}

/*}}}*/
/*{{{  PUBLIC treenode *nextresultof(tptr)*/
/*****************************************************************************
 *
 * nextresultof takes a formal parameter list 'tptr' and returns a list whose
 *              head is the next FN_RESULTPTR node on 'tptr'.
 *
 *****************************************************************************/
PUBLIC treenode *nextresultof (treenode * tptr)
{
	DEBUG_MSG (("nextresultof\n"));
	return (firstresultof (NextItem (tptr)));
}

/*}}}*/
/*{{{  PUBLIC wordnode *libcallstring (op, type)*/
/***************************************************************************
 *
 *  libcallstring returns a string representing the name of extended
 *                type operation 'op' for type 'type'.
 *
 **************************************************************************/
PUBLIC wordnode *libcallstring (int op, int type)
{
	int opix = (-1), typeix = (-1);
	int i;
	/*{{{  set up opix */
	i = 0;
	while ((opix < 0) && (i < (sizeof (libops) / sizeof (int))))
		if (libops[i] == op)
			opix = i;
		else
			i++;
	if (opix < 0)
		geninternal_is (GEN_ERROR_IN_ROUTINE, op, "libcallstring-op");
	/*}}} */
	/*{{{  set up typeix */
	i = 0;
	while ((typeix < 0) && (i < (sizeof (libtypes) / sizeof (int))))
		if (libtypes[i] == type)
			typeix = i;
		else
			i++;
	if (typeix < 0)
		geninternal_is (GEN_ERROR_IN_ROUTINE, op, "libcallstring-type");
	/*}}} */
	return processlibname (libstrings[opix][typeix], 4, compilersuffix);
}

/*}}}*/
/*{{{  PRIVATE wordnode *libconvertstring (sourcetype, desttype)*/
/***************************************************************************
 *
 *  libconvertstring returns a string representing the name of the extended
 *                   type conversion routine from sourcetype to desttype.
 *
 **************************************************************************/
PRIVATE wordnode *libconvertstring (int sourcetype, int desttype)
{
	int sourceix = (-1), destix = (-1);
	int i;
	/*{{{  set up sourceix */
	for (i = 0; (sourceix < 0) && (i < (sizeof (libtypes) / sizeof (int))); i++)
		if (libtypes[i] == sourcetype)
			sourceix = i;
	if (sourceix < 0)
		badtag (genlocn, sourcetype, "libconvertstring");
	/*}}} */
	/*{{{  set up destix */
	for (i = 0; (destix < 0) && (i < (sizeof (libtypes) / sizeof (int))); i++)
		if (libtypes[i] == desttype)
			destix = i;
	if (destix < 0)
		badtag (genlocn, desttype, "libconvertstring");
	/*}}} */

	return processlibname (convertstrings[sourceix][destix], 4, compilersuffix);
}

/*}}}*/
/*{{{  PUBLIC int implicitparams(Op, type)*/
PUBLIC int implicitparams (int op, int type)
  /* Conceptually this returns the number of parameter slots required
     for any monadic or dyadic operator which is turned into a function call.
     It returns (-1) if the operation is perfomed inline.
     However, we can get away with being lazy and only returning the values
     for those function calls which require more parameters than there
     are registers. They are 'long' real operations only.
     We can get away with returning -1 for all others, because 'regsfor'
     correctly decides that all registers will be used for all other
     conversions.
     CO'N 30/4/90
   */
{
	switch (op) {
	case S_ADD:
	case S_SUBTRACT:
	case S_MULT:
	case S_DIV:
	case S_NEG:		/* unary negation *//* Added to fix bug 295 30/4/90 by CO'N */
		/* We haven't turned REAL64 add etc into a lib call yet. Here's a bodge:
		   They get turned into z := REAL64OPERR (x, op, y), or
		   z := REAL32OPERR (x, op, y), but since types are
		   double length, the extra hidden parameter causes problems */
		/* Unary negation is turned into a subtraction from zero */
		return (has_fpu_core ? (-1) : (type == S_REAL64) ? 4 : (type == S_REAL32) ? ((targetintsize == S_INT16) ? 4 : 3) : (-1));
	default:
		return (-1);
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *makeconversion(sourcetype, desttype, source, mode)*/
/*****************************************************************************
 *
 *  makeconversion creates a function call to a standard library routine
 *                 to convert 'source' from 'sourcetype' to 'desttype' with
 *                 rounding mode 'mode'.
 *
 *****************************************************************************/
PUBLIC treenode *makeconversion (int sourcetype, int desttype, treenode * source, const int mode, const SOURCEPOSN locn)
{
	treenode *iname;
	treenode *paramlist;
	if (sourcetype == S_INT)
		sourcetype = targetintsize;
	if (desttype == S_INT)
		desttype = targetintsize;
	iname = libentry (libconvertstring (sourcetype, desttype), locn);
	paramlist = newlistnode (S_LIST, locn, source, NULL);
	if
		/*{{{  conversion requires a rounding mode */
		((isreal (sourcetype) || isreal (desttype)) &&
		 (!((sourcetype == S_REAL32 && desttype == S_REAL64) ||
		    (sourcetype == S_INT32 && desttype == S_REAL64) || (sourcetype == S_INT16 && (desttype == S_REAL32 || desttype == S_REAL64)))))
		/*}}} */
		/*{{{  add rounding mode to parameter list */
	{
		treenode *modeptr = newconstant ((mode == S_ROUND) ? Nearest : Truncate);
		paramlist = newlistnode (S_LIST, locn, modeptr, paramlist);
	}
	/*}}} */
	return newinstancenode (S_FINSTANCE, locn, iname, paramlist);
}

/*}}}*/
/*{{{  PUBLIC treenode *makedopfunction (tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  makedopfunction takes a dyadic operator node tptr and returns
 *                  a function instance tree which calls the appropriate
 *                  library routine for the dyadic.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC treenode *makedopfunction (treenode * tptr)
{
	const int op = TagOf (tptr);
	const SOURCEPOSN locn = LocnOf (tptr);
	treenode *left = LeftOpOf (tptr);
	treenode *right = RightOpOf (tptr);
	treenode *iname;
	treenode *paramlist;
	int type = DOpTypeOf (tptr);
	if (type == S_INT)
		type = targetintsize;
	if (commutes (tptr) && (regsfor (left) > regsfor (right)))
		/*{{{  swap left and right */
	{
		treenode *temp = left;
		left = right;
		right = temp;
	}
	/*}}} */

	iname = libentry (libcallstring (op, type), locn);
	/*{{{  make the parameter list */
	if (isreal (type))
		/*{{{  make a call to realxxop/realxxrem */
	{
		if (op == S_REM)
			/*{{{  call REALxxREM */
			/*{{{  COMMENT what the tree looks like */
	/**********************  Start comment out ****************************
        @*
                rem                    finstance
              /    \       =>         /       \
             e1    e2          real32rem      list
                                              /   \
                                            e1    list
                                                  /   \
                                                e2    NULL
        *@
         **********************   End comment out  ****************************/
			/*}}} */
			paramlist = newlistnode (S_LIST, locn, left, newlistnode (S_LIST, locn, right, NULL));
		/*}}} */
		else
			/*{{{  call REALxxOP */
			/*{{{  COMMENT what the tree looks like */
	/**********************  Start comment out ****************************
        @*
                op                    finstance
              /    \       =>         /       \
             e1    e2          real32operr    list
                                              /   \
                                            e1'   list
                                                  /   \
                                              op.val  list
                                                      /   \
                                                    e2'   NULL

           If op is commutative, e1' =  most.complex(e1, e2)
                                 e2' =  least.complex(e1, e2)
        *@
         **********************   End comment out  ****************************/
			/*}}} */
		{
			INT32 op_mode = (op == S_ADD) ? Op_Add : (op == S_SUBTRACT) ? Op_Sub : (op == S_MULT) ? Op_Mul : Op_Div;
			paramlist =
				newlistnode (S_LIST, locn, left,
					     newlistnode (S_LIST, locn,
							  newconstexpnode (S_CONSTEXP, locn, dummyexp_p, 0, op_mode),
							  newlistnode (S_LIST, locn, right, NULL)));
		}
		/*}}} */
	}
	/*}}} */
	else
		paramlist = newlistnode (S_LIST, locn, left, newlistnode (S_LIST, locn, right, NULL));
	/*}}} */

	return newinstancenode (S_FINSTANCE, locn, iname, paramlist);
}

/*}}}*/
/*{{{  PUBLIC BOOL istrueguard(tptr)*/
/*****************************************************************************
 *
 *  istrueguard returns TRUE if the guard represented by 'tptr' is a 'TRUE'
 *              guard.
 *
 *****************************************************************************/
PUBLIC BOOL istrueguard (treenode * const tptr)
{
	return ((TagOf (tptr) == S_CONSTEXP) && (LoValOf (tptr) == 1));
}

/*}}}*/
/*{{{  PUBLIC BOOL isfalseguard(tptr)*/
/*****************************************************************************
 *
 *  isfalseguard returns TRUE if the guard represented by 'tptr' is a 'FALSE'
 *              guard.
 *
 *****************************************************************************/
PUBLIC BOOL isfalseguard (treenode * const tptr)
{
	return ((TagOf (tptr) == S_CONSTEXP) && (LoValOf (tptr) == 0));
}

/*}}}*/
/*{{{  PUBLIC BOOL isskipbody(tptr)*/
/*****************************************************************************
 *
 *  isskipbody returns TRUE if the process represented by tptr is a SKIP
 *             process (any leading specifications are ignored
 *
 *****************************************************************************/
PUBLIC BOOL isskipbody (treenode * const tptr)
{
	return (TagOf (skipspecifications (tptr)) == S_SKIP);
}

/*}}}*/
/*{{{  PUBLIC BOOL complexinitialise(typeptr)*/
/*****************************************************************************
 *
 *  complexinitialise returns TRUE if the initialisation of a variable of
 *                    type 'typeptr' requires workspace slots during the
 *                    initialisation.
 *                    ie. the type is an array of CHAN with more than 3
 *                    elements.
 *
 *****************************************************************************/
PUBLIC BOOL complexinitialise (treenode * const typeptr)
{
	if (channel_basetype (typeptr)) {
#ifdef RECORDS_OF_CHANNELS
		return bytesin (typeptr) > (int) (bytesperword * 3);
#else
		return elementsin (typeptr) > 3;
#endif
	}
	return FALSE;
}

/*}}}*/
/*{{{  PUBLIC BOOL channel_basetype(treenode *type)*/
PUBLIC BOOL channel_basetype (treenode * type)
{
	treenode *const base_type = basetype_tree (type);
	if (TagOf (base_type) == S_CHAN)
		return TRUE;
#ifdef RECORDS_OF_CHANNELS
	if ((TagOf (base_type) == S_RECORD) && ((TypeAttrOf (base_type) & TypeAttr_chantype) != 0))
		return TRUE;
#endif
	return FALSE;
}

/*}}}*/

/*{{{  PUBLIC BOOL timerguardinalt(tptr)*/
/*****************************************************************************
 *
 *  timerguardinalt takes an ALT or replicated ALT tree 'tptr' and returns
 *                  TRUE if there is a timer guard in the ALT or any nested
 *                  ALTs, otherwise returns FALSE.
 *
 *****************************************************************************/
PUBLIC BOOL timerguardinalt (treenode * tptr)
{
	tptr = skipspecifications (tptr);
	switch (TagOf (tptr)) {
		/*{{{  S_ALT S_PRIALT */
	case S_ALT:
	case S_PRIALT:
		{
			BOOL result = FALSE;
			for (tptr = CBodyOf (tptr); !EndOfList (tptr) && !result; tptr = NextItem (tptr)) {
				/* result = timerguardinalt(SpBodyOf(ThisItem(tptr))); */
				result = timerguardinalt (ThisItem (tptr));
			}
			return (result);
		}
		/*}}} */
		/*{{{  S_REPLALT S_PRIREPLALT */
	case S_REPLALT:
	case S_PRIREPLALT:
		return (timerguardinalt (ReplCBodyOf (tptr)));
		/*}}} */
		/*{{{  S_ALTERNATIVE */
	case S_ALTERNATIVE:
		/*return(TagOf(AltInputOf(tptr)) == S_DELAYED_INPUT); */
		return AltTimeExpOf (tptr) != NULL;	/* bug TS/1626 22/04/92 */
		/*}}} */
	default:
		badtag (genlocn, TagOf (tptr), "timerguardinalt");
	}
	return (0);		/* not reached */
}

/*}}}*/
/*{{{  PUBLIC BOOL fitsin (sourcetype, desttype)        NEVER USED*/
#if 0				/* fitsin is never used */
/*****************************************************************************
 *
 *  fitsin returns TRUE if a value of type 'sourcetype' will fit in an
 *         object of type 'desttype'.
 *
 *****************************************************************************/
PUBLIC BOOL fitsin (int sourcetype, int desttype)
{
	if (sourcetype == S_INT)
		sourcetype = targetintsize;
	if (desttype == S_INT)
		desttype = targetintsize;
	switch (sourcetype)
		/*{{{  pick out the FALSE cases */
	{
	case S_INT16:
		if ((desttype == S_BOOL) || (desttype == S_BYTE))
			return (FALSE);
		break;
	case S_INT32:
	case S_REAL32:
		if ((desttype == S_BOOL) || (desttype == S_BYTE) || (desttype == S_INT16))
			return (FALSE);
		break;
	case S_INT64:
	case S_REAL64:
		if ((desttype != S_INT64) && (desttype != S_REAL64))
			return (TRUE);
		break;
	case S_RESOURCE:
		if (desttype != S_RESOURCE) {
			return (FALSE);
		}
		break;
	default:		/* BOOL and BYTE */
		break;
	}
	/*}}} */
	return (TRUE);
}
#endif /* fitsin is never used */
/*}}}*/
/*{{{  PUBLIC BOOL commutes (tptr)*/
/*****************************************************************************
 *
 *  commutes returns TRUE if the dyadic operator tree 'tptr' is commutative.
 *
 *****************************************************************************/
PUBLIC BOOL commutes (treenode * const tptr)
{
	switch (TagOf (tptr)) {
		/*{{{  ADD MULT BITAND BITOR XOR PLUS EQ */
	case S_ADD:
	case S_MULT:
	case S_BITAND:
	case S_BITOR:
	case S_XOR:
	case S_PLUS:
	case S_EQ:
		return (TRUE);
		/*}}} */
		/*{{{  SUBTRACT DIV REM LSHIFT RSHIFT MINUS CSUB0 CCNT1 GR */
	case S_SUBTRACT:
	case S_DIV:
	case S_REM:
	case S_LSHIFT:
	case S_RSHIFT:
	case S_MINUS:
	case S_GR:
	case S_CSUB0:
	case S_CCNT1:
		return (FALSE);
		/*}}} */
		/*{{{  TIMES */
	case S_TIMES:
		/* INSdi02140 - On a T9000, consider 'prod' to be commutative */
		return tx_global.t9000_timings;
		/*}}} */
	default:
		badtag (genlocn, TagOf (tptr), "commutes");
	}
	return (FALSE);		/* not reached */
}

/*}}}*/
/*{{{  PUBLIC BOOL issimpleopd (opdmode, opd)*/
/*****************************************************************************
 *
 *  issimpleopd returns TRUE if (opdmode, opd) is simple, ie. not
 *              subscripted or segmented.
 *
 *****************************************************************************/
PUBLIC BOOL issimpleopd (const int opdmode, treenode * const opd)
{
	switch (opdmode) {
	case P_TEMPPTR:
		return FALSE;
	case P_PTR:		/* added 30/8/90 for bug 351 CO'N */
	case P_EXP:		/*return issimple(opd); */
		return ((TagOf (opd) == T_TEMP) ? FALSE : issimple (opd));
	default:		/*case P_TEMP: */
		return TRUE;
	}
}

/*}}}*/
/*{{{  PUBLIC BOOL issimplelocalopd(opdmode, opd)       NEVER USED*/
#if 0				/* issimplelocalopd is never used */
/*****************************************************************************
 *
 *  issimplelocalopd returns TRUE if (opdmode, opd) represents a simple
 *                   local variable.
 *
 *****************************************************************************/
PUBLIC BOOL issimplelocalopd (int opdmode, treenode * opd)
{
	switch (opdmode) {
	case P_TEMP:		/* Temporaries aren't neccessarily local */
	case P_PTR:
	case P_EXP:
		return (issimplelocal (opd));
	default:		/*case P_TEMPPTR: */
		return FALSE;
	}
}
#endif /* issimplelocalopd is never used */

/*}}}*/
/*{{{  PUBLIC BOOL isaddressableopd (opdmode, opd)*/
/*****************************************************************************
 *
 *  isaddressableopd returns TRUE if (opdmode, opd) is an addressable object.
 *
 *****************************************************************************/
PUBLIC BOOL isaddressableopd (const int opdmode, treenode * const opd)
{
	switch (opdmode) {
	case P_TEMP:
	case P_TEMPPTR:
		return (TRUE);
	case P_EXP:
	case P_PTR:
		return (isaddressable (opd));
	default:
		geninternal_is (GEN_BAD_OPD, opdmode, "isaddressableopd");
		return (FALSE);	/* not reached */
	}
}

/*}}}*/
/*{{{  PUBLIC BOOL iscomplexopd (const int opdmode, treenode *const opd)*/
/*
 *	this returns TRUE if the operand is deemed to be "complex".  Currently
 *	used by KRoC for the i386 architecture, where DIV and REM are messy.
 */
PUBLIC BOOL iscomplexopd (const int opdmode, treenode *const opd)
{
	BOOL cx = FALSE;

#if 0
fprintf (stderr, "iscomplexopd(): opd = ");
printtreenl (stderr, 4, opd);
#endif
	if (opdmode == P_EXP) {
		switch (TagOf (opd)) {
		case S_DIV:
			if (kroc_complex_div) {
				cx = TRUE;
			}
			break;
		case S_REM:
			if (kroc_complex_rem) {
				cx = TRUE;
			}
			break;
		default:
			break;
		}
		if (!cx) {
			switch (nodetypeoftag (TagOf (opd))) {
			case DOPNODE:
				if (iscomplexopd (opdmode, LeftOpOf (opd))) {
					cx = TRUE;
				} else if (iscomplexopd (opdmode, RightOpOf (opd))) {
					cx = TRUE;
				}
				break;
			case MOPNODE:
				if (iscomplexopd (opdmode, OpOf (opd))) {
					cx = TRUE;
				}
				break;
			case ARRAYSUBNODE:
				if (iscomplexopd (P_EXP, ASIndexOf (opd))) {
					cx = TRUE;
				}
				break;
			default:
				break;
			}
		}
	}
	return cx;
}
/*}}}*/
/*{{{  PUBLIC BOOL preeval(mode, opd)*/
/*****************************************************************************
 *
 *  preeval returns TRUE if (mode, opd) should be preevaluated in a
 *          temporary (in this case, opd will point to a tempnode),
 *          FALSE otherwise.
 *
 *****************************************************************************/
PUBLIC BOOL preeval (int mode, treenode * opd)
{
	return (((mode == P_EXP) || (mode == P_PTR)) && (TagOf (opd) == T_TEMP));
}

/*}}}*/
/*{{{  PUBLIC BOOL hasgreaterrange(type1, type2)*/
/*****************************************************************************
 *
 *  hasgreaterrange returns TRUE if type1 has a greater range than type2,
 *                  otherwise it returns FALSE.
 *                  It does not cope with REAL types.
 *
 *****************************************************************************/
PUBLIC BOOL hasgreaterrange (int type1, int type2)
{
	if (type1 == S_INT)
		type1 = targetintsize;
	if (type2 == S_INT)
		type2 = targetintsize;
	switch (type2) {
	case S_INT64:
		return (FALSE);
	case S_INT32:
		return (type1 == S_INT64);
	case S_INT16:
		return ((type1 == S_INT64) || (type1 == S_INT32));
	case S_BYTE:
		return ((type1 != S_BYTE) && (type1 != S_BOOL));
	case S_BOOL:
		return (type1 != S_BOOL);
	default:
		badtag (genlocn, type2, "hasgreaterrange");
	}
	return (FALSE);		/* not reached */
}

/*}}}*/
/*{{{  PUBLIC BOOL issignedtype(type)*/
/*****************************************************************************
 *
 *  issignedtype returns TRUE if 'type' is a signed type
 *
 *****************************************************************************/
PUBLIC BOOL issignedtype (int type)
{
	return ((type != S_BOOL) && (type != S_BYTE));
}

/*}}}*/
/*{{{  PUBLIC BIT32 checkmask(type)*/
/*****************************************************************************
 *
 *  checkmask returns the constant to be used as operand when checking the
 *            range of integer/byte/boolean expressions
 *
 *****************************************************************************/
PUBLIC BIT32 checkmask (int type)
{
	switch (type) {
	case S_BOOL:
		return (BOOL_CHECK_MASK);
	case S_BYTE:
		return (BYTE_CHECK_MASK);
	case S_INT16:
		return (INT16_CHECK_MASK);
	case S_INT32:
		return (INT32_CHECK_MASK);
	default:
		badtag (genlocn, type, "checkmask");
		return 0;
	}
}

/*}}}*/
/*{{{  PUBLIC BIT32 typemask(type)*/
/*****************************************************************************
 *
 *  typemask returns the constant to be used as operand when and'ing out the
 *           upper part of a partword signed type.
 *
 *****************************************************************************/
PUBLIC BIT32 typemask (int type)
{
	if (type != S_INT16)
		badtag (genlocn, type, "typemask");
	return (0xffff);
}

/*}}}*/
/*{{{  PUBLIC treenode *consttableof(tptr)*/
/*****************************************************************************
 *
 * consttableof takes a S_VALABBR or S_VALRETYPE node
 *              and if it is an abbreviation of a
 *              constant table returns a pointer to the constant table node,
 *              otherwise retuns NULL.
 *
 *****************************************************************************/
PUBLIC treenode *consttableof (treenode * tptr)
{
	treenode *const t = DValOf (tptr);
	if ((TagOf (t) == S_STRING) || (TagOf (t) == S_CONSTCONSTRUCTOR))
		return (t);
	else
		return (NULL);
}

/*}}}*/
/*{{{  PUBLIC BOOL isinlibrary (nptr)*/
/*****************************************************************************
 *
 *  isinlibrary returns TRUE if the namenode nptr represents a
 *              library entry point
 *
 *****************************************************************************/
PUBLIC BOOL isinlibrary (treenode * nptr)
{
	switch (TagOf (nptr)) {
	case N_LIBPROCDEF:
	case N_LIBMPROCDECL:
	case N_LIBFUNCDEF:
	case N_STDLIBPROCDEF:
	case N_STDLIBFUNCDEF:
		return TRUE;
	default:
		return FALSE;
	}
}

/*}}}*/
/*{{{  PUBLIC void applytovalofs(tptr, p)              NEVER USED*/
#if 0				/* applytovalofs is never used */
/*{{{  comment*/
/*****************************************************************************
 *
 *  applytovalofs walks the expression tree 'tptr' looking for VALOFs.
 *                For each VALOF found, it calls the function 'p' with the
 *                VALOF tree as the parameter.
 *                N.B. we define the VALOF tree as including any leading
 *                specifications.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void applytovalofs (treenode * tptr, void (*p) ())
{
	while (tptr != NULL)
		switch (TagOf (tptr)) {
		default:
			return;
			/*{{{  monadics                        break */
		case S_NEG:
		case S_BITNOT:
		case S_SIZE:
			tptr = OpOf (tptr);
			break;
			/*}}} */
			/*{{{  conversions                     break */
		case S_EXACT:
		case S_ROUND:
		case S_TRUNC:
			tptr = OpOf (tptr);
			break;
			/*}}} */
			/*{{{  dyadics                         break */
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
		case S_COLON2:
		case S_CSUB0:
		case S_CCNT1:
		case S_EVAL:
			applytovalofs (LeftOpOf (tptr), p);
			tptr = RightOpOf (tptr);
			break;
		case S_AFTER:
			assert (FALSE);	/* after is processd in trans */
			break;

			/*}}} */
			/*{{{  constructor function call       return */
#if 0				/* no constructors are passed to the backend any more */
		case S_CONSTRUCTOR:
#endif
		case S_FINSTANCE:
			{
#if 0				/* no constructors are passed to the backend any more */
				treenode *elist;
				elist = (TagOf (tptr) == S_CONSTRUCTOR) ? LitExpOf (tptr) : IParamListOf (tptr);
#else
				treenode *elist = IParamListOf (tptr);
#endif
				for (; !EndOfList (elist); elist = NextItem (elist))
					applytovalofs (ThisItem (elist), p);
				return;
			}
			/*}}} */
			/*{{{  element subscript or segment    break */
		case S_ARRAYITEM:
		case S_RECORDITEM:
			tptr = ASExpOf (tptr);
			break;
		case S_SEGMENTITEM:
		case S_SEGMENT:
			applytovalofs (SNameOf (tptr), p);
			applytovalofs (SStartExpOf (tptr), p);
			tptr = SLengthExpOf (tptr);
			break;
			/*}}} */
			/*{{{  specification ... valof         return */
		case S_VALOF:
		case S_VALABBR:
		case S_ABBR:
		case S_VALRETYPE:
		case S_RETYPE:
		case S_TPROTDEF:
		case S_SPROTDEF:
		case S_DECL:
		case S_SFUNCDEF:
		case S_LFUNCDEF:
		case S_PROCDEF:
		case S_PRAGMA:	/* bug 829 19/9/91 */
		case S_TYPEDECL:
			(*p) (tptr);
			return;
			/*}}} */
		}
}
#endif /* applytovalofs is never used */
/*}}}*/
/*{{{  PUBLIC int ptrmodeof(mode)*/
/*****************************************************************************
 *
 *  ptrmodeof converts an operand expression mode 'mode' to a pointer mode.
 *
 *****************************************************************************/
PUBLIC int ptrmodeof (int mode)
{
	return ((mode == P_EXP) ? P_PTR : (mode == P_TEMP) ? P_TEMPPTR : mode);
}

/*}}}*/
/*{{{  PUBLIC int tempmodeof(mode)*/
/*****************************************************************************
 *
 *  tempmodeof converts an operand mode 'mode' to a temporary mode.
 *
 *****************************************************************************/
PUBLIC int tempmodeof (int mode)
{
	return ((mode == P_EXP) ? P_TEMP : (mode == P_PTR) ? P_TEMPPTR : mode);
}

/*}}}*/
/*{{{  PUBLIC BOOL usedinopd (opdmode, opd, exptree)*/
/* Check if (opdmode, opd) is used or aliased in exptree.
Cannot assume alias checking. */
/*{{{  comment on definition of 'used or aliased'*/
/*  For r := f(r) op exp :
We want to evaluate f(r) and wish to know if we can assign the result
directly into r.

We may not assign to r if:
1. r' is used directly, or an alias is used directly, in exp
2. if r' is a var. parameter and there are free variables (or other var.
parameters) in exp
Tighten this to  if r' is a var. parameter
3. if r' is a free variable and there are var. parameters
where
r' is the base decl. of r (without local aliasses)
*/
/*}}}*/
PUBLIC BOOL usedinopd (int opdmode, treenode * opd, treenode * exptree, const int my_lexlevel)
{
	switch (opdmode) {
	case P_TEMP:
		{
			/* if mode of tempory is a pointer then it may be an abbreviation */
			if (NModeOf (opd) == NM_POINTER)
				return (usedin (NDeclOf (opd), exptree, my_lexlevel));
			else
				return (FALSE);	/* A temporary cannot be needed */
		}
	case P_TEMPPTR:
		return (usedin (NDeclOf (opd), exptree, my_lexlevel));
	case P_EXP:
	case P_PTR:
		{
			treenode *n = basedecl (opd);
			if ((TagOf (n) == N_PARAM) || (TagOf (n) == N_RESULTPARAM))
				/* A var. param. is very unpredictable, so assume n is used */
				return (TRUE);
			else
				return (usedin (n, exptree, my_lexlevel));
		}
	default:
		geninternal_is (GEN_BAD_OPD, opdmode, "usedinopd");
		return (FALSE);	/* not reached */
	}
}

/*}}}*/
/*{{{  PUBLIC BOOL isconstexpnd (tptr)*/
/*****************************************************************************
 *
 *  isconstexpnd returns TRUE if tptr is a constexp tree node
 *
 *****************************************************************************/
PUBLIC BOOL isconstexpnd (treenode * tptr)
{
	return (TagOf (tptr) == S_CONSTEXP);
}

/*}}}*/
/*{{{  PUBLIC BOOL isconstopd (opdmode, opd)*/
/*****************************************************************************
 *
 *  isconstopd returns TRUE if (opdmode, opd) is constant)
 *
 *****************************************************************************/
PUBLIC BOOL isconstopd (const int opdmode, treenode *const opd)
{
	return ((opdmode == P_EXP) && isconst (opd));
}

/*}}}*/
/*{{{  PUBLIC BOOL directstore (destmode, dest)*/
/*****************************************************************************
 *
 *  directstore returns TRUE if we can store from a register directly into
 *              dest without loading a pointer first.
 *              Only ever called for word-length operands.
 *
 *****************************************************************************/
PUBLIC BOOL directstore (const int destmode, treenode *const dest, const int my_lexlevel)
{
#if 0
printf ("directstore: my_lexlevel = %d, destmode = %s, dest = ", my_lexlevel, opdmode_string (destmode));
printtree (stdout, 4, dest);
printf ("\n");
#endif
	if (issimpleopd (destmode, dest)) {
#if 0
printf ("directstore: issimpleopd (destmode, dest) TRUE\n");
#endif
		return (TRUE);
	}
	if ((destmode == P_EXP) && (TagOf (dest) == S_ARRAYITEM || TagOf (dest) == S_RECORDITEM)) {
		/* Constant subscript into local array in workspace where each element
		   of the array occupies a whole number of words */
		treenode *const nptr = nameof (dest);
#if 0
fprintf (stderr, "gen2: directstore: nptr = ");
printtreenl (stderr, 4, nptr);
#endif
		if ((nodetypeoftag (TagOf (nptr)) == NAMENODE) && (nodetypeoftag (TagOf (NTypeOf (nptr))) == TYPENODE) && (TypeAttrOf (NTypeOf (nptr)) & TypeAttr_placed)) {
			return FALSE;
		}
		if (!ispointer (nptr) && (ASExpOf (dest) == NULL) &&
		    islocal (nptr, my_lexlevel) && (bytesinscalar (basetype (gettype (dest))) >= bytesperword)) {
			return (TRUE);
		}
	}
	return (FALSE);
}

/*}}}*/
/*{{{  PUBLIC BOOL directload (sourcemode, source)*/
/*****************************************************************************
 *
 *  directload returns TRUE if we can load (sourcemode, source) into a
 *             register without loading a pointer first.
 *             Only ever called for word-length operands.
 *
 *****************************************************************************/
PUBLIC BOOL directload (const int sourcemode, treenode * const source, const int my_lexlevel)
{
	/* The algorithm is the same for loading or storing */
	return directstore (sourcemode, source, my_lexlevel);
}

/*}}}*/
/*{{{  PUBLIC BOOL multiloadaddr (treenode *const source)*/
/*****************************************************************************
 *
 *  multiload returns TRUE if loading the address of (source) is complex,
 *            i.e. requires more than one distinct load, as is the case
 *            for nested MOBILE array elements
 *
 *****************************************************************************/
PUBLIC BOOL multiloadaddr (treenode *const source)
{
#if 0
fprintf (stderr, "gen2: multiloadaddr: source = ");
printtreenl (stderr, 4, source);
#endif
	if (TagOf (source) == S_ARRAYITEM) {
		treenode *innertype = gettype (ASBaseOf (source));

#if 0
fprintf (stderr, "gen2: multiloadaddr: innertype = ");
printtreenl (stderr, 4, innertype);
#endif
		if (isdynmobilearraytype (innertype)) {
			return TRUE;
		}
	}
	return FALSE;
}
/*}}}*/
/*}}}*/
