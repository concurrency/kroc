/*
 *	transputer.h - transputer constants
 *	Copyright (C) 2000 Fred Barnes <frmb@kent.ac.uk>
 *	Based on transput.inc Copyright (C) 1997 M D Poole
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

#ifndef __TRANSPUTER_H
#define __TRANSPUTER_H


#define WSH		2		/* ws shift (x4) */
#define BytesPerWord	(1<<WSH)
#define BitsPerWord	(BytesPerWord * 8)
#define ByteSelectMask	(~(-1 << WSH))
#define WShift		2		/* x4 */
#define DWShift		3		/* x8 */


/*{{{  Primaries*/
#define I_J		0x00
#define I_LDLP		0x01
#define I_PFIX		0x02
#define I_LDNL		0x03
#define I_LDC		0x04
#define I_LDNLP		0x05
#define I_NFIX		0x06
#define I_LDL		0x07
#define I_ADC		0x08
#define I_CALL		0x09
#define I_CJ		0x0a
#define I_AJW		0x0b
#define I_EQC		0x0c
#define I_STL		0x0d
#define I_STNL		0x0e
#define I_OPR		0x0f

/*}}}*/
/*{{{  Secondaries*/
#define I_REV		0x00
#define I_LB		0x01
#define I_BSUB		0x02
#define I_ENDP		0x03
#define I_DIFF		0x04
#define I_ADD		0x05
#define I_GCALL		0x06
#define I_IN		0x07
#define I_PROD		0x08
#define I_GT		0x09
#define I_WSUB		0x0a
#define I_OUT		0x0b
#define I_SUB		0x0c
#define I_STARTP	0x0d
#define I_OUTBYTE	0x0e
#define I_OUTWORD	0x0f

#define I_SETERR	0x10
#define I_MRELEASEP	0x11
#define I_RESETCH	0x12
#define I_CSUB0		0x13
#define I_EXTVRFY	0x14
#define I_STOPP		0x15
#define I_LADD		0x16
#define I_STLB		0x17
#define I_STHF		0x18
#define I_NORM		0x19
#define I_LDIV		0x1a
#define I_LDPI		0x1b
#define I_STLF		0x1c
#define I_XDBLE		0x1d
#define I_LDPRI		0x1e
#define I_REM		0x1f

#define I_RET		0x20
#define I_LEND		0x21
#define I_LDTIMER	0x22
#define I_TESTLDS	0x23
#define I_TESTLDE	0x24
#define I_TESTLDD	0x25
#define I_TESTSTS	0x26
#define I_TESTSTE	0x27
#define I_TESTSTD	0x28
#define I_TESTERR	0x29
#define I_TESTPRANAL	0x2a
#define I_TIN		0x2b
#define I_DIV		0x2c
#define I_TESTHARDCHAN	0x2d
#define I_DIST		0x2e
#define I_DISC		0x2f

#define I_DISS		0x30
#define I_LMUL		0x31
#define I_NOT		0x32
#define I_XOR		0x33
#define I_BCNT		0x34
#define I_LSHR		0x35
#define I_LSHL		0x36
#define I_LSUM		0x37
#define I_LSUB		0x38
#define I_RUNP		0x39
#define I_XWORD		0x3a
#define I_SB		0x3b
#define I_GAJW		0x3c
#define I_SAVEL		0x3d
#define I_SAVEH		0x3e
#define I_WCNT		0x3f

#define I_SHR		0x40
#define I_SHL		0x41
#define I_MINT		0x42
#define I_ALT		0x43
#define I_ALTWT		0x44
#define I_ALTEND	0x45
#define I_AND		0x46
#define I_ENBT		0x47
#define I_ENBC		0x48
#define I_ENBS		0x49
#define I_MOVE		0x4a
#define I_OR		0x4b
#define I_CSNGL		0x4c
#define I_CCNT1		0x4d
#define I_TALT		0x4e
#define I_LDIFF		0x4f

#define I_STHB		0x50
#define I_TALTWT	0x51
#define I_SUM		0x52
#define I_MUL		0x53
#define I_STTIMER	0x54
#define I_STOPERR	0x55
#define I_CWORD		0x56
#define I_CLRHALTERR	0x57
#define I_SETHALTERR	0x58
#define I_TESTHALTERR	0x59
#define I_DUP		0x5a
#define I_MOVE2DINIT	0x5b
#define I_MOVE2DALL	0x5c
#define I_MOVE2DNONZERO	0x5d
#define I_MOVE2DZERO	0x5e
#define I_GTU		0x5f	/* T9000 */

#define	I_EXTIN		0x60
#define I_EXTOUT	0x61
#define I_MINN		0x62
#define I_UNPACKSN	0x63
/* #define I_SLMUL	0x64 */	/* T450 */
/* #define I_SULMUL	0x65 */	/* T450 */
#define I_MOUTN		0x64
#define I_XMINN		0x65
#define I_EXTENBC	0x66
#define I_EXTNDISC	0x67
/* #define I_SATADD	0x68 */	/* T450 */
/* #define I_SATSUB	0x69 */	/* T450 */
/* #define I_SATMUL	0x6a */	/* T450 */
#define I_EXTMIN	0x68
#define I_EXTMOUT	0x69
#define I_EXTMIN64	0x6a
#define I_EXTMOUT64	0x6b
#define I_POSTNORMSN	0x6c
#define I_ROUNDSN	0x6d
/* #define I_LDTRAPH	0x6e */	/* T450 */
/* #define I_STTRAPH	0x6f */	/* T450 */
#define I_EXTMINN	0x6e
#define I_EXTMOUTN	0x6f

#define I_ENBC3		0x70
#define I_LDINF		0x71
#define I_FMUL		0x72
#define I_CFLERR	0x73
#define I_CRCWORD	0x74
#define I_CRCBYTE	0x75
#define I_BITCNT	0x76
#define I_BITREVWORD	0x77
#define I_BITREVNBITS	0x78
#define I_POP		0x79
#define I_TIMERDISABLEH	0x7a
#define I_TIMERDISABLEL	0x7b
#define I_TIMERENABLEH	0x7c
#define I_TIMERENABLEL	0x7d
#define I_LDMEMSTARTVAL	0x7e
/*      I_MISSING       0x7f */

#define I_FPSTTEST	0x80
#define I_WSUBDB	0x81
#define I_FPLDNLDBI	0x82
#define I_FPCHKERR	0x83
#define I_FPSTNLDB	0x84
#define I_FPLDTEST	0x85
#define I_FPLDNLSNI	0x86
#define I_FPADD		0x87
#define I_FPSTNLSN	0x88
#define I_FPSUB		0x89
#define I_FPLDNLDB	0x8a
#define I_FPMUL		0x8b
#define I_FPDIV		0x8c
#define I_FPRANGE	0x8d
#define I_FPLDNLSN	0x8e
#define I_FPREMFIRST	0x8f

#define I_FPREMSTEP	0x90
#define I_FPNAN		0x91
#define I_FPORDERED	0x92
#define I_FPNOTFINITE	0x93
#define I_FPGT		0x94
#define I_FPEQ		0x95
#define I_FPI32TOR32	0x96
#define I_FPGE		0x97
#define I_FPI32TOR64	0x98
#define I_ENBT3		0x99
#define I_FPB32TOR64	0x9a
#define I_FPLG		0x9b
#define I_FPTESTERR	0x9c
#define I_FPRTOI32	0x9d
#define I_FPSTNLI32	0x9e
#define I_FPLDZEROSN	0x9f

#define I_FPLDZERODB	0xa0
#define I_FPINT		0xa1
#define I_GETPRI	0xa2
#define I_FPDUP		0xa3
#define I_FPREV		0xa4
#define I_SETPRI	0xa5
#define I_FPLDNLADDDB	0xa6
#define I_FPENTRY3	0xa7
#define I_FPLDNLMULDB	0xa8
#define I_FPENTRY2	0xa9
#define I_FPLDNLADDSN	0xaa
#define I_FPENTRY	0xab
#define I_FPLDNLMULSN	0xac
#define I_ENBS3		0xad
/*      I_MISSING       0xae */
/*      I_MISSING       0xaf */

#define I_SETTIMESLICE	0xb0
#define I_BREAK		0xb1
#define I_CLRJ0BREAK	0xb2
#define I_SETJ0BREAK	0xb3
#define I_TESTJ0BREAK	0xb4
/*      I_MISSING       0xb5 */
#define I_LDFLAGS	0xb6
#define I_STFLAGS	0xb7
#define I_XBWORD	0xb8
#define I_LBX		0xb9
#define I_CB		0xba
#define I_CBU		0xbb
#define I_INSPHDR	0xbc
#define I_READBFR	0xbd
#define I_LDCONF	0xbe
#define I_STCONF	0xbf

#define I_LDCNT		0xc0
#define I_SSUB		0xc1
#define I_LDTH		0xc2
#define I_LDCHSTATUS	0xc3
#define I_INTDIS	0xc4
#define I_INTENB	0xc5
#define I_LDTRAPPED	0xc6
#define I_CIR		0xc7
#define I_SS		0xc8
#define I_CHANTYPE	0xc9
#define I_LS		0xca
#define I_STTRAPPED	0xcb
#define I_CIRU		0xcc
#define I_GINTDIS	0xcd
#define I_GINTENB	0xce
#define I_FPREM		0xcf

#define I_FPRN		0xd0
#define I_FPDIVBY2	0xd1
#define I_FPMULBY2	0xd2
#define I_FPSQRT	0xd3
#define I_FPRP		0xd4
#define I_FPRM		0xd5
#define I_FPRZ		0xd6
#define I_FPR32TOR64	0xd7
#define I_FPR64TOR32	0xd8
#define I_FPEXPDEC32	0xd9
#define I_FPEXPINC32	0xda
#define I_FPABS		0xdb
/*      I_MISSING       0xdc */
#define I_FPADDDBSN	0xdd
#define I_FPCHKI32	0xde
#define I_FPCHKI64	0xdf

#define I_MNEW		0xe0
#define I_MFREE		0xe1
#define I_MALLOC	0xe2
#define I_MRELEASE	0xe3
#define I_MIN		0xe4
#define I_MOUT		0xe5
#define I_MIN64		0xe6
#define I_MOUT64	0xe7
#define I_XABLE		0xe8
#define I_XIN		0xe9
#define I_XMIN		0xea
#define I_XMIN64	0xeb
#define I_XEND		0xec
#define I_NDISC		0xed
#define I_NDIST		0xee
#define I_NDISS		0xef

#define I_DEVLB		0xf0
#define I_DEVSB		0xf1
#define I_DEVLS		0xf2
#define I_DEVSS		0xf3
#define I_DEVLW		0xf4
#define I_DEVSW		0xf5
#define I_XSWORD	0xf8
#define I_LSX		0xf9
#define I_CS		0xfa
#define I_CSU		0xfb
#define I_TRAP		0xfc
#define I_NULL		0xfd
/*      I_MISSING       0xfe */
/*      I_MISSING       0xff */

#define I_START		0x1ff
#define I_LDDEVID	0x17c

/*}}}*/

/*{{{  these are never generated by the compiler -- the pre-optimiser puts them in*/

#define I_IN8		0x200
#define I_IN32		0x201
#define I_OUT8		0x202
#define I_OUT32		0x203
#define I_XSTL		0x204		/* STL x, LDL x (keeps register) */

/*}}}*/

/*{{{  these are for NOCC code-generation*/

#define I_NCALL		0x205
#define I_NRET		0x206
#define I_NWSADJ	0x207
#define I_NSTARTP	0x208
#define I_NNEG		0x209
#define I_NLW		0x20a
#define I_NSW		0x20b
#define I_NALTEND	0x20c
#define I_NMWENB	0x20d
#define I_NMWDIS	0x20e
#define I_NMWALTWT	0x20f
#define I_NMWALT	0x210
#define I_NMWALTEND	0x211

/*}}}*/
/*{{{  extras*/

#define I_FPPOP		0x212		/* gets this as an ETC special, this one used internally */

/*}}}*/
/*{{{  new multi-way synchronisations*/

#define I_MWS_BINIT	0x213
#define I_MWS_PBRILNK	0x214
#define I_MWS_PBRULNK	0x215
#define I_MWS_PPILNK	0x216
#define I_MWS_PBENROLL	0x217
#define I_MWS_PBRESIGN	0x218
#define I_MWS_PBADJSYNC	0x219
#define I_MWS_SYNC	0x21a
#define I_MWS_ALTLOCK	0x21b
#define I_MWS_ALTUNLOCK	0x21c
#define I_MWS_ALT	0x21d
#define I_MWS_ALTEND	0x21e
#define I_MWS_ENB	0x21f
#define I_MWS_DIS	0x220
#define I_MWS_ALTPOSTLOCK	0x221
#define I_MWS_PPBASEOF	0x222
#define I_MWS_PPPAROF	0x223

/*}}}*/
/*{{{  more NOCC specials*/

#define I_IOR		0x224
#define I_IOW		0x225
#define I_IOR8		0x226
#define I_IOW8		0x227
#define I_IOR16		0x228
#define I_IOW16		0x229
#define I_IOR32		0x22a
#define I_IOW32		0x22b

#define I_ENBC2		0x22c
#define I_ENBT2		0x22d
#define I_ENBS2		0x22e

/*}}}*/

/*{{{  new (forked) process interface*/

#define I_PROC_ALLOC	0x22f
#define I_PROC_PARAM	0x230
#define I_PROC_MT_COPY	0x231
#define I_PROC_MT_MOVE	0x232
#define I_PROC_START	0x233
#define I_PROC_END	0x234

/*}}}*/
/*{{{  processor affinity instructions*/

#define I_GETAFF	0x235
#define I_SETAFF	0x236
#define I_GETPAS	0x237

/*}}}*/
/*{{{  mobile type instructions*/

#define I_MT_ALLOC	0x238
#define I_MT_RELEASE	0x239
#define I_MT_CLONE	0x23a
#define I_MT_IN		0x23b
#define I_MT_OUT	0x23c
#define I_MT_XCHG	0x23d
#define I_MT_LOCK	0x23e
#define I_MT_UNLOCK	0x23f
#define I_MT_ENROLL	0x240
#define I_MT_RESIGN	0x241
#define I_MT_SYNC	0x242
#define I_MT_XIN	0x243
#define I_MT_XOUT	0x244
#define I_MT_XXCHG	0x245
#define I_MT_DCLONE	0x246
#define I_MT_BIND	0x247

/*}}}*/
/*{{{  memory barrier instructions*/

#define I_MB		0x248
#define I_RMB		0x249
#define I_WMB		0x24a

/*}}}*/
/*{{{  placed channel mobile type instructions*/

#define I_EXT_MT_IN	0x24b
#define I_EXT_MT_OUT	0x24c

/*}}}*/
/*{{{  (more) mobile type instructions*/

#define I_MT_RESIZE	0x24d

/*}}}*/
/*{{{  additional NOCC specials*/
/* FIXME: we really ought to reorder these sometime.. */

#define I_NJTABLE	0x24e
#define I_NLABADDR	0x24f
#define I_NJCSUB0	0x250

#define I_XSTLN		0x251		/* STL -x, LDL -x (keeps register) */

/*}}}*/
/*{{{  negative instructions (mostly T9000)*/
#define I_NEGATIVE	0x8000
#define I_FPSTALL	(0x01 | I_NEGATIVE)
#define I_FPLDALL	(0x02 | I_NEGATIVE)
#define I_STSHADOW	(0x03 | I_NEGATIVE)
#define I_LDSHADOW	(0x04 | I_NEGATIVE)
#define I_TRET		(0x05 | I_NEGATIVE)
#define I_GOPROT	(0x06 | I_NEGATIVE)
#define I_SELTH		(0x07 | I_NEGATIVE)
#define I_SYSCALL	(0x08 | I_NEGATIVE)
#define I_TRAPENB	(0x09 | I_NEGATIVE)
#define I_TRAPDIS	(0x0a | I_NEGATIVE)
#define I_WAIT		(0x0b | I_NEGATIVE)
#define I_SIGNAL	(0x0c | I_NEGATIVE)
#define I_TIMESLICE	(0x0d | I_NEGATIVE)
#define I_INSERTQUEUE	(0x0e | I_NEGATIVE)
#define I_SWAPTIMER	(0x0f | I_NEGATIVE)
#define I_SWAPQUEUE	(0x10 | I_NEGATIVE)
#define I_IRET		(0x11 | I_NEGATIVE)
#define I_STOPCH	(0x12 | I_NEGATIVE)
#define I_VOUT		(0x13 | I_NEGATIVE)
#define I_VIN		(0x14 | I_NEGATIVE)
#define I_STVLCB	(0x15 | I_NEGATIVE)
#define I_LDVLCB	(0x16 | I_NEGATIVE)
#define I_SWAPBFR	(0x17 | I_NEGATIVE)
#define I_SETHDR	(0x18 | I_NEGATIVE)
#define I_SETCHMODE	(0x19 | I_NEGATIVE)
#define I_INITVLCB	(0x1a | I_NEGATIVE)
#define I_WRITEHDR	(0x1b | I_NEGATIVE)
#define I_READHDR	(0x1c | I_NEGATIVE)
#define I_DISG		(0x1d | I_NEGATIVE)
#define I_ENBG		(0x1e | I_NEGATIVE)
#define I_GRANT		(0x1f | I_NEGATIVE)
#define I_STMOVE2DINIT	(0x20 | I_NEGATIVE)
#define I_CAUSEERROR	(0x21 | I_NEGATIVE)
#define I_RESTART	(0x22 | I_NEGATIVE)
#define I_UNMKRC	(0x23 | I_NEGATIVE)
#define I_MKRC		(0x24 | I_NEGATIVE)
#define I_IRDSQ		(0x25 | I_NEGATIVE)
#define I_ERDSQ		(0x26 | I_NEGATIVE)
#define I_STRESPTR	(0x27 | I_NEGATIVE)
#define I_LDRESPTR	(0x28 | I_NEGATIVE)
#define I_DEVMOVE	(0x2c | I_NEGATIVE)
#define I_ICL		(0x2d | I_NEGATIVE)
#define I_FDCL		(0x2e | I_NEGATIVE)
#define I_ICA		(0x2f | I_NEGATIVE)
#define I_FDCA		(0x30 | I_NEGATIVE)
#define I_NOP		(0x40 | I_NEGATIVE)
#define I_CLOCKENB	(0x41 | I_NEGATIVE)
#define I_CLOCKDIS	(0x42 | I_NEGATIVE)
#define I_LDCLOCK	(0x43 | I_NEGATIVE)
#define I_STCLOCK	(0x44 | I_NEGATIVE)
#define I_LDPRODID	(0x84 | I_NEGATIVE)

/*}}}*/
/*{{{  pseudo ops.*/
#define I_PSEUDO_OP	(0x40000)
#define I_R32SIN	(0x01 | I_PSEUDO_OP)
#define I_R64SIN	(0x02 | I_PSEUDO_OP)
#define I_R32COS	(0x03 | I_PSEUDO_OP)
#define I_R64COS	(0x04 | I_PSEUDO_OP)
#define I_R32TAN	(0x05 | I_PSEUDO_OP)
#define I_R64TAN	(0x06 | I_PSEUDO_OP)

/*}}}*/

/*{{{  constants and workspace offsets*/

#define CONST_INFINITY	0x7f800000


#ifdef PROCESS_PRIORITY
	/* priorities shove these (post Iptr) down a slot */
	#define W_TIME		((-6) << WSH)
	#define W_TLINK		((-5) << WSH)
	#define W_STATUS	((-4) << WSH)
	#define W_POINTER	((-4) << WSH)
	#define W_PRIORITY	((-3) << WSH)
#else	/* !PROCESS_PRIORITY */
	#define W_TIME		((-5) << WSH)
	#define W_TLINK		((-4) << WSH)
	#define W_STATUS	((-3) << WSH)
	#define W_POINTER	((-3) << WSH)
#endif	/* !PROCESS_PRIORITY */
#define W_LINK		((-2) << WSH)
#define W_IPTR		((-1) << WSH)
#define W_TEMP		(0)

#define W_COUNT		(1 << WSH)
#define W_IPTRSUCC	(0)

#define Z_ENABLING	(NOT_PROCESS + 1)
#define Z_WAITING	(NOT_PROCESS + 2)
#define Z_READY		(NOT_PROCESS + 3)
#define Z_BUFFERED	(NOT_PROCESS + 4)
#define Z_TIMESET	(NOT_PROCESS + 1)
#define Z_TIMENOTSET	(NOT_PROCESS + 2)
#define NONE_SELECTED	0x80000000

/*}}}*/

#ifdef __TSTACK_C
/*{{{  primary adjustments*/
static int tsdiff_prim[] = { \
	16, 1, 0, 0,	/* 00: j, ldlp, pfix, ldnl */
	1, 0, 0, 1,	/* 04: ldc, ldnlp, nfix, ldl */
	0, 16, -1, 0, 	/* 08: adc, call, cj, ajw */
	0, -1, -2, 0	/* 0c: eqc, stl, stnl, opr */
};

/*}}}*/
/*{{{  secondary adjustments*/
static int tsdiff_sec[] = { \
	0, 0, -1, 16,	/* 00: rev, lb, bsub, endp */
	-1, -1, 0, 16,	/* 04: diff, add, gcall, in */
	-1, -1, -1, 16,	/* 08: prod, gt, wsub, out */
	-1, 16, 16, 16,	/* 0c: sub, startp, outbyte, outword */
	0, 16, 0, -1,	/* 10: seterr, mreleasep, resetch, csub0 */
	-2, 16, -2, -1,	/* 14: extvrfy, stopp, ladd, stlb */
#ifdef PROCESS_PRIORITY
	-2, 1, -1, 0,	/* 18: sthf, norm, ldiv, ldpi */
#else
	-1, 1, -1, 0,	/* 18: sthf, norm, ldiv, ldpi */
#endif
	-1, 1, 1, -1,	/* 1c: stlf, xdble, ldpri, rem */
	16, 16, 1, 0,	/* 20: ret, lend, ldtimer, . */
	0, 0, 0, 0,	/* 24: ., ., ., . */
	0, 1, 1, 16,	/* 28: ., testerr, testpranal, tin */
	-1, 0, -2, -2,	/* 2c: div, ., dist, disc */
	-1, -1, 0, -1,	/* 30: diss, lmul, not, xor */
	0, -1, -1, -1,	/* 34: bcnt, lshr, lshl, lsum */
	-2, 0, -1, -2,	/* 38: lsub, runp, xword, sb */
#ifdef PROCESS_PRIORITY
	0, -1, -2, 1,	/* 3c: gajw, savel, saveh, wcnt */
#else
	0, -1, -1, 1,	/* 3c: gajw, savel, saveh, wcnt */
#endif
	-1, -1, 1, 0,	/* 40: shr, shl, mint, alt */
	16, 0, -1, -1,	/* 44: altwt, altend, and, enbt */
	-1, 0, -3, -1,	/* 48: enbc, enbs, move, or */
	-1, -1, 0, -1,	/* 4c: csngl, ccnt1, talt, ldiff */
#ifdef PROCESS_PRIORITY
	-2, 16, -1, -1,	/* 50: sthb, taltwt, sum, mul */
#else
	-1, 16, -1, -1,	/* 50: sthb, taltwt, sum, mul */
#endif
	-1, 0, -1, 0,	/* 54: sttimer, stoperr, cword, clrhalterr */
	0, 1, 1, 16,	/* 58: sethalterr, testhalterr, dup, move2dinit */
	16, 16, 16, 0,	/* 5c: move2dall, move2dnonzero, move2dzero, . */
	16, 16, 16, 19,	/* 60: extin, extout, minn, unpacksn */
	16, 16, -1, -2,	/* 64: moutn, xminn, extenbc, extndisc */
	16, 16, 16, 16,	/* 68: extmin, extmout, extmin64, extmout64 */
	19, -2, 16, 16,	/* 6c: postnormsn, roundsn, extminn, extmoutn */
	-2, 0, -1, 0,	/* 70: enbc3, ldinf, fmul, cflerr */
	-1, -1, -1, 0,	/* 74: crcword, crcbyte, bitcnt, bitrevword */
	-1, -1, 0, 0,	/* 78: bitrevnbits, pop, ., . */
	0, 0, 0, 0,	/* 7c: ., ., ., . */
	0, -1, -2, 0,	/* 80: ., wsubdb, fpldnldbi, fpchkerr */
	-1, 0, -2, 0,	/* 84: fpstnldb, ., fpldnlsni, fpadd */
	-1, 0, -1, 0,	/* 88: fpstnlsn, fpsub, fpldnldb, fpmul */
	0, 0, -1, 1,	/* 8c: fpdiv, ., fpldnlsn, fpremfirst */
	1, 1, 1, 1,	/* 90: fpremstep, fpnan, fpordered, fpnotfinite */
	1, 1, -1, 0,	/* 94: fpgt, fpeq, fpi32tor32, . */
	-1, -2, -1, 0,	/* 98: fpi32tor64, enbt3, fpb32tor64, . */
	1, 0, -1, 0,	/* 9c: fptesterr, fprtoi32, fpstnli32, fpldzerosn */
	0, 0, 1, 0,	/* a0: fpldzerodb, fpint, getpri, fpdup */
	0, 0, -1, 0,	/* a4: fprev, ., fpldnladddb, . */
	-1, 16, -1, -1,	/* a8: fpldnlmuldb, setpri, fpldnladdsn, fpentry */
	-1, -1, 0, 0	/* ac: fpldnlmulsn, enbs3, ., . */
};
/*}}}*/
/*{{{  secondary adjustments for 0xEx operands*/
static int tsdiff_sec_ecodes[] = { \
	0, -1, 0, -1,	/* e0: mnew, mfree, malloc, mrelease */
	16, 16, 16, 16,	/* e4: min, mout, min64, mout64 */
	16, 16, 16, 16,	/* e8: xable, xin, xmin, xmin64 */
	-1, -2, -2, -1	/* ec: xend, ndisc, ndist, ndiss */
};


/*}}}*/
/*{{{  secondary adjustments for 0x2xx operands*/

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
	-3, -3, -3, 16,	/* 230: proc_param, proc_mt_copy, proc_mt_move, proc_start */
	16, 1, 16, 1,	/* 234: proc_end, getaff, setaff, getpas */
	-1, -1, 0, 16,	/* 238: mt_alloc, mt_release, mt_clone, mt_in */
	16, 16, 16, 16,	/* 23c: mt_out, mt_xchg, mt_lock, mt_unlock */
	-2, -2,	16, 16,	/* 240: mt_enroll, mt_resign, mt_sync, mt_xin */
	16, 16, -2, -2,	/* 244: mt_xout, mt_xxchg, mt_dclone, mt_bind */
	0, 0, 0, 16,	/* 248: mb, rmb, wmb, ext_mt_in */
	16, -2, -1, 0,	/* 24c: ext_mt_out, mt_resize, njtable, nlabaddr */
	-2, 0		/* 250: njcsub0, xstln */
};

/*}}}*/
#endif /* __TSTACK_C */

#endif	/* !__TRANSPUTER_H */

