/* $Id: instruct.h,v 1.1 1996/04/15 10:52:11 djb1 Exp $ */

/*
 *	Instruction numbers
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


/*{{{  masks and other bits */
#define INST_MASK       0x0ffff /* enough for the 'step' stuff */
#define I_NEGATIVE      0x08000 /* Not masked off by the INST_MASK */
#define I_STEP_BIT      0x00800 /* Microcode 'step' bit */
#define I_PRIMARY       0x10000
#define I_FPU_ENTRY_BIT 0x20000
#define I_PSEUDO_OP     0x40000
/*}}}*/

/*{{{  primary instructions */
#define I_J       0x00
#define I_LDLP    0x10
#define I_PFIX    0x20
#define I_LDNL    0x30
#define I_LDC     0x40
#define I_LDNLP   0x50
#define I_NFIX    0x60
#define I_LDL     0x70
#define I_ADC     0x80
#define I_CALL    0x90
#define I_CJ      0xA0
#define I_AJW     0xB0
#define I_EQC     0xC0
#define I_STL     0xD0
#define I_STNL    0xE0
#define I_OPR     0xF0
/*}}}*/
/*{{{  secondary instructions */
/*{{{  0x */
#define I_REV         0x00
#define I_LB          0x01
#define I_BSUB        0x02
#define I_ENDP        0x03
#define I_DIFF        0x04
#define I_ADD         0x05
#define I_GCALL       0x06
#define I_IN          0x07
#define I_PROD        0x08
#define I_GT          0x09
#define I_WSUB        0x0A
#define I_OUT         0x0B
#define I_SUB         0x0C
#define I_STARTP      0x0D
#define I_OUTBYTE     0x0E
#define I_OUTWORD     0x0F

/*}}}*/
/*{{{  1x */
#define I_SETERR      0x10
#define I_MRELEASEP   0x11	/* used to release the workspace of a process (and end it) */
#define I_RESETCH     0x12
#define I_CSUB0       0x13
#define I_EXTVRFY     0x14
#define I_STOPP       0x15
#define I_LADD        0x16
#define I_STLB        0x17
#define I_STHF        0x18
#define I_NORM        0x19
#define I_LDIV        0x1A
#define I_LDPI        0x1B
#define I_STLF        0x1C
#define I_XDBLE       0x1D
#define I_LDPRI       0x1E
#define I_REM         0x1F

/*}}}*/
/*{{{  2x */
#define I_RET         0x20
#define I_LEND        0x21
#define I_LDTIMER     0x22
#define I_TESTLDS     0x23
#define I_TESTLDE     0x24
#define I_TESTLDD     0x25
#define I_TESTSTS     0x26
#define I_TESTSTE     0x27
#define I_TESTSTD     0x28
#define I_TESTERR     0x29
#define I_TESTPRANAL  0x2A
#define I_TIN         0x2B
#define I_DIV         0x2C
#define I_TESTHARDCHAN 0x2D
#define I_DIST        0x2E
#define I_DISC        0x2F

/*}}}*/
/*{{{  3x */
#define I_DISS        0x30
#define I_LMUL        0x31
#define I_NOT         0x32
#define I_XOR         0x33
#define I_BCNT        0x34
#define I_LSHR        0x35
#define I_LSHL        0x36
#define I_LSUM        0x37
#define I_LSUB        0x38
#define I_RUNP        0x39
#define I_XWORD       0x3A
#define I_SB          0x3B
#define I_GAJW        0x3C
#define I_SAVEL       0x3D
#define I_SAVEH       0x3E
#define I_WCNT        0x3F

/*}}}*/
/*{{{  4x */
#define I_SHR         0x40
#define I_SHL         0x41
#define I_MINT        0x42
#define I_ALT         0x43
#define I_ALTWT       0x44
#define I_ALTEND      0x45
#define I_AND         0x46
#define I_ENBT        0x47
#define I_ENBC        0x48
#define I_ENBS        0x49
#define I_MOVE        0x4A
#define I_OR          0x4B
#define I_CSNGL       0x4C
#define I_CCNT1       0x4D
#define I_TALT        0x4E
#define I_LDIFF       0x4F

/*}}}*/
/*{{{  5x */
#define I_STHB        0x50
#define I_TALTWT      0x51
#define I_SUM         0x52
#define I_MUL         0x53
#define I_STTIMER     0x54
#define I_STOPERR     0x55
#define I_CWORD       0x56
#define I_CLRHALTERR  0x57
#define I_SETHALTERR  0x58
#define I_TESTHALTERR 0x59
#define I_DUP         0x5A
#define I_MOVE2DINIT  0x5B
#define I_MOVE2DALL   0x5C
#define I_MOVE2DNONZERO 0x5D
#define I_MOVE2DZERO  0x5E
#define I_GTU         0x5F /* T9000 */

/*}}}*/
/*{{{  6x */
/*{{{  added by frmb -- for "special" channel handling */
#define I_EXTIN       0x60
#define I_EXTOUT      0x61
/*}}}  */
/* #define I_MINN        0x62 */
#define I_UNPACKSN    0x63
#define I_SLMUL       0x64 /* T450 */
#define I_SULMUL      0x65 /* T450 */
/* #define I_MOUTN       0x64 */
/* #define I_XMINN       0x65 */
/*{{{  added by frmb -- for "special" channel handling */
#define I_EXTENBC     0x66
#define I_EXTNDISC    0x67
/*}}}  */
#define I_SATADD      0x68 /* T450 */
#define I_SATSUB      0x69 /* T450 */
#define I_SATMUL      0x6A /* T450 */
/*{{{  added by frmb -- more "special" channel handling */
/* #define I_EXTMIN      0x68 */
/* #define I_EXTMOUT     0x69 */
/* #define I_EXTMIN64    0x6A */
/* #define I_EXTMOUT64   0x6B */
/*}}}  */
#define I_POSTNORMSN  0x6C
#define I_ROUNDSN     0x6D
#define I_LDTRAPH     0x6E /* T450 */
#define I_STTRAPH     0x6F /* T450 */
/*{{{  added by frmb -- more "special" channel handling */
/* #define I_EXTMINN    0x6E */
/* #define I_EXTMOUTN   0x6F */
/*}}}  */

/*}}}*/
/*{{{  7x */
#define I_ENBC3       0x70
#define I_LDINF       0x71
#define I_FMUL        0x72
#define I_CFLERR      0x73
#define I_CRCWORD     0x74
#define I_CRCBYTE     0x75
#define I_BITCNT      0x76
#define I_BITREVWORD  0x77
#define I_BITREVNBITS 0x78
#define I_POP          0x79
#define I_TIMERDISABLEH 0x7A
#define I_TIMERDISABLEL 0x7B
#define I_TIMERENABLEH  0x7C
#define I_TIMERENABLEL  0x7D
#define I_LDMEMSTARTVAL 0x7E

/*}}}*/
/*{{{  8x */
#define I_FPSTTEST    0x80
#define I_WSUBDB      0x81
#define I_FPLDNLDBI   0x82
#define I_FPCHKERR    0x83
#define I_FPSTNLDB    0x84
#define I_FPLDTEST    0x85
#define I_FPLDNLSNI   0x86
#define I_FPADD       0x87
#define I_FPSTNLSN    0x88
#define I_FPSUB       0x89
#define I_FPLDNLDB    0x8A
#define I_FPMUL       0x8B
#define I_FPDIV       0x8C
#define I_FPRANGE     0x8D /* T9000 */
#define I_FPLDNLSN    0x8E
#define I_FPREMFIRST  0x8F

/*}}}*/
/*{{{  9x */
#define I_FPREMSTEP   0x90
#define I_FPNAN       0x91
#define I_FPORDERED   0x92
#define I_FPNOTFINITE 0x93
#define I_FPGT        0x94
#define I_FPEQ        0x95
#define I_FPI32TOR32  0x96
#define I_FPGE        0x97 /* T9000 */
#define I_FPI32TOR64  0x98
#define I_ENBT3       0x99 /* added by frmb */
#define I_FPB32TOR64  0x9A
#define I_FPLG        0x9B /* T9000 */
#define I_FPTESTERR   0x9C
#define I_FPRTOI32    0x9D
#define I_FPSTNLI32   0x9E
#define I_FPLDZEROSN  0x9F

/*}}}*/
/*{{{  Ax */
#define I_FPLDZERODB  0xA0
#define I_FPINT       0xA1
#define I_GETPRI      0xA2
#define I_FPDUP       0xA3
#define I_FPREV       0xA4
#define I_SETPRI      0xA5
#define I_FPLDNLADDDB 0xA6
#define I_FPENTRY3    0xA7
#define I_FPLDNLMULDB 0xA8
#define I_FPENTRY2    0xA9
#define I_FPLDNLADDSN 0xAA
#define I_FPENTRY     0xAB
#define I_FPLDNLMULSN 0xAC
#define I_ENBS3       0xAD

/*}}}*/
/*{{{  Bx */
#define I_SETTIMESLICE 0xB0 /* T9000 */
#define I_BREAK       0xB1
#define I_CLRJ0BREAK  0xB2
#define I_SETJ0BREAK  0xB3
#define I_TESTJ0BREAK 0xB4
/*#define I_LDPROC    0xB5*/ /* T9000 */ /* removed bug TS/1539 07/04/92 */
#define I_LDFLAGS     0xB6 /* T9000 */
#define I_STFLAGS     0xB7 /* T9000 */
#define I_XBWORD      0xB8 /* T9000 */
#define I_LBX         0xB9 /* T9000 */
#define I_CB          0xBA /* T9000 */
#define I_CBU         0xBB /* T9000 */
#define I_INSPHDR     0xBC /* T9000 */
#define I_READBFR     0xBD /* T9000 */
#define I_LDCONF      0xBE /* T9000 */
#define I_STCONF      0xBF /* T9000 */

/*}}}*/
/*{{{  Cx */
#define I_LDCNT       0xC0 /* T9000 */
#define I_SSUB        0xC1 /* T9000 */
#define I_LDTH        0xC2 /* T9000 */
#define I_LDCHSTATUS  0xC3 /* T9000 */
#define I_INTDIS      0xC4 /* T9000 */
#define I_INTENB      0xC5 /* T9000 */
#define I_LDTRAPPED   0xC6 /* T450  */
#define I_CIR         0xC7 /* T9000 */
#define I_SS          0xC8 /* T9000 */
#define I_CHANTYPE    0xC9 /* T9000 */
#define I_LS          0xCA /* T9000 */
/*#define I_FPSETERR    0xCB*/ /* T9000 */ /* bug TS/1547 12/05/92 */
#define I_STTRAPPED   0xCB /* T450 */ 
#define I_CIRU        0xCC /* T9000 */
#define  I_GINTDIS     0xCD /* T450 */
#define  I_GINTENB     0xCE /* T450 */
#define I_FPREM       0xCF /* T9000 */

/*}}}*/
/*{{{  Dx */
#define I_FPRN        0xD0 /* T9000 */
#define I_FPDIVBY2    0xD1 /* T9000 */
#define I_FPMULBY2    0xD2 /* T9000 */
#define I_FPSQRT      0xD3 /* T9000 */
#define I_FPRP        0xD4 /* T9000 */
#define I_FPRM        0xD5 /* T9000 */
#define I_FPRZ        0xD6 /* T9000 */
#define I_FPR32TOR64  0xD7 /* T9000 */
#define I_FPR64TOR32  0xD8 /* T9000 */
#define I_FPEXPDEC32  0xD9 /* T9000 */
#define I_FPEXPINC32  0xDA /* T9000 */
#define I_FPABS       0xDB /* T9000 */
/*#define I_FPCLRERR    0xDC*/ /* T9000 */ /* bug TS/1547 12/05/92 */
/*#define I_FPNOROUND   0xDD*/ /* replaced with FPADDDBSN 24/6/91 */
#define I_FPADDDBSN   0xDD     /* new 24/6/91 */ /* T9000 */
#define I_FPCHKI32    0xDE /* T9000 */
#define I_FPCHKI64    0xDF /* T9000 */

/*}}}*/
/*{{{  Ex */
/* #define I_MNEW	0xE0 */
/* #define I_MFREE	0xE1 */
#define I_MALLOC        0xE2
#define I_MRELEASE      0xE3
/* #define I_MIN	0xE4 */
/* #define I_MOUT	0xE5 */
/* #define I_MIN64	0xE6 */
/* #define I_MOUT64	0xE7 */
#define I_XABLE	        0xE8
#define I_XIN	        0xE9
/* #define I_XMIN	0xEA */
/* #define I_XMIN64	0xEB */
#define I_XEND          0xEC
#define I_NDISC         0xED
#define I_NDIST         0xEE
#define I_NDISS         0xEF

/*}}}*/
/*{{{  Fx */
#define I_DEVLB       0xF0 /* T9000 */ /* added 27/11/91 */
#define I_DEVSB       0xF1 /* T9000 */ /* added 27/11/91 */
#define I_DEVLS       0xF2 /* T9000 */ /* added 27/11/91 */
#define I_DEVSS       0xF3 /* T9000 */ /* added 27/11/91 */
#define I_DEVLW       0xF4 /* T9000 */ /* added 27/11/91 */
#define I_DEVSW       0xF5 /* T9000 */ /* added 27/11/91 */
#define I_XSWORD      0xF8 /* T9000 */
#define I_LSX         0xF9 /* T9000 */
#define I_CS          0xFA /* T9000 */
#define I_CSU         0xFB /* T9000 */
#define I_TRAP        0xFC /* KRoC break */
#define I_NULL        0xFD /* Load NULL (as opposed to MINT) */

/*}}}*/
/*{{{  1xx */
#define I_START        0x1FF
#define I_LDDEVID      0x17C
/*}}}*/
/*{{{  2xx */
#define I_IOR          0x224
#define I_IOW          0x225
#define I_IOR8         0x226
#define I_IOW8         0x227
#define I_IOR16        0x228
#define I_IOW16        0x229
#define I_IOR32        0x22A
#define I_IOW32        0x22B
#define I_ENBC2        0x22C
#define I_ENBT2        0x22D
#define I_ENBS2        0x22E

#define I_PROC_ALLOC   0x22F
#define I_PROC_PARAM   0x230
#define I_PROC_MT_COPY 0x231
#define I_PROC_MT_MOVE 0x232
#define I_PROC_START   0x233
#define I_PROC_END     0x234
#define I_GETAFF       0x235
#define I_SETAFF       0x236
#define I_GETPAS       0x237
#define I_MT_ALLOC     0x238
#define I_MT_RELEASE   0x239
#define I_MT_CLONE     0x23A
#define I_MT_IN        0x23B
#define I_MT_OUT       0x23C
#define I_MT_XCHG      0x23D
#define I_MT_LOCK      0x23E
#define I_MT_UNLOCK    0x23F
#define I_MT_ENROLL    0x240
#define I_MT_RESIGN    0x241
#define I_MT_SYNC      0x242
#define I_MT_XIN       0x243
#define I_MT_XOUT      0x244
#define I_MT_XXCHG     0x245
#define I_MT_DCLONE    0x246
#define I_MT_BIND      0x247

#define I_MB           0x248
#define I_RMB          0x249
#define I_WMB          0x24A

#define I_EXT_MT_IN    0x24B
#define I_EXT_MT_OUT   0x24C

#define I_MT_RESIZE    0x24D
/*}}}*/
/*{{{  negatives */
#define I_FPSTALL     ( 0x01 | I_NEGATIVE) /* T9000 */
#define I_FPLDALL     ( 0x02 | I_NEGATIVE) /* T9000 */
#define I_STSHADOW    ( 0x03 | I_NEGATIVE) /* was SAVESHADOW */ /* T9000 */
#define I_LDSHADOW    ( 0x04 | I_NEGATIVE) /* was LOADSHADOW */ /* T9000 */
#define I_TRET        ( 0x05 | I_NEGATIVE) /* T9000 */
#define I_GOPROT      ( 0x06 | I_NEGATIVE) /* was GOPROTECTED */ /* T9000 */
#define I_SELTH       ( 0x07 | I_NEGATIVE) /* was SELTRAPHANDLER */ /* T9000 */
#define I_SYSCALL     ( 0x08 | I_NEGATIVE) /* T9000 */
/*#define I_SWAPGSTATUS ( 0x09 | I_NEGATIVE)*/ /* T9000 */ /* removed bug TS/1539 07/04/92 */
#define I_TRAPENB     ( 0x09 | I_NEGATIVE) /* T450 */
#define I_TRAPDIS     ( 0x0a | I_NEGATIVE) /* T450 */
#define I_WAIT        ( 0x0B | I_NEGATIVE) /* T9000 */
#define I_SIGNAL      ( 0x0C | I_NEGATIVE) /* T9000 */
#define I_TIMESLICE   ( 0x0D | I_NEGATIVE) /* T9000 */
#define I_INSERTQUEUE ( 0x0E | I_NEGATIVE) /* T9000 */
#define I_SWAPTIMER   ( 0x0F | I_NEGATIVE) /* T9000 */
#define I_SWAPQUEUE   ( 0x10 | I_NEGATIVE) /* T9000 */
#define I_IRET        ( 0x11 | I_NEGATIVE) /* T450  */
#define I_STOPCH      ( 0x12 | I_NEGATIVE) /* T9000 */
#define I_VOUT        ( 0x13 | I_NEGATIVE) /* T9000 */
#define I_VIN         ( 0x14 | I_NEGATIVE) /* T9000 */
#define I_STVLCB      ( 0x15 | I_NEGATIVE) /* T9000 */
#define I_LDVLCB      ( 0x16 | I_NEGATIVE) /* T9000 */
#define I_SWAPBFR     ( 0x17 | I_NEGATIVE) /* T9000 */
#define I_SETHDR      ( 0x18 | I_NEGATIVE) /* T9000 */
#define I_SETCHMODE   ( 0x19 | I_NEGATIVE) /* T9000 */
#define I_INITVLCB    ( 0x1A | I_NEGATIVE) /* T9000 */
#define I_WRITEHDR    ( 0x1B | I_NEGATIVE) /* T9000 */
#define I_READHDR     ( 0x1C | I_NEGATIVE) /* T9000 */
#define I_DISG        ( 0x1D | I_NEGATIVE) /* T9000 */
#define I_ENBG        ( 0x1E | I_NEGATIVE) /* T9000 */
#define I_GRANT       ( 0x1F | I_NEGATIVE) /* T9000 */
#define I_STMOVE2DINIT (0x20 | I_NEGATIVE) /* T9000 */
/*#define I_CLAIM     ( 0x20 | I_NEGATIVE)*/ /* removed 13/12/90 */ /* T9000 */
#define I_CAUSEERROR  ( 0x21 | I_NEGATIVE) /* added 22/01/92 */ /* bug TS/1547 */ /* T9000 */
#define I_RESTART     ( 0x22 | I_NEGATIVE) /* added 19/03/92 */ /* bug TS/1547 */ /* T9000 */
#define I_UNMKRC      ( 0x23 | I_NEGATIVE) /* T9000 */
#define I_MKRC        ( 0x24 | I_NEGATIVE) /* T9000 */
#define I_IRDSQ       ( 0x25 | I_NEGATIVE) /* T9000 */
#define I_ERDSQ       ( 0x26 | I_NEGATIVE) /* T9000 */
#define I_STRESPTR    ( 0x27 | I_NEGATIVE) /* T9000 */
#define I_LDRESPTR    ( 0x28 | I_NEGATIVE) /* T9000 */
/*#define I_WRRESPTR  ( 0x29 | I_NEGATIVE)*/ /* removed 13/12/90 */ /* T9000 */
/*#define I_TESTWRTAG ( 0x29 | I_NEGATIVE)*/ /* removed 13/12/90 */ /* T9000 */
/*#define I_RDRESPTR  ( 0x2A | I_NEGATIVE)*/ /* removed 13/12/90 */ /* T9000 */
/*#define I_TESTRDTAG ( 0x2A | I_NEGATIVE)*/ /* removed 13/12/90 */ /* T9000 */
/*#define I_TESTFLINE ( 0x2B | I_NEGATIVE)*/ /* removed 13/12/90 */ /* T9000 */
/*#define I_TESTULINE ( 0x2C | I_NEGATIVE)*/ /* removed 13/12/90 */ /* T9000 */
#define I_DEVMOVE     ( 0x2C | I_NEGATIVE) /* T9000 */ /* added 27/11/91 */
#define I_ICL         ( 0x2D | I_NEGATIVE) /* T9000 */
/*#define I_TESTUPREP ( 0x2D | I_NEGATIVE)*/ /* removed 13/12/90 */ /* T9000 */
#define I_FDCL        ( 0x2E | I_NEGATIVE) /* T9000 */
#define I_ICA         ( 0x2F | I_NEGATIVE) /* was ICB */ /* T9000 */
#define I_FDCA        ( 0x30 | I_NEGATIVE) /* was FDCB */ /* T9000 */
#define I_NOP         ( 0x40 | I_NEGATIVE) /* added 08/11/91 */ /* T9000 */
#define I_CLOCKENB    ( 0x41 | I_NEGATIVE) /* T450 */
#define I_CLOCKDIS    ( 0x42 | I_NEGATIVE) /* T450 */
#define I_LDCLOCK     ( 0x43 | I_NEGATIVE) /* T450 */ 
#define I_STCLOCK     ( 0x44 | I_NEGATIVE) /* T450 */
#define I_LDPRODID    ( 0x84 | I_NEGATIVE) /* added 22/01/92 */ /* bug TS/1536 */ /* T9000 */
/*#define I_TESTDELAY ( 0x90 | I_NEGATIVE)*/ /* added 17/09/92 */ /* bug TS/1874 */
#define I_WSMAP       ( 0x91 | I_NEGATIVE) /* KRoC */
#define I_CODEMAP     ( 0x92 | I_NEGATIVE) /* KRoC */
/*}}}*/

/*}}}*/
/*{{{  fpu entry codes */
#define I_FPUSQRTFIRST (0x01 | I_FPU_ENTRY_BIT)
#define I_FPUSQRTSTEP  (0x02 | I_FPU_ENTRY_BIT)
#define I_FPUSQRTLAST  (0x03 | I_FPU_ENTRY_BIT)
#define I_FPURP        (0x04 | I_FPU_ENTRY_BIT)
#define I_FPURM        (0x05 | I_FPU_ENTRY_BIT)
#define I_FPURZ        (0x06 | I_FPU_ENTRY_BIT)
#define I_FPUR32TOR64  (0x07 | I_FPU_ENTRY_BIT)
#define I_FPUR64TOR32  (0x08 | I_FPU_ENTRY_BIT)
#define I_FPUEXPDEC32  (0x09 | I_FPU_ENTRY_BIT)
#define I_FPUEXPINC32  (0x0A | I_FPU_ENTRY_BIT)
#define I_FPUABS       (0x0B | I_FPU_ENTRY_BIT)
#define I_FPUNOROUND   (0x0D | I_FPU_ENTRY_BIT)
#define I_FPUCHKI32    (0x0E | I_FPU_ENTRY_BIT)
#define I_FPUCHKI64    (0x0F | I_FPU_ENTRY_BIT)
#define I_FPUDIVBY2    (0x11 | I_FPU_ENTRY_BIT)
#define I_FPUMULBY2    (0x12 | I_FPU_ENTRY_BIT)
#define I_FPURN        (0x22 | I_FPU_ENTRY_BIT)
#define I_FPUSETERR    (0x23 | I_FPU_ENTRY_BIT)
#define I_FPUCLRERR    (0x9C | I_FPU_ENTRY_BIT)

/*}}}*/
/*{{{  FPU pseudo-ops*/
#define I_REAL32SIN	(0x01 | I_PSEUDO_OP)
#define I_REAL64SIN	(0x02 | I_PSEUDO_OP)
#define I_REAL32COS	(0x03 | I_PSEUDO_OP)
#define I_REAL64COS	(0x04 | I_PSEUDO_OP)
#define I_REAL32TAN	(0x05 | I_PSEUDO_OP)
#define I_REAL64TAN	(0x06 | I_PSEUDO_OP)

/*}}}*/

