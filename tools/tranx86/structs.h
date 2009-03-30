/*
 *	structs.h - Various structures
 *	Copyright (C) 2000 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __STRUCTS_H
#define __STRUCTS_H

	/* used in tracing errors -- very memory hungry */
	/* #define INSTRUCTION_HISTORY */

struct TAG_tdfixup;


typedef struct TAG_etc_chain {
	struct TAG_etc_chain *next;
	int fn, opd;
	char *o_bytes;
	int o_len;
} etc_chain;

typedef enum {
	RTL_UNDEFINED = 0,
	RTL_SOURCEFILE = 1,		/* sourcefile */
	RTL_CODELINE = 3,
	RTL_CODE = 4,			/* code */
	RTL_DATA = 5,			/* data */
	RTL_RDATA = 6,			/* rdata */
	RTL_XDATA = 7,			/* xdata */
	RTL_SETNAMEDLABEL = 10,		/* label_name */
	RTL_STUBIMPORT = 16,		/* label_name */
	RTL_PUBLICSETNAMEDLABEL = 19,	/* label_name */
	RTL_ALIGN = 20,			/* alignment */
	RTL_COMMENT = 21,		/* data */
	RTL_MESSAGE = 22,		/* data */
	RTL_WSVS = 23,			/* wsvs */
	RTL_CODEMAP = 24,		/* codemap */
	RTL_PUBLICENDNAMEDLABEL = 25,	/* label_name */
	RTL_DYNCODEENTRY = 26		/* dyncode */
} rtl_t;

/* forwards */
struct TAG_ins_chain;
struct TAG_procinf;

typedef struct TAG_rtl_chain {
	struct TAG_rtl_chain *next;
	struct TAG_rtl_chain *prev;
	rtl_t type;
	union {
		char *sourcefile;
		int alignment;
		struct {
			struct TAG_ins_chain *head;
			struct TAG_ins_chain *tail;
		} code;
		struct {
			char *bytes;
			int length;
		} data;
		struct {
			int label;
			char *bytes;
			int length;
		} rdata;
		struct {
			int label;
			char *bytes;
			int length;
			struct TAG_tdfixup *fixups;
		} xdata;
		struct {
			int ws_bytes;
			int ws_adjust;
			int vs_bytes;
			int ms_bytes;
		} wsvs;
		struct {
			struct TAG_procinf *pinf;
		} codemap;
		struct {
			char *label_name;
			char *fcn_name;
			int ws_slots;
			int vs_slots;
			unsigned int typehash;
			rmoxmode_e rmoxmode;
		} dyncode;
		char *label_name;
		char *stub_name;
	} u;
} rtl_chain;

#define INS_UNDEFINED 0
#define INS_MOVE 1
#define INS_NOP 2
#define INS_LEA 3
#define INS_SETCC 4
#define INS_CMP 5
#define INS_ADD 6
#define INS_AND 7
#define INS_OR 8
#define INS_INTO 9
#define INS_CJUMP 10
#define INS_SETLABEL 11
#define INS_PUSH 12
#define INS_POP 13
#define INS_RET 14
#define INS_CALL 15
#define INS_KCALL 16			/* special */
#define INS_JUMP 17
#define INS_SWAP 18
#define INS_DEC 19
#define INS_INC 20
#define INS_SUB 21
#define INS_CWDE 22
#define INS_XOR 23
#define INS_MUL 24
#define INS_RDTSC 25
#define INS_LOADLABDIFF 26
#define INS_CDQ 27
#define INS_DIV 28
#define INS_SHL 29
#define INS_SHR 30
#define INS_MOVEB 31
#define INS_NOT 32
#define INS_CONSTLABDIFF 33	/* for case tables */
#define INS_REPMOVEB 34
#define INS_MOVEZEXT8TO32 35		/* zero-extend 8 to 32 bit move */
#define INS_RCR 36
#define INS_RCL 37
#define INS_ROR 38
#define INS_ROL 39
#define INS_ADC 40
#define INS_SBB 41
#define INS_UMUL 42
#define INS_UDIV 43
#define INS_SHLD 44
#define INS_SHRD 45
#define INS_FSTCW 46
#define INS_FLDCW 47
#define INS_WAIT 48
#define INS_FSTP 49			/* replaces FPOP (used with ST(0)) */
#define INS_ANNO 50			/* annotation (comes out as ASM comment) */
#define INS_FILD32 51
#define INS_FXCH 52
#define INS_FLD 53
#define INS_FILD64 54
#define INS_FLD32 55
#define INS_FLD64 56
#define INS_FLD80 57
#define INS_FADD32 58
#define INS_FADD64 59
#define INS_FMUL32 60
#define INS_FMUL64 61
#define INS_FST32 62
#define INS_FST64 63
#define INS_FST80 64
#define INS_FIST32 65
#define INS_FIST64 66
#define INS_FLD1 67
#define INS_FLDL2T 68
#define INS_FLDL2E 69
#define INS_FLDPI 70
#define INS_FLDLG2 71
#define INS_FLDLN2 72
#define INS_FLDZ 73
#define INS_FADD 74
#define INS_FSUB 75
#define INS_FMUL 76
#define INS_FDIV 77
#define INS_SAHF 78
#define INS_FTST 79
#define INS_FXAM 80
#define INS_FSTSW 81
#define INS_FUCOM 82
#define INS_FUCOMP 83
#define INS_FUCOMPP 84
#define INS_FCOM 85
#define INS_FCOMP 86
#define INS_FCOMPP 87
#define INS_FRNDINT 88
#define INS_FSQRT 89
#define INS_FABS 90
#define INS_FCHS 91
#define INS_FSCALE 92
#define INS_FPREM1 93
#define INS_SETFLABEL 94	/* n: type labels */
#define INS_MOVESEXT16TO32 95		/* sign-extend 16 to 32 bit move */
#define INS_LAHF 96
#define INS_REPMOVEL 97
#define INS_CMOVE 98		/* conditional move (cmovCC) */
#define INS_PJUMP 99		/* pausing jump (check sync-flags/resched/etc.) */
#define INS_INB 100		/* for IOSPACE handling */
#define INS_OUTB 101		/* for IOSPACE handling */
#define INS_CCALL 102		/* for external C calls */
#define INS_MOVEZEXT16TO32 103		/* zero-extend 16 to 32 bit move */
#define INS_MOVEW 104		/* move word */
#define INS_MTFSB 105		/* set FPU control/status bit (bit-num, val-bit) consts */
#define INS_MTFSI 106		/* set FPU control/status field (field, val-nibble) consts */
#define INS_MFFS 107		/* copy FPU control/status register to FP register */
#define INS_MTFSF 108		/* copy FP register to FPU control/status register with mask */
#define INS_FSIN 109
#define INS_FCOS 110
#define INS_INW 111		/* for IOSPACE handling */
#define INS_OUTW 112		/* for IOSPACE handling */
#define INS_INL 113		/* for IOSPACE handling */
#define INS_OUTL 114		/* for IOSPACE handling */
#define INS_LOCK 115		/* for i386 atomic ops */
#define INS_FPTAN 116		/* partial tangent */
#define INS_MB 117		/* memory barrier */
#define INS_RMB 118		/* read memory barrier */
#define INS_WMB 119		/* write memory barrier */
#define INS_CONSTLABADDR 120	/* constant label address */

#define INS_FIRST INS_MOVE
#define INS_LAST INS_CONSTLABADDR

#define INS_START_REG 65535	/* ARG_REG */
#define INS_END_REG 65534	/* ARG_REG */
#define INS_CONSTRAIN_REG 65533	/* ARG_REG, ARG_REG */
#define INS_FREE_REG 65532	/* ARG_REG */
#define INS_START_CC 65531	/* ARG_CONST */
#define INS_END_CC 65530	/* ARG_CONST */
#define INS_UNCONSTRAIN_REG 65529	/* ARG_REG */

#define INS_SOURCELINE 32767	/* ARG_CONST */
#define INS_CLEANUP 32766


#define ARG_UNDEFINED 0x00
#define ARG_REG 0x01
#define ARG_CONST 0x02
#define ARG_REGIND 0x03
#define ARG_COND 0x04
#define ARG_LABEL 0x05
#define ARG_NAMEDLABEL 0x06	/* (char *)regconst */
#define ARG_TEXT 0x07		/* (char *)regconst */
#define ARG_REGINDSIB 0x08	/* (ins_sib_arg *)regconst */
#define ARG_FREG 0x09
#define ARG_FLABEL 0x0a		/* forward label reference ($nf type) */
#define ARG_BLABEL 0x0b		/* backward label reference ($nb type) */
#define ARG_INSLABEL 0x0c	/* (ins_chain *)regconst */
#define ARG_LABREFS 0x0f	/* (ins_labrefs *)regconst */
#define ARG_MODEMASK 0x0f
#define ARG_DISP 0x10
#define ARG_IND 0x20		/* used for certain jump/call instructions */
#define ARG_IMP 0x40		/* implied operand, ie, not specified in the encoding */
#define ARG_ISCONST 0x80	/* treat value as a constant */
#define ARG_IS8BIT 0x100	/* argument refers to low 8-bit quantity (al,bl,dl,dl) */
#define ARG_IS16BIT 0x200	/* argument refers to 16-bit quantity */
#define ARG_IS8HIGH 0x400	/* argument refers to high 8-bit quantity (ah,bh,ch,dh) */
#define ARG_FLAGMASK 0x7ff	/* bit-mask for all flags & modes */
#define ARG_ARG 0x1000		/* special when composing for "existing argument" (ins_arg *) */

typedef struct TAG_ins_sib_arg {
	int base;		/* register */
	int index;		/* register */
	int disp;		/* constant */
	int scale;		/* 1, 2, 4 or 8 */
} ins_sib_arg;

typedef struct TAG_ins_arg {
	int regconst;
	int disp;
	unsigned int flags;
	unsigned char padding[4];
} ins_arg;

#define ArgMode(X)	((X)->flags & ARG_MODEMASK)
#define ArgReg(X)	((X)->regconst)
#define ArgConst(X)	((X)->regconst)
#define ArgCC(X)	ArgConst(X)
#define ArgLabel(X)	((X)->regconst)
#define ArgFLabel(X)	((X)->regconst)
#define ArgName(X)	(char *)((X)->regconst)
#define ArgDisp(X)	((X)->disp)
#define ArgHasDisp(X)	((X)->flags & ARG_DISP)
#define ArgIsConst(X)	((X)->flags & ARG_ISCONST)
#define ArgIsImplied(X)	((X)->flags & ARG_IMP)

#define ETC_PRI 0x10000
#define ETC_SPC 0x20000
#define ETC_SEC 0x30000

#define EtcPrimary(X)	(ETC_PRI | X)
#define EtcSpecial(X)	(ETC_SPC | X)
#define EtcSecondary(X)	(ETC_SEC | X)

#define HasEtcType(X)		((X)->etc_type > 0)
#define GetEtcType(X)		((X)->etc_type)
#define EtcTypeIsPrimary(X)	((X)->etc_type & ETC_PRI)
#define EtcTypeIsSpecial(X)	((X)->etc_type & ETC_SPC)
#define EtcTypeIsSecondary(X)	((X)->etc_type & ETC_SEC)
#define EtcTypeGetPrimary(X)  	(EtcTypeIsPrimary(X)?(X)->etc_type ^ ETC_PRI:-1)
#define EtcTypeGetSpecial(X)	(EtcTypeIsSpecial(X)?(X)->etc_type ^ ETC_SPC:-1)
#define EtcTypeGetSecondary(X)	(EtcTypeIsSecondary(X)?(X)->etc_type ^ ETC_SEC:-1)

#define MAX_IN_ARGS 6
#define MAX_OUT_ARGS 6

typedef struct TAG_ins_chain {
	struct TAG_ins_chain *next;
	struct TAG_ins_chain *prev;
	rtl_chain *rtl;
	int type;
	int etc_type;
	ins_arg *in_args[MAX_IN_ARGS];
	ins_arg *out_args[MAX_OUT_ARGS];
	#ifdef INSTRUCTION_HISTORY
		char alloc_file[128];
		long alloc_line;
	#endif	/* INSTRUCTION_HISTORY */
} ins_chain;

#define InsType(X)		(X)->type

#define ArgInsLab(X)		(ins_chain *)((X)->regconst)
#define SetArgInsLab(X,V)	((X)->regconst = (int)(V))

typedef struct TAG_ins_labrefs {
	int ref_cur, ref_max;
	ins_chain **refs;
} ins_labrefs;

#define ArgLabRefs(X)		(ins_labrefs *)((X)->regconst)
#define SetArgLabRefs(X,V)	((X)->regconst = (int)(V))

/* condition codes */
#define CC_NONE -1
#define CC_O 0
#define CC_NO 1
#define CC_B 2
#define CC_C 2
#define CC_AE 3
#define CC_Z 4
#define CC_E 4
#define CC_NZ 5
#define CC_NE 5
#define CC_BE 6
#define CC_A 7
#define CC_S 8
#define CC_NS 9
#define CC_PE 10
#define CC_PO 11
#define CC_LT 12
#define CC_GE 13
#define CC_LE 14
#define CC_GT 15
#define CC_FPNAN 10
#define CC_FPINFNAN 2
#define CC_FPORD 11

/* special for some architectures */
#define CC_ALWAYS 16
#define CC_NEVER 17

/* FPU rounding modes */
#define FPU_NONE (-1)
#define FPU_N 0 /* round to nearest */
#define FPU_P 1 /* round up */
#define FPU_M 2 /* round down */
#define FPU_Z 3 /* round toward zero (truncate) */

/* misc. constants */
#define NOT_PROCESS 0


#endif	/* __STRUCTS_H */


