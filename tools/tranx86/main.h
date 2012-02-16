/*
 *	main.h - global stuff
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

#ifndef __MAIN_H
#define __MAIN_H

#define OUTPUT_ELF 0
#define OUTPUT_ASM 1
#define OUTPUT_ETC 2
#define OUTPUT_RTL0 3
#define OUTPUT_RTL1 4
#define OUTPUT_RTL2 5
#define OUTPUT_RTL3 6
#define OUTPUT_RAW 7
#define OUTPUT_COMMENTS 8

#define DEBUG_OVERFLOW 0x1
#define DEBUG_DEADLOCK 0x2
#define DEBUG_RANGESTOP 0x4
#define DEBUG_FLOAT 0x8
#define DEBUG_INSERT 0x10
#define DEBUG_MEMCHK 0x20
#define DEBUG_STOPMODE 0x8000
#define DEBUG_DTRACES 0x4000

#define KRNLIFACE_NONE 0x00
#define KRNLIFACE_NEWCCSP 0x02
#define KRNLIFACE_MESH 0x04
#define KRNLIFACE_CSPLINUX 0x08
#define KRNLIFACE_RMOX 0x10
#define KRNLIFACE_IX 0x20
#define KRNLIFACE_MP 0x40

/* first 8 behave as 3-bit-field, [MSB: err, scr, kyb :LSB]. */
/* next 3 bits 0x08..0x20 indicate shared-ness (anon-chantypes) */
/* INVALID (42) is 0x2A (00101010) so safe to use */
#define TLP_NULL 0x00
#define TLP_KYB 0x01
#define TLP_SCR 0x02
#define TLP_KYBSCR 0x03
#define TLP_ERR 0x04
#define TLP_KYBERR 0x05
#define TLP_SCRERR 0x06
#define TLP_KYBSCRERR 0x07
#define TLP_FSTS 0x41
#define TLP_FSTSMEM 0x42
#define TLP_FSTSSIZEDMEM 0x43
#define TLP_SHAREDKYB 0x08		/* FLAG */
#define TLP_SHAREDSCR 0x10		/* FLAG */
#define TLP_SHAREDERR 0x20		/* FLAG */
#define TLP_INVALID 0x2A
#define TLP_FORK_BARRIER 0x100		/* FLAG */
#define TLP_FORK_NOWAIT 0x200		/* FLAG */
#define TLP_EXT_NOWAIT 0x400		/* FLAG */
#define TLP_MPP_BARRIER 0x800		/* FLAG */
#define TLP_PRINT_MEMSTATS 0x1000	/* FLAG */
#define TLP_CTTD 0x2000			/* FLAG */

#define TLP_SHAREDMASK 0x38		/* mask for SHARED params */
#define TLP_EFLAGMASK 0x3f00		/* mask for extra flags */

#define INLINE_IN 0x01			/* inline IN instructions */
#define INLINE_OUT 0x02			/* inline OUT instructions */
#define INLINE_ALT 0x04			/* inline ALTWT, ENBC and DISC */
#define INLINE_SCHEDULER 0x08		/* inline bits of the scheduler and STL[FB] */
#define INLINE_LDTIMER 0x10		/* inline LDTIMER */
#define INLINE_IN2 0x20			/* inline IN completely */
#define INLINE_OUT2 0x40		/* inline OUT completely */
#define INLINE_MIN_MOUT 0x80		/* inline MIN/MOUT/MIN64/MOUT64 instructions */
#define INLINE_REDUCED_POLL 0x100	/* reduced kernel entries */
#define INLINE_ALTERNATE_POLICY 0x200	/* use alternate re-scheduling policy for inlining */
#define INLINE_TIN 0x400		/* inline TIN */

#define INTERNAL_NOOPT 0x01		/* don't run the optimiser */
#define INTERNAL_NOCONSTMAP 0x02	/* disable the constant map */
#define INTERNAL_BIGENDIAN 0x04		/* generate constants in big-endian */
#define INTERNAL_DUMPPROCTAB 0x08	/* dump PROC table */
#define INTERNAL_TEMPFLOAT 0x10		/* insert space for a temporary floating-point value */
#define INTERNAL_FLOATCONV 0x20		/* need a floating-point conversion constant */
#define INTERNAL_ALIGNEDCODE 0x40	/* if code needs to be sensibly aligned */

typedef enum {
	RM_NONE = 0,			/* not building for RMoX */
	RM_APP = 1,			/* building RMoX application */
	RM_DRV = 2,			/* building RMoX device driver */
	RM_SRV = 3,			/* building RMoX service */
	RM_FS = 4,			/* building RMoX file-system handler */
	RM_NET = 5			/* building RMoX network driver */
} rmoxmode_e;

typedef struct {
	/* general options */
	int not_main_module;
	int is_library;
	int suppress_fpu;
	int pause_at_loopend;
	int verbose;
	int use_cpuid;
	/* output */
	int output_format;
	int annotate_output;
	int gstabs;
	int drop_assembler;
	int tlp_interface;		/* overrides automatic checking */
	rmoxmode_e rmoxmode;
	/* debugging */
	int debug_options;
	int diagnostics;
	int internal_options;
	/* code generation */
	int aeconst;			/* generate alternate-endian constants */
	int machine_target;
	int machine_class;
	int machine_options;
	int kernel_interface;
	int inline_options;
	int disable_checking;
	int disable_dynmem;		/* disable dynamic memory support */
	int disable_symbol_ops;
	int no_ext_chan_checks;		/* don't generate external channel checks */
	int always_annotate;		/* hmm.. */
	int rtflags;			/* user-controllable TLP_EFLAGs */
	char *extref_prefix;		/* prefix for external C references (typically "_") */
	int use_cttd;			/* whether the source was compiled with chan-type type descriptions */
	int mpenable;			/* whether multi-processor support is enabled in the run-time */
	int nocc_codegen;		/* non-zero if generating code compiled by NOCC */
	int underflow_error;		/* whether FP underflow is an error */
	char *etab_filename;		/* if non-NULL, write exports to this file */
	FILE *etabfile;			/* if non-NULL, writing EXPORTs to this file */
} optstruct;

#ifndef __MAIN_C
	extern char *progname;
	extern optstruct options;
	extern long glob_in_icount, glob_out_icount;
#endif	/* !__MAIN_C */

extern char *machine_class_str (int mclass);

/* dev stuff */
#undef TRACE_MEMORY

#endif	/* !__MAIN_H */

