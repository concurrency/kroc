/*
 *	cpuid.h - stuff to handle CPUID
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

#ifndef __CPUID_H
#define __CPUID_H

#ifdef HOST_CPU_IS_I486

/* used to check that we're an intel/AMD/Cyrix */
#define INTEL_GENU 0x756e6547
#define INTEL_INEI 0x49656e69
#define INTEL_NTEL 0x6c65746e

#define CPU_VENDOR_UNKNOWN 0
#define CPU_VENDOR_INTEL 1
#define CPU_VENDOR_AMD 2
#define CPU_VENDOR_CYRIX 3
#define CPU_VENDOR_UMC 4
#define CPU_VENDOR_CENTAUR 5
#define CPU_VENDOR_NEXGEN 6
#define CPU_VENDOR_RISE 7
#define CPU_VENDOR_TRANSMETA 8

typedef union {
	struct {
		unsigned int fpu : 1;		/* FPU */
		unsigned int vme : 1;		/* virtual mode enhancements */
		unsigned int de : 1;		/* debugging extensions */
		unsigned int pse : 1;		/* page-size extensions */
		unsigned int tsc : 1;		/* time-stamp counter (rdtsc) */
		unsigned int msr : 1;		/* model-specific registers (rdmsr/wrmsr) */
		unsigned int pae : 1;		/* physical address extensions */
		unsigned int mce : 1;		/* machine check exception */
		unsigned int cx8 : 1;		/* supports 64-bit compare+exchange (cmpxchg8b) */
		unsigned int apic : 1;		/* APIC chippage */
		unsigned int reserved1 : 1;
		unsigned int sep : 1;		/* fast sysenter/sysexit instructions */
		unsigned int mtrr : 1;		/* mem type+range regs */
		unsigned int pge : 1;		/* something to do with paging */
		unsigned int mca : 1;		/* machine check architecture */
		unsigned int cmov : 1;		/* supports cmovcc. fcmovcc & fcomi instructions */
		unsigned int fgpat : 1;		/* ditto, according to Intel's docs.. */
		unsigned int pse36 : 1;		/* more paging stuff */
		unsigned int pn : 1;		/* processor number feature */
		unsigned int reserved2 : 4;
		unsigned int mmx : 1;		/* matrix maths extensions */
		unsigned int fxsr : 1;		/* fast FP/MMX technology (fxsave, fxrstor) */
		unsigned int xmm : 1;		/* streaming SIMD extensions */
		unsigned int sse2 : 1;
		unsigned int reserved3 : 5;
	} bits __attribute__ ((packed));
	unsigned int val;
} cpuid_feature;

#ifndef __CPUID_C
	extern int check_intel_processor (void);
	extern int get_intel_options (void);
	extern int get_processor_class (void);
#endif	/* !__CPUID_C */

#endif	/* HOST_CPU_IS_I486 */
#endif	/* !__CPUID_H */

