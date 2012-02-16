/*
 *	cpuid.c - CPUID stuff
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#define __CPUID_C

#ifdef HOST_CPU_IS_I486

#include "main.h"
#include "support.h"
#include "structs.h"
#include "transputer.h"
#include "trancomm.h"
#include "tstack.h"
#include "postmortem.h"
#include "intel.h"
#include "machine.h"
#include "cpuid.h"


static unsigned int read_cpuid (int which_one, int reg_abcd);



/*
 *	unsigned int read_cpuid (int which_one, int reg_abcd)
 *	reads one of the CPUID things (reb_abcd = [0..3])
 */
static unsigned int read_cpuid (int which_one, int reg_abcd)
{
	unsigned int *u_ptr, val;
	unsigned int x;

	x = which_one;
	__asm__ __volatile__ ("			\n" \
		"pushl	%%ebx			\n" \
		"pushl	%%ecx			\n" \
		"pushl	%%edx			\n" \
		"cpuid				\n" \
		"pushl	%%edx			\n" \
		"pushl	%%ecx			\n" \
		"pushl	%%ebx			\n" \
		"pushl	%%eax			\n" \
		"movl	%%esp, %%eax		\n" \
		"addl	$16, %%esp		\n" \
		"popl	%%edx			\n" \
		"popl	%%ecx			\n" \
		"popl	%%ebx			\n" \
		: "=a" (u_ptr) : "a" (x));
	val = u_ptr[reg_abcd];
	return val;
}


/*
 *	int check_intel_processor (void)
 *	returns 1 if this is an Intel processor, 0 otherwise
 */
int check_intel_processor (void)
{
	if (read_cpuid (0, 1) != INTEL_GENU) {
		fprintf (stderr, "CPUID0.1: %8.8x\n", read_cpuid (0, 1));
		return 0;
	}
	if (read_cpuid (0, 3) != INTEL_INEI) {
		fprintf (stderr, "CPUID0.3: %8.8x\n", read_cpuid (0, 3));
		return 0;
	}
	if (read_cpuid (0, 2) != INTEL_NTEL) {
		fprintf (stderr, "CPUID0.2: %8.8x\n", read_cpuid (0, 2));
		return 0;
	}
	return 1;
}


/*
 *	int get_processor_class (void)
 *	returns processor class
 *	The tables of processor model names were taken from the Linux kernel sources (arch/i386/kernel/setup.c)
 */
int get_processor_class (void)
{
	char str[13];
	unsigned int v[3];
	int i;
	int verinfo;
	/*
	 *	intel:4
	 *	intel:5
	 *	intel:6
	 *	amd:4
	 *	amd:5
	 *	amd:6
	 */
	static char *x86_modeltable[6][16] = {
		{"486 DX-25/33", "486 DX-50", "486 SX", "486 DX/2",
		 "486 SL", "486 SX/2", NULL, "486 DX/2-WB",
		 "486 DX/4", "486 DX/4-WB", NULL, NULL,
		 NULL, NULL, NULL, NULL},
		{"Pentium 60/66 A-step", "Pentium 60/66", "Pentium 75 - 200", "OverDrive PODP5V83",
		 "Pentium MMX", NULL, NULL, "Mobile Pentium 75 - 200",
		 "Mobile Pentium MMX", NULL, NULL, NULL,
		 NULL, NULL, NULL, NULL},
		{"Pentium Pro A-step", "Pentium Pro", NULL, "Pentium II (Klamath)",
		 NULL, "Pentium II (Deschutes)", "Mobile Pentium II", "Pentium III (Katmai)",
		 "Pentium III (Coppermine)", NULL, "Pentium III (Cascades)", NULL,
		 NULL, NULL, NULL, NULL},
		{NULL, NULL, NULL, "486 DX/2",
		 NULL, NULL, NULL, "486 DX/2-WB",
		 "486 DX/4", "486 DX/4-WB", NULL, NULL,
		 NULL, NULL, "Am5x86-WT", "Am5x86-WB"},
		{"K5/SSA5", "K5", "K5", "K5",
		 NULL, NULL, "K6", "K6",
		 "K6-2", "K6-3", NULL, NULL,
		 NULL, NULL, NULL, NULL},
		{"Athlon", "Athlon", "Athlon", NULL,
		 "Athlon", NULL, NULL, NULL,
		 NULL, NULL, NULL, NULL,
		 NULL, NULL, NULL, NULL}};
	static int x86_classtable[6][16] = {
		{CLASS_486, CLASS_486, CLASS_486, CLASS_486,
		 CLASS_486, CLASS_486, CLASS_UNKNOWN, CLASS_486,
		 CLASS_486, CLASS_486, CLASS_UNKNOWN, CLASS_UNKNOWN,
		 CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN},
		{CLASS_586, CLASS_586, CLASS_586, CLASS_586,
		 CLASS_586, CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_586,
		 CLASS_586, CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN,
		 CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN},
		{CLASS_686, CLASS_686, CLASS_UNKNOWN, CLASS_686,
		 CLASS_UNKNOWN, CLASS_686, CLASS_686, CLASS_686,
		 CLASS_686, CLASS_UNKNOWN, CLASS_686, CLASS_UNKNOWN,
		 CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN},
		{CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_486,
		 CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_486,
		 CLASS_486, CLASS_486, CLASS_UNKNOWN, CLASS_UNKNOWN,
		 CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_586, CLASS_586},
		{CLASS_586, CLASS_586, CLASS_586, CLASS_586,
		 CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_586, CLASS_586,
		 CLASS_586, CLASS_586, CLASS_UNKNOWN, CLASS_UNKNOWN,
		 CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN},
		{CLASS_686, CLASS_686, CLASS_686, CLASS_UNKNOWN,
		 CLASS_686, CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN,
		 CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN,
		 CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN, CLASS_UNKNOWN}};
	unsigned int x86_vendor, x86_family, x86_model, x86_stepping;

	str[12] = '\0';
	v[0] = read_cpuid (0, 1);
	v[1] = read_cpuid (0, 3);
	v[2] = read_cpuid (0, 2);
	for (i=0; i<4; i++) {
		str[i+0] = ((v[0] >> (i << 3)) & 0xff);
		str[i+4] = ((v[1] >> (i << 3)) & 0xff);
		str[i+8] = ((v[2] >> (i << 3)) & 0xff);
	}
	if (options.verbose) {
		printf ("%s: CPUID gives [%s]\n", progname, str);
	}
	if (!strcmp (str, "GenuineIntel")) {
		x86_vendor = CPU_VENDOR_INTEL;
	} else if (!strcmp (str, "AuthenticAMD")) {
		x86_vendor = CPU_VENDOR_AMD;
	} else if (!strcmp (str, "CyrixInstead")) {
		x86_vendor = CPU_VENDOR_CYRIX;
	} else if (!strcmp (str, "UMC UMC UMC ")) {
		x86_vendor = CPU_VENDOR_UMC;
	} else if (!strcmp (str, "CentaurHauls")) {
		x86_vendor = CPU_VENDOR_CENTAUR;
	} else if (!strcmp (str, "NexGenDriven")) {
		x86_vendor = CPU_VENDOR_NEXGEN;
	} else if (!strcmp (str, "RiseRiseRise")) {
		x86_vendor = CPU_VENDOR_RISE;
	} else if (!strcmp (str, "GenuineTMx86") || !strcmp (str, "TransmetaCPU")) {
		x86_vendor = CPU_VENDOR_TRANSMETA;
	} else {
		x86_vendor = CPU_VENDOR_UNKNOWN;
	}
	if (!read_cpuid (0, 0)) {
		/* old processor.. */
		return CLASS_386;
	}
	verinfo = read_cpuid (1, 0);
	x86_family = ((verinfo >> 8) & 0x0f);
	if ((x86_family < 4) || (x86_family > 6)) {
		/* unhandled family */
		return CLASS_386;
	}
	x86_model = ((verinfo >> 4) & 0x0f);
	x86_stepping = (verinfo & 0x0f);
	switch (x86_vendor) {
	case CPU_VENDOR_INTEL:
		if (options.verbose) {
			printf ("%s: CPU is %s, stepping %d\n", progname, x86_modeltable[x86_family - 4][x86_model], x86_stepping);
		}
		return x86_classtable[x86_family - 4][x86_model];
	case CPU_VENDOR_AMD:
		if (options.verbose) {
			printf ("%s: CPU is %s, stepping %d\n", progname, x86_modeltable[x86_family - 1][x86_model], x86_stepping);
		}
		return x86_classtable[x86_family - 1][x86_model];
	case CPU_VENDOR_CYRIX:
		if (options.verbose) {
			printf ("%s: CPU is cyrix\n", progname);
		}
		return CLASS_386;
	case CPU_VENDOR_UMC:
		if (options.verbose) {
			printf ("%s: CPU is UMC\n", progname);
		}
		return CLASS_386;
	case CPU_VENDOR_CENTAUR:
		if (options.verbose) {
			printf ("%s: CPU is centaur\n", progname);
		}
		return CLASS_386;
	case CPU_VENDOR_NEXGEN:
		if (options.verbose) {
			printf ("%s: CPU in NexGen\n", progname);
		}
		return CLASS_386;
	case CPU_VENDOR_RISE:
		if (options.verbose) {
			printf ("%s: CPU is rise\n", progname);
		}
		return CLASS_386;
	case CPU_VENDOR_TRANSMETA:
		if (options.verbose) {
			printf ("%s: CPU is Transmeta\n", progname);
		}
		return CLASS_386;
	}
	return CLASS_386;
}


/*
 *	int get_intel_options (void)
 *	gets the options supported (from main.h)
 */
int get_intel_options (void)
{
	cpuid_feature fdata;
	int opts;

	fdata.val = read_cpuid (1, 3);
	opts = 0;
	if (fdata.bits.mmx) {
		opts |= OPTION_MMX;
	}
	if (fdata.bits.cmov) {
		opts |= OPTION_CMOVC | OPTION_FCOMI;
	}
	if (fdata.bits.cx8) {
		opts |= OPTION_CMPXCHG8;
	}
	if (fdata.bits.fxsr) {
		opts |= OPTION_FXSR;
	}
	if (fdata.bits.xmm) {
		opts |= OPTION_XMM;
	}
	if (fdata.bits.sep) {
		opts |= OPTION_SYSENTER;
	}
	if (fdata.bits.sse2) {
		opts |= OPTION_SSE2;
	}
	return opts;
}

#endif	/* HOST_CPU_IS_I486 */

