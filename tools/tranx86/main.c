/*
 *	main.c -- main glue-code
 *	Copyright (C) 2000-2008 Fred Barnes <frmb@kent.ac.uk>
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

/*{{{  includes*/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define __MAIN_C


#ifndef VERSION
#define VERSION "1.4.0"
#endif

#include "main.h"
#include "support.h"
#include "structs.h"
#include "etcdump.h"
#include "tceread.h"
#include "netcread.h"
#include "etcops.h"
#include "machine.h"
#include "cpuid.h"
#include "archdef.h"
#include "rtldump.h"
#include "rtlops.h"
#include "optimise.h"
#include "regcolour.h"
#include "arch386.h"
#include "elf.h"
#include "archmips.h"
#include "etcrtl.h"
#include "archsparc.h"
#include "archppc.h"
/*}}}*/

#define PARANOID 0

/*{{{  local prototypes*/
static void usage (FILE *);
static void tranx86_banner (FILE *stream);
static void dump_supported_things (FILE *);
static void dump_supported_tlpifs (FILE *);
static void dump_version (FILE *);
static int tranx86 (char *, char *, arch_t *);
static arch_t *init_architecture (int mclass);
/*}}}*/
/*{{{  global vars*/
char *progname;
optstruct options;
long glob_in_icount, glob_out_icount;
/*}}}*/

#ifndef EXIT_SUCCESS
	#define EXIT_SUCCESS 0
#endif	/* !EXIT_SUCCESS */
#ifndef EXIT_FAILURE
	#define EXIT_FAILURE 1
#endif	/* !EXIT_FAILURE */


/*{{{  int main (int argc, char **argv)*/
/*
 *	int main (int argc, char **argv)
 *	Start here
 */
int main (int argc, char **argv)
{
	char **walk;
	char *output_filename;
	char *input_filename;
	int i, sel;
	char opchars[4];
	char *targ;
	arch_t *t_arch;

	for (targ = *argv + (strlen (*argv) - 1); (targ > *argv) && (*targ != '/'); targ--);
	if (targ == *argv) {
		progname = *argv;
	} else {
		progname = targ + 1;
	}
	output_filename = input_filename = NULL;
	/* default options */
	options.aeconst = 0;
	options.not_main_module = 0;
	options.is_library = 0;
	options.suppress_fpu = 0;
	options.pause_at_loopend = 0;
	options.verbose = 0;
	options.output_format = OUTPUT_ELF;
	options.annotate_output = 0;
	options.drop_assembler = 0;
	options.gstabs = 0;
	options.rmoxmode = RM_NONE;
	options.debug_options = 0;
	options.diagnostics = 0;
	options.use_cpuid = 0;
	options.machine_target = TARGET_DEFAULT;
#if defined(HOST_CPU_IS_I386) || defined(HOST_CPU_IS_I486)
	options.machine_class = CLASS_386;
#elif defined(HOST_CPU_IS_MIPS)
	options.machine_class = CLASS_R4600;
#elif defined(HOST_CPU_IS_SPARC)
	options.machine_class = CLASS_SPARCV8;
#elif defined(HOST_CPU_IS_PPC64)
	options.machine_class = CLASS_POWERPC;
#else
	options.machine_class = CLASS_UNKNOWN;
#endif
	options.machine_options = 0;
	options.disable_checking = 0;
	options.disable_dynmem = 0;
	options.disable_symbol_ops = 0;
	options.kernel_interface = KRNLIFACE_NONE;
	options.inline_options = 0;
	options.internal_options = 0;
	options.tlp_interface = TLP_INVALID;
	options.always_annotate = 0;
#ifdef USER_DEFINED_CHANNELS
	options.no_ext_chan_checks = 0;
#endif
	options.rtflags = 0;
	options.extref_prefix = NULL;
	options.use_cttd = 0;
	options.mpenable = 0;
	options.nocc_codegen = 0;
	options.underflow_error = 0;
	options.etab_filename = NULL;
	options.etabfile = NULL;

	glob_in_icount = 0;
	glob_out_icount = 0;
	for (i=1, walk=argv+1; (i < argc) && *walk; i++, walk++) {
		memcpy (opchars, *walk, 4);
		if (opchars[0] != '-') {
			if (input_filename) {
				fprintf (stderr, "%s: only one input file allowed\n", progname);
				exit (1);
			}
			input_filename = string_dup (*walk);
			continue;
		}
		switch (opchars[1]) {
			/*{{{  c*/
		case 'c':
			options.not_main_module = 1;
			break;
			/*}}}*/
			/*{{{  l*/
		case 'l':
			options.is_library = 1;
			break;
			/*}}}*/
			/*{{{  f*/
		case 'f':
			options.suppress_fpu = 1;
			break;
			/*}}}*/
			/*{{{  p*/
		case 'p':
			switch (opchars[2]) {
			case '\0':
			case '1':
				options.pause_at_loopend = 1;
				break;
			case '2':
				options.pause_at_loopend = 2;
				break;
			default:
				goto unknown_option;
			}
			break;
			/*}}}*/
			/*{{{  v*/
		case 'v':
			options.verbose = 1;
			break;
			/*}}}*/
			/*{{{  e*/
		case 'e':
			switch (opchars[2]) {
			case '\0':
				options.output_format = OUTPUT_ELF;
				break;
			case 's':
				options.output_format = OUTPUT_ELF;
				options.drop_assembler = 1;
				break;
			default:
				goto unknown_option;
			}
			break;
			/*}}}*/
			/*{{{  n*/
		case 'n':
			switch (opchars[2]) {
			case '\0':
				options.disable_checking = 1;
				break;
			case 'd':
				options.disable_dynmem = 1;
				break;
			case 'e':
				if (!strcmp (*walk, "-nec")) {
					options.no_ext_chan_checks = 1;
				} else {
					goto unknown_option;
				}
				break;
			case 's':
				if (!strcmp (*walk, "-nsymops")) {
					options.disable_symbol_ops = 1;
				} else {
					goto unknown_option;
				}
				break;
			default:
				goto unknown_option;
			}
			break;
			/*}}}*/
			/*{{{  s*/
		case 's':
			switch (opchars[2]) {
			case '\0':
				options.output_format = OUTPUT_ASM;
				break;
			case 'a':
				options.output_format = OUTPUT_ASM;
				options.annotate_output = 1;
				break;
			default:
				goto unknown_option;
			}
			break;
			/*}}}*/
			/*{{{  r*/
		case 'r':
			if (opchars[2] == 'a') {
				sel = 3;
				options.annotate_output = 1;
			} else {
				sel = 2;
			}
			switch (opchars[sel]) {
			case '0':
				options.output_format = OUTPUT_RTL0;
				break;
			case '1':
				options.output_format = OUTPUT_RTL1;
				break;
			case '2':
				options.output_format = OUTPUT_RTL2;
				break;
			case '3':
				options.output_format = OUTPUT_RTL3;
				break;
			case '\0':
				options.output_format = OUTPUT_RAW;
				break;
			default:
				goto unknown_option;
			}
			break;
			/*}}}*/
			/*{{{  H*/
		case 'H':
			if (opchars[2] != '\0') {
				goto unknown_option;
			}
			options.debug_options &= ~DEBUG_STOPMODE;
			break;
			/*}}}*/
			/*{{{  S*/
		case 'S':
			if (opchars[2] != '\0') {
				goto unknown_option;
			}
			options.debug_options |= DEBUG_STOPMODE;
			break;
			/*}}}*/
			/*{{{  d*/
		case 'd':
			switch (opchars[2]) {
			case '\0':
				options.output_format = OUTPUT_ETC;
				break;
			case 'o':
				options.debug_options |= DEBUG_OVERFLOW;
				break;
			case 'd':
				options.debug_options |= DEBUG_DEADLOCK;
				break;
			case 'r':
				options.debug_options |= DEBUG_RANGESTOP;
				break;
			case 'f':
				options.debug_options |= DEBUG_FLOAT;
				break;
			case 'i':
				options.debug_options |= DEBUG_INSERT;
				break;
			case 'X':
				options.debug_options |= DEBUG_OVERFLOW;
				options.debug_options |= DEBUG_DEADLOCK;
				options.debug_options |= DEBUG_RANGESTOP;
				options.debug_options |= DEBUG_FLOAT;
				break;
			case 'm':
				options.debug_options |= DEBUG_MEMCHK;
				break;
			case 's':
				options.rtflags |= TLP_PRINT_MEMSTATS;
				break;
			case 't':
				options.debug_options |= DEBUG_DTRACES;
				break;
			default:
				goto unknown_option;
			}
			break;
			/*}}}*/
			/*{{{  C*/
		case 'C':
			options.output_format = OUTPUT_COMMENTS;
			break;
			/*}}}*/
			/*{{{  k*/
		case 'k':
			switch (opchars[2]) {
			case 'c':
				options.kernel_interface |= KRNLIFACE_MP;
				break;
			case 'f':
				options.kernel_interface |= KRNLIFACE_NEWCCSP;
				break;
			case 'm':
				options.kernel_interface |= KRNLIFACE_MESH;
				break;
			case 'l':
				options.kernel_interface |= KRNLIFACE_CSPLINUX;
				break;
			case 'r':
				options.kernel_interface |= KRNLIFACE_RMOX;
				break;
			case 'i':
				options.kernel_interface |= KRNLIFACE_IX;
				break;
			default:
				goto unknown_option;
			}
			break;
			/*}}}*/
			/*{{{  o*/
		case 'o':
			i++, walk++;
			if ((i==argc) || !*walk) {
				fprintf (stderr, "%s: option %s lacking filename\n", progname, *(walk-1));
				exit (EXIT_FAILURE);
			}
			output_filename = string_dup (*walk);
			break;
			/*}}}*/
			/*{{{  h*/
		case 'h':
			usage (stdout);
			exit (EXIT_SUCCESS);
			/*}}}*/
			/*{{{  X*/
		case 'X':
			if (opchars[2] == '-') {
				options.diagnostics = 1;
			}
			break;
			/*}}}*/
#ifdef HOST_CPU_IS_I486
			/*{{{  u*/
		case 'u':
			options.use_cpuid = 1;
			break;
			/*}}}*/
#endif	/* HOST_CPU_IS_I486 */
			/*{{{  m*/
		case 'm':
#ifdef MP_ENABLE
			if (!strcmp (*walk, "-mp")) {
				/* special -- multi-processor enable */
				options.mpenable = 1;
				break;
			}
#endif
			if (opchars[2] == '\0') {
				dump_supported_things (stdout);
				exit (EXIT_SUCCESS);
			}
			if (!strcmp (*walk + 2, "386")) {
				options.machine_target = TARGET_X86;
				options.machine_class = CLASS_386;
			} else if (!strcmp (*walk + 2, "486")) {
				options.machine_target = TARGET_X86;
				options.machine_class = CLASS_486;
			} else if (!strcmp (*walk + 2, "586")) {
				options.machine_target = TARGET_X86;
				options.machine_class = CLASS_586;
			} else if (!strcmp (*walk + 2, "686")) {
				options.machine_target = TARGET_X86;
				options.machine_class = CLASS_686;
			} else if (!strcmp (*walk + 2, "sparc")) {
				options.machine_target = TARGET_SPARC;
				options.machine_class = CLASS_SPARCV8;
			} else if (!strcmp (*walk + 2, "sparcv8")) {
				options.machine_target = TARGET_SPARC;
				options.machine_class = CLASS_SPARCV8;
			} else if (!strcmp (*walk + 2, "MMX")) {
				options.machine_options |= OPTION_MMX;
			} else if (!strcmp (*walk + 2, "SSE2")) {
				options.machine_options |= OPTION_SSE2;
			} else if (!strcmp (*walk + 2, "cmovc")) {
				options.machine_options |= OPTION_CMOVC;
			} else if (!strcmp (*walk + 2, "mips")) {
				options.machine_target = TARGET_MIPS;
				options.machine_class = CLASS_R5000;
			} else if (!strcmp (*walk + 2, "r3k")) {
				options.machine_target = TARGET_MIPS;
				options.machine_class = CLASS_R3000;
			} else if (!strcmp (*walk + 2, "r43k")) {
				options.machine_target = TARGET_MIPS;
				options.machine_class = CLASS_R4300;
			} else if (!strcmp (*walk + 2, "r46k")) {
				options.machine_target = TARGET_MIPS;
				options.machine_class = CLASS_R4600;
			} else if (!strcmp (*walk + 2, "r5k")) {
				options.machine_target = TARGET_MIPS;
				options.machine_class = CLASS_R5000;
			} else if (!strcmp (*walk +2, "ppc")) {
				options.machine_target = TARGET_POWERPC;
				options.machine_class = CLASS_POWERPC;
			} else if (!strcmp (*walk +2, "ppc64")) {
				options.machine_target = TARGET_POWERPC;
				options.machine_class = CLASS_POWERPC;
			} else {
				goto unknown_option;
			}
			break;
			/*}}}*/
			/*{{{  i*/
		case 'i':
			switch (opchars[2]) {
			case 'i':
				options.inline_options |= INLINE_IN;
				break;
			case 'o':
				options.inline_options |= INLINE_OUT;
				break;
			case 'a':
				options.inline_options |= INLINE_ALT;
				break;
			case 's':
				options.inline_options |= INLINE_SCHEDULER;
				break;
			case 't':
				options.inline_options |= INLINE_LDTIMER;
				break;
			case 'T':
				options.inline_options |= INLINE_TIN;
				break;
			case 'X':
				options.inline_options |= INLINE_IN;
				options.inline_options |= INLINE_OUT;
				options.inline_options |= INLINE_ALT;
				options.inline_options |= INLINE_SCHEDULER;
				options.inline_options |= INLINE_LDTIMER;
				options.inline_options |= INLINE_TIN;
				break;
			default:
				goto unknown_option;
			}
			break;
			/*}}}*/
			/*{{{  E*/
		case 'E':
			/* experimental stuff */
			switch (opchars[2]) {
			case '1':
				options.inline_options |= INLINE_IN2;
				break;
			case '2':
				options.inline_options |= INLINE_OUT2;
				break;
			case '3':
				options.inline_options |= INLINE_MIN_MOUT;
				break;
			case '4':
				options.inline_options |= INLINE_REDUCED_POLL;
				break;
			case '5':
				options.inline_options |= INLINE_ALTERNATE_POLICY;
				break;
			default:
				goto unknown_option;
			}
			break;
			/*}}}*/
			/*{{{  V*/
		case 'V':
			if (opchars[2] == '\0') {
				dump_version (stdout);
				exit (EXIT_SUCCESS);
			}
			goto unknown_option;
			/*}}}*/
			/*{{{  long options (--)*/
		case '-':
			/* long option */
			if (!strncmp (*walk + 2, "tlp", 3)) {
				if ((*walk)[5] == '\0') {
					dump_supported_tlpifs (stdout);
					exit (EXIT_SUCCESS);
				} else if (!strcmp (*walk + 5, "-null")) {
					options.tlp_interface = TLP_NULL;
				} else if (!strcmp (*walk + 5, "-kyb")) {
					options.tlp_interface = TLP_KYB;
				} else if (!strcmp (*walk + 5, "-scr")) {
					options.tlp_interface = TLP_SCR;
				} else if (!strcmp (*walk + 5, "-err")) {
					options.tlp_interface = TLP_ERR;
				} else if (!strcmp (*walk + 5, "-kybscr")) {
					options.tlp_interface = TLP_KYBSCR;
				} else if (!strcmp (*walk + 5, "-kyberr")) {
					options.tlp_interface = TLP_KYBERR;
				} else if (!strcmp (*walk + 5, "-screrr")) {
					options.tlp_interface = TLP_SCRERR;
				} else if (!strcmp (*walk + 5, "-kybscrerr") || !strcmp (*walk + 5, "-std")) {
					options.tlp_interface = TLP_KYBSCRERR;
				} else if (!strcmp (*walk + 5, "-fsts")) {
					options.tlp_interface = TLP_FSTS;
				} else if (!strcmp (*walk + 5, "-fstsmem")) {
					options.tlp_interface = TLP_FSTSMEM;
				} else if (!strcmp (*walk + 5, "-fstsmem")) {
					options.tlp_interface = TLP_FSTSSIZEDMEM;
				} else {
					goto unknown_option;
				}
			} else if (!strcmp (*walk + 2, "nfw")) {
				options.rtflags |= TLP_FORK_NOWAIT;
			} else if (!strcmp (*walk + 2, "new")) {
				options.rtflags |= TLP_EXT_NOWAIT;
			} else if (!strcmp (*walk + 2, "aeconst")) {
				options.aeconst = 1;
			} else if (!strcmp (*walk + 2, "int-noopt")) {
				options.internal_options |= INTERNAL_NOOPT;
			} else if (!strcmp (*walk + 2, "int-nocmap")) {
				options.internal_options |= INTERNAL_NOCONSTMAP;
			} else if (!strcmp (*walk + 2, "int-dpt")) {
				options.internal_options |= INTERNAL_DUMPPROCTAB;
			} else if (!strcmp (*walk + 2, "version")) {
				dump_version (stdout);
				exit (EXIT_SUCCESS);
			} else if (!strcmp (*walk + 2, "help")) {
				usage (stdout);
				exit (EXIT_SUCCESS);
			} else if (!strcmp (*walk + 2, "gstabs")) {
				options.gstabs = 1;
			} else if (!strcmp (*walk + 2, "gstabs+")) {
				options.gstabs = 2;
			} else if (!strcmp (*walk + 2, "anno")) {
				options.always_annotate = 1;
			} else if (!strcmp (*walk + 2, "cnpfx")) {
				i++, walk++;
				if ((i == argc) || !*walk) {
					fprintf (stderr, "%s: option %s requires an argument\n", progname, walk[-1]);
					exit (EXIT_FAILURE);
				}
				options.extref_prefix = *walk;
			} else if (!strcmp (*walk + 2, "cttd")) {
				/* goes in as a top-level interface flag as well */
				options.rtflags |= TLP_CTTD;
				options.use_cttd = 1;
			} else if (!strcmp (*walk + 2, "underflow-error")) {
				options.underflow_error = 1;
			} else if (!strcmp (*walk + 2, "exporttab")) {
				i++, walk++;
				if ((i == argc) || !*walk) {
					fprintf (stderr, "%s: option %s requires an argument\n", progname, walk[-1]);
					exit (EXIT_FAILURE);
				}
				options.etab_filename = *walk;
			} else if (!strcmp (*walk + 2, "rmoxmode")) {
				i++, walk++;
				if ((i == argc) || !*walk) {
					fprintf (stderr, "%s: option %s requires an argument\n", progname, walk[-1]);
					exit (EXIT_FAILURE);
				}
				if (options.rmoxmode != RM_NONE) {
					fprintf (stderr, "%s: warning: RMoX mode is already set!\n", progname);
				} else if (!strcmp (*walk, "app")) {
					options.rmoxmode = RM_APP;
				} else if (!strcmp (*walk, "drv")) {
					options.rmoxmode = RM_DRV;
				} else if (!strcmp (*walk, "srv")) {
					options.rmoxmode = RM_SRV;
				} else if (!strcmp (*walk, "fs")) {
					options.rmoxmode = RM_FS;
				} else if (!strcmp (*walk, "net")) {
					options.rmoxmode = RM_NET;
				} else {
					fprintf (stderr, "%s: error: unknown RMoX mode [%s]\n", progname, *walk);
					exit (EXIT_FAILURE);
				}
			} else {
				goto unknown_option;
			}
			break;
			/*}}}*/
			/*{{{  default / unknown_option:*/
		default:
		unknown_option:
			fprintf (stderr, "%s: warning: unrecognised option %s\n", progname, *walk);
			break;
			/*}}}*/
		}
	}
	if ((argc == 1) || !input_filename) {
		usage (stderr);
		exit (1);
	}
	if (options.kernel_interface == KRNLIFACE_NONE) {
#ifdef MP_ENABLE
		options.kernel_interface = KRNLIFACE_MP | KRNLIFACE_NEWCCSP;
#else
		options.kernel_interface = KRNLIFACE_NEWCCSP;
#endif
	}
	if (options.kernel_interface & KRNLIFACE_MP) {
		options.kernel_interface |= KRNLIFACE_NEWCCSP;
		options.inline_options = 0;
	}
	if (options.inline_options && !(options.kernel_interface & KRNLIFACE_NEWCCSP)) {
		fprintf (stderr, "%s: cannot use inlining without -kf\n", progname);
		exit (1);
	}
	if (options.always_annotate) {
		if (!options.annotate_output && options.verbose) {
			fprintf (stderr, "%s: annotating output.\n", progname);
		}
		options.annotate_output = 1;
	}
	if (!options.extref_prefix) {
		options.extref_prefix = string_dup ("");
	} else {
		options.extref_prefix = string_dup (options.extref_prefix);
	}

	/* fixup export table dump file if requested */
	if (options.etab_filename && (!strlen (options.etab_filename) || !strcmp (options.etab_filename, "."))) {
		/* means pick something sensible based on the source filename */
		for (i=strlen (input_filename)-1; (input_filename[i] != '.') && i>0; i--);
		if (!i) {
			i = strlen (input_filename);
		}
		options.etab_filename = (char *)smalloc (i+6);
		memcpy (options.etab_filename, input_filename, i);
		strcpy (options.etab_filename + i, ".etab");
	} else if (options.etab_filename) {
		char *copy = string_dup (options.etab_filename);

		options.etab_filename = copy;
	}

	if (options.etab_filename) {
		/* remove the file if it exists */
		if (!access (options.etab_filename, W_OK)) {
			unlink (options.etab_filename);
		}
	}
	
#ifdef HOST_CPU_IS_I486
	/* if we're using CPUID, grab it now */
	if (options.use_cpuid) {
		options.machine_class = get_processor_class ();
		if (check_intel_processor ()) {
			int m_opts = get_intel_options ();

			if (options.verbose) {
				fprintf (stderr, "%s: CPU features: ", progname);
				if (m_opts & OPTION_MMX) {
					fprintf (stderr, "mmx ");
				}
				if (m_opts & OPTION_SSE2) {
					fprintf (stderr, "sse2 ");
				}
				if (m_opts & OPTION_CMOVC) {
					fprintf (stderr, "cmovc ");
				}
				if (m_opts & OPTION_FCOMI) {
					fprintf (stderr, "fcomi ");
				}
				if (m_opts & OPTION_CMPXCHG8) {
					fprintf (stderr, "swap64 ");
				}
				if (m_opts & OPTION_FXSR) {
					fprintf (stderr, "fxsr ");
				}
				if (m_opts & OPTION_XMM) {
					fprintf (stderr, "xmm ");
				}
				if (m_opts & OPTION_SYSENTER) {
					fprintf (stderr, "sysenter");
				}
				fprintf (stderr, "\n");
			}
			options.machine_options |= m_opts;
		} else {
			fprintf (stderr, "%s: warning: Not an intel processor, won\'t get features from CPUID\n", progname);
		}
	}
#endif	/* HOST_CPU_IS_I486 */
	if (options.verbose) {
		printf ("%s: generating %s code.\n", progname, machine_class_str (options.machine_class));
	}
	/* fixup output_filename */
	if (!output_filename) {
		for (i=strlen (input_filename)-1; (input_filename[i] != '.') && i>0; i--);
		if (!i) {
			i = strlen (input_filename);
		}
		output_filename = (char *)smalloc (i+5);
		memcpy (output_filename, input_filename, i);
		switch (options.output_format) {
		case OUTPUT_ELF:
		default:
			strcpy (output_filename + i, ".o");
			break;
		case OUTPUT_ASM:
			strcpy (output_filename + i, ".s");
			break;
		case OUTPUT_ETC:
		case OUTPUT_RAW:
			strcpy (output_filename + i, ".etc");
			break;
		case OUTPUT_RTL0:
		case OUTPUT_RTL1:
		case OUTPUT_RTL2:
		case OUTPUT_RTL3:
			strcpy (output_filename + i, ".imc");
			break;
		}
	}
	if (options.verbose) {
		tranx86_banner (stdout);
		printf ("converting %s to %s\n", input_filename, output_filename);
	}
	/* semi-lame check to see if what we've got is a NOCC-generated .etc file */
	if (!strcmp (input_filename + (strlen (input_filename) - 4), ".etc")) {
		if (options.verbose) {
			printf ("assuming NOCC generated ETC input\n");
		}
		if (open_netc_file (input_filename)) {
			exit (1);
		}
		options.nocc_codegen = 1;
	} else if (open_tce_file (input_filename)) {
		exit (1);
	}
	t_arch = init_architecture (options.machine_class);
	if (!t_arch) {
		fprintf (stderr, "%s: failed to initialise architecture base for %s\n", progname, machine_class_str (options.machine_class));
		close_tce_file ();
		exit (EXIT_FAILURE);
	}
	if (options.verbose) {
		printf ("using target architecture: %s\n", t_arch->archname);
	}
	i = tranx86 (input_filename, output_filename, t_arch);
	if (!strcmp (input_filename + (strlen (input_filename) - 4), ".etc")) {
		close_netc_file ();
	} else {
		close_tce_file ();
	}

	if (options.etabfile) {
		fclose (options.etabfile);
		options.etabfile = NULL;
	}

	#ifdef TRACE_MEMORY
		ss_cleanup ();
	#endif
	if (i) {
		return 1;
	}
	return 0;
}
/*}}}*/
/*{{{  static void tranx86_banner (FILE *stream)*/
/*
 *	void tranx86_banner (FILE *stream)
 *	dumps program banner
 */
static void tranx86_banner (FILE *stream)
{
	fprintf (stream, "tranx86 " VERSION " -- occam ETC to Intel/i386 and MIPS code converter\n");
	fprintf (stream, "Copyright (C) 2000-2008 Fred Barnes <frmb@kent.ac.uk>\n");
	fprintf (stream, "This software is released under the GNU General Public License\n");
	return;
}
/*}}}*/
/*{{{  static int tranx86 (char *infile, char *outfile, arch_t *arch)*/
/*
 *	main driver
 */
static int tranx86 (char *infile, char *outfile, arch_t *arch)
{
	etc_chain *etc_code;
	rtl_chain *rtl_code, *x_rtl_code;
	int i, opt_didopt, labglob, labrm;
	char out_report_str[128];

	out_report_str[0] = '\0';
	if (!strcmp (infile + (strlen (infile) - 4), ".etc")) {
		etc_code = read_netc_chain ();
	} else {
		etc_code = read_etc_chain ();
	}
	if (!etc_code) {
		return -1;
	}
	if (options.output_format == OUTPUT_RAW) {
		if (dump_textual_etc (etc_code, outfile) < 0) {
			return -1;
		}
		return 0;
	} else if (options.output_format == OUTPUT_COMMENTS) {
		if (dump_comments (etc_code) < 0) {
			return -1;
		}
		return 0;
	}
	/* make the conversion from FPREMFIRST/FPREMSTEP here */
	etc_code = sub_fpremfirststep (etc_code);
	if (!etc_code) {
		return -1;
	}
	/* then if we're using fast CCSP interface, put in I_{IN,OUT}{8,32} instrs */
	etc_code = etc_preoptimise (etc_code);
	if (!etc_code) {
		return -1;
	}
	if (options.output_format == OUTPUT_ETC) {
		if (dump_textual_etc (etc_code, outfile) < 0) {
			return -1;
		}
		return 0;
	}
	/* convert */
	rtl_code = etc_to_rtl (etc_code, arch);
	if (!rtl_code) {
		return -1;
	}
	rtl_refix_codeblocks (rtl_code);
	if (options.output_format == OUTPUT_RTL0) {
		if (dump_textual_rtl (rtl_code, outfile, arch) < 0) {
			return -1;
		}
		return 0;
	}
	#if PARANOID == 1
		if (rtl_check_consistency (rtl_code)) {
			fprintf (stderr, "%s: internal error -- failed IMC consistency check 0\n", progname);
			return -1;
		}
	#endif	/* PARANOID == 1 */
	/* tidy up the RTL (?) */

	/* compact data-blocks */
	x_rtl_code = rtl_compact_datablocks (rtl_code);
	if (!x_rtl_code) {
		fprintf (stderr, "%s: error while compacting datablocks\n", progname);
		return -1;
	}
	rtl_code = x_rtl_code;
	rtl_refix_codeblocks (rtl_code);

	#if PARANOID == 1
		if (rtl_check_consistency (rtl_code)) {
			fprintf (stderr, "%s: internal error -- failed IMC consistency check 1\n", progname);
			return -1;
		}
	#endif	/* PARANOID == 1 */

	/* remove unused textual labels (necessary to prevent assembler name-conflicts) */
	labrm = rtl_remove_deadnamelabels (rtl_code);
	if (labrm > 0) {
		sprintf (out_report_str, "%d string label%s removed", labrm, (labrm == 1) ? "" : "s");
	}

	/* relocate data */
	x_rtl_code = rtl_relocate_data (rtl_code);
	if (!x_rtl_code) {
		fprintf (stderr, "%s: error while moving relocatable data\n", progname);
		return -1;
	}

	rtl_refix_codeblocks (rtl_code);

	/* oki, architecture specific re-writing in here.. */
	if (!rtl_prevalidate_rtl (rtl_code, arch)) {
		fprintf (stderr, "%s: failed to pre-validate instruction(s).\n", progname);
		return -1;
	}

	/* link jump instruction arguments to target instructions */
	/* (aka links labels and label references to each other) */
	if (rtl_link_jumps (rtl_code)) {
		/* rtl_link_jumps will emit the error */
		return -1;
	}

	rtl_code = rtl_remove_empty (x_rtl_code);

	/* compact code-blocks */
	x_rtl_code = rtl_compact_codeblocks (rtl_code);
	if (!x_rtl_code) {
		fprintf (stderr, "%s: error while compacting codeblocks\n", progname);
		return -1;
	}
	rtl_code = x_rtl_code;
	rtl_code = rtl_remove_empty (rtl_code);
	rtl_refix_codeblocks (rtl_code);
	#if PARANOID == 1
		if (rtl_check_consistency (rtl_code)) {
			fprintf (stderr, "%s: internal error -- failed IMC consistency check 2\n", progname);
			return -1;
		}
	#endif	/* PARANOID == 1 */

	/* glob labels together */
	labglob = rtl_glob_labels (rtl_code);
	if (options.verbose) {
		sprintf (out_report_str + strlen (out_report_str), "%s%d label%s merged", (strlen (out_report_str) > 0) ? ", " : "", labglob, (labglob == 1) ? "" : "s");
	}

	/* register trace */
	i = rtl_trace_regs (rtl_code);
	if (i < 0) {
		fprintf (stderr, "%s: error while tracing registers\n", progname);
		return -1;
	}
	rtl_refix_codeblocks (rtl_code);
	#if PARANOID == 1
		if (rtl_check_consistency (rtl_code)) {
			fprintf (stderr, "%s: internal error -- failed IMC consistency check 3\n", progname);
			return -1;
		}
	#endif	/* PARANOID == 1 */
	rtl_set_lastvreg (i);

	/* output stage 1 RTL here (before optimisation) */
	if (options.output_format == OUTPUT_RTL1) {
		if (dump_textual_rtl (rtl_code, outfile, arch) < 0) {
			return -1;
		}
		return 0;
	}

	if (options.internal_options & INTERNAL_NOOPT) {
		fprintf (stderr, "%s: warning: not running optimiser\n", progname);
		/* don't optimise */
	} else {
		opt_didopt = optimise_run (rtl_code, arch);
		if (opt_didopt < 0) {
			fprintf (stderr, "%s: error during optimisation\n", progname);
			return -1;
		}
		if (options.verbose) {
			sprintf (out_report_str + strlen (out_report_str), "%s%d optimisation%s", (strlen (out_report_str) > 0) ? ", " : "", opt_didopt, (opt_didopt == 1) ? "" : "s");
		}
		#if PARANOID == 1
			if (rtl_check_consistency (rtl_code)) {
				fprintf (stderr, "%s: internal error -- failed IMC consistency check 5\n", progname);
				return -1;
			}
		#endif	/* PARANOID == 1 */
	}

	/* output stage 2 RTL here (before colouring) */
	if (options.output_format == OUTPUT_RTL2) {
		if (dump_textual_rtl (rtl_code, outfile, arch) < 0) {
			return -1;
		}
		return 0;
	}

	/* check instruction sanity */
	if (!rtl_validate_rtl (rtl_code, arch)) {
		fprintf (stderr, "%s: invalid instruction\n", progname);
		return -1;
	}

	/* colour registers */
	if (colour_registers (rtl_code, arch) < 0) {
		fprintf (stderr, "%s: error colouring registers\n", progname);
		if (options.output_format != OUTPUT_RTL3) {
			return -1;
		} else {
			fprintf (stderr, "%s: notice: continuing IMC3 dump\n", progname);
		}
	}

	#if PARANOID == 1
		if (rtl_check_consistency (rtl_code)) {
			fprintf (stderr, "%s: internal error -- failed IMC consistency check 6\n", progname);
			return -1;
		}
	#endif	/* PARANOID == 1 */
	if (options.verbose) {
		printf ("%s\n", out_report_str);
	}

	glob_out_icount = rtl_count_instrs (rtl_code);
	switch (options.output_format) {
	case OUTPUT_RTL3:
		if (dump_textual_rtl (rtl_code, outfile, arch) < 0) {
			return -1;
		}
		break;
	case OUTPUT_ASM:
		if (arch->code_to_asm (rtl_code, outfile) < 0) {
			return -1;
		}
		break;
	case OUTPUT_ELF:
		if (options.drop_assembler) {
			static char other_outfile[FILENAME_MAX];
			int oflen;

			oflen = strlen (outfile);
			strcpy (other_outfile, outfile);
			if ((oflen >= 2) && (other_outfile[oflen - 1] == 'o') && (other_outfile[oflen - 2] == '.')) {
				/* change name from blah.o to blah.s */
				other_outfile[oflen - 1] = 's';
			} else if ((oflen + 3) < FILENAME_MAX) {
				/* just add .s */
				strcpy (other_outfile + oflen, ".s");
			} else {
				fprintf (stderr, "%s: error: cannot make assembler filename! (from %s)\n", progname, outfile);
				return -1;
			}
			if (arch->code_to_asm (rtl_code, other_outfile) < 0) {
				return -1;
			}
			if (dump_elf (NULL, other_outfile, outfile, arch) < 0) {
				return -1;
			}
		} else if (dump_elf (rtl_code, NULL, outfile, arch) < 0) {
			return -1;
		}
		break;
	default:
		fprintf (stderr, "%s: error: no output!\n", progname);
		break;
	}
	if (options.verbose && glob_in_icount) {
		printf ("%s: generated %ld instructions from %ld (in:out ratio is 1:%.3f)\n", progname, glob_out_icount, glob_in_icount,
			((double)glob_out_icount / (double)glob_in_icount));
	}

	return 0;
}
/*}}}*/
/*{{{  static void usage (FILE *stream)*/
/*
 *	void usage (FILE *stream)
 *	Dumps the usage information
 */
static void usage (FILE *stream)
{
	tranx86_banner (stream);
	fprintf (stream, "\n");
	fprintf (stream, "usage: %s [options] [-o <output-file>] <input.tce>\n", progname);
	fprintf (stream, "where options are:\n");
	/* general options */
	fprintf (stream, "\t-c   required if this is not a main module\n");
	fprintf (stream, "\t-l   translate into dynamic processes\n");
	fprintf (stream, "\t-f   suppress FPU initialisation\n");
	fprintf (stream, "\t-n   disable check translation (CSNGL, CSUB0, CCNT1)\n");
#ifdef HOST_CPU_IS_I486
	fprintf (stream, "\t-u   use CPUID to get processor info (x86 only)\n");
#endif
	fprintf (stream, "\t-v   verbose output\n");
	fprintf (stream, "\n");
	/* output options */
	fprintf (stream, "\t-e   output to ELF object file (default)\n");
	fprintf (stream, "\t-s   output to Linux/x86 assembler source file\n");
	fprintf (stream, "\t-sa  output to Linux/x86 assembler source file with annotations\n");
	fprintf (stream, "\t-es  output to ELF object file via assembler file\n");
	fprintf (stream, "\t-d   convert input file to textual ETC\n");
	fprintf (stream, "\t-r   dump raw textual ETC (no processing)\n");
	fprintf (stream, "\t-rN  dump N-th stage IMC N=[0..3]\n");
	fprintf (stream, "\t-raN dump N-th stage IMC with annotations N=[0..3]\n");
	fprintf (stream, "\t-C   dump #PRAGMA COMMENTs as text\n");
	fprintf (stream, "\n");
	/* debugging */
	fprintf (stream, "\t-H   translate for HALT error mode (default)\n");
	fprintf (stream, "\t-S   translate for STOP error mode\n");
	fprintf (stream, "\t-do  overflow debugging\n");
	fprintf (stream, "\t-dd  deadlock debugging\n");
	fprintf (stream, "\t-dr  range/stop debugging\n");
	fprintf (stream, "\t-df  floating-point debugging\n");
	fprintf (stream, "\t-dX  full debugging (-do, -dd, -dr, -df)\n");
	fprintf (stream, "\t-di  insert debug markers (very expensive)\n");
	fprintf (stream, "\t-dm  run-time memory debugging (requires run-time kernel support)\n");
	fprintf (stream, "\t-ds  display memory pool statistics at program exit\n");
	fprintf (stream, "\t-dt  enable debugging traces\n");
	/* kernel interface options */
	fprintf (stream, "\t-kc  multi-processor CCSP kernel interface (MP default)\n");
	fprintf (stream, "\t-kf  fast CCSP kernel interface (required for inlining) (UP default)\n");
	fprintf (stream, "\t-km  MESH kernel interface\n");
	fprintf (stream, "\t-kl  CSP/Linux interface\n");
	fprintf (stream, "\t-kr  RMoX interface\n");
	fprintf (stream, "\t-ki  inline-kernel interface (experimental)\n");
	/* inlining things */
	fprintf (stream, "\t-ii  inline IN instructions\n");
	fprintf (stream, "\t-io  inline OUT, OUTBYTE and OUTWORD instructions\n");
	fprintf (stream, "\t-ia  inline parts of the ALT construct\n");
	fprintf (stream, "\t-is  inline parts of the scheduler\n");
	fprintf (stream, "\t-it  inline LDTIMER (requires CPU timers and a kroc_clock)\n");
	fprintf (stream, "\t-iT  inline TIN (requires CPU timers and a kroc_clock)\n");
	fprintf (stream, "\t-iX  full inlining (-ii, -io, -ia, -is, -it, -iT)\n");
	/* experimental things */
	fprintf (stream, "\t-E1  experimental IN inlining\n");
	fprintf (stream, "\t-E2  experimental OUT inlining\n");
	fprintf (stream, "\t-E3  experimental MIN/MOUT inlining\n");
	fprintf (stream, "\t-E4  reduced number of kernel reschedules\n");
	fprintf (stream, "\t-E5  alternate inline rescheduling policy\n");
	/* code generation options */
	fprintf (stream, "\n");
	fprintf (stream, "\t-p | -p1      check sync-flag for reschedule on loop ends\n");
	fprintf (stream, "\t-p2           check sync-flag and run-queue for resched on loop ends\n");
	fprintf (stream, "\t-m<thing>     set target architecture/class/options\n");
	fprintf (stream, "\t-m            gives list of available <thing>s\n");
	fprintf (stream, "\t--int-nocmap  disable constant mapping\n");
	fprintf (stream, "\t--int-noopt   disable optimiser\n");
	fprintf (stream, "\t--int-dpt     show process table\n");
	fprintf (stream, "\t--gstabs      pass --gstabs option to assembler (if appropriate)\n");
	fprintf (stream, "\t--gstabs+     pass --gstabs+ option to assembler (if appropriate)\n");
	fprintf (stream, "\t-nd           disallow dynamic memory (MALLOC, MRELEASE, MNEW & MFREE)\n");
#ifdef USER_DEFINED_CHANNELS
	fprintf (stream, "\t-nec          don\'t generate external channel checks\n");
#endif
	fprintf (stream, "\t-nsymops      don\'t generate dynamic lib symbol pseudo-ops in output\n");
#ifdef MP_ENABLE
	fprintf (stream, "\t-mp           generate multi-processor safe atomic ops\n");
#endif
	fprintf (stream, "\t--anno        always annotate\n");
	fprintf (stream, "\t--aeconst     alternate-endian constants\n");
	/* anything else ?? */
	fprintf (stream, "\n");
	fprintf (stream, "\t--tlp-<if>    set top-level PROC interface (not with -c), overrides\n");
	fprintf (stream, "\t              automatic checking.  --tlp gives list of available <if>s\n");
	fprintf (stream, "\t--nfw         do not wait for global FORKed processes at exit\n");
	fprintf (stream, "\t--new         do not wait for blocking sys-calls at exit\n");
	fprintf (stream, "\t--cnpfx <str> use external C name prefix string <str>\n");
	fprintf (stream, "\t--cttd        input compiled with chan-type type descriptions\n");
	fprintf (stream, "\t--underflow-error\n");
	fprintf (stream, "\t              treat floating-point underflow as an error\n");
	fprintf (stream, "\t--exporttab <file>\n");
	fprintf (stream, "\t              write EXPORT table to specified file (removes if none)\n");
	fprintf (stream, "\t--rmoxmode <type>\n");
	fprintf (stream, "\t              generate alternate RMoX main module prolog, <type> is one\n");
	fprintf (stream, "\t              of: app, drv, srv, fs, net\n");
	fprintf (stream, "\n");
	fprintf (stream, "\t-V | --version    dump version and exit\n");
	fprintf (stream, "\t-h | --help       this help text\n");
	return;
}
/*}}}*/
/*{{{  static void dump_supported_things (FILE *stream)*/
/*
 *	void dump_supported_things (FILE *stream)
 *	Dumps information about supported targets
 */
static void dump_supported_things (FILE *stream)
{
	fprintf (stream, "Supported target options (more than one may be specified depending on target):\n");
	fprintf (stream, "\t-m386      select 386 target\n");
	fprintf (stream, "\t-m486      select 486 target\n");
	fprintf (stream, "\t-m586      select pentium target\n");
	fprintf (stream, "\t-m686      select pentium 2/3/pro target\n");
	fprintf (stream, "\t-mMMX      enable MMX instructions\n");
	fprintf (stream, "\t-mSSE2     enable SSE2 instructions\n");
	fprintf (stream, "\t-mcmovc    enable conditional move instructions\n");
	fprintf (stream, "\t-mmips     select MIPS target (defaults to r5000)\n");
	fprintf (stream, "\t-mr3k      select MIPS R3000 target\n");
	fprintf (stream, "\t-mr43k     select MIPS R4300 target\n");
	fprintf (stream, "\t-mr46k     select MIPS R4600 target\n");
	fprintf (stream, "\t-mr5k      select MIPS R5000 target\n");
	fprintf (stream, "\t-msparcv8  select SPARC V8 target\n");
	fprintf (stream, "\t-mppc      select Power-PC target\n");
#ifdef HOST_CPU_IS_I486
	fprintf (stream, "The -u (CPUID) flag will automatically determine which of -mMMX and -mcmovc these are possible\n");
#endif
	return;
}
/*}}}*/
/*{{{  static void dump_supported_tlpifs (FILE *stream)*/
/*
 *	void dump_supported_tlpifs (FILE *stream)
 *	dumps information about supported top-level PROC interfaces
 */
static void dump_supported_tlpifs (FILE *stream)
{
	fprintf (stream, "Supported top-level PROC interfaces:\n");
	fprintf (stream, "\t--tlp-null           PROC p ()\n");
	fprintf (stream, "\t--tlp-kyb            PROC p (CHAN BYTE keyboard?)\n");
	fprintf (stream, "\t--tlp-scr            PROC p (CHAN BYTE screen!)\n");
	fprintf (stream, "\t--tlp-err            PROC p (CHAN BYTE error!)\n");
	fprintf (stream, "\t--tlp-kybscr         PROC p (CHAN BYTE keyboard?, screen!)\n");
	fprintf (stream, "\t--tlp-kyberr         PROC p (CHAN BYTE keyboard?, error!)\n");
	fprintf (stream, "\t--tlp-screrr         PROC p (CHAN BYTE screen!, error!)\n");
	fprintf (stream, "\t--tlp-kybscrerr      PROC p (CHAN BYTE keyboard?, screen!, error!)\n");
	fprintf (stream, "\t--tlp-fsts           PROC p (CHAN SP fs?, ts!)\n");
	fprintf (stream, "\t--tlp-fstsmem        PROC p (CHAN SP fs?, ts!, []INT mem)\n");
	fprintf (stream, "\t--tlp-fstssizedmem   PROC p (CHAN SP fs?, ts!, [..]INT mem)\n");
	fprintf (stream, "\t--tlp-std            same as --tlp-kybscrerr (standard stdin, stdout, stderr interface)\n");
	return;
}
/*}}}*/
/*{{{  static void dump_version (FILE *stream)*/
/*
 *	void dump_version (FILE *stream)
 *	dumps version information
 */
static void dump_version (FILE *stream)
{
	fprintf (stream, "tranx86 " VERSION "\n");
	return;
}
/*}}}*/
/*{{{  char *machine_class_str (int mclass)*/
/*
 *	char *machine_class_str (int mclass)
 *	returns a string representing the machine class
 */
char *machine_class_str (int mclass)
{
	static char *mtable[] = {"<unknown>", "386", "486", "586", "686", "R3000", "R6000", "R4300", "R4600", "R5000", "Nevada", "R8000", "R10000", "sparcv8", "powerpc"};

	switch (mclass) {
	default:
		return mtable[0];
	case CLASS_386:
		return mtable[1];
	case CLASS_486:
		return mtable[2];
	case CLASS_586:
		return mtable[3];
	case CLASS_686:
		return mtable[4];
	case CLASS_R3000:
		return mtable[5];
	case CLASS_R6000:
		return mtable[6];
	case CLASS_R4300:
		return mtable[7];
	case CLASS_R4600:
		return mtable[8];
	case CLASS_R5000:
		return mtable[9];
	case CLASS_NEVADA:
		return mtable[10];
	case CLASS_R8000:
		return mtable[11];
	case CLASS_R10000:
		return mtable[12];
	case CLASS_SPARCV8:
		return mtable[13];
	case CLASS_POWERPC:
		return mtable[14];
	}
}
/*}}}*/
/*{{{  static arch_t *init_architecture (int mclass)*/
/*
 *	arch_t *init_architecture (int mclass)
 *	selects and initialises architecture code-gen
 */
static arch_t *init_architecture (int mclass)
{
	switch (mclass) {
	default:
		return NULL;
	case CLASS_386:
	case CLASS_486:
	case CLASS_586:
	case CLASS_686:
		return init_arch_i386 (mclass);
	case CLASS_R3000:
	case CLASS_R6000:
	case CLASS_R4300:
	case CLASS_R4600:
	case CLASS_R5000:
	case CLASS_NEVADA:
	case CLASS_R8000:
	case CLASS_R10000:
		/* don't run the optimiser for MIPS */
		options.internal_options |= INTERNAL_NOOPT;
		/* and don't use the constant map at the moment.. */
		options.internal_options |= INTERNAL_NOCONSTMAP;

		/* big-endian constants please */
		options.internal_options |= INTERNAL_BIGENDIAN;
		options.internal_options |= INTERNAL_ALIGNEDCODE;

		return init_arch_mips (mclass);
	case CLASS_SPARCV8:
		options.internal_options |= INTERNAL_BIGENDIAN;
		/* and don't use the constant map at the moment.. */
		options.internal_options |= INTERNAL_NOCONSTMAP;
		options.internal_options |= INTERNAL_ALIGNEDCODE;

		return init_arch_sparc (mclass);
	case CLASS_POWERPC:
		options.internal_options |= INTERNAL_BIGENDIAN;
		options.internal_options |= INTERNAL_NOCONSTMAP;
		options.internal_options |= INTERNAL_NOOPT;		/* at the moment.. */
		options.internal_options |= INTERNAL_TEMPFLOAT;		/* need this cos some FPU instructions only load/store in memory */
		options.internal_options |= INTERNAL_FLOATCONV;		/* need this too, since PowerPC can't do some conversions directly */
		options.internal_options |= INTERNAL_ALIGNEDCODE;

		return init_arch_ppc (mclass);
	}
}
/*}}}*/

