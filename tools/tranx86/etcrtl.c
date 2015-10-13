/*
 *	etcrtl.c - ETC to RTL converter
 *	Copyright (C) 2000-2008 Fred Barnes <frmb@kent.ac.uk>
 *	Parts based on tranetcp.occ Copyright (C) 1997 M D Poole
 *	MIPS modifications by Christian Jacobsen / Fred Barnes
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

/*{{{  includes/defines/etc.*/
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

#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#include "main.h"
#include "support.h"
#include "structs.h"
#include "transputer.h"
#include "trancomm.h"
#include "tstack.h"
#include "postmortem.h"
#include "etcdump.h"
#include "tstate.h"
#include "archdef.h"
#include "rtlops.h"
#include "stubtable.h"
#define WANT_TCOFF_TAGNAMES
#include "tcoff.h"
#undef WANT_TCOFF_TAGNAMES
#include "etcrtl.h"
#include "kif.h"

#ifndef EXIT_FAILURE
	#define EXIT_FAILURE 1
#endif	/* !EXIT_FAILURE */
/*}}}*/

#define CONVMISSING(X) fprintf (stderr, "%s: error: missing conversion for %s\n", progname, X)

/*{{{  private variables*/
static rtl_chain *rtl_head, *rtl_tail;
static ins_chain *ins_head, *ins_tail;
static int the_last_lab;
static tstate *the_last_tstate;
/*}}}*/
/*{{{  private function prototypes*/
static void gencodemap (procinf *pinf, void *param);
static int addlastlabtocodemap (tstate *ts);
static int check_top_level_process_signature (tstate *ts, char *namesig, int len, int *cdspecs_ptr);
static void gen_mobilespace_init (tstate *ts, int msp_offset, int count, int *slot_offsets, int *data_offsets);
static void compose_cond_jump (tstate *ts, int cond, int label);
static void deferred_cond (tstate *ts);
static void generate_overflow_code (tstate *ts, int dcode, arch_t *arch);
static void generate_overflowed_code (tstate *ts, int dcode, arch_t *arch);
static void generate_range_code (tstate *ts, int rcode, arch_t *arch);
static void declare_data_bytes_fixup (char *data, int len, tdfixup_t *fixups);
static void declare_data_bytes_spec_chain (char *data, int len, rtl_chain **head, rtl_chain **tail);
static void do_code_primary (tstate *ts, int prim, int operand, arch_t *arch);
static void do_code_special (tstate *ts, int ins, int opd, arch_t *arch);
static void do_code_nocc_special (tstate *ts, etc_chain **ecodeptr, arch_t *arch);
static void do_code_secondary (tstate *ts, int sec, arch_t *arch);
static void generate_constmapped_21instr (tstate *ts, int etc_instr, int instr, int src_reg1, int src_reg2, int dst_reg, int usecc);
static void translate_csub0 (tstate *ts, arch_t *arch);
static void translate_range_check (tstate *ts, int lwb, arch_t *arch, int ecode);
static void make_c_name (char *src, int slen, char *dst);
static void set_line_pending (tstate *ts, int lineno);
static void set_file_pending (tstate *ts, char *fname, int flen);
static void set_proc_pending (tstate *ts, char *pname, int plen);
static int is_power_of_two (const int val);
static ins_chain **ptr_add_to_ins_chain (void);
/*}}}*/
/*{{{  private constants (tlp checking)*/
#define TLU_UNKNOWN 0
#define TLU_INPUT 1
#define TLU_OUTPUT 2

#define TLC_UNKNOWN 0
#define TLC_KEYBOARD 1
#define TLC_SCREEN 2
#define TLC_ERROR 3
#define TLC_FS 4
#define TLC_TS 5
#define TLC_MEM 6
#define TLC_SIZEDMEM 7
/*}}}*/

/*{{{  static void generate_call (tstate *ts, etc_chain *etc_code, arch_t *arch, int stub) */
static void generate_call (tstate *ts, etc_chain *etc_code, arch_t *arch, int stub)
{
	ins_chain *st_first = NULL, *st_last = NULL;
	char *d_str = string_ndup (etc_code->o_bytes, etc_code->o_len);
	int kernel_call = -1;
	int i = 0;

	/* handle special calling sequences here (spares pain in optimisation) */
	/* NOTE: these should never be called directly! -- stubtable.[ch] and translation of
	 * I_CALL inline these (can help reduce cache displacement) */

	switch (d_str[0]) {
	case 'B':
		if ((d_str[1] == 'X') && (d_str[2] == '.')) {
			i = 3;
			kernel_call = K_BX_DISPATCH;
		} else if (d_str[1] == '.') {
			i = 2;
			kernel_call = K_B_DISPATCH;
		}
		break;
	case 'C':
		if (d_str[1] == '.') {
			i = 1;
		}
		if ((strlen (d_str) >= 5) && (!strncmp (d_str, "CIF.", 4))) {
			i = 6;
		}
		break;
	case 'K':
		if ((d_str[1] == 'R') && (d_str[2] == '.')) {
			i = 4;
			kernel_call = K_KERNEL_RUN;
		}
		break;
	}

	switch (i) {
	case 0:
		/* normal call */

		if (stub || options.nocc_codegen) {
			/* if using NOCC, remember to handle return-address (goes at WS offset 0 always, adjustment done) */
			st_first = compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 0, ARG_REGIND, REG_WPTR);
			add_to_ins_chain (st_first);
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_NAMEDLABEL, string_dup (d_str)));
			st_last = compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0);
			add_to_ins_chain (st_last);
		} else {
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_NAMEDLABEL, string_dup (d_str)));
		}
		break;
	case 1:
		arch->compose_external_ccall (ts, stub, d_str, &st_first, &st_last);
		break;
	case 2:
	case 3:
	case 4:
		/* external B (blocking), BX (killable blocking) or KR (kernel-run) call */
		arch->compose_bcall (ts, i, kernel_call, stub, d_str, &st_first, &st_last);

		break;
	case 6:
		arch->compose_cif_call (ts, stub, d_str, &st_first, &st_last);
		break;
	default:
		st_first = st_last = NULL;
		fprintf (stderr, "%s: fatal: (i = %d) => impossible.  probably random stack corruption..\n", progname, i);
		exit (EXIT_FAILURE);
		break;
	}

	/* perhaps add it to the stub table */
	if (stub && st_first) {
		ins_chain *tmp_ins;

		/* scan backwards for the last set-label instruction (this should always succeed) */
		for (tmp_ins = ins_tail; tmp_ins && (tmp_ins->type != INS_SETLABEL); tmp_ins = tmp_ins->prev);
		
		if (tmp_ins) {
			procinf *tmp;

			add_to_stubtable (ArgLabel (tmp_ins->in_args[0]), st_first, st_last);
			tmp = procinf_lookup (etc_code->o_bytes, etc_code->o_len);
			tmp->eplab = ArgLabel (tmp_ins->in_args[0]);
			tmp->is_proc = 1;
		} else {
			fprintf (stderr, "%s: warning: unable to inline stub-call (%s) because it didn\'t have a SETLABEL\n", progname, d_str);
		}
	}

	sfree (d_str);
}
/*}}}*/

/*{{{  rtl_chain *etc_to_rtl (etc_chain *etc_code, arch_t *arch)*/
/*
 *	rtl_chain *etc_to_rtl (etc_chain *etc_code, arch_t *arch)
 *	converter wrapper/helper
 */
rtl_chain *etc_to_rtl (etc_chain *etc_code, arch_t *arch)
{
	rtl_chain *trtl;
	tstate *ts;
	int x_fn, x_opd, y_fn, y_opd;
	int i, le_l1, le_l2, le_wsoff;
	int ms_usage;
	char *tmp_string;
	char sbuffer[128];
	int tmp_reg;
	ins_chain *tmp_ins;
	rtl_chain *def_lib_head, *def_lib_tail;
	int set_ifacetype = TLP_INVALID;

	rtl_head = rtl_tail = NULL;
	ins_head = ins_tail = NULL;
	def_lib_head = def_lib_tail = NULL;

	ts = new_tstate ();
	ts->stack = new_tstack ();
	ts->stack->transtate = (void *)ts;
	tstate_initialise (ts, etc_code);
	ts->overflow_label = ++(ts->last_lab);
	ts->floaterr_label = ++(ts->last_lab);
	ts->filename_label = ++(ts->last_lab);
	ts->procedure_label = ++(ts->last_lab);
	ts->procfile_setup_label = ++(ts->last_lab);
	ts->insert_setup_label = ++(ts->last_lab);
	ts->range_entry_label = ++(ts->last_lab);
	ts->zerodiv_label = ++(ts->last_lab);
	if (options.internal_options & INTERNAL_TEMPFLOAT) {
		ts->floatspace_label = ++(ts->last_lab);
	} else {
		ts->floatspace_label = -1;
	}
	if (options.internal_options & INTERNAL_FLOATCONV) {
		ts->floatconv_label = ++(ts->last_lab);
	} else {
		ts->floatconv_label = -1;
	}
	tstack_fpclear (ts->stack, arch);
	/* ts->stack->fs_depth = 0; */
	ts->cond = CC_NONE;
	for (i=0; i<4; i++) {
		ts->floatrange_label[i] = ++(ts->last_lab);
	}

	/* module header */
	if (!options.not_main_module) {
		if (options.rmoxmode != RM_NONE) {
			/* generate alternative RMoX entry prolog */
			arch->compose_rmox_entry_prolog (ts, options.rmoxmode);
		} else {
			arch->compose_entry_prolog (ts);
		}
	}
	trtl = new_rtl ();
	trtl->type = RTL_SETNAMEDLABEL;
	trtl->u.label_name = string_dup ("base");
	add_to_rtl_chain (trtl);

	arch->compose_reset_fregs (ts);

	while (etc_code /* && !ts->end_of_module */) {
		ins_chain **savedheadptr = ptr_add_to_ins_chain ();

		x_fn = etc_code->fn;
		x_opd = etc_code->opd;
		if (x_fn < I_OPR) {
			/*{{{  primary instruction*/
			if (options.annotate_output) {
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_of_primary (ts->stack, x_fn, x_opd)));
			}
			glob_in_icount++;
			do_code_primary (ts, x_fn, x_opd, arch);
			/*}}}*/
		} else if (x_opd < (signed int)ETC_MAX) {
			/*{{{  ETC special*/
			ts->stack->old_ts_depth = ts->stack->ts_depth;
			ts->stack->old_fs_depth = ts->stack->fs_depth;
			switch (x_opd) {
				/*{{{  ETC0  (special instructions) */
			case ETC0:
				etc_code = etc_code->next;
				if (etc_code->fn != I_LDC) {
					fprintf (stderr, "%s: warning: ETC0 with no LDC.\n", progname);
					break;
				}
				if (options.annotate_output) {
					/*{{{  generate text description*/
					switch (etc_code->opd) {
					case FINISH_OP:
						strcpy (sbuffer, ".END");
						break;
					case I64TOREAL:
						strcpy (sbuffer, ".I64TOREAL");
						break;
					case BOOLINVERT:
						strcpy (sbuffer, ".BOOLINVERT");
						break;
					case NOTPROCESS:
						strcpy (sbuffer, ".NOTPROCESS");
						break;
					case WIDENSHORT:
						strcpy (sbuffer, ".WIDENSHORT");
						break;
					case FPPOP:
						strcpy (sbuffer, ".FPPOP");
						break;
					case STARTTABLE:
						strcpy (sbuffer, ".STARTTABLE");
						break;
					case CONTRSPLIT:
						strcpy (sbuffer, ".CONTRSPLIT");
						break;
					case CONTRJOIN:
						strcpy (sbuffer, ".CONTRJOIN");
						break;
					case CHECKNOTNULL:
						strcpy (sbuffer, ".CHECKNOTNULL");
						break;
					case SEMCLAIM:
						strcpy (sbuffer, ".SEMCLAIM");
						break;
					case SEMRELEASE:
						strcpy (sbuffer, ".SEMRELEASE");
						break;
					case SEMINIT:
						strcpy (sbuffer, ".SEMINIT");
						break;
					case RESCHEDULE:
						strcpy (sbuffer, ".RESCHEDULE");
						break;
					case INDIRECT_AREG:
						strcpy (sbuffer, ".INDIRECT_AREG");
						break;
					case INDIRECT_BREG:
						strcpy (sbuffer, ".INDIRECT_BREG");
						break;
					case INDIRECT_CREG:
						strcpy (sbuffer, ".INDIRECT_CREG");
						break;
					case RMWSMAP:
						strcpy (sbuffer, ".RMWSMAP");
						break;
					case MPPCLONE:
						strcpy (sbuffer, ".MPPCLONE");
						break;
					case MPPSERIALISE:
						strcpy (sbuffer, ".MPPSERIALISE");
						break;
					case MPPDESERIALISE:
						strcpy (sbuffer, ".MPPDESERIALISE");
						break;
					case LOADCODEMAP:
						sprintf (sbuffer, ".LOADCODEMAP %d", etc_code->next->opd);
						break;
					case FBARINIT:
						strcpy (sbuffer, ".FBARINIT");
						break;
					case FBARSYNC:
						strcpy (sbuffer, ".FBARSYNC");
						break;
					case FBARRESIGN:
						strcpy (sbuffer, ".FBARRESIGN");
						break;
					case FBARENROLL:
						strcpy (sbuffer, ".FBARENROLL");
						break;
					case R32SIN:
						strcpy (sbuffer, ".R32SIN");
						break;
					case R64SIN:
						strcpy (sbuffer, ".R64SIN");
						break;
					case R32COS:
						strcpy (sbuffer, ".R32COS");
						break;
					case R64COS:
						strcpy (sbuffer, ".R64COS");
						break;
					case DTRACE:
						strcpy (sbuffer, ".DTRACE");
						break;
					case KILLCALL:
						strcpy (sbuffer, ".KILLCALL");
						break;
					case WAIT_FOR_INTERRUPT:
						strcpy (sbuffer, ".WAIT_FOR_INTERRUPT");
						break;
					case R32TAN:
						strcpy (sbuffer, ".R32TAN");
						break;
					case R64TAN:
						strcpy (sbuffer, ".R64TAN");
						break;
					default:
						strcpy (sbuffer, "<unknown>");
						break;
					}
					sprintf (sbuffer + strlen (sbuffer), " [tsd=%d,%d]", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					/*}}}*/
				}
				switch (etc_code->opd) {
					/*{{{  CHECKNOTNULL*/
				case CHECKNOTNULL:		/* check mobile-channel (for null-ness) */
					glob_in_icount++;
					if (!options.disable_checking) {
						/* i can't imagine and cases where cc would be set from this.. */
						add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg, ARG_REG | ARG_IMP, REG_CC));
						add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_FLABEL, 0));
						do_code_secondary (ts, I_SETERR, arch);
						add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
					}
					break;
					/*}}}*/
					/*{{{  FINISH_OP*/
				case FINISH_OP:
					ts->end_of_module = 1;
					break;
					/*}}}*/
					/*{{{  I64TOREAL*/
				case I64TOREAL:
#if 0
fprintf (stderr, "*** I64TOREAL: ts_depth=%d, fs_depth=%d\n", ts->stack->ts_depth, ts->stack->fs_depth);
#endif
					ts->stack->old_a_reg = ts->stack->a_reg;
					ts->stack->old_b_reg = ts->stack->b_reg;
					ts->stack->old_c_reg = ts->stack->c_reg;
					deferred_cond (ts);
					tstack_setsec (ts->stack, I_POP, arch);
					add_to_ins_chain (compose_ins (INS_FILD64, 1, 0, ARG_REGIND, ts->stack->old_a_reg));
					arch->compose_fp_set_fround (ts, FPU_N);
					ts->stack->fs_depth++;
					tstate_ctofp (ts);
					break;
					/*}}}*/
					/*{{{  BOOLINVERT*/
				case BOOLINVERT:
					glob_in_icount++;
					if (ts->stack->must_set_cmp_flags) {
						add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg, ARG_REG | ARG_IMP, REG_CC));
					}
					if (ts->cond == CC_NONE) {
						ts->cond = CC_Z;
					} else {
						ts->cond ^= 1;
					}
					ts->stack->must_set_cmp_flags = 0;
					break;
					/*}}}*/
					/*{{{  NOTPROCESS*/
				case NOTPROCESS:
					glob_in_icount++;
					tstack_setprim (ts->stack, I_LDC, arch);
					tmp_ins = compose_ins (INS_MOVE, 1, 1, ARG_CONST, NOT_PROCESS, ARG_REG, ts->stack->a_reg);
					constmap_new (ts->stack->a_reg, VALUE_CONST, NOT_PROCESS, tmp_ins);
					add_to_ins_chain (tmp_ins);
					ts->stack->must_set_cmp_flags = 0;
					break;
					/*}}}*/
					/*{{{  WIDENSHORT*/
				case WIDENSHORT:
					glob_in_icount++;
					deferred_cond (ts);
					tmp_reg = arch->compose_widenshort (ts);
					ts->stack->c_reg = REG_UNDEFINED;
					ts->stack->must_set_cmp_flags = 1;
					break;
					/*}}}*/
					/*{{{  FPPOP*/
				case FPPOP:
					glob_in_icount++;
					arch->compose_fpop (ts, I_FPPOP);
					break;
					/*}}}*/
					/*{{{  STARTTABLE*/
				case STARTTABLE:
					glob_in_icount++;
					ts->incasetable = 1;
					ts->casetable_label = ++(ts->last_lab);
					ts->stack->old_a_reg = ts->stack->a_reg;
					tmp_reg = tstack_newreg (ts->stack);
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->casetable_label, ARG_REG, tmp_reg));
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGINDSIB, 4, ts->stack->a_reg, tmp_reg, ARG_REG, ts->stack->a_reg));
					add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, ts->stack->a_reg, ARG_REG, tmp_reg, ARG_REG, tmp_reg));
					add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, tmp_reg));
					add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->casetable_label));
					constmap_remove (ts->stack->a_reg);
					break;
					/*}}}*/
					/*{{{  CONTRSPLIT*/
				case CONTRSPLIT:
					{
						etc_chain *csins = etc_code->next;
						int cslab;

						/* need to peek at next instruction to find out which label it is -- expecting CJ (encoded in ETCL0)*/
						if (!etc_code->next) {
							fprintf (stderr, "%s: warning: CONTRSPLIT at end of chain.\n", progname);
							break;
						}

						while (csins) {
							if (!csins->next) {
								/* bad */
							} else if (csins->fn == I_OPR && csins->opd == ETCL0 && csins->next->fn == I_CJ) {
								/* found */
								csins = csins->next;
								break;
							} else if (csins->fn == I_OPR) {
								switch (csins->opd) {
									case I_FPCHKERR:
										/* skip */
										csins = csins->next;
										continue;
								}
							}
							csins = NULL;
						}
						
						if (!csins) {
							fprintf (stderr, "%s: warning: CONTRSPLIT without following CJ.\n", progname);
							break;
						}
						cslab = csins->opd;

						if (ts->stack->ts_depth > 0) {
							control_set_split (ts, cslab, ins_tail);

							deferred_cond (ts);
						}
					}
					break;
					/*}}}*/
					/*{{{  CONTRJOIN*/
				case CONTRJOIN:
					{
						int cslab;

						/* need to peek at next instruction to find out which label it is -- expecting SETLAB (encoded in ETC6/7)*/
						if (!etc_code->next) {
							fprintf (stderr, "%s: warning: CONTRJOIN at end of chain.\n", progname);
							break;
						}
						if ((etc_code->next->fn < I_OPR) || ((etc_code->next->opd != ETC7) && (etc_code->next->opd != ETC6)) ||
								!etc_code->next->next || (etc_code->next->next->fn != I_LDC)) {
							fprintf (stderr, "%s: warning: CONTRJOIN without following SETLAB.\n", progname);
							break;
						}
						cslab = etc_code->next->next->opd;

						if (ts->stack->ts_depth > 0) {
							control_set_join (ts, cslab, ins_tail);

							deferred_cond (ts);
						}
					}
					break;
					/*}}}*/
					/*{{{  SEMCLAIM*/
				case SEMCLAIM:
					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					/* claim semaphore pointed at by Areg.  new semaphore structure (2-word, pre-null, hidden bit) */
					if (options.kernel_interface & KRNLIFACE_MP) {
						arch->compose_deadlock_kcall (ts, K_SEM_CLAIM, 1, 0);
						ts->stack->ts_depth -= 1;
					} else {
						tmp_reg = tstack_newreg (ts->stack);
						tstack_setprim (ts->stack, I_STL, arch);
						
						add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 1, ARG_REGIND, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));				/* sem-val = 1 ? */
						add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_FLABEL, 5));					/* jump if yes */
						/* semaphore busy, join the queue */
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, W_LINK));			/* Wptr[Link] = null */
						add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REGIND, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));				/* sem-fptr = null ? */
						add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_FLABEL, 6));					/* jump if yes */
						/* just bung on the end */
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, (1 << WSH), ARG_REG, tmp_reg));	/* tmp = sem-bptr */
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND | ARG_DISP, tmp_reg, W_LINK));			/* tmp[link] = Wptr */
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, (1 << WSH)));	/* sem-bptr = Wptr */
						add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, 7));

						/* this here because it fits nicely: set sem-val to 0 */
						add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 5));
						add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_CONST, 0xfffffffe, ARG_REGIND, ts->stack->old_a_reg, ARG_REGIND, ts->stack->old_a_reg));	/* sem-val = 0 */
						add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, 8));

						/* nothing here yet, set both fptr and bptr */
						add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 6));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, (1 << WSH)));	/* sem-bptr = Wptr */
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND, ts->stack->old_a_reg));				/* sem-fptr = Wptr */
						add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 7));

						/* oki, ok queue, time to deschedule */
						if ((options.inline_options & INLINE_SCHEDULER) && arch->compose_inline_stopp) {
							arch->compose_inline_stopp (ts);
						} else {
							arch->compose_kcall (ts, K_STOPP, 0, 0);
						}
						add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 8));
					}
					ts->stack->must_set_cmp_flags = 0;
					break;
					/*}}}*/
					/*{{{  SEMRELEASE*/
				case SEMRELEASE:			/* NOTE: compiler has reserved local 0 for a temporary.. */
					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					/* release semaphore pointed at by Areg.  new semaphore structure (2-word, pre-null, hidden bit) */
					if (options.kernel_interface & KRNLIFACE_MP) {
						arch->compose_kcall (ts, K_SEM_RELEASE, 1, 0);
						ts->stack->ts_depth -= 1;
					} else {
						tmp_reg = tstack_newreg (ts->stack);
						tstack_setprim (ts->stack, I_STL, arch);
						
						add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REGIND, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));				/* sem-fptr = null ? */
						add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_FLABEL, 5));					/* jump if yes (no-one to wake) */

						/* other processes here, pick one of the queue and schedule it */
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, ts->stack->old_a_reg, ARG_REG, tmp_reg));			/* tmp = sem-fptr */
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_TEMP));		/* Wptr[Temp] = tmp */
						/* update sem-fptr */
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, tmp_reg, W_LINK, ARG_REG, tmp_reg));		/* tmp = tmp[Link] */
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND, ts->stack->old_a_reg));			/* sem-fptr = tmp */

						/* hack tstack for kcall */
						ts->stack->old_a_reg = tstack_newreg (ts->stack);
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, W_TEMP, ARG_REG, ts->stack->old_a_reg));
						if ((options.inline_options & INLINE_SCHEDULER) && arch->compose_inline_runp) {
							arch->compose_inline_runp (ts);
						} else {
							arch->compose_kcall (ts, K_RUNP, 1, 0);
						}
						add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, 6));

						add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 5));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 1, ARG_REGIND, ts->stack->old_a_reg));			/* sem-val = 1 */
						add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 6));
					}
					ts->stack->must_set_cmp_flags = 0;
					break;
					/*}}}*/
					/*{{{  SEMINIT*/
				case SEMINIT:
					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					/* initialise semaphore pointed at by Areg.  new semaphore structure (2-word, pre-null, hidden bit) */
					if (options.kernel_interface & KRNLIFACE_MP) {
						arch->compose_kcall (ts, K_SEM_INIT, 1, 0);
						ts->stack->ts_depth -= 1;
					} else {
						tmp_reg = tstack_newreg (ts->stack);
						tstack_setprim (ts->stack, I_STL, arch);
						
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 1, ARG_REGIND, ts->stack->old_a_reg));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, (1 << WSH)));
					}
					ts->stack->must_set_cmp_flags = 0;
					break;
					/*}}}*/
					/*{{{  RESCHEDULE*/
				case RESCHEDULE:
					glob_in_icount++;
					if ((options.inline_options & INLINE_SCHEDULER) && arch->compose_inline_full_reschedule) {
						arch->compose_inline_full_reschedule (ts);
					} else {
						arch->compose_kcall (ts, K_PAUSE, 0, 0);
					}
					ts->stack->must_set_cmp_flags = 0;
					break;
					/*}}}*/
					/*{{{  INDIRECT_AREG*/
				case INDIRECT_AREG:
					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg));
					break;
					/*}}}*/
					/*{{{  INDIRECT_BREG*/
				case INDIRECT_BREG:
					glob_in_icount++;
					ts->stack->old_b_reg = ts->stack->b_reg;
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
					break;
					/*}}}*/
					/*{{{  INDIRECT_CREG*/
				case INDIRECT_CREG:
					glob_in_icount++;
					ts->stack->old_c_reg = ts->stack->c_reg;
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg));
					break;
					/*}}}*/
#if 0
					/*{{{  INDIRECTCHAN*/
				case INDIRECTCHAN:
					glob_in_icount++;
					if (ts->indirectchan) {
						fprintf (stderr, "%s: serious: indirectchan marker already set.\n", progname);
					}
					ts->indirectchan = 1;
					break;
					/*}}}*/
#endif
					/*{{{  RMWSMAP*/
				case RMWSMAP:
					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					arch->compose_kcall (ts, K_RMWSMAP, 1, 0);
					ts->stack->must_set_cmp_flags = 0;
					ts->stack->ts_depth--;
					break;
					/*}}}*/
					/*{{{  MPPCLONE*/
				case MPPCLONE:
					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					arch->compose_kcall (ts, K_MPPCLONE, 1, 1);
					ts->stack->must_set_cmp_flags = 0;
					break;
					/*}}}*/
					/*{{{  MPPSERIALISE, MPPDESERIALISE*/
				case MPPSERIALISE:
				case MPPDESERIALISE:
					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					ts->stack->old_b_reg = ts->stack->b_reg;
					ts->stack->old_c_reg = ts->stack->c_reg;
					arch->compose_kcall (ts, (etc_code->opd == MPPSERIALISE) ? K_MPPSERIALISE : K_MPPDESERIALISE, 3, 0);
					ts->stack->must_set_cmp_flags = 0;
					ts->stack->ts_depth = 0;
					break;
					/*}}}*/
					/*{{{  LOADCODEMAP*/
				case LOADCODEMAP:
					glob_in_icount++;
					etc_code = etc_code->next;
					y_fn = etc_code->fn;
					y_opd = etc_code->opd;
					if (y_fn != I_LDC) {
						fprintf (stderr, "%s: warning: ETC0 (LOADCODEMAP) without following LDC\n", progname);
						break;
					}
					tstack_setprim (ts->stack, I_LDC, arch);
					/* label in y_opd is an entry-point -- lookup for code-map */
					{
						procinf *pitmp = procinf_findbylab (y_opd);

						if (!pitmp) {
							fprintf (stderr, "%s: warning: ETC0 (LOADCODEMAP) label %d does not exist\n", progname, y_opd);
							break;
						} else if (pitmp->maplab < 0) {
							/* no local code map, use external name */
							add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REG, ts->stack->a_reg));
						} else {
							add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, pitmp->maplab, ARG_REG, ts->stack->a_reg));
						}
					}

					break;
					/*}}}*/
					/*{{{  FBARINIT*/
				case FBARINIT:
					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					ts->stack->old_b_reg = ts->stack->b_reg;
					ts->stack->old_c_reg = ts->stack->c_reg;
					if (options.kernel_interface & KRNLIFACE_MP) {
						arch->compose_kcall (ts, K_FBAR_INIT, 1, 0);
					} else {
						/* initialise barrier to (0,0,notprocess,notprocess) */
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 0));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 4));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 8));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 12));
					}

					ts->stack->must_set_cmp_flags = 0;
					ts->stack->ts_depth -= 1;
					break;
					/*}}}*/
					/*{{{  FBARSYNC*/
				case FBARSYNC:
					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					ts->stack->old_b_reg = ts->stack->b_reg;
					ts->stack->old_c_reg = ts->stack->c_reg;
					/* old_a_reg has the barrier-pointer */
					if (options.kernel_interface & KRNLIFACE_MP) {
						arch->compose_deadlock_kcall (ts, K_FBAR_SYNC, 1, 0);
					} else {
						int skiplab = ++(ts->last_lab);
						int outlab = ++(ts->last_lab);
						int fqlab = ++(ts->last_lab);
						int fxlab = ++(ts->last_lab);
						int fslab = ++(ts->last_lab);
						int rslab = ++(ts->last_lab);

						add_to_ins_chain (compose_ins (INS_DEC, 1, 2, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 4, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 4, ARG_REG | ARG_IMP, REG_CC));
						add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_LABEL, skiplab));
						/* barrier complete, reset count attach barrier queue to run-queue */
						tmp_reg = tstack_newreg (ts->stack);
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 0, ARG_REG, tmp_reg));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 4));
						/* fptr == NotProcess.p ? */
						add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 8, ARG_REG | ARG_IMP, REG_CC));
						add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_LABEL, outlab));
						/* no, append it to the run-queue */
						add_to_ins_chain (compose_ins (INS_OR, 2, 1, ARG_REG, REG_FPTR, ARG_REG, REG_FPTR, ARG_REG, REG_FPTR, ARG_REG | ARG_IMP, REG_CC));
						add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_LABEL, fqlab));
						/* run-queue non-empty, append onto BPTR */
						tmp_reg = tstack_newreg (ts->stack);
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 8, ARG_REG, tmp_reg));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_BPTR, W_LINK));
						add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, fslab));

#if 0
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 12, ARG_REG, REG_BPTR));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 8));
						add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, outlab));
#endif
						
						/* run-queue empty, load fptr and bptr into regs */
						add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, fqlab));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 8, ARG_REG, REG_FPTR));
						add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, fslab));

						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 12, ARG_REG, REG_BPTR));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 8));
						add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, outlab));

						add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, skiplab));
						/* barrier not complete, add process to queue and reschedule */
						add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 8, ARG_REG | ARG_IMP, REG_CC));
						add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_LABEL, fxlab));
						/* queue already has something on it, add ourselves */
						tmp_reg = tstack_newreg (ts->stack);
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 12, ARG_REG, tmp_reg));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND | ARG_DISP, tmp_reg, W_LINK));
						add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, rslab));

						/* queue emoty, put ourselves in it */
						add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, fxlab));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 8));

						/* put in Bptr and reschedule */
						add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, rslab));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, 12));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, W_LINK));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, outlab, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));

						if (options.inline_options & INLINE_SCHEDULER) {
							arch->compose_inline_quick_reschedule (ts);
						} else {
							add_to_ins_chain (arch->compose_kjump (ts, INS_JUMP, 0, kif_entry (K_FASTSCHEDULER)));
						}

						add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, outlab));
					}
					ts->stack->must_set_cmp_flags = 0;
					ts->stack->ts_depth -= 1;
					break;
					/*}}}*/
					/*{{{  FBARRESIGN*/
				case FBARRESIGN:
					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					ts->stack->old_b_reg = ts->stack->b_reg;
					ts->stack->old_c_reg = ts->stack->c_reg;
					/* old_b_reg has the barrier pointer, old_a_reg has the number of processes we wish to resign */
					if (options.kernel_interface & KRNLIFACE_MP) {
						arch->compose_kcall (ts, K_FBAR_RESIGN, 2, 0);
					} else {
						int outlab = ++(ts->last_lab);
						int fslab = ++(ts->last_lab);
						int fqlab = ++(ts->last_lab);
						int cslab = ++(ts->last_lab);
						int extends_magic;

						extends_magic = (ts->magic_pending & TS_MAGIC_EXTENDS_RESIGN);
						if (extends_magic) {
							ts->magic_pending &= ~TS_MAGIC_EXTENDS_RESIGN;
						}

						if (constmap_typeof (ts->stack->old_a_reg) == VALUE_CONST) {
							add_to_ins_chain (compose_ins (INS_SUB, 2, 2, ARG_CONST | ARG_ISCONST, constmap_regconst (ts->stack->old_a_reg),
										ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 0, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 0, ARG_REG | ARG_IMP, REG_CC));
						} else {
							add_to_ins_chain (compose_ins (INS_SUB, 2, 2, ARG_REG, ts->stack->old_a_reg, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 0,
										ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 0, ARG_REG | ARG_IMP, REG_CC));
						}
						/* if this goes below zero, badness */
						add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NS, ARG_LABEL, cslab));
						/* generate error */
						if (options.debug_options & DEBUG_RANGESTOP) {
							int x;

							x = (0xfb00 << 16) + (ts->line_pending & 0xffff);
							add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_CONST, x));
							x = ((ts->file_pending & 0xffff) << 16) + (ts->proc_pending & 0xffff);
							add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_CONST, x));
							add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_LABEL | ARG_ISCONST, ts->filename_label));
							add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_LABEL | ARG_ISCONST, ts->procedure_label));
							ts->stack_drift += 4;
							add_to_ins_chain (arch->compose_kjump (ts, INS_CALL, 0, kif_entry (K_SETERR)));
							ts->stack_drift -= 4;
						} else {
							arch->compose_kcall (ts, K_BNSETERR, 0, 0);
						}

						add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, cslab));
						if (constmap_typeof (ts->stack->old_a_reg) == VALUE_CONST) {
							add_to_ins_chain (compose_ins (INS_SUB, 2, 2, ARG_CONST | ARG_ISCONST, constmap_regconst (ts->stack->old_a_reg),
										ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 4, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 4, ARG_REG | ARG_IMP, REG_CC));
						} else {
							add_to_ins_chain (compose_ins (INS_SUB, 2, 2, ARG_REG, ts->stack->old_a_reg, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 4,
										ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 4, ARG_REG | ARG_IMP, REG_CC));
						}

						if (!extends_magic) {
							add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_LABEL, outlab));

							/* barrier complete, reset count and attach barrier queue to run-queue */
							tmp_reg = tstack_newreg (ts->stack);
							add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 0, ARG_REG, tmp_reg));
							add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 4));
							/* fptr == NotProcess.p ? */
							add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 8, ARG_REG | ARG_IMP, REG_CC));
							add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_LABEL, outlab));
							/* no, append it to the run-queue */
							add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REG, REG_FPTR, ARG_REG, REG_FPTR, ARG_REG, REG_FPTR, ARG_REG | ARG_IMP, REG_CC));
							add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_LABEL, fqlab));
							/* run-queue non-empty, append onto BPTR */
							tmp_reg = tstack_newreg (ts->stack);
							add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 8, ARG_REG, tmp_reg));
							add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_BPTR, W_LINK));
							add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, fslab));
							/* run-queue empty, load fptr and bptr into regs */
							add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, fqlab));
							add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 8, ARG_REG, REG_FPTR));
							add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, fslab));

							add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 12, ARG_REG, REG_BPTR));
							add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 8));

							add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, outlab));
						}

					}
					ts->stack->must_set_cmp_flags = 0;
					ts->stack->ts_depth -= 2;
					break;
					/*}}}*/
					/*{{{  FBARENROLL*/
				case FBARENROLL:
					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					ts->stack->old_b_reg = ts->stack->b_reg;
					ts->stack->old_c_reg = ts->stack->c_reg;
					/* old_b_reg has the barrier-pointer, old_a_reg has the count (possibly constant) */
					if (options.kernel_interface & KRNLIFACE_MP) {
						arch->compose_kcall (ts, K_FBAR_ENROLL, 2, 0);
					} else {
						switch (constmap_typeof (ts->stack->old_a_reg)) {
						case VALUE_CONST:
							add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, constmap_regconst (ts->stack->old_a_reg), ARG_REGIND | ARG_DISP,
										ts->stack->old_b_reg, 4, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 4));
							add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, constmap_regconst (ts->stack->old_a_reg), ARG_REGIND | ARG_DISP,
										ts->stack->old_b_reg, 0, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 0));
							break;
						default:
							add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 4,
										ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 4));
							add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 0,
										ARG_REGIND | ARG_DISP, ts->stack->old_b_reg, 0));
							break;
						}
					}
					ts->stack->must_set_cmp_flags = 0;
					ts->stack->ts_depth -= 2;
					break;
					/*}}}*/
					/*{{{  R32SIN, R64SIN, R32COS, R64COS, R32TAN, R64TAN*/
				case R32SIN:
				case R64SIN:
				case R32COS:
				case R64COS:
				case R32TAN:
				case R64TAN:
					ts->stack->old_a_reg = ts->stack->a_reg;
					ts->stack->old_b_reg = ts->stack->b_reg;
					ts->stack->old_c_reg = ts->stack->c_reg;

					switch (etc_code->opd) {
					case R32SIN:
						arch->compose_fpop (ts, I_R32SIN);
						break;
					case R64SIN:
						arch->compose_fpop (ts, I_R64SIN);
						break;
					case R32COS:
						arch->compose_fpop (ts, I_R32COS);
						break;
					case R64COS:
						arch->compose_fpop (ts, I_R64COS);
						break;
					case R32TAN:
						arch->compose_fpop (ts, I_R32TAN);
						break;
					case R64TAN:
						arch->compose_fpop (ts, I_R64TAN);
						break;
					}
					break;
					/*}}}*/
					/*{{{  DTRACE*/
				case DTRACE:
					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					ts->stack->old_b_reg = ts->stack->b_reg;
					ts->stack->old_c_reg = ts->stack->c_reg;

					deferred_cond (ts);

					arch->compose_kcall (ts, K_DTRACE, 2, 0);
					
					break;
					/*}}}*/
					/*{{{  KILLCALL*/
				case KILLCALL:
					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					
					arch->compose_kcall (ts, K_BX_KILL, 1, 1);
					
					ts->stack->ts_depth = 1;
					break;
					/*}}}*/
					/*{{{  WAIT_FOR_INTERRUPT*/
				case WAIT_FOR_INTERRUPT:
					#ifdef K_WAIT_INT
					if (!(options.kernel_interface & KRNLIFACE_RMOX)) {
						fprintf (stderr, "%s: fatal: must have RMOX kernel-interface support in order to use WAIT.FOR.INTERRUPT()\n", progname);
						exit (EXIT_FAILURE);
					}

					glob_in_icount++;
					ts->stack->old_a_reg = ts->stack->a_reg;
					ts->stack->old_b_reg = ts->stack->b_reg;

					arch->compose_kcall (ts, K_WAIT_INT, 2, 0);
					
					ts->stack->a_reg = tstack_newreg (ts->stack);
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, REG_WPTR, ARG_REG, ts->stack->a_reg));
					ts->stack->ts_depth = 1;

					#else
					fprintf (stderr, "%s: fatal: must have RMOX compiled into tranx86, update KRoC/CCSP and rebuild\n", progname);
					exit (EXIT_FAILURE);
					#endif
					break;
					/*}}}*/
					/*{{{  default (special message)*/
				default:
					tmp_string = string_dup ("special message             ");
					sprintf (tmp_string + 17, "%d", etc_code->opd);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, tmp_string));
					break;
					/*}}}*/
				}
				break;
				/*}}}*/
				/*{{{  ETC6 ETC7  (set label) */
			case ETC6:
			case ETC7:
				etc_code = etc_code->next;
				if (options.annotate_output) {
					sprintf (sbuffer, ".SETLABEL L%d [tsd=%d,%d]", etc_code->opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				deferred_cond (ts);
				if (etc_code->fn != I_LDC) {
					fprintf (stderr, "%s: warning: ETC6/7 with no LDC.\n", progname);
					break;
				}
				if (ts->incasetable) {
					/* label always follows STARTTABLE jumps */
					/* better re-align code if the architecture needs it */
					if (options.internal_options & INTERNAL_ALIGNEDCODE) {
						flush_ins_chain ();
						trtl = new_rtl ();
						trtl->type = RTL_ALIGN;
						trtl->u.alignment = 2;
						add_to_rtl_chain (trtl);
					}
					ts->incasetable = 0;
				}
				add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, etc_code->opd));
				/* might be a relevant label for code-mapping */
				if (ts->cpinfo && (ts->magic_pending & TS_MAGIC_JOINLAB)) {
					procinf *refd;

					refd = procinf_internallab (ts->cpinfo, etc_code->opd);
					procinf_addref (ts->cpinfo, refd);
					ts->magic_pending &= ~(TS_MAGIC_JOINLAB | TS_MAGIC_CODEMAP);		/* clear CODEMAP bit too if set */
				} else if (ts->magic_pending & TS_MAGIC_CODEMAP) {
					if (ts->cpinfo) {
						procinf *refd;

						refd = procinf_internallab (ts->cpinfo, etc_code->opd);
						procinf_addref (ts->cpinfo, refd);
					}
					ts->magic_pending &= ~TS_MAGIC_CODEMAP;
				}
				break;
				/*}}}*/
				/*{{{  ETC1-ETC5 ETC8-ETC14  */
			case ETC1:
			case ETC2:
			case ETC3:
			case ETC4:
			case ETC5:
			case ETC8:
			case ETC9:
			case ETC10:
			case ETC11:
			case ETC12:
			case ETC13:
			case ETC14:
				x_opd = x_opd - ETC0;
				etc_code = etc_code->next;
				y_fn = etc_code->fn;
				y_opd = etc_code->opd;
				if (y_fn != I_LDC) {
					fprintf (stderr, "%s: warning: ETC[1-5,8-14] with no LDC.\n", progname);
					break;
				}
				switch (x_opd) {
					/*{{{  LINENUM*/
				case LINENUM:
					if (options.annotate_output) {
						sprintf (sbuffer, ".LINE %d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					set_line_pending (ts, y_opd);
					add_to_ins_chain (compose_ins (INS_SOURCELINE, 1, 0, ARG_CONST, y_opd));
					if (options.debug_options & DEBUG_INSERT) {
						arch->compose_debug_insert (ts, 0);
					}
					break;
					/*}}}*/
					/*{{{  DEBUGLINE*/
				case DEBUGLINE:
					if (options.annotate_output) {
						sprintf (sbuffer, ".DEBUGLINE [tsd=%d,%d]", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					CONVMISSING ("DEBUGLINE");
					/* **INCOMPLETE** */
					break;
					/*}}}*/
					/*{{{  TSDEPTH*/
				case TSDEPTH:
					if (options.annotate_output) {
						sprintf (sbuffer, ".TSDEPTH %d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					if (y_opd >= 4) {
						while (y_opd > 3) {
							add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("*** register lost from stack ***")));
							fprintf (stderr, "%s: warning: register lost from stack.\n", progname);
							y_opd--;
						}
					} else if (y_opd >= 1) {
						if (ts->stack->old_ts_depth != (y_opd + 1)) {
							fprintf (stderr, "%s: warning: TSDEPTH experiencing stack problem (old depth was %d, expected it to be %d)..\n", progname,
								ts->stack->old_ts_depth, (y_opd + 1));
						}
						deferred_cond (ts);
						if (y_opd == 1) {
							ts->stack->b_reg = REG_UNDEFINED;
							ts->stack->c_reg = REG_UNDEFINED;
						} else if (y_opd == 2) {
							ts->stack->c_reg = REG_UNDEFINED;
						} else {
							fprintf (stderr, "%s: warning: TSDEPTH experiencing stack problem..\n", progname);
						}
					} else {
						ts->stack->a_reg = REG_UNDEFINED;
					}
					ts->stack->ts_depth = y_opd;
					break;
					/*}}}*/
					/*{{{  FUNCRESULTS*/
				case FUNCRESULTS:
					glob_in_icount++;
					if (options.annotate_output) {
						sprintf (sbuffer, ".FUNCRESULTS %d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					arch->compose_funcresults (ts, y_opd);

					ts->stack->must_set_cmp_flags = 1;
					ts->stack->ts_depth = y_opd;
					break;
					/*}}}*/
					/*{{{  FUNCRETURN*/
				case FUNCRETURN:
					/* get this shortly before the real RET */
					glob_in_icount++;
					if (options.annotate_output) {
						sprintf (sbuffer, ".FUNCRETURN %d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					ts->numfuncresults = y_opd;
					ts->stack->fs_depth = 0;		/* suppress possible reset.fregs */
					break;
					/*}}}*/
					/*{{{  REALRESULT*/
				case REALRESULT:
					glob_in_icount++;
					if (options.annotate_output) {
						sprintf (sbuffer, ".REALRESULT %d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					/* suppresion of reset_fregs means fix depth */
					// ts->stack->fs_depth = y_opd;
					ts->stack->fs_depth = 1;
					break;
					/*}}}*/
					/*{{{  SRLIMM*/
				case SRLIMM:
					glob_in_icount++;
					if (options.annotate_output) {
						sprintf (sbuffer, ".SRLIMM %d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					deferred_cond (ts);
					ts->stack->old_a_reg = ts->stack->a_reg;
					add_to_ins_chain (compose_ins (INS_SHR, 2, 2, ARG_CONST, y_opd, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
					constmap_remove (ts->stack->old_a_reg);
					break;
					/*}}}*/
					/*{{{  SLLIMM*/
				case SLLIMM:
					glob_in_icount++;
					if (options.annotate_output) {
						sprintf (sbuffer, ".SLLIMM %d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					deferred_cond (ts);
					ts->stack->old_a_reg = ts->stack->a_reg;
					add_to_ins_chain (compose_ins (INS_SHL, 2, 2, ARG_CONST, y_opd, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
					if (constmap_typeof (ts->stack->old_a_reg) == VALUE_CONST) {
						constmap_modregconst (ts->stack->old_a_reg, constmap_regconst(ts->stack->old_a_reg) << y_opd);
					} else {
						constmap_remove (ts->stack->old_a_reg);
					}
					break;
					/*}}}*/
					/*{{{  SETWS*/
				case SETWS:
					if (options.annotate_output) {
						sprintf (sbuffer, ".SETWS %d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					ts->ws_size = y_opd;
#if 0
fprintf (stderr, "setting ts->ws_size = %d\n", y_opd);
#endif
					break;
					/*}}}*/
					/*{{{  SETVS*/
				case SETVS:
					if (options.annotate_output) {
						sprintf (sbuffer, ".SETVS %d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					ts->vs_size = y_opd;
					break;
					/*}}}*/
					/*{{{  ALIGN*/
				case ALIGN:
					if (options.annotate_output) {
						sprintf (sbuffer, ".ALIGN %d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					flush_ins_chain ();
					trtl = new_rtl ();
					trtl->type = RTL_ALIGN;
					trtl->u.alignment = y_opd;
					add_to_rtl_chain (trtl);
					break;
					/*}}}*/
					/*{{{  default*/
				default:
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("<specialnames>")));
					break;
					/*}}}*/
				}
				break;
				/*}}}*/
				/*{{{  ETCS1  (stubname)*/
			case ETCS1:		/* stubname */
				glob_in_icount++;
				if (options.annotate_output) {
					sprintf (sbuffer, ".STUBNAME %*s [tsd=%d,%d]", etc_code->o_len, etc_code->o_bytes, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				generate_call (ts, etc_code, arch, 1);
				break;
				/*}}}*/
				/*{{{  ETCS2  (global name)*/
			case ETCS2:		/* globname */
				{
					int glab = -1;
					procinf *pitmp;

					if (options.annotate_output) {
						sprintf (sbuffer, ".GLOBNAME %*s [tsd=%d,%d]", etc_code->o_len, etc_code->o_bytes, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					/* scan backwards for associated label and save */
					for (tmp_ins = ins_tail; tmp_ins && (tmp_ins->type != INS_SETLABEL); tmp_ins = tmp_ins->prev);
					if (tmp_ins) {
						glab = ArgLabel (tmp_ins->in_args[0]);
					}

					flush_ins_chain ();
					if (options.is_library && ts->libout_pending) {
						/* dump WS+6 and VS to output */
						sprintf (sbuffer, "%s_%s_wsbytes", (options.extref_prefix ? options.extref_prefix : ""), ts->libout_name);
						trtl = new_rtl ();
						trtl->type = RTL_PUBLICSETNAMEDLABEL;
						trtl->u.label_name = string_dup (sbuffer);
						add_rtl_to_spec_chain (trtl, &def_lib_head, &def_lib_tail);
						i = (ts->ws_size + 6) * 4;
						declare_data_bytes_spec_chain (mem_ndup ((char *)&i, sizeof (int)), sizeof (int), &def_lib_head, &def_lib_tail);

						/* frmb: added ws_adjust for NOCC (also correct for default) */
						sprintf (sbuffer, "%s_%s_wsadjust", (options.extref_prefix ? options.extref_prefix : ""), ts->libout_name);
						trtl = new_rtl ();
						trtl->type = RTL_PUBLICSETNAMEDLABEL;
						trtl->u.label_name = string_dup (sbuffer);
						add_rtl_to_spec_chain (trtl, &def_lib_head, &def_lib_tail);
						i = ts->ws_adjust * 4;
						declare_data_bytes_spec_chain (mem_ndup ((char *)&i, sizeof (int)), sizeof (int), &def_lib_head, &def_lib_tail);

						sprintf (sbuffer, "%s_%s_vsbytes", (options.extref_prefix ? options.extref_prefix : ""), ts->libout_name);
						trtl = new_rtl ();
						trtl->type = RTL_PUBLICSETNAMEDLABEL;
						trtl->u.label_name = string_dup (sbuffer);
						add_rtl_to_spec_chain (trtl, &def_lib_head, &def_lib_tail);
						i = ts->vs_size * 4;
						declare_data_bytes_spec_chain (mem_ndup ((char *)&i, sizeof (int)), sizeof (int), &def_lib_head, &def_lib_tail);

						sprintf (sbuffer, "%s_%s_msbytes", (options.extref_prefix ? options.extref_prefix : ""), ts->libout_name);
						trtl = new_rtl ();
						trtl->type = RTL_PUBLICSETNAMEDLABEL;
						trtl->u.label_name = string_dup (sbuffer);
						add_rtl_to_spec_chain (trtl, &def_lib_head, &def_lib_tail);
						i = ts->ms_size * 4;
						declare_data_bytes_spec_chain (mem_ndup ((char *)&i, sizeof (int)), sizeof (int), &def_lib_head, &def_lib_tail);
						ts->libout_pending = 0;
						sfree (ts->libout_name);
					}
					if (options.is_library) {
						/* set pending and name for next time */
						ts->libout_pending = 1;
						ts->libout_name = string_ndup (etc_code->o_bytes, etc_code->o_len);
					}
					tstate_add_glob_name (ts, etc_code->o_bytes, etc_code->o_len);
#if 1
					trtl = new_rtl ();
					trtl->type = RTL_PUBLICSETNAMEDLABEL;
					trtl->u.label_name = (char *)smalloc (etc_code->o_len + 4);
					make_c_name (etc_code->o_bytes, etc_code->o_len, trtl->u.label_name);
					add_to_rtl_chain (trtl);
#endif
					/* declare this name and entry-point */
					pitmp = procinf_declare (etc_code->o_bytes, etc_code->o_len);
					pitmp->eplab = glab;

					ts->supress_debug_insert = 1;
				}
				break;
				/*}}}*/
				/*{{{  ETCS3  (j-entry)*/
			case ETCS3:		/* jentry */
				if (options.annotate_output) {
					sprintf (sbuffer, ".JENTRY %*s [tsd=%d,%d]", etc_code->o_len, etc_code->o_bytes, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				if (!ts->had_jentry && !options.not_main_module && !options.suppress_fpu) {
					flush_ins_chain ();
					arch->compose_fp_init (ts);
				}
				if (ts->had_jentry || options.not_main_module) {
					char *tmp = (char *)smalloc (etc_code->o_len + 4);

					make_c_name (etc_code->o_bytes, etc_code->o_len, tmp);
					generate_call (ts, etc_code, arch, 0);
					#if 0
					fprintf (stderr, "X1\n");
					add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_NAMEDLABEL, tmp));
					#endif
				} else {
					char *tmp;

					if (ts->jentry_name) {
						sfree (ts->jentry_name);
						ts->jentry_name = NULL;
					}
					tmp = (char *)smalloc (etc_code->o_len + 4);
					ts->jentry_name = (char *)smalloc (etc_code->o_len + 1);
					sprintf (ts->jentry_name, "%*s", etc_code->o_len, etc_code->o_bytes);

					make_c_name (etc_code->o_bytes, etc_code->o_len, tmp);
					ts->flushscreenpoint = ++(ts->last_lab);
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->flushscreenpoint, ARG_REGIND, REG_WPTR));
					add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_NAMEDLABEL, tmp));
				}
				ts->had_jentry = 1;
				break;
				/*}}}*/
				/*{{{  ETCS4  (proc-entry) */
			case ETCS4:		/* proc entry */
				if (options.annotate_output) {
					sprintf (sbuffer, ".PROCENTRY %*s [tsd=%d,%d]", etc_code->o_len, etc_code->o_bytes, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				flush_ins_chain ();
				set_proc_pending (ts, etc_code->o_bytes, etc_code->o_len);
				if (!tstate_is_glob_name (ts, etc_code->o_bytes, etc_code->o_len)) {
					trtl = new_rtl ();
					trtl->type = RTL_SETNAMEDLABEL;
					trtl->u.label_name = (char *)smalloc (etc_code->o_len + 4);
					make_c_name (etc_code->o_bytes, etc_code->o_len, trtl->u.label_name);
					add_to_rtl_chain (trtl);
				}
				/* not needed anymore -- caller puts return address in for us */
					/* pop return address into Wptr[0] */
					/* add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REGIND, REG_WPTR)); */
				/* reset FPU */
				arch->compose_reset_fregs (ts);
				ts->supress_debug_insert = 0;
				
				/* mark as processing this PROC */
				{
					procinf *pitmp = procinf_findbyname (etc_code->o_bytes, etc_code->o_len);

					if (pitmp) {
						pitmp->is_proc = 1;
						pitmp->is_local = 1;
					}
#if 0
fprintf (stderr, "ETCS4: PROCENTRY %*s, setting ts->cpinfo = %p\n", etc_code->o_len, etc_code->o_bytes, pitmp);
#endif
					ts->cpinfo = pitmp;
				}

				/* if debugging traces are enabled, tell the run-time about us -- vs/ws/ms should be right */
				if (options.debug_options & DEBUG_DTRACES) {
					int namelab = ++(ts->last_lab);

					add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, 0));
					add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, namelab));
					declare_data_bytes (string_ndup (etc_code->o_bytes, etc_code->o_len), etc_code->o_len + 1);		/* make sure there's a null-terminator */
					add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));

					ts->stack->old_a_reg = tstack_newreg (ts->stack);
					ts->stack->old_b_reg = tstack_newreg (ts->stack);
					ts->stack->old_ts_depth = 2;

					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 1, ARG_REG, ts->stack->old_a_reg));
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, namelab, ARG_REG, ts->stack->old_b_reg));
					arch->compose_kcall (ts, K_DTRACE, 2, 0);
				}
				break;
				/*}}}*/
				/*{{{  ETCS5  (source filename)*/
			case ETCS5:		/* source file-name */
				if (options.annotate_output) {
					sprintf (sbuffer, ".SOURCEFILE %*s [tsd=%d,%d]", etc_code->o_len, etc_code->o_bytes, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				flush_ins_chain ();
				set_file_pending (ts, etc_code->o_bytes, etc_code->o_len);
				trtl = new_rtl ();
				trtl->type = RTL_SOURCEFILE;
				trtl->u.sourcefile = ts->file_list[ts->file_pending];
				add_to_rtl_chain (trtl);
				break;
				/*}}}*/
				/*{{{  ETCS6  (compiler comment) */
			case ETCS6:		/* occ21 comment */
				if (options.annotate_output) {
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_ndup (etc_code->o_bytes, etc_code->o_len)));
				}
				if (!strncmp (etc_code->o_bytes, ".MAGIC ", (etc_code->o_len < 7) ? etc_code->o_len : 7)) {
					/* some magic */
					char *arg = etc_code->o_bytes + 7;
					int arglen = etc_code->o_len - 7;

					if (!strncmp (arg, "IOSPACE", (arglen < 7) ? arglen : 7)) {
						ts->magic_pending |= TS_MAGIC_IOSPACE;
					} else if (!strncmp (arg, "UNUSED LOOPVAR", (arglen < 14) ? arglen : 14)) {
						ts->magic_pending |= TS_MAGIC_UNUSED_LOOPVAR;
					} else if (!strncmp (arg, "PREENABLE", (arglen < 9) ? arglen : 9)) {
						ts->magic_pending |= TS_MAGIC_PREENABLE;
					} else if (!strncmp (arg, "UNCHECKED", (arglen < 9) ? arglen : 9)) {
						ts->magic_pending |= TS_MAGIC_UNCHECKED;
					} else if (!strncmp (arg, "EXTENDS RESIGN", (arglen < 14) ? arglen : 14)) {
						ts->magic_pending |= TS_MAGIC_EXTENDS_RESIGN;
					} else if (!strncmp (arg, "JOINLAB", (arglen < 7) ? arglen : 7)) {
						ts->magic_pending |= TS_MAGIC_JOINLAB;
					} else if (!strncmp (arg, "DOSUBCODEMAP", (arglen < 12) ? arglen : 12)) {
						ts->magic_pending |= TS_MAGIC_DOSUBCODEMAP;
					} else if (!strncmp (arg, "CODEMAP", (arglen < 7) ? arglen : 7)) {
						ts->magic_pending |= TS_MAGIC_CODEMAP;
					} else if (!strncmp (arg, "TYPEDESC", (arglen < 8) ? arglen : 8)) {
						ts->magic_pending |= TS_MAGIC_TYPEDESC;
						tstate_clear_fixups (ts);
					} else if (!strncmp (arg, "FIXUP", (arglen < 5) ? arglen : 5)) {
						/*{{{  state fixup*/
						int xlab, xoffs, xolab;

						if (sscanf (arg, "FIXUP %d %d %d", &xlab, &xoffs, &xolab) == 3) {
							/* add it */
							tstate_addfixup (ts, xlab, xoffs, xolab);
						} else {
							fprintf (stderr, "%s: error: broken fixup data: %*s\n", progname, etc_code->o_len - 7, etc_code->o_bytes + 7);
						}
						/*}}}*/
					} else if (!strncmp (arg, "DYNCALL", (arglen < 7) ? arglen : 7)) {
						/*{{{  indicator to generate dynamic call stub data*/
						/* expecting name and 3 numeric arguments, last hexadecimal */
						if (arglen < 22) {
							fprintf (stderr, "%s: error: broken DYNCALL data: %*s\n", progname,
									etc_code->o_len - 7, etc_code->o_bytes + 7);
						} else {
							unsigned int ws, vs, thash;
							char *ch, *pname;
							int left, pnlen;

							pname = arg + 8;
							left = arglen - 8;
							for (ch=pname; (left > 0) && (*ch != ' '); ch++, left--);
							pnlen = (int)(ch - pname);
							for (; (left > 0) && (*ch == ' '); ch++, left--);

							if (sscanf (ch, "%d %d %x", &ws, &vs, &thash) != 3) {
								fprintf (stderr, "%s: error: broken DYNCALL data: %*s\n",
										progname, etc_code->o_len - 7, etc_code->o_bytes + 7);
							} else {
								flush_ins_chain ();
								trtl = new_rtl ();
								trtl->type = RTL_DYNCODEENTRY;
								trtl->u.dyncode.fcn_name = string_ndup (pname, pnlen);

								ch = (char *)smalloc (pnlen + 6);
								snprintf (ch, pnlen + 6, "DCR_%s", trtl->u.dyncode.fcn_name);
								trtl->u.dyncode.label_name = ch;
#if 0
fprintf (stderr, "DYNCALL: label_name = [%s], fcn_name = [%s]\n", trtl->u.dyncode.label_name, trtl->u.dyncode.fcn_name);
#endif
								trtl->u.dyncode.ws_slots = ws;
								trtl->u.dyncode.vs_slots = vs;
								trtl->u.dyncode.typehash = thash;
								add_to_rtl_chain (trtl);
							}
						}
						/*}}}*/
					} else if (!strncmp (arg, "MAINDYNCALL", (arglen < 11) ? arglen : 11)) {
						/*{{{  indicator to generate dynamic call stub data for "main" routine*/
						/* expecting name and 3 numeric arguments, last hexadecimal */
						if (arglen < 26) {
							fprintf (stderr, "%s: error: broken MAINDYNCALL data: %*s\n", progname,
									etc_code->o_len - 7, etc_code->o_bytes + 7);
						} else {
							unsigned int ws, vs, thash;
							char *ch, *pname;
							int left, pnlen;

							pname = arg + 12;
							left = arglen - 12;
							for (ch=pname; (left > 0) && (*ch != ' '); ch++, left--);
							pnlen = (int)(ch - pname);
							for (; (left > 0) && (*ch == ' '); ch++, left--);

							if (sscanf (ch, "%d %d %x", &ws, &vs, &thash) != 3) {
								fprintf (stderr, "%s: error: broken MAINDYNCALL data: %*s\n",
										progname, etc_code->o_len - 7, etc_code->o_bytes + 7);
							} else {
								flush_ins_chain ();
								trtl = new_rtl ();
								trtl->type = RTL_DYNCODEENTRY;
								trtl->u.dyncode.fcn_name = string_ndup (pname, pnlen);
								trtl->u.dyncode.rmoxmode = options.rmoxmode;
								switch (options.rmoxmode) {
								case RM_NONE:
									trtl->u.dyncode.label_name = string_dup ("DCR_occam_start");
									break;
								case RM_APP:
									trtl->u.dyncode.label_name = string_dup ("DCR_rmox_app_main");
									break;
								case RM_DRV:
									trtl->u.dyncode.label_name = string_dup ("DCR_rmox_drv_main");
									break;
								case RM_SRV:
									trtl->u.dyncode.label_name = string_dup ("DCR_rmox_srv_main");
									break;
								case RM_FS:
									trtl->u.dyncode.label_name = string_dup ("DCR_rmox_fs_main");
									break;
								case RM_NET:
									trtl->u.dyncode.label_name = string_dup ("DCR_rmox_net_main");
									break;
								}
#if 0
fprintf (stderr, "MAINDYNCALL: label_name = [%s], fcn_name = [%s]\n", trtl->u.dyncode.label_name, trtl->u.dyncode.fcn_name);
#endif
								trtl->u.dyncode.ws_slots = ws;
								trtl->u.dyncode.vs_slots = vs;
								trtl->u.dyncode.typehash = thash;
								add_to_rtl_chain (trtl);

								/* set jentry_name, as this effectively replaces that */
								if (ts->jentry_name) {
									sfree (ts->jentry_name);
									ts->jentry_name = NULL;
								}
								ts->jentry_name = string_ndup (pname, pnlen);
							}
						}
						/*}}}*/
					} else if (!strncmp (arg, "EXPORT", (arglen < 6) ? arglen : 6)) {
						/*{{{  exported procedure information, contains signature*/
						if (options.etab_filename && !options.etabfile) {
							/* open file */
							options.etabfile = fopen (options.etab_filename, "w");
							if (!options.etabfile) {
								fprintf (stderr, "%s: error: failed to open %s for writing: %s\n",
										progname, options.etab_filename, strerror (errno));
							}
						}

						if (options.etabfile) {
							fprintf (options.etabfile, "%s\n", arg + 7);
						}
						/*}}}*/
					} else {
						/* unknown magic comment */
						fprintf (stderr, "%s: warning: unknown magic: %*s\n", progname, etc_code->o_len - 7, etc_code->o_bytes + 7);
					}
				}
				break;
				/*}}}*/
				/*{{{  ETCS7  (code map) */
			case ETCS7:
				if (options.annotate_output) {
					sprintf (sbuffer, ".CODEMAP %*s", etc_code->o_len, etc_code->o_bytes);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				flush_ins_chain ();
				trtl = new_rtl ();
				trtl->type = RTL_PUBLICSETNAMEDLABEL;
				trtl->u.label_name = (char *)smalloc (etc_code->o_len + 4);
				make_c_name (etc_code->o_bytes, etc_code->o_len, trtl->u.label_name);
				*(trtl->u.label_name) = '^';
				add_to_rtl_chain (trtl);

				/* allocate a label and mark PROC as code-mapped */
				{
					procinf *pitmp = procinf_findbyname (etc_code->o_bytes, etc_code->o_len);

					if (pitmp) {
						pitmp->maplab = ++(ts->last_lab);
					}
				}
				break;
				/*}}}*/
				/*{{{  ETCS8  (data-bytes) */
			case ETCS8:		/* data-bytes */
				if (options.annotate_output) {
					sprintf (sbuffer, ".DATABYTES [tsd=%d,%d]", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				if (ts->magic_pending & TS_MAGIC_TYPEDESC) {
					/* might have some fixup data, so use it */
					declare_data_bytes_fixup (mem_ndup (etc_code->o_bytes, etc_code->o_len), etc_code->o_len, tstate_getfixups (ts));
					ts->magic_pending &= ~TS_MAGIC_TYPEDESC;
				} else {
					/* ordinary data */
					declare_data_bytes (mem_ndup (etc_code->o_bytes, etc_code->o_len), etc_code->o_len);
				}
				break;
				/*}}}*/
				/*{{{  ETCS9  (message-bytes) */
			case ETCS9:		/* message-bytes */
				if (options.annotate_output) {
					sprintf (sbuffer, ".MESSAGEBYTES [tsd=%d,%d]", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				flush_ins_chain ();
				trtl = new_rtl ();
				trtl->type = RTL_MESSAGE;
				trtl->u.data.bytes = string_ndup (etc_code->o_bytes, etc_code->o_len);
				trtl->u.data.length = etc_code->o_len;
				add_to_rtl_chain (trtl);
				break;
				/*}}}*/
				/*{{{  ETCS10  (load named label)*/
			case ETCS10:
				if (options.annotate_output) {
					sprintf (sbuffer, ".LOADLABELNAME [tsd=%d,%d] \"%*s\"", ts->stack->old_ts_depth, ts->stack->old_fs_depth, etc_code->o_len, etc_code->o_bytes);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				glob_in_icount++;
				deferred_cond (ts);
				tstack_setprim (ts->stack, I_LDC, arch);
				add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL | ARG_ISCONST, string_ndup (etc_code->o_bytes, etc_code->o_len), ARG_REG, ts->stack->a_reg));
				break;
				/*}}}*/
				/*{{{  ETCS11  (load named code-map)*/
			case ETCS11:
				if (options.annotate_output) {
					sprintf (sbuffer, ".LOADCODEMAPNAME [tsd=%d,%d] \"%*s\"", ts->stack->old_ts_depth, ts->stack->old_fs_depth, etc_code->o_len, etc_code->o_bytes);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				glob_in_icount++;
				deferred_cond (ts);
				tstack_setprim (ts->stack, I_LDC, arch);
				sprintf (sbuffer, "*%*s", etc_code->o_len, etc_code->o_bytes);
				add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL | ARG_ISCONST, string_dup (sbuffer), ARG_REG, ts->stack->a_reg));
				break;
				/*}}}*/
				/*{{{  ETCS12  (global-name end)*/
			case ETCS12:
				if (options.annotate_output) {
					sprintf (sbuffer, ".GLOBNAMEEND %*s [tsd=%d,%d]", etc_code->o_len, etc_code->o_bytes, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}

				flush_ins_chain ();
				trtl = new_rtl ();
				trtl->type = RTL_PUBLICENDNAMEDLABEL;
				trtl->u.label_name = (char *)smalloc (etc_code->o_len + 4);
				make_c_name (etc_code->o_bytes, etc_code->o_len, trtl->u.label_name);
				add_to_rtl_chain (trtl);
				break;
				/*}}}*/
				/*{{{  ETCL0  (J, CJ, CALL, LDC  for labels)*/
			case ETCL0:		/* I_J, I_CJ, I_CALL, I_LDC (&labels) */
				glob_in_icount++;
				ts->stack->old_a_reg = ts->stack->a_reg;
				ts->stack->old_b_reg = ts->stack->b_reg;
				ts->stack->old_c_reg = ts->stack->c_reg;
				etc_code = etc_code->next;
				y_fn = etc_code->fn;
				y_opd = etc_code->opd;
				switch (y_fn) {
					/*{{{  J*/
				case I_J:
					if (options.annotate_output && !ts->incasetable) {
						sprintf (sbuffer, "J L%d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					if (ts->incasetable) {
						deferred_cond (ts);
						tstack_setprim (ts->stack, I_J, arch);
						add_to_ins_chain (compose_ins (INS_CONSTLABDIFF, 2, 0, ARG_LABEL, y_opd, ARG_LABEL, ts->casetable_label));
					} else {
						deferred_cond (ts);
						tstack_setprim (ts->stack, I_J, arch);
						if (!ts->stack->ts_depth && options.pause_at_loopend) {
							/* FIX: if we're using the new CCSP in multiprocessor-mode, better do this as PAUSE then JUMP */
							if ((options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_MP)) == (KRNLIFACE_NEWCCSP | KRNLIFACE_MP)) {
								arch->compose_kcall (ts, K_PAUSE, 0, 0);
								add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, y_opd));
							} else {
								add_to_ins_chain (compose_ins (INS_PJUMP, 1, 0, ARG_LABEL, y_opd));
							}
						} else {
							add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, y_opd));
						}
					}
					break;
					/*}}}*/
					/*{{{  CJ*/
				case I_CJ:
					if (options.annotate_output) {
						sprintf (sbuffer, "CJ L%d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					tstack_setprim (ts->stack, I_CJ, arch);
					compose_cond_jump (ts, ts->cond, y_opd);
					ts->cond = CC_NONE;
					break;
					/*}}}*/
					/*{{{  CALL*/
				case I_CALL:
					if (options.debug_options & DEBUG_INSERT) {
						arch->compose_debug_insert (ts, 1);
					}
					if (options.annotate_output) {
						sprintf (sbuffer, "CALL L%d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					deferred_cond (ts);
					for (i=2; i>=0; i--) {
						if (ts->stack->old_ts_depth > i) {
							tmp_reg = ((i == 0) ? ts->stack->old_a_reg : ((i == 1) ? ts->stack->old_b_reg : ts->stack->old_c_reg));
							switch (constmap_typeof (tmp_reg)) {
							default:
								add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_WPTR, (i - 3) << WSH));
								break;
							case VALUE_CONST:
								add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, constmap_regconst (tmp_reg),
									ARG_REGIND | ARG_DISP, REG_WPTR, (i - 3) << WSH));
								break;
							}
						}
					}
					add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, -4 << WSH, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR));
					tstack_setprim (ts->stack, I_CALL, arch);
					if ((ts->magic_pending & TS_MAGIC_DOSUBCODEMAP) && ts->cpinfo) {
						/* add to current if here */
						procinf *refd = procinf_findbylab (y_opd);

						if (refd) {
							procinf_addref (ts->cpinfo, refd);
						}
					}
					if (label_is_stub (y_opd)) {
						/* inline stub code */
						add_chain_to_ins_chain (rtl_copy_code (get_stubcode (ts, y_opd)));
					} else {
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_ISCONST | ARG_FLABEL, 0, ARG_REGIND, REG_WPTR));
						add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, y_opd));
						add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
						/* add_to_ins_chain (compose_ins (INS_CALL, 1, 0, ARG_LABEL, y_opd)); */
					}
					constmap_clearall ();
					if (ts->magic_pending & TS_MAGIC_DOSUBCODEMAP) {
						ts->magic_pending &= ~TS_MAGIC_DOSUBCODEMAP;
						/* chances are we planted a label locally, scan backwards for it, add to code-map if needed */
						addlastlabtocodemap (ts);
					}
					break;
					/*}}}*/
					/*{{{  LDC*/
				case I_LDC:
					etc_code = etc_code->next;
					if (etc_code->fn != I_LDC) {
						fprintf (stderr, "%s: warning: ETCL0/I_LDC but no I_LDC.\n", progname);
						break;
					}
					if (options.annotate_output) {
						if (etc_code->opd < 0) {
							sprintf (sbuffer, ".LOADLABADDR L%d [tsd=%d,%d]", y_opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
							if (ts->cpinfo) {
								/* add to current if here */
								procinf *refd = procinf_findbylab (y_opd);

								if (refd) {
									procinf_addref (ts->cpinfo, refd);
								}
							}
						} else {
							sprintf (sbuffer, ".LOADLABDIFF L%d L%d [tsd=%d,%d]", y_opd, etc_code->opd, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						}
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					deferred_cond (ts);
					tstack_setprim (ts->stack, I_LDC, arch);
					if (etc_code->opd < 0) {
						tmp_ins = compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, y_opd, ARG_REG, ts->stack->a_reg);
						constmap_new (ts->stack->a_reg, VALUE_LABADDR, y_opd, tmp_ins);
						add_to_ins_chain (tmp_ins);
					} else {
						int labs[2];

						tmp_ins = compose_ins (INS_LOADLABDIFF, 2, 1, ARG_LABEL, y_opd, ARG_LABEL, etc_code->opd, ARG_REG, ts->stack->a_reg);
						labs[0] = y_opd;
						labs[1] = etc_code->opd;
						constmap_new (ts->stack->a_reg, VALUE_LABDIFF, (int)labs, tmp_ins);
						add_to_ins_chain (tmp_ins);
					}
					break;
					/*}}}*/
				}
				break;
				/*}}}*/
				/*{{{  ETCL1  (loop-end) */
			case ETCL1:		/* new loop-end */
				{
					int unused = 0;

					if (ts->magic_pending & TS_MAGIC_UNUSED_LOOPVAR) {
						ts->magic_pending &= ~TS_MAGIC_UNUSED_LOOPVAR;
						unused = 1;
					}
					glob_in_icount++;
					etc_code = etc_code->next;
					le_wsoff = etc_code->opd;
					etc_code = etc_code->next;
					le_l1 = etc_code->opd;		/* loop end */
					etc_code = etc_code->next;
					le_l2 = etc_code->opd;		/* loop start */
					if (options.annotate_output) {
						sprintf (sbuffer, ".LOOPEND %d, L%d, L%d %s[tsd=%d,%d]", le_wsoff, le_l2, le_l1, unused ? "(unused) " : "", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					deferred_cond (ts);
					/* decrement count */
					add_to_ins_chain (compose_ins (INS_DEC, 1, 2, ARG_REGIND | ARG_DISP, REG_WPTR, (le_wsoff+1) << WSH, ARG_REGIND | ARG_DISP, REG_WPTR, (le_wsoff+1) << WSH, ARG_REG | ARG_IMP, REG_CC));
					/* jump over if count == 0 */
					if (unused && !options.pause_at_loopend) {
						ts->stack->must_set_cmp_flags = 0;
						compose_cond_jump (ts, CC_E, le_l2);		/* confused logic, but.. */
					} else {
						ts->stack->must_set_cmp_flags = 0;
						compose_cond_jump (ts, CC_NONE, le_l1);
						/* increment index */
						if (!unused) {
							add_to_ins_chain (compose_ins (INS_INC, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, le_wsoff << WSH, ARG_REGIND | ARG_DISP, REG_WPTR, le_wsoff << WSH));
						}
						if (options.pause_at_loopend) {
							/* FIX: if we're using the new CCSP in multiprocessor-mode, better do this as PAUSE then JUMP */
							if ((options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_MP)) == (KRNLIFACE_NEWCCSP | KRNLIFACE_MP)) {
								arch->compose_kcall (ts, K_PAUSE, 0, 0);
								add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, le_l2));
							} else {
								add_to_ins_chain (compose_ins (INS_PJUMP, 1, 0, ARG_LABEL, le_l2));
							}
						} else {
							add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, le_l2));
						}
					}
					add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, le_l1));
				}
				break;
				/*}}}*/
				/*{{{  ETCL2  (loop-end3)*/
			case ETCL2:		/* new loop-end3 */
				glob_in_icount++;
				etc_code = etc_code->next;
				le_wsoff = etc_code->opd;
				etc_code = etc_code->next;
				le_l1 = etc_code->opd;		/* loop end */
				etc_code = etc_code->next;
				le_l2 = etc_code->opd;		/* loop start */
				if (options.annotate_output) {
					sprintf (sbuffer, ".LOOPEND %d, L%d, L%d [tsd=%d,%d]", le_wsoff, le_l2, le_l1, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				deferred_cond (ts);
				/* decrement count */
				add_to_ins_chain (compose_ins (INS_DEC, 1, 2, ARG_REGIND | ARG_DISP, REG_WPTR, (le_wsoff+1) << WSH, ARG_REGIND | ARG_DISP, REG_WPTR, (le_wsoff+1) << WSH, ARG_REG | ARG_IMP, REG_CC));
				/* jump over if count == 0 */
				ts->stack->must_set_cmp_flags = 0;
				compose_cond_jump (ts, CC_NONE, le_l1);
				/* increment index (by STEP) */
				tmp_reg = tstack_newreg (ts->stack);
				add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, (le_wsoff + 2) << WSH, ARG_REG, tmp_reg));
				add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_WPTR, le_wsoff << WSH, ARG_REGIND | ARG_DISP, REG_WPTR, le_wsoff << WSH));
				/* add_to_ins_chain (compose_ins (INS_INC, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, le_wsoff << WSH, ARG_REGIND | ARG_DISP, REG_WPTR, le_wsoff << WSH)); */
				if (options.pause_at_loopend) {
					/* FIX: if we're using the new CCSP in multiprocessor-mode, better do this as PAUSE then JUMP */
					if ((options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_MP)) == (KRNLIFACE_NEWCCSP | KRNLIFACE_MP)) {
						arch->compose_kcall (ts, K_PAUSE, 0, 0);
						add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, le_l2));
					} else {
						add_to_ins_chain (compose_ins (INS_PJUMP, 1, 0, ARG_LABEL, le_l2));
					}
				} else {
					add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, le_l2));
				}
				add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, le_l1));
				break;
				/*}}}*/
				/*{{{  ETCL3  (backwards loop-end)*/
			case ETCL3:		/* new loop-end backwards */
				glob_in_icount++;
				etc_code = etc_code->next;
				le_wsoff = etc_code->opd;
				etc_code = etc_code->next;
				le_l1 = etc_code->opd;		/* loop end */
				etc_code = etc_code->next;
				le_l2 = etc_code->opd;		/* loop start */
				if (options.annotate_output) {
					sprintf (sbuffer, ".LOOPEND %d, L%d, L%d [tsd=%d,%d]", le_wsoff, le_l2, le_l1, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				deferred_cond (ts);
				/* decrement count */
				add_to_ins_chain (compose_ins (INS_DEC, 1, 2, ARG_REGIND | ARG_DISP, REG_WPTR, (le_wsoff+1) << WSH, ARG_REGIND | ARG_DISP, REG_WPTR, (le_wsoff+1) << WSH, ARG_REG | ARG_IMP, REG_CC));
				/* jump over if count == 0 */
				ts->stack->must_set_cmp_flags = 0;
				compose_cond_jump (ts, CC_NONE, le_l1);
				/* increment index */
				add_to_ins_chain (compose_ins (INS_DEC, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, le_wsoff << WSH, ARG_REGIND | ARG_DISP, REG_WPTR, le_wsoff << WSH));
				if (options.pause_at_loopend) {
					/* FIX: if we're using the new CCSP in multiprocessor-mode, better do this as PAUSE then JUMP */
					if ((options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_MP)) == (KRNLIFACE_NEWCCSP | KRNLIFACE_MP)) {
						arch->compose_kcall (ts, K_PAUSE, 0, 0);
						add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, le_l2));
					} else {
						add_to_ins_chain (compose_ins (INS_PJUMP, 1, 0, ARG_LABEL, le_l2));
					}
				} else {
					add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, le_l2));
				}
				add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, le_l1));
				break;
				/*}}}*/
				/*{{{  ETCL4  (mobile-space usage)*/
			case ETCL4:		/* mobile-space usage */
				etc_code = etc_code->next;
				ms_usage = etc_code->opd;
				if (options.annotate_output) {
					sprintf (sbuffer, ".SETMS %d [tsd=%d,%d]", ms_usage, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				ts->ms_size = ms_usage;
				break;
				/*}}}*/
				/*{{{  ETCL5  (mobile-space initialisation) */
			case ETCL5:		/* mobile-space initialisation code */
				{
					int msp_offset, z, count;
					int *slot_offsets, *data_offsets;

					glob_in_icount++;
					etc_code = etc_code->next;
					msp_offset = etc_code->opd;
					etc_code = etc_code->next;
					count = etc_code->opd;
					if (options.annotate_output) {
						sprintf (sbuffer, ".MOBILEINIT %d %d [tsd=%d,%d]", msp_offset, count, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}
					/* FIXME: this is a nasty problem if we declare one mobile, <start %..> gets eaten up at the start of RTL_CODE... .. */
					/* inserting something which will go before <start %..> fixes it.. */
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("generating .MOBILEINIT")));
					if (count > 0) {
						slot_offsets = (int *)smalloc (count * sizeof (int));
						data_offsets = (int *)smalloc (count * sizeof (int));

						for (z=0; z<count; z++) {
							etc_code = etc_code->next;
							slot_offsets[z] = etc_code->opd;
							etc_code = etc_code->next;
							data_offsets[z] = etc_code->opd;
						}
						gen_mobilespace_init (ts, msp_offset, count, slot_offsets, data_offsets);

						sfree (data_offsets);
						sfree (slot_offsets);
					}
				}
				break;
				/*}}}*/
				/*{{{  ETCL6  (TCOFF record)*/
			case ETCL6:		/* TCOFF record */
				{
					int tcoff_tag = *(int *)etc_code->o_bytes;
					int tcoff_len = etc_code->o_len - sizeof (int);
					unsigned char *tcoff_data = (unsigned char *)etc_code->o_bytes + sizeof (int);

					if (options.annotate_output) {
						unsigned char *outbuf = smalloc ((tcoff_len * 4) + 64);
						int p;
						unsigned char *sptr;
						int i;

						p = sprintf ((char *)outbuf, ".TCOFF %d \"%s\" %d ", tcoff_tag,
							tcoff_tag_names[(tcoff_tag <= TCOFF_MAX_TAG) ? tcoff_tag : TCOFF_INVALID_TAG], tcoff_len);
						i = tcoff_len;
						sptr = tcoff_data;
						while (i) {
							if ((*sptr >= ' ') && !(*sptr & 0x80)) {
								outbuf[p++] = *sptr;
								outbuf[p] = '\0';
							} else {
								p += sprintf ((char *)outbuf + p, "\\x%2.2X", *sptr);
							}
							sptr++, i--;
						}
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ((char *)outbuf)));
						flush_ins_chain ();
						sfree (outbuf);
					}
					switch (tcoff_tag) {
						/*{{{  DESCRIPTOR_TAG -- descriptor line*/
					case DESCRIPTOR_TAG:
						if (!ts->jentry_name || options.not_main_module) {
							/* uninteresting */
							break;
						} else {
							char *pname = (char *)tcoff_data;
							char *ch;
							int pidx = 0, psidx;
							int failed = 1;
							int cdspecs = 0;

							while ((pidx < (tcoff_len - 7)) && strncmp (pname, "PROC", 4)) {
								pidx++, pname++;
							}
							if ((pidx < (tcoff_len - 7)) && !strncmp (pname, "PROC", 4)) {
								pidx += 4;
								pname += 4;
								failed = 0;
							}
							while ((pidx < (tcoff_len - 3)) && (*pname == ' ')) {
								pidx++, pname++;
							}
							/* should have PROC name now.. */
							for (psidx = pidx, ch = pname; (pidx < (tcoff_len - 3)) && (*ch != ' ') && (*ch != '('); ch++) {
								pidx++;
							}
							if (!failed && !strncmp (ts->jentry_name, pname, (pidx - psidx)) && (strlen (ts->jentry_name) == (pidx - psidx)) &&
								!(options.kernel_interface & KRNLIFACE_RMOX)) {
								int tlp_type;

								/* found relevant PROC */
								tlp_type = check_top_level_process_signature (ts, pname, tcoff_len - psidx, &cdspecs);
								if (options.tlp_interface != TLP_INVALID) {
									if ((tlp_type != TLP_INVALID) && (tlp_type != options.tlp_interface)) {
										fprintf (stderr, "%s: warning: specified top-level process interface does not match detected interface.\n", progname);
									}
									tlp_type = options.tlp_interface;
								}
								if (tlp_type == TLP_INVALID) {
									fprintf (stderr, "%s: fatal: top-level process does not have required process signature/usage.\n", progname);
									fprintf (stderr, "Should be something like:\n");
									fprintf (stderr, "    PROC %s (CHAN BYTE kyb%s, scr%s, err%s)\n", ts->jentry_name,
										cdspecs ? "?" : "", cdspecs ? "!" : "", cdspecs ? "!" : "");
									exit (EXIT_FAILURE);
								} else {
									/* add special variable to output */
									set_ifacetype = tlp_type;
								}
							}
						}
						break;
						/*}}}*/
					}
				}
				break;
				/*}}}*/
				/*{{{  ETCL7  (load workspace-map)*/
			case ETCL7:
				{
					int mpp_offset, wsmaplab;

					glob_in_icount++;
					etc_code = etc_code->next;
					mpp_offset = etc_code->opd;
					etc_code = etc_code->next;
					wsmaplab = etc_code->opd;

					if (options.annotate_output) {
						if (mpp_offset != OCC21_NO_SLOT) {
							sprintf (sbuffer, ".LOADWSMAP %d L%d [tsd=%d,%d]", mpp_offset, wsmaplab, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						} else {
							sprintf (sbuffer, ".LOADWSMAP L%d [tsd=%d,%d]", wsmaplab, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						}
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}

					ts->stack->old_a_reg = tstack_newreg (ts->stack);
					ts->stack->old_b_reg = tstack_newreg (ts->stack);
					ts->stack->old_ts_depth = 2;

					if (mpp_offset != OCC21_NO_SLOT) {
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, mpp_offset << WSH, ARG_REG, ts->stack->old_a_reg));
					} else {
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REG, ts->stack->old_a_reg));
					}
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, wsmaplab, ARG_REG, ts->stack->old_b_reg));
					arch->compose_kcall (ts, K_LDWSMAP, 2, 0);
					ts->stack->must_set_cmp_flags = 0;
				}
				break;
				/*}}}*/
				/*{{{  ETCL8  (unload workspace-map)*/
			case ETCL8:
				{
					int mpp_offset, wsmaplab;

					glob_in_icount++;
					etc_code = etc_code->next;
					mpp_offset = etc_code->opd;
					etc_code = etc_code->next;
					wsmaplab = etc_code->opd;

					if (options.annotate_output) {
						if (mpp_offset != OCC21_NO_SLOT) {
							sprintf (sbuffer, ".UNLOADWSMAP %d L%d [tsd=%d,%d]", mpp_offset, wsmaplab, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						} else {
							sprintf (sbuffer, ".UNLOADWSMAP L%d [tsd=%d,%d]", wsmaplab, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
						}
						add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
					}

					ts->stack->old_a_reg = tstack_newreg (ts->stack);
					ts->stack->old_b_reg = tstack_newreg (ts->stack);
					ts->stack->old_ts_depth = 2;

					if (mpp_offset != OCC21_NO_SLOT) {
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, mpp_offset << WSH, ARG_REG, ts->stack->old_a_reg));
					} else {
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REG, ts->stack->old_a_reg));
					}
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, wsmaplab, ARG_REG, ts->stack->old_b_reg));
					arch->compose_kcall (ts, K_ULWSMAP, 2, 0);
					ts->stack->must_set_cmp_flags = 0;
				}
				break;
				/*}}}*/
				/*{{{  (default) */
			default:
				fprintf (stderr, "%s: warning: bad/inconsistent ETC (0x%8.8x).\n", progname, x_opd);
				break;
				/*}}}*/
			}
			/*}}}*/
		} else {
			/*{{{  XSTL, XSTLN, NCALL, NRET, secondary instructions*/
			glob_in_icount++;
			switch (x_opd) {
			case I_XSTL:
			case I_XSTLN:
				glob_in_icount++;
				do_code_special (ts, x_opd, x_fn - I_OPR, arch);
				break;
			case I_NCALL:
			case I_NRET:
			case I_NSTARTP:
			case I_NNEG:
			case I_NLW:
			case I_NSW:
			case I_NALTEND:
			case I_NMWENB:
			case I_NMWDIS:
			case I_NMWALTWT:
			case I_NMWALT:
			case I_NMWALTEND:
			case I_MWS_BINIT:
			case I_MWS_PBRILNK:
			case I_MWS_PBRULNK:
			case I_MWS_PPILNK:
			case I_MWS_PBENROLL:
			case I_MWS_PBRESIGN:
			case I_MWS_PBADJSYNC:
			case I_MWS_SYNC:
			case I_MWS_ALTLOCK:
			case I_MWS_ALTUNLOCK:
			case I_MWS_ALT:
			case I_MWS_ALTEND:
			case I_MWS_ENB:
			case I_MWS_DIS:
			case I_MWS_ALTPOSTLOCK:
			case I_MWS_PPBASEOF:
			case I_MWS_PPPAROF:
			case I_NLABADDR:
			case I_NJCSUB0:
				glob_in_icount++;
				do_code_nocc_special (ts, &etc_code, arch);
				break;
			case I_NJTABLE:
				glob_in_icount++;
				do_code_nocc_special (ts, &etc_code, arch);
				/* will force alignment of the following stuff */
				if (options.internal_options & INTERNAL_ALIGNEDCODE) {
					flush_ins_chain ();
					trtl = new_rtl ();
					trtl->type = RTL_ALIGN;
					trtl->u.alignment = 2;
					add_to_rtl_chain (trtl);
				}
				break;
			case I_NWSADJ:
				if (options.annotate_output) {
					sprintf (sbuffer, ".SETWSADJ %d [tsd=%d,%d]", x_fn - I_OPR, ts->stack->old_ts_depth, ts->stack->old_fs_depth);
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
				}
				ts->ws_adjust = x_fn - I_OPR;
#if 0
fprintf (stderr, "setting ts->ws_adjust = %d\n", ts->ws_adjust);
#endif
				break;
			default:
				if (options.annotate_output) {
					add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_of_secondary (ts->stack, x_opd)));
				}
				do_code_secondary (ts, x_opd, arch);
				break;
			}
			/*}}}*/
		}
		tstack_note (ts->stack);
		if (ts->stack->ts_depth < 0) {
			fprintf (stderr, "%s: warning: unexpected tstack underflow, func = %d, operand = %d\n", progname, x_fn, x_opd);
			ts->stack->ts_depth = 0;
		}
		if (!ts->stack->old_ts_depth && ts->stack->ts_depth && !control_in_split (ts)) {
			ts->zeropoint = *savedheadptr;				/* record instruction if tsdepth was zero previously */
		}
		constmap_cleanup (ts->stack);
		etc_code = etc_code->next;
	}
	if (ts->stack->fs_depth != 0) {
		fprintf (stderr, "%s: warning: floating point stack left at depth %d\n", progname, ts->stack->fs_depth);
		ts->stack->fs_depth = 0;
	}
	flush_ins_chain ();
	if (set_ifacetype != TLP_INVALID) {
		set_ifacetype |= (options.rtflags & TLP_EFLAGMASK);
	}
	/*{{{  code for flushing screen*/
	/* this was hoisted from below to stop it landing in the data area */
	/* now that we know the interface, dump code needed to flush the screen (or not) */
	/* arrange to communicate FLUSH (0xff) down the screen channel, to flush anything pending:
	 *	movb $0xff, (%ebp)		-- store 0xff in Wptr[0]
	 *	movl %ebp, %breg		-- LDLP 0
	 *	movl W_LINK(%ebp), %areg	-- LDL -2
	 *	..kernel call..
	 *
	 * note: SHARED top-level channels are not handled (yet..)
	 */
	if (ts->flushscreenpoint >= 0) {
		int scr_offset;

		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->flushscreenpoint));
		/* we got here via a regular PROC return, so Wptr has been adjusted -- better refix (pretending to be the top-level PROC) */
		/* frmb: the required offset is now stored in ts->ws_adjust */
		add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_CONST, ts->ws_adjust, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR));

		/*{{{  code for FORK synchronisation*/
		if ((set_ifacetype != TLP_INVALID) && (set_ifacetype & TLP_FORK_BARRIER) && !(set_ifacetype & TLP_FORK_NOWAIT)) {
			/* yes, better synchronise */
			int fboffset = 1;

			switch (set_ifacetype & ~(TLP_EFLAGMASK | TLP_SHAREDMASK)) {
			case TLP_SCR:
			case TLP_KYB:
			case TLP_ERR:
				fboffset++;
				break;
			case TLP_SCRERR:
			case TLP_KYBSCR:
			case TLP_KYBERR:
			case TLP_FSTS:
				fboffset += 2;
				break;
			case TLP_FSTSMEM:
			case TLP_KYBSCRERR:
				fboffset += 3;
				break;
			case TLP_FSTSSIZEDMEM:
				fboffset += 4;
				break;
			}
			/* past any VS/MS params */
			if (ts->vs_size) {
				fboffset++;
			}
			if (ts->ms_size) {
				fboffset++;
			}
			
			ts->stack->old_a_reg = tstack_newreg (ts->stack);
			ts->stack->old_ts_depth = 1;
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, fboffset << WSH, ARG_REG, ts->stack->old_a_reg));
			arch->compose_kcall (ts, K_MT_SYNC, 1, 0);
		}
		/*}}}*/
		switch (set_ifacetype & ~TLP_EFLAGMASK) {
		default:
			scr_offset = 0;
			break;
		case TLP_SCR:
		case TLP_SCRERR:
			scr_offset = 1;
			break;
		case TLP_KYBSCR:
		case TLP_KYBSCRERR:
			scr_offset = 2;
			break;
		}
		if ((set_ifacetype & TLP_MPP_BARRIER) && (scr_offset > 0)) {
			/* barrier in the way */
			scr_offset++;
		}
		if (scr_offset > 0) {
			ts->stack->old_a_reg = tstack_newreg (ts->stack);
			ts->stack->old_b_reg = tstack_newreg (ts->stack);
			ts->stack->old_ts_depth = 2;
			add_to_ins_chain (compose_ins (INS_MOVEB, 1, 1, ARG_CONST | ARG_IS8BIT, 0xff, ARG_REGIND, REG_WPTR));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, ts->stack->old_b_reg));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, scr_offset << WSH, ARG_REG, ts->stack->old_a_reg));
			if (set_ifacetype & TLP_MPP_BARRIER) {
				/* better de-reference it too */
				add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg));
			}
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (".MAGIC SCREEN")));
			arch->compose_kcall (ts, K_OUT8, 2, 0);
		}
		/* fixup Wptr again */
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR));
		/* do scheduler! */
		if (options.kernel_interface & KRNLIFACE_MP) {
			arch->compose_kcall (ts, K_SHUTDOWN, 0, 0);
		} else {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 1, ARG_NAMEDLABEL, string_dup ("&occam_finished")));
			add_to_ins_chain (arch->compose_kjump (ts, INS_JUMP, 0, kif_entry (K_OCCSCHEDULER)));
		}
		flush_ins_chain ();

		if (set_ifacetype & TLP_MPP_BARRIER) {
			fprintf (stderr, "%s: warning: top-level process SUSPENDs\n", progname);
		}
	}
	/*}}}*/
	/*{{{  generate maps for any code-mapped PROCs/FUNCTIONs*/
	{
		void *lcl[] = {(void *)arch, (void *)ts};

		procinf_iterate (gencodemap, (void *)lcl);
		flush_ins_chain ();
	}
	/*}}}*/

	trtl = new_rtl ();
	trtl->type = RTL_ALIGN;
	trtl->u.alignment = 2;
	add_to_rtl_chain (trtl);
	/*{{{  drop floating-point constants*/
	/* drop floating-point range constants */
	/* -(2^31) */
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->floatrange_label[0]));
	if (options.internal_options & INTERNAL_BIGENDIAN) {
		declare_data_bytes (mem_ndup ("\xc1\xe0\x00\x00\x00\x00\x00\x00", 8), 8);
	} else {
		declare_data_bytes (mem_ndup ("\x00\x00\x00\x00\x00\x00\xe0\xc1", 8), 8);
	}
	/* 2^31 */
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->floatrange_label[1]));
	if (options.internal_options & INTERNAL_BIGENDIAN) {
		declare_data_bytes (mem_ndup ("\x41\xe0\x00\x00\x00\x00\x00\x00", 8), 8);
	} else {
		declare_data_bytes (mem_ndup ("\x00\x00\x00\x00\x00\x00\xe0\x41", 8), 8);
	}
	/* -(2^63) */
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->floatrange_label[2]));
	if (options.internal_options & INTERNAL_BIGENDIAN) {
		declare_data_bytes (mem_ndup ("\xc3\xe0\x00\x00\x00\x00\x00\x00", 8), 8);
	} else {
		declare_data_bytes (mem_ndup ("\x00\x00\x00\x00\x00\x00\xe0\xc3", 8), 8);
	}
	/* 2^63 */
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->floatrange_label[3]));
	if (options.internal_options & INTERNAL_BIGENDIAN) {
		declare_data_bytes (mem_ndup ("\x43\xe0\x00\x00\x00\x00\x00\x00", 8), 8);
	} else {
		declare_data_bytes (mem_ndup ("\x00\x00\x00\x00\x00\x00\xe0\x43", 8), 8);
	}
	/*}}}*/
	/*{{{  reserve some memory for floating-point if necessary*/
	if (options.internal_options & INTERNAL_TEMPFLOAT) {
		trtl = new_rtl ();
		trtl->type = RTL_ALIGN;
		trtl->u.alignment = 2;
		add_to_rtl_chain (trtl);

		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->floatspace_label));
		declare_data_bytes (mem_ndup ("\x00\x00\x00\x00\x00\x00\x00\x00", 8), 8);
	}
	if (options.internal_options & INTERNAL_FLOATCONV) {
		trtl = new_rtl ();
		trtl->type = RTL_ALIGN;
		trtl->u.alignment = 2;
		add_to_rtl_chain (trtl);

		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->floatconv_label));
		if (options.internal_options & INTERNAL_BIGENDIAN) {
			declare_data_bytes (mem_ndup ("\x43\x30\x00\x00\x80\x00\x00\x00", 8), 8);
		} else {
			declare_data_bytes (mem_ndup ("\x00\x00\x00\x80\x00\x00\x30\x43", 8), 8);
		}
	}
	/*}}}*/
	/*{{{  drop debugging info as needed*/
	/* drop debugging info if appropriate */
	flush_ins_chain ();
	trtl = new_rtl ();
	trtl->type = RTL_ALIGN;
	trtl->u.alignment = 2;
	add_to_rtl_chain (trtl);
	/* procedure and file names out first */
	if (options.debug_options && arch->compose_debug_procnames && arch->compose_debug_filenames) {
		arch->compose_debug_procnames (ts);
		arch->compose_debug_filenames (ts);
	}
	/* zero-div entry point */
	if ((options.debug_options & DEBUG_OVERFLOW) && arch->compose_debug_zero_div) {
		arch->compose_debug_zero_div (ts);
	}
	/* floating-point error entry point */
	if ((options.debug_options & DEBUG_FLOAT) && arch->compose_debug_floaterr) {
		arch->compose_debug_floaterr (ts);
	}
	/* overflow entry point */
	if ((options.debug_options & DEBUG_OVERFLOW) && arch->compose_debug_overflow) {
		arch->compose_debug_overflow (ts);
	} else if (!options.disable_checking) {
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->overflow_label));
		arch->compose_kcall (ts, K_BNSETERR, 0, 0);
	}
	/* deadlock setup entry point */
	if ((options.debug_options & DEBUG_DEADLOCK) && arch->compose_debug_deadlock_set) {
		arch->compose_debug_deadlock_set (ts);
	}
	/* range-error entry point */
	if ((options.debug_options & DEBUG_RANGESTOP) && arch->compose_debug_rangestop) {
		arch->compose_debug_rangestop (ts);
	}
	/*}}}*/
	/*{{{  drop WS/VS/MS as necessary*/
	flush_ins_chain ();
	if (!options.not_main_module) {
		trtl = new_rtl ();
		trtl->type = RTL_WSVS;
#if 0
fprintf (stderr, "ts->ws_size = %d\n", ts->ws_size);
#endif
		trtl->u.wsvs.ws_bytes = (ts->ws_size << WSH);
		trtl->u.wsvs.ws_adjust = (ts->ws_adjust << WSH);
		trtl->u.wsvs.vs_bytes = (ts->vs_size << WSH);
		trtl->u.wsvs.ms_bytes = (ts->ms_size << WSH);
		add_to_rtl_chain (trtl);
	}
	/* drop library WS/VS counts if appropriate */
	if (def_lib_head) {
		add_chain_to_rtl_chain (def_lib_head);
	}
	if (options.is_library && ts->libout_pending) {
		/* dump WS+6 and VS to output */
		sprintf (sbuffer, "%s_%s_wsbytes", (options.extref_prefix ? options.extref_prefix : ""), ts->libout_name);
		trtl = new_rtl ();
		trtl->type = RTL_PUBLICSETNAMEDLABEL;
		trtl->u.label_name = string_dup (sbuffer);
		add_to_rtl_chain (trtl);
		i = (ts->ws_size + 6) * 4;
		declare_data_bytes (mem_ndup ((char *)&i, sizeof (int)), sizeof (int));
		sprintf (sbuffer, "%s_%s_wsadjust", (options.extref_prefix ? options.extref_prefix : ""), ts->libout_name);
		trtl = new_rtl ();
		trtl->type = RTL_PUBLICSETNAMEDLABEL;
		trtl->u.label_name = string_dup (sbuffer);
		add_to_rtl_chain (trtl);
		i = ts->ws_adjust * 4;
		declare_data_bytes (mem_ndup ((char *)&i, sizeof (int)), sizeof (int));
		sprintf (sbuffer, "%s_%s_vsbytes", (options.extref_prefix ? options.extref_prefix : ""), ts->libout_name);
		trtl = new_rtl ();
		trtl->type = RTL_PUBLICSETNAMEDLABEL;
		trtl->u.label_name = string_dup (sbuffer);
		add_to_rtl_chain (trtl);
		i = ts->vs_size * 4;
		declare_data_bytes (mem_ndup ((char *)&i, sizeof (int)), sizeof (int));
		sprintf (sbuffer, "%s_%s_msbytes", (options.extref_prefix ? options.extref_prefix : ""), ts->libout_name);
		trtl = new_rtl ();
		trtl->type = RTL_PUBLICSETNAMEDLABEL;
		trtl->u.label_name = string_dup (sbuffer);
		add_to_rtl_chain (trtl);
		i = ts->ms_size * 4;
		declare_data_bytes (mem_ndup ((char *)&i, sizeof (int)), sizeof (int));
		ts->libout_pending = 0;
		sfree (ts->libout_name);
	}
	/*}}}*/
	/*{{{  stop or halt error-mode (global choice) */
	if (!options.not_main_module && ts->jentry_name) {
		int tmp = (options.debug_options & DEBUG_STOPMODE);

		trtl = new_rtl ();
		trtl->type = RTL_PUBLICSETNAMEDLABEL;
		sprintf (sbuffer, "%s_occam_errormode", (options.extref_prefix ? options.extref_prefix : ""));
		trtl->u.label_name = string_dup (sbuffer);
		add_to_rtl_chain (trtl);
		declare_data_bytes (mem_ndup ((char *)&tmp, sizeof (int)), sizeof (int));
	}
	/*}}}*/
	/*{{{  top-level interface handling*/
	if (set_ifacetype != TLP_INVALID) {
		/* dump "_occam_tlp_iface" symbol */
		trtl = new_rtl ();
		trtl->type = RTL_PUBLICSETNAMEDLABEL;
		sprintf (sbuffer, "%s_occam_tlp_iface", (options.extref_prefix ? options.extref_prefix : ""));
		trtl->u.label_name = string_dup (sbuffer);
		add_to_rtl_chain (trtl);
		declare_data_bytes (mem_ndup ((char *)&set_ifacetype, sizeof (int)), sizeof (int));
	}
	/*}}}*/
	the_last_lab = ts->last_lab;
	the_last_tstate = ts;
	/* just translated, set the RTL last virtual register */
	rtl_set_lastvreg (ts->stack->reg_counter);

	if (options.internal_options & INTERNAL_DUMPPROCTAB) {
		procinf_dumptab (stderr);
	}
	return rtl_head;
}
/*}}}*/
/*{{{  static void gencodemap (procinf *pinf, void *param)*/
/*
 *	called for each mapped PROC/FUNCTION/other.  If the map label is set, dumps info
 */
static void gencodemap (procinf *pinf, void *param)
{
	void **lcl = (void **)param;
	/* arch_t *arch = (arch_t *)(lcl[0]); */
	tstate *ts = (tstate *)(lcl[1]);
	rtl_chain *trtl;
	int i;

	/* always allocate name label */
	pinf->namelab = ++(ts->last_lab);
	if ((pinf->eplab < 0) || (pinf->maplab < 0)) {
		return;
	}
	for (i=0; i<pinf->refs_cur; i++) {
		if (pinf->refs[i]->is_internal) {
			/* make an internal name for pinf */
			if (!pinf->iname) {
				pinf->inamelen = pinf->namelen + 2;
				pinf->iname = (char *)smalloc (pinf->inamelen + 1);
				sprintf (pinf->iname, "%s%%i", pinf->name);
				pinf->inamelab = ++(ts->last_lab);
			}
			break;		/* for() */
		}
	}
	/* this one */
	flush_ins_chain ();
	trtl = new_rtl ();
	trtl->type = RTL_CODEMAP;
	trtl->u.codemap.pinf = pinf;
	add_to_rtl_chain (trtl);

	return;
}
/*}}}*/
/*{{{  static int addlastlabtocodemap (tstate *ts)*/
/*
 *	scans backwards for the last SETLABEL instruction (or SETFLABEL) and adds
 *	it as an internal label to the given code-map
 */
static int addlastlabtocodemap (tstate *ts)
{
#if 0
fprintf (stderr, "addlastlabtocodemap(): ts->cpinfo = %p\n", ts->cpinfo);
#endif
	if (ts->cpinfo) {
		ins_chain *setlabins = rtl_scan_iclass_backward (ins_tail, ICLASS_SETLAB);

#if 0
fprintf (stderr, "addlastlabtocodemap(): search backwards found instruction at %p\n", setlabins);
#endif
		if (setlabins) {
			procinf *refd = NULL;

			switch (setlabins->type) {
			case INS_SETLABEL:
				refd = procinf_internallab (ts->cpinfo, ArgLabel (setlabins->in_args[0]));
				break;
			case INS_SETFLABEL:
				/* need a real label */
				{
					ins_chain *slins = compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ++(ts->last_lab));

					refd = procinf_internallab (ts->cpinfo, ArgLabel (slins->in_args[0]));
					/* insert manually */
					if (setlabins == ins_tail) {
						add_to_ins_chain (slins);
					} else {
						if (setlabins->next) {
							slins->next = setlabins->next;
							slins->next->prev = slins;
						}
						slins->prev = setlabins;
						setlabins->next = slins;
					}
				}
				break;
			}
			if (refd) {
				procinf_addref (ts->cpinfo, refd);
				return 1;
			}
		}
	}
	return 0;
}
/*}}}*/
/*{{{  static int check_top_level_process_signature (tstate *ts, char *namesig, int len, int *cdspecs_ptr)*/
/*
 *	int check_top_level_process_signature (tstate *ts, char *namesig, int len, int *cdspecs_ptr)
 *	checks the TCOFF signature of a top-level process, to make sure it conforms to a known thing
 *	`cdspecs_ptr' is set if "kyb", etc. are annotated with "?" or "!", eg, "CHAN OF BYTE kyb?, scr!, err!"
 *	returns indicator (TLP_...)
 */
static int check_top_level_process_signature (tstate *ts, char *namesig, int len, int *cdspecs_ptr)
{
	char *local = (char *)smalloc (len+1);
	char *name_list[3];	/* channel names */
	char *type_list[3];	/* channel types */
	int usage_list[3];	/* channel usage (TLU_...), from usage block */
	int spec_list[3];	/* specified usage (TLU_...), taken out of channel names */
	int chan_list[3];	/* actual channels (TLC_...), guessed from names */
	int shared_flag[3];	/* if SHARED channel, flag this */
	int n_params, i;
	char *ch, *dh, *eh, *fh;
	int is_sp_type;
	int result = 0, specs_didwarn = 0;
	int empty_mem_dim = 0;
	int flags = 0;

	memcpy (local, namesig, len);
	local[len] = '\0';

#if 0
fprintf (stderr, "check: local = [%s]\n", local);
#endif
	*cdspecs_ptr = 0;
	for (ch = local; (*ch != '(') && (*ch != '\0'); ch++);
	if (*ch == '\0') {
		/* this definitely shouldn't happen */
		sfree (local);
		return TLP_INVALID;
	}
	*ch = '\0';
	ch++;
	for (dh = ch; (*dh != '\0') && (*dh != ')'); dh++);
	if (*dh == '\0') {
		/* this shouldn't happen either */
		sfree (local);
		return TLP_INVALID;
	}
	*dh = '\0';
	dh++;
	/* oki, local = proc name, ch = params, dh = usage info */
	for (i=0; i<3; i++) {
		name_list[i] = NULL;
		type_list[i] = NULL;
		usage_list[i] = TLU_UNKNOWN;
		spec_list[i] = TLU_UNKNOWN;
		chan_list[i] = TLC_UNKNOWN;
		shared_flag[i] = 0;
	}
	n_params = 0;
	do {
		char *last;

		/* search for comma or end */
		for (eh=ch; (*eh != ',') && (*eh != '\0'); eh++);
		last = eh - 1;
		if (*eh == ',') {
			*eh = '\0';
			eh++;
		} else {
			/* might have no parameter.. */
			if (eh == ch) {
				break;	/* do/while () */
			}
		}
		/* pick out param at ch..last */
		for (fh = last; (*fh != ' ') && (fh > ch); fh--);
		if (fh <= ch) {
			/* parameter with no whitespace ??! */
			sfree (local);
			return TLP_INVALID;
		}
		*fh = '\0';
		fh++;
		/* ch has type, fh has name */
		name_list[n_params] = fh;
		type_list[n_params] = ch;
		n_params++;
		ch = eh;
	} while (*eh != '\0');
	/* clean up collected info */
#if 0
fprintf (stderr, "check: n_params = %d\n", n_params);
#endif
	for (i=0; i<n_params; i++) {
		for (ch = name_list[i] + (strlen (name_list[i]) - 1); ((*ch == '\n') || (*ch == '\r') || (*ch == ' ')) && (ch > name_list[i]); *ch = '\0', ch--);
		if (ch == name_list[i]) {
			sfree (local);
			return TLP_INVALID;
		}
		/* channel-direction on name ? */
		if ((*ch == '?') || (*ch == '!')) {
			*cdspecs_ptr = 1;
			spec_list[i] = (*ch == '?') ? TLU_INPUT : TLU_OUTPUT;
			*ch = '\0';
			for (ch--; ((*ch == '\n') || (*ch == '\r') || (*ch == ' ')) && (ch > name_list[i]); *ch = '\0', ch--);
			if (ch == name_list[i]) {
				sfree (local);
				return TLP_INVALID;
			}
		} else if (*cdspecs_ptr && !specs_didwarn) {
			/* tell user they should be consistent -- blah, compiler will catch with --strict if really needed */
			/* fprintf (stderr, "%s: warning: should be more consistent with channel-direction specifiers (top-level PROC %s)\n", progname, local); */
			specs_didwarn = 1;
		}
		for (ch = type_list[i] + (strlen (type_list[i]) - 1); ((*ch == '\n') || (*ch == '\r') || (*ch == ' ')) && (ch > name_list[i]); *ch = '\0', ch--);
		if (ch == name_list[i]) {
			sfree (local);
			return TLP_INVALID;
		}
		for (ch = name_list[i]; ((*ch == '\n') || (*ch == '\r') || (*ch == ' ')) && (*ch != '\0'); ch++);
		if (*ch == '\0') {
			sfree (local);
			return TLP_INVALID;
		}
		name_list[i] = ch;
		for (ch = type_list[i]; ((*ch == '\n') || (*ch == '\r') || (*ch == ' ')) && (*ch != '\0'); ch++);
		if (*ch == '\0') {
			sfree (local);
			return TLP_INVALID;
		}
		type_list[i] = ch;
	}
#if 0
fprintf (stderr, "check: dh now [%s]\n", dh);
#endif
	/* might have special indicator for FORK */
	if (!strncmp (dh, " FORK", 5)) {
		dh += 5;
		flags |= TLP_FORK_BARRIER;
	}
	/* and maybe a SUSPEND indicator */
	if (!strncmp (dh, " SUSPEND", 8)) {
		dh += 8;
		flags |= TLP_MPP_BARRIER;
	}
	/* process usage information */
	while ((*dh != '\0') && (*dh != ':')) {
		/* skip any leading whitespace */
		for (; ((*dh == '\n') || (*dh == '\r') || (*dh == ' ')) && ((*dh != '\0') && (*dh != ':')); dh++);
		if ((*dh == '\0') || (*dh == ':')) {
			continue;
		}
		for (ch=dh; ((*ch != '\n') && (*ch != '\r') && (*ch != ' ') && (*ch != '\0') && (*ch != ':')); ch++);
		for (;(*ch == '\n') || (*ch == '\r') || (*ch == ' '); *ch = '\0', ch++);
		/* ch points at start of next whatever, or sat on end marked -- will update to dh, currently pointing at usage data */
		if (!strcmp (dh, "SEQ") || !strcmp (dh, "PRI PAR") || !strcmp (dh, "PAR")) {
			/* do nothing with these */
		} else {
			const int dilen = strlen (dh);

			/* hopefully a name.. */
			for (i=0; i<n_params; i++) {
				const int nilen = strlen (name_list[i]);

				if ((dilen == (nilen + 1)) && !strncmp (name_list[i], dh, nilen) && ((dh[nilen] == '?') || (dh[nilen] == '!'))) {
					/* looks good */
					if (usage_list[i] != TLU_UNKNOWN) {
						/* already set ??! */
						sfree (local);
						return TLP_INVALID;
					}
					usage_list[i] = (dh[nilen] == '?') ? TLU_INPUT : TLU_OUTPUT;
					break;	/* for() */
				}
			}
		}
		dh = ch;
	}
	is_sp_type = 0;
	/* typecheck here -- must all either be "CHAN OF SP" or "CHAN OF BYTE" */
	/* update: need to handle "[1]INT mem" or "[]INT mem" parameters as well -- latter requires an additional dimension parameter */
	for (i=0; i<n_params; i++) {
		if (!strcmp (type_list[i], "CHAN OF BYTE")) {
			continue;
		} else if (!strcmp (type_list[i], "CHAN BYTE")) {
			continue;
		} else if (!strcmp (type_list[i], "CHAN! BYTE") || !strcmp (type_list[i], "CHAN? BYTE")) {
			/* put in chan-dir info here */
			spec_list[i] = (type_list[i][4] == '?') ? TLU_INPUT : TLU_OUTPUT;
			continue;
		}
		/* SP is a named protocol, so may be decorated with a type-hash */
		if (!strcmp (type_list[i], "CHAN OF SP")) {
			is_sp_type = 1;
			continue;
		} else if (!strncmp (type_list[i], "CHAN OF SP@#", 12)) {
			is_sp_type = 1;
			continue;
		}
		if (!strcmp (type_list[i], "SHARED CHAN OF BYTE")) {
			shared_flag[i] = 1;
			continue;
		} else if (!strcmp (type_list[i], "FIXED SHARED CHAN OF BYTE")) {
			shared_flag[i] = 1;
			continue;
		}
		if (is_sp_type && *(type_list[i]) == '[') {
			char *ch = type_list[i];

			/* probably the memory parameter, we hope! */
			ch++;
			if (*ch == ']') {
				empty_mem_dim = 1;
			} else {
				for (; (*ch != ']') && (*ch != '\0'); ch++);
			}
			if (*ch == '\0') {
				/* really ought not to happen.. */
				sfree (local);
				return TLP_INVALID;
			}
			ch++;
			/* be generous.. */
			if (!strcmp (ch, "INT") || !strcmp (ch, "INT32")) {
				continue;
			}
		}
		/* not known type! */
		sfree (local);
		return TLP_INVALID;
	}
	/* try and guess sensible names of parameters */
	for (i=0; i<n_params; i++) {
		if (!is_sp_type && (chan_list[i] == TLC_UNKNOWN)) {
			/*{{{  try and figure out which channel it is (based on name)*/
			if (!strncmp (name_list[i], "key", 3) || !strncmp (name_list[i], "kyb", 3)
					|| !strncmp (name_list[i], "kb", 2) || !strncmp (name_list[i], "in", 2)) {
				/* looks like it's called keyboard */
				chan_list[i] = TLC_KEYBOARD;
			} else if (!strncmp (name_list[i], "scr", 3) || !strncmp (name_list[i], "out", 3)) {
				/* looks like it's called screen */
				chan_list[i] = TLC_SCREEN;
			} else if (!strncmp (name_list[i], "err", 3)) {
				/* looks like it's called error */
				chan_list[i] = TLC_ERROR;
			} else if (strstr (name_list[i], "in")) {
				/* assume input/stdin */
				chan_list[i] = TLC_KEYBOARD;
			} else if (strstr (name_list[i], "out")) {
				/* assume output/stdout */
				chan_list[i] = TLC_SCREEN;
			} else if (strstr (name_list[i], "err")) {
				/* assume stderr */
				chan_list[i] = TLC_ERROR;
			}
			/*}}}*/
		} else if (is_sp_type && (chan_list[i] == TLC_UNKNOWN)) {
			/*{{{  figure out SP channel flavour from names*/
			if (!strcmp (name_list[i], "fs") || !strncmp (name_list[i], "from", 4) || !strncmp (name_list[i], "in", 2)) {
				chan_list[i] = TLC_FS;
			} else if (!strcmp (name_list[i], "ts") || !strncmp (name_list[i], "to", 2) || !strncmp (name_list[i], "out", 3)) {
				chan_list[i] = TLC_TS;
			} else if (!strncmp (name_list[i], "mem", 3) || !strncmp (name_list[i], "free", 4)) {
				/* looks like it's called memory/freespace */
				if (empty_mem_dim) {
					chan_list[i] = TLC_SIZEDMEM;
				} else {
					chan_list[i] = TLC_MEM;
				}
			}
			/*}}}*/
		}
		if (chan_list[i] == TLC_UNKNOWN) {
			/* maybe usage info can help.. */
			if (usage_list[i] == TLU_INPUT) {
				chan_list[i] = is_sp_type ? TLC_FS : TLC_KEYBOARD;
			} else if ((usage_list[i] == TLU_OUTPUT) && is_sp_type) {
				chan_list[i] = TLC_TS;
			} else if ((usage_list[i] == TLU_OUTPUT) && !is_sp_type) {
				int had_scr = 0;
				int j;

				for (j=0; j<i; j++) {
					had_scr |= (chan_list[j] == TLC_SCREEN);
				}
				if (!had_scr) {
					chan_list[i] = TLC_SCREEN;
				} else {
					chan_list[i] = TLC_ERROR;
				}
			}
		}
	}
	/* check direction specifications are all intact/correct */
	for (i=0; i<n_params; i++) {
		if ((chan_list[i] == TLC_KEYBOARD) && ((spec_list[i] == TLU_OUTPUT) || (usage_list[i] == TLU_OUTPUT))) {
			sfree (local);
			return TLP_INVALID;
		} else if ((chan_list[i] == TLC_SCREEN) && ((spec_list[i] == TLU_INPUT) || (usage_list[i] == TLU_INPUT))) {
			sfree (local);
			return TLP_INVALID;
		} else if ((chan_list[i] == TLC_ERROR) && ((spec_list[i] == TLU_INPUT) || (usage_list[i] == TLU_INPUT))) {
			sfree (local);
			return TLP_INVALID;
		}
	}

	result = TLP_INVALID;
	if (n_params == 0) {
		result = TLP_NULL;
	} else if (!is_sp_type && (n_params == 1)) {
		if (chan_list[0] == TLC_KEYBOARD) {
			result = TLP_KYB | (shared_flag[0] ? TLP_SHAREDKYB : 0);
		} else if (chan_list[0] == TLC_SCREEN) {
			result = TLP_SCR | (shared_flag[0] ? TLP_SHAREDSCR : 0);
		} else if (chan_list[0] == TLC_ERROR) {
			result = TLP_ERR | (shared_flag[0] ? TLP_SHAREDERR : 0);
		}
	} else if (n_params == 2) {
		if (is_sp_type && (chan_list[0] == TLC_FS) && (chan_list[1] == TLC_TS)) {
			result = TLP_FSTS;
		} else if (!is_sp_type && (chan_list[0] == TLC_KEYBOARD) && (chan_list[1] == TLC_SCREEN)) {
			result = TLP_KYBSCR | (shared_flag[0] ? TLP_SHAREDKYB : 0) | (shared_flag[1] ? TLP_SHAREDSCR : 0);
		} else if (!is_sp_type && (chan_list[0] == TLC_KEYBOARD) && (chan_list[1] == TLC_ERROR)) {
			result = TLP_KYBERR | (shared_flag[0] ? TLP_SHAREDKYB : 0) | (shared_flag[1] ? TLP_SHAREDERR : 0);
		} else if (!is_sp_type && (chan_list[0] == TLC_SCREEN) && (chan_list[1] == TLC_ERROR)) {
			result = TLP_SCRERR | (shared_flag[0] ? TLP_SHAREDSCR : 0) | (shared_flag[1] ? TLP_SHAREDERR : 0);
		}
	} else if (n_params == 3) {
		if (!is_sp_type && (chan_list[0] == TLC_KEYBOARD) && (chan_list[1] == TLC_SCREEN) && (chan_list[2] == TLC_ERROR)) {
			result = TLP_KYBSCRERR | (shared_flag[0] ? TLP_SHAREDKYB : 0) | (shared_flag[1] ? TLP_SHAREDSCR : 0) | (shared_flag[2] ? TLP_SHAREDERR : 0);
		} else if (is_sp_type && (chan_list[0] == TLC_FS) && (chan_list[1] == TLC_TS) && (chan_list[2] == TLC_MEM)) {
			result = TLP_FSTSMEM;
		} else if (is_sp_type && (chan_list[0] == TLC_FS) && (chan_list[1] == TLC_TS) && (chan_list[2] == TLC_SIZEDMEM)) {
			result = TLP_FSTSSIZEDMEM;
		}
	}
#if 0
fprintf (stderr, "tranx86: checking top-level signature.  %d params (identified as %d):\n", n_params, result);
for (i=0; i<n_params; i++) {
static char *u_text[] = {"dunno", "in", "out"};
static char *c_text[] = {"dunno", "keyb", "scr", "err", "fs", "ts", "mem"};
fprintf (stderr, "[%s]\t[%-16s]\t[%s]\t[%s]\t[%s]\n", type_list[i], name_list[i], u_text[spec_list[i]], u_text[usage_list[i]], c_text[chan_list[i]]);
}
#endif
	sfree (local);
	if (result != TLP_INVALID) {
		result |= flags;
	}
	return result;
}
/*}}}*/
/*{{{  static void gen_mobilespace_init (tstate *ts, int msp_offset, int count, int *slot_offsets, int *data_offsets)*/
/*
 *	void gen_mobilespace_init (tstate *ts, int msp_offset, int count, int *slot_offsets, int *data_offsets)
 *	generates code to initialise mobile-space
 */
static void gen_mobilespace_init (tstate *ts, int msp_offset, int count, int *slot_offsets, int *data_offsets)
{
	int i;
	static char sbuffer[32];
	int out_lab, tmp_reg, tmp_reg2;

	out_lab = ++(ts->last_lab);
	tmp_reg = tstack_newreg (ts->stack);
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, (msp_offset << WSH), ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST | ARG_ISCONST, 0x80000000, ARG_REGIND, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_LABEL, out_lab));
	for (i=0; i<count; i++) {
		if (slot_offsets[i] < 0) {
			if (options.annotate_output) {
				sprintf (sbuffer, ".MOBILEINITMINT %d", data_offsets[i]);
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
			}
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0x80000000, ARG_REGIND | ARG_DISP, tmp_reg, (data_offsets[i] << WSH)));
		} else if (data_offsets[i] > 0) {
			if (options.annotate_output) {
				sprintf (sbuffer, ".MOBILEINITPAIR %d %d", slot_offsets[i], data_offsets[i]);
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
			}
			tmp_reg2 = tstack_newreg (ts->stack);
			add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, tmp_reg, (data_offsets[i] << WSH), ARG_REG, tmp_reg2));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg2, ARG_REGIND | ARG_DISP, tmp_reg, (slot_offsets[i] << WSH)));
		} else if (data_offsets[i] < 0) {
			if (options.annotate_output) {
				sprintf (sbuffer, ".MOBILEINITNULL %d", slot_offsets[i]);
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
			}
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, tmp_reg, (slot_offsets[i] << WSH)));
		} else {
			if (options.annotate_output) {
				sprintf (sbuffer, ".MOBILEINITARRAY %d", slot_offsets[i]);
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));
			}
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0x80000000, ARG_REGIND | ARG_DISP, tmp_reg, (slot_offsets[i] << WSH)));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, tmp_reg, ((slot_offsets[i] + 1) << WSH)));
		}
	}
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, out_lab));
	return;
}
/*}}}*/
/*{{{  static void deferred_cond (tstate *ts)*/
/*
 *	deferred conversion to boolean
 */
static void deferred_cond (tstate *ts)
{
	if (ts->cond != CC_NONE) {
		add_to_ins_chain (compose_ins (INS_SETCC, 1, 1, ARG_COND, ts->cond, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
		add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_CONST, 1, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg));
		ts->cond = CC_NONE;
	}
	return;
}
/*}}}*/
/*{{{  static void compose_cond_jump (tstate *ts, int cond, int label)*/
/*
 *	generates a conditional jump to a label (maybe modifies ts)
 */
static void compose_cond_jump (tstate *ts, int cond, int label)
{
	int jcond;

	if (cond == CC_NONE) {
		jcond = CC_E;
	} else {
		jcond = cond ^ 1;
	}
	if (ts->stack->must_set_cmp_flags) {
		add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
	}
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, jcond, ARG_LABEL, label));
	/* could jump anywhere! */
	constmap_clearall ();
	return;
}
/*}}}*/
/*{{{  static void generate_overflow_code (tstate *ts, int dcode, arch_t *arch)*/
/*
 *	generates interrupt/overflow handling code
 */
static void generate_overflow_code (tstate *ts, int dcode, arch_t *arch)
{
	int this_lab;

	if (options.debug_options & DEBUG_OVERFLOW) {
		this_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NO, ARG_LABEL, this_lab));
		/* generate the necessary constants + jump */
		arch->compose_overflow_jumpcode (ts, dcode);
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
	} else if (!options.disable_checking) {
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_O, ARG_LABEL, ts->overflow_label));
	}

	return;
}
/*}}}*/
/*{{{  static void generate_overflowed_code (tstate *ts, int dcode, arch_t *arch)*/
/*
 *	similar to the above, but assumes the overflow has already happened (rather than checking CC for it)
 */
static void generate_overflowed_code (tstate *ts, int dcode, arch_t *arch)
{
	if (options.debug_options & DEBUG_OVERFLOW) {
		arch->compose_overflow_jumpcode (ts, dcode);
	} else if (!options.disable_checking) {
		arch->compose_kcall (ts, K_BNSETERR, 0, 0);
	}
	return;
}
/*}}}*/
/*{{{  static void generate_range_code (tstate *ts, int rcode, arch_t *arch)*/
/*
 *	void generate_range_code (tstate *ts, int rcode)
 *	generates range error handling code
 */
static void generate_range_code (tstate *ts, int rcode, arch_t *arch)
{
	if (arch->compose_rangestop_jumpcode && (options.debug_options & DEBUG_RANGESTOP)) {
		arch->compose_rangestop_jumpcode (ts, rcode);
	} else {
		arch->compose_kcall (ts, K_BNSETERR, 0, 0);
	}
	return;
}
/*}}}*/
/*{{{  static void do_code_primary (tstate *ts, int prim, int operand, arch_t *arch)*/
/*
 *	void do_code_primary (tstate *ts, int prim, int operand, arch_t *arch)
 *	translates a primary instruction
 */
static void do_code_primary (tstate *ts, int prim, int operand, arch_t *arch)
{
	ins_chain *tmp_ins;

	if (!operand && ((prim == I_LDNLP) || (prim == I_AJW) || (prim == I_ADC))) {
		/* does nothing */
	} else {
		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		if (prim != I_EQC) {
			deferred_cond (ts);
		}
		tstack_setprim (ts->stack, prim, arch);
		switch (prim) {
		case I_LDC:
			tmp_ins = compose_ins (INS_MOVE, 1, 1, ARG_CONST, operand, ARG_REG, ts->stack->a_reg);
			constmap_new (ts->stack->a_reg, VALUE_CONST, operand, tmp_ins);
			add_to_ins_chain (tmp_ins);
			ts->stack->must_set_cmp_flags = 1;
			break;
		case I_LDL:
			tmp_ins = compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, operand << WSH, ARG_REG, ts->stack->a_reg);
			constmap_new (ts->stack->a_reg, VALUE_LOCAL, operand, tmp_ins);
			add_to_ins_chain (tmp_ins);
			ts->stack->must_set_cmp_flags = 1;
			break;
		case I_LDLP:
			if (operand == 0) {
				tmp_ins = compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, ts->stack->a_reg);
			} else {
				tmp_ins = compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, operand << WSH, ARG_REG, ts->stack->a_reg);
			}
			constmap_new (ts->stack->a_reg, VALUE_LOCALPTR, operand, tmp_ins);
			add_to_ins_chain (tmp_ins);
			ts->stack->must_set_cmp_flags = 1;
			break;
		case I_LDNL:
			add_to_ins_chain (compose_ins_ex (EtcPrimary (I_LDNL), INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, ts->stack->a_reg, operand << WSH, ARG_REG, ts->stack->a_reg));
			constmap_remove (ts->stack->a_reg);
			ts->stack->must_set_cmp_flags = 1;
			break;
		case I_LDNLP:
			if (operand != 0) {
				add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, ts->stack->a_reg, operand << WSH, ARG_REG, ts->stack->a_reg));
				constmap_remove (ts->stack->a_reg);
			}
			ts->stack->must_set_cmp_flags = 1;
			break;
		case I_STL:
			switch (constmap_typeof (ts->stack->old_a_reg)) {
			case VALUE_CONST:
				add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, constmap_regconst (ts->stack->old_a_reg), ARG_REGIND | ARG_DISP, REG_WPTR, operand << WSH));
				break;
			case VALUE_LABADDR:
				add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, constmap_regconst (ts->stack->old_a_reg), ARG_REGIND | ARG_DISP, REG_WPTR, operand << WSH));
				break;
			default:
				add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REGIND | ARG_DISP, REG_WPTR, operand << WSH));
				break;
			}
			/* trash any constant offset at that location */
			constmap_removelocal (operand);
			ts->stack->must_set_cmp_flags = 1;
			break;
		case I_STNL:
			switch (constmap_typeof (ts->stack->old_b_reg)) {
			case VALUE_CONST:
				add_to_ins_chain (compose_ins_ex (EtcPrimary (I_STNL), INS_MOVE, 1, 1, ARG_CONST, constmap_regconst (ts->stack->old_b_reg), ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, operand << WSH));
				break;
			default:
				add_to_ins_chain (compose_ins_ex (EtcPrimary (I_STNL), INS_MOVE, 1, 1, ARG_REG, ts->stack->old_b_reg, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, operand << WSH));
				break;
			}
			ts->stack->must_set_cmp_flags = 1;
			break;
		case I_EQC:
			if (!operand) {
				if (ts->stack->must_set_cmp_flags) {
					add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg, ARG_REG | ARG_IMP, REG_CC));
				}
				if (ts->cond == CC_NONE) {
					ts->cond = CC_Z;
				} else {
					ts->cond = (ts->cond ^ 1);
				}
			} else {
				switch (constmap_typeof (ts->stack->a_reg)) {
				default:
					add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, operand, ARG_REG, ts->stack->a_reg, ARG_REG | ARG_IMP, REG_CC));
					break;
				case VALUE_LOCAL:
					add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, operand, ARG_REGIND | ARG_DISP, REG_WPTR, (constmap_regconst (ts->stack->a_reg) << WSH), ARG_REG | ARG_IMP, REG_CC));
					break;
				}
				ts->cond = CC_Z;
			}
			ts->stack->must_set_cmp_flags = 0;
			break;
		case I_ADC:
			/* if constant, update it */
			if (constmap_typeof (ts->stack->a_reg) == VALUE_CONST) {
				int constant = constmap_regconst (ts->stack->a_reg);
				int newconst;

				constmap_remove (ts->stack->a_reg);
				ts->stack->a_reg = tstack_newreg (ts->stack);
				newconst = constant + operand;
				if (!(ts->magic_pending & TS_MAGIC_UNCHECKED) && (((operand > 0) && (newconst < constant)) || ((operand < 0) && (newconst > constant)))) {
					/* overflows and checked, do error */
					generate_overflowed_code (ts, PMOP_ADC, arch);
				} else {
					/* safe */
					tmp_ins = compose_ins_ex (EtcPrimary (I_ADC), INS_MOVE, 1, 1, ARG_CONST, newconst, ARG_REG, ts->stack->a_reg);
					add_to_ins_chain (tmp_ins);
					constmap_new (ts->stack->a_reg, VALUE_CONST, newconst, tmp_ins);
				}

				/* and skip check */
				ts->magic_pending |= TS_MAGIC_UNCHECKED;
			} else {
				if (ts->magic_pending & TS_MAGIC_UNCHECKED) {
					add_to_ins_chain (compose_ins_ex (EtcPrimary (I_ADC), INS_ADD, 2, 1, ARG_CONST, operand, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg));
				} else {
					add_to_ins_chain (compose_ins_ex (EtcPrimary (I_ADC), INS_ADD, 2, 2, ARG_CONST, operand, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg, ARG_REG | ARG_IMP, REG_CC));
				}
				constmap_remove (ts->stack->a_reg);
			}

			if (ts->magic_pending & TS_MAGIC_UNCHECKED) {
				ts->magic_pending &= ~TS_MAGIC_UNCHECKED;
			} else {
				generate_overflow_code (ts, PMOP_ADC, arch);
			}
			ts->stack->must_set_cmp_flags = 0;
			break;
		case I_AJW:
			add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, operand << WSH, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR));
			constmap_clearall ();
			break;
		case I_J:
		case I_CJ:
		case I_CALL:
			fprintf (stderr, "%s: warning: primary instruction %d expected in ETC\n", progname, prim);
			break;
		default:
			fprintf (stderr, "%s: warning: unknown primary instruction %d\n", progname, prim);
			break;
		}
	}
	return;
}
/*}}}*/
/*{{{  static void do_code_special (tstate *ts, int ins, int opd, arch_t *arch)*/
/*
 *	translates a special instruction (less general than secondaries)
 *	opd is any op value, if appropriate
 */
static void do_code_special (tstate *ts, int ins, int opd, arch_t *arch)
{
	ins_chain *tmp_ins;
	static char sbuf[64];

	switch (ins) {
		/*{{{  I_XSTL, I_XSTLN -- STL/LDL combo instruction*/
	case I_XSTL:
		if (options.annotate_output) {
			sprintf (sbuf, "XSTL %d", opd);
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuf)));
		}
		tmp_ins = compose_ins_ex (EtcSpecial(I_XSTL), INS_MOVE, 1, 1, ARG_REG, ts->stack->a_reg, ARG_REGIND | ARG_DISP, REG_WPTR, opd << WSH);
		add_to_ins_chain (tmp_ins);
		break;
	case I_XSTLN:
		if (options.annotate_output) {
			sprintf (sbuf, "XSTLN -%d", opd);
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuf)));
		}
		tmp_ins = compose_ins_ex (EtcSpecial(I_XSTLN), INS_MOVE, 1, 1, ARG_REG, ts->stack->a_reg, ARG_REGIND | ARG_DISP, REG_WPTR, -(opd << WSH));
		add_to_ins_chain (tmp_ins);
		break;
		/*}}}*/
	default:
		fprintf (stderr, "%s: error: unhandled %d (%d) in code special!\n", progname, ins, opd);
		exit (EXIT_FAILURE);
	}
	return;
}
/*}}}*/
/*{{{  static void do_code_nocc_special (tstate *ts, etc_chain **ecodeptr, arch_t *arch)*/
/*
 *	translates a NOCC specific instruction, gives the whole chain incase advanced
 */
static void do_code_nocc_special (tstate *ts, etc_chain **ecodeptr, arch_t *arch)
{
	etc_chain *etc_code = *ecodeptr;
	static char sbuf[64];

	switch (etc_code->opd) {
		/*{{{  I_NCALL -- NOCC CALL instruction*/
	case I_NCALL:
		{
			int adj = etc_code->fn - I_OPR;
			char *fname;

			etc_code = etc_code->next;
			fname = etc_code->o_bytes;

			ts->stack->old_a_reg = ts->stack->a_reg;
			ts->stack->old_b_reg = ts->stack->b_reg;
			ts->stack->old_c_reg = ts->stack->c_reg;
			deferred_cond (ts);
			tstack_setsec (ts->stack, I_NCALL, arch);

			if (options.annotate_output) {
				if (fname) {
					sprintf (sbuf, "NCALL %s %d", fname, adj);
				} else {
					sprintf (sbuf, "NCALL L%d %d", etc_code->opd, adj);
				}
				sprintf (sbuf + strlen (sbuf), " [tsd=%d,%d]", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuf)));
			}

			add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_CONST | ARG_ISCONST, adj << WSH, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR));
#if 0
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_ISCONST | ARG_FLABEL, 0, ARG_REGIND, REG_WPTR));
			if (fname) {
				add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_NAMEDLABEL, string_dup (fname)));
			} else {
				add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, etc_code->opd));
			}
			add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
#endif
			generate_call (ts, etc_code, arch, 0);
			/* XXX: if this was an external C call, need to adjust the workspace back to where it was */
			if (fname && !strncmp (fname, "C.", 2)) {
				add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, adj << WSH, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR));
			}
		}
		break;
		/*}}}*/
		/*{{{  I_NRET -- NOCC RET instruction*/
	case I_NRET:
		{
			int adj = etc_code->fn - I_OPR;

			ts->stack->old_a_reg = ts->stack->a_reg;
			ts->stack->old_b_reg = ts->stack->b_reg;
			ts->stack->old_c_reg = ts->stack->c_reg;
			deferred_cond (ts);
			tstack_setsec (ts->stack, I_NRET, arch);

			if (options.annotate_output) {
				sprintf (sbuf, "NRET %d", adj);
				sprintf (sbuf + strlen (sbuf), " [tsd=%d,%d]", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuf)));
			}

			if (arch->compose_nreturn) {
				/* need to do this a bit more carefully.. */
				arch->compose_nreturn (ts, adj);
			} else {
				int tmpreg = tstack_newreg (ts->stack);

				add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, REG_WPTR, ARG_REG, tmpreg));
				add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, adj << WSH, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR));
				add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, tmpreg));
			}
		}
		break;
		/*}}}*/
		/*{{{  I_NJTABLE -- NOCC JTABLE instruction*/
	case I_NJTABLE:
		{
			int lab = etc_code->fn - I_OPR;
			int tmp_reg;

			ts->stack->old_a_reg = ts->stack->a_reg;
			ts->stack->old_b_reg = ts->stack->b_reg;
			ts->stack->old_c_reg = ts->stack->c_reg;
			deferred_cond (ts);
			tstack_setsec (ts->stack, I_NJTABLE, arch);

			if (options.annotate_output) {
				sprintf (sbuf, "JTABLE %d", lab);
				sprintf (sbuf + strlen (sbuf), " [tsd=%d,%d]", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuf)));
			}

			tmp_reg = tstack_newreg (ts->stack);
			/* Areg has the index for the jump, in a table at label 'lab' */
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, lab, ARG_REG, tmp_reg));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGINDSIB, 4, ts->stack->old_a_reg, tmp_reg, ARG_REG, tmp_reg));
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, tmp_reg));
		}
		break;
		/*}}}*/
		/*{{{  I_NLABADDR -- NOCC constant label address (used in jump-tables)*/
	case I_NLABADDR:
		{
			int lab = etc_code->fn - I_OPR;

			ts->stack->old_a_reg = ts->stack->a_reg;
			ts->stack->old_b_reg = ts->stack->b_reg;
			ts->stack->old_c_reg = ts->stack->c_reg;
			tstack_setsec (ts->stack, I_NLABADDR, arch);

			if (options.annotate_output) {
				sprintf (sbuf, ".labaddr %d", lab);
				sprintf (sbuf + strlen (sbuf), " [tsd=%d,%d]", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuf)));
			}
			add_to_ins_chain (compose_ins (INS_CONSTLABADDR, 1, 0, ARG_LABEL, lab));
		}
		break;
		/*}}}*/
		/*{{{  I_NJCSUB0 -- NOCC jump if CSUB0 condition (jump if Breg is outside of [0..(Areg-1)])*/
	case I_NJCSUB0:
		{
			int lab = etc_code->fn - I_OPR;

			ts->stack->old_a_reg = ts->stack->a_reg;
			ts->stack->old_b_reg = ts->stack->b_reg;
			ts->stack->old_c_reg = ts->stack->c_reg;
			deferred_cond (ts);
			tstack_setsec (ts->stack, I_NJCSUB0, arch);

			if (options.annotate_output) {
				sprintf (sbuf, "JCSUB0 %d", lab);
				sprintf (sbuf + strlen (sbuf), " [tsd=%d,%d]", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuf)));
			}

			add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
			add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_BE, ARG_LABEL, lab));
		}
		break;
		/*}}}*/
		/*{{{  I_NSTARTP -- NOCC STARTP instruction, value in Areg is absolute*/
	case I_NSTARTP:
		{
			ts->stack->old_a_reg = ts->stack->a_reg;
			ts->stack->old_b_reg = ts->stack->b_reg;
			ts->stack->old_c_reg = ts->stack->c_reg;
			deferred_cond (ts);
			tstack_setsec (ts->stack, I_NSTARTP, arch);

			if (options.annotate_output) {
				sprintf (sbuf, "NSTARTP");
				sprintf (sbuf + strlen (sbuf), " [tsd=%d,%d]", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuf)));
			}

			add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_FLABEL | ARG_ISCONST, 0, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
			constmap_remove (ts->stack->old_b_reg);

			if (options.kernel_interface & KRNLIFACE_MP) {
				arch->compose_kcall (ts, K_STARTP, 2, 0);
			} else if ((options.inline_options & INLINE_SCHEDULER) && arch->compose_inline_startp) {
				arch->compose_inline_startp (ts);
			} else {
				arch->compose_kcall (ts, K_STARTP, 2, 0);
			}
			add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));

		}
		break;
		/*}}}*/
		/*{{{  I_NNEG -- invert value in Areg*/
	case I_NNEG:
		{
			int tmpreg = tstack_newreg (ts->stack);

			ts->stack->old_a_reg = ts->stack->a_reg;
			ts->stack->old_b_reg = ts->stack->b_reg;
			ts->stack->old_c_reg = ts->stack->c_reg;
			deferred_cond (ts);
			tstack_setsec (ts->stack, I_NNEG, arch);

			if (options.annotate_output) {
				sprintf (sbuf, "NNEG");
				sprintf (sbuf + strlen (sbuf), " [tsd=%d,%d]", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuf)));
			}

			add_to_ins_chain (compose_ins (INS_XOR, 2, 1, ARG_REG, tmpreg, ARG_REG, tmpreg, ARG_REG, tmpreg));
			add_to_ins_chain (compose_ins (INS_SUB, 2, 2, ARG_REG, ts->stack->old_a_reg, ARG_REG, tmpreg, ARG_REG, tmpreg, ARG_REG | ARG_IMP, REG_CC));
			ts->stack->a_reg = tmpreg;

			ts->stack->must_set_cmp_flags = 0;
			generate_overflow_code (ts, PMOP_SUB, arch);
		}
		break;
		/*}}}*/
		/*{{{  I_NLW -- load word at address in Areg into Areg'*/
	case I_NLW:
		{
			ts->stack->old_a_reg = ts->stack->a_reg;
			ts->stack->old_b_reg = ts->stack->b_reg;
			ts->stack->old_c_reg = ts->stack->c_reg;
			deferred_cond (ts);
			tstack_setsec (ts->stack, I_NLW, arch);

			if (options.annotate_output) {
				sprintf (sbuf, "NLW");
				sprintf (sbuf + strlen (sbuf), " [tsd=%d,%d]", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuf)));
			}

			if (ts->magic_pending & TS_MAGIC_IOSPACE) {
				int tmp_reg;

#if 0
fprintf (stderr, "MAGIC IOSPACE! (load-word) [%d] --> %d\n", ts->stack->old_a_reg, ts->stack->a_reg);
#endif
				ts->magic_pending &= ~TS_MAGIC_IOSPACE;
				tmp_reg = arch->compose_iospace_loadword (ts, ts->stack->old_a_reg, ts->stack->a_reg);
				ts->stack->a_reg = tmp_reg;
				/* zero-extend word result */
				add_to_ins_chain (compose_ins (INS_MOVEZEXT16TO32, 1, 1, ARG_REG | ARG_IS16BIT, ts->stack->a_reg, ARG_REG, ts->stack->a_reg));
			} else {
				add_to_ins_chain (compose_ins (INS_MOVEZEXT16TO32, 1, 1, ARG_REGIND, ts->stack->old_a_reg, ARG_REG, ts->stack->a_reg));
			}
			constmap_remove (ts->stack->a_reg);
			ts->stack->must_set_cmp_flags = 1;
		}
		break;
		/*}}}*/
		/*{{{  I_NSW -- store word in Breg at address in Areg*/
	case I_NSW:
		{
			ts->stack->old_a_reg = ts->stack->a_reg;
			ts->stack->old_b_reg = ts->stack->b_reg;
			ts->stack->old_c_reg = ts->stack->c_reg;
			deferred_cond (ts);
			tstack_setsec (ts->stack, I_NSW, arch);

			if (options.annotate_output) {
				sprintf (sbuf, "NSW");
				sprintf (sbuf + strlen (sbuf), " [tsd=%d,%d]", ts->stack->old_ts_depth, ts->stack->old_fs_depth);
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuf)));
			}

			if (ts->magic_pending & TS_MAGIC_IOSPACE) {
#if 0
fprintf (stderr, "MAGIC IOSPACE! (store-word) %d --> [%d]\n", ts->stack->old_b_reg, ts->stack->old_a_reg);
#endif
				ts->magic_pending &= ~TS_MAGIC_IOSPACE;
				arch->compose_iospace_storeword (ts, ts->stack->old_a_reg, ts->stack->old_b_reg);
			} else {
				add_to_ins_chain (compose_ins (INS_MOVEW, 1, 1, ARG_REG, ts->stack->old_b_reg, ARG_REGIND, ts->stack->old_a_reg));
			}
			ts->stack->must_set_cmp_flags = 1;
		}
		break;
		/*}}}*/
		/*{{{  I_NALTEND -- NOCC ALTEND instruction, saved pointer is absolute*/
	case I_NALTEND:
		{
			int tmp_reg;
			/*
			 *	move	0(Wptr), %tmpreg
			 *	jmp	*%tmpreg
			 */
			if (options.annotate_output) {
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("NALTEND")));
			}

			ts->stack->old_a_reg = ts->stack->a_reg;
			ts->stack->old_b_reg = ts->stack->b_reg;
			ts->stack->old_c_reg = ts->stack->c_reg;
			deferred_cond (ts);
			tstack_setsec (ts->stack, I_NALTEND, arch);

			tmp_reg = tstack_newreg (ts->stack);
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, W_TEMP, ARG_REG, tmp_reg));
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, tmp_reg));
		}
		break;
		/*}}}*/
		/*{{{  I_NMWENB -- NOCC multiway sync enable*/
	case I_NMWENB:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("NMWENB")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_NMWENB, arch);

		arch->compose_kcall (ts, K_MWENB, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_NMWDIS -- NOCC multiway sync disable*/
	case I_NMWDIS:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("NMWDIS")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_NMWDIS, arch);

		arch->compose_kcall (ts, K_MWDIS, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_NMWALTWT -- NOCC multiway sync ALT wait*/
	case I_NMWALTWT:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("NMWALTWT")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_NMWALTWT, arch);

		arch->compose_deadlock_kcall (ts, K_MWALTWT, 0, 0);
		break;
		/*}}}*/
		/*{{{  I_NMWALT -- NOCC multiway sync ALT start*/
	case I_NMWALT:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("NMWALT")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_NMWALT, arch);

		arch->compose_kcall (ts, K_MWALT, 0, 0);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, Z_ENABLING, ARG_REGIND | ARG_DISP, REG_WPTR, W_STATUS));
		break;
		/*}}}*/
		/*{{{  I_NWALTEND -- NOCC multiway sync ALT end*/
	case I_NMWALTEND:
		{
			int tmp_reg;
			/*
			 *	move	0(Wptr), %tmpreg
			 *	jmp	*%tmpreg
			 */
			if (options.annotate_output) {
				add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("NALTEND")));
			}

			ts->stack->old_a_reg = ts->stack->a_reg;
			ts->stack->old_b_reg = ts->stack->b_reg;
			ts->stack->old_c_reg = ts->stack->c_reg;
			deferred_cond (ts);
			tstack_setsec (ts->stack, I_NMWALTEND, arch);

			arch->compose_kcall (ts, K_MWALTEND, 0, 0);

			tmp_reg = tstack_newreg (ts->stack);
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, W_TEMP, ARG_REG, tmp_reg));
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, tmp_reg));
		}
		break;
		/*}}}*/
		/*{{{  I_MWS_BINIT -- NOCC new multi-way sync BARRIER init*/
	case I_MWS_BINIT:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_BINIT")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_BINIT, arch);

		arch->compose_kcall (ts, K_MWS_BINIT, 1, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_PBRILNK -- NOCC new multi-way sync init PAR-BARRIER and link*/
	case I_MWS_PBRILNK:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_PBRILNK")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_PBRILNK, arch);

		arch->compose_kcall (ts, K_MWS_PBRILNK, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_PBRULNK -- NOCC new multi-way sync PAR-BARRIER unlink*/
	case I_MWS_PBRULNK:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_PBRULNK")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_PBRULNK, arch);

		arch->compose_kcall (ts, K_MWS_PBRULNK, 1, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_PPILNK -- NOCC new multi-way sync init PROC-BARRIER and link*/
	case I_MWS_PPILNK:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_PPILNK")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_PPILNK, arch);

		arch->compose_kcall (ts, K_MWS_PPILNK, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_PBENROLL -- NOCC new multi-way sync PAR-BARRIER enroll*/
	case I_MWS_PBENROLL:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_PBENROLL")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_PBENROLL, arch);

		arch->compose_kcall (ts, K_MWS_PBENROLL, 3, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_PBRESIGN -- NOCC new multi-way sync PAR-BARRIER resign*/
	case I_MWS_PBRESIGN:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_PBRESIGN")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_PBRESIGN, arch);

		arch->compose_kcall (ts, K_MWS_PBRESIGN, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_PBADJSYNC -- NOCC new multi-way sync PAR-BARRIER adjust synch*/
	case I_MWS_PBADJSYNC:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_PBADJSYNC")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_PBADJSYNC, arch);

		arch->compose_kcall (ts, K_MWS_PBADJSYNC, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_SYNC -- NOCC new multi-way sync SYNC*/
	case I_MWS_SYNC:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_SYNC")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_SYNC, arch);

		arch->compose_deadlock_kcall (ts, K_MWS_SYNC, 1, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_ALTLOCK -- NOCC new multi-way sync ALT lock*/
	case I_MWS_ALTLOCK:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_ALTLOCK")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_ALTLOCK, arch);

		arch->compose_kcall (ts, K_MWS_ALTLOCK, 0, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_ALTUNLOCK -- NOCC new multi-way sync ALT unlock*/
	case I_MWS_ALTUNLOCK:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_ALTUNLOCK")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_ALTUNLOCK, arch);

		arch->compose_kcall (ts, K_MWS_ALTUNLOCK, 0, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_ALT -- NOCC new multi-way sync ALT start*/
	case I_MWS_ALT:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_ALT")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_ALT, arch);

		arch->compose_kcall (ts, K_MWS_ALT, 0, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_ALTEND -- NOCC new multi-way sync ALT end*/
	case I_MWS_ALTEND:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_ALTEND")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_ALTEND, arch);

		arch->compose_kcall (ts, K_MWS_ALTEND, 0, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_ENB -- NOCC new multi-way sync ALT enable*/
	case I_MWS_ENB:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_ENB")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_ENB, arch);

		arch->compose_kcall (ts, K_MWS_ENB, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_DIS -- NOCC new multi-way sync ALT disable*/
	case I_MWS_DIS:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_DIS")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_DIS, arch);

		arch->compose_kcall (ts, K_MWS_DIS, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_ALTPOSTLOCK -- NOCC new multi-way sync lock-after-ALT*/
	case I_MWS_ALTPOSTLOCK:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_ALTPOSTLOCK")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_ALTPOSTLOCK, arch);

		arch->compose_kcall (ts, K_MWS_ALTPOSTLOCK, 0, 0);
		break;
		/*}}}*/
		/*{{{  I_MWS_PPBASEOF -- NOCC new multi-way sync barrier-base from proc-barrier*/
	case I_MWS_PPBASEOF:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_PPBASEOF")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_PPBASEOF, arch);

		arch->compose_kcall (ts, K_MWS_PPBASEOF, 1, 1);
		/*{{{  copy out result into different register (hacky..)*/
		{
			int tmpreg = tstack_newreg (ts->stack);

			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->a_reg, ARG_REG, tmpreg));
			ts->stack->a_reg = tmpreg;
		}
		/*}}}*/
		break;
		/*}}}*/
		/*{{{  I_MWS_PPPAROF -- NOCC new multi-way sync par-barrier from proc-barrier*/
	case I_MWS_PPPAROF:
		if (options.annotate_output) {
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("MWS_PPPAROF")));
		}

		ts->stack->old_a_reg = ts->stack->a_reg;
		ts->stack->old_b_reg = ts->stack->b_reg;
		ts->stack->old_c_reg = ts->stack->c_reg;
		deferred_cond (ts);
		tstack_setsec (ts->stack, I_MWS_PPPAROF, arch);

		arch->compose_kcall (ts, K_MWS_PPPAROF, 1, 1);
		/*{{{  copy out result into different register (hacky..)*/
		{
			int tmpreg = tstack_newreg (ts->stack);

			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->a_reg, ARG_REG, tmpreg));
			ts->stack->a_reg = tmpreg;
		}
		/*}}}*/
		break;
		/*}}}*/
	default:
		fprintf (stderr, "%s: error: unhandled %d (%d) in NOCC code special!\n", progname, etc_code->opd, etc_code->fn - I_OPR);
		exit (EXIT_FAILURE);
		break;
	}

	*ecodeptr = etc_code;
	return;
}
/*}}}*/
/*{{{  static void do_code_secondary (tstate *ts, int sec, arch_t *arch)*/
/*
 *	translates a secondary instruction
 */
static void do_code_secondary (tstate *ts, int sec, arch_t *arch)
{
	int this_lab, this_lab2, this_lab3;
	int tmp_reg;
	int c_val;
	ins_chain *tmp_ins;

	ts->stack->old_a_reg = ts->stack->a_reg;
	ts->stack->old_b_reg = ts->stack->b_reg;
	ts->stack->old_c_reg = ts->stack->c_reg;
	deferred_cond (ts);
	tstack_setsec (ts->stack, sec, arch);
	switch (sec) {
		/*{{{  I_XSTL, I_XSTLN -- store and load local (not expected here)*/
	case I_XSTL:
	case I_XSTLN:
		fprintf (stderr, "%s: error: unexpected XSTL/XSTLN in secondary.\n", progname);
		exit (EXIT_FAILURE);
		break;
		/*}}}*/
		/*{{{  I_XWORD, I_UNPACKSN, I_ROUNDSN, I_POSTNORMSN -- unsupported instructions*/
	case I_XWORD:
	case I_UNPACKSN:
	case I_ROUNDSN:
	case I_POSTNORMSN:
		fprintf (stderr, "%s: warning: not translatable %d\n", progname, sec);
		break;
		/*}}}*/
		/*{{{  I_REV -- reverse top-two stack entries*/
	case I_REV:
		ts->stack->a_reg = ts->stack->old_b_reg;
		ts->stack->b_reg = ts->stack->old_a_reg;
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_POP -- pop a result from the stack*/
	case I_POP:
		ts->stack->a_reg = ts->stack->old_b_reg;
		ts->stack->b_reg = ts->stack->old_c_reg;
		ts->stack->c_reg = REG_UNDEFINED;
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_RET -- return from PROC or FUNCTION*/
	case I_RET:
		arch->compose_return (ts);
		ts->stack->must_set_cmp_flags = 1;
		constmap_clearall ();
		break;
		/*}}}*/
		/*{{{  I_LDPI -- load code address*/
	case I_LDPI:
		this_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_LABEL | ARG_ISCONST, this_lab, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_GAJW -- general adjust workspace pointer*/
	case I_GAJW:
		add_to_ins_chain (compose_ins (INS_SWAP, 2, 2, ARG_REG, ts->stack->a_reg, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR, ARG_REG, ts->stack->a_reg));
		constmap_clearall ();
		break;
		/*}}}*/
		/*{{{  I_GCALL -- general call*/
	case I_GCALL:
		if (constmap_typeof (ts->stack->a_reg) == VALUE_LABADDR) {
			int origlab = constmap_regconst (ts->stack->a_reg);		/* label whose address we loaded */

			if (label_is_stub (origlab)) {
				/* inline stub code */
				add_chain_to_ins_chain (rtl_copy_code (get_stubcode (ts, origlab)));
			} else {
				add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_ISCONST | ARG_FLABEL, 0, ARG_REGIND, REG_WPTR));
				add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, ts->stack->a_reg));
				add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
			}
		} else {
			/* modified to handle new calling convention, since I'm about to use this instruction.. */
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_ISCONST | ARG_FLABEL, 0, ARG_REGIND, REG_WPTR));
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, ts->stack->a_reg));
			add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
		}

		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_MINT -- generate MOSTNEG INT*/
	case I_MINT:
		tmp_ins = compose_ins (INS_MOVE, 1, 1, ARG_CONST, (signed int)0x80000000, ARG_REG, ts->stack->a_reg);
		constmap_new (ts->stack->a_reg, VALUE_CONST, (signed int)0x80000000, tmp_ins);
		add_to_ins_chain (tmp_ins);
		break;
		/*}}}*/
		/*{{{  I_NULL -- generate NULL (zero)*/
	case I_NULL:
		tmp_ins = compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, ts->stack->a_reg);
		constmap_new (ts->stack->a_reg, VALUE_CONST, 0, tmp_ins);
		add_to_ins_chain (tmp_ins);
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_MNEW -- allocate memory by pool index*/
	case I_MNEW:
		if (!options.disable_dynmem) {
			arch->compose_kcall (ts, K_MNEW, 1, 1);
		} else {
			fprintf (stderr, "%s: (%s:%d) error: dynamic memory support is disabled.\n", progname,
				(ts->file_pending >= 0) ? ts->file_list[ts->file_pending] : "???.occ", ts->line_pending);
			exit (EXIT_FAILURE);
		}
		break;
		/*}}}*/
		/*{{{  I_MFREE -- free memory allocated by MNEW*/
	case I_MFREE:
		if (!options.disable_dynmem) {
			arch->compose_kcall (ts, K_MFREE, 1, 0);
		} else {
			fprintf (stderr, "%s: (%s:%d) error: dynamic memory support is disabled.\n", progname,
				(ts->file_pending >= 0) ? ts->file_list[ts->file_pending] : "???.occ", ts->line_pending);
			exit (EXIT_FAILURE);
		}
		break;
		/*}}}*/
		/*{{{  I_MALLOC -- allocate memory by size*/
	case I_MALLOC:
		if (!options.disable_dynmem) {
			if (options.inline_options & INLINE_SCHEDULER) {
				arch->compose_kcall (ts, K_MALLOC, 1, 1);
				/* arch->compose_inline_malloc (ts); */
			} else {
				arch->compose_kcall (ts, K_MALLOC, 1, 1);
			}
		} else {
			fprintf (stderr, "%s: (%s:%d) error: dynamic memory support is disabled.\n", progname,
				(ts->file_pending >= 0) ? ts->file_list[ts->file_pending] : "???.occ", ts->line_pending);
			exit (EXIT_FAILURE);
		}
		break;
		/*}}}*/
		/*{{{  I_MRELEASE -- free memory allocated by MALLOC*/
	case I_MRELEASE:
		if (options.debug_options & DEBUG_INSERT) {
			arch->compose_debug_insert (ts, 1);
		}
		if (!options.disable_dynmem) {
			arch->compose_kcall (ts, K_MRELEASE, 1, 0);
		} else {
			fprintf (stderr, "%s: (%s:%d) error: dynamic memory support is disabled.\n", progname,
				(ts->file_pending >= 0) ? ts->file_list[ts->file_pending] : "???.occ", ts->line_pending);
			exit (EXIT_FAILURE);
		}
		break;
		/*}}}*/
		/*{{{  I_MRELEASEP -- free MALLOC'd workspace and terminate process*/
	case I_MRELEASEP:
		if (options.debug_options & DEBUG_INSERT) {
			arch->compose_debug_insert (ts, 1);
		}
		if (!options.disable_dynmem) {
			arch->compose_kcall (ts, K_MRELEASEP, 1, 0);
		} else {
			fprintf (stderr, "%s: (%s:%d) error: dynamic memory support is disabled.\n", progname,
				(ts->file_pending >= 0) ? ts->file_list[ts->file_pending] : "???.occ", ts->line_pending);
			exit (EXIT_FAILURE);
		}
		break;
		/*}}}*/
		/*{{{  I_LEND -- loop-end (not expected here)*/
	case I_LEND:
		/* not generated -- loop-end comes in ETC special */
		fprintf (stderr, "%s: error: expected LEND instruction in ETC\n", progname);
		break;
		/*}}}*/
		/*{{{  I_CSUB0 -- range check*/
	case I_CSUB0:
		if (!options.disable_checking) {
			translate_csub0 (ts, arch);
		}
		break;
		/*}}}*/
		/*{{{  I_CCNT1 -- range check*/
	case I_CCNT1:
		if (!options.disable_checking) {
			translate_range_check (ts, 1, arch, REOP_CCNT1);
		}
		break;
		/*}}}*/
		/*{{{  I_SETERR -- set error flag (halt program)*/
	case I_SETERR:
		if (arch->compose_debug_seterr && (options.debug_options & DEBUG_RANGESTOP)) {
			arch->compose_debug_seterr (ts);
		} else {
			arch->compose_kcall (ts, K_BNSETERR, 0, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_STOPERR -- generate STOP error*/
	case I_STOPERR:
		generate_overflowed_code (ts, PMOP_STOP, arch);
		break;
		/*}}}*/
		/*{{{  I_TESTERR -- test error flag*/
	case I_TESTERR:
		add_to_ins_chain (compose_ins (INS_SETCC, 1, 1, ARG_COND, CC_NO, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
		add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_CONST, 1, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg));
		break;
		/*}}}*/
		/*{{{  I_BSUB -- byte index*/
	case I_BSUB:
		switch (constmap_typeof (ts->stack->old_a_reg)) {
		case VALUE_LABADDR:
			add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_LABEL | ARG_ISCONST, constmap_regconst (ts->stack->old_a_reg), ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->a_reg));
			break;
		default:
			/* areg' = areg, breg' = creg, creg' = undefined */
			ts->stack->a_reg = ts->stack->old_a_reg;
			switch (constmap_typeof (ts->stack->old_b_reg)) {
			case VALUE_CONST:
				add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, constmap_regconst (ts->stack->old_b_reg), ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->a_reg));
				break;
			case VALUE_LABADDR:
				add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_LABEL | ARG_ISCONST, constmap_regconst (ts->stack->old_b_reg), ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->a_reg));
				break;
			default:
				add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->a_reg));
				break;
			}
			break;
		}
		constmap_remove (ts->stack->a_reg);
		break;
		/*}}}*/
		/*{{{  I_WSUB -- word index*/
	case I_WSUB:
#if 1
		switch (constmap_typeof (ts->stack->old_a_reg)) {	/* base expression */
		case VALUE_LOCAL:
			add_to_ins_chain (compose_ins (INS_SHL, 2, 1, ARG_CONST, WShift, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
			ts->stack->a_reg = ts->stack->old_b_reg;
			add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH,
						ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->a_reg));
			break;
		default:
			/* areg' = areg, breg' = creg, creg' = undefined */
			ts->stack->a_reg = ts->stack->old_a_reg;
			add_to_ins_chain (compose_ins (INS_SHL, 2, 1, ARG_CONST, WShift, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
			add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->a_reg));
			break;
		}
		constmap_remove (ts->stack->a_reg);
#else
		ts->stack->a_reg = ts->stack->old_a_reg;
		add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_REGINDSIB, 4, ts->stack->old_b_reg, ts->stack->old_a_reg, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
#endif
		break;
		/*}}}*/
		/*{{{  I_WSUBDB -- doubleword index*/
	case I_WSUBDB:
		/* areg' = areg, breg' = creg, creg' = undefined */
		ts->stack->a_reg = ts->stack->old_a_reg;
		add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_REGINDSIB, 8, ts->stack->old_b_reg, ts->stack->old_a_reg, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
		break;
		/*}}}*/
		/*{{{  I_BCNT -- bytes in words*/
	case I_BCNT:
		add_to_ins_chain (compose_ins (INS_SHL, 2, 1, ARG_CONST, WSH, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
		break;
		/*}}}*/
		/*{{{  I_WCNT -- words in bytes, and remainder*/
	case I_WCNT:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_SHR, 2, 1, ARG_CONST, WSH, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_CONST, 3, ARG_REG, ts->stack->b_reg, ARG_REG, ts->stack->b_reg));
		constmap_remove (ts->stack->a_reg);
		constmap_remove (ts->stack->b_reg);
		ts->stack->must_set_cmp_flags = 0;
		break;
		/*}}}*/
		/*{{{  I_IOR, I_IOW -- arbitrary width I/O read and write (unsuppoted)*/
	case I_IOR:
	case I_IOW:
		fprintf (stderr, "%s: warning: unsupported IOR/IOW\n", progname);
		break;
		/*}}}*/
		/*{{{  I_IOR8 -- 8-bit I/O read*/
	case I_IOR8:
		if (arch->compose_iospace_read) {
			arch->compose_iospace_read (ts, ts->stack->old_a_reg, ts->stack->old_b_reg, 8);
		} else {
			fprintf (stderr, "%s: warning: unsupported IOR8\n", progname);
		}
		break;
		/*}}}*/
		/*{{{  I_IOW8 -- 8-bit I/O write*/
	case I_IOW8:
		if (arch->compose_iospace_write) {
			arch->compose_iospace_write (ts, ts->stack->old_a_reg, ts->stack->old_b_reg, 8);
		} else {
			fprintf (stderr, "%s: warning: unsupported IOW8\n", progname);
		}
		break;
		/*}}}*/
		/*{{{  I_IOR16 -- 16-bit I/O read*/
	case I_IOR16:
		if (arch->compose_iospace_read) {
			arch->compose_iospace_read (ts, ts->stack->old_a_reg, ts->stack->old_b_reg, 16);
		} else {
			fprintf (stderr, "%s: warning: unsupported IOR16\n", progname);
		}
		break;
		/*}}}*/
		/*{{{  I_IOW16 -- 16-bit I/O write*/
	case I_IOW16:
		if (arch->compose_iospace_write) {
			arch->compose_iospace_write (ts, ts->stack->old_a_reg, ts->stack->old_b_reg, 16);
		} else {
			fprintf (stderr, "%s: warning: unsupported IOW16\n", progname);
		}
		break;
		/*}}}*/
		/*{{{  I_IOR32 -- 32-bit I/O read*/
	case I_IOR32:
		if (arch->compose_iospace_read) {
			arch->compose_iospace_read (ts, ts->stack->old_a_reg, ts->stack->old_b_reg, 32);
		} else {
			fprintf (stderr, "%s: warning: unsupported IOR32\n", progname);
		}
		break;
		/*}}}*/
		/*{{{  I_IOW32 -- 32-bit I/O write*/
	case I_IOW32:
		if (arch->compose_iospace_write) {
			arch->compose_iospace_write (ts, ts->stack->old_a_reg, ts->stack->old_b_reg, 32);
		} else {
			fprintf (stderr, "%s: warning: unsupported IOW32\n", progname);
		}
		break;
		/*}}}*/
		/*{{{  I_LB -- load byte*/
	case I_LB:
		/* add_to_ins_chain (compose_ins (INS_MOVEB, 1, 1, ARG_REGIND, ts->stack->a_reg, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_CONST, 0xff, ARG_REG, ts->stack->a_reg, ARG_REG, ts->stack->a_reg)); */
		if (ts->magic_pending & TS_MAGIC_IOSPACE) {
#if 0
fprintf (stderr, "MAGIC IOSPACE! (load-byte) [%d] --> %d\n", ts->stack->old_a_reg, ts->stack->a_reg);
#endif
			ts->magic_pending &= ~TS_MAGIC_IOSPACE;
			tmp_reg = arch->compose_iospace_loadbyte (ts, ts->stack->old_a_reg, ts->stack->a_reg);
			ts->stack->a_reg = tmp_reg;
			/* zero-extend BYTE result */
			add_to_ins_chain (compose_ins (INS_MOVEZEXT8TO32, 1, 1, ARG_REG | ARG_IS8BIT, ts->stack->a_reg, ARG_REG, ts->stack->a_reg));
		} else {
			add_to_ins_chain (compose_ins (INS_MOVEZEXT8TO32, 1, 1, ARG_REGIND, ts->stack->old_a_reg, ARG_REG, ts->stack->a_reg));
		}
		constmap_remove (ts->stack->a_reg);
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_SB -- store byte*/
	case I_SB:
		if (ts->magic_pending & TS_MAGIC_IOSPACE) {
#if 0
fprintf (stderr, "MAGIC IOSPACE! (store-byte) %d --> [%d]\n", ts->stack->old_b_reg, ts->stack->old_a_reg);
#endif
			ts->magic_pending &= ~TS_MAGIC_IOSPACE;
			arch->compose_iospace_storebyte (ts, ts->stack->old_a_reg, ts->stack->old_b_reg);
		} else {
			add_to_ins_chain (compose_ins (INS_MOVEB, 1, 1, ARG_REG, ts->stack->old_b_reg, ARG_REGIND, ts->stack->old_a_reg));
		}
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_AND -- bitwise and*/
	case I_AND:
		generate_constmapped_21instr (ts, EtcSecondary (I_AND), INS_AND, ts->stack->old_a_reg, ts->stack->old_b_reg, ts->stack->a_reg, 0);
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_OR -- bitwise or*/
	case I_OR:
		generate_constmapped_21instr (ts, EtcSecondary (I_OR), INS_OR, ts->stack->old_a_reg, ts->stack->old_b_reg, ts->stack->a_reg, 0);
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_XOR -- bitwise exclusive or*/
	case I_XOR:
		generate_constmapped_21instr (ts, EtcSecondary (I_XOR), INS_XOR, ts->stack->old_a_reg, ts->stack->old_b_reg, ts->stack->a_reg, 0);
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_ADD -- addition*/
	case I_ADD:
		generate_constmapped_21instr (ts, EtcSecondary (I_ADD), INS_ADD, ts->stack->old_a_reg, ts->stack->old_b_reg, ts->stack->a_reg, 1);
		ts->stack->must_set_cmp_flags = 0;
		generate_overflow_code (ts, PMOP_ADD, arch);
		break;
		/*}}}*/
		/*{{{  I_SUB -- subtraction*/
	case I_SUB:
		generate_constmapped_21instr (ts, EtcSecondary (I_SUB), INS_SUB, ts->stack->old_a_reg, ts->stack->old_b_reg, ts->stack->a_reg, 1);
		ts->stack->must_set_cmp_flags = 0;
		generate_overflow_code (ts, PMOP_SUB, arch);
		break;
		/*}}}*/
		/*{{{  I_MUL -- multiply*/
	case I_MUL:
		generate_constmapped_21instr (ts, EtcSecondary (I_MUL), INS_MUL, ts->stack->old_a_reg, ts->stack->old_b_reg, ts->stack->a_reg, 1);
		ts->stack->must_set_cmp_flags = 0;
		generate_overflow_code (ts, PMOP_MUL, arch);
		break;
		/*}}}*/
		/*{{{  I_DIV -- divide*/
	case I_DIV:
		{
			int skip_codegen = 0;

			if (options.debug_options & DEBUG_OVERFLOW) {
				arch->compose_divcheck_zero (ts, ts->stack->old_a_reg);
			} else if ((constmap_typeof (ts->stack->old_a_reg) == VALUE_CONST) && (constmap_regconst (ts->stack->old_a_reg) == 0)) {
				fprintf (stderr, "%s: serious: division by zero seen around line %d in %s\n", progname, ts->line_pending, ts->file_list[ts->file_pending]);
				arch->compose_kcall (ts, K_BNSETERR, 0, 0);
				skip_codegen = 1;
			} else if (!options.disable_checking) {
				arch->compose_divcheck_zero_simple (ts, ts->stack->old_a_reg);
			}
			if (!skip_codegen) {
				int pot;

				/* if the divisor is a constant power-of-two.. */
				if (0 && (constmap_typeof (ts->stack->old_a_reg) == VALUE_CONST) && ((pot = is_power_of_two (constmap_regconst (ts->stack->old_a_reg))) != 0)) {
					add_to_ins_chain (compose_ins (INS_SHR, 2, 1, ARG_CONST | ARG_ISCONST, pot, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
				} else {
					arch->compose_division (ts, ts->stack->old_b_reg, ts->stack->old_a_reg, ts->stack->old_b_reg);
				}
				if (ts->stack->ts_depth <= 1) {
					ts->stack->b_reg = REG_UNDEFINED;
				} else {
					constmap_remove (ts->stack->b_reg);
				}
			} else {
				constmap_clearall ();
			}
		}
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_REM -- remainder*/
	case I_REM:
		/* this isn't particularly pleasant... */
		{
			int skip_codegen = 0;

			if (options.debug_options & DEBUG_OVERFLOW) {
				arch->compose_divcheck_zero (ts, ts->stack->old_a_reg);
			} else if ((constmap_typeof (ts->stack->old_a_reg) == VALUE_CONST) && (constmap_regconst (ts->stack->old_a_reg) == 0)) {
				fprintf (stderr, "%s: serious: remainder by zero seen around line %d in %s\n", progname, ts->line_pending, ts->file_list[ts->file_pending]);
				arch->compose_kcall (ts, K_BNSETERR, 0, 0);
				skip_codegen = 1;
			} else if (!options.disable_checking) {
				arch->compose_divcheck_zero_simple (ts, ts->stack->old_a_reg);
			}
			if (!skip_codegen) {
				tmp_reg = arch->compose_remainder (ts, ts->stack->old_b_reg, ts->stack->old_a_reg);
				ts->stack->a_reg = tmp_reg;
				if (ts->stack->ts_depth <= 1) {
					ts->stack->b_reg = REG_UNDEFINED;
				} else {
					constmap_remove (ts->stack->b_reg);
				}
			} else {
				constmap_clearall ();
			}
		}
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_SUM -- addition (without overflow checking)*/
	case I_SUM:
		if ((constmap_typeof (ts->stack->old_a_reg) == VALUE_CONST) && (constmap_regconst (ts->stack->old_a_reg) == 1)) {
			add_to_ins_chain (compose_ins (INS_INC, 1, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->a_reg));
			constmap_remove (ts->stack->a_reg);
		} else {
			generate_constmapped_21instr (ts, EtcSecondary (I_SUM), INS_ADD, ts->stack->old_a_reg, ts->stack->old_b_reg, ts->stack->a_reg, 0);
		}
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_DIFF -- subtraction (without overflow checking)*/
	case I_DIFF:
		generate_constmapped_21instr (ts, EtcSecondary (I_DIFF), INS_SUB, ts->stack->old_a_reg, ts->stack->old_b_reg, ts->stack->a_reg, 0);
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_PROD -- multiplication (without overflow checking)*/
	case I_PROD:
		switch (constmap_typeof (ts->stack->old_a_reg)) {
		default:
			add_to_ins_chain (compose_ins_ex (EtcSecondary (I_PROD), INS_MUL, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->a_reg));
			constmap_remove (ts->stack->a_reg);
			break;
		case VALUE_CONST:
			if (constmap_regconst (ts->stack->old_a_reg) != 1) {
				add_to_ins_chain (compose_ins_ex (EtcSecondary (I_PROD), INS_MUL, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->a_reg));
				constmap_remove (ts->stack->a_reg);
			}
			break;
		}
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_NOT -- bitwise not*/
	case I_NOT:
		add_to_ins_chain (compose_ins (INS_NOT, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg));
		constmap_remove (ts->stack->a_reg);
		ts->stack->must_set_cmp_flags = 0;
		break;
		/*}}}*/
		/*{{{  I_SHL, I_SHR -- shifts*/
	case I_SHL:
	case I_SHR:
		/*
		 *	cmp	$32, %oldareg
		 *	jb	L0
		 *	jz	L1
		 */
		this_lab = ++(ts->last_lab);
		this_lab2 = ++(ts->last_lab);
		this_lab3 = ++(ts->last_lab);
		c_val = -1;
		switch (constmap_typeof (ts->stack->old_a_reg)) {
		default:
			/* do comparison */
			add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 32, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
			add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_A, ARG_LABEL, this_lab));
			add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_LABEL, this_lab2));
			/* code shifted to architecture-dependant handling for actual shift */
			arch->compose_shift (ts, sec, ts->stack->old_a_reg, ts->stack->a_reg, ts->stack->a_reg);
			break;
		case VALUE_CONST:
			/* already know the result */
			c_val = constmap_regconst (ts->stack->old_a_reg);
			if (c_val > 32) {
				fprintf (stderr, "%s: serious: bit-shift (%d) out of range around line %d in %s\n", progname, c_val, ts->line_pending, ts->file_list[ts->file_pending]);
			}
			break;
		}
		switch (constmap_typeof (ts->stack->old_a_reg)) {
		default:
			/* cleanup code */
			/*
			 *	jmp	L2
			 *  L0:
			 *	<error condition>
			 *  L1:
			 *	mov	$0, %areg
			 *	jmp	L2
			 *  L2:
			 */
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, this_lab3));
			add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
			if (options.debug_options & DEBUG_RANGESTOP) {
				generate_range_code (ts, REOP_SHIFT, arch);
			} else {	
				arch->compose_kcall (ts, K_BRANGERR, 0, -1);
			}
			add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab2));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, ts->stack->a_reg));
			add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab3));
			break;
		case VALUE_CONST:
			/* c_val set just before */
			if (c_val > 32) {
				/* insert error */
				if (options.debug_options & DEBUG_RANGESTOP) {
					generate_range_code (ts, REOP_SHIFT, arch);
				} else {	
					arch->compose_kcall (ts, K_BRANGERR, 0, -1);
				}
			} else if (c_val == 32) {
				/* clear to zero (L1) */
				add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, ts->stack->a_reg));
			} else {
				/* insert shift (moved to arch) */
				arch->compose_shift (ts, sec, ts->stack->old_a_reg, ts->stack->a_reg, ts->stack->a_reg);
			}
			break;
		}
		ts->stack->a_reg = ts->stack->old_b_reg;
		ts->stack->b_reg = ts->stack->old_c_reg;
		ts->stack->c_reg = REG_UNDEFINED;
		ts->stack->must_set_cmp_flags = 1;
		constmap_remove (ts->stack->a_reg);
		/*
		 *	obvious optimisation for (==32 case):
		 *		cmovz	$0,%areg
		 */
		break;
		/*}}}*/
		/*{{{  I_GT -- tests for greater-than*/
	case I_GT:
		ts->cond = CC_GT;
		switch (constmap_typeof (ts->stack->old_a_reg)) {
		default:
			switch (constmap_typeof (ts->stack->old_b_reg)) {
			default:
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_CC));
				break;
			case VALUE_LOCAL:
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_b_reg) << WSH, ARG_REG | ARG_IMP, REG_CC));
				break;
			case VALUE_CONST:
				/* invert condition */
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, constmap_regconst (ts->stack->old_b_reg), ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
				ts->cond = CC_LT;
				break;
			}
			break;
		case VALUE_CONST:
			switch (constmap_typeof (ts->stack->old_b_reg)) {
			default:
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, constmap_regconst (ts->stack->old_a_reg), ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_CC));
				break;
			case VALUE_LOCAL:
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, constmap_regconst (ts->stack->old_a_reg),
					ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_b_reg) << WSH, ARG_REG | ARG_IMP, REG_CC));
				break;
			}
			break;
		case VALUE_LOCAL:
			switch (constmap_typeof (ts->stack->old_b_reg)) {
			default:
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH, ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_CC));
				break;
			case VALUE_CONST:
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, constmap_regconst (ts->stack->old_b_reg),
					ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH, ARG_REG | ARG_IMP, REG_CC));
				ts->cond = CC_LT;
			}
			break;
		}
		ts->stack->must_set_cmp_flags = 0;
		break;
		/*}}}*/
		/*{{{  I_XDBLE -- widen INT32 to INT64*/
	case I_XDBLE:
		arch->compose_widenword (ts);
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_CWORD -- range check*/
	case I_CWORD:
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
		add_to_ins_chain (compose_ins (INS_SHL, 2, 1, ARG_CONST, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg));
		translate_range_check (ts, 0, arch, REOP_CWORD);
		ts->stack->a_reg = tmp_reg;
		constmap_remove (ts->stack->a_reg);
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_CSNGL -- range check*/
	case I_CSNGL:
		if (!options.disable_checking) {
			/*
			 *	inc	%oldbreg
			 *	cmp	2,%oldbreg
			 *	jae	L0
			 *	dec	%oldbreg
			 *	xor	%oldareg,%oldbreg
			 *	jns	L1
			 * L0:
			 *	<range-code> | <kcall (BRANGERR)>
			 * L1:
			 */
			add_to_ins_chain (compose_ins_ex (EtcSecondary (I_CSNGL), INS_INC, 1, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
			add_to_ins_chain (compose_ins_ex (EtcSecondary (I_CSNGL), INS_CMP, 2, 1, ARG_CONST, 2, ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_CC));
			ts->last_lab += 2;
			this_lab = ts->last_lab - 1;
			add_to_ins_chain (compose_ins_ex (EtcSecondary (I_CSNGL), INS_CJUMP, 2, 0, ARG_COND, CC_AE, ARG_LABEL, this_lab));
			add_to_ins_chain (compose_ins_ex (EtcSecondary (I_CSNGL), INS_DEC, 1, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
			add_to_ins_chain (compose_ins_ex (EtcSecondary (I_CSNGL), INS_XOR, 2, 2, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_CC));
			add_to_ins_chain (compose_ins_ex (EtcSecondary (I_CSNGL), INS_CJUMP, 2, 0, ARG_COND, CC_NS, ARG_LABEL, this_lab + 1));
			add_to_ins_chain (compose_ins_ex (EtcSecondary (I_CSNGL), INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
			if (options.debug_options & DEBUG_RANGESTOP) {
				generate_range_code (ts, REOP_CSNGL, arch);
			} else {
				arch->compose_kcall (ts, K_BRANGERR, 0, -1);
			}
			add_to_ins_chain (compose_ins_ex (EtcSecondary (I_CSNGL), INS_SETLABEL, 1, 0, ARG_LABEL, this_lab + 1));
		}
		ts->stack->a_reg = ts->stack->old_a_reg;
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_LADD -- long addition*/
	case I_LADD:
		arch->compose_longop (ts, I_LADD);
		ts->stack->must_set_cmp_flags = 0;
		generate_overflow_code (ts, PMOP_LADD, arch);
		break;
		/*}}}*/
		/*{{{  I_LSUB -- long subtraction*/
	case I_LSUB:
		arch->compose_longop (ts, I_LSUB);
		ts->stack->must_set_cmp_flags = 0;
		generate_overflow_code (ts, PMOP_LSUB, arch);
		break;
		/*}}}*/
		/*{{{  I_LSUM -- long addition (without overflow checking)*/
	case I_LSUM:
		arch->compose_longop (ts, I_LSUM);
		ts->stack->must_set_cmp_flags = 0;
		break;
		/*}}}*/
		/*{{{  I_LDIFF -- long subtraction (without overflow checking)*/
	case I_LDIFF:
		arch->compose_longop (ts, I_LDIFF);
		ts->stack->must_set_cmp_flags = 0;
		break;
		/*}}}*/
		/*{{{  I_LMUM -- long multiplication*/
	case I_LMUL:
		arch->compose_longop (ts, I_LMUL);
		ts->stack->must_set_cmp_flags = 0;
		break;
		/*}}}*/
		/*{{{  I_LSHL, I_LSHR -- long shifts*/
	case I_LSHL:
	case I_LSHR:
		arch->compose_longop (ts, sec);
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_MOVE -- block move*/
	case I_MOVE:
		arch->compose_move (ts);
		break;
		/*}}}*/
		/*{{{  I_LDIV -- long division*/
	case I_LDIV:
		if (options.debug_options & DEBUG_OVERFLOW) {
			/* check that divsor (oldAreg) < high-32 bits of dividend (oldCreg) */
			add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
			this_lab = ++(ts->last_lab);
			add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_AE, ARG_LABEL, this_lab));
			arch->compose_overflow_jumpcode (ts, PMOP_LDIV);
			add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		} /* else if the following division overflows, #DE exception is raised */
		arch->compose_longop (ts, I_LDIV);
		ts->stack->must_set_cmp_flags = 0;
		break;
		/*}}}*/
		/*{{{  I_NORM -- normalise*/
	case I_NORM:
		arch->compose_kcall (ts, K_NORM, 2, 3);
		break;
		/*}}}*/
		/*{{{  I_STARTP -- start process*/
	case I_STARTP:
		if (options.kernel_interface & KRNLIFACE_MP) {
			arch->compose_kcall (ts, K_STARTP, 2, 0);
		} else if ((options.inline_options & INLINE_SCHEDULER) && arch->compose_inline_startp) {
			arch->compose_inline_startp (ts);
		} else {
			arch->compose_kcall (ts, K_STARTP, 2, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_ENDP -- end process*/
	case I_ENDP:
		if (options.kernel_interface & KRNLIFACE_MP) {
			arch->compose_kcall (ts, K_ENDP, 1, 0);
		} else if ((options.inline_options & INLINE_SCHEDULER) && arch->compose_inline_endp) {
			arch->compose_inline_endp (ts);
		} else {
			arch->compose_kcall (ts, K_ENDP, 1, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_RUNP -- run process*/
	case I_RUNP:
		if (options.kernel_interface & KRNLIFACE_MP) {
			arch->compose_kcall (ts, K_RUNP, 1, 0);
		} else if ((options.inline_options & INLINE_SCHEDULER) && arch->compose_inline_runp) {
			arch->compose_inline_runp (ts);
		} else {
			arch->compose_kcall (ts, K_RUNP, 1, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_STOPP -- stop process*/
	case I_STOPP:
		if (options.kernel_interface & KRNLIFACE_MP) {
			arch->compose_kcall (ts, K_STOPP, 0, 0);
		} else if ((options.inline_options & INLINE_SCHEDULER) && arch->compose_inline_stopp) {
			arch->compose_inline_stopp (ts);
		} else {
			arch->compose_kcall (ts, K_STOPP, 0, 0);
		}

		if (ts->magic_pending & TS_MAGIC_CODEMAP) {
			/* STOPP can be a legal descheduling point for a mobile process, so save it (scan back for setlabel)*/
			addlastlabtocodemap (ts);
			ts->magic_pending &= ~TS_MAGIC_CODEMAP;
		}
		break;
		/*}}}*/
		/*{{{  I_MIN -- static mobile input*/
	case I_MIN:
		if ((options.inline_options & INLINE_MIN_MOUT) && arch->compose_inline_min) {
			arch->compose_inline_min (ts, 0);
		} else {
			arch->compose_deadlock_kcall (ts, K_MIN, 2, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_MOUT -- static mobile output*/
	case I_MOUT:
		if ((options.inline_options & INLINE_MIN_MOUT) && arch->compose_inline_mout) {
			arch->compose_inline_mout (ts, 0);
		} else {
			arch->compose_deadlock_kcall (ts, K_MOUT, 2, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_MIN64 -- 64-bit mobile input*/
	case I_MIN64:
		if ((options.inline_options & INLINE_MIN_MOUT) && arch->compose_inline_min) {
			arch->compose_inline_min (ts, 1);
		} else {
			arch->compose_deadlock_kcall (ts, K_MIN64, 2, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_MOUT64 -- 64-bit mobile output*/
	case I_MOUT64:
		if ((options.inline_options & INLINE_MIN_MOUT) && arch->compose_inline_mout) {
			arch->compose_inline_mout (ts, 1);
		} else {
			arch->compose_deadlock_kcall (ts, K_MOUT64, 2, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_MINN -- variable size mobile input*/
	case I_MINN:
		arch->compose_deadlock_kcall (ts, K_MINN, 3, 0);
		break;
		/*}}}*/
		/*{{{  I_MOUTN -- variable size mobile output*/
	case I_MOUTN:
		arch->compose_deadlock_kcall (ts, K_MOUTN, 3, 0);
		break;
		/*}}}*/
		/*{{{  I_XMINN -- variable size extended mobile input*/
	case I_XMINN:
		arch->compose_kcall (ts, K_XMINN, 3, 0);
		break;
		/*}}}*/
		/*{{{  I_IN -- channel input*/
	case I_IN:
		if ((options.inline_options & INLINE_IN2) && arch->compose_inline_in_2) {
			arch->compose_inline_in_2 (ts, 0);
		} else if ((options.inline_options & INLINE_IN) && arch->compose_inline_in) {
			arch->compose_inline_in (ts, 0);
		} else {
			arch->compose_deadlock_kcall (ts, K_IN, 3, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_IN8 -- byte input from channel*/
	case I_IN8:
		if ((options.inline_options & INLINE_IN2) && arch->compose_inline_in_2) {
			arch->compose_inline_in_2 (ts, 8);
		} else if ((options.inline_options & INLINE_IN) && arch->compose_inline_in) {
			arch->compose_inline_in (ts, 8);
		} else {
			arch->compose_deadlock_kcall (ts, K_IN8, 2, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_IN32 -- word input from channel*/
	case I_IN32:
		if ((options.inline_options & INLINE_IN2) && arch->compose_inline_in_2) {
			arch->compose_inline_in_2 (ts, 32);
		} else if ((options.inline_options & INLINE_IN) && arch->compose_inline_in) {
			arch->compose_inline_in (ts, 32);
		} else {
			arch->compose_deadlock_kcall (ts, K_IN32, 2, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_XABLE -- extended enable*/
	case I_XABLE:
		arch->compose_deadlock_kcall (ts, K_XABLE, 1, 0);
		break;
		/*}}}*/
		/*{{{  I_XIN -- extended input*/
	case I_XIN:
		arch->compose_kcall (ts, K_XIN, 3, 0);
		break;
		/*}}}*/
		/*{{{  I_XMIN -- extended mobile input*/
	case I_XMIN:
		arch->compose_kcall (ts, K_XMIN, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_XMIN64 -- 64-bit extended mobile input*/
	case I_XMIN64:
		arch->compose_kcall (ts, K_XMIN64, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_XEND -- extended end*/
	case I_XEND:
		arch->compose_deadlock_kcall (ts, K_XEND, 1, 0);
		break;
		/*}}}*/
		/*{{{  I_OUT -- channel output*/
	case I_OUT:
		if ((options.inline_options & INLINE_OUT) && arch->compose_inline_out) {
			arch->compose_inline_out (ts, 0);
		} else {
			arch->compose_deadlock_kcall (ts, K_OUT, 3, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_OUT8 -- byte output on channel*/
	case I_OUT8:
		if ((options.inline_options & INLINE_OUT2) && arch->compose_inline_out_2) {
			arch->compose_inline_out_2 (ts, 8);
		} else if ((options.inline_options & INLINE_OUT) && arch->compose_inline_out) {
			arch->compose_inline_out (ts, 8);
		} else {
			arch->compose_deadlock_kcall (ts, K_OUT8, 2, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_OUT32 -- word output on channel*/
	case I_OUT32:
		if ((options.inline_options & INLINE_OUT2) && arch->compose_inline_out_2) {
			arch->compose_inline_out_2 (ts, 32);
		} else if ((options.inline_options & INLINE_OUT) && arch->compose_inline_out) {
			arch->compose_inline_out (ts, 32);
		} else {
			arch->compose_deadlock_kcall (ts, K_OUT32, 2, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_OUTWORD -- word output from stack on channel*/
	case I_OUTWORD:
		arch->compose_deadlock_kcall (ts, K_OUTWORD, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_OUTBYTE -- byte output from stack on channel*/
	case I_OUTBYTE:
		if ((options.inline_options & INLINE_OUT2) && arch->compose_inline_out_2) {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REGIND, REG_WPTR));
			ts->stack->old_a_reg = ts->stack->old_b_reg;
			ts->stack->old_b_reg = tstack_newreg (ts->stack);
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, ts->stack->old_b_reg));
			arch->compose_inline_out_2 (ts, 8);
		} else {
			arch->compose_deadlock_kcall (ts, K_OUTBYTE, 2, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_EXTIN -- external channel input*/
	case I_EXTIN:
		arch->compose_deadlock_kcall (ts, K_EXTIN, 3, 0);
		break;
		/*}}}*/
		/*{{{  I_EXTOUT -- external channel output*/
	case I_EXTOUT:
		arch->compose_deadlock_kcall (ts, K_EXTOUT, 3, 0);
		break;
		/*}}}*/
		/*{{{  I_EXTVRFY -- external channel verify*/
	case I_EXTVRFY:
		arch->compose_kcall (ts, K_EXTVRFY, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_EXTENBC -- external channel enable*/
	case I_EXTENBC:
		arch->compose_kcall (ts, K_EXTENBC, 2, 1);
		break;
		/*}}}*/
		/*{{{  I_EXTNDISC -- external channel disable*/
	case I_EXTNDISC:
		arch->compose_kcall (ts, K_EXTNDISC, 3, 1);
		break;
		/*}}}*/
		/*{{{  I_EXTMIN -- external channel mobile input*/
	case I_EXTMIN:
		arch->compose_kcall (ts, K_EXTMIN, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_EXTMOUT -- external channel mobile output*/
	case I_EXTMOUT:
		arch->compose_kcall (ts, K_EXTMOUT, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_EXTMIN64 -- external channel 64-bit mobile input*/
	case I_EXTMIN64:
		arch->compose_kcall (ts, K_EXTMIN64, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_EXTMOUT64 -- external channel 64-bit mobile out*/
	case I_EXTMOUT64:
		arch->compose_kcall (ts, K_EXTMOUT64, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_EXTMINN -- external channel variable size mobile input*/
	case I_EXTMINN:
		arch->compose_kcall (ts, K_EXTMINN, 3, 0);
		break;
		/*}}}*/
		/*{{{  I_EXTMOUTN -- external channel variable size mobile output*/
	case I_EXTMOUTN:
		arch->compose_kcall (ts, K_EXTMOUTN, 3, 0);
		break;
		/*}}}*/
		/*{{{  I_ALTWT -- ALT wait*/
	case I_ALTWT:
		if ((options.inline_options & INLINE_ALT) && arch->compose_inline_altwt) {
			arch->compose_inline_altwt (ts);
		} else {
			arch->compose_deadlock_kcall (ts, K_ALTWT, 0, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_TALTWT -- TIMER ALT wait*/
	case I_TALTWT:
		arch->compose_kcall (ts, K_TALTWT, 0, 0);
		break;
		/*}}}*/
		/*{{{  I_ENBS -- enable SKIP guard*/
	case I_ENBS:
		if (options.kernel_interface & KRNLIFACE_MP) {
			arch->compose_kcall (ts, K_ENBS, 1, 1);
		} else {
			if (ts->stack->must_set_cmp_flags) {
				add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
			}
			this_lab = ++(ts->last_lab);
			add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_LABEL, this_lab));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, Z_READY, ARG_REGIND | ARG_DISP, REG_WPTR, W_STATUS));
			add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
			ts->stack->must_set_cmp_flags = 1;
		}
		break;
		/*}}}*/
		/*{{{  I_DISS -- disable SKIP guard*/
	case I_DISS:
		arch->compose_kcall (ts, K_DISS, 2, 1);
		break;
		/*}}}*/
		/*{{{  I_NDISS -- new disable SKIP guard*/
	case I_NDISS:
		arch->compose_kcall (ts, K_NDISS, 2, 1);
		break;
		/*}}}*/
		/*{{{  I_ENBC -- enable channel guard*/
	case I_ENBC:
		if ((options.inline_options & INLINE_ALT) && arch->compose_inline_enbc) {
			arch->compose_inline_enbc (ts, I_ENBC);
		} else {
			arch->compose_kcall (ts, K_ENBC, 2, 1);
		}
		break;
		/*}}}*/
		/*{{{  I_ENBC3 -- new enable channel guard*/
	case I_ENBC3:
		if (ts->magic_pending & TS_MAGIC_PREENABLE) {
			ts->magic_pending &= ~TS_MAGIC_PREENABLE;
			/* pre-enabling! */
			arch->compose_pre_enbc (ts);
		} else if ((options.inline_options & INLINE_ALT) && arch->compose_inline_enbc) {
			arch->compose_inline_enbc (ts, I_ENBC3);
		} else {
			arch->compose_kcall (ts, K_ENBC3, 3, 1);
		}
		break;
		/*}}}*/
		/*{{{  I_ENBC2 -- new enable channel guard (2 params)*/
	case I_ENBC2:
		arch->compose_kcall (ts, K_ENBC2, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_ENBT3 -- new enable TIMER guard*/
	case I_ENBT3:
		if (ts->magic_pending & TS_MAGIC_PREENABLE) {
			ts->magic_pending &= ~TS_MAGIC_PREENABLE;
			/* pre-enabling! */
			arch->compose_pre_enbt (ts);
		} else {
			arch->compose_kcall (ts, K_ENBT3, 3, 1);
		}
		break;
		/*}}}*/
		/*{{{  I_ENBT2 -- new enable TIMER guard (2 params)*/
	case I_ENBT2:
		arch->compose_kcall (ts, K_ENBT2, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_ENBS3 -- new enable SKIP guard*/
	case I_ENBS3:
		{
			/* ENBS3 inlined here */
			int preenable = 0;

			if (ts->magic_pending & TS_MAGIC_PREENABLE) {
				ts->magic_pending &= ~TS_MAGIC_PREENABLE;
				preenable = 1;
			}

			if ((options.kernel_interface & KRNLIFACE_MP) && !preenable) {
				arch->compose_kcall (ts, K_ENBS3, 2, 1);
			} else {
				/* old_b_reg has the guard -- and that's what we leave on the stack */
				ts->stack->a_reg = ts->stack->old_b_reg;
				if ((constmap_typeof (ts->stack->old_b_reg) == VALUE_CONST) && constmap_regconst (ts->stack->old_b_reg)) {
					/* TRUE precondition -- do jump */
					if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LABADDR) {
						add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, constmap_regconst (ts->stack->old_a_reg)));
					} else {
						add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, ts->stack->old_a_reg));
					}
				} else if ((constmap_typeof (ts->stack->old_b_reg) == VALUE_CONST) && !constmap_regconst (ts->stack->old_b_reg)) {
					/* FALSE precondition -- do nothing */
				} else {
					/* unknown precondition */
					if (ts->stack->must_set_cmp_flags) {
						add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_CC));
					}
					if (preenable) {
						/* direct jump to guarded process if pre-enabling */
						if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LABADDR) {
							add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_LABEL, constmap_regconst (ts->stack->old_a_reg)));
						} else {
							add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_REG | ARG_IND, ts->stack->old_a_reg));
						}
					} else {
						this_lab = ++(ts->last_lab);
						add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_LABEL, this_lab));
						add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, Z_READY, ARG_REGIND | ARG_DISP, REG_WPTR, W_STATUS));
						if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LABADDR) {
							add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, constmap_regconst (ts->stack->old_a_reg)));
						} else {
							add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, ts->stack->old_a_reg));
						}
						add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
					}
					constmap_clearall ();
					ts->stack->must_set_cmp_flags = 1;
				}
			}
		}
		break;
		/*}}}*/
		/*{{{  I_ENBS2 -- new enable SKIP guard (1 param)*/
	case I_ENBS2:
		if (options.kernel_interface & KRNLIFACE_MP) {
			arch->compose_kcall (ts, K_ENBS2, 1, 0);
		} else {
			/* ENBS2 inlined here */
			if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LABADDR) {
				add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, constmap_regconst (ts->stack->old_a_reg)));
			} else {
				add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, ts->stack->old_a_reg));
			}
			constmap_clearall ();
			ts->stack->must_set_cmp_flags = 1;
		}
		break;
		/*}}}*/
		/*{{{  I_DISC -- disable channel guard*/
	case I_DISC:
		if ((options.inline_options & INLINE_ALT) && arch->compose_inline_disc) {
			arch->compose_inline_disc (ts, I_DISC);
		} else {
			arch->compose_kcall (ts, K_DISC, 3, 1);
		}
		break;
		/*}}}*/
		/*{{{  I_NDISC -- new disable channel guard*/
	case I_NDISC:
		if ((options.inline_options & INLINE_ALT) && arch->compose_inline_disc) {
			arch->compose_inline_disc (ts, I_NDISC);
		} else {
			arch->compose_kcall (ts, K_NDISC, 3, 1);
		}
		break;
		/*}}}*/
		/*{{{  I_ENBT -- enable TIMER guard*/
	case I_ENBT:
		arch->compose_kcall (ts, K_ENBT, 2, 1);
		break;
		/*}}}*/
		/*{{{  I_DIST -- disable TIMER guard*/
	case I_DIST:
		arch->compose_kcall (ts, K_DIST, 3, 1);
		break;
		/*}}}*/
		/*{{{  I_NDIST -- new disable TIMER guard*/
	case I_NDIST:
		arch->compose_kcall (ts, K_NDIST, 3, 1);
		break;
		/*}}}*/
		/*{{{  I_TIN -- TIMER input (wait)*/
	case I_TIN:
		if ((options.inline_options & INLINE_TIN) && arch->compose_inline_tin) {
			arch->compose_inline_tin (ts);
		} else {
			arch->compose_kcall (ts, K_TIN, 1, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_SAVEL -- save low-priority queue pointers*/
	case I_SAVEL:
		arch->compose_kcall (ts, K_SAVEL, 1, 0);
		break;
		/*}}}*/
#ifndef PROCESS_PRIORITY
		/*{{{  I_STHF, I_STHB, I_SAVEH -- high-priority queue operations (unsupported)*/
	case I_STHF:
	case I_STHB:
	case I_SAVEH:
		/* illegal -- this run-queue does not exist! */
		fprintf (stderr, "%s: warning: no support for instruction %s, generating run-time error.\n",
				progname, (sec == I_STHF) ? "STHF" : ((sec == I_STHB) ? "STHB" : "SAVEH"));

		if (options.debug_options & DEBUG_RANGESTOP) {
			x = (0xfb00 << 16) + (ts->line_pending & 0xffff);
			add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_CONST, x));
			x = ((ts->file_pending & 0xffff) << 16) + (ts->proc_pending & 0xffff);
			add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_CONST, x));
			add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_LABEL | ARG_ISCONST, ts->filename_label));
			add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_LABEL | ARG_ISCONST, ts->procedure_label));
			ts->stack_drift += 4;
			add_to_ins_chain (arch->compose_kjump (ts, INS_CALL, 0, kif_entry (K_SETERR)));
			ts->stack_drift -= 4;
		} else {
			arch->compose_kcall (ts, K_BNSETERR, 0, 0);
		}
		break;
		/*}}}*/
#endif
#ifdef PROCESS_PRIORITY
		/*{{{  I_STHF -- set specific priority Fptr*/
	case I_STHF:			/* Areg=priority, Breg=Fptr */
		if (options.kernel_interface & KRNLIFACE_MP) {
			fprintf (stderr, "%s: warning: not translatable %d\n", progname, sec);
		} else {
			arch->compose_kcall (ts, K_STPRIF, 2, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_STHB -- set specific priority Bptr*/
	case I_STHB:
		if (options.kernel_interface & KRNLIFACE_MP) {
			fprintf (stderr, "%s: warning: not translatable %d\n", progname, sec);
		} else {
			arch->compose_kcall (ts, K_STPRIB, 2, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_SAVEH -- save priority queue pointers*/
	case I_SAVEH:
		if (options.kernel_interface & KRNLIFACE_MP) {
			fprintf (stderr, "%s: warning: not translatable %d\n", progname, sec);
		} else {
			arch->compose_kcall (ts, K_SAVEPRI, 2, 0);
		}
		break;
		/*}}}*/
#endif
		/*{{{  I_STLB -- set Bptr*/
	case I_STLB:
		if (options.kernel_interface & KRNLIFACE_MP) {
			fprintf (stderr, "%s: warning: not translatable %d\n", progname, sec);
		} else if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
			/* Bptr is now in REG_BPTR */
			add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0x80000000, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
			add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_FLABEL, 0));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, ts->stack->old_a_reg));
			add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, REG_BPTR));
		} else if ((options.inline_options & INLINE_SCHEDULER) && arch->compose_inline_stlx) {
			arch->compose_inline_stlx (ts, I_STLB);
		} else {
			arch->compose_kcall (ts, K_STLB, 1, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_STLF -- set Fptr*/
	case I_STLF:
		if (options.kernel_interface & KRNLIFACE_MP) {
			fprintf (stderr, "%s: warning: not translatable %d\n", progname, sec);
		} else if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
			/* Fptr is now in REG_FPTR */
			add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0x80000000, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
			add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_FLABEL, 0));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, ts->stack->old_a_reg));
			add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, REG_FPTR));
		} else if ((options.inline_options & INLINE_SCHEDULER) && arch->compose_inline_stlx) {
			arch->compose_inline_stlx (ts, I_STLF);
		} else {
			arch->compose_kcall (ts, K_STLF, 1, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_TRAP -- debugging trap*/
	case I_TRAP:
		{
			int i, saveddepth;

			/* fill out stack if not enough */
			saveddepth = ts->stack->old_ts_depth;
			for (i=ts->stack->old_ts_depth; i<3; ts->stack->old_ts_depth++, i++) {
				switch (i) {
				case 0:
					ts->stack->old_a_reg = tstack_newreg (ts->stack);
					break;
				case 1:
					ts->stack->old_b_reg = tstack_newreg (ts->stack);
					break;
				case 2:
					ts->stack->old_c_reg = tstack_newreg (ts->stack);
					break;
				}
			}
			ts->stack->a_reg = ts->stack->old_a_reg;
			ts->stack->b_reg = ts->stack->old_b_reg;
			ts->stack->c_reg = ts->stack->old_c_reg;
			ts->stack->ts_depth = ts->stack->old_ts_depth;

			arch->compose_kcall (ts, K_TRAP, 3, 3);

			ts->stack->old_ts_depth = saveddepth;
			ts->stack->ts_depth = saveddepth;
		}
		break;
		/*}}}*/
		/*{{{  I_ALT -- setup ALT*/
	case I_ALT:
		if (options.kernel_interface & KRNLIFACE_MP) {
			arch->compose_kcall (ts, K_ALT, 0, 0);
		} else {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, Z_ENABLING, ARG_REGIND | ARG_DISP, REG_WPTR, W_STATUS));
		}
		break;
		/*}}}*/
		/*{{{  I_ALTEND -- finish ALT*/
	case I_ALTEND:
		/*
		 *	movl	$L0, %tmpreg
		 *	addl	0(Wptr), %tmpreg
		 *	jmp	*%tmpreg
		 *  L0:
		 */
		if (options.kernel_interface & KRNLIFACE_MP) {
			arch->compose_kcall (ts, K_ALTEND, 0, 0);
		} else {
			this_lab = ++(ts->last_lab);
			tmp_reg = tstack_newreg (ts->stack);
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, this_lab, ARG_REG, tmp_reg));
			add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REGIND | ARG_DISP, REG_WPTR, W_TEMP, ARG_REG, tmp_reg, ARG_REG, tmp_reg));
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, tmp_reg));
			add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		}
		break;
		/*}}}*/
		/*{{{  I_LDTIMER -- load current time*/
	case I_LDTIMER:
		if ((options.inline_options & INLINE_LDTIMER) && arch->compose_inline_ldtimer) {
			arch->compose_inline_ldtimer (ts);
		} else {
			arch->compose_kcall (ts, K_LDTIMER, 0, 1);
		}
		break;
		/*}}}*/
		/*{{{  I_TALT -- start TIMER ALT*/
	case I_TALT:
		if (options.kernel_interface & KRNLIFACE_MP) {
			arch->compose_kcall (ts, K_TALT, 0, 0);
		} else {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, Z_ENABLING, ARG_REGIND | ARG_DISP, REG_WPTR, W_STATUS));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, Z_TIMENOTSET, ARG_REGIND | ARG_DISP, REG_WPTR, W_TLINK));
		}
		break;
		/*}}}*/
		/*{{{  I_LDINF -- load infinity*/
	case I_LDINF:
		tmp_ins = compose_ins (INS_MOVE, 1, 1, ARG_CONST, CONST_INFINITY, ARG_REG, ts->stack->a_reg);
		constmap_new (ts->stack->a_reg, VALUE_CONST, (signed int)CONST_INFINITY, tmp_ins);
		add_to_ins_chain (tmp_ins);
		break;
		/*}}}*/
		/*{{{  I_CFLERR -- clear error-flag (unsupported)*/
	case I_CFLERR:
		/* no translation for this, or never generated ? */
		CONVMISSING ("CFLERR");
		/* **INCOMPLETE** */
		break;
		/*}}}*/
		/*{{{  I_DUP -- duplicate top stack entry*/
	case I_DUP:
		tmp_ins = compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->a_reg);
		constmap_regcopy (ts->stack->old_a_reg, ts->stack->a_reg, tmp_ins);
		add_to_ins_chain (tmp_ins);
		break;
		/*}}}*/
		/*{{{  I_FMUL -- fractional multiply*/
	case I_FMUL:
		if (options.debug_options & DEBUG_OVERFLOW) {
			this_lab = ++(ts->last_lab);
			add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0x80000000, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
			add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NE, ARG_LABEL, this_lab));
			add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0x80000000, ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_CC));
			add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NE, ARG_LABEL, this_lab));
			arch->compose_overflow_jumpcode (ts, PMOP_FMUL);
			add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		}
		/* okay..  do it with a kernel call... */
		arch->compose_kcall (ts, K_FMUL, 2, 1);
		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_FPRZ -- set FP round to zero*/
	case I_FPRZ:
		arch->compose_fp_set_fround (ts, FPU_Z);
		break;
		/*}}}*/
		/*{{{  I_FPRM -- set FP round to nearest*/
	case I_FPRM:
		arch->compose_fp_set_fround (ts, FPU_M);
		break;
		/*}}}*/
		/*{{{  I_FPRP -- set FP round to positive infinity*/
	case I_FPRP:
		arch->compose_fp_set_fround (ts, FPU_P);
		break;
		/*}}}*/
		/*{{{  I_FPRN -- set FP round to negative infinity*/
	case I_FPRN:
		arch->compose_fp_set_fround (ts, FPU_N);
		break;
		/*}}}*/
		/*{{{  I_FPTESTERR -- FP test error (unsupported)*/
	case I_FPTESTERR:
		CONVMISSING ("FPTESTERR");
		break;
		/*}}}*/
		/*{{{  I_FP... -- various floating-point operations*/
	case I_FPCHKI32:
	case I_FPCHKI64:
	case I_FPCHKERR:
	case I_FPREV:
	case I_FPDUP:
	case I_FPLDNLDB:
	case I_FPLDNLSN:
	case I_FPLDNLADDDB:
	case I_FPLDNLADDSN:
	case I_FPLDNLMULDB:
	case I_FPLDNLMULSN:
	case I_FPLDNLDBI:
	case I_FPLDNLSNI:
	case I_FPSTNLDB:
	case I_FPSTNLSN:
	case I_FPSTNLI32:
	case I_FPLDZEROSN:
	case I_FPLDZERODB:
	case I_FPADD:
	case I_FPSUB:
	case I_FPMUL:
	case I_FPDIV:
	case I_FPNAN:
	case I_FPNOTFINITE:
	case I_FPORDERED:
	case I_FPGT:
	case I_FPEQ:
	case I_FPI32TOR64:
	case I_FPI32TOR32:
	case I_FPB32TOR64:
	case I_FPINT:
	case I_FPRTOI32:
	case I_FPREM:
	case I_FPSQRT:
	case I_FPABS:
	case I_FPEXPDEC32:
	case I_FPEXPINC32:
	case I_FPMULBY2:
	case I_FPDIVBY2:
	case I_FPR32TOR64:
	case I_FPR64TOR32:
		arch->compose_fpop (ts, sec);
		break;
		/*}}}*/
		/*{{{  I_FPREMFIRST, I_FPREMSTEP -- unsupported looping floating-point remainder*/
	case I_FPREMSTEP:
		fprintf (stderr, "%s: warning: FPREMSTEP instruction encountered -- should have been removed\n", progname);
		break;
	case I_FPREMFIRST:
		fprintf (stderr, "%s: warning: FPREMFIRST instruction encountered -- should have been removed\n", progname);
		break;
		/*}}}*/
#ifdef PROCESS_PRIORITY
		/*{{{  I_GETPRI -- get priority*/
	case I_GETPRI:
		/* load current priority into Areg */
		if (options.inline_options & INLINE_SCHEDULER) {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup ("&PPriority"), ARG_REG, ts->stack->a_reg));
		} else {
			arch->compose_kcall (ts, K_GETPRI, 0, 1);
		}
		break;
		/*}}}*/
		/*{{{  I_SETPRI -- set priority*/
	case I_SETPRI:
		/* set current priority to old Areg */
		arch->compose_kcall (ts, K_SETPRI, 1, 0);
		/* forcefully undefine the stack here */
		tstack_undefine (ts->stack);
		break;
		/*}}}*/
#endif
		/*{{{  I_PROC_ALLOC -- allocate a process workspace*/
	case I_PROC_ALLOC:
		arch->compose_kcall (ts, K_PROC_ALLOC, 2, 1);
		break;
		/*}}}*/
		/*{{{  I_PROC_PARAM -- copy a parameter to a workspace*/
	case I_PROC_PARAM:
		if (1) {
			/* CGR FIXME: I'm pretty sure this is wrong, will fix when I have code
			 * that generates these instructions */
			add_to_ins_chain (compose_ins (INS_SHL, 2, 1, ARG_CONST, WShift, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg));
			add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_c_reg, ARG_REGIND, ts->stack->old_b_reg));
			tstack_undefine (ts->stack);
		} else {
			arch->compose_kcall (ts, K_PROC_ALLOC, 3, 0);
		}
		break;
		/*}}}*/
		/*{{{  I_PROC_MT_COPY -- copy a mobile type to a workspace*/
	case I_PROC_MT_COPY:
		arch->compose_kcall (ts, K_PROC_MT_COPY, 3, 0);
		break;
		/*}}}*/
		/*{{{  I_PROC_MT_MOVE -- move a mobile type to a workspace*/
	case I_PROC_MT_MOVE:
		arch->compose_kcall (ts, K_PROC_MT_MOVE, 3, 0);
		break;
		/*}}}*/
		/*{{{  I_PROC_START -- start a process allocated with PROC_ALLOC*/
	case I_PROC_START:
		arch->compose_kcall (ts, K_PROC_START, 3, 0);
		break;
		/*}}}*/
		/*{{{  I_PROC_END -- end a process started by PROC_START*/
	case I_PROC_END:
		arch->compose_kcall (ts, K_PROC_END, 1, 0);
		break;
		/*}}}*/
		/*{{{  I_GETPRI -- get processor affinity*/
	case I_GETAFF:
		/* load current affinity into Areg */
		if (options.kernel_interface & KRNLIFACE_MP) {
			arch->compose_kcall (ts, K_GETAFF, 0, 1);
		} else {
			compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REG, ts->stack->a_reg);
		}
		break;
		/*}}}*/
		/*{{{  I_SETAFF -- set processor affinity*/
	case I_SETAFF:
		/* set current affinity to old Areg */
		if (options.kernel_interface & KRNLIFACE_MP) {
			arch->compose_kcall (ts, K_SETAFF, 1, 0);
		}
		/* forcefully undefine the stack here */
		tstack_undefine (ts->stack);
		break;
		/*}}}*/
		/*{{{  I_GETPAS -- get priority and affinity state*/
	case I_GETPAS:
		/* load current priofinity into Areg */
		if (options.kernel_interface & KRNLIFACE_MP) {
			arch->compose_kcall (ts, K_GETPAS, 0, 1);
		} else {
			arch->compose_kcall (ts, K_GETPRI, 0, 1);
		}
		break;
		/*}}}*/
		/*{{{  I_MT_ALLOC -- allocate a mobile type*/
	case I_MT_ALLOC:
		arch->compose_kcall (ts, K_MT_ALLOC, 2, 1);
		break;
		/*}}}*/
		/*{{{  I_MT_RELEASE -- free a mobile type*/
	case I_MT_RELEASE:
		arch->compose_kcall (ts, K_MT_RELEASE, 1, 0);
		break;
		/*}}}*/
		/*{{{  I_MT_CLONE -- clone a mobile type*/
	case I_MT_CLONE:
		arch->compose_kcall (ts, K_MT_CLONE, 1, 1);
		break;
		/*}}}*/
		/*{{{  I_MT_LOCK -- lock a mobile type*/
	case I_MT_LOCK:
		arch->compose_kcall (ts, K_MT_LOCK, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MT_UNLOCK -- unlock a mobile type*/
	case I_MT_UNLOCK:
		arch->compose_kcall (ts, K_MT_UNLOCK, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MT_ENROLL -- increase enroll count of a mobile type*/
	case I_MT_ENROLL:
		arch->compose_kcall (ts, K_MT_ENROLL, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MT_RESIGN -- decrease enroll count of a mobile type*/
	case I_MT_RESIGN:
		arch->compose_kcall (ts, K_MT_RESIGN, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MT_SYNC -- synchronise on a mobile type*/
	case I_MT_SYNC:
		arch->compose_kcall (ts, K_MT_SYNC, 1, 0);
		break;
		/*}}}*/
		/*{{{  I_MT_IN -- mobile type input on channel*/
	case I_MT_IN:
		arch->compose_kcall (ts, K_MT_IN, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MT_OUT -- mobile type output on channel*/
	case I_MT_OUT:
		arch->compose_kcall (ts, K_MT_OUT, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MT_XCHG -- mobile type exchange on channel*/
	case I_MT_XCHG:
		arch->compose_kcall (ts, K_MT_XCHG, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MT_XIN -- extended mobile type input on channel*/
	case I_MT_XIN:
		arch->compose_kcall (ts, K_MT_XIN, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MT_XOUT -- extended mobile type output on channel*/
	case I_MT_XOUT:
		arch->compose_kcall (ts, K_MT_XOUT, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MT_XXCHG -- extended mobile type exchange on channel*/
	case I_MT_XXCHG:
		arch->compose_kcall (ts, K_MT_XXCHG, 2, 0);
		break;
		/*}}}*/
		/*{{{  I_MT_DCLONE -- mobile type data clone*/
	case I_MT_DCLONE:
		arch->compose_kcall (ts, K_MT_DCLONE, 3, 1);
		break;
		/*}}}*/
		/*{{{  I_MT_BIND -- mobile type binding*/
	case I_MT_BIND:
		arch->compose_kcall (ts, K_MT_BIND, 3, 1);
		break;
		/*}}}*/
		/*{{{  I_MT_RESIZE -- mobile type resizing*/
	case I_MT_RESIZE:
		arch->compose_kcall (ts, K_MT_RESIZE, 3, 1);
		break;
		/*}}}*/
		/*{{{  I_MB, I_RMB, I_WMB -- memory barriers*/
	case I_MB:
	case I_RMB:
	case I_WMB:
		if (arch->compose_memory_barrier) {
			arch->compose_memory_barrier (ts, sec);
		}
		break;
		/*}}}*/
		/*{{{  default -- error*/
	default:
		fprintf (stderr, "%s: warning: not supported %d\n", progname, sec);
		break;
		/*}}}*/
	}
	return;
}
/*}}}*/
/*{{{  static void generate_constmapped_21instr (tstate *ts, int etc_instr, int instr, int src_reg1, int src_reg2, int dst_reg, int usecc)*/
/*
 *	generates a standard op (2 inputs, 1 output) with constant stuffing
 */
static void generate_constmapped_21instr (tstate *ts, int etc_instr, int instr, int src_reg1, int src_reg2, int dst_reg, int usecc)
{
	switch (constmap_typeof (src_reg1)) {
	default:
		if (usecc) {
			add_to_ins_chain (compose_ins_ex (etc_instr, instr, 2, 2, ARG_REG, src_reg1, ARG_REG, src_reg2, ARG_REG, dst_reg, ARG_REG | ARG_IMP, REG_CC));
		} else {
			add_to_ins_chain (compose_ins_ex (etc_instr, instr, 2, 1, ARG_REG, src_reg1, ARG_REG, src_reg2, ARG_REG, dst_reg));
		}
		break;
	case VALUE_CONST:
		if (usecc) {
			add_to_ins_chain (compose_ins_ex (etc_instr, instr, 2, 2, ARG_CONST, constmap_regconst (src_reg1), ARG_REG, src_reg2, ARG_REG, dst_reg, ARG_REG | ARG_IMP, REG_CC));
		} else {
			add_to_ins_chain (compose_ins_ex (etc_instr, instr, 2, 1, ARG_CONST, constmap_regconst (src_reg1), ARG_REG, src_reg2, ARG_REG, dst_reg));
		}
		break;
	case VALUE_LOCAL:
		if (usecc) {
			add_to_ins_chain (compose_ins_ex (etc_instr, instr, 2, 2, ARG_REGIND | ARG_DISP, REG_WPTR, (constmap_regconst (src_reg1) << WSH), ARG_REG, src_reg2, ARG_REG, dst_reg, ARG_REG | ARG_IMP, REG_CC));
		} else {
			add_to_ins_chain (compose_ins_ex (etc_instr, instr, 2, 1, ARG_REGIND | ARG_DISP, REG_WPTR, (constmap_regconst (src_reg1) << WSH), ARG_REG, src_reg2, ARG_REG, dst_reg));
		}
		break;
	}
	constmap_remove (dst_reg);
	return;
}
/*}}}*/
/*{{{  static void make_c_name (char *src, int slen, char *dst)*/
/*
 *	void make_c_name (char *src, int slen, char *dst)
 *	prepends '$' if `src' doesn't start with C., B. or BX. (drops the C, B, BX if it does)
 */
static void make_c_name (char *src, int slen, char *dst)
{
	int i, j;

	if (!strncmp (src, "C.", 2) || !strncmp (src, "B.", 2)) {
		i = 0, j = 1;
	} else if (!strncmp (src, "BX.", 3) || !strncmp (src, "KR.", 3)) {
		i = 0, j = 2;
	} else {
		*dst = '$';
		i = 1, j = 0;
	}
	memcpy (dst + i, src + j, slen - j);
	dst[i + (slen - j)] = '\0';
	return;
}
/*}}}*/
/*{{{  static void translate_csub0 (tstate *ts, arch_t *arch)*/
/*
 *	void translate_csub0 (tstate *ts, arch_t *arch)
 *	translates CSUB0 instruction
 */
static void translate_csub0 (tstate *ts, arch_t *arch)
{
	int new_ts_depth;
	int thislab;

	new_ts_depth = ts->stack->old_ts_depth - 1;
	ts->stack->old_ts_depth = 0;
	thislab = ++(ts->last_lab);
	if ((constmap_typeof (ts->stack->old_b_reg) == VALUE_CONST) && (constmap_typeof (ts->stack->old_a_reg) == VALUE_CONST)) {
		/* two constants in check */
		if ((unsigned int)constmap_regconst (ts->stack->old_b_reg) < (unsigned int)constmap_regconst (ts->stack->old_a_reg)) {
			/* ok, do nothing */
		} else {
			/* invalid, generate error */
			fprintf (stderr, "%s: serious: CSUB0 fails near line %d in %s\n", progname, ts->line_pending, ts->file_list[ts->file_pending]);
			if (options.debug_options & DEBUG_RANGESTOP) {
				generate_range_code (ts, REOP_CSUB0, arch);
			} else {
				arch->compose_kcall (ts, K_BRANGERR, 0, -1);
			}
		}
	} else {
		/* generate check */
		switch (constmap_typeof (ts->stack->old_b_reg)) {
		default:
			switch (constmap_typeof (ts->stack->old_a_reg)) {
			default:
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
				add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_A, ARG_LABEL, thislab));
				break;
			case VALUE_CONST:
				/* invert comparison */
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, constmap_regconst (ts->stack->old_a_reg), ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_CC));
				add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_B, ARG_LABEL, thislab));
				break;
			case VALUE_LOCAL:
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_REG, ts->stack->old_b_reg, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH, ARG_REG | ARG_IMP, REG_CC));
				add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_A, ARG_LABEL, thislab));
				break;
			}
			break;
		case VALUE_LOCAL:
			switch (constmap_typeof (ts->stack->old_a_reg)) {
			case VALUE_CONST:
				/* invert comparison */
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, constmap_regconst (ts->stack->old_a_reg),
					ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_b_reg) << WSH, ARG_REG | ARG_IMP, REG_CC));
				add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_B, ARG_LABEL, thislab));
				break;
			default:
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_b_reg) << WSH, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
				add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_A, ARG_LABEL, thislab));
				break;
			}
			break;
		case VALUE_CONST:
			switch (constmap_typeof (ts->stack->old_a_reg)) {
			default:
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, constmap_regconst (ts->stack->old_b_reg), ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
				add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_A, ARG_LABEL, thislab));
				break;
			case VALUE_LOCAL:
				add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, constmap_regconst (ts->stack->old_b_reg),
					ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH, ARG_REG | ARG_IMP, REG_CC));
				add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_A, ARG_LABEL, thislab));
				break;
			}
			break;
		}
		if (options.debug_options & DEBUG_RANGESTOP) {
			generate_range_code (ts, REOP_CSUB0, arch);
		} else {
			arch->compose_kcall (ts, K_BRANGERR, 0, -1);
		}
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, thislab));
	}
	ts->stack->a_reg = ts->stack->old_b_reg;
	ts->stack->b_reg = ts->stack->old_c_reg;
	ts->stack->c_reg = REG_UNDEFINED;
	ts->stack->ts_depth = new_ts_depth;
	ts->stack->must_set_cmp_flags = 1;
	return;
}
/*}}}*/
/*{{{  static void translate_range_check (tstate *ts, int lwb, arch_t *arch, int ecode)*/
/*
 *	void translate_range_check (tstate *ts, int lwb, arch_t *arch, int ecode)
 *	generates a range check
 */
static void translate_range_check (tstate *ts, int lwb, arch_t *arch, int ecode)
{
	int new_ts_depth;
	int thislab;

	new_ts_depth = ts->stack->old_ts_depth - 1;
	ts->stack->old_ts_depth = 0;
	if (lwb == 1) {
		add_to_ins_chain (compose_ins (INS_DEC, 1, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
		constmap_remove (ts->stack->old_b_reg);
	} else if (lwb != 0) {
		fprintf (stderr, "%s: warning: range-check argument not 0/1.  Setting to 0\n", progname);
		lwb = 0;
	}
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
	thislab = ++(ts->last_lab);
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_A, ARG_LABEL, thislab));
	if (options.debug_options & DEBUG_RANGESTOP) {
		generate_range_code (ts, ecode, arch);
	} else {
		arch->compose_kcall (ts, K_BRANGERR, 0, -1);
	}
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, thislab));
	if (lwb == 1) {
		add_to_ins_chain (compose_ins (INS_INC, 1, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
	}
	ts->stack->a_reg = ts->stack->old_b_reg;
	ts->stack->b_reg = ts->stack->old_c_reg;
	ts->stack->c_reg = REG_UNDEFINED;
	ts->stack->ts_depth = new_ts_depth;
	ts->stack->must_set_cmp_flags = 1;
	return;
}
/*}}}*/
/*{{{  void declare_data_bytes (char *data, int len)*/
/*
 *	dumps data in the code..
 */
void declare_data_bytes (char *data, int len)
{
	rtl_chain *trtl;

	flush_ins_chain ();
	trtl = new_rtl ();
	trtl->type = RTL_DATA;
	trtl->u.data.bytes = data;
	trtl->u.data.length = len;
	add_to_rtl_chain (trtl);
	return;
}
/*}}}*/
/*{{{  static void declare_data_bytes_fixup (char *data, int len, tdfixup_t *fixups)*/
/*
 *	dumps constant data, with fixup information
 */
static void declare_data_bytes_fixup (char *data, int len, tdfixup_t *fixups)
{
	rtl_chain *trtl;

	flush_ins_chain ();
	trtl = new_rtl ();
	trtl->type = RTL_XDATA;
	trtl->u.xdata.label = -1;		/* might absorb preceeding label later on */
	trtl->u.xdata.bytes = data;
	trtl->u.xdata.length = len;
	trtl->u.xdata.fixups = fixups;
	add_to_rtl_chain (trtl);
	return;
}
/*}}}*/
/*{{{  static void declare_data_bytes_spec_chain (char *data, int len, rtl_chain **head, rtl_chain **tail)*/
/*
 *	declares data bytes on a specific chain
 */
static void declare_data_bytes_spec_chain (char *data, int len, rtl_chain **head, rtl_chain **tail)
{
	rtl_chain *trtl;

	trtl = new_rtl ();
	trtl->type = RTL_DATA;
	trtl->u.data.bytes = data;
	trtl->u.data.length = len;
	add_rtl_to_spec_chain (trtl, head, tail);
	return;
}
/*}}}*/
/*{{{  void add_to_ins_chain (ins_chain *ins)*/
/*
 *	void add_to_ins_chain (ins_chain *ins)
 *	adds an instruction to the instruction chain
 */
void add_to_ins_chain (ins_chain *ins)
{
	if (!ins_head) {
		ins_head = ins_tail = ins;
	} else {
		ins_tail->next = ins;
		ins->prev = ins_tail;
		ins_tail = ins;
	}
	return;
}
/*}}}*/
/*{{{  static ins_chain **ptr_add_to_ins_chain (void)*/
/*
 *	returns a pointer to where the next instruction added with "add_to_ins_chain" will land
 */
static ins_chain **ptr_add_to_ins_chain (void)
{
	if (!ins_head) {
		return &ins_head;
	} else {
		return &(ins_tail->next);
	}
}
/*}}}*/
/*{{{  void add_chain_to_ins_chain (ins_chain *start)*/
/*
 *	void add_chain_to_ins_chain (ins_chain *start)
 *	adds an instruction chain to the instruction chain
 */
void add_chain_to_ins_chain (ins_chain *start)
{
	if (!ins_head) {
		ins_head = start;
	} else {
		ins_tail->next = start;
		start->prev = ins_tail;
	}
	for (ins_tail = start; ins_tail->next; ins_tail = ins_tail->next);
	return;
}
/*}}}*/
/*{{{  void flush_ins_chain (void)*/
/*
 *	void flush_ins_chain (void)
 *	flushes the current instruction chain onto the list
 */
void flush_ins_chain (void)
{
	rtl_chain *tmp;
	ins_chain *runner;

	if (!ins_head) {
		return;
	}
	tmp = new_rtl ();
	tmp->type = RTL_CODE;
	tmp->u.code.head = ins_head;
	tmp->u.code.tail = ins_tail;
	add_to_rtl_chain (tmp);
	ins_head = ins_tail = NULL;
	/* link instructions to their RTL block */
	for (runner = tmp->u.code.head; runner; runner=runner->next) {
		runner->rtl = tmp;
	}
	return;
}
/*}}}*/
/*{{{  void add_to_rtl_chain (rtl_chain *rtl)*/
/*
 *	void add_to_rtl_chain (rtl_chain *rtl)
 *	adds an RTL structure to the RTL chain
 */
void add_to_rtl_chain (rtl_chain *rtl)
{
	if (!rtl_head) {
		rtl_head = rtl_tail = rtl;
	} else {
		rtl_tail->next = rtl;
		rtl->prev = rtl_tail;
		rtl_tail = rtl;
	}
	return;
}
/*}}}*/
/*{{{  void add_chain_to_rtl_chain (rtl_chain *start)*/
/*
 *	void add_chain_to_rtl_chain (rtl_chain *start)
 *	adds an RTL chain to the main RTL chain
 */
void add_chain_to_rtl_chain (rtl_chain *start)
{
	if (!rtl_head) {
		rtl_head = start;
	} else {
		rtl_tail->next = start;
		start->prev = rtl_tail;
	}
	for (rtl_tail = start; rtl_tail->next; rtl_tail = rtl_tail->next);
	return;
}
/*}}}*/
/*{{{  void add_rtl_to_spec_chain (rtl_chain *rtl, rtl_chain **head, rtl_chain **tail)*/
/*
 *	void add_rtl_to_spec_chain (rtl_chain *rtl, rtl_chain **head, rtl_chain **tail)
 *	adds an RTL structure to a specified chain
 */
void add_rtl_to_spec_chain (rtl_chain *rtl, rtl_chain **head, rtl_chain **tail)
{
	if (!*head) {
		*head = *tail = rtl;
	} else {
		(*tail)->next = rtl;
		rtl->prev = *tail;
		*tail = rtl;
	}
	return;
}
/*}}}*/
/*{{{  ins_chain *scan_ins_chain_back (int instr)*/
/*
 *	scans the instruction chain backwards for a specific instruction
 */
ins_chain *scan_ins_chain_back (int instr)
{
	ins_chain *tmp;

	for (tmp = ins_tail; tmp && (tmp->type != instr); tmp = tmp->prev);
	return tmp;
}
/*}}}*/
/*{{{  static void set_line_pending (tstate *ts, int lineno)*/
/*
 *	void set_line_pending (tstate *ts, int lineno)
 *	sets the current source-file input line number
 */
static void set_line_pending (tstate *ts, int lineno)
{
	ts->line_pending = lineno;
	return;
}
/*}}}*/
/*{{{  static void set_file_pending (tstate *ts, char *fname, int flen)*/
/*
 *	void set_file_pending (tstate *ts, char *fname, int flen)
 *	sets the current source-file name
 */
static void set_file_pending (tstate *ts, char *fname, int flen)
{
	int i;

	for (i=0; i<ts->file_cur; i++) {
		if (!strncmp (fname, ts->file_list[i], flen) && (ts->file_list[i][flen] == '\0')) {
			ts->file_pending = i;
			return;
		}
	}
	if (ts->file_cur == ts->file_max) {
		ts->file_list = (char **)srealloc (ts->file_list, ts->file_max * sizeof (char *), (ts->file_max + 5) * sizeof (char *));
		ts->file_max += 5;
	}
	ts->file_list[ts->file_cur] = string_ndup (fname, flen);
	ts->file_pending = ts->file_cur;
	ts->file_cur++;
	return;
}
/*}}}*/
/*{{{  static void set_proc_pending (tstate *ts, char *pname, int plen)*/
/*
 *	void set_proc_pending (tstate *ts, char *pname, int plen)
 *	sets the current procedure name
 */
static void set_proc_pending (tstate *ts, char *pname, int plen)
{
	int i;

	for (i=0; i<ts->proc_cur; i++) {
		if (!strncmp (pname, ts->proc_list[i], plen) && (ts->proc_list[i][plen] == '\0')) {
			ts->proc_pending = i;
			return;
		}
	}
	if (ts->proc_cur == ts->proc_max) {
		ts->proc_list = (char **)srealloc (ts->proc_list, ts->proc_max * sizeof (char *), (ts->proc_max + 5) * sizeof (char *));
		ts->proc_max += 5;
	}
	ts->proc_list[ts->proc_cur] = string_ndup (pname, plen);
	ts->proc_pending = ts->proc_cur;
	ts->proc_cur++;
	return;
}
/*}}}*/
/*{{{  static int is_power_of_two (const int val)*/
/*
 *	returns the nth power of 2 of val if exact, 0 otherwise
 */
static int is_power_of_two (const int val)
{
	int i;

	for (i=1; i<32; i++) {
		if ((1 << i) == val) {
			return i;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  int get_last_lab (void)*/
/*
 *	int get_last_lab (void)
 *	returns the last label seen
 */
int get_last_lab (void)
{
	return the_last_lab;
}
/*}}}*/

