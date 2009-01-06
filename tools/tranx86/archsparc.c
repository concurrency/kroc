/*
 *	archsparc.c -- sparc architecture stuff
 *	Copyright (C) 2004 Fred Barnes <frmb@kent.ac.uk>
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
#include "sparc.h"
#include "tstate.h"
#include "stubtable.h"
#include "archdef.h"
#include "rtlops.h"
#include "etcdump.h"
#include "etcrtl.h"
#include "asmsparc.h"
#include "kif.h"

#ifndef EXIT_FAILURE
	#define EXIT_FAILURE 1
#endif  /* !EXIT_FAILURE */

/* this is how many might be used for allocation */
#define RMAX_SPARC 7
#define NODEMAX_SPARC 32

static void set_implied_inputs (ins_chain *instr, int n_inputs, int *i_regs);
static void set_implied_outputs (ins_chain *instr, int n_outputs, int *o_regs);
static void compose_fp_set_fround_sparc (tstate *ts, int mode);

/*{{{  static void compose_kcall_sparc (tstate *ts, int call, int regs_in, int regs_out)*/
/*
 *	void compose_kcall_sparc (tstate *ts, int call, int regs_in, int regs_out)
 *	creates a kernel-call (constraining registers appropriately)
 */
static void compose_kcall_sparc (tstate *ts, int call, int regs_in, int regs_out)
{
	kif_entrytype *entry = kif_entry (call);
	int to_preserve, r_in, r_out;
	int i, cregs[3], xregs[5], oregs[3];
	int tmp_reg;
	ins_chain *tmp_ins, *tmp_ins2;
#ifdef USER_DEFINED_CHANNELS
	int target_reg = -1;
	int target_call = -1;
#endif	/* USER_DEFINED_CHANNELS */

	if (regs_out > 0) {
		to_preserve = ts->stack->old_ts_depth - regs_in;
	} else {
		to_preserve = 0;
	}
	r_in = regs_in + to_preserve;
	if (regs_out < 0) {
		r_out = 0;
	} else {
		r_out = regs_out + to_preserve;
	}
	if (to_preserve < 0) {
		/* more parameters to pass than actual things on the stack */
		fprintf (stderr, "%s: warning: %d registers into kernel call %d, but only %d registers on stack\n", progname, regs_in, call, ts->stack->old_ts_depth);
		regs_out += to_preserve;
		to_preserve = 0;
	}
	if (to_preserve) {
		fprintf (stderr, "%s: warning: this doesn\'t work properly yet... (preserving regs across kernel calls)\n", progname);
		fprintf (stderr, "%s: warning: call = %d (%s), regs_in = %d, regs_out = %d, to_preserve = %d\n", progname, call, entry->entrypoint, regs_in, regs_out, to_preserve);
	}
	if (r_out > 3) {
		fprintf (stderr, "%s: warning: %d registers out from kernel call\n", progname, r_out);
	}
	tstack_checkdepth_ge (ts->stack, regs_in);
	/* constrain operands to registers */
	cregs[0] = ts->stack->old_a_reg;
	cregs[1] = ts->stack->old_b_reg;
	cregs[2] = ts->stack->old_c_reg;
	xregs[0] = REG_L0;
	xregs[1] = REG_L1;
	xregs[2] = REG_L2;
	xregs[3] = REG_L3;
	xregs[4] = REG_I1;
	for (i = (r_in - 1); i >= 0; i--) {
		/* all arguments in registers */
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, cregs[i], ARG_REG, xregs[i]));
	}
	/* generate call */
	tmp_ins = NULL;			/* used for adding implied regs later */
	tmp_ins2 = NULL;

	/* call the function by name */
#ifdef USER_DEFINED_CHANNELS
	if (!options.no_ext_chan_checks) {
		target_reg = -1;
		target_call = -1;
		switch (call) {
		case K_ENBC:
			target_reg = ts->stack->old_b_reg;
			target_call = K_EXTENBC;
			break;
		case K_NDISC:
			target_reg = ts->stack->old_c_reg;
			target_call = K_EXTNDISC;
			break;
		case K_EXTENBC:
			/* compiler generated, just mask off the spare bit */
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0xfffffffc, ARG_REG, REG_ALT_L4));
			add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_REG, REG_ALT_L4, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
			break;
		case K_EXTNDISC:
			/* compiler generated, just mask off the spare bit */
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0xfffffffc, ARG_REG, REG_ALT_L4));
			add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_REG, REG_ALT_L4, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg));
			break;
		}
	}
	if (!options.no_ext_chan_checks && (target_reg != -1) && (target_call != -1)) {
		/* if user-defined channels, check low bit for special case, first call to regular */
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_L3));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, target_reg, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0xfffffffc, ARG_REG, REG_ALT_L4));
		add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_REG, REG_ALT_L4, ARG_REG, target_reg, ARG_REG, target_reg));			/* mask off here */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0x00000003, ARG_REG, REG_ALT_L4));
		add_to_ins_chain (compose_ins (INS_AND, 2, 2, ARG_REG, REG_ALT_L4, ARG_REG, tmp_reg, ARG_REG, tmp_reg, ARG_REG | ARG_IMP, REG_CC));			/* mask in here */
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_FLABEL, 0));
		tmp_ins = compose_ins (INS_KCALL, 2, 0, ARG_NAMEDLABEL, string_dup (entry->entrypoint), ARG_CONST | ARG_ISCONST, 12);			/* regular */
		add_to_ins_chain (tmp_ins);
		/* add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, 1)); */
		add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
		tmp_ins2 = compose_ins (INS_KCALL, 2, 0, ARG_NAMEDLABEL, string_dup ((kif_entry (target_call))->entrypoint), ARG_CONST | ARG_ISCONST, 8);		/* special */
		add_to_ins_chain (tmp_ins2);
		/* add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 1)); */
	} else
#endif	/* USER_DEFINED_CHANNELS */
	{
		/* use the KCALL: 2nd input is the adjustment we make in the delay slot, if zero, nop goes in */
		tmp_ins = compose_ins (INS_KCALL, 2, 0, ARG_NAMEDLABEL, string_dup (entry->entrypoint), ARG_CONST | ARG_ISCONST, 8);
		add_to_ins_chain (tmp_ins);
	}

	if (tmp_ins) {
		set_implied_inputs (tmp_ins, r_in, cregs);
	}
	if (tmp_ins2) {
		set_implied_inputs (tmp_ins2, r_in, cregs);
	}
	/* unconstrain regs */
	for (i=0; i<r_in; i++) {
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, cregs[i]));
	}

	/* clean up and collect results */
	if (regs_out > 0) {
		tstack_undefine (ts->stack);
		for (i=0; (i < r_out) && (i < r_in); i++) {
			/* re-use as many input registers as were available */
			tstack_push (ts->stack);
			ts->stack->must_set_cmp_flags = 1;
			oregs[i] = cregs[i];
			constmap_remove (oregs[i]);
		}
		for (; i<r_out; i++) {
			tstack_push (ts->stack);
			ts->stack->must_set_cmp_flags = 1;
			oregs[i] = ts->stack->a_reg;
		}
		ts->stack->ts_depth = r_out;
		for (; i<3; i++) {
			oregs[i] = REG_UNDEFINED;
		}
		ts->stack->a_reg = oregs[0];
		ts->stack->b_reg = oregs[1];
		ts->stack->c_reg = oregs[2];
		if (tmp_ins) {
			set_implied_outputs (tmp_ins, r_out, oregs);
		}
		/* constrain new regs into registers */
		for (i=r_in; i<r_out; i++) {
			add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, oregs[i], ARG_REG, xregs[i]));
			add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, oregs[i]));
		}
	}
	return;
}
/*}}}*/
/*{{{  static void compose_deadlock_kcall_sparc (tstate *ts, int call, int regs_in, int regs_out)*/
/*
 *	void compose_deadlock_kcall_sparc (tstate *ts, int call, int regs_in, int regs_out)
 *	creates a kernel-call with deadlock info (constraining registers appropriately)
 */
static void compose_deadlock_kcall_sparc (tstate *ts, int call, int regs_in, int regs_out)
{
	int this_lab;
	unsigned int x;
	ins_chain *kcall_ins;

	if (options.debug_options & DEBUG_DEADLOCK) {
		/* set own WPTR in link-field */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND | ARG_DISP, REG_WPTR, W_LINK));
	}
	compose_kcall_sparc (ts, call, regs_in, regs_out);
	kcall_ins = scan_ins_chain_back (INS_KCALL);

	if (options.debug_options & DEBUG_DEADLOCK) {
		this_lab = ++(ts->last_lab);
		if (!kcall_ins) {
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, this_lab));
		}
		/* do necessary debug stuff */
		switch (call) {
		case K_IN:
		case K_IN8:
		case K_IN32:
		#if K_MIN != K_UNSUPPORTED
		case K_MIN:
		#endif
		#if K_MINN != K_UNSUPPORTED
		case K_MINN:
		#endif
		#ifdef USER_DEFINED_CHANNELS
		case K_EXTIN:
		#endif
		#ifdef K_MT_IN
		case K_MT_IN:
		#endif
			x = DLOP_IN;
			break;
		case K_OUT:
		case K_OUT8:
		case K_OUT32:
		#if K_MOUT != K_UNSUPPORTED
		case K_MOUT:
		#endif
		#if K_MOUTN != K_UNSUPPORTED
		case K_MOUTN:
		#endif
		#ifdef USER_DEFINED_CHANNELS
		case K_EXTOUT:
		#endif
		#ifdef K_MT_OUT
		case K_MT_OUT:
		#endif
			x = DLOP_OUT;
			break;
		case K_OUTBYTE:
			x = DLOP_OUTBYTE;
			break;
		case K_OUTWORD:
			x = DLOP_OUTWORD;
			break;
		case K_ALTWT:
			x = DLOP_ALTWT;
			break;
		case K_TALTWT:
			x = DLOP_TALTWT;
			break;
		case K_XABLE:
			x = DLOP_XABLE;
			break;
		default:
			x = DLOP_INVALID;
			break;
		}
		x = (x << 24) + ((call & 0xff) << 16) + (ts->line_pending & 0xffff);
		declare_data_bytes (mem_ndup ((char *)&x, 4), 4);
		x = ((ts->file_pending & 0xffff) << 16) + (ts->proc_pending & 0xffff);
		declare_data_bytes (mem_ndup ((char *)&x, 4), 4);
		declare_data_bytes (mem_ndup ("\xde\xad\xbe\xef", 4), 4);
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, ts->procfile_setup_label));
		if (kcall_ins) {
			/* adjust for extra code -- jump will be 1 word */
			if (!kcall_ins->in_args[1] || (ArgMode (kcall_ins->in_args[1]) != ARG_CONST)) {
				fprintf (stderr, "%s: error: KCALL has no constant part\n", progname);
			} else {
				kcall_ins->in_args[1]->regconst += 16;
			}
		} else {
			add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		}
		/* clear the link field -- incase we didn't get put back on the run queue before being rescheduled.. */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, W_LINK));
	}
	return;
}
/*}}}*/
/*{{{  static void set_implied_inputs (ins_chain *instr, int n_inputs, int *i_regs)*/
/*
 *	void set_implied_inputs (ins_chain *instr, int n_inputs, int *i_regs)
 *	sets implied inputs on instruction
 */
static void set_implied_inputs (ins_chain *instr, int n_inputs, int *i_regs)
{
	int i, j;

	for (i=0; instr->in_args[i]; i++);
	for (j=0; j<n_inputs; j++) {
		instr->in_args[i+j] = new_ins_arg ();
		instr->in_args[i+j]->regconst = i_regs[j];
		instr->in_args[i+j]->flags = (ARG_REG | ARG_IMP) & ARG_FLAGMASK;
	}
	return;
}
/*}}}*/
/*{{{  static void set_implied_outputs (ins_chain *instr, int n_outputs, int *o_regs)*/
/*
 *	void set_implied_outputs (ins_chain *instr, int n_outputs, int *o_regs)
 *	sets implied outputs on instruction
 */
static void set_implied_outputs (ins_chain *instr, int n_outputs, int *o_regs)
{

	int i, j;

	for (i=0; instr->out_args[i]; i++);
	for (j=0; j<n_outputs; j++) {
		instr->out_args[i+j] = new_ins_arg ();
		instr->out_args[i+j]->regconst = o_regs[j];
		instr->out_args[i+j]->flags = (ARG_REG | ARG_IMP) & ARG_FLAGMASK;
	}
	return;
}
/*}}}*/
/*{{{  static void compose_inline_quick_reschedule_sparc (tstate *ts)*/
/*
 *	void compose_inline_quick_reschedule_sparc (tstate *ts, int next_is_flabel0)
 *	reschedules next process (off run queue) or X_occscheduler
 *	assumes the current process is neatly saved
 */
static void compose_inline_quick_reschedule_sparc (tstate *ts)
{
	int tmp_reg;

	if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
		/* more processes ? */
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, NOT_PROCESS, ARG_REG, REG_FPTR, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_NAMEDLABEL, string_dup ("_X_occscheduler")));

		/* load next process */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_FPTR, ARG_REG, REG_WPTR));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_FPTR, W_LINK, ARG_REG, REG_FPTR));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_IND | ARG_DISP, REG_WPTR, W_IPTR));
	} else {
		/* jump to _X_schedule if Fptr == NotProcess_p */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup ("&Fptr"), ARG_REG, REG_WPTR));
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, NOT_PROCESS, ARG_REG, REG_WPTR, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_NAMEDLABEL, string_dup ("_X_occscheduler")));

		/* load next process */
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, W_LINK, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_NAMEDLABEL, string_dup ("&Fptr")));

		/* out and jump */
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_IND | ARG_DISP, REG_WPTR, W_IPTR));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_pre_enbc_sparc (tstate *ts)*/
/*
 *	areg has process address/label, breg has the guard, creg has the channel
 */
static void compose_pre_enbc_sparc (tstate *ts)
{
	int skip_lab = -1;

	if ((constmap_typeof (ts->stack->old_b_reg) == VALUE_CONST) && !constmap_regconst (ts->stack->old_b_reg)) {
		/* false pre-condition, nothing to do */
		return;
	} else if (constmap_typeof (ts->stack->old_b_reg) != VALUE_CONST) {
		/* dunno, test and maybe skip */
		if (constmap_typeof (ts->stack->old_b_reg) == VALUE_LOCAL) {
			add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_b_reg) << WSH, ARG_REG | ARG_IMP, REG_CC));
		} else {
			add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_CC));
		}
		skip_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_LABEL, skip_lab));
	}
	/* test channel for readyness */
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REGIND, ts->stack->old_c_reg, ARG_REG | ARG_IMP, REG_CC));
	switch (constmap_typeof (ts->stack->old_a_reg)) {
	case VALUE_LABADDR:
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NE, ARG_LABEL, constmap_regconst (ts->stack->old_a_reg)));
		break;
	default:
		if (skip_lab < 0) {
			skip_lab = ++(ts->last_lab);
		}
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_LABEL, skip_lab));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, ts->stack->old_a_reg));
		break;
	}
	if (skip_lab > -1) {
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, skip_lab));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_pre_enbt_sparc (tstate *ts)*/
/*
 *	areg has process address/label, breg has the guard, creg has the timeout expression
 */
static void compose_pre_enbt_sparc (tstate *ts)
{
	int skip_lab = -1;

	ts->stack->a_reg = ts->stack->old_b_reg;		/* make guard result (discarded anyway, but..) */

	if ((constmap_typeof (ts->stack->old_b_reg) == VALUE_CONST) && !constmap_regconst (ts->stack->old_b_reg)) {
		/* false pre-condition, nothing to do */
		return;
	} else if (constmap_typeof (ts->stack->old_b_reg) != VALUE_CONST) {
		/* dunno, test and maybe skip */
		if (constmap_typeof (ts->stack->old_b_reg) == VALUE_LOCAL) {
			add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_b_reg) << WSH, ARG_REG | ARG_IMP, REG_CC));
		} else {
			add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_CC));
		}
		skip_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_LABEL, skip_lab));
	}
	/* time now is in Wptr[-5/-6] (W_TIME) */
	add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_REGIND | ARG_DISP, REG_WPTR, W_TIME, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg));
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REG, ts->stack->old_c_reg, ARG_REG | ARG_IMP, REG_CC));
	switch (constmap_typeof (ts->stack->old_a_reg)) {
	case VALUE_LABADDR:
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_LT, ARG_LABEL, constmap_regconst (ts->stack->old_a_reg)));
		break;
	default:
		if (skip_lab < 0) {
			skip_lab = ++(ts->last_lab);
		}
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_GE, ARG_LABEL, skip_lab));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, ts->stack->old_a_reg));
		break;
	}
	if (skip_lab > -1) {
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, skip_lab));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_inline_stlx_sparc (tstate *ts, int ins)*/
/*
 *	void compose_inline_stlx_sparc (tstate *ts, int ins)
 *	used to inline STLF and STLB
 */
static void compose_inline_stlx_sparc (tstate *ts, int ins)
{
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, (signed int)0x80000000, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NE, ARG_FLABEL, 0));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, ts->stack->old_a_reg));
	constmap_remove (ts->stack->old_a_reg);
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	switch (ins) {
	case I_STLB:
		if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
			add_to_ins_chain  (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, REG_BPTR));
		} else {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_NAMEDLABEL, string_dup ("&Bptr")));
		}
		break;
	case I_STLF:
		if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, REG_FPTR));
		} else {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_NAMEDLABEL, string_dup ("&Fptr")));
		}
		break;
	}
	return;
}
/*}}}*/
/*{{{  static void compose_debug_insert_sparc (tstate *ts, int mdpairid)*/
/*
 *	void compose_debug_insert_sparc (tstate *ts, int mdpairid)
 *	generates an insert-debugging thing
 */
static void compose_debug_insert_sparc (tstate *ts, int mdpairid)
{
	unsigned int x;
	static char *mdparam_vars[] = {"&mdparam1", "&mdparam2", "&mdparam3", "&mdparam4", "&mdparam5", "&mdparam6"};

	if ((options.debug_options & DEBUG_INSERT) && !(ts->supress_debug_insert)) {
		x = ((ts->file_pending & 0xffff) << 16) + (ts->line_pending & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_NAMEDLABEL, string_dup (mdparam_vars[(mdpairid << 1)])));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->filename_label, ARG_NAMEDLABEL, string_dup (mdparam_vars[(mdpairid << 1) + 1])));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_debug_procnames_sparc (tstate *ts)*/
/*
 *	void compose_debug_procnames_sparc (tstate *ts)
 *	generates procedure-name list for post-mortem debugging
 */
static void compose_debug_procnames_sparc (tstate *ts)
{
	char *procname_buffer;
	int procname_buflen;
	int procname_vcount;
	int procname_fcount;
	int procname_strlen;
	static char *procname_nulls = "\0\0\0\0";
	rtl_chain *trtl;

	procname_buflen = 64000;
	procname_buffer = (char *)smalloc (procname_buflen);
	procname_fcount = ((ts->proc_cur + 1) * sizeof(int));
	*(int *)procname_buffer = ts->proc_cur;
	for (procname_vcount=0; procname_vcount < ts->proc_cur; procname_vcount++) {
		((int *)procname_buffer)[procname_vcount+1] = procname_fcount;
		procname_strlen = strlen (ts->proc_list[procname_vcount]);
		memcpy (procname_buffer + procname_fcount, ts->proc_list[procname_vcount], procname_strlen);
		procname_fcount += procname_strlen;
		memcpy (procname_buffer + procname_fcount, procname_nulls, 4 - (procname_fcount % 4));
		procname_fcount += (4 - (procname_fcount % 4));
	}
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->procedure_label));
	flush_ins_chain ();
	trtl = new_rtl ();
	trtl->type = RTL_DATA;
	trtl->u.data.bytes = (char *)smalloc (procname_fcount);
	trtl->u.data.length = procname_fcount;
	memcpy (trtl->u.data.bytes, procname_buffer, procname_fcount);
	add_to_rtl_chain (trtl);
	sfree (procname_buffer);
	return;
}
/*}}}*/
/*{{{  static void compose_debug_filenames_sparc (tstate *ts)*/
/*
 *	void compose_debug_filenames_sparc (tstate *ts)
 *	generates file-name list for post-mortem debugging
 */
static void compose_debug_filenames_sparc (tstate *ts)
{
	char *filename_buffer;
	int filename_buflen;
	int filename_vcount;
	int filename_fcount;
	int filename_strlen;
	static char *filename_nulls = "\0\0\0\0";
	rtl_chain *trtl;

	filename_buflen = 10000;
	filename_buffer = (char *)smalloc (filename_buflen);
	filename_fcount = ((ts->file_cur + 1) * sizeof(int));
	*(int *)filename_buffer = ts->file_cur;
	for (filename_vcount = 0; filename_vcount < ts->file_cur; filename_vcount++) {
		((int *)filename_buffer)[filename_vcount+1] = filename_fcount;
		filename_strlen = strlen (ts->file_list[filename_vcount]);
		memcpy (filename_buffer + filename_fcount, ts->file_list[filename_vcount], filename_strlen);
		filename_fcount += filename_strlen;
		memcpy (filename_buffer + filename_fcount, filename_nulls, 4 - (filename_fcount % 4));
		filename_fcount += (4 - (filename_fcount % 4));
	}
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->filename_label));
	flush_ins_chain ();
	trtl = new_rtl ();
	trtl->type = RTL_DATA;
	trtl->u.data.bytes = (char *)smalloc (filename_fcount);
	trtl->u.data.length = filename_fcount;
	memcpy (trtl->u.data.bytes, filename_buffer, filename_fcount);
	add_to_rtl_chain (trtl);
	sfree (filename_buffer);
	return;
}
/*}}}*/
/*{{{  static void compose_debug_zero_div_sparc (tstate *ts)*/
/*
 *	void compose_debug_zero_div_sparc (tstate *ts)
 *	generates zero-div target point for post-mortem debugging
 */
static void compose_debug_zero_div_sparc (tstate *ts)
{
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->zerodiv_label));
	/* add_to_ins_chain (compose_ins (INS_LOADLABADDR, 1, 1, ARG_LABEL, ts->filename_label, ARG_REG, REG_ALT_L0)); */
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_LABEL | ARG_ISCONST, ts->filename_label));
	/* add_to_ins_chain (compose_ins (INS_LOADLABADDR, 1, 1, ARG_LABEL, ts->procedure_label, ARG_REG, REG_ALT_L0)); */
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_LABEL | ARG_ISCONST, ts->procedure_label));
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_ALT_L3));
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_ALT_L2));
	add_to_ins_chain (compose_ins (INS_CALL, 1, 0, ARG_NAMEDLABEL, string_dup ("_X_zero_div")));
	return;
}
/*}}}*/
/*{{{  static void compose_debug_floaterr_sparc (tstate *ts)*/
/*
 *	void compose_debug_floaterr_sparc (tstate *ts)
 *	generates floating-point-error target for post-mortem debugging
 */
static void compose_debug_floaterr_sparc (tstate *ts)
{
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->floaterr_label));
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_ALT_L0));
	/* add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_L0, ARG_REG, REG_L6)); */	/* put exception flags in ESI -- nah, using that now ;) */
	/* add_to_ins_chain (compose_ins (INS_LOADLABADDR, 1, 1, ARG_LABEL, ts->filename_label, ARG_REG, REG_ALT_L0)); */
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_LABEL | ARG_ISCONST, ts->filename_label));
	/* add_to_ins_chain (compose_ins (INS_LOADLABADDR, 1, 1, ARG_LABEL, ts->procedure_label, ARG_REG, REG_ALT_L0)); */
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_LABEL | ARG_ISCONST, ts->procedure_label));
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_ALT_L3));
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_ALT_L2));
	add_to_ins_chain (compose_ins (INS_CALL, 1, 0, ARG_NAMEDLABEL, string_dup ("_X_floaterr")));
	return;
}
/*}}}*/
/*{{{  static void compose_debug_overflow_sparc (tstate *ts)*/
/*
 *	void compose_debug_overflow_sparc (tstate *ts)
 *	generates overflow target point for post-mortem debugging
 */
static void compose_debug_overflow_sparc (tstate *ts)
{
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->overflow_label));
	/* add_to_ins_chain (compose_ins (INS_LOADLABADDR, 1, 1, ARG_LABEL, ts->filename_label, ARG_REG, REG_ALT_L0)); */
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_LABEL | ARG_ISCONST, ts->filename_label));
	/* add_to_ins_chain (compose_ins (INS_LOADLABADDR, 1, 1, ARG_LABEL, ts->procedure_label, ARG_REG, REG_ALT_L0)); */
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_LABEL | ARG_ISCONST, ts->procedure_label));
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_ALT_L3));
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_ALT_L2));
	add_to_ins_chain (compose_ins (INS_CALL, 1, 0, ARG_NAMEDLABEL, string_dup ("_X_overflow")));
	return;
}
/*}}}*/
/*{{{  static void compose_debug_rangestop_sparc (tstate *ts)*/
/*
 *	void compose_debug_rangestop_sparc (tstate *ts)
 *	generates range/STOP error target point for post-mortem debugging
 */
static void compose_debug_rangestop_sparc (tstate *ts)
{
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->range_entry_label));
	/* add_to_ins_chain (compose_ins (INS_LOADLABADDR, 1, 1, ARG_LABEL, ts->filename_label, ARG_REG, REG_ALT_L0)); */
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_LABEL | ARG_ISCONST, ts->filename_label));
	/* add_to_ins_chain (compose_ins (INS_LOADLABADDR, 1, 1, ARG_LABEL, ts->procedure_label, ARG_REG, REG_ALT_L0)); */
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0,  ARG_LABEL | ARG_ISCONST, ts->procedure_label));
	add_to_ins_chain (compose_ins (INS_CALL, 1, 0, ARG_NAMEDLABEL, string_dup ("_X_RangeCheckError")));
	return;
}
/*}}}*/
/*{{{  static void compose_debug_seterr_sparc (tstate *ts)*/
/*
 *	generates code for SETERR
 */
static void compose_debug_seterr_sparc (tstate *ts)
{
	unsigned int x;

	x = (0xfb00 << 16) + (ts->line_pending & 0xffff);
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_L5));
	x = ((ts->file_pending & 0xffff) << 16) + (ts->proc_pending & 0xffff);
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_L4));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->filename_label, ARG_REG, REG_ALT_L3));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->procedure_label, ARG_REG, REG_ALT_L2));
	add_to_ins_chain (compose_ins (INS_CALL, 1, 0, ARG_NAMEDLABEL, string_dup ("_X_Seterr")));
}
/*}}}*/
/*{{{  static void compose_overflow_jumpcode_sparc (tstate *ts, int dcode)*/
/*
 *	void compose_overflow_jumpcode_sparc (tstate *ts, int dcode)
 *	generates overflow error jump-code
 */
static void compose_overflow_jumpcode_sparc (tstate *ts, int dcode)
{
	unsigned int x;

	if (options.debug_options & DEBUG_OVERFLOW) {
		x = ((dcode & 0xff) << 24) + (ts->line_pending & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_L3));
		x = ((ts->file_pending & 0xffff) << 16) + ((ts->proc_pending) & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_L2));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, ts->overflow_label));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_floaterr_jumpcode_sparc (tstate *ts)*/
/*
 *	void compose_floaterr_jumpcode_sparc (tstate *ts)
 *	generates floating-point error jump-code
 */
static void compose_floaterr_jumpcode_sparc (tstate *ts)
{
	unsigned int x;

	if (options.debug_options & DEBUG_FLOAT) {
		x = (ts->fp_line_pending & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_L3));
		x = ((ts->fp_file_pending & 0xffff) << 16) + ((ts->fp_proc_pending) & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_L2));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, ts->floaterr_label));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_rangestop_jumpcode_sparc (tstate *ts, int rcode)*/
/*
 *	generates range-check/STOP error jump-code
 */
static void compose_rangestop_jumpcode_sparc (tstate *ts, int rcode)
{
	unsigned int x;

	if (options.debug_options & DEBUG_RANGESTOP) {
		x = ((rcode & 0xff) << 24) + (0xff << 16) + (ts->line_pending & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, x, ARG_REG, REG_ALT_L4));
		x = ((ts->file_pending & 0xffff) << 16) + (ts->proc_pending & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, x, ARG_REG, REG_ALT_L5));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, ts->range_entry_label));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_debug_deadlock_set_sparc (tstate *ts)*/
/*
 *	void compose_debug_deadlock_set_sparc (tstate *ts)
 *	generates deadlock setup point for post-mortem debugging
 */
static void compose_debug_deadlock_set_sparc (tstate *ts)
{
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->procfile_setup_label));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->filename_label, ARG_REG, REG_ALT_L0));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->procedure_label, ARG_REG, REG_ALT_L1));
	add_to_ins_chain (compose_ins (INS_RET, 0, 0));
	return;
}
/*}}}*/
/*{{{  static void compose_divcheck_zero_sparc (tstate *ts, int reg)*/
/*
 *	void compose_divcheck_zero_sparc (tstate *ts, int reg)
 *	checks that `reg' isn't zero before performing division
 */
static void compose_divcheck_zero_sparc (tstate *ts, int reg)
{
	int this_lab;
	unsigned int x;

	switch (constmap_typeof (reg)) {
	default:
		this_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REG, reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_LABEL, this_lab));
		x = (ts->line_pending & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_L3));
		x = ((ts->file_pending & 0xffff) << 16) + (ts->proc_pending & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_L2));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, ts->zerodiv_label));
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		break;
	case VALUE_CONST:
		if (constmap_regconst (reg) == 0) {
			/* wehey! */
			fprintf (stderr, "%s: serious: division by zero seen around line %d in %s\n", progname, ts->line_pending, ts->file_list[ts->file_pending]);
			x = (ts->line_pending & 0xffff);
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_L3));
			x = ((ts->file_pending & 0xffff) << 16) + (ts->proc_pending & 0xffff);
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_L2));
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, ts->zerodiv_label));
		}
		/* otherwise it's a constant non-zero */
		break;
	}
	return;
}
/*}}}*/
/*{{{  static void compose_divcheck_zero_simple_sparc (tstate *ts, int reg)*/
/*
 *	void compose_divcheck_zero_simple_sparc (tstate *ts, int reg)
 *	checks that `reg' isn't zero before performing division
 */
static void compose_divcheck_zero_simple_sparc (tstate *ts, int reg)
{
	switch (constmap_typeof (reg)) {
	default:
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REG, reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_NAMEDLABEL, string_dup ("_X_BNSeterr")));
		break;
	case VALUE_CONST:
		if (constmap_regconst (reg) == 0) {
			/* wehey! */
			fprintf (stderr, "%s: serious: division by zero seen around line %d in %s\n", progname, ts->line_pending, ts->file_list[ts->file_pending]);
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_NAMEDLABEL, string_dup ("_X_BNSeterr")));
		}
		/* otherwise it's a constant non-zero */
		break;
	}
	return;
}
/*}}}*/
/*{{{  static int compose_widenshort_sparc (tstate *ts)*/
/*
 *	int compose_widenshort_sparc (tstate *ts)
 *	sign-extends AX into EAX (Areg), returns the new Areg
 *	modifies Areg on the stack to stop collisions in the register allocator (need to improve that..)
 */
static int compose_widenshort_sparc (tstate *ts)
{
	int tmp_reg = tstack_newreg (ts->stack);

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_L0));
	add_to_ins_chain (compose_ins (INS_CWDE, 1, 1, ARG_REG | ARG_IMP | ARG_IS16BIT, ts->stack->a_reg, ARG_REG | ARG_IMP, ts->stack->a_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->a_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->a_reg, ARG_REG, tmp_reg));
	constmap_remove (ts->stack->a_reg);
	ts->stack->a_reg = tmp_reg;
	return tmp_reg;
}
/*}}}*/
/*{{{  static int compose_widenword_sparc (tstate *ts)*/
/*
 *	int compose_widenword_sparc (tstate *ts)
 *	sign-extends EAX into EDX:EAX.  returns extended register.  mangles the stack somewhat
 */
static int compose_widenword_sparc (tstate *ts)
{
	int tmp_reg = tstack_newreg (ts->stack);

	/* arrange for old_a_reg to stay at stack-top */
	tmp_reg = tstack_newreg (ts->stack);
	ts->stack->a_reg = ts->stack->old_a_reg;
	ts->stack->b_reg = tmp_reg;
	ts->stack->c_reg = ts->stack->old_b_reg;

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_L0));
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->b_reg, ARG_REG, REG_L3));
	add_to_ins_chain (compose_ins (INS_CDQ, 1, 2, ARG_REG | ARG_IMP, ts->stack->old_a_reg, ARG_REG | ARG_IMP, ts->stack->a_reg, ARG_REG | ARG_IMP, ts->stack->b_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->b_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->a_reg));
	constmap_remove (ts->stack->a_reg);
	constmap_remove (ts->stack->b_reg);
	return tmp_reg;
}
/*}}}*/
/*{{{  static void compose_division_sparc (tstate *ts, int dividend, int divisor, int quotient)*/
/*
 *	void compose_division_sparc (tstate *ts, int dividend, int divisor, int quotient)
 *	does division.  Sign-extends EAX into EAX:EDX, then does the division
 */
static void compose_division_sparc (tstate *ts, int dividend, int divisor, int quotient)
{
	int tmp_reg = tstack_newreg (ts->stack);
	int remainder = tmp_reg;

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, dividend, ARG_REG, REG_L0));
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_L3));
	add_to_ins_chain (compose_ins (INS_CDQ, 1, 2, ARG_REG | ARG_IMP, dividend, ARG_REG | ARG_IMP, dividend, ARG_REG | ARG_IMP, tmp_reg));
	add_to_ins_chain (compose_ins (INS_DIV, 3, 2, ARG_REG | ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, dividend, ARG_REG, divisor, \
		ARG_REG | ARG_IMP, quotient, ARG_REG | ARG_IMP, remainder));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, dividend));
	constmap_remove (quotient);
	return;
}
/*}}}*/
/*{{{  static int compose_remainder_sparc (tstate *ts, int dividend, int divisor)*/
/*
 *	int compose_remainder_sparc (tstate *ts, int dividend, int divisor)
 *	does remainder.  Sign-extends EAX into EAX:EDX, does the remainder and returns the register holding it (EDX)
 */
static int compose_remainder_sparc (tstate *ts, int dividend, int divisor)
{
	int tmp_reg = tstack_newreg (ts->stack);
	int quotient = dividend;

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, dividend, ARG_REG, REG_L0));
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, divisor, ARG_REG, REG_L2));
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_L3));
	add_to_ins_chain (compose_ins (INS_CDQ, 1, 2, ARG_REG | ARG_IMP, dividend, ARG_REG | ARG_IMP, dividend, ARG_REG | ARG_IMP, tmp_reg));
	add_to_ins_chain (compose_ins (INS_DIV, 3, 2, ARG_REG | ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, dividend, ARG_REG, divisor, \
		ARG_REG | ARG_IMP, quotient, ARG_REG | ARG_IMP, tmp_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, divisor));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, dividend));
	constmap_remove (dividend);
	return tmp_reg;
}
/*}}}*/
#if 0
/* don't do this on Sparc, yet.. (or by other means) */
/*{{{  static int compose_iospace_loadbyte_sparc (tstate *ts, int portreg, int targetreg)*/
/*
 *	int compose_iospace_loadbyte_sparc (tstate *ts, int portreg, int targetreg)
 *	does INB instruction, targetreg is only a suggestion, real target is returned
 */
static int compose_iospace_loadbyte_sparc (tstate *ts, int portreg, int targetreg)
{
	int tmp_reg = tstack_newreg (ts->stack);

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_L3));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, portreg, ARG_REG, tmp_reg));
	/* upgrade targetreg with fresh register if needed */
	if (targetreg == portreg) {
		targetreg = tstack_newreg (ts->stack);
	}
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, targetreg, ARG_REG, REG_L0));
	add_to_ins_chain (compose_ins (INS_INB, 1, 1, ARG_REG | ARG_IS16BIT, tmp_reg, ARG_REG | ARG_IS8BIT, targetreg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, targetreg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
	return targetreg;
}
/*}}}*/
/*{{{  static void compose_iospace_storebyte_sparc (tstate *ts, int portreg, int sourcereg)*/
/*
 *	void compose_iospace_storebyte_sparc (tstate *ts, int portreg, int sourcereg)
 *	does OUTB instruction
 */
static void compose_iospace_storebyte_sparc (tstate *ts, int portreg, int sourcereg)
{
	int tmp_reg = tstack_newreg (ts->stack);

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_L3));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, portreg, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, sourcereg, ARG_REG, REG_L0));
	add_to_ins_chain (compose_ins (INS_OUTB, 2, 0, ARG_REG | ARG_IS8BIT, sourcereg, ARG_REG | ARG_IS16BIT, tmp_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, sourcereg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
	return;
}
/*}}}*/
#endif
/*{{{  static void compose_move_sparc (tstate *ts)*/
/*
 *	does a move of Areg bytes from *(Creg) to *(Breg)
 */
static void compose_move_sparc (tstate *ts)
{
	int tmpreg;

	/*
	 *	do this a byte at a time for now -- should really be using 64-bit, 32-bit, 16-bit and byte last
	 */
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));		/* set cc */
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_FLABEL, 1));
	add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_CONST | ARG_ISCONST, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg));
	tmpreg = tstack_newreg (ts->stack);
	add_to_ins_chain (compose_ins (INS_MOVEB, 1, 1, ARG_REGIND, ts->stack->old_c_reg, ARG_REG | ARG_IS8BIT, tmpreg));
	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, 1, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg));
	add_to_ins_chain (compose_ins (INS_MOVEB, 1, 1, ARG_REG | ARG_IS8BIT, tmpreg, ARG_REGIND, ts->stack->old_b_reg));
	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
	add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_BLABEL, 0));
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 1));

	return;
}
/*}}}*/
/*{{{  static void compose_shift_sparc (tstate *ts, int sec, int r1, int r2, int r3)*/
static void compose_shift_sparc (tstate *ts, int sec, int r1, int r2, int r3)
{
	add_to_ins_chain (compose_ins (((sec == I_SHL) ? INS_SHL : INS_SHR), 2, 1, ARG_REG, r1, ARG_REG, r2, ARG_REG, r3));
	return;
}
/*}}}*/
/*{{{  static void compose_longop_sparc (tstate *ts, int sec)*/
/*
 *	void compose_longop_sparc (tstate *ts, int sec)
 *	does long-integer operations.  Mangles stack
 */
static void compose_longop_sparc (tstate *ts, int sec)
{
	int tmp_reg;

	switch (sec) {
	case I_LADD:
		/* fresh register */
		ts->stack->a_reg = tstack_newreg (ts->stack);

		add_to_ins_chain (compose_ins (INS_SHL, 2, 1, ARG_CONST, 31, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg, ARG_REG, REG_ALT_R0));
		add_to_ins_chain (compose_ins (INS_ADC, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->a_reg));

		break;
	case I_LSUB:
		/* fresh register */
		ts->stack->a_reg = tstack_newreg (ts->stack);

		add_to_ins_chain (compose_ins (INS_SHL, 2, 1, ARG_CONST, 31, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg, ARG_REG, REG_ALT_R0));
		add_to_ins_chain (compose_ins (INS_SBB, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->a_reg));

		break;
	case I_LSUM:
		/* fresh registers */
		ts->stack->b_reg = tstack_newreg (ts->stack);
		ts->stack->a_reg = tstack_newreg (ts->stack);

		add_to_ins_chain (compose_ins (INS_SHL, 2, 1, ARG_CONST, 31, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg, ARG_REG, REG_ALT_R0));
		add_to_ins_chain (compose_ins (INS_ADC, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_ADC, 2, 1, ARG_REG, REG_ALT_R0, ARG_REG, REG_ALT_R0, ARG_REG, ts->stack->b_reg));

		break;
	case I_LDIFF:
		/* fresh registers */
		ts->stack->b_reg = tstack_newreg (ts->stack);
		ts->stack->a_reg = tstack_newreg (ts->stack);

		add_to_ins_chain (compose_ins (INS_SHL, 2, 1, ARG_CONST, 31, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg, ARG_REG, REG_ALT_R0));
		add_to_ins_chain (compose_ins (INS_SBB, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_ADC, 2, 1, ARG_REG, REG_ALT_R0, ARG_REG, REG_ALT_R0, ARG_REG, ts->stack->b_reg));

		break;
	case I_LMUL:
		/* fresh registers */
		ts->stack->b_reg = tstack_newreg (ts->stack);
		ts->stack->a_reg = tstack_newreg (ts->stack);
		tmp_reg = tstack_newreg (ts->stack);

		add_to_ins_chain (compose_ins (INS_UMUL, 2, 2, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_ALT_Y));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_Y, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_ADC, 2, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REG, tmp_reg, ARG_REG, ts->stack->b_reg));

		break;
	case I_LDIV:
		/* fresh registers */
		ts->stack->a_reg = tstack_newreg (ts->stack);
		ts->stack->b_reg = tstack_newreg (ts->stack);
		tmp_reg = tstack_newreg (ts->stack);

		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_c_reg, ARG_REG, REG_ALT_Y));
		add_to_ins_chain (compose_ins (INS_UDIV, 3, 2, ARG_REG | ARG_IMP, REG_ALT_Y, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_a_reg,
					ARG_REG | ARG_IMP, REG_ALT_Y, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_UMUL, 2, 2, ARG_REG, tmp_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_ALT_Y));
		add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->b_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REG, ts->stack->a_reg));
		
		break;
	case I_LSHL:
	case I_LSHR:
		/* fresh registers */
		ts->stack->a_reg = tstack_newreg (ts->stack);
		ts->stack->b_reg = tstack_newreg (ts->stack);
		tmp_reg = tstack_newreg (ts->stack);

		/* test for zero */
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_REG, REG_ALT_R0, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_FLABEL, 1));

		/* test for overflow */
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST | ARG_ISCONST, 64, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_GE, ARG_FLABEL, 2));

		/* test for long shift */
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST | ARG_ISCONST, 32, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_AE, ARG_FLABEL, 3));

		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ((sec == I_LSHL) ? ts->stack->old_b_reg : ts->stack->old_c_reg), ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins ((sec == I_LSHL) ? INS_SHL : INS_SHR, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->b_reg));
		add_to_ins_chain (compose_ins ((sec == I_LSHL) ? INS_SHL : INS_SHR, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, REG_ALT_R0, ARG_REG, ts->stack->old_a_reg));
		constmap_remove (ts->stack->old_a_reg);
		add_to_ins_chain (compose_ins ((sec == I_LSHL) ? INS_SHR : INS_SHL, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, tmp_reg, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_OR, 2, 1, ARG_REG, tmp_reg, ARG_REG, ts->stack->b_reg, ARG_REG, ts->stack->b_reg));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, 4));

		/* zero-shift */
		add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 1));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->b_reg));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, 4));

		/* excessive-shift */
		add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 2));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_R0, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_R0, ARG_REG, ts->stack->b_reg));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, 4));

		/* long shift */
		add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 3));
		add_to_ins_chain (compose_ins ((sec == I_LSHL) ? INS_SHL : INS_SHR, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->b_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_R0, ARG_REG, ts->stack->a_reg));

		/* out */
		add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 4));

		break;
	default:
		fprintf (stderr, "%s: fatal: secondary op-code 0x%X in compose_longop_sparc()\n", progname, sec);
		break;
	}
	return;
}
/*}}}*/
/*{{{  static void compose_return_sparc (tstate *ts)*/
/*
 *	generates PROC/FUNCTION return code -- moved out here to deal with different mechanisms for
 *	returning arguments from FUNCTIONs
 */
static void compose_return_sparc (tstate *ts)
{
	int i;
	int toldregs[3], tfixedregs[3];

	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR));

	toldregs[0] = ts->stack->old_a_reg;
	toldregs[1] = ts->stack->old_b_reg;
	toldregs[2] = ts->stack->old_c_reg;
	tfixedregs[0] = REG_ALT_L0;
	tfixedregs[1] = REG_ALT_L1;
	tfixedregs[2] = REG_ALT_L2;

	for (i=0; i<ts->numfuncresults; i++) {
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, toldregs[i], ARG_REG, tfixedregs[i]));
	}
	ts->numfuncresults = 0;
	/* use r11 to do the bridging */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, -16, ARG_REG, REG_ALT_L4));
	add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, REG_ALT_L4));
	for (i--; i>=0; i--) {
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, toldregs[i]));
	}

	return;
}
/*}}}*/
/*{{{  static void compose_funcresults_sparc (tstate *ts, int nresults)*/
/*
 *	generates other half of FUNCTION return, picking up values set by compose_return
 */
static void compose_funcresults_sparc (tstate *ts, int nresults)
{
	int i;
	int tnewregs[3], tfixedregs[3];

	for (i=0; i<nresults; i++) {
		tstack_push (ts->stack);
	}
	tfixedregs[0] = REG_ALT_L0;
	tfixedregs[1] = REG_ALT_L1;
	tfixedregs[2] = REG_ALT_L2;
	tnewregs[0] = ts->stack->a_reg;
	tnewregs[1] = ts->stack->b_reg;
	tnewregs[2] = ts->stack->c_reg;

	for (i=0; i<nresults; i++) {
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tnewregs[i], ARG_REG, tfixedregs[i]));
	}
	for (i=0; i<nresults; i++) {
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tnewregs[i]));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_fpop_sparc (tstate *ts, int sec)*/
/*
 *	generates FP instructions
 */
static void compose_fpop_sparc (tstate *ts, int sec)
{
	int tmp_reg, this_lab;

	switch (sec) {
	case I_FPCHKERR:
		/* okay... */
		tmp_reg = tstack_newreg (ts->stack);
		this_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_L0));
		/* need to store current flags... (hide in stack) */
		add_to_ins_chain (compose_ins (INS_LAHF, 0, 1, ARG_REG|ARG_IMP, tmp_reg));
		add_to_ins_chain (compose_ins (INS_MOVEB, 1, 1, ARG_REG|ARG_IS8BIT|ARG_IS8HIGH, tmp_reg, ARG_REGIND|ARG_DISP, REG_ALT_L4, -4));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG|ARG_IS16BIT, tmp_reg));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_CONST, 0x3c, ARG_REG|ARG_IS16BIT, tmp_reg, ARG_REG|ARG_IS16BIT, tmp_reg));
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0x20, ARG_REG|ARG_IS16BIT, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_LABEL, this_lab));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG|ARG_IS16BIT, tmp_reg));
		add_to_ins_chain (compose_ins (INS_AND, 2, 2, ARG_CONST, 0x3c, ARG_REG|ARG_IS16BIT, tmp_reg, ARG_REG|ARG_IS16BIT, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_LABEL, this_lab));
		/* might be a lone ineaxact-result */
		if (options.debug_options & DEBUG_FLOAT) {
			compose_floaterr_jumpcode_sparc (ts);
			/* normal execution continues */
		} else if (!options.disable_checking) {
			compose_kcall_sparc (ts, K_BSETERR, 0, -1);
		}
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		/* restore flags */
		add_to_ins_chain (compose_ins (INS_MOVEB, 1, 1, ARG_REGIND|ARG_DISP, REG_ALT_L4, -4, ARG_REG|ARG_IS8BIT|ARG_IS8HIGH, tmp_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG|ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		break;
	case I_FPEXPDEC32:
		add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_CONST, -32));
		add_to_ins_chain (compose_ins (INS_FILD32, 1, 0, ARG_REGIND, REG_ALT_L4));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, 4, ARG_REG, REG_ALT_L4, ARG_REG, REG_ALT_L4));
		add_to_ins_chain (compose_ins (INS_FXCH, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSCALE, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTP, 0, 1, ARG_FREG, 1));
		compose_fp_set_fround_sparc (ts, FPU_N);
		tstate_ctofp (ts);
		break;
	case I_FPEXPINC32:
		add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_CONST, 32));
		add_to_ins_chain (compose_ins (INS_FILD32, 1, 0, ARG_REGIND, REG_ALT_L4));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, 4, ARG_REG, REG_ALT_L4, ARG_REG, REG_ALT_L4));
		add_to_ins_chain (compose_ins (INS_FXCH, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSCALE, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTP, 0, 1, ARG_FREG, 1));
		compose_fp_set_fround_sparc (ts, FPU_N);
		tstate_ctofp (ts);
		break;
	case I_FPCHKI32:
	case I_FPCHKI64:
		/* this should work, but are never actually generated for a T800 */
		/* check that (-2^xx <= value) */
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->floatrange_label[(sec == I_FPCHKI32) ? 0 : 2], ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_FLD64, 1, 0, ARG_REGIND, tmp_reg));
		add_to_ins_chain (compose_ins (INS_FCOMP, 0, 0));
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_L0));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		this_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_BE, ARG_LABEL, this_lab));
		compose_overflow_jumpcode_sparc (ts, (sec == I_FPCHKI32) ? PMOP_FPCHKI32 : PMOP_FPCHKI64);
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		/* check that (2^xx > value) */
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->floatrange_label[(sec == I_FPCHKI32) ? 1 : 3], ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_FLD64, 1, 0, ARG_REGIND, tmp_reg));
		add_to_ins_chain (compose_ins (INS_FCOMP, 0, 0));
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_L0));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		this_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_A, ARG_LABEL, this_lab));
		compose_overflow_jumpcode_sparc (ts, (sec == I_FPCHKI32) ? PMOP_FPCHKI32 : PMOP_FPCHKI64);
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		break;
	case I_FPNAN:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_L0));
		add_to_ins_chain (compose_ins (INS_FTST, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, ts->stack->a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
		ts->cond = CC_FPNAN;
		ts->stack->must_set_cmp_flags = 0;
		compose_fp_set_fround_sparc (ts, FPU_N);
		tstate_ctofp (ts);
		break;
	case I_FPNOTFINITE:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_L0));
		add_to_ins_chain (compose_ins (INS_FXAM, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, ts->stack->a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
		ts->cond = CC_FPINFNAN;
		ts->stack->must_set_cmp_flags = 0;
		compose_fp_set_fround_sparc (ts, FPU_N);
		tstate_ctofp (ts);
		break;
	case I_FPORDERED:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_L0));
		add_to_ins_chain (compose_ins (INS_FUCOM, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, ts->stack->a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
		ts->cond = CC_FPORD;
		ts->stack->must_set_cmp_flags = 0;
		tstate_ctofp (ts);
		break;
	case I_FPGT:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_L0));
		add_to_ins_chain (compose_ins (INS_FCOMPP, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, ts->stack->a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
		ts->cond = CC_B;
		ts->stack->must_set_cmp_flags = 0;
		ts->stack->fs_depth -= 2;
		tstate_ctofp (ts);
		break;
	case I_FPEQ:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_L0));
		add_to_ins_chain (compose_ins (INS_FCOMPP, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, ts->stack->a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
		ts->cond = CC_Z;
		ts->stack->must_set_cmp_flags = 0;
		ts->stack->fs_depth -= 2;
		tstate_ctofp (ts);
		break;
	case I_FPREM:
		/* this is unpleasant.. */
		this_lab = ++(ts->last_lab);
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_FXCH, 0, 0));
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		add_to_ins_chain (compose_ins (INS_FPREM1, 0, 0));
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_L0));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_AND, 2, 2, ARG_CONST, 0x400, ARG_REG, tmp_reg, ARG_REG, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_LABEL, this_lab));
		add_to_ins_chain (compose_ins (INS_FSTP, 0, 1, ARG_FREG, 1));
		ts->stack->fs_depth--;
		tstate_ctofp (ts);
		break;
	default:
		fprintf (stderr, "%s: compose_fpop_sparc: warning: unknown instruction %d (ignoring)\n", progname, sec);
		break;
	}
	return;
}
/*}}}*/
/*{{{  static void compose_external_ccall_sparc (tstate *ts, int inlined, char *name, ins_chain **pst_first, ins_chain **pst_last)*/
/*
 *	generates code to perform an external C call
 */
static void compose_external_ccall_sparc (tstate *ts, int inlined, char *name, ins_chain **pst_first, ins_chain **pst_last)
{
	int tmp_reg, kernel_call;

	tmp_reg = tstack_newreg (ts->stack);
	*pst_first = compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, 4, ARG_REG, REG_WPTR, ARG_REG, REG_ALT_O0);
	add_to_ins_chain (*pst_first);
#if 0
	add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_CONST | ARG_ISCONST, 8, ARG_REG, REG_ALT_L4, ARG_REG, REG_ALT_L4));
	if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_FPTR, ARG_NAMEDLABEL, string_dup ("&Fptr")));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_BPTR, ARG_NAMEDLABEL, string_dup ("&Bptr")));
	}
	add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, 4, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, tmp_reg));
#endif
	if (options.extref_prefix) {
		char sbuf[128];

		sprintf (sbuf, "%s%s", options.extref_prefix, name + 1);
		add_to_ins_chain (compose_ins (INS_CCALL, 1, 0, ARG_NAMEDLABEL, string_dup (sbuf)));
	} else {
		add_to_ins_chain (compose_ins (INS_CCALL, 1, 0, ARG_NAMEDLABEL, string_dup (name + 1)));
	}
#if 0
	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, 4, ARG_REG, REG_ALT_L4, ARG_REG, REG_ALT_L4));
	if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup ("&Fptr"), ARG_REG, REG_FPTR));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup ("&Bptr"), ARG_REG, REG_BPTR));
	}
	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, 8, ARG_REG, REG_ALT_L4, ARG_REG, REG_ALT_L4));
	add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REG, REG_WPTR));
#endif
	if (!strcmp (name, "C.ccsp.suspendproc")) {
		/* hack for dynamic process suspension -- we've done ccsp.suspendproc, now call kernel */
		*pst_last = NULL;
		/* re-assess and push parameters (and fix Wptr) */
		add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, 4, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR));
		add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, tmp_reg));
		kernel_call = K_DYNPROC_SUSPEND;
		if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
			*pst_last = compose_ins (INS_CALL, 1, 0, ARG_NAMEDLABEL, string_dup ((kif_entry (kernel_call))->entrypoint));
		} else if (options.kernel_interface & KRNLIFACE_MESH) {
			fprintf (stderr, "%s: error: do not have dynamic process support for MESH yet\n", progname);
			exit (EXIT_FAILURE);
		} else if (options.kernel_interface & KRNLIFACE_CSPLINUX) {
			fprintf (stderr, "%s: error support for CSP/Linux not yet implemented\n", progname);
			exit (EXIT_FAILURE);
		}
	} else {
		*pst_last = compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR);
	}
	add_to_ins_chain (*pst_last);
}
/*}}}*/
/*{{{  static void compose_bcall_sparc (tstate *ts, int i, int kernel_call, int inlined, char *name, ins_chain **pst_first, ins_chain **pst_last)*/
/*
 *	generates code for a blocking system-call
 */
static void compose_bcall_sparc (tstate *ts, int i, int kernel_call, int inlined, char *name, ins_chain **pst_first, ins_chain **pst_last)
{
	int tmp_reg, tmp_reg2;
	ins_chain *st_first, *st_last;

	tmp_reg = tstack_newreg (ts->stack);
	tmp_reg2 = tstack_newreg (ts->stack);

	st_first = compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, 4, ARG_REG, tmp_reg);
	add_to_ins_chain (st_first);
	constmap_remove (tmp_reg);
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, (kernel_call != K_KERNEL_RUN) ? REG_L1 : REG_L0));
	if (kernel_call != K_KERNEL_RUN) {
		/* load address of function to call */
		if (options.extref_prefix) {
			char sbuf[128];

			sprintf (sbuf, "%s%s", options.extref_prefix, name + ((i == 2) ? 1 : 2));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL | ARG_ISCONST, string_dup (sbuf), ARG_REG, tmp_reg2));
		} else {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL | ARG_ISCONST, string_dup (name + ((i == 2) ? 1 : 2)), ARG_REG, tmp_reg2));
		}
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg2, ARG_REG, REG_L0));
	}
	add_to_ins_chain (compose_ins (INS_KCALL, 2, 0, ARG_NAMEDLABEL, string_dup ((kif_entry (kernel_call))->entrypoint), ARG_CONST | ARG_ISCONST, 8));
	st_last = compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR);
	add_to_ins_chain (st_last);

	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
	if (kernel_call != K_KERNEL_RUN) {
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg2));
	}

	*pst_first = st_first;
	*pst_last = st_last;
	return;
}
/*}}}*/
/*{{{  static void compose_entry_prolog_sparc (tstate *ts)*/
/*
 *	generates entry prologue for main-module
 */
static void compose_entry_prolog_sparc (tstate *ts)
{
	rtl_chain *trtl;
	char sbuffer[128];

	trtl = new_rtl ();
	trtl->type = RTL_PUBLICSETNAMEDLABEL;
	sprintf (sbuffer, "%s_occam_start", (options.extref_prefix ? options.extref_prefix : ""));
	trtl->u.label_name = string_dup (sbuffer);
	add_to_rtl_chain (trtl);

	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup ("&Wptr"), ARG_REG, REG_WPTR));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup ("&Fptr"), ARG_REG, REG_FPTR));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup ("&Bptr"), ARG_REG, REG_BPTR));

	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_NAMEDLABEL, string_dup ("&occam_finished")));
	compose_kcall_sparc (ts, K_PAUSE, 0, 0);
	flush_ins_chain ();

	return;
}
/*}}}*/
/*{{{  static void compose_fp_init_sparc (tstate *ts)*/
/*
 *	initialises the FPU
 */
static void compose_fp_init_sparc (tstate *ts)
{
#if 0
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0xf3e, ARG_REGIND | ARG_DISP, REG_ALT_L4, -4));
	add_to_ins_chain (compose_ins (INS_FLDCW, 1, 0, ARG_REGIND | ARG_DISP, REG_ALT_L4, -4));
#endif
	return;
}
/*}}}*/
/*{{{  static void compose_reset_fregs_sparc (tstate *ts)*/
/*
 *	resets FPU registers (clears them out)
 */
static void compose_reset_fregs_sparc (tstate *ts)
{
	ts->stack->fpu_mode = FPU_NONE;
	return;
}
/*}}}*/
/*{{{  static void compose_fp_set_fround_sparc (tstate *ts, int mode)*/
/*
 *	sets FPU rounding mode (uses stack)
 */
static void compose_fp_set_fround_sparc (tstate *ts, int mode)
{
	int fpu_orval;

	if (mode == ts->stack->fpu_mode) {
		return;
	}
	add_to_ins_chain (compose_ins (INS_FSTCW, 0, 1, ARG_REGIND | ARG_DISP, REG_ALT_L4, -4));
	/* actually only a 16-bit op, but it doesn't make any speed difference */
	add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_CONST | ARG_ISCONST, 0xf3ff, ARG_REGIND | ARG_DISP, REG_ALT_L4, -4, ARG_REGIND | ARG_DISP, REG_ALT_L4, -4));
	if (options.debug_options & DEBUG_FLOAT) {
		fpu_orval = 0x3f;		/* all exceptions masked */
	} else {
		fpu_orval = 0;			/* let exceptions get generated */
	}
	if (mode > 0) {
		fpu_orval |= (mode << 10);
	}
	if (fpu_orval) {
		add_to_ins_chain (compose_ins (INS_OR, 2, 1, ARG_CONST | ARG_ISCONST, fpu_orval, ARG_REGIND | ARG_DISP, REG_ALT_L4, -4, ARG_REGIND | ARG_DISP, REG_ALT_L4, -4));
	}
	add_to_ins_chain (compose_ins (INS_FLDCW, 1, 0, ARG_REGIND | ARG_DISP, REG_ALT_L4, -4));
	ts->stack->fpu_mode = mode;
	return;
}
/*}}}*/
/*{{{  static int regcolour_special_to_real_sparc (int sreg)*/
/*
 *	int regcolour_special_to_real_sparc (int sreg)
 *	turns a special register into a real register
 */
static int regcolour_special_to_real_sparc (int sreg)
{
	if ((sreg >= FIRST_ALT_REG) && (sreg <= LAST_ALT_REG)) {
		return ((sreg - FIRST_ALT_REG) + FIRST_REAL_REG);
	}
	switch (sreg) {
	case REG_WPTR:
		return REG_I0;
		break;
	case REG_JPTR:
		return REG_L6;
		break;
	case REG_LPTR:
		return REG_L7;
		break;
	case REG_SPTR:
		return REG_SP;
		break;
	}
	return sreg;
}
/*}}}*/
/*{{{  static char *get_register_name_sparc (int reg)*/
/*
 *	returns a string representing the register name
 */
static char *get_register_name_sparc (int reg)
{
	static char *mainregs[] = {"%r0", "%g1", "%g2", "%g3", "%g4", "%g5", "%g6", "%g7",
					"%o0", "%o1", "%o2", "%o3", "%o4", "%o5", "%sp", "%o7",
					"%l0", "%l1", "%l2", "%l3", "%l4", "%l5", "%l6", "%l7",
					"%i0", "%i1", "%i2", "%i3", "%i4", "%i5", "%fp", "%i7", "%y"};
	static char *otherregs[] = {"%Wptr", "%Fptr", "%Bptr", "%Sptr"};
	static char *unkreg = "?";
	
	if ((reg >= FIRST_REAL_REG) && (reg <= LAST_REAL_REG)) {
		return mainregs[reg];
	} else if ((reg >= FIRST_ALT_REG) && (reg <= LAST_ALT_REG)) {
		return mainregs[(reg - FIRST_ALT_REG) + FIRST_REAL_REG];
	} else {
		switch (reg) {
		case REG_WPTR:
			return otherregs[0];
		case REG_FPTR:
			return otherregs[1];
		case REG_BPTR:
			return otherregs[2];
		case REG_SPTR:
			return otherregs[3];
		default:
			return unkreg;
		}
	}
}
/*}}}*/
/*{{{  static int regcolour_get_regs_sparc (int *regs)*/
/*
 *	populates an array with registers which will be used for allocation
 */
static int regcolour_get_regs_sparc (int *regs)
{
	static int r_names[RMAX_SPARC] = {REG_L0, REG_L1, REG_L2, REG_L3, REG_I1, REG_I2, REG_I3};

	memcpy (regs, r_names, RMAX_SPARC * sizeof (int));

	return RMAX_SPARC;
}
/*}}}*/
/*{{{  static int rtl_validate_instr_sparc (ins_chain *ins)*/
/*
 *	int rtl_validate_instr (ins_chain *ins)
 *	validates a single instruction
 *	returns 1 if valid instruction, 0 if not
 */
static int rtl_validate_instr_sparc (ins_chain *ins)
{
	switch (ins->type) {
	case INS_UNDEFINED:
		fprintf (stderr, "error: undefined instruction\n");
		return 0;
		break;
	case INS_MOVE:
		if (!rtl_validate_checknumargs (ins, 1, 1)) {
			fprintf (stderr, "error: MOVE has wrong number of arguments\n");
			return 0;
		}
		if (!rtl_validate_checkargtype (ins->in_args[0], ARG_CONST, ARG_REG, ARG_REGIND, ARG_FLABEL, ARG_BLABEL, ARG_NAMEDLABEL, ARG_LABEL, ARG_INSLABEL, 0)) {
			fprintf (stderr, "error: MOVE input of invalid type %d\n", ArgMode (ins->in_args[0]));
			return 0;
		}
		if (!rtl_validate_checkargtype (ins->out_args[0], ARG_REG, ARG_REGIND, ARG_REGINDSIB, ARG_NAMEDLABEL, 0)) {
			fprintf (stderr, "error: MOVE output of invalid type %d\n", ArgMode (ins->out_args[0]));
			return 0;
		}
		if ((ArgMode (ins->in_args[0]) == ARG_REGIND) && (ArgMode (ins->out_args[0]) == ARG_REGIND)) {
			fprintf (stderr, "error: MOVE has two memory operands\n");
			return 0;
		}
		break;
	case INS_NOP:
		if (!rtl_validate_checknumargs (ins, 0, 0)) {
			return 0;
		}
		break;
	case INS_CJUMP:
		{
			ins_chain *setting = rtl_scan_setscc_backward (ins, 1);

			if (!setting) {
				fprintf (stderr, "warning: didn\'t find a CC-setting instruction before CJUMP\n");
			}
		}
		break;
	case INS_SETCC:
		{
			ins_chain *setting = rtl_scan_setscc_backward (ins, 1);

			if (!setting) {
				fprintf (stderr, "warning: didn\'t find a CC-setting instruction before SETCC\n");
			}
		}
		break;
	case INS_CMP:
		if (!rtl_validate_checknumargs (ins, 2, 1)) {
			fprintf (stderr, "error: CMP has wrong number of arguments\n");
			return 0;
		}
		if (!rtl_validate_checkargtype (ins->out_args[0], ARG_REG, 0)) {
			fprintf (stderr, "error: CMP output of invalid type %d\n", ArgMode (ins->out_args[0]));
			return 0;
		}
		if ((ArgReg (ins->out_args[0]) != REG_CC) || !ArgIsImplied (ins->out_args[0])) {
			fprintf (stderr, "error: CMP output register invalid: %d\n", ArgReg (ins->out_args[0]));
			return 0;
		}
		break;
	case INS_SAHF:
		if (!rtl_validate_checknumargs (ins, 1, 1)) {
			fprintf (stderr, "error: SAHF has wrong number of arguments\n");
			return 0;
		}
		if (!rtl_validate_checkargtype (ins->out_args[0], ARG_REG, 0)) {
			fprintf (stderr, "error: SAHF output of invalid type %d\n", ArgMode (ins->out_args[0]));
			return 0;
		}
		if ((ArgReg (ins->out_args[0]) != REG_CC) || !ArgIsImplied (ins->out_args[0])) {
			fprintf (stderr, "error: SAHF output register invalid: %d\n", ArgReg (ins->out_args[0]));
			return 0;
		}
		break;
	case INS_LEA:
	case INS_ADD:
	case INS_AND:
	case INS_OR:
	case INS_INTO:
	case INS_SETLABEL:
	case INS_PUSH:
	case INS_POP:
	case INS_RET:
	case INS_CALL:
	case INS_KCALL:
	case INS_JUMP:
	case INS_SWAP:
	case INS_DEC:
	case INS_INC:
	case INS_SUB:
	case INS_CWDE:
	case INS_XOR:
	case INS_MUL:
	case INS_RDTSC:
	case INS_LOADLABDIFF:
	case INS_CDQ:
	case INS_DIV:
	case INS_SHR:
	case INS_SHL:
	case INS_MOVEB:
	case INS_NOT:
	case INS_CONSTLABDIFF:
	case INS_CONSTLABADDR:
	case INS_REPMOVEB:
	case INS_MOVEZEXT8TO32:
	case INS_RCR:
	case INS_RCL:
	case INS_ROR:
	case INS_ROL:
	case INS_ADC:
	case INS_SBB:
	case INS_UMUL:
	case INS_UDIV:
	case INS_SHLD:
	case INS_SHRD:
	case INS_FSTCW:
	case INS_FLDCW:
	case INS_WAIT:
	case INS_FSTP:
	case INS_ANNO:
	case INS_FILD32:
	case INS_FXCH:
	case INS_FLD:
	case INS_FILD64:
	case INS_FLD32:
	case INS_FLD64:
	case INS_FLD80:
	case INS_FADD32:
	case INS_FADD64:
	case INS_FMUL32:
	case INS_FMUL64:
	case INS_FST32:
	case INS_FST64:
	case INS_FST80:
	case INS_FIST32:
	case INS_FIST64:
	case INS_FLD1:
	case INS_FLDL2T:
	case INS_FLDL2E:
	case INS_FLDPI:
	case INS_FLDLG2:
	case INS_FLDLN2:
	case INS_FLDZ:
	case INS_FADD:
	case INS_FSUB:
	case INS_FMUL:
	case INS_FDIV:
	case INS_FTST:
	case INS_FXAM:
	case INS_FSTSW:
	case INS_FUCOM:
	case INS_FUCOMP:
	case INS_FUCOMPP:
	case INS_FCOM:
	case INS_FCOMP:
	case INS_FCOMPP:
	case INS_FRNDINT:
	case INS_FSQRT:
	case INS_FABS:
	case INS_FCHS:
	case INS_FSCALE:
	case INS_FPREM1:
	case INS_SETFLABEL:
	case INS_MOVESEXT16TO32:
	case INS_LAHF:
	case INS_REPMOVEL:
	case INS_CMOVE:
	case INS_PJUMP:
	case INS_INB:
	case INS_OUTB:
	case INS_CCALL:
		break;
	case INS_START_REG:
	case INS_END_REG:
	case INS_CONSTRAIN_REG:
	case INS_UNCONSTRAIN_REG:
	case INS_FREE_REG:
	case INS_START_CC:
	case INS_END_CC:
	case INS_SOURCELINE:
	case INS_CLEANUP:
		break;
	default:
		fprintf (stderr, "%s: unknown instruction %d in rtl_validate_instr\n", progname, ins->type);
		return 0;
		break;
	}
	return 1;
}
/*}}}*/
/*{{{  static int rtl_prevalidate_instr_sparc (ins_chain *ins)*/
static int rtl_prevalidate_instr_sparc (ins_chain *ins)
{
	int redo = 0;

	do {
		redo = 0;

		switch (ins->type) {
			/*{{{  INS_LEA*/
		case INS_LEA:
			if (ArgMode (ins->out_args[0]) != ARG_REG) {
				fprintf (stderr, "%s: error: (prevalidate sparc): LEA into non-register.\n", progname);
			} else {
				ins_arg *tmparg;

				switch (ArgMode (ins->in_args[0])) {
				case ARG_REGIND:
					/* turn:
					 *	lea <regind>, <reg>
					 * into:
					 *	add <reg[ind]>, <disp>, <reg>
					 */
					ins->type = INS_ADD;

					ins->in_args[1] = compose_ins_arg (ARG_CONST, (ArgHasDisp (ins->in_args[0]) ? ArgDisp (ins->in_args[0]) : 0));
					ins->in_args[0]->flags = ARG_REG;

					/* swap args 0 and 1 */
					tmparg = ins->in_args[0];
					ins->in_args[0] = ins->in_args[1];
					ins->in_args[1] = tmparg;
					break;
				case ARG_LABEL:
				case ARG_NAMEDLABEL:
				case ARG_FLABEL:
				case ARG_BLABEL:
					/* turn:
					 *	lea <label>, <reg>
					 * into:
					 *	move <const:label>, <reg>
					 *	add <reg>, <disp>, <reg>
					 */
					ins->type = INS_MOVE;
					ins->in_args[0]->flags |= ARG_ISCONST;

					if (ArgHasDisp (ins->in_args[0])) {
						ins_chain *newins = compose_ins (INS_ADD, 2, 1, ARG_CONST, ArgDisp (ins->in_args[0]), ARG_REG, ArgReg (ins->out_args[0]),
								ARG_REG, ArgReg (ins->out_args[0]));

						ins->in_args[0]->flags &= ~ARG_DISP;

						rtl_insert_instr_after (newins, ins);
					}
					break;
				}
			}
			break;
			/*}}}*/
			/*{{{  INS_MOVEB*/
		case INS_MOVEB:
			/* can only do this to/from a register */
			{
				ins_arg *src = ins->in_args[0];
				ins_arg *dest = ins->out_args[0];

				if ((ArgMode (src) == ARG_CONST) && (ArgMode (dest) == ARG_REGIND)) {
					/* turn:
					 *	moveb	<const>, <regind>
					 * into:
					 *	move	<const>, %l4
					 *	moveb	%l4, <regind>
					 */
					ins_chain *newins = compose_ins (INS_MOVEB, 1, 1, ARG_REG, REG_ALT_L4, ARG_REGIND, ArgReg (dest));

					if (ArgHasDisp (dest)) {
						newins->out_args[0]->flags |= ARG_DISP;
						newins->out_args[0]->disp = ArgDisp (dest);
					}
					ins->type = INS_MOVE;
					ins->out_args[0]->flags = ARG_REG;
					ins->out_args[0]->regconst = REG_ALT_L4;

					rtl_insert_instr_after (newins, ins);
				} else if ((ArgMode (src) == ARG_CONST) && (ArgMode (dest) == ARG_NAMEDLABEL)) {
					/* turn:
					 *	moveb	<const>, <namedlabel>
					 * into:
					 *	move	<const:namedlabel>, %l4
					 *	move	<const>, %l5
					 *	moveb	%l5, [%l4]
					 */
					ins_chain *newins1 = compose_ins (INS_MOVE, 1, 1, ARG_CONST, ArgConst (src), ARG_REG, REG_ALT_L5);
					ins_chain *newins2 = compose_ins (INS_MOVEB, 1, 1, ARG_REG, REG_ALT_L5, ARG_REGIND, REG_ALT_L4);
					ins_arg *tmp;

					ins->type = INS_MOVE;
					tmp = ins->in_args[0];
					ins->in_args[0] = ins->out_args[0];
					ins->out_args[0] = tmp;

					ins->out_args[0]->flags = ARG_REG;
					ins->out_args[0]->regconst = REG_ALT_L4;
					ins->in_args[0]->flags |= ARG_ISCONST;

					rtl_insert_instr_after (newins1, ins);
					rtl_insert_instr_after (newins2, newins1);
				}
			}
			break;
			/*}}}*/
			/*{{{  INS_MOVE*/
		case INS_MOVE:
			/* tidy named operands that we can't handle directly */
			{
				ins_arg *src = ins->in_args[0];
				ins_arg *dest = ins->out_args[0];

				if ((ArgMode (dest) == ARG_NAMEDLABEL) && (ArgMode (src) == ARG_CONST)) {
					/*
					 * turn:
					 *	mov <const>, <name>
					 * into:
					 *	mov <const:name>, %l4
					 *	mov const, %l5
					 *	mov %l5, [%l4]
					 */
					ins_chain *newins = compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, src->regconst, ARG_REG, REG_ALT_L5);
					ins_chain *newins2 = compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_L5, ARG_REGIND, REG_ALT_L4);

					src->flags = ARG_NAMEDLABEL | ARG_ISCONST;
					src->regconst = (int)(dest->regconst);
					dest->flags = ARG_REG;
					dest->regconst = REG_ALT_L4;

					rtl_insert_instr_after (newins, ins);
					rtl_insert_instr_after (newins2, newins);
				} else if ((ArgMode (dest) == ARG_NAMEDLABEL) && (ArgMode (src) == ARG_REG)) {
					/* turn:
					 *	move <reg>, <name>
					 * into:
					 *	move <const:name>, %l4
					 *	move <reg>, [%l4]
					 */
					ins_chain *newins = compose_ins (INS_MOVE, 1, 1, ARG_REG, ArgReg (src), ARG_REGIND, REG_ALT_L4);

					src->flags = ARG_NAMEDLABEL | ARG_ISCONST;
					src->regconst = (int)(dest->regconst);
					dest->flags = ARG_REG;
					dest->regconst = REG_ALT_L4;

					rtl_insert_instr_after (newins, ins);
				} else if ((ArgMode (dest) == ARG_REG) && (ArgMode (src) == ARG_NAMEDLABEL)) {
					if (!(src->flags & ARG_ISCONST)) {
						/* loading contents of name */
						/*
						 * turn:
						 *	mov <name>, <reg>
						 * into:
						 *	mov <const:name>, %l4
						 *	mov [%l4], reg
						 */
						ins_chain *newins = compose_ins (INS_MOVE, 1, 1, ARG_REGIND, REG_ALT_L4, ARG_REG, dest->regconst);

						src->flags |= ARG_ISCONST;
						dest->regconst = REG_ALT_L4;
						dest->flags = ARG_REG;

						rtl_insert_instr_after (newins, ins);
					}
				} else if ((ArgMode (dest) == ARG_REGIND) && (ArgMode (src) == ARG_CONST)) {
					/* loading constant into memory */
					/*
					 * turn:
					 *	mov <const>, <regind>
					 * into:
					 *	mov <const>, %l4
					 *	mov %l4, <regind>
					 */
					ins_chain *newins = compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_L4, ARG_REGIND, dest->regconst);

					if (dest->flags & ARG_DISP) {
						newins->out_args[0]->flags |= ARG_DISP;
						newins->out_args[0]->disp = dest->disp;
					}
					dest->flags = ARG_REG;
					dest->regconst = REG_ALT_L4;

					rtl_insert_instr_after (newins, ins);
				} else if ((ArgMode (dest) == ARG_REGIND) &&
						((ArgMode (src) == ARG_NAMEDLABEL) ||
						 (ArgMode (src) == ARG_LABEL) ||
						 (ArgMode (src) == ARG_INSLABEL) ||
						 (ArgMode (src) == ARG_FLABEL) ||
						 (ArgMode (src) == ARG_BLABEL))) {
					if (src->flags & ARG_ISCONST) {
						/* loading name address into memory */
						/* turn:
						 *	mov <const:name>, <regind>
						 * into:
						 *	mov <const:name>, %l4
						 *	mov %l4, <regind>
						 */
						ins_chain *newins = compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_L4, ARG_REGIND, dest->regconst);

						if (dest->flags & ARG_DISP) {
							newins->out_args[0]->flags |= ARG_DISP;
							newins->out_args[0]->disp = dest->disp;
						}
						dest->flags = ARG_REG;
						dest->regconst = REG_ALT_L4;

						rtl_insert_instr_after (newins, ins);
					}
				}
			}
			break;
			/*}}}*/
			/*{{{  INS_ADD, INS_SUB*/
		case INS_ADD:
		case INS_SUB:
			if ((ArgMode (ins->in_args[0]) != ARG_REG) && (ArgMode (ins->in_args[0]) != ARG_CONST)) {
				/* turn:
				 *	add/sub <src2>, <src1>, <dest>
				 * into:
				 * 	move <src1>, %l4
				 * 	add/sub <src2>, %l4, <dest>
				 */
				ins_chain *newins = compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_L4, ARG_REG, REG_ALT_L4);

				newins->in_args[0]->flags = ins->in_args[0]->flags;
				newins->in_args[0]->regconst = ins->in_args[0]->regconst;
				newins->in_args[0]->disp = ins->in_args[0]->disp;

				ins->in_args[0]->flags = ARG_REG;
				ins->in_args[0]->regconst = REG_ALT_L4;
				ins->in_args[0]->disp = 0;

				rtl_insert_instr_before (newins, ins);
			} else {
				if (ArgMode (ins->in_args[1]) != ARG_REG) {
					/* turn:
					 *	add/sub <src2>, <src1>, <dest>
					 * into:
					 *	move <src2>, %l4
					 *	add/sub %l4, <src1>, <dest>
					 */
					ins_chain *newins = compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_L4, ARG_REG, REG_ALT_L4);

					newins->in_args[0]->flags = ins->in_args[1]->flags;
					newins->in_args[0]->regconst = ins->in_args[1]->regconst;
					newins->in_args[0]->disp = ins->in_args[1]->disp;

					ins->in_args[1]->flags = ARG_REG;
					ins->in_args[1]->regconst = REG_ALT_L4;
					ins->in_args[1]->disp = 0;

					rtl_insert_instr_before (newins, ins);
				}
				if (ArgMode (ins->out_args[0]) != ARG_REG) {
					/* turn:
					 *	add/sub <src2>, <src1>, <dest>
					 * into:
					 *	add/sub <src2>, <src1>, %l4
					 *	move %l4, <dest>
					 */
					ins_chain *newins = compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_L4, ARG_REG, REG_ALT_L4);

					newins->out_args[0]->flags = ins->out_args[0]->flags;
					newins->out_args[0]->regconst = ins->out_args[0]->regconst;
					newins->out_args[0]->disp = ins->out_args[0]->disp;

					ins->out_args[0]->flags = ARG_REG;
					ins->out_args[0]->regconst = REG_ALT_L4;
					ins->out_args[0]->disp = 0;

					rtl_insert_instr_after (newins, ins);
				}
			}
			break;
			/*}}}*/
			/*{{{  INS_MOVEZEXT8TO32*/
		case INS_MOVEZEXT8TO32:
			/* move 8 to 32-bit with zero extension */
			/* turn:
			 *	movzbl <src>, <dest>
			 * into:
			 *	moveb <src>, <dest>
			 *	and <dest>, $0xff, <dest>
			 */
			{
				ins_chain *newins = compose_ins (INS_AND, 2, 1, ARG_CONST | ARG_ISCONST, 0xff, ARG_REG, REG_ALT_L4, ARG_REG, REG_ALT_L4);

				newins->in_args[1]->flags = ins->out_args[0]->flags;
				newins->in_args[1]->regconst = ins->out_args[0]->regconst;
				newins->in_args[1]->disp = ins->out_args[0]->disp;
				newins->out_args[0]->flags = ins->out_args[0]->flags;
				newins->out_args[0]->regconst = ins->out_args[0]->regconst;
				newins->out_args[0]->disp = ins->out_args[0]->disp;

				ins->type = INS_MOVEB;

				rtl_insert_instr_after (newins, ins);

				/* and re-do to transform the moveb if needed */
				redo = 1;
			}
			break;
			/*}}}*/
			/*{{{  INS_CALL*/
		case INS_CALL:
			if ((ArgMode (ins->in_args[0]) == ARG_NAMEDLABEL) ||
					(ArgMode (ins->in_args[0]) == ARG_LABEL)) {
				/* can't call on Sparc, turn:
				 *	call <name/label>
				 * into:
				 *	sub %sp, 4, %sp
				 *	mov <const:flab 6>, %l4
				 *	mov %l4, [%sp]
				 *	mov <const:name/label>, %l4
				 *	jmp %l4
				 *   6:
				 */
				ins_chain *newins1 = compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 6, ARG_REG, REG_ALT_L4);
				ins_chain *newins2 = compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_L4, ARG_REGIND, REG_SPTR);
				ins_chain *newins3 = compose_ins (INS_MOVE, 1, 1, ArgMode (ins->in_args[0]) | ARG_ISCONST, ins->in_args[0]->regconst,
						ARG_REG, REG_ALT_L4);
				ins_chain *newins4 = compose_ins (INS_JUMP, 1, 0, ARG_REG, REG_ALT_L4);
				ins_chain *newins5 = compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 6);

				/* change the "call" into a "sub" */
				ins->type = INS_SUB;
				ins->out_args[0] = new_ins_arg ();
				ins->in_args[1] = new_ins_arg ();

				ins->in_args[0]->flags = ARG_CONST | ARG_ISCONST;
				ins->in_args[0]->regconst = 4;
				ins->in_args[1]->flags = ARG_REG;
				ins->in_args[1]->regconst = REG_SPTR;
				ins->out_args[0]->flags = ARG_REG;
				ins->out_args[0]->regconst = REG_SPTR;

				rtl_insert_instr_after (newins1, ins);
				rtl_insert_instr_after (newins2, newins1);
				rtl_insert_instr_after (newins3, newins2);
				rtl_insert_instr_after (newins4, newins3);
				rtl_insert_instr_after (newins5, newins4);
			}
			break;
			/*}}}*/
			/*{{{  INS_INC, INS_DEC*/
		case INS_INC:
		case INS_DEC:
			if (ArgMode (ins->in_args[0]) == ARG_REG) {
				/* turn:
				 *	inc/dec <reg>
				 * into:
				 *	add/sub reg, 1, reg
				 */
				ins->in_args[1] = new_ins_arg ();
				ins->in_args[1]->flags = ins->in_args[0]->flags;
				ins->in_args[1]->regconst = ins->in_args[0]->regconst;
				ins->in_args[0]->flags = ARG_CONST | ARG_ISCONST;
				ins->in_args[0]->regconst = 1;

				if (ins->type == INS_INC) {
					ins->type = INS_ADD;
				} else {
					ins->type = INS_SUB;
				}
			} else if (ArgMode (ins->in_args[0]) == ARG_REGIND) {
				/* turn:
				 *	inc/dec <regind>
				 * into:
				 *	mov regind, %l4
				 *	add/sub %l4, 1, %l4
				 *	mov %l4, regind
				 */
				ins_chain *newins1 = compose_ins ((ins->type == INS_INC) ? INS_ADD : INS_SUB, 2, 1, ARG_CONST, 1, ARG_REG, REG_ALT_L4, ARG_REG, REG_ALT_L4);
				ins_chain *newins2 = compose_ins (INS_MOVE, 1 ,1, ARG_REG, REG_ALT_L4, ARG_REGIND, ArgReg (ins->in_args[0]));

				if (ArgHasDisp (ins->in_args[0])) {
					newins2->out_args[0]->flags |= ARG_DISP;
					newins2->out_args[0]->disp = ArgDisp (ins->in_args[0]);
				}
				ins->type = INS_MOVE;
				ins->out_args[0]->flags = ARG_REG;
				ins->out_args[0]->regconst = REG_ALT_L4;

				rtl_insert_instr_after (newins1, ins);
				rtl_insert_instr_after (newins2, newins1);
			}
			break;
			/*}}}*/
			/*{{{  INS_JUMP*/
		case INS_JUMP:
			if ((ArgMode (ins->in_args[0]) == ARG_REGIND) && (ins->in_args[0]->flags & ARG_IND)) {
				/* turn:
				 *	jump	*<regind>
				 * into:
				 *	mov	<regind>, %l4
				 *	jump	%l4
				 */
				ins_chain *newins = compose_ins (INS_JUMP, 1, 0, ARG_REG, REG_ALT_L4);

				ins->type = INS_MOVE;
				ins->in_args[0]->flags &= ~ARG_IND;
				ins->out_args[0] = compose_ins_arg (ARG_REG, REG_ALT_L4);

				rtl_insert_instr_after (newins, ins);
			} else if (ArgMode (ins->in_args[0]) == ARG_NAMEDLABEL) {
				/* turn:
				 *	jump	<label>
				 * into:
				 * 	mov	<const:label>, %l4
				 * 	jump	%l4
				 */
				ins_chain *newins = compose_ins (INS_JUMP, 1, 0, ARG_REG, REG_ALT_L4);

				ins->type = INS_MOVE;
				ins->in_args[0]->flags |= ARG_ISCONST;
				ins->out_args[0] = compose_ins_arg (ARG_REG, REG_ALT_L4);

				rtl_insert_instr_after (newins, ins);
			} else if (ArgMode (ins->in_args[0]) == ARG_LABEL) {
				/* turn:
				 *	jump	<label>
				 * into:
				 *	ba	<label>
				 */
				ins->type = INS_CJUMP;
				ins->in_args[1] = ins->in_args[0];
				ins->in_args[0] = compose_ins_arg (ARG_COND, CC_ALWAYS);
			}
			break;
			/*}}}*/
			/*{{{  INS_PUSH, INS_POP*/
		case INS_PUSH:
		case INS_POP:
			{
				ins_chain *tmp_ins;

				fprintf (stderr, "%s: warning: PUSH/POP not really supported on Sparc..\n", progname);
				tmp_ins = compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("unsupported PUSH/POP!"));

				rtl_insert_instr_before (tmp_ins, ins);
			}
			{
				int ispush = (ins->type == INS_PUSH);

				if (ispush && rtl_next_instr (ins) && (rtl_next_instr (ins)->type == INS_RET)) {
					/* push/return optimisation, turn:
					 *	push	<arg>
					 *	ret
					 * into:
					 *	move	<arg>, %l4
					 *	jump	%l4
					 */
					ins_chain *retins = rtl_next_instr (ins);

					ins->type = INS_MOVE;
					ins->out_args[0] = compose_ins_arg (ARG_REG, REG_ALT_L4);

					retins->type = INS_JUMP;
					retins->in_args[0] = compose_ins_arg (ARG_REG, REG_ALT_L4);

				} else if (ispush) {
					/* push transform, turn:
					 *	push <arg>
					 * into:
					 *	sub	%sp, 4, %sp
					 *	move	<arg>, %l4
					 *	move	%l4, [%sp]
					 */
					ins_chain *newins1 = compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_L4, ARG_REG, REG_ALT_L4);
					ins_chain *newins2 = compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_L4, ARG_REGIND, REG_SPTR);

					newins1->in_args[0]->flags = ins->in_args[0]->flags;
					newins1->in_args[0]->regconst = ins->in_args[0]->regconst;
					newins1->in_args[0]->disp = ins->in_args[0]->disp;

					ins->type = INS_SUB;
					ins->in_args[0]->flags = ARG_CONST | ARG_ISCONST;
					ins->in_args[0]->regconst = 4;
					ins->in_args[1] = compose_ins_arg (ARG_REG, REG_SPTR);
					ins->out_args[0] = compose_ins_arg (ARG_REG, REG_SPTR);

					rtl_insert_instr_after (newins1, ins);
					rtl_insert_instr_after (newins2, newins1);
				} else {
					/* pop transform, turn:
					 *	pop <arg>
					 * into:
					 *	move	[%sp], %l4
					 *	move	%l4, <arg>
					 *	add	%sp, 4, %sp
					 */
					ins_chain *newins1 = compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_L4, ARG_REG, REG_ALT_L4);
					ins_chain *newins2 = compose_ins (INS_ADD, 2, 1, ARG_CONST, 4, ARG_REG, REG_SPTR, ARG_REG, REG_SPTR);

					newins1->out_args[0]->flags = ins->out_args[0]->flags;
					newins1->out_args[0]->regconst = ins->out_args[0]->regconst;
					newins1->out_args[0]->disp = ins->out_args[0]->disp;

					ins->type = INS_MOVE;
					ins->in_args[0] = compose_ins_arg (ARG_REGIND, REG_SPTR);
					ins->out_args[0]->flags = ARG_REG;
					ins->out_args[0]->regconst = REG_ALT_L4;

					rtl_insert_instr_after (newins1, ins);
					rtl_insert_instr_after (newins2, newins1);
				}
			}
			break;
			/*}}}*/
			/*{{{  INS_CMP*/
		case INS_CMP:
			if (ArgMode (ins->in_args[0]) != ARG_REG) {
				/* turn:
				 *	cmp <non-reg>, <arg>
				 * into:
				 *	move <non-reg>, %l4
				 *	cmp %l4, <arg>
				 */
				ins_chain *newins = compose_ins (INS_CMP, 2, 1, ARG_REG, REG_ALT_L4, ARG_REG, REG_ALT_L4, ARG_REG | ARG_IMP, REG_CC);
				ins_arg *tmparg;

				tmparg = ins->in_args[1];
				ins->type = INS_MOVE;
				ins->in_args[1] = NULL;
				ins->out_args[0] = newins->in_args[1];
				newins->in_args[1] = tmparg;

				rtl_insert_instr_after (newins, ins);

				/* update ins to point at the comparison again */
				ins = newins;
			}
			if (ArgMode (ins->in_args[1]) != ARG_REG) {
				/* turn:
				 *	cmp <arg>, <non-reg>
				 * into:
				 *	move <non-reg>, %l5
				 *	cmp <arg>, %l5
				 */
				ins_chain *newins = compose_ins (INS_CMP, 2, 1, ARG_REG, REG_ALT_L5, ARG_REG, REG_ALT_L5, ARG_REG | ARG_IMP, REG_CC);
				ins_arg *tmparg;

				ins->type = INS_MOVE;
				tmparg = ins->in_args[0];
				ins->in_args[0] = ins->in_args[1];
				ins->in_args[1] = NULL;
				ins->out_args[0] = newins->in_args[0];
				newins->in_args[0] = tmparg;

				rtl_insert_instr_after (newins, ins);
			}
			break;
			/*}}}*/
			/*{{{  INS_CJUMP*/
		case INS_CJUMP:
#if 0
			if (ArgMode (ins->in_args[1]) == ARG_NAMEDLABEL) {
				/* turn:
				 *	cjump <cc>, <name>
				 * into:
				 *	cjump <!cc>, 7f
				 *	jump <name>
				 *	7:
				 */
				ins_chain *newins1 = compose_ins (INS_JUMP, 1, 0, ARG_NAMEDLABEL, ArgName (ins->in_args[1]));
				ins_chain *newins2 = compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 7);

				ins->in_args[1]->flags = ARG_FLABEL;
				ins->in_args[1]->regconst = 7;
				ins->in_args[1]->disp = 0;
				ins->in_args[0]->regconst ^= 1;

				rtl_insert_instr_after (newins1, ins);
				rtl_insert_instr_after (newins2, newins1);
			}
#endif
			break;
			/*}}}*/
		}
	} while (redo);

	return 1;
}
/*}}}*/
/*{{{  arch_t *init_arch_sparc (int mclass)*/
/*
 *	arch_t *init_arch_sparc (int mclass)
 *	initialises and returns architecture definition
 */
arch_t *init_arch_sparc (int mclass)
{
	arch_t *arch = (arch_t *)smalloc (sizeof (arch_t));

	/* common */
	arch->archname = string_dup ("sparc");

	/* kernel calls */
	arch->compose_kcall = compose_kcall_sparc;
	arch->compose_deadlock_kcall = compose_deadlock_kcall_sparc;

	/* other stuff */
	arch->compose_pre_enbc = compose_pre_enbc_sparc;
	arch->compose_pre_enbt = compose_pre_enbt_sparc;

	/* inlining */
	arch->compose_inline_ldtimer = NULL;
	arch->compose_inline_tin = NULL;
	arch->compose_inline_quick_reschedule = compose_inline_quick_reschedule_sparc;
	arch->compose_inline_full_reschedule = NULL;
	arch->compose_inline_in = NULL;
	arch->compose_inline_in_2 = NULL;
	arch->compose_inline_min = NULL;
	arch->compose_inline_out = NULL;
	arch->compose_inline_out_2 = NULL;
	arch->compose_inline_mout = NULL;
	arch->compose_inline_enbc = NULL;
	arch->compose_inline_disc = NULL;
	arch->compose_inline_altwt = NULL;
	arch->compose_inline_stlx = compose_inline_stlx_sparc;
	arch->compose_inline_malloc = NULL;
	arch->compose_inline_startp = NULL;
	arch->compose_inline_endp = NULL;
	arch->compose_inline_runp = NULL;
	arch->compose_inline_stopp = NULL;

	/* debugging */
	arch->compose_debug_insert = compose_debug_insert_sparc;
	arch->compose_debug_procnames = compose_debug_procnames_sparc;
	arch->compose_debug_filenames = compose_debug_filenames_sparc;
	arch->compose_debug_zero_div = compose_debug_zero_div_sparc;
	arch->compose_debug_floaterr = compose_debug_floaterr_sparc;
	arch->compose_debug_overflow = compose_debug_overflow_sparc;
	arch->compose_debug_rangestop = compose_debug_rangestop_sparc;
	arch->compose_debug_seterr = compose_debug_seterr_sparc;
	arch->compose_overflow_jumpcode = compose_overflow_jumpcode_sparc;
	arch->compose_floaterr_jumpcode = compose_floaterr_jumpcode_sparc;
	arch->compose_rangestop_jumpcode = compose_rangestop_jumpcode_sparc;

	arch->compose_debug_deadlock_set = compose_debug_deadlock_set_sparc;

	/* code-gen */
	arch->compose_divcheck_zero = compose_divcheck_zero_sparc;
	arch->compose_divcheck_zero_simple = compose_divcheck_zero_simple_sparc;
	arch->compose_division = compose_division_sparc;
	arch->compose_remainder = compose_remainder_sparc;
	arch->compose_iospace_loadbyte = NULL;
	arch->compose_iospace_storebyte = NULL;
	arch->compose_iospace_loadword = NULL;
	arch->compose_iospace_storeword = NULL;
	arch->compose_iospace_read = NULL;
	arch->compose_iospace_write = NULL;
	arch->compose_move = compose_move_sparc;
	arch->compose_move_loadptrs = NULL;
	arch->compose_shift = compose_shift_sparc;

	arch->compose_widenshort = compose_widenshort_sparc;
	arch->compose_widenword = compose_widenword_sparc;
	arch->compose_longop = compose_longop_sparc;
	arch->compose_fpop = compose_fpop_sparc;
	arch->compose_external_ccall = compose_external_ccall_sparc;
	arch->compose_bcall = compose_bcall_sparc;

	arch->compose_entry_prolog = compose_entry_prolog_sparc;
	arch->compose_rmox_entry_prolog = NULL;
	arch->compose_fp_set_fround = compose_fp_set_fround_sparc;
	arch->compose_fp_init = compose_fp_init_sparc;
	arch->compose_reset_fregs = compose_reset_fregs_sparc;

	arch->compose_return = compose_return_sparc;
	arch->compose_nreturn = NULL;
	arch->compose_funcresults = compose_funcresults_sparc;

	/* register allocation */
	arch->regcolour_special_to_real = regcolour_special_to_real_sparc;
	arch->regcolour_rmax = RMAX_SPARC;
	arch->regcolour_nodemax = NODEMAX_SPARC;
	arch->regcolour_get_regs = regcolour_get_regs_sparc;
	arch->regcolour_fp_regs = NULL;

	/* output generation */
	arch->code_to_asm = dump_asmsparc;
	arch->code_to_asm_stream = dump_asmsparc_stream;

	/* RTL stuff */
	arch->rtl_validate_instr = rtl_validate_instr_sparc;
	arch->rtl_prevalidate_instr = rtl_prevalidate_instr_sparc;
	arch->get_register_name = get_register_name_sparc;

	arch->int_options = INTOPT_NOREGSQUEEZE;
	return arch;
}
/*}}}*/


