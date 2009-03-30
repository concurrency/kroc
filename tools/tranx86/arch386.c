/*
 *	arch386.c -- 386 architecture stuff
 *	Copyright (C) 2002-2004 Fred Barnes <frmb@ukc.ac.uk>
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
#include "intel.h"
#include "tstate.h"
#include "stubtable.h"
#include "archdef.h"
#include "rtlops.h"
#include "etcdump.h"
#include "etcrtl.h"
#include "asm386.h"
#include "kif.h"
#include "machine.h"

#include <stddef.h>
#ifndef offsetof
#define offsetof(t,f) ((int) (&((((t *)(0))->f))))
#endif

#ifndef EXIT_FAILURE
	#define EXIT_FAILURE 1
#endif  /* !EXIT_FAILURE */

#define RMAX_I386 4
#define NODEMAX_I386 32
#define KIFACE_TABLEOFFS_I386 192

static const int xregs[3] = { REG_EAX, REG_EDX, REG_ECX };

static void set_implied_inputs (ins_chain *instr, int n_inputs, int *i_regs);
static void set_implied_outputs (ins_chain *instr, int n_outputs, int *o_regs);
static void compose_fp_set_fround_i386 (tstate *ts, int mode);

static char *Wptr_name;
static char *Fptr_name;
static char *Bptr_name;
static char *PPriority_name;

/*{{{   static ins_chain *compose_kjump_i386 (tstate *ts, const int type, const int cond, const kif_entrytype *entry)*/
/*
 * 	composes a jump or call instruction via the calltable
 */
static ins_chain *compose_kjump_i386 (tstate *ts, const int type, const int cond, const kif_entrytype *entry)
{
	return compose_ins (INS_CALL, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SCHED, offsetof(ccsp_sched_t, calltable[entry->call_offset]));
	#if 0
	if (type == INS_CJUMP) {
		return compose_ins (type, 2, 0, ARG_COND, cond, ARG_NAMEDLABEL, string_dup (entry->entrypoint));
	} else if (options.kernel_interface & KRNLIFACE_MP) {
		return compose_ins (INS_CALL, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SCHED, entry->call_offset << 2);
	} else {
		return compose_ins (type, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (entry->call_offset + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2);
	}
	#endif
}
/*}}}*/

/*{{{  static void compose_kcall_i386 (tstate *ts, int call, int regs_in, int regs_out)*/
/*
 *	void compose_kcall_i386 (tstate *ts, int call, int regs_in, int regs_out)
 *	creates a kernel-call (constraining registers appropriately)
 */
static void compose_kcall_i386 (tstate *ts, int call, int regs_in, int regs_out)
{
	kif_entrytype *entry = kif_entry (call);
	int to_preserve, r_in, r_out;
	int i, cregs[3], oregs[3];
	/* int tmp_reg; */
	ins_chain *call_ins;
#ifdef USER_DEFINED_CHANNELS
	int target_reg = -1;
	int target_call = -1;
#endif	/* USER_DEFINED_CHANNELS */

	/*{{{  check registers in/out*/
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
	/*}}}*/

#if 0
fprintf (stderr, "compose_kcall_i386: regs_in = %d, regs_out = %d, r_in = %d, r_out = %d, [areg = %d, breg = %d, creg = %d]\n", regs_in, regs_out, r_in, r_out,
		ts->stack->old_a_reg, ts->stack->old_b_reg, ts->stack->old_c_reg);
#endif
	/*{{{  constrain operands to registers (or push onto stack)*/
	cregs[0] = ts->stack->old_a_reg;
	cregs[1] = ts->stack->old_b_reg;
	cregs[2] = ts->stack->old_c_reg;
	for (i = 0; i < r_in; i++) {
		if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
			if (i > 0) {
				switch (constmap_typeof (cregs[i])) {
				default:
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, cregs[i], ARG_REGIND | ARG_DISP, REG_SCHED, offsetof(ccsp_sched_t, cparam[(i - 1)])));
					break;
				case VALUE_CONST:
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, constmap_regconst (cregs[i]), ARG_REGIND | ARG_DISP, REG_SCHED, offsetof(ccsp_sched_t, cparam[(i - 1)])));
					break;
				case VALUE_LABADDR:
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, constmap_regconst (cregs[i]), ARG_REGIND | ARG_DISP, REG_SCHED, offsetof(ccsp_sched_t, cparam[(i - 1)])));
					break;
				/*
				case VALUE_LOCAL:
					tmp_reg = tstack_newreg (ts->stack);
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, (constmap_regconst (cregs[i])) << WSH, ARG_REG, tmp_reg));
					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_SCHED, (i - 1) << WSH));
					break;
				*/
				}
			} else {
				add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, cregs[i], ARG_REG, xregs[i]));
			}
		} else if (options.kernel_interface & KRNLIFACE_MESH) {
			fprintf (stderr, "%s: warning: MESH kernel interface not supported yet (ungenerated kernel call %d)\n", progname, call);
		} else if (options.kernel_interface & KRNLIFACE_CSPLINUX) {
			fprintf (stderr, "%s: warning: CSP/Linux interface not supported yet (ungenerated kernel call %d)\n", progname, call);
		}
	}
	/*}}}*/
	
	if (r_in < r_out) {
		switch (r_out - r_in) {
			case 3: cregs[r_in + 2] = ts->stack->c_reg;
			case 2: cregs[r_in + 1] = ts->stack->b_reg;
			case 1: cregs[r_in + 0] = ts->stack->a_reg;
		}

		for (i = r_in; i < 1; i++) {
			add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, cregs[i], ARG_REG, xregs[i]));
		}
	}

	/* generate call */
	call_ins = NULL;		/* used for adding implied regs later */
	if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
		if (options.annotate_output) {
			char sbuf[128];

			sprintf (sbuf, "CCSP [%s]", entry->entrypoint);
			add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuf)));
		}
		call_ins = compose_kjump_i386 (ts, INS_CALL, 0, entry);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_SCHED, ARG_REG, xregs[1]));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, xregs[2]));
		add_to_ins_chain (call_ins);
		if (call_ins) {
			set_implied_inputs (call_ins, r_in > r_out ? r_in : r_out, cregs);
		}
	} else if (options.kernel_interface & KRNLIFACE_MESH) {
		/* unsupported as yet */
		fprintf (stderr, "%s: warning: MESH kernel interface not supported yet (ungenerated kernel call %d)\n", progname, call);
	} else if (options.kernel_interface & KRNLIFACE_CSPLINUX) {
		/* unsupported as yet */
		fprintf (stderr, "%s: warning: CSP/Linux kernel interface not supported yet (ungenerated kernel call %d)\n", progname, call);
	}
	ts->stack_drift = 0;

	/*{{{  unconstrain registers*/
	if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
		for (i = 0; i < (r_in >= r_out ? r_in : 1); i++) {
			add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, cregs[i]));
		}
	}
	/*}}}*/

	/*{{{  clean up and collect results */
	if (regs_out > 0) {
		tstack_undefine (ts->stack);
		constmap_clearall ();
		ts->stack->must_set_cmp_flags = 1;
		ts->stack->ts_depth = r_out;
		for (i = 0; i < r_out; i++) {
			oregs[i] = cregs[i];
			if (i >= 1) {
				add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_SCHED, offsetof(ccsp_sched_t, cparam[(i - 1)]), ARG_REG, oregs[i]));
			}
		}
		for (i = ts->stack->ts_depth; i < 3; i++) {
			oregs[i] = REG_UNDEFINED;
		}
		ts->stack->a_reg = oregs[0];
		ts->stack->b_reg = oregs[1];
		ts->stack->c_reg = oregs[2];
		if (call_ins) {
			set_implied_outputs (call_ins, r_out, oregs);
		}
		#if 0
		/* constrain into registers */
		for (i = 0; i < r_out; i++) {
			if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
				add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, oregs[i], ARG_REG, xregs[i]));
				add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, oregs[i]));
			} else if (options.kernel_interface & KRNLIFACE_MESH) {
				fprintf (stderr, "%s: warning: MESH kernel interface not supported yet (ungenerated kernel output from call %d)\n", progname, call);
			}
		}
		#endif
	}
	/*}}}*/

	return;
}
/*}}}*/
/*{{{  static void compose_deadlock_kcall_i386 (tstate *ts, int call, int regs_in, int regs_out)*/
/*
 *	void compose_deadlock_kcall_i386 (tstate *ts, int call, int regs_in, int regs_out)
 *	creates a kernel-call with deadlock info (constraining registers appropriately)
 */
static void compose_deadlock_kcall_i386 (tstate *ts, int call, int regs_in, int regs_out)
{
	int this_lab;
	unsigned int x;

	if (options.debug_options & DEBUG_DEADLOCK) {
		/* set own WPTR in link-field */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND | ARG_DISP, REG_WPTR, W_LINK));
	}
	compose_kcall_i386 (ts, call, regs_in, regs_out);
	if (options.debug_options & DEBUG_DEADLOCK) {
		this_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, this_lab));
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
		case K_MWALTWT:
			x = DLOP_ALTWT;
			break;
		case K_TALTWT:
			x = DLOP_TALTWT;
			break;
		case K_XABLE:
			x = DLOP_XABLE;
			break;
		#if K_MWS_SYNC != K_UNSUPPORTED
		case K_MWS_SYNC:
			x = DLOP_SYNC;
			break;
		#endif
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
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
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
/*{{{  static void compose_inline_ldtimer_i386 (tstate *ts)*/
/*
 *	void compose_inline_ldtimer_i386 (tstate *ts)
 *	loads the time (requires CPU timers in kernel)
 */
static void compose_inline_ldtimer_i386 (tstate *ts)
{
	int eax_reg, edx_reg;
	int tmp_reg;

	eax_reg = tstack_newreg (ts->stack);
	edx_reg = tstack_newreg (ts->stack);
	tmp_reg = tstack_newreg (ts->stack);
	/*
	 *	rdtsc			# loads timer into EDX:EAX
	 *	mov	%edx, %t	# save high 32-bits
	 *	mul	glob_cpufactor	# multiply low 32-bits by factor into EDX:EAX
	 *	mov	%t, %eax	# restore high 32-bits
	 *	mov	%edx, %t	# save high bits of result
	 *	mul	glob_cpufactor	# multiply high 32-bits by factor into EDX:EAX
	 *	add	%t, %eax	# add high bits of first result with low bits of second result
	 */
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, eax_reg, ARG_REG, REG_EAX));
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, edx_reg, ARG_REG, REG_EDX));
	add_to_ins_chain (compose_ins (INS_RDTSC, 0, 2, ARG_REG | ARG_IMP, eax_reg, ARG_REG | ARG_IMP, edx_reg));

	/* high-order bits are in tmp_reg2, low-order bits in tmp_reg1 */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, edx_reg, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_UMUL, 2, 2, ARG_REG | ARG_IMP, eax_reg, ARG_NAMEDLABEL, string_dup ("&glob_cpufactor"),
		ARG_REG | ARG_IMP, edx_reg, ARG_REG | ARG_IMP, eax_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REG, eax_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, edx_reg, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_UMUL, 2, 2, ARG_REG | ARG_IMP, eax_reg, ARG_NAMEDLABEL, string_dup ("&glob_cpufactor"),
		ARG_REG | ARG_IMP, edx_reg, ARG_REG | ARG_IMP, eax_reg));
	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, tmp_reg, ARG_REG, eax_reg, ARG_REG, eax_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, edx_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, eax_reg));

	/* ok, place in regmap */
	ts->stack->a_reg = eax_reg;
	return;
}
/*}}}*/
/*{{{  static void compose_inline_quick_reschedule_i386 (tstate *ts)*/
/*
 *	void compose_inline_quick_reschedule_i386 (tstate *ts, int next_is_flabel0)
 *	reschedules next process (off run queue) or X_occscheduler
 *	assumes the current process is neatly saved
 */
static void compose_inline_quick_reschedule_i386 (tstate *ts)
{
	if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
		/* more processes ? */
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, NOT_PROCESS, ARG_REG, REG_FPTR, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_FLABEL, 2));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_OCCSCHEDULER + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2));
		add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 2));

		/* load next process */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_FPTR, ARG_REG, REG_WPTR));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_FPTR, W_LINK, ARG_REG, REG_FPTR));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_IND | ARG_DISP, REG_WPTR, W_IPTR));
	} else {
		fprintf (stderr, "%s: warning: compose_inline_quick_reschedule() does not support selected kernel interface\n", progname);
	}
	return;
}
/*}}}*/
/*{{{  static void compose_inline_tin_i386 (tstate *ts)*/
/*
 *	composes an inline TIN.  Areg has the target time, if it's before now, do nothing.
 *	otherwise add to the timer-queue and wait
 */
static void compose_inline_tin_i386 (tstate *ts)
{
	int eax_reg, edx_reg;
	int tmp_reg;

	/* Areg just about fits in amonst these, usually allocated to ECX */
	eax_reg = tstack_newreg (ts->stack);
	edx_reg = tstack_newreg (ts->stack);
	tmp_reg = tstack_newreg (ts->stack);
	/*
	 *	rdtsc			# loads timer into EDX:EAX
	 *	mov	%edx, %t	# save high 32-bits
	 *	mul	glob_cpufactor	# multiply low 32-bits by factor into EDX:EAX
	 *	mov	%t, %eax	# restore high 32-bits
	 *	mov	%edx, %t	# save high bits of result
	 *	mul	glob_cpufactor	# multiply high 32-bits by factor into EDX:EAX
	 *	add	%t, %eax	# add high bits of first result with low bits of second result
	 */
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, eax_reg, ARG_REG, REG_EAX));
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, edx_reg, ARG_REG, REG_EDX));
	add_to_ins_chain (compose_ins (INS_RDTSC, 0, 2, ARG_REG | ARG_IMP, eax_reg, ARG_REG | ARG_IMP, edx_reg));

	/* high-order bits are in EDX, low-order bits in EAX */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, edx_reg, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_UMUL, 2, 2, ARG_REG | ARG_IMP, eax_reg, ARG_NAMEDLABEL, string_dup ("&glob_cpufactor"),
		ARG_REG | ARG_IMP, edx_reg, ARG_REG | ARG_IMP, eax_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REG, eax_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, edx_reg, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_UMUL, 2, 2, ARG_REG | ARG_IMP, eax_reg, ARG_NAMEDLABEL, string_dup ("&glob_cpufactor"),
		ARG_REG | ARG_IMP, edx_reg, ARG_REG | ARG_IMP, eax_reg));
	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, tmp_reg, ARG_REG, eax_reg, ARG_REG, eax_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, edx_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, eax_reg));

	/* time _now_ is in EAX */
	/*
	 *	sub	Areg, %eax	# if (Areg AFTER eax), leaves sign bit
	 *	jns	<out>
	 */
	add_to_ins_chain (compose_ins (INS_SUB, 2, 2, ARG_REG, ts->stack->old_a_reg, ARG_REG, eax_reg, ARG_REG, eax_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NS, ARG_FLABEL, 0));

	/* slightly adjusted code -- do Y_fasttin kernel entry to add to queue */
	add_to_ins_chain (compose_ins (INS_INC, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_TIME));	/* store time */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));		/* store return address */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, Z_WAITING, ARG_REGIND | ARG_DISP, REG_WPTR, W_STATUS));	/* store status */
	/* run-time kernel handles priority field */
	compose_kcall_i386 (ts, K_FASTTIN, 1, 0);				/* reschedules */

	/* out */
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	return;
}
/*}}}*/
/*{{{  static void compose_inline_full_reschedule_i386 (tstate *ts)*/
/*
 *	if there is any activity in the sync-words (64-bits at "sf"), a jump to K_OCCSCHEDULER is made.
 *	otherwise if Fptr is NotProcess.p (0), nothing happens.  Else a reschedule takes place.
 */
static void compose_inline_full_reschedule_i386 (tstate *ts)
{
	int tmp_reg = tstack_newreg (ts->stack);
	int tmp_reg2 = tstack_newreg (ts->stack);

#if 0
	/* FIXME: temporary hack */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0x40, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0x50, ARG_REGIND | ARG_DISP, REG_WPTR, W_LINK));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0x60, ARG_REGIND | ARG_DISP, REG_WPTR, W_PRIORITY));
	return;
#endif

	add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_NAMEDLABEL, string_dup ("&sf"), ARG_REG, tmp_reg2));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, tmp_reg2, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REGIND | ARG_DISP, tmp_reg2, 4, ARG_REG, tmp_reg, ARG_REG, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_FLABEL, 3));
	/* need to reschedule, synch activity */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, W_LINK));

	#ifdef PROCESS_PRIORITY
	{
		int tmp_reg = tstack_newreg (ts->stack);

		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup (PPriority_name), ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_PRIORITY));
	}
	#endif	/* PROCESS_PRIORITY */

	/* add this process to the back of the run-queue.. */
	add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REG, REG_FPTR, ARG_REG, REG_FPTR, ARG_REG, REG_FPTR, ARG_REG | ARG_IMP, REG_CC));		/* Fptr = NotProcess.p ? */
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_FLABEL, 1));				/* yup, then jump. */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND | ARG_DISP, REG_BPTR, W_LINK));	/* Bptr[Link] = Wptr */
	add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, 2));

	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 1));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, REG_FPTR));				/* Fptr = Wptr */

	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 2));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, REG_BPTR));				/* Bptr = Wptr */

	/* reschedule */
	add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_OCCSCHEDULER + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2));

	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 3));						/* get here when synch flags clear */
	add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REG, REG_FPTR, ARG_REG, REG_FPTR, ARG_REG, REG_FPTR, ARG_REG | ARG_IMP, REG_CC));		/* Fptr = NotProcess.p ? */
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_FLABEL, 0));				/* yup, then jump (out) */

	/* inline reschedule */
	#ifdef PROCESS_PRIORITY
	{
		int tmp_reg = tstack_newreg (ts->stack);

		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup (PPriority_name), ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_PRIORITY));
	}
	#endif	/* PROCESS_PRIORITY */

	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, W_LINK));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_REG, REG_FPTR, ARG_REG, REG_BPTR, ARG_REG | ARG_IMP, REG_CC));				/* Fptr = Bptr ? */
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_FLABEL, 4));				/* yes, then jump */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND | ARG_DISP, REG_BPTR, W_LINK));	/* Bptr[Link] = Wptr */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, REG_BPTR));				/* Bptr = Wptr */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_FPTR, ARG_REG, REG_WPTR));				/* Wptr = Fptr */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_FPTR, W_LINK, ARG_REG, REG_FPTR));	/* Fptr = Fptr[Link] */
	add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_WPTR, W_IPTR));		/* jump to process resume address */

	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 4));		/* only one other on the run-queue */
	add_to_ins_chain (compose_ins (INS_SWAP, 2, 0, ARG_REG, REG_WPTR, ARG_REG, REG_FPTR));				/* Wptr, Fptr = Fptr, Wptr */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_FPTR, ARG_REG, REG_BPTR));				/* Bptr = Fptr */
	add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_WPTR, W_IPTR));		/* jump to process resume address */

	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
}
/*}}}*/
/*{{{  static void compose_inline_enqueue_i386 (tstate *ts, int preg, int nflab)*/
/*
 *	void compose_inline_enqueue_i386 (tstate *ts, int preg, int nflab)
 *	enqueues a process (whose descriptor is in preg)
 *	Note: this is insensitive to process priority!
 */
static void compose_inline_enqueue_i386 (tstate *ts, int preg, int nflab)
{
	int tmp_reg;

	if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
		/* P[Link] = NotProcess; run-queue empty ? */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, NOT_PROCESS, ARG_REGIND | ARG_DISP, preg, W_LINK));
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, NOT_PROCESS, ARG_REG, REG_FPTR, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_FLABEL, nflab));

		/* emoty queue */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, preg, ARG_REG, REG_FPTR));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, nflab + 1));

		/* non-empty queue */
		add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, nflab));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, preg, ARG_REGIND | ARG_DISP, REG_BPTR, W_LINK));

		/* out */
		add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, nflab + 1));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, preg, ARG_REG, REG_BPTR));
	} else {
		/* P[Link] = NotProcess; run-queue empty ? */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, NOT_PROCESS, ARG_REGIND | ARG_DISP, preg, W_LINK));
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, NOT_PROCESS, ARG_NAMEDLABEL, string_dup (Fptr_name), ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_FLABEL, nflab));

		/* empty queue */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, preg, ARG_NAMEDLABEL, string_dup (Fptr_name)));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, nflab + 1));

		/* non-empty queue */
		add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, nflab));
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup (Bptr_name), ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, preg, ARG_REGIND | ARG_DISP, tmp_reg, W_LINK));

		/* out */
		add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, nflab + 1));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, preg, ARG_NAMEDLABEL, string_dup (Bptr_name)));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_inline_startp_i386 (tstate *ts)*/
/*
 *	void compose_inline_startp_i386 (tstate *ts)
 *	inlined STARTP (start process) call
 *	areg has "other workspace", breg has "start offset"
 */
static void compose_inline_startp_i386 (tstate *ts)
{
	switch (constmap_typeof (ts->stack->old_b_reg)) {
	case VALUE_LABADDR:
#if 0
fprintf (stderr, "strange, STARTP at label %d (unlike occ21), generating code anyway :)\n", constmap_regconst (ts->stack->old_b_reg));
#endif
		/* fall through */
	case VALUE_LABDIFF:
#if 0
fprintf (stderr, "generating very quick STARTP from LABDIFF (%d,%d)\n", constmap_regconst (ts->stack->old_b_reg), constmap_otherlab (ts->stack->old_b_reg));
#endif
		/* nice, start point is a label */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, constmap_regconst (ts->stack->old_b_reg),
			ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, W_IPTR));
		#ifdef PROCESS_PRIORITY
		{
			int tmp_reg = tstack_newreg (ts->stack);

			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup (PPriority_name), ARG_REG, tmp_reg));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, W_PRIORITY));
		}
		#endif	/* PROCESS_PRIORITY */
		compose_inline_enqueue_i386 (ts, ts->stack->old_a_reg, 0);
		break;
	case VALUE_CONST:
		/* bit more complicated...:
		 *	move $0f, IPTR(%areg)
		 *	add %breg, IPTR(%areg), IPTR(%areg)
		 *	<enqueue %areg>
		 *  0:
		 */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, W_IPTR));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, constmap_regconst (ts->stack->old_b_reg), ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, W_IPTR,
			ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, W_IPTR));
		#ifdef PROCESS_PRIORITY
		{
			int tmp_reg = tstack_newreg (ts->stack);

			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup (PPriority_name), ARG_REG, tmp_reg));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, ts->stack->old_a_reg, W_PRIORITY));
		}
		#endif	/* PROCESS_PRIORITY */
		compose_inline_enqueue_i386 (ts, ts->stack->old_a_reg, 1);
		add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
		break;
	default:
#if 0
fprintf (stderr, "non-constant start point, huh, turn scheduler inlining off, you\'re obviously non-conformant!..\n");
#endif
		break;
	}
	return;
}
/*}}}*/
#ifndef PROCESS_PRIORITY
/*{{{  static void compose_inline_endp_i386 (tstate *ts)*/
/*
 *	void compose_inline_endp_i386 (tstate *ts)
 *	inlined ENDP (end process) call
 */
static void compose_inline_endp_i386 (tstate *ts)
{
	/* going to generate:
	 *	move %areg, %Wptr
	 *	decl COUNT(%Wptr)
	 *	je 0f
	 *	;; schedule
	 *	<inline schedule>
	 *	;; cjump CC_NE, K_OCCSCHEDULER
	 *  0:
	 *	jump *IPTR(%Wptr)
	 */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, REG_WPTR));
	add_to_ins_chain (compose_ins (INS_DEC, 1, 2, ARG_REGIND | ARG_DISP, REG_WPTR, W_COUNT, ARG_REGIND | ARG_DISP, REG_WPTR, W_COUNT, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_FLABEL, 0));
	compose_inline_quick_reschedule_i386 (ts);
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_IND, REG_WPTR));
	return;
}
/*}}}*/
#endif
/*{{{  static void compose_inline_stopp_i386 (tstate *ts)*/
/*
 *	void compose_inline_stopp_i386 (tstate *ts)
 *	inlined STOPP (stop process) call
 */
static void compose_inline_stopp_i386 (tstate *ts)
{
	/* going to generate:
	 *	move $0f, IPTR(%Wptr)
	 *	PROCESS_PRORITY -->
	 *		pushl	PPriority
	 *		popl	PRIORITY(%Wptr)
	 *	<inline schedule>
	 *  0:
	 */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));
	#ifdef PROCESS_PRIORITY
	{
		int tmp_reg = tstack_newreg (ts->stack);

		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup (PPriority_name), ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_PRIORITY));
	}
	#endif
	compose_inline_quick_reschedule_i386 (ts);
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	return;
}
/*}}}*/
/*{{{  static void compose_inline_in_i386 (tstate *ts, int width)*/
/*
 *	void compose_inline_in_i386 (tstate *ts, int width)
 *	generates an in-line version of IN (EXPERIMENTAL and no debugging. :(  no matter..]
 */
static void compose_inline_in_i386 (tstate *ts, int width)
{
	int i, lim, cregs[3], count_reg, chan_reg, dest_reg;

	cregs[0] = ts->stack->old_a_reg;
	cregs[1] = ts->stack->old_b_reg;
	cregs[2] = ts->stack->old_c_reg;
	if (width) {
		lim = 2;
		count_reg = -1;
		chan_reg = cregs[0];
		dest_reg = cregs[1];
	} else {
		lim = 3;
		count_reg = cregs[0];
		chan_reg = cregs[1];
		dest_reg = cregs[2];
	}
	/* constrain registers as per usual call */
	for (i=0; i<lim; i++) {
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, cregs[i], ARG_REG, xregs[i]));
	}

	/* in-line-ness */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));
	#ifdef PROCESS_PRIORITY
	{
		int tmp_reg = tstack_newreg (ts->stack);

		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup (PPriority_name), ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_PRIORITY));
	}
	#endif
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, NOT_PROCESS, ARG_REGIND, chan_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_FLABEL, 1));

	/* normal kernel call (channel ready) */
	switch (width) {
	case 8:
		add_to_ins_chain (compose_ins (INS_JUMP, 3, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_FASTIN8 + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2, ARG_REG | ARG_IMP, chan_reg, ARG_REG | ARG_IMP, dest_reg));
		break;
	case 32:
		add_to_ins_chain (compose_ins (INS_JUMP, 3, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_FASTIN32 + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2, ARG_REG | ARG_IMP, chan_reg, ARG_REG | ARG_IMP, dest_reg));
		break;
	default:
		add_to_ins_chain (compose_ins (INS_JUMP, 4, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_FASTIN + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2, ARG_REG | ARG_IMP, count_reg, ARG_REG | ARG_IMP, chan_reg,
			ARG_REG | ARG_IMP, dest_reg));
		break;
	}

	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 1));
	for (i=(lim-1); i>=0; i--) {
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, cregs[i]));
	}

	/* in-line stores and schedule (channel not ready) */

	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND, chan_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, dest_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_POINTER));

	if (options.inline_options & INLINE_SCHEDULER) {
		compose_inline_quick_reschedule_i386 (ts);
	} else {
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_FASTSCHEDULER + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2));
	}

	/* out */
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	return;
}
/*}}}*/
#ifndef PROCESS_PRIORITY
/*{{{  static void compose_inline_in_2_i386 (tstate *ts, int width)*/
/*
 *	void compose_inline_in_2_i386 (tstate *ts, int width) (EXPERIMENTAL)
 *	generates an inline input (2nd version, only kernel for reschedule)
 */
static void compose_inline_in_2_i386 (tstate *ts, int width)
{
	int count_reg, chan_reg, dest_reg;
	int tmp_reg;
	int known_size;
	int tmp_reg2;

	if (width) {
		count_reg = -1;
		chan_reg = ts->stack->old_a_reg;
		dest_reg = ts->stack->old_b_reg;
	} else {
		count_reg = ts->stack->old_a_reg;
		chan_reg = ts->stack->old_b_reg;
		dest_reg = ts->stack->old_c_reg;
	}
	/* test channel word
	 *	cmp $0, 0(%chan_reg)
	 *	jne 0f
	 */
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REGIND, chan_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_FLABEL, 0));

	/* channel not ready code
	 *	move %wptr, 0(%chan_reg)
	 *	move %dest_reg, W_POINTER(%wptr)
	 *	move $1f, W_IPTR(%wptr)
	 *	<inline schedule || Y_fastscheduler>
	 */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND, chan_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, dest_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_POINTER));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 1, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));

	if (options.inline_options & INLINE_SCHEDULER) {
		compose_inline_quick_reschedule_i386 (ts);
	} else {
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_FASTSCHEDULER + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2));
	}

	/* channel ready code
	 *  0:
	 *	;; IF (width)
	 *		move 0(%chan_reg), %t1
	 *		move W_POINTER(%t1), %t2
	 *		move[bl] 0(%t2), %t2 [8-BIT]
	 *		move[bl] %t2 [8-BIT], 0(%dest_reg)
	 *	;; ELSE
	 *		move 0(%chan_reg), %t1
	 *		push %esi
	 *		push %edi
	 *		move W_POINTER(%t1), %esi
	 *		move %dest_reg, %edi
	 *		<constrain %count_reg, REG_ECX>
	 *		rep moveb
	 *		pop %edi
	 *		pop %esi
	 *		<unconstrain %count_reg>
	 *	;; ENDIF
	 *	;; queue previously blocked process and clear channel
	 *	<inline queue || Y_queue>
	 *  1:
	 */
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	tmp_reg = tstack_newreg (ts->stack);
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, chan_reg, ARG_REG, tmp_reg));
	/* enqueue blocked process */
	if (options.inline_options & INLINE_SCHEDULER) {
		compose_inline_enqueue_i386 (ts, tmp_reg, 2);
	} else {
		fprintf (stderr, "** missing code for enqueue (inline_in_2)!\n");
	}
	/* clear channel */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REGIND, chan_reg));

	if (width) {
		known_size = (width >> 3);
	} else if (constmap_typeof (count_reg) == VALUE_CONST) {
		known_size = constmap_regconst (count_reg);
#if 0
fprintf (stderr, "** coding for copy (inline_in_2)! size is %d bytes\n", constmap_regconst (count_reg));
#endif
	} else {
		known_size = 0;
#if 0
fprintf (stderr, "** coding for copy (inline_in_2)! size is in %%%d\n", count_reg);
#endif
		add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup ("** coding for copy (inline_in_2)  size in reg")));
	}
	if (known_size && (known_size <= 16)) {
		/* anything <= 16 bytes gets optimised into straight sequences of load/store */
		int tmp_reg2 = tstack_newreg (ts->stack);
		int tmp_reg3 = tstack_newreg (ts->stack);
		int offset = 0;

		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, tmp_reg, W_POINTER, ARG_REG, tmp_reg2));
		while (known_size > 3) {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, tmp_reg2, offset, ARG_REG, tmp_reg3));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg3, ARG_REGIND | ARG_DISP, dest_reg, offset));
			offset += 4;
			known_size -= 4;
		}
		while (known_size) {
			add_to_ins_chain (compose_ins (INS_MOVEB, 1, 1, ARG_REGIND | ARG_DISP, tmp_reg2, offset, ARG_REG, tmp_reg3));
			add_to_ins_chain (compose_ins (INS_MOVEB, 1, 1, ARG_REG, tmp_reg3, ARG_REGIND | ARG_DISP, dest_reg, offset));
			offset++;
			known_size--;
		}
	} else {
		add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_FPTR));
		add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_BPTR));
		ts->stack_drift += 2;
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, tmp_reg, W_POINTER, ARG_REG, REG_FPTR));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, dest_reg, ARG_REG, REG_BPTR));
		if (known_size) {
			tmp_reg2 = tstack_newreg (ts->stack);
			if (!(known_size & 0x3)) {
				add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, known_size >> 2, ARG_REG, tmp_reg2));
			} else {
				add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, known_size, ARG_REG, tmp_reg2));
			}
		} else {
			tmp_reg2 = count_reg;
		}
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg2, ARG_REG, REG_ECX));
		if (known_size && !(known_size & 0x3)) {
			add_to_ins_chain (compose_ins (INS_REPMOVEL, 2, 1, ARG_REG | ARG_IMP, tmp_reg2, ARG_REG | ARG_IMP, REG_FPTR, ARG_REG | ARG_IMP, REG_BPTR));
		} else {
			add_to_ins_chain (compose_ins (INS_REPMOVEB, 2, 1, ARG_REG | ARG_IMP, tmp_reg2, ARG_REG | ARG_IMP, REG_FPTR, ARG_REG | ARG_IMP, REG_BPTR));
		}
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg2));
		add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REG, REG_BPTR));
		add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REG, REG_FPTR));
		ts->stack_drift -= 2;
	}
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 1));
	return;
}
/*}}}*/
/*{{{  static void compose_inline_min_i386 (tstate *ts, int wide)*/
/*
 *	void compose_inline_min_i386 (tstate *ts, int wide)
 *	generates an in-line version of MIN (EXPERIMENTAL)
 */
static void compose_inline_min_i386 (tstate *ts, int wide)
{
	int tmp_reg1, tmp_reg2, src_reg;
	int chan_reg, dest_reg;

	tmp_reg1 = tstack_newreg (ts->stack);
	tmp_reg2 = tstack_newreg (ts->stack);
	src_reg = tstack_newreg (ts->stack);
	dest_reg = ts->stack->old_b_reg;
	chan_reg = ts->stack->old_a_reg;

	/* check channel word */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 1, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST | ARG_ISCONST, NOT_PROCESS, ARG_REGIND, chan_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NE, ARG_FLABEL, 0));

	/* place process in channel word and reschedule */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, dest_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_POINTER));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND, chan_reg));
	if (options.inline_options & INLINE_REDUCED_POLL) {
		compose_inline_quick_reschedule_i386 (ts);
	} else {
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_OCCSCHEDULER + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2));
	}

	/* channel-ready, queue us, reschedule other */
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	compose_inline_enqueue_i386 (ts, REG_WPTR, 2);
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, chan_reg, ARG_REG, REG_WPTR));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, NOT_PROCESS, ARG_REGIND, chan_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, W_POINTER, ARG_REG, src_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, src_reg, ARG_REG, tmp_reg1));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, dest_reg, ARG_REG, tmp_reg2));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg1, ARG_REGIND, dest_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg2, ARG_REGIND, src_reg));
	if (wide) {
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, src_reg, 4, ARG_REG, tmp_reg1));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, dest_reg, 4, ARG_REG, tmp_reg2));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg1, ARG_REGIND | ARG_DISP, dest_reg, 4));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg2, ARG_REGIND | ARG_DISP, src_reg, 4));
	}
	add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_WPTR, W_IPTR));

	/* out */
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 1));
	return;
}
#endif	/* !PROCESS_PRIORITY */


#ifndef PROCESS_PRIORITY
/*
 *	void compose_inline_mout_i386 (tstate *ts, int wide)
 *	generates an in-line version of MOUT (EXPERIMENTAL)
 */
static void compose_inline_mout_i386 (tstate *ts, int wide)
{
	int tmp_reg1, tmp_reg2, dest_reg;
	int chan_reg, src_reg;

	src_reg = ts->stack->old_b_reg;
	chan_reg = ts->stack->old_a_reg;

	/* check channel word */
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST | ARG_ISCONST, NOT_PROCESS, ARG_REGIND, chan_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NE, ARG_FLABEL, 0));

	/* place process in channel word and reschedule */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, src_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_POINTER));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 1, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND, chan_reg));

	if (options.inline_options & INLINE_REDUCED_POLL) {
		compose_inline_quick_reschedule_i386 (ts);
	} else {
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_OCCSCHEDULER + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2));
	}

	/* channel ready code */
	tmp_reg1 = tstack_newreg (ts->stack);
	dest_reg = tstack_newreg (ts->stack);

	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, chan_reg, ARG_REG, tmp_reg1));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, tmp_reg1, W_POINTER, ARG_REG, dest_reg));
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST | ARG_ISCONST, Z_READY, ARG_REG, dest_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_GT, ARG_FLABEL, 2));

	/* talking to something ALTy */
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST | ARG_ISCONST, Z_WAITING, ARG_REG, dest_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NE, ARG_FLABEL, 3));
	compose_inline_enqueue_i386 (ts, tmp_reg1, 4);

	/* set us up and queue ALTer */
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 3));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, Z_READY, ARG_REGIND | ARG_DISP, tmp_reg1, W_STATUS));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND, chan_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, src_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_POINTER));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 1, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));
	if (options.inline_options & INLINE_REDUCED_POLL) {
		compose_inline_quick_reschedule_i386 (ts);
	} else {
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_OCCSCHEDULER + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2));
	}

	/* channel ready, do pointer swap */
	tmp_reg1 = tstack_newreg (ts->stack);
	tmp_reg2 = tstack_newreg (ts->stack);

	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 2));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 1, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));
	compose_inline_enqueue_i386 (ts, REG_WPTR, 6);
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, chan_reg, ARG_REG, REG_WPTR));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, NOT_PROCESS, ARG_REGIND, chan_reg));

	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, src_reg, ARG_REG, tmp_reg1));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, dest_reg, ARG_REG, tmp_reg2));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg1, ARG_REGIND, dest_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg2, ARG_REGIND, src_reg));
	if (wide) {
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, src_reg, 4, ARG_REG, tmp_reg1));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, dest_reg, 4, ARG_REG, tmp_reg2));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg1, ARG_REGIND | ARG_DISP, dest_reg, 4));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg2, ARG_REGIND | ARG_DISP, src_reg, 4));
	}
	add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_WPTR, W_IPTR));

	/* out */
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 1));
	return;
}
/*}}}*/
#endif	/* !PROCESS_PRIORITY */
/*{{{  static void compose_inline_out_i386 (tstate *ts, int width)*/
/*
 *	void compose_inline_out_i386 (tstate *ts, int width)
 *	generates an in-line version of OUT (EXPERIMENTAL)
 */
static void compose_inline_out_i386 (tstate *ts, int width)
{
	int i, lim, cregs[3], count_reg, chan_reg, src_reg;

	cregs[0] = ts->stack->old_a_reg;
	cregs[1] = ts->stack->old_b_reg;
	cregs[2] = ts->stack->old_c_reg;
	if (width) {
		lim = 2;
		count_reg = -1;
		chan_reg = cregs[0];
		src_reg = cregs[1];
	} else {
		lim = 3;
		count_reg = cregs[0];
		chan_reg = cregs[1];
		src_reg = cregs[2];
	}
	/* constrain as per usual call */
	for (i=0; i<lim; i++) {
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, cregs[i], ARG_REG, xregs[i]));
	}

	/* in-line-ness */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));
	#ifdef PROCESS_PRIORITY
	{
		int tmp_reg = tstack_newreg (ts->stack);

		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup (PPriority_name), ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_PRIORITY));
	}
	#endif
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, NOT_PROCESS, ARG_REGIND, chan_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_FLABEL, 1));

	/* normal kernel call (channel ready) */
	switch (width) {
	case 8:
		add_to_ins_chain (compose_ins (INS_JUMP, 3, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_FASTOUT8 + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2, ARG_REG | ARG_IMP, chan_reg, ARG_REG | ARG_IMP, src_reg));
		break;
	case 32:
		add_to_ins_chain (compose_ins (INS_JUMP, 3, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_FASTOUT32 + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2, ARG_REG | ARG_IMP, chan_reg, ARG_REG | ARG_IMP, src_reg));
		break;
	default:
		add_to_ins_chain (compose_ins (INS_JUMP, 4, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_FASTOUT + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2, ARG_REG | ARG_IMP, count_reg,
					ARG_REG | ARG_IMP, chan_reg, ARG_REG | ARG_IMP, src_reg));
		break;
	}
	for (i=(lim-1); i>=0; i--) {
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, cregs[i]));
	}

	/* in-line stores and schedule (channel not ready) */
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 1));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND, chan_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, src_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_POINTER));
	if (options.inline_options & INLINE_SCHEDULER) {
		compose_inline_quick_reschedule_i386 (ts);
	} else {
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_FASTSCHEDULER + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2));
	}

	/* out */
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
}
/*}}}*/
#ifndef PROCESS_PRIORITY
/*{{{  static void compose_inline_out_2_i386 (tstate *ts, int width)*/
/*
 *	void compose_inline_out_2_i386 (tstate *ts, int width)
 *	inline output (2nd version, kernel only for reschedule)
 */
static void compose_inline_out_2_i386 (tstate *ts, int width)
{
	int count_reg, chan_reg, src_reg;
	int tmp_reg, tmp_reg2;
	int known_size;
	int tmp_reg3;

	if (width) {
		count_reg = -1;
		chan_reg = ts->stack->old_a_reg;
		src_reg = ts->stack->old_b_reg;
	} else {
		count_reg = ts->stack->old_a_reg;
		chan_reg = ts->stack->old_b_reg;
		src_reg = ts->stack->old_c_reg;
	}
	/* test channel word
	 *	cmp $0, 0(%chan_reg)
	 *	jne 0f
	 */
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REGIND, chan_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_FLABEL, 0));

	/* channel not ready code
	 *	move %wptr, 0(%chan_reg)
	 *	move %src_reg, W_POINTER(%wptr)
	 *	move $2f, W_IPTR(%wptr)
	 *	<inline schedule || Y_fastscheduler>
	 */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND, chan_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, src_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_POINTER));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 2, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));
	if (options.inline_options & INLINE_REDUCED_POLL) {
		compose_inline_quick_reschedule_i386 (ts);
	} else {
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_OCCSCHEDULER + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2));
	}

	/* channel ready code
	 *  0:
	 *	move 0(%chan_reg), %t1
	 *	move W_POINTER(%t1), %t2
	 *	cmp $READY_P, %t2
	 *	jgt 1f
	 */
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	tmp_reg = tstack_newreg (ts->stack);
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, chan_reg, ARG_REG, tmp_reg));
	tmp_reg2 = tstack_newreg (ts->stack);
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, tmp_reg, W_STATUS, ARG_REG, tmp_reg2));
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, Z_READY, ARG_REG, tmp_reg2, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_GT, ARG_FLABEL, 1));
	/* ALTing input
	 *	cmp $WAITING_P, %t2
	 *	jne 3f
	 *	;; queue waiting process in channel word
	 *	<inline queue || Y_queue>
	 *  3:
	 *	move $READY_P, W_POINTER(%t1)
	 *	move %Wptr, 0(%chan_reg)
	 *	move %src_reg, W_POINTER(%Wptr)
	 *	move $2f, W_IPTR(%Wptr)
	 *	;; reschedule
	 *	jump K_OCCSCHEDULER
	 */
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, Z_WAITING, ARG_REG, tmp_reg2, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NE, ARG_FLABEL, 3));
	compose_inline_enqueue_i386 (ts, tmp_reg, 4);
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 3));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, Z_READY, ARG_REGIND | ARG_DISP, tmp_reg, W_STATUS));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND, chan_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, src_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_STATUS));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 2, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));
	if (options.inline_options & INLINE_REDUCED_POLL) {
		compose_inline_quick_reschedule_i386 (ts);
	} else {
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_OCCSCHEDULER + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2));
	}
	/* channel ready code
	 *  1:
	 *	;; IF (width)
	 *		move[bl] 0(%src_reg), %t3 [8-BIT]
	 *		move[bl] %t3 [8-bit], 0(%t2)
	 *	;; ELSE
	 *		push %esi
	 *		push %edi
	 *		movl %src_reg, %esi
	 *		movl %t2, %edi
	 *		<constrain %count_reg, %ecx>
	 *		rep movsb
	 *		<unconstrain %count_reg>
	 *		popl %edi
	 *		popl %esi
	 *	;; ENDIF
	 *	;; queue previously blocked process and clear channel
	 *	<inline queue || Y_queue>
	 *  2:
	 */
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 1));

	if (width) {
		known_size = (width >> 3);
	} else if (constmap_typeof (count_reg) == VALUE_CONST) {
		known_size = constmap_regconst (count_reg);
		fprintf (stderr, "** coding for copy (inline_out_2)! size is %d bytes\n", constmap_regconst (count_reg));
	} else {
		known_size = 0;
		fprintf (stderr, "** coding for copy (inline_out_2)! size is in %%%d\n", count_reg);
	}
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_FPTR));
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_BPTR));
	ts->stack_drift += 2;
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, src_reg, ARG_REG, REG_FPTR));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg2, ARG_REG, REG_BPTR));
	if (known_size) {
		tmp_reg3 = tstack_newreg (ts->stack);
		if (!(known_size & 0x3)) {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, known_size >> 2, ARG_REG, tmp_reg3));
		} else {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, known_size, ARG_REG, tmp_reg3));
		}
	} else {
		tmp_reg3 = count_reg;
	}
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg3, ARG_REG, REG_ECX));
	if (known_size && !(known_size & 0x3)) {
		add_to_ins_chain (compose_ins (INS_REPMOVEL, 2, 1, ARG_REG | ARG_IMP, tmp_reg3, ARG_REG | ARG_IMP, REG_FPTR, ARG_REG | ARG_IMP, REG_BPTR));
	} else {
		add_to_ins_chain (compose_ins (INS_REPMOVEB, 2, 1, ARG_REG | ARG_IMP, tmp_reg3, ARG_REG | ARG_IMP, REG_FPTR, ARG_REG | ARG_IMP, REG_BPTR));
	}
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg3));
	add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REG, REG_BPTR));
	add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REG, REG_FPTR));
	ts->stack_drift -= 2;

	/* clear channel */
	tmp_reg = tstack_newreg (ts->stack);
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, chan_reg, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, NOT_PROCESS, ARG_REGIND, chan_reg));

	if (options.inline_options & INLINE_SCHEDULER) {
		compose_inline_enqueue_i386 (ts, tmp_reg, 6);
	} else {
		fprintf (stderr, "** missing code for enqueue (inline_out_2)!\n");
	}
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 2));
	return;
}
/*}}}*/
#endif	/* !PROCESS_PRIORITY */
/*{{{  static void compose_inline_enbc_i386 (tstate *ts, int instr)*/
/*
 *	void compose_inline_enbc_i386 (tstate *ts, int instr)
 *	generates an in-line version of ENBC/ENBC3
 */
static void compose_inline_enbc_i386 (tstate *ts, int instr)
{
	int tmp_lab, out_lab;
	int guard_reg, chan_reg;

	tmp_lab = ++(ts->last_lab);
	out_lab = ++(ts->last_lab);

	guard_reg = (instr == I_ENBC) ? ts->stack->old_a_reg : ts->stack->old_b_reg;
	chan_reg = (instr == I_ENBC) ? ts->stack->old_b_reg : ts->stack->old_c_reg;

	/*
	 *  going to generate:
	 *	cmp	$0, %areg		guard == FALSE ?
	 *	je	Lout				yes, leave
	 */
	/* if guard is a constant 0, can optimise away */
	if (constmap_typeof (guard_reg) == VALUE_CONST) {
		if (constmap_regconst (guard_reg) == 0) {
			ts->stack->a_reg = guard_reg;
			return;
		}
		/* constant 1 means omit-test */
	} else {
		/* don't know guard value -- do test */
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REG, guard_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_LABEL, out_lab));
	}
	/*
	 *	cmp	$0, 0(%breg)		*channel_address == 0 ?
	 *	jne	Ltmp				no, jump
	 *	mov	%wptr, 0(%breg)			yes, set *channel_address = Wptr
	 *	jmp	Lout				     and leave
	 */
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REGIND, chan_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NE, ARG_LABEL, tmp_lab));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND, chan_reg));
	constmap_remove (chan_reg);
	add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, out_lab));
	/*
	 *  Ltmp:
	 *	cmp	%wptr, 0(%breg)		*channel_address == Wptr ?
	 *	je	Lout				yes, leave
	 *	mov	$Ready_p, W_POINTER(%wptr)	no, set Wptr[State] = Ready_p
	 *[[	jmp	%areg								]] FOR ENBC3 ONLY
	 *  Lout:
	 */
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, tmp_lab));
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_REG, REG_WPTR, ARG_REGIND, chan_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_LABEL, out_lab));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, Z_READY, ARG_REGIND | ARG_DISP, REG_WPTR, W_STATUS));
	if (instr == I_ENBC3) {
		if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LABADDR) {
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, constmap_regconst (ts->stack->old_a_reg)));
		} else {
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, ts->stack->old_a_reg));
		}
	}
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, out_lab));
	ts->stack->a_reg = guard_reg;
	return;
}
/*}}}*/
/*{{{  static void compose_pre_enbc_i386 (tstate *ts)*/
/*
 *	areg has process address/label, breg has the guard, creg has the channel
 */
static void compose_pre_enbc_i386 (tstate *ts)
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
/*{{{  static void compose_pre_enbt_i386 (tstate *ts)*/
/*
 *	areg has process address/label, breg has the guard, creg has the timeout expression
 */
static void compose_pre_enbt_i386 (tstate *ts)
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
/*{{{  static void compose_inline_disc_i386 (tstate *ts, int instr)*/
/*
 *	void compose_inline_disc_i386 (tstate *ts, int instr)
 *	generates an in-line version of DISC/NDISC (EXPERIMENTAL)
 */
static void compose_inline_disc_i386 (tstate *ts, int instr)
{
	int tmp_reg;
	int out_rlab, out_flab, out_lab;

	out_rlab = ++(ts->last_lab);
	out_flab = ++(ts->last_lab);
	out_lab = ++(ts->last_lab);
	tmp_reg = tstack_newreg (ts->stack);
	/*
	 *  going to generate:
	 *	cmp	$0, %breg		guard == FALSE ?
	 *	je	Lout_flab			yes, leave, setting fired = false
	 */
	/* if guard is constant 0, propagate through and return */
	if (constmap_typeof (ts->stack->old_b_reg) == VALUE_CONST) {
		if (constmap_regconst (ts->stack->old_b_reg) == 0) {
			ts->stack->a_reg = ts->stack->old_b_reg;
			ts->stack->b_reg = REG_UNDEFINED;
			ts->stack->c_reg = REG_UNDEFINED;
			return;
		}
	} else {
		/* don't know guard -- do check */
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_LABEL, out_flab));
	}
	/*
	 *	cmp	%wptr, 0(%creg)		*channel_address == Wptr ?
	 *	je	Lout_rlab			yes, leave, clearing channel and setting fired = false
	 *	cmp	$0, 0(%creg)		*channel_address == 0 ?
	 *	je	Lout_flab			yes, leave, setting fired = false
	 */
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_REG, REG_WPTR, ARG_REGIND, ts->stack->old_c_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_LABEL, out_rlab));
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REGIND, ts->stack->old_c_reg, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_LABEL, out_flab));
	if (instr == I_DISC) {
		/*
		 *	cmp	$NONE_SELECTED, 0(%wptr)	Wptr[Temp] == NoneSelected_o ?
		 *	jne	Lout_flab				no, leave, setting fired = false
		 *	mov	%areg, 0(%wptr)			Wptr[Temp] = Areg (process offset)
		 *	mov	$1, %tmp_reg			fired = true
		 *	jmp	Lout_lab			leave
		 */
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, NONE_SELECTED, ARG_REGIND, REG_WPTR, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NE, ARG_LABEL, out_flab));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REGIND, REG_WPTR));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 1, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, out_lab));
	} else {
		/* NDISC code: always select */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REGIND, REG_WPTR));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 1, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, out_lab));
	}
	/*
	 *  Lout_rlab:
	 *	mov	$NOT_PROCESS, 0(%creg)		*channel_address = NotProcess_p
	 *  Lout_flab:
	 *	mov	$0, %tmp			fired = false
	 *  Lout_lab:
	 */
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, out_rlab));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, NOT_PROCESS, ARG_REGIND, ts->stack->old_c_reg));
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, out_flab));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, out_lab));
	ts->stack->a_reg = tmp_reg;
	ts->stack->b_reg = REG_UNDEFINED;
	ts->stack->c_reg = REG_UNDEFINED;
	return;
}
/*}}}*/
/*{{{  static void compose_inline_altwt_i386 (tstate *ts)*/
/*
 *	void compose_inline_altwt_i386 (tstate *ts)
 *	generates an inline ALTWT (EXPERIMENTAL)
 */
static void compose_inline_altwt_i386 (tstate *ts)
{
	/*
	 *  going to generate:
	 *	mov	$NONE_SELECTED, 0(%wptr)	Wptr[Temp] = NoneSelected_o
	 *	cmp	$Z_READY, W_POINTER(%wptr)	Wptr[State] == Ready_p ?
	 *	je	0f					yes, leave
	 *	mov	$Z_WAITING, W_POINTER(%wptr)	Wptr[State] = Waiting_p
	 *	mov	$0f, W_IPTR(%wptr)		Wptr[Iptr] = address-of-leave
	 *	<reschedule>				RESCHEDULE ()
	 *  0:
	 */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, NONE_SELECTED, ARG_REGIND, REG_WPTR));
	add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, Z_READY, ARG_REGIND | ARG_DISP, REG_WPTR, W_POINTER, ARG_REG | ARG_IMP, REG_CC));
	add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_E, ARG_FLABEL, 0));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, Z_WAITING, ARG_REGIND | ARG_DISP, REG_WPTR, W_POINTER));
	#ifdef PROCESS_PRIORITY
	{
		int tmp_reg = tstack_newreg (ts->stack);

		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup (PPriority_name), ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_WPTR, W_PRIORITY));
	}
	#endif
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, W_IPTR));
	if (options.inline_options & INLINE_SCHEDULER) {
		compose_inline_quick_reschedule_i386 (ts);
	} else {
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REGIND | ARG_DISP | ARG_IND, REG_SPTR, (K_FASTSCHEDULER + ts->stack_drift + KIFACE_TABLEOFFS_I386) << 2));
	}
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	return;
}
/*}}}*/
/*{{{  static void compose_inline_stlx_i386 (tstate *ts, int ins)*/
/*
 *	void compose_inline_stlx_i386 (tstate *ts, int ins)
 *	used to inline STLF and STLB
 */
static void compose_inline_stlx_i386 (tstate *ts, int ins)
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
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_NAMEDLABEL, string_dup (Bptr_name)));
		}
		break;
	case I_STLF:
		if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, REG_FPTR));
		} else {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_NAMEDLABEL, string_dup (Fptr_name)));
		}
		break;
	}
	return;
}
/*}}}*/
/*{{{  static void compose_inline_malloc_i386 (tstate *ts)*/
/*
 *	void compose_inline_malloc_i386 (tstate *ts)
 *	allocates memory from the dmem_ allocator directly
 */
static void compose_inline_malloc_i386 (tstate *ts)
{
	int tmp_reg;

	tmp_reg = tstack_newreg (ts->stack);
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_WPTR));
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, ts->stack->old_a_reg));
	ts->stack_drift += 2;
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ESP, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_CALL, 1, 0, ARG_NAMEDLABEL, string_dup ("&dmem_alloc2")));
	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, 4, ARG_REG, REG_ESP, ARG_REG, REG_ESP));
	add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REG, ts->stack->a_reg));
	add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REG, REG_WPTR));
	ts->stack_drift -= 2;
	return;
}
/*}}}*/
/*{{{  static void compose_debug_insert_i386 (tstate *ts, int mdpairid)*/
/*
 *	void compose_debug_insert_i386 (tstate *ts, int mdpairid)
 *	generates an insert-debugging thing
 */
static void compose_debug_insert_i386 (tstate *ts, int mdpairid)
{
	unsigned int x;
	static char *mdparam_vars[] = {"&mdparam1", "&mdparam2", "&mdparam3", "&mdparam4", "&mdparam5", "&mdparam6"};

	if ((options.debug_options & DEBUG_INSERT) && !(ts->supress_debug_insert)) {
		x = ((ts->file_pending & 0xffff) << 16) + (ts->line_pending & 0xffff);

		if (!(options.kernel_interface & KRNLIFACE_MP)) {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_NAMEDLABEL, string_dup (mdparam_vars[(mdpairid << 1)])));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->filename_label, ARG_NAMEDLABEL, string_dup (mdparam_vars[(mdpairid << 1) + 1])));
		} else {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REGIND | ARG_DISP, REG_SCHED, offsetof(ccsp_sched_t, mdparam[(mdpairid << 1)])));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->filename_label, ARG_REGIND | ARG_DISP, REG_SCHED, offsetof(ccsp_sched_t, mdparam[((mdpairid << 1) + 1)])));
		}
	}
	return;
}
/*}}}*/
/*{{{  static void compose_debug_procnames_i386 (tstate *ts)*/
/*
 *	void compose_debug_procnames_i386 (tstate *ts)
 *	generates procedure-name list for post-mortem debugging
 */
static void compose_debug_procnames_i386 (tstate *ts)
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
/*{{{  static void compose_debug_filenames_i386 (tstate *ts)*/
/*
 *	void compose_debug_filenames_i386 (tstate *ts)
 *	generates file-name list for post-mortem debugging
 */
static void compose_debug_filenames_i386 (tstate *ts)
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
/*{{{  static void compose_error_entry (tstate *ts, int type, int args)*/
static void compose_error_entry (tstate *ts, int type, int args)
{
	/* argument 2 */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_EDX, ARG_REGIND | ARG_DISP, REG_SCHED, offsetof(ccsp_sched_t, cparam[0])));
	/* argument 3 */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->procedure_label, ARG_REGIND | ARG_DISP, REG_SCHED, offsetof(ccsp_sched_t, cparam[1])));
	/* argument 4 */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->filename_label, ARG_REGIND | ARG_DISP, REG_SCHED, offsetof(ccsp_sched_t, cparam[2])));
	if (args == 5) {
		/* argument 5 */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_EAX, ARG_REGIND | ARG_DISP, REG_SCHED, offsetof(ccsp_sched_t, cparam[3])));
	}
	
	/* argument 1 */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_ALT_ECX, ARG_REG, xregs[0]));

	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_SCHED, ARG_REG, xregs[1]));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, xregs[2]));

	add_to_ins_chain (compose_kjump_i386 (ts, INS_CALL, 0, kif_entry (type)));
}
/*}}}*/
/*{{{  static void compose_debug_zero_div_i386 (tstate *ts)*/
/*
 *	void compose_debug_zero_div_i386 (tstate *ts)
 *	generates zero-div target point for post-mortem debugging
 */
static void compose_debug_zero_div_i386 (tstate *ts)
{
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->zerodiv_label));
	compose_error_entry (ts, K_ZERODIV, 4);
}
/*}}}*/
/*{{{  static void compose_debug_floaterr_i386 (tstate *ts)*/
/*
 *	void compose_debug_floaterr_i386 (tstate *ts)
 *	generates floating-point-error target for post-mortem debugging
 */
static void compose_debug_floaterr_i386 (tstate *ts)
{
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->floaterr_label));
	compose_error_entry (ts, K_FLOATERR, 5);
}
/*}}}*/
/*{{{  static void compose_debug_overflow_i386 (tstate *ts)*/
/*
 *	void compose_debug_overflow_i386 (tstate *ts)
 *	generates overflow target point for post-mortem debugging
 */
static void compose_debug_overflow_i386 (tstate *ts)
{
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->overflow_label));
	compose_error_entry (ts, K_OVERFLOW, 4);
}
/*}}}*/
/*{{{  static void compose_debug_rangestop_i386 (tstate *ts)*/
/*
 *	void compose_debug_rangestop_i386 (tstate *ts)
 *	generates range/STOP error target point for post-mortem debugging
 */
static void compose_debug_rangestop_i386 (tstate *ts)
{
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->range_entry_label));
	compose_error_entry (ts, K_RANGERR, 4);
}
/*}}}*/
/*{{{  static void compose_debug_seterr_i386 (tstate *ts)*/
/*
 *	generates code for SETERR
 */
static void compose_debug_seterr_i386 (tstate *ts)
{
	unsigned int x;

	x = (0xfb00 << 16) + (ts->line_pending & 0xffff);
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_EDX));
	x = ((ts->file_pending & 0xffff) << 16) + (ts->proc_pending & 0xffff);
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_ECX));

	compose_error_entry (ts, K_SETERR, 4);
}
/*}}}*/
/*{{{  static void compose_overflow_jumpcode_i386 (tstate *ts, int dcode)*/
/*
 *	void compose_overflow_jumpcode_i386 (tstate *ts, int dcode)
 *	generates overflow error jump-code
 */
static void compose_overflow_jumpcode_i386 (tstate *ts, int dcode)
{
	unsigned int x;

	if (options.debug_options & DEBUG_OVERFLOW) {
		x = ((dcode & 0xff) << 24) + (ts->line_pending & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_EDX));
		x = ((ts->file_pending & 0xffff) << 16) + ((ts->proc_pending) & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_ECX));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, ts->overflow_label));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_floaterr_jumpcode_i386 (tstate *ts)*/
/*
 *	void compose_floaterr_jumpcode_i386 (tstate *ts)
 *	generates floating-point error jump-code
 */
static void compose_floaterr_jumpcode_i386 (tstate *ts)
{
	unsigned int x;

	if (options.debug_options & DEBUG_FLOAT) {
		x = (ts->fp_line_pending & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_EDX));
		x = ((ts->fp_file_pending & 0xffff) << 16) + ((ts->fp_proc_pending) & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_ECX));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, ts->floaterr_label));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_rangestop_jumpcode_i386 (tstate *ts, int rcode)*/
/*
 *	generates range-check/STOP error jump-code
 */
static void compose_rangestop_jumpcode_i386 (tstate *ts, int rcode)
{
	unsigned int x;

	if (options.debug_options & DEBUG_RANGESTOP) {
		x = ((rcode & 0xff) << 24) + (0xff << 16) + (ts->line_pending & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_EDX));
		x = ((ts->file_pending & 0xffff) << 16) + (ts->proc_pending & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_ECX));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, ts->range_entry_label));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_debug_deadlock_set_i386 (tstate *ts)*/
/*
 *	void compose_debug_deadlock_set_i386 (tstate *ts)
 *	generates deadlock setup point for post-mortem debugging
 */
static void compose_debug_deadlock_set_i386 (tstate *ts)
{
	add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, ts->procfile_setup_label));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->filename_label, ARG_REG, REG_ALT_EAX));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->procedure_label, ARG_REG, REG_ALT_ECX));
	add_to_ins_chain (compose_ins (INS_RET, 0, 0));
	return;
}
/*}}}*/
/*{{{  static void compose_divcheck_zero_i386 (tstate *ts, int reg)*/
/*
 *	void compose_divcheck_zero_i386 (tstate *ts, int reg)
 *	checks that `reg' isn't zero before performing division
 */
static void compose_divcheck_zero_i386 (tstate *ts, int reg)
{
	int this_lab;
	unsigned int x;

	switch (constmap_typeof (reg)) {
	default:
		this_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REG, reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_LABEL, this_lab));
		x = (ts->line_pending & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_EDX));
		x = ((ts->file_pending & 0xffff) << 16) + (ts->proc_pending & 0xffff);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_ECX));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, ts->zerodiv_label));
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		break;
	case VALUE_CONST:
		if (constmap_regconst (reg) == 0) {
			/* wehey! */
			fprintf (stderr, "%s: serious: division by zero seen around line %d in %s\n", progname, ts->line_pending, ts->file_list[ts->file_pending]);
			x = (ts->line_pending & 0xffff);
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_EDX));
			x = ((ts->file_pending & 0xffff) << 16) + (ts->proc_pending & 0xffff);
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, x, ARG_REG, REG_ALT_ECX));
			add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, ts->zerodiv_label));
		}
		/* otherwise it's a constant non-zero */
		break;
	}
	return;
}
/*}}}*/
/*{{{  static void compose_divcheck_zero_simple_i386 (tstate *ts, int reg)*/
/*
 *	void compose_divcheck_zero_simple_i386 (tstate *ts, int reg)
 *	checks that `reg' isn't zero before performing division
 */
static void compose_divcheck_zero_simple_i386 (tstate *ts, int reg)
{
	switch (constmap_typeof (reg)) {
	default:
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0, ARG_REG, reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_FLABEL, 2));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_SCHED, ARG_REG, xregs[1]));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, xregs[2]));
		add_to_ins_chain (compose_kjump_i386 (ts, INS_CALL, 0, kif_entry (K_BNSETERR)));
		add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 2));
		break;
	case VALUE_CONST:
		if (constmap_regconst (reg) == 0) {
			/* wehey! */
			fprintf (stderr, "%s: serious: division by zero seen around line %d in %s\n", progname, ts->line_pending, ts->file_list[ts->file_pending]);
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_SCHED, ARG_REG, xregs[1]));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, xregs[2]));
			add_to_ins_chain (compose_kjump_i386 (ts, INS_CALL, 0, kif_entry (K_BNSETERR)));
		}
		/* otherwise it's a constant non-zero */
		break;
	}
	return;
}
/*}}}*/
/*{{{  static int compose_widenshort_i386 (tstate *ts)*/
/*
 *	int compose_widenshort_i386 (tstate *ts)
 *	sign-extends AX into EAX (Areg), returns the new Areg
 *	modifies Areg on the stack to stop collisions in the register allocator (need to improve that..)
 */
static int compose_widenshort_i386 (tstate *ts)
{
	int tmp_reg = tstack_newreg (ts->stack);

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_EAX));
	add_to_ins_chain (compose_ins (INS_CWDE, 1, 1, ARG_REG | ARG_IMP | ARG_IS16BIT, ts->stack->a_reg, ARG_REG | ARG_IMP, ts->stack->a_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->a_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->a_reg, ARG_REG, tmp_reg));
	constmap_remove (ts->stack->a_reg);
	ts->stack->a_reg = tmp_reg;
	return tmp_reg;
}
/*}}}*/
/*{{{  static int compose_widenword_i386 (tstate *ts)*/
/*
 *	int compose_widenword_i386 (tstate *ts)
 *	sign-extends EAX into EDX:EAX.  returns extended register.  mangles the stack somewhat
 */
static int compose_widenword_i386 (tstate *ts)
{
	int tmp_reg = tstack_newreg (ts->stack);

	/* arrange for old_a_reg to stay at stack-top */
	tmp_reg = tstack_newreg (ts->stack);
	ts->stack->a_reg = ts->stack->old_a_reg;
	ts->stack->b_reg = tmp_reg;
	ts->stack->c_reg = ts->stack->old_b_reg;

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_EAX));
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->b_reg, ARG_REG, REG_EDX));
	add_to_ins_chain (compose_ins (INS_CDQ, 1, 2, ARG_REG | ARG_IMP, ts->stack->old_a_reg, ARG_REG | ARG_IMP, ts->stack->a_reg, ARG_REG | ARG_IMP, ts->stack->b_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->b_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->a_reg));
	constmap_remove (ts->stack->a_reg);
	constmap_remove (ts->stack->b_reg);
	return tmp_reg;
}
/*}}}*/
/*{{{  static void compose_division_i386 (tstate *ts, int dividend, int divisor, int quotient)*/
/*
 *	void compose_division_i386 (tstate *ts, int dividend, int divisor, int quotient)
 *	does division.  Sign-extends EAX into EAX:EDX, then does the division
 */
static void compose_division_i386 (tstate *ts, int dividend, int divisor, int quotient)
{
	int tmp_reg = tstack_newreg (ts->stack);
	int remainder = tmp_reg;

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, dividend, ARG_REG, REG_EAX));
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EDX));
	add_to_ins_chain (compose_ins (INS_CDQ, 1, 2, ARG_REG | ARG_IMP, dividend, ARG_REG | ARG_IMP, dividend, ARG_REG | ARG_IMP, tmp_reg));
	add_to_ins_chain (compose_ins (INS_DIV, 3, 2, ARG_REG | ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, dividend, ARG_REG, divisor, \
		ARG_REG | ARG_IMP, quotient, ARG_REG | ARG_IMP, remainder));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, dividend));
	constmap_remove (quotient);
	return;
}
/*}}}*/
/*{{{  static int compose_remainder_i386 (tstate *ts, int dividend, int divisor)*/
/*
 *	int compose_remainder_i386 (tstate *ts, int dividend, int divisor)
 *	does remainder.  Sign-extends EAX into EAX:EDX, does the remainder and returns the register holding it (EDX)
 */
static int compose_remainder_i386 (tstate *ts, int dividend, int divisor)
{
	int tmp_reg = tstack_newreg (ts->stack);
	int quotient = dividend;

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, dividend, ARG_REG, REG_EAX));
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, divisor, ARG_REG, REG_ECX));
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EDX));
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
/*{{{  static int compose_iospace_loadbyte_i386 (tstate *ts, int portreg, int targetreg)*/
/*
 *	int compose_iospace_loadbyte_i386 (tstate *ts, int portreg, int targetreg)
 *	does INB instruction, targetreg is only a suggestion, real target is returned
 */
static int compose_iospace_loadbyte_i386 (tstate *ts, int portreg, int targetreg)
{
	int tmp_reg = tstack_newreg (ts->stack);

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EDX));
	switch (constmap_typeof (portreg)) {
	case VALUE_CONST:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, constmap_regconst (portreg), ARG_REG, tmp_reg));
		break;
	default:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, portreg, ARG_REG, tmp_reg));
		break;
	}
	/* upgrade targetreg with fresh register if needed */
	if (targetreg == portreg) {
		targetreg = tstack_newreg (ts->stack);
	}
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, targetreg, ARG_REG, REG_EAX));
	add_to_ins_chain (compose_ins (INS_INB, 1, 1, ARG_REG | ARG_IS16BIT, tmp_reg, ARG_REG | ARG_IS8BIT, targetreg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, targetreg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
	return targetreg;
}
/*}}}*/
/*{{{  static void compose_iospace_storebyte_i386 (tstate *ts, int portreg, int sourcereg)*/
/*
 *	void compose_iospace_storebyte_i386 (tstate *ts, int portreg, int sourcereg)
 *	does OUTB instruction
 */
static void compose_iospace_storebyte_i386 (tstate *ts, int portreg, int sourcereg)
{
	int tmp_reg = tstack_newreg (ts->stack);

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EDX));
	switch (constmap_typeof (portreg)) {
	case VALUE_CONST:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, constmap_regconst (portreg), ARG_REG, tmp_reg));
		break;
	default:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, portreg, ARG_REG, tmp_reg));
		break;
	}
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, sourcereg, ARG_REG, REG_EAX));
	add_to_ins_chain (compose_ins (INS_OUTB, 2, 0, ARG_REG | ARG_IS8BIT, sourcereg, ARG_REG | ARG_IS16BIT, tmp_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, sourcereg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
	return;
}
/*}}}*/
/*{{{  static void compose_iospace_read_i386 (tstate *ts, int portreg, int addrreg, int width)*/
/*
 *	does port io-space input, fixed widths only
 */
static void compose_iospace_read_i386 (tstate *ts, int portreg, int addrreg, int width)
{
	int tmp_reg = tstack_newreg (ts->stack);
	int tmp_reg2 = tstack_newreg (ts->stack);
	int instr, mvinstr, flags;

	switch (width) {
	case 8:
		instr = INS_INB;
		mvinstr = INS_MOVEB;
		flags = ARG_IS8BIT;
		break;
	case 16:
		instr = INS_INW;
		mvinstr = INS_MOVEW;
		flags = ARG_IS16BIT;
		break;
	case 32:
		instr = INS_INL;
		mvinstr = INS_MOVE;
		flags = 0;
		break;
	default:
		fprintf (stderr, "compose_iospace_read_i386(): unsupported width %d\n", width);
		return;
	}

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EDX));
	switch (constmap_typeof (portreg)) {
	case VALUE_CONST:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, constmap_regconst (portreg), ARG_REG, tmp_reg));
		break;
	case VALUE_LOCAL:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (portreg) << WSH, ARG_REG, tmp_reg));
		break;
	default:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, portreg, ARG_REG, tmp_reg));
		break;
	}
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg2, ARG_REG, REG_EAX));
	add_to_ins_chain (compose_ins (instr, 1, 1, ARG_REG | ARG_IS16BIT, tmp_reg, ARG_REG | flags, tmp_reg2));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg2));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));

	switch (constmap_typeof (addrreg)) {
	case VALUE_LOCALPTR:
		add_to_ins_chain (compose_ins (mvinstr, 1, 1, ARG_REG, tmp_reg2, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (addrreg) << WSH));
		break;
	default:
		add_to_ins_chain (compose_ins (mvinstr, 1, 1, ARG_REG, tmp_reg2, ARG_REGIND, addrreg));
		break;
	}

	return;
}
/*}}}*/
/*{{{  static void compose_iospace_write_i386 (tstate *ts, int portreg, int addrreg, int width)*/
/*
 *	does port io-space output, fixed widths only
 */
static void compose_iospace_write_i386 (tstate *ts, int portreg, int addrreg, int width)
{
	int tmp_reg = tstack_newreg (ts->stack);
	int tmp_reg2 = tstack_newreg (ts->stack);
	int instr, mvinstr, flags;

	switch (width) {
	case 8:
		instr = INS_OUTB;
		mvinstr = INS_MOVEB;
		flags = ARG_IS8BIT;
		break;
	case 16:
		instr = INS_OUTW;
		mvinstr = INS_MOVEW;
		flags = ARG_IS16BIT;
		break;
	case 32:
		instr = INS_OUTL;
		mvinstr = INS_MOVE;
		flags = 0;
		break;
	default:
		fprintf (stderr, "compose_iospace_write_i386(): unsupported width %d\n", width);
		return;
	}

	add_to_ins_chain (compose_ins (mvinstr, 1, 1, ARG_REGIND, addrreg, ARG_REG, tmp_reg2));

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EDX));
	switch (constmap_typeof (portreg)) {
	case VALUE_CONST:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, constmap_regconst (portreg), ARG_REG, tmp_reg));
		break;
	case VALUE_LOCAL:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (portreg) << WSH, ARG_REG, tmp_reg));
		break;
	default:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, portreg, ARG_REG, tmp_reg));
		break;
	}
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg2, ARG_REG, REG_EAX));
	add_to_ins_chain (compose_ins (instr, 1, 1, ARG_REG | flags, tmp_reg2, ARG_REG | ARG_IS16BIT, tmp_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg2));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));

	return;
}
/*}}}*/
/*{{{  static void compose_move_loadptrs_i386 (tstate *ts)*/
static void compose_move_loadptrs_i386 (tstate *ts)
{
	switch (constmap_typeof (ts->stack->old_c_reg)) {
	case VALUE_LABADDR:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, constmap_regconst (ts->stack->old_c_reg), ARG_REG, REG_JPTR));
		break;
	case VALUE_LOCALPTR:
		if (!constmap_regconst (ts->stack->old_c_reg)) {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, REG_JPTR));
		} else {
			add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_c_reg) << WSH, ARG_REG, REG_JPTR));
		}
		break;
	case VALUE_LOCAL:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_c_reg) << WSH, ARG_REG, REG_JPTR));
		break;
	default:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_c_reg, ARG_REG, REG_JPTR));
		break;
	}
	switch (constmap_typeof (ts->stack->old_b_reg)) {
	case VALUE_LABADDR:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, constmap_regconst (ts->stack->old_b_reg), ARG_REG, REG_LPTR));
		break;
	case VALUE_LOCALPTR:
		if (!constmap_regconst (ts->stack->old_b_reg)) {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, REG_LPTR));
		} else {
			add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_b_reg) << WSH, ARG_REG, REG_LPTR));
		}
		break;
	case VALUE_LOCAL:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_b_reg) << WSH, ARG_REG, REG_LPTR));
		break;
	default:
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, REG_LPTR));
		break;
	}
	return;
}
/*}}}*/
/*{{{  static void compose_move_i386 (tstate *ts)*/
static void compose_move_i386 (tstate *ts)
{
	int tmp_reg, tmp_reg2;

	if ((constmap_typeof (ts->stack->old_a_reg) == VALUE_CONST) && (constmap_regconst (ts->stack->old_a_reg) == 4)) {
		/* very special case... (src = Creg, dst = Breg) */
		tmp_reg = tstack_newreg (ts->stack);
		switch (constmap_typeof (ts->stack->old_c_reg)) {
		case VALUE_LABADDR:
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL, constmap_regconst (ts->stack->old_c_reg), ARG_REG, tmp_reg));
			break;
		default:
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, ts->stack->old_c_reg, ARG_REG, tmp_reg));
			break;
		}
		switch (constmap_typeof (ts->stack->old_b_reg)) {
		case VALUE_LOCALPTR:
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_b_reg) << WSH));
			break;
		default:
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND, ts->stack->old_b_reg));
			break;
		}
	} else {
		add_to_ins_chain (compose_ins (INS_OR, 2, 2, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_FLABEL, 0));
		add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_LPTR));
		add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_JPTR));
		ts->stack_drift += 2;
		compose_move_loadptrs_i386 (ts);
		tmp_reg = tstack_newreg (ts->stack);
		tmp_reg2 = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_JPTR));
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg2, ARG_REG, REG_LPTR));
		switch (constmap_typeof (ts->stack->old_a_reg)) {
		case VALUE_CONST:
			{
				int loop_count;

				loop_count = constmap_regconst (ts->stack->old_a_reg);
				if (!(loop_count & 0x03)) {
					/* do replicated word move */
					int new_reg = tstack_newreg (ts->stack);

					add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, loop_count >> 2, ARG_REG, new_reg));
					add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, new_reg, ARG_REG, REG_ECX));
					add_to_ins_chain (compose_ins (INS_REPMOVEL, 2, 1, ARG_REG | ARG_IMP, new_reg, ARG_REG | ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, tmp_reg2));
					add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, new_reg));
				} else {
					/* regular move */
					add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->old_a_reg, ARG_REG, REG_ECX));
					add_to_ins_chain (compose_ins (INS_REPMOVEB, 2, 1, ARG_REG | ARG_IMP, ts->stack->old_a_reg, ARG_REG | ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, tmp_reg2));
					add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->old_a_reg));
				}
			}
			break;
		default:
			/* regular move */
			add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->old_a_reg, ARG_REG, REG_ECX));
			add_to_ins_chain (compose_ins (INS_REPMOVEB, 2, 1, ARG_REG | ARG_IMP, ts->stack->old_a_reg, ARG_REG | ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, tmp_reg2));
			add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->old_a_reg));
			break;
		}
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg2));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REG, REG_JPTR));
		add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REG, REG_LPTR));
		ts->stack_drift -= 2;
		add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_shift_i386 (tstate *ts, int sec, int r1, int r2, int r3)*/
static void compose_shift_i386 (tstate *ts, int sec, int r1, int r2, int r3)
{
	/*
	 *	sh{rl}	%areg		# (by ECX == %oldareg)
	 */
	/* ensure that the shift is in ECX -- IA32 automatically masks */
	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, r1, ARG_REG, REG_ECX));
	add_to_ins_chain (compose_ins (((sec == I_SHL) ? INS_SHL : INS_SHR), 2, 1, ARG_REG | ARG_IMP, r1, ARG_REG, r2, ARG_REG, r3));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, r1));
	return;
}
/*}}}*/
/*{{{  static void compose_longop_i386 (tstate *ts, int sec)*/
/*
 *	void compose_longop_i386 (tstate *ts, int sec)
 *	does long-integer operations.  Mangles stack
 */
static void compose_longop_i386 (tstate *ts, int sec)
{
	int tmp_reg, this_lab;

	switch (sec) {
		/*{{{  I_LADD -- long addition*/
	case I_LADD:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->old_c_reg, ARG_REG, REG_EDX));
		add_to_ins_chain (compose_ins (INS_RCR, 2, 1, ARG_CONST, 1, ARG_REG | ARG_IS8BIT, ts->stack->old_c_reg, ARG_REG | ARG_IS8BIT, ts->stack->old_c_reg));
		constmap_remove (ts->stack->old_c_reg);
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->old_c_reg));

		add_to_ins_chain (compose_ins (INS_ADC, 2, 2, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_CC));
		constmap_remove (ts->stack->old_b_reg);
		ts->stack->a_reg = ts->stack->old_b_reg;
		break;
		/*}}}*/
		/*{{{  I_LSUB -- long subtraction*/
	case I_LSUB:
		add_to_ins_chain (compose_ins (INS_RCR, 2, 1, ARG_CONST, 1, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg));
		constmap_remove (ts->stack->old_c_reg);
		add_to_ins_chain (compose_ins (INS_SBB, 2, 2, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, REG_CC));
		constmap_remove (ts->stack->old_b_reg);
		ts->stack->a_reg = ts->stack->old_b_reg;
		break;
		/*}}}*/
		/*{{{  I_LSUM -- long sum (no overflow checks)*/
	case I_LSUM:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->old_c_reg, ARG_REG, REG_ECX));
		add_to_ins_chain (compose_ins (INS_RCR, 2, 1, ARG_CONST, 1, ARG_REG | ARG_IS8BIT, ts->stack->old_c_reg, ARG_REG | ARG_IS8BIT, ts->stack->old_c_reg));
		constmap_remove (ts->stack->old_c_reg);
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->old_c_reg));

		add_to_ins_chain (compose_ins (INS_ADC, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
		constmap_remove (ts->stack->old_b_reg);

		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EDX));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_RCL, 2, 1, ARG_CONST, 1, ARG_REG | ARG_IS8BIT, tmp_reg, ARG_REG | ARG_IS8BIT, tmp_reg));
		constmap_remove (tmp_reg);
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		ts->stack->b_reg = tmp_reg;
		break;
		/*}}}*/
		/*{{{  I_LDIFF -- long difference (no overflow checks)*/
	case I_LDIFF:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->old_c_reg, ARG_REG, REG_ECX));
		add_to_ins_chain (compose_ins (INS_RCR, 2, 1, ARG_CONST, 1, ARG_REG | ARG_IS8BIT, ts->stack->old_c_reg, ARG_REG | ARG_IS8BIT, ts->stack->old_c_reg));
		constmap_remove (ts->stack->old_c_reg);
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->old_c_reg));
		
		add_to_ins_chain (compose_ins (INS_SBB, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
		constmap_remove (ts->stack->old_b_reg);

		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EDX));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_RCL, 2, 1, ARG_CONST, 1, ARG_REG | ARG_IS8BIT, tmp_reg, ARG_REG | ARG_IS8BIT, tmp_reg));
		constmap_remove (tmp_reg);
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		ts->stack->b_reg = tmp_reg;
		break;
		/*}}}*/
		/*{{{  I_LMUL -- long multiplication*/
	case I_LMUL:
		tmp_reg = tstack_newreg (ts->stack); 
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EDX));
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->old_a_reg, ARG_REG, REG_EAX));
		add_to_ins_chain (compose_ins (INS_UMUL, 2, 2, ARG_REG | ARG_IMP, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG | ARG_IMP, ts->stack->old_a_reg, ARG_REG | ARG_IMP, tmp_reg));
		constmap_remove (ts->stack->old_a_reg);
		constmap_remove (tmp_reg);
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->old_a_reg));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg));
		add_to_ins_chain (compose_ins (INS_ADC, 2, 1, ARG_CONST, 0, ARG_REG, tmp_reg, ARG_REG, tmp_reg));
		ts->stack->a_reg = ts->stack->old_a_reg;
		ts->stack->b_reg = tmp_reg;
		break;
		/*}}}*/
		/*{{{  I_LSHL, I_LSHR -- long shifts*/
	case I_LSHL:
	case I_LSHR:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->old_a_reg, ARG_REG, REG_ECX));
		if (sec == I_LSHL) {
			add_to_ins_chain (compose_ins (INS_SHLD, 3, 2, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_c_reg, \
				ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_c_reg));
			add_to_ins_chain (compose_ins (INS_SHL, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
		} else {
			add_to_ins_chain (compose_ins (INS_SHRD, 3, 2, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_b_reg, \
				ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_b_reg));
			add_to_ins_chain (compose_ins (INS_SHR, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg));
		}
		constmap_remove (ts->stack->old_b_reg);
		constmap_remove (ts->stack->old_c_reg);
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->old_a_reg));
		this_lab = ++(ts->last_lab);

		/* compare 32 with shift-size and jump if below */
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 32, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_B, ARG_LABEL, this_lab));
		/* exchange oldBreg and oldCreg */
		add_to_ins_chain (compose_ins (INS_SWAP, 2, 2, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_c_reg, \
			ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_b_reg));
		/* clear register with bits shifted completely out */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, (sec == I_LSHL) ? ts->stack->old_b_reg : ts->stack->old_c_reg));

		/* compare 64 with shift-size and jump if below */
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 64, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_B, ARG_LABEL, this_lab));
		/* clear register with bits shifted completely out */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, (sec == I_LSHL) ? ts->stack->old_c_reg : ts->stack->old_b_reg));

		/* done */
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		ts->stack->a_reg = ts->stack->old_b_reg;
		ts->stack->b_reg = ts->stack->old_c_reg;
		break;
		/*}}}*/
		/*{{{  I_LDIV -- long division*/
	case I_LDIV:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->old_c_reg, ARG_REG, REG_EDX));
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->old_b_reg, ARG_REG, REG_EAX));
		add_to_ins_chain (compose_ins (INS_UDIV, 3, 2, ARG_REG | ARG_IMP, ts->stack->old_c_reg, ARG_REG | ARG_IMP, ts->stack->old_b_reg, ARG_REG, ts->stack->old_a_reg, \
			ARG_REG | ARG_IMP, ts->stack->old_b_reg, ARG_REG | ARG_IMP, ts->stack->old_c_reg));
		constmap_remove (ts->stack->old_b_reg);
		constmap_remove (ts->stack->old_c_reg);
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->old_b_reg));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->old_c_reg));
		ts->stack->a_reg = ts->stack->old_b_reg;
		ts->stack->b_reg = ts->stack->old_c_reg;
		break;
		/*}}}*/
		/*{{{  default -- error*/
	default:
		fprintf (stderr, "%s: fatal: secondary op-code 0x%X in compose_longop_i386()\n", progname, sec);
		break;
		/*}}}*/
	}
	return;
}
/*}}}*/
/*{{{  static void compose_fpop_i386 (tstate *ts, int sec)*/
/*
 *	generates FP instructions
 */
static void compose_fpop_i386 (tstate *ts, int sec)
{
	int tmp_reg, this_lab, this_lab2, error_flags;

	switch (sec) {
		/*{{{  I_FPPOP -- pop floating-point stack (no-op)*/
	case I_FPPOP:
		add_to_ins_chain (compose_ins (INS_FSTP, 0, 1, ARG_FREG, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		ts->stack->fs_depth--;
		break;
		/*}}}*/
		/*{{{  I_FPSQRT -- floating-point square-root*/
	case I_FPSQRT:
		add_to_ins_chain (compose_ins (INS_FSQRT, 0, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPABS -- floating-point absolute value*/
	case I_FPABS:
		add_to_ins_chain (compose_ins (INS_FABS, 0, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPREV -- reverse top two floating-point stack entries*/
	case I_FPREV:
		add_to_ins_chain (compose_ins (INS_FXCH, 0, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		break;
		/*}}}*/
		/*{{{  I_FPDUP -- duplicate top floating-point stack entry*/
	case I_FPDUP:
		ts->stack->fs_depth++;
		add_to_ins_chain (compose_ins (INS_FLD, 1, 0, ARG_FREG, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		break;
		/*}}}*/
		/*{{{  I_FPLDNLDB -- load non-local REAL64*/
	case I_FPLDNLDB:
		ts->stack->fs_depth++;
		if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LOCALPTR) {
			add_to_ins_chain (compose_ins (INS_FLD64, 1, 0, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH));
		} else {
			add_to_ins_chain (compose_ins (INS_FLD64, 1, 0, ARG_REGIND, ts->stack->old_a_reg));
		}
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPLDNLSN -- load non-local REAL32*/
	case I_FPLDNLSN:
		ts->stack->fs_depth++;
		if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LOCALPTR) {
			add_to_ins_chain (compose_ins (INS_FLD32, 1, 0, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH));
		} else {
			add_to_ins_chain (compose_ins (INS_FLD32, 1, 0, ARG_REGIND, ts->stack->old_a_reg));
		}
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPLDNLADDDB -- add non-local REAL64 (to top of FP stack)*/
	case I_FPLDNLADDDB:
		if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LOCALPTR) {
			add_to_ins_chain (compose_ins (INS_FADD64, 1, 0, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH));
		} else {
			add_to_ins_chain (compose_ins (INS_FADD64, 1, 0, ARG_REGIND, ts->stack->old_a_reg));
		}
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPLDNLADDSN -- add non-local REAL32 (to top of FP stack)*/
	case I_FPLDNLADDSN:
		if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LOCALPTR) {
			add_to_ins_chain (compose_ins (INS_FADD32, 1, 0, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH));
		} else {
			add_to_ins_chain (compose_ins (INS_FADD32, 1, 0, ARG_REGIND, ts->stack->old_a_reg));
		}
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPLDNLMULDB -- multiply non-local REAL64 (with top of FP stack)*/
	case I_FPLDNLMULDB:
		if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LOCALPTR) {
			add_to_ins_chain (compose_ins (INS_FMUL64, 1, 0, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH));
		} else {
			add_to_ins_chain (compose_ins (INS_FMUL64, 1, 0, ARG_REGIND, ts->stack->old_a_reg));
		}
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPLDNLMULSN -- multiply non-local REAL32 (with top of FP stack)*/
	case I_FPLDNLMULSN:
		if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LOCALPTR) {
			add_to_ins_chain (compose_ins (INS_FMUL32, 1, 0, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH));
		} else {
			add_to_ins_chain (compose_ins (INS_FMUL32, 1, 0, ARG_REGIND, ts->stack->old_a_reg));
		}
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPLDNLDBI -- load indexed non-local REAL64*/
	case I_FPLDNLDBI:
		ts->stack->fs_depth++;
		add_to_ins_chain (compose_ins (INS_FLD64, 1, 0, ARG_REGINDSIB, 8, ts->stack->old_b_reg, ts->stack->old_a_reg));
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPLDNLSNI -- load indexed non-local REAL32*/
	case I_FPLDNLSNI:
		ts->stack->fs_depth++;
		add_to_ins_chain (compose_ins (INS_FLD32, 1, 0, ARG_REGINDSIB, 4, ts->stack->old_b_reg, ts->stack->old_a_reg));
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPSTNLDB -- store non-local REAL64*/
	case I_FPSTNLDB:
		if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LOCALPTR) {
			add_to_ins_chain (compose_ins (INS_FST64, 0, 1, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH));
		} else {
			add_to_ins_chain (compose_ins (INS_FST64, 0, 1, ARG_REGIND, ts->stack->old_a_reg));
		}
		compose_fp_set_fround_i386 (ts, FPU_N);
		ts->stack->fs_depth--;
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPSTNLSN -- store non-local REAL32*/
	case I_FPSTNLSN:
		if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LOCALPTR) {
			add_to_ins_chain (compose_ins (INS_FST32, 0, 1, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH));
		} else {
			add_to_ins_chain (compose_ins (INS_FST32, 0, 1, ARG_REGIND, ts->stack->old_a_reg));
		}
		compose_fp_set_fround_i386 (ts, FPU_N);
		ts->stack->fs_depth--;
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPSTNLI32 -- store non-local INT32*/
	case I_FPSTNLI32:
		/* FIST64 is horribly mis-named -- generates fistpl, which means long INTEGER, not long floating-point value */
		if (constmap_typeof (ts->stack->old_a_reg) == VALUE_LOCALPTR) {
			add_to_ins_chain (compose_ins (INS_FIST64, 0, 1, ARG_REGIND | ARG_DISP, REG_WPTR, constmap_regconst (ts->stack->old_a_reg) << WSH));
		} else {
			add_to_ins_chain (compose_ins (INS_FIST64, 0, 1, ARG_REGIND, ts->stack->old_a_reg));
		}
		compose_fp_set_fround_i386 (ts, FPU_N);
		ts->stack->fs_depth--;
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPLDZEROSN, I_FPLDZERODB -- load floating-point zero*/
	case I_FPLDZEROSN:
	case I_FPLDZERODB:
		ts->stack->fs_depth++;
		add_to_ins_chain (compose_ins (INS_FLDZ, 0, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		break;
		/*}}}*/
		/*{{{  I_FPADD -- FP addition*/
	case I_FPADD:
		add_to_ins_chain (compose_ins (INS_FADD, 0, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		ts->stack->fs_depth--;
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPSUB -- FP subtraction*/
	case I_FPSUB:
		add_to_ins_chain (compose_ins (INS_FSUB, 0, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		ts->stack->fs_depth--;
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPMUL -- FP multiplication*/
	case I_FPMUL:
		add_to_ins_chain (compose_ins (INS_FMUL, 0, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		ts->stack->fs_depth--;
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPDIV -- FP division*/
	case I_FPDIV:
		add_to_ins_chain (compose_ins (INS_FDIV, 0, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		ts->stack->fs_depth--;
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_R32SIN, I_R64SIN*/
	case I_R32SIN:
	case I_R64SIN:
		add_to_ins_chain (compose_ins (INS_FSIN, 0, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_R32COS, I_R64COS*/
	case I_R32COS:
	case I_R64COS:
		add_to_ins_chain (compose_ins (INS_FCOS, 0, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_R32TAN, I_R64TAN*/
	case I_R32TAN:
	case I_R64TAN:
		add_to_ins_chain (compose_ins (INS_FPTAN, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTP, 0, 1, ARG_FREG, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPI32TOR64, I_FPI32TOR32 -- integer to real conversions*/
	case I_FPI32TOR64:
	case I_FPI32TOR32:
		add_to_ins_chain (compose_ins (INS_FILD32, 1, 0, ARG_REGIND, ts->stack->old_a_reg));
		compose_fp_set_fround_i386 (ts, FPU_N);
		ts->stack->fs_depth++;
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPB32TOR64 -- unsigned int32 to real conversion*/
	case I_FPB32TOR64:
		/* this is a mess; intel has no useful method of loading an unsigned int into the FPU.. */
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0x80000000, ARG_REG, ts->stack->old_a_reg, ARG_REG | ARG_IMP, REG_CC));
		this_lab = ++(ts->last_lab);
		this_lab2 = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_BE, ARG_LABEL, this_lab));

		/* strip MSB, FILD32 then add 2^31 (have this handy) */
		add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_CONST, 0x7fffffff, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg));
		add_to_ins_chain (compose_ins (INS_FILD32, 1, 0, ARG_REGIND, ts->stack->old_a_reg));
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->floatrange_label[1], ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_FADD64, 1, 0, ARG_REGIND, tmp_reg));
		add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_LABEL, this_lab2));

		/* basic conversion */
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		add_to_ins_chain (compose_ins (INS_FILD32, 1, 0, ARG_REGIND, ts->stack->old_a_reg));

		/* out */
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab2));
		ts->stack->fs_depth++;
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPINT, I_FPRTOI32 -- convert FP value to INT32*/
	case I_FPINT:
	case I_FPRTOI32:
		add_to_ins_chain (compose_ins (INS_FRNDINT, 0, 0));
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPCHKERR -- check for pending floating-point error*/
	case I_FPCHKERR:
		this_lab = ++(ts->last_lab);

		/* "precision", "overflow", "zero divide" */
		error_flags = 0x2c;
		if (options.underflow_error) {
			/* "underflow" */
			error_flags |= 0x10;
		}

		/* store current flags (in stack) */
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EAX));
		add_to_ins_chain (compose_ins (INS_LAHF, 0, 1, ARG_REG|ARG_IMP, tmp_reg));
		add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, tmp_reg));

		/* extract exception flags */
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG|ARG_IS16BIT, tmp_reg));
		add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_CONST, error_flags, ARG_REG|ARG_IS16BIT, tmp_reg, ARG_REG|ARG_IS16BIT, tmp_reg));
		/* if it's only a "precision" exception, that's OK */
		add_to_ins_chain (compose_ins (INS_CMP, 2, 1, ARG_CONST, 0x20, ARG_REG|ARG_IS16BIT, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_LABEL, this_lab));
		/* extract flags again */
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG|ARG_IS16BIT, tmp_reg));
		add_to_ins_chain (compose_ins (INS_AND, 2, 2, ARG_CONST, error_flags, ARG_REG|ARG_IS16BIT, tmp_reg, ARG_REG|ARG_IS16BIT, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		/* if none are set, that's OK too */
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_Z, ARG_LABEL, this_lab));

		/* error detected => fixup stack; call kernel */
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, 4, ARG_REG, REG_ESP, ARG_REG, REG_ESP));
		if (options.debug_options & DEBUG_FLOAT) {
			compose_floaterr_jumpcode_i386 (ts);
			/* normal execution continues */
		} else if (!options.disable_checking) {
			compose_kcall_i386 (ts, K_BSETERR, 0, -1);
		}

		/* no error => restore flags (from stack); continue */
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EAX));
		add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG|ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));

		ts->stack->must_set_cmp_flags = 1;
		break;
		/*}}}*/
		/*{{{  I_FPEXPDEC32 -- ..?*/
	case I_FPEXPDEC32:
		add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_CONST, -32));
		ts->stack_drift++;
		add_to_ins_chain (compose_ins (INS_FILD32, 1, 0, ARG_REGIND, REG_ESP));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, 4, ARG_REG, REG_ESP, ARG_REG, REG_ESP));
		ts->stack_drift--;
		add_to_ins_chain (compose_ins (INS_FXCH, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSCALE, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTP, 0, 1, ARG_FREG, 1));
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPEXTINC32 -- ..?*/
	case I_FPEXPINC32:
		add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_CONST, 32));
		ts->stack_drift++;
		add_to_ins_chain (compose_ins (INS_FILD32, 1, 0, ARG_REGIND, REG_ESP));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, 4, ARG_REG, REG_ESP, ARG_REG, REG_ESP));
		ts->stack_drift--;
		add_to_ins_chain (compose_ins (INS_FXCH, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSCALE, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTP, 0, 1, ARG_FREG, 1));
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPCHKI32, I_FPCHKI64 -- check that value at top of FP stack fits in an INT32/INT64*/
	case I_FPCHKI32:
	case I_FPCHKI64:
		/* this should work, but are never actually generated for a T800 */
		/* check that (-2^xx <= value) */
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->floatrange_label[(sec == I_FPCHKI32) ? 0 : 2], ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_FLD64, 1, 0, ARG_REGIND, tmp_reg));
		add_to_ins_chain (compose_ins (INS_FCOMP, 0, 0));
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EAX));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		this_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_BE, ARG_LABEL, this_lab));
		compose_overflow_jumpcode_i386 (ts, (sec == I_FPCHKI32) ? PMOP_FPCHKI32 : PMOP_FPCHKI64);
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		/* check that (2^xx > value) */
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->floatrange_label[(sec == I_FPCHKI32) ? 1 : 3], ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_FLD64, 1, 0, ARG_REGIND, tmp_reg));
		add_to_ins_chain (compose_ins (INS_FCOMP, 0, 0));
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EAX));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		this_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_A, ARG_LABEL, this_lab));
		compose_overflow_jumpcode_i386 (ts, (sec == I_FPCHKI32) ? PMOP_FPCHKI32 : PMOP_FPCHKI64);
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		break;
		/*}}}*/
		/*{{{  I_FPNAN -- test for not-a-number*/
	case I_FPNAN:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_EAX));
		add_to_ins_chain (compose_ins (INS_FTST, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, ts->stack->a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
		ts->cond = CC_FPNAN;
		ts->stack->must_set_cmp_flags = 0;
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPNOTFINITE -- test for not-finite*/
	case I_FPNOTFINITE:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_EAX));
		add_to_ins_chain (compose_ins (INS_FXAM, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, ts->stack->a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
		ts->cond = CC_FPINFNAN;
		ts->stack->must_set_cmp_flags = 0;
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPORDERED -- test for ordered*/
	case I_FPORDERED:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_EAX));
		add_to_ins_chain (compose_ins (INS_FUCOM, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, ts->stack->a_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, ts->stack->a_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->a_reg));
		constmap_remove (ts->stack->a_reg);
		ts->cond = CC_FPORD;
		ts->stack->must_set_cmp_flags = 0;
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPGT -- test for FP greater-than*/
	case I_FPGT:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_EAX));
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
		/*}}}*/
		/*{{{  I_FPEQ -- test for FP equal*/
	case I_FPEQ:
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_EAX));
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
		/*}}}*/
		/*{{{  I_FPREM -- floating-point remainder*/
	case I_FPREM:
		/* this is unpleasant.. */
		this_lab = ++(ts->last_lab);
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_FXCH, 0, 0));
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		add_to_ins_chain (compose_ins (INS_FPREM1, 0, 0));
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EAX));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_AND, 2, 2, ARG_CONST, 0x400, ARG_REG, tmp_reg, ARG_REG, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_NZ, ARG_LABEL, this_lab));
		add_to_ins_chain (compose_ins (INS_FSTP, 0, 1, ARG_FREG, 1));
		ts->stack->fs_depth--;
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPMULBY2 -- floating-point multiply by 2*/
	case I_FPMULBY2:
		add_to_ins_chain (compose_ins (INS_FLD1, 0, 0));
		add_to_ins_chain (compose_ins (INS_FXCH, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSCALE, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTP, 0, 1, ARG_FREG, 1));
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPDIVBY2 -- floating-point divide by 2*/
	case I_FPDIVBY2:
		add_to_ins_chain (compose_ins (INS_FLD1, 0, 0));
		add_to_ins_chain (compose_ins (INS_FCHS, 0, 0));
		add_to_ins_chain (compose_ins (INS_FXCH, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSCALE, 0, 0));
		add_to_ins_chain (compose_ins (INS_FSTP, 0, 1, ARG_FREG, 1));
		compose_fp_set_fround_i386 (ts, FPU_N);
		tstate_ctofp (ts);
		break;
		/*}}}*/
		/*{{{  I_FPR32TO664, I_FPR64TOR32 -- convert between REAL32 and REAL64*/
	case I_FPR32TOR64:
	case I_FPR64TOR32:
		/* does nothing */
		break;
		/*}}}*/
		/*{{{  default -- error*/
	default:
		fprintf (stderr, "%s: compose_fpop_i386: warning: unknown instruction %d (ignoring)\n", progname, sec);
		break;
		/*}}}*/
	}
	return;
}
/*}}}*/
/*{{{  static void compose_external_ccall_i386 (tstate *ts, char *name, ins_chain **pst_first, ins_chain **pst_last)*/
/*
 *	generates code to perform an external C call
 */
static void compose_external_ccall_i386 (tstate *ts, int inlined, char *name, ins_chain **pst_first, ins_chain **pst_last)
{
	int tmp_reg, kernel_call;

	/* In the i386 ABI used by Linux, *BSD, Darwin and presumably Windows
	 * defines EBP, ESI, EDI as private to the caller, hence we don't
	 * need to save them.
	 *
	 * Hence WPTR, SCHED, FPTR, BPTR should all be safe.
	 * - 20080113
	 */

	*pst_first = compose_ins (INS_SUB, 2, 1, ARG_CONST | ARG_ISCONST, 16, ARG_REG, REG_ESP, ARG_REG, REG_ESP);
	add_to_ins_chain (*pst_first);

	tmp_reg = tstack_newreg (ts->stack);
	add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, 4, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND, REG_ESP));
	if (options.extref_prefix) {
		char sbuf[128];

		sprintf (sbuf, "%s%s", options.extref_prefix, name + 1);
		add_to_ins_chain (compose_ins (INS_CALL, 1, 0, ARG_NAMEDLABEL, string_dup (sbuf)));
	} else {
		add_to_ins_chain (compose_ins (INS_CALL, 1, 0, ARG_NAMEDLABEL, string_dup (name + 1)));
	}
	*pst_last = compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, REG_ESP, ARG_REG, REG_ESP);
	add_to_ins_chain (*pst_last);

	if (!strcmp (name, "C.ccsp.suspendproc")) {
		/* hack for dynamic process suspension -- we've done ccsp.suspendproc, now call kernel */
		*pst_last = NULL;
		/* re-assess and push parameters (and fix Wptr) */
		add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, 4, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR));
		kernel_call = K_DYNPROC_SUSPEND;
		if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
			/* really we should use a constraint on tmp_reg for the param */
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REG, xregs[0]));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_SCHED, ARG_REG, xregs[1]));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, xregs[2]));
			*pst_last = compose_kjump_i386 (ts, INS_CALL, 0, (kif_entry (kernel_call)));
		} else if (options.kernel_interface & KRNLIFACE_MESH) {
			fprintf (stderr, "%s: error: do not have dynamic process support for MESH yet\n", progname);
			exit (EXIT_FAILURE);
		} else if (options.kernel_interface & KRNLIFACE_CSPLINUX) {
			fprintf (stderr, "%s: error support for CSP/Linux not yet implemented\n", progname);
			exit (EXIT_FAILURE);
		}
	} else {
		/* if we're using NOCC, call return does not do this anymore */
		if (!options.nocc_codegen) {
			*pst_last = compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR);
		}
	}
	add_to_ins_chain (*pst_last);
}
/*}}}*/
/*{{{  static void compose_bcall_i386 (tstate *ts, int i, int kernel_call, char *name, ins_chain **pst_first, ins_chain **pst_last)*/
/*
 *	generates code for a blocking system-call
 */
static void compose_bcall_i386 (tstate *ts, int i, int kernel_call, int inlined, char *name, ins_chain **pst_first, ins_chain **pst_last)
{
	int arg_reg, tmp_reg;
	ins_chain *st_first, *st_last;

	arg_reg = tstack_newreg (ts->stack);
	tmp_reg = tstack_newreg (ts->stack);
	add_to_ins_chain (st_first = compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, 4, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND | ARG_DISP, REG_SCHED, offsetof(ccsp_sched_t, cparam[0])));

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, arg_reg, ARG_REG, xregs[0]));
	if (kernel_call != K_KERNEL_RUN) {
		/* push address of function to call */
		if (options.extref_prefix) {
			char sbuf[128];

			sprintf (sbuf, "%s%s", options.extref_prefix, name + ((i == 2) ? 1 : 2));
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL | ARG_ISCONST, string_dup (sbuf), ARG_REG, arg_reg));
		} else {
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL | ARG_ISCONST, string_dup (name + ((i == 2) ? 1 : 2)), ARG_REG, arg_reg));
		}
	}
	if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_SCHED, ARG_REG, xregs[1]));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, xregs[2]));
		add_to_ins_chain (compose_kjump_i386 (ts, INS_CALL, 0, kif_entry (kernel_call)));
	} else if (options.kernel_interface & KRNLIFACE_MESH) {
		fprintf (stderr, "%s: error: do not have blocking call support in MESH yet\n", progname);
	} else if (options.kernel_interface & KRNLIFACE_CSPLINUX) {
		fprintf (stderr, "%s: error: support for CSP/Linux not yet implemented\n", progname);
	}
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, arg_reg));

	st_last = compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR);
	add_to_ins_chain (st_last);

	*pst_first = st_first;
	*pst_last = st_last;
	return;
}
/*}}}*/
/*{{{  static void compose_cif_call_i386 (tstate *ts, char *name, ins_chain **pst_first, ins_chain **pst_last)*/
/*
 *	generates code to perform an external C call to a CIF process
 */
static void compose_cif_call_i386 (tstate *ts, int inlined, char *name, ins_chain **pst_first, ins_chain **pst_last)
{
	char sbuf[128];
	int tmp_reg;

	tmp_reg = tstack_newreg (ts->stack);
	
	*pst_first = compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, 4, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR);
	add_to_ins_chain (*pst_first);
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, -36));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, -1, ARG_REGIND | ARG_DISP, REG_WPTR, -32));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_SCHED, ARG_REGIND | ARG_DISP, REG_WPTR, -28));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REG, tmp_reg));
	constmap_remove (tmp_reg);
	add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_CONST | ARG_ISCONST, 40, ARG_REG, tmp_reg, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_CONST | ARG_ISCONST, ~0xf, ARG_REG, tmp_reg, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REG, REG_ESP));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND, REG_ESP));

	sprintf (sbuf, "@%s%s", options.extref_prefix ? options.extref_prefix : "" , name + 4);
	add_to_ins_chain (compose_ins (INS_CALL, 1, 0, ARG_NAMEDLABEL, string_dup (sbuf)));

	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, -28, ARG_REG, REG_SCHED));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND | ARG_DISP, REG_SCHED, offsetof(ccsp_sched_t, stack), ARG_REG, REG_ESP));
	add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_CONST | ARG_ISCONST, 4, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR));
	*pst_last = compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR);
	add_to_ins_chain (*pst_last);
}
/*}}}*/
/*{{{  static void compose_fp_load_control_i386 (tstate *ts, int rounding_mode)*/
/*
 *	load the FPU's control word (uses stack)
 */
static void compose_fp_load_control_i386 (tstate *ts, int rounding_mode)
{
	int rmodes[4] = {I386_FPU_N, I386_FPU_P, I386_FPU_M, I386_FPU_Z};

	/* 0b << 12       infinity control -- unused on 387+
	 * 00b << 10      rounding control -- from rmodes above
	 * 11b << 8       precision control -- double extended precision (64 bits)
	 * 111111b << 0   exception mask -- all masked
	 */
	int val = 0x033f | (rmodes[rounding_mode] << 10);

	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_CONST | ARG_ISCONST, val));
	add_to_ins_chain (compose_ins (INS_FLDCW, 1, 0, ARG_REGIND, REG_ESP));
	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, 4, ARG_REG, REG_ESP, ARG_REG, REG_ESP));

	return;
}
/*}}}*/
/*{{{  static void compose_fp_init_i386 (tstate *ts)*/
/*
 *	initialises the FPU
 */
static void compose_fp_init_i386 (tstate *ts)
{
	compose_fp_load_control_i386 (ts, FPU_N);

	return;
}
/*}}}*/
/*{{{  static void compose_return_i386 (tstate *ts)*/
/*
 *	generates PROC/FUNCTION return code -- moved out here to deal with different mechanisms for
 *	returning arguments from FUNCTIONs
 */
static void compose_return_i386 (tstate *ts)
{
	int toldregs[3];
	int i;

	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR));
	toldregs[0] = ts->stack->old_a_reg;
	toldregs[1] = ts->stack->old_b_reg;
	toldregs[2] = ts->stack->old_c_reg;

	for (i=0; i<ts->numfuncresults; i++) {
		add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, toldregs[i]));
	}
	ts->numfuncresults = 0;
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REGIND | ARG_DISP, REG_WPTR, -16));
	add_to_ins_chain (compose_ins (INS_RET, 0, 0));

	return;
}
/*}}}*/
/*{{{  static void compose_nreturn_i386 (tstate *ts, int adjust)*/
/*
 *	generates PROC/FUNCTION return code for NOCC -- moved here to deal with FUNCTION returns again
 */
static void compose_nreturn_i386 (tstate *ts, int adjust)
{
	int tmpreg = tstack_newreg (ts->stack);
	int toldregs[3];
	int i;

	toldregs[0] = ts->stack->old_a_reg;
	toldregs[1] = ts->stack->old_b_reg;
	toldregs[2] = ts->stack->old_c_reg;

	for (i=0; i<ts->numfuncresults; i++) {
		add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, toldregs[i]));
	}
	ts->numfuncresults = 0;

	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, REG_WPTR, ARG_REG, tmpreg));
	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, adjust << WSH, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR));
	add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_REG | ARG_IND, tmpreg));

	return;
}
/*}}}*/
/*{{{  static void compose_funcresults_i386 (tstate *ts, int nresults)*/
/*
 *	generates the other half of a FUNCTION return, picking up values set by compose_return
 */
static void compose_funcresults_i386 (tstate *ts, int nresults)
{
	int i;

	for (i=0; i<nresults; i++) {
		tstack_push (ts->stack);
		add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REG, ts->stack->a_reg));
	}
	return;
}
/*}}}*/
/*{{{  static void compose_entry_prolog_i386 (tstate *ts)*/
/*
 *	generates the standard prologue code for a main module, done here
 *	to abstract away kernel-interface initialisation
 */
static void compose_entry_prolog_i386 (tstate *ts)
{
	rtl_chain *trtl;
	char sbuffer[128];
	int tmp_reg;

	trtl = new_rtl ();
	trtl->type = RTL_PUBLICSETNAMEDLABEL;
	sprintf (sbuffer, "%s_occam_start", (options.extref_prefix ? options.extref_prefix : ""));
	trtl->u.label_name = string_dup (sbuffer);
	add_to_rtl_chain (trtl);

	if (!(options.kernel_interface & KRNLIFACE_MP)) {
		/*{{{  setup stack frame with EntryPointTable*/
		int new_reg, tmp_reg2;

		new_reg = tstack_newreg (ts->stack);
		tmp_reg = tstack_newreg (ts->stack);
		tmp_reg2 = tstack_newreg (ts->stack);

		add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_CONST | ARG_ISCONST, 768, ARG_REG, REG_SPTR, ARG_REG, REG_SPTR));
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_ESI));
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg2, ARG_REG, REG_EDI));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_SPTR, ARG_REG, tmp_reg2));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL | ARG_ISCONST, string_dup ("&EntryPointTable"), ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 191, ARG_REG, new_reg));

		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, new_reg, ARG_REG, REG_ECX));
		add_to_ins_chain (compose_ins (INS_REPMOVEL, 2, 1, ARG_REG | ARG_IMP, new_reg, ARG_REG | ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, tmp_reg2));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, new_reg));

		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg2));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		flush_ins_chain ();

		add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_CONST | ARG_ISCONST, KIFACE_TABLEOFFS_I386 << 2, ARG_REG, REG_SPTR, ARG_REG, REG_SPTR));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup ("&Wptr"), ARG_REG, REG_WPTR));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup ("&Fptr"), ARG_REG, REG_FPTR));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup ("&Bptr"), ARG_REG, REG_BPTR));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_NAMEDLABEL, string_dup ("&occam_finished")));

		tmp_reg = tstack_newreg (ts->stack);
		ts->stack->old_a_reg = tmp_reg;
		ts->stack->old_ts_depth = 1;
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_SPTR, ARG_REG, ts->stack->old_a_reg));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, KIFACE_TABLEOFFS_I386 << 2, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_a_reg));
		compose_kcall_i386 (ts, K_RTTHREADINIT, 1, 0);
		/*}}}*/
	}

	compose_kcall_i386 (ts, K_PAUSE, 0, 0);
	flush_ins_chain ();

	return;
}
/*}}}*/
/*{{{  static void compose_rmox_entry_prolog_i386 (tstate *ts, rmoxmode_e rmode)*/
/*
 *	generates the standard prologue code for an RMoX main module.
 */
static void compose_rmox_entry_prolog_i386 (tstate *ts, rmoxmode_e rmode)
{
	char sbuffer[128];

	sprintf (sbuffer, "RMoX entry prolog for mode %d", (int)rmode);
	add_to_ins_chain (compose_ins (INS_ANNO, 1, 0, ARG_TEXT, string_dup (sbuffer)));

	flush_ins_chain ();

	return;
}
/*}}}*/
/*{{{  static void compose_fp_set_fround_i386 (tstate *ts, int mode)*/
/*
 *	sets FPU rounding mode (uses stack)
 */
static void compose_fp_set_fround_i386 (tstate *ts, int mode)
{
	if (mode == ts->stack->fpu_mode) {
		return;
	}

	compose_fp_load_control_i386 (ts, mode);

	ts->stack->fpu_mode = mode;
	return;
}
/*}}}*/
/*{{{  static void compose_reset_fregs_i386 (tstate *ts)*/
/*
 *	resets FPU registers
 */
static void compose_reset_fregs_i386 (tstate *ts)
{
	if (ts->stack->fs_depth < 0) {
		fprintf (stderr, "%s: error: ts->stack->fs_depth = %d\n", progname, ts->stack->fs_depth);
		ts->stack->fs_depth = 0;
		return;
	}
	while (ts->stack->fs_depth) {
		add_to_ins_chain (compose_ins (INS_FSTP, 0, 1, ARG_FREG, 0));
/*		generate_set_fround (ts, FPU_N); */
		ts->stack->fs_depth--;
	}
	ts->stack->fpu_mode = FPU_NONE;
	return;
}
/*}}}*/
#if 0
/*{{{  static void compose_refcountop_i386 (tstate *ts, int op, int reg)*/
/*
 *	performs a reference count operation
 */
static void compose_refcountop_i386 (tstate *ts, int op, int reg)
{
	switch (op) {
		/*{{{  I_RCINIT -- initialise reference count (to 1)*/
		case I_RCINIT:
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST | ARG_ISCONST, 1, ARG_REGIND, reg));
			break;
		/*}}}*/
		/*{{{  I_RCINC -- increment reference count*/
		case I_RCINC:
			if (options.mpenable) {
				add_to_ins_chain (compose_ins (INS_LOCK, 0, 0));
			}
			add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, 1, ARG_REGIND, reg, ARG_REGIND, reg));
			ts->stack->must_set_cmp_flags = 1;
			break;
		/*}}}*/
		/*{{{  I_RCDEC -- decrement reference count and test zero*/
		case I_RCDEC:
			if (options.mpenable) {
				add_to_ins_chain (compose_ins (INS_LOCK, 0, 0));
			}
			add_to_ins_chain (compose_ins (INS_SUB, 2, 2, ARG_CONST | ARG_ISCONST, 1, ARG_REGIND, reg, ARG_REGIND, reg, ARG_REG | ARG_IMP, REG_CC));
			add_to_ins_chain (compose_ins (INS_SETCC, 1, 1, ARG_COND, CC_NZ, ARG_REG, reg));
			add_to_ins_chain (compose_ins (INS_AND, 1, 1, ARG_CONST | ARG_ISCONST, 1, ARG_REG, reg));
			constmap_remove (reg);
			ts->stack->must_set_cmp_flags = 0;
			break;
		/*}}}*/
		/*{{{  default -- error*/
		default:
			fprintf (stderr, "%s: compose_refcountop_i386: warning: unknown instruction %d (ignoring)\n", progname, op);
			break;
		/*}}}*/
	}
	return;
}
/*}}}*/
#endif
/*{{{  static void compose_memory_barrier_i386 (tstate *ts, int sec)*/
/*
 *	generates memory barrier instructions
 */
static void compose_memory_barrier_i386 (tstate *ts, int sec)
{
	if (options.machine_options & OPTION_SSE2) {
		switch (sec) {
			case I_MB:
				add_to_ins_chain (compose_ins (INS_MB, 0, 0));
				break;
			case I_RMB:
				add_to_ins_chain (compose_ins (INS_RMB, 0, 0));
				break;
			case I_WMB:
				add_to_ins_chain (compose_ins (INS_WMB, 0, 0));
				break;
		}
	} else {
		add_to_ins_chain (compose_ins (INS_LOCK, 0, 0));
		add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST | ARG_ISCONST, 0, ARG_REGIND, REG_SPTR, ARG_REGIND, REG_SPTR));
	}
	tstack_undefine (ts->stack);
	constmap_clearall ();
}
/*}}}*/
/*{{{  static int regcolour_special_to_real_i386 (int sreg)*/
/*
 *	int regcolour_special_to_real_i386 (int sreg)
 *	turns a special register into a real register
 */
static int regcolour_special_to_real_i386 (int sreg)
{
	switch (sreg) {
	case REG_ALT_EAX:
		return REG_EAX;
		break;
	case REG_ALT_EBX:
		return REG_EBX;
		break;
	case REG_ALT_ECX:
		return REG_ECX;
		break;
	case REG_ALT_EDX:
		return REG_EDX;
		break;
	case REG_ALT_EDI:
		return REG_EDI;
		break;
	case REG_WPTR:
		return REG_EBP;
		break;
	case REG_JPTR:
		return REG_ESI;
		break;
	case REG_LPTR:
		return REG_EDI;
		break;
	case REG_SPTR:
		return REG_ESP;
		break;
	case REG_SCHED:
		return REG_ESI;
		break;
	}
	return sreg;
}
/*}}}*/
/*{{{  static int regcolour_get_regs_i386 (int *regs)*/
/*
 *	void regcolour_get_regs_i386 (int *regs)
 *	populates an array with registers which will be used for allocation
 */
static int regcolour_get_regs_i386 (int *regs)
{
	static int r_names[RMAX_I386] = {REG_EAX, REG_EDX, REG_ECX, REG_EDI};

	memcpy (regs, r_names, RMAX_I386 * sizeof (int));

	return RMAX_I386;
}
/*}}}*/
/*{{{  static char *get_register_name_i386 (int reg)*/
/*
 *	returns a string with the given register name in
 */
static char *get_register_name_i386 (int reg)
{
	static char *mainregs[] = {"%eax", "%ebx", "%ecx", "%edx", "%esp", "%ebp", "%esi", "%edi"};
	static char *altregs[] = {"%eax", "%ebx", "%ecx", "%edx", "%edi"};
	static char *unkreg = "?";

	if ((reg >= 0) && (reg <= 7)) {
		return mainregs[reg];
	} else if ((reg >= -128) && (reg <= -124)) {
		return altregs[reg + 128];
	} else {
		return unkreg;
	}
}
/*}}}*/
/*{{{  static int rtl_validate_instr_i386 (ins_chain *ins)*/
/*
 *	int rtl_validate_instr (ins_chain *ins)
 *	validates a single instruction
 *	returns 1 if valid instruction, 0 if not
 */
static int rtl_validate_instr_i386 (ins_chain *ins)
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
		if (!rtl_validate_checkargtype (ins->in_args[0], ARG_REGINDSIB, ARG_CONST, ARG_REG, ARG_REGIND, ARG_FLABEL, ARG_BLABEL, ARG_NAMEDLABEL, ARG_LABEL, ARG_INSLABEL, ARG_NAMEDLABEL, 0)) {
			fprintf (stderr, "error: MOVE input of invalid type %d\n", ins->in_args[0]->flags & ARG_MODEMASK);
			return 0;
		}
		if (!rtl_validate_checkargtype (ins->out_args[0], ARG_REG, ARG_REGIND, ARG_REGINDSIB, ARG_NAMEDLABEL, 0)) {
			fprintf (stderr, "error: MOVE output of invalid type %d\n", ins->out_args[0]->flags & ARG_MODEMASK);
			return 0;
		}
		if (((ins->in_args[0]->flags & ARG_MODEMASK) == ARG_REGIND) && ((ins->out_args[0]->flags & ARG_MODEMASK) == ARG_REGIND)) {
			fprintf (stderr, "error: MOVE has two memory operands\n");
			return 0;
		}
		break;
	case INS_NOP:
		if (!rtl_validate_checknumargs (ins, 0, 0)) {
			return 0;
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
	case INS_CJUMP:
		{
			ins_chain *setting = rtl_scan_setscc_backward (ins, 1);

			if (!setting && !(options.debug_options & (DEBUG_OVERFLOW | DEBUG_RANGESTOP | DEBUG_FLOAT | DEBUG_MEMCHK))) {
				fprintf (stderr, "warning: didn\'t find a CC-setting instruction before CJUMP (cond %d to ", ArgCC (ins->in_args[0]));
				switch (ArgMode (ins->in_args[1])) {
				case ARG_LABEL:
					fprintf (stderr, "L%d", ArgLabel (ins->in_args[1]));
					break;
				case ARG_FLABEL:
					fprintf (stderr, "%df", ArgLabel (ins->in_args[1]));
					break;
				case ARG_BLABEL:
					fprintf (stderr, "%db", ArgLabel (ins->in_args[1]));
					break;
				case ARG_NAMEDLABEL:
					fprintf (stderr, "%s", ArgName (ins->in_args[1]));
					break;
				case ARG_INSLABEL:
					fprintf (stderr, "IL%d", ArgLabel ((ArgInsLab (ins->in_args[1]))->in_args[0]));
					break;
				default:
					fprintf (stderr, "..=%d", ArgMode (ins->in_args[1]));
					break;
				}
				fprintf (stderr, ")\n");

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
	case INS_LOCK:
		/* FIXME: validate the following instruction for compatibility... hard... */
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
	case INS_MOVEZEXT16TO32:
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
	case INS_FSIN:
	case INS_FCOS:
	case INS_FPTAN:
	case INS_MB:
	case INS_RMB:
	case INS_WMB:
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
/*{{{  static int rtl_prevalidate_instr_i386 (ins_chain *ins)*/
static int rtl_prevalidate_instr_i386 (ins_chain *ins)
{
	/* nothing to do for the i386 (base architecture..) */
	return 1;
}
/*}}}*/
/*{{{  arch_t *init_arch_i386 (int mclass)*/
/*
 *	arch_t *init_arch_i386 (int mclass)
 *	initialises and returns architecture definition
 */
arch_t *init_arch_i386 (int mclass)
{
	arch_t *arch = (arch_t *)smalloc (sizeof (arch_t));

	/* setup statics */
	Wptr_name = string_dup ("&Wptr");
	Fptr_name = string_dup ("&Fptr");
	Bptr_name = string_dup ("&Bptr");
	PPriority_name = string_dup ("&PPriority");


	/* common */
	arch->archname = string_dup ("i386");

	/* kernel calls */
	arch->compose_kcall = compose_kcall_i386;
	arch->compose_kjump = compose_kjump_i386;
	arch->compose_deadlock_kcall = compose_deadlock_kcall_i386;

	/* other stuff */
	arch->compose_pre_enbc = compose_pre_enbc_i386;
	arch->compose_pre_enbt = compose_pre_enbt_i386;

	/* inlining */
	arch->compose_inline_ldtimer = compose_inline_ldtimer_i386;
	arch->compose_inline_tin = compose_inline_tin_i386;
	arch->compose_inline_quick_reschedule = compose_inline_quick_reschedule_i386;
	arch->compose_inline_full_reschedule = compose_inline_full_reschedule_i386;
	#ifdef PROCESS_PRIORITY
		arch->compose_inline_in = compose_inline_in_i386;
		arch->compose_inline_in_2 = NULL;
		arch->compose_inline_min = NULL;
		arch->compose_inline_out = compose_inline_out_i386;
		arch->compose_inline_out_2 = NULL;
		arch->compose_inline_mout = NULL;
	#else
		arch->compose_inline_in = compose_inline_in_i386;
		arch->compose_inline_in_2 = compose_inline_in_2_i386;
		arch->compose_inline_min = compose_inline_min_i386;
		arch->compose_inline_out = compose_inline_out_i386;
		arch->compose_inline_out_2 = compose_inline_out_2_i386;
		arch->compose_inline_mout = compose_inline_mout_i386;
	#endif
	arch->compose_inline_enbc = compose_inline_enbc_i386;
	arch->compose_inline_disc = compose_inline_disc_i386;
	arch->compose_inline_altwt = compose_inline_altwt_i386;
	arch->compose_inline_stlx = compose_inline_stlx_i386;
	arch->compose_inline_malloc = compose_inline_malloc_i386;
	arch->compose_inline_startp = compose_inline_startp_i386;
	#ifdef PROCESS_PRIORITY
		arch->compose_inline_endp = NULL;
	#else
		arch->compose_inline_endp = compose_inline_endp_i386;
	#endif
	arch->compose_inline_runp = NULL;
	arch->compose_inline_stopp = compose_inline_stopp_i386;

	/* debugging */
	arch->compose_debug_insert = compose_debug_insert_i386;
	arch->compose_debug_procnames = compose_debug_procnames_i386;
	arch->compose_debug_filenames = compose_debug_filenames_i386;
	arch->compose_debug_zero_div = compose_debug_zero_div_i386;
	arch->compose_debug_floaterr = compose_debug_floaterr_i386;
	arch->compose_debug_overflow = compose_debug_overflow_i386;
	arch->compose_debug_rangestop = compose_debug_rangestop_i386;
	arch->compose_debug_seterr = compose_debug_seterr_i386;
	arch->compose_overflow_jumpcode = compose_overflow_jumpcode_i386;
	arch->compose_floaterr_jumpcode = compose_floaterr_jumpcode_i386;
	arch->compose_rangestop_jumpcode = compose_rangestop_jumpcode_i386;

	arch->compose_debug_deadlock_set = compose_debug_deadlock_set_i386;

	/* code-gen */
	arch->compose_divcheck_zero = compose_divcheck_zero_i386;
	arch->compose_divcheck_zero_simple = compose_divcheck_zero_simple_i386;
	arch->compose_division = compose_division_i386;
	arch->compose_remainder = compose_remainder_i386;
	arch->compose_iospace_loadbyte = compose_iospace_loadbyte_i386;
	arch->compose_iospace_storebyte = compose_iospace_storebyte_i386;
	arch->compose_iospace_loadword = NULL;
	arch->compose_iospace_storeword = NULL;
	arch->compose_iospace_write = compose_iospace_write_i386;
	arch->compose_iospace_read = compose_iospace_read_i386;
	arch->compose_move = compose_move_i386;
	arch->compose_move_loadptrs = compose_move_loadptrs_i386;
	arch->compose_shift = compose_shift_i386;

	arch->compose_widenshort = compose_widenshort_i386;
	arch->compose_widenword = compose_widenword_i386;
	arch->compose_longop = compose_longop_i386;
	arch->compose_fpop = compose_fpop_i386;
	arch->compose_external_ccall = compose_external_ccall_i386;
	arch->compose_bcall = compose_bcall_i386;
	arch->compose_cif_call = compose_cif_call_i386;

	arch->compose_entry_prolog = compose_entry_prolog_i386;
	arch->compose_rmox_entry_prolog = compose_rmox_entry_prolog_i386;
	arch->compose_fp_set_fround = compose_fp_set_fround_i386;
	arch->compose_fp_init = compose_fp_init_i386;
	arch->compose_reset_fregs = compose_reset_fregs_i386;

	/* arch->compose_refcountop = compose_refcountop_i386; */
	arch->compose_memory_barrier = compose_memory_barrier_i386;

	arch->compose_return = compose_return_i386;
	arch->compose_nreturn = compose_nreturn_i386;
	arch->compose_funcresults = compose_funcresults_i386;

	/* register allocation */
	arch->regcolour_special_to_real = regcolour_special_to_real_i386;
	arch->regcolour_rmax = RMAX_I386;
	arch->regcolour_nodemax = NODEMAX_I386;
	arch->regcolour_get_regs = regcolour_get_regs_i386;
	arch->regcolour_fp_regs = NULL;

	/* output generation */
	arch->code_to_asm = dump_asm386;
	arch->code_to_asm_stream = dump_asm386_stream;

	/* RTL stuff */
	arch->rtl_validate_instr = rtl_validate_instr_i386;
	arch->rtl_prevalidate_instr = rtl_prevalidate_instr_i386;
	arch->get_register_name = get_register_name_i386;

	arch->int_options = 0;
	arch->kiface_tableoffs = KIFACE_TABLEOFFS_I386;

	return arch;
}
/*}}}*/



