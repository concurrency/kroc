/*
 *	archmips.c -- MIPS architecture stuff
 *	Copyright (C) 2002-2003 Fred Barnes and Christian Jacobsen  <frmb@kent.ac.uk>
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

/*{{{  includes, etc.*/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
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
#include "mips.h"
#include "tstate.h"
#include "stubtable.h"
#include "archdef.h"
#include "rtlops.h"
#include "etcdump.h"
#include "etcrtl.h"
#include "asmmips.h"
#include "kif.h"

#ifndef EXIT_FAILURE
	#define EXIT_FAILURE 1
#endif  /* !EXIT_FAILURE */
/*}}}*/

#define RMAX_MIPS 8
#define NODEMAX_MIPS 256

#if 0
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
#endif

/*{{{  static void compose_kcall_mips (tstate *ts, int call, int regs_in, int regs_out)*/
/*
 *	void compose_kcall_mips (tstate *ts, int call, int regs_in, int regs_out)
 *	creates a kernel-call (constraining registers appropriately)
 */
static void compose_kcall_mips (tstate *ts, int call, int regs_in, int regs_out)
{
	kif_entrytype *entry = kif_entry (call);
	int to_preserve, r_in, r_out;
	int cregs[3], xregs[4];

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
	/* FIXME: We can do more regs on the mips than this... */
	xregs[0] = 4; /* REG_EAX */
	xregs[1] = 5; /* REG_EBX */
	xregs[2] = 6; /* REG_ECX */
	xregs[3] = 7; /* REG_EDX */
	#if 0 /* this code is just a duplicate of i386 */
	for (i = (r_in - 1); i >= 0; i--) {
		if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
			/* some things still require the operands to be pushed.. */
			switch (entry->input_mode) {
			case ARGS_ON_STACK:
				switch (constmap_typeof (cregs[i])) {
				default:
					add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, cregs[i]));
					break;
				case VALUE_CONST:
					add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_CONST, constmap_regconst (cregs[i])));
					break;
				case VALUE_LABADDR:
					add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_LABEL | ARG_ISCONST, constmap_regconst (cregs[i])));
					break;
				case VALUE_LOCAL:
					add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REGIND | ARG_DISP, REG_WPTR, (constmap_regconst (cregs[i])) << WSH));
					break;
				}
				break;
			case ARGS_IN_REGS:
				add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, cregs[i], ARG_REG, xregs[i]));
				break;
			default:
				fprintf (stderr, "%s: error: call %d attempting to provide arguments to kernel call\n", progname, call);
				break;
			}
		} else if (options.kernel_interface & KRNLIFACE_MESH) {
			fprintf (stderr, "%s: warning: MESH kernel interface not supported yet (ungenerated kernel call %d)\n", progname, call);
		} else if (options.kernel_interface & KRNLIFACE_CSPLINUX) {
			fprintf (stderr, "%s: warning: CSP/Linux interface not supported yet (ungenerated kernel call %d)\n", progname, call);
		}
	}
	/* generate call */
	tmp_ins = NULL;			/* used for adding implied regs later */
	tmp_ins2 = NULL;
	if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
		switch (entry->call_mode) {
		case KCALL_CALL:
			/* call the function by name */
			/* FIXME: User Defined Channel stuff goes in here */
			{
				tmp_ins = compose_ins (INS_CALL, 1, 0, ARG_NAMEDLABEL, string_dup (entry->entrypoint));
				add_to_ins_chain (tmp_ins);
			}
			break;
		case KCALL_STOREIP_JUMP:
			/* only for descheduling points */
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 0, ARG_REGIND | ARG_DISP, REG_WPTR, -4));
			/* FIXME: User Defined Channel stuff goes in here */
			{
				tmp_ins = compose_ins (INS_JUMP, 1, 0, ARG_NAMEDLABEL, string_dup (entry->entrypoint));
				add_to_ins_chain (tmp_ins);
			}
			add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
			break;
		case KCALL_REGIP_JUMP:
			/* EXPERIMENTAL */
			tmp_reg = tstack_newreg (ts->stack);
			add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_FLABEL | ARG_ISCONST, 0, ARG_REG, tmp_reg));
			add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, xregs[r_in]));
			tmp_ins = compose_ins (INS_JUMP, 1, 0, ARG_NAMEDLABEL, string_dup (entry->entrypoint));
			set_implied_inputs (tmp_ins, 1, &tmp_reg);
			add_to_ins_chain (tmp_ins);
			add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
			add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
			break;
		case KCALL_JUMP:
			/* jump to the function by name */
			tmp_ins = compose_ins (INS_CALL, 1, 0, ARG_NAMEDLABEL, string_dup (entry->entrypoint));
			add_to_ins_chain (tmp_ins);
			break;
		default:
			fprintf (stderr, "%s: error: not supposed to be calling this (%d)\n", progname, call);
			break;
		}
		if (tmp_ins && (entry->input_mode == ARGS_IN_REGS)) {
			set_implied_inputs (tmp_ins, r_in, cregs);
			if (tmp_ins2) {
				set_implied_inputs (tmp_ins2, r_in, cregs);
			}
		}
	} else if (options.kernel_interface & KRNLIFACE_MESH) {
		/* unsupported as yet */
		fprintf (stderr, "%s: warning: MESH kernel interface not supported yet (ungenerated kernel call %d)\n", progname, call);
	} else if (options.kernel_interface & KRNLIFACE_CSPLINUX) {
		/* unsupported as yet */
		fprintf (stderr, "%s: warning: CSP/Linux kernel interface not supported yet (ungenerated kernel call %d)\n", progname, call);
	}
	if ((options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) && (entry->input_mode == ARGS_IN_REGS)) {
		/* unconstrain regs */
		for (i=0; i<r_in; i++) {
			add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, cregs[i]));
		}
	}

	/* clean up and collect results */
	if (regs_out > 0) {
		tstack_undefine (ts->stack);
		for (i=0; i<r_out; i++) {
			tstack_push (ts->stack);
			ts->stack->must_set_cmp_flags = 1;
			oregs[i] = ts->stack->a_reg;
		}
		ts->stack->ts_depth = r_out;
		for (i=ts->stack->ts_depth; i<3; i++) {
			oregs[i] = REG_UNDEFINED;
		}
		ts->stack->a_reg = oregs[0];
		ts->stack->b_reg = oregs[1];
		ts->stack->c_reg = oregs[2];
		if (tmp_ins && (entry->output_mode == ARGS_IN_REGS)) {
			set_implied_outputs (tmp_ins, r_out, oregs);
		}
		/* constrain into registers */
		for (i=0; i<r_out; i++) {
			if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
				switch (entry->output_mode) {
				case ARGS_ON_STACK:
					add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REG, oregs[i]));
					break;
				case ARGS_IN_REGS:
					add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, oregs[i], ARG_REG, xregs[i]));
					add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, oregs[i]));
					break;
				default:
					fprintf (stderr, "%s: error: call %d attempting to get output from kernel call\n", progname, call);
					break;
				}
			} else if (options.kernel_interface & KRNLIFACE_MESH) {
				fprintf (stderr, "%s: warning: MESH kernel interface not supported yet (ungenerated kernel output from call %d)\n", progname, call);
			}
		}
	}

	#endif
	return;
}
/*}}}*/
/*{{{  static void compose_deadlock_kcall_mips (tstate *ts, int call, int regs_in, int regs_out)*/
/*
 *	void compose_deadlock_kcall_mips (tstate *ts, int call, int regs_in, int regs_out)
 *	creates a kernel-call with deadlock info (constraining registers appropriately)
 */
static void compose_deadlock_kcall_mips (tstate *ts, int call, int regs_in, int regs_out)
{
	int this_lab;
	unsigned int x;

	if (options.debug_options & DEBUG_DEADLOCK) {
		/* set own WPTR in link-field */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_WPTR, ARG_REGIND | ARG_DISP, REG_WPTR, W_LINK));
	}
	compose_kcall_mips (ts, call, regs_in, regs_out);
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
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
	}
	return;
}
/*}}}*/


/*{{{  static void compose_debug_insert_mips (tstate *ts, int mdpairid)*/
/*
 *	void compose_debug_insert_mips (tstate *ts, int mdpairid)
 *	generates an insert-debugging thing
 */
static void compose_debug_insert_mips (tstate *ts, int mdpairid)
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
/*{{{  static void compose_debug_procnames_mips (tstate *ts)*/
/*
 *	void compose_debug_procnames_mips (tstate *ts)
 *	generates procedure-name list for post-mortem debugging
 */
static void compose_debug_procnames_mips (tstate *ts)
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
/*{{{  static void compose_debug_filenames_mips (tstate *ts)*/
/*
 *	void compose_debug_filenames_mips (tstate *ts)
 *	generates file-name list for post-mortem debugging
 */
static void compose_debug_filenames_mips (tstate *ts)
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


/*{{{ int regcolour_special_to_real_mips (int sreg)*/
/*
 *	turns special registers into real registers
 */
static int regcolour_special_to_real_mips (int sreg)
{
	switch(sreg) {
		case REG_WPTR:
			return 8;
		case REG_JPTR:
			return 9;
		case REG_LPTR:
			return 10;
		case REG_SPTR:
			return 29;
	}

	return sreg;
}
/*}}}*/
/*{{{  int regcolour_get_regs_mips (int *regs)*/
/*
 *	populates an array with names of registers used for allocation
 */
static int regcolour_get_regs_mips (int *regs)
{
	static int r_names[RMAX_MIPS] = {11, 12, 13, 14, 15, -1, -1, -1};

	memcpy (regs, r_names, RMAX_MIPS * sizeof (int));

	return RMAX_MIPS;
}
/*}}}*/
/*{{{  static char *get_register_name_mips (int reg)*/
/*
 *	returns a string representing a register name
 */
static char *get_register_name_mips (int reg)
{
	static char *mainregs[] = {"%r11", "%r12", "%r13", "%r14", "%r15", "?", "?", "?"};
	static char *unkreg = "?";

	if ((reg >= 0) && (reg <= 7)) {
		return mainregs[reg];
	} else {
		return unkreg;
	}
}
/*}}}*/


/*{{{*/static ins_chain *add_to_tmp_ins_chain(ins_chain *ins, ins_chain *new_ins)
{
	if(ins == NULL)
		return ins = new_ins;

	ins->next = new_ins;
	new_ins->prev = ins;

	return new_ins;
}
/*}}}*/
/*{{{ static void swap_in_args(ins_chain *ins, int a, int b) 
 * Swap in_args[a] and in_args[b]
 * This is for certain arithmetic instructions 
 */
static void swap_in_args(ins_chain *ins, int a, int b)
{
	ins_arg *tmp_arg;

	/* Just swap the pointers */
	tmp_arg = ins->in_args[a];
	ins->in_args[a] = ins->in_args[b];
	ins->in_args[b] = tmp_arg;
}
/*}}}*/
#if 0
/*{{{ static void swap_imm_reg_args(ins_chain *ins) 
 * Swap args if in[0] is ARG_CONST, cos only src2 can be imm on MIPS 
 * This is for certain arithmetic instructions 
 */
static void swap_imm_reg_args(ins_chain *ins)
{
	ins_arg *tmp_arg;

	if(ArgMode(ins->in_args[0]) == ARG_CONST)
	{
		/* Just swap the pointers */
		tmp_arg = ins->in_args[0];
		ins->in_args[0] = ins->in_args[1];
		ins->in_args[1] = tmp_arg;
	}
}
/*}}}*/
#endif

/*{{{*/static ins_chain *find_next_ins_backward(ins_chain *ins)
{
	ins_chain * tmp_ins;

	for(tmp_ins = ins->prev; tmp_ins && (tmp_ins->type < INS_FIRST || tmp_ins->type > INS_LAST); tmp_ins = tmp_ins->prev);

	return tmp_ins;
}
/*}}}*/
/*{{{*/static ins_chain *find_next_ins_forward(ins_chain *ins)
{
	ins_chain *tmp_ins;

	for(tmp_ins = ins->next; tmp_ins && (tmp_ins->type < INS_FIRST || tmp_ins->type > INS_LAST); tmp_ins = tmp_ins->next);

	return tmp_ins;
}
/*}}}*/
#if 0
/*{{{ static int expand_args_to_regs(ins_chain *ins)
 *
 * Expands the arguments of an instruction into purely using direct
 * registers. Most instructions on the MIPS must work on registers
 * directly.
 */
static int expand_args_to_regs(ins_chain *ins)
{
	int i, new_reg;
	ins_chain *tmp_ins;

	/* Check through all in args, and make sure they are regs */
	for(i = 0; ins->in_args[i]; i++)
	{
		/* If the arg is a reg or it is implied do nothing */
		if(ArgMode(ins->in_args[i]) != ARG_IMP && ArgMode(ins->in_args[i]) != ARG_REG)
		{
			new_reg = rtl_get_newvreg();

			tmp_ins = compose_ins(INS_MOVE, 1, 1, ARG_REG, 0, ARG_REG, new_reg);
			/* FIXME: Will this bugger up anything:??? should not think so */
			tmp_ins->in_args[0] = ins->in_args[i]; /* We are not using this arg in ins anymore */
			ins->in_args[i] = new_ins_arg();
			ins->in_args[i]->flags = ARG_REG;
			ins->in_args[i]->regconst = new_reg;

			rtl_insert_instr_before(tmp_ins, ins);
		}
	}

	/* FIXME: Do the same for out_args */
	return 1;
}/*}}}*/
#endif
/*{{{*/static ins_chain *find_and_check_setcc_regs(ins_chain *ins)
/*
 * Oh, this is so ugly, possibly... In this case we have a SETCC
 * instruction just after a label, which means that the test could really
 * be from any number of instructions. I am going to assume that the
 * instruction whichs condition we are testing is the one immediately
 * before the label, or the one immediately before the first jump to the
 * label we find by searching backwards.  Basically I am just going to
 * test that the two instructions whichs results I am testing leave their
 * results in the same register. If this is the case, then we should be
 * fine doing a SETCC with a condition using that register.
 *
 * We could possibly just assume that the register which the result of
 * the instruction above the label is the one which we need. I still feel
 * safer checking it though.
 */
{
	ins_chain *cur_ins;
	ins_chain *cond_ins;
	int cond_reg;
	
	if(ins->type != INS_SETLABEL)
	{
		fprintf (stderr, "error: find_and_check_setcc_regs needs a setlabel as first ins\n");
		return NULL;
	}

	/* Right, found a label, now look at the ins above */
	cond_ins = find_next_ins_backward(ins);
	if(cond_ins->type != INS_AND && cond_ins->type != INS_OR && 
			cond_ins->type != INS_XOR && cond_ins->type != INS_SUB)
	{
		fprintf (stderr, 
				"error: find_and_check_setcc_regs did not find AND, OR, XOR, SUB\n");
		return NULL;
	}
	/* Extract the output register */
	cond_reg = ArgReg(cond_ins->out_args[0]);
	/* Constrain this register */
	rtl_insert_instr_before (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ArgReg(cond_ins->out_args[0]), ARG_REG, MIPS_KROC_TMP_REG), cond_ins);
	rtl_insert_instr_after (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ArgReg(cond_ins->out_args[0])), cond_ins);

	/* Look back and find references to the label at instruction ins */
	cur_ins = cond_ins;
	while(cur_ins != NULL)
	{
		/* Find the next CJUMP */
		while( (cur_ins != NULL) && cur_ins->type != INS_CJUMP)
			cur_ins = find_next_ins_backward(cur_ins);

		/* If we did not run off the end */
		if(cur_ins != NULL)
		{
			/* Check if the CJUMP references the label */
			if((ArgMode(cur_ins->in_args[1]) == ARG_LABEL || 
					ArgMode(cur_ins->in_args[1]) == ARG_FLABEL) && 
					ArgLabel(cur_ins->in_args[1]) == ArgLabel(ins->in_args[0]))
			{
				/* If it jumps to ins, then check the instruction before which should
				 * set the condition for our setCC */
				/* FIXME: Sometimes there is a prior to a jump to the setcc. This could be
				 * bad if anything actually jumps to this, as this could mean that we may not
				 * be dealing corretly with condition codes set by that jump. I am however ignoring
				 * this case for now, and just emitting a warning if there happens to be a label.
				 * I can then go and check things by hand... pain in the arse, but oh well.
				 *
				 * It looks like not all labels used as markers for .CONTRSPLIT get removed :(
				 * These are the labels we see here
				 */
				cur_ins = find_next_ins_backward(cur_ins);
				while(cur_ins->type == INS_SETFLABEL || cur_ins->type == INS_SETLABEL)
				{
					cur_ins = find_next_ins_backward(cur_ins);
					if(cur_ins->type == INS_SETLABEL)
					{
						fprintf (stderr, 
								"warning: while finding SETcc regs, found LABEL after CJUMP. Ignoring. \n");
					}
					else
					{
						fprintf (stderr, 
								"warning: while finding SETcc regs, found FLABEL after CJUMP. Ignoring. \n");
					}
				}

				if(cur_ins->type != INS_AND && cur_ins->type != INS_OR && 
						cur_ins->type != INS_XOR && cur_ins->type != INS_SUB)
				{
					fprintf (stderr, 
							"error: find_and_check_setcc_regs did not find AND, OR, XOR, SUB\n");
					return NULL;
				}
	
				/*
				 * We now constrain rather than just dying
				 *
				if(ArgReg(cur_ins->out_args[0]) != cond_reg)
				{
					fprintf (stderr, "error: find_and_check_setcc_regs regs do not match up\n");
					return NULL;
				}
				*/

				/* We must now have found an instruction whichs output value we need, constrain */
				rtl_insert_instr_before (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ArgReg(cur_ins->out_args[0]), ARG_REG, MIPS_KROC_TMP_REG), cur_ins);
				rtl_insert_instr_after (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ArgReg(cur_ins->out_args[0])), cur_ins);
			}
			else
				cur_ins = find_next_ins_backward(cur_ins);
		}
	}
	/* If we get here we should be fine. */
	return cond_ins;
}
/*}}}*/
/*{{{  static void dump_ins_info (char *str, ins_chain *ins)*/
/*
 *	Dumps a bit of usefull info about an instruction
 */
static void dump_ins_info(char *str, ins_chain *ins)
{
	int i;
/* FIXME: This is only some temporary debug output... However it is now at least possible
	 * to turn it off (by not giving the -v flag... This needs to go when things are working
	 * better though...
	 */
	if(options.verbose)
	{
		fprintf(stderr, str);
		fprintf(stderr, " ");
		if(ins->type)
		{
			fprintf(stderr, "%d ", ins->type);
			for(i = 0; i < 6; i++)
				if(ins->in_args[i])
					fprintf(stderr, "in[%d]=%d ", i, ins->in_args[i]->flags & ARG_MODEMASK);
			for(i = 0; i < 6; i++)
				if(ins->out_args[i])
					fprintf(stderr, "out[%d]=%d ", i, ins->out_args[i]->flags & ARG_MODEMASK);

		}
		else
			fprintf(stderr, "null instruction (?)");
		fprintf(stderr, "\n");
	}
}
/* }}} */

/*{{{  static int rtl_prevalidate_instr_mips (ins_chain *ins)*/
/*
 *	validates MIPS instructions.  Also re-arranges stuff where necessary.
 */
static int rtl_prevalidate_instr_mips (ins_chain *ins)
{
	ins_chain *tmp_ins_s, *tmp_ins_c, *tmp_ins;
	//ins_chain *regend;
	int new_reg;

	tmp_ins_c = NULL;

	// FIXME: Temporary debug output, only shown with -v
	dump_ins_info("Prevalidating", ins);

	switch (ins->type) {
	case INS_UNDEFINED:
		fprintf (stderr, "error: undefined instruction\n");
		return 0;
	case INS_MOVE: /* 1 */
		if (!rtl_validate_checknumargs (ins, 1, 1)) {
			fprintf (stderr, "error: MOVE has wrong number of arguments\n");
			return 0;
		}
		/* FIXME: More validation thingys to do here... */
		/* Immediate -> Memory */
		// FIXME: If immediate is zero, reduce to one instruction
		switch(ArgMode(ins->in_args[0])) {
		case ARG_CONST: /* in */
			switch(ArgMode(ins->out_args[0])){
			case ARG_REGIND:     /* out */
			case ARG_TEXT:       /* out */
			case ARG_LABEL:      /* out */
			case ARG_NAMEDLABEL: /* out */
				/* Turn this from a move const, outtype into a load reg, const; store reg, outtype; */
				tmp_ins_c = tmp_ins_s = add_to_tmp_ins_chain(NULL, 
						compose_ins(INS_MOVE, 1, 1, ARG_CONST, ins->in_args[0]->regconst, ARG_REG, MIPS_KROC_TMP_REG));
				tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c, 
						compose_ins(INS_MOVE, 1, 1, ARG_REG, MIPS_KROC_TMP_REG, ins->out_args[0]->flags & ARG_MODEMASK, 0));
				tmp_ins_c->out_args[0] = rtl_copy_arg(ins->out_args[0], ins);
				rtl_insert_instr_block_after(tmp_ins_s, ins);

				ins->type = INS_CLEANUP;
			}
			break; /* in - ARG_CONST */
		case ARG_TEXT:        /* in */
		case ARG_LABEL:       /* in */
		case ARG_NAMEDLABEL:  /* in */
		case ARG_FLABEL:      /* in */
		case ARG_BLABEL:      /* in */
		case ARG_INSLABEL:    /* in */
			if(ArgMode(ins->out_args[0]) == ARG_REG)
				return 1;
			tmp_ins_c = tmp_ins_s = add_to_tmp_ins_chain(NULL,
					compose_ins(INS_MOVE, 1, 1, ARG_CONST, 0, 
					ARG_REG, MIPS_KROC_TMP_REG));
			//tmp_ins_c->in_args[0] = rtl_copy_arg(ins->in_args[0], tmp_ins_c);
			tmp_ins_c->in_args[0]->flags = ins->in_args[0]->flags;
			tmp_ins_c->in_args[0]->regconst = ins->in_args[0]->regconst;
		 	tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
					compose_ins(INS_MOVE, 1, 1, ARG_REG, MIPS_KROC_TMP_REG, ins->out_args[0]->flags & ARG_MODEMASK, 0));
			tmp_ins_c->out_args[0] = rtl_copy_arg(ins->out_args[0], tmp_ins_c);

			rtl_insert_instr_block_after(tmp_ins_s, ins);
			ins->type = INS_CLEANUP; 
			break; /* in - ARG_TEXT, ARG_LABEL, ARG_NAMEDLABEL */
		}
		break; /* INS_MOVE */
	case INS_SETCC: /* 4 */
			/* Check to see if we have modified this SETcc instruction already, if so we are
			 * good and can continue without doing anything else.
			 */
			if(rtl_validate_checknumargs (ins, 3, 1))
			{
				return 1;
			}

			/* Otherwise we'll have to start looking back to figure out what is going to 
			 * happen here.
			 */
			tmp_ins = find_next_ins_backward(ins);
			if(ArgCC(ins->in_args[0]) == CC_Z || ArgCC(ins->in_args[0]) == CC_NZ)
			{
				/* If we got the INS_SETLABEL case, find the instruction(s) with regs to test */
				if(tmp_ins->type == INS_SETLABEL)
				{
					tmp_ins =  find_and_check_setcc_regs(tmp_ins);
					if(!tmp_ins)
					{
						return 0; /* if there was an error */
					}

					/* Put the reg we are testing into the SETCC instruction */
					ins->in_args[1] = rtl_copy_arg(tmp_ins->out_args[0], tmp_ins_c);
					/* Zero, put in the zero register */
					ins->in_args[2] = new_ins_arg();
					ins->in_args[2]->flags = ARG_REG;
					ins->in_args[2]->regconst = MIPS_REG_ZERO;

					/* Constrain the register so that we can insure that all instructions
					 * whichs output we are going to use are put in an appropriate register
					 */
					rtl_insert_instr_before (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ArgReg(ins->out_args[0]), ARG_REG, MIPS_KROC_TMP_REG), ins);
					rtl_insert_instr_after (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ArgReg(ins->out_args[0])), ins);

					return 1;
				} /* SETcc, cc=CC_Z,CC_NZ: INS_SETLABEL */
		
				/* A CMP and SUB should be equivalent */
				if(tmp_ins->type == INS_CMP || tmp_ins->type == INS_SUB)
				{
					/* Put the reg we are testing into the SETCC instruction */
					ins->in_args[1] = rtl_copy_arg(tmp_ins->in_args[0], tmp_ins);

					/* Are we testing for equality or zero? */
					if(ArgReg(tmp_ins->in_args[0]) != ArgReg(tmp_ins->in_args[1]))
					{
						/* Equality, need to supply the register */
						ins->in_args[2] = rtl_copy_arg(tmp_ins->in_args[1], tmp_ins);

						/* Check if we may need the result from an SUB */
						if(tmp_ins->type == INS_CMP)
						{
							/* We dont need the instruction which would have set cc's on intel */
							tmp_ins->type = INS_CLEANUP;
						} else if(ArgReg(tmp_ins->in_args[0]) == ArgReg(tmp_ins->out_args[0]))
						{
							/* We dont need the instruction which would have set cc's on intel */
							tmp_ins->type = INS_CLEANUP;
						}
					}
					else
					{
						/* Zero, put in the zero register */
						ins->in_args[2] = new_ins_arg();
						ins->in_args[2]->flags = ARG_REG;
						ins->in_args[2]->regconst = MIPS_REG_ZERO;
					
						/* We dont need the instruction which would have set cc's on intel */
						tmp_ins->type = INS_CLEANUP;
					}

					/* phew! */
					return 1;
				} /* SETcc, cc=CC_Z,CC_NZ: INS_CMP, INS_SUB */

				if(tmp_ins->type == INS_OR)
				{
					if(ArgReg(tmp_ins->in_args[0]) != ArgReg(tmp_ins->in_args[1]))
					{
						fprintf (stderr, 
								"error: SETcc, cc=CC_Z, OR: don't know how to deal with differing operands\n");
						return 0;
					}
					
					/* Put the reg we are testing into the SETCC instruction */
					ins->in_args[1] = rtl_copy_arg(tmp_ins->in_args[0], tmp_ins);
					/* Zero, put in the zero register */
					ins->in_args[2] = new_ins_arg();
					ins->in_args[2]->flags = ARG_REG;
					ins->in_args[2]->regconst = MIPS_REG_ZERO;

					/* Check if it looks like the value resulting from the or is used?
					 * ie check if the destination reg is same as others, if not dont
					 * cleanup instruction
					 */
					if(ArgReg(tmp_ins->in_args[0]) == ArgReg(tmp_ins->out_args[0]))
					{
						/* We dont need the instruction which would have set cc's on intel */
						tmp_ins->type = INS_CLEANUP;
					}

					return 1;
				} /* SETcc, cc=CC_Z,CC_NZ: INS_OR */

				/* FIXME: I am not quite sure what is going on here */
				if(tmp_ins->type == INS_AND)
				{
					/* Put the reg we are testing into the SETCC instruction */
					ins->in_args[1] = rtl_copy_arg(tmp_ins->out_args[0], tmp_ins);
					/* Zero, put in the zero register */
					ins->in_args[2] = new_ins_arg();
					ins->in_args[2]->flags = ARG_REG;
					ins->in_args[2]->regconst = MIPS_REG_ZERO;

					return 1;
				} /* SETcc, cc=CC_Z,CC_NZ: INS_AND */
				
				/* Clueless as to what we do now :) */
				fprintf (stderr, 
						"error: SETcc, cc=CC_Z,CC_NZ, does not how to deal with this instruction (%d)\n",
						tmp_ins->type);
				return 0;
			} /* SETcc, cc=CC_Z,CC_NZ */

			if(ArgCC(ins->in_args[0]) == CC_GT || ArgCC(ins->in_args[0]) == CC_GE || ArgCC(ins->in_args[0]) == CC_LT || ArgCC(ins->in_args[0]) == CC_LE)
			{
				if(tmp_ins->type == INS_CMP || tmp_ins->type == INS_SUB)
				{
					/* Put the regs we are testing into the SETCC instruction */
					ins->in_args[1] = rtl_copy_arg(tmp_ins->in_args[0], tmp_ins);
					ins->in_args[2] = rtl_copy_arg(tmp_ins->in_args[1], tmp_ins);

					/* Well assume that the cc setting (on intel) ins aint needed anymore */
					tmp_ins->type = INS_CLEANUP;

					return 1;
				} /* SETcc, cc=CC_GT,CC_GE,CC_LT,CC_LE: INS_CMP, INS_SUB */
				
				/* Clueless as to what we do now :) */
				fprintf (stderr, 
						"error: SETcc, cc=CC_GT,CC_GE,CC_LT,CC_LE does not how to deal with this instruction (%d)\n",
						tmp_ins->type);
				return 0;
			} /* SETcc, cc=CC_GT,CC_GE,CC_LT,CC_LE */

			/* We found a condition code we dont know, oh well... too bad! */
			fprintf (stderr, 
					"error: does not know how to deal with this kind of SETcc condition\n");
			return 0;





			if(tmp_ins->type == INS_CMP || tmp_ins->type == INS_OR || tmp_ins->type == INS_AND || tmp_ins->type == INS_SUB || tmp_ins->type == INS_SETLABEL)
			{
				/* FIXME: Check if we can optimise this with slt, sgt, etc instructoins */
				/* FIXME: The code generated here could be simplified by negating the test
				 * This code currently generates something like this:
				 * Jcc %x, %y, 0
				 * J 1
				 * 0:
				 * lb %z, 1
				 * 1:
				 *
				 * We should generate:
				 * J!cc %x, %y, 0
				 * lb %z, 1
				 * 0:
				 */
			/* Check that the condition is something we can test using the dest reg only */
					if(ArgCC(ins->in_args[0]) != CC_Z)
					{
						fprintf (stderr, 
								"error: does not know how to deal with this kind of SETcc condition\n");
						return 0;
					}
				if(tmp_ins->type == INS_OR)
				{
					/* This is very conservative, but I am only dealing with the specific cases
					 * at the moment, not trying to figure out the general case, if there is one
					 */
					if((ArgCC(ins->in_args[0]) != CC_Z) && (ArgCC(ins->in_args[0]) != CC_NZ))
					{
						fprintf (stderr,
								"error: SETcc w/OR, does not know how to deal with this condition\n");
						return 0;
					}

					/* Just check all the regs are the same */
					if((ArgReg(tmp_ins->in_args[0]) == ArgReg(tmp_ins->in_args[1])) &&  
							(ArgReg(tmp_ins->in_args[1]) == ArgReg(tmp_ins->out_args[0])))
					{
						tmp_ins_s = tmp_ins_c = add_to_tmp_ins_chain(NULL, 
							compose_ins (INS_CJUMP, 3, 0, ARG_COND, ArgCC(ins->in_args[0]), ARG_FLABEL, 0, ARG_REG, 0));
						tmp_ins_c->in_args[2] = rtl_copy_arg(tmp_ins->in_args[0], tmp_ins_c);

						tmp_ins->type = INS_CLEANUP;
					}
					else
					{
						fprintf (stderr,
								"error: SETcc w/OR, where OR does not use all same regs\n");
						return 0;
					}
				}
				else
				{
					/* Take the condition from the SETcc instruction, and the arguments from the
					 * compare instruction.
					 */
					tmp_ins_s = tmp_ins_c = add_to_tmp_ins_chain(NULL, 
							compose_ins (INS_CJUMP, 4, 0, ARG_COND, ArgCC(ins->in_args[0]), ARG_FLABEL, 0, ARG_REG, 0, ARG_REG, 0));
					tmp_ins_c->in_args[2] = rtl_copy_arg(tmp_ins->in_args[0], tmp_ins_c);
					tmp_ins_c->in_args[3] = rtl_copy_arg(tmp_ins->in_args[1], tmp_ins_c);
				}

				/*
				tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
						compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, 0));
				tmp_ins_c->out_args[0] = rtl_copy_arg(ins->out_args[0], tmp_ins_c);

				tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
						compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, 1));

				tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
						compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));

				tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
						compose_ins (INS_MOVE, 1, 1, ARG_CONST, 1, ARG_REG, 0));
				tmp_ins_c->out_args[0] = rtl_copy_arg(ins->out_args[0], tmp_ins_c);

				tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
						compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 1));
				
				rtl_insert_instr_block_after(tmp_ins_s, ins);
				ins->type = INS_CLEANUP;
				*/

				return 1;
			}
			
			/* We dont know how to deal with SETCC otherwise */
			fprintf (stderr, "error: does not know how to deal with this kind of SETcc\n");
			return 0;
		break; /* INS_SETCC */
	case INS_CMP: /* 5 */
		/* This INS_CMP has been MIPSified already, return */
		if (rtl_validate_checknumargs (ins, 3, 1)) 
			return 1;
		
		if (!rtl_validate_checknumargs (ins, 2, 0)) {
			fprintf (stderr, "error: CMP has wrong number of arguments\n");
			return 0;
		}

		/* Check that the arguments to the cmp are registers only */
		if(ArgMode(ins->in_args[0]) != ARG_REG || ArgMode(ins->in_args[1]) != ARG_REG)
		{
			/* Make a new compare instruction */
			tmp_ins = compose_ins_ex(ins->etc_type, INS_CMP, 2, 0, ARG_REG, 0, ARG_REG, 0);
			tmp_ins_c = tmp_ins_s = NULL;
			/* Sort the first arg out if not reg */
			if(ArgMode(ins->in_args[0]) != ARG_REG)
			{
				new_reg = rtl_get_newvreg();
				tmp_ins_c = tmp_ins_s = add_to_tmp_ins_chain(NULL,
						compose_ins_ex(ins->etc_type, INS_MOVE, 1, 1, ARG_REG, 0, ARG_REG, new_reg));
				/* Set the move instructions source to whatever we want to compare */
				tmp_ins_c->in_args[0] = rtl_copy_arg(ins->in_args[0], tmp_ins_c);
				/* Set the compare instructions register */
				tmp_ins->in_args[0]->flags = ARG_REG;
				tmp_ins->in_args[0]->regconst = new_reg;
			}
			else
				tmp_ins->in_args[0] = rtl_copy_arg(ins->in_args[0], tmp_ins); /* Was ok, just copy */

			/* Sort the second arg out if not a reg */
			if(ArgMode(ins->in_args[1]) != ARG_REG)
			{
				new_reg = rtl_get_newvreg();
				if(tmp_ins_c == NULL)
					tmp_ins_c = tmp_ins_s = add_to_tmp_ins_chain(NULL,
							compose_ins_ex(ins->etc_type, INS_MOVE, 1, 1, ARG_REG, 0, ARG_REG, new_reg));
				else
					tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
							compose_ins_ex(ins->etc_type, INS_MOVE, 1, 1, ARG_REG, 0, ARG_REG, new_reg));
				/* Set the move instructions source to whatever we want to compare */
				tmp_ins_c->in_args[0] = rtl_copy_arg(ins->in_args[1], tmp_ins_c);
				/* Set the compare instructions register */
				tmp_ins->in_args[1]->flags = ARG_REG;
				tmp_ins->in_args[1]->regconst = new_reg;
			}
			else
				tmp_ins->in_args[1] = rtl_copy_arg(ins->in_args[1], tmp_ins); /* Was ok, just copy */

			/* Add the compare ins to the end of the tmp chain */
			add_to_tmp_ins_chain(tmp_ins_c, tmp_ins);

			/* Insert the stuff... the new cmp will be caught by rtl_validate_ins and modified further */
			rtl_insert_instr_block_after(tmp_ins_s, ins);
			ins->type = INS_CLEANUP;

			return 1;
		}

		/* If one or more conditional branchs follows, compare instruction combine them */
		tmp_ins_c = NULL; /* So we know if we combined instructions */
		tmp_ins = find_next_ins_forward(ins);
		while(tmp_ins->type == INS_CJUMP)
		{
			/* Do whatever depending on the kind of conditional */
			switch(ArgCC(tmp_ins->in_args[0]))
			{
				case CC_Z:
				case CC_NZ:
				default:
					/* We need to swap the arguments around cos CMP does things like for gt
					 * cmpl req1, reg2 == reg2 > reg1;
					 * */
					tmp_ins->in_args[2] = rtl_copy_arg(ins->in_args[1], tmp_ins);
					tmp_ins->in_args[3] = rtl_copy_arg(ins->in_args[0], tmp_ins);

					/* Use this so we know that we combined the instructions further on */
					tmp_ins_c = tmp_ins;
			}
			tmp_ins = find_next_ins_forward(tmp_ins);
		}
		/* We need to check if we combined */
		if(tmp_ins_c)
			ins->type = INS_CLEANUP;
		break; /* INS_CMP */
	case INS_ADD: /* 6 */
	case INS_SUB: /* 21 */
	case INS_MUL: /* 24 */
		if (!rtl_validate_checknumargs (ins, 2, 1)) {
			fprintf (stderr, "error: ADD/SUB/MUL has wrong number of arguments\n");
			return 0;
		}
		/* Swap args if neccesary */
		/* swap_imm_reg_args(ins); */
		/* FIXME: Commented this out, as I am going to drop the args in the opposite order... 
		 * it seems as if we always need to have the args the opposite way around, cos intel
		 * does it different than the mips, but interestingly MIPS subtracts can do this
		 * sub $reg1, $reg2, $reg3/imm ($reg1 = $reg2 - $reg3/imm) but intel does this:
		 * subl $reg1/imm, $reg2 ($reg2 = $reg1/imm - $reg2) ie the other way around :(
		 * we can probably do some clever thing here to fix that but for now dont... :)
		 * And on second thoughts I am going to swap the args here, cos it is causing me too
		 * much headfuck not too...
		 *
		 * Hummm... This still seems to cause headfuck :) Ill try not swapping them
		 * again. It is not possible to blindly swap the arguments, cos:
		 * -1 - +1 = -2    !=    +1 - -1 = +2. For now always pull out immidiates
		 * into registers, we can deal with them later.
		 *
		 * I have now swappedthe dropping of arguments in INS_ADD and INS_SUB
		 */

		/* Put args the right way around */
		/*swap_in_args(ins, 0, 1);*/
		/* Fix immidiates: */
		/* FIXME: We can probably elliminate some of these in clever ways */
		/*
		if(ArgMode(ins->in_args[0]) == ARG_CONST)
		{
			new_reg = rtl_get_newvreg();
			tmp_ins_c = tmp_ins_s = add_to_tmp_ins_chain(NULL,
					compose_ins_ex(ins->etc_type, INS_MOVE, 1, 1, ARG_CONST, ArgConst(ins->in_args[0]), ARG_REG, new_reg));
			tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
					compose_ins_ex(ins->etc_type, ins->type, 2, 1, ARG_REG, new_reg, ins->in_args[1]->flags, ins->in_args[1]->regconst, ins->out_args[0]->flags, ins->out_args[0]->regconst));

			rtl_insert_instr_block_after(tmp_ins_s, ins);
			ins->type = INS_CLEANUP;
		}*/
		//expand_args_to_regs(ins);
		
		/* Check to see if we need to remove a error check jump after this instruction */
		if(
			(EtcTypeGetSecondary(ins) == I_ADD && ins->type == INS_ADD) || 
			(EtcTypeGetPrimary(ins) == I_ADC && ins->type == INS_ADD) ||
			(EtcTypeGetSecondary(ins) == I_SUB && ins->type == INS_SUB) ||
			(EtcTypeGetSecondary(ins) == I_MUL && ins->type == INS_MUL))

		{
			/* Next ins is jump to check for overflow, not valid on mips, remove */
			/* First find the next real instruction */
			tmp_ins = find_next_ins_forward(ins);
			if(tmp_ins->type == INS_CJUMP && ArgCC(tmp_ins->in_args[0]) == CC_O)
				tmp_ins->type = INS_CLEANUP;
			else
			{
				fprintf (stderr, "error: after ADD/SUB/MUL instruction, tried to remove an overflow check which was not there\n"); 
				return 0;
			}
		}
		break; /* INS_ADD, INS_SUB, INS_MUL */
	case INS_AND: /* 7 */
		/* If we have a constant as the first in arg, it needs to be swapped. */
		if(ArgIsConst(ins->in_args[0]))
			swap_in_args(ins, 0, 1);
		break; /* INS_AND */
	case INS_CJUMP: /* 10 */
		/* FIXME: Does this instruction not just use the result from the
		 * instruction immidiately above?  If so then it should be fairly easy to
		 * fix this into mips happy things... except we insert new instructions for
		 * load/store etc which messes with that... however these load/store
		 * instructions probably still operate on the same register, so maybe we
		 * CAN just pull the register off the instruction immidiately before this
		 * one... humm...  Which would also be nice as we have to CJUMPs following 
		 * each other at times...
		 *
		 * FIXME: We should be able to get rid of at least some of
		 * the comparison instructions that the CJUMPS test... is that a task for
		 * an optimiser?
		 */

		/* Check if this CJUMP, jumps to an external label, in which case we are
		 * likely to run out of space for the PC relative jump value. Therefore
		 * modify the cjump, to make a local jump, which then jumps far.
		 *
		 * At the momement we are transforming something like:
		 * sub %1, %2, %3
		 * beqz %3, _X_BNSeterr
		 *
		 * TO:
		 * sub %1, %2, %3
		 * beqz %3, 0f
		 * b 1f
		 * 0:
		 * j _X_BNSeterr
		 * 1:
		 *
		 * FIXME: In what cases do we need to do this? Atm, I am assuming only 
		 * when we have: ARG_NAMEDLABEL
		 * FIXME: This could be optimised by negating the C branch, and then getting
		 * rid of the b 1f.
		 */
		if(ArgMode(ins->in_args[1]) == ARG_NAMEDLABEL)
		{
			/* Copy the branch instruction */
			tmp_ins_s = tmp_ins_c = add_to_tmp_ins_chain(NULL,
					rtl_copy_instr(ins));
			/* Change the jump label type and name */
			tmp_ins_c->in_args[1]->flags = ARG_FLABEL;
			ArgLabel(tmp_ins_c->in_args[1]) =  0;
			tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c, 
					compose_ins(INS_JUMP, 1, 0, ARG_FLABEL, 1));
			tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c, 
					compose_ins(INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
			/* The new jump instruction */
			tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c, 
					compose_ins(INS_JUMP, 1, 0, ARG_REG, 0));
			/* Set the label */
			tmp_ins_c->in_args[0] = rtl_copy_arg(ins->in_args[1], tmp_ins_c);
			tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c, 
					compose_ins(INS_SETFLABEL, 1, 0, ARG_FLABEL, 1));

			/* insert it */
			rtl_insert_instr_block_after(tmp_ins_s, ins);

			/* Clean up the current instruction. The new cjump may be modified
			 * again next time through prevalidate */
			ins->type = INS_CLEANUP;

			return 1;
		}

		/* If this CJUMP is part of a I_CSNGL instruction and has a xor in front of
		 * it, sort it out!  The first test in a I_CSNGL instruction chain tests if
		 * the most significant word of the 64 bit integer is purly a sign
		 * extension of the least significant word, which is fine. The next two
		 * instructions however: xor MSW, LSW, MSW; jns MSW is a bit tricky. Those
		 * two instructions essentially test if the most significant bit of the
		 * least significant byte agrees with the sign extension in the most
		 * significant word. Ie if the MSW is 0, then the first bit in the LSW must
		 * also be 0, and if the MSW is -1 (ie 11111...), then the the first bit of
		 * the LSW must be 1.
		 */
		if(HasEtcType(ins))
		{
			tmp_ins = find_next_ins_backward(ins);
			if((EtcTypeGetSecondary(ins) == I_CSNGL) && tmp_ins->type == INS_XOR)
			{
				/* Insert an AND instruction which will pull out only the MSB of the output
				 * reg of the XOR. If this number is now 0, we are ok, otherwise we have overflown.
				 */
				rtl_insert_instr_after(compose_ins(INS_AND, 2, 1, ARG_REG, ArgReg(tmp_ins->out_args[0]), ARG_CONST, 0x80000000, ARG_REG, ArgReg(tmp_ins->out_args[0])), tmp_ins);
				/* Now fix the conditional jump */
				ArgCC(ins->in_args[0]) = CC_Z;
				ins->in_args[2] = rtl_copy_arg(tmp_ins->out_args[0], ins);

				return 1;
			}
		}

		/* If this CJUMP contains an and immeditately before, then we are testing that ands output. */
		/* If this CJUMP contains a sub immeditately before, then we are testing that subs output. */
		/* If this CJUMP contains an or immeditately before, then we are testing that or's output. */
		tmp_ins = find_next_ins_backward(ins);
		/* Check if we have a label before the jump, is this is a FLABEL with the value 99 then we
		 * are ok, otherwise this may be badness FIXME: We ought to get rid of that label so we
		 * dont need this test.
		 */
		while(tmp_ins->type == INS_SETFLABEL || tmp_ins->type == INS_SETLABEL)
		{
			tmp_ins = find_next_ins_backward(tmp_ins);
			if(tmp_ins->type == INS_SETLABEL)
			{
				fprintf (stderr, 
						"warning: while finding CJUMP regs, found LABEL after CJUMP. BAD. \n");
				return 0;
			}
			else
			{
				fprintf (stderr, 
						"warning: while finding CJUMP regs, found FLABEL after CJUMP. Ignoring. \n");
			}
		}
		if(tmp_ins->type == INS_AND || tmp_ins->type == INS_SUB || tmp_ins->type == INS_OR || tmp_ins->type == INS_XOR)
		{
			ins->in_args[2] = rtl_copy_arg(tmp_ins->out_args[0], ins);

			return 1;
		}

		/* If this CJUMP already has a register (or two) then skip it as it has already been fixed by
		 * INS_CMP... Maybe we could use the validate args function here and just count arguments, 
		 * rather than testing if one is a register? donno...
		 * FIXME: Is there a better way of doing this? This seems pretty safe...
		 */
		if(ins->in_args[2] != NULL) {
			if(ArgMode(ins->in_args[2]) == ARG_REG) {
				return 1;
			}
		}

		/* If this is a conditional jump due to a loop, fix it... */
		tmp_ins = NULL;
		if(ins->prev->type != INS_DEC || ins->prev->type != INS_INC) {
			/* Try again one instruction further up, ie skip any potential stores */
			if(ins->prev->prev->type == INS_DEC || ins->prev->prev->type == INS_INC) {
				tmp_ins = ins->prev->prev;
			}
			/* FIXME: It seems like the condition for CC_Z (ie is the loop variable zero
			 * is actually being set to CC_E, which on the intel is the same thing... not
			 * on the mips tho */
			if(ArgCC(ins->in_args[0]) == CC_E) {
				ins->in_args[0]->regconst = CC_Z;
			}
		} else {
			tmp_ins = ins->prev;
		}
		
		tmp_ins_c = find_next_ins_backward(ins);
		/* FIXME: This could be done better */
		/* If the instruction above is an INS_OR with all regs the same, we know that
		 * the conditional jump is testing that reg. */
		if(tmp_ins_c->type == INS_OR) {
			if((ArgMode(tmp_ins_c->in_args[0]) == ARG_REG) && (ArgMode(tmp_ins_c->in_args[1]) == ARG_REG) && (ArgMode(tmp_ins_c->out_args[0]) == ARG_REG)) {
				if((ArgReg(tmp_ins_c->in_args[0]) == ArgReg(tmp_ins_c->in_args[1])) && 
						(ArgReg(tmp_ins_c->in_args[1]) == ArgReg(tmp_ins_c->out_args[0]))) {
					 tmp_ins = tmp_ins_c;
				}
			}
		}

		/* FIXME: This could be done better I think */
		/* If we have a beq and a sub above it use the reg in the add for comparison 
		 * and change to beqz*/
		if(!tmp_ins && ((ArgCC(ins->in_args[0]) == CC_E) || (ArgCC(ins->in_args[0]) == CC_Z) || (ArgCC(ins->in_args[0]) == CC_NZ))) {
			if(tmp_ins_c->type == INS_SUB || tmp_ins_c->type == INS_XOR) {
				tmp_ins = tmp_ins_c;
			}
			/*
			if(!tmp_ins && tmp_ins_c->prev->type == INS_SUB)
				tmp_ins = tmp_ins_c->prev;
			*/
		}

		if(tmp_ins) {
			ins->in_args[2] = new_ins_arg();
			ins->in_args[2]->flags = (ARG_REG | ARG_IMP);
			if(ArgMode(tmp_ins->out_args[0]) == ARG_REG) {
				ArgReg(ins->in_args[2]) = ArgReg(tmp_ins->out_args[0]);
			} else {
				fprintf(stderr, "error: CJUMP, dont know how to find the register to test...\n");
				return 0;
			}
		}
		break; /* INS_CJUMP */
	case INS_PUSH: /* 12 */
		/* Check to see if this push immideately preceds a RET, if so just load the value into a reg */
		tmp_ins_c = find_next_ins_forward(ins);
		if(tmp_ins_c->type == INS_RET) {
			tmp_ins = compose_ins(INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, MIPS_REG_RA);
			tmp_ins->in_args[0] = rtl_copy_arg(ins->in_args[0], tmp_ins);
			rtl_insert_instr_after(tmp_ins, ins);
			
			ins->type = INS_CLEANUP;

			return 1;
		}
		/* If it just a normal push, turn it into a MIPS push (ie using load/store) */
		tmp_ins_c = tmp_ins_s = add_to_tmp_ins_chain(NULL,
				compose_ins_ex(ins->etc_type, INS_SUB, 2, 1, ARG_CONST, 4, ARG_REG, REG_SPTR, ARG_REG, REG_SPTR));
		tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
				compose_ins_ex(ins->etc_type, INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REGIND, REG_SPTR));
		tmp_ins_c->in_args[0] = rtl_copy_arg(ins->in_args[0], tmp_ins_c);
		
		rtl_insert_instr_block_after(tmp_ins_s, ins);
		ins->type = INS_CLEANUP;
		break; /* INS_PUSH */
	case INS_POP: /* 13 */
		/* If it just a normal push, turn it into a MIPS push (ie using load/store) */
		tmp_ins_c = tmp_ins_s = add_to_tmp_ins_chain(NULL,
				compose_ins_ex(ins->etc_type, INS_MOVE, 1, 1, ARG_REGIND, REG_SPTR, ARG_REG, 0));
		tmp_ins_c->out_args[0] = rtl_copy_arg(ins->out_args[0], tmp_ins_c);
		tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
				compose_ins_ex(ins->etc_type, INS_ADD, 2, 1, ARG_CONST, 4, ARG_REG, REG_SPTR, ARG_REG, REG_SPTR));
		
		rtl_insert_instr_block_after(tmp_ins_s, ins);
		ins->type = INS_CLEANUP;
		break; /* INS_POP */
	case INS_RET: /* 14 */
		/* Return instructoins on the MIPS need a register */
		ins->in_args[0] = new_ins_arg ();
		ins->in_args[0]->flags = ARG_REG;
		ArgReg(ins->in_args[0]) = MIPS_REG_RA;
		break; /* INS_RET */
	case INS_JUMP: /* 17 */
		/* If this is not a local jump, then load the address into a register, then jump reg */
		/* Currently we assume that it is not a local jump if it is a: namedlabel (6) */
		if(ArgMode(ins->in_args[0]) == ARG_NAMEDLABEL)
		{
			tmp_ins_c = tmp_ins_s = add_to_tmp_ins_chain(NULL,
					compose_ins(INS_MOVE, 1, 1, ARG_NAMEDLABEL, 0, ARG_REG, MIPS_KROC_TMP_REG));
			tmp_ins_c->in_args[0] = rtl_copy_arg(ins->in_args[0], tmp_ins_c);
			tmp_ins_c->in_args[0]->flags = tmp_ins_c->in_args[0]->flags | ARG_ISCONST;
			tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
					compose_ins(INS_JUMP, 1, 0, ARG_REG, MIPS_KROC_TMP_REG));

			rtl_insert_instr_block_after(tmp_ins_s, ins);

			ins->type = INS_CLEANUP;
		}
		break; /* INS_JUMP */
	case INS_DEC:	/* 19 */
	case INS_INC: /* 20 */
		/* If these (which just get turned into add and subs) do not deal with registers, make them so
		   by doing loads and stores as appropriate */
		if(ArgMode(ins->in_args[0]) != ARG_REG || ArgMode(ins->out_args[0]) != ARG_REG)
		{
			tmp_ins_c = tmp_ins_s = NULL;
			/* Load */
			if(ArgMode(ins->in_args[0]) != ARG_REG)
			{
				/* FIXME: cant use tmp_ins_c->in_args[0]->flags here for in_args[0] as compose_ins throws a wibbely */
				tmp_ins_c = tmp_ins_s = add_to_tmp_ins_chain(NULL,
						compose_ins_ex(ins->etc_type, INS_MOVE, 1, 1, ARG_REG, 0, ARG_REG, MIPS_KROC_TMP_REG));
				tmp_ins_c->in_args[0] = rtl_copy_arg(ins->in_args[0], tmp_ins_c);
			}
			/* Inc/Dec */
			/*
			   Leaving this as an INC/DEC instruction, so we can find it again when we need to modify the branching instructions
			tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
					compose_ins_ex(ins->etc_type, (ins->type == INS_INC?INS_ADD:INS_SUB), 2, 1, 
						(tmp_ins_s == NULL?ins->in_args[0]->flags:ARG_REG), (tmp_ins_s == NULL?ins->in_args[0]->regconst:MIPS_KROC_TMP_REG),
						ARG_CONST, 1,
						(tmp_ins_s == NULL?ins->out_args[0]->flags:ARG_REG), (tmp_ins_s == NULL?ins->out_args[0]->regconst:MIPS_KROC_TMP_REG)
					));
			*/
			tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
					compose_ins_ex(ins->etc_type, ins->type, 1, 1, 
						(tmp_ins_s == NULL?ins->in_args[0]->flags:ARG_REG), (tmp_ins_s == NULL?ins->in_args[0]->regconst:MIPS_KROC_TMP_REG),
						(tmp_ins_s == NULL?ins->out_args[0]->flags:ARG_REG), (tmp_ins_s == NULL?ins->out_args[0]->regconst:MIPS_KROC_TMP_REG)
					));
			/* If we did not generate the load instruction, this is the first instruciton in the chain... 
			   ... and the in argument to inc was a register, so nothing else to do */
			if(tmp_ins_s == NULL)
				tmp_ins_s = tmp_ins_c;
			/* Store */
			if(ArgMode(ins->out_args[0]) != ARG_REG)
			{
				tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
						compose_ins_ex(ins->etc_type, INS_MOVE, 1, 1, ARG_REG, MIPS_KROC_TMP_REG, ins->out_args[0]->flags, 0));
				tmp_ins_c->out_args[0] = rtl_copy_arg(ins->out_args[0], tmp_ins_c);
			}
			
			rtl_insert_instr_block_after(tmp_ins_s, ins);
			ins->type = INS_CLEANUP;
		}
		break; /* INS_DEC, INS_INC */
	case INS_LOADLABDIFF: /* 26 */
		if (!rtl_validate_checknumargs (ins, 2, 1)) {
			fprintf (stderr, "error: LOADLABDIFF has wrong number of arguments\n");
			return 0;
		}
		if(ArgMode(ins->out_args[0]) != ARG_REG)
		{
			fprintf (stderr, "error: LOADLABDIFF's out(0) is not a register \n");
			return 0;
		}
		/* Turn this into two load instructions and a subtract */
		tmp_ins_c = tmp_ins_s = add_to_tmp_ins_chain(NULL,
			// FIXME: Why does compose_ins not like this:
			//compose_ins_ex(ins->etc_type, INS_MOVE, 1, 1, ins->in_args[0]->flags, 0, ARG_REG, ins->out_args[0]->regconst));
			compose_ins_ex(ins->etc_type, INS_MOVE, 1, 1, ARG_REG, 0, ARG_REG, ins->out_args[0]->regconst));
		// FIXME: Who does this not work either?
		//tmp_ins_c->in_args[0] = rtl_copy_arg(ins->in_args[0], tmp_ins_c);
		tmp_ins_c->in_args[0]->flags = ins->in_args[0]->flags;
		tmp_ins_c->in_args[0]->regconst = ins->in_args[0]->regconst;
		tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
			// FIXME: Why does compose_ins not like this:
			//compose_ins_ex(ins->etc_type, INS_MOVE, 1, 1, ins->in_args[1]->flags, 0, ARG_REG, MIPS_KROC_TMP_REG));
			compose_ins_ex(ins->etc_type, INS_MOVE, 1, 1, ARG_REG, 0, ARG_REG, MIPS_KROC_TMP_REG));
		// FIXME: Who does this not work either?
		//tmp_ins_c->in_args[1] = rtl_copy_arg(ins->in_args[1], ins);
		tmp_ins_c->in_args[0]->flags = ins->in_args[1]->flags;
		tmp_ins_c->in_args[0]->regconst = ins->in_args[1]->regconst;
		tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
			compose_ins_ex(ins->etc_type, INS_SUB, 2, 1, ARG_REG, ins->out_args[0]->regconst, ARG_REG, MIPS_KROC_TMP_REG, ARG_REG, ins->out_args[0]->regconst));

		rtl_insert_instr_block_after(tmp_ins_s, ins);
		ins->type = INS_CLEANUP;
		break; /* INS_LOADLABDIFF */
	case INS_SHL: /* 29 */
	case INS_SHR: /* 30 */
		if (!rtl_validate_checknumargs (ins, 2, 1)) {
			fprintf (stderr, "error: SHL/SHR has wrong number of arguments\n");
			return 0;
		}
		if(ArgMode(ins->in_args[0]) != ARG_REG)
		{
			new_reg = rtl_get_newvreg();
			rtl_insert_instr_before(compose_ins(INS_MOVE, 1, 1, ARG_REG, 0, ARG_REG, new_reg),
					ins);
			ins->prev->in_args[0] = rtl_copy_arg(ins->out_args[0], ins->prev);
			
			ArgReg(ins->in_args[0]) = new_reg;
			ins->in_args[0]->flags = ARG_REG;
		}
		/*swap_imm_reg_args(ins);*/
		break; /* INS_SHL, INS_SHR */
	case INS_MOVEB: /* 31 */
		switch(ArgMode(ins->in_args[0])) {
			case ARG_CONST: /* in */
				if(ArgMode(ins->out_args[0]) != ARG_REG)
				{
					tmp_ins_c = tmp_ins_s = add_to_tmp_ins_chain(NULL,
							compose_ins_ex(ins->etc_type, INS_MOVE, 1, 1, ins->in_args[0]->flags, ins->in_args[0]->regconst, ARG_REG, MIPS_KROC_TMP_REG));
					tmp_ins_c = add_to_tmp_ins_chain(tmp_ins_c,
							compose_ins_ex(ins->etc_type, INS_MOVEB, 1, 1, ARG_REG, MIPS_KROC_TMP_REG, ARG_REG, 0));
					tmp_ins_c->out_args[0]->flags = ins->out_args[0]->flags;
					tmp_ins_c->out_args[0]->regconst = ins->out_args[0]->regconst;

					rtl_insert_instr_block_after(tmp_ins_s, ins);
					ins->type = INS_CLEANUP;
				}
		}
		break; /* INS_MOVEB */
	case INS_FSTCW:
	case INS_FLDCW:
	case INS_FSTP:
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
		fprintf(stderr, "Encountered floating point instruction (%d), removing!\n", ins->type);
		fprintf(stderr, "WARNING: Inserted invalid (segfauting) move instead of FP inst\n");
		rtl_insert_instr_before(compose_ins(INS_MOVE, 1, 1, ARG_REGIND, MIPS_REG_ZERO, ARG_REG, MIPS_REG_ZERO), ins);
		ins->type = INS_CLEANUP;
		break;
	} 

	return 1;
}
/*}}}*/
/*{{{  static int rtl_validate_instr_mips (ins_chain *ins)*/
/*
 *	validates MIPS instructions
 */
static int rtl_validate_instr_mips (ins_chain *ins)
{
	// FIXME: Temporary debug output, only shown with -v
	dump_ins_info("Validating", ins);

	switch (ins->type) {
	case INS_SETCC:
		if (!rtl_validate_checknumargs (ins, 3, 1)) {
			fprintf (stderr, "error: SETcc has wrong number of arguments\n");
			return 0;
		}
		break;
	case INS_CJUMP: /* 10 */
		/* Check that the condition is not zero, as that would be bad... (ie segfault later on) */
		if(ArgCC(ins->in_args[0]) == CC_NONE)
		{
			fprintf (stderr, "error: CJUMP has invalid condition code (CC_NONE)\n");
			return 0;
		}
		if(ArgMode(ins->in_args[0]) != ARG_COND)
		{
			fprintf (stderr, "error: CJUMP's first argument is not a condition\n");
			return 0;
		}
		if(ArgMode(ins->in_args[1]) != ARG_LABEL && ArgMode(ins->in_args[1]) != ARG_BLABEL && ArgMode(ins->in_args[1]) != ARG_FLABEL && ArgMode(ins->in_args[1]) != ARG_NAMEDLABEL && ArgMode(ins->in_args[1]) != ARG_INSLABEL)
		{
			fprintf (stderr, "error: CJUMP's second argument is not a label\n");
			return 0;
		}

		break; /* INS_CJUMP */
	}

	return 1;
}
/*}}}*/

/*{{{  static void compose_divcheck_zero_simple_mips (tstate *ts, int reg)*/
/*
 *	void compose_divcheck_zero_simple_mips (tstate *ts, int reg)
 *	checks that `reg' isn't zero before performing division
 */
static void compose_divcheck_zero_simple_mips (tstate *ts, int reg)
{
	add_to_ins_chain (compose_ins (INS_CJUMP, 3, 0, ARG_COND, CC_Z, ARG_NAMEDLABEL, string_dup      ("&_X_BNSeterr"), ARG_REG, reg));

	return;
}
/*}}}*/
/*{{{  static int compose_widenshort_mips (tstate *ts)*/
/*
 *	int compose_widenshort_mips (tstate *ts)
 *	On intel:sign-extends AX into EAX (Areg), returns the new Areg
 *	On MIPS: sign extends Areg, returns the new Areg
 *	modifies Areg on the stack to stop collisions in the register allocator (need to improve that..)
 */
static int compose_widenshort_mips (tstate *ts)
{
	/*
	int tmp_reg = tstack_newreg (ts->stack);

	add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, ts->stack->a_reg, ARG_REG, REG_EAX));
	add_to_ins_chain (compose_ins (INS_CWDE, 1, 1, ARG_REG | ARG_IMP | ARG_IS16BIT, ts->stack->a_reg, ARG_REG | ARG_IMP, ts->stack->a_reg));
	add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, ts->stack->a_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->a_reg, ARG_REG, tmp_reg));
	constmap_remove (ts->stack->a_reg);
	ts->stack->a_reg = tmp_reg;
	return tmp_reg;
	*/
	int tmp_reg = tstack_newreg (ts->stack);
	add_to_ins_chain (compose_ins (INS_CJUMP, 3, 0, ARG_COND, CC_LT,  ARG_FLABEL, 0, ARG_REG, ts->stack->a_reg)); /* branch less than zero */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0x0000FFFF, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_AND, 2, 1, ARG_REG, ts->stack->a_reg, ARG_REG, tmp_reg,  ARG_REG, ts->stack->a_reg));
	add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, 1));
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0xFFFF0000, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_OR, 2, 1, ARG_REG, ts->stack->a_reg, ARG_REG, tmp_reg,  ARG_REG, ts->stack->a_reg));
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 1));
	/* Dont really understand what is going on here... todo with the register allocator,
	 * but do we need to do it for the MIPS!!! FIXME!!!
	 */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->a_reg, ARG_REG, tmp_reg));
	ts->stack->a_reg = tmp_reg;
	return tmp_reg;
}
/*}}}*/
/*{{{  static int compose_widenword_mips (tstate *ts)*/
/*
 *	int compose_widenword_mips (tstate *ts)
 *	On IA32: sign-extends EAX into EDX:EAX.  returns extended register.  mangles the stack somewhat
 *	On the MIPS we have to implement the sign extension implicitly
 */
static int compose_widenword_mips (tstate *ts)
{
	int tmp_reg = tstack_newreg (ts->stack);

	/* arrange for old_a_reg to stay at stack-top */
	tmp_reg = tstack_newreg (ts->stack);
	ts->stack->a_reg = ts->stack->old_a_reg;
	ts->stack->b_reg = tmp_reg;
	ts->stack->c_reg = ts->stack->old_b_reg;

	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->a_reg));
	add_to_ins_chain (compose_ins (INS_CJUMP, 3, 0, ARG_COND, CC_LT,  ARG_FLABEL, 0, ARG_REG, ts->stack->old_a_reg)); /* branch less than zero */
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, -1, ARG_REG, ts->stack->b_reg));
	add_to_ins_chain (compose_ins (INS_JUMP, 1, 0, ARG_FLABEL, 1));
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 0));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, ts->stack->b_reg));
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_FLABEL, 1));

	return tmp_reg;
}
/*}}}*/
/*{{{  static void compose_division_mips (tstate *ts, int dividend, int divisor, int quotient)*/
/*
 *	void compose_division_mips (tstate *ts, int dividend, int divisor, int quotient)
 *	does division.  
 */
static void compose_division_mips (tstate *ts, int dividend, int divisor, int quotient)
{
	int tmp_reg = tstack_newreg (ts->stack);
	int remainder = tmp_reg;

	add_to_ins_chain (compose_ins_ex (EtcSecondary(I_DIV), INS_DIV, 2, 3, ARG_REG, dividend, ARG_REG, divisor, ARG_REG, quotient, ARG_REG | ARG_IMP, MIPS_REG_HI, ARG_REG | ARG_IMP, MIPS_REG_LO));
	/* On the MIPS we have to move the remainder out of the HI register ourselves */
	add_to_ins_chain (compose_ins_ex (EtcSecondary(I_DIV), INS_MOVE, 1, 1, ARG_REG | ARG_IMP, MIPS_REG_HI, ARG_REG, remainder));

	return;
}
/*}}}*/
/*{{{  static int compose_remainder_mips (tstate *ts, int dividend, int divisor)*/
/*
 *	int compose_remainder_mips (tstate *ts, int dividend, int divisor)
 *	does remainder.  Sign-extends EAX into EAX:EDX, does the remainder and returns the register holding it (EDX)
 */
static int compose_remainder_mips (tstate *ts, int dividend, int divisor)
{
	int tmp_reg = tstack_newreg (ts->stack);
	/* int quotient = dividend; */

	add_to_ins_chain (compose_ins_ex (EtcSecondary(I_REM), INS_DIV, 2, 3, ARG_REG, dividend, ARG_REG, divisor, ARG_REG, tmp_reg, ARG_REG | ARG_IMP, MIPS_REG_HI, ARG_REG | ARG_IMP, MIPS_REG_LO));

	return tmp_reg; /* remainder? */
}
/*}}}*/
/*{{{  static int compose_move_mips (tstate *ts)*/
/*
 */
static void compose_move_mips (tstate *ts)
{
	// FIXME: Optimise this to use branch likely?
	// FIXME: Optimise to use word size moves if possible?
	// FIXME: Are we ever going to get a case where $counter is 0??? that would be 
	// bad at the moment

	int counter_reg = ts->stack->old_a_reg;
	int saddr_reg = ts->stack->old_b_reg;
	int daddr_reg = ts->stack->old_c_reg;
	int tmp_reg = tstack_newreg (ts->stack);

	// Generate this kind of code:
	// 0:
	// lb $tmp, $saddr
	// sb $tmp, $daddr
	// subu $counter, $counter, 1
	// addu $saddr, $saddr, 1
	// addu $daddr, $daddr, 1
	// bnez $counter, 0b
	add_to_ins_chain (compose_ins (INS_SETFLABEL, 1, 0, ARG_BLABEL, 0));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REGIND, saddr_reg, ARG_REG, tmp_reg));
	add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REGIND, daddr_reg));
	add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_REG, counter_reg, ARG_CONST, 1, ARG_REG, counter_reg));
	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, saddr_reg, ARG_CONST, 1, ARG_REG, saddr_reg));
	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_REG, daddr_reg, ARG_CONST, 1, ARG_REG, daddr_reg));
	add_to_ins_chain (compose_ins (INS_CJUMP, 3, 0, ARG_COND, CC_NZ, ARG_BLABEL, 0, ARG_REG, counter_reg));

	return;
}
/*}}}*/
/*{{{  static int compose_shift_mips(tstate *ts)*/
/*
 */
static void compose_shift_mips (tstate *ts, int shift_ins, int reg1, int reg2, int reg3)
{

	add_to_ins_chain (compose_ins (((shift_ins == I_SHL) ? INS_SHL : INS_SHR), 2, 1, ARG_REG, reg1, ARG_REG, reg2, ARG_REG, reg3));

	return;
}
/*}}}*/

/*{{{  static void compose_longop_mips (tstate *ts, int sec)*/
/*
 *	void compose_longop_mips (tstate *ts, int sec)
 *	does long-integer operations.  Mangles stack
 */
static void compose_longop_mips (tstate *ts, int sec)
{
	int tmp_reg, this_lab;

	switch (sec) {
	case I_LADD:
		tmp_reg = *(int *)0;
		/*
		add_to_ins_chain (compose_ins (INS_RCR, 2, 1, ARG_CONST, 1, ARG_REG | ARG_IS8BIT, ts->stack->old_c_reg, ARG_REG | ARG_IS8BIT, ts->stack->old_c_reg));
		constmap_remove (ts->stack->old_c_reg);
		add_to_ins_chain (compose_ins (INS_ADC, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
		constmap_remove (ts->stack->old_b_reg);
		ts->stack->a_reg = ts->stack->old_b_reg;
		*/
		break;
	case I_LSUB:
		tmp_reg = *(int *)0;
		/*
		add_to_ins_chain (compose_ins (INS_RCR, 2, 1, ARG_CONST, 1, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg));
		constmap_remove (ts->stack->old_c_reg);
		add_to_ins_chain (compose_ins (INS_SBB, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
		constmap_remove (ts->stack->old_b_reg);
		ts->stack->a_reg = ts->stack->old_b_reg;
		*/
		break;
	case I_LSUM:
		tmp_reg = *(int *)0;
		/*
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_RCR, 2, 1, ARG_CONST, 1, ARG_REG | ARG_IS8BIT, ts->stack->old_c_reg, ARG_REG | ARG_IS8BIT, ts->stack->old_c_reg));
		constmap_remove (ts->stack->old_c_reg);
		add_to_ins_chain (compose_ins (INS_ADC, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
		constmap_remove (ts->stack->old_b_reg);
		add_to_ins_chain (compose_ins (INS_RCL, 2, 1, ARG_CONST, 1, ARG_REG | ARG_IS8BIT, tmp_reg, ARG_REG | ARG_IS8BIT, tmp_reg));
		constmap_remove (tmp_reg);
		ts->stack->b_reg = tmp_reg;
		*/
		break;
	case I_LDIFF:
		tmp_reg = *(int *)0;
		/*
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_RCR, 2, 1, ARG_CONST, 1, ARG_REG | ARG_IS8BIT, ts->stack->old_c_reg, ARG_REG | ARG_IS8BIT, ts->stack->old_c_reg));
		constmap_remove (ts->stack->old_c_reg);
		add_to_ins_chain (compose_ins (INS_SBB, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_b_reg));
		constmap_remove (ts->stack->old_b_reg);
		add_to_ins_chain (compose_ins (INS_RCL, 2, 1, ARG_CONST, 1, ARG_REG | ARG_IS8BIT, tmp_reg, ARG_REG | ARG_IS8BIT, tmp_reg));
		constmap_remove (tmp_reg);
		ts->stack->b_reg = tmp_reg;
		*/
		break;
	case I_LMUL:
		tmp_reg = *(int *)0;
		/*
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
		*/
		break;
	case I_LSHL:
	case I_LSHR:
		tmp_reg = tstack_newreg (ts->stack); 
		/*
		 * This code should generate the following sequence of instructions to perform
		 * the shift (left): (c = MSW, b = LSW, a = shiftval)
		 *   negu	tmp_reg, old_a_reg	-> subu tmp_reg, 0, old_a_reg
		 *   srl	tmp_reg, old_b_reg, tmp_reg
		 *   sll	old_c_reg, old_c_reg, old_a_reg
		 *   or	old_c_reg, tmp_reg, old_c_reg
		 *   sll	old_b_reg, old_b_reg, old_a_reg
		 * To perform the shift right, we invert the shift directions on all the shifts
		 */
		/* FIXME: The in args are the wrong way around, cos prevalidation of SUB
		 * blindly swaps arguments... */
		add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, MIPS_REG_ZERO, ARG_REG, tmp_reg)); /* INS_SUB is subu by default */
		add_to_ins_chain (compose_ins ((sec == I_LSHL ? INS_SHR : INS_SHL), 2, 1, ARG_REG,  ts->stack->old_b_reg, ARG_REG, tmp_reg, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins ((sec == I_LSHL ? INS_SHL : INS_SHR), 2, 1, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_c_reg));
		add_to_ins_chain (compose_ins (INS_OR, 2, 1, ARG_REG, tmp_reg, ARG_REG, ts->stack->old_c_reg, ARG_REG, ts->stack->old_c_reg));
		add_to_ins_chain (compose_ins ((sec == I_LSHL ? INS_SHL : INS_SHR), 2, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg));

		this_lab = ++(ts->last_lab);

		/* compare 32 with shift-size and jump if below */
		add_to_ins_chain (compose_ins (INS_CJUMP, 4, 0, ARG_COND, CC_B, ARG_LABEL, this_lab, ARG_REG, ts->stack->old_a_reg, ARG_CONST, 32));
		/* exchange oldBreg and oldCreg */
		tmp_reg = tstack_newreg (ts->stack); 
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_b_reg, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, ts->stack->old_a_reg, ARG_REG, ts->stack->old_b_reg));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, tmp_reg, ARG_REG, ts->stack->old_b_reg));
		/* clear register with bits shifted completely out */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, (sec == I_LSHL) ? ts->stack->old_b_reg : ts->stack->old_c_reg));

		/* compare 64 with shift-size and jump if below */
		add_to_ins_chain (compose_ins (INS_CJUMP, 4, 0, ARG_COND, CC_B, ARG_LABEL, this_lab, ARG_REG, ts->stack->old_a_reg, ARG_CONST, 64));
		/* clear register with bits shifted completely out */
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_CONST, 0, ARG_REG, (sec == I_LSHL) ? ts->stack->old_c_reg : ts->stack->old_b_reg));

		/* done */
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		ts->stack->a_reg = ts->stack->old_b_reg;
		ts->stack->b_reg = ts->stack->old_c_reg;
		break;
	case I_LDIV:
		tmp_reg = *(int *)0;
		/*
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
		*/
		break;
	default:
		fprintf (stderr, "%s: fatal: secondary op-code 0x%X in compose_longop_i386()\n", progname, sec);
		break;
	}
	return;
}
/*}}}*/

/*{{{  static void compose_fpop_mips (tstate *ts, int sec)*/
/*
 *	generates FP code
 */
static void compose_fpop_mips (tstate *ts, int sec)
{
	switch (sec) {
	case I_FPCHKERR:
	case I_FPEXPDEC32:
	case I_FPEXPINC32:
	case I_FPCHKI64:
	case I_FPCHKI32:
	case I_FPNAN:
	case I_FPNOTFINITE:
	case I_FPORDERED:
	case I_FPGT:
	case I_FPEQ:
	case I_FPREM:
	default:
		fprintf (stderr, "%s: compose_fpop_mips: warning: unknown instruction %d (ignoring)\n", progname, sec);
		break;
	}
	return;
}
/*}}}*/
/*{{{  static void compose_external_ccall_mips (tstate *ts, int inlined, char *name, ins_chain **pst_first, ins_chain **pst_last)*/
static void compose_external_ccall_mips (tstate *ts, int inlined, char *name, ins_chain **pst_first, ins_chain **pst_last)
{
	*pst_first = compose_ins (INS_PUSH, 1, 0, ARG_REG, REG_WPTR);
	add_to_ins_chain (*pst_first);
	if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_FPTR, ARG_NAMEDLABEL, string_dup ("&Fptr")));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_REG, REG_BPTR, ARG_NAMEDLABEL, string_dup ("&Bptr")));
	}
	/* Args in $v0/$4 */
	add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, 4, ARG_REG, 4));
	add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_NAMEDLABEL, string_dup (name + 1), ARG_REG, 25));
	/* Must always allocate at least 16 bytes on stack for arguments, even
	 * when not used, on the MIPS
	 */
	add_to_ins_chain (compose_ins (INS_SUB, 2, 1, ARG_CONST, 16, ARG_REG, MIPS_REG_SP, ARG_REG, MIPS_REG_SP));

	add_to_ins_chain (compose_ins (INS_CALL, 1, 0, ARG_REG, 25));
	/* Remove space for args */
	add_to_ins_chain (compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, MIPS_REG_SP, ARG_REG, MIPS_REG_SP));
	if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup ("&Fptr"), ARG_REG, REG_FPTR));
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_NAMEDLABEL, string_dup ("&Bptr"), ARG_REG, REG_BPTR));
	}
	add_to_ins_chain (compose_ins (INS_POP, 0, 1, ARG_REG, REG_WPTR));
	if (!strcmp (name, "C.ccsp.suspendproc")) {
			fprintf (stderr, "%s: error: do not have dynamic process support for MIPS architecture  yet\n", progname);
			exit (EXIT_FAILURE);
	}  else 
	{
		*pst_last = compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR);
	}
	add_to_ins_chain (*pst_last);
	return;
}
/*}}}*/
/*{{{  static void compose_bcall_mips (tstate *ts, int i, int inlined, char *name, ins_chain **pst_first, ins_chain **pst_last)*/
/*
 *	generates code to perform a blocking system call (or kernel run call)
 */
static void compose_bcall_mips (tstate *ts, int i, int kernel_call, int inlined, char *name, ins_chain **pst_first, ins_chain **pst_last)
{
	int tmp_reg;

	tmp_reg = tstack_newreg (ts->stack); 
	*pst_first = compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, 4, ARG_REG, tmp_reg); 
	/* *pst_first = compose_ins (INS_LEA, 1, 1, ARG_REGIND | ARG_DISP, REG_WPTR, 4, ARG_REG, MIPS_REG_V0); */
	add_to_ins_chain (*pst_first);
	constmap_remove (tmp_reg); 
	add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_REG, tmp_reg)); 
	if (kernel_call != K_KERNEL_RUN) {
		/* push address of function to call */
		 add_to_ins_chain (compose_ins (INS_PUSH, 1, 0, ARG_NAMEDLABEL | ARG_ISCONST, string_dup (name + ((i == 2) ? 1 : 2)))); 
		/*
		add_to_ins_chain (compose_ins (INS_LEA, 1, 1, ARG_NAMEDLABEL | ARG_ISCONST, string_dup (name + ((i == 2) ? 1 : 2)), ARG_REG, MIPS_REG_V1)); */
	}
	if (options.kernel_interface & (KRNLIFACE_NEWCCSP | KRNLIFACE_RMOX)) {
		add_to_ins_chain (compose_ins (INS_CALL, 1, 0, ARG_NAMEDLABEL, string_dup ((kif_entry (kernel_call))->entrypoint)));
	} else if (options.kernel_interface & KRNLIFACE_MESH) {
		fprintf (stderr, "%s: error: do not have blocking call support in MESH yet\n", progname);
	} else if (options.kernel_interface & KRNLIFACE_CSPLINUX) {
		fprintf (stderr, "%s: error: support for CSP/Linux not yet implemented\n", progname);
	}
	*pst_last = compose_ins (INS_ADD, 2, 1, ARG_CONST, 16, ARG_REG, REG_WPTR, ARG_REG, REG_WPTR);
	add_to_ins_chain (*pst_last);
}
/*}}}*/
/*{{{  static void compose_entry_prolog_mips (tstate *ts)*/
/*
 *	generates entry prologue for MIPS
 */
static void compose_entry_prolog_mips (tstate *ts)
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
	compose_kcall_mips (ts, K_PAUSE, 0, 0);
	flush_ins_chain ();

	return;
}
/*}}}*/
/*{{{  static void compose_fp_init_mips (tstate *ts)*/
static void compose_fp_init_mips (tstate *ts)
{
	return;
}
/*}}}*/
/*{{{  static void compose_fp_set_fround_mips (tstate *ts, int mode)*/
static void compose_fp_set_fround_mips (tstate *ts, int mode)
{
	if (mode == ts->stack->fpu_mode) {
		return;
	}
	/* ... */
	ts->stack->fpu_mode = mode;
	return;
}
/*}}}*/
/*{{{  static void compose_reset_fregs_mips (tstate *ts)*/
/*
 *	resets FPU registers (clears them out)
 */
static void compose_reset_fregs_mips (tstate *ts)
{
	ts->stack->fpu_mode = FPU_NONE;
	return;
}
/*}}}*/

/*{{{  arch_t *init_arch_mips (int mclass)*/
/*
 *	initialises and returns architecture definition
 */
arch_t *init_arch_mips (int mclass)
{
	arch_t *arch = (arch_t *)smalloc (sizeof (arch_t));

	/* common */
	arch->archname = string_dup ("mips");

	/* kernel calls */
	arch->compose_kcall = compose_kcall_mips;
	arch->compose_deadlock_kcall = compose_deadlock_kcall_mips;

	/* inlining */
	arch->compose_inline_ldtimer = NULL;
	arch->compose_inline_quick_reschedule = NULL;
	arch->compose_inline_in = NULL;
	arch->compose_inline_in_2 = NULL;
	arch->compose_inline_min = NULL;
	arch->compose_inline_out = NULL;
	arch->compose_inline_out_2 = NULL;
	arch->compose_inline_mout = NULL;
	arch->compose_inline_enbc = NULL;
	arch->compose_inline_disc = NULL;
	arch->compose_inline_altwt = NULL;
	arch->compose_inline_stlx = NULL;
	arch->compose_inline_malloc = NULL;
	arch->compose_inline_startp = NULL;
	arch->compose_inline_endp = NULL;
	arch->compose_inline_runp = NULL;
	arch->compose_inline_stopp = NULL;

	/* debugging */
	arch->compose_debug_insert = compose_debug_insert_mips;
	arch->compose_debug_procnames = compose_debug_procnames_mips;
	arch->compose_debug_filenames = compose_debug_filenames_mips;
	arch->compose_debug_zero_div = NULL;
	arch->compose_debug_floaterr = NULL;
	arch->compose_debug_overflow = NULL;
	arch->compose_debug_rangestop = NULL;
	arch->compose_debug_seterr = NULL;
	arch->compose_overflow_jumpcode = NULL;
	arch->compose_floaterr_jumpcode = NULL;
	arch->compose_rangestop_jumpcode = NULL;

	arch->compose_debug_deadlock_set = NULL;

	/* code-gen */
	arch->compose_divcheck_zero = NULL;
	arch->compose_divcheck_zero_simple = compose_divcheck_zero_simple_mips;
	arch->compose_division = compose_division_mips;
	arch->compose_remainder = compose_remainder_mips;
	arch->compose_iospace_loadbyte = NULL;
	arch->compose_iospace_storebyte = NULL;
	arch->compose_iospace_loadword = NULL;
	arch->compose_iospace_storeword = NULL;
	arch->compose_iospace_read = NULL;
	arch->compose_iospace_write = NULL;
	arch->compose_move = compose_move_mips;
	arch->compose_move_loadptrs = NULL;
	arch->compose_shift = compose_shift_mips;

	arch->compose_widenshort = compose_widenshort_mips;
	arch->compose_widenword = compose_widenword_mips;
	arch->compose_longop = compose_longop_mips;
	arch->compose_fpop = compose_fpop_mips;
	arch->compose_external_ccall = compose_external_ccall_mips;
	arch->compose_bcall = compose_bcall_mips;

	arch->compose_entry_prolog = compose_entry_prolog_mips;
	arch->compose_rmox_entry_prolog = NULL;
	arch->compose_fp_set_fround = compose_fp_set_fround_mips;
	arch->compose_fp_init = compose_fp_init_mips;
	arch->compose_reset_fregs = compose_reset_fregs_mips;

	arch->compose_return = NULL;
	arch->compose_nreturn = NULL;
	arch->compose_funcresults = NULL;

	/* register allocation */
	arch->regcolour_special_to_real = regcolour_special_to_real_mips;
	arch->regcolour_rmax = RMAX_MIPS;
	arch->regcolour_nodemax = NODEMAX_MIPS;
	arch->regcolour_get_regs = regcolour_get_regs_mips;
	arch->regcolour_fp_regs = NULL;

	/* output generation */
	arch->code_to_asm = dump_asmmips;
	arch->code_to_asm_stream = dump_asmmips_stream;

	/* RTL stuff */
	arch->rtl_validate_instr = rtl_validate_instr_mips;
	arch->rtl_prevalidate_instr = rtl_prevalidate_instr_mips;
	arch->get_register_name = get_register_name_mips;

	arch->int_options = INTOPT_NOREGSQUEEZE;
	return arch;
}
/*}}}*/

