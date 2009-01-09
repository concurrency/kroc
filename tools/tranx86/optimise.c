/*
 *	optimise.c - optimising bits
 *	Copyright (C) 2000-2005 Fred Barnes <frmb@kent.ac.uk>
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

/*{{{  includes, etc.*/
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
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "main.h"
#include "support.h"
#include "structs.h"
#include "transputer.h"
#include "trancomm.h"
#include "tstack.h"
#include "archdef.h"
#include "rtlops.h"
#include "postmortem.h"
#include "intel.h"
#include "rtldump.h"
#include "machine.h"

#ifndef EXIT_FAILURE
	#define EXIT_FAILURE 1
#endif	/* !EXIT_FAILURE */


/*}}}*/


/*{{{  structs, forward-decls, local defines*/

typedef struct {
	int lab;
	char *stub;
} stubentry;

static void codeblock_clean_regmarkers (ins_chain *instrs);
static int codeblock_reg_squeeze (ins_chain *ins);
static int is_reg_load (ins_chain *ins, int reg);
static int is_reg_store (ins_chain *ins, int reg);
static int regblock_squeeze (ins_chain *first_ins, ins_chain *last_ins, int reg);
static int register_is_source_or_target (ins_chain *ins, int reg);
static int register_is_source (ins_chain *ins, int reg);
static int register_is_target (ins_chain *ins, int reg);
static int codeblock_pack_sequences (ins_chain *head);
static int clean_up_deadcode (ins_chain *start);

#undef DEBUG_OPTIMISER

#define OPTNODE_UNKNOWN 0
#define OPTNODE_REG 1
#define OPTNODE_INSTR 2
#define OPTNODE_MEMORY 3
#define OPTNODE_LABEL 4

typedef struct TAG_optnode {
	int type;
	union {
		ins_chain *instr;
		int regconst;
	} u;
} optnode;


/*}}}*/

/*{{{  static void codeblock_clean_regmarkers (ins_chain *instrs)*/
/*
 *	void codeblock_clean_regmarkers (ins_chain *instrs)
 *	turns START/END markers into cleanups
 */
static void codeblock_clean_regmarkers (ins_chain *instrs)
{
	ins_chain *tmp;

	for (tmp=instrs; tmp; tmp=tmp->next) {
		if ((tmp->type == INS_START_REG) || (tmp->type == INS_END_REG)) {
			tmp->type = INS_CLEANUP;
		}
	}
	return;
}
/*}}}*/
/*{{{  static int register_is_target (ins_chain *ins, int reg)*/
/*
 *	int register_is_target (ins_chain *ins, int reg)
 *	returns 1 if `reg' will be modified by `ins'
 */
static int register_is_target (ins_chain *ins, int reg)
{
	int nargs;
	unsigned char mode;
	ins_arg *arg;

	for (nargs=0; ins->out_args[nargs]; nargs++) {
		arg = ins->out_args[nargs];
		mode = (arg->flags & ARG_MODEMASK);
		switch (mode) {
		case ARG_REG:
			if (arg->regconst == reg) {
				return 1;
			}
			break;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  static int register_is_source (ins_chain *ins, int reg)*/
/*
 *	int register_is_source (ins_chain *ins, int reg)
 *	returns 1 if `reg' is a source in `ins'
 */
static int register_is_source (ins_chain *ins, int reg)
{
	int nargs;
	unsigned char mode;
	ins_arg *arg;
	ins_sib_arg *sib_arg;

	for (nargs=0; ins->in_args[nargs]; nargs++) {
		arg = ins->in_args[nargs];
		mode = (arg->flags & ARG_MODEMASK);
		switch (mode) {
		case ARG_REG:
		case ARG_REGIND:
			if (arg->regconst == reg) {
				return 1;
			}
			break;
		case ARG_REGINDSIB:
			sib_arg = (ins_sib_arg *)arg->regconst;
			if (sib_arg->base == reg) {
				return 1;
			}
			if (sib_arg->index == reg) {
				return 1;
			}
			break;
		}
	}
	for (nargs=0; ins->out_args[nargs]; nargs++) {
		arg = ins->out_args[nargs];
		mode = (arg->flags & ARG_MODEMASK);
		switch (mode) {
		case ARG_REGIND:
			if (arg->regconst == reg) {
				return 1;
			}
			break;
		case ARG_REGINDSIB:
			sib_arg = (ins_sib_arg *)arg->regconst;
			if (sib_arg->base == reg) {
				return 1;
			}
			if (sib_arg->index == reg) {
				return 1;
			}
			break;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  static int register_is_source_or_target (ins_chain *ins, int reg)*/
/*
 *	int register_is_source_or_target (ins_chain *ins, int reg)
 *	returns 1 if `reg' is a source or a target in `ins'
 */
static int register_is_source_or_target (ins_chain *ins, int reg)
{
	int nargs;
	unsigned char mode;
	ins_arg *arg;
	ins_sib_arg *sib_arg;

	for (nargs=0; ins->in_args[nargs]; nargs++) {
		arg = ins->in_args[nargs];
		mode = (arg->flags & ARG_MODEMASK);
		switch (mode) {
		case ARG_REG:
		case ARG_REGIND:
			if (arg->regconst == reg) {
				return 1;
			}
			break;
		case ARG_REGINDSIB:
			sib_arg = (ins_sib_arg *)arg->regconst;
			if (sib_arg->base == reg) {
				return 1;
			}
			if (sib_arg->index == reg) {
				return 1;
			}
			break;
		}
	}
	for (nargs=0; ins->out_args[nargs]; nargs++) {
		arg = ins->out_args[nargs];
		mode = (arg->flags & ARG_MODEMASK);
		switch (mode) {
		case ARG_REG:
		case ARG_REGIND:
			if (arg->regconst == reg) {
				return 1;
			}
			break;
		case ARG_REGINDSIB:
			sib_arg = (ins_sib_arg *)arg->regconst;
			if (sib_arg->base == reg) {
				return 1;
			}
			if (sib_arg->index == reg) {
				return 1;
			}
			break;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  static int codeblock_reg_squeeze (ins_chain *ins)*/
/*
 *	int codeblock_reg_squeeze (ins_chain *ins)
 *	squeezes registers in a block of code
 */
static int codeblock_reg_squeeze (ins_chain *ins)
{
	ins_chain *tmp, *tmp2;
	int did_squeeze, this_reg, i;

	did_squeeze = 0;
	for (tmp = ins; tmp; tmp = tmp->next) {
		if (tmp->type == INS_START_REG) {
			if (!tmp->next) {
				fprintf (stderr, "%s: error: INS_START_REG at end of instruction chain\n", progname);
				return -1;
			}
			this_reg = tmp->in_args[0]->regconst;
			tmp = tmp->next;
			tmp2 = rtl_scan_end_forward (tmp, this_reg);
			if (!tmp2) {
				fprintf (stderr, "%s: error: INS_START_REG without INS_END_REG\n", progname);
				return -1;
			}
			tmp2 = tmp2->prev;
			/* okay, have the instructions in tmp..tmp2 */
			if (rtl_scan_constrain_forward (tmp, this_reg) == NULL) {
				i = regblock_squeeze (tmp, tmp2, this_reg);
				if (i < 0) {
					return -1;
				}
				did_squeeze += i;
			}
		}
	}
	return did_squeeze;
}
/*}}}*/
/*{{{  static int is_reg_load (ins_chain *ins, int reg)*/
/*
 *	int is_reg_load (ins_chain *ins, int reg)
 *	returns non-zero if this instruction loads something into `reg' (via a move)
 */
static int is_reg_load (ins_chain *ins, int reg)
{
	return ((ins->type == INS_MOVE) && (ArgMode (ins->out_args[0]) == ARG_REG) && (ArgReg (ins->out_args[0]) == reg));
}
/*}}}*/
/*{{{  static int is_reg_store (ins_chain *ins, int reg)*/
/*
 *	int is_reg_store (ins_chain *ins, int reg)
 *	returns non-zero if this instruction stores something from `reg' (via a move)
 */
static int is_reg_store (ins_chain *ins, int reg)
{
	return ((ins->type == INS_MOVE) && (ArgMode (ins->in_args[0]) == ARG_REG) && (ArgReg (ins->in_args[0]) == reg));
}
/*}}}*/
/*{{{  static int regblock_squeeze (ins_chain *first_ins, ins_chain *last_ins, int reg)*/
/*
 *	int regblock_squeeze (ins_chain *first_ins, ins_chain *last_ins, int reg)
 *	tries to squeeze register usage
 */
static int regblock_squeeze (ins_chain *first_ins, ins_chain *last_ins, int reg)
{
	ins_chain **reg_instrs;
	int *reg_srcdest;		/* 1=source 2=dest 3=both */
	ins_chain *tmp, *tmp_ins;
	int n_instrs, i, did_squeeze;

	n_instrs = 0;
	did_squeeze = 0;
	for (tmp=first_ins; tmp != last_ins->next; tmp=tmp->next) {
		if (register_is_source_or_target (tmp, reg)) {
			/* quick noddy things in here */

			/*
			 *	lea  C(%r), %r
			 * into:
			 *	add C, %r, %r
			 */
			if ((tmp->type == INS_LEA) && (ArgMode (tmp->in_args[0]) == ARG_REGIND) && ArgHasDisp (tmp->in_args[0]) &&
				(ArgReg (tmp->in_args[0]) == reg) && (ArgMode (tmp->out_args[0]) == ARG_REG) && (ArgReg (tmp->out_args[0]) == reg)) {

				int disp = ArgDisp (tmp->in_args[0]);

#if 0
fprintf (stderr, "re-writing:\n    ");
dump_ins_chunk (stderr, tmp, tmp);
#endif
				tmp->type = INS_ADD;
				tmp->in_args[1] = rtl_copy_arg (tmp->out_args[0], NULL);
				tmp->in_args[0]->flags = (ARG_CONST | ARG_ISCONST);
				tmp->in_args[0]->regconst = disp;
#if 0
fprintf (stderr, "into:\n    ");
dump_ins_chunk (stderr, tmp, tmp);
#endif
			}
			n_instrs++;
		}
	}
	if (!n_instrs) {
		return 0;
	} else if (n_instrs == 1) {
		/* single reference, can clean this one up */
		if (first_ins != last_ins) {
			fprintf (stderr, "%s: fatal: single instruction for register %d, but first (%p) != last (%p)\n", progname, reg, first_ins, last_ins);
		}
		switch (first_ins->type) {
		case INS_POP:
			/* subsitute for stack addition */
			rtl_free_arg (first_ins->out_args[0]);
			/* manufacture "addl $4, %esp" and transplant */
			tmp_ins = compose_ins (INS_ADD, 2, 1, ARG_CONST, 4, ARG_REG, REG_ESP, ARG_REG, REG_ESP);
			first_ins->type = tmp_ins->type;
			first_ins->in_args[0] = tmp_ins->in_args[0];
			first_ins->in_args[1] = tmp_ins->in_args[1];
			first_ins->out_args[0] = tmp_ins->out_args[0];
			tmp_ins->in_args[0] = tmp_ins->in_args[1] = tmp_ins->out_args[0] = NULL;
			rtl_free_instr (tmp_ins);
			return 1;
			break;
		case INS_PUSH:
			fprintf (stderr, "%s: warning: PUSHing lonely register %d onto stack (it can\'t be initialised)\n", progname, reg);
			return 0;
			break;
		}
		rtl_removing_labref (first_ins);	/* don't know */
		first_ins->type = INS_CLEANUP;
		return 1;
	} else if (n_instrs == 2) {
		if ((first_ins->type == INS_POP) && (last_ins->type == INS_MOVE)) {
			/*  optimise:
			 *	pop	%R
			 *	...
			 *	mov	%R, <something>
			 *  into:
			 *	pop	<something>
			 */
			/* register is source in move ? */
			if (((last_ins->in_args[0]->flags & ARG_MODEMASK) == ARG_REG) && (last_ins->in_args[0]->regconst == reg)) {
				/* yup, make sure sequence between them is non-destructive */
				if ((first_ins->next == last_ins) ? 1 : (!rtl_destructive_sequence (first_ins->next, last_ins->prev) &&
									!rtl_arg_in_sequence (last_ins->out_args[0], first_ins->next, last_ins->prev))) {
					rtl_free_arg (first_ins->out_args[0]);
					first_ins->out_args[0] = last_ins->out_args[0];
					last_ins->out_args[0] = NULL;
					last_ins->type = INS_CLEANUP;
					return 1;
				}
			}
		} else if ((first_ins->type == INS_MOVE) && (last_ins->type == INS_OR) && (rtl_next_instr(first_ins) == last_ins)) {
			/*  optimise:
			 *	mov	<something>, %R
			 *	orl	%R, %R
			 *  into:
			 *	cmpl	$0, <something>
			 */
			/* register is dest in MOVE, and source/target in OR ? */
			if ((ArgMode (first_ins->out_args[0]) == ARG_REG) && (ArgReg (first_ins->out_args[0]) == reg) &&
				(ArgMode (last_ins->in_args[0]) == ARG_REG) && (ArgReg (last_ins->in_args[0]) == reg) &&
				(ArgMode (last_ins->in_args[1]) == ARG_REG) && (ArgReg (last_ins->in_args[1]) == reg) &&
				(ArgMode (last_ins->out_args[0]) == ARG_REG) && (ArgReg (last_ins->out_args[0]) == reg)) {
				/* optimise it away */
				first_ins->type = INS_CMP;
				first_ins->in_args[1] = first_ins->in_args[0];
				first_ins->in_args[0] = first_ins->out_args[0];
				first_ins->out_args[0] = NULL;
				first_ins->in_args[0]->flags = (ARG_CONST | ARG_ISCONST);	/* set constant 0 (was `reg') */
				first_ins->in_args[0]->regconst = 0;
				first_ins->out_args[0] = compose_ins_arg (ARG_REG | ARG_IMP, REG_CC);
				last_ins->type = INS_CLEANUP;
				return 1;
			}
		}
	}
	/* anything load-<something>-store ish optimisable ? */
	if (is_reg_load (first_ins, reg) && is_reg_store (last_ins, reg) && rtl_compare_args (first_ins->in_args[0], last_ins->out_args[0])) {
		ins_arg *the_arg = first_ins->in_args[0];

#if 0
fprintf (stderr, "found load-store sequence (%d instrs):\n", n_instrs);
dump_ins_chunk (stderr, first_ins, last_ins);
#endif
		if (n_instrs == 2) {
			/* have a load-store to no effect (shouldn't happen, but get it if it does) */
			first_ins->type = INS_CLEANUP;
			last_ins->type = INS_CLEANUP;
			did_squeeze++;
		} else if (n_instrs == 3) {
			/* check that `the_arg' doesn't occur in the sequence */
			if (rtl_arg_in_sequence (the_arg, first_ins->next, last_ins->prev)) {
				goto local_out;
			}
			tmp = first_ins->next;
			while ((tmp != last_ins) && !register_is_source_or_target (tmp, reg)) {
				tmp = tmp->next;
			}
			/* do anything useful with this instruction ? */
			if (((tmp->type == INS_ADD) || (tmp->type == INS_SUB)) && register_is_source (tmp, reg) && register_is_target (tmp, reg)) {
				switch (ArgMode (tmp->in_args[0])) {
				case ARG_REG:
					if (ArgReg (tmp->in_args[0]) == reg) {
						goto local_out;
					}
					/* fall-through */
				case ARG_CONST:
					/* yup... */
					rtl_free_arg (tmp->in_args[1]);
					rtl_free_arg (tmp->out_args[0]);
					tmp->in_args[1] = rtl_copy_arg (the_arg, tmp);
					tmp->out_args[0] = rtl_copy_arg (the_arg, tmp);
					first_ins->type = INS_CLEANUP;
					last_ins->type = INS_CLEANUP;
					did_squeeze++;
					break;
				}
#if 0
fprintf (stderr, "re-wrote into:\n");
dump_ins_chunk (stderr, first_ins, last_ins);
#endif
			}
		}
	}
local_out:
	if (n_instrs > 1) {
		/* don't bother looking at anything too complicated */
		return did_squeeze;
	}
	/* ..might need re-thinking below here.. */
	reg_instrs = (ins_chain **)smalloc (n_instrs * sizeof (ins_chain *));
	reg_srcdest = (int *)smalloc (n_instrs * sizeof (int));
	for (i=0; i<n_instrs; i++) {
		reg_instrs[i] = NULL;
		reg_srcdest[i] = 0;
	}
	i = 0;
	for (tmp=first_ins; tmp != last_ins->next; tmp=tmp->next) {
		if (register_is_source (tmp, reg)) {
			reg_instrs[i] = tmp;
			reg_srcdest[i]++;
		}
		if (register_is_target (tmp, reg)) {
			reg_instrs[i] = tmp;
			reg_srcdest[i] += 2;
		}
		if (reg_instrs[i]) {
			i++;
		}
	}
	sfree (reg_instrs);
	sfree (reg_srcdest);
	return did_squeeze;
}
/*}}}*/
/*{{{  static int clean_up_deadcode (ins_chain *start)*/
/*
 *	int clean_up_deadcode (ins_chain *start)
 *	cleans up unreachable code (by setting to INS_CLEANUP)
 *	returns number of instructions cleaned
 */
static int clean_up_deadcode (ins_chain *start)
{
	int n_removed;

	n_removed = 0;
	while (start) {
		switch (start->type) {
		case INS_SETLABEL:
		case INS_SETFLABEL:
			return n_removed;
			break;
		default:
			if ((start->type >= INS_FIRST) && (start->type <= INS_LAST)) {
				rtl_removing_labref (start);		/* don't know */
				start->type = INS_CLEANUP;
				n_removed++;
#if 0
			} else {
fprintf (stderr, "clean_up_deadcode(): stepping over instr %d\n", start->type);
#endif
			}
			start = start->next;
			break;
		}
	}
	return n_removed;
}
/*}}}*/
/*{{{  static int codeblock_pack_sequences (ins_chain *head)*/
/*
 *	int codeblock_pack_sequences (ins_chain *head)
 *	condenses known optimisable sequences (see code for details)
 *	-- also responsible for curing any occurences of "cmp CONST,CONST"
 */
static int codeblock_pack_sequences (ins_chain *head)
{
	ins_chain *tmp, *first_op, *next_ins, *this_ins;
	int op_type, op_const, op_reg;
	int clear_firstop;
	int n_done, i;
	int tmp_addsub_const;
	ins_arg *jump_arg;

	first_op = NULL;
	op_reg = op_type = op_const = -1;
	n_done = 0;
	for (tmp=head; tmp; tmp=tmp->next) {
		tmp_addsub_const = 0;
		/*
		 *  pack up ADDs and SUBs, turning:
		 *	add	c0, %R, %R
		 *	add	c1, %R, %R
		 *  into:
		 *	add	(c0 + c1), %R, %R
		 *  (and likewise for permuataions with SUB)
		 */
		clear_firstop = 1;
		switch (tmp->type) {
		case INS_ADD:
		case INS_SUB:
			if ((tmp->in_args[0]->flags & ARG_MODEMASK) != ARG_CONST) {
				break;		/* from switch() */
			}
			tmp_addsub_const = 1;
			if ((ArgMode (tmp->in_args[1]) != ARG_REG) || (ArgMode (tmp->out_args[0]) != ARG_REG)) {
				break;		/* from switch() */
			}
			/* must have same source/dest register..! */
			if (ArgReg (tmp->in_args[1]) != ArgReg (tmp->out_args[0])) {
				break;		/* from switch() */
			}
			if (first_op) {
				if (tmp->in_args[1]->regconst != op_reg) {
					/* not us, but update first_op */
					clear_firstop = 0;
					first_op = tmp;
					op_type = tmp->type;
					op_const = tmp->in_args[0]->regconst;
					op_reg = tmp->in_args[1]->regconst;
					break;		/* from switch() */
				}
				/* remove first_op and combine with this */
				if (op_type == tmp->type) {
					/* add saved constant */
					tmp->in_args[0]->regconst += op_const;
				} else {
					/* subtract saved constant */
					tmp->in_args[0]->regconst -= op_const;
				}
				first_op->type = INS_CLEANUP;
				#ifdef DEBUG_OPTIMISER
					fprintf (stderr, "== ADD/SUB concatenation\n");
				#endif
				n_done++;
				/* first op gone, new first op */
				clear_firstop = 0;
				first_op = tmp;
				op_type = tmp->type;
				op_const = tmp->in_args[0]->regconst;
				op_reg = tmp->in_args[1]->regconst;
			} else {
				/* this is the first op */
				clear_firstop = 0;
				first_op = tmp;
				op_type = tmp->type;
				op_const = tmp->in_args[0]->regconst;
				op_reg = tmp->in_args[1]->regconst;
			}
			break;
		}
		if (first_op && clear_firstop) {
			first_op = NULL;
		}
		switch (tmp->type) {
		/*
		 *  transform (optimise returns):
		 *	push	<something>	=>	jmp *<something>
		 *	ret
		 *  and, (optimise -ve instrs)
		 *	add	-c, %R, %R	=>	sub	c, %R, %R
		 *	sub	-c, %R, %R	=>	add	c, %R, %R
		 */
		case INS_ADD:
		case INS_SUB:
			if (!tmp_addsub_const) {
				break;		/* from switch() */
			}
			if (tmp->in_args[0]->regconst < 0) {
				tmp->type = (tmp->type == INS_ADD) ? INS_SUB : INS_ADD;
				tmp->in_args[0]->regconst = -(tmp->in_args[0]->regconst);
			}
			break;
		case INS_PUSH:
			next_ins = rtl_next_instr (tmp);
			if (next_ins && (next_ins->type == INS_RET)) {
				tmp->type = INS_JUMP;
				tmp->in_args[0]->flags |= ARG_IND;
				next_ins->type = INS_CLEANUP;
				#ifdef DEBUG_OPTIMISER
					fprintf (stderr, "== PUSH/RET optimisation\n");
				#endif
				n_done++;
			}
			break;
		/*
		 *  resolve:
		 *	cmp	CONST,CONST
		 *	cjump	cond, <labins>
		 */
		case INS_CMP:
			if ((ArgMode (tmp->in_args[0]) == ARG_CONST) && (ArgMode (tmp->in_args[1]) == ARG_CONST)) {
				next_ins = rtl_next_instr (tmp);
				if (next_ins->type == INS_CJUMP) {
					/* maybe */
					switch (rtl_result_of_const_compare (ArgConst (tmp->in_args[1]), ArgConst (tmp->in_args[0]), ArgCC (next_ins->in_args[0]))) {
					case 0:
						/* comparison is false, will not jump -- thus easy */
						tmp->type = INS_CLEANUP;		/* freeup constant comparison */
						next_ins->type = INS_CLEANUP;		/* freeup conditional jump (could also do label removal if applicable) */
						#ifdef DEBUG_OPTMISER
							fprintf (stderr, "== constant comparison is 0, removed jump and compare\n");
						#endif
						n_done++;
						break;
					default:
						/* comparison is true, will jump */
						tmp->type = INS_CLEANUP;		/* freeup constant comparison */
						rtl_free_arg (next_ins->in_args[0]);	/* free condition argument */
						next_ins->in_args[0] = next_ins->in_args[1];
						next_ins->in_args[1] = NULL;
						next_ins->type = INS_JUMP;		/* turn into unconditional jump */
						#ifdef DEBUG_OPTIMISER
							fprintf (stderr, "== constant comparison is TRUE, removing comparison and following jump\n");
						#endif
						n_done++;
						break;
					}
				} else {
					fprintf (stderr, "%s: error: unable to resolve constant comparison (instruction type was %d)\n", progname, next_ins->type);
					fprintf (stderr, "%s: error: sequence will not assemble, aborting translation\n", progname);
					exit (EXIT_FAILURE);
				}
			}
			break;
		/*
		 *  turn:
		 *	jmp	<somewhere>
		 *	...
		 *  into:
		 *	jmp	<somewhere>
		 *	[nothing!]
		 */
		case INS_JUMP:
		case INS_PJUMP:
			i = clean_up_deadcode (tmp->next);
			#ifdef DEBUG_OPTIMISER
				if (i) fprintf (stderr, "== jump cleaned up code (%d)\n", i);
			#endif
			n_done += i;
			/* FALL THROUGH */
		/*
		 *  turn:
		 *	jmp	Lx	||
		 *	jcc	CC, Lx
		 *    Lx:
		 *  into "Lx:" (removes jump)
		 *  also, if we have cmovc support, turn:
		 *	jcc	CC, Lx
		 *	(move args  ||  jump args)
		 *	Lx:
		 *  into:
		 *	(cmovc CC^1, args   ||  jcc CC^1, args)
		 *	Lx:
		 */
		case INS_CJUMP:
			next_ins = rtl_next_instr (tmp);
			if (next_ins && (next_ins->type == INS_SETLABEL)) {
				jump_arg = (((tmp->type == INS_JUMP) || (tmp->type == INS_PJUMP)) ? tmp->in_args[0] : tmp->in_args[1]);
				if (ArgInsLab(jump_arg) == next_ins) {
					rtl_removing_labref (tmp);
					tmp->type = INS_CLEANUP;
					#ifdef DEBUG_OPTIMISER
						fprintf (stderr, "== cleaed up jump-to-next-instr\n");
					#endif
					n_done++;
				}
			} else if ((tmp->type == INS_CJUMP) && (options.machine_options & OPTION_CMOVC)) {
				ins_chain *jump_target;
				ins_arg *condition;

				/* scan forward over moves and jumps (they're cc'able) until we find label target */
				next_ins = rtl_next_instr (tmp);
				jump_target = ArgInsLab (tmp->in_args[1]);
				condition = tmp->in_args[0];
				while (next_ins && ((next_ins->type == INS_MOVE) || (next_ins->type == INS_PJUMP) || (next_ins->type == INS_JUMP)) && (next_ins != jump_target)) {
					if ((next_ins->type == INS_JUMP) || (next_ins->type == INS_PJUMP)) {
						/* code cannot continue past here (unconditional jump) */
						next_ins = rtl_next_instr (next_ins);
						break;
					} else {
						/* move instruction -- cmovc can't handle immediate values */
						if ((ArgMode (next_ins->in_args[0]) == ARG_CONST) || (next_ins->in_args[0]->flags & ARG_ISCONST)) {
							/* dammit.. */
							break;
						}
						if ((ArgMode (next_ins->in_args[0]) == ARG_REG) && (ArgMode (next_ins->out_args[0]) != ARG_REG)) {
							/* can only have reg->reg */
							break;
						}
						next_ins = rtl_next_instr (next_ins);
					}
				}
				if (next_ins == jump_target) {
					ins_chain *tmp_ins;
	
					for (tmp_ins = rtl_next_instr (tmp); tmp_ins != next_ins; tmp_ins = rtl_next_instr (tmp_ins)) {
						if (tmp_ins->type == INS_MOVE) {
							tmp_ins->type = INS_CMOVE;
							tmp_ins->in_args[1] = tmp_ins->in_args[0];
							tmp_ins->in_args[0] = rtl_copy_arg (condition, tmp_ins);
							/* invert the condition */
							tmp_ins->in_args[0]->regconst ^= 1;
						} else {
							/* unconditional jump */
							tmp_ins->type = INS_CJUMP;
							tmp_ins->in_args[1] = tmp_ins->in_args[0];
							tmp_ins->in_args[0] = rtl_copy_arg (condition, tmp_ins);
							/* invert the condition */
							tmp_ins->in_args[0]->regconst ^= 1;
						}
					}
					/* remove `tmp' (jcc line) */
					rtl_removing_labref (tmp);
					tmp->type = INS_CLEANUP;
					n_done++;
				}
			}
			break;
#if 0
		/*
		 * this tends to be generated by .boolinvert:
		 *	or	%r, %r, %r
		 *	setcc	_Z_, %r
		 *	and	$1, %r, %r
		 * into:
		 *	xor	$1, %r, %r
		 *	and	$1, %r, %r
		 */
		case INS_OR:
			/* also see this sequence for other stuff, which ain't .boolinvert.. (disabled) */
			if ((ArgMode (tmp->in_args[0]) == ARG_REG) && (ArgMode (tmp->in_args[1]) == ARG_REG) && (ArgReg (tmp->in_args[0]) == ArgReg (tmp->in_args[1]))) {
				int reg = ArgReg (tmp->in_args[0]);
				ins_chain *next = rtl_next_instr (tmp);

				if (next && (next->type == INS_SETCC) && (ArgCC (next->in_args[0]) == CC_Z) && (ArgMode (next->out_args[0]) == ARG_REG) && (ArgReg (next->out_args[0]) == reg)) {
					ins_chain *final = rtl_next_instr (next);

					if (final && (final->type == INS_AND) && (ArgMode (final->in_args[0]) == ARG_CONST) && (ArgMode (final->in_args[1]) == ARG_REG) &&
						(ArgConst (final->in_args[0]) == 1) && (ArgReg (final->in_args[1]) == reg)) {

						/* oki, have something optimisable */

						tmp->type = INS_XOR;
						tmp->in_args[0]->flags = ARG_CONST | ARG_ISCONST;
						tmp->in_args[0]->regconst = 1;
						rtl_removing_labref (next);
						next->type = INS_CLEANUP;
						n_done++;
					}
				}
			}
			break;
#endif
#if 0
		/*
		 * turn:
		 *   Ln:
		 *	jump Lm
		 * into:
		 *	jump Lm
		 * (and subsitute Lm for Ln)
		 */
		case INS_SETLABEL:
			next_ins = rtl_next_instr (tmp);
			if (next_ins && ((next_ins->type == INS_JUMP) || (next_ins->type == INS_PJUMP)) && (ArgMode (next_ins->in_args[0]) == ARG_INSLABEL)) {
				int src_lab = ArgLabel (tmp->in_args[0]);
				ins_chain *dst_instr = ArgInsLab (next_ins->in_args[0]);
				int dst_lab = ArgLabel (dst_instr->in_args[0]);

				rtl_rename_label (src_lab, tmp, dst_lab, dst_instr);
				tmp->type = INS_CLEANUP;
				n_done++;
			}
			break;
#endif
		/*
		 *  transforms:
		 *	move S0, T
		 *	   ...        =>     move Sn, T
		 *	move Sn, T
		 */
		case INS_MOVE:
			next_ins = rtl_next_instr (tmp);
			this_ins = NULL;
			while (next_ins && (next_ins->type == INS_MOVE) && rtl_compare_args (tmp->out_args[0], next_ins->out_args[0])) {
				ins_arg *out_arg = tmp->out_args[0];

				if (ArgMode(out_arg) == ARG_REG) {
					if (register_is_source (next_ins, ArgReg (out_arg))) {
						break;
					}
				}

				for (i = 0; i < MAX_IN_ARGS; ++i) {
					ins_arg *in_arg = next_ins->in_args[i];

					if (in_arg && rtl_compare_args (out_arg, in_arg)) {
						break;
					}
				}
				
				if (i < MAX_IN_ARGS) {
					break;
				}
				
				this_ins = next_ins;
				next_ins = rtl_next_instr (next_ins);
			}
			/* this_ins is set to the last move (or NULL) */
			if (this_ins) {
				/* something to clear up */
				ins_chain *tmp_ins = tmp;

				while (tmp_ins != this_ins) {
					/* remove tmp_ins */
					rtl_removing_labref (tmp_ins);
					tmp_ins->type = INS_CLEANUP;
					tmp_ins = rtl_next_instr (tmp_ins);
					n_done++;
				}
			}
			break;
		/*
		 *   merge a sequence of shifts:
		 *     sh[lr]  %imm0, %r
		 *     ...
		 *     sh[lr]  %immN, %r
		 *
		 *   into a single shift:
		 *     sl[lr]  (%imm0 + ... + %immN), %r
		 */
		case INS_SHL:
		case INS_SHR:
			i = 0; /* total shift */
			next_ins = tmp;
			this_ins = NULL;
			while (
				next_ins 
				&& ((next_ins->type == INS_SHL) || (next_ins->type == INS_SHR)) 
				&& (ArgMode (next_ins->in_args[0]) == ARG_CONST) 
				&& rtl_compare_args (tmp->in_args[1], next_ins->in_args[1]) 
				&& rtl_compare_args (tmp->out_args[0], next_ins->out_args[0])
			) {
				this_ins = next_ins;
				next_ins = rtl_next_instr (this_ins);
				if (this_ins->type == INS_SHL) {
					i -= ArgConst (this_ins->in_args[0]);
				} else {
					i += ArgConst (this_ins->in_args[0]);
				}
				if ((i <= (-32)) || (i >= 32)) {
					i |= 0xffff0000; /* force sticky overflow */
				}
			}
			if (this_ins && (this_ins != tmp)) {
				ins_chain *tmp_ins = tmp;

				while (tmp_ins != this_ins) {
					rtl_removing_labref (tmp_ins);
					tmp_ins->type = INS_CLEANUP;
					tmp_ins = rtl_next_instr (tmp_ins);
					n_done++;
				}

				if (i == 0) {
					/* all shifts cancel */
					rtl_removing_labref (this_ins);
					this_ins->type = INS_CLEANUP;
					n_done++;
				} else if ((i <= (-32)) || (i >= 32)) {
					/* shifts wiped out data */

					/* delete second input argument */
					rtl_free_arg (this_ins->in_args[1]);
					this_ins->in_args[1] = NULL;

					if (ArgHasDisp (this_ins->out_args[0])) {
						/* memory operation use move */
						this_ins->type = INS_MOVE;
						this_ins->in_args[0]->regconst = 0;
					} else {
						/* register operation use xor */
						this_ins->type = INS_XOR;
						this_ins->in_args[0] = rtl_copy_arg (this_ins->out_args[0], this_ins);
					}
				} else if (i < 0) {
					/* left shift */
					this_ins->type = INS_SHL;
					this_ins->in_args[0]->regconst = -i;
				} else {
					/* right shift */
					this_ins->type = INS_SHR;
					this_ins->in_args[0]->regconst = i;
				}
			}
			break;
		}
	}
	return n_done;
}
/*}}}*/
/*{{{  int optimise_run (rtl_chain *rtl_code, arch_t *arch)*/
/*
 *	runs the various optimisations (mostly peep-hole style)
 */
int optimise_run (rtl_chain *rtl_code, arch_t *arch)
{
	rtl_chain *tmp;
	int tot_squeezed, i, j, k;

	tot_squeezed = 0;
	for (tmp=rtl_code; tmp; tmp=tmp->next) {
		if (tmp->type == RTL_CODE) {
			/* don't squeeze registers for RISC architectures..! */
			if (arch->int_options & INTOPT_NOREGSQUEEZE) {
				i = 0;
			} else {
				i = codeblock_reg_squeeze (tmp->u.code.head);
			}
			j = codeblock_pack_sequences (tmp->u.code.head);
			k = rtl_glob_codelabels (tmp->u.code.head);
			#ifdef DEBUG_OPTIMISER
				fprintf (stderr, "optimiser: i = %d, j = %d, k = %d\n", i, j, k);
			#endif
			if ((i < 0) || (j < 0) || (k < 0)) {
				/* got unhappy somewhere.. */
				return -1;
			} else if (i || j || k) {
				rtl_chain *next, *prev;
				/* mark all setart/end registers to INS_CLEANUP */
				codeblock_clean_regmarkers (tmp->u.code.head);
				tmp->u.code.head = rtl_cleanup_code (tmp->u.code.head);
				/* retrace registers */
				next = tmp->next;
				prev = tmp->prev;
				tmp->next = NULL;
				tmp->prev = NULL;
				if (rtl_trace_regs (tmp) < 0) {
					fprintf (stderr, "%s: error: rtl_trace_regs failed during squeeze\n", progname);
					return -1;
				}
				/* re-attach */
				tmp->next = next;
				tmp->prev = prev;
				tot_squeezed += (i + j + k);
			}
		}
	}
	/* may well have un-reachable code at the start of the program! */
	{
		rtl_chain *xstart;
		int stoploop = 0;
		int tmpsqz;

		for (xstart = rtl_code; !stoploop && xstart; xstart = xstart->next) {
			switch (xstart->type) {
			case RTL_CODE:
				stoploop = 1;
				break;
			case RTL_DATA:
			case RTL_SETNAMEDLABEL:
			case RTL_PUBLICSETNAMEDLABEL:
			case RTL_XDATA:
			case RTL_RDATA:
				xstart = NULL;
				stoploop = 1;
				break;
			default:
				break;
			}
			if (stoploop) {
				break;
			}
		}
		if (xstart && (xstart->type == RTL_CODE)) {
#if 0
fprintf (stderr, "DEBUG: deadcode near program start\n");
#endif
			tot_squeezed += clean_up_deadcode (xstart->u.code.head);
			xstart->u.code.head = rtl_cleanup_code (xstart->u.code.head);
		}

		tmpsqz = rtl_cleanup_flabels (rtl_code);
		if (tmpsqz) {
			rtl_cleanup_code_all (rtl_code);
		}
		tot_squeezed += tmpsqz;
	}

	return tot_squeezed;
}
/*}}}*/

