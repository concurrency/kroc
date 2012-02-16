/*
 *	tstack.c - transputer stack manipulation
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

/*{{{  includes, etc.*/
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

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#define __TSTACK_C

#include "main.h"
#include "structs.h"
#include "support.h"
#include "tstack.h"
#include "transputer.h"
#include "archdef.h"
#include "etcrtl.h"

#ifndef EXIT_FAILURE
	#define EXIT_FAILURE 1
#endif	/* !EXIT_FAILURE */


/*}}}*/
/*{{{  constant map stuff*/
#undef DEBUG_CONSTMAP
/* stuff for the constant handling */
static constmap **tran_constmap = NULL;
static int tran_cm_cur = 0;
static int tran_cm_max = 0;
static int tran_cm_cidx = -1;
/*}}}*/
/*{{{  tstack *new_tstack (void)*/
/*
 *	creates and returns a blank tstack structure
 */
tstack *new_tstack (void)
{
	tstack *tmp;

	tmp = (tstack *)smalloc (sizeof (tstack));
	memset (tmp, 0, sizeof (tstack));
	tmp->reg_counter = FIRST_VIRTUAL_REG;		/* start at FIRST_VIRTUAL_REG */
	return tmp;
}
/*}}}*/


/*{{{  void tstack_destroy (tstack *stack)*/
/*
 *	frees memory used by the stack
 */
void tstack_destroy (tstack *stack)
{
	free (stack);
	return;
}
/*}}}*/
/*{{{  void tstack_undefine (tstack *stack)*/
/*
 *	undefines `resets' the stack
 */
void tstack_undefine (tstack *stack)
{
	stack->a_reg = REG_UNDEFINED;
	stack->b_reg = REG_UNDEFINED;
	stack->c_reg = REG_UNDEFINED;
	stack->ts_depth = 0;
	stack->must_set_cmp_flags = 1;
	stack->fs_depth = 0;
	return;
}
/*}}}*/
/*{{{  void tstack_pop (tstack *stack)*/
/*
 *	pops the stack
 */
void tstack_pop (tstack *stack)
{
	stack->a_reg = stack->b_reg;
	stack->b_reg = stack->c_reg;
	stack->c_reg = REG_UNDEFINED;
	return;
}
/*}}}*/
/*{{{  void tstack_checkdepth_le (tstack *stack, int depth)*/
/*
 *	makes sure depth of stack <= depth
 */
void tstack_checkdepth_le (tstack *stack, int depth)
{
	if (stack->old_ts_depth > depth) {
		switch (stack->old_ts_depth - depth) {
		case 1:
			stack->c_reg = REG_UNDEFINED;
			break;
		case 2:
			stack->b_reg = REG_UNDEFINED;
			stack->c_reg = REG_UNDEFINED;
			break;
		case 3:
			stack->a_reg = REG_UNDEFINED;
			stack->b_reg = REG_UNDEFINED;
			stack->c_reg = REG_UNDEFINED;
			break;
		}
		stack->ts_depth = depth;
	}
	return;
}
/*}}}*/
/*{{{  void tstack_push (tstack *stack)*/
/*
 *	pushes the stack
 */
void tstack_push (tstack *stack)
{
	tstack_checkdepth_le (stack, 2);
	stack->c_reg = stack->b_reg;
	stack->b_reg = stack->a_reg;
	stack->a_reg = stack->reg_counter++;
	return;
}
/*}}}*/
/*{{{  void tstack_checkdepth_ge (tstack *stack, int depth)*/
/*
 *	makes sure depth of stack >= depth
 */
void tstack_checkdepth_ge (tstack *stack, int depth)
{
	int i;

	if (stack->old_ts_depth < depth) {
		for (i=0; i < (depth - stack->old_ts_depth); i++) {
			tstack_push (stack);
			fprintf (stderr, "%s: warning: tstack underflow\n", progname);
		}
	}
	return;
}
/*}}}*/
/*{{{  void tstack_note (tstack *stack)*/
/*
 *	makes a note of the stack -- registers in use
 */
void tstack_note (tstack *stack)
{
	/* don't need this here */
	return;
}
/*}}}*/
/*{{{  void tstack_setprim (tstack *stack, int prim, arch_t *arch)*/
/*
 *	adjusts stack for primary instruction
 */
void tstack_setprim (tstack *stack, int prim, arch_t *arch)
{
	int ts_diff;
	int i;

	ts_diff = tsdiff_prim[prim];
	stack->old_ts_depth = stack->ts_depth;
	if (ts_diff >= 16) {
		tstack_undefine (stack);
		for (i=0; i<(ts_diff - 16); i++) {
			tstack_push (stack);
		}
		stack->ts_depth = ts_diff - 16;
	} else if (ts_diff < 0) {
		tstack_checkdepth_ge (stack, ts_diff);
		for (i=0; i<(-ts_diff); i++) {
			tstack_pop (stack);
		}
		stack->ts_depth = stack->old_ts_depth + ts_diff;
	} else if (ts_diff) {
		for (i=0; i<ts_diff; i++) {
			tstack_push (stack);
		}
		stack->ts_depth = stack->old_ts_depth + ts_diff;
		stack->must_set_cmp_flags = 1;
	}
	return;
}
/*}}}*/
/*{{{  void tstack_setsec (tstack *stack, int sec, arch_t *arch)*/
/*
 *	adjusts stack for secondary instruction
 */
void tstack_setsec (tstack *stack, int sec, arch_t *arch)
{
	int ts_diff;
	int i;

	if (sec < 0) {
		return;
	}
	if (sec == I_NULL) {
		ts_diff = 1;
	} else if ((sec >= 0x200) && (sec <= 0x2ff)) {
		ts_diff = tsdiff_sec_twocodes[sec - 0x200];
	} else if ((sec >= 0xe0) && (sec <= 0xef)) {
		ts_diff = tsdiff_sec_ecodes[sec - 0xe0];
	} else if (sec > 0xb0) {
		return;
	} else {
		ts_diff = tsdiff_sec[sec];
	}
	stack->old_ts_depth = stack->ts_depth;
	if (ts_diff >= 16) {
		tstack_undefine (stack);
		arch->compose_reset_fregs (stack->transtate);
		/* generate_reset_fregs_ext (stack->transtate); */
		for (i=0; i<(ts_diff - 16); i++) {
			tstack_push (stack);
		}
		stack->ts_depth = ts_diff - 16;
	} else if (ts_diff < 0) {
		tstack_checkdepth_ge (stack, ts_diff);
		for (i=0; i<(-ts_diff); i++) {
			tstack_pop (stack);
		}
		stack->ts_depth = stack->old_ts_depth + ts_diff;
	} else if (ts_diff) {
		for (i=0; i<ts_diff; i++) {
			tstack_push (stack);
		}
		stack->ts_depth = stack->old_ts_depth + ts_diff;
		stack->must_set_cmp_flags = 1;
	}
	return;
}
/*}}}*/
/*{{{  int tstack_newreg (tstack *stack)*/
/*
 *	returns a new register
 */
int tstack_newreg (tstack *stack)
{
	return stack->reg_counter++;
}
/*}}}*/


/*{{{  void constmap_cleanup (tstack *stack)*/
/*
 *	cleans up lost registers
 */
void constmap_cleanup (tstack *stack)
{
	int regs_to_keep[4];
	int n_regs, i, j;

	tran_cm_cidx = -1;
	regs_to_keep[0] = (stack->ts_depth > 0) ? stack->a_reg : REG_UNDEFINED;
	regs_to_keep[1] = (stack->ts_depth > 1) ? stack->b_reg : REG_UNDEFINED;
	regs_to_keep[2] = (stack->ts_depth > 2) ? stack->c_reg : REG_UNDEFINED;
	regs_to_keep[3] = REG_UNDEFINED;
	for (n_regs = 0; regs_to_keep[n_regs] != REG_UNDEFINED; n_regs++);
	for (i=0; i<tran_cm_cur; i++) {
		for (j=0; j<n_regs; j++) {
			if (tran_constmap[i]->v_reg == regs_to_keep[j]) {
				#ifdef DEBUG_CONSTMAP
					fprintf (stderr, "cleanup: should keep register %d\n", tran_constmap[i]->v_reg);
				#endif	/* DEBUG_CONSTMAP */
				break;
			}
		}
		if (j == n_regs) {
			/* not here */
			constmap_remove (tran_constmap[i]->v_reg);
		}
	}
	return;
}
/*}}}*/
/*{{{  int constmap_typeof (int reg)*/
/*
 *	returns VALUE_... for `reg'
 */
int constmap_typeof (int reg)
{
	int i;

	for (i=0; (i<tran_cm_cur) && (tran_constmap[i]->v_reg <= reg); i++) {
		if (tran_constmap[i]->v_reg == reg) {
			tran_cm_cidx = i;
			return tran_constmap[i]->type;
		}
	}
	return VALUE_UNDEFINED;
}
/*}}}*/
/*{{{  int constmap_regconst (int reg)*/
/*
 *	returns constant value of `reg' -- errors if not here
 */
int constmap_regconst (int reg)
{
	int i;

	if ((tran_cm_cidx > -1) && (tran_constmap[tran_cm_cidx]->v_reg == reg)) {
		return tran_constmap[tran_cm_cidx]->c_val;
	}
	for (i=0; (i<tran_cm_cur) && (tran_constmap[i]->v_reg <= reg); i++) {
		if (tran_constmap[i]->v_reg == reg) {
			return tran_constmap[i]->c_val;
		}
	}
	fprintf (stderr, "%s: fatal: no constant for constmap_regconst (%d)\n", progname, reg);
	exit (EXIT_FAILURE);
}
/*}}}*/
/*{{{  void constmap_modregconst (int reg, int val)*/
/*
 *	modifies the value of constant in `reg' -- errors if not here
 */
void constmap_modregconst (int reg, int val)
{
	int i;

	if (options.internal_options & INTERNAL_NOCONSTMAP) {
		/* avoid mapping constants */
		return;
	}
	if ((tran_cm_cidx > -1) && (tran_constmap[tran_cm_cidx]->v_reg == reg)) {
		tran_constmap[tran_cm_cidx]->c_val = val;
		return;
	}
	for (i=0; (i<tran_cm_cur) && (tran_constmap[i]->v_reg <= reg); i++) {
		if (tran_constmap[i]->v_reg == reg) {
			tran_constmap[i]->c_val = val;
			return;
		}
	}
	fprintf (stderr, "%s: fatal: no constant for constmap_modregconst (%d,0x%8.8x)\n", progname, reg, (unsigned int)val);
	exit (EXIT_FAILURE);
}
/*}}}*/
/*{{{  int constmap_otherlab (int reg)*/
/*
 *	returns the other label for VALUE_LABDIFF
 */
int constmap_otherlab (int reg)
{
	int i;

	if ((tran_cm_cidx > -1) && (tran_constmap[tran_cm_cidx]->v_reg == reg)) {
		return tran_constmap[tran_cm_cidx]->c_val2;
	}
	for (i=0; (i<tran_cm_cur) && (tran_constmap[i]->v_reg <= reg); i++) {
		if (tran_constmap[i]->v_reg == reg) {
			return tran_constmap[i]->c_val2;
		}
	}
	fprintf (stderr, "%s: fatal: no constant for constmap_otherlab (%d)\n", progname, reg);
	exit (EXIT_FAILURE);
}
/*}}}*/
/*{{{  void constmap_remove (int reg)*/
/*
 *	removes `reg' from constant map (destructive updates, etc.)
 */
void constmap_remove (int reg)
{
	int i, j;

	if ((tran_cm_cidx > -1) && (tran_constmap[tran_cm_cidx]->v_reg == reg)) {
		i = tran_cm_cidx;
		tran_cm_cidx = -1;
	} else {
		for (i=0; (i < tran_cm_cur) && (tran_constmap[i]->v_reg != reg); i++);
		if (i == tran_cm_cur) {
			/* not here */
			#ifdef DEBUG_CONSTMAP
				fprintf (stderr, "reg %d not in constmap in constmap_remove\n", reg);
			#endif	/* DEBUG_CONSTMAP */
			return;
		}
	}
	/* remove at index i */
	if (tran_cm_cidx >= i) {
		tran_cm_cidx = -1;
	}
	sfree (tran_constmap[i]);
	tran_cm_cur--;
	if (i == tran_cm_cur) {
		tran_constmap[i] = NULL;
	} else {
		for (j=i; j<tran_cm_cur; j++) {
			tran_constmap[j] = tran_constmap[j+1];
		}
		tran_constmap[j] = NULL;
	}
	#ifdef DEBUG_CONSTMAP
		fprintf (stderr, "removed reg %d from constant map\n", reg);
	#endif	/* DEBUG_CONSTMAP */
	return;
}
/*}}}*/
/*{{{  void constmap_removelocal (int slot)*/
/*
 *	removes any registers which have loaded constant offset of `slot'
 */
void constmap_removelocal (int slot)
{
	int i;

	for (i=0; i<tran_cm_cur;) {
		if ((tran_constmap[i]->type == VALUE_LOCAL) && (tran_constmap[i]->c_val == slot)) {
			tran_cm_cidx = i;
			constmap_remove (tran_constmap[i]->v_reg);
		} else {
			i++;
		}
	}
	return;
}
/*}}}*/
/*{{{  void constmap_clearall (void)*/
/*
 *	removes all entries in the constant map
 */
void constmap_clearall (void)
{
	while (tran_cm_cur) {
		constmap_remove (tran_constmap[0]->v_reg);
	}
	return;
}
/*}}}*/
/*{{{  void constmap_new (int reg, int consttype, int constval, ins_chain *ins)*/
/*
 *	adds `reg' with constant value `constval' (loaded at instruction `ins') to constant table
 */
void constmap_new (int reg, int consttype, int constval, ins_chain *ins)
{
	constmap *tmp;
	int i, j;

	if (options.internal_options & INTERNAL_NOCONSTMAP) {
		/* avoid mapping constants */
		return;
	}
	#ifdef DEBUG_CONSTMAP
		fprintf (stderr, "new constant in register %d, type %d, val = 0x%8.8x, instruction = %p\n", reg, consttype, constval, ins);
	#endif	/* DEBUG_CONSTMAP */
	tmp = (constmap *)smalloc (sizeof (constmap));
	tmp->v_reg = reg;
	tmp->type = consttype;
	if (consttype == VALUE_LABDIFF) {
		/* dirty... */
		int *labs;

		labs = (int *)constval;
		tmp->c_val = labs[0];
		tmp->c_val2 = labs[1];
	} else {
		tmp->c_val = constval;
		tmp->c_val2 = 0;
	}
	tmp->load_ins = ins;
	/* insert sort */
	if (tran_cm_cur == tran_cm_max) {
		tran_constmap = (constmap **)srealloc (tran_constmap, (tran_cm_max * sizeof (constmap *)), ((tran_cm_max + 5) * sizeof (constmap *)));
		tran_cm_max += 5;
	}
	for (i=0; i<tran_cm_cur; i++) {
		if (reg < tran_constmap[i]->v_reg) {
			break;
		}
	}
	if (i < tran_cm_cur) {
		for (j = tran_cm_cur; j > i; j--) {
			tran_constmap[j] = tran_constmap[j - 1];
		}
	}
	tran_constmap[i] = tmp;
	tran_cm_cur++;
	#ifdef DEBUG_CONSTMAP
		fprintf (stderr, "regmap is now [");
		for (i=0; i<tran_cm_cur; i++) {
			fprintf (stderr, "%d%s", tran_constmap[i]->v_reg, (i == (tran_cm_cur - 1)) ? "]\n" : ",");
		}
	#endif	/* DEBUG_CONSTMAP */
	return;
}
/*}}}*/
/*{{{  void constmap_regcopy (int old_reg, int new_reg, ins_chain *ins)*/
/*
 *	if `old_reg' is a constant something, `new_reg' is added with instruction `ins'
 */
void constmap_regcopy (int old_reg, int new_reg, ins_chain *ins)
{
	if (constmap_typeof (old_reg) != VALUE_UNDEFINED) {
		constmap_new (new_reg, constmap_typeof (old_reg), constmap_regconst (old_reg), ins);
	}
	return;
}
/*}}}*/


/*{{{  void tstack_fpclear (tstack *stack, arch_t *arch)*/
/*
 *	clears the floating-point stack
 */
void tstack_fpclear (tstack *stack, arch_t *arch)
{
	int i;
	int regs[16];

	stack->fs_depth = 0;
	if (!arch || !arch->regcolour_fp_regs) {
		for (i=0; i<16; i++) {
			regs[i] = -1;
		}
	} else {
		int nregs = arch->regcolour_fp_regs (regs);

		for (i=nregs; i<16; i++) {
			regs[i] = -1;
		}
	}
	for (i=0; i<FPSTACKDEPTH; i++) {
		stack->fregs[i] = regs[i];
	}
	return;
}
/*}}}*/
/*{{{  int tstack_fppush (tstack *stack, arch_t *arch)*/
/*
 *	pushes the floating-point stack, returns the new register in use.
 *	Uses real registers, so needs arch info to get these.
 */
int tstack_fppush (tstack *stack, arch_t *arch)
{
	int reg = stack->fregs[stack->fs_depth];

	stack->fs_depth++;
	if (stack->fs_depth >= FPSTACKDEPTH) {
		fprintf (stderr, "%s: warning: floating-point stack overflow\n", progname);
		stack->fs_depth--;
	}
	return reg;
}
/*}}}*/
/*{{{  int tstack_fppop (tstack *stack, arch_t *arch)*/
/*
 *	pops the floating-point stack, returns the register lost.
 */
int tstack_fppop (tstack *stack, arch_t *arch)
{
	stack->fs_depth--;
	if (stack->fs_depth < 0) {
		fprintf (stderr, "%s: warning: floating-point stack underflow\n", progname);
		stack->fs_depth++;
	}
	return stack->fregs[stack->fs_depth];
}
/*}}}*/
/*{{{  int tstack_fpinstack (tstack *stack, int reg)*/
/*
 *	returns a register in the FPU stack, "reg" is given
 *	as 0=FPAREG, 1=FPBREG, 2=FPCREG, ..  (more allowed for some architectures)
 */
int tstack_fpinstack (tstack *stack, int reg)
{
	if (reg >= stack->fs_depth) {
		fprintf (stderr, "%s: serious: tstack_fpinstack(): asking for %d, FP stack %d deep\n", progname, reg, stack->fs_depth);
		return 0;
	}
	return stack->fregs[stack->fs_depth - (reg + 1)];
}
/*}}}*/
/*{{{  int tstack_fpsetstack (tstack *stack, int reg, int fpr)*/
/*
 *	sets a register in the FPU stack, "reg" is given as 0=FPAREG, ...,
 *	"fpr" is the actual register number.  returns the register replaced.
 */
int tstack_fpsetstack (tstack *stack, int reg, int fpr)
{
	int r;

	if (reg >= stack->fs_depth) {
		fprintf (stderr, "%s: serious: tstack_fpsetstack(): setting %d, FP stack %d deep\n", progname, reg, stack->fs_depth);
		return 0;
	}
	r = stack->fregs[stack->fs_depth - (reg + 1)];
	stack->fregs[stack->fs_depth - (reg + 1)] = fpr;

	return r;
}
/*}}}*/


