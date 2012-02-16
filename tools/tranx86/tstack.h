/*
 *	tstack.h - definition of transputer stack
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

#ifndef __TSTACK_H
#define __TSTACK_H

#define REG_SCHED -8		/* ends up in ESI (on MP x86) */
#define REG_CA -7		/* carry-out register (needed for some architectures) */
#define REG_CC -6		/* condition codes */
#define REG_SPTR -5		/* ends up in ESP (on i386) */
#define REG_BPTR -4		/* ends up in EDI (on i386) */
#define REG_FPTR -3		/* ends up in ESI (on i386) */
#define REG_LPTR -4		/* ends up in EDI (old/depricated) (on i386) */
#define REG_JPTR -3		/* ends up in ESI (old/depricated) (on i386) */
#define REG_WPTR -2		/* ends up in EBP (on i386) */
#define REG_UNDEFINED -1

#define FIRST_VIRTUAL_REG 48	/* for all architectures */

#define VALUE_UNDEFINED	0
#define VALUE_CONST	1	/* numeric constants (LDC) */
#define VALUE_LOCAL	2	/* workspace offset constants (LDL) */
#define VALUE_LABADDR	3	/* label address constant */
#define VALUE_LOCALPTR	4	/* workspace offset (LDLP) */
#define VALUE_LABDIFF	5	/* label difference */


#define FPSTACKDEPTH	5

typedef struct constmap_t {
	int v_reg;		/* register */
	int type;		/* VALUE_... */
	int c_val;		/* constant value */
	int c_val2;		/* other label for VALUE_LABDIFF */
	ins_chain *load_ins;	/* pointer to load instruction */
} constmap;

typedef struct TAG_tstack {
	struct TAG_tstack *next;
	void *transtate;		/* cast to (tstate *) in etcrtl.c for resetting of FPU */
	int a_reg, b_reg, c_reg;
	int old_a_reg, old_b_reg, old_c_reg;
	int ts_depth, old_ts_depth;
	int fs_depth, old_fs_depth, fpu_mode;
	int fregs[FPSTACKDEPTH];	/* floating-point registers (if needed) */
	int reg_counter;
	int must_set_cmp_flags;
} tstack;

struct TAG_arch_t;

#ifndef __TSTACK_C
	extern tstack *new_tstack (void);
	extern void tstack_destroy (tstack *stack);
	extern void tstack_undefine (tstack *stack);
	extern void tstack_pop (tstack *stack);
	extern void tstack_push (tstack *stack);
	extern void tstack_checkdepth_le (tstack *stack, int depth);
	extern void tstack_checkdepth_ge (tstack *stack, int depth);
	extern void tstack_note (tstack *stack);
	extern void tstack_setprim (tstack *stack, int prim, struct TAG_arch_t *arch);
	extern void tstack_setsec (tstack *stack, int sec, struct TAG_arch_t *arch);
	extern int tstack_newreg (tstack *stack);
#endif	/* !__TSTACK_C */

extern void constmap_cleanup (tstack *stack);
extern int constmap_typeof (int reg);
extern int constmap_regconst (int reg);
extern int constmap_otherlab (int reg);
extern void constmap_remove (int reg);
extern void constmap_removelocal (int slot);
extern void constmap_clearall (void);
extern void constmap_new (int reg, int consttype, int constval, ins_chain *ins);
extern void constmap_regcopy (int old_reg, int new_reg, ins_chain *ins);
extern void constmap_modregconst (int reg, int val);

extern void tstack_fpclear (tstack *stack, struct TAG_arch_t *arch);
extern int tstack_fppush (tstack *stack, struct TAG_arch_t *arch);
extern int tstack_fppop (tstack *stack, struct TAG_arch_t *arch);
extern int tstack_fpinstack (tstack *stack, int reg);			/* 0 = FPAREG, 1 = FPBREG, .. */
extern int tstack_fpsetstack (tstack *stack, int reg, int fpr);		/* 0 = FPAREG, 1 = FPBREG, .. */


#endif	/* !__TSTACK_H */

