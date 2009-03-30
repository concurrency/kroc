/*
 *	archdef.h -- architecture definition
 *	Copyright (C) 2002 Fred Barnes <frmb@kent.ac.uk>
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

#ifndef __ARCHDEF_H
#define __ARCHDEF_H

#include "tstate.h"
#include "kif.h"

typedef struct TAG_arch_t {
	/* common */
	char *archname;									/* something vaguely descriptive */

	/* run-time kernel interfacing */
	void (*compose_kcall)(tstate *, const int, const int, const int);		/* state, call, regs in, regs out */
	ins_chain *(*compose_kjump)(tstate *, const int, const int, const kif_entrytype *);	/* state, instruction, condition, entry */
	void (*compose_deadlock_kcall)(tstate *, const int, const int, const int);	/* state, call, regs in, regs out */

	/* half-way stuff */
	void (*compose_pre_enbc)(tstate *);						/* state */
	void (*compose_pre_enbt)(tstate *);						/* state */

	/* inlining stuff */
	void (*compose_inline_ldtimer)(tstate *);					/* state */
	void (*compose_inline_tin)(tstate *);						/* state */
	void (*compose_inline_quick_reschedule)(tstate *);				/* state */
	void (*compose_inline_full_reschedule)(tstate *);				/* state */
	void (*compose_inline_in)(tstate *, int);					/* state, width */
	void (*compose_inline_in_2)(tstate *, int);					/* state, width */
	void (*compose_inline_min)(tstate *, int);					/* state, wide */
	void (*compose_inline_out)(tstate *, int);					/* state, width */
	void (*compose_inline_out_2)(tstate *, int);					/* state, width */
	void (*compose_inline_mout)(tstate *, int);					/* state, wide */
	void (*compose_inline_enbc)(tstate *, int);					/* state, instr */
	void (*compose_inline_disc)(tstate *, int);					/* state, instr */
	void (*compose_inline_altwt)(tstate *);						/* state */
	void (*compose_inline_stlx)(tstate *, int);					/* state, instr */
	void (*compose_inline_malloc)(tstate *);					/* state */
	void (*compose_inline_startp)(tstate *);					/* state */
	void (*compose_inline_endp)(tstate *);						/* state */
	void (*compose_inline_runp)(tstate *);						/* state */
	void (*compose_inline_stopp)(tstate *);						/* state */

	/* run-time debugging stuff */
	void (*compose_debug_insert)(tstate *, int);					/* state, md id -- debugging info */
	void (*compose_debug_procnames)(tstate *);					/* state */
	void (*compose_debug_filenames)(tstate *);					/* state */
	void (*compose_debug_zero_div)(tstate *);					/* state */
	void (*compose_debug_floaterr)(tstate *);					/* state */
	void (*compose_debug_overflow)(tstate *);					/* state */
	void (*compose_debug_rangestop)(tstate *);					/* state */
	void (*compose_debug_seterr)(tstate *);						/* state */
	void (*compose_overflow_jumpcode)(tstate *, int);				/* state, dcode */
	void (*compose_floaterr_jumpcode)(tstate *);					/* state */
	void (*compose_rangestop_jumpcode)(tstate *, int);				/* state, rcode */

	void (*compose_debug_deadlock_set)(tstate *);					/* state -- return point */

	/* code-gen stuff */
	void (*compose_divcheck_zero)(tstate *, int);					/* state, reg */
	void (*compose_divcheck_zero_simple)(tstate *, int);				/* state, reg */
	void (*compose_division)(tstate *, int, int, int);				/* state, reg1 / reg2 -> reg3 */
	int (*compose_remainder)(tstate *, int, int);					/* state, reg1 / reg2 => reg */
	int (*compose_iospace_loadbyte)(tstate *, int, int);				/* state, reg1 -> reg2 => reg */
	void (*compose_iospace_storebyte)(tstate *, int, int);				/* state, reg1 -> reg2 */
	int (*compose_iospace_loadword)(tstate *, int, int);				/* state, reg1 -> reg2 => reg */
	void (*compose_iospace_storeword)(tstate *, int, int);				/* state, reg1 -> reg2 */
	void (*compose_iospace_read)(tstate *, int, int, int);				/* state, reg1 -> reg2 -> width */
	void (*compose_iospace_write)(tstate *, int, int, int);				/* state, reg1 -> reg2 -> width */
	void (*compose_move_loadptrs)(tstate *);					/* state */
	void (*compose_move)(tstate *);							/* state */
	void (*compose_shift)(tstate *, int, int, int, int);				/* state, shift-type, reg1 shifted-by reg2 => reg3 */

	/* more specific code-gen stuff */
	int (*compose_widenshort)(tstate *);						/* state => reg */
	int (*compose_widenword)(tstate *);						/* state => reg */
	void (*compose_longop)(tstate *, int);						/* state, secondary-opcode */
	void (*compose_fpop)(tstate *, int);						/* state, secondary-opcode */
	void (*compose_external_ccall)(tstate *, int, char *, ins_chain **, ins_chain **);	/* state, inlined, name, ret-1st-instr, ret-last-instr */
	void (*compose_bcall)(tstate *, int, int, int, char *, ins_chain **, ins_chain **);	/* state, inlined, name, ret-1st-instr, reg-last-instr */
	void (*compose_cif_call)(tstate *, int,char *, ins_chain **, ins_chain **);		/* state, inlined, name, ret-1st-instr, ret-last-instr */

	void (*compose_entry_prolog)(tstate *);						/* state */
	void (*compose_rmox_entry_prolog)(tstate *, rmoxmode_e);			/* state, RMoX mode */
	void (*compose_fp_set_fround)(tstate *, int);					/* state, mode */
	void (*compose_fp_init)(tstate *);						/* state */
	void (*compose_reset_fregs)(tstate *);						/* state */

	void (*compose_refcountop)(tstate *, int, int);					/* state, opcode, addr reg */

	void (*compose_memory_barrier)(tstate *, int);					/* state, secondary-opcode */

	/* function/proc return handling */
	void (*compose_return)(tstate *);						/* state */
	void (*compose_nreturn)(tstate *, int);						/* state, adjustment */
	void (*compose_funcresults)(tstate *, int);					/* state, num-results */

	/* register allocation */
	int (*regcolour_special_to_real)(int reg);					/* reg => reg */
	int regcolour_rmax;								/* count */
	int regcolour_nodemax;								/* count */
	int (*regcolour_get_regs)(int *);						/* reg-array */
	int (*regcolour_fp_regs)(int *);						/* reg-array */

	/* output generation */
	int (*code_to_asm)(rtl_chain *, char *);					/* RTL/IMC, filename */
	int (*code_to_asm_stream)(rtl_chain *, FILE *);					/* RTL/IMC, stream */

	/* arch-specific RTL stuff */
	int (*rtl_validate_instr)(ins_chain *);						/* instr */
	int (*rtl_prevalidate_instr)(ins_chain *);					/* instr */
	char *(*get_register_name)(int);						/* reg */

	int int_options;								/* internal options (below) */
	int kiface_tableoffs;								/* how far away on the C stack the call-table is */
} arch_t;

#define INTOPT_NOREGSQUEEZE	0x0001							/* don't attempt to squeeze register usage */


#endif	/* !__ARCHDEF_H */

