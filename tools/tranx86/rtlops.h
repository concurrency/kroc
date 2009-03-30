/*
 *	rtlops.h - interface to simple RTL operations
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


#ifndef __RTLOPS_H
#define __RTLOPS_H

/* things returned by rtl_classify_instr () */
#define ICLASS_UNKNOWN 0
#define ICLASS_LOADCONST 1
#define ICLASS_TERMINAL 2
#define ICLASS_SETLAB 3

extern rtl_chain *rtl_compact_datablocks (rtl_chain *rtl_code);
extern rtl_chain *rtl_compact_codeblocks (rtl_chain *rtl_code);
extern rtl_chain *rtl_relocate_data (rtl_chain *rtl_code);
extern void rtl_insert_instr_before (ins_chain *newins, ins_chain *before);
extern void rtl_insert_instr_after (ins_chain *newins, ins_chain *after);
extern void rtl_insert_instr_block_after (ins_chain *newchain, ins_chain *after);
extern void rtl_remove_instr (ins_chain *instr);
extern void rtl_unlink_instr (ins_chain *ins);
extern ins_chain *rtl_scan_constrain_forward (ins_chain *startfrom, int vreg);
extern ins_chain *rtl_scan_constrain_backward (ins_chain *startfrom, int vreg);
extern ins_chain *rtl_scan_unconstrain_forward (ins_chain *startfrom, int vreg);
extern ins_chain *rtl_scan_unconstrain_backward (ins_chain *startfrom, int vreg);
extern ins_chain *rtl_scan_for_constrain_to (ins_chain *startfrom, ins_chain *endat, int reg);
extern ins_chain *rtl_scan_start_forward (ins_chain *startfrom, int vreg);
extern ins_chain *rtl_scan_start_backward (ins_chain *startfrom, int vreg);
extern ins_chain *rtl_scan_end_forward (ins_chain *startfrom, int vreg);
extern ins_chain *rtl_scan_end_backward (ins_chain *startfrom, int vreg);
extern ins_chain *rtl_scan_setscc_backward (ins_chain *startfrom, int stopatlab);
extern ins_chain *rtl_scan_setsca_backward (ins_chain *startfrom, int stopatlab);
extern ins_chain *rtl_scan_iclass_backward (ins_chain *startfrom, int iclass);
extern int rtl_instr_setscc (ins_chain *ins);
extern int rtl_instr_setsca (ins_chain *ins);
extern int rtl_instr_usesca (ins_chain *ins);
extern void rtl_refix_codeblocks (rtl_chain *rtl_code);
extern int rtl_nvregs_in_arg (ins_arg *arg);
extern int rtl_nvreg_of_arg (ins_arg *arg, int n);
extern int rtl_rename_reg (ins_chain *startfrom, int old_reg, int new_reg);
extern int rtl_rename_reg_block (ins_chain *startfrom, ins_chain *endat, int old_reg, int new_reg);
extern int rtl_rename_reg_in_arg (ins_arg *arg, int old_reg, int new_reg);
extern int rtl_trace_regs (rtl_chain *rtl_code);
extern int rtl_check_consistency (rtl_chain *rtl_code);
extern rtl_chain *rtl_remove_empty (rtl_chain *rtl_code);
extern int rtl_remove_deadnamelabels (rtl_chain *rtl_code);
extern int rtl_validate_instr (ins_chain *ins, arch_t *arch);
extern int rtl_prevalidate_instr (ins_chain *ins, arch_t *arch);
extern int rtl_validate_rtl (rtl_chain *rtl_code, arch_t *arch);
extern int rtl_prevalidate_rtl (rtl_chain *rtl_code, arch_t *arch);
extern void rtl_free_arg (ins_arg *arg);
extern void rtl_free_instr (ins_chain *ins);
extern rtl_chain *new_rtl (void);
extern void free_rtl (rtl_chain *);
extern ins_chain *new_ins (void);
extern ins_arg *new_ins_arg (void);
extern ins_arg *compose_ins_arg (int argtype, ...);
extern ins_sib_arg *new_ins_sib_arg (void);
extern void rtl_set_lastvreg (int reg);
extern int rtl_get_newvreg (void);
extern int rtl_get_lastvreg (void);
#ifdef INSTRUCTION_HISTORY
	extern ins_chain *compose_ins2 (char *file, long line, int etc_ins, int ins, int ops_in, int ops_out, ...);
	#define compose_ins(ins, ops_in, ops_out, args...) \
		compose_ins2(__FILE__, __LINE__, 0, ins, ops_in, ops_out, ## args)
	#define compose_ins_ex(etc_ins, ins, ops_in, ops_out, args...) \
		compose_ins2(__FILE__, __LINE__, etc_ins, ins, ops_in, ops_out, ## args)
#else	/* INSTRUCTION_HISTORY */
	//extern ins_chain *compose_ins (int ins, int ops_in, int ops_out, ...);
	#define compose_ins(ins, ops_in, ops_out, args...) \
		compose_ins_ex(0, ins, ops_in, ops_out, ## args)
	extern ins_chain *compose_ins_ex(int etc_ins, int ins, int ops_in, int ops_out, ...);
#endif	/* !INSTRUCTION_HISTORY */
extern int rtl_const_bitwidth (int val, int issigned);
extern int rtl_instr_width (ins_chain *ins);
extern int rtl_classify_instr (ins_chain *ins);
extern int rtl_cleanup_flabels (rtl_chain *rtl_code);
extern ins_chain *rtl_cleanup_code (ins_chain *ins);
extern rtl_chain *rtl_cleanup_code_all (rtl_chain *rtl);
extern int rtl_link_jumps (rtl_chain *);
extern int rtl_destructive_sequence (ins_chain *first, ins_chain *last);
extern int rtl_compare_args (ins_arg *arg1, ins_arg *arg2);
extern int rtl_arg_in_sequence (ins_arg *arg, ins_chain *first, ins_chain *last);
extern ins_chain *rtl_next_instr (ins_chain *ins);
extern ins_chain *rtl_prev_instr (ins_chain *ins);
extern ins_chain *rtl_last_instr (rtl_chain *rtl);
extern int rtl_result_of_const_compare (int c1, int c2, int cond);
extern ins_labrefs *rtl_add_labref (ins_labrefs *labrefs, ins_chain *ins);
extern int rtl_del_labref (ins_labrefs *labrefs, ins_chain *ins);
extern void rtl_removing_labref (ins_chain *ins);
extern void rtl_rename_label (int from_lab, ins_chain *from_lab_def, int to_lab, ins_chain *to_lab_def);
extern int rtl_glob_codelabels (ins_chain *head);
extern int rtl_glob_labels (rtl_chain *rtl_code);
extern ins_sib_arg *rtl_copy_sib_arg (ins_sib_arg *sibarg);
extern ins_arg *rtl_copy_arg (ins_arg *arg, ins_chain *addins);
extern ins_chain *rtl_copy_instr (ins_chain *ins);
extern ins_chain *rtl_copy_codeblock (ins_chain *first, ins_chain *last);
extern ins_chain *rtl_copy_code (ins_chain *code);
extern int rtl_count_instrs (rtl_chain *rtl_code);
extern int rtl_validate_checknumargs (ins_chain *, int, int);
extern int rtl_validate_checkargtype (ins_arg *, ...);

#endif	/* !__RTLOPS_H */

