/*
 *	regcolour.c - does the register colouring..
 *	Copyright (C) 2000-2002 Fred Barnes <frmb@kent.ac.uk>
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
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "main.h"
#include "support.h"
#include "structs.h"
#include "transputer.h"
#include "trancomm.h"
#include "tstack.h"
#include "postmortem.h"
#include "intel.h"
#include "archdef.h"
#include "rtlops.h"
#include "arch386.h"

#define DEBUG_REGCOLOUR 0

typedef struct TAG_graphnode {
	int vreg;			/* virtual register */
	int rreg;			/* real register */
	struct TAG_graphnode **links;	/* links to other nodes */
	int n_links;			/* how many links */
	ins_chain *start_ins;		/* start register instruction */
	ins_chain *constrain_ins;	/* register constraint instruction */
	ins_chain *end_ins;		/* end register instruction */
} graphnode;


static graphnode *new_graphnode (int vreg, ins_chain *start_ins);
static void free_graphnode (graphnode *node);
static int count_edges (int *letab, int *hetab, int n_edges, int reg);
static void link_node_by_edges (graphnode *node, graphnode **nodes, int n_nodes, int *letab, int *hetab, int n_edges);
static int constrain_and_check (graphnode *node);
static int set_edges_in_table (int *live_regs, int n_live_regs, int **letab, int **hetab, int *tabcur, int *tabmax);
static int add_to_edge_table (int reg1, int reg2, int **letab, int **hetab, int *tabcur, int *tabmax);
static int merge_multiple_constraints (ins_chain *code);
static int do_graph_colour (graphnode **nodes, int n_nodes, arch_t *arch);
static int select_register (graphnode *node, arch_t *arch);
static int choose_if_split (graphnode **nodes, int n_nodes, int this_one, arch_t *arch);
static int colour_code_block (ins_chain *code, arch_t *arch);
static int colour_code_fragment (ins_chain *start, ins_chain *end, int vreg, int rreg);
static int fix_special_regs (ins_chain *ins, arch_t *arch);


#if DEBUG_REGCOLOUR
	static void dump_edge_table (int *letab, int *hetab, int n_edges);
	static void dump_graph (graphnode **nodes, int n_nodes);
#endif

/*{{{  static int colour_code_fragment (ins_chain *start, ins_chain *end, int vreg, int rreg)*/
/*
 *	colours in a code block
 */
static int colour_code_fragment (ins_chain *start, ins_chain *end, int vreg, int rreg)
{
	ins_chain *tmp;
	ins_arg *arg;
	ins_sib_arg *t_sib;
	int i;

	for (tmp=start; tmp != end->next; tmp=tmp->next) {
		for (i=0; tmp->in_args[i]; i++) {
			arg = tmp->in_args[i];
			switch (arg->flags & ARG_MODEMASK) {
			case ARG_REG:
			case ARG_REGIND:
				if (arg->regconst == vreg) {
					arg->regconst = rreg;
				}
				break;
			case ARG_REGINDSIB:
				t_sib = (ins_sib_arg *)arg->regconst;
				if (t_sib->base == vreg) {
					t_sib->base = rreg;
				}
				if (t_sib->index == vreg) {
					t_sib->index = rreg;
				}
				break;
			default:
				break;
			}
		}
		for (i=0; tmp->out_args[i]; i++) {
			arg = tmp->out_args[i];
			switch (arg->flags & ARG_MODEMASK) {
			case ARG_REG:
			case ARG_REGIND:
				if (arg->regconst == vreg) {
					arg->regconst = rreg;
				}
				break;
			case ARG_REGINDSIB:
				t_sib = (ins_sib_arg *)arg->regconst;
				if (t_sib->base == vreg) {
					t_sib->base = rreg;
				}
				if (t_sib->index == vreg) {
					t_sib->index = rreg;
				}
				break;
			default:
				break;
			}
		}
	}
	return 0;
}
/*}}}*/
/*{{{  static int add_to_edge_table (int reg1, int reg2, int **letab, int **hetab, int *tabcur, int *tabmax)*/
/*
 *	adds an edge (reg1--reg2) to the edge table (letab, hetab, tabcur, tabmax)
 */
static int add_to_edge_table (int reg1, int reg2, int **letab, int **hetab, int *tabcur, int *tabmax)
{
	int tmp, i;

	if (reg1 > reg2) {
		tmp = reg1;
		reg1 = reg2;
		reg2 = tmp;
	} else if (reg1 == reg2) {
		fprintf (stderr, "%s: add_to_edge_table: ignoring confused edge (%d, %d)\n", progname, reg1, reg2);
		return 0;
	}
	for (i=0; i<*tabcur; i++) {
		if (((*letab)[i] == reg1) && ((*hetab)[i] == reg2)) {
			break;
		}
	}
	if (i == *tabcur) {
		/* add to edge table */
		if (*tabcur == *tabmax) {
			*letab = (int *)srealloc ((void *)*letab, *tabmax * sizeof(int), (*tabmax + 10) * sizeof(int));
			*hetab = (int *)srealloc ((void *)*hetab, *tabmax * sizeof(int), (*tabmax + 10) * sizeof(int));
			*tabmax += 10;
		}
		(*letab)[*tabcur] = reg1;
		(*hetab)[*tabcur] = reg2;
		(*tabcur)++;
	}
	return 0;
}
/*}}}*/
/*{{{  static int set_edges_in_table (int *live_regs, int n_live_regs, int **letab, int **hetab, int *tabcur, int *tabmax)*/
/*
 *	sets edges in the edge table from the live register set
 */
static int set_edges_in_table (int *live_regs, int n_live_regs, int **letab, int **hetab, int *tabcur, int *tabmax)
{
	int i, j, r;

	for (i=0; i<(n_live_regs-1); i++) {
		if (live_regs[i] > -1) {
			for (j=(i+1); j<n_live_regs; j++) {
				if (live_regs[j] > -1) {
					r = add_to_edge_table (live_regs[i], live_regs[j], letab, hetab, tabcur, tabmax);
					if (r < 0) {
						return r;
					}
				}
			}
		}
	}
	return 0;
}
/*}}}*/
/*{{{  static graphnode *new_graphnode (int vreg, ins_chain *start_ins)*/
/*
 *	creates + returns a new graph node
 */
static graphnode *new_graphnode (int vreg, ins_chain *start_ins)
{
	graphnode *tmp;

	tmp = (graphnode *)smalloc (sizeof (graphnode));
	tmp->vreg = vreg;
	tmp->rreg = -1;
	tmp->links = NULL;
	tmp->n_links = 0;
	tmp->start_ins = start_ins;
	tmp->constrain_ins = NULL;
	tmp->end_ins = NULL;
	return tmp;
}
/*}}}*/
/*{{{  static void free_graphnode (graphnode *node)*/
/*
 *	frees a graph node
 */
static void free_graphnode (graphnode *node)
{
	sfree (node);
	return;
}
/*}}}*/
/*{{{  static int count_edges (int *letab, int *hetab, int n_edges, int reg)*/
/*
 *	counts the number of edges involving a particular register
 */
static int count_edges (int *letab, int *hetab, int n_edges, int reg)
{
	int i, c;

	for (i=c=0; i<n_edges; i++) {
		if ((letab[i] == reg) || (hetab[i] == reg)) {
			c++;
		}
	}
	return c;
}
/*}}}*/
/*{{{  static void link_node_by_edges (graphnode *node, graphnode **nodes, int n_nodes, int *letab, int *hetab, int n_edges)*/
/*
 *	links a node to its neighbours
 */
static void link_node_by_edges (graphnode *node, graphnode **nodes, int n_nodes, int *letab, int *hetab, int n_edges)
{
	int i, j, c, linkreg;

	for (i=c=0; i<n_edges; i++) {
		linkreg = -1;
		if (letab[i] == node->vreg) {
			linkreg = hetab[i];
		} else if (hetab[i] == node->vreg) {
			linkreg = letab[i];
		}
		if (linkreg > -1) {
			for (j=0; j<n_nodes; j++) {
				if (nodes[j]->vreg == linkreg) {
					node->links[c++] = nodes[j];
					break;
				}
			}
			if (j == n_nodes) {
				fprintf (stderr, "%s: link_node_by_edges: serious -- companion node ain\'t here!\n", progname);
			}
		}
	}
	return;
}
/*}}}*/
/*{{{  static int constrain_and_check (graphnode *node)*/
/*
 *	applies constraints to registers (checks with neighbours to make sure it's OK to move in)
 */
static int constrain_and_check (graphnode *node)
{
	int i;
	ins_chain *move_ins;

	if (!node->constrain_ins) {
		fprintf (stderr, "%s: warning: non-constrained register %d in constrain_and_check\n", progname, node->vreg);
		return 0;
	}
	node->rreg = node->constrain_ins->in_args[1]->regconst;
	for (i=0; i<node->n_links; i++) {
		if (node->links[i]->rreg == node->rreg) {
			/* might not be a problem */
			move_ins = NULL;
			if (node->end_ins->prev && (node->end_ins->prev == node->links[i]->start_ins->next)) {
				move_ins = node->end_ins->prev;
			} else if (node->start_ins->next && (node->start_ins->next == node->links[i]->end_ins->prev)) {
				move_ins = node->start_ins->next;
			} else {
				fprintf (stderr, "%s: error: collision in constrained register %d with register %d\n", progname, node->vreg, node->links[i]->vreg);
				return -1;
			}
			/* will be left with a move which is ineffective */
		}
	}
#if 0
fprintf (stderr, "constrain_and_check(): constrained vreg %d to rreg %d\n", node->vreg, node->rreg);
#endif
	return 0;
}
/*}}}*/
/*{{{  static int select_register (graphnode *node, arch_t *arch)*/
/*
 *	finds a suitable register for a node
 */
static int select_register (graphnode *node, arch_t *arch)
{
	static int *r_names = NULL;
	static int *r_free = NULL;
	static int arch_rmax = 0;
	int i, j;

	if (!r_names) {
		arch_rmax = arch->regcolour_rmax;
		r_names = (int *)smalloc (arch_rmax * sizeof (int));
		r_free = (int *)smalloc (arch_rmax * sizeof (int));
		arch->regcolour_get_regs (r_names);
	}
	for (i=0; i<arch_rmax; i++) {
		r_free[i] = 1;
	}

	for (i=0; i<node->n_links; i++) {
		if (node->links[i]->rreg > -1) {
			for (j=0; (j<arch_rmax) && (node->links[i]->rreg != r_names[j]); j++);
			if (j < arch_rmax) {
				r_free[j] = 0;
			}
		}
	}
	for (i=0; i<arch_rmax; i++) {
		if (r_free[i]) {
			return r_names[i];
		}
	}
	return -1;
}
/*}}}*/
/*{{{  static int choose_if_split (graphnode **nodes, int n_nodes, int this_one, arch_t *arch)*/
/*
 *	chooses split register for colouring choice if possible (only called after select_register fails)
 */
static int choose_if_split (graphnode **nodes, int n_nodes, int this_one, arch_t *arch)
{
	ins_chain *istart, *iend;
	static int *r_names = NULL;
	static int *r_free = NULL;
	static int arch_rmax = 0;
	int i, j;
	int other_one;

	if (!r_names) {
		arch_rmax = arch->regcolour_rmax;
		r_names = (int *)smalloc (arch_rmax * sizeof (int));
		r_free = (int *)smalloc (arch_rmax * sizeof (int));
		arch->regcolour_get_regs (r_names);
	}
	for (i=0; i<arch_rmax; i++) {
		r_free[i] = 1;
	}

	iend = nodes[this_one]->end_ins;
	if (iend->prev && iend->prev->prev) {
		istart = iend->prev->prev;
		if (istart->type != INS_START_REG) {
			return -1;
		}
		for (other_one = 0; other_one < n_nodes; other_one++) {
			if (nodes[other_one]->vreg == istart->in_args[0]->regconst) {
				/* found it */
				break;
			}
		}
		if (other_one == n_nodes) {
			/* split not in register map! */
			return -1;
		}
		if ((istart->next->type != INS_MOVE) || !istart->next->in_args[0] || !istart->next->out_args[0]) {
			/* not move in the middle */
			return -1;
		}
		if ((istart->next->in_args[0]->regconst == nodes[this_one]->vreg) && (istart->next->out_args[0]->regconst == nodes[other_one]->vreg)) {
			/* looks good :-)   [debug]*/
			#if DEBUG_REGCOLOUR
				fprintf (stderr, "choose_if_split found previously split registers (%d -> %d)\n", nodes[this_one]->vreg, nodes[other_one]->vreg);
			#endif
		} else {
			return -1;
		}
		if (nodes[other_one]->constrain_ins) {
			#if DEBUG_REGCOLOUR
				fprintf (stderr, "choose_if_split found %d constrained to %d\n", nodes[other_one]->vreg, nodes[other_one]->rreg);
			#endif
		}
		for (i=0; i<nodes[this_one]->n_links; i++) {
			if ((nodes[this_one]->links[i]->rreg > -1) && (nodes[this_one]->links[i] != nodes[other_one])) {
				for (j=0; (j<arch_rmax) && (nodes[this_one]->links[i]->rreg != r_names[j]); j++);
				if (j < arch_rmax) {
					r_free[j] = 0;
				}
			}
		}
		for (i = 0; i<arch_rmax; i++) {
			if (r_free[i]) {
				return r_names[i];
			}
		}
	}
	return -1;
}
/*}}}*/
/*{{{  static int do_graph_colour (graphnode **nodes, int n_nodes, arch_t *arch)*/
/*
 *	finds a valid colouring in the graph
 */
static int do_graph_colour (graphnode **nodes, int n_nodes, arch_t *arch)
{
	int i, breg;

	for (i=0; i<n_nodes; i++) {
		if (nodes[i]->rreg > -1) {
			/* already coloured (from constraint) */
			continue;
		}
		breg = select_register (nodes[i], arch);
		if (breg < 0) {
			/* very rare case (as far as I've seen) usually fixup from excessive splitting. */
			breg = choose_if_split (nodes, n_nodes, i, arch);
			if (breg < 0) {
				fprintf (stderr, "%s: error: cannot colour register %d\n", progname, nodes[i]->vreg);
				#if DEBUG_REGCOLOUR
					dump_graph (nodes, n_nodes);
				#endif
				return -1;
			}
		}
		nodes[i]->rreg = breg;
	}
	return 0;
}
/*}}}*/
/*{{{  static int merge_multiple_constraints (ins_chain *code)*/
/*
 *	finds registers with more than 1 same consequetive constraint, and merges them
 *	will also split registers which might collide (this doesn't occur very often)
 */
static int merge_multiple_constraints (ins_chain *code)
{
	ins_chain *tmp;
	int reg, c_reg, tmp_reg;
	ins_chain *reg_start, *reg_end, *reg_first_c, *reg_last_uc, *reg_next_c;
	ins_chain *looking;

	for (tmp=code; tmp; tmp=tmp->next) {
		reg = -1;
		if (tmp->type == INS_START_REG) {
			reg_start = tmp;
			reg = reg_start->in_args[0]->regconst;
			reg_end = rtl_scan_end_forward (reg_start->next, reg);
			if (!reg_end) {
				fprintf (stderr, "%s: error: register %d does not have END\n", progname, reg);
				return -1;
			}
			reg_first_c = rtl_scan_constrain_forward (reg_start->next, reg);
			if (reg_first_c) {
				c_reg = reg_first_c->in_args[1]->regconst;
				/* search forward for next constraint */
				reg_next_c = reg_first_c->next;
				do {
					reg_next_c = rtl_scan_constrain_forward (reg_next_c, reg);
					if (reg_next_c) {
						looking = reg_next_c->next;
					} else {
						looking = NULL;
					}
					if (reg_next_c && (reg_next_c->in_args[1]->regconst == c_reg)) {
						/* same constraint */
						reg_last_uc = rtl_scan_unconstrain_backward (reg_next_c->prev, reg);
						if (!reg_last_uc) {
							fprintf (stderr, "%s: error: register %d has CONSTRAIN but no matching UNCONSTRAIN\n", progname, reg);
							return -1;
						}
						if (!rtl_scan_for_constrain_to (reg_last_uc, reg_next_c, c_reg)) {
							/* remove unconstrain/constrain (in-the-middle) pair */
							rtl_remove_instr (reg_next_c);
							rtl_remove_instr (reg_last_uc);
						}
					} else if (reg_next_c) {
						/* impending register collision */
						if (0 && options.verbose) {
							fprintf (stderr, "%s: message: potential conflict on register %d (constrained to %d and %d)\n", progname, reg, c_reg, reg_next_c->in_args[1]->regconst);
						}
						tmp_reg = rtl_get_newvreg ();
						if (0 && options.verbose) {
							fprintf (stderr, "%s: message: spilling register %d to register %d\n", progname, reg, tmp_reg);
						}
						rtl_insert_instr_before (compose_ins (INS_START_REG, 1, 0, ARG_REG, tmp_reg), reg_next_c);
						rtl_insert_instr_before (compose_ins (INS_MOVE, 1, 1, ARG_REG, reg, ARG_REG, tmp_reg), reg_next_c);
						rtl_insert_instr_before (compose_ins (INS_END_REG, 1, 0, ARG_REG, reg), reg_next_c);
						/* rename `reg' to `tmp_reg' */
						rtl_rename_reg (reg_next_c, reg, tmp_reg);
						reg = tmp_reg;
					}
					reg_next_c = looking;
				} while (reg_next_c);
			}
		}
	}
	return 0;
}
/*}}}*/
/*{{{  static int colour_code_block (ins_chain *code, arch_t *arch)*/
/*
 *	colours a chunk of code
 */
static int colour_code_block (ins_chain *code, arch_t *arch)
{
	int *low_edge_table;
	int *high_edge_table;
	int edge_cur, edge_max;
	int rdepth;
	ins_chain *first, *last, *tmp, *tmp_forw;
	int *active_regs, *constrain_regs;
	ins_chain **constrain_instrs, **unconstrain_instrs, **start_instrs;
	int i, j, tmp_reg;
	graphnode **nodes;
	graphnode *this_node;
	int n_nodes;
	int arch_rmax = arch->regcolour_rmax;
	int arch_nodemax = arch->regcolour_nodemax;

	nodes = (graphnode **)smalloc (arch_nodemax * sizeof (graphnode *));
	active_regs = (int *)smalloc (arch_rmax * sizeof (int));
	constrain_regs = (int *)smalloc (arch_rmax * sizeof (int));
	constrain_instrs = (ins_chain **)smalloc (arch_rmax * sizeof (ins_chain *));
	unconstrain_instrs = (ins_chain **)smalloc (arch_rmax * sizeof (ins_chain *));
	start_instrs = (ins_chain **)smalloc (arch_rmax * sizeof (ins_chain *));

restart:
	low_edge_table = high_edge_table = NULL;
	edge_cur = edge_max = 0;

	/* scan through code to wrap a block of register usage up */
	rdepth = 0;
	n_nodes = 0;
	for (i=0; i<arch_rmax; i++) {
		active_regs[i] = -1;		/* inactive */
		constrain_regs[i] = -1;		/* not constrained */
		constrain_instrs[i] = NULL;
		unconstrain_instrs[i] = NULL;
	}
	for (i=0; i<arch_nodemax; i++) {
		nodes[i] = NULL;
	}
	first = last = NULL;

	/* first, scan the code block to resolve any constraint conflicts */
	for (tmp=code; tmp; tmp=tmp->next) {
		switch (tmp->type) {
			/*{{{  INS_START_REG -- starting a register*/
		case INS_START_REG:
			for (i=0; (i<arch_rmax) && (active_regs[i] > -1); i++);
			if (i == arch_rmax) {
				fprintf (stderr, "%s: colour_code_block: too many active registers (arch_rmax = %d)\n", progname, arch_rmax);
				goto out_error;
			}
			active_regs[i] = tmp->in_args[0]->regconst;
			start_instrs[i] = tmp;
			/* search forward for a constraint */
			tmp_forw = rtl_scan_constrain_forward (tmp->next, active_regs[i]);
			if (tmp_forw) {
				constrain_regs[i] = tmp_forw->in_args[1]->regconst;
			}
			break;
			/*}}}*/
			/*{{{  INS_CONSTRAIN_REG -- constraining register*/
		case INS_CONSTRAIN_REG:
			for (i=0; (i<arch_rmax) && (active_regs[i] != tmp->in_args[0]->regconst); i++);
			if (i == arch_rmax) {
				fprintf (stderr, "%s: colour_code_block: INS_CONSTRAIN_REG encountered for unseen register %d\n", progname, tmp->in_args[0]->regconst);
				goto out_error;
			}
			/* are we going to collide with something already being constrained ? */
			for (j=0; j<arch_rmax; j++) {
				if ((i != j) && (constrain_regs[j] == tmp->in_args[1]->regconst)) {
					break;
				}
			}
			if (j < arch_rmax) {
				if (options.diagnostics) {
					fprintf (stderr, "constrained reg %d (to %d) conflicts with reg %d (to %d) -- constrain_instrs[j] = %p\n", active_regs[j],
						constrain_regs[j], active_regs[i], tmp->in_args[1]->regconst, constrain_instrs[j]);
				}
				if (!constrain_instrs[j]) {
					/*{{{  active_regs[i] constrains before active_regs[j] -- split active_regs[j] before constraint */
					last = rtl_scan_end_forward (tmp->next, active_regs[i]);
					/* tmp_forw = rtl_scan_constrain_forward (last->next, active_regs[j]); */
					tmp_forw = rtl_scan_constrain_forward (tmp->next, active_regs[j]);
					if (!tmp_forw) {
						fprintf (stderr, "%s: error: expected to see constraint on register %d, but didn\'t\n", progname, active_regs[j]);
						goto out_error;
					}
					tmp_reg = rtl_get_newvreg ();
					rtl_insert_instr_before (compose_ins (INS_START_REG, 1, 0, ARG_REG, tmp_reg), tmp_forw);
					rtl_insert_instr_before (compose_ins (INS_MOVE, 1, 1, ARG_REG, active_regs[j], ARG_REG, tmp_reg), tmp_forw);
					rtl_insert_instr_before (compose_ins (INS_END_REG, 1, 0, ARG_REG, active_regs[j]), tmp_forw);
					rtl_rename_reg (tmp_forw, active_regs[j], tmp_reg);
					constrain_regs[j] = -1;
					/*}}}*/
				} else if (!unconstrain_instrs[j]) {
					/*{{{  overlapping constraints -- error*/
					fprintf (stderr, "%s: error: overlapping constraints\n", progname);
					goto out_error;
					/*}}}*/
				} else {
					/*{{{  active_regs[j] constrains before active_regs[i] -- split active_regs[j] just before active_regs[i] starts*/
					tmp_forw = rtl_scan_start_forward (unconstrain_instrs[j], active_regs[i]);
					if (!tmp_forw) {
						tmp_reg = rtl_get_newvreg ();
						tmp_forw = rtl_scan_unconstrain_forward (constrain_instrs[j], active_regs[j]);
						if (!tmp_forw) {
							fprintf (stderr, "%s: error: unable to find unconstrain instruction for register %d\n", progname, active_regs[j]);
							goto out_error;
						}
						rtl_insert_instr_after (compose_ins (INS_END_REG, 1, 0, ARG_REG, active_regs[j]), tmp_forw);
						rtl_insert_instr_after (compose_ins (INS_MOVE, 1, 1, ARG_REG, active_regs[j], ARG_REG, tmp_reg), tmp_forw);
						rtl_insert_instr_after (compose_ins (INS_START_REG, 1, 0, ARG_REG, tmp_reg), tmp_forw);
						rtl_rename_reg (tmp_forw->next->next->next, active_regs[j], tmp_reg);
					} else {
						/* split active_regs[j] before `tmp_forw' (start of active_regs[i]) */
						tmp_reg = rtl_get_newvreg ();
						rtl_insert_instr_before (compose_ins (INS_START_REG, 1, 0, ARG_REG, tmp_reg), tmp_forw);
						rtl_insert_instr_before (compose_ins (INS_MOVE, 1, 1, ARG_REG, active_regs[j], ARG_REG, tmp_reg), tmp_forw);
						rtl_insert_instr_before (compose_ins (INS_END_REG, 1, 0, ARG_REG, active_regs[j]), tmp_forw);
						rtl_rename_reg (tmp_forw, active_regs[j], tmp_reg);
					}
					/*}}}*/
					goto restart;
				}
			}
			constrain_instrs[i] = tmp;
			break;
			/*}}}*/
			/*{{{  INS_UNCONSTRAIN_REG -- unconstraining*/
		case INS_UNCONSTRAIN_REG:
			for (i=0; (i<arch_rmax) && (active_regs[i] != tmp->in_args[0]->regconst); i++);
			if (i == arch_rmax) {
				fprintf (stderr, "%s: colour_code_block: INS_UNCONSTRAIN_REG encountered for unseen register %d\n", progname, tmp->in_args[0]->regconst);
				goto out_error;
			}
			unconstrain_instrs[i] = tmp;
			break;
			/*}}}*/
			/*{{{  INS_END_REG -- ending register*/
		case INS_END_REG:
			for (i=0; (i<arch_rmax) && (active_regs[i] != tmp->in_args[0]->regconst); i++);
			if (i == arch_rmax) {
				fprintf (stderr, "%s: colour_code_block: INS_END_REG encountered for unseen register %d\n", progname, tmp->in_args[0]->regconst);
				goto out_error;
			}
			if (constrain_regs[i] != -1) {
				/*{{{  check other active registers for a possible overlapping constraint*/
				for (j=0; j<arch_rmax; j++) {
					if ((j != i) && (constrain_regs[i] == constrain_regs[j])) {
						break;
					}
				}
				if (j < arch_rmax) {
					if (options.diagnostics) {
						fprintf (stderr, "%s: colour_code_block: INS_END_REG for %d constrained to %d;  overlaps with %d constrained to %d\n",
								progname, active_regs[i], constrain_regs[i], active_regs[j], constrain_regs[j]);
					}
					if (!constrain_instrs[j]) {
						tmp_forw = rtl_scan_constrain_forward (tmp->next, active_regs[j]);
						if (!tmp_forw) {
							fprintf (stderr, "%s: error: expected to see constraint on register %d, but didn\'t\n", progname, active_regs[j]);
							goto out_error;
						}
						tmp_reg = rtl_get_newvreg ();
						rtl_insert_instr_before (compose_ins (INS_START_REG, 1, 0, ARG_REG, tmp_reg), tmp_forw);
						rtl_insert_instr_before (compose_ins (INS_MOVE, 1, 1, ARG_REG, active_regs[j], ARG_REG, tmp_reg), tmp_forw);
						rtl_insert_instr_before (compose_ins (INS_END_REG, 1, 0, ARG_REG, active_regs[j]), tmp_forw);
						rtl_rename_reg (tmp_forw, active_regs[j], tmp_reg);
						constrain_regs[j] = -1;

						if (options.diagnostics) {
							fprintf (stderr, "%s: colour_code_block: INS_END_REG for %d;  overlapping active register %d not constrained yet, split into vreg %d\n",
									progname, active_regs[i], active_regs[j], tmp_reg);
						}

						/* safe to continue :) */
					}
				}
				/*}}}*/
			}
			active_regs[i] = -1;
			constrain_regs[i] = -1;
			constrain_instrs[i] = NULL;
			unconstrain_instrs[i] = NULL;
			break;
			/*}}}*/
		}
	}
	for (i=0; i<arch_rmax; i++) {
		active_regs[i] = -1;	/* inactive */
		constrain_regs[i] = -1;	/* not constrained */
		constrain_instrs[i] = NULL;
		unconstrain_instrs[i] = NULL;
	}
	first = last = NULL;

	for (tmp=code; tmp; tmp=tmp->next) {
		switch (tmp->type) {
			/*{{{  INS_START_REG -- starting register*/
		case INS_START_REG:
			for (i=0; (i<arch_rmax) && (active_regs[i] > -1); i++);
			if (i == arch_rmax) {
				fprintf (stderr, "%s: colour_code_block: too many active registers (arch_rmax = %d)\n", progname, arch_rmax);
				goto out_error;
			}
			active_regs[i] = tmp->in_args[0]->regconst;
			if (!rdepth++) {
				first = tmp;
			}
			set_edges_in_table (active_regs, arch_rmax, &low_edge_table, &high_edge_table, &edge_cur, &edge_max);
			if (n_nodes == arch_nodemax) {
				fprintf (stderr, "%s: colour_code_block: too many nodes in graph (arch_nodemax = %d)\n", progname, arch_nodemax);
				goto out_error;
			}
			nodes[n_nodes++] = new_graphnode (active_regs[i], tmp);
			#if DEBUG_REGCOLOUR
				fprintf (stderr, "INS_START_REG (%d): rdepth = %d (first = %p, last = %p)\n", active_regs[i], rdepth, first, last);
			#endif
			break;
			/*}}}*/
			/*{{{  INS_CONSTRAIN_REG -- constraining register*/
		case INS_CONSTRAIN_REG:
			for (i=0; i<n_nodes; i++) {
				if (nodes[i]->vreg == tmp->in_args[0]->regconst) {
					break;
				}
			}
			if (i == n_nodes) {
				fprintf (stderr, "%s: colour_code_block: INS_CONSTRAIN_REG encountered for unseen register %d\n", progname, tmp->in_args[0]->regconst);
				goto out_error;
			} else {
				this_node = nodes[i];
			}
			if (this_node->constrain_ins && (this_node->constrain_ins->in_args[0]->regconst != tmp->in_args[0]->regconst)) {
				fprintf (stderr, "%s: colour_code_block: conflicting constraints on register %d\n", progname, this_node->vreg);
				goto out_error;
			} else {
				this_node->constrain_ins = tmp;
			}
			break;
			/*}}}*/
			/*{{{  INS_END_REG -- ending register*/
		case INS_END_REG:
			/* mark end instruction in node list */
			for (i=0; i<n_nodes; i++) {
				if (nodes[i]->vreg == tmp->in_args[0]->regconst) {
					break;
				}
			}
			if (i == n_nodes) {
				fprintf (stderr, "%s: colour_code_block: INS_END_REG encountered for unseen register %d\n", progname, tmp->in_args[0]->regconst);
				goto out_error;
			} else {
				this_node = nodes[i];
			}
			this_node->end_ins = tmp;
			/* mark inactive */
			for (i=0; (i<arch_rmax) && (active_regs[i] != tmp->in_args[0]->regconst); i++);
			if (i == arch_rmax) {
				fprintf (stderr, "%s: colour_code_block: INS_END_REG encountered for inactive register %d\n", progname, tmp->in_args[0]->regconst);
				goto out_error;
			}
			active_regs[i] = -1;
			if (!--rdepth) {
				last = tmp;
			}
			#if DEBUG_REGCOLOUR
				fprintf (stderr, "INS_END_REG (%d): rdepth = %d (first = %p, last = %p)\n", tmp->in_args[0]->regconst, rdepth, first, last);
			#endif
			break;
			/*}}}*/
		}
		if (!rdepth && first && last && n_nodes) {
			/* have something suitable for colouring */
			/*{{{  build links between nodes first*/
			for (i=0; i<n_nodes; i++) {
				nodes[i]->n_links = count_edges (low_edge_table, high_edge_table, edge_cur, nodes[i]->vreg);
				if (nodes[i]->n_links) {
					nodes[i]->links = (graphnode **)smalloc (nodes[i]->n_links * sizeof (graphnode *));
					link_node_by_edges (nodes[i], nodes, n_nodes, low_edge_table, high_edge_table, edge_cur);
				}
			}
			/*}}}*/
			/*{{{  apply any constraints and check*/
			for (i=0; i<n_nodes; i++) {
				if (nodes[i]->constrain_ins) {
					if (constrain_and_check (nodes[i]) < 0) {
						goto out_error;
					}
				}
			}
			/*}}}*/
			/*{{{  find a valid colouring*/
			if (do_graph_colour (nodes, n_nodes, arch) < 0) {
				goto out_error;
			}
			/*}}}*/
			/*{{{  colour them in*/
			for (i=0; i<n_nodes; i++) {
				/* store real-register allocated in start node */
				nodes[i]->start_ins->out_args[0] = new_ins_arg ();
				nodes[i]->start_ins->out_args[0]->flags = ARG_REG;
				nodes[i]->start_ins->out_args[0]->regconst = nodes[i]->rreg;
				colour_code_fragment (nodes[i]->start_ins->next, nodes[i]->end_ins->prev, nodes[i]->vreg, nodes[i]->rreg);
			}
			/*}}}*/
			/*{{{  free things*/
			#if DEBUG_REGCOLOUR
				dump_edge_table (low_edge_table, high_edge_table, edge_cur);
				dump_graph (nodes, n_nodes);
			#endif
			edge_cur = 0;
			first = last = NULL;
			for (i=0; i<n_nodes; i++) {
				free_graphnode (nodes[i]);
			}
			n_nodes = 0;
			/*}}}*/
		}
	}
	if (low_edge_table) {
		sfree (low_edge_table);
	}
	if (high_edge_table) {
		sfree (high_edge_table);
	}
	/* free up memory */
	sfree (start_instrs);
	sfree (unconstrain_instrs);
	sfree (constrain_instrs);
	sfree (constrain_regs);
	sfree (active_regs);
	sfree (nodes);
	return 0;
out_error:
	/* free up memory */
	sfree (start_instrs);
	sfree (unconstrain_instrs);
	sfree (constrain_instrs);
	sfree (constrain_regs);
	sfree (active_regs);
	sfree (nodes);
	return -1;
}
/*}}}*/
#if DEBUG_REGCOLOUR
/*{{{  static void dump_edge_table (int *letab, int *hetab, int n_edges)*/
/*
 *	dumps the edge table (for debugging purposes)
 */
static void dump_edge_table (int *letab, int *hetab, int n_edges)
{
	int i;

	fprintf (stderr, "BEGIN edge table dump:\n");
	for (i=0; i<n_edges; i++) {
		fprintf (stderr, "\t%d\t%d\n", letab[i], hetab[i]);
	}
	return;
}
/*}}}*/
/*{{{  static void dump_graph (graphnode **nodes, int n_nodes)*/
/*
 *	dumps the graph (for debugging purposes)
 */
static void dump_graph (graphnode **nodes, int n_nodes)
{
	int i, j;

	fprintf (stderr, "BEGIN graph node dump:\n");
	fprintf (stderr, "\tnode #\tvreg\trreg\tn_links\tstart-ins\tconstrain-ins\tend-ins\n");
	for (i=0; i<n_nodes; i++) {
		fprintf (stderr, "\t%d\t%d\t%d\t%d\t0x%8.8x\t0x%8.8x\t0x%8.8x  [", i, nodes[i]->vreg, nodes[i]->rreg, nodes[i]->n_links,
			(int)nodes[i]->start_ins, (int)nodes[i]->constrain_ins, (int)nodes[i]->end_ins);
		for (j=0; j < (nodes[i]->n_links - 1); j++) {
			fprintf (stderr, "%d,", nodes[i]->links[j]->vreg);
		}
		if (j < nodes[i]->n_links) {
			fprintf (stderr, "%d", nodes[i]->links[j]->vreg);
		}
		fprintf (stderr, "]\n");
	}
	return;
}
/*}}}*/
#endif
/*{{{  static int fix_special_regs (ins_chain *ins, arch_t *arch)*/
/*
 *	fixes pre-set registers
 */
static int fix_special_regs (ins_chain *ins, arch_t *arch)
{
	ins_chain *tmp;
	ins_arg *arg;
	ins_sib_arg *t_sib;
	int i;

	for (tmp=ins; tmp; tmp=tmp->next) {
		for (i=0; tmp->in_args[i]; i++) {
			arg = tmp->in_args[i];
			switch (arg->flags & ARG_MODEMASK) {
			case ARG_REG:
			case ARG_REGIND:
				arg->regconst = arch->regcolour_special_to_real (arg->regconst);
				break;
			case ARG_REGINDSIB:
				t_sib = (ins_sib_arg *)arg->regconst;
				t_sib->base = arch->regcolour_special_to_real (t_sib->base);
				t_sib->index = arch->regcolour_special_to_real (t_sib->index);
				break;
			default:
				break;
			}
		}
		for (i=0; tmp->out_args[i]; i++) {
			arg = tmp->out_args[i];
			switch (arg->flags & ARG_MODEMASK) {
			case ARG_REG:
			case ARG_REGIND:
				arg->regconst = arch->regcolour_special_to_real (arg->regconst);
				break;
			case ARG_REGINDSIB:
				t_sib = (ins_sib_arg *)arg->regconst;
				t_sib->base = arch->regcolour_special_to_real (t_sib->base);
				t_sib->index = arch->regcolour_special_to_real (t_sib->index);
				break;
			default:
				break;
			}
		}
	}
	return 0;
}
/*}}}*/
/*{{{  int colour_registers (rtl_chain *rtl_code, arch_t *arch)*/
/*
 *	drives the register colouring
 */
int colour_registers (rtl_chain *rtl_code, arch_t *arch)
{
	rtl_chain *tmp_rtl;

	for (tmp_rtl = rtl_code; tmp_rtl; tmp_rtl = tmp_rtl->next) {
		if (tmp_rtl->type == RTL_CODE) {
			if (merge_multiple_constraints (tmp_rtl->u.code.head)) {
				return -1;
			}
			if (colour_code_block (tmp_rtl->u.code.head, arch)) {
				return -1;
			}
			if (fix_special_regs (tmp_rtl->u.code.head, arch)) {
				return -1;
			}
		}
	}
	return 0;
}
/*}}}*/


