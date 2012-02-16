/*
 *	rtlops.c - RTL operations (basic sortings out of the RTL/instruction chain)
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

#include <string.h>
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
#include "etcrtl.h"
#include "rtlops.h"

#ifndef EXIT_FAILURE
	#define EXIT_FAILURE 1
#endif	/* !EXIT_FAILURE */
/*}}}*/

/* this is set in main.c, and used if any registers are needed after translation */
static int last_virtual_register;

/*{{{  rtl_chain *rtl_compact_datablocks (rtl_chain *rtl_code)*/
/*
 *	collects together consecutive data-blocks
 */
rtl_chain *rtl_compact_datablocks (rtl_chain *rtl_code)
{
	rtl_chain *holding, *head, *tmp, *xtmp;
	int size, i;

	head = rtl_code;
	while (rtl_code) {
		if (rtl_code->type != RTL_DATA) {
			rtl_code = rtl_code->next;
			continue;
		}
		holding = rtl_code;
		while (rtl_code->next && (rtl_code->next->type == RTL_DATA)) {
			rtl_code = rtl_code->next;
		}
		/* holding -> rtl_code defines the blocks to concatanate */
		if (holding == rtl_code) {
			rtl_code = rtl_code->next;
			continue;
		}
		size = 0;
		for (tmp = holding; tmp != rtl_code->next; tmp = tmp->next) {
			size += tmp->u.data.length;
		}
		/* recycle old block :-) */
		i = holding->u.data.length;
		holding->u.data.bytes = (char *)srealloc (holding->u.data.bytes, i, size);
		holding->u.data.length = size;
		for (tmp = holding->next; tmp != rtl_code->next; tmp = tmp->next) {
			memcpy (holding->u.data.bytes + i, tmp->u.data.bytes, tmp->u.data.length);
			i += tmp->u.data.length;
		}
		/* drop dead blocks */
		tmp = holding->next;
		holding->next = rtl_code->next;
		rtl_code->next = NULL;
		if (holding->next) {
			holding->next->prev = holding;
		}
		while (tmp) {
			free (tmp->u.data.bytes);
			xtmp = tmp->next;
			free (tmp);
			tmp = xtmp;
		}
		rtl_code = holding->next;
	}
	return head;
}
/*}}}*/
/*{{{  rtl_chain *rtl_compact_codeblocks (rtl_chain *rtl_code)*/
/*
 *	collects together consecutive code-blocks
 */
rtl_chain *rtl_compact_codeblocks (rtl_chain *rtl_code)
{
	rtl_chain *holding, *head, *tmp, *xtmp;

	head = rtl_code;
	while (rtl_code) {
		if (rtl_code->type != RTL_CODE) {
			rtl_code = rtl_code->next;
			continue;
		}
		holding = rtl_code;
		while (rtl_code->next && (rtl_code->next->type == RTL_CODE)) {
			rtl_code = rtl_code->next;
		}
		/* holding -> rtl_code defines the blocks to concatanate */
		if (holding == rtl_code) {
			rtl_code = rtl_code->next;
			continue;
		}
		/* recycle old block :) */
		for (tmp = holding->next; tmp != rtl_code->next; tmp = tmp->next) {
			if (!tmp->u.code.head || !tmp->u.code.tail) {
				fprintf (stderr, "%s: warning: ignoring NULL code-block.\n", progname);
				continue;
			}
			holding->u.code.tail->next = tmp->u.code.head;
			tmp->u.code.head->prev = holding->u.code.tail;
			holding->u.code.tail = tmp->u.code.tail;
		}
		/* drop dead blocks */
		tmp = holding->next;
		holding->next = rtl_code->next;
		rtl_code->next = NULL;
		if (holding->next) {
			holding->next->prev = holding;
		}
		while (tmp) {
			/* ought to clean up ins chain really... */
			xtmp = tmp->next;
			free (tmp);
			tmp = xtmp;
		}
		rtl_code = holding->next;
	}
	return head;
}
/*}}}*/
/*{{{  rtl_chain *rtl_relocate_data (rtl_chain *rtl_code)*/
/*
 *	pushes possibily re-locatable data-blocks out of the way
 */
rtl_chain *rtl_relocate_data (rtl_chain *rtl_code)
{
	rtl_chain *head, *tmp, *last_rtl, *o_last_rtl;
	ins_chain *tins;
	char *tc;
	int ti;

	head = rtl_code;
	last_rtl = NULL;
	while (rtl_code) {
		if ((rtl_code->type != RTL_DATA) && (rtl_code->type != RTL_XDATA)) {
			last_rtl = rtl_code;
			rtl_code = rtl_code->next;
			continue;
		}
		/* at a DATA or XDATA block */
		if ((rtl_code->type == RTL_XDATA) && (rtl_code->u.xdata.label >= 0)) {
			/* already got label, ignore this one */
			last_rtl = rtl_code;
			rtl_code = rtl_code->next;
			continue;
		}

		tmp = rtl_code->prev;
		if (tmp && (tmp->type == RTL_CODE)) {
			tins = tmp->u.code.tail;
			if ((tins->type == INS_SETLABEL) && ((tins->in_args[0]->flags & ARG_MODEMASK) == ARG_LABEL)) {
				/* mark as re-locatable */
				if (rtl_code->type == RTL_XDATA) {
					rtl_code->u.xdata.label = tins->in_args[0]->regconst;
				} else {
					tc = rtl_code->u.data.bytes;
					ti = rtl_code->u.data.length;
					rtl_code->type = RTL_RDATA;
					rtl_code->u.rdata.bytes = tc;
					rtl_code->u.rdata.length = ti;
					rtl_code->u.rdata.label = tins->in_args[0]->regconst;
				}
				/* remove SETLABEL from previous */
				if (tmp->u.code.head == tmp->u.code.tail) {
					tmp->u.code.head = tmp->u.code.tail = NULL;
				} else {
					tmp->u.code.tail->prev->next = NULL;
					tmp->u.code.tail = tmp->u.code.tail->prev;
				}
			}
		}
		last_rtl = rtl_code;
		rtl_code = rtl_code->next;
	}
	/* run through again, and move RDATA/labelled XDATA/DYNCODEENTRY to the back */
	rtl_code = head;
	/* add an alignment to the back before moving data */
	tmp = new_rtl ();
	tmp->type = RTL_ALIGN;
	tmp->u.alignment = 2;
	tmp->prev = last_rtl;
	tmp->next = NULL;
	last_rtl->next = tmp;
	last_rtl = tmp;
	o_last_rtl = last_rtl;
	while (rtl_code && (rtl_code != o_last_rtl)) {
		if ((rtl_code->type == RTL_RDATA) || ((rtl_code->type == RTL_XDATA) && (rtl_code->u.xdata.label >= 0)) ||
				(rtl_code->type == RTL_DYNCODEENTRY)) {
			tmp = rtl_code;
			rtl_code = rtl_code->next;
			/* cut tmp from the list */
			if (!tmp->prev) {
				head = rtl_code;
			} else {
				tmp->prev->next = rtl_code;
			}
			if (rtl_code) {
				rtl_code->prev = tmp->prev;
			}
			/* add at back */
			last_rtl->next = tmp;
			tmp->prev = last_rtl;
			tmp->next = NULL;
			last_rtl = tmp;
		} else {
			rtl_code = rtl_code->next;
		}
	}
	return head;
}
/*}}}*/
/*{{{  void rtl_free_arg (ins_arg *arg)*/
/*
 *	frees an argument to an instruction
 */
void rtl_free_arg (ins_arg *arg)
{
	ins_labrefs *tmp_refs;

	switch (arg->flags & ARG_MODEMASK) {
	case ARG_NAMEDLABEL:
	case ARG_TEXT:
	case ARG_REGINDSIB:
		sfree ((void *)arg->regconst);
		break;
	case ARG_LABREFS:
		tmp_refs = ArgLabRefs (arg);
		if (tmp_refs) {
			if (tmp_refs->refs) {
				sfree (tmp_refs->refs);
			}
			sfree (tmp_refs);
		}
		break;
	}
	sfree (arg);
	return;
}
/*}}}*/
/*{{{  void rtl_free_instr (ins_chain *ins)*/
/*
 *	frees an instruction structure, and its arguments
 */
void rtl_free_instr (ins_chain *ins)
{
	int i;

	for (i=0; ins->in_args[i]; i++) {
		rtl_free_arg (ins->in_args[i]);
		ins->in_args[i] = NULL;
	}
	for (i=0; ins->out_args[i]; i++) {
		rtl_free_arg (ins->out_args[i]);
		ins->out_args[i] = NULL;
	}
	sfree (ins);
	return;
}
/*}}}*/
/*{{{  void rtl_insert_instr_before (ins_chain *newins, ins_chain *before)*/
/*
 *	inserts instruction before another one
 */
void rtl_insert_instr_before (ins_chain *newins, ins_chain *before)
{
	rtl_chain *rtl_block;

	rtl_block = before->rtl;
	if (!rtl_block) {
		fprintf (stderr, "%s: error: rtl_insert_instr_before: instruction does not belong to RTL block!\n", progname);
		#ifdef INSTRUCTION_HISTORY
			fprintf (stderr, "***: instruction allocated at %s:%ld\n", before->alloc_file, before->alloc_line);
		#endif
		return;
	}
	newins->rtl = rtl_block;
	if (!before->prev) {
		newins->next = before;
		newins->prev = NULL;
		before->prev = newins;
		rtl_block->u.code.head = newins;
	} else {
		newins->prev = before->prev;
		newins->prev->next = newins;
		newins->next = before;
		before->prev = newins;
	}
	return;
}
/*}}}*/
/*{{{  void rtl_insert_instr_after (ins_chain *newins, ins_chain *after)*/
/*
 *	inserts instruction after another one
 */
void rtl_insert_instr_after (ins_chain *newins, ins_chain *after)
{
	rtl_chain *rtl_block;

	rtl_block = after->rtl;
	if (!rtl_block) {
		fprintf (stderr, "%s: error: rtl_insert_instr_after: instruction does not belong to RTL block!\n", progname);
		return;
	}
	newins->rtl = rtl_block;
	if (!after->next) {
		newins->next = NULL;
		newins->prev = after;
		after->next = newins;
		rtl_block->u.code.tail = newins;
	} else {
		newins->next = after->next;
		newins->next->prev = newins;
		after->next = newins;
		newins->prev = after;
	}
	return;
}
/*}}}*/
/*{{{  void rtl_insert_instr_block_after (ins_chain *newchain, ins_chain *after)*/
/*
 *	inserts block of instructions after another one
 */
void rtl_insert_instr_block_after (ins_chain *newchain, ins_chain *after)
{
	rtl_chain *rtl_block;
	ins_chain *tmp;

	rtl_block = after->rtl;
	if (!rtl_block) {
		fprintf (stderr, "%s: error: rtl_insert_instr_block_after: instruction does not belong to RTL block!\n", progname);
		return;
	}
	for (tmp = newchain; tmp->next; tmp = tmp->next) {
		tmp->rtl = rtl_block;
	}
	tmp->rtl = rtl_block;
	/* tmp now points at the last instruction in the block */

	if (!after->next) {
		tmp->next = NULL;
		newchain->prev = after;
		after->next = newchain;
		rtl_block->u.code.tail = tmp;
	} else {
		tmp->next = after->next;
		tmp->next->prev = tmp;
		after->next = newchain;
		newchain->prev = after;
	}
	return;
}
/*}}}*/
/*{{{  void rtl_remove_instr (ins_chain *instr)*/
/*
 *	removes an instruction
 */
void rtl_remove_instr (ins_chain *instr)
{
	rtl_unlink_instr (instr);
	rtl_free_instr (instr);
	return;
}
/*}}}*/
/*{{{  void rtl_unlink_instr (ins_chain *instr)*/
/*
 *	removes an instruction, but doesn't free it
 */
void rtl_unlink_instr (ins_chain *instr)
{
	rtl_chain *rtl_block;

	rtl_block = instr->rtl;
	if (!rtl_block) {
		fprintf (stderr, "%s: error: rtl_unlink_instr: instruction does not belong to RTL block!\n", progname);
		return;
	}
	if (instr->prev && instr->next) {
		instr->prev->next = instr->next;
		instr->next->prev = instr->prev;
	} else if (instr->prev) {
		instr->prev->next = NULL;
		rtl_block->u.code.tail = instr->prev;
	} else if (instr->next) {
		instr->next->prev = NULL;
		rtl_block->u.code.head = instr->next;
	} else {
		rtl_block->u.code.head = NULL;
		rtl_block->u.code.tail = NULL;
	}
	return;
}
/*}}}*/
/*{{{  int rtl_trace_regs (rtl_chain *rtl_code)*/
/*
 *	generates INS_START_REG/INS_END_REG instructions
 */
int rtl_trace_regs (rtl_chain *rtl_code)
{
	ins_chain *tmp_ins;
	rtl_chain *tmp_rtl;
	ins_chain **reg_starts, **reg_ends;
	int i, last_reg;
	ins_arg *arg;
	ins_sib_arg *sib_arg;

	last_reg = 0;

	/* find highest reg */
	for (tmp_rtl = rtl_code; tmp_rtl; tmp_rtl=tmp_rtl->next) {
		if (tmp_rtl->type == RTL_CODE) {
			for (tmp_ins = tmp_rtl->u.code.head; tmp_ins; tmp_ins = tmp_ins->next) {
				for (i=0; tmp_ins->in_args[i]; i++) {
					arg = tmp_ins->in_args[i];
					switch (arg->flags & ARG_MODEMASK) {
					case ARG_REG:
					case ARG_REGIND:
						if (arg->regconst > last_reg) {
							last_reg = arg->regconst;
						}
						break;
					case ARG_REGINDSIB:
						sib_arg = (ins_sib_arg *)arg->regconst;
						if (sib_arg->base > last_reg) {
							last_reg = sib_arg->base;
						}
						if (sib_arg->index > last_reg) {
							last_reg = sib_arg->index;
						}
						break;
					}
				}
				for (i=0; tmp_ins->out_args[i]; i++) {
					arg = tmp_ins->out_args[i];
					switch (arg->flags & ARG_MODEMASK) {
					case ARG_REG:
					case ARG_REGIND:
						if (arg->regconst > last_reg) {
							last_reg = arg->regconst;
						}
						break;
					case ARG_REGINDSIB:
						sib_arg = (ins_sib_arg *)arg->regconst;
						if (sib_arg->base > last_reg) {
							last_reg = sib_arg->base;
						}
						if (sib_arg->index > last_reg) {
							last_reg = sib_arg->index;
						}
						break;
					}
				}
			}
		}
	}

	reg_starts = (ins_chain **)smalloc ((last_reg + 1) * sizeof (ins_chain *));
	reg_ends = (ins_chain **)smalloc ((last_reg + 1) * sizeof (ins_chain *));
	for (i=0; i<=last_reg; i++) {
		reg_starts[i] = NULL;
		reg_ends[i] = NULL;
	}

	/* scan */
	for (tmp_rtl = rtl_code; tmp_rtl; tmp_rtl=tmp_rtl->next) {
		if (tmp_rtl->type == RTL_CODE) {
			for (tmp_ins = tmp_rtl->u.code.head; tmp_ins; tmp_ins = tmp_ins->next) {
				for (i=0; tmp_ins->in_args[i]; i++) {
					arg = tmp_ins->in_args[i];
					switch (arg->flags & ARG_MODEMASK) {
					case ARG_REG:
					case ARG_REGIND:
						if (arg->regconst < 0) {
							break;
						}
						if (!reg_starts[arg->regconst]) {
							reg_starts[arg->regconst] = tmp_ins;
						}
						reg_ends[arg->regconst] = tmp_ins;
						break;
					case ARG_REGINDSIB:
						sib_arg = (ins_sib_arg *)arg->regconst;
						if ((sib_arg->base >= 0) && !reg_starts[sib_arg->base]) {
							reg_starts[sib_arg->base] = tmp_ins;
						}
						if ((sib_arg->index >= 0) && !reg_starts[sib_arg->index]) {
							reg_starts[sib_arg->index] = tmp_ins;
						}
						if (sib_arg->base >= 0) {
							reg_ends[sib_arg->base] = tmp_ins;
						}
						if (sib_arg->index >= 0) {
							reg_ends[sib_arg->index] = tmp_ins;
						}
						break;
					}
				}
				for (i=0; tmp_ins->out_args[i]; i++) {
					arg = tmp_ins->out_args[i];
					switch (arg->flags & ARG_MODEMASK) {
					case ARG_REG:
					case ARG_REGIND:
						if (arg->regconst < 0) {
							break;
						}
						if (!reg_starts[arg->regconst]) {
							reg_starts[arg->regconst] = tmp_ins;
						}
						reg_ends[arg->regconst] = tmp_ins;
						break;
					case ARG_REGINDSIB:
						sib_arg = (ins_sib_arg *)arg->regconst;
						if ((sib_arg->base >= 0) && !reg_starts[sib_arg->base]) {
							reg_starts[sib_arg->base] = tmp_ins;
						}
						if ((sib_arg->index >= 0) && !reg_starts[sib_arg->index]) {
							reg_starts[sib_arg->index] = tmp_ins;
						}
						if (sib_arg->base >= 0) {
							reg_ends[sib_arg->base] = tmp_ins;
						}
						if (sib_arg->index >= 0) {
							reg_ends[sib_arg->index] = tmp_ins;
						}
						break;
					}
				}
			}
		}
	}

	/* drop in starts/ends on registers (ignore before FIRST_VIRTUAL_REG -- special) */
	for (i=FIRST_VIRTUAL_REG; i<=last_reg; i++) {
		if (reg_starts[i] && reg_ends[i]) {
			/* trace this small chunk for sanity */
			for (tmp_ins = reg_starts[i]; tmp_ins && tmp_ins != reg_ends[i]; tmp_ins = tmp_ins->next);
			if (!tmp_ins) {
				fprintf (stderr, "%s: error: register %d is muddled (end does not follow start)\n", progname, i);
				return -1;
			}
			tmp_ins = compose_ins (INS_START_REG, 1, 0, ARG_REG, i);
			tmp_ins->rtl = reg_starts[i]->rtl;
			rtl_insert_instr_before (tmp_ins, reg_starts[i]);
			tmp_ins = compose_ins (INS_END_REG, 1, 0, ARG_REG, i);
			tmp_ins->rtl = reg_ends[i]->rtl;
			rtl_insert_instr_after (tmp_ins, reg_ends[i]);
		} else if (reg_ends[i]) {
			fprintf (stderr, "%s: error: register %d has end but no start\n", progname, i);
			return -1;
		} else if (reg_starts[i]) {
			fprintf (stderr, "%s: error: register %d has start but no end\n", progname, i);
			return -1;
		}
	}

	sfree (reg_starts);
	sfree (reg_ends);

	return last_reg;
}
/*}}}*/
/*{{{  void rtl_refix_codeblocks (rtl_chain *rtl_code)*/
/*
 *	fixes code links to RTL after fiddling
 */
void rtl_refix_codeblocks (rtl_chain *rtl_code)
{
	rtl_chain *trtl;
	ins_chain *ins;

	for (trtl=rtl_code; trtl; trtl=trtl->next) {
		if (trtl->type == RTL_CODE) {
			for (ins=trtl->u.code.head; ins; ins=ins->next) {
				ins->rtl = trtl;
			}
		}
	}
	return;
}
/*}}}*/
/*{{{  int rtl_check_consistency (rtl_chain *rtl_code)*/
/*
 *	checks that an RTL chain is consistent
 */
int rtl_check_consistency (rtl_chain *rtl_code)
{
	int bi, err;
	ins_chain *tins;
	int i;
	rtl_chain *srtl, *o_srtl;
	ins_chain *sins, *o_sins;

	bi = -1;
	err = 0;
	i = 0;
	for (o_srtl = srtl = rtl_code; srtl; o_srtl = srtl, srtl = srtl->next) {
		i++;
	}
	for (srtl = o_srtl; srtl; srtl = srtl->prev) {
		i--;
	}
	if (i) {
		fprintf (stderr, "%s: error: RTL chain has imbalance (%d)\n", progname, i);
		err = 1;
	}
	while (rtl_code) {
		bi++;
		switch (rtl_code->type) {
		case RTL_UNDEFINED:
		default:
			fprintf (stderr, "%s: error: undefined RTL block (type %d) at index %d\n", progname, rtl_code->type, bi);
			err = 1;
			break;
		case RTL_SOURCEFILE:
			if (!rtl_code->u.sourcefile) {
				fprintf (stderr, "%s: error: NULL source-file block at index %d\n", progname, bi);
				err = 1;
			}
			break;
		case RTL_DATA:
			if (!rtl_code->u.data.bytes || !rtl_code->u.data.length) {
				fprintf (stderr, "%s: error: empty RTL data block at index %d\n", progname, bi);
				err = 1;
			}
			break;
		case RTL_RDATA:
			if (!rtl_code->u.rdata.bytes || !rtl_code->u.rdata.length) {
				fprintf (stderr, "%s: error: empty RTL rdata block at index %d\n", progname, bi);
				err = 1;
			}
			break;
		case RTL_CODE:
			i = 0;
			for (o_sins = sins = rtl_code->u.code.head; sins; o_sins = sins, sins = sins->next) {
				i++;
			}
			for (sins = o_sins; sins; sins = sins->prev) {
				i--;
			}
			if (i) {
				fprintf (stderr, "%s: error: code block has imbalance %d at index %d\n", progname, i, bi);
				err = 1;
			}
			if (!rtl_code->u.code.head || !rtl_code->u.code.tail) {
				fprintf (stderr, "%s: error: empty RTL code block at index %d\n", progname, bi);
				err = 1;
			} else {
				for (tins=rtl_code->u.code.head; tins != rtl_code->u.code.tail; tins = tins->next) {
					if (tins->prev && (tins->prev->next != tins)) {
						fprintf (stderr, "%s: error: broken RTL code chain block at index %d\n", progname, bi);
						err = 1;
					}
					if (tins->next && (tins->next->prev != tins)) {
						fprintf (stderr, "%s: error: broken RTL code chain block at index %d\n", progname, bi);
						err = 1;
					}
					if (tins->rtl != rtl_code) {
						if (!tins->rtl) {
							tins->rtl = rtl_code;
						} else {
							fprintf (stderr, "%s: error: broken RTL code chain block at index %d (rtl=%p, expecting %p)\n", progname, bi, tins->rtl, rtl_code);
							/* quick fix-it anyway */
							tins->rtl = rtl_code;
							/* err = 1; */
						}
					}
				}
				if (rtl_code->u.code.tail->next) {
					fprintf (stderr, "%s: error: broken RTL code (tail) block at index %d\n", progname, bi);
					err = 1;
				}
				if (rtl_code->u.code.head->prev) {
					fprintf (stderr, "%s: error: broken RTL code (head) block at index %d\n", progname, bi);
					err = 1;
				}
			}
			break;
		case RTL_SETNAMEDLABEL:
			if (!rtl_code->u.label_name) {
				fprintf (stderr, "%s: error: NULL set-named-label block at index %d\n", progname, bi);
				err = 1;
			}
			break;
		case RTL_STUBIMPORT:
			if (!rtl_code->u.label_name) {
				fprintf (stderr, "%s: error: NULL stub-import block at index %d\n", progname, bi);
				err = 1;
			}
			break;
		case RTL_PUBLICSETNAMEDLABEL:
			if (!rtl_code->u.label_name) {
				fprintf (stderr, "%s: error: NULL public-set-named-label block at index %d\n", progname, bi);
				err = 1;
			}
			break;
		case RTL_ALIGN:
		case RTL_COMMENT:
		case RTL_MESSAGE:
		case RTL_WSVS:
		case RTL_DYNCODEENTRY:
			break;
		}
		if (rtl_code->prev && (rtl_code->prev->next != rtl_code)) {
			fprintf (stderr, "%s: error: broken RTL chain block at index %d\n", progname, bi);
			err = 1;
		}
		if (rtl_code->next && (rtl_code->next->prev != rtl_code)) {
			fprintf (stderr, "%s: error: broken RTL chain block at index %d\n", progname, bi);
			err = 1;
		}
		rtl_code = rtl_code->next;
	}
	if (err) {
		return -1;
	}
	return 0;
}
/*}}}*/
/*{{{  rtl_chain *rtl_remove_empty (rtl_chain *rtl_code)*/
/*
 *	removes empty code blocks
 */
rtl_chain *rtl_remove_empty (rtl_chain *rtl_code)
{
	rtl_chain *head, *tmp;
	int bi;

	head = rtl_code;
	bi = -1;
	while (rtl_code) {
		bi++;
		if (rtl_code->type != RTL_CODE) {
			rtl_code = rtl_code->next;
			continue;
		}
		if (!rtl_code->u.code.head && !rtl_code->u.code.tail) {
			/* remove this */
			tmp = rtl_code->next;
			if (!rtl_code->prev) {
				head = rtl_code->next;
			} else {
				rtl_code->prev->next = rtl_code->next;
			}
			if (rtl_code->next) {
				rtl_code->next->prev = rtl_code->prev;
			}
			free (rtl_code);
			rtl_code = tmp;
			continue;
		}
		if (!rtl_code->u.code.head || !rtl_code->u.code.tail) {
			/* seriously bad */
			fprintf (stderr, "%s: error: found very wrong RTL code block at index %d\n", progname, bi);
			return NULL;
		}
		rtl_code = rtl_code->next;
	}
	return head;
}
/*}}}*/
/*{{{  int rtl_remove_deadnamelabels (rtl_chain *rtl_code)*/
/*
 *	removes unreferenced labels (RTL_SETNAMEDLABEL) only
 */
int rtl_remove_deadnamelabels (rtl_chain *rtl_code)
{
	rtl_chain *tmp;
	char **strdef;
	int *strref;
	int str_max;
	ins_chain *itmp;
	int new_max, i, j;
	char *tmp_str;
	int str_lab_removed;

	str_max = 0;
	strref = NULL;
	strdef = NULL;
	for (tmp=rtl_code; tmp; tmp=tmp->next) {
		switch (tmp->type) {
		case RTL_CODE:
			for (itmp=tmp->u.code.head; itmp; itmp=itmp->next) {
				if (itmp->type != INS_SETLABEL) {
					for (i=0; itmp->in_args[i]; i++) {
						if ((itmp->in_args[i]->flags & ARG_MODEMASK) == ARG_NAMEDLABEL) {
							tmp_str = (char *)itmp->in_args[i]->regconst;
							for (j=0; (j<str_max) && (strdef[j]) && strcmp(tmp_str, strdef[j]); j++);
							if (j == str_max) {
								new_max = str_max + 10;
								strdef = (char **)srealloc ((void *)strdef, str_max * sizeof(char *), new_max * sizeof(char *));
								strref = (int *)srealloc ((void *)strref, str_max * sizeof(int), new_max * sizeof(int));
								str_max = new_max;
								/* mark as defined */
								strdef[j] = tmp_str;
							} else if (!strdef[j]) {
								strdef[j] = tmp_str;
							}
							strref[j]++;
						} /* if () */
					} /* for () */
				} /* if () */
				for (i=0; itmp->out_args[i]; i++) {
					if ((itmp->out_args[i]->flags & ARG_MODEMASK) == ARG_NAMEDLABEL) {
						tmp_str = (char *)itmp->out_args[i]->regconst;
						for (j=0; (j<str_max) && (strdef[j]) && strcmp(tmp_str, strdef[j]); j++);
						if (j == str_max) {
							new_max = str_max + 10;
							strdef = (char **)srealloc ((void *)strdef, str_max * sizeof(char *), new_max * sizeof(char *));
							strref = (int *)srealloc ((void *)strref, str_max * sizeof(int), new_max * sizeof(int));
							str_max = new_max;
							/* mark as defined */
							strdef[j] = tmp_str;
						} else if (!strdef[j]) {
							strdef[j] = tmp_str;
						}
						strref[j]++;
					}
				} /* for () */
			} /* for () */
			break;
		case RTL_SETNAMEDLABEL:
			tmp_str = tmp->u.label_name;
			for (j=0; (j<str_max) && (strdef[j]) && strcmp(tmp_str, strdef[j]); j++);
			if (j == str_max) {
				new_max = str_max + 10;
				strdef = (char **)srealloc ((void *)strdef, str_max * sizeof(char *), new_max * sizeof(char *));
				strref = (int *)srealloc ((void *)strref, str_max * sizeof(int), new_max * sizeof(int));
				str_max = new_max;
				/* mark as defined */
				strdef[j] = tmp_str;
			} else if (!strdef[j]) {
				strdef[j] = tmp_str;
			}
			break;
		default:
			break;
		}
	}
	/* now go through and remove un-referenced ones... */
	str_lab_removed = 0;
	for (tmp=rtl_code; tmp; tmp=tmp->next) {
		if (tmp->type == RTL_SETNAMEDLABEL) {
			tmp_str = tmp->u.label_name;
			for (i=0; (i < str_max) && strdef[i]; i++) {
				if (!strcmp (strdef[i], tmp_str)) {
					break;
				}
			}
			if (i == str_max) {
			} else if (!strdef[i]) {
			} else {
				if (!strref[i]) {
					/* change type and get hoovered later */
					tmp->type = RTL_CODE;
					tmp->u.code.head = tmp->u.code.tail = NULL;
					str_lab_removed++;
				}
			}
		}
	}
	sfree (strdef);
	sfree (strref);
	return str_lab_removed;
}
/*}}}*/
/*{{{  int rtl_validate_checknumargs (ins_chain *ins, int nin, int nout)*/
/*
 *	checks instruction for specified number of input and output operands
 */
int rtl_validate_checknumargs (ins_chain *ins, int nin, int nout)
{
	int i;

	for (i=0; i<nin; i++) {
		if (!ins->in_args[i]) {
			return 0;
		}
	}
	for (i=0; i<nout; i++) {
		if (!ins->out_args[i]) {
			return 0;
		}
	}
	return 1;
}
/*}}}*/
/*{{{  int rtl_validate_checkargtype (ins_arg *arg, ...)*/
/*
 *	checks argument types -- note: this relies on arg (0 < type < 32)!
 */
int rtl_validate_checkargtype (ins_arg *arg, ...)
{
	va_list ap;
	int valid, type;

	valid = 0;
	va_start (ap, arg);
	type = va_arg (ap, int);
	while (type != 0) {
		valid |= (1 << type);
		type = va_arg (ap, int);
	}
	va_end (ap);
	type = (1 << (arg->flags & ARG_MODEMASK));
	if (type & ~valid) {
		return 0;
	}
	return 1;
}
/*}}}*/
/*{{{  int rtl_validate_instr (ins_chain *ins, arch_t *arch)*/
/*
 *	validates a single instruction, calls via arch if not control instruction
 */
int rtl_validate_instr (ins_chain *ins, arch_t *arch)
{
	switch (ins->type) {
	case INS_START_REG:
	case INS_END_REG:
	case INS_CONSTRAIN_REG:
	case INS_UNCONSTRAIN_REG:
	case INS_FREE_REG:
	case INS_START_CC:
	case INS_END_CC:
	case INS_SOURCELINE:
	case INS_CLEANUP:
		return 1;
	}
	return arch->rtl_validate_instr (ins);
}
/*}}}*/
/*{{{  int rtl_prevalidate_instr (ins_chain *ins, arch_t *arch)*/
/*
 *	pre-validates a single instruction, calls via arch if not control instruction
 */
int rtl_prevalidate_instr (ins_chain *ins, arch_t *arch)
{
	switch (ins->type) {
	case INS_START_REG:
	case INS_END_REG:
	case INS_CONSTRAIN_REG:
	case INS_UNCONSTRAIN_REG:
	case INS_FREE_REG:
	case INS_START_CC:
	case INS_END_CC:
	case INS_SOURCELINE:
	case INS_CLEANUP:
		return 1;
	}
	return arch->rtl_prevalidate_instr (ins);
}
/*}}}*/
/*{{{  int rtl_validate_rtl (rtl_chain *rtl_code, arch_t *arch)*/
/*
 *	validates RTL instructions
 *	returns 1 if all valid, 0 if not
 */
int rtl_validate_rtl (rtl_chain *rtl_code, arch_t *arch)
{
	rtl_chain *tmp;
	ins_chain *itmp;

	for (tmp=rtl_code; tmp; tmp=tmp->next) {
		if (tmp->type == RTL_CODE) {
			for (itmp=tmp->u.code.head; itmp; itmp=itmp->next) {
				if (!rtl_validate_instr (itmp, arch)) {
					#ifdef INSTRUCTION_HISTORY
						fprintf (stderr, "invalid instruction (type %d) allocated from %s:%ld\n", itmp->type, itmp->alloc_file, itmp->alloc_line);
					#endif
					return 0;
				}
			}
		}
	}
	return 1;
}
/*}}}*/
/*{{{  int rtl_prevalidate_rtl (rtl_chain *rtl_code, arch_t *arch)*/
/*
 *	prevalidates RTL instructions
 *	returns 1 if all OK, 0 if not
 */
int rtl_prevalidate_rtl (rtl_chain *rtl_code, arch_t *arch)
{
	rtl_chain *tmp;
	ins_chain *itmp;

	for (tmp=rtl_code; tmp; tmp=tmp->next) {
		if (tmp->type == RTL_CODE) {
			for (itmp=tmp->u.code.head; itmp; itmp=itmp->next) {
				if (!rtl_prevalidate_instr (itmp, arch)) {
					#ifdef INSTRUCTION_HISTORY
						fprintf (stderr, "invalid instruction (type %d) allocated from %s:%ld\n", itmp->type, itmp->alloc_file, itmp->alloc_line);
					#endif
					return 0;
				}
			}
		}
	}
	return 1;
}
/*}}}*/

#define P2CHR(X,Y) (unsigned int)(((unsigned int)X << 8) | (unsigned int)Y)
/*{{{  static int decode_ins_mnemonic (char *str)*/
/*
 *	attempts to get an instruction from a string
 */
static int decode_ins_mnemonic (char *str)
{
	int ins = INS_UNDEFINED;

	switch (P2CHR (str[0],str[1])) {
	case P2CHR ('a','d'):
		if (!strcmp (str + 2, "c")) {
			ins = INS_ADC;
		} else if (!strcmp (str + 2, "d")) {
			ins = INS_ADD;
		}
		break;
	case P2CHR ('a','n'):
		if (!strcmp (str + 2, "d")) {
			ins = INS_AND;
		} else if (!strcmp (str + 2, "no")) {
			ins = INS_ANNO;
		}
		break;
	case P2CHR ('c','a'):
		if (!strcmp (str + 2, "ll")) {
			ins = INS_CALL;
		}
		break;
	case P2CHR ('c','q'):
		if (!strcmp (str + 2, "q")) {
			ins = INS_CDQ;
		}
		break;
	case P2CHR ('c','j'):
		if (!strcmp (str + 2, "ump")) {
			ins = INS_CJUMP;
		}
		break;
	case P2CHR ('c','l'):
		if (!strcmp (str + 2, "eanup")) {
			ins = INS_CLEANUP;
		}
		break;
	case P2CHR ('c','m'):
		if (!strcmp (str + 2, "ove")) {
			ins = INS_CMOVE;
		} else if (!strcmp (str + 2, "p")) {
			ins = INS_CMP;
		}
		break;
	case P2CHR ('c','o'):
		if (!strcmp (str + 2, "nstlabdiff")) {
			ins = INS_CONSTLABDIFF;
		} else if (!strcmp (str + 2, "nstrain")) {
			ins = INS_CONSTRAIN_REG;
		} else if (!strcmp (str + 2, "nstlabaddr")) {
			ins = INS_CONSTLABADDR;
		}
		break;
	case P2CHR ('c','w'):
		if (!strcmp (str + 2, "de")) {
			ins = INS_CWDE;
		}
		break;
	case P2CHR ('d','e'):
		if (!strcmp (str + 2, "c")) {
			ins = INS_DEC;
		}
		break;
	case P2CHR ('d','i'):
		if (!strcmp (str + 2, "v")) {
			ins = INS_DIV;
		}
		break;
	case P2CHR ('e','n'):
		if (!strcmp (str + 2, "dcc")) {
			ins = INS_END_CC;
		} else if (!strcmp (str + 2, "dreg")) {
			ins = INS_END_REG;
		}
		break;
	case P2CHR ('f','a'):
		if (!strcmp (str + 2, "bs")) {
			ins = INS_FABS;
		} else if (!strcmp (str + 2, "dd")) {
			ins = INS_FADD;
		} else if (!strcmp (str + 2, "dd32")) {
			ins = INS_FADD32;
		} else if (!strcmp (str + 2, "dd64")) {
			ins = INS_FADD64;
		}
		break;
	case P2CHR ('f','c'):
		if (!strcmp (str + 2, "hs")) {
			ins = INS_FCHS;
		} else if (!strcmp (str + 2, "om")) {
			ins = INS_FCOM;
		} else if (!strcmp (str + 2, "omp")) {
			ins = INS_FCOMP;
		} else if (!strcmp (str + 2, "ompp")) {
			ins = INS_FCOMPP;
		}
		break;
	case P2CHR ('f','d'):
		if (!strcmp (str + 2, "iv")) {
			ins = INS_FDIV;
		}
		break;
	case P2CHR ('f','i'):
		if (!strcmp (str + 2, "ld32")) {
			ins = INS_FILD32;
		} else if (!strcmp (str + 2, "ld64")) {
			ins = INS_FILD64;
		} else if (!strcmp (str + 2, "st32")) {
			ins = INS_FIST32;
		} else if (!strcmp (str + 2, "st64")) {
			ins = INS_FIST64;
		}
		break;
	case P2CHR ('f','l'):
		if (!strcmp (str + 2, "d")) {
			ins = INS_FLD;
		} else if (!strcmp (str + 2, "d1")) {
			ins = INS_FLD1;
		} else if (!strcmp (str + 2, "d32")) {
			ins = INS_FLD32;
		} else if (!strcmp (str + 2, "d64")) {
			ins = INS_FLD64;
		} else if (!strcmp (str + 2, "d80")) {
			ins = INS_FLD80;
		} else if (!strcmp (str + 2, "dcw")) {
			ins = INS_FLDCW;
		} else if (!strcmp (str + 2, "dl2e")) {
			ins = INS_FLDL2E;
		} else if (!strcmp (str + 2, "dl2t")) {
			ins = INS_FLDL2T;
		} else if (!strcmp (str + 2, "dlg2")) {
			ins = INS_FLDLG2;
		} else if (!strcmp (str + 2, "dln2")) {
			ins = INS_FLDLN2;
		} else if (!strcmp (str + 2, "dpi")) {
			ins = INS_FLDPI;
		} else if (!strcmp (str + 2, "dz")) {
			ins = INS_FLDZ;
		}
		break;
	case P2CHR ('f','m'):
		if (!strcmp (str + 2, "ul")) {
			ins = INS_FMUL;
		} else if (!strcmp (str + 2, "ul32")) {
			ins = INS_FMUL32;
		} else if (!strcmp (str + 2, "ul64")) {
			ins = INS_FMUL64;
		}
		break;
	case P2CHR ('f','p'):
		if (!strcmp (str + 2, "rem1")) {
			ins = INS_FPREM1;
		}
		break;
	case P2CHR ('f','r'):
		if (!strcmp (str + 2, "eereg")) {
			ins = INS_FREE_REG;
		} else if (!strcmp (str + 2, "ndint")) {
			ins = INS_FRNDINT;
		}
		break;
	case P2CHR ('f','s'):
		if (!strcmp (str + 2, "cale")) {
			ins = INS_FSCALE;
		} else if (!strcmp (str + 2, "qrt")) {
			ins = INS_FSQRT;
		} else if (!strcmp (str + 2, "t32")) {
			ins = INS_FST32;
		} else if (!strcmp (str + 2, "t64")) {
			ins = INS_FST64;
		} else if (!strcmp (str + 2, "t80")) {
			ins = INS_FST80;
		} else if (!strcmp (str + 2, "tcw")) {
			ins = INS_FSTCW;
		} else if (!strcmp (str + 2, "tp")) {
			ins = INS_FSTP;
		} else if (!strcmp (str + 2, "tsw")) {
			ins = INS_FSTSW;
		} else if (!strcmp (str + 2, "ub")) {
			ins = INS_FSUB;
		}
		break;
	case P2CHR ('f','t'):
		if (!strcmp (str + 2, "st")) {
			ins = INS_FTST;
		}
		break;
	case P2CHR ('f','u'):
		if (!strcmp (str + 2, "com")) {
			ins = INS_FUCOM;
		} else if (!strcmp (str + 2, "comp")) {
			ins = INS_FUCOMP;
		} else if (!strcmp (str + 2, "compp")) {
			ins = INS_FUCOMPP;
		}
		break;
	case P2CHR ('f','x'):
		if (!strcmp (str + 2, "am")) {
			ins = INS_FXAM;
		} else if (!strcmp (str + 2, "ch")) {
			ins = INS_FXCH;
		}
		break;
	case P2CHR ('i','n'):
		if (!strcmp (str + 2, "c")) {
			ins = INS_INC;
		} else if (!strcmp (str + 2, "to")) {
			ins = INS_INTO;
		}
		break;
	case P2CHR ('j','u'):
		if (!strcmp (str + 2, "mp")) {
			ins = INS_JUMP;
		}
		break;
	case P2CHR ('k','c'):
		if (!strcmp (str + 2, "all")) {
			ins = INS_KCALL;
		}
		break;
	case P2CHR ('l','a'):
		if (!strcmp (str + 2, "hf")) {
			ins = INS_LAHF;
		}
		break;
	case P2CHR ('l','e'):
		if (!strcmp (str + 2, "a")) {
			ins = INS_LEA;
		}
		break;
	case P2CHR ('l','o'):
		if (!strcmp (str + 2, "adlabdiff")) {
			ins = INS_LOADLABDIFF;
		}
		break;
	case P2CHR ('m','o'):
		if (!strcmp (str + 2, "ve")) {
			ins = INS_MOVE;
		} else if (!strcmp (str + 2, "veb")) {
			ins = INS_MOVEB;
		} else if (!strcmp (str + 2, "vezext16to32")) {
			ins = INS_MOVEZEXT16TO32;
		} else if (!strcmp (str + 2, "vesext16to32")) {
			ins = INS_MOVESEXT16TO32;
		} else if (!strcmp (str + 2, "vezext8to32")) {
			ins = INS_MOVEZEXT8TO32;
		}
		break;
	case P2CHR ('m','u'):
		if (!strcmp (str + 2, "l")) {
			ins = INS_MUL;
		}
		break;
	case P2CHR ('n','o'):
		if (!strcmp (str + 2, "p")) {
			ins = INS_NOP;
		} else if (!strcmp (str + 2, "t")) {
			ins = INS_NOT;
		}
		break;
	case P2CHR ('o','r'):
		if (!strcmp (str + 2, "")) {
			ins = INS_OR;
		}
		break;
	case P2CHR ('p', 'j'):
		if (!strcmp (str + 2, "ump")) {
			ins = INS_PJUMP;
		}
		break;
	case P2CHR ('p','o'):
		if (!strcmp (str + 2, "p")) {
			ins = INS_POP;
		}
		break;
	case P2CHR ('p','u'):
		if (!strcmp (str + 2, "sh")) {
			ins = INS_PUSH;
		}
		break;
	case P2CHR ('r','c'):
		if (!strcmp (str + 2, "l")) {
			ins = INS_RCL;
		} else if (!strcmp (str + 2, "r")) {
			ins = INS_RCR;
		}
		break;
	case P2CHR ('r','d'):
		if (!strcmp (str + 2, "tsc")) {
			ins = INS_RDTSC;
		}
		break;
	case P2CHR ('r','e'):
		if (!strcmp (str + 2, "pmoveb")) {
			ins = INS_REPMOVEB;
		} else if (!strcmp (str + 2, "pmovel")) {
			ins = INS_REPMOVEL;
		} else if (!strcmp (str + 2, "t")) {
			ins = INS_RET;
		}
		break;
	case P2CHR ('r','o'):
		if (!strcmp (str + 2, "l")) {
			ins = INS_ROL;
		} else if (!strcmp (str + 2, "r")) {
			ins = INS_ROR;
		}
		break;
	case P2CHR ('s','a'):
		if (!strcmp (str + 2, "hf")) {
			ins = INS_SAHF;
		}
		break;
	case P2CHR ('s','b'):
		if (!strcmp (str + 2, "b")) {
			ins = INS_SBB;
		}
		break;
	case P2CHR ('s','e'):
		if (!strcmp (str + 2, "tcc")) {
			ins = INS_SETCC;
		} else if (!strcmp (str + 2, "tflabel")) {
			ins = INS_SETFLABEL;
		} else if (!strcmp (str + 2, "tlabel")) {
			ins = INS_SETLABEL;
		}
		break;
	case P2CHR ('s','h'):
		if (!strcmp (str + 2, "l")) {
			ins = INS_SHL;
		} else if (!strcmp (str + 2, "ld")) {
			ins = INS_SHLD;
		} else if (!strcmp (str + 2, "r")) {
			ins = INS_SHR;
		} else if (!strcmp (str + 2, "rd")) {
			ins = INS_SHRD;
		}
		break;
	case P2CHR ('s','o'):
		if (!strcmp (str + 2, "urceline")) {
			ins = INS_SOURCELINE;
		}
		break;
	case P2CHR ('s','t'):
		if (!strcmp (str + 2, "artcc")) {
			ins = INS_START_CC;
		} else if (!strcmp (str + 2, "artreg")) {
			ins = INS_START_REG;
		}
		break;
	case P2CHR ('s','u'):
		if (!strcmp (str + 2, "b")) {
			ins = INS_SUB;
		}
		break;
	case P2CHR ('s','w'):
		if (!strcmp (str + 2, "ap")) {
			ins = INS_SWAP;
		}
		break;
	case P2CHR ('u','d'):
		if (!strcmp (str + 2, "iv")) {
			ins = INS_UDIV;
		}
		break;
	case P2CHR ('u','m'):
		if (!strcmp (str + 2, "ul")) {
			ins = INS_UMUL;
		}
		break;
	case P2CHR ('u','n'):
		if (!strcmp (str + 2, "constrain")) {
			ins = INS_UNCONSTRAIN_REG;
		} else if (!strcmp (str + 2, "defined")) {
			/* why, i don't know, but heyho */
			ins = INS_UNDEFINED;
		}
		break;
	case P2CHR ('w','a'):
		if (!strcmp (str + 2, "it")) {
			ins = INS_WAIT;
		}
		break;
	case P2CHR ('x','o'):
		if (!strcmp (str + 2, "r")) {
			ins = INS_XOR;
		}
		break;
	default:
		break;
	}
	return ins;
}
/*}}}*/
/*{{{  static ins_arg *decode_arg (char *arg)*/
/*
 *	turns text argument into real argument (or returns NULL on failure)
 */
static ins_arg *decode_arg (char *arg)
{
	static char arg_copy[128];
	ins_arg *tmp;
	int l = strlen (arg);

	if (l > 127) {
		/* ick! */
		return NULL;
	}
	strcpy (arg_copy, arg);
	arg = (char *)arg_copy;

	tmp = new_ins_arg ();
	if (*arg == '[') {
		/* scoop up implicit operand */
		tmp->flags |= ARG_IMP;
		if (arg[l-1] != ']') {
			return NULL;
		}
		l--;
		arg[l] = '\0';		/* chop off trailing ] */
		arg++, l--;
	}

	return tmp;
}
/*}}}*/

#define IMCDIRECTIVE_UNDEFINED 0
#define IMCDIRECTIVE_INIT 1
#define IMCDIRECTIVE_NEWREG 2

/*{{{  ins_chain *decode_ins (tstate *ts, char *str)*/
/*
 *	creates an instruction from a string representation (like in RTL)
 *	FIXME: seriously incomplete..
 */
ins_chain *decode_ins (tstate *ts, char *str)
{
	static char str_copy[256];
	int l = strlen (str);
	ins_chain *tmp_ins;
	int i, i_in, i_out;
	char *ch, *dh, *orig_str;
	int is_directive;
	#define chr_eol(C) ((C) == '\0')

	if (l > 255) {
		fprintf (stderr, "%s: error: what sort of instruction is that ??: %s\n", progname, str);
		return NULL;
	}
	orig_str = str;
	strcpy (str_copy, str);
	str = (char *)str_copy;
	ch = strchr (str, '\n');
	if (ch) {
		*ch = '\0';
	}
	ch = strchr (str, '\r');
	if (ch) {
		*ch = '\0';
	}

	/* skip leading whitespace */
	for (ch = str; (*ch == ' ') || (*ch == '\t'); ch++);
	/* pick off mnemonic */
	for (dh = ch; (*ch != ' ') && (*ch != '\t') && (*ch != '\0'); ch++);
	if (*dh == '\0') {
		/* no mnemonic */
		fprintf (stderr, "%s: error: badly formed instruction for decode: %s\n", progname, orig_str);
		exit (EXIT_FAILURE);
	}
	*ch = '\0';
	for (ch++; (*ch == ' ') || (*ch == '\t'); ch++);
	tmp_ins = new_ins ();
	is_directive = IMCDIRECTIVE_UNDEFINED;
	if (*dh == '.') {
		/* directive, rather than instruction */
		if (!strcmp (dh, ".INIT")) {
			is_directive = IMCDIRECTIVE_INIT;
		} else if (!strcmp (dh, ".NEWREG")) {
			is_directive = IMCDIRECTIVE_NEWREG;
		} else {
			/* unrecognised directive */
			fprintf (stderr, "%s: error: unrecognised directive in decode: %s\n", progname, orig_str);
			exit (EXIT_FAILURE);
		}
	} else {
		tmp_ins->type = decode_ins_mnemonic (dh);
		if (tmp_ins->type == INS_UNDEFINED) {
			/* unrecognised mnemonic */
			fprintf (stderr, "%s: error: unrecognised mnemonic in decode: %s\n", progname, orig_str);
			exit (EXIT_FAILURE);
		}
	}
	i_in = 0;
	i_out = 0;
	/* do input operands */
	while (!chr_eol (*ch)) {
		ins_arg *targ;
		char tch, *eh;

		if (*ch == ';') {
			/* no input operands */
			*ch = '\0';
			ch++;
			break;
		}
		/* scan dh over ch to get argument */
		for (dh = ch; (*dh != ',') && (*dh != ';') && !chr_eol (*dh); dh++);
		tch = *dh;
		*dh = '\0';
		/* move eh backwards over whitespace */
		for (eh = dh - 1; (*eh == ' ') || (*eh == '\t'); *eh = '\0', eh--);
		targ = decode_arg (ch);
		if (!targ) {
			/* busted operand */
			fprintf (stderr, "%s: error: unrecognised operand in decode: %s\n", progname, orig_str);
			exit (EXIT_FAILURE);
		}
		tmp_ins->in_args[i_in++] = targ;
		if (chr_eol (tch)) {
			ch = dh;
		} else {
			for (ch = dh + 1; (*ch == ' ') || (*ch == '\t'); ch++);
			if (tch == ';') {
				break;
			}
		}
	}
	/* do output operands */
	while (!chr_eol (*ch)) {
		ins_arg *targ;
		char tch, *eh;

		/* scan dh over ch to get argument */
		for (dh = ch; (*dh != ',') && !chr_eol (*dh); dh++);
		tch = *dh;
		*dh = '\0';
		/* move eh backwards over whitespace */
		for (eh = dh - 1; (*eh == ' ') || (*eh == '\t'); *eh = '\0', eh--);
		targ = decode_arg (ch);
		if (!targ) {
			/* busted operand */
			fprintf (stderr, "%s: error: unrecognised operand in decode: %s\n", progname, orig_str);
			exit (EXIT_FAILURE);
		}
		tmp_ins->out_args[i_out++] = targ;
		if (chr_eol (tch)) {
			ch = dh;
		} else {
			for (ch = dh + 1; (*ch == ' ') || (*ch == '\t'); ch++);
		}
	}
	/* do directive processing first */
	switch (is_directive) {
	default:
		/* umm */
		break;
	case IMCDIRECTIVE_INIT:
		ustate_clear (ts);
		break;
	case IMCDIRECTIVE_UNDEFINED:
		/* it's an instruction mnemonic, subsitute in (user) labels and registers */
		for (i = 0; i < i_in; i++) {
			switch (ArgMode (tmp_ins->in_args[i])) {
			case ARG_REG:
			case ARG_REGIND:
				ArgReg (tmp_ins->in_args[i]) = ustate_regof (ts, ArgReg (tmp_ins->in_args[i]));
				break;
			}
		}
		for (i = 0; i < i_out; i++) {
			switch (ArgMode (tmp_ins->out_args[i])) {
			case ARG_REG:
			case ARG_REGIND:
				ArgReg (tmp_ins->out_args[i]) = ustate_regof (ts, ArgReg (tmp_ins->out_args[i]));
				break;
			}
		}
		break;
	}
	return tmp_ins;
	#undef chr_eol
}
/*}}}*/
/*{{{  ins_chain *compose_ins (int ins, int ops_in, int ops_out, ...)*/
/*
 *	creates an instruction
 */
#ifdef INSTRUCTION_HISTORY
ins_chain *compose_ins2 (char *file, long line, int etc_ins, int ins, int ops_in, int ops_out, ...)
#else
ins_chain *compose_ins_ex (int etc_ins, int ins, int ops_in, int ops_out, ...)
#endif
{
	ins_chain *tmp_ins;
	va_list ap;
	int i, i_in, i_out;
	unsigned int flag;
	ins_sib_arg *t_sib;

	tmp_ins = new_ins ();
	tmp_ins->type = ins;
	tmp_ins->etc_type = etc_ins;
	#ifdef INSTRUCTION_HISTORY
		strcpy (tmp_ins->alloc_file, file);
		tmp_ins->alloc_line = line;
	#endif
	va_start (ap, ops_out);
	for (i_in=0; i_in<ops_in; i_in++) {
		i = va_arg (ap, int);

		if (i == ARG_ARG) {
			tmp_ins->in_args[i_in] = va_arg (ap, ins_arg *);
		} else {
			tmp_ins->in_args[i_in] = new_ins_arg ();
			flag = i & ARG_FLAGMASK;
			if ((flag & ARG_MODEMASK) == ARG_CONST) {
				flag |= ARG_ISCONST;
			}
			tmp_ins->in_args[i_in]->flags = flag;
			switch (flag & ARG_MODEMASK) {
			case ARG_REG:
			case ARG_CONST:
			case ARG_REGIND:
			case ARG_COND:
			case ARG_LABEL:
			case ARG_FREG:
			case ARG_FLABEL:
			case ARG_BLABEL:
				tmp_ins->in_args[i_in]->regconst = va_arg (ap, int);
				break;
			case ARG_NAMEDLABEL:
			case ARG_TEXT:
				tmp_ins->in_args[i_in]->regconst = (int)va_arg (ap, char *);
				break;
			case ARG_REGINDSIB:
				t_sib = (ins_sib_arg *)smalloc (sizeof (ins_sib_arg));
				t_sib->scale = va_arg (ap, int);
				t_sib->index = va_arg (ap, int);
				t_sib->base = va_arg (ap, int);
				tmp_ins->in_args[i_in]->regconst = (int)t_sib;
				break;
			default:
				fprintf (stderr, "%s: warning: input operand not reg/const/regind/cond/label\n", progname);
				break;
			}
			if (flag & ARG_DISP) {
				tmp_ins->in_args[i_in]->disp = va_arg (ap, int);
			}
			if ((flag & ARG_IND) && (ins != INS_CALL) && (ins != INS_JUMP) && (ins != INS_PJUMP) && (ins != INS_CJUMP)) {
				fprintf (stderr, "%s: warning: IND specified on non call/jump instruction %d\n", progname, tmp_ins->type);
				break;
			}
		}
	}
	for (i_out=0; i_out<ops_out; i_out++) {
		i = va_arg (ap, int);

		if (i == ARG_ARG) {
			tmp_ins->out_args[i_out] = va_arg (ap, ins_arg *);
		} else {
			tmp_ins->out_args[i_out] = new_ins_arg ();
			flag = i & ARG_FLAGMASK;
			tmp_ins->out_args[i_out]->flags = flag;
			switch (flag & ARG_MODEMASK) {
			case ARG_REG:
			case ARG_REGIND:
			case ARG_FREG:
				tmp_ins->out_args[i_out]->regconst = va_arg (ap, int);
				break;
			case ARG_NAMEDLABEL:
				/* allowed usually -- destination is whatever label points at */
				tmp_ins->out_args[i_out]->regconst = (int)va_arg (ap, char *);
				break;
			default:
				fprintf (stderr, "%s: warning: output operand not reg/regind\n", progname);
				break;
			}
			if (flag & ARG_DISP) {
				tmp_ins->out_args[i_out]->disp = va_arg (ap, int);
			}
			if ((flag & ARG_IND) && (ins != INS_CALL) && (ins != INS_JUMP) && (ins != INS_PJUMP) && (ins != INS_CJUMP)) {
				fprintf (stderr, "%s: warning: IND specified on non call/jump instruction %d\n", progname, tmp_ins->type);
				break;
			}
		}
	}
	va_end (ap);
	return tmp_ins;
}
/*}}}*/
/*{{{  ins_arg *new_ins_arg (void)*/
/*
 *	creates a new ins_arg structure
 */
ins_arg *new_ins_arg (void)
{
	ins_arg *tmp;

	tmp = (ins_arg *)smalloc (sizeof (ins_arg));
	memset (tmp, 0, sizeof (ins_arg));
	return tmp;
}
/*}}}*/
/*{{{  ins_arg *compose_ins_arg (int argtype, ...)*/
/*
 *	composes a new ins_arg structure
 */
ins_arg *compose_ins_arg (int argtype, ...)
{
	va_list ap;
	ins_arg *tmp_arg;

	if (argtype == ARG_ARG) {
		va_start (ap, argtype);
		tmp_arg = va_arg (ap, ins_arg *);
		va_end (ap);
	} else {
		int flag = argtype & ARG_FLAGMASK;


		if ((flag & ARG_MODEMASK) == ARG_CONST) {
			flag |= ARG_ISCONST;
		}
		tmp_arg = new_ins_arg ();
		tmp_arg->flags = flag;

		va_start (ap, argtype);
		switch (flag & ARG_MODEMASK) {
		case ARG_REG:
		case ARG_CONST:
		case ARG_REGIND:
		case ARG_COND:
		case ARG_LABEL:
		case ARG_FREG:
		case ARG_FLABEL:
		case ARG_BLABEL:
			tmp_arg->regconst = va_arg (ap, int);
			break;
		case ARG_NAMEDLABEL:
		case ARG_TEXT:
			tmp_arg->regconst = (int)va_arg (ap, char *);
			break;
		case ARG_REGINDSIB:
			{
				ins_sib_arg *t_sib;

				t_sib = (ins_sib_arg *)smalloc (sizeof (ins_sib_arg));
				t_sib->scale = va_arg (ap, int);
				t_sib->index = va_arg (ap, int);
				t_sib->base = va_arg (ap, int);
				tmp_arg->regconst = (int)t_sib;
			}
			break;
		default:
			fprintf (stderr, "%s: warning: input operand not reg/const/regind/cond/label\n", progname);
			abort ();
			break;
		}
		if (flag & ARG_DISP) {
			tmp_arg->disp = va_arg (ap, int);
		}
		va_end (ap);
	}

	return tmp_arg;
}

/*}}}*/
/*{{{  ins_sib_arg *new_ins_sib_arg (void)*/
/*
 *	creates a new ins_sib_arg structure
 */
ins_sib_arg *new_ins_sib_arg (void)
{
	ins_sib_arg *tmp;

	tmp = (ins_sib_arg *)smalloc (sizeof (ins_sib_arg));
	memset (tmp, 0, sizeof (ins_sib_arg));
	return tmp;
}
/*}}}*/
/*{{{  rtl_chain *new_rtl (void)*/
/*
 *	creates a new RTL structure
 */
rtl_chain *new_rtl (void)
{
	rtl_chain *tmp;

	tmp = (rtl_chain *)smalloc (sizeof (rtl_chain));
	memset (tmp, 0, sizeof (rtl_chain));
	return tmp;
}
/*}}}*/
/*{{{  void free_rtl (rtl_chain *rtl)*/
/*
 *	frees an RTL structure (and any relevant contents)
 */
void free_rtl (rtl_chain *rtl)
{
	/* ...to be completed... */
	return;
}
/*}}}*/
/*{{{  ins_chain *new_ins (void)*/
/*
 *	creates a new instruction structure
 */
ins_chain *new_ins (void)
{
	ins_chain *tmp;

	tmp = (ins_chain *)smalloc (sizeof (ins_chain));
	memset (tmp, 0, sizeof (ins_chain));
	return tmp;
}
/*}}}*/
/*{{{  void rtl_set_lastvreg (int reg)*/
/*
 *	sets the last virtual register (called from main.c)
 */
void rtl_set_lastvreg (int reg)
{
	last_virtual_register = reg;
	return;
}
/*}}}*/
/*{{{  int rtl_get_newvreg (void)*/
/*
 *	gets a new virtual register (when needed) after translation
 */
int rtl_get_newvreg (void)
{
	return ++last_virtual_register;
}
/*}}}*/
/*{{{  int rtl_get_lastvreg (void)*/
/*
 *	returns the last virtual register
 */
int rtl_get_lastvreg (void)
{
	return last_virtual_register;
}
/*}}}*/
/*{{{  int rtl_nvregs_in_arg (ins_arg *arg)*/
/*
 *	returns the number of virtual registers used in `arg'
 */
int rtl_nvregs_in_arg (ins_arg *arg)
{
	ins_sib_arg *sib_arg;
	
	switch (arg->flags & ARG_MODEMASK) {
	case ARG_REG:
	case ARG_REGIND:
		if (arg->regconst >= FIRST_VIRTUAL_REG) {
			return 1;
		}
		return 0;
	case ARG_REGINDSIB:
		sib_arg = (ins_sib_arg *)arg->regconst;
		if ((sib_arg->base >= FIRST_VIRTUAL_REG) && (sib_arg->index >= FIRST_VIRTUAL_REG)) {
			return 2;
		} else if ((sib_arg->base >= FIRST_VIRTUAL_REG) || (sib_arg->index >= FIRST_VIRTUAL_REG)) {
			return 1;
		}
		return 0;
	}
	return 0;
}
/*}}}*/
/*{{{  int rtl_nvreg_of_arg (ins_arg *arg, int n)*/
/*
 *	returns the `n'th virtual register used in `arg'
 */
int rtl_nvreg_of_arg (ins_arg *arg, int n)
{
	ins_sib_arg *sib_arg;

	switch (arg->flags & ARG_MODEMASK) {
	case ARG_REG:
	case ARG_REGIND:
		if ((arg->regconst >= FIRST_VIRTUAL_REG) && !n) {
			return arg->regconst;
		}
		return 0;
	case ARG_REGINDSIB:
		sib_arg = (ins_sib_arg *)arg->regconst;
		if ((sib_arg->base >= FIRST_VIRTUAL_REG) && !n) {
			return sib_arg->base;
		} else if ((sib_arg->base >= FIRST_VIRTUAL_REG) && (sib_arg->index >= FIRST_VIRTUAL_REG) && (n == 1)) {
			return sib_arg->index;
		} else if ((sib_arg->index >= FIRST_VIRTUAL_REG) && !n) {
			return sib_arg->index;
		}
		return 0;
	}
	return 0;
}
/*}}}*/
/*{{{  int rtl_rename_reg_in_arg (ins_arg *arg, int old_reg, int new_reg)*/
/*
 *	renames a register in a single argument
 *	returns non-zero if something got changed
 */
int rtl_rename_reg_in_arg (ins_arg *arg, int old_reg, int new_reg)
{
	ins_sib_arg *sib_arg;
	int r = 0;

	switch (arg->flags & ARG_MODEMASK) {
	case ARG_REG:
	case ARG_REGIND:
		if (arg->regconst == old_reg) {
			arg->regconst = new_reg;
			r++;
		}
		break;
	case ARG_REGINDSIB:
		sib_arg = (ins_sib_arg *)arg->regconst;
		if (sib_arg->base == old_reg) {
			sib_arg->base = new_reg;
			r++;
		}
		if (sib_arg->index == old_reg) {
			sib_arg->index = new_reg;
			r++;
		}
		break;
	}
	return r;
}
/*}}}*/
/*{{{  int rtl_rename_reg (ins_chain *startfrom, int old_reg, int new_reg)*/
/*
 *	renames a register starting from `startfrom' until it INS_END_REGs
 */
int rtl_rename_reg (ins_chain *startfrom, int old_reg, int new_reg)
{
	ins_chain *tmp;
	int i, r = 0;

	for (tmp=startfrom; tmp; tmp=tmp->next) {
		for (i=0; tmp->in_args[i]; i++) {
			r += rtl_rename_reg_in_arg (tmp->in_args[i], old_reg, new_reg);
		}
		for (i=0; tmp->out_args[i]; i++) {
			r += rtl_rename_reg_in_arg (tmp->out_args[i], old_reg, new_reg);
		}
		if ((tmp->type == INS_END_REG) && (tmp->in_args[0]->regconst == new_reg)) {
			/* just finished */
			break;
		}
	}
	return r;
}
/*}}}*/
/*{{{  int rtl_rename_reg_block (ins_chain *startfrom, ins_chain *endat, int old_reg, int new_reg)*/
/*
 *	renames a register starting from `startfrom' and finishing at `endat'
 *	returns the number changed
 */
int rtl_rename_reg_block (ins_chain *startfrom, ins_chain *endat, int old_reg, int new_reg)
{
	ins_chain *tmp, *next;
	int i, r = 0;

	next = startfrom;
	do {
		tmp = next;
		for (i=0; tmp->in_args[i]; i++) {
			r += rtl_rename_reg_in_arg (tmp->in_args[i], old_reg, new_reg);
		}
		for (i=0; tmp->out_args[i]; i++) {
			r += rtl_rename_reg_in_arg (tmp->out_args[i], old_reg, new_reg);
		}
		next = tmp->next;
	} while (tmp != endat);

	return r;
}
/*}}}*/
/*{{{  ins_chain *rtl_scan_constrain_forward (ins_chain *startfrom, int vreg)*/
/*
 *	scans forwards for a constraint on vreg
 */
ins_chain *rtl_scan_constrain_forward (ins_chain *startfrom, int vreg)
{
	while (startfrom) {
		if ((startfrom->type == INS_END_REG) && (startfrom->in_args[0]->regconst == vreg)) {
			return NULL;
		}
		if ((startfrom->type == INS_CONSTRAIN_REG) && (startfrom->in_args[0]->regconst == vreg)) {
			break;
		}
		startfrom = startfrom->next;
	}
	return startfrom;
}
/*}}}*/
/*{{{  ins_chain *rtl_scan_constrain_backward (ins_chain *startfrom, int vreg)*/
/*
 *	scans backwards for a constraint on `vreg'
 */
ins_chain *rtl_scan_constrain_backward (ins_chain *startfrom, int vreg)
{
	while (startfrom) {
		if ((startfrom->type == INS_START_REG) && (startfrom->in_args[0]->regconst == vreg)) {
			return NULL;
		}
		if ((startfrom->type == INS_CONSTRAIN_REG) && (startfrom->in_args[0]->regconst == vreg)) {
			break;
		}
		startfrom = startfrom->prev;
	}
	return startfrom;
}
/*}}}*/
/*{{{  ins_chain *rtl_scan_unconstrain_forward (ins_chain *startfrom, int vreg)*/
/*
 *	scans forwards for an un-constrain on `vreg'
 */
ins_chain *rtl_scan_unconstrain_forward (ins_chain *startfrom, int vreg)
{
	while (startfrom) {
		if ((startfrom->type == INS_END_REG) && (startfrom->in_args[0]->regconst == vreg)) {
			return NULL;
		}
		if ((startfrom->type == INS_UNCONSTRAIN_REG) && (startfrom->in_args[0]->regconst == vreg)) {
			break;
		}
		startfrom = startfrom->next;
	}
	return startfrom;
}
/*}}}*/
/*{{{  ins_chain *rtl_scan_unconstrain_backward (ins_chain *startfrom, int vreg)*/
/*
 *	scans backwards for an un-constrain on `vreg'
 */
ins_chain *rtl_scan_unconstrain_backward (ins_chain *startfrom, int vreg)
{
	while (startfrom) {
		if ((startfrom->type == INS_START_REG) && (startfrom->in_args[0]->regconst == vreg)) {
			return NULL;
		}
		if ((startfrom->type == INS_UNCONSTRAIN_REG) && (startfrom->in_args[0]->regconst == vreg)) {
			break;
		}
		startfrom = startfrom->prev;
	}
	return startfrom;
}
/*}}}*/
/*{{{  ins_chain *rtl_scan_for_constrain_to (ins_chain *startfrom, ins_chain *endat, int reg)*/
/*
 *	scans in a range for any constraints to a reg
 */
ins_chain *rtl_scan_for_constrain_to (ins_chain *startfrom, ins_chain *endat, int reg)
{
	while (startfrom != endat) {
		if ((startfrom->type == INS_CONSTRAIN_REG) && (startfrom->in_args[1]->regconst == reg)) {
			break;
		}
		startfrom = startfrom->next;
	}
	return startfrom == endat ? NULL : startfrom;
}
/*}}}*/
/*{{{  ins_chain *rtl_scan_start_forward (ins_chain *startfrom, int vreg)*/
/*
 *	scans forwards for a register start
 */
ins_chain *rtl_scan_start_forward (ins_chain *startfrom, int vreg)
{
	while (startfrom) {
		if ((startfrom->type == INS_START_REG) && (startfrom->in_args[0]->regconst == vreg)) {
			break;
		}
		startfrom = startfrom->next;
	}
	return startfrom;
}
/*}}}*/
/*{{{  ins_chain *rtl_scan_start_backward (ins_chain *startfrom, int vreg)*/
/*
 *	scans backwards for a register start
 */
ins_chain *rtl_scan_start_backward (ins_chain *startfrom, int vreg)
{
	while (startfrom) {
		if ((startfrom->type == INS_START_REG) && (startfrom->in_args[0]->regconst == vreg)) {
			break;
		}
		startfrom = startfrom->prev;
	}
	return startfrom;
}
/*}}}*/
/*{{{  ins_chain *rtl_scan_end_forward (ins_chain *startfrom, int vreg)*/
/*
 *	scans forwards for a register end
 */
ins_chain *rtl_scan_end_forward (ins_chain *startfrom, int vreg)
{
	while (startfrom) {
		if ((startfrom->type == INS_END_REG) && (startfrom->in_args[0]->regconst == vreg)) {
			break;
		}
		startfrom = startfrom->next;
	}
	return startfrom;
}
/*}}}*/
/*{{{  ins_chain *rtl_scan_end_backward (ins_chain *startfrom, int vreg)*/
/*
 *	scans backwards for a register end
 */
ins_chain *rtl_scan_end_backward (ins_chain *startfrom, int vreg)
{
	for (; startfrom; startfrom = startfrom->prev) {
		if ((startfrom->type == INS_END_REG) && (startfrom->in_args[0]->regconst == vreg)) {
			break;
		}
	}
	return startfrom;
}
/*}}}*/
/*{{{  ins_chain *rtl_scan_setscc_backward (ins_chain *startfrom, int stopatlab)*/
/*
 *	scans backwards for an implied REG_CC register in output.  if stopatlab is 1
 *	this won't look past a non-returning jump.  if stopatlab is 2, this won't look past a
 *	non-returning jump or a setlabel of some kind.
 */
ins_chain *rtl_scan_setscc_backward (ins_chain *startfrom, int stopatlab)
{
	for (; startfrom; startfrom = startfrom->prev) {
		int i;

		switch (startfrom->type) {
		case INS_SETLABEL:
			if (stopatlab == 2) {
				return NULL;
			}
			break;
		case INS_JUMP:
			if (stopatlab >= 1) {
				return NULL;
			}
			break;
		}
		for (i=0; startfrom->out_args[i]; i++) {
			if ((ArgMode (startfrom->out_args[i]) == ARG_REG) && ArgIsImplied (startfrom->out_args[i]) &&
					(ArgReg (startfrom->out_args[i]) == REG_CC)) {
				/* this one */
				return startfrom;
			}
		}
	}
	return NULL;
}
/*}}}*/
/*{{{  ins_chain *rtl_scan_iclass_backward (ins_chain *startfrom, int iclass)*/
/*
 *	scans backwards for an instruction of a particular type
 */
ins_chain *rtl_scan_iclass_backward (ins_chain *startfrom, int iclass)
{
	for (; startfrom; startfrom = startfrom->prev) {
		if (rtl_classify_instr (startfrom) == iclass) {
			return startfrom;
		}
	}
	return NULL;
}
/*}}}*/
/*{{{  ins_chain *rtl_scan_setsca_backward (ins_chain *startfrom, int stopatlab)*/
/*
 *	scans backwards for an implied REG_CA register in output.  if stopatlab is 1
 *	this won't look past a non-returning jump.  if stopatlab is 2, this won't look past a
 *	non-returning jump or setlabel of some kind.
 */
ins_chain *rtl_scan_setsca_backward (ins_chain *startfrom, int stopatlab)
{
	for (; startfrom; startfrom = startfrom->prev) {
		int i;

		switch (startfrom->type) {
		case INS_SETLABEL:
			if (stopatlab == 2) {
				return NULL;
			}
			break;
		case INS_JUMP:
			if (stopatlab >= 1) {
				return NULL;
			}
			break;
		}
		for (i=0; startfrom->out_args[i]; i++) {
			if ((ArgMode (startfrom->out_args[i]) == ARG_REG) && ArgIsImplied (startfrom->out_args[i]) &&
					(ArgReg (startfrom->out_args[i]) == REG_CA)) {
				/* this one */
				return startfrom;
			}
		}
	}
	return NULL;
}
/*}}}*/
/*{{{  int rtl_instr_setscc (ins_chain *ins)*/
/*
 *	returns non-zero if the instruction given sets REG_CC
 */
int rtl_instr_setscc (ins_chain *ins)
{
	int i;

	for (i=0; ins->out_args[i]; i++) {
		if ((ArgMode (ins->out_args[i]) == ARG_REG) && (ArgReg (ins->out_args[i]) == REG_CC)) {
			return 1;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  int rtl_instr_setsca (ins_chain *ins)*/
/*
 *	returns non-zero if the instruction given sets REG_CA
 */
int rtl_instr_setsca (ins_chain *ins)
{
	int i;

	for (i=0; ins->out_args[i]; i++) {
		if ((ArgMode (ins->out_args[i]) == ARG_REG) && (ArgReg (ins->out_args[i]) == REG_CA)) {
			return 1;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  int rtl_instr_usesca (ins_chain *ins)*/
/*
 *	returns non-zero if the instruction given uses REG_CA (as input)
 */
int rtl_instr_usesca (ins_chain *ins)
{
	int i;

	for (i=0; ins->in_args[i]; i++) {
		if ((ArgMode (ins->in_args[i]) == ARG_REG) && (ArgReg (ins->in_args[i]) == REG_CA)) {
			return 1;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  int rtl_instr_width (ins_chain *ins)*/
/*
 *	returns the width of the instruction based on lowest argument width present (8, 16 or 32)
 */
int rtl_instr_width (ins_chain *ins)
{
	int i;
	int width;

	width = 32;
	for (i=0; ins->in_args[i]; i++) {
		if ((ins->in_args[i]->flags & ARG_IS8BIT) && (width > 8)) {
			width = 8;
		} else if ((ins->in_args[i]->flags & ARG_IS16BIT) && (width > 16)) {
			width = 16;
		}
	}
	for (i=0; ins->out_args[i]; i++) {
		if ((ins->out_args[i]->flags & ARG_IS8BIT) && (width > 8)) {
			width = 8;
		} else if ((ins->out_args[i]->flags & ARG_IS16BIT) && (width > 16)) {
			width = 16;
		}
	}
	return width;
}
/*}}}*/
/*{{{  int rtl_const_bitwidth (int val, int issigned)*/
/*
 *	returns the number of bits required to store a constant value (signed or unsigned)
 */
int rtl_const_bitwidth (int val, int issigned)
{
		unsigned int uval = (unsigned int)val;
	int i = 0;

	if (!issigned) {
		if (!uval) {
			i = 1;
		} else {
			for (i=0; uval && !(uval & 0x80000000); uval <<= 1, i++);
			i = 32 - i;
		}
	} else if (!val) {
		i = 1;
	} else if ((unsigned int)val & 0x80000000) {
		/* signed negative */
		uval <<= 1;
		for (i=0; uval && (uval & 0x80000000); uval <<= 1, i++);
		i = 32 - i;
	} else {
		/* signed positive */
		for (i=0; uval && !(uval & 0x80000000); uval <<= 1, i++);
		i = 32 - i;
	}
#if 0
fprintf (stderr, "rtl_const_bitwidth(): val = 0x%8.8x, issigned=%d, width=%d\n", (unsigned int)val, issigned, i);
#endif
	return i;
}

/*}}}*/
/*{{{  int rtl_classify_instr (ins_chain *ins)*/
/*
 *	classifies an instruction
 */
int rtl_classify_instr (ins_chain *ins)
{
	switch (ins->type) {
	case INS_MOVE:
		if ((ins->in_args[0]->flags & ARG_ISCONST) && ((ins->out_args[0]->flags & ARG_MODEMASK) == ARG_REG)) {
			return ICLASS_LOADCONST;
		}
		break;
	case INS_JUMP:
	case INS_PJUMP:
	case INS_RET:
		return ICLASS_TERMINAL;
	case INS_SETLABEL:
	case INS_SETFLABEL:
		return ICLASS_SETLAB;
	}
	return ICLASS_UNKNOWN;
}
/*}}}*/
/*{{{  ins_chain *rtl_next_instr (ins_chain *ins)*/
/*
 *	returns the next instruction, skipping annotations etc.
 */
ins_chain *rtl_next_instr (ins_chain *ins)
{
	ins = ins->next;
	while (ins && ((ins->type == INS_ANNO) || (ins->type < INS_FIRST) || (ins->type > INS_LAST))) {
		ins = ins->next;
	}
	return ins;
}
/*}}}*/
/*{{{  ins_chain *rtl_prev_instr (ins_chain *ins)*/
/*
 *	returns the previous instruction, skipping annotations etc.
 */
ins_chain *rtl_prev_instr (ins_chain *ins)
{
	ins = ins->prev;
	while (ins && ((ins->type == INS_ANNO) || (ins->type < INS_FIRST) || (ins->type > INS_LAST))) {
		ins = ins->prev;
	}
	return ins;
}
/*}}}*/
/*{{{  ins_chain *rtl_last_instr (rtl_chain *rtl)*/
/*
 *	returns the last instruction in an RTL chain, skipping annotations, etc.
 */
ins_chain *rtl_last_instr (rtl_chain *rtl)
{
	ins_chain *ins = NULL;

	if (rtl->type != RTL_CODE) {
		return NULL;
	}
	ins = rtl->u.code.tail;
	while (ins && ((ins->type == INS_ANNO) || (ins->type == INS_CLEANUP) || (ins->type < INS_FIRST) || (ins->type > INS_LAST))) {
		ins = ins->prev;
	}
	return ins;
}
/*}}}*/
/*{{{  static void rtl_procinf_setlabrefs (procinf *pinfo, void *arg)*/
/*
 *	called for each entry in the procinf table, sets label refcounts for internal labels
 *	only to avoid losing them.
 */
static void rtl_procinf_setlabrefs (procinf *pinfo, void *arg)
{
	int *label_refcount = (int *)arg;
	int i;

	if (!pinfo->is_local || !pinfo->is_proc) {
		return;
	}
	for (i=0; i<pinfo->refs_cur; i++) {
		if (pinfo->refs[i]->is_internal && (pinfo->refs[i]->eplab >= 0)) {
			label_refcount[pinfo->refs[i]->eplab]++;
		}
	}
	return;
}
/*}}}*/
/*{{{  static int rtl_count_code_instr (rtl_chain *rtl, int instr)*/
/*
 *	counts up the number of particular instructions in generated code
 */
static int rtl_count_code_instr (rtl_chain *rtl, int instr)
{
	int count = 0;

	for (; rtl; rtl = rtl->next) {
		if (rtl->type == RTL_CODE) {
			ins_chain *ins;
			
			for (ins = rtl->u.code.head; ins; ins = ins->next) {
				if (ins->type == instr) {
					count++;
				}
			}

		}
	}

	return count;
}
/*}}}*/
/*{{{  static int rtl_find_code_instr (rtl_chain *rtl, int instr, ins_chain **rarry, int *rptr)*/
/*
 *	retrieves instances of an instruction in generated code (pointers to)
 *	returns number found
 */
static int rtl_find_code_instr (rtl_chain *rtl, int instr, ins_chain **rarry, int *rptr)
{
	int count = 0;

	for (; rtl; rtl = rtl->next) {
		if (rtl->type == RTL_CODE) {
			ins_chain *ins;
			
			for (ins = rtl->u.code.head; ins; ins = ins->next) {
				if (ins->type == instr) {
					rarry[*rptr] = ins;
					*rptr = *rptr + 1;
					count++;
				}
			}

		}
	}
	return count;
}
/*}}}*/
/*{{{  int rtl_cleanup_flabels (rtl_chain *rtl_code)*/
/*
 *	cleans up SETFLABELs that don't have any references.
 *	returns number of SETFLABELs removed, -1 on error.
 */
int rtl_cleanup_flabels (rtl_chain *rtl_code)
{
	ins_chain **flabel_links;		/* links to where SETFLABEL instructions are */
	int *flabel_refcounts;
	int flabel_ptr = -1;
	int i, num_flabs;
	int rcode = -1;

	num_flabs = rtl_count_code_instr (rtl_code, INS_SETFLABEL);
#if 0
fprintf (stderr, "rtl_cleanup_flabels(): %d SETFLABEL instances\n", num_flabs);
#endif
	if (num_flabs > 0) {
		flabel_links = (ins_chain **)smalloc (num_flabs * sizeof (ins_chain *));
		flabel_refcounts = (int *)smalloc (num_flabs * sizeof (int));
		for (i=0; i<num_flabs; i++) {
			flabel_links[i] = NULL;
			flabel_refcounts[i] = 0;
		}
		/* put these in first */
		i = 0;
		rtl_find_code_instr (rtl_code, INS_SETFLABEL, flabel_links, &i);
		if (i != num_flabs) {
			fprintf (stderr, "%s: error: inconsistent number of SETFLABELs (%d vs. %d)\n", progname, i, num_flabs);
			sfree (flabel_links);
			sfree (flabel_refcounts);
			return -1;
		}
	} else {
		return 0;
	}

	flabel_ptr = -1;
	for (; rtl_code; rtl_code = rtl_code->next) {
		if (rtl_code->type == RTL_CODE) {
			ins_chain *tmp_ins;

			for (tmp_ins = rtl_code->u.code.head; tmp_ins; tmp_ins=tmp_ins->next) {
				switch (tmp_ins->type) {
					/*{{{  INS_SETFLABEL: check that this matches the expected one in flabel_links[]*/
				case INS_SETFLABEL:
					if (tmp_ins->in_args[0] && ((tmp_ins->in_args[0]->flags & ARG_MODEMASK) == ARG_FLABEL)) {
						flabel_ptr++;
						if (flabel_links[flabel_ptr] != tmp_ins) {
							fprintf (stderr, "%s: error: confusion in SETFLABEL organisation!\n", progname);
							rcode = -1;
							goto get_out_of_here;
						}
						/* flabel_ptr points at the *last seen* label */
					}
					break;
					/*}}}*/
					/*{{{  INS_CJUMP: conditional jump, check for FLABELs and BLABELs*/
				case INS_CJUMP:
					if (tmp_ins->in_args[1] && (ArgMode (tmp_ins->in_args[1]) == ARG_FLABEL)) {
						/* forward label reference */
						int j;

						for (j=flabel_ptr + 1; j<num_flabs; j++) {
							if (flabel_links[j]->in_args[0] && (ArgFLabel (flabel_links[j]->in_args[0]) == ArgFLabel (tmp_ins->in_args[1]))) {
								/* reference to this one */
								flabel_refcounts[j]++;
								break;		/* for() */
							}
						}
						if (j == num_flabs) {
							fprintf (stderr, "%s: error: undefined reference to FLABEL\n", progname);
							rcode = -1;
							goto get_out_of_here;
						}
					} else if (tmp_ins->in_args[1] && (ArgMode (tmp_ins->in_args[1]) == ARG_BLABEL)) {
						/* backward label reference */
						int j;

						for (j=flabel_ptr; j>=0; j--) {
							if (flabel_links[j]->in_args[0] && (ArgFLabel (flabel_links[j]->in_args[0]) == ArgFLabel (tmp_ins->in_args[1]))) {
								/* reference to this one */
								flabel_refcounts[j]++;
								break;		/* for() */
							}
						}
						if (j < 0) {
							fprintf (stderr, "%s: error: undefined reference to BLABEL\n", progname);
							rcode = -1;
							goto get_out_of_here;
						}
					}
					break;
					/*}}}*/
					/*{{{  INS_PJUMP, INS_JUMP: unconditional jump, same as above*/
				case INS_PJUMP:
				case INS_JUMP:
					if (tmp_ins->in_args[0] && (ArgMode (tmp_ins->in_args[0]) == ARG_FLABEL)) {
						/* forward label reference */
						int j;

						for (j=flabel_ptr + 1; j<num_flabs; j++) {
							if (flabel_links[j]->in_args[0] && (ArgFLabel (flabel_links[j]->in_args[0]) == ArgFLabel (tmp_ins->in_args[0]))) {
								/* reference to this one */
								flabel_refcounts[j]++;
								break;		/* for() */
							}
						}
						if (j == num_flabs) {
							fprintf (stderr, "%s: error: undefined reference to FLABEL\n", progname);
							rcode = -1;
							goto get_out_of_here;
						}
					} else if (tmp_ins->in_args[0] && (ArgMode (tmp_ins->in_args[0]) == ARG_BLABEL)) {
						/* backward label reference */
						int j;

						for (j=flabel_ptr; j>=0; j--) {
							if (flabel_links[j]->in_args[0] && (ArgFLabel (flabel_links[j]->in_args[0]) == ArgFLabel (tmp_ins->in_args[0]))) {
								/* reference to this one */
								flabel_refcounts[j]++;
								break;		/* for() */
							}
						}
						if (j < 0) {
							fprintf (stderr, "%s: error: undefined reference to BLABEL\n", progname);
							rcode = -1;
							goto get_out_of_here;
						}
					}
					break;
					/*}}}*/
					/*{{{  default: look for FLABEL references*/
				default:
					for (i=0; tmp_ins->in_args[i]; i++) {
						int j;

						switch (ArgMode (tmp_ins->in_args[i])) {
						case ARG_FLABEL:
							/* forward label reference */

							for (j=flabel_ptr + 1; j<num_flabs; j++) {
								if (flabel_links[j]->in_args[0] && (ArgFLabel (flabel_links[j]->in_args[0]) == ArgFLabel (tmp_ins->in_args[i]))) {
									/* reference to this one */
									flabel_refcounts[j]++;
									break;		/* for() */
								}
							}
							if (j == num_flabs) {
								fprintf (stderr, "%s: error: undefined reference to FLABEL\n", progname);
								rcode = -1;
								goto get_out_of_here;
							}
							break;
						case ARG_BLABEL:
							/* backward label reference */

							for (j=flabel_ptr; j>=0; j--) {
								if (flabel_links[j]->in_args[0] && (ArgFLabel (flabel_links[j]->in_args[0]) == ArgFLabel (tmp_ins->in_args[i]))) {
									/* reference to this one */
									flabel_refcounts[j]++;
									break;		/* for() */
								}
							}
							if (j < 0) {
								fprintf (stderr, "%s: error: undefined reference to BLABEL\n", progname);
								rcode = -1;
								goto get_out_of_here;
							}
							break;
						}
					}
					for (i=0; tmp_ins->out_args[i]; i++) {
						int j;

						switch (ArgMode (tmp_ins->out_args[i])) {
						case ARG_FLABEL:
							/* forward label reference */

							for (j=flabel_ptr + 1; j<num_flabs; j++) {
								if (flabel_links[j]->out_args[0] && (ArgFLabel (flabel_links[j]->out_args[0]) == ArgFLabel (tmp_ins->out_args[i]))) {
									/* reference to this one */
									flabel_refcounts[j]++;
									break;		/* for() */
								}
							}
							if (j == num_flabs) {
								fprintf (stderr, "%s: error: undefined reference to FLABEL\n", progname);
								rcode = -1;
								goto get_out_of_here;
							}
							break;
						case ARG_BLABEL:
							/* backward label reference */

							for (j=flabel_ptr; j>=0; j--) {
								if (flabel_links[j]->out_args[0] && (ArgFLabel (flabel_links[j]->out_args[0]) == ArgFLabel (tmp_ins->out_args[i]))) {
									/* reference to this one */
									flabel_refcounts[j]++;
									break;		/* for() */
								}
							}
							if (j < 0) {
								fprintf (stderr, "%s: error: undefined reference to BLABEL\n", progname);
								rcode = -1;
								goto get_out_of_here;
							}
							break;
						}
					}
					break;
					/*}}}*/
				}
			}
		}
	}
	rcode = 0;
	for (i=0; i<num_flabs; i++) {
#if 0
fprintf (stderr, "flabel_cleanup: %d references..\n", flabel_refcounts[i]);
#endif
		if (!flabel_refcounts[i]) {
			ins_chain *tins = rtl_prev_instr (flabel_links[i]);
			int do_trash = 0;

#if 0
fprintf (stderr, "flabel_cleanup: unreferenced flabel, looking..\n");
#endif
			if (!tins) {
				/* means this occurs effective at the start of an RTL code-block, see what's before it */
				rtl_chain *rwalk;
				int stopwalk = 0;

#if 0
fprintf (stderr, "flabel_cleanup: flabel at start of RTL, scanning backwards..\n");
#endif
				for (rwalk = flabel_links[i]->rtl->prev; !stopwalk && rwalk; rwalk = rwalk->prev) {
					switch (rwalk->type) {
					case RTL_CODE:
						/* if last instruction is a jump, can't reach this from above */
						tins = rtl_last_instr (rwalk);
						if ((tins->type == INS_JUMP) || (tins->type == INS_PJUMP)) {
							/* can't reach this from above, no references, so dead */
							do_trash = 1;
						}
						stopwalk = 1;
						break;
					case RTL_SETNAMEDLABEL:
					case RTL_PUBLICSETNAMEDLABEL:
						/* entry points, reachable */
						stopwalk = 1;
						break;
					case RTL_DATA:
						/* possibly reachable */
						stopwalk = 1;
						break;
					default:
						/* assume benign, skip through */
						break;
					}
				}
				if (!rwalk && !stopwalk) {
					/* walked off top of RTL, assume first thing in program, no references, so dead */
					do_trash = 1;
				}
#if 0
fprintf (stderr, "flabel_cleanup: scanned_backwards, rwalk = %p, stopwalk = %d, do_trash = %d\n", rwalk, stopwalk, do_trash);
#endif
			} else if ((tins->type == INS_JUMP) || (tins->type == INS_PJUMP)) {
				/* means we can't reach this from above, and no references, so dead */
				do_trash = 1;
			}

			if (do_trash) {
				ins_chain *walk;
				int stop = 0;
				int x;

#if 0
fprintf (stderr, "%s: no references to unreachable SETFLABEL %d!\n", progname, ArgFLabel (flabel_links[i]->in_args[0]));
#endif
				flabel_links[i]->type = INS_CLEANUP;
				rcode++;
				for (walk = flabel_links[i]->next; walk && !stop; walk = walk->next) {
					switch (walk->type) {
					case INS_SETLABEL:
					case INS_SETFLABEL:
						/* hit a label */
						stop = 1;
						break;
					case INS_ANNO:
						/* leave annotations here (for now..) */
						break;
					default:
						/*{{{  unreachable code, mark for cleanup and find any other flabel references */
						for (x=0; walk->in_args[x]; x++) {
							int j;

							switch (ArgMode (walk->in_args[x])) {
							case ARG_FLABEL:
								/* forward label reference */

								for (j=i+1; j<num_flabs; j++) {
									if (flabel_links[j]->in_args[0] && (ArgFLabel (flabel_links[j]->in_args[0]) == ArgFLabel (walk->in_args[x]))) {
										/* reference to this one */
										flabel_refcounts[j]--;
										break;		/* for() */
									}
								}
								if (j == num_flabs) {
									fprintf (stderr, "%s: error: undefined reference to FLABEL\n", progname);
									rcode = -1;
									goto get_out_of_here;
								}
								break;
							case ARG_BLABEL:
								/* backward label reference */

								for (j=i; j>=0; j--) {
									if (flabel_links[j]->in_args[0] && (ArgFLabel (flabel_links[j]->in_args[0]) == ArgFLabel (walk->in_args[x]))) {
										/* reference to this one */
										flabel_refcounts[j]--;
										break;		/* for() */
									}
								}
								if (j < 0) {
									fprintf (stderr, "%s: error: undefined reference to BLABEL\n", progname);
									rcode = -1;
									goto get_out_of_here;
								}
								break;
							}
						}
						walk->type = INS_CLEANUP;
						rcode++;
						/*}}}*/
						break;
					}
				}
			}
		}
	}

get_out_of_here:
	if (num_flabs > 0) {
		sfree (flabel_links);
		sfree (flabel_refcounts);
	}

	return rcode;
}
/*}}}*/
/*{{{  ins_chain *rtl_cleanup_code (ins_chain *ins)*/
/*
 *	removes code marked with INS_CLEANUP
 */
ins_chain *rtl_cleanup_code (ins_chain *ins)
{
	ins_chain *tmp, *head, *holding;

	head = ins;
	tmp = head;
	while (tmp) {
		if (tmp->type == INS_CLEANUP) {
			if (tmp == head) {
				head = tmp->next;
				tmp->rtl->u.code.head = head;
			} else {
				tmp->prev->next = tmp->next;
			}
			if (tmp->next) {
				tmp->next->prev = tmp->prev;
			} else if (tmp->prev) {
				tmp->prev->next = NULL;
				tmp->rtl->u.code.tail = tmp->prev;
			}
			holding = tmp->next;
			rtl_free_instr (tmp);
			tmp = holding;
		} else {
			tmp = tmp->next;
		}
	}
	return head;
}
/*}}}*/
/*{{{  rtl_chain *rtl_cleanup_code_all (rtl_chain *rtl)*/
/*
 *	removes code marked with INS_CLEANUP
 */
rtl_chain *rtl_cleanup_code_all (rtl_chain *rtl)
{
	rtl_chain *head;

	head = rtl;
	while (rtl) {
		if (rtl->type == RTL_CODE) {
			rtl->u.code.head = rtl_cleanup_code (rtl->u.code.head);
		}
		rtl = rtl->next;
	}

	return head;
}
/*}}}*/
/*{{{  int rtl_link_jumps (rtl_chain *rtl_code)*/
/*
 *	links jump arguments to code and places label references in labels
 *	(this may be non too pleasant, but it does the job)
 */
int rtl_link_jumps (rtl_chain *rtl_code)
{
	ins_chain **label_links;
	int *label_refcounts, *label_drefs;
	ins_arg ***label_references;		/* the first two elements of the inner arrays (label_references[n][0,1]) are
						 * the current and max number of *data items* in the array (see code) */
	ins_chain ***label_refins;		/* referencing instructions (same as above but without the first two) */
	ins_arg *jref_arg[10];
	ins_arg *lab_refarg;
	ins_chain *lab_refins;
	int the_lab[10];
	int n_jref_arg;
	int first_lab, last_lab;
	ins_chain *tmp_ins;
	int i, j;
	int rcode;

	first_lab = 0;
	last_lab = get_last_lab ();

	label_links = (ins_chain **)smalloc ((last_lab + 1) * sizeof (ins_chain *));
	label_references = (ins_arg ***)smalloc ((last_lab + 1) * sizeof (ins_arg **));
	label_refins = (ins_chain ***)smalloc ((last_lab + 1) * sizeof (ins_chain **));
	label_refcounts = (int *)smalloc ((last_lab + 1) * sizeof (int));
	label_drefs = (int *)smalloc ((last_lab + 1) * sizeof (int));
	for (i=0; i<=last_lab; i++) {
		label_links[i] = NULL;
		label_references[i] = NULL;
		label_refins[i] = NULL;
		label_refcounts[i] = 0;
		label_drefs[i] = 0;
	}
	rcode = 0;
	for (i=0; i<10; i++) {
		jref_arg[i] = NULL;
		the_lab[i] = -1;
	}
	n_jref_arg = 0;
	while (rtl_code) {
		switch (rtl_code->type) {
			/*{{{  CODE*/
		case RTL_CODE:
			for (tmp_ins = rtl_code->u.code.head; tmp_ins; tmp_ins=tmp_ins->next) {
				the_lab[0] = -1;
				n_jref_arg = 0;
				switch (tmp_ins->type) {
					/*{{{  INS_SETLABEL: mark in the_lab[0]*/
				case INS_SETLABEL:
					if (tmp_ins->in_args[0] && ((tmp_ins->in_args[0]->flags & ARG_MODEMASK) == ARG_LABEL)) {
						the_lab[0] = tmp_ins->in_args[0]->regconst;
					}
					break;
					/*}}}*/
					/*{{{  INS_CJUMP: if regular label, mark in the_lab[0] and jref_arg[0]; if FLABEL, increment refcount*/
				case INS_CJUMP:
					if (tmp_ins->in_args[1] && ((tmp_ins->in_args[1]->flags & ARG_MODEMASK) == ARG_LABEL)) {
						the_lab[0] = tmp_ins->in_args[1]->regconst;
						jref_arg[0] = tmp_ins->in_args[1];
						n_jref_arg = 1;
					}
					break;
					/*}}}*/
					/*{{{  INS_PJUMP, INS_JUMP: unconditional jump, same as above*/
				case INS_PJUMP:
				case INS_JUMP:
					if (tmp_ins->in_args[0] && (ArgMode (tmp_ins->in_args[0]) == ARG_LABEL)) {
						the_lab[0] = tmp_ins->in_args[0]->regconst;
						jref_arg[0] = tmp_ins->in_args[0];
						n_jref_arg = 1;
					}
					break;
					/*}}}*/
					/*{{{  default: look for label references and fill up jref_arg[], the_lab[]*/
				default:
					for (i=0; tmp_ins->in_args[i]; i++) {
						switch (ArgMode (tmp_ins->in_args[i])) {
						case ARG_LABEL:
							jref_arg[n_jref_arg] = tmp_ins->in_args[i];
							the_lab[n_jref_arg] = jref_arg[n_jref_arg]->regconst;
							n_jref_arg++;
							break;
						}
					}
					for (i=0; tmp_ins->out_args[i]; i++) {
						switch (ArgMode (tmp_ins->out_args[i])) {
						case ARG_LABEL:
							jref_arg[n_jref_arg] = tmp_ins->out_args[i];
							the_lab[n_jref_arg] = jref_arg[n_jref_arg]->regconst;
							n_jref_arg++;
							break;
						}
					}
					break;
					/*}}}*/
				}
				if (the_lab[0] == -1) {
					continue;	/* to for() */
				}
				for (i=0; i<n_jref_arg; i++) {
					if ((the_lab[i] < first_lab) || (the_lab[i] > last_lab)) {
						fprintf (stderr, "%s: error: occurence of label %d outside bounds [%d,%d]\n", progname, the_lab[i], first_lab, last_lab);
						rcode = -1;
						goto get_out_of_here;
					}
				}
				if (tmp_ins->type == INS_SETLABEL) {
					/*{{{  make labrefs for SETLABEL (put in out_args[0])*/
					if (label_links[the_lab[0]]) {
						fprintf (stderr, "%s: error: label %d has duplicate definition\n", progname, the_lab[0]);
						rcode = -1;
						goto get_out_of_here;
					}
					label_links[the_lab[0]] = tmp_ins;
					/* setup references for it */
					tmp_ins->out_args[0] = new_ins_arg ();
					tmp_ins->out_args[0]->flags = ARG_LABREFS;
					tmp_ins->out_args[0]->regconst = (int)NULL;
					/* resolve any references */
					if (label_references[the_lab[0]]) {
						j = *(int *)(label_references[the_lab[0]]);
						for (i=0; i<j; i++) {
							lab_refarg = label_references[the_lab[0]][i+2];
							lab_refarg->flags = ARG_INSLABEL | (lab_refarg->flags & ~ARG_MODEMASK);
							lab_refarg->regconst = (int)tmp_ins;
							lab_refins = label_refins[the_lab[0]][i];
							SetArgLabRefs(tmp_ins->out_args[0], rtl_add_labref (ArgLabRefs(tmp_ins->out_args[0]), lab_refins));
						}
						sfree (label_references[the_lab[0]]);
						label_references[the_lab[0]] = NULL;
						sfree (label_refins[the_lab[0]]);
						label_refins[the_lab[0]] = NULL;
					}
					/*}}}*/
				} else {
					/*{{{  resolve any references to regular labels*/
					while (n_jref_arg) {
						n_jref_arg--;
						if (!label_links[the_lab[n_jref_arg]]) {
							/* not seen label yet -- queue up stuff */
							if (tmp_ins->type == INS_PJUMP) {
								/* forward reference, demote to regular jump */
								tmp_ins->type = INS_JUMP;
							}
							if (!label_references[the_lab[n_jref_arg]]) {
								/* create a new one */
								label_references[the_lab[n_jref_arg]] = (ins_arg **)smalloc (12 * sizeof (ins_arg *));
								*(int *)(label_references[the_lab[n_jref_arg]]) = 0;
								*(int *)(label_references[the_lab[n_jref_arg]] + 1) = 10;
								label_refins[the_lab[n_jref_arg]] = (ins_chain **)smalloc (10 * sizeof (ins_chain *));
							} else if (*(int *)(label_references[the_lab[n_jref_arg]]) == *(int *)(label_references[the_lab[n_jref_arg]] + 1)) {
								/* make older one larger */
								j = *(int *)(label_references[the_lab[n_jref_arg]] + 1);
								label_references[the_lab[n_jref_arg]] = (ins_arg **)srealloc ((void *)label_references[the_lab[n_jref_arg]], \
									((j + 2) * sizeof (ins_arg *)), ((j + 12) * sizeof (ins_arg *)));
								*(int *)(label_references[the_lab[n_jref_arg]] + 1) += 10;
								label_refins[the_lab[n_jref_arg]] = (ins_chain **)srealloc ((void *)label_refins[the_lab[n_jref_arg]], \
									j * sizeof (ins_chain *), (j + 10) * sizeof (ins_chain *));
							}
							j = (*(int *)(label_references[the_lab[n_jref_arg]]))++;
							label_refcounts[the_lab[n_jref_arg]]++;
							label_references[the_lab[n_jref_arg]][j+2] = jref_arg[n_jref_arg];
							label_refins[the_lab[n_jref_arg]][j] = tmp_ins;
						} else {
							/* have seen label, add this target to label references, etc. */
							label_refcounts[the_lab[n_jref_arg]]++;
							jref_arg[n_jref_arg]->flags = ARG_INSLABEL | (jref_arg[n_jref_arg]->flags & ~ARG_MODEMASK);
							jref_arg[n_jref_arg]->regconst = (int)label_links[the_lab[n_jref_arg]];
							lab_refins = label_links[the_lab[n_jref_arg]];
							SetArgLabRefs(lab_refins->out_args[0], rtl_add_labref (ArgLabRefs(lab_refins->out_args[0]), tmp_ins));
						}
					}
					/*}}}*/
				}
			}
			break;
			/*}}}*/
			/*{{{  RDATA*/
		case RTL_RDATA:
			the_lab[0] = rtl_code->u.rdata.label;
			if ((the_lab[0] < first_lab) || (the_lab[0] > last_lab)) {
				fprintf (stderr, "%s: error: occurence of label %d outside bounds [%d,%d]\n", progname, the_lab[0], first_lab, last_lab);
				rcode = -1;
				goto get_out_of_here;
			}
			label_drefs[the_lab[0]]++;
			break;
			/*}}}*/
			/*{{{  XDATA*/
		case RTL_XDATA:
			{
				tdfixup_t *walk;

				if (rtl_code->u.xdata.label >= 0) {
					the_lab[0] = rtl_code->u.xdata.label;
					if ((the_lab[0] < first_lab) || (the_lab[0] > last_lab)) {
						fprintf (stderr, "%s: error: occurence of label %d outside bounds [%d,%d]\n", progname, the_lab[0], first_lab, last_lab);
						rcode = -1;
						goto get_out_of_here;
					}
					label_drefs[the_lab[0]]++;
				}
				for (walk=rtl_code->u.xdata.fixups; walk; walk = walk->next) {
					the_lab[0] = walk->otherlab;
					if ((the_lab[0] < first_lab) || (the_lab[0] > last_lab)) {
						fprintf (stderr, "%s: error: occurence of label %d outside bounds [%d,%d]\n", progname, the_lab[0], first_lab, last_lab);
						rcode = -1;
						goto get_out_of_here;
					}
					label_refcounts[the_lab[0]]++;
				}
			}
			break;
			/*}}}*/
			/*{{{  CODEMAP*/
		case RTL_CODEMAP:
			{
				procinf *pinf = rtl_code->u.codemap.pinf;

				/* like RDATA, we just mark the label as being defined -- without actually linking anything */
				the_lab[0] = pinf->maplab;
				if ((the_lab[0] < first_lab) || (the_lab[0] > last_lab)) {
					fprintf (stderr, "%s: error: occurence of label %d outside bounds [%d,%d]\n", progname, the_lab[0], first_lab, last_lab);
					rcode = -1;
					goto get_out_of_here;
				}
				label_drefs[the_lab[0]]++;
			}
			break;
			/*}}}*/
			/*{{{  default*/
		default:
			break;
			/*}}}*/
		}
		rtl_code = rtl_code->next;
	}

	/*{{{  incorporate any code-map information*/
	procinf_iterate (rtl_procinf_setlabrefs, (void *)label_refcounts);
	/*}}}*/
get_out_of_here:
	for (i=first_lab; i<=last_lab; i++) {
		if (label_references[i]) {
			if (!label_drefs[i]) {
				fprintf (stderr, "%s: warning: %d undefined references to label %d.\n", progname, *(int *)(label_references[i]), i);
			}
			sfree (label_references[i]);
			label_references[i] = NULL;
		}
		if (label_links[i] && !label_refcounts[i]) {
			/* remove the label (can analyse unreachable code later) */
			rtl_remove_instr (label_links[i]);
			label_links[i] = NULL;
		}
	}
	sfree (label_links);
	sfree (label_refcounts);
	sfree (label_drefs);
	sfree (label_references);
	return rcode;
}
/*}}}*/
/*{{{  int rtl_destructive_sequence (ins_chain *first, ins_chain *last)*/
/*
 *	determines if a sequence of instructions is `destructive' -- does not take memory into account
 */
int rtl_destructive_sequence (ins_chain *first, ins_chain *last)
{
	ins_chain *tmp;
	ins_arg *tmp_ins;
	int i;

	for (tmp=first; tmp != last->next; tmp = tmp->next) {
		switch (tmp->type) {
		case INS_CALL:
		case INS_JUMP:
		case INS_PJUMP:
		case INS_CJUMP:
		case INS_RET:
			return 1;
			break;
		}
		for (i=0; tmp->in_args[i]; i++) {
			tmp_ins = tmp->in_args[i];
			if (((tmp_ins->flags & ARG_MODEMASK) == ARG_REG) && (tmp_ins->regconst < FIRST_VIRTUAL_REG)) {
				return 1;
			}
		}
		for (i=0; tmp->out_args[i]; i++) {
			tmp_ins = tmp->out_args[i];
			if (((tmp_ins->flags & ARG_MODEMASK) == ARG_REG) && (tmp_ins->regconst < FIRST_VIRTUAL_REG)) {
				return 1;
			}
		}
	}
	return 0;
}
/*}}}*/
/*{{{  int rtl_compare_args (ins_arg *arg1, ins_arg *arg2)*/
/*
 *	compares 2 arguments for equality
 */
int rtl_compare_args (ins_arg *arg1, ins_arg *arg2)
{
	ins_sib_arg *sib1, *sib2;

	if (arg1->flags != arg2->flags) {
		return 0;
	}
	if (arg1->flags & ARG_DISP) {
		if (arg1->disp != arg2->disp) {
			return 0;
		}
	}
	switch (arg1->flags & ARG_MODEMASK) {
	case ARG_REG:
	case ARG_CONST:
	case ARG_REGIND:
	case ARG_COND:
	case ARG_LABEL:
	case ARG_INSLABEL:
	case ARG_FREG:
		return (arg1->regconst == arg2->regconst);
		break;
	case ARG_NAMEDLABEL:
	case ARG_TEXT:
		return (!strcmp ((char *)arg1->regconst, (char *)arg2->regconst));
		break;
	case ARG_REGINDSIB:
		sib1 = (ins_sib_arg *)arg1->regconst;
		sib2 = (ins_sib_arg *)arg2->regconst;
		return ((sib1->base == sib2->base) && (sib1->index == sib2->index) &&
			(sib1->disp == sib2->disp) && (sib1->scale == sib2->scale));
		break;
	case ARG_FLABEL:
	case ARG_BLABEL:
		/* disallow -- they're very context sensitive */
		return 0;
		break;
	}
	return 0;
}
/*}}}*/
/*{{{  int rtl_arg_in_sequence (ins_arg *arg, ins_chain *first, ins_chain *last)*/
/*
 *	determines if `arg' occurs in some sequence
 */
int rtl_arg_in_sequence (ins_arg *arg, ins_chain *first, ins_chain *last)
{
	ins_chain *tmp;
	int i;

	for (tmp=first; tmp != last->next; tmp = tmp->next) {
		for (i=0; tmp->in_args[i]; i++) {
			if (rtl_compare_args (tmp->in_args[i], arg)) {
				return 1;
			}
		}
		for (i=0; tmp->out_args[i]; i++) {
			if (rtl_compare_args (tmp->out_args[i], arg)) {
				return 1;
			}
		}
	}
	return 0;
}
/*}}}*/
/*{{{  int rtl_result_of_const_compare (int c1, int c2, int cond)*/
/*
 *	returns the result of a constant comparison (ie, whether a jump on `cond' will branch)
 */
int rtl_result_of_const_compare (int c1, int c2, int cond)
{
	switch (cond) {
	case CC_NONE:
		fprintf (stderr, "%s: fatal: conditional jump on null condition\n", progname);
		exit (EXIT_FAILURE);
		break;
	case CC_B:		/* below */
		return ((unsigned int)c1 < (unsigned int)c2);
	case CC_AE:		/* above or equal */
		return ((unsigned int)c1 >= (unsigned int)c2);
	case CC_E:		/* equal */
		return (c1 == c2);
	case CC_NE:		/* not equal */
		return (c1 != c2);
	case CC_BE:		/* below or equal */
		return ((unsigned int)c1 <= (unsigned int)c2);
	case CC_A:		/* above */
		return ((unsigned int)c1 > (unsigned int)c2);
	case CC_LT:		/* less than */
		return (c1 < c2);
	case CC_GE:		/* greater or equal */
		return (c1 >= c2);
	case CC_LE:		/* less than or equal */
		return (c1 <= c2);
	case CC_GT:		/* greater than */
		return (c1 > c2);
	default:
		fprintf (stderr, "%s: fatal: conditional jump on irrelevant condition\n", progname);
		return 0;
		break;
	}
}
/*}}}*/
/*{{{  ins_labrefs *rtl_add_labref (ins_labrefs *labrefs, ins_chain *ins)*/
/*
 *	adds an instruction to the list `labrefs' and returns it
 */
ins_labrefs *rtl_add_labref (ins_labrefs *labrefs, ins_chain *ins)
{
	if (!labrefs) {
		labrefs = (ins_labrefs *)smalloc (sizeof (ins_labrefs));
		labrefs->ref_cur = labrefs->ref_max = 0;
		labrefs->refs = NULL;
	}
	if (labrefs->ref_cur == labrefs->ref_max) {
		labrefs->refs = (ins_chain **)srealloc (labrefs->refs, labrefs->ref_max * sizeof (ins_chain *), (labrefs->ref_max + 10) * sizeof (ins_chain *));
		labrefs->ref_max += 10;
	}
	labrefs->refs[labrefs->ref_cur] = ins;
	(labrefs->ref_cur)++;
	return labrefs;
}
/*}}}*/
/*{{{  int rtl_del_labref (ins_labrefs *labrefs, ins_chain *ins)*/
/*
 *	removes a label reference (doesn't moan if not here)
 */
int rtl_del_labref (ins_labrefs *labrefs, ins_chain *ins)
{
	int i, j;

	if (!labrefs) {
		return 0;
	}
	for (i=0; i<labrefs->ref_cur; i++) {
		if (labrefs->refs[i] == ins) {
			break;
		}
	}
	if (i == labrefs->ref_cur) {
		/* not here */
		return 0;
	}
	(labrefs->ref_cur)--;
	for (j=i; j<labrefs->ref_cur; j++) {
		labrefs->refs[j] = labrefs->refs[j+1];
	}
	labrefs->refs[labrefs->ref_cur] = NULL;
	return 1;
}
/*}}}*/
/*{{{  void rtl_removing_labref (ins_chain *ins)*/
/*
 *	looks at something involving a label reference and removes it from the label in question
 */
void rtl_removing_labref (ins_chain *ins)
{
	int i;
	ins_chain *tmp_ins;
	ins_labrefs *tmp_refs;

	for (i=0; ins->in_args[i]; i++) {
		if ((ins->in_args[i]->flags & ARG_MODEMASK) == ARG_INSLABEL) {
			tmp_ins = (ins_chain *)(ins->in_args[i]->regconst);
			if (tmp_ins->out_args[0]) {
				tmp_refs = (ins_labrefs *)(tmp_ins->out_args[0]->regconst);
				rtl_del_labref (tmp_refs, ins);
			}
		}
	}
	for (i=0; ins->out_args[i]; i++) {
		if ((ins->out_args[i]->flags & ARG_MODEMASK) == ARG_INSLABEL) {
			tmp_ins = (ins_chain *)(ins->out_args[i]->regconst);
			if (tmp_ins->out_args[0]) {
				tmp_refs = (ins_labrefs *)(tmp_ins->out_args[0]->regconst);
				rtl_del_labref (tmp_refs, ins);
			}
		}
	}
	return;
}
/*}}}*/

#define MAX_LABEL_GLOB 50

/*{{{  void rtl_rename_label (int from_lab, ins_chain *from_lab_def, int to_lab, ins_chain *to_lab_def)*/
/*
 *	renames a label
 */
void rtl_rename_label (int from_lab, ins_chain *from_lab_def, int to_lab, ins_chain *to_lab_def)
{
	int i, set;
	ins_chain *tmp_ins;

	while ((ArgLabRefs (from_lab_def->out_args[0]))->ref_cur) {
		tmp_ins = (ArgLabRefs (from_lab_def->out_args[0]))->refs[0];
		SetArgLabRefs (to_lab_def->out_args[0], rtl_add_labref (ArgLabRefs(to_lab_def->out_args[0]), tmp_ins));
		rtl_del_labref (ArgLabRefs(from_lab_def->out_args[0]), tmp_ins);
		/* okay, sort references in tmp_ins */
		set = 0;
		for (i=0; tmp_ins->in_args[i] && !set; i++) {
			if (ArgMode (tmp_ins->in_args[i]) == ARG_INSLABEL) {
				if (ArgInsLab (tmp_ins->in_args[i]) == from_lab_def) {
					/* change input label to target label */
					SetArgInsLab (tmp_ins->in_args[i], to_lab_def);
					set = 1;
				}
			}
		}
		for (i=0; tmp_ins->out_args[i] && !set; i++) {
			if (ArgMode (tmp_ins->out_args[i]) == ARG_INSLABEL) {
				if (ArgInsLab (tmp_ins->out_args[i]) == from_lab_def) {
					/* change input label to target label */
					SetArgInsLab (tmp_ins->out_args[i], to_lab_def);
					set = 1;
				}
			}
		}
	}
	return;
}
/*}}}*/
/*{{{  int rtl_glob_codelabels (ins_chain *head)*/
/*
 *	merges together similar labels (within a code block)
 */
int rtl_glob_codelabels (ins_chain *head)
{
	ins_chain *labels[MAX_LABEL_GLOB];
	int n_labels;
	int n_globbed;
	int i;
	int src_lab, dst_lab;
	ins_chain *dst_lab_def;

	n_labels = 0;
	n_globbed = 0;
	while (head) {
		if (head->type == INS_SETLABEL) {
			labels[n_labels] = head;
			n_labels++;
		} else if ((head->type >= INS_FIRST) && (head->type <= INS_LAST) && n_labels) {
			if (n_labels > 1) {
				/* use the first to rename to */
				dst_lab = ArgLabel (labels[0]->in_args[0]);
				dst_lab_def = labels[0];
				for (i=1; i<n_labels; i++) {
					src_lab = ArgLabel (labels[i]->in_args[0]);
					rtl_rename_label (src_lab, labels[i], dst_lab, dst_lab_def);
					n_globbed++;
					/* trash this label */
					rtl_remove_instr (labels[i]);
				}
			}
			n_labels = 0;
		}
		head = rtl_next_instr (head);
	}
	return n_globbed;
}
/*}}}*/
/*{{{  int rtl_glob_labels (rtl_chain *rtl_code)*/
/*
 *	merges together similar labels
 */
int rtl_glob_labels (rtl_chain *rtl_code)
{
	int globbed = 0;

	while (rtl_code) {
		if (rtl_code->type == RTL_CODE) {
			globbed += rtl_glob_codelabels (rtl_code->u.code.head);
		}
		rtl_code = rtl_code->next;
	}
	return globbed;
}
/*}}}*/
/*{{{  ins_sib_arg *rtl_copy_sib_arg (ins_sib_arg *sibarg)*/
/*
 *	returns a copy of `sibarg'
 */
ins_sib_arg *rtl_copy_sib_arg (ins_sib_arg *sibarg)
{
	ins_sib_arg *tmp;

	tmp = new_ins_sib_arg ();
	memcpy (tmp, sibarg, sizeof (ins_sib_arg));
	return tmp;
}
/*}}}*/
/*{{{  ins_arg *rtl_copy_arg (ins_arg *arg, ins_chain *addins)*/
/*
 *	returns a copy of `arg' (addins is used to update pointers on the other side of ARG_INSLABELs)
 */
ins_arg *rtl_copy_arg (ins_arg *arg, ins_chain *addins)
{
	ins_arg *tmp;

	if (!arg) {
		return NULL;
	}
	tmp = new_ins_arg ();
	tmp->flags = arg->flags;
	tmp->disp = arg->disp;
	switch (arg->flags & ARG_MODEMASK) {
	case ARG_REG:
	case ARG_CONST:
	case ARG_REGIND:
	case ARG_COND:
	case ARG_LABEL:
	case ARG_FLABEL:
	case ARG_BLABEL:
	case ARG_FREG:
		tmp->regconst = arg->regconst;
		break;
	case ARG_NAMEDLABEL:
	case ARG_TEXT:
		tmp->regconst = (int)string_dup ((char *)arg->regconst);
		break;
	case ARG_REGINDSIB:
		tmp->regconst = (int)rtl_copy_sib_arg ((ins_sib_arg *)arg->regconst);
		break;
	case ARG_INSLABEL:
		tmp->regconst = arg->regconst;
		/* update pointers on other side */
		tmp->regconst = (int)rtl_add_labref ((ins_labrefs *)tmp->regconst, addins);
		break;
	default:
	case ARG_LABREFS:
		fprintf (stderr, "%s: error: cannot copy this argument (flags = 0x%4.4x)\n", progname, arg->flags);
		/* this will blow it out later */
		sfree (tmp);
		tmp = NULL;
	}
	return tmp;
}
/*}}}*/
/*{{{  ins_chain *rtl_copy_instr (ins_chain *ins)*/
/*
 *	returns a copy of `ins'
 */
ins_chain *rtl_copy_instr (ins_chain *ins)
{
	int i;
	ins_chain *tmp;

	if (!ins) {
		return NULL;
	}
	tmp = new_ins ();
	tmp->next = tmp->prev = NULL;
	tmp->rtl = NULL;
	tmp->type = ins->type;
	tmp->etc_type = ins->etc_type;
	for (i=0; ins->out_args[i]; i++) {
		tmp->out_args[i] = rtl_copy_arg (ins->out_args[i], tmp);
	}
	for (i=0; ins->in_args[i]; i++) {
		tmp->in_args[i] = rtl_copy_arg (ins->in_args[i], tmp);
	}
	#ifdef INSTRUCTION_HISTORY
		strcpy (tmp->alloc_file, ins->alloc_file);
		tmp->alloc_line = ins->alloc_line;
	#endif	/* INSTRUCTION_HISTORY */
	return tmp;
}
/*}}}*/
/*{{{  ins_chain *rtl_copy_codeblock (ins_chain *first, ins_chain *last)*/
/*
 *	coppies a chunk of code
 */
ins_chain *rtl_copy_codeblock (ins_chain *first, ins_chain *last)
{
	ins_chain *t_head, *t_tail;
	ins_chain *tmp, *copy;

	t_head = t_tail = NULL;
	for (tmp=first; tmp != last->next; tmp = tmp->next) {
		copy = rtl_copy_instr (tmp);
		if (!t_head) {
			t_head = t_tail = copy;
		} else {
			t_tail->next = copy;
			copy->prev = t_tail;
			t_tail = copy;
		}
	}
	return t_head;
}
/*}}}*/
/*{{{  ins_chain *rtl_copy_code (ins_chain *code)*/
/*
 *	returns a copy of `code'
 */
ins_chain *rtl_copy_code (ins_chain *code)
{
	ins_chain *last;

	for (last = code; last->next; last = last->next);
	return rtl_copy_codeblock (code, last);
}
/*}}}*/
/*{{{  int rtl_count_instrs (rtl_chain *rtl_code)*/
/*
 *	counts the number of instructions on a chain
 */
int rtl_count_instrs (rtl_chain *rtl_code)
{
	int count;
	ins_chain *tmp_ins;

	count = 0;
	while (rtl_code) {
		switch (rtl_code->type) {
		case RTL_STUBIMPORT:
			count++;
			break;
		case RTL_CODE:
			for (tmp_ins = rtl_code->u.code.head; tmp_ins; tmp_ins = tmp_ins->next) {
				switch (tmp_ins->type) {
				case INS_SETLABEL:
				case INS_SETFLABEL:
				case INS_ANNO:
					break;
				default:
					if ((tmp_ins->type >= INS_FIRST) && (tmp_ins->type <= INS_LAST)) {
						count++;
					}
					break;
				}
			}
			break;
		default:
			break;
		}
		rtl_code = rtl_code->next;
	}
	return count;
}
/*}}}*/

