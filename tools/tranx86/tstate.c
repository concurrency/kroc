/*
 *	tstate.c -- transputer state functions
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

#include "main.h"
#include "support.h"
#include "structs.h"
#include "transputer.h"
#include "trancomm.h"
#include "tstack.h"
#include "postmortem.h"
#include "intel.h"
#include "tstate.h"
#include "archdef.h"
#include "rtlops.h"
#include "etcdump.h"
#include "tstack.h"


#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

/*{{{  tstate *new_tstate (void)*/
/*
 *	creates a new transputer-state structure
 */
tstate *new_tstate (void)
{
	tstate *tmp;

	tmp = (tstate *)smalloc (sizeof (tstate));
	memset (tmp, 0, sizeof (tstate));
	return tmp;
}
/*}}}*/
/*{{{  void tstate_initialise (tstate *state, etc_chain *etc_code)*/
/*
 *	initialises the transputer state
 */
void tstate_initialise (tstate *state, etc_chain *etc_code)
{
	etc_chain *tmp;

	state->cond = CC_NONE;
	state->incasetable = 0;
	state->numfuncresults = 0;
	state->line_pending = -1;
	state->file_pending = -1;
	state->proc_pending = -1;
	state->fp_line_pending = -1;
	state->fp_file_pending = -1;
	state->fp_proc_pending = -1;
	state->file_list = state->proc_list = NULL;
	state->file_cur = state->file_max = 0;
	state->proc_cur = state->proc_max = 0;
	state->ws_size = state->vs_size = state->ms_size = 0;
	state->ws_adjust = 16;
	state->stack_drift = 0;
	state->end_of_module = 0;
	state->last_lab = 0;
	state->libout_pending = 0;
	state->libout_name = NULL;
	state->supress_debug_insert = 0;
	state->had_jentry = 0;
	state->jentry_name = NULL;
	state->magic_pending = TS_MAGIC_NONE;
	state->flushscreenpoint = -1;
	ustate_clear (state);
	tstack_undefine (state->stack);
	state->stack->must_set_cmp_flags = 0;
	state->glob_names = NULL;
	state->glob_names_cur = 0;
	state->glob_names_max = 0;
	state->cpinfo = NULL;
	state->fixups = NULL;

	for (tmp=etc_code; tmp; tmp=tmp->next) {
		if ((tmp->fn >= I_OPR) && (tmp->opd < (signed int)ETC_MAX)) {
			switch (tmp->opd) {
			case ETC0:
				tmp = tmp->next;
				break;
			case ETC6:
			case ETC7:
				tmp = tmp->next;
				if (tmp->opd > state->last_lab) {
					state->last_lab = tmp->opd;
				}
				break;
			case ETCS1:
			case ETCS2:
			case ETCS3:
			case ETCS4:
			case ETCS5:
			case ETCS6:
			case ETCS7:
			case ETCS8:
			case ETCS9:
			case ETCS12:
			default:
				break;
			case ETC1:
			case ETC2:
			case ETC3:
			case ETC4:
			case ETC5:
			case ETC8:
			case ETC9:
			case ETC10:
			case ETC11:
			case ETC12:
			case ETC13:
			case ETC14:
				tmp = tmp->next;
				break;
			case ETCL0:
				tmp = tmp->next;
				if (tmp->fn == I_LDC) {
					tmp = tmp->next;
				}
				break;
			case ETCL1:
			case ETCL2:
			case ETCL3:
				tmp = tmp->next;
				tmp = tmp->next;
				if (tmp->opd > state->last_lab) {
					state->last_lab = tmp->opd;
				}
				tmp = tmp->next;
				if (tmp->opd > state->last_lab) {
					state->last_lab = tmp->opd;
				}
				break;
			case ETCL4:
				tmp = tmp->next;
				break;
			case ETCL5:
				tmp = tmp->next;
				tmp = tmp->next;
				{
					int count = tmp->opd;

					while (count) {
						tmp = tmp->next;
						tmp = tmp->next;
						count--;
					}
				}
				break;
			case ETCL7:
			case ETCL8:
				tmp = tmp->next;
				tmp = tmp->next;
				break;
			}
		}
	}
	return;
}
/*}}}*/
/*{{{  void tstate_ctofp (tstate *ts)*/
/*
 *	copies current line-number etc. into fp_ line-number etc.
 */
void tstate_ctofp (tstate *ts)
{
	ts->fp_line_pending = ts->line_pending;
	ts->fp_file_pending = ts->file_pending;
	ts->fp_proc_pending = ts->proc_pending;
	return;
}
/*}}}*/
/*{{{  int tstate_is_glob_name (tstate *state, char *name, int len)*/
/*
 *	returns non-zero if the name has been marked as global
 */
int tstate_is_glob_name (tstate *state, char *name, int len)
{
	int i;

	for (i=0; i<state->glob_names_cur; i++) {
		if (!strncmp (state->glob_names[i], name, len) && (state->glob_names[i][len] == '\0')) {
			return 1;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  void tstate_add_glob_name (tstate *state, char *name, int len)*/
/*
 *	adds the specified name to the list of global names
 */
void tstate_add_glob_name (tstate *state, char *name, int len)
{
	int i;

	for (i=0; i<state->glob_names_cur; i++) {
		if (!strncmp (state->glob_names[i], name, len) && (state->glob_names[i][len] == '\0')) {
			return;
		}
	}
	/* add it */
	if (state->glob_names_cur == state->glob_names_max) {
		state->glob_names = (char **)srealloc (state->glob_names, state->glob_names_max * sizeof (char *), (state->glob_names_max + 8) * sizeof (char *));
		state->glob_names_max += 8;
	}
	state->glob_names[state->glob_names_cur] = string_ndup (name, len);
	state->glob_names_cur = state->glob_names_cur + 1;

	return;
}
/*}}}*/
/*{{{  void tstate_addfixup (tstate *state, int thislab, int offs, int otherlab)*/
/*
 *	adds a fixup record to the tstate
 */
void tstate_addfixup (tstate *state, int thislab, int offs, int otherlab)
{
	tdfixup_t *fix = (tdfixup_t *)smalloc (sizeof (tdfixup_t));
	tdfixup_t **fixp;
	
	for (fixp = &(state->fixups); *fixp; fixp = &((*fixp)->next));

	fix->thislab = thislab;
	fix->offset = offs;
	fix->otherlab = otherlab;
	fix->next = NULL;
	*fixp = fix;

	return;
}
/*}}}*/
/*{{{  void tstate_clear_fixups (tstate *state)*/
/*
 *	clears all fixup records in the current tstate
 */
void tstate_clear_fixups (tstate *state)
{
	tdfixup_t *walk, *next;

	for (walk = state->fixups, next = NULL; walk; walk = next) {
		next = walk->next;

		sfree (walk);
	}
	state->fixups = NULL;
	return;
}
/*}}}*/
/*{{{  tdfixup_t *tstate_getfixups (tstate *state)*/
/*
 *	returns fixups in the current tstate and detaches
 */
tdfixup_t *tstate_getfixups (tstate *state)
{
	tdfixup_t *fix = state->fixups;

	state->fixups = NULL;
	return fix;
}
/*}}}*/


/************************************************************************************************/
/*		user-state stuff								*/
/************************************************************************************************/

/*{{{  definitions*/
typedef struct {
	int *regmap;
	int reg_max;
	int *labmap;
	int lab_max;
} ustate;

#define GetUState(T)	((ustate *)((T)->ustate))
#define SetUState(T,V)	((T)->ustate = (void *)(V))


/*}}}*/
/*{{{  void ustate_clear (tstate *state)*/
/*
 *	clears out any user-state (must be called before first attempts!)
 */
void ustate_clear (tstate *state)
{
	ustate *tmp;
	int i;

	if (!GetUState (state)) {
		tmp = (ustate *)smalloc (sizeof (ustate));
		tmp->regmap = NULL;
		tmp->reg_max = 0;
		tmp->labmap = NULL;
		tmp->lab_max = 0;
		SetUState (state, tmp);
	} else {
		tmp = GetUState (state);
	}
	for (i = 0; i < tmp->reg_max; i++) {
		tmp->regmap[i] = -1;
	}
	for (i = 0; i < tmp->lab_max; i++) {
		tmp->labmap[i] = -1;
	}
	return;
}
/*}}}*/
/*{{{  int ustate_newreg (tstate *state, int ureg)*/
/*
 *	creates a new virtual register (user specifies slot)
 */
int ustate_newreg (tstate *state, int ureg)
{
	ustate *us = GetUState (state);
	int i, j;

	if (ureg >= us->reg_max) {
		i = (((ureg >> 2) + 1) << 2);		/* round up into nearest 4 */
		us->regmap = (int *)srealloc (us->regmap, us->reg_max * sizeof (int), i * sizeof (int));
		for (j = us->reg_max; j < i; j++) {
			us->regmap[j] = -1;
		}
		us->reg_max = i;
	}
	us->regmap[ureg] = tstack_newreg (state->stack);
	return us->regmap[ureg];
}
/*}}}*/
/*{{{  int ustate_regof (tstate *state, int ureg)*/
/*
 *	gets a virtual register from user-specified slot (FIXME: aborts on error)
 */
int ustate_regof (tstate *state, int ureg)
{
	ustate *us = GetUState (state);

	if (ureg >= us->reg_max) {
		fprintf (stderr, "%s: error: user-register %d does not exist here\n", progname, ureg);
		exit (EXIT_FAILURE);
	}
	if (us->regmap[ureg] == -1) {
		fprintf (stderr, "%s: error: user-register %d does not exist here\n", progname, ureg);
		exit (EXIT_FAILURE);
	}
	return us->regmap[ureg];
}
/*}}}*/


/************************************************************************************************/
/*		proc-info stuff									*/
/************************************************************************************************/

/*{{{  static store*/
static procinf **procinfs = NULL;
static int procinf_max = 0;
static int procinf_cur = 0;
/*}}}*/

/*{{{  static procinf *procinf_create (void)*/
/*
 *	creates a new procinf structure
 */
static procinf *procinf_create (void)
{
	procinf *tmp;

	tmp = (procinf *)smalloc (sizeof (procinf));
	tmp->name = NULL;
	tmp->namelen = 0;
	tmp->iname = NULL;
	tmp->inamelen = 0;
	tmp->is_proc = 0;
	tmp->is_local = 0;
	tmp->is_internal = 0;
	tmp->eplab = -1;
	tmp->maplab = -1;
	tmp->namelab = -1;
	tmp->inamelab = -1;
	tmp->refs = NULL;
	tmp->refs_cur = 0;
	tmp->refs_max = 0;
	tmp->written_out = 0;

	return tmp;
}
/*}}}*/
/*{{{  static void add_to_pi_array (procinf ***arry, int *cur, int *max, procinf *item)*/
/*
 *	adds a procinf entry to a list
 */
static void add_to_pi_array (procinf ***arry, int *cur, int *max, procinf *item)
{
	if (*cur == *max) {
		*arry = (procinf **)srealloc (*arry, *cur * sizeof (procinf *), (*max + 10) * sizeof (procinf *));
		*max = *max + 10;
	}
	(*arry)[*cur] = item;
	*cur = *cur + 1;
	return;
}
/*}}}*/

/*{{{  procinf *procinf_declare (char *name, int len)*/
/*
 *	declares a new PROC/FUNCTION/STUB name (as an entrypoint)
 */
procinf *procinf_declare (char *name, int len)
{
	procinf *tmp = NULL;
	int i;
	char *lname;

	/* if name ends in %O, chop this off (on occam-pi library routines)*/
	lname = string_ndup (name, len);
	if ((len > 2) && (lname[len-2] == '%') && (lname[len-1] == 'O')) {
		len -= 2;
		lname[len] = '\0';
	}
#if 0
fprintf (stderr, "procinf_declare(): declaring new name [%s]\n", lname);
#endif
	/* make sure it's not already here */
	for (i=0; i<procinf_cur; i++) {
		if ((procinfs[i]->namelen == len) && !strncmp (procinfs[i]->name, lname, len)) {
#if 0
			/* updated: not actually fatal;  can occur from short FUNCTIONs #INCLUDE'd */
			/* reupdated: well, permitted in theory, but will usually result in a linker error
			 * from multiply defined symbols -- best to avoid in the source */
			tmp = procinfs[i];
			break;		/* for() */
#else
			fprintf (stderr, "%s: internal error: procinf_declare(): already seen name [%*s]\n", progname, procinfs[i]->namelen, procinfs[i]->name);
			/* abort.. */
			exit (EXIT_FAILURE);
#endif
		}
	}

	/* create/modify an entry and add it */
	if (tmp) {
		if (tmp->name) {
			sfree (tmp->name);
		}
	} else {
		tmp = procinf_create ();
	}
	tmp->namelen = len;
	tmp->name = lname;
	tmp->is_local = 1;

	if (i == procinf_cur) {
		add_to_pi_array (&procinfs, &procinf_cur, &procinf_max, tmp);
	}

	return tmp;
}
/*}}}*/
/*{{{  procinf *procinf_internallab (procinf *current, int lab)*/
/*
 *	adds an internal label reference to a PROC -- needed for things
 *	like return-address (local label usually)
 */
procinf *procinf_internallab (procinf *current, int lab)
{
	procinf *tmp;

	tmp = procinf_create ();
	tmp->namelen = 0;
	tmp->name = NULL;
	tmp->is_local = 1;
	tmp->is_proc = 0;
	tmp->is_internal = 1;
	tmp->eplab = lab;
	tmp->namelab = current->namelab;

	return tmp;
}
/*}}}*/
/*{{{  procinf *procinf_lookup (char *name, int len)*/
/*
 *	looks up a procinf entry for another name, adds if not here
 */
procinf *procinf_lookup (char *name, int len)
{
	procinf *tmp;

	tmp = procinf_findbyname (name, len);
	if (tmp) {
		return tmp;
	}

	/* add a new entry for it */
	tmp = procinf_declare (name, len);
	tmp->is_local = 0;

	return tmp;
}
/*}}}*/
/*{{{  procinf *procinf_findbylab (int lab)*/
/*
 *	finds a procinf by its numeric entry-point
 */
procinf *procinf_findbylab (int lab)
{
	int i;

	for (i=0; i<procinf_cur; i++) {
		if (procinfs[i]->eplab == lab) {
			return procinfs[i];
		}
	}

	return NULL;
}
/*}}}*/
/*{{{  procinf *procinf_findbyname (char *name, int len)*/
/*
 *	finds a procinf by its numeric entry-point
 */
procinf *procinf_findbyname (char *name, int len)
{
	int i;
	char *lname;

	lname = string_ndup (name, len);
	if ((len > 2) && (lname[len-2] == '%') && (lname[len-1] == 'O')) {
		len -= 2;
		lname[len] = '\0';
	}
#if 0
fprintf (stderr, "procinf_findbyname(): looking up name [%s]\n", lname);
#endif
	for (i=0; i<procinf_cur; i++) {
		if ((procinfs[i]->namelen == len) && !strncmp (procinfs[i]->name, lname, len)) {
			sfree (lname);
			return procinfs[i];
		}
	}

	sfree (lname);
	return NULL;
}
/*}}}*/
/*{{{  void procinf_addref (procinf *current, procinf *ref)*/
/*
 *	adds a reference
 */
void procinf_addref (procinf *current, procinf *ref)
{
	add_to_pi_array (&(current->refs), &(current->refs_cur), &(current->refs_max), ref);

	return;
}
/*}}}*/
/*{{{  void procinf_dumpentry (FILE *stream, procinf *pinfo)*/
/*
 *	dumps a single procinfo entry (debugging/dump)
 */
void procinf_dumpentry (FILE *stream, procinf *pinfo)
{
	int i;

	fprintf (stream, "%c%c L%-4d [L%-4d] %-20s [L%-4d] :: ", (pinfo->is_local ? 'L' : '-'), (pinfo->is_proc ? 'P' : '-'),
			pinfo->eplab, pinfo->maplab, pinfo->name, pinfo->namelab);
	for (i=0; i<pinfo->refs_cur; i++) {
		fprintf (stream, "L%d \"%s\", ", pinfo->refs[i]->eplab, pinfo->refs[i]->name);
	}
	fprintf (stream, "\n");
	return;
}
/*}}}*/
/*{{{  void procinf_dumptab (FILE *stream)*/
/*
 *	dumps the procinf table (debugging)
 */
void procinf_dumptab (FILE *stream)
{
	int i;

	for (i=0; i<procinf_cur; i++) {
		procinf_dumpentry (stream, procinfs[i]);
	}
	return;
}
/*}}}*/
/*{{{  void procinf_iterate (void (*func)(procinf *, void *), void *param)*/
/*
 *	iterates over the procinf table
 */
void procinf_iterate (void (*func)(procinf *, void *), void *param)
{
	int i;

	for (i=0; i<procinf_cur; i++) {
		func (procinfs[i], param);
	}
	return;
}
/*}}}*/


/************************************************************************************************/
/*		control split/join handling							*/
/************************************************************************************************/

/*{{{  void control_set_split (tstate *ts, int cslab, ins_chain *lastins)*/
/*
 *	called when a CONTRSPLIT is encountered.  Records the last zero-point encountered as where
 *	the sequence starts, and the vregs in the stack
 */
void control_set_split (tstate *ts, int cslab, ins_chain *lastins)
{
	csinfo *csi;
	cssplitinfo *cssi;

	for (csi=ts->csinfo; csi; csi = csi->next) {
		if (csi->label == cslab) {
			break;		/* using this one */
		}
	}
	if (!csi) {
		/* create a new one for this label */
		csi = (csinfo *)smalloc (sizeof (csinfo));
		csi->next = ts->csinfo;
		csi->label = cslab;
		csi->splits = NULL;
		csi->nsplits = 0;
		ts->csinfo = csi;
	}

	/* add this split */
	cssi = (cssplitinfo *)smalloc (sizeof (cssplitinfo));
	cssi->next = csi->splits;
	cssi->startins = ts->zeropoint ?: lastins;
	cssi->depth = ts->stack->ts_depth;
	cssi->vregs[0] = ts->stack->a_reg;
	cssi->vregs[1] = ts->stack->b_reg;
	cssi->vregs[2] = ts->stack->c_reg;

	csi->splits = cssi;
	csi->nsplits++;

	return;
}
/*}}}*/
/*{{{  void control_set_join (tstate *ts, int cslab, ins_chain *lastins)*/
/*
 *	called when a CONTRJOIN is encountered.  Uses previously saved information to
 *	re-write virtual registers used 
 */
void control_set_join (tstate *ts, int cslab, ins_chain *lastins)
{
	csinfo *csi, *prevcsi;
	cssplitinfo *cssi;
	int i;
	int curregs[3];

	prevcsi = NULL;
	for (csi=ts->csinfo; csi; csi = csi->next) {
		if (csi->label == cslab) {
			break;		/* using this one */
		}
		prevcsi = csi;
	}
	if (!csi) {
		fprintf (stderr, "%s: warning: CONTRJOIN without CONTRSPLIT\n", progname);
		return;
	}

	curregs[0] = ts->stack->a_reg;
	curregs[1] = ts->stack->b_reg;
	curregs[2] = ts->stack->c_reg;
	
	/* run through splits and re-write virtual registers */
	for (cssi = csi->splits; cssi; cssi = cssi->next) {
		int coloured;

#if 0
fprintf (stderr, "control_set_join(): doing join from instructions 0x%8.8x to 0x%8.8x.  splitdepth=%d (", (unsigned int)cssi->startins, (unsigned int)lastins, cssi->depth);
for (i=0; i<cssi->depth; i++) {
fprintf (stderr, "%d,", cssi->vregs[i]);
}
fprintf (stderr, ")  curdepth=%d (", ts->stack->ts_depth);
for (i=0; i<ts->stack->ts_depth; i++) {
fprintf (stderr, "%d,", curregs[i]);
}
fprintf (stderr, ")\n");
#endif
		if (cssi->depth != ts->stack->ts_depth) {
			fprintf (stderr, "%s: warning: split depth not the same as join depth.. (not joining)\n", progname);
		} else {
			for (i=0; i<cssi->depth; i++) {
				coloured += rtl_rename_reg_block (cssi->startins, lastins, cssi->vregs[i], curregs[i]);
			}
		}
	}

	/* clean up split info entries */
	for (; csi->splits; csi->splits = cssi) {
		cssplitinfo *thiscssi = csi->splits;

		cssi = thiscssi->next;
		sfree (thiscssi);
	}

	/* remove the csi entry from the list */
	if (!prevcsi) {
		ts->csinfo = csi->next;
	} else {
		prevcsi->next = csi->next;
	}
	sfree (csi);

	return;
}
/*}}}*/
/*{{{  int control_in_split (tstate *ts)*/
/*
 *	returns non-zero if the translator is currently in the middle of a control split/join
 */
int control_in_split (tstate *ts)
{
	if (ts->csinfo) {
		return 1;
	}
	return 0;
}
/*}}}*/


