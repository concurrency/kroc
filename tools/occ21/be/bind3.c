/* $Id: bind3.c,v 1.3 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	workspace allocation
 *	Copyright (C) 1987 Inmos Limited
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

/*#define DEBUG*/

/*
 *  modified for alpha to align anything bigger than one slot on quad word
 *  boundary, and to round up wssize to even number of slots
 */

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>
#include "suplib.h"		/* IMPORTED */

#include "includes.h"
#include "generror.h"
#include "genhdr.h"
#include "bind3def.h"
#include "gen1def.h"
#include "gen2def.h"
#include "code1def.h"
/*}}}*/
/*{{{  #defines*/
/*#define MAXVAR 2048*//* changed for bug 1075 13/12/90 */
#define MAXVAR 64 /* start value - now no fixed limit */	/* changed for bug 1075 24/9/91 */

#define VARROW (MAXVAR/(sizeof(int) * 8))

#define   SORT_USAGE_CUTOFF 16	/* first 16 vars are sorted by usage, rest by scope */
#define RESORT_SLOTS_CUTOFF 16	/* first 16 slots are sorted by usage, rest by scope */
/*}}}*/
/*{{{  global variables*/
PUBLIC BIT32 datasize, vsp, maxvsp;
PUBLIC treenode *enclosing_vsp_nptr;	/* nptr of next enclosing vectorpsace variable */
PUBLIC int staticlinkused;
PUBLIC int var_base, num_var;
PUBLIC INT32 loop_weight;

/*PUBLIC treenode *current_proc;*//* no longer needed bug 1075 24/9/91 */

PUBLIC int alloc_strategy = ALLOC_DEFAULT;
/*}}}*/
/*{{{  local variables*/
PRIVATE int maxvar = MAXVAR;	/* Current max number of variables in bitmap */
PRIVATE int varrow = VARROW;	/* Current size of bitmap */
PRIVATE int nextlabel;
/*PRIVATE int live_var[VARROW];*//* bug 1075 24/9/91 */
PRIVATE int *live_var = NULL;	/* bug 1075 24/9/91 */
PRIVATE int *var_map = NULL;
/*PRIVATE treenode *var_list[MAXVAR];*//* bug 1075 24/9/91 */
PRIVATE treenode **var_list = NULL;	/* bug 1075 24/9/91 */
PRIVATE treenode *fixedws_list;

PRIVATE treenode *vars_mapped_onto_vsptr;
PRIVATE BOOL need_wsmap = FALSE;

typedef struct {
	int wsoffset;
	treenode *var;
	int vtype;
	int vsize;		/* number of slots used */
	treenode *basetype;
} wsmap_entry_t;

PRIVATE wsmap_entry_t *wsmap = NULL;
PRIVATE int wsmap_cur = 0;
PRIVATE int wsmap_max = 0;

/*}}}*/


#if 0
PRIVATE int stretchdebug = 0;
#endif


#ifdef MOBILES
/*{{{  constants/private vars for mobile-space mapping*/
/* added May 2001 */

typedef enum { MSMAP_VAR, MSMAP_INSTANCE, MSMAP_REPLPAR } msmapslot_t;

typedef struct TAG_msmap_t {
	struct TAG_msmap_t *next;		/* next in the map for this PROC/FUNCTION */
	msmapslot_t type;			/* MSMAP_...  used for determining allocation in the mapper */
	int words;				/* words required for this variable */
	int msoffset;				/* offset from start of mobile-space for this thing */
	int sloffset;				/* offset from start of mobile-space for shadow parameter */
	treenode *varptr;			/* pointer to variable (or to INSTANCENODE) */
	struct TAG_msmap_t *replptr;		/* pointer to variables/instances inside a replicated PAR */
	int replcount;				/* replicator count for things inside replicated PARs (-1 for dynamic things) */
	treenode *spacenode;			/* spacenode pointer for replicated PARs */
} msmap_t;

PRIVATE msmap_t **msmaptab;
PRIVATE treenode **msproctab;
PRIVATE int msproctab_ptr, msproctab_size;
PRIVATE int replparcount;			/* incremented during repl-par mapping */
#define RPSTACKSIZE 128
PRIVATE msmap_t *replparstarts[RPSTACKSIZE];	/* stack of replicated PAR msmap_t nodes (MSMAP_REPLPAR type) */
PRIVATE int replparusage[RPSTACKSIZE];		/* usage (words) of stuff mapped during a REPL PAR */
/*}}}*/
/*{{{  PUBLIC void initmobilespace (void)*/
/*
 *	initialises mobilespace allocator (called once)
 */
PUBLIC void initmobilespace (void)
{
	int i;

	msproctab_size = 32;
	msproctab_ptr = -1;
	msmaptab = (msmap_t **)memalloc (msproctab_size * sizeof (msmap_t *));
	msproctab = (treenode **)memalloc (msproctab_size * sizeof (treenode *));
	for (i=0; i<msproctab_size; i++) {
		msproctab[i] = NULL;
		msmaptab[i] = NULL;
	}
	replparcount = 0;
	return;
}
/*}}}*/
/*{{{  PUBLIC int initmobilenewproc (treenode *procdef)*/
/*
 *	called as we start mapping a PROC or FUNCTION
 */
PUBLIC int initmobilenewproc (treenode *procdef)
{
	msproctab_ptr++;
	if (msproctab_ptr == msproctab_size) {
		fprintf (stderr, "initmobilenewproc(): out of space!\n");
		return -1;
	}
	msproctab[msproctab_ptr] = procdef;
	return msproctab_ptr;
}
/*}}}*/
/*{{{  PRIVATE msmap_t *msmap_mergesort (msmap_t *list, int length)*/
/*
 *	performs a merge-sort on msmap_t's sorting by NVUseCountOf
 */
PRIVATE msmap_t *msmap_mergesort (msmap_t *list, int length)
{
	int i;
	msmap_t *list1, *list2, *last;

	if (length < 2) {
		if (list && (list->type == MSMAP_REPLPAR)) {
			int n_length = 0;
			msmap_t *n_tmp;

			for (n_tmp = list->replptr; n_tmp; n_tmp = n_tmp->next, n_length++);
			if (n_length) {
				/* subsort nested lists */
				list->replptr = msmap_mergesort (list->replptr, n_length);
			}
		}
		return list;
	}
	list1 = list;
	for (i = 1; i < (length >> 1); i++) {
		list = list->next;
	}
	list2 = list->next;
	list->next = NULL;
	list1 = msmap_mergesort (list1, (length >> 1));
	list2 = msmap_mergesort (list2, (length - (length >> 1)));
	if ((list1->type == MSMAP_VAR) && (list2->type == MSMAP_VAR)) {
		if (NVUseCountOf (list1->varptr) >= NVUseCountOf (list2->varptr)) {
			list = list1;
			list1 = list1->next;
		} else {
			list = list2;
			list2 = list2->next;
		}
	} else if (list1->type == MSMAP_VAR) {
		list = list1;
		list1 = list1->next;
	} else if (list2->type == MSMAP_VAR) {
		list = list2;
		list2 = list2->next;
	} else {
		list = list1;
		list1 = list1->next;
	}
	last = list;
	while (list1 && list2) {
		if ((list1->type == MSMAP_VAR) && (list2->type == MSMAP_VAR)) {
			if (NVUseCountOf (list1->varptr) >= NVUseCountOf (list2->varptr)) {
				last->next = list1;
				list1 = list1->next;
			} else {
				last->next = list2;
				list2 = list2->next;
			}
		} else if (list1->type == MSMAP_VAR) {
			last->next = list1;
			list1 = list1->next;
		} else if (list2->type == MSMAP_VAR) {
			last->next = list2;
			list2 = list2->next;
		} else {
			last->next = list1;
			list1 = list1->next;
		}
		last = last->next;
	}
	last->next = (list1 ? list1 : list2);
	return list;
}
/*}}}*/
/*{{{  PRIVATE int bindmobilespace (msmap_t *map, int s_offset)*/

/* forward */
PRIVATE int mobilecountinitsequence (msmap_t *map);

/*
 *	allocates mobilespace
 */
PRIVATE int bindmobilespace (msmap_t *map, int s_offset)
{
	int length;
	int offsptr;
	msmap_t *tmp;
	treenode *list;

	/* allocate shadow space */
	for (tmp = map, length = 0; tmp && (tmp->type == MSMAP_VAR); tmp = tmp->next, length++);
	offsptr = (s_offset + length);

	/* allocate proper */
	for (tmp = map, length = 0; tmp; tmp = tmp->next) {
		tmp->sloffset = length;
		tmp->msoffset = offsptr;
		switch (tmp->type) {
		case MSMAP_VAR:
			/* set slot offset for local MOBILE */
			SetNVMSOffset (tmp->varptr, tmp->sloffset);
			offsptr += tmp->words;
			length++;
			break;
		case MSMAP_INSTANCE:
			{
				int found_msp;

				/* set ms offset in MSP param of PROC */
				list = IParamListOf (tmp->varptr);
				found_msp = 0;
				while (!EmptyList (list)) {
					treenode *item = ThisItem (list);

					if (TagOf (item) == S_PARAM_MSP) {
#if 0
fprintf (stderr, "bindmobilespace: setting hidden dimension for mobilespace to %d\n", tmp->msoffset);
#endif
						SetHDimension (item, tmp->msoffset);
						found_msp = 1;
						list = NULL;
					} else {
						list = NextItem (list);
					}
				}
				if (!found_msp && IRecursiveOf (tmp->varptr)) {
#if 0
fprintf (stderr, "bindmobilespace: INSTANCE of [%s].  IRecursiveOf = %d.  IForkedOf = %d.  Setting IRMSPOffset to %d\n", WNameOf (NNameOf (INameOf (tmp->varptr))),
		IRecursiveOf (tmp->varptr), IForkedOf (tmp->varptr), tmp->msoffset);
#endif
					SetIRMSPOffset (tmp->varptr, tmp->msoffset);
				}
				if (IForkedOf (tmp->varptr)) {
					/* always set for FORKs */
					SetIRMSPOffset (tmp->varptr, tmp->msoffset);
				}
				offsptr += tmp->words;
			}
			break;
		case MSMAP_REPLPAR:
			/* set ms offset in SpNestedMSOf */
			SetSpNestedMS (tmp->spacenode, offsptr);

			/* map out nested mobilespace */
			bindmobilespace (tmp->replptr, 0);
			if (tmp->replcount == -1) {
				offsptr += 2;		/* 2 words.. */
				if (mobilecountinitsequence (tmp->replptr) > 0) {
					SetSpMSPtr (tmp->spacenode, tmp->replptr);
				} else {
					SetSpMSPtr (tmp->spacenode, NULL);
				}
			} else {
				offsptr += (tmp->words * tmp->replcount);
				SetSpMSPtr (tmp->spacenode, NULL);
			}
			break;
		}
	}

	return 0;
}
/*}}}*/
#define DO_DUMP_MOBILESPACEMAP 0
/*{{{  [debug] PRIVATE void dump_mobilespacemap (FILE *stream, msmap_t *map, int loffset)*/
#if DO_DUMP_MOBILESPACEMAP
/* debug code -- dumps mobilespace map */
PRIVATE void dump_mobilespacemap (FILE *stream, msmap_t *map, int loffset)
{
	int i;
	msmap_t *tmp;

	for (tmp = map; tmp; tmp = tmp->next) {
		for (i=0; i<loffset; i++) {
			fprintf (stream, " ");
		}
		switch (tmp->type) {
		case MSMAP_VAR:
			fprintf (stderr, "VAR  words= %d, var-name= \"%*s\", ms-offset= %d, sl-offset= %d, ms-slot= %d, use-count= %d, replcount= %d\n", tmp->words,
				WLengthOf (NNameOf (tmp->varptr)), WNameOf (NNameOf (tmp->varptr)), tmp->msoffset, tmp->sloffset, (int)NVMSOffsetOf (tmp->varptr),
				(int)NVUseCountOf (tmp->varptr), tmp->replcount);
			break;
		case MSMAP_INSTANCE:
			fprintf (stderr, "INS  words= %d, instance of \"%*s\", ms-offset= %d, replcount= %d, INameOf =", tmp->words, WLengthOf (NNameOf (INameOf (tmp->varptr))),
				WNameOf (NNameOf (INameOf (tmp->varptr))), tmp->msoffset, tmp->replcount);
			printtreenl (stderr, 5 + loffset, INameOf (tmp->varptr));
			break;
		case MSMAP_REPLPAR:
			fprintf (stderr, "RPR  words= %d, replcount= %d, NestedMSOf= %d, MSUsageOf= %d, nested map:\n", tmp->words, tmp->replcount, (int)SpNestedMSOf (tmp->spacenode),
				(int)SpMSUsageOf (tmp->spacenode));
			dump_mobilespacemap (stream, tmp->replptr, loffset + 4);
			break;
		}
	}
}
#endif
/*}}}*/
/*{{{  PUBLIC int mobileoutproc (void)*/
/*
 *	called when mapping of a PROC or FUNCTION has finished
 */
PUBLIC int mobileoutproc (void)
{
	treenode *procptr = msproctab[msproctab_ptr];
	msmap_t *mapptr = msmaptab[msproctab_ptr];
	msmap_t *tmp;
	int length;

	msmaptab[msproctab_ptr] = NULL;
	msproctab[msproctab_ptr] = NULL;
	msproctab_ptr--;
	for (tmp = mapptr, length = 0; tmp; tmp = tmp->next, length++);
	/* sort mapptr elements by usage-count (highest first), followed by non-variables on space usage */
	mapptr = msmap_mergesort (mapptr, length);
	/* allocate mobile-space slots for them */
	bindmobilespace (mapptr, 0);
	SetNPMSPtr (DNameOf (procptr), mapptr);
#if DO_DUMP_MOBILESPACEMAP
	/* DEBUG CODE */
	if (mapptr) {
		fprintf (stderr, "mobileoutproc (%*s): list is:\n", WLengthOf (NNameOf (DNameOf (procptr))), WNameOf (NNameOf (DNameOf (procptr))));
		dump_mobilespacemap (stderr, NPMSPtrOf (DNameOf (procptr)), 4);
	}
#endif
	return (msproctab_ptr + 1);
}
/*}}}*/
/*{{{  PRIVATE int mobilecountnestedinit (treenode *typetree)*/
PRIVATE int mobilecountnestedinit (treenode *typetree)
{
	int count = 0;
	treenode *decl;

#if 0
fprintf (stderr, "mobilecountnestedinit: typetree =");
printtreenl (stderr, 4, ARTypeOf (typetree));
#endif
	if (TagOf (typetree) != S_RECORD) {
		return 0;
	}
	decl = ARTypeOf (typetree);
	while (decl && (TagOf (decl) == S_DECL)) {
		treenode *name = DNameOf (decl);

		if (isdynmobilearray (name)) {
#if 0
fprintf (stderr, "mobilecountnestedinit: found mobile array! name = %s, type =", WNameOf (NNameOf (name)));
printtreenl (stderr, 4, NTypeOf (name));
#endif
			/* only need to initialise dimensions */
			count += 1 + dynmobiledimensioncount (name);
		} else if (isdynmobilechantype (name) || isdynmobileproctype (name)) {
			count++;
		}
		decl = DBodyOf (decl);
	}
	return count;
}
/*}}}*/
/*{{{  PRIVATE int mobiledropnestedinit (treenode *varptr, void (*ldcinst)(const int), int msoffset, int disp)*/
PRIVATE int mobiledropnestedinit (treenode *varptr, void (*ldcinst)(const int), int msoffset, int disp)
{
	treenode *type, *decl;
	int count = 0;

	if (TagOf (NTypeOf (varptr)) != N_TYPEDECL) {
		return 0;
	}
	type = follow_user_type (NTypeOf (varptr));
	if (TagOf (type) != S_MOBILE) {
		return 0;
	}
	type = MTypeOf (type);
	if (TagOf (type) != S_RECORD) {
		return 0;
	}
	decl = ARTypeOf (type);
	while (decl && (TagOf (decl) == S_DECL)) {
		treenode *name = DNameOf (decl);

		if (isdynmobilearray (name)) {
#if 0
fprintf (stderr, "dropping code for dynmobilearray-init.  NVOffsetOf(name) = %d\n", NVOffsetOf (name));
#endif
			(*ldcinst)(msoffset + disp + (NVOffsetOf (name) / bytesperword));
			(*ldcinst)(0);
			count++;
		} else if (isdynmobilechantype (name) || isdynmobileproctype (name)) {
			(*ldcinst)(msoffset + disp + (NVOffsetOf (name) / bytesperword));
			(*ldcinst)(-1);
			count++;
		}
		decl = DBodyOf (decl);
	}
	return count;
}
/*}}}*/
/*{{{  PRIVATE int mobilecountinitsequence (msmap_t *map)*/
/*
 *	counts the number of init sequences required for a mobilespace map
 */
PRIVATE int mobilecountinitsequence (msmap_t *map)
{
	int count = 0;
	msmap_t *tmp;

	for (tmp = map; tmp; tmp = tmp->next) {
		switch (tmp->type) {
		case MSMAP_VAR:
			count++;
			/* if there are any nested MOBILEs in here, count them too */
#if 0
fprintf (stderr, "mobilecountinitsequence: map = %p, tmp->varptr =", map);
printtreenl (stderr, 4, tmp->varptr);
#endif
			if (TagOf (NTypeOf (tmp->varptr)) == N_TYPEDECL) {
				treenode *type = follow_user_type (NTypeOf (tmp->varptr));

				if (TagOf (type) != S_MOBILE) {
					geninternal_is (GEN_ERROR_IN_ROUTINE, 1, "mobilecountinitsequence");
					return 0;
				}
				type = MTypeOf (type);
				count += mobilecountnestedinit (type);
			}
			break;
		case MSMAP_REPLPAR:
			if (tmp->replcount == -1) {
				count++;		/* sequence for initialising MS array */
#if 0
fprintf (stderr, "mobilecountinitsequence: REPLPAR: replcount= -1, adding 1 to count..\n");
#endif
			} else {
				/* int nested_count = mobilecountinitsequence (tmp->replptr); */

#if 0
fprintf (stderr, "mobilecountinitsequence: REPLPAR: replcount= %d, mobilecount...(tmp->replptr)= %d\n", tmp->replcount, mobilecountinitsequence (tmp->replptr));
#endif
				count += (tmp->replcount * mobilecountinitsequence (tmp->replptr));
			}
			break;
		case MSMAP_INSTANCE:
			/* if recursive -- or forked --, reserve single slot initialised to MINT */
			/* update: always do this, otherwise called PROCs may not find first slot MINT */
			count++;
			break;
		}
	}
	return count;
}
/*}}}*/
/*{{{  PRIVATE int mobiledropinitsequence (msmap_t *map, void (*ldcinst)(const int), const int disp)*/
/*
 *	drops the MOBILE initialisation code for a mobilespace map
 */
PRIVATE int mobiledropinitsequence (msmap_t *map, void (*ldcinst)(const int), const int disp)
{
	msmap_t *tmp;

	for (tmp = map; tmp; tmp = tmp->next) {
		switch (tmp->type) {
		case MSMAP_VAR:
			(*ldcinst)(tmp->sloffset + disp);
			(*ldcinst)(tmp->msoffset + disp);
			mobiledropnestedinit (tmp->varptr, ldcinst, tmp->msoffset, disp);
			break;
		case MSMAP_INSTANCE:
			/* if recursive -- or forked --, initialise mobilespace slot to MINT */
			(*ldcinst)(-1);
			(*ldcinst)(tmp->msoffset);
			break;
		case MSMAP_REPLPAR:
			if (tmp->replcount == -1) {
				(*ldcinst)(tmp->msoffset + disp);
				(*ldcinst)(0);
			} else {
				int i;
				int voffset = tmp->msoffset;

				for (i = 0; i < tmp->replcount; i++) {
					mobiledropinitsequence (tmp->replptr, ldcinst, voffset);
					voffset += SpMSUsageOf (tmp->spacenode);
				}
			}
			break;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  PUBLIC void mobilegeninitsequence (treenode *nptr, void (*ldcinst)(const int), int is_namenode)*/
/*
 *	drops the MOBILE initialisation code (indirectly) for a PROC/FUNCTION
 */
PUBLIC void mobilegeninitsequence (treenode *nptr, void (*ldcinst)(const int), int is_namenode)
{
	msmap_t *themap;
	int count;

#if 0
fprintf (stderr, "bind3: mobilegeninitsequence (enter): nptr=%p, ldcinst=%p, is_namenode=%d\n", nptr, ldcinst, is_namenode);
#endif
	if (is_namenode) {
		themap = (msmap_t *)NPMSPtrOf (nptr);
	} else {
		themap = (msmap_t *)SpMSPtrOf (nptr);
	}
	#if DO_DUMP_MOBILESPACEMAP
	fprintf (stderr, "bind3: mobilegeninitsequence (enter): nptr=%p, ldcinst=%p, is_namenode=%d: map is:\n", nptr, ldcinst, is_namenode);
	dump_mobilespacemap (stderr, themap, 4);
	#endif
	count = mobilecountinitsequence (themap);
	(*ldcinst)(count);
#if 0
fprintf (stderr, "mobilegeninitsequence: generating %d initialisations.\n", count);
#endif
	mobiledropinitsequence (themap, ldcinst, 0);
	gencomment0 (".MOBILEINITSEQUENCE");
#if 0
fprintf (stderr, "bind3: mobilegeninitsequence (leave)\n");
#endif
	return;
}
/*}}}*/
/*{{{  PUBLIC void mobilereplparstart (const int replcount)*/
/*
 *	called before a replicated par is mapped with the replicator count
 */
PUBLIC void mobilereplparstart (const int replcount)
{
	msmap_t *tmp;

	if (replparcount == RPSTACKSIZE) {
		fprintf (stderr, "mobilereplparstart: serious! RPSTACKSIZE is not enough, please edit bind3.c and make it bigger :-)\n");
		geninternal_is (GEN_ERROR_IN_ROUTINE, 0, "mobilereplparstart");
		return;
	}
	tmp = (msmap_t *)memalloc (sizeof (msmap_t));
	tmp->next = NULL;
	tmp->type = MSMAP_REPLPAR;
	tmp->words = 0;
	tmp->msoffset = -1;
	tmp->sloffset = -1;
	tmp->varptr = NULL;
	tmp->replptr = NULL;
	tmp->replcount = replcount;
	tmp->spacenode = NULL;
	replparstarts[replparcount] = tmp;
	replparusage[replparcount] = 0;
	replparcount++;
	if (enable_mobilespace) {
		if (msmaptab[msproctab_ptr]) {
			tmp->next = msmaptab[msproctab_ptr];
		}
		msmaptab[msproctab_ptr] = tmp;
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void mobilereplparfinish (treenode *spacenode)*/
/*
 *	called after a replicated par is mapped
 */
PUBLIC void mobilereplparfinish (treenode *spacenode)
{
	msmap_t *tmp;

	if (!replparcount) {
		fprintf (stderr, "mobilereplparstart: serious! mobilereplparfinish() called more times than mobilereplparstart()\n");
		geninternal_is (GEN_ERROR_IN_ROUTINE, 0, "mobilereplparfinish");
		return;
	}
	tmp = replparstarts[replparcount - 1];
	tmp->words = replparusage[replparcount - 1];
	tmp->spacenode = spacenode;
	if (!replparusage[replparcount - 1] && !tmp->replptr) {
		/* nothing mapped into mobilespace here, remove it -- ought to be on top of mobilespace stack! */
		if (enable_mobilespace) {
			if (msmaptab[msproctab_ptr] == tmp) {
#if 0
fprintf (stderr, "mobilereplparfinish: looks like no mobilespace required, removing map-node!  :)\n");
#endif
				msmaptab[msproctab_ptr] = tmp->next;
				memfree (tmp);
			}
		}
	}
	replparcount--;
	return;
}
/*}}}*/
/*{{{  PUBLIC int mobilereplparnmapped (void)*/
/*
 *	returns the number of things mapped into mobile-space during a replicated par
 */
PUBLIC int mobilereplparnmapped (void)
{
	msmap_t *tmp;
	int count;

	if (!enable_mobilespace) {
		return 0;
	} else if (!replparcount) {
		return 0;
	} else if (!msmaptab[msproctab_ptr]) {
		return 0;
	} else if (!replparstarts[replparcount - 1]->replptr) {
		return 0;
	}
	for (count = 0, tmp = replparstarts[replparcount - 1]->replptr; tmp; tmp = tmp->next, count++);
	return count;
}
/*}}}*/
/*{{{  PUBLIC int mobilereplparusage (void)*/
/*
 *	returns the mobile-space usage of something mapped during a replicated par
 */
PUBLIC int mobilereplparusage (void)
{
	if (!replparcount) {
		geninternal_is (GEN_ERROR_IN_ROUTINE, 0, "mobilereplparusage");
		return -1;
	}
	return replparusage[replparcount-1];
}
/*}}}*/
/*{{{  PUBLIC int mobilewordsin (treenode *nptr)*/
/*
 *	returns number of words (in mobile-space) this PROC requires
 */
PUBLIC int mobilewordsin (treenode *nptr)
{
	msmap_t *tmp;
	int length;

	for (tmp = NPMSPtrOf (nptr), length = 0; tmp; tmp = tmp->next) {
		switch (tmp->type) {
		case MSMAP_VAR:
			length += (tmp->words + 1);	/* one for the shadow */
			break;
		case MSMAP_INSTANCE:
			length += tmp->words;
			break;
		case MSMAP_REPLPAR:
			if (tmp->replcount == -1) {
				length += 2;
			} else {
				length += (tmp->replcount * tmp->words);
			}
			break;
		}
	}
	return length;
}
/*}}}*/
/*{{{  PRIVATE int newmobilevar (treenode *nptr)*/
/*
 *	sets a new mobile var in the current PROC (via create_var_subsidiary)
 */
PRIVATE int newmobilevar (treenode *nptr)
{
	msmap_t *tmp = (msmap_t *)memalloc (sizeof (msmap_t));

	tmp->next = NULL;
	tmp->words = wordsin (follow_user_type (NTypeOf (nptr)));
	tmp->type = MSMAP_VAR;
	tmp->msoffset = -1;
	tmp->sloffset = -1;
	tmp->varptr = nptr;
	tmp->spacenode = NULL;
	tmp->replptr = NULL;
	if (replparcount) {
		msmap_t *on_here = replparstarts[replparcount - 1];

		tmp->next = on_here->replptr;
		on_here->replptr = tmp;
		replparusage[replparcount-1] += (tmp->words + 1);	/* 1 for the variable's shadow */
		tmp->replcount = on_here->replcount;
	} else {
		tmp->replcount = 0;
		if (enable_mobilespace) {
			if (msmaptab[msproctab_ptr]) {
				tmp->next = msmaptab[msproctab_ptr];
			}
			msmaptab[msproctab_ptr] = tmp;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  PUBLIC void newmobileinstance (treenode *tptr, INT32 nwords)*/
/*
 *	called to allocate mobile-space for a PROC/FUNCTION instance
 */
PUBLIC void newmobileinstance (treenode *tptr, INT32 nwords)
{
	if (nwords) {
		msmap_t *tmp = (msmap_t *)memalloc (sizeof (msmap_t));

#if 0
fprintf (stderr, "newmobileinstance: for \"%*s\"\n", WLengthOf (NNameOf (INameOf (tptr))), WNameOf (NNameOf (INameOf (tptr))));
#endif
		tmp->next = NULL;
		tmp->words = nwords;
		tmp->type = MSMAP_INSTANCE;
		tmp->msoffset = -1;
		tmp->sloffset = -1;
		tmp->varptr = tptr;
		tmp->spacenode = NULL;
		tmp->replptr = NULL;
		if (replparcount) {
			msmap_t *on_here = replparstarts[replparcount - 1];

			tmp->next = on_here->replptr;
			on_here->replptr = tmp;
			replparusage[replparcount-1] += nwords;
			tmp->replcount = on_here->replcount;
		} else {
			tmp->replcount = 0;
			if (enable_mobilespace) {
				if (msmaptab[msproctab_ptr]) {
					tmp->next = msmaptab[msproctab_ptr];
				}
				msmaptab[msproctab_ptr] = tmp;
			}
		}
	}
}
/*}}}*/
#endif	/* MOBILES */

/*{{{  vectorspace allocation*/
/*{{{  PUBLIC void initvsp (n)*/
/*****************************************************************************
 *
 *  initvsp initialises the vector space and maximum vector space usage to
 *          n slots.
 *
 *****************************************************************************/
PUBLIC void initvsp (INT32 n)
{
	if (needs_quadalign && ((n & 1) == 1))
		n++;
	vsp = n;
	maxvsp = n;
	enclosing_vsp_nptr = NULL;	/* INSdi01511 */
}

/*}}}*/
/*{{{  PRIVATE void setvsp(n)*/
/*****************************************************************************
 *
 *  setvsp sets the vector space to be n words, adjusting maxvsp if neccessary.
 *
 *****************************************************************************/
PRIVATE void setvsp (INT32 n)
{
	vsp = n;
	maxvsp = max_INT32 (maxvsp, vsp);
}

/*}}}*/
/*{{{  PUBLIC BIT32 newvs (n)*/
/*****************************************************************************
 *
 *  newvs reserves n words of vector space and adjusts maxvs if neccessary.
 *        Returns the starting slot number of the reserved space.
 *
 *****************************************************************************/
PUBLIC INT32 newvs (INT32 n)
{
	BIT32 result;
	result = (needs_quadalign && ((n & 1) == 1)) ? vsp + 1 : vsp;
	setvsp (result + (BIT32) n);
	return (result);
}

/*}}}*/
/*}}}*/
/*{{{  workspace allocation*/
/*{{{  comment*/
/* allocation:
 *
 * A bit map showing the interference between variables is built up
 * during the register mapping phase of this pass.
 * The map is an  n x n bit var showing which are 'live'
 * at the same time:
 *    For example the map for
 *
 *    a1:                            a1 a2 a3 a4
 *    SEQ                         a1 1  1  1  1
 *      : :                       a2 1  1  1  0
 *      a2:           =           a3 1  1  1  0
 *      SEQ                       a4 1  0  0  0
 *        : :
 *        a3:
 *        SEQ
 *          : :
 *      a4:
 *      SEQ
 *        : :
 *
 * After the map has been built it is sorted by usecount.
 * Those variables requiring fixed positions are allocated first.
 * The allocation is done by scanning the list of variables looking for the
 * first slot large enough to take the variable which is not used by any
 * variable live at the same time as it.
 *
 * Variables which overlay each other are chained onto the original varible.
 */
/*}}}*/
/*{{{  PUBLIC INT32 allocsize(nptr)*/
PUBLIC INT32 allocsize (treenode *const nptr)
{
	treenode *const type = NTypeOf (nptr);
	INT32 size = wordsin (type);

	if (nodetypeoftag (TagOf (nptr)) != NAMENODE) {	/* bug 1254 8/5/91 */
		badtag (genlocn, TagOf (nptr), "allocsize");
	}
	if (size < 0) {
		geninternal_is (GEN_ERROR_IN_ROUTINE, size, "allocsize");
	}
	if (chanaspointer && (TagOf (nptr) == N_DECL) && channel_basetype (type)) {
		if (!is_channel_constructor (nptr)) {	/* bug TS/2038 15/01/93 */
			/* it must not be a constructor */
#if OPTIMISE_LOCAL_CHANS
			if ((TagOf (type) != S_CHAN) || NChanMarkOf (nptr)) {
				size *= 2;
			}
			/* this leaves (unmarked) scalar channels as using a single word */
#else
			size *= 2;
#endif
		}
	}
#if 0
fprintf (stderr, "allocsize() = %d for: ", size);
printtreenl (stderr, 4, nptr);
#endif
	return size;
}

/*}}}*/
/*{{{  support*/
/*{{{  magic macros*/
/* These macros are used for setting and testing bits in arrays of words */
#define WORDSHIFT 5		/* such that 2^WORDSHIFT = (MAXVAR/VARROW) */
#define BYTEMASK  ((1 << WORDSHIFT) - 1)	/* (2^WORDSHIFT) - 1 */

// #define wordnum(n) ((n) >> WORDSHIFT)
// #define bitnum(n)  ((n) &  BYTEMASK)

/*{{{  PRIVATE int wordnum (unsigned long long bit)*/
PRIVATE int wordnum (unsigned long long bit)
{
	int w = (int)(bit >> WORDSHIFT);

	return w;
}
/*}}}*/
/*{{{  PRIVATE int bitnum (unsigned long long bit)*/
PRIVATE int bitnum (unsigned long long bit)
{
	int b = (int)(bit & BYTEMASK);

	return b;
}
/*}}}*/

#define setbit(array,n)   ((array)[wordnum(n)] |=  (1 << (bitnum(n))))
#define clearbit(array,n) ((array)[wordnum(n)] &= ~(1 << (bitnum(n))))
#define testbit(array,n)  ((array)[wordnum(n)] &   (1 << (bitnum(n))))

/* Because the bit map is symetrical (var_map[i][j] == var_map[j][i])
 * we can store it in half the space.
 *
 * This macro is used to map a row number to its position in a linear array
 */

// #define startofrow(n)    ((int)((((long long)(n)+1)*(long long)(n)) >> 1))	/* 1/2 n(n+1) */
/*{{{  PRIVATE unsigned long long startofrow (int n)*/
/*
 *	this version of startofrow() computes things as unsigned long long.
 */
PRIVATE unsigned long long startofrow (int n)
{
	unsigned long long v = (unsigned long long)n + 1;

	v *= (unsigned long long)n;
	v >>= 1;

	return v;
}
/*}}}*/

/*}}}*/

/*{{{  PRIVATE void setbitmap (map, i, j)*/
PRIVATE void setbitmap (int *map, int i, int j)
{
	unsigned long long bitpsn;

	if (i > j) {
		bitpsn = startofrow (i) + (unsigned long long)j;
	} else {
		bitpsn = startofrow (j) + (unsigned long long)i;
	}
	setbit (map, bitpsn);
}

/*}}}*/
/*{{{  PRIVATE int testbitmap (map, i, j)*/
PRIVATE int testbitmap (int *map, int i, int j)
{
	unsigned long long bitpsn;

	if (i > j) {
		bitpsn = startofrow (i) + (unsigned long long)j;
	} else {
		bitpsn = startofrow (j) + (unsigned long long)i;
	}
	return (testbit (map, bitpsn) != 0);
}

/*}}}*/
/*{{{  PRIVATE int markbitmap(map, mask, from, to, n)*/

/* Allocation map             */
/* map of live variables      */
/* range of variables to mark */
/* bit to be set              */
PRIVATE int markbitmap (int *const map, int *const mask, const int from, const int to, const int n)
{
	register int i;
	int count = 0;
	for (i = from; i <= to; i++)
		if (testbit (mask, i)) {
			setbitmap (map, i, n);
			count++;
		}
	return (count);
}

/*}}}*/
/*{{{  PRIVATE BOOL vars_overlap (b1, l1, b2, l2)*/
/* Test for overlap of two vars,
* a variable of length 0 never overlaps
*/
PRIVATE BOOL vars_overlap (const INT32 b1, const INT32 l1, const INT32 b2, const INT32 l2)
{
	return ((l1 != 0 && l2 != 0) && (((b1 >= b2) && (b1 < b2 + l2)) || ((b2 >= b1) && (b2 < b1 + l1))));
}

/*}}}*/
/*{{{  PRIVATE INT32 numslots (nptr)*/
/* Give number of slots needed to store name nptr */
PRIVATE INT32 numslots (treenode * nptr)
{
	INT32 length = 0;	/* initialised to shut up gcc's optimiser */
	switch (TagOf (nptr)) {
		/*{{{  T_RESERVEDWS */
	case T_RESERVEDWS:
		length = NVRSizeOf (nptr);
		break;
		/*}}} */
		/*{{{  T_TEMP, N_DECL, T_PREEVALTEMP */
	case T_TEMP:
	case N_DECL:
	case T_PREEVALTEMP:
		if ((nodetypeoftag (TagOf (NTypeOf (nptr))) == TYPENODE) && (TypeAttrOf (NTypeOf (nptr)) & TypeAttr_placed)) {
			length = 1;
		} else if (isplaced (nptr)) {
			length = 0;
#ifdef MOBILES
		} else if (isinmobilespace (nptr) || isinvectorspace (nptr) || (NModeOf (nptr) == NM_POINTER)) {
#else
		} else if (isinvectorspace (nptr) || (NModeOf (nptr) == NM_POINTER)) {
#endif
#ifdef MOBILES
			/* if this is a dynamic array, need 2 + dimension slots */
			if (isdynmobilearray (nptr)) {
#if 0
fprintf (stderr, "bind3: numslots: isdynmobilearray(nptr), dynmobiledimensioncount(nptr) = %d, nptr =", dynmobiledimensioncount (nptr));
printtreenl (stderr, 4, nptr);
#endif
				length = dynmobiledimensioncount (nptr) + 2;
			} else if (isdynmobilechantype (nptr)) {
				/* this needs one slot -- always a single endpoint */
#if 0
fprintf (stderr, "numslots: allocating for dynmobilechantype: nptr =");
printtreenl (stderr, 4, nptr);
#endif
				length = 1;
			} else if (isdynmobileproctype (nptr)) {
				/* single slot for the dynamic pointer */
				length = 1;
			} else if (isinmobilespace (nptr)) {
				/* single pointer for static mobiles */
				length = 1;
			} else
#endif	/* MOBILES */
			{
				length = 1;
			}
		} else {
			length = allocsize (nptr);
		}
		break;
		/*}}} */
		/*{{{  N_ABBR N_VALABBR N_RETYPE N_VALRETYPE */
	case N_VALABBR:
	case N_ABBR:
	case N_VALRETYPE:
	case N_RETYPE:
		{
			const abbrevmode_t am = be_abbrevmode (NDeclOf (nptr));
			if ((am == AM_CONST) || (am == AM_ISRHS)) {
				length = 0;
			}
#ifdef MOBILES
			else if ((am == AM_PTR) || isinmobilespace (nptr) || isinvectorspace (nptr))
#else	/* !MOBILES */
			else if ((am == AM_PTR) || isinvectorspace (nptr))
#endif	/* !MOBILES */
			{
#ifdef MOBILES
				/* check for MOBILE abbreviation */
#if 0
fprintf (stderr, "bind3: numslots(), number of dimensions = %d, for: ", dynmobiledimensioncount (nptr));
printtreenl (stderr, 4, nptr);
#endif
				if (isdynmobilearray (nptr)) {
					#if 0
					if (mobile_size_field) {
						length = dynmobiledimensioncount (nptr) + 2;
					} else {
						length = dynmobiledimensioncount (nptr) + 1;
					}
					#endif
					length = dynmobiledimensioncount (nptr) + 2;
				} else if (isdynmobilechantype (nptr)) {
					length = 1;
				} else if (isdynmobileproctype (nptr)) {
					length = 1;
				} else if (isinmobilespace (nptr)) {
					/* single pointer for static mobiles */
					length = 1;
				} else
#endif	/* MOBILES */
				{
					length = 1;
				}
			} else {
				length = allocsize (nptr);
			}
		}
		break;
		/*}}} */
		/*{{{  N_REPL */
	case N_REPL:
		/* Replicator occupies two words, one for base, one for count */
		{
			/* This was used before the change in the way replicated ALT was done: */
			/*
			   int t = TagOf(NDeclOf(nptr));
			   length = (t == S_REPLALT || t == S_PRIREPLALT) ? 3 : 2;
			 */
			if (ReplCStepExpOf (NDeclOf (nptr))) {
				/* MODIFICATION (frmb2): replicator now includes STEP expression (constant 1 if not specified) */
				length = 4;	/* make alignment happy */
			} else {
				/* otherwise 2 */
				length = 2;
			}
		}
		break;
		/*}}} */
	default:
		badtag (genlocn, TagOf (nptr), "numslots");
	}
#if 0
fprintf (stderr, "bind3: numslots() = %d, for: ", length);
printtreenl (stderr, 4, nptr);
#endif
	return (length);
}

/*}}}*/
/*{{{  PRIVATE void check_bitmap_relationships*/
PRIVATE void check_bitmap_relationships (void)
{
	/* Here we have some code to check the assumptions made in the
	   live variable maps. Eg that sizeof(int) == 32

	   It also checks the consistency of updating maxvar and varrow.
	 */

	if (((1 << WORDSHIFT) != (sizeof (int) * 8))	/* check WORDSHIFT is correct */
	    ||((maxvar & BYTEMASK) != 0)	/* check we have a whole number of varrows */
	    ||((maxvar / varrow) != (1 << WORDSHIFT)))
		err_abort ("check_bitmap_relationships");
}

/*}}}*/
/*{{{  PRIVATE void initrow (row)*/
/* Set all elements of row zero */
PRIVATE void initrow (int *row)
{
	/* ANSI C guarantees that (int)0 is all-bits-zero */
	memset (row, '\0', varrow * sizeof (*row));
}

/*}}}*/
/*{{{  PRIVATE int numwords_of()*/
PRIVATE int numwords_of (const int n)
{
	const unsigned long long i = startofrow (n);
	int numwords = wordnum (i);

	if (bitnum (i) > 0) {
		numwords++;
	}

#if 0
	if (stretchdebug) {
		fprintf (stderr, "numwords_of (%d) = %d: startofrow() = %Lu, wordnum() = %d, bitnum() = %d\n", n, numwords, i, wordnum(i), bitnum(i));
	}
#endif

	return numwords;
}

/*}}}*/
/*{{{  PRIVATE void init_varmap(map)*/
PRIVATE void init_varmap (int map[])
{
	const int numwords = numwords_of (maxvar);

	/* ANSI C guarantees that (int)0 is all-bits-zero */
	memset (map, '\0', numwords * sizeof (*map));
}

/*}}}*/
/*{{{  PRIVATE int *alloc_var_map ()*/
/* allocate new bitmap */
PRIVATE int *alloc_var_map (void)
{
	/* maxvar is never small enough for numwords_of(maxvar) to be zero */
	return memalloc (sizeof (int) * numwords_of (maxvar));
}

/*}}}*/
/*{{{  PRIVATE int *alloc_live_var*/
PRIVATE int *alloc_live_var (void)
{
	return memalloc (sizeof (*live_var) * varrow);	/* bug 1075 24/9/91 */
}

/*}}}*/
/*{{{  PRIVATE treenode **alloc_var_list*/
PRIVATE treenode **alloc_var_list (void)
{
	return memalloc (sizeof (*var_list) * maxvar);	/* bug 1075 24/9/91 */
}

/*}}}*/
/*{{{  PRIVATE int nextvarnum()*/
PRIVATE int nextvarnum (void)
{
	if (num_var >= maxvar) {
		/*{{{  stretch all the relevant arrays */
		const int old_varrow = varrow;
		const int old_maxvar = maxvar;

		varrow *= 2;
		maxvar *= 2;
		DEBUG_MSG (("nextvarnum: stretching maxvar from %d to %d\n", old_maxvar, maxvar));
#if 0
		stretchdebug = 1;
fprintf (stderr, "nextvarnum: stretching maxvar from %d to %d, bytes %d to %d\n", old_maxvar, maxvar, numwords_of (old_maxvar) * sizeof (*var_map),
		numwords_of (maxvar) * sizeof (*var_map));
		stretchdebug = 0;
#endif

		{
			int *const new_var_map = alloc_var_map ();

			init_varmap (new_var_map);
			memcpy (new_var_map, var_map, numwords_of (old_maxvar) * sizeof (*var_map));
			memfree (var_map);
			var_map = new_var_map;
		}
		{
			treenode **const new_var_list = alloc_var_list ();

			memcpy (new_var_list, var_list, old_maxvar * sizeof (*var_list));
			memfree (var_list);
			var_list = new_var_list;
		}
		{
			int *const new_live_var = alloc_live_var ();

			initrow (new_live_var);
			memcpy (new_live_var, live_var, old_varrow * sizeof (*live_var));
			memfree (live_var);
			live_var = new_live_var;
		}
		check_bitmap_relationships ();
		/*}}} */
	}
	return num_var++;
}

/*}}}*/

/*{{{  compare routines*/
/*{{{  PRIVATEPARAM int compare_scope (v1, v2) ** passed as a parameter*/
/*{{{  comment*/
/* Compare two variables c1 and c2,
   The ordering used to compare vars is :

   T_RESERVEDWS <  all other vars

   all other vars are then compared on scope
*/
/*}}}*/
PRIVATEPARAM int compare_scope (const void *const v1, const void *const v2)
{
	const treenode *const var1 = var_list[*(const int *) v1];
	const treenode *const var2 = var_list[*(const int *) v2];
	const int tag1 = TagOf (var1);
	const int tag2 = TagOf (var2);
	if (tag1 == T_RESERVEDWS || tag2 == T_RESERVEDWS)
		/*{{{  */
	{
		if (tag1 == tag2)
			return 0;
		else if (tag1 == T_RESERVEDWS)
			return -1;
		else
			return 1;
	}
	/*}}} */
	else if (tag1 == T_TEMP || tag2 == T_TEMP)
		/*{{{  */
	{
		if (tag1 == tag2)
			return 0;
		else if (tag1 == T_TEMP)
			return -1;
		else
			return 1;
	}
	/*}}} */
	else if (NScopeOf (var1) > NScopeOf (var2))
		return -1;
	else if (NScopeOf (var1) == NScopeOf (var2))
		return 0;
	else
		return 1;
}

/*}}}*/
/*{{{  PRIVATEPARAM int compare_use (v1, v2) ** passed as a parameter*/
/*{{{  comment*/
/* Compare two variables c1 and c2,
   The ordering used to compare vars is :

   T_RESERVEDWS <  all other vars

   all other vars are then compared on use count
*/
/*}}}*/
PRIVATEPARAM int compare_use (const void *const v1, const void *const v2)
{
	const treenode *const var1 = var_list[*(const int *) v1];
	const treenode *const var2 = var_list[*(const int *) v2];
	const int tag1 = TagOf (var1);
	const int tag2 = TagOf (var2);
	if (tag1 == T_RESERVEDWS || tag2 == T_RESERVEDWS) {
		/*{{{  */
		if (tag1 == tag2) {
			return 0;
		} else if (tag1 == T_RESERVEDWS) {
			return -1;
		} else {
			return 1;
		}
	/*}}} */
	} else if (NVUseCountOf (var1) > NVUseCountOf (var2)) {
		return -1;
	} else if (NVUseCountOf (var1) == NVUseCountOf (var2)) {
		/*return 0; */
		return compare_scope (v1, v2);
	} else {
		return 1;
	}
}

/*}}}*/
/*}}}*/
/*}}}*/
/*{{{  PRIVATE void printname(nptr)*/
PRIVATE void printname (treenode * nptr)
{
	switch (TagOf (nptr)) {
	case T_RESERVEDWS:
		fprintf (outfile, "$Reserved%-6d", NVVarNumOf (nptr));
		break;
	case T_TEMP:
	case T_PREEVALTEMP:
		fprintf (outfile, "$temp%-10d", NVVarNumOf (nptr));
		break;
	default:
		fprintf (outfile, "%-15s", WNameOf (NNameOf (nptr)));
		break;
	}
}

/*}}}*/

/*{{{  PUBLIC int *save_scope()*/
/* Copy the contents of source to dest */
PUBLIC int *save_scope (void)
{
	const size_t size = sizeof (int) * varrow;
	int *const scope = (int *) memalloc (size + sizeof (int));	/* one extra for varrow */
	scope[0] = varrow;	/* save current value of varrow */
	memcpy (&scope[1], live_var, size);
	DEBUG_MSG (("save_scope: varrow is: %d\n", varrow));
	return scope;
}

/*}}}*/
/*{{{  PUBLIC void restore_scope(scope)*/
/* Copy the contents of source to dest */
PUBLIC void restore_scope (int *scope)
{
	const int old_varrow = scope[0];
	memcpy (live_var, &scope[1], sizeof (int) * old_varrow);
	if (old_varrow != varrow) {	/* bug 1075 24/9/91 */
		memset (&live_var[old_varrow], '\0', (varrow - old_varrow) * sizeof (int));
		/*printf("restore_scope: varrow is: %d, old_varrow is %d\n", varrow, old_varrow); */
	}
	DEBUG_MSG (("restore_scope: varrow is: %d, old_varrow is %d\n", varrow, old_varrow));
	memfree (scope);
}

/*}}}*/

/*{{{  PUBLIC void init_mapping()*/
PUBLIC void init_mapping (void)
{
	initrow (live_var);
	init_varmap (var_map);
}

/*}}}*/

/*{{{  PRIVATE void printbitmap (int sorted_var[])*/
PRIVATE void printbitmap (int sorted_var[])
{
	int i, j;
	fputs ("********** Variable allocation map **********\n", outfile);
	fputs ("      WS SIZE USAGE                 ", outfile);
	for (i = var_base; i < num_var; i++) {
		fputc ('0' + (i % 10), outfile);
	}
	fputc ('\n', outfile);
	for (i = var_base; i < num_var; i++) {
		const int varnum = sorted_var[i];
		treenode *nptr = var_list[varnum];
		if (NVOffsetOf (nptr) == NO_SLOT) {
			fprintf (outfile, "%-3d:NONE %3d %5d : ", i, numslots (nptr), NVUseCountOf (nptr));
		} else {
			fprintf (outfile, "%-3d:%4d %3d %5d : ", i, NVOffsetOf (nptr), numslots (nptr), NVUseCountOf (nptr));
		}
		printname (nptr);
		for (j = var_base; j < num_var; j++) {
			fputs (testbitmap (var_map, varnum, sorted_var[j]) ? "*" : ".", outfile);
		}
		fputc ('\n', outfile);
	}
	fprintf (outfile, "*********************************************\n");
}

/*}}}*/
/*{{{  PUBLIC void create_var (treenode *nptr)*/
/* Make a new variable, and mark all live variables */
PUBLIC void create_var (treenode *nptr)
{
	const int n = nextvarnum ();

	setbit (live_var, n);
	markbitmap (var_map, live_var, var_base, num_var - 1, n);
	SetNVVarNum (nptr, n);
	SetNVOffset (nptr, NO_SLOT);	/* bug 1135 31/1/91 */
	if (TagOf (nptr) != T_RESERVEDWS) {
		SetNVNext (nptr, NULL);	/* Clear list of overlayed vars */
	}
	var_list[n] = nptr;
	DEBUG_MSG (("create_var: %s, n=%d, RESERVED?=%d\n", WNameOf (NNameOf (nptr)), n, (TagOf (nptr) == T_RESERVEDWS)));
}

/*}}}*/
/*{{{  PUBLIC void create_var_subsidiary (treenode *nptr)*/
PUBLIC void create_var_subsidiary (treenode *nptr)
{
#ifdef MOBILES
	if (isinmobilespace (nptr)) {
		/* allocate mobile-space slot for this variable */
		newmobilevar (nptr);
	}
#endif
	/* This creates the vectorspace and other temporaries required for this var */
	if (isinvectorspace (nptr)) {
		SetNVVSOffset (nptr, newvs (allocsize (nptr)));
		SetNVNextTemp (nptr, enclosing_vsp_nptr);	/* INSdi01515 */
		enclosing_vsp_nptr = nptr;
	}
	if (complexinitialise (NTypeOf (nptr))) {
		/* Pointer and count held in workspace */
		reservelowworkspace (2);
	}
}

/*}}}*/
/*{{{  PUBLIC treenode *get_var (const int n)*/
PUBLIC treenode *get_var (const int n)
{
	return var_list[n];
}

/*}}}*/
/*{{{  PUBLIC void kill_var (treenode * nptr)*/
/* variable no longer in use */
PUBLIC void kill_var (treenode * nptr)
{
	const int n = (int) NVVarNumOf (nptr);

	DEBUG_MSG (("kill_var: n=%d\n", n));
	clearbit (live_var, n);
}

/*}}}*/
/*{{{  PUBLIC void resurrect_var (treenode * nptr)*/
/* Bring a variable back from the dead */
PUBLIC void resurrect_var (treenode * nptr)
{
	const int n = (int) NVVarNumOf (nptr);

	DEBUG_MSG (("resurrect_var: n=%d, RESERVED?=%d ", n, (TagOf (nptr) == T_RESERVEDWS)));
	if (!testbit (live_var, n)) {
		DEBUG_MSG (("(not already live)\n"));
		setbit (live_var, n);
		markbitmap (var_map, live_var, var_base, num_var - 1, n);
		if (TagOf (nptr) == N_DECL)
			create_var_subsidiary (nptr);	/* resurrect the vectorspace etc */
	}
#ifdef DEBUG
	else {
		DEBUG_MSG (("(already live)\n"));
	}
#endif
}

/*}}}*/
/*{{{  PUBLIC treenode *resurrect_specs (treenode *tptr)*/
/* ressurect list of specs returning a pointer to the process
 * following the specs.
 */
PUBLIC treenode *resurrect_specs (treenode *tptr)
{
	for (; isspecification (tptr); tptr = DBodyOf (tptr))
		/*{{{  resurect vars */
	{
		switch (TagOf (tptr)) {
		case S_DECL:
		case S_ABBR:
		case S_VALABBR:
		case S_RETYPE:
		case S_VALRETYPE:
			{
				treenode *n = DNameOf (tptr);
				if (TagOf (n) == S_LIST)
					while (!EndOfList (n)) {
						resurrect_var (ThisItem (n));
						n = NextItem (n);
				} else
					resurrect_var (n);
			}
			break;
		}
	}
	/*}}} */
	return tptr;
}

/*}}}*/
/*{{{  PUBLIC int create_immortalvar (treenode *nptr)*/
/* Create a new variable which is live for all varibles */
PUBLIC int create_immortalvar (treenode *nptr)
{
	const int n = nextvarnum ();
	int i;
	/* mark all those in scope as interfering with the new var */
	for (i = var_base; i <= n; i++)
		setbitmap (var_map, i, n);
	var_list[n] = nptr;
	SetNVVarNum (nptr, n);
	SetNVNext (nptr, NULL);	/* Clear list of overlayed vars */
	return n;
}

/*}}}*/

/*{{{  PUBLIC treenode *alloc_fixedws (const INT32 slot, const INT32 size)*/
/*{{{  comment*/
/* Reserve workspace from slot to (slot+size) - 1.
 * Mark all live variables as interfering with this area of workspace
 *
 * We keep a list of nodes already created in the past.
 */
/*}}}*/
PUBLIC treenode *alloc_fixedws (const INT32 slot, const INT32 size)
{
	/* Look down the list of slots for one which is
	   the same start, the same size, not allocated at all or
	   is allocated within our block.
	 */
	treenode *nptr = fixedws_list;
	DEBUG_MSG (("alloc_fixedws; slot:%d, size:%d\n", slot, size));
	{
		int i;

		for (i = var_base; i < num_var; i++) {
			treenode *ptr = var_list[i];

			if (testbit (live_var, i) && (TagOf (ptr) == T_RESERVEDWS)) {
				const BOOL ov = vars_overlap (slot, size, NVOffsetOf (ptr), NVRSizeOf (ptr));

				DEBUG_MSG (("alloc_fixedws: Live with reserved %d, clashes? %d\n", i, ov));
				if (ov) {
					genwarn_i (GEN_WORKSPACE_CLASH, NVOffsetOf (ptr));
				}
			}
		}
	}
	while ((nptr != NULL) && !((NVOffsetOf (nptr) == slot && NVRSizeOf (nptr) == size) &&
				((NVVarNumOf (nptr) == -1) || (NVVarNumOf (nptr) >= var_base && NVVarNumOf (nptr) < num_var)))) {
		nptr = NVNextOf (nptr);
	}
	if (nptr == NULL) {
		/*{{{  Make new fixed workspace node */
		switch_to_real_workspace ();
		nptr = newnamenode (T_RESERVEDWS, genlocn, tempname_p, NULL, NULL, be_lexlevel, (BIT16) (-1), NM_DEFAULT);
		SetNVRSize (nptr, size);
		SetNVNext (nptr, fixedws_list);
		SetNVWSMapTag (nptr, S_SKIP);
		fixedws_list = nptr;
		create_var (nptr);	/* Make a new var */
		DEBUG_MSG (("alloc_fixedws; creating new node\n"));
		/*}}} */
	} else {
		/*{{{  Use existing record */
		DEBUG_MSG (("alloc_fixedws; resurrecting?:%d\n", NVVarNumOf (nptr) != -1));
		if (NVVarNumOf (nptr) == -1) {
			create_var (nptr);	/* Not used in this block yet */
		} else {
			resurrect_var (nptr);	/* re-use this node */
		}
		/*}}}*/
	}
	SetNVOffset (nptr, slot);	/* leave this set correctly */
	return nptr;
}

/*}}}*/
/*{{{  PUBLIC void free_fixedwslist (void)*/
PUBLIC void free_fixedwslist (void)
{
	/* Mark fixed workspace node as being unused */
	int i;
	DEBUG_MSG (("free_fixedwslist:\n"));
	for (i = var_base; i < num_var; i++) {
		if (TagOf (var_list[i]) == T_RESERVEDWS) {
			SetNVVarNum (var_list[i], -1);
		}
	}
	return;
}

/*}}}*/
/*{{{  PUBLIC void addnamechain (treenode *const chain, treenode *const item)*/
/* Add a new name onto a names namechain */
PUBLIC void addnamechain (treenode *const chain, treenode *const item)
{
	SetNVNext (item, NVNextOf (chain));
	SetNVNext (chain, item);
	SetNVVarNum (item, NVVarNumOf (chain));
}

/*}}}*/
/*{{{  PUBLIC void reservelowworkspace (INT32 n)*/
/*****************************************************************************
 *
 *  reservelowworkspace reserves the bottom n slots of workspace.
 *  only those variable currently live will be effected.
 *
 *****************************************************************************/
PUBLIC void reservelowworkspace (INT32 n)
{
	kill_var (alloc_fixedws (0, n));
	return;
}

/*}}}*/
/*{{{  PUBLIC void reservelowworkspacetagged (INT32 n, int tag)*/
/*****************************************************************************
 *
 *  reservelowworkspacetagged reserves the bottom n slots of workspace as
 *  reservelowworkspace() does, but also tags it.
 *  only those variable currently live will be effected.
 *
 *****************************************************************************/
PUBLIC void reservelowworkspacetagged (INT32 n, int tag)
{
	treenode *nptr = alloc_fixedws (0, n);

	SetNVWSMapTag (nptr, tag);
	kill_var (nptr);
	return;
}

/*}}}*/

/*{{{  PRIVATE int can_alloc_in (treenode *const var, treenode *const target)*/
/*
 *	tests whether two variables can share the same workspace slot,
 *	based on type compatibility;  overlap checks are handled elsewhere
 */
PRIVATE int can_alloc_in (treenode *const var, treenode *const target, int offset)
{
	if (need_wsmap) {
#if 0
fprintf (stderr, "can_alloc_in (offset = %d, need_wsmap = %d): var = %p: ", offset, (int)need_wsmap, var);
printtreenl (stderr, 4, var);
fprintf (stderr, "  target = %p: ", target);
printtreenl (stderr, 4, target);
#endif
		treenode *vartype = gettype_main (var);
		treenode *tgtype = gettype_main (target);

		if (isdynmobilearray (var)) {
			if (isdynmobilearray (target)) {
				if ((dynmobiledimensioncount (var) != dynmobiledimensioncount (target)) || offset) {
					return FALSE;
				}
				/* dynamic mobile arrays of the same dimension-count can live together */
				return TRUE;
			}
			return FALSE;
		} else if (isdynmobilechantype (var)) {
			if (isdynmobilechantype (target)) {
				/* FIXME.. */
			}
			return FALSE;
		}

		if ((TagOf (vartype) == TagOf (tgtype)) && !offset) {
			return TRUE;
		}
#if 0
fprintf (stderr, "can_alloc_in: gettype_main(var) = ");
printtreenl (stderr, 4, vartype);
fprintf (stderr, "  gettype_main(target) = ");
printtreenl (stderr, 4, tgtype);
#endif
		switch (TagOf (vartype)) {
		case S_CHAN:
			return FALSE;
		}
	}
	return TRUE;
}
/*}}}*/
/*{{{  PRIVATE void printdiagnostic (treenode * const var, const BOOL show_ws, const BOOL mapped_onto_vsptr)*/
PRIVATE void printdiagnostic (treenode * const var, const BOOL show_ws, const BOOL mapped_onto_vsptr)
{
	printname (var);
	if (show_ws) {
		fprintf (outfile, " at workspace offset %d", NVOffsetOf (var));
	} else if (mapped_onto_vsptr) {
		fputs (" mapped onto vsptr", outfile);
	}
	if (isinvectorspace (var)) {
		INT32 vsoffset = NVVSOffsetOf (var);
		fprintf (outfile, "  vector space [%d..%d]", vsoffset, vsoffset + allocsize (var) - 1);
	}
	fputc ('\n', outfile);
}

/*}}}*/
/*{{{  PRIVATE void allocate_overlays (treenode * nptr, const INT32 base)*/
PRIVATE void allocate_overlays (treenode * nptr, const INT32 base)
{
	for (; nptr != NULL; nptr = NVNextOf (nptr)) {
		SetNVOffset (nptr, (INT32) ((BIT32) base + NVOffsetOf (nptr)));
		if (diagnostics && (TagOf (nptr) != T_TEMP) && (TagOf (nptr) != T_PREEVALTEMP)) {
			fputs (" - ", outfile);
			printdiagnostic (nptr, TRUE, FALSE);
		}
	}
}

/*}}}*/
/*{{{  PUBLIC void initallocvars (void)*/
PUBLIC void initallocvars (const BOOL needmap)
{
	vars_mapped_onto_vsptr = NULL;	/* added for bug 1061 30/11/90 */
	need_wsmap = needmap;
	wsmap = NULL;
	wsmap_cur = 0;
	wsmap_max = 0;
	return;
}

/*}}}*/
/*{{{  PUBLIC void *sp_saveallocvars (const BOOL needmap)*/
/*
 *	saves the current state (needed when dealing with nested things, e.g. PAR processes)
 *	we leave vars_mapped_onto_vsptr alone
 */
PUBLIC void *sp_saveallocvars (const BOOL needmap)
{
	int *saved_vars = (int *)newvec (4 * sizeof (int));

#if 0
fprintf (stderr, "bind3: sp_saveallocvars(): needmap = %d\n", (int)needmap);
#endif
	saved_vars[0] = need_wsmap;
	saved_vars[1] = (int)wsmap;
	saved_vars[2] = wsmap_cur;
	saved_vars[3] = wsmap_max;

	need_wsmap = needmap;
	wsmap = NULL;
	wsmap_cur = 0;
	wsmap_max = 0;

	return (void *)saved_vars;
}
/*}}}*/
/*{{{  PUBLIC void sp_restoreallocvars (void *saved)*/
/*
 *	restores a previously saved state (needed when dealing with nested PAR processes)
 */
PUBLIC void sp_restoreallocvars (void *saved)
{
	int *saved_vars = (int *)saved;

#if 0
fprintf (stderr, "bind3: sp_restoreallocvars(): current need_wsmap = %d\n", need_wsmap);
#endif
	need_wsmap = saved_vars[0];
	wsmap = (wsmap_entry_t *)saved_vars[1];
	wsmap_cur = saved_vars[2];
	wsmap_max = saved_vars[3];

	freevec (saved, 4 * sizeof (int));
	return;
}
/*}}}*/
/*{{{  PRIVATE void wsmap_new_entry (const int wsoffset, treenode *varptr, const int vtype, const int nslots, treenode *basetype)*/
/*
 *	adds a specific entry to the workspace map
 */
PRIVATE void wsmap_new_entry (const int wsoffset, treenode *varptr, const int vtype, const int nslots, treenode *basetype)
{
#if 0
fprintf (stderr, "bind3: wsmap_new_entry(): wsoffset=%d, vtype=%d, nslots=%d\n", wsoffset, vtype, nslots);
#endif
	if ((wsmap_cur + 1) >= wsmap_max) {
		if (!wsmap) {
			wsmap_max = 10;
			wsmap = (wsmap_entry_t *)memalloc (sizeof (wsmap_entry_t) * wsmap_max);
		} else {
			wsmap_entry_t *newmap = (wsmap_entry_t *)memalloc (sizeof (wsmap_entry_t) * (wsmap_max + 10));

			memcpy (newmap, wsmap, sizeof (wsmap_entry_t) * wsmap_max);
			wsmap_max += 10;
			memfree (wsmap);
			wsmap = newmap;
		}
	}
	wsmap[wsmap_cur].wsoffset = wsoffset;
	wsmap[wsmap_cur].var = varptr;
	wsmap[wsmap_cur].vtype = vtype;
	wsmap[wsmap_cur].vsize = nslots;
	wsmap[wsmap_cur].basetype = basetype;
	wsmap_cur++;
	wsmap[wsmap_cur].wsoffset = NO_SLOT;
	return;
}
/*}}}*/
/*{{{  PRIVATE void wsmap_add_entry (treenode *nptr, int *last_offset)*/
/*
 *	adds an entry for a specific name to the workspace map
 *	assumes this is called in WS order, with last_offset reflecting where the last
 *	map-entry was from
 */
PRIVATE void wsmap_add_entry (treenode *nptr, int *last_offset)
{
	if ((NVOffsetOf (nptr) != NO_SLOT) && (NVOffsetOf (nptr) != *last_offset)) {
		treenode *vartype = gettype_main (nptr);
		treenode *basetype = vartype;
		int mapthis = 0;
		int vtype = WSMAP_INVALID;
		int tag = NVWSMapTagOf (nptr);
		int reservedslots = 0;		/* for sizing reserved bits */
		int reservedoffset = 0;		/* in slots, from NVOffset */

#if 0
fprintf (stderr, "bind3: wsmap_add_entry(): NVOffsetOf(nptr)=%d, NVRSizeOf(nptr)=%d, tag=%s, *last_offset=%d, nptr:", NVOffsetOf (nptr), NVRSizeOf (nptr), tagstring (tag), *last_offset);
printtreenl (stderr, 4, nptr);
#endif
#if 0
fprintf (stderr, "bind3: wsmap_add_entry(): vartype:");
printtreenl (stderr, 4, vartype);
#endif
		if (TagOf (nptr) == T_RESERVEDWS) {
			switch (tag) {
			case S_PAR:
				/* this will be the PAR-space, join-label in slot 0 of it */
				reservedslots = 1;
				vtype = WSMAP_CODEPTR;
				mapthis = 1;
				break;
			default:			/* for something else (ignore) */
				break;
			}
		} else if (isdynmobilearray (nptr)) {
#if 0
fprintf (stderr, "WSMAP [%d, MOB:DA-%d]\n", NVOffsetOf (nptr), dynmobiledimensioncount (nptr));
#endif
			vtype = WSMAP_MOB_DA;
			if (TagOf (basetype) == S_MOBILE) {
				basetype = MTypeOf (basetype);
			}
			while (TagOf (basetype) == S_ARRAY) {
				basetype = ARTypeOf (basetype);
			}
			mapthis = 1;
		} else if (isdynmobilechantype (nptr)) {
			vtype = WSMAP_MOB_CT;
			mapthis = 1;
		} else if (isdynmobileproctype (nptr)) {
			vtype = WSMAP_MOB_PT;
			mapthis = 1;
		} else {
			switch (TagOf (vartype)) {
			case S_CHAN:
#if 0
fprintf (stderr, "WSMAP [%d, CHAN]\n", NVOffsetOf (nptr));
#endif
				vtype = (TagOf (nptr) == N_DECL) ? WSMAP_CHANWORD : WSMAP_CHANPTR;
				mapthis = 1;
				break;
			}
			if (!mapthis && (TagOf (nptr) == N_PARAM)) {
				vtype = WSMAP_GENPTR;
				mapthis = 1;
			}
		}
		if (mapthis) {
			int nslots;
			int offset = 0;

			switch (TagOf (nptr)) {
			case N_PARAM:
			case N_VALPARAM:
			case N_RESULTPARAM:
			case S_FNFORMALRESULT:
				nslots = 1;
				break;
			case T_RESERVEDWS:
				nslots = reservedslots;
				offset = reservedoffset;
				break;
			default:
				nslots = numslots (nptr);
				break;
			}
			wsmap_new_entry (NVOffsetOf (nptr) + offset, nptr, vtype, nslots, basetype);
			*last_offset = NVOffsetOf (nptr) + offset;
		}
	}
	return;
}
/*}}}*/
/*{{{  PRIVATE void create_workspace_map (int *sorted_var)*/
/*
 *	generates a workspace map for a process
 */
PRIVATE void create_workspace_map (int *sorted_var)
{
	int i;
	int last_offset = NO_SLOT;

	/* memalloc (bytes) / memfree (ptr) */
	for (i = var_base; i < num_var; i++) {
		treenode *var = var_list[sorted_var[i]];

		wsmap_add_entry (var, &last_offset);
	}

	return;
}
/*}}}*/
/*{{{  PRIVATE int wsmap_bytecode (const int val, BYTE *mappntr)*/
/*
 *	returns the number of bytes needed to encode some value in the workspace map;
 *	if "mappntr" is not null, this encodes them too
 */
PRIVATE int wsmap_bytecode (const int val, BYTE *mappntr)
{
	/* now deal with signed integers */
	if ((val < (1 << 6)) && (val >= -(1 << 6))) {
		if (mappntr) {
			mappntr[0] = (val & 0x7f);
		}
		return 1;
	} else if ((val < (1 << 13) && (val >= - (1 << 13)))) {
		if (mappntr) {
			unsigned int uval = (unsigned int)val;

			mappntr[0] = 0x80 | ((uval >> 8) & 0x3f);
			mappntr[1] = (uval & 0xff);
		}
		return 2;
	} else if ((val < (1 << 21) && (val >= -(1 << 21)))) {
		if (mappntr) {
			unsigned int uval = (unsigned int)val;

			mappntr[0] = (0xc0 | ((uval >> 16) & 0x3f));
			mappntr[1] = ((uval >> 8) & 0xff);
			mappntr[2] = (uval & 0xff);
		}
		return 3;
	}
	geninternal_is (GEN_ERROR_IN_ROUTINE, 2, "wsmap_bytecode: (val >= (1 << 21))");

	return 0;
}
/*}}}*/
/*{{{  PUBLIC void save_wsallocmap (treenode *nptr)*/
/*
 *	retrives the last build workspace-map and stores it in the given name (PROC definition)
 *	might also be called with a SPACENODE for PARs
 */
PUBLIC void save_wsallocmap (treenode *nptr)
{
#if 0
fprintf (stderr, "save_wsmallocmap: (need_wsmap = %d) called with nptr =", (int)need_wsmap);
printtreenl (stderr, 4, nptr);
#endif
	switch (TagOf (nptr)) {
	case N_PROCDEF:
	case N_MPROCDECL:
		if (!wsmap_cur) {
			SetNPWSMap (nptr, NULL);
			return;
		}
		SetNPWSMap (nptr, (void *)wsmap);
		break;
	case S_SPACEUSAGE:
		if (!wsmap_cur) {
			SetSpWSMap (nptr, NULL);
			return;
		}
		SetSpWSMap (nptr, (void *)wsmap);
		break;
	case N_LIBPROCDEF:
	case N_LIBMPROCDECL:
	case N_LIBFUNCDEF:
	case N_LFUNCDEF:
	case N_SFUNCDEF:
	case N_STDLIBFUNCDEF:
	case N_STDLIBPROCDEF:
		return;
	}
#if 0
{
int i;
fprintf (stderr, "saving workspace map for [%s] (%d entries):\n", (TagOf (nptr) == S_SPACEUSAGE) ? "PARPROCESS" : WNameOf (NNameOf (nptr)), wsmap_cur);
for (i=0; i<wsmap_cur; i++) {
	fprintf (stderr, "    %d - %d  (type %d) [%s]\n", wsmap[i].wsoffset, wsmap[i].vsize, wsmap[i].vtype, ((wsmap[i].vtype != WSMAP_FIRSTPARAM) && wsmap[i].var) ? WNameOf (NNameOf (wsmap[i].var)) : "none");
}
fprintf (stderr, "nptr = ");
printtreenl (stderr, 4, nptr);
}
#endif

	wsmap_cur = 0;
	wsmap_max = 0;
	wsmap = NULL;
	need_wsmap = FALSE;
	return;
}
/*}}}*/
/*{{{  PUBLIC int size_wsallocmap (treenode *nptr)*/
/*
 *	counts the number of entries in a workspace-map
 */
PUBLIC int size_wsallocmap (treenode *nptr)
{
	int i;
	wsmap_entry_t *lmap = NULL;

	switch (TagOf (nptr)) {
	case N_PROCDEF:
	case N_MPROCDECL:
		lmap = (wsmap_entry_t *)NPWSMapOf (nptr);
		break;
	case S_SPACEUSAGE:
		lmap = (wsmap_entry_t *)SpWSMapOf (nptr);
		break;
	default:
#if 0
fprintf (stderr, "SERIOUS: size_wsallocmap() on non-procdef name:");
printtreenl (stderr, 4, nptr);
#endif
		return 0;
	}
	if (!lmap) {
		return 0;
	}
	for (i=0; lmap[i].wsoffset != NO_SLOT; i++);
	return i;
}
/*}}}*/
/*{{{  PUBLIC BYTE *bytesof_wsallocmap (treenode *nptr, int *sizereturn)*/
/*
 *	creates a byte-array that encodes a workspace map
 *	encoding is in pairs (offset,type + optional type-data), space-conservative encoding:
 *	    [0xxxxxxx]				-- 7-bit(x)
 *	    [10xxxxxx][xxxxxxxx]		-- 14-bit(x)
 *	    [11xxxxxx][xxxxxxxx][xxxxxxxx]	-- 22-bit(x)
 *
 *	the map starts with two 2-byte words:
 *	    [h.......][.......l]		-- 16-bit number of individual entries
 *	    [h.......][.......l]		-- 16-bit number of bytes remaining
 *
 *	sizereturn is set to the number of bytes returned
 */
PUBLIC BYTE *bytesof_wsallocmap (treenode *nptr, int *sizereturn)
{
	BYTE *data = NULL;
	BYTE *dptr;
	int mapsize = 0;
	int maplen;
	int i;
	wsmap_entry_t *lmap = NULL;

	maplen = size_wsallocmap (nptr);
	if (!maplen) {
		*sizereturn = 0;
		return NULL;
	}
	switch (TagOf (nptr)) {
	case N_PROCDEF:
	case N_MPROCDECL:
		lmap = (wsmap_entry_t *)NPWSMapOf (nptr);
		break;
	case S_SPACEUSAGE:
		lmap = (wsmap_entry_t *)SpWSMapOf (nptr);
		break;
	default:
		*sizereturn = 0;
		return NULL;
	}

	mapsize = 4;
	for (i=0; i<maplen; i++) {
		mapsize += wsmap_bytecode (lmap[i].wsoffset, NULL);
		mapsize += wsmap_bytecode (lmap[i].vtype, NULL);

		switch (lmap[i].vtype) {
		case WSMAP_MOB_DA:		/* <n-slots>, <base-bytes> */
			mapsize += wsmap_bytecode (lmap[i].vsize, NULL);
			mapsize += wsmap_bytecode (bytesin (lmap[i].basetype), NULL);
			break;
		case WSMAP_MOB_CT:		/* <is-server>, <is-shared> */
			switch (TagOf (lmap[i].var)) {
			case N_DECL:
			case N_ABBR:
			case N_PARAM:
			case N_RESULTPARAM:
				{
					int nchans = wordsin (NTypeOf (lmap[i].var));
					int is_server = (NTypeAttrOf (NTypeOf (lmap[i].var)) & TypeAttr_marked_in) ? 1 : 0;
					int is_shared = (NTypeAttrOf (NTypeOf (lmap[i].var)) & TypeAttr_shared) ? 1 : 0;

#if 0
fprintf (stderr, "bytesof_wsallocmap(): MOB_CT: is_server=%d, is_shared=%d, var = ", is_server, is_shared);
printtreenl (stderr, 4, lmap[i].var);
#endif
					mapsize += wsmap_bytecode (nchans, NULL);
					mapsize += wsmap_bytecode (is_server, NULL);
					mapsize += wsmap_bytecode (is_shared, NULL);
				}
				break;
			default:
				mapsize += wsmap_bytecode (0, NULL);
				mapsize += wsmap_bytecode (0, NULL);
				mapsize += wsmap_bytecode (0, NULL);
				break;
			}
			break;
		default:
			/* FIXME: others */
			break;
		}
	}

	data = (BYTE *)memalloc (mapsize);
	data[0] = ((maplen >> 8) & 0xff);
	data[1] = (maplen & 0xff);
	data[2] = (((mapsize - 4) >> 8) & 0xff);
	data[3] = ((mapsize - 4) & 0xff);
	dptr = data + 4;
	for (i=0; i<maplen; i++) {
		dptr += wsmap_bytecode (lmap[i].wsoffset, dptr);
		dptr += wsmap_bytecode (lmap[i].vtype, dptr);

		switch (lmap[i].vtype) {
		case WSMAP_MOB_DA:
			dptr += wsmap_bytecode (lmap[i].vsize, dptr);
			dptr += wsmap_bytecode (bytesin (lmap[i].basetype), dptr);
			break;
		case WSMAP_MOB_CT:
			switch (TagOf (lmap[i].var)) {
			case N_DECL:
			case N_ABBR:
			case N_PARAM:
			case N_RESULTPARAM:
				{
					int nchans = wordsin (NTypeOf (lmap[i].var));
					int is_server = (NTypeAttrOf (NTypeOf (lmap[i].var)) & TypeAttr_marked_in) ? 1 : 0;
					int is_shared = (NTypeAttrOf (NTypeOf (lmap[i].var)) & TypeAttr_shared) ? 1 : 0;

					dptr += wsmap_bytecode (nchans, dptr);
					dptr += wsmap_bytecode (is_server, dptr);
					dptr += wsmap_bytecode (is_shared, dptr);
				}
				break;
			default:
				dptr += wsmap_bytecode (0, dptr);
				dptr += wsmap_bytecode (0, dptr);
				dptr += wsmap_bytecode (0, dptr);
				break;
			}
			break;
		default:
			/* FIXME: others */
			break;
		}
	}

	*sizereturn = mapsize;
	return data;
}
/*}}}*/
/*{{{  PUBLIC void freebytes_wsallocmap (BYTE *data)*/
/*
 *	frees data returned by the above
 */
PUBLIC void freebytes_wsallocmap (BYTE *data)
{
	if (!data) {
#if 0
fprintf (stderr, "SERIOUS: freebytes_allocwsmap: NULL data\n");
#endif
		return;
	}
	memfree (data);
	return;
}
/*}}}*/
/*{{{  PUBLIC void free_wsallocmap (treenode *nptr)*/
/*
 *	frees workspace map
 */
PUBLIC void free_wsallocmap (treenode *nptr)
{
	wsmap_entry_t *lmap;

	switch (TagOf (nptr)) {
	case N_PROCDEF:
	case N_MPROCDECL:
		lmap = (wsmap_entry_t *)NPWSMapOf (nptr);
		SetNPWSMap (nptr, NULL);
		break;
	case S_SPACEUSAGE:
		lmap = (wsmap_entry_t *)SpWSMapOf (nptr);
		SetSpWSMap (nptr, NULL);
		break;
	default:
#if 0
fprintf (stderr, "SERIOUS: free_wsallocmap() on non-procdef name: ");
printtreenl (stderr, 4, nptr);
#endif
		return;
	}
	if (!lmap) {
		return;
	}
	memfree (lmap);
	return;
}
/*}}}*/
/*{{{  PUBLIC INT32 allocvars (const int first_var, const int free_paramslots, const BOOL inside_par)*/
/* Allocate the vars using bit map built up during first pass.
 * Only first_var and above are allocated.
 * Returns size of workspace allocated.
 * If free_paramslots > 0 then they will be used for least used vars
 * which will fit.
 */
PUBLIC INT32 allocvars (const int first_var, const int free_paramslots, const BOOL inside_par)
{
	/* If we have more than a small number of variables, we allocate them
	   using the heap; if just a small number, use the stack - bug 1075 24/9/91 */
	int local_sorted_var[MAXVAR];
	int *const sorted_var =	/* sorted order of vars */
		(num_var > MAXVAR) ? memalloc (sizeof (*sorted_var) * num_var)
		: &local_sorted_var[0];
	INT32 wssize = 0;

	/*{{{  debug message */
	if (diagnostics) {
		fprintf (outfile, "Allocating vars from %d upto %d\n", var_base, num_var);
	}
	/*}}} */
	if (num_var > first_var) {
		int i;
		treenode *alloclist = NULL;
		/*{{{  initialise names */
		for (i = first_var; i < num_var; i++) {
			if (TagOf (var_list[i]) != T_RESERVEDWS) {
				treenode *next;
				INT32 total = 0;
				const INT32 size = numslots (var_list[i]);

				for (next = var_list[i]; next != NULL; next = NVNextOf (next)) {
					total += NVUseCountOf (next);
				}
				/* IF the object is greater than a word long (changed to 2 words)
				   then we scale down the usage count:
				 */
				if ((total != 0) && (size > 2) &&	/* bug TS/1506 02/12/91 */
				    ((alloc_strategy & ALLOC_NODIVBYSIZE) == 0)) {
					/* bug 1153 - 11/2/91 - take the sizes of objects into account */
					total /= size;
					if (total == 0) {
						total = 1;	/* in case of `underflow' */
					}
				}
				SetNVUseCount (var_list[i], total);
				SetNVOffset (var_list[i], NO_SLOT);
				/* SetNVClashCount(var_list[i], clashcount(first_var, i)); */
			}
			sorted_var[i] = i;
		}
		/*}}} */
		/*{{{  sort names */
		{
			const int size = num_var - first_var;
			if (alloc_strategy & ALLOC_BYSCOPE) {
				sup_qsort (&sorted_var[first_var], size, sizeof (int), compare_scope);
			} else {
				sup_qsort (&sorted_var[first_var], size, sizeof (int), compare_use);
			}
		}
		/*}}} */
		/*{{{  allocate all the variables */
		{
			/* alloclist is the list of allocated variables kept in the order:
			 *     (NVOffsetOf(n) < NVOffset(NVAllocNextOf(n))) ||
			 *     ((NVOffsetOf(n) == NVOffset(NVAllocNextOf(n)) &&
			 *       ((numslots(n) >= numslots(NVAllocNextOf(n)))
			 */
			BOOL re_sorted = FALSE;
			for (i = first_var; i < num_var; i++) {
				/*{{{  allocate next var */
				const int varnum = sorted_var[i];
				treenode *const var = var_list[varnum];
				const INT32 length = numslots (var);
				if (length == 0 || NVUseCountOf (var) == 0) {
					/*{{{  Don't allocate */
					if (diagnostics) {
						printname (var);
						fputs (" not allocated\n", outfile);
					}
					/*}}} */
				} else if (!inside_par && ((alloc_strategy & ALLOC_NOVSOPT) == 0) && isinvectorspace (var) && (NVVSOffsetOf (var) == 0)) {
					/*{{{  Map onto vectorspace pointer */
					SetNVAllocNext (var, vars_mapped_onto_vsptr);
					vars_mapped_onto_vsptr = var;
					if (diagnostics) {
						printdiagnostic (var, FALSE, TRUE);
					}
					/*}}} */
				} else {
					INT32 base = 0;
					treenode *this = alloclist;
					treenode *last = NULL;

					if ((TagOf (var) != T_RESERVEDWS) && (NVOffsetOf (var) == NO_SLOT)) {
						/*{{{  allocate var */
						BOOL quadneeded = FALSE; /* shut-up gcc */
						BOOL slotfound = FALSE;

						if (needs_quadalign) {
							quadneeded = (length >= 2);
						}
						/* Only look for overlap with vars already allocated */
						while ((this != NULL) && !slotfound) {
							/*{{{  look for ws slot */
							/* Base is the next guess at the position to allocate variable */
							BOOL baseisquad;
							INT32 end = base + length;

							if (needs_quadalign) {
								baseisquad = (base & 1) == 0;
								if (quadneeded && !baseisquad) {
									base++;
									end++;
									if (diagnostics) {
										fprintf (outfile, "align, length = %d, base = %d\n", (int)length, (int)base);
									}
								}
							}
							if ((end <= NVOffsetOf (this)) && can_alloc_in (var, this, (base - NVOffsetOf (this)))) {	/* bug 709 changed to '<=' from '<' 14/9/90 */
								slotfound = TRUE;	/* position found */
							} else {
								treenode *firstclash = this;
								/*{{{  look for end of gap */
								while ((firstclash != NULL) && (!testbitmap (var_map, varnum, NVVarNumOf (firstclash)) || (FALSE))) {
									firstclash = NVAllocNextOf (firstclash);
								}
								/*}}} */
								if ((firstclash == NULL) || ((end <= NVOffsetOf (firstclash)) && can_alloc_in (var, firstclash, (base - NVOffsetOf (firstclash))))) {
												/* bug 709 changed to '<=' from '<' 14/9/90 */
									slotfound = TRUE;
								} else {
									/*{{{  look for start of next gap */
									base = NVOffsetOf (firstclash) + numslots (firstclash);
									last = firstclash;
									this = NVAllocNextOf (firstclash);
									while ((this != NULL) && ((NVOffsetOf (this) < base) || !can_alloc_in (var, this, base - NVOffsetOf (this)))) {
										INT32 thisend = NVOffsetOf (this) + numslots (this);

										if ((testbitmap (var_map, varnum, NVVarNumOf (this)) || (!can_alloc_in (var, this, base - NVOffsetOf (this)))) &&
												(base < thisend)) {
											base = thisend;
										}
										last = this;
										this = NVAllocNextOf (this);
									}
									if (needs_quadalign) {
										baseisquad = (base & 1) == 0;
										if ((this == NULL) && quadneeded && !baseisquad) {
											base++;
											end++;
											if (diagnostics) {
												fprintf (outfile, "align, length = %d, base = %d\n", (int)length, (int)base);
											}
										}
									}
									/*}}} */
								}
							}
							/*}}} */
						}
						/*{{{  Allocate Offset for this var */
						SetNVOffset (var, base);
						if (diagnostics) {
							printdiagnostic (var, TRUE, FALSE);
						}
						allocate_overlays (NVNextOf (var), base);
						/*}}} */
						/*}}} */
					} else {
						/*{{{  find position in alloc list where this should go */
						while ((this != NULL) && (NVOffsetOf (this) < NVOffsetOf (var))) {
							last = this;
							this = NVAllocNextOf (this);
						}
						base = NVOffsetOf (var);
						if (diagnostics) {
							printdiagnostic (var, TRUE, FALSE);
						}
						/*}}} */
					}
					wssize = max_INT32 (wssize, base + length);
					/*{{{  Add var to alloc list */
					/* Assume :
					 *   (last == NULL || NVOffsetOf(last) < base)
					 *   (this == NULL || NVOffsetOf(this) >= base)
					 */
					while ((this != NULL) && ((NVOffsetOf (this) < base) || ((NVOffsetOf (this) == base) && (numslots (this) > length)))) {
						last = this;
						this = NVAllocNextOf (this);
					}
					if (last == NULL) {
						/* insert on front */
						SetNVAllocNext (var, alloclist);
						alloclist = var;
					} else {
						/* insert after last  */
						SetNVAllocNext (last, var);
						SetNVAllocNext (var, this);
					}
					/*}}} */
					/*{{{  debugging */
#if 0
					{
						treenode *tptr = alloclist;
						fputs ("alloclist: ", outfile);
						while (tptr != NULL) {
							fprintf (outfile, "%s @:%ld-%ld, ",
								 (TagOf (tptr) == T_RESERVEDWS) ? "reserved" : WNameOf (NNameOf (tptr)),
								 NVOffsetOf (tptr), NVOffsetOf (tptr) + numslots (tptr) - 1);
							tptr = NVAllocNextOf (tptr);
						}
						fputc ('\n', outfile);
					}
#endif
					/*}}} */
					if (!re_sorted && (wssize >= RESORT_SLOTS_CUTOFF) && (i < num_var - 2)) {
						/*{{{  re-sort by scope */
						if (diagnostics) {
							fprintf (outfile, "re-sorting %d variables by scope\n", num_var - (i + 1));
						}
						if ((alloc_strategy & ALLOC_BYSCOPE) == 0) {
							sup_qsort (&sorted_var[i + 1], num_var - (i + 1), sizeof (int), compare_scope);
						}
						re_sorted = TRUE;
						/*}}} */
					}
				}
				/*}}} */
			}
		}
		/*}}} */
		/*{{{  try and squeeze top variables into parameter slots */
		if (free_paramslots > 0) {
			treenode *movelist[MAXREGS];
			int free_slot = free_paramslots - 1;
			BOOL move_vars = TRUE;
			/* bug TS/2019 04/01/93: this used to say `free_slot > 0' */
			while ((free_slot >= 0) && move_vars && (wssize > 0)) {
				int j;
				movelist[free_slot] = NULL;

				for (j = first_var; j < num_var && move_vars; j++) {
					/*{{{  check we can move all vars at top of workspace */
					treenode *const var = var_list[j];
					const INT32 length = numslots (var);
					if ((NVOffsetOf (var) + length) == wssize) {
						if ((TagOf (var) == T_RESERVEDWS) || (length > (int) (free_slot + 1))) {
							move_vars = FALSE;	/* No point in doing more moves */
						} else {
							SetNVAllocNext (var, movelist[free_slot]);
							movelist[free_slot] = var;
						}
					}
					/*}}} */
				}
				if (move_vars) {	/* We can move top variables into unused param slot */
					free_slot--;
					wssize--;
				}
			}
			/*{{{  Allocate to vars to their new slots */
			{
				INT32 top;

				if (needs_quadalign) {
					wssize = (wssize + 1) & (~1);	/* round up to an even number */
				}
				free_slot++;
				top = wssize + INS_EXTRA + (int) (REG_PARAMS - free_paramslots) + free_slot;
				while (free_slot < free_paramslots) {
					treenode *var;
					for (var = movelist[free_slot]; var != NULL; var = NVAllocNextOf (var)) {
						const INT32 base = top - (numslots (var) - 1);
						const INT32 original_base = NVOffsetOf (var);	/* bug INSdi01968 */
						/*{{{  Allocate Offset for this var */
						SetNVOffset (var, base);
						if (diagnostics) {
							/*{{{  print some diagnostics */
							printname (var);
							fprintf (outfile, " moved to param slot, ws offset = %d\n", NVOffsetOf (var));
							/*}}} */
						}
						/*}}} */
						/*{{{  allocate offsets for overlayed vars */
						{
							treenode *nextvar;
							for (nextvar = NVNextOf (var); nextvar != NULL; nextvar = NVNextOf (nextvar)) {
								const INT32 old_diff = NVOffsetOf (nextvar) - original_base;
								SetNVOffset (nextvar, base + old_diff);
								if (diagnostics) {
									/*{{{  print some diagnostics */
									fputs (" + ", outfile);
									printname (nextvar);
									if (old_diff != 0) {
										fprintf (outfile, " moved to overlay, ws offset = %d", NVOffsetOf (nextvar));
									}
									fputc ('\n', outfile);
									/*}}} */
								}
							}
						}
						/*}}} */
					}
					top++;
					free_slot++;
				}
			}
			/*}}} */
		}
		/*}}} */
		/*{{{  debug message */
		if (diagnostics) {
			printbitmap (sorted_var);
		}
		/*}}} */
		/*{{{  generate workspace map if required*/
		if (need_wsmap) {
			create_workspace_map (sorted_var);
		}
		/*}}}*/
	}
	if (num_var > MAXVAR) {
		memfree (sorted_var);
	}
	if (needs_quadalign) {
		wssize = (wssize + 1) & (~1);	/* round up to an even number */
	}
	return (wssize);
}

/*}}}*/
/*{{{  PUBLIC void mark_hidden_var (int nodetype, INT32 wsoffset)*/
/*
 *	this is used by code in bind1 to indicate the relative positions of any `hidden'
 *	pointer variables in a process workspace.  currently only used for replicated PAR
 *	mapping and indicating code pointers.
 */
PUBLIC void mark_hidden_var (int nodetype, INT32 wsoffset)
{
#if 0
fprintf (stderr, "mark_hidden_var (): at offset %d, need_wsmap = %d\n", (int)wsoffset, need_wsmap);
#endif
	if (need_wsmap) {
		switch (nodetype) {
		case S_PARAM_STATICLINK:
			wsmap_new_entry (wsoffset, NULL, WSMAP_STATICLINK, 1, NULL);
			break;
		case S_PARAM_VSP:
			wsmap_new_entry (wsoffset, NULL, WSMAP_VSPTR, 1, NULL);
			break;
		case S_PARAM_MSP:
			wsmap_new_entry (wsoffset, NULL, WSMAP_MSPTR, 1, NULL);
			break;
		case S_CONSTPTR:
			wsmap_new_entry (wsoffset, NULL, WSMAP_CODEPTR, 1, NULL);
			break;
		}
	}
	return;
}
/*}}}*/
/*{{{  PUBLIC void wsmap_hidden_var (int offset, int size, int type)*/
/*
 *	this is used by code in bind1 to indicate the relative positions of any hidden
 *	pointer variables in workspace, generated from "WSMAP" instructions in ASM blocks
 */
PUBLIC void wsmap_hidden_var (int offset, int size, int type)
{
	if (need_wsmap) {
		wsmap_new_entry (offset, NULL, type, size, NULL);
	}
	return;
}
/*}}}*/
/*{{{  label allocation*/
/*{{{  PUBLIC int newlab ()*/
PUBLIC int newlab (void)
{
	return (nextlabel++);
}

/*}}}*/
/*{{{  PUBLIC void initlabels()*/
PUBLIC void initlabels (void)
{
	nextlabel = 0;
}

/*}}}*/
/*}}}*/
/*{{{  use count handling*/
#define HUGEVAL ((INT32)0x7fffffff)
/*{{{  PUBLIC void setusecount*/
PUBLIC void setusecount (treenode * const nptr, const INT32 n)
{
	/*DEBUG_MSG(("setusecount: setting usecount of %s to %ld\n",
	   WNameOf(NNameOf(nptr)), n)); */
	SetNVUseCount (nptr, n);
}

/*}}}*/
/*{{{  PUBLIC void addusecount(nptr, n)*/
/* up the use count of a variable by n, where n has already been loop weighted
 * If the use count would overflow it is set to HUGEVAL
 */
PUBLIC void addusecount (treenode * nptr, INT32 n)
{
	const INT32 oldcount = NVUseCountOf (nptr);
	if (n > (HUGEVAL - oldcount)) {
		SetNVUseCount (nptr, HUGEVAL);
	} else {
		/*DEBUG_MSG(("addusecount: setting usecount of %s to %ld\n",
		   WNameOf(NNameOf(nptr)), oldcount + n)); */
		SetNVUseCount (nptr, oldcount + n);
	}
}

/*}}}*/
/*{{{  PUBLIC void upusecount(nptr, n)*/
/* up the use count of a variable n times.
 * If the use count would overflow it is set to HUGEVAL
 */
PUBLIC void upusecount (treenode * const nptr, const INT32 n)
{
	if (loop_weight == 0);	/* skip */
	else if (n > (HUGEVAL / loop_weight)) {
		SetNVUseCount (nptr, HUGEVAL);
	} else {
		addusecount (nptr, loop_weight * n);
	}
}

/*}}}*/
/*{{{  PUBLIC void uploop_weight(n)*/
/* up loop_weight n times
 * If the loop_weight would overflow it is set to HUGEVAL
 */
PUBLIC void uploop_weight (INT32 n)
{
	if (n == 0 || loop_weight == 0) {
		loop_weight = 0;
	} else if (n > (HUGEVAL / loop_weight)) {
		loop_weight = HUGEVAL;
	} else {
		loop_weight *= n;
	}
}

/*}}}*/
/*}}}*/
/*{{{  temporary slot allocation*/
/*{{{  comment*/
/*
 *  To allocate a temporary we create a new temporary node then look through
 *  the current variable table looking for a temporary of the same size
 *  which is unused.
 *  If an unused temporary is found we add the new temporary node  to the
 *    variable chain of the unused temporary. This gives the new temporary
 *    the same variable number as the unused temporary so they will share the
 *    same slot in workspace .
 *    The unused temporary is then resurrected and a pointer to the new
 *    temporary node returned.
 *  If an unused temporary is not found the new temporary node is added
 *    to the variable table.
 *
 * To free a temporary we simply kill the entry in the variable table given by
 * the tempories variable number.
 */
/*}}}*/

/*{{{  PUBLIC void alloctemp(treenode *tempptr)*/
PUBLIC void alloctemp (treenode * tempptr)
{
	int i = var_base;
	/*INT32 size = wordsin(NTypeOf(tempptr)); */
	const INT32 size = numslots (tempptr);
	/* look for an unused temp of the same size */
#if 0
	while ((i < num_var) && (TagOf (var_list[i]) != T_TEMP || testbit (live_var, i) != 0 || size != numslots (var_list[i]))) {
		i++;
	}
#else
	/* look for preeval temps too! */
	while ((i < num_var) &&
	       ((TagOf (var_list[i]) != T_TEMP && TagOf (var_list[i]) != T_PREEVALTEMP) ||
		testbit (live_var, i) != 0 || size != numslots (var_list[i]))) {

		i++;
	}
#endif
	if (i < num_var) {
		/* Add the new temp node to the unused tempories variable chain
		 * and resurrect the old temporary
		 */
		DEBUG_MSG (("alloctemp: Re-using temp%d\n", i));
		addnamechain (var_list[i], tempptr);
		SetNVOffset (tempptr, 0);
		resurrect_var (var_list[i]);
	} else {
		/* Allocate a new variable */
		DEBUG_MSG (("alloctemp: New temp%d\n", i));
		create_var (tempptr);
	}
}

/*}}}*/
/*{{{  PRIVATE treenode *newtypedtempnode(t, tptr, mode, type)*/
PRIVATE treenode *newtypedtempnode (const int t, treenode * const tptr, const int mode, treenode * const type, const int current_lexlevel)
{
	treenode *const tempptr = newnamenode (t, genlocn, tempname_p, type, tptr,
					       current_lexlevel, 0, mode);
	DEBUG_MSG (("newtypedtempnode\n"));
	SetNVOffset (tempptr, NO_SLOT);
	SetNVNext (tempptr, NULL);
	setusecount (tempptr, 1);	/* make sure it always gets a slot allocated */
	return (tempptr);
}

/*}}}*/
/*{{{  PUBLIC treenode *newtempnode(t, tptr, mode)*/
PUBLIC treenode *newtempnode (const int t, treenode * const tptr, const int mode, const int current_lexlevel)
{
	treenode *tmpnode;

	tmpnode = newtypedtempnode (t, tptr, mode, gettype_main (tptr), current_lexlevel);

	/* We call gettype_main so that any type tree fabricated is created
	   in real workspace, not temporary workspace */

#if 0
	/* tried using copytree here to fix bug INSdi03067 - unsuccessful */
	return (newtypedtempnode (t, tptr, mode, copytree (gettype (tptr), current_lexlevel), current_lexlevel));
#endif

	return tmpnode;
}

/*}}}*/
/*{{{  PUBLIC treenode *gettemp(tptr, mode)*/
PUBLIC treenode *gettemp (treenode * const tptr, const int mode)
{
	treenode *const tempptr = newtempnode (T_TEMP, tptr, mode, be_lexlevel);
	alloctemp (tempptr);

#if 0
fprintf (stderr, "gettemp: returning tempptr =");
printtreenl (stderr, 4, tempptr);
#endif
	return (tempptr);
}

/*}}}*/
/*{{{  PUBLIC treenode *gettypedtemp(tptr, mode, type)*/
PUBLIC treenode *gettypedtemp (treenode * tptr, int mode, treenode * type)
{
	treenode *tempptr = newtypedtempnode (T_TEMP, tptr, mode, type, be_lexlevel);
	alloctemp (tempptr);

#if 0
fprintf (stderr, "gettypedtemp: returning tempptr =");
printtreenl (stderr, 4, tempptr);
#endif
	return (tempptr);
}

/*}}}*/
/*{{{  PUBLIC treenode *getopdtemp(int opdmode, treenode *opd)*/
/****************************************************************************
 *
 *  getopdtemp takes an operand (opdmode, opd) and returns a suitable temp.
 *             node for it.
 *             This is a bit of a kludge as it pretends a pointer is an
 *             integer in workspace; ideally, we should have proper pointer
 *             types in the backend.
 *
 ****************************************************************************/
PUBLIC treenode *getopdtemp (int opdmode, treenode * opd)
{
	return (gettemp (opd, (opdmode == P_PTR) ? NM_POINTER : NM_WORKSPACE));
}

/*}}}*/
/*{{{  PUBLIC void freetemp(temp)*/
PUBLIC void freetemp (treenode * temp)
{
	kill_var (temp);
}

/*}}}*/
/*{{{  PUBLIC void freeiftemp(treenode *tptr)*/
/*{{{  comment*/
/*****************************************************************************
 *
 *  freeiftemp takes a treenode 'tptr', and if it is a temporary, or
 *             evaluated temporary, frees it.
 *
 *****************************************************************************/
/*}}}*/
PUBLIC void freeiftemp (treenode * tptr)
{
	if (TagOf (tptr) == T_TEMP || TagOf (tptr) == T_PREEVALTEMP) {
		freetemp (tptr);
	}
}

/*}}}*/
/*}}}*/
/*{{{  PUBLIC void allocparams(nptr)*/
/*****************************************************************************
 *
 *  allocparams allocates workspace offsets to the formal parameters.
 *
 *****************************************************************************/
PUBLIC void allocparams (treenode * nptr)
{
	treenode *fparams = NParamListOf (nptr);
	/*{{{  allocate workspace positions to the parameters */
	{
		int hiddentempno = -1;	/* numbering for the hidden parameters */
		/*int paramposn = 1; */
		INT32 paramposn = NPMaxwspOf (nptr) + 1;
		int olddiagnostics = diagnostics;

		if (separatelycompiled (nptr)) {
			diagnostics = FALSE;
		}
		if (diagnostics) {
			fprintf (outfile, "******* Parameter workspace allocation ******\n");
		}
		if (need_wsmap) {
			wsmap_new_entry (paramposn, fparams, WSMAP_FIRSTPARAM, 0, NULL);
		}
		while (!EndOfList (fparams)) {
			treenode *thisfparam = ThisItem (fparams);

			switch (TagOf (thisfparam)) {
				/*{{{  formal parameter */
			case N_PARAM:
			case N_VALPARAM:
			case N_RESULTPARAM:
			case S_FNFORMALRESULT:
				if (basetype (gettype (thisfparam)) != S_TIMER) {
					/*{{{  allocate the parameter */
					SetNVOffset (thisfparam, paramposn);
					if (diagnostics) {
						fprintf (outfile, "Parameter %s at workspace offset %d, use count = %d\n",
							 WNameOf (NNameOf (thisfparam)), NVOffsetOf (thisfparam), NVUseCountOf (thisfparam));
					}
					allocate_overlays (NVNextOf (thisfparam), paramposn);
					if (need_wsmap) {
						int last_offset = NO_SLOT;

						wsmap_add_entry (thisfparam, &last_offset);
					}
					/*}}} */
				} else {
					/* decrement paramposn to cancel out the increment below */
					paramposn--;
				}
				break;
				/*}}} */
				/*{{{  hidden array dimension */
			case S_HIDDEN_PARAM:
				SetNVOffset (HExpOf (thisfparam), paramposn);
				/* added for more readability of assembly output: */
				SetNVVarNum (HExpOf (thisfparam), hiddentempno--);	/* 14/3/91 */
				if (diagnostics) {
					fprintf (outfile, "Hidden param at workspace offset %d\n", paramposn);
				}
				break;
				/*}}} */
				/*{{{  static link */
			case S_PARAM_STATICLINK:
				if (diagnostics) {
					fprintf (outfile, "Static link at workspace offset %d\n", paramposn);
				}
				if (need_wsmap) {
					wsmap_new_entry (paramposn, thisfparam, WSMAP_STATICLINK, 1, NULL);
				}
				break;
				/*}}} */
				/*{{{  vector space pointer */
			case S_PARAM_VSP:
				{
					treenode *vptr;
					for (vptr = vars_mapped_onto_vsptr; vptr != NULL; vptr = NVAllocNextOf (vptr)) {
						SetNVOffset (vptr, paramposn);
						if (diagnostics) {
							printdiagnostic (vptr, TRUE, FALSE);
						}
						allocate_overlays (NVNextOf (vptr), paramposn);
					}
					if (diagnostics) {
						fprintf (outfile, "Vectorspace pointer at workspace offset %d\n", paramposn);
					}
					if (need_wsmap) {
						wsmap_new_entry (paramposn, thisfparam, WSMAP_VSPTR, 1, NULL);
					}
				}
				break;
				/*}}} */
				/*{{{  fork-barrier pointer*/
			case S_PARAM_FB:
				if (diagnostics) {
					fprintf (outfile, "Fork barrier at workspace offset %d\n", paramposn);
				}
				if (need_wsmap) {
					wsmap_new_entry (paramposn, thisfparam, WSMAP_FB, 1, NULL);
				}
				break;
				/*}}}*/
				/*{{{  workspace size*/
			case S_PARAM_WS:
				if (diagnostics) {
					fprintf (outfile, "Workspace size at workspace offset %d\n", paramposn);
				}
				/*}}}*/
#ifdef MOBILES
				/*{{{  mobile-space pointer*/
			case S_PARAM_MSP:
				if (diagnostics) {
					fprintf (outfile, "Mobilespace pointer at workspace offset %d\n", paramposn);
				}
				if (need_wsmap) {
					wsmap_new_entry (paramposn, thisfparam, WSMAP_MSPTR, 1, NULL);
				}
				break;
				/*}}}*/
				/*{{{  mobile-process pointer*/
			case S_PARAM_MPP:
				if (diagnostics) {
					fprintf (outfile, "Mobile process pointer at workspace offset %d\n", paramposn);
				}
				if (need_wsmap) {
					wsmap_new_entry (paramposn, thisfparam, WSMAP_MPP, 1, NULL);
				}
				break;
				/*}}}*/
				/*{{{  hidden type node*/
			case S_HIDDEN_TYPE:
				if (diagnostics) {
					fprintf (outfile, "Hidden type at workspace offset %d\n", paramposn);
				}
				break;
				/*}}}*/
#endif
				/*{{{  function result pointer */
#if 0
			case S_FNFORMALRESULT:
				SetNVOffset (thisfparam, paramposn);
				if (diagnostics) {
					fputs ("Function result", outfile);
				}
				break;
#endif
				/*}}} */
			default:
				badtag (genlocn, TagOf (thisfparam), "allocparams");
			}
			paramposn++;
			fparams = NextItem (fparams);
		}
		if (diagnostics) {
			fprintf (outfile, "*********************************************\n");
		}
		diagnostics = olddiagnostics;
	}
	/*}}} */
}

/*}}}*/
/*{{{  PUBLIC void initalloc ()*/
PUBLIC void initalloc (void)
{
	check_bitmap_relationships ();

	staticlinkused = 0;
	initvsp (0);
	datasize = DS_MIN;
	fixedws_list = NULL;
	if (var_map == NULL) {
		/* incase this is called more than once (in configurer) */
		var_map = alloc_var_map ();
	}
	if (live_var == NULL) {
		live_var = alloc_live_var ();
	}
	if (var_list == NULL) {
		var_list = alloc_var_list ();
	}
}

/*}}}*/
/*{{{  PUBLIC void freevarmap*/
PUBLIC void freevarmap (void)
{
	if (var_map != NULL) {
		memfree (var_map);
		var_map = NULL;
	}
	if (live_var != NULL) {
		memfree (live_var);
		live_var = NULL;
	}			/* bug 1075 24/9/91 */
	if (var_list != NULL) {
		memfree (var_list);
		var_list = NULL;
	}			/* bug 1075 24/9/91 */
}

/*}}}*/
/*}}}*/
