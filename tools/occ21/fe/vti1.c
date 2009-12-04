/* $Id: vti1.c,v 1.4 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	Occam two virtual tree interface routines
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

/* adapted for long pointers to avoid alignment problems
   by MDP 26/04/95,28/11/95 */

/*{{{  include files*/
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <stdio.h>

#include "midinc.h"

#ifdef STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#include "vtierror.h"

#if defined(DEBUG) && defined(IMS)
#include <misc.h>
#endif
/*}}}*/

#if 0		/* for extensive debugging.. */
#define DEBUG
#define CHECK_MEMALLOC
#define CHECK_MEMALLOC_CHAIN
#endif

/*{{{  private variables*/
#ifdef DEBUG
PRIVATE int node_count[256];
#endif

/* We keep a free list chain for all sizes up to 15 words long.
   This covers all the tree nodes.
   (Changed '15' to size of treenodes, plus a bit).
*/
/*#define MAXFREEUPSIZE (15 * sizeof(int))*/
#define MAXFREEUPSIZE ((int)(sizeof(treenode) + 2*sizeof(int)))
PRIVATE void *freelist[MAXFREEUPSIZE];	/* will be initialised to NULLs */

PRIVATE SOURCEPOSN *vti_locn_ptr = NULL;
/*}}}*/
/*{{{  space allocation*/
/*{{{  comment on workspace handling*/
/*
 * Two workspaces are maintained:
 *   the real workspace is where the parse tree and symbol table are built up.
 *   The real workspace is never reused.
 *
 *   the temporary workspace is used for building up temporary tree
 *     structures, for instance by the checker.
 *     This can be freed up at request, and this is done after each major
 *     phase of the compiler.
 *
 *   switch_to_temp_workspace causes the free space allocator, newvec to
 *     satisfy future space requests from the temporary workspace.
 *
 *   switch_to_real_workspace causes the free space allocator, newvec, to
 *     satisfy future space requests from the real workspace.
 *     This is the initial state.
 *
 *  There is also a mechanism for marking and freeing real workspace.
 */
/*}}}*/

/*{{{  flags controlling w/s allocation strategy*/
/* when this is enabled, we perform a simple check for malloc overruns
   every time newvec needs more memory - default is TRUE */
/* (Normally ON) */
#define CHECK_MALLOC

/* When this is defined, we perform a simple check for malloc overruns
   every time memfree is called. Cheap enough to leave on permanently */
/* (Normally ON). */

#if 0
#define CHECK_MEMALLOC


/* when this is defined, all calls to newvec and freevec are mapped
   directly into calls to memalloc and memfree */
/* (Normally OFF) */

#define NEWVEC_IS_MEMALLOC

/* When this is defined, all calls to newvec are changed to calls to memalloc,
   and memalloc walks the entire list of malloc-ed blocks checking for
   over-runs and under-runs.
   VERY SLOW - Only used when debugging memory corruption */
/* (Normally OFF) */
#define CHECK_MEMALLOC_CHAIN
#endif


#if defined(CHECK_MEMALLOC_CHAIN) && !defined(CHECK_MEMALLOC)
#error CHECK_MEMALLOC_CHAIN should only be defined if CHECK_MEMALLOC is also defined
#endif

#if defined(CHECK_MALLOC) || defined(CHECK_MEMALLOC)
#define MAGIC1 12345678
#define MAGIC2 87654321
#endif

/* if this is defined, any freed-up areas are set to this value before
freeing */
#define CLOBBER_WHEN_FREEING (0xFF)
/*}}}*/
/*{{{  definitions*/
/* Both of these must be integer multiples of the host's alignment requirement. */
#define WORKSPACESIZE     50000
#define TEMPWORKSPACESIZE 10000

struct wsblock
{
#if defined(CHECK_MALLOC) || TRUE
	struct wsblock *prev;
#endif
#ifdef CHECK_MALLOC
	long magic1;		/* check for under-runs */
#endif
	char ws[WORKSPACESIZE];	/* This must align correctly */
#ifdef CHECK_MALLOC
	long magic2;		/* check for over-runs */
#endif
};
struct tempwsblock
{
	struct tempwsblock *prev;
#ifdef CHECK_MALLOC
	long magic1;		/* check for under-runs */
#endif
	char ws[TEMPWORKSPACESIZE];	/* This must align correctly */
#ifdef CHECK_MALLOC
	long magic2;		/* check for over-runs */
#endif
};

/*}}}*/

/*{{{  PRIVATE variables*/
PRIVATE int vecp, tempvecp, blocks, tempblocks;
PRIVATE struct wsblock *workspace = NULL;
/*PRIVATE char *tempworkspace;*/
PRIVATE struct tempwsblock *tempworkspace;
PRIVATE int tempworkspaceflag;

#ifdef DEBUG
PRIVATE size_t small_malloc = 0;
PRIVATE size_t large_malloc = 0;
PRIVATE size_t newvec_size = 0;
PRIVATE size_t newvec_temp = 0;
PRIVATE int malloc_count = 0;
PRIVATE int free_count = 0;
PRIVATE int newvec_count = 0;
PRIVATE int newvec_malloc = 0;
#ifdef IMS
PRIVATE size_t total_malloc = 0;
#endif
#endif

#ifdef CHECK_MEMALLOC_CHAIN
PRIVATE void **memalloc_chain = NULL;
#endif
/*}}}*/
/*{{{  PRIVATE void check_memalloc_chain*/
#ifdef CHECK_MEMALLOC_CHAIN
PRIVATE void
check_memalloc_chain (void)
{
	void **ptr;
	for (ptr = memalloc_chain; ptr != NULL; ptr = *ptr) {
		long *const p = (long *) ((char *) ptr + sizeof (void *));
		if ((p[0] != MAGIC1) || (p[p[1] + 2] != MAGIC2))
			err_abort ("check_memalloc_chain");
	}
}
#endif
/*}}}*/
/*{{{  PUBLIC void *memalloc(size)*/
PUBLIC void *memalloc (const size_t size)
{
	void *p;
#ifdef CHECK_MEMALLOC
	{
		const size_t size_ints = (size + sizeof (long) - 1) / sizeof (long);
#ifdef CHECK_MEMALLOC_CHAIN
		check_memalloc_chain ();
		p = malloc (((size_ints + 3) * sizeof (long)) + sizeof (void *));
		if (p != NULL) {
			*((void **) p) = memalloc_chain;
			memalloc_chain = p;
			p = (char *) p + sizeof (void *);
		}
#else
		p = malloc ((size_ints + 3) * sizeof (long));
#endif
		if (p != NULL) {
			((long *) p)[0] = MAGIC1;
			((long *) p)[1] = size_ints;
			((long *) p)[size_ints + 2] = MAGIC2;
			p = (void *) ((long *) p + 2);	/* increment by two long words */
		}
	}
#else
	p = malloc (size);
#endif
	/*{{{  debugging */
#ifdef DEBUG
	malloc_count++;
	if (size > 500 || size == 0) {
		large_malloc += size;
		DEBUG_MSG (("memalloc(%d), ", size));
	} else {
		small_malloc += size;
	}
#ifdef IMS
	/* the word below the pointer returned by malloc contains the
	   length actually allocated
	 */
	if (p != NULL) {
		total_malloc += ((int *) p)[-1];
	}
#endif
#endif
	/*}}} */
	if (size == 0) {
		err_abort ("memalloc");
	}
	if (p == NULL) {
		/*{{{  error */
		DEBUG_MSG (("memalloc: malloc of %d bytes returned NULL\n", size));
		/*vtiabort(VTI_OUT_OF_SPACE, vti_locn_ptr == NULL ? NOPOSN : *vti_locn_ptr); */
		msg_out (SEV_ABORT, ANY_MODULE, ANY_OUT_OF_SPACE, vti_locn_ptr == NULL ? NOPOSN : *vti_locn_ptr);
		/*}}} */
	}
	return p;
}

/*}}}*/
/*{{{  PUBLIC void memfree(ptr)*/
PUBLIC void memfree (void *const ptr)
{
#ifdef CHECK_MEMALLOC_CHAIN
	{
		void *dummy;
		dummy = ptr;
		check_memalloc_chain ();
	}
#else
	/*{{{  debugging */
#ifdef DEBUG
	free_count++;
#ifdef IMS
	/* the word below the pointer returned by malloc contains the
	   length actually allocated
	 */
	if (ptr != NULL)
		total_malloc -= ((int *) ptr)[-1];
#endif
#endif
	/*}}} */
#ifdef CHECK_MEMALLOC
	if (ptr != NULL) {
		long *const base = (long *) ptr - 2;
		const long size = base[1];
		if ((base[0] != MAGIC1) || (base[size + 2] != MAGIC2))
			err_abort ("memfree");
		free (base);
	}
#else
	free (ptr);
#endif
#endif
}

/*}}}*/

/*{{{  PRIVATE void check_tempblocks*/
#ifdef CHECK_MALLOC
PRIVATE void check_tempblocks (const struct tempwsblock *t)
{
	for (; t != NULL; t = t->prev)
		if ((t->magic1 != MAGIC1) || (t->magic2 != MAGIC2))
			err_abort ("check_tempblocks");
}
#endif

/*}}}*/
/*{{{  PUBLIC void *newvec*/
/* Allocate n bytes of workspace and return a pointer to them */
PUBLIC void *newvec (const size_t asked_n)
{
#if defined (CHECK_MEMALLOC_CHAIN) || defined (NEWVEC_IS_MEMALLOC)
	return memalloc (asked_n);
#else
	size_t n;
	/*{{{  align n to a multiple of HOST_ALIGN_SIZE */
	/* n must be a multiple of the host's alignment size, and at least as
	   big as a pointer (since blocks on the freelist have the pointer to the next
	   block stored in them). We assume the host needs pointer alignment. */
	{
		const int tmp = asked_n % sizeof (void *);
		if (tmp != 0)
			n = asked_n + (sizeof (void *) - tmp);
		else
			n = asked_n;
	}
	/*}}} */
	if (tempworkspaceflag)
		/*{{{  allocate from temporary workspace */
	{
		/*{{{  debugging */
#ifdef DEBUG
		newvec_temp += n;
#endif
		/*}}} */
		if (n > TEMPWORKSPACESIZE)
			/*{{{  just malloc a chunk */
		{
#if 0
			DEBUG_MSG (("newvec: out of temp space\n"));
			/*vtiabort(VTI_OUT_OF_SPACE, vti_locn_ptr == NULL ? NOPOSN : *vti_locn_ptr); */
			msg_out (SEV_ABORT, ANY_MODULE, ANY_OUT_OF_SPACE, vti_locn_ptr == NULL ? NOPOSN : *vti_locn_ptr);
#endif
			return memalloc (n);
		}
		/*}}} */

		tempvecp -= n;
		if (tempvecp < 0)
			/*{{{  get a new chunk via malloc */
		{
			struct tempwsblock *t = (struct tempwsblock *)
				memalloc (sizeof (struct tempwsblock));
			DEBUG_MSG (("new temp block <%x>\n", (int) t));

			t->prev = tempworkspace;
#ifdef CHECK_MALLOC
			t->magic1 = MAGIC1;
			t->magic2 = MAGIC2;
			check_tempblocks (tempworkspace);
#endif
			tempworkspace = t;
			tempvecp = TEMPWORKSPACESIZE - n;
			tempblocks++;
		}
		/*}}} */
		return &(tempworkspace->ws[tempvecp]);
	}
	/*}}} */
	else
		/*{{{  allocate from permanent workspace */
	{
#ifdef DEBUG
		newvec_count++;
		newvec_size += n;
#endif
		if (n > WORKSPACESIZE)
			/*{{{  malloc a chunk */
		{
#if 0
			DEBUG_MSG (("newvec: out of normal space\n"));
			/*vtiabort(VTI_OUT_OF_SPACE, vti_locn_ptr == NULL ? NOPOSN : *vti_locn_ptr); */
			msg_out (SEV_ABORT, ANY_MODULE, ANY_OUT_OF_SPACE, vti_locn_ptr == NULL ? NOPOSN : *vti_locn_ptr);
#endif
			return memalloc (n);
		}
		/*}}} */
		if ((n < MAXFREEUPSIZE) && (freelist[n] != NULL))
			/*{{{  get from the freelist */
		{
			void *ptr = freelist[n];
			freelist[n] = *(void **) ptr;
			/*DEBUG_MSG(("newvec(free):%u ", n)); */
			return ptr;
		}
		/*}}} */
		DEBUG_MSG (("newvec:%u ", n));
		vecp -= n;
		if (vecp < 0)
			/*{{{  get a new chunk via malloc */
		{
			struct wsblock *t = (struct wsblock *)
				memalloc (sizeof (struct wsblock));
			/*{{{  debugging */
#ifdef DEBUG
			newvec_malloc += sizeof (struct wsblock);
			DEBUG_MSG (("newblock <%x>\n", (int) t));
#endif
			/*}}} */

#if defined(CHECK_MALLOC) || TRUE
			t->prev = workspace;
#endif
			/*{{{  checking */
#ifdef CHECK_MALLOC
			t->magic1 = MAGIC1;
			t->magic2 = MAGIC2;
			{
				struct wsblock *temp = workspace;
				while (temp != NULL) {
					if ((temp->magic1 != MAGIC1) || (temp->magic2 != MAGIC2))
						err_abort ("newvec");
					temp = temp->prev;
				}
			}
#endif
			/*}}} */

			workspace = t;
			vecp = WORKSPACESIZE - n;
			blocks++;
		}
		/*}}} */
		return &(workspace->ws[vecp]);
	}
	/*}}} */
#endif
}

/*}}}*/
/*{{{  PUBLIC void freevec*/
PUBLIC void
freevec (void *ptr, const size_t size)
{
#ifdef CLOBBER_WHEN_FREEING
	(void) memset (ptr, CLOBBER_WHEN_FREEING, size);
#endif

#if defined(CHECK_MEMALLOC_CHAIN) || defined(NEWVEC_IS_MEMALLOC)
	{
		size_t dummy;
		dummy = size;
		memfree (ptr);
	}
#else
	/*DEBUG_MSG(("freevec:%u ", size)); */
	if (size < MAXFREEUPSIZE) {
		*(void **) ptr = freelist[size];
		freelist[size] = ptr;
	}
#endif
}

/*}}}*/
/*{{{  markws() and freews UNUSED*/
#if 0				/*defined(CONFIG) */
#define MAXWSSTACK 10
struct wsmark
{
	struct wsblock *blocklist;
	int offset;
};
PRIVATE struct wsmark wsstack[MAXWSSTACK];
PRIVATE int wsstackptr;
PUBLIC void
markws (const SOURCEPOSN locn)
{
	if (wsstackptr >= MAXWSSTACK)
		vtiabort (VTI_STACK_OVERFLOW, locn);
	DEBUG_MSG (("Workspace mark <%x><%d>\n", (int) workspace, (int) vecp));
	wsstack[wsstackptr].blocklist = workspace;
	wsstack[wsstackptr].offset = vecp;
	wsstackptr++;
}
PUBLIC void
freews (const SOURCEPOSN locn)
{
	wsstackptr--;
	if (wsstackptr < 0)
		vtiabort (VTI_STACK_UNDERFLOW, locn);
	workspace = wsstack[wsstackptr].blocklist;
	vecp = wsstack[wsstackptr].offset;
	DEBUG_MSG (("freews: Workspace free <%x><%d>\n", (int) workspace, (int) vecp));
	if (workspace != NULL) {
		struct wsblock *wptr = workspace->next;
		while (wptr != NULL) {
			struct wsblock *t = wptr->next;
			DEBUG_MSG (("freed <%x>\n", (int) wptr));
			memfree (wptr);
			wptr = t;
		}
		workspace->next = NULL;
	}
}
#endif

/*}}}*/
/*{{{  PUBLIC int  switch_to_temp_workspace*/
PUBLIC int switch_to_temp_workspace (void)
{
	const int old = tempworkspaceflag;
	tempworkspaceflag = TRUE;
	return old;
}

/*}}}*/
/*{{{  PUBLIC int  switch_to_real_workspace*/
PUBLIC int switch_to_real_workspace (void)
{
	const int old = tempworkspaceflag;
	tempworkspaceflag = FALSE;
	return old;
}

/*}}}*/
/*{{{  PUBLIC void switch_to_prev_workspace*/
PUBLIC void switch_to_prev_workspace (const int old)
{
	tempworkspaceflag = old;
}

/*}}}*/
/*{{{  PUBLIC void freeup_temp_workspace*/
PUBLIC void freeup_temp_workspace (void)
{
#if 1
	DEBUG_MSG (("freeup_temp_workspace"));
	switch_to_real_workspace ();
#ifdef CHECK_MALLOC
	check_tempblocks (tempworkspace);
#endif
	while (tempworkspace != NULL) {
		struct tempwsblock *prev = tempworkspace->prev;
		DEBUG_MSG ((", freeing <%lx>", (BIT32) tempworkspace));
#ifdef CLOBBER_WHEN_FREEING
		(void) memset (tempworkspace, CLOBBER_WHEN_FREEING, sizeof (struct tempwsblock));
#endif
		memfree (tempworkspace);
		tempworkspace = prev;
	}
	DEBUG_MSG (("\n"));
	tempvecp = 0;		/* this forces another initial block to be created */
	tempblocks = 0;
#endif
	return;
}

/*}}}*/
/*{{{  PUBLIC void freeup_all_workspace*/
PUBLIC void
freeup_all_workspace (void)
{
	int i;
	DEBUG_MSG (("freeup_all_workspace"));
	switch_to_real_workspace ();
	while (workspace != NULL) {
		struct wsblock *prev = workspace->prev;
		DEBUG_MSG ((", freeing <%lx>", (BIT32) workspace));
#ifdef CLOBBER_WHEN_FREEING
		(void) memset (workspace, CLOBBER_WHEN_FREEING, sizeof (struct wsblock));
#endif
		memfree (workspace);
		workspace = prev;
	}
	DEBUG_MSG (("\n"));
	vecp = 0;		/* this forces another initial block to be created */
	blocks = 0;

	for (i = 0; i < MAXFREEUPSIZE; i++)
		freelist[i] = NULL;

	return;
}

/*}}}*/
/*{{{  PUBLIC long tablesize*/
PUBLIC long
tablesize (void)
{
	return (((long) WORKSPACESIZE * (long) blocks) - (long) vecp) + (((long) TEMPWORKSPACESIZE * (long) tempblocks) - (long) tempvecp);
}

/*}}}*/

/*}}}*/

/*{{{  new treenode creation*/
/*{{{  PRIVATE void checknodetype*/
PRIVATE void checknodetype (const int tag, const nodetypeoftag_t nodetype)
{
	if (nodetypeoftag (tag) != nodetype) {
fprintf (stderr, "checknodetype: check tag (%d) != %d\n", tag, nodetype);
abort ();
		badtag (NOPOSN, tag, "checknodetype");
	}
}

/*}}}*/

/*{{{  treenode *newactionnode (t, ln, l, r)*/
treenode *newactionnode (int t, SOURCEPOSN ln, treenode * l, treenode * r)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct actionnode_s));
	checknodetype (t, ACTIONNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetLHS (nptr, l);
	SetRHS (nptr, r);
	SetActionType (nptr, NULL);
	SetActionDuring (nptr, NULL);
	SetActionAfter (nptr, NULL);
	SetActionFlags (nptr, ActionFlag_default);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}
/*}}}*/
/*{{{  treenode *newactionnodex (t, ln, l, r, d, a)*/
treenode *newactionnodex (int t, SOURCEPOSN ln, treenode *l, treenode *r, treenode *d, treenode *a)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct actionnode_s));
	checknodetype (t, ACTIONNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetLHS (nptr, l);
	SetRHS (nptr, r);
	SetActionType (nptr, NULL);
	SetActionDuring (nptr, d);
	SetActionAfter (nptr, a);
	SetActionFlags (nptr, ActionFlag_default);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}
/*}}}  */
/*{{{  treenode *newaltnode (t, ln, g, i, b)*/
treenode *newaltnode (int t, SOURCEPOSN ln, treenode * g, treenode * i, treenode * b)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct altnode_s));
	checknodetype (t, ALTNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetAltGuard (nptr, g);
	SetAltInput (nptr, i);
	SetAltBody (nptr, b);
	SetAltChanExp (nptr, NULL);	/* added for bug 779 2/11/90 */
	SetAltTimeExp (nptr, NULL);	/* Added for bug TS/1626 21/04/92 */
	SetAltLabel (nptr, -1);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newarraysubnode (t, ln, d, tp)*/
treenode *newarraysubnode (int t, SOURCEPOSN ln, treenode * base, treenode * index)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct arraysubnode_s));
	checknodetype (t, ARRAYSUBNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetASBase (nptr, base);
	SetASIndex (nptr, index);
	SetASExp (nptr, NULL);
	SetASOffset (nptr, 0);
	SetASLength (nptr, NULL);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newcnode (t, ln, b)*/
treenode *newcnode (int t, SOURCEPOSN ln, treenode * b)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct cnode_s));
	checknodetype (t, CNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetCBody (nptr, b);
	SetCTemp (nptr, NULL);	/* Used in ALT constructors containing replicators */
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newcondnode (t, ln, condition, process)*/
treenode *newcondnode (int t, SOURCEPOSN ln, treenode * condition, treenode * process)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct condnode_s));
	checknodetype (t, CONDNODE);

	SetTag (nptr, t);
	SetLocn (nptr, ln);

	/* make sure the fields for other uses are clean */
	SetVRTaggedList (nptr, NULL);
	SetVRDuring (nptr, NULL);
	SetVRAfter (nptr, NULL);

#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetCondGuard (nptr, condition);
	SetCondBody (nptr, process);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}
/*}}}*/
/*{{{  treenode *newcondnodex (int t, SOURCEPOSN ln, treenode *inputlist, treenode *d_proc, treenode *a_proc)*/
treenode *newcondnodex (int t, SOURCEPOSN ln, treenode *inputlist, treenode *d_proc, treenode *a_proc)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct condnode_s));
	checknodetype (t, CONDNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
	SetVRTaggedList (nptr, inputlist);
	SetVRDuring (nptr, d_proc);
	SetVRAfter (nptr, a_proc);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}
/*}}}  */
/*{{{  treenode *newcondexpnode (t, ln, condition, true, false)*/
#ifdef CONDEXP
treenode *newcondexpnode (int t, SOURCEPOSN ln, treenode * condition, treenode * true, treenode * false)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct condexpnode_s));
	checknodetype (t, CONDEXPNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetCondExpGuard (nptr, condition);
	SetCondExpTrue (nptr, true);
	SetCondExpFalse (nptr, false);
	SetCondExpType (nptr, 0);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}
#endif

/*}}}*/
/*{{{  treenode *newconfignode*/
#if 1				/*def CONFIG */
treenode *newconfignode (int t, SOURCEPOSN ln, treenode * a, treenode * b, treenode * c)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct confignode_s));
	checknodetype (t, CONFIGNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
	SetSTDev (nptr, a);
	SetSTAttrName (nptr, b);
	SetSTAttrExp (nptr, c);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}
#endif

/*}}}*/
/*{{{  treenode *newconstexpnode (t, ln, e, h, l)*/
treenode *newconstexpnode (int t, SOURCEPOSN ln, treenode * e, BIT32 h, BIT32 l)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct constexpnode_s));
	checknodetype (t, CONSTEXPNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetCExp (nptr, e);
	SetHiVal (nptr, h);
	SetLoVal (nptr, l);
	SetCENext (nptr, NULL);
	SetCEOffset (nptr, -1);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newconstant*/
PUBLIC treenode *newconstant (const BIT32 n)
{
	static treenode *dummyexp_p = NULL;	/* yes, I _mean_ static */

	if (dummyexp_p == NULL) {
		const int old = switch_to_real_workspace ();
		dummyexp_p = newleafnode (S_DUMMYEXP, NOPOSN);
		switch_to_prev_workspace (old);
	}

	return newconstexpnode (S_CONSTEXP, NOPOSN, dummyexp_p, 0, n);
}

/*}}}*/
/*{{{  treenode *newintconstant*/
PUBLIC treenode *newintconstant (const BIT32 n, const int type)
{
	char num[30];		/* plenty big enough for a 32-bit integer */
	wordnode *literal;
	sprintf (num, "%d", n);
	literal = lookupword (num, strlen (num));

	return newconstexpnode (S_CONSTEXP, NOPOSN, newlitnode (S_UINTLIT, NOPOSN, (treenode *) literal, newleafnode (type, NOPOSN)), 0, n);
}

/*}}}*/
/*{{{  treenode *newlongintconstant*/
PUBLIC treenode *newlongintconstant (const BIT32 lo, const BIT32 hi, const char *const str, const int type)
{
	wordnode *literal = NULL;
	if (str != NULL)
		literal = lookupword (str, strlen (str));
	return newconstexpnode (S_CONSTEXP, NOPOSN, newlitnode (S_UINTLIT, NOPOSN, (treenode *) literal, newleafnode (type, NOPOSN)), hi, lo);
}

/*}}}*/
/*{{{  treenode *newrealconstant*/
PUBLIC treenode *newrealconstant (const BIT32 lo, const BIT32 hi, const char *const str, const int type)
{
	wordnode *literal = NULL;
	if (str != NULL)
		literal = lookupword (str, strlen (str));

	return newconstexpnode (S_CONSTEXP, NOPOSN, newlitnode (S_UREALLIT, NOPOSN, (treenode *) literal, newleafnode (type, NOPOSN)), hi, lo);
}

/*}}}*/
/*{{{  treenode *newtypedconstant*/
PUBLIC treenode *newtypedconstant (const BIT32 lo, const BIT32 hi, const char *const str, const int type)
{
	if (isreal (type))
		return newrealconstant (lo, hi, str, type);
	else if (hi == 0)
		return newintconstant (lo, type);
	else
		return newlongintconstant (lo, hi, str, type);
}

/*}}}*/
/*{{{  treenode *newconsttablenode (t, ln, v, e)*/
treenode *newconsttablenode (int t, SOURCEPOSN ln, wordnode * v, treenode * e)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct consttablenode_s));
	checknodetype (t, CONSTTABLENODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetCTVal (nptr, v);
	SetCTExp (nptr, e);
	SetCTNext (nptr, NULL);
	SetConstTableType (nptr, NULL);
	SetCTLabel (nptr, -1);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newdeclnode (t, ln, n, v)*/
treenode *
newdeclnode (int t, SOURCEPOSN ln, treenode * n, treenode * val, treenode * b)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct declnode_s));
	checknodetype (t, DECLNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetDName (nptr, n);
	SetDVal (nptr, val);
	SetDBody (nptr, b);
	SetDExtra (nptr, NULL);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newdopnode (t, ln, l, r)*/
treenode *
newdopnode (int t, SOURCEPOSN ln, treenode * l, treenode * r, int type)
{
	const int min_size = max_INT32 (sizeof (struct dopnode_s), sizeof (struct instancenode_s));

	treenode *nptr = (treenode *) newvec (TREENODEBASE + min_size);
	checknodetype (t, DOPNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetLeftOp (nptr, l);
	SetRightOp (nptr, r);
	SetDOpType (nptr, type);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newhiddenparamnode (t, ln, e, d)*/
#if 0
treenode *
newhiddenparamnode (const int t, const SOURCEPOSN ln, treenode * const e, const int d, const int lexlevel, const int arg)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct hiddenparamnode_s));
	checknodetype (t, HIDDENPARAMNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetHExp (nptr, e);
	SetHDimension (nptr, d);
	SetHLexLevel (nptr, lexlevel);
	SetHArg (nptr, arg);

#ifdef COMPILING_TO_JCODE
	SetHBinder (nptr, NULL);	/* Used for  J_CODE creation */
#endif

#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}
#endif
/*}}}*/
/*{{{  treenode *newinstancenode (t, ln, n, p)*/
treenode *newinstancenode (const int t, const SOURCEPOSN ln, treenode * const n, treenode * const p)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct instancenode_s));
	checknodetype (t, INSTANCENODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetIName (nptr, n);
	SetIParamList (nptr, p);
	SetILoadSeq (nptr, 0);
	SetIRecursive (nptr, 0);
	SetIFork (nptr, 0);
	SetIForked (nptr, 0);
	SetIDynmem (nptr, 0);
	SetIDynaddr (nptr, NULL);
	SetIRPSlots (nptr, 0);
#ifdef MOBILES
	SetIRMSPOffset (nptr, 0);
#endif
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newleafnode (t, ln)*/
treenode *newleafnode (int t, SOURCEPOSN ln)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct leafnode_s));
	checknodetype (t, LEAFNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
	SetLeafLink (nptr, NULL);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newlistnode (t, ln, l, r)*/
treenode *newlistnode (const int t, const SOURCEPOSN ln, treenode * const l, treenode * const r)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct listnode_s));
	checknodetype (t, LISTNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetLeft (nptr, l);
	SetRight (nptr, r);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newlitnode (t, ln, w)*/
treenode *newlitnode (const int t, const SOURCEPOSN ln, treenode * const e, treenode * const type)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct litnode_s));
	checknodetype (t, LITNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetLitExp (nptr, e);
	SetLitType (nptr, type);
	SetLitUnspec (nptr, FALSE);	/* added by Jim 6/3/97 for udo's, see vti.h etc */
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newmopnode (t, ln, o)*/
treenode *newmopnode (int t, SOURCEPOSN ln, treenode * o, int type)
{
	const int min_size = max_INT32 (sizeof (struct mopnode_s), sizeof (struct instancenode_s));

	treenode *nptr = (treenode *) newvec (TREENODEBASE + min_size);

	checknodetype (t, MOPNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetOp (nptr, o);
	SetMOpType (nptr, type);
	SetOpTypeAttr (nptr, 0);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newnamenode(tag, ln, name, type, spec, lexlevel, scope, mode)*/
PUBLIC treenode *newnamenode (const int tag, const SOURCEPOSN ln, wordnode * const name,
	     treenode * const type, treenode * const spec, const int lexlevel, const int scope, const int mode)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct namenode_s));
	static const struct namenode_s zeronode = { 0 };
	/* we insert an initialiser to make icc put this into the text section;
	   otherwise it thinks that it might be a tentative declaration with a
	   different initialiser later, which might consist of non-NULL function
	   pointers, etc.
	 */
	checknodetype (tag, NAMENODE);
	nptr->n_u.n_s = zeronode;	/* Zero the miscellaneous bits just to be sure */
	SetTag (nptr, tag);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetNName (nptr, name);
	SetNType (nptr, type);
	SetNDecl (nptr, spec);
	SetNLexLevel (nptr, lexlevel);
/*SetNNestedPriPar(nptr, FALSE);*//* done automatically by the copy of the static variable */
/*SetNUsed(nptr, FALSE);*//* done automatically by the copy of the static variable */
	SetNScope (nptr, scope);
	SetNMode (nptr, mode);
	SetNTypeAttr (nptr, 0);
/*SetNChecker(nptr, NULL);*//* done automatically by the copy of the static variable */
	SetNUndef (nptr, NULL);
	SetNFMCheck (nptr, NULL);

	/* specific initialisation for Vars, PROCs etc: */
	switch (nametypeoftag (tag)) {	/* This tells us which part of the union is relevant */
	case NAMENODE_VAR:
		SetNVShared (nptr, FALSE);
		SetNVAliased (nptr, FALSE);
		SetNVRDefined (nptr, FALSE);
		SetNVDevice (nptr, FALSE);
		SetNVEndPosn (nptr, NOPOSN);
		SetNVNameUsed (nptr, FALSE);
		SetNVIndirect (nptr, FALSE);
		break;
	case NAMENODE_ROUTINE:
		SetNPRecursive (nptr, 0);
		SetNPForks (nptr, 0);
		SetNPSuspends (nptr, 0);
		SetNPDyncall (nptr, 0);
		break;
	default:
		break;
	}
#ifdef DEBUG
	node_count[tag]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newoverlapnode (t, ln, b1, c1, b2, c2) UNUSED*/
#if 0				/* never used */
PRIVATE treenode *newoverlapnode (int t, SOURCEPOSN ln, treenode *b1, treenode *c1, treenode *b2, treenode *c2)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct overlapnode_s));
	checknodetype (t, OVERLAPNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
	SetOBase1 (nptr, b1);
	SetOCount1 (nptr, c1);
	SetOBase2 (nptr, b2);
	SetOCount2 (nptr, c2);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}
#endif /* 0 */

/*}}}*/
/*{{{  treenode *newprocessornode (t, ln, condition, process)*/
treenode *
newprocessornode (int t, SOURCEPOSN ln, treenode * exp, wordnode * type, treenode * process)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct processornode_s));
	checknodetype (t, PROCESSORNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
	SetProcessorExp (nptr, exp);
	SetProcessorType (nptr, type);
	SetProcessorBody (nptr, process);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newreplcnode (t, ln, n, s, l, step, b)*/
treenode *
newreplcnode (int t, SOURCEPOSN ln, treenode * n, treenode * s, treenode * l, treenode * step, treenode * b)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct replcnode_s));
	checknodetype (t, REPLCNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetReplCName (nptr, n);
	SetReplCStartExp (nptr, s);
	SetReplCLengthExp (nptr, l);
	SetReplCStepExp (nptr, step);
	SetReplCBody (nptr, b);
	SetReplCTemp (nptr, NULL);	/* Used in ALT constructors containing replicators */
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newsegmentnode (t, ln, n, s, l)*/
treenode *
newsegmentnode (int t, SOURCEPOSN ln, treenode * n, treenode * s, treenode * l)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct segmentnode_s));
	checknodetype (t, SEGMENTNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetSName (nptr, n);
	SetSStartExp (nptr, s);
	SetSLengthExp (nptr, l);
	SetSSubscriptExp (nptr, NULL);
	SetSCheckExp (nptr, NULL);
	SetSLength (nptr, NULL);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newspacenode (t, ln, p, maxwsp, datasize,... )*/
#ifdef MOBILES
treenode *newspacenode (int t, SOURCEPOSN ln, treenode * p, BIT32 maxwsp, BIT32 datasize, BIT32 vsusage, BIT32 nestedvs, BIT32 msusage, BIT32 nestedms, treenode * namechain, int cpoffset)
#else
treenode *newspacenode (int t, SOURCEPOSN ln, treenode * p, BIT32 maxwsp, BIT32 datasize, BIT32 vsusage, BIT32 nestedvs, treenode * namechain, int cpoffset)
#endif
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct spacenode_s));
	checknodetype (t, SPACENODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
	SetSpBody (nptr, p);
	SetSpMaxwsp (nptr, maxwsp);
	SetSpDatasize (nptr, datasize);
	SetSpVSUsage (nptr, vsusage);
	SetSpNestedVS (nptr, nestedvs);
#ifdef MOBILES
	SetSpMSUsage (nptr, msusage);
	SetSpNestedMS (nptr, nestedms);
	SetSpMSPtr (nptr, NULL);
	SetSpWSMap (nptr, NULL);
	SetSpMapLabel (nptr, -1);
#endif
	SetSpNamechain (nptr, namechain);
	SetSpCPOffset (nptr, cpoffset);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newtypenode (t, ln, d, tp)*/
treenode *newtypenode (int t, SOURCEPOSN ln, treenode * d, treenode * tp)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct typenode_s));
	checknodetype (t, TYPENODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetARDimLength (nptr, d);
	SetARType (nptr, tp);
	/* This field is filled in by chk if the dimension is known */
	SetARDim (nptr, -1);
	SetMTDLab (nptr, -1);
	SetTypeAttr (nptr, TypeAttr_default);
	SetTypeTraces (nptr, NULL);
	SetARAlignment (nptr, NULL);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/
/*{{{  treenode *newvalofnode (t, ln, b, r)*/
treenode *newvalofnode (int t, SOURCEPOSN ln, treenode * b, treenode * r)
{
	treenode *nptr = (treenode *) newvec (TREENODEBASE + sizeof (struct valofnode_s));
	checknodetype (t, VALOFNODE);
	SetTag (nptr, t);
	SetLocn (nptr, ln);
#ifdef COMPILING_TO_JCODE
	SetDbgPList (nptr, NULL);
#endif
	SetVLBody (nptr, b);
	SetVLResultList (nptr, r);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/

/*{{{  wordnode *newwordnode (t, name, len, next)*/
wordnode *newwordnode (int t, const char *name, int len, wordnode * next)
{
	wordnode *nptr = (wordnode *) newvec (sizeof (wordnode));
	checknodetype (t, WORDNODE);
	SetWTag (nptr, t);
	SetWName (nptr, name);
	SetWLength (nptr, len);
	SetWNext (nptr, next);
#ifdef DEBUG
	node_count[t]++;
#endif
	return (nptr);
}

/*}}}*/

/*{{{  PRIVATE void print_node_count()*/
#ifdef NOTHING_AT_ALL /*DEBUG*/
	PRIVATE void
print_node_count (void)
{
	int i, tot = 0;
	for (i = 0; i < 256; i++) {
		tot += node_count[i];
		if (node_count[i])
			printf ("%-16s - %d\n", itagstring (i), node_count[i]);
	}
	printf ("Total new nodes: %d\n", tot);
}
#endif /* DEBUG */
/*}}}*/
/*}}}*/
/*{{{  tree accessing*/
/*{{{  treenode *NParamListOf (treenode *const n)*/
treenode *NParamListOf (treenode *const n)
{
	switch (TagOf (n)) {
	case N_PROCDEF:
#ifdef MOBILES
	case N_MPROCDECL:
	case N_PROCTYPEDECL:
#endif
	case N_SCPROCDEF:
	case N_LIBPROCDEF:
	case N_LIBMPROCDECL:
	case N_STDLIBPROCDEF:
	case N_INLINEPROCDEF:
	case N_PREDEFPROC:
		return (NTypeOf (n));
	case N_SFUNCDEF:
	case N_LFUNCDEF:
	case N_SCFUNCDEF:
	case N_LIBFUNCDEF:
	case N_STDLIBFUNCDEF:
	case N_INLINEFUNCDEF:
	case N_PREDEFFUNCTION:
		return (FnParamsOf (NTypeOf (n)));
	case N_DECL:
	case N_PARAM:
	case N_RESULTPARAM:
	case N_ABBR:
		/* if this is a MOBILE . PROC TYPE, return its parameter list */
		if ((TagOf (NTypeOf (n)) == S_MOBILE) && (TagOf (MTypeOf (NTypeOf (n))) == N_PROCTYPEDECL)) {
			return NTypeOf (MTypeOf (NTypeOf (n)));
		}
		/* fall through */
	default:
		badtag (NOPOSN, TagOf (n), "NParamListOf");
		return NULL;	/* not actually reached */
	}
}

/*}}}*/
/*{{{  void SetNParamList (treenode *const n, treenode *const v)*/
void SetNParamList (treenode *const n, treenode *const v)
{
	switch (TagOf (n)) {
	case N_PROCDEF:
#ifdef MOBILES
	case N_MPROCDECL:
	case N_PROCTYPEDECL:
#endif
	case N_SCPROCDEF:
	case N_LIBPROCDEF:
	case N_LIBMPROCDECL:
	case N_STDLIBPROCDEF:
	case N_INLINEPROCDEF:
	case N_PREDEFPROC:
		SetNType (n, v);
		break;
	case N_SFUNCDEF:
	case N_LFUNCDEF:
	case N_SCFUNCDEF:
	case N_LIBFUNCDEF:
	case N_STDLIBFUNCDEF:
	case N_INLINEFUNCDEF:
	case N_PREDEFFUNCTION:
		SetFnParams (NTypeOf (n), v);
		break;
	default:
		badtag (NOPOSN, TagOf (n), "SetNParamList");
		break;
	}
}

/*}}}*/
#ifdef OCCAM2_5
/*{{{  PUBLIC int NFieldsOf (treenode *type)*/
PUBLIC int NFieldsOf (treenode *type)
{
	int fields = 0;
	treenode *decl;

	type = follow_user_type (type);

	if (TagOf (type) == S_RECORD)
		for (decl = ARTypeOf (type); decl != NULL; decl = DBodyOf (decl))
			fields++;

	return fields;
}
/*}}}*/
#endif
/*}}}*/

/*{{{  PUBLIC void vtiinit*/
PUBLIC void
vtiinit (FILE * fptr, const INT32 no_slot_value)
{
	static BOOL initialised = FALSE;	/* YES - static */

	if (!initialised) {

#if 0				/*defined(CONFIG) */
		wsstackptr = 0;
#endif
		init_nodetypeoftag ();
		initcopytree ();
/*vti_outfile = fptr; *//* where to print trees */
		USE_VAR (fptr);	/* stop unused variable warning */
		vti_no_slot_value = no_slot_value;	/* Used for printing trees */
		tempworkspace = NULL;
		tempvecp = 0;	/* this forces an initial block to be created */
		tempblocks = 0;
		vecp = 0;	/* this forces an initial block to be created */
		blocks = 0;
		switch_to_real_workspace ();

		initialised = TRUE;
	}
}

/*}}}*/
/*{{{  PUBLIC void vtifinish*/
#ifdef _ICC
#ifndef DEBUG
#pragma IMS_nosideeffects (vtifinish)
#endif
#endif
PUBLIC void
vtifinish (FILE * fptr)
{
#ifdef DEBUG
	fprintf (fptr, "VTIP diagnostics:\n");
	fprintf (fptr, "Total number of calls to malloc: %d, total calls to free: %d", malloc_count, free_count);
	fprintf (fptr, " (Diff is %d)\n", malloc_count - free_count);
	fprintf (fptr, "Total size of small mallocs (< 500): %d, total size of large mallocs: %d\n", small_malloc, large_malloc);
	fprintf (fptr, "Total malloc-ed size: %d, tree size: %ld", small_malloc + large_malloc, tablesize ());
#ifdef IMS
	fprintf (fptr, " (actual left is %u)", total_malloc);
#endif
	fprintf (fptr, "\nThere were %d calls to newvec, for a total of %d bytes\n", newvec_count, newvec_size);
	fprintf (fptr, "(or %d from malloc). Plus %d temp bytes\n", newvec_malloc, newvec_temp);
#ifdef IMS
	fprintf (fptr, "Max stack usage is: %ld\n", max_stack_usage ());
#endif /* IMS */
#if 0				/*defined(SUN) || defined(GNU) */
	fprintf (fptr, "mallocmap:\n");
	mallocmap ();
#endif
	/*print_node_count(); */
#else
	USE_VAR (fptr);		/* stop unused variable warning */
#endif /* DEBUG */

#ifdef DEBUG
	{
		int i;
		for (i = 0; i < MAXFREEUPSIZE; i++) {
			int count = 0;
			void **p;
/* this blows something up... */
/*
			for (p = (void **) freelist[i]; p != NULL; p = *p)
				count++; */
			if (count != 0)
				fprintf (fptr, "freelist[%d]:%d(%d) ", i, count, i * count);
		}
		fputc ('\n', fptr);
	}
#endif /* DEBUG */
}

/*}}}*/
/*{{{  PUBLIC void set_vtilocn*/
PUBLIC void
set_vtilocn (SOURCEPOSN * locn_ptr)
{
	vti_locn_ptr = locn_ptr;
}

/*}}}*/
