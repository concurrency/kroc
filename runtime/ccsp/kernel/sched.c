/*
 *	Transputer style kernel in mixed C/asm
 *	Copyright (C) 1998 Jim Moores
 *	Based on the KRoC/sparc kernel Copyright (C) 1994-2000 D.C. Wood and P.H. Welch
 *	RMOX hacks copyright (C) 2002 Fred Barnes and Brian Vinter
 *	Portions copyright (C) 1999-2005 Fred Barnes <frmb@kent.ac.uk>
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

//#define CHECKING_MODE

/*{{{  includes, etc.*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined(RMOX_BUILD)
	#include <rmox_if.h>
#else
	#include <stdio.h>
	#ifdef HAVE_STDLIB_H
	#include <stdlib.h>
	#endif	/* HAVE_STDLIB_H */
	#include <sys/types.h>
#endif

#define __SCHED_C

#ifdef USE_PTHREADS
	#include <pthread.h>
#endif

#include <inlining.h>
#include <kernel.h>
#include <rts.h>
#include <ccsp_if.h>
#include <dmem_if.h>

#include <arch/alignment.h>
#include <arch/asm_ops.h>
#include <arch/atomics.h>
#include <arch/sched_asm_inserts.h>
#include <calltable.h>
#include <ccsp_timer.h>

#define MT_CCSP 1
#define MT_DEFINES 1
#include <mobile_types.h>

#include <typedesc.h>

#include <deadlock.h>
#include <dynproc.h>
#include <mobproc.h>

#include <mwsync.h>
#include <mws_sync.h>

#include <debug.h>

#if defined(BLOCKING_SYSCALLS) && !defined(RMOX_BUILD)
	#include <bsyscalls_if.h>
#endif

#if defined(ENABLE_DTRACES) && !defined(RMOX_BUILD)
	#include <dtrace.h>
#endif

#if DEFAULT_SCHED_POLICY == SCHED_POLICY_US
	#define SCHED_POLICY(me,other) QUEUE(other)
#elif DEFAULT_SCHED_POLICY == SCHED_POLICY_OTHER
	#define SCHED_POLICY(me,other) QUEUE(me); me = other
#elif DEFAULT_SCHED_POLICY == SCHED_POLICY_NEITHER_US
	#define SCHED_POLICY(me,other) QUEUE(me); QUEUE(other); me = GetNextProcess_NonEmptyQ()
#elif DEFAULT_SCHED_POLICY == SCHED_POLICY_NEITHER_OTHER
	#define SCHED_POLICY(me,other) QUEUE(other); QUEUE(me); me = GetNextProcess_NonEmptyQ()
#else
	#define SCHED_POLICY(me,other) fprintf(stderr, "BUG! " __FILE__ ":" #__LINE__ "\n");
#endif

#define MWSDEBUG 0

/*}}}*/

/*{{{  PUBLIC variables that are used elsewhere in the run time system */
#if defined(RMOX_BUILD)
word *inttab[RMOX_NUM_INTERRUPTS];			/* interrupt process linkage (Wptr of waiting process) */
volatile word intcount[RMOX_NUM_INTERRUPTS];		/* count of interrupts received */
extern void no_dynamic_process_support (void);
#endif
/*}}}*/
/*{{{  global variables*/
#if defined(USE_TLS)
__thread sched_t	*_ccsp_scheduler		CACHELINE_ALIGN;
#elif defined(ENABLE_MP) && defined(USE_PTHREADS)
static pthread_key_t 	scheduler_key			CACHELINE_ALIGN;
#else
sched_t			*_ccsp_scheduler 		CACHELINE_ALIGN = NULL;
#endif 

ccsp_global_t		_ccsp				CACHELINE_ALIGN = {};
#if 0
mwsyncglock_t 		mwaltlock 			CACHELINE_ALIGN;
#endif
/*}}}*/

/*{{{  ENTRY_TRACE macros*/
#ifdef ENABLE_KTRACES

	static word the_stackptr;

	#define ENTRY_TRACE(X, FMT, args...) \
		LOAD_ESP (the_stackptr); \
		MESSAGE ("<enter>   "#X" [0x%x,0x%8.8x,%p,%p,%p,%p] (esp=%8.8x) [%d:%d] ", (unsigned int)PPriority, (unsigned int)PState, Wptr, Fptr, Bptr, Tptr, the_stackptr, \
				(mdparam1 == 0xffffffff) ? 0 : ((mdparam1 >> 16) & 0xffff), (mdparam1 == 0xffffffff) ? 0 : (mdparam1 & 0xffff)); \
		if ((MaskedPPriority < 0) || (MaskedPPriority >= MAX_PRIORITY_LEVELS)) { MESSAGE ("b0rked!\n"); ccsp_kernel_exit(0,(int)Wptr); } \
		MESSAGE ("(" FMT ")\n", ##args)
	#define ENTRY_TRACE0(X) \
		LOAD_ESP (the_stackptr); \
		MESSAGE ("<enter>   "#X" [0x%x,0x%8.8x,%p,%p,%p,%p] (esp=%8.8x) [%d:%d]\n", (unsigned int)PPriority, (unsigned int)PState, Wptr, Fptr, Bptr, Tptr, the_stackptr, \
				(mdparam1 == 0xffffffff) ? 0 : ((mdparam1 >> 16) & 0xffff), (mdparam1 == 0xffffffff) ? 0 : (mdparam1 & 0xffff))
#else	/* !ENABLE_KTRACES */
	#define ENTRY_TRACE(X, FMT, args...)
	#define ENTRY_TRACE0(X)
#endif	/* !ENABLE_KTRACES */


/*}}}*/
/*{{{  DTRACE macro*/
#if defined(ENABLE_DTRACES)
	#define DTRACE(FMT,args...) \
		do_dtrace(FMT,##args)
#else	/* !defined (ENABLE_DTRACES) */
	#define DTRACE(FMT,args...)
#endif	/* !defined (ENABLE_DTRACES) */


/*}}}*/
/*{{{  checking mode macros*/
#ifdef CHECKING_MODE
#include <assert.h>
#define ASSERT(X) 	assert((X))
#define SAFETY 		if (true)
#define TRACE		if (true)
#else
#define ASSERT(X)	
#define SAFETY		if (false)
#define TRACE		if (false)
#endif
/*}}}*/

/*{{{  compiled in flags */
#ifdef BLOCKING_SYSCALLS
/*{{{  int scheduler_can_do_bsyscalls (void)*/
/* dummy function */
int scheduler_can_do_bsyscalls (void)
{
	return 1;
}
/*}}}*/
#endif
#ifdef ENABLE_CPU_TIMERS
/*{{{  int scheduler_can_do_cputimers (void)*/
/* dummy function */
int scheduler_can_do_cputimers (void)
{
	return 1;
}
/*}}}*/
#endif
#ifdef DYNAMIC_PROCS
/*{{{  int scheduler_can_do_dynproc (void)*/
/* dummy function */
int scheduler_can_do_dynproc (void)
{
	return 1;
}
/*}}}*/
#endif
/*{{{  int ccsp_enable_mp (void)*/
int ccsp_enable_mp (void)
{
	#if defined(ENABLE_MP)
	return 1;
	#else
	return 0;
	#endif
}
/*}}}*/
/*{{{  int ccsp_use_tls (void)*/
int ccsp_use_tls (void)
{
	#if defined(USE_TLS)
	return 1;
	#else
	return 0;
	#endif
}
/*}}}*/
/*}}}*/

/*{{{  scheduler support defines/functions */
/*{{{  scheduler pointer storage */
#if defined(USE_TLS)
static int init_local_schedulers (void) { return 0; }
#define set_local_scheduler(X)	do { _ccsp_scheduler = (X); } while (0)
sched_t *local_scheduler(void)
{
	return _ccsp_scheduler;
}
#elif defined(ENABLE_MP) && defined(USE_PTHREADS)
static pthread_key_t scheduler_key;

static int init_local_schedulers (void)
{
	return pthread_key_create (&scheduler_key, NULL);
}

static inline void set_local_scheduler (sched_t *scheduler)
{
	int ret = pthread_setspecific (scheduler_key, scheduler);
	ASSERT ( ret == 0 );
}

inline sched_t *local_scheduler (void)
{
	return (sched_t *) pthread_getspecific (scheduler_key);
}
#else
/* CGR FIXME: insert suitable RMOX code */
static int init_local_schedulers (void) { return 0; }
#define set_local_scheduler(X)	do { _ccsp_scheduler = (X); } while (0)
sched_t *local_scheduler(void)
{
	return _ccsp_scheduler;
}
#endif
/*}}}*/
/*{{{  batch state */
#define BATCH_DIRTY_BIT			31
#define BATCH_DIRTY			(1 << BATCH_DIRTY_BIT)
#define clean_batch(b) 			(!(att_val (&((b)->state)) & BATCH_DIRTY))
#define dirty_batch(b) 			(att_val (&((b)->state)) & BATCH_DIRTY)
#define mark_batch_clean(b) 		\
	do { att_clear_bit (&((b)->state), BATCH_DIRTY_BIT); } while (0)
#define mark_batch_dirty(b) 		\
	do { att_set_bit (&((b)->state), BATCH_DIRTY_BIT); } while (0)
#define set_batch_clean(b)		\
	do { att_set (&((b)->state), 0); } while (0)
#define set_batch_dirty(b)		\
	do { att_set (&((b)->state), BATCH_DIRTY); } while (0)
#define set_batch_dirty_value(b,v)	\
	do { att_set (&((b)->state), one_if_true ((v)) << BATCH_DIRTY_BIT); } while (0)
#define batch_window(b)			(att_val (&((b)->state)) & 0xff)
#define set_batch_window(b,w) 		\
	do { att_set (&((b)->state), BATCH_DIRTY | (w)); } while (0)
/*}}}*/
/*{{{  static void verify_batch_integrity (batch_t *batch) */
static void verify_batch_integrity (batch_t *batch)
{
	word *ptr = batch->Fptr;
	word size = 1;

	while (ptr[Link] != NotProcess_p) {
		ASSERT ( (batch->size & (~BATCH_EMPTIED)) > size );
		size++;
		ptr = (word *) ptr[Link];
	}

	ASSERT ( batch->Bptr == ptr );
	ASSERT ( (batch->size & (~BATCH_EMPTIED)) == size );
}
/*}}}*/
/*{{{  static TRIVIAL void release_clean_batch (sched_t *sched, batch_t *batch)*/
static TRIVIAL void release_clean_batch (sched_t *sched, batch_t *batch)
{
	ASSERT ( clean_batch (batch) );
	batch->next	= sched->free;
	sched->free	= batch;
}
/*}}}*/
/*{{{  static TRIVIAL void release_dirty_batch (sched_t *sched, batch_t *batch)*/
static TRIVIAL void release_dirty_batch (sched_t *sched, batch_t *batch)
{
	batch->next	= sched->laundry;
	sched->laundry	= batch;
}
/*}}}*/
/*{{{  static void do_laundry (sched_t *sched)*/
static void do_laundry (sched_t *sched)
{
	batch_t *b = sched->laundry, *p = NULL;
	
	while (b != NULL) {
		if (clean_batch (b)) {
			batch_t *n = b->next;
			reinit_batch_t (b);
			release_clean_batch (sched, b);
			if (p == NULL)
				sched->laundry = n;
			else
				p->next = n;
			b = n;
		} else {
			p = b;
			b = b->next;
		}
	}
}
/*}}}*/
/*{{{  static void release_excess_memory (sched_t *sched)*/
static void release_excess_memory (sched_t *sched)
{
	batch_t *b = sched->free;
	word count;

	for (count = 0; b != NULL && count < 32; ++count) {
		b = b->next;
	}

	if (b != NULL) {
		batch_t *n 	= b->next;
		b->next 	= NULL;
		b 		= n;

		while (b != NULL) {
			n = b->next;
			dmem_thread_release (sched->allocator, b);
			b = n;
		}
	}
}
/*}}}*/
/*{{{  static HOT void release_batch (sched_t *sched, batch_t *batch)*/
static HOT void release_batch (sched_t *sched, batch_t *batch)
{
	if (clean_batch (batch)) {
		reinit_batch_t (batch);
		release_clean_batch (sched, batch);
	} else {
		release_dirty_batch (sched, batch);
	}
}

/*}}}*/
/*{{{  static void allocate_to_free_list (sched_t *sched, unsigned int count)*/
static void allocate_to_free_list (sched_t *sched, unsigned int count)
{
	while (count--) {
		batch_t *b = dmem_thread_alloc (sched->allocator, BATCH_ALLOC_SIZE);
		init_batch_t (b);
		release_clean_batch (sched, b);
	}
}
/*}}}*/
/*{{{  static HOT batch_t *allocate_batch (sched_t *sched)*/
static HOT batch_t *allocate_batch (sched_t *sched) {
	batch_t *b = sched->free;

	if (b == NULL) {
		allocate_to_free_list (sched, 16);
		do_laundry (sched);
		b = sched->free;
	}

	sched->free = b->next;
	SAFETY { b->next = (batch_t *)(-1); }
	
	return b;
}
/*}}}*/
/*{{{  static TRIVIAL bar_head_t *allocate_bar_head (sched_t *sched)*/
static TRIVIAL bar_head_t *allocate_bar_head (sched_t *sched)
{
	bar_head_t *bh = (bar_head_t *) allocate_batch (sched);
	init_bar_head_t (bh);
	return bh;
}
/*}}}*/
/*{{{  static TRIVIAL void release_bar_head (sched_t *sched, bar_head_t *bh)*/
static TRIVIAL void release_bar_head (sched_t *sched, bar_head_t *bh)
{
	batch_t *b = (batch_t *) bh;
	init_batch_t (b);
	release_clean_batch (sched, b);
}
/*}}}*/
/*{{{  static TRIVIAL void setup_tqnode (tqnode_t *tn, word *wptr, word time, bool alt)*/
static TRIVIAL void setup_tqnode (tqnode_t *tn, word *wptr, word time, bool alt)
{
	tn->wptr 	= (word *) (((word )wptr) | alt);
	tn->time 	= time;
	set_batch_dirty_value ((batch_t *) tn, alt);
}

/*}}}*/
/*{{{  static TRIVIAL tqnode_t *init_tqnode (sched_t *sched, word *wptr, word time, word alt)*/
static TRIVIAL tqnode_t *init_tqnode (sched_t *sched, word *wptr, word time, word alt)
{
	tqnode_t *tn = (tqnode_t *) allocate_batch (sched);
	setup_tqnode (tn, wptr, time, alt);
	tn->scheduler = sched;
	return tn;
}
/*}}}*/
/*{{{  static TRIVIAL void release_tqnode (sched_t *sched, tqnode_t *tn)*/
static TRIVIAL void release_tqnode (sched_t *sched, tqnode_t *tn)
{
	release_batch (sched, (batch_t *) tn);
}
/*}}}*/
/*{{{  static TRIVIAL void save_priofinity (sched_t *sched, word *Wptr)*/
static TRIVIAL void save_priofinity (sched_t *sched, word *Wptr)
{
	Wptr[Priofinity] = sched->priofinity;
}
/*}}}*/
/*{{{  empty_batch(batch)*/
#define empty_batch(batch) ((batch)->Fptr == NotProcess_p)
/*}}}*/
/*{{{  end_of_curb(sched)*/
#define end_of_curb(sched) (((sched)->dispatches <= 0) || (sched->curb.Fptr == NotProcess_p))
/*}}}*/
/*{{{  static HOT void enqueue_to_batch_with_hint (batch_t *batch, word *Wptr, bool front)*/
static HOT void enqueue_to_batch_with_hint (batch_t *batch, word *Wptr, bool front)
{
	Wptr[Link] = NotProcess_p;

	if (front) {
		batch->Fptr = Wptr;
		batch->Bptr = Wptr;
		batch->size = 1;
	} else {
		batch->Bptr[Link] = (word) Wptr;
		batch->Bptr = Wptr;
		batch->size = batch->size + 1;
	}
}
/*}}}*/
/*{{{  static HOT void enqueue_to_batch (batch_t *batch, word *Wptr)*/
static HOT void enqueue_to_batch (batch_t *batch, word *Wptr)
{
	Wptr[Link] = NotProcess_p;

	if (batch->Fptr == NotProcess_p) {
		batch->Fptr = Wptr;
	} else {
		batch->Bptr[Link] = (word) Wptr;
	}

	batch->Bptr = Wptr;
	batch->size = batch->size + 1;
}
/*}}}*/
/*{{{  static WARM void enqueue_to_batch_front (batch_t *batch, word *Wptr)*/
static WARM void enqueue_to_batch_front (batch_t *batch, word *Wptr)
{
	if ((Wptr[Link] = (word) batch->Fptr) == NotProcess_p) {
		batch->Fptr = Wptr;
		batch->Bptr = Wptr;
	} else {
		batch->Fptr = Wptr;
	}

	batch->size = batch->size + 1;
}
/*}}}*/
/*{{{  static HOT word *dequeue_from_batch (batch_t *batch)*/
static HOT word *dequeue_from_batch (batch_t *batch)
{
	word *Wptr = batch->Fptr;
	word size = batch->size;

	batch->Fptr = (word *) Wptr[Link];
	batch->size = ((size - 2) & BATCH_EMPTIED) | (size - 1);
	/* The previous line is "clever":
	 *
	 *   If size is 1, i.e. the last process in the batch,
	 *   then (size - 2) will be -1 which is 0xffffffff.
	 *
	 *   Complementing this with the flag we want to set will
	 *   give us either 0 or BATCH_EMPTIED, which we then OR
	 *   with the real new size.
	 */
	ASSERT ( batch->Fptr != NotProcess_p || batch->Bptr == Wptr );
	SAFETY { Wptr[Link] = ~NotProcess_p; };

	return Wptr;
}	
/*}}}*/
/*{{{  static WARM void atomic_enqueue_to_runqueue (runqueue_t *rq, bool workspace, void *ptr)*/
static WARM void atomic_enqueue_to_runqueue (runqueue_t *rq, bool workspace, void *ptr)
{
	void *back;
	
	if (workspace) {
		atw_safe_set (&(((word *) ptr)[Link]), (word) NULL);
	} else {
		atw_safe_set (&(((batch_t *) ptr)->next), (word) NULL);
	}

	weak_write_barrier ();
	
	back = (void *) atw_safe_swap ((word *) &(rq->Bptr), (word) ptr);
	
	if (back == NULL) {
		atw_safe_set ((word *) &(rq->Fptr), (word) ptr);
	} else if (workspace) {
		atw_safe_set (&(((word *) back)[Link]), (word) ptr);
	} else {
		atw_safe_set (&(((batch_t *) back)->next), (word) ptr);
	}
}
/*}}}*/
/*{{{  static WARM void *atomic_dequeue_from_runqueue (runqueue_t *rq, bool workspace)*/
static WARM void *atomic_dequeue_from_runqueue (runqueue_t *rq, bool workspace)
{
	void *ptr = (void *) atw_safe_val ((word *) &(rq->Fptr));

	if (ptr != NULL) {
		void *next;

		if (ptr == ((void *) atw_safe_val ((word *) &(rq->Bptr)))) {
			if (atw_safe_cas ((word *) &(rq->Bptr), (word) ptr, (word) NULL)) {
				atw_safe_cas ((word *) &(rq->Fptr), (word) ptr, (word) NULL);
				SAFETY {
					if (workspace) {
						atw_safe_set (&(((word *) ptr)[Link]), ~NotProcess_p);
					} else {
						atw_safe_set (&(((batch_t *) ptr)->next), (word) (-1));
					}
				}
				return ptr;
			}
			weak_read_barrier ();
		}
		
		if (workspace) {
			next = (void *) atw_safe_val (&(((word *) ptr)[Link]));
		} else {
			next = (void *) atw_safe_val (&(((batch_t *) ptr)->next));
		}
		
		if (next != NULL) {
			atw_safe_set ((word *) &(rq->Fptr), (word) next);
			weak_write_barrier ();
			SAFETY {
				if (workspace) {
					atw_safe_set (&(((word *) ptr)[Link]), ~NotProcess_p);
				} else {
					atw_safe_set (&(((batch_t *) ptr)->next), (word) (-1));
				}
			}
			return ptr;
		}
	}

	return NULL;
}
/*}}}*/
/*{{{  static HOT void add_to_local_runqueue (runqueue_t *rq, batch_t *batch)*/
static HOT void add_to_local_runqueue (runqueue_t *rq, batch_t *batch)
{
	batch->next = NULL;

	if (rq->Fptr == NULL) {
		rq->Fptr = rq->Bptr = batch;
	} else {
		rq->Bptr->next = batch;
		rq->Bptr = batch;
	}
}
/*}}}*/
/*{{{  static TRIVIAL unsigned int increment_mwindow_head (unsigned int head)*/
static TRIVIAL unsigned int increment_mwindow_head (unsigned int head)
{
	head = head + 1;
	return (head | (head >> MWINDOW_HEAD_WRAP_BIT)) & MWINDOW_SIZE;
}
/*}}}*/
/*{{{  static HOT void add_to_visible_runqueue (runqueue_t *rq, mwindow_t *mw, batch_t *batch)*/
static HOT void add_to_visible_runqueue (runqueue_t *rq, mwindow_t *mw, batch_t *batch)
{
	unsigned int state = att_val (&(mw->data[MWINDOW_STATE]));
	unsigned int w = increment_mwindow_head (MWINDOW_HEAD (state));

	set_batch_window (batch, w);
	weak_write_barrier ();

	if (att_val (&(mw->data[w])) != ((unsigned int) NULL)) {
		batch_t *old = (batch_t *) att_swap (&(mw->data[w]), (unsigned int) batch);
		if (old != NULL) {
			set_batch_clean (old);
		}
	} else {
		att_set (&(mw->data[w]), (unsigned int) batch);
		weak_write_barrier ();
	}
	att_set (&(mw->data[MWINDOW_STATE]), MWINDOW_NEW_STATE (state, w));

	add_to_local_runqueue (rq, batch);
}
/*}}}*/
/*{{{  static TRIVIAL void add_affine_batch_to_runqueue (runqueue_t *rq, batch_t *batch)*/
static TRIVIAL void add_affine_batch_to_runqueue (runqueue_t *rq, batch_t *batch)
{
	set_batch_clean (batch); /* make sure batch doesn't have a window */
	add_to_local_runqueue (rq, batch);
}
/*}}}*/
/*{{{  static HOT void add_to_runqueue (sched_t *sched, word priofinity, unsigned int rq_n, batch_t *batch)*/
static HOT void add_to_runqueue (sched_t *sched, word priofinity, unsigned int rq_n, batch_t *batch)
{
	SAFETY { verify_batch_integrity (batch); }
	if (PHasAffinity (priofinity)) {
		add_affine_batch_to_runqueue (&(sched->rq[rq_n]), batch);
	} else {
		add_to_visible_runqueue (&(sched->rq[rq_n]), &(sched->mw[rq_n]), batch);
		att_unsafe_set_bit (&(sched->mwstate), rq_n);
	}
}
/*}}}*/
/*{{{  static HOT batch_t *try_pull_from_runqueue (sched_t *sched, unsigned int rq_n)*/
static HOT batch_t *try_pull_from_runqueue (sched_t *sched, unsigned int rq_n)
{
	runqueue_t *rq = &(sched->rq[rq_n]);
	batch_t *batch = rq->Fptr;

	if (batch != NULL) {
		unsigned int window;

		rq->Fptr = batch->next;

		if ((window = batch_window (batch))) {
			mwindow_t *mw = &(sched->mw[rq_n]);

			ASSERT ( (window > 0) && (window <= 15) );

			if (att_cas (&(mw->data[window]), (unsigned int) batch, (unsigned int) NULL)) {
				att_unsafe_clear_bit (&(mw->data[MWINDOW_STATE]), window + MWINDOW_BM_OFFSET);
				set_batch_clean (batch);
			} else {
				release_dirty_batch (sched, batch);
				batch = NULL;
			}
		}
	} else if (rq->priofinity) {
		batch = rq->pending;
		rq->priofinity = 0;
		rq->pending = allocate_batch (sched);
	}

	SAFETY {
		if (batch != NULL) {
			ASSERT ( !empty_batch (batch) );
			batch->next = (batch_t *) (-1);
		}
	}

	return batch;
}
/*}}}*/
/*{{{  static TEPID void mail_batch (word affinity, batch_t *batch)*/
static TEPID void mail_batch (word affinity, batch_t *batch)
{
	unsigned int n;
	sched_t *s;
	
	if (!affinity) {
		affinity = -1; /* i.e. 0xffffffff */
	}
	
	n = pick_random_bit (affinity & (att_val (&enabled_threads)));
	s = schedulers[n];
	atomic_enqueue_to_runqueue (&(s->bmail), false, batch);

	att_safe_set_bit (&(s->sync), SYNC_BMAIL_BIT);
#if !defined (RMOX_BUILD)
	strong_read_barrier ();
	if (att_safe_val (&sleeping_threads) & s->id) {
		ccsp_wake_thread (s, SYNC_BMAIL_BIT);
	}
#else
	/* CGR FIXME: do something... (IPI) */
#endif /* !RMOX_BUILD */
}
/*}}}*/
/*{{{  void schedule_batch (batch_t *batch)*/
void schedule_batch (batch_t *batch)
{
	mail_batch (PAffinity (batch->priofinity), batch);
}
/*}}}*/
/*{{{  static TEPID void mail_process (word affinity, word *Wptr)*/
static TEPID void mail_process (word affinity, word *Wptr)
{
	unsigned int n;
	sched_t *s;
	
	if (!affinity) {
		affinity = -1; /* i.e. 0xffffffff */
	}
	
	n = pick_random_bit (affinity & (att_val (&enabled_threads)));
	s = schedulers[n];
	atomic_enqueue_to_runqueue (&(s->pmail), true, Wptr);
	weak_write_barrier ();
	att_safe_set_bit (&(s->sync), SYNC_PMAIL_BIT);
#if !defined (RMOX_BUILD)
	strong_read_barrier ();
	if (att_safe_val (&sleeping_threads) & s->id) {
		ccsp_wake_thread (s, SYNC_PMAIL_BIT);
	}
#else
	/* CGR FIXME: do something... (IPI) */
#endif /* !RMOX_BUILD */
}
/*}}}*/
/*{{{  static TRIVIAL void enqueue_process_nopri (sched_t *sched, word *Wptr)*/
static TRIVIAL void enqueue_process_nopri (sched_t *sched, word *Wptr)
{
	enqueue_to_batch (&(sched->curb), Wptr);
}
/*}}}*/
/*{{{  static TEPID enqueue_far_process (sched_t *sched, word priofinity, word *Wptr)*/
static TEPID void enqueue_far_process (sched_t *sched, word priofinity, word *Wptr)
{
	if (!PHasAffinity (priofinity)) {
		word prio       = PPriority (priofinity);
		runqueue_t *rq 	= &(sched->rq[prio]);

		if (PHasAffinity (rq->priofinity)) {
			add_affine_batch_to_runqueue (rq, rq->pending);
			rq->pending = allocate_batch (sched);
		}

		rq->priofinity = BuildPriofinity (0, 1);
		enqueue_to_batch (rq->pending, Wptr);

		att_unsafe_set_bit (&(sched->rqstate), prio);
		if (prio < PPriority (sched->priofinity))
			sched->dispatches = 0;
	} else if (PAffinity (priofinity) & sched->id) {
		word prio       = PPriority (priofinity);
		runqueue_t *rq 	= &(sched->rq[prio]);
	
		if (rq->priofinity && PAffinity (rq->priofinity) != PAffinity (priofinity)) {
			add_to_runqueue (sched, rq->priofinity, prio, rq->pending);
			rq->pending = allocate_batch (sched);
		}

		rq->priofinity = priofinity;
		enqueue_to_batch (rq->pending, Wptr);

		att_unsafe_set_bit (&(sched->rqstate), prio);
		if (prio < PPriority (sched->priofinity))
			sched->dispatches = 0;
	} else {
		mail_process (PAffinity (priofinity), Wptr);
	}
}
/*}}}*/
/*{{{  static HOT void enqueue_process (sched_t *sched, word *Wptr)*/
static HOT void enqueue_process (sched_t *sched, word *Wptr)
{
	word priofinity = Wptr[Priofinity];

	if (sched->priofinity == priofinity) {
		enqueue_to_batch (&(sched->curb), Wptr);
	} else {
		enqueue_far_process (sched, priofinity, Wptr);
	}
}
/*}}}*/
/*{{{  static HOT int calculate_dispatches (word size)*/
static HOT int calculate_dispatches (word size)
{
	size <<= BATCH_PPD_SHIFT;
	size |= (one_if_z (size, (~BATCH_MD_MASK)) - 1);
	return (int) (size & BATCH_MD_MASK);
}
/*}}}*/
/*{{{  static HOT void load_curb (sched_t *sched, batch_t *batch, bool remote)*/
static HOT void load_curb (sched_t *sched, batch_t *batch, bool remote)
{
	sched->curb.Fptr = batch->Fptr;
	sched->curb.Bptr = batch->Bptr;
	sched->curb.size = batch->size & (~BATCH_EMPTIED);
	
	sched->dispatches = calculate_dispatches (sched->curb.size);
	sched->priofinity = sched->curb.Fptr[Priofinity];

	if (!remote) {
		reinit_batch_t (batch);
		release_clean_batch (sched, batch);
	} else {
		/* batch is remote, mark it clean for owner */
		mark_batch_clean (batch);
	}
}
/*}}}*/
/*{{{  static HOT batch_t *save_curb (sched_t *sched)*/
static HOT batch_t *save_curb (sched_t *sched)
{
	batch_t *batch = allocate_batch (sched);

	batch->Fptr = sched->curb.Fptr;
	batch->Bptr = sched->curb.Bptr;
	batch->size = sched->curb.size & (~BATCH_EMPTIED);

	return batch;
}
/*}}}*/
/*{{{  static TRIVIAL word *dequeue_from_curb (sched_t *sched)*/
static TRIVIAL word *dequeue_from_curb (sched_t *sched)
{
	return dequeue_from_batch (&(sched->curb));
}	
/*}}}*/
/*{{{  static WARM word *schedule_point (sched_t *sched, word *Wptr, word *other)*/
static WARM word *schedule_point (sched_t *sched, word *Wptr, word *other)
{
#if DEFAULT_SCHED_POLICY == SCHED_POLICY_US
	enqueue_process_nopri (sched, other);
#elif DEFAULT_SCHED_POLICY == SCHED_POLICY_OTHER
	save_priofinity (sched, Wptr);
	enqueue_process_nopri (sched, Wptr);
	Wptr = other;
#elif DEFAULT_SCHED_POLICY == SCHED_POLICY_NEITHER_US
	save_priofinity (sched, Wptr);
	enqueue_process_nopri (sched, Wptr);
	enqueue_process_nopri (sched, other);
	Wptr = dequeue_from_curb (sched);
#elif DEFAULT_SCHED_POLICY == SCHED_POLICY_NEITHER_OTHER
	enqueue_process_nopri (sched, other);
	save_priofinity (sched, Wptr);
	enqueue_process_nopri (sched, Wptr);
	Wptr = dequeue_from_curb (sched);
#else
	fprintf(stderr, "BUG! " __FILE__ ":" #__LINE__ "\n");
#endif
	return Wptr;
}
/*}}}*/
/*{{{  static WARM word *reschedule_point (sched_t *sched, word *Wptr, word *other)*/
static WARM word *reschedule_point (sched_t *sched, word *Wptr, word *other)
{
	if (sched->priofinity != other[Priofinity]) {
		if (PPriority (other[Priofinity]) < PPriority (sched->priofinity)) {
			enqueue_process (sched, other);
			save_priofinity (sched, Wptr);
			enqueue_process_nopri (sched, Wptr);
			RESCHEDULE;
		} else {
			enqueue_process (sched, other);
			return Wptr;
		}
	}
	return schedule_point (sched, Wptr, other);
}
/*}}}*/
/*{{{  static TRIVIAL word *get_process_or_reschedule (sched_t *sched)*/
static TRIVIAL word *get_process_or_reschedule (sched_t *sched)
{
	if (end_of_curb (sched))
		RESCHEDULE;
	return dequeue_from_curb (sched);
}

/*}}}*/
/*{{{  static HOT batch_t *pick_batch (sched_t *sched, unsigned int rq_n)*/
static HOT batch_t *pick_batch (sched_t *sched, unsigned int rq_n)
{
	batch_t *batch;
	
	for (;;) {
		if ((batch = try_pull_from_runqueue (sched, rq_n)) != NULL) {
			return batch;
		} else if (sched->rq[rq_n].Fptr == NULL && !sched->rq[rq_n].priofinity) {
			att_unsafe_clear_bit (&(sched->rqstate), rq_n);
			att_unsafe_clear_bit (&(sched->mwstate), rq_n);
			return NULL;
		}
	}
}
/*}}}*/
/*{{{  static HOT void push_batch (sched_t *sched, word priofinity, batch_t *batch)*/
static HOT void push_batch (sched_t *sched, word priofinity, batch_t *batch)
{
	unsigned int rq_n = PPriority (priofinity);
	runqueue_t *rq = &(sched->rq[rq_n]);
	
	ASSERT ( batch->Fptr != NULL );
	ASSERT ( (batch->size & (~BATCH_EMPTIED)) > 0 );
	SAFETY { verify_batch_integrity (batch); }

	if (rq->priofinity) {
		batch_t *p_batch;
		word p_priofinity;

		p_priofinity 	= rq->priofinity;
		p_batch 	= rq->pending;

		rq->priofinity 	= priofinity | BuildPriofinity (0, 1);
		rq->pending 	= batch;

		add_to_runqueue (sched, p_priofinity, rq_n, p_batch);
	} else {
		release_clean_batch (sched, rq->pending);
		rq->priofinity = priofinity | BuildPriofinity (0, 1);
		rq->pending = batch;
		att_unsafe_set_bit (&(sched->rqstate), rq_n);
	}
}
/*}}}*/
/*{{{  static TRIVIAL void new_curb (sched_t *sched)*/
static TRIVIAL void new_curb (sched_t *sched)
{
	sched->dispatches= BATCH_PPD;
	sched->curb.Fptr = NotProcess_p;
	sched->curb.size = BATCH_EMPTIED;
}
/*}}}*/
/*{{{  static HOT void push_curb (sched_t *sched)*/
static HOT void push_curb (sched_t *sched)
{
	if (sched->dispatches <= 0 && (sched->curb.size ^ BATCH_EMPTIED) > (BATCH_EMPTIED + 1)) {
		/* split batch */
		batch_t *batch = allocate_batch (sched);
		enqueue_to_batch_with_hint (batch, dequeue_from_curb (sched), true);
		push_batch (sched, sched->priofinity, batch);
	}
	
	push_batch (sched, sched->priofinity, save_curb (sched));
}
/*}}}*/
/*{{{  static WARM void switch_priofinity (sched_t *sched, word priofinity)*/
static WARM void switch_priofinity (sched_t *sched, word priofinity)
{
	if (sched->priofinity != priofinity) {
		if (!empty_batch (&(sched->curb)))
			push_curb (sched);
		new_curb (sched);
		sched->priofinity = priofinity;
	}
}
/*}}}*/
/*{{{  static TEPID batch_t *try_migrate_from_scheduler (sched_t *sched, unsigned int rq_n)*/
static TEPID batch_t *try_migrate_from_scheduler (sched_t *sched, unsigned int rq_n)
{
	mwindow_t *mw 		= &(sched->mw[rq_n]);
	unsigned int state 	= att_val (&(mw->data[MWINDOW_STATE]));
	unsigned int head, bm;
	batch_t *batch = NULL;

	head 	= MWINDOW_HEAD (state);
	bm 	= state >> MWINDOW_BM_OFFSET;

	while (bm && batch == NULL) {
		unsigned int w;
		
		w = bm & (MWINDOW_MASK << head);
		if (w) {
			w = bsr (w);
		} else {
			w = bsr (bm & (MWINDOW_MASK >> ((MWINDOW_SIZE + 1) - head)));
		}
		
		att_clear_bit (&(mw->data[MWINDOW_STATE]), w + MWINDOW_BM_OFFSET);
		batch = (batch_t *) att_swap (&(mw->data[w]), (unsigned int) NULL);
		bm &= ~(1 << w);
	}

	/* don't worry about race in following line */
	if ((!bm) && head == att_val (&(mw->data[MWINDOW_STATE]))) {
		att_clear_bit (&(sched->mwstate), rq_n);
	}

	return batch;
}
/*}}}*/
/*{{{  static TEPID batch_t *migrate_some_work (sched_t *sched)*/
static TEPID batch_t *migrate_some_work (sched_t *sched)
{
	unsigned int active = att_val (&enabled_threads) & (~att_val (&sleeping_threads));
	unsigned int shift = (sched->index & ~0x3);
	batch_t *batch = NULL;

	while (active && batch == NULL) {
		unsigned int best_n = MAX_RUNTIME_THREADS, best_prio = MAX_PRIORITY_LEVELS;
		unsigned int i;

		for (i = 0; i < MAX_RUNTIME_THREADS; ++i) {
			unsigned int n = (i + shift) & (MAX_RUNTIME_THREADS - 1);
			if (active & (1 << n)) {
				unsigned int work = att_val (&(schedulers[n]->mwstate));

				if (work) {
					unsigned int prio = bsf (work);
					if (prio < best_prio) {
						best_n = n;
						best_prio = prio;
					}
				} else {
					active &= ~(1 << n);
				}
			}
		}

		if (best_n < MAX_RUNTIME_THREADS) {
			batch = try_migrate_from_scheduler (schedulers[best_n], best_prio);
		}
	}

	return batch;
}
/*}}}*/
/*{{{  static WARM tqnode_t *insert_tqnode (sched_t *sched, tqnode_t *node, bool before, word *wptr, word time, bool alt)*/
static WARM tqnode_t *insert_tqnode (sched_t *sched, tqnode_t *node, bool before, word *wptr, word time, bool alt)
{
	tqnode_t *tn;

	if (node->wptr != NotProcess_p || dirty_batch ((batch_t *) node)) {
		/* insert new node */
		tn = init_tqnode (sched, wptr, time, alt);
		if (before) {
			/* before current */
			tn->next = node;
			if ((tn->prev = node->prev) == NULL) {
				sched->tq_fptr = tn;
				Time_SetTimeout (sched, tn->time);
			} else {
				tn->prev->next = tn;
			}
			node->prev = tn;
		} else {
			/* after current */
			if ((tn->next = node->next) == NULL) {
				sched->tq_bptr = tn;
			} else {
				tn->next->prev = tn;
			}
			node->next = tn;
			tn->prev = node;
		}
	} else {
		/* implies: wptr == NULL && node is clean */
		/* hence this is a dead node, so reuse it */
		tn = node;
		setup_tqnode (tn, wptr, time, alt);
		if (tn->prev == NULL) {
			Time_SetTimeout (sched, tn->time);
		}
	}
	
	return tn;
}
/*}}}*/
/*{{{  static WARM void delete_tqnode (sched_t *sched, tqnode_t *node)*/
static WARM void delete_tqnode (sched_t *sched, tqnode_t *node) {
	if (node->prev == NULL) {
		/* front of queue */
		if ((sched->tq_fptr = node->next) == NULL) {
			/* back of queue */
			sched->tq_bptr = NULL;
		} else {
			/* not back of queue */
			sched->tq_fptr->prev = NULL;
			Time_SetTimeout (sched, sched->tq_fptr->time);
		}
	} else {
		/* not front of queue */
		if ((node->prev->next = node->next) == NULL) {
			/* back of queue */
			sched->tq_bptr = node->prev;
		} else {
			/* not back of queue */
			node->next->prev = node->prev;
		}
	}
}
/*}}}*/
/*{{{  static WARM void clean_timer_queue (sched_t *sched)*/
static WARM void clean_timer_queue (sched_t *sched)
{
	tqnode_t *tn = sched->tq_fptr;
	while (tn != NULL) {
		if (tn->wptr == NotProcess_p) {
			tqnode_t *next = tn->next;
			delete_tqnode (sched, tn);
			set_batch_clean ((batch_t *) tn);
			tn = next;
		} else {
			tn = tn->next;
		}
	}
}
/*}}}*/
/*{{{  static HOT void trigger_alt_guard (sched_t *sched, word ptr)*/
static HOT void trigger_alt_guard (sched_t *sched, word ptr)
{
	word *wptr = (word *)(((word) ptr) & (~1));
	word state, nstate;

	do {
		state = atw_val (&(wptr[State]));
		nstate = (state - 1) & (~(ALT_NOT_READY | ALT_WAITING));
	} while (!atw_cas (&(wptr[State]), state, nstate));

	if ((state & ALT_WAITING) || (nstate == 0)) {
		enqueue_process (sched, wptr);
	}
}
/*}}}*/
/*{{{  static WARM void walk_timer_queue (sched_t *sched)*/
static WARM void walk_timer_queue (sched_t *sched)
{
	tqnode_t *tn 	= sched->tq_fptr;
	Time now 	= Time_GetTime (sched);
	do {
		if (tn->wptr == NotProcess_p || !Time_AFTER (tn->time, now)) {
			/* expired node */
			word ptr = atw_val ((word *)&(tn->wptr));
			tqnode_t *next = tn->next;
			if ((ptr != NotProcess_p) && !(ptr & 1)) {
				/* not an ALT, simply requeue */
				SetTimeField (tn->wptr, now);
				enqueue_process (sched, tn->wptr);
				release_tqnode (sched, tn);
			} else {
				if (ptr != NotProcess_p) {
					/* challenge ALT */
					tn->time = now;
					weak_write_barrier ();
					ptr = atw_swap ((word *)&(tn->wptr), NotProcess_p);
					if (ptr != NotProcess_p) {
						trigger_alt_guard (sched, ptr);
					}
					compiler_barrier ();
				}
				set_batch_clean ((batch_t *) tn);
			}
			tn = next;
		} else {
			/* valid node, becomes new head */
			tn->prev 	= NULL;
			sched->tq_fptr  = tn;
			Time_SetTimeoutN (sched, now, tn->time);
			return;
		}
	} while (tn != NULL);

	/* if we reached here the queue is empty */
	sched->tq_fptr = NULL;
	sched->tq_bptr = NULL;
}
/*}}}*/
/*{{{  static HOT void check_timer_queue (sched_t *sched)*/
static HOT void check_timer_queue (sched_t *sched)
{
	if (sched->tq_fptr != NULL) {
		#ifdef ENABLE_CPU_TIMERS
		if (Time_PastTimeout (sched)) {
			walk_timer_queue (sched);
		}
		#else
		walk_timer_queue (sched);
		#endif
	}
}
/*}}}*/
/*{{{  static WARM tqnode_t *add_to_timer_queue (sched_t *sched, word *wptr, word time, word alt)*/
static WARM tqnode_t *add_to_timer_queue (sched_t *sched, word *wptr, word time, word alt) {
	tqnode_t *tn;

	if (sched->tq_fptr == NULL) {
		tn	 	= init_tqnode (sched, wptr, time, alt);
		tn->next 	= NULL;
		tn->prev 	= NULL;
		sched->tq_fptr	= tn;
		sched->tq_bptr	= tn;
		Time_SetTimeout (sched, tn->time);
	} else {
		tqnode_t *fptr = sched->tq_fptr;
		tqnode_t *bptr = sched->tq_bptr;

		tn = NULL;
		do {
			if (Time_AFTER (fptr->time, time)) {
				tn = insert_tqnode (sched, fptr, true, wptr, time, alt);
			} else if (!Time_AFTER (bptr->time, time)) {
				tn = insert_tqnode (sched, bptr, false, wptr, time, alt);
			} else {
				fptr = fptr->next;
				bptr = bptr->prev;
			}
		} while (tn == NULL);
	}

	return tn;
}
/*}}}*/
/*{{{  static WARM bool remove_from_timer_queue (sched_t *sched, tqnode_t *tn, word *wptr)*/
static WARM bool remove_from_timer_queue (sched_t *sched, tqnode_t *tn, word *wptr) {
	bool fired = true;

	if (tn->scheduler == sched) {
		/* local timer queue - can unlink node */
		if (tn->wptr != NotProcess_p) {
			delete_tqnode (sched, tn);
			set_batch_clean ((batch_t *) tn);
			fired = false;
		}
	} else {
		/* remote timer queue */
		if (atw_val ((word *)&(tn->wptr)) != NotProcess_p) {
			word wptr = atw_swap ((word *)&(tn->wptr), NotProcess_p);
			if (wptr != NotProcess_p) {
				fired = false;
				att_set_bit (&(tn->scheduler->sync), SYNC_TQ_BIT);
			}
		}
	}
	
	return fired;
}
/*}}}*/
/*{{{  static void setup_spin (sched_t *sched)*/
#if !defined(RMOX_BUILD)
static void setup_spin (sched_t *sched)
{
	Time start, end;
	word i, ns;

	i 	= 10000;
	start 	= Time_GetTime (sched);
	while (i--) {
		idle_cpu ();
	}
	end 	= Time_GetTime (sched);
	ns 	= Time_MINUS (end, start) / 10U;

	sched->spin = (1000 * ccsp_spin_us ()) / (ns ? ns : 1);
}
#endif /* !defined(RMOX_BUILD) */
/*}}}*/
/*}}}*/
/*{{{  error handling support functions */
/*{{{  void print_tq (void)*/
/* print out timer queue for debugging*/
void print_tq (sched_t *sched)
{
	tqnode_t *tn = sched->tq_fptr;

	MESSAGE ("Timer Queue dump.\n");
	while (tn != NULL) {
		MESSAGE ("(Wptr = 0x%x, Wptr[Time_f] = %d, Wptr[State] = 0x%x)\n", (unsigned int)tn->wptr, tn->time, tn->wptr != NULL ? (unsigned int)tn->wptr[State] : 0);
		tn = tn->next;
	}
	MESSAGE ("--- end Timer queue dump.\n");
}
/*}}}*/
/*{{{  void kernel_panic(void) */
/* kernel panic function*/
void kernel_panic (void)
{
	BMESSAGE ("panic: called kernel function not implemented.\n");
	ccsp_show_last_debug_insert ();
	ccsp_kernel_exit (1, 0);
}
/*}}}*/
/*{{{  void zerodiv_happened (unsigned int zerodiv_info, unsigned int zerodiv_info2, unsigned int filename_addr, unsigned int procedure_addr)*/
/*
 *	divide-by-zero handling function
 */
void zerodiv_happened (unsigned int zerodiv_info, unsigned int zerodiv_info2, unsigned int filename_addr, unsigned int procedure_addr)
{
	int line_num, file_num, proc_num;
	char *div_filename, *div_procname;
	int *div_fc, *div_off;
	int *pvr_fc, *pvr_off;

	div_filename = div_procname = NULL;
	line_num = zerodiv_info & 0xffff;
	file_num = (zerodiv_info2 >> 16) & 0xffff;
	proc_num = zerodiv_info2 & 0xffff;
	div_fc = (int *)filename_addr;
	if (file_num > *div_fc) {
		BMESSAGE ("filename table inconsistent!\n");
		file_num = -1;
	} else {
		div_off = div_fc + (file_num + 1);
		div_filename = (char *)filename_addr + *div_off;
	}
	pvr_fc = (int *)procedure_addr;
	if (proc_num > *pvr_fc) {
		BMESSAGE ("procedure table inconsistent!\n");
		proc_num = -1;
	} else {
		pvr_off = pvr_fc + (proc_num + 1);
		div_procname = (char *)procedure_addr + *pvr_off;
	}
	BMESSAGE ("divide-by-zero in PROC \"%s\" in file \"%s\" line %d\n", (proc_num >= 0) ? div_procname : "<unknown>",
		(file_num >= 0) ? div_filename : "<unknown>", line_num);
	return;
}
/*}}}*/
/*{{{  void overflow_happened (unsigned int overflow_info, unsigned int overflow_info2, unsigned int filename_addr, unsigned int procedure_addr)*/
/*
 *	overflow handling function
 */
void overflow_happened (unsigned int overflow_info, unsigned int overflow_info2, unsigned int filename_addr, unsigned int procedure_addr)
{
	static char *overflow_ops[] = { "INVALID", "add", "subtract", "multiply", "divide", "modulo", "long-add", "long-subtract", "add-constant",
					"STOP", "long-divide", "fractional-multiply", "fp-check-int32", "fp-check-int64" };
	static int num_overflow_ops = 14;
	int ovr_idx, line_num, file_num, proc_num;
	char *ovr_filename, *ovr_procname;
	int *ovr_fc, *ovr_off;
	int *pvr_fc, *pvr_off;

	ovr_filename = ovr_procname = NULL;
	ovr_idx = (overflow_info >> 24) & 0xff;
	line_num = overflow_info & 0xffff;
	file_num = (overflow_info2 >> 16) & 0xffff;
	proc_num = overflow_info2 & 0xffff;
	if ((ovr_idx >= num_overflow_ops) || (ovr_idx < 0)) {
		ovr_idx = 0;
	}
	ovr_fc = (int *)filename_addr;
	if (file_num > *ovr_fc) {
		BMESSAGE ("filename table inconsistent!\n");
		file_num = -1;
	} else {
		ovr_off = ovr_fc + (file_num + 1);
		ovr_filename = (char *)filename_addr + *ovr_off;
	}
	pvr_fc = (int *)procedure_addr;
	if (proc_num > *pvr_fc) {
		BMESSAGE ("procedure table inconsistent!\n");
		proc_num = -1;
	} else {
		pvr_off = pvr_fc + (proc_num + 1);
		ovr_procname = (char *)procedure_addr + *pvr_off;
	}
	BMESSAGE ("overflow on %s operation in PROC \"%s\" in file \"%s\" line %d\n",
		overflow_ops[ovr_idx], (proc_num >= 0) ? ovr_procname : "<unknown>", (file_num >= 0) ? ovr_filename : "<unknown>", line_num);
	return;
}
/*}}}*/
/*{{{  void floaterr_happened (unsigned int floaterr_info, unsigned int floaterr_info2, unsigned int floaterr_fpustatus, unsigned int filename_addr, unsigned int procedure_addr)*/
/*
 *	floating-point error handling
 **/
void floaterr_happened (unsigned int floaterr_info, unsigned int floaterr_info2, unsigned int floaterr_fpustatus, unsigned int filename_addr, unsigned int procedure_addr)
{
	int line_num, file_num, proc_num;
	char *flt_filename, *flt_procname;
	int *flt_fc, *flt_fo;
	int *flt_pc, *flt_po;
	static char *fpuerrs[] = {"invalid-op", "denormalised-operand", "divide-by-zero", "overflow", "underflow", "inexact-result"};
	int num_fpuerrs = 6;
	int i;

	flt_filename = flt_procname = NULL;
	line_num = floaterr_info & 0xfff;
	file_num = (floaterr_info2 >> 16) & 0xffff;
	proc_num = floaterr_info2 & 0xffff;
	flt_fc = (int *)filename_addr;
	if (file_num > *flt_fc) {
		BMESSAGE ("filename table inconsistent!\n");
		file_num = -1;
	} else {
		flt_fo = flt_fc + (file_num + 1);
		flt_filename = (char *)filename_addr + *flt_fo;
	}
	flt_pc = (int *)procedure_addr;
	if (proc_num > *flt_pc) {
		BMESSAGE ("procedure table inconsistent!\n");
		proc_num = -1;
	} else {
		flt_po = flt_pc + (proc_num + 1);
		flt_procname = (char *)procedure_addr + *flt_po;
	}
	BMESSAGE ("floating-point error (");
	floaterr_fpustatus &= 0x3f;
	for (i=0; i<num_fpuerrs; i++) {
		if (floaterr_fpustatus & (1 << i)) {
			MESSAGE ("%s", fpuerrs[i]);
			floaterr_fpustatus &= ~(1 << i);
			if (floaterr_fpustatus) {
				MESSAGE (",");
			}
		}
	}
	MESSAGE (") in PROC \"%s\" in file \"%s\" line %d\n", 
		(proc_num >= 0) ? flt_procname : "<unknown>", (file_num >= 0) ? flt_filename : "<unknown>", line_num);
	return;
}
/*}}}*/
/*{{{  void handle_range_error (unsigned int range_info1, unsigned int range_info2, unsigned int filename_addr, unsigned int procedure_addr)*/
/* range error handling */
void handle_range_error (unsigned int range_info1, unsigned int range_info2, unsigned int filename_addr, unsigned int procedure_addr)
{
	static char *rangerr_ops[] = { "INVALID", "shift", "CSNGL", "CSUB0", "CCNT1", "CWORD" };
	static int num_rangerr_ops = 6;
	int range_op, line_num, file_num, proc_num;
	char *range_file, *range_proc;
	int rt_bits;

	range_op = (range_info1 >> 24) & 0xff;
	rt_bits = (range_info1 >> 16) & 0xff;
	line_num = range_info1 & 0xffff;
	if (rt_bits != 0xff) {
		BMESSAGE ("Range error (debug data incorrect - rt_bits=%x)\n", rt_bits);
		return;
	}
	if ((range_op >= num_rangerr_ops) || (range_op < 0)) {
		BMESSAGE ("Range error (debug data incorrect - range_op=%d)\n", range_op);
		return;
	}
	file_num = (range_info2 >> 16) & 0xffff;
	proc_num = range_info2 & 0xffff;
	range_file = (char *)filename_addr;
	range_proc = (char *)procedure_addr;
	if ((file_num >= *(int *)(range_file)) || (file_num < 0)) {
		BMESSAGE ("Range error (debug data incorrect - file_num=%d)\n", file_num);
		return;
	}
	if ((proc_num >= *(int *)(range_proc)) || (proc_num < 0)) {
		BMESSAGE ("Range error (debug data incorrect - proc_num=%d)\n", proc_num);
		return;
	}
	range_file += *(int *)(range_file + (4 * (file_num+1)));
	range_proc += *(int *)(range_proc + (4 * (proc_num+1)));
	BMESSAGE ("range error on %s operation in PROC \"%s\" in file \"%s\" line %d\n",
		rangerr_ops[range_op], range_proc, range_file, line_num);
	return;
}
/*}}}*/
/*{{{  void handle_seterr (unsigned int seterr_info1, unsigned int seterr_info2, unsigned int filename_addr, unsigned int procedure_addr)*/
/* seterr error handling */
void handle_seterr (unsigned int seterr_info1, unsigned int seterr_info2, unsigned int filename_addr, unsigned int procedure_addr)
{
	bool occam_mode = false;
	int line_num, rt_bits;

	rt_bits = (seterr_info1 >> 16) & 0xffff;
	if (rt_bits == 0xfb00) {
		line_num = seterr_info1 & 0xffff;
		occam_mode = true;
	} else if ((rt_bits & 0xff00) == 0xfe00) {
		line_num = seterr_info1 & 0xffffff;
	} else {
		BMESSAGE ("SetErr: application level error (debug data incorrect - rt_bits=%4.4x)\n", rt_bits);
		return;
	}
	
	if (occam_mode) {
		char *seterr_file, *seterr_proc;
		int file_num = (seterr_info2 >> 16) & 0xffff;
		int proc_num = seterr_info2 & 0xffff;
		seterr_file = (char *)filename_addr;
		seterr_proc = (char *)procedure_addr;
		if ((file_num >= *(int *)(seterr_file)) || (file_num < 0)) {
			BMESSAGE ("SetErr: application level error (debug data incorrect - file_num=%d)\n", file_num);
			return;
		}
		if ((proc_num >= *(int *)(seterr_proc)) || (proc_num < 0)) {
			BMESSAGE ("SetErr: application level error (debug data incorrect - proc_num=%d)\n", proc_num);
			return;
		}
		seterr_file += *(int *)(seterr_file + (4 * (file_num+1)));
		seterr_proc += *(int *)(seterr_proc + (4 * (proc_num+1)));
		BMESSAGE ("error in PROC \"%s\", in file \"%s\" line %d.\n",
			seterr_proc, seterr_file, line_num);
	} else {
		BMESSAGE ("error in file \"%s\" at line %d.\n",
			(char *)filename_addr, line_num);
	}
}
/*}}}*/
/*{{{  void ccsp_show_last_debug_insert (void)*/
/* ccsp_show_last_debug_insert*/
void ccsp_show_last_debug_insert (void)
{
	sched_t *sched = _local_scheduler;
	void (*setup_fcn)(void);
	int file_num, line_num;
	char *insert_file;

	if ((sched->mdparam[0] != 0xffffffff) && (sched->mdparam[1] != 0xffffffff) && sched->mdparam[0] && sched->mdparam[1]) {
		file_num = (sched->mdparam[0] >> 16) & 0xffff;
		line_num = sched->mdparam[0] & 0xffff;
		/* mdparam[1] holds address of setup code */
		setup_fcn = (void (*)(void))sched->mdparam[1];
		setup_fcn ();
		insert_file = (char *)sched->mdparam[0];
		if ((file_num >= *(int *)(insert_file)) || (file_num < 0)) {
			BMESSAGE ("debug insert: invalid file number (file_num=%d)\n", file_num);
			return;
		}
		insert_file += *(int *)(insert_file + (4 * (file_num+1)));
		BMESSAGE ("last debug position was in file \"%s\" near line %d\n", insert_file, line_num);
	}
	if ((sched->mdparam[2] != 0xffffffff) && (sched->mdparam[3] != 0xffffffff) && sched->mdparam[2] && sched->mdparam[3]) {
		file_num = (sched->mdparam[2] >> 16) & 0xffff;
		line_num = sched->mdparam[2] & 0xffff;
		/* mdparam[3] holds address of setup code */
		setup_fcn = (void (*)(void))sched->mdparam[3];
		setup_fcn ();
		insert_file = (char *)sched->mdparam[0];
		if ((file_num >= *(int *)(insert_file)) || (file_num < 0)) {
			BMESSAGE ("debug insert: invalid file number (file_num=%d)\n", file_num);
			return;
		}
		insert_file += *(int *)(insert_file + (4 * (file_num+1)));
		BMESSAGE ("last position before CALL was in file \"%s\" near line %d\n", insert_file, line_num);
	}
	return;
}
/*}}}*/
/*{{{  void err_no_bsyscalls (word *wptr, int ra)*/
void err_no_bsyscalls (word *wptr, int ra)
{
	BMESSAGE ("blocking system calls not enabled in this kernel. (Wptr=0x%x)\n", (unsigned int)wptr);
	ccsp_kernel_exit (1, ra);
}
/*}}}*/
/*}}}*/
/*{{{  dyanmic/mobile-process support functions */
/*{{{  static bool find_remove_from_batch (batch_t *batch, bool remove, word ws_base, word ws_limit)*/
static bool find_remove_from_batch (batch_t *batch, bool remove, word ws_base, word ws_limit) {
	word *prev = NotProcess_p;
	word *wptr = batch->Fptr;

	while (wptr != NotProcess_p) {
		word ptr = (word) wptr;
		if (ptr >= ws_base && ptr < ws_limit) {
			if (remove) {
				if (prev == NotProcess_p) {
					batch->Fptr = (word *) wptr[Link];
				} else {
					prev[Link] = wptr[Link];
				}
			}
			return true;
		}
		prev = wptr;
		wptr = (word *) wptr[Link];
	}

	return false;
}
/*}}}*/
/*{{{  static bool find_remove_from_runqueue (sched_t *sched, word rq_n, bool remove, word ws_base, word ws_limit)*/
static bool find_remove_from_runqueue (sched_t *sched, word rq_n, bool remove, word ws_base, word ws_limit) {
	runqueue_t *rq = &(sched->rq[rq_n]);
	batch_t *p = NULL;
	batch_t *b = rq->Fptr;

	while (b != NULL) {
		unsigned int window = batch_window (b);

		if (window && (att_val (&(enabled_threads)) & (~sched->id))) {
			BMESSAGE ("attempted to perform unsafe find_remove_from_runqueue (multiple runtime threads unsupported)\n");
			ccsp_kernel_exit (1, ws_base);
		}

		if (find_remove_from_batch (b, remove, ws_base, ws_limit)) {
			if (remove && (b->size <= 0)) {
				if (window) {
					mwindow_t *mw = &(sched->mw[rq_n]);
					if (att_cas (&(mw->data[window]), (unsigned int) b, (unsigned int) NULL)) {
						att_unsafe_clear_bit (&(mw->data[MWINDOW_STATE]), window + MWINDOW_BM_OFFSET);
						set_batch_clean (b);
					} else {
						BMESSAGE ("reached undefined state during find_remove_from_runqueue (multiple runtime threads?)\n");
						ccsp_kernel_exit (1, ws_base);
					}
				}
				if (p == NULL) {
					rq->Fptr = b->next;
				} else {
					p->next = b->next;
				}
				release_batch (sched, b);
			}
			return true;
		}
		p = b;
		b = b->next;
	}

	if (rq->priofinity) {
		if (find_remove_from_batch (rq->pending, remove, ws_base, ws_limit)) {
			if (remove && (rq->pending->size <= 0)) {
				rq->priofinity = 0;
			}
			return true;
		}
	}

	return false;
}
/*}}}*/
/*{{{  static bool find_remove_from_timerq (sched_t *sched, word ws_base, word ws_limit)*/
static bool find_remove_from_timerq (sched_t *sched, bool remove, word ws_base, word ws_limit) {
	tqnode_t *p = NULL;
	tqnode_t *n = sched->tq_fptr;

	while (n != NULL) {
		word ptr = (word) n->wptr;
		if (ptr >= ws_base && ptr < ws_limit) {
			if (remove) {
				if (ptr & 1) {
					ptr = atw_swap (&(n->wptr), (word) NotProcess_p);
					if (ptr == NotProcess_p) {
						BMESSAGE ("reached possibly undefined state during find_remove_from_timerq (multiple runtime threads?)\n");
						return false;
					} else {
						set_batch_clean ((batch_t *) n);
						((word *)(ptr & (~1)))[TLink] = TimeNotSet_p;
					}
				}
				delete_tqnode (sched, n);
				release_tqnode (sched, n);
			}
			return true;
		}
		p = n;
		n = n->next;
	}

	return false;
}
/*}}}*/
/*{{{  int not_on_any_queue (unsigned int ws_base, unsigned int ws_limit)*/
int not_on_any_queue (unsigned int ws_base, unsigned int ws_limit)
{
	sched_t *sched 		= _local_scheduler;
	unsigned int rqstate 	= att_val (&(sched->rqstate));
	int i;

	if (find_remove_from_batch (&(sched->curb), false, ws_base, ws_limit)) {
		return false;
	}

	for (i = 0; i < MAX_PRIORITY_LEVELS; ++i) {
		if (rqstate & (1 << i)) {
			if (find_remove_from_runqueue (sched, i, false, ws_base, ws_limit)) {
				return false;
			}
		}
	}

	return find_remove_from_timerq (sched, false, ws_base, ws_limit) ? false : true;
}
/*}}}*/
/*{{{  int remove_from_any_queue (unsigned int ws_base, unsigned int ws_limit)*/
int remove_from_any_queue (unsigned int ws_base, unsigned int ws_limit)
{
	sched_t *sched 		= _local_scheduler;
	unsigned int rqstate 	= att_val (&(sched->rqstate));
	int i;

	if (find_remove_from_batch (&(sched->curb), true, ws_base, ws_limit)) {
		return false;
	}

	for (i = 0; i < MAX_PRIORITY_LEVELS; ++i) {
		if (rqstate & (1 << i)) {
			if (find_remove_from_runqueue (sched, i, true, ws_base, ws_limit)) {
				return false;
			}
		}
	}

	return find_remove_from_timerq (sched, true, ws_base, ws_limit) ? false : true;
}
/*}}}*/
/*{{{  void do_queue_process (word *ws)*/
/* do_queue_process*/
void do_queue_process (word *ws)
{
	sched_t *sched = _local_scheduler;
	enqueue_process (sched, ws);
}
/*}}}*/
/*}}}*/

/*{{{  void ccsp_kernel_init (void)*/
void ccsp_kernel_init (void)
{
	#if 0
	/* initialise global multiway-sync data */
	mwaltlock.value = 1;
	mwaltlock.qfptr = NULL;
	mwaltlock.qbptr = NULL;
	mwaltlock.count = 0;
	#endif

	/* initialise run-time */
	init_ccsp_global_t (&_ccsp);
	init_local_schedulers ();
}
/*}}}*/

/*{{{  error entry-points */
/*{{{  static void kernel_X_common_error (...) */
static void kernel_X_common_error (word *Wptr, sched_t *sched, unsigned int return_address, char *name)
{
#if defined(DYNAMIC_PROCS) && !defined(RMOX_BUILD)
	d_process *kr_dptr;
#endif
#if defined(DYNAMIC_PROCS) && !defined(RMOX_BUILD)
	/* check for faulting dynamic proc */
	if (faulting_dynproc ((word **)&(Wptr), &return_address, name, &kr_dptr)) {
		BMESSAGE ("dynamic process generated a fault, killed it.\n");
		switch_priofinity (sched, kr_dptr->holding_priofinity);
		K_ZERO_OUT ();
	} else
#endif
	{
		if (ccsp_ignore_errors) {
			RESCHEDULE;
		} else {
			BMESSAGE ("application error, stopped.\n");
			ccsp_kernel_exit (1, return_address);
		}
	}
}
/*}}}*/
/*{{{  void kernel_X_zero_div (void)*/
/*
 *	entry point for integer division-by-zero
 *
 *	@SYMBOL:	X_zero_div
 *	@TYPE:		LCR
 *	@INPUT:		STACK(4)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_ZERODIV
 *	@PRIO:		0
 */
void kernel_X_zero_div (void)
{
	unsigned int filename_addr, procedure_addr, zerodiv_info, zerodiv_info2;
	
	K_SETGLABEL_STKFOUR_IN_LCR (X_zero_div, zerodiv_info2, zerodiv_info, procedure_addr, filename_addr);
	ENTRY_TRACE (X_zero_div, "%x, %x, %p, %p", zerodiv_info2, zerodiv_info, (void *)procedure_addr, (void *)filename_addr);

	zerodiv_happened (zerodiv_info, zerodiv_info2, filename_addr, procedure_addr);
	kernel_X_common_error (Wptr, sched, return_address, "zero_div");
}
/*}}}*/
/*{{{  void kernel_X_overflow (void)*/
/*
 *	arithmetic overflow entry point
 *
 *	@SYMBOL:	X_overflow
 *	@TYPE:		LCR
 *	@INPUT:		STACK(4)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_OVERFLOW
 *	@PRIO:		0
 */
void kernel_X_overflow (void)
{
	unsigned int filename_addr, overflow_info, overflow_info2, procedure_addr;
	
	K_SETGLABEL_STKFOUR_IN_LCR (X_overflow, overflow_info2, overflow_info, procedure_addr, filename_addr);
	ENTRY_TRACE (X_overflow, "%x, %x, %p, %p", overflow_info2, overflow_info, (void *)procedure_addr, (void *)filename_addr);

	overflow_happened (overflow_info, overflow_info2, filename_addr, procedure_addr);		/* Parameters dealt with (they're visible throughout) */
	kernel_X_common_error (Wptr, sched, return_address, "overflow");
}
/*}}}*/
/*{{{  void kernel_X_floaterr (void)*/
/*
 *	floating-point error entry point
 *
 *	@SYMBOL:	X_floaterr
 *	@TYPE:		LCR
 *	@INPUT:		STACK(5)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_FLOATERR
 *	@PRIO:		0
 */
void kernel_X_floaterr (void)
{
	unsigned int filename_addr, floaterr_fpustatus, floaterr_info, floaterr_info2, procedure_addr;
	
	K_SETGLABEL_STKFIVE_IN_LCR (X_floaterr, floaterr_info2, floaterr_info, procedure_addr, filename_addr, floaterr_fpustatus);
	ENTRY_TRACE (X_floaterr, "%x, %x, %p, %p, %x", floaterr_info2, floaterr_info, (void *)procedure_addr, (void *)filename_addr, floaterr_fpustatus);

	floaterr_happened (floaterr_info, floaterr_info2, floaterr_fpustatus, filename_addr, procedure_addr);
	kernel_X_common_error (Wptr, sched, return_address, "floaterr");
}
/*}}}*/
/*{{{  void kernel_X_Seterr (void)*/
/*
 *	SETERR entry-point
 *
 *	@SYMBOL:	X_Seterr
 *	@TYPE:		LCR
 *	@INPUT:		STACK(4)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_SETERR
 *	@PRIO:		0
 */
void kernel_X_Seterr (void)
{
	unsigned int filename_addr, procedure_addr, seterr_info1, seterr_info2;
	
	K_SETGLABEL_STKFOUR_IN_LCR (X_Seterr, procedure_addr, filename_addr, seterr_info2, seterr_info1);
	ENTRY_TRACE (X_Seterr, "%p, %p, %x, %x", (void *)procedure_addr, (void *)filename_addr, seterr_info2, seterr_info1);
	
	handle_seterr (seterr_info1, seterr_info2, filename_addr, procedure_addr);
	kernel_X_common_error (Wptr, sched, return_address, "Seterr");
}
/*}}}*/
/*{{{  void kernel_X_BSeterr (void)*/
/*
 *	basic SETERR entry-point
 *
 *	@SYMBOL:	X_BSeterr
 *	@TYPE:		LCR
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_BSETERR
 *	@PRIO:		0
 */
void kernel_X_BSeterr (void)
{
	K_SETGLABEL_ZERO_IN_LCR (X_BSeterr);

	ENTRY_TRACE0 (X_BSeterr);
	
	kernel_X_common_error (Wptr, sched, return_address, "BSeterr");
}
/*}}}*/
/*{{{  void kernel_X_BNSeterr (void)*/
/*
 *	basic SETERR entry-point, but no return address expected
 *
 *	@SYMBOL:	X_BNSeterr
 *	@TYPE:		JUMP
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_BNSETERR
 *	@PRIO:		0
 */
void kernel_X_BNSeterr (void)
{
	K_SETGLABEL_ZERO_IN (X_BNSeterr);
	ENTRY_TRACE0 (X_BNSeterr);

	/* return_address = 0 */
	kernel_X_common_error (Wptr, sched, 0, "BNSeterr");
}
/*}}}*/
/*{{{  void kernel_X_RangeCheckError (void)*/
/*
 *	Range error entry-point
 *
 *	@SYMBOL:	X_RangeCheckError
 *	@TYPE:		LCR
 *	@INPUT:		STACK(4)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_RANGERR
 *	@PRIO:		0
 */
void kernel_X_RangeCheckError (void)
{
	unsigned int filename_addr, procedure_addr, range_info1, range_info2;
	
	K_SETGLABEL_STKFOUR_IN_LCR (X_RangeCheckError, procedure_addr, filename_addr, range_info2, range_info1);
	ENTRY_TRACE (X_RangeCheckError, "%p, %p, %x, %x", (void *)procedure_addr, (void *)filename_addr, range_info2, range_info1);

	handle_range_error (range_info1, range_info2, filename_addr, procedure_addr);
	kernel_X_common_error (Wptr, sched, return_address, "RangeCheckError");
}
/*}}}*/
/*{{{  void kernel_X_BasicRangeError (void)*/
/*
 *	basic range error entry point
 *
 *	@SYMBOL:	X_BasicRangeError
 *	@TYPE:		LCR
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_BRANGERR
 *	@PRIO:		0
 */
void kernel_X_BasicRangeError (void)
{
	K_SETGLABEL_ZERO_IN_LCR (X_BasicRangeError);
	ENTRY_TRACE0 (X_BasicRangeError);

	BMESSAGE ("range error.\n");
	kernel_X_common_error (Wptr, sched, return_address, "BasicRangeError");
}
/*}}}*/
/*{{{  void dump_trap_info (unsigned int return_address, word a_val, word b_val, word c_val)*/
/* dump_trap_info for dumping on TRAP*/
void dump_trap_info (word *Wptr, word *Fptr, word *Bptr, unsigned int return_address, word a_val, word b_val, word c_val)
{
	int i;

	BMESSAGE ("** TRAP **\n");
	MESSAGE ("\tWptr  0x%8.8x    raddr 0x%8.8x\n", (unsigned int)Wptr, (unsigned int)return_address);
	MESSAGE ("\tFptr  0x%8.8x    Bptr  0x%8.8x\n", (unsigned int)Fptr, (unsigned int)Bptr);
	MESSAGE ("\tAreg  0x%8.8x    Iptr  0x%8.8x\n", (unsigned int)a_val, (unsigned int)Wptr[Iptr]);
	MESSAGE ("\tBreg  0x%8.8x    Creg  0x%8.8x\n", (unsigned int)b_val, (unsigned int)c_val);
	for (i=6; i >= -5; i-=2) {
		MESSAGE ("\tWptr[%-2d] @ (0x%8.8x) = 0x%8.8x", i, (unsigned int)&(Wptr[i]), (unsigned int)Wptr[i]);
		MESSAGE ("\tWptr[%-2d] @ (0x%8.8x) = 0x%8.8x\n", i-1, (unsigned int)&(Wptr[i-1]), (unsigned int)Wptr[i-1]);
	}
	
	ccsp_show_last_debug_insert ();
	return;
}
/*}}}*/
#if defined(ENABLE_DTRACES) && !defined(RMOX_BUILD)
/*{{{  void kernel_X_dtrace (void)*/
/*
 *	this handles debugging traces generated by tranx86
 *
 *	@SYMBOL:	X_dtrace
 *	@TYPE:		LCR
 *	@INPUT:		STACK(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_DTRACE
 *	@PRIO:		0
 *	@DEPEND:	ENABLE_DTRACES
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
void kernel_X_dtrace (void)
{
	unsigned int trapval_A, trapval_B, trapval_C;
	
	K_SETGLABEL_STKTWO_IN_LCR (X_dtrace, trapval_A, trapval_B);
	ENTRY_TRACE (X_dtrace, "0x8.8x, 0x8.8x", trapval_A, trapval_B);

	rt_dtrace ((void *)Wptr, trapval_A, trapval_B);

	K_ZERO_OUT ();
}
/*}}}*/
#endif	/* defined(ENABLE_DTRACES) && !defined(RMOX_BUILD) */
/*{{{  void kernel_X_trap (void)*/
/*
 *	trap entry-point
 *
 *	@SYMBOL:	X_trap
 *	@TYPE:		LCR
 *	@INPUT:		STACK(3)
 *	@OUTPUT: 	STACK(3)
 *	@CALL: 		K_TRAP
 *	@PRIO:		0
 */
void kernel_X_trap (void)
{
	unsigned int trapval_A, trapval_B, trapval_C;
	
	K_SETGLABEL_STKTHREE_IN_LCR (X_trap, trapval_A, trapval_B, trapval_C);
	ENTRY_TRACE (X_trap, "0x%x, 0x%x, 0x%x", trapval_A, trapval_B, trapval_C);
	
	dump_trap_info (Wptr, sched->curb.Fptr, sched->curb.Bptr, return_address, trapval_A, trapval_B, trapval_C);

	K_STKTHREE_OUT (trapval_A, trapval_B, trapval_C);
}
/*}}}*/
/*{{{  void kernel_Y_unsupported (void)*/
/*
 *	this handles unsupported kernel calls
 *
 *	@SYMBOL:	Y_unsupported
 *	@TYPE:		LCR
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_UNSUPPORTED
 *	@PRIO:		0
 *	@HANDLE:	K_UNPACKSN, K_POSTNORMSN, K_ROUNDSN
 *	@HANDLE:	K_SAVEL, K_STLB, K_STLF
 *	@HANDLE:	K_RES_RELEASE, K_RES_CLAIM
 *	@HANDLE:	K_MNEW, K_MFREE
 *	@HANDLE:	K_EXTIN, K_EXTOUT, K_EXTVRFY
 *	@HANDLE:	K_EXTIN8, K_EXTIN32, K_EXTOUT8, K_EXTOUT32
 *	@HANDLE:	K_EXTOUTBYTE, K_EXTOUTWORD, K_EXTENBC, K_EXTNDISC
 *	@HANDLE:	K_EXTMIN, K_EXTMIN64, K_EXTMOUT, K_EXTMOUT64
 *	@HANDLE:	K_EXTMINN, K_EXTMOUTN
 *	@HANDLE:	K_SAVEPRI, K_STPRIF, K_STPRIB
 *	@HANDLE:	K_MWENB, K_MWDIS, K_MWALTWT, K_MWALT, K_MWALTEND
 *	@HANDLE:	K_FASTIN8, K_FASTIN32, K_FASTIN, K_FASTOUT8, K_FASTOUT32, K_FASTOUT
 *	@HANDLE:	K_MWS_BINIT, K_MWS_PBRILNK, K_MWS_PBRULNK, K_MWS_PPILNK
 *	@HANDLE:	K_MWS_PBENROLL, K_MWS_PBRESIGN, K_MWS_PBADJSYNC, K_MWS_SYNC
 *	@HANDLE:	K_MWS_ALTLOCK, K_MWS_ALTUNLOCK, K_MWS_ALT, K_MWS_ALTEND
 *	@HANDLE:	K_MWS_ENB, K_MWS_DIS, K_MWS_ALTPOSTLOCK
 *	@HANDLE:	K_MWS_PPBASEOF, K_MWS_PPPAROF
 *	@HANDLE:	K_MIN, K_MOUT, K_MIN64, K_MOUT64, K_XMIN, K_XMIN64
 *	@HANDLE:	K_MINN, K_MOUTN, K_XMINN
 *	@HANDLE:	K_FBAR_INIT, K_FBAR_SYNC, K_FBAR_ENROLL, K_FBAR_RESIGN
 */
void kernel_Y_unsupported (void)
{
	K_SETGLABEL_ZERO_IN_RR (Y_unsupported);
	ENTRY_TRACE0 (Y_unsupported);

	if (ccsp_ignore_errors) {
		RESCHEDULE;
	} else {
		BMESSAGE ("unsupported kernel call.\n");
		ccsp_kernel_exit (1, return_address);
	}
}
/*}}}*/
/*}}}*/

/*{{{  semaphore operations */
/*{{{  static INLINE void sem_init (ccsp_sem_t *sem)*/
static INLINE void sem_init (ccsp_sem_t *sem)
{
	atw_set (&(sem->fptr), (word) NotProcess_p);
	atw_set (&(sem->bptr), (word) (NotProcess_p | 1));
	weak_write_barrier ();
}
/*}}}*/
/*{{{  static INLINE word *sem_dequeue (ccsp_sem_t *sem)*/
static INLINE word *sem_dequeue (ccsp_sem_t *sem) {
	word *fptr = (word *) atw_val (&(sem->fptr));

	for (;;) {
		word *to_test = NULL;

		if (fptr != NotProcess_p) {
			word *bptr = (word *) atw_val (&(sem->bptr));
			word *link = (word *) atw_val (&(fptr[Link]));

			if (bptr == fptr) {
				atw_set (&(sem->fptr), (word) NotProcess_p);
				if (atw_cas (&(sem->bptr), (word) fptr, (word) NotProcess_p)) {
					return fptr;
				}
				/* CAS failed, i.e. someone enqueued; so loop */
				atw_set (&(sem->fptr), (word) fptr);
				continue;
			} else if (link != NotProcess_p) {
				atw_set (&(sem->fptr), (word) link);
				return fptr;
			} else {
				to_test = &(fptr[Link]);
			}
		} else {
			to_test = &(sem->fptr);
		}
		
		/* Partially complete enqueue, so we can't dequeue.
		 * Instead we release the lock and retry once.
		 */
		atw_set_bit (&(sem->bptr), 0);
		strong_memory_barrier ();
		if (atw_val (to_test) != NotProcess_p) {
			if (atw_test_clear_bit (&(sem->bptr), 0)) {
				/* got lock back; reload fptr and loop */
				strong_read_barrier ();
				fptr = (word *) atw_val (&(sem->fptr));
				continue;
			}
		}
		return NotProcess_p;
	}
}
/*}}}*/
/*{{{  static INLINE void sem_claim (sched_t *sched, word *Wptr, ccsp_sem_t *sem)*/
static INLINE void sem_claim (sched_t *sched, word *Wptr, ccsp_sem_t *sem)
{
	word *ptr, val;

	if ((val = atw_val (&(sem->bptr))) == (NotProcess_p | 1)) {
		if (atw_cas (&(sem->bptr), val, NotProcess_p)) {
			K_ZERO_OUT_JRET ();
			return;
		}
		/* We could read barrier here, but because of the 
		 * previous CAS our cache is probably clean.
		 * Also as the result of val is verified by the next
		 * CAS there is no danger in a stale value here.
		 */
		val = atw_val (&(sem->bptr));
	}

	save_priofinity (sched, Wptr);
	Wptr[Link] = NotProcess_p;
	weak_write_barrier ();

	while (!atw_cas (&(sem->bptr), val, (word) Wptr)) {
		val = atw_val (&(sem->bptr));
	}
	
	if ((val & (~1)) == NotProcess_p) {
		atw_set (&(sem->fptr), (word) Wptr);
	} else {
		atw_set (&(((word *) (val & (~1)))[Link]), (word) Wptr);
	}

	weak_write_barrier ();

	if (val & 1) {
		if ((ptr = sem_dequeue (sem)) != NotProcess_p) {
			enqueue_process (sched, ptr);
		}
	} else if (atw_test_clear_bit (&(sem->bptr), 0)) {
		strong_read_barrier ();
		if ((ptr = sem_dequeue (sem)) != NotProcess_p) {
			enqueue_process (sched, ptr);
		}
	}

	RESCHEDULE;
}
/*}}}*/
/*{{{  static INLINE void sem_release (sched_t *sched, ccsp_sem_t *sem)*/
static INLINE void sem_release (sched_t *sched, ccsp_sem_t *sem)
{
	word *ptr;

	if ((ptr = (word *) atw_val (&(sem->bptr))) == NotProcess_p) {
		if (atw_cas (&(sem->bptr), (word) ptr, NotProcess_p | 1)) {
			return;
		}
	}

	if ((ptr = sem_dequeue (sem)) != NotProcess_p) {
		enqueue_process (sched, ptr);
	}
}
/*}}}*/
/*}}}*/
/*{{{  barrier operations */
/*{{{  static INLINE void bar_init (bar_t *bar, word initial_count)*/
static INLINE void bar_init (bar_t *bar, word initial_count)
{
	int i;
	bar->state = BAR_STATE (initial_count, 0, 0);
	bar->heads = 0;
	for (i = 0; i < MAX_RUNTIME_THREADS; ++i) {	
		bar->head[i] = (word) NULL;
	}
	weak_write_barrier ();
}
/*}}}*/
/*{{{  static INLINE batch_t *bar_add_batch (sched_t *sched, ...)*/
static INLINE batch_t *bar_add_batch (sched_t *sched, bar_head_t *head, batch_t *parent, bool front, bool current, word *Wptr)
{
	word priority 	= PPriority (sched->priofinity);
	batch_t *batch 	= allocate_batch (sched);

	enqueue_to_batch_with_hint (batch, Wptr, true);
	batch->next 		= NULL;
	batch->priofinity 	= sched->priofinity;
	
	if (parent == NULL) {
		batch->prio[0] = NULL;
		batch->prio[1] = NULL;
		batch->prio[2] = NULL;
		batch->prio[3] = NULL;
		batch->prio[4] = NULL;
		batch->prio[5] = NULL;
		batch->prio[6] = NULL;
		batch->prio[7] = NULL;
	} else {
		batch->prio[0] = parent->prio[0];
		batch->prio[1] = parent->prio[1];
		batch->prio[2] = parent->prio[2];
		batch->prio[3] = parent->prio[3];
		batch->prio[4] = parent->prio[4];
		batch->prio[5] = parent->prio[5];
		batch->prio[6] = parent->prio[6];
		batch->prio[7] = parent->prio[7];
	}

	batch->prio[priority >> BATCH_PRIO_SHIFT] = batch;
	
	if (front) {
		head->fptr = batch;
		head->bptr = batch;
	} else {
		head->bptr->next = batch;
		head->bptr = batch;
	}

	if (current) {
		head->current = batch;
	}

	return batch;
}
/*}}}*/
/*{{{  static INLINE void bar_complete_head (sched_t *sched, bar_t *bar, bool local, bar_head_t *head)*/
static INLINE void bar_complete_head (sched_t *sched, bar_t *bar, bool local, bar_head_t *head)
{
	batch_t *batch = head->fptr;

	atw_add (&(bar->state), head->count);

	do {
		batch_t *next = batch->next;

		ASSERT ( next != NULL || head->bptr == batch );

		if (PHasAffinity (batch->priofinity)) {
			if (dirty_batch (batch)) {
				word *Wptr = batch->Fptr;

				do {
					word *next = (word *) Wptr[Link];
					enqueue_process (sched, Wptr);
					Wptr = next;
				} while (Wptr != NotProcess_p);

				reinit_batch_t (batch);
				set_batch_clean (batch);
				release_clean_batch (sched, batch);
			} else if (local) {
				ASSERT ( batch->priofinity & sched->id );
				push_batch (sched, batch->priofinity, batch);
			} else {
				mail_batch (PAffinity (batch->priofinity), batch);
			}
		} else {
			push_batch (sched, batch->priofinity, batch);
		}

		batch = next;
	} while (batch != NULL);

	release_bar_head (sched, head);
}
/*}}}*/
/*{{{  static void bar_complete (sched_t *sched, bar_t *bar, word tag)*/
static void bar_complete (sched_t *sched, bar_t *bar, word tag)
{
	for (;;) {
		word heads = atw_swap (&(bar->heads), 0);
		word sync = false;
		word idx;

		/* release local batches first */
		if (heads & sched->id) {
			bar_complete_head (sched, bar, true, BAR_HEAD_PTR(bar->head[sched->index]));
			bar->head[sched->index] = (word) NULL;
			heads &= ~sched->id;
		}

		/* release other threads batches */
		for (idx = 0; heads; idx++) {
			word ptr;

			if (!(heads & (1 << idx))) {
				idx = bsf (heads);
			}
			
			ptr = atw_val (&(bar->head[idx]));

			if (ptr) {
				if (BAR_HEAD_TAG(ptr) == tag) {
					atw_set_bit (&(bar->heads), idx);
				} else if (atw_cas (&(bar->head[idx]), ptr, (word) NULL)) {
					bar_complete_head (sched, bar, false, BAR_HEAD_PTR(ptr));
				}
			}

			heads &= ~(1 << idx);
		}

		do {
			word state = atw_val (&(bar->state));

			if ((state == BAR_STATE (1, tag, BAR_SYNCING)) && atw_val (&(bar->heads))) {
				word next_tag = BAR_NEXT_TAG (tag);

				if (atw_cas (&(bar->state), state, BAR_STATE (1, next_tag, BAR_SYNCING))) {
					sync = true;
					tag = next_tag;
				}
			} else if (atw_cas (&(bar->state), state, BAR_STATE (BAR_COUNT(state) - 1, tag, 0)))  {
				return;
			}
		} while (!sync);
	}
}
/*}}}*/
/*{{{  static void bar_enroll (sched_t *sched, bar_t *bar, word count)*/
static void bar_enroll (sched_t *sched, bar_t *bar, word count)
{
	atw_add (&(bar->state), count);
}
/*}}}*/
/*{{{  static void bar_resign (sched_t *sched, bar_t *bar, word resign_count)*/
static void bar_resign (sched_t *sched, bar_t *bar, word resign_count)
{
	for (;;) {
		word state = atw_val (&(bar->state));
		word tag = BAR_TAG(state);

		if ((state & (~BAR_TAG_MASK)) == resign_count) {
			if (atw_val (&(bar->heads))) {
				tag = BAR_NEXT_TAG (tag);

				if (atw_cas (&(bar->state), state, BAR_STATE (1, tag, BAR_SYNCING))) {
					bar_complete (sched, bar, tag);
					return;
				}
			} else {
				if (atw_cas (&(bar->state), state, BAR_STATE (0, tag, 0))) {
					return;
				}
			}
		} else {
			word count = BAR_COUNT(state);
			word new_state = BAR_STATE (count - resign_count, tag, state & BAR_SYNCING);
			if (atw_cas (&(bar->state), state, new_state))  {
				return;
			}
		}
	}
}
/*}}}*/
/*{{{  static void bar_sync (sched_t *sched, bar_t *bar, word *Wptr)*/
static void bar_sync (sched_t *sched, bar_t *bar, word *Wptr)
{
	bar_head_t *head;
	word bsize, hdata, state;
	
	save_priofinity (sched, Wptr);

	state = atw_val (&(bar->state));
	hdata = atw_val (&(bar->head[sched->index]));

	if (state & BAR_SYNCING) {
		if (!hdata || (BAR_HEAD_TAG(hdata) != BAR_TAG(state))) {
			if (hdata) {
				hdata = atw_swap (&(bar->head[sched->index]), (word) NULL);
				if (hdata) {
					bar_complete_head (sched, bar, true, BAR_HEAD_PTR(hdata));
					hdata = 0;
				}
			}
			head = allocate_bar_head (sched);
			atw_set_bit (&(bar->heads), sched->index);
			atw_set (&(bar->head[sched->index]), ((word) head) | BAR_TAG(state));
		} else {
			head = BAR_HEAD_PTR(hdata);
		}
	} else if (!hdata) {
		head = allocate_bar_head (sched);
		atw_set_bit (&(bar->heads), sched->index);
		atw_set (&(bar->head[sched->index]), ((word) head) | BAR_TAG(state));
	} else {
		head = BAR_HEAD_PTR(hdata);
	}
	
	bsize = BAR_COUNT(state) + head->count;
	bsize = (bsize >> 3) | (bsize & 0xf);

	head->count++;

	if (PHasAffinity (sched->priofinity)) {
		batch_t *batch;

		if ((batch = head->affine) == NULL) {
			batch = head->affine = allocate_batch (sched);
			
			enqueue_to_batch_with_hint (batch, Wptr, true);
			batch->next 		= NULL;
			batch->priofinity 	= sched->priofinity;
			
			if (!hdata) {
				head->fptr = batch;
			} else {
				head->bptr->next = batch;
			}
			head->bptr = batch;
		} else {
			if (batch->priofinity != sched->priofinity) {
				set_batch_dirty (batch);
			}
			
			enqueue_to_batch_with_hint (batch, Wptr, false);
			
			if (batch->size > bsize) {
				head->affine = NULL;
			}
		}
	} else if (!hdata) {
		bar_add_batch (sched, head, NULL, true, true, Wptr);
	} else if (head->current != NULL) {
		batch_t *batch = head->current;

		if (batch->priofinity == sched->priofinity) {
			if (batch->size >= bsize) {
				bar_add_batch (sched, head, NULL, false, true, Wptr);
			} else {
				enqueue_to_batch_with_hint (batch, Wptr, false);
			}
		} else {
			word bprio = PPriority (batch->priofinity) & BAR_HEAD_PRIO_MASK;
			word prio = PPriority (sched->priofinity) & BAR_HEAD_PRIO_MASK;
			head->current = NULL;
			head->prio[bprio] = batch;
			head->prio[prio] = bar_add_batch (sched, head, head->prio[prio], false, false, Wptr);
		}
	} else {
		word prio = PPriority (sched->priofinity);
		word hprio = prio & BAR_HEAD_PRIO_MASK;
		batch_t *batch = head->prio[hprio];
		
		if (batch == NULL) {
			head->prio[hprio] = bar_add_batch (sched, head, NULL, false, false, Wptr);
		} else if (batch->priofinity != sched->priofinity) {
			batch_t *parent = batch;

			batch = parent->prio[prio >> BATCH_PRIO_SHIFT];

			if (batch == NULL) {
				batch = bar_add_batch (sched, head, parent, false, false, Wptr);
			} else if (batch->size >= bsize) {
				batch = bar_add_batch (sched, head, parent, false, false, Wptr);
			} else {
				enqueue_to_batch_with_hint (batch, Wptr, false);
			}

			head->prio[hprio] = batch;
		} else if (batch->size >= bsize) {
			head->prio[hprio] = bar_add_batch (sched, head, batch, false, false, Wptr);
		} else {
			enqueue_to_batch_with_hint (batch, Wptr, false);
		}
	}

	/* make sure our changes are visible before we potentially complete */
	weak_write_barrier ();

	/* update barrier count, maybe complete barrier */
	for (;;) {
		word tag = BAR_TAG(state);

		if ((state & (~BAR_TAG_MASK)) == 1) {
			tag = BAR_NEXT_TAG (tag);

			if (atw_cas (&(bar->state), state, BAR_STATE (1, tag, BAR_SYNCING))) {
				bar_complete (sched, bar, tag);
				RESCHEDULE;
				return;
			}
		} else {
			word count = BAR_COUNT(state);
			word new_state = BAR_STATE (count - 1, tag, state & BAR_SYNCING);
			if (atw_cas (&(bar->state), state, new_state))  {
				RESCHEDULE;
				return;
			}
		}

		state = atw_val (&(bar->state));
	}
}
/*}}}*/
/*}}}*/
/*{{{  fork barrier operations */
/*{{{  static void fork_bar_complete (sched_t *sched, word *bar)*/
static void fork_bar_complete (sched_t *sched, word *bar)
{
	word *Wptr = (word *) atw_val (bar);

	if (Wptr != NotProcess_p) {
		enqueue_process (sched, Wptr);
	}
}
/*}}}*/
/*{{{  static void fork_bar_enroll (sched_t *sched, word *bar, word count)*/
static void fork_bar_enroll (sched_t *sched, word *bar, word count)
{
	/* no operation */
}
/*}}}*/
/*{{{  static void fork_bar_resign (sched_t *sched, word *bar, word count)*/
static void fork_bar_resign (sched_t *sched, word *bar, word count)
{
	/* no operation */
}
/*}}}*/
/*{{{  static void fork_bar_sync (sched_t *sched, word *bar, word *Wptr)*/
static void fork_bar_sync (sched_t *sched, word *bar, word *Wptr)
{
	mt_barrier_internal_t *mb = (mt_barrier_internal_t *)
		(((byte *) (bar - MT_BARRIER_PTR_OFFSET)) - (3 * sizeof (void *)));

	save_priofinity (sched, Wptr);
	atw_set (bar, Wptr);
	weak_write_barrier ();

	if (atw_dec_z (&(mb->ref_count))) {
		dmem_thread_release (sched->allocator, mb);
		K_ZERO_OUT_JRET ();
	} else {
		RESCHEDULE;
	}
}
/*}}}*/
/*}}}*/
/*{{{  mobile process barrier operations */
#if !defined(RMOX_BUILD) && defined(DYNAMIC_PROCS)
/*{{{  static void mproc_bar_init (mproc_bar_t *bar, word initial_count)*/
static void mproc_bar_init (mproc_bar_t *bar, word initial_count)
{
	ASSERT (initial_count > 0);
	bar->enrolled	= initial_count;
	bar->state	= initial_count - 1;
	bar->fptr	= NotProcess_p;
	bar->bptr	= NotProcess_p;
}
/*}}}*/
/*{{{  static void mproc_bar_complete (sched_t *sched, mproc_bar_t *bar)*/
static void mproc_bar_complete (sched_t *sched, mproc_bar_t *bar)
{
	word enrolled = atw_val (&(bar->enrolled));
	word *ws = (word *) atw_val (&(bar->fptr));

	atw_set (&(bar->state), enrolled - 1);
	atw_set ((word *) &(bar->fptr), NotProcess_p);
	atw_set ((word *) &(bar->bptr), NotProcess_p);

	weak_write_barrier ();

	while (ws != NotProcess_p) {
		word *next = (word *) ws[Link];
		enqueue_process (sched, ws);
		ws = next;
	}
}
/*}}}*/
/*{{{  static void mproc_bar_enroll (sched_t *sched, mproc_bar_t *bar, word count)*/
static void mproc_bar_enroll (sched_t *sched, mproc_bar_t *bar, word count)
{
	atw_add (&(bar->enrolled), count);
	atw_add (&(bar->state), count);
}
/*}}}*/
/*{{{  static void mproc_bar_resign (sched_t *sched, mproc_bar_t *bar, word count)*/
static void mproc_bar_resign (sched_t *sched, mproc_bar_t *bar, word count)
{
	
	atw_sub (&(bar->enrolled), count);

	for (;;) {
		word state = atw_val (&(bar->state));
		if ((state & MPROC_BAR_COUNT) < count) {
			/* completed barrier: re-schedule a process */
			word *Wptr = (word *) atw_val (&(bar->fptr));
			
			if ((state & MPROC_BAR_PHASE) || (Wptr == NotProcess_p)) {
				BMESSAGE ("mobile process barrier inconsistent (state: %08x, resign: %d\n)", state, count);
				ccsp_kernel_exit (1, 0);
			}
			
			atw_set (&(bar->state), MPROC_BAR_PHASE);
			atw_set (&(Wptr[Temp]), 1);

			weak_write_barrier ();

			enqueue_process (sched, Wptr);

			return;
		} else if (atw_cas (&(bar->state), state, state - count)) {
			return;
		}	
	}	
}
/*}}}*/
/*{{{  static void mproc_bar_sync (sched_t *sched, mproc_bar_t *bar, word *Wptr)*/
static void mproc_bar_sync (sched_t *sched, mproc_bar_t *bar, word *Wptr)
{
	word retry = false;
	
	for (;;) {
		word state = atw_val (&(bar->state));

		if ((state & MPROC_BAR_COUNT) == 0) {
			/* last process */
			if (retry) {
				/* special case: remove ourselves from queue */
				word *prev = NotProcess_p;
				word *ws;
				
				strong_read_barrier ();

				for (ws = bar->fptr; ws != Wptr; ws = (word *) ws[Link]) {
					prev = ws;
				}

				if (prev == NotProcess_p) {
					bar->fptr = (word *) ws[Link];
				} else {
					prev[Link] = ws[Link];
				}
				if (ws[Link] == NotProcess_p) {
					bar->bptr = prev;
				}

				weak_write_barrier ();
			}

			if ((state & MPROC_BAR_PHASE) == 0) {
				/* phase 0: re-schedule just this process */
				atw_set (&(bar->state), MPROC_BAR_PHASE);
				atw_set (&(Wptr[Temp]), 1);
			} else {
				/* phase 1: re-schedule all processes, reset barrier */
				mproc_bar_complete (sched, bar);
			}
			
			K_ZERO_OUT_JRET ();
			return;
		} else {
			/* not last process: queue */
			word *bptr;

			atw_set (&(Wptr[Link]), NotProcess_p);
			atw_set (&(Wptr[Temp]), 0);
			
			weak_write_barrier ();
			
			bptr = (word *) atw_swap ((word *) &(bar->bptr), (word) Wptr);

			if (bptr == NotProcess_p) {
				atw_set ((word *) &(bar->fptr), (word) Wptr);
			} else {
				atw_set ((word *) &(bptr[Link]), (word) Wptr);
			}

			if (atw_cas (&(bar->state), state, state - 1)) {
				RESCHEDULE;
				return;
			}

			/* retry: loop */
			retry = true;
		}
	}
}
/*}}}*/
#endif
/*}}}*/
/*{{{  mobile type operations */
/*{{{ mobile_type_error() */
#define mobile_type_error() \
	do { \
		BMESSAGE ("mobile typing error (%s:%d)\n", __FILE__, __LINE__); \
		ccsp_kernel_exit (1, 0); \
	} while (0)
/*}}}*/
/*{{{  static INLINE mt_array_internal_t *mt_alloc_array_internal (...)*/
static INLINE mt_array_internal_t *mt_alloc_array_internal (void *allocator, word type, word size, bool init, word *size_shift)
{
	mt_array_internal_t *ma;
	word alignment		= 0;
	word dimensions		= MT_ARRAY_DIM(type);
	word dma		= 0;
	word inner_type		= MT_ARRAY_INNER_TYPE(type);
	word meta_words		= dimensions + MT_ARRAY_PTR_OFFSET + 1;
	word bytes;

	ASSERT ( inner_type & MT_SIMPLE );

	if (MT_TYPE(inner_type) == MT_ARRAY_OPTS) {
		if (MT_FLAGS(inner_type) & MT_ARRAY_OPTS_DMA) {
			/* allocate space for hardware address */
			dma = dimensions;
			meta_words += 1;
		}
		alignment	= (1 << MT_ARRAY_OPTS_ALIGN(inner_type)) - 1;
		inner_type	= MT_ARRAY_OPTS_INNER(inner_type);
	}

	if (MT_TYPE(inner_type) == MT_NUM) {
		*size_shift = mt_num_size_shift (MT_NUM_TYPE(inner_type));

		if ((*size_shift > WSH) && !alignment) {
			alignment = (1 << *size_shift) - 1;
		}
	} else {
		*size_shift = WSH;
	}

	bytes		= (size << *size_shift) + alignment + (meta_words << WSH);
	ma		= (mt_array_internal_t *) dmem_thread_alloc (allocator, bytes);
	ma->size	= size;
	ma->type	= type;
	if (size) {
		ma->array.data = (void *) (
			(word) ((((byte *) ma) + (meta_words << WSH) + alignment)) & (~alignment)
		);
	} else {
		ma->array.data = NULL;
	}
	if (dma) {
		/* eventually this may need virt->phys mapping */
		ma->array.dimensions[dma] = (word) ma->array.data;
	}

	if (init && MT_TYPE(inner_type) != MT_NUM) {
		word **walk = (word **) ma->array.data;
		while (size--) {
			*(walk++) = NULL;
		}
	}

	return ma;
}
/*}}}*/
/*{{{  static INLINE word *mt_alloc_array (void *allocator, word type, word size)*/
static INLINE word *mt_alloc_array (void *allocator, word type, word size)
{
	mt_array_internal_t *ma;
	word size_shift;
	
	ma = mt_alloc_array_internal (allocator, type, size, true, &size_shift);

	return ((word *) ma) + MT_ARRAY_PTR_OFFSET;
}
/*}}}*/
/*{{{  static INLINE word *mt_alloc_cb (void *allocator, word type, word channels)*/
static INLINE word *mt_alloc_cb (void *allocator, word type, word channels)
{
	mt_cb_t *cb;
	word words = channels;
	word i;

	if (type & MT_CB_STATE_SPACE) {
		words += 5;
	}

	if (type & MT_CB_SHARED) {
		mt_cb_shared_internal_t	*i_cb;
		
		words += MT_CB_SHARED_PTR_OFFSET;

		i_cb = (mt_cb_shared_internal_t *) dmem_thread_alloc (allocator, words << WSH);
		sem_init (&(i_cb->sem[0]));
		sem_init (&(i_cb->sem[1]));
		i_cb->ref_count = 2;
		i_cb->type	= type;
		cb		= (mt_cb_t *) (((word *) i_cb) + MT_CB_SHARED_PTR_OFFSET);
	} else {
		mt_cb_internal_t 	*i_cb;
		
		words += MT_CB_PTR_OFFSET;

		i_cb = (mt_cb_internal_t *) dmem_thread_alloc (allocator, words << WSH);
		i_cb->ref_count = 2;
		i_cb->type	= type;
		cb		= (mt_cb_t *) (((word *) i_cb) + MT_CB_PTR_OFFSET);
	}

	for (i = 0; i < channels; ++i) {
		cb->channels[i] = NotProcess_p;
	}

	return (word *) cb;
}
/*}}}*/
/*{{{  static INLINE word *mt_alloc_barrier (void *allocator, word type)*/
static INLINE word *mt_alloc_barrier (void *allocator, word type)
{	
	mt_barrier_internal_t *mb;
	word bytes = (MT_BARRIER_PTR_OFFSET * sizeof (word)) + (3 * sizeof (void *));

	switch (MT_BARRIER_TYPE (type)) {
		case MT_BARRIER_FULL:
			bytes 	+= sizeof (bar_t);
			mb 	= (mt_barrier_internal_t *) dmem_thread_alloc (allocator, bytes);
			mb->barrier.sync 	= (ccsp_barrier_sync_t) bar_sync;
			mb->barrier.enroll	= (ccsp_barrier_enroll_t) bar_enroll;
			mb->barrier.resign	= (ccsp_barrier_resign_t) bar_resign;
			bar_init ((bar_t *) &(mb->barrier.data), 1);
			break;
		case MT_BARRIER_FORKING:
			bytes	+= sizeof (word);
			mb 	= (mt_barrier_internal_t *) dmem_thread_alloc (allocator, bytes);
			mb->barrier.sync	= (ccsp_barrier_sync_t) fork_bar_sync;
			mb->barrier.enroll	= (ccsp_barrier_enroll_t) fork_bar_enroll;
			mb->barrier.resign	= (ccsp_barrier_resign_t) fork_bar_resign;
			mb->barrier.data[0]	= NotProcess_p;
	  		break;
		#if !defined(RMOX_BUILD) && defined(DYNAMIC_PROCS)
		case MT_BARRIER_MPROC:
			bytes 	+= sizeof (mproc_bar_t);
			mb 	= (mt_barrier_internal_t *) dmem_thread_alloc (allocator, bytes);
			mb->barrier.sync	= (ccsp_barrier_sync_t) mproc_bar_sync;
			mb->barrier.enroll	= (ccsp_barrier_enroll_t) mproc_bar_enroll;
			mb->barrier.resign	= (ccsp_barrier_resign_t) mproc_bar_resign;
			mproc_bar_init ((mproc_bar_t *) &(mb->barrier.data), 1);
			break;
		#endif /* !defined(RMOX_BUILD) && defined(DYNAMIC_PROCS) */
		default:
			mobile_type_error ();
			mb = NULL;
			break;
	}
		
	mb->ref_count	= 1;
	mb->type	= type;
	
	return ((word *) mb) + MT_BARRIER_PTR_OFFSET;
}
/*}}}*/
/*{{{  static INLINE word *mt_alloc_data (void *allocator, word type, word size)*/
static INLINE word *mt_alloc_data (void *allocator, word type, word size)
{	
	mt_data_internal_t *md;
	word bytes = (size + (sizeof(word) - 1)) & (~(sizeof(word) - 1));
	
	bytes += MT_DATA_PTR_OFFSET << WSH;

	md = (mt_data_internal_t *) dmem_thread_alloc (allocator, bytes);
	md->size	= size;
	md->type	= type;

	return ((word *) md) + MT_DATA_PTR_OFFSET;
}
/*}}}*/
/*{{{  static void mt_release_simple (sched_t *sched, word *ptr, word type)*/
static void mt_release (sched_t *sched, word *ptr);
static void mt_release_simple (sched_t *sched, word *ptr, word type)
{
	void *allocator = sched != NULL ? sched->allocator : NULL;

	switch (MT_TYPE(type)) {
		case MT_ARRAY:
			{
				
				mt_array_internal_t *ma = (mt_array_internal_t *) 
					(ptr - MT_ARRAY_PTR_OFFSET);
				word inner_type = MT_ARRAY_INNER_TYPE(type);

				ASSERT ( inner_type & MT_SIMPLE );

				if (MT_TYPE(inner_type) == MT_ARRAY_OPTS) {
					inner_type = MT_ARRAY_OPTS_INNER(inner_type);
				}

				if (MT_TYPE(inner_type) != MT_NUM) {
					word size	= ma->size;
					word **walk	= (word **) ma->array.data;
					word i;

					for (i = 0; i < size; ++i, ++walk) {
						if (*walk != NULL) {
							mt_release (sched, *walk);
						}
					}
				}

				dmem_thread_release (allocator, ma);
			}
			break;
		case MT_CB:
			{
				mt_cb_internal_t *cb = (mt_cb_internal_t *) (ptr - MT_CB_PTR_OFFSET);

				if (atw_dec_z (&(cb->ref_count))) {
					if (type & MT_CB_SHARED) {
						dmem_thread_release (
							allocator, 
							ptr - MT_CB_SHARED_PTR_OFFSET
						);
					} else {
						dmem_thread_release (allocator, cb);
					}
				}
			}
			break;
		case MT_BARRIER:
			{
				mt_barrier_internal_t *mb = (mt_barrier_internal_t *)
					(ptr - MT_BARRIER_PTR_OFFSET);
				
				if (sched == NULL)
					mobile_type_error ();

				if (atw_dec_z (&(mb->ref_count))) {
					if (MT_BARRIER_TYPE(mb->type) == MT_BARRIER_FORKING) {
						fork_bar_complete (sched, (word *) &(mb->barrier.data));
					}
					dmem_thread_release (allocator, mb);
				} else {
					mb->barrier.resign (sched, &(mb->barrier.data), 1);
				}
			}
			break;
		case MT_DATA:
			{
				mt_data_internal_t *md = (mt_data_internal_t *)
					(ptr - MT_DATA_PTR_OFFSET);
				dmem_thread_release (allocator, md);
			}
			break;
		default:
			mobile_type_error ();
			break;
	}
}
/*}}}*/
/*{{{  static void mt_release (sched_t *sched, word *ptr)*/
static void mt_release (sched_t *sched, word *ptr)
{
	word type = ptr[MTType];

	if (type & MT_SIMPLE) {
		mt_release_simple (sched, ptr, type);
	} else {
		mobile_type_error ();
	}
}
/*}}}*/
/*{{{  static INLINE word *mt_clone_array (sched_t *sched, word *ptr, word type)*/
static word *mt_clone (sched_t *sched, word *ptr);
static INLINE word *mt_clone_array (sched_t *sched, word *ptr, word type)
{
	mt_array_internal_t *src = (mt_array_internal_t *) (ptr - MT_ARRAY_PTR_OFFSET);
	mt_array_internal_t *dst;
	word dimensions	= MT_ARRAY_DIM(type);
	word inner_type	= MT_ARRAY_INNER_TYPE(type);
	word size_shift;
	word i;

	dst = mt_alloc_array_internal (sched->allocator, type, src->size, false, &size_shift);

	for (i = 0; i < dimensions; ++i) {
		dst->array.dimensions[i] = src->array.dimensions[i];
	}

	if (MT_TYPE(inner_type) == MT_ARRAY_OPTS) {
		inner_type = MT_ARRAY_OPTS_INNER(inner_type);
	}

	if (MT_TYPE(inner_type) == MT_NUM) {
		memcpy (dst->array.data, src->array.data, src->size << size_shift);
	} else {
		word **dst_walk = (word **) dst->array.data;
		word **src_walk = (word **) src->array.data;

		for (i = dst->size; i > 0; --i, ++dst_walk, ++src_walk) {
			if (*src_walk != NULL) {
				*dst_walk = mt_clone (sched, *src_walk);
			} else {
				*dst_walk = NULL;
			}
		}
	}

	return ((word *) dst) + MT_ARRAY_PTR_OFFSET;
}	
/*}}}*/
/*{{{  static INLINE word *mt_clone_simple (sched_t *sched, word *ptr, word type)*/
static INLINE word *mt_clone_simple (sched_t *sched, word *ptr, word type)
{
	switch (MT_TYPE(type)) {
		case MT_ARRAY:
			return mt_clone_array (sched, ptr, type);
		case MT_CB:
			{
				mt_cb_internal_t *cb = (mt_cb_internal_t *) (ptr - MT_CB_PTR_OFFSET);
				atw_inc (&(cb->ref_count));
			}
			return ptr;
		case MT_BARRIER:
			{
				mt_barrier_internal_t *mb = (mt_barrier_internal_t *)
					(ptr - MT_BARRIER_PTR_OFFSET);
				
				atw_inc (&(mb->ref_count));
				mb->barrier.enroll (sched, &(mb->barrier.data), 1);
			}
			return ptr;
		case MT_DATA:
			{
				mt_data_internal_t *src = (mt_data_internal_t *)
					(ptr - MT_DATA_PTR_OFFSET);
				word *dst;

				dst = mt_alloc_data (sched->allocator, src->type, src->size);
				memcpy (dst, &(src->data.data[0]), src->size);

				return dst;
			}
		default:
			mobile_type_error ();
			return NULL;
	}
}
/*}}}*/
/*{{{  static word *mt_clone (sched_t *sched, word *ptr)*/
static word *mt_clone (sched_t *sched, word *ptr)
{
	word type = ptr[MTType];

	if (type & MT_SIMPLE) {
		return mt_clone_simple (sched, ptr, type);
	} else {
		mobile_type_error ();
		return NULL;
	}
}
/*}}}*/
/*{{{  static INLINE void mt_io_update_shared_cb (word **pptr)*/
static INLINE void mt_io_update_shared_cb (word **pptr)
{
	mt_cb_shared_internal_t *cb = (mt_cb_shared_internal_t *) ((*pptr) - MT_CB_SHARED_PTR_OFFSET);
	atw_inc (&(cb->ref_count));
}
/*}}}*/
/*{{{  static INLINE void mt_io_update_barrier (sched_t *sched, word **pptr)*/
static INLINE void mt_io_update_barrier (sched_t *sched, word **pptr)
{
	mt_barrier_internal_t *mb = (mt_barrier_internal_t *) ((*pptr) - MT_BARRIER_PTR_OFFSET);
	
	atw_inc (&(mb->ref_count));
	mb->barrier.enroll (sched, &(mb->barrier.data), 1);
}
/*}}}*/
/*{{{  static INLINE void mt_io_update_array (sched_t *sched, void *ptr, word inner)*/
static HOT bool mt_io_update (sched_t *sched, word **pptr);
static void mt_io_update_array (sched_t *sched, word **pptr, word inner)
{
	mt_array_internal_t *ma = (mt_array_internal_t *) ((*pptr) - MT_ARRAY_PTR_OFFSET);
	word **data = (word **) ma->array.data;
	word size = ma->size;

	switch (MT_TYPE(inner)) {
		case MT_ARRAY:
			inner = MT_ARRAY_INNER_TYPE (inner);
			while (size--) {
				if (*data != NULL) {
					mt_io_update_array (sched, data, inner);
				}
				data++;
			}
			break;
		case MT_CB:
			if (!(inner & MT_CB_SHARED)) {
				break;
			}
			while (size--) {
				if (*data != NULL) {
					mt_io_update_shared_cb (data);
				}
				data++;
			}
			break;
		case MT_BARRIER:
			while (size--) {
				if (*data != NULL) {
					mt_io_update_barrier (sched, data);
				}
				data++;
			}
			break;
		case MT_MT:
			while (size--) {
				if (*data != NULL) {
					mt_io_update (sched, data);
				}
				data++;
			}
			break;
		case MT_DATA:
			break;
		default:
			/* explode here */
			break;
	}
}
/*}}}*/
/*{{{  static HOT bool mt_io_update (sched_t *sched, word **pptr)*/
static HOT bool mt_io_update (sched_t *sched, word **pptr)
{
	word type = (*pptr)[MTType];

	if (type & MT_SIMPLE) {
		if (MT_TYPE(type) == MT_ARRAY) {
			word temp = type;

			do {
				temp = MT_ARRAY_INNER_TYPE (temp);
				if (MT_TYPE(temp) == MT_ARRAY_OPTS) {
					temp = MT_ARRAY_OPTS_INNER(temp);
				}
				if (MT_TYPE(temp) == MT_NUM) {
					return true;
				}
			} while (MT_TYPE(temp) == MT_ARRAY);

			mt_io_update_array (sched, pptr, MT_ARRAY_INNER_TYPE(type));

			return true;
		} else if (MT_TYPE(type) == MT_CB) {
			if (type & MT_CB_SHARED) {
				mt_io_update_shared_cb (pptr);
				return false;
			} else {
				return true;
			}
		} else if (MT_TYPE(type) == MT_BARRIER) {
			mt_io_update_barrier (sched, pptr);
			return false;
		} else if (MT_TYPE(type) == MT_DATA) {
			return true;
		}
	} else {
		mobile_type_error ();
	}
	
	return false;
}
/*}}}*/
/*{{{  static HOT word *mt_alloc (void *allocator, word type, word size)*/
static HOT word *mt_alloc (void *allocator, word type, word size)
{
	if (type & MT_SIMPLE) {
		switch (MT_TYPE(type)) {
			case MT_ARRAY:
				return mt_alloc_array (allocator, type, size);
			case MT_CB:
				return mt_alloc_cb (allocator, type, size);
			case MT_BARRIER:
				return mt_alloc_barrier (allocator, type);
			case MT_DATA:
				return mt_alloc_data (allocator, type, size);
			default:
				break;
		}
	}

	mobile_type_error ();

	return NULL;
}
/*}}}*/
/*{{{  void *ccsp_mt_alloc (word type, word size)*/
void *ccsp_mt_alloc (word type, word size)
{
	sched_t *sched = _local_scheduler;
	void *allocator = sched != NULL ? sched->allocator : NULL;
	
	return mt_alloc (allocator, type, size);
}
/*}}}*/
/*{{{  void ccsp_mt_release (void *ptr)*/
void ccsp_mt_release (void *ptr)
{
	sched_t *sched = _local_scheduler;
	
	mt_release (sched, (word *) ptr);
}
/*}}}*/
/*}}}*/

/*{{{  blocking system calls */
#if !defined(RMOX_BUILD) && defined(BLOCKING_SYSCALLS)
/*{{{  static void kernel_bsc_dispatch (...)*/
static void kernel_bsc_dispatch (sched_t *sched, unsigned int return_address, word *Wptr, void *b_func, void *b_param, int adjust)
{
	bsc_batch_t *job;

	#ifdef BLOCKING_DEBUG
	{
		int dummy, i;
		(void)&dummy;
		MESSAGE ("kernel_bsc_dispatch: return_address = %p, b_param = %p, b_func = %p, Wptr = %p, adjust = %d\n",
			(void *)return_address, (void *)b_param, (void *)b_func, (void *)Wptr, adjust);
		for (i=6; i >= -5; i-=2) {
			MESSAGE ("\tWptr[%-2d] @ (0x%8.8x) = 0x%8.8x", i, (unsigned int)&(Wptr[i]), (unsigned int)Wptr[i]);
			MESSAGE ("\tWptr[%-2d] @ (0x%8.8x) = 0x%8.8x\n", i-1, (unsigned int)&(Wptr[i-1]), (unsigned int)Wptr[i-1]);
		}
	}
	#endif
	
	Wptr[Link]	= NotProcess_p;

	job 		= (bsc_batch_t *) allocate_batch (sched);
	job->wptr 	= Wptr;
	job->bptr 	= Wptr;
	job->size 	= 1;
	job->priofinity = sched->priofinity;
	job->bsc.ws_arg = (word *) b_param;
	job->bsc.func	= (void (*)(word *)) b_func;
	job->bsc.iptr	= (word) return_address;
	job->bsc.adjust	= adjust;

	bsyscall_dispatch (job);
	
	RESCHEDULE;
}
/*}}}*/
/*{{{  void kernel_X_b_dispatch (void)*/
/*
 *	dispatches a blocking call
 *
 *	@SYMBOL:	X_b_dispatch
 *	@TYPE:		LCR
 *	@INPUT:		STACK(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_B_DISPATCH
 *	@PRIO:		20
 *	@DEPEND:	BLOCKING_SYSCALLS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
void kernel_X_b_dispatch (void)
{
	void *b_func, *b_param;
	
	K_SETGLABEL_STKTWO_IN_LCR (X_b_dispatch, b_func, b_param);
	ENTRY_TRACE (X_b_dispatch, "%p, %p", (void *)b_func, (void *)b_param);

	kernel_bsc_dispatch (sched, return_address, Wptr, b_func, b_param, 0);
}
/*}}}*/
/*{{{  void kernel_X_bx_dispatch (void)*/
/*
 *	dispatches a terminateable blocking call
 *
 *	@SYMBOL:	X_bx_dispatch
 *	@TYPE:		LCR
 *	@INPUT:		STACK(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_BX_DISPATCH
 *	@PRIO:		20
 *	@DEPEND:	BLOCKING_SYSCALLS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
void kernel_X_bx_dispatch (void)
{
	void *b_func, *b_param;
	
	K_SETGLABEL_STKTWO_IN_LCR (X_bx_dispatch, b_func, b_param);
	ENTRY_TRACE (X_bx_dispatch, "%p, %p", (void *)b_func, (void *)b_param);

	kernel_bsc_dispatch (sched, return_address, Wptr, b_func, b_param, 1);
}
/*}}}*/
/*{{{  void kernel_Y_bx_kill (void)*/
/*
 *	dispatches a terminateable blocking call
 *
 *	@SYMBOL:	Y_bx_kill
 *	@TYPE:		RR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_BX_KILL
 *	@PRIO:		20
 *	@DEPEND:	BLOCKING_SYSCALLS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
void kernel_Y_bx_kill (void)
{
	word *ptr;
	int result;
	
	K_SETGLABEL_ONE_IN_RR (Y_bx_kill, ptr);
	ENTRY_TRACE (Y_bx_kill, "%p", ptr);

	result = bsyscall_kill (ptr);

	K_ONE_OUT (result);
}
/*}}}*/
#endif	/* RMOX_BUILD || !BLOCKING_SYSCALLS */
/*}}}*/
/*{{{  memory allocation */
/*{{{  void kernel_Y_mt_alloc (void)*/
/*
 *	allocates a new mobile type and initialises it
 *
 *	@SYMBOL:	Y_mt_alloc
 *	@TYPE:		RR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_MT_ALLOC
 *	@PRIO:		100
 */
void kernel_Y_mt_alloc (void)
{
	word *ptr, type, size;
	
	K_SETGLABEL_TWO_IN_RR (Y_mt_alloc, type, size);
	ENTRY_TRACE (Y_mt_alloc, "%08x %d", type, size);

	ptr = mt_alloc (sched->allocator, type, size);

	K_ONE_OUT (ptr);
}
/*}}}*/
/*{{{  void kernel_Y_mt_release (void)*/
/*
 *	frees a mobile type
 *
 *	@SYMBOL:	Y_mt_release
 *	@TYPE:		RR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MT_RELEASE
 *	@PRIO:		100
 */
void kernel_Y_mt_release (void)
{
	word *ptr;
	
	K_SETGLABEL_ONE_IN_RR (Y_mt_release, ptr);
	ENTRY_TRACE (Y_mt_release, "%p", ptr);

	mt_release (sched, ptr);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mt_clone (void)*/
/*
 *	clones a mobile type
 *
 *	@SYMBOL:	Y_mt_clone
 *	@TYPE:		RR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_MT_CLONE
 *	@PRIO:		100
 */
void kernel_Y_mt_clone (void)
{
	word *dst, *src;
	
	K_SETGLABEL_ONE_IN_RR (Y_mt_clone, src);
	ENTRY_TRACE (Y_mt_clone, "%p", src);

	dst = mt_clone (sched, src);

	K_ONE_OUT (dst);
}
/*}}}*/
/*{{{  void kernel_Y_mt_dclone (void)*/
/*
 *	clones some data into a new mobile type
 *
 *	@SYMBOL:	Y_mt_dclone
 *	@TYPE:		RR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_MT_DCLONE
 *	@PRIO:		60
 */
void kernel_Y_mt_dclone (void)
{
	word bytes, *dst, type;
	void *src;
	
	K_SETGLABEL_THREE_IN_RR (Y_mt_dclone, type, bytes, src);
	ENTRY_TRACE (Y_mt_dclone, "%08x, %d, %p", type, bytes, src);
	
	if (bytes && (type == (MT_SIMPLE | MT_MAKE_TYPE (MT_DATA)))) {
		dst = mt_alloc_data (sched->allocator, type, bytes);
		memcpy (dst, src, bytes);
	} else {
		if (bytes) {
			mobile_type_error ();
		}
		dst = NULL;
	}

	K_ONE_OUT (dst);
}
/*}}}*/
/*{{{  void kernel_X_malloc (void)*/
/*
 *	allocates memory of a given size
 *
 *	@SYMBOL:	X_malloc
 *	@TYPE:		RR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_MALLOC
 *	@PRIO:		110
 */
void kernel_X_malloc (void)
{
	word *ptr, size;
	
	K_SETGLABEL_ONE_IN_RR (X_malloc, size);
	ENTRY_TRACE (X_malloc, "%d", size);

	if (size) {
		ptr = mt_alloc_data (sched->allocator, MT_SIMPLE | MT_MAKE_TYPE (MT_DATA), size);
	} else {
		ptr = NULL;
	}
	ENTRY_TRACE (X_malloc_out, "%p", ptr);

	K_ONE_OUT (ptr);
}
/*}}}*/
/*{{{  void kernel_X_mrelease (void)*/
/*
 *	frees memory allocated from _X_malloc()
 *
 *	@SYMBOL:	X_mrelease
 *	@TYPE:		RR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MRELEASE
 *	@PRIO:		110
 */
void kernel_X_mrelease (void)
{
	word *ptr;
	
	K_SETGLABEL_ONE_IN_RR (X_mrelease, ptr);
	ENTRY_TRACE (X_mrelease, "%p", ptr);

	mt_release_simple (sched, ptr, MT_MAKE_TYPE (MT_DATA));

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mt_bind (void)*/
/*
 *	bind a mobile type in some way to a bit of data
 *
 *	@SYMBOL:	Y_mt_bind
 *	@TYPE:		RR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_MT_BIND
 *	@PRIO:		50
 */
void kernel_Y_mt_bind (void)
{
	word bind_type, *data, *ptr, type;
	
	K_SETGLABEL_THREE_IN_RR (Y_mt_bind, bind_type, ptr, data);
	ENTRY_TRACE (Y_mt_bind, "%08x %p %p", bind_type, ptr, data);

	ASSERT (ptr != NULL);

	type = ptr[MTType];
	if ((type & MT_SIMPLE) && (MT_TYPE(type) == MT_ARRAY)) {
		mt_array_t *mt = (mt_array_t *) ptr;
		word dimensions = MT_ARRAY_DIM(type);

		if (bind_type == MT_BIND_VIRTUAL || bind_type == MT_BIND_PHYSICAL) {
			void *phys_addr, *virt_addr;

			if (bind_type == MT_BIND_VIRTUAL) {
				virt_addr = (void *) data;
				phys_addr = virt_addr; /* FIXME: translate */
			} else {
				phys_addr = (void *) data;
				virt_addr = phys_addr; /* FIXME: translate */
			}
			
			if (MT_ARRAY_INNER_TYPE(type) == MT_ARRAY_OPTS) {
				word flags = MT_FLAGS(MT_ARRAY_INNER_TYPE(type));
				
				if (flags & MT_ARRAY_OPTS_SEPARATED) {
					if (mt->data != NULL) {
						mt_release (sched, mt->data);
					}
				}

				if (flags & MT_ARRAY_OPTS_DMA) {
					mt->dimensions[dimensions] = (word) phys_addr;
				}
			}

			mt->data = virt_addr;
		} else {
			mobile_type_error ();
		}
	} else {
		mobile_type_error ();
	}

	K_ONE_OUT (ptr);
}
/*}}}*/
/*}}}*/
/*{{{  numerics */
/*{{{  void kernel_X_norm (void)*/
/*
 *	normalises 64-bit double-length integer (Breg:Areg)
 *
 *	@SYMBOL:	X_norm
 *	@TYPE:		LCR
 *	@INPUT:		STACK(2)
 *	@OUTPUT: 	STACK(3)
 *	@CALL: 		K_NORM
 *	@PRIO:		50
 */
void kernel_X_norm (void)
{
	unsigned int Areg, Breg, Creg;
	
	K_SETGLABEL_STKTWO_IN_LCR (X_norm, Areg, Breg);
	ENTRY_TRACE (X_norm, "0x%x, 0x%x", Areg, Breg);

	Creg = 0;
	if (!Areg && !Breg) {
		Creg = 64;
	} else {
		while (!(Breg & 0x80000000)) {
			Breg <<= 1;
			if (Areg & 0x80000000) {
				Breg |= 1;
			}
			Areg <<= 1;
			Creg++;
		}
	}
	
	K_STKTHREE_OUT (Areg, Breg, Creg);
}
/*}}}*/
/*{{{  void kernel_X_fmul (void)*/
/*
 *	FMUL implementation (easier in C..)
 *
 *	@SYMBOL:	X_fmul
 *	@TYPE:		LCR
 *	@INPUT:		STACK(2)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_FMUL
 *	@PRIO:		50
 */
void kernel_X_fmul (void)
{
	long long tmp_long;
	int hi_word, lo_word;
	int tmpint_a, tmpint_b, tmpint_c;
	
	K_SETGLABEL_STKTWO_IN_LCR (X_fmul, tmpint_a, tmpint_b);
	ENTRY_TRACE (X_fmul, "%d, %d", tmpint_a, tmpint_b);

	/* do calculation */
	tmp_long = (long long)tmpint_a * (long long)tmpint_b;
	hi_word = (int)((tmp_long >> 32) & 0xffffffff);
	lo_word = (int)(tmp_long & 0xffffffff);
	hi_word = (int)((unsigned int)hi_word << 1);
	if ((unsigned int)lo_word & 0x80000000) {
		hi_word |= 1;
	}
	lo_word = (int)((unsigned int)lo_word << 1);
	if (lo_word >= 0) {
		tmpint_c = hi_word;
	} else if (lo_word != 0x80000000) {
		tmpint_c = hi_word + 1;
	} else if (hi_word & 1) {
		tmpint_c = hi_word + 1;
	} else {
		tmpint_c = hi_word;
	}

	K_STKONE_OUT (tmpint_c);
}
/*}}}*/
/*}}}*/

/*{{{  process control */
/*{{{  void kernel_Y_startp (void)*/
/*
 *	start process
 *
 *	@SYMBOL:	Y_startp
 *	@TYPE:		RR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_STARTP
 *	@PRIO:		90
 */
void kernel_Y_startp (void)
{
	unsigned int start_offset;
	word *workspace;

	K_SETGLABEL_TWO_IN_RR (Y_startp, workspace, start_offset);
	ENTRY_TRACE (Y_startp, "%p, %d", workspace, start_offset);

	SAFETY {
		if (sched->curb.Fptr != NotProcess_p) {
			verify_batch_integrity (&(sched->curb));
		}
	}
	
	save_priofinity (sched, workspace);
	workspace[Iptr] = return_address + start_offset;
	enqueue_process_nopri (sched, workspace);
	sched->stats.startp++;

	SAFETY { verify_batch_integrity (&(sched->curb)); }

	if ((--sched->dispatches) <= 0) {
		Wptr[Iptr] = return_address;
		save_priofinity (sched, Wptr);
		enqueue_to_batch_front (&(sched->curb), Wptr);
		RESCHEDULE;
		return;
	} else {
		K_ZERO_OUT ();
		return;
	}
}
/*}}}*/
/*{{{  void kernel_Y_runp (void)*/
/*
 *	run process (fast interface)
 *
 *	@SYMBOL:	Y_runp
 *	@TYPE:		RR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_RUNP
 *	@PRIO:		80
 */
void kernel_Y_runp (void)
{
	word *other_workspace;
	
	K_SETGLABEL_ONE_IN_RR (Y_runp, other_workspace);
	ENTRY_TRACE (Y_runp, "%p", other_workspace);

	other_workspace = (word *)(((word)other_workspace) & (~(sizeof(word) - 1)));
	enqueue_process (sched, other_workspace);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_pause (void)*/
/*
 *	reschedule
 *
 *	@SYMBOL:	X_pause
 *	@TYPE:		SR
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_PAUSE
 *	@PRIO:		80
 */
void kernel_X_pause (void)
{
	K_SETGLABEL_ZERO_IN_SR (X_pause);
	ENTRY_TRACE0 (X_pause);

	save_priofinity (sched, Wptr);
	enqueue_process_nopri (sched, Wptr);
	
	RESCHEDULE;
}
/*}}}*/
/*{{{  void kernel_X_stopp (void)*/
/*
 *	stop process
 *
 *	@SYMBOL:	X_stopp
 *	@TYPE:		SR
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_STOPP
 *	@PRIO:		80
 */
void kernel_X_stopp (void)
{
	K_SETGLABEL_ZERO_IN_SR (X_stopp);
	ENTRY_TRACE0 (X_stopp);

	save_priofinity (sched, Wptr);

	RESCHEDULE;
}
/*}}}*/
/*{{{  void kernel_Y_endp (void)*/
/*
 *	end process (alternate interface)
 *
 *	@SYMBOL:	Y_endp
 *	@TYPE:		JUMP
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_ENDP
 *	@PRIO:		90
 *	@CIF:		FORCE_SR
 */
void kernel_Y_endp (void)
{
	word *ptr;
	
	K_SETGLABEL_ONE_IN (Y_endp, ptr);
	ENTRY_TRACE (Y_endp, "%p", ptr);

	if (atw_dec_z (&(ptr[Count]))) {
		ptr[Priofinity] = ptr[SavedPriority];
		ptr[Iptr]	= ptr[IptrSucc]; /* copy Iptr from top of workspace */

		enqueue_process (sched, ptr);
	} else {
		sched->stats.endp++;
	}

	RESCHEDULE;
}
/*}}}*/
/*{{{  void kernel_Y_par_enroll (void)*/
/*
 *	end process
 *
 *	@SYMBOL:	Y_par_enroll
 *	@TYPE:		RR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_PAR_ENROLL
 *	@PRIO:		50
 */
void kernel_Y_par_enroll (void)
{
	word count, *ptr;
	
	K_SETGLABEL_TWO_IN_RR (Y_par_enroll, count, ptr);
	ENTRY_TRACE (Y_par_enroll, "%d %p", count, ptr);

	atw_add (&(ptr[Count]), count);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_mreleasep (void)*/
/*
 *	frees a process whos workspace was allocated by X_malloc, first adjusting by the parameter
 *
 *	@SYMBOL:	X_mreleasep
 *	@TYPE:		JUMP
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MRELEASEP
 *	@PRIO:		90
 */
void kernel_X_mreleasep (void)
{
	word *ptr, adjust;
	
	K_SETGLABEL_ONE_IN (X_mreleasep, adjust);
	ENTRY_TRACE (X_mreleasep, "%d", adjust);

	ptr = (word *)(((char *) Wptr) + (((signed int) adjust) * sizeof(word)));

	mt_release_simple (sched, ptr, MT_SIMPLE | MT_MAKE_TYPE (MT_DATA));
	
	TRACE { 
		sched->mdparam[14] = (word) Wptr;
		sched->mdparam[15] = (word) adjust;
	}

	RESCHEDULE;
}
/*}}}*/
/*{{{  void kernel_Y_proc_alloc (void)*/
/*
 *	allocate a process workspace
 *
 *	@SYMBOL:	Y_proc_alloc
 *	@TYPE:		RR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_PROC_ALLOC
 *	@PRIO:		90
 */
void kernel_Y_proc_alloc (void)
{
	word flags, words, *ws;

	K_SETGLABEL_TWO_IN_RR (Y_proc_alloc, flags, words);
	ENTRY_TRACE (Y_proc_alloc, "%d, %d", flags, words);

	ws = mt_alloc_data (sched->allocator, MT_SIMPLE | MT_MAKE_TYPE (MT_DATA), words << WSH);
	
	K_ONE_OUT (ws);
}
/*}}}*/
/*{{{  void kernel_Y_proc_param (void)*/
/*
 *	pass a param to workspace allocated via Y_proc_alloc
 *
 *	@SYMBOL:	Y_proc_param
 *	@TYPE:		RR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_PROC_PARAM
 *	@PRIO:		90
 */
void kernel_Y_proc_param (void)
{
	word offset, param, *ws;

	K_SETGLABEL_THREE_IN_RR (Y_proc_param, offset, ws, param);
	ENTRY_TRACE (Y_proc_param, "%d, %p, %08x", offset, ws, param);

	ws[offset] = param;

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_proc_mt_copy (void)*/
/*
 *	copy a mobile type to workspace allocated via Y_proc_alloc
 *
 *	@SYMBOL:	Y_proc_mt_copy
 *	@TYPE:		RR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_PROC_MT_COPY
 *	@PRIO:		90
 */
void kernel_Y_proc_mt_copy (void)
{
	word offset, *ptr, *ws;

	K_SETGLABEL_THREE_IN_RR (Y_proc_mt_copy, offset, ws, ptr);
	ENTRY_TRACE (Y_proc_mt_copy, "%d, %p, %p", offset, ws, ptr);

	if (ptr != NULL) {
		ws[offset] = (word) mt_clone (sched, ptr);
	} else {
		ws[offset] = (word) NULL;
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_proc_mt_move (void)*/
/*
 *	move a mobile type to workspace allocated via Y_proc_alloc
 *
 *	@SYMBOL:	Y_proc_mt_move
 *	@TYPE:		RR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_PROC_MT_MOVE
 *	@PRIO:		90
 */
void kernel_Y_proc_mt_move (void)
{
	word offset, *ptr, **pptr, *ws;

	K_SETGLABEL_THREE_IN_RR (Y_proc_mt_move, offset, ws, pptr);
	ENTRY_TRACE (Y_proc_mt_move, "%d, %p, %p (%p)", offset, ws, pptr, (word *) *pptr);

	ptr = *pptr;
	if (ptr != NULL) {
		if (mt_io_update (sched, &ptr)) {
			*pptr = NULL;
		}
	}
	ws[offset] = (word) ptr;

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_proc_start (void)*/
/*
 *	start a process using a workspace allocated via Y_proc_alloc
 *
 *	@SYMBOL:	Y_proc_start
 *	@TYPE:		RR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_PROC_START
 *	@PRIO:		90
 */
void kernel_Y_proc_start (void)
{
	word code, offset, *ws;

	K_SETGLABEL_THREE_IN_RR (Y_proc_start, offset, ws, code);
	ENTRY_TRACE (Y_proc_start, "%d, %p, %08x", offset, ws, code);

	ws += offset;
	save_priofinity (sched, ws);
	ws[Iptr] = code;

	enqueue_process_nopri (sched, ws);
	sched->stats.proc_start++;

	SAFETY { verify_batch_integrity (&(sched->curb)); }

	if ((--sched->dispatches) <= 0) {
		Wptr[Iptr] = return_address;
		save_priofinity (sched, Wptr);
		enqueue_to_batch_front (&(sched->curb), Wptr);
		RESCHEDULE;
		return;
	} else {
		K_ZERO_OUT ();
		return;
	}
}
/*}}}*/
/*{{{  void kernel_Y_proc_end (void)*/
/*
 *	called by a process started by Y_proc_start to terminate
 *
 *	@SYMBOL:	Y_proc_end
 *	@TYPE:		JUMP
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_PROC_END
 *	@PRIO:		90
 */
void kernel_Y_proc_end (void)
{
	word *ws;

	K_SETGLABEL_ONE_IN (Y_proc_end, ws);
	ENTRY_TRACE (Y_proc_end, "%p", ws);

	mt_release_simple (sched, ws, MT_MAKE_TYPE (MT_DATA));
	sched->stats.proc_end++;

	RESCHEDULE;
}
/*}}}*/
/*{{{  word *ccsp_proc_alloc (word flags, word words)*/
word *ccsp_proc_alloc (word flags, word words)
{
	sched_t *sched = _local_scheduler;
	void *allocator = sched != NULL ? sched->allocator : NULL;
	
	return mt_alloc_data (allocator, MT_SIMPLE | MT_MAKE_TYPE (MT_DATA), words << WSH);
}
/*}}}*/
/*}}}*/
/*{{{  priority and affinity */
/*{{{  void kernel_Y_getaff (void)*/
/*
 *	get processor affinity
 *
 *	@SYMBOL:	Y_getaff
 *	@TYPE:		RR
 *	@INPUT:		NONE
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_GETAFF
 *	@PRIO:		30
 */
void kernel_Y_getaff (void)
{
	K_SETGLABEL_ZERO_IN_RR (Y_getaff);
	ENTRY_TRACE0 (Y_getaff);
	K_STKONE_OUT (PAffinity (sched->priofinity));
}
/*}}}*/
/*{{{  void kernel_Y_setaff (void)*/
/*
 *	set processor affinity
 *
 *	@SYMBOL:	Y_setaff
 *	@TYPE:		SR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_SETAFF
 *	@PRIO:		30
 */
void kernel_Y_setaff (void)
{
	unsigned int affinity;
	
	K_SETGLABEL_ONE_IN_SR (Y_setaff, affinity);
	ENTRY_TRACE (Y_setaff, "%d", affinity);

	if (affinity) {
		unsigned int threads = att_val (&(enabled_threads));

		/* We must make sure we don't set an affinity which 
		 * would prevent a process ever being executed.
		 *
		 * However, we do allow a process to have bits set in 
		 * its affinity mask that don't correspond to valid
		 * threads, so simply masking is not an option.
		 */

		if (!(affinity & threads)) {
			affinity = 0;
		}
	}

	Wptr[Priofinity] = BuildPriofinity (affinity, PPriority (sched->priofinity));

	if (Wptr[Priofinity] != sched->priofinity) {
		enqueue_process (sched, Wptr);
		Wptr = get_process_or_reschedule (sched);
	}

	K_ZERO_OUT_JRET ();
}
/*}}}*/
/*{{{  void kernel_Y_getpas (void)*/
/*
 *	get current raw priofinity
 *
 *	@SYMBOL:	Y_getpas
 *	@TYPE:		RR
 *	@INPUT:		NONE
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_GETPAS
 *	@PRIO:		80
 */
void kernel_Y_getpas (void)
{
	K_SETGLABEL_ZERO_IN_RR (Y_getpas);
	ENTRY_TRACE0 (Y_getpas);
	K_STKONE_OUT (sched->priofinity);
}
/*}}}*/
/*{{{  void kernel_X_getpri (void)*/
/*
 *	get process priority
 *
 *	@SYMBOL:	X_getpri
 *	@TYPE:		LCR
 *	@INPUT:		NONE
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_GETPRI
 *	@PRIO:		30
 */
void kernel_X_getpri (void)
{
	K_SETGLABEL_ZERO_IN_LCR (X_getpri);
	ENTRY_TRACE0 (X_getpri);
	K_STKONE_OUT (PPriority (sched->priofinity));
}
/*}}}*/
/*{{{  void kernel_Y_setpri (void)*/
/*
 *	set process priority
 *
 *	@SYMBOL:	Y_setpri
 *	@TYPE:		SR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_SETPRI
 *	@PRIO:		30
 */
void kernel_Y_setpri (void)
{
	int priority;
	
	K_SETGLABEL_ONE_IN_SR (Y_setpri, priority);
	ENTRY_TRACE (Y_setpri, "%d", priority);

	if (priority < 0) {
		priority = 0;
	} else if (priority >= MAX_PRIORITY_LEVELS) {
		priority = MAX_PRIORITY_LEVELS - 1;
	}
	if (priority != PPriority (sched->priofinity)) {
		Wptr[Priofinity] = BuildPriofinity (PAffinity (sched->priofinity), priority);
		enqueue_process (sched, Wptr);
		Wptr = get_process_or_reschedule (sched);
	}

	K_ZERO_OUT_JRET ();
}
/*}}}*/
/*}}}*/
/*{{{  scheduler */
/*{{{  void kernel_Y_rtthreadinit (void)*/
/*
 *	call-in for a run-time thread as it starts up
 *
 *	@SYMBOL:	Y_rtthreadinit
 *	@TYPE:		RR
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_RTTHREADINIT
 *	@PRIO:		10
 */
void kernel_Y_rtthreadinit (void)
{
	unsigned int stack;
	void *allocator;
	sched_t *sched;
	word i, *fptr, tried, *Wptr;
	
	K_THREADINIT (Y_rtthreadinit);
	ENTRY_TRACE (Y_rtthreadinit, "(%08x)", att_val (&enabled_threads));

	allocator 		= dmem_new_allocator ();
	sched 			= dmem_thread_alloc (allocator, sizeof(sched_t));
	init_sched_t (sched);
	memcpy (sched->calltable, ccsp_calltable, sizeof (ccsp_calltable));
	sched->allocator 	= allocator;
	sched->stack		= stack;
	
	set_local_scheduler (sched);

#if !defined(RMOX_BUILD)
	ccsp_init_signal_pipe (sched);
#endif
	allocate_to_free_list (sched, MAX_PRIORITY_LEVELS * 2);
	for (i = 0; i < MAX_PRIORITY_LEVELS; ++i) {
		sched->rq[i].pending = allocate_batch (sched);
	}
	new_curb (sched);

	while (fptr != NotProcess_p) {
		word *next = (word *) fptr[Link];
		fptr[Priofinity] = sched->priofinity;
		enqueue_process_nopri (sched, fptr);
		sched->stats.startp++;
		fptr = next;
	}

	tried = 0;
	do {
		int available = (~(att_val (&enabled_threads))) & (~tried);
		int n = bsf (available);

		if (available == 0) {
			BMESSAGE0 ("attempted to start more runtime threads than supported\n");
			ccsp_kernel_exit (1, (word) Wptr);
		}
			
		tried 		|= (1 << n);
		sched->index    = n;
		sched->id 	= 1 << n;

	} while (!atw_cas ((word *) &(schedulers[sched->index]), (word) NULL, (word) sched));

	if (!(att_val (&enabled_threads) & (~sched->id))) {
		/* first run-time thread setups up timing then starts the rest */
		#if defined(ENABLE_CPU_TIMERS)
		ccsp_initial_cpu_speed (&(sched->cpu_factor), &(sched->cpu_khz));
		#endif /* defined(ENABLE_CPU_TIMERS) */

		#if defined(RMOX_BUILD)
		sched->spin = 0;
		#else
		setup_spin (sched);
		#endif /* defined (RMOX_BUILD) */

		att_set_bit (&enabled_threads, sched->index);
		
		ccsp_start_threads ();
	} else {
		/* copy calibration from another thread */
		unsigned int other 	= bsf (att_val (&enabled_threads));
		#if defined(ENABLE_CPU_TIMERS)
		sched->cpu_factor 	= schedulers[other]->cpu_factor;
		sched->cpu_khz 		= schedulers[other]->cpu_khz;
		#endif
		sched->spin 		= schedulers[other]->spin;
		att_set_bit (&enabled_threads, sched->index);
	}

	if (Wptr != NotProcess_p) {
		sched->stats.startp++;
		K_ZERO_OUT_JRET ();
	} else {
		RESCHEDULE;
	}
}
/*}}}*/
/*{{{  void kernel_Y_fastscheduler (void)*/
/*
 *	fast scheduler entry point -- doesn't enqueue us, just runs next process
 *
 *	@SYMBOL:	Y_fastscheduler
 *	@TYPE:		JUMP
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_FASTSCHEDULER
 *	@PRIO:		50
 */
void kernel_Y_fastscheduler (void)
{
	K_SETGLABEL_ZERO_IN (Y_fastscheduler);
	ENTRY_TRACE0 (Y_fastscheduler);

	Wptr = get_process_or_reschedule (sched); 

	K_ZERO_OUT_JRET ();
}
/*}}}*/
/*{{{  void kernel_X_occscheduler (void)*/
/*
 *	@SYMBOL:	X_occscheduler
 *	@TYPE:		JUMP
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_OCCSCHEDULER
 *	@PRIO:		50
 */
void kernel_X_occscheduler (void)
{
	K_SETGLABEL_ZERO_IN (X_occscheduler);
	RESCHEDULE;
}
/*}}}*/
/*{{{  void kernel_X_scheduler (void)*/
/*
 *	scheduler proper
 *
 *	This routine cannot be "call'ed" because it doesn't deal with the stack, so
 *	it should never be called from C directly, only from assembler using a jump.
 *	It should not be called externally anyway...
 *	FOOTNOTE: But it is jumped to (now!) from CSPlib in ProcPar().
 *	NOTE: [CSPlib disabled in this scheduler]
 */
void kernel_X_scheduler (void)
{
	sched_t *sched;
	word *Wptr;

	K_SCHEDULER (X_scheduler);
	
	//fprintf (stderr, ">> S = %p, F = %p, B = %p\n", sched, BFptr, BBptr);

	ENTRY_TRACE (X_scheduler, "sync=%d", att_val (&(sched->sync)));
	
	Wptr = NotProcess_p;
	do {
		if (att_val (&(sched->sync))) {
			unsigned int sync = att_swap (&(sched->sync), 0);

			#ifndef ENABLE_CPU_TIMERS
			if (sync & SYNC_TIME) {
				check_timer_queue (sched);
			}
			#endif	/* !ENABLE_CPU_TIMERS */
			
			while (sync & SYNC_BMAIL) {
				batch_t *batch = (batch_t *) atomic_dequeue_from_runqueue (&(sched->bmail), false);
				if (batch != NULL) {
					push_batch (sched, batch->priofinity, batch);
				} else {
					sync &= ~SYNC_BMAIL;
				}
			}
			
			while (sync & SYNC_PMAIL) {
				word *ptr = (word *) atomic_dequeue_from_runqueue (&(sched->pmail), true);
				if (ptr != NULL) {
					enqueue_process (sched, ptr);
				} else {
					sync &= ~SYNC_PMAIL;
				}
			}

			if (sync & SYNC_TQ) {
				clean_timer_queue (sched);
				check_timer_queue (sched);
			}
		}

		if (end_of_curb (sched)) {
			#ifdef ENABLE_CPU_TIMERS
			check_timer_queue (sched);
			#endif

			if (sched->curb.size > BATCH_EMPTIED && !att_val (&(sched->rqstate))) {
				word size = sched->curb.size & (~BATCH_EMPTIED);
				sched->dispatches = calculate_dispatches (size);
				sched->curb.size = size;
				Wptr = dequeue_from_curb (sched);
			} else {
				unsigned int tmp;
				batch_t *new_batch = NULL;

				if (!(empty_batch (&(sched->curb)))) {
					push_curb (sched);
				}

				while (new_batch == NULL && (tmp = att_val (&(sched->rqstate)))) {
					unsigned int rq = bsf (tmp);
					new_batch = pick_batch (sched, rq);
				}

				if (new_batch != NULL) {
					#if !defined(RMOX_BUILD)
					if (att_val (&(sched->mwstate)) && (tmp = att_val (&sleeping_threads))) {
						ccsp_wake_thread (schedulers[bsf (tmp)], SYNC_WORK_BIT);
					}
					#endif
					ASSERT ( clean_batch (new_batch) );
					load_curb (sched, new_batch, false);
					Wptr = dequeue_from_curb (sched);
				} else if ((new_batch = migrate_some_work (sched)) != NULL) {
					ASSERT ( dirty_batch (new_batch) );
					SAFETY { verify_batch_integrity (new_batch); }
					sched->loop = sched->spin;
					load_curb (sched, new_batch, true);
					Wptr = dequeue_from_curb (sched);
				} else {
					new_curb (sched);

					if ((sched->loop & 0xf) == 0) {
						clean_timer_queue (sched);
						do_laundry (sched);
						release_excess_memory (sched);
					}

					if (sched->loop > 0) {
						sched->loop--;
						idle_cpu ();
					} else {
						att_set_bit (&sleeping_threads, sched->index);
						strong_read_barrier ();
						if (sched->tq_fptr != NULL) {
							#if !defined(RMOX_BUILD) && defined(ENABLE_CPU_TIMERS)
							ccsp_safe_pause_timeout (sched);
							#elif !defined(RMOX_BUILD)
							ccsp_safe_pause (sched);
							#else
							/* spin... */
							#endif
							check_timer_queue (sched);
						}
						else if (!att_val (&(sched->sync))) {
							att_set_bit (&idle_threads, sched->index);

							#if !defined(RMOX_BUILD) && defined(BLOCKING_SYSCALLS)
							if (bsyscalls_pending () > (ccsp_external_event_is_bsc () ? (ccsp_external_event_is_ready () ? 0 : 1) : 0)) {
								ccsp_safe_pause (sched);
							}
							else
							#endif
							if (ccsp_blocked_on_external_event ()) {
								#if !defined(RMOX_BUILD)
								ccsp_safe_pause (sched);
								#endif
							} else {
								unsigned int idle;

								strong_read_barrier ();
								idle = att_val (&idle_threads) & att_val (&sleeping_threads);
								if (idle == att_val (&enabled_threads)) {
									ccsp_kernel_deadlock ();
								} else {
									#if !defined(RMOX_BUILD)
									ccsp_safe_pause (sched);
									#endif /* !defined(RMOX_BUILD) */
								}
							}

							att_clear_bit (&idle_threads, sched->index);
						} else {
							att_clear_bit (&sleeping_threads, sched->index);
						}
						#if defined(RMOX_BUILD)
						att_clear_bit (&sleeping_threads, sched->index);
						#endif /* defined(RMOX_BUILD) */
						sched->loop = sched->spin;
					}
				}
			}
		} else {
			SAFETY { verify_batch_integrity (&(sched->curb)); }
			sched->dispatches--;
			Wptr = dequeue_from_curb (sched);
		}
	} while (Wptr == NotProcess_p);

	ENTRY_TRACE (X_scheduler_dispatch, "sync=%d, raddr=0x%8.8x", att_val (&(sched->sync)), Wptr[Iptr]);
	DTRACE ("SSW", Wptr);

	//fprintf (stderr, "<< W = %p (%p), F = %p, B = %p\n", Wptr, Wptr[-1], Fptr, Bptr);
	K_ZERO_OUT_JRET ();
}
/*}}}*/
/*{{{  void kernel_Y_shutdown (void)*/
/*
 *	@SYMBOL:	Y_shutdown
 *	@TYPE:		JUMP
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_SHUTDOWN
 *	@PRIO:		10
 */
void kernel_Y_shutdown (void)
{
	K_SETGLABEL_ZERO_IN (Y_shutdown);
	ENTRY_TRACE (Y_shutdown, "");
	att_set (&(ccsp_shutdown), true);
	RESCHEDULE;
}
/*}}}*/
/*}}}*/

/*{{{ channels */
#define CIO_INPUT 	0x01
#define CIO_OUTPUT	0x02
#define CIO_EXCHANGE 	(CIO_INPUT | CIO_OUTPUT)
#define CIO_EXTENDED	0x10
#define CIO_MOBILE	0x20
/*{{{  static INLINE void kernel_chan_io_static (word flags, byte *src, byte *dst, unsigned int count)*/
static INLINE void kernel_chan_io_static (word flags, byte *src, byte *dst, unsigned int count)
{
	switch (count) {
		case 1:
			*((byte *)dst) = *((byte *)src);
			break;
		case (sizeof(word)):
			*((word *)dst) = *((word *)src);
			break;
		case (sizeof(long long)):
			*((long long *)dst) = *((long long *)src);
			break;
		default:
			if (count)
				xmemcpy (src, dst, count);
			break;
	}
}
/*}}}*/
/*{{{  static INLINE void kernel_chan_io_mobile (sched_t *sched, word flags, word *src, word *dst)*/
static INLINE void kernel_chan_io_mobile (sched_t *sched, word flags, word *src, word *dst)
{
	if ((flags & CIO_EXCHANGE) == CIO_EXCHANGE) {
		word temp = *dst;
		*(dst) = *src;
		*(src) = temp;
	} else {
		word *ptr = (word *) *src;

		if (ptr != NULL) {
			if (mt_io_update (sched, &ptr)) {
				*src = (word) NULL;
			}
		}

		*dst = (word) ptr;
	}
}
/*}}}*/
/*{{{  static INLINE void kernel_chan_io (...)*/
/*
 *	channel input and output
 */
static INLINE void kernel_chan_io (word flags, word *Wptr, sched_t *sched, word *channel_address, byte *pointer, unsigned int count)
{
	byte *destination_address, *source_address;
	word temp;

	temp = atw_val (channel_address);

	if (temp == NotProcess_p || (temp & 1)) {
		Wptr[Pointer] = (word) pointer;
		save_priofinity (sched, Wptr);
		weak_write_barrier ();

		temp = atw_swap (channel_address, (word) Wptr);
		if (temp == NotProcess_p) {
			RESCHEDULE;
			return;
		} else if (temp & 1) {
			trigger_alt_guard (sched, temp);
			RESCHEDULE;
			return;
		}
	}
	
	if (!(flags & CIO_EXTENDED)) {
		atw_set (channel_address, NotProcess_p);
	}

	if (flags & CIO_INPUT) {
		destination_address = pointer;
		source_address = (byte *)(((word *)temp)[Pointer]);
	} else {
		destination_address = (byte *)(((word *)temp)[Pointer]);
		source_address = pointer;
	}

	if (flags & CIO_MOBILE) {
		kernel_chan_io_mobile (
			sched,
			flags,
			(word *) source_address,
			(word *) destination_address
		);
	} else {
		kernel_chan_io_static (flags, source_address, destination_address, count);
	}
	
	if (!(flags & CIO_EXTENDED)) {
		weak_write_barrier ();
		Wptr = reschedule_point (sched, Wptr, (word *) temp);
	}

	K_ZERO_OUT_JRET ();
}
#define BUILD_CHANNEL_IO(symbol,count,flags) \
void kernel_##symbol (void) 		\
{					\
	word *channel_address;		\
	byte *pointer;			\
					\
	K_SETGLABEL_TWO_IN_SR (symbol, channel_address, pointer); \
	kernel_chan_io ((flags), Wptr, sched, channel_address, pointer, count); \
}

#define BUILD_CHANNEL_COUNTED_IO(symbol,shift,flags) \
void kernel_##symbol (void)		\
{					\
	word count, *channel_address;	\
	byte *pointer;			\
					\
	K_SETGLABEL_THREE_IN_SR (symbol, count, channel_address, pointer); \
	if ((shift)) {			\
		count <<= (shift);	\
	}				\
	kernel_chan_io ((flags), Wptr, sched, channel_address, pointer, count); \
}
/*}}}*/
/*{{{  Y_in8 */
/*
 *	@SYMBOL:	Y_in8
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_IN8
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_in8, 1, CIO_INPUT)
/*}}}*/
/*{{{  Y_in32 */
/*
 *	@SYMBOL:	Y_in32
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_IN32
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_in32, 4, CIO_INPUT)
/*}}}*/
/*{{{  Y_out8 */
/*
 *	@SYMBOL:	Y_out8
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_OUT8
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_out8, 1, CIO_OUTPUT)
/*}}}*/
/*{{{  Y_out32 */
/*
 *	@SYMBOL:	Y_out32
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_OUT32
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_out32, 4, CIO_OUTPUT)
/*}}}*/
/*{{{  Y_in */
/*
 *	@SYMBOL:	Y_in
 *	@TYPE:		SR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_IN
 *	@PRIO:		110
 */
BUILD_CHANNEL_COUNTED_IO (Y_in, 0, CIO_INPUT)
/*}}}*/
/*{{{  Y_out */
/*
 *	@SYMBOL:	Y_out
 *	@TYPE:		SR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_OUT
 *	@PRIO:		110
 */
BUILD_CHANNEL_COUNTED_IO (Y_out, 0, CIO_OUTPUT)
/*}}}*/
/*{{{  void kernel_Y_outbyte (void)*/
/*
 *	byte channel output
 *
 *	@SYMBOL:	Y_outbyte
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_OUTBYTE
 *	@PRIO:		100
 */
void kernel_Y_outbyte (void)
{
	word *channel_address;
	byte *pointer;
	word value;

	K_SETGLABEL_TWO_IN_SR (Y_outbyte, value, channel_address);

	pointer		= (byte *) Wptr;
	*pointer	= (byte) value;
	
	kernel_chan_io (CIO_OUTPUT, Wptr, sched, channel_address, pointer, 1);
}
/*}}}*/
/*{{{  void kernel_Y_outword (void)*/
/*
 *	word channel output
 *
 *	@SYMBOL:	Y_outword
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_OUTWORD
 *	@PRIO:		100
 */
void kernel_Y_outword (void)
{
	word *channel_address;
	byte *pointer;
	word value;
	
	K_SETGLABEL_TWO_IN_SR (Y_outword, value, channel_address);
	
	Wptr[0]	= value;
	pointer	= (byte *) Wptr;
	
	kernel_chan_io (CIO_OUTPUT, Wptr, sched, channel_address, pointer, sizeof (word));
}
/*}}}*/
/*{{{  void kernel_X_xable (void)*/
/*
 *	called to synchronise with outputting process
 *	also works with an inputting process in the case where output ALTs are enabled
 *
 *	@SYMBOL:	X_xable
 *	@TYPE:		SR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_XABLE
 *	@PRIO:		70
 */
void kernel_X_xable (void)
{
	word *channel_address, temp;
	
	K_SETGLABEL_ONE_IN_SR (X_xable, channel_address);
	ENTRY_TRACE (X_xable, "%p", channel_address);

	temp = atw_val (channel_address);

	if (temp == NotProcess_p || (temp & 1)) {
		atw_set (&(Wptr[State]), ALT_WAITING | 1);
		save_priofinity (sched, Wptr);
		weak_write_barrier ();

		temp = atw_swap (channel_address, ((word) Wptr) | 1);
		if (temp == NotProcess_p) {
			RESCHEDULE;
			return;
		} else if (temp & 1) {
			trigger_alt_guard (sched, temp);
			RESCHEDULE;
			return;
		}

		atw_set (channel_address, temp);
	}

	K_ZERO_OUT_JRET ();
}
/*}}}*/
/*{{{  void kernel_X_xend (void)*/
/*
 *	called to resume outputting process
 *	(or inputting process if output ALTs are enabled)
 *
 *	@SYMBOL:	X_xend
 *	@TYPE:		RR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_XEND
 *	@PRIO:		70
 */
void kernel_X_xend (void)
{
	word *channel_address, *ptr;

	K_SETGLABEL_ONE_IN_RR (X_xend, channel_address);

	ENTRY_TRACE (X_xend, "%p (process = %p)", channel_address, (word *)*channel_address);

	ptr = (word *) atw_val (channel_address);
	atw_set (channel_address, (word) NotProcess_p);
	weak_write_barrier ();

	enqueue_process (sched, ptr);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  Y_xin */
/*
 *	@SYMBOL:	Y_xin
 *	@TYPE:		SR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_XIN
 *	@PRIO:		90
 */
BUILD_CHANNEL_COUNTED_IO (Y_xin, 0, CIO_EXTENDED | CIO_INPUT)
/*}}}*/
/*{{{  Y_mt_in */
/*
 *	@SYMBOL:	Y_mt_in
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MT_IN
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_mt_in, 0, CIO_MOBILE | CIO_INPUT)
/*}}}*/
/*{{{  Y_mt_out */
/*
 *	@SYMBOL:	Y_mt_out
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MT_OUT
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_mt_out, 0, CIO_MOBILE | CIO_OUTPUT)
/*}}}*/
/*{{{  Y_mt_xchg */
/*
 *	@SYMBOL:	Y_mt_xchg
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MT_XCHG
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_mt_xchg, 0, CIO_MOBILE | CIO_EXCHANGE)
/*}}}*/
/*{{{  Y_mt_xin */
/*
 *	@SYMBOL:	Y_mt_xin
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MT_XIN
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_mt_xin, 0, CIO_EXTENDED | CIO_MOBILE | CIO_INPUT)
/*}}}*/
/*{{{  Y_mt_xout */
/*
 *	@SYMBOL:	Y_mt_xout
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MT_XOUT
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_mt_xout, 0, CIO_EXTENDED | CIO_MOBILE | CIO_OUTPUT)
/*}}}*/
/*{{{  Y_mt_xxchg */
/*
 *	@SYMBOL:	Y_mt_xxchg
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MT_XXCHG
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_mt_xxchg, 0, CIO_EXTENDED | CIO_MOBILE | CIO_EXCHANGE)
/*}}}*/
/*}}}*/
/*{{{  timers */
/*{{{  void kernel_X_ldtimer (void)*/
/*
 *	load timer
 *
 *	@SYMBOL:	X_ldtimer
 *	@TYPE:		LCR
 *	@INPUT:		NONE
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_LDTIMER
 *	@PRIO:		90
 */
void kernel_X_ldtimer (void)
{
	Time now;
	
	K_SETGLABEL_ZERO_IN_LCR (X_ldtimer);
	ENTRY_TRACE0 (X_ldtimer);

	now = Time_GetTime(sched);

	K_ONE_OUT (now);
}
/*}}}*/
/*{{{  void kernel_X_tin (void)*/
/*
 *	timer input (delay)
 *
 *	@SYMBOL:	X_tin
 *	@TYPE:		SR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_TIN
 *	@PRIO:		80
 */
void kernel_X_tin (void)
{
	Time now, wait_time;
	
	K_SETGLABEL_ONE_IN_SR (X_tin, wait_time);
	ENTRY_TRACE (X_tin, "%d", wait_time);

	now = Time_GetTime(sched);

	if (!Time_AFTER (now, wait_time)) {
		save_priofinity (sched, Wptr);
		wait_time++; /* from T9000 book... */
		SetTimeField(Wptr, wait_time);
		add_to_timer_queue (sched, Wptr, wait_time, false);
		RESCHEDULE;
		return;
	} else {
		K_ZERO_OUT_JRET ();
		return;
	}
}
/*}}}*/
/*{{{  void kernel_Y_fasttin (void)*/
/*
 *	fast timer input
 *
 *	@SYMBOL:	Y_fasttin
 *	@TYPE:		SR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_FASTTIN
 *	@PRIO:		80
 */
void kernel_Y_fasttin (void)
{
	Time wait_time;

	K_SETGLABEL_ONE_IN_SR (Y_fasttin, wait_time);
	ENTRY_TRACE (Y_fasttin, "%d", wait_time);

	save_priofinity (sched, Wptr);
	add_to_timer_queue (sched, Wptr, wait_time, false);

	RESCHEDULE;
}
/*}}}*/
/*}}}*/
/*{{{  ALTing */
/*{{{  void kernel_Y_alt (void)*/
/*
 *	ALT start
 *
 *	@SYMBOL:	Y_alt
 *	@TYPE:		RR
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_ALT
 *	@PRIO:		70
 */
void kernel_Y_alt (void)
{
	K_SETGLABEL_ZERO_IN_RR (Y_alt);
	ENTRY_TRACE0 (Y_alt);

	atw_set (&(Wptr[State]), ALT_ENABLING | ALT_NOT_READY | 1);
	weak_write_barrier ();

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_talt (void)*/
/*
 *	timer ALT start
 *
 *	@SYMBOL:	Y_talt
 *	@TYPE:		RR
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_TALT
 *	@PRIO:		70
 */
void kernel_Y_talt (void)
{
	K_SETGLABEL_ZERO_IN_RR (Y_talt);
	ENTRY_TRACE0 (Y_talt);

	atw_set (&(Wptr[State]), ALT_ENABLING | ALT_NOT_READY | 1);
	atw_set (&(Wptr[TLink]), TimeNotSet_p);
	weak_write_barrier ();

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  static INLINE void kernel_altend (word *Wptr, sched_t *sched, unsigned int return_address, bool jump)*/
static INLINE void kernel_altend (word *Wptr, sched_t *sched, unsigned int return_address, bool jump)
{
	if (jump) {
		return_address += Wptr[Temp];
	}

	save_priofinity (sched, Wptr);
	atw_set (&(Wptr[Iptr]), (word) return_address);
	weak_write_barrier ();

	if (atw_dec_z (&(Wptr[State]))) {
		K_ZERO_OUT ();
		return;
	} else {
		RESCHEDULE;
		return;
	}
}
/*}}}*/
/*{{{  void kernel_Y_altend (void)*/
/*
 *	ALT end
 *
 *	@SYMBOL:	Y_altend
 *	@TYPE:		RR
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_ALTEND
 *	@PRIO:		70
 */
void kernel_Y_altend (void)
{
	K_SETGLABEL_ZERO_IN_RR (Y_altend);
	ENTRY_TRACE0 (Y_altend);

	kernel_altend (Wptr, sched, return_address, true);
}
/*}}}*/
/*{{{  void kernel_Y_caltend (void)*/
/*
 *	CIF ALT end
 *
 *	@SYMBOL:	Y_caltend
 *	@TYPE:		RR
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_CALTEND
 *	@PRIO:		70
 */
void kernel_Y_caltend (void)
{
	K_SETGLABEL_ZERO_IN_RR (Y_caltend);
	ENTRY_TRACE0 (Y_caltend);

	kernel_altend (Wptr, sched, return_address, false);
}
/*}}}*/
/*{{{  void kernel_X_altwt (void)*/
/*
 *	ALT wait
 *
 *	@SYMBOL:	X_altwt
 *	@TYPE:		LCR
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_ALTWT
 *	@PRIO:		70
 */
void kernel_X_altwt (void)
{
	word state;

	K_SETGLABEL_ZERO_IN_LCR (X_altwt);
	ENTRY_TRACE0 (X_altwt);

	Wptr[Temp] = NoneSelected_o;

	if ((state = atw_val (&(Wptr[State]))) & ALT_NOT_READY) {
		word nstate = (state | ALT_WAITING) & (~(ALT_ENABLING | ALT_NOT_READY));
		
		save_priofinity (sched, Wptr);
		atw_set (&(Wptr[Iptr]), (word) return_address);
		weak_write_barrier ();
		
		if (atw_cas (&(Wptr[State]), state, nstate)) {
			RESCHEDULE;
			return;
		}
	}
	
	atw_clear_bit (&(Wptr[State]), ALT_ENABLING_BIT);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_taltwt (void)*/
/*
 *	timer ALT wait
 *
 *	@SYMBOL:	X_taltwt
 *	@TYPE:		LCR
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_TALTWT
 *	@PRIO:		70
 */
void kernel_X_taltwt (void)
{
	Time now;
	word state;

	K_SETGLABEL_ZERO_IN_LCR (X_taltwt);
	ENTRY_TRACE0 (X_taltwt);

	Wptr[Temp] = NoneSelected_o;
	
	now = Time_GetTime (sched);

	if ((state = atw_val (&(Wptr[State]))) & ALT_NOT_READY) {
		word nstate = (state | ALT_WAITING) & (~(ALT_ENABLING | ALT_NOT_READY));

		if (Wptr[TLink] == TimeSet_p && !Time_AFTER(GetTimeField(Wptr), now)) {
			/* already past or at timeout */
		} else {
			tqnode_t *tn = NULL;

			save_priofinity (sched, Wptr);
			atw_set (&(Wptr[Iptr]), (word) return_address);

			if (Wptr[TLink] == TimeSet_p) {
				tn = add_to_timer_queue (sched, Wptr, GetTimeField(Wptr), true);
				atw_set (&(Wptr[TLink]), (word) tn);
				nstate = nstate + 1;
			}
			
			weak_write_barrier ();
			
			if (atw_cas (&(Wptr[State]), state, nstate)) {
				RESCHEDULE;
				return;
			} else if (tn != NULL) {
				Wptr[TLink] = TimeSet_p;
				delete_tqnode (sched, tn);
				set_batch_clean ((batch_t *) tn);
				release_tqnode (sched, tn);
			}
		}
	}

	SetTimeField (Wptr, now);
	atw_and (&(Wptr[State]), ~(ALT_NOT_READY | ALT_ENABLING));

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{ static INLINE void kernel_enbc (...)*/
/*
 *	enable channel
 */
static INLINE bool kernel_enbc (word *Wptr, sched_t *sched, unsigned int return_address, word **channel_address, bool jump, bool set_address)
{
	const word ptr = (((word) Wptr) | 1);
	word temp = atw_val (channel_address);

	if (temp == NotProcess_p) {
		temp = atw_swap (channel_address, ptr);
		if (temp != NotProcess_p) {
			atw_set (channel_address, temp);
			if (jump) {
				atw_and (&(Wptr[State]), ~(ALT_NOT_READY | ALT_ENABLING));
				K_ZERO_OUT ();
			} else if (atw_val (&(Wptr[State])) & ALT_NOT_READY) {
				atw_and (&(Wptr[State]), ~(ALT_NOT_READY | ALT_ENABLING));
				if (set_address) {
					atw_set (&(Wptr[Temp]), return_address);
				}
			}
			return true;
		} else {
			atw_inc (&(Wptr[State]));
		}
	} else if (temp != ptr) {
		if (jump) {
			atw_and (&(Wptr[State]), ~(ALT_NOT_READY | ALT_ENABLING));
			K_ZERO_OUT ();
		} else if (atw_val (&(Wptr[State])) & ALT_NOT_READY) {
			atw_and (&(Wptr[State]), ~(ALT_NOT_READY | ALT_ENABLING));
			if (set_address) {
				Wptr[Temp] = return_address;
			}
		}
		return true;
	}

	return false;
}
/*}}}*/
/*{{{  void kernel_Y_enbc (void)*/
/*
 *	enable channel
 *
 *	@SYMBOL:	Y_enbc
 *	@TYPE:		LCR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_ENBC
 *	@PRIO:		80
 */
void kernel_Y_enbc (void)
{
	word **channel_address, guard;

	K_SETGLABEL_TWO_IN_LCR (Y_enbc, guard, channel_address);
	ENTRY_TRACE (Y_enbc, "%d, %p (ready = %d)", guard, channel_address, guard && (*channel_address != NotProcess_p) && (*channel_address != Wptr));

	if (!guard) {
		K_STKONE_OUT (false);
		return;
	}
	
	kernel_enbc (Wptr, sched, return_address, channel_address, false, false);

	K_STKONE_OUT (true);
}
/*}}}*/
/*{{{  void kernel_Y_enbc2 (void)*/
/*
 *	enable channel (2 param, with ready address)
 *
 *	@SYMBOL:	Y_enbc2
 *	@TYPE:		LCR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_ENBC2
 *	@PRIO:		80
 */
void kernel_Y_enbc2 (void)
{
	unsigned int process_address;
	word **channel_address;

	K_SETGLABEL_TWO_IN_LCR (Y_enbc2, process_address, channel_address);
	ENTRY_TRACE (Y_enbc2, "%p (ready = %d), %p", channel_address, (*channel_address != NotProcess_p) && (*channel_address != Wptr), (void *)process_address);

	kernel_enbc (Wptr, sched, process_address, channel_address, true, false);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_enbc3 (void)*/
/*
 *	enable channel (with ready address)
 *
 *	@SYMBOL:	Y_enbc3
 *	@TYPE:		LCR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_ENBC3
 *	@PRIO:		80
 */
void kernel_Y_enbc3 (void)
{
	unsigned int process_address;
	word **channel_address, guard;

	K_SETGLABEL_THREE_IN_LCR (Y_enbc3, process_address, guard, channel_address);
	ENTRY_TRACE (Y_enbc3, "%d, %p (ready = %d), %p", guard, channel_address, guard && (*channel_address != NotProcess_p) && (*channel_address != Wptr), (void *)process_address);

	if (!guard) {
		K_STKONE_OUT (false);
		return;
	}
	
	kernel_enbc (Wptr, sched, process_address, channel_address, true, false);

	K_STKONE_OUT (true);
}
/*}}}*/
/*{{{  void kernel_Y_cenbc (void)*/
/*
 *	CIF enable channel (2 param)
 *
 *	@SYMBOL:	Y_cenbc
 *	@TYPE:		RR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_CENBC
 *	@PRIO:		80
 */
void kernel_Y_cenbc (void)
{
	word **channel_address, id;

	K_SETGLABEL_TWO_IN_RR (Y_cenbc, id, channel_address);
	ENTRY_TRACE (Y_cenbc, "%d %p (ready = %d), %p", id, channel_address, (*channel_address != NotProcess_p) && (*channel_address != Wptr));

	K_ONE_OUT (kernel_enbc (Wptr, sched, id, channel_address, false, true));
}
/*}}}*/
/*{{{ static INLINE void kernel_enbs (...)*/
/*
 *	enable skip guard
 */
static INLINE void kernel_enbs (word *Wptr, unsigned int return_address, bool jump, bool set_address)
{
	if (jump) {
		atw_and (&(Wptr[State]), ~(ALT_NOT_READY | ALT_ENABLING));
	} else if (atw_val (&(Wptr[State])) & ALT_NOT_READY) {
		atw_and (&(Wptr[State]), ~(ALT_NOT_READY | ALT_ENABLING));
		if (set_address) {
			Wptr[Temp] = return_address;
		}
	}
}
/*}}}*/
/*{{{  void kernel_Y_enbs (void)*/
/*
 *	enable skip guard
 *
 *	@SYMBOL:	Y_enbs
 *	@TYPE:		LCR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_ENBS
 *	@PRIO:		60
 */
void kernel_Y_enbs (void)
{
	word guard;

	K_SETGLABEL_ONE_IN_LCR (Y_enbs, guard);
	ENTRY_TRACE (Y_enbs, "%d", guard);

	if (!guard) {
		K_STKONE_OUT (false);
		return;
	}
	
	kernel_enbs (Wptr, 0, false, false);

	K_STKONE_OUT (true);
}
/*}}}*/
/*{{{  void kernel_Y_enbs2 (void)*/
/*
 *	enable skip guard (1 param, with ready address)
 *
 *	@SYMBOL:	Y_enbs2
 *	@TYPE:		LCR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_ENBS2
 *	@PRIO:		60
 */
void kernel_Y_enbs2 (void)
{
	unsigned int process_address;

	K_SETGLABEL_ONE_IN_LCR (Y_enbs2, process_address);
	ENTRY_TRACE (Y_enbs2, "%p", process_address);

	kernel_enbs (Wptr, 0, true, false);
	return_address = process_address;

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_enbs3 (void)*/
/*
 *	enable skip guard (with ready address)
 *
 *	@SYMBOL:	Y_enbs3
 *	@TYPE:		LCR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_ENBS3
 *	@PRIO:		60
 */
void kernel_Y_enbs3 (void)
{
	unsigned int process_address;
	word guard;

	K_SETGLABEL_TWO_IN_LCR (Y_enbs3, process_address, guard);
	ENTRY_TRACE (Y_enbs3, "%d %p", process_address, guard);

	if (!guard) {
		K_STKONE_OUT (false);
		return;
	}

	kernel_enbs (Wptr, 0, true, false);
	return_address = process_address;

	K_STKONE_OUT (true);
}
/*}}}*/
/*{{{  void kernel_Y_cenbs (void)*/
/*
 *	CIF enable skip guard
 *
 *	@SYMBOL:	Y_cenbs
 *	@TYPE:		RR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_CENBS
 *	@PRIO:		60
 */
void kernel_Y_cenbs (void)
{
	word id;

	K_SETGLABEL_ONE_IN_RR (Y_cenbs, id);
	ENTRY_TRACE (Y_cenbs, "%d", id);

	kernel_enbs (Wptr, id, false, true);

	K_ONE_OUT (true);
}
/*}}}*/
/*{{{ static INLINE void kernel_enbt (...)*/
/*
 *	enable timer
 */
static INLINE bool kernel_enbt (word *Wptr, sched_t *sched, unsigned int return_address, Time timeout, bool jump, bool check, bool set_address)
{
	Time now = (jump || check) ? Time_GetTime (sched) : 0;

	if ((jump || check) && !Time_AFTER (timeout, now)) {
		Wptr[TLink] = TimeSet_p;
		SetTimeField (Wptr, now);
		if (jump) {
			atw_and (&(Wptr[State]), ~(ALT_NOT_READY | ALT_ENABLING));
			K_ZERO_OUT ();
		} else if (atw_val (&(Wptr[State])) & ALT_NOT_READY) {
			atw_and (&(Wptr[State]), ~(ALT_NOT_READY | ALT_ENABLING));
			if (set_address) {
				Wptr[Temp] = return_address;
			}
		}
		return true;
	} else if (Wptr[TLink] == TimeNotSet_p) {
		SetTimeField (Wptr, timeout);
		Wptr[TLink] = TimeSet_p;
	} else if (Time_AFTER (GetTimeField (Wptr), timeout)) {
		SetTimeField (Wptr, timeout);
	}

	return false;
}
/*}}}*/
/*{{{  void kernel_X_enbt (void)*/
/*
 *	enable timer
 *
 *	@SYMBOL:	X_enbt
 *	@TYPE:		LCR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_ENBT
 *	@PRIO:		70
 */
void kernel_X_enbt (void)
{
	Time timeout;
	word guard;

	K_SETGLABEL_TWO_IN_LCR (X_enbt, guard, timeout);
	ENTRY_TRACE (X_enbt, "%d, %d", guard, timeout);

	if (!guard) {
		K_STKONE_OUT (false);
		return;
	}
	
	kernel_enbt (Wptr, sched, 0, timeout, false, false, false);

	K_STKONE_OUT (true);
}
/*}}}*/
/*{{{  void kernel_Y_enbt2 (void)*/
/*
 *	enable timer (with ready address)
 *
 *	@SYMBOL:	Y_enbt2
 *	@TYPE:		LCR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_ENBT2
 *	@PRIO:		70
 */
void kernel_Y_enbt2 (void)
{
	unsigned int process_address;
	Time timeout;

	K_SETGLABEL_TWO_IN_LCR (Y_enbt2, process_address, timeout);
	ENTRY_TRACE (Y_enbt2, "%d, %p", timeout, (void *)process_address);

	kernel_enbt (Wptr, sched, process_address, timeout, true, false, false);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_enbt3 (void)*/
/*
 *	enable timer (with ready address)
 *
 *	@SYMBOL:	Y_enbt3
 *	@TYPE:		LCR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_ENBT3
 *	@PRIO:		70
 */
void kernel_Y_enbt3 (void)
{
	unsigned int process_address;
	Time timeout;
	word guard;

	K_SETGLABEL_THREE_IN_LCR (Y_enbt3, process_address, guard, timeout);
	ENTRY_TRACE (Y_enbt3, "%d, %d, %p", guard, timeout, (void *)process_address);

	if (!guard) {
		K_STKONE_OUT (false);
		return;
	}
	
	kernel_enbt (Wptr, sched, process_address, timeout, true, false, false);

	K_STKONE_OUT (true);
}
/*}}}*/
/*{{{  void kernel_Y_cenbt (void)*/
/*
 *	CIF enable timer
 *
 *	@SYMBOL:	Y_cenbt
 *	@TYPE:		RR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_CENBT
 *	@PRIO:		70
 */
void kernel_Y_cenbt (void)
{
	Time id, timeout;

	K_SETGLABEL_TWO_IN_RR (Y_cenbt, id, timeout);
	ENTRY_TRACE (Y_cenbt, "%d %d", id, timeout);

	K_ONE_OUT (kernel_enbt (Wptr, sched, id, timeout, false, true, true));
}
/*}}}*/
/*{{{ static INLINE void kernel_disc (...)*/
/*
 *	disable channel
 */
static INLINE word kernel_disc (word *Wptr, unsigned int process_address, word **channel_address, bool set_jump)
{
	word temp = atw_val (channel_address);

	if (temp == (((word) Wptr) | 1)) {
		if (atw_cas (channel_address, temp, NotProcess_p)) {
			atw_dec (&(Wptr[State]));
			return false;
		}
	} else if (temp == NotProcess_p) {
		return false;
	}
	
	if (set_jump) {
		Wptr[Temp] = process_address;
	}

	return true;
}
/*}}}*/
/*{{{  void kernel_Y_disc (void)*/
/*
 *	disable channel
 *
 *	@SYMBOL:	Y_disc
 *	@TYPE:		LCR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_DISC
 *	@PRIO:		80
 */
void kernel_Y_disc (void)
{
	unsigned int process_address;
	word **channel_address, guard;

	K_SETGLABEL_THREE_IN_LCR (Y_disc, process_address, guard, channel_address);
	ENTRY_TRACE (Y_disc, "%p, %d, %p", (void *)process_address, guard, channel_address);

	if (!guard) {
		K_STKONE_OUT (false);
		return;
	}

	K_STKONE_OUT (kernel_disc (Wptr, process_address, channel_address, (Wptr[Temp] == NoneSelected_o)));
}
/*}}}*/
/*{{{  void kernel_Y_cdisc (void)*/
/*
 *	CIF disable channel
 *
 *	@SYMBOL:	Y_cdisc
 *	@TYPE:		RR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_CDISC
 *	@PRIO:		80
 */
void kernel_Y_cdisc (void)
{
	word **channel_address, id;

	K_SETGLABEL_TWO_IN_RR (Y_cdisc, id, channel_address);
	ENTRY_TRACE (Y_cdisc, "%d %p", id, channel_address);

	K_ONE_OUT (kernel_disc (Wptr, id, channel_address, (Wptr[Temp] == NoneSelected_o)));
}
/*}}}*/
/*{{{  void kernel_Y_ndisc (void)*/
/*
 *	disable channel
 *
 *	@SYMBOL:	Y_ndisc
 *	@TYPE:		LCR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_NDISC
 *	@PRIO:		80
 */
void kernel_Y_ndisc (void)
{
	unsigned int process_address;
	word **channel_address, guard;

	K_SETGLABEL_THREE_IN_LCR (Y_ndisc, process_address, guard, channel_address);
	ENTRY_TRACE (Y_ndisc, "%p, %d, %p", (void *)process_address, guard, channel_address);

	if (!guard) {
		K_STKONE_OUT (false);
		return;
	}

	K_STKONE_OUT (kernel_disc (Wptr, process_address, channel_address, true));
}
/*}}}*/
/*{{{  void kernel_X_diss (void)*/
/*
 *	disable SKIP guard
 *
 *	@SYMBOL:	X_diss
 *	@TYPE:		LCR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_DISS
 *	@PRIO:		60
 */
void kernel_X_diss (void)
{
	unsigned int process_address;
	word fired, guard;

	K_SETGLABEL_TWO_IN_LCR (X_diss, process_address, guard);
	ENTRY_TRACE (X_diss, "%p, %d", (void *)process_address, guard);

	if ((fired = guard)) {
		if (Wptr[Temp] == NoneSelected_o) {
			Wptr[Temp] = process_address;
		} else {
			fired = false;
		}
	}

	K_STKONE_OUT (fired);
}
/*}}}*/
/*{{{  void kernel_Y_cdiss (void)*/
/*
 *	CIF disable SKIP guard
 *
 *	@SYMBOL:	Y_cdiss
 *	@TYPE:		RR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_CDISS
 *	@PRIO:		60
 */
void kernel_Y_cdiss (void)
{
	word id;

	K_SETGLABEL_ONE_IN_RR (Y_cdiss, id);
	ENTRY_TRACE (Y_cdiss, "%d", id);

	if (Wptr[Temp] == NoneSelected_o) {
		Wptr[Temp] = id;
		K_ONE_OUT (true);
	}

	K_ONE_OUT (false);
}
/*}}}*/
/*{{{  void kernel_X_ndiss (void)*/
/*
 *	disables SKIP guard
 *
 *	@SYMBOL:	X_ndiss
 *	@TYPE:		LCR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_NDISS
 *	@PRIO:		60
 */
void kernel_X_ndiss (void)
{
	unsigned int fired, guard, process_address;

	K_SETGLABEL_TWO_IN_LCR (X_ndiss, process_address, guard);
	ENTRY_TRACE (X_ndiss, "%p, %d", (void *)process_address, guard);

	if ((fired = guard)) {
		Wptr[Temp] = process_address;
	}
	
	K_STKONE_OUT (fired);
}
/*}}}*/
/*{{{ static INLINE void kernel_dist (...)*/
/*
 *	disable timer
 */
static INLINE word kernel_dist (word *Wptr, sched_t *sched, unsigned int process_address, Time timeout, bool set_jump)
{
	word tlink = Wptr[TLink];
	
	if (tlink == TimeSet_p) {
		if (!Time_AFTER (timeout, GetTimeField (Wptr))) {
			if (set_jump)
				Wptr[Temp] = (word) process_address;
			return true;
		}
	} else if (tlink != TimeNotSet_p) {
		tqnode_t *tn = (tqnode_t *) tlink;
		bool fired;
		
		Wptr[TLink] = TimeNotSet_p;
		
		if ((fired = remove_from_timer_queue (sched, tn, Wptr))) {
			Time now = tn->time;
			
			SetTimeField (Wptr, now);

			if (Time_AFTER (timeout, now)) {
				Wptr[TLink] = TimeSet_p;
				fired = false;
			} else if (set_jump) {
				Wptr[Temp] = (word) process_address;
			}

			return fired;
		} else {
			atw_dec (&(Wptr[State]));
		}

		release_tqnode (sched, tn);
	}

	return false;
}
/*}}}*/
/*{{{  void kernel_X_dist (void)*/
/*
 *	disable timer
 *
 *	@SYMBOL:	X_dist
 *	@TYPE:		LCR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_DIST
 *	@PRIO:		70
 */
void kernel_X_dist (void)
{
	unsigned int process_address;
	Time timeout;
	word guard;
	
	K_SETGLABEL_THREE_IN_LCR (X_dist, process_address, guard, timeout);
	ENTRY_TRACE (X_dist, "%d, %d, %d", process_address, guard, timeout);

	if (!guard) {
		K_STKONE_OUT (false);
		return;
	}
	
	K_STKONE_OUT (kernel_dist (Wptr, sched, process_address, timeout, (Wptr[Temp] == NoneSelected_o)));
}
/*}}}*/
/*{{{  void kernel_Y_cdist (void)*/
/*
 *	CIF disable timer
 *
 *	@SYMBOL:	Y_cdist
 *	@TYPE:		RR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	REG(1)
 *	@CALL: 		K_CDIST
 *	@PRIO:		70
 */
void kernel_Y_cdist (void)
{
	Time id, timeout;
	
	K_SETGLABEL_TWO_IN_RR (Y_cdist, id, timeout);
	ENTRY_TRACE (Y_cdist, "%d, %d", id, timeout);

	K_ONE_OUT (kernel_dist (Wptr, sched, id, timeout, (Wptr[Temp] == NoneSelected_o)));
}
/*}}}*/
/*{{{  void kernel_X_ndist (void)*/
/*
 *	disable timer
 *
 *	@SYMBOL:	X_ndist
 *	@TYPE:		LCR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_NDIST
 *	@PRIO:		70
 */
void kernel_X_ndist (void)
{
	unsigned int process_address;
	Time timeout;
	word guard;

	K_SETGLABEL_THREE_IN_LCR (X_ndist, process_address, guard, timeout);
	ENTRY_TRACE (X_ndist, "%d, %d, %d", process_address, guard, timeout);

	if (!guard) {
		K_STKONE_OUT (false);
		return;
	}
	
	K_STKONE_OUT (kernel_dist (Wptr, sched, process_address, timeout, true));
}
/*}}}*/
/*}}}*/
/*{{{  semaphores */
/*{{{  void kernel_Y_sem_claim (void)*/
/*
 *	@SYMBOL:	Y_sem_claim
 *	@TYPE:		SR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_SEM_CLAIM
 *	@PRIO:		90
 */
void kernel_Y_sem_claim (void)
{
	ccsp_sem_t *sem;
	
	K_SETGLABEL_ONE_IN_SR (Y_sem_claim, sem);
	ENTRY_TRACE (Y_sem_claim, "%p", sem);

	sem_claim (sched, Wptr, sem);
}
/*}}}*/
/*{{{  void kernel_Y_sem_release (void)*/
/*
 *	@SYMBOL:	Y_sem_release
 *	@TYPE:		RR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_SEM_RELEASE
 *	@PRIO:		90
 */
void kernel_Y_sem_release (void)
{
	ccsp_sem_t *sem;
	
	K_SETGLABEL_ONE_IN_RR (Y_sem_release, sem);
	ENTRY_TRACE (Y_sem_release, "%p", sem);

	sem_release (sched, sem);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_sem_init (void)*/
/*
 *	@SYMBOL:	Y_sem_init
 *	@TYPE:		RR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_SEM_INIT
 *	@PRIO:		70
 */
void kernel_Y_sem_init (void)
{
	ccsp_sem_t *sem;
	
	K_SETGLABEL_ONE_IN_RR (Y_sem_init, sem);
	ENTRY_TRACE (Y_sem_init, "%p", sem);

	sem_init (sem);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mt_lock (void)*/
/*
 *	lock a mobile type
 *
 *	@SYMBOL:	Y_mt_lock
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MT_LOCK
 *	@PRIO:		90
 */
void kernel_Y_mt_lock (void)
{
	mt_cb_shared_internal_t *cb;
	word *ptr, type;
	
	K_SETGLABEL_TWO_IN_SR (Y_mt_lock, type, ptr);
	ENTRY_TRACE (Y_mt_lock, "%p, %08x", type, ptr);

	cb = (mt_cb_shared_internal_t *) (ptr - MT_CB_SHARED_PTR_OFFSET);

	ASSERT ( MT_TYPE(cb->type) == MT_CB );
	ASSERT ( cb->type & MT_CB_SHARED );
	ASSERT ( type == MT_CB_CLIENT || type == MT_CB_SERVER );

	sem_claim (sched, Wptr, &(cb->sem[type]));
}
/*}}}*/
/*{{{  void kernel_Y_mt_unlock (void)*/
/*
 *	unlock a mobile type
 *
 *	@SYMBOL:	Y_mt_unlock
 *	@TYPE:		RR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MT_UNLOCK
 *	@PRIO:		90
 */
void kernel_Y_mt_unlock (void)
{
	mt_cb_shared_internal_t *cb;
	word *ptr, type;
	
	K_SETGLABEL_TWO_IN_RR (Y_mt_unlock, type, ptr);
	ENTRY_TRACE (Y_mt_unlock, "%p", type, ptr);

	cb = (mt_cb_shared_internal_t *) (ptr - MT_CB_SHARED_PTR_OFFSET);
	
	ASSERT ( MT_TYPE(cb->type) == MT_CB );
	ASSERT ( cb->type & MT_CB_SHARED );
	ASSERT ( type == MT_CB_CLIENT || type == MT_CB_SERVER );

	sem_release (sched, &(cb->sem[type]));

	K_ZERO_OUT ();
}
/*}}}*/
/*}}}*/
/*{{{  barriers */
/*{{{  void kernel_Y_mt_sync (void)*/
/*
 *	@SYMBOL:	Y_mt_sync
 *	@TYPE:		SR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MT_SYNC
 *	@PRIO:		90
 */
void kernel_Y_mt_sync (void)
{
	ccsp_barrier_t *bar;
	
	K_SETGLABEL_ONE_IN_SR (Y_mt_sync, bar);
	ENTRY_TRACE (Y_mt_sync, "%p", bar);
	
	bar->sync (sched, &(bar->data), Wptr);
}
/*}}}*/
/*{{{  void kernel_Y_mt_resign (void)*/
/*
 *	@SYMBOL:	Y_mt_resign
 *	@TYPE:		RR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MT_RESIGN
 *	@PRIO:		80
 */
void kernel_Y_mt_resign (void)
{
	ccsp_barrier_t *bar;
	word count;
	
	K_SETGLABEL_TWO_IN_RR (Y_mt_resign, count, bar);
	ENTRY_TRACE (Y_mt_resign, "%d %p", count, bar);

	bar->resign (sched, &(bar->data), count);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mt_enroll (void)*/
/*
 *	@SYMBOL:	Y_mt_enroll
 *	@TYPE:		RR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MT_ENROLL
 *	@PRIO:		80
 */
void kernel_Y_mt_enroll (void)
{
	ccsp_barrier_t *bar;
	word count;
	
	K_SETGLABEL_TWO_IN_RR (Y_mt_enroll, count, bar);
	ENTRY_TRACE (Y_mt_enroll, "%d %p", count, bar);

	bar->enroll (sched, &(bar->data), count);

	K_ZERO_OUT ();
}
/*}}}*/
/*}}}*/

/*{{{  RMOX support */
#if defined(RMOX_BUILD)
/*{{{  int ccsp_next_timeout (int *us, int *valid)*/
/*
 * 	provide a clean access for code to check when the head of the timer 
 * 	queue is due to run.  if the timer queue is valid then 'valid' is
 * 	set none zero and 'us' is appropriately.  RMoX uses this to decide
 * 	whether to halt the processor when idle.
 */
void ccsp_next_timeout (int *us, int *valid)
{
	sched_t *sched = _local_scheduler;
	if (sched->tq_fptr != NULL) {
		*us = sched->tq_fptr->time;
		*valid = 1;
	} else {
		*valid = 0;
	}
}
/*}}}*/
/*{{{  void ccsp_interrupt_handler (int irq) */
/*
 *	called by a lower level (asynchronously) when an interrupt is received
 *	according to the relevant docs, interrupts are disabled here
 *	returns the type of treatment the interrupt should receive from the
 *	interrupt handler (e.g. acknowledge, mask, etc)
 */
void ccsp_interrupt_handler (int irq)
{
	if (inttab[irq] != NotProcess_p) {
		word *Wptr = inttab[irq];
		mail_process (PAffinity (Wptr[Priofinity]), Wptr);
		inttab[irq] = NotProcess_p;
	} else {
		intcount[irq]++;
	}
}
/*}}}*/
/*{{{  void kernel_Y_wait_int (void)*/
/*
 *	entry-point for an RMoX process to wait for an interrupt
 *
 *	@SYMBOL:	Y_wait_int
 *	@TYPE:		SR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_WAIT_INT
 *	@PRIO:		50
 *	@DEPEND:	RMOX_BUILD
 */
void kernel_Y_wait_int (void)
{
	word number, mask;

	K_SETGLABEL_TWO_IN_SR (Y_wait_int, number, mask);

	ENTRY_TRACE (Y_wait_int, "0x%8.8X", number);

	if (inttab[number]) {
		MESSAGE ("scheduler: ieee, someone already waiting for this interrupt!\n");
		/* blind reschedule */
		RESCHEDULE;
	}
	
	cli ();

	if (!intcount[number]) {
		/* no interrupt yet */
		save_priofinity (sched, Wptr);
		Wptr[Temp] = 0;
		inttab[number] = Wptr;
		sti ();
		
		RESCHEDULE;
	} else {
		Wptr[Temp] = intcount[number];
		intcount[number] = 0;
		sti ();

		K_ZERO_OUT_JRET ();
	}
}
/*}}}*/
#endif	/* defined(RMOX_BUILD) */
/*}}}*/
/*{{{  dynamic/mobile-processes */
#if !defined(RMOX_BUILD) && defined(DYNAMIC_PROCS)
extern void X_dynproc_exit (void);
/*{{{  void kernel_X_kernel_run (void)*/
/*
 *	dynamic kernel run entry point (and resume point)
 *
 *	@SYMBOL:	X_kernel_run
 *	@TYPE:		LCR
 *	@INPUT:		STACK(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_KERNEL_RUN
 *	@PRIO:		50
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
void kernel_X_kernel_run (void)
{
	unsigned int kr_param;
	d_process *kr_dptr;
	
	K_SETGLABEL_STKONE_IN_LCR (X_kernel_run, kr_param);
	ENTRY_TRACE (X_kernel_run, "%p", (void *)kr_param);

	kr_dptr = dynproc_startprocess ((int *)kr_param, (void *)X_dynproc_exit);
	kr_dptr->holding_wptr 		= Wptr;
	kr_dptr->holding_raddr 		= return_address;
	kr_dptr->holding_priofinity 	= sched->priofinity;

	if (kr_dptr->suspended) {
		switch_priofinity (sched, kr_dptr->suspended->priofinity);
		*(kr_dptr->suspended->result) 	= DPROCESS_RESUMED;
		Wptr 				= kr_dptr->suspended->wptr;
		return_address 			= kr_dptr->suspended->return_addr;
	} else {
		#ifdef DEBUG_DYNPROC
		MESSAGE ("X_kernel_run: running process [%p] at %p (Fptr = %p, Bptr = %p), &Fptr = %p, func = %p\n", kr_dptr, kr_dptr->ws_ptr, Fptr, Bptr, &Fptr, kr_dptr->entrypoint);
		MESSAGE ("X_kernel_run: ->holding_wptr = 0x%8.8x,  ->holding_raddr = 0x%8.8x\n", kr_dptr->holding_wptr, kr_dptr->holding_raddr);
		#endif
		
		Wptr 			= kr_dptr->ws_ptr;
		Wptr[IptrSucc]		= (word) X_dynproc_exit;
		return_address 		= (word) kr_dptr->entrypoint;
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_dynproc_suspend (void)*/
/*
 *	entered when a dynamic process is suspending
 *
 *	@SYMBOL:	X_dynproc_suspend
 *	@TYPE:		LCR
 *	@INPUT:		STACK(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_DYNPROC_SUSPEND
 *	@PRIO:		0
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
void kernel_X_dynproc_suspend (void)
{
	word *ds_param;
	
	K_SETGLABEL_STKONE_IN_LCR (X_dynproc_suspend, ds_param);
	ENTRY_TRACE (X_dynproc_suspend, "%p", ds_param);

	/* ds_param should point at the argument-set (VAL DPROCESS p, INT result) */
	if (dynproc_suspendprocess ((d_process *)(ds_param[0]), (int *)(ds_param[1]), Wptr, return_address, sched->priofinity)) {
		/* failed */
		K_ZERO_OUT ();
	} else {
		RESCHEDULE;
	}
}
/*}}}*/
/*{{{  void kernel_X_dynproc_exit (void)*/
/*
 *	entered when dynamic process finishes
 *
 *	@SYMBOL:	X_dynproc_exit
 *	@TYPE:		JUMP
 *	@INPUT:		NONE
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_DYNPROC_EXIT
 *	@PRIO:		0
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
void kernel_X_dynproc_exit (void)
{
	unsigned int return_address;
	d_process *kr_dptr;
	word *kr_wptr;
	
	K_SETGLABEL_ZERO_IN (X_dynproc_exit);
	ENTRY_TRACE0 (X_dynproc_exit);

	kr_wptr 		= (word *) (((word) Wptr) - (4 * sizeof(word)));
	kr_dptr 		= dynproc_endprocess (kr_wptr);
	*(kr_dptr->result) 	= DPROCESS_FINISHED;
	Wptr 			= kr_dptr->holding_wptr;
	return_address 		= kr_dptr->holding_raddr;
	switch_priofinity (sched, kr_dptr->holding_priofinity);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_ldwsmap (void)*/
/*
 *	load workspace-map
 *
 *	@SYMBOL:	Y_ldwsmap
 *	@TYPE:		LCR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_LDWSMAP
 *	@PRIO:		0
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
void kernel_Y_ldwsmap (void)
{
	unsigned int code_offset, process_address;
	
	K_SETGLABEL_TWO_IN_LCR (Y_ldwsmap, process_address, code_offset);

	mpcb_add_wsmap ((mp_ctrlblk *)process_address, (unsigned char *)code_offset, Wptr);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_ulwsmap (void)*/
/*
 *	unload workspace-map
 *
 *	@SYMBOL:	Y_ulwsmap
 *	@TYPE:		LCR
 *	@INPUT:		REG(2)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_ULWSMAP
 *	@PRIO:		0
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
void kernel_Y_ulwsmap (void)
{
	unsigned int code_offset, process_address;
	
	K_SETGLABEL_TWO_IN_LCR (Y_ulwsmap, process_address, code_offset);

	mpcb_del_wsmap ((mp_ctrlblk *)process_address, (unsigned char *)code_offset, Wptr);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_rmwsmap (void)*/
/*
 *	delete workspace-map
 *
 *	@SYMBOL:	Y_rmwsmap
 *	@TYPE:		LCR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_RMWSMAP
 *	@PRIO:		0
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
void kernel_Y_rmwsmap (void)
{
	unsigned int process_address;
	
	K_SETGLABEL_ONE_IN_LCR (Y_rmwsmap, process_address);

	mpcb_rm_wsmap ((mp_ctrlblk *)process_address);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mppclone (void)*/
/*
 *	clone mobile process
 *
 *	@SYMBOL:	Y_mppclone
 *	@TYPE:		LCR
 *	@INPUT:		REG(1)
 *	@OUTPUT: 	STACK(1)
 *	@CALL: 		K_MPPCLONE
 *	@PRIO:		20
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
void kernel_Y_mppclone (void)
{
	unsigned int process_address;

	K_SETGLABEL_ONE_IN_LCR (Y_mppclone, process_address);

	process_address = (word)mpcb_mpp_clone ((mp_ctrlblk *)process_address);
	if (process_address == NotProcess_p) {
		if (ccsp_ignore_errors) {
			RESCHEDULE;
		} else {
			BMESSAGE ("mobile process CLONE error at 0x%x, Wptr = 0x%x.\n", return_address, (unsigned int)Wptr);
			ccsp_kernel_exit (1, return_address);
		}
	}
	
	K_STKONE_OUT (process_address);
}
/*}}}*/
/*{{{  void kernel_Y_mppserialise (void)*/
/*
 *	serialise mobile process
 *
 *	@SYMBOL:	Y_mppserialise
 *	@TYPE:		LCR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MPPSERIALISE
 *	@PRIO:		20
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
void kernel_Y_mppserialise (void)
{
	unsigned int count, process_address;
	word **channel_address;
	byte *destination_address;
	
	K_SETGLABEL_THREE_IN_LCR (Y_mppserialise, count, destination_address, channel_address);

	/* actually pass a pointer to it, may need to nullify */
	process_address = ((word *)(*channel_address))[Pointer];
	if (!mpcb_mpp_serialise ((mp_ctrlblk **)process_address, (unsigned int *)process_address + 1, (int *)destination_address, (int *)count)) {
		if (ccsp_ignore_errors) {
			RESCHEDULE;
		} else {
			BMESSAGE ("mobile process serialise error at 0x%x, Wptr = 0x%x.\n", return_address, (unsigned int)Wptr);
			ccsp_kernel_exit (1, return_address);
		}
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mppdeserialise (void)*/
/*
 *	deserialise mobile process
 *
 *	@SYMBOL:	Y_mppdeserialise
 *	@TYPE:		LCR
 *	@INPUT:		REG(3)
 *	@OUTPUT: 	NONE
 *	@CALL: 		K_MPPDESERIALISE
 *	@PRIO:		20
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
void kernel_Y_mppdeserialise (void)
{
	unsigned int count, process_address;
	word **channel_address;
	byte *source_address;
	
	K_SETGLABEL_THREE_IN_LCR (Y_mppdeserialise, channel_address, source_address, count);

	/* pass a pointer to the ws locn */
	process_address = ((word *)(*channel_address))[Pointer];
	if (!mpcb_mpp_deserialise ((int)source_address, (int)count, (mp_ctrlblk **)process_address, (unsigned int *)process_address + 1)) {
		if (ccsp_ignore_errors) {
			RESCHEDULE;
		} else {
			BMESSAGE ("mobile process serialise error at 0x%x, Wptr = 0x%x.\n", return_address, (unsigned int)Wptr);
			ccsp_kernel_exit (1, return_address);
		}
	}

	K_ZERO_OUT ();
}
/*}}}*/
#endif	/* defined(RMOX_BUILD) || !defined(DYNAMIC_PROCS) */
/*}}}*/
/*{{{  MWS barriers */
#if 0
/*{{{  void kernel_Y_mwenb (void)*/
/*
 *	enable multiway sync (with ready address)
 */
void kernel_Y_mwenb (void)
{
	unsigned int *mwsync_address, process_address;
	word *temp_ptr;

	K_SETGLABEL_TWO_IN_LCR (Y_mwenb, process_address, mwsync_address);
	ENTRY_TRACE (Y_mwenb, "%p (count-down = %d), %p", mwsync_address, ((mwsync_t *)mwsync_address)->down_count, (void *)process_address);
	DTRACE ("YEWA", Wptr, mwsync_address);

#if 0
fprintf (stderr, "kernel_Y_mwenb(): 0x%8.8x: bar = 0x%8.8x: rc = %d, ec = %d, dc = %d, qfptr = 0x%8.8x, qbptr = 0x%8.8x\n", (unsigned int)Wptr, (unsigned int)mwsync_address,
		((mwsync_t *)mwsync_address)->ref_count, ((mwsync_t *)mwsync_address)->enroll_count, ((mwsync_t *)mwsync_address)->down_count,
		(unsigned int)((mwsync_t *)mwsync_address)->qfptr, (unsigned int)((mwsync_t *)mwsync_address)->qbptr);
#endif
	if (((mwsync_t *)mwsync_address)->qfptr && (((mwsync_t *)mwsync_address)->qfptr->wptr == (unsigned int *)Wptr)) {
		DTRACE ("YQWA", Wptr, mwsync_address);
		/* already on the queue */
	} else {
		((mwsync_t *)mwsync_address)->down_count--;
		if (((mwsync_t *)mwsync_address)->down_count > 0) {
			temp_ptr = (word *)dmem_alloc (sizeof (mwsyncwait_t));

			((mwsyncwait_t *)temp_ptr)->next = NULL;
			((mwsyncwait_t *)temp_ptr)->wptr = (unsigned int *)Wptr;
			((mwsyncwait_t *)temp_ptr)->priority = sched->priofinity;

			if (!((mwsync_t *)mwsync_address)->qfptr) {
				((mwsync_t *)mwsync_address)->qfptr = (mwsyncwait_t *)temp_ptr;
			} else {
				((mwsync_t *)mwsync_address)->qbptr->next = (mwsyncwait_t *)temp_ptr;
			}
			((mwsync_t *)mwsync_address)->qbptr = (mwsyncwait_t *)temp_ptr;

		} else {
			DTRACE ("YXWA", Wptr, mwsync_address);

			/* process_address is really in the disabling sequence somewhere */
			Wptr[MWSyncChosen] = (int)process_address;
			((mwsync_t *)mwsync_address)->down_count = ((mwsync_t *)mwsync_address)->enroll_count;

			/*{{{  reschedule other processes*/
			while (((mwsync_t *)mwsync_address)->qfptr) {
				word *currentp, temp1;
				temp_ptr = (word *)((mwsync_t *)mwsync_address)->qfptr;

				if (((mwsync_t *)mwsync_address)->qfptr == ((mwsync_t *)mwsync_address)->qbptr) {
					((mwsync_t *)mwsync_address)->qfptr = NULL;		/* end */
					((mwsync_t *)mwsync_address)->qbptr = NULL;
				} else {
					((mwsync_t *)mwsync_address)->qfptr = ((mwsyncwait_t *)temp_ptr)->next;
				}
				temp1 = ((mwsyncwait_t *)temp_ptr)->priority;
				currentp = (word *)((mwsyncwait_t *)temp_ptr)->wptr;

				currentp[Priofinity] = temp1; /* CGR FIXME: do something else */
				enqueue_process (sched, currentp);

				dmem_release (temp_ptr);
			}
			/*}}}*/

			Wptr[State] = Ready_p;
			mwaltlock.count = ((mwsync_t *)mwsync_address)->enroll_count;

			return_address = process_address;
			K_ZERO_OUT ();
		}
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mwdis (void)*/
/*
 *	disable multiway synch
 */
void kernel_Y_mwdis (void)
{
	unsigned int *mwsync_address, process_address;
	word *temp_ptr;
	
	K_SETGLABEL_TWO_IN_LCR (Y_mwdis, process_address, mwsync_address);
	ENTRY_TRACE (Y_mwdis, "%p, %p", (void *)process_address, mwsync_address);
	DTRACE ("YDWA", Wptr, mwsync_address);

#if 0
fprintf (stderr, "kernel_Y_mwdis(): 0x%8.8x: bar = 0x%8.8x\n", (unsigned int)Wptr, (unsigned int)mwsync_address);
fprintf (stderr, "kernel_Y_mwdis(): 0x%8.8x: bar = 0x%8.8x: rc = %d, ec = %d, dc = %d, qfptr = 0x%8.8x, qbptr = 0x%8.8x\n", (unsigned int)Wptr, (unsigned int)mwsync_address,
		((mwsync_t *)mwsync_address)->ref_count, ((mwsync_t *)mwsync_address)->enroll_count, ((mwsync_t *)mwsync_address)->down_count,
		(unsigned int)((mwsync_t *)mwsync_address)->qfptr, (unsigned int)((mwsync_t *)mwsync_address)->qbptr);
#endif
	if (((mwsync_t *)mwsync_address)->down_count == ((mwsync_t *)mwsync_address)->enroll_count) {
		DTRACE ("YYWA", Wptr, mwsync_address);
		Wptr[MWSyncChosen] = (int)process_address;
		Wptr[Temp] = (int)process_address;
	} else {
		word *previousp;

		DTRACE ("YZWA", Wptr, mwsync_address);

		/* didn't fire, remove ourselves from the wait set */
		temp_ptr = (word *)((mwsync_t *)mwsync_address)->qfptr;
		previousp = NULL;

		while (temp_ptr && (((mwsyncwait_t *)temp_ptr)->wptr != (unsigned int *)Wptr)) {
			previousp = temp_ptr;
			temp_ptr = (word *)((mwsyncwait_t *)temp_ptr)->next;
		}
		if (!temp_ptr) {
			/* fell off */
			impossible (5, return_address);
		}
		if (!previousp) {
			/* at the front */
			if (((mwsync_t *)mwsync_address)->qfptr == ((mwsync_t *)mwsync_address)->qbptr) {
				((mwsync_t *)mwsync_address)->qfptr = NULL;
				((mwsync_t *)mwsync_address)->qbptr = NULL;
			} else {
				((mwsync_t *)mwsync_address)->qfptr = ((mwsyncwait_t *)temp_ptr)->next;
			}
		} else {
			((mwsyncwait_t *)previousp)->next = ((mwsyncwait_t *)temp_ptr)->next;
			if (((mwsync_t *)mwsync_address)->qbptr == (mwsyncwait_t *)temp_ptr) {
				/* at the back */
				((mwsync_t *)mwsync_address)->qbptr = (mwsyncwait_t *)previousp;
			}
		}
		((mwsync_t *)mwsync_address)->down_count++;
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mwalt (void)*/
/*
 *	multiway alt start
 */
void kernel_Y_mwalt (void)
{
	word *temp_ptr;

	K_SETGLABEL_ZERO_IN_LCR (Y_mwalt);
	ENTRY_TRACE0 (Y_mwalt);
	DTRACE ("YAW", Wptr);
#if 0
fprintf (stderr, "kernel_Y_mwalt(): \n");
#endif

	/* claim mutex */
	if (!mwaltlock.value) {
		DTRACE ("YMW", Wptr);

		/* block */
		if (mwaltlock.qfptr) {
			mwaltlock.qbptr[Link] = (word)Wptr;
		} else {
			mwaltlock.qfptr = Wptr;
		}
		mwaltlock.qbptr = Wptr;

		Wptr[Temp] = return_address;
		K_LOADLABADDR (Y_mwalt2, temp_ptr);
		Wptr[Iptr] = (word)temp_ptr;
		Wptr[Link] = NotProcess_p;
		save_priofinity (sched, Wptr);
		RESCHEDULE;
	} else {
		/* do claim */
		mwaltlock.value = 0;
		mwaltlock.count = 1;
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mwalt2 (void)*/
/*
 *	2nd half of multiway alt start -- get left here
 *	if we blocked at the mutex claim
 */
void kernel_Y_mwalt2 (void)
{
	unsigned int return_address;
	
	K_SETGLABEL_ZERO_IN (Y_mwalt2);
	ENTRY_TRACE0 (Y_mwalt);
#if 0
fprintf (stderr, "kernel_Y_mwalt2(): \n");
#endif

	return_address = Wptr[Temp];
	mwaltlock.count = 1;

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mwaltwt (void)*/
/*
 *	multiway alt wait
 */
void kernel_Y_mwaltwt (void)
{
	K_SETGLABEL_ZERO_IN_LCR (Y_mwaltwt);
	ENTRY_TRACE0 (Y_mwaltwt);
	DTRACE ("YWW", Wptr);
#if 0
fprintf (stderr, "kernel_Y_mwaltwt(): \n");
#endif

	mwaltlock.count = 0;
	/* release mutex */
	if (!mwaltlock.value) {
		if (mwaltlock.qfptr) {
			/* release one process */
			word *currentp = mwaltlock.qfptr;

			if (mwaltlock.qfptr == mwaltlock.qbptr) {
				/* last */
				mwaltlock.qfptr = NULL;
				mwaltlock.qbptr = NULL;
			} else {
				mwaltlock.qfptr = (word *)(mwaltlock.qfptr[Link]);
			}

			enqueue_process (sched, currentp);
		} else {
			mwaltlock.value = 1;
		}
	} else {
		impossible (6, return_address);
	}

	K_JUMP (I_altwt);
}
/*}}}*/
/*{{{  void kernel_Y_mwaltend (void)*/
/*
 *	multiway alt end
 */
void kernel_Y_mwaltend (void)
{
	K_SETGLABEL_ZERO_IN_LCR (Y_mwaltend);
	ENTRY_TRACE0 (Y_mwaltend);
	DTRACE ("YNW", Wptr);
#if 0
fprintf (stderr, "kernel_Y_mwaltend(): \n");
#endif

	mwaltlock.count--;
	if (mwaltlock.count == 0) {
		if (mwaltlock.qfptr) {
			/* release one process */
			word *currentp = mwaltlock.qfptr;

			if (mwaltlock.qfptr == mwaltlock.qbptr) {
				/* last */
				mwaltlock.qfptr = NULL;
				mwaltlock.qbptr = NULL;
			} else {
				mwaltlock.qfptr = (word *)(mwaltlock.qfptr[Link]);
			}

			enqueue_process (sched, currentp);
		} else {
			mwaltlock.value = 1;
		}
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  static inline void mwaltlock_releaseprocess (void)*/
/*
 *	releases a single process from the mwaltlock (global) queue
 */
static inline void mwaltlock_releaseprocess (word *Wptr, sched_t *sched)
{
	if (mwaltlock.qfptr) {
		/* release one process */
		word *currentp = mwaltlock.qfptr;

		if (mwaltlock.qfptr == mwaltlock.qbptr) {
			/* last */
			mwaltlock.qfptr = NULL;
			mwaltlock.qbptr = NULL;
		} else {
			mwaltlock.qfptr = (word *)(mwaltlock.qfptr[Link]);
		}

		enqueue_process (sched, currentp);
	} else {
		mwaltlock.value = 1;
	}
}
/*}}}*/
/*{{{  static inline void mws_barriercomplete (mws_parbarrier_t *me)*/
/*
 *	called to reschedule all processes blocked on a barrier
 *	if 'me' is set, the matching set gets one less synch (presumably because it's us)
 */
static inline void mws_barriercomplete (word *Wptr, sched_t *sched, mws_parbarrier_t *me, mws_barrier_t *mws_barrier)
{
	mws_parbarrier_t *mws_parbarrier;
	int tmpint_a = 0;

	/* reschedule other processes */
	for (mws_parbarrier = mws_barrier->set_fptr; mws_parbarrier; mws_parbarrier = mws_parbarrier->next_set) {
		/* for each blocked process, add to run-queue */

		int mws_count = (mws_parbarrier == me) ? mws_parbarrier->sync_count - 1 : mws_parbarrier->sync_count;

#if 0
		BMESSAGE ("mws_barriercomplete: resheduling %d processes from par-barrier at 0x%8.8x (%d enrolled here)\n", mws_count, (unsigned int)mws_parbarrier, mws_parbarrier->enroll_count);
#endif
		for (; mws_count; mws_count--) {
			mws_procbarrier_t *mws_thisprocbarrier;
			word *currentp;
			int do_queue = 1;

			mws_thisprocbarrier = mws_parbarrier->q_fptr;
			mws_parbarrier->q_fptr = mws_thisprocbarrier->q_next;
			if (mws_parbarrier->q_fptr) {
				mws_parbarrier->q_fptr->q_prev = NULL;
			}
			mws_thisprocbarrier->q_next = NULL;
			currentp = mws_thisprocbarrier->wptr;
			
			if (mws_thisprocbarrier->flags == MWS_ALT_SELECTED) {
				/* already selected -- do nothing */
				do_queue = 0;
			} else if (mws_thisprocbarrier->flags == MWS_ALT) {
				/* unselected ALT, better force completion in disabling*/
				mws_thisprocbarrier->flags = MWS_ALT_SELECTED;
				if (currentp[State] == Ready_p) {
					/* ALT is already on the run-queue */
					do_queue = 0;
				} else if (currentp[State] == Waiting_p) {
					/* we're rescheduling a now ready ALT */
					mwaltlock.count++;
					currentp[State] = Ready_p;
					currentp[MWSyncChosen] = (int)currentp;			/* tell waking ALTWT that it was a multiway sync which did it */
				}
			}

			if (do_queue) {
				enqueue_process (sched, currentp);
			}

		}
		if (!mws_parbarrier->q_fptr) {
			mws_parbarrier->q_bptr = NULL;
		}
		mws_parbarrier->down_count += mws_parbarrier->sync_count;
#if (MWSDEBUG == 1)
		BMESSAGE ("mws_barriercomplete: reset PAR barrier to %d from (%d,%d)\n", mws_parbarrier->down_count, mws_parbarrier->sync_count, mws_parbarrier->enroll_count);
#endif
		if ((mws_parbarrier->sync_count > 0) && (mws_parbarrier->down_count <= 0)) {
			/* this set can still synchronise, so remember that.. */
			tmpint_a++;
		}
	}

	mws_barrier->sets_downcount = (mws_barrier->sets_enrolled - tmpint_a);
#if (MWSDEBUG == 1)
	BMESSAGE ("mws_barriercomplete: reset top-level barrier to %d\n", mws_barrier->sets_downcount);
	BMESSAGE ("mws_barriercomplete: all blocked processes scheduled, there are %d ALTs which need to disable\n", mwaltlock.count);
#endif
}
/*}}}*/
/*{{{  void kernel_Y_mws_binit (void)*/
/*
 *	new multiway sync barrier init
 */
void kernel_Y_mws_binit (void)
{
	mws_barrier_t *mws_barrier;
	
	K_SETGLABEL_ONE_IN_LCR (Y_mws_binit, mws_barrier);

#if (MWSDEBUG == 1)
	MESSAGE ("Y_mws_binit: barrier at 0x%8.8x\n", (unsigned int)mws_barrier);
#endif

	mws_barrier->parent = NULL;
	mws_barrier->sets_enrolled = 0;
	mws_barrier->sets_downcount = 0;
	mws_barrier->set_fptr = NULL;
	mws_barrier->set_bptr = NULL;

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_pbrilnk (void)*/
/*
 *	new multiway sync par-barrier init and link
 */
void kernel_Y_mws_pbrilnk (void)
{
	mws_parbarrier_t *mws_parbarrier;
	mws_barrier_t *mws_barrier;
	
	K_SETGLABEL_TWO_IN_LCR (Y_mws_pbrilnk, mws_parbarrier, mws_barrier);

#if (MWSDEBUG == 1)
	MESSAGE ("Y_mws_pbrilnk: parbarrier at 0x%8.8x, barrier at 0x%8.8x\n", (unsigned int)mws_parbarrier, (unsigned int)mws_barrier);
#endif

	mws_parbarrier->next_set = NULL;
	mws_parbarrier->prev_set = NULL;
	mws_parbarrier->parent_set = NULL;
	mws_parbarrier->barrier_link = mws_barrier;
	mws_parbarrier->enroll_count = 0;
	mws_parbarrier->sync_count = 0;
	mws_parbarrier->down_count = 0;
	mws_parbarrier->q_fptr = NULL;
	mws_parbarrier->q_bptr = NULL;

	/* stitch in */
	if (mws_barrier->set_fptr) {
		mws_parbarrier->prev_set = mws_barrier->set_bptr;
		mws_barrier->set_bptr->next_set = mws_parbarrier;
	} else {
		mws_barrier->set_fptr = mws_parbarrier;
	}
	mws_barrier->set_bptr = mws_parbarrier;

	/* mws_barrier->sets_enrolled++; */

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_pbrulnk (void)*/
/*
 *	new multiway sync par-barrier unlink
 */
void kernel_Y_mws_pbrulnk (void)
{
	mws_parbarrier_t *mws_parbarrier;
	mws_barrier_t *mws_barrier;
	
	K_SETGLABEL_ONE_IN_LCR (Y_mws_pbrulnk, mws_parbarrier);

#if (MWSDEBUG == 1)
	MESSAGE ("Y_mws_pbrulnk: parbarrier at 0x%8.8x\n", (unsigned int)mws_parbarrier);
#endif
	if (mws_parbarrier->enroll_count) {
		BMESSAGE ("serious: mws_pbrulink: barrier has %d enrolled processes!\n", mws_parbarrier->enroll_count);
	}

	mws_barrier = mws_parbarrier->barrier_link;
	if ((mws_barrier->set_fptr == mws_parbarrier) && (mws_barrier->set_bptr == mws_parbarrier)) {
		/* singleton */
		mws_barrier->set_fptr = NULL;
		mws_barrier->set_bptr = NULL;
	} else if (mws_barrier->set_fptr == mws_parbarrier) {
		/* front of queue */
		mws_barrier->set_fptr = mws_parbarrier->next_set;
		mws_barrier->set_fptr->prev_set = NULL;
		mws_parbarrier->next_set = NULL;
	} else if (mws_barrier->set_bptr == mws_parbarrier) {
		/* back of queue */
		mws_barrier->set_bptr = mws_parbarrier->prev_set;
		mws_barrier->set_bptr->next_set = NULL;
		mws_parbarrier->prev_set = NULL;
	} else {
		/* in the middle somewhere */
		mws_parbarrier->prev_set->next_set = mws_parbarrier->next_set;
		mws_parbarrier->next_set->prev_set = mws_parbarrier->prev_set;
		mws_parbarrier->prev_set = NULL;
		mws_parbarrier->next_set = NULL;
	}

	/* mws_barrier->sets_enrolled--; */

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_ppilnk (void)*/
/*
 *	new multiway sync proc-barrier init and link
 */
void kernel_Y_mws_ppilnk (void)
{
	mws_procbarrier_t *mws_procbarrier;
	mws_parbarrier_t *mws_parbarrier;
	
	K_SETGLABEL_TWO_IN_LCR (Y_mws_ppilnk, mws_procbarrier, mws_parbarrier);

#if (MWSDEBUG == 1)
	MESSAGE ("Y_mws_ppilnk: procbarrier at 0x%8.8x, parbarrier at 0x%8.8x\n", (unsigned int)mws_procbarrier, (unsigned int)mws_parbarrier);
#endif
	mws_procbarrier->q_next = NULL;
	mws_procbarrier->q_prev = NULL;
	mws_procbarrier->parbarrier_link = mws_parbarrier;
	mws_procbarrier->wptr = NULL;
	mws_procbarrier->flags = MWS_NONE;

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_pbenroll (void)*/
/*
 *	new multiway sync par-barrier enroll (and link-to-parent)
 */
void kernel_Y_mws_pbenroll (void)
{
	mws_parbarrier_t *mws_parbarrier, *mws_parent;
	mws_barrier_t *mws_barrier;
	int mws_count;
	
	K_SETGLABEL_THREE_IN_LCR (Y_mws_pbenroll, mws_parbarrier, mws_parent, mws_count);

#if (MWSDEBUG == 1)
	MESSAGE ("Y_mws_pbenroll: Wptr=0x%8.8x, count = %d, parbarrier at 0x%8.8x, parent barrier at 0x%8.8x\n", (unsigned int)Wptr, mws_count, (unsigned int)mws_parbarrier, (unsigned int)mws_parent);
#endif
	mws_barrier = mws_parbarrier->barrier_link;
	if (mws_count) {
		/* enrolling something */
		mws_parbarrier->enroll_count += mws_count;
		mws_parbarrier->sync_count += mws_count;
		mws_parbarrier->down_count += mws_count;

		if (mws_parbarrier->enroll_count == mws_count) {
			/* first processes enrolling here */
			mws_barrier->sets_enrolled++;
			mws_barrier->sets_downcount++;

			if (mws_parent) {
				mws_parbarrier->parent_set = mws_parent;
#if (MWSDEBUG == 1)
				BMESSAGE ("mws_pbenroll: resigning in parent-set (%d,%d,%d)\n", mws_parent->down_count, mws_parent->enroll_count, mws_parent->sync_count);
#endif
				/* and resign it from the parent */
				mws_parent->sync_count--;
				mws_parent->down_count--;

				if (!mws_parent->sync_count) {
					/* this is now empty, remove from sync */
					mws_barrier->sets_enrolled--;
					mws_barrier->sets_downcount--;
#if (MWSDEBUG == 1)
					BMESSAGE ("mws_pbenroll: removed parent-set from sync (now %d of %d sets left)\n", mws_barrier->sets_downcount, mws_barrier->sets_enrolled);
#endif
				} else if (!mws_parent->down_count) {
#if (MWSDEBUG == 1)
					BMESSAGE ("mws_pbenroll: parent now ready-to-sync (%d enrolled processes)\n", mws_parent->enroll_count);
#endif
					/* we just completed the synchronisation here */
					mws_barrier->sets_downcount--;
				}
			}
		}
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_pbresign (void)*/
/*
 *	new multiway sync par-barrier resign
 */
void kernel_Y_mws_pbresign (void)
{
	mws_parbarrier_t *mws_parbarrier, *mws_parent;
	mws_barrier_t *mws_barrier;
	int mws_count, tmpint_a, tmpint_b;
	
	K_SETGLABEL_TWO_IN_LCR (Y_mws_pbresign, mws_parbarrier, mws_count);

	mws_barrier = mws_parbarrier->barrier_link;
	mws_parent = mws_parbarrier->parent_set;
	tmpint_b = 0;

	if (mws_count) {
		tmpint_a = mws_parbarrier->down_count;

		/* resigning something */
		mws_parbarrier->enroll_count -= mws_count;
		mws_parbarrier->sync_count -= mws_count;
		mws_parbarrier->down_count -= mws_count;

#if (MWSDEBUG == 1)
		BMESSAGE ("mws_pbresign: Wptr=0x%8.8x parbarrier=0x%8.8x count=%d, down=%d, sync=%d, enroll=%d, orig-down=%d, parent-set=0x%8.8x\n", (unsigned int)Wptr, (unsigned int)mws_parbarrier,
			mws_count, mws_parbarrier->down_count, mws_parbarrier->sync_count, mws_parbarrier->enroll_count, tmpint_a, (unsigned int)mws_parent);
#endif

		if (!mws_parbarrier->enroll_count) {
			/*{{{  last processes resigning in this PARBARRIER*/
			mws_barrier->sets_enrolled--;
			mws_barrier->sets_downcount--;

			if (mws_parent) {
				/* re-enroll in the parent set */
				if (!mws_parent->sync_count) {
					/* this will no longer be empty, re-add to sync */
					mws_barrier->sets_enrolled++;
					mws_barrier->sets_downcount++;
				} else if (!mws_parent->down_count) {
					/* this will no longer be synching */
					mws_barrier->sets_downcount++;
				}
				mws_parent->sync_count++;
				mws_parent->down_count++;
			}
			/*}}}*/
		} else if ((mws_parbarrier->down_count <= 0) && (tmpint_a > 0)) {
			/*{{{  we completed a synchronisation locally*/
			mws_barrier->sets_downcount--;
			/*}}}*/
		}

		/* check to see if we just completed a top-level sync */
		if (!mws_barrier->sets_downcount && mws_barrier->sets_enrolled) {
			/*{{{  top-level synchronisation completed*/
			mwaltlock.count = 0;
			tmpint_b = 1;			/* flag the fact that we did the synchronisation */

			mws_barriercomplete (Wptr, sched, NULL, mws_barrier);
			/*}}}*/
		}
	}

	/* we had the ALT lock on the way in, release it only if no ALTy processes were selected */
	if (!tmpint_b || !mwaltlock.count) {
		mwaltlock.count = 0;
		mwaltlock_releaseprocess (Wptr, sched);
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_pbadjsync (void)*/
/*
 *	new multiway sync par barrier adjust
 */
void kernel_Y_mws_pbadjsync (void)
{
	mws_parbarrier_t *mws_parbarrier;
	mws_barrier_t *mws_barrier;
	int mws_count, tmpint_a, tmpint_b;
	
	K_SETGLABEL_TWO_IN_LCR (Y_mws_pbadjsync, mws_parbarrier, mws_count);

	mws_barrier = mws_parbarrier->barrier_link;
	tmpint_b = 0;

#if (MWSDEBUG == 1)
	BMESSAGE ("mws_pbadjsync: Wptr=0x%8.8x, parbarrier at 0x%8.8x, adjust = %d\n", (unsigned int)Wptr, (unsigned int)mws_parbarrier, (int)mws_count);
#endif

	if (mws_count) {
		tmpint_a = mws_parbarrier->down_count;

		/* adjust sync-count and down-count */
		mws_parbarrier->sync_count += mws_count;
		mws_parbarrier->down_count += mws_count;

#if (MWSDEBUG == 1)
		BMESSAGE ("mws_pbadjsync: Wptr=0x%8.8x parbarrier=0x%8.8x count=%d, down=%d, sync=%d, enroll=%d, orig-down=%d\n", (unsigned int)Wptr, (unsigned int)mws_parbarrier,
			mws_count, mws_parbarrier->down_count, mws_parbarrier->sync_count, mws_parbarrier->enroll_count, tmpint_a);
#endif

		if (mws_parbarrier->sync_count <= 0) {
			BMESSAGE ("mws_pbadjsync: PANIC! adjust %d moved sync-count to %d, restoring to %d", mws_count, mws_parbarrier->sync_count, tmpint_a);
			mws_parbarrier->sync_count -= mws_count;
			mws_parbarrier->down_count -= mws_count;		/* so things don't go (too) strange */
			goto skip_to_out;
		}

		if ((mws_parbarrier->down_count <= 0) && (tmpint_a > 0)) {
			/*{{{  we completed a synchronisation locally*/
			mws_barrier->sets_downcount--;
			/*}}}*/
		} else if ((mws_parbarrier->down_count > 0) && (tmpint_a <= 0)) {
			/*{{{  we stopped a synchronisation locally*/
			mws_barrier->sets_downcount++;
			/*}}}*/
		}

		/* check to see if we just completed a top-level sync */
		if (!mws_barrier->sets_downcount && mws_barrier->sets_enrolled) {
			/*{{{  top-level synchronisation completed*/
			mwaltlock.count = 0;
			tmpint_b = 1;			/* flag the fact that we did the synchronisation */

			mws_barriercomplete (Wptr, sched, NULL, mws_barrier);
			/*}}}*/
		}
	}
skip_to_out:

	/* we had the ALT lock on the way in, release it only if no ALTy processes were selected */
	if (!tmpint_b || !mwaltlock.count) {
		mwaltlock.count = 0;
		mwaltlock_releaseprocess (Wptr, sched);
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_sync (void)*/
/*
 *	new multiway sync instruction
 */
void kernel_Y_mws_sync (void)
{
	mws_procbarrier_t *mws_procbarrier;
	mws_parbarrier_t *mws_parbarrier, *mws_parent;
	mws_barrier_t *mws_barrier;
	
	K_SETGLABEL_ONE_IN_LCR (Y_mws_sync, mws_procbarrier);

	mws_parbarrier = mws_procbarrier->parbarrier_link;

	/* NOTE: down_count could be <= 0, in which case we're blocking on an interleaving synchronisation which is effectively ready */

	/* first step -- synchronise on this set */
	mws_parbarrier->down_count--;

	if (!mws_parbarrier->down_count) {
		/* we just completed this synchronisation;  second step -- synchronise at the top-level */
		mws_barrier = mws_parbarrier->barrier_link;
		mws_barrier->sets_downcount--;
	}

#if (MWSDEBUG == 1)
	BMESSAGE ("mws_sync: sync on procbarrier at 0x%8.8x, parbarrier at 0x%8.8x, %d left here, %d of %d sets\n", (unsigned int)mws_procbarrier, (unsigned int)mws_parbarrier,
			mws_parbarrier->down_count, mws_barrier->sets_downcount, mws_barrier->sets_enrolled);
#endif

	if (!mws_barrier->sets_downcount) {
		/* synchronisation complete! */
#if (MWSDEBUG == 1)
		BMESSAGE ("mws_sync: synchronisation complete! (%d sets enrolled, in this set, %d of (%d,%d))\n", mws_barrier->sets_enrolled, mws_parbarrier->down_count, mws_parbarrier->sync_count, mws_parbarrier->enroll_count);
#endif

		mwaltlock.count = 0;

		/* this process is a slightly odd case -- because it's not on the queue */
		mws_parent = mws_parbarrier;
		mws_barriercomplete (Wptr, sched, mws_parent, mws_barrier);

		/* we had the ALT lock on the way in, release it only if no ALTy processes were selected */
		if (!mwaltlock.count) {
			mwaltlock_releaseprocess (Wptr, sched);
		}

	} else {
#if (MWSDEBUG == 1)
		BMESSAGE ("mws_sync: synchronisation incomplete (%d sets left), sleeping\n", mws_barrier->sets_downcount);
#endif

		/* release the ALT lock */
		mwaltlock_releaseprocess (Wptr, sched);

		/* synchronisation not complete, sleep on the parbarrier */
		if (!mws_parbarrier->q_fptr) {
			/* first */
			mws_parbarrier->q_fptr = mws_procbarrier;
		} else {
			/* not first */
			mws_parbarrier->q_bptr->q_next = mws_procbarrier;
			mws_procbarrier->q_prev = mws_parbarrier->q_bptr;
		}
		mws_parbarrier->q_bptr = mws_procbarrier;

		/* suspend */
		mws_procbarrier->wptr = Wptr;

		Wptr[Iptr] = (word)return_address;
		/* Wptr[Link] = NotProcess_p; */
		save_priofinity (sched, Wptr);
		RESCHEDULE;
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_altlock (void)*/
/*
 *	new multiway sync ALT lock
 */
void kernel_Y_mws_altlock (void)
{
	K_SETGLABEL_ZERO_IN_LCR (Y_mws_altlock);

#if (MWSDEBUG == 1)
	BMESSAGE ("mws_altlock: here (value = %d, count = %d)\n", mwaltlock.value, mwaltlock.count);
#endif

	/* claim mutex */
	if (!mwaltlock.value) {
		/* block */
		if (mwaltlock.qfptr) {
			mwaltlock.qbptr[Link] = (word)Wptr;
		} else {
			mwaltlock.qfptr = Wptr;
		}
		mwaltlock.qbptr = Wptr;

		Wptr[Iptr] = (word)return_address;
		Wptr[Link] = NotProcess_p;
		save_priofinity (sched, Wptr);
		RESCHEDULE;
	} else {
		/* do claim */
		mwaltlock.value = 0;
		mwaltlock.count = 0;
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_altunlock (void)*/
/*
 *	new multiway sync ALT unlock
 */
void kernel_Y_mws_altunlock (void)
{
	K_SETGLABEL_ZERO_IN_LCR (Y_mws_altunlock);

#if (MWSDEBUG == 1)
	BMESSAGE ("mws_altunlock: here (value = %d, count = %d)\n", mwaltlock.value, mwaltlock.count);
#endif

	mwaltlock.count = 0;
	mwaltlock_releaseprocess (Wptr, sched);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_alt (void)*/
/*
 *	new multiway sync ALT start
 */
void kernel_Y_mws_alt (void)
{
	K_SETGLABEL_ZERO_IN_LCR (Y_mws_alt);

	Wptr[MWSyncChosen] = NotProcess_p;
	Wptr[State] = Enabling_p;

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_altend (void)*/
/*
 *	new multiway sync ALT end
 */
void kernel_Y_mws_altend (void)
{
	K_SETGLABEL_ZERO_IN_LCR (Y_mws_altend);

	ENTRY_TRACE0 (Y_mws_altend);

	if (Wptr[MWSyncChosen] == (int)Wptr) {
		/* slightly special case: shouldn't happen probably.. */
		BMESSAGE ("mws_altend: we were woken by mwsync, but didn't choose one!\n");
		return_address = Wptr[Temp];
	} else if (Wptr[MWSyncChosen] != NotProcess_p) {
		return_address = Wptr[MWSyncChosen];
		mwaltlock.count--;
#if (MWSDEBUG == 1)
		BMESSAGE ("mws_altend: multi-way sync selected in this ALT, mwaltlock.count = %d\n", mwaltlock.count);
#endif

		if (!mwaltlock.count) {
			mwaltlock_releaseprocess (Wptr, sched);
		}
	} else {
		return_address = Wptr[Temp];
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_enb (void)*/
/*
 *	new multiway sync ALT enable
 */
void kernel_Y_mws_enb (void)
{
	mws_procbarrier_t *mws_procbarrier;
	mws_parbarrier_t *mws_parbarrier, *mws_parent;
	unsigned int process_address;
	
	K_SETGLABEL_TWO_IN_LCR (Y_mws_enb, process_address, mws_procbarrier);

#if (MWSDEBUG == 1)
	BMESSAGE ("mws_enb: here!  mws_procbarrier = 0x%8.8x\n", (unsigned int)mws_procbarrier);
#endif

	mws_parbarrier = mws_procbarrier->parbarrier_link;
	if (mws_procbarrier->flags == MWS_ALT) {
		/* already ALTing on this one */
	} else {
		mws_barrier_t *mws_barrier;
		/* NOTE: down_count could be <= 0, in which case we're blocking on an interleaving synchronisation which is effectively ready */

		/* first step -- synchronise on this set */
		mws_parbarrier->down_count--;

		if (!mws_parbarrier->down_count) {
			/* we just completed this synchronisation;  second step -- synchronise at the top-level */
			mws_barrier = mws_parbarrier->barrier_link;
			mws_barrier->sets_downcount--;
		}

		if (!mws_barrier->sets_downcount) {
			/*{{{  synchronisation complete!*/

			mwaltlock.count = 1;		/* because we'll need to disable too! */

#if (MWSDEBUG == 1)
			BMESSAGE ("mws_enb: synchronisation complete! (%d sets enrolled, in this set, %d of (%d,%d))\n", mws_barrier->sets_enrolled, mws_parbarrier->down_count, mws_parbarrier->sync_count, mws_parbarrier->enroll_count);
			BMESSAGE ("mws_enb: selecting enabling proc-barrier at 0x%8.8x\n", (unsigned int)mws_procbarrier);
#endif
			mws_procbarrier->flags = MWS_ALT_SELECTED;
			Wptr[MWSyncChosen] = (int)Wptr;

			/* this process is a slightly odd case -- because it's not on the queue */
			mws_parent = mws_parbarrier;
			mws_barriercomplete (Wptr, sched, mws_parent, mws_barrier);

			return_address = process_address;		/* go direct to disabling */
			/*}}}*/
		} else {
#if (MWSDEBUG == 1)
			BMESSAGE ("mws_enb: synchronisation incomplete (%d sets left), sleeping\n", mws_barrier->sets_downcount);
#endif

			/* synchronisation not complete, place process on the parbarrier */
			if (!mws_parbarrier->q_fptr) {
				/* first */
				mws_parbarrier->q_fptr = mws_procbarrier;
			} else {
				/* not first */
				mws_parbarrier->q_bptr->q_next = mws_procbarrier;
				mws_procbarrier->q_prev = mws_parbarrier->q_bptr;
			}
			mws_parbarrier->q_bptr = mws_procbarrier;

			/* finish up, but don't suspend */
			mws_procbarrier->wptr = Wptr;
			mws_procbarrier->flags = MWS_ALT;
		}
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_dis (void)*/
/*
 *	new multiway sync ALT disable
 */
void kernel_Y_mws_dis (void)
{
	mws_procbarrier_t *mws_procbarrier;
	mws_parbarrier_t *mws_parbarrier;
	mws_barrier_t *mws_barrier;
	unsigned int process_address;
	
	K_SETGLABEL_TWO_IN_LCR (Y_mws_dis, process_address, mws_procbarrier);

#if 0
	BMESSAGE ("mws_dis: here!\n");
#endif

	if (mws_procbarrier->flags == MWS_ALT_SELECTED) {
#if (MWSDEBUG == 1)
		BMESSAGE ("mws_dis: ALT selected on procbarrier at 0x%8.8x\n", (unsigned int)mws_procbarrier);
#endif
		/* this one triggered */
		Wptr[MWSyncChosen] = (int)process_address;
		Wptr[Temp] = (int)process_address;
		/* hold the lock until past MWSALTEND -- last will see count == 0 */
	} else if (mws_procbarrier->flags == MWS_ALT) {
		/* unselected ALT, remove from wait set */
#if (MWSDEBUG == 1)
		BMESSAGE ("mws_dis: ALT _not_ selected on procbarrier at 0x%8.8x\n", (unsigned int)mws_procbarrier);
#endif
		mws_parbarrier = mws_procbarrier->parbarrier_link;

		if (mws_parbarrier->q_fptr == mws_parbarrier->q_bptr) {
			if (mws_procbarrier != mws_parbarrier->q_fptr) {
				/* something waiting, but it's not us..! */
				BMESSAGE ("mws_dis: serious: not me!\n");
			} else {
				/* lone process on queue */
				mws_parbarrier->q_fptr = NULL;
				mws_parbarrier->q_bptr = NULL;
			}
		} else if (mws_procbarrier == mws_parbarrier->q_fptr) {
			/* first on queue */
			mws_parbarrier->q_fptr = mws_procbarrier->q_next;
			mws_parbarrier->q_fptr->q_prev = NULL;
			mws_procbarrier->q_next = NULL;
		} else if (mws_procbarrier == mws_parbarrier->q_bptr) {
			/* last on queue */
			mws_parbarrier->q_bptr = mws_procbarrier->q_prev;
			mws_parbarrier->q_bptr->q_next = NULL;
			mws_procbarrier->q_prev = NULL;
		} else {
			/* in the middle somewhere */
			mws_procbarrier->q_prev->q_next = mws_procbarrier->q_next;
			mws_procbarrier->q_next->q_prev = mws_procbarrier->q_prev;
			mws_procbarrier->q_next = NULL;
			mws_procbarrier->q_prev = NULL;
		}

		mws_barrier = mws_parbarrier->barrier_link;
		mws_parbarrier->down_count++;

		if (mws_parbarrier->down_count == 1) {
			/* we just stopped this set from synchronising */
			mws_barrier->sets_downcount++;
		}

	}
	mws_procbarrier->wptr = NULL;
	mws_procbarrier->flags = MWS_NONE;

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_altpostlock (void)*/
/*
 *	new multiway sync lock-after-ALT
 */
void kernel_Y_mws_altpostlock (void)
{
	K_SETGLABEL_ZERO_IN_LCR (Y_mws_altpostlock);

#if (MWSDEBUG == 1)
	BMESSAGE ("mws_altpostlock: Wptr=0x%8.8x, Wptr[MWSyncChosen]=0x%8.8x, mwaltlock.value = %d, .count = %d\n", (unsigned int)Wptr, (unsigned int)Wptr[MWSyncChosen], mwaltlock.value, mwaltlock.count);
#endif
	if (Wptr[MWSyncChosen] == (int)Wptr) {
		/* yes, this ALT was woken up by a multi-way sync, so already have the lock */
	} else {
		/* claim mutex */
		if (!mwaltlock.value) {
			/* block */
			if (mwaltlock.qfptr) {
				mwaltlock.qbptr[Link] = (word)Wptr;
			} else {
				mwaltlock.qfptr = Wptr;
			}
			mwaltlock.qbptr = Wptr;

			Wptr[Iptr] = (word)return_address;
			Wptr[Link] = NotProcess_p;
			save_priofinity (sched, Wptr);
			RESCHEDULE;
		} else {
			/* do claim */
			mwaltlock.value = 0;
			mwaltlock.count = 0;
		}
	}
	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mws_ppbaseof (void)*/
/*
 *	turns a proc-barrier reference into a base-of-barrier reference
 */
void kernel_Y_mws_ppbaseof (void)
{
	mws_procbarrier_t *mws_procbarrier;
	mws_parbarrier_t *mws_parbarrier;
	mws_barrier_t *mws_barrier;
	
	K_SETGLABEL_ONE_IN_LCR (Y_mws_ppbaseof, mws_procbarrier);
	ENTRY_TRACE (Y_mws_ppbaseof, "%p", mws_procbarrier);

#if (MWSDEBUG == 1)
	MESSAGE ("Y_mws_ppbaseof: procbarrier at 0x%8.8x\n", (unsigned int)mws_procbarrier);
#endif
	mws_parbarrier = mws_procbarrier->parbarrier_link;
	mws_barrier = mws_parbarrier ? mws_parbarrier->barrier_link : NULL;

	K_STKONE_OUT (mws_barrier);
}
/*}}}*/
/*{{{  void kernel_Y_mws_ppparof (void)*/
/*
 *	turns a proc-barrier reference into a par-barrier reference
 */
void kernel_Y_mws_ppparof (void)
{
	mws_procbarrier_t *mws_procbarrier;
	mws_parbarrier_t *mws_parbarrier;
	
	K_SETGLABEL_ONE_IN_LCR (Y_mws_ppparof, mws_procbarrier);
	ENTRY_TRACE (Y_mws_ppparof, "%p", mws_procbarrier);

#if (MWSDEBUG == 1)
	MESSAGE ("Y_mws_ppparof: procbarrier at 0x%8.8x\n", (unsigned int)mws_procbarrier);
#endif
	mws_parbarrier = mws_procbarrier->parbarrier_link;

	K_STKONE_OUT (mws_parbarrier);
}
/*}}}*/
#endif
/*}}}*/

