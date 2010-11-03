/*
 *	Transputer style kernel in mixed C/asm
 *	Copyright (C) 1998 Jim Moores
 *	Based on the KRoC/sparc kernel Copyright (C) 1994-2000 D.C. Wood and P.H. Welch
 *	RMoX hacks copyright (C) 2002 Fred Barnes and Brian Vinter
 *	Portions copyright (C) 1999-2005 Fred Barnes <frmb@kent.ac.uk>
 *	Portions copyright (C) 2005-2008 Carl Ritson <cgr@kent.ac.uk>
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

#include <compiler.h>
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
#include <ccsp_pony.h>

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

#define CONST_ARRAY_LENGTH(array) (sizeof (array) / sizeof ((array)[0]))

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
#elif defined(ENABLE_MP) && defined(RMOX_BUILD)
static sched_t		*_ccsp_schedulers[RMOX_MAX_CPUS]	CACHELINE_ALIGN;
static sched_t		*_ccsp_default_scheduler	CACHELINE_ALIGN = NULL;
#else
sched_t			*_ccsp_scheduler 		CACHELINE_ALIGN = NULL;
#endif 

ccsp_global_t		_ccsp				CACHELINE_ALIGN = {};

void 			**_ccsp_calltable		CACHELINE_ALIGN = NULL;
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

/*{{{  architecture independent kernel call macros */
#define K_CALL_PARAMS_0() \
	K_CALL_HEADER
#define K_CALL_PARAMS_1(A) \
	K_CALL_HEADER \
	do { \
		(A) = (typeof((A))) K_CALL_PARAM(0); \
	} while (0)
#define K_CALL_PARAMS_2(A,B) \
	K_CALL_HEADER \
	do { \
		(A) = (typeof((A))) K_CALL_PARAM(0); \
		(B) = (typeof((B))) K_CALL_PARAM(1); \
	} while (0)
#define K_CALL_PARAMS_3(A,B,C) \
	K_CALL_HEADER \
	do { \
		(A) = (typeof((A))) K_CALL_PARAM(0); \
		(B) = (typeof((B))) K_CALL_PARAM(1); \
		(C) = (typeof((C))) K_CALL_PARAM(2); \
	} while (0)
#define K_CALL_PARAMS_4(A,B,C,D) \
	K_CALL_HEADER \
	do { \
		(A) = (typeof((A))) K_CALL_PARAM(0); \
		(B) = (typeof((B))) K_CALL_PARAM(1); \
		(C) = (typeof((C))) K_CALL_PARAM(2); \
		(D) = (typeof((D))) K_CALL_PARAM(3); \
	} while (0)
#define K_CALL_PARAMS_5(A,B,C,D,E) \
	K_CALL_HEADER \
	do { \
		(A) = (typeof((A))) K_CALL_PARAM(0); \
		(B) = (typeof((B))) K_CALL_PARAM(1); \
		(C) = (typeof((C))) K_CALL_PARAM(2); \
		(D) = (typeof((D))) K_CALL_PARAM(3); \
		(E) = (typeof((E))) K_CALL_PARAM(4); \
	} while (0)
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
static void NO_RETURN REGPARM kernel_scheduler (sched_t *sched);
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

#elif defined(ENABLE_MP) && defined(RMOX_BUILD)

static int (*rmox_cpu_identifier)(void) = NULL;

/* called from RMoX code to provide a function that can
 * return a CPU identifier
 */
void set_cpu_identifier_fcn (int (*fcn)(void))
{
	rmox_cpu_identifier = fcn;
}

static int init_local_schedulers (void)
{
	int i;

	for (i=0; i<RMOX_MAX_CPUS; i++) {
		_ccsp_schedulers[i] = NULL;
	}
	rmox_cpu_identifier = NULL;

	return 0;
}

static inline void set_local_scheduler (sched_t *scheduler)
{
	if (!rmox_cpu_identifier) {
		_ccsp_default_scheduler = scheduler;
	} else {
		int cpuid = rmox_cpu_identifier ();

#if 0
		printk ("set_local_scheduler(): cpuid = %d, scheduler = %p\n", cpuid, scheduler);
#endif
		_ccsp_schedulers[cpuid] = scheduler;
	}
	return;
}

void set_sched_fromdefault (void)
{
	if (!rmox_cpu_identifier) {
		printk ("set_sched_fromdefault(): no CPU identifier code!\n");
	} else {
		int cpuid = rmox_cpu_identifier ();

#if 0
		printk ("set_sched_fromdefault(): cpuid = %d, scheduler = %p\n", cpuid, _ccsp_default_scheduler);
#endif
		_ccsp_schedulers[cpuid] = _ccsp_default_scheduler;
	}
}

inline sched_t *local_scheduler (void)
{
	if (!rmox_cpu_identifier) {
		return _ccsp_default_scheduler;
	} else {
		int cpuid = rmox_cpu_identifier ();
		sched_t *sch = _ccsp_schedulers[cpuid];

		if (!sch) {
			sch = _ccsp_default_scheduler;
		}
		return sch;
	}
}

#else	/* !USE_TLS, !(ENABLE_MP && USE_PTHREADS), !RMOX_BUILD */

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
/*{{{  static TRIVIAL void save_return (sched_t *sched, word *Wptr, unsigned int return_address)*/
static TRIVIAL void save_return (sched_t *sched, word *Wptr, unsigned int return_address)
{
	Wptr[Iptr] = (word) return_address;
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
	unsigned int n, targets;
	sched_t *s;
	
	if (!affinity) {
		targets = att_val (&enabled_threads);
	} else {
		targets = affinity & (att_val (&enabled_threads));
		
		if (unlikely (targets == 0)) {
			BMESSAGE (
				"impossible affinity detected: %08x (batch = %p).\n", 
				affinity, batch
			);
			ccsp_show_last_debug_insert ();
			ccsp_kernel_exit (1, 0);
		}
	}
	
	n = pick_random_bit (targets);
	s = schedulers[n];
	atomic_enqueue_to_runqueue (&(s->bmail), false, batch);
	weak_write_barrier ();
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
	unsigned int n, targets;
	sched_t *s;
	
	if (!affinity) {
		targets = att_val (&enabled_threads);
	} else {
		targets = affinity & (att_val (&enabled_threads));
	
		if (unlikely (targets == 0)) {
			BMESSAGE (
				"impossible affinity detected: %08x (Wptr = %p, Iptr = %p).\n", 
				affinity, Wptr, Wptr != NULL ? (void *) Wptr[Iptr] : 0
			);
			ccsp_show_last_debug_insert ();
			ccsp_kernel_exit (1, Wptr != NULL ? Wptr[Iptr] : 0);
		}
	}
	
	n = pick_random_bit (targets);
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
			kernel_scheduler (sched);
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
		kernel_scheduler (sched);
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
/*{{{  void kernel_panic(void) */
/* kernel panic function*/
void kernel_panic (void)
{
	BMESSAGE ("panic: called kernel function not implemented.\n");
	ccsp_show_last_debug_insert ();
	ccsp_kernel_exit (1, 0);
}
/*}}}*/
/*{{{  error_info */
/*
 *	Information supplied by the runtime system about an error.
 */
typedef struct {
	word info1;
	word info2;
	word filename_info;
	word proc_info;
} error_info;
/*}}}*/
/*{{{  static void print_error_location (const error_info *info) */
/*
 *	Print the " in PROC ... \n" part of an error message.
 */
static void print_error_location (const error_info *info)
{
	const char *filename_tab = (const char *) info->filename_info;
	const char *procname_tab = (const char *) info->proc_info;

	const char *filename, *procname;
	int line_num, file_num, proc_num;

	line_num = info->info1 & 0xffff;

	file_num = (info->info2 >> 16) & 0xffff;
	if (file_num < 0 || file_num > ((const int *) filename_tab)[0]) {
		filename = NULL;
	} else {
		filename = filename_tab + ((const int *) filename_tab)[file_num + 1];
	}

	proc_num = info->info2 & 0xffff;
	if (file_num < 0 || proc_num > ((const int *) procname_tab)[0]) {
		procname = NULL;
	} else {
		procname = procname_tab + ((const int *) procname_tab)[proc_num + 1];
	}

	MESSAGE (" in PROC \"%s\" in file \"%s\" line %d\n",
	         (procname == NULL) ? "<unknown>" : procname,
	         (filename == NULL) ? "<unknown>" : filename,
	         line_num);
}
/*}}}*/
/*{{{  void handle_zerodiv_error (const error_info *info) */
/*
 *	Handle integer divide-by-zero error.
 */
static void handle_zerodiv_error (const error_info *info)
{
	BMESSAGE ("divide-by-zero error");
	print_error_location (info);
}
/*}}}*/
/*{{{  static void handle_overflow_error (const error_info *info) */
/*
 *	Handle integer overflow error.
 */
static void handle_overflow_error (const error_info *info)
{
	static const char *overflow_ops[] = {
		"INVALID",
		"add",
		"subtract",
		"multiply",
		"divide",
		"modulo",
		"long-add",
		"long-subtract",
		"add-constant",
		"STOP",
		"long-divide",
		"fractional-multiply",
		"fp-check-int32",
		"fp-check-int64"
	};
	int op_num;

	op_num = (info->info1 >> 24) & 0xff;
	if (op_num < 0 || op_num >= CONST_ARRAY_LENGTH (overflow_ops)) {
		op_num = 0;
	}

	BMESSAGE ("overflow error during %s operation", overflow_ops[op_num]);
	print_error_location (info);
}
/*}}}*/
/*{{{  void handle_fp_error (const error_info *info, word fpu_status) */
/*
 *	Handle floating-point error.
 */
static void handle_fp_error (const error_info *info, word fpu_status)
{
	static const char *fpu_errors[] = {
		"invalid operation",
		"denormalised operand",
		"divide by zero",
		"overflow",
		"underflow",
		"inexact result"
	};
	int i;

	BMESSAGE ("floating-point error (");
	fpu_status &= 0x3f;
	for (i = 0; i < CONST_ARRAY_LENGTH (fpu_errors); i++) {
		if (fpu_status & (1 << i)) {
			MESSAGE ("%s", fpu_errors[i]);
			fpu_status &= ~(1 << i);
			if (fpu_status) {
				MESSAGE (", ");
			}
		}
	}
	MESSAGE (")");
	print_error_location (info);
}
/*}}}*/
/*{{{  void handle_range_error (const error_info *info) */
/*
 *	Handle range error.
 */
static void handle_range_error (const error_info *info)
{
	static const char *range_ops[] = {
		"INVALID",
		"shift",
		"CSNGL",
		"CSUB0",
		"CCNT1",
		"CWORD"
	};
	int op_num, rt_bits;

	rt_bits = (info->info1 >> 16) & 0xff;
	if (rt_bits != 0xff) {
		BMESSAGE ("range error (debug data incorrect - rt_bits=%04x)\n", rt_bits);
		return;
	}

	op_num = (info->info1 >> 24) & 0xff;
	if (op_num < 0 || op_num >= CONST_ARRAY_LENGTH (range_ops)) {
		op_num = 0;
	}

	BMESSAGE ("range error during %s operation", range_ops[op_num]);
	print_error_location (info);
}
/*}}}*/
/*{{{  static void handle_seterr (const error_info *info) */
/*
 *	Handle general (SETERR) errors.
 */
static void handle_seterr (const error_info *info)
{
	bool occam_mode;
	int rt_bits;

	/* XXX: It would be nicer if errors always set these bits consistently. */
	rt_bits = (info->info1 >> 16) & 0xffff;
	if (rt_bits == 0xfb00) {
		occam_mode = true;
	} else if ((rt_bits & 0xff00) == 0xfe00) {
		occam_mode = false;
	} else {
		BMESSAGE ("error (debug data incorrect - rt_bits=%04x)\n", rt_bits);
		return;
	}
	
	BMESSAGE("error");
	if (occam_mode) {
		print_error_location (info);
	} else {
		BMESSAGE ("error in file \"%s\" line %d\n",
		          (char *) info->filename_info,
		          info->info1 & 0xffffff);
	}
}
/*}}}*/
/*{{{  void ccsp_decode_debug_insert (sched_t *sched, int offset, const char **file, int *line)*/
/*
 *	Decode the current insert debugging information.
 *	Pass 0 as the offset for the current position; pass 2 for the position
 *	before the last PROC call.
 *	If no valid information is available, returns 0 as the line number.
 */
void ccsp_decode_debug_insert (int offset, const char **filename, int *line)
{
	const sched_t *sched = _local_scheduler;
	const word *info;

	if (sched == NULL) {
		*line = 0;
		*filename = "no scheduler";
		return;
	}

	info = &(sched->mdparam[offset]);
	if (info[0] == 0xffffffff || info[1] == 0xffffffff
		   || info[0] == 0 || info[1] == 0) {
		*line = 0;
		*filename = "(no debugging information recorded)";
	} else {
		const char *file_tab = (const char *) info[1];
		int file_num = (info[0] >> 16) & 0xffff;

		if (file_num < 0 || file_num >= ((const int *) file_tab)[0]) {
			*line = 0;
			*filename = "(bad file number in debugging information)";
		} else {
			*line = info[0] & 0xffff;
			*filename = file_tab + ((const int *) file_tab)[file_num + 1];
		}
	}
}
/*}}}*/
/*{{{  void ccsp_show_last_debug_insert (void)*/
/* ccsp_show_last_debug_insert*/
/*
 *	Show any debug insert information that's currently available.
 */
void ccsp_show_last_debug_insert (void)
{
	const char *filename;
	int line_num;

	ccsp_decode_debug_insert (0, &filename, &line_num);
	if (line_num != 0) {
		BMESSAGE ("last debug position was in file \"%s\" near line %d\n", filename, line_num);
	}

	ccsp_decode_debug_insert (2, &filename, &line_num);
	if (line_num != 0) {
		BMESSAGE ("last position before CALL was in file \"%s\" near line %d\n", filename, line_num);
	}
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
	/* initialise run-time */
	init_ccsp_global_t (&_ccsp);
	build_calltable (_ccsp.calltable);
	_ccsp_calltable = _ccsp.calltable;
	init_local_schedulers ();

}
/*}}}*/

/*{{{  scheduler */
/*{{{  void kernel_scheduler (void)*/
static void NO_RETURN REGPARM kernel_scheduler (sched_t *sched)
{
	word *Wptr = NotProcess_p;
	
	ENTRY_TRACE (scheduler, "sync=%d", att_val (&(sched->sync)));
	
	do {
		if (unlikely (att_val (&(sched->sync)))) {
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

	K_ZERO_OUT_JRET ();
	
	no_return ();
}
/*}}}*/
/*{{{  void kernel_Y_fastscheduler (void)*/
/*
 *	fast scheduler entry point -- doesn't enqueue us, just runs next process
 *
 *	@SYMBOL:	Y_fastscheduler
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_FASTSCHEDULER
 *	@PRIO:		50
 */
K_CALL_DEFINE_0_0 (Y_fastscheduler)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (Y_fastscheduler);

	Wptr = get_process_or_reschedule (sched); 

	K_ZERO_OUT_JRET ();
}
/*}}}*/
/*{{{  void kernel_Y_occscheduler (void)*/
/*
 *	@SYMBOL:	Y_occscheduler
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_OCCSCHEDULER
 *	@PRIO:		50
 */
K_CALL_DEFINE_0_0 (Y_occscheduler)
{
	K_CALL_PARAMS_0 ();
	kernel_scheduler (sched);
}
/*}}}*/
/*{{{  void kernel_Y_rtthreadinit (void)*/
/*
 *	call-in for a run-time thread as it starts up
 *
 *	@SYMBOL:	Y_rtthreadinit
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_RTTHREADINIT
 *	@PRIO:		10
 */
K_CALL_DEFINE_1_0 (Y_rtthreadinit)
{
	K_CALL_HEADER;

	unsigned int stack = K_CALL_PARAM(0);
	word *fptr = (word *) sched;
	void *allocator;
	word i, tried;
	
	ENTRY_TRACE (Y_rtthreadinit, "(%08x)", att_val (&enabled_threads));
#if 0
BMESSAGE0 ("Y_rtthreadinit()\n");
#endif

	sched			= (sched_t *) stack;
	allocator 		= dmem_new_allocator ();
	init_sched_t (sched);
	memcpy (sched->calltable, ccsp_calltable, sizeof(void *) * K_MAX_SUPPORTED);
	sched->allocator 	= allocator;
	sched->stack		= stack;
	sched->priofinity	= BuildPriofinity (0, (MAX_PRIORITY_LEVELS / 2));
	
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
		kernel_scheduler (sched);
	}
}
/*}}}*/
/*{{{  void kernel_Y_shutdown (void)*/
/*
 *	@SYMBOL:	Y_shutdown
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_SHUTDOWN
 *	@PRIO:		10
 */
K_CALL_DEFINE_0_0 (Y_shutdown)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE (Y_shutdown, "");
	att_set (&(ccsp_shutdown), true);
	kernel_scheduler (sched);
}
/*}}}*/
/*}}}*/
/*{{{  error entry-points */
/*{{{  static void kernel_common_error (...) */
static void kernel_common_error (word *Wptr, sched_t *sched, unsigned int return_address, char *name)
{
#if defined(DYNAMIC_PROCS) && !defined(RMOX_BUILD)
	d_process *kr_dptr;
#endif
#if defined(DYNAMIC_PROCS) && !defined(RMOX_BUILD)
	/* check for faulting dynamic proc */
	if (faulting_dynproc ((word **)&(Wptr), &return_address, name, &kr_dptr)) {
		BMESSAGE ("dynamic process generated a fault, killed it.\n");
		switch_priofinity (sched, kr_dptr->holding_priofinity);
		K_ZERO_OUT_JUMP (return_address);
	} else
#endif
	{
		if (ccsp_ignore_errors) {
			kernel_scheduler (sched);
		} else {
#if defined(RMOX_BUILD)
			BMESSAGE ("application error, stopped. (%s)\n", name ?: "(unknown)");
#else
			BMESSAGE ("application error, stopped.\n");
#endif
			ccsp_kernel_exit (1, return_address);
		}
	}
}
/*}}}*/
/*{{{  void kernel_Y_zero_div (void)*/
/*
 *	entry point for integer division-by-zero
 *
 *	@SYMBOL:	Y_zero_div
 *	@INPUT:		4
 *	@OUTPUT: 	0
 *	@CALL: 		K_ZERODIV
 *	@PRIO:		0
 */
K_CALL_DEFINE_4_0 (Y_zero_div)
{
	error_info info;

	K_CALL_PARAMS_4 (info.info2, info.info1, info.proc_info, info.filename_info);
	
	handle_zerodiv_error (&info);
	kernel_common_error (Wptr, sched, return_address, "zero_div");
}
/*}}}*/
/*{{{  void kernel_Y_overflow (void)*/
/*
 *	arithmetic overflow entry point
 *
 *	@SYMBOL:	Y_overflow
 *	@INPUT:		4
 *	@OUTPUT: 	0
 *	@CALL: 		K_OVERFLOW
 *	@PRIO:		0
 */
K_CALL_DEFINE_4_0 (Y_overflow)
{
	error_info info;

	K_CALL_PARAMS_4 (info.info2, info.info1, info.proc_info, info.filename_info);

	handle_overflow_error (&info);
	kernel_common_error (Wptr, sched, return_address, "overflow");
}
/*}}}*/
/*{{{  void kernel_Y_floaterr (void)*/
/*
 *	floating-point error entry point
 *
 *	@SYMBOL:	Y_floaterr
 *	@INPUT:		5
 *	@OUTPUT: 	0
 *	@CALL: 		K_FLOATERR
 *	@PRIO:		0
 */
K_CALL_DEFINE_5_0 (Y_floaterr)
{
	error_info info;
	word fpu_status;

	K_CALL_PARAMS_5 (info.info2, info.info1, info.proc_info, info.filename_info, fpu_status);
	
	handle_fp_error (&info, fpu_status);
	kernel_common_error (Wptr, sched, return_address, "floaterr");
}
/*}}}*/
/*{{{  void kernel_Y_Seterr (void)*/
/*
 *	SETERR entry-point
 *
 *	@SYMBOL:	Y_Seterr
 *	@INPUT:		4
 *	@OUTPUT: 	0
 *	@CALL: 		K_SETERR
 *	@PRIO:		0
 */
K_CALL_DEFINE_4_0 (Y_Seterr)
{
	error_info info;

	K_CALL_PARAMS_4 (info.info2, info.info1, info.proc_info, info.filename_info);
	ENTRY_TRACE (Y_Seterr, "%p, %p, %x, %x", (void *)info.proc_info, (void *)info.filename_info, info.info2, info.info1);
	
	handle_seterr (&info);
	kernel_common_error (Wptr, sched, return_address, "Seterr");
}
/*}}}*/
/*{{{  void kernel_Y_BSeterr (void)*/
/*
 *	basic SETERR entry-point
 *
 *	@SYMBOL:	Y_BSeterr
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_BSETERR
 *	@PRIO:		0
 */
K_CALL_DEFINE_0_0 (Y_BSeterr)
{
	K_CALL_PARAMS_0 ();

	ENTRY_TRACE0 (Y_BSeterr);
	
	kernel_common_error (Wptr, sched, return_address, "BSeterr");
}
/*}}}*/
/*{{{  void kernel_Y_BNSeterr (void)*/
/*
 *	basic SETERR entry-point, but no return address expected
 *
 *	@SYMBOL:	Y_BNSeterr
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_BNSETERR
 *	@PRIO:		0
 */
K_CALL_DEFINE_0_0 (Y_BNSeterr)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (Y_BNSeterr);

	kernel_common_error (Wptr, sched, 0, "BNSeterr");
}
/*}}}*/
/*{{{  void kernel_Y_RangeCheckError (void)*/
/*
 *	Range error entry-point
 *
 *	@SYMBOL:	Y_RangeCheckError
 *	@INPUT:		4
 *	@OUTPUT: 	0
 *	@CALL: 		K_RANGERR
 *	@PRIO:		0
 */
K_CALL_DEFINE_4_0 (Y_RangeCheckError)
{
	error_info info;

	K_CALL_PARAMS_4 (info.info2, info.info1, info.proc_info, info.filename_info);
	ENTRY_TRACE (Y_RangeCheckError, "%p, %p, %x, %x", (void *)info.proc_info, (void *)info.filename_info, info.info2, info.info1);

	handle_range_error (&info);
	kernel_common_error (Wptr, sched, return_address, "RangeCheckError");
}
/*}}}*/
/*{{{  void kernel_Y_BasicRangeError (void)*/
/*
 *	basic range error entry point
 *
 *	@SYMBOL:	Y_BasicRangeError
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_BRANGERR
 *	@PRIO:		0
 */
K_CALL_DEFINE_0_0 (Y_BasicRangeError)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (Y_BasicRangeError);

	BMESSAGE ("range error.\n");
	kernel_common_error (Wptr, sched, return_address, "BasicRangeError");
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
}
/*}}}*/
#if defined(ENABLE_DTRACES) && !defined(RMOX_BUILD)
/*{{{  void kernel_Y_dtrace (void)*/
/*
 *	this handles debugging traces generated by tranx86
 *
 *	@SYMBOL:	X_dtrace
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_DTRACE
 *	@PRIO:		0
 *	@DEPEND:	ENABLE_DTRACES
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
K_CALL_DEFINE_2_0 (Y_dtrace)
{
	unsigned int trapval_A, trapval_B, trapval_C;
	
	K_CALL_PARAMS_2 (trapval_A, trapval_B);
	ENTRY_TRACE (Y_dtrace, "0x8.8x, 0x8.8x", trapval_A, trapval_B);

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
 *	@INPUT:		3
 *	@OUTPUT: 	3
 *	@CALL: 		K_TRAP
 *	@PRIO:		0
 */
K_CALL_DEFINE_3_3 (X_trap)
{
	unsigned int trapval_A, trapval_B, trapval_C;
	
	K_CALL_PARAMS_3 (trapval_A, trapval_B, trapval_C);
	ENTRY_TRACE (X_trap, "0x%x, 0x%x, 0x%x", trapval_A, trapval_B, trapval_C);
	
	dump_trap_info (Wptr, sched->curb.Fptr, sched->curb.Bptr, return_address, trapval_A, trapval_B, trapval_C);

	K_THREE_OUT (trapval_A, trapval_B, trapval_C);
}
/*}}}*/
/*{{{  void kernel_Y_unsupported (void)*/
/*
 *	this handles unsupported kernel calls
 *
 *	@SYMBOL:	Y_unsupported
 *	@INPUT:		0
 *	@OUTPUT: 	0
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
K_CALL_DEFINE_0_0 (Y_unsupported)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (Y_unsupported);

	if (ccsp_ignore_errors) {
		kernel_scheduler (sched);
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
/*{{{  static INLINE void sem_claim (sched_t *sched, word *Wptr, unsigned int return_address, ccsp_sem_t *sem)*/
static INLINE void sem_claim (sched_t *sched, word *Wptr, unsigned int return_address, ccsp_sem_t *sem)
{
	word *ptr, val;

	if ((val = atw_val (&(sem->bptr))) == (NotProcess_p | 1)) {
		if (atw_cas (&(sem->bptr), val, NotProcess_p)) {
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
	save_return (sched, Wptr, return_address);
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

	kernel_scheduler (sched);
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
static REGPARM void bar_complete (sched_t *sched, bar_t *bar, word tag)
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
static REGPARM void bar_enroll (sched_t *sched, bar_t *bar, word count)
{
	atw_add (&(bar->state), count);
}
/*}}}*/
/*{{{  static void bar_resign (sched_t *sched, bar_t *bar, word resign_count)*/
static REGPARM void bar_resign (sched_t *sched, bar_t *bar, word resign_count)
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
static REGPARM void bar_sync (sched_t *sched, bar_t *bar, word *Wptr)
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

		if (unlikely ((state & (~BAR_TAG_MASK)) == 1)) {
			tag = BAR_NEXT_TAG (tag);

			if (atw_cas (&(bar->state), state, BAR_STATE (1, tag, BAR_SYNCING))) {
				bar_complete (sched, bar, tag);
				kernel_scheduler (sched);
				return;
			}
		} else {
			word count = BAR_COUNT(state);
			word new_state = BAR_STATE (count - 1, tag, state & BAR_SYNCING);
			if (atw_cas (&(bar->state), state, new_state))  {
				kernel_scheduler (sched);
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
static REGPARM void fork_bar_complete (sched_t *sched, word *bar)
{
	word *Wptr = (word *) atw_val (bar);

	if (Wptr != NotProcess_p) {
		enqueue_process (sched, Wptr);
	}
}
/*}}}*/
/*{{{  static void fork_bar_enroll (sched_t *sched, word *bar, word count)*/
static REGPARM void fork_bar_enroll (sched_t *sched, word *bar, word count)
{
	/* no operation */
}
/*}}}*/
/*{{{  static void fork_bar_resign (sched_t *sched, word *bar, word count)*/
static REGPARM void fork_bar_resign (sched_t *sched, word *bar, word count)
{
	/* no operation */
}
/*}}}*/
/*{{{  static void fork_bar_sync (sched_t *sched, word *bar, word *Wptr)*/
static REGPARM void fork_bar_sync (sched_t *sched, word *bar, word *Wptr)
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
		kernel_scheduler (sched);
	}
}
/*}}}*/
/*}}}*/
/*{{{  mobile process barrier operations */
#if !defined(RMOX_BUILD) && defined(DYNAMIC_PROCS)
/*{{{  static void mproc_bar_init (mproc_bar_t *bar, word initial_count)*/
static REGPARM void mproc_bar_init (mproc_bar_t *bar, word initial_count)
{
	ASSERT (initial_count > 0);
	bar->enrolled	= initial_count;
	bar->state	= initial_count - 1;
	bar->fptr	= NotProcess_p;
	bar->bptr	= NotProcess_p;
}
/*}}}*/
/*{{{  static void mproc_bar_complete (sched_t *sched, mproc_bar_t *bar)*/
static REGPARM void mproc_bar_complete (sched_t *sched, mproc_bar_t *bar)
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
static REGPARM void mproc_bar_enroll (sched_t *sched, mproc_bar_t *bar, word count)
{
	atw_add (&(bar->enrolled), count);
	atw_add (&(bar->state), count);
}
/*}}}*/
/*{{{  static void mproc_bar_resign (sched_t *sched, mproc_bar_t *bar, word count)*/
static REGPARM void mproc_bar_resign (sched_t *sched, mproc_bar_t *bar, word count)
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
static REGPARM void mproc_bar_sync (sched_t *sched, mproc_bar_t *bar, word *Wptr)
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

			save_priofinity (sched, Wptr);

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
				kernel_scheduler (sched);
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
/*{{{ mobile_type_error(), mobile_type_error_type() */
#define mobile_type_error() \
	do { \
		BMESSAGE ("mobile typing error (%s:%d)\n", __FILE__, __LINE__); \
		ccsp_kernel_exit (1, 0); \
	} while (0)
#define mobile_type_error_type(X) \
	do { \
		BMESSAGE ("mobile typing error (%s:%d), type 0x%8.8x\n", __FILE__, __LINE__, (unsigned int)(X)); \
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

	#ifdef RMOX_BUILD
	meta_words += 1; /* always allocate space for DMA pointer on RMoX */
	#endif

	ASSERT ( inner_type & MT_SIMPLE );

	if (MT_TYPE(inner_type) == MT_ARRAY_OPTS) {
		if (MT_FLAGS(inner_type) & MT_ARRAY_OPTS_DMA) {
			/* allocate space for hardware address */
			dma = dimensions;
			#ifndef RMOX_BUILD
			meta_words += 1; /* only needed for non-RMoX */
			#endif
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
#if 0
fprintf (stderr, "mt_alloc_array(): here, size = %d, type=0x%8.8x, ptr = %p\n",
		(int)size, (unsigned int)type, ma);
fprintf (stderr, " ditto()        : ptr = 0x%8.8x, size = 0x%8.8x\n",
		((unsigned int *)ma)[0], ((unsigned int *)ma)[1]);
#endif

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
		words += sizeof(mt_cb_pony_state_t) / sizeof(word);
	}
	type |= channels << MT_CB_CHANNELS_SHIFT;

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

	if (type & MT_CB_STATE_SPACE) {
		mt_cb_pony_state_t *pony = (mt_cb_pony_state_t *) &(cb->channels[channels]);

		pony->typedesc	= NULL;
		pony->uiohook	= NULL;
		pony->state	= 0;
		sem_init (&(pony->statesem));
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
				
				if (unlikely (sched == NULL))
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

	if (expect (type & MT_SIMPLE, MT_SIMPLE)) {
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
			mobile_type_error_type (MT_TYPE (type));
			return NULL;
	}
}
/*}}}*/
/*{{{  static word *mt_clone (sched_t *sched, word *ptr)*/
static word *mt_clone (sched_t *sched, word *ptr)
{
	word type = ptr[MTType];

	if (expect (type & MT_SIMPLE, MT_SIMPLE)) {
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
/*{{{  static HOT bool mt_io_update (sched_t *sched, word **pptr)*/
static void mt_io_update_array (sched_t *sched, word **pptr, word inner);
static HOT bool mt_io_update (sched_t *sched, word **pptr)
{
	word type = (*pptr)[MTType];

	if (expect (type & MT_SIMPLE, MT_SIMPLE)) {
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
/*{{{  static INLINE void mt_io_update_array (sched_t *sched, void *ptr, word inner)*/
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
/*{{{  static HOT word *mt_alloc (void *allocator, word type, word size)*/
static HOT word *mt_alloc (void *allocator, word type, word size)
{
	if (expect (type & MT_SIMPLE, MT_SIMPLE)) {
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
	
	kernel_scheduler (sched);
}
/*}}}*/
/*{{{  void kernel_Y_b_dispatch (void)*/
/*
 *	dispatches a blocking call
 *
 *	@SYMBOL:	Y_b_dispatch
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_B_DISPATCH
 *	@PRIO:		20
 *	@DEPEND:	BLOCKING_SYSCALLS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
K_CALL_DEFINE_2_0 (Y_b_dispatch)
{
	void *b_func, *b_param;
	
	K_CALL_PARAMS_2 (b_func, b_param);
	ENTRY_TRACE (Y_b_dispatch, "%p, %p", (void *)b_func, (void *)b_param);

	kernel_bsc_dispatch (sched, return_address, Wptr, b_func, b_param, 0);
}
/*}}}*/
/*{{{  void kernel_Y_bx_dispatch (void)*/
/*
 *	dispatches a terminateable blocking call
 *
 *	@SYMBOL:	Y_bx_dispatch
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_BX_DISPATCH
 *	@PRIO:		20
 *	@DEPEND:	BLOCKING_SYSCALLS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
K_CALL_DEFINE_2_0 (Y_bx_dispatch)
{
	void *b_func, *b_param;
	
	K_CALL_PARAMS_2 (b_func, b_param);
	ENTRY_TRACE (Y_bx_dispatch, "%p, %p", (void *)b_func, (void *)b_param);

	kernel_bsc_dispatch (sched, return_address, Wptr, b_func, b_param, 1);
}
/*}}}*/
/*{{{  void kernel_X_bx_kill (void)*/
/*
 *	kills a terminateable blocking call
 *
 *	@SYMBOL:	X_bx_kill
 *	@INPUT:		1
 *	@OUTPUT: 	1
 *	@CALL: 		K_BX_KILL
 *	@PRIO:		20
 *	@DEPEND:	BLOCKING_SYSCALLS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
K_CALL_DEFINE_1_1 (X_bx_kill)
{
	word *ptr;
	int result;
	
	K_CALL_PARAMS_1 (ptr);
	ENTRY_TRACE (X_bx_kill, "%p", ptr);

	result = bsyscall_kill (ptr);

	K_ONE_OUT (result);
}
/*}}}*/
#endif	/* RMOX_BUILD || !BLOCKING_SYSCALLS */
/*}}}*/
/*{{{  memory allocation */
/*{{{  void kernel_X_mt_alloc (void)*/
/*
 *	allocates a new mobile type and initialises it
 *
 *	@SYMBOL:	X_mt_alloc
 *	@INPUT:		2
 *	@OUTPUT: 	1
 *	@CALL: 		K_MT_ALLOC
 *	@PRIO:		100
 */
K_CALL_DEFINE_2_1 (X_mt_alloc)
{
	word *ptr, type, size;
	
	K_CALL_PARAMS_2 (type, size);
	ENTRY_TRACE (X_mt_alloc, "%08x %d", type, size);

	ptr = mt_alloc (sched->allocator, type, size);

	K_ONE_OUT (ptr);
}
/*}}}*/
/*{{{  void kernel_X_mt_release (void)*/
/*
 *	frees a mobile type
 *
 *	@SYMBOL:	X_mt_release
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_MT_RELEASE
 *	@PRIO:		100
 */
K_CALL_DEFINE_1_0 (X_mt_release)
{
	word *ptr;
	
	K_CALL_PARAMS_1 (ptr);
	ENTRY_TRACE (X_mt_release, "%p", ptr);

	mt_release (sched, ptr);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_mt_clone (void)*/
/*
 *	clones a mobile type
 *
 *	@SYMBOL:	X_mt_clone
 *	@INPUT:		1
 *	@OUTPUT: 	1
 *	@CALL: 		K_MT_CLONE
 *	@PRIO:		100
 */
K_CALL_DEFINE_1_1 (X_mt_clone)
{
	word *dst, *src;
	
	K_CALL_PARAMS_1 (src);
	ENTRY_TRACE (X_mt_clone, "%p", src);

	dst = mt_clone (sched, src);

	K_ONE_OUT (dst);
}
/*}}}*/
/*{{{  void kernel_X_mt_dclone (void)*/
/*
 *	clones some data into a new mobile type
 *
 *	@SYMBOL:	X_mt_dclone
 *	@INPUT:		3
 *	@OUTPUT: 	1
 *	@CALL: 		K_MT_DCLONE
 *	@PRIO:		60
 */
K_CALL_DEFINE_3_1 (X_mt_dclone)
{
	word bytes, *dst, type;
	void *src;
	
	K_CALL_PARAMS_3 (type, bytes, src);
	ENTRY_TRACE (X_mt_dclone, "%08x, %d, %p", type, bytes, src);
	
	if (bytes && (type == (MT_SIMPLE | MT_MAKE_TYPE (MT_DATA)))) {
		dst = mt_alloc_data (sched->allocator, type, bytes);
		memcpy (dst, src, bytes);
	} else {
		if (unlikely (bytes)) {
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
 *	@INPUT:		1
 *	@OUTPUT: 	1
 *	@CALL: 		K_MALLOC
 *	@PRIO:		110
 */
K_CALL_DEFINE_1_1 (X_malloc)
{
	word *ptr, size;
	
	K_CALL_PARAMS_1 (size);
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
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_MRELEASE
 *	@PRIO:		110
 */
K_CALL_DEFINE_1_0 (X_mrelease)
{
	word *ptr;
	
	K_CALL_PARAMS_1 (ptr);
	ENTRY_TRACE (X_mrelease, "%p", ptr);

	mt_release_simple (sched, ptr, MT_MAKE_TYPE (MT_DATA));

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_mt_bind (void)*/
/*
 *	bind a mobile type in some way to a bit of data
 *
 *	@SYMBOL:	X_mt_bind
 *	@INPUT:		3
 *	@OUTPUT: 	1
 *	@CALL: 		K_MT_BIND
 *	@PRIO:		50
 */
K_CALL_DEFINE_3_1 (X_mt_bind)
{
	word bind_type, *data, *ptr, type;
	
	K_CALL_PARAMS_3 (bind_type, ptr, data);
	ENTRY_TRACE (X_mt_bind, "%08x %p %p", bind_type, ptr, data);

	ASSERT (ptr != NULL);

	type = ptr[MTType];
	if ((type & MT_SIMPLE) && (MT_TYPE(type) == MT_ARRAY)) {
		mt_array_internal_t *ma = (mt_array_internal_t *) (ptr - MT_ARRAY_PTR_OFFSET);
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
			
			if (MT_TYPE(MT_ARRAY_INNER_TYPE(type)) == MT_ARRAY_OPTS) {
				word flags = MT_FLAGS(MT_ARRAY_INNER_TYPE(type));
				
				if (flags & MT_ARRAY_OPTS_SEPARATED) {
					if (ma->array.data != NULL) {
						mt_release (sched, ma->array.data);
					}
				}

				if (flags & MT_ARRAY_OPTS_DMA) {
					ma->array.dimensions[dimensions] = (word) phys_addr;
				}
			}

			ma->array.data = virt_addr;
		} else if (bind_type == MT_BIND_DMA) {
			word align	= 0;
			word flags	= 0;
			word inner	= MT_ARRAY_INNER_TYPE(type);
			bool dma_ready;

			if (MT_TYPE(inner) == MT_ARRAY_OPTS) {
				if (MT_FLAGS(inner) & MT_ARRAY_OPTS_DMA) {
					/* already capable */
					K_ONE_OUT (ptr);
				}
				align = MT_ARRAY_OPTS_ALIGN(inner);
				flags = MT_FLAGS(inner);
				inner = MT_ARRAY_OPTS_INNER(inner);
			}
			
			#ifdef RMOX_BUILD
			{
				word low = (word) ma->array.data;
				word high = low + ma->size;
				if ((low | high) & 0x3fffffff) { /* FIXME: magic mask */
					dma_ready = true;
				} else {
					dma_ready = false;
				}
			}
			#else
			dma_ready = false;
			#endif

			if (dma_ready) {
				/* FIXME: translate */
				ma->type = MT_MAKE_ARRAY_TYPE (dimensions, MT_MAKE_ARRAY_OPTS (flags | MT_ARRAY_OPTS_DMA, align, inner));
				ma->array.dimensions[dimensions] = (word) ma->array.data;
			} else {
				word old_type = ma->type;
				word *new_ptr;

				ma->type = MT_MAKE_ARRAY_TYPE (dimensions, MT_MAKE_ARRAY_OPTS (flags | MT_ARRAY_OPTS_DMA, align, inner));
				new_ptr = mt_clone (sched, ptr);
				ma->type = old_type;
				mt_release (sched, ptr);
				ptr = new_ptr;
			}
		} else {
			mobile_type_error ();
		}
	} else {
		mobile_type_error ();
	}

	K_ONE_OUT (ptr);
}
/*}}}*/
/*{{{  void kernel_X_mt_resize (void)*/
/*
 *	resize a mobile type 
 *
 *	@SYMBOL:	X_mt_resize
 *	@INPUT:		3
 *	@OUTPUT: 	1
 *	@CALL: 		K_MT_RESIZE
 *	@PRIO:		50
 */
K_CALL_DEFINE_3_1 (X_mt_resize)
{
	word arg, *ptr, resize_type;
	
	K_CALL_PARAMS_3 (resize_type, ptr, arg);
	ENTRY_TRACE (X_mt_resize, "%08x %p %08x", resize, ptr, arg);

	if ((resize_type == MT_RESIZE_DATA) && (ptr != NULL)) {
		word new_size	= arg;
		word type	= ptr[MTType];

		if ((type & MT_SIMPLE) && (MT_TYPE(type) == MT_ARRAY)) {
			mt_array_internal_t *ma = (mt_array_internal_t *) (ptr - MT_ARRAY_PTR_OFFSET);
			word inner_type		= MT_ARRAY_INNER_TYPE(type);

			if (MT_TYPE(inner_type) == MT_ARRAY_OPTS) {
				inner_type = MT_ARRAY_OPTS_INNER(inner_type);
			}

			/* Reallocate the array if it needs to grow, or if it
			 * shrinks to less than 50% of the allocated memory.
			 */
			if ((ma->size < new_size) || (new_size < (ma->size / 2))) {
				word dimensions	= MT_ARRAY_DIM(type);
				mt_array_internal_t *new;
				word i, size_shift, count;
				
				new = mt_alloc_array_internal (
					sched->allocator, type, new_size, false, &size_shift
				);
				count = ma->size < new->size ? ma->size : new->size;

				for (i = 0; i < dimensions; ++i) {
					new->array.dimensions[i] = ma->array.dimensions[i];
				}

				if (MT_TYPE(inner_type) != MT_NUM) {
					word **dst = (word **) new->array.data;
					word **src = (word **) ma->array.data;

					while (count--) {
						*(dst++) = *src;
						*(src++) = NULL;
					}
					
					if (new->size > ma->size) {
						count = new->size - ma->size;
						while (count--) {
							*(dst++) = NULL;
						}
					}
				} else if (count > 0) {
					memcpy (
						new->array.data,
						ma->array.data, 
						count << size_shift
					);
				}

				mt_release_simple (sched->allocator, ptr, type);
				ptr = ((word *) new) + MT_ARRAY_PTR_OFFSET;
			} else if (ma->size > new_size) {
				if (MT_TYPE(inner_type) != MT_NUM) {
					word **data 	= ((word **) ma->array.data) + new_size;
					word count 	= ma->size - new_size;
					while (count--) {
						word *p = *data;
						if (p != NULL) {
							mt_release (sched->allocator, p);
							*data = NULL;
						}
						data++;
					}
				}
			}
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
 *	@INPUT:		2
 *	@OUTPUT: 	3
 *	@CALL: 		K_NORM
 *	@PRIO:		50
 */
K_CALL_DEFINE_2_3 (X_norm)
{
	unsigned int Areg, Breg, Creg;
	
	K_CALL_PARAMS_2 (Areg, Breg);
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
	
	K_THREE_OUT (Areg, Breg, Creg);
}
/*}}}*/
/*{{{  void kernel_X_fmul (void)*/
/*
 *	FMUL implementation (easier in C..)
 *
 *	@SYMBOL:	X_fmul
 *	@INPUT:		2
 *	@OUTPUT: 	1
 *	@CALL: 		K_FMUL
 *	@PRIO:		50
 */
K_CALL_DEFINE_2_1 (X_fmul)
{
	long long tmp_long;
	int hi_word, lo_word;
	int tmpint_a, tmpint_b, tmpint_c;
	
	K_CALL_PARAMS_2 (tmpint_a, tmpint_b);
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

	K_ONE_OUT (tmpint_c);
}
/*}}}*/
/*}}}*/

/*{{{  process control */
/*{{{  void kernel_Y_startp (void)*/
/*
 *	start process
 *
 *	@SYMBOL:	Y_startp
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_STARTP
 *	@PRIO:		90
 */
K_CALL_DEFINE_2_0 (Y_startp)
{
	unsigned int start_offset;
	word *workspace;

	K_CALL_PARAMS_2 (workspace, start_offset);
	ENTRY_TRACE (Y_startp, "%p, %d", workspace, start_offset);

	SAFETY {
		if (sched->curb.Fptr != NotProcess_p) {
			verify_batch_integrity (&(sched->curb));
		}
	}
	
	save_priofinity (sched, workspace);
	save_return (sched, workspace, return_address + start_offset);
	enqueue_process_nopri (sched, workspace);
	sched->stats.startp++;

	SAFETY { verify_batch_integrity (&(sched->curb)); }

	if ((--sched->dispatches) <= 0) {
		save_priofinity (sched, Wptr);
		save_return (sched, Wptr, return_address);
		enqueue_to_batch_front (&(sched->curb), Wptr);
		kernel_scheduler (sched);
	}
	
	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_runp (void)*/
/*
 *	run process (fast interface)
 *
 *	@SYMBOL:	X_runp
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_RUNP
 *	@PRIO:		80
 */
K_CALL_DEFINE_1_0 (X_runp)
{
	word *other_workspace;
	
	K_CALL_PARAMS_1 (other_workspace);
	ENTRY_TRACE (X_runp, "%p", other_workspace);

	other_workspace = (word *)(((word)other_workspace) & (~(sizeof(word) - 1)));
	enqueue_process (sched, other_workspace);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_pause (void)*/
/*
 *	reschedule
 *
 *	@SYMBOL:	Y_pause
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_PAUSE
 *	@PRIO:		80
 */
K_CALL_DEFINE_0_0 (Y_pause)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (Y_pause);

	save_priofinity (sched, Wptr);
	save_return (sched, Wptr, return_address);
	enqueue_process_nopri (sched, Wptr);
	
	kernel_scheduler (sched);
}
/*}}}*/
/*{{{  void kernel_Y_stopp (void)*/
/*
 *	stop process
 *
 *	@SYMBOL:	Y_stopp
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_STOPP
 *	@PRIO:		80
 */
K_CALL_DEFINE_0_0 (Y_stopp)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (Y_stopp);

	save_priofinity (sched, Wptr);
	save_return (sched, Wptr, return_address);

	kernel_scheduler (sched);
}
/*}}}*/
/*{{{  void kernel_Y_endp (void)*/
/*
 *	end process (alternate interface)
 *
 *	@SYMBOL:	Y_endp
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_ENDP
 *	@PRIO:		90
 */
K_CALL_DEFINE_1_0 (Y_endp)
{
	word *ptr;
	
	K_CALL_PARAMS_1 (ptr);
	ENTRY_TRACE (Y_endp, "%p", ptr);

	/* save the return address for CIF */
	save_return (sched, Wptr, return_address);

	if (atw_dec_z (&(ptr[Count]))) {
		ptr[Priofinity] = ptr[SavedPriority];
		ptr[Iptr]	= ptr[IptrSucc]; /* copy Iptr from top of workspace */

		enqueue_process (sched, ptr);
	} else {
		sched->stats.endp++;
	}

	kernel_scheduler (sched);
}
/*}}}*/
/*{{{  void kernel_X_par_enroll (void)*/
/*
 *	end process
 *
 *	@SYMBOL:	X_par_enroll
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_PAR_ENROLL
 *	@PRIO:		50
 */
K_CALL_DEFINE_2_0 (X_par_enroll)
{
	word count, *ptr;
	
	K_CALL_PARAMS_2 (count, ptr);
	ENTRY_TRACE (X_par_enroll, "%d %p", count, ptr);

	atw_add (&(ptr[Count]), count);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mreleasep (void)*/
/*
 *	frees a process whos workspace was allocated by X_malloc, first adjusting by the parameter
 *
 *	@SYMBOL:	Y_mreleasep
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_MRELEASEP
 *	@PRIO:		90
 */
K_CALL_DEFINE_1_0 (Y_mreleasep)
{
	word *ptr, adjust;
	
	K_CALL_PARAMS_1 (adjust);
	ENTRY_TRACE (Y_mreleasep, "%d", adjust);

	ptr = (word *)(((char *) Wptr) + (((signed int) adjust) * sizeof(word)));

	mt_release_simple (sched, ptr, MT_SIMPLE | MT_MAKE_TYPE (MT_DATA));
	
	TRACE { 
		sched->mdparam[14] = (word) Wptr;
		sched->mdparam[15] = (word) adjust;
	}

	kernel_scheduler (sched);
}
/*}}}*/
/*{{{  void kernel_X_proc_alloc (void)*/
/*
 *	allocate a process workspace
 *
 *	@SYMBOL:	X_proc_alloc
 *	@INPUT:		2
 *	@OUTPUT: 	1
 *	@CALL: 		K_PROC_ALLOC
 *	@PRIO:		90
 */
K_CALL_DEFINE_2_1 (X_proc_alloc)
{
	word flags, words, *ws;

	K_CALL_PARAMS_2 (flags, words);
	ENTRY_TRACE (X_proc_alloc, "%d, %d", flags, words);

	ws = mt_alloc_data (sched->allocator, MT_SIMPLE | MT_MAKE_TYPE (MT_DATA), words << WSH);
	
	K_ONE_OUT (ws);
}
/*}}}*/
/*{{{  void kernel_X_proc_param (void)*/
/*
 *	pass a param to workspace allocated via X_proc_alloc
 *
 *	@SYMBOL:	X_proc_param
 *	@INPUT:		3
 *	@OUTPUT: 	0
 *	@CALL: 		K_PROC_PARAM
 *	@PRIO:		90
 */
K_CALL_DEFINE_3_0 (X_proc_param)
{
	word offset, param, *ws;

	K_CALL_PARAMS_3 (offset, ws, param);
	ENTRY_TRACE (X_proc_param, "%d, %p, %08x", offset, ws, param);

	ws[offset] = param;

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_proc_mt_copy (void)*/
/*
 *	copy a mobile type to workspace allocated via X_proc_alloc
 *
 *	@SYMBOL:	X_proc_mt_copy
 *	@INPUT:		3
 *	@OUTPUT: 	0
 *	@CALL: 		K_PROC_MT_COPY
 *	@PRIO:		90
 */
K_CALL_DEFINE_3_0 (X_proc_mt_copy)
{
	word offset, *ptr, *ws;

	K_CALL_PARAMS_3 (offset, ws, ptr);
	ENTRY_TRACE (X_proc_mt_copy, "%d, %p, %p", offset, ws, ptr);

	if (ptr != NULL) {
		ws[offset] = (word) mt_clone (sched, ptr);
	} else {
		ws[offset] = (word) NULL;
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_proc_mt_move (void)*/
/*
 *	move a mobile type to workspace allocated via X_proc_alloc
 *
 *	@SYMBOL:	X_proc_mt_move
 *	@INPUT:		3
 *	@OUTPUT: 	0
 *	@CALL: 		K_PROC_MT_MOVE
 *	@PRIO:		90
 */
K_CALL_DEFINE_3_0 (X_proc_mt_move)
{
	word offset, *ptr, **pptr, *ws;

	K_CALL_PARAMS_3 (offset, ws, pptr);
	ENTRY_TRACE (X_proc_mt_move, "%d, %p, %p (%p)", offset, ws, pptr, (word *) *pptr);

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
 *	start a process using a workspace allocated via X_proc_alloc
 *
 *	@SYMBOL:	Y_proc_start
 *	@INPUT:		3
 *	@OUTPUT: 	0
 *	@CALL: 		K_PROC_START
 *	@PRIO:		90
 */
K_CALL_DEFINE_3_0 (Y_proc_start)
{
	word code, offset, *ws;

	K_CALL_PARAMS_3 (offset, ws, code);
	ENTRY_TRACE (Y_proc_start, "%d, %p, %08x", offset, ws, code);

	ws += offset;
	save_priofinity (sched, ws);
	save_return (sched, ws, code);

	enqueue_process_nopri (sched, ws);
	sched->stats.proc_start++;

	SAFETY { verify_batch_integrity (&(sched->curb)); }

	if ((--sched->dispatches) <= 0) {
		save_priofinity (sched, Wptr);
		save_return (sched, Wptr, return_address);
		enqueue_to_batch_front (&(sched->curb), Wptr);
		kernel_scheduler (sched);
	} else {
		K_ZERO_OUT ();
	}
}
/*}}}*/
/*{{{  void kernel_Y_proc_end (void)*/
/*
 *	called by a process started by Y_proc_start to terminate
 *
 *	@SYMBOL:	Y_proc_end
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_PROC_END
 *	@PRIO:		90
 */
K_CALL_DEFINE_1_0 (Y_proc_end)
{
	word *ws;

	K_CALL_PARAMS_1 (ws);
	ENTRY_TRACE (Y_proc_end, "%p", ws);

	mt_release_simple (sched, ws, MT_MAKE_TYPE (MT_DATA));
	sched->stats.proc_end++;

	kernel_scheduler (sched);
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
/*{{{  void kernel_X_getaff (void)*/
/*
 *	get processor affinity
 *
 *	@SYMBOL:	X_getaff
 *	@INPUT:		0
 *	@OUTPUT: 	1
 *	@CALL: 		K_GETAFF
 *	@PRIO:		30
 */
K_CALL_DEFINE_0_1 (X_getaff)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (X_getaff);
	K_ONE_OUT (PAffinity (sched->priofinity));
}
/*}}}*/
/*{{{  void kernel_Y_setaff (void)*/
/*
 *	set processor affinity
 *
 *	@SYMBOL:	Y_setaff
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_SETAFF
 *	@PRIO:		30
 */
K_CALL_DEFINE_1_0 (Y_setaff)
{
	unsigned int affinity;
	
	K_CALL_PARAMS_1 (affinity);
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

	if (affinity != PAffinity (sched->priofinity)) {
		Wptr[Priofinity] = BuildPriofinity (affinity, PPriority (sched->priofinity));
		save_return (sched, Wptr, return_address);
		enqueue_process (sched, Wptr);
		Wptr = get_process_or_reschedule (sched);
		K_ZERO_OUT_JRET ();
	} else {
		K_ZERO_OUT ();
	}
}
/*}}}*/
/*{{{  void kernel_X_getpas (void)*/
/*
 *	get current raw priofinity
 *
 *	@SYMBOL:	X_getpas
 *	@INPUT:		0
 *	@OUTPUT: 	1
 *	@CALL: 		K_GETPAS
 *	@PRIO:		80
 */
K_CALL_DEFINE_0_1 (X_getpas)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (X_getpas);
	K_ONE_OUT (sched->priofinity);
}
/*}}}*/
/*{{{  void kernel_X_getpri (void)*/
/*
 *	get process priority
 *
 *	@SYMBOL:	X_getpri
 *	@INPUT:		0
 *	@OUTPUT: 	1
 *	@CALL: 		K_GETPRI
 *	@PRIO:		30
 */
K_CALL_DEFINE_0_1 (X_getpri)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (X_getpri);
	K_ONE_OUT (PPriority (sched->priofinity));
}
/*}}}*/
/*{{{  void kernel_Y_setpri (void)*/
/*
 *	set process priority
 *
 *	@SYMBOL:	Y_setpri
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_SETPRI
 *	@PRIO:		30
 */
K_CALL_DEFINE_1_0 (Y_setpri)
{
	int priority;
	
	K_CALL_PARAMS_1 (priority);
	ENTRY_TRACE (Y_setpri, "%d", priority);

	if (priority < 0) {
		priority = 0;
	} else if (priority >= MAX_PRIORITY_LEVELS) {
		priority = MAX_PRIORITY_LEVELS - 1;
	}
	
	if (priority != PPriority (sched->priofinity)) {
		Wptr[Priofinity] = BuildPriofinity (PAffinity (sched->priofinity), priority);
		save_return (sched, Wptr, return_address);
		enqueue_process (sched, Wptr);
		Wptr = get_process_or_reschedule (sched);
		K_ZERO_OUT_JRET ();
	} else {
		K_ZERO_OUT ();
	}
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

		if (likely (ptr != NULL)) {
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
			kernel_scheduler (sched);
			return;
		} else if (temp & 1) {
			trigger_alt_guard (sched, temp);
			kernel_scheduler (sched);
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
K_CALL_DEFINE_2_0 (symbol)		\
{					\
	word *channel_address;		\
	byte *pointer;			\
					\
	K_CALL_PARAMS_2 (channel_address, pointer); \
	save_return (sched, Wptr, return_address); \
	kernel_chan_io ((flags), Wptr, sched, channel_address, pointer, count); \
}

#define BUILD_CHANNEL_COUNTED_IO(symbol,shift,flags) \
K_CALL_DEFINE_3_0 (symbol)		\
{					\
	word count, *channel_address;	\
	byte *pointer;			\
					\
	K_CALL_PARAMS_3 (count, channel_address, pointer); \
	if ((shift)) {			\
		count <<= (shift);	\
	}				\
	save_return (sched, Wptr, return_address); \
	kernel_chan_io ((flags), Wptr, sched, channel_address, pointer, count); \
}
/*}}}*/
/*{{{  Y_in8 */
/*
 *	@SYMBOL:	Y_in8
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_IN8
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_in8, 1, CIO_INPUT)
/*}}}*/
/*{{{  Y_in32 */
/*
 *	@SYMBOL:	Y_in32
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_IN32
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_in32, 4, CIO_INPUT)
/*}}}*/
/*{{{  Y_out8 */
/*
 *	@SYMBOL:	Y_out8
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_OUT8
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_out8, 1, CIO_OUTPUT)
/*}}}*/
/*{{{  Y_out32 */
/*
 *	@SYMBOL:	Y_out32
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_OUT32
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_out32, 4, CIO_OUTPUT)
/*}}}*/
/*{{{  Y_in */
/*
 *	@SYMBOL:	Y_in
 *	@INPUT:		3
 *	@OUTPUT: 	0
 *	@CALL: 		K_IN
 *	@PRIO:		110
 */
BUILD_CHANNEL_COUNTED_IO (Y_in, 0, CIO_INPUT)
/*}}}*/
/*{{{  Y_out */
/*
 *	@SYMBOL:	Y_out
 *	@INPUT:		3
 *	@OUTPUT: 	0
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
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_OUTBYTE
 *	@PRIO:		100
 */
K_CALL_DEFINE_2_0 (Y_outbyte)
{
	word *channel_address;
	byte *pointer;
	word value;

	K_CALL_PARAMS_2 (value, channel_address);

	save_return (sched, Wptr, return_address);

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
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_OUTWORD
 *	@PRIO:		100
 */
K_CALL_DEFINE_2_0 (Y_outword)
{
	word *channel_address;
	byte *pointer;
	word value;
	
	K_CALL_PARAMS_2 (value, channel_address);
	
	save_return (sched, Wptr, return_address);

	Wptr[0]	= value;
	pointer	= (byte *) Wptr;
	
	kernel_chan_io (CIO_OUTPUT, Wptr, sched, channel_address, pointer, sizeof (word));
}
/*}}}*/
/*{{{  void kernel_Y_xable (void)*/
/*
 *	called to synchronise with outputting process
 *	also works with an inputting process in the case where output ALTs are enabled
 *
 *	@SYMBOL:	Y_xable
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_XABLE
 *	@PRIO:		70
 */
K_CALL_DEFINE_1_0 (Y_xable)
{
	word *channel_address, temp;
	
	K_CALL_PARAMS_1 (channel_address);
	ENTRY_TRACE (Y_xable, "%p", channel_address);

	temp = atw_val (channel_address);

	if (temp == NotProcess_p || (temp & 1)) {
		atw_set (&(Wptr[State]), ALT_WAITING | 1);
		save_priofinity (sched, Wptr);
		save_return (sched, Wptr, return_address);
		weak_write_barrier ();

		temp = atw_swap (channel_address, ((word) Wptr) | 1);
		if (temp == NotProcess_p) {
			kernel_scheduler (sched);
			return;
		} else if (temp & 1) {
			trigger_alt_guard (sched, temp);
			kernel_scheduler (sched);
			return;
		}

		atw_set (channel_address, temp);
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_xend (void)*/
/*
 *	called to resume outputting process
 *	(or inputting process if output ALTs are enabled)
 *
 *	@SYMBOL:	X_xend
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_XEND
 *	@PRIO:		70
 */
K_CALL_DEFINE_1_0 (X_xend)
{
	word *channel_address, *ptr;

	K_CALL_PARAMS_1 (channel_address);

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
 *	@INPUT:		3
 *	@OUTPUT: 	0
 *	@CALL: 		K_XIN
 *	@PRIO:		90
 */
BUILD_CHANNEL_COUNTED_IO (Y_xin, 0, CIO_EXTENDED | CIO_INPUT)
/*}}}*/
/*{{{  Y_mt_in */
/*
 *	@SYMBOL:	Y_mt_in
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_MT_IN
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_mt_in, 0, CIO_MOBILE | CIO_INPUT)
/*}}}*/
/*{{{  Y_mt_out */
/*
 *	@SYMBOL:	Y_mt_out
 *	@INPUT:		2
 *	@OUTPUT: 	0	
 *	@CALL: 		K_MT_OUT
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_mt_out, 0, CIO_MOBILE | CIO_OUTPUT)
/*}}}*/
/*{{{  Y_mt_xchg */
/*
 *	@SYMBOL:	Y_mt_xchg
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_MT_XCHG
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_mt_xchg, 0, CIO_MOBILE | CIO_EXCHANGE)
/*}}}*/
/*{{{  Y_mt_xin */
/*
 *	@SYMBOL:	Y_mt_xin
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_MT_XIN
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_mt_xin, 0, CIO_EXTENDED | CIO_MOBILE | CIO_INPUT)
/*}}}*/
/*{{{  Y_mt_xout */
/*
 *	@SYMBOL:	Y_mt_xout
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_MT_XOUT
 *	@PRIO:		100
 */
BUILD_CHANNEL_IO (Y_mt_xout, 0, CIO_EXTENDED | CIO_MOBILE | CIO_OUTPUT)
/*}}}*/
/*{{{  Y_mt_xxchg */
/*
 *	@SYMBOL:	Y_mt_xxchg
 *	@INPUT:		2
 *	@OUTPUT: 	0
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
 *	@INPUT:		0
 *	@OUTPUT: 	1
 *	@CALL: 		K_LDTIMER
 *	@PRIO:		90
 */
K_CALL_DEFINE_0_1 (X_ldtimer)
{
	Time now;
	
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (X_ldtimer);

	now = Time_GetTime(sched);

	K_ONE_OUT (now);
}
/*}}}*/
/*{{{  void kernel_Y_tin (void)*/
/*
 *	timer input (delay)
 *
 *	@SYMBOL:	Y_tin
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_TIN
 *	@PRIO:		80
 */
K_CALL_DEFINE_1_0 (Y_tin)
{
	Time now, wait_time;
	
	K_CALL_PARAMS_1 (wait_time);
	ENTRY_TRACE (Y_tin, "%d", wait_time);

	now = Time_GetTime(sched);

	if (!Time_AFTER (now, wait_time)) {
		save_priofinity (sched, Wptr);
		save_return (sched, Wptr, return_address);
		wait_time++; /* from T9000 book... */
		SetTimeField(Wptr, wait_time);
		add_to_timer_queue (sched, Wptr, wait_time, false);
		kernel_scheduler (sched);
	}
	
	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_fasttin (void)*/
/*
 *	fast timer input
 *
 *	@SYMBOL:	Y_fasttin
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_FASTTIN
 *	@PRIO:		80
 */
K_CALL_DEFINE_1_0 (Y_fasttin)
{
	Time wait_time;

	K_CALL_PARAMS_1 (wait_time);
	ENTRY_TRACE (Y_fasttin, "%d", wait_time);

	save_priofinity (sched, Wptr);
	save_return (sched, Wptr, return_address);
	add_to_timer_queue (sched, Wptr, wait_time, false);

	kernel_scheduler (sched);
}
/*}}}*/
/*}}}*/
/*{{{  ALTing */
/*{{{  void kernel_X_alt (void)*/
/*
 *	ALT start
 *
 *	@SYMBOL:	X_alt
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_ALT
 *	@PRIO:		70
 */
K_CALL_DEFINE_0_0 (X_alt)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (X_alt);

	atw_set (&(Wptr[State]), ALT_ENABLING | ALT_NOT_READY | 1);
	weak_write_barrier ();

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_talt (void)*/
/*
 *	timer ALT start
 *
 *	@SYMBOL:	X_talt
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_TALT
 *	@PRIO:		70
 */
K_CALL_DEFINE_0_0 (X_talt)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (X_talt);

	atw_set (&(Wptr[State]), ALT_ENABLING | ALT_NOT_READY | 1);
	atw_set (&(Wptr[TLink]), TimeNotSet_p);
	weak_write_barrier ();

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  static INLINE void kernel_altend (word *Wptr, sched_t *sched, unsigned int return_address, bool jump)*/
static INLINE void kernel_altend (word *Wptr, sched_t *sched, unsigned int return_address, bool jump)
{
	word state = atw_val (&(Wptr[State]));

	if (jump) {
		return_address += Wptr[Temp];
	}

	save_return (sched, Wptr, return_address);
	
	if (unlikely (state != 1)) {
		save_priofinity (sched, Wptr);
		weak_write_barrier ();

		if (!atw_dec_z (&(Wptr[State]))) {
			kernel_scheduler (sched);
		}
	}
	
	K_ZERO_OUT_JRET ();
}
/*}}}*/
/*{{{  void kernel_Y_altend (void)*/
/*
 *	ALT end
 *
 *	@SYMBOL:	Y_altend
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_ALTEND
 *	@PRIO:		70
 */
K_CALL_DEFINE_0_0 (Y_altend)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (Y_altend);

	kernel_altend (Wptr, sched, return_address, true);
}
/*}}}*/
/*{{{  void kernel_Y_caltend (void)*/
/*
 *	CIF ALT end
 *
 *	@SYMBOL:	Y_caltend
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_CALTEND
 *	@PRIO:		70
 */
K_CALL_DEFINE_0_0 (Y_caltend)
{
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (Y_caltend);

	kernel_altend (Wptr, sched, return_address, false);
}
/*}}}*/
/*{{{  void kernel_Y_altwt (void)*/
/*
 *	ALT wait
 *
 *	@SYMBOL:	Y_altwt
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_ALTWT
 *	@PRIO:		70
 */
K_CALL_DEFINE_0_0 (Y_altwt)
{
	word state;

	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (Y_altwt);

	Wptr[Temp] = NoneSelected_o;

	if ((state = atw_val (&(Wptr[State]))) & ALT_NOT_READY) {
		word nstate = (state | ALT_WAITING) & (~(ALT_ENABLING | ALT_NOT_READY));
		
		save_priofinity (sched, Wptr);
		save_return (sched, Wptr, return_address);
		weak_write_barrier ();
		
		if (likely (atw_cas (&(Wptr[State]), state, nstate))) {
			kernel_scheduler (sched);
		}
	}
	
	atw_clear_bit (&(Wptr[State]), ALT_ENABLING_BIT);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_taltwt (void)*/
/*
 *	timer ALT wait
 *
 *	@SYMBOL:	Y_taltwt
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_TALTWT
 *	@PRIO:		70
 */
K_CALL_DEFINE_0_0 (Y_taltwt)
{
	Time now;
	word state;

	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (Y_taltwt);

	Wptr[Temp] = NoneSelected_o;
	
	now = Time_GetTime (sched);

	if ((state = atw_val (&(Wptr[State]))) & ALT_NOT_READY) {
		word nstate = (state | ALT_WAITING) & (~(ALT_ENABLING | ALT_NOT_READY));

		if (Wptr[TLink] == TimeSet_p && !Time_AFTER(GetTimeField(Wptr), now)) {
			/* already past or at timeout */
		} else {
			tqnode_t *tn = NULL;

			save_priofinity (sched, Wptr);
			save_return (sched, Wptr, return_address);

			if (Wptr[TLink] == TimeSet_p) {
				tn = add_to_timer_queue (sched, Wptr, GetTimeField(Wptr), true);
				atw_set (&(Wptr[TLink]), (word) tn);
				nstate = nstate + 1;
			}
			
			weak_write_barrier ();
			
			if (likely (atw_cas (&(Wptr[State]), state, nstate))) {
				kernel_scheduler (sched);
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
				save_return (sched, Wptr, return_address);
				K_ZERO_OUT_JRET ();
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
			save_return (sched, Wptr, return_address);
			K_ZERO_OUT_JRET ();
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
/*{{{  void kernel_X_enbc (void)*/
/*
 *	enable channel
 *
 *	@SYMBOL:	X_enbc
 *	@INPUT:		2
 *	@OUTPUT: 	1
 *	@CALL: 		K_ENBC
 *	@PRIO:		80
 */
K_CALL_DEFINE_2_1 (X_enbc)
{
	word **channel_address, guard;

	K_CALL_PARAMS_2 (guard, channel_address);
	ENTRY_TRACE (X_enbc, "%d, %p (ready = %d)", guard, channel_address, guard && (*channel_address != NotProcess_p) && (*channel_address != Wptr));

	if (!guard) {
		K_ONE_OUT (false);
	}
	
	kernel_enbc (Wptr, sched, return_address, channel_address, false, false);

	K_ONE_OUT (true);
}
/*}}}*/
/*{{{  void kernel_Y_enbc2 (void)*/
/*
 *	enable channel (2 param, with ready address)
 *
 *	@SYMBOL:	Y_enbc2
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_ENBC2
 *	@PRIO:		80
 */
K_CALL_DEFINE_2_0 (Y_enbc2)
{
	unsigned int process_address;
	word **channel_address;

	K_CALL_PARAMS_2 (process_address, channel_address);
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
 *	@INPUT:		3
 *	@OUTPUT: 	1
 *	@CALL: 		K_ENBC3
 *	@PRIO:		80
 */
K_CALL_DEFINE_3_1 (Y_enbc3)
{
	unsigned int process_address;
	word **channel_address, guard;

	K_CALL_PARAMS_3 (process_address, guard, channel_address);
	ENTRY_TRACE (Y_enbc3, "%d, %p (ready = %d), %p", guard, channel_address, guard && (*channel_address != NotProcess_p) && (*channel_address != Wptr), (void *)process_address);

	if (!guard) {
		K_ONE_OUT (false);
	}
	
	kernel_enbc (Wptr, sched, process_address, channel_address, true, false);

	K_ONE_OUT (true);
}
/*}}}*/
/*{{{  void kernel_X_cenbc (void)*/
/*
 *	CIF enable channel (2 param)
 *
 *	@SYMBOL:	X_cenbc
 *	@INPUT:		2
 *	@OUTPUT: 	1
 *	@CALL: 		K_CENBC
 *	@PRIO:		80
 */
K_CALL_DEFINE_2_1 (X_cenbc)
{
	word **channel_address, id;

	K_CALL_PARAMS_2 (id, channel_address);
	ENTRY_TRACE (X_cenbc, "%d %p (ready = %d), %p", id, channel_address, (*channel_address != NotProcess_p) && (*channel_address != Wptr));

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
/*{{{  void kernel_X_enbs (void)*/
/*
 *	enable skip guard
 *
 *	@SYMBOL:	X_enbs
 *	@INPUT:		1
 *	@OUTPUT: 	1
 *	@CALL: 		K_ENBS
 *	@PRIO:		60
 */
K_CALL_DEFINE_1_1 (X_enbs)
{
	word guard;

	K_CALL_PARAMS_1 (guard);
	ENTRY_TRACE (X_enbs, "%d", guard);

	if (!guard) {
		K_ONE_OUT (false);
	}
	
	kernel_enbs (Wptr, 0, false, false);

	K_ONE_OUT (true);
}
/*}}}*/
/*{{{  void kernel_Y_enbs2 (void)*/
/*
 *	enable skip guard (1 param, with ready address)
 *
 *	@SYMBOL:	Y_enbs2
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_ENBS2
 *	@PRIO:		60
 */
K_CALL_DEFINE_1_0 (Y_enbs2)
{
	unsigned int process_address;

	K_CALL_PARAMS_1 (process_address);
	ENTRY_TRACE (X_enbs2, "%p", process_address);

	kernel_enbs (Wptr, 0, true, false);

	K_ZERO_OUT_JUMP (process_address);
}
/*}}}*/
/*{{{  void kernel_Y_enbs3 (void)*/
/*
 *	enable skip guard (with ready address)
 *
 *	@SYMBOL:	Y_enbs3
 *	@INPUT:		2
 *	@OUTPUT: 	1
 *	@CALL: 		K_ENBS3
 *	@PRIO:		60
 */
K_CALL_DEFINE_2_1 (Y_enbs3)
{
	unsigned int process_address;
	word guard;

	K_CALL_PARAMS_2 (process_address, guard);
	ENTRY_TRACE (Y_enbs3, "%d %p", process_address, guard);

	if (!guard) {
		K_ONE_OUT (false);
	}

	kernel_enbs (Wptr, 0, true, false);

	K_ONE_OUT_JUMP (process_address, true);
}
/*}}}*/
/*{{{  void kernel_X_cenbs (void)*/
/*
 *	CIF enable skip guard
 *
 *	@SYMBOL:	X_cenbs
 *	@INPUT:		1
 *	@OUTPUT: 	1
 *	@CALL: 		K_CENBS
 *	@PRIO:		60
 */
K_CALL_DEFINE_1_1 (X_cenbs)
{
	word id;

	K_CALL_PARAMS_1 (id);
	ENTRY_TRACE (X_cenbs, "%d", id);

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
			save_return (sched, Wptr, return_address);
			K_ZERO_OUT_JRET ();
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
 *	@INPUT:		2
 *	@OUTPUT: 	1
 *	@CALL: 		K_ENBT
 *	@PRIO:		70
 */
K_CALL_DEFINE_2_1 (X_enbt)
{
	Time timeout;
	word guard;

	K_CALL_PARAMS_2 (guard, timeout);
	ENTRY_TRACE (X_enbt, "%d, %d", guard, timeout);

	if (!guard) {
		K_ONE_OUT (false);
	}
	
	kernel_enbt (Wptr, sched, 0, timeout, false, false, false);

	K_ONE_OUT (true);
}
/*}}}*/
/*{{{  void kernel_Y_enbt2 (void)*/
/*
 *	enable timer (with ready address)
 *
 *	@SYMBOL:	Y_enbt2
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_ENBT2
 *	@PRIO:		70
 */
K_CALL_DEFINE_2_0 (Y_enbt2)
{
	unsigned int process_address;
	Time timeout;

	K_CALL_PARAMS_2 (process_address, timeout);
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
 *	@INPUT:		3
 *	@OUTPUT: 	1
 *	@CALL: 		K_ENBT3
 *	@PRIO:		70
 */
K_CALL_DEFINE_3_1 (Y_enbt3)
{
	unsigned int process_address;
	Time timeout;
	word guard;

	K_CALL_PARAMS_3 (process_address, guard, timeout);
	ENTRY_TRACE (Y_enbt3, "%d, %d, %p", guard, timeout, (void *)process_address);

	if (!guard) {
		K_ONE_OUT (false);
	}
	
	kernel_enbt (Wptr, sched, process_address, timeout, true, false, false);

	K_ONE_OUT (true);
}
/*}}}*/
/*{{{  void kernel_X_cenbt (void)*/
/*
 *	CIF enable timer
 *
 *	@SYMBOL:	X_cenbt
 *	@INPUT:		2
 *	@OUTPUT: 	1
 *	@CALL: 		K_CENBT
 *	@PRIO:		70
 */
K_CALL_DEFINE_2_1 (X_cenbt)
{
	Time id, timeout;

	K_CALL_PARAMS_2 (id, timeout);
	ENTRY_TRACE (X_cenbt, "%d %d", id, timeout);

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
/*{{{  void kernel_X_disc (void)*/
/*
 *	disable channel
 *
 *	@SYMBOL:	X_disc
 *	@INPUT:		3
 *	@OUTPUT: 	1
 *	@CALL: 		K_DISC
 *	@PRIO:		80
 */
K_CALL_DEFINE_3_1 (X_disc)
{
	unsigned int process_address;
	word **channel_address, guard;

	K_CALL_PARAMS_3 (process_address, guard, channel_address);
	ENTRY_TRACE (X_disc, "%p, %d, %p", (void *)process_address, guard, channel_address);

	if (!guard) {
		K_ONE_OUT (false);
	}

	K_ONE_OUT (kernel_disc (Wptr, process_address, channel_address, (Wptr[Temp] == NoneSelected_o)));
}
/*}}}*/
/*{{{  void kernel_X_cdisc (void)*/
/*
 *	CIF disable channel
 *
 *	@SYMBOL:	X_cdisc
 *	@INPUT:		2
 *	@OUTPUT: 	1
 *	@CALL: 		K_CDISC
 *	@PRIO:		80
 */
K_CALL_DEFINE_2_1 (X_cdisc)
{
	word **channel_address, id;

	K_CALL_PARAMS_2 (id, channel_address);
	ENTRY_TRACE (X_cdisc, "%d %p", id, channel_address);

	K_ONE_OUT (kernel_disc (Wptr, id, channel_address, (Wptr[Temp] == NoneSelected_o)));
}
/*}}}*/
/*{{{  void kernel_X_ndisc (void)*/
/*
 *	disable channel
 *
 *	@SYMBOL:	X_ndisc
 *	@INPUT:		3
 *	@OUTPUT: 	1
 *	@CALL: 		K_NDISC
 *	@PRIO:		80
 */
K_CALL_DEFINE_3_1 (X_ndisc)
{
	unsigned int process_address;
	word **channel_address, guard;

	K_CALL_PARAMS_3 (process_address, guard, channel_address);
	ENTRY_TRACE (X_ndisc, "%p, %d, %p", (void *)process_address, guard, channel_address);

	if (!guard) {
		K_ONE_OUT (false);
	}

	K_ONE_OUT (kernel_disc (Wptr, process_address, channel_address, true));
}
/*}}}*/
/*{{{  void kernel_X_diss (void)*/
/*
 *	disable SKIP guard
 *
 *	@SYMBOL:	X_diss
 *	@INPUT:		2
 *	@OUTPUT: 	1
 *	@CALL: 		K_DISS
 *	@PRIO:		60
 */
K_CALL_DEFINE_2_1 (X_diss)
{
	unsigned int process_address;
	word fired, guard;

	K_CALL_PARAMS_2 (process_address, guard);
	ENTRY_TRACE (X_diss, "%p, %d", (void *)process_address, guard);

	if ((fired = guard)) {
		if (Wptr[Temp] == NoneSelected_o) {
			Wptr[Temp] = process_address;
		} else {
			fired = false;
		}
	}

	K_ONE_OUT (fired);
}
/*}}}*/
/*{{{  void kernel_X_cdiss (void)*/
/*
 *	CIF disable SKIP guard
 *
 *	@SYMBOL:	X_cdiss
 *	@INPUT:		1
 *	@OUTPUT: 	1
 *	@CALL: 		K_CDISS
 *	@PRIO:		60
 */
K_CALL_DEFINE_1_1 (X_cdiss)
{
	word id;

	K_CALL_PARAMS_1 (id);
	ENTRY_TRACE (X_cdiss, "%d", id);

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
 *	@INPUT:		2
 *	@OUTPUT: 	1
 *	@CALL: 		K_NDISS
 *	@PRIO:		60
 */
K_CALL_DEFINE_2_1 (X_ndiss)
{
	unsigned int fired, guard, process_address;

	K_CALL_PARAMS_2 (process_address, guard);
	ENTRY_TRACE (X_ndiss, "%p, %d", (void *)process_address, guard);

	if ((fired = guard)) {
		Wptr[Temp] = process_address;
	}
	
	K_ONE_OUT (fired);
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
		} else {
			atw_dec (&(Wptr[State]));
		}

		release_tqnode (sched, tn);
		
		return fired;
	}

	return false;
}
/*}}}*/
/*{{{  void kernel_X_dist (void)*/
/*
 *	disable timer
 *
 *	@SYMBOL:	X_dist
 *	@INPUT:		3
 *	@OUTPUT: 	1
 *	@CALL: 		K_DIST
 *	@PRIO:		70
 */
K_CALL_DEFINE_3_1 (X_dist)
{
	unsigned int process_address;
	Time timeout;
	word guard;
	
	K_CALL_PARAMS_3 (process_address, guard, timeout);
	ENTRY_TRACE (X_dist, "%d, %d, %d", process_address, guard, timeout);

	if (!guard) {
		K_ONE_OUT (false);
	}
	
	K_ONE_OUT (kernel_dist (Wptr, sched, process_address, timeout, (Wptr[Temp] == NoneSelected_o)));
}
/*}}}*/
/*{{{  void kernel_X_cdist (void)*/
/*
 *	CIF disable timer
 *
 *	@SYMBOL:	X_cdist
 *	@INPUT:		2
 *	@OUTPUT: 	1
 *	@CALL: 		K_CDIST
 *	@PRIO:		70
 */
K_CALL_DEFINE_2_1 (X_cdist)
{
	Time id, timeout;
	
	K_CALL_PARAMS_2 (id, timeout);
	ENTRY_TRACE (X_cdist, "%d, %d", id, timeout);

	K_ONE_OUT (kernel_dist (Wptr, sched, id, timeout, (Wptr[Temp] == NoneSelected_o)));
}
/*}}}*/
/*{{{  void kernel_X_ndist (void)*/
/*
 *	disable timer
 *
 *	@SYMBOL:	X_ndist
 *	@INPUT:		3
 *	@OUTPUT: 	1
 *	@CALL: 		K_NDIST
 *	@PRIO:		70
 */
K_CALL_DEFINE_3_1 (X_ndist)
{
	unsigned int process_address;
	Time timeout;
	word guard;

	K_CALL_PARAMS_3 (process_address, guard, timeout);
	ENTRY_TRACE (X_ndist, "%d, %d, %d", process_address, guard, timeout);

	if (!guard) {
		K_ONE_OUT (false);
	}
	
	K_ONE_OUT (kernel_dist (Wptr, sched, process_address, timeout, true));
}
/*}}}*/
/*}}}*/
/*{{{  semaphores */
/*{{{  void kernel_Y_sem_claim (void)*/
/*
 *	@SYMBOL:	Y_sem_claim
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_SEM_CLAIM
 *	@PRIO:		90
 */
K_CALL_DEFINE_1_0 (Y_sem_claim)
{
	ccsp_sem_t *sem;
	
	K_CALL_PARAMS_1 (sem);
	ENTRY_TRACE (Y_sem_claim, "%p", sem);

	sem_claim (sched, Wptr, return_address, sem);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_sem_release (void)*/
/*
 *	@SYMBOL:	X_sem_release
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_SEM_RELEASE
 *	@PRIO:		90
 */
K_CALL_DEFINE_1_0 (X_sem_release)
{
	ccsp_sem_t *sem;
	
	K_CALL_PARAMS_1 (sem);
	ENTRY_TRACE (X_sem_release, "%p", sem);

	sem_release (sched, sem);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_sem_init (void)*/
/*
 *	@SYMBOL:	X_sem_init
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_SEM_INIT
 *	@PRIO:		70
 */
K_CALL_DEFINE_1_0 (X_sem_init)
{
	ccsp_sem_t *sem;
	
	K_CALL_PARAMS_1 (sem);
	ENTRY_TRACE (X_sem_init, "%p", sem);

	sem_init (sem);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_mt_lock (void)*/
/*
 *	lock a mobile type
 *
 *	@SYMBOL:	Y_mt_lock
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_MT_LOCK
 *	@PRIO:		90
 */
K_CALL_DEFINE_2_0 (Y_mt_lock)
{
	mt_cb_shared_internal_t *cb;
	word *ptr, type;
	
	K_CALL_PARAMS_2 (type, ptr);
	ENTRY_TRACE (Y_mt_lock, "%p, %08x", type, ptr);

	cb = (mt_cb_shared_internal_t *) (ptr - MT_CB_SHARED_PTR_OFFSET);

	ASSERT ( MT_TYPE(cb->type) == MT_CB );
	ASSERT ( cb->type & MT_CB_SHARED );
	ASSERT ( type == MT_CB_CLIENT || type == MT_CB_SERVER );

	sem_claim (sched, Wptr, return_address, &(cb->sem[type]));

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_mt_unlock (void)*/
/*
 *	unlock a mobile type
 *
 *	@SYMBOL:	X_mt_unlock
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_MT_UNLOCK
 *	@PRIO:		90
 */
K_CALL_DEFINE_2_0 (X_mt_unlock)
{
	mt_cb_shared_internal_t *cb;
	word *ptr, type;
	
	K_CALL_PARAMS_2 (type, ptr);
	ENTRY_TRACE (X_mt_unlock, "%p", type, ptr);

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
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_MT_SYNC
 *	@PRIO:		90
 */
K_CALL_DEFINE_1_0 (Y_mt_sync)
{
	ccsp_barrier_t *bar;
	
	K_CALL_PARAMS_1 (bar);
	ENTRY_TRACE (Y_mt_sync, "%p", bar);
	
	save_return (sched, Wptr, return_address);

	bar->sync (sched, &(bar->data), Wptr);
}
/*}}}*/
/*{{{  void kernel_X_mt_resign (void)*/
/*
 *	@SYMBOL:	X_mt_resign
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_MT_RESIGN
 *	@PRIO:		80
 */
K_CALL_DEFINE_2_0 (X_mt_resign)
{
	ccsp_barrier_t *bar;
	word count;
	
	K_CALL_PARAMS_2 (count, bar);
	ENTRY_TRACE (X_mt_resign, "%d %p", count, bar);

	bar->resign (sched, &(bar->data), count);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_mt_enroll (void)*/
/*
 *	@SYMBOL:	X_mt_enroll
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_MT_ENROLL
 *	@PRIO:		80
 */
K_CALL_DEFINE_2_0 (X_mt_enroll)
{
	ccsp_barrier_t *bar;
	word count;
	
	K_CALL_PARAMS_2 (count, bar);
	ENTRY_TRACE (X_mt_enroll, "%d %p", count, bar);

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
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_WAIT_INT
 *	@PRIO:		50
 *	@DEPEND:	RMOX_BUILD
 */
K_CALL_DEFINE_2_0 (Y_wait_int)
{
	word number, mask;

	K_CALL_PARAMS_2 (number, mask);

	ENTRY_TRACE (Y_wait_int, "0x%8.8X", number);

	if (inttab[number]) {
		MESSAGE ("scheduler: ieee, someone already waiting for this interrupt!\n");
		/* blind reschedule */
		kernel_scheduler (sched);
	}
	
	cli ();

	if (!intcount[number]) {
		/* no interrupt yet */
		save_priofinity (sched, Wptr);
		save_return (sched, Wptr, return_address);
		Wptr[Temp] = 0;
		inttab[number] = Wptr;
		sti ();
		
		kernel_scheduler (sched);
	} else {
		Wptr[Temp] = intcount[number];
		intcount[number] = 0;
		sti ();

		K_ZERO_OUT ();
	}
}
/*}}}*/
#endif	/* defined(RMOX_BUILD) */
/*}}}*/
/*{{{  dynamic/mobile-processes */
#if !defined(RMOX_BUILD) && defined(DYNAMIC_PROCS)
/*{{{  void kernel_X_kernel_run (void)*/
/*
 *	dynamic kernel run entry point (and resume point)
 *
 *	@SYMBOL:	X_kernel_run
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_KERNEL_RUN
 *	@PRIO:		50
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
K_CALL_DEFINE_1_0 (X_kernel_run)
{
	unsigned int kr_param;
	d_process *kr_dptr;
	
	K_CALL_PARAMS_1 (kr_param);
	ENTRY_TRACE (X_kernel_run, "%p", (void *)kr_param);

	kr_dptr = dynproc_startprocess ((int *)kr_param, K_CALL_PTR (Y_dynproc_exit));
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
		Wptr[IptrSucc]		= (word) K_CALL_PTR (Y_dynproc_exit);
		return_address 		= (word) kr_dptr->entrypoint;
	}

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_Y_dynproc_suspend (void)*/
/*
 *	entered when a dynamic process is suspending
 *
 *	@SYMBOL:	Y_dynproc_suspend
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_DYNPROC_SUSPEND
 *	@PRIO:		0
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
K_CALL_DEFINE_1_0 (Y_dynproc_suspend)
{
	word *ds_param;
	
	K_CALL_PARAMS_1 (ds_param);
	ENTRY_TRACE (Y_dynproc_suspend, "%p", ds_param);

	/* ds_param should point at the argument-set (VAL DPROCESS p, INT result) */
	if (dynproc_suspendprocess ((d_process *)(ds_param[0]), (int *)(ds_param[1]), Wptr, return_address, sched->priofinity)) {
		/* failed */
		K_ZERO_OUT ();
	} else {
		kernel_scheduler (sched);
	}
}
/*}}}*/
/*{{{  void kernel_Y_dynproc_exit (void)*/
/*
 *	entered when dynamic process finishes
 *
 *	@SYMBOL:	Y_dynproc_exit
 *	@INPUT:		0
 *	@OUTPUT: 	0
 *	@CALL: 		K_DYNPROC_EXIT
 *	@PRIO:		0
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
K_CALL_DEFINE_0_0 (Y_dynproc_exit)
{
	d_process *kr_dptr;
	word *kr_wptr;
	
	K_CALL_PARAMS_0 ();
	ENTRY_TRACE0 (Y_dynproc_exit);

	kr_wptr 		= (word *) (((word) Wptr) - (4 * sizeof(word)));
	kr_dptr 		= dynproc_endprocess (kr_wptr);
	*(kr_dptr->result) 	= DPROCESS_FINISHED;
	Wptr 			= kr_dptr->holding_wptr;
	switch_priofinity (sched, kr_dptr->holding_priofinity);

	K_ZERO_OUT_JUMP (kr_dptr->holding_raddr);
}
/*}}}*/
/*{{{  void kernel_X_ldwsmap (void)*/
/*
 *	load workspace-map
 *
 *	@SYMBOL:	X_ldwsmap
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_LDWSMAP
 *	@PRIO:		0
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
K_CALL_DEFINE_2_0 (X_ldwsmap)
{
	unsigned int code_offset, process_address;
	
	K_CALL_PARAMS_2 (process_address, code_offset);

	mpcb_add_wsmap ((mp_ctrlblk *)process_address, (unsigned char *)code_offset, Wptr);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_ulwsmap (void)*/
/*
 *	unload workspace-map
 *
 *	@SYMBOL:	X_ulwsmap
 *	@INPUT:		2
 *	@OUTPUT: 	0
 *	@CALL: 		K_ULWSMAP
 *	@PRIO:		0
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
K_CALL_DEFINE_2_0 (X_ulwsmap)
{
	unsigned int code_offset, process_address;
	
	K_CALL_PARAMS_2 (process_address, code_offset);

	mpcb_del_wsmap ((mp_ctrlblk *)process_address, (unsigned char *)code_offset, Wptr);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_rmwsmap (void)*/
/*
 *	delete workspace-map
 *
 *	@SYMBOL:	X_rmwsmap
 *	@INPUT:		1
 *	@OUTPUT: 	0
 *	@CALL: 		K_RMWSMAP
 *	@PRIO:		0
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
K_CALL_DEFINE_1_0 (X_rmwsmap)
{
	unsigned int process_address;
	
	K_CALL_PARAMS_1 (process_address);

	mpcb_rm_wsmap ((mp_ctrlblk *)process_address);

	K_ZERO_OUT ();
}
/*}}}*/
/*{{{  void kernel_X_mppclone (void)*/
/*
 *	clone mobile process
 *
 *	@SYMBOL:	X_mppclone
 *	@INPUT:		1
 *	@OUTPUT: 	1
 *	@CALL: 		K_MPPCLONE
 *	@PRIO:		20
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
K_CALL_DEFINE_1_1 (X_mppclone)
{
	unsigned int process_address;

	K_CALL_PARAMS_1 (process_address);

	process_address = (word)mpcb_mpp_clone ((mp_ctrlblk *)process_address);
	if (process_address == NotProcess_p) {
		if (ccsp_ignore_errors) {
			kernel_scheduler (sched);
		} else {
			BMESSAGE ("mobile process CLONE error at 0x%x, Wptr = 0x%x.\n", return_address, (unsigned int)Wptr);
			ccsp_kernel_exit (1, return_address);
		}
	}
	
	K_ONE_OUT (process_address);
}
/*}}}*/
/*{{{  void kernel_Y_mppserialise (void)*/
/*
 *	serialise mobile process
 *
 *	@SYMBOL:	Y_mppserialise
 *	@INPUT:		3
 *	@OUTPUT: 	0
 *	@CALL: 		K_MPPSERIALISE
 *	@PRIO:		20
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
K_CALL_DEFINE_3_0 (Y_mppserialise)
{
	unsigned int count, process_address;
	word **channel_address;
	byte *destination_address;
	
	K_CALL_PARAMS_3 (count, destination_address, channel_address);

	/* actually pass a pointer to it, may need to nullify */
	process_address = ((word *)(*channel_address))[Pointer];
	if (!mpcb_mpp_serialise ((mp_ctrlblk **)process_address, (unsigned int *)process_address + 1, (int *)destination_address, (int *)count)) {
		if (ccsp_ignore_errors) {
			kernel_scheduler (sched);
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
 *	@INPUT:		3
 *	@OUTPUT: 	0
 *	@CALL: 		K_MPPDESERIALISE
 *	@PRIO:		20
 *	@DEPEND:	DYNAMIC_PROCS
 *	@INCOMPATIBLE:	RMOX_BUILD
 */
K_CALL_DEFINE_3_0 (Y_mppdeserialise)
{
	unsigned int count, process_address;
	word **channel_address;
	byte *source_address;
	
	K_CALL_PARAMS_3 (channel_address, source_address, count);

	/* pass a pointer to the ws locn */
	process_address = ((word *)(*channel_address))[Pointer];
	if (!mpcb_mpp_deserialise ((int)source_address, (int)count, (mp_ctrlblk **)process_address, (unsigned int *)process_address + 1)) {
		if (ccsp_ignore_errors) {
			kernel_scheduler (sched);
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

/*{{{  CIF stubs */
/*{{{  void *kernel_CIF_endp_resume_stub (void)*/
/*
 *	@SYMBOL:	CIF_endp_resume_stub
 *	@CALL: 		K_CIF_ENDP_RESUME_STUB
 *	@PRIO:		0
 */
static void *kernel_CIF_endp_resume_stub (void)
{
	void *address;
	K_CIF_ENDP_RESUME (address);
	return address;
}
/*}}}*/
/*{{{  void *kernel_CIF_light_proc_stub (void)*/
/*
 *	@SYMBOL:	CIF_light_proc_stub
 *	@CALL: 		K_CIF_LIGHT_PROC_STUB
 *	@PRIO:		0
 */
static void *kernel_CIF_light_proc_stub (void)
{
	void *address;
	K_CIF_PROC_IND (address, K_ENDP, BarrierPtr);
	return address;
}
/*}}}*/
/*{{{  void *kernel_CIF_proc_stub (void)*/
/*
 *	@SYMBOL:	CIF_proc_stub
 *	@CALL: 		K_CIF_PROC_STUB
 *	@PRIO:		0
 */
static void *kernel_CIF_proc_stub (void)
{
	void *address;
	K_CIF_PROC (address, K_PROC_END, -CIF_PROCESS_WORDS);
	return address;
}
/*}}}*/
/*}}}*/

