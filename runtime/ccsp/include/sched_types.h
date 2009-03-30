/*
 *	Type definitions for C scheduler and occam support
 *	Copyright (C) 1998 Jim Moores
 *	Modifications (C) 1999-2007 Fred Barnes <frmb@kent.ac.uk>
 *	Modifications (C) 2007-2008 Carl Ritson <cgr@kent.ac.uk>
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

#if defined(HAVE_CONFIG_H)
#include <config.h>
#endif

#ifndef SCHED_TYPES_H
#define SCHED_TYPES_H

#include <arch/alignment.h>
#include <arch/atomics.h>
#include <arch/ccsp_types.h>
#include <ukcthreads_types.h>
#include <sched_consts.h>
#include <kiface.h>

#ifdef __GNUC__
#define _PACK_STRUCT __attribute__ ((packed))
#else
#warning "Unable to enforce alignment and packing on structures."
#define _PACK_STRUCT
#endif

/*{{{  pre-declared type definitions */
typedef struct _batch_t batch_t;
typedef struct _bar_head_t bar_head_t;
typedef struct _bsc_batch_t bsc_batch_t;
typedef struct _bsc_job_t bsc_job_t;
typedef struct _mwindow_t mwindow_t;
typedef struct _runqueue_t runqueue_t;
typedef struct _sched_t sched_t;
typedef struct _tqnode_t tqnode_t;
/*}}}*/

/*{{{  batch_t */
#define BATCH_ALLOC_SIZE (sizeof(word) * 16)
struct _batch_t {
	/* persistent state */
	word		*Fptr;
	word		*Bptr;
	word		size;
	/* link state */
	batch_t		*next;
	/* migration fields */
	atomic_t	state;
	word		priofinity;
	/* barrier fields */
	batch_t		*prio[8];
	/* alignment */
	word		pad[2];
} _PACK_STRUCT;

static inline void init_batch_t (batch_t *batch) {
	batch->Fptr 		= NotProcess_p;
	batch->Bptr 		= NotProcess_p;
	batch->size		= 0;
	batch->next		= NULL;
	att_init (&(batch->state), 0);
	batch->priofinity	= 0;
}

static inline void reinit_batch_t (batch_t *batch) {
	batch->Fptr 	= NotProcess_p;
	batch->size	= 0;
}
/*}}}*/

/*{{{  bar_head_t */
#define BAR_HEAD_PRIO_MASK 0x3
#if MAX_PRIORITY_LEVELS <= 8
#define BATCH_PRIO_SHIFT 0
#elif MAX_PRIORITY_LEVELS <= 16
#define BATCH_PRIO_SHIFT 1
#elif MAX_PRIORITY_LEVELS <= 32
#define BATCH_PRIO_SHIFT 2
#elif MAX_PRIORITY_LEVELS <= 64
#define BATCH_PRIO_SHIFT 3
#else
#warning "Please define BATCH_PRIO_SHIFT to match MAX_PRIORITY_LEVELS."
#endif

struct _bar_head_t {
	batch_t		*fptr;
	batch_t		*bptr;
	batch_t		*current;
	batch_t		*affine;
	word		count;
	batch_t		*prio[4];
} _PACK_STRUCT;

static inline void init_bar_head_t (bar_head_t *bh) {
	bh->fptr	= NULL;
	bh->bptr	= NULL;
	bh->current	= NULL;
	bh->affine	= NULL;
	bh->count	= 0;
	bh->prio[0]	= NULL;
	bh->prio[1]	= NULL;
	bh->prio[2]	= NULL;
	bh->prio[3]	= NULL;
}
/*}}}*/

/*{{{  bsc_batch_t */
struct _bsc_job_t {
	word		*ws_arg;			/* argument to func */
	void		(*func)(word *);		/* function to execute */
	word		iptr;				/* return address */
	int		adjust;				/* workspace adjustment */
	word		thread;				/* pthread pointer */
} _PACK_STRUCT;

struct _bsc_batch_t {
	/* batch (must match layout of batch_t */
	word		*wptr;				/* fptr single proc batch */
	word		*bptr;				/* don't use */
	word		size;				/* don't use */
	bsc_batch_t	*next;
	atomic_t	state;				/* don't use */
	word		priofinity;
	/* job */
	bsc_job_t	bsc;
} _PACK_STRUCT;
/*}}}*/

/*{{{  runqueue_t */
struct _runqueue_t {
	batch_t		*Fptr;
	batch_t		*Bptr;
	word		priofinity;	/* of pending batch */
	batch_t		*pending;
} _PACK_STRUCT;

static inline void init_runqueue_t (runqueue_t *rq) {
	rq->Fptr 	= NULL;
	rq->Bptr 	= NULL;
	rq->priofinity 	= 0;
	rq->pending 	= NULL;
}
/*}}}*/

/*{{{  mwindow_t */
#define MWINDOW_SIZE 		(15)
#define MWINDOW_HEAD_WRAP_BIT	(4)
#define MWINDOW_MASK		0xffff
struct _mwindow_t {
	atomic_t		data[MWINDOW_SIZE + 1];
} _PACK_STRUCT;

static inline void init_mwindow_t (mwindow_t *mw) {
	int i;
	for (i = 0; i < MWINDOW_SIZE + 1; ++i) {
		if (i == MWINDOW_STATE) {
			att_init (&(mw->data[i]), (unsigned int)0);
		} else {
			att_init (&(mw->data[i]), (unsigned int)NULL);
		}
	}
}
/*}}}*/

/*{{{  tqnode_t */
struct _tqnode_t {
	Time		time;
	tqnode_t	*next;
	tqnode_t	*prev;
	batch_t		*bnext; /* must match location of next in batch_t */
	atomic_t	state; /* must match location of state in batch_t */
	sched_t		*scheduler;
	word		*wptr;
} _PACK_STRUCT;

static inline void init_tqnode_t (tqnode_t *tn) {
	tn->time	= 0;
	tn->next	= NULL;
	tn->prev	= NULL;
	/* not required: tn->bnext = NULL; */
	/* not required: att_init (&(tn->state), 0); */
	tn->scheduler	= NULL;
	tn->wptr	= NULL;
}
/*}}}*/

/*{{{  sched_t */
struct _sched_t {
	/** stack pointer	- must be at the right offset **/
	unsigned int	stack;
	/** call params		- must be at the right offset **/
	word		cparam[5];
	/** calltable		- must be at the right offset **/
	void		*calltable[K_MAX_SUPPORTED];

	/** local debug state **/
	word 		mdparam[32];

	/** scheduler constants	**/
	unsigned int 	index;
	unsigned int 	id; 			/* 1 << index */
	unsigned int 	cpu_factor;
	unsigned int 	cpu_khz;
	int 		signal_in;
	int		signal_out;
	void		*allocator;
	unsigned int	spin;
	word 		pad1[CACHELINE_WORDS]	CACHELINE_ALIGN;
	
	/** local scheduler state **/
	int 		dispatches		CACHELINE_ALIGN;
	word 		priofinity;
	word		loop;
	atomic_t	rqstate;

	batch_t 	*free;
	batch_t 	*laundry;

	cputime_t 	timeout;
	tqnode_t	*tq_fptr;
	tqnode_t	*tq_bptr;

	batch_t		curb			CACHELINE_ALIGN;
	runqueue_t 	rq[MAX_PRIORITY_LEVELS];
	struct {
		word proc_start;
		word proc_end;
		word startp;
		word endp;
	} stats;
	word		pad2[CACHELINE_WORDS];

	/** globally accessed scheduling state **/

	/* event flags */
	atomic_t 	sync			CACHELINE_ALIGN;
	word		pad3[CACHELINE_WORDS]	CACHELINE_ALIGN;
	
	/* batch mail */
	runqueue_t 	bmail			CACHELINE_ALIGN;
	word		pad4[CACHELINE_WORDS]	CACHELINE_ALIGN;

	/* process mail */
	runqueue_t 	pmail			CACHELINE_ALIGN;
	word		pad5[CACHELINE_WORDS]	CACHELINE_ALIGN;

	/* migration windows */
	atomic_t 	mwstate			CACHELINE_ALIGN;
	word		pad6[CACHELINE_WORDS]	CACHELINE_ALIGN;

	mwindow_t 	mw[MAX_PRIORITY_LEVELS];

} _PACK_STRUCT;

static inline void init_sched_t (sched_t *sched) {
	int i;

	for (i = 0; i < 32; ++i) {
		sched->mdparam[i] = 0xffffffff;
	}

	sched->index 		= -1;
	sched->id		= 0;
	sched->cpu_factor	= 0;
	sched->cpu_khz		= 0;
	sched->signal_in	= -1;
	sched->signal_out	= -1;
	sched->allocator	= NULL;
	sched->spin		= 0;

	sched->dispatches	= 0;
	sched->priofinity	= 0;
	sched->loop		= 0;
	att_init (&(sched->rqstate), 0);

	sched->free		= NULL;
	sched->laundry		= NULL;

	sched->tq_fptr		= NULL;
	sched->tq_bptr		= NULL;

	init_batch_t (&(sched->curb));

	for (i = 0; i < MAX_PRIORITY_LEVELS; ++i) {
		init_runqueue_t (&(sched->rq[i]));
	}

	sched->stats.proc_start	= 0;
	sched->stats.proc_end	= 0;
	sched->stats.startp	= 0;
	sched->stats.endp	= 0;

	att_init (&(sched->sync), 0);

	init_runqueue_t (&(sched->bmail));
	init_runqueue_t (&(sched->pmail));

	att_init (&(sched->mwstate), 0);
	for (i = 0; i < MAX_PRIORITY_LEVELS; ++i) {
		init_mwindow_t (&(sched->mw[i]));
	}
}
/*}}}*/

/*{{{  ccsp_sem_t (semaphore) */
typedef struct _ccsp_sem_t {
	word fptr;
	word bptr;
} _PACK_STRUCT ccsp_sem_t;
/*}}}*/

/*{{{  ccsp_barrier_t */
typedef REGPARM void (*ccsp_barrier_sync_t) (sched_t *, void *, word *);
typedef REGPARM void (*ccsp_barrier_enroll_t) (sched_t *, void *, word);
typedef REGPARM void (*ccsp_barrier_resign_t) (sched_t *, void *, word);
typedef struct _ccsp_barrier_t {
	ccsp_barrier_sync_t	sync;
	ccsp_barrier_enroll_t	enroll;
	ccsp_barrier_resign_t	resign;
	word 			data[];
} _PACK_STRUCT ccsp_barrier_t;
/*}}}*/

/*{{{  bar_t (full barrier) */
#define BAR_SYNCING_BIT 	((sizeof (word) * 8) - 1)
#define BAR_SYNCING		(1 << BAR_SYNCING_BIT)
#define BAR_TAG_SHIFT		((sizeof (word) * 8) - 3)
#define BAR_TAG_MASK		(0x3 << BAR_TAG_SHIFT)
#define BAR_TAG(S)		(((S) >> BAR_TAG_SHIFT) & 0x3)
#define BAR_NEXT_TAG(T)		(((T) + 1) & 0x3)
#define BAR_COUNT_MASK		(~(BAR_SYNCING | BAR_TAG_MASK))
#define BAR_COUNT(S)		((S) & BAR_COUNT_MASK)
#define BAR_STATE(C,T,F)	((C) | (T << BAR_TAG_SHIFT) | (F))
#define BAR_HEAD_PTR(X)		((bar_head_t *) ((X) & (~0x3)))
#define BAR_HEAD_TAG(X)		((X) & 0x3)
typedef struct _bar_t {
	word state;
	word heads;
	word head[MAX_RUNTIME_THREADS];
} _PACK_STRUCT bar_t;
/*}}}*/

/*{{{  mproc_bar_t (mobile process barrier) */
#define MPROC_BAR_PHASE_BIT	((sizeof (word) * 8) - 1)
#define MPROC_BAR_PHASE		(1 << MPROC_BAR_PHASE_BIT)
#define MPROC_BAR_COUNT		(~MPROC_BAR_PHASE)
typedef struct _mproc_bar_t {
	word enrolled;
	word state;
	word *fptr;
	word *bptr;
} _PACK_STRUCT mproc_bar_t;
/*}}}*/

/*{{{  ccsp_global_t (global data) */
typedef struct _ccsp_global_t {
	atomic_t	enabled_threads;
	atomic_t	idle_threads;
	atomic_t	sleeping_threads;
	atomic_t	shutdown;
	byte		pad0[(2 * CACHELINE_BYTES) - (sizeof (atomic_t) * 4)];

	sched_t		*schedulers[MAX_RUNTIME_THREADS]	CACHELINE_ALIGN;
	word		pad1[CACHELINE_WORDS]			CACHELINE_ALIGN;
	void		*calltable[K_MAX_SUPPORTED]		CACHELINE_ALIGN;
} _PACK_STRUCT ccsp_global_t;

static inline void init_ccsp_global_t (ccsp_global_t *ccsp) {
	int i;

	att_init (&(ccsp->enabled_threads), 0);
	att_init (&(ccsp->idle_threads), 0);
	att_init (&(ccsp->sleeping_threads), 0);
	att_init (&(ccsp->shutdown), 0);

	for (i = 0; i < MAX_RUNTIME_THREADS; ++i) {
		ccsp->schedulers[i] = NULL;
	}
}
/*}}}*/

#undef _PACK_STRUCT

#endif

