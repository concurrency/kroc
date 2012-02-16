/*
 *	bsyscalls.c
 *	Blocking system calls
 *	Copyright (C) 2000-2005 Fred Barnes <frmb@kent.ac.uk>
 *	          (C) 2007	Carl Ritson <cgr@kent.ac.uk>
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
#include <config.h>
#endif

#if defined(RMOX_BUILD)
/*{{{  RMoX stuff*/
#include <rmox_if.h>
/*{{{  void _killcall (int *w)*/
/*
 *	dummy for RMOX
 */
void _killcall (int *w)
{
	rmox_panic ("_killcall()");
}
/*}}}*/
/*}}}*/
#else	/* !RMOX_BUILD */

#ifdef BLOCKING_SYSCALLS
/*{{{  includes*/
#define __BSYSCALLS_C

#include <stdio.h>
#include <string.h>
/* #include <stdlib.h> */
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>

#ifdef USE_PTHREADS
	#include <pthread.h>
#endif
#include <signal.h>
#include <errno.h>
#include <sys/time.h>
#include <time.h>

#include <kernel.h>
#include <arch/asm_ops.h>
#include <arch/atomics.h>
#include <rts.h>
#include <dmem_if.h>

/*}}}*/
/*{{{  prototypes and structures*/
/*{{{  private types*/
typedef struct _bsc_pool_t {
	int 			number;

	pthread_mutex_t		lock;
	pthread_cond_t		signal;

	atomic_t		running;
	atomic_t                waiting;
	atomic_t		shutdown;
	atomic_t		dead;

	bsc_batch_t		*fptr;
	bsc_batch_t		*bptr;

	pthread_t	 	to_reap;

	word			pad[16];
} bsc_pool_t;
/*}}}*/
/*{{{  private data*/
static atomic_t		pending;
static bsc_pool_t 	pools[MAX_RUNTIME_THREADS + 1];
/*}}}*/
/*{{{  prototypes and defines*/
#define MAX_BSC_THREADS 1024
extern void schedule_batch (batch_t *);
/*}}}*/
/*}}}*/

/*{{{  int bsyscalls_create_clones (void)*/
/*
 *	int bsyscalls_create_clones (void)
 *	Creates the clones (and sets them off)
 *	Returns 0 on success, -1 on failure
 */
int bsyscalls_create_clones (void)
{
	int i;

	att_init (&(pending), 0);

	for (i = 0; i < (MAX_RUNTIME_THREADS + 1); ++i) {
		bsc_pool_t *p = &(pools[i]);
		p->number = i;
		pthread_mutex_init (&(p->lock), NULL);
		pthread_cond_init (&(p->signal), NULL);
		att_init (&(p->running), 0);
		att_init (&(p->waiting), 0);
		att_init (&(p->shutdown), 0);
		att_init (&(p->dead), 0);
		p->fptr = NULL;
		p->bptr = NULL;
	}
	
	weak_write_barrier ();

	return 0;
}
/*}}}*/
/*{{{  void bsyscalls_destroy_clones (void)*/
/*
 *	void bsyscalls_destroy_clones (void)
 *	Destroyes the clones (sends a SIGKILL)
 */
void bsyscalls_destroy_clones (void)
{
	int i;

	for (i = 0; i < (MAX_RUNTIME_THREADS + 1); ++i) {
		bsc_pool_t *p = &(pools[i]);
		att_safe_set (&(p->shutdown), 1);
		pthread_mutex_lock (&(p->lock));
		if (att_safe_val (&(p->running)) > 0) {
			pthread_cond_broadcast (&(p->signal));
		}
		pthread_mutex_unlock (&(p->lock));
	}
}
/*}}}*/
/*{{{  void bsyscalls_recover_dead_threads (void)*/
/*
 *	called to recover any recently dead threads
 */
void bsyscalls_recover_dead_threads (void)
{
	bsc_pool_t *pool = &(pools[0]);

	/* speculative */
	if (att_safe_val (&(pool->dead))) {
		unsigned int dead;
		pthread_t to_reap;

		pthread_mutex_lock (&(pool->lock));
		dead = att_safe_swap (&(pool->dead), 0);
		to_reap = pool->to_reap;
		pthread_mutex_unlock (&(pool->lock));

		if (dead) {
			void *tmp;
			pthread_join (to_reap, &tmp);
		}
	}
}
/*}}}*/
/*{{{  static void bsc_cleanup_pool (void *arg)*/
static void bsc_cleanup_pool (void *arg)
{
	bsc_pool_t 	*pool = (bsc_pool_t *) arg;
	unsigned int	dead;
	pthread_t	to_reap;

	dead 		= att_safe_swap (&(pool->dead), 1);
	to_reap		= pool->to_reap;
	pool->to_reap 	= pthread_self ();

	pthread_mutex_unlock (&(pool->lock));

	att_safe_dec (&(pool->running));

	if (dead) {
		void *tmp;
		pthread_join (to_reap, &(tmp));
	}
}
/*}}}*/
/*{{{  static void bsc_cleanup_prepool (void *arg)*/
static void bsc_cleanup_prepool (void *arg)
{
	bsc_pool_t *pool = (bsc_pool_t *) arg;
	pthread_mutex_lock (&(pool->lock));
}
/*}}}*/
/*{{{  static void bsc_cleanup_job (void *arg)*/
static void bsc_cleanup_job (void *arg)
{
	bsc_batch_t *job = (bsc_batch_t *) arg;

	job->wptr[Priofinity] 	= job->priofinity;
	job->wptr[Iptr] 	= job->bsc.iptr;
	
	if (job->bsc.adjust != 0) {
		atw_set ((word *) job->bsc.ws_arg[-1], 0);
	}

	schedule_batch ((batch_t *) job);

	if (att_safe_dec_z (&pending)) {
		/* mitigate a shutdown race */
		if (att_val (&(ccsp_shutdown))) {
			unsigned int id = bsf (att_val (&enabled_threads));
			ccsp_wake_thread (schedulers[id], SYNC_WORK_BIT);
		}
	}
}
/*}}}*/
/*{{{  static void *bsc_thread (void *arg)*/
/*
 *	Clone enters here -- in fact, it never leaves...
 */
static void *bsc_thread (void *arg)
{
	bsc_pool_t *pool = (bsc_pool_t *) arg;
	sigset_t block;
	pthread_t self;

	self = pthread_self ();

	/* block all signals */
	sigemptyset (&block);
	sigaddset (&block, SIGTERM);
	sigaddset (&block, SIGHUP);
	sigaddset (&block, SIGCHLD);
	sigaddset (&block, SIGBUS);
	sigaddset (&block, SIGPIPE);
	sigaddset (&block, SIGUSR1);
	sigaddset (&block, SIGUSR2);
	sigaddset (&block, SIGINT);
	sigaddset (&block, SIGSTOP);
	sigaddset (&block, SIGCONT);
	pthread_sigmask (SIG_BLOCK, &block, NULL);

	/* set cancellation type */
	pthread_setcanceltype (PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

	pthread_mutex_lock (&(pool->lock));
	pthread_cleanup_push (bsc_cleanup_pool, pool);
	
	for (;;) {
		if (pool->fptr != NULL) {
			bsc_batch_t *job 	= pool->fptr;
			pool->fptr 		= job->next;

			pthread_mutex_unlock (&(pool->lock));
			
			/* execute call */
			if (job->bsc.adjust == 0) {
				weak_write_barrier ();
				job->bsc.func ((word *) job->bsc.ws_arg);
				weak_write_barrier ();
			} else {
				word temp;

				job->bsc.thread = (word) &self;
				job->bsc.ws_arg = job->bsc.ws_arg + job->bsc.adjust;
				pthread_cleanup_push (bsc_cleanup_prepool, pool);
				pthread_cleanup_push (bsc_cleanup_job, job);

				weak_write_barrier ();
				
				/* start: cancellable region (after swap) */
				temp = atw_swap ((word *) job->bsc.ws_arg[-1], (word) job);
				if (temp == 0 || temp == 2) {
					weak_write_barrier ();
					job->bsc.func ((word *) job->bsc.ws_arg);
					weak_write_barrier ();
					
					if (!atw_cas ((word *) job->bsc.ws_arg[-1], (word) job, 0)) {
						/* we got cancelled - shouldn't be here */
						pthread_testcancel ();
						BMESSAGE ("killable bsyscall reached bad place.\n");
						pthread_exit (NULL);
					}
				} else {
					temp = atw_swap ((word *) job->bsc.ws_arg[-1], 2);
					if (temp != (word) job) {
						/* we got cancelled - shouldn't be here */
						pthread_testcancel ();
						BMESSAGE ("killable bsyscall reached bad place.\n");
						pthread_exit (NULL);
					}
				}
				/* end: cancellable region */

				pthread_cleanup_pop (0);
				pthread_cleanup_pop (0);
			}

			/* clean-up (reschedule process, etc) */
			bsc_cleanup_job (job);
			pthread_mutex_lock (&(pool->lock));
		} else {
			att_safe_inc (&(pool->waiting));

			if (pool->number == 0) {
				struct timespec timeout;
				struct timeval tv;

				memset (&timeout, 0, sizeof (timeout));
				gettimeofday (&tv, NULL);
				timeout.tv_sec = tv.tv_sec + 60;

				if (pthread_cond_timedwait (&(pool->signal), &(pool->lock), &timeout) < 0) {
					compiler_barrier ();
					if (pool->fptr == NULL) {
						att_safe_dec (&(pool->waiting));
					}
					break;
				}
			} else {
				pthread_cond_wait (&(pool->signal), &(pool->lock));
			}
		}

		if (att_safe_val (&(pool->shutdown))) {
			break;
		}
	}
	
	/* pop and execute pool cleanup */
	pthread_cleanup_pop (1);

	return NULL;
}
/*}}}*/
/*{{{  void bsyscall_dispatch (bsc_batch_t *job)*/
/*
 *	Dispatches a thread to handle a blocking system call
 */
void bsyscall_dispatch (bsc_batch_t *job)
{
	bsc_pool_t *pool;

	/* pick pool */
	if (PAffinity (job->priofinity) == 0) {
		pool = &(pools[0]);
	} else {
		pool = &(pools[(bsr (PAffinity (job->priofinity))) + 1]);
	}

	/* lock */
	pthread_mutex_lock (&(pool->lock));

	/* update pending call count */
	att_safe_inc (&pending);

	/* enqueue job */
	job->next = NULL;
	if (pool->fptr == NULL) {
		pool->fptr = job;
		pool->bptr = job;
	} else {
		pool->bptr->next = job;
		pool->bptr = job;
	}

	/* start or signal a thread */
	if (pool->number == 0) {
		if (att_safe_val (&(pool->waiting))) {
			att_safe_dec (&(pool->waiting));
			pthread_cond_signal (&(pool->signal));
		} else if (att_safe_val (&(pool->running)) < MAX_BSC_THREADS) {
			pthread_t thread;
			int ret;
			if ((ret = pthread_create (&thread, NULL, bsc_thread, pool))) {
				BMESSAGE ("pthread_create failed, ret = %d, errno = %d\n", ret, errno);
				if (att_safe_val (&(pool->running)) == 0)
					ccsp_kernel_exit (1, 0);
			} else {
				att_safe_inc (&(pool->running));
			}
		}
	} else {
		if (att_safe_val (&(pool->waiting))) {
			att_safe_dec (&(pool->waiting));
			pthread_cond_signal (&(pool->signal));
		} else if (!att_safe_val (&(pool->running))) {
			pthread_t thread;
			int ret;
			if ((ret = pthread_create (&thread, NULL, bsc_thread, pool))) {
				BMESSAGE ("pthread_create failed, ret = %d, errno = %d", ret, errno);
				ccsp_kernel_exit (1, 0);
			} else {
				att_safe_set (&(pool->running), 1);
			}
		}
	}

	/* unlock */
	pthread_mutex_unlock (&(pool->lock));
}
/*}}}*/
/*{{{  int bsyscall_kill (word *ptr)*/
/*
 *	terminates a blocking system call (gulp)
 */
int bsyscall_kill (word *ptr)
{
	bsc_batch_t 	*job;
	pthread_t	thread;
	word 		temp;
	int 		result;

	temp = atw_swap (ptr, 1);
	switch (temp) {
		case 0: /* job hasn't started yet */
			result = -1;
			break;
		case 1: /* thread termination already started */
			result = 0;
			break;
		case 2: /* thread has already terminated */
			atw_set (ptr, 0);
			result = 1;
			break;
		default:
			job 	= (bsc_batch_t *) temp;
			thread 	= *((pthread_t *) job->bsc.thread);
			pthread_cancel (thread);
			result = 0;
			break;
	}

	return result;
}
/*}}}*/
/*{{{  void _killcall (word *w)*/
/*
 * 	Legacy support for occam:
 *	PROC C.killcall (CHAN OF INT c, INT status)
 */
void _killcall (word *w)
{
	word	*ptr 	= (word *)(w[0]);
	int	*result = (int *)(w[1]);
	
	*result = bsyscall_kill (ptr);
}
/*}}}*/
/*{{{  int bsyscalls_pending (void)*/
/*
 *	int bsyscalls_pending (void)
 *	Returns number of clones currently blocked
 */
int bsyscalls_pending (void)
{
	strong_read_barrier ();
	return (int) att_safe_val (&pending);
}
/*}}}*/
#else	/* !BLOCKING_SYSCALLS */

#include <stddef.h>
#include <kernel.h>
#include <rts.h>

/*{{{  void _killcall (word *w)*/
/*
 *	void _killcall (word *w)
 *	PROC C.killcall (CHAN OF INT c, INT status)
 *	dummy function for when blocking calls disabled
 */
void _killcall (word *w)
{
	return;
}
/*}}}*/

#if defined(BLOCKING_SYSCALLS_UNSUPPORTED)
void _blocking_syscalls_unsupported (void)
#else
void _blocking_syscalls_disabled (void)
#endif
{
	return;
}

#endif	/* !BLOCKING_SYSCALLS */

/*{{{  void *bsyscalls_set_cleanup (void (*cleanup)(void *))*/
void *bsyscalls_set_cleanup (void (*cleanup)(void *))
{
	BMESSAGE0 ("unsupported bsyscalls_set_cleanup() called");
	ccsp_kernel_exit (1, 0);
	return NULL;
}
/*}}}*/

#endif	/* !RMOX_BUILD */

