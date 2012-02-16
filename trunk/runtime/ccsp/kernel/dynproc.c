/*
 *	dynproc.c - Dynamic process management
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

/*{{{  general includes*/
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef RMOX_BUILD
	#include <rmox_if.h>
#else	/* !RMOX_BUILD */
	#include <stdio.h>
	#include <string.h>
	#include <sys/types.h>
	#include <sys/fcntl.h>
	#include <unistd.h>
#endif	/* !RMOX_BUILD */

#include <kernel.h>
#include <rts.h>
/*}}}*/

#if defined(DYNAMIC_PROCS) && !defined(RMOX_BUILD)

/*{{{  dynproc includes/defines*/
#include <dlfcn.h>

#define __DYNPROC_C

#include <deadlock.h>
#include <dynproc.h>
#include <dmem_if.h>
/*}}}*/

/*{{{  forward declarations*/
static void fixup_channel_words (d_process *p);
static int workspace_in_dprocess (word *wptr, d_process *p);
/*}}}*/
/*{{{  private variables*/
static d_process *dyn_process_list = NULL;
static d_process_msqueue *dyn_process_msqueue = NULL;
static d_process_libnamehash *dyn_process_lhash = NULL;
/*}}}*/

/*{{{  void _dynproc_dumpinfo (int *ws)*/
/*
 *	void _dynproc_dumpinfo (int *ws)
 *	dumps information about dynamic processes (debug mostly, called from occam)
 */
void _dynproc_dumpinfo (int *ws)
{
	d_process *tp_pq;
	d_process_msqueue *tp_mq;
	d_process_libnamehash *tp_lh;

	MESSAGE ("dynamic processes information:\n");
	MESSAGE ("------------------------------\n\n");
	for (tp_pq = dyn_process_list; tp_pq; tp_pq = tp_pq->next) {
		MESSAGE ("DPCB %p (par:%p, chld:%p, q:%p,%p) mem %p\n", tp_pq, tp_pq->parent, tp_pq->children, tp_pq->prev, tp_pq->next, tp_pq->mem_start);
		MESSAGE ("     ws %d @ %p (base @ %p), vs %d @ %p\n", tp_pq->ws_size, tp_pq->ws_ptr, tp_pq->ws_base, tp_pq->vs_size, tp_pq->vs_ptr);
		MESSAGE ("     ms %d MSCP %p\n", tp_pq->ms_size, tp_pq->ms_ptr);
		MESSAGE ("     occam caller Wptr %p, raddr %p\n", (word *)tp_pq->holding_wptr, (char *)tp_pq->holding_raddr);
		MESSAGE ("     proc [%s] entered @ %p, suspended @ %p, running %d\n", tp_pq->proc_name, tp_pq->entrypoint, tp_pq->suspended, tp_pq->is_running);
	}
	for (tp_mq = dyn_process_msqueue; tp_mq; tp_mq = tp_mq->next) {
		MESSAGE ("MSCB %p (q:%p) data @ %p, hashcode %16.16LX, in-use @ %p\n", tp_mq, tp_mq->next, tp_mq->msptr, tp_mq->hashcode, tp_mq->in_use);
	}
	for (tp_lh = dyn_process_lhash; tp_lh; tp_lh = tp_lh->next) {
		MESSAGE ("LHNB %p (q:%p) handle @ %p, hashcode %16.16LX, filename [%s]\n", tp_lh, tp_lh->next, tp_lh->lhandle, tp_lh->hashcode, tp_lh->filename);
	}
	return;
}
/*}}}*/
/*{{{  int faulting_dynproc (word **wptr_ptr, unsigned int *raddr_ptr, char *fault, d_process **tp_return)*/
/*
 *	int faulting_dynproc (word **wptr_ptr, unsigned int *raddr_ptr, char *fault, d_process **tp_return)
 *	called to see if a dynamic process is responsible for a run-time error,
 *	and if so, arrange for its termination (and return to occam for the
 *	calling process).
 */
int faulting_dynproc (word **wptr_ptr, unsigned int *raddr_ptr, char *fault, d_process **tp_return)
{
	d_process *tp;

#if 0
MESSAGE ("faulting_dynproc: having a look for broken dynamic processes... *wptr_ptr = %p, *raddr_ptr = %p\n", *wptr_ptr, *raddr_ptr);
_dynproc_dumpinfo (NULL);
#endif
	for (tp = dyn_process_list; tp; tp = tp->next) {
		if (tp->is_running && ((int)(*wptr_ptr) >= (int)tp->ws_base) && ((int)(*wptr_ptr) < ((int)tp->ws_base + tp->ws_size + (4 * sizeof (int))))) {
			if (!not_on_any_queue ((unsigned int)tp->ws_base, ((unsigned int)tp->ws_base) + tp->ws_size)) {
				BMESSAGE ("dynamic process at %p generated %s but is still active, removing from queues\n", *wptr_ptr, fault);
				remove_from_any_queue ((unsigned int)tp->ws_base, ((unsigned int)tp->ws_base) + tp->ws_size);
			}
			if (tp->ms_ptr) {
				BMESSAGE ("dynamic process has mobilespace at %p, invalidating it\n", tp->ms_ptr->msptr);
				tp->ms_ptr->hashcode = 0LL;
			}
			/* this one.. */
			*wptr_ptr 	= tp->holding_wptr;
			*raddr_ptr 	= tp->holding_raddr;
			*tp_return 	= tp;
			*(tp->result) 	= DPROCESS_FAULTED;
			tp->is_running 	= 0;
			ccsp_take_ws ((char *)tp->ws_ptr - tp->ws_size);
			return 1;
		}
	}
	return 0;
}
/*}}}*/
/*{{{  static unsigned long long dynproc_libnamehash (char *name)*/
/*
 *	unsigned long long dynproc_libnamehash (char *name)
 *	generates a hashcode for a library name
 */
static unsigned long long dynproc_libnamehash (char *name)
{
	unsigned long long hashcode = 0LL;
	unsigned long long thash = 0LL;
	int i;

	if (!name) {
		return 0LL;
	}
	for (i=0; name[i] != '\0'; i++) {
		thash >>= 5;
		thash |= (((unsigned long long)(name[i] - 0x20)) << 58);
		if (!((i+1) & 0x7)) {
			hashcode ^= thash;
			thash = 0;
		}
	}
	return hashcode;
}
/*}}}*/
/*{{{  static d_process_libnamehash *get_libnamehash (char *fname, void *lhandle)*/
/*
 *	d_process_libnamehash *get_libnamehash (char *fname, void *lhandle)
 *	loads a library name hash entry
 */
static d_process_libnamehash *get_libnamehash (char *fname, void *lhandle)
{
	d_process_libnamehash *tmp;

	if (!fname || !lhandle) {
		return NULL;
	}
	for (tmp = dyn_process_lhash; tmp; tmp = tmp->next) {
		if (!strcmp (fname, tmp->filename) && (lhandle == tmp->lhandle)) {
			return tmp;
		} else if (lhandle == tmp->lhandle) {
			/* recycle this one (cannot be re-using old pointer) */
			break;
		}
	}
	if (!tmp) {
		/* allocate a fresh one */
		tmp = (d_process_libnamehash *)dmem_alloc (sizeof (d_process_libnamehash));
		if (!tmp) {
			BMESSAGE ("out of memory allocating dynamic library name hash block..\n");
			return NULL;
		}
	} else {
		dmem_release (tmp->filename);
	}
	tmp->filename = (char *)dmem_alloc (strlen (fname) + 1);
	if (!tmp->filename) {
		BMESSAGE ("out of memory allocating dynamic library name..\n");
		return NULL;
	}
	strcpy (tmp->filename, fname);
	tmp->lhandle = lhandle;
	tmp->hashcode = dynproc_libnamehash (tmp->filename);
	tmp->next = dyn_process_lhash;
	dyn_process_lhash = tmp;
	return tmp;
}
/*}}}*/
/*{{{  static unsigned long long get_libnamehash_ptr (void *lhandle)*/
/*
 *	unsigned long long *get_libnamehash_ptr (void *lhandle)
 *	looks up a library name hash by pointer
 */
static unsigned long long get_libnamehash_ptr (void *lhandle)
{
	d_process_libnamehash *tmp;

	for (tmp = dyn_process_lhash; tmp; tmp = tmp->next) {
		if (tmp->lhandle == lhandle) {
			return tmp->hashcode;
		}
	}
	return 0LL;
}
/*}}}*/
#if defined(DM_DEBUG)
/*{{{  static char *get_libname (void *lhandle)*/
/*
 *	looks up a library name by pointer
 */
static char *get_libname (void *lhandle)
{
	d_process_libnamehash *tmp;

	for (tmp = dyn_process_lhash; tmp; tmp = tmp->next) {
		if (tmp->lhandle == lhandle) {
			return tmp->filename;
		}
	}
	return NULL;
}
/*}}}*/
#endif
/*{{{  static d_process_msqueue *allocate_dynproc_mobilespace (unsigned long long hashcode, d_process *dproc, int msbytes)*/
/*
 *	d_process_msqueue *allocate_dynproc_mobilespace (unsigned long long hashcode, d_process *dproc, int msbytes)
 *	allocated mobilespace for a dynamic process
 */
static d_process_msqueue *allocate_dynproc_mobilespace (unsigned long long hashcode, d_process *dproc, int msbytes)
{
	d_process_msqueue *tmp;

	if (!msbytes) {
		return NULL;
	}
	for (tmp = dyn_process_msqueue; tmp; tmp = tmp->next) {
		if ((tmp->hashcode == hashcode) && (tmp->in_use = NULL)) {
			tmp->in_use = dproc;
			return tmp;
		}
	}
	/* allocate a fresh one */
	tmp = (d_process_msqueue *)dmem_alloc (sizeof (d_process_msqueue));
	if (!tmp) {
		BMESSAGE ("eugh, ran out of memory allocating dynamic mobile-space control block..\n");
		return NULL;
	}
	tmp->hashcode = hashcode;
	tmp->in_use = dproc;
	tmp->msptr = (word *)dmem_alloc (msbytes);
	if (!tmp->msptr) {
		BMESSAGE ("eugh, ran out of memory allocating dynamic mobile-space..\n");
		dmem_release (tmp);
		return NULL;
	}
	tmp->next = dyn_process_msqueue;
	dyn_process_msqueue = tmp;
	return tmp;
}
/*}}}*/
/*{{{  static unsigned long long dynproc_hashcode (char *procname, int wsbytes, int vsbytes, int msbytes, void *lhandle)*/
/*
 *	unsigned long long dynproc_hashcode (char *procname, int wsbytes, int vsbytes, int msbytes, void *lhandle)
 *	generates a hash for a dynamic process
 */
static unsigned long long dynproc_hashcode (char *procname, int wsbytes, int vsbytes, int msbytes, void *lhandle)
{
	unsigned long long hash = 0LL;
	int i;

	for (i=0; procname[i] != '\0'; i++) {
		unsigned long long leftover = (hash >> 58);

		hash = (hash << 6) | (leftover ^ ((unsigned long long)(procname[i] - 0x20)));
	}
	hash |= (((unsigned long long)wsbytes << 32) | ((unsigned long long)vsbytes << 16) | (unsigned long long)msbytes);
	hash ^= get_libnamehash_ptr (lhandle);

	return hash;
}
/*}}}*/
/*{{{  static void do_ccsp_openlib (char *filename, int namelen, void **handle)*/
/*
 *	void do_ccsp_openlib (char *filename, int namelen, void **handle)
 *	loads a dynamic library
 */
static void do_ccsp_openlib (char *filename, int namelen, void **handle)
{
	int bufsize = (FILENAME_MAX * 2) - 1;
	char fname[FILENAME_MAX * 2];
	int noffs = 0;

	/* should do this really.. */
	if ((*filename != '/') && (getcwd (fname, bufsize) != NULL)) {
		noffs = strlen (fname);
		if ((noffs > 0) && (fname[noffs - 1] != '/')) {
			fname[noffs] = '/';
			noffs++;
			fname[noffs] = '\0';
		}
	}
	if ((noffs + namelen) > bufsize) {
		namelen = (bufsize - (noffs + 1));
	}
	memcpy (fname + noffs, filename, namelen);
	fname[namelen + noffs] = '\0';
	*handle = dlopen (fname, RTLD_LAZY|RTLD_GLOBAL);
	if (!(*handle)) {
		BMESSAGE ("failed to ccsp_openlib [%s] because: %s\n", fname, dlerror());
	} else {
		get_libnamehash (fname, *handle);
		#if defined(DM_DEBUG) && (DM_DEBUG==1)
			extadd_elf_mem (fname);
		#endif
	}
	return;
}
/*}}}*/
/*{{{  static void do_ccsp_closelib (void *handle)*/
/*
 *	void do_ccsp_closelib (void *handle)
 *	closes a dynamic library
 */
static void do_ccsp_closelib (void *handle)
{
	if (handle != (void *)0) {
		#if defined(DM_DEBUG) && (DM_DEBUG==1)
			/* find filename and remove ELF blocks */
			char *fname = get_libname (handle);

			if (fname) {
				extdel_elf_mem (fname);
			}
		#endif
		dlclose (handle);
	}
}
/*}}}*/
/*{{{  static void do_ccsp_loadproc (void *libhandle, char *pname, int plen, int *process)*/
/*
 *	void do_ccsp_loadproc (void *libhandle, char *pname, int plen, int *process)
 *	loads a dynamic process (sets up initial workspace, etc.)
 */
static void do_ccsp_loadproc (void *libhandle, char *pname, int plen, int *process)
{
	d_process *tp;
	d_process_msqueue *msinfo;
	char symname[128], *tptr, *error, *ch;
	void *func;
	int i, j;
	int *new_wsbytes;
	int *new_vsbytes;
	int *new_msbytes;

	if ((plen > 112) || !libhandle) {
		BMESSAGE ("load_dynamic_process: name too long!\n");
		*process = 0;
		return;
	}
	/* get `O_pname' symbol */
	symname[0] = 'O';
	symname[1] = '_';
	memcpy (symname+2, pname, plen);
	symname[plen+2] = '\0';
	for (ch=symname+2; *ch != '\0'; ch++) {
		if (*ch == '.') {
			*ch = '_';
		}
	}
	func = dlsym (libhandle, symname);
	error = dlerror();
	if (error) {
		BMESSAGE ("load_dynamic_process: symbol %s not found in library %p\n", symname, libhandle);
		*process = 0;
		return;
	}
	/* get `pname_wsbytes' symbol */
	symname[0] = '_';
	memcpy (symname + 1, pname, plen);
	memcpy (symname + 1 + plen, "_wsbytes", 8);
	symname[plen + 9] = '\0';
	for (ch=symname+1; *ch != '\0'; ch++) {
		if (*ch == '.') {
			*ch = '_';
		}
	}
	new_wsbytes = (int *)dlsym (libhandle, symname);
	error = dlerror ();
	if (error) {
		BMESSAGE ("load_dynamic_process: symbol %s not found in library %p\n", symname, libhandle);
		*process = 0;
		return;
	}
	/* get 'pname_vsbytes' symbol */
	symname[plen + 2] = 'v';
	new_vsbytes = (int *)dlsym (libhandle, symname);
	error = dlerror ();
	if (error) {
		BMESSAGE ("load_dynamic_process: symbol %s not found in library %p\n", symname, libhandle);
		*process = 0;
		return;
	}
	/* get 'pname_msbytes' symbol */
	symname[plen + 2] = 'm';
	new_msbytes = (int *)dlsym (libhandle, symname);
	error = dlerror ();
	if (error) {
		BMESSAGE ("load_dynamic_process: symbol %s not found in library %p\n", symname, libhandle);
		*process = 0;
		return;
	}
	/* create new workspace */
	i = (*new_wsbytes + 64 + *new_vsbytes + sizeof (d_process));
	i += (4 - (i % 4));
	tptr = (char *)dmem_alloc (i);
	if (!tptr) {
		BMESSAGE ("load_dynamic_process: out of memory (wanted %d bytes)\n", i);
		*process = 0;
		return;
	}
	/* allocate any mobilespace:
	 *	this has to be done fairly carefully, since we MUST NOT ever release this memory;
	 *	stuff might have migrated elsewhere.  We can re-cycle it tho :)
	 */
	if (*new_msbytes) {
		int k;

		symname[0] = 'O';
		symname[1] = '_';
		memcpy (symname+2, pname, plen);
		symname[plen+2] = '\0';
		
		msinfo = allocate_dynproc_mobilespace (dynproc_hashcode (symname, *new_wsbytes, *new_vsbytes, *new_msbytes, libhandle), NULL, *new_msbytes);
		if (!msinfo) {
			*process = 0;
			return;
		}
		for (k=0; k<(*new_msbytes >> 2); k++) {
			((int *)(msinfo->msptr))[k] = 0x80000000;
		}
	} else {
		msinfo = NULL;
	}

	/* place + fill d_process structure */
	tp = (d_process *)tptr;
	tp->mem_start 		= tptr;
	tp->ws_size 		= *new_wsbytes;
	tp->vs_size 		= *new_vsbytes;
	tp->ms_size 		= *new_msbytes;
	j = sizeof (d_process);
	j = ((j + 3) >> 2) << 2;	/* round up to word boundary */
	tp->ws_ptr 		= (word *)(((char *)tptr) + j + tp->ws_size);
	tp->ws_base 		= (word *)(((char *)tp->ws_ptr) - tp->ws_size);
	tp->vs_ptr 		= (word *)(((char *)tp->ws_ptr) + 64);
	tp->ms_ptr 		= msinfo;
	if (msinfo) {
		msinfo->in_use	= tp;
	}
	/* tp->ms_ptr = (word *)ms_tptr; */
	tp->holding_wptr 	= NULL;
	tp->holding_raddr 	= 0;
	tp->holding_priofinity 	= 0;
	tp->lhandle 		= libhandle;
	tp->proc_name 		= (char *) dmem_alloc (plen + 1);
	memcpy (tp->proc_name, pname, plen);
	tp->proc_name[plen] 	= '\0';
	/* store function at Wptr[Temp] for new process */
	tp->entrypoint 		= (char *)func;
	tp->suspended 		= NULL;
	tp->is_running 		= 0;
	#ifdef DEBUG_DYNPROC
		MESSAGE ("*** dynamic process loaded (structure allocated at %p:\n", tp);
		MESSAGE ("***     (mem_start=%p [for %d], ws_ptr=%p, ws_size=%d, holding_wptr=%p, holding_raddr=%p)\n",
			tp->mem_start, i, tp->ws_ptr, tp->ws_size, (void *)tp->holding_wptr, (void *)tp->holding_raddr);
		MESSAGE ("***     (entrypoint=%p, lhandle=%p, result=%p, suspended=%p)\n",
			tp->entrypoint, tp->lhandle, tp->result, tp->suspended);
	#endif
	*process = (int)tp;

	/* add to the queue */
	tp->next = NULL;
	tp->prev = NULL;
	tp->children = NULL;
	tp->parent = NULL;
	if (dyn_process_list) {
		dyn_process_list->prev = tp;
		tp->next = dyn_process_list;
	} else {
		tp->next = NULL;
	}
	tp->prev = NULL;
	dyn_process_list = tp;
	return;
}
/*}}}*/
/*{{{  static void do_ccsp_freeproc (int *process)*/
/*
 *	void do_ccsp_freeproc (int *process)
 *	frees a dynamic process (deallocates workspace, etc.)
 */
static void do_ccsp_freeproc (int *process)
{
	d_process *tp;

	tp = (d_process *)*process;
	if (tp->is_running) {
		BMESSAGE ("fatal: ccsp.freeproc called on running process\n");
		ccsp_bad_exit ();
	}
	if (tp->suspended) {
		if (tp->suspended->ichans) {
			dmem_release (tp->suspended->ichans);
			tp->suspended->ichans = NULL;
		}
		if (tp->suspended->ochans) {
			dmem_release (tp->suspended->ochans);
			tp->suspended->ochans = NULL;
		}
		dmem_release (tp->suspended);
		tp->suspended = NULL;
	}
	if (tp->proc_name) {
		dmem_release (tp->proc_name);
		tp->proc_name = NULL;
	}
	if (tp->ms_size) {
		tp->ms_ptr->in_use = NULL;
		tp->ms_ptr = NULL;
	}
	if (!tp->prev && !tp->next) {
		dyn_process_list = NULL;
	} else if (!tp->prev) {
		tp->next->prev = NULL;
		dyn_process_list = tp->next;
	} else if (!tp->next) {
		tp->prev->next = NULL;
	} else {
		tp->prev->next = tp->next;
		tp->next->prev = tp->prev;
	}

	dmem_release (tp->mem_start);
	*process = 0;
	return;
}
/*}}}*/
/*{{{  static void do_ccsp_suspendproc (d_process *tp, int *result)*/
/*
 *	void do_ccsp_suspendproc (d_process *tp, int *result)
 *	suspends a dynamic process
 */
static void do_ccsp_suspendproc (d_process *tp, int *result)
{
	int i, saved;
	unsigned int ws_base, ws_limit;
	unsigned int **ca_base;
	d_suspended_inf *t_suspended;
	word *xx_wptr;

	ws_base = (unsigned int)tp->ws_ptr - tp->ws_size;
	ws_limit = (unsigned int)tp->ws_ptr;
	#ifdef DEBUG_DYNPROC
		MESSAGE ("debug: do_ccsp_suspendproc (%p, %p)\ndebug: checking [base=0x%x, limit=0x%x, size=0x%x]", tp, result, ws_base, ws_limit, ws_limit - ws_base);
	#endif
	if (!not_on_any_queue (ws_base, ws_limit)) {
		/* component on run queue or timer queue */
		#ifdef DEBUG_DYNPROC
			MESSAGE ("debug: process component(s) on run/timer queues\n");
		#endif
		*result = -1;
		return;
	}
	#ifdef DEBUG_DYNPROC
		MESSAGE ("debug: saving channel words..\n");
	#endif
	/* save in/out channel params (and reset channel words) */
	if (!tp->suspended) {
		t_suspended = (d_suspended_inf *)dmem_alloc (sizeof (d_suspended_inf));
		t_suspended->num_ichans = (int)(tp->ws_ptr[3]);
		t_suspended->ichans = (int *)dmem_alloc ((t_suspended->num_ichans + 1) * sizeof (int));
		t_suspended->num_ochans = (int)(tp->ws_ptr[5]);
		t_suspended->ochans = (int *)dmem_alloc ((t_suspended->num_ochans + 1) * sizeof (int));
	} else {
		t_suspended = tp->suspended;
	}
	saved = 0;
	ca_base = (unsigned int **)tp->ws_ptr[2];
	for (i=0; i<t_suspended->num_ichans; i++) {
		if (!(*ca_base)[i]) {
			t_suspended->ichans[i] = 0;
		} else if (((*ca_base)[i] < ws_base) || ((*ca_base)[i] >= ws_limit)) {
			/* not one of our channels */
MESSAGE ("debug: not one of my input channels (do_ccsp_suspendproc)\n");
			t_suspended->ichans[i] = 0;
		} else {
			t_suspended->ichans[i] = (*ca_base)[i];
			*ca_base[i] = NotProcess_p;
			saved++;
		}
	}
	ca_base = (unsigned int **)tp->ws_ptr[4];
	for (i=0; i<t_suspended->num_ochans; i++) {
		if (!(*ca_base)[i]) {
			t_suspended->ochans[i] = 0;
		} else if (((*ca_base)[i] < ws_base) || ((*ca_base)[i] >= ws_limit)) {
			/* not one of our channels */
MESSAGE ("debug: not one of my output channels (do_ccsp_suspendproc)\n");
			t_suspended->ochans[i] = 0;
		} else {
			t_suspended->ochans[i] = (*ca_base)[i];
			(*ca_base)[i] = NotProcess_p;
			saved++;
		}
	}
	#ifdef DEBUG_DYNPROC
		MESSAGE ("debug: %d channel words saved.\n", saved);
	#endif
	/* these two are filled in later.. */
	t_suspended->wptr = NULL;
	t_suspended->return_addr 	= 0;
	t_suspended->priofinity 	= 0;
	tp->suspended 			= t_suspended;
	/* tranx86 does something special here: upon return we jump back into the occam kernel */
	*(tp->result) 			= DPROCESS_SUSPENDED;
	xx_wptr 			= (word *)tp->holding_wptr;
	xx_wptr[Iptr] 			= tp->holding_raddr;
	xx_wptr[Priofinity] 		= tp->holding_priofinity;
	do_queue_process (xx_wptr);
	*result = 0;
	return;
}
/*}}}*/
/*{{{  static void do_ccsp_sizeproc (int *process, int *bytes)*/
/*
 *	void do_ccsp_sizeproc (int *process, int *bytes)
 *	sizes a process (bytes required to store it)
 */
static void do_ccsp_sizeproc (int *process, int *bytes)
{
	d_process *tp;

	tp = (d_process *)*process;
	*bytes = -1;
	return;
}
/*}}}*/
/*{{{  static void do_ccsp_writeproc (int *process, char *fname, int flen, int *result)*/
/*
 *	void do_ccsp_writeproc (int *process, char *fname, int flen, int *result)
 *	writes a (suspended) process to disk
 */
static void do_ccsp_writeproc (int *process, char *fname, int flen, int *result)
{
	d_process *tp;
	static char dp_wfname[FILENAME_MAX];
	int fd;
	dp_header dph;
	int i;
	dp_offset *offsets, *new_offsets;
	int num_offs, max_offs, new_max_offs;
	word *ws_start, *ws_limit, *ws_walk;
	int *c_offsets;

	c_offsets = NULL;
	tp = (d_process *)*process;
	if (tp->is_running || !tp->suspended) {
		*result = -1;
		return;
	}
	if (flen >= FILENAME_MAX) {
		flen = FILENAME_MAX - 1;
	}
	strncpy (dp_wfname, fname, flen);
	dp_wfname[flen] = '\0';
	fd = open (dp_wfname, O_CREAT | O_TRUNC | O_WRONLY, 0600);
	if (fd < 0) {
		*result = -1;
		return;
	}
	memcpy (dph.magic, "\033SOPv0.9", 8);
	dph.ws_size = tp->ws_size;
	dph.vs_size = tp->vs_size;
	dph.num_ichans = tp->suspended->num_ichans;
	dph.num_ochans = tp->suspended->num_ochans;
	dph.pnamelen = strlen (tp->proc_name) + 1;		/* get NULL terminator as well */
	dph.offtabsize = 0;
	dph.flags = 0;
	/* examine the workspace and pick out offsets into self */
	ws_start = (word *)((char *)(tp->ws_ptr) - (tp->ws_size));
	ws_limit = tp->ws_ptr;
	num_offs = 0;
	max_offs = 63;
	offsets = (dp_offset *)dmem_alloc (max_offs * sizeof (dp_offset));
	memset (offsets, 0, max_offs * sizeof (dp_offset));
	for (ws_walk=ws_start, i=0; ws_walk <= ws_limit; ws_walk++, i++) {
		if ((*ws_walk >= (word)ws_start) && (*ws_walk <= (word)ws_limit)) {
			/* pointer into workspace or vectorspace */
			if (num_offs == max_offs) {
				/* increase size of array */
				new_max_offs = ((max_offs + 1) * 2) - (((max_offs + 1) / 2) + 1);
				new_offsets = (dp_offset *)dmem_alloc (new_max_offs * sizeof (dp_offset));
				memcpy (new_offsets, offsets, max_offs * sizeof (dp_offset));
				memset (&(new_offsets[max_offs]), 0, (new_max_offs - max_offs) * sizeof (dp_offset));
				dmem_release (offsets);
				offsets = new_offsets;
				new_offsets = NULL;
				max_offs = new_max_offs;
			}
			offsets[num_offs].ws_offset = i;
			offsets[num_offs].adjustment = (int)(*ws_walk - (word)tp->mem_start);
			num_offs++;
		}
	}
	dph.offtabsize = num_offs;

	/* write out header */
	if (write (fd, (char *)&dph, sizeof (dph)) != sizeof (dph)) {
		goto cleanup_out_error;
	}

	/* write out process name */
	if (write (fd, tp->proc_name, dph.pnamelen) != dph.pnamelen) {
		goto cleanup_out_error;
	}

	/* generate channel offsets */
	i = (tp->suspended->num_ichans > tp->suspended->num_ochans) ? tp->suspended->num_ichans : tp->suspended->num_ochans;
	if (i) {
		c_offsets = (int *)dmem_alloc (i * sizeof (int));

		if (tp->suspended->num_ichans) {
			/* input channel offsets */
			for (i=0; i<tp->suspended->num_ichans; i++) {
				c_offsets[i] = (tp->suspended->ichans[i] > 0) ? (int)((word)tp->suspended->ichans[i] - (word)tp->mem_start) : 0;
			}
			i *= sizeof (int);
			if (write (fd, (char *)c_offsets, i) != i) {
				goto cleanup_out_error;
			}
		}
		if (tp->suspended->num_ochans) {
			/* output channel offsets */
			for (i=0; i<tp->suspended->num_ochans; i++) {
				c_offsets[i] = (tp->suspended->ochans[i] > 0) ? (int)((word)tp->suspended->ochans[i] - (word)tp->mem_start) : 0;
			}
			i *= sizeof (int);
			if (write (fd, (char *)c_offsets, i) != i) {
				goto cleanup_out_error;
			}
		}
	}

	/* write out process workspace */
	if (write (fd, (char *)ws_start, tp->ws_size) != tp->ws_size) {
		goto cleanup_out_error;
	}

	/* write out offset tables */
	i = num_offs * sizeof (dp_offset);
	if (write (fd, (char *)offsets, i) != i) {
		goto cleanup_out_error;
	}

	if (c_offsets) {
		dmem_release (c_offsets);
		c_offsets = NULL;
	}
	if (offsets) {
		dmem_release (offsets);
		offsets = NULL;
	}
	close (fd);
	*result = 0;
	return;
cleanup_out_error:
	if (c_offsets) {
		dmem_release (c_offsets);
		c_offsets = NULL;
	}
	if (offsets) {
		dmem_release (offsets);
		offsets = NULL;
	}
	close (fd);
	*result = -1;
	return;
}
/*}}}*/
/*{{{  static void do_ccsp_readproc (int *process, char *fname, int flen, int *result)*/
/*
 *	void do_ccsp_readproc (int *process, char *fname, int flen, int *result)
 *	reads a (suspended) process from disk
 */
static void do_ccsp_readproc (int *process, char *fname, int flen, int *result)
{
	dp_header dph;
	int fd, i;
	static char dp_rname[FILENAME_MAX];
	d_process *tp;
	char *tmp_name;
	int *c_offsets;
	word *ws_start, *ws_limit;
	dp_offset dpo;

	c_offsets = NULL;
	tmp_name = NULL;
	tp = (d_process *)*process;
	if (tp->is_running || !tp->suspended) {
		*result = -1;
		return;
	}
	if (flen >= FILENAME_MAX) {
		flen = FILENAME_MAX - 1;
	}
	memcpy (dp_rname, fname, flen);
	dp_rname[flen] = '\0';
	fd = open (fname, O_RDONLY);
	if (fd < 0) {
		*result = -1;
		return;
	}

	/* read header */
	if (read (fd, (char *)&dph, sizeof (dph)) != sizeof (dph)) {
		close (fd);
		*result = -1;
		return;
	}
	if (memcmp (dph.magic, "\033SOPv0.9", 8)) {
		close (fd);
		*result = -1;
		return;
	}

	/* check various header stuff */
	if ((dph.ws_size != tp->ws_size) || (dph.vs_size != tp->vs_size) ||
		(dph.num_ichans != tp->suspended->num_ichans) || (dph.num_ochans != tp->suspended->num_ochans)) {
		close (fd);
		*result = -1;
		return;
	}

	/* check pnamelen => read name => check name */
	if (dph.pnamelen != strlen (tp->proc_name) + 1) {
		goto r_out_error;
	}
	tmp_name = (char *)dmem_alloc (dph.pnamelen);
	if (read (fd, tmp_name, dph.pnamelen) != dph.pnamelen) {
		goto r_out_error;
	}
	if (memcmp (tmp_name, tp->proc_name, dph.pnamelen)) {
		goto r_out_error;
	}

	/* allocate buffer read in channel offsets */
	i = (dph.num_ichans > dph.num_ochans) ? dph.num_ichans : dph.num_ochans;
	if (i) {
		c_offsets = (int *)dmem_alloc (i * sizeof (int));
		if (dph.num_ichans) {
			i = dph.num_ichans * sizeof (int);
			if (read (fd, (char *)c_offsets, i) != i) {
				goto r_out_error;
			}
			for (i=0; i<dph.num_ichans; i++) {
				if (!c_offsets[i]) {
					/* tp->suspended->ichans[i] = 0; */
				} else {
					tp->suspended->ichans[i] = (c_offsets[i] + (int)tp->mem_start);
				}
			}
		}
		if (dph.num_ochans) {
			i = dph.num_ochans * sizeof (int);
			if (read (fd, (char *)c_offsets, i) != i) {
				goto r_out_error;
			}
			for (i=0; i<dph.num_ochans; i++) {
				if (!c_offsets[i]) {
					/* tp->suspended->ochans[i] = 0; */
				} else {
					tp->suspended->ochans[i] = (c_offsets[i] + (int)tp->mem_start);
				}
			}
		}
		if (c_offsets) {
			dmem_release (c_offsets);
			c_offsets = NULL;
		}
	}

	/* read in the workspace image.. */
	ws_start = (word *)((char *)(tp->ws_ptr) - (tp->ws_size));
	ws_limit = tp->ws_ptr;
	if (read (fd, (char *)ws_start, tp->ws_size) != tp->ws_size) {
		goto r_out_error;
	}

	/* read in the offsets.. */
	for (i=0; i<dph.offtabsize; i++) {
		if (read (fd, (char *)&dpo, sizeof (dp_offset)) != sizeof (dp_offset)) {
			goto r_out_error;
		}
		ws_start[dpo.ws_offset] = dpo.adjustment + (int)tp->mem_start;
	}
	/* okay, should have done that OK... */

	if (tmp_name) {
		dmem_release (tmp_name);
		tmp_name = NULL;
	}
	close (fd);
	*result = 0;
	return;
r_out_error:
	if (c_offsets) {
		dmem_release (c_offsets);
		c_offsets = NULL;
	}
	if (tmp_name) {
		dmem_release (tmp_name);
		tmp_name = NULL;
	}
	close (fd);
	*result = -1;
	return;
}
/*}}}*/
/*{{{  static void do_ccsp_libhandleof (d_process *tp, int *hptr)*/
/*
 *	void do_ccsp_libhandleof (d_process *tp, int *hptr)
 *	returns the libhandle for a particular process
 */
static void do_ccsp_libhandleof (d_process *tp, int *hptr)
{
	*hptr = (int)tp->lhandle;
	return;
}
/*}}}*/
/*{{{  occam -> C stubs*/
/*
 *	occam -> C stubs
 */
void _ccsp_openlib (int *wsarg) { do_ccsp_openlib ((char *)(wsarg[0]), (int)(wsarg[1]), (void **)(wsarg[2])); }
void _ccsp_closelib (int *wsarg) { do_ccsp_closelib ((void *)(wsarg[0])); }
void _ccsp_loadproc (int *wsarg) { do_ccsp_loadproc ((void *)(wsarg[0]), (char *)(wsarg[1]), (int)(wsarg[2]), (int *)(wsarg[3])); }
void _ccsp_freeproc (int *wsarg) { do_ccsp_freeproc ((int *)(wsarg[0])); }
void _ccsp_suspendproc (int *wsarg) { do_ccsp_suspendproc ((d_process *)(wsarg[0]), (int *)(wsarg[1])); }
void _ccsp_sizeproc (int *wsarg) { do_ccsp_sizeproc ((int *)(wsarg[0]), (int *)(wsarg[1])); }
void _ccsp_writeproc (int *wsarg) { do_ccsp_writeproc ((int *)(wsarg[0]), (char *)(wsarg[1]), (int)(wsarg[2]), (int *)(wsarg[3])); }
void _ccsp_readproc (int *wsarg) { do_ccsp_readproc ((int *)(wsarg[0]), (char *)(wsarg[1]), (int)(wsarg[2]), (int *)(wsarg[3])); }
void _ccsp_libhandleof (int *wsarg) { do_ccsp_libhandleof ((d_process *)(wsarg[0]), (int *)(wsarg[1])); }

/*}}}*/
/*{{{  void dynproc_dumpprocess (d_process *p)*/
/*
 *	void dynproc_dumpprocess (d_process *p)
 *	dumps the workspace of a dynamic process
 */
void dynproc_dumpprocess (d_process *p)
{
	word *ws_start, *ws_limit;
	word *ws_walk;
	int i;

	MESSAGE ("dump of dynamic process workspace:\n");
	if (!p) {
		return;
	}
	ws_start = (word *)((char *)(p->ws_ptr) - (p->ws_size));
	ws_limit = (word *)((char *)(p->ws_ptr) + 64);
	MESSAGE ("process at %p: {mem_start=%p, ws_ptr=%p, vs_ptr=%p, ws_size=%d, vs_size=%d, holding_wptr=0x%x, holding_raddr=0x%x, entrypoint=%p, lhandle=%p, result=%p, suspended=%p\n",
		p, p->mem_start, p->ws_ptr, p->vs_ptr, p->ws_size, p->vs_size, (word) p->holding_wptr, p->holding_raddr, p->entrypoint, p->lhandle, p->result, p->suspended);
	for (i=0, ws_walk=ws_start; ws_walk <= ws_limit; ws_walk++, i = ((i+1)%4)) {
		if (!i) {
			MESSAGE ("0x%8.8x:  ", (int)ws_walk);
		}
		MESSAGE ("%8.8X  ", (unsigned int)*ws_walk);
		if (i == 3) {
			MESSAGE ("\n");
		}
	}
	return;
}
/*}}}*/
/*{{{  static int workspace_in_dprocess (word *wptr, d_process *p)*/
/*
 *	int workspace_in_dprocess (word *wptr, d_process *p)
 *	returns 1 if `wptr' is inside the workspace of `p'
 */
static int workspace_in_dprocess (word *wptr, d_process *p)
{
	word *ws_base, *ws_limit;

	ws_base = (word *)(((char *)p->ws_ptr) - p->ws_size);
	ws_limit = (word *)p->ws_ptr;
	if ((wptr < ws_base) || (wptr >= ws_limit)) {
		return 0;
	}
	return 1;
}
/*}}}*/
/*{{{  static void fixup_channel_words (d_process *p)*/
/*
 *	void fixup_channel_words (d_process *p)
 *	sets channel words (internal to the dynamic process) prior to process resume
 */
static void fixup_channel_words (d_process *p)
{
	unsigned int **ca_base;
	int i, fixedup;

	fixedup = 0;
	if (p->suspended->num_ichans != (int)p->ws_ptr[3]) {
		BMESSAGE ("fatal: resumed process given %d input channels (expected %d)\n", (int)p->ws_ptr[3], p->suspended->num_ichans);
		ccsp_bad_exit ();
	}
	ca_base = (unsigned int **)p->ws_ptr[2];
	for (i=0; i<p->suspended->num_ichans; i++) {
		if (p->suspended->ichans[i]) {
			if ((*ca_base)[i]) {
				BMESSAGE ("fatal: collision in input channel\n");
				ccsp_bad_exit ();
			}
			(*ca_base)[i] = p->suspended->ichans[i];
			fixedup++;
		}
	}
	if (p->suspended->num_ochans != (int)p->ws_ptr[5]) {
		BMESSAGE ("fatal: resumed process given %d input channels (expected %d)\n", (int)p->ws_ptr[5], p->suspended->num_ochans);
		ccsp_bad_exit ();
	}
	ca_base = (unsigned int **)p->ws_ptr[4];
	for (i=0; i<p->suspended->num_ochans; i++) {
		if (p->suspended->ochans[i]) {
			if ((*ca_base)[i]) {
				BMESSAGE ("fatal: collision in output channel\n");
				ccsp_bad_exit ();
			}
			(*ca_base)[i] = p->suspended->ochans[i];
			fixedup++;
		}
	}
	#ifdef DEBUG_DYNPROC
		BMESSAGE ("fixup_channel_words(): %d channel replaced out of %d\n", fixedup, p->suspended->num_ichans + p->suspended->num_ochans);
	#endif
	return;
}
/*}}}*/
/*{{{  d_process *dynproc_startprocess (int *params, void *exitpoint)*/
/*
 *	d_process *dynproc_startprocess (int *params, void *exitpoint)
 *	called before a dynamic process is launched
 */
d_process *dynproc_startprocess (int *params, void *exitpoint)
{
	d_process *tp;
	int i;

	tp = *(d_process **)(params[0]);
	/* tp->ws_ptr[0] = 0; */
	tp->ws_ptr[0] = (int)exitpoint;
	tp->ws_ptr[1] = (int)tp;
	/* copy channel array pointers */
	for (i=0; i<4; i++) {
		tp->ws_ptr[i+2] = params[i+1];
	}
	tp->result = (int *)(params[5]);
	if (!tp->suspended) {
		int sptr = 6;

		if (tp->vs_size) {
			tp->ws_ptr[sptr] = (int)tp->vs_ptr;
			sptr++;
		}
		if (tp->ms_size) {
			tp->ws_ptr[sptr] = (int)tp->ms_ptr;
			sptr++;
		}
		tp->ws_ptr[8] = (int)tp;
		for (sptr=9; sptr<16; tp->ws_ptr[sptr] = 0, sptr++);
		ccsp_give_ws_code ((char *)tp->ws_ptr - tp->ws_size, tp->ws_size, (unsigned char *)tp->entrypoint);
	} else {
		fixup_channel_words (tp);
		#ifdef DEBUG_DYNPROC
			MESSAGE ("dynproc_startprocess: repaired channels for suspended process wptr = %p, raddr = %p\n", tp->suspended->wptr, tp->suspended->return_addr);
		#endif
	}
	#ifdef DEBUG_DYNPROC
		for (i=0; i<16; i++) {
			MESSAGE ("dynproc_startprocess: tp->wsptr[%d] = 0x%8.8x\n", i, tp->ws_ptr[i]);
		}
	#endif
	tp->is_running = 1;
	return tp;
}
/*}}}*/
/*{{{  int dynproc_suspendprocess (d_process *p, int *result, word *wptr, unsigned int raddr, word paf)*/
/*
 *	int dynproc_suspendprocess (d_process *p, int *result, word *wptr, unsigned int raddr, word paf)
 *	called to save various things when suspending a dynamic process
 */
int dynproc_suspendprocess (d_process *p, int *result, word *wptr, unsigned int raddr, word paf)
{
	#ifdef DEBUG_DYNPROC
		MESSAGE ("debug: in dynproc_suspendprocess (%p, %p, %p, %p)\n", p, result, wptr, raddr);
	#endif
	/* check to make sure that `wptr' is in the range (tp->ws_ptr - tp->ws_size) --> tp->ws_ptr */
	if (!workspace_in_dprocess (wptr, p)) {
		BMESSAGE ("fatal: dynproc_suspendprocess() not invoked by dynamic process being suspended!\n");
		ccsp_bad_exit ();
	}
	if (!p->suspended) {
		/* could happen if do_ccsp_suspendproc failed */
		return 1;
	}
	#ifdef DEBUG_DYNPROC
		MESSAGE ("debug: suspended process has %d input channels and %d output channels.\n", p->suspended->num_ichans, p->suspended->num_ochans);
	#endif
	p->suspended->result 		= result;
	p->suspended->wptr 		= wptr;
	p->suspended->return_addr 	= raddr;
	p->suspended->priofinity	= paf;
	p->is_running 			= 0;
	return 0;
}
/*}}}*/
/*{{{  d_process *dynproc_endprocess (word *wptr)*/
/*
 *	d_process *dynproc_endprocess (word *wptr)
 *	called after a dynamic process has finished
 */
d_process *dynproc_endprocess (word *wptr)
{
	d_process *tp;

	tp = (d_process *)wptr[8];
	tp->is_running = 0;
	ccsp_take_ws ((char *)tp->ws_ptr - tp->ws_size);
	return tp;
}
/*}}}*/

#else	/* !DYNAMIC_PROCS || RMOX_BUILD */

/*{{{  void no_dynamic_process_support (void)*/
/*
 *	void no_dynamic_process_support (void)
 *	called if somebody tries to execute a dynamic process (ccsp.openlib should fail during compilation though..)
 */
void no_dynamic_process_support (void)
{
	BMESSAGE ("dynamic process support is not enabled!\n");
	ccsp_bad_exit ();
}
/*}}}*/

#endif	/* !DYNAMIC_PROCS || RMOX_BUILD */

