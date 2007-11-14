/*
 *	dynproc.h - interface to dynamic PROCs
 *	Copyright (C) 2000 Fred Barnes <frmb2@ukc.ac.uk>
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

#ifndef __DYNPROC_H
#define __DYNPROC_H

/* this structure keeps info about a `dynamic' process */
/* it is placed in the workspace above the process, and pointed to */

typedef struct {
	int num_ichans, num_ochans;		/* number of input and output channels */
	int *ichans, *ochans;			/* arrays to hold main channel addresses */
	int *result;				/* hold pointer to result param of ccsp.suspendproc */
	word *wptr;				/* Wptr of suspended process */
	unsigned int return_addr;		/* return address of suspended process */
	word priofinity;			/* priofinity of suspended process */
} d_suspended_inf;

struct tag_d_process_msqueue;	/* fwd */

typedef struct tag_d_process {
	struct tag_d_process *next;		/* next in linked-list */
	struct tag_d_process *prev;		/* previous in linked-list */
	struct tag_d_process *children;		/* children of this one */
	struct tag_d_process *parent;		/* parent of this one */
	char *mem_start;			/* where the allocated block starts */
	word *ws_ptr;				/* pointer to workspace start (high address of workspace) */
	word *ws_base;				/* pointer to lowest workspace address (bottom) */
	word *vs_ptr;				/* vectorspace pointer (low address of vectorspace) */
	struct tag_d_process_msqueue *ms_ptr;	/* mobilespace info block */
	/* word *ms_ptr;			-- mobilespace pointer (low address of mobilespace) */
	int ws_size;				/* workspace size */
	int vs_size;				/* vectorspace size */
	int ms_size;				/* mobilespace size */
	word *holding_wptr;			/* process waiting for the completion of this */
	unsigned holding_raddr;			/* and it's return address */
	word holding_priofinity;		/* and it's priofinity */
	char *entrypoint;			/* where the function starts */
	void *lhandle;				/* associated dlopen() handle */
	int *result;				/* pointer to the (INT result) parameter in an */
						/* occam workspace (calling ccsp.{run,resume}proc) */
	d_suspended_inf *suspended;		/* info about a suepended process */
	int is_running;				/* 1 if the process is running/blocked, 0 if it is suspended */
	char *proc_name;			/* text of occam process name */
} d_process;

typedef struct tag_d_process_msqueue {
	struct tag_d_process_msqueue *next;	/* next in linked-list */
	word *msptr;				/* mobilespace pointer (low address) */
	unsigned long long hashcode;		/* big enough for something unique (efficiency not paramount here!) */
	d_process *in_use;			/* NULL if free, pointer to dprocess otherwise */
} d_process_msqueue;

typedef struct tag_d_process_libnamehash {
	struct tag_d_process_libnamehash *next;	/* next in linked-list */
	void *lhandle;				/* library handle */
	char *filename;				/* filename of library (dmem_alloc'd) */
	unsigned long long hashcode;		/* reverse hashcode for this filename */
} d_process_libnamehash;


/*
 *  in-file layout for suspended processes is:
 *	<struct dp_header_struct>
 *	"process-name"
 *	<input-channel offsets>
 *	<output-channel offsets>
 *	"process-workspace"
 *	<offset tables>
 */

struct dp_header_struct {
	unsigned char magic[8];			/* magic string: "\033SOPv0.9" */
	int ws_size;				/* workspace size */
	int vs_size;				/* vectorspace size */
	int num_ichans, num_ochans;		/* number of input/output channels */
	int pnamelen;				/* process name length */
	int offtabsize;				/* offset table size */
	int flags;				/* (for later use..) */
#ifdef __GNUC__
	} __attribute__ ((packed));
#else
	};
	#warning Not GNU C -- might have problems reading/writing dynamic processes
#endif
typedef struct dp_header_struct dp_header;

struct dp_offset_struct {
	int ws_offset;				/* offset (in slots) from workspace bottom */
	int adjustment;				/* difference from d_process->mem_start */
#ifdef __GNUC__
	} __attribute__ ((packed));
#else
	};
	#warning Not GNU C -- might have problems reading/writing dynamic processes
#endif
typedef struct dp_offset_struct dp_offset;


#define DPROCESS_FINISHED	0
#define DPROCESS_SUSPENDED	1
#define DPROCESS_RESUMED	2
#define DPROCESS_FAULTED	3		/* process suffered a run-time error */

#ifndef __DYNPROC_C
	#ifdef DYNAMIC_PROCS
		extern d_process *dynproc_startprocess (int *params, void *exitpoint);
		extern int dynproc_suspendprocess (d_process *p, int *result, word *wptr, unsigned int raddr, word paf);
		extern d_process *dynproc_endprocess (word *wptr);
		extern void dynproc_dumpprocess (d_process *p);
		extern int faulting_dynproc (word **wptr_ptr, unsigned int *raddr_ptr, char *fault, d_process **tp_return);
	#else	/* !DYNAMIC_PROCS */
		extern void no_dynamic_process_support (void);
	#endif	/* !DYNAMIC_PROCS */
#endif	/* !__DYNPROC_C */

#endif	/* !__DYNPROC_H */

