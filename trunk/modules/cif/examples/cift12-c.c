
/* cift12-c.c -- CIF test 12, process forking, C part */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cif.h>
#include "ciftutils.h"

void proc_test12 (Workspace wptr)
{
	mt_barrier_t *fb = ProcGetParam (wptr, 0, mt_barrier_t *);
	Channel *out = ProcGetParam (wptr, 1, Channel *);
	char *data = ProcGetParam (wptr, 2, char *);

	out_string (wptr, "Hello from proc_test12! (forked)\n", out);
	out_string (wptr, "My data is \"", out);
	out_string (wptr, data, out);
	out_string (wptr, "\"\n", out);

	/* Release mobile data parameter */
	MTRelease (wptr, data);
	/* Release forking barrier when complete */
	MTRelease (wptr, fb);
}

void my_process (Workspace wptr)
{
	Channel *kroc_err = ProcGetParam (wptr, 0, Channel *);
	mt_barrier_t *fb;
	Workspace p;
	char *data;

	out_string (wptr, "Hello, world! (from C).  Allocating another process and forking...\n", kroc_err);

	/* Allocate forking barrier */
	fb = (mt_barrier_t *) MTAlloc (wptr, MT_MAKE_BARRIER (MT_BARRIER_FORKING), 0);

	/* Allocate some mobile data */
	data = (char *) MTAlloc (wptr, MT_SIMPLE | MT_MAKE_TYPE (MT_DATA), 16);
	strcpy (data, "123456789");

	/* Allocate process */
	p = ProcAlloc (wptr, 1, 1024);

	/* Copy forking barrier */
	ProcMTCopy (wptr, p, 0, fb);
	/* Pass channel pointer - non-mobile */
	ProcParam (wptr, p, 1, kroc_err);
	/* Move mobile data */
	ProcMTMove (wptr, p, 2, &data);

	/* Start process */
	ProcStart (wptr, p, proc_test12);

	/* Wait on forking barrier */
	MTSync (wptr, fb);
	/* Forking barrier is implicitly released by sync */

	out_string (wptr, "Forked process is complete\n", kroc_err);
}

