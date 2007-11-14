
/* cift3-c.c -- CIF test 3, C part */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cif.h>
#include "ciftutils.h"

void proc_p1 (Workspace wptr)
{
	Channel *out = ProcGetParam (wptr, 0, Channel *);

	int i;

	for (i = 0; i < 10; i++) {
		ChanOutInt (wptr, out, i);
	}
	ChanOutInt (wptr, out, -1);
}

void proc_p2 (Workspace wptr)
{
	Channel *in = ProcGetParam (wptr, 0, Channel *);
	Channel *out = ProcGetParam (wptr, 1, Channel *);

	int v;
	char str[20];

	do {
		ChanInInt (wptr, in, &v);
		ExternalCallN (sprintf, 3, str, "%d\n", v);
		out_string (wptr, str, out);
	} while (v > -1);
}

void my_process (Workspace wptr)
{
	Channel *kroc_err = ProcGetParam (wptr, 0, Channel *);

	Channel c;
	Workspace p1, p2;
	word stack_p1[WORKSPACE_SIZE (1, 1024)];
	word stack_p2[WORKSPACE_SIZE (2, 1024)];

	ChanInit (wptr, &c);
	out_string (wptr, "Hello, world! (from C).  Allocating some other processes and going parallel..\n", kroc_err);

	p1 = LightProcInit (wptr, stack_p1, 1, 1024);
	ProcParam (wptr, p1, 0, &c);
	p2 = LightProcInit (wptr, stack_p2, 2, 1024);
	ProcParam (wptr, p2, 0, &c);
	ProcParam (wptr, p2, 1, kroc_err);

	ProcPar (wptr, 2, p1, proc_p1, p2, proc_p2);
}
