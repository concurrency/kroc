
/* cift15.c -- CIF test 15: test CIF without occam */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <cif.h>

const int CYCLES = 20;

void proc_numbers (Workspace wptr)
{
	Channel *out = ProcGetParam (wptr, 0, Channel *);

	int i;

	for (i = 0; i < CYCLES; i++) {
		ChanOutInt (wptr, out, i);
	}
}

void proc_integrate (Workspace wptr)
{
	Channel *in = ProcGetParam (wptr, 0, Channel *);
	Channel *out = ProcGetParam (wptr, 1, Channel *);

	int i, sum = 0;

	for (i = 0; i < CYCLES; i++) {
		int v;

		ChanInInt (wptr, in, &v);
		sum += v;
		ChanOutInt (wptr, out, sum);
	}
}

void proc_print (Workspace wptr)
{
	Channel *in = ProcGetParam (wptr, 0, Channel *);

	int i;

	for (i = 0; i < CYCLES; i++) {
		int v;

		ChanInInt (wptr, in, &v);
		ExternalCallN (printf, 2, "Received: %d\n", v);
	}
}

void proc_main (Workspace wptr)
{
	Channel ca, cb;
	Workspace p1, p2, p3;
	word stack_p1[WORKSPACE_SIZE (1, 1024)];
	word stack_p2[WORKSPACE_SIZE (2, 1024)];
	word stack_p3[WORKSPACE_SIZE (1, 1024)];

	ExternalCallN (printf, 1, "Hello, world!\n");

	ChanInit (wptr, &ca);
	ChanInit (wptr, &cb);

	p1 = LightProcInit (wptr, stack_p1, 1, 1024);
	ProcParam (wptr, p1, 0, &ca);
	p2 = LightProcInit (wptr, stack_p2, 2, 1024);
	ProcParam (wptr, p2, 0, &ca);
	ProcParam (wptr, p2, 1, &cb);
	p3 = LightProcInit (wptr, stack_p3, 1, 1024);
	ProcParam (wptr, p3, 0, &cb);

	ProcPar (wptr, 3,
	         p1, proc_numbers,
	         p2, proc_integrate,
	         p3, proc_print);

	ExternalCallN (printf, 1, "Done.\n");

	Shutdown (wptr);
}

int main (int argc, char *argv[]) {
	Workspace p;

	printf ("Initialising CCSP\n");
	if (!ccsp_init ())
		return 1;

	printf ("Running CIF process\n");
	p = ProcAllocInitial (0, 4096);
	ProcStartInitial (p, proc_main);

	/* NOTREACHED */
	return 0;
}
