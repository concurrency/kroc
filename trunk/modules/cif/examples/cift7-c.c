
/* cift7-c.c -- CIF test 7, C part */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <cif.h>
#include "ciftutils.h"

void my_bsyscall (const char *string)
{
	struct timeval tv = {1, 500000};

	fprintf (stderr, "the message was [%s]\n", string);
	select (0, NULL, NULL, NULL, &tv);
	fprintf (stderr, "the message was [%s]\n", string);
}
	

void proc_p1 (Workspace wptr)
{
	BlockingCallN (wptr, my_bsyscall, 1, "wibble!");
}

void proc_p2 (Workspace wptr)
{
	Channel *out = ProcGetParam (wptr, 0, Channel *);

	int i;
	char *strs[] = {"bing", "bang", "bosh", NULL};

	for (i=0; strs[i]; i++) {
		out_string (wptr, "** ", out);
		out_string (wptr, strs[i], out);
		out_string (wptr, "\n", out);

		TimerDelay (wptr, 800000);
	}
}

void my_process (Workspace wptr)
{
	Channel *kroc_err = ProcGetParam (wptr, 0, Channel *);

	Workspace p1, p2;
	word stack_p1[WORKSPACE_SIZE (0, 1024)];
	word stack_p2[WORKSPACE_SIZE (1, 1024)];

	p1 = LightProcInit (wptr, stack_p1, 0, 1024);
	p2 = LightProcInit (wptr, stack_p2, 1, 1024);
	ProcParam (wptr, p2, 0, kroc_err);

	ProcPar (wptr, 2, p1, proc_p1, p2, proc_p2);
}

