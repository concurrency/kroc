
/* cift13-c.c -- CIF test 13, C part */

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
	struct timeval tv = {2, 0};

	fprintf (stderr, "the message was [%s]\n", string);
	select (0, NULL, NULL, NULL, &tv);
	fprintf (stderr, "you shouldn't see this message\n", string);
}

void proc_p1 (Workspace wptr)
{
	Channel *kroc_err = ProcGetParam (wptr, 0, Channel *);
	Channel *killchan = ProcGetParam (wptr, 1, Channel *);

	KillableBlockingCallN (wptr, my_bsyscall, killchan, 2, "wibble!");
	out_string (wptr, "that should have worked!\n", kroc_err);
}

void proc_p2 (Workspace wptr)
{
	Channel *killchan = ProcGetParam (wptr, 0, Channel *);

	word result;

	ExternalCallN (fprintf, 2, stderr, "proc_p2: sleeping for 200000 and killing process.\n");
	TimerDelay (wptr, 200000);
	result = KillBlockingCall (wptr, killchan);
	ExternalCallN (fprintf, 3, stderr, "proc_p2: killed process, got result = %d.\n", result);
}

void my_process (Workspace wptr)
{
	Channel *kroc_err = ProcGetParam (wptr, 0, Channel *);

	Channel killchan;
	Workspace p1, p2;
	word stack_p1[WORKSPACE_SIZE (2, 1024)];
	word stack_p2[WORKSPACE_SIZE (1, 1024)];

	ChanInit (wptr, &killchan);

	p1 = LightProcInit (wptr, stack_p1, 2, 1024);
	ProcParam (wptr, p1, 0, kroc_err);
	ProcParam (wptr, p1, 1, &killchan);
	p2 = LightProcInit (wptr, stack_p2, 1, 1024);
	ProcParam (wptr, p2, 0, &killchan);

	ProcPar (wptr, 2, p1, proc_p1, p2, proc_p2);
}

