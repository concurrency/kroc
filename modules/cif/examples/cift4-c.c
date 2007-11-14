
/* cift4-c.c -- CIF test 4, C part */

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
		TimerDelay (wptr, 500000);
	}
	ChanOutInt (wptr, out, -1);
}

void proc_p2 (Workspace wptr)
{
	Channel *in = ProcGetParam (wptr, 0, Channel *);
	Channel *key_in = ProcGetParam (wptr, 1, Channel *);
	Channel *out = ProcGetParam (wptr, 2, Channel *);

	int v = 0;
	char c = 0, str[20];

	do {
		int i;

		ExternalCallN (printf, 1, "at top of loop\n");
		i = ProcAlt (wptr, in, key_in, NULL);
		switch (i) {
		case 0:
			ChanInInt (wptr, in, &v);
			break;
		case 1:
			ChanInChar (wptr, key_in, &c);
			break;
		default:
			SetErrW (wptr);
			break;
		}
		ExternalCallN (sprintf, 5, str, "%d:%d:%d\n", i, v, c);
		out_string (wptr, str, out);
		Reschedule (wptr);
		if (i == 1) {
			switch (c) {
			case 'q':
				SetErrW (wptr);
				break;
			case 'p':
				ExternalCallN (sprintf, 3, str, "pri=%d\n", GetPriority (wptr));
				out_string (wptr, str, out);
				break;
			case '+':
				SetPriority (wptr, GetPriority (wptr) - 1);
				break;
			case '-':
				SetPriority (wptr, GetPriority (wptr) + 1);
				break;
			}
		}
	} while (v > -1);
}

void my_process (Workspace wptr)
{
	Channel *kroc_kyb = ProcGetParam (wptr, 0, Channel *);
	Channel *kroc_err = ProcGetParam (wptr, 1, Channel *);

	Channel c;
	Workspace p1, p2;
	word stack_p1[WORKSPACE_SIZE (1, 1024)];
	word stack_p2[WORKSPACE_SIZE (3, 1024)];

	ChanInit (wptr, &c);
	out_string (wptr, "Hello, world! (from C).  Allocating some other processes and going parallel..\n", kroc_err);

	p1 = LightProcInit (wptr, stack_p1, 1, 1024);
	ProcParam (wptr, p1, 0, &c);
	p2 = LightProcInit (wptr, stack_p2, 3, 1024);
	ProcParam (wptr, p2, 0, &c);
	ProcParam (wptr, p2, 1, kroc_kyb);
	ProcParam (wptr, p2, 2, kroc_err);

	ProcPar (wptr, 2, p1, proc_p1, p2, proc_p2);
}

