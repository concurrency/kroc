
/* cif-commstime-c.c -- CIF commstime */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cif.h>
#include "ciftutils.h"

void proc_prefix (Workspace wptr)
{
	int n = ProcGetParam (wptr, 0, int);
	Channel *in = ProcGetParam (wptr, 1, Channel *);
	Channel *out = ProcGetParam (wptr, 2, Channel *);

	int i;

	ChanOutInt (wptr, out, n);
	for (;;) {
		ChanInInt (wptr, in, &i);
		ChanOutInt (wptr, out, i);
	}
}

void proc_succ (Workspace wptr)
{
	Channel *in = ProcGetParam (wptr, 0, Channel *);
	Channel *out = ProcGetParam (wptr, 1, Channel *);

	int i;

	for (;;) {
		ChanInInt (wptr, in, &i);
		ChanOutInt (wptr, out, i + 1);
	}
}

void proc_delta (Workspace wptr)
{
	Channel *in = ProcGetParam (wptr, 0, Channel *);
	Channel *out1 = ProcGetParam (wptr, 1, Channel *);
	Channel *out2 = ProcGetParam (wptr, 2, Channel *);

	int i;

	for (;;) {
		ChanInInt (wptr, in, &i);
		ChanOutInt (wptr, out1, i);
		ChanOutInt (wptr, out2, i);
	}
}

void proc_consume (Workspace wptr)
{
	Channel *in = ProcGetParam (wptr, 0, Channel *);
	Channel *kroc_err = ProcGetParam (wptr, 1, Channel *);

	Time t0, t1;
	int i, n;
	char str[128];

	out_string (wptr, "warming up...\n", kroc_err);
	for (i = 0; i < 1000; i++) {
		ChanInInt (wptr, in, &n);
	}
	out_string (wptr, "go!\n", kroc_err);
	for (;;) {
		t0 = TimerRead (wptr);
		for (i = 0; i < 1000000; i++) {
			ChanInInt (wptr, in, &n);
		}
		t1 = TimerRead (wptr);
		ExternalCallN (sprintf, 4, str, "last value: %d, comms/cxt = %d ns\n", n, (t1 - t0) / 4000);
		out_string (wptr, str, kroc_err);
	}
}

void commstime (Workspace wptr)
{
	Channel *kroc_err = ProcGetParam (wptr, 0, Channel *);

	Workspace pfx, delta, succ, con;
	word stack_pfx[WORKSPACE_SIZE (3, 1024)];
	word stack_delta[WORKSPACE_SIZE (3, 1024)];
	word stack_succ[WORKSPACE_SIZE (2, 1024)];
	word stack_con[WORKSPACE_SIZE (2, 4096)];
	Channel a, b, c, d;

	ChanInit (wptr, &a);
	ChanInit (wptr, &b);
	ChanInit (wptr, &c);
	ChanInit (wptr, &d);

	out_string (wptr, "commstime in C.\n", kroc_err);

	pfx = LightProcInit (wptr, stack_pfx, 3, 1024);
	ProcParam (wptr, pfx, 0, 0);
	ProcParam (wptr, pfx, 1, &c);
	ProcParam (wptr, pfx, 2, &a);
	delta = LightProcInit (wptr, stack_delta, 3, 1024);
	ProcParam (wptr, delta, 0, &a);
	ProcParam (wptr, delta, 1, &b);
	ProcParam (wptr, delta, 2, &d);
	succ = LightProcInit (wptr, stack_succ, 2, 1024);
	ProcParam (wptr, succ, 0, &b);
	ProcParam (wptr, succ, 1, &c);
	con = LightProcInit (wptr, stack_con, 2, 4096);
	ProcParam (wptr, con, 0, &d);
	ProcParam (wptr, con, 1, kroc_err);

	ProcPar (wptr, 4,
	         pfx, proc_prefix,
	         delta, proc_delta,
	         succ, proc_succ,
	         con, proc_consume);
}
