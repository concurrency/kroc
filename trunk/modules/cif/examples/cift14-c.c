
/* cift14-c.c -- CIF test 14, C part: test the raw ALT interface */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <cif.h>

void proc_send (Workspace wptr)
{
	int id = ProcGetParam (wptr, 0, int);
	Channel *out = ProcGetParam (wptr, 1, Channel *);

	int n = 0;
	for (;;) {
		ChanOutInt (wptr, out, n++);
		TimerDelay (wptr, 200000 * id);
	}
}

void proc_receive (Workspace wptr)
{
	Channel *in1 = ProcGetParam (wptr, 0, Channel *);
	Channel *in2 = ProcGetParam (wptr, 1, Channel *);
	Channel *out = ProcGetParam (wptr, 2, Channel *);

	int turn = 0;

	for (;;) {
		/* Only test SKIP guards half the time. */
		const int test_skip = (turn % 10) < 5;
		Time t = TimerRead (wptr) + 50000;
		int n;

		TimerAlt (wptr);

		AltEnableChannel (wptr, 0, in1);
		AltEnableChannel (wptr, 1, in2);
		AltEnableTimer (wptr, 2, t);
		if (test_skip)
			AltEnableSkip (wptr, 3);

		TimerAltWait (wptr);

		AltDisableChannel (wptr, 0, in1);
		AltDisableChannel (wptr, 1, in2);
		AltDisableTimer (wptr, 2, t);
		if (test_skip)
			AltDisableSkip (wptr, 3);

		switch (AltEnd (wptr)) {
		case 0:
			ChanInInt (wptr, in1, &n);
			ExternalCallN (fprintf, 3, stderr, "Got number from channel 1: %d\n", n);
			break;
		case 1:
			ChanInInt (wptr, in2, &n);
			ExternalCallN (fprintf, 3, stderr, "Got number from channel 2: %d\n", n);
			break;
		case 2:
			ExternalCallN (fprintf, 2, stderr, "Timer fired\n");
			break;
		case 3:
			ExternalCallN (fprintf, 2, stderr, "Skip fired\n");
			TimerDelay (wptr, 100000);
			break;
		default:
			SetErrW (wptr);
			break;
		}

		turn++;
	}
}

void my_process (Workspace wptr)
{
	Channel *kroc_out = ProcGetParam (wptr, 0, Channel *);

	Channel ca, cb;
	Workspace p1, p2, p3;
	word stack_p1[WORKSPACE_SIZE (2, 1024)];
	word stack_p2[WORKSPACE_SIZE (2, 1024)];
	word stack_p3[WORKSPACE_SIZE (3, 1024)];

	ChanInit (wptr, &ca);
	ChanInit (wptr, &cb);

	p1 = LightProcInit (wptr, stack_p1, 2, 1024);
	ProcParam (wptr, p1, 0, 1);
	ProcParam (wptr, p1, 1, &ca);
	p2 = LightProcInit (wptr, stack_p2, 2, 1024);
	ProcParam (wptr, p2, 0, 2);
	ProcParam (wptr, p2, 1, &cb);
	p3 = LightProcInit (wptr, stack_p3, 3, 1024);
	ProcParam (wptr, p3, 0, &ca);
	ProcParam (wptr, p3, 1, &cb);
	ProcParam (wptr, p3, 2, kroc_out);

	ProcPar (wptr, 3, p1, proc_send, p2, proc_send, p3, proc_receive);
}

