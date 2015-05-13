/*
 *	calltest.c -- various calls to the run-time API to calculate stack requirements.
 *	For use with the ARM/CCSP run-time and regular CCSP runtime.
 *	Fred Barnes, May 2015
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <sys/types.h>

#include <cif.h>


void test_calls_wptr (Workspace p, int i)
{
	void *ptr;

	Reschedule (p);
	if (i == 0) {
		SetErr (p);
		SetErrM (p, "test");
		Shutdown (p);
	}

	ptr = MAlloc (p, i);
	MRelease (p, ptr);
}

void test_calls_wptr_chans (Workspace p, Channel *in0, Channel *in1, Channel *out)
{
	int data;
	Channel local;
	
	ChanInit (p, &local);
	ChanOut (p, out, &data, sizeof (data));
	ChanIn (p, in0, &data, sizeof (data));

	Alt (p);
	AltEnableChannel (p, 1, in0);
	AltEnableChannel (p, 1, in1);
	AltWait (p);
	AltDisableChannel (p, 1, in0);
	AltDisableChannel (p, 1, in1);
	AltEnd (p);

	ProcAlt (p, in0, in1, NULL);
}

void dummyfcn (Workspace me, int arg0, int arg1)
{
	return;
}

void test_calls_wptr_proc (Workspace p, Workspace other, int i)
{
	LightProcBarrier b;
	Workspace ws;
	word tstk[42];

	LightProcBarrierInit (p, &b, 42);
	if (i) {
		LightProcBarrierWait (p, &b);
	}

	RunP (p, other);
	if (i) {
		StopP (p);
	}

	ws = LightProcInit (p, tstk, 2, 42);
	ProcParamAny (p, ws, 0, &i);
	ProcGetParamAny (ws, 0);

	ProcPar (p, 2, other, ws);

	LightProcStart (p, &b, ws, (void *)dummyfcn);
	LightProcFree (p, ws);
}

void test_calls_wptr_time (Workspace p)
{
	int timeo, to2;

	timeo = TimerRead (p);
	to2 = timeo + 42;
	TimerWait (p, to2);
	if (Time_AFTER (to2, timeo)) {
		/* boo */
		StopP (p);
	}
}

void dummyfcn0 (void)
{
	return;
}

void test_calls_wptr_ext (Workspace p)
{
	char *str = "foo";

	ExternalCall0 ((void *)dummyfcn0);
	ExternalCall1 ((void *)printf, (word)str);
	ExternalCallN ((void *)printf, 2, "foo%d", 42);
}

