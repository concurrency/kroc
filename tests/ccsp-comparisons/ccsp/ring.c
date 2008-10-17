#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <cif.h>

#define ELEMENTS	256
#define STACK_SIZE	256

static Channel 	channel[ELEMENTS];
static word	workspace[ELEMENTS - 1][WORKSPACE_SIZE (1, STACK_SIZE)];

static void root (Workspace wptr, int cycles, int this)
{
	int next = (this + 1) % ELEMENTS;
	int token;

	ChanOutInt (wptr, &(channel[next]), 1);
	ChanInInt (wptr, &(channel[this]), &token);
	
	fprintf (stdout, "start\n");
	fflush (stdout);

	while (cycles > 0) {	
		ChanOutInt (wptr, &(channel[next]), (token + 1));
		ChanInInt (wptr, &(channel[this]), &token);
		cycles--;
	}
	
	fprintf (stdout, "end\n");
	fflush (stdout);

	fprintf (stdout, "%d\n", token);
	fflush (stdout);

	ChanOutInt (wptr, &(channel[next]), 0);
	ChanInInt (wptr, &(channel[this]), &token);
}

static void element (Workspace wptr)
{
	int this = ProcGetParam (wptr, 0, int);
	int next = (this + 1) % ELEMENTS;
	int token;

	do {
		ChanInInt (wptr, &(channel[this]), &token);
		if (token > 0)
			ChanOutInt (wptr, &(channel[next]), (token + 1));
		else
			ChanOutInt (wptr, &(channel[next]), token);
	} while (token);
}

static void proc_main (Workspace wptr)
{
	LightProcBarrier bar;
	int cycles = ProcGetParam (wptr, 0, int);
	int i;

	LightProcBarrierInit (wptr, &bar, ELEMENTS - 1);

	for (i = 0; i < ELEMENTS; ++i)
		ChanInit (wptr, &(channel[i]));

	for (i = 1; i < ELEMENTS; ++i) {
		Workspace ws = LightProcInit (wptr, workspace[i - 1], 1, STACK_SIZE);
		ProcParam (wptr, ws, 0, i);
		LightProcStart (wptr, &bar, ws, element);
	}

	root (wptr, cycles, 0);

	LightProcBarrierWait (wptr, &bar);

	Shutdown (wptr);
}

int main (int argc, char *argv[])
{
	Workspace p;
	int cycles;

	if (argc >= 2)
		cycles = atoi (argv[1]);
	else
		cycles = 0;

	if (!ccsp_init ())
		return 1;

	p = ProcAllocInitial (1, 1024 * 1024);
	ProcParam (p, p, 0, cycles);
	ProcStartInitial (p, proc_main);

	return 0;
}
