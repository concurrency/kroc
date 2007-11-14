
/* cift16-c.c -- CIF test 16, barriers from C, C part */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cif.h>

void rower (Workspace wptr)
{
	int id = ProcGetParam (wptr, 0, int);
	mt_barrier_t *out = ProcGetParam (wptr, 1, mt_barrier_t *);
	bool synced = ProcGetParam (wptr, 2, bool);
	int i;

	for (i = 0; i < 6; i++) {
		if (synced)
			MTSync (wptr, out);

		ExternalCallN (printf, 3, "Rower %d: %s!\n",
		               id, (i % 2 == 0) ? "Heave" : "Ho");

		TimerDelay (wptr, random () % 1000000);
	}
}

