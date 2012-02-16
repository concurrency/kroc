

/* cift2-c.c -- CIF test 2, C part */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cif.h>

void first_process (Workspace wptr)
{
	Channel *out = ProcGetParam (wptr, 0, Channel *);

	int i;

	for (i = 0;; i++) {
		ChanOutInt (wptr, out, i);
		TimerDelay (wptr, 50000);
	}
}

void second_process (Workspace wptr)
{
	Channel *in = ProcGetParam (wptr, 0, Channel *);
	Channel *out = ProcGetParam (wptr, 1, Channel *);

	for (;;) {
		int i;

		ChanInInt (wptr, in, &i);
		i = i * 2;
		ChanOutInt (wptr, out, i);
	}
}

void third_process (Workspace wptr)
{
	Channel *in = ProcGetParam (wptr, 0, Channel *);
	Channel *byteout = ProcGetParam (wptr, 1, Channel *);

	char txtbuf[64];

	for (;;) {
		int i;

		ChanInInt (wptr, in, &i);
		ExternalCallN (sprintf, 3, txtbuf, "0x%08x\n", i);

		for (i = 0; txtbuf[i] != '\0'; i++) {
			ChanOutChar (wptr, byteout, txtbuf[i]);
		}
	}
}
