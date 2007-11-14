
/* cift8-c.c -- CIF test 8, C part */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <cif.h>

void my_process (Workspace wptr)
{
	Channel *in = ProcGetParam (wptr, 0, Channel *);
	Channel *out = ProcGetParam (wptr, 1, Channel *);

	int i;

	for (i = 0; i < 5; i++) {
		mt_array_t *da_in = NULL, *da_out;

		MTChanIn (wptr, in, &da_in);

		da_out = MTAllocArray (wptr, MT_MAKE_NUM (MT_NUM_BYTE),
		                       1, da_in->dimensions[0] + 4);
		memcpy (da_out->data, da_in->data, da_in->dimensions[0]);
		memcpy (da_out->data + da_in->dimensions[0], " :) ", 4);

		MTChanOut (wptr, out, &da_out);

		MTRelease (wptr, da_in);
	}
}
