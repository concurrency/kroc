
/* cift17-c.c -- CIF test 17, calling occam from C, C part */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cif.h>
#include "ciftutils.h"

extern void O_do_stuff (void);

void my_process (Workspace wptr)
{
	Channel *out = ProcGetParam (wptr, 0, Channel *);
	int i;

	out_string (wptr, "The magic word is ", out);

	for (i = 0; i < 23; i += 3) {
		OccamCall (O_do_stuff, 1024, 2, i, out);
	}

	out_string (wptr, " (but not yz)!\n", out);
}

