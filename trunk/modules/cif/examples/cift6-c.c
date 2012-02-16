
/* cift6-c.c -- CIF test 6, C part */

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
	fprintf (stderr, "the message was [%s]\n", string);
}

void my_process (Workspace wptr)
{
	Channel *kroc_err = ProcGetParam (wptr, 0, Channel *);

	BlockingCallN (wptr, my_bsyscall, 1, "wibble!");
	out_string (wptr, "that should have worked!\n", kroc_err);
}


