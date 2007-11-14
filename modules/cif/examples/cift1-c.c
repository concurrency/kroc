
/* cift1-c.c -- CIF test 1, C part */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cif.h>
#include "ciftutils.h"

void my_process (Workspace wptr)
{
	Channel *kroc_err = ProcGetParam (wptr, 0, Channel *);

	out_string (wptr, "Hello, world! (from C)\n", kroc_err);
}
