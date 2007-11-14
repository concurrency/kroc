
/* cift5-c.c -- CIF test 5, C part */

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

	char str[64];
	int v;

	v = 0;
	memcpy (str, "XXXfoobarcoXXX\n\0", 16);
	out_string (wptr, str, kroc_err);
	ExternalCallN (fprintf, 3, stderr, "hello EXTERNAL_CALLN world! %d\n", v);
	ExternalCallN (sprintf, 3, str, "hello world! %d\n", v);
	out_string (wptr, str, kroc_err);
}


