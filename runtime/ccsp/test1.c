
/* $ gcc -Wall -Iinclude -O2 -o test1 test1.c libccsp.a */ 

#include <stdlib.h>
#include <stdio.h>

#include "ccsp.h"

static char *test_str = "Hello World";

static void code_entry (void *sched, word *Wptr)
{
	fprintf (stderr, "code_entry: %p, %p\n", sched, Wptr);
	if (Wptr != NULL) {
		fprintf (stderr, "\tWptr = \"%s\"\n", (char *) Wptr);
	}
}

int main (int argc, char *argv[])
{
	ccsp_init (code_entry);
	ccsp_kernel_entry ((word *) test_str, (word *) NotProcess_p);
	return 0;
}
