
/* cift9-c.c -- CIF test 9, C part */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <cif.h>

enum LINK_channels {
	LINK_in = 0,
	LINK_out,
};

void my_process (Workspace wptr)
{
	mt_cb_t *link_cli = ProcGetParam (wptr, 0, mt_cb_t *);

	int i, v;

	for (i = 0; i < 5; i++) {
		ChanOutInt (wptr, &(link_cli->channels[LINK_in]), i);
		ChanInInt (wptr, &(link_cli->channels[LINK_out]), &v);

		ExternalCallN (fprintf, 3, stderr, "got: %d\n", v);
	}
}

