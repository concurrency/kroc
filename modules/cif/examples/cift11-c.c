
/* cift11-c.c -- CIF test 11, C part */

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
	LINK_out
};

void my_process (Workspace wptr)
{
	mt_cb_t *cli = ProcGetParam (wptr, 0, mt_cb_t *);

	int i;

	for (i = 99; i >= 95; i--) {
		MTLock (wptr, cli, MT_CB_CLIENT);
		ChanOutInt (wptr, &(cli->channels[LINK_in]), i);
		MTUnlock (wptr, cli, MT_CB_CLIENT);
	}
}

