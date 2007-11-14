
/* cift10-c.c -- CIF test 10, C part */

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
	LINK_num_chans
};

void my_process (Workspace wptr)
{
	Channel *link_out = ProcGetParam (wptr, 0, Channel *);

	int i, v, sum = 0;
	mt_cb_t *cli, *svr;

	cli = svr = MTAllocChanType (wptr, LINK_num_chans, false);

	/* communicate client end back to occam */
	MTChanOut (wptr, link_out, &cli);

	/* now serve it */
	for (i = 0; i < 5; i++) {
		ChanInInt (wptr, &(svr->channels[LINK_in]), &v);
		sum += v;
		ChanOutInt (wptr, &(svr->channels[LINK_out]), sum);
	}

	MTRelease (wptr, svr);
}

