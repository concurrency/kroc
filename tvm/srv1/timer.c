/*
 * timer.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

#include "srv.h"

/* Accessed from assembly interrupt wrapper */
volatile unsigned long core_timer_wrap_count;

void init_timers (void)
{
	core_timer_wrap_count	= 0;
	*pTSCALE		= ((CORE_CLOCK / 2000000) - 1);
	*pTPERIOD		= 0xffffffff;
	*pTCOUNT		= 0xffffffff;
	SSYNC;
	*pTCNTL			= TMREN_P | TAUTORLD_P | TINT_P;
	CSYNC;
}

/* Read the time counter, returns number of microseconds since reset */
static WORD read_time (void)
{
	unsigned long pre_w, post_w;
	unsigned long tcount;

	do {
		pre_w 	= core_timer_wrap_count;
		tcount 	= *pTCOUNT;
		CSYNC;
		post_w	= core_timer_wrap_count;
	} while (pre_w != post_w);

	return (~tcount) >> 1 | (post_w << 31);
}

void delay_us (WORD delay)
{
	int timeout = read_time () + delay;

	if ((delay < 0) || (delay >= (1 << 30)))
		return;
	while (read_time () < timeout)
		continue;
}

WORD srv_get_time (ECTX ectx)
{
	return read_time ();
}

