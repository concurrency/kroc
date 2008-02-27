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
	/* Setup core timer to run at ~2000000 ticks a second */
	core_timer_wrap_count	= 0;
	*pTSCALE		= ((CORE_CLOCK / 2000000) - 1);
	*pTPERIOD		= 0xffffffff;
	*pTCOUNT		= 0xffffffff;
	SSYNC;
	*pTCNTL			= TMREN_P | TAUTORLD_P | TINT_P;
	CSYNC;

	/* Setup RTC to run via pre-scaler */
	*pRTC_ICTL 		= 0;
	*pRTC_STAT		= 0;
	*pRTC_PREN		= 1;
	*pRTC_ISTAT		= 
		STOPWATCH | ALARM | SECOND | MINUTE | HOUR | DAY | DAY_ALARM | WRITE_COMPLETE;
	*pRTC_SWCNT		= 0;
	*pRTC_ALARM		= 0;
	SSYNC;
}

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

void sleep_until (WORD timeout)
{
	/* CGR FIXME: implement this */
}

void sleep (void)
{
	unsigned short imask;

	DISABLE_INTERRUPTS (imask);

	IDLE;
	SSYNC;

	ENABLE_INTERRUPTS (imask);
}

