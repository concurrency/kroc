/*
 * timer.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

#include "srv.h"

/* Maximum sleep depends on how many peripheral clock ticks fit in 2^32 */
const unsigned int max_sleep = (((unsigned int) -1) / PERIPHERAL_CLOCK) * 1000000U;
/* Minimum sleep is 10000 peripheral clocks */
const unsigned int min_sleep = (10000U * 1000000U) / PERIPHERAL_CLOCK;

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

static void start_timer7 (unsigned int us)
{
	/* Program timer to fire in <= us microseconds */
	*pTIMER7_PERIOD		= 0;
	*pTIMER7_WIDTH		= us * (PERIPHERAL_CLOCK / 1000000);
	*pTIMER7_CONFIG		= OUT_DIS | IRQ_ENA | PWM_OUT;
	SSYNC;
	/* Enable timer */
	*pTIMER_ENABLE		= TIMEN7;
}

static unsigned int stop_timer7 (void)
{
	/* Disable timer */
	*pTIMER_DISABLE		= TIMEN7;
	SSYNC;
	/* Force disable timer and clear any pending interrupt */
	*pTIMER_STATUS		= TIMIL7 | TRUN7;
	SSYNC;
	return *pTIMER7_COUNTER;
}

static void go_to_sleep (unsigned int sleep_for)
{
	unsigned short imask;

	DISABLE_INTERRUPTS (imask);

	/* Only sleep if there are no pending interrupts */
	if (!tvm_interrupt_pending ()) {
		unsigned int core_start, core_end, slept;

		/* Set an alarm using timer 7 */
		start_timer7 (sleep_for);

		/* Stop core timer */
		*pTCNTL  &= ~TMREN_P;
		SSYNC;

		/* Enable core clock stopping */
		*pPLL_CTL |= STOPCK;
		SSYNC;

		/* Go to sleep... */
		IDLE;
		SSYNC;
		/* Got woken up... */

		/* Disable core clock stopping */
		*pPLL_CTL &= ~STOPCK;
		SSYNC;

		/* Stop the alarm */
		slept 		= stop_timer7 ();
		slept 		/= (PERIPHERAL_CLOCK / 1000000);

		/* Restore core clock */
		core_start 	= *pTCOUNT;
		core_end 	= core_start - (slept << 1);
		if (core_start < core_end) {
			core_timer_wrap_count++;
		}
		*pTCOUNT	= core_end;
		CSYNC;
		*pTCNTL  	|= TMREN_P;
		CSYNC;
	}

	ENABLE_INTERRUPTS (imask);
}

void sleep_until (WORD timeout)
{
	unsigned int sleep_for = (unsigned int) (timeout - read_time ());
	
	if (sleep_for >= min_sleep) {
		go_to_sleep (sleep_for > max_sleep ? max_sleep : sleep_for);
	}
}

void sleep (void)
{
	go_to_sleep (max_sleep);
}

