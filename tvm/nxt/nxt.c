/*
 * nxt.c - NXT low-level hardware manipulation functions
 *
 * Copyright (C) 2008  the NxOS developers (see NXOS)
 * Copyright (C) 2010  Carl G. Ritson
 *
 * Redistribution of this file is permitted under
 * the terms of the GNU Public License (GPL) version 2.
 */

#include "tvm-nxt.h"
#include "at91sam7s256.h"

/*{{{  Interrupt Controller */
enum {
	AIC_TRIG_LEVEL		= 0,
	AIC_TRIG_EDGE		= 1
};

enum {
	AIC_PRIO_LOW		= 1,
	AIC_PRIO_DRIVER		= 3,
	AIC_PRIO_SOFTMAC 	= 4,
	AIC_PRIO_SCHED		= 5,
	AIC_PRIO_RT 		= 6,
	AIC_PRIO_TICK		= 7
};

static inline void aic_enable (int vector)
{
	*AT91C_AIC_IECR = (1 << vector);
}

static inline void aic_disable (int vector)
{
	*AT91C_AIC_IDCR = (1 << vector);
}

static inline void aic_set (int vector)
{
	*AT91C_AIC_ISCR = (1 << vector);
}

static inline void aic_clear (int vector)
{
	*AT91C_AIC_ICCR = (1 << vector);
}

static void aic_install_isr (int vector, uint32_t prio,
                     uint32_t trig_mode, void *isr)
{
	/* Disable the interrupt we're installing. Getting interrupted while
	 * we are tweaking it could be bad.
	 */
	aic_disable (vector);
	aic_clear (vector);

	AT91C_AIC_SMR[vector] = (trig_mode << 5) | prio;
	AT91C_AIC_SVR[vector] = (uint32_t) isr;

	aic_enable (vector);
}

static void init_interrupts (void)
{
	int i;

	/* Prevent the ARM core from being interrupted while we set up the
	 * AIC.
	 */
	nxt_interrupts_disable ();

	/* If we're coming from a warm boot, the AIC may be in a weird
	 * state. Do some cleaning up to bring the AIC back into a known
	 * state:
	 *  - All interrupt lines disabled,
	 *  - No interrupt lines handled by the FIQ handler,
	 *  - No pending interrupts,
	 *  - AIC idle, not handling an interrupt.
	 */
	*AT91C_AIC_IDCR = 0xFFFFFFFF;
	*AT91C_AIC_FFDR = 0xFFFFFFFF;
	*AT91C_AIC_ICCR = 0xFFFFFFFF;
	*AT91C_AIC_EOICR = 1;

	/* Enable debug protection. This is necessary for JTAG debugging, so
	 * that the hardware debugger can read AIC registers without
	 * triggering side-effects.
	 */
	*AT91C_AIC_DCR = 1;

	/* Set default handlers for all interrupt lines. */
	for (i = 0; i < 32; i++) {
		AT91C_AIC_SMR[i] = 0;
		AT91C_AIC_SVR[i] = (uint32_t) nxt__default_irq;
	}
	AT91C_AIC_SVR[AT91C_ID_FIQ] = (uint32_t) nxt__default_fiq;
	*AT91C_AIC_SPU = (uint32_t) nxt__spurious_irq;

	nxt_interrupts_enable ();
}
/*}}}*/

/*{{{  System Clock */
/* The main clock is at 48MHz, and the PIT divides that by 16 to get
 * its base timer frequency.
 */
#define PIT_BASE_FREQUENCY (NXT_CLOCK_FREQ/16)

/* We want a timer interrupt 1000 times per second. */
#define SYSIRQ_FREQ 1000

/* The system IRQ processing takes place in two different interrupt
 * handlers: the main PIT interrupt handler runs at a high priority,
 * keeps the system time accurate, and triggers the lower priority
 * IRQ.
 *
 * All other periodic processing (needed eg. by drivers) takes place
 * in this lower priority interrupt handler. This ensures that we
 * don't miss a clock beat.
 *
 * As the PIT only has one interrupt line, we steal the PWM
 * controller's interrupt line and use it as our low-priority system
 * interrupt. The PWM controller is unused on the NXT, so this is no
 * problem.
 */
#define SCHEDULER_SYSIRQ AT91C_ID_PWMC

/* The system timer. Counts the number of milliseconds elapsed since
 * the system's initialization.
 */
static volatile uint32_t systick_time;

/* High priority handler, called 1000 times a second */
static void systick_isr (void)
{
	uint32_t status;
	
	/* Read PIT value register to acknowledge the interrupt.*/
	status = *AT91C_PITC_PIVR;

	/* Do the system timekeeping. */
	systick_time++;

	/* FIXME: do management */
}

void init_systick (void) {
	nxt_interrupts_disable ();

	systick_time = 0;

	aic_install_isr (AT91C_ID_SYS, AIC_PRIO_TICK, 
		AIC_TRIG_EDGE, systick_isr);

	/* Configure and enable the Periodic Interval Timer. The counter
	 * value is 1/16th of the master clock (base frequency), divided by
	 * our desired interrupt period of 1ms.
	 */
	*AT91C_PITC_PIMR = (((PIT_BASE_FREQUENCY / SYSIRQ_FREQ) - 1) |
			AT91C_PITC_PITEN | AT91C_PITC_PITIEN);

	nxt_interrupts_enable ();
}
/*}}}*/


void nxt_init (void)
{
	init_interrupts ();
	init_systick ();
	return;
}

void nxt_abort (bool data, uint32_t pc, uint32_t cpsr)
{
	return;
}
