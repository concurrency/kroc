/*
 * aic.c - Interrupt Controller Manipulation Functions
 *
 * Copyright (C) 2008  the NxOS developers (see NXOS)
 * Copyright (C) 2010  Carl G. Ritson
 *
 * Redistribution of this file is permitted under
 * the terms of the GNU Public License (GPL) version 2.
 */

#include "tvm-nxt.h"
#include "at91sam7s256.h"

void aic_enable (int vector)
{
	*AT91C_AIC_IECR = (1 << vector);
}

void aic_disable (int vector)
{
	*AT91C_AIC_IDCR = (1 << vector);
}

void aic_set (int vector)
{
	*AT91C_AIC_ISCR = (1 << vector);
}

void aic_clear (int vector)
{
	*AT91C_AIC_ICCR = (1 << vector);
}

void aic_install_isr (int vector, uint32_t prio, uint32_t trig_mode, void *isr)
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

void aic_init (void)
{
	int i;

	/* Prevent the ARM core from being interrupted while we set up the
	 * AIC.
	 */
	nxt__interrupts_disable ();

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

	nxt__interrupts_enable ();
}
