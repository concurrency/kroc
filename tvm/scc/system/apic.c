/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#include <stddef.h>
#include <apic.h>
#include <interrupt.h>

volatile struct apic *lapic;

void init_APIC()
{
	lapic = (struct apic *)APIC_BASE;

	// Set enable bit in lapic
	ulong value;
	value = lapic->spiv;
	value &= ~APIC_VECTOR_MASK;
	value |= APIC_SPIV_APIC_ENABLED;
	lapic->spiv = value;
}

int setupLINT(int lint_num, int irq, void (*handler)(void))
{
	if (!(lint_num == 0 || lint_num == 1))
		return SYSERR;
	if (irq < 0 || irq > IRQMAX)
		return SYSERR;

	// NOTE: some comment in linux mentioned doing reads before writes
	// even when not necessary to get around the "P5 double write bug"
	ulong value = lint_num == 0 ? lapic->lvt0 : lapic->lvt1;
	
	value = (IRQBASE + irq) | APIC_MODE_FIXED;
	
	if (lint_num == 0)
		lapic->lvt0 = value;
	else
		lapic->lvt1 = value;

	set_handler(IRQBASE + irq, handler);

	return OK;
}
