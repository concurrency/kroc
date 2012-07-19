/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#include <apic.h>
#include <conf.h>
#define TRUE 1    /* so we don't have to include stddef */
#define FALSE 0

#if RTCLOCK
.text
	.globl clockIRQ
clockIRQ:
	cli
	pushal
	call	clkhandler
	xorl	%eax, %eax
	movl	%eax, APIC_BASE + APIC_EOI
	popal
	sti
	iret
#endif /* RTCLOCK */
