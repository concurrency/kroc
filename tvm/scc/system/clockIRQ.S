/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#include <apic.h>
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
