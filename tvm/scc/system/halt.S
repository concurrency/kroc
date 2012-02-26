/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

.text
	.globl halt

halt:
	hlt
	jmp halt	/* Take care of any straggling interrupts. */
