/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#define CR0_NW (1 << 29)
#define CR0_CD (1 << 30)

/* void enable_caching(void) */
	.globl enable_caching
enable_caching:
	# CR0: clear NW, clear CD
	movl	%cr0, %eax
	andl	$~(CR0_CD | CR0_NW), %eax
	movl	%eax, %cr0

	ret
