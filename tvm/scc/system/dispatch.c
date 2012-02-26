/* Embedded Xinu, Copyright (C) 2011.  All rights reserved. */
#include <interrupt.h>

extern void xtrap(int, int *);

interrupt (*handlertab[NID])(void);

void dispatch(int exc_num, int *stack_ptr)
{
	if ( handlertab[exc_num] != NULL ) {
		/* execute handler */
		(*handlertab[exc_num])();
	} else {
		xtrap(exc_num, stack_ptr);
	}

	return;
}
