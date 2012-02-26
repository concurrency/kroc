/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#include <stddef.h>
#include <interrupt.h>

#define KERNEL_CS (0x8)

extern struct trap_gate idt[NID];
extern void *defhandler[];

void init_idt()
{
	int i;

	for (i = 0; i < NID; i++) {
		/* No handler defined yet */
		handlertab[i] = NULL;

		set_trap_gate(i, defhandler[i]); 
	}
}

/* Set the handler address for an entry in the IDT */
void set_trap_gate(int vec, void *addr)
{
	struct trap_gate *tgptr;

	tgptr = &idt[vec];
	tgptr->loffset = (ushort)((ulong)addr & 0xffff);
	tgptr->hoffset = (ushort)((ulong)addr >> 16);
	tgptr->segsel = KERNEL_CS;
	tgptr->mbz = 0;
	tgptr->type = IDT_TRAP_GATE;
	tgptr->dpl = 0;
	tgptr->present = 1;
}

/* Set the handler for an interrupt whose IDT entry points to the default 
 * handler.  I.e. set the handler that is called by dispatch(). */
void set_handler(int vec, interrupt (*handler)(void))
{
	handlertab[vec] = handler;
}
