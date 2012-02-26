/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#ifndef _INTERRUPT_H_
#define _INTERRUPT_H_

#ifndef __ASSEMBLER__
#include <stddef.h>
#endif

#define IRQBASE 32 /* base ivec for IRQ0                  */
#define IRQ0     0 /* programmable interval timer         */
#define IRQ1     1
#define IRQ2     2
#define IRQ3     3 /* MPB                                 */
#define IRQ4     4
#define IRQ5     5
#define IRQ6     6
#define IRQ7     7
#define IRQ8     8
#define IRQ9     9
#define IRQ10   10
#define IRQ11   11
#define IRQ12   12
#define IRQ13   13
#define IRQ14   14
#define IRQ15   15
#define IRQMAX  IRQ15

#define LOCAL_TIMER_IRQ	IRQ0

#define IDT_TASK_GATE    5  /* task gate IDT descriptor       */
#define IDT_INTR_GATE   14  /* interrupt gate IDT descriptor  */
#define IDT_TRAP_GATE   15  /* Trap Gate                      */

#define NID  48

#ifndef __ASSEMBLER__

typedef unsigned long irqmask;  /**< machine status for disable/restore  */

/** Tables of exception/interrupt vectors */
extern interrupt (*handlertab[NID])(void);

/* Interrrupt enabling function prototypes */
irqmask disable(void);
irqmask restore(irqmask);
irqmask enable(void);
irqmask enable_irq(irqmask);
irqmask disable_irq(irqmask);

/* Interrupt configuration function prototypes */
void init_idt(void);
void set_trap_gate(int, void *);
void set_handler(int, interrupt (*)(void));
void dispatch(int, int *);

struct trap_gate
{
    unsigned short  loffset;
    unsigned short  segsel;
    unsigned int    rsvd : 5;
    unsigned int    mbz : 3;
    unsigned int    type : 5;
    unsigned int    dpl : 2;
    unsigned int    present : 1;
    unsigned short  hoffset;
};

#endif /* __ASSEMBLER__ */

#endif /* _INTERRUPT_H_ */

