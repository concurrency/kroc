/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

/* Most of these definitions are taken from include/asm-i386/apicdef.h from 
 * Intel's SCC Linux; that file is attributed to Alan Cox and Ingo Molnar. */

#ifndef _APIC_H_
#define _APIC_H_

#define APIC_BASE	0xfee00000

#define     APIC_EOI    0xB0
#define         APIC_EOI_ACK    0x0
#define     APIC_SPIV   0xF0
#define         APIC_SPIV_APIC_ENABLED   (1<<8)
#define         APIC_VECTOR_MASK         0x000FF
#define     APIC_LVTT   0x320
#define     APIC_LVT0   0x350
#define         APIC_LVT_TIMER_PERIODIC (1<<17)
#define         APIC_LVT_MASKED         (1<<16)
#define         APIC_LVT_LEVEL_TRIGGER  (1<<15)
#define         APIC_LVT_REMOTE_IRR     (1<<14)
#define         APIC_INPUT_POLARITY     (1<<13)
#define         APIC_SEND_PENDING       (1<<12)
#define         APIC_MODE_MASK          0x700
#define             APIC_MODE_FIXED     (0x0 << 8)
#define             APIC_MODE_NMI       (0x4 << 8)
#define             APIC_MODE_EXTINT    (0x7 << 8)
#define     APIC_LVT1   0x360
#define     APIC_LVTERR 0x370
#define     APIC_TMICT  0x380
#define     APIC_TMCCT  0x390
#define     APIC_TDCR   0x3E0
#define         APIC_TDR_DIV_1      0xB
#define         APIC_TDR_DIV_2      0x0
#define         APIC_TDR_DIV_4      0x1
#define         APIC_TDR_DIV_8      0x2
#define         APIC_TDR_DIV_16     0x3
#define         APIC_TDR_DIV_32     0x8
#define         APIC_TDR_DIV_64     0x9
#define         APIC_TDR_DIV_128    0xA

#ifndef __ASSEMBLER__

struct apic
{
	char pad00[0xb0];
	unsigned long     eoi;    /* end-of-interrupt reg */
	char pad01[0x3c];
	unsigned long     spiv;   /* spurrious interrupt vector reg */
	char pad02[0x22c];
	unsigned long     lvtt;   /* local vector table timer interrupt entry */
	char pad03[0x2c];
	unsigned long     lvt0;   /* local vector table LINT0 interrupt entry */
	char pad04[0xc];
	unsigned long     lvt1;   /* local vector table LINT1 interrupt entry */
	char pad05[0xc];
	unsigned long     lvterr; /* local vector table error interrupt entry */
	char pad06[0xc];
	unsigned long     tmict;  /* timer initial count reg */
	char pad07[0xc];
	unsigned long     tmcct;  /* timer current count reg */
	char pad08[0x4c];
	unsigned long     tdcr;   /* timer divide configuration reg */
};

extern volatile struct apic *lapic; /* pointer to local APIC */


__inline__ static void ack_APIC_irq(void) __attribute__((always_inline));

__inline__ static void ack_APIC_irq(void)
{
	lapic->eoi = APIC_EOI_ACK;
}

void init_APIC(void);
int setupLINT(int, int, void (*)(void));

#endif /* __ASSEMBLER__ */
#endif /* _APIC_H_ */
