 /* intr.S
 */

/*------------------------------------------------------------------------
 * Generic exception handler(s) for x86-xinu.  Adapted from D.W.Brylow's 
 * version for the PPC port of XINU
 *------------------------------------------------------------------------
 */
/* Embedded Xinu, Copyright (C) 2011.  All rights reserved. */

#include <apic.h>
	.globl enable
	.globl disable
	.globl restore

#define EXCEPTION(num)                      \
        .globl _Xint##num;                  \
_Xint##num:                                 \
        cli;                                \
        pushal;                             \
        pushl   %esp;                       \
        pushl   $num;                       \
        call    dispatch;                   \
        addl    $2*4, %esp;                 \
		xorl	%eax, %eax;                 \
		movl	%eax, APIC_BASE + APIC_EOI; \
        popal;                              \
        sti;                                \
        iret;

/* Create the individual exception handlers */
EXCEPTION(0x00)
EXCEPTION(0x01)
EXCEPTION(0x02)
EXCEPTION(0x03)
EXCEPTION(0x04)
EXCEPTION(0x05)
EXCEPTION(0x06)
EXCEPTION(0x07)
EXCEPTION(0x08)
EXCEPTION(0x09)
EXCEPTION(0x0A)
EXCEPTION(0x0B)
EXCEPTION(0x0C)
EXCEPTION(0x0D)
EXCEPTION(0x0E)
EXCEPTION(0x0F)
EXCEPTION(0x10)
EXCEPTION(0x11)
EXCEPTION(0x12)
EXCEPTION(0x13)
EXCEPTION(0x14)
EXCEPTION(0x15)
EXCEPTION(0x16)
EXCEPTION(0x17)
EXCEPTION(0x18)
EXCEPTION(0x19)
EXCEPTION(0x1A)
EXCEPTION(0x1B)
EXCEPTION(0x1C)
EXCEPTION(0x1D)
EXCEPTION(0x1E)
EXCEPTION(0x1F)
EXCEPTION(0x20)
EXCEPTION(0x21)
EXCEPTION(0x22)
EXCEPTION(0x23)
EXCEPTION(0x24)
EXCEPTION(0x25)
EXCEPTION(0x26)
EXCEPTION(0x27)
EXCEPTION(0x28)
EXCEPTION(0x29)
EXCEPTION(0x2A)
EXCEPTION(0x2B)
EXCEPTION(0x2C)
EXCEPTION(0x2D)
EXCEPTION(0x2E)
EXCEPTION(0x2F)

/*------------------------------------------------------------------------
 * _Xint? - default exception and interrupt handler entry points
 *  NB: These must be contiguous
 *------------------------------------------------------------------------
 */

        .globl  defhandler
defhandler:
        .long   _Xint0x00
        .long   _Xint0x01
        .long   _Xint0x02
        .long   _Xint0x03
        .long   _Xint0x04
        .long   _Xint0x05
        .long   _Xint0x06
        .long   _Xint0x07
        .long   _Xint0x08
        .long   _Xint0x09
        .long   _Xint0x0A
        .long   _Xint0x0B
        .long   _Xint0x0C
        .long   _Xint0x0D
        .long   _Xint0x0E
        .long   _Xint0x0F
        .long   _Xint0x10
        .long   _Xint0x11
        .long   _Xint0x12
        .long   _Xint0x13
        .long   _Xint0x14
        .long   _Xint0x15
        .long   _Xint0x16
        .long   _Xint0x17
        .long   _Xint0x18
        .long   _Xint0x19
        .long   _Xint0x1A
        .long   _Xint0x1B
        .long   _Xint0x1C
        .long   _Xint0x1D
        .long   _Xint0x1E
        .long   _Xint0x1F
        .long   _Xint0x20
        .long   _Xint0x21
        .long   _Xint0x22
        .long   _Xint0x23
        .long   _Xint0x24
        .long   _Xint0x25
        .long   _Xint0x26
        .long   _Xint0x27
        .long   _Xint0x28
        .long   _Xint0x29
        .long   _Xint0x2A
        .long   _Xint0x2B
        .long   _Xint0x2C
        .long   _Xint0x2D
        .long   _Xint0x2E
        .long   _Xint0x2F

/*---------------------------------------------------------
 * enable all interrupts
 *---------------------------------------------------------
 */
enable:
    sti
    ret


/*---------------------------------------------------------
 * disable()    - disable interrupts, return old state
 *---------------------------------------------------------
 */
disable:
    pushfl
    cli
    popl    %eax
    ret


/*---------------------------------------------------------
 * restore(ps)    - restore interrupts to value in ps
 *---------------------------------------------------------
 */
restore:
    cli
    pushl   4(%esp)
    popfl
    ret
