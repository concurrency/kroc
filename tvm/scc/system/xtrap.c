/**
 * @file xtrap.c 
 * @provides xtrap
 *
 * $Id: xtrap.c 1390 2008-07-17 23:33:58Z mschul $
 */
/* Embedded XINU, Copyright (C) 2011.  All rights reserved. */

#include <interrupt.h>
//#include <kernel.h>
//#include <thread.h>
#include <stddef.h>
#include <stdio.h>

#define STACKMAGIC 0x0A0AAAA9

/* image text bounds */
extern void *_start(void);
extern void *_etext(void);

extern void halt(void);

#define TRAPS 16

char *trap_names[TRAPS] =
{
    "divide by zero",
    "debug exception",
    "non-maskable interrupt",
    "breakpoint",
    "overflow",
    "bounds check failure",
    "invalid opcode",
    "coprocessor not available",
    "double fault",
    "coprocessor segment overrun",
    "invalid TSS",
    "segment not present",
    "stack fault",
    "general protection violation",
    "page fault",
    "coprocessor error"
};

/**
 * Generic XINU Trap (Interrupt) Handler.
 *
 * @param cause the contents of the cause register used to decode the trap
 * @param frame pointer to the interrupt frame with saved status
 *
 * NOTE: this function assumes that when an exception occurs, _Xint#() is 
 * called (located in intr.S), which calls dispatch(), which calls xtrap().  
 * If that is changed (e.g. dispatch is removed, since it is rather 
 * unnecessary in x86), then the stack might be different, and the printed 
 * register values might be inaccurate.
 *
 * NOTE: it also assumes there is no privilege-level change when the handler 
 * is called.  In Xinu, we don't deal with privilege levels anyway.
 */
void xtrap(long cause, long *frame)
{
    int offset = 0;

    if ( (8 == cause) || (10 <= cause && cause <= 14) )
    {   
        offset = 1;
    }   

    printf("XINU Trap/Exception 0x%02x", cause);

    if (cause < TRAPS) { printf(" (%s)", trap_names[cause]); }

//    printf(": Process %d (\"%s\")\r\n", thrcurrent, 
//    thrtab[thrcurrent].name);

	/* pushed by pushal */
    printf("   edi: 0x%08x  ",   *(frame+0));
    printf("   esi: 0x%08x  ",   *(frame+1));
    printf("   ebp: 0x%08x  ",   *(frame+2));
    printf("   esp: 0x%08x\r\n", *(frame+3) + 4*(3+offset));
	/* add to esp to get its value just before the exception */
    printf("   ebx: 0x%08x  ",   *(frame+4));
    printf("   edx: 0x%08x  ",   *(frame+5));
    printf("   ecx: 0x%08x  ",   *(frame+6));
    printf("   eax: 0x%08x\r\n", *(frame+7));

	/* pushed by interrupt */
    printf("   eip: 0x%08x  ",   *(frame+offset+8));
    printf("    cs: 0x%08x  ",   *(frame+offset+9));
    printf("eflags: 0x%08x\r\n", *(frame+offset+10));

    if ( 1 == offset )
    {
        printf("error code: 0x%08x\r\n", *(frame+8));
    }

	printf("stack dump\r\n");
    for ( offset += 10 + 1; offset <= 50; offset+=1 )
    {   
        if ( (int)_start <= *(frame+offset) && *(frame+offset) < (int)_etext )
        {   
            printf("[0x%08x] 0x%08x <-- possible call boundary\r\n", frame+    offset, *(frame+offset));
        }   
        else
        {   
            printf("[0x%08x] 0x%08x\r\n", frame+offset, *(frame+offset));
        }   
        if ( STACKMAGIC == *(frame+offset) )
        {   
            printf("End of stack\r\n");
            break;
        }   
    }   

    halt();

    printf("did not halt.");
    while (1)
        ;
}
