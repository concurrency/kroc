/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

.code16	
.text

.org 0x0000
backabit:
    cli
    xor     %ax, %ax
    mov     %ax, %ds
    mov     %ax, %es
    mov     %ax, %fs
    mov     %ax, %gs
    mov     %ax, %ss
    mov     $0x8ffe, %sp
    ljmp    $0, $GETPROT_ADDR

.org 0x0ff0
_start:
	jmp	backabit
