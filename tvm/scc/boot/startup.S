 /*
 * After getprotected.S gets us into 32-bit protected mode, startup.S now
 * creates another GDT and loads it, then it does some initialization and
 * jumps to C code.
 * Based off of Embedded X86 Programming: Protected Mode by Jean Gareau
 * with help from Mike Schultz's startup.S from Embedded Xinu for x86.
 */
/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

/* for NID */
#include <interrupt.h>
#include <paging.h>
#define STACKMAGIC 0x0A0AAAA9

.code32
.text
	.globl _start
	.globl gdt
	.globl idt
	.extern platforminit
	.extern main
	.extern halt
	.extern _end

.org 0x0000
_start:
	jmp startup
	
	.align	8	/* Intel likes 8-byte alignment for the GDT */
gdt:
/* GDT[0]: Null entry, never used. */
	.word	0, 0, 0, 0

/* GDT[1]: Executable, read-only code, base address of 0, limit of FFFFFh,
 * granularity bit (G) set (making the limit 4GB) */
	.word	0xffff		/* Limit[15..0] */
	.word	0x0000		/* Base[15..0] */
	.byte	0x00		/* Base[23..16] */
	.byte	0b10011010	/* P(1) DPL(00) S(1) 1 C(0) R(1) A(0) */
	.byte	0b11001111	/* G(1) D(1) 0 0 Limit[19..16] */
	.byte	0x00		/* Base[31..24] */

/* GDT[2]: Writable data segment, covering the same address space as GDT[1]. */
	.word	0xffff		/* Limit[15..0] */
	.word	0x0000		/* Base[15..0] */
	.byte	0x00		/* Base[23..16] */
	.byte	0b10010010	/* P(1) DPL(00) S(1) 0 E(0) W(1) A(0) */
	.byte	0b11001111	/* G(1) B(1) 0 0 Limit[19..16] */
	.byte	0x00		/* Base[31..24] */
gdt_end:

	.align	4
gdt_desc:
	.word	gdt_end - gdt - 1	/* gdt limit */
	.long	gdt					/* gdt base */

	.align	8	/* Intel likes 8-byte alignment for the IDT */
idt:
	.space	NID * 8, 0			/* must equal NID*8 */
idt_desc:
	.word	idt_desc - idt - 1	/* size of idt - 1 (in bytes) */
	.long	idt

startup:
	lgdt	gdt_desc
	ljmp	$0x8, $prot_world

prot_world:
	lidt	idt_desc		/* load interrupt descriptor table register */

	movl	$_end,	%eax	/* set stack pointer to 64k after end of image */
	addl	$64*1024, %eax
	decl	%eax			/* 16-byte align stack pointer */
	shrl	$0x04,	%eax
	shll	$0x04,	%eax
	movl	%eax,	%esp

	#
	# clear stack
	#
	movl	%esp,	%ecx
	movl	$_end,	%eax
	subl	%eax,	%ecx
	subl	$5*4,	%ecx
	pushl	%ecx
	pushl	$_end
	call	_asm_bzero
	addl	$2*4,	%esp

	pushl	$STACKMAGIC		# put the magic number in place

	#
	# clear Bss section
	#
	movl	$_end,	%ecx	# Clear from `edata` to `_end`
	subl	$edata,	%ecx
	pushl	%ecx
	pushl	$edata
	call	_asm_bzero
	addl	$2*4,	%esp

#ifdef RCCE_SUPPORT

	/* mark beginning of heap mem right after pagetable */
	.extern memheap
	leal	memheap, %edx
	movl	$(PTBASE + 0x400000), (%edx)

	/* Give a dummy 'stdout' symbol so fprintf doesn't complain */
	.globl	stdout
stdout:

#endif /* RCCE_SUPPORT */

	finit	/* Initialize the floating point unit */

	/* set base pointer equal to stack pointer for C code */
	movl	%esp,	%ebp

	call	platforminit
	call	main		/* C code! */
	call	halt


	#
	# _asm_bzero (base, count)
	#
_asm_bzero:
	pushl	%edi
	movl	8(%esp),	%edi
	movl	12(%esp),	%ecx
	movb	$0x00,	%al
	cld
	rep
	stosb
	popl	%edi
	ret
