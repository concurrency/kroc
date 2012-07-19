 /*
 * This is intended to get the processor into 32-bit protected mode.
 * Based off of Embedded X86 Programming: Protected Mode by Jean Gareau
 * with help from Mike Schultz's startup.S from Embedded Xinu for x86.
 */
/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#define STACKMAGIC 0x0A0AAAA9

.code16
.text
	.globl _start
	.extern initPaging

.org 0x0000
_start:
	jmp getprotected
	
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

getprotected:
	/* hard code an LGDT */
	.byte	0x66		/* 32-bit operand override */
	.byte	0x8d		/* lea (e)bx,Addr */
	.byte	0x1e
	.long	gdt_desc
	.byte	0x0f		/* lgdt fword ptr [bx] */
	.byte	0x01
	.byte	0x17

	movw	$1, %ax		/* Protected mode bit */
	lmsw	%ax			/* Protected mode is go! (but not 32-bit yet!) */
	movw	$0x10, %ax	/* Load data seg regs with index to GDT[2] entry! */
	movw	%ax, %ds
	movw	%ax, %es
	movw	%ax, %ss
	movw	%ax, %fs
	movw	%ax, %gs

	/* hard code an LJMP */
	.byte	0x66		/* 32-bit operand override */
	.byte	0xea		/* far jump */
	.long	prot_world	/* 32-bit offset */
	.word	0x8			/* 16-bit selector ([index into GDT] * 8) */
						/* NOTE: that ljmp implicitly sets %cs to 0x8 */

.code32
prot_world:
	movl	$_end,	%eax	/* set stack pointer to 8k after end of startup code */
	addl	$8*1024, %eax
	decl	%eax			/* 16-byte align stack pointer */
	shrl	$0x04,	%eax
	shll	$0x04,	%eax
	movl	%eax,	%esp

	#
	# clear Bss section
	#
	movl	$_end,	%ecx
	subl	$edata,	%ecx
	pushl	%ecx
	pushl	$edata
	call	_asm_bzero
	addl	$2*4, %esp

	call	initPaging

	ljmp	$0x8, $IMG_ADDR /* Jump to kernel */


    #
    # _asm_bzero (base, count)
    #
	.globl _asm_bzero
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


#define CR3_ADDR_MASK 0xfffff000
#define CR0_PG (1 << 31)
#define CR4_MPE (1 << 11)
#define CR4_PSE (1 << 4)
#define CR4_PAE (1 << 5)

/* void enable_paging(ulong page_dir_loc) */
	.globl enable_paging
enable_paging:
	# stuff CR3 with page_dir_loc
	movl	4(%esp), %eax
	andl	$CR3_ADDR_MASK, %eax
	movl	%eax, %cr3
	
	# CR4: set MPE, clear PSE, clear PAE
	movl	%cr4, %eax
	orl 	$CR4_MPE, %eax
	andl	$~(CR4_PSE | CR4_PAE), %eax
	movl	%eax, %cr4

	# CR0: set PG bit
	movl	%cr0, %eax
	orl 	$CR0_PG, %eax
	movl	%eax, %cr0
	ret
