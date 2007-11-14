/*
 *	PPC and GCC specific assembler inserts
 *	Copyright (C) 2005 Fred Barnes  <frmb@kent.ac.uk>
 *	Loosely based on i386 and Sparc versions
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * This file attempts to isolate as many as possible of the platform 
 * specific features of this package as possible.  The macros have been
 * designed so that it should be easy to port this kernel to a more
 * register based machine fairly easily (eg Alpha, ARM etc).  Note that
 * there are numerous functions in this file labelled such that are no
 * longer used.
 */

#ifndef SCHED_ASM_INSERTS_H
#define SCHED_ASM_INSERTS_H

/* these for debugging mostly */
#define LOAD_ESP(X) \
	__asm__ __volatile__ ("\n\tmr %0, 1\n\t" : "=r" (X))

#define STACK_CHK_ENTRY(callname) \
	LOAD_ESP (the_stackptr)
#define STACK_CHK_LEAVE(callname) \
	LOAD_ESP (the_stackptr2); \
	if (the_stackptr2 != the_stackptr) \
		fprintf (stderr, "CCSP: serious: stack deformation in %s: 0x%8.8x --> 0x%8.8x (%d words)\n", callname, the_stackptr, the_stackptr2, (int)(the_stackptr - the_stackptr2))



/* experimental kernel interface stuff
 */

#define K_PPC_SAVERTVARS() \
		"	lis	11, Fptr@ha		\n" \
		"	stw	4, Fptr@l(11)		\n" \
		"	lis	11, Bptr@ha		\n" \
		"	stw	5, Bptr@l(11)		\n" \
		"	lis	11, Wptr@ha		\n" \
		"	stw	3, Wptr@l(11)		\n"

#define K_PPC_SAVEPARAM(P,REG) \
		"	lis	11, "#P"@ha		\n" \
		"	stw	" REG ", "#P"@l(11)	\n"

#define K_PPC_STORERETURN() \
		"	mflr	0			\n" \
		"	stw	0, -4(3)		\n"

#define K_PPC_SAVERETURN() \
		"	lis	11, return_address@ha	\n" \
		"	mflr	0			\n" \
		"	stw	0, return_address@l(11)	\n"

#define K_PPC_LOADRTVARS() \
		"	lis	11, Fptr@ha		\n" \
		"	lwz	4, Fptr@l(11)		\n" \
		"	lis	11, Bptr@ha		\n" \
		"	lwz	5, Bptr@l(11)		\n" \
		"	lis	11, Wptr@ha		\n" \
		"	lwz	3, Wptr@l(11)		\n"

#define K_PPC_LOADRETURN() \
		"	lis	11, return_address@ha	\n" \
		"	lwz	0, return_address@l(11)	\n" \
		"	mtlr	0			\n" \
		"	blr				\n"

#define K_PPC_LOADJRETURN() \
		"	lwz	0, -4(3)		\n" \
		"	mtlr	0			\n" \
		"	blr				\n"

#define K_PPC_LOADIRETURN() \
		"	lwz	0, 0(3)			\n" \
		"	mtlr	0			\n" \
		"	blr				\n"

#define K_PPC_LOADPARAM(P,REG) \
		"	lis	11, "#P"@ha		\n" \
		"	lwz	" REG ", "#P"@l(11)	\n"

#define K_PPC_GLOBENTRYPOINT(X) \
		"					\n" \
		".globl _"#X"				\n" \
		"	_"#X":				\n" \
		"	"#X":				\n" 


#define K_SETGLABEL_FIVE_IN(X,A,B,C,D,E) \
	__asm__ __volatile__ ("				\n" \
		K_PPC_GLOBENTRYPOINT(X)			\
		K_PPC_SAVERTVARS()			\
		K_PPC_SAVERETURN()			\
		K_PPC_SAVEPARAM(A, "6")			\
		K_PPC_SAVEPARAM(B, "7")			\
		K_PPC_SAVEPARAM(C, "8")			\
		K_PPC_SAVEPARAM(D, "9")			\
		K_PPC_SAVEPARAM(E, "10")		\
	::: "memory", "cc")
#define K_SETGLABEL_FOUR_IN(X,A,B,C,D) \
	__asm__ __volatile__ ("				\n" \
		K_PPC_GLOBENTRYPOINT(X)			\
		K_PPC_SAVERTVARS()			\
		K_PPC_SAVERETURN()			\
		K_PPC_SAVEPARAM(A, "6")			\
		K_PPC_SAVEPARAM(B, "7")			\
		K_PPC_SAVEPARAM(C, "8")			\
		K_PPC_SAVEPARAM(D, "9")			\
	::: "memory", "cc")

#define K_SETGLABEL_THREE_IN(X,A,B,C) \
	__asm__ __volatile__ ("				\n" \
		K_PPC_GLOBENTRYPOINT(X)			\
		K_PPC_SAVERTVARS()			\
		K_PPC_SAVERETURN()			\
		K_PPC_SAVEPARAM(A, "6")			\
		K_PPC_SAVEPARAM(B, "7")			\
		K_PPC_SAVEPARAM(C, "8")			\
	::: "memory", "cc")


#define K_SETGLABEL_TWO_IN(X,A,B) \
	__asm__ __volatile__ ("				\n" \
		K_PPC_GLOBENTRYPOINT(X)			\
		K_PPC_SAVERTVARS()			\
		K_PPC_SAVERETURN()			\
		K_PPC_SAVEPARAM(A, "6")			\
		K_PPC_SAVEPARAM(B, "7")			\
	::: "memory", "cc")


#define K_SETGLABEL_ONE_IN(X,A) \
	__asm__ __volatile__ ("				\n" \
		K_PPC_GLOBENTRYPOINT(X)			\
		K_PPC_SAVERTVARS()			\
		K_PPC_SAVERETURN()			\
		K_PPC_SAVEPARAM(A, "6")			\
	::: "memory", "cc")


#define K_SETGLABEL_ZERO_IN(X) \
	__asm__ __volatile__ ("				\n" \
		K_PPC_GLOBENTRYPOINT(X)			\
		K_PPC_SAVERTVARS()			\
		K_PPC_SAVERETURN()			\
	::: "memory", "cc")

#define K_SETGLABEL_ZERO_IN_SR(X) \
	__asm__ __volatile__ ("				\n" \
		K_PPC_GLOBENTRYPOINT(X)			\
		K_PPC_SAVERTVARS()			\
		K_PPC_STORERETURN()			\
	::: "memory", "cc")

#define K_SETGLABEL_ONE_IN_SR(X,A) \
	__asm__ __volatile__ ("				\n" \
		K_PPC_GLOBENTRYPOINT(X)			\
		K_PPC_SAVERTVARS()			\
		K_PPC_STORERETURN()			\
		K_PPC_SAVEPARAM(A, "6")			\
	::: "memory", "cc")

#define K_SETGLABEL_TWO_IN_SR(X,A,B) \
	__asm__ __volatile__ ("				\n" \
		K_PPC_GLOBENTRYPOINT(X)			\
		K_PPC_SAVERTVARS()			\
		K_PPC_STORERETURN()			\
		K_PPC_SAVEPARAM(A, "6")			\
		K_PPC_SAVEPARAM(B, "7")			\
	::: "memory", "cc")

#define K_SETGLABEL_THREE_IN_SR(X,A,B,C) \
	__asm__ __volatile__ ("				\n" \
		K_PPC_GLOBENTRYPOINT(X)			\
		K_PPC_SAVERTVARS()			\
		K_PPC_STORERETURN()			\
		K_PPC_SAVEPARAM(A, "6")			\
		K_PPC_SAVEPARAM(B, "7")			\
		K_PPC_SAVEPARAM(C, "8")			\
	::: "memory", "cc")

#define K_ZERO_OUT() \
	__asm__ __volatile__ ("				\n" \
		K_PPC_LOADRTVARS()			\
		K_PPC_LOADRETURN()			\
	::: "memory", "cc")

#define K_ONE_OUT(A) \
	__asm__ __volatile__ ("				\n" \
		K_PPC_LOADRTVARS()			\
		K_PPC_LOADPARAM(A, "6")			\
		K_PPC_LOADRETURN()			\
	::: "memory", "cc")

#define K_THREE_OUT(A,B,C) \
	__asm__ __volatile__ ("				\n" \
		K_PPC_LOADRTVARS()			\
		K_PPC_LOADPARAM(A, "6")			\
		K_PPC_LOADPARAM(B, "7")			\
		K_PPC_LOADPARAM(C, "8")			\
		K_PPC_LOADRETURN()			\
	::: "memory", "cc")

#define K_ZERO_OUT_JRET() \
	__asm__ __volatile__ ("				\n" \
		K_PPC_LOADRTVARS()			\
		K_PPC_LOADJRETURN()			\
	::: "memory", "cc")

#define K_ZERO_OUT_IRET() \
	__asm__ __volatile__ ("				\n" \
		K_PPC_LOADRTVARS()			\
		K_PPC_LOADIRETURN()			\
	::: "memory", "cc")


/*
 * scheduler label functions
 */
#define K_LOADLABADDR(X,V) \
	__asm__ __volatile__ ("				\n" \
		"	lis	11, "#X"@ha		\n" \
		"	ori	%0, 11, "#X"@l		\n" \
		: "=r" (V) : : "9" \
		)
#define K_JUMP(X) \
	__asm__ __volatile__ ("				\n" \
		"	lis	0, "#X"@ha		\n" \
		"	ori	0, 0, "#X"@l		\n" \
		"	mtlr	0			\n" \
		"	blr				\n" \
		::: "0", "memory")
#define K_SETLABEL(X) \
	__asm__ __volatile__ ("\n" \
		"	"#X":\n" \
		::: "memory")
#define K_SETGLABEL(X) \
	__asm__ __volatile__ ("\n" \
		".globl _"#X"\n" \
		"	_"#X":\n" \
		"	"#X":\n" \
		::: "memory")


/* an EJUMP is a jump out of the kernel into user code, usually the return
   address 
   Note that only GCC supports goto *(X), may need to be replaced with an
   asm jump if porting. */
#define EJUMP(X) \
	__asm__ __volatile__ ("				\n" \
		"	mtlr	%0			\n" \
		"	blr				\n" \
		: /* no outputs */ : "r" (X) : "memory", "cc")
/* #define EJUMP(X) goto *(X) */

/* Restore the stack pointer before returning to C */
#define RESTORESP(X) \
  __asm__ __volatile__ ("				\n" \
		"	mr	1, %0			\n" \
		: /* no outputs */ \
		: "r" (X) \
		) 

#if 0
#define MEMCPY(SRC,DEST,COUNT) \
  __asm__ __volatile__ ( \
    "cld\n" \
    "rep\n" \
    "movsb\n" \
    : /* no outputs */ \
    : "S" (SRC), "D" (DEST), "c" (COUNT) \
  )
#else
#define MEMCPY(S,D,C) memcpy((D),(S),(C))
#endif

#ifdef PROCESS_PRIORITY

/* FIXME: ASM_BSF for PPC */
#if 0
#define ASM_BSF(V,R)	\
	__asm__ __volatile__ ("				\n" \
		"	mov	%1, %%l0		\n" \
		"	mov	0, %%l1			\n" \
		"	mov	1, %%l2			\n" \
		"	cmp	%%l0, %%r0		\n" \
		"	be	1f			\n" \
		"	nop				\n" \
		"0:					\n" \
		"	andcc	%%l0, %%l2, %%r0	\n" \
		"	bne	1f			\n" \
		"	nop				\n" \
		"	inc	%%l1			\n" \
		"	sll	%%l2, 1, %%l2		\n" \
		"	ba	0b			\n" \
		"	nop				\n" \
		"1:					\n" \
		"	mov	%%l1, %0		\n" \
    : "=r" (R) \
    : "r" (V) \
    : "cc", "l0", "l1", "l2" \
  )
#else
#define ASM_BSF(V,R) \
	{ \
		int l_i; 						\
		unsigned int l_w = 1;					\
									\
		for (l_i=0; l_i<32; l_i++, l_w <<= 1) {			\
			if (((V) & l_w) == l_w) {			\
				(R) = l_i;				\
				break;					\
			}						\
		}							\
		if (l_i == 32) {					\
			(R) = l_i;					\
		}							\
	}
#endif

#endif	/* PROCESS_PRIORITY */

#ifdef DYNAMIC_PROCS

/* FIXME: dynamic PROCs code for PPC */
#if 0
#define KERNEL_RUN_CODE(WP,EP) \
	__asm__ __volatile__ ("						\n" \
		"	sethi	%%hi("#WP"), %%l0			\n" \
		"	or	%%l0, %%lo("#WP"), %%l0			\n" \
		"	ld	[%%l0], %%i0				\n" \
		"	sethi	%%hi(Fptr), %%l0			\n" \
		"	or	%%l0, %%lo(Fptr), %%l0			\n" \
		"	ld	[%%l0], %%l6				\n" \
		"	sethi	%%hi(Bptr), %%l0			\n" \
		"	or	%%l0, %%lo(Bptr), %%l0			\n" \
		"	ld	[%%l0], %%l7				\n" \
		"	sethi	%%hi(0f), %%l0				\n" \
		"	or	%%l0, %%lo(0f), %%l0			\n" \
		"	st	%%l0, [%%i0]				\n" \
		"	sethi	%%hi("#EP"), %%l0			\n" \
		"	or	%%l0, %%lo("#EP"), %%l0			\n" \
		"	jmpl	%%l0, %%r0				\n" \
		"	nop						\n" \
		"0:     sethi	%%hi(_X_dynproc_exit), %%l4		\n" \
		"	or	%%l4, %%lo(_X_dynproc_exit), %%l4	\n" \
		"	jmpl	%%l4, %%r0				\n" \
		"	nop						\n" \
		::: "memory", "cc", "l0", "l1", "l2", "l3", "l4", "l5", "l6", "l7")
#else
#define KERNEL_RUN_CODE(WP,EP)
#endif


#endif	/* DYNAMIC_PROCS */

#ifndef BLOCKING_SYSCALLS

/* FIXME: outbyte code for PPC */
#if 0
#define CALL_X_OUTBYTE_CODE \
	__asm__ __volatile__ ("					\n" \
		"	sub	%%sp, 4, %%sp			\n" \
		"	sethi	%%hi(0f), %%l0			\n" \
		"	or	%%l0, %%lo(0f), %%l0		\n" \
		"	st	%%l0, [%%sp]			\n" \
		"	sethi	%%hi(X_outbyte), %%l4		\n" \
		"	or	%%l4, %%lo(X_outbyte), %%l4	\n" \
		"	jmpl	%%l4, %%r0			\n" \
		"	nop					\n" \
		: /* no outputs */ : /* no inputs */ : "memory", "cc", "l0", "l1", "l2", "l3", "l4", "l5");
#else
#define CALL_X_OUTBYTE_CODE
#endif

#endif	/* !BLOCKING_SYSCALLS */


#endif /* sched_asm_inserts.h */


