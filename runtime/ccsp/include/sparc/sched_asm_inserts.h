/*
 *	Sparc and GCC specific assembler inserts
 *	Copyright (C) 2004 Fred Barnes  <frmb@kent.ac.uk>
 *	Loosely based on i386 version
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
	__asm__ __volatile__ ("\n\tmov %%sp, %0\n\t" : "=r" (X))

#define STACK_CHK_ENTRY(callname) \
	LOAD_ESP (the_stackptr)
#define STACK_CHK_LEAVE(callname) \
	LOAD_ESP (the_stackptr2); \
	if (the_stackptr2 != the_stackptr) \
		fprintf (stderr, "CCSP: serious: stack deformation in %s: 0x%8.8x --> 0x%8.8x (%d words)\n", callname, the_stackptr, the_stackptr2, (int)(the_stackptr - the_stackptr2))



/* experimental kernel interface stuff
 */

#define K_SPARC_SAVERTVARS() \
		"	set	Fptr, %%l4		\n" \
		"	st	%%l6, [%%l4]		\n" \
		"	set	Bptr, %%l4		\n" \
		"	st	%%l7, [%%l4]		\n" \
		"	set	Wptr, %%l4		\n" \
		"	st	%%i0, [%%l4]		\n"

#define K_SPARC_SAVEPARAM(P,REG) \
		"	set	"#P", %%l4		\n" \
		"	st	" REG ", [%%l4]		\n"

#define K_SPARC_STORERETURN() \
		"	st	%%o7, [%%i0 - 4]	\n"

#define K_SPARC_SAVERETURN() \
		"	set	return_address, %%l4	\n" \
		"	st	%%o7, [%%l4]		\n"

#define K_SPARC_LOADRTVARS() \
		"	set	Fptr, %%l4		\n" \
		"	ld	[%%l4], %%l6		\n" \
		"	set	Bptr, %%l4		\n" \
		"	ld	[%%l4], %%l7		\n" \
		"	set	Wptr, %%l4		\n" \
		"	ld	[%%l4], %%i0		\n"

#define K_SPARC_LOADRETURN() \
		"	set	return_address, %%l4	\n" \
		"	ld	[%%l4], %%l5		\n" \
		"	jmpl	%%l5, %%r0		\n" \
		"	nop				\n"

#define K_SPARC_LOADJRETURN() \
		"	ld	[%%i0 - 4], %%l4	\n" \
		"	jmpl	%%l4, %%r0		\n" \
		"	nop				\n"

#define K_SPARC_LOADIRETURN() \
		"	ld	[%%i0], %%l4		\n" \
		"	jmpl	%%l4, %%r0		\n" \
		"	nop				\n"

#define K_SPARC_LOADPARAM(P,REG) \
		"	set	"#P", %%l4		\n" \
		"	ld	[%%l4], " REG "		\n"

#define K_SPARC_GLOBENTRYPOINT(X) \
		"					\n" \
		".globl _"#X"				\n" \
		"	_"#X":				\n" \
		"	"#X":				\n" 


#define K_SETGLABEL_FIVE_IN(X,A,B,C,D,E) \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_GLOBENTRYPOINT(X)		\
		K_SPARC_SAVERTVARS()			\
		K_SPARC_SAVERETURN()			\
		K_SPARC_SAVEPARAM(A, "%%l0")		\
		K_SPARC_SAVEPARAM(B, "%%l1")		\
		K_SPARC_SAVEPARAM(C, "%%l2")		\
		K_SPARC_SAVEPARAM(D, "%%l3")		\
		K_SPARC_SAVEPARAM(E, "%%i1")		\
	::: "memory", "cc")
#define K_SETGLABEL_FOUR_IN(X,A,B,C,D) \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_GLOBENTRYPOINT(X)		\
		K_SPARC_SAVERTVARS()			\
		K_SPARC_SAVERETURN()			\
		K_SPARC_SAVEPARAM(A, "%%l0")		\
		K_SPARC_SAVEPARAM(B, "%%l1")		\
		K_SPARC_SAVEPARAM(C, "%%l2")		\
		K_SPARC_SAVEPARAM(D, "%%l3")		\
	::: "memory", "cc")

#define K_SETGLABEL_THREE_IN(X,A,B,C) \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_GLOBENTRYPOINT(X)		\
		K_SPARC_SAVERTVARS()			\
		K_SPARC_SAVERETURN()			\
		K_SPARC_SAVEPARAM(A, "%%l0")		\
		K_SPARC_SAVEPARAM(B, "%%l1")		\
		K_SPARC_SAVEPARAM(C, "%%l2")		\
	::: "memory", "cc")


#define K_SETGLABEL_TWO_IN(X,A,B) \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_GLOBENTRYPOINT(X)		\
		K_SPARC_SAVERTVARS()			\
		K_SPARC_SAVERETURN()			\
		K_SPARC_SAVEPARAM(A, "%%l0")		\
		K_SPARC_SAVEPARAM(B, "%%l1")		\
	::: "memory", "cc")


#define K_SETGLABEL_ONE_IN(X,A) \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_GLOBENTRYPOINT(X)		\
		K_SPARC_SAVERTVARS()			\
		K_SPARC_SAVERETURN()			\
		K_SPARC_SAVEPARAM(A, "%%l0")		\
	::: "memory", "cc")


#define K_SETGLABEL_ZERO_IN(X) \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_GLOBENTRYPOINT(X)		\
		K_SPARC_SAVERTVARS()			\
		K_SPARC_SAVERETURN()			\
	::: "memory", "cc")

#define K_SETGLABEL_ZERO_IN_SR(X) \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_GLOBENTRYPOINT(X)		\
		K_SPARC_SAVERTVARS()			\
		K_SPARC_STORERETURN()			\
	::: "memory", "cc")

#define K_SETGLABEL_ONE_IN_SR(X,A) \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_GLOBENTRYPOINT(X)		\
		K_SPARC_SAVERTVARS()			\
		K_SPARC_STORERETURN()			\
		K_SPARC_SAVEPARAM(A, "%%l0")		\
	::: "memory", "cc")

#define K_SETGLABEL_TWO_IN_SR(X,A,B) \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_GLOBENTRYPOINT(X)		\
		K_SPARC_SAVERTVARS()			\
		K_SPARC_STORERETURN()			\
		K_SPARC_SAVEPARAM(A, "%%l0")		\
		K_SPARC_SAVEPARAM(B, "%%l1")		\
	::: "memory", "cc")

#define K_SETGLABEL_THREE_IN_SR(X,A,B,C) \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_GLOBENTRYPOINT(X)		\
		K_SPARC_SAVERTVARS()			\
		K_SPARC_STORERETURN()			\
		K_SPARC_SAVEPARAM(A, "%%l0")		\
		K_SPARC_SAVEPARAM(B, "%%l1")		\
		K_SPARC_SAVEPARAM(C, "%%l2")		\
	::: "memory", "cc")

#define K_ZERO_OUT() \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_LOADRTVARS()			\
		K_SPARC_LOADRETURN()			\
	::: "memory", "cc")

#define K_ONE_OUT(A) \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_LOADRTVARS()			\
		K_SPARC_LOADPARAM(A, "%%l0")		\
		K_SPARC_LOADRETURN()			\
	::: "memory", "cc")

#define K_THREE_OUT(A,B,C) \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_LOADRTVARS()			\
		K_SPARC_LOADPARAM(A, "%%l0")		\
		K_SPARC_LOADPARAM(B, "%%l1")		\
		K_SPARC_LOADPARAM(C, "%%l2")		\
		K_SPARC_LOADRETURN()			\
	::: "memory", "cc")

#define K_ZERO_OUT_JRET() \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_LOADRTVARS()			\
		K_SPARC_LOADJRETURN()			\
	::: "memory", "cc")

#define K_ZERO_OUT_IRET() \
	__asm__ __volatile__ ("				\n" \
		K_SPARC_LOADRTVARS()			\
		K_SPARC_LOADIRETURN()			\
	::: "memory", "cc")


/*
 * scheduler label functions
 */
#define K_LOADLABADDR(X,V) \
	__asm__ __volatile__ ("\n" \
		"	set "#X", %%l0\n" \
		"	mov %%l0, %0\n" \
		: "=r" (V) : : "l0" \
		)
#define K_JUMP(X) \
	__asm__ __volatile__ ("				\n" \
		"	ba	"#X"			\n" \
		"	nop				\n" \
		::: "memory")
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
		"	jmpl	%0, %%r0		\n" \
		"	nop				\n" \
		: /* no outputs */ : "r" (X) : "memory", "cc")
/* #define EJUMP(X) goto *(X) */

/* Restore the stack pointer before returning to C */
#define RESTORESP(X) \
  __asm__ __volatile__ ("ld   %0, %%sp\n" \
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

#endif	/* PROCESS_PRIORITY */

#ifdef DYNAMIC_PROCS

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


#endif	/* DYNAMIC_PROCS */

#ifndef BLOCKING_SYSCALLS
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

#endif	/* !BLOCKING_SYSCALLS */


#endif /* sched_asm_inserts.h */


