/*
 *	Pentium/386 and GCC specific assembler inserts
 *	Copyright (C) 1998 Jim Moores
 *	Modification copyright (C) 1999-2002 Fred Barnes  <frmb2@ukc.ac.uk>
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
	__asm__ __volatile__ ("\n\tmovl %%esp, %0\n\t" : "=r" (X))

#define STACK_CHK_ENTRY(callname) \
	LOAD_ESP (the_stackptr)
#define STACK_CHK_LEAVE(callname) \
	LOAD_ESP (the_stackptr2); \
	if (the_stackptr2 != the_stackptr) \
		fprintf (stderr, "CCSP: serious: stack deformation in %s: 0x%8.8x --> 0x%8.8x (%d words)\n", callname, the_stackptr, the_stackptr2, (int)(the_stackptr - the_stackptr2))

#ifdef CHECKING_MODE
#define SAVE_EIP(X) \
	__asm__ __volatile__ ("		\n"		\
		"	movl $0f, %0	\n"		\
		"0:			\n"		\
		: "=m" ((X))				\
	)
#define TRACE_RETURN(addr)	\
	do { 						\
		sched->mdparam[8] = (word) Wptr;	\
		sched->mdparam[9] = (word) (addr);	\
		SAVE_EIP (sched->mdparam[11]);		\
	} while (0)
#define TRACE_RESCHEDULE \
	SAVE_EIP (sched->mdparam[12]);
#else
#define	TRACE_RETURN(addr)	do { } while (0)
#define TRACE_RESCHEDULE	do { } while (0)
#endif /* CHECKING_MODE */

/* Restore the workspace pointer and kernel entry table pointers to their
 * values before this function call.  This should be combined with a EJUMP
 * really, because it always happens.  Note that the jump table pointer is
 * not necessary in C.
 */

/*{{{  K_ZERO_OUT_JRET */
#ifdef CHECKING_MODE
#define K_ZERO_OUT_JRET() \
	TRACE_RETURN (Wptr[Iptr]); \
	__asm__ __volatile__ ("				\n" \
		"	movl	%3, %%esp		\n" \
		"	movl	%0, %%ebp		\n" \
		"	xchg	%%edi, -4(%%ebp)	\n" \
		"	jmp	*%%edi			\n" \
		: /* no outputs */ \
		: "r" (Wptr), "S" (sched), "D" (-1), "g" (sched->stack) \
		: "memory")
#else
#define K_ZERO_OUT_JRET() \
	TRACE_RETURN (Wptr[Iptr]); \
	__asm__ __volatile__ ("				\n" \
		"	movl	%2, %%esp		\n" \
		"	movl	%0, %%ebp		\n" \
		"	jmp	*-4(%%ebp)		\n" \
		: /* no outputs */ \
		: "r" (Wptr), "S" (sched), "g" (sched->stack) \
		: "memory")
#endif /* CHECKING_MODE */
/*}}}*/

/*{{{  RESCHEDULE - enter schedule from another kernel function */
#define RESCHEDULE \
	TRACE_RESCHEDULE; \
	__asm__ __volatile__ ("				\n" \
		"	jmp	_X_scheduler		\n" \
		: /* no outputs */ \
		: "S" (sched) \
		: "memory")
/*}}}*/

/*{{{  _K_SETGLABEL - internal global label define for inside asm blocks */
#define LABEL_ALIGN ".p2align 4	\n"
#ifndef NO_ASM_TYPE_DIRECTIVE
#define LABEL_TYPE(P,X) ".type "#P""#X", @function \n"
#else
#define LABEL_TYPE(P,X) 
#endif

#define _K_SETGLABEL(X) \
	LABEL_ALIGN \
	LABEL_TYPE(_,X) \
	".globl _"#X"	\n" \
	"	_"#X":	\n" \
	"	"#X":	\n"

#define _K_SETGGLABEL(X) \
	LABEL_ALIGN \
	LABEL_TYPE(_,X) \
	".globl _"#X"	\n" \
	"	_"#X":	\n" \
	LABEL_TYPE( ,X) \
	".globl "#X"	\n" \
	"	"#X":	\n"
/*}}}*/

/*{{{  incoming entry-point macros*/
#define K_SETGLABEL_IN_HEADER \
	sched_t *sched; \
	word *Wptr;
#define K_SETGLABEL_IN_SR_HEADER K_SETGLABEL_IN_HEADER

#define _K_SETGLABEL_FOUR_IN(X,A,B,C,D) \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), \
		  "=d" (D), "=c" (C), "=b" (B), "=a" (A) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_FOUR_IN(X,A,B,C,D) \
	K_SETGLABEL_IN_HEADER \
	_K_SETGLABEL_FOUR_IN(X,A,B,C,D)
#define _K_SETGLABEL_FOUR_IN_SR(X,A,B,C,D) _K_SETGLABEL_FOUR_IN(X,A,B,C,D)
#define K_SETGLABEL_FOUR_IN_SR(X,A,B,C,D) K_SETGLABEL_FOUR_IN(X,A,B,C,D)

#define _K_SETGLABEL_THREE_IN(X,A,B,C) \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=c" (C), "=b" (B), "=a" (A) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_THREE_IN(X,A,B,C) \
	K_SETGLABEL_IN_HEADER \
	_K_SETGLABEL_THREE_IN(X,A,B,C)
#define _K_SETGLABEL_THREE_IN_SR(X,A,B,C) _K_SETGLABEL_THREE_IN(X,A,B,C)
#define K_SETGLABEL_THREE_IN_SR(X,A,B,C) K_SETGLABEL_THREE_IN(X,A,B,C)

#define _K_SETGLABEL_TWO_IN(X,A,B) \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=b" (B), "=a" (A) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_TWO_IN(X,A,B) \
	K_SETGLABEL_IN_HEADER \
	_K_SETGLABEL_TWO_IN(X,A,B)
#define _K_SETGLABEL_TWO_IN_SR(X,A,B) _K_SETGLABEL_TWO_IN(X,A,B)
#define K_SETGLABEL_TWO_IN_SR(X,A,B) K_SETGLABEL_TWO_IN(X,A,B)

#define _K_SETGLABEL_ONE_IN(X,A) \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=a" (A) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_ONE_IN(X,A) \
	K_SETGLABEL_IN_HEADER \
	_K_SETGLABEL_ONE_IN(X,A)
#define _K_SETGLABEL_ONE_IN_SR(X,A) _K_SETGLABEL_ONE_IN(X,A)
#define K_SETGLABEL_ONE_IN_SR(X,A) K_SETGLABEL_ONE_IN(X,A)

#define _K_SETGLABEL_ZERO_IN(X) \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_ZERO_IN(X) \
	K_SETGLABEL_IN_HEADER \
	_K_SETGLABEL_ZERO_IN(X)
#define _K_SETGLABEL_ZERO_IN_SR(X) _K_SETGLABEL_ZERO_IN(X,A)
#define K_SETGLABEL_ZERO_IN_SR(X) K_SETGLABEL_ZERO_IN(X)
/*}}}*/
/*{{{  incoming entry-point macros (with load-call-return)*/
#define K_SETGLABEL_IN_LCR_HEADER \
	unsigned int return_address; \
	sched_t *sched; \
	word *Wptr;

#define K_SETGLABEL_THREE_IN_LCR(X,A,B,C) \
	K_SETGLABEL_IN_LCR_HEADER \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	popl	%2			\n" \
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=q" (return_address), "=c" (C), "=b" (B), "=a" (A) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_TWO_IN_LCR(X,A,B) \
	K_SETGLABEL_IN_LCR_HEADER \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	popl	%2			\n" \
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=q" (return_address), "=b" (B), "=a" (A) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_ONE_IN_LCR(X,A) \
	K_SETGLABEL_IN_LCR_HEADER \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	popl	%2			\n" \
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=q" (return_address), "=a" (A) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_ZERO_IN_LCR(X) \
	K_SETGLABEL_IN_LCR_HEADER \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	popl	%2			\n" \
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=q" (return_address) \
		: /* no inputs */ \
		: "memory", "cc")
/*}}}*/
/*{{{  incoming entry-point macros (with register-return)*/
#define K_SETGLABEL_IN_RR_HEADER \
	unsigned int return_address; \
	sched_t *sched; \
	word *Wptr;

#define K_SETGLABEL_THREE_IN_RR(X,A,B,C) \
	K_SETGLABEL_IN_RR_HEADER \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=d" (return_address), "=c" (C), "=b" (B), "=a" (A) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_TWO_IN_RR(X,A,B) \
	K_SETGLABEL_IN_RR_HEADER \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=c" (return_address), "=b" (B), "=a" (A) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_ONE_IN_RR(X,A) \
	K_SETGLABEL_IN_RR_HEADER \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=b" (return_address), "=a" (A) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_ZERO_IN_RR(X) \
	K_SETGLABEL_IN_RR_HEADER \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=a" (return_address) \
		: /* no inputs */ \
		: "memory", "cc")
/*}}}*/
/*{{{  incoming entry-point macros (with stack-params and load-call-return)*/
#define K_SETGLABEL_IN_STK_HEADER \
	unsigned int return_address; \
	sched_t *sched; \
	word *Wptr; \

#define K_SETGLABEL_STKFIVE_IN_LCR(X,A,B,C,D,E) \
	K_SETGLABEL_IN_STK_HEADER \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	subl    $8, %%esp	      	\n" \
		"	movl	%%ebp, 0(%%esp)		\n" \
		"	movl	%%esi, 4(%%esp)		\n" \
		"	movl	%%esp, %%ebp		\n" \
		"	addl 	$8, %%ebp		\n" \
		"	movl	0(%%ebp), %2		\n" \
		"	movl	4(%%ebp), %3		\n" \
		"	movl	8(%%ebp), %4		\n" \
		"	movl	12(%%ebp), %5		\n" \
		"	movl	16(%%ebp), %6		\n" \
		"	movl	20(%%ebp), %7		\n" \
		"	addl	$24, %%ebp		\n" \
		"	popl	0(%%ebp)		\n" \
		"	popl	4(%%ebp)		\n" \
		"	movl 	%%ebp, %%esp		\n" \
		"	movl	4(%%esp), %%ebp		\n" \
		"	movl	%%ebp, %1		\n" \
		"	movl	0(%%esp), %%ebp		\n" \
		"	movl	%%ebp, %0		\n" \
		: "=m" (Wptr), "=m" (sched), "=D" (return_address), \
		  "=q" (A), "=q" (B), "=q" (C), "=q" (D), "=S" (E) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_STKFOUR_IN_LCR(X,A,B,C,D) \
	K_SETGLABEL_IN_STK_HEADER \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	popl	%2	#; return-addr	\n" \
		"	popl	%3			\n" \
		"	popl	%4			\n" \
		"	popl	%5			\n" \
		"	popl	%6			\n" \
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=D" (return_address), \
		  "=q" (A), "=q" (B), "=q" (C), "=q" (D) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_STKTHREE_IN_LCR(X,A,B,C) \
	K_SETGLABEL_IN_STK_HEADER \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	popl	%2	#; return-addr	\n" \
		"	popl	%3			\n" \
		"	popl	%4			\n" \
		"	popl	%5			\n" \
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=q" (return_address), "=q" (A), "=q" (B), "=q" (C) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_STKTWO_IN_LCR(X,A,B) \
	K_SETGLABEL_IN_STK_HEADER \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	popl	%2	#; return-addr	\n" \
		"	popl	%3			\n" \
		"	popl	%4			\n" \
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=q" (return_address), "=q" (A), "=q" (B) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETGLABEL_STKONE_IN_LCR(X,A) \
	K_SETGLABEL_IN_STK_HEADER \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	popl	%2	#; return-addr	\n" \
		"	popl	%3			\n" \
		"	movl	%%ebp, %0		\n" \
		: "=g" (Wptr), "=S" (sched), "=q" (return_address), "=q" (A) \
		: /* no inputs */ \
		: "memory", "cc")

/*}}}*/
/*{{{  outgoing entry-point macros*/
#define K_ZERO_OUT() \
	TRACE_RETURN (return_address); \
	__asm__ __volatile__ ("\n" \
		"	movl	%3, %%esp		\n" \
		"	movl	%0, %%ebp		\n" \
		"	jmp	*%2			\n" \
		: /* no outputs */ \
		: "r" (Wptr), "S" (sched), "q" (return_address), "g" (sched->stack) \
		: "memory")

#define K_ONE_OUT(A) \
	TRACE_RETURN (return_address); \
	__asm__ __volatile__ ("\n" \
		"	movl	%3, %%esp		\n" \
		"	movl	%0, %%ebp		\n" \
		"	jmp	*%2			\n" \
		: /* no outputs */ \
		: "r" (Wptr), "S" (sched), "q" (return_address), "g" (sched->stack), "a" (A) \
		: "memory")

#define K_STKONE_OUT(A) \
	TRACE_RETURN (return_address); \
	__asm__ __volatile__ ("				\n" \
		"	movl	%3, %%esp		\n" \
		"	movl	%0, %%ebp		\n" \
		"	pushl	%4			\n" \
		"	jmp	*%2			\n" \
		: /* no outputs */  \
		: "r" (Wptr), "S" (sched), "q" (return_address), "g" (sched->stack), "q" (A) \
		: "memory")

#define K_STKTHREE_OUT(A,B,C) \
	TRACE_RETURN (return_address); \
	__asm__ __volatile__ ("				\n" \
		"	movl	%3, %%esp		\n" \
		"	pushl	%6			\n" \
		"	pushl	%5			\n" \
		"	pushl	%4			\n" \
		"	movl	%0, %%ebp		\n" \
		"	jmp	*%2			\n" \
		: /* no outputs */ \
		: "r" (Wptr), "S" (sched), "q" (return_address), "g" (sched->stack), \
		  "r" (A), "r" (B), "r" (C) \
		: "memory")
/*}}}*/

/*{{{  jump through entry-point macros*/
#define K_JUMP_THROUGH_TWO_I1(X,Y,C) \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	movl %%ebx, %%ecx		\n" \
		"	movl %%eax, %%ebx		\n" \
		"	movl %0, %%eax			\n" \
		"	jmp "#Y"			\n" \
		: /* no outputs */ \
		: "i" (C) \
		: "eax", "ebx", "ecx", "esi", "memory", "cc")
/*}}}*/

/*
 * scheduler label functions
 */
#define K_ENTRY(init, stack, Wptr, Fptr) \
	__asm__ __volatile__ ("				\n" \
		"	movl %0, %%ebp			\n" \
		"       movl %3, %%esp          	\n" \
		"	subl $64, %%esp			\n" \
		"	andl $0xffffffe0, %%esp		\n" \
		"	subl $768, %%esp		\n" \
		"	jmp *%2				\n" \
		: /* no outputs */ \
		: "g" (Wptr), "S" (Fptr), "D" (init), "q" (stack) \
		: "memory")
#define K_LOADLABADDR(X,V) \
	__asm__ __volatile__ ("		\n" \
		"	leal "#X", %0	\n" \
		: "=r" (V) : : "eax")
#define K_JUMP(X) \
	__asm__ __volatile__ ("		\n" \
		"	jmp "#X"	\n" \
		: : : "memory")
#define K_JUMP_THREE(X,A,B,C) \
	__asm__ __volatile__ ("\n" \
		"	movl	%0, %%ebp		\n" \
		"	jmp	"#X"			\n" \
		: /* no outputs */ \
		: "r" (Wptr), "S" (sched), "a" (A), "b" (B), "c" (C) \
		: "memory")
#define K_THREADINIT(X) \
	__asm__ __volatile__ ("				\n" \
		_K_SETGLABEL (X)			\
		"	movl	%%ebp, %0		\n" \
		"	movl    %%esp, %2		\n" \
		: "=g" (Wptr), "=S" (fptr), "=g" (stack) \
		: /* no inputs */ \
		: "memory", "cc")
#define K_SETLABEL(X) \
	__asm__ __volatile__ ("		\n"	\
		".align 16		\n"	\
		"	"#X":		\n"	\
		: : : "memory", "cc")
#define K_SETGLABEL(X) \
	__asm__ __volatile__ ("		\n"	\
		_K_SETGLABEL (X)		\
		: : : "memory", "cc")
#define K_SCHEDULER(X) \
	__asm__ __volatile__ ("		\n"	\
		_K_SETGLABEL (X)		\
		: "=S" (sched) 			\
		: /* no inputs */		\
		: "memory", "cc")

#ifdef DYNAMIC_PROCS
#define KERNEL_RUN_CODE(Wptr, Fptr, Bptr, EP) \
	__asm__ __volatile__ ("			\n" \
		"       movl    %0, %%ebp	\n" \
		"       movl    $0f, 0(%%ebp)	\n" \
		"       jmp     *%2		\n" \
		"0:     jmp     _X_dynproc_exit	\n" \
		: /* no outputs */ \
		: "g" (Wptr), "S" (sched), "q" (EP) \
		: "memory", "cc")
#endif	/* DYNAMIC_PROCS */

/*
 * CIF helpers
 */
#define K_CIF_BCALLN(func, argc, argv, ret) \
	__asm__ __volatile__ ("				\n" \
		"	movl	%%esp, %%ebx		\n" \
		"	subl	%%ecx, %%esp		\n" \
		"	andl	$-16, %%esp		\n" \
		"	movl	%%esp, %%edi		\n" \
		"	cld				\n" \
		"	rep				\n" \
		"	movsb				\n" \
		"	call	*%%eax			\n" \
		"	movl	%%ebx, %%esp		\n" \
		: "=a" (ret) \
		: "0" (func), "c" (argc << WSH), "S" (argv) \
		: "cc", "memory", "ebx", "edx", "edi")
#define K_CIF_ENDP_STUB(X) \
	__asm__ __volatile__ ("				\n" \
		_K_SETGGLABEL (X)			\
		"	movl	-28(%%ebp), %%ebp	\n" \
		"	movl	-4(%%ebp), %%eax	\n" \
		"	jmp	*%%eax			\n" \
		: /* no outputs */ \
		: /* no inputs */ \
		: "memory")
#define K_CIF_PROC_STUB(X, wptr) \
	__asm__ __volatile__ ("				\n" \
		_K_SETGGLABEL (X)			\
		"	movl	(%%ebp), %%esp		\n" \
		"	popl	%%eax			\n" \
		"	call	*%%eax			\n" \
		"	movl	(%%esp), %0		\n" \
		"	subl	$32, %%esp		\n" \
		: "=r" (wptr) \
		: /* no inputs */ \
		: "cc", "memory", "eax", "ebx", "ecx", "edx", "esi", "edi")
#define K_CIF_SCHED_CALL(sched, stack, call, wptr) \
	__asm__ __volatile__ ("				\n" \
		"	movl	%0, %%esp		\n" \
		"	jmp	*%1			\n" \
		: /* no outputs */ \
		:  "r" (stack), "r" (call), "S" (sched), "a" (wptr) \
		: "cc", "memory")

#endif /* sched_asm_inserts.h */

