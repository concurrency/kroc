/*
 *	IA32 Architecture (GCC specific) Inserts
 *	Copyright (C) 2008  Carl Ritson <cgr@kent.ac.uk>
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

#ifndef I386_SCHED_ASM_INSERTS_H
#define I386_SCHED_ASM_INSERTS_H

/*{{{  architecture dependent kernel call declarations */
#define _K_CALL_DEFINE(X) \
	static void __attribute__ ((regparm(3))) kernel_##X (word param0, sched_t *sched, word *Wptr)
#define _K_CALL_DEFINE_O(X) \
	static word __attribute__ ((regparm(3))) kernel_##X (word param0, sched_t *sched, word *Wptr)
#define K_CALL_DEFINE_0_0(X) _K_CALL_DEFINE(X)
#define K_CALL_DEFINE_1_0(X) _K_CALL_DEFINE(X)
#define K_CALL_DEFINE_2_0(X) _K_CALL_DEFINE(X)
#define K_CALL_DEFINE_3_0(X) _K_CALL_DEFINE(X)
#define K_CALL_DEFINE_4_0(X) _K_CALL_DEFINE(X)
#define K_CALL_DEFINE_5_0(X) _K_CALL_DEFINE(X)
#define K_CALL_DEFINE_0_1(X) _K_CALL_DEFINE_O(X)
#define K_CALL_DEFINE_1_1(X) _K_CALL_DEFINE_O(X)
#define K_CALL_DEFINE_2_1(X) _K_CALL_DEFINE_O(X)
#define K_CALL_DEFINE_3_1(X) _K_CALL_DEFINE_O(X)
#define K_CALL_DEFINE_2_3(X) _K_CALL_DEFINE_O(X)
#define K_CALL_DEFINE_3_3(X) _K_CALL_DEFINE_O(X)

#define K_CALL_PTR(X) \
	((void *) (kernel_##X))

#define K_CALL_HEADER \
	__attribute__ ((unused)) \
	unsigned int return_address = (unsigned int) __builtin_return_address (0);
#define K_CALL_PARAM(N) \
	((N) == 0 ? param0 : sched->cparam[(N) - 1])
/*}}}*/

/*{{{  debugging support */
#define LOAD_ESP(X) \
	__asm__ __volatile__ ("		\n"	\
		"	movl %%esp, %0	\n" 	\
		: "=g" (X)			\
	)
#define SAVE_EIP(X) \
	__asm__ __volatile__ ("		\n"	\
		"	call $0f	\n"	\
		"0:	popl %0		\n"	\
		: "=g" (X)			\
	)
/*}}}*/

/*{{{  tracing support */
#ifdef CHECKING_MODE
#define TRACE_RETURN(addr)	\
	do { 						\
		sched->mdparam[8] = (word) Wptr;	\
		sched->mdparam[9] = (word) (addr);	\
		SAVE_EIP (sched->mdparam[11]);		\
	} while (0)
#else
#define	TRACE_RETURN(addr)	do { } while (0)
#endif /* CHECKING_MODE */
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

/*{{{  outgoing entry-point macros*/
#define _SET_RETURN_ADDRESS(R) \
	((word *) sched->stack)[-1] = (word) (R)

#define K_ZERO_OUT() \
	return
#define K_ZERO_OUT_JUMP(R) \
	do { \
		_SET_RETURN_ADDRESS (R);\
		K_ZERO_OUT ();		\
	} while (0)
#define K_ZERO_OUT_JRET() \
	do { \
		TRACE_RETURN (Wptr[Iptr]); \
		__asm__ __volatile__ ("			\n" \
			"	movl	%0, %%ebp	\n" \
			"	movl	(%%esi), %%esp	\n" \
			"	jmp	*-4(%%ebp)	\n" \
			: /* no outputs */ \
			: "g" (Wptr), "S" (sched) \
			: "memory"); \
	} while (0)

#define K_ONE_OUT(A) \
	return ((word) (A))
#define K_ONE_OUT_JUMP(R,A) \
	do { \
		_SET_RETURN_ADDRESS (R);\
		K_ONE_OUT (A);		\
	} while (0)

#define K_TWO_OUT(A,B) \
	do { \
		sched->cparam[0] = (word) (B);	\
		K_ONE_OUT (A);			\
	} while (0)

#define K_THREE_OUT(A,B,C) \
	do { \
		sched->cparam[0] = (word) (B); 	\
		sched->cparam[1] = (word) (C); 	\
		K_ONE_OUT (A);			\
	} while (0)
/*}}}*/

/*{{{  entry and label functions */
#define K_ENTRY(init,stack,Wptr,Fptr) \
	__asm__ __volatile__ ("				\n" \
		"	movl %0, %%esp			\n" \
		"	call *%3			\n" \
		: /* no outputs */ \
		: "a" (stack), "c" (Wptr), "d" (Fptr), "r" (init) \
		: "memory")
/*}}}*/

/*{{{  CIF helpers */
#define K_CIF_BCALLN(func, argc, argv, ret) \
	__asm__ __volatile__ ("				\n" \
		"	pushl	%%ebp			\n" \
		"	movl	%%esp, %%ebp		\n" \
		"	subl	%%ecx, %%esp		\n" \
		"	andl	$-16, %%esp		\n" \
		"	movl	%%esp, %%edi		\n" \
		"	cld				\n" \
		"	rep				\n" \
		"	movsb				\n" \
		"	call	*%%eax			\n" \
		"	movl	%%ebp, %%esp		\n" \
		"	popl	%%ebp			\n" \
		: "=a" (ret) \
		: "0" (func), "c" (argc << WSH), "S" (argv) \
		: "cc", "memory", "edx", "edi")
#define K_CIF_ENDP_RESUME(address) \
	__asm__ __volatile__ ("				\n" \
		"	call	0f			\n" \
		"	movl	-16(%%ebp), %%ebp	\n" \
		"	jmp	*-4(%%ebp)		\n" \
		"0:	popl	%0			\n" \
		: "=g" (address) \
		: /* no inputs */ \
		: "memory")
#define _K_CIF_PROC \
		"	movl	(%%ebp), %%esp		\n" \
		"	movl	%%esi, -28(%%ebp)	\n" \
		"	popl	%%eax			\n" \
		"	call	*%%eax			\n" \
		"	movl	-28(%%ebp), %%edx	\n" \
		"	movl	%%ebp, %%ecx		\n" \
		"	movl	%%edx, %%esi		\n" \
		"	movl	%%ecx, %%eax		\n" \
		"	movl	(%%edx), %%esp		\n" \
		"	addl	%2, %%eax		\n" \
		"	addl	%1, %%esi		\n"
#define K_CIF_PROC(address, call, offset) \
	__asm__ __volatile__ ("				\n" \
		"	call	0f			\n" \
		_K_CIF_PROC \
		"	call	*(%%esi)		\n" \
		"0:	popl	%0			\n" \
		: "=g" (address) \
		: "i" (offsetof(sched_t, calltable[call])), "i" (offset * sizeof(word)) \
		: "memory")
#define K_CIF_PROC_IND(address, call, offset) \
	__asm__ __volatile__ ("				\n" \
		"	call	0f			\n" \
		_K_CIF_PROC \
		"	movl	(%%eax), %%eax		\n" \
		"	call	*(%%esi)		\n" \
		"0:	popl	%0			\n" \
		: "=g" (address) \
		: "i" (offsetof(sched_t, calltable[call])), "i" (offset * sizeof(word)) \
		: "memory")
/*}}}*/

#endif /* I386_SCHED_ASM_INSERTS */

