/*
 *	MIPS 3 specific assembler bits
 *	Copyright (C) 2002 Fred Barnes  <frmb2@ukc.ac.uk>
 *	Based on similar file for i386:
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

/* note: removed all OOS stuff in the transition -- not for MIPS! */

#ifndef SCHED_ASM_INSERTS_H
#define SCHED_ASM_INSERTS_H

/* these for debugging mostly */
#define LOAD_ESP(X) \
	__asm__ __volatile__ ("\n\tmove	%0,$sp\n\t" : "=r" (X))

#define STACK_CHK_ENTRY(callname) \
	LOAD_ESP (the_stackptr)
#define STACK_CHK_LEAVE(callname) \
	LOAD_ESP (the_stackptr2); \
	if (the_stackptr2 != the_stackptr) \
		fprintf (stderr, "CCSP: serious: stack deformation in %s: 0x%8.8x --> 0x%8.8x (%d words)\n", callname, the_stackptr, the_stackptr2, (int)(the_stackptr - the_stackptr2))

/* Load the Workspace pointer register into a variable.
 */
#define LOADWPTR \
  __asm__ __volatile__ ("		\n" \
		" # LOADWPTR		\n" \
		"	sw	$8, %0\n" \
		"	sw	$9, %1\n" \
		"	sw	$10, %2\n" \
                : "=m" (Wptr), "=m" (Fptr), "=m" (Bptr) \
                )
#define LOADQPTR \
	__asm__ __volatile__ ("		\n" \
		" # LOADQPTR		\n" \
		"	sw	$9, %0		\n" \
		"	sw	$10, %1		\n" \
		: "=m" (Fptr), "=m" (Bptr))

#define LOADWPTRRET \
	__asm__ __volatile__ ("		\n" \
		" # LOADWPTRRET		\n" \
		"	sw	$8, %0\n" \
		"	sw	$9, %1\n" \
		"	sw	$10, %2\n" \
		"	sw	$31, -4($8)\n" \
		: "=m" (Wptr), "=m" (Fptr), "=m" (Bptr))


#define SAVEWPTR \
  __asm__ __volatile__ ("		\n" \
		  " # SAVEWPTR		\n" \
		  "	lw	$8, %0\n" \
  			: /* no outputs */ \
  			: "g" (Wptr))

/* Restore the workspace pointer and kernel entry table pointers to their
 * values before this function call.  This should be combined with a EJUMP
 * really, because it always happens.  Note that the jump table pointer is
 * not necessary in C.
 */
#define RESTORE \
	__asm__ __volatile__ ( \
		" # RESTORE			\n" \
		"	lw	$8, %0		\n" \
		"	lw	$9, %1		\n" \
		"	lw	$10, %2		\n" \
		: /* no outputs */ \
		: "m" (Wptr), "m" (Fptr), "m" (Bptr) \
		: "memory", "cc")

#define RESTOREJRET \
	__asm__ __volatile__ ( \
		" # RESTOREJRET			\n" \
		"	lw	$8, %0		\n" \
		"	lw	$9, %1		\n" \
		"	lw	$10, %2		\n" \
		"	lw	$31, -4($8)	\n" \
		"	j	$31		\n" \
		: /* no outputs */ \
		: "m" (Wptr), "m" (Fptr), "m" (Bptr) \
		: "memory", "cc")

#define RESTOREIRET \
	__asm__ __volatile__ ( \
		" # RESTOREIRET			\n" \
		"	lw	$8, %0		\n" \
		"	lw	$9, %1		\n" \
		"	lw	$10, %2		\n" \
		"	lw	$31, 0($8)	\n" \
		"	j	$31		\n" \
		: /* no outputs */ \
		: "m" (Wptr), "m" (Fptr), "m" (Bptr) \
		: "memory", "cc")

/* Load the parameter value passed to the kernel into a variable.  
 */ 
#define PARAM(A) \
  __asm__ __volatile__ (" # PARAM			\n" \
		  	"	lw	$2, 0($sp)	\n" \
		  	"	addu	$sp, $sp, 4	\n" \
		  	"	sw	$2, %0" \
                        : "=g" (A) \
                       )
/* 
 * Load the two parameter values passed to the kernel into variables.  
 */ 
#define TWOPARAMS(A,B) \
  __asm__ __volatile__ (" # TWOPARAMS			\n" \
		  	"	lw	$2, 0($sp)	\n" \
		  	"	lw	$3, 4($sp)	\n" \
			"	addu	$sp, $sp, 8	\n" \
			"	sw	$2, %1		\n" \
			"	sw	$3, %0		\n" \
                        : "=g" (A), "=g" (B) \
                       )

/* Load the three parameter values passed to the kernel into variables.  
 */ 
#define THREEPARAMS(A,B,C) \
  __asm__ __volatile__ (" # THREEPARAMS			\n" \
		  	"	lw	$2, 0($sp)	\n" \
		  	"	lw	$3, 4($sp)	\n" \
			"	sw	$2, %2		\n" \
			"	sw	$3, %1		\n" \
			"	lw	$2, 8($sp)	\n" \
			"	addu	$sp, $sp, 12	\n" \
			"	sw	$2, %0		\n" \
                        : "=g" (A), "=g" (B), "=g" (C) \
                       )

/* Load the four parameter values passed to the kernel into variables.  
 * this is in fact used to pass the time as a 64bit value, in two 32bit
 * chunks.
 */ 
 /* Mainly used for 64-bit quantities, but I use it for occam debugging. [fred] */
#define FOURPARAMS(A,B,C,D) \
  __asm__ __volatile__ (" # FOURPARAMS			\n" \
		  	"	lw	$2, 0($sp)	\n" \
		  	"	lw	$3, 4($sp)	\n" \
			"	sw	$2, %3		\n" \
			"	sw	$3, %2		\n" \
			"	lw	$2, 8($sp)	\n" \
			"	lw	$3, 12($sp)	\n" \
			"	addu	$sp, $sp, 16	\n" \
			"	sw	$2, %1		\n" \
			"	sw	$3, %0		\n" \
                        : "=g" (A), "=g" (B), "=g" (C), "=g" (D) \
                       )

/* didn't want to trample on a register for floating-point errors.. */
#define FIVEPARAMS(A,B,C,D,E) \
  __asm__ __volatile__ (" # FIVEPARAMS			\n" \
		  	"	lw	$2, 0($sp)	\n" \
		  	"	lw	$3, 4($sp)	\n" \
			"	sw	$2, %4		\n" \
			"	sw	$3, %3		\n" \
			"	lw	$2, 8($sp)	\n" \
			"	lw	$3, 12($sp)	\n" \
			"	sw	$2, %2		\n" \
			"	sw	$3, %1		\n" \
			"	lw	$2, 16($sp)	\n" \
			"	addu	$sp, $sp, 20	\n" \
			"	sw	$2, %0		\n" \
  			: "=g" (A), "=g" (B), "=g" (C), "=g" (D), "=g" (E) \
  			)

/* experimental kernel interface stuff
 */

#define K_SETGLABEL_THREE_IN(X,A,B,C) \
	__asm__ __volatile__ ("				\n" \
		" # SETGLABEL_THREE_IN			\n" \
		".globl _"#X"				\n" \
		"	_"#X":				\n" \
		"	"#X":				\n" \
		"	sw	$8, %0			\n" \
		"	sw	$9, %1			\n" \
		"	sw	$10, %2			\n" \
		"	sw	$6, %3			\n" \
		"	sw	$5, %4			\n" \
		"	sw	$4, %5			\n" \
		: "=m" (Wptr), "=m" (Fptr), "=m" (Bptr), "=m" (C), "=m" (B), "=m" (A))
#define K_SETGLABEL_TWO_IN(X,A,B) \
	__asm__ __volatile__ ("				\n" \
		" # SETGLABEL_TWO_IN			\n" \
		".globl _"#X"				\n" \
		"	_"#X":				\n" \
		"	"#X":				\n" \
		"	sw	$8, %0			\n" \
		"	sw	$9, %1			\n" \
		"	sw	$10, %2			\n" \
		"	sw	$5, %3			\n" \
		"	sw	$4, %4			\n" \
		: "=m" (Wptr), "=m" (Fptr), "=m" (Bptr), "=m" (B), "=m" (A))
#define K_SETGLABEL_ONE_IN(X,A) \
	__asm__ __volatile__ ("				\n" \
		" # SETGLABEL_ONE_IN			\n" \
		".globl _"#X"				\n" \
		"	_"#X":				\n" \
		"	"#X":				\n" \
		"	sw	$8, %0			\n" \
		"	sw	$9, %1			\n" \
		"	sw	$10, %2			\n" \
		"	sw	$4, %3			\n" \
		: "=m" (Wptr), "=m" (Fptr), "=m" (Bptr), "=m" (A))
#define K_SETGLABEL_ZERO_IN(X) \
	__asm__ __volatile__ ("				\n" \
		" # SETGLABEL_ZERO_IN			\n" \
		".globl _"#X"				\n" \
		"	_"#X":				\n" \
		"	"#X":				\n" \
		"	sw	$8, %0			\n" \
		"	sw	$9, %1			\n" \
		"	sw	$10, %2			\n" \
		: "=m" (Wptr), "=m" (Fptr),"=m" (Bptr))

#define ONE_PARAM_IN(A) \
	__asm__ __volatile__ ( \
		"\n" \
		: "=$4" (A))
#define TWO_PARAM_IN(A,B) \
	__asm__ __volatile__ ( \
		"\n" \
		: "=$4" (A), "=$5" (B))
#define THREE_PARAM_IN(A,B,C) \
	__asm__ __volatile__ ( \
		"\n" \
		: "=$4" (A), "=$5" (B), "=$6" (C));
#define FOUR_PARAM_IN(A,B,C,D) \
	__asm__ __volatile__ ( \
		"\n" \
		: "=$4" (A), "=$5" (B), "=$6" (C), "=$7" (D));
#define ONE_PARAM_OUT(A) \
	__asm__ __volatile__ ( \
		"\n" \
		: : "$4" (A));
#define TWO_PARAM_OUT(A,B) \
	__asm__ __volatile__ ( \
		"\n" \
		: : "$4" (A), "$5" (B));
#define THREE_PARAM_OUT(A,B,C) \
	__asm__ __volatile__ ( \
		"\n" \
		: : "$4" (A), "$5" (B), "$6" (C));


/* loads the return address on entry to the kernel function into a global 
 * variable called 'return_address'.  This could be paramterised, but we
 * never use any other variable, so this seemed pointless.
 */
#define LOADRETURN \
  __asm__ __volatile__ (" # LOADRETURN \n" \
		  	"	sw	$31, %0		\n" \
                        : "=g" (return_address) \
                       )

/* Return a value from the current kernel function.  In tranpc this is done
 * by pushing on the system stack, but the convension is to use EAX on the
 * 386.  We use tranpc's convention for compatablilty with occam.
 */
#define RETURNVAL(X) \
  __asm__ __volatile__ ("			\n" \
		" # RETURNVAL			\n" \
		"	lw	$2, %0		\n" \
		"	subu	$sp, $sp, 4	\n" \
		"	sw	$2, 0($sp)	\n" \
		: /* no outputs */ \
		: "g" (X) \
		)


/*
 * scheduler label functions
 */
#define K_LOADLABADDR(X,V) \
	__asm__ __volatile__ ("		\n" \
		" # LOADLABADDR		\n" \
		"	la "#X", $8	\n" \
		"	sw $8, %0	\n" \
		: "=g" (V) : : "$8" \
		)
#define K_JUMP(X) \
	__asm__ __volatile__ ("\n" \
		" # K_JUMP	\n" \
		"	j "#X"\n" \
		)
#define K_SETLABEL(X) \
	__asm__ __volatile__ ("\n" \
		" # K_SETLABEL	\n" \
		"	"#X":\n" \
		)
#define K_SETGLABEL(X) \
	__asm__ __volatile__ ("\n" \
		" # K_SETGLABEL	\n" \
		".globl _"#X"\n" \
		"	_"#X":\n" \
		"	"#X":\n" \
		)


/* an EJUMP is a jump out of the kernel into user code, usually the return
   address 
   Note that only GCC supports goto *(X), may need to be replaced with an
   asm jump if porting. */
#define EJUMP(X) \
	__asm__ __volatile__ ("\n" \
		" # EJUMP			\n" \
		"	lw	$31, %0		\n" \
		"	j	$31		\n" \
		: /* no outputs */ : "g" (X))
/* #define EJUMP(X) goto *(X) */

#if 0

/* Restore the stack pointer before returning to C */
#define RESTORESP(X) \
  __asm__ __volatile__ ("movl   %0, %%esp\n" \
                        : /* no outputs */ \
                        : "g" (X) \
                       ) 

/* Some macros for stacking/unstacking the static context so that kernel
 * calls can be called back from calls out to the communications subsystem
 * (is this 're-entrancy'?).  The current use of these assumes that they 
 * have LIFO stacking semantics.
 */
#define SAVESTATIC(X) \
    __asm__ __volatile__ ("pushl  %0\n" \
                          : /* no ouputs */ \
                          : "q" (X) \
                         );

#define RESTORESTATIC(X) \
    __asm__ __volatile__ ("popl  %0\n" \
                          : "=q" (X) \
                          : /* no inputs */ \
                         )
#define SAVESTATICS \
/*  __asm__ __volatile__ ("pushl  %0\n" \
                        "pushl  %1\n" \
                        : \
                        : "q" (Wptr), "q" (return_address) \
                        : "eax", "ebx" \
                       )*/

#define RESTORESTATICS \
/*  __asm__ __volatile__ ("popl %0\n" \
                        "popl %1\n" \
                        : "=q" (return_address), "=q" (Wptr) \
                        : \
                        : "eax", "ebx" \
                       )*/

#endif

/* MACRO LBL(A) -   declared a kernel function's label on two levels: 
 *                  the C level, as in
 *                    label: 
 *                  and as a global asm label so that functions can be
 *                  called without going through the jump table.  
 *                    asm(".globl label\n label:");
 *
 * IMPLEMENTATION NOTE:
 * this is really wierd - we rely here on ANSI C string concatenation: 
 * "Jim" "John" "Jan" == "JimJohnJan" therefore if B is a string we get 
 * ".globl" "<label>" concating to ".globl <label>" followed by 
 * "<label>" ":" concatting to "<label>:".  Items that appear quoted
 * are actually outside the quotes, this is the confusing thing.
 * I also use 'stringification' as documented in the GNU cpp 
 * documentation.  If '#define A foo' then '#A' is '"foo"'.  This is
 * needed to insert the name into the asm block.  Before I new how to
 * do this, this macro took two paramters, one 'stringified' and one
 * not.  This is nicer. 
 */ 

#define MEMCPY(SRC,DEST,COUNT) \
	memcpy(DEST,SRC,COUNT)
#if 0
  __asm__ __volatile__ ( \
    "cld\n" \
    "rep\n" \
    "movsb\n" \
    : /* no outputs */ \
    : "S" (SRC), "D" (DEST), "c" (COUNT) \
  )
#endif

#ifdef GSL_SMP
#define CLAIM(SEMA) \
  __asm__ __volatile__ ( \
    "   movl    $1, %%eax\n" \
    "1: xchg    %%eax, %0\n" \
    "   cmpl    $0, %%eax\n" \
    "   jne     1b\n" \
    : /* no outputs */ \
    : "m" (SEMA) \
    : "eax" \
  )

#define RELEASE(SEMA) \
  __asm__ __volatile__ ( \
    "   movl   $0, %0" \
    : /* no outputs */ \
    : "m" (SEMA) \
  )
#endif
#endif /* sched_asm_inserts.h */

#ifdef PROCESS_PRIORITY

#define ASM_BSF(V,R) \
	{ unsigned int temp = (V); (R) = 0; while (((R) < 32) && !(temp & 0x00000001)) { (R)++; temp >>= 1; } }
#if 0
	__asm__ __volatile__ ("         \n" \
	"       bsf     %1,%0   \n" \
	: "=r" (R) : "r" (V) : "cc")
#endif

#endif	/* PROCESS_PRIORITY */

#ifdef DYNAMIC_PROCS

#define KERNEL_RUN_CODE(WP,EP) \
	__asm__ __volatile__ ("			\n" \
		" # KERNEL_RUN_CODE		\n" \
		"	move	$8, %0		\n" \
		"	lw	$9, %2		\n" \
		"	lw	$10, %3		\n" \
		"	la	$2, 0f		\n" \
		"	sw	$2, 0($8)	\n" \
		"	lw	$2, %1		\n" \
		"	j	$2		\n" \
		"0:	j	_X_dynproc_exit	\n" \
		: /* no outputs */ : "g" (WP), "g" (EP), "g" (Fptr), "g" (Bptr) : "memory", "cc")


#endif	/* DYNAMIC_PROCS */

