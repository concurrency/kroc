/*
tvm - instructions.h
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2004-2008 Christian L. Jacobsen, Matthew C. Jadud

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef INSTRUCTIONS_H
#define INSTRUCTIONS_H

#include "transputer.h"

#ifdef TVM_DISPATCH_SWITCH
#define TVM_HELPER static inline
#define TVM_HELPER_PROTO static inline
#define TVM_INSTRUCTION static inline
#define TVM_INSTRUCTION_PROTO static inline
#else
#define TVM_HELPER
#define TVM_HELPER_PROTO extern
#define TVM_INSTRUCTION
#define TVM_INSTRUCTION_PROTO extern
#endif

/* This instruction MAY timeslice */
#define TIMESLICE()
/* This instruction MAY reschedule */
#define RESCHEDULE()
/* This instruction clears a register */
#define CLEAR(reg) \
	do { (reg) = 0; } while (0)
/* This instruction makes a register undefined */
#define UNDEFINE(reg) (reg)
/* Increment the instruction pointer */
#define NEXTINS() ++iptr

/* This macro assumes a decent C compiler (i.e. gcc) 
 * which removes things like "y = x; x = y".
 * 
 * Furthermore, we expect to see code like
 * 
 * STACK(breg, creg, UNDEFINE(creg))
 * 
 * because this will evaluate to
 *
 * STACK(breg, creg, creg)
 * 
 * which will then expand to the parrallel
 * execution of
 *
 * areg = breg;
 * breg = creg;
 * creg = creg;
 *
 * which will then optimize out to
 *
 * areg = breg;
 * breg = creg;
 *
 * which is all we want.
 * */
#define STACK(AREG, BREG, CREG) 		\
	do {					\
		WORD atmp = (WORD) (AREG); 	\
		WORD btmp = (WORD) (BREG); 	\
		WORD ctmp = (WORD) (CREG); 	\
		areg = atmp;			\
		breg = btmp;			\
		creg = ctmp;			\
	} while (0)

/* Push all registers down by one, essentially leaves 'areg' undefined */ 
#define PUSH_STACK() \
	STACK(UNDEFINE(areg), areg, breg)
/* Pop all registers up by one, essentially leaves 'creg' undefined */
#define POP_STACK() \
	STACK(breg, creg, UNDEFINE(creg))
/* Pop all registers up by two, leaves 'breg' and 'creg' undefined */
#define POP_STACK2() \
	STACK(creg, UNDEFINE(breg), UNDEFINE(creg))
#define STACK_CLAIM(reg, amount) reg = reg - amount;
#define STACK_RELEASE(reg, amount) reg = reg + amount;

#define UNDEFINE_STACK() \
	/* STACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg)) */

/* The WORKSPACE macros (_SET and _GET). These are (roughly) equivalent to
 * the "workspace" and "workspace!" macros in soccam. */

/* First, the symbols for the various workspace locations */
#define WS_TOP       0
#define WS_IPTR      1 /* Valid when: the process has been descheduled */
#define WS_NEXT      2 /* Valid when: the process is on a scheduling list */
#define WS_CHAN      3 /* Valid when: the process is waiting for */
#define WS_ALT_STATE 3 /*             communication, or is executing an ALT */
#define WS_NEXT_T    4 /* Valid when: the process is on */
#define WS_ALT_T     4 /*             a timer list */
#define WS_TIMEOUT   5 /* Valid when: the process is on a timer list */

/* Now the actual macros */
#define WORKSPACE_GET(WPTR, LOC) \
	read_word(wordptr_minus((WPTR), (LOC)))
#define WORKSPACE_SET(WPTR, LOC, VAL) \
	write_word(wordptr_minus((WPTR), (LOC)), (VAL))

/* Error values */
enum {
	EFLAG_SETERR = 1,
	EFLAG_CSUB0,
	EFLAG_LADD,
	EFLAG_LDIV,
	EFLAG_REM,
	EFLAG_DIV,
	EFLAG_LSUB,
	EFLAG_CSNGL,
	EFLAG_CCNT1,
	EFLAG_CWORD,
	EFLAG_ADC,
	EFLAG_ADD,
	EFLAG_SUB,
	EFLAG_MUL,
	EFLAG_BAR,	/* Barrier underflow */
	EFLAG_FP,	/* Floating point error */
	EFLAG_ALT,
	EFLAG_CHAN,	/* Channel communication error */
	EFLAG_DMEM,	/* Memory allocator error */
	EFLAG_MT,	/* Mobile type error */
	EFLAG_PROC,	/* Process API error */
	EFLAG_SHUTDOWN	/* Erroneous shutdown */
};

extern void (*not_implemented)(void);
extern void (*invalid)(void);

extern void ins_not_implemented(void);
extern void ins_invalid(void);

#ifdef DEBUG_ERRORS
void debug_error_flag(char *file, int line, int error_num);
#define set_error_flag(n) \
	debug_error_flag(__FILE__, __LINE__, (n))
#else /* !DEBUG_ERRORS */
void set_error_flag(int error_num);
#endif /* !DEBUG_ERRORS */

void clear_error_flag(void);

#endif /* INSTRUCTIONS_H */
