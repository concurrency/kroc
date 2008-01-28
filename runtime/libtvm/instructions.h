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
#define TVM_INSTRUCTION(X) \
	static inline int X (tvm_ectx_t *ectx)
#define TVM_INSTRUCTION_PROTO(X) \
	static inline int X (tvm_ectx_t *ectx)
#else
#define TVM_HELPER
#define TVM_HELPER_PROTO extern
#define TVM_INSTRUCTION(X) \
	int X (tvm_ectx_t *ectx)
#define TVM_INSTRUCTION_PROTO(X) \
	extern int X (tvm_ectx_t *ectx)
#endif

/* This instruction clears a register */
#define CLEAR(reg) \
	do { (reg) = 0; } while (0)
/* This instruction makes a register undefined */
#define UNDEFINE(reg) (reg)

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
#define STACK(A,B,C) 				\
	do {					\
		WORD a_tmp = (WORD) (A); 	\
		WORD b_tmp = (WORD) (B); 	\
		WORD c_tmp = (WORD) (C); 	\
		AREG = a_tmp;			\
		BREG = b_tmp;			\
		CREG = c_tmp;			\
	} while (0)
#define STACK_RET(A,B,C) 			\
	do {					\
		STACK(A,B,C);			\
		return ECTX_INS_OK;		\
	} while (0)

/* Push all registers down by one, essentially leaves 'areg' undefined */ 
#define PUSH_STACK() \
	STACK(UNDEFINE(AREG), AREG, BREG)
/* Pop all registers up by one, essentially leaves 'creg' undefined */
#define POP_STACK() \
	STACK(BREG, CREG, UNDEFINE(CREG))
/* Pop all registers up by two, leaves 'breg' and 'creg' undefined */
#define POP_STACK2() \
	STACK(creg, UNDEFINE(breg), UNDEFINE(creg))

#define UNDEFINE_STACK()
#define UNDEFINE_STACK_RET() \
	return ECTX_INS_OK;

TVM_INSTRUCTION_PROTO (ins_not_implemented);

#ifdef DEBUG_ERRORS
void debug_error_flag(char *file, int line, int error_num);
#define set_error_flag(n) \
	debug_error_flag(__FILE__, __LINE__, (n))
#else /* !DEBUG_ERRORS */
void set_error_flag(int error_num);
#endif /* !DEBUG_ERRORS */

void clear_error_flag(void);

#endif /* INSTRUCTIONS_H */
