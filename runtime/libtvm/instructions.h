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
#define TVM_HELPER \
	static TVM_INLINE
#define TVM_HELPER_PROTO \
	static TVM_INLINE
#define TVM_INSTRUCTION(X) \
	static TVM_INLINE int X (ECTX ectx)
#define TVM_INSTRUCTION_PROTO(X) \
	static TVM_INLINE int X (ECTX ectx)
#else
#define TVM_HELPER
#define TVM_HELPER_PROTO \
	extern
#define TVM_INSTRUCTION(X) \
	int X (ECTX ectx)
#define TVM_INSTRUCTION_PROTO(X) \
	extern int X (ECTX ectx)
#endif

/* This instruction clears a register */
#define CLEAR(reg) \
	do { (reg) = 0; } while (0)

#ifdef TVM_TYPE_SHADOW
#define INTERNAL_STACK_TYPES(A,B,C) 		\
	do {					\
		WORD at_tmp = (WORD) (A); 	\
		WORD bt_tmp = (WORD) (B); 	\
		WORD ct_tmp = (WORD) (C); 	\
		AREGt = at_tmp;			\
		BREGt = bt_tmp;			\
		CREGt = ct_tmp;			\
	} while (0)
#define PICK_POINTER_TYPE(A,B)			\
	(((A) == STYPE_MT || (B) == STYPE_MT)	\
	 ? STYPE_MOBILE				\
	 : (((A) == STYPE_DATA) ? (B) : (A)))
#else /* !TVM_TYPE_SHADOW */
#define INTERNAL_STACK_TYPES(A,B,C)		\
	do { } while (0)
#define PICK_POINTER_TYPE(A,B)
#endif /* !TVM_TYPE_SHADOW */
#define STACK(A,B,C,At,Bt,Ct) 			\
	do {					\
		WORD a_tmp = (WORD) (A); 	\
		WORD b_tmp = (WORD) (B); 	\
		WORD c_tmp = (WORD) (C); 	\
		AREG = a_tmp;			\
		BREG = b_tmp;			\
		CREG = c_tmp;			\
		INTERNAL_STACK_TYPES (At,Bt,Ct);\
	} while (0)
#define STACK2(A,B,At,Bt) 			\
	do {					\
		WORD a_tmp = (WORD) (A); 	\
		WORD b_tmp = (WORD) (B); 	\
		AREG = a_tmp;			\
		BREG = b_tmp;			\
		INTERNAL_STACK_TYPES (At,Bt,STYPE_UNDEF);\
	} while (0)
#define STACK1(A,At) 				\
	do {					\
		WORD a_tmp = (WORD) (A); 	\
		AREG = a_tmp;			\
		INTERNAL_STACK_TYPES (At,STYPE_UNDEF,STYPE_UNDEF);\
	} while (0)
#define STACK_RET(A,B,C,At,Bt,Ct) 		\
	do {					\
		STACK (A,B,C,At,Bt,Ct);		\
		return ECTX_CONTINUE;		\
	} while (0)
#define STACK2_RET(A,B,At,Bt)			\
	do {					\
		STACK2 (A,B,At,Bt);		\
		return ECTX_CONTINUE;		\
	} while (0)
#define STACK1_RET(A,At)			\
	do {					\
		STACK1 (A,At);			\
		return ECTX_CONTINUE;		\
	} while (0)


#define UNDEFINE_STACK()		\
	do {				\
		SET_AREGt (STYPE_UNDEF);\
		SET_BREGt (STYPE_UNDEF);\
		SET_CREGt (STYPE_UNDEF);\
	} while (0)
#define UNDEFINE_STACK_RET()		\
	do {				\
		UNDEFINE_STACK();	\
		return ECTX_CONTINUE;	\
	} while (0)

#define SET_ERROR_FLAG(F) \
	do {							\
		int ret = ectx->set_error_flag (ectx, (F)); 	\
		if (ret) {					\
			return ret;				\
		}						\
	} while (0)
#define SET_ERROR_FLAG_RET(F) \
	do { return ectx->set_error_flag (ectx, (F)); } while (0)

TVM_INSTRUCTION_PROTO (ins_not_implemented);

#endif /* INSTRUCTIONS_H */
