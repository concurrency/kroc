/*
tvm - ins_pri.c
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

#include "tvm.h"
#include "instructions.h"
#include "interpreter.h"
#include "ins_pri.h"

#ifndef TVM_DISPATCH_SWITCH
#include "jumptbl_sec.h"
#ifdef TVM_OCCAM_PI
#include "jumptbl_ex_sec.h"
#else /* !TVM_OCCAM_PI */
#include "ins_pi.h"
#endif /* !TVM_OCCAM_PI */
#endif /* !TVM_DISPATCH_SWITCH */

/* 0x8_ - adc - add constant */

/** DESCRIPTION
 * This instruction adds a constant to the A register. 
 */

/** EXAMPLE
 * Add the constant 20 to a local variable at workspace offset 1
 *
 * ldl 1
 * adc 20
 * stl 1
 */

/** EXAMPLE
 * The previous code is somewhat idealised, as in reality any primary
 * instruction can only hold operand values from 0-15. Prefixing instructions
 * must be used to build up the actual operand in the operand register. The
 * code for the previous example would look like this with prefixing
 * instructions shown.
 *
 * ldl 1
 * pfix 16
 * adc 4
 * stl 1
 */

/** STATE
 * (AREG (in (str addend)) (out (+checked AREG OREG)))
 * (OREG (in (str constant)) (out 0))
 * (IPTR (out nextinst))
 * (errorflag (out (str set on overflow from addition)))
 */

TVM_INSTRUCTION (ins_adc)
{
	/* Add the operand register to AREG */
	WORD result = AREG + OREG;


	/* Check for overflow, from Hackers Delight p. 27 */
	if( ((UWORD) (((result) ^ AREG) & ((result) ^ OREG)) >> (WORD_BITS - 1)) )
	{
		SET_ERROR_FLAG(EFLAG_INTOV);
	}

	/* Areg gets result, Breg and Creg stay the same */
	AREG = result;
	SET_AREGt(STYPE_DATA);

	CLEAR(OREG);

	return ECTX_CONTINUE;
}

/* 0xB_ - ajw - adjust workspace */

/** DESCRIPTION
 * This instruction adjusts the workspace by the value supplied as its
 * argument. 
 *
 * - To claim workspace: use a negative argument
 * - to release workspace: use a positive argument
 *
 * Note: the workspace is analogous to the stack on other architectures
 */

/** EXAMPLE
 * Claim some workspace, perform some computation, release the workspace.
 *
 * ajw -5
 * ... do something
 * ajw 5
 */

/** STATE
 * (OREG (in (str offset)) (out 0))
 * (WPTR (out  (+wordsize WPTR OREG)))
 * (IPTR (out nextinst))
 */

TVM_INSTRUCTION (ins_ajw)
{
	#ifdef TVM_TYPE_SHADOW
	/* Release memory */
	if(OREG < 0) {
		fill_type_shadow(ectx, (BYTEPTR)WPTR, (-OREG) << WSH, STYPE_UNDEF);
	}
	#endif

	/* Add the value in the operand register to the workspace pointer. */
	WPTR = wordptr_plus(WPTR, OREG);

	CLEAR(OREG);

	return ECTX_CONTINUE;
}

/* 0x9_ - call - call */

/** DESCRIPTION
 * The call instruction is used to perform jumps which are going to return to
 * the back to the origin of the call.
 *
 * The call instruction allocates four locations in the workspace to hold the
 * A, B and C register and the Instruction pointer. Given that the call
 * instruction saves the evaluation stack into the workspace, up to three
 * arguments can be passed to the called procedure (without explicitly
 * storing them in the workspace). If more than three arguments are to be
 * passed, workspace locations must be allocated for these arguments, and their
 * values stored in the allocated locations.
 *
 * It is the instruction address of the instruction after the call which is
 * stored into the workspace by the call instruction. This value will be used
 * by a ret instruction, in order to return execution to the origin of the call.
 *
 * Note that to begin the execution of an occam program, its entrypoint is
 * effectively called, with the appropriate arguments on the stack, as defined
 * by the signature of the top level process (TLP).
 */

/** EXAMPLE
 * Perform a call to a named function
 *
 * callme:
 * ... function does some work
 * ret
 *
 * ...
 * call callme
 * ...
 */

/** EXAMPLE
 * Perform a call with more than three arguments
 *
 * ajw -2
 * ldl 10
 * stl 0
 * ldl 20
 * stl 1
 * ldl 4
 * ldl 6
 * ldl 9
 * call callme
 * ajw 2
 */

/** STATE
 * (AREG (in (str actual param 1)) (out nextinst))
 * (BREG (in (str actual param 2)) (out BREG))
 * (CREG (in (str actual param 3)) (out CREG))
 * (OREG (in (str jump offset)) (out 0))
 * (WPTR (out (+wordsize WPTR -4)))
 * (IPTR (out (+bytesize nextinst OREG)))
 * (mem (out (begin 
 *   (writemem (+wordsize (prime WPTR) 0) IPTR)
 *   (writemem (+wordsize (prime WPTR) 1) AREG)
 *   (writemem (+wordsize (prime WPTR) 2) BREG)
 *   (writemem (+wordsize (prime WPTR) 3) CREG)
 *   )))
 */

TVM_INSTRUCTION (ins_call)
{
	/* Store registers in a new stack frame */
	write_word_and_type(ectx, wordptr_minus(WPTR, 4 - 0), (WORD)IPTR, STYPE_RET);
	write_word_and_type(ectx, wordptr_minus(WPTR, 4 - 1), AREG, AREGt);
	write_word_and_type(ectx, wordptr_minus(WPTR, 4 - 2), BREG, BREGt);
	write_word_and_type(ectx, wordptr_minus(WPTR, 4 - 3), CREG, CREGt);
	/* Actually allocate the stack frame */
	WPTR = wordptr_minus(WPTR, 4);

	/* Set the AREG to the old IPTR */
	STACK1((WORD)IPTR, STYPE_BC);

	/* Set the new IPTR from the OREG */
	IPTR = byteptr_plus(IPTR, OREG);

	CLEAR(OREG);

	return ECTX_CONTINUE;
}

/* 0xA_ - cj - conditional jump */

/** DESCRIPTION
 * This instruction performs a conditional jump depending on the value in the A
 * register. If the A register contains the value zero, this instruction jumps
 * to location specified as its argument, otherwise it pops the evaluation stack
 * and continues execution.
 *
 * When the conditional branch is taken, the stack is left unaffected.
 */

/** STATE
 * (where
 *   ((= AREG 0)
 *    (AREG (in (str condition)) (out AREG))
 *    (BREG (out BREG))
 *    (CREG (out CREG))
 *    (IPTR (out (+bytesize nextinst OREG)))
 *    (OREG (in (str jump offset)) (out 0)))
 *   ((!= AREG 0)
 *    (AREG (in (str condition)) (out BREG))
 *    (BREG (out CREG))
 *    (CREG (out undefined))
 *    (IPTR (out nextinst))
 *    (OREG (in (str jump offset)) (out 0))))
 */

TVM_INSTRUCTION (ins_cj)
{
	/* If AREG is = 0 then we jump, otherwise pop the stack */
	if(AREG == 0)
	{
		/* Set the IPTR to the new address: IPTR offset by OREG */
		IPTR = byteptr_plus(IPTR, OREG);

		/* Stack is left untouched */
		/* STACK(AREG, BREG, CREG); */
	}
	else
	{
		/* Pop the stack */
		STACK2(BREG, CREG, BREGt, CREGt);
	}

	CLEAR(OREG);

	return ECTX_CONTINUE;
}

/* 0xC_ - eqc - test if constant is equal to AREG */

/** DESCRIPTION
 * This instruction tests if the constant supplied as an argument to the
 * instruction is equal to the A register. If the constant and the A register
 * are equal, the A register is set to the value 1, otherwise it is set to 0.
 */

/** STATE
 * (where 
 *  ((= AREG OREG)
 *   (AREG (in (str value)) (out 1))
 *   (OREG (in (str constant)) (out 0))
 *   (IPTR (out nextinst)))
 *  ((!= AREG OREG)
 *   (AREG (in (str value)) (out 0))
 *   (OREG (in (str constant)) (out 0))
 *   (IPTR (out nextinst))))
 */

TVM_INSTRUCTION (ins_eqc)
{
	/* Check if AREG is equal to the OREG, set AREG accordingly */
	if(AREG == OREG)
	{
		AREG = 1; /* Set AREG to true */
	}
	else
	{
		AREG = 0; /* Set AREG to false */
	}

	SET_AREGt(STYPE_DATA);
	CLEAR(OREG);

	return ECTX_CONTINUE;
}

/* 0x0_ - j - jump */

/** DESCRIPTION
 * This is an unconditional jump. It jumps to the location given as the
 * argument to this instruction unconditionally.
 *
 * On the T9000, an unconditional jump of length 0, was used to indicate that a
 * debug trap should be triggered. Jumps of length 0 do not cause debug traps
 * on the Transterpreter.
 */

/** TIMESLICE */

/** STATE
 * (OREG (in (str offset)) (out 0))
 * (IPTR (out (+bytesize nextinst OREG)))
 */

TVM_INSTRUCTION (ins_j)
{
	IPTR = byteptr_plus(IPTR, OREG);

	/* FIXME: The T9000 provides a breakpoint hook if the OREG is 0 (ie a jump of
	 * size zero, which would just execute the next instruction). The
	 * Transterpreter does not currently have this implemented. Could be
	 * implemented in the future if needed.
	 */
	/*
	if(OREG == 0)
	{
		exit_runloop(EXIT_DEBUG_TRAP);
	}
	*/

	/* This instruction undefines all of the stack */
 	/* STACK(UNDEFINE(AREG), UNDEFINE(BREG), UNDEFINE(CREG)); */
	
	CLEAR(OREG);

	return ECTX_CONTINUE;
}

/* 0x4_ - ldc - load constant */

/** DESCRIPTION
 * This instruction loads a constant (supplied as an argument) onto the 
 * evaluation stack.
 */

/** STATE
 * (AREG (out OREG))
 * (BREG (out AREG))
 * (CREG (out BREG))
 * (OREG (in (str constant)) (out 0))
 * (IPTR (out nextinst))
 */

TVM_INSTRUCTION (ins_ldc)
{
	/* Push the stack down and put the constant on the top of the stack (AREG) */
	STACK(OREG, AREG, BREG, STYPE_DATA, AREGt, BREGt);

	CLEAR(OREG);

	return ECTX_CONTINUE;
}

/* 0x7_ - ldl - load local */

/** DESCRIPTION
 * 
 * Loads a value from the workspace, relative to the WPTR, and puts it in the A
 * register of the evaluation stack. The workspace slot to load from is
 * determined by addressing using WPTR as a base and OREG as an offset.
 *
 * The first 16 workspace slots (in relation to WPTR) can be accessed using
 * just one instruction (ie ldl) slots which are further away will need one or
 * more prefixing instructions. To load from slot number n, log16(n)
 * instructions are required (log16(n) - 1) prefixing instructions and the ldl.
 */

/** STATE
 * (AREG (out (readmem (+wordsize WPTR OREG))))
 * (BREG (out AREG))
 * (CREG (out BREG))
 * (OREG (in (str offset into workspace)) (out 0))
 * (IPTR (out nextinst))
 */

TVM_INSTRUCTION (ins_ldl)
{
	/* Push the stack down and read a value from from memory(OREG+WPTR) */
	WORDPTR offset = wordptr_plus(WPTR, OREG);
	STACK(read_word(offset), AREG, BREG, read_type(ectx, offset), AREGt, BREGt);

	CLEAR(OREG);

	return ECTX_CONTINUE;
}

/* 0x1_ - ldlp - load local pointer */

/** DESCRIPTION
 *
 * Loads a pointer to a value in the workspace in relation to the WPTR and the
 * operand of ldlp. Much like ldl, but instead of loading the value out of a
 * workspace slot it loads the pointer to that slot. This pointer can then be
 * passed to other pieces piecies of code so they can access that particular 
 * workspace slot.
 */

/** EXAMPLE
 *
 * Call a function and pass a reference to workspace slot 3 as the first actual
 * parameter.
 *
 * ldlp 3
 * call aproc
 */

/** STATE
 * (AREG (out (+wordsize WPTR OREG)))
 * (BREG (out AREG))
 * (CREG (out BREG))
 * (OREG (in (str offset into workspace)) (out 0))
 * (IPTR (out nextinst))
 */

TVM_INSTRUCTION (ins_ldlp)
{
	/* Push WPTR+OREG onto the stack */
	STACK((WORD)wordptr_plus(WPTR, OREG), AREG, BREG,
		STYPE_WS, AREGt, BREGt);

	CLEAR(OREG);

	return ECTX_CONTINUE;
}

/* 0x3_ - ldnl - load non-local */

/** DESCRIPTION
 * Loads a value onto the stack, addressed by using the A register as a base
 * and the operand register as an offset.
 *
 * The address in the A register is replaced with the value loaded from memory.
 */

/** STATE
 * (assumes (= (|| AREG byteselectmask) 0))
 * (AREG (in (str base)) (out (readmem (+wordsize AREG OREG))))
 * (OREG (in (str offset)) (out 0))
 * (IPTR (out nextinst))
 */

TVM_INSTRUCTION (ins_ldnl)
{
	/* Read from memory(AREG+OREG) */
	WORDPTR offset = wordptr_plus((WORDPTR)AREG, OREG);
	AREG = read_word(offset);
	SET_AREGt(read_type(ectx, offset));

	CLEAR(OREG);

	return ECTX_CONTINUE;
}

/* 0x5_ - ldnlp - load non-local pointer */

/** DESCRIPTION
 * Load a pointer to a workspace slot by using the A register as a base and the
 * operand register as an offset. Much like ldnl, but instead of loading the
 * value from a slot, it loads a pointer to that slot. For an example see ldlp
 * which performs the same operation, but using the WPTR instead of the A
 * register.
 */

/** STATE
 * (assumes (= (|| AREG byteselectmask) 0))
 * (AREG (in (str base)) (out (readmem (+wordsize AREG OREG))))
 * (OREG (in (str offset)) (out 0))
 * (IPTR (out nextinst))
 */
TVM_INSTRUCTION (ins_ldnlp)
{
	/* Add the OREG to the AREG and store it in the AREG */
	AREG = (WORD)wordptr_plus((WORDPTR)AREG, OREG);
	/* Type signature unchanged */

	CLEAR(OREG);

	return ECTX_CONTINUE;
}

/* 0xD_ - stl - store local */

/** DESCRIPTION
 *
 * This instruction stores a value into a workspace slot indexed by the 
 * workspace pointer. The value which is to be stored is found in the A
 * register and the slot (offset from WPTR) at which it is to be stored is the
 * operand to this instruction.
 */

/** STATE
 * (AREG (in (str value to be stored)) (out BREG))
 * (BREG (out CREG))
 * (CREG (out undefined))
 * (OREG (in (str offset into workspace)) (out 0))
 * (IPTR (out nextinst))
 * (mem (out (writemem (+wordsize WPTR OREG) AREG)))
 */

TVM_INSTRUCTION (ins_stl)
{
	/* Put the top of the stack into mem(WPTR + OREG) */
	write_word_and_type(ectx, wordptr_plus(WPTR, OREG), AREG, AREGt);
	CLEAR(OREG);
	
	/* Pop the stack */
	STACK2_RET(BREG, CREG, BREGt, CREGt);
}

/* 0xE_ - stnl - store non-local */

/** DESCRIPTION
 *
 * This instruction stores a value into a slot addressed by using the A
 * register as a base and the operand register as an offset. The value which is
 * to be stored in memory is located in the B register.
 */

/** STATE
 * (assumes (= (|| AREG byteselectmask) 0))
 * (AREG (in (str base)) (out CREG))
 * (BREG (in (str value)) (out undefined))
 * (CREG (out undefined))
 * (OREG (in (str offset)) (out 0))
 * (IPTR (out nextinst))
 * (mem (out (writemem (+wordsize AREG OREG) BREG)))
 */

TVM_INSTRUCTION (ins_stnl)
{
	/* Put value in BREG into mem(AREG + OREG) */
	write_word_and_type(ectx, wordptr_plus((WORDPTR)AREG, OREG), BREG, BREGt);
	CLEAR(OREG);

	/* Pop the stack */
	STACK1_RET(CREG, CREGt);
}




/* 0x2_ - pfix - prefix */

/** DESCRIPTION
 * This instruction builds up a number in the operand register. It shifts the
 * operand register left by four bits.
 *
 * The instruction decode will have put the operand into the operand register,
 * and hence the pfix instruction does not need to do this. It does however
 * need to shift the operand register to make space for the next part of the
 * operand which will be deposited on the next instruction decode.
 */

/** STATE
 * (OREG (out (<< OREG 4)))
 * (IPTR (out nextinst))
 */

TVM_INSTRUCTION (ins_pfix)
{
	/* Shift the operand register up */
	/* ANNO: Cast to UNSIGNED WORD as the behaviour of a shift on negative values
	 * is implementation defined in C */
	OREG = (WORD)((UWORD)OREG << 4);

	return ECTX_CONTINUE;
}

/* 0x6_ - nfix - negative prefix */

/** DESCRIPTION
 *
 * This instruction builds up a number in the operand register. It performs a
 * bitwise not of the operand register and shifts it left by four bits.
 *
 * The instruction decode will have put the operand into the operand register,
 * and hence the nfix instruction does not need to do this. It does however
 * need to shift the operand register to make space for the next part of the
 * operand which will be deposited on the next instruction decode.
 */

/** STATE
 * (OREG (out (<< (bitnot OREG) 4)))
 * (IPTR (out nextinst))
 */

TVM_INSTRUCTION (ins_nfix)
{
	/* Negate the value in the operand register, and shift it up */
	OREG = (WORD)((~(UWORD)OREG) << 4);

	return ECTX_CONTINUE;
}

#ifndef TVM_DISPATCH_SWITCH
/* 0xF_ - opr - operate */

/** DESCRIPTION
 *
 * The operate instruction dispatches secondary instructions. 
 *
 * It is not possible for secondary instructions to have an operand, as the
 * operand is used by the operate instruction to dispatch the secondary
 * instruction. Secondary instructions instead must already have their
 * arguments in registers, memory or both.
 *
 * The operate instruction does not itself advance the instruction pointer,
 * this is left to the secondary instruction, as it may want to change the
 * instruction pointer in non-obvious ways.
 *
 * While the operate instruction only changes the operand register to zero, the
 * secondary instruction which it invokes may alter the state of the
 * Transterpreter in any way it sees fit.
 */

/** EXAMPLE
 *
 * Dispatch the in instruction (0x07)
 *
 * opr 7
 */

/** EXAMPLE
 *
 * Dispatch the alt start instruction (0x43)
 *
 * pfix 4
 * opr 3
 */

/** STATE
 * (OREG (in (str operator selector)) (out 0))
 */

TVM_INSTRUCTION (ins_opr)
{
	WORD ins = OREG;

	if(ins <= secondaries_max)
	{
		CLEAR(OREG);
		return secondaries[ins](ectx);
	}
	else
	{
#ifdef TVM_OCCAM_PI
		if(ins >= extended_secondaries_min && ins <= extended_secondaries_max)
		{
			CLEAR(OREG);
			return extended_secondaries[ins - extended_secondaries_min](ectx);
		} 
#else /* !TVM_OCCAM_PI */
		if (ins == 0x237) /* getpas */
		{
			CLEAR(OREG);
			return ins_getpas(ectx);
		}
#endif /* !TVM_OCCAM_PI */
	}

	return ECTX_INS_INVALID;
}
#endif
