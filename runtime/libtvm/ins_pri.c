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

#include "transputer.h"
#include "instructions.h"
#include "interpreter.h"
#include "mem.h"
#include "ins_pri.h"

#ifndef TVM_DISPATCH_SWITCH
#include "jumptbl_sec.h"
#ifdef __PI_SUPPORT__
#include "jumptbl_ex_sec.h"
#else /* !__PI_SUPPORT__ */
#include "ins_fred.h"
#endif /* !__PI_SUPPORT__ */
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
 * (areg (in (str addend)) (out (+checked areg oreg)))
 * (oreg (in (str constant)) (out 0))
 * (iptr (out nextinst))
 * (errorflag (out (str set on overflow from addition)))
 */

TVM_INSTRUCTION void ins_adc(void)
{
	/* Add the operand register to areg */
	WORD result = areg + oreg;


	/* Check for overflow, from Hackers Delight p. 27 */
	if( ((UWORD) (((result) ^ areg) & ((result) ^ oreg)) >> (WORD_BITS - 1)) )
	{
		set_error_flag(EFLAG_ADC);
	}

	/* Areg gets result, Breg and Creg stay the same */
	areg = result;

	CLEAR(oreg);
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
 * (oreg (in (str offset)) (out 0))
 * (wptr (out  (+wordsize wptr oreg)))
 * (iptr (out nextinst))
 */

TVM_INSTRUCTION void ins_ajw(void)
{
	/* Add the value in the operand register to the workspace pointer. */
	wptr = pooter_plus(wptr, oreg);

	CLEAR(oreg);
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
 * (areg (in (str actual param 1)) (out nextinst))
 * (breg (in (str actual param 2)) (out breg))
 * (creg (in (str actual param 3)) (out creg))
 * (oreg (in (str jump offset)) (out 0))
 * (wptr (out (+wordsize wptr -4)))
 * (iptr (out (+bytesize nextinst oreg)))
 * (mem (out (begin 
 *   (writemem (+wordsize (prime wptr) 0) iptr)
 *   (writemem (+wordsize (prime wptr) 1) areg)
 *   (writemem (+wordsize (prime wptr) 2) breg)
 *   (writemem (+wordsize (prime wptr) 3) creg)
 *   )))
 */

TVM_INSTRUCTION void ins_call(void)
{
	/* Store registers in a new stack frame */
	write_mem(pooter_minus(wptr, 4 - 0), (WORD)iptr);
	write_mem(pooter_minus(wptr, 4 - 1), areg);
	write_mem(pooter_minus(wptr, 4 - 2), breg);
	write_mem(pooter_minus(wptr, 4 - 3), creg);
	/* Actually allocate the stack frame */
	wptr = pooter_minus(wptr, 4);

	/* Set the areg to the old iptr */
	areg = (WORD)iptr;

	/* Set the new iptr from the oreg */
	iptr = bpooter_plus(iptr, oreg);

	CLEAR(oreg);
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
 *   ((= areg 0)
 *    (areg (in (str condition)) (out areg))
 *    (breg (out breg))
 *    (creg (out creg))
 *    (iptr (out (+bytesize nextinst oreg)))
 *    (oreg (in (str jump offset)) (out 0)))
 *   ((!= areg 0)
 *    (areg (in (str condition)) (out breg))
 *    (breg (out creg))
 *    (creg (out undefined))
 *    (iptr (out nextinst))
 *    (oreg (in (str jump offset)) (out 0))))
 */

TVM_INSTRUCTION void ins_cj(void)
{
	/* If areg is = 0 then we jump, otherwise pop the stack */
	if(areg == 0)
	{
		/* Set the iptr to the new address: iptr offset by oreg */
		iptr = bpooter_plus(iptr, oreg);

		/* Stack is left untouched */
		STACK(areg, breg, creg);
	}
	else
	{
		/* Pop the stack */
		STACK(breg, creg, UNDEFINE(creg));
	}

	CLEAR(oreg);
}

/* 0xC_ - eqc - test if constant is equal to areg */

/** DESCRIPTION
 * This instruction tests if the constant supplied as an argument to the
 * instruction is equal to the A register. If the constant and the A register
 * are equal, the A register is set to the value 1, otherwise it is set to 0.
 */

/** STATE
 * (where 
 *  ((= areg oreg)
 *   (areg (in (str value)) (out 1))
 *   (oreg (in (str constant)) (out 0))
 *   (iptr (out nextinst)))
 *  ((!= areg oreg)
 *   (areg (in (str value)) (out 0))
 *   (oreg (in (str constant)) (out 0))
 *   (iptr (out nextinst))))
 */

TVM_INSTRUCTION void ins_eqc(void)
{
	/* Check if areg is equal to the oreg, set areg accordingly */
	if(areg == oreg)
	{
		areg = 1; /* Set areg to true */
	}
	else
	{
		areg = 0; /* Set areg to false */
	}

	CLEAR(oreg);
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
 * (oreg (in (str offset)) (out 0))
 * (iptr (out (+bytesize nextinst oreg)))
 */

TVM_INSTRUCTION void ins_j(void)
{
	iptr = bpooter_plus(iptr, oreg);

	/* FIXME: The T9000 provides a breakpoint hook if the oreg is 0 (ie a jump of
	 * size zero, which would just execute the next instruction). The
	 * Transterpreter does not currently have this implemented. Could be
	 * implemented in the future if needed.
	 */
	/*
	if(oreg == 0)
	{
		exit_runloop(EXIT_DEBUG_TRAP);
	}
	*/

	/* This instruction undefines all of the stack */
 	/* STACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg)); */
	
	CLEAR(oreg);
}

/* 0x4_ - ldc - load constant */

/** DESCRIPTION
 * This instruction loads a constant (supplied as an argument) onto the 
 * evaluation stack.
 */

/** STATE
 * (areg (out oreg))
 * (breg (out areg))
 * (creg (out breg))
 * (oreg (in (str constant)) (out 0))
 * (iptr (out nextinst))
 */

TVM_INSTRUCTION void ins_ldc(void)
{
	/* Push the stack down and put the constant on the top of the stack (areg) */
	STACK(oreg, areg, breg);

	CLEAR(oreg);
}

/* 0x7_ - ldl - load local */

/** DESCRIPTION
 * 
 * Loads a value from the workspace, relative to the wptr, and puts it in the A
 * register of the evaluation stack. The workspace slot to load from is
 * determined by addressing using wptr as a base and oreg as an offset.
 *
 * The first 16 workspace slots (in relation to wptr) can be accessed using
 * just one instruction (ie ldl) slots which are further away will need one or
 * more prefixing instructions. To load from slot number n, log16(n)
 * instructions are required (log16(n) - 1) prefixing instructions and the ldl.
 */

/** STATE
 * (areg (out (readmem (+wordsize wptr oreg))))
 * (breg (out areg))
 * (creg (out breg))
 * (oreg (in (str offset into workspace)) (out 0))
 * (iptr (out nextinst))
 */

TVM_INSTRUCTION void ins_ldl(void)
{
	/* Push the stack down and read a value from from memory(oreg+wptr) */
	STACK(read_mem(pooter_plus(wptr, oreg)), areg, breg);

	CLEAR(oreg);
}

/* 0x1_ - ldlp - load local pointer */

/** DESCRIPTION
 *
 * Loads a pointer to a value in the workspace in relation to the wptr and the
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
 * (areg (out (+wordsize wptr oreg)))
 * (breg (out areg))
 * (creg (out breg))
 * (oreg (in (str offset into workspace)) (out 0))
 * (iptr (out nextinst))
 */

TVM_INSTRUCTION void ins_ldlp(void)
{
	/* Push wptr+oreg onto the stack */
	STACK((WORD)pooter_plus(wptr, oreg), areg, breg);

	CLEAR(oreg);
}

/* 0x3_ - ldnl - load non-local */

/** DESCRIPTION
 * Loads a value onto the stack, addressed by using the A register as a base
 * and the operand register as an offset.
 *
 * The address in the A register is replaced with the value loaded from memory.
 */

/** STATE
 * (assumes (= (|| areg byteselectmask) 0))
 * (areg (in (str base)) (out (readmem (+wordsize areg oreg))))
 * (oreg (in (str offset)) (out 0))
 * (iptr (out nextinst))
 */

TVM_INSTRUCTION void ins_ldnl(void)
{
	/* Read from memory(areg+oreg) */
	areg = read_mem(pooter_plus((POOTER)areg, oreg));

	CLEAR(oreg);
}

/* 0x5_ - ldnlp - load non-local pointer */

/** DESCRIPTION
 * Load a pointer to a workspace slot by using the A register as a base and the
 * operand register as an offset. Much like ldnl, but instead of loading the
 * value from a slot, it loads a pointer to that slot. For an example see ldlp
 * which performs the same operation, but using the wptr instead of the A
 * register.
 */

/** STATE
 * (assumes (= (|| areg byteselectmask) 0))
 * (areg (in (str base)) (out (readmem (+wordsize areg oreg))))
 * (oreg (in (str offset)) (out 0))
 * (iptr (out nextinst))
 */
TVM_INSTRUCTION void ins_ldnlp(void)
{
	/* Add the oreg to the areg and store it in the areg */
	areg = (WORD)pooter_plus((POOTER)areg, oreg);

	CLEAR(oreg);
}

/* 0xD_ - stl - store local */

/** DESCRIPTION
 *
 * This instruction stores a value into a workspace slot indexed by the 
 * workspace pointer. The value which is to be stored is found in the A
 * register and the slot (offset from wptr) at which it is to be stored is the
 * operand to this instruction.
 */

/** STATE
 * (areg (in (str value to be stored)) (out breg))
 * (breg (out creg))
 * (creg (out undefined))
 * (oreg (in (str offset into workspace)) (out 0))
 * (iptr (out nextinst))
 * (mem (out (writemem (+wordsize wptr oreg) areg)))
 */

TVM_INSTRUCTION void ins_stl(void)
{
	/* Put the top of the stack into mem(wptr + oreg) */
	write_mem(pooter_plus(wptr, oreg), areg);

	/* Pop the stack */
	STACK(breg, creg, UNDEFINE(creg));

	CLEAR(oreg);
}

/* 0xE_ - stnl - store non-local */

/** DESCRIPTION
 *
 * This instruction stores a value into a slot addressed by using the A
 * register as a base and the operand register as an offset. The value which is
 * to be stored in memory is located in the B register.
 */

/** STATE
 * (assumes (= (|| areg byteselectmask) 0))
 * (areg (in (str base)) (out creg))
 * (breg (in (str value)) (out undefined))
 * (creg (out undefined))
 * (oreg (in (str offset)) (out 0))
 * (iptr (out nextinst))
 * (mem (out (writemem (+wordsize areg oreg) breg)))
 */

TVM_INSTRUCTION void ins_stnl(void)
{
	/* Put value in breg into mem(areg + oreg) */
	write_mem(pooter_plus((POOTER)areg, oreg), breg);

	/* Pop the stack */
	STACK(creg, UNDEFINE(breg), UNDEFINE(creg));
	
	CLEAR(oreg);
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
 * (oreg (out (<< oreg 4)))
 * (iptr (out nextinst))
 */

TVM_INSTRUCTION void ins_pfix(void)
{
	/* Shift the operand register up */
	/* ANNO: Cast to UNSIGNED WORD as the behaviour of a shift on negative values
	 * is implementation defined in C */
	oreg = (WORD)((UWORD)oreg << 4);
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
 * (oreg (out (<< (bitnot oreg) 4)))
 * (iptr (out nextinst))
 */

TVM_INSTRUCTION void ins_nfix(void)
{
	/* Negate the value in the operand register, and shift it up */
	oreg = (WORD)((~(UWORD)oreg) << 4);
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
 * (oreg (in (str operator selector)) (out 0))
 */

TVM_INSTRUCTION void ins_opr(void)
{
	if(oreg <= secondaries_max)
	{
		secondaries[oreg]();
	}
	else
	{
#ifdef __PI_SUPPORT__
		if(oreg >= extended_secondaries_min && oreg <= extended_secondaries_max)
		{
			extended_secondaries[oreg - extended_secondaries_min]();
		} 
#else /* !__PI_SUPPORT__*/
		if (oreg == 0x237) /* getpas */
		{
			ins_getpri();
		}
#endif /* !__PI_SUPPORT__*/
		else
		{
			ins_not_implemented();
		}
	}

	CLEAR(oreg);
}
#endif
