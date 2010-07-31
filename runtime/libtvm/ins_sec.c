/*
tvm - ins_sec.c
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
#include "scheduler.h"
#include "ins_sec.h"

/****************************************************************************
 *                   0xF_              0xF_              0xF_               *
 ****************************************************************************/

/* 0x00 - 0xF0 - rev - reverse */
TVM_INSTRUCTION (ins_rev)
{
	STACK_RET(BREG, AREG, CREG, BREGt, AREGt, CREGt);
}

/* 0x01 - 0xF1 - lb - load byte */
TVM_INSTRUCTION (ins_lb)
{
	STACK_RET((WORD)read_byte((BYTEPTR)AREG), BREG, CREG, STYPE_DATA, BREGt, CREGt);
}

/* 0x02 - 0xF2 - bsub - byte subscript */
TVM_INSTRUCTION (ins_bsub)
{
	STACK2_RET((WORD)byteptr_plus((BYTEPTR) AREG, BREG), CREG, PICK_POINTER_TYPE (AREGt, BREGt), CREGt);
}

/* 0x03 - 0xF3 - endp - end process */
TVM_INSTRUCTION (ins_endp)
{
	/* Check the process count */
	/* if(((WORD *)AREG)[1] == 1) */
	if(read_word(wordptr_plus((WORDPTR)AREG, 1)) == 1) 
	{
		/* No more child processes, continue as the parent process */

		/* Set the process count to zero */
		/* ((WORD *)AREG)[1] = 0; */
		write_word(wordptr_plus((WORDPTR)AREG, 1), 0);
		/* Get the resume address from the workspace */
		/* IPTR = (BYTE *)((WORD *)AREG)[0]; */
		IPTR = (BYTEPTR)read_word((WORDPTR)AREG);
		/* The AREG becomes the new WPTR */
		WPTR = (WORDPTR)AREG;
		/* The entire stack becomes undefined */
		UNDEFINE_STACK_RET();
	}
	else
	{
		/* Terminate current process, and reschedule another from the queue */

		/* Subtract one from the process count */
		/*((WORD *)AREG)[1] = ((WORD *)AREG)[1] - 1;*/
		write_word(wordptr_plus((WORDPTR)AREG, 1), 
				read_word(wordptr_plus((WORDPTR)AREG, 1)) - 1);
		/* The entire stack becomes undefined */
		UNDEFINE_STACK();
		/* Run the next process */
		RUN_NEXT_ON_QUEUE_RET();
	}
}

/* 0x04 - 0xF4 - diff - difference */
TVM_INSTRUCTION (ins_diff)
{
	/* Unsigned subtract */
	STACK2_RET((WORD)((UWORD) BREG) - ((UWORD) AREG), CREG, PICK_POINTER_TYPE(AREGt, BREGt), CREGt);
}

/* 0x05 - 0xF5 - add - addition */
TVM_INSTRUCTION (ins_add)
{
	WORD result = BREG + AREG;

	/* Check for overflow, from Hackers Delight p. 27 */
	if( ((UWORD) (((result) ^ BREG) & ((result) ^ AREG)) >> (WORD_BITS - 1)) )
	{
		SET_ERROR_FLAG(EFLAG_INTOV);
	}

	STACK2_RET(result, CREG, STYPE_DATA, CREGt);
}

/* 0x06 - 0xF6 - gcall - general call */
TVM_INSTRUCTION (ins_gcall)
{
	WORD temp;

	/* FIXME: this is just an exchange of two registers... whats the word on the
	 * stack macro in places like this?
	 */
	temp = AREG;
	AREG = (WORD)IPTR;
	SET_AREGt (STYPE_BC);
	IPTR = (BYTEPTR)temp;

	/* FIXME: This is not the INMOS GCALL, it has been modified by fred (I
	 * presume) to store the IPTR in WS_TOP. The original instruction does NOT
	 * do this!!!! */
	WORKSPACE_SET(WPTR, WS_TEMP, AREG);

	return ECTX_CONTINUE;
}

/* 0x08 - 0xF8 - prod - Unchecked Multiplication (product) */
TVM_INSTRUCTION (ins_prod)
{
	STACK2_RET(AREG * BREG, CREG, STYPE_DATA, CREGt);
}

/* 0x09 - 0xF9 - gt - greater than */
TVM_INSTRUCTION (ins_gt)
{
	if(BREG > AREG)
	{
		STACK2_RET(1, CREG, STYPE_DATA, CREGt);
	}
	else
	{
		STACK2_RET(0, CREG, STYPE_DATA, CREGt);
	}
}

/* 0x0A - 0xFA - wsub - word subscript */
TVM_INSTRUCTION (ins_wsub)
{
	/* FIXME: Same check as for bsub */
	STACK2_RET((WORD)wordptr_plus((WORDPTR)AREG, BREG), CREG, AREGt, CREGt);
}

/* 0x0C - 0xFC - sub - subtract */
TVM_INSTRUCTION (ins_sub)
{
	WORD result = BREG - AREG;

	/* Overflow detection from Hackers Delight p. 27 */
	if( ((UWORD) ((BREG ^ AREG) & (result ^ BREG))) >> (WORD_BITS - 1))
	{
		SET_ERROR_FLAG(EFLAG_INTOV);
	}

	STACK2_RET(BREG - AREG, CREG, STYPE_DATA, CREGt);
}

/* 0x0D - 0xFD - startp - star process */
TVM_INSTRUCTION (ins_startp)
{
	ADD_TO_QUEUE_IPTR((WORDPTR)AREG, byteptr_plus(IPTR, BREG));
	UNDEFINE_STACK_RET();
}


/****************************************************************************
 *              0x21 0xF_         0x21 0xF_         0x21 0xF_               *
 ****************************************************************************/

/* 0x10 - 0x21 0xF0 - seterr - set error */
TVM_INSTRUCTION (ins_seterr)
{
	SET_ERROR_FLAG_RET(EFLAG_SETERR);
}

/* 0x13 - 0x21 0xF3 - csub0 - check subscript from 0 */
TVM_INSTRUCTION (ins_csub0)
{
	/* FIXME: The implementation of this is incorrect in soccam, the
	 * error flag can be cleared in soccam, this is wrong.
	 */
	if(((UWORD) BREG) >= ((UWORD) AREG))
	{
		SET_ERROR_FLAG(EFLAG_INTERR);
	}
	STACK2_RET(BREG, CREG, BREGt, CREGt);
}

/* 0x15 - 0x21 0xF5 - stopp - stop process */
TVM_INSTRUCTION (ins_stopp)
{
	WORKSPACE_SET((WORDPTR) WPTR, WS_IPTR, (WORD) IPTR);

	RUN_NEXT_ON_QUEUE_RET();
}
	
/* 0x16 - 0x21 0xF6 LADD - long addition  - used in conjunction with lsum*/
TVM_INSTRUCTION (ins_ladd)
{
	WORD result = ((WORD) BREG) + ((WORD) AREG) + ((WORD) CREG & 1);

	/* Check for overflow, from Hackers Delight p. 27 */
	if( ((UWORD) (((result) ^ BREG) & ((result) ^ AREG)) >> (WORD_BITS - 1)) )
	{
		SET_ERROR_FLAG(EFLAG_INTOV);
	}

	STACK1_RET(result, STYPE_DATA);
}

/* 0x19 - 0x21 0xF9 - norm - normalise */
TVM_INSTRUCTION (ins_norm)
{
	CREG = 0;
	if(AREG == 0 && BREG == 0)
	{
		STACK(AREG, BREG, 2 * WORD_BITS, AREGt, BREGt, STYPE_DATA);
	}
	else
	{
		while(!(BREG & MIN_INT))
		{
			BREG = BREG << 1;
			if(AREG & MIN_INT)
			{
				BREG = BREG | 1;
			}
			AREG = AREG << 1;
			++CREG;
		}
		/* STACK(AREG, BREG, CREG) */
	}

	return ECTX_CONTINUE;
}

#if !defined(TVM_HAVE_TWOWORD) && TVM_WORD_LENGTH == 4
static TVM_INLINE int nlz(unsigned int x) {
	int n;

	if (x == 0) return(32);
	n = 0;
	if (x <= 0x0000FFFF) {n = n +16; x = x <<16;}
	if (x <= 0x00FFFFFF) {n = n + 8; x = x << 8;}
	if (x <= 0x0FFFFFFF) {n = n + 4; x = x << 4;}
	if (x <= 0x3FFFFFFF) {n = n + 2; x = x << 2;}
	if (x <= 0x7FFFFFFF) {n = n + 1;}
	return n;
}
#endif

#if defined(TVM_HAVE_TWOWORD) || TVM_WORD_LENGTH == 4
/* 0x1A - 0x21 0xFA LDIV - divides a double length value in the 
 * reg pair CB by a single length value in A */
TVM_INSTRUCTION (ins_ldiv)
{
#ifdef TVM_HAVE_TWOWORD
	UTWOWORD value = (((UTWOWORD) ((UWORD) CREG)) << WORD_BITS) | ((UWORD) BREG);

	BREG = value % ((UWORD) AREG);
	AREG = value / ((UWORD) AREG);

	STACK2_RET(AREG, BREG, STYPE_DATA, STYPE_DATA);
#else
	/* 
	 * The dividend is u1 and u0, with u1 being the most significant word.
	 * The divisor is parameter v.
	 */
	const unsigned b = 65536; /* Number base (16 bits). */
	unsigned un1, un0,        /* Norm. dividend LSD's. */
		 vn1, vn0,        /* Norm. divisor digits. */
		 q1, q0,          /* Quotient digits. */
		 un32, un21, un10,/* Dividend digit pairs. */
		 rhat;            /* A remainder. */ 
	int s;                    /* Shift amount for norm. */

	unsigned u1 = CREG;
	unsigned u0 = BREG;
	unsigned v  = AREG;

	/*
	   if (u1 >= v) {            // If overflow, set rem.
	   if (r != NULL)         // to an impossible value,
	 *r = 0xFFFFFFFF;    // and return the largest
	 return 0xFFFFFFFF;}    // possible quotient.
	 */
	if(u1 >= v) /* CREG >= AREG */
	{
		SET_ERROR_FLAG(EFLAG_INTOV);
	}

	s = nlz(v);               /* 0 <= s <= 31. */
	v = v << s;               /* Normalize divisor. */
	vn1 = v >> 16;            /* Break divisor up into */
	vn0 = v & 0xFFFF;         /* two 16-bit digits. */

	/* un32 = (u1 << s) | (u0 >> 32 - s) & (-s >> 31); */
	un32 = (u1 << s) | ((u0 >> (32 - s)) & ((-s) >> 31));
	un10 = u0 << s;           /* Shift dividend left. */

	un1 = un10 >> 16;         /* Break right half of */
	un0 = un10 & 0xFFFF;      /* dividend into two digits. */

	q1 = un32/vn1;            /* Compute the first */
	rhat = un32 - q1*vn1;     /* quotient digit, q1. */
again1:
	if (q1 >= b || q1*vn0 > b*rhat + un1)
	{
		q1 = q1 - 1;
		rhat = rhat + vn1;
		if (rhat < b) 
		{
			goto again1;
		}
	}

	un21 = un32*b + un1 - q1*v;  /* Multiply and subtract. */

	q0 = un21/vn1;            /* Compute the second */
	rhat = un21 - q0*vn1;     /* quotient digit, q0. */
again2:
	if (q0 >= b || q0*vn0 > b*rhat + un0) 
	{
		q0 = q0 - 1;
		rhat = rhat + vn1;
		if (rhat < b)
		{
			goto again2;
		}
	}

	/*if (r != NULL)            // If remainder is wanted, */
	/*	*r = (un21*b + un0 - q0*v) >> s;     // return it. */
	/*return q1*b + q0; */
	BREG = (un21*b + un0 - q0*v) >> s;
	AREG = q1*b + q0;

	STACK2_RET(AREG, BREG, STYPE_DATA, STYPE_DATA);
#endif
}
#endif /* defined(TVM_HAVE_TWOWORD) || TVM_WORD_LENGTH == 4 */

/* 0x1B - 0x21 0xFB - ldpi - load pointer to instruction */
TVM_INSTRUCTION (ins_ldpi)
{
	STACK_RET((WORD)byteptr_plus(IPTR, AREG), BREG, CREG, STYPE_BC, BREGt, CREGt);
}

/* 0x1D - 0x21 0xFD - xdble - extend to double */
TVM_INSTRUCTION (ins_xdble)
{
	if(AREG < 0)
	{
		STACK_RET(AREG, -1, BREG, AREGt, STYPE_DATA, CREGt);
	}
	else
	{
		/* AREG >= 0 */
		STACK_RET(AREG, 0, BREG, AREGt, STYPE_DATA, CREGt);
	}
}

/* 0x1F - 0x21 0xFF - rem - (integer) remainder */
TVM_INSTRUCTION (ins_rem)
{
	if((AREG == 0) || ((AREG == -1) && (BREG == MIN_INT)))
	{
		SET_ERROR_FLAG(EFLAG_INTOV);
	}
	
	STACK2_RET((BREG % AREG), CREG, STYPE_DATA, CREGt);
}


/****************************************************************************
 *              0x22 0xF_         0x22 0xF_         0x22 0xF_               *
 ****************************************************************************/

/* 0x20 - 0x22 0xF0 - ret - return */
TVM_INSTRUCTION (ins_ret)
{
	BYTEPTR ret_addr = (BYTEPTR)read_word(WPTR);

	fill_type_shadow(ectx, (BYTEPTR)WPTR, 4 << WSH, STYPE_UNDEF);

	IPTR = ret_addr;
	WPTR = wordptr_plus(WPTR, 4);

	return ECTX_CONTINUE;
}

/* 0x21 - 0x22 0xF1 - lend - loop end */
TVM_INSTRUCTION (ins_lend)
{
	/* FIXME: I dont think that the soccam version of this sets CREG to 
	 * undefined */
	/* FIXME: WARNING:
	 * If memory is not initialised to zero by the runtime, we need to
	 * initialise it to zero for this to work. The occam compiler seems to
	 * assume that memory is all zeros before the program runs. eg: The scheme
	 * interpreter inits memory to <void>.
	 */
	if(read_word(wordptr_plus((WORDPTR)BREG, 1)) > 1)
	{
		/* FIXME: This is done the other way around in soccam, I think I followed
		 * the order it was done in the book... check though */
		write_word((WORDPTR)BREG, read_word((WORDPTR)BREG) + 1);
		write_word(wordptr_plus((WORDPTR)BREG, 1), 
				read_word(wordptr_plus((WORDPTR)BREG, 1)) - 1);

		IPTR = byteptr_minus(IPTR, AREG);
	} 
	else 
	{
		/* Decrement the counter */
		write_word(wordptr_plus((WORDPTR)BREG, 1), 
				read_word(wordptr_plus((WORDPTR)BREG, 1)) - 1);
	 }			
		
	/* FIXME: I dont think the soccam instruction set the stack properly, it
	 * should use the stack macro, it dont (this is related to the first comment
	 * in this function  */
	STACK2_RET(AREG, BREG, AREGt, BREGt);
}

/* 0x2C - 0x22 0xFC - div - divide */
TVM_INSTRUCTION (ins_div)
{
	if((AREG == 0) || ((AREG == -1) && (BREG == MIN_INT)))
	{
		SET_ERROR_FLAG(EFLAG_INTOV);
	}
	
	STACK2_RET(BREG / AREG, CREG, STYPE_DATA, CREGt);
}
 




/****************************************************************************
 *              0x23 0xF_         0x23 0xF_         0x23 0xF_               *
 ****************************************************************************/

#if defined(TVM_HAVE_TWOWORD) || TVM_WORD_LENGTH == 4
/* 0x31 - 0x23 F1 - lmul - long multiply */
TVM_INSTRUCTION (ins_lmul)
{
#ifdef TVM_HAVE_TWOWORD
	UTWOWORD value = (((UTWOWORD) ((UWORD) BREG)) * ((UWORD) AREG)) + ((UWORD) CREG);

	AREG = value & LONG_LO_MASK;
	BREG = value >> WORD_BITS;

	STACK2_RET(AREG, BREG, STYPE_DATA, STYPE_DATA);
#else
	unsigned int u = BREG;
	unsigned int v = AREG;

	unsigned int u0, u1, v0, v1, k, t;
	unsigned int w1, w2, w3;

	WORD res, carry; /* For adding the carry (CREG) later */

	u0 = u >> 16; u1 = u & 0xFFFF;
	v0 = v >> 16; v1 = v & 0xFFFF;

	t = u1*v1;
	w3 = t & 0xFFFF;             /* (*) */
	k = t >> 16;

	t = u0*v1 + k;
	w2 = t & 0xFFFF;
	w1 = t >> 16;

	t = u1*v0 + w2;
	k = t >> 16;

	BREG = u0*v0 + w1 + k;
	AREG = (t << 16) + w3;       /* (*) */

	/* FIXME: This aint right, we need to do a 64 bit add no? 
	 * ie [AREG,BREG]+[CREG] */
	res = AREG + CREG;
	carry = ((UWORD) ((AREG & CREG) | ((AREG | CREG) & ~(res))) >> 31);
	AREG = res;
	BREG = BREG + carry;
	
	STACK2_RET(AREG, BREG, STYPE_DATA, STYPE_DATA);
#endif
}
#endif /* defined(TVM_HAVE_TWOWORD) || TVM_WORD_LENGTH == 4 */

/* 0x32 - 0x23 F2 - not - bitwise complement */
TVM_INSTRUCTION (ins_not)
{
	STACK_RET(~AREG, BREG, CREG, AREGt, BREGt, CREGt);
}

/* 0x33 - 0x23 F3 - xor - bitwise exclusive or */
TVM_INSTRUCTION (ins_xor)
{
	STACK2_RET(AREG ^ BREG, CREG, STYPE_DATA, CREGt);
}

#if defined(TVM_HAVE_TWOWORD) || TVM_WORD_LENGTH == 4
/* 0x35 - 0x23 F5 - lshr - long shift right */
TVM_INSTRUCTION (ins_lshr)
{
#ifdef TVM_HAVE_TWOWORD
	UTWOWORD value = (((UTWOWORD) ((UWORD) CREG)) << WORD_BITS) | ((UWORD) BREG);

	if (((UWORD) AREG) < TWOWORD_BITS) {
		value >>= ((UWORD) AREG);
		AREG = value & LONG_LO_MASK;
		BREG = value >> WORD_BITS;
	} else {
		AREG = 0;
		BREG = 0;
	}

	STACK2_RET(AREG, BREG, STYPE_DATA, STYPE_DATA);
#else
	int loop, bit;

	/* Reduce shift length to 64 if larger */
	if(AREG > 64)
	{
		AREG = 64;
	}
	
	for (loop = 0; loop < AREG; loop++)
	{
		if ((CREG & 0x00000001) == 0)
			bit = 0x00000000;
		else
			bit = 0x80000000;

		CREG = ((UWORD) CREG) >> 1;
		BREG = ((UWORD) BREG) >> 1;

		BREG = BREG | bit;
	}

	STACK2_RET(BREG, CREG, STYPE_DATA, STYPE_DATA);
#endif
}
#endif /* defined(TVM_HAVE_TWOWORD) || TVM_WORD_LENGTH == 4 */

#if defined(TVM_HAVE_TWOWORD) || TVM_WORD_LENGTH == 4
/* 0x36 - 0x23 F6 - lshl - long shift left */
TVM_INSTRUCTION (ins_lshl)
{
#ifdef TVM_HAVE_TWOWORD
	UTWOWORD value = (((UTWOWORD) ((UWORD) CREG)) << WORD_BITS) | ((UWORD) BREG);

	if (((UWORD) AREG) < TWOWORD_BITS) {
		value <<= ((UWORD) AREG);
		AREG = value & LONG_LO_MASK;
		BREG = value >> WORD_BITS;
	} else {
		AREG = 0;
		BREG = 0;
	}

	STACK2_RET(AREG, BREG, STYPE_DATA, STYPE_DATA);
#else
	int loop, bit;

	/* Reduce shift length to 64 if larger */
	if(AREG > 64)
	{
		AREG = 64;
	}
	
	for (loop = 0; loop < AREG; loop++)
	{
		if ((BREG & 0x80000000) == 0)
			bit = 0x00000000;
		else
			bit = 0x00000001;

		BREG = ((UWORD) BREG) << 1;
		CREG = ((UWORD) CREG) << 1;

		CREG = CREG | bit;
	}

	STACK2_RET(BREG, CREG, STYPE_DATA, STYPE_DATA);
#endif
}
#endif /* defined(TVM_HAVE_TWOWORD) || TVM_WORD_LENGTH == 4 */

/* 0x37 - 0x23 F7 - lsum - long sum - used in conjunction with ladd */
TVM_INSTRUCTION (ins_lsum)
{
	/* FIXME: Does the carry need to be calculated using the incomming carry? */
	/* carry algorithm from Hackers Delight p. 34 */
	WORD resnocarry = BREG + AREG;
	WORD result = resnocarry + (CREG & 1);
	WORD carry = ((UWORD) ((BREG & AREG) | ((BREG | AREG) & ~resnocarry)) >> (WORD_BITS - 1));

	STACK2_RET(result, carry, STYPE_DATA, STYPE_DATA);
}


/* 0x38 - 0x23 0xF8 - lsub - long subtract */
TVM_INSTRUCTION (ins_lsub)
{
	WORD result = BREG - AREG - (CREG & 1);

	/* Overflow detection from Hackers Delight p. 27 */
	if( ((UWORD) ((BREG ^ AREG) & (result ^ BREG))) >> (WORD_BITS - 1))
	{
		SET_ERROR_FLAG(EFLAG_INTOV);
	}

	STACK1_RET(result, STYPE_DATA);
}
	
/* 0x39 - 0x23 0xF9 - runp - run process */
TVM_INSTRUCTION (ins_runp)
{
	ADD_TO_QUEUE_ECTX_RET((WORDPTR)AREG);
}

/* 0x3B - 0x23 0xFB - sb - store byte */
TVM_INSTRUCTION (ins_sb)
{
	write_byte_and_type(ectx, (BYTEPTR)AREG, (BYTE)BREG, STYPE_DATA);

	STACK1_RET(CREG, CREGt);
}


/* 0x3C - 0x23 0xFC - gajw - general adjust workspace */
TVM_INSTRUCTION (ins_gajw)
{
	WORD tmp = (WORD)WPTR;

	WPTR = (WORDPTR)AREG;
	AREG = tmp;
	SET_AREGt (STYPE_WS);

	return ECTX_CONTINUE;
}


/****************************************************************************
 *              0x24 0xF_         0x24 0xF_         0x24 0xF_               *
 ****************************************************************************/

/* 0x40 - 0x24 0xF0 - shr - shift right (logical) */
TVM_INSTRUCTION (ins_shr)
{
	/* FIXME: I think that I am doing the right thing here, ie if AREG >
	 * numberofbiutsinword then the result is zero! However that could be wrong,
	 * due to sign bits? Not sure if they factor in here...
	 */
	/* FIXME: Not all platforms implement shifting in the same way. We need the
	 * instructions to be consistent across platforms though, so at the moment we
	 * are doing a check now which is redundant on some platforms! We could check
	 * for this, and not do it where it is not needed.
	 */
	/* STACK(((UWORD) ((UWORD) BREG) >> ((UWORD) AREG)), CREG, UNDEFINE(CREG)); */
	STACK2_RET(
			((unsigned) AREG >= WORD_BITS) ? 0 :
			((UWORD) ((UWORD) BREG) >> ((UWORD) AREG)), 
			CREG,
			STYPE_DATA,
			CREGt);
}

/* 0x41 - 0x24 0xF1 - shl - shift left (logical) */
TVM_INSTRUCTION (ins_shl)
{
	/* FIXME: Not all platforms implement shifting in the same way. We need the
	 * instructions to be consistent across platforms though, so at the moment we
	 * are doing a check now which is redundant on some platforms! We could check
	 * for this, and not do it where it is not needed.
	 */
	/* STACK(((UWORD) BREG) << ((UWORD) AREG), CREG, UNDEFINE(CREG)); */
	STACK2_RET(
			((unsigned) AREG >= WORD_BITS) ? 0 : ((UWORD) BREG) << ((UWORD) AREG),
			CREG,
			STYPE_DATA,
			CREGt);
}

/* 0x42 - 0x24 0xF2 - mint - minimum integer */
TVM_INSTRUCTION (ins_mint)
{
	STACK_RET(MIN_INT, AREG, BREG, STYPE_DATA, AREGt, BREGt);
}

/* 0x46 - 0x24 0xF6 - and - bitwise and */
TVM_INSTRUCTION (ins_and)
{
	STACK2_RET(AREG & BREG, CREG, STYPE_DATA, CREGt);
}

/* 0x4A - 0x24 0xFA - move - move message */
TVM_INSTRUCTION (ins_move)
{
	
	tvm_memcpy((BYTEPTR) BREG, (BYTEPTR) CREG, AREG);
	copy_type_shadow(ectx, (BYTEPTR) BREG, (BYTEPTR) CREG, AREG);
	UNDEFINE_STACK_RET();
}

/* 0x4B - 0x24 0xFB - or - or */
TVM_INSTRUCTION (ins_or)
{
	STACK2_RET(AREG | BREG, CREG, STYPE_DATA, CREGt);
}

/* 0x4C - 0x24 0xFC - csngl - check single */
TVM_INSTRUCTION (ins_csngl)
{
	if((AREG < 0 && BREG != -1) || (AREG >= 0 && BREG != 0))
	{
		SET_ERROR_FLAG(EFLAG_INTERR);
	}

	STACK2_RET(AREG, CREG, AREGt, CREGt);
}

/* 0x4D - 0x24 0xFD - ccnt1 - check count from 1 */
TVM_INSTRUCTION (ins_ccnt1)
{
	if((BREG == 0) || (((UWORD) BREG) > ((UWORD) AREG)))
	{
		SET_ERROR_FLAG(EFLAG_INTERR);
	}
	
	STACK2_RET(BREG, CREG, BREGt, CREGt);
}

/* 0x4F - 0x24 0xFF - ldiff - Long Difference */
TVM_INSTRUCTION (ins_ldiff)
{
	/* carry algorithm from Hackers Delight p. 35 */
	WORD resnocarry = BREG - AREG;
	WORD result = resnocarry - (CREG & 1);
	WORD equiv = (BREG & AREG) - (BREG | AREG) - 1;
	WORD carry = ((UWORD) ((~BREG & AREG) | (equiv & result)) >> (WORD_BITS - 1));

	STACK2_RET(result, carry, STYPE_DATA, STYPE_DATA);
}

/****************************************************************************
 *              0x25 0xF_         0x25 0xF_         0x25 0xF_               *
 ****************************************************************************/

/* 0x52 - 0x25 0xF2 - sum - sum */
TVM_INSTRUCTION (ins_sum)
{
	STACK2_RET(AREG + BREG, CREG, PICK_POINTER_TYPE (AREGt, BREGt), CREGt);
}

/* 0x53 - 0x25 0xF3 - mul - multiply */
TVM_INSTRUCTION (ins_mul)
{
	/* FIXME: Overflow detection does not seem to be so easy for signed
	 * multiplication. Shame we are not doing unsigned multiplication, where this
	 * is much simpler.
	 *
	 * Overflow detection for multiplication also looks like a quite expensive
	 * operation :( It would be nice if we could use the underlying processor for
	 * this, ie inspects its registers (if it has hi+lo) or if it has a status
	 * flag which indicates overflow on multiplication. I dont think this is
	 * possible without using assembly, which is unfortunately non portable.
	 *
	 * See Hackers Delight p. 29-32
	 */
	STACK2_RET(BREG * AREG, CREG, PICK_POINTER_TYPE (AREGt, BREGt), CREGt);
}

/* 0x55 - 0x25 0xF5 - stoperr - stop on error */
TVM_INSTRUCTION (ins_stoperr)
{
	if(ectx->eflags)
	{
		/* Undefine the whole stack */
		UNDEFINE_STACK();

		/* Store the instruction pointer at WPTR-1 */
		write_word(wordptr_minus(WPTR, 1), (WORD)IPTR);

		/* Run a new process */
		RUN_NEXT_ON_QUEUE_RET();
	}
	else
	{
		return ECTX_CONTINUE;
	}
}

/* 0x56 - 0x25 0xF6 - cword - check word */
TVM_INSTRUCTION (ins_cword)
{
	if((BREG >= AREG) || (BREG < -AREG))
	{
		SET_ERROR_FLAG(EFLAG_INTERR);
	}

	STACK2_RET(BREG, CREG, BREGt, CREGt);
}

/****************************************************************************
 *              0x27 0xF_         0x27 0xF_         0x27 0xF_               *
 ****************************************************************************/

/* 0x79 - 0x27 0xF9 - pop - pop top of stack */
TVM_INSTRUCTION (ins_pop)
{
	STACK2_RET(BREG, CREG, BREGt, CREGt);
}

/****************************************************************************
 *              0x2F 0xF_         0x27 0xF_         0x27 0xF_               *
 ****************************************************************************/

/* 0xFE - 0x2F 0xFE - ins_shutdown - application shutdown */
TVM_INSTRUCTION (ins_shutdown)
{
	/* This instruction and opcode do not exist on the transputer.
	 *
	 * We place this instruction at the top of the main stack frame,
	 * and use it to cleanly terminate the application on completion.
	 */
	return ECTX_SHUTDOWN;
}

