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
	STACK_RET(BREG, AREG, CREG);
}

/* 0x01 - 0xF1 - lb - load byte */
TVM_INSTRUCTION (ins_lb)
{
	STACK_RET((WORD)read_byte((BYTEPTR)AREG),BREG,CREG);
}

/* 0x02 - 0xF2 - bsub - byte subscript */
TVM_INSTRUCTION (ins_bsub)
{
	STACK_RET((WORD)byteptr_plus((BYTEPTR) AREG, BREG), CREG, UNDEFINE(CREG));
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
		//STACK(UNDEFINE(AREG), UNDEFINE(BREG), UNDEFINE(CREG));
		/* Run the next process */
		RUN_NEXT_ON_QUEUE_RET();
	}
}

/* 0x04 - 0xF4 - diff - difference */
TVM_INSTRUCTION (ins_diff)
{
	/* Unsigned subtract */
	STACK_RET((WORD)((UWORD) BREG) - ((UWORD) AREG), CREG, UNDEFINE(CREG));
}

/* 0x05 - 0xF5 - add - addition */
TVM_INSTRUCTION (ins_add)
{
	WORD result = BREG + AREG;

	/* Check for overflow, from Hackers Delight p. 27 */
	if( ((UWORD) (((result) ^ BREG) & ((result) ^ AREG)) >> (WORD_BITS - 1)) )
	{
		return ectx->set_error_flag(ectx, EFLAG_ADD);
	}

	STACK_RET(result, CREG, UNDEFINE(CREG));
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
	IPTR = (BYTEPTR)temp;

	/* FIXME: This is not the INMOS GCALL, it has been modified by fred (I
	 * presume) to store the IPTR in WS_TOP. The original instruction does NOT
	 * do this!!!! */
	WORKSPACE_SET(WPTR, WS_TOP, AREG);

	return ECTX_CONTINUE;
}

/* 0x08 - 0xF8 - prod - Unchecked Multiplication (product) */
TVM_INSTRUCTION (ins_prod)
{
	STACK_RET(AREG * BREG, CREG, UNDEFINE(CREG));
}

/* 0x09 - 0xF9 - gt - greater than */
TVM_INSTRUCTION (ins_gt)
{
	if(BREG > AREG)
	{
		STACK_RET(1, CREG, UNDEFINE(CREG));
	}
	else
	{
		STACK_RET(0, CREG, UNDEFINE(CREG));
	}
}

/* 0x0A - 0xFA - wsub - word subscript */
TVM_INSTRUCTION (ins_wsub)
{
	/* FIXME: Same check as for bsub */
	/*STACK((WORD)((WORD *) AREG) + BREG, CREG, UNDEFINE(CREG));*/
	STACK_RET((WORD)wordptr_plus((WORDPTR)AREG, BREG), CREG, UNDEFINE(CREG));
}

/* 0x0C - 0xFC - sub - subtract */
TVM_INSTRUCTION (ins_sub)
{
	WORD result = BREG - AREG;

	/* Overflow detection from Hackers Delight p. 27 */
	if( ((UWORD) ((BREG ^ AREG) & (result ^ BREG))) >> (WORD_BITS - 1))
	{
		ectx->set_error_flag(ectx, EFLAG_SUB);
	}

	STACK_RET(BREG - AREG, CREG, UNDEFINE(CREG));
}

/* 0x0D - 0xFD - startp - star process */
TVM_INSTRUCTION (ins_startp)
{
	tvm_add_to_queue((WORDPTR)AREG, byteptr_plus(IPTR, BREG));
	/* FIXME: Due to the different semantics of the C add_to_queue, and the
	 * soccam add to queue, (which we need to bring in sync) I have this line
	 * which is not currently present in soccam */
	STACK_RET(CREG, UNDEFINE(BREG), UNDEFINE(CREG));
}





/****************************************************************************
 *              0x21 0xF_         0x21 0xF_         0x21 0xF_               *
 ****************************************************************************/

/* 0x10 - 0x21 0xF0 - seterr - set error */
TVM_INSTRUCTION (ins_seterr)
{
	return ectx->set_error_flag(ectx, EFLAG_SETERR);
}

/* 0x13 - 0x21 0xF3 - csub0 - check subscript from 0 */
TVM_INSTRUCTION (ins_csub0)
{
	/* FIXME: The implementation of this is incorrect in soccam, the
	 * error flag can be cleared in soccam, this is wrong.
	 */
	if(((UWORD) BREG) >= ((UWORD) AREG))
	{
		return ectx->set_error_flag(ectx, EFLAG_CSUB0);
	}
	STACK_RET(BREG, CREG, UNDEFINE(CREG));
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
#define SIGN(x) ((x>0)-(x<0))
	WORD result = ((WORD) BREG) + ((WORD) AREG) + ((WORD) CREG & 1);

	/* Check for overflow, from Hackers Delight p. 27 */
	if( ((UWORD) (((result) ^ BREG) & ((result) ^ AREG)) >> (WORD_BITS - 1)) )
	{
		return ectx->set_error_flag(ectx, EFLAG_LADD);
	}

	STACK_RET(result, UNDEFINE(BREG), UNDEFINE(CREG));
}

/* 0x19 - 0x21 0xF9 - norm - normalise */
TVM_INSTRUCTION (ins_norm)
{
	/*printf("norm\n");*/
	CREG = 0;
	if(AREG == 0 && BREG == 0)
	{
		/*STACK(AREG, BREG, 2 * TVM_WORD_LENGTH);*/
		STACK(AREG, BREG, 64);
	}
	else
	{
		/* FIXME: TVM_WORD_LENGTH */
		while(!(BREG & 0x80000000))
		{
			BREG = BREG << 1;
			if(AREG & 0x80000000)
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

#if TVM_WORD_LENGTH == 4
/* FIXME: We should have an INLINE define, that can be turned on and off */
#ifdef WIN32
static int nlz(unsigned x) {
#else
static inline int nlz(unsigned x) {
#endif
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

#if TVM_WORD_LENGTH >= 4
/* 0x1A - 0x21 0xFA LDIV - divides a double length value in the 
 * reg pair CB by a single length value in A */
TVM_INSTRUCTION (ins_ldiv)
{
	/*
	   unsigned divlu2(unsigned u1, unsigned u0, unsigned v,
	   unsigned *r) {

*/
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
		return ectx->set_error_flag(ectx, EFLAG_LDIV);
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

	STACK_RET(AREG, BREG, UNDEFINE(CREG));
#if 0
	unsigned long long result, tmp;
	/* FIXME: UNTESTED */	
	/* Maybe the below should not be cast to unsigned word.... */
	if(((UWORD) CREG) >= ((UWORD) AREG ))
	{
		set_error_flag(ectx, );
	} 
	tmp = (unsigned int)CREG;
	result = (tmp << WORD_BITS) + ((unsigned int)BREG);
	STACK( result / AREG, result % AREG, UNDEFINE(CREG));
#endif
}
#endif /* TVM_WORD_LENGTH >= 4 */

/* FIXME: This instruction has not been implemented in soccam!!! */
/* 0x1B - 0x21 0xFB - ldpi - load pointer to instruction */
TVM_INSTRUCTION (ins_ldpi)
{
	/*STACK((WORD)(IPTR + AREG), BREG, CREG);*/
	STACK_RET((WORD)byteptr_plus(IPTR, AREG), BREG, CREG);
}

/* 0x1D - 0x21 0xFD - xdble - extend to double */
TVM_INSTRUCTION (ins_xdble)
{
	if(AREG < 0)
	{
		STACK_RET(AREG, -1, BREG);
	}
	else
	{
		/* AREG >= 0 */
		STACK_RET(AREG, 0, BREG);
	}
}

/* 0x1F - 0x21 0xFF - rem - (integer) remainder */
TVM_INSTRUCTION (ins_rem)
{
	if((AREG == 0) || ((AREG == -1) && (BREG == MIN_INT)))
	{
		return ectx->set_error_flag(ectx, EFLAG_REM);
	}
	else
	{
		STACK_RET ((BREG % AREG), CREG, UNDEFINE(CREG));	
	}
}


/****************************************************************************
 *              0x22 0xF_         0x22 0xF_         0x22 0xF_               *
 ****************************************************************************/

/* 0x20 - 0x22 0xF0 - ret - return */
TVM_INSTRUCTION (ins_ret)
{
	BYTEPTR ret_addr = (BYTEPTR)read_word(WPTR);

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
	STACK_RET(AREG, BREG, UNDEFINE(CREG));
}

/* 0x2C - 0x22 0xFC - div - divide */
TVM_INSTRUCTION (ins_div)
{
	if((AREG == 0) || ((AREG == -1) && (BREG == MIN_INT)))
	{
		return ectx->set_error_flag(ectx, EFLAG_DIV);
	}
	else
	{
		STACK_RET(BREG / AREG, CREG, UNDEFINE(CREG));
	}
}
 




/****************************************************************************
 *              0x23 0xF_         0x23 0xF_         0x23 0xF_               *
 ****************************************************************************/

#if TVM_WORD_LENGTH >= 4
/* 0x31 - 0x23 F1 - lmul - long multiply */
TVM_INSTRUCTION (ins_lmul)
{
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

	/*
	printf("t:  0x%08x, k:  0x%08x w1: 0x%08x w2: 0x%08x \n", t, k, w1, w2);
	printf("w3: 0x%08x, u0: 0x%08x u1: 0x%08x v0: 0x%08x \n", w3, u0, u1, v0);
	*/

	BREG = u0*v0 + w1 + k;
	AREG = (t << 16) + w3;       /* (*) */
	/* w[1] = u*v;                  // Alternative. */
	/*AREG = u*v; */

	/* FIXME: This aint right, we need to do a 64 bit add no? 
	 * ie [AREG,BREG]+[CREG] */
	res = AREG + CREG;
	carry = ((UWORD) ((AREG & CREG) | ((AREG | CREG) & ~(res))) >> 31);
	AREG = res;
	BREG = BREG + carry;
	
	STACK_RET(AREG, BREG, UNDEFINE(CREG));
#if 0
	/*FIXME: not tested...*/
	/*FIXME: the hi calculation has a nasty warning that the thing we are sticking in it does not fit. */
	/* Multiplies AREG * BREG and adds CREG (not sure why)
	 * then gets the hi end of the multiplication and sticks it in hi
	 * and gets the lo end of the result and stick it in lo.  lo goes 
	 * in AREG and hi goes in BREG */
	unsigned int hi, lo;
	unsigned long long result, tmp;
	result = BREG;
	tmp = (unsigned int)AREG;
	result = (result * tmp) + CREG;
	tmp = (result >> WORD_BITS);
	hi = (int) tmp;
	lo = (result & LONG_LO_MASK );
	STACK( lo, hi, UNDEFINE(CREG));
#endif
}
#endif /* TVM_WORD_LENGTH >= 4 */

/* 0x32 - 0x23 F2 - not - bitwise complement */
TVM_INSTRUCTION (ins_not)
{
	STACK_RET(~AREG, BREG, CREG);
}

/* 0x33 - 0x23 F3 - xor - bitwise exclusive or */
TVM_INSTRUCTION (ins_xor)
{
	STACK_RET(AREG ^ BREG, CREG, UNDEFINE(CREG));
}

#if TVM_WORD_LENGTH >= 4
/* my god the two below are ugly! */
/* FIXME: Things like this can be implemented very efficiently (and easily!) in
   assembler for little platforms like the AVR. */
/* 0x35 - 0x23 F5 - lshr - long shift right */
TVM_INSTRUCTION (ins_lshr)
{
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

	STACK_RET(BREG, CREG, UNDEFINE(CREG));
#if 0
	/*FIXME: not tested...*/
	/* lo does not need to be long, bits rightshifted out of lo are supposed to 
	* fall out into the void anyway */
	unsigned int lo;  
	unsigned long long hi;
	if(AREG <= 2* WORD_BITS  && AREG >= 0 )
        {
                hi = (unsigned int) CREG;
                hi = hi << (WORD_BITS - AREG); /*make hi really be the 'hi' and then shift it.*/
                /* shift lo - done on seperage line coz shifting BREG directly buggered up
		 * coz it's unsigned, I think.  Maybe not though.  This seems to work, and
		 * shifting BREG directly didn't.  */
                lo = BREG;
                lo = lo >> ((UWORD) AREG);  
                lo += hi;  /*add what 'overflowed' from lo to hi.*/
                hi = hi >> WORD_BITS; /*set hi back to what it would have been in a real reg.*/
                STACK((WORD)lo,(WORD) hi, 0);
        }
#endif
}
#endif /* TVM_WORD_LENGTH >= 4 */

#if TVM_WORD_LENGTH >= 4
/* 0x36 - 0x23 F6 - lshl - long shift left */
TVM_INSTRUCTION (ins_lshl)
{
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

	STACK_RET(BREG, CREG, UNDEFINE(CREG));
#if 0
	/* From: Hackers Delight
	 * y1 = x1 << n | x0 >> (32 -n) | x0 << (n - 32)
	 * y0 = x0 << n
	 */
	UWORD hi, lo;

	/* 0 <= AREG <= 2 * wordlength */
	/* I am assuming that shift by any value out of this range is going to result
	 * in all zeros
	 * FIXME: do we need to do this test, or is the line in the instruciton
	 * breakdown in the compiler writers guide an assertion that AREG is always
	 * going to be that value?
	 */
	if(((UWORD) AREG >= 2 * TVM_WORD_LENGTH))
	{
		STACK(0, 0, UNDEFINE(CREG));
	}
	else
	{
		hi = ((UWORD) BREG << (UWORD) AREG) | 
			   ((UWORD) CREG >> (32 - (UWORD) AREG)) | 
				 ((UWORD) CREG << ((UWORD) AREG - 32));
		lo = (UWORD) CREG << AREG;
		STACK(hi, lo, UNDEFINE(CREG));
	}
#endif
#if 0
	/*FIXME: not tested...*/
	/* hi does not need to be long, bits leftshifted out of hi are supposed to 
	* fall out into the void anyway */
	unsigned int hi;  
	unsigned long long lo;
	if(AREG <= 2* WORD_BITS  && AREG >= 0 )
	{
		hi = ((unsigned int)CREG) << AREG; /*shift hi*/
		/* shift lo - if BREG is shifted directly then you get the result of
		 * a shifted int put in a long long.  Not what we want... shift the value
		 * in the long long to get what we want (doesn't overflow).  Probably not
		 * very efficient.*/
		lo = BREG; 
		lo = lo << AREG;  
		hi += lo >> WORD_BITS;  /*add what 'overflowed' from lo to hi.*/
		STACK((WORD)lo, (WORD)hi, UNDEFINE(CREG));
	} 
#endif
}
#endif /* TVM_WORD_LENGTH >= 4 */

/* 0x37 - 0x23 F7 - lsum - long sum - used in conjunction with ladd */
TVM_INSTRUCTION (ins_lsum)
{
	/* FIXME: Does the carry need to be calculated using the incomming carry? */
	/* carry algorithm from Hackers Delight p. 34 */
	WORD resnocarry = BREG + AREG;
	WORD result = resnocarry + (CREG & 1);
	WORD carry = ((UWORD) ((BREG & AREG) | ((BREG | AREG) & ~resnocarry)) >> (WORD_BITS - 1));

	STACK_RET(result, carry, UNDEFINE(CREG));
}


/* 0x38 - 0x23 0xF8 - lsub - long subtract */
TVM_INSTRUCTION (ins_lsub)
{
	WORD result = BREG - AREG - (CREG & 1);

	/* Overflow detection from Hackers Delight p. 27 */
	if( ((UWORD) ((BREG ^ AREG) & (result ^ BREG))) >> (WORD_BITS - 1))
	{
		return ectx->set_error_flag(ectx, EFLAG_LSUB);
	}

	STACK_RET(result, UNDEFINE(CREG), UNDEFINE(BREG));
}
	
/* 0x39 - 0x23 0xF9 - runp - run process */
TVM_INSTRUCTION (ins_runp)
{
	tvm_just_add_to_queue((WORDPTR)AREG);
	return ECTX_CONTINUE;
}

/* 0x3B - 0x23 0xFB - sb - store byte */
TVM_INSTRUCTION (ins_sb)
{
	write_byte((BYTEPTR)AREG, (BYTE)BREG);

	STACK_RET(CREG, UNDEFINE(BREG), UNDEFINE(CREG));
}


/* 0x3C - 0x23 0xFC - gajw - general adjust workspace */
TVM_INSTRUCTION (ins_gajw)
{
	WORD tmp = (WORD)WPTR;

	WPTR = (WORDPTR)AREG;
	AREG = tmp;

	return ECTX_CONTINUE;
}

/* WARNING: saveh as implemented here does not work as described in the
 * instruction set manual (p. 145), but rather works as described here:
 *     saveh       #3E         Save queue registers
 *   This instruction saves the queue registers (assuming that there is no
 *   priority, and therefore only one set of queue registers). It saves FPTR,
 *   BPTR, and TPTR.
 *   This instruction could be modified in the future to take an argument in
 *   a unused register, to enable the possibility of specifying a priority level
 *   (if the transterpreter is running with different priority levels).
 *     Areg' = Breg
 *     Breg' = Creg
 *     Creg' = undefined
 *     Iptr' = NextInst
 *     Mem' = Mem (+) { (Index Areg 0) -> Fptr,
 *                      (Index Areg 1) -> Bptr,
 *                      (Index Areg 2) -> Tptr }
 *   This differs from the original instruction which saves only Fptr and Bptr.
 *   In the transterpreter it is also neccesary to save the Tptr, as this is
 *   stored in a 'register' and not in a special memory location as on the
 *   original Transputers. The original instruction also dealt with the high
 *   priority registers, we have no priority, so this is not applicable.
 */
/* 0x3E - 0x23 0xFE - saveh - Save queue registers */
TVM_INSTRUCTION (ins_saveh)
{
	write_word(wordptr_plus((WORDPTR) AREG, 0), (WORD) FPTR);
	write_word(wordptr_plus((WORDPTR) AREG, 1), (WORD) BPTR);
	write_word(wordptr_plus((WORDPTR) AREG, 2), (WORD) TPTR);

	/* printf("fptr: 0x%x; bptr: 0x%x; TPTR: 0x%x (WPTR: 0x%x; IPTR: 0x%x)\n", FPTR, BPTR, TPTR[pri], WPTR, IPTR); */

	/* Bump the stack */
	STACK_RET(BREG, CREG, UNDEFINE(CREG));
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
	STACK_RET(
			((unsigned) AREG >= WORD_BITS) ? 0 :
			((UWORD) ((UWORD) BREG) >> ((UWORD) AREG)), 
			CREG, UNDEFINE(CREG));
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
	STACK_RET(
			((unsigned) AREG >= WORD_BITS) ? 0 : ((UWORD) BREG) << ((UWORD) AREG),
			CREG, UNDEFINE(CREG));
}

/* 0x42 - 0x24 0xF2 - mint - minimum integer */
TVM_INSTRUCTION (ins_mint)
{
	STACK_RET(MIN_INT, AREG, BREG);
}

/* 0x46 - 0x24 0xF6 - and - bitwise and */
TVM_INSTRUCTION (ins_and)
{
	STACK_RET(AREG & BREG, CREG, UNDEFINE(CREG));
}

/* 0x4A - 0x24 0xFA - move - move message */
TVM_INSTRUCTION (ins_move)
{
	
	tvm_copy_data((BYTEPTR) BREG, (BYTEPTR) CREG, AREG);
	UNDEFINE_STACK_RET();
}

/* 0x4B - 0x24 0xFB - or - or */
TVM_INSTRUCTION (ins_or)
{
	STACK_RET(AREG | BREG, CREG, UNDEFINE(CREG));
}

/* 0x4C - 0x24 0xFC - csngl - check single */
TVM_INSTRUCTION (ins_csngl)
{
	if((AREG < 0 && BREG != -1) || (AREG >= 0 && BREG != 0))
	{
		return ectx->set_error_flag(ectx, EFLAG_CSNGL);
	}

	STACK_RET(AREG, CREG, UNDEFINE(CREG));
}

/* 0x4D - 0x24 0xFD - ccnt1 - check count from 1 */
TVM_INSTRUCTION (ins_ccnt1)
{
	if((BREG == 0) || (((UWORD) BREG) > ((UWORD) AREG)))
	{
		return ectx->set_error_flag(ectx, EFLAG_CCNT1);
	}
	
	STACK_RET(BREG, CREG, UNDEFINE(CREG));
}

/* 0x4F - 0x24 0xFF - ldiff - Long Difference */
TVM_INSTRUCTION (ins_ldiff)
{
	/* carry algorithm from Hackers Delight p. 35 */
	WORD resnocarry = BREG - AREG;
	WORD result = resnocarry - (CREG & 1);
	WORD equiv = (BREG & AREG) - (BREG | AREG) - 1;
	WORD carry = ((UWORD) ((~BREG & AREG) | (equiv & result)) >> (WORD_BITS - 1));

	STACK_RET(result, carry, UNDEFINE(CREG));
}

/****************************************************************************
 *              0x25 0xF_         0x25 0xF_         0x25 0xF_               *
 ****************************************************************************/

/* 0x52 - 0x25 0xF2 - sum - sum */
TVM_INSTRUCTION (ins_sum)
{
	STACK_RET(AREG + BREG, CREG, UNDEFINE(CREG));
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
	STACK_RET(BREG * AREG, CREG, UNDEFINE(CREG));
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
		return ectx->set_error_flag(ectx, EFLAG_CWORD);
	}

	STACK_RET(BREG, CREG, UNDEFINE(CREG));
}

/****************************************************************************
 *              0x27 0xF_         0x27 0xF_         0x27 0xF_               *
 ****************************************************************************/

/* 0x79 - 0x27 0xF9 - pop - pop top of stack */
TVM_INSTRUCTION (ins_pop)
{
	STACK_RET(BREG, CREG, UNDEFINE(CREG));
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

