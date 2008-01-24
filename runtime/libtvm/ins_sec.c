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

#include "transputer.h"
#include "instructions.h"
#include "interpreter.h"
#include "scheduler.h"
#include "mem.h"
#include "ins_sec.h"


/****************************************************************************
 *                   0xF_              0xF_              0xF_               *
 ****************************************************************************/

/* 0x00 - 0xF0 - rev - reverse */
TVM_INSTRUCTION void ins_rev(void)
{
	STACK(breg, areg, creg);
}

/* 0x01 - 0xF1 - lb - load byte */
TVM_INSTRUCTION void ins_lb(void)
{
  STACK((WORD)read_byte((BPOOTER)areg),breg,creg);
}

/* 0x02 - 0xF2 - bsub - byte subscript */
TVM_INSTRUCTION void ins_bsub(void)
{
	/* FIXME: Does it make sense to implement this like so: 
	 * ie using the (BYTE *) to ensure that things are incremented by
	 * the correct amount? I think so.*/
	/* STACK((WORD)((BYTE *) areg) + breg, creg, UNDEFINE(creg)); */
	STACK((WORD)bpooter_plus((BPOOTER) areg, breg), creg, UNDEFINE(creg));
}

/* 0x03 - 0xF3 - endp - end process */
TVM_INSTRUCTION void ins_endp(void)
{
	/* Check the process count */
	/* if(((WORD *)areg)[1] == 1) */
	if(read_mem(pooter_plus((POOTER)areg, 1)) == 1) 
	{
		/* No more child processes, continue as the parent process */

		/* Set the process count to zero */
		/* ((WORD *)areg)[1] = 0; */
		write_mem(pooter_plus((POOTER)areg, 1), 0);
		/* Get the resume address from the workspace */
		/* iptr = (BYTE *)((WORD *)areg)[0]; */
		iptr = (BPOOTER)read_mem((POOTER)areg);
		/* The areg becomes the new wptr */
		wptr = (POOTER)areg;
		/* The entire stack becomes undefined */
		//STACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));
	}
	else
	{
		/* Terminate current process, and reschedule another from the queue */

		/* Subtract one from the process count */
		/*((WORD *)areg)[1] = ((WORD *)areg)[1] - 1;*/
		write_mem(pooter_plus((POOTER)areg, 1), 
				read_mem(pooter_plus((POOTER)areg, 1)) - 1);
		/* The entire stack becomes undefined */
		//STACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));
		/* Run the next process */
		iptr = run_next_on_queue();
	}
}

/* 0x04 - 0xF4 - diff - difference */
TVM_INSTRUCTION void ins_diff(void)
{
	/* Unsigned subtract */
	STACK((WORD)((UWORD) breg) - ((UWORD) areg), creg, UNDEFINE(creg));
}

/* 0x05 - 0xF5 - add - addition */
TVM_INSTRUCTION void ins_add(void)
{
	WORD result = breg + areg;

	/* Check for overflow, from Hackers Delight p. 27 */
	if( ((UWORD) (((result) ^ breg) & ((result) ^ areg)) >> (WORDSIZE_BITS - 1)) )
	{
		set_error_flag(EFLAG_ADD);
	}

	STACK(result, creg, UNDEFINE(creg));
}

/* 0x06 - 0xF6 - gcall - general call */
TVM_INSTRUCTION void ins_gcall(void)
{
	WORD temp;

	/* FIXME: this is just an exchange of two registers... whats the word on the
	 * stack macro in places like this?
	 */
	temp = areg;
	areg = (WORD)iptr;
	iptr = (BPOOTER)temp;

	/* FIXME: This is not the INMOS GCALL, it has been modified by fred (I
	 * presume) to store the IPTR in WS_TOP. The original instruction does NOT
	 * do this!!!! */
	WORKSPACE_SET(wptr, WS_TOP, areg);
}

/* 0x08 - 0xF8 - prod - Unchecked Multiplication (product) */
TVM_INSTRUCTION void ins_prod(void)
{
	STACK(areg * breg, creg, UNDEFINE(creg));
}

/* 0x09 - 0xF9 - gt - greater than */
TVM_INSTRUCTION void ins_gt(void)
{
	if(breg > areg)
	{
		STACK(1, creg, UNDEFINE(creg));
	}
	else
	{
		STACK(0, creg, UNDEFINE(creg));
	}
}

/* 0x0A - 0xFA - wsub - word subscript */
TVM_INSTRUCTION void ins_wsub(void)
{
	/* FIXME: Same check as for bsub */
	/*STACK((WORD)((WORD *) areg) + breg, creg, UNDEFINE(creg));*/
	STACK((WORD)pooter_plus((POOTER)areg, breg), creg, UNDEFINE(creg));
}

/* 0x0C - 0xFC - sub - subtract */
TVM_INSTRUCTION void ins_sub(void)
{
	WORD result = breg - areg;

	/* Overflow detection from Hackers Delight p. 27 */
	if( ((UWORD) ((breg ^ areg) & (result ^ breg))) >> (WORDSIZE_BITS - 1))
	{
		set_error_flag(EFLAG_SUB);
	}

	STACK(breg - areg, creg, UNDEFINE(creg));
}

/* 0x0D - 0xFD - startp - star process */
TVM_INSTRUCTION void ins_startp(void)
{
	add_to_queue(areg, ((WORD) iptr) + breg);
	/* FIXME: Due to the different semantics of the C add_to_queue, and the
	 * soccam add to queue, (which we need to bring in sync) I have this line
	 * which is not currently present in soccam */
	STACK(creg, UNDEFINE(breg), UNDEFINE(creg));
}





/****************************************************************************
 *              0x21 0xF_         0x21 0xF_         0x21 0xF_               *
 ****************************************************************************/

/* 0x10 - 0x21 0xF0 - seterr - set error */
TVM_INSTRUCTION void ins_seterr(void)
{
	set_error_flag(EFLAG_SETERR);
}

/* 0x13 - 0x21 0xF3 - csub0 - check subscript from 0 */
TVM_INSTRUCTION void ins_csub0(void)
{
	/* FIXME: The implementation of this is incorrect in soccam, the
	 * error flag can be cleared in soccam, this is wrong.
	 */
	if(((UWORD) breg) >= ((UWORD) areg))
	{
		set_error_flag(EFLAG_CSUB0);
	}
	STACK(breg, creg, UNDEFINE(creg));
}

/* 0x15 - 0x21 0xF5 - stopp - stop process */
TVM_INSTRUCTION void ins_stopp(void)
{
	WORKSPACE_SET((POOTER) wptr, WS_IPTR, (WORD) iptr);

	iptr = run_next_on_queue();
}
	
/* 0x16 - 0x21 0xF6 LADD - long addition  - used in conjunction with lsum*/
TVM_INSTRUCTION void ins_ladd(void)
{
#define SIGN(x) ((x>0)-(x<0))
	WORD result = ((WORD) breg) + ((WORD) areg) + ((WORD) creg & 1);

	/* Check for overflow, from Hackers Delight p. 27 */
	if( ((UWORD) (((result) ^ breg) & ((result) ^ areg)) >> (WORDSIZE_BITS - 1)) )
	{
		set_error_flag(EFLAG_LADD);
	}

	STACK(result, UNDEFINE(breg), UNDEFINE(creg));
}

/* 0x19 - 0x21 0xF9 - norm - normalise */
TVM_INSTRUCTION void ins_norm(void)
{
	/*printf("norm\n");*/
	creg = 0;
	if(areg == 0 && breg == 0)
	{
		/*STACK(areg, breg, 2 * WORDLENGTH);*/
		STACK(areg, breg, 64);
	}
	else
	{
		/* FIXME: WORDLENGTH */
		while(!(breg & 0x80000000))
		{
			breg = breg << 1;
			if(areg & 0x80000000)
			{
				breg = breg | 1;
			}
			areg = areg << 1;
			++creg;
		}
		/* STACK(areg, breg, creg) */
	}
}

#if WORDLENGTH == 4
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

/* 0x1A - 0x21 0xFA LDIV - divides a double length value in the 
 * reg pair CB by a single length value in A */
TVM_INSTRUCTION void ins_ldiv(void)
{
#if WORDLENGTH != 4
	/* FIXME: Does not work with BCC32 */
	ins_not_implemented();
#else
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

	unsigned u1 = creg;
	unsigned u0 = breg;
	unsigned v  = areg;

	/*
  if (u1 >= v) {            // If overflow, set rem.
    if (r != NULL)         // to an impossible value,
      *r = 0xFFFFFFFF;    // and return the largest
    return 0xFFFFFFFF;}    // possible quotient.
	*/
	if(u1 >= v) /* creg >= areg */
	{
		set_error_flag(EFLAG_LDIV);
		return;
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
	breg = (un21*b + un0 - q0*v) >> s;
	areg = q1*b + q0;
#endif
#if 0
	unsigned long long result, tmp;
	/* FIXME: UNTESTED */	
	/* Maybe the below should not be cast to unsigned word.... */
	if(((UWORD) creg) >= ((UWORD) areg ))
	{
		set_error_flag();
	} 
	tmp = (unsigned int)creg;
	result = (tmp << WORDSIZE_BITS) + ((unsigned int)breg);
	STACK( result / areg, result % areg, UNDEFINE(creg));
#endif
}

/* FIXME: This instruction has not been implemented in soccam!!! */
/* 0x1B - 0x21 0xFB - ldpi - load pointer to instruction */
TVM_INSTRUCTION void ins_ldpi(void)
{
	/*STACK((WORD)(iptr + areg), breg, creg);*/
	STACK((WORD)bpooter_plus(iptr, areg), breg, creg);
}

/* 0x1D - 0x21 0xFD - xdble - extend to double */
TVM_INSTRUCTION void ins_xdble(void)
{
	if(areg < 0)
	{
		STACK(areg, -1, breg);
	}
	else
	{
		/* areg >= 0 */
		STACK(areg, 0, breg);
	}
}

/* 0x1F - 0x21 0xFF - rem - (integer) remainder */
TVM_INSTRUCTION void ins_rem(void)
{
	if((areg == 0) || ((areg == -1) && (breg == MIN_INT)))
	{
		STACK(UNDEFINE(areg), creg, UNDEFINE(creg));
		set_error_flag(EFLAG_REM);
	}
	else
	{
	  STACK((breg % areg), creg, UNDEFINE(creg));	
	}
}





/****************************************************************************
 *              0x22 0xF_         0x22 0xF_         0x22 0xF_               *
 ****************************************************************************/

/* 0x20 - 0x22 0xF0 - ret - return */
TVM_INSTRUCTION void ins_ret(void)
{
	BPOOTER ret_addr = (BPOOTER)read_mem(wptr);

	iptr = ret_addr;
	wptr = pooter_plus(wptr, 4);
}

/* 0x21 - 0x22 0xF1 - lend - loop end */
TVM_INSTRUCTION void ins_lend(void)
{
	/* FIXME: I dont think that the soccam version of this sets creg to 
	 * undefined */
	/* FIXME: WARNING:
	 * If memory is not initialised to zero by the runtime, we need to
	 * initialise it to zero for this to work. The occam compiler seems to
	 * assume that memory is all zeros before the program runs. eg: The scheme
	 * interpreter inits memory to <void>.
	 */
	if(read_mem(pooter_plus((POOTER)breg, 1)) > 1)
	{
		/* FIXME: This is done the other way around in soccam, I think I followed
		 * the order it was done in the book... check though */
		write_mem((POOTER)breg, read_mem((POOTER)breg) + 1);
		write_mem(pooter_plus((POOTER)breg, 1), 
				read_mem(pooter_plus((POOTER)breg, 1)) - 1);

		iptr = bpooter_minus(iptr, areg);
	} 
	else 
	{
		/* Decrement the counter */
		write_mem(pooter_plus((POOTER)breg, 1), 
				read_mem(pooter_plus((POOTER)breg, 1)) - 1);
	 }			
		
	/* FIXME: I dont think the soccam instruction set the stack properly, it
	 * should use the stack macro, it dont (this is related to the first comment
	 * in this function  */
	STACK(areg, breg, UNDEFINE(creg));
}

/* 0x2C - 0x22 0xFC - div - divide */
TVM_INSTRUCTION void ins_div(void)
{
	if((areg == 0) || ((areg == -1) && (breg == MIN_INT)))
	{
		STACK(UNDEFINE(areg), creg, UNDEFINE(creg));
		set_error_flag(EFLAG_DIV);
	}
	else
	{
		STACK(breg / areg, creg, UNDEFINE(creg));
	}
}





/****************************************************************************
 *              0x23 0xF_         0x23 0xF_         0x23 0xF_               *
 ****************************************************************************/

/* 0x31 - 0x23 F1 - lmul - long multiply */
TVM_INSTRUCTION void ins_lmul(void)
{
#if WORDLENGTH != 4
	/* FIXME: Does not work with BCC32 */
	ins_not_implemented();
#else
	unsigned int u = breg;
	unsigned int v = areg;

	unsigned int u0, u1, v0, v1, k, t;
	unsigned int w1, w2, w3;

	WORD res, carry; /* For adding the carry (creg) later */

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

	breg = u0*v0 + w1 + k;
	areg = (t << 16) + w3;       /* (*) */
	/* w[1] = u*v;                  // Alternative. */
	/*areg = u*v; */

	/* FIXME: This aint right, we need to do a 64 bit add no? 
	 * ie [areg,breg]+[creg] */
	res = areg + creg;
	carry = ((UWORD) ((areg & creg) | ((areg | creg) & ~(res))) >> 31);
	areg = res;
	breg = breg + carry;
#endif
#if 0
	/*FIXME: not tested...*/
	/*FIXME: the hi calculation has a nasty warning that the thing we are sticking in it does not fit. */
	/* Multiplies areg * breg and adds creg (not sure why)
	 * then gets the hi end of the multiplication and sticks it in hi
	 * and gets the lo end of the result and stick it in lo.  lo goes 
	 * in areg and hi goes in breg */
	unsigned int hi, lo;
	unsigned long long result, tmp;
	result = breg;
	tmp = (unsigned int)areg;
	result = (result * tmp) + creg;
	tmp = (result >> WORDSIZE_BITS);
	hi = (int) tmp;
	lo = (result & LONG_LO_MASK );
	STACK( lo, hi, UNDEFINE(creg));
#endif
}

/* 0x32 - 0x23 F2 - not - bitwise complement */
TVM_INSTRUCTION void ins_not(void)
{
	STACK(~areg, breg, creg);
}

/* 0x33 - 0x23 F3 - xor - bitwise exclusive or */
TVM_INSTRUCTION void ins_xor(void)
{
	STACK(areg ^ breg, creg, UNDEFINE(creg));
}

/* my god the two below are ugly! */
/* FIXME: Things like this can be implemented very efficiently (and easily!) in
   assembler for little platforms like the AVR. */
/* 0x35 - 0x23 F5 - lshr - long shift right */
TVM_INSTRUCTION void ins_lshr(void)
{
#if WORDLENGTH != 4
	/* FIXME: Does not work with BCC32 */
	ins_not_implemented();
#else
	int loop, bit;

	/* Reduce shift length to 64 if larger */
	if(areg > 64)
	{
		areg = 64;
	}
	
	for (loop = 0; loop < areg; loop++)
	{
		if ((creg & 0x00000001) == 0)
			bit = 0x00000000;
		else
			bit = 0x80000000;

		creg = ((UWORD) creg) >> 1;
		breg = ((UWORD) breg) >> 1;

		breg = breg | bit;
	}

	areg = breg;
	breg = creg;

	return;
#if 0
	/*FIXME: not tested...*/
	/* lo does not need to be long, bits rightshifted out of lo are supposed to 
	* fall out into the void anyway */
	unsigned int lo;  
	unsigned long long hi;
	if(areg <= 2* WORDSIZE_BITS  && areg >= 0 )
        {
                hi = (unsigned int) creg;
                hi = hi << (WORDSIZE_BITS - areg); /*make hi really be the 'hi' and then shift it.*/
                /* shift lo - done on seperage line coz shifting breg directly buggered up
		 * coz it's unsigned, I think.  Maybe not though.  This seems to work, and
		 * shifting breg directly didn't.  */
                lo = breg;
                lo = lo >> ((UWORD) areg);  
                lo += hi;  /*add what 'overflowed' from lo to hi.*/
                hi = hi >> WORDSIZE_BITS; /*set hi back to what it would have been in a real reg.*/
                STACK((WORD)lo,(WORD) hi, 0);
        }
#endif
#endif
}

/* 0x36 - 0x23 F6 - lshl - long shift left */
TVM_INSTRUCTION void ins_lshl(void)
{
#if WORDLENGTH != 4
	/* FIXME: Does not work with BCC32 */
	ins_not_implemented();
#else
	int loop, bit;

	/* Reduce shift length to 64 if larger */
	if(areg > 64)
	{
		areg = 64;
	}
	
	for (loop = 0; loop < areg; loop++)
	{
		if ((breg & 0x80000000) == 0)
			bit = 0x00000000;
		else
			bit = 0x00000001;

		breg = ((UWORD) breg) << 1;
		creg = ((UWORD) creg) << 1;

		creg = creg | bit;
	}

	areg = breg;
	breg = creg;

	return;

#if 0
	/* From: Hackers Delight
	 * y1 = x1 << n | x0 >> (32 -n) | x0 << (n - 32)
	 * y0 = x0 << n
	 */
	UWORD hi, lo;

	/* 0 <= areg <= 2 * wordlength */
	/* I am assuming that shift by any value out of this range is going to result
	 * in all zeros
	 * FIXME: do we need to do this test, or is the line in the instruciton
	 * breakdown in the compiler writers guide an assertion that areg is always
	 * going to be that value?
	 */
	if(((UWORD) areg >= 2 * WORDLENGTH))
	{
		STACK(0, 0, UNDEFINE(creg));
	}
	else
	{
		hi = ((UWORD) breg << (UWORD) areg) | 
			   ((UWORD) creg >> (32 - (UWORD) areg)) | 
				 ((UWORD) creg << ((UWORD) areg - 32));
		lo = (UWORD) creg << areg;
		STACK(hi, lo, UNDEFINE(creg));
	}
#endif
#if 0
	/*FIXME: not tested...*/
	/* hi does not need to be long, bits leftshifted out of hi are supposed to 
	* fall out into the void anyway */
	unsigned int hi;  
	unsigned long long lo;
	if(areg <= 2* WORDSIZE_BITS  && areg >= 0 )
	{
		hi = ((unsigned int)creg) << areg; /*shift hi*/
		/* shift lo - if breg is shifted directly then you get the result of
		 * a shifted int put in a long long.  Not what we want... shift the value
		 * in the long long to get what we want (doesn't overflow).  Probably not
		 * very efficient.*/
		lo = breg; 
		lo = lo << areg;  
		hi += lo >> WORDSIZE_BITS;  /*add what 'overflowed' from lo to hi.*/
		STACK((WORD)lo, (WORD)hi, UNDEFINE(creg));
	} 
#endif
#endif
}

/* 0x37 - 0x23 F7 - lsum - long sum - used in conjunction with ladd */
TVM_INSTRUCTION void ins_lsum(void)
{
	/* FIXME: Does the carry need to be calculated using the incomming carry? */
	/* carry algorithm from Hackers Delight p. 34 */
	WORD resnocarry = breg + areg;
	WORD result = resnocarry + (creg & 1);
	WORD carry = ((UWORD) ((breg & areg) | ((breg | areg) & ~resnocarry)) >> (WORDSIZE_BITS - 1));

	STACK(result, carry, UNDEFINE(creg));
}


/* 0x38 - 0x23 0xF8 - lsub - long subtract */
TVM_INSTRUCTION void ins_lsub(void)
{
	WORD result = breg - areg - (creg & 1);

	/* Overflow detection from Hackers Delight p. 27 */
	if( ((UWORD) ((breg ^ areg) & (result ^ breg))) >> (WORDSIZE_BITS - 1))
	{
		set_error_flag(EFLAG_LSUB);
	}

	STACK(result, UNDEFINE(creg), UNDEFINE(breg));
}
	
/* 0x39 - 0x23 0xF9 - runp - run process */
TVM_INSTRUCTION void ins_runp(void)
{
	/* FIXME: Details on this instruction are scetchy in the compiler writers
	 * guide... */
	just_add_to_queue(areg);
}

/* 0x3B - 0x23 0xFB - sb - store byte */
TVM_INSTRUCTION void ins_sb(void)
{
	write_byte((BPOOTER)areg, (BYTE)breg);

	STACK(creg, UNDEFINE(breg), UNDEFINE(creg));
}


/* 0x3C - 0x23 0xFC - gajw - general adjust workspace */
TVM_INSTRUCTION void ins_gajw(void)
{
	/* FIXME: This does not seem to be a very frequently generated instruction...
	 * Is it needed? Possibly have a TVM where instructions not generated by
	 * compiler are not included, which would probalby include this one??? */
	WORD tmp = (WORD)wptr;

	wptr = (POOTER)areg;
	areg = tmp;
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
TVM_INSTRUCTION void ins_saveh(void)
{
	write_mem(pooter_plus((POOTER) areg, 0), (WORD) fptr[pri]);
	write_mem(pooter_plus((POOTER) areg, 1), (WORD) bptr[pri]);
	write_mem(pooter_plus((POOTER) areg, 2), (WORD) tptr[pri]);

	/* printf("fptr: 0x%x; bptr: 0x%x; tptr: 0x%x (wptr: 0x%x; iptr: 0x%x)\n", fptr[pri], bptr[pri], tptr[pri], wptr, iptr); */

	/* Bump the stack */
	STACK(breg, creg, UNDEFINE(creg));
}


	
/****************************************************************************
 *              0x24 0xF_         0x24 0xF_         0x24 0xF_               *
 ****************************************************************************/

/* 0x40 - 0x24 0xF0 - shr - shift right (logical) */
TVM_INSTRUCTION void ins_shr(void)
{
	/* FIXME: I think that I am doing the right thing here, ie if areg >
	 * numberofbiutsinword then the result is zero! However that could be wrong,
	 * due to sign bits? Not sure if they factor in here...
	 */
	/* FIXME: Not all platforms implement shifting in the same way. We need the
	 * instructions to be consistent across platforms though, so at the moment we
	 * are doing a check now which is redundant on some platforms! We could check
	 * for this, and not do it where it is not needed.
	 */
	/* STACK(((UWORD) ((UWORD) breg) >> ((UWORD) areg)), creg, UNDEFINE(creg)); */
	STACK(
			((unsigned) areg >= WORDSIZE_BITS) ? 0 :
			((UWORD) ((UWORD) breg) >> ((UWORD) areg)), 
			creg, UNDEFINE(creg));
}

/* 0x41 - 0x24 0xF1 - shl - shift left (logical) */
TVM_INSTRUCTION void ins_shl(void)
{
	/* FIXME: Not all platforms implement shifting in the same way. We need the
	 * instructions to be consistent across platforms though, so at the moment we
	 * are doing a check now which is redundant on some platforms! We could check
	 * for this, and not do it where it is not needed.
	 */
	/* STACK(((UWORD) breg) << ((UWORD) areg), creg, UNDEFINE(creg)); */
	STACK(
			((unsigned) areg >= WORDSIZE_BITS) ? 0 : ((UWORD) breg) << ((UWORD) areg),
			creg, UNDEFINE(creg));
}

/* 0x42 - 0x24 0xF2 - mint - minimum integer */
TVM_INSTRUCTION void ins_mint(void)
{
	STACK(MIN_INT, areg, breg);
}

/* 0x46 - 0x24 0xF6 - and - bitwise and */
TVM_INSTRUCTION void ins_and(void)
{
	STACK(areg & breg, creg, UNDEFINE(creg));
}

/* 0x4A - 0x24 0xFA - move - move message */
TVM_INSTRUCTION void ins_move(void)
{
	
	/* Areg has the count, we use that as our counter variable and count down to 0
	 * We also modify breg (dest) and creg (src) by adding one to them each
	 * time through the loop in order to perform the move */
	/* FIXME: Optimise this for WORDALIGNED WORDLENGTH moves? */
	for(; areg > 0 ; areg--)
	{
		write_byte((BPOOTER) breg++, read_byte((BPOOTER) creg++));
	}
		
	//STACK(UNDEFINE(areg), UNDEFINE(breg), UNDEFINE(creg));
}

/* 0x4B - 0x24 0xFB - or - or */
TVM_INSTRUCTION void ins_or(void)
{
	STACK(areg | breg, creg, UNDEFINE(creg));
}

/* 0x4C - 0x24 0xFC - csngl - check single */
TVM_INSTRUCTION void ins_csngl(void)
{
	if((areg < 0 && breg != -1) || (areg >= 0 && breg != 0))
	{
		set_error_flag(EFLAG_CSNGL);
	}

	STACK(areg, creg, UNDEFINE(creg));
}

/* 0x4D - 0x24 0xFD - ccnt1 - check count from 1 */
TVM_INSTRUCTION void ins_ccnt1(void)
{
	if((breg == 0) || (((UWORD) breg) > ((UWORD) areg)))
	{
		set_error_flag(EFLAG_CCNT1);
	}
	
	STACK(breg, creg, UNDEFINE(creg));
}

/* 0x4F - 0x24 0xFF - ldiff - Long Difference */
TVM_INSTRUCTION void ins_ldiff(void)
{
	/* carry algorithm from Hackers Delight p. 35 */
	WORD resnocarry = breg - areg;
	WORD result = resnocarry - (creg & 1);
	WORD equiv = (breg & areg) - (breg | areg) - 1;
	WORD carry = ((UWORD) ((~breg & areg) | (equiv & result)) >> (WORDSIZE_BITS - 1));

	STACK(result, carry, UNDEFINE(creg));
}

/****************************************************************************
 *              0x25 0xF_         0x25 0xF_         0x25 0xF_               *
 ****************************************************************************/

/* 0x52 - 0x25 0xF2 - sum - sum */
TVM_INSTRUCTION void ins_sum(void)
{
	STACK(areg + breg, creg, UNDEFINE(creg));
}

/* 0x53 - 0x25 0xF3 - mul - multiply */
TVM_INSTRUCTION void ins_mul(void)
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
	STACK(breg * areg, creg, UNDEFINE(creg));
}

/* 0x55 - 0x25 0xF5 - stoperr - stop on error */
TVM_INSTRUCTION void ins_stoperr(void)
{
	if(error_flag)
	{
		/* Undefine the whole stack */
		//STACK(UNDEFINE(areg), UNDEFINE(areg), UNDEFINE(breg));

		/* Store the instruction pointer at wptr-1 */
		write_mem(pooter_minus(wptr, 1), (WORD)iptr);

		/* Run a new process */
		iptr = run_next_on_queue();
	}
}

/* 0x56 - 0x25 0xF6 - cword - check word */
TVM_INSTRUCTION void ins_cword(void)
{
	if((breg >= areg) || (breg < -areg))
	{
		set_error_flag(EFLAG_CWORD);
	}

	STACK(breg, creg, UNDEFINE(creg));
}

/****************************************************************************
 *              0x27 0xF_         0x27 0xF_         0x27 0xF_               *
 ****************************************************************************/

/* 0x79 - 0x27 0xF9 - pop - pop top of stack */
TVM_INSTRUCTION void ins_pop(void)
{
	STACK(breg, creg, UNDEFINE(creg));
}

/****************************************************************************
 *              0x2F 0xF_         0x27 0xF_         0x27 0xF_               *
 ****************************************************************************/

/* 0xFE - 0x2F 0xFE - ins_shutdown - application shutdown */
TVM_INSTRUCTION void ins_shutdown(void)
{
	/* This instruction and opcode do not exist on the transputer.
	 *
	 * We place this instruction at the top of the main stack frame,
	 * and use it to cleanly terminate the application on completion.
	 */
	if(!has_shutdown)
	{
		has_shutdown = 1;
		ins_stopp();
	}
	else
	{
		set_error_flag(EFLAG_SHUTDOWN);
	}
}

