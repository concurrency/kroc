/*
tvm - ins_float.c
The Transterpreter - a portable virtual machine for Transputer bytecode
Copyright (C) 2004-2008 Christian L. Jacobsen, Matthew C. Jadud
Copyright (C) 2007 University of Kent 

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
#include "ins_float.h"

#ifdef TVM_USE_FPU
#include <math.h>
#include <assert.h>

#ifdef HAVE_FENV_H
#include <fenv.h>
#else
#warning No fegetround/fesetround available some floating point instructions will be wrong
#define FE_TONEAREST	0
#define FE_TOWARDZERO 	0xc00
static inline int _not_fesetround(x) {
	return 1;
}
#define fegetround() (0)
#define fesetround(x) _not_fesetround(x)
#endif


#ifdef TVM_OS_SOLARIS
/* defines all the floating point versions of sqrt and friends. */
#include <sunmath.h>
#endif /* TVM_OS_SOLARIS */

#define SET_ROUNDMODE(X) \
	do {					\
		if((X) != fegetround()) 	\
			fesetround((X));	\
	} while(0)
#define SET_ROUNDMODE_RET(X) \
	do {					\
		SET_ROUNDMODE(X);		\
		return ECTX_CONTINUE;		\
	} while(0)

#endif /* TVM_USE_FPU */

#if defined(TVM_EMULATE_T4) || defined(TVM_EMULATE_T8)
static const WORD FP_INFINITY = 0x7F800000;

/* 0x63 - 0x26 0xF3 - unpacksn - unpack single len floating point number */
/* On entry:
     Areg = 32-bit float
     Breg = n
   On exit:
     Areg = fraction part, left-aligned in the word, with the leading 1
     Breg = exponent part
     Creg = (4 * n) + type of float
       (type: 0 = zero; 1 = any number; 2 = infinity; 3 = NaN)
  This is based on UNPACKSN from the occama library.
 */
TVM_INSTRUCTION (ins_unpacksn)
{
	WORD mantissa, exponent, type;

	/* A REAL32 holds, from left to right:
	   - one sign bit (which we ignore)
	   - 8 bits of exponent
	   - 23 bits of mantissa, with an implicit leading 1 */
	exponent = (AREG >> 23) & 0xFF;
	/* Leave space for us to add the leading 1 later. */
	mantissa = (AREG & 0x7FFFFF) << 8;

	if (exponent == 0) {
		if (mantissa == 0) {
			/* zero */
			type = 0;
		} else {
			/* denormalised */
			type = 1;
			/* This is bug-compatible with the occam UNPACKSN:
			   if you've got a subnormal number like 0x00000001
			   then the occam one returns (0x100, 0x1, 0x1)
			   rather than (0x10, 0x0, 0x1) --
			   i.e. it'd be perfectly reasonable to say
			   mantissa <<= 1; here instead. */
			exponent = 1;
		}
	} else if (exponent == 0xFF) {
		if (mantissa == 0) {
			/* infinity */
			type = 2;
		} else {
			/* NaN */
			type = 3;
		}
	} else {
		/* normalised - add the implicit 1 */
		type = 1;
		mantissa |= 0x80000000;
	}

	STACK_RET(mantissa, exponent, (4 * BREG) + type, STYPE_DATA, STYPE_DATA, STYPE_DATA);
}

/* 0x6C - 0x26 0xFC - postnormsn - post normalise correction of single length fp nr */
/* On entry:
     Areg = guard word (least significant bits of mantissa)
     Breg = high word (most significant bits of mantissa)
     Creg = number of bits shifted during normalisation
     Wptr[0] = exponent
  On exit:
     Areg = guard word
     Breg = fraction
     Creg = exponent
  This is based on ROUNDSN from the occama library (which does the equivalent
  of NORM, POSTNORMSN, ROUNDSN).
 */
TVM_INSTRUCTION (ins_postnormsn)
{
	WORD guard = AREG;
	/* Adjust the exponent for the number of places shifted. */
	WORD exponent = WORKSPACE_GET(WPTR, WS_TEMP) - CREG;
	WORD mantissa = BREG;
	if (exponent <= -32) {
		/* too small to represent -- make it zero */
		exponent = 0;
		mantissa = 0;
		guard = 0;
	} else if (exponent <= 0) {
		/* denormalised -- shift mantissa appropriately */
		long long unsigned int x = mantissa;
		x = ((x << 32) | guard) >> (1 - exponent);
		mantissa = x >> 32;
		guard = x & 0xFFFFFFFF;
		exponent = 0;
	} else {
		/* normalised -- nothing more to be done */
	}
	STACK_RET(guard, mantissa, exponent, STYPE_DATA, STYPE_DATA, STYPE_DATA);
}

/* 0x6D - 0x26 0xFD - roundsn - round single length fp number */
/* On entry:
     Areg = guard word (least significant bits of mantissa)
     Breg = fraction (most significant bits of mantissa)
     Creg = exponent
   On exit:
     Areg = 32-bit float built from the given values
   This is likewise based on ROUNDSN from occama.
 */
TVM_INSTRUCTION (ins_roundsn)
{
	WORD f;
	if (CREG >= 0xFF) {
		/* too large to represent -- make it infinity */
		f = FP_INFINITY;
	} else {
		/* Create the REAL32. */
		f = (CREG & 0xFF) << 23 | ((BREG >> 8) & 0x7FFFFF);

		if ((BREG & 0x80) == 0) {
			/* round bit not set -- round down */
		} else if ((AREG | (BREG & 0x7F) | (f & 1)) == 0) {
			/* round bit is set, but the LSB of f and everything to
			   its right are 0 -- round down */
		} else {
			/* otherwise -- round up */
			f += 1;
		}
	}
	STACK1_RET(f, STYPE_DATA);
}

/* 0x71 - 0x27 0xF1 - ldinf - load single length floating point infinity */
TVM_INSTRUCTION (ins_ldinf)
{
	STACK_RET(FP_INFINITY, AREG, BREG, STYPE_DATA, AREGt, BREGt);
}

/* This is a T4  only specific instruction */
/* 0x72 - 0x27 0xF2 - fmul - fractional multiply */
TVM_INSTRUCTION (ins_fmul)
{
	/* The code below is borrowed from the CCSP kernel */
	long long tmp_long;
	int hi_word, lo_word;
	int tmpint_c;

	tmp_long = (long long)AREG * (long long)BREG;
	hi_word = (int)((tmp_long >> 32) & 0xffffffff);
	lo_word = (int)(tmp_long & 0xffffffff);
	hi_word  = (int)((unsigned int)hi_word << 1);
	if ((unsigned int)lo_word & 0x80000000) {
		hi_word |= 1;
	}
	lo_word = (int)((unsigned int)lo_word << 1);
	if (lo_word >= 0) {
		tmpint_c = hi_word;
	} else if (lo_word != 0x80000000) {
		tmpint_c = hi_word + 1;
	} else if (hi_word & 1) {
		tmpint_c = hi_word + 1;
	} else {
		tmpint_c = hi_word;
	}

	STACK2_RET(tmpint_c, CREG, STYPE_DATA, CREGt);
}

#endif /* TVM_EMULATE_T4 || TVM_EMUALTE_T8 */

#ifdef TVM_EMULATE_T8

/* The floating point registers. */
double fAREG, fBREG, fCREG;
#define DOUBLE 1
#define SINGLE 0
int fAREG_length, fBREG_length, fCREG_length;

/*Pushes the new AREG onto the stack */
#define PUSH_FPREG(X, Y) \
	do { 					\
		fCREG 		= fBREG;	\
		fCREG_length 	= fBREG_length;	\
		fBREG 		= fAREG; 	\
		fBREG_length	= fAREG_length; \
		fAREG 		= (X);		\
		fAREG_length 	= (Y);		\
	} while (0)

#define POP_FPREG(X, Y) \
	do {					\
		fAREG 		= X;		\
		fAREG_length 	= Y;		\
        	fBREG 		= fCREG;	\
		fBREG_length 	= fCREG_length; \
	} while (0)

/**************************
*  0x28 0xFx Starts here  *
***************************/

/* 0x82 - 0x28 0xF2 - fpldnldbi - floating load non-local indexed double */
TVM_INSTRUCTION (ins_fpldnldbi)
{
	/* BREG is double word index */
	PUSH_FPREG(read_wordd(wordptr_plus(AREG, (BREG * 2))), DOUBLE);
	STACK1(CREG, CREGt);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x83 - 0x28 0xF3 - fpcheckerr - check floating error */
/*FIXME: Erm, I guess we should do something here...
 * Oh, and speaking of which, C should really bomb if we get one
 * of these.  We could check for overflow and the like 'inline'*/
TVM_INSTRUCTION (ins_fpchkerr)
{
	//From gray tputer book.
	//error_flag' = error_flag \/ fp_error_flag
	//round_mode' = ToNearest
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x84 - 0x28 0xF4 - fpstnldb - floating point store non-local double */
TVM_INSTRUCTION (ins_fpstnldb)
{
	write_wordd(AREG, fAREG);
	POP_FPREG(fBREG, fBREG_length);
	STACK2_RET(BREG, CREG, BREGt, CREGt);
}

/* 0x86 - 0x28 0xF6 - fpldnlsni - floating load non local indexed single */
TVM_INSTRUCTION (ins_fpldnlsni)
{
	PUSH_FPREG(read_wordf(wordptr_plus(AREG, BREG)), SINGLE);
	STACK1(CREG, CREGt);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x87 - 0x28 0xF7 - fpadd - floating point add */
TVM_INSTRUCTION (ins_fpadd)
{
	if((fAREG_length == fBREG_length) && (fAREG_length == SINGLE)) {
		POP_FPREG((float) ((float)fBREG) + ((float)fAREG), fAREG_length);
	} else if ((fAREG_length == fBREG_length) && (fAREG_length == DOUBLE)) {
		POP_FPREG((double) ((double)fBREG) + ((double)fAREG), fAREG_length);
	} else {
		SET_ERROR_FLAG(EFLAG_FP);
	}
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x88 - 0x28 0xF8 - fpstnlsn - floating store non local single */
TVM_INSTRUCTION (ins_fpstnlsn)
{

	write_wordf(AREG, (float)fAREG);
	STACK2(BREG, CREG, BREGt, CREGt);
	POP_FPREG(fBREG, fBREG_length);
	SET_ROUNDMODE_RET(FE_TONEAREST);

}

/* 0x89 - 0x28 0xF9 - fpsub - floating point subtract */
TVM_INSTRUCTION (ins_fpsub)
{
	if((fAREG_length == fBREG_length) && (fAREG_length == SINGLE)) {
		POP_FPREG((float) ((float)fBREG) - ((float)fAREG), fAREG_length);
	} else if ((fAREG_length == fBREG_length) && (fAREG_length == DOUBLE)) {
		POP_FPREG((double) ((double)fBREG) - ((double)fAREG), fAREG_length);
	} else {
		SET_ERROR_FLAG(EFLAG_FP);
	}
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x8A - 0x28 0xFA - fpldnldb - floating point load non-local double */
TVM_INSTRUCTION (ins_fpldnldb)
{
	PUSH_FPREG(read_wordd(AREG), DOUBLE);
	STACK2_RET(BREG, CREG, BREGt, CREGt);
}

/* 0x8B - 0x28 0xFB - fpmul - floating point multiply */
TVM_INSTRUCTION (ins_fpmul)
{
	if((fAREG_length == fBREG_length) && (fAREG_length == SINGLE)) {
		POP_FPREG((float) ((float)fBREG) * ((float)fAREG), fAREG_length);
	} else if ((fAREG_length == fBREG_length) && (fAREG_length == DOUBLE)) {
		POP_FPREG((double) ((double)fBREG) * ((double)fAREG), fAREG_length);
	} else {
		SET_ERROR_FLAG(EFLAG_FP);
	}
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x8C - 0x28 0xFC - fpdiv - floating point divide */
TVM_INSTRUCTION (ins_fpdiv)
{
	if((fAREG_length == fBREG_length) && (fAREG_length == SINGLE))
	{
		POP_FPREG((float) ((float)fBREG) / ((float)fAREG), fAREG_length);
	} else if ((fAREG_length == fBREG_length) && (fAREG_length == DOUBLE)) {
		POP_FPREG((double) ((double)fBREG) / ((double)fAREG), fAREG_length);
	} else {
		SET_ERROR_FLAG(EFLAG_FP);
	}
	//fAREG.len = fBREG.len
	//fAREG  = (float) ((float)fBREG) / ((float)fAREG);
	//fBREG = fCREG;
	//fCREG = undefined
	//fp_error flag can be set by the division
	//round_mode = ToNearest
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x8E - 0x28 0xFE - fpldnlsn - floating point load non local single */
TVM_INSTRUCTION (ins_fpldnlsn) 
{
	PUSH_FPREG(read_wordf( AREG), SINGLE);
	STACK2(BREG, CREG, BREGt, CREGt);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/**************************
 *  0x29 0xFx Starts here  *
 ***************************/
/* 0x91 - 0x29 0xF1 - fpnan - floating point test for NaN */
TVM_INSTRUCTION (ins_fpnan)
{
	WORD result = isnan(fAREG);
	STACK(result, AREG, BREG, STYPE_DATA, STYPE_DATA, BREGt);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x92 - 0x29 0xF2 - fpordered - floating point ordereability */
TVM_INSTRUCTION (ins_fpordered)
{ 
	WORD result;
	if ((isnan(fAREG)) || (isnan(fBREG))) {
		result = 0;
	} else {
		result = 1;
	}
	STACK(result, AREG, BREG, STYPE_DATA, AREGt, BREGt);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x93 - 0x29 0xF3 - fpnotfinite - floating point test for not finite */
TVM_INSTRUCTION (ins_fpnotfinite)
{
	WORD result;
	if (isnan(fAREG) || isinf(fAREG)) {
		result = 1; 
	} else {
		result = 0;
	}
	STACK(result, AREG, BREG, STYPE_DATA, AREGt, BREGt);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x94 - 0x29 0xF4 - fpgt - floating point equals */
TVM_INSTRUCTION (ins_fpgt)
{
	WORD result = 0;
	/* This one pops 2 floating regs off the stack so it does not use the pop macro*/
	if (isnan(fAREG) || isnan(fBREG)) {
		/* result = 0 */
	} else if ((fAREG_length == fBREG_length) && (fAREG_length == SINGLE)) {
		if((float)fBREG > (float)fAREG) {
			result = 1;
		}
	} else if ((fAREG_length == fBREG_length) && (fAREG_length == DOUBLE)) {
		if((double)fBREG > (double)fAREG) {
			result = 1;
		}
	} else { 
		SET_ERROR_FLAG(EFLAG_FP); 
	}

	fAREG = fCREG;
	fAREG_length = fCREG_length;
	//fBREG = undefined
	//fCREG = undefined 
	// Could set fp error flag here?  Fp.Error.flg = fp.error.flag |? (fareeg E Inf U NAN) \/ (fBREG E Inf U Nan)
	STACK(result, AREG, BREG, STYPE_DATA, AREGt, BREGt);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x95 - 0x29 0xF5 - fpeq - floating point equals */
TVM_INSTRUCTION (ins_fpeq)
{
	WORD result = 0;
	//fAREG_length = fBREG_length
	if (isnan(fAREG) || isnan(fBREG)) {
		/* result = 0; */
	} else if ((fAREG_length == fBREG_length) && (fAREG_length == SINGLE)) {
		if((float)fAREG == (float)fBREG) {
			result = 1;
		}
	} else if ((fAREG_length == fBREG_length) && (fAREG_length == DOUBLE)) {
		if((double)fAREG == (double)fBREG) {
			result = 1;
		}
	} else { 
		SET_ERROR_FLAG(EFLAG_FP);
	}
	fAREG = fCREG;
	fAREG_length = fCREG_length;
	//fBREG = undefined
	//fCREG = undefined 
	// Could set fp error flag here?  Fp.Error.flg = fp.error.flag |? (fareeg E Inf U NAN) \/ (fBREG E Inf U Nan)
	STACK(result, AREG, BREG, STYPE_DATA, AREGt, BREGt);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x96 - 0x29 0xF6 - fpi32tor32 - load int32 as real32 */
TVM_INSTRUCTION (ins_fpi32tor32)
{
	/*Use read_word not read_wordf since the book says not to RETYPE*/
	PUSH_FPREG((float)read_word(AREG), SINGLE);
	STACK2(BREG, CREG, BREGt, CREGt);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x98 - 0x29 0xF8 - fpi32tor64 - load int32 as real64 */
TVM_INSTRUCTION (ins_fpi32tor64)
{
	/*Use read_word not read_wordf since the book says not to RETYPE*/
	PUSH_FPREG((double)read_word(AREG), DOUBLE);
	STACK2(BREG, CREG, BREGt, CREGt);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}


/* 0x9A - 0x29 0xFA - fpb32tor64 - load unsigned word as real64 */
TVM_INSTRUCTION (ins_fpb32tor64)
{
	PUSH_FPREG(*(unsigned int *)AREG, DOUBLE);
	STACK2(BREG, CREG, BREGt, CREGt);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x9D - 0x29 0xFD - fprtoi32 - real to int32 */
TVM_INSTRUCTION (ins_fprtoi32)
{
	/* FIXME: verify fAREG < 2^31 and >= -2^31 */
	if (fAREG_length == SINGLE) { 
		/* This truncates any decimals... 
		 * I think it should actually round since that seems
		 * to be the default (need to check) */
		fAREG = rintf(fAREG);
	} else {
		fAREG = rint(fAREG);
	}
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x9E - 0x29 0xFE - fpstnli32 - store non local int32 */
TVM_INSTRUCTION (ins_fpstnli32)
{
	/* FIXME: verify fAREG < 2^31 and >= -2^31 */
	
	/* IMPORTANT: This uses write_word instead of write_wordf
	 * since we are storing the float as an int (casting)i.
	 * The below line is supposed to correspond to:
	 * Mem' = Mem { Areg -> INT32 TRUNC pack (fAREG)} 
	 * where pack is either pack.sn or pack.db depending
	 * on fAREG.len */
	write_word(AREG, (int)fAREG);
	STACK2(BREG, CREG, BREGt, CREGt);
	POP_FPREG(fBREG, fBREG_length);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0x9F - 0x29 0xFF - fpldzerosn - floating load zero single */
TVM_INSTRUCTION (ins_fpldzerosn)
{
	PUSH_FPREG((float) 0.0, SINGLE);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/**************************
 *  0x2A 0xFx Starts here  *
 ***************************/
/* 0xA0 - 0x2A 0xF0 - fpldzerodb - floating load zero double */
TVM_INSTRUCTION (ins_fpldzerodb)
{
	PUSH_FPREG((double) 0.0, DOUBLE);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xA1 - 0x2A 0xF1 - fpint - round to floating integer */
/* This rounds FAreg, wrt Round_Mode, to a floating point number of the same format iwth an integer value. - from Gray Book.*/
TVM_INSTRUCTION (ins_fpint)
{
	if(fAREG_length == SINGLE) {
		fAREG = rintf(fAREG);
	} else {
		fAREG = rint(fAREG);
	}
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xA3 - 0x2A 0xF3 - fpdup - floating point duplicate */
TVM_INSTRUCTION (ins_fpdup)
{
	PUSH_FPREG(fAREG, fAREG_length);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xA4 - 0x2A 0xF4 - fprev - floating reverse */
TVM_INSTRUCTION (ins_fprev)
{
	double tmp = fAREG;
	int tmp_length = fAREG_length;
	fAREG = fBREG; 
	fAREG_length = fBREG_length;
	fBREG = tmp;
	fBREG_length = tmp_length;
	//fCREG' = fCREG;
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xA6 - 0x2A 0xF6 - fpldnladddb - floating load non local and add double */
TVM_INSTRUCTION (ins_fpldnladddb)
{
	// AREG /\ byteselectmask = 0
	// fAREG.len = db - means fAREG is a double.
	//fAREG' = fAREG +IEEE unpack.db (RETYPE REAL64 [Mem Areg, Mem (Index Areg 1)])
	fAREG = (double) ((double)fAREG) + read_wordd(AREG);
	fAREG_length = DOUBLE;
	//fBREG' = fBREG
	//fCREG = undefined
	STACK2(BREG, CREG, BREGt, CREGt);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xA8 - 0x2A 0xF8 - fpldnlmuldb - floating load non local and multiply double */
TVM_INSTRUCTION (ins_fpldnlmuldb)
{
	fAREG = (double)((double)fAREG) * read_wordd(AREG);
	fAREG_length = DOUBLE;
	//fBREG = fBREG (doesn't change)
	//fCREG = undefined
	STACK2(BREG, CREG, BREGt, CREGt);
	//fp_error_flag can be set from float multiply
	//round_mode = ToNearest
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xAA - 0x2A 0xFA - fpldnladdsn - floating load non local and add single */
TVM_INSTRUCTION (ins_fpldnladdsn)
{
	// AREG /\ byteselectmask = 0
	// fAREG.len = sn - means fAREG is a single, not a double.

	//fAREG' = fAREG +ieee unpack.sn (RETYPE REAL32 Mem Areg)
	fAREG = (float) ((float)fAREG) + read_wordf(AREG);
	fAREG_length = SINGLE;
	//printf("fAREG is %f, read_word is %f, AREG* is %i\n", fAREG, read_wordf(AREG), AREG);

	//fBREG' = fBREG (no changes, so do nothing)
	//fCREG' = undefined..

	STACK2(BREG, CREG, BREGt, CREGt);
	//FP_Error_FLag' = can be set by error from +ieee..
	//round_mode' = ToNearest - see FIXME above.
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xAC - 0x2A 0xFC - fpldnlmulsn - floating load non local and multiply single */
TVM_INSTRUCTION (ins_fpldnlmulsn)
{
	fAREG = (float)((float)fAREG) * read_wordf(AREG);
	fAREG_length = SINGLE;
	//fBREG = fBREG (doesn't change)
	//fCREG = undefined
	STACK2(BREG, CREG, BREGt, CREGt);
	//fp_error_flag can be set from float multiply
	//round_mode = ToNearest
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/**************************
 *  0x2C 0xFx Starts here  *
 ***************************/
/* 0xCF - 0x2C 0xFF - fprem - floating point remainder */
/* This 'instruction' is composed of what used to be 2 instruction on the transputer #8F and #90 
 * which are fpremfirst and fpremstep respectivley */
TVM_INSTRUCTION (ins_fprem)
{
	if((fAREG_length == fBREG_length) && (fAREG_length == SINGLE)) {
		fAREG = remainderf(fBREG, fAREG);
	} else if ((fAREG_length == fBREG_length) && (fAREG_length == DOUBLE)) {
		fAREG = remainder(fBREG, fAREG);
	} else { 
		SET_ERROR_FLAG(EFLAG_FP);
	}
	SET_ROUNDMODE_RET(FE_TONEAREST);

	/* From gray book: The value of fBREG' will be the quotient used to produce the remainder when (fBREG.exp - fAREG.exp)
	   is <= 20 for single length and <= 30 for double length operands.  This is inteded to correct the rounding error
	   in argument reduction and is explained in greater detail in teh arithmetic operations section earlier. */
}

/**************************
 *  0x2D 0xFx Starts here  *
 ***************************/
/* 0xD0 - 0x2D 0xF0 - i64toreal - 64bit into to real (converted from special .I64TOREAL) */
TVM_INSTRUCTION (ins_i64toreal)
{
	//FIXME: Fill me in...
	/*  From tranx86:
	    case I64TOREAL:
#if 0
fprintf (stderr, "*** I64TOREAL: ts_depth=%d, fs_depth=%d\n", ts->stack->ts_depth, ts->stack->fs_depth);
#endif
ts->stack->old_a_reg = ts->stack->a_reg;
ts->stack->old_b_reg = ts->stack->b_reg;
ts->stack->old_c_reg = ts->stack->c_reg;
deferred_cond (ts);
tstack_setsec (ts->stack, I_POP, arch);
add_to_ins_chain (compose_ins (INS_FILD64, 1, 0, ARG_REGIND, ts->stack->old_a_reg));
arch->compose_fp_set_fround (ts, FPU_N);
ts->stack->fs_depth++;
tstate_ctofp (ts);
break;
*/
	/* Push stack */
	//STACK(AREG, AREG, BREG);
	fAREG = (double) read_word(AREG);
	fAREG_length = DOUBLE;
	//STACK(AREG, AREG, BREG);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xD1 - 0x2D 0xF1 - fpdivby2 - floating point divide by 2 */
TVM_INSTRUCTION (ins_fpdivby2)
{
	if (fAREG_length == SINGLE) { 
		fAREG = (float)fAREG / (float) 2.0;
	} else {
		fAREG = fAREG / 2.0;
	}
	//fp error can be set from fAREG * 2
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xD2 - 0x2D 0xF2 - fpmulby2 - floating point multiply by 2 */
TVM_INSTRUCTION (ins_fpmulby2)
{
	if (fAREG_length == SINGLE) {
		fAREG = (float)fAREG * (float)2.0;
	} else { //if ( fAREG_length && DOUBLE) {
		fAREG = fAREG * 2.0;
	}
	//fp error can be set from fAREG * 2
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xD3 - 0x2D 0xF3 - fpsqrt - floating point square root */
TVM_INSTRUCTION (ins_fpsqrt)
{
	/* While this instruction was actually composed of 3 instructions, 
	 * it is only called by fpsqrt and hence can be implemented as a
	 * single instruction.  The question is, is it safe to assume
	 * that we have a math.h libary on every system, and does it 
	 * compromise portability? (if fp is used, then yes, probably)*/
	if(fAREG_length == SINGLE) { 
		fAREG = (float)sqrtf(fAREG);
	} else { //if (fAREG_length == DOUBLE) {
		fAREG = sqrt(fAREG);
	} 
	//fp_error_flag = can be set by error from SQRT-ieee FAREG
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xD6 - 0x2D 0xF6 - fprz - floating point rounding mode to zero
 * - This is a 'new' instruction, which did exist on the transputer
 *   but was strange (0x44; 0x2A; 0xFB), and was renumbered in kroc.*/
TVM_INSTRUCTION (ins_fprz)
{
	SET_ROUNDMODE_RET(FE_TOWARDZERO);
}

/* 0xD7 - 0x2D 0xF7 - fpr32tor64 - real32 to real64 */
TVM_INSTRUCTION (ins_fpr32to64) 
{
	//FIXME: Should check for over/underflow.
	fAREG = (double) fAREG;
	fAREG_length = DOUBLE;
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xD8 - 0x2D 0xF8 - fpr64tor32 - real64 to real32 */
TVM_INSTRUCTION (ins_fpr64to32) 
{
	//FIXME: Should check for over/underflow.
	fAREG = (float) fAREG;
	fAREG_length = SINGLE;
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xD9 - 0x2D 0xF9 - fpexpdec32 - floating divide by 2^32 */
TVM_INSTRUCTION (ins_fpexpdec32)
{
	//FIXME: Check if this is for both single and double
	//Gray transputer book says: fAREG' = fAREG / IEEE 2^32
	//fAREG = fAREG / 4294967296;
	fAREG = fAREG / pow(2, 32);
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xDB - 0x2D 0xFB - fpabs - floating point absolute value */
TVM_INSTRUCTION (ins_fpabs)
{
	if (fAREG_length == SINGLE) {
		fAREG = fabsf(fAREG);
	} else {
		fAREG = fabs(fAREG);
	}
	SET_ROUNDMODE_RET(FE_TONEAREST);
}

/* 0xDF - 0x2D 0xFF - fpchki64 -  check that value at top of FP stack (fAREG) fits in an INT32/INT64 
 * this should not be genereated for t8 but it seems to be. Wierd.  Tranx86 says so. */
TVM_INSTRUCTION (ins_fpchki64)
{
	return ECTX_CONTINUE; /* FIXME: really ECTX_INS_UNSUPPORTED */
	//FIXME:  Implement something here atm it does nothing... Should be ok till it does actually overflow.
	/* From tranx86: */
#if 0
	case I_FPCHKI32:
	case I_FPCHKI64:
		/* this should work, but are never actually generated for a T800 */
		/* check that (-2^xx <= value) */
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->floatrange_label[(sec == I_FPCHKI32) ? 0 : 2], ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_FLD64, 1, 0, ARG_REGIND, tmp_reg));
		add_to_ins_chain (compose_ins (INS_FCOMP, 0, 0));
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EAX));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		this_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_BE, ARG_LABEL, this_lab));
		compose_overflow_jumpcode_i386 (ts, (sec == I_FPCHKI32) ? PMOP_FPCHKI32 : PMOP_FPCHKI64);
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		/* check that (2^xx > value) */
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_MOVE, 1, 1, ARG_LABEL | ARG_ISCONST, ts->floatrange_label[(sec == I_FPCHKI32) ? 1 : 3], ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_FLD64, 1, 0, ARG_REGIND, tmp_reg));
		add_to_ins_chain (compose_ins (INS_FCOMP, 0, 0));
		tmp_reg = tstack_newreg (ts->stack);
		add_to_ins_chain (compose_ins (INS_CONSTRAIN_REG, 2, 0, ARG_REG, tmp_reg, ARG_REG, REG_EAX));
		add_to_ins_chain (compose_ins (INS_FSTSW, 0, 1, ARG_REG, tmp_reg));
		add_to_ins_chain (compose_ins (INS_SAHF, 1, 1, ARG_REG | ARG_IMP, tmp_reg, ARG_REG | ARG_IMP, REG_CC));
		add_to_ins_chain (compose_ins (INS_UNCONSTRAIN_REG, 1, 0, ARG_REG, tmp_reg));
		this_lab = ++(ts->last_lab);
		add_to_ins_chain (compose_ins (INS_CJUMP, 2, 0, ARG_COND, CC_A, ARG_LABEL, this_lab));
		compose_overflow_jumpcode_i386 (ts, (sec == I_FPCHKI32) ? PMOP_FPCHKI32 : PMOP_FPCHKI64);
		add_to_ins_chain (compose_ins (INS_SETLABEL, 1, 0, ARG_LABEL, this_lab));
		break;
#endif
}

#endif /* TVM_EMULATE_T8 */

