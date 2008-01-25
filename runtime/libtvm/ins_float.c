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

#include "transputer.h"
#include "instructions.h"
#include "mem.h"
#include "ins_float.h"

#ifdef __FPU_SUPPORT__
#include <math.h>
#include <fenv.h>
#include <assert.h>

#ifdef SOLARIS
//defines all the floating point versions of sqrt and friends.
#include <sunmath.h>
#endif
#endif

#ifndef __FPU_SUPPORT__
#if TVM_WORD_LENGTH < 4
/* INT is too small to hold a REAL32 on this platform, so we can't provide the
   REAL32 helper instructions. */
#define TOO_SMALL_FOR_FP
#endif

/*Fixes warning on lego*/
#ifndef TOO_SMALL_FOR_FP
/* Sign bit 0; exponent 0xFF; mantissa 0. */
static const WORD FP_INFINITY = 0x7F800000;
#endif

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
TVM_INSTRUCTION void ins_unpacksn(void)
{
#ifdef TOO_SMALL_FOR_FP
	ins_not_implemented();
#else
	WORD mantissa, exponent, type;

	/* A REAL32 holds, from left to right:
	   - one sign bit (which we ignore)
	   - 8 bits of exponent
	   - 23 bits of mantissa, with an implicit leading 1 */
	exponent = (areg >> 23) & 0xFF;
	/* Leave space for us to add the leading 1 later. */
	mantissa = (areg & 0x7FFFFF) << 8;

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

	STACK(mantissa, exponent, (4 * breg) + type);
#endif
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
TVM_INSTRUCTION void ins_postnormsn(void)
{
#ifdef TOO_SMALL_FOR_FP
	ins_not_implemented();
#else
	WORD guard = areg;
	/* Adjust the exponent for the number of places shifted. */
	WORD exponent = WORKSPACE_GET(wptr, WS_TOP) - creg;
	WORD mantissa = breg;
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
	STACK(guard, mantissa, exponent);
#endif
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
TVM_INSTRUCTION void ins_roundsn(void)
{
#ifdef TOO_SMALL_FOR_FP
	ins_not_implemented();
#else
	WORD f;
	if (creg >= 0xFF) {
		/* too large to represent -- make it infinity */
		f = FP_INFINITY;
	} else {
		/* Create the REAL32. */
		f = (creg & 0xFF) << 23 | ((breg >> 8) & 0x7FFFFF);

		if ((breg & 0x80) == 0) {
			/* round bit not set -- round down */
		} else if ((areg | (breg & 0x7F) | (f & 1)) == 0) {
			/* round bit is set, but the LSB of f and everything to
			   its right are 0 -- round down */
		} else {
			/* otherwise -- round up */
			f += 1;
		}
	}
	STACK(f, UNDEFINE(breg), UNDEFINE(creg));
#endif
}

/* 0x71 - 0x27 0xF1 - ldinf - load single length floating point infinity */
TVM_INSTRUCTION void ins_ldinf(void)
{
#ifdef TOO_SMALL_FOR_FP
	ins_not_implemented();
#else
	STACK(FP_INFINITY, areg, breg);
#endif
}

#endif //if not defined __FPU_SUPPORT__

/* This is a T4  only specific instruction */
/* 0x72 - 0x27 0xF2 - fmul - fractional multiply */
TVM_INSTRUCTION void ins_fmul(void)
{
#ifdef TOO_SMALL_FOR_FP
	  ins_not_implemented();
#else
  /* The code below is borrowed from the CCSP kernel */
  long long tmp_long;
  int hi_word, lo_word;
  int tmpint_c;

  tmp_long = (long long)areg * (long long)breg;
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
  STACK(tmpint_c, creg, UNDEFINE(creg));
#endif /* TOO_SMALL_FOR_FP */
}



#ifdef __FPU_SUPPORT__
/*The floating point registers.*/

double fareg, fbreg, fcreg;
#define DOUBLE 1
#define SINGLE 0
int fareg_length, fbreg_length, fcreg_length;

/*Pushes the new areg onto the stack */
#define PUSH_FPREG(X, Y)  fcreg = fbreg; fcreg_length = fbreg_length; \
                               fbreg = fareg; fbreg_length = fareg_length; \
                               fareg = X; fareg_length = Y
/*NOTE: This may be risky, always assigning fbreg_length to fareg_length here 
  It might also be ok.*/
#define POP_FPREG(X) fareg = X; fareg_length = fbreg_length; \
                            fbreg = fcreg; fbreg_length = fcreg_length
/**************************
*  0x28 0xFx Starts here  *
***************************/

/* 0x82 - 0x28 0xF2 - fpldnldbi - floating load non-local indexed double */
TVM_INSTRUCTION void ins_fpldnldbi(void)
{
  /*FIXME!  the * 2 of breg here is very magical:
  * I think this has to do with the index being 64 bit (as in indexing into 64 bit words)
  * so the index needs to be multiplied by 2 to get the correct value.  This makes the cgtest
  * pass.  There is probably some better way to designate this though... DJD 24012007*/
  PUSH_FPREG(read_wordd(wordptr_plus(areg, (breg * 2))), DOUBLE);
  //fcreg = fbreg;
  //fbreg = fareg;
  //fareg = read_wordd(wordptr_plus(areg, (breg * 2)));
  //printf("areg %x, breg %x\n ", areg, breg);
  STACK(breg, creg, UNDEFINE(creg));
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x83 - 0x28 0xF3 - fpcheckerr - check floating error */
/*FIXME: Erm, I guess we should do something here...
 * Oh, and speaking of which, C should really bomb if we get one
 * of these.  We could check for overflow and the like 'inline'*/
TVM_INSTRUCTION void ins_fpchkerr(void)
{
	//From gray tputer book.
	//error_flag' = error_flag \/ fp_error_flag
	//round_mode' = ToNearest
  SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x84 - 0x28 0xF4 - fpstnldb - floating point store non-local double */
TVM_INSTRUCTION void ins_fpstnldb(void)
{
  write_wordd(areg, fareg);
  POP_FPREG(fbreg);
  //fareg = fbreg;
  //fbreg = fcreg;
  //fcreg = UNDEFINE
  STACK(breg, creg, UNDEFINE(creg));
}

/* 0x86 - 0x28 0xF6 - fpldnlsni - floating load non local indexed single */
TVM_INSTRUCTION void ins_fpldnlsni(void)
{
  PUSH_FPREG(read_wordf(wordptr_plus(areg, breg)), SINGLE);
  //fcreg = fbreg;
  //fbreg = fareg;
  //fareg = read_wordf(wordptr_plus(areg, breg));
  //printf("areg %x, breg %x\n ", areg, breg);
  STACK(creg, UNDEFINE(breg), UNDEFINE(creg));
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x87 - 0x28 0xF7 - fpadd - floating point add */
TVM_INSTRUCTION void ins_fpadd(void)
{
	//fareg.len = fbreg.len
  if((fareg_length == fbreg_length) && (fareg_length == SINGLE))
  {
    POP_FPREG((float) ((float)fbreg) + ((float)fareg));
  } else if ((fareg_length == fbreg_length) && (fareg_length == DOUBLE)) {
    POP_FPREG((double) ((double)fbreg) + ((double)fareg));
  } else {
    set_error_flag(EFLAG_FP);
  }
	  //fbreg = fcreg;
	//fcreg = undefined
	//fp_error flag can be set by the division
	//round_mode = ToNearest
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x88 - 0x28 0xF8 - fpstnlsn - floating store non local single */
TVM_INSTRUCTION void ins_fpstnlsn(void)
{
	
	// areg /\ byteselectmask = 0
	// fareg.len = sn - means fareg is a single, not a double.
	write_wordf(areg, (float)fareg);
	//printf("fareg is %f, read_word is %f, areg* is %i\n", fareg, read_wordf(areg), areg);

	STACK(breg, creg, UNDEFINE(creg));
	POP_FPREG(fbreg);
	//fareg = fbreg;
	//fbreg = fcreg;
	//fcreg = undefined
	//
	//round_mode = ToNearest
	SET_ROUNDMODE(FE_TONEAREST);
	
}

/* 0x89 - 0x28 0xF9 - fpsub - floating point subtract */
TVM_INSTRUCTION void ins_fpsub(void)
{
	//fareg.len = fbreg.len
  if((fareg_length == fbreg_length) && (fareg_length == SINGLE))
  {
	  POP_FPREG((float) ((float)fbreg) - ((float)fareg));
  } else if ((fareg_length == fbreg_length) && (fareg_length == DOUBLE)) {
    POP_FPREG((double) ((double)fbreg) - ((double)fareg));
  } else {
    set_error_flag(EFLAG_FP);
  }
	//fbreg = fcreg;
	//fcreg = undefined
	//fp_error flag can be set by the division
	//round_mode = ToNearest
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x8A - 0x28 0xFA - fpldnldb - floating point load non-local double */
TVM_INSTRUCTION void ins_fpldnldb(void)
{
  //fcreg = fbreg;
  //fbreg = fareg;
  //read memory double in gotta see if this actually works.
  //fareg = read_wordd(areg);  
  PUSH_FPREG(read_wordd(areg), DOUBLE);
	STACK(breg, creg, UNDEFINE(creg));
}

/* 0x8B - 0x28 0xFB - fpmul - floating point multiply */
TVM_INSTRUCTION void ins_fpmul(void)
{
	//fareg.len = fbreg.len
  if((fareg_length == fbreg_length) && (fareg_length == SINGLE))
  {
    POP_FPREG((float) ((float)fbreg) * ((float)fareg));
  } else if ((fareg_length == fbreg_length) && (fareg_length == DOUBLE)) {
    POP_FPREG((double) ((double)fbreg) * ((double)fareg));
  } else {
    set_error_flag(EFLAG_FP);
  }
	//fbreg = fcreg;
	//fcreg = undefined
	//fp_error flag can be set by the division
	//round_mode = ToNearest
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x8C - 0x28 0xFC - fpdiv - floating point divide */
TVM_INSTRUCTION void ins_fpdiv(void)
{
  if((fareg_length == fbreg_length) && (fareg_length == SINGLE))
  {
    POP_FPREG((float) ((float)fbreg) / ((float)fareg));
  } else if ((fareg_length == fbreg_length) && (fareg_length == DOUBLE)) {
    POP_FPREG((double) ((double)fbreg) / ((double)fareg));
  } else {
    set_error_flag(EFLAG_FP);
  }
	//fareg.len = fbreg.len
	//fareg  = (float) ((float)fbreg) / ((float)fareg);
	//fbreg = fcreg;
	//fcreg = undefined
	//fp_error flag can be set by the division
	//round_mode = ToNearest
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x8E - 0x28 0xFE - fpldnlsn - floating point load non local single */
TVM_INSTRUCTION void ins_fpldnlsn(void) 
{
	// From graytransputer book Areg /\ byteselectmask = 0
	// This is supposed to be word-aligned.
	//if((areg | byteselectmask) == 0)
	//{
	
		/* From graytransputer book creg' = undefined */
  // creg = UNDEFINED;
	// FIXME: whats the line above become?

	/*Gray tvm book:
	 * fareg' = unpack.sn (RETYPE REAL32 Mem Areg)
	 * fberg' = fareg
	 * fcreg' = fbreg */
	/* Writing these 'in reverse' so we don't need temp variables. */
	  //fcreg = fbreg;
		//fbreg = fareg;
		
  /* Since fareg is a double, the unpack.sn (above) may happen automagically.. maybe... */
		//fareg = read_wordf( areg ); 
  PUSH_FPREG(read_wordf( areg), SINGLE);
	//	printf("fareg is %f, read_word is %f, areg* is %i\n", fareg, read_wordf(areg), areg);

	/* These come last since areg gets modified, and the above needs it.*/
  /* From graytransputer book areg' = breg */
	/* From graytransputer book breg' = creg */

	STACK(breg, creg, UNDEFINE(creg));
	/*There's also the rounding mode...*/
		//round_mode = ToNearest.. hmm.
	SET_ROUNDMODE(FE_TONEAREST);

}

/**************************
*  0x29 0xFx Starts here  *
***************************/
/* 0x91 - 0x29 0xF1 - fpnan - floating point test for NaN */
TVM_INSTRUCTION void ins_fpnan (void)
{
  //printf("fpnan \n");
  STACK(areg, areg, breg);
  areg = isnan(fareg);
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x92 - 0x29 0xF2 - fpordered - floating point ordereability */
TVM_INSTRUCTION void ins_fpordered (void)
{ 
  //printf("fpordered \n");
  STACK(areg, areg, breg);
  /*This is possibly wrong.*/
  if((isnan(fareg)) ||  (isnan(fbreg))) {
    areg = 0;
  } else {
    areg = 1;
  }
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x93 - 0x29 0xF3 - fpnotfinite - floating point test for not finite */
TVM_INSTRUCTION void ins_fpnotfinite (void)
{
  //printf("fpnotfinite fareg %f  %i  %i  %i\n", fareg, areg, breg, creg);
  STACK(areg, areg, breg);
  if(isinf(fareg)) {
    areg = 1; 
  } else {
    areg = 0;
  }
  //printf("fpnotfinite fareg %f  %i  %i  %i\n", fareg, areg, breg, creg);
    
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x94 - 0x29 0xF4 - fpgt - floating point equals */
TVM_INSTRUCTION void ins_fpgt (void)
{
  //FIXME: fpgt and fpeq may want to take into account if something is a double or a float?  
  //fareg_length = fbreg_length 
  STACK(areg, areg, breg);
  /* This one pops 2 floating regs off the stack so it does not use the pop macro*/
  if((fareg_length == fbreg_length) && (fareg_length == SINGLE))
  {
    if((float)fbreg > (float)fareg) {
      areg = 1;
    } else {
      areg = 0;
    }
  } else if ((fareg_length == fbreg_length) && (fareg_length == DOUBLE)) {
    if((double)fbreg > (double)fareg) {
      areg = 1;
    } else {
      areg = 0;
    }
  } else { 
    set_error_flag(EFLAG_FP); 
  }
   
  fareg = fcreg;
  fareg_length = fcreg_length;
  //fbreg = undefined
  //fcreg = undefined 
  // Could set fp error flag here?  Fp.Error.flg = fp.error.flag |? (fareeg E Inf U NAN) \/ (fbreg E Inf U Nan)
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x95 - 0x29 0xF5 - fpeq - floating point equals */
TVM_INSTRUCTION void ins_fpeq (void)
{
  //fareg_length = fbreg_length 
  STACK(areg, areg, breg);
  if((fareg_length == fbreg_length) && (fareg_length == SINGLE))
  {
    if((float)fareg == (float)fbreg) {
      areg = 1;
    } else {
      areg = 0;
    }
  } else if ((fareg_length == fbreg_length) && (fareg_length == DOUBLE)) {
    if((double)fareg == (double)fbreg) {
      areg = 1;
    } else {
      areg = 0;
    }
  } else { 
    set_error_flag(EFLAG_FP);
  }
  fareg = fcreg;
  fareg_length = fcreg_length;
  //fbreg = undefined
  //fcreg = undefined 
  // Could set fp error flag here?  Fp.Error.flg = fp.error.flag |? (fareeg E Inf U NAN) \/ (fbreg E Inf U Nan)
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x96 - 0x29 0xF6 - fpi32tor32 - load int32 as real32 */
TVM_INSTRUCTION void ins_fpi32tor32(void)
{
	/*push stack up and cast*/
	//fcreg = fbreg;
	//fbreg = fareg;
	/*Use read_word not read_wordf since the book says not to RETYPE*/
	//fareg = read_word(areg);
  PUSH_FPREG((float)read_word(areg), SINGLE);
	//printf("fpi32tor32 fareg %f areg %i\n", fareg, areg);

	STACK(breg, creg, UNDEFINE(creg));
	//creg = undefined;
	//round_mode = ToNearest
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x98 - 0x29 0xF8 - fpi32tor64 - load int32 as real64 */
TVM_INSTRUCTION void ins_fpi32tor64 (void)
{
   /*push stack up and cast*/
  //fcreg = fbreg;
  //fbreg = fareg;
  /*Use read_word not read_wordf since the book says not to RETYPE*/
  //Not sure if this is correct... may need a (double) cast there
  //fareg = read_word(areg);
  PUSH_FPREG((double)read_word(areg), DOUBLE);
  //printf("fpi32tor64 fareg %f areg %i\n", fareg, areg);

	STACK(breg, creg, UNDEFINE(creg));
  //round_mode = ToNearest
  SET_ROUNDMODE(FE_TONEAREST);
}


/* 0x9A - 0x29 0xFA - fpb32tor64 - load unsigned word as real64 */
TVM_INSTRUCTION void ins_fpb32tor64(void)
{
  // printf("fpb32tor64 %i \n", *(unsigned int*)areg);
  PUSH_FPREG(*(unsigned int *)areg, DOUBLE);
  STACK(breg, creg, UNDEFINE(creg));
  SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x9D - 0x29 0xFD - fprtoi32 - real to int32 */
TVM_INSTRUCTION void ins_fprtoi32(void)
{
	//fareg,len = single
	if(fareg_length == SINGLE) { 
	//printf("pre fprtoi32 fareg %f\n", fareg);
	
	/* This truncates any decimals... 
	 * I think it should actually round since that seems
	 * to be the default (need to check) */
	  fareg = rintf(fareg);
  } else {
	  fareg = rint(fareg);
  }

	//printf("post fprtoi32 fareg %f\n", fareg);
	// Should check if value outside of minint > x < maxint range...
	// round_mode = ToNearest
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x9E - 0x29 0xFE - fpstnli32 - store non local int32 */
TVM_INSTRUCTION void ins_fpstnli32(void)
{
	// Should test to see if fareg contains an value which fits 
	// into an int...
	/* IMPORTANT: This uses write_word instead of write_wordf
	 * since we are storing the float as an int (casting)i.
	 * The below line is supposed to correspond to:
	 * Mem' = Mem { Areg -> INT32 TRUNC pack (fareg)} 
	 * where pack is either pack.sn or pack.db depending
	 * on fareg.len */
	write_word(areg, (int)fareg);
	
	STACK(breg, creg, UNDEFINE(creg));
	POP_FPREG(fbreg);
	//fareg = fbreg;
	//fbreg = fcreg;
	//fcreg = undefined
	//round_mode = ToNearest
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0x9F - 0x29 0xFF - fpldzerosn - floating load zero single */
TVM_INSTRUCTION void ins_fpldzerosn(void)
{
  //Set that fbreg is single - (not done yet)
  //fareg_length = single
  PUSH_FPREG((float) 0.0, SINGLE);
  /*fcreg = fbreg;
  fbreg = fareg;
  fareg = (float) 0.0; */
	SET_ROUNDMODE(FE_TONEAREST);
}

/**************************
*  0x2A 0xFx Starts here  *
***************************/
/* 0xA0 - 0x2A 0xF0 - fpldzerodb - floating load zero double */
TVM_INSTRUCTION void ins_fpldzerodb(void)
{
  //Set that fbreg is double - (not done yet)
  //fareg_length = double
  PUSH_FPREG((double) 0.0, DOUBLE);
  /*fcreg = fbreg;
  fbreg = fareg;
  fareg = (double) 0.0;*/
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xA1 - 0x2A 0xF1 - fpint - round to floating integer */
/* This rounds FAreg, wrt Round_Mode, to a floating point number of the same format iwth an integer value. - from Gray Book.*/
TVM_INSTRUCTION void ins_fpint(void)
{
  /*So we round fareg to the nearest int by casting it an int and then back to a float... hmm.*/
  if(fareg_length == SINGLE) {
    fareg = rintf(fareg);
  } else {
    fareg = rint(fareg);
  }
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xA3 - 0x2A 0xF3 - fpdup - floating point duplicate */
TVM_INSTRUCTION void ins_fpdup(void)
{
  //fareg = fareg;
  PUSH_FPREG(fareg, fareg_length);
  //fbreg = fareg;
  //fcreg = fbreg;
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xA4 - 0x2A 0xF4 - fprev - floating reverse */
TVM_INSTRUCTION void ins_fprev(void)
{
  double tmp = fareg;
  int tmp_length = fareg_length;
  fareg = fbreg; 
  fareg_length = fbreg_length;
  fbreg = tmp;
  fbreg_length = tmp_length;
  //fcreg' = fcreg;
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xA6 - 0x2A 0xF6 - fpldnladddb - floating load non local and add double */
TVM_INSTRUCTION void ins_fpldnladddb(void)
{
	// areg /\ byteselectmask = 0
	// fareg.len = db - means fareg is a double.
  //fareg' = fareg +IEEE unpack.db (RETYPE REAL64 [Mem Areg, Mem (Index Areg 1)])
  fareg = (double) ((double)fareg) + read_wordd(areg);
  fareg_length = DOUBLE;
  //fbreg' = fbreg
  //fcreg = undefined
	STACK(breg, creg, UNDEFINE(creg));
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xA8 - 0x2A 0xF8 - fpldnlmuldb - floating load non local and multiply double */
TVM_INSTRUCTION void ins_fpldnlmuldb(void)
{
	fareg = (double)((double)fareg) * read_wordd(areg);
  fareg_length = DOUBLE;
	//fbreg = fbreg (doesn't change)
	//fcreg = undefined
	STACK(breg, creg, UNDEFINE(creg));
	//fp_error_flag can be set from float multiply
	//round_mode = ToNearest
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xAA - 0x2A 0xFA - fpldnladdsn - floating load non local and add single */
TVM_INSTRUCTION void ins_fpldnladdsn(void)
{
	// areg /\ byteselectmask = 0
	// fareg.len = sn - means fareg is a single, not a double.
	
	//fareg' = fareg +ieee unpack.sn (RETYPE REAL32 Mem Areg)
	fareg = (float) ((float)fareg) + read_wordf(areg);
  fareg_length = SINGLE;
	//printf("fareg is %f, read_word is %f, areg* is %i\n", fareg, read_wordf(areg), areg);

	//fbreg' = fbreg (no changes, so do nothing)
	//fcreg' = undefined..

	STACK(breg, creg, UNDEFINE(creg));
	//FP_Error_FLag' = can be set by error from +ieee..
	//round_mode' = ToNearest - see FIXME above.
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xAC - 0x2A 0xFC - fpldnlmulsn - floating load non local and multiply single */
TVM_INSTRUCTION void ins_fpldnlmulsn(void)
{
	fareg = (float)((float)fareg) * read_wordf(areg);
  fareg_length = SINGLE;
	//fbreg = fbreg (doesn't change)
	//fcreg = undefined
	STACK(breg, creg, UNDEFINE(creg));
	//fp_error_flag can be set from float multiply
	//round_mode = ToNearest
	SET_ROUNDMODE(FE_TONEAREST);
}

/**************************
*  0x2C 0xFx Starts here  *
***************************/
/* 0xCF - 0x2C 0xFF - fprem - floating point remainder */
/* This 'instruction' is composed of what used to be 2 instruction on the transputer #8F and #90 
 * which are fpremfirst and fpremstep respectivley */
TVM_INSTRUCTION void ins_fprem(void)
{
  //fareg_length = fbreg_length

  if((fareg_length == fbreg_length) && (fareg_length == SINGLE))
  {
    fareg = remainderf(fbreg, fareg);
  } else if ((fareg_length == fbreg_length) && (fareg_length == DOUBLE)) {
    fareg = remainder(fbreg, fareg);
  } else { 
    set_error_flag(EFLAG_FP);
  }
  //FIXME:  Really, what we want here is a check for the size of the register, and do fmod or fmodf depending.
  // fbreg unchanged?
  // fcreg = undefined.
	STACK(breg, creg, UNDEFINE(creg));
	SET_ROUNDMODE(FE_TONEAREST);

  /* From gray book: The value of fbreg' will be the quotient used to produce the remainder when (fbreg.exp - fareg.exp)
   is <= 20 for single length and <= 30 for double length operands.  This is inteded to correct the rounding error
   in argument reduction and is explained in greater detail in teh arithmetic operations section earlier. */
}

/**************************
*  0x2D 0xFx Starts here  *
***************************/
/* 0xD0 - 0x2D 0xF0 - i64toreal - 64bit into to real (converted from special .I64TOREAL) */
TVM_INSTRUCTION void ins_i64toreal(void)
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
  //STACK(areg, areg, breg);
  fareg = (double) read_word(areg);
  fareg_length = DOUBLE;
  //STACK(areg, areg, breg);
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xD1 - 0x2D 0xF1 - fpdivby2 - floating point divide by 2 */
TVM_INSTRUCTION void ins_fpdivby2(void)
{
  // Again, checking for single or double should take place...
  if(fareg_length == SINGLE)
  { 
    fareg = (float)fareg /(float) 2.0;
  } else { //if ( fareg_length && DOUBLE) {
    fareg = fareg / 2.0;
  }
  //fp error can be set from fareg * 2
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xD2 - 0x2D 0xF2 - fpmulby2 - floating point multiply by 2 */
TVM_INSTRUCTION void ins_fpmulby2(void)
{
  // Again, checking for single or double should take place...
  if(fareg_length == SINGLE)
  {
    fareg = (float)fareg * (float)2.0;
  } else { //if ( fareg_length && DOUBLE) {
    fareg = fareg * 2.0;
  }
  //fp error can be set from fareg * 2
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xD3 - 0x2D 0xF3 - fpsqrt - floating point square root */
TVM_INSTRUCTION void ins_fpsqrt(void)
{
	/* While this instruction was actually composed of 3 instructions, 
	 * it is only called by fpsqrt and hence can be implemented as a
	 * single instruction.  The question is, is it safe to assume
	 * that we have a math.h libary on every system, and does it 
	 * compromise portability? (if fp is used, then yes, probably)*/
  if(fareg_length == SINGLE) 
  { 
	  fareg = (float)sqrtf(fareg);
  } else { //if (fareg_length == DOUBLE) {
	  fareg = sqrt(fareg);
  } 
	//fbreg = undefined
	//fcreg = undefined
	//
	//fp_error_flag = can be set by error from SQRT-ieee FAREG
	//Round_Mode = ToNearest
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xD6 - 0x2D 0xF6 - fprz - floating point rounding mode to zero
 * - This is a 'new' instruction, which did exist on the transputer
 *   but was strange (0x44; 0x2A; 0xFB), and was renumbered in kroc.*/
TVM_INSTRUCTION void ins_fprz(void)
{
  //printf("setting rounding to fprz\n");
  fesetround(FE_TOWARDZERO);
}

/* 0xD7 - 0x2D 0xF7 - fpr32tor64 - real32 to real64 */
TVM_INSTRUCTION void ins_fpr32to64(void) 
{
  //FIXME: Should check for over/underflow.
  fareg = (double) fareg;
  fareg_length = DOUBLE;
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xD8 - 0x2D 0xF8 - fpr64tor32 - real64 to real32 */
TVM_INSTRUCTION void ins_fpr64to32(void) 
{
  //FIXME: Should check for over/underflow.
  fareg = (float) fareg;
  fareg_length = SINGLE;
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xD9 - 0x2D 0xF9 - fpexpdec32 - floating divide by 2^32 */
TVM_INSTRUCTION void ins_fpexpdec32(void)
{
  //FIXME: Check if this is for both single and double
  //Gray transputer book says: fareg' = fareg / IEEE 2^32
  //fareg = fareg / 4294967296;
  fareg = fareg / pow(2, 32);
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xDB - 0x2D 0xFB - fpabs - floating point absolute value */
TVM_INSTRUCTION void ins_fpabs(void)
{
  if(fareg_length == SINGLE) {
    fareg = fabsf(fareg);
  } else {
    fareg = fabs(fareg);
  }
	SET_ROUNDMODE(FE_TONEAREST);
}

/* 0xDF - 0x2D 0xFF - fpchki64 -  check that value at top of FP stack (fareg) fits in an INT32/INT64 
 * this should not be genereated for t8 but it seems to be. Wierd.  Tranx86 says so. */
TVM_INSTRUCTION void ins_fpchki64(void)
{
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

#endif /*__FPU_SUPPORT__*/

