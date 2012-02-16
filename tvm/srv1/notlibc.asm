/*
 *  notlibc.asm - bits usually provided by libc for Blackfin
 *    Copyright (C) 2004-2005  Martin Strubel <hackfin@section5.ch>
 *    Copyright (C) 2005-2007  Surveyor Corporation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details (www.gnu.org/licenses)
 */

#include "bfin_config.h"
#include "asmmacros.h"

#ifndef USE_LIBC

.text;

.global _atexit;
_atexit:
    rts;

#define __ALIGN .align 4
#define __ALIGN_STR ".align 4"

.text
.extern ___udivsi3;
.global ___udivsi3;
___udivsi3:

    /* Attempt to use divide primitive first; these will handle
     * most cases, and they're quick - avoids stalls incurred by
     * testing for identities. */

    R2.H = 0x8000;
    R2 >>>= 16;                     /* R2 now 0xFFFF8000 */
    R3 = R0 | R1;                   /* If either dividend or */
    R2 = R3 & R2;                   /* divisor have bits in */
    CC = R2;                        /* top half or low half's sign */
    IF CC JUMP .LIDENTS;            /* bit, skip builtins. */

    /* Can use the builtins. */

    AQ = CC;                        /* Clear AQ (CC==0) */
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    DIVQ(R0, R1);
    R0 = R0.L (Z);
    RTS;

.LIDENTS:
    /* Test for common identities. Value to be returned is
     * placed in R2. */
    CC = R0 == 0;                   /* NR==0 => 0 */
    IF CC JUMP .LRETURN_R0;
    R2 = -1 (X);                    /* DR==0 => 0xFFFFFFFF */
    CC = R1 == 0;
    IF CC JUMP .LRETURN_IDENT;
    R2 = -R2;                       /* R2 now 1 */
    CC = R0 == R1;                  /* NR==DR => 1 */
    IF CC JUMP .LRETURN_IDENT;
    R2 = R0;
    CC = R1 == 1;                   /* DR==1 => NR */
    IF CC JUMP .LRETURN_IDENT;
    R2 = 0 (Z);
    CC = R0 < R1 (IU);
    IF CC JUMP .LRETURN_IDENT;        /* NR < DR => 0 */

    /* Idents don't match. Go for the full operation. */

    [--SP] = (R7:4);                /* Push registers R4-R7 */
    [--SP] = P3;                    /* Push P3 */
    P1 = R0;
    P2 = R1;
                                    /* If either R0 or R1 have sign set, */
                                    /* divide them by two, and note it's */
                                    /* been done. */
    R6 = 2;                         /* assume we'll shift two */
    R7 = 1;
    R3 = 0;
    R5 = R1 >> 1;
    R4 = R0 >> 1;
    CC = R1 < 0;
    IF !CC R6 = R7;                 /* R1 doesn't, so at most 1 shifted */
    IF !CC R5 = R1;                 /* and use original value. */
    CC = R0 < 0;
    IF CC R3 = R6;                  /* Number of values divided */
    IF CC R0 = R4;                  /* Shifted R0 */
    R1 = R5;                        /* Possibly-shifted R1 */
    P0 = R3;                        /* 0, 1 (NR/=2) or 2 (NR/=2, DR/=2) */

    R2 = R0;                        /* Copy dividend  */
    R3 = 0;                         /* Clear partial remainder */
    P3 = 32;                        /* Set loop counter */
    R4 = 0;                         /* Initialise quotient bit */

    LSETUP (.LULST, .LULEND) LC0 = P3;   /* Set loop counter */
.LULST:
    R6 = R2 >> 31;                  /* R6 = sign bit of R2, for carry */
    R2 <<= 1;                       /* Shift 64 bit dividend up by 1 bit */
    R3 <<= 1;
    R3 = R3 | R6;                   /* Include any carry */
    CC = R4 < 0;                    /* Check quotient(AQ) */
    R5 = -R1;                       /* If AQ==0, we'll sub divisor */
    IF CC R5 = R1;                  /* and if AQ==1, we'll add it. */
    R3= R3 + R5;                    /* Add/sub divsor to partial remainder */
    R4 = R3^R1;                     /* Generate next quotient bit */
    BITCLR(R2,0);                   /* Assume AQ==1, so "shift in" 0 */
    R5 = R4 >> 31;                  /* Get AQ */
    BITTGL(R5, 0);                  /* Invert it, to get what we'll shift */
.LULEND:
    R2 = R2 + R5;                   /* and "shift" it in. */

    CC = P0 == 0;                   /* Check how many inputs we shifted */
    IF CC JUMP .LNO_MULT;           /* if none... */
    R2 <<= 0x1;
    R6 = R2;
    CC = P0 == 1;
    IF CC R2 = R6;                  /* if 1, Q = Q*2 */
    IF !CC R1 = P2;                 /* if 2, restore stored divisor */

    R3 = R2;                        /* Copy of R2 */
    R3 *= R1;                       /* Q * divisor */
    R4 = P1;                        /* Get stored dividend(R0)  */
    R5 = R4 - R3;                   /* Z = (dividend - Q * divisor) */
    CC = R1<= R5 (IU);              /* Check if divisor <= Z? */
    R6 = CC;                        /* if yes, R6 = 1 */
    R2 = R2 + R6;                   /* if yes, add one to quotient(Q) */
.LNO_MULT:
    P3 = [SP++];                    /* Pop register P3 */
    ( R7:4) = [SP++];               /* Pop registers R4-R7 */
    R0 = R2;                        /* Store quotient */
    RTS;

.LRETURN_IDENT:
    R0 = R2;
.LRETURN_R0:
    RTS;

.text
.extern ___udivsi3;
.globl    ___umodsi3
___umodsi3:

    CC=R0==0;
    IF CC JUMP .LRETURN_RR0;        /* Return 0, if NR == 0 */
    CC= R1==0;
    IF CC JUMP .LRETURN_ZERO_VAL;    /* Return 0, if DR == 0 */
    CC=R0==R1;
    IF CC JUMP .LRETURN_ZERO_VAL;    /* Return 0, if NR == DR */
    CC = R1 == 1;
    IF CC JUMP .LRETURN_ZERO_VAL;    /* Return 0, if  DR == 1 */
    CC = R0<R1 (IU);
    IF CC JUMP .LRETURN_RR0;        /* Return dividend (R0),IF NR<DR */

    [--SP] = (R7:6);        /* Push registers and */
    [--SP] = RETS;            /* Return address */
    R7 = R0;            /* Copy of R0 */
    R6 = R1;
    SP += -12;            /* Should always provide this space */
    CALL ___udivsi3;        /* Compute unsigned quotient using ___udiv32()*/
    SP += 12;
    R0 *= R6;            /* Quotient * divisor */
    R0 = R7 - R0;            /* Dividend - (quotient * divisor) */
    RETS = [SP++];            /* Pop return address */
    ( R7:6) = [SP++];        /* And registers */
    RTS;                /* Return remainder */
.LRETURN_ZERO_VAL:
    R0 = 0;
.LRETURN_RR0:
    RTS;



.text
.extern  ___divsi3;
.globl   ___divsi3;
___divsi3 :

.align 2;


  R3 = R0 ^ R1;
  R0 = ABS R0;

  CC = V;

  r3 = rot r3 by -1;
  r1 = abs r1;      /* now both positive, r3.30 means "negate result",
                    ** r3.31 means overflow, add one to result
                    */
  cc = r0 < r1;
  if cc jump .Lret_zero;
  r2 = r1 >> 15;
  cc = r2;
  if cc jump .Lidents;
  r2 = r1 << 16;
  cc = r2 <= r0;
  if cc jump .Lidents;

  DIVS(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);
  DIVQ(R0, R1);

  R0 = R0.L (Z);
  r1 = r3 >> 31;    /* add overflow issue back in */
  r0 = r0 + r1;
  r1 = -r0;
  cc = bittst(r3, 30);
  if cc r0 = r1;
  RTS;

/* Can't use the primitives. Test common identities.
** If the identity is true, return the value in R2.
*/

.Lidents:
  CC = R1 == 0;                   /* check for divide by zero */
  IF CC JUMP .Lident_return;

  CC = R0 == 0;                   /* check for division of zero */
  IF CC JUMP .Lzero_return;

  CC = R0 == R1;                  /* check for identical operands */
  IF CC JUMP .Lident_return;

  CC = R1 == 1;                   /* check for divide by 1 */
  IF CC JUMP .Lident_return;

  R2.L = ONES R1;
  R2 = R2.L (Z);
  CC = R2 == 1;
  IF CC JUMP .Lpower_of_two;

  /* Identities haven't helped either.
  ** Perform the full division process.
  */

  P1 = 31;                        /* Set loop counter   */

  [--SP] = (R7:5);                /* Push registers R5-R7 */
  R2 = -R1;
  [--SP] = R2;
  R2 = R0 << 1;                   /* R2 lsw of dividend  */
  R6 = R0 ^ R1;                   /* Get sign */
  R5 = R6 >> 31;                  /* Shift sign to LSB */

  R0 = 0 ;                        /* Clear msw partial remainder */
  R2 = R2 | R5;                   /* Shift quotient bit */
  R6 = R0 ^ R1;                   /* Get new quotient bit */

  LSETUP(.Llst,.Llend)  LC0 = P1;   /* Setup loop */
.Llst:   R7 = R2 >> 31;            /* record copy of carry from R2 */
        R2 = R2 << 1;             /* Shift 64 bit dividend up by 1 bit */
        R0 = R0 << 1 || R5 = [SP];
        R0 = R0 | R7;             /* and add carry */
        CC = R6 < 0;              /* Check quotient(AQ) */
                                  /* we might be subtracting divisor (AQ==0) */
        IF CC R5 = R1;            /* or we might be adding divisor  (AQ==1)*/
        R0 = R0 + R5;             /* do add or subtract, as indicated by AQ */
        R6 = R0 ^ R1;             /* Generate next quotient bit */
        R5 = R6 >> 31;
                                  /* Assume AQ==1, shift in zero */
        BITTGL(R5,0);             /* tweak AQ to be what we want to shift in */
.Llend:  R2 = R2 + R5;             /* and then set shifted-in value to
                                  ** tweaked AQ.
                                  */
  r1 = r3 >> 31;
  r2 = r2 + r1;
  cc = bittst(r3,30);
  r0 = -r2;
  if !cc r0 = r2;
  SP += 4;
  (R7:5)= [SP++];                 /* Pop registers R6-R7 */
  RTS;

.Lident_return:
  CC = R1 == 0;                   /* check for divide by zero  => 0x7fffffff */
  R2 = -1 (X);
  R2 >>= 1;
  IF CC JUMP .Ltrue_ident_return;

  CC = R0 == R1;                  /* check for identical operands => 1 */
  R2 = 1 (Z);
  IF CC JUMP .Ltrue_ident_return;

  R2 = R0;                        /* assume divide by 1 => numerator */
  /*FALLTHRU*/

.Ltrue_ident_return:
  R0 = R2;                        /* Return an identity value */
  R2 = -R2;
  CC = bittst(R3,30);
  IF CC R0 = R2;
.Lzero_return:
  RTS;                            /* ...including zero */

.Lpower_of_two:
  /* Y has a single bit set, which means it's a power of two.
  ** That means we can perform the division just by shifting
  ** X to the right the appropriate number of bits
  */

  /* signbits returns the number of sign bits, minus one.
  ** 1=>30, 2=>29, ..., 0x40000000=>0. Which means we need
  ** to shift right n-signbits spaces. It also means 0x80000000
  ** is a special case, because that *also* gives a signbits of 0
  */

  R2 = R0 >> 31;
  CC = R1 < 0;
  IF CC JUMP .Ltrue_ident_return;

  R1.l = SIGNBITS R1;
  R1 = R1.L (Z);
  R1 += -30;
  R0 = LSHIFT R0 by R1.L;
  r1 = r3 >> 31;
  r0 = r0 + r1;
  R2 = -R0;                       // negate result if necessary
  CC = bittst(R3,30);
  IF CC R0 = R2;
  RTS;

.Lret_zero:
  R0 = 0;
  RTS;

#endif
