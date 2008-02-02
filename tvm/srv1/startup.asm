//
//  startup.asm - startup code for SRV-1 robot
//    Copyright (C) 2005-2007  Surveyor Corporation
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details (www.gnu.org/licenses)
//

//
// startup.asm
//
// This is the startup code to get a supervisor program going.
//
// For BF533 and BF537 only
// 2004, 2005 Martin Strubel <hackfin@section5.ch>
//
// $Id: crt0.asm,v 1.3 2006/01/22 17:08:05 strubi Exp $
//
//

.text;

#include <defBF537.h>

#include "config.h"
#include "asmmacros.h"
#include "context.asm"

////////////////////////////////////////////////////////////////////////////
// core clock dividers -- DO NOT CHANGE!
#define CCLK_1 0x00
#define CCLK_2 0x10
#define CCLK_4 0x20
#define CCLK_8 0x30

// EBIU setup

#define SDRAM_tRP    TRP_2
#define SDRAM_tRP_num    2
#define SDRAM_tRAS    TRAS_7
#define SDRAM_tRAS_num    7
#define SDRAM_tRCD    TRCD_2
#define SDRAM_tWR    TWR_2

/*
#define SDRAM_tRP    TRP_2
#define SDRAM_tRP_num    2
#define SDRAM_tRAS    TRAS_6
#define SDRAM_tRAS_num    6
#define SDRAM_tRCD    TRCD_2
#define SDRAM_tWR    TWR_2
*/

#define SDRAM_Tref    64       /* Refresh period in milliseconds   */
#define SDRAM_NRA    4096     /* Number of row addresses in SDRAM */
#define SDRAM_CL    CL_3

#define SDRAM_SIZE    EBSZ_32
//#define SDRAM_WIDTH     EBCAW_11
//#define SDRAM_WIDTH     EBCAW_10
#define SDRAM_WIDTH     EBCAW_9
//#define SDRAM_WIDTH     EBCAW_8


/*
#define mem_SDBCTL    SDRAM_WIDTH | SDRAM_SIZE | EBE
#define mem_SDRRC    ((( CORE_CLOCK / 1000) * SDRAM_Tref)  / SDRAM_NRA) -(SDRAM_tRAS_num + SDRAM_tRP_num)
//#define mem_SDRRC    ((( CORE_CLOCK / 250) * SDRAM_Tref)  / SDRAM_NRA) -(SDRAM_tRAS_num + SDRAM_tRP_num)
#define mem_SDGCTL    ( SCTLE | SDRAM_CL | SDRAM_tRAS | SDRAM_tRP | SDRAM_tRCD | SDRAM_tWR | PSS )
*/

#define mem_SDGCTL 0x0091998d
#define mem_SDBCTL 0x0013
#define mem_SDRRC  0x0817


// little macro trick to resolve macros before concatenating:
#define _GET_CCLK(x) CCLK_##x
#define GET_CCLK(x) _GET_CCLK(x)

// Short bootstrap

.global start
start:

    sp.h = 0xFFB0;        //Set up supervisor stack in scratch pad
    sp.l = 0x0400;
    fp = sp;


////////////////////////////////////////////////////////////////////////////
// PLL and clock setups
//
//



setupPLL:
    // we have to enter the idle state after changes applied to the
    // VCO clock, because the PLL needs to lock in on the new clocks.


    p0.l = LO(PLL_CTL);
    p0.h = HI(PLL_CTL);
    r1 = w[p0](z);
    r2 = r1;  
    r0 = 0(z);
        
    r0.l = ~(0x3f << 9);
    r1 = r1 & r0;
    r0.l = ((VCO_MULTIPLIER & 0x3f) << 9);
    r1 = r1 | r0;


     p1.l = LO(SIC_IWR);  // enable PLL Wakeup Interrupt
    p1.h = HI(SIC_IWR);

    r0 = [p1];            
    bitset(r0,0);      
    [p1] = r0;
    
     w[p0] = r1;          // Apply PLL_CTL changes.
    ssync;
     
    cli r0;
     idle;    // wait for Loop_count expired wake up
    sti r0;

    // now, set clock dividers:
    p0.l = LO(PLL_DIV);
    p0.h = HI(PLL_DIV);


    // SCLK = VCOCLK / SCLK_DIVIDER
    r0.l = (GET_CCLK(CCLK_DIVIDER) | (SCLK_DIVIDER & 0x000f));


    w[p0] = r0; // set Core and system clock dividers


    /*
     * Now, Initialize the SDRAM,
     * start with the SDRAM Refresh Rate Control Register
         */


    p0.l = LO(EBIU_SDRRC);
        p0.h = HI(EBIU_SDRRC);
        r0 = mem_SDRRC;
        w[p0] = r0.l;
        ssync;

    /*
     * SDRAM Memory Bank Control Register - bank specific parameters
     */
    p0.l = (EBIU_SDBCTL & 0xFFFF);
    p0.h = (EBIU_SDBCTL >> 16);
    r0 = mem_SDBCTL;
    w[p0] = r0.l;
    ssync;

    /*
     * SDRAM Global Control Register - global programmable parameters
     * Disable self-refresh
     */
    p2.h = HI(EBIU_SDGCTL);
        p2.l = LO(EBIU_SDGCTL);
        R0 = [P2];
        BITCLR (R0, 24);

    /*
         * Check if SDRAM is already powered up, if it is, enable self-refresh
         */
    p0.h = HI(EBIU_SDSTAT);
    p0.l = LO(EBIU_SDSTAT);
    r2.l = w[p0];
    cc = bittst(r2,3);
    if !cc jump skip;
        NOP;
    BITSET (R0, 23);
skip:
    [P2] = R0;
        SSYNC;

    /* Write in the new value in the register */
        r0.l = LO(mem_SDGCTL);
        r0.h = HI(mem_SDGCTL);
    [P2] = R0;
        SSYNC;
    nop;



    // not needed in reset routine: sti r1;

////////////////////////////////////////////////////////////////////////////
// install default interrupt handlers

    p0.l = LO(EVT2);
    p0.h = HI(EVT2);

    r0.l = _NHANDLER;
    r0.h = _NHANDLER;      // NMI Handler (Int2)
    [p0++] = r0;

    r0.l = EXC_HANDLER;
    r0.h = EXC_HANDLER;    // Exception Handler (Int3)
    [p0++] = r0;
    
    [p0++] = r0;           // IVT4 isn't used

    r0.l = _HWHANDLER;
    r0.h = _HWHANDLER;     // HW Error Handler (Int5)
    [p0++] = r0;
    
    r0.l = _THANDLER;
    r0.h = _THANDLER;      // Core Timer Handler (Int6)
    [p0++] = r0;
    
    r0.l = _RTCHANDLER;
    r0.h = _RTCHANDLER;    // IVG7 Handler
    [p0++] = r0;
    
    r0.l = _I8HANDLER;
    r0.h = _I8HANDLER;     // IVG8 Handler
    [p0++] = r0;
      
    r0.l = _I9HANDLER;
    r0.h = _I9HANDLER;     // IVG9 Handler
    [p0++] = r0;
    
    r0.l = _I10HANDLER;
    r0.h = _I10HANDLER;    // IVG10 Handler
    [p0++] = r0;
     
    r0.l = _I11HANDLER;
    r0.h = _I11HANDLER;    // IVG11 Handler
    [p0++] = r0;
      
    r0.l = _I12HANDLER;
    r0.h = _I12HANDLER;    // IVG12 Handler
    [p0++] = r0;
      
    r0.l = _I13HANDLER;
    r0.h = _I13HANDLER;    // IVG13 Handler
    [p0++] = r0;

    r0.l = _I14HANDLER;
    r0.h = _I14HANDLER;    // IVG14 Handler
    [p0++] = r0;

    r0.l = _I15HANDLER;
    r0.h = _I15HANDLER;    // IVG15 Handler
    [p0++] = r0;

    // We want to run our program in supervisor mode,
    // therefore we need to leave the reset vector
    // and re-enter by raising an interrupt again.

    r0 = 0xffff(z);    // interrupt mask to enable all interrupts
    sti r0;            // set mask
    raise 15;          // raise sw interrupt
    
    p0.l = wait;
    p0.h = wait;

    reti = p0;
    rti;               // return from reset (to wait)

wait:
    jump wait;         // wait until irq 15 is being serviced.


call_main:
    [--sp] = reti;  // pushing RETI allows interrupts to occur inside all main routines

    p0.l = _main;
    p0.h = _main;

    r0.l = end;
    r0.h = end;

    rets = r0;      // return address for main()'s RTS

    jump (p0);

end:
    idle;
    jump end;

.global idle_loop
idle_loop:
    idle;
    ssync;
    jump idle_loop;


start.end:

////////////////////////////////////////////////////////////////////////////
// SETUP ROUTINES
//




////////////////////////////////////////////////////////////////////////////
// Default handlers:
//


// If we get caught in one of these handlers for some reason, 
// display the IRQ vector on the EZKIT LEDs and enter
// endless loop.

display_fail:
    bitset(r0, 5);    // mark error
#ifdef EXCEPTION_REPORT
    call EXCEPTION_REPORT;
#endif
    jump stall;


_HWHANDLER:           // HW Error Handler 5
rti;

_NHANDLER:
stall:
    idle
    jump stall;

EXC_HANDLER:          // exception handler
#ifdef EXCEPTION_REPORT
    r0 = seqstat;
    r1 = retx;
    call EXCEPTION_REPORT;
    cc = r0 == 0;
    if !cc jump cont_program;
#endif
    jump idle_loop;
cont_program:
    rtx;

_THANDLER:            // Timer Handler 6
    /* Update core timer wrap count */
    [--sp] = (r7:7, p5:5);
    p5.l = _core_timer_wrap_count;
    p5.h = _core_timer_wrap_count;
    r7 = [p5];
    r7 += 1;
    [p5] = r7;
    (r7:7, p5:5) = [sp++];
    rti;

_RTCHANDLER:          // IVG 7 Handler  
    r0.l = 7;
    jump display_fail;

_I8HANDLER:           // IVG 8 Handler
    r0.l = 8;
    jump display_fail;

_I9HANDLER:           // IVG 9 Handler
    r0.l = 9;
    jump display_fail;

_I10HANDLER:          // IVG 10 Handler
    // Save context on stack and call C handler
    save_context

    p0.l = _handle_int10;
    p0.h = _handle_int10;

    call (p0);

    restore_context
    rti;

_I11HANDLER:          // IVG 11 Handler
    r0.l = 11;
    jump display_fail;

_I12HANDLER:          // IVG 12 Handler
    r0.l = 12;
    jump display_fail;

_I13HANDLER:          // IVG 13 Handler
    r0.l = 13;
    jump display_fail;
 
_I14HANDLER:          // IVG 14 Handler
    r0.l = 14;
    jump display_fail;

_I15HANDLER:          // IVG 15 Handler
    jump call_main
    
    
////////////////////////////////////////////////////////////////////////////
// we need _atexit if we don't use a libc..
#ifndef USE_LIBC

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
