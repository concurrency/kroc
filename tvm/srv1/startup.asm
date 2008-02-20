/*
 *  startup.asm - startup code for SRV-1 robot
 *    Copyright (C) 2004-2005  Martin Strubel <hackfin@section5.ch>
 *    Modifications Copyright (C) 2005-2007  Surveyor Corporation
 *    Mofifications Copyright (C) 2008  Carl Ritson <cgr@kent.ac.uk>
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

#include <defBF537.h>

#include "bfin_config.h"
#include "asmmacros.h"

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

.text;

.global start
start:

    sp.h = 0xFFB0;        // Set up supervisor stack in scratch pad
    sp.l = 0x0400;
    fp = sp;


setup_PLL:
    /* Setup PLL and system clocks. */
    // We have to enter the idle state after changes applied to the
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


setup_SDRAM:
    /* Initialize the SDRAM. */

    /* Manual suggests waiting 100us for power to stablise before
     * enabling SDRAM; we don't *need* this, but it seems like a
     * good idea.
     * At 500MHz, 100us is 50000 instructions, we do 65536.
     */
    p0.l = 0xffff;
    p0.h = 0;
    loop sdram_wait LC0 = p0;
    loop_begin sdram_wait;
    nop;
    loop_end sdram_wait;

    /* 
     * SDRAM Refresh Rate Control Register
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
    r0 = [p2];
    bitclr (R0, 24);

    /*
     * Check if SDRAM is already powered up, if it is, enable self-refresh
     */
    p0.h = HI(EBIU_SDSTAT);
    p0.l = LO(EBIU_SDSTAT);
    r2.l = w[p0];
    cc = bittst(r2,3);
    if !cc jump skip;
    nop;
    bitset (R0, 23);

skip:
    [p2] = r0;
    ssync;

    /* Write in the new value in the register */
    r0.l = LO(mem_SDGCTL);
    r0.h = HI(mem_SDGCTL);
    [p2] = r0;
    ssync;
    nop;


setup_events:
    /* Install default event handlers. */

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


setup_main:
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
    jump stall;
    /* rti; */

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
    // Save context on stack and call C handler
    save_context

    p0.l = _handle_int8;
    p0.h = _handle_int8;

    call (p0);

    restore_context
    rti;

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
    
