/******************************************************************************
 *
 * A simple program which sends a greeting to UART0, then echos
 * characters on UART0 and blinks an LED every 1/2 second.
 * from: Bill Knight, R O SoftWare <BillK@rosw.com>
 * ----------------------------------------------------------------------------
 *
 * - Adapted to the Olimex LPC-P2106 demo-board (Philips LPC2106).
 * - Sends message if button/switch on demo-board is hit.
 * - Slightly modified and extended as WinARM demo-application.
 * by Martin THOMAS <eversmith@heizung-thomas.de>
 *
 * $RCSfile: $
 * $Revision: $
 *
 *****************************************************************************/
#include "types.h"
#include "LPC210x.h"
#include "config.h"
#include "armVIC.h"
#include "sysTime.h"
#include "uart.h"
#include "fluke.h"

/******************************************************************************
 *
 * Function Name: lowInit()
 *
 * Description:
 *    This function starts up the PLL then sets up the GPIO pins before
 *    waiting for the PLL to lock.  It finally engages the PLL and
 *    returns
 *
 * Calling Sequence: 
 *    void
 *
 * Returns:
 *    void
 *
 *****************************************************************************/
static void lowInit(void)
{

  // set PLL multiplier & divisor.
  // values computed from config.h
  PLLCFG = PLLCFG_MSEL | PLLCFG_PSEL;

  // enable PLL
  PLLCON = PLLCON_PLLE;
  PLLFEED = 0xAA;                       // Make it happen.  These two updates
  PLLFEED = 0x55;                       // MUST occur in sequence.

  // wait for PLL lock
  while (!(PLLSTAT & PLLSTAT_LOCK))
    continue;

  // enable & connect PLL
  PLLCON = PLLCON_PLLE | PLLCON_PLLC;
  PLLFEED = 0xAA;                       // Make it happen.  These two updates
  PLLFEED = 0x55;                       // MUST occur in sequence.

  // setup & enable the MAM
  MAMTIM = MAMTIM_CYCLES;
  MAMCR = MAMCR_FULL;

  // set the peripheral bus speed
  // value computed from config.h
  VPBDIV = VPBDIV_VALUE;                // set the peripheral bus clock speed

}

/******************************************************************************
 *
 * Function Name: sysInit()
 *
 * Description:
 *    This function is responsible for initializing the program
 *    specific hardware
 *
 * Calling Sequence: 
 *    void
 *
 * Returns:
 *    void
 *
 *****************************************************************************/
static void sysInit(void)
{
  lowInit();                            // setup clocks and processor port pins

  // set the interrupt controller defaults
#if defined(RAM_RUN)
  MEMMAP = MEMMAP_SRAM;                 // map interrupt vectors space into SRAM
#elif defined(ROM_RUN)
  MEMMAP = MEMMAP_FLASH;                // map interrupt vectors space into FLASH
#else
#error RUN_MODE not defined!
#endif
  VICIntEnClear = 0xFFFFFFFF;           // clear all interrupts
  VICIntSelect = 0x00000000;            // clear all FIQ selections
  VICDefVectAddr = (uint32_t)reset;     // point unvectored IRQs to reset()

  //  wdtInit();                        // initialize the watchdog timer
  //  initSysTime();                        // initialize the system timer
  
  /* Set up timer, also wiring in an ISR to ack the match IRQ */
  init_timer();
}

/******************************************************************************
 *
 * Function Name: powerInit()
 *
 * Description:
 *    This function is responsible for turning off devices that are not used. 
 *
 * Calling Sequence: 
 *    void
 *
 * Returns:
 *    void
 *
 *****************************************************************************/
static void powerInit(void)
{
  /* Turn off the listed devices (everything is on by default) */
  PCONP &= ~(PCONP_PCTIM1 | PCONP_PCPWM0 | 
             PCONP_PCI2C  | PCONP_PCSPI  | 
             PCONP_PCRTC);
}

/******************************************************************************
 *
 * Function Name: pinInit()
 *
 * Description:
 *    Sets up the direction of the io pins
 *
 * Calling Sequence: 
 *    void
 *
 * Returns:
 *    void
 *
 *****************************************************************************/
static void pinInit(void)
{
  IODIR = LED;
}

/******************************************************************************
 *
 * Function Name: main()
 *
 * Description:
 *    This function is the program entry point.  After initializing the
 *    system, it sends a greeting out UART0 then enters an endless loop
 *    echoing chracters on the UART and blinking an LED every half
 *    second.
 *
 * Calling Sequence: 
 *    void
 *
 * Returns:
 *    void
 *
 *****************************************************************************/
int main(void)
{
  powerInit();
  pinInit();
  sysInit();

  init_uart0(UART_BAUD(HOST_BAUD), UART_8N1, UART_FIFO_1); /* setup the UART0 */

/* #if defined(UART0_TX_INT_MODE) || defined(UART0_RX_INT_MODE) || defined(TIMER_INT_MODE)*/
  enableIRQ();
/* #endif */
 
  debug_print_str(version_string);
  debug_print_chr('\n');

  return run_tvm();
}
