/******************************************************************************
 *
 * Fluke UART code
 * 
 * The LPC2106 data sheet is necessary for understanding this code.
 * 
 * keith.ohara@gatech.edu
 * April 2008
 * IPRE Fluke Firmware
 *
 ******************************************************************************/


/******************************************************************************
 *
 *
 * This module provides interface routines to the LPC ARM UARTs.
 * Copyright 2004, R O SoftWare
 * No guarantees, warrantees, or promises, implied or otherwise.
 * May be used for hobby or commercial purposes provided copyright
 * notice remains intact.
 *
 * reduced to see what has to be done for minimum UART-support by mthomas
 *****************************************************************************/

#include "lpc/lpc210x.h"
#include "fluke_uart.h"
#include "fluke.h"

/* on LPC210x: UART0 TX-Pin=P0.2, RX-Pin=P0.1 
   PINSEL0 has to be set to "UART-Function" = Function "01" 
   for Pin 0.0 and 0.1 */
   

/*    baudrate divisor - use UART_BAUD macro
 *    mode - see typical modes (uart.h)
 *    fmode - see typical fmodes (uart.h)
 *    NOTE: uart0Init(UART_BAUD(9600), UART_8N1, UART_FIFO_8); 
 */
void uart0Init(uint16_t baud, uint8_t mode, uint8_t fmode)
{
  // setup Pin Function Select Register (Pin Connect Block) 
  // make sure old values of Bits 0-4 are masked out and
  // set them according to UART0-Pin-Selection
  PINSEL0 = (PINSEL0 & ~U0_PINMASK) | U0_PINSEL;

  U0IER = 0x00;             // disable all interrupts
  U0IIR = 0x00;             // clear interrupt ID register
  U0LSR = 0x00;             // clear line status register

  // set the baudrate - DLAB must be set to access DLL/DLM
  U0LCR = (1<<U0LCR_DLAB); // set divisor latches (DLAB)

  U0DLL = (uint8_t)baud;         // set for baud low byte
  U0DLM = (uint8_t)(baud >> 8);  // set for baud high byte
  
  // set the number of characters and other
  // user specified operating parameters
  // Databits, Parity, Stopbits - Settings in Line Control Register
  U0LCR = (mode & ~(1<<U0LCR_DLAB)); // clear DLAB "on-the-fly"
  // setup FIFO Control Register (fifo-enabled + xx trig) 
  U0FCR = fmode;
}

int uart0Putch(int ch)
{
  while (!(U0LSR & ULSR_THRE))          // wait for TX buffer to empty
    continue;                           // also either WDOG() or swap()

  U0THR = (uint8_t)ch;  // put char to Transmit Holding Register
  return (uint8_t)ch;      // return char ("stdio-compatible"?)
}

int uart0TxEmpty(void)
{
  return (U0LSR & (ULSR_THRE | ULSR_TEMT)) == (ULSR_THRE | ULSR_TEMT);
}

void uart0TxFlush(void)
{
  U0FCR |= UFCR_TX_FIFO_RESET;          // clear the TX fifo
}


/* Returns: character on success, -1 if no character is available */
int uart0Getch(void)
{
  if (U0LSR & ULSR_RDR)                 // check if character is available
    return U0RBR;                       // return character

  return -1;
}

int uart0GetchBlock(void)
{
  while (!(U0LSR & ULSR_RDR));
  return U0RBR;
}


/*    baudrate divisor - use UART_BAUD macro
 *    mode - see typical modes (uart.h)
 *    fmode - see typical fmodes (uart.h)
 *    NOTE: uart1Init(UART_BAUD(9600), UART_8N1, UART_FIFO_8); 
 */
void uart1Init(uint16_t baud, uint8_t mode, uint8_t fmode)
{
  // setup Pin Function Select Register (Pin Connect Block) 
  // make sure old values of Bits 0-4 are masked out and
  // set them according to UART1-Pin-Selection
  PINSEL0 = (PINSEL0 & ~U1_PINMASK) | U1_PINSEL;
  
  // turn on CTS
  PINSEL0 = (PINSEL0 & ~U1CTS_PINMASK) | U1CTS_PINSEL;
  
  U1IER = 0x00;             // disable all interrupts
  U1IIR = 0x00;             // clear interrupt ID register
  U1LSR = 0x00;             // clear line status register

  // set the baudrate - DLAB must be set to access DLL/DLM
  U1LCR = (1<<U1LCR_DLAB); // set divisor latches (DLAB)
  U1DLL = (uint8_t)baud;         // set for baud low byte
  U1DLM = (uint8_t)(baud >> 8);  // set for baud high byte
  
  // set the number of characters and other
  // user specified operating parameters
  // Databits, Parity, Stopbits - Settings in Line Control Register
  U1LCR = (mode & ~(1<<U1LCR_DLAB)); // clear DLAB "on-the-fly"
  // setup FIFO Control Register (fifo-enabled + xx trig) 
  U1FCR = fmode;
}

int uart1Putch(int ch)
{
  while (!(U1LSR & ULSR_THRE))    // wait for TX buffer to empty
    continue;                         // also either WDOG() or swap()

  U1THR = (uint8_t)ch;  // put char to Transmit Holding Register
  return (uint8_t)ch;       // return char ("stdio-compatible"?)
}

int uart1PutchCTS(int ch)
{  

  while (!(U1MSR & UMSR_CTS) || !(U1LSR & ULSR_THRE))  // wait for CTS to change
    {
      ;
    }
  
  
  U1THR = (uint8_t)ch;  // put char to Transmit Holding Register
  
  return (uint8_t)ch;       // return char ("stdio-compatible"?)
}

int uart1TxEmpty(void)
{
  return (U1LSR & (ULSR_THRE | ULSR_TEMT)) == (ULSR_THRE | ULSR_TEMT);
}

void uart1TxFlush(void)
{
  U1FCR |= UFCR_TX_FIFO_RESET;          // clear the TX fifo
}

/* Returns: character on success, -1 if no character is available */
int uart1Getch(void)
{
  if (U1LSR & ULSR_RDR)                 // check if character is available
    return U1RBR;                       // return character

  return -1;
}

int uart1GetchRTS(void)
{
  uart1_set_rts();

  // check if character is available
  if (U1LSR & ULSR_RDR)
    {
      uart1_clear_rts();
      // return character
      return U1RBR;   
    }
  
  
  uart1_clear_rts();  
  return -1;
}

int uart1GetchBlock(void)
{
  while (!(U1LSR & ULSR_RDR));
  return U1RBR;
}

int uart1GetchBlockRTS(void)
{
  uart1_set_rts();
  while (!(U1LSR & ULSR_RDR));

  uart1_clear_rts();
  
  return U1RBR;
}

inline void uart1_set_rts()
{
  //U1MCR |= UMCR_RTS;
  IOCLR = B_RTS;
}

inline void uart1_clear_rts()
{
  //U1MCR &= ~UMCR_RTS;
  IOSET = B_RTS;
}

inline int uart1_cts()
{
  return (U1MSR & UMSR_CTS);
}
