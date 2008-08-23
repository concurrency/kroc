/*******************************************************************************
 *
 * UART header for baud rates and API
 * 
 * The LPC2106 data sheet is necessary for understanding this code.
 * 
 * keith.ohara@gatech.edu
 * April 2008
 * IPRE Fluke Firmware
 *
 ******************************************************************************/


/******************************************************************************
 * based on software from:
 * Copyright 2004, R O SoftWare
 * No guarantees, warrantees, or promises, implied or otherwise.
 * May be used for hobby or commercial purposes provided copyright
 * notice remains intact.
 * 
 * reduced to learn what has to be done to enable and use UART0
 *****************************************************************************/
#ifndef FLUKE_UART_H
#define FLUKE_UART_H

#include "lpc/types.h"

#include "lpc/lpc210x.h"
#include "lpc/lpcUART.h"

/* 20080823 MCJ
 * This was all in lpc/uart.h, so I commented it out here, and 
 * let that header do the work. Ultimately, this code can be removed,
 * and I would like to see all of the Fluke/TVM UART code 
 * consolidated. 
 */
// #define UART_BAUD(baud) (uint16_t)(((FOSC*PLL_M/VPBDIV_VAL) / ((baud) * 16.0)) + 0.5)

#if 0
void uart0Init(uint16_t baud, uint8_t mode, uint8_t fmode);
int uart0Putch(int ch);
int uart0TxEmpty(void);
void uart0TxFlush(void);
int uart0Getch(void);
int uart0GetchBlock(void);

void uart1Init(uint16_t baud, uint8_t mode, uint8_t fmode);
int uart1Putch(int ch);
int uart1PutchCTS(int ch);

int uart1TxEmpty(void);
void uart1TxFlush(void);

int uart1GetchRTS(void);
int uart1GetchBlockRTS(void);
int uart1Getch(void);
int uart1GetchBlock(void);
#endif 

inline void uart1_set_rts();
inline void uart1_clear_rts();
inline int uart1_cts();

#define PINSEL_BITPIN0  0
#define PINSEL_BITPIN1  2
// #define PINSEL_BITPIN2  4
#define PINSEL_FIRST_ALT_FUNC   1
// #define PINSEL_SECOND_ALT_FUNC   2

// Values of Bits 0-3 in PINSEL to activate UART0
#define U0PINSEL    ((PINSEL_FIRST_ALT_FUNC<<PINSEL_BITPIN0)|(PINSEL_FIRST_ALT_FUNC<<PINSEL_BITPIN1))
// Mask of Bits 0-4
#define U0PINMASK      (0x0000000F)    /* PINSEL0 Mask for UART0 */

// U0_LCR devisor latch bit 
#define U0LCR_DLAB  7

#define PINSEL1_BITPIN0  16
#define PINSEL1_BITPIN1  18
#define PINSEL1_RTS_BITPIN  20
#define PINSEL1_CTS_BITPIN  22
#define PINSEL1_FIRST_ALT_FUNC   1

#define U1LCR_DLAB  7

// Values of Bits 16-19 in PINSEL to activate UART1
#define U1PINSEL    ((PINSEL1_FIRST_ALT_FUNC<<PINSEL1_BITPIN0)|(PINSEL1_FIRST_ALT_FUNC<<PINSEL1_BITPIN1))
// Mask of Bits 16-21
#define U1PINMASK   ((1 << 16) | (1 << 17) | (1 << 18) | (1 << 19))

#define U1CTS_PINSEL    ((PINSEL1_FIRST_ALT_FUNC<<PINSEL1_CTS_BITPIN))
#define U1CTS_PINMASK   ((1 << 22) | (1 << 23))

// U0_LCR devisor latch bit 
#define U1LCR_DLAB  7

#endif
