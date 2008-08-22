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
#ifndef INC_UART_H
#define INC_UART_H

#include "inttypes.h"

#include "lpc210x.h"
#include "lpcUART.h"

///////////////////////////////////////////////////////////////////////////////
// use the following macros to determine the 'baud' parameter values
// for uart0Init() and uart1Init()
// CAUTION - 'baud' SHOULD ALWAYS BE A CONSTANT or
// a lot of code will be generated.
// Baud-Rate is calculated based on pclk (VPB-clock)
// the devisor must be 16 times the desired baudrate
#define UART_BAUD(baud) (uint16_t)(((FOSC*PLL_M/VPBDIV_VAL) / ((baud) * 16.0)) + 0.5)

///////////////////////////////////////////////////////////////////////////////
// Definitions for typical UART 'baud' settings
#define B1200         UART_BAUD(1200)
#define B2400         UART_BAUD(2400)
#define B4800         UART_BAUD(4800)
#define B9600         UART_BAUD(9600)
#define B19200        UART_BAUD(19200)
#define B38400        UART_BAUD(38400)
#define B57600        UART_BAUD(57600)
#define B115200       UART_BAUD(115200)
#define B230400       UART_BAUD(230400)
#define B460800       UART_BAUD(460800)
#define B921600       UART_BAUD(921600)
#define B1843200      UART_BAUD(1843200)
#define B3686400      UART_BAUD(3686400)

#define NB1200        0
#define NB2400        1
#define NB4800        2
#define NB9600        3
#define NB19200       4
#define NB38400       5
#define NB57600       6
#define NB115200      7
#define NB230400      8
#define NB460800      9
#define NB921600      10
#define NB1843200     11
#define NB3686400     12

///////////////////////////////////////////////////////////////////////////////
// Definitions for typical UART 'mode' settings
#define UART_8N1      (uint8_t)(ULCR_CHAR_8 + ULCR_PAR_NO   + ULCR_STOP_1)
#define UART_7N1      (uint8_t)(ULCR_CHAR_7 + ULCR_PAR_NO   + ULCR_STOP_1)
#define UART_8N2      (uint8_t)(ULCR_CHAR_8 + ULCR_PAR_NO   + ULCR_STOP_2)
#define UART_7N2      (uint8_t)(ULCR_CHAR_7 + ULCR_PAR_NO   + ULCR_STOP_2)
#define UART_8E1      (uint8_t)(ULCR_CHAR_8 + ULCR_PAR_EVEN + ULCR_STOP_1)
#define UART_7E1      (uint8_t)(ULCR_CHAR_7 + ULCR_PAR_EVEN + ULCR_STOP_1)
#define UART_8E2      (uint8_t)(ULCR_CHAR_8 + ULCR_PAR_EVEN + ULCR_STOP_2)
#define UART_7E2      (uint8_t)(ULCR_CHAR_7 + ULCR_PAR_EVEN + ULCR_STOP_2)
#define UART_8O1      (uint8_t)(ULCR_CHAR_8 + ULCR_PAR_ODD  + ULCR_STOP_1)
#define UART_7O1      (uint8_t)(ULCR_CHAR_7 + ULCR_PAR_ODD  + ULCR_STOP_1)
#define UART_8O2      (uint8_t)(ULCR_CHAR_8 + ULCR_PAR_ODD  + ULCR_STOP_2)
#define UART_7O2      (uint8_t)(ULCR_CHAR_7 + ULCR_PAR_ODD  + ULCR_STOP_2)

#define NUART_8N1     0
#define NUART_7N1     1
#define NUART_8N2     2
#define NUART_7N2     3
#define NUART_8E1     4
#define NUART_7E1     5
#define NUART_8E2     6
#define NUART_7E2     7
#define NUART_8O1     8
#define NUART_7O1     9
#define NUART_8O2     10
#define NUART_7O2     11

///////////////////////////////////////////////////////////////////////////////
// Definitions for typical UART 'fmode' settings
#define UART_FIFO_OFF (0x00)
#define UART_FIFO_1   (uint8_t)(UFCR_FIFO_ENABLE + UFCR_FIFO_TRIG1)
#define UART_FIFO_4   (uint8_t)(UFCR_FIFO_ENABLE + UFCR_FIFO_TRIG4)
#define UART_FIFO_8   (uint8_t)(UFCR_FIFO_ENABLE + UFCR_FIFO_TRIG8)
#define UART_FIFO_14  (uint8_t)(UFCR_FIFO_ENABLE + UFCR_FIFO_TRIG14)

#define NUART_FIFO_OFF 0
#define NUART_FIFO_1   1
#define NUART_FIFO_4   2
#define NUART_FIFO_8   3
#define NUART_FIFO_14  4

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

inline void uart1_set_rts();
inline void uart1_clear_rts();
inline int uart1_cts();


#define PINSEL_BITPIN0  0
#define PINSEL_BITPIN1  2
// #define PINSEL_BITPIN2  4
#define PINSEL_FIRST_ALT_FUNC   1
// #define PINSEL_SECOND_ALT_FUNC   2

// Values of Bits 0-3 in PINSEL to activate UART0
#define UART0_PINSEL    ((PINSEL_FIRST_ALT_FUNC<<PINSEL_BITPIN0)|(PINSEL_FIRST_ALT_FUNC<<PINSEL_BITPIN1))
// Mask of Bits 0-4
#define UART0_PINMASK      (0x0000000F)    /* PINSEL0 Mask for UART0 */

// U0_LCR devisor latch bit 
#define UART0_LCR_DLAB  7

#define PINSEL1_BITPIN0  16
#define PINSEL1_BITPIN1  18
#define PINSEL1_RTS_BITPIN  20
#define PINSEL1_CTS_BITPIN  22
#define PINSEL1_FIRST_ALT_FUNC   1

#define UART1_LCR_DLAB  7

// Values of Bits 16-19 in PINSEL to activate UART1
#define UART1_PINSEL    ((PINSEL1_FIRST_ALT_FUNC<<PINSEL1_BITPIN0)|(PINSEL1_FIRST_ALT_FUNC<<PINSEL1_BITPIN1))
// Mask of Bits 16-21
#define UART1_PINMASK   ((1 << 16) | (1 << 17) | (1 << 18) | (1 << 19))

#define UART1_CTS_PINSEL    ((PINSEL1_FIRST_ALT_FUNC<<PINSEL1_CTS_BITPIN))
#define UART1_CTS_PINMASK   ((1 << 22) | (1 << 23))

// U0_LCR devisor latch bit 
#define UART1_LCR_DLAB  7

#endif
