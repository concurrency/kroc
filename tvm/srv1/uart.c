/*
 * uart.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

#include "srv.h"

#define CTS_PIN		6
#define	RTS_PIN		0
#define CTS_MASK	(1 << CTS_PIN)
#define RTS_MASK	(1 << RTS_PIN)

volatile WORDPTR 		uart0_rx_channel= (WORDPTR) NOT_PROCESS_P;
volatile WORDPTR 		uart0_tx_channel= (WORDPTR) NOT_PROCESS_P;

static volatile BYTEPTR		rx_ptr		= (BYTEPTR) NULL_P;
static volatile unsigned char 	rx_buffer	= '\0';
static volatile short		rx_pending	= 0;
static volatile short		rx_intr		= 0;

static volatile BYTEPTR		tx_ptr		= (BYTEPTR) NULL_P;
static volatile short		tx_pending	= 0;
static volatile short		tx_intr		= 0;

void init_uart (void)
{
	unsigned char temp;

	/* Configure port H for flow control CTS */
	*pPORTHIO_DIR	|= CTS_MASK;
	/* Configure port H for flow control RTS */
	*pPORTHIO_INEN	|= RTS_MASK;
	/* Raise CTS to block input */
	*pPORTHIO_SET	= CTS_MASK;
	
	/* Enable UART pins on port F */
	*pPORTF_FER 	|= 0x000f;

	/* Enable UART0 clocks */
	*pUART0_GCTL	= UCEN;
	/* Switch on divisor programming */
	*pUART0_LCR	= DLAB;
	/* Program divisor */
	*pUART0_DLL	= UART0_DIVIDER;
	*pUART0_DLH	= UART0_DIVIDER >> 8;
	/* Set operational mode (disables divisor programming) */
	*pUART0_LCR	= WLS(8); /* 8 bit, no parity, one stop bit */

	/* Reads to clear possible pending errors / irqs */
	temp = *pUART0_RBR;
	temp = *pUART0_LSR;
	temp = *pUART0_IIR;
	SSYNC;

	/* Enable receive interrupts on IVG 10 */
	*pSIC_IAR1	= (*pSIC_IAR1 & (~P11_IVG(0xf))) | P11_IVG(10);
	*pSIC_IMASK	|= IRQ_DMA8;
	*pUART0_IER	|= ERBFI;
	SSYNC;

	/* Enable RTS interrupts on IVG 14 */
	*pSIC_IAR2	= (*pSIC_IAR2 & (~P17_IVG(0xf))) | P17_IVG(14);
	*pSIC_IMASK	|= IRQ_PFA_PORTH;
	*pPORTHIO_EDGE	|= RTS_MASK; /* interrupts on edges */
	*pPORTHIO_POLAR	|= RTS_MASK; /* falling edges */
	*pPORTHIO_MASKA	|= RTS_MASK; /* enable interrupt */
	SSYNC;
}

void handle_int10 (void)
{
	unsigned char buffer;

	/* Raise (clear) CTS */
	*pPORTHIO_SET	= CTS_MASK;

	/* Read character (clears interrupt condition) */
	buffer		= *pUART0_RBR;

	/* Is anything waiting for the character? */
	if (rx_ptr != (BYTEPTR) NULL_P) {
		/* Yes; give it the data and trigger requeue */
		write_byte (rx_ptr, (BYTE) buffer);
		rx_ptr		= (BYTEPTR) NULL_P;
		rx_intr		= 1;
		raise_tvm_interrupt ();
	} else {
		/* No; buffer the character */
		rx_buffer	= buffer;
		rx_pending++;
	}
}

void handle_int14 (void)
{
	*pPORTHIO_CLEAR = RTS_MASK;
}

void complete_uart0_interrupt (ECTX ectx)
{
	WORDPTR wptr;

	if (rx_intr && ((wptr = uart0_rx_channel) != (WORDPTR) NOT_PROCESS_P)) {
		uart0_rx_channel	= (WORDPTR) NOT_PROCESS_P;
		rx_intr			= 0;
		ectx->add_to_queue (ectx, wptr);
	}
}

int uart0_in (ECTX ectx, WORD count, BYTEPTR pointer)
{
	unsigned short imask;
	int reschedule;
	
	DISABLE_INTERRUPTS (imask);

	if (rx_pending) {
		write_byte (pointer, (BYTE) rx_buffer);
		rx_pending	= 0;
		reschedule	= 0;
	} else {
		uart0_rx_channel= ectx->wptr;
		rx_ptr		= pointer;
		reschedule	= 1;
	}

	ENABLE_INTERRUPTS (imask);

	if (reschedule) {
		/* Lower (set) CTS */
		*pPORTHIO_CLEAR = CTS_MASK;
		/* Save instruction pointer */
		WORKSPACE_SET (ectx->wptr, WS_IPTR, (WORD) ectx->iptr);
		/* Reschedule */
		return ectx->run_next_on_queue (ectx);
	} else {
		return ECTX_CONTINUE;
	}
}

void uart0_send_char (const unsigned char c)
{
	/* Wait for RTS to go low (remote ready) */
	while (*pPORTHIO & RTS_MASK) {
		continue;
	}

	/* Wait for UART0 send buffer to be ready */
	while (!(*pUART0_LSR & THRE)) {
		continue;
	}

	/* Send data */
	*pUART0_THR = c;
}

void uart0_send_string (const char *str)
{
	const unsigned char *p = (unsigned char *) str;
	unsigned char c;

	while ((c = *p++) != '\0') {
		uart0_send_char (c);
	}
}


int uart0_out (ECTX ectx, WORD count, BYTEPTR pointer)
{
	/* If count is the size of a word then throw away data,
	 * as it will be the count from a counted array
	 * output.  This should change in future.
	 */
	if (count != sizeof(WORD)) {
		while (count--) {
			uart0_send_char (read_byte (pointer));
			pointer = byteptr_plus (pointer, 1);
		}
	}

	return ECTX_CONTINUE;
}

