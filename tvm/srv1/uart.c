/*
 * uart.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

#include "srv.h"

volatile WORDPTR 		uart0_channel 	= (WORDPTR) NOT_PROCESS_P;
static volatile BYTEPTR		uart0_ptr	= (BYTEPTR) NULL_P;
static volatile unsigned char 	uart0_buffer	= '\0';
static volatile short		uart0_pending	= 0;
static volatile short		uart0_intr	= 0;

void init_uart (void)
{
	unsigned char temp;

	/* Configure port H pin 6 (PH6) for flow control CTS */
	*pPORTHIO_DIR	|= 0x0040;
	/* Configure PH0 for flow control RTS */
	*pPORTHIO_INEN	= 0x0001;
	/* Raise PH6 to block input */
	*pPORTHIO	|= 0x0040;
	
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

	/* Enable receive interrupts */
	*pSIC_IMASK |= IRQ_DMA8;
	*pUART0_IER |= ERBFI;
	SSYNC;
}

void handle_int10 (void)
{
	unsigned char buffer;

	/* Raise (clear) CTS */
	*pPORTHIO	= *pPORTHIO | 0x0040;

	/* Read character (clears interrupt condition) */
	buffer		= *pUART0_RBR;

	/* Is anything waiting for the character? */
	if (uart0_ptr != (BYTEPTR) NULL_P) {
		/* Yes; give it the data and trigger requeue */
		write_byte (uart0_ptr, (BYTE) buffer);
		uart0_ptr		= (BYTEPTR) NULL_P;
		uart0_intr		= 1;
		raise_tvm_interrupt ();
	} else {
		/* No; buffer the character */
		uart0_buffer = buffer;
		uart0_pending++;
	}
}

void complete_uart0_interrupt (ECTX ectx)
{
	WORDPTR wptr;

	if (uart0_intr && ((wptr = uart0_channel) != (WORDPTR) NOT_PROCESS_P)) {
		uart0_channel		= (WORDPTR) NOT_PROCESS_P;
		uart0_intr		= 0;
		ectx->add_to_queue (ectx, wptr);
	}
}

int uart0_in (ECTX ectx, WORD count, BYTEPTR pointer)
{
	unsigned short imask;
	int reschedule;
	
	DISABLE_INTERRUPTS (imask);

	if (uart0_pending) {
		write_byte (pointer, (BYTE) uart0_buffer);
		uart0_pending	= 0;
		reschedule	= 0;
	} else {
		uart0_channel	= ectx->wptr;
		uart0_ptr	= pointer;
		reschedule	= 1;
	}

	ENABLE_INTERRUPTS (imask);

	if (reschedule) {
		/* Lower (set) CTS */
		*pPORTHIO = *pPORTHIO & (~0x0040);
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
	while (*pPORTHIO & 0x0001) {
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

