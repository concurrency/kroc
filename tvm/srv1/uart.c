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

static volatile unsigned char 	rx_buffer	= '\0';
static WORDPTR			rx_channel	= (WORDPTR) NOT_PROCESS_P;
static volatile short		rx_pending	= 0;
static volatile BYTEPTR		rx_ptr		= (BYTEPTR) NULL_P;

static WORDPTR			tx_channel	= (WORDPTR) NOT_PROCESS_P;
static volatile WORD		tx_pending	= 0;
static BYTEPTR			tx_ptr		= (BYTEPTR) NULL_P;

void init_uart (void)
{
	unsigned char temp;

	/* Configure port H for flow control CTS */
	*pPORTHIO_DIR	|= CTS_MASK;
	/* Configure port H for flow control RTS */
	*pPORTHIO_INEN	|= RTS_MASK;
	*pPORTHIO_POLAR	|= RTS_MASK;
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
	*pSIC_IAR1	= (*pSIC_IAR1 & ~(0xf << 0xc)) | P11_IVG(10);
	*pSIC_IMASK	|= IRQ_DMA8;
	*pUART0_IER	|= ERBFI;
	SSYNC;

	/* Setup RTS interrupts on IVG 14, but don't enable */
	*pSIC_IAR2	= (*pSIC_IAR2 & ~(0xf << 0x4)) | P17_IVG(14);
	*pSIC_IMASK	|= IRQ_PFA_PORTH;
	SSYNC;
}

static WORD run_output (WORD count, BYTEPTR *pptr)
{
	BYTEPTR ptr = *pptr;

	while (count && (*pPORTHIO & RTS_MASK)) {
		unsigned char c = read_byte (ptr);

		ptr = byteptr_plus (ptr, 1);

		while (!(*pUART0_LSR & THRE)) {
			continue;
		}

		/* Send data */
		*pUART0_THR = c;

		count--;
	}

	*pptr = ptr;

	return count;
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
		raise_tvm_interrupt (TVM_INTR_UART0_RX);
	} else {
		/* No; buffer the character */
		rx_buffer	= buffer;
		rx_pending++;
	}
}

void handle_int14 (void)
{
	if (!(tx_pending = run_output (tx_pending, &tx_ptr))) {
		/* Complete; notify TVM and disable interrupt */
		raise_tvm_interrupt (TVM_INTR_UART0_TX);
		*pPORTHIO_MASKA_CLEAR = RTS_MASK;
		SSYNC;
	}
}

void complete_uart0_rx_interrupt (ECTX ectx)
{
	ectx->add_to_queue (ectx, rx_channel);
	rx_channel = NOT_PROCESS_P;
}

void complete_uart0_tx_interrupt (ECTX ectx)
{
	ectx->add_to_queue (ectx, tx_channel);
	tx_channel = NOT_PROCESS_P;
}

int uart0_is_blocking (void)
{
	/* Return non-zero if rx_channel or tx_channel is not NOT_PROCESS_P. */
	return ((int) rx_channel | (int) tx_channel) ^ NOT_PROCESS_P;
}

int uart0_in (ECTX ectx, WORD count, BYTEPTR pointer)
{
	unsigned short imask;
	int reschedule;
	
	DISABLE_INTERRUPTS (imask);

	if (rx_pending) {
		write_byte (pointer, (BYTE) rx_buffer);
		rx_pending	= 0;
		BARRIER;
		reschedule	= 0;
	} else {
		rx_channel	= ectx->wptr;
		rx_ptr		= pointer;
		BARRIER;
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
	while (!(*pPORTHIO & RTS_MASK)) {
		continue;
	}

	/* Wait for UART0 send buffer to be ready */
	while (!(*pUART0_LSR & THRE)) {
		continue;
	}

	/* Send data */
	*pUART0_THR = c;
}

int uart0_out (ECTX ectx, WORD count, BYTEPTR pointer)
{
	if (count == sizeof(WORD)) {
		/* If count is the size of a word then 
		 * throw away data as it will be the count 
		 * from a counted array output.
		 */
	} else if ((count = run_output (count, &pointer))) {
		/* Couldn't ship all the output, hand the
		 * rest to the interrupt handler.
		 */
		tx_channel	= ectx->wptr;
		tx_ptr		= pointer;
		tx_pending	= count;
		
		BARRIER;

		/* Enable interrupt */
		*pPORTHIO_MASKA_SET = RTS_MASK;

		/* Reschedule */
		WORKSPACE_SET (ectx->wptr, WS_IPTR, (WORD) ectx->iptr);
		return ectx->run_next_on_queue (ectx);
	}
	
	return ECTX_CONTINUE;
}

