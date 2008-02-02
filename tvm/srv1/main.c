/*
 * main.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2007-2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

/* Generic TVM Includes */
#include <tvm.h>
#include <cdefBF537.h>
#include "config.h"
#include <uart.h>

#define CSYNC __asm__ __volatile__ ("csync;" : : : "memory")
#define SSYNC __asm__ __volatile__ ("ssync;" : : : "memory")

static char version_string[] = "TVM SRV-1 Blackfin w/C interpreter - " __TIME__ " - " __DATE__;

/*{{{  Surveyor support code */
static void init_io (void)
{
	*pPORTGIO_DIR	= 0x0300;	// LEDs (PG8 and PG9)
	*pPORTGIO	= 0x0200;	// LED1 on
	*pPORTH_FER	= 0x0000;	// set for GPIO
	*pPORTHIO_DIR	|= 0x0040;	// set PORTH6 to output for serial flow control
	*pPORTHIO	= 0x0000;	// set output low 
	*pPORTHIO_INEN	= 0x0001;
	*pPORTHIO_DIR	|= 0x0380; 	// set up lasers
}

static void clear_sdram (void)
{
	const BYTE *end = (BYTE *) 0x02000000;
	BYTE *cp	= (BYTE *) 0;

	while (cp < end) {
		*(cp++) = 0;
	}
}

/*{{{  Timer code */
volatile unsigned long core_timer_wrap_count;

static void init_timers (void)
{
	core_timer_wrap_count = 0;
	*pTSCALE = ((CORE_CLOCK / 2000000) - 1);
	*pTPERIOD = 0xffffffff;
	*pTCOUNT = 0xffffffff;
	SSYNC;
	*pTCNTL = 0x7;
}

/* Read the time counter, returns number of microseconds since reset */
static WORD read_time (void)
{
	unsigned long pre_w, post_w;
	unsigned long tcount;

	do {
		pre_w 	= core_timer_wrap_count;
		tcount 	= *pTCOUNT;
		CSYNC;
		post_w	= core_timer_wrap_count;
	} while (pre_w != post_w);

	return (~tcount) >> 1 | (post_w << 31);
}

static void delay_ms (WORD delay)
{
	int timeout = read_time () + (delay * 1000);

	if ((delay < 0) || (delay > 100000))
		return;
	while (read_time () < timeout)
		continue;
}

static WORD srv_get_time (ECTX ectx)
{
	return read_time ();
}
/*}}}*/

static void serial_out_version (void)
{
	uart0SendString ((unsigned char *)"Version - ");
	uart0SendString ((unsigned char *)version_string);
	uart0SendChar ('\n');
}
/*}}}*/

/*{{{  TVM state */
static tvm_t 		tvm;
static tvm_ectx_t 	firmware_ctx, user_ctx;
/*}}}*/

/*{{{  UART functionality */
static volatile WORDPTR 	uart0_channel 	= (WORDPTR) NOT_PROCESS_P;
static volatile BYTEPTR		uart0_ptr	= (BYTEPTR) NULL_P;
static volatile unsigned char 	uart0_buffer	= '\0';
static volatile int		uart0_pending	= 0;

static void init_uart (void)
{
	unsigned char temp;

	/* Enable UART pins on port F */
	*pPORTF_FER |= 0x000f;

	/* Enable UART0 clocks */
	*pUART0_GCTL = UCEN;
	/* Switch on divisor programming */
	*pUART0_LCR = DLAB;
	/* Program divisor */
	*pUART0_DLL = UART0_DIVIDER;
	*pUART0_DLH = UART0_DIVIDER >> 8;
	/* Set operational mode (disables divisor programming) */
	*pUART0_LCR = WLS(8); /* 8 bit, no parity, one stop bit */

	/* Reads to clear possible pending errors / irqs */
	temp = *pUART0_RBR;
	temp = *pUART0_LSR;
	temp = *pUART0_IIR;
	SSYNC;

	/* Enable receive interrupts */
	*pSIC_IMASK |= IRQ_DMA8;
	*pUART0_IER |= ERBFI;
	SSYNC;

	/* The same for UART1 */
	#if 0
	*pUART1_GCTL = UCEN;
	*pUART1_LCR = DLAB;
	*pUART1_DLL = UART1_DIVIDER;
	*pUART1_DLH = UART1_DIVIDER >> 8;
	*pUART1_LCR = WLS(8); /* 8 bit, no parity, one stop bit */

	temp = *pUART1_RBR;
	temp = *pUART1_LSR;
	temp = *pUART1_IIR;
	SSYNC;
	#endif
}

void handle_int10 (void)
{
	unsigned char buffer;

	/* Raise (clear) CTS */
	*pPORTHIO	= *pPORTHIO | 0x0400;

	/* Read character (clears interrupt condition) */
	buffer		= *pUART0_RBR;

	/* Is anything waiting for the character? */
	if (uart0_ptr != (BYTEPTR) NULL_P) {
		/* Yes; give it the data and trigger requeue */
		write_byte (uart0_ptr, (BYTE) buffer);
		uart0_ptr		= (BYTEPTR) NULL_P;
		firmware_ctx.sflags	|= SFLAG_INTR;
		user_ctx.sflags		|= SFLAG_INTR;
	} else {
		/* No; buffer the character */
		uart0_buffer = buffer;
		uart0_pending++;
	}
}

static int uart0_in (ECTX ectx, WORD count, BYTEPTR pointer)
{
	unsigned short imask;
	int reschedule;
	
	/* Disable interrupts */
	__asm__ __volatile__ ("cli %0;" : "=r" (imask));

	if (uart0_pending) {
		write_byte (pointer, (BYTE) uart0_buffer);
		uart0_pending	= 0;
		reschedule	= 0;
	} else {
		uart0_channel	= ectx->wptr;
		uart0_ptr	= pointer;
		reschedule	= 1;
	}

	/* Enable (restore) interrupts */
	__asm__ __volatile__ ("sti %0;" : : "r" (imask));

	if (reschedule) {
		/* Lower (set) CTS */
		*pPORTHIO = *pPORTHIO & (~0x0400);
		/* Save instruction pointer */
		WORKSPACE_SET (ectx->wptr, WS_IPTR, (WORD) ectx->iptr);
		/* Reschedule */
		return ectx->run_next_on_queue (ectx);
	} else {
		return ECTX_CONTINUE;
	}
}

static int uart0_out (ECTX ectx, WORD count, BYTEPTR pointer)
{
	unsigned char data = (unsigned char) read_byte (pointer);

	/* Wait for RTS to go low (remote ready) */
	while (*pPORTHIO & 0x0001) {
		continue;
	}

	/* Wait for UART0 send buffer to be ready */
	while (!(*pUART0_LSR & THRE)) {
		continue;
	}
	
	/* Send data */
	*pUART0_THR = data;

	return ECTX_CONTINUE;
}
/*}}}*/

/*{{{  scheduling code */
static void srv_modify_sync_flags(ECTX ectx, WORD set, WORD clear)
{
	unsigned short imask;
	
	/* Disable interrupts */
	__asm__ __volatile__ ("cli %0;" : "=r" (imask));

	ectx->sflags = (ectx->sflags & (~clear)) | set;

	/* Enable (restore) interrupts */
	__asm__ __volatile__ ("sti %0;" : : "r" (imask));
}
/*}}}*/ 

/*{{{  External channel definitions */
static EXT_CHAN_ENTRY	ext_chans[] = {
	{ 
		.typehash 	= 0,
		.in 		= uart0_in, 
		.out 		= uart0_out
	}
};
static const int	ext_chans_length = sizeof(ext_chans) / sizeof(EXT_CHAN_ENTRY);
/*}}}*/

#include "firmware.h"

static WORD memory[256];

int main (void) {
	ECTX firmware 	= &firmware_ctx;
	ECTX user	= &user_ctx;
	WORDPTR	base	= (WORDPTR) memory;
	WORDPTR ws, vs, ms;
	WORD size;
	int ret;

	clear_sdram ();
	init_uart ();
	init_io ();
	init_timers ();

	serial_out_version ();
	
	/* Initialise interpreter */
	tvm_init (&tvm);
	
	/* Initialise firmware execution context */
	tvm_ectx_init (&tvm, firmware);
	firmware->get_time 		= srv_get_time;
	firmware->modify_sync_flags	= srv_modify_sync_flags;
	firmware->ext_chan_table	= ext_chans;
	firmware->ext_chan_table_length	= ext_chans_length;

	/* Initialise user execution context */
	tvm_ectx_init (&tvm, user);
	user->get_time 			= srv_get_time;
	user->modify_sync_flags		= srv_modify_sync_flags;

	/* Setup memory and initial workspace */
	tvm_ectx_layout (firmware, base, "", 0, ws_size, vs_size, ms_size, &size, &ws, &vs, &ms);
	tvm_ectx_install_tlp (firmware, (BYTEPTR) transputercode, ws, vs, ms, "", 0, NULL);

	uart0SendString ((unsigned char *) "##TVM Initialiation Complete");
	uart0SendChar ('\n');
	delay_ms (1000);
	
	for (;;) {
		ret = tvm_run (firmware);

		if (ret == ECTX_SLEEP) {
			continue;
		} else if (ret == ECTX_EMPTY && uart0_channel != (WORDPTR) NOT_PROCESS_P) {
			continue;
		} else if (ret == ECTX_INTERRUPT) {
			firmware->sflags	&= ~(SFLAG_INTR);
			user->sflags		&= ~(SFLAG_INTR);
			firmware->add_to_queue (firmware, uart0_channel);
			uart0_channel		= (WORDPTR) NOT_PROCESS_P;
		} else {
			break;
		}
	}
	
	uart0SendString ((unsigned char *) "##Out of runloop, state: ");
	uart0SendChar ((unsigned char) ret);
	uart0SendChar ('\n');
	delay_ms (1000);
}
