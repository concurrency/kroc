/*
 * main.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2007-2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

/* Generic TVM Includes */
#include <tvm.h>
#include <cdefBF537.h>
#include "config.h"
#include "memory_map.h"
#include <uart.h>

#define CSYNC	__asm__ __volatile__ ("csync;" : : : "memory")
#define SSYNC	__asm__ __volatile__ ("ssync;" : : : "memory")
#define IDLE	__asm__ __volatile__ ("idle;")

static char version_string[] = "TVM SRV-1 Blackfin w/C interpreter - " __TIME__ " - " __DATE__;

/*{{{  Surveyor support code */
static void init_io (void)
{
	*pPORTGIO_DIR	= 0x0300;	// LEDs (PG8 and PG9)
	*pPORTGIO	= 0x0200;	// LED1 on
	*pPORTH_FER	= 0x0000;	// set for GPIO
	*pPORTHIO_DIR	|= 0x0040;	// set PORTH6 to output for serial flow control
	*pPORTHIO	= 0x0040;	// set PORTH6 output high
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

static void serial_out_version (void)
{
	uart0SendString ((unsigned char *)"Version - ");
	uart0SendString ((unsigned char *)version_string);
	uart0SendChar ('\n');
}
/*}}}*/

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
		*pPORTHIO = *pPORTHIO & (~0x0040);
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

/*{{{  Scheduling support */
static void srv_modify_sync_flags (ECTX ectx, WORD set, WORD clear)
{
	unsigned short imask;
	
	/* Disable interrupts */
	__asm__ __volatile__ ("cli %0;" : "=r" (imask));

	ectx->sflags = (ectx->sflags & (~clear)) | set;

	/* Enable (restore) interrupts */
	__asm__ __volatile__ ("sti %0;" : : "r" (imask));
}

/* Look for dependency conditions in top-level channels. */
static int waiting_on (ECTX ectx, WORDPTR ws_base, WORD ws_len)
{
	WORDPTR ws_end = wordptr_plus (ws_base, ws_len);
	WORDPTR ptr;
	int i;

	if (ws_base > ws_end) {
		WORDPTR tmp	= ws_base;
		ws_base		= ws_end;
		ws_end		= tmp;
	}

	for (i = 0; i < ectx->tlp_argc; ++i) {
		switch (ectx->tlp_fmt[i]) {
			case '?': 
			case '!':
				ptr = (WORDPTR) ectx->tlp_argv[i];
				ptr = (WORDPTR) read_word (ptr);
				ptr = (WORDPTR) read_word (ptr);
				if (ptr >= ws_base && ptr <= ws_end)
					return 1; /* dependency */
				break;
			default:
				break;
		}
	}

	return 0; /* no dependencies; deadlock? */
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
static const int	ext_chans_length =
				sizeof(ext_chans) / sizeof(EXT_CHAN_ENTRY);
/*}}}*/

/*{{{  User context */
static BYTEPTR		user_bytecode;
static WORD		user_bytecode_len;
static const WORDPTR 	user_memory	= (WORDPTR) USER_MEMORY;
static WORD		user_memory_len	= 0;
static WORDPTR 		user_parent	= (WORDPTR) NOT_PROCESS_P;

static void install_user_ctx (void)
{
	ECTX user = &user_ctx;

	tvm_ectx_init (&tvm, user);
	user->get_time 			= srv_get_time;
	user->modify_sync_flags		= srv_modify_sync_flags;
}

static int run_user (void)
{
	int ret = tvm_run_count (&user_ctx, 1000);

	switch (ret) {
		case ECTX_INTERRUPT:
		case ECTX_PREEMPT:
		case ECTX_SLEEP:
		case ECTX_TIME_SLICE:
			return ret; /* OK */
		case ECTX_EMPTY:
			if (waiting_on (&user_ctx, user_memory, user_memory_len)) {
				return ret; /* OK - waiting for firmware */
			}
			break;
		default:
			break;
	}

	/* User context broke down for some reason. */
	/* Restore parent in firmware */
	firmware_ctx.add_to_queue (&firmware_ctx, user_parent);
	user_parent = (WORDPTR) NOT_PROCESS_P;

	return ECTX_ERROR;
}
/*}}}*/

/*{{{  Firmware SFFI */
static int firmware_run_user (ECTX ectx, WORD args[])
{
	BYTEPTR bytecode	= (BYTEPTR) args[0];
	WORD	bytecode_len	= args[1];
	WORD	ws_size		= args[2];
	WORD	vs_size		= args[3];
	WORD	ms_size		= args[4];
	BYTEPTR	tlp_fmt		= (BYTEPTR) args[5];
	WORD	tlp_fmt_len	= args[6];
	WORD	argc		= args[7];
	WORDPTR	argv		= (WORD *) &(args[8]);
	WORDPTR	ws, vs, ms;
	int ret;

	if (user_parent != (WORDPTR) NOT_PROCESS_P)
	{
		/* User context is already running */
		return ectx->set_error_flag (ectx, EFLAG_FFI);
	}

	tvm_ectx_reset (&user_ctx);
	tvm_ectx_layout (
		&user_ctx, user_memory, 
		tlp_fmt, argc, ws_size, vs_size, ms_size,
		&user_memory_len, &ws, &vs, &ms
	);
	ret = tvm_ectx_install_tlp (
		&user_ctx, bytecode, ws, vs, ms,
		tlp_fmt, argc, argv
	);
	if (ret) {
		/* Install TLP failed */
		return ectx->set_error_flag (ectx, EFLAG_FFI);
	}

	/* Save bytecode addresses */
	user_bytecode		= bytecode;
	user_bytecode_len	= bytecode_len;

	/* Simulate return, and deschedule */
	/* Push WPTR up 4 words */
	user_parent = wordptr_plus (ectx->wptr, 4);
	/* Store return address as descheduled IPTR */
	WORKSPACE_SET (user_parent, WS_IPTR, read_word (ectx->wptr));
	/* Save execution context for good measure */
	WORKSPACE_SET (user_parent, WS_ECTX, (WORD) ectx);

	return SFFI_RESCHEDULE;
}

static SFFI_FUNCTION	firmware_sffi_table[] = {
	firmware_run_user
};
static const int	firmware_sffi_table_length =
				sizeof(firmware_sffi_table) / sizeof(SFFI_FUNCTION);
/*}}}*/

/*{{{  Firmware context */
#include "firmware.h"

static WORD firmware_memory[128];
static WORD firmware_memory_len;

static void init_firmware_memory (void)
{
	WORD *ptr = firmware_memory;
	int words = (sizeof(firmware_memory) / sizeof(WORD));
	
	while ((words--) > 0) {
		*(ptr++) = MIN_INT;
	}
}

static void install_firmware_ctx (void)
{
	WORDPTR ws, vs, ms;
	ECTX firmware = &firmware_ctx;

	/* Initialise firmware execution context */
	tvm_ectx_init (&tvm, firmware);
	firmware->get_time 		= srv_get_time;
	firmware->modify_sync_flags	= srv_modify_sync_flags;
	firmware->ext_chan_table	= ext_chans;
	firmware->ext_chan_table_length	= ext_chans_length;
	firmware->sffi_table		= firmware_sffi_table;
	firmware->sffi_table_length	= firmware_sffi_table_length;
	
	/* Setup memory and initial workspace */
	init_firmware_memory ();
	tvm_ectx_layout (
		firmware, firmware_memory,
		"", 0, ws_size, vs_size, ms_size, 
		&firmware_memory_len, &ws, &vs, &ms
	);
	tvm_ectx_install_tlp (
		firmware, (BYTEPTR) transputercode, ws, vs, ms, 
		"", 0, NULL
	);
}

static int run_firmware (void)
{
	int ret = tvm_run (&firmware_ctx);

	if (ret == ECTX_SLEEP) {
		return ret; /* OK - timer sleep */
	} else if (ret == ECTX_EMPTY) {
		if (uart0_channel != (WORDPTR) NOT_PROCESS_P) {
			return ret; /* OK - waiting for input */
		} else if (user_parent != (WORDPTR) NOT_PROCESS_P) {
			if (waiting_on (&user_ctx, firmware_memory, firmware_memory_len)) {
				if (user_ctx.state != ECTX_EMPTY) {
					return ret; /* OK - waiting on user process */
				} else if (waiting_on (&user_ctx, user_memory, user_memory_len)) {
					/* Circular dependency - probably bad firmware */
				} else {
					return ret;
				}
			}
		}
	} else if (ret == ECTX_INTERRUPT) {
		firmware_ctx.sflags	&= ~(SFLAG_INTR);
		user_ctx.sflags		&= ~(SFLAG_INTR);
		firmware_ctx.add_to_queue (&firmware_ctx, uart0_channel);
		uart0_channel		= (WORDPTR) NOT_PROCESS_P;
		return ret; /* OK - interrupt */
	}

	/* Being here means some unexpected happened... */
	
	uart0SendString ((unsigned char *) "## Firmware failed; state = ");
	uart0SendChar ((unsigned char) ret);
	uart0SendChar ('\n');

	if (user_parent != (WORDPTR) NOT_PROCESS_P) {
		uart0SendString ((unsigned char *) "## User state = ");
		uart0SendChar ((unsigned char) user_ctx.state);
		uart0SendChar ('\n');
	}

	/* Go into an idle loop */
	for (;;) {
		IDLE;
		SSYNC;
	}

	return ret;
}
/*}}}*/

int main (void) {
	clear_sdram ();
	init_uart ();
	init_io ();
	init_timers ();

	serial_out_version ();
	
	/* Initialise interpreter */
	tvm_init (&tvm);
	install_firmware_ctx ();
	install_user_ctx ();

	uart0SendString ((unsigned char *) "## TVM initialisation complete...\n");
	
	for (;;) {
		int f_ret = run_firmware ();
		int u_ret = ECTX_EMPTY;

		if (user_parent != (WORDPTR) NOT_PROCESS_P) {
			u_ret = run_user ();
		}

		if ((f_ret == ECTX_EMPTY || f_ret == ECTX_SLEEP) && 
			(u_ret == ECTX_EMPTY || u_ret == ECTX_SLEEP)) {
			/* FIXME: power management goes here */
		}
	}
}
