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

#define SSYNC asm("ssync;")

static char version_string[] = "TVM SRV-1 Blackfin w/C interpreter - " __TIME__ " - " __DATE__;

void handle_int10 (void)
{
}

/*{{{  Surveyor support code */
static void init_io (void) {
	*pPORTGIO_DIR	= 0x0300;	// LEDs (PG8 and PG9)
	*pPORTGIO	= 0x0200;	// LED1 on
	*pPORTH_FER	= 0x0000;	// set for GPIO
	*pPORTHIO_DIR	|= 0x0040;	// set PORTH6 to output for serial flow control
	*pPORTHIO	= 0x0000;	// set output low 
	*pPORTHIO_INEN	= 0x0001;
	*pPORTHIO_DIR	|= 0x0380; 	// set up lasers
}

static void clear_sdram (void) {
	const BYTE *end = (BYTE *) 0x02000000;
	BYTE *cp	= (BYTE *) 0;

	while (cp < end) {
		*(cp++) = 0;
	}
}

static void init_time (void) {
	*pTSCALE = ((CORE_CLOCK / 2000000) - 1);
	*pTPERIOD = 0xffffffff;
	*pTCOUNT = 0xffffffff;
	SSYNC;
	*pTCNTL = 0x7;
}

/* Read the time counter, returns number of microseconds since reset */
static int read_time (void) { 
	return (((unsigned int) 0xffffffff) - ((unsigned int) *pTCOUNT)) >> 1;
}

static void delay_ms (int delay) {
	int timeout = read_time () + (delay * 1000);

	if ((delay < 0) || (delay > 100000))
		return;
	while (read_time () < timeout)
		continue;
}

static WORD srv_get_time (ECTX ectx)
{
	return (WORD) read_time ();
}

static void serial_out_version (void) {
	uart0SendString ((unsigned char *)"Version - ");
	uart0SendString ((unsigned char *)version_string);
	uart0SendChar ('\n');
}
/*}}}*/

/*{{{  TVM state */
static tvm_t 		tvm;
static tvm_ectx_t 	firmware_ctx, user_ctx;
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
	init_uarts ();
	init_io ();
	init_time ();

	serial_out_version ();
	
	/* Initialise interpreter */
	tvm_init (&tvm);
	
	/* Initialise firmware execution context */
	tvm_ectx_init (&tvm, firmware);
	firmware->get_time 	= srv_get_time;

	/* Initialise user execution context */
	tvm_ectx_init (&tvm, user);
	user->get_time 		= srv_get_time;

	/* Setup memory and initial workspace */
	tvm_ectx_layout (firmware, base, "", 0, ws_size, vs_size, ms_size, &size, &ws, &vs, &ms);
	tvm_ectx_install_tlp (firmware, (BYTEPTR) transputercode, ws, vs, ms, "", 0, NULL);

	uart0SendString ((unsigned char *) "##TVM Initialiation Complete");
	uart0SendChar ('\n');
	delay_ms (1000);
	
	do {
		ret = tvm_run (firmware);
	} while (ret == ECTX_SLEEP);
	
	uart0SendString ((unsigned char *) "##Out of runloop, state: ");
	uart0SendChar ((unsigned char) ret);
	uart0SendChar ('\n');
	delay_ms (1000);
}
