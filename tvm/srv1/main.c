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

static void init_rtc (void) {
	*pRTC_ICTL = 0;  // disable interrupts
	SSYNC;
	*pRTC_PREN = 0;  // disable prescaler - clock counts at 32768 Hz
	SSYNC;
	*pRTC_STAT = 0;  // clear counter
	SSYNC;
}

/* Read the RTC counter, returns number of milliseconds since reset */
static int read_rtc (void) {     
	int i1, i2;
	i1 = *pRTC_STAT;
	i2 = (i1 & 0x0000003F) + (((i1 >> 6) & 0x0000003F) * 60) +  
		(((i1 >> 12) & 0x0000001F) * 3600) + (((i1 >> 17) & 0x00007FFF) * 86400);
	return (i2 / 33);  // converts tick count to milliseconds
	//    32,768 / 32.77 = 1,000
}

static void delay_ms (int delay) {
	int timeout = read_rtc () + delay;

	if ((delay < 0) || (delay > 100000))
		return;
	while (read_rtc () < timeout)
		continue;
}

static WORD srv_get_time (ECTX ectx)
{
	return (WORD) read_rtc ();
}

static void serial_out_version (void) {
	uart0SendString ((unsigned char *)"Version - ");
	uart0SendString ((unsigned char *)version_string);
	uart0SendChar ('\n');
}

int main (void) {
	clear_sdram ();
	init_uarts ();
	init_io ();
	init_rtc ();

	serial_out_version ();

	//get_time = srv_get_time;
	//special_ffi_table = get_special_hooks();
	
	uart0SendString ((unsigned char *)"##TVM Initialiation Complete");
	uart0SendChar ('\n');
	delay_ms (1000);
	
	for (;;) {
		*pPORTGIO = *pPORTGIO ^ 0x0200;
		delay_ms (1000);
	}
	
	uart0SendString ((unsigned char *)"##Out of runloop.");
	uart0SendChar ('\n');
	delay_ms (1000);
}
