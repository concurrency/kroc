/*
 * main.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2007-2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

#include "srv.h"

int main (void) {
	/* Initialise Hardware */
	init_gpio ();
	init_timers ();
	init_uart ();
	init_ppi_dma ();
	init_twi ();

	/* Wait for 7 seconds for MatchPort to boot */
	delay_us (7000000);

	/* Output boot message */
	uart0_send_string (version_string);
	uart0_send_char ('\n');

	/* Run the TVM */
	return run_tvm ();;
}

