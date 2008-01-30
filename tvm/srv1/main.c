/*
 * main.c - SRV-1 Blackfin TVM Wrapper
 *
 * Copyright (C) 2007-2008 Jon Simpson, Matthew C. Jadud, Carl G. Ritson
 */

/* Generic TVM Includes */
#include <tvm.h>

void handle_int10 (void)
{
}

int srv_get_time (void)
{
	return readRTC();
}

int main() {

	init_io(); // Initialise LED, GPIO, serial flow and lasers.
	initRTC();
	init_uarts();
	initPWM();
	
	init_motors(); // Set up motor globals.

	//setPWM(lspeed, rspeed);

	/* Print version info on startup */
	serial_out_version();

	clear_sdram(); // Clears from 0x00000000 to 0x02000000
	camera_setup(); // Sets up the camera, initially 320x256.

	//get_time = srv_get_time;
	//special_ffi_table = get_special_hooks();
	
	uart0SendString((unsigned char *)"##TVM Initialiation Complete");
	uart0SendChar('\n');
	delayMS(1000);
	#if 0
	/* Go! */
	runresult = run();
	
	#endif
	uart0SendString((unsigned char *)"##Out of runloop.");
	uart0SendChar('\n');
	delayMS(1000);
}
