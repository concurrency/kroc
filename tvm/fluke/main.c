/**************************************
 * Transterpreter Fluke
 * 
 * A port of the Transterpreter virtual machine to the IPRE Fluke.
 *
 **************************************/
#include "setup.h"
#include "fluke.h"

#define BLINKS 2

void blinken(int max) {
	int i,j,c;
	j = 0;

	for(c = 0 ; c < max ; c++) 
	{
		led_on();
		for(i=0;i<j;i++) 
		{
			__asm__ __volatile__ ("nop");
		}
		
		led_off();
		for(i=0;i<j;i++)
		{
			__asm__ __volatile__ ("nop");
		}
		j = j + 10000;
	}
}
	
int main (void)
{
	setup_pins();
	setup_pll();
	setup_mam();

	/* Setup UART0 */
	uart0Init(B38400, UART_8N1, UART_FIFO_8);

	debug_print_str("In main.c\n");

	blinken(BLINKS);
	run_tvm();
	debug_print_str("Done with the TVM\n");

	for(;;) 
	{
		blinken(BLINKS);
	}
}
