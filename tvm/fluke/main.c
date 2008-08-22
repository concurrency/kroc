/**************************************
 * Transterpreter Fluke
 * 
 * A port of the Transterpreter virtual machine to the IPRE Fluke.
 *
 **************************************/
#include "setup.h"

int main (void)
{
	int i,j;

	setup_pins();
	setup_pll();
	setup_mam();
	
	led_on();
	// `run_tvm();

	j = 0;	
	for(;;) 
	{
		led_off();
		for(i=0;i<j;i++) 
		{
			__asm__ __volatile__ ("nop");
		}
		
		led_on();
		for(i=0;i<j;i++)
		{
			__asm__ __volatile__ ("nop");
		}
		j = j + 10000;
		

	}
}
