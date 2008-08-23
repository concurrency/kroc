#include "lpc/LPC210x.h"
#include "setup.h"

void led_on()
{
  IOSET = LED;
}

void led_off()
{
  IOCLR = LED;
}


void setup_pins()
{
  IODIR = (S_RST  | MCLK   | MDIN   | LED   | D2A_CS | A2D_CS | MCS | 
	   IROUT1 | IROUT2 | IROUT3 | B_TXD | B_RTS);
  IOSET = (D2A_CS | A2D_CS | MCS | B_TXD);

  PINSEL0 |= (1 << 27);  // Match 1.1 (pin 13/IROUT1)  PROG   (R)
  PINSEL1 |= (1 << 6);   // Match 1.3 (pin 19/IROUT2)  IPRE   (L)
  PINSEL1 |= (1 << 8);   // Match 1.3 (pin 20/IROUT3)  MIDDLE (M)
}

void setup_pll()
{
  // --- enable and connect the PLL (Phase Locked Loop) ---
  // a. set multiplier and divider
  //     = MSEL | (1 << 6) | (0 << 5)
  //PLLCFG = MSEL | (1<<PSEL1) | (0<<PSEL0);
  PLLCFG = MSEL | (PSEL << 5);
  // b. enable PLL
  PLLCON = (1<<PLLE);
  // c. feed sequence
  PLLFEED = PLL_FEED1;
  PLLFEED = PLL_FEED2;
  // d. wait for PLL lock (PLOCK bit is set if locked)
  while (!(PLLSTAT & (1<<PLOCK)));
  // e. connect (and enable) PLL
  PLLCON = (1<<PLLE) | (1<<PLLC);
  // f. feed sequence
  PLLFEED = PLL_FEED1;
  PLLFEED = PLL_FEED2;

  VPBDIV = 1;
}

void setup_mam()
{  
  // --- setup and enable the MAM (Memory Accelerator Module) ---
  // a. start change by turning of the MAM (redundant)
  MAMCR = 0;	
  // b. set MAM-Fetch 
  MAMTIM = 2;
  // c. enable MAM 
  MAMCR = 2;
}
