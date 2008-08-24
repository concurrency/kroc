#include "lpc210x.h"
#include "setup.h"

void led_on()
{
  IOSET = LED;
}

void led_off()
{
  IOCLR = LED;
}

// VIC Channel Assignments for UART0/1
#define VIC_UART0       6
#define VIC_UART1       7

// Vector Control Register bit definitions
#define VIC_ENABLE      (1 << 5)

// Convert Channel Number to Bit Value
#define VIC_BIT(chan)   (1 << (chan))

void setup_interrupts() 
{
	VICIntEnClr = 0xFFFFFFFF;                     // clear all interrupts
	VICIntSelect = 0x00000000;                    // clear all FIQ selections
	VICDefVectAddr = (uint32_t)Reset_Handler;     // point unvectored IRQs to reset()
}

#define UART0_RX_BUFFER_SIZE  32
#define UART0_TX_BUFFER_SIZE  32

uint8_t  uart0_rx_buffer[UART0_RX_BUFFER_SIZE];
uint16_t uart0_rx_insert_idx, uart0_rx_extract_idx;

uint8_t  uart0_tx_buffer[UART0_TX_BUFFER_SIZE];
uint16_t uart0_tx_insert_idx, uart0_tx_extract_idx;
int      uart0_tx_running;

void setup_uarts() 
{
	/* Setup UART0 */
	uart0Init(B38400, UART_8N1, UART_FIFO_8);
  /* Setup UART0 Interrupts */
	VICIntSelect &= ~VIC_BIT(VIC_UART0);  // UART0 selected as IRQ
  VICIntEnable = VIC_BIT(VIC_UART0);    // UART0 interrupt enabled
  VICVectCntl0 = VIC_ENABLE | VIC_UART0;
  VICVectAddr0 = (uint32_t)uart0ISR;    // address of the ISR

  /* Transmit and receive queues for UART0 */
	
	// initialize the transmit data queue
  uart0_tx_extract_idx = uart0_tx_insert_idx = 0;
  uart0_tx_running = 0;

	// initialize the receive data queue
  uart0_rx_extract_idx = uart0_rx_insert_idx = 0;
  // enable receiver interrupts
  UART0_IER = UIER_ERBFI;

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
  // a. start change by turning off the MAM (redundant)
  MAMCR = 0;	
  // b. set MAM-Fetch 
  MAMTIM = 2;
  // c. enable MAM 
  MAMCR = 2;
}
