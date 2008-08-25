#include "fluke.h"
#include "sysTime.h"

ISR timerISR (void)
{

  // Enter and disable IRQs
   ISR_ENTRY();

  // Check that this is clock 0 
  // (match channel 0?)
#if 0
  if (T0IR & TIR_MR0I)
  {
    debug_print_str(".\n");

  } else {
    debug_print_str("_\n");
  }
#endif

  if (IOPIN & LED) {
    IOCLR = LED;
  } else {
    IOSET = LED;
  }
  
  // Reset the interrupt.
  T0IR  = 0xFF; // TIR_MR0I;
  T0MCR = TMCR_MR0_I;
  T0MR0 = T0MR0 + 500000;
  T0CCR = 0;
  T0EMR = 0;
  VICVectAddr = 0;

  ISR_EXIT();
}

void init_timerISR (void)
{
  VICIntSelect &= ~VIC_BIT(VIC_TIMER0);
  VICIntEnable =  VIC_BIT(VIC_TIMER0);
  VICVectCntl0 =  VIC_ENABLE | VIC_TIMER0;
  VICVectAddr0 =  (uint32_t)timerISR;

  T0TCR = TCR_RESET;
  T0PR  = T0_PCLK_DIV - 1;
  T0MCR = TMCR_MR0_I;
  T0MR0 = 500000;
  T0CCR = 0;
  T0EMR = 0;
  T0TCR = TCR_ENABLE;

}

WORD arm7tdmi_get_time(ECTX ectx)
{
  return getSysTICs();
}

void sleep_until(WORD timeout)
{
}

void sleep(void)
{
}
