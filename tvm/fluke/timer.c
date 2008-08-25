#include "fluke.h"
#include "sysTime.h"

/* occam-pi channel defined in C */
static WORDPTR                  tick_channel = (WORDPTR) NOT_PROCESS_P;
static volatile short           tick_pending = 0;
static volatile BYTEPTR         tick_ptr     = (BYTEPTR) NULL_P;

ISR timerISR (void)
{

  // Enter and disable IRQs
   ISR_ENTRY();

  // Reset the interrupt.
  T0IR  = 0xFF; // TIR_MR0I;
  T0MCR = TMCR_MR0_I;
  T0MR0 = T0MR0 + 500000;
  T0CCR = 0;
  T0EMR = 0;
  VICVectAddr = 0;

  // Magic occam-pi channel interaction
  tick_pending = 1;

  ISR_EXIT();
}

#define DISABLE_INTERRUPTS(mask) disableIRQ(mask);
#define ENABLE_INTERRUPTS(mask)  restoreIRQ(mask); enableIRQ();

int led_toggle_out (ECTX ectx, WORD count, BYTEPTR pointer)
{

  if (IOPIN & LED) {
    IOCLR = LED;
  } else {
    IOSET = LED;
  }

  return ECTX_CONTINUE;
 
}

int timer_in (ECTX ectx, WORD count, BYTEPTR pointer)
{
  unsigned mask;
  int reschedule;
  
  write_byte(pointer, (BYTE) '*');
  return ECTX_CONTINUE;

#if 0
  // DISABLE_INTERRUPTS(mask);
  if (tick_pending)
  {
    write_byte (pointer, (BYTE) '*');
    tick_pending = 0;
    // BARRIER
    reschedule = 0;
  } else {
    tick_channel = ectx->wptr;
    tick_ptr = pointer;
    // BARRIER
    reschedule = 1;
  }

  // ENABLE_INTERRUPTS(mask);

  if (reschedule) 
  {
    WORKSPACE_SET (ectx->wptr, WS_IPTR, (WORD) ectx->iptr);
    return ectx->run_next_on_queue (ectx);
  } else {
    return ECTX_CONTINUE;
  }
#endif 

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
