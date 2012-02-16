#include "fluke.h"
#include "sysTime.h"

/* occam-pi channel defined in C */
static WORDPTR                  tick_channel = (WORDPTR) NOT_PROCESS_P;
static volatile short           tick_pending = 0;
static volatile BYTEPTR         tick_ptr     = (BYTEPTR) NULL_P;
static int debug = 1;

/* Timer interrupt frequency. */
#define INTERRUPT_DELAY 10000000

#if 0
/*{{{ timerISR
 * The timer interrupt service routine.
 */
ISR timerISR (void)
{

  // Enter and disable IRQs
  ISR_ENTRY();
    
  // Raise an interrupt in the VM; otherwise, we won't 
  // be able to complete the channel rendevous later.
  if (tick_ptr != (BYTEPTR) NULL_P) 
  {
    raise_tvm_interrupt(TVM_INTR_MAGIC_TIMER);
    tick_ptr = (BYTEPTR) NULL_P;
  } else {
    tick_pending = 1;
  }
  
  // Reset the interrupt.
  T0IR  = TIR_MR0I;
 
  if (IOPIN & LED) {
    debug_print_str("T0MR0: ");
    debug_print_hex(T0MR0);
    debug_print_str("\n");
    debug = 0;
  }

  T0MR0 = T0MR0 + INTERRUPT_DELAY;
  VICVectAddr = 0;

  ISR_EXIT();
}
/*}}}*/

/*{{{ led_toggle_out
 * This is a magic TVM channel. When written to 
 * (or read from) it toggles the LED.
 */
int led_toggle_out (ECTX ectx, WORD count, BYTEPTR pointer)
{

  if (IOPIN & LED) {
    IOCLR = LED;
    debug = 1;
  } else {
    IOSET = LED;
  }

  return ECTX_CONTINUE;
}
/*}}}*/

/*{{{ timer_in
 * A magic TVM channel. When read from, it produces a '*'.
 * This is, effectively, a "signal" channel from the timer 
 * interrupt. The contents of the channel are less interesting
 * than the scheduler interractions.
 */
int timer_in (ECTX ectx, WORD count, BYTEPTR pointer)
{
  unsigned mask;
  int reschedule;

  DISABLE_INTERRUPTS(mask);

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

  ENABLE_INTERRUPTS(mask);

  if (reschedule) 
  {
    WORKSPACE_SET (ectx->wptr, WS_IPTR, (WORD) ectx->iptr);
    return ectx->run_next_on_queue (ectx);
  } else {
    return ECTX_CONTINUE;
  }

}
/*}}}*/

/*{{{ complete_magic_timer_interrupt
 * This completes the channel rendevous on the magic
 * timer channel. It sticks us back on the queue to be
 * read from again later. Because we are magic, we are always
 * ready, so it is only the logic coded into the function
 * "timer_in" (in this case) that handles whether a value
 * is produced when read from (or not).
 */
void complete_magic_timer_interrupt (ECTX ectx) 
{
  ectx->add_to_queue(ectx, tick_channel);
  tick_channel = NOT_PROCESS_P;
}
/*}}}*/

/*{{{ init_timerISR
 * Everything needed to initialize an IRQ on the LPC2106.
 * This particular ISR is to handle a timer alarm.
 */
void init_timerISR (void)
{
  VICIntSelect &= ~VIC_BIT(VIC_TIMER0);
  VICIntEnable =  VIC_BIT(VIC_TIMER0);
  VICVectCntl0 =  VIC_ENABLE | VIC_TIMER0;
  VICVectAddr0 =  (uint32_t)timerISR;

  T0TCR = TCR_RESET;
  T0PR  = T0_PCLK_DIV - 1;
  T0MCR = TMCR_MR0_I;
  T0MR0 = INTERRUPT_DELAY;
  T0CCR = 0;
  T0EMR = 0;
  T0TCR = TCR_ENABLE;
}
/*}}}*/
#endif

/*{{{ timerISR
 * The timer interrupt service routine.
 */
ISR timerISR (void)
{
  // Enter and disable IRQs
  ISR_ENTRY();
    
  /* Ack the interrupt. */
  T0IR  = TIR_MR0I;
  VICVectAddr = 0;

  ISR_EXIT();
}
/*}}}*/
/*{{{ init_timer
 * Everything needed to initialize an IRQ on the LPC2106.
 * This particular ISR is to handle a timer alarm.
 */
void init_timer (void)
{
  VICIntSelect &= ~VIC_BIT(VIC_TIMER0);
  VICIntEnable =  VIC_BIT(VIC_TIMER0);
  VICVectCntl0 =  VIC_ENABLE | VIC_TIMER0;
  VICVectAddr0 =  (uint32_t)timerISR;

  T0TCR = TCR_RESET;
  T0PR  = T0_PCLK_DIV - 1;
  T0MR0 = 0;
  T0MR1 = 0;
  T0MR2 = 0;
  T0CCR = 0;
  T0EMR = 0;
  T0TCR = TCR_ENABLE;
}
/*}}}*/

WORD arm7tdmi_get_time(ECTX ectx)
{
  return getSysTICs();
}

static void start_timer(unsigned int timeout)
{
  T0MCR = TMCR_MR0_I;
  T0MR0 = timeout;
}

static void stop_timer(void)
{
  T0MCR = 0; /* Clear the match control register */
}

static void go_to_sleep(unsigned int timeout)
{
  unsigned short imask;

  DISABLE_INTERRUPTS (imask);
  
  /* Only sleep if there are no pending interrupts */
  if (!tvm_interrupt_pending ()) {
    // IOSET = LED;
    start_timer(timeout);

    /* Enter idle mode */
    WB_CACHE_FLUSH;
    PCON = PCON_IDL;
    BARRIER;
    /* Got woken up */

    stop_timer();
    // IOCLR = LED;
  }

  ENABLE_INTERRUPTS (imask);
}

void sleep_for(WORD duration)
{
  unsigned int timeout = (unsigned int) getSysTICs() + duration;
  go_to_sleep(timeout);
}

void sleep_until(WORD timeout)
{
  go_to_sleep(timeout);
}

void sleep(void)
{
  sleep_for(0xFFFFFFFF);
}
