#include "tvm-arduino.h"

static int num_waiting = 0;

/*{{{ ATmega328 enumeration*/
#if defined(atmega328p)
enum {
	vintr_INT0 = 0,
	vintr_INT1,
	vintr_PCINT0,
	vintr_PCINT1,
	vintr_PCINT2,
	vintr_TIMER1,
	vintr_TIMER2,
	vintr_ADC,
	vintr_USART_RX,
	vintr_USART_UDRE,
	vintr_USART_TX,
	NUM_INTERRUPTS
};
/*}}}*/
/*{{{ ATmega1280 enumeration */
#elif defined(atmega1280)
enum {
	vintr_INT0 = 0,
	vintr_INT1,
	vintr_INT2,
	vintr_INT3,
	vintr_INT4,
	vintr_INT5,
	vintr_INT6,
	vintr_INT7,
	vintr_PCINT0,
	vintr_PCINT1,
	vintr_PCINT2,
	vintr_TIMER0,
	vintr_TIMER1,
	vintr_TIMER2,
	vintr_TIMER3,
	vintr_TIMER4,
	vintr_TIMER5,	
	vintr_ADC,
	vintr_USART_RX0,
	vintr_USART_RX1,
	vintr_USART_RX2,
	vintr_USART_RX3,
	vintr_USART_UDRE0,
	vintr_USART_UDRE1,
	vintr_USART_UDRE2,
	vintr_USART_UDRE3,
	vintr_USART_TX0,
	vintr_USART_TX1,
	vintr_USART_TX2,
	vintr_USART_TX3,
	NUM_INTERRUPTS
};
#endif
/*}}}*/

typedef struct _vinterrupt {
	/* Workspaces are always word-aligned, so we can use the least
	   significant bit of wptr to indicate whether the interrupt has been
	   triggered. */
	WORDPTR wptr;
	WORD pending;
} vinterrupt;

static vinterrupt interrupts[NUM_INTERRUPTS];

void init_interrupts () {
	int i;

	for (i = 0; i < NUM_INTERRUPTS; i++) {
		interrupts[i].wptr = (WORDPTR) NOT_PROCESS_P;
		interrupts[i].pending = MIN_INT;
	}
}

/* This runs in interrupt context, so no need to cli()/sei(). */
static void raise_tvm_interrupt (WORD flag) {
	context.sflags |= SFLAG_INTR | flag;
}

static void handle_interrupt (vinterrupt *intr) {
	WORD data = time_millis ();
	if (intr->wptr == (WORDPTR) NOT_PROCESS_P) {
		/* Nothing waiting; just record that the interrupt fired. */
		if (data == (WORD) MIN_INT) {
			++data;
		}
		intr->pending = data;
	} else if ((((WORD) intr->wptr) & 1) != 0) {
		/* The interrupt has already been raised; nothing to do. */
	} else {
		/* Process waiting: raise the interrupt. */
		WORDPTR ptr = (WORDPTR) WORKSPACE_GET (intr->wptr, WS_POINTER);
		if (ptr != NULL_P) {
			write_word (ptr, data);
			WORKSPACE_SET (intr->wptr, WS_POINTER, NULL_P);
		}
		intr->wptr = (WORDPTR) (((WORD) intr->wptr) | 1);
		raise_tvm_interrupt (TVM_INTR_VIRTUAL);
	}
}

static int wait_interrupt (vinterrupt *intr, ECTX ectx, WORDPTR time_ptr) {
	int ret;

	cli ();

	if (intr->pending != (WORD) MIN_INT) {
		/* The interrupt has already fired; just return. */
		write_word (time_ptr, intr->pending);
		intr->pending = MIN_INT;

		ret = SFFI_OK;
	} else {
		/* The interrupt hasn't fired, so park this process on it and
		   schedule another one. */

		/* Simulate a return -- since we want to be rescheduled
		   *following* the FFI call. */
		/* FIXME This should be a macro (it's also used in srv1). */
		WORD ret_addr = read_word (ectx->wptr);
		ectx->wptr = wordptr_plus (ectx->wptr, 4);
		WORKSPACE_SET (ectx->wptr, WS_IPTR, ret_addr);
		WORKSPACE_SET (ectx->wptr, WS_ECTX, (WORD) ectx);

		WORKSPACE_SET (ectx->wptr, WS_POINTER, (WORD) time_ptr);
		intr->wptr = ectx->wptr;

		++num_waiting;

		ret = SFFI_RESCHEDULE;
	}

	sei ();

	return ret;
}

#define MAP_SIMPLE_INTERRUPT(vector, interrupt) \
	ISR(vector) { \
		handle_interrupt (&interrupts[interrupt]); \
	}
MAP_SIMPLE_INTERRUPT(INT0_vect, vintr_INT0)
MAP_SIMPLE_INTERRUPT(INT1_vect, vintr_INT1)
MAP_SIMPLE_INTERRUPT(PCINT0_vect, vintr_PCINT0)
MAP_SIMPLE_INTERRUPT(PCINT1_vect, vintr_PCINT1)
MAP_SIMPLE_INTERRUPT(PCINT2_vect, vintr_PCINT2)
MAP_SIMPLE_INTERRUPT(TIMER1_OVF_vect, vintr_TIMER1)
MAP_SIMPLE_INTERRUPT(TIMER2_OVF_vect, vintr_TIMER2)
MAP_SIMPLE_INTERRUPT(ADC_vect, vintr_ADC)

/*{{{ ATmega328 serial interrupts */
#if defined(atmega328p)
ISR(USART_RX_vect) {
	/* Disable the interrupt to stop it firing again immediately. */
	UCSR0B &= ~_BV (RXCIE0);

	handle_interrupt (&interrupts[vintr_USART_RX]);
}
ISR(USART_UDRE_vect) {
	/* Disable the interrupt to stop it firing again immediately. */
	UCSR0B &= ~_BV (UDRIE0);

	handle_interrupt (&interrupts[vintr_USART_UDRE]);
}
MAP_SIMPLE_INTERRUPT(USART_TX_vect, vintr_USART_TX)
/*}}}*/
/*{{{ ATmega1280 serial interrupts */
#elif defined(atmega1280)
#define SERIAL_INTERRUPT(vect_RX, vintr_RX, \
												 vect_TX, vintr_TX, \
												 vect_UDRE, vintr_UDRE, \
												 UCSRnB, RXCIEn, UDRIEn) \
	ISR(vect_RX) { \
	UCSRnB &= ~_BV (RXCIEn); \
	handle_interrupt (&interrupts[vintr_RX]); \
	} \
	ISR(vect_UDRE) { \
	UCSRnB &= ~_BV (UDRIEn); \
	handle_interrupt (&interrupts[vintr_UDRE]); \
	} \
	MAP_SIMPLE_INTERRUPT(vect_TX, vintr_TX)

SERIAL_INTERRUPT(USART0_RX_vect, vintr_USART_RX0, 
								 USART0_TX_vect, vintr_USART_TX0,
								 USART0_UDRE_vect, vintr_USART_UDRE0,
								 UCSR0B, RXCIE0, UDRIE0)

SERIAL_INTERRUPT(USART1_RX_vect, vintr_USART_RX1, 
								 USART1_TX_vect, vintr_USART_TX1,
								 USART1_UDRE_vect, vintr_USART_UDRE1,
								 UCSR1B, RXCIE1, UDRIE1)

SERIAL_INTERRUPT(USART2_RX_vect, vintr_USART_RX2, 
								 USART2_TX_vect, vintr_USART_TX2,
								 USART2_UDRE_vect, vintr_USART_UDRE2,
								 UCSR2B, RXCIE2, UDRIE2)

SERIAL_INTERRUPT(USART3_RX_vect, vintr_USART_RX3, 
								 USART3_TX_vect, vintr_USART_TX3,
								 USART3_UDRE_vect, vintr_USART_UDRE3,
								 UCSR3B, RXCIE3, UDRIE3)

#endif
/*}}}*/

void clear_pending_interrupts () {
	int i;

	for (i = 0; i < NUM_INTERRUPTS; i++) {
		vinterrupt *intr = &interrupts[i];
		if ((((WORD) intr->wptr) & 1) != 0) {
			/* This interrupt has fired; reschedule the process. */
			context.add_to_queue (&context, (WORDPTR) (((WORD) intr->wptr) & ~1));
			intr->wptr = (WORDPTR) NOT_PROCESS_P;
			--num_waiting;
		}
	}

	cli ();
	context.sflags &= ~TVM_INTR_SFLAGS;
	sei ();
}

int waiting_on_interrupts () {
	return (num_waiting > 0);
}

int ffi_wait_for_interrupt (ECTX ectx, WORD args[]) {
	WORD interrupt = args[0];
	WORDPTR time_ptr = (WORDPTR) args[1];

	if (interrupt < 0 || interrupt >= NUM_INTERRUPTS) {
		return ectx->set_error_flag (ectx, EFLAG_FFI);
	}

	return wait_interrupt (&interrupts[interrupt], ectx, time_ptr);
}
