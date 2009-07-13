#include "tvm-arduino.h"

static int num_waiting = 0;

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
	} else if (((WORD) intr->wptr) & 1 != 0) {
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
