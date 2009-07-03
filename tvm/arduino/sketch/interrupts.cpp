#include "tvm-arduino.h"

static WORDPTR timer_channel = (WORDPTR) NOT_PROCESS_P;
static WORD timer_fired = MIN_INT;
static bool timer_pending = false;

// This runs in interrupt context, so no need to cli()/sei().
static void raise_tvm_interrupt (WORD flag) {
	context.sflags |= SFLAG_INTR | flag;
}

extern "C" {
	ISR(TIMER1_OVF_vect) {
		static int ticks = 0;

		ticks++;
		if (ticks % 20 == 0) {
			timer_fired = millis ();
			if (timer_channel != (WORDPTR) NOT_PROCESS_P) {
				WORDPTR ptr = (WORDPTR) WORKSPACE_GET (timer_channel, WS_POINTER);
				write_word (ptr, timer_fired);
				WORKSPACE_SET (timer_channel, WS_POINTER, NULL_P);
				raise_tvm_interrupt (TVM_INTR_TIMER);
			} else {
				timer_pending = true;
			}
		}
	}
}

void clear_pending_interrupts () {
	if ((context.sflags & TVM_INTR_TIMER) != 0) {
		context.add_to_queue (&context, timer_channel);
		timer_channel = (WORDPTR) NOT_PROCESS_P;
	}

	cli ();
	context.sflags &= ~TVM_INTR_SFLAGS;
	sei ();
}

bool waiting_on_interrupts () {
	bool waiting;

	// If timer_channel contains a process, there is a process blocked on
	// this channel, either before or after the interrupt handler is fired.
	waiting = (timer_channel != (WORDPTR) NOT_PROCESS_P);

	return waiting;
}

static int timer_in (ECTX ectx, WORD count, BYTEPTR pointer) {
	bool reschedule;

	cli ();

	if (timer_pending) {
		write_word (pointer, timer_fired);
		timer_pending = false;
		reschedule = false;
	} else {
		timer_channel = ectx->wptr;
		WORKSPACE_SET (timer_channel, WS_POINTER, (WORD) pointer);
		reschedule = true;
	}

	sei ();

	if (reschedule) {
		WORKSPACE_SET (ectx->wptr, WS_IPTR, (WORD) ectx->iptr);
		return ectx->run_next_on_queue (ectx);
	} else {
		return ECTX_CONTINUE;
	}
}

extern "C" {
	EXT_CHAN_ENTRY ext_chan_table[] = {
		{ 0, timer_in, NULL, NULL, NULL }
	};
	const int ext_chan_table_length = sizeof(ext_chan_table) / sizeof(EXT_CHAN_ENTRY);
}
