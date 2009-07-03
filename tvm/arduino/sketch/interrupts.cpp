#include "tvm-arduino.h"

static int num_waiting = 0;

static WORDPTR timer1_channel = (WORDPTR) NOT_PROCESS_P;
static WORD timer1_pending = MIN_INT;
static WORDPTR timer2_channel = (WORDPTR) NOT_PROCESS_P;
static WORD timer2_pending = MIN_INT;

// This runs in interrupt context, so no need to cli()/sei().
static void raise_tvm_interrupt (WORD flag) {
	context.sflags |= SFLAG_INTR | flag;
}

static void handle_interrupt_channel (WORDPTR *channel, WORD *pending, WORD flag) {
	WORD now = millis ();
	if (*channel != (WORDPTR) NOT_PROCESS_P) {
		WORDPTR ptr = (WORDPTR) WORKSPACE_GET (*channel, WS_POINTER);
		write_word (ptr, now);
		WORKSPACE_SET (*channel, WS_POINTER, NULL_P);
		raise_tvm_interrupt (flag);
	} else {
		if (now == MIN_INT) {
			++now;
		}
		*pending = now;
	}
}

static void clear_interrupt_channel (WORDPTR *channel) {
	context.add_to_queue (&context, *channel);
	*channel = (WORDPTR) NOT_PROCESS_P;
	--num_waiting;
}

static int in_interrupt_channel (WORDPTR *channel, WORD *pending, ECTX ectx, BYTEPTR pointer) {
	bool reschedule;

	cli ();

	if (*pending != MIN_INT) {
		write_word (pointer, *pending);
		*pending = MIN_INT;
		reschedule = false;
	} else {
		*channel = ectx->wptr;
		WORKSPACE_SET (*channel, WS_POINTER, (WORD) pointer);
		++num_waiting;
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
	ISR(TIMER1_OVF_vect) {
		static int ticks = 0;

		ticks++;
		if (ticks % 20 == 0) {
			handle_interrupt_channel (&timer1_channel, &timer1_pending, TVM_INTR_TIMER1);
		}
	}
	ISR(TIMER2_OVF_vect) {
		handle_interrupt_channel (&timer2_channel, &timer2_pending, TVM_INTR_TIMER2);
	}
}

void clear_pending_interrupts () {
	if ((context.sflags & TVM_INTR_TIMER1) != 0) {
		clear_interrupt_channel (&timer1_channel);
	}
	if ((context.sflags & TVM_INTR_TIMER2) != 0) {
		clear_interrupt_channel (&timer2_channel);
	}

	cli ();
	context.sflags &= ~TVM_INTR_SFLAGS;
	sei ();
}

bool waiting_on_interrupts () {
	return (num_waiting > 0);
}

static int timer1_in (ECTX ectx, WORD count, BYTEPTR pointer) {
	return in_interrupt_channel (&timer1_channel, &timer1_pending, ectx, pointer);
}

static int timer2_in (ECTX ectx, WORD count, BYTEPTR pointer) {
	return in_interrupt_channel (&timer2_channel, &timer2_pending, ectx, pointer);
}

extern "C" {
	EXT_CHAN_ENTRY ext_chan_table[] = {
		{ 0, timer1_in, NULL, NULL, NULL },
		{ 0, timer2_in, NULL, NULL, NULL }
	};
	const int ext_chan_table_length = sizeof(ext_chan_table) / sizeof(EXT_CHAN_ENTRY);
}
