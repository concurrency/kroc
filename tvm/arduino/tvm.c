#include "tvm-arduino.h"

static tvm_t tvm;
tvm_ectx_t context;

#define MEM_WORDS 700
static WORD raw_memory[MEM_WORDS + 1];
static WORDPTR memory;

/* The bytecode file, loaded into flash at a fixed address. */
static const prog_char *tbc_data = (prog_char *) BYTECODE_ADDR;
static BYTEPTR initial_iptr;

/* Time is in milliseconds, since microseconds wrap round too fast in
   16 bits to be useful. */
static WORD arduino_get_time (ECTX ectx) {
	return time_millis ();
}

static void arduino_modify_sync_flags (ECTX ectx, WORD set, WORD clear) {
	cli ();
	ectx->sflags = (ectx->sflags & (~clear)) | set;
	sei ();
}

static void dump_machine_state () {
	WORDPTR wp;
	BYTEPTR bp;
	const prog_char *file;
	UWORD line;
	const UWORD iptr_offset = (UWORD) (context.iptr - initial_iptr);

	if (tbc_file_and_line (tbc_data, iptr_offset, &file, &line) == 0) {
		printf_P (PSTR ("\nfile=%S line=%d"), file, (int) line);
	}

	printf_P (PSTR ("\nwptr=%04x (rel=%04x)  iptr=%04x (rel=%04x)  "
	                "eflags=%04x sflags=%04x\n"
	                "areg=%04x breg=%04x creg=%04x  oreg=%04x\n"),
	          (int) context.wptr, (int) (context.wptr - memory),
	          (int) context.iptr, (int) (context.iptr - initial_iptr),
	          context.eflags, context.sflags,
	          context.areg, context.breg, context.creg, context.oreg);

	for (wp = context.wptr - 7; wp < context.wptr + 7; ++wp) {
		if (wp == context.wptr) {
			printf_P (PSTR ("wptr>"));
		}
		printf_P (PSTR ("%04x "), read_word (wp));
	}
	printf_P (PSTR ("\n"));

	for (bp = context.iptr - 12; bp < context.iptr + 12; ++bp) {
		if (bp == context.iptr) {
			printf_P (PSTR ("iptr>"));
		}
		printf_P (PSTR ("%02x "), read_byte (bp));
	}
	printf_P (PSTR ("\n"));
}

void terminate (const prog_char *message, const int *status) {
	/* FIXME: offer other behaviours as options */
	printf_P (PSTR ("tvm-arduino: %S"), message);
	if (status != NULL) {
		printf_P (PSTR ("%c"), *status);
	}
	printf_P (PSTR ("\n"));

	if (status != NULL) {
		printf_P (PSTR ("\nFinal machine state:"));
		dump_machine_state ();
	}

	while (1) {}
}

int main () {
	int i;

	time_init ();
	init_interrupts ();

	/* Enable interrupts, now all the handlers are set up. */
	sei ();

	serial_stdout_init (57600);

#ifdef DEBUG
	printf_P (PSTR ("Arduino-TVM starting...\n"));
#endif

	/* The Transputer memory must be word-aligned. */
	memory = (WORDPTR) (((int) (raw_memory + 1)) & ~1);

	for (i = 0; i < MEM_WORDS; i++) {
		memory[i] = MIN_INT;
	}

	tvm_init (&tvm);
	tvm_ectx_init (&tvm, &context);

	if (init_context_from_tbc (&context, tbc_data, memory, MEM_WORDS) != 0) {
		terminate (PSTR ("program loading failed"), NULL);
	}
	initial_iptr = context.iptr;

	context.get_time = arduino_get_time;
	context.modify_sync_flags = arduino_modify_sync_flags;
	context.sffi_table = sffi_table;
	context.sffi_table_length = sffi_table_length;

#ifdef DEBUG
	dump_machine_state ();
#endif

	while (1) {
#ifdef DEBUG
		printf_P (PSTR ("Before tvm_run:"));
		dump_machine_state ();

		int ret = tvm_run_count (&context, 1);

		printf_P (PSTR ("After tvm_run = %d (%c):"), ret, ret);
		dump_machine_state ();
#else
		int ret = tvm_run (&context);
#endif
		switch (ret) {
			case ECTX_PREEMPT:
			case ECTX_TIME_SLICE: {
				/* Safe to continue. */
				break;
			}
			case ECTX_SLEEP: {
				WORD next = context.tnext;
				WORD now;
				do {
					/* FIXME: sleep, rather than busywaiting */
					now = time_millis ();
				} while (TIME_AFTER (next, now));
				break;
			}
			case ECTX_INTERRUPT: {
				clear_pending_interrupts ();
				break;
			}
			case ECTX_EMPTY: {
				if (!waiting_on_interrupts ()) {
					terminate(PSTR ("deadlock"), NULL);
				}
				break;
			}
			case ECTX_SHUTDOWN: {
				terminate(PSTR ("end of program"), NULL);
			}
			default: {
				terminate(PSTR ("error status "), &ret);
			}
		}
	}

	/* NOTREACHED */
	return 0;
}
