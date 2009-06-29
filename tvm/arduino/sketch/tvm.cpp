#include "tvm-arduino.h"

extern "C" {
	// G++ requires this to be defined somewhere, as the behaviour for when
	// you call a pure virtual method. arduino-0016 appears to need it if
	// you're using the serial code, but defines it with the wrong
	// extern-ness, hence this workaround.
	void __cxa_pure_virtual() {
	}
}

static tvm_t tvm;
static tvm_ectx_t context;

#define MEM_WORDS 256
static WORD memory[MEM_WORDS];

// The bytecode file, loaded into flash at a fixed address.
static const prog_char *tbc_data = (prog_char *) OCCAM_PROGRAM_ADDR;

extern "C" {
	// Time is in milliseconds, since microseconds wrap round too fast in
	// 16 bits to be useful.
	static WORD arduino_get_time (ECTX ectx) {
		return millis ();
	}
}

static void terminate(const char *message, const int *status) {
	// FIXME: offer other behaviours as options
	Serial.print("tvm-arduino: ");
	Serial.print(message);
	if (status != NULL) {
		Serial.print(*status, 0); // BYTE
	}
	Serial.println();

	while (true) {}
}

int main () {
	// Set up the Arduino environment.
	init ();

	Serial.begin (57600);
#ifdef DEBUG
	Serial.println ("Arduino-TVM starting...");
#endif

	for (int i = 0; i < MEM_WORDS; i++) {
		memory[i] = MIN_INT;
	}

	tvm_init (&tvm);
	tvm_ectx_init (&tvm, &context);

	if (init_context_from_tbc (&context, tbc_data, memory, MEM_WORDS) != 0) {
		terminate ("program loading failed", NULL);
	}

	context.get_time = arduino_get_time;
	context.sffi_table = sffi_table;
	context.sffi_table_length = sffi_table_length;

#ifdef DEBUG
	int a;
	Serial.print("stack pointer is (more or less) ");
	hexprint((int) &a);
	Serial.println();
	Serial.print("memory is ");
	hexprint((int) &memory[0]);
	Serial.println();
#endif

#ifdef DEBUG
	Serial.print("wptr is ");
	hexprint((int) context.wptr);
	Serial.println();
	Serial.print("iptr is ");
	hexprint((int) context.iptr);
	Serial.println();
	delay(1000);
	Serial.println("GO!");
#endif

	while (true) {
#ifdef DEBUG
		Serial.print("about to tvm_run with eflags=");
		hexprint((int) context.eflags);
		Serial.print(" iptr=");
		hexprint((int) context.iptr);
		Serial.print(" wptr=");
		hexprint((int) context.wptr);
		Serial.println();

		int ret = tvm_run_count (&context, 1);

		Serial.print("tvm_run returned ");
		Serial.print(ret, DEC);
		Serial.print(" ");
		Serial.print(ret, 0); // BYTE
		Serial.print(" eflags=");
		hexprint((int) context.eflags);
		Serial.print(" iptr=");
		hexprint((int) context.iptr);
		Serial.print(" wptr=");
		hexprint((int) context.wptr);
		Serial.println();
#else
		int ret = tvm_run (&context);
#endif
		switch (ret) {
			case ECTX_PREEMPT:
			case ECTX_TIME_SLICE: {
				// Safe to continue.
				break;
			}
			case ECTX_SLEEP: {
				WORD now = millis();
				WORD next = context.tnext;
				if (TIME_AFTER (next, now)) {
					delay(next - now);
				}
				break;
			}
			case ECTX_EMPTY: {
				// FIXME: handle waiting on interrupts, etc.
				terminate("deadlock", NULL);
			}
			case ECTX_SHUTDOWN: {
				terminate("end of program", NULL);
			}
			default: {
				terminate("error status ", &ret);
			}
		}
	}

	// NOTREACHED
	return 0;
}
