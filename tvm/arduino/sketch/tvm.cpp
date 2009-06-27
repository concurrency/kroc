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

#include "blink.h"

extern "C" {
	// Time is in milliseconds, since microseconds wrap round too fast in
	// 16 bits to be useful.
	static WORD arduino_get_time (ECTX ectx) {
		return millis ();
	}
}

int main () {
	// Set up the Arduino environment.
	init ();

	Serial.begin (57600);
#ifdef DEBUG
	Serial.println ("Arduino-TVM starting...");
#endif

	tvm_init (&tvm);
	tvm_ectx_init (&tvm, &context);

	context.get_time = arduino_get_time;
	for (int i = 0; i < MEM_WORDS; i++) {
		memory[i] = MIN_INT;
	}
	context.sffi_table = sffi_table;
	context.sffi_table_length = sffi_table_length;

#ifdef DEBUG
	int a;
	Serial.print("stack pointer is (more or less) ");
	hexprint((int) &a);
	Serial.println("");
	Serial.print("memory is ");
	hexprint((int) &memory[0]);
	Serial.println("");
	Serial.print("code is ");
	hexprint((int) transputercode);
	Serial.println("");
#endif

	WORDPTR ws, vs;
	tvm_ectx_layout (
		&context, memory,
		"", 0,
		ws_size, vs_size,
		&ws, &vs
	);
	int ret = tvm_ectx_install_tlp (
		&context, (BYTEPTR) transputercode,
		ws, vs,
		"", 0, NULL
	);
	if (ret != 0) {
		Serial.print("install_tlp failed");
	}

#ifdef DEBUG
	Serial.print("ws is ");
	hexprint((int) ws);
	Serial.println("");
	Serial.print("wptr is ");
	hexprint((int) context.wptr);
	Serial.println("");
	Serial.print("iptr is ");
	hexprint((int) context.iptr);
	Serial.println("");
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
		Serial.println("");

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
		Serial.println("");
#else
		int ret = tvm_run (&context);
#endif
		if (ret == ECTX_EMPTY || ret == ECTX_SLEEP) {
			// tvm_sleep ();
		}
	}

	// NOTREACHED
	return 0;
}
