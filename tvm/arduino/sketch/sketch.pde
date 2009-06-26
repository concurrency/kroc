// vim:syntax=cpp

// The Wiring stuff defines BYTE as 0.
#undef BYTE

extern "C" {
	#include <tvm.h>
}

static tvm_t tvm;
static tvm_ectx_t context;

#include "nothing.h"

#define MEM_WORDS 42
static WORD memory[MEM_WORDS];

int ledPin = 13;                 // LED connected to digital pin 13

extern "C" {
	static WORD arduino_get_time(ECTX ectx) {
		return micros();
	}
}

void setup() {
	Serial.begin(57600);
	Serial.println("Arduino-TVM starting...");

	pinMode(ledPin, OUTPUT);      // sets the digital pin as output
	for (int i = 0; i < 5; i++) {
		digitalWrite(ledPin, HIGH);   // sets the LED on
		delay(500);
		digitalWrite(ledPin, LOW);    // sets the LED off
		delay(500);
	}

	Serial.println("tvm_init");
	tvm_init (&tvm);
	Serial.println("tvm_ectx_init");
	tvm_ectx_init (&tvm, &context);

	context.get_time = arduino_get_time;
	for (int i = 0; i < MEM_WORDS; i++) {
		memory[i] = MIN_INT;
	}

	WORDPTR ws, vs;
	tvm_ectx_layout (
		&context, memory,
		"", 0,
		ws_size, vs_size,
		&ws, &vs
	);
	tvm_ectx_install_tlp (
		&context, (BYTEPTR) transputercode,
		ws, vs,
		"", 0, NULL
	);
	Serial.print("sizeof int is ");
	Serial.print(sizeof (int), DEC);
	Serial.println("");
	Serial.print("ws is ");
	Serial.print((int) ws, DEC);
	Serial.println("");
}

void loop() {
	Serial.println("at top of run loop, about to tvm_run");
	int ret = tvm_run (&context);
	Serial.print("tvm_run returned ");
	Serial.print(ret, DEC);
	Serial.println("");
	if (ret == ECTX_EMPTY || ret == ECTX_SLEEP) {
		// tvm_sleep ();
	}
}
