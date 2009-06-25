// The Wiring stuff defines BYTE as 0.
#undef BYTE

extern "C" {
	#include <tvm.h>
}

static tvm_t tvm;
static tvm_ectx_t context;

/* Blinking LED and Transterpreter
 * -------------------------------
 *
 * turns on and off a light emitting diode(LED) connected to a digital  
 * pin, in intervals of 2 seconds. Ideally we use pin 13 on the Arduino 
 * board because it has a resistor attached to it, needing only an LED

 *
 * Created 1 June 2005
 * copyleft 2005 DojoDave <http://www.0j0.org>
 * http://arduino.berlios.de
 *
 * based on an orginal by H. Barragan for the Wiring i/o board
 */

int ledPin = 13;                 // LED connected to digital pin 13

void setup() {
	pinMode(ledPin, OUTPUT);      // sets the digital pin as output
	for (int i = 0; i < 5; i++) {
		digitalWrite(ledPin, HIGH);   // sets the LED on
		delay(100);
		digitalWrite(ledPin, LOW);    // sets the LED off
		delay(100);
	}
}

void loop() {
	tvm_init (&tvm);
	tvm_ectx_init (&tvm, &context);
	int ret = tvm_run (&context);
}
