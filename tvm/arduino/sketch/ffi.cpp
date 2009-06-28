#include "tvm-arduino.h"

// Copy a block of memory from the Transterpreter's possibly-virtual memory
// space into real memory.
static void memcpy_from_tvm (BYTEPTR from, void *to, int size) {
	uint8_t *real_to = (uint8_t *) to;

	while (size > 0) {
		*real_to = read_byte (from);
		from = byteptr_plus (from, 1);
		++real_to;
		--size;
	}
}

extern "C" {
	static int ffi_digitalWrite (ECTX ectx, WORD args[]) {
		WORD pin = args[0];
		WORD state = args[1];

		digitalWrite(pin, state);

		return SFFI_OK;
	}

	static int ffi_digitalRead (ECTX ectx, WORD args[]) {
		WORD pin = args[0];
		WORD *state = (WORD *) args[1];

		*state = digitalRead(pin);

		return SFFI_OK;
	}

	static int ffi_pinMode (ECTX ectx, WORD args[]) {
		WORD pin = args[0];
		WORD mode = args[1];

		pinMode(pin, mode);

		return SFFI_OK;
	}

	static int ffi_beginSerial (ECTX ectx, WORD args[]) {
		int32_t baud;
		memcpy_from_tvm ((BYTEPTR) args[0], &baud, sizeof baud);

		Serial.begin (baud);

		return SFFI_OK;
	}

	static int ffi_serialWrite (ECTX ectx, WORD args[]) {
		BYTEPTR string = (BYTEPTR) args[0];
		int length = (int) args[1];

		for (int i = 0; i < length; i++) {
			uint8_t c;
			memcpy_from_tvm (byteptr_plus (string, i), &c, 1);

			Serial.write (&c, 1);
		}

		return SFFI_OK;
	}

	static int ffi_serialAvailable (ECTX ectx, WORD args[]) {
		WORD *result = (WORD *) args[0];

		*result = Serial.available ();

		return SFFI_OK;
	}

	static int ffi_serialRead (ECTX ectx, WORD args[]) {
		WORD *result = (WORD *) args[0];

		*result = Serial.read ();

		return SFFI_OK;
	}

	static int ffi_serialFlush (ECTX ectx, WORD args[]) {
		Serial.flush ();

		return SFFI_OK;
	}

	SFFI_FUNCTION sffi_table[] = {
		ffi_digitalWrite,
		ffi_digitalRead,
		ffi_pinMode,
		ffi_beginSerial,
		ffi_serialWrite,
		ffi_serialAvailable,
		ffi_serialRead,
		ffi_serialFlush
	};
	const int sffi_table_length = sizeof(sffi_table) / sizeof(SFFI_FUNCTION);
}
