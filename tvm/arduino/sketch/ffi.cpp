#include "tvm-arduino.h"

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
		int32_t *baud = (int32_t *) args[0];

		Serial.begin (*baud);

		return SFFI_OK;
	}

	static int ffi_serialWrite (ECTX ectx, WORD args[]) {
		const uint8_t *string = (const uint8_t *) args[0];
		int length = (int) args[1];

		Serial.write (string, length);

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
