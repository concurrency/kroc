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

	SFFI_FUNCTION sffi_table[] = {
		ffi_digitalWrite,
		ffi_digitalRead,
		ffi_pinMode
	};
	const int sffi_table_length = sizeof(sffi_table) / sizeof(SFFI_FUNCTION);
}
