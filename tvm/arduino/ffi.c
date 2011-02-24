#include "tvm-arduino.h"

/* Copy a block of memory from the Transterpreter's possibly-virtual memory
   space into real memory. */
static void memcpy_from_tvm (BYTEPTR from, void *to, int size) {
	uint8_t *real_to = (uint8_t *) to;

	while (size > 0) {
		*real_to = read_byte (from);
		from = byteptr_plus (from, 1);
		++real_to;
		--size;
	}
}

/* FIXME: Get rid of IFDEF. We do not want these in the code. */
SFFI_FUNCTION sffi_table[] = {
	ffi_wait_for_interrupt,
	#if defined(atmega1280) || defined(atmega2560)
	ffi_read_buffer_blocking
	#endif
};
const int sffi_table_length = sizeof(sffi_table) / sizeof(SFFI_FUNCTION);
