#include "tvm-arduino.h"
#include "util/delay.h"

#define BITDELAY9600      84  
#define HALFBITDELAY9600  42
#define BITDELAY4800     188
#define HALFBITDELAY4800  94

/* DDRD, PORTD, PINB are Arduino digital 0 - 7 
 * Only use 2 - 7, as 0, 1 are already serial.
 */

int ffi_softserial_write (ECTX ectx, WORD args[])
{
	int  tx_pin = args[0];
	int  byte   = args[1];
	char mask;
 	
	printf_P (PSTR ("S %d, %04x\n"), tx_pin, byte);
	// vprintf(PSTR("p: %04x, b %04x\n"), tx_pin, byte);

	cli();

	/* Set pin as output */
	DDRD |= (1 << tx_pin);

	/* Pull the write pin low to start transmission */
  PORTD &= ~(1 << tx_pin);

	_delay_us(BITDELAY4800);

  /* Bang out each bit on the pin */
	for (mask = 0x01 ; mask ; mask <<= 1) {
		if (byte & mask) {
			PORTD |= (1 << tx_pin);
		} else {
			PORTD &= ~(1 << tx_pin);
		}
		_delay_us(BITDELAY4800);
	}

	/* Pull high to send stop bit.*/
	PORTD |= (1 << tx_pin);
	_delay_us(BITDELAY4800);
	
	sei();
	
	return SFFI_OK;
}

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

SFFI_FUNCTION sffi_table[] = {
	ffi_wait_for_interrupt,
	ffi_softserial_write
};

const int sffi_table_length = sizeof(sffi_table) / sizeof(SFFI_FUNCTION);
