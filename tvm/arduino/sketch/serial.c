#include "tvm-arduino.h"

static int serial_write(char c, FILE *f) {
	/* Wait for transmit buffer to be empty */
	while ((UCSR0A & _BV(UDRE0)) == 0)
		;

	/* Put byte into transmit buffer */
	UDR0 = c;

	return 0;
}
void serial_stdout_init (long speed) {
	/* Set baud rate */
	uint16_t factor = (F_CPU / 16 + speed / 2) / speed - 1;
	UBRR0H = factor >> 8;
	UBRR0L = factor;

	/* Set format (8N1) */
	UCSR0C = 3 << UCSZ00;

	/* Enable transmitter */
	UCSR0B = _BV(TXEN0);

	/* Set up stdout to write to the serial port */
	stdout = fdevopen (serial_write, NULL);
}
