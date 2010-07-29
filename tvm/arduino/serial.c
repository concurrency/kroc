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

/* Params:
 * VAL BYTE start.char
 * VAL BYTE end.char
 * []BYTE buffer
 * RESULT INT count
 */
#define VAL_BYTE(w) (*(char*)&(w))
#define BYTE(w) ((char *)(w)) 
#define INT(w) ((int *)(w))
#define VAL_INT(w) (w)


char read_char_0() {
			while ( !(UCSR0A & (1 << RXC0)) )
				;
			return UDR0;
}

char read_char_1() {
			while ( !(UCSR1A & (1 << RXC1)) )
				;
			return UDR1;
}

char read_char_2() {
			while ( !(UCSR2A & (1 << RXC2)) )
				;
			return UDR2;
}

char read_char_3() {
			while ( !(UCSR3A & (1 << RXC3)) )
				;
			return UDR3;
}

int ffi_read_buffer_blocking (ECTX ectx, WORD args[]) {
	int   port         = VAL_INT(args[0]);

	char  start_char  = VAL_BYTE(args[1]);
	char  end_char    = VAL_BYTE(args[2]);
	char* buffer      = BYTE(args[3]);
	int   buffer_leng = VAL_INT(args[4]);
	int*  result      = INT(args[5]);

	int   count = 0;
	int   running = 1;
	char  ch;

	char (*read_chars[])() = { read_char_0, read_char_1, read_char_2, read_char_3 };
	char (*read_char)();
	read_char = read_chars[port];

	cli();	

	/* Look for the start character */
	do {
		ch = read_char();
	} while (ch != start_char);

	/* Fill the buffer */
	while (running) {
		if (count >= buffer_leng) {
			running = 0;
		} else { 
			ch = read_char();
			if (ch == end_char) {
				running = 0;
			} else {
				buffer[count] = ch;
				count++;
			}
		}
	}
	
	*result = count;
	sei();

	return SFFI_OK;
}
