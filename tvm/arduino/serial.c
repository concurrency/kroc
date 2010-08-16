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

/* FIXME: We really don't want these IFDEF statements.
   Further, this should be made to work on the Arduino as well.
*/
#if defined(atmega1280)
char rc0(void) {
			while ( !(UCSR0A & (1 << RXC0)) )
				;
			return UDR0;
}

char rc1(void) {
			while ( !(UCSR1A & (1 << RXC1)) )
				;
			return UDR1;
}

char rc2(void) {
			while ( !(UCSR2A & (1 << RXC2)) )
				;
			return UDR2;
}

char rc3(void) {
			while ( !(UCSR3A & (1 << RXC3)) )
				;
			return UDR3;
}

#define VAL_BYTE(w) (*(char*)&(w))
#define BYTE(w) ((char *)(w)) 
#define INT(w) ((int *)(w))
#define VAL_INT(w) (w)
int ffi_read_buffer_blocking (ECTX ectx, WORD args[]) {
	int   port         = VAL_INT(args[0]);

	char  start_char  = VAL_BYTE(args[1]);
	char  end_char    = VAL_BYTE(args[2]);
	char* buffer      = BYTE(args[3]);
	/* int   buffer_leng = VAL_INT(args[4]); */
	int*  result      = INT(args[5]);

	int   count = 0;
	char  ch;
	/* Declare the fn ptr */
	char (*rca[])(void) = { rc0, rc1, rc2, rc3 };

	cli();	

	/* Flush the USART */
	rca[port]();

	/* Look for the start character */
	do {
		ch = rca[port]();
	} while (ch != start_char);

	/* Fill the buffer */
	do {
		ch = rca[port]();
		buffer[count] = ch;
		count++;
	} while (ch != end_char);
	
	*result = count - 1;
	sei();

	return SFFI_OK;
}
#endif

