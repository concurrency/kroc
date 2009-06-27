#include "tvm-arduino.h"

#ifdef DEBUG
static const char digits[] = "0123456789abcdef";
void hexprint(int n) {
	char a[5];

	a[4] = '\0';

	int p = 3;
	while (p >= 0) {
		int d = n & 0xF;
		n >>= 4;

		a[p--] = digits[d];
	}

	Serial.write((const uint8_t *) &a[0], 4);
}
#endif
