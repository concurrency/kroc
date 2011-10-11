#include <stdlib.h>
#include <stdio.h>

#include "tvm_nacl.h"

int tvm_base64_decode (const char *src, uint8_t *dst)
{
	uint8_t buffer = 0;
	int length = 0;
	int byte = 0;
	int i;

	for (i = 0; src[i] != '\0'; ++i) {
		const char c = src[i];
		uint8_t bits;
		
		if (c >= 'A' && c <= 'Z') {
			bits = (c - 'A');
		} else if (c >= 'a' && c <= 'z') {
			bits = 26 + (c - 'a');
		} else if (c >= '0' && c <= '9') {
			bits = 52 + (c - '0');
		} else if (c == '+') {
			bits = 62;
		} else if (c == '/') {
			bits = 63;
		} else {
			continue;
		}

		switch (byte) {
			case 0:
				buffer = (bits << 2);
				byte = 1;
				break;
			case 1:
				buffer |= (bits >> 4);
				dst[length++] = buffer;
				buffer = bits << 4;
				byte = 2;
				break;
			case 2:
				buffer |= ((bits >> 2) & 0xf);
				dst[length++] = buffer;
				buffer = bits << 6;
				byte = 3;
				break;
			case 3:
				buffer |= bits;
				dst[length++] = buffer;
				byte = 0;
				break;
		}
	}

	if (byte > 0) {
		dst[length++] = buffer;
	}

	return length;
}
