#include "tvm-scc.h"

/* Copy a block of memory from the Transterpreter's possibly-virtual memory
   space into real memory. */
static void memcpy_from_tvm (BYTEPTR from, void *to, int size) {
	BYTE *real_to = (BYTE *) to;

	while (size > 0) {
		*real_to = read_byte (from);
		from = byteptr_plus (from, 1);
		++real_to;
		--size;
	}
}

void print(char* str)
{
	printf(str);
}

void _print(ECTX ectx, WORD args[])
{
	print((char*)args[0]);
}

SFFI_FUNCTION sffi_table[] = {_print};
const int sffi_table_length = sizeof(sffi_table) / sizeof(SFFI_FUNCTION);
