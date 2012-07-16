#include "tvm-scc.h"

int _print(ECTX ectx, WORD args[])
{
	char* str = (char*)args[0];
	int len = args[1];
	int pos = 0;
	volatile char buffer[256];

	while(pos < len)
	{
		buffer[pos] = str[pos];
		pos++;
	}
	buffer[len] = '\0';
	
	printf(buffer);
	
	return SFFI_OK;
}

SFFI_FUNCTION sffi_table[] = {_print};
const int sffi_table_length = sizeof(sffi_table) / sizeof(SFFI_FUNCTION);
