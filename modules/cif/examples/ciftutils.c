
/* ciftutils.c -- CIF test utilities */

#include <cif.h>

void out_string (Workspace wptr, const char *str, Channel *out)
{
	while (*str != '\0') {
		ChanOutChar (wptr, out, *str);
		str++;
	}
}

