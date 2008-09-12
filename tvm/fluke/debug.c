#include "fluke.h"

static const unsigned char digits[] = "0123456789abcdef";

void debug_print_chr (const unsigned char c)
{ 
  uart0_send_char(c);
}

void debug_print_hex (unsigned int val)
{
  int i;

  debug_print_str ("0x");
  for (i = 7; i >= 0; --i) {
    unsigned int n = (val >> (i << 2)) & 0xf;
    debug_print_chr (digits[n]);
  }
}


void debug_print_str (const char *str)
{
  const unsigned char *p = (unsigned char *) str;
  unsigned char c;

  while ((c = *p++) != '\0') {
    debug_print_chr (c);
  }
}
