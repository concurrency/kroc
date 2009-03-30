#include <cdefBF537.h>
#include "bfin_config.h"
#include "uart.h"

#define SSYNC    asm("ssync;")

void init_uarts(void)
{
    *pPORTF_FER |= 0x000f;  // enable UART pins

    *pUART0_GCTL = UCEN;
    *pUART0_LCR = DLAB;
    *pUART0_DLL = UART0_DIVIDER;
    *pUART0_DLH = UART0_DIVIDER >> 8;
    *pUART0_LCR = WLS(8); // 8 bit, no parity, one stop bit

    // dummy reads to clear possible pending errors / irqs
    char dummy = *pUART0_RBR;
    dummy = *pUART0_LSR;
    dummy = *pUART0_IIR;
    SSYNC;

    *pUART1_GCTL = UCEN;
    *pUART1_LCR = DLAB;
    *pUART1_DLL = UART1_DIVIDER;
    *pUART1_DLH = UART1_DIVIDER >> 8;
    *pUART1_LCR = WLS(8); // 8 bit, no parity, one stop bit

    dummy = *pUART1_RBR;
    dummy = *pUART1_LSR;
    dummy = *pUART1_IIR;
    SSYNC;
}

void uart0SendChar(unsigned char c)
{
    while (!(*pUART0_LSR & THRE));
    *pUART0_THR = c;
}

void uart0SendString(unsigned char *s)
{
    char a;
    while ((a = *s++)) {
        uart0SendChar(a);
    }
}

void uart0SendChars(unsigned char *buf, unsigned int size)
{
    while (size--) {
        uart0SendChar(*buf++);
    }
}

unsigned char uart0GetCh()
{
    while (!(*pUART0_LSR & DR));
    return *pUART0_RBR;
}

unsigned char uart0GetChar(unsigned char *a)
{
    if (!(*pUART0_LSR & DR))
        return 0;
    *a = *pUART0_RBR;
    return 1;
}

void uart1SendChar(unsigned char c)
{
    while (!(*pUART1_LSR & THRE));
    *pUART1_THR = c;
}

void uart1SendString(unsigned char *s)
{
    char a;
    while ((a = *s++)) {
        uart1SendChar(a);
    }
}

void uart1SendChars(unsigned char *buf, unsigned int size)
{
    while (size--) {
        uart1SendChar(*buf++);
    }
}

unsigned char uart1GetCh()
{
    while (!(*pUART1_LSR & DR));
        return *pUART1_RBR;
}

unsigned char uart1GetChar(unsigned char *a)
{
    if (!(*pUART1_LSR & DR))
        return 0;
    *a = *pUART1_RBR;
    return 1;
}

/*****************************************************************************
 *
 * Description:
 *    Routine for printing integer numbers in various formats. The number is 
 *    printed in the specified 'base' using exactly 'noDigits', using +/- if 
 *    signed flag 'sign' is TRUE, and using the character specified in 'pad' 
 *    to pad extra characters. 
 *
 * Params:
 *    [in] base     - Base to print number in (2-16) 
 *    [in] noDigits - Number of digits to print (max 32) 
 *    [in] sign     - Flag if sign is to be used (TRUE), or not (FALSE) 
 *    [in] pad      - Character to pad any unused positions 
 *    [in] number   - Signed number to print 
 *
 ****************************************************************************/
void
printNumber(unsigned char  base,
            unsigned char  noDigits,
            unsigned char  sign,
            unsigned char  pad,
            unsigned int number)
{
  static unsigned char  hexChars[16] = "0123456789ABCDEF";
  unsigned char        *pBuf;
  unsigned char         buf[32];
  unsigned int        numberAbs;
  unsigned int        count;

  // prepare negative number
  if(sign && (number < 0))
    numberAbs = -number;
  else
    numberAbs = number;

  // setup little string buffer
  count = (noDigits - 1) - (sign ? 1 : 0);
  pBuf = buf + sizeof(buf);
  *--pBuf = '\0';

  // force calculation of first digit
  // (to prevent zero from not printing at all!!!)
  *--pBuf = hexChars[(numberAbs % base)];
  numberAbs /= base;

  // calculate remaining digits
  while(count--)
  {
    if(numberAbs != 0)
    {
      //calculate next digit
      *--pBuf = hexChars[(numberAbs % base)];
      numberAbs /= base;
    }
    else
      // no more digits left, pad out to desired length
      *--pBuf = pad;
  }

  // apply signed notation if requested
  if(sign)
  {
    if(number < 0)
      *--pBuf = '-';
    else if(number > 0)
       *--pBuf = '+';
    else
       *--pBuf = ' ';
  }

  // print the string right-justified
  uart0SendString(pBuf);
}

