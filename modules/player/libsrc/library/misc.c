#include <stdio.h>


//Magic things to make these work on windows
//Is magical mon.
#ifdef WIN32
#define DLLPREFIX extern __declspec(dllexport)
#else
#define DLLPREFIX
#endif


static int magic_debug_counter;


void r_magic_debug_init()
{
  magic_debug_counter = 0;
}

DLLPREFIX void _magic_debug_init(int *w)
{
  r_magic_debug_init();
}

void increment_counter()
{
  magic_debug_counter++;
}

void r_magic_debug_newline()
{
  increment_counter();
  printf("\r\n%d: ", magic_debug_counter);
}

void r_magic_string(char* str, int leng)
{
  int i;

  for(i = 0 ; i < leng ; i++)
    {
      printf("%c", str[i]);
    }

  fflush(stdout);
}

void r_magic_int(int x)
{
  printf("%d", x);
  fflush(stdout);
}

void r_magic_real32(float x)
{
  printf("%f", x);
  fflush(stdout);
}

DLLPREFIX void _magic_string(int *w) 
{
  r_magic_string((char*)w[0], (int)w[1]);
}

DLLPREFIX void _magic_int(int *w) 
{
  r_magic_int((int)w[0]);
}

DLLPREFIX void _magic_real32(int *w) 
{
  r_magic_real32(*((float *)w));
}
DLLPREFIX void _magic_newline(int *w)
{
  r_magic_debug_newline();
}
