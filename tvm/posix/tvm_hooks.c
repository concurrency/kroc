#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "interpreter/transputer.h"
#include "interpreter/mem.h"

#include "tvm_hooks.h"

/* Include IO code */
#ifdef WIN32
#include "win32_io.h" /* Win32 IO code */
#else
#include "unix_io.h"
#endif /* WIN32 */

#if (defined MEMORY_INTF_BIGENDIAN)
#	define Swap4OnBigendian(a) SwapFourBytes(a)
#else
#	define Swap4OnBigendian(a) a
#endif

static FFI_FUNCTION hooks[] = {
	HOOK_READCHAR,
	HOOK_PRINTSCREEN,
	HOOK_FLUSHSCREEN,
	HOOK_PRINTERROR,
	HOOK_FLUSHERROR,
	HOOK_PTR,
	HOOK_VER,
	HOOK_EXIT,
	HOOK_DEBUG_STR,
	HOOK_DEBUG_INT
};

FFI_FUNCTION *get_special_hooks()
{
	return hooks;
}

/* Callable from occam programs run on the tvm
 * Allows the programmer to get a pointer to ther arguments */
void HOOK_PTR(int w[]) 
{
	//printf("tvm_hook called!: argc %i, argv addr %p\n", tvm_argc, &tvm_argv);
	*((int *)w[0]) = tvm_argc;
	*((int *)w[1]) = (int) tvm_argv;
	return;
}

/* Returns the TVM version we are running. */
void HOOK_VER(int w[])
{
	/* This makes sure that no more is copied than there is space allocated.
	 * w[1] contains the size of the BYTE[] array that is being passed in from occam*/
	strncpy((char *)w[0], VERSION, w[1]);
	return;
}

/* Exit (like really really exit) with some error code */
void HOOK_EXIT(int w[])
{
	/* This makes sure that no more is copied than there is space allocated.
	 * w[1] contains the size of the BYTE[] array that is being passed in from occam*/
	exit(Swap4OnBigendian((int)w[0]));
}


void HOOK_READCHAR(int w[])
{
	static int firsttime = 1;

	/* Do initialisation, once */
	if(firsttime)
	{
		/* Initialise the terminal and set up the restore function */
		init_terminal();
		atexit(restore_terminal);
		/* Make sure we don't do that again... */
		firsttime = 0;
	}

	/* Return -1 if nothing was available, and the character otherwise */
	if(char_available())
	{
		//*((int *)w[0]) = read_char();
		write_word(Swap4OnBigendian(w[0]), read_char());
		//printf("available %p %d\n", w[0], *((int *)w[0]));
	}
	else
	{
		//*((int *)w[0]) = -1;
		write_word(Swap4OnBigendian(w[0]), -1);
		//printf("unavailable\n");
	}
}

void HOOK_PRINTSCREEN(int w[])
{
	fprintf(stdout, "%c", Swap4OnBigendian(w[0]));
}

void HOOK_FLUSHSCREEN(int w[])
{
	fflush(stdout);
}

void HOOK_PRINTERROR(int w[])
{
	fprintf(stderr, "%c", Swap4OnBigendian(w[0]));
}

void HOOK_FLUSHERROR(int w[])
{
	fflush(stderr);
}

void HOOK_DEBUG_STR(int w[])
{
	/*FIXME: check for endianism - (w[0] is the string, w[1] is the length of the string)*/
  char *str = (char *) Swap4OnBigendian(w[0]);
  int i, leng = (int) Swap4OnBigendian(w[1]);
	for (i = 0 ; i < leng ; i++)
	{
		printf("%c", str[i]);
	}

	fflush(stdout);
}

void HOOK_DEBUG_INT(int w[])
{
	printf("%i", Swap4OnBigendian(w[0]));
}
