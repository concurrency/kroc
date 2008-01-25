#include "stiw.h"

#define MSECS 1000000
#define NSEC  1000

#ifndef WIN32
#include <time.h>
void tvm_sleep(void)
{
	unsigned int now = get_time();
	if(AFTER(tnext[pri], now))
	{
		struct timespec to;
		unsigned int timeout = tnext[pri] - now;

		/* The - 1000 is a bit arbitrary really... just felt it'd be a good thing to
		 * do, it probably isnt :)
		 */
		to.tv_sec = (timeout / MSECS);
		to.tv_nsec = (((timeout - (to.tv_sec * MSECS)) * NSEC) - 1000);

		/* printf("now: %u, timeout: %u, tnext: %u, tptr: 0x%08x, tv_sec: %u, tv_nsec: %u\n", now, timeout, tnext[pri], tptr[pri], to.tv_sec, to.tv_nsec); */

		/* This might get interrupted, in which case it return with -1 or something,
		 * but we dont really care... as we'll just go round the scheduling loop
		 * again, which is fine.
		 */
		nanosleep(&to, 0);
	}
}

#else

#ifdef WIN32
#define BYTE WIN_BYTE
#define WORD WIN_WORD
#define UWORD WIN_UWORD
#include <windows.h>
#undef BYTE
#undef WORD
#undef UWORD 
#endif

void tvm_sleep(void)
{
	unsigned int now = get_time();
	if(AFTER(tnext[pri], now))
	{
		unsigned int timeout = (tnext[pri] - now) / 1000;
		Sleep(timeout);
	}
}
#endif

