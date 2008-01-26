#include "stiw.h"

#define MSECS 1000000
#define NSEC  1000

#ifndef WIN32
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_TIME_H
#include <time.h>
#endif

void stiw_sleep(void)
{
	unsigned int now = (unsigned int) tvm_get_time();
	if(TIME_AFTER(tnext, now))
	{
		struct timespec to;
		unsigned int timeout = tnext - now;

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

/* FIXME: Should we sanity check for time > 0 here? */
void stiw_set_alarm(WORD time)
{
	struct itimerval timeoutval;

	if(time <= 0)
	{
		/* If we got something which is less than zero then the timeout already
		 * happened.
		 * FIXME: Do something more intelligent than just setting the alarm even
		 * though we have already timed out. */
		time = 1;
	}

	timerclear(&timeoutval.it_interval);	
	timerclear(&timeoutval.it_value);	
	timeoutval.it_value.tv_sec = time / 1000000;
	timeoutval.it_value.tv_usec = time % 1000000;

	if(setitimer(ITIMER_REAL, &timeoutval, NULL) != 0)
	{
		/* If setitimer fails then we're going to deadlock, so it's
		 * better to explicitly blow up here. */
		/* FIXME: exit_runloop(EXIT_SCHEDULER_BAD_1); */
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

void stiw_sleep(void)
{
	unsigned int now = (unsigned int) tvm_get_time();
	if(TIME_AFTER(tnext, now))
	{
		unsigned int timeout = (tnext - now) / 1000;
		Sleep(timeout);
	}
}
#endif

