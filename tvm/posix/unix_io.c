
#include "tvm_posix.h"

#if !defined(TCGETS) && defined(TIOCGETA)
#define TCGETS TIOCGETA
#endif
#if !defined(TCSETS) && defined(TIOCSETA)
#define TCSETS TIOCSETA
#endif

static struct termios oldT;

void init_terminal (void)
{
	struct termios newT;
	
	ioctl (0, TCGETS, &oldT); /*get current mode */
	newT = oldT;
	newT.c_lflag &= ~ECHO; /* echo off */
	newT.c_lflag &= ~ICANON; /* one char @ a time*/
	ioctl (0, TCSETS, &newT); /* set new terminal mode */
}

void restore_terminal (void)
{
	ioctl (0, TCSETS, &oldT); /* restore previous terminal mode */
}

int char_available (void)
{
	fd_set rfds;
	struct timeval tv;

	/* Watch FD 0 (stdin) */
	FD_ZERO (&rfds);
	FD_SET (0, &rfds);
	/* Do not wait for any time at all */
	tv.tv_sec = 0;
	tv.tv_usec = 0;

	/* Check if we got data available.
	   Note that the select might fail if it gets interrupted by a signal,
	   so we need to check that it's returned 1. */
	return select (1, &rfds, NULL, NULL, &tv) == 1;
}

BYTE read_char (void)
{
	BYTE ch;
	
	read (0, &ch, 1);

	return ch;
}

