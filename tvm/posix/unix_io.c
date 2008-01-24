
#include <unistd.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <sys/time.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mem.h"

/* #if defined(HAVE_IOCTL_H) && defined(HAVE_TERMIOS_H) */
//#if defined _MAC_UNIX || defined LINUX ||defined SOLARIS

#if defined(_FREEBSD) || defined(_OPENBSD) || defined(_NETBSD) || defined(_MAC_UNIX)
#define TCGETS TIOCGETA
#define TCSETS TIOCSETA
#endif /* _FREEBSD || _OPENBSD || _NETBSD || _MAC_UNIX */

static struct termios oldT;



void init_terminal()
{
	struct termios newT;
	
	ioctl(0, TCGETS, &oldT); /*get current mode */
	newT = oldT;
	newT.c_lflag &= ~ECHO; /* echo off */
	newT.c_lflag &= ~ICANON; /* one char @ a time*/
	ioctl(0, TCSETS, &newT); /* set new terminal mode */
}

void restore_terminal()
{
	ioctl(0, TCSETS, &oldT); /* restore previous terminal mode */
}

int char_available()
{
	fd_set rfds;
	struct timeval tv;

	/* Watch FD 0 (stdin) */
	FD_ZERO(&rfds);
	FD_SET(0, &rfds);
	/* Do not wait for any time at all */
	tv.tv_sec = 0;
	tv.tv_usec = 0;

	/* Check if we got data available.
	   Note that the select might fail if it gets interrupted by a signal,
	   so we need to check that it's returned 1. */
	return select(1, &rfds, NULL, NULL, &tv) == 1;
}

char read_char()
{
	char ch;
	
	read (0, &ch, 1);

	return ch;
}

/*
#	define init_terminal init_terminal_unix
#	define restore_terminal restore_terminal_unix
#	define char_available char_available_unix
#	define read_char read_char_unix
#endif
*/

#if 0
/* This is an INT channel, it sends either a character, if one was obtained,
   or -1 if no characters are awailable at the current time. */
static void ext_chan_kyb_nonblocking(WORD count, BPOOTER address)
{
}

#ifndef WIN32
static void ext_chan_kyb_blocking(WORD count, BPOOTER address)
{
#ifndef WIN32
	struct termios oldT, newT;
#endif /* WIN32 */
	BYTE ch;

	if(count != 1)
	{
		fprintf(stderr, "STIWC ERROR: count for KYB channel != 1\n");
		exit(1);
	}
#if ((! defined (CYGWIN)) && (! defined (WIN32)))
	ioctl(0,TCGETS,&oldT); /*get current mode */
	newT=oldT;
	newT.c_lflag &= ~ECHO; /* echo off */
	newT.c_lflag &= ~ICANON; /*one char @ a time*/
	ioctl(0,TCSETS,&newT); /* set new terminal mode */
#endif /* WIN32 */

	read (0, &ch, 1);
	write_byte(address, ch);
#if ((!defined WIN32) && (!defined CYGWIN))
	ioctl(0,TCSETS,&oldT); /* restore previous terminal mode */
#endif /* WIN32 */
}
#endif

#endif

#ifdef POOTERS_REAL

void ext_chan_scr(int count, char* address)
{
	unsigned char c;

	if(count != 1)
	{
		fprintf(stderr, "STIWC ERROR: count for SCR channel != 1\n");
		exit(1);
	}
	c = read_byte(address);
	if(c == 0xFF)
	{
		fflush(stdout);
	}
	else
	{
		fprintf(stdout, "%c", c);
		if(c == '\n')
		{
			fflush(stdout);
		}
	}
}

void ext_chan_err(int count, char* address)
{
	int i;

	for(i = 0; i < count; i++)
	{
		fprintf(stderr, "%c", read_byte(address + i));
		/*fprintf(stderr, "i: %d a: %p (%c)\n", i, address, address[i]); */
	}
}

#endif

