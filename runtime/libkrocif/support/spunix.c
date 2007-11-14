/*
 *	KRoC interface to SP library
 *	Copyright (C) 1996 Michael Poole
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * $Source: /proj/ofa/inmoslibs/hostio/RCS/spunix.c,v $
 *
 * $Id: spunix.c,v 1.6 1997/10/23 17:10:22 djb1 Exp $
 *
 * (C) Copyright 1996 M.D. Poole <M.D.Poole@ukc.ac.uk>
 * University of Kent at Canterbury
 * Modified by Fred Barnes, 2000, 2001, 2002
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <unistd.h>

#if defined(HAVE_STRING_H)
#include <string.h>
#elif defined(HAVE_STRINGS_H)
#include <strings.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#else
	/* just prototype them and hope they exist.. */
	extern char *getenv (const char *name);
	extern int system (const char *string);
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <errno.h>
#if defined(TIME_WITH_SYS_TIME)
#include <sys/time.h>
#include <time.h>
#elif defined(HAVE_SYS_TIME_H)
#include <sys/time.h>
#else
#include <time.h>
#endif


#include <assert.h>

#include <ccsp.h>
#include <spunixhdr.h>

extern char *long_cmdline, *short_cmdline;

#define SP_OK 0
#define SP_ERROR 128
#define SP_ACCESS_DENIED (-1)
#define SP_INVALID_HANDLE (-2)
#define SP_BUFFER_OVERFLOW (-3)

typedef long PTRINT;

void
call_occam_exit (void)
{
	ccsp_exit (0, false);
}

void
C_fopen (int *handle, int p_name, int p_mode)
{
	FILE *Fd;
	PTRINT P_name = p_name;
	PTRINT P_mode = p_mode;
	Fd = fopen ((char *) P_name, (char *) P_mode);
	*handle = (int) ((PTRINT) Fd & 0xffffffff);
}

void
C_fflush (int *result, int handle)
{
	PTRINT Handle = handle;
	if (fflush ((FILE *) (Handle))) {
		if (errno == EBADF)
			*result = SP_INVALID_HANDLE;
		else
			*result = SP_ERROR;
	} else
		*result = SP_OK;
}

void
C_fclose (int *result, int handle)
{
	PTRINT Handle = handle;
	if (fclose ((FILE *) (Handle))) {
		if (errno == EBADF)
			*result = SP_INVALID_HANDLE;
		else
			*result = SP_ERROR;
	} else
		*result = SP_OK;
}

void
C_fread (int *result, int handle, int p_buffer, int SIZEbuffer, int *bytes_read)
{
	PTRINT P_buffer = p_buffer;
	PTRINT Handle = handle;
	FILE *Fhandle = (handle == 0) ? stdin : (FILE *) (Handle);
	*bytes_read = fread ((void *) (P_buffer), 1, SIZEbuffer, Fhandle);
	*result = SP_OK;
}

void
C_fgets (int *result, int handle, int p_buffer, int SIZEbuffer, int *bytes_read)
{
	char terminator;
	PTRINT P_buffer = p_buffer;
	PTRINT Handle = handle;
	FILE *Fhandle = (handle == 0) ? stdin : (FILE *) (Handle);
	char *str = fgets ((void *) (P_buffer), SIZEbuffer, Fhandle);
	if (str == NULL) {
		*bytes_read = 0;
		*result = SP_ERROR;
		return;
	} else
		*bytes_read = strlen (str);
	if (*bytes_read == 0) {
		*result = SP_ERROR;
		return;
	}
	terminator = ((char *) P_buffer)[*bytes_read - 1];
	if (terminator != '\n') {
		while (terminator != '\n')
			terminator = (char) fgetc (Fhandle);	/* read file to end of line */
		*result = SP_BUFFER_OVERFLOW;
	} else {
		while ((terminator == '\n') || (terminator == '\r'))	/* handle DOS text files better */
			terminator = ((char *) P_buffer)[--(*bytes_read)];
		((char *) P_buffer)[++(*bytes_read)] = '\0';
		*result = SP_OK;
	}
}
void
C_fwrite (int *result, int handle, int p_buffer, int SIZEbuffer, int *bytes_written)
{
	PTRINT P_buffer = p_buffer;
	PTRINT Handle = handle;
	FILE *Fhandle = (handle == 1) ? stdout : (handle == 2) ? stderr : (FILE *) (Handle);
	*bytes_written = fwrite ((void *) (P_buffer), 1, SIZEbuffer, Fhandle);
	if (handle == 1)
		fflush (Fhandle);
	*result = SP_OK;
}

void
C_fremove (int *result, int p_fname)
{
	PTRINT P_fname = p_fname;
	if (remove ((char *) (P_fname)))
		*result = SP_ERROR;
	else
		*result = SP_OK;
}

void
C_frename (int *result, int p_oldname, int p_newname)
{
	PTRINT P_oldname = p_oldname;
	PTRINT P_newname = p_newname;
	if (rename ((char *) (P_oldname), (char *) (P_newname)))
		*result = SP_ERROR;
	else
		*result = SP_OK;
}

void
C_fseek (int *result, int handle, int origin, int position)
{
	PTRINT Handle = handle;
	if (fseek ((FILE *) (Handle), position, origin))
		*result = SP_ERROR;
	else
		*result = SP_OK;
}

void
C_ftell (int *result, int handle, int *position)
{
	PTRINT Handle = handle;
	*position = ftell ((FILE *) (Handle));
	if (*position < 0)
		*result = SP_ERROR;
	else
		*result = SP_OK;
}

void
C_comdline (int *result, int all, int *len, int p_block, int SIZEblock)
{
	PTRINT P_block = p_block;
	if (all)
		strcpy ((char *) (P_block), long_cmdline);
	else
		strcpy ((char *) (P_block), short_cmdline);
	*len = strlen ((char *) (P_block));
	assert (*len < (SIZEblock - 1));
	*result = SP_OK;
}

void
C_getenv (int *result, int p_envname, int *len, int p_block, int SIZEblock)
{
	PTRINT P_envname = p_envname;
	PTRINT P_block = p_block;
	char *Name;

#if 0
/* debug */
	fprintf (stderr, "C_getenv: result @ %p, p_envname = [%s], len @ %p, P_block @ %p, SIZEblock = %d\n", result, (char *) P_envname, len, (char *) P_block, SIZEblock);
#endif
	Name = (char *) getenv ((char *) (P_envname));
	if (Name == NULL) {
#if 0
		fprintf (stderr, "C_getenv: yuk! getenv() returned NULL..\n");
#endif
		*result = SP_ERROR;
	} else {
#if 0
/* debug */
		fprintf (stderr, "C_getenv: looks good.., result is [%s]\n", Name);
#endif
		*len = strlen (Name);
		assert (*len < (SIZEblock - 1));
		(void) strcpy ((char *) (P_block), Name);
		*result = SP_OK;
	}
}
void
C_time (int *loctime, int *UTCtime)
{
#if defined(TARGET_CPU_ALPHA)
	struct timespec tp;
	getclock (TIMEOFDAY, &tp);
	*loctime = tp.tv_sec;
#else
	/*if defined(TARGET_CPU_SPARC) */
	struct timeval tp;	/* resolution 1 usec?   */
#if 0	/* DEBUG */
fprintf (stderr, "C_time: here! loctime @ %p, UTCtime @ %p\n", loctime, UTCtime);
#endif
	(void) gettimeofday (&tp, 0);
	*loctime = tp.tv_sec;
#endif
	*UTCtime = 0;
}

void
C_system (int *result, int *rstatus, int p_block)
{
	PTRINT P_block = p_block;
	*rstatus = system ((char *) (P_block));
	*result = SP_OK;
}

void
C_exit (int *result, int estatus)
{
#if defined(TARGET_CPU_ALPHA)
	atexit (call_occam_exit);
#else
	call_occam_exit ();
#endif
	exit (estatus);		/* never returns */
}

