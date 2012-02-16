/* $Id: argcmn.c,v 1.2 1997/03/25 14:52:40 djb1 Exp $ */

/*
 *	routines for old and new arglib
 *	Copyright (C) 1989, 1990, 1994 Inmos Limited
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

/* History */

/* bas: First version written - moved out of old arg.c */

/*{{{  Includes */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include "arg.h"
#include "arg0.h"
#include <imsmisc.h>

#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#if defined(HOST_OS_IS_SERVER)
#include <host.h>
#endif
/*}}}  */
/*{{{  Defines */
#ifndef COMPONENT_ID
#define COMPONENT_ID "[UNDER-DEVELOPMENT]"
#endif
/*}}}  */
/*{{{  Local Functions */
/*{{{  to_lower */
/* To prevent problems with some implementations that do not behave */
/* as ANSI defines. */

static int to_lower (int c) {
	if ((c >= 'A') && (c <= 'Z')) return (tolower (c));
	return (c);
}
/*}}}  */
/*}}}  */
/*{{{  Exported Functions */
/*{{{  arg_host_switch_char */
char arg_host_switch_char (void) {
	char prefix_character;

#if (defined(HOST_OS_IS_MSDOS) || defined(HOST_OS_IS_VMS))
	prefix_character = '/';
#elif (defined(HOST_OS_IS_UNIX))
	prefix_character = '-';
#elif (defined(HOST_OS_IS_SERVER))
	{
		int host, os, board;

		host_info (&host, &os, &board);
		switch (os) {
			case _IMS_OS_DOS:
			case _IMS_OS_VMS:
				prefix_character = '/';
				break;

			case _IMS_OS_HELIOS:
			case _IMS_OS_SUNOS:
				prefix_character = '-';
				break;
		}
	}
#else
#include <assert.h>
	assert(0);
#endif

	return (prefix_character);
}
/*}}}  */
/*{{{  arg_host_alternate_switch_char */
char arg_host_alternate_switch_char (void) {
	char prefix_character;
#if (defined(HOST_OS_IS_MSDOS) || defined(HOST_OS_IS_VMS) || defined(HOST_OS_IS_UNIX) || defined(HOST_OS_IS_SERVER))
	prefix_character = '-';
#else
#include <assert.h>
	assert(0);
#endif

	return (prefix_character);
}
/*}}}  */
/*{{{  arg2_copy_token */
/* This function copies a token from a string into allocated storage */
/* If the token starts with a quote, then the token is a string - */
/* quotes are stripped off and doubled quotes are reduced to single. */
/* If any errors were found, we might still have to copy it. */
/* If a malloc fails, then the routine returns NULL */

char const *arg2_copy_token (char const * const st, char const * const en) {
	char *buf, *dst;
	char const *src;

	if (en == NULL) {
		buf = (char *) malloc (sizeof (char));
		if (buf == NULL) return (NULL);
		dst = buf;
	} else {
		buf = (char *) malloc ((en - st + 1) * sizeof (char));
		if (buf == NULL) return (NULL);
		dst = buf;
		for (src = (*st == QUOTE ? st+1 : st); src < en; ++src) {
			if ((*src == QUOTE) && (*st == QUOTE)) {
				src += 1;
				if (src == en) break;
			}
			*dst = *src;
			dst += 1;
		}
	}
	*dst = NUL;

	return ((char const *) buf);
}
/*}}}  */
/*{{{  arg2_compare */
/* This function compares two strings. If they are identical, then */
/* the result is 'ident'. If the first is a prefix substring of the */
/* second, then the result is 'prefix'. Otherwise, the result is */
/* 'noteq'. */

str_compare arg2_compare (char const * const sub, char const * const str) {
	char const *sub_ch, *str_ch;

	sub_ch = sub;                                                                                                   /* Expected substring */
	str_ch = str;                                                                                                   /* Check start of this string */
	while (true) {
		if (*sub_ch == NUL) {                                                           /* End of substring */
			if (*str_ch == NUL) {                                                   /* Also end of the string */
				return (ident);                                                                 /* They are identical */
			} else {
				return (prefix);                                                                /* Substring is prefix of string */
			}
		}

		if (to_lower ((int) *sub_ch) != to_lower ((int) *str_ch)) {
			return (noteq);                                                                         /* Difference found */
		}

		sub_ch += 1;                                                                                            /* Keep searching */
		str_ch += 1;
	}
}
/*}}}  */
/*{{{  arg2_new_string */
/* This function creates a new string to be passed to the user for */
/* processing. This ensures that the original command line tokens are */
/* not changed by the parser -- they may of course be clobbered by */
/* the user directly.   Replaces first occurrance of \n\0 with \0\0 */
/* in effect removes a new-line char if there is one at the end of the */
/* string. */

char *arg2_new_string (char const *const old_str) {
	char *new_str, *p;

	new_str = (char *) malloc (sizeof (char) * (strlen (old_str) + 1));
	if (new_str == NULL) return (NULL);
	strcpy (new_str, old_str);
	p = new_str;
	while (*p != NUL) p++;
	if ((p > new_str) && (*(p-1) == NL)) *(p-1) = '\0';

	return (new_str);
}
/*}}}  */
/*{{{  arg_version */
/* This function returns the configuration version string of the         */
/* library as built.                                                     */

/* This function is deprecated - it serves no useful purpose now ICMS is */
/* dead. I will leave it just in case people are using it.               */

static char const * const cms_version = COMPONENT_ID;

char const *arg_version (void) {
	return (cms_version);
}
/*}}}  */
/*}}}  */
