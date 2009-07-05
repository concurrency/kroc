/* $Id: fni.c,v 1.3 1997/08/22 10:38:42 mdp2 Exp $ */

/*
 *	implements the filename interpreter
 *	Copyright (C) 1991, 1993 Inmos Limited
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
/* ahp: First version written */
/* stevec: 16-Jul-1993:
           Fixes for INSdi02357 and INSdi02358. Renamed as version 1.1. */
/* stevec: 11-Aug-1993:
           Fixes for INSdi02424 and INSdi02425. Renamed as version 1.2. */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#ifndef FNI_VERSION
#define FNI_VERSION "$Revision: 1.3 $"
#endif

#include "fnilib.h"
#include "fnidef.h"		/* bits that are shared with test case              */
#include <ctype.h>
#include <stddef.h>
#include <stdlib.h>
#include <assert.h>

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#include <string.h>

#if defined(HOST_OS_IS_SERVER)
#include <host.h>
#endif

#define NUL         '\0'
#define semi_colon  ';'
#define full_colon  ':'

#if !defined(HOST_OS_IS_VMS)
typedef enum
{ false = 0, true = 1 } bool;
#else
typedef int bool;

#define false 0
#define true 1
#endif

/* fni_access_info holds all the information which persists during a set of
                   calls (session) to the fnilib library.  An initial call
                   to fni_open_access() creates and returns an fni_access_info
                   record which must then be passed in to all calls to fnilib
                   functions.  At the end of the session, a terminating call
                   to fni_close_access() should be made. */
struct fni_access_info
{
	void *(*malloc_fn) (size_t);	/* Function to use to allocate memory      */
	void (*free_fn) (void *);	/* Function used to free memory allocated  */
	/* using malloc_fn                         */
	fni_style style;	/* File-name format being used             */
	bool debug;		/* If true, diagnostics will be emitted by */
	/* called fnilib functions                 */
	char *wk_buffer;	/* Workspace buffer used during filename   */
	/* construction and deconstruction         */
	int wk_next;		/* Next available char in wk_buffer        */
	int wk_length;		/* Current length of wk_buffer             */
};



typedef enum
{
	dir_none,
	dir_normal,
	dir_level,
	dir_up,
	dir_root
} dir_type;

#if defined(HOST_OS_IS_MSDOS)
#define default_style msdos_style
#elif defined(HOST_OS_IS_VMS)
#define default_style vms_style
#elif defined(HOST_OS_IS_SERVER)
#define default_style host_style
#else
#define default_style unix_style
#endif

/* Supply the version identification for those who are interested         */
char const *
fni_version (void)
{
	static char const *version = FNI_VERSION;
	return (version);
}

fni_access
fni_open_access (void *(*malloc_fn) (size_t), void (*free_fn) (void *))
{
	fni_access result;

	assert (malloc_fn != NULL && free_fn != NULL);

	result = malloc_fn (sizeof (*result));
	result->malloc_fn = malloc_fn;
	result->free_fn = free_fn;
	result->style = default_style;
	result->debug = false;
	result->wk_buffer = NULL;
	result->wk_next = 0;
	result->wk_length = 0;

	return result;
}

void fni_close_access (fni_access * access_p)
{
	if ((*access_p)->wk_buffer != NULL) {
		(*access_p)->free_fn ((*access_p)->wk_buffer);
	}
	(*access_p)->free_fn (*access_p);
	*access_p = NULL;
}

int fni_debug (fni_access access, int debug_on)
{
	bool old_debug = access->debug;
	access->debug = (bool) debug_on;

	return (int) old_debug;
}

static void *get_mem (fni_access access, size_t sz)
{
	return access->malloc_fn (sz);
}

static void free_mem (fni_access access, void *v)
{
	access->free_fn (v);
}

static fni_style get_host_style (void)
{
#ifdef HOST_OS_IS_SERVER
	static fni_style known_style = host_style;
	int host, os, board;

	if (known_style == host_style) {
		host_info (&host, &os, &board);
		switch (os) {
		case _IMS_OS_DOS:
			{
				known_style = msdos_style;
				break;
			}
		case _IMS_OS_VMS:
			{
				known_style = vms_style;
				break;
			}
		default:
			{
				known_style = unix_style;
				break;
			}
		}
	}
	return (known_style);
#else
	return (default_style);
#endif
}

/* fni_set_style:                                                         */
/* This function changes the style for scanning the external file names.  */
/* It is provided to allow the C compiler to try the Unix file type if    */
/* the normal host type fails.                                            */
/* It returns what the style was before the call.                         */

fni_style
fni_set_style (fni_access access, fni_style const st)
{
	fni_style res;

	if ((res = access->style) == host_style)
		res = get_host_style ();
	switch (st) {
	case host_style:
		{
			access->style = get_host_style ();
			break;
		}
	case unix_style:
	case msdos_style:
	case vms_style:
		{
			access->style = st;
			break;
		}
	default:
		{
			access->style = unix_style;
			break;
		}
	}
	return (res);
}

#define unix_dir            '/'
#define unix_ext            '.'
#define unix_up1            ".."
#define unix_same           "."
#define unix_invalid_chars  ""

#define msdos_dev           ':'
#define msdos_dir           '\\'
#define msdos_ext           '.'
#define msdos_up1           ".."
#define msdos_same          "."
#define msdos_invalid_chars "/"

#define vms_dev             ':'
#define vms_dir_beg         '['
#define vms_dir_bg1         '['
#define vms_dir_bg2         '<'
#define vms_dir_up          '-'
#define vms_dir             '.'
#define vms_dir_end         ']'
#define vms_dir_en1         ']'
#define vms_dir_en2         '>'
#define vms_ext             '.'
#define vms_ver             ';'
#define vms_invalid_chars   "/"

/* Define the flags buried in the canonical format                        */
#define cfn_flag    '\001'
#define dev_flag    '\002'
#define dir_flag    '\003'
#define fil_flag    '\004'
#define ext_flag    '\005'
#define end_flag    '\006'
#define usr_flag    'U'
#define sys_flag    'S'
#define cur_flag    '\010'
#define up1_flag    '\011'
#define dot_flag    '.'

/* When a file is opened, a structure is returned that contains the       */
/* system handle and the canonical form of the directory which it         */
/* contains. These are recorded in a local structure.                     */

typedef struct fni_file_data
{
	FILE *fd_handle;
	fni_canonical fd_position;
} file_data;

/* fni_canonical's can be combined into lists, represented by a list      */
/* structure.                                                             */

/* struct fni_list_entry is defined in fnidef.h                           */

static void print_canonical (fni_canonical cfn)
{
	char const *str;
	static char const *const hex_digit = "0123456789ABCDEF";

	if (cfn == NULL) {
		fprintf (stderr, "<Invalid filename>");
		return;
	}
	str = (char const *) cfn;
	while (*str != '\0') {
		if (isprint (*str)) {
			fprintf (stderr, "%c", *str);
		} else {
			fprintf (stderr, "<%c%c>", hex_digit[((*str) >> 4) & 0x0f], hex_digit[(*str) & 0x0f]);
		}
		str += 1;
	}
}






/* Here are the functions to handle the work areas to build up character  */
/* string results.                                                        */

#define wk_default 32

/*****************************************************************************
 *
 *  add_to_work adds the character 'ch' to the work buffer.
 *
 *****************************************************************************/
static void add_to_work (fni_access access, char const ch)
{
	char *w;

	if (access->wk_next >= access->wk_length) {
		if (access->wk_buffer == NULL) {
			access->wk_length = wk_default;
			w = (char *) get_mem (access, access->wk_length * sizeof (char));
		} else {
			access->wk_length *= 2;
			w = (char *) get_mem (access, access->wk_length * sizeof (char));
			memcpy (w, access->wk_buffer, access->wk_next * sizeof (char));
			free_mem (access, access->wk_buffer);
		}
		access->wk_buffer = w;
	}
	access->wk_buffer[access->wk_next] = ch;
	access->wk_next += 1;
}

/*****************************************************************************
 *
 *  add_part adds 'len' characters pointed to by 'ch' to the work buffer.
 *
 *****************************************************************************/
static void add_part (fni_access access, char const *const ch, int const len)
{
	int ix;

	for (ix = 0; ix < len; ++ix) {
		add_to_work (access, ch[ix]);
	}
}

/*****************************************************************************
 *
 *  add_upto adds the characters pointed to by 'ch' to the work buffer until
 *           either the terminator 'term', or a nul-character is seen.
 *           The terminator/nul is NOT added to the work buffer.
 *           Returns a pointer to the first character in 'ch' after the
 *           terminator 'term', or NULL if a nul-character was seen.
 *
 *****************************************************************************/
static char const *add_upto (fni_access access, char const *const ch, char const term)
{
	int ix;

	for (ix = 0; ch[ix] != NUL; ++ix) {
		if (ch[ix] == term) {
			return (&ch[ix + 1]);
		}
		add_to_work (access, ch[ix]);
	}
	return (NULL);
}

/*****************************************************************************
 *
 *  copy_wk makes a copy of the work buffer and returns it.
 *
 *****************************************************************************/
static char const *copy_wk (fni_access access)
{
	char *buf;

	buf = (char *) get_mem (access, access->wk_next * sizeof (char));
	memcpy (buf, access->wk_buffer, access->wk_next);
	return ((char const *) buf);
}

/* These functions extract parts from a file name.                        */
/* They return the length of the part in a pointer parameter; this length */
/* is negative if a syntax error is found while searching for the part.   */
/* They return a pointer to the start of the part; and NULL if the part   */
/* is not present. Note that a part may be present, but have zero length. */

/*****************************************************************************
 *
 *  find_dev returns a pointer to the device part of an external file name
 *           'fn'.  The length of the device part is returned in 'len'.
 *           NB, The device part includes the trailing ':' for VMS.
 *
 *****************************************************************************/
static char const *find_dev (fni_access access, char const *const fn, int *const len)
{
	int ix;

	*len = 0;
	if (fn == NULL) {
		return (NULL);
	}
	switch (access->style) {
	case host_style:
		{
			access->style = get_host_style ();
			return (find_dev (access, fn, len));
		}
	case unix_style:
		{
			return (NULL);
		}
	case msdos_style:
		{
			for (ix = 0; (fn[ix] != NUL) && (fn[ix] != msdos_dir); ++ix) {
				if (fn[ix] == msdos_dev) {
					/* The following checks the implementation restriction that names
					   with devices are rooted, or don't have a directory specifier */
					if (fn[ix + 1] == msdos_same[0]) {
						*len = -1;
					} else {
						*len = ix + 1;
					}
					return (fn);
				}
			}
			return (fn);
		}
	case vms_style:
		{
			bool machine_part_found = false;

			for (ix = 0; (fn[ix] != NUL) && (fn[ix] != vms_dir_bg1) && (fn[ix] != vms_dir_bg2); ++ix) {
				if (fn[ix] == vms_dev) {
					if (fn[ix + 1] == vms_dev) {
						ix += 1;
						machine_part_found = true;
					} else {
						/* The following checks the implementation restriction that
						   names with devices are rooted, or don't have a directory
						   specifier. */
						if (((fn[ix + 1] == vms_dir_bg1) || (fn[ix + 1] == vms_dir_bg2)) && ((fn[ix + 2] == vms_dir) || (fn[ix + 2] == vms_dir_up))) {
							*len = -1;
						} else {
							*len = ix + 1;
						}
						return (fn);
					}
				}
			}

			if (machine_part_found) {
				/* We have a name with a machine part, but not a device part.
				   This is illegal. */
				*len = -1;
			}
			return (fn);
		}
	case other_style:
		{
			assert (false);
		}
	}
	return (NULL);
}


/*****************************************************************************
 *
 *  find_dir takes an external filename, 'fn', and applies the function
 *           'do_dir' to each directory part of it in order.
 *           'do_dir' has the prototype
 *             void do_dir(char const *const dirname,
 *                         int const dirname_length,
 *                         dir_type const type)
 *             Here type can take the values dir_root, dir_up, dir_level,
 *             and dir_normal.  When type has the value dir_normal,
 *             (dirname, dirname_length) give the name and length of the
 *             directory.  For other values of type, dirname and dirname_length
 *             are undefined.
 *
 *****************************************************************************/
static char const *find_dir (fni_access access, char const *const fn, void (*const do_dir) (fni_access, char const *const, int const, dir_type const), int *const len)
{
	int sx, ix;
	char const *dirs;

	*len = 0;
	if (fn == NULL)
		return (NULL);
	switch (access->style) {
	case host_style:
		{
			access->style = get_host_style ();
			return (find_dir (access, fn, do_dir, len));
		}
	case unix_style:
		{
			sx = 0;
			if (fn[sx] == unix_dir) {
				do_dir (access, fn, 0, dir_root);
				sx += 1;
			}
			for (ix = sx; fn[ix] != NUL; ++ix) {
				if (fn[ix] == unix_dir) {
					if (((ix - sx) == strlen (unix_up1)) && (memcmp (&fn[sx], unix_up1, (ix - sx)) == 0)) {
						do_dir (access, fn, 0, dir_up);
					} else if (((ix - sx) == strlen (unix_same)) && (memcmp (&fn[sx], unix_same, (ix - sx)) == 0)) {
						do_dir (access, fn, 0, dir_level);
					} else {
						do_dir (access, &fn[sx], (ix - sx), dir_normal);
					}
					sx = ix + 1;
				}
			}
			return (fn);
		}
	case msdos_style:
		{
			dirs = find_dev (access, fn, len);
			if (*len < 0)
				return (dirs);
			dirs = &dirs[*len];
			sx = 0;
			if (dirs[sx] == msdos_dir) {
				do_dir (access, dirs, 0, dir_root);
				sx += 1;
			}
			for (ix = sx; dirs[ix] != NUL; ++ix) {
				if (dirs[ix] == msdos_dir) {
					if (((ix - sx) == strlen (msdos_up1)) && (memcmp (&dirs[sx], msdos_up1, (ix - sx)) == 0)) {
						do_dir (access, dirs, 0, dir_up);
					} else if (((ix - sx) == strlen (msdos_same)) && (memcmp (&dirs[sx], msdos_same, (ix - sx)) == 0)) {
						do_dir (access, dirs, 0, dir_level);
					} else {
						do_dir (access, &dirs[sx], (ix - sx), dir_normal);
					}
					sx = ix + 1;
				}
			}
			return (dirs);
		}
	case vms_style:
		{
			dirs = find_dev (access, fn, len);
			if (*len < 0)
				return (dirs);
			dirs = &dirs[*len];
			if ((dirs[0] != vms_dir_bg1) && (dirs[0] != vms_dir_bg2))
				return (NULL);
			dirs = &dirs[1];
			sx = 0;
			if ((dirs[sx] == vms_dir_en1) || (dirs[sx] == vms_dir_en2)) {
				do_dir (access, dirs, 0, dir_level);
				return (dirs);
			}
			if (dirs[sx] == vms_dir_up) {
				while (dirs[sx] == vms_dir_up) {
					do_dir (access, dirs, 0, dir_up);
					sx += 1;
					if (dirs[sx] == vms_dir) {
						sx += 1;
					}
					if ((dirs[sx] == vms_dir_en1) || (dirs[sx] == vms_dir_en2)) {
						return (dirs);
					}
				}
			} else if (dirs[sx] == vms_dir) {
				sx += 1;
			} else {
				do_dir (access, dirs, 0, dir_root);
			}
			for (ix = sx; dirs[ix] != NUL; ++ix) {
				if ((dirs[ix] == vms_dir_en1) || (dirs[ix] == vms_dir_en2)) {
					if (ix > sx) {
						/* The above test mean if we get something like
						   [fred.bill.]
						   then we don't think there is an empty directory specifier
						   after 'bill'.  */
						do_dir (access, &dirs[sx], (ix - sx), dir_normal);
					}
					return (dirs);
				}
				if (dirs[ix] == vms_dir) {
					do_dir (access, &dirs[sx], (ix - sx), dir_normal);
					sx = ix + 1;
				}
			}
			*len = -1;
			return (NULL);
		}
	case other_style:
		{
			assert (false);
		}
	}
	return (NULL);
}

/*****************************************************************************
 *
 *  find_fil takes an external filename 'fn' and returns a pointer to
 *           the filename part of it.  The length of the filename part is
 *           returned in len.  NB the filename part does not include the
 *           filename extension.
 *
 *****************************************************************************/
static char const *find_fil (fni_access access, char const *const fn, int *const len)
{
	int ix, ex, lx;

	*len = 0;
	if (fn == NULL) {
		return (NULL);
	}
	lx = strlen (fn);
	ex = lx;
	switch (access->style) {
	case host_style:
		{
			access->style = get_host_style ();
			return (find_fil (access, fn, len));
		}
	case unix_style:
		{
			for (ix = lx - 1; ix >= 0; --ix) {
				if ((fn[ix] == unix_ext) && (ex == lx)) {
					ex = ix;
				}
				if (fn[ix] == unix_dir) {
					break;
				}
			}
			ix += 1;
			*len = ex - ix;
			return (&fn[ix]);
		}
	case msdos_style:
		{
			for (ix = lx - 1; ix >= 0; --ix) {
				if ((fn[ix] == msdos_ext) && (ex == lx)) {
					ex = ix;
				}
				if (fn[ix] == msdos_dir || fn[ix] == msdos_dev) {
					break;
				}
			}
			ix += 1;
			*len = ex - ix;
			return (&fn[ix]);
		}
	case vms_style:
		{
			for (ix = lx - 1; ix >= 0; --ix) {
				if (fn[ix] == vms_ver) {
					lx = ix;
				}
				if ((fn[ix] == vms_ext) && (ex >= lx)) {
					ex = ix;
				}
				if ((fn[ix] == vms_dir_en1) || (fn[ix] == vms_dir_en2) || (fn[ix] == vms_dev)) {
					break;
				}
			}
			ix += 1;
			*len = ex - ix;

			return (&fn[ix]);
		}
	case other_style:
		{
			assert (false);
		}
	}
	return (NULL);
}

/*****************************************************************************
 *
 *  find_ext takes an external filename 'fn', and returns a pointer to the
 *           filename extension part of it. The length of the extension is
 *           returned in len.
 *           If no extension is found len is set to 0.
 *           NB The returned extension and length include the '.' character
 *           marking the start of the extension.
 *
 *****************************************************************************/
static char const *
find_ext (fni_access access, char const *const fn, int *const len)
{
	int ix, lx;

	*len = 0;
	if (fn == NULL) {
		return (NULL);
	}
	lx = strlen (fn);
	switch (access->style) {
	case host_style:
		{
			access->style = get_host_style ();
			return (find_ext (access, fn, len));
		}
	case unix_style:
		{
			for (ix = lx - 1; (ix >= 0) && (fn[ix] != unix_dir); --ix) {
				if (fn[ix] == unix_ext) {
					*len = lx - ix;
					return (&fn[ix]);
				}
			}
			return (&fn[lx]);
		}
	case msdos_style:
		{
			for (ix = lx - 1; (ix >= 0) && (fn[ix] != msdos_dir) && (fn[ix] != msdos_dev); --ix) {
				if (fn[ix] == msdos_ext) {
					*len = lx - ix;
					return (&fn[ix]);
				}
			}
			return (&fn[lx]);
		}
	case vms_style:
		{
			for (ix = lx - 1; (ix >= 0) && (fn[ix] != vms_dir_en1) && (fn[ix] != vms_dir_en2) && (fn[ix] != vms_dev); --ix) {
				if (fn[ix] == vms_ver) {
					lx = ix;
				}
				if (fn[ix] == vms_ext) {
					*len = lx - ix;
					return (&fn[ix]);
				}
			}
			return (&fn[lx]);
		}
	case other_style:
		{
			assert (false);
		}
	}
	return (NULL);
}

/*****************************************************************************
 *
 *  contains_invalid_char returns TRUE if the external filename 'fn'
 *        contains characters which are invalid for the current host.
 *
 *****************************************************************************/
static bool
contains_invalid_char (fni_access access, const char *fn)
{
	const char *invalid_chars;
	if (access->style == host_style)
		access->style = get_host_style ();
	switch (access->style) {
	default:
	case host_style:
		assert (false);
		invalid_chars = NULL;
		break;
	case unix_style:
		invalid_chars = unix_invalid_chars;
		break;
	case msdos_style:
		invalid_chars = msdos_invalid_chars;
		break;
	case vms_style:
		invalid_chars = vms_invalid_chars;
		break;
	}
	return strpbrk (fn, invalid_chars) != NULL;
}


/* add_dir:                                                               */
/* Adds a directory into the internal form.                               */

static void
add_dir (fni_access access, char const *const dir, int const dir_len, dir_type const dt)
{
	switch (dt) {
	case dir_none:
		{
			break;
		}
	case dir_normal:
		{
			add_part (access, dir, dir_len);
			break;
		}
	case dir_level:
		{
			add_to_work (access, cur_flag);
			break;
		}
	case dir_up:
		{
			add_to_work (access, up1_flag);
			break;
		}
	case dir_root:
		{
			break;
		}
	}
	add_to_work (access, dir_flag);
	return;
}

/* fni_internalise:                                                       */
/* Converts a filename in the host's format into a canonical form.        */
/* It returns a fni_canonical with the converted form, or NULL.           */
/* If NULL is returned, it means that the filename was not recognised     */
/* as valid for the current host.                                         */
/* The parameter is the host file name in its external form.              */
/* It is the user's responsibility to free the space obtained for the     */
/* result.                                                                */

fni_canonical
fni_internalise (fni_access access, char const *const ext, fni_file_type const typ)
{
	char const *part;
	int part_len;

	if (access->debug) {
		fprintf (stderr, "Internalise \"%s\" to ", ext);
	}

	if (contains_invalid_char (access, ext)) {
		if (access->debug) {
			fprintf (stderr, "<null>\n");
		}
		return NULL;
	}

	access->wk_next = 0;
	add_to_work (access, cfn_flag);
	add_to_work (access, (typ == fni_user ? usr_flag : sys_flag));
	add_to_work (access, dev_flag);
	part = find_dev (access, ext, &part_len);
	if (part_len < 0) {
		if (access->debug) {
			fprintf (stderr, "<null>\n");
		}
		return NULL;
	}
	add_part (access, part, part_len);
	add_to_work (access, dir_flag);
	part = find_dir (access, ext, add_dir, &part_len);
	if (part_len < 0) {
		if (access->debug) {
			fprintf (stderr, "<null>\n");
		}
		return NULL;
	}
	add_to_work (access, fil_flag);
	part = find_fil (access, ext, &part_len);
	if (part_len < 0) {
		if (access->debug) {
			fprintf (stderr, "<null>\n");
		}
		return NULL;
	}
	add_part (access, part, part_len);
	add_to_work (access, ext_flag);
	part = find_ext (access, ext, &part_len);
	if (part_len < 0) {
		if (access->debug) {
			fprintf (stderr, "<null>\n");
		}
		return NULL;
	}
	add_part (access, part, part_len);
	add_to_work (access, end_flag);
	add_to_work (access, NUL);
	{
		fni_canonical result = (fni_canonical) copy_wk (access);
		if (access->debug) {
			print_canonical (result);
			fputc ('\n', stderr);
		}
		return result;
	}
}

/* fni_externalise:                                                       */
/* Converts a canonical filename into the right form for the current host.*/
/* If it cannot be converted, NULL is returned.                           */
/* Note that it can fail to convert because the host does not support     */
/* some attribute of the name (eg. device names are meaningless on Unix   */
/* systems).                                                              */
/* It is the user's responsibility to free the space obtained for the     */
/* result.                                                                */

char const *fni_externalise (fni_access access, fni_canonical const cfn)
{
	char const *ch;

	if (access->debug) {
		fprintf (stderr, "Externalise ");
		print_canonical (cfn);
		fprintf (stderr, " to ");
	}

	access->wk_next = 0;
	ch = (char const *) cfn;
	if (*ch != cfn_flag) {
		if (access->debug) {
			fprintf (stderr, "<null>\n");
		}
		return (NULL);
	}
	ch += 1;
	if (*ch == NUL) {
		if (access->debug) {
			fprintf (stderr, "<null>\n");
		}
		return (NULL);
	}
	ch += 1;
	if (*ch != dev_flag) {
		if (access->debug) {
			fprintf (stderr, "<null>\n");
		}
		return (NULL);
	}
	ch += 1;
	ch = add_upto (access, ch, dir_flag);
	if (ch == NULL) {
		if (access->debug) {
			fprintf (stderr, "<null>\n");
		}
		return (NULL);
	}

	if (access->style == host_style) {
		access->style = get_host_style ();
	}

	if (*ch != fil_flag) {
		switch (access->style) {
		case host_style:
			{
				if (access->debug) {
					fprintf (stderr, "<null>\n");
				}
				return (NULL);
			}
		case unix_style:
			{
				if (*ch == cur_flag) {
					add_part (access, unix_same, strlen (unix_same));
					ch += 2;
				} else if (*ch == up1_flag) {
					add_part (access, unix_up1, strlen (unix_up1));
					ch += 2;
				} else {
					ch = add_upto (access, ch, dir_flag);
					if (ch == NULL) {
						if (access->debug) {
							fprintf (stderr, "<null>\n");
						}
						return (NULL);
					}
				}
				while (*ch != fil_flag) {
					add_to_work (access, unix_dir);
					if (*ch == cur_flag) {
						add_part (access, unix_same, strlen (unix_same));
						ch += 2;
					} else if (*ch == up1_flag) {
						add_part (access, unix_up1, strlen (unix_up1));
						ch += 2;
					} else {
						ch = add_upto (access, ch, dir_flag);
						if (ch == NULL) {
							if (access->debug) {
								fprintf (stderr, "<null>\n");
							}
							return (NULL);
						}
					}
				}
				add_to_work (access, unix_dir);
				break;
			}
		case msdos_style:
			{
				if (*ch == cur_flag) {
					add_part (access, msdos_same, strlen (msdos_same));
					ch += 2;
				} else if (*ch == up1_flag) {
					add_part (access, msdos_up1, strlen (msdos_up1));
					ch += 2;
				} else {
					ch = add_upto (access, ch, dir_flag);
					if (ch == NULL) {
						if (access->debug) {
							fprintf (stderr, "<null>\n");
						}
						return (NULL);
					}
				}
				while (*ch != fil_flag) {
					add_to_work (access, msdos_dir);
					if (*ch == cur_flag) {
						add_part (access, msdos_same, strlen (msdos_same));
						ch += 2;
					} else if (*ch == up1_flag) {
						add_part (access, msdos_up1, strlen (msdos_up1));
						ch += 2;
					} else {
						ch = add_upto (access, ch, dir_flag);
						if (ch == NULL) {
							if (access->debug) {
								fprintf (stderr, "<null>\n");
							}
							return (NULL);
						}
					}
				}
				add_to_work (access, msdos_dir);
				break;
			}
		case vms_style:
			{
				add_to_work (access, vms_dir_beg);	/* Add a '[' directory opening character */
				if (*ch == cur_flag) {
					/* Skip cur_flag and next directory separator */
					ch += 2;
				} else {
					if (*ch == up1_flag) {
						add_to_work (access, vms_dir_up);	/* Add '-' to go up a level */
						ch += 2;	/* Skip up1_flag and next directory separator */
					} else {
						if (*ch == dir_flag) {
							/* dir_flag immediately following the first dir_flag means
							   the name is rooted */
							ch += 1;	/* Skip the second dir_flag */
							if (*ch != fil_flag) {
								ch = add_upto (access, ch, dir_flag);	/* add the directory name */
								if (ch == NULL) {
									if (access->debug) {
										fprintf (stderr, "<null>\n");
									}
									return (NULL);
								}
							}
						}
					}
					while (*ch != fil_flag) {
						add_to_work (access, vms_dir);	/* Add a '.' directory separator */
						if (*ch == up1_flag) {
							add_to_work (access, vms_dir_up);
							ch += 2;
						} else {
							ch = add_upto (access, ch, dir_flag);
							if (ch == NULL) {
								if (access->debug) {
									fprintf (stderr, "<null>\n");
								}
								return (NULL);
							}
						}
					}
				}
				add_to_work (access, vms_dir_end);	/* Add a ']' to close the directory part */
				break;
			}
		case other_style:
			assert (false);
		}
	}

	if (*ch != fil_flag) {
		if (access->debug) {
			fprintf (stderr, "<null>\n");
		}
		return (NULL);
	}
	ch += 1;
	ch = add_upto (access, ch, ext_flag);
	if (ch == NULL) {
		if (access->debug) {
			fprintf (stderr, "<null>\n");
		}
		return (NULL);
	}
	ch = add_upto (access, ch, end_flag);
	if (ch == NULL) {
		if (access->debug) {
			fprintf (stderr, "<null>\n");
		}
		return (NULL);
	}
	if (*ch != NUL) {
		if (access->debug) {
			fprintf (stderr, "<null>\n");
		}
		return (NULL);
	}
	add_to_work (access, NUL);
	{
		const char *result = copy_wk (access);
		if (access->debug) {
			fprintf (stderr, "\"%s\"\n", result);
		}
		return result;
	}
}

/* fni_basename:                                                          */
/* Extracts the name of the file, without directory paths or extension.   */
/* It operates on the internal form only.                                 */
/* If there is no filename, then a pointer to an empty string is returned.*/
/* This function returns NULL only if the input is badly formed.          */
/* It is the user's responsibility to free the space obtained for the     */
/* result.                                                                */

char const *fni_basename (fni_access access, fni_canonical const cfn)
{
	char const *filpart;

	if (cfn == NULL) {
		return (NULL);
	}
	access->wk_next = 0;
	filpart = strchr ((char const *) cfn, fil_flag);
	if (filpart == NULL) {
		return (NULL);
	}
	filpart += 1;
	add_upto (access, filpart, ext_flag);
	add_to_work (access, NUL);

	return (copy_wk (access));
}

/* fni_extension:                                                         */
/* Extracts the extension from a canonical filename and returns it in a   */
/* character string that has been obtained for the purpose.               */
/* If there is no extension, then NULL is returned.                       */
/* If the extension is nempty (ie the filename ends with a '.', then an   */
/* empty string ("") is returned.                                         */

char const *fni_extension (fni_access access, fni_canonical const cfn)
{
	char const *extpart;

	if (cfn == NULL) {
		return (NULL);
	}
	access->wk_next = 0;
	extpart = strchr ((char const *) cfn, ext_flag);
	if (extpart == NULL || extpart[1] != '.') {
		return (NULL);
	}
	extpart += 2;		/* omit leading period too */
	add_upto (access, extpart, end_flag);
	add_to_work (access, NUL);

	return (copy_wk (access));
}

/* fni_switch_extension:                                                  */
/* Change the extension on a canonical filename. It will always succeed   */
/* and will return a new string with the new name.                        */
/* An empty extension string ('') is acceptable and will result in a name */
/* ending with a '.'. If the extension is NULL, then the extension will   */
/* be removed.                                                            */

fni_canonical fni_switch_extension (fni_access access, fni_canonical const cfn, char const *const ext)
{
	if (cfn == NULL) {
		return (NULL);
	}
	access->wk_next = 0;
	add_upto (access, cfn, ext_flag);
	add_to_work (access, ext_flag);
	if (ext != NULL) {
		add_to_work (access, dot_flag);
		add_upto (access, ext, NUL);
	}
	add_to_work (access, end_flag);
	add_to_work (access, NUL);

	return ((fni_canonical) copy_wk (access));
}

/* fni_add_extension:                                                     */
/* Add an extension if there is no extension already on the filename.     */
/* The extension can be empty (""). If it is NULL, then the name returned */
/* will be the same. In all cases, new memory is allocated.               */

fni_canonical fni_add_extension (fni_access access, fni_canonical const cfn, char const *const ext)
{
	char const *extpart;

	if (cfn == NULL) {
		return (NULL);
	}
	access->wk_next = 0;
	extpart = add_upto (access, cfn, ext_flag);
	add_to_work (access, ext_flag);
	if ((ext == NULL) || (extpart[0] == dot_flag)) {
		/* No extension to add, or there is already an extension on 'cfn' */
		add_upto (access, extpart, NUL);
	} else {
		add_to_work (access, dot_flag);
		add_upto (access, ext, NUL);
		add_to_work (access, end_flag);
	}
	add_to_work (access, NUL);
	return ((fni_canonical) copy_wk (access));
}

/* is_file:                                                               */
/* Determines whether a canonical file name is for a file or directory.   */
/* Returns true if a file, false otherwise.                               */

static bool is_file (fni_canonical const cfn)
{
	char const *f;

	f = strchr ((char const *) cfn, fil_flag);
	assert (f != NULL);
	return ((*(f + 1) != ext_flag) || (*(f + 2) != end_flag));
}

/* is_separator:                                                          */
/* Checks to see whether a character is a valid separator in the search   */
/* list. Valid separators are: whitespace (as defined by C's isspace) and */
/* semicolons. Any number of these can occur between path names.          */
static bool is_separator (char const c)
{
#ifdef WIN32
	return (c == semi_colon);
#else
	return (c == full_colon);
#endif
}

/* fni_add_dirs_to_fni_list:                                              */
/* Receives a character string containing a list of names of directories. */
/* These are examined for syntactic accuracy for the correct host and     */
/* then appended to the list of canonical file names provided.            */
/* The filenames are separated by whitespace or semicolons in the input   */
/* string.                                                                */
/* For each file name which is syntactically invalid, a function parameter*/
/* is invoked passing the erroneous string.                               */
/* NULL represents an empty list.                                         */

fni_list fni_add_dirs_to_list (fni_access access, fni_list const old_list, char const *const input_str, void (*const error_handler) (char const *const))
{
	char *str, *scanner;
	char const *inscan;
	fni_list_entry *front, *back, *new_one;
	fni_canonical cfn;

	front = (fni_list_entry *) old_list;
	if (front != NULL) {
		for (back = front; back->cl_next != NULL; back = back->cl_next);
	} else {
		back = front;
	}
	/* Add 2 to the length of the input string to get size of str:
	   one for the nul-terminator, and another in case we want to add a
	   trailing directory separator. */
	str = (char *) get_mem (access, (strlen (input_str) + 2) * sizeof (char));
	inscan = input_str;
	while (is_separator (*inscan)) {
		inscan += 1;
	}
	while (*inscan != NUL) {
		scanner = str;
		while ((!is_separator (*inscan)) && (*inscan != NUL)) {
			*scanner = *inscan;
			scanner += 1;
			inscan += 1;
		}
		if (access->style == host_style) {
			access->style = get_host_style ();
		}

		/* Add a trailing directory separator if not already present */
		switch (access->style) {
		case unix_style:
			{
				if (*(scanner - 1) != unix_dir) {
					*scanner = unix_dir;
					scanner += 1;
				}
				break;
			}
		case msdos_style:
			{
				if (*(scanner - 1) != msdos_dir) {
					*scanner = msdos_dir;
					scanner += 1;
				}
				break;
			}
		case vms_style:
			{
				break;
			}
		default:
			{
				assert (false);
			}
		}
		*scanner = NUL;
		cfn = fni_internalise (access, (char const *) str, fni_user);
		if ((cfn == NULL) || is_file (cfn)) {
			error_handler (str);
		} else {
			new_one = (fni_list_entry *) get_mem (access, sizeof (fni_list_entry));
			new_one->cl_cfn = cfn;
			new_one->cl_next = NULL;
			if (front == NULL) {
				front = new_one;
			} else {
				back->cl_next = new_one;
			}
			back = new_one;
		}
		while (is_separator (*inscan)) {
			inscan += 1;
		}
	}
	free_mem (access, str);
	return (front);
}

/* fni_free_list:                                                         */
/* Recovers the space used by a list of canonical file names.             */
/* Note that if any of these are to be saved, it is the user's            */
/* responsibility to do so before freeing the space.                      */

void fni_free_list (fni_access access, fni_list const cl)
{
	fni_list cur, nxt;

	for (cur = cl; cur != NULL; cur = nxt) {
		nxt = cur->cl_next;
		free_mem (access, (void *) (cur->cl_cfn));
		free_mem (access, cur);
	}
	return;
}

/* fni_system_handle:                                                     */
/* Extracts the system file handle from the fni_handle.                   */

FILE *fni_system_handle (fni_access access, fni_handle const fh)
{
	return (fh->fd_handle);
}

/* fni_position:                                                          */
/* Extracts the position of the file in the filesystem from the           */
/* fni_handle.                                                            */
/* It is the user's responsibility to free the space obtained for the     */
/* result.                                                                */

fni_canonical fni_position (fni_access access, fni_handle const fh)
{
	char *res;

	res = (char *) get_mem (access, (strlen ((char *) fh->fd_position) + 1) * sizeof (char));
	strcpy (res, (char *) fh->fd_position);

	return (res);
}


/* is_absolute:                                                           */
/* Tests a filename for relative or absolute type.                        */

static bool is_absolute (fni_canonical const cfn)
{
	char *str, *dir;

	str = (char *) cfn;
	dir = strchr (str, dir_flag);
	if (dir == NULL) {
		return (false);
	}
	if (dir[1] == dir_flag)	{ /* adjacent directory markers mean root     */
		return (true);
	}
	return (false);
}

/* combine:                                                               */
/* Creates a canonical form from the directory part of one filename       */
/* and the second filename. If the second is an absolute name, then       */
/* the combination is the same as the second name.                        */

static fni_canonical combine (fni_access access, fni_canonical const pos, fni_canonical const fil)
{
	char *res;

	if (is_absolute (fil) || (pos == NULL)) {
		res = (char *) get_mem (access, (strlen ((char *) fil) + 1) * sizeof (char));
		strcpy (res, (char *) fil);
		return (res);
	}
	access->wk_next = 0;
	add_upto (access, (char *) pos, fil_flag);
	res = strchr ((char *) fil, dir_flag);
	if (res != NULL) {
		res += 1;
		add_upto (access, res, end_flag);
	}
	add_to_work (access, end_flag);
	add_to_work (access, NUL);

	return ((fni_canonical) copy_wk (access));
}

/* file_type:                                                             */
/* Gets the type of the filename presented to it.                         */

static fni_file_type file_type (fni_canonical const cfn)
{
	char *str;

	str = (char *) cfn;
	str = strchr (str, cfn_flag);
	if (str == NULL) {
		return (fni_none);
	}
	if (str[1] == usr_flag) {
		return (fni_user);
	}
	if (str[1] == sys_flag) {
		return (fni_system);
	}
	return (fni_none);
}

/* try_open:                                                              */
/* Tries to open a file in a given position for a given access.           */

static fni_handle try_open (fni_access access, fni_canonical const pos, fni_canonical const fil, char const *const file_access)
{
	char const *ext;
	fni_canonical cfn;
	FILE *f;
	fni_handle fh;

	cfn = combine (access, pos, fil);
	if (cfn == NULL) {
		return (NULL);
	}
	ext = fni_externalise (access, cfn);
	if (ext == NULL) {
		return (NULL);
	}

	if (access->debug) {
		fprintf (stderr, "Try opening \"%s\" ... ", ext);
	}
	f = fopen (ext, file_access);
	free_mem (access, (void *) ext);
	if (f == NULL) {
		if (access->debug) {
			fprintf (stderr, " failed\n");
		}
		free_mem (access, (void *) cfn);
		fh = NULL;
	} else {
		if (access->debug) {
			fprintf (stderr, " succeeded\n");
		}
		fh = (fni_handle) get_mem (access, sizeof (file_data));
		fh->fd_handle = f;
		fh->fd_position = cfn;
	}
	return (fh);
}

/* try_list:                                                              */
/* Try opening a file with each member of a list as prefix, until it is   */
/* successful, or the end is reached.                                     */

static fni_handle try_list (fni_access access, fni_list const list, fni_canonical const fil, char const *const file_access)
{
	fni_list cur;
	fni_handle fh;

	for (cur = list; cur != NULL; cur = cur->cl_next) {
		fh = try_open (access, cur->cl_cfn, fil, file_access);
		if (fh != NULL) {
			return (fh);
		}
	}
	return (NULL);
}

/* fni_open:                                                              */
/* When a file is opened, this function looks through the current         */
/* location and lists of canonical names as defined in SW-260.            */
/* If the file cannot be opened, then NULL is returned.                   */
/* It is the user's responsibility to free the space obtained for the     */
/* result.                                                                */

fni_handle fni_open (fni_access access,	/* fnilib access handle   */
	  fni_canonical const fil,	/* file to be opened      */
	  char const *const file_access,
	  /* access method to use   */
	  fni_canonical const pos,	/* position of source     */
	  fni_list const usr,	/* user list              */
	  fni_list const sys,	/* system list            */
	  fni_list const def)
{				/* default list (env.)    */
	fni_handle fh;

	if (file_type (fil) != fni_system) {
		fh = try_open (access, pos, fil, file_access);
		if (fh != NULL) {
			return (fh);
		}
		fh = try_list (access, usr, fil, file_access);
		if (fh != NULL) {
			return (fh);
		}
	}
	fh = try_list (access, sys, fil, file_access);
	if (fh != NULL) {
		return (fh);
	}
	fh = try_list (access, def, fil, file_access);
	if (fh != NULL) {
		return (fh);
	}
	return (NULL);
}

/* fni_close:                                                             */
/* Closing a file will also free the fni_handle. The user should not      */
/* do so.                                                                 */
/* The function returns the value that fclose returned when the close was */
/* performed. The user should check the result to see if errors occurred. */

int fni_close (fni_access access, fni_handle const fh)
{
	int e;

	e = fclose (fh->fd_handle);
	fni_free (access, fh);

	return (e);
}

/* fni_free:                                                              */
/* Free the space used by the fni handle. This function does NOT close    */
/* the file. fni_close should be used if the file is to be closed.        */

void fni_free (fni_access access, fni_handle const fh)
{
	free_mem (access, (void *) (fh->fd_position));
	free_mem (access, fh);
}

/* For the curious, the format of the canonical file name is as follows:  */
/* (^X means a <control-X> character)                                     */
/*    ^A        marker to indicate it is canonical, not external          */
/*  'U' | 'S'   indicates user or system file                             */
/*    ^B        separator                                                 */
/*  device      device name for the filesystem, includes node names       */
/*    ^C        separator                                                 */
/*  dir-list    list of directory names, separated by ^C (not / or \ )    */
/*    ^D        separator                                                 */
/*  filename    name of the file, without extension                       */
/*    ^E        separator                                                 */
/*  extension   file extension                                            */
/*    ^F        terminator                                                */
/*    NUL       end of a C string                                         */

/* In the directory list, the following conventions are used:             */
/*    ^H        means current directory                                   */
/*    ^I        means move up a level                                     */

/* Note that each part is separated from the next part by a different     */
/* separator. When converting from the external form, the following rules */
/* are assumed:                                                           */
/* Any part of the file name may be enmpty.                               */
/* Blanks and control characters are not valid characters in any file name*/
/* Version numbers (VAX/VMS) are stripped off and ignored.                */
/* If the device name is present, then the name will contain a root.      */
/* The extension is separated from the filename by the last dot.          */
