/*
 *	mkoccdeps.c -- create Makefile dependencies for occam sources
 *	Copyright (C) 2004 Fred Barnes <frmb@kent.ac.uk>
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifndef VERSION
#define VERSION "(none)"
#endif

/*{{{  includes*/
#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#include "global.h"
#include "support.h"
/*}}}*/

/*{{{  local and global vars*/
#define FOLLOW_NONE 0
#define FOLLOW_ALL 1
#define FOLLOW_LOCAL 2

static struct {
	int follow;
	int verbose;
	char *outfile;
	int allobjs;
	int localonly;
	DYNARRAY(char *, ipath);
} options = {
	.follow = FOLLOW_NONE,
	.verbose = 0,
	.outfile = NULL,
	.allobjs = 0,
	.localonly = 0
};

#define FTYPE_UNKNOWN 0
#define FTYPE_SOURCE 1
#define FTYPE_INCLUDE 2
#define FTYPE_LIBRARY 3
#define FTYPE_BINARY 4

typedef struct TAG_dfile_t {
	char *name;
	int ftype;
	int skip;
	int hidden;
	int given;
	DYNSARRAY(TAG_dfile_t*, deps);
} dfile_t;

STATICDYNARRAY(dfile_t *, files);


static char *argvzero;
char *progname;
/*}}}*/


/*{{{  void mkoccdeps_fatal (char *fmt, ...)*/
/*
 *	fatal error handler
 */
void mkoccdeps_fatal (char *fmt, ...)
{
	va_list ap;

	va_start (ap, fmt);
	fprintf (stderr, "%s: fatal: ", progname);
	vfprintf (stderr, fmt, ap);
	fprintf (stderr, "\n");
	fflush (stderr);
	va_end (ap);

	exit (EXIT_FAILURE);
}
/*}}}*/
/*{{{  static void usage (FILE *stream)*/
/*
 *	print usage info
 */
static void usage (FILE *stream)
{
	fprintf (stream, "%s %s: occam dependency generator\n", progname, VERSION);
	fprintf (stream, "Copyright (C) 2004 Fred Barnes <frmb@kent.ac.uk>.  GPL >= 2\n\n");
	fprintf (stream, "usage: %s [options] [--] <file1> [file2 [...]]\n", argvzero);
	fprintf (stream, "where options are:\n");
	fprintf (stream, "    -h | --help             print this help and exit\n");
	fprintf (stream, "    -V | --version          print version and exit\n");
	fprintf (stream, "    -v | --verbose          verbose operation\n");
	fprintf (stream, "    -o | --output  <file>   output to <file> (default stdout)\n");
	fprintf (stream, "    -f | --follow           follow referenced files\n");
	fprintf (stream, "    -F | --follow-local     only follow files referenced in the current directory\n");
	fprintf (stream, "    -a | --allobjs          output ALLOBJS entry\n");
	fprintf (stream, "    -A | --allotherobjs     output ALLOBJS entry with all object files\n");
	fprintf (stream, "                            except those given (only makes sense with -f)\n");
	fprintf (stream, "    -I | --include  <path>  search <path> for occam libraries and includes\n");
	fprintf (stream, "    -l | --local-only       only output dependencies for files in the current directory\n");

	return;
}
/*}}}*/
/*{{{  static void version (FILE *stream)*/
/*
 *	print version info
 */
static void version (FILE *stream)
{
	fprintf (stream, "%s %s\n", progname, VERSION);
	return;
}
/*}}}*/

/*{{{  static int guess_ftype (char *fname)*/
/*
 *	guess at file-type based on extension
 */
static int guess_ftype (char *fname)
{
	char *ch;
	int ftype = FTYPE_UNKNOWN;

	if (!fname || !strlen (fname)) {
		mkoccdeps_fatal ("bad file-name");
	}
	for (ch = fname + (strlen (fname) - 1); (ch > fname) && (ch[-1] != '.') && (ch[-1] != '/'); ch--);
	if (ch == fname) {
		ftype = FTYPE_BINARY;
	} else if (!strcmp (ch - 1, ".occ")) {
		ftype = FTYPE_SOURCE;
	} else if (!strcmp (ch - 1, ".inc")) {
		ftype = FTYPE_INCLUDE;
	} else if (!strcmp (ch - 1, ".lib")) {
		ftype = FTYPE_LIBRARY;
	} else {
		ftype = FTYPE_BINARY;
	}

	return ftype;
}
/*}}}*/
/*{{{  static char *find_object_file (char *fname)*/
/*
 *	searches the "include path" for something that produces "fname"
 */
static char *find_object_file (char *fname)
{
	char *tmpname, *ch, *rpath;
	int i;

	if (!fname || !strlen (fname)) {
		return NULL;
	}
	rpath = NULL;

	/* find extension and replace with .occ */
	tmpname = (char *)smalloc (strlen (fname) + 8);
	memcpy (tmpname, fname, strlen (fname) + 1);
	for (ch = tmpname + (strlen (tmpname) - 1); (ch > tmpname) && (*ch != '.'); ch--);
	if (ch == tmpname) {
		sprintf (tmpname, "%s.occ", fname);
		ch = tmpname + strlen (fname);
	} else {
		sprintf (ch, ".occ");
	}

	/* always search here first */
	if (!access (tmpname, R_OK)) {
		/* this source exists here */
		rpath = string_dup (tmpname);
	}
	/* iterate over include directories */
	for (i=0; !rpath && (i < DA_CUR (options.ipath)); i++) {
		char *ipath = DA_NTHITEM (options.ipath, i);

		rpath = (char *)smalloc (strlen (ipath) + strlen (tmpname) + 4);
		if (!strcmp (ipath, "/")) {
			sprintf (rpath, "/%s", tmpname);
		} else {
			sprintf (rpath, "%s/%s", ipath, tmpname);
		}

		if (access (rpath, R_OK)) {
			sfree (rpath);
			rpath = NULL;
		}
	}

	if (!rpath) {
		/* doesn't seem to exist */
		fprintf (stderr, "%s: warning: cannot find source for %s\n", progname, fname);
	} else {
		/* extension must be .occ, set back to .o */
		rpath[strlen (rpath) - 2] = '\0';
	}

	sfree (tmpname);
	return rpath;
}
/*}}}*/
/*{{{  static char *find_include_file (char *fname)*/
/*
 *	searches the "include path" for exactly fname
 */
static char *find_include_file (char *fname)
{
	char *ch, *rpath;
	int i;

	if (!fname || !strlen (fname)) {
		return NULL;
	}
	rpath = NULL;

	/* always search here first */
	if (!access (fname, R_OK)) {
		/* this source exists here */
		rpath = string_dup (fname);
	}
	/* iterate over include directories */
	for (i=0; !rpath && (i < DA_CUR (options.ipath)); i++) {
		char *ipath = DA_NTHITEM (options.ipath, i);

		rpath = (char *)smalloc (strlen (ipath) + strlen (fname) + 4);
		if (!strcmp (ipath, "/")) {
			sprintf (rpath, "/%s", fname);
		} else {
			sprintf (rpath, "%s/%s", ipath, fname);
		}

		if (access (rpath, R_OK)) {
			sfree (rpath);
			rpath = NULL;
		}
	}

	if (!rpath) {
		/* doesn't seem to exist */
		fprintf (stderr, "%s: warning: cannot find %s\n", progname, fname);
	}

	return rpath;
}
/*}}}*/
/*{{{  static int process_file (dfile_t *file)*/
/*
 *	processes the given file.
 *	return 0 on success, -1 on error
 */
static int process_file (dfile_t *file)
{
	if (file->skip) {
		return 0;
	}
	if (options.verbose) {
		fprintf (stderr, "processing %s...\n", file->name);
	}
	if ((file->ftype == FTYPE_SOURCE) || (file->ftype == FTYPE_INCLUDE)) {
		FILE *fp;
		char buf[512];
		int curline = 0;

		if (!(fp = fopen (file->name, "rt"))) {
			/* not a problem if it's an include file */
			if (file->ftype == FTYPE_INCLUDE) {
				file->hidden = 1;
				return 0;
			}
			return -1;
		}
		while (fgets (buf, 511, fp)) {
			char *ch, *dh;
			int include = -1;

			curline++;
			/* skip leading whitespace */
			for (ch = buf; (*ch != '\0') && ((*ch == ' ') || (*ch == '\t')); ch++);
			if (*ch != '#') {
				/* uninteresting */
				continue;
			}
			if (!strncmp (ch, "#USE", 4)) {
				include = 0;
				dh = ch + 4;
			} else if (!strncmp (ch, "#INCLUDE", 8)) {
				include = 1;
				dh = ch + 8;
			} else {
				dh = NULL;
			}
			if (dh) {
				/* attempt to extract string portion */
				char *eh;
				dfile_t *tmp = NULL;
				int ttype;

				for (; (*dh == ' ') || (*dh == '\t'); dh++);
				if (*dh != '\"') {
					/* wierd */
					fprintf (stderr, "%s:%d: badly formed directive\n", file->name, curline);
					continue;
				}
				dh++;

				for (eh=dh; (*eh != '\0') && (*eh != '\"'); eh++);
				if (*eh != '\"') {
					fprintf (stderr, "%s:%s: badly formed directive\n", file->name, curline);
					continue;
				}
				*eh = '\0';
				/* dh now has the USE'd or INCLUDE'd name, create an entry for it */
				ttype = guess_ftype (dh);
				if ((ttype == FTYPE_BINARY) && !include) {
					/* depends on an object-file */
					char *tmpname = (char *)smalloc (strlen (dh) + 8);
					char *tn2;
					dfile_t *tmp2 = NULL;
					int i;
					
					sprintf (tmpname, "%s.o", dh);
					/* search for it */
					tn2 = find_object_file (tmpname);
					if (tn2) {
						sfree (tmpname);
						tmpname = tn2;
					}
					for (i = 0; i < DA_CUR (files); i++) {
						if (!strcmp ((DA_NTHITEM (files, i))->name, tmpname)) {
							break;
						}
					}
					if (i == DA_CUR (files)) {
						/* not here, create an entry */
						tmp = (dfile_t *)smalloc (sizeof (dfile_t));
						tmp->name = string_dup (tmpname);
						tmp->ftype = ttype;
						tmp->skip = 1;				/* no point for an object-file */
						tmp->hidden = 0;
						tmp->given = 0;
						dynarray_init (tmp->deps);
						dynarray_add (files, tmp);
					} else {
						tmp = DA_NTHITEM (files, i);
					}

					/* search for .occ source and add to list */
					sprintf (tmpname, "%s.occ", dh);
					tn2 = find_include_file (tmpname);
					if (tn2) {
						sfree (tmpname);
						tmpname = tn2;
					}
					for (i = 0; i < DA_CUR (files); i++) {
						if (!strcmp ((DA_NTHITEM (files, i))->name, tmpname)) {
							break;
						}
					}
					if (i == DA_CUR (files)) {
						/* not here, create an entry */
						tmp2 = (dfile_t *)smalloc (sizeof (dfile_t));
						tmp2->name = string_dup (tmpname);
						tmp2->ftype = FTYPE_SOURCE;
						tmp2->hidden = 0;
						switch (options.follow) {
						case FOLLOW_NONE:
							tmp2->skip = 1;
							break;
						case FOLLOW_ALL:
							tmp2->skip = 0;
							break;
						case FOLLOW_LOCAL:
							tmp2->skip = ((strchr (tmp2->name, '/')) != NULL);
							break;
						}
						tmp2->given = 0;
						dynarray_init (tmp2->deps);
						dynarray_add (files, tmp2);
					} else {
						tmp2 = DA_NTHITEM (files, i);
					}
					/* object depends on the source */
					dynarray_maybeadd (tmp->deps, tmp2);

					if (tmpname) {
						sfree (tmpname);
						tmpname = NULL;
					}
				} else if ((ttype == FTYPE_INCLUDE) && include) {
					/* depends on include file, search for it */
					int i;
					char *tmpname, *tn2;
					int nonlocal = 0;

					tmpname = string_dup (dh);
					tn2 = find_include_file (tmpname);
					if (tn2) {
						sfree (tmpname);
						tmpname = tn2;
					} else {
						nonlocal = 1;
					}
					for (i = 0; i < DA_CUR (files); i++) {
						if (!strcmp ((DA_NTHITEM (files, i))->name, tmpname)) {
							break;
						}
					}
					if (i == DA_CUR (files)) {
						/* not here, create an entry */
						tmp = (dfile_t *)smalloc (sizeof (dfile_t));
						tmp->name = string_dup (tmpname);
						tmp->ftype = ttype;
						switch (options.follow) {
						case FOLLOW_NONE:
							tmp->skip = 1;
							break;
						case FOLLOW_ALL:
							tmp->skip = 0;
							break;
						case FOLLOW_LOCAL:
							tmp->skip = ((strchr (tmp->name, '/')) != NULL);
							break;
						}
						tmp->hidden = nonlocal;
						tmp->given = 0;
						dynarray_init (tmp->deps);
						dynarray_add (files, tmp);
					} else {
						tmp = DA_NTHITEM (files, i);
					}

					if (tmpname) {
						sfree (tmpname);
						tmpname = NULL;
					}
				}

				/* if tmp is set, link it (if not already a dependency) */
				if (tmp) {
					dynarray_maybeadd (file->deps, tmp);
				}
			}
		}
		fclose (fp);

		if (file->ftype == FTYPE_SOURCE) {
			/* convert into object-file dependencies */
			int i;
			char *tmpname = string_dup (file->name);
			char *ch = tmpname + (strlen (tmpname) - 4);
			dfile_t *tmp;

			if ((ch < tmpname) || strcmp (ch, ".occ")) {
				fprintf (stderr, "%s: warning: strange looking source file: %s\n", progname, file->name);
			} else {
				sprintf (ch, ".o");

				/* search for it */
				for (i = 0; i < DA_CUR (files); i++) {
					if (!strcmp ((DA_NTHITEM (files, i))->name, tmpname)) {
						break;
					}
				}
				if (i == DA_CUR (files)) {
					/* create new entry for object-file */
					tmp = (dfile_t *)smalloc (sizeof (dfile_t));
					tmp->name = tmpname;
					tmp->ftype = FTYPE_BINARY;
					tmp->skip = 1;
					tmp->hidden = 0;
					tmp->given = file->given;
					dynarray_init (tmp->deps);
					dynarray_add (files, tmp);
				} else {
					tmp = DA_NTHITEM (files, i);
				}

				/* move dependencies from file->deps to tmp->deps */
				for (i = 0; i < DA_CUR (file->deps); i++) {
					dfile_t *ditem = DA_NTHITEM (file->deps, i);

					dynarray_maybeadd (tmp->deps, ditem);
				}
				dynarray_trash (file->deps);

				/* and add the new dependency */
				dynarray_maybeadd (tmp->deps, file);
			}
		}
	}
	return 0;
}
/*}}}*/
/*{{{  static void write_deps (dfile_t *file, FILE *stream)*/
/*
 *	outputs dependency info for a file
 */
static void write_deps (dfile_t *file, FILE *stream)
{
	int i;

	if (!file) {
		return;
	}
	if (!DA_CUR (file->deps)) {
		/* no dependencies */
		return;
	}
	if (file->hidden) {
		/* not our problem */
		return;
	}

	if (options.localonly && strchr (file->name, '/')) {
		/* elsewhere */
		return;
	}
	for (i = 0; i < DA_CUR (file->deps); i++) {
		dfile_t *dep = DA_NTHITEM (file->deps, i);

		if (!dep->hidden) {
			fprintf (stream, "%s: %s\n", file->name, dep->name);
		}
	}
	return;
}
/*}}}*/
/*{{{  static void write_allobjs (FILE *stream)*/
/*
 *	outputs an ALLOBJS= ... for Makefiles
 */
static void write_allobjs (FILE *stream)
{
	int i;

	fprintf (stream, "ALLOBJS=");
	for (i = 0; i < DA_CUR (files); i++) {
		dfile_t *file = DA_NTHITEM (files, i);

		if ((file->ftype == FTYPE_BINARY) && !file->hidden) {
			if ((options.allobjs == 2) && !file->given) {
				fprintf (stream, "%s ", file->name);
			} else if (options.allobjs == 1) {
				fprintf (stream, "%s ", file->name);
			}
		}
	}
	fprintf (stream, "\n");
	return;
}
/*}}}*/

/*{{{  int main (int argc, char **argv)*/
/*
 *	start here
 */
int main (int argc, char **argv)
{
	int i;
	char **walk;
	int eoo = 0;

	argvzero = *argv;
	for (progname = *argv + (strlen (*argv) - 1); (progname > *argv) && (progname[-1] != '/'); progname--);
	dynarray_init (files);
	dynarray_init (options.ipath);

	/* process arguments */
	for (walk = argv + 1, i = argc; *walk && i; walk++, i--) {
		if (!eoo && (!strcmp (*walk, "-h") || !strcmp (*walk, "--help"))) {
			usage (stdout);
			exit (EXIT_SUCCESS);
		} else if (!eoo && (!strcmp (*walk, "-V") || !strcmp (*walk, "--version"))) {
			version (stdout);
			exit (EXIT_SUCCESS);
		} else if (!eoo && (!strcmp (*walk, "-v") || !strcmp (*walk, "--verbose"))) {
			options.verbose = 1;
		} else if (!eoo && (!strcmp (*walk, "-f") || !strcmp (*walk, "--follow"))) {
			options.follow = FOLLOW_ALL;
		} else if (!eoo && (!strcmp (*walk, "-F") || !strcmp (*walk, "--follow-local"))) {
			options.follow = FOLLOW_LOCAL;
		} else if (!eoo && (!strcmp (*walk, "-a") || !strcmp (*walk, "--allobjs"))) {
			options.allobjs = 1;
		} else if (!eoo && (!strcmp (*walk, "-A") || !strcmp (*walk, "--allotherobjs"))) {
			options.allobjs = 2;
		} else if (!eoo && (!strcmp (*walk, "-o") || !strcmp (*walk, "--output"))) {
			walk++, i--;
			if (!*walk || !i) {
				mkoccdeps_fatal ("option %s requires an argument", walk[-1]);
			}
			if (!strcmp (*walk, "-")) {
				options.outfile = NULL;		/* use stdout */
			} else {
				options.outfile = string_dup (*walk);
			}
		} else if (!eoo && (!strcmp (*walk, "-l") || !strcmp (*walk, "--local-only"))) {
			options.localonly = 1;
		} else if (!eoo && (!strncmp (*walk, "-I", 2) || !strcmp (*walk, "--include"))) {
			char *opt;

			/* option might be bolted on if -I */
			if (!strncmp (*walk, "-I", 2) && strcmp (*walk, "-I")) {
				opt = string_dup (*walk + 2);
			} else {
				walk++, i--;
				if (!*walk || !i) {
					mkoccdeps_fatal ("option %s requires an argument", walk[-1]);
				}
				opt = string_dup (*walk);
			}
			if ((opt[strlen(opt) - 1] == '/') && (strlen(opt) > 1)) {
				/* remove trailing / */
				opt[strlen(opt) - 1] = '\0';
			}
			dynarray_add (options.ipath, opt);
		} else if (!eoo && !strcmp (*walk, "--")) {
			eoo = 1;
		} else if (!eoo && (**walk == '-')) {
			mkoccdeps_fatal ("unrecognised option: %s", *walk);
		} else {
			/* assume filename */
			int j;

			for (j = 0; j < DA_CUR (files); j++) {
				if (!strcmp ((DA_NTHITEM (files, j))->name, *walk)) {
					/* already got this one */
					break;
				}
			}
			if (j == DA_CUR (files)) {
				struct stat stbuf;
				dfile_t *tmp;

				/* make sure it's a real file first */
				if (stat (*walk, &stbuf) < 0) {
					mkoccdeps_fatal ("unable to stat file: %s", *walk);
				}
				if (!S_ISREG (stbuf.st_mode)) {
					mkoccdeps_fatal ("%s is not a regular file", *walk);
				}
				tmp = (dfile_t *)smalloc (sizeof (dfile_t));
				tmp->name = string_dup (*walk);
				tmp->ftype = guess_ftype (tmp->name);
				tmp->skip = 0;
				tmp->hidden = 0;
				tmp->given = 1;
				dynarray_init (tmp->deps);

				dynarray_add (files, tmp);
			}
		}
	}

	/* check for at least one file */
	if (!DA_CUR (files)) {
		mkoccdeps_fatal ("no input files");
	}

	/* process */
	for (i = 0; i < DA_CUR (files); i++) {
		dfile_t *fitem = DA_NTHITEM (files, i);

		if (process_file (fitem) < 0) {
			mkoccdeps_fatal ("error processing %s", fitem->name);
		}
	}

	/* output */
	{
		FILE *outfile;

		if (!options.outfile) {
			outfile = stdout;
		} else {
			outfile = fopen (options.outfile, "wt");
			if (!outfile) {
				mkoccdeps_fatal ("unable to open %s for output", options.outfile);
			}
		}
		for (i = 0; i < DA_CUR (files); i++) {
			dfile_t *fitem = DA_NTHITEM (files, i);

			write_deps (fitem, outfile);
		}
		if (options.allobjs) {
			write_allobjs (outfile);
		}
		if (options.outfile) {
			fclose (outfile);
		}
	}

	return 0;
}
/*}}}*/

