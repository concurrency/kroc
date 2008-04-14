/*
 *	elf.c - ELF object file writer
 *	Copyright (C) 2000 Fred Barnes <frmb@kent.ac.uk>
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

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifndef WIN32
#include <sys/wait.h>
#endif

#include "main.h"
#include "support.h"
#include "structs.h"
#include "transputer.h"
#include "trancomm.h"
#include "tstack.h"
#include "postmortem.h"
#include "intel.h"
#include "archdef.h"


#ifndef EXIT_FAILURE
	#define EXIT_FAILURE 1
#endif

/* don't want to include stdlib.h as it doesn't exist on some Linux installations */
extern char *getenv(const char *name);

#ifdef GNUASFLAGS
 #define GNUASFLAGSADD GNUASFLAGS, 
#else
 #define GNUASFLAGSADD
#endif


/*{{{  int dump_elf (rtl_chain *rtl_code, char *sourcefile, char *filename, arch_t *arch)*/
/*
 *	dumps to an ELF object file
 *	it launches `as' (or $AS) to assemble the code.  uses `arch' to get at ASM coder
 *	this is the Right Way IMHO
 */
int dump_elf (rtl_chain *rtl_code, char *sourcefile, char *filename, arch_t *arch)
{
#ifdef WIN32
	/* Windows doesn't have execlp (or as, for that matter). */
	fprintf (stderr, "%s: ELF output is not supported on Windows\n");
	return -1;
#else
	int status;
	pid_t chld_pid, waited_pid;
	int pipes[2];
	FILE *wfile;
	int d_result;
	char *as_env;
	static char *as_command = "as";

	if (!sourcefile) {
		if (pipe (pipes)) {
			fprintf (stderr, "%s: unable to create pipe: %s\n", progname, strerror (errno));
			return -1;
		}
	} else {
		pipes[0] = -1;
		pipes[1] = -1;
	}
	chld_pid = fork ();
	switch (chld_pid) {
	default:
		/* parent */
		if (!sourcefile) {
			close (pipes[0]);
			wfile = fdopen (pipes[1], "w");
			d_result = arch->code_to_asm_stream (rtl_code, wfile);
		}
		break;
	case 0:
		/* child */
		if (!sourcefile) {
			close (pipes[1]);
			if (dup2 (pipes[0], 0)) {
				exit (EXIT_FAILURE);
			}
		} else {
			close (0);	/* don't need/want standard input for this */
		}
		as_env = getenv ("AS");
		if (!as_env) {
			/* I think this is pointless (unsure).. */
			as_env = getenv ("as");
		}
		if (as_env) {
			if (access (as_env, X_OK)) {
				as_env = NULL;
			}
		}
		if (!as_env) {
			as_env = as_command;
		}
		if (options.gstabs) {
			if (sourcefile) {
				execlp (as_env, as_env, (options.gstabs == 2) ? "--gstabs+" : "--gstabs", GNUASFLAGSADD "-o", filename, sourcefile, NULL);
			} else {
				execlp (as_env, as_env, (options.gstabs == 2) ? "--gstabs+" : "--gstabs", GNUASFLAGSADD "-o", filename, NULL);
			}
		} else {
			if (sourcefile) {
				execlp (as_env, as_env, GNUASFLAGSADD "-o", filename, sourcefile, NULL);
			} else {
				execlp (as_env, as_env, GNUASFLAGSADD "-o", filename, NULL);
			}
		}
		exit (EXIT_FAILURE);
		break;
	case -1:
		/* whoops */
		fprintf (stderr, "%s: unable to fork(): %s\n", progname, strerror (errno));
		return -1;
	}
	if (!sourcefile) {
		fclose (wfile);
		close (pipes[1]);
	}
	waited_pid = waitpid (chld_pid, &status, 0);
	if (waited_pid != chld_pid) {
		fprintf (stderr, "%s: warning: problem catching child: %s\n", progname, strerror (errno));
	}
	if (!WIFEXITED (status)) {
		fprintf (stderr, "%s: error: child process exited abnormally: %s\n", progname, strerror (errno));
		return -1;
	} else {
		if (WEXITSTATUS (status)) {
			fprintf (stderr, "%s: error: failed to assemble code\n", progname);
			return -1;
		}
	}
	return 0;
#endif
}
/*}}}*/

