/*
 *	cproc.c - C parts of the occam process library
 *	Copyright (C) 2000 Fred Barnes <frmb2@ukc.ac.uk>
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>

/* structures */
struct _occ_environment {
	char ident[128];
	int ident_size;
	char value[128];
	int value_size;
#ifdef __GNUC__
	} __attribute__ ((packed));
#else
	};
	#warning Not GNU C -- might get structure problems
#endif

typedef struct _occ_environment occ_environment;

#ifdef OCCBUILD_KROC
/* declare clean-up function */
extern void *bsyscalls_set_cleanup (void (*)(void *));
#endif


/*
 *	void r_run_cleanup (void *ptr)
 *	cleanup function if r_run gets terminated
 */
static void r_run_cleanup (void *ptr)
{
	int *mem_ptr = (int *)ptr;
	int chld_status, got_pid;

	if (*mem_ptr == -1) {
		/* not started yet */
		return;
	} else {
		/* stop it.. */
		kill (*mem_ptr, SIGKILL);
		retry_waitpid:
		got_pid = waitpid (*mem_ptr, &chld_status, 0);
		if (got_pid != *mem_ptr) {
			if ((got_pid == -1) && (errno == EINTR)) {
				/* should not happen! */
				goto retry_waitpid;
			}
		}
	}
	return;
}


/*
 *	void r_run (occ_environment *env, int env_size, char *args, int arg0_size, int arg1_size, int kyb_fd, int scr_fd, int err_fd, int *result, int killable)
 *	Runs a command to completion
 */
static __inline__ void r_run (occ_environment *env, int env_size, char *args, int arg0_size, int arg1_size, int kyb_fd, int scr_fd, int err_fd, int *result, int killable)
{
	pid_t chld_pid, got_pid;
	int chld_status;
	char **chld_args;
	int i;
	int *mem_ptr = NULL;

#ifdef OCCBUILD_KROC
	if (killable) {
		mem_ptr = bsyscalls_set_cleanup (NULL);
		*mem_ptr = -1;
		bsyscalls_set_cleanup (r_run_cleanup);
	}
#endif
	signal (SIGCHLD, SIG_IGN);
	switch ((chld_pid = fork())) {
	case -1:
		*result = -1;
		fprintf (stderr, "KRoC: proclib: r_run: unable to fork(): %s\n", strerror(errno));
		break;
	case 0:
		/* child -- cam abuse heap in here :) */
		if (kyb_fd == -1) {
			close (0);
		} else if (kyb_fd != 0) {
			dup2 (kyb_fd, 0);
		}
		if (scr_fd == -1) {
			close (1);
		} else if (scr_fd != 1) {
			dup2 (scr_fd, 1);
		}
		if (err_fd == -1) {
			close (2);
		} else if (err_fd != 2) {
			dup2 (err_fd, 2);
		}
		for (i=0; i<env_size; i++) {
			env[i].ident[env[i].ident_size] = '\0';
			env[i].value[env[i].value_size] = '\0';
			setenv (env[i].ident, env[i].value, 0);
		}
		chld_args = (char **)malloc ((arg0_size + 1) * sizeof (char *));
		if (!chld_args) {
			fprintf (stderr, "KRoC: proclib: r_run: child out of memory\n");
			_exit (1);
		}
		for (i=0; i<arg0_size; i++) {
			chld_args[i] = (char *)malloc (arg1_size + 1);
			if (!chld_args[i]) {
				fprintf (stderr, "KRoC: proclib: r_run: child out of memory\n");
				_exit (1);
			}
			memcpy (chld_args[i], &(args[i * arg1_size]), arg1_size);
			chld_args[i][arg1_size] = '\0';
		}
		chld_args[arg0_size] = NULL;
		execvp (*chld_args, chld_args);
		_exit (1);
		break;
	default:
		/* parent */
		if (killable) {
			*mem_ptr = chld_pid;
		}
		retry_waitpid:
		got_pid = waitpid (chld_pid, &chld_status, 0);
		if (got_pid != chld_pid) {
			if (errno == EINTR) {
				goto retry_waitpid;
			}
			fprintf (stderr, "KRoC: proclib: r_run: waitpid returned %d, expecting %d.  Failing\n", got_pid, chld_pid);
			*result = -1;
		} else {
			if (WIFEXITED (chld_status)) {
				*result = WEXITSTATUS (chld_status);
			} else if (WIFSIGNALED (chld_status)) {
				fprintf (stderr, "KRoC: proclib: r_run: child exited on signal %d\n", WTERMSIG(chld_status));
				*result = -1;
			} else {
				fprintf (stderr, "KRoC: proclib: r_run: child stopped on signal %d\n", WSTOPSIG(chld_status));
				*result = -1;
			}
		}
	}
}


/*
 *	void r_os_getenv (occ_environment *env, char *ident, int ident_size)
 *	gets an environment variable from this OS process
 */
static __inline__ void r_os_getenv (occ_environment *env, char *ident, int ident_size)
{
	int x;
	char *ptr;

	x = (ident_size > 127) ? 127 : ident_size;
	memcpy (env->ident, ident, x);
	env->ident[x] = '\0';
	env->ident_size = x;
	ptr = getenv (env->ident);
	if (!ptr) {
		env->value_size = 0;
	} else {
		x = (strlen (ptr) > 127) ? 127 : strlen (ptr);
		memcpy (env->value, ptr, x);
		env->value[x] = '\0';
		env->value_size = x;
	}
}


/*
 *	void r_os_setenv (occ_environment *env, int env_size)
 *	sets environmental varibles in this OS process
 */
static __inline__ void r_os_setenv (occ_environment *env, int env_size)
{
	int x, isize, vsize;

	for (x=0; x<env_size; x++) {
		if (env[x].ident_size == 0) {
			continue;
		}
		isize = (env[x].ident_size > 127) ? 127 : env[x].ident_size;
		env[x].ident[isize] = '\0';
		vsize = (env[x].value_size > 127) ? 127 : env[x].value_size;
		env[x].value[vsize] = '\0';
		if (!vsize) {
			unsetenv (env[x].ident);
		} else {
			setenv (env[x].ident, env[x].value, 1);
		}
	}
}


/* interface */

void _pl_run (int *w)				{ r_run ((occ_environment *)(w[0]), (int)(w[1]), (char *)(w[2]), (int)(w[3]), (int)(w[4]),
							(int)(w[5]), (int)(w[6]), (int)(w[7]), (int *)(w[8]), 0); }
void _pl_run2 (int *w)				{ r_run ((occ_environment *)(w[0]), (int)(w[1]), (char *)(w[2]), (int)(w[3]), (int)(w[4]),
							(int)(w[5]), (int)(w[6]), (int)(w[7]), (int *)(w[8]), 1); }
void _pl_os_getenv (int *w)			{ r_os_getenv ((occ_environment *)(w[0]), (char *)(w[1]), (int)(w[2])); }
void _pl_os_setenv (int *w)			{ r_os_setenv ((occ_environment *)(w[0]), (int)(w[1])); }


