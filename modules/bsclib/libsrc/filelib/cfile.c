/*
 *	cfile.c -- C parts of the occam file library
 *	Copyright (C) 2000-2003 Fred Barnes <frmb2@ukc.ac.uk>
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
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_SYS_SENDFILE_H
#include <sys/sendfile.h>
#endif
#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#if defined(HAVE_STDINT_H)
#include <stdint.h>
#elif defined(HAVE_INTTYPES_H)
#include <inttypes.h>
#endif

#include <dirent.h>

/*{{{  occam constants */
/* MUST MATCH filelib.inc */
#define OCC_O_RDONLY	0x00000000
#define OCC_O_WRONLY	0x00000001
#define OCC_O_RDWR	0x00000002
#define OCC_O_CREAT	0x00000040
#define OCC_O_EXCL	0x00000080
#define OCC_O_NOCTTY	0x00000100
#define OCC_O_TRUNC	0x00000200
#define OCC_O_APPEND	0x00000400
#define OCC_O_NONBLOCK	0x00000800
#define OCC_O_SYNC	0x00001000
#define OCC_O_MASK	0x00001FC3

#define OCC_SEEK_SET	0
#define OCC_SEEK_CUR	1
#define OCC_SEEK_END	2

#define OCC_F_DUPFD 0
#define OCC_F_GETFD 1
#define OCC_F_SETFD 2
#define OCC_F_GETFL 3
#define OCC_F_SETFL 4
#define OCC_F_GETLK 5
#define OCC_F_SETLK 6
#define OCC_F_SETLKW 7
#define OCC_F_SETOWN 8
#define OCC_F_GETOWN 9
/*}}}*/

/*{{{  flag/option converters */
static int r_encode_open_flags (int flags)
{
	int ret = (flags & (~OCC_O_MASK));

	if (flags & OCC_O_RDWR)
		ret |= O_RDWR;
	else if (flags & OCC_O_WRONLY)
		ret |= O_WRONLY;
	else
		ret |= O_RDONLY;
	
	if (flags & OCC_O_CREAT)
		ret |= O_CREAT;
	if (flags & OCC_O_EXCL)
		ret |= O_EXCL;
	if (flags & OCC_O_TRUNC)
		ret |= O_TRUNC;
	if (flags & OCC_O_APPEND)
		ret |= O_APPEND;
#ifndef HOSTOS_MINGW
	if (flags & OCC_O_NOCTTY)
		ret |= O_NOCTTY;
	if (flags & OCC_O_NONBLOCK)
		ret |= O_NONBLOCK;
	if (flags & OCC_O_SYNC)
		ret |= O_SYNC;
#endif
	
	return ret;
}

static int r_decode_open_flags (int flags)
{
	const int mask = O_RDWR | O_WRONLY | O_RDONLY
		| O_CREAT | O_EXCL | O_TRUNC | O_APPEND
#ifndef HOSTOS_MINGW
		| O_NOCTTY | O_NONBLOCK | O_SYNC
#endif
		;
	int ret = (flags & (~mask));

	if ((flags & O_RDWR) == O_RDWR)
		ret |= OCC_O_RDWR;
	else if ((flags & O_WRONLY) == O_WRONLY)
		ret |= OCC_O_WRONLY;
	else
		ret |= OCC_O_RDONLY;
	
	if ((flags & O_CREAT) == O_CREAT)
		ret |= OCC_O_CREAT;
	if ((flags & O_EXCL) == O_EXCL)
		ret |= OCC_O_EXCL;
	if ((flags & O_TRUNC) == O_TRUNC)
		ret |= OCC_O_TRUNC;
	if ((flags & O_APPEND) == O_APPEND)
		ret |= OCC_O_APPEND;
#ifndef HOSTOS_MINGW
	if ((flags & O_NOCTTY) == O_NOCTTY)
		ret |= OCC_O_NOCTTY;
	if ((flags & O_NONBLOCK) == O_NONBLOCK)
		ret |= OCC_O_NONBLOCK;
	if ((flags & O_SYNC) == O_SYNC)
		ret |= OCC_O_SYNC;
#endif

	return ret;
}

#ifndef HOSTOS_MINGW
static int r_fcntl_cmd (int cmd)
{
	switch (cmd) {
		case OCC_F_DUPFD: cmd = F_DUPFD; break;
		case OCC_F_GETFD: cmd = F_GETFD; break;
		case OCC_F_SETFD: cmd = F_SETFD; break;
		case OCC_F_GETFL: cmd = F_GETFL; break;
		case OCC_F_SETFL: cmd = F_SETFL; break;
		case OCC_F_GETLK: cmd = F_GETLK; break;
		case OCC_F_SETLK: cmd = F_SETLK; break;
		case OCC_F_SETLKW: cmd = F_SETLKW; break;
		case OCC_F_SETOWN: cmd = F_SETOWN; break;
		case OCC_F_GETOWN: cmd = F_GETOWN; break;
	}
	return cmd;
}
#endif
/*}}}*/

/*{{{  static __inline__ void r_check_access (char *fname, int flen, int what, int *result)*/
/*
 *	void r_check_access (char *fname, int flen, int what, int *result)
 *	Checks for file access "rights"
 */
static __inline__ void r_check_access (char *fname, int flen, int what, int *result)
{
	char pbuffer[FILENAME_MAX];
	int x;

	if (flen >= (FILENAME_MAX - 1)) {
		x = FILENAME_MAX - 1;
	} else {
		x = flen;
	}
	memcpy (pbuffer, fname, x);
	pbuffer[x] = '\0';
	*result = access (pbuffer, what);
}
/*}}}*/
/*{{{  static __inline__ void r_size (char *fname, int flen, int *result)*/
/*
 *	void r_size (char *fname, int flen, int *result)
 *	gets the size of a file
 */
static __inline__ void r_size (char *fname, int flen, int *result)
{
	char pbuffer[FILENAME_MAX];
	int x;
	struct stat st_buf;

	if (flen >= (FILENAME_MAX - 1)) {
		x = FILENAME_MAX - 1;
	} else {
		x = flen;
	}
	memcpy (pbuffer, fname, x);
	pbuffer[x] = '\0';
	x = stat (pbuffer, &st_buf);
	if (x < 0) {
		*result = -1;
	} else {
		*result = (int)st_buf.st_size;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_open (char *fname, int flen, int mode, int *fd)*/
/*
 *	void r_open (char *fname, int flen, int mode, int *fd)
 *	opens a file
 */
static __inline__ void r_open (char *fname, int flen, int mode, int *fd)
{
	char pbuffer[FILENAME_MAX];
	int x;

	if (flen >= (FILENAME_MAX - 1)) {
		x = FILENAME_MAX - 1;
	} else {
		x = flen;
	}
	memcpy (pbuffer, fname, x);
	pbuffer[x] = '\0';
	*fd = open (pbuffer, r_encode_open_flags (mode));
}
/*}}}*/
/*{{{  static __inline__ void r_open3 (char *fname, int flen, int mode, int perm, int *fd)*/
/*
 *	void r_open3 (char *fname, int flen, int mode, int perm, int *fd)
 *	opens a file with permission if creating
 */
static __inline__ void r_open3 (char *fname, int flen, int mode, int perm, int *fd)
{
	char pbuffer[FILENAME_MAX];
	int x;

	if (flen >= (FILENAME_MAX - 1)) {
		x = FILENAME_MAX - 1;
	} else {
		x = flen;
	}
	memcpy (pbuffer, fname, x);
	pbuffer[x] = '\0';
	*fd = open (pbuffer, r_encode_open_flags (mode), perm);
}
/*}}}*/
/*{{{  static __inline__ void r_pipe (int *fd_0, int *fd_1, int *result)*/
/*
 *	void r_pipe (int *fd_0, int *fd_1, int *result)
 *	Creates a pair of pipe descriptors
 */
static __inline__ void r_pipe (int *fd_0, int *fd_1, int *result)
{
#ifdef HOSTOS_MINGW
	*result = -1;
#else
	int p[2];

	*result = pipe (p);
	if (!*result) {
		*fd_0 = p[0];
		*fd_1 = p[1];
	}
#endif
}
/*}}}*/
/*{{{  static __inline__ void r_dup2 (int old_fd, int new_fd, int *result)*/
/*
 *	void r_dup2 (int old_fd, int new_fd, int *result)
 *	Duplicates a file descriptor
 */
static __inline__ void r_dup2 (int old_fd, int new_fd, int *result)
{
	*result = dup2 (old_fd, new_fd);
}
/*}}}*/
/*{{{  static __inline__ void r_read (int fd, char *buffer, int bufsize, int *result)*/
/*
 *	r_read (int fd, char *buffer, int bufsize, int *result)
 *	reads from a file
 */
static __inline__ void r_read (int fd, char *buffer, int bufsize, int *result)
{
	*result = read (fd, buffer, bufsize);
}
/*}}}*/
/*{{{  static __inline__ void r_write (int fd, char *buffer, int bufsize, int *result)*/
/*
 *	void r_write (int fd, char *buffer, int bufsize, int *result)
 *	writes to a file
 */
static __inline__ void r_write (int fd, char *buffer, int bufsize, int *result)
{
	*result = write (fd, buffer, bufsize);
}
/*}}}*/
/*{{{  static __inline__ void r_seek (int fd, int offset, int whence, int *result)*/
/*
 *	void r_seek (int fd, int offset, int whence, int *result)
 *	seeks to a specified point in a file
 */
static __inline__ void r_seek (int fd, int offset, int whence, int *result)
{
	switch (offset) {
		case OCC_SEEK_SET: offset = SEEK_SET; break;
		case OCC_SEEK_CUR: offset = SEEK_CUR; break;
		case OCC_SEEK_END: offset = SEEK_END; break;
	}
	
	*result = (int)lseek (fd, offset, whence);
}
/*}}}*/
/*{{{  static __inline__ void r_close (int fd, int *result)*/
/*
 *	void r_close (int fd, int *result)
 *	closes a file
 */
static __inline__ void r_close (int fd, int *result)
{
	*result = close (fd);
}
/*}}}*/
/*{{{  static __inline__ void r_mkdir (char *path, int path_len, int perm, int *result)*/
/*
 *	void r_mkdir (char *path, int path_len, int perm, int *result)
 *	creates a directory
 */
static __inline__ void r_mkdir (char *path, int path_len, int perm, int *result)
{
	char fname[FILENAME_MAX];

	if (FILENAME_MAX <= path_len) {
		path_len = FILENAME_MAX - 1;
	}
	memcpy (fname, path, path_len);
	fname[path_len] = '\0';
#ifdef HOSTOS_MINGW
	*result = mkdir (fname);
#else
	*result = mkdir (fname, perm);
#endif
}
/*}}}*/
/*{{{  static __inline__ void r_rmdir (char *path, int path_len, int *result)*/
/*
 *	void r_rmdir (char *path, int path_len, int *result)
 *	removes a directory
 */
static __inline__ void r_rmdir (char *path, int path_len, int *result)
{
	char fname[FILENAME_MAX];

	if (FILENAME_MAX <= path_len) {
		path_len = FILENAME_MAX - 1;
	}
	memcpy (fname, path, path_len);
	fname[path_len] = '\0';
	*result = rmdir (fname);
}
/*}}}*/
/*{{{  static __inline__ void r_unlink (char *file, int file_len, int *result)*/
/*
 *	void r_unlink (char *file, int file_len, int *result)
 *	removes a file
 */
static __inline__ void r_unlink (char *file, int file_len, int *result)
{
	char fname[FILENAME_MAX];

	if (FILENAME_MAX <= file_len) {
		file_len = FILENAME_MAX - 1;
	}
	memcpy (fname, file, file_len);
	fname[file_len] = '\0';
	*result = unlink (fname);
}
/*}}}*/
/*{{{  static __inline__ void r_fd_fd_copy (int src_fd, int dst_fd, int count, int *result)*/
/*
 *	void r_fd_fd_copy (int src_fd, int dst_fd, int count, int *result)
 *	coppies `count' bytes from `src_fd' to `dst_fd'.  If `count' is 0, data is
 *	coppied until the source EOFs, or an error occurs
 *	`result' gets:	0 if done OK, -1 if src error, -2 if dest error, or -3 if
 *			`count' > 0 and `src_fd' EOFs before it's all been read.
 */
static __inline__ void r_fd_fd_copy (int src_fd, int dst_fd, int count, int *result)
{
	int x, y, z, gone_out, tmp;
	char buffer[1024];

	for (gone_out = 0;;) {
		if (!count) {
			x = 1024;
		} else {
			x = ((count - gone_out) <= 1024) ? (count - gone_out) : 1024;
		}
		y = read (src_fd, buffer, x);
		if (y < 0) {
			/* got error */
			*result = -1;
			return;
		} else if (!y) {
			/* got EOF */
			if (count) {
				*result = -3;
			} else {
				*result = 0;
			}
			return;
		}
		for (z=0; z<y;) {
			tmp = write (dst_fd, buffer, (y - z));
			if (tmp < 0) {
				/* got error */
				*result = -1;
				return;
			}
			z += tmp;
		}
		gone_out += y;
		if (gone_out == count) {
			*result = 0;
			return;
		}
	}
}
/*}}}*/
/*{{{  static __inline__ void r_sendfile (int src_fd, int dst_fd, int count, int *offset, int *result)*/
/*
 *	void r_sendfile (int src_fd, int dst_fd, int count, int *offset, int *result)
 *	uses sendfile(2) to do a descriptor -> descriptor copy
 */
static __inline__ void r_sendfile (int src_fd, int dst_fd, int count, int *offset, int *result)
{
	int ires;

#ifdef HAVE_SYS_SENDFILE_H
	ires = (int)sendfile (dst_fd, src_fd, (off_t *)offset, count);
	if (ires == -1) {
		*result = -errno;
	} else {
		*result = ires;
	}
#else
	/* this isn't entirely compatible with sendfile (no offset), but it gets the job done.. */
	r_fd_fd_copy (src_fd, dst_fd, count, result);
	ires = -1;	/* use to prevent compiler warning */
#endif
}
/*}}}*/

/*{{{  PROC C.fl.fcntl0 (VAL INT fd, cmd, RESULT INT result) */
static __inline__ void r_fcntl0 (int fd, int cmd, int *result)
{
#ifdef HOSTOS_MINGW
	*result = -1;
#else
	if (fd < 0) {
		*result = -1;
	} else {
		*result = fcntl (fd, r_fcntl_cmd (cmd));
	}
#endif
}
void _fl_fcntl0 (int *ws) { r_fcntl0 ((int)(ws[0]), (int)(ws[1]), (int *)(ws[2])); }
/*}}}*/
/*{{{  PROC C.fl.fcntl1 (VAL INT fd, cmd, arg, RESULT INT result) */
static __inline__ void r_fcntl1 (int fd, int cmd, int arg, int *result)
{
#ifdef HOSTOS_MINGW
	*result = -1;
#else
	if (fd < 0) {
		*result = -1;
	} else {
		if (cmd == OCC_F_SETFL)
			arg = r_encode_open_flags (arg);
		*result = fcntl (fd, r_fcntl_cmd (cmd), (long)arg);
		if (cmd == OCC_F_GETFL)
			*result = r_decode_open_flags (*result);
	}
#endif
}
void _fl_fcntl1 (int *ws) { r_fcntl1 ((int)(ws[0]), (int)(ws[1]), (int)(ws[2]), (int *)(ws[3])); }
/*}}}*/
/*{{{  PROC [BC].fl.select ([]INT read.set, write.set, except.set, VAL INT high.fd, timeout, RESULT INT result) */
static __inline__ void r_select (int *rs, int n_rs, int *ws, int n_ws, int *es, int n_es, int high_fd,
		int timeout, int *result)
{
#ifdef HOSTOS_MINGW
	*result = -1;
#else
	struct timeval tv;
	fd_set read_set, write_set, except_set;
	int use_timeout = 0;
	int i;

	if (timeout >= 0) {
		tv.tv_sec = timeout / 1000000;
		tv.tv_usec = timeout % 1000000;
		use_timeout = 1;
	}
	FD_ZERO (&read_set);
	FD_ZERO (&write_set);
	FD_ZERO (&except_set);

	/* set descriptors in sets */
	for (i=0; i<n_rs; i++) {
		if (rs[i] >= 0) {
			FD_SET (rs[i], &read_set);
		}
	}
	for (i=0; i<n_ws; i++) {
		if (ws[i] >= 0) {
			FD_SET (ws[i], &write_set);
		}
	}
	for (i=0; i<n_es; i++) {
		if (es[i] >= 0) {
			FD_SET (es[i], &except_set);
		}
	}
	
	*result = select (high_fd, n_rs ? &read_set : NULL, n_ws ? &write_set : NULL,
			n_ws ? &except_set : NULL, use_timeout ? &tv : NULL);

	if (*result > 0) {
		/* update sets */
		for (i=0; i<n_rs; i++) {
			if ((rs[i] >= 0) && !FD_ISSET (rs[i], &read_set)) {
				rs[i] = -1;
			}
		}
		for (i=0; i<n_ws; i++) {
			if ((ws[i] >= 0) && !FD_ISSET (ws[i], &write_set)) {
				ws[i] = -1;
			}
		}
		for (i=0; i<n_ws; i++) {
			if ((es[i] >= 0) && !FD_ISSET (es[i], &except_set)) {
				es[i] = -1;
			}
		}
	}
#endif
}
void _fl_select (int *ws) { r_select ((int *)(ws[0]), (int)(ws[1]), (int *)(ws[2]), (int)(ws[3]), (int *)(ws[4]), (int)(ws[5]), (int)(ws[6]), (int)(ws[7]), (int *)(ws[8])); }
/*}}}*/

/*{{{  command-line access stuff */
#if (defined OCCBUILD_KROC) || (defined OCCBUILD_TOCK)
extern int kroc_argc;
extern char **kroc_argv;
#elif (defined OCCBUILD_TVM)
int kroc_argc = 0;
char **kroc_argv = NULL;

/* PROC C.fl.tvm.set.args (VAL INT argc, argv) */
static __inline__ void r_tvm_set_args (int argc, int argv)
{
	kroc_argc = argc;
	kroc_argv = (char **) argv;
	return;
}
void _fl_tvm_set_args (int *ws) { r_tvm_set_args ((int)(ws[0]), (int)(ws[1])); }
#else
#error No implementation of argc/argv for this environment
#endif

/* PROC C.fl.num.args (RESULT INT n) */
static __inline__ void r_num_args (int *n)
{
	*n = kroc_argc;
	return;
}
void _fl_num_args (int *ws) { r_num_args ((int *)(ws[0])); }

/* PROC C.fl.nth.arg (VAL INT n, RESULT []BYTE arg, RESULT INT len) */
static __inline__ void r_nth_arg (int n, char *arg, int arglen, int *len)
{
	int i;

	if ((n < 0) || (n >= kroc_argc) || !arglen) {
		*len = 0;
		return;
	}
	i = strlen (kroc_argv[n]);
	if (i > arglen) {
		i = arglen;
	}
	*len = i;
	memcpy (arg, kroc_argv[n], i);

	return;
}
void _fl_nth_arg (int *ws) { r_nth_arg ((int)(ws[0]), (char *)(ws[1]), (int)(ws[2]), (int *)(ws[3])); }
/*}}}*/

/*{{{  struct occam_stat */
/* This must match DATA TYPE STAT. */
struct occam_stat {
	int64_t dev;
	int64_t ino;
	int mode;
	int nlink;
	int uid;
	int gid;
	int64_t rdev;
	int size;
	int blksize;
	int blocks;
	int atime;
	int mtime;
	int ctime;
}
#ifdef __GNUC__
	__attribute__ ((packed))
#else
	#warning Not GNU C -- might get structure problems
#endif
;
/*}}}*/
/*{{{  struct occam_dirent */
/* This must match DATA TYPE DIRENT. */
struct occam_dirent {
	char filename[256];
}
#ifdef __GNUC__
	__attribute__ ((packed))
#else
	#warning Not GNU C -- might get structure problems
#endif
;
/*}}}*/

/*{{{  static __inline__ void unpack_stat (struct occam_stat *dest, const struct stat *st) */
static __inline__ void unpack_stat (struct occam_stat *dest, const struct stat *st)
{
	dest->dev = st->st_dev;
	dest->ino = st->st_ino;
	dest->mode = st->st_mode;
	dest->nlink = st->st_nlink;
	dest->uid = st->st_uid;
	dest->gid = st->st_gid;
	dest->rdev = st->st_rdev;
	dest->size = st->st_size;
#ifndef HOSTOS_MINGW
	dest->blksize = st->st_blksize;
	dest->blocks = st->st_blocks;
#endif
	dest->atime = st->st_atime;
	dest->mtime = st->st_mtime;
	dest->ctime = st->st_ctime;
}
/*}}}*/
/*{{{  static __inline__ void r_stat (char *fname, int flen, struct occam_stat *stat_struct, int *res) */
static __inline__ void r_stat (char *fname, int flen, struct occam_stat *stat_struct, int *res)
{
	struct stat sbuf;
	char pbuffer[FILENAME_MAX];
	int x;

	if (flen >= (FILENAME_MAX - 1)) {
		x = FILENAME_MAX - 1;
	} else {
		x = flen;
	}
	memcpy (pbuffer, fname, x);
	pbuffer[x] = '\0';

	*res = stat(pbuffer, &sbuf);

	unpack_stat(stat_struct, &sbuf);
}
/*}}}*/
/*{{{  static __inline__ void r_lstat (char *fname, int flen, struct occam_stat *stat_struct, int *res) */
static __inline__ void r_lstat (char *fname, int flen, struct occam_stat *stat_struct, int *res)
{
#ifdef HOSTOS_MINGW
	*res = -1;
#else
	struct stat sbuf;
	char pbuffer[FILENAME_MAX];
	int x;

	if (flen >= (FILENAME_MAX - 1)) {
		x = FILENAME_MAX - 1;
	} else {
		x = flen;
	}
	memcpy (pbuffer, fname, x);
	pbuffer[x] = '\0';

	*res = lstat(pbuffer, &sbuf);
	
	unpack_stat(stat_struct, &sbuf);
#endif
}
/*}}}*/
/*{{{  static __inline__ void r_fstat (int fd, struct occam_stat *stat_struct, int *res) */
static __inline__ void r_fstat (int fd, struct occam_stat *stat_struct, int *res)
{
	struct stat sbuf;

	*res = fstat(fd, &sbuf);
	
	unpack_stat(stat_struct, &sbuf);
}
/*}}}*/
/*{{{  static __inline__ void r_opendir (char *fname, int flen, int *dd) */
static __inline__ void r_opendir (char *fname, int flen, int *dd)
{
	char pbuffer[FILENAME_MAX];
	int x;

	if (flen >= (FILENAME_MAX - 1)) {
		x = FILENAME_MAX - 1;
	} else {
		x = flen;
	}
	memcpy (pbuffer, fname, x);
	pbuffer[x] = '\0';

	*dd = (int)opendir(pbuffer);
}
/*}}}*/
/*{{{  static __inline__ void r_readdir (int dd, struct occam_dirent *dirent_struct, int *result) */
/*
 * This function returns non-zero (ie not null) on success... The real things
 * returns the pointer of course, but we wont do that as it is not needed.
 */
static __inline__ void r_readdir (int dd, struct occam_dirent *dest, int *result)
{
	struct dirent *de;

	de = readdir((DIR *) dd);
	if (de != NULL && (strlen (de->d_name) + 1) <= sizeof (dest->filename)) {
		strcpy (dest->filename, de->d_name);
		*result = 1;
	} else {
		*result = 0;
	}
}
/*}}}*/
/*{{{  static __inline__ void r_closedir (int dd, int *result) */
static __inline__ void r_closedir (int dd, int *result)
{
	*result = closedir((DIR *) dd);
}
/*}}}*/
/*{{{  static __inline__ void r_chmod (char *fname, int flen, int mode, int *result) */
static __inline__ void r_chmod (char *fname, int flen, int mode, int *result)
{
	char pbuffer[FILENAME_MAX];
	int x;

	if (flen >= (FILENAME_MAX - 1)) {
		x = FILENAME_MAX - 1;
	} else {
		x = flen;
	}
	memcpy (pbuffer, fname, x);
	pbuffer[x] = '\0';

	*result = chmod(pbuffer, mode);
}
/*}}}*/
/*{{{  static __inline__ void r_fsync (int fd, int *result) */
static __inline__ void r_fsync (int fd, int *result)
{
#ifdef HOSTOS_MINGW
	*result = -1;
#else
	*result = fsync(fd);
#endif
}
/*}}}*/

/*{{{  interface functions*/
void _fl_check_access (int *w)				{ r_check_access ((char *)(w[0]), (int)(w[1]), (int)(w[2]), (int *)(w[3])); }
void _fl_size (int *w)					{ r_size ((char *)(w[0]), (int)(w[1]), (int *)(w[2])); }
void _fl_open (int *w)					{ r_open ((char *)(w[0]), (int)(w[1]), (int)(w[2]), (int *)(w[3])); }
void _fl_open3 (int *w)					{ r_open3 ((char *)(w[0]), (int)(w[1]), (int)(w[2]), (int)(w[3]), (int *)(w[4])); }
void _fl_pipe (int *w)					{ r_pipe ((int *)(w[0]), (int *)(w[1]), (int *)(w[2])); }
void _fl_dup2 (int *w)					{ r_dup2 ((int)(w[0]), (int)(w[1]), (int *)(w[2])); }
void _fl_read (int *w)					{ r_read ((int)(w[0]), (char *)(w[1]), (int)(w[2]), (int *)(w[3])); }
void _fl_write (int *w)					{ r_write ((int)(w[0]), (char *)(w[1]), (int)(w[2]), (int *)(w[3])); }
void _fl_seek (int *w)					{ r_seek ((int)(w[0]), (int)(w[1]), (int)(w[2]), (int *)(w[3])); }
void _fl_close (int *w)					{ r_close ((int)(w[0]), (int *)(w[1])); }
void _fl_mkdir (int *w)					{ r_mkdir ((char *)(w[0]), (int)(w[1]), (int)(w[2]), (int *)(w[3])); }
void _fl_rmdir (int *w)					{ r_rmdir ((char *)(w[0]), (int)(w[1]), (int *)(w[2])); }
void _fl_unlink (int *w)				{ r_unlink ((char *)(w[0]), (int)(w[1]), (int *)(w[2])); }
void _fl_fd_fd_copy (int *w)				{ r_fd_fd_copy ((int)(w[0]), (int)(w[1]), (int)(w[2]), (int *)(w[3])); }
void _fl_sendfile (int *w)				{ r_sendfile ((int)(w[0]), (int)(w[1]), (int)(w[2]), (int *)(w[3]), (int *)(w[4])); }
void _fl_stat(int *w)					{ r_stat ((char *)(w[0]), (int)(w[1]), (struct occam_stat *)(w[2]), (int *)(w[3])); }
void _fl_lstat(int *w)					{ r_lstat ((char *)(w[0]), (int)(w[1]), (struct occam_stat *)(w[2]), (int *)(w[3])); }
void _fl_fstat(int *w)					{ r_fstat ((int)(w[0]), (struct occam_stat *)(w[1]), (int *)(w[2])); }
void _fl_opendir(int *w)				{ r_opendir ((char *)(w[0]), (int)(w[1]), (int *)(w[2])); }
void _fl_readdir(int *w)				{ r_readdir ((int)(w[0]), (struct occam_dirent *)(w[1]), (int *)(w[2])); }
void _fl_closedir(int *w)				{ r_closedir ((int)(w[0]), (int *)(w[1])); }
void _fl_chmod(int *w)					{ r_chmod ((char *)(w[0]), (int)(w[1]), (int)(w[2]), (int *)(w[3])); }
void _fl_fsync(int *w)					{ r_fsync ((int)(w[0]), (int *)(w[1])); }
/*}}}*/

