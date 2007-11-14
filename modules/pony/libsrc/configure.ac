dnl -*- shell-script -*-
dnl
dnl Process this file with autoconf to produce a configure script.
dnl

AC_INIT(ponylib.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(pony, 0.1)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module sock.module file.module proc.module cif.module], ,
              [AC_MSG_ERROR(Required modules not found)])

dnl Checks for headers.
AC_CHECK_HEADERS(stdlib.h sys/time.h)
AC_CHECK_HEADER(sys/sendfile.h, [have_sendfile_h=yes], [have_sendfile_h=no])
if test "$have_sendfile_h" = yes; then
 AC_DEFINE(HAVE_SENDFILE_H)
fi
AC_CHECK_HEADER(sys/uio.h, [have_uio_h=yes], [have_uio_h=no])
if test "$have_uio_h" = yes; then
 AC_DEFINE(HAVE_UIO_H)
fi
OCCAM_DEFS=""
AC_CHECK_FUNC(readv, [have_readv=yes], [have_readv=no])
AC_CHECK_FUNC(writev, [have_writev=yes], [have_writev=no])
if test "$have_readv" = yes; then
 if test "$have_writev" = yes; then
  OCCAM_DEFS="-DHAVE.RWVEC"
  AC_DEFINE(HAVE_UIO_RWVEC)
 fi
fi
AC_SUBST(OCCAM_DEFS)

AC_OUTPUT([Makefile])
