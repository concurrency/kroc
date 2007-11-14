dnl Process this file with autoconf to produce a configure script.

AC_INIT(README)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(udc, 0.1)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

dnl Checks for headers.
AC_CHECK_HEADERS(stdlib.h sys/time.h)

dnl Checks for options.
AC_ARG_ENABLE([bsyscalls], AS_HELP_STRING([--disable-bsyscalls], [disable blocking syscalls support (default enabled)]), [enable_bsyscalls=$enableval], [enable_bsyscalls=yes])
AC_MSG_CHECKING(whether to compile with blocking syscalls support)
AC_MSG_RESULT($enable_bsyscalls)
if test "$enable_bsyscalls" = yes; then
 AC_DEFINE(BLOCKING_SYSCALLS)
fi

AC_OUTPUT([Makefile])


