dnl Process this file with autoconf to produce a configure script.

AC_INIT(cdx.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(cdx, 1.0)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

dnl Checks for libraries.
AC_CHECK_HEADERS(stdlib.h sys/time.h)
AM_PATH_SDL(1.2.0, , [AC_MSG_ERROR(SDL not found)])

AC_OUTPUT([Makefile])
