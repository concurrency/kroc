dnl Process this file with autoconf to produce a configure script.

AC_INIT(mysql.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(moa, 1.0)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

dnl Checks for libraries.
AC_CHECK_HEADERS(stdlib.h sys/time.h)
AC_CHECK_MYSQLCLIENT

AC_OUTPUT([Makefile])
