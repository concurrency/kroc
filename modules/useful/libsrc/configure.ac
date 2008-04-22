dnl Process this file with autoconf to produce a configure script.

AC_INIT(useful.inc)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(useful, 1.0)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
AM_PATH_PYTHON([2.2], [], [exit 1])

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module maths.module], ,
              [AC_MSG_ERROR(Required modules not found)])

AC_OUTPUT([Makefile])
