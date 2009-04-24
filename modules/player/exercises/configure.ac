dnl Process this file with autoconf to produce a configure script.

AC_REVISION($Revision: 1.1 $)
AC_INIT(README)
AC_PREREQ(2.12)

AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(player-exercises, 1.0)

dnl Checks for programs.
AC_PROG_MAKE_SET
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([player.module], ,
              [AC_MSG_ERROR(Required modules not found)])

AC_OUTPUT([Makefile])
