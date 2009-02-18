dnl -*- shell-script -*-
dnl
dnl Process this file with autoconf to produce a configure script.
dnl

AC_REVISION($Revision: 1.1 $)
AC_INIT(game.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(game-demo, 1.0)

dnl Checks for programs.
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module sock.module file.module string.module], ,
              [AC_MSG_ERROR(Required modules not found)])

INSTALLED_DATA_PATH="$datadir/examples/demos/game-data/"
AC_SUBST(INSTALLED_DATA_PATH)

AC_OUTPUT([Makefile data/Makefile])
