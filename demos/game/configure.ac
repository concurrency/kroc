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
AC_PROG_MAKE_SET
AC_PROG_CC
AM_PROG_CC_STDC
AC_CHECK_TOOL(LD,ld)

AM_C_PROTOTYPES

dnl Checks for programs.
AC_CHECK_PROG(KROC, kroc, kroc)

dnl Checks for libraries.

INSTALLED_DATA_PATH="$DESTDIR$datadir/examples/demos/game-data/"
AC_SUBST(INSTALLED_DATA_PATH)

AC_OUTPUT([Makefile config.inc])

