dnl -*- shell-script -*-
dnl
dnl Process this file with autoconf to produce a configure script.
dnl

AC_REVISION($Revision: 1.1 $)
AC_INIT(README)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(course-answers, 1.0)

dnl Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_CC
AC_CHECK_TOOL(LD,ld)

dnl Checks for programs.
AC_CHECK_PROG(KROC, kroc, kroc)

OCTRAN=$($KROC -c --tran)
OCTRANFLAGS=$($KROC -c --tranflags)

AC_MSG_CHECKING([for kroc translator])
AC_MSG_RESULT([$OCTRAN])
AC_MSG_CHECKING([for kroc translator options])
AC_MSG_RESULT([$OCTRANFLAGS])


dnl Checks for libraries.

AC_SUBST(OCTRAN)
AC_SUBST(OCTRANFLAGS)

AC_OUTPUT([Makefile])

