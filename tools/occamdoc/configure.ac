dnl -*- shell-script -*-
dnl
dnl Process this file with autoconf to produce a configure script.
dnl

AC_REVISION($Revision: 1.1 $)
AC_INIT(occamdoc.in)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(occamdoc, 4.0)

AC_CHECK_PROG(XSLTPROC,xsltproc,xsltproc)
AC_SUBST(XSLTPROC)

AM_PROG_CC_STDC
AM_PROG_AS
AC_PROG_LN_S
AC_PROG_MAKE_SET

AM_PATH_PYTHON([2.2],[],[exit 1])

AC_OUTPUT([Makefile])

