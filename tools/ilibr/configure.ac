dnl -*- shell-script -*-
dnl
dnl Process this file with autoconf to produce a configure script.
dnl
dnl $Source:$
dnl
dnl $Id:$
dnl

AC_REVISION($Revision: 1.1 $)
AC_INIT(ilibr.c)
AM_CONFIG_HEADER(config.h)
AC_PREREQ(2.12)

AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE([ilibr], [1.1])

dnl Checks for programs.
AC_PROG_CC
AM_PROG_CC_STDC
AC_PROG_MAKE_SET
AC_PROG_RANLIB

AM_C_PROTOTYPES

AC_DEFINE(TARGET_OS_IS_UNIX,1,[define if target OS is UNIX])

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(string.h stdlib.h string.h strings.h unistd.h)

AC_OUTPUT([Makefile tcofflib/Makefile])
