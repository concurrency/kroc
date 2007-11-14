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

AC_DEFINE_UNQUOTED(TARGET_CANONICAL,"${target}",[canonical host])

dnl Maintainer mode not used
dnl jm_MAINTAINER_MODE

dnl Checks for programs.
AC_PROG_CC
AM_PROG_CC_STDC
AC_PROG_MAKE_SET
AC_PROG_RANLIB

AM_C_PROTOTYPES

AC_DEFINE(TARGET_OS_IS_UNIX,1,[define if target OS is UNIX])
if test "X$GCC" = Xyes ; then
  AC_DEFINE(COMPILER_IS_GCC,1,[define if compiler is gcc])
fi

ARCH_DEFS=""

case "${target_cpu}" in
 x86_64)
  ARCH_DEFS="-m32"
  ;;
esac

AC_SUBST(ARCH_DEFS)

dnl Checks for libraries.

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(string.h stdlib.h string.h strings.h unistd.h)

dnl Checks for typedefs, structures, and compiler characteristics.

dnl Checks for library functions.

AC_LINK_FILES(tcofflib/tcofflib.h,tcofflib.h)
AC_LINK_FILES(tcofflib/toolkit.h,toolkit.h)
AC_LINK_FILES(tcofflib/tcoff.h,tcoff.h)


AC_OUTPUT([Makefile tcofflib/Makefile])
