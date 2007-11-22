dnl -*- shell-script -*-
dnl
dnl Process this file with autoconf to produce a configure script.
dnl

AC_REVISION($Revision: 1.2 $)
AC_INIT([rts/rtsmain.c],[1.5],[kroc-bugs@kent.ac.uk])
AM_CONFIG_HEADER(config.h)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(libkrocif, 1.4)

dnl Checks for programs.
AC_PROG_CC
AM_PROG_CC_STDC
AM_PROG_AS
AC_PROG_RANLIB
AC_PROG_MAKE_SET
AC_CHECK_TOOL(LD,ld)

AM_C_PROTOTYPES
AC_CHECK_HEADERS(stdlib.h string.h strings.h sys/ioctl.h sys/time.h sys/types.h sys/uio.h time.h unistd.h)

AC_ARG_ENABLE([mobiles], AS_HELP_STRING([--disable-mobiles],[disable MOBILE data types (default enabled)]), [enable_mobiles=$enableval], [enable_mobiles=yes])

dnl Checks for programs.
AC_CHECK_PROG(KROC, kroc, kroc)

CCEXTRAOPTS=$($KROC --cflags)
CCINCLUDES=$($KROC --ccincpath)
CCLIBPATH=$($KROC --cclibpath)
LDEXTRAOPTS=$($KROC --ldflags)

AC_MSG_CHECKING([for additional flags needed during compilation])
AC_MSG_RESULT([$CCEXTRAOPTS])
AC_MSG_CHECKING([path to run-time header files])
AC_MSG_RESULT([$CCINCLUDES])

AC_SUBST(CCEXTRAOPTS)
AC_SUBST(CCINCLUDES)
AC_SUBST(CCLIBPATH)
AC_SUBST(LDEXTRAOPTS)

case "$host_os" in
cygwin*)
 build_shared="no"
 LIBKROCIF="krocif.dll"
 AC_DEFINE([NO_WEAK_SYMBOLS],1,[define if weak symbols are unsupported])
 ;;
*Darwin* | *darwin*)
 build_shared="no"
 LIBKROCIF="libkrocif.dylib"
 AC_DEFINE([TARGET_OS_DARWIN],1,[define if using Darwin])
 dnl Disable automatic PIC usage on Apple's GCC
 CFLAGS="$CFLAGS -mdynamic-no-pic"
 ;;
*)
 build_shared="yes"
 SLINKFLAGS="-shared"
 LIBKROCIF="libkrocif.so"
 ;;
esac

AC_SUBST(SLINKFLAGS)
AC_SUBST(LIBKROCIF)
AM_CONDITIONAL(BUILD_SHARED, [test "$build_shared" = "yes"])

dnl Checks for libraries.

AC_OUTPUT([Makefile include/Makefile io/Makefile rts/Makefile support/Makefile])

