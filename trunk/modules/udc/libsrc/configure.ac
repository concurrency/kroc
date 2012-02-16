AC_INIT([udc], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

build_udc=no
AC_ARG_ENABLE([udc], AS_HELP_STRING([--enable-udc], [enable (old) user-defined channels]), [build_udc=yes])

dnl Checks for headers.
AC_CHECK_HEADERS(stdlib.h sys/time.h)

dnl Checks for options.
AC_ARG_ENABLE([bsyscalls], AS_HELP_STRING([--disable-bsyscalls], [disable blocking syscalls support (default enabled)]), [enable_bsyscalls=$enableval], [enable_bsyscalls=yes])
AC_MSG_CHECKING(whether to compile with blocking syscalls support)
AC_MSG_RESULT($enable_bsyscalls)
if test "$enable_bsyscalls" = yes; then
 AC_DEFINE(BLOCKING_SYSCALLS)
fi

AC_MSG_CHECKING([whether to build udc.module])
AC_MSG_RESULT($build_udc)
AM_CONDITIONAL(BUILD_UDC, [test $build_udc = yes])
OCCAM_PROVIDE(udc.module, , , [test $build_udc = yes])

AC_OUTPUT([Makefile])
