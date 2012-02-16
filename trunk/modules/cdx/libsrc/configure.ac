AC_INIT([cdx], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
CFLAGS="$CFLAGS $OCCBUILD_CFLAGS"

build_cdx=yes

dnl Checks for libraries.
AC_CHECK_HEADERS(stdlib.h sys/time.h)
OCCAM_PATH_SDL(1.2.0, , [build_cdx=no])
OCCAM_SOCK_LIBS

dnl Checks for occam modules.
OCCAM_INCLUDE([sock.module], ,
              [build_cdx=no])

AC_MSG_CHECKING([whether to build cdx.module])
AC_MSG_RESULT($build_cdx)
AM_CONDITIONAL(BUILD_CDX, [test $build_cdx = yes])
OCCAM_PROVIDE(cdx.module netbar.module, , , [test $build_cdx = yes])

AC_OUTPUT([Makefile])
