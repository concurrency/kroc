dnl Process this file with autoconf to produce a configure script.

AC_INIT(cdx.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(cdx, 1.0)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

build_cdx=yes

dnl Checks for libraries.
AC_CHECK_HEADERS(stdlib.h sys/time.h)
AM_PATH_SDL(1.2.0, , [build_cdx=no])

dnl Checks for occam modules.
OCCAM_INCLUDE([sock.module], ,
              [build_cdx=no])

AC_MSG_CHECKING([whether to build cdx.module])
AC_MSG_RESULT($build_cdx)
AM_CONDITIONAL(BUILD_CDX, [test $build_cdx = yes])
OCCAM_PROVIDE(cdx.module netbar.module, , , [test $build_cdx = yes])

AC_OUTPUT([Makefile])
