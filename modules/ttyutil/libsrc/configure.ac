dnl Process this file with autoconf to produce a configure script.

AC_INIT(ttyutil.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(ttyutil, 0.1)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
CFLAGS="$CFLAGS $OCCBUILD_CFLAGS"

build_ttyutil=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module], ,
              [build_ttyutil=no])

AC_MSG_CHECKING([whether to build ttyutil.module])
AC_MSG_RESULT($build_ttyutil)
AM_CONDITIONAL(BUILD_TTYUTIL, [test $build_ttyutil = yes])
OCCAM_PROVIDE(ttyutil.module, , , [test $build_ttyutil = yes])

AC_OUTPUT([Makefile])
