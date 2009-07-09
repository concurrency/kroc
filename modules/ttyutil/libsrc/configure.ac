AC_INIT([ttyutil], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

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
