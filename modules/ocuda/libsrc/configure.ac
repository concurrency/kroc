AC_INIT([ocuda], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

build_ocuda=yes

OCCAM_HAVE_CUDA(4.2, sm_13, ,[build_ocuda=no])


AC_MSG_CHECKING([whether to build ocuda.module])
AC_MSG_RESULT($build_ocuda)
AM_CONDITIONAL(BUILD_OCUDA, [test $build_ocuda = yes])
OCCAM_PROVIDE(ocuda.module, , , [test $build_ocuda = yes])

AC_OUTPUT([Makefile])

