AC_INIT([ttyutil-examples], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_ttyutil_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module useful.module ttyutil.module], ,
              [build_ttyutil_examples=no])

AC_MSG_CHECKING([whether to build ttyutil examples])
AC_MSG_RESULT($build_ttyutil_examples)
AM_CONDITIONAL([BUILD_TTYUTIL_EXAMPLES], [test $build_ttyutil_examples = yes])

AC_OUTPUT([Makefile])
