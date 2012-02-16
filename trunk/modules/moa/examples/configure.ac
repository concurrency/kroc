AC_INIT([moa-examples], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_moa_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([moa.module course.module], ,
              [build_moa_examples=no])

AC_MSG_CHECKING([whether to build moa examples])
AC_MSG_RESULT($build_moa_examples)
AM_CONDITIONAL([BUILD_MOA_EXAMPLES], [test $build_moa_examples = yes])

AC_OUTPUT([Makefile])
