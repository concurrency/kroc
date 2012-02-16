AC_INIT([occade-examples], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_occade_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([occade.module maths.module], ,
              [build_occade_examples=no])

AC_MSG_CHECKING([whether to build occade examples])
AC_MSG_RESULT($build_occade_examples)
AM_CONDITIONAL([BUILD_OCCADE_EXAMPLES], [test $build_occade_examples = yes])

AC_OUTPUT([Makefile images/Makefile])
