AC_INIT([button-examples], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_button_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([button.module course.module], ,
              [build_button_examples=no])

AC_MSG_CHECKING([whether to build button examples])
AC_MSG_RESULT($build_button_examples)
AM_CONDITIONAL([BUILD_BUTTON_EXAMPLES], [test $build_button_examples = yes])

AC_OUTPUT([Makefile])
