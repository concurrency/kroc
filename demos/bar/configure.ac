AC_INIT([bar-demo], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_bar=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module], ,
              [build_bar=no])

dnl Check word size.
OCCAM_WORD_SIZE([-ge 4], , [build_bar=no])

AC_MSG_CHECKING([whether to build bar])
AC_MSG_RESULT($build_bar)
AM_CONDITIONAL([BUILD_BAR], [test $build_bar = yes])

AC_OUTPUT([Makefile])
