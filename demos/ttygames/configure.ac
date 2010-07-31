AC_INIT([ttygames], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_ttygames=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module ss.module], ,
              [build_ttygames=no])

dnl Check word size.
OCCAM_WORD_SIZE([-ge 4], , [build_ttygames=no])

AC_MSG_CHECKING([whether to build ttygames])
AC_MSG_RESULT($build_ttygames)
AM_CONDITIONAL([BUILD_TTYGAMES], [test $build_ttygames = yes])

AC_OUTPUT([Makefile])
