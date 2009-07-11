AC_INIT([shootout], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_shootout=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module file.module convert.module], ,
              [build_shootout=no])

AC_MSG_CHECKING([whether to build shootout])
AC_MSG_RESULT($build_shootout)
AM_CONDITIONAL([BUILD_SHOOTOUT], [test $build_shootout = yes])

AC_OUTPUT([Makefile])
