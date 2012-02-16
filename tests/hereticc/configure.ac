AC_INIT([hereticc], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_hereticc=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module file.module], ,
              [build_hereticc=no])

AC_MSG_CHECKING([whether to build hereticc])
AC_MSG_RESULT($build_hereticc)
AM_CONDITIONAL([BUILD_HERETICC], [test $build_hereticc = yes])

AC_OUTPUT([Makefile])
