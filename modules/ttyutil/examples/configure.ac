dnl Process this file with autoconf to produce a configure script.

AC_INIT(menutest.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(ttyutil-examples, 0.1)

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
