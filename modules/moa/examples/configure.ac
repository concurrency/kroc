dnl Process this file with autoconf to produce a configure script.

AC_INIT(mysqlclient.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(moa-examples, 1.0)

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
