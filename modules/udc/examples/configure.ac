dnl Process this file with autoconf to produce a configure script.

AC_INIT(README)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(udc-examples, 1.0)

dnl Checks for programs.
OCCAM_OCCBUILD

build_udc_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([udc.module course.module], ,
              [build_udc_examples=no])

AC_MSG_CHECKING([whether to build udc examples])
AC_MSG_RESULT($build_udc_examples)
AM_CONDITIONAL([BUILD_UDC_EXAMPLES], [test $build_udc_examples = yes])

AC_OUTPUT([Makefile])
