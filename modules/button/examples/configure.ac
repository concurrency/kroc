dnl Process this file with autoconf to produce a configure script.

AC_INIT(button_sample.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(button-examples, 0.1)

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
