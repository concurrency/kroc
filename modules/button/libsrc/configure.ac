dnl Process this file with autoconf to produce a configure script.

AC_INIT(button.inc)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(button, 0.1)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

build_button=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module occSDL.module occGL.module], ,
              [build_button=no])

AC_MSG_CHECKING([whether to build button.module])
AC_MSG_RESULT($build_button)
AM_CONDITIONAL(BUILD_BUTTON, [test $build_button = yes])
OCCAM_PROVIDE(button.module, , , [test $build_button = yes])

dnl Checks for libraries.
AC_OUTPUT([Makefile])
