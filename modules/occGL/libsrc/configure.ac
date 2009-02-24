dnl Process this file with autoconf to produce a configure script.

AC_INIT(gl.i)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(occGL, 0.1)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
OCCAM_SWIG

build_occGL=yes

dnl Checks for libraries.
MDL_HAVE_OPENGL
AC_CHECK_HEADERS([GL/gl.h GL/glu.h], , [build_occGL=no])

AC_MSG_CHECKING([whether to build occGL.module])
AC_MSG_RESULT($build_occGL)
AM_CONDITIONAL(BUILD_OCCGL, [test $build_occGL = yes])
OCCAM_PROVIDE(occGL.module, , , [test $build_occGL = yes])

AC_OUTPUT([Makefile])
