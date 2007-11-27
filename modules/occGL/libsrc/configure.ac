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

dnl Checks for libraries.
MDL_HAVE_OPENGL
AC_CHECK_HEADERS([GL/gl.h GL/glu.h], , [AC_MSG_ERROR([OpenGL headers were not found])])

AC_OUTPUT([Makefile])
