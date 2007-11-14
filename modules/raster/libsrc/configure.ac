dnl Process this file with autoconf to produce a configure script.

AC_INIT(base/raster.inc)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(raster, 0.1)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module file.module], ,
              [AC_MSG_ERROR(Required modules not found)])

AC_OUTPUT([Makefile base/Makefile graphics/Makefile io/Makefile])
