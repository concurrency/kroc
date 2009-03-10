dnl Process this file with autoconf to produce a configure script.

AC_INIT(graphics3d.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(graphics3d, 0.1)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
CFLAGS="$CFLAGS $OCCBUILD_CFLAGS"

build_graphics3d=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module raster.module sdlraster.module], ,
              [build_graphics3d=no])
OCCAM_PROVIDE(graphics3d.module)

AC_MSG_CHECKING([whether to build graphics3d.module])
AC_MSG_RESULT($build_graphics3d)
AM_CONDITIONAL(BUILD_GRAPHICS3D, [test $build_graphics3d = yes])

AC_OUTPUT([Makefile])
