AC_INIT([graphics3d], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
CFLAGS="$CFLAGS $OCCBUILD_CFLAGS"

build_graphics3d=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module maths.module raster.module sdlraster.module], ,
              [build_graphics3d=no])

AC_MSG_CHECKING([whether to build graphics3d.module])
AC_MSG_RESULT($build_graphics3d)
AM_CONDITIONAL(BUILD_GRAPHICS3D, [test $build_graphics3d = yes])
OCCAM_PROVIDE(graphics3d.module, , , [test $build_graphics3d = yes])

AC_OUTPUT([Makefile])
