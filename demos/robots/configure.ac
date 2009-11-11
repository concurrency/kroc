AC_INIT([robots-demo], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_robots=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module sdlraster.module occSDL.module rastergraphics.module rasterio.module maths.module], ,
              [build_robots=no])

AC_MSG_CHECKING([whether to build robots])
AC_MSG_RESULT($build_robots)
AM_CONDITIONAL([BUILD_ROBOTS], [test $build_robots = yes])

AC_OUTPUT([Makefile])
