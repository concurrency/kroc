AC_INIT([occade], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_occade=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([file.module course.module occSDL.module raster.module sdlraster.module rasterio.module], ,
              [build_occade=no])

AC_MSG_CHECKING([whether to build occade.module])
AC_MSG_RESULT($build_occade)
AM_CONDITIONAL(BUILD_OCCADE, [test $build_occade = yes])
OCCAM_PROVIDE(occade.module miniraster.module, , , [test $build_occade = yes])

AC_OUTPUT([Makefile])
