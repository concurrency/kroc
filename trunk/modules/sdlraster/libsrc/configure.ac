AC_INIT([sdlraster], [2.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

build_sdlraster=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([occSDL.module raster.module], ,
              [build_sdlraster=no])

AC_MSG_CHECKING([whether to build sdlraster.module])
AC_MSG_RESULT($build_sdlraster)
AM_CONDITIONAL(BUILD_SDLRASTER, [test $build_sdlraster = yes])
OCCAM_PROVIDE(sdlraster.module miniraster.module, , , [test $build_sdlraster = yes])

AC_OUTPUT([Makefile])
