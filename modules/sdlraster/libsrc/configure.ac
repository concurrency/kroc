dnl Process this file with autoconf to produce a configure script.

AC_INIT(sdlrcompat.inc)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(sdlraster, 2.0)

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
