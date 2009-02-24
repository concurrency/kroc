dnl Process this file with autoconf to produce a configure script.

AC_INIT(SDL.i)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(occSDL, 0.1)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
OCCAM_SWIG

build_occSDL=yes
build_occSDLsound=yes

dnl Checks for libraries.
AM_PATH_SDL(1.2.0, , [build_occSDL=no])
CFLAGS="$CFLAGS $SDL_CFLAGS"
AC_CHECK_HEADERS([SDL/SDL_sound.h], , [build_occSDLsound=no])

dnl Check for OSes needing helpers.
cocoa_helper=no
case "$host_os" in
 *Darwin* | *darwin*)
  cocoa_helper=yes
  AC_DEFINE([COCOA_HELPER], 1, [define if host needs special Cocoa initialisation])
  ;;
 *)
  ;;
esac
AM_CONDITIONAL(COCOA_HELPER, test "x$cocoa_helper" = xyes)

AC_MSG_CHECKING([whether to build occSDL.module])
AC_MSG_RESULT($build_occSDL)
AM_CONDITIONAL(BUILD_OCCSDL, [test $build_occSDL = yes])
OCCAM_PROVIDE(occSDL.module, , , [test $build_occSDL = yes])

AC_MSG_CHECKING([whether to build occSDLsound.module])
AC_MSG_RESULT($build_occSDLsound)
AM_CONDITIONAL(BUILD_OCCSDLSOUND, [test $build_occSDLsound = yes])
OCCAM_PROVIDE(occSDLsound.module, , , [test $build_occSDLsound = yes])

AC_OUTPUT([Makefile])
