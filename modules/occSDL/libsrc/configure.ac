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

dnl Checks for libraries.
AM_PATH_SDL(1.2.0, [], [AC_MSG_ERROR(SDL is required)])
CFLAGS="$CFLAGS $SDL_CFLAGS"
AC_CHECK_HEADERS([SDL/SDL_sound.h],
                 [have_sdl_sound=yes],
                 [have_sdl_sound=no])
AM_CONDITIONAL(HAVE_SDL_SOUND, [test $have_sdl_sound = yes])

dnl Check for OSes needing helpers.
case "$host_os" in
 *Darwin* | *darwin*)
  cocoa_helper=yes
  AC_DEFINE([COCOA_HELPER], 1, [define if host needs special Cocoa initialisation])
  ;;
 *)
  ;;
esac

dnl Define required helpers.
AM_CONDITIONAL(COCOA_HELPER, test "x$cocoa_helper" = xyes)

AC_OUTPUT([Makefile])
