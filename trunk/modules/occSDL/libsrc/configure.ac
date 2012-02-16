AC_INIT([occSDL], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
CFLAGS="$CFLAGS $OCCBUILD_CFLAGS"
OCCAM_SWIG

build_occSDL=yes
build_occSDLsound=yes

dnl Checks for libraries.
OCCAM_PATH_SDL(1.2.0, , [build_occSDL=no; build_occSDLsound=no])
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
