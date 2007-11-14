dnl Process this file with autoconf to produce a configure script.

AC_INIT(simple_surface.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(occSDL-examples, 0.1)

dnl Checks for programs.
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([occSDL.module course.module], ,
              [AC_MSG_ERROR(Required modules were not found)])
OCCAM_INCLUDE([occSDLsound.module],
              [have_sdl_sound=yes], [have_sdl_sound=no])
AM_CONDITIONAL([HAVE_SDL_SOUND], [test $have_sdl_sound = yes])

AC_OUTPUT([Makefile])
