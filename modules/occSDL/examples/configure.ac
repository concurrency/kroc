dnl Process this file with autoconf to produce a configure script.

AC_INIT(simple_surface.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(occSDL-examples, 0.1)

dnl Checks for programs.
OCCAM_OCCBUILD

build_occSDL_examples=yes
build_occSDLsound_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([occSDL.module course.module], ,
              [build_occSDL_examples=no])
OCCAM_INCLUDE([occSDLsound.module], ,
              [build_occSDLsound_examples=no])

AC_MSG_CHECKING([whether to build occSDL examples])
AC_MSG_RESULT($build_occSDL_examples)
AM_CONDITIONAL([BUILD_OCCSDL_EXAMPLES], [test $build_occSDL_examples = yes])

AC_MSG_CHECKING([whether to build occSDLsound examples])
AC_MSG_RESULT($build_occSDLsound_examples)
AM_CONDITIONAL([BUILD_OCCSDLSOUND_EXAMPLES], [test $build_occSDLsound_examples = yes])

AC_OUTPUT([Makefile])
