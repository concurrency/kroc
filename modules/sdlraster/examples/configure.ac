dnl Process this file with autoconf to produce a configure script.

AC_INIT(sdltest.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(sdlraster-examples, 1.0)

dnl Checks for programs.
OCCAM_OCCBUILD

build_sdlraster_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([sdlraster.module rasterio.module miniraster.module course.module maths.module], ,
              [build_sdlraster_examples=no])

AC_MSG_CHECKING([whether to build sdlraster examples])
AC_MSG_RESULT($build_sdlraster_examples)
AM_CONDITIONAL([BUILD_SDLRASTER_EXAMPLES], [test $build_sdlraster_examples = yes])

AC_OUTPUT([Makefile])
