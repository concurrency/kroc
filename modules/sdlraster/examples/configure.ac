AC_INIT([sdlraster-examples], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_sdlraster_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([sdlraster.module rasterio.module miniraster.module course.module maths.module useful.module], ,
              [build_sdlraster_examples=no])

AC_MSG_CHECKING([whether to build sdlraster examples])
AC_MSG_RESULT($build_sdlraster_examples)
AM_CONDITIONAL([BUILD_SDLRASTER_EXAMPLES], [test $build_sdlraster_examples = yes])

AC_OUTPUT([Makefile])
