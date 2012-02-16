AC_INIT([raster-examples], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_raster_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([raster.module rasterio.module rastergraphics.module course.module file.module], ,
              [build_raster_examples=no])

AC_MSG_CHECKING([whether to build raster examples])
AC_MSG_RESULT($build_raster_examples)
AM_CONDITIONAL([BUILD_RASTER_EXAMPLES], [test $build_raster_examples = yes])

AC_OUTPUT([Makefile])
