AC_INIT([raster], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
KROC_CCSP_FLAGS
CFLAGS="$CFLAGS $OCCBUILD_CFLAGS"

build_raster=yes

dnl Checks for libraries.
OCCAM_PKG_CHECK_MODULES([LIBPNG], [libpng], [have_libpng=yes], [have_libpng=no])
old_LIBS="$LIBS"
LIBS="$LIBS $LIBPNG_LIBS"
AC_CHECK_FUNC(png_init_io, , [have_libpng=no])
LIBS="$old_LIBS"
AM_CONDITIONAL(HAVE_LIBPNG, [test $have_libpng = yes])

dnl raster requires 4-byte words.
OCCAM_WORD_SIZE([-eq 4], , [build_raster=no])

build_rasterio=$build_raster

dnl Checks for occam modules (only required by rasterio library)
OCCAM_INCLUDE([course.module file.module], ,
              [build_rasterio=no])

if test "x$KROC_RMOX" != "x"; then
  # Building for RMoX version, no rasterio
  build_rasterio=no
fi

AC_MSG_CHECKING([whether to build raster.module])
AC_MSG_RESULT($build_raster)
AM_CONDITIONAL(BUILD_RASTER, [test $build_raster = yes])
AM_CONDITIONAL(BUILD_RASTERIO, [test $build_rasterio = yes])

OCCAM_PROVIDE(raster.module, base, , [test $build_raster = yes])
OCCAM_PROVIDE(rastergraphics.module, graphics, , [test $build_raster = yes])
OCCAM_PROVIDE(rasterio.module, io, , [test $build_rasterio = yes])
OCCAM_PROVIDE(rastertext.module, text, , [test $build_raster = yes])

AC_OUTPUT([Makefile base/Makefile graphics/Makefile io/Makefile text/Makefile])
