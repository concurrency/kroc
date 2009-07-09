AC_INIT([raster], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
CFLAGS="$CFLAGS $OCCBUILD_CFLAGS"

dnl Checks for libraries.
OCCAM_PKG_CHECK_MODULES([LIBPNG], [libpng], [have_libpng=yes], [have_libpng=no])
old_LIBS="$LIBS"
LIBS="$LIBS $LIBPNG_LIBS"
AC_CHECK_FUNC(png_init_io, , [have_libpng=no])
LIBS="$old_LIBS"
if ! test $OCCBUILD_TOOLCHAIN = kroc; then
	if ! test $OCCBUILD_TOOLCHAIN = tock; then
		# FIXME: PNG support currently requires CCSP's interface.
		have_libpng=no
	fi
fi
AM_CONDITIONAL(HAVE_LIBPNG, [test $have_libpng = yes])

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module file.module], ,
              [AC_MSG_ERROR(Required modules not found)])
OCCAM_PROVIDE(raster.module, base)
OCCAM_PROVIDE(rastergraphics.module, graphics)
OCCAM_PROVIDE(rasterio.module, io)
OCCAM_PROVIDE(rastertext.module, text)

AC_OUTPUT([Makefile base/Makefile graphics/Makefile io/Makefile text/Makefile])
