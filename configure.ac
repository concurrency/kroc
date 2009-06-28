# Top-level configure.ac for the KRoC tree.
# KROC_IS_ROOT - m4/occbuild.m4 looks for this comment to find the top-level
# directory; don't change it.

AC_INIT([kroc], [1.5.0-pre5], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE([-Wall foreign])

OCCAM_TOOLCHAIN
OCCAM_OCCBUILD

if test "x$OCCBUILD_API_VERSION" != x2; then
  AC_MSG_ERROR([this configure script has been built with an out-of-date version of `occbuild.m4'; try clearing your `ACLOCAL' environment variable.])
fi

AC_CONFIG_SUBDIRS(tools/ilibr)
AC_CONFIG_SUBDIRS(tools/kroc)
AC_CONFIG_SUBDIRS(tools/mkoccdeps)
if test $OCCBUILD_TOOLCHAIN != tock; then
  AC_CONFIG_SUBDIRS(tools/occ21)
fi
AC_CONFIG_SUBDIRS(tools/occamdoc)

if test $OCCBUILD_TOOLCHAIN = kroc -o $OCCBUILD_TOOLCHAIN = tock; then
  AC_CONFIG_SUBDIRS(runtime/ccsp)
fi

if test $OCCBUILD_TOOLCHAIN = kroc; then
  AC_CONFIG_SUBDIRS(tools/tranx86)

  AC_CONFIG_SUBDIRS(runtime/libkrocif)
fi

if test $OCCBUILD_TOOLCHAIN = tvm; then
  AC_CONFIG_SUBDIRS(tools/plinker)
  AC_CONFIG_SUBDIRS(tools/skroc)
  AC_CONFIG_SUBDIRS(tools/slinker)
  AC_CONFIG_SUBDIRS(tools/tinyswig)

  AC_CONFIG_SUBDIRS(runtime/libtvm)
  AC_CONFIG_SUBDIRS(tvm/posix)
fi

# The modules must be built in dependency order.
AC_CONFIG_SUBDIRS(modules/inmoslibs/libsrc)
#AC_CONFIG_SUBDIRS(modules/nocclibs/libsrc)
AC_CONFIG_SUBDIRS(modules/bsclib/libsrc)
AC_CONFIG_SUBDIRS(modules/cif/libsrc)
AC_CONFIG_SUBDIRS(modules/dynproc/libsrc)
AC_CONFIG_SUBDIRS(modules/course/libsrc)
AC_CONFIG_SUBDIRS(modules/pony/libsrc)
AC_CONFIG_SUBDIRS(modules/udc/libsrc)
AC_CONFIG_SUBDIRS(modules/useful/libsrc)
AC_CONFIG_SUBDIRS(modules/fmtoutlib/libsrc)
AC_CONFIG_SUBDIRS(modules/cdx/libsrc)
AC_CONFIG_SUBDIRS(modules/trap/libsrc)
AC_CONFIG_SUBDIRS(modules/ttyutil/libsrc)
AC_CONFIG_SUBDIRS(modules/moa/libsrc)
AC_CONFIG_SUBDIRS(modules/raster/libsrc)
AC_CONFIG_SUBDIRS(modules/occSDL/libsrc)
AC_CONFIG_SUBDIRS(modules/sdlraster/libsrc)
AC_CONFIG_SUBDIRS(modules/occade/libsrc)
AC_CONFIG_SUBDIRS(modules/occGL/libsrc)
AC_CONFIG_SUBDIRS(modules/button/libsrc)
AC_CONFIG_SUBDIRS(modules/video/libsrc)
AC_CONFIG_SUBDIRS(modules/graphics3d/libsrc)
AC_CONFIG_SUBDIRS(modules/player/libsrc)

AC_CONFIG_SUBDIRS(modules/bsclib/examples)
AC_CONFIG_SUBDIRS(modules/button/examples)
AC_CONFIG_SUBDIRS(modules/cdx/examples)
AC_CONFIG_SUBDIRS(modules/cif/examples)
AC_CONFIG_SUBDIRS(modules/course/answers)
AC_CONFIG_SUBDIRS(modules/course/examples)
AC_CONFIG_SUBDIRS(modules/course/exercises)
AC_CONFIG_SUBDIRS(modules/dynproc/examples)
AC_CONFIG_SUBDIRS(modules/graphics3d/examples)
AC_CONFIG_SUBDIRS(modules/moa/examples)
AC_CONFIG_SUBDIRS(modules/occGL/examples)
AC_CONFIG_SUBDIRS(modules/occSDL/examples)
AC_CONFIG_SUBDIRS(modules/occade/examples)
AC_CONFIG_SUBDIRS(modules/player/examples)
AC_CONFIG_SUBDIRS(modules/player/exercises)
AC_CONFIG_SUBDIRS(modules/pony/examples)
AC_CONFIG_SUBDIRS(modules/raster/examples)
AC_CONFIG_SUBDIRS(modules/sdlraster/examples)
AC_CONFIG_SUBDIRS(modules/trap/examples)
AC_CONFIG_SUBDIRS(modules/ttyutil/examples)
AC_CONFIG_SUBDIRS(modules/udc/examples)
AC_CONFIG_SUBDIRS(modules/useful/examples)
AC_CONFIG_SUBDIRS(modules/video/examples)

AC_CONFIG_SUBDIRS(demos/bar)
AC_CONFIG_SUBDIRS(demos/game)
AC_CONFIG_SUBDIRS(demos/robots)
AC_CONFIG_SUBDIRS(demos/shootout)
AC_CONFIG_SUBDIRS(demos/ttygames)

AC_CONFIG_SUBDIRS(tests/cgtests)
AC_CONFIG_SUBDIRS(tests/hereticc)

AC_CONFIG_SUBDIRS(doc)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
