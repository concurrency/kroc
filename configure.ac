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

OCCAM_CONFIG_SUBDIRS(tools/ilibr)
OCCAM_CONFIG_SUBDIRS(tools/kroc)
OCCAM_CONFIG_SUBDIRS(tools/mkoccdeps)
if test $OCCBUILD_TOOLCHAIN != tock; then
  OCCAM_CONFIG_SUBDIRS(tools/occ21)
fi
OCCAM_CONFIG_SUBDIRS(tools/occamdoc)

if test $OCCBUILD_TOOLCHAIN = kroc -o $OCCBUILD_TOOLCHAIN = tock; then
  OCCAM_CONFIG_TARGET_SUBDIRS(runtime/ccsp)
fi

if test $OCCBUILD_TOOLCHAIN = kroc; then
  OCCAM_CONFIG_SUBDIRS(tools/tranx86)

  OCCAM_CONFIG_TARGET_SUBDIRS(runtime/libkrocif)
fi

if test $OCCBUILD_TOOLCHAIN = tvm; then
  OCCAM_CONFIG_SUBDIRS(tools/plinker)
  OCCAM_CONFIG_SUBDIRS(tools/skroc)
  OCCAM_CONFIG_SUBDIRS(tools/slinker)
  OCCAM_CONFIG_SUBDIRS(tools/tinyswig)

  OCCAM_CONFIG_TARGET_SUBDIRS(runtime/libtvm)
  OCCAM_CONFIG_TARGET_SUBDIRS(tvm/posix)
fi

# The modules must be built in dependency order.
OCCAM_CONFIG_TARGET_SUBDIRS(modules/inmoslibs/libsrc)
#OCCAM_CONFIG_TARGET_SUBDIRS(modules/nocclibs/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/bsclib/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/cif/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/dynproc/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/course/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/pony/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/udc/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/useful/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/fmtoutlib/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/cdx/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/trap/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/ttyutil/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/moa/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/raster/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/occSDL/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/sdlraster/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/occade/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/occGL/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/button/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/video/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/graphics3d/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/player/libsrc)

OCCAM_CONFIG_TARGET_SUBDIRS(modules/bsclib/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/button/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/cdx/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/cif/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/course/answers)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/course/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/course/exercises)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/dynproc/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/graphics3d/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/moa/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/occGL/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/occSDL/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/occade/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/player/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/player/exercises)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/pony/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/raster/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/sdlraster/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/trap/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/ttyutil/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/udc/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/useful/examples)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/video/examples)

OCCAM_CONFIG_TARGET_SUBDIRS(demos/bar)
OCCAM_CONFIG_TARGET_SUBDIRS(demos/game)
OCCAM_CONFIG_TARGET_SUBDIRS(demos/robots)
OCCAM_CONFIG_TARGET_SUBDIRS(demos/shootout)
OCCAM_CONFIG_TARGET_SUBDIRS(demos/ttygames)

OCCAM_CONFIG_TARGET_SUBDIRS(tests/cgtests)
OCCAM_CONFIG_TARGET_SUBDIRS(tests/hereticc)

OCCAM_CONFIG_SUBDIRS(doc)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
OCCAM_OUTPUT_SUBDIRS
