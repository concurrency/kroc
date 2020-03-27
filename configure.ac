# Top-level configure.ac for the KRoC tree.

AC_INIT([kroc], [1.6.1-pre0], [kroc-bugs@kent.ac.uk])
AC_PREREQ(2.59)
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE([-Wall foreign])

OCCAM_TOOLCHAIN
KROC_BUILD_ROOT="`pwd`"
KROC_SRC_ROOT="`(cd $srcdir && pwd)`"
OCCAM_OCCBUILD

if test "x$OCCBUILD_API_VERSION" != x3; then
  AC_MSG_ERROR([this configure script has been built with an out-of-date version of `occbuild.m4'; try clearing your `ACLOCAL' environment variable.])
fi

OCCAM_CONFIG_SUBDIRS(tools/ilibr)
OCCAM_CONFIG_SUBDIRS(tools/kroc)
OCCAM_CONFIG_SUBDIRS(tools/mkoccdeps)
if test $OCCBUILD_TOOLCHAIN != tock; then
  OCCAM_CONFIG_SUBDIRS(tools/occ21)
fi
OCCAM_CONFIG_SUBDIRS(tools/occamdoc)

#if test $OCCBUILD_TOOLCHAIN = kroc -o $OCCBUILD_TOOLCHAIN = tock; then
#  OCCAM_CONFIG_TARGET_SUBDIRS(runtime/ccsp)
#fi

#if test $OCCBUILD_TOOLCHAIN = kroc; then
#  OCCAM_CONFIG_SUBDIRS(tools/tranx86)
#  OCCAM_CONFIG_TARGET_SUBDIRS(runtime/libkrocif)
#fi

if test $OCCBUILD_TOOLCHAIN = tvm; then
  OCCAM_CONFIG_SUBDIRS(tools/plinker)
  OCCAM_CONFIG_SUBDIRS(tools/tenctool)
  # OCCAM_CONFIG_SUBDIRS(tools/skroc)
  # OCCAM_CONFIG_SUBDIRS(tools/slinker)
  # OCCAM_CONFIG_SUBDIRS(tools/tinyswig)

  OCCAM_CONFIG_TARGET_SUBDIRS(runtime/libtvm)

  AC_ARG_WITH([wrapper],
              AC_HELP_STRING([--with-wrapper=...],
                             [Transterpreter wrapper to use (posix, none)]),
              [wrapper=$withval], [wrapper=posix])
  if test "x$wrapper" = "xposix"; then
    OCCAM_CONFIG_TARGET_SUBDIRS(tvm/posix)
  fi
fi

# The modules must be built in dependency order.
OCCAM_CONFIG_TARGET_SUBDIRS(modules/inmoslibs/libsrc)
#OCCAM_CONFIG_TARGET_SUBDIRS(modules/nocclibs/libsrc)
#OCCAM_CONFIG_TARGET_SUBDIRS(modules/bsclib/libsrc)
#OCCAM_CONFIG_TARGET_SUBDIRS(modules/cif/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/course/libsrc)

OCCAM_CONFIG_TARGET_SUBDIRS(modules/random/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(modules/time/libsrc)
OCCAM_CONFIG_TARGET_SUBDIRS(tests/cgtests)

# if test "x$KROC_RMOX" = "x"; then
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/dynproc/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/course/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/pony/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/udc/libsrc)
# fi
# OCCAM_CONFIG_TARGET_SUBDIRS(modules/random/libsrc)
# OCCAM_CONFIG_TARGET_SUBDIRS(modules/time/libsrc)
# OCCAM_CONFIG_TARGET_SUBDIRS(modules/raster/libsrc)
# if test "x$KROC_RMOX" = "x"; then
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/useful/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/fmtoutlib/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/cdx/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/trap/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/ttyutil/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/moa/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/occSDL/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/sdlraster/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/occade/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/occGL/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/ocuda/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/button/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/video/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/graphics3d/libsrc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/player/libsrc)
# 
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/bsclib/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/button/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/cdx/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/cif/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/course/answers)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/course/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/course/exercises)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/dynproc/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/graphics3d/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/moa/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/occGL/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/occSDL/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/occade/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/ocuda/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/player/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/player/exercises)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/pony/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/random/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/raster/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/sdlraster/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/trap/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/ttyutil/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/udc/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/useful/examples)
#   OCCAM_CONFIG_TARGET_SUBDIRS(modules/video/examples)
# 
#   OCCAM_CONFIG_TARGET_SUBDIRS(demos/bar)
#   OCCAM_CONFIG_TARGET_SUBDIRS(demos/game)
#   OCCAM_CONFIG_TARGET_SUBDIRS(demos/robots)
#   OCCAM_CONFIG_TARGET_SUBDIRS(demos/shootout)
#   OCCAM_CONFIG_TARGET_SUBDIRS(demos/ttygames)
# 
#   OCCAM_CONFIG_TARGET_SUBDIRS(tests/cgtests)
#   OCCAM_CONFIG_TARGET_SUBDIRS(tests/hereticc)
#   OCCAM_CONFIG_TARGET_SUBDIRS(tests/occbench)
# fi
 
# OCCAM_CONFIG_SUBDIRS(doc)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
OCCAM_OUTPUT_SUBDIRS
