dnl Process this file with autoconf to produce a configure script.

AC_INIT(dynproc.inc)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(dynproc, 1.0)

dnl Checks for programs.
OCCAM_OCCBUILD

build_dynproc=yes
if test "$OCCBUILD_TOOLCHAIN" != "kroc"; then
  build_dynproc=no
elif test "$KROC_CCSP_ENABLE_DYNPROC" != "yes"; then
  build_dynproc=no
fi

AC_MSG_CHECKING([whether to build dynproc.module])
AC_MSG_RESULT($build_dynproc)
AM_CONDITIONAL(BUILD_DYNPROC, [test $build_dynproc = yes])
OCCAM_PROVIDE(dynproc.module, , , [test $build_dynproc = yes])

AC_OUTPUT([Makefile])
