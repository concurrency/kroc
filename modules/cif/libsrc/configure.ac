AC_INIT([cif], [0.3], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_cif=yes
if test "$OCCBUILD_TOOLCHAIN" != "kroc"; then
  build_cif=no
fi

AC_MSG_CHECKING([whether to build cif.module])
AC_MSG_RESULT($build_cif)
AM_CONDITIONAL(BUILD_CIF, [test $build_cif = yes])
OCCAM_PROVIDE(cif.module, , , [test $build_cif = yes])

AC_OUTPUT([Makefile])
