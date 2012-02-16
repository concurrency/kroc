AC_INIT([cdx-examples], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_cdx_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([cdx.module netbar.module snglmath.module], ,
              [build_cdx_examples=no])

AC_MSG_CHECKING([whether to build cdx examples])
AC_MSG_RESULT($build_cdx_examples)
AM_CONDITIONAL([BUILD_CDX_EXAMPLES], [test $build_cdx_examples = yes])

AC_OUTPUT([Makefile])
