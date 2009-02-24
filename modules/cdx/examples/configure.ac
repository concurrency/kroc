dnl Process this file with autoconf to produce a configure script.

AC_INIT(cdxmandel.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(cdx-examples, 1.0)

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
