AC_INIT([cif-examples], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

build_cif_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([cif.module course.module], ,
              [build_cif_examples=no])

if test $build_cif_examples = yes; then
  KROC_CCSP_FLAGS
fi

AC_MSG_CHECKING([whether to build cif examples])
AC_MSG_RESULT($build_cif_examples)
AM_CONDITIONAL([BUILD_CIF_EXAMPLES], [test $build_cif_examples = yes])

AC_OUTPUT([Makefile])
