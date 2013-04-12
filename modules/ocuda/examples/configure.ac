AC_INIT([ocuda-examples], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_ocuda_examples=yes

OCCAM_HAVE_CUDA(4.2, ,[build_ocuda_examples=no])

OCCAM_INCLUDE([course.module ocuda.module], ,[build_ocuda_examples=no])

AC_MSG_CHECKING([whether to build ocuda examples])
AC_MSG_RESULT($build_ocuda_examples=no)

AM_CONDITIONAL([BUILD_OCUDA_EXAMPLES], [test $build_ocuda_examples = yes])

AC_OUTPUT([Makefile])

