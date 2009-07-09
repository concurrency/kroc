AC_INIT([bsclib-examples], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_MAKE_SET
OCCAM_OCCBUILD

build_bsclib_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module file.module proc.module sock.module], ,
              [build_bsclib_examples=no])

AC_MSG_CHECKING([whether to build bsclib examples])
AC_MSG_RESULT($build_bsclib_examples)
AM_CONDITIONAL([BUILD_BSCLIB_EXAMPLES], [test $build_bsclib_examples = yes])

AC_OUTPUT([Makefile])
