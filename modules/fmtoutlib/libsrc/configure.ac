AC_INIT([fmtoutlib], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([convert.module], ,
              [AC_MSG_ERROR(Required modules not found)])
OCCAM_PROVIDE(fmtout.module)

AC_OUTPUT([Makefile])
