AC_INIT([useful], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
AM_PATH_PYTHON([2.2], [], [exit 1])

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module maths.module], ,
              [AC_MSG_ERROR(Required modules not found)])
OCCAM_PROVIDE(useful.module)

AC_OUTPUT([Makefile])
