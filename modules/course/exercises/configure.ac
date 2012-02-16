AC_INIT([course-exercises], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_MAKE_SET
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module], ,
              [AC_MSG_ERROR(Required modules not found)])

AC_OUTPUT([Makefile])
