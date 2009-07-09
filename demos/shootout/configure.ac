AC_INIT([shootout], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module file.module convert.module], ,
              [AC_MSG_ERROR(Required modules not found)])

AC_OUTPUT([Makefile])
