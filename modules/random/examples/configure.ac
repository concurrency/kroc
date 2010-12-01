AC_INIT([random-examples], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([random.module useful.module], ,
              [AC_MSG_ERROR(Required modules were not found)])

AC_OUTPUT([Makefile])
