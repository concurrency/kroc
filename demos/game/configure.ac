AC_INIT([game-demo], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module sock.module file.module string.module], ,
              [AC_MSG_ERROR(Required modules not found)])

INSTALLED_DATA_PATH="$datadir/examples/demos/game-data/"
AC_SUBST(INSTALLED_DATA_PATH)

AC_OUTPUT([Makefile data/Makefile])
