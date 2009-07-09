AC_INIT([game-demo], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_game=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module sock.module file.module string.module], ,
              [build_game=no])

INSTALLED_DATA_PATH="$datadir/examples/demos/game-data/"
AC_SUBST(INSTALLED_DATA_PATH)

AC_MSG_CHECKING([whether to build game])
AC_MSG_RESULT($build_game)
AM_CONDITIONAL([BUILD_GAME], [test $build_game = yes])

AC_OUTPUT([Makefile data/Makefile])
