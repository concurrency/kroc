AC_INIT([player-exercises], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_MAKE_SET
OCCAM_OCCBUILD

build_player_exercises=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([player.module], ,
              [build_player_exercises=no])

AC_MSG_CHECKING([whether to build player exercises])
AC_MSG_RESULT($build_player_exercises)
AM_CONDITIONAL([BUILD_PLAYER_EXERCISES], [test $build_player_exercises = yes])

AC_OUTPUT([Makefile])
