AC_INIT([player-examples], [0.2], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
OCCAM_OCCBUILD

build_player_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([occplayer.module player.module useful.module random.module], ,
              [build_player_examples=no])

AC_MSG_CHECKING([whether to build player examples])
AC_MSG_RESULT($build_player_examples)
AM_CONDITIONAL([BUILD_PLAYER_EXAMPLES], [test $build_player_examples = yes])

AC_OUTPUT([Makefile worlds/Makefile])
