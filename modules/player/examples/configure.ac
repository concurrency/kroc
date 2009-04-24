dnl Process this file with autoconf to produce a configure script.

AC_INIT(simple.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(player-examples, 0.2)

dnl Checks for programs.
OCCAM_OCCBUILD

build_player_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([occplayer.module player.module useful.module], ,
              [build_player_examples=no])

AC_MSG_CHECKING([whether to build player examples])
AC_MSG_RESULT($build_player_examples)
AM_CONDITIONAL([BUILD_PLAYER_EXAMPLES], [test $build_player_examples = yes])

AC_OUTPUT([Makefile])
