dnl Process this file with autoconf to produce a configure script.

AC_INIT(occplayer/player.i)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(player, 0.2)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
CFLAGS="$CFLAGS $OCCBUILD_CFLAGS"
OCCAM_SWIG
AM_PATH_PYTHON([2.2], [], [exit 1])

build_occplayer=yes
build_player=yes

dnl Checks for libraries.
PKG_CHECK_MODULES([PLAYER], [playerc playercore playerxdr], , [build_occplayer=no])
old_LIBS="$LIBS"
LIBS="$LIBS $PLAYER_LIBS"
AC_CHECK_FUNC(playerc_client_connect, , [build_occplayer=no])
LIBS="$old_LIBS"
if test "x$HAVE_SWIG_OCCAM" = "xno"; then
	build_occplayer=no
fi
if test "x$build_occplayer" = "xno"; then
	build_player=no
fi

dnl Checks for occam modules.
OCCAM_INCLUDE([useful.module], ,
              [build_player=no])

AC_MSG_CHECKING([whether to build occplayer.module])
AC_MSG_RESULT($build_occplayer)
AM_CONDITIONAL(BUILD_OCCPLAYER, [test $build_occplayer = yes])
OCCAM_PROVIDE(occplayer.module, occplayer, none, [test $build_occplayer = yes])

AC_MSG_CHECKING([whether to build player.module])
AC_MSG_RESULT($build_player)
AM_CONDITIONAL(BUILD_PLAYER, [test $build_player = yes])
OCCAM_PROVIDE(player.module, player, occplayer.module useful.module, [test $build_player = yes])

AC_OUTPUT([Makefile player/Makefile occplayer/Makefile])
