dnl Process this file with autoconf to produce a configure script.

AC_INIT(pioneerutils.inc)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(occplayer, 0.1)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
OCCAM_SWIG

dnl Checks for libraries.
AC_ARG_WITH(player,AS_HELP_STRING(--with-player[=DIR],compile player bindings), , player=maybe)
if test "x$with_player" != xno ; then
	# User explicitly wanted player, but no swig
	if test "x$player" = "xmaybe" && test "x$swig_occampi" = "xno" ; then
		AC_MSG_WARN([No occam-pi enabled SWIG found, not generating Player wrapper])
	else
		if test "x$swig_occampi" = "xno" ; then
			AC_MSG_ERROR([No occam-pi enabled SWIG found, cannot generate Player wrapper])
		fi
		# Set up paths maybe
		if test -d "$with_player/include/player-2.0"; then
			CPPFLAGS="$CPPFLAGS -I$with_player/include/player-2.0"
		fi
		if test -d $with_readline/lib; then
			LDFLAGS="$LDFLAGS -L$with_player/lib"
		fi
		# Check for things
		AC_CHECK_LIB(playerc,playerc_client_connect,[playerlib=yes],[playerlib=no],[-lm])
		AC_CHECK_HEADERS([libplayerc/playerc.h libplayercore/player.h \
											libplayercore/playercommon.h libplayercore/error.h \
											libplayerxdr/playerxdr.h],[playerh=yes],[playerh=no])

		player=no
		if test "x$playerlib" = "xyes" && test "x$playerh" = "xyes" ; then
			player=yes
		else
			AC_MSG_NOTICE([No Player installation found, will not compile the Player wrapper])
		fi
	fi
fi



AC_OUTPUT([Makefile])
