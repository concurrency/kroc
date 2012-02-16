AC_INIT([moa], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD
CFLAGS="$CFLAGS $OCCBUILD_CFLAGS"

build_moa=yes

dnl Checks for libraries.
AC_CHECK_HEADERS(stdlib.h sys/time.h)
OCCAM_CHECK_MYSQLCLIENT(, , [build_moa=no])

# Make sure the MySQL library is actually usable, since OCCAM_CHECK_MYSQLCLIENT
# doesn't try to link against it.
old_LIBS="$LIBS"
LIBS="$LIBS $MYSQLCLIENT_LIBS"
AC_CHECK_FUNC(mysql_init, , [build_moa=no])
LIBS="$old_LIBS"

dnl Checks for occam modules.
OCCAM_INCLUDE([cif.module], , [build_moa=no])

AC_MSG_CHECKING([whether to build moa.module])
AC_MSG_RESULT($build_moa)
AM_CONDITIONAL(BUILD_MOA, [test $build_moa = yes])
OCCAM_PROVIDE(moa.module, , , [test $build_moa = yes])

AC_OUTPUT([Makefile doc/Makefile])
