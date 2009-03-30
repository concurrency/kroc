
AC_INIT([slinker.sxml],[1.0.0],[])
AC_PREREQ(2.12)

AM_INIT_AUTOMAKE(slinker,1.0.0)

AC_ARG_WITH([mzscheme], AS_HELP_STRING([--with-mzscheme=PATH], [specifying the path to the mzscheme executables]), [mzscheme=$withval], [mzscheme=])

MZ_PATH="$PATH"
if test "x$mzscheme" != "x"; then
  MZ_PATH="$mzscheme:$MZ_PATH"
fi

AC_PATH_PROG([MZC], [mzc], [no], $MZ_PATH)
if test "$MZC" = "no"; then
  AC_MSG_ERROR([mzc is required])
fi

AC_PATH_PROG([MZSCHEME], [mzscheme], [no], $MZ_PATH)
if test "$MZSCHEME" = "no"; then
  AC_MSG_ERROR([mzscheme is required])
fi

AC_OUTPUT([Makefile])

