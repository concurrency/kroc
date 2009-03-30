
AC_INIT([skroc.ss],[1.0.0],[])
AC_PREREQ(2.12)

AM_INIT_AUTOMAKE(skroc.ss,1.0.0)

#######################
#  Options
#######################

AC_ARG_WITH([default-libs], AS_HELP_STRING([--with-default-libs=PATH], [specify default libraries (default: forall)]), [default_libs=$withval], [default_libs=forall])
AC_ARG_WITH([mzscheme], AS_HELP_STRING([--with-mzscheme=PATH], [specifying the path to the mzscheme executables]), [mzscheme=$withval], [mzscheme=])

#######################
#  Find mzscheme bits
#######################

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

#######################
#  Output
#######################

AC_SUBST([skroc_default_libs], $default_libs)
AC_OUTPUT([Makefile defaults.scm])

