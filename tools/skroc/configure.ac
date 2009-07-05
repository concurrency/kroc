AC_INIT([skroc.ss],[1.0.0],[])
AC_PREREQ(2.12)

AM_INIT_AUTOMAKE(skroc.ss,1.0.0)

AC_ARG_WITH([default-libs], AS_HELP_STRING([--with-default-libs=PATH], [specify default libraries (default: forall)]), [default_libs=$withval], [default_libs=forall])

OCCAM_MZSCHEME

AC_SUBST([skroc_default_libs], $default_libs)
AC_OUTPUT([Makefile defaults.scm])
