AC_INIT([skroc], [1.0.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

AC_ARG_WITH([default-libs], AS_HELP_STRING([--with-default-libs=PATH], [specify default libraries (default: forall)]), [default_libs=$withval], [default_libs=forall])

OCCAM_MZSCHEME

AC_SUBST([skroc_default_libs], $default_libs)
AC_OUTPUT([Makefile defaults.scm])
