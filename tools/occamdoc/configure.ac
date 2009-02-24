dnl Process this file with autoconf to produce a configure script.

AC_INIT([occamdoc], [4.0], [kroc-bugs@kent.ac.uk])
AM_INIT_AUTOMAKE

AC_PROG_MAKE_SET
AM_PATH_PYTHON([2.2],[],[exit 1])

AC_CHECK_PROG(XSLTPROC, xsltproc, xsltproc, no)
if test "$XSLTPROC" = "no"; then
	AC_MSG_ERROR([xsltproc is required])
fi
AC_SUBST(XSLTPROC)

AC_OUTPUT([Makefile])
