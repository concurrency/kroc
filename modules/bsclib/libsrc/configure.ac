AC_INIT([bsclib], [1.0], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

dnl Checks for headers
AC_CHECK_HEADER(sys/sendfile.h, [have_sendfile_h=yes], [have_sendfile_h=no])
if test "$have_sendfile_h" = yes; then
 AC_DEFINE(HAVE_SENDFILE_H)
fi
AC_CHECK_HEADER(sys/uio.h, [have_uio_h=yes], [have_uio_h=no])
if test "$have_uio_h" = yes; then
 AC_DEFINE(HAVE_UIO_H)
fi
OCCAM_DEFS=""
AC_CHECK_FUNC(readv, [have_readv=yes], [have_readv=no])
AC_CHECK_FUNC(writev, [have_writev=yes], [have_writev=no])
if test "$have_readv" = yes; then
 if test "$have_writev" = yes; then
  OCCAM_DEFS="-DHAVE.RWVEC"
  AC_DEFINE(HAVE_UIO_RWVEC)
 fi
fi

dnl Checks for libraries
OCCAM_SOCK_LIBS

if test "x$OCCBUILD_TOOLCHAIN" = "xkroc"; then
 SAVED_CPPFLAGS="$CFLAGS"
 CPPFLAGS="$CPPFLAGS $OCCBUILD_CFLAGS"
 AC_CHECK_HEADER(udc.h, [have_udc_h=yes], [have_udc_h=no])
 CPPFLAGS="$SAVED_CPPFLAGS"
else
 have_udc_h=no
fi
AC_ARG_ENABLE([cspdrvlib], AS_HELP_STRING([--enable-cspdrvlib], [build cspdrvlib, requires kroc and UDC]), [cspdrvlib=yes], [cspdrvlib=$have_udc_h])
AC_MSG_CHECKING(build cspdrvlib)
AC_MSG_RESULT($cspdrvlib)
AM_CONDITIONAL(BUILD_CSPDRVLIB, test "$cspdrvlib" = yes)

AC_SUBST(OCCAM_DEFS)

if test "$host_os" = "cygwin"; then
 AC_DEFINE(HOSTOS_CYGWIN)
fi

dnl Checks for libraries.
OCCAM_PROVIDE(cspdrv.module, cspdrvlib, none, [test $cspdrvlib = yes])
OCCAM_PROVIDE(file.module, filelib, none)
OCCAM_PROVIDE(http.module, httplib, file.module sock.module)
OCCAM_PROVIDE(proc.module, proclib, file.module)
OCCAM_PROVIDE(sock.module, socklib, none)

AC_OUTPUT([Makefile socklib/Makefile filelib/Makefile proclib/Makefile httplib/Makefile cspdrvlib/Makefile])

