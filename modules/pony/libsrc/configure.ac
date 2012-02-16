AC_INIT([pony], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

build_pony=yes
if test "$OCCBUILD_TOOLCHAIN" != "kroc"; then
  build_pony=no
elif test "$KROC_CCSP_ENABLE_PONY" != "yes"; then
  build_pony=no
fi

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module sock.module file.module proc.module cif.module], ,
              [build_pony=no])

dnl Checks for headers.
AC_CHECK_HEADERS(stdlib.h sys/time.h)
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
AC_SUBST(OCCAM_DEFS)

AC_MSG_CHECKING([whether to build pony.module])
AC_MSG_RESULT($build_pony)
AM_CONDITIONAL(BUILD_PONY, [test $build_pony = yes])
OCCAM_PROVIDE(pony.module, , , [test $build_pony = yes])

AC_OUTPUT([Makefile])
