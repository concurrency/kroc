###################################
#  Autoconf script for POSIX tvm  #
###################################

AC_INIT([tvm-posix], [1.0.0])
AM_CONFIG_HEADER(config.h)
AC_PREREQ(2.59)

AM_INIT_AUTOMAKE([tvm], [1.0.0])
AC_CANONICAL_HOST

AC_PROG_CC
AC_PROG_RANLIB

AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h stdio.h stdarg.h string.h])
AC_CHECK_HEADERS([dlfcn.h inttypes.h limits.h math.h search.h time.h unistd.h])
AC_CHECK_HEADERS([sys/ioctl.h sys/time.h sys/timeb.h sys/types.h])
AC_CHECK_FUNCS([gettimeofday ftime])

#########################
#  Configuration Options
#########################

AC_ARG_WITH([skroc], AS_HELP_STRING([--with-skroc=PATH], [specifying the path to the directory containing skroc]), [skroc=$withval], [skroc=])

#########################
#  Find skroc
#########################

SKROC_PATH="$PATH"
if test "x$skroc" != "x"; then
  SKROC_PATH="$skroc:$SKROC_PATH"
fi

AC_PATH_PROG([SKROC], [skroc], [no], $SKROC_PATH)
if test "$SKROC" = "no"; then
  AC_MSG_ERROR([skroc is required])
fi

#########################
#  Host Configuration
#########################

# OS configuration
case "$host_os" in
  cygwin*)
    x_os=windows
    ;;
  *Darwin* | *darwin*)
    x_os=darwin
    ;;
  *freebsd*)
    x_os=freebsd
    ;;
  linux*)
    x_os=linux
    ;;
  mingw32*)
    x_os=windows
    ;;
  solaris*)
    x_os=solaris
    ;;
  *)
    x_os=none
    ;;
esac

#########################
#  Defines
#########################

CFLAGS="-Wall $CFLAGS"
LIBS="-lm -ltvm $LIBS"

# OS
case "$x_os" in
  darwin)
    ;;
  freebsd)
    ;;
  linux)
    LIBS="$LIBS -ldl"
    ;;
  solaris)
    CFLAGS="$CFLAGS -I/opt/SUNWspro/prod/include/cc/"
    LDFLAGS="$LDFLAGS -L/opt/SUNWspro/lib"
    LIBS="$LIBS -ldl -lrt -lm9x -lsunmath"
    ;;
  windows)
    ;;
esac

AM_CONDITIONAL([OS_DARWIN],	test "$x_os" = "darwin")
AM_CONDITIONAL([OS_FREEBSD],	test "$x_os" = "freebsd")
AM_CONDITIONAL([OS_LINUX],	test "$x_os" = "linux")
AM_CONDITIONAL([OS_SOLARIS],	test "$x_os" = "solaris")
AM_CONDITIONAL([OS_WINDOWS],	test "$x_os" = "windows")

#########################
#  Find libtvm bits
#########################

tvm_h=no

AC_CHECK_LIB([tvm], [tvm_init], [], AC_MSG_ERROR([libtvm missing]))

AC_CHECK_HEADER([tvm.h], [tvm_h=yes])
AC_CHECK_HEADER([tvm/tvm.h], [tvm_h=yes])
AC_CHECK_HEADER([kroc/tvm.h], [tvm_h=yes])
if test "$tvm_h" = "no"; then
  AC_MSG_ERROR([tvm.h missing])
else
  # Check headers again to define them
  AC_CHECK_HEADERS([tvm.h tvm/tvm.h kroc/tvm.h])
fi

#########################
#  Output
#########################

AC_OUTPUT([Makefile])
