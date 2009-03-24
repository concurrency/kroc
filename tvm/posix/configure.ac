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
AC_CHECK_HEADERS([dlfcn.h errno.h inttypes.h limits.h math.h])
AC_CHECK_HEADERS([search.h signal.h termios.h time.h unistd.h])
AC_CHECK_HEADERS([sys/ioctl.h sys/time.h sys/timeb.h sys/types.h])
AC_CHECK_FUNCS([ftime gettimeofday nanosleep setitimer Sleep])

#########################
#  Find occbuild
#########################

OCCAM_OCCBUILD

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
#  Find libtvm bits
#########################

TVM_LIBTVM

#########################
#  Defines
#########################

CFLAGS="-Wall $TVM_CFLAGS $CFLAGS"
LIBS="-lm $TVM_LIBS $LIBS"
TVM_FIRMWARE_PATH="$datadir/tvm/firmware/"
TVM_LIBRARY_PATH="$libdir/tvm/"

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
    TVM_FIRMWARE_PATH="$datadir\\tvm\\firmware\\"
    TVM_LIBRARY_PATH="$libdir\\tvm\\"
    ;;
esac

CFLAGS="$CFLAGS -DTVM_FIRMWARE_PATH='\"$TVM_FIRMWARE_PATH\"'"
CFLAGS="$CFLAGS -DTVM_LIBRARY_PATH='\"$TVM_LIBRARY_PATH\"'"

AM_CONDITIONAL([OS_DARWIN],	test "$x_os" = "darwin")
AM_CONDITIONAL([OS_FREEBSD],	test "$x_os" = "freebsd")
AM_CONDITIONAL([OS_LINUX],	test "$x_os" = "linux")
AM_CONDITIONAL([OS_SOLARIS],	test "$x_os" = "solaris")
AM_CONDITIONAL([OS_WINDOWS],	test "$x_os" = "windows")

#########################
#  Output
#########################

AC_OUTPUT([Makefile])
