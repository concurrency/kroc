###################################
#  Autoconf script for POSIX tvm  #
###################################

AC_INIT([tvm-posix], [1.0.0], [kroc-bugs@kent.ac.uk])
AM_CONFIG_HEADER(config.h)
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

AC_PROG_CC([llvm-gcc gcc])
AC_PROG_RANLIB

AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h stdio.h stdarg.h string.h])
AC_CHECK_HEADERS([dlfcn.h errno.h inttypes.h limits.h math.h])
AC_CHECK_HEADERS([search.h signal.h termios.h time.h unistd.h])
AC_CHECK_HEADERS([sys/ioctl.h sys/time.h sys/timeb.h sys/types.h])
AC_SEARCH_LIBS([dlopen], [dl])
AC_SEARCH_LIBS([nanosleep], [rt])
AC_CHECK_FUNCS([ftime gettimeofday nanosleep setitimer Sleep])

#########################
#  Find occbuild
#########################

OCCAM_OCCBUILD

#########################
#  Host Configuration
#########################

OCCAM_TARGET_PREFIX

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

CFLAGS="$TVM_CFLAGS $CFLAGS"
OCCAM_CHECK_CFLAGS([-Wall], [CFLAGS="$CFLAGS -Wall"])
LIBS="-lm $TVM_LIBS $LIBS"
TVM_FIRMWARE_PATH="$datadir/tvm/${KROC_TARGET_PREFIX}firmware/"
TVM_LIBRARY_PATH="$libdir/${KROC_TARGET_PREFIX}tvm/"

# OS
case "$x_os" in
  darwin)
    ;;
  freebsd)
    ;;
  linux)
    ;;
  solaris)
    CFLAGS="$CFLAGS -I/opt/SUNWspro/prod/include/cc/"
    LDFLAGS="$LDFLAGS -L/opt/SUNWspro/lib"
    AC_CHECK_LIB([m9x], [fesetround], [LIBS="$LIBS -lm9x -lsunmath"], [], [-lsunmath])
    ;;
  windows)
    TVM_FIRMWARE_PATH="$datadir/tvm/firmware/"
    TVM_LIBRARY_PATH="$libdir/tvm/"
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
