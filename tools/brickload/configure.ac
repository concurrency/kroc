AC_INIT([brickload], [0.1], [kroc-bugs@kent.ac.uk])
AM_CONFIG_HEADER(config.h)
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
AM_PROG_CC_STDC
AC_PROG_MAKE_SET

AM_C_PROTOTYPES

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(stdlib.h string.h assert.h errno.h)
AC_CHECK_HEADERS(stdint.h unistd.h sys/ioctl.h time.h)

dnl Check for libraries.
AC_CHECK_LIB(usb, libusb_init)

dnl OS related defines.
case "$host_os" in
  *Darwin* | *darwin*)
    AC_DEFINE([OS_MAC], 1, [define for Mac OS X/Darwin])
    LDFLAGS="$LDFLAGS -framework CoreFoundation -framework IOKit"
    ;;
  cygwin* | mingw32*)
    AC_DEFINE([OS_WIN], 1, [define for Windows])
    ;;
  *)
    AC_DEFINE([OS_NIX], 1, [define for *nix])
    ;;
esac

dnl Finish up and output.
AC_OUTPUT([Makefile])
