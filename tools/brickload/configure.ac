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
AC_CHECK_HEADERS(ctype.h)

dnl Check for USB library.
AC_CHECK_HEADERS(usb.h libusb.h libusb/libusb.h libusb-1.0/libusb.h)
AC_CHECK_PROG(LIBUSB_CONFIG, libusb-config, libusb-config, no)
AC_CHECK_LIB(usb, usb_init, [have_libusb0=yes])        # 0.1 API
AC_CHECK_LIB(usb, libusb_init, [have_libusb1=yes])     # 1.0 API
AC_CHECK_LIB(usb-1.0, libusb_init, [have_libusb1=yes]) # 1.0 API 

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
    AC_MSG_CHECKING([libusb API version])

    if test "$have_libusb1" == "yes"; then
      AC_DEFINE([USB_API_1_0], 1, [define for libusb 1.0])
      AC_MSG_RESULT(1.0)
      USB_LDFLAGS="-lusb-1.0"
    else 
      if test "$have_libusb0" == "yes"; then
        AC_DEFINE([USB_API_0_1], 1, [define for libusb 0.1])
        AC_MSG_RESULT(0.1)
        USB_CFLAGS=`$LIBUSB_CONFIG --cflags`
        USB_LDFLAGS=`$LIBUSB_CONFIG --libs`
      else
        AC_MSG_RESULT(none)
	AC_MSG_ERROR([libusb not found])
      fi
    fi
    
    CFLAGS="$LDFLAGS $USB_CFLAGS"
    LDFLAGS="$LDFLAGS $USB_LDFLAGS"
    ;;
esac

dnl Finish up and output.
AC_OUTPUT([Makefile])
