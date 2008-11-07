dnl Process this file with autoconf to produce a configure script.

AC_INIT(video.inc)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(video, 0.1)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

AC_CHECK_HEADER([linux/videodev2.h],
		[have_linux_videodev2=yes],
		[have_linux_videodev2=no])

AC_CHECK_HEADER([libv4lconvert.h],
                [have_v4lconvert=yes],
		[have_v4lconvert=no])
AM_CONDITIONAL(HAVE_V4LCONVERT, [test $have_v4lconvert = yes])

dnl Checks for occam modules.
OCCAM_INCLUDE([raster.module], ,
              [AC_MSG_ERROR(Required modules not found)])

if test "$have_linux_videodev2" = no; then
 AC_MSG_ERROR(No linux/videodev2.h header file found)
fi

if test "$have_v4lconvert" = no; then
 AC_MSG_ERROR(No libv4lconvert.h header file found)
fi

AC_OUTPUT([Makefile])

