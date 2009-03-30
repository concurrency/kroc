dnl Process this file with autoconf to produce a configure script.

AC_INIT(video.inc)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(video, 0.1)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

build_video=yes

dnl Checks for headers.
AC_CHECK_HEADER([linux/videodev2.h], , [build_video=no])
AC_CHECK_HEADER([libv4lconvert.h], , [build_video=no])

dnl Checks for occam modules.
OCCAM_INCLUDE([raster.module], ,
              [AC_MSG_ERROR(Required modules not found)])

AC_MSG_CHECKING([whether to build video.module])
AC_MSG_RESULT($build_video)
AM_CONDITIONAL(BUILD_VIDEO, [test $build_video = yes])
OCCAM_PROVIDE(video.module, , , [test $build_video = yes])

AC_OUTPUT([Makefile])
