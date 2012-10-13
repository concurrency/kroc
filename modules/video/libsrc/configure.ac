AC_INIT([video], [0.1], [kroc-bugs@kent.ac.uk])
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

build_video=yes

dnl Checks for headers.
AC_CHECK_HEADER([linux/videodev.h], , [build_video=no])
AC_CHECK_HEADER([linux/videodev2.h], , [build_video=no])
AC_CHECK_HEADER([libv4lconvert.h], , [build_video=no])

dnl Checks for occam modules.
OCCAM_INCLUDE([raster.module], ,
              [build_video=no])

AC_MSG_CHECKING([whether to build video.module])
AC_MSG_RESULT($build_video)
AM_CONDITIONAL(BUILD_VIDEO, [test $build_video = yes])
OCCAM_PROVIDE(video.module, , , [test $build_video = yes])

AC_OUTPUT([Makefile])
