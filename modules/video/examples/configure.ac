dnl Process this file with autoconf to produce a configure script.

AC_INIT(vidgrab.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(video-examples, 1.0)

dnl Checks for programs.
OCCAM_OCCBUILD

build_video_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([sdlraster.module course.module video.module], ,
              [build_video_examples=no])

AC_MSG_CHECKING([whether to build video examples])
AC_MSG_RESULT($build_video_examples)
AM_CONDITIONAL([BUILD_VIDEO_EXAMPLES], [test $build_video_examples = yes])

AC_OUTPUT([Makefile])
