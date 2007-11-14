dnl -*- shell-script -*-
dnl
dnl Process this file with autoconf to produce a configure script.
dnl

AC_INIT(commstest.occ)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(pony-examples, 0.1)

dnl Checks for programs.
AC_PROG_CC
OCCAM_OCCBUILD

dnl Checks for occam modules.
OCCAM_INCLUDE([pony.module course.module convert.module], ,
              [AC_MSG_ERROR(Required modules not found)])
OCCAM_INCLUDE([sdlraster.module],
              [have_sdlraster=yes], [have_sdlraster=no])
AM_CONDITIONAL([HAVE_SDLRASTER], [test $have_sdlraster = yes])

AC_OUTPUT([Makefile])
