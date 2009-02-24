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
OCCAM_OCCBUILD

build_pony_examples=yes

dnl Checks for occam modules.
OCCAM_INCLUDE([pony.module course.module convert.module], ,
              [build_pony_examples=no])
OCCAM_INCLUDE([sdlraster.module],
              [have_sdlraster=yes], [have_sdlraster=no])
AM_CONDITIONAL([HAVE_SDLRASTER], [test $have_sdlraster = yes])

AC_MSG_CHECKING([whether to build pony examples])
AC_MSG_RESULT($build_pony_examples)
AM_CONDITIONAL([BUILD_PONY_EXAMPLES], [test $build_pony_examples = yes])

AC_OUTPUT([Makefile])
