dnl -*- shell-script -*-
dnl
dnl Process this file with autoconf to produce a configure script.
dnl

AC_REVISION($Revision: 1.1 $)
AC_INIT(README)
AC_PREREQ(2.12)

dnl for automake 1.1 
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(course-answers, 1.0)

dnl Checks for programs.
AC_PROG_MAKE_SET
OCCAM_OCCBUILD

build_course_answers=yes
if test "$OCCBUILD_TOOLCHAIN" = "kroc"; then
  KROC_PROG_KROC
else
  build_course_answers=no
fi

dnl Checks for occam modules.
OCCAM_INCLUDE([course.module shared_screen.module], ,
              [build_course_answers=no])

AC_MSG_CHECKING([whether to build course answers])
AC_MSG_RESULT($build_course_answers)
AM_CONDITIONAL([BUILD_COURSE_ANSWERS], [test $build_course_answers = yes])

AC_OUTPUT([Makefile])
